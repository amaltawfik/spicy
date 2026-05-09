# Internal glm-specific computation backbone for table_regression().
# Mirrors R/lm_compute.R: per-coef inference (z-asymptotic with
# Inf df), pseudo-R² family (McFadden, Nagelkerke, Tjur), family /
# link helpers used by the rendering layer (title, exponentiate
# header rebrand, gaussian-glm caveat).

# ---- Family / link introspection -----------------------------------------

# Return a small descriptor for a glm fit summarising its family
# and link, used by the rendering layer for title strings, the
# exponentiate column-header rebrand, and the gaussian-glm caveat.
spicy_glm_family_info <- function(fit) {
  fam <- stats::family(fit)
  family_name <- fam$family
  link_name <- fam$link
  list(
    family = family_name,
    link = link_name,
    is_gaussian = identical(family_name, "gaussian"),
    is_quasi = grepl("^quasi", family_name),
    # Title prefix per family / link (Q34)
    title_prefix = spicy_glm_title_prefix(family_name, link_name),
    # Column-header label for `exp(B)` per family / link
    exp_header = spicy_glm_exp_header(family_name, link_name)
  )
}

spicy_glm_title_prefix <- function(family_name, link_name) {
  if (identical(family_name, "binomial")) {
    return(switch(link_name,
      "logit"   = "Logistic regression",
      "probit"  = "Probit regression",
      "cloglog" = "Complementary log-log regression",
      "log"     = "Log-binomial regression",
      "Binomial regression"
    ))
  }
  if (identical(family_name, "poisson")) return("Poisson regression")
  if (identical(family_name, "Gamma"))   return("Gamma regression")
  if (identical(family_name, "inverse.gaussian")) {
    return("Inverse-Gaussian regression")
  }
  if (identical(family_name, "quasibinomial")) {
    return("Quasi-binomial regression")
  }
  if (identical(family_name, "quasipoisson")) {
    return("Quasi-Poisson regression")
  }
  if (grepl("^quasi", family_name)) return("Quasi-likelihood regression")
  if (identical(family_name, "gaussian")) return("Regression")
  "Regression"
}

spicy_glm_exp_header <- function(family_name, link_name) {
  if (identical(family_name, "binomial") && identical(link_name, "logit")) {
    return("OR")
  }
  if (identical(family_name, "poisson") && identical(link_name, "log")) {
    return("IRR")
  }
  if (identical(family_name, "binomial") && identical(link_name, "cloglog")) {
    return("HR")
  }
  if (identical(family_name, "binomial") && identical(link_name, "log")) {
    return("RR")
  }
  if (identical(family_name, "Gamma") && identical(link_name, "log")) {
    return("MR")     # mean ratio
  }
  "exp(B)"
}


# ---- Per-coefficient inference (z-asymptotic) ----------------------------

# glm coef inference — mirrors compute_lm_coef_inference() in
# lm_compute.R but with z-asymptotic CIs and df = Inf, matching
# `summary.glm()` and the conventional output of Stata `logit`,
# SPSS `LOGISTIC REGRESSION`, and SAS `PROC LOGISTIC`.
#
# Robust / cluster-robust / resampling vcov flow through the same
# interface as for lm (sandwich and clubSandwich both support glm
# via S3 methods).
compute_glm_coef_inference <- function(
  fit,
  coef_idx,
  vc,
  vcov_type,
  cluster = NULL,
  ci_level = 0.95
) {
  cf <- stats::coef(fit)
  estimate <- unname(cf[coef_idx])

  # CR* on glm: clubSandwich supports both vcov + Satterthwaite df
  # via coef_test. For glm the coef_test family computes asymptotic
  # z-style inference but with Satterthwaite-corrected df, which is
  # more conservative under few clusters than naive z (Pustejovsky
  # & Tipton 2018, §4).
  if (startsWith(vcov_type, "CR") && !is.null(cluster)) {
    ct <- tryCatch(
      clubSandwich::coef_test(
        fit,
        vcov = vc,
        cluster = cluster,
        test = "Satterthwaite"
      ),
      error = function(e) NULL
    )
    if (
      !is.null(ct) &&
        is.data.frame(ct) &&
        nrow(ct) >= coef_idx &&
        all(c("df_Satt", "p_Satt", "SE", "tstat") %in% names(ct))
    ) {
      df <- ct$df_Satt[coef_idx]
      se_est <- ct$SE[coef_idx]
      stat <- ct$tstat[coef_idx]
      pval <- ct$p_Satt[coef_idx]
      crit <- if (is.finite(df) && df > 0) {
        stats::qt(1 - (1 - ci_level) / 2, df = df)
      } else {
        stats::qnorm(1 - (1 - ci_level) / 2)
      }
      return(list(
        estimate = estimate,
        se = unname(se_est),
        statistic = unname(stat),
        df = as.double(unname(df)),
        p.value = unname(pval),
        ci_lower = estimate - crit * unname(se_est),
        ci_upper = estimate + crit * unname(se_est),
        test_type = "t"
      ))
    }
  }

  # Default for glm: z-asymptotic Wald (matches summary.glm,
  # parameters::model_parameters, Stata logit, SPSS LOGISTIC).
  se_est <- sqrt(diag(vc))[coef_idx]
  stat <- estimate / se_est
  crit <- stats::qnorm(1 - (1 - ci_level) / 2)
  pval <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
  list(
    estimate = estimate,
    se = unname(se_est),
    statistic = unname(stat),
    df = Inf,
    p.value = unname(pval),
    ci_lower = estimate - crit * unname(se_est),
    ci_upper = estimate + crit * unname(se_est),
    test_type = "z"
  )
}


# ---- Pseudo-R² family ----------------------------------------------------

# Pseudo-R² for glm. Three variants (the most reported in the
# applied literature) implemented from scratch — no new package
# dependency, full control over formulas + edge cases.
#
#   * McFadden (1974)   : 1 - LL_full / LL_null
#                         Most general; defined whenever a true
#                         log-likelihood exists. Returns NA for
#                         quasi-likelihood families.
#   * Nagelkerke (1991) : (1 - exp((LL_null - LL_full) * 2 / n))
#                         / (1 - exp(LL_null * 2 / n))
#                         Cox-Snell rescaled to [0, 1]. Standard
#                         in SPSS / SAS for binary outcomes.
#   * Tjur (2009)       : mean(prob | y=1) - mean(prob | y=0)
#                         "Coefficient of discrimination"; only
#                         defined for binary outcomes (binomial
#                         family with 0/1 y). Returns NA otherwise.
#
# All three return a finite value in [0, 1] (Nagelkerke) or
# (-Inf, 1] (McFadden and Tjur, both bounded above by 1 in
# well-specified models) when defined; NA when not applicable.
compute_pseudo_r2_mcfadden <- function(fit) {
  if (!inherits(fit, "glm")) return(NA_real_)
  fam <- stats::family(fit)
  if (grepl("^quasi", fam$family)) return(NA_real_)
  ll_full <- tryCatch(as.numeric(stats::logLik(fit)),
                       error = function(e) NA_real_)
  null_fit <- tryCatch(stats::update(fit, . ~ 1),
                        error = function(e) NULL)
  if (is.null(null_fit)) return(NA_real_)
  ll_null <- tryCatch(as.numeric(stats::logLik(null_fit)),
                       error = function(e) NA_real_)
  if (!is.finite(ll_full) || !is.finite(ll_null) || ll_null == 0) {
    return(NA_real_)
  }
  1 - (ll_full / ll_null)
}

compute_pseudo_r2_nagelkerke <- function(fit) {
  if (!inherits(fit, "glm")) return(NA_real_)
  fam <- stats::family(fit)
  if (grepl("^quasi", fam$family)) return(NA_real_)
  ll_full <- tryCatch(as.numeric(stats::logLik(fit)),
                       error = function(e) NA_real_)
  null_fit <- tryCatch(stats::update(fit, . ~ 1),
                        error = function(e) NULL)
  if (is.null(null_fit)) return(NA_real_)
  ll_null <- tryCatch(as.numeric(stats::logLik(null_fit)),
                       error = function(e) NA_real_)
  n <- stats::nobs(fit)
  if (!is.finite(ll_full) || !is.finite(ll_null) || !is.finite(n) ||
        n <= 0) {
    return(NA_real_)
  }
  cox_snell <- 1 - exp((ll_null - ll_full) * 2 / n)
  upper <- 1 - exp(ll_null * 2 / n)
  if (!is.finite(upper) || upper <= 0) return(NA_real_)
  cox_snell / upper
}

compute_pseudo_r2_tjur <- function(fit) {
  if (!inherits(fit, "glm")) return(NA_real_)
  fam <- stats::family(fit)
  if (!identical(fam$family, "binomial")) return(NA_real_)
  y <- stats::model.response(stats::model.frame(fit))
  if (is.factor(y)) y <- as.integer(y) - 1L
  if (!all(y %in% c(0, 1))) return(NA_real_)
  pi_hat <- stats::fitted(fit)
  if (length(pi_hat) != length(y)) return(NA_real_)
  m1 <- mean(pi_hat[y == 1])
  m0 <- mean(pi_hat[y == 0])
  if (!is.finite(m1) || !is.finite(m0)) return(NA_real_)
  m1 - m0
}
