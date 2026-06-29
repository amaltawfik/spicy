# Internal glm-specific computation backbone for table_regression().
# Mirrors R/lm_compute.R. Contents:
#   * Family / link helpers (title prefix, exponentiate header)
#   * Per-coef inference: z-asymptotic Wald, with CR* Satterthwaite
#     branch via clubSandwich
#   * Pseudo-R^2 family: McFadden, Nagelkerke, Tjur
#   * Term-level partial chi-square via drop1(test = "LRT")
#   * apply_exponentiate_to_coefs(): exp() transform on coefs +
#     CIs + delta-method SE

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
  # gaussian + identity (and any other unrecognised family) falls
  # through to the generic "Regression" prefix.
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
  if (identical(family_name, "cox") && identical(link_name, "log")) {
    return("HR")     # hazard ratio (coxph / rms::cph)
  }
  if (identical(family_name, "cumulative") && identical(link_name, "logit")) {
    return("OR")     # proportional-odds ratio (ordinal: polr / clm)
  }
  if (identical(family_name, "cumulative") && identical(link_name, "cloglog")) {
    return("HR")     # proportional-hazards ratio (ordinal cloglog link)
  }
  if (identical(family_name, "multinomial") && identical(link_name, "logit")) {
    return("OR")     # multinomial odds / relative-risk ratio (multinom / mlogit)
  }
  if (identical(family_name, "beta") && identical(link_name, "logit")) {
    return("OR")     # betareg mean model (logit link)
  }
  if (identical(family_name, "negbin") && identical(link_name, "log")) {
    return("IRR")    # negative-binomial rate ratio (fixest / pscl)
  }
  if (grepl("^Negative Binomial", family_name) && identical(link_name, "log")) {
    return("IRR")    # MASS::glm.nb family string is "Negative Binomial(theta)"
  }
  if (identical(family_name, "quasibinomial") && identical(link_name, "logit")) {
    return("OR")     # survey::svyglm(family = quasibinomial())
  }
  if (identical(family_name, "quasipoisson") && identical(link_name, "log")) {
    return("IRR")    # survey::svyglm(family = quasipoisson())
  }
  "exp(B)"
}


# ---- Per-coefficient inference (z-asymptotic) ----------------------------

# glm coef inference -- mirrors compute_coef_inference() in
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
  # & Tipton 2018, Section 4).
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
        # Normal-approximation fallback. clubSandwich::coef_test() does NOT
        # always return a finite, positive Satterthwaite df: a coefficient
        # that is constant within every cluster (a between-cluster
        # predictor) under complete/quasi-complete separation yields a
        # degenerate Satterthwaite projection and df_Satt = NaN for that
        # coefficient. Upstream validation does not reject such fits, so
        # this branch is reachable from a valid user call; we then fall
        # back to the z critical value (mirrors R/lm_compute.R).
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


# ---- Pseudo-R^2 family ----------------------------------------------------

# Pseudo-R^2 for glm. Three variants (the most reported in the
# applied literature) implemented from scratch -- no new package
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
#                         Implemented from the original Nagelkerke
#                         (1991) log-likelihood formula, NOT the
#                         deviance-based variant used by
#                         `performance::r2()`. The two forms agree
#                         exactly for binomial (LL_saturated = 0)
#                         and disagree mildly for poisson / Gamma /
#                         inverse.gaussian (where LL_saturated != 0).
#                         For binary outcomes (the dominant use case)
#                         spicy and performance return identical
#                         values to machine precision.
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
  if (grepl("^quasi", stats::family(fit)$family)) return(NA_real_)
  ll_full <- tryCatch(as.numeric(stats::logLik(fit)),
                       error = function(e) NA_real_)
  ll_null <- compute_intercept_only_loglik_glm(fit)
  if (!is.finite(ll_full) || !is.finite(ll_null) || ll_null == 0) {
    # nocov start: a converged non-quasi glm always has a finite,
    # non-zero null log-likelihood (the intercept-only model is
    # never saturated for a non-degenerate response), so this guard
    # is defensive only.
    return(NA_real_)
    # nocov end
  }
  1 - (ll_full / ll_null)
}

compute_pseudo_r2_nagelkerke <- function(fit) {
  if (!inherits(fit, "glm")) return(NA_real_)
  if (grepl("^quasi", stats::family(fit)$family)) return(NA_real_)
  ll_full <- tryCatch(as.numeric(stats::logLik(fit)),
                       error = function(e) NA_real_)
  ll_null <- compute_intercept_only_loglik_glm(fit)
  n <- stats::nobs(fit)
  if (!is.finite(ll_full) || !is.finite(ll_null) || !is.finite(n) ||
        n <= 0) {
    # nocov start: a converged non-quasi glm always has a finite
    # log-likelihood and a positive nobs(), so this guard is
    # defensive only. (The reachable Nagelkerke NA path is the
    # upper <= 0 guard below, exercised by the low-variance Gamma
    # test in test-cov-glm_compute.R.)
    return(NA_real_)
    # nocov end
  }
  cox_snell <- 1 - exp((ll_null - ll_full) * 2 / n)
  upper <- 1 - exp(ll_null * 2 / n)
  if (!is.finite(upper) || upper <= 0) return(NA_real_)
  cox_snell / upper
}

# Log-likelihood of the intercept-only ("null") model for a glm,
# robust to:
#   * formula transforms on the response (`I(round(y))`, `log(y)`,
#     `cbind(y, n - y)`)
#   * `offset(...)` terms in the formula or `offset = ` argument
#   * `weights = ` argument
#
# A naive `update(fit, . ~ 1)` fails when the response side itself
# contains function calls because update reuses the LHS expression
# and tries to re-evaluate the bare symbols against the model frame
# (whose columns are named after the wrapped expression, not the
# inner symbol). It also drops the offset by default -- and an
# intercept-only fit without the offset over-attributes outcome
# variation to the intercept, producing pseudo-R^2 < 0 when the
# full model includes a real-rate offset (Long & Freese 2014 Section 3.6
# explicitly: the null model must carry the same offset as the full
# model, otherwise pseudo-R^2 is not a valid 0-1 statistic).
#
# Workaround: extract the *evaluated* response, weights, and
# offset from the model frame and refit on a fresh data.frame.
# Falls back to NA on any failure.
compute_intercept_only_loglik_glm <- function(fit) {
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (is.null(mf)) return(NA_real_)  # nocov: a converged glm always carries a model.frame
  y <- tryCatch(stats::model.response(mf), error = function(e) NULL)
  if (is.null(y)) return(NA_real_)  # nocov: a glm model.frame always has a response column
  fam <- stats::family(fit)
  weights <- tryCatch(stats::weights(fit), error = function(e) NULL)
  offset_vec <- tryCatch(stats::model.offset(mf), error = function(e) NULL)

  args <- list(formula = y ~ 1, family = fam,
                data = data.frame(y = y))
  if (!is.null(weights)) args$weights <- weights
  if (!is.null(offset_vec)) args$offset <- offset_vec
  null_fit <- tryCatch(
    suppressWarnings(do.call(stats::glm, args)),
    error = function(e) NULL
  )
  if (is.null(null_fit)) return(NA_real_)  # nocov: intercept-only refit of a valid response always converges
  tryCatch(as.numeric(stats::logLik(null_fit)),
           error = function(e) NA_real_)
}

compute_pseudo_r2_tjur <- function(fit) {
  if (!inherits(fit, "glm")) return(NA_real_)
  fam <- stats::family(fit)
  if (!identical(fam$family, "binomial")) return(NA_real_)
  y <- stats::model.response(stats::model.frame(fit))
  if (is.factor(y)) y <- as.integer(y) - 1L
  if (!all(y %in% c(0, 1))) return(NA_real_)
  pi_hat <- stats::fitted(fit)
  # nocov: once y is a 0/1 vector (guard above), fitted() and the
  # response always have matching length for a converged glm.
  if (length(pi_hat) != length(y)) return(NA_real_)
  m1 <- mean(pi_hat[y == 1])
  m0 <- mean(pi_hat[y == 0])
  if (!is.finite(m1) || !is.finite(m0)) return(NA_real_)
  m1 - m0
}


# ---- exp() transform for response-scale display --------------------------

# Apply exp() to B-row (and beta-row) estimates + CI bounds, and a
# delta-method approximation to the SE. The test statistic and the
# p-value are invariant under monotone transformation and stay on
# the link scale (matches Stata `logit, or` / SPSS exp(B) / SAS
# OR convention).
#
# Reference rows (em-dash) and singular coefs (NA) pass through
# untouched. The "(Intercept)" row IS exponentiated because exp()
# of the intercept is the baseline odds / rate / ... -- meaningful
# in its own right (Stata reports it; SPSS reports it; APA Manual 7
# Section 6.46 example).
apply_exponentiate_to_coefs <- function(coefs) {
  if (is.null(coefs) || nrow(coefs) == 0L) return(coefs)
  is_b_or_beta <- coefs$estimate_type %in% c("B", "beta")
  is_eligible <- is_b_or_beta &
                   !coefs$is_singular &
                   !coefs$is_reference &
                   !is.na(coefs$estimate)
  if (!any(is_eligible)) return(coefs)

  rows <- which(is_eligible)
  est_orig <- coefs$estimate[rows]
  se_orig  <- coefs$se[rows]

  exp_est <- exp(est_orig)
  coefs$estimate[rows] <- exp_est
  coefs$ci_low[rows]   <- exp(coefs$ci_low[rows])
  coefs$ci_high[rows]  <- exp(coefs$ci_high[rows])
  # Delta-method: Var(g(X)) ~ (g'(X))^2 x Var(X) ; for g = exp,
  # g'(X) = exp(X), so SE_exp = exp(B) x SE_logit.
  coefs$se[rows]       <- exp_est * se_orig
  # Statistic (z) and p_value: invariant under exp() -- the test of
  # H0: B = 0 <-> H0: exp(B) = 1 has the same z and p. Leave as-is.
  coefs
}


# Phase 7c16: frame-schema sibling of apply_exponentiate_to_coefs().
# The mixed-effects methods produce coefs in the frame schema
# (`std_error`, `ci_lower`, `ci_upper`, `is_ref`, NO `is_singular`)
# rather than the legacy long-format (`se`, `ci_low`, `ci_high`,
# `is_reference`, `is_singular`); operate directly on the frame-schema
# columns so we don't need a round-trip translation.
#
# AME rows pass through unchanged: marginaleffects already returns the
# response-scale effect, so exponentiating again would be wrong (and
# the "OR" / "IRR" / ... label only applies to B / beta rows).
apply_exponentiate_to_frame_coefs <- function(coefs) {
  if (is.null(coefs) || nrow(coefs) == 0L) return(coefs)
  is_b_or_beta <- coefs$estimate_type %in% c("B", "beta")
  is_eligible <- is_b_or_beta &
                   !(coefs$is_ref %in% TRUE) &
                   !is.na(coefs$estimate)
  if (!any(is_eligible)) return(coefs)

  rows <- which(is_eligible)
  est_orig <- coefs$estimate[rows]
  se_orig  <- coefs$std_error[rows]

  exp_est <- exp(est_orig)
  coefs$estimate[rows] <- exp_est
  coefs$ci_lower[rows] <- exp(coefs$ci_lower[rows])
  coefs$ci_upper[rows] <- exp(coefs$ci_upper[rows])
  coefs$std_error[rows] <- exp_est * se_orig  # Delta-method.
  # Statistic and p-value invariant under exp(): leave as-is.
  coefs
}


# Apply exp() to a frame's B / beta rows when exponentiate = TRUE, driven
# entirely by the frame's own info$family / info$supports -- NOT stats::family(),
# which errors for polr / clm / multinom / fixest / betareg / mlogit (no family()
# method). This is the class-agnostic generalisation of
# .apply_exp_to_survival_frame() / .apply_exp_to_mixed_frame(): any
# as_regression_frame.* method that sets info$family$link and
# info$supports$exponentiate can route its coefs through it before assembling
# the frame, and the title/footer layer reads info$extras$exp_applied /
# exp_header without any class-specific branching.
#
# A no-op (coefs pass through unchanged) when exponentiate is not TRUE or the
# frame does not advertise supports$exponentiate.
.apply_exp_to_frame <- function(coefs, info, exponentiate) {
  # Already exponentiated by a self-applying method (lm/glm legacy, merMod /
  # glmmTMB / nlme mixed, coxph / survreg)? Leave it untouched. This guard
  # makes a second, central application in the table_regression() orchestrator
  # a no-op, so exp() is never applied twice.
  if (isTRUE(info$extras$exp_applied)) {
    return(list(coefs = coefs, info = info))
  }
  # exp() of an identity-link coefficient is meaningless (the coef IS the
  # effect on the response scale). Some classes advertise
  # supports$exponentiate = TRUE generically (e.g. gaussian glm), so guard on
  # the link too -- mirrors the identity no-op the lm/glm and mixed paths
  # already apply. Survival reaches here only via .apply_exp_to_survival_frame,
  # which has already set exp_applied = TRUE, so its log-scale links are safe.
  if (!isTRUE(exponentiate) || !isTRUE(info$supports$exponentiate) ||
      identical(info$family$link, "identity")) {
    return(list(coefs = coefs, info = info))
  }
  coefs <- apply_exponentiate_to_frame_coefs(coefs)
  info$extras$exp_applied <- TRUE
  info$extras$exp_header  <- spicy_glm_exp_header(
    info$family$family, info$family$link
  )
  list(coefs = coefs, info = info)
}


# ---- Partial likelihood-ratio chi-square ---------------------------------

# Term-level partial chi^2 via drop1(test = "LRT") -- the glm analog
# of the partial F-test in lm. For each model term, refit without that
# term and compare via likelihood ratio:
#
#   LR = 2 * (LL_full - LL_reduced)  ~  chi^2(df = #params dropped)
#
# Convention follows SAS PROC LOGISTIC `TYPE3`, Stata `test, accumulate`,
# Allison "TYPE3", Long & Freese 2014 Section 3.5. For factor terms with k
# levels, the test is joint over all k-1 dummies and df = k-1
# (matching how `car::Anova(type = 3)` reports it for glm).
#
# Returns NULL on any failure (drop1 error, non-finite chi-square, etc.)
# so the caller can skip the term and the renderer em-dashes the cells.
# Quasi families (quasibinomial / quasipoisson / quasi) have no proper
# log-likelihood, so the LRT is undefined; we return NULL -- consistent
# with how the pseudo-R^2 family handles them.
compute_partial_chi2_for_term <- function(fit, term_label) {
  if (!inherits(fit, "glm")) return(NULL)
  if (grepl("^quasi", stats::family(fit)$family)) return(NULL)
  d1 <- tryCatch(
    suppressWarnings(
      stats::drop1(fit, scope = stats::reformulate(term_label), test = "LRT")
    ),
    error = function(e) NULL
  )
  if (is.null(d1) || nrow(d1) < 2L) return(NULL)
  # Column names vary by R version: "LRT" (modern) vs "scaled dev." vs
  # "Deviance" depending on dispersion handling. The chi-square value
  # is in whichever of these is present; we look for them in order.
  chi2_col <- intersect(c("LRT", "scaled dev.", "Deviance"), names(d1))
  # nocov: drop1(test = "LRT") on a glm always returns a "Df" column and
  # one of "LRT"/"scaled dev."/"Deviance"; an empty match is unreachable.
  if (length(chi2_col) == 0L || !"Df" %in% names(d1)) return(NULL)
  chi2 <- d1[[chi2_col[1L]]][2L]
  df1 <- d1[["Df"]][2L]
  if (!is.finite(chi2) || !is.finite(df1) || df1 < 1L || chi2 < 0) {
    return(NULL)
  }
  p_value <- stats::pchisq(chi2, df = df1, lower.tail = FALSE)
  list(chi2 = chi2, df = as.integer(df1), p_value = p_value)
}
