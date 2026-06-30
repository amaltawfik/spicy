# Class-generic variance-covariance + coefficient-inference backbone,
# reused by every model class via table_regression() (and by
# table_continuous_lm()). Moved here from R/lm_compute.R: nothing below is
# lm-specific. Contents:
#   * compute_model_vcov(): vcov family dispatch -- classical / HC* (sandwich) /
#     CR* (clubSandwich) / bootstrap / jackknife. The resamplers
#     (compute_resample_vcov_*) refit lm/glm and so apply only to those classes.
#   * compute_coef_inference(): single-coefficient inference; the reference
#     distribution (t vs z) follows the ESTIMATOR, not the fit class.
#   * compute_wald_test(): multi-coefficient Wald F / chi^2.
#   * compute_satt_df_per_coef(): clubSandwich Satterthwaite df map.

compute_model_vcov <- function(
  fit,
  type = "classical",
  cluster = NULL,
  weights = NULL,
  boot_n = 1000L
) {
  # D2(a) guard: the resamplers refit the model with stats::lm() / stats::glm(),
  # so for any other class they would silently fit a WRONG model on the
  # resamples (an lm on survival times, etc.). The orchestrator's per-class
  # capability check (validate_vcov_cluster_lists) already rejects this, but
  # guard here too so a direct/internal caller cannot trigger the silent misfit.
  if (type %in% c("bootstrap", "jackknife") &&
      !inherits(fit, c("lm", "glm"))) {
    spicy_abort(
      sprintf(
        "`vcov = \"%s\"` (resampling) is only available for lm / glm fits, not `%s`.",
        type, class(fit)[1L]
      ),
      class = "spicy_unsupported_vcov"
    )
  }
  if (identical(type, "classical")) {
    return(stats::vcov(fit))
  }

  if (identical(type, "bootstrap")) {
    return(compute_resample_vcov_bootstrap(
      fit,
      cluster = cluster,
      weights = weights,
      boot_n = boot_n
    ))
  }

  if (identical(type, "jackknife")) {
    return(compute_resample_vcov_jackknife(
      fit,
      cluster = cluster,
      weights = weights
    ))
  }

  if (startsWith(type, "HC")) {
    return(tryCatch(
      sandwich::vcovHC(fit, type = type),
      error = function(e) {
        spicy_warn(
          c(
            sprintf(
              "Robust `vcov = \"%s\"` could not be computed.",
              type
            ),
            "x" = paste0("Underlying error: ", conditionMessage(e)),
            "i" = "Falling back to the classical OLS variance; the result may contain NA."
          ),
          class = "spicy_fallback"
        )
        stats::vcov(fit)
      }
    ))
  }

  if (startsWith(type, "CR")) {
    if (is.null(cluster)) {
      spicy_abort(
        sprintf(
          "`vcov = \"%s\"` requires `cluster` to be specified.",
          type
        ), class = "spicy_invalid_input")
    }
    if (!requireNamespace("clubSandwich", quietly = TRUE)) {
      spicy_abort(
        sprintf(
          paste0(
            "`vcov = \"%s\"` requires the 'clubSandwich' package. ",
            "Install it with install.packages(\"clubSandwich\")."
          ),
          type
        ), class = "spicy_invalid_input")
    }
    return(tryCatch(
      clubSandwich::vcovCR(fit, type = type, cluster = cluster),
      error = function(e) {
        spicy_warn(
          c(
            sprintf(
              "Cluster-robust `vcov = \"%s\"` could not be computed.",
              type
            ),
            "x" = paste0("Underlying error: ", conditionMessage(e)),
            "i" = "Falling back to the classical OLS variance; the result may contain NA."
          ),
          class = "spicy_fallback"
        )
        stats::vcov(fit)
      }
    ))
  }

  spicy_abort(
    sprintf("Unknown `vcov` type \"%s\".", type), class = "spicy_invalid_input")
}

# Internal: nonparametric / cluster bootstrap variance-covariance
# matrix of the coefficient vector. Resamples observations (or whole
# clusters when `cluster` is supplied), refits `lm()` on each
# replicate, and returns the empirical covariance of the bootstrapped
# coefficients. References: Davison & Hinkley (1997); Cameron, Gelbach
# & Miller (2008) for the cluster bootstrap.
compute_resample_vcov_bootstrap <- function(
  fit,
  cluster = NULL,
  weights = NULL,
  boot_n = 1000L
) {
  mf <- stats::model.frame(fit)
  # Drop the auxiliary "(weights)" column, if any: it carries a
  # parenthesised name that confuses `lm()` when `mf` is passed back
  # in as `data`. Weights are reattached explicitly via `weights =`.
  mf[["(weights)"]] <- NULL
  formula <- stats::formula(fit)
  n_obs <- nrow(mf)
  orig_coefs <- stats::coef(fit)
  k <- length(orig_coefs)
  coef_names <- names(orig_coefs)

  if (is.null(weights)) {
    weights <- stats::weights(fit)
  }

  # Class-aware refit: glm fits must be re-fit with stats::glm and the
  # original family / link, otherwise the bootstrap variance is computed
  # for a misspecified linear model on the (often binary) response. The
  # original family is captured once and reused across replicates.
  is_glm <- inherits(fit, "glm")
  fam <- if (is_glm) stats::family(fit) else NULL
  refit <- function(boot_idx) {
    sub_data <- mf[boot_idx, , drop = FALSE]
    sub_w <- if (is.null(weights)) NULL else weights[boot_idx]
    args <- list(formula = formula, data = sub_data)
    if (!is.null(sub_w)) {
      args$weights <- sub_w
    }
    if (is_glm) {
      args$family <- fam
      tryCatch(
        suppressWarnings(do.call(stats::glm, args)),
        error = function(e) NULL
      )
    } else {
      tryCatch(
        suppressWarnings(do.call(stats::lm, args)),
        error = function(e) NULL
      )
    }
  }

  beta_boot <- matrix(NA_real_, nrow = boot_n, ncol = k)
  colnames(beta_boot) <- coef_names

  if (is.null(cluster)) {
    # Nonparametric obs bootstrap
    for (b in seq_len(boot_n)) {
      boot_idx <- sample.int(n_obs, n_obs, replace = TRUE)
      fit_b <- refit(boot_idx)
      if (!is.null(fit_b)) {
        coefs_b <- stats::coef(fit_b)
        common <- intersect(names(coefs_b), coef_names)
        beta_boot[b, common] <- coefs_b[common]
      }
    }
  } else {
    # Cluster bootstrap: resample whole clusters
    unique_g <- unique(cluster)
    G <- length(unique_g)
    cl_indices <- split(seq_along(cluster), cluster)
    for (b in seq_len(boot_n)) {
      boot_g <- sample(unique_g, G, replace = TRUE)
      boot_idx <- unlist(cl_indices[as.character(boot_g)], use.names = FALSE)
      fit_b <- refit(boot_idx)
      if (!is.null(fit_b)) {
        coefs_b <- stats::coef(fit_b)
        common <- intersect(names(coefs_b), coef_names)
        beta_boot[b, common] <- coefs_b[common]
      }
    }
  }

  valid <- stats::complete.cases(beta_boot)
  n_valid <- sum(valid)
  if (n_valid < 10L) {
    spicy_warn(
      c(
        sprintf(
          "Bootstrap: only %d / %d replicates were valid; the bootstrap vcov is unreliable.",
          n_valid,
          boot_n
        ),
        "i" = "Falling back to the classical OLS variance."
      ),
      class = "spicy_fallback"
    )
    return(stats::vcov(fit))
  }
  if (n_valid < boot_n %/% 2L) {
    spicy_warn(
      c(
        sprintf(
          "Bootstrap: %d / %d replicates failed (likely rank-deficient resamples).",
          boot_n - n_valid,
          boot_n
        ),
        "i" = sprintf(
          "The bootstrap vcov is computed from the %d valid replicates.",
          n_valid
        )
      ),
      class = "spicy_fallback"
    )
  }

  beta_boot <- beta_boot[valid, , drop = FALSE]
  stats::cov(beta_boot)
}

# Internal: leave-one-out (or leave-one-cluster-out) jackknife
# variance-covariance matrix of the coefficient vector. References:
# Quenouille (1956) and Tukey (1958) for the original jackknife;
# MacKinnon & White (1985) for the linear-regression form.
compute_resample_vcov_jackknife <- function(
  fit,
  cluster = NULL,
  weights = NULL
) {
  mf <- stats::model.frame(fit)
  mf[["(weights)"]] <- NULL
  formula <- stats::formula(fit)
  n_obs <- nrow(mf)
  orig_coefs <- stats::coef(fit)
  k <- length(orig_coefs)
  coef_names <- names(orig_coefs)

  if (is.null(weights)) {
    weights <- stats::weights(fit)
  }

  # Class-aware refit: glm fits must be re-fit with stats::glm and the
  # original family / link (see `compute_resample_vcov_bootstrap` for the
  # same rationale).
  is_glm <- inherits(fit, "glm")
  fam <- if (is_glm) stats::family(fit) else NULL
  refit <- function(jack_idx) {
    sub_data <- mf[jack_idx, , drop = FALSE]
    sub_w <- if (is.null(weights)) NULL else weights[jack_idx]
    args <- list(formula = formula, data = sub_data)
    if (!is.null(sub_w)) {
      args$weights <- sub_w
    }
    if (is_glm) {
      args$family <- fam
      tryCatch(
        suppressWarnings(do.call(stats::glm, args)),
        error = function(e) NULL
      )
    } else {
      tryCatch(
        suppressWarnings(do.call(stats::lm, args)),
        error = function(e) NULL
      )
    }
  }

  if (is.null(cluster)) {
    G <- n_obs
    units <- seq_len(n_obs)
    leave_out <- function(g) which(units != g)
  } else {
    unique_g <- unique(cluster)
    G <- length(unique_g)
    leave_out <- function(g) which(cluster != unique_g[g])
  }

  beta_jack <- matrix(NA_real_, nrow = G, ncol = k)
  colnames(beta_jack) <- coef_names
  for (g in seq_len(G)) {
    fit_g <- refit(leave_out(g))
    if (!is.null(fit_g)) {
      coefs_g <- stats::coef(fit_g)
      common <- intersect(names(coefs_g), coef_names)
      beta_jack[g, common] <- coefs_g[common]
    }
  }

  valid <- stats::complete.cases(beta_jack)
  n_valid <- sum(valid)
  if (n_valid < 2L) {
    spicy_warn(
      c(
        "Jackknife: fewer than 2 valid leave-out replicates.",
        "i" = "Falling back to the classical OLS variance."
      ),
      class = "spicy_fallback"
    )
    return(stats::vcov(fit))
  }
  beta_jack <- beta_jack[valid, , drop = FALSE]
  beta_mean <- colMeans(beta_jack)
  centered <- sweep(beta_jack, 2L, beta_mean, FUN = "-")
  scale <- (n_valid - 1L) / n_valid
  scale * crossprod(centered)
}

# Internal: single-coefficient inference (estimate, SE, statistic, df, p, CI).
# Class-generic. The reference distribution of the classical / HC* default path
# is chosen by `test`, NOT by the fit class: `test = "t"` uses df.residual(fit)
# (or `df_resid` when supplied) -- the OLS / lmer convention; `test = "z"` uses
# df = Inf -- the ML convention (glm, cox, ordinal, glmmTMB). Resampling
# (bootstrap / jackknife) is always asymptotic z; CR* is always clubSandwich
# Satterthwaite t (falling back to z when coef_test fails).
compute_coef_inference <- function(
  fit,
  coef_idx,
  vc,
  vcov_type,
  cluster = NULL,
  ci_level = 0.95,
  test = c("t", "z"),
  df_resid = NULL
) {
  test <- match.arg(test)
  cf <- stats::coef(fit)
  estimate <- unname(cf[coef_idx])

  # Resampling-based vcov: asymptotic z inference (df = Inf).
  if (vcov_type %in% c("bootstrap", "jackknife")) {
    se_est <- sqrt(diag(vc))[coef_idx]
    stat <- estimate / se_est
    crit <- stats::qnorm(1 - (1 - ci_level) / 2)
    pval <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
    return(list(
      estimate = estimate,
      se = unname(se_est),
      statistic = unname(stat),
      df = Inf,
      p.value = unname(pval),
      ci_lower = estimate - crit * unname(se_est),
      ci_upper = estimate + crit * unname(se_est),
      test_type = "z"
    ))
  }

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

  # Classical / HC* / CR-fallback default path: t (df.residual or `df_resid`)
  # for `test = "t"`, z (df = Inf) for `test = "z"`. The axis is the estimator's
  # reference distribution, not the fit class.
  se_est <- sqrt(diag(vc))[coef_idx]
  stat <- estimate / se_est
  if (identical(test, "z")) {
    df <- Inf
    crit <- stats::qnorm(1 - (1 - ci_level) / 2)
    pval <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
    test_type <- "z"
  } else {
    df <- if (!is.null(df_resid)) df_resid else stats::df.residual(fit)
    if (is.finite(df) && df > 0) {
      crit <- stats::qt(1 - (1 - ci_level) / 2, df = df)
      pval <- 2 * stats::pt(abs(stat), df = df, lower.tail = FALSE)
      test_type <- "t"
    } else {
      crit <- stats::qnorm(1 - (1 - ci_level) / 2)
      pval <- 2 * stats::pnorm(abs(stat), lower.tail = FALSE)
      test_type <- "z"
    }
  }
  list(
    estimate = estimate,
    se = unname(se_est),
    statistic = unname(stat),
    df = as.double(df),
    p.value = unname(pval),
    ci_lower = estimate - crit * unname(se_est),
    ci_upper = estimate + crit * unname(se_est),
    test_type = test_type
  )
}

# Internal: multi-coefficient Wald F (used for the global test in
# k > 2 categorical predictors). For CR* mode uses
# clubSandwich::Wald_test() with the HTZ (Hotelling-T-squared with
# Satterthwaite df) method; for classical / HC* uses the Wald F with
# df.residual.
compute_wald_test <- function(
  fit,
  coef_idx_set,
  vc,
  vcov_type,
  cluster = NULL
) {
  cf <- stats::coef(fit)
  beta_sub <- cf[coef_idx_set]
  q <- length(beta_sub)
  df_resid_classical <- stats::df.residual(fit)

  if (q == 0L) {
    return(list(
      statistic = NA_real_,
      df1 = NA_integer_,
      df2 = NA_integer_,
      p.value = NA_real_,
      test_type = NA_character_
    ))
  }

  # Resampling-based vcov: asymptotic chi^2 (Wald) test, df = q.
  if (vcov_type %in% c("bootstrap", "jackknife")) {
    vc_sub <- vc[coef_idx_set, coef_idx_set, drop = FALSE]
    chi2 <- tryCatch(
      as.numeric(crossprod(beta_sub, solve(vc_sub, beta_sub))),
      error = function(e) NA_real_
    )
    pval <- if (is.na(chi2) || !is.finite(chi2)) {
      NA_real_
    } else {
      stats::pchisq(chi2, df = q, lower.tail = FALSE)
    }
    return(list(
      statistic = chi2,
      df1 = as.integer(q),
      df2 = Inf,
      p.value = pval,
      test_type = "chi2"
    ))
  }

  if (startsWith(vcov_type, "CR") && !is.null(cluster)) {
    constraints <- tryCatch(
      clubSandwich::constrain_zero(coef_idx_set, coefs = cf),
      error = function(e) NULL
    )
    wt <- if (!is.null(constraints)) {
      tryCatch(
        clubSandwich::Wald_test(
          fit,
          constraints = constraints,
          vcov = vc,
          cluster = cluster,
          test = "HTZ"
        ),
        error = function(e) NULL
      )
    } else {
      NULL
    }
    if (
      !is.null(wt) &&
        is.data.frame(wt) &&
        nrow(wt) >= 1L &&
        all(c("Fstat", "df_num", "df_denom", "p_val") %in% names(wt))
    ) {
      return(list(
        statistic = unname(wt$Fstat[1]),
        df1 = as.integer(unname(wt$df_num[1])),
        df2 = as.double(unname(wt$df_denom[1])),
        p.value = unname(wt$p_val[1]),
        test_type = "F"
      ))
    }
  }

  # Classical / HC* path
  vc_sub <- vc[coef_idx_set, coef_idx_set, drop = FALSE]
  global_stat <- tryCatch(
    as.numeric(crossprod(beta_sub, solve(vc_sub, beta_sub)) / q),
    error = function(e) NA_real_
  )
  global_p <- if (is.na(global_stat) || !is.finite(global_stat)) {
    NA_real_
  } else {
    stats::pf(global_stat, q, df_resid_classical, lower.tail = FALSE)
  }
  list(
    statistic = global_stat,
    df1 = as.integer(q),
    df2 = as.double(df_resid_classical),
    p.value = global_p,
    test_type = "F"
  )
}


# Map coef name -> Satterthwaite df via clubSandwich::coef_test on the
# original glm. Returns NULL on failure (e.g., clubSandwich missing,
# coef_test errored). Caller falls back to z-asymptotic.
compute_satt_df_per_coef <- function(fit, vc, cluster) {
  ct <- tryCatch(
    clubSandwich::coef_test(
      fit,
      vcov = vc,
      cluster = cluster,
      test = "Satterthwaite"
    ),
    error = function(e) NULL
  )
  if (is.null(ct) || !is.data.frame(ct) || !"df_Satt" %in% names(ct)) {
    return(NULL)
  }
  setNames(as.numeric(ct$df_Satt), rownames(ct))
}


# ---- Robust-vcov capability (C2) ------------------------------------------

# Which `vcov` types table_regression() can actually COMPUTE for this fit's
# class. Default: "classical" only -- a robust vcov the class does not (yet)
# support fails fast in validate_vcov_cluster_lists() with a clear
# spicy_unsupported_vcov error, instead of silently returning model-based SEs
# under a robust label (audit finding C2). The supported set grows per class as
# the robust path is wired + cross-validated; see dev/C2_robust_vcov_spec.md.
#
# Note: "classical" is the user-facing token for the model-based default and is
# supported by EVERY class, so default calls never error -- only an explicit
# robust request on a class that cannot honour it does.
.robust_vcov_support <- function(fit) {
  full <- c("classical", paste0("HC", 0:5), paste0("CR", 0:3),
            "bootstrap", "jackknife")
  switch(
    class(fit)[1L],
    lm     = full,
    glm    = full,
    negbin = full,   # MASS::glm.nb delegates to the glm path
    # --- classes whose robust path is wired in later C2 increments go here ---
    "classical"
  )
}
