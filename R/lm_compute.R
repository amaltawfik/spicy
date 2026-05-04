# Internal lm-based computation backbone shared by table_continuous_lm()
# and (in 0.13.0) table_regression(): vcov family (classical / HC* / CR* /
# bootstrap / jackknife), single-coef and Wald inference, model stats, and
# noncentral effect-size CIs.

compute_lm_vcov <- function(
  fit,
  type = "classical",
  cluster = NULL,
  weights = NULL,
  boot_n = 1000L
) {
  if (identical(type, "classical")) {
    return(stats::vcov(fit))
  }

  if (identical(type, "bootstrap")) {
    return(compute_lm_vcov_bootstrap(
      fit,
      cluster = cluster,
      weights = weights,
      boot_n = boot_n
    ))
  }

  if (identical(type, "jackknife")) {
    return(compute_lm_vcov_jackknife(
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
compute_lm_vcov_bootstrap <- function(
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

  refit <- function(boot_idx) {
    sub_data <- mf[boot_idx, , drop = FALSE]
    sub_w <- if (is.null(weights)) NULL else weights[boot_idx]
    args <- list(formula = formula, data = sub_data)
    if (!is.null(sub_w)) {
      args$weights <- sub_w
    }
    tryCatch(
      suppressWarnings(do.call(stats::lm, args)),
      error = function(e) NULL
    )
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
compute_lm_vcov_jackknife <- function(
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

  refit <- function(jack_idx) {
    sub_data <- mf[jack_idx, , drop = FALSE]
    sub_w <- if (is.null(weights)) NULL else weights[jack_idx]
    args <- list(formula = formula, data = sub_data)
    if (!is.null(sub_w)) {
      args$weights <- sub_w
    }
    tryCatch(
      suppressWarnings(do.call(stats::lm, args)),
      error = function(e) NULL
    )
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

# Internal: single-coefficient inference (estimate, SE, t, df, p, CI).
# For classical / HC* mode, uses df.residual(fit) and the supplied vcov.
# For CR* mode, uses clubSandwich::coef_test() with Satterthwaite df.
# Falls back to df.residual + supplied vcov if coef_test fails.
compute_lm_coef_inference <- function(
  fit,
  coef_idx,
  vc,
  vcov_type,
  cluster = NULL,
  ci_level = 0.95
) {
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

  # Classical / HC* / CR fallback path
  se_est <- sqrt(diag(vc))[coef_idx]
  df <- stats::df.residual(fit)
  stat <- estimate / se_est
  crit <- if (is.finite(df) && df > 0) {
    stats::qt(1 - (1 - ci_level) / 2, df = df)
  } else {
    stats::qnorm(1 - (1 - ci_level) / 2)
  }
  pval <- 2 * stats::pt(abs(stat), df = df, lower.tail = FALSE)
  list(
    estimate = estimate,
    se = unname(se_est),
    statistic = unname(stat),
    df = as.double(unname(df)),
    p.value = unname(pval),
    ci_lower = estimate - crit * unname(se_est),
    ci_upper = estimate + crit * unname(se_est),
    test_type = "t"
  )
}

# Internal: multi-coefficient Wald F (used for the global test in
# k > 2 categorical predictors). For CR* mode uses
# clubSandwich::Wald_test() with the HTZ (Hotelling-T-squared with
# Satterthwaite df) method; for classical / HC* uses the Wald F with
# df.residual.
compute_lm_wald_test <- function(
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

compute_lm_model_stats <- function(fit, focal_term = NULL) {
  sm <- summary(fit)
  r2 <- unname(sm$r.squared)         # always overall R² (model-level)
  adj_r2 <- unname(sm$adj.r.squared) # always overall adj. R²
  sigma_hat <- unname(sm$sigma)
  df_resid <- stats::df.residual(fit)
  cf <- stats::coef(fit)

  if (is.null(focal_term)) {
    # Bivariate path: every non-intercept coef belongs to the focal
    # predictor, so the model-level f² / ω² coincide with the
    # focal-term effect size.
    df_effect <- length(cf) - 1L
    f2 <- if (is.na(r2) || r2 >= 1) NA_real_ else r2 / (1 - r2)
    d <- if (
      length(cf) < 2L ||
        !is.finite(sigma_hat) ||
        sigma_hat <= 0 ||
        anyNA(cf[2])
    ) {
      NA_real_
    } else {
      unname(cf[2]) / sigma_hat
    }
    g <- if (is.na(d) || !is.finite(df_resid) || df_resid <= 1) {
      NA_real_
    } else {
      (1 - 3 / (4 * df_resid - 1)) * d
    }
    omega2 <- compute_lm_omega2(fit, df_effect, df_resid)
  } else {
    # Covariate-adjusted path: f² and ω² are restricted to the focal
    # term via partial F (see `extract_lm_focal_f_stat`). Cohen's d
    # and Hedges' g are undefined under adjustment and the public
    # API rejects them upstream; we still set them to NA defensively
    # in case this helper is reached on an unexpected path.
    fs <- extract_lm_focal_f_stat(fit, focal_term)
    if (is.null(fs) || !is.finite(fs$f_obs) || fs$f_obs <= 0) {
      f2 <- NA_real_
      omega2 <- NA_real_
    } else {
      # Partial f² = F * df1 / df_resid. Equivalent to
      # SS_focal / SS_residual where SS_focal = F * df1 * MSE_full.
      f2 <- fs$f_obs * fs$df1 / fs$df2
      omega2 <- compute_lm_partial_omega2(fit, fs)
    }
    d <- NA_real_
    g <- NA_real_
  }

  list(r2 = r2, adj_r2 = adj_r2, f2 = f2, d = d, g = g, omega2 = omega2)
}

# Internal: partial ω² computation for a focal term, given the
# partial F-stat already extracted via `extract_lm_focal_f_stat`.
# Mirrors `compute_lm_omega2()`'s formula but uses focal-term
# SS_effect = F × df_focal × MSE_full instead of the model-level
# SS_effect implied by the global F.
compute_lm_partial_omega2 <- function(fit, fs) {
  rss_full <- stats::deviance(fit)
  if (!is.finite(rss_full) || rss_full <= 0) {
    return(NA_real_)
  }
  mse_full <- rss_full / fs$df2
  ss_focal <- fs$f_obs * fs$df1 * mse_full

  y <- stats::model.response(stats::model.frame(fit))
  if (!is.numeric(y)) {
    return(NA_real_)
  }
  w <- stats::weights(fit)
  if (is.null(w)) {
    w <- rep(1, length(y))
  }
  if (length(w) != length(y)) {
    return(NA_real_)
  }
  sw <- sum(w)
  if (!is.finite(sw) || sw <= 0) {
    return(NA_real_)
  }
  y_bar_w <- sum(w * y) / sw
  ss_total <- sum(w * (y - y_bar_w)^2)
  if (!is.finite(ss_total) || ss_total <= 0) {
    return(NA_real_)
  }

  omega2 <- (ss_focal - fs$df1 * mse_full) / (ss_total + mse_full)
  if (!is.finite(omega2)) {
    return(NA_real_)
  }
  max(0, omega2)
}

compute_lm_omega2 <- function(fit, df_effect, df_resid) {
  if (
    !is.finite(df_effect) ||
      df_effect < 1L ||
      !is.finite(df_resid) ||
      df_resid <= 0
  ) {
    return(NA_real_)
  }
  y <- stats::model.response(stats::model.frame(fit))
  if (!is.numeric(y)) {
    return(NA_real_)
  }
  resid <- stats::residuals(fit)
  w <- stats::weights(fit)
  if (is.null(w)) {
    w <- rep(1, length(resid))
  }
  if (length(w) != length(y) || length(resid) != length(y)) {
    return(NA_real_)
  }
  sw <- sum(w)
  if (!is.finite(sw) || sw <= 0) {
    return(NA_real_)
  }
  y_bar_w <- sum(w * y) / sw
  ss_total <- sum(w * (y - y_bar_w)^2)
  ss_resid <- sum(w * resid^2)
  if (!is.finite(ss_total) || ss_total <= 0) {
    return(NA_real_)
  }
  ss_effect <- ss_total - ss_resid
  mse <- ss_resid / df_resid
  omega2 <- (ss_effect - df_effect * mse) / (ss_total + mse)
  if (!is.finite(omega2)) {
    return(NA_real_)
  }
  max(0, omega2)
}

pick_es_type_lm <- function(effect_size) {
  if (identical(effect_size, "none")) NA_character_ else effect_size
}

pick_es_value_lm <- function(model_stats, effect_size) {
  if (identical(effect_size, "none")) {
    return(NA_real_)
  }
  switch(
    effect_size,
    f2 = model_stats$f2,
    d = model_stats$d,
    g = model_stats$g,
    omega2 = model_stats$omega2,
    spicy_abort(paste0("Unknown `effect_size`: ", effect_size), class = "spicy_invalid_input")
  )
}

# ---- Effect-size confidence intervals -----------------------------------
#
# CIs use the modern noncentral-distribution inversion approach
# (Steiger & Fouladi 1997; Steiger 2004; Goulet-Pelletier & Cousineau
# 2018, 2021). Verified empirically against the `effectsize` package.

find_ncp_t_lm <- function(t_obs, df, p) {
  if (
    !is.finite(t_obs) ||
      !is.finite(df) ||
      df <= 0 ||
      !is.finite(p) ||
      p <= 0 ||
      p >= 1
  ) {
    return(NA_real_)
  }

  pt_diff <- function(ncp) {
    suppressWarnings(stats::pt(t_obs, df = df, ncp = ncp)) - p
  }

  half_width <- max(50, 5 * abs(t_obs) + 20)
  lo <- t_obs - half_width
  hi <- t_obs + half_width
  f_lo <- pt_diff(lo)
  f_hi <- pt_diff(hi)
  expand <- 0L
  while (
    is.finite(f_lo) &&
      is.finite(f_hi) &&
      f_lo * f_hi > 0 &&
      expand < 6L
  ) {
    half_width <- half_width * 2
    lo <- t_obs - half_width
    hi <- t_obs + half_width
    f_lo <- pt_diff(lo)
    f_hi <- pt_diff(hi)
    expand <- expand + 1L
  }
  if (!is.finite(f_lo) || !is.finite(f_hi) || f_lo * f_hi > 0) {
    return(NA_real_)
  }

  tryCatch(
    stats::uniroot(
      pt_diff,
      interval = c(lo, hi),
      tol = 1e-8,
      maxiter = 200
    )$root,
    error = function(e) NA_real_
  )
}

find_ncp_f_lm <- function(f_obs, df1, df2, p) {
  if (
    !is.finite(f_obs) ||
      f_obs < 0 ||
      !is.finite(df1) ||
      df1 <= 0 ||
      !is.finite(df2) ||
      df2 <= 0 ||
      !is.finite(p) ||
      p <= 0 ||
      p >= 1
  ) {
    return(NA_real_)
  }

  pf_diff <- function(ncp) {
    suppressWarnings(stats::pf(f_obs, df1 = df1, df2 = df2, ncp = ncp)) - p
  }

  if (pf_diff(0) <= 0) {
    return(0)
  }

  hi <- max(100, 5 * f_obs * (df1 + df2))
  f_hi <- pf_diff(hi)
  expand <- 0L
  while (is.finite(f_hi) && f_hi > 0 && expand < 6L) {
    hi <- hi * 2
    f_hi <- pf_diff(hi)
    expand <- expand + 1L
  }
  if (!is.finite(f_hi) || f_hi > 0) {
    return(NA_real_)
  }

  tryCatch(
    stats::uniroot(
      pf_diff,
      interval = c(0, hi),
      tol = 1e-8,
      maxiter = 200
    )$root,
    error = function(e) NA_real_
  )
}

compute_smd_ci_lm <- function(fit, ci_level, hedges_correct) {
  d <- unname(stats::coef(fit)[2]) / summary(fit)$sigma
  if (!is.finite(d)) {
    return(c(NA_real_, NA_real_))
  }

  predictor_name <- all.vars(stats::formula(fit))[2]
  mf <- stats::model.frame(fit)
  x <- mf[[predictor_name]]
  if (is.null(x) || !is.factor(x)) {
    return(c(NA_real_, NA_real_))
  }
  group_counts <- table(x)
  if (length(group_counts) != 2L) {
    return(c(NA_real_, NA_real_))
  }
  n1 <- as.integer(group_counts[1])
  n2 <- as.integer(group_counts[2])
  df_resid <- stats::df.residual(fit)
  if (!is.finite(df_resid) || df_resid <= 1) {
    return(c(NA_real_, NA_real_))
  }

  n_harm <- (n1 * n2) / (n1 + n2)
  t_obs <- d * sqrt(n_harm)
  alpha <- 1 - ci_level

  ncp_lo <- find_ncp_t_lm(t_obs, df_resid, 1 - alpha / 2)
  ncp_hi <- find_ncp_t_lm(t_obs, df_resid, alpha / 2)

  bounds <- c(ncp_lo, ncp_hi) / sqrt(n_harm)
  if (isTRUE(hedges_correct)) {
    j <- 1 - 3 / (4 * df_resid - 1)
    bounds <- j * bounds
  }
  bounds
}

extract_lm_f_stat <- function(fit) {
  sm <- summary(fit)
  fst <- sm$fstatistic
  if (is.null(fst) || length(fst) < 3L) {
    return(NULL)
  }
  list(
    f_obs = unname(fst[["value"]]),
    df1 = unname(fst[["numdf"]]),
    df2 = unname(fst[["dendf"]])
  )
}

# Internal: F-stat restricted to a single focal term (partial F via
# `drop1`). When `focal_term = NULL`, returns the model-level F via
# `extract_lm_f_stat()`, which for a bivariate `y ~ x` model coincides
# with the focal-term F. With covariates (`y ~ x + cov1 + cov2`),
# `focal_term = "x"` returns the partial F restricted to x — the
# correct quantity for partial f² / partial ω² CI inversion under
# adjustment.
extract_lm_focal_f_stat <- function(fit, focal_term = NULL) {
  if (is.null(focal_term)) {
    return(extract_lm_f_stat(fit))
  }
  d1 <- tryCatch(
    suppressWarnings(
      stats::drop1(fit, scope = stats::reformulate(focal_term), test = "F")
    ),
    error = function(e) NULL
  )
  if (
    is.null(d1) ||
      nrow(d1) < 2L ||
      !"F value" %in% names(d1) ||
      !"Df" %in% names(d1)
  ) {
    return(NULL)
  }
  f_obs <- d1[["F value"]][2]
  df1 <- d1[["Df"]][2]
  if (!is.finite(f_obs) || !is.finite(df1) || df1 < 1L) {
    return(NULL)
  }
  list(
    f_obs = f_obs,
    df1 = df1,
    df2 = stats::df.residual(fit)
  )
}

compute_omega2_ci_lm <- function(fit, ci_level, focal_term = NULL) {
  fs <- extract_lm_focal_f_stat(fit, focal_term)
  if (is.null(fs) || !is.finite(fs$f_obs) || fs$f_obs <= 0) {
    return(c(NA_real_, NA_real_))
  }
  alpha <- 1 - ci_level
  ncp_lo <- find_ncp_f_lm(fs$f_obs, fs$df1, fs$df2, 1 - alpha / 2)
  ncp_hi <- find_ncp_f_lm(fs$f_obs, fs$df1, fs$df2, alpha / 2)
  if (anyNA(c(ncp_lo, ncp_hi))) {
    return(c(NA_real_, NA_real_))
  }
  n_total <- fs$df1 + fs$df2 + 1L
  bounds <- c(ncp_lo, ncp_hi) / (c(ncp_lo, ncp_hi) + n_total)
  pmax(0, bounds)
}

compute_f2_ci_lm <- function(fit, ci_level, focal_term = NULL) {
  fs <- extract_lm_focal_f_stat(fit, focal_term)
  if (is.null(fs) || !is.finite(fs$f_obs) || fs$f_obs <= 0) {
    return(c(NA_real_, NA_real_))
  }
  alpha <- 1 - ci_level
  ncp_lo <- find_ncp_f_lm(fs$f_obs, fs$df1, fs$df2, 1 - alpha / 2)
  ncp_hi <- find_ncp_f_lm(fs$f_obs, fs$df1, fs$df2, alpha / 2)
  if (anyNA(c(ncp_lo, ncp_hi))) {
    return(c(NA_real_, NA_real_))
  }
  n_total <- fs$df1 + fs$df2 + 1L
  c(ncp_lo, ncp_hi) / n_total
}

compute_es_ci_lm <- function(fit, effect_size, ci_level, focal_term = NULL) {
  if (identical(effect_size, "none")) {
    return(c(NA_real_, NA_real_))
  }
  switch(
    effect_size,
    f2 = compute_f2_ci_lm(fit, ci_level, focal_term = focal_term),
    d = compute_smd_ci_lm(fit, ci_level, hedges_correct = FALSE),
    g = compute_smd_ci_lm(fit, ci_level, hedges_correct = TRUE),
    omega2 = compute_omega2_ci_lm(fit, ci_level, focal_term = focal_term),
    spicy_abort(paste0("Unknown `effect_size`: ", effect_size), class = "spicy_invalid_input")
  )
}
