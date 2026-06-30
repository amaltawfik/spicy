# Internal lm-specific computation backbone for table_continuous_lm():
# model stats (R^2 / F / AIC), partial effect sizes (omega^2 / eta^2 /
# f^2 / Cohen's d) and their noncentral CIs, and the EMM average row.
# The class-generic vcov family + coefficient / Wald inference live in
# R/vcov.R.

compute_lm_model_stats <- function(fit, focal_term = NULL) {
  sm <- summary(fit)
  r2 <- unname(sm$r.squared)         # always overall R^2 (model-level)
  adj_r2 <- unname(sm$adj.r.squared) # always overall adj. R^2
  sigma_hat <- unname(sm$sigma)
  df_resid <- stats::df.residual(fit)
  cf <- stats::coef(fit)
  # A (near-)perfect fit leaves the residual SD ~ 0 (QR can leave ~1e-16 on
  # some platforms), which would make Cohen's d astronomical instead of
  # undefined. Treat sigma below the machine-precision floor of the
  # fitted-value scale as zero so d / g fall back to NA.
  sigma_floor <- sqrt(.Machine$double.eps) * sqrt(mean(stats::fitted(fit)^2))

  if (is.null(focal_term)) {
    # Bivariate path: every non-intercept coef belongs to the focal
    # predictor, so the model-level f^2 / omega^2 coincide with the
    # focal-term effect size.
    df_effect <- length(cf) - 1L
    f2 <- if (is.na(r2) || r2 >= 1) NA_real_ else r2 / (1 - r2)
    d <- if (
      length(cf) < 2L ||
        !is.finite(sigma_hat) ||
        sigma_hat <= sigma_floor ||
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
    # Covariate-adjusted path: f^2 and omega^2 are restricted to the focal
    # term via partial F (see `extract_lm_focal_f_stat`). Cohen's d
    # and Hedges' g are undefined under adjustment and the public
    # API rejects them upstream; we still set them to NA defensively
    # in case this helper is reached on an unexpected path.
    fs <- extract_lm_focal_f_stat(fit, focal_term)
    if (is.null(fs) || !is.finite(fs$f_obs) || fs$f_obs <= 0) {
      f2 <- NA_real_
      omega2 <- NA_real_
    } else {
      # Partial f^2 = F * df1 / df_resid. Equivalent to
      # SS_focal / SS_residual where SS_focal = F * df1 * MSE_full.
      f2 <- fs$f_obs * fs$df1 / fs$df2
      omega2 <- compute_lm_partial_omega2(fit, fs)
    }
    d <- NA_real_
    g <- NA_real_
  }

  list(r2 = r2, adj_r2 = adj_r2, f2 = f2, d = d, g = g, omega2 = omega2)
}

# Internal: partial omega^2 for a focal term using the Olejnik & Algina
# (2003) formula (also used by `effectsize::omega_squared(partial =
# TRUE)` and SPSS UNIANOVA):
#   omega^2_partial = (SS_effect - df_effect . MSE) / (SS_effect + (N - df_effect) . MSE)
# where SS_effect = F . df_effect . MSE_full and MSE_full = SS_resid / df_resid.
# Note this differs from the *model-level* Hays omega^2 in `compute_lm_omega2()`
# (denominator uses SS_total): the partial form normalises by the
# effect-only sum of squares plus residual sum of squares, which is
# the correct partial-effect quantity in a covariate-adjusted model.
compute_lm_partial_omega2 <- function(fit, fs) {
  rss_full <- stats::deviance(fit)
  # A (near-)perfect fit has rss ~ 0; QR can leave ~1e-16 on some platforms,
  # so compare against the machine-precision floor of the fitted-value scale
  # rather than testing the exact zero (which is platform-fragile).
  scale_ref <- sum(stats::fitted(fit)^2)
  if (!is.finite(rss_full) || rss_full <= scale_ref * .Machine$double.eps) {
    return(NA_real_)
  }
  mse_full <- rss_full / fs$df2
  ss_focal <- fs$f_obs * fs$df1 * mse_full
  n <- stats::nobs(fit)
  if (!is.finite(n) || n <= fs$df1) {
    return(NA_real_)
  }
  omega2 <- (ss_focal - fs$df1 * mse_full) /
    (ss_focal + (n - fs$df1) * mse_full)
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
  # nocov start: defensive. After the guards above ss_total is finite > 0,
  # mse >= 0 finite and ss_effect finite, so the denominator (ss_total + mse)
  # is > 0 and the ratio is always finite -- no valid `fit` can reach here.
  if (!is.finite(omega2)) {
    return(NA_real_)
  }
  # nocov end
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
  # nocov start: defensive. pf(f_obs, df1, df2, ncp) -> 0 (finite) as ncp
  # grows for any finite f_obs >= 0, and 6 doublings widen `hi` by 64x, which
  # always overshoots the root; f_hi can neither stay > 0 nor become
  # non-finite, so this NA return is unreachable from a valid F-stat.
  if (!is.finite(f_hi) || f_hi > 0) {
    return(NA_real_)
  }
  # nocov end

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
  sigma_hat <- summary(fit)$sigma
  # A (near-)perfect fit leaves the residual SD ~ 0 (QR can leave ~1e-16 on
  # some platforms), which would make d astronomical instead of undefined.
  # Treat sigma below the machine-precision floor of the fitted-value scale
  # as zero so the CI falls back to NA.
  sigma_floor <- sqrt(.Machine$double.eps) * sqrt(mean(stats::fitted(fit)^2))
  d <- unname(stats::coef(fit)[2]) / sigma_hat
  if (!is.finite(d) || sigma_hat <= sigma_floor) {
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
# `focal_term = "x"` returns the partial F restricted to x -- the
# correct quantity for partial f^2 / partial omega^2 CI inversion under
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

# CI for eta^2 / omega^2 via noncentral-F inversion (Steiger 2004). Two
# regimes, switched by `focal_term`:
#   * `focal_term = NULL` (model-level R^2): bounds = ncp / (ncp + N)
#     where N = df1 + df2 + 1; this matches the global-F partition
#     SS_total = SS_model + SS_resid, with df_total = N - 1.
#   * `focal_term != NULL` (partial eta^2): bounds = ncp / (ncp + df2);
#     this matches the partial-eta^2 definition
#     eta^2_p = SS_effect / (SS_effect + SS_error) (Smithson 2003;
#     used by `effectsize::eta_squared(partial = TRUE)`).
# In a covariate-adjusted model the two formulas diverge -- the
# partial form is the correct one for individual-term inference.
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
  denom <- if (is.null(focal_term)) fs$df1 + fs$df2 + 1L else fs$df2
  bounds <- c(ncp_lo, ncp_hi) / (c(ncp_lo, ncp_hi) + denom)
  pmax(0, bounds)
}

# CI for Cohen's f^2 via noncentral-F inversion. Same partial vs
# model-level dispatch as `compute_omega2_ci_lm()`:
#   * model-level: f^2 = ncp / N         (N = df1 + df2 + 1)
#   * partial    : f^2 = ncp / df_error  (= eta^2 / (1 - eta^2) under the
#                  partial eta^2 mapping above)
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
  denom <- if (is.null(focal_term)) fs$df1 + fs$df2 + 1L else fs$df2
  c(ncp_lo, ncp_hi) / denom
}

# Internal: build the "average design row" used to compute a single
# covariate-adjusted estimated marginal mean (emmean). Both supported
# methods reduce to the same linear-contrast formula
#   emmean = avg_row %*% beta_hat
#   SE     = sqrt(avg_row %*% V %*% t(avg_row))
# The methods differ only in WHAT is averaged to obtain `avg_row`:
#
#   * `"proportional"` (spicy 0.12+ default; matches Stata `margins`
#     and `marginaleffects::avg_predictions(by = "x")`):
#     newdata = the OBSERVED data with `x` set to the focal level.
#     Predictions are averaged over the empirical joint distribution
#     of covariates -- the G-computation / standardisation estimand.
#     Population-weighted by construction.
#
#   * `"balanced"` (matches `emmeans::emmeans()` default, SPSS
#     UNIANOVA EMMEANS, SAS LSMEANS): newdata = synthetic grid of
#     factor-covariate level combinations x numeric covariates fixed
#     at their sample mean. Each grid cell weighted equally (1 / k).
#     Treats the design as if covariates were balanced -- the
#     "marginal mean assuming a balanced design" estimand.
#
# Behaviour collapses to the bivariate fast path (just `x = focal`)
# when there are no covariates -- both methods coincide trivially.
# When all covariates are numeric / logical (no factor levels to
# expand over), the two methods also coincide because the mean of a
# numeric / logical column is the same regardless of weighting.
#
# The user-facing argument in `table_continuous_lm()` is
# `adjustment`, which dispatches to this helper's `method`. The
# internal name avoids tying the helper to one paradigm vocabulary.
build_emmean_avg_row <- function(
  fit,
  x_focal_level,
  x_levels,
  covariates_observed,
  method = c("proportional", "balanced")
) {
  method <- match.arg(method)
  has_covs <- !is.null(covariates_observed) &&
    ncol(covariates_observed) > 0L

  if (!has_covs) {
    newdata <- data.frame(
      x = factor(x_focal_level, levels = x_levels)
    )
  } else if (method == "proportional") {
    newdata <- covariates_observed
    newdata$x <- factor(
      rep(x_focal_level, nrow(newdata)),
      levels = x_levels
    )
  } else {
    # `"balanced"`: factor / character covariates expanded over their
    # observed level cross-product; numeric / logical covariates
    # fixed at the sample mean. R's `lm()` treats logical as numeric
    # (TRUE -> 1, FALSE -> 0), so we DO NOT expand them as factors:
    # their mean is the proportion of TRUE in the observed data,
    # which matches what `model.matrix()` would produce for a single
    # representative row. This means a logical covariate behaves
    # identically under "proportional" and "balanced".
    factor_idx <- vapply(
      covariates_observed,
      function(z) is.factor(z) || is.character(z),
      logical(1)
    )
    factor_covs <- covariates_observed[, factor_idx, drop = FALSE]
    numeric_covs <- covariates_observed[, !factor_idx, drop = FALSE]

    if (ncol(factor_covs) == 0L) {
      grid <- data.frame(.row = 1L)
      grid$.row <- NULL
    } else {
      level_lists <- lapply(factor_covs, function(z) {
        if (is.factor(z)) {
          levels(droplevels(z))
        } else {
          sort(unique(stats::na.omit(as.character(z))))
        }
      })
      grid <- do.call(
        expand.grid,
        c(
          level_lists,
          list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
        )
      )
      # Restore original factor encoding so `model.matrix()`
      # produces the same contrast columns as the fitted model.
      for (nm in names(factor_covs)) {
        if (is.factor(covariates_observed[[nm]])) {
          grid[[nm]] <- factor(
            grid[[nm]],
            levels = levels(covariates_observed[[nm]])
          )
        }
      }
    }

    n_grid <- max(1L, nrow(grid))
    for (nm in names(numeric_covs)) {
      grid[[nm]] <- mean(numeric_covs[[nm]], na.rm = TRUE)
    }
    grid$x <- factor(
      rep(x_focal_level, n_grid),
      levels = x_levels
    )
    newdata <- grid
  }

  design <- stats::model.matrix(
    stats::delete.response(stats::terms(fit)),
    newdata
  )
  colMeans(design)
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
