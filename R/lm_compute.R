# Internal lm-specific computation backbone for table_continuous_lm():
# model stats (R^2 / F / AIC), partial effect sizes (omega^2 / eta^2 /
# f^2 / Cohen's d) and their noncentral CIs, and the EMM average row.
# The class-generic vcov family + coefficient / Wald inference live in
# R/vcov.R.

compute_lm_model_stats <- function(fit, focal_term = NULL) {
  sm <- summary(fit)
  r2 <- unname(sm$r.squared) # always overall R^2 (model-level)
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
    spicy_abort(
      paste0("Unknown `effect_size`: ", effect_size),
      class = "spicy_invalid_input"
    )
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

# Internal: model.matrix column masks for the Type-II nested pair of
# one focal term. Type II (Fox 2016 ch. 8; Fox & Weisberg 2019;
# `car::Anova(type = 2)`) tests a term T by comparing the two nested
# models { all terms that do NOT contain T } vs { those terms + T }:
# BOTH models exclude every higher-order relative of T (A:B is out of
# both sides when T = A), which respects the principle of marginality
# and makes the test invariant to the factor coding (treatment / sum /
# Helmert / polynomial). A term S "contains" T when every variable of
# T also appears in S (car's `is.relative()`). Shared by the lm partial
# F (`compute_lm_type2_f_stat`) and the glm partial LRT
# (`compute_glm_type2_lrt` in R/glm_compute.R). Returns NULL when
# `focal_term` is not a term label of `trm`.
type2_nested_column_masks <- function(trm, asgn, focal_term) {
  term_labels <- attr(trm, "term.labels")
  k <- match(focal_term, term_labels)
  if (is.na(k)) {
    return(NULL)
  }
  fac <- attr(trm, "factors")
  contains_focal <- vapply(
    seq_along(term_labels),
    function(j) j != k && all(fac[, k] == 0 | fac[, j] != 0),
    logical(1)
  )
  excluded <- c(k, which(contains_focal))
  keep_base <- !(asgn %in% excluded)
  list(
    keep_base = keep_base,
    keep_with = keep_base | asgn == k,
    has_relatives = any(contains_focal)
  )
}

# Internal: F-stat restricted to a single focal term. When
# `focal_term = NULL`, returns the model-level F via
# `extract_lm_f_stat()`, which for a bivariate `y ~ x` model coincides
# with the focal-term F. With a focal term, returns the
# marginality-respecting Type-II partial F (`compute_lm_type2_f_stat`):
# for additive models this is the classic partial F -- SS(term | all
# other terms) over the full-model MSE, identical to `drop1` -- and
# with interactions the main-effect test excludes the higher-order
# terms from BOTH nested models instead of holding their columns fixed
# (a forced-scope `drop1`, i.e. a contrast-dependent Type-III-style
# test).
extract_lm_focal_f_stat <- function(fit, focal_term = NULL) {
  if (is.null(focal_term)) {
    return(extract_lm_f_stat(fit))
  }
  tryCatch(
    suppressWarnings(compute_lm_type2_f_stat(fit, focal_term)),
    error = function(e) NULL
  )
}

# Internal: Type-II partial F for `focal_term` by nested refits on the
# fitted estimation sample. Mirrors the `stats::drop1.lm` internals --
# column subsets of the fitted model.matrix refit via lm.fit / lm.wfit
# with the fit's own response, weights, and offset, so the user's
# `data` expression is never re-evaluated and the additive-case values
# are bit-identical to the previous drop1-based ones. df1 is the
# difference of effective ranks (not a naive column count), so aliased
# collinear columns are handled. The error term is the FULL model's
# MSE (`car::Anova(type = 2)` convention), so for the highest-order
# term Type II coincides with drop1 exactly.
compute_lm_type2_f_stat <- function(fit, focal_term) {
  x <- stats::model.matrix(fit)
  asgn <- attr(x, "assign")
  if (is.null(asgn) || length(asgn) != ncol(x)) {
    return(NULL)
  }
  masks <- type2_nested_column_masks(stats::terms(fit), asgn, focal_term)
  if (is.null(masks)) {
    return(NULL)
  }
  y <- fit$residuals + fit$fitted.values
  wt <- fit$weights
  off <- fit$offset
  rss_rank <- function(keep) {
    if (!any(keep)) {
      # Empty base model (no-intercept fit whose only term is the
      # focal): the offset-adjusted response is the residual.
      r <- if (is.null(off)) y else y - off
      rss <- if (is.null(wt)) sum(r^2) else sum(wt * r^2)
      return(list(rss = rss, rank = 0L))
    }
    z <- if (is.null(wt)) {
      stats::lm.fit(x[, keep, drop = FALSE], y, offset = off)
    } else {
      stats::lm.wfit(x[, keep, drop = FALSE], y, wt, offset = off)
    }
    rss <- if (is.null(wt)) sum(z$residuals^2) else sum(wt * z$residuals^2)
    list(rss = rss, rank = z$rank)
  }
  base <- rss_rank(masks$keep_base)
  augmented <- if (masks$has_relatives) {
    rss_rank(masks$keep_with)
  } else {
    # No higher-order relative contains the focal term: the augmented
    # model IS the fitted model, so reuse its deviance and rank.
    list(rss = stats::deviance(fit), rank = fit$rank)
  }
  df1 <- augmented$rank - base$rank
  df2 <- stats::df.residual(fit)
  if (!is.finite(df1) || df1 < 1L || !is.finite(df2) || df2 <= 0) {
    return(NULL)
  }
  f_obs <- (max(0, base$rss - augmented$rss) / df1) /
    (stats::deviance(fit) / df2)
  if (!is.finite(f_obs)) {
    return(NULL)
  }
  list(
    f_obs = f_obs,
    df1 = df1,
    df2 = df2
  )
}

# CI for omega^2 via noncentral-F inversion (Steiger 2004). Two
# regimes, switched by `focal_term`:
#   * `focal_term = NULL` (model-level Hays omega^2): bounds =
#     ncp / (ncp + N) where N = df1 + df2 + 1; this matches the
#     global-F partition SS_total = SS_model + SS_resid, with
#     df_total = N - 1.
#   * `focal_term != NULL` (partial omega^2): the
#     `effectsize::omega_squared(partial = TRUE)` convention. The
#     noncentrality inversion runs at the F-value EQUIVALENT of the
#     omega^2 point estimate, F_om = (omega^2_p / df1) /
#     ((1 - omega^2_p) / df2) -- the F that would produce a partial
#     eta^2 equal to omega^2_p -- and the ncp bounds map through the
#     partial transform ncp / (ncp + df2). Inverting at the RAW
#     partial F would reproduce the partial-eta^2 CI (Smithson 2003),
#     which is systematically wider to the right than the omega^2 CI
#     because omega^2 shrinks the point estimate (audit phase 2,
#     finding 24).
compute_omega2_ci_lm <- function(fit, ci_level, focal_term = NULL) {
  fs <- extract_lm_focal_f_stat(fit, focal_term)
  if (is.null(fs) || !is.finite(fs$f_obs) || fs$f_obs <= 0) {
    return(c(NA_real_, NA_real_))
  }
  alpha <- 1 - ci_level
  if (is.null(focal_term)) {
    ncp_lo <- find_ncp_f_lm(fs$f_obs, fs$df1, fs$df2, 1 - alpha / 2)
    ncp_hi <- find_ncp_f_lm(fs$f_obs, fs$df1, fs$df2, alpha / 2)
    if (anyNA(c(ncp_lo, ncp_hi))) {
      return(c(NA_real_, NA_real_))
    }
    denom <- fs$df1 + fs$df2 + 1L
    bounds <- c(ncp_lo, ncp_hi) / (c(ncp_lo, ncp_hi) + denom)
    return(pmax(0, bounds))
  }
  omega2_p <- compute_lm_partial_omega2(fit, fs)
  if (!is.finite(omega2_p) || omega2_p >= 1) {
    return(c(NA_real_, NA_real_))
  }
  f_om <- (omega2_p / fs$df1) / ((1 - omega2_p) / fs$df2)
  ncp_lo <- find_ncp_f_lm(f_om, fs$df1, fs$df2, 1 - alpha / 2)
  ncp_hi <- find_ncp_f_lm(f_om, fs$df1, fs$df2, alpha / 2)
  if (anyNA(c(ncp_lo, ncp_hi))) {
    return(c(NA_real_, NA_real_))
  }
  bounds <- c(ncp_lo, ncp_hi) / (c(ncp_lo, ncp_hi) + fs$df2)
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

# Internal: reorder prediction-design columns to the fitted
# coefficient vector BY NAME before any `design %*% coef` product.
# A silent positional product is exactly the failure mode behind the
# ordered-factor audit findings (contr.poly coefficients multiplied
# against treatment-coded columns: same length, different meanings).
# Aborts loudly on any column-set mismatch instead of recycling or
# reordering silently. The mismatch arm is reachable from user input:
# a factor covariate with a declared-but-unobserved level makes
# `lm()` drop the level at fit time (model.frame drops unused
# levels) while the prediction design still expands it, so the
# design gains a column the fit has no coefficient for.
align_design_to_coef <- function(design, cf) {
  cf_names <- names(cf)
  if (
    is.null(cf_names) ||
      is.null(colnames(design)) ||
      anyDuplicated(cf_names) > 0L ||
      !setequal(colnames(design), cf_names)
  ) {
    spicy_abort(
      c(
        "The prediction design does not match the fitted coefficients (column-set mismatch).",
        "i" = "This can happen when a factor covariate declares levels that never occur in the data. Drop the empty levels first, e.g. `data$cov <- droplevels(data$cov)`.",
        "i" = "If all covariate levels are observed, this is a bug in spicy: please report it at https://github.com/amaltawfik/spicy/issues."
      ),
      class = "spicy_internal_invariant"
    )
  }
  design[, cf_names, drop = FALSE]
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
    # `"balanced"`: factor / character / logical covariates expanded
    # over their observed level cross-product; numeric covariates
    # fixed at the sample mean. Logicals are two-level factors to
    # `lm()` (model.matrix encodes a `<name>TRUE` dummy and registers
    # contr.treatment in fit$contrasts), so they must be expanded like
    # factors -- both for the estimand (balanced = equal FALSE/TRUE
    # weight, the emmeans convention) and mechanically: freezing the
    # column at its numeric mean made model.matrix(contrasts.arg =
    # fit$contrasts) error with "contrasts can be applied only to
    # factors" (wave-2 vignette review, 2026-08-05).
    factor_idx <- vapply(
      covariates_observed,
      function(z) is.factor(z) || is.character(z) || is.logical(z),
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
      # Restore the original factor encoding -- INCLUDING the
      # `ordered` class -- so `model.matrix()` produces the same
      # contrast columns as the fitted model. Dropping the ordered
      # class here rebuilt an ordered covariate with treatment
      # columns against contr.poly coefficients (audit phase 2,
      # finding 21).
      for (nm in names(factor_covs)) {
        if (is.factor(covariates_observed[[nm]])) {
          grid[[nm]] <- factor(
            grid[[nm]],
            levels = levels(covariates_observed[[nm]]),
            ordered = is.ordered(covariates_observed[[nm]])
          )
        } else if (is.logical(covariates_observed[[nm]])) {
          # Back to logical so model.matrix() re-derives the same
          # `<name>TRUE` dummy the fit was built with.
          grid[[nm]] <- as.logical(grid[[nm]])
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

  # `contrasts.arg = fit$contrasts` pins the prediction grid to the
  # coding the model was actually fitted with (treatment, poly, or a
  # user-supplied contrast matrix), and the columns are then aligned
  # to the coefficient vector BY NAME before the caller multiplies.
  design <- stats::model.matrix(
    stats::delete.response(stats::terms(fit)),
    newdata,
    contrasts.arg = fit$contrasts
  )
  design <- align_design_to_coef(design, stats::coef(fit))
  # Weighted fit + "proportional": Stata `margins` after a weighted
  # regression averages the predictions with the case weights -- the
  # empirical covariate distribution being standardized over is the
  # weighted one (matches marginaleffects::avg_predictions(wts = )).
  # The observed rows and the fit rows are the same complete-case
  # sample, so weights(fit) aligns row by row. The "balanced" grid
  # stays equal-weight by construction (emmeans / SPSS EMMEANS
  # convention).
  if (identical(method, "proportional") && has_covs) {
    w <- stats::weights(fit)
    if (!is.null(w)) {
      return(colSums(design * w) / sum(w))
    }
  }
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
    spicy_abort(
      paste0("Unknown `effect_size`: ", effect_size),
      class = "spicy_invalid_input"
    )
  )
}
