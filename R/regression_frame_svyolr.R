# ---------------------------------------------------------------------------
# as_regression_frame() method for survey::svyolr() fits: a
# design-weighted cumulative-link (proportional-odds) model.
#
# svyolr is NOT a polr subclass. `class(fit)` is the single string
# "svyolr", on a linearised design and on a replicate design alike
# (svyolr.svyrep.design ends with `class(rval) <- "svyolr"`, so there is
# no svrepolr), and it inherits from nothing. Everything below is written
# rather than borrowed, and the three places where borrowing would have
# been wrong are:
#
#   * `coef(fit)` returns the slopes AND the cut-points, unlike
#     `coef(polr)` -- so `.polr_coefs()`, whose comment says "excludes
#     thresholds by construction", would have published two cut-points as
#     slopes. The slopes are read from `fit$coefficients` and the
#     cut-points from `fit$zeta`, the two slots survey posts explicitly;
#   * `nobs(fit)` is the SUM OF THE WEIGHTS (6194 for 200 schools):
#     survey sets `nobs = sum(wt)` and there is no `nobs.svyolr` to
#     correct it. The observed count comes from the fitted-probability
#     matrix (`.design_fit_n_obs()`), the population from the design;
#   * AIC / BIC / logLik / extractAIC / family() all fail on this class
#     -- design-weighted estimation has no likelihood -- so the
#     information criteria are absent rather than approximated.
#
# Inference is Wald-t at `fit$df.residual`, the value survey writes on
# the object and the denominator `regTermTest()` uses. summary.svyolr
# names its own column "t value"; the normal intervals `confint(fit)`
# returns come from `confint.default`, since survey ships no
# `confint.svyolr` -- an absent method, not a choice of distribution.
# ---------------------------------------------------------------------------

#' `as_regression_frame()` method for `svyolr` fits.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.svyolr <- function(
  fit,
  vcov = "survey-Taylor",
  vcov_label = NULL,
  cluster = NULL,
  cluster_name = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  show_columns = character(0),
  model_id = "M1",
  ...
) {
  .check_survey_available()

  df <- .design_model_df(fit)
  coefs <- .svyolr_coefs(fit, ci_level = ci_level, df = df)
  # Either a no-op (the design-based default) or an abort: the design is
  # the variance authority, so compute_model_vcov() refuses every
  # model-derived estimator for this class.
  coefs <- .apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov,
    cluster,
    ci_level,
    test = "t"
  )
  # Per-category AME on the response scale. avg_slopes() already reads
  # the design vcov (SE 0.0410 against 0.0090 from the naive Hessian);
  # what it does not read is the sampling weights, which
  # `.spicy_ame_fit_wts()` supplies.
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = cluster,
    df = df
  )
  info <- .svyolr_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    df = df
  )

  new_regression_frame(coefs, info, fit)
}


# ---- Internal helpers -----------------------------------------------------

# Build the coefs tibble for a svyolr fit: the SLOPES only. The
# cut-points go to info$extras$thresholds and are promoted to their own
# block by the orchestrator.
.svyolr_coefs <- function(fit, ci_level, df) {
  est <- fit$coefficients %||% numeric(0)
  nm <- names(est)
  # vcov(fit) covers slopes AND cut-points, in that order; subset BY
  # NAME rather than by position so a future ordering change cannot
  # silently pair a slope with a cut-point's variance.
  V <- as.matrix(stats::vcov(fit))
  se <- unname(sqrt(diag(V)[nm]))
  est <- unname(est)

  stat <- est / se
  p_value <- 2 * stats::pt(-abs(stat), df = df)
  crit <- stats::qt(0.5 + ci_level / 2, df = df)

  factor_meta <- detect_factor_term_meta(fit)
  ft <- vapply(
    nm,
    function(n) factor_meta[[n]]$factor_term %||% NA_character_,
    character(1)
  )
  lvl <- vapply(
    nm,
    function(n) factor_meta[[n]]$factor_level %||% NA_character_,
    character(1)
  )
  pos <- vapply(
    nm,
    function(n) factor_meta[[n]]$factor_level_pos %||% NA_integer_,
    integer(1)
  )

  coefs <- data.frame(
    term = nm,
    parent_var = ifelse(is.na(ft), nm, ft),
    label = ifelse(is.na(lvl), nm, lvl),
    factor_level_pos = as.integer(pos),
    is_ref = rep(FALSE, length(nm)),
    estimate_type = rep("B", length(nm)),
    estimate = est,
    std_error = se,
    df = rep(df, length(nm)),
    statistic = stat,
    p_value = p_value,
    ci_lower = est - crit * se,
    ci_upper = est + crit * se,
    test_type = rep("t", length(nm)),
    stringsAsFactors = FALSE
  )

  ref_rows <- .ordinal_reference_rows(fit)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }
  coefs
}


# Build the info list for a svyolr fit.
.svyolr_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id,
  df
) {
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)
  link <- fit$method %||% "logistic"

  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  n_obs <- .design_fit_n_obs(fit)

  # Every likelihood-based statistic is absent for this class, and the
  # two that survey does expose are not what their names suggest:
  # `nobs(fit)` is the sum of the weights, and `fit$deviance` is the
  # weighted objective, a number that grows with the population rather
  # than describing the fit. Both are posted as NA here rather than left
  # out, because an absent field is republished by any caller that asks
  # for the token explicitly.
  fit_stats <- list(
    r_squared = NA_real_,
    adj_r_squared = NA_real_,
    pseudo_r2 = NULL,
    aic = NA_real_,
    bic = NA_real_,
    log_lik = NA_real_,
    deviance = NA_real_,
    sigma = NA_real_,
    nobs = n_obs,
    weighted_nobs = .design_weighted_n(fit, n_obs)
  )

  # nested_lrt = FALSE: there is no likelihood to compare. survey's
  # canonical omnibus test for this class is regTermTest(), a Wald test
  # at the same degrees of freedom the rows use.
  supports <- list(
    ame = TRUE,
    partial_effect_size = FALSE,
    classical_r2 = FALSE,
    nested_lrt = FALSE,
    exponentiate = TRUE,
    standardise_refit = FALSE
  )

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    has_weights = TRUE,
    weighted_n = fit_stats$weighted_nobs,
    title_prefix = paste0(
      "Survey-weighted ",
      tolower(.polr_link_title(link)),
      " regression (",
      .ordinal_assumption_label(link),
      ")"
    ),
    exp_applied = FALSE,
    exp_header = NA_character_,
    response_levels = as.character(fit$lev %||% character(0)),
    thresholds = .polr_thresholds(fit, df = df, test = "t"),
    # Footer disclosure: the sampling scheme, read off the ANALYTIC
    # design, and the degrees of freedom the table's own tests use.
    design_meta = .design_meta_or_null(.design_analytic(fit, n_obs)),
    design_degf_resid = df
  )

  list(
    class = "svyolr",
    family = list(family = "cumulative", link = .polr_link_short(link)),
    dv = dv,
    dv_label = dv_label,
    n_obs = n_obs,
    n_groups = NULL,
    weights_kind = "sampling",
    random_effects = empty_random_effects(),
    fit_stats = fit_stats,
    vcov_kind = vcov_kind,
    vcov_label = vcov_label %||% .design_vcov_label(fit),
    ci_level = as.numeric(ci_level),
    ci_method = ci_method,
    supports = supports,
    extras = extras
  )
}
