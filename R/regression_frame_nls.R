# ---------------------------------------------------------------------------
# Phase 6j: as_regression_frame() method for stats::nls.
#
# stats::nls -- non-linear least squares. Class = "nls". Used in
# pharmacokinetics (Michaelis-Menten), dose-response (Hill), growth
# curves (3- / 4-parameter logistic / Gompertz), and other contexts
# where the response is a known non-linear function of the predictors
# with parameters to be estimated.
#
# Key design choices:
#   * No intercept / factor / reference-row logic. The "coefficients"
#     are the model's free parameters (e.g. A, k for an exponential
#     decay; Asym, xmid, scal for SSlogis). Each parameter is its own
#     row in the coefs table; parent_var = term, label = term.
#   * Inference: Wald-t with df = df.residual(fit), byte-equivalent
#     to summary(fit)$coefficients (Estimate / Std. Error / t value /
#     Pr(>|t|)).
#   * stats::terms(fit) and stats::model.frame(fit) BOTH ERROR for
#     nls (the RHS has parameter names mixed with data names, so the
#     terms machinery cannot parse it). The frame does not need them
#     because there are no factor predictors to detect.
#   * No supports$ame (marginaleffects support for nls is partial),
#     no supports$exponentiate (not meaningful), no supports$
#     classical_r2 (no canonical R^2 for non-linear fits).
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `nls` fits (stats::nls()).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.nls <- function(fit,
                                     vcov = "model",
                                     vcov_label = NULL,
                                     ci_level = 0.95,
                                     ci_method = NULL,
                                     model_id = "M1",
                                     ...) {
  coefs <- .nls_coefs(fit, ci_level = ci_level)
  info  <- .nls_info(fit,
                     vcov_kind  = vcov,
                     vcov_label = vcov_label,
                     ci_level   = ci_level,
                     ci_method  = ci_method,
                     model_id   = model_id)

  new_regression_frame(coefs, info, fit)
}


# ---- Internal helpers -----------------------------------------------------

# Build the coefs tibble for an nls fit. The "coefficients" here are
# the free model parameters; no factor predictors or reference rows.
.nls_coefs <- function(fit, ci_level) {
  cf <- stats::coef(fit)
  est <- unname(cf)
  nm  <- names(cf)
  V <- as.matrix(stats::vcov(fit))
  se <- sqrt(diag(V))[nm]

  # Read inference directly from summary -- byte-equivalent to nls's
  # native report.
  sm <- summary(fit)$coefficients
  if (!is.null(sm) && all(c("t value", "Pr(>|t|)") %in% colnames(sm))) {
    stat    <- unname(sm[nm, "t value"])
    p_value <- unname(sm[nm, "Pr(>|t|)"])
  } else {
    stat    <- est / se                                                # nocov
    p_value <- 2 * stats::pnorm(-abs(stat))                            # nocov
  }
  dfr <- tryCatch(stats::df.residual(fit), error = function(e) Inf)
  if (is.null(dfr) || !is.finite(dfr)) dfr <- Inf
  df <- rep(as.numeric(dfr), length(est))
  t_crit <- stats::qt(0.5 + ci_level / 2, df = dfr)
  ci_lower <- est - t_crit * se
  ci_upper <- est + t_crit * se

  data.frame(
    term             = nm,
    parent_var       = nm,
    label            = nm,
    factor_level_pos = rep(NA_integer_, length(nm)),
    is_ref           = rep(FALSE, length(nm)),
    estimate_type    = rep("B", length(nm)),
    estimate         = est,
    std_error        = se,
    df               = as.numeric(df),
    statistic        = stat,
    p_value          = p_value,
    ci_lower         = ci_lower,
    ci_upper         = ci_upper,
    test_type        = rep("t", length(nm)),
    stringsAsFactors = FALSE
  )
}


# Build the info list for an nls fit.
.nls_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method, model_id) {
  # The response variable name is the first var in the formula -- safe
  # for nls since formula(fit) returns the formula as the user wrote it.
  dv <- tryCatch(all.vars(stats::formula(fit))[1L],
                 error = function(e) "response")
  dv_label <- dv

  fam <- list(family = "gaussian", link = "identity")
  if (is.null(ci_method)) ci_method <- "wald"

  fit_stats <- list(
    r_squared      = NA_real_,
    adj_r_squared  = NA_real_,
    pseudo_r2      = NULL,
    aic            = tryCatch(stats::AIC(fit), error = function(e) NA_real_),
    bic            = tryCatch(stats::BIC(fit), error = function(e) NA_real_),
    log_lik        = tryCatch(as.numeric(stats::logLik(fit)),
                              error = function(e) NA_real_),
    deviance       = tryCatch(suppressWarnings(stats::deviance(fit)),
                              error = function(e) NA_real_),
    sigma          = tryCatch(stats::sigma(fit), error = function(e) NA_real_),
    nobs           = as.integer(stats::nobs(fit))
  )

  supports <- list(
    ame                 = FALSE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = FALSE,
    standardise_refit   = FALSE
  )

  formula_string <- tryCatch(deparse1(stats::formula(fit)),
                              error = function(e) NA_character_)

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = !is.null(stats::weights(fit)) &&
                            length(stats::weights(fit)) > 0L,
    weighted_n            = NA_real_,
    title_prefix          = "Non-linear least squares regression",
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    nls_formula           = formula_string,
    parameter_names       = names(stats::coef(fit))
  )

  list(
    class          = "nls",
    family         = fam,
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(stats::nobs(fit)),
    n_groups       = NULL,
    weights_kind   = "none",
    random_effects = empty_random_effects(),
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% "Classical",
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}
