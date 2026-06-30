# ---------------------------------------------------------------------------
# Phase 2: as_regression_frame() method for survey::svyglm() fits.
#
# `svyglm` inherits from `glm` and `lm`, so without an explicit method
# spicy's `as_regression_frame.lm()` would dispatch on inheritance and
# treat the fit as a plain glm -- losing the design-based variance,
# the sampling-weights semantics, and the Taylor-linearised CI / p.
# This module ships the explicit method so the design context is
# preserved end-to-end.
#
# Extraction strategy:
#   * coefs from coef(fit) and design-based vcov from vcov(fit)
#     (Taylor-linearisation variance is computed by survey internally).
#   * t-statistic + Wald CI using survey's df.residual().
#   * family from family(fit) (gaussian / quasibinomial / quasipoisson /
#     ...). Title prefix names the design-based context explicitly.
#
# Design doc section 6 + section 12.4 (minimum dependency:
# survey >= 4.4).
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `svyglm` fits.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.svyglm <- function(fit,
                                        vcov = "survey-Taylor",
                                        vcov_label = NULL,
                                        cluster = NULL,
                                        cluster_name = NULL,
                                        ci_level = 0.95,
                                        ci_method = NULL,
                                        show_columns = character(0),
                                        model_id = "M1",
                                        ...) {
  .check_survey_available()

  coefs <- .svyglm_coefs(fit, ci_level = ci_level)
  # CR* -> clubSandwich design-aware vcovCR (Wald z); a no-op for the
  # design-based default ("classical" / "survey-Taylor").
  coefs <- .apply_robust_vcov_to_coefs(coefs, fit, vcov, cluster, ci_level,
                                       test = "z")
  # Design-based response-scale AME (marginaleffects::avg_slopes uses the
  # survey design vcov).
  coefs <- .attach_ame_to_frame_coefs(coefs, fit, ci_level, show_columns)
  info  <- .svyglm_info(fit,
                        vcov_kind  = vcov,
                        vcov_label = vcov_label,
                        ci_level   = ci_level,
                        ci_method  = ci_method,
                        model_id   = model_id)
  if (!vcov %in% c("model", "classical", "survey-Taylor")) {
    info$vcov_label <- .robust_vcov_label(vcov, cluster_name %||% NA_character_)
  }

  new_regression_frame(coefs, info, fit)
}


# ---- Internal helpers -----------------------------------------------------

# Guard: survey must be available to extract from an svyglm object.
.check_survey_available <- function() {
  if (!spicy_pkg_available("survey")) {
    spicy_abort(
      c(
        "Cannot extract a regression frame from a survey::svyglm() fit without `survey`.",
        "i" = "Install survey: `install.packages(\"survey\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
}


# Build the coefs tibble for an svyglm fit. Design-based vcov via
# vcov(fit); Wald inference with t-distribution (df = df.residual()).
.svyglm_coefs <- function(fit, ci_level) {
  est <- stats::coef(fit)
  nm  <- names(est)
  V <- as.matrix(stats::vcov(fit))
  se  <- sqrt(diag(V))

  # survey's summary() returns a coefficient matrix with
  # Estimate / Std. Error / t value / Pr(>|t|). Read directly when
  # available; fall back to recomputing if the API ever changes.
  sm <- tryCatch(summary(fit), error = function(e) NULL)
  if (!is.null(sm) && !is.null(sm$coefficients) &&
        all(c("t value", "Pr(>|t|)") %in% colnames(sm$coefficients))) {
    stat    <- unname(sm$coefficients[nm, "t value"])
    p_value <- unname(sm$coefficients[nm, "Pr(>|t|)"])
  } else {
    stat    <- unname(est) / se                                    # nocov start
    df_resid <- tryCatch(stats::df.residual(fit),
                          error = function(e) Inf)
    if (is.null(df_resid) || !is.finite(df_resid)) df_resid <- Inf
    p_value <- 2 * stats::pt(-abs(stat), df = df_resid)
  }                                                                # nocov end

  # df: survey's t-Wald uses df.residual(). For a sufficiently large
  # design this is large; for highly-stratified designs it can be small.
  dfr <- tryCatch(stats::df.residual(fit), error = function(e) Inf)
  # nocov: df.residual() on a valid svyglm always returns a finite count
  # (length(residuals) - rank via df.residual.default); this z-fallback
  # assignment is a defensive guard for a degenerate fit we can't construct.
  if (is.null(dfr) || !is.finite(dfr)) dfr <- Inf # nocov
  df_vec <- rep(as.numeric(dfr), length(est))

  # Wald CI with t. Falls back to z if df is Inf.
  crit <- stats::qt(0.5 + ci_level / 2, df = df_vec)
  ci_lower <- unname(est) - crit * se
  ci_upper <- unname(est) + crit * se

  # Factor metadata via the polymorphic accessor (added Phase 1 in
  # regression_extract.R). svyglm carries xlevels because it inherits
  # from glm; the helper reads them via the fast path.
  factor_meta <- detect_factor_term_meta(fit)
  ft  <- vapply(nm, function(n) factor_meta[[n]]$factor_term  %||% NA_character_,
                character(1))
  lvl <- vapply(nm, function(n) factor_meta[[n]]$factor_level %||% NA_character_,
                character(1))
  pos <- vapply(nm, function(n) factor_meta[[n]]$factor_level_pos %||% NA_integer_,
                integer(1))

  parent_var <- ifelse(is.na(ft),  nm,  ft)
  label      <- ifelse(is.na(lvl), nm, lvl)

  coefs <- data.frame(
    term             = nm,
    parent_var       = parent_var,
    label            = label,
    factor_level_pos = as.integer(pos),
    is_ref           = rep(FALSE, length(nm)),
    estimate_type    = rep("B", length(nm)),
    estimate         = unname(est),
    std_error        = se,
    df               = df_vec,
    statistic        = stat,
    p_value          = p_value,
    ci_lower         = ci_lower,
    ci_upper         = ci_upper,
    test_type        = rep("t", length(nm)),
    stringsAsFactors = FALSE
  )

  # Append reference-level rows for treatment-coded factor predictors.
  ref_rows <- .svyglm_reference_rows(fit)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }

  coefs
}


# Reference rows mirroring the lm / glm and merMod helpers.
.svyglm_reference_rows <- function(fit) {
  fts <- detect_factor_terms(fit)
  if (length(fts) == 0L) {
    return(.empty_coefs_frame())
  }
  rows <- list()
  for (ft in fts) {
    if (!isTRUE(ft$reference_dropped)) next
    ref_lvl <- ft$reference_level
    term_name <- paste0(ft$factor_term, ref_lvl)
    ref_pos <- match(ref_lvl, ft$levels) %||% NA_integer_
    rows[[length(rows) + 1L]] <- data.frame(
      term             = term_name,
      parent_var       = ft$factor_term,
      label            = ref_lvl,
      factor_level_pos = as.integer(ref_pos),
      is_ref           = TRUE,
      estimate_type    = "B",
      estimate         = NA_real_,
      std_error        = NA_real_,
      df               = NA_real_,
      statistic        = NA_real_,
      p_value          = NA_real_,
      ci_lower         = NA_real_,
      ci_upper         = NA_real_,
      test_type        = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(.empty_coefs_frame())
  }
  do.call(rbind, rows)
}


# Build the info list for an svyglm fit.
.svyglm_info <- function(fit, vcov_kind, vcov_label, ci_level, ci_method,
                          model_id) {
  fam <- stats::family(fit)
  family_info <- list(family = fam$family, link = fam$link)
  dv <- all.vars(stats::formula(fit))[1L]
  dv_label <- .extract_dv_label(fit, dv)
  has_identity_link <- identical(fam$link, "identity")

  # survey's logLik / AIC / BIC / deviance / sigma helpers issue warnings
  # because design-based estimation does not produce a true likelihood.
  # We catch them defensively (the values are reported as NA when the
  # method isn't applicable) and suppress the noise so the renderer
  # footer stays clean.
  fit_stats <- list(
    r_squared     = NA_real_,
    adj_r_squared = NA_real_,
    pseudo_r2     = NULL,
    aic           = tryCatch(suppressWarnings(stats::AIC(fit)),
                              error = function(e) NA_real_),
    bic           = tryCatch(suppressWarnings(stats::BIC(fit)),
                              error = function(e) NA_real_),
    log_lik       = tryCatch(suppressWarnings(
                                as.numeric(stats::logLik(fit))),
                              error = function(e) NA_real_),
    deviance      = tryCatch(suppressWarnings(stats::deviance(fit)),
                              error = function(e) NA_real_),
    sigma         = tryCatch(suppressWarnings(stats::sigma(fit)),
                              error = function(e) NA_real_),
    nobs          = as.integer(stats::nobs(fit))
  )

  if (is.null(ci_method)) ci_method <- "wald"

  # Capabilities. partial_effect_size = FALSE because the f^2 / eta^2
  # framework is not defined for design-based estimation. nested_lrt
  # = FALSE because the canonical comparison in survey is Wald via
  # regTermTest() (LRT requires likelihood that the design-based
  # sandwich doesn't produce). exponentiate is TRUE iff the link is
  # non-identity (logit / log / probit produce OR / IRR / RR).
  supports <- list(
    ame                 = TRUE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = FALSE,
    exponentiate        = !has_identity_link,
    standardise_refit   = TRUE
  )

  # Family-aware title prefix names the design-based context.
  title_prefix <- if (has_identity_link) {
    "Survey-weighted linear regression"
  } else {
    paste0("Survey-weighted ", .svyglm_family_title(fam), " regression")
  }

  extras <- list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = TRUE,
    weighted_n            = .svyglm_weighted_n(fit),
    title_prefix          = title_prefix,
    exp_applied           = FALSE,
    exp_header            = NA_character_,
    design_class          = .svyglm_design_class(fit)
  )

  list(
    class          = "svyglm",
    family         = family_info,
    dv             = dv,
    dv_label       = dv_label,
    n_obs          = as.integer(stats::nobs(fit)),
    n_groups       = NULL,
    weights_kind   = "sampling",
    random_effects = empty_random_effects(),
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% "Design-based (Taylor linearisation)",
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


# Title-case family label. Note: survey uses quasibinomial / quasipoisson
# under the hood because the score variance does not coincide with the
# nominal-likelihood variance; we still display "Logistic" / "Poisson"
# to the user because that is the model class they typed.
.svyglm_family_title <- function(fam) {
  switch(fam$family,
    quasibinomial = "logistic",
    quasipoisson  = "Poisson",
    binomial      = "logistic",
    poisson       = "Poisson",
    Gamma         = "Gamma",
    inverse.gaussian = "inverse-Gaussian",
    gaussian      = "linear",
    paste0(tolower(substr(fam$family, 1L, 1L)),
           substring(fam$family, 2L))
  )
}


# Sum of design weights for the analytic sample. Returns NA when the
# design object cannot be accessed (e.g., serialised fits with detached
# design).
.svyglm_weighted_n <- function(fit) {
  tryCatch({
    des <- fit$survey.design
    if (is.null(des)) return(NA_real_)
    w <- stats::weights(des)
    if (is.null(w) || length(w) == 0L) return(NA_real_)
    sum(w)
  }, error = function(e) NA_real_)
}


# Class of the survey design object (e.g., "twophase" / "svyrep.design" /
# "survey.design2"). Surfaced in info$extras for renderers that want to
# document the design type in the footer.
.svyglm_design_class <- function(fit) {
  tryCatch({
    des <- fit$survey.design
    if (is.null(des)) return(NA_character_)
    class(des)[1L]
  }, error = function(e) NA_character_)
}
