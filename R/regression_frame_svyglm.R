# ---------------------------------------------------------------------------
# as_regression_frame() methods for survey-design fits: the supported one
# (svyglm / svrepglm) and the explicit refusal for svycoxph, which would
# otherwise be swallowed by inheritance (see the bottom of this file).
#
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
as_regression_frame.svyglm <- function(
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

  coefs <- .svyglm_coefs(fit, ci_level = ci_level)
  # `vcov` can only be the design-based default here ("survey-Taylor" /
  # "model" / "classical"): the design is the variance authority, so
  # compute_model_vcov() refuses every model-derived estimator for a
  # survey-design fit, and the shared applier reaches it for anything
  # else. The applier is therefore either a no-op or an abort -- which
  # is why nothing below relabels info$vcov_label for a robust request.
  coefs <- .apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov,
    cluster,
    ci_level,
    test = "z"
  )
  # Design-based response-scale AME (marginaleffects::avg_slopes uses the
  # survey design vcov). `df =`: the AME rows sit under the same column
  # headers as the coefficient rows and must answer to the same
  # reference distribution -- the design's residual degrees of freedom,
  # not the asymptotic normal marginaleffects defaults to.
  coefs <- .attach_ame_to_frame_coefs(
    coefs,
    fit,
    ci_level,
    show_columns,
    vcov_type = vcov,
    cluster = cluster,
    df = .design_model_df(fit)
  )
  info <- .svyglm_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id
  )

  new_regression_frame(coefs, info, fit)
}


# ---- Design-based Cox: explicit refusal -----------------------------------

# class(svycoxph_fit) is c("svycoxph", "coxph"), so WITHOUT this method the
# fit dispatches to as_regression_frame.coxph() by inheritance, walks into
# the coxph extractor and dies there: .coxph_info() calls stats::AIC(fit),
# and survey's extractAIC method stops with the unclassed
# "No AIC for survey models" -- preceded by six lines of design description
# printed as a side effect by the two summary(fit) calls the coxph path
# makes (survey's summary.svycoxph prints the design). A caller could
# neither catch the failure by class nor keep the console clean.
#
# The refusal is deliberate rather than provisional. A design-based Cox
# table is not a coxph table with a different vcov: the coefficients come
# from a design-weighted partial likelihood, the variance from the design,
# there is no likelihood (hence no AIC / BIC / logLik and no Cox-Snell R2),
# and the canonical global test is survey::regTermTest(), not the three
# likelihood-ratio tests a coxph footer reports. Shipping half of that now
# would be worse than saying so.
#
#' `as_regression_frame()` method for `svycoxph` fits (refusal).
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.svycoxph <- function(fit, ...) {
  spicy_abort(
    c(
      sprintf(
        "table_regression() does not support model class %s yet.",
        sQuote(class(fit)[1L])
      ),
      "i" = paste0(
        "Design-based Cox models arrive with the survey-design work; ",
        "a partial table now would report likelihood statistics that do ",
        "not exist for a design-weighted fit."
      ),
      "i" = paste0(
        "For now, `summary(fit)` gives the design-based coefficient ",
        "table and `survey::regTermTest(fit, ~term)` the design-based ",
        "Wald test."
      )
    ),
    class = "spicy_unsupported_class"
  )
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
  nm <- names(est)
  V <- as.matrix(stats::vcov(fit))
  se <- sqrt(diag(V))

  # survey's summary() returns a coefficient matrix with
  # Estimate / Std. Error / t value / Pr(>|t|). Read directly when
  # available; fall back to recomputing if the API ever changes.
  sm <- tryCatch(summary(fit), error = function(e) NULL)
  if (
    !is.null(sm) &&
      !is.null(sm$coefficients) &&
      all(c("t value", "Pr(>|t|)") %in% colnames(sm$coefficients))
  ) {
    stat <- unname(sm$coefficients[nm, "t value"])
    p_value <- unname(sm$coefficients[nm, "Pr(>|t|)"])
  } else {
    stat <- unname(est) / se # nocov start
    df_resid <- tryCatch(stats::df.residual(fit), error = function(e) Inf)
    if (is.null(df_resid) || !is.finite(df_resid)) {
      df_resid <- Inf
    }
    p_value <- 2 * stats::pt(-abs(stat), df = df_resid)
  } # nocov end

  # df: survey's t-Wald uses df.residual(), which is what regTermTest()
  # takes as its denominator too. Read through the shared accessor, which
  # aborts rather than falling back to Inf: a silent z under a footer
  # declaring a t is the failure mode worth refusing.
  df_vec <- rep(.design_model_df(fit), length(est))

  # Wald CI with t at the design's residual degrees of freedom.
  crit <- stats::qt(0.5 + ci_level / 2, df = df_vec)
  ci_lower <- unname(est) - crit * se
  ci_upper <- unname(est) + crit * se

  # Factor metadata via the polymorphic accessor (added Phase 1 in
  # regression_extract.R). svyglm carries xlevels because it inherits
  # from glm; the helper reads them via the fast path.
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

  parent_var <- ifelse(is.na(ft), nm, ft)
  label <- ifelse(is.na(lvl), nm, lvl)

  coefs <- data.frame(
    term = nm,
    parent_var = parent_var,
    label = label,
    factor_level_pos = as.integer(pos),
    is_ref = rep(FALSE, length(nm)),
    estimate_type = rep("B", length(nm)),
    estimate = unname(est),
    std_error = se,
    df = df_vec,
    statistic = stat,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    test_type = rep("t", length(nm)),
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
    if (!isTRUE(ft$reference_dropped)) {
      next
    }
    ref_lvl <- ft$reference_level
    term_name <- paste0(ft$factor_term, ref_lvl)
    ref_pos <- match(ref_lvl, ft$levels) %||% NA_integer_
    rows[[length(rows) + 1L]] <- data.frame(
      term = term_name,
      parent_var = ft$factor_term,
      label = ref_lvl,
      factor_level_pos = as.integer(ref_pos),
      is_ref = TRUE,
      estimate_type = "B",
      estimate = NA_real_,
      std_error = NA_real_,
      df = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      test_type = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(.empty_coefs_frame())
  }
  do.call(rbind, rows)
}


# Build the info list for an svyglm fit.
.svyglm_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id
) {
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
  n_obs <- .design_fit_n_obs(fit)
  fit_stats <- list(
    r_squared = NA_real_,
    adj_r_squared = NA_real_,
    pseudo_r2 = NULL,
    aic = .svyglm_aic(fit),
    bic = tryCatch(suppressWarnings(stats::BIC(fit)), error = function(e) {
      NA_real_
    }),
    log_lik = tryCatch(
      suppressWarnings(
        as.numeric(stats::logLik(fit))
      ),
      error = function(e) NA_real_
    ),
    deviance = tryCatch(
      suppressWarnings(stats::deviance(fit)),
      error = function(e) NA_real_
    ),
    sigma = tryCatch(suppressWarnings(stats::sigma(fit)), error = function(e) {
      NA_real_
    }),
    nobs = n_obs,
    # Sum of design weights: makes the "weighted_nobs" token render
    # (it was silently swallowed -- extras carried the value but the
    # fit-stats materialiser only reads fit_stats).
    weighted_nobs = .design_weighted_n(fit, n_obs)
  )

  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  # Capabilities. partial_effect_size = FALSE because the f^2 / eta^2
  # framework is not defined for design-based estimation. nested_lrt
  # = FALSE because the canonical comparison in survey is Wald via
  # regTermTest() (LRT requires likelihood that the design-based
  # sandwich doesn't produce). exponentiate is TRUE iff the link is
  # non-identity (logit / log / probit produce OR / IRR / RR).
  supports <- list(
    ame = TRUE,
    partial_effect_size = FALSE,
    classical_r2 = FALSE,
    nested_lrt = FALSE,
    exponentiate = !has_identity_link,
    standardise_refit = FALSE
  )

  # Family-aware title prefix names the design-based context.
  title_prefix <- if (has_identity_link) {
    "Survey-weighted linear regression"
  } else {
    paste0("Survey-weighted ", .svyglm_family_title(fam), " regression")
  }

  extras <- list(
    cluster_name = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular = FALSE,
    singular_terms = character(0),
    has_weights = TRUE,
    weighted_n = fit_stats$weighted_nobs,
    title_prefix = title_prefix,
    exp_applied = FALSE,
    exp_header = NA_character_,
    design_class = .svyglm_design_class(fit)
  )

  list(
    class = "svyglm",
    family = family_info,
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


# Title-case family label. Note: survey uses quasibinomial / quasipoisson
# under the hood because the score variance does not coincide with the
# nominal-likelihood variance; we still display "Logistic" / "Poisson"
# to the user because that is the model class they typed.
.svyglm_family_title <- function(fam) {
  # Binomial titles are LINK-aware: a probit svyglm is NOT a logistic
  # regression (title mistitle caught in the Group D verification pass).
  if (fam$family %in% c("binomial", "quasibinomial")) {
    return(switch(
      fam$link,
      "logit" = "logistic",
      "probit" = "probit",
      "cloglog" = "complementary log-log",
      "log" = "log-binomial",
      "binomial"
    ))
  }
  switch(
    fam$family,
    quasipoisson = "Poisson",
    poisson = "Poisson",
    Gamma = "Gamma",
    inverse.gaussian = "inverse-Gaussian",
    gaussian = "linear",
    paste0(tolower(substr(fam$family, 1L, 1L)), substring(fam$family, 2L))
  )
}


# The information criterion of a design-based glm, as a SCALAR.
#
# `AIC.svyglm` does not honour the `stats::AIC` contract: it returns
# survey's `extractAIC.svyglm` verbatim, a named vector of length three
# -- `c(eff.p, AIC, deltabar)`. Stored whole, the downstream compactor
# takes element 1, so the table printed the effective number of design
# parameters (4.6) under the header "AIC" where the criterion itself is
# 2002.2. The element is therefore read BY NAME, here, once.
#
# The value named "AIC" is Lumley & Scott's design-based AIC (deviance
# plus k times the sum of the Rao-Scott eigenvalues), the criterion
# written for model comparison under a complex design and the only
# quantity of its kind published for this class. `BIC.svyglm` is not
# its counterpart -- it requires a `maximal =` model and errors without
# one -- so bic stays NA.
.svyglm_aic <- function(fit) {
  a <- tryCatch(suppressWarnings(stats::AIC(fit)), error = function(e) NULL)
  if (is.null(a) || !is.numeric(a) || length(a) == 0L) {
    return(NA_real_) # nocov
  }
  if ("AIC" %in% names(a)) {
    return(as.numeric(a[["AIC"]]))
  }
  # A future survey that honours the contract returns a scalar; anything
  # else is not an AIC we can name.
  if (length(a) == 1L) {
    return(as.numeric(a)) # nocov
  }
  NA_real_ # nocov
}


# Class of the survey design object (e.g., "twophase" / "svyrep.design" /
# "survey.design2"). Surfaced in info$extras for renderers that want to
# document the design type in the footer.
.svyglm_design_class <- function(fit) {
  tryCatch(
    {
      des <- fit$survey.design
      if (is.null(des)) {
        return(NA_character_)
      }
      class(des)[1L]
    },
    error = function(e) NA_character_
  )
}
