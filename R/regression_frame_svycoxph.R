# ---------------------------------------------------------------------------
# as_regression_frame() methods for survey::svycoxph() fits and their
# replicate-weight sibling svrepcoxph.
#
# A design-based Cox table is not a coxph table with a different vcov:
# the coefficients come from a design-weighted partial likelihood, the
# variance from the design, and there is no likelihood -- so no AIC, no
# BIC, no logLik, no Cox-Snell R2. The canonical omnibus test is
# survey::regTermTest(), not the three likelihood-ratio tests a coxph
# footer reports. Hence siblings of the coxph extractors rather than a
# detour through them, for three reasons that are all measurable:
#
#   * .coxph_info() calls stats::AIC(), BIC() and logLik() bare. On this
#     class AIC() stops with "No AIC for survey models" (and on
#     svrepcoxph with .NotYetImplemented), while logLik() SUCCEEDS on
#     svrepcoxph and returns NA (df = 3). Three engines, three answers,
#     none of them an information criterion.
#   * survey's summary.svycoxph is
#       function(object, ...) { print(object$survey.design, ...); NextMethod() }
#     -- it PRINTS the design on every call, including when only one
#     field is read (`summary(fit)$rsq` prints before returning
#     numeric(0)). The coxph path calls it twice. Nothing below calls it
#     at all: every quantity is read off the fit.
#   * deviance() returns -184.90 on the linearised engine and 0 on the
#     replicate one (survey's deviance.svycoxph reads `object$ll`, which
#     the replicate engine never sets, and falls through to `else 0`).
#     A zero is more dangerous than a negative: it looks like a result.
#
# Degrees of freedom: survey writes them under degf.resid on one engine
# and degf.residual on the other; `.design_model_df()` reads both with
# `[[`, because `degf.resid` is a unique PREFIX of `degf.residual` and
# `$` silently partial-matches. Inference is Wald-t at that value
# (decision 30), so the p-values are recomputed rather than read from
# summary(), whose column is a z.
# ---------------------------------------------------------------------------

#' `as_regression_frame()` method for `svycoxph` fits.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.svycoxph <- function(
  fit,
  vcov = "survey-Taylor",
  vcov_label = NULL,
  cluster = NULL,
  cluster_name = NULL,
  ci_level = 0.95,
  ci_method = NULL,
  show_columns = character(0),
  model_id = "M1",
  exponentiate = FALSE,
  tau = NULL,
  at_time = NULL,
  boot_n = 1000L,
  ...
) {
  .check_survey_available()
  # Before anything is built: the estimand rows would otherwise be
  # constructed and only then meet a validator that does not know their
  # estimate_type.
  .svycoxph_refuse_estimands(show_columns, model_id)

  df <- .design_model_df(fit)
  coefs <- .svycoxph_coefs(fit, ci_level = ci_level, df = df)
  coefs <- .apply_robust_vcov_to_coefs(
    coefs,
    fit,
    vcov,
    cluster,
    ci_level,
    test = "t"
  )
  info <- .svycoxph_info(
    fit,
    vcov_kind = vcov,
    vcov_label = vcov_label,
    ci_level = ci_level,
    ci_method = ci_method,
    model_id = model_id,
    df = df
  )

  ex <- .apply_exp_to_survival_frame(coefs, info, exponentiate)
  frame <- new_regression_frame(ex$coefs, ex$info, fit)
  # Observed event counts per level ("n_events" column), the unweighted
  # robustness figure the field convention asks for beside a weighted
  # estimate. Right-censored single-record responses only: a
  # counting-process response has no per-subject indicator.
  if ("n_events" %in% show_columns) {
    y <- tryCatch(
      stats::model.response(stats::model.frame(fit)),
      error = function(e) NULL
    )
    if (inherits(y, "Surv") && ncol(y) == 2L) {
      frame <- .attach_event_counts(frame, fit, ev = as.integer(y[, 2L]))
    }
  }
  frame
}


# ---- Internal helpers -----------------------------------------------------

# Absolute survival estimands (RMST difference, risk difference) under a
# sampling design: refused, with the cause named and a route out.
#
# The estimand is not what breaks. The RMST and risk-difference columns
# were validated against exact adjustedCurves / riskRegression oracles,
# and the g-computation step is unchanged under a design. What breaks is
# the INFERENCE: `.coxph_baseline()` resamples SUBJECTS to get the
# uncertainty of the baseline hazard, and resampling rows ignores the
# strata and clusters the design declares -- the same mechanism
# compute_model_vcov() already refuses for these classes, measured there
# at a factor of ~0.4 (anti-conservative).
#
# The replacement is a component swap, not a new project: the design's
# own replication (as.svrepdesign() / withReplicates()) in place of the
# row bootstrap, with the validated g-computation kept as it is.
.svycoxph_refuse_estimands <- function(show_columns, model_id) {
  tokens <- c(
    "rmst",
    "rmst_se",
    "rmst_ci",
    "rmst_p",
    "risk_diff",
    "risk_diff_se",
    "risk_diff_ci",
    "risk_diff_p"
  )
  asked <- intersect(tokens, show_columns)
  if (length(asked) == 0L) {
    return(invisible(NULL))
  }
  spicy_abort(
    c(
      sprintf(
        "RMST / risk-difference columns are not available for a design-based Cox fit (%s).",
        model_id
      ),
      "x" = paste0(
        "Their uncertainty comes from resampling subjects to rebuild the ",
        "baseline hazard, and resampling rows ignores the strata and ",
        "clusters the survey design declares."
      ),
      "i" = paste0(
        "The estimands themselves are not in question: the RMST and risk ",
        "differences are validated against exact oracles for an ",
        "unweighted Cox fit. A design-correct version -- the design's own ",
        "replicate weights in place of the row bootstrap -- is planned."
      ),
      "i" = paste0(
        "For a marginal survival curve under the design, use ",
        "`survey::svykm()`; for a design-based test of a term, ",
        "`survey::regTermTest(fit, ~term)`."
      )
    ),
    class = "spicy_unsupported"
  )
}


# Build the coefs tibble for a svycoxph / svrepcoxph fit. No intercept
# (the baseline hazard absorbs it), Wald-t at the design's residual
# degrees of freedom.
.svycoxph_coefs <- function(fit, ci_level, df) {
  est <- stats::coef(fit)
  nm <- names(est)
  # stats::vcov(fit) IS the design-based ("robust se") matrix -- the
  # column summary() labels `robust se`, not `se(coef)`. Read here rather
  # than from summary(), which prints the design as a side effect and
  # whose p-value column is a z.
  se <- unname(sqrt(diag(as.matrix(stats::vcov(fit)))))
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

  ref_rows <- .survival_reference_rows(fit)
  if (nrow(ref_rows) > 0L) {
    coefs <- rbind(coefs, ref_rows)
  }
  coefs
}


# Build the info list for a svycoxph / svrepcoxph fit.
.svycoxph_info <- function(
  fit,
  vcov_kind,
  vcov_label,
  ci_level,
  ci_method,
  model_id,
  df
) {
  dv <- tryCatch(deparse1(stats::formula(fit)[[2L]]), error = function(e) {
    all.vars(stats::formula(fit))[1L] # nocov
  })

  if (is.null(ci_method)) {
    ci_method <- "wald"
  }

  # fit$n is the subject count; nobs() would be the number of EVENTS.
  n_obs <- .design_fit_n_obs(fit)
  n_events <- as.integer(fit$nevent %||% NA_integer_)

  # Every likelihood-derived statistic is absent, and the two survey does
  # compute are not usable: deviance() is a likelihood-ratio statistic
  # with the sign inverted on one engine and a bare 0 on the other, and
  # logLik() returns NA (df = 3) on the replicate engine while erroring
  # on the linearised one. They are posted as NA in the FRAME, not merely
  # left out of the default token set, so an explicit `show_fit_stats`
  # cannot republish them.
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
    weighted_nobs = .design_weighted_n(fit, n_obs),
    n_events = n_events
  )

  # AME is refused on the same ground as for a plain Cox fit: a
  # proportional-hazards model has no natural response scale to average a
  # marginal effect on. (marginaleffects also recurses without end on
  # this class, which is a separate matter -- the refusal stands even
  # once that is fixed.) The absolute-effect estimands that replace it
  # for coxph are themselves refused here; see
  # .svycoxph_refuse_estimands().
  supports <- list(
    ame = FALSE,
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
    title_prefix = "Survey-weighted Cox proportional hazards regression",
    exp_applied = FALSE,
    exp_header = NA_character_,
    n_events = n_events,
    # Read off the fit, never through summary(): survey's method prints
    # the design on every access.
    concordance = .svycoxph_concordance(fit),
    design_class = .svyglm_design_class(fit),
    # Footer disclosure: the sampling scheme, read off the ANALYTIC
    # design, and the degrees of freedom the table's own tests use.
    design_meta = .design_meta_or_null(.design_analytic(fit, n_obs)),
    design_degf_resid = df
  )

  list(
    class = "svycoxph",
    family = list(family = "cox", link = "log"),
    dv = dv,
    dv_label = dv,
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


# Harrell's C and its standard error, from the fit's own `concordance`
# component (named entries "concordance" and "std"). Both engines carry
# it; summary() would give the same two numbers and print the design on
# the way.
.svycoxph_concordance <- function(fit) {
  cc <- fit$concordance
  if (is.null(cc) || !is.numeric(cc) || is.null(names(cc))) {
    return(NULL) # nocov
  }
  # `[[` on a named ATOMIC vector whose name is absent throws
  # "subscript out of bounds"; it does not return NULL, so `%||%` has
  # nothing to substitute and cannot intercept it. The `is.null(names())`
  # test above only covers a vector with no names at all -- a vector
  # named but missing one of the two entries would have reached
  # as_regression_frame() as an exception. Each name is asked for by
  # membership instead.
  pick <- function(nm) {
    if (nm %in% names(cc)) unname(cc[[nm]]) else NA_real_
  }
  c_stat <- pick("concordance")
  if (!is.finite(c_stat)) {
    return(NULL)
  }
  se <- pick("std")
  list(c = c_stat, se = if (is.finite(se)) se else NA_real_)
}
