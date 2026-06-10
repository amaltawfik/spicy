# ---------------------------------------------------------------------------
# Phase 0b sub-step 2: as_regression_frame.lm() / .glm() methods.
#
# Strangler-fig wrappers around the existing extract_lm_phase1() pipeline.
# Production code STILL routes through extract_lm_phase1() directly; these
# methods exist so we can (a) prove the schema is sufficient on real fits
# via the validator, and (b) get oracle cross-validation tests in place
# before any downstream consumer migrates in sub-step 3.
#
# Design: dev/design_as_regression_frame.md (single source of truth).
# Q2 settled: this generic stays internal throughout the 0.x cycle. The
# methods carry @export only to register the S3method() dispatch entry;
# the method names are NOT user-visible.
# ---------------------------------------------------------------------------


#' `as_regression_frame()` method for `lm` fits.
#'
#' Thin wrapper around `extract_lm_phase1()` that reshapes the legacy
#' long-format extractor output into the standardised `{coefs, info}`
#' frame documented in `dev/design_as_regression_frame.md`.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.lm <- function(fit,
                                    vcov = "classical",
                                    vcov_label = NULL,
                                    cluster = NULL,
                                    boot_n = 1000L,
                                    ci_level = 0.95,
                                    ci_method = "wald",
                                    standardized = "none",
                                    exponentiate = FALSE,
                                    show_columns = c("b", "se", "ci", "p"),
                                    show_fit_stats = c("nobs", "r2", "adj_r2"),
                                    use_ame_satterthwaite = FALSE,
                                    cluster_name = NULL,
                                    model_id = "M1",
                                    ...) {
  # `glm` inherits from `lm`, so this method serves both. The reshape
  # picks up class-specific bits (family, supports) by branching on
  # inherits(fit, "glm") below.
  legacy <- extract_lm_phase1(
    fit                   = fit,
    model_id              = model_id,
    vcov_type             = vcov,
    cluster               = cluster,
    boot_n                = boot_n,
    ci_level              = ci_level,
    ci_method             = ci_method,
    standardized          = standardized,
    exponentiate          = exponentiate,
    show_columns          = show_columns,
    show_fit_stats        = show_fit_stats,
    use_ame_satterthwaite = use_ame_satterthwaite,
    cluster_name          = cluster_name
  )
  .legacy_to_frame(legacy, fit,
                   vcov_kind  = vcov,
                   vcov_label = vcov_label,
                   ci_level   = ci_level,
                   ci_method  = ci_method)
}

#' `as_regression_frame()` method for `glm` fits.
#'
#' `glm` inherits from `lm`, so dispatch would normally land on `.lm()`.
#' We register a dedicated method anyway: this makes the support matrix
#' explicit in `methods("as_regression_frame")`, and lets future
#' divergence (e.g. when glm-specific options accumulate) happen without
#' surprise dispatch rewiring.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.glm <- function(fit, ...) {
  as_regression_frame.lm(fit, ...)
}


# ---- Reshape: legacy extractor output -> standardised frame ---------------

# Takes the list returned by extract_lm_phase1() plus the original fit
# object and returns a list(coefs = <tibble>, info = <list>) carrying the
# two required attributes (spicy_frame_version, fit). The schema is
# documented in dev/design_as_regression_frame.md sections 3-4.
#
# The reshape preserves every legacy field; renamed columns map to the
# documented schema names, and unmapped legacy fields stay accessible via
# the existing accessor code (info$fit_stats is permissive on additional
# fields). Sub-step 4 will trim what is genuinely unused once downstream
# consumers all read from the frame.
.legacy_to_frame <- function(legacy, fit,
                              vcov_kind, vcov_label,
                              ci_level, ci_method) {
  coefs <- .reshape_coefs(legacy$coefs)
  info  <- .build_info(legacy, fit,
                       vcov_kind  = vcov_kind,
                       vcov_label = vcov_label,
                       ci_level   = ci_level,
                       ci_method  = ci_method)

  frame <- list(coefs = coefs, info = info)
  attr(frame, "spicy_frame_version") <- spicy_frame_version()
  attr(frame, "fit") <- fit
  frame
}


# Internal: reshape the legacy long-format coefs data.frame into the
# standardised coefs tibble.
#
# Legacy columns and their schema targets:
#   model_id        -> dropped (renderer concern, not part of frame)
#   outcome         -> dropped (lives in info$dv)
#   term            -> term
#   estimate_type   -> estimate_type (already "B" / "beta" / "ame")
#   estimate        -> estimate
#   se              -> std_error (renamed)
#   ci_low          -> ci_lower  (renamed)
#   ci_high         -> ci_upper  (renamed)
#   statistic       -> statistic
#   df              -> df
#   p_value         -> p_value
#   test_type       -> dropped (not in schema; legacy footer concern)
#   is_singular     -> dropped (legacy renderer marker)
#   is_intercept    -> dropped (derivable: term == "(Intercept)")
#   is_reference    -> is_ref   (renamed)
#   factor_term     -> parent_var (fallback to term when NA)
#   factor_level    -> label      (fallback to term when NA)
#   factor_level_pos -> factor_level_pos
.reshape_coefs <- function(legacy_coefs) {
  # Empty fit edge case (no coefficients): return a zero-row data.frame
  # with the required schema columns. Validator catches missing columns,
  # not empty data.
  if (nrow(legacy_coefs) == 0L) {
    return(.empty_coefs_frame())
  }

  parent_var <- ifelse(
    is.na(legacy_coefs$factor_term),
    legacy_coefs$term,
    legacy_coefs$factor_term
  )
  label <- ifelse(
    is.na(legacy_coefs$factor_level),
    legacy_coefs$term,
    legacy_coefs$factor_level
  )

  data.frame(
    term             = legacy_coefs$term,
    parent_var       = parent_var,
    label            = label,
    factor_level_pos = as.integer(legacy_coefs$factor_level_pos),
    is_ref           = as.logical(legacy_coefs$is_reference),
    estimate_type    = legacy_coefs$estimate_type,
    estimate         = as.numeric(legacy_coefs$estimate),
    std_error        = as.numeric(legacy_coefs$se),
    df               = as.numeric(legacy_coefs$df),
    statistic        = as.numeric(legacy_coefs$statistic),
    p_value          = as.numeric(legacy_coefs$p_value),
    ci_lower         = as.numeric(legacy_coefs$ci_low),
    ci_upper         = as.numeric(legacy_coefs$ci_high),
    # Optional per-row test_type ("t" for lm, "z" for glm, NA for ref /
    # singular / partial / AME rows). Read by broom::tidy.spicy_regression_table
    # and downstream consumers; preserved as a string column.
    test_type        = as.character(legacy_coefs$test_type),
    stringsAsFactors = FALSE
  )
}


# Internal: zero-row coefs frame with the schema columns. Used for fits
# with no coefficients (degenerate but valid input).
.empty_coefs_frame <- function() {
  data.frame(
    term             = character(0),
    parent_var       = character(0),
    label            = character(0),
    factor_level_pos = integer(0),
    is_ref           = logical(0),
    estimate_type    = character(0),
    estimate         = numeric(0),
    std_error        = numeric(0),
    df               = numeric(0),
    statistic        = numeric(0),
    p_value          = numeric(0),
    ci_lower         = numeric(0),
    ci_upper         = numeric(0),
    test_type        = character(0),
    stringsAsFactors = FALSE
  )
}


# Internal: build the info list from the legacy extractor output + fit.
#
# Captures every field the downstream renderers (title, footer, body) read
# from the legacy extract list. Schema-level fields go into `info` proper
# (class, family, dv, n_obs, weights_kind, vcov_*, ci_*, supports,
# fit_stats); class-specific oddities that the FOOTER reads case-by-case
# go into `info$extras` per Q5 (cluster_name, exp_applied, exp_header,
# use_ame_satterthwaite, singular_terms, weighted_n, title_prefix,
# family_info). This keeps the schema minimal while preserving every
# byte the footer renderer needs for byte-identical output after
# round-tripping through `.frame_to_legacy_extract()` in sub-step 3.
.build_info <- function(legacy, fit, vcov_kind, vcov_label,
                         ci_level, ci_method) {
  is_glm <- inherits(fit, "glm")
  family <- .family_info(fit)
  weights_kind <- .weights_kind_from_fit(fit)
  supports <- if (is_glm) .glm_supports() else .lm_supports()

  # Build fit_stats list from the legacy single-row data.frame. The legacy
  # frame ships about 18 fields (nobs, r2, adj_r2, aic, AICc, BIC, sigma,
  # rmse, omega2, f2, pseudo_r2_*, deviance, df_residual, weighted_nobs).
  # The validator requires only `nobs`; the rest are additive and survive
  # the schema unchanged. Renderer code (sub-steps 3-4) will consume them
  # via the same keys it uses today.
  fit_stats <- as.list(legacy$fit_stats)
  fit_stats$model_id <- NULL  # bookkeeping, not data
  fit_stats$outcome  <- NULL  # lives in info$dv

  # Provide the design-doc-documented schema field names AS aliases on
  # top of the legacy names. Sub-step 4 will pick one canonical naming
  # once downstream consumers migrate; for now, ship both so the renderer
  # can keep reading legacy keys while new tests can target the schema
  # keys.
  if (!is.null(fit_stats$r2))        fit_stats$r_squared     <- fit_stats$r2
  if (!is.null(fit_stats$adj_r2))    fit_stats$adj_r_squared <- fit_stats$adj_r2
  if (!is.null(fit_stats$AIC))       fit_stats$aic           <- fit_stats$AIC
  if (!is.null(fit_stats$BIC))       fit_stats$bic           <- fit_stats$BIC
  # log_lik is not in the legacy fit_stats; derive directly from the fit.
  fit_stats$log_lik <- as.numeric(stats::logLik(fit))

  # pseudo_r2 schema field collapses the three legacy pseudo-R^2 values
  # into a single list keyed by method. NULL for lm.
  fit_stats$pseudo_r2 <- if (is_glm) {
    list(
      mcfadden   = fit_stats$pseudo_r2_mcfadden,
      nagelkerke = fit_stats$pseudo_r2_nagelkerke,
      tjur       = fit_stats$pseudo_r2_tjur
    )
  } else {
    NULL
  }

  # Extras vocabulary (sub-step 3): every field the legacy extract carried
  # that the footer / title renderer reads case-by-case. Documented here
  # so future per-class methods (lmer, glmer, svyglm, ...) know which
  # extras keys to populate for parity with the existing renderers.
  extras <- list(
    cluster_name          = legacy$cluster_name,
    use_ame_satterthwaite = isTRUE(legacy$use_ame_satterthwaite),
    has_singular          = isTRUE(legacy$has_singular),
    singular_terms        = legacy$singular_terms %||% character(0),
    has_weights           = isTRUE(legacy$has_weights),
    weighted_n            = legacy$weighted_n %||% NA_real_,
    title_prefix          = legacy$title_prefix %||%
      (if (is_glm) "Generalized linear regression" else "Linear regression"),
    family_info           = legacy$family_info,
    exp_applied           = isTRUE(legacy$exp_applied),
    exp_header            = legacy$exp_header %||% NA_character_
  )

  list(
    class          = class(fit)[1],
    family         = family,
    dv             = legacy$outcome,
    dv_label       = legacy$outcome_label,
    n_obs          = as.integer(legacy$nobs),
    n_groups       = NULL,
    weights_kind   = weights_kind,
    random_effects = NULL,
    fit_stats      = fit_stats,
    vcov_kind      = vcov_kind,
    vcov_label     = vcov_label %||% .vcov_label_from_kind(vcov_kind, is_glm),
    ci_level       = as.numeric(ci_level),
    ci_method      = ci_method,
    supports       = supports,
    extras         = extras
  )
}


# ---- Adapter: frame -> legacy-extract shape -------------------------------

# Round-trips a frame back into a list shaped like `extract_lm_phase1()`'s
# return value. Used by the title / footer renderers (and the body builder
# in sub-step 4) so they can switch their input source from the legacy
# pipeline to the frame pipeline without changing a single line of their
# own code.
#
# The adapter is the dual of `.legacy_to_frame()`. Together they prove
# that the frame is information-equivalent to the legacy extract for the
# downstream consumers we ship today.
#
# `coefs` round-trip rules (inverse of `.reshape_coefs()`):
#   se               <- std_error
#   ci_low           <- ci_lower
#   ci_high          <- ci_upper
#   is_reference     <- is_ref
#   factor_term      <- parent_var unless parent_var == term (NA fallback)
#   factor_level     <- label      unless label      == term (NA fallback)
#   is_intercept     <- term == "(Intercept)"   (derived; not stored)
#   is_singular      <- term %in% info$extras$singular_terms (derived)
#   test_type        <- NA_character_ (dropped; legacy footer doesn't read)
#   model_id         <- "M1" or supplied (renderer concern only)
#   outcome          <- info$dv
.frame_to_legacy_extract <- function(frame, model_id = "M1") {
  validate_regression_frame(frame)
  info  <- frame$info
  coefs <- frame$coefs
  fit   <- attr(frame, "fit")
  is_glm <- inherits(fit, "glm")

  singular_terms <- info$extras$singular_terms %||% character(0)
  is_singular_vec <- coefs$term %in% singular_terms

  factor_term_back <- ifelse(
    coefs$parent_var == coefs$term,
    NA_character_,
    coefs$parent_var
  )
  factor_level_back <- ifelse(
    coefs$label == coefs$term,
    NA_character_,
    coefs$label
  )

  legacy_coefs <- data.frame(
    model_id         = rep(model_id, nrow(coefs)),
    outcome          = rep(info$dv, nrow(coefs)),
    term             = coefs$term,
    estimate_type    = coefs$estimate_type,
    estimate         = coefs$estimate,
    se               = coefs$std_error,
    ci_low           = coefs$ci_lower,
    ci_high          = coefs$ci_upper,
    statistic        = coefs$statistic,
    df               = coefs$df,
    p_value          = coefs$p_value,
    test_type        = if (!is.null(coefs$test_type)) {
      as.character(coefs$test_type)
    } else {
      rep(NA_character_, nrow(coefs))
    },
    is_singular      = is_singular_vec,
    is_intercept     = coefs$term == "(Intercept)",
    is_reference     = coefs$is_ref,
    factor_term      = factor_term_back,
    factor_level     = factor_level_back,
    factor_level_pos = coefs$factor_level_pos,
    stringsAsFactors = FALSE
  )

  list(
    model_id              = model_id,
    outcome               = info$dv,
    outcome_label         = info$dv_label %||% info$dv,
    coefs                 = legacy_coefs,
    fit_stats             = as.data.frame(
      .compact_fit_stats_for_legacy(info$fit_stats, model_id, info$dv),
      stringsAsFactors = FALSE
    ),
    vcov_type             = info$vcov_kind,
    cluster_name          = info$extras$cluster_name,
    use_ame_satterthwaite = isTRUE(info$extras$use_ame_satterthwaite),
    has_singular          = isTRUE(info$extras$has_singular),
    singular_terms        = singular_terms,
    has_weights           = isTRUE(info$extras$has_weights),
    weighted_n            = info$extras$weighted_n %||% NA_real_,
    nobs                  = as.integer(info$n_obs),
    is_glm                = is_glm,
    family_info           = info$extras$family_info,
    title_prefix          = info$extras$title_prefix %||%
      (if (is_glm) "Generalized linear regression" else "Linear regression"),
    exp_applied           = isTRUE(info$extras$exp_applied),
    exp_header            = info$extras$exp_header %||% NA_character_
  )
}


# Helper: legacy `fit_stats` is a single-row data.frame. The frame stores
# it as a list. Round-trip: rebuild the data.frame structure by selecting
# only the legacy-named scalar fields (drop the schema aliases and the
# pseudo_r2 nested list we added in `.build_info()`).
.compact_fit_stats_for_legacy <- function(fs, model_id, outcome) {
  # Order matches the original `extract_fit_stats()` return.
  out <- list(
    model_id             = model_id,
    outcome              = outcome,
    nobs                 = fs$nobs,
    weighted_nobs        = fs$weighted_nobs        %||% NA_real_,
    r2                   = fs$r2                   %||% NA_real_,
    adj_r2               = fs$adj_r2               %||% NA_real_,
    omega2               = fs$omega2               %||% NA_real_,
    pseudo_r2_mcfadden   = fs$pseudo_r2_mcfadden   %||% NA_real_,
    pseudo_r2_nagelkerke = fs$pseudo_r2_nagelkerke %||% NA_real_,
    pseudo_r2_tjur       = fs$pseudo_r2_tjur       %||% NA_real_,
    sigma                = fs$sigma                %||% NA_real_,
    rmse                 = fs$rmse                 %||% NA_real_,
    f2                   = fs$f2                   %||% NA_real_,
    AIC                  = fs$AIC                  %||% NA_real_,
    AICc                 = fs$AICc                 %||% NA_real_,
    BIC                  = fs$BIC                  %||% NA_real_,
    deviance             = fs$deviance             %||% NA_real_,
    df_residual          = fs$df_residual          %||% NA_real_
  )
  out
}


# ---- Per-class helpers ----------------------------------------------------

# Returns list(family = chr, link = chr) for a fit. For lm we hard-code
# gaussian / identity; for glm we read from fit$family.
.family_info <- function(fit) {
  if (inherits(fit, "glm")) {
    fam <- stats::family(fit)
    list(family = fam$family, link = fam$link)
  } else {
    list(family = "gaussian", link = "identity")
  }
}


# Returns a `weights_kind` value from the design-doc-documented vocabulary
# c("none", "frequency", "sampling", "case") based on whether the fit has
# non-trivial weights.
#
# lm/glm cannot distinguish frequency vs. sampling vs. case weights without
# user metadata, so we default to "case" -- the most common research
# convention (analytic weights in Stata). This is a conservative choice:
# the renderer's variance estimator description will not mis-claim a
# probability-sample interpretation.
.weights_kind_from_fit <- function(fit) {
  w <- tryCatch(stats::weights(fit), error = function(e) NULL)
  if (is.null(w) || length(w) == 0L || length(unique(w)) == 1L) {
    "none"
  } else {
    "case"
  }
}


# Maps the internal vcov_kind string to a human-readable footer label.
# Falls back to the kind name when unknown.
.vcov_label_from_kind <- function(vcov_kind, is_glm = FALSE) {
  labels <- c(
    classical = if (is_glm) "Model-based (asymptotic)" else "OLS",
    model     = if (is_glm) "Model-based (asymptotic)" else "OLS",
    HC0       = "HC0 heteroskedasticity-consistent",
    HC1       = "HC1 heteroskedasticity-consistent",
    HC2       = "HC2 heteroskedasticity-consistent",
    HC3       = "HC3 heteroskedasticity-consistent",
    HC4       = "HC4 heteroskedasticity-consistent",
    HC5       = "HC5 heteroskedasticity-consistent",
    CR0       = "CR0 cluster-robust",
    CR1       = "CR1 cluster-robust",
    CR2       = "CR2 cluster-robust",
    CR3       = "CR3 cluster-robust",
    bootstrap = "Bootstrap",
    jackknife = "Jackknife"
  )
  out <- labels[vcov_kind]
  if (is.na(out)) vcov_kind else unname(out)
}


# Capability flags for an `lm` fit. See dev/design_as_regression_frame.md
# section 4 for the schema and section 6 for the per-class rationale.
.lm_supports <- function() {
  list(
    ame                 = TRUE,    # via marginaleffects::avg_slopes()
    partial_effect_size = TRUE,    # partial f^2 / eta^2 / omega^2 with NCF CIs
    classical_r2        = TRUE,    # R^2 / adj-R^2 well defined for lm
    nested_lrt          = TRUE,    # nested model comparisons via anova()
    exponentiate        = FALSE,   # identity link -- nothing to exponentiate
    standardise_refit   = TRUE     # z-scored refit is class-agnostic for lm
  )
}


# Capability flags for a `glm` fit. partial_effect_size = TRUE for the
# partial chi-square family; classical_r2 is FALSE (use pseudo_r2 instead);
# exponentiate is TRUE because non-identity links produce OR / RR / IRR.
.glm_supports <- function() {
  list(
    ame                 = TRUE,
    partial_effect_size = TRUE,    # partial chi^2 (regression_partial.R)
    classical_r2        = FALSE,
    nested_lrt          = TRUE,
    exponentiate        = TRUE,
    standardise_refit   = TRUE
  )
}
