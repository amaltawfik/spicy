# ---------------------------------------------------------------------------
# Phase 0b scaffolding: as_regression_frame() generic + validator.
#
# Internal S3 generic that produces a standardised data frame from a fitted
# regression model object, plus a per-model metadata list. Sits upstream of
# build_structured_body() in the rendering pipeline.
#
# This file ships the generic, the default fallback, the schema validator,
# and the version constant. Per-class methods (lm, glm, ...) live in
# regression_frame_lm.R and similar siblings.
#
# Design: dev/design_as_regression_frame.md (single source of truth).
# Phase 0b status: SCAFFOLDING ONLY. No production code calls the generic
# yet -- existing extract_lm_phase1() pipeline remains the live path.
# Phase 0b sub-step 2 will add the lm / glm methods. Phase 0b sub-steps
# 3-5 will migrate downstream consumers one at a time.
#
# Q2 settlement (dev/design_as_regression_frame.md): this generic stays
# INTERNAL throughout the 0.x cycle. No @export. No public reference page.
# ---------------------------------------------------------------------------


# ---- Version constant -----------------------------------------------------

# Schema version attached as attr(frame, "spicy_frame_version"). Bumped only
# on documented-breaking schema changes (rename or remove a field; change
# semantics of a field). Additive changes do NOT bump. See design doc §5.
spicy_frame_version <- function() {
  "1"
}


# ---- Schema constructors --------------------------------------------------

# Default capability flags: every capability is FALSE unless a method opts in.
# new_regression_frame() merges a method's `info$supports` onto this, so a
# method need only set the flags it supports and any newly-added flag (e.g.
# robust_vcov) defaults safely across all ~30 classes. This list is ALSO the
# single source of truth for the supports field set the validator requires.
default_supports <- function() {
  list(
    ame                 = FALSE,
    partial_effect_size = FALSE,
    classical_r2        = FALSE,
    nested_lrt          = FALSE,
    exponentiate        = FALSE,
    standardise_refit   = FALSE,
    robust_vcov         = FALSE
  )
}

# Default `info$extras` skeleton: the keys common to (nearly) every frame.
# Method-specific extras (smooth_terms, random-effect engine, zero-inflation
# component, ...) are merged on top by new_regression_frame().
default_extras <- function() {
  list(
    cluster_name          = NULL,
    use_ame_satterthwaite = FALSE,
    has_singular          = FALSE,
    singular_terms        = character(0),
    has_weights           = FALSE,
    weighted_n            = NA_real_,
    title_prefix          = NULL,
    exp_applied           = FALSE,
    exp_header            = NA_character_
  )
}

# Canonical empty `info$random_effects` for non-mixed (or RE-less) fits. One
# shape everywhere -- key set matches the populated form built by the
# mixed-effects methods (variance_components / icc / method / null_lrt) -- so
# consumers never NULL-deref `$icc` (NA_real_ vs NULL) or `$variance_components`.
empty_random_effects <- function() {
  list(
    variance_components = data.frame(),
    icc                 = NA_real_,
    method              = NA_character_,
    null_lrt            = NULL
  )
}

# Low-level constructor for the `spicy_regression_frame` S3 object: the single
# place that assembles a {coefs, info} frame, normalises info$supports /
# info$extras against their defaults, sets the object class, and attaches the
# two schema attributes (spicy_frame_version, fit). Every as_regression_frame.*
# method ends with a call to this, so the object's structure + schema contract
# live in exactly one location. Pairs with validate_regression_frame() per the
# standard new_<class>() / validate_<class>() constructor convention.
new_regression_frame <- function(coefs, info, fit) {
  info$supports <- utils::modifyList(default_supports(), info$supports %||% list())
  info$extras   <- utils::modifyList(default_extras(),   info$extras   %||% list())
  structure(
    list(coefs = coefs, info = info),
    class               = "spicy_regression_frame",
    spicy_frame_version = spicy_frame_version(),
    fit                 = fit
  )
}

#' @export
print.spicy_regression_frame <- function(x, ...) {
  info <- x$info
  fam  <- info$family
  cat(sprintf(
    "<spicy_regression_frame> %s  (n = %s)\n",
    info$class %||% "?", format(info$n_obs %||% NA)
  ))
  cat(sprintf(
    "  coefs: %d rows x %d cols | family: %s / %s | ci: %s\n",
    nrow(x$coefs), ncol(x$coefs),
    fam$family %||% "?", fam$link %||% "?", info$ci_method %||% "?"
  ))
  on_flags <- names(Filter(isTRUE, info$supports))
  cat(sprintf("  supports: %s\n",
              if (length(on_flags)) paste(on_flags, collapse = ", ") else "(none)"))
  invisible(x)
}

# Type predicate for the frame object.
is_regression_frame <- function(x) inherits(x, "spicy_regression_frame")


# ---- The generic ----------------------------------------------------------

# as_regression_frame(fit, ...) -> list(coefs = tibble, info = list)
#
# Returns the spicy-standardised frame for one fit. Per-class methods do the
# extraction; the rest of the pipeline (alignment, AME orchestration, body
# build, render) is class-agnostic and reads from the returned object only.
#
# Per design doc §2, the return value carries two attributes:
#   * spicy_frame_version: schema version string ("1" today).
#   * fit                : reference to the original fit object; read by
#                          AME orchestration, refit standardisation, and
#                          nested LRT downstream. Stripped on
#                          as.data.frame() / clipboard / Excel / Word
#                          serialisation paths (§12.1).
#
# The generic itself is NOT exported per Q2 settlement: it stays internal
# throughout 0.x. Tests reach it via `spicy:::as_regression_frame()`. The
# per-class methods below carry `@export` because roxygen treats that as
# "register the S3 dispatch via S3method() in NAMESPACE" -- it does NOT
# add the method name to the user-facing namespace.
#'
#' @keywords internal
#' @noRd
as_regression_frame <- function(fit, ...) {
  # R's S3 dispatch on NULL is special: UseMethod() on NULL does not fall
  # through to as_regression_frame.default() (it errors with "no applicable
  # method"). Handle NULL explicitly here so the user-facing error is the
  # spicy_unsupported_class one, not R's cryptic dispatch error.
  if (is.null(fit)) {
    return(as_regression_frame.default(fit, ...))
  }
  UseMethod("as_regression_frame")
}

#' Default `as_regression_frame()` method (unsupported class fallback).
#'
#' Aborts with a structured message naming the offending class and
#' pointing the user at the issue tracker. Per Q2 there is no public
#' extension contract in the 0.x cycle.
#'
#' @keywords internal
#' @noRd
#' @export
as_regression_frame.default <- function(fit, ...) {
  cls <- if (is.null(fit)) "NULL" else paste(class(fit), collapse = "/")
  spicy_abort(
    c(
      sprintf(
        "table_regression() does not support model class %s.",
        sQuote(cls)
      ),
      "i" = paste0(
        "Supported classes today: lm, glm, lmerMod / lmerModLmerTest, glmerMod, ",
        "glmmTMB, lme (nlme), gls (nlme), coxph (survival), survreg (survival), ",
        "polr / glm.nb / rlm (MASS), clm (ordinal), multinom (nnet), ",
        "lm_robust / iv_robust (estimatr), fixest (fixest), ",
        "hurdle / zeroinfl (pscl), rq (quantreg), ivreg / tobit (AER), ",
        "gam / bam (mgcv), ols / lrm / cph / Glm (rms), ",
        "mlogit (mlogit), betareg (betareg), ",
        "flexsurvreg (flexsurv), selection (sampleSelection), nls (stats), ",
        "svyglm, stanreg, brmsfit. ",
        "Robust mixed (robustlmm) and Bayesian-hierarchical (MCMCglmm) ",
        "are on the 2026-2027 roadmap."
      ),
      "i" = paste0(
        "If you would like to see support for ", sQuote(cls), ", ",
        "please open an issue: ",
        "https://github.com/amaltawfik/spicy/issues"
      )
    ),
    class = "spicy_unsupported_class"
  )
}


# ---- Schema validator -----------------------------------------------------

# Checks that `frame` matches the schema documented in
# dev/design_as_regression_frame.md §§3-4. Returns invisibly TRUE on
# success; aborts with a structured message naming the violated field on
# failure.
#
# Required `coefs` columns (with types):
#   term            chr
#   parent_var      chr
#   label           chr
#   factor_level_pos int
#   is_ref          lgl
#   estimate_type   chr  (values restricted to "B", "beta", "ame")
#   estimate        dbl
#   std_error       dbl
#   ci_lower        dbl
#   ci_upper        dbl
#
# Optional `coefs` columns (type-checked if present):
#   outcome_level   chr
#   df              dbl
#   statistic       dbl
#   p_value         dbl
#   pd              dbl
#   row_extras      list
#
# Required `info` fields:
#   class           chr
#   family          list (with $family chr, $link chr)
#   dv              chr
#   n_obs           int / dbl
#   weights_kind    chr  (one of "none", "frequency", "sampling", "case")
#   fit_stats       list (must contain nobs)
#   vcov_kind       chr
#   vcov_label      chr
#   ci_level        dbl (in (0, 1))
#   ci_method       chr
#   supports        list (with all six logical fields)
#   extras          list
#
# Optional `info` fields (type-checked if present):
#   dv_label        chr
#   n_groups        named int vector OR NULL
#   random_effects  list OR NULL
#
# Required attributes:
#   spicy_frame_version chr (must equal the current version)
#   fit                 (any) -- presence checked, not type
#
# @keywords internal
# @noRd
validate_regression_frame <- function(frame) {
  # ---- Type ---------------------------------------------------------------
  if (!inherits(frame, "spicy_regression_frame")) {
    spicy_abort(
      "Invalid regression frame: not a <spicy_regression_frame> object.",
      class = "spicy_invalid_frame"
    )
  }
  # ---- Top-level structure ------------------------------------------------
  if (!is.list(frame) || is.null(names(frame))) {
    spicy_abort(
      "Invalid regression frame: must be a named list.",
      class = "spicy_invalid_frame"
    )
  }
  required_top <- c("coefs", "info")
  missing_top <- setdiff(required_top, names(frame))
  if (length(missing_top) > 0L) {
    spicy_abort(
      c(
        "Invalid regression frame: missing top-level slot(s).",
        "x" = paste("missing:", paste(missing_top, collapse = ", "))
      ),
      class = "spicy_invalid_frame"
    )
  }

  # ---- Attributes ---------------------------------------------------------
  ver <- attr(frame, "spicy_frame_version")
  if (is.null(ver)) {
    spicy_abort(
      c(
        "Invalid regression frame: missing attribute 'spicy_frame_version'.",
        "i" = paste0(
          "Set attr(frame, 'spicy_frame_version') <- spicy_frame_version() ",
          "at the end of the per-class method."
        )
      ),
      class = "spicy_invalid_frame"
    )
  }
  if (!identical(ver, spicy_frame_version())) {
    spicy_abort(
      c(
        sprintf(
          "Regression frame schema version mismatch: got '%s', expected '%s'.",
          ver, spicy_frame_version()
        ),
        "i" = paste0(
          "If you are running an external as_regression_frame() method ",
          "written for an older spicy, update the method to match the ",
          "current schema (see dev/design_as_regression_frame.md)."
        )
      ),
      class = "spicy_invalid_frame"
    )
  }
  if (!"fit" %in% names(attributes(frame))) {
    spicy_abort(
      c(
        "Invalid regression frame: missing attribute 'fit'.",
        "i" = paste0(
          "Set attr(frame, 'fit') <- fit at the end of the per-class ",
          "method. Downstream AME and refit-standardisation paths read ",
          "from this attribute."
        )
      ),
      class = "spicy_invalid_frame"
    )
  }

  # ---- coefs tibble -------------------------------------------------------
  .validate_coefs(frame$coefs)

  # ---- info list ----------------------------------------------------------
  .validate_info(frame$info)

  invisible(TRUE)
}


# Internal: coefs-specific schema checks. Split out for readability.
.validate_coefs <- function(coefs) {
  if (!is.data.frame(coefs)) {
    spicy_abort(
      "Invalid regression frame: `coefs` must be a data.frame / tibble.",
      class = "spicy_invalid_frame"
    )
  }

  required_cols <- c(
    "term", "parent_var", "label", "factor_level_pos", "is_ref",
    "estimate_type", "estimate", "std_error", "ci_lower", "ci_upper"
  )
  missing_cols <- setdiff(required_cols, names(coefs))
  if (length(missing_cols) > 0L) {
    spicy_abort(
      c(
        "Invalid regression frame: `coefs` missing required column(s).",
        "x" = paste("missing:", paste(missing_cols, collapse = ", "))
      ),
      class = "spicy_invalid_frame"
    )
  }

  # Type checks for required columns. Per-column to give actionable errors.
  type_specs <- list(
    term             = "character",
    parent_var       = "character",
    label            = "character",
    factor_level_pos = "integer",
    is_ref           = "logical",
    estimate_type    = "character",
    estimate         = "double",
    std_error        = "double",
    ci_lower         = "double",
    ci_upper         = "double"
  )
  for (col in names(type_specs)) {
    expected <- type_specs[[col]]
    actual <- typeof(coefs[[col]])
    if (!identical(actual, expected)) {
      spicy_abort(
        c(
          sprintf(
            "Invalid regression frame: `coefs$%s` has wrong type.",
            col
          ),
          "x" = sprintf("expected %s, got %s", expected, actual)
        ),
        class = "spicy_invalid_frame"
      )
    }
  }

  # Optional columns: type-check only if present. Skipping checks on a
  # missing optional column is the contract -- per-class methods may
  # legitimately omit them.
  optional_specs <- list(
    outcome_level = "character",
    df            = "double",
    statistic     = "double",
    p_value       = "double",
    pd            = "double",
    test_type     = "character",
    row_extras    = "list"
  )
  for (col in names(optional_specs)) {
    if (col %in% names(coefs)) {
      expected <- optional_specs[[col]]
      actual <- typeof(coefs[[col]])
      if (!identical(actual, expected)) {
        spicy_abort(
          c(
            sprintf(
              "Invalid regression frame: optional `coefs$%s` has wrong type.",
              col
            ),
            "x" = sprintf("expected %s, got %s", expected, actual)
          ),
          class = "spicy_invalid_frame"
        )
      }
    }
  }

  # Restricted values for estimate_type. Canonical vocabulary (all
  # lowercase): c("B", "beta", "ame") for coefficient / standardised /
  # average-marginal-effect rows, plus the partial-effect-size tokens
  # ("partial_f2", "partial_eta2", "partial_omega2", "partial_chi2")
  # emitted by extract_partial_effect_rows(). The legacy uppercase
  # "AME" emitted by extract_lm_phase1() has been normalised to "ame".
  allowed_types <- c(
    "B", "beta", "ame", "vc",
    "partial_f2", "partial_eta2", "partial_omega2", "partial_chi2"
  )
  bad_types <- setdiff(unique(coefs$estimate_type), allowed_types)
  if (length(bad_types) > 0L) {
    spicy_abort(
      c(
        "Invalid regression frame: `coefs$estimate_type` contains unknown token(s).",
        "x" = paste("unknown:", paste(bad_types, collapse = ", ")),
        "i" = paste(
          "Allowed values:",
          paste(allowed_types, collapse = ", ")
        )
      ),
      class = "spicy_invalid_frame"
    )
  }

  # is_ref rows must have NA estimate (em-dash placeholder). Catching this
  # here prevents downstream rendering anomalies where a non-NA estimate
  # in a reference row would be silently dropped.
  if (any(coefs$is_ref & !is.na(coefs$estimate))) {
    spicy_abort(
      c(
        "Invalid regression frame: `coefs` has reference row(s) with non-NA estimate.",
        "i" = "Reference rows must have estimate = NA_real_ (em-dash placeholder)."
      ),
      class = "spicy_invalid_frame"
    )
  }

  invisible(TRUE)
}


# Internal: info-specific schema checks. Split out for readability.
.validate_info <- function(info) {
  if (!is.list(info) || is.null(names(info))) {
    spicy_abort(
      "Invalid regression frame: `info` must be a named list.",
      class = "spicy_invalid_frame"
    )
  }

  required_info <- c(
    "class", "family", "dv", "n_obs", "weights_kind",
    "fit_stats", "vcov_kind", "vcov_label",
    "ci_level", "ci_method", "supports", "extras"
  )
  missing_info <- setdiff(required_info, names(info))
  if (length(missing_info) > 0L) {
    spicy_abort(
      c(
        "Invalid regression frame: `info` missing required field(s).",
        "x" = paste("missing:", paste(missing_info, collapse = ", "))
      ),
      class = "spicy_invalid_frame"
    )
  }

  # Scalar string fields
  for (field in c("class", "dv", "vcov_kind", "vcov_label",
                  "ci_method", "weights_kind")) {
    val <- info[[field]]
    if (!is.character(val) || length(val) != 1L || is.na(val)) {
      spicy_abort(
        c(
          sprintf(
            "Invalid regression frame: `info$%s` must be a length-1 non-NA character.",
            field
          )
        ),
        class = "spicy_invalid_frame"
      )
    }
  }

  # weights_kind vocabulary
  allowed_weights <- c("none", "frequency", "sampling", "case")
  if (!info$weights_kind %in% allowed_weights) {
    spicy_abort(
      c(
        sprintf(
          "Invalid regression frame: `info$weights_kind` = %s is not a recognised value.",
          sQuote(info$weights_kind)
        ),
        "i" = paste(
          "Allowed:",
          paste(allowed_weights, collapse = ", ")
        )
      ),
      class = "spicy_invalid_frame"
    )
  }

  # n_obs: positive integer or numeric
  if (!is.numeric(info$n_obs) || length(info$n_obs) != 1L ||
        is.na(info$n_obs) || info$n_obs <= 0) {
    spicy_abort(
      "Invalid regression frame: `info$n_obs` must be a positive scalar.",
      class = "spicy_invalid_frame"
    )
  }

  # ci_level in (0, 1)
  cl <- info$ci_level
  if (!is.numeric(cl) || length(cl) != 1L || is.na(cl) ||
        cl <= 0 || cl >= 1) {
    spicy_abort(
      "Invalid regression frame: `info$ci_level` must be a scalar in (0, 1).",
      class = "spicy_invalid_frame"
    )
  }

  # family: list with $family and $link
  fam <- info$family
  if (!is.list(fam) || is.null(fam$family) || is.null(fam$link)) {
    spicy_abort(
      c(
        "Invalid regression frame: `info$family` must be a list with $family and $link.",
        "i" = paste0(
          "Example: list(family = 'gaussian', link = 'identity'). ",
          "For lm: family = 'gaussian', link = 'identity'."
        )
      ),
      class = "spicy_invalid_frame"
    )
  }

  # fit_stats: list, must contain nobs
  fs <- info$fit_stats
  if (!is.list(fs)) {
    spicy_abort(
      "Invalid regression frame: `info$fit_stats` must be a list.",
      class = "spicy_invalid_frame"
    )
  }
  if (!"nobs" %in% names(fs)) {
    spicy_abort(
      "Invalid regression frame: `info$fit_stats$nobs` is required.",
      class = "spicy_invalid_frame"
    )
  }

  # supports: list with six required logical fields
  sp <- info$supports
  if (!is.list(sp)) {
    spicy_abort(
      "Invalid regression frame: `info$supports` must be a list.",
      class = "spicy_invalid_frame"
    )
  }
  # Single source of truth: the supports field set IS whatever
  # default_supports() declares (so adding a flag there auto-updates the
  # validator + every method via new_regression_frame()'s normalisation).
  required_supports <- names(default_supports())
  missing_supports <- setdiff(required_supports, names(sp))
  if (length(missing_supports) > 0L) {
    spicy_abort(
      c(
        "Invalid regression frame: `info$supports` missing required field(s).",
        "x" = paste("missing:", paste(missing_supports, collapse = ", "))
      ),
      class = "spicy_invalid_frame"
    )
  }
  for (field in required_supports) {
    if (!is.logical(sp[[field]]) || length(sp[[field]]) != 1L ||
          is.na(sp[[field]])) {
      spicy_abort(
        sprintf(
          "Invalid regression frame: `info$supports$%s` must be a length-1 non-NA logical.",
          field
        ),
        class = "spicy_invalid_frame"
      )
    }
  }

  # extras: list (may be empty); ensures forward-compatibility for
  # class-specific oddities documented in design doc §9 Q5.
  if (!is.list(info$extras)) {
    spicy_abort(
      "Invalid regression frame: `info$extras` must be a list (possibly empty).",
      class = "spicy_invalid_frame"
    )
  }

  # Optional info fields: type-check if present.
  if ("dv_label" %in% names(info) && !is.null(info$dv_label)) {
    if (!is.character(info$dv_label) || length(info$dv_label) != 1L) {
      spicy_abort(
        "Invalid regression frame: optional `info$dv_label` must be length-1 character or NULL.",
        class = "spicy_invalid_frame"
      )
    }
  }
  if ("n_groups" %in% names(info) && !is.null(info$n_groups)) {
    if (!is.numeric(info$n_groups) || is.null(names(info$n_groups))) {
      spicy_abort(
        "Invalid regression frame: optional `info$n_groups` must be a named numeric vector or NULL.",
        class = "spicy_invalid_frame"
      )
    }
  }
  if ("random_effects" %in% names(info) && !is.null(info$random_effects)) {
    if (!is.list(info$random_effects)) {
      spicy_abort(
        "Invalid regression frame: optional `info$random_effects` must be a list or NULL.",
        class = "spicy_invalid_frame"
      )
    }
  }

  invisible(TRUE)
}


# Phase 7c19: LR test vs no-random-effects model.
#
# Stata `mixed` / `melogit` / `mepoisson` print a single line at the
# bottom of the output:
#
#   LR test vs. linear regression: chibar2(01) = 121.07
#                                Prob >= chibar2 = 0.0000
#
# It answers the publication-substantive question "are the random
# effects needed at all?" The statistic is
#
#   LRT = 2 * ( logLik(fit_full) - logLik(fit_null) )
#
# where fit_null is an `lm` / `glm` / `gls` fit on the same data
# without any random effects. The p-value uses the chi-bar-squared
# correction (Self & Liang 1987; Stata's mixed convention):
#
#   p_chibar2 = 0.5 * P( chi^2_q > LRT )
#
# where q = number of free parameters in the random structure
# (variances + covariances per grouping factor). The factor 0.5
# accounts for the point mass at the boundary 0; the simpler
# convention covers the common case "single test of all random
# effects together" without invoking the multi-component Stram-Lee
# (1994) mixture.
#
# Returns a list(chi2, df, p_chibar2, family_label) or NULL when
# computation isn't possible (refit error, lme4 unavailable, etc.).
#
# lme4 / glmmTMB / nlme::lme are handled via engine dispatch.
#
# LIKELIHOOD CONVENTION (2026-07-09, dev/re_lrt_ml_reml_finding.md):
# the test follows the FIT'S OWN estimator, matching
# lmerTest::ranova(), Stata mixed, and RLRsim. A Gaussian REML fit is
# compared on the REML likelihood to the fixed-effects-only model's
# REML likelihood (nlme::gls(method = "REML") -- nlme is a
# recommended package); ML fits (and glmer, always ML) compare on ML
# vs lm/glm as before. REML-LRT between models with identical fixed
# effects and nested random structures is valid (Morrell 1998).
# Previously REML fits were auto-refit with ML, which made the
# "(REML)" footer label describe the fit but not the statistic, and
# broke reproduction against ranova.
#
# Known limitation (recorded): prior weights on the mixed fit are NOT
# propagated to the null lm/glm/gls refit -- the whole-block LRT is
# only exact for unweighted fits (pre-existing on the ML path too).
# REML log-likelihood of the fixed-effects-only Gaussian null model,
# via nlme::gls(method = "REML") on the same data. Returns NA_real_
# when gls is unavailable or fails -- callers then return NULL (no
# LRT line) rather than printing a number under a wrong label.
.null_reml_loglik_lm <- function(null_f, data) {
  if (!requireNamespace("nlme", quietly = TRUE)) return(NA_real_)      # nocov
  fit0 <- tryCatch(
    nlme::gls(null_f, data = data, method = "REML"),
    error = function(e) NULL
  )
  if (is.null(fit0)) return(NA_real_)
  as.numeric(stats::logLik(fit0))
}


.compute_null_model_lrt <- function(fit) {
  res <- tryCatch({
    if (inherits(fit, "glmmTMB"))            .null_lrt_glmmTMB(fit)
    else if (inherits(fit, "merMod"))         .null_lrt_merMod(fit)
    else if (inherits(fit, "lme"))            .null_lrt_lme(fit)
    else                                       NULL
  }, error = function(e) NULL)
  if (is.null(res)) return(NULL)
  # nocov start -- the per-engine helpers already guard df <= 0 / non-finite
  # chi^2 and return NULL upstream, so a non-NULL res is always well-formed.
  if (!is.finite(res$chi2) || !is.finite(res$df) || res$df <= 0L) {
    return(NULL)
  }
  # nocov end
  res$p_chibar2 <- 0.5 * stats::pchisq(res$chi2, df = res$df,
                                          lower.tail = FALSE)
  res
}


# Count the number of free parameters in lme4's random structure:
# per grouping factor, n * (n + 1) / 2 (lower triangle of the
# variance / covariance block).
.merMod_n_re_params <- function(fit) {
  vc <- tryCatch(lme4::VarCorr(fit), error = function(e) NULL)
  if (is.null(vc)) return(NA_integer_)  # nocov -- VarCorr never fails on a converged merMod
  total <- 0L
  for (g in names(vc)) {
    n <- nrow(as.matrix(vc[[g]]))
    total <- total + n * (n + 1L) / 2L
  }
  total
}


# lme4 >= 1.1-37 moved findbars() / nobars() to the `reformulas` package and
# re-exports them with a once-per-session deprecation warning. Prefer the new
# home when installed (it is a hard dependency of those lme4 versions), fall
# back to the lme4 re-export otherwise.
.re_findbars <- function(x) {
  if (requireNamespace("reformulas", quietly = TRUE)) {
    reformulas::findbars(x)
  } else {
    suppressWarnings(lme4::findbars(x))                                  # nocov
  }
}

.re_nobars <- function(x) {
  if (requireNamespace("reformulas", quietly = TRUE)) {
    reformulas::nobars(x)
  } else {
    suppressWarnings(lme4::nobars(x))                                    # nocov
  }
}


.null_lrt_merMod <- function(fit) {
  if (!requireNamespace("lme4", quietly = TRUE)) return(NULL)
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  is_gaussian <- !is.null(fam) && identical(fam$family, "gaussian")

  no_re_formula <- suppressWarnings(
    .re_nobars(stats::formula(fit, fixed.only = TRUE))
  )
  data <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (is.null(data)) return(NULL)  # nocov -- model.frame succeeds for a fitted merMod

  # Likelihood follows the fit's estimator (ranova / Stata / RLRsim
  # convention): a REML lmer keeps its REML logLik and is compared to
  # the REML logLik of the null lm (via gls) below; ML fits and glmer
  # (always ML) stay on ML.
  is_reml <- inherits(fit, "lmerMod") && isTRUE(lme4::isREML(fit))
  fit_full_ml <- fit

  # Refit against the model frame under a SAFE response name: a transformed
  # or matrix response (e.g. `cbind(incidence, size - incidence)`) exists in
  # the model frame only as its combined deparsed column, so re-evaluating
  # the original response expression there fails ("object 'incidence' not
  # found"). model.response() hands us the already-evaluated response.
  data[[".spicy_response."]] <- stats::model.response(data)
  null_f <- tryCatch(
    stats::as.formula(
      paste(".spicy_response. ~", deparse1(no_re_formula[[3L]]))
    ),
    error = function(e) NULL
  )
  if (is.null(null_f)) return(NULL)                                    # nocov
  fit_null <- tryCatch(
    if (is_gaussian) {
      stats::lm(null_f, data = data)
    } else {
      stats::glm(null_f, data = data, family = fam)
    },
    error = function(e) NULL
  )
  if (is.null(fit_null)) return(NULL)

  ll_full <- as.numeric(stats::logLik(fit_full_ml))
  ll_null <- if (is_reml) {
    .null_reml_loglik_lm(null_f, data)
  } else {
    as.numeric(stats::logLik(fit_null))
  }
  if (!is.finite(ll_null)) return(NULL)
  chi2 <- 2 * (ll_full - ll_null)
  df_q <- .merMod_n_re_params(fit)
  if (is.na(df_q)) return(NULL)  # nocov -- .merMod_n_re_params is non-NA for a converged fit

  list(
    chi2          = chi2,
    df            = as.integer(df_q),
    family_label  = if (is_gaussian) "linear regression" else
                       sprintf("%s regression",
                               .merMod_glm_family_title_safe(fit, fam))
  )
}


# Pretty family label for the LRT footer line. Falls back to the
# bare family name when the title-case helper isn't available.
.merMod_glm_family_title_safe <- function(fit, fam) {
  if (exists(".merMod_glm_family_title", mode = "function",
              inherits = TRUE)) {
    out <- tryCatch(.merMod_glm_family_title(fit), error = function(e) NA_character_)
    if (!is.na(out) && nzchar(out)) {
      return(tolower(out))
    }
  }
  fam$family %||% "regression"
}


.null_lrt_glmmTMB <- function(fit) {
  if (!requireNamespace("glmmTMB", quietly = TRUE)) return(NULL)
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  is_gaussian <- !is.null(fam) && identical(fam$family, "gaussian")

  no_re_formula <- suppressWarnings(
    .re_nobars(stats::formula(fit, fixed.only = TRUE, component = "cond"))
  )
  data <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (is.null(data)) return(NULL)  # nocov -- model.frame succeeds for a fitted glmmTMB

  # Safe response name: see the .null_lrt_merMod comment (a transformed or
  # matrix response only exists in the model frame as its combined column).
  data[[".spicy_response."]] <- stats::model.response(data)
  null_f <- tryCatch(
    stats::as.formula(
      paste(".spicy_response. ~", deparse1(no_re_formula[[3L]]))
    ),
    error = function(e) NULL
  )
  if (is.null(null_f)) return(NULL)                                    # nocov
  fit_null <- tryCatch(
    if (is_gaussian) {
      stats::lm(null_f, data = data)
    } else {
      stats::glm(null_f, data = data, family = fam)
    },
    error = function(e) NULL
  )
  if (is.null(fit_null)) return(NULL)

  ll_full <- as.numeric(stats::logLik(fit))
  # Likelihood follows the fit's estimator: a Gaussian glmmTMB fitted
  # with REML = TRUE compares to the REML logLik of the null lm (via
  # gls). Non-Gaussian REML fits keep the ML null (no simple REML
  # null exists for a glm) -- a documented approximation.
  is_reml_gauss <- is_gaussian &&
    isTRUE(tryCatch(fit$modelInfo$REML, error = function(e) FALSE))
  ll_null <- if (is_reml_gauss) {
    .null_reml_loglik_lm(null_f, data)
  } else {
    as.numeric(stats::logLik(fit_null))
  }
  if (!is.finite(ll_null)) return(NULL)
  chi2 <- 2 * (ll_full - ll_null)

  # Count random params in glmmTMB: VarCorr()$cond is a list of
  # per-grouping covariance matrices.
  vc <- tryCatch(glmmTMB::VarCorr(fit)$cond, error = function(e) NULL)
  if (is.null(vc)) return(NULL)  # nocov -- VarCorr$cond is non-NULL for a converged glmmTMB
  df_q <- sum(vapply(vc, function(m) {
    n <- nrow(as.matrix(m))
    as.integer(n * (n + 1L) / 2L)
  }, integer(1)))
  if (df_q <= 0L) return(NULL)  # nocov -- a fitted random structure has >= 1 free parameter

  list(
    chi2 = chi2,
    df   = df_q,
    family_label = if (is_gaussian) "linear regression" else
                      sprintf("%s regression", fam$family)
  )
}


.null_lrt_lme <- function(fit) {
  if (!requireNamespace("nlme", quietly = TRUE)) return(NULL)
  no_re_formula <- stats::formula(fit)
  data <- tryCatch(nlme::getData(fit), error = function(e) NULL)
  if (is.null(data)) return(NULL)  # nocov -- getData succeeds for a fitted lme

  # nlme::lme is Gaussian by spec. Likelihood follows the fit's
  # estimator (ranova / Stata convention): a REML lme keeps its REML
  # logLik and compares to the REML logLik of the null lm (gls,
  # trivially available here since nlme is loaded); an ML lme
  # compares to the ML logLik of lm.
  is_reml <- identical(fit$method, "REML")
  fit_null <- tryCatch(stats::lm(no_re_formula, data = data),
                        error = function(e) NULL)
  if (is.null(fit_null)) return(NULL)  # nocov -- null lm fits on the same data

  ll_full <- as.numeric(stats::logLik(fit))
  ll_null <- if (is_reml) {
    .null_reml_loglik_lm(no_re_formula, data)
  } else {
    as.numeric(stats::logLik(fit_null))
  }
  if (!is.finite(ll_null)) return(NULL)
  chi2 <- 2 * (ll_full - ll_null)

  # Count random params in lme: reStruct holds per-grouping pdMat.
  re_struct <- tryCatch(fit$modelStruct$reStruct, error = function(e) NULL)
  if (is.null(re_struct)) return(NULL)  # nocov -- reStruct is present for a fitted lme
  df_q <- 0L
  for (g in names(re_struct)) {
    n <- nrow(as.matrix(nlme::pdMatrix(re_struct[[g]])))
    df_q <- df_q + as.integer(n * (n + 1L) / 2L)
  }
  if (df_q <= 0L) return(NULL)  # nocov -- a fitted random structure has >= 1 free parameter

  list(
    chi2 = chi2,
    df   = df_q,
    family_label = "linear regression"
  )
}


# Phase 7c9a: Nakagawa & Schielzeth (2013, 2017) marginal /
# conditional R^2 for mixed-effects fits. Implemented natively
# (no `performance` runtime dependency) following the formulas:
#
#   sigma^2_f = var( fixed-effect linear predictor X beta_fixed )
#   sigma^2_g = sum_g mean_i ( z_i,g  W_g  z_i,g^T )   over grouping factors g
#   sigma^2_d = family-specific distribution variance on link scale
#                (Nakagawa et al. 2017 sec. 3):
#                  gaussian  ->  sigma(fit)^2
#                  binomial(logit)   ->  pi^2 / 3
#                  binomial(probit)  ->  1
#                  binomial(cloglog) ->  pi^2 / 6
#                  poisson(log)      ->  log(1 + 1/lambda),
#                                          lambda = exp(intercept_LP + 0.5 sigma^2_g)
#
#   R^2_marginal    = sigma^2_f / (sigma^2_f + sigma^2_g + sigma^2_d)
#   R^2_conditional = (sigma^2_f + sigma^2_g) / (denominator)
#
# Returns list(marginal, conditional) of length-1 numerics, with
# NA_real_ when computation is not possible (missing engine, fit
# singular, family unsupported -- e.g. quasi, beta, nbinom, Tweedie).
#
# Cross-validated against `performance::r2_nakagawa()` to 1e-10 for
# every (engine x family) combination spicy supports, in
# tests/testthat/test-nakagawa_r2.R.
.nakagawa_r2 <- function(fit) {
  # Self-implementation: Gaussian + Bernoulli (binomial 1-trial) +
  # Poisson(log). For these three the formula is closed-form per
  # Nakagawa & Schielzeth (2013) / Nakagawa et al. (2017) and the
  # output matches `performance::r2_nakagawa()` to 1e-10 (oracle
  # cross-validation in tests/testthat/test-nakagawa_r2.R).
  comps <- tryCatch(.nakagawa_components(fit), error = function(e) NULL)
  if (!is.null(comps)) {
    out <- .nakagawa_assemble(comps)
    if (is.finite(out$marginal) && is.finite(out$conditional)) return(out)
  }
  # Fallback for cases we don't (yet) implement natively: binomial
  # with multiple trials (cbind), negative binomial / beta /
  # Tweedie families, zero-inflation, dispersion components.
  # These require the delta / trigamma / lognormal approximation
  # families catalogued in Nakagawa et al. 2017 sec. 3 + the
  # observation-level overdispersion handling implemented in
  # `insight::.variance_distributional()`. We delegate to
  # `performance::r2_nakagawa()` (Suggests) so the user sees a value
  # rather than NA, accepting the runtime dependency for this case.
  .nakagawa_r2_via_performance(fit)
}

.nakagawa_r2_via_performance <- function(fit) {
  if (!requireNamespace("performance", quietly = TRUE)) return(.nakagawa_na())
  res <- tryCatch(
    suppressWarnings(suppressMessages(performance::r2_nakagawa(fit))),
    error = function(e) NULL
  )
  if (is.null(res)) return(.nakagawa_na())  # nocov -- performance::r2_nakagawa() does not error on supported fits
  marg <- tryCatch(as.numeric(res$R2_marginal),    error = function(e) NA_real_)
  cond <- tryCatch(as.numeric(res$R2_conditional), error = function(e) NA_real_)
  if (length(marg) != 1L) marg <- NA_real_  # nocov -- r2_nakagawa() returns scalar R2_marginal
  if (length(cond) != 1L) cond <- NA_real_  # nocov -- r2_nakagawa() returns scalar R2_conditional
  list(marginal = marg, conditional = cond)
}

.nakagawa_na <- function() list(marginal = NA_real_, conditional = NA_real_)

# Engine dispatch. Returns NULL when no method applies, or a list
# with the building blocks (var_f, var_g, family, link, sigma,
# intercept_lp) consumed by .nakagawa_assemble().
.nakagawa_components <- function(fit) {
  if (inherits(fit, "glmmTMB")) return(.nakagawa_components_glmmTMB(fit))
  if (inherits(fit, "merMod"))  return(.nakagawa_components_merMod(fit))
  if (inherits(fit, "lme"))     return(.nakagawa_components_lme(fit))
  NULL
}

# Combine the components into the two R^2 values.
.nakagawa_assemble <- function(comps) {
  var_d <- .nakagawa_distribution_variance(comps)
  if (!is.finite(var_d)) return(.nakagawa_na())
  total <- comps$var_f + comps$var_g + var_d
  if (!is.finite(total) || total <= 0) return(.nakagawa_na())
  list(
    marginal    = comps$var_f / total,
    conditional = (comps$var_f + comps$var_g) / total
  )
}

# Link-scale distribution variance per Nakagawa et al. 2017 sec. 3.
# Gaussian uses the residual sigma; non-Gaussian uses the closed-form
# theoretical variance of the latent-link representation.
.nakagawa_distribution_variance <- function(comps) {
  fam  <- comps$family
  link <- comps$link

  # Gaussian / Gaussian identity: residual variance from sigma(fit).
  if (identical(fam, "gaussian")) {
    sig <- comps$sigma
    if (!is.finite(sig)) return(NA_real_)
    return(sig^2)
  }

  # Binomial: distribution variance depends on link function.
  if (identical(fam, "binomial")) {
    return(switch(link,
                  logit   = pi^2 / 3,
                  probit  = 1,
                  cloglog = pi^2 / 6,
                  NA_real_))
  }

  # Poisson log: lognormal approximation (Nakagawa et al. 2017
  # sec. 3.6, "Observation-level log-normal approximation"). The
  # baseline lambda is exp(intercept_null + 0.5 * var_g_null) where
  # the intercept AND the random-effect variance come from the NULL
  # model (random structure intact, fixed effects collapsed to ~ 1).
  # Using var_g of the FULL model would underestimate var_g_null,
  # because removing the fixed-effect predictors shifts unexplained
  # variance into the random-effect component. insight uses var_g_null;
  # we match it to keep the cross-validation tolerance at 1e-10.
  if (identical(fam, "poisson") && identical(link, "log")) {
    int_null <- comps$null_intercept
    var_g_null <- comps$null_var_g
    if (!is.finite(int_null) || !is.finite(var_g_null)) return(NA_real_)
    lambda <- exp(int_null + 0.5 * var_g_null)
    if (!is.finite(lambda) || lambda <= 0) return(NA_real_)
    return(log1p(1 / lambda))
  }

  NA_real_
}


# ---- Family-coverage gate ------------------------------------------------

# Return TRUE iff `fit`'s family is one our self-implementation covers
# exactly (matches `performance::r2_nakagawa()` to 1e-10):
#   * gaussian   identity   (any link inverse becomes identity here)
#   * binomial   logit / probit / cloglog   AND Bernoulli (single trial)
#   * poisson    log
# Returns FALSE for cbind() / matrix-response binomial (the lognormal
# distribution-variance formula is more involved -- Nakagawa et al. 2017
# sec. 3.4), negative binomial, beta, Tweedie, dispersion models, zero-
# inflation. The caller falls through to performance for those.
.nakagawa_supported_family <- function(fit) {
  fam <- tryCatch(stats::family(fit), error = function(e) NULL)
  if (is.null(fam) || is.null(fam$family)) return(FALSE)
  if (identical(fam$family, "gaussian")) return(TRUE)
  if (identical(fam$family, "binomial")) {
    if (!fam$link %in% c("logit", "probit", "cloglog")) return(FALSE)
    # Reject cbind(succ, fail) / matrix response (multi-trial binomial).
    resp <- tryCatch(stats::model.response(stats::model.frame(fit)),
                      error = function(e) NULL)
    if (is.matrix(resp) && ncol(resp) >= 2L) return(FALSE)
    return(TRUE)
  }
  if (identical(fam$family, "poisson") && identical(fam$link, "log")) {
    return(TRUE)
  }
  FALSE
}


# ---- Engine-specific component extractors --------------------------------

.nakagawa_components_merMod <- function(fit) {
  if (!requireNamespace("lme4", quietly = TRUE)) return(NULL)

  # Self-impl covers Gaussian + Bernoulli + Poisson(log). Anything else
  # (cbind binomial, negbinom, custom families) -> NULL so the caller
  # falls through to performance::r2_nakagawa().
  if (!.nakagawa_supported_family(fit)) return(NULL)

  # Fixed-effect linear predictor (population-level, re.form = NA).
  fix_lp <- tryCatch(stats::predict(fit, re.form = NA),
                      error = function(e) NULL)
  if (is.null(fix_lp) || !any(is.finite(fix_lp))) return(NULL)  # nocov -- predict(re.form=NA) is finite for a converged fit
  var_f <- as.numeric(stats::var(fix_lp))
  if (!is.finite(var_f)) return(NULL)  # nocov -- variance of a finite linear predictor is finite

  # Random-effect variance contribution: sum_g mean_i(z_i W_g z_i^T).
  # VarCorr() keys by grouping factor (e.g. "Subject"); mmList() keys
  # by the full term "<RHS> | <group>". Match them via the trailing
  # "| group" segment so (1 | g) and (x | g) both map to grouping g.
  vc <- tryCatch(lme4::VarCorr(fit),  error = function(e) NULL)
  mm_list <- tryCatch(lme4::getME(fit, "mmList"), error = function(e) NULL)
  if (is.null(vc) || is.null(mm_list)) return(NULL)  # nocov -- VarCorr / mmList present for a fitted merMod
  # Build group_for_mm: each mmList name parsed to its grouping factor.
  group_for_mm <- vapply(names(mm_list), function(nm) {
    parts <- strsplit(nm, "\\|", fixed = FALSE)[[1L]]
    if (length(parts) < 2L) return(NA_character_)  # nocov -- mmList names always contain a "| group" segment
    trimws(parts[length(parts)])
  }, character(1))
  var_g <- 0
  for (g in names(vc)) {
    W  <- as.matrix(vc[[g]])             # random-effect covariance matrix
    idx <- which(group_for_mm == g)
    if (length(idx) == 0L) return(NULL)  # nocov -- every VarCorr group has a matching mmList entry
    # Per-group contribution: sum over mmList entries assigned to g.
    for (k in idx) {
      Z <- as.matrix(mm_list[[k]])
      # The per-term mmList entry has columns matching the RHS of the
      # random-effect formula; W is keyed by the same columns.
      cols <- intersect(colnames(W), colnames(Z))
      if (length(cols) == 0L) return(NULL)  # nocov -- W and Z share random-effect column names by construction
      W_sub <- W[cols, cols, drop = FALSE]
      Z_sub <- Z[, cols, drop = FALSE]
      var_g <- var_g + sum((Z_sub %*% W_sub) * Z_sub) / nrow(Z_sub)
    }
  }
  if (!is.finite(var_g)) return(NULL)  # nocov -- accumulated random-effect variance is finite

  fam <- stats::family(fit)
  null <- .nakagawa_null_params(fit, fam)
  list(
    var_f          = var_f,
    var_g          = var_g,
    family         = fam$family,
    link           = fam$link,
    sigma          = stats::sigma(fit),
    intercept_lp   = mean(fix_lp),
    null_intercept = null$intercept,
    null_var_g     = null$var_g
  )
}

# Refit the null model (random structure intact, fixed effects ~ 1)
# and return its intercept + random-effect variance on the link scale.
# Used by the Poisson distribution-variance formula (Nakagawa et al.
# 2017 sec. 3.6, "Observation-level log-normal approximation"). Returns
# a list of NAs when not needed (Gaussian, Bernoulli -- the formula
# doesn't depend on the null model) or when the refit fails.
#
# Note. `update(fit, . ~ 1)` strips the random part for lme4 fits
# ("No random effects terms specified" error). Rebuild the formula as
# `~ 1 + (re | g)` preserving the original random structure.
.nakagawa_null_params <- function(fit, fam) {
  na_pair <- list(intercept = NA_real_, var_g = NA_real_)
  if (!identical(fam$family, "poisson")) return(na_pair)
  bars <- tryCatch(
    (.re_findbars(stats::formula(fit))),
    error = function(e) NULL
  )
  if (is.null(bars) || length(bars) == 0L) return(na_pair)  # nocov -- a mixed-effects poisson fit always has random-effect bars
  null_rhs <- paste0("1 + ",
                     paste(vapply(bars, function(b) {
                       # deparse1(), not deparse(): a long random-effect bar
                       # deparses to a multi-line character vector, which would
                       # violate the vapply `character(1)` contract and throw
                       # (silently bypassing native Nakagawa R^2). deparse1()
                       # collapses to a single string. R >= 4.0 (Depends 4.1).
                       paste0("(", deparse1(b), ")")
                     }, character(1)), collapse = " + "))
  null_formula <- stats::reformulate(
    null_rhs,
    response = as.character(stats::formula(fit)[[2L]])
  )
  null_fit <- tryCatch(
    suppressWarnings(suppressMessages(stats::update(fit, null_formula))),
    error = function(e) NULL
  )
  if (is.null(null_fit)) return(na_pair)
  fe <- tryCatch(lme4::fixef(null_fit), error = function(e) NULL)
  if (is.null(fe) || length(fe) == 0L) return(na_pair)  # nocov -- the null "~ 1 + (re|g)" fit always has an intercept
  int <- as.numeric(fe[1L])
  # Random-effect variance of the null model for the Poisson lognormal
  # baseline lambda = exp(intercept_null + 0.5 * var_g_null). Nakagawa
  # et al. (2017) / insight use the null model's random-INTERCEPT
  # variance -- the (Intercept),(Intercept) diagonal of each grouping
  # factor's VarCorr block, summed across grouping factors -- NOT the
  # full design-averaged Z W Z' / n accumulation used for var_g of the
  # FULL model. The two coincide for a plain (1 | g) structure (where
  # the only random term IS the intercept) but diverge for random-slope
  # structures: the ZWZ'/n form over-counts by folding in the slope
  # variance and intercept-slope covariance, inflating lambda and
  # corrupting the distribution variance. Verified to reproduce
  # performance::r2_nakagawa() to ~1e-16 on (x | g) Poisson fits.
  vc_null <- tryCatch(lme4::VarCorr(null_fit), error = function(e) NULL)
  if (is.null(vc_null)) return(list(intercept = int, var_g = NA_real_))  # nocov -- VarCorr present for the converged null fit
  var_g_null <- 0
  for (g in names(vc_null)) {
    W <- as.matrix(vc_null[[g]])
    int_col <- match("(Intercept)", colnames(W))
    if (is.na(int_col)) next  # nocov -- the rebuilt "1 + (re|g)" null model always carries a random intercept
    var_g_null <- var_g_null + W[int_col, int_col]
  }
  list(intercept = int, var_g = var_g_null)
}

.nakagawa_components_lme <- function(fit) {
  if (!requireNamespace("nlme", quietly = TRUE)) return(NULL)
  # nlme::lme is Gaussian / identity in the standard interface.
  # Fixed-effect linear predictor: level = 0 -> population-level.
  fix_lp <- tryCatch(stats::predict(fit, level = 0),
                      error = function(e) NULL)
  if (is.null(fix_lp) || !any(is.finite(fix_lp))) return(NULL)  # nocov -- predict(level=0) is finite for a converged lme
  var_f <- as.numeric(stats::var(fix_lp))
  if (!is.finite(var_f)) return(NULL)  # nocov -- variance of a finite linear predictor is finite

  # Random-effect variance: sum_g mean_i(z_i W_g z_i^T).
  vc <- tryCatch(.lme_random_covariance_matrices(fit),
                  error = function(e) NULL)
  if (is.null(vc)) return(NULL)  # nocov -- reStruct-derived covariance matrices are recoverable for a fitted lme
  var_g <- tryCatch(.lme_re_variance_contribution(fit, vc),
                     error = function(e) NA_real_)
  if (!is.finite(var_g)) return(NULL)  # nocov -- accumulated random-effect variance is finite

  list(
    var_f          = var_f,
    var_g          = var_g,
    family         = "gaussian",
    link           = "identity",
    sigma          = stats::sigma(fit),
    intercept_lp   = mean(fix_lp),
    null_intercept = NA_real_,
    null_var_g     = NA_real_   # not needed for Gaussian
  )
}

# Build the list of random-effect covariance matrices (one per grouping
# factor) for an `lme` fit. nlme::VarCorr() returns a character matrix
# with Variance / StdDev / Corr columns; we parse it back into numeric
# covariance matrices keyed by grouping factor.
.lme_random_covariance_matrices <- function(fit) {
  re_struct <- fit$modelStruct$reStruct
  sig2 <- stats::sigma(fit)^2
  out <- list()
  # reStruct is a list of pdMat objects keyed by grouping-factor name.
  for (g in names(re_struct)) {
    # pdMat objects store the random-effect covariance ON THE SIGMA SCALE
    # (i.e. scaled by sigma^2). nlme::pdMatrix() returns the scaled matrix;
    # multiply by sigma^2 to get the unscaled covariance.
    W_scaled <- as.matrix(nlme::pdMatrix(re_struct[[g]]))
    out[[g]] <- W_scaled * sig2
  }
  out
}

# Random-effect variance contribution for an `lme` fit: sum over
# grouping factors g of mean_i(z_i W_g z_i^T).
.lme_re_variance_contribution <- function(fit, vc_list) {
  data <- tryCatch(nlme::getData(fit), error = function(e) NULL)
  if (is.null(data)) return(NA_real_)  # nocov -- getData succeeds for a fitted lme
  re_formulas <- nlme::reStruct(fit$modelStruct$reStruct)
  total <- 0
  n <- stats::nobs(fit)
  for (g in names(vc_list)) {
    W <- vc_list[[g]]
    rhs <- attr(re_formulas[[g]], "formula")
    if (is.null(rhs)) rhs <- stats::formula(re_formulas[[g]])  # nocov -- reStruct entries always carry a "formula" attribute
    Z <- stats::model.matrix(rhs, data = data)
    if (nrow(Z) != n) return(NA_real_)  # nocov -- Z is built from the same n-row model data
    total <- total + sum((Z %*% W) * Z) / n
  }
  total
}

# glmmTMB: implemented via the engine's own VarCorr + model.matrix
# infrastructure for the conditional model. zero-inflation and
# multi-level dispersion components are NOT included (Nakagawa et al.
# 2017 only formalises the standard one-level case); fits with active
# zi / dispformula components return NA gracefully.
.nakagawa_components_glmmTMB <- function(fit) {
  if (!requireNamespace("glmmTMB", quietly = TRUE)) return(NULL)

  # Self-impl families only; fall through to performance otherwise.
  if (!.nakagawa_supported_family(fit)) return(NULL)
  # Refuse zero-inflated / dispformula models (Nakagawa formulas don't
  # cover these without the extra components performance handles).
  zi_form    <- tryCatch(stats::formula(fit, component = "zi"),
                          error = function(e) NULL)
  disp_form  <- tryCatch(stats::formula(fit, component = "disp"),
                          error = function(e) NULL)
  if (!is.null(zi_form) && length(zi_form) >= 3L &&
      !identical(deparse(zi_form[[length(zi_form)]]), "0")) {
    return(NULL)  # nocov -- glmmTMB zi formulas are one-sided (length 2); this >= 3 arm is a defensive guard
  }
  if (!is.null(disp_form) && length(disp_form) >= 2L &&
      !identical(deparse(disp_form[[length(disp_form)]]), "1") &&
      !identical(deparse(disp_form[[length(disp_form)]]), "")) {
    # ~1 is the default flat dispersion -- fine. Anything else: bail.
    if (length(all.vars(disp_form)) > 0L) return(NULL)
  }

  # Conditional fixed-effect linear predictor.
  fix_lp <- tryCatch(stats::predict(fit, re.form = NA, type = "link"),
                      error = function(e) NULL)
  if (is.null(fix_lp) || !any(is.finite(fix_lp))) return(NULL)  # nocov -- conditional predict is finite for a converged glmmTMB
  var_f <- as.numeric(stats::var(fix_lp))
  if (!is.finite(var_f)) return(NULL)  # nocov -- variance of a finite linear predictor is finite

  # Random-effect variance: glmmTMB::VarCorr()$cond is a list of
  # grouping-factor covariance matrices on the variance scale. We
  # reconstruct the per-group random-effect model matrix from the
  # right-hand side of each random-effect formula in formula(fit).
  vc <- tryCatch(glmmTMB::VarCorr(fit), error = function(e) NULL)
  if (is.null(vc) || is.null(vc$cond)) return(NULL)  # nocov -- VarCorr$cond is present for a fitted glmmTMB
  var_g <- tryCatch(.glmmTMB_re_variance_contribution(fit, vc$cond),
                     error = function(e) NA_real_)
  if (!is.finite(var_g)) return(NULL)  # nocov -- accumulated random-effect variance is finite

  fam <- stats::family(fit)
  null <- .nakagawa_null_params(fit, fam)
  list(
    var_f          = var_f,
    var_g          = var_g,
    family         = fam$family,
    link           = fam$link,
    sigma          = tryCatch(stats::sigma(fit), error = function(e) NA_real_),
    intercept_lp   = mean(fix_lp),
    null_intercept = null$intercept,
    null_var_g     = null$var_g
  )
}

# Per-grouping random-effect variance contribution for glmmTMB. Builds
# the per-row random-effect model matrix from the random-effect formula
# (RHS of each (...|group) term in formula(fit, component = "cond")).
.glmmTMB_re_variance_contribution <- function(fit, vc_cond) {
  data <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (is.null(data)) return(NA_real_)  # nocov -- model.frame succeeds for a fitted glmmTMB
  # lme4-style parser: pick (..|group) terms from the conditional formula.
  full_formula <- stats::formula(fit, component = "cond")
  re_terms <- .extract_re_terms(full_formula)
  if (length(re_terms) == 0L) return(NA_real_)  # nocov -- a mixed-effects fit always has >= 1 random-effect term
  n <- stats::nobs(fit)
  total <- 0
  for (g in names(vc_cond)) {
    W <- as.matrix(vc_cond[[g]])
    # Find the random-effect formula whose RHS-group matches g.
    re_form <- re_terms[[g]]
    if (is.null(re_form)) next  # nocov -- every VarCorr$cond group has a matching parsed random-effect term
    Z <- tryCatch(
      stats::model.matrix(re_form, data = data),
      error = function(e) NULL
    )
    if (is.null(Z) || nrow(Z) != n) return(NA_real_)  # nocov -- Z is built from the same n-row model data
    total <- total + sum((Z %*% W) * Z) / n
  }
  total
}

# Walk a (lme4-style) formula and return a named list of random-effect
# formulas (one per grouping factor), keyed by grouping factor name.
# `(slope | group)` -> list(group = ~ slope), `(1 | group)` -> list(group = ~ 1).
.extract_re_terms <- function(formula) {
  rhs <- formula[[length(formula)]]
  out <- list()
  walk <- function(node) {
    if (length(node) == 1L) return(invisible(NULL))
    op <- as.character(node[[1L]])
    if (op == "(" && length(node) == 2L) {
      walk(node[[2L]])
      return(invisible(NULL))
    }
    if (op %in% c("+", "*", ":")) {
      for (k in 2:length(node)) walk(node[[k]])
      return(invisible(NULL))
    }
    if (op == "|") {
      slope_expr <- node[[2L]]
      group_expr <- node[[3L]]
      slope_f <- stats::as.formula(call("~", slope_expr))
      group  <- deparse(group_expr)
      out[[group]] <<- slope_f
    }
  }
  walk(rhs)
  out
}
