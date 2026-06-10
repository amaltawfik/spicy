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
        "Supported classes today: lm, glm. ",
        "Mixed-effects (lmer, glmer), survey (svyglm), and Bayesian ",
        "(stanreg, brmsfit) are on the 2026-2027 roadmap."
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

  # Restricted values for estimate_type. The design doc canonical
  # vocabulary is c("B", "beta", "ame") (lowercase); the legacy
  # extract_lm_phase1() pipeline additionally emits "AME" (uppercase
  # capitalisation accident, harmless) and partial-effect-size tokens
  # ("partial_f2", "partial_eta2", "partial_omega2", "partial_chi2")
  # via extract_partial_effect_rows(). During the strangler-fig phases
  # (Phase 0b sub-steps 2-4) the frame must accept the full legacy
  # vocabulary so the round-trip adapter does not need to rewrite
  # rows. Sub-step 5 will decide whether to normalise to lowercase
  # and / or split partial_* into a dedicated pipeline.
  allowed_types <- c(
    "B", "beta", "ame", "AME",
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
  required_supports <- c(
    "ame", "partial_effect_size", "classical_r2",
    "nested_lrt", "exponentiate", "standardise_refit"
  )
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
