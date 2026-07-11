# broom integration for table_regression() -- Step 12.
#
# Per dev/table_regression_design.md Q8 / Q17 / Q18 / Q20:
#
#   tidy()   -- long format, broom-canonical column names; one row per
#              (model_id, term, estimate_type) triplet across all models.
#              Drops reference-row placeholders (NA estimates).
#
#   glance() -- one row per (model_id, outcome) with model-level
#              statistics. Numeric `df.residual` (Satterthwaite-safe).
#
#   as.data.frame() / as_tibble() -- wide raw output (= the same content
#              as `output = "data.frame"`). Drops spicy classes and
#              rendering attrs; keeps title / note as plain attributes
#              for users who want them.
#
# Conventions:
#   * snake_case spicy tokens (`r2`, `adj_r2`, `p_value`, `ci_low`,
#     `ci_high`, `se`, `df_residual`) -> broom canonical
#     (`r.squared`, `adj.r.squared`, `p.value`, `conf.low`,
#     `conf.high`, `std.error`, `df.residual`).
#   * `df.residual` is kept numeric, not integer, to preserve
#     Satterthwaite-corrected df under cluster-robust vcov
#     (Pustejovsky & Tipton 2018).


#' Tidy / glance methods for `spicy_regression_table`
#'
#' Standard [broom::tidy()] / [broom::glance()] interfaces for an
#' object returned by [table_regression()]. They re-shape the
#' underlying long-format data into the two canonical broom views
#' so the table can be consumed by any downstream tidyverse-stats
#' pipeline.
#'
#' `tidy()` returns one row per `(model_id, term, estimate_type,
#' outcome_level)` combination, with `estimate_type` in
#' `c("B", "beta", "ame", "partial_f2", "partial_eta2", "partial_omega2")`.
#' `outcome_level` names the response category of per-category rows
#' (ordinal / multinomial average marginal effects) and is `NA` for
#' single-outcome models. Reference-row placeholders (factor reference
#' levels) and singular coefficients (NA estimates) are dropped. Columns:
#' `model_id, outcome, outcome_level, term, estimate_type, estimate,
#' std.error, conf.low, conf.high, statistic, df, p.value, test_type,
#' is_intercept, factor_term, factor_level`.
#'
#' `glance()` returns one row per `(model_id, outcome)` with
#' model-level statistics. Columns: `model_id, outcome, nobs,
#' weighted_nobs, r.squared, adj.r.squared, omega2, sigma, rmse,
#' f2, AIC, AICc, BIC, deviance, df.residual` (numeric --
#' Satterthwaite-safe).
#'
#' @param x A `spicy_regression_table` returned by
#'   [table_regression()].
#' @param ... Currently ignored. Present for compatibility with the
#'   [broom::tidy()] / [broom::glance()] generics.
#'
#' @return A `tbl_df` (when `tibble` is installed) or a plain
#'   `data.frame`.
#'
#' @seealso [as.data.frame.spicy_regression_table()] for the wide
#'   raw view.
#'
#' @name tidy.spicy_regression_table
#' @keywords internal
NULL

#' @rdname tidy.spicy_regression_table
#' @exportS3Method broom::tidy
tidy.spicy_regression_table <- function(x, ...) {
  long <- attr(x, "spicy_long")
  if (is.null(long) || nrow(long) == 0L) {
    return(maybe_as_tibble(empty_tidy_long()))
  }
  # Drop reference-row placeholders -- they have no estimable values.
  long <- long[!long$is_reference, , drop = FALSE]
  # Drop rows whose primary estimate is NA (singular coefs / partial
  # rows for intercept-only or singular models).
  long <- long[!is.na(long$estimate), , drop = FALSE]

  # Per-category rows (ordinal / multinomial AME) are otherwise
  # INDISTINGUISHABLE in the long frame: four `age` AME rows share the same
  # term and estimate_type, and nothing names the response category they
  # belong to. Carry `outcome_level` so the frame is self-describing (NA for
  # single-outcome models).
  outcome_level <- if (!is.null(long$outcome_level)) {
    as.character(long$outcome_level)
  } else {
    rep(NA_character_, nrow(long))
  }
  out <- data.frame(
    model_id      = long$model_id,
    outcome       = long$outcome,
    outcome_level = outcome_level,
    term          = long$term,
    estimate_type = long$estimate_type,
    estimate      = long$estimate,
    std.error     = long$se,
    conf.low      = long$ci_low,
    conf.high     = long$ci_high,
    statistic     = long$statistic,
    df            = long$df,
    p.value       = long$p_value,
    test_type     = long$test_type,
    is_intercept  = long$is_intercept,
    factor_term   = long$factor_term,
    factor_level  = long$factor_level,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  maybe_as_tibble(out)
}

#' @rdname tidy.spicy_regression_table
#' @exportS3Method broom::glance
glance.spicy_regression_table <- function(x, ...) {
  fs <- attr(x, "spicy_fit_stats")
  if (is.null(fs) || nrow(fs) == 0L) {
    return(maybe_as_tibble(empty_glance()))
  }
  out <- data.frame(
    model_id       = fs$model_id,
    outcome        = fs$outcome,
    nobs           = as.integer(fs$nobs),
    weighted_nobs  = fs$weighted_nobs,
    r.squared      = fs$r2,
    adj.r.squared  = fs$adj_r2,
    omega2         = fs$omega2,
    sigma          = fs$sigma,
    rmse           = fs$rmse,
    f2             = fs$f2,
    # broom's glance() convention keeps the UPPERCASE column names;
    # the source columns follow the lowercase 0.13 token schema.
    AIC            = fs$aic,
    AICc           = fs$aicc,
    BIC            = fs$bic,
    deviance       = fs$deviance,
    # df.residual kept numeric -- see file header.
    df.residual    = as.numeric(fs$df_residual),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  maybe_as_tibble(out)
}


# ---- as.data.frame / as_tibble -------------------------------------------

#' Convert a `spicy_regression_table` to a plain data.frame / tibble
#'
#' Strips the `spicy_regression_table` / `spicy_table` classes and
#' the `col_spec` rendering metadata, returning the wide character
#' display as a plain `data.frame` (or `tbl_df` via `as_tibble()`).
#' The `title` and `note` attributes are preserved.
#'
#' Equivalent to passing `output = "data.frame"` to
#' [table_regression()].
#'
#' @param x A `spicy_regression_table` returned by
#'   [table_regression()].
#' @param row.names,optional Standard `as.data.frame()` arguments
#'   (currently ignored -- the table's row layout is preserved).
#' @param ... Currently ignored.
#'
#' @return A plain `data.frame` (for `as.data.frame()`) or a
#'   `tbl_df` (for `as_tibble()`).
#'
#' @seealso [tidy.spicy_regression_table()],
#'   [glance.spicy_regression_table()] for broom-canonical long
#'   views.
#'
#' @name as.data.frame.spicy_regression_table
#' @keywords internal
NULL

#' @rdname as.data.frame.spicy_regression_table
#' @exportS3Method base::as.data.frame
as.data.frame.spicy_regression_table <- function(
    x,
    row.names = NULL,
    optional = FALSE,
    ...) {
  unclass_spicy_regression_table(x)
}

#' @rdname as.data.frame.spicy_regression_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_regression_table <- function(x, ...) {
  # tibble is a hard dependency (Imports), so no availability guard
  # is needed here -- `tibble::as_tibble()` resolves at install time.
  tibble::as_tibble(unclass_spicy_regression_table(x), ...)
}


# ---- Helpers -------------------------------------------------------------

unclass_spicy_regression_table <- function(x) {
  out <- as.data.frame.data.frame(x)        # avoid recursion
  attr(out, "title")          <- attr(x, "title")
  attr(out, "note")           <- attr(x, "note")
  attr(out, "spicy_long")     <- NULL
  attr(out, "spicy_fit_stats") <- NULL
  attr(out, "col_spec")       <- NULL
  class(out) <- "data.frame"
  out
}

maybe_as_tibble <- function(df) {
  # tibble is a hard dependency (Imports); the `maybe_` prefix is
  # historical and kept for call-site readability -- the function
  # always returns a tbl_df.
  tibble::as_tibble(df)
}

empty_tidy_long <- function() {
  data.frame(
    model_id      = character(0),
    outcome       = character(0),
    outcome_level = character(0),
    term          = character(0),
    estimate_type = character(0),
    estimate      = numeric(0),
    std.error     = numeric(0),
    conf.low      = numeric(0),
    conf.high     = numeric(0),
    statistic     = numeric(0),
    df            = numeric(0),
    p.value       = numeric(0),
    test_type     = character(0),
    is_intercept  = logical(0),
    factor_term   = character(0),
    factor_level  = character(0),
    stringsAsFactors = FALSE
  )
}

empty_glance <- function() {
  data.frame(
    model_id      = character(0),
    outcome       = character(0),
    nobs          = integer(0),
    weighted_nobs = numeric(0),
    r.squared     = numeric(0),
    adj.r.squared = numeric(0),
    omega2        = numeric(0),
    sigma         = numeric(0),
    rmse          = numeric(0),
    f2            = numeric(0),
    AIC           = numeric(0),
    AICc          = numeric(0),
    BIC           = numeric(0),
    deviance      = numeric(0),
    df.residual   = numeric(0),
    stringsAsFactors = FALSE
  )
}
