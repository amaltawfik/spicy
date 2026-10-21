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
#   as.data.frame() / as_tibble() -- the displayed table with its
#              numbers (= `output = "data.frame"`), read off the typed
#              view; the note is its one attribute.
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
#' @return A `tbl_df`.
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
    model_id = long$model_id,
    outcome = long$outcome,
    outcome_level = outcome_level,
    term = long$term,
    estimate_type = long$estimate_type,
    estimate = long$estimate,
    std.error = long$se,
    conf.low = long$ci_low,
    conf.high = long$ci_high,
    statistic = long$statistic,
    df = long$df,
    p.value = long$p_value,
    test_type = long$test_type,
    is_intercept = long$is_intercept,
    factor_term = long$factor_term,
    factor_level = long$factor_level,
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
    model_id = fs$model_id,
    outcome = fs$outcome,
    nobs = as.integer(fs$nobs),
    weighted_nobs = fs$weighted_nobs,
    r.squared = fs$r2,
    adj.r.squared = fs$adj_r2,
    omega2 = fs$omega2,
    sigma = fs$sigma,
    rmse = fs$rmse,
    f2 = fs$f2,
    # broom's glance() convention keeps the UPPERCASE column names;
    # the source columns follow the lowercase 0.13 token schema.
    AIC = fs$aic,
    AICc = fs$aicc,
    BIC = fs$bic,
    deviance = fs$deviance,
    # df.residual kept numeric -- see file header.
    df.residual = as.numeric(fs$df_residual),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  maybe_as_tibble(out)
}


# ---- as.data.frame / as_tibble -------------------------------------------

#' Convert a `spicy_regression_table` to a plain data.frame / tibble
#'
#' `as.data.frame()` returns the table [table_regression()] displays,
#' with its numbers: exactly what `output = "data.frame"` returns. One
#' row per printed row -- the coefficients, the header and the levels of
#' each factor, the reference levels, the fit statistics -- with the key
#' columns `variable` (the variable of the row: the term of a
#' coefficient, the factor of a level, the token of a fit statistic such
#' as `"nobs"` or `"r2"`), `label` (the label displayed for that
#' variable, or the text of a fit-statistic row), `level` (the level of a
#' factor row) and `.row_role` (`"coef"`, `"factor_header"`, `"level"`,
#' `"reference"`, `"fit_stat"`, ..., the roles of [as_structured()]),
#' then the printed columns under their names, numeric: the interval of
#' a coefficient is split into its two bounds (`"95\% CI LL"`,
#' `"95\% CI UL"`), a fit statistic sits in the first column of its
#' model, and a cell of events over a total (`n_events`) keeps the
#' events in its column and the total beside it (`"<column>: N"`). With
#' several models each column name starts with the model's label. The
#' note, as printed, is kept in `attr(, "note")`. `as_tibble()` returns
#' the same table as a tibble.
#'
#' For one row per coefficient with the broom names, use
#' `output = "long"` or [tidy.spicy_regression_table()].
#'
#' @param x A `spicy_regression_table` returned by
#'   [table_regression()].
#' @param row.names,optional Standard `as.data.frame()` arguments
#'   (currently ignored -- the table's row layout is preserved).
#' @param ... Currently ignored.
#'
#' @return A plain `data.frame` (for `as.data.frame()`) or a
#'   `tbl_df` (for `as_tibble()`), identical to the
#'   `output = "data.frame"` of the same call.
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
  ...
) {
  .regression_data_frame(x)
}

#' @rdname as.data.frame.spicy_regression_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_regression_table <- function(x, ...) {
  # tibble is a hard dependency (Imports), so no availability guard
  # is needed here -- `tibble::as_tibble()` resolves at install time.
  tibble::as_tibble(.regression_data_frame(x), ...)
}


# ---- Helpers -------------------------------------------------------------

# The displayed table as a data frame (decision 5 of the ADR of
# 2026-10-20), read off the typed view the engines render: numbers where
# the console prints strings, the interval already in two bounds, the
# rendering attributes left behind (`col_spec`, `structured`, ...: the
# data frames of the family carry the note alone).
#
# The label of a row's variable is the one the table displays: the
# `labels` entry of the term (or of the factor), or the block name of a
# subordinate block (thresholds, random effects), or the term itself.
.regression_data_frame <- function(x) {
  st <- as_structured(x)
  body <- st$body
  term_labels <- attr(x, "var_labels", exact = TRUE)
  vars <- unique(body[[".variable"]][!is.na(body[[".variable"]])])
  labels <- vapply(
    vars,
    function(v) {
      if (!is.null(term_labels) && v %in% names(term_labels)) {
        term_labels[[v]]
      } else {
        .reg_block_label(v)
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
  names(labels) <- vars
  df <- .table_data_frame(
    st,
    labels,
    key = "level",
    parts = .regression_df_parts(st, attr(x, "spicy_long", exact = TRUE)),
    note = attr(x, "note", exact = TRUE)
  )
  # The bounds of an interval are named after the interval and the bound,
  # "95% CI LL", as in every other table of the family. The typed view
  # keeps its own sub-column grammar ("95% CI: LL"), part of the
  # `as_structured()` contract.
  for (nm in intersect(names(st$col_meta), names(df))) {
    role <- st$col_meta[[nm]]$ci_role
    suffix <- paste0(": ", role)
    if (
      length(role) == 1L &&
        role %in% c(.REG_CI_ROLE_LL, .REG_CI_ROLE_UL) &&
        endsWith(nm, suffix)
    ) {
      names(df)[names(df) == nm] <- paste(
        substr(nm, 1L, nchar(nm) - nchar(suffix)),
        role
      )
    }
  }
  df
}

# The cells of the typed view that print two numbers: the `n_events`
# column holds the events and displays "events/N". Its total travels
# beside it as `"<column>: N"`, the sub-column grammar of the typed view
# ("95% CI: LL"). Read off the long frame -- the model of the column, the
# term (or the factor and its level) of the row -- never parsed back from
# the displayed string.
.regression_df_parts <- function(st, long) {
  body <- st$body
  parts <- list()
  if (is.null(long) || nrow(long) == 0L) {
    return(parts)
  }
  for (nm in .struct_value_cols(body)) {
    meta <- st$col_meta[[nm]]
    if (!identical(meta$token, "n_events")) {
      next
    }
    total <- rep(NA_real_, nrow(body))
    sub <- long[long$model_id == meta$model_id, , drop = FALSE]
    for (i in which(!is.na(body[[nm]]))) {
      v <- body[[".variable"]][[i]]
      lvl <- body[[".level"]][[i]]
      hit <- if (!is.na(lvl)) {
        which(sub$factor_term %in% v & sub$factor_level %in% lvl)
      } else {
        which(sub$term %in% v)
      }
      if (length(hit)) {
        total[[i]] <- as.numeric(sub$events_n[hit[[1L]]])
      }
    }
    parts[[nm]] <- stats::setNames(list(total), paste0(nm, ": N"))
  }
  parts
}

maybe_as_tibble <- function(df) {
  # tibble is a hard dependency (Imports); the `maybe_` prefix is
  # historical and kept for call-site readability -- the function
  # always returns a tbl_df.
  tibble::as_tibble(df)
}

empty_tidy_long <- function() {
  data.frame(
    model_id = character(0),
    outcome = character(0),
    outcome_level = character(0),
    term = character(0),
    estimate_type = character(0),
    estimate = numeric(0),
    std.error = numeric(0),
    conf.low = numeric(0),
    conf.high = numeric(0),
    statistic = numeric(0),
    df = numeric(0),
    p.value = numeric(0),
    test_type = character(0),
    is_intercept = logical(0),
    factor_term = character(0),
    factor_level = character(0),
    stringsAsFactors = FALSE
  )
}

empty_glance <- function() {
  data.frame(
    model_id = character(0),
    outcome = character(0),
    nobs = integer(0),
    weighted_nobs = numeric(0),
    r.squared = numeric(0),
    adj.r.squared = numeric(0),
    omega2 = numeric(0),
    sigma = numeric(0),
    rmse = numeric(0),
    f2 = numeric(0),
    AIC = numeric(0),
    AICc = numeric(0),
    BIC = numeric(0),
    deviance = numeric(0),
    df.residual = numeric(0),
    stringsAsFactors = FALSE
  )
}
