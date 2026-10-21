# Console rendering and coercion for `table_outcome()`.

#' Print method for outcome tables
#'
#' @description
#' Formats and prints a `spicy_outcome_table` object as a styled ASCII
#' table using [spicy_print_table()].
#'
#' @param x A `data.frame` of class `"spicy_outcome_table"` as returned
#'   by [table_outcome()].
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_outcome()], [spicy_print_table()]
#' @keywords internal
#' @export
print.spicy_outcome_table <- function(x, ...) {
  # This method re-formats from the raw values, so a journal style used
  # to build the table has to be back in force here.
  .style_pushed <- .style_restore(x)
  on.exit(.style_end(.style_pushed), add = TRUE)

  decimal_mark <- attr(x, "decimal_mark") %||% "."
  align <- attr(x, "align") %||% "decimal"
  ci_level <- attr(x, "ci_level") %||% 0.95
  display_df <- .outcome_rendered_df(x)

  # One stub column: the block geometry lives in the ROW roles, not in
  # a second label column. That is the whole shape difference with
  # `table_continuous()`, and it is why the level labels carry their
  # own indent.
  align_left <- 1L
  numeric_j <- setdiff(seq_along(display_df), align_left)

  if (identical(align, "decimal") && length(numeric_j) > 0L) {
    for (j in numeric_j) {
      display_df[[j]] <- decimal_align_strings(
        display_df[[j]],
        decimal_mark = decimal_mark
      )
    }
    align_center <- numeric_j
  } else if (identical(align, "center")) {
    align_center <- numeric_j
  } else {
    align_center <- integer(0)
  }

  # A rule above every block, the marginal row included -- the shared
  # predicate, read off the typed roles rather than off the labels.
  sep_rows <- .struct_block_sep_rows(list(body = .outcome_body_geometry(x)))

  header_labels <- .continuous_labels(
    names(display_df),
    ci_level,
    decimal_mark,
    es_ci = isTRUE(attr(x, "show_effect_size_ci"))
  )

  padding <- 2L
  col_widths <- vapply(
    seq_along(display_df),
    function(i) {
      max(
        crayon::col_nchar(
          c(header_labels[i], as.character(display_df[[i]])),
          type = "width"
        ),
        na.rm = TRUE
      )
    },
    numeric(1)
  )
  console_w <- getOption("width", 80L)
  if (sum(col_widths + padding + 2L) + 1L > console_w) {
    padding <- 0L
  }

  spicy_print_table(
    display_df,
    title = .outcome_title(attr(x, "outcome_label", exact = TRUE)),
    note = attr(x, "note", exact = TRUE),
    padding = padding,
    first_column_line = TRUE,
    row_total_line = FALSE,
    bottom_line = FALSE,
    align_left_cols = align_left,
    align_center_cols = align_center,
    group_sep_rows = sep_rows,
    display_labels = header_labels
  )

  invisible(x)
}

# Internal: the display frame of an outcome table, rebuilt when the
# object does not carry one (a frame that traveled through
# `structure()` or a `[` that dropped the attribute).
.outcome_rendered_df <- function(x) {
  cached <- attr(x, "display_df", exact = TRUE)
  # The cache is only usable while it still describes THIS object.
  # `[.data.frame` copies every attribute onto the subset, so
  # `print(x[1:4, ])` used to render the eight original rows -- with
  # the block rules recomputed from the four-row subset, so the body
  # and its rules did not even agree with each other. A row count is
  # the cheapest thing that tells the two apart.
  if (!is.null(cached) && identical(nrow(cached), nrow(x))) {
    return(cached)
  }
  .outcome_display_df(
    x,
    tokens = attr(x, "show_columns", exact = TRUE),
    digits = attr(x, "digits") %||% 2L,
    effect_size_digits = attr(x, "effect_size_digits") %||% 2L,
    p_digits = attr(x, "p_digits") %||% 3L,
    decimal_mark = attr(x, "decimal_mark") %||% ".",
    ci_level = attr(x, "ci_level") %||% 0.95,
    show_statistic = isTRUE(attr(x, "show_statistic")),
    show_p = isTRUE(attr(x, "show_p")),
    show_effect_size = isTRUE(attr(x, "show_effect_size")),
    show_effect_size_ci = isTRUE(attr(x, "show_effect_size_ci")),
    indent_text = attr(x, "indent_text") %||% "  "
  )
}

# Internal: the two identity columns the shared geometry predicates
# read. The compute frame already carries the role; the indent follows
# from it, and both are the same fields the typed view exposes.
.outcome_body_geometry <- function(x) {
  role <- x$.row_role
  data.frame(
    .row_role = role,
    .indent = ifelse(role %in% c("level", "missing"), 1L, 0L),
    stringsAsFactors = FALSE
  )
}

# ---- Coercion to plain data.frame / tibble --------------------------------

# Internal: drop the spicy classes and the rendering-only attributes,
# keeping the data.frame contract plus the two provenance markers that
# say what the table was about, and the note. Used by every coercion /
# broom method.
unclass_spicy_outcome_table <- function(x) {
  keep <- list(
    outcome = attr(x, "outcome", exact = TRUE),
    select = attr(x, "select", exact = TRUE),
    note = attr(x, "note", exact = TRUE)
  )
  for (nm in setdiff(names(attributes(x)), c("names", "row.names", "class"))) {
    attr(x, nm) <- NULL
  }
  class(x) <- "data.frame"
  for (nm in names(keep)) {
    if (!is.null(keep[[nm]])) {
      attr(x, nm) <- keep[[nm]]
    }
  }
  x
}

# The displayed table as a data frame (decision 5 of the ADR of
# 2026-10-20): `as.data.frame()` and `output = "data.frame"` both call
# it, on the object the table prints. One body row per compute-frame row.
.outcome_data_frame <- function(x) {
  st <- as_structured(x)
  .table_data_frame(
    st,
    stats::setNames(x$label, x$variable),
    key = "level",
    parts = .continuous_df_parts(
      st,
      x,
      es_ci = isTRUE(attr(x, "show_effect_size_ci", exact = TRUE))
    ),
    note = attr(x, "note", exact = TRUE)
  )
}

#' Coerce a `spicy_outcome_table` to a plain data frame or tibble
#'
#' `as.data.frame()` returns the table [table_outcome()] displays, with
#' its numbers: exactly what `output = "data.frame"` returns. One row per
#' printed row -- the `Overall` row, then for each grouping its header
#' row, which carries the block's comparison, and its levels -- with the
#' key columns `variable` (the outcome on the `Overall` row, the
#' grouping elsewhere), `label`, `level` and `.row_role` (`"summary"`,
#' `"factor_header"`, `"level"`, or `"missing"`), then the printed
#' columns under their names (`"M"`, `"SD"`, ..., `"n"`, `"p"`). A cell
#' that prints several numbers keeps the others beside it, as in
#' [as.data.frame.spicy_continuous_table()]. The note, as printed, is
#' kept in `attr(, "note")`. `as_tibble()` returns the same table as a
#' tibble. The original `x` is unaffected, and `print(x)` continues to
#' render the formatted table.
#'
#' For every statistic the table computes, use `output = "long"` or
#' [tidy.spicy_outcome_table()].
#'
#' @param x A `spicy_outcome_table` returned by [table_outcome()].
#' @param row.names,optional Standard [base::as.data.frame()]
#'   arguments, currently ignored.
#' @param ... Further arguments passed to [tibble::as_tibble()] (for
#'   the tibble method) or ignored.
#'
#' @return A plain `data.frame` (or `tbl_df`), identical to the
#'   `output = "data.frame"` of the same call.
#'
#' @seealso [tidy.spicy_outcome_table()], [glance.spicy_outcome_table()].
#'
#' @name as.data.frame.spicy_outcome_table
#' @keywords internal
NULL

#' @rdname as.data.frame.spicy_outcome_table
#' @exportS3Method base::as.data.frame
as.data.frame.spicy_outcome_table <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  .outcome_data_frame(x)
}

#' @rdname as.data.frame.spicy_outcome_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_outcome_table <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    spicy_abort("Install package 'tibble'.", class = "spicy_missing_pkg")
  }
  tibble::as_tibble(.outcome_data_frame(x), ...)
}

# ---- broom integration ----------------------------------------------------

#' Tidying methods for a `spicy_outcome_table`
#'
#' Standard [broom::tidy()] and [broom::glance()] interfaces for an
#' object returned by [table_outcome()].
#'
#' `tidy()` returns the DESCRIBED rows: the marginal Overall row and
#' one row per (grouping x level). Columns: `outcome` (the outcome
#' name, constant down the frame), `variable` (the grouping, or the
#' outcome itself on the marginal row), `label`, `level` (`NA` on the
#' marginal row), `.row_role` (`"summary"` on the marginal row,
#' `"missing"` on the row of missing values the table adds, `"level"`
#' otherwise), `estimate` (the mean), `std.error` (`sd / sqrt(n)`),
#' `conf.low`, `conf.high`, `n`, `weighted_n` (the sum of the weights,
#' `NA` without `weights`), `median`, `q1`, `q3`, `min`, `max`, `sd`:
#' the statistics of [tidy.spicy_continuous_table()].
#'
#' Two identity columns where the sibling has one, and deliberately:
#' here the outcome is fixed and the variable changes, so a single
#' `outcome` column would have to mean two different things down the
#' frame. The schema reads without knowing which function produced it.
#'
#' `glance()` returns one row per grouping -- one BLOCK -- with that
#' block's own comparison. Columns: `outcome`, `variable`, `label`,
#' `n_levels`, `test_type`, `statistic`, `df`, `df.residual`,
#' `p.value`, `es_type`, `es_value`, `es_ci_lower`, `es_ci_upper`,
#' `nobs`. Index by NAME rather than by position.
#'
#' `n_levels` counts the levels the TABLE displays, so a missing-value
#' display level counts; the comparison behind `test_type` runs on the
#' observed levels only, as it does everywhere in the family.
#'
#' @param x A `spicy_outcome_table` returned by [table_outcome()].
#' @param ... Ignored, for S3 compatibility.
#'
#' @return A `tbl_df` (or `data.frame` when tibble is not installed).
#'
#' @name tidy.spicy_outcome_table
#' @keywords internal
NULL

.outcome_as_tbl <- function(df) {
  rownames(df) <- NULL
  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(df))
  }
  df
}

#' @rdname tidy.spicy_outcome_table
#' @exportS3Method broom::tidy
tidy.spicy_outcome_table <- function(x, ...) {
  outcome <- attr(x, "outcome", exact = TRUE)
  long <- unclass_spicy_outcome_table(x)
  # The header rows describe nothing: they carry the block comparison,
  # which is what `glance()` is for.
  rows <- long[long$.row_role != "factor_header", , drop = FALSE]
  .as_broom_tbl(data.frame(
    outcome = rep(outcome, nrow(rows)),
    variable = rows$variable,
    label = rows$label,
    level = rows$level,
    .row_role = rows$.row_role,
    estimate = rows$mean,
    std.error = ifelse(
      is.na(rows$sd) | is.na(rows$n) | rows$n < 1L,
      NA_real_,
      rows$sd / sqrt(rows$n)
    ),
    conf.low = rows$ci_lower,
    conf.high = rows$ci_upper,
    n = as.integer(rows$n),
    weighted_n = rows$weighted_n,
    median = rows$median,
    q1 = rows$q1,
    q3 = rows$q3,
    min = rows$min,
    max = rows$max,
    sd = rows$sd,
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}

#' @rdname tidy.spicy_outcome_table
#' @exportS3Method broom::glance
glance.spicy_outcome_table <- function(x, ...) {
  outcome <- attr(x, "outcome", exact = TRUE)
  long <- unclass_spicy_outcome_table(x)
  headers <- long[long$.row_role == "factor_header", , drop = FALSE]
  lvl <- long[long$.row_role %in% c("level", "missing"), , drop = FALSE]
  n_levels <- vapply(
    headers$variable,
    function(v) sum(lvl$variable == v),
    integer(1),
    USE.NAMES = FALSE
  )
  nobs <- vapply(
    headers$variable,
    function(v) as.integer(sum(lvl$n[lvl$variable == v], na.rm = TRUE)),
    integer(1),
    USE.NAMES = FALSE
  )
  # The test columns may be absent from the frame (`p_value = FALSE`
  # drops the p-value, and the rest when nothing ran the test); the
  # summary keeps its fixed schema, NA there.
  test_field <- function(nm, na) {
    if (is.null(headers[[nm]])) rep(na, nrow(headers)) else headers[[nm]]
  }
  .as_broom_tbl(data.frame(
    outcome = rep(outcome, nrow(headers)),
    variable = headers$variable,
    label = headers$label,
    n_levels = n_levels,
    test_type = test_field("test_type", NA_character_),
    statistic = test_field("statistic", NA_real_),
    df = test_field("df1", NA_real_),
    df.residual = test_field("df2", NA_real_),
    p.value = test_field("p.value", NA_real_),
    es_type = headers$es_type,
    es_value = headers$es_value,
    es_ci_lower = headers$es_ci_lower,
    es_ci_upper = headers$es_ci_upper,
    nobs = nobs,
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}
