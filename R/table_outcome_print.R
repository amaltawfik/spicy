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
    decimal_mark
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
# object does not carry one (a frame that travelled through
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
# say what the table was about. Used by every coercion / broom method.
unclass_spicy_outcome_table <- function(x) {
  keep <- list(
    outcome = attr(x, "outcome", exact = TRUE),
    select = attr(x, "select", exact = TRUE)
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

#' Coerce a `spicy_outcome_table` to a plain data frame or tibble
#'
#' These S3 methods strip the `"spicy_outcome_table"` class and the
#' rendering-only attributes from an object returned by
#' [table_outcome()], so the underlying long-format data can be
#' manipulated with downstream tools under the standard `data.frame` /
#' `tbl_df` contract. The `"outcome"` and `"select"` attributes are kept as
#' lightweight provenance markers. The original `x` is unaffected, and
#' `print(x)` continues to render the formatted table.
#'
#' The returned data is identical to what `output = "long"` (or
#' `output = "data.frame"`) returns directly from [table_outcome()].
#'
#' @param x A `spicy_outcome_table` returned by [table_outcome()].
#' @param row.names,optional Standard [base::as.data.frame()]
#'   arguments, currently ignored.
#' @param ... Further arguments passed to [tibble::as_tibble()] (for
#'   the tibble method) or ignored.
#'
#' @return A plain `data.frame` (or `tbl_df`), one row per displayed
#'   row of the table.
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
  unclass_spicy_outcome_table(x)
}

#' @rdname as.data.frame.spicy_outcome_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_outcome_table <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    spicy_abort("Install package 'tibble'.", class = "spicy_missing_pkg")
  }
  tibble::as_tibble(unclass_spicy_outcome_table(x), ...)
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
#' marginal row), `estimate` (the mean), `std.error` (`sd / sqrt(n)`),
#' `conf.low`, `conf.high`, `n`, `min`, `max`, `sd`.
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
#' `smd_type`, `smd_value`, `n_total`.
#'
#' `n_levels` counts the levels the TABLE displays, so a missing-value
#' display level counts; the comparison behind `test_type` runs on the
#' observed levels only, as it does everywhere in the family.
#'
#' The schema is FIXED: `smd_type` / `smd_value` are present and `NA`
#' from the first version, so the day a standardized mean difference
#' enters this table it cannot break a pipeline that indexes the
#' frame. Index by NAME rather than by position.
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
  .outcome_as_tbl(data.frame(
    outcome = rep(outcome, nrow(rows)),
    variable = rows$variable,
    label = rows$label,
    level = rows$level,
    estimate = rows$mean,
    std.error = ifelse(
      is.na(rows$sd) | is.na(rows$n) | rows$n < 1L,
      NA_real_,
      rows$sd / sqrt(rows$n)
    ),
    conf.low = rows$ci_lower,
    conf.high = rows$ci_upper,
    n = as.integer(rows$n),
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
  n_total <- vapply(
    headers$variable,
    function(v) as.integer(sum(lvl$n[lvl$variable == v], na.rm = TRUE)),
    integer(1),
    USE.NAMES = FALSE
  )
  .outcome_as_tbl(data.frame(
    outcome = rep(outcome, nrow(headers)),
    variable = headers$variable,
    label = headers$label,
    n_levels = n_levels,
    test_type = headers$test_type,
    statistic = headers$statistic,
    df = headers$df1,
    df.residual = headers$df2,
    p.value = headers$p.value,
    es_type = headers$es_type,
    es_value = headers$es_value,
    es_ci_lower = headers$es_ci_lower,
    es_ci_upper = headers$es_ci_upper,
    # Present and NA from the first version: a fixed schema is what
    # makes adding the statistic later a non-breaking change.
    smd_type = headers$smd_type,
    smd_value = headers$smd_value,
    n_total = n_total,
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}
