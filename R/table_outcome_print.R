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
  if (!is.null(cached)) {
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
