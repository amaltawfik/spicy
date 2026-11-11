# Console rendering and coercion of `table_battery()`.

#' Print method for item battery tables
#'
#' @description
#' Formats and prints a `spicy_battery_table` object as a styled ASCII
#' table using [spicy_print_table()]. A table wider than the console is
#' printed in panels, the item labels repeated in each.
#'
#' @param x A `data.frame` of class `"spicy_battery_table"` or
#'   `"spicy_battery_svy_table"` as returned by [table_battery()] or
#'   [table_battery_svy()] with `output = "default"`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_battery()], [table_battery_svy()], [spicy_print_table()]
#' @keywords internal
#' @export
print.spicy_battery_table <- function(x, ...) {
  display_df <- attr(x, "display_df")
  s <- attr(x, "structured", exact = TRUE)
  # An object whose rendering attributes no longer describe its rows -- a
  # subset taken with `[` keeps them -- prints as the data frame it is.
  if (is.null(display_df) || is.null(s) || nrow(display_df) != nrow(x)) {
    return(invisible(NextMethod()))
  }
  .desc_items_print(x, display_df, s)
}

# The design twin prints, and coerces, as the battery it is.
#' @rdname print.spicy_battery_table
#' @export
print.spicy_battery_svy_table <- print.spicy_battery_table

# Internal: the title of a battery table, from the label of the grouping
# variable (`NULL` without `by`). Single source for the console and the
# caption every rendering engine sets.
.bat_title <- function(by_label = NULL) {
  if (is.null(by_label)) {
    spicy_str("title_battery")
  } else {
    spicy_fmt("title_battery_by", by_label)
  }
}

# ---- Coercion to plain data.frame / tibble --------------------------------

# The displayed table as a data frame: `as.data.frame()` and
# `output = "data.frame"` both call it, on the object the table prints.
# One row per item: `variable` (its column) and `label` (the label it
# displays), then the printed columns under their keys, with numbers.
.bat_data_frame <- function(x) {
  st <- as_structured(x)
  body <- st$body
  out <- data.frame(
    variable = body[[".variable"]],
    label = body[[.CAT_KEY_VARIABLE]],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  for (nm in .struct_value_cols(body)) {
    out[[nm]] <- body[[nm]]
  }
  rownames(out) <- NULL
  attr(out, "note") <- attr(x, "note", exact = TRUE)
  out
}

#' Coerce a `spicy_battery_table` to a plain data frame or tibble
#'
#' `as.data.frame()` returns the table [table_battery()] displays, with
#' its numbers: exactly what `output = "data.frame"` returns. One row per
#' item, with the columns `variable` (the item's column), `label` (the label
#' displayed), then the printed columns under their names (one per level
#' shown, `"(Missing)"`, the summary column of `top`, `"n"`, each prefixed
#' with its group under `by`, then `"p"`). The `"note"` attribute is kept.
#' `as_tibble()` returns the same table as a tibble. The original `x` is
#' unaffected, and `print(x)` continues to render the formatted table.
#'
#' @param x A `spicy_battery_table` returned by [table_battery()], or a
#'   `spicy_battery_svy_table` returned by [table_battery_svy()].
#' @param row.names,optional Standard [base::as.data.frame()] arguments,
#'   currently ignored.
#' @param ... Further arguments passed to [tibble::as_tibble()] (for the
#'   tibble method) or ignored.
#'
#' @return A plain `data.frame` (or `tbl_df`), identical to the
#'   `output = "data.frame"` of the same call.
#'
#' @name as.data.frame.spicy_battery_table
#' @keywords internal
NULL

#' @rdname as.data.frame.spicy_battery_table
#' @exportS3Method base::as.data.frame
as.data.frame.spicy_battery_table <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  .bat_data_frame(x)
}

#' @rdname as.data.frame.spicy_battery_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_battery_table <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    spicy_abort("Install package 'tibble'.", class = "spicy_missing_pkg") # nocov
  }
  tibble::as_tibble(.bat_data_frame(x), ...)
}

#' @rdname as.data.frame.spicy_battery_table
#' @exportS3Method base::as.data.frame
as.data.frame.spicy_battery_svy_table <- as.data.frame.spicy_battery_table

#' @rdname as.data.frame.spicy_battery_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_battery_svy_table <- as_tibble.spicy_battery_table
