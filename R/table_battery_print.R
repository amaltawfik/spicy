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
#' displayed), then one column per displayed column (one per level shown,
#' `"(Missing)"`, the summary column of `top`, `"n"`, then `"p"`). Under
#' `by` each name joins the group printed above the column to the header
#' printed under it (`"Female n"`). The `"note"` attribute is kept.
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

# ---- broom integration ----------------------------------------------------

#' Tidying methods for a `spicy_battery_table`
#'
#' Standard [broom::tidy()] and [broom::glance()] interfaces for an object
#' returned by [table_battery()] or [table_battery_svy()].
#'
#' `tidy()` returns one row per item, group and level: the long reading of
#' the table, the rows of `output = "long"`, with the percentage as a
#' proportion. Columns: `variable` (the item's column), `label` (the label
#' the row displays), `level` (the level, or the label of the column of
#' missing answers), `.row_role` (`"level"`, or `"missing"` on the missing
#' answers of `drop_na = FALSE`), `group` (the `by` level, `NA` without
#' `by`), `n` (the count: weighted under the `weights` of
#' [table_battery()], observed in [table_battery_svy()]), `proportion`
#' (between 0 and 1, the share of the item's base, estimated under the
#' design in [table_battery_svy()]), and the test of the item:
#' `test_type`, `statistic` and `df` (`NA` for an exact test, and in
#' [table_battery_svy()], whose long output does not carry them),
#' `p.value` (adjusted under `p_adjust`) and `p_unadjusted`. The summary
#' column of `top` has no row of its own: its proportion is the sum of
#' those of the levels it names.
#'
#' `glance()` returns one row per item: `variable`, `label`, `test_type`,
#' `statistic`, `df`, `p.value`, `p_unadjusted` and `nobs`, the base of
#' the item summed over the groups (a weighted count under `weights`, as
#' the `n` column of the table).
#'
#' Both column sets are fixed: a test the table did not run is present and
#' `NA`. Index by name rather than by position.
#'
#' @param x A `spicy_battery_table` returned by [table_battery()], or a
#'   `spicy_battery_svy_table` returned by [table_battery_svy()].
#' @param ... Ignored, for S3 compatibility.
#'
#' @return A `tbl_df` (or `data.frame` when tibble is not installed).
#'
#' @name tidy.spicy_battery_table
#' @keywords internal
NULL

# Internal: the long frame tidy() and glance() read, refused when the
# object no longer carries it.
.bat_broom_long <- function(x, fn) {
  long <- attr(x, "long_data", exact = TRUE)
  if (is.null(long)) {
    spicy_abort(
      sprintf(
        "`%s()` requires the data attached by `table_battery(output = \"default\")`.",
        fn
      ),
      class = "spicy_invalid_data",
      call = rlang::caller_env()
    )
  }
  long
}

# Internal: a column of the long frame, or a column of `na` when the table
# did not compute it.
.bat_field <- function(long, nm, na = NA_real_) {
  if (is.null(long[[nm]])) rep(na, nrow(long)) else long[[nm]]
}

# Internal: counts stay integer unless weights made them fractional.
.bat_count <- function(n) {
  if (all(is.na(n) | abs(n - round(n)) < 1e-8)) as.integer(round(n)) else n
}

#' @rdname tidy.spicy_battery_table
#' @exportS3Method broom::tidy
tidy.spicy_battery_table <- function(x, ...) {
  long <- .bat_broom_long(x, "tidy")
  .as_broom_tbl(data.frame(
    variable = long$variable,
    label = long$label,
    level = long$level,
    .row_role = long$.row_role,
    group = .bat_field(long, "group", NA_character_),
    n = .bat_count(long$n),
    proportion = long$pct / 100,
    test_type = .bat_field(long, "test_type", NA_character_),
    statistic = .bat_field(long, "statistic"),
    df = as.integer(.bat_field(long, "df")),
    p.value = .bat_field(long, "p.value"),
    p_unadjusted = .bat_field(long, "p_unadjusted"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}

#' @rdname tidy.spicy_battery_table
#' @exportS3Method broom::glance
glance.spicy_battery_table <- function(x, ...) {
  long <- .bat_broom_long(x, "glance")
  keys <- unique(long$variable)
  first <- match(keys, long$variable)
  .as_broom_tbl(data.frame(
    variable = keys,
    label = long$label[first],
    test_type = .bat_field(long, "test_type", NA_character_)[first],
    statistic = .bat_field(long, "statistic")[first],
    df = as.integer(.bat_field(long, "df")[first]),
    p.value = .bat_field(long, "p.value")[first],
    p_unadjusted = .bat_field(long, "p_unadjusted")[first],
    nobs = .bat_count(vapply(
      keys,
      function(k) sum(long$n[long$variable == k], na.rm = TRUE),
      numeric(1),
      USE.NAMES = FALSE
    )),
    stringsAsFactors = FALSE
  ))
}

#' @rdname tidy.spicy_battery_table
#' @exportS3Method broom::tidy
tidy.spicy_battery_svy_table <- tidy.spicy_battery_table

#' @rdname tidy.spicy_battery_table
#' @exportS3Method broom::glance
glance.spicy_battery_svy_table <- glance.spicy_battery_table
