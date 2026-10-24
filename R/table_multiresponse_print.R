# Console rendering of `table_multiresponse()`.

#' Print method for multiple-response tables
#'
#' @description
#' Formats and prints a `spicy_multiresponse_table` object as a styled
#' ASCII table using [spicy_print_table()].
#'
#' @param x A `data.frame` of class `"spicy_multiresponse_table"` as
#'   returned by [table_multiresponse()] with `output = "default"`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_multiresponse()], [spicy_print_table()]
#' @keywords internal
#' @export
print.spicy_multiresponse_table <- function(x, ...) {
  display_df <- attr(x, "display_df")
  s <- attr(x, "structured", exact = TRUE)
  # An object whose rendering attributes no longer describe its rows -- a
  # subset taken with `[` keeps them -- prints as the data frame it is.
  if (is.null(display_df) || is.null(s) || nrow(display_df) != nrow(x)) {
    return(invisible(NextMethod()))
  }
  align <- attr(x, "align") %||% "decimal"
  decimal_mark <- attr(x, "decimal_mark") %||% "."

  # The header a reader sees is the typed view's display label, never the
  # frozen column key (decision 13), as in the categorical print method.
  header_labels <- c(
    spicy_str("header_variable"),
    vapply(
      .struct_value_cols(s$body),
      function(nm) s$col_meta[[nm]]$display_label %||% nm,
      character(1),
      USE.NAMES = FALSE
    )
  )

  numeric_j <- setdiff(seq_along(display_df), 1L)
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

  # Compact padding when the default would overflow the console, measured
  # on what is printed (the labels, by display width).
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
  if (sum(col_widths + padding + 2L) + 1L > getOption("width", 80L)) {
    padding <- 0L
  }

  spicy_print_table(
    display_df,
    title = attr(x, "title"),
    note = attr(x, "note"),
    padding = padding,
    first_column_line = TRUE,
    row_total_line = FALSE,
    bottom_line = FALSE,
    align_left_cols = 1L,
    align_center_cols = align_center,
    # The base row is ruled off from the rows above, from its typed role.
    group_sep_rows = which(s$body$.row_role == "fit_stat"),
    # No total row: without an index the printer looks for the word
    # "Total" in the rows and rules off an item "Total score".
    total_row_idx = integer(0),
    display_labels = header_labels
  )
  invisible(x)
}

# Internal: the title of a multiple-response table, from the label of the
# grouping variable (`NULL` = one-way). Single source for the console and
# the caption every rendering engine sets.
.mr_title <- function(by_label = NULL) {
  if (is.null(by_label)) {
    spicy_str("title_multiresponse")
  } else {
    spicy_fmt("title_multiresponse_by", by_label)
  }
}

# ---- Coercion to plain data.frame / tibble --------------------------------

# The displayed table as a data frame (decision 5 of the ADR of
# 2026-10-20): `as.data.frame()` and `output = "data.frame"` both call
# it, on the object the table prints. The typed view holds one body row
# per row of the wide frame; the test cell ("chi2(1) = 0.52") keeps its
# degrees of freedom beside it, as in the continuous family.
.mr_data_frame <- function(x) {
  st <- as_structured(x)
  items <- attr(x, "items", exact = TRUE)
  is_item <- x$.row_role == "summary"
  parts <- list()
  if (.CON_KEY_TEST %in% names(st$body)) {
    df <- as.numeric(x[["df"]] %||% rep(NA_real_, nrow(x)))
    df[is.na(st$body[[.CON_KEY_TEST]])] <- NA_real_
    parts[[.CON_KEY_TEST]] <- list(df = df)
  }
  out <- .table_data_frame(
    st,
    stats::setNames(x$Variable[is_item], items),
    key = NULL,
    parts = parts,
    note = attr(x, "note", exact = TRUE)
  )
  attr(out, "global_test") <- attr(x, "global_test", exact = TRUE)
  out
}

#' Coerce a `spicy_multiresponse_table` to a plain data frame or tibble
#'
#' `as.data.frame()` returns the table [table_multiresponse()] displays,
#' with its numbers: exactly what `output = "data.frame"` returns. One row
#' per printed row -- the items, the rows of `none_row`, then the base --
#' with the key columns `variable` (the item's column, or the token of a
#' row the table adds: `"none"`, `"no_data"`, `"nobs"` for the base),
#' `label` (the label displayed) and `.row_role` (`"summary"` for an item,
#' `"none"`, `"missing"`, or `"fit_stat"` for the base), then the printed
#' columns under their names (`"<group> n"`, `"<group> %"`, ...,
#' `"Test"` with `df` beside it under `statistic = TRUE`, `"p"`). The
#' `"note"` and `"global_test"` attributes are kept. `as_tibble()`
#' returns the same table as a tibble. The original `x` is unaffected,
#' and `print(x)` continues to render the formatted table.
#'
#' @param x A `spicy_multiresponse_table` returned by
#'   [table_multiresponse()].
#' @param row.names,optional Standard [base::as.data.frame()] arguments,
#'   currently ignored.
#' @param ... Further arguments passed to [tibble::as_tibble()] (for the
#'   tibble method) or ignored.
#'
#' @return A plain `data.frame` (or `tbl_df`), identical to the
#'   `output = "data.frame"` of the same call.
#'
#' @seealso [tidy.spicy_multiresponse_table()],
#'   [glance.spicy_multiresponse_table()].
#'
#' @name as.data.frame.spicy_multiresponse_table
#' @keywords internal
NULL

#' @rdname as.data.frame.spicy_multiresponse_table
#' @exportS3Method base::as.data.frame
as.data.frame.spicy_multiresponse_table <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  .mr_data_frame(x)
}

#' @rdname as.data.frame.spicy_multiresponse_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_multiresponse_table <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    spicy_abort("Install package 'tibble'.", class = "spicy_missing_pkg") # nocov
  }
  tibble::as_tibble(.mr_data_frame(x), ...)
}

# ---- broom integration ----------------------------------------------------

#' Tidying methods for a `spicy_multiresponse_table`
#'
#' Standard [broom::tidy()] and [broom::glance()] interfaces for an
#' object returned by [table_multiresponse()].
#'
#' `tidy()` returns one row per (item x group), the margin included and
#' flagged, and the items only: the rows of `none_row` and the base row
#' are left out. Columns: `variable` (the item's column), `label` (its
#' displayed label), `group` (the `by` level or the margin, `NA` without
#' `by`), `.group_role` (the role of `group`: `"group"` or `"total"`, `NA`
#' without `by`), `total` (whether the row is the margin), `n` (the
#' respondents who chose the item), `base` (the respondents of the group's
#' base; in the margin, those of the groups with data for the item),
#' `proportion_cases` (`n / base`, the share of respondents),
#' `proportion_responses` (`n` over the choices made in the group), and
#' the item's test: `test_type` (`"chi_squared"`, `"n1_chi_squared"`,
#' `"fisher_exact"`, `"fisher_midp"` or `"fisher_exact_mc"`), `statistic`
#' (the chi-squared, Pearson's or the N-1, `NA` for an exact test), `df`,
#' `p.value` (adjusted under `p_adjust`) and `p_unadjusted`.
#'
#' `glance()` returns one row for the question. Columns: `n_items`,
#' `n_items_tested` (the items the global test kept), `n_groups` (the
#' groups of `by`, `NA` without it), `nobs` (the respondents of the base),
#' `test_type` (`"rs2"` or `"bootstrap"`, from `global_test`), `statistic`,
#' `df`, `p.value` and `boot_n` of the global test, `rs_cv` (the
#' \eqn{\hat a} of the second-order Rao-Scott correction, see *Tests* in
#' [table_multiresponse()]), and `p_adjust`, the adjustment of the item
#' p-values.
#'
#' Both column sets are FIXED: a column the table does not show (a test it
#' did not run, an adjustment it did not make) is present and `NA`. Index
#' by name rather than by position.
#'
#' @param x A `spicy_multiresponse_table` returned by
#'   [table_multiresponse()].
#' @param ... Ignored, for S3 compatibility.
#'
#' @return A `tbl_df` (or `data.frame` when tibble is not installed).
#'
#' @name tidy.spicy_multiresponse_table
#' @keywords internal
NULL

# Internal: the long frame and the item columns tidy() and glance() read,
# refused when the object no longer carries them. The rows of `none_row`
# are not items: the long frame is restricted to the items.
.mr_broom_data <- function(x, fn) {
  long <- attr(x, "long_data", exact = TRUE)
  items <- attr(x, "items", exact = TRUE)
  if (is.null(long) || is.null(items)) {
    spicy_abort(
      sprintf(
        "`%s()` requires the data attached by `table_multiresponse(output = \"default\")`.",
        fn
      ),
      class = "spicy_invalid_data",
      call = rlang::caller_env()
    )
  }
  long <- long[long$.row_role == "summary", , drop = FALSE]
  list(long = long, items = items)
}

#' @rdname tidy.spicy_multiresponse_table
#' @exportS3Method broom::tidy
tidy.spicy_multiresponse_table <- function(x, ...) {
  dat <- .mr_broom_data(x, "tidy")
  long <- dat$long
  margin <- attr(x, "margin_key", exact = TRUE)
  group <- long$group %||% rep(NA_character_, nrow(long))
  # The choices made in each group: the denominator of the percentage of
  # responses, whichever percentage the table displays. An item with no
  # data in the group has no count there (NA), and adds no choice.
  responses <- stats::ave(
    long$n,
    if (is.null(long$group)) rep(1L, nrow(long)) else long$group,
    FUN = function(v) sum(v, na.rm = TRUE)
  )
  ratio <- function(a, b) ifelse(b > 0, a / b, NA_real_)
  field <- function(nm, na = NA_real_) {
    if (is.null(long[[nm]])) rep(na, nrow(long)) else long[[nm]]
  }
  total <- !is.na(group) & group %in% margin
  .as_broom_tbl(data.frame(
    variable = long$variable,
    label = long$label,
    group = group,
    # The role of `group`: "group" or "total" (the margin), `NA` without
    # `by` (missing `by` values leave this table, so no group is missing).
    .group_role = ifelse(
      is.na(group),
      NA_character_,
      ifelse(total, "total", "group")
    ),
    total = total,
    n = as.integer(long$n),
    base = as.integer(long$base),
    # Each share named by its base, as in the long output.
    proportion_cases = ratio(long$n, long$base),
    proportion_responses = ratio(long$n, responses),
    test_type = field("test_type", NA_character_),
    statistic = field("chi2"),
    # A chi-squared on r - 1 degrees of freedom: integer by construction.
    df = as.integer(field("df")),
    p.value = field("p"),
    p_unadjusted = field("p_unadjusted"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}

#' @rdname tidy.spicy_multiresponse_table
#' @exportS3Method broom::glance
glance.spicy_multiresponse_table <- function(x, ...) {
  dat <- .mr_broom_data(x, "glance")
  long <- dat$long
  g <- attr(x, "global_test", exact = TRUE)
  has_group <- !is.null(long$group)
  s <- attr(x, "structured", exact = TRUE)
  p_meta <- s$col_meta$p
  global <- function(nm, na) if (is.null(g)) na else g[[nm]]
  # The respondents of the base: the count of the base row in the margin
  # (the only count column without `by`). Not an item's base, which leaves
  # out the groups with no data for that item.
  n_col <- names(s$col_meta)[vapply(
    s$col_meta,
    function(m) identical(m$token, "n") && (!has_group || isTRUE(m$total)),
    logical(1)
  )]
  .as_broom_tbl(data.frame(
    n_items = length(dat$items),
    n_items_tested = global("n_items", NA_integer_),
    n_groups = if (has_group) {
      length(unique(long$group)) - 1L
    } else {
      NA_integer_
    },
    nobs = as.integer(
      s$body[[n_col[[1L]]]][s$body$.row_role == "fit_stat"][[1L]]
    ),
    test_type = global("test", NA_character_),
    statistic = global("statistic", NA_real_),
    df = global("df", NA_real_),
    p.value = global("p.value", NA_real_),
    boot_n = global("boot_n", NA_integer_),
    rs_cv = global("rs_cv", NA_real_),
    p_adjust = if (is.null(p_meta)) {
      NA_character_
    } else {
      p_meta$p_adjust %||% "none"
    },
    stringsAsFactors = FALSE
  ))
}
