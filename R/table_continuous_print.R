#' Print method for continuous summary tables
#'
#' @description
#' Formats and prints a `spicy_continuous_table` object as a styled ASCII
#' table using [spicy_print_table()].
#'
#' @param x A `data.frame` of class `"spicy_continuous_table"` as returned
#'   by [table_continuous()].
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_continuous()], [spicy_print_table()]
#' @keywords internal
#' @export
print.spicy_continuous_table <- function(x, ...) {
  digits <- attr(x, "digits") %||% 2L
  decimal_mark <- attr(x, "decimal_mark") %||% "."
  ci_level <- attr(x, "ci_level") %||% 0.95
  group_var <- attr(x, "group_var")

  show_p <- isTRUE(attr(x, "show_p"))
  show_statistic <- isTRUE(attr(x, "show_statistic"))
  show_effect_size <- isTRUE(attr(x, "show_effect_size"))
  show_effect_size_ci <- isTRUE(attr(x, "show_effect_size_ci"))

  display_df <- build_display_df(
    x,
    digits,
    decimal_mark,
    ci_level,
    show_p = show_p,
    show_statistic = show_statistic,
    show_effect_size = show_effect_size,
    show_effect_size_ci = show_effect_size_ci
  )

  has_group <- !is.null(group_var)
  has_statistic <- "Test" %in% names(display_df)
  has_p <- "p" %in% names(display_df)
  align_left <- if (has_group) c(1L, 2L) else 1L

  # Center M, SD, Min, Max, CI LL, CI UL (and Test if present);
  # right-align n (and p if present)
  nc <- ncol(display_df)
  right_cols <- which(names(display_df) == "n")
  if (has_p) {
    right_cols <- c(right_cols, which(names(display_df) == "p"))
  }
  align_center <- setdiff(seq_len(nc), c(align_left, right_cols))

  # Compute separator rows: first row of each variable block (except first)
  sep_rows <- integer(0)
  if (has_group && "Variable" %in% names(display_df)) {
    vars <- display_df$Variable
    for (i in seq_along(vars)) {
      if (i > 1L && nzchar(vars[i])) {
        sep_rows <- c(sep_rows, i)
      }
    }
  }

  title <- "Descriptive statistics"

  # Auto-select padding: use compact when normal would overflow console.
  # Each column in build_ascii_table uses: 1 (pad) + w[i] + 1 (pad) chars,

  # plus 1 char for the vertical separator after column 1.
  # "normal" adds 5 to each w[i]; "compact" uses raw w[i].
  padding <- "normal"
  col_widths <- vapply(
    seq_along(display_df),
    function(i) {
      max(nchar(c(names(display_df)[i], as.character(display_df[[i]]))))
    },
    numeric(1)
  )
  console_w <- getOption("width", 80L)
  normal_width <- sum(col_widths + 5L + 2L) + 1L
  if (normal_width > console_w) {
    padding <- "compact"
  }

  spicy_print_table(
    display_df,
    title = title,
    note = NULL,
    padding = padding,
    first_column_line = TRUE,
    row_total_line = FALSE,
    column_total_line = FALSE,
    bottom_line = FALSE,
    align_left_cols = align_left,
    align_center_cols = align_center,
    group_sep_rows = sep_rows
  )

  invisible(x)
}
