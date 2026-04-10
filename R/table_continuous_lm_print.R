#' Print method for bivariate linear-model tables
#'
#' @description
#' Formats and prints a `spicy_continuous_lm_table` object as a styled
#' ASCII table using [spicy_print_table()].
#'
#' @param x A `data.frame` of class `"spicy_continuous_lm_table"` as
#'   returned by [table_continuous_lm()].
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_continuous_lm()], [spicy_print_table()]
#' @export
print.spicy_continuous_lm_table <- function(x, ...) {
  digits <- attr(x, "digits") %||% 2L
  decimal_mark <- attr(x, "decimal_mark") %||% "."
  ci_level <- attr(x, "ci_level") %||% 0.95
  by_label <- attr(x, "by_label") %||% "Predictor"
  show_statistic <- attr(x, "show_statistic") %||% FALSE
  show_p_value <- attr(x, "show_p_value") %||% TRUE
  show_n <- attr(x, "show_n") %||% TRUE
  effect_size <- attr(x, "effect_size") %||% "none"
  r2_type <- attr(x, "r2_type") %||% "r2"
  show_ci <- attr(x, "show_ci") %||% TRUE

  display_df <- build_wide_display_df_continuous_lm(
    x,
    digits = digits,
    decimal_mark = decimal_mark,
    ci_level = ci_level,
    show_statistic = show_statistic,
    show_p_value = show_p_value,
    show_n = show_n,
    effect_size = effect_size,
    r2_type = r2_type,
    ci = show_ci
  )

  align_left <- 1L
  right_cols <- which(names(display_df) %in% c("n", "p"))
  align_center <- setdiff(seq_len(ncol(display_df)), c(align_left, right_cols))

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
    title = paste0("Continuous outcomes by ", by_label),
    note = NULL,
    padding = padding,
    first_column_line = TRUE,
    row_total_line = FALSE,
    column_total_line = FALSE,
    bottom_line = FALSE,
    align_left_cols = align_left,
    align_center_cols = align_center,
    group_sep_rows = integer(0)
  )

  invisible(x)
}
