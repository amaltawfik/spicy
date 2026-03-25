#' Print method for categorical summary tables
#'
#' @description
#' Formats and prints a `spicy_categorical_table` object as a styled ASCII table using
#' [spicy_print_table()].
#'
#' @param x A `data.frame` of class `"spicy_categorical_table"` as returned by
#'   [table_categorical()] with `output = "default"` and `styled = TRUE`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_categorical()], [spicy_print_table()]
#' @export
print.spicy_categorical_table <- function(x, ...) {
  display_df <- attr(x, "display_df")
  group_var <- attr(x, "group_var")
  indent_text <- attr(x, "indent_text") %||% "  "

  if (is.null(display_df)) {
    display_df <- x
  }

  align_left <- 1L
  align_center <- integer(0)

  sep_rows <- integer(0)
  first_col <- display_df[[1]]
  for (i in seq_along(first_col)) {
    if (
      i > 1L && nzchar(first_col[i]) && !startsWith(first_col[i], indent_text)
    ) {
      sep_rows <- c(sep_rows, i)
    }
  }

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

  title <- if (is.null(group_var)) {
    "Categorical table"
  } else {
    paste0("Categorical table by ", group_var)
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
