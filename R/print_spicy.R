#' Print a Formatted Data Frame with Aligned Columns
#'
#' `print.spicy()` prints a data frame with properly aligned columns, following a structured three-line table format.
#' The first column is left-aligned, while all other columns are right-aligned. Column widths are dynamically adjusted
#' based on the longest value in each column, including column names.
#'
#' @param x A data frame, matrix, array (2D), or table.
#' @param ... Additional arguments (not used).
#'
#' @returns Invisibly returns `x` after displaying its formatted content.
#'
#' @importFrom utils capture.output
#' @importFrom stringr str_pad
#' @export
#'
#' @examples
#' x <- mtcars
#' class(x) <- c("spicy", class(x))
#' print(x)

print.spicy <- function(x, ...) {
  df <- as.data.frame(x)
  df[] <- lapply(df, as.character)

  col_widths <- sapply(seq_along(df), function(idx) {
    max(nchar(c(df[[idx]], colnames(df)[idx]), type = "width"), na.rm = TRUE)
  })

  df[] <- lapply(seq_along(df), function(idx) {
    col_chr <- df[[idx]]
    width <- col_widths[idx]
    if (idx == 1) {
      stringr::str_pad(col_chr, width, side = "right")
    } else {
      stringr::str_pad(col_chr, width, side = "left")
    }
  })

  colnames(df) <- sapply(seq_along(df), function(idx) {
    width <- col_widths[idx]
    if (idx == 1) {
      stringr::str_pad(colnames(df)[idx], width, side = "right")
    } else {
      stringr::str_pad(colnames(df)[idx], width, side = "left")
    }
  })

  table_lines <- utils::capture.output(print(df, row.names = FALSE))
  line_width <- max(nchar(table_lines, type = "width"), na.rm = TRUE)
  line_sep <- strrep("\u2500", line_width)

  title <- attr(x, "title")
  note <- attr(x, "note")

  if (!is.null(title)) writeLines(title)
  writeLines(line_sep)
  writeLines(table_lines[1])
  writeLines(line_sep)
  writeLines(table_lines[-1])
  writeLines(line_sep)

  if (!is.null(note)) writeLines(note)

  invisible(x)
}
