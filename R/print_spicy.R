#' Three-Line Table
#'
#' `print.spicy()` prints any data frame as a three-line table with horizontal separators. Used in other functions of `spicy`
#'
#' @param x A data frame, matrix, array (2d) or table
#' @param ... Additional arguments
#'
#' @returns Invisibly returns `x`, after printing its formatted content.
#' @export
#'
#' @examples
#' \dontrun{
#' print.spicy(mtcars)
#' }

print.spicy <- function(x, ...) {

  table_lines <- capture.output(print(as.data.frame(x), row.names = FALSE))
  line_width <- max(nchar(table_lines), na.rm = TRUE)
  line_sep <- strrep("\u2500", line_width)

  cat(attr(x, "title"), "\n", sep = "")
  cat(line_sep, "\n", sep = "")

  cat(table_lines[1], "\n", sep = "")
  cat(line_sep, "\n", sep = "")
  for (i in 2:length(table_lines)) {
    cat(table_lines[i], "\n", sep = "")
  }
  cat(line_sep, "\n", sep = "")

  if (!is.null(attr(x, "note"))) {
    cat(attr(x, "note"), "\n", sep = "")
  }

  invisible(x)
}
