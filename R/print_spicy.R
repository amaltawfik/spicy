#' Three-Line Table
#'
#' `print.spicy()` prints any data frame as a three-line table with horizontal separators. Used in other functions of `spicy`
#'
#' @param x A data frame, matrix, array (2d) or table
#' @param ... Additional arguments
#'
#' @returns Invisibly returns `x`, after printing its formatted content.
#' @importFrom utils capture.output
#' @export
#'
#' @examples
#' \dontrun{
#' print.spicy(mtcars)
#' }

print.spicy <- function(x, ...) {
  df <- as.data.frame(x)

  if ("Values" %in% names(df)) {
    df$Values <- sprintf("%-20s", as.character(df$Values))  # Valeurs alignées à gauche
    names(df)[names(df) == "Values"] <- sprintf("%-20s", "Values")  # Nom aligné à gauche
  }

  table_lines <- utils::capture.output(print(df, row.names = FALSE))

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

