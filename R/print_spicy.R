#' Three-Line Table
#'
#' `print.spicy()` prints any data frame as a three-line table with horizontal separators. Used in other functions of `spicy`
#'
#' @param x A data frame, matrix, array (2d) or table
#' @param ... Additional arguments
#'
#' @returns Invisibly returns `x`, after printing its formatted content.
#' @importFrom utils capture.output
#' @importFrom stringi stri_width
#' @export
#'
#' @examples
#' \dontrun{
#' print.spicy(mtcars)
#' }

print.spicy <- function(x, ...) {
  df <- as.data.frame(x)

  if ("Values" %in% names(df)) {
    values_chr <- as.character(df$Values)

    # Largeur réelle d'affichage
    display_width <- stringi::stri_width(values_chr)
    max_width <- max(display_width, na.rm = TRUE)

    # Aligner à gauche selon largeur réelle
    pad_right <- function(s, width) {
      padding <- width - stringi::stri_width(s)
      paste0(s, strrep(" ", padding))
    }

    df$Values <- vapply(
      values_chr,
      pad_right,
      character(1),
      width = max_width
    )
  }

  table_lines <- utils::capture.output(print(df, row.names = FALSE))

  line_width <- max(nchar(table_lines), na.rm = TRUE)
  line_sep <- strrep("\u2500", line_width)

  cat(attr(x, "title"), "\n")
  cat(line_sep, "\n")
  cat(table_lines[1], "\n")
  cat(line_sep, "\n")
  for (i in 2:length(table_lines)) {
    cat(table_lines[i], "\n")
  }
  cat(line_sep, "\n")

  if (!is.null(attr(x, "note"))) {
    cat(attr(x, "note"), "\n")
  }

  invisible(x)
}
