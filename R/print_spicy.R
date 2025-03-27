#' Print a Formatted Data Frame with Aligned Columns
#'
#' `print.spicy()` prints a data frame with properly aligned columns, following a structured three-line table format.
#' The first column is left-aligned, while all other columns are right-aligned. Column widths are dynamically adjusted
#' based on the longest value in each column.
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
#' \dontrun{
#' print.spicy(mtcars)
#' }

print.spicy <- function(x, ...) {
  df <- as.data.frame(x)

  # Convertir toutes les colonnes en caractère
  df[] <- lapply(df, as.character)

  # Déterminer la largeur maximale de chaque colonne (y compris l'en-tête)
  col_widths <- sapply(df, function(col) max(nchar(c(col, names(df))), na.rm = TRUE))

  # Appliquer l'alignement aux valeurs
  df[] <- Map(function(col, idx) {
    col_chr <- as.character(col)  # Conversion explicite en texte
    width <- col_widths[idx]  # Largeur max pour cette colonne

    # 1ère colonne à gauche, les autres à droite
    if (idx == 1) {
      str_pad(col_chr, width, side = "right")  # Gauche
    } else {
      str_pad(col_chr, width, side = "left")   # Droite
    }
  }, df, seq_along(df))

  # Appliquer l'alignement aux noms de colonnes aussi !
  colnames(df) <- Map(function(name, idx) {
    width <- col_widths[idx]
    if (idx == 1) {
      str_pad(name, width, side = "right")  # Gauche
    } else {
      str_pad(name, width, side = "left")   # Droite
    }
  }, colnames(df), seq_along(df))

  # Capturer l'affichage formaté
  table_lines <- utils::capture.output(print(df, row.names = FALSE))
  line_width <- max(nchar(table_lines), na.rm = TRUE)
  line_sep <- strrep("\u2500", line_width)

  # Affichage final
  writeLines(c(attr(x, "title"), line_sep, table_lines[1], line_sep))
  writeLines(table_lines[-1])
  writeLines(line_sep)

  # Afficher la note si elle existe
  if (!is.null(attr(x, "note"))) {
    writeLines(attr(x, "note"))
  }

  invisible(x)
}
