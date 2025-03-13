#' Copy data to the clipboard
#'
#' `copy_clipboard()` copy a data frame, table, array or matrix to the clipboard. Paste to a text editor (Notepad++, Sublime Text, etc.), a spreadsheet or word processor software (Excel, LibreOffice Calc, Word, etc.).
#'
#' @param x A data frame, matrix, array (2d) or table
#' @param row.names.as.col If `FALSE` (the default), do not add the row names of x as a column. If `TRUE`, add a column "rownames". You can supply a name e.g. row.names.as.col = "variable".
#' @param col.names Logical. If `TRUE` (the default), add the column names of x. If `FALSE`, do not add the column names of x.
#' @param ... parameters passed to other methods.
#'
#' @returns A data frame to the clipboard
#' @importFrom collapse qTBL
#' @importFrom utils write.table
#' @export
#'
#' @examples
#' \dontrun{
#' copy_clipboard(x)
#' copy_clipboard(x, row.names.as.col = "var_rownames")
#' }

copy_clipboard <- function(x, row.names.as.col = FALSE, col.names = TRUE, ...) {
  if (row.names.as.col == T) {
    x1 <- collapse::qTBL(x, row.names.col = "rownames")
  }
  if (row.names.as.col == F) {
    x1 <- x
  } else {
    x1 <- collapse::qTBL(x, row.names.col = row.names.as.col[1L])
  }


  utils::write.table(x1, "clipboard-1000000", sep = "\t", row.names = FALSE, col.names = col.names, ...)
}
