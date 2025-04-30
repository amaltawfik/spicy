#' Copy data to the clipboard
#'
#' `copy_clipboard()` copies a data frame, matrix, array (2D or higher), table or vector to the clipboard.
#' You can paste the result into a text editor (e.g. Notepad++, Sublime Text), a spreadsheet (e.g. Excel, LibreOffice Calc), or a word processor (e.g. Word).
#'
#' Note: Objects that are not data frames or 2D matrices (e.g. atomic vectors, arrays, tables) are automatically converted to character
#' when copied to the clipboard, as required by [clipr::write_clip()]. The original object in R remains unchanged.
#'
#' For multidimensional arrays (e.g. 3D arrays), the entire array is flattened into a 1D character vector, with each element on a new line.
#' To preserve a tabular structure, you should extract a 2D slice before copying. For example: \code{copy_clipboard(my_array[, , 1])}.
#'
#' @param x A data frame, matrix, 2D array, 3D array, table, or atomic vector to be copied.
#' @param row.names.as.col Logical or character. If `FALSE` (default), row names are not added as a column. If `TRUE`, a column named `"rownames"` is prepended. If a character string is supplied, it is used as the column name for row names.
#' @param row.names Logical. If `TRUE` (default), includes row names in the clipboard output. If `FALSE`, row names are omitted.
#' @param col.names Logical. If `TRUE` (default), includes column names in the clipboard output. If `FALSE`, column names are omitted.
#' @param message Logical. If `TRUE` (default), displays a success message after copying. If `FALSE`, no success message is printed.
#' @param quiet Logical. If `TRUE`, suppresses all messages, including success, coercion notices, and warnings. If `FALSE` (default), messages are shown.
#' @param ... Additional arguments passed to [clipr::write_clip()].
#'
#' @returns Invisibly returns the object `x`. The main purpose is the side effect of copying data to the clipboard.
#'
#' @importFrom clipr clipr_available write_clip
#' @importFrom tibble rownames_to_column
#' @export
#'
#' @examples
#' \donttest{
#' if (clipr::clipr_available()) {
#'   # Data frame
#'   copy_clipboard(mtcars)
#'
#'   # Data frame with row names as column
#'   copy_clipboard(mtcars, row.names.as.col = "car")
#'
#'   # Matrix
#'   mat <- matrix(1:6, nrow = 2)
#'   copy_clipboard(mat)
#'
#'   # Table
#'   tbl <- table(iris$Species)
#'   copy_clipboard(tbl)
#'
#'   # Array (3D) — flattened to character
#'   arr <- array(1:8, dim = c(2, 2, 2))
#'   copy_clipboard(arr)
#'
#'   # Recommended: copy 2D slice for tabular layout
#'   copy_clipboard(arr[, , 1])
#'
#'   # Numeric vector
#'   copy_clipboard(c(3.14, 2.71, 1.618))
#'
#'   # Character vector
#'   copy_clipboard(c("apple", "banana", "cherry"))
#'
#'   # Quiet mode (no messages shown)
#'   copy_clipboard(mtcars, quiet = TRUE)
#' }
#' }



copy_clipboard <- function(
    x,
    row.names.as.col = FALSE,
    row.names = TRUE,
    col.names = TRUE,
    message = TRUE,
    quiet = FALSE,
    ...
) {
  if (!clipr::clipr_available()) {
    stop("Clipboard is not available on this system.")
  }

  is_df <- is.data.frame(x)
  is_strict_matrix <- is.matrix(x) && !inherits(x, "table")

  needs_warning <- FALSE
  warn_msg <- NULL

  if (is.logical(row.names.as.col)) {
    if (isTRUE(row.names.as.col)) {
      if (is_df) {
        x <- tibble::rownames_to_column(x, var = "rownames")
      } else if (is_strict_matrix) {
        x <- tibble::rownames_to_column(as.data.frame(x), var = "rownames")
      } else {
        needs_warning <- TRUE
        warn_msg <- "`row.names.as.col = TRUE` has no effect when `x` is not a data frame or matrix."
      }
    }
  } else if (is.character(row.names.as.col)) {
    if (is_df) {
      x <- tibble::rownames_to_column(x, var = row.names.as.col[1L])
    } else if (is_strict_matrix) {
      x <- tibble::rownames_to_column(as.data.frame(x), var = row.names.as.col[1L])
    } else {
      needs_warning <- TRUE
      warn_msg <- "`row.names.as.col` is ignored because `x` is not a data frame or matrix."
    }
  } else if (!identical(row.names.as.col, FALSE)) {
    stop("`row.names.as.col` must be either FALSE, TRUE, or a character string.")
  }

  msg_captured <- NULL
  warn_captured <- NULL
  withCallingHandlers(
    clipr::write_clip(
      x,
      row.names = row.names,
      col.names = col.names,
      ...
    ),
    message = function(m) {
      if (!quiet) msg_captured <<- conditionMessage(m)
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      if (!quiet) warn_captured <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  if (!quiet && isTRUE(message)) {
    cat("\033[32mData successfully copied to clipboard!\033[0m\n")
  }

  if (!quiet && !is.null(msg_captured)) {
    cat("\033[33mMessage: ", msg_captured, "\033[0m\n", sep = "")
  }

  if (!quiet && !is.null(warn_captured)) {
    cat("\033[33mWarning: ", warn_captured, "\033[0m\n", sep = "")
  }

  if (!quiet && needs_warning) {
    cat("\033[33m", warn_msg, "\033[0m\n", sep = "")
  }

  invisible(x)
}
