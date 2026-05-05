#' Copy data to the clipboard
#'
#' @description
#' Copies a `data.frame`, matrix, 2D or higher array, table, or
#' atomic vector to the system clipboard, ready to paste into a
#' text editor, spreadsheet, or word processor. Wraps
#' [clipr::write_clip()] (a Suggests dependency); requires `clipr`
#' to be installed and a clipboard backend to be available on the
#' platform.
#'
#' @details
#' Objects that are not `data.frame`s or 2D matrices (atomic
#' vectors, arrays, tables) are automatically coerced to character
#' on the way to the clipboard, as required by
#' [clipr::write_clip()]. The R-side object passed to `x` is never
#' mutated.
#'
#' Multidimensional arrays (3D and higher) are flattened to a 1D
#' character vector with one element per line. To preserve a
#' tabular layout, extract a 2D slice first, e.g.
#' \code{copy_clipboard(my_array[, , 1])}.
#'
#' @param x A `data.frame`, matrix, 2D array, 3D array, table, or
#'   atomic vector to be copied.
#' @param row.names.as.col Logical or character. If `FALSE` (the
#'   default), row names are not added as a column. If `TRUE`, a
#'   column named `"rownames"` is prepended. If a character string,
#'   it is used as the column name for the promoted row names.
#'   Ignored (with a warning) when `x` is neither a `data.frame`
#'   nor a strict matrix.
#' @param row.names Logical. If `TRUE` (the default), row names are
#'   included in the clipboard output; `FALSE` omits them.
#' @param col.names Logical. If `TRUE` (the default), column names
#'   are included in the clipboard output; `FALSE` omits them.
#' @param show_message Logical. If `TRUE` (the default), prints a
#'   success message after copying.
#' @param quiet Logical. If `FALSE` (the default), messages are
#'   shown. If `TRUE`, suppresses all messages, including the
#'   success message, coercion notices, and warnings.
#' @param ... Additional arguments passed to [clipr::write_clip()].
#'
#' @returns Invisibly returns `x`; the function is called for its
#'   clipboard side effect.
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (clipr::clipr_available()) {
#'   # Data frame
#'   copy_clipboard(sochealth)
#'
#'   # Data frame with row names as column
#'   copy_clipboard(head(sochealth), row.names.as.col = "id")
#'
#'   # Matrix
#'   mat <- matrix(1:6, nrow = 2)
#'   copy_clipboard(mat)
#'
#'   # Table
#'   tbl <- table(sochealth$education)
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
#'   copy_clipboard(sochealth, quiet = TRUE)
#' }
#' }
copy_clipboard <- function(
  x,
  row.names.as.col = FALSE,
  row.names = TRUE,
  col.names = TRUE,
  show_message = TRUE,
  quiet = FALSE,
  ...
) {
  if (!requireNamespace("clipr", quietly = TRUE)) {
    spicy_abort(
      "Package 'clipr' is required for copy_clipboard(). Please install it.",
      class = "spicy_missing_pkg"
    )
  }

  if (!clipr::clipr_available()) {
    spicy_abort(
      "Clipboard is not available on this system.",
      class = "spicy_unsupported"
    )
  }

  is_df <- is.data.frame(x)
  # Strict matrix excludes `table` because `as.data.frame.table()` reshapes
  # the table into long-form (one row per cell), which is not what the
  # `row.names.as.col` user expects -- they want a tabular layout with
  # the dimnames promoted to a column.
  is_strict_matrix <- is.matrix(x) && !inherits(x, "table")

  needs_warning <- FALSE
  warn_msg <- NULL

  if (is.logical(row.names.as.col)) {
    if (length(row.names.as.col) != 1L || is.na(row.names.as.col)) {
      spicy_abort(
        "`row.names.as.col` must be either FALSE, TRUE, or a single non-empty character string.",
        class = "spicy_invalid_input"
      )
    }
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
    if (
      length(row.names.as.col) != 1L ||
        is.na(row.names.as.col) ||
        !nzchar(row.names.as.col)
    ) {
      spicy_abort(
        "`row.names.as.col` must be either FALSE, TRUE, or a single non-empty character string.",
        class = "spicy_invalid_input"
      )
    }
    if (is_df) {
      x <- tibble::rownames_to_column(x, var = row.names.as.col)
    } else if (is_strict_matrix) {
      x <- tibble::rownames_to_column(
        as.data.frame(x),
        var = row.names.as.col
      )
    } else {
      needs_warning <- TRUE
      warn_msg <- "`row.names.as.col` is ignored because `x` is not a data frame or matrix."
    }
  } else if (!identical(row.names.as.col, FALSE)) {
    spicy_abort(
      "`row.names.as.col` must be either FALSE, TRUE, or a single non-empty character string.",
      class = "spicy_invalid_input"
    )
  }

  # Accumulate clipr's messages and warnings (rather than overwriting),
  # so a clipboard backend that emits more than one of either does not
  # silently lose the earlier ones.
  msgs_captured <- character()
  warns_captured <- character()
  withCallingHandlers(
    clipr::write_clip(
      x,
      row.names = row.names,
      col.names = col.names,
      ...
    ),
    message = function(m) {
      if (!quiet) {
        msgs_captured[[length(msgs_captured) + 1L]] <<- conditionMessage(m)
      }
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      if (!quiet) {
        warns_captured[[length(warns_captured) + 1L]] <<- conditionMessage(w)
      }
      invokeRestart("muffleWarning")
    }
  )

  if (!quiet) {
    # Lazy color: only build the styles when there is something to print.
    use_color <- crayon::has_color()
    green <- if (use_color) crayon::make_style("green") else identity
    yellow <- if (use_color) crayon::make_style("yellow") else identity

    if (isTRUE(show_message)) {
      cat(green("Data successfully copied to clipboard!"), "\n")
    }
    for (msg in msgs_captured) {
      cat(yellow(paste0("Message: ", msg)), "\n")
    }
    for (warn in warns_captured) {
      cat(yellow(paste0("Warning: ", warn)), "\n")
    }
    if (needs_warning) {
      cat(yellow(paste0("Warning: ", warn_msg)), "\n")
    }
  }

  invisible(x)
}
