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
#' [clipr::write_clip()]. The caller's object is never modified in
#' place; transformations happen on a local copy (see the return
#' value).
#'
#' Multidimensional arrays (3D and higher) are flattened to a 1D
#' character vector with one element per line. To preserve a
#' tabular layout, extract a 2D slice first, e.g.
#' \code{copy_clipboard(my_array[, , 1])}.
#'
#' Messages and warnings raised by the clipboard backend are
#' re-emitted as regular R conditions, so `suppressMessages()` /
#' `suppressWarnings()` work as usual; `quiet = TRUE` silences them
#' all at once.
#'
#' @param x A `data.frame`, matrix, 2D array, 3D array, table, or
#'   atomic vector to be copied.
#' @param row_names_as_col Logical or character. If `FALSE` (the
#'   default), row names are not added as a column. If `TRUE`, a
#'   column named `"rownames"` is prepended. If a character string,
#'   it is used as the column name for the promoted row names.
#'   Ignored (with a warning) when `x` is neither a `data.frame`
#'   nor a strict matrix.
#' @param row_names Logical. If `TRUE` (the default), row names are
#'   included in the clipboard output; `FALSE` omits them.
#' @param col_names Logical. If `TRUE` (the default), column names
#'   are included in the clipboard output; `FALSE` omits them.
#' @param show_message Logical. If `TRUE` (the default), prints a
#'   success message after copying.
#' @param quiet Logical. If `FALSE` (the default), messages are
#'   shown. If `TRUE`, suppresses all messages, including the
#'   success message, coercion notices, and warnings.
#' @param ... Additional arguments passed to [clipr::write_clip()].
#'   The pre-0.13.0 dot.case argument names (`row.names.as.col`,
#'   `row.names`, `col.names`) are trapped here and raise an error
#'   naming their snake_case replacements.
#'
#' @returns Invisibly returns the object as it was sent to the
#'   clipboard: identical to `x` by default, but reflecting the
#'   `row_names_as_col` transformation when one was requested (e.g.
#'   a matrix comes back as a `data.frame` with the promoted
#'   row-name column). The function is called for its clipboard
#'   side effect.
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
#'   copy_clipboard(head(sochealth), row_names_as_col = "id")
#'
#'   # Matrix
#'   mat <- matrix(1:6, nrow = 2)
#'   copy_clipboard(mat)
#'
#'   # Table
#'   tbl <- table(sochealth$education)
#'   copy_clipboard(tbl)
#'
#'   # Array (3D) -- flattened to character
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
  row_names_as_col = FALSE,
  row_names = TRUE,
  col_names = TRUE,
  show_message = TRUE,
  quiet = FALSE,
  ...
) {
  # Trap the pre-0.13.0 dot.case argument names before anything else:
  # they no longer match a formal, so they would otherwise fall into
  # `...`, reach utils::write.table() through clipr, and die there
  # with a confusing "matched by multiple actual arguments" error.
  legacy_map <- c(
    "row.names.as.col" = "row_names_as_col",
    "row.names" = "row_names",
    "col.names" = "col_names"
  )
  legacy_used <- intersect(names(legacy_map), ...names())
  if (length(legacy_used) > 0L) {
    renames <- sprintf(
      "`%s` is now `%s`.",
      legacy_used,
      legacy_map[legacy_used]
    )
    spicy_abort(
      c(
        "copy_clipboard() arguments were renamed in spicy 0.13.0.",
        stats::setNames(renames, rep("x", length(renames))),
        "i" = "Update the call to the snake_case names."
      ),
      class = "spicy_invalid_input"
    )
  }

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
  # `row_names_as_col` user expects -- they want a tabular layout with
  # the dimnames promoted to a column.
  is_strict_matrix <- is.matrix(x) && !inherits(x, "table")

  needs_warning <- FALSE
  warn_msg <- NULL

  if (is.logical(row_names_as_col)) {
    if (length(row_names_as_col) != 1L || is.na(row_names_as_col)) {
      spicy_abort(
        "`row_names_as_col` must be either FALSE, TRUE, or a single non-empty character string.",
        class = "spicy_invalid_input"
      )
    }
    if (isTRUE(row_names_as_col)) {
      if (is_df) {
        x <- tibble::rownames_to_column(x, var = "rownames")
      } else if (is_strict_matrix) {
        x <- tibble::rownames_to_column(as.data.frame(x), var = "rownames")
      } else {
        needs_warning <- TRUE
        warn_msg <- "`row_names_as_col = TRUE` has no effect when `x` is not a data frame or matrix."
      }
    }
  } else if (is.character(row_names_as_col)) {
    if (
      length(row_names_as_col) != 1L ||
        is.na(row_names_as_col) ||
        !nzchar(row_names_as_col)
    ) {
      spicy_abort(
        "`row_names_as_col` must be either FALSE, TRUE, or a single non-empty character string.",
        class = "spicy_invalid_input"
      )
    }
    if (is_df) {
      x <- tibble::rownames_to_column(x, var = row_names_as_col)
    } else if (is_strict_matrix) {
      x <- tibble::rownames_to_column(
        as.data.frame(x),
        var = row_names_as_col
      )
    } else {
      needs_warning <- TRUE
      warn_msg <- "`row_names_as_col` is ignored because `x` is not a data frame or matrix."
    }
  } else if (!identical(row_names_as_col, FALSE)) {
    spicy_abort(
      "`row_names_as_col` must be either FALSE, TRUE, or a single non-empty character string.",
      class = "spicy_invalid_input"
    )
  }

  # Accumulate clipr's messages and warnings (rather than overwriting),
  # so a clipboard backend that emits more than one of either does not
  # silently lose the earlier ones. They are re-emitted below as real
  # R conditions (spicy_inform / spicy_warn), so suppressMessages() /
  # suppressWarnings() and condition handlers work; `quiet = TRUE`
  # drops them entirely.
  msgs_captured <- character()
  warns_captured <- character()
  withCallingHandlers(
    clipr::write_clip(
      x,
      row.names = row_names,
      col.names = col_names,
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
    if (isTRUE(show_message)) {
      # Lazy color: only build the style when the banner is printed.
      green <- if (crayon::has_color()) {
        crayon::make_style("green")
      } else {
        identity
      }
      cat(green("Data successfully copied to clipboard!"), "\n")
    }
    for (msg in msgs_captured) {
      # message() conditions carry a trailing newline; strip it so the
      # re-emitted condition does not print a blank line.
      spicy_inform(sub("\n+$", "", msg))
    }
    for (warn in warns_captured) {
      spicy_warn(warn)
    }
    if (needs_warning) {
      spicy_warn(warn_msg, class = "spicy_ignored_arg")
    }
  }

  invisible(x)
}
