#' Build a formatted ASCII table (internal spicy engine)
#'
#' @description
#' Low-level internal function that constructs a visually aligned ASCII table
#' from a `data.frame`.
#' It supports Unicode characters, ANSI colors, dynamic width adjustment,
#' left/right alignment, and spacing control.
#' This function is primarily used internally by higher-level wrappers such as
#' [spicy_print_table()] or [print.spicy_freq_table()].
#'
#' @details
#' `build_ascii_table()` is the rendering engine that produces the aligned text
#' layout of **spicy-formatted tables**.
#' It automatically detects cell widths (including colored text), inserts Unicode
#' separators, and applies padding for different display modes (`"compact"`,
#' `"normal"`, `"wide"`).
#'
#' For most users, this function should not be called directly. Instead, use
#' [spicy_print_table()] which adds headers, notes, and alignment logic
#' automatically.
#'
#' @param x A `data.frame` or `spicy_table` object containing the table to format.
#'   Typically, this includes columns such as *Category*, *Values*, *Freq.*, *Percent*, etc.
#' @param padding Character string controlling horizontal spacing between columns:
#'   * `"compact"` — minimal spacing
#'   * `"normal"` — moderate spacing (default)
#'   * `"wide"` — extra spacing (for large displays or wide content)
#' @param first_column_line Logical. If `TRUE` (default), a vertical separator
#'   is drawn after the first column (useful for separating categories from data).
#' @param row_total_line,column_total_line Logical. Control horizontal rules
#'   before total rows or columns (default: `TRUE`).
#' @param bottom_line Logical. If `TRUE` (default), draws a closing line at the
#'   bottom of the table.
#' @param lines_color Character. Color used for table separators (default: `"darkgrey"`).
#'   The color is applied only when ANSI color support is available
#'   (see [crayon::has_color()]).
#' @param align_left_cols Integer vector of column indices to left-align.
#'   Defaults to `c(1, 2)` for frequency tables (Category + Values).
#' @param ... Additional arguments (currently ignored).
#'
#' @return
#' A single character string containing the full ASCII-formatted table,
#' suitable for direct printing with `cat()`.
#'
#' @examples
#' # Internal usage example (for developers)
#' df <- data.frame(
#'   Category = c("Valid", "", "Missing", "Total"),
#'   Values = c("Yes", "No", "NA", ""),
#'   Freq. = c(12, 8, 1, 21),
#'   Percent = c(57.1, 38.1, 4.8, 100.0)
#' )
#'
#' cat(build_ascii_table(df, padding = "compact"))
#'
#' @seealso
#' [spicy_print_table()] for a user-facing wrapper that adds titles and notes.
#'
#' @importFrom crayon has_color make_style col_nchar
#' @importFrom stringr str_pad
#'
#' @keywords internal
#' @export

build_ascii_table <- function(x,
                              padding = c("compact", "normal", "wide"),
                              first_column_line = TRUE,
                              row_total_line = TRUE,
                              column_total_line = TRUE,
                              bottom_line = TRUE,
                              lines_color = "darkgrey",
                              align_left_cols = c(1L, 2L),
                              ...) {
  stopifnot(is.data.frame(x))
  padding <- match.arg(padding)

  df <- as.data.frame(x, check.names = FALSE)
  df[] <- lapply(df, as.character)

  # --- Compute visible column widths
  w <- vapply(seq_along(df), function(i) {
    max(crayon::col_nchar(c(df[[i]], colnames(df)[i]), type = "width"), na.rm = TRUE)
  }, integer(1))

  # --- Adjust padding
  if (padding == "normal") w <- w + 5L
  if (padding == "wide") w <- w + 9L

  # --- Helper for cell alignment
  pad_cell <- function(txt, width, left = FALSE) {
    if (left) {
      stringr::str_pad(txt, width, side = "right")
    } else {
      stringr::str_pad(txt, width, side = "left")
    }
  }

  # --- Define where to place vertical bars
  sep_after <- integer(0)
  if (isTRUE(first_column_line) && ncol(df) > 1) sep_after <- c(sep_after, 1L)
  if (isTRUE(row_total_line) && any(c("Row_Total", "Total") %in% names(df))) {
    idx <- which(names(df) %in% c("Row_Total", "Total"))[1]
    sep_after <- c(sep_after, idx - 1L)
  }
  sep_after <- sort(unique(sep_after[sep_after >= 1 & sep_after <= ncol(df)]))

  # --- Build line for header or data row
  build_line <- function(values, widths) {
    stopifnot(length(values) == length(widths))
    pieces <- character(0)
    bars <- integer(0)
    pos <- 0L
    for (i in seq_along(values)) {
      pieces <- c(pieces, " ")
      pos <- pos + 1L

      # Align Category + Values left, rest right (configurable)
      cell <- pad_cell(values[i], widths[i], left = (i %in% align_left_cols))

      pieces <- c(pieces, cell)
      pos <- pos + nchar(cell, type = "width")
      pieces <- c(pieces, " ")
      pos <- pos + 1L

      if (i %in% sep_after) {
        pieces <- c(pieces, "\u2502")
        pos <- pos + 1L
        bars <- c(bars, pos)
      }
    }
    list(text = paste0(pieces, collapse = ""), bars = bars, width = pos)
  }

  header_line <- build_line(colnames(df), w)
  data_lines <- lapply(seq_len(nrow(df)), function(i) build_line(df[i, ], w))

  full_width <- max(c(header_line$width, vapply(data_lines, `[[`, integer(1), "width")))
  normalize <- function(s) stringr::str_pad(s, full_width, side = "right")
  header_txt <- normalize(header_line$text)
  rows_txt <- vapply(data_lines, function(z) normalize(z$text), character(1))

  # --- Determine bar positions for horizontal rules
  bar_positions <- sort(unique(c(header_line$bars, unlist(lapply(data_lines, `[[`, "bars")))))
  bar_positions <- bar_positions[bar_positions >= 1 & bar_positions <= full_width]

  make_rule <- function(width, bars, junction = "\u253c") {
    chars <- rep("\u2500", width)
    if (length(bars)) chars[bars] <- junction
    paste0(chars, collapse = "")
  }

  style <- if (crayon::has_color()) crayon::make_style(lines_color) else identity
  header_rule <- style(make_rule(full_width, bar_positions, "\u253c"))
  total_rule <- style(make_rule(full_width, bar_positions, "\u253c")) # line before Total
  bottom_rule <- style(make_rule(full_width, bar_positions, "\u2534"))

  # --- Retrieve attributes
  # title <- attr(x, "title")
  # note <- attr(x, "note")

  # --- Colorize vertical bars if supported
  if (crayon::has_color()) {
    header_txt <- gsub("\u2502", style("\u2502"), header_txt, fixed = TRUE)
    rows_txt <- gsub("\u2502", style("\u2502"), rows_txt, fixed = TRUE)
  }

  out <- character(0)

  # --- Add header
  out <- c(out, header_txt, header_rule)

  # --- Add rows, with horizontal line before Total
  total_idx <- grep("\\b(Total|Column_Total)\\b", rows_txt, perl = TRUE)
  if (length(total_idx) == 1 && total_idx > 1) {
    out <- c(
      out,
      rows_txt[seq_len(total_idx - 1)],
      total_rule,
      rows_txt[total_idx:length(rows_txt)]
    )
  } else {
    if (length(rows_txt)) out <- c(out, rows_txt)
  }

  # --- Bottom rule
  if (isTRUE(bottom_line)) out <- c(out, bottom_rule)


  paste(out, collapse = "\n")
}


#' Print a spicy-formatted ASCII table
#'
#' @description
#' User-facing helper that prints a visually aligned, spicy-styled ASCII table
#' created by functions such as [freq()] or cross_table().
#' It automatically adjusts column alignment, spacing, and separators for
#' improved readability in console outputs.
#'
#' This function wraps the internal renderer [build_ascii_table()], adding
#' optional titles, notes, and automatic alignment rules depending on the type
#' of table.
#'
#' @details
#' `spicy_print_table()` detects whether the table represents frequencies
#' (`freq`-style) or cross-tabulations (`cross`-style) and adjusts formatting
#' accordingly:
#' * For **frequency tables**, the first two columns (*Category* and *Values*)
#'   are left-aligned.
#' * For **cross tables**, only the first column (row variable) is left-aligned.
#'
#' The function supports Unicode line-drawing characters and colored separators
#' using the **crayon** package, with graceful fallback to monochrome output when
#' color is not supported.
#'
#' @param x A `spicy_table` or `data.frame` to be printed.
#' @param title Optional title displayed above the table. Defaults to the
#'   `"title"` attribute of `x` if present.
#' @param note Optional note displayed below the table. Defaults to the `"note"`
#'   attribute of `x` if present.
#' @param padding Character string controlling horizontal spacing between columns:
#'   * `"compact"` — minimal spacing
#'   * `"normal"` — moderate spacing (default)
#'   * `"wide"` — extra spacing (for wide displays)
#' @param first_column_line Logical; if `TRUE` (default), adds a vertical separator
#'   after the first column.
#' @param row_total_line,column_total_line,bottom_line Logical flags controlling
#'   the presence of horizontal lines before total rows/columns or at the bottom
#'   of the table (default: `TRUE` for all).
#' @param lines_color Character; color for table separators (default: `"darkgrey"`).
#'   Only applied if the output supports ANSI colors (see [crayon::has_color()]).
#' @param align_left_cols Integer vector of column indices to left-align.
#'   If `NULL` (default), alignment is auto-detected based on `x`:
#'   * For `freq` tables → `c(1, 2)`
#'   * For `cross` tables → `1`
#' @param ... Additional arguments passed to [build_ascii_table()].
#'
#' @return
#' Invisibly returns `x`, after printing the formatted ASCII table to the console.
#'
#' @examples
#' # Simple demonstration
#' df <- data.frame(
#'   Category = c("Valid", "", "Missing", "Total"),
#'   Values = c("Yes", "No", "NA", ""),
#'   Freq. = c(12, 8, 1, 21),
#'   Percent = c(57.1, 38.1, 4.8, 100.0)
#' )
#'
#' spicy_print_table(df,
#'   title = "Frequency table: Example",
#'   note = "Class: data.frame\nData: demo"
#' )
#'
#' @seealso
#' [build_ascii_table()] for the underlying text rendering engine.
#' [print.spicy_freq_table()] for the specialized printing method used by [freq()].
#'
#' @importFrom crayon has_color make_style
#'
#' @export

spicy_print_table <- function(x,
                              title = attr(x, "title"),
                              note = attr(x, "note"),
                              padding = c("compact", "normal", "wide"),
                              first_column_line = TRUE,
                              row_total_line = TRUE,
                              column_total_line = TRUE,
                              bottom_line = TRUE,
                              lines_color = "darkgrey",
                              align_left_cols = NULL,
                              ...) {
  stopifnot(is.data.frame(x))
  padding <- match.arg(padding)

  # --- Détection automatique du type de table
  table_type <- if (any(grepl("^Category$", names(x)))) "freq" else "cross"

  # Si l’utilisateur n’a pas spécifié align_left_cols, on applique la logique par défaut
  if (is.null(align_left_cols)) {
    align_left_cols <- if (table_type == "freq") c(1L, 2L) else 1L
  }

  # --- Ajouter les métadonnées pour affichage
  if (!is.null(title)) attr(x, "title") <- title
  if (!is.null(note)) attr(x, "note") <- note

  # --- Construire la table ASCII
  txt <- build_ascii_table(
    x,
    padding = padding,
    first_column_line = first_column_line,
    row_total_line = row_total_line,
    column_total_line = column_total_line,
    bottom_line = bottom_line,
    lines_color = lines_color,
    align_left_cols = align_left_cols,
    ...
  )

  # --- Appliquer style darkgrey au titre et à la note
  style_grey <- if (crayon::has_color()) crayon::make_style("darkgrey") else identity
  if (!is.null(title)) cat(style_grey(title), "\n", sep = "")
  cat(txt, "\n", sep = "")
  if (!is.null(note)) cat(style_grey(note), "\n", sep = "")

  invisible(x)
}
