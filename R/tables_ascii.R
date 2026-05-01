# Internal: validate the `padding` argument shared by `build_ascii_table()`
# and `spicy_print_table()`. Pre-1.0 spicy switched from a string enum
# (`"compact" / "normal" / "wide"`) to a single non-negative integer
# giving the extra characters added to each column's auto-computed
# width. Passing the legacy strings raises an actionable migration
# error rather than silently mapping to an integer, so users see the
# change instead of debugging an unexplained layout shift.
.validate_padding <- function(padding) {
  if (is.character(padding)) {
    stop(
      "`padding` must be a non-negative integer (e.g. `padding = 2L`).\n",
      "The string choices `\"compact\"`, `\"normal\"` and `\"wide\"` were ",
      "removed in spicy 0.11.0; use `0L`, `2L` and `4L` respectively.",
      call. = FALSE
    )
  }
  padding <- suppressWarnings(as.integer(padding))
  if (length(padding) != 1L || is.na(padding) || padding < 0L) {
    stop("`padding` must be a non-negative integer.", call. = FALSE)
  }
  padding
}


#' Build a formatted ASCII table
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
#' separators, and applies a configurable amount of horizontal padding.
#'
#' For most users, this function should not be called directly. Instead, use
#' [spicy_print_table()] which adds headers, notes, and alignment logic
#' automatically.
#'
#' @param x A `data.frame` or `spicy_table` object containing the table to format.
#'   Typically, this includes columns such as *Category*, *Values*, *Freq.*, *Percent*, etc.
#' @param padding Non-negative integer giving the number of extra
#'   characters added to each column's auto-computed width (the
#'   maximum of the cell-content width and the header width).
#'   Defaults to `2L`, which gives a Stata- / `cli`-like compact
#'   look. Each cell additionally receives a one-space gutter on
#'   each side, so a `padding = 2L` column whose content is at
#'   most 5 characters wide occupies 9 characters in total
#'   (`1 + 5 + 2 + 1`).
#'
#'   The string choices `"compact"`, `"normal"` and `"wide"` from
#'   spicy `< 0.11.0` were removed; pass `0L`, `2L` or `4L`
#'   instead. Passing a string raises an actionable error.
#' @param first_column_line Logical. If `TRUE` (the default), a vertical separator
#'   is drawn after the first column (useful for separating categories from data).
#' @param row_total_line,column_total_line Logical. Control horizontal rules
#'   before total rows or columns. Both default to `TRUE`.
#' @param bottom_line Logical. If `FALSE` (the default), no closing line is drawn.
#'   If `TRUE`, draws a closing line at the bottom of the table.
#' @param lines_color Character. Color used for table separators. Defaults to `"darkgrey"`.
#'   The color is applied only when ANSI color support is available
#'   (see [crayon::has_color()]).
#' @param align_left_cols Integer vector of column indices to left-align.
#'   Defaults to `c(1, 2)` for frequency tables (Category + Values).
#' @param align_center_cols Integer vector of column indices to
#'   center-align. Defaults to `integer(0)` (no centered columns).
#'   Columns not in `align_left_cols` or `align_center_cols` are
#'   right-aligned.
#' @param group_sep_rows Integer vector of row indices before which a
#'   light dashed separator line is drawn. Defaults to `integer(0)`.
#' @param total_row_idx Optional integer vector of 1-based row indices
#'   identifying the totals rows; a horizontal rule is drawn just
#'   before each. When `NULL` (the default), falls back to a regex
#'   match on `"Total"` / `"Column_Total"` in the formatted row text,
#'   which can mis-fire if a user category is literally named "Total"
#'   or "Sub-Total". Cross-tabs and frequency tables built by
#'   `cross_tab()` and `freq()` set this attribute on their result so
#'   the print methods are immune to that false positive.
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
#' cat(build_ascii_table(df, padding = 0L))
#'
#' @seealso
#' [spicy_print_table()] for a user-facing wrapper that adds titles and notes.
#'
#' @importFrom crayon has_color make_style col_nchar
#' @importFrom stringr str_pad
#'
#' @keywords internal
#' @export

build_ascii_table <- function(
  x,
  padding = 2L,
  first_column_line = TRUE,
  row_total_line = TRUE,
  column_total_line = TRUE,
  bottom_line = FALSE,
  lines_color = "darkgrey",
  align_left_cols = c(1L, 2L),
  align_center_cols = integer(0),
  group_sep_rows = integer(0),
  total_row_idx = NULL,
  ...
) {
  stopifnot(is.data.frame(x))
  padding <- .validate_padding(padding)

  df <- as.data.frame(x, check.names = FALSE)
  df[] <- lapply(df, as.character)

  w <- ascii_table_widths(df, padding)

  # Helper for cell alignment
  pad_cell <- function(txt, width, align = "right") {
    side <- switch(align, left = "right", center = "both", "left")
    stringr::str_pad(txt, width, side = side)
  }

  sep_after <- ascii_table_separators(df, first_column_line, row_total_line)

  # Build line for header or data row
  build_line <- function(values, widths) {
    stopifnot(length(values) == length(widths))
    pieces <- character(0)
    bars <- integer(0)
    pos <- 0L
    for (i in seq_along(values)) {
      pieces <- c(pieces, " ")
      pos <- pos + 1L

      # Alignment: left, center, or right (default)
      col_align <- if (i %in% align_left_cols) {
        "left"
      } else if (i %in% align_center_cols) {
        "center"
      } else {
        "right"
      }
      cell <- pad_cell(values[i], widths[i], align = col_align)

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

  full_width <- max(c(
    header_line$width,
    vapply(data_lines, `[[`, integer(1), "width")
  ))
  normalize <- function(s) stringr::str_pad(s, full_width, side = "right")
  header_txt <- normalize(header_line$text)
  rows_txt <- vapply(data_lines, function(z) normalize(z$text), character(1))

  # Determine bar positions for horizontal rules
  bar_positions <- sort(unique(c(
    header_line$bars,
    unlist(lapply(data_lines, `[[`, "bars"))
  )))
  bar_positions <- bar_positions[
    bar_positions >= 1 & bar_positions <= full_width
  ]

  make_rule <- function(width, bars, junction = "\u253c") {
    chars <- rep("\u2500", width)
    if (length(bars)) {
      chars[bars] <- junction
    }
    paste0(chars, collapse = "")
  }

  style <- if (crayon::has_color()) {
    crayon::make_style(lines_color) # nocov
  } else {
    identity # nocov
  }
  header_rule <- style(make_rule(full_width, bar_positions, "\u253c"))
  total_rule <- style(make_rule(full_width, bar_positions, "\u253c")) # line before Total
  bottom_rule <- style(make_rule(full_width, bar_positions, "\u2534"))

  # Light dashed rule for group separators (U+254C = BOX DRAWINGS LIGHT DOUBLE DASH HORIZONTAL)
  make_light_rule <- function(width, bars) {
    chars <- rep("\u254c", width)
    if (length(bars)) {
      chars[bars] <- "\u253c"
    }
    paste0(chars, collapse = "")
  }
  light_rule <- style(make_light_rule(full_width, bar_positions))

  # --- Colorize vertical bars if supported
  if (crayon::has_color()) { # nocov start
    header_txt <- gsub("\u2502", style("\u2502"), header_txt, fixed = TRUE)
    rows_txt <- gsub("\u2502", style("\u2502"), rows_txt, fixed = TRUE)
  } # nocov end

  out <- character(0)

  # --- Add header
  out <- c(out, header_txt, header_rule)

  # --- Add rows, with horizontal line before Total and light separators.
  # Caller-supplied `total_row_idx` is preferred over the regex fallback,
  # which can false-positive on a user category literally named "Total".
  total_idx <- if (!is.null(total_row_idx)) {
    total_row_idx <- as.integer(total_row_idx)
    total_row_idx[total_row_idx >= 1L & total_row_idx <= length(rows_txt)]
  } else {
    grep("\\b(Total|Column_Total)\\b", rows_txt, perl = TRUE)
  }
  sep_set <- as.integer(group_sep_rows)
  sep_set <- sep_set[sep_set >= 2L & sep_set <= length(rows_txt)]

  if (length(total_idx) == 1 && total_idx > 1) {
    # Insert light separators before total rule, then total
    body_rows <- seq_len(total_idx - 1)
    for (r in body_rows) {
      if (r %in% sep_set) {
        out <- c(out, light_rule)
      }
      out <- c(out, rows_txt[r])
    }
    out <- c(out, total_rule, rows_txt[total_idx:length(rows_txt)])
  } else if (length(sep_set) > 0L && length(rows_txt) > 0L) {
    for (r in seq_along(rows_txt)) {
      if (r %in% sep_set) {
        out <- c(out, light_rule)
      }
      out <- c(out, rows_txt[r])
    }
  } else {
    if (length(rows_txt)) out <- c(out, rows_txt)
  }

  # --- Bottom rule
  if (isTRUE(bottom_line)) {
    out <- c(out, bottom_rule)
  }

  paste(out, collapse = "\n")
}

ascii_table_widths <- function(df, padding) {
  widths <- vapply(
    seq_along(df),
    function(i) {
      max(
        crayon::col_nchar(c(df[[i]], colnames(df)[i]), type = "width"),
        na.rm = TRUE
      )
    },
    numeric(1)
  )
  widths + as.integer(padding)
}

ascii_table_separators <- function(df, first_column_line, row_total_line) {
  sep_after <- integer(0)

  if (isTRUE(first_column_line) && ncol(df) > 1L) {
    sep_after <- c(sep_after, 1L)
  }

  if (isTRUE(row_total_line) && any(c("Row_Total", "Total") %in% names(df))) {
    idx <- which(names(df) %in% c("Row_Total", "Total"))[1]
    sep_after <- c(sep_after, idx - 1L)
  }

  sort(unique(sep_after[sep_after >= 1L & sep_after <= ncol(df)]))
}

ascii_table_total_width <- function(
  df,
  padding,
  first_column_line,
  row_total_line
) {
  widths <- ascii_table_widths(df, padding)
  separators <- ascii_table_separators(df, first_column_line, row_total_line)

  sum(widths + 2L) + length(separators)
}

ascii_table_panels <- function(
  df,
  console_width,
  padding,
  first_column_line,
  row_total_line,
  sticky_cols
) {
  cols <- seq_len(ncol(df))
  if (length(cols) <= 1L) {
    return(list(cols))
  }

  sticky_cols <- sort(unique(as.integer(sticky_cols)))
  sticky_cols <- sticky_cols[sticky_cols %in% cols]
  if (!length(sticky_cols)) {
    sticky_cols <- 1L # nocov
  }

  if (
    ascii_table_total_width(
      df,
      padding = padding,
      first_column_line = first_column_line,
      row_total_line = row_total_line
    ) <=
      console_width
  ) {
    return(list(cols))
  }

  remaining_cols <- setdiff(cols, sticky_cols)
  if (!length(remaining_cols)) {
    return(list(cols)) # nocov
  }

  panels <- list()
  current <- sticky_cols

  for (col in remaining_cols) {
    candidate <- sort(c(current, col))
    candidate_width <- ascii_table_total_width(
      df[, candidate, drop = FALSE],
      padding = padding,
      first_column_line = first_column_line,
      row_total_line = row_total_line
    )

    if (
      candidate_width <= console_width ||
        identical(sort(current), sort(sticky_cols))
    ) {
      current <- candidate
      next
    }

    panels[[length(panels) + 1L]] <- current
    current <- sort(c(sticky_cols, col))
  }

  panels[[length(panels) + 1L]] <- current
  panels
}


#' Print a spicy-formatted ASCII table
#'
#' @description
#' User-facing helper that prints a visually aligned, spicy-styled ASCII table
#' created by functions such as [freq()] or [cross_tab()].
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
#' If the table exceeds the console width, it is split into stacked horizontal
#' panels while repeating the left-most identifier columns.
#'
#' @param x A `spicy_table` or `data.frame` to be printed.
#' @param title Optional title displayed above the table. Defaults to the
#'   `"title"` attribute of `x` if present.
#' @param note Optional note displayed below the table. Defaults to the `"note"`
#'   attribute of `x` if present.
#' @param padding Non-negative integer giving the number of extra
#'   characters added to each column's auto-computed width
#'   (max of cell-content width and header width). Defaults to
#'   `2L`. See [build_ascii_table()] for the precise formula and
#'   the migration note from the pre-0.11.0 string enum.
#' @param first_column_line Logical. If `TRUE` (the default), adds a vertical separator
#'   after the first column.
#' @param row_total_line,column_total_line,bottom_line Logical flags controlling
#'   the presence of horizontal lines before total rows/columns or at the bottom
#'   of the table.
#'   Both `row_total_line` and `column_total_line` default to `TRUE`;
#'   `bottom_line` defaults to `FALSE`.
#' @param lines_color Character. Color for table separators. Defaults to `"darkgrey"`.
#'   Only applied if the output supports ANSI colors (see [crayon::has_color()]).
#' @param align_left_cols Integer vector of column indices to left-align.
#'   If `NULL` (the default), alignment is auto-detected based on `x`:
#'   * For `freq` tables -> `c(1, 2)`
#'   * For `cross` tables -> `1`
#' @param align_center_cols Integer vector of column indices to
#'   center-align. Defaults to `integer(0)`.
#' @param group_sep_rows Integer vector of row indices before which a
#'   light dashed separator line is drawn. Defaults to `integer(0)`.
#' @param total_row_idx Optional integer vector of 1-based row indices
#'   identifying the totals rows; defaults to the `"total_row_idx"`
#'   attribute of `x` (set by `cross_tab()`). See [build_ascii_table()].
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

spicy_print_table <- function(
  x,
  title = attr(x, "title"),
  note = attr(x, "note"),
  padding = 2L,
  first_column_line = TRUE,
  row_total_line = TRUE,
  column_total_line = TRUE,
  bottom_line = FALSE,
  lines_color = "darkgrey",
  align_left_cols = NULL,
  align_center_cols = integer(0),
  group_sep_rows = integer(0),
  total_row_idx = attr(x, "total_row_idx"),
  ...
) {
  stopifnot(is.data.frame(x))
  padding <- .validate_padding(padding)

  table_type <- if (any(grepl("^Category$", names(x)))) "freq" else "cross"

  if (is.null(align_left_cols)) {
    align_left_cols <- if (table_type == "freq") c(1L, 2L) else 1L
  }

  if (!is.null(title)) {
    attr(x, "title") <- title
  }
  if (!is.null(note)) {
    attr(x, "note") <- note
  }

  panel_cols <- ascii_table_panels(
    x,
    console_width = getOption("width", 80L),
    padding = padding,
    first_column_line = first_column_line,
    row_total_line = row_total_line,
    sticky_cols = align_left_cols
  )

  txt <- vapply(
    panel_cols,
    function(cols) {
      build_ascii_table(
        x[, cols, drop = FALSE],
        padding = padding,
        first_column_line = first_column_line,
        row_total_line = row_total_line,
        column_total_line = column_total_line,
        bottom_line = bottom_line,
        lines_color = lines_color,
        align_left_cols = which(cols %in% align_left_cols),
        align_center_cols = which(cols %in% align_center_cols),
        group_sep_rows = group_sep_rows,
        total_row_idx = total_row_idx,
        ...
      )
    },
    character(1)
  )

  style_grey <- if (crayon::has_color()) {
    crayon::make_style("darkgrey") # nocov
  } else {
    identity # nocov
  }
  if (!is.null(title)) {
    cat(style_grey(title), "\n\n", sep = "")
  }
  cat(txt[1], "\n", sep = "")
  if (length(txt) > 1L) {
    for (i in 2:length(txt)) {
      cat("\n", txt[i], "\n", sep = "")
    }
  }
  if (!is.null(note)) {
    cat("\n", style_grey(note), "\n", sep = "")
  }

  invisible(x)
}
