# Internal: validate the `padding` argument shared by `build_ascii_table()`
# and `spicy_print_table()`. Pre-1.0 spicy switched from a string enum
# (`"compact" / "normal" / "wide"`) to a single non-negative integer
# giving the extra characters added to each column's auto-computed
# width. Passing the legacy strings raises an actionable migration
# error rather than silently mapping to an integer, so users see the
# change instead of debugging an unexplained layout shift.
.validate_padding <- function(padding) {
  if (is.character(padding)) {
    spicy_abort(
      c(
        "`padding` must be a non-negative integer (e.g. `padding = 2L`).",
        "x" = "The string choices `\"compact\"`, `\"normal\"` and `\"wide\"` were removed in spicy 0.11.0.",
        "i" = "Use `0L`, `2L` and `4L` respectively."
      ),
      class = "spicy_invalid_input"
    )
  }
  padding <- suppressWarnings(as.integer(padding))
  if (length(padding) != 1L || is.na(padding) || padding < 0L) {
    spicy_abort(
      "`padding` must be a non-negative integer.",
      class = "spicy_invalid_input"
    )
  }
  padding
}

# Internal: validate / normalize the `spanners` argument. Returns
# either NULL (no spanner row) or a named list whose values are
# unique sorted integer column indices in 1..n_cols. Each group
# must be contiguous (the renderer centers one label over one
# continuous range; gaps would require multiple spanner rows).
.validate_spanners <- function(spanners, n_cols) {
  if (is.null(spanners)) {
    return(NULL)
  }
  if (
    !is.list(spanners) ||
      is.null(names(spanners)) ||
      any(!nzchar(names(spanners)))
  ) {
    spicy_abort(
      c(
        paste0(
          "`spanners` must be a named list (label \u2192 ",
          "integer column indices)."
        ),
        "i" = "Example: `list(\"Step 1\" = 2:5, \"Step 2\" = 6:9)`."
      ),
      class = "spicy_invalid_input"
    )
  }
  out <- list()
  used <- integer(0)
  for (lbl in names(spanners)) {
    idx <- suppressWarnings(as.integer(spanners[[lbl]]))
    if (
      length(idx) == 0L || any(is.na(idx)) || any(idx < 1L) || any(idx > n_cols)
    ) {
      spicy_abort(
        sprintf("`spanners[[\"%s\"]]` must be integers in 1..%d.", lbl, n_cols),
        class = "spicy_invalid_input"
      )
    }
    idx <- sort(unique(idx))
    if (any(diff(idx) != 1L)) {
      spicy_abort(
        sprintf("`spanners[[\"%s\"]]` must be a contiguous column range.", lbl),
        class = "spicy_invalid_input"
      )
    }
    if (any(idx %in% used)) {
      spicy_abort(
        sprintf("Spanner \"%s\" overlaps with another spanner.", lbl),
        class = "spicy_invalid_input"
      )
    }
    used <- c(used, idx)
    out[[lbl]] <- idx
  }
  # Sort spanners by leftmost column for stable rendering order.
  out[order(vapply(out, min, integer(1)))]
}


#' Build a formatted ASCII table
#'
#' @description
#' Low-level rendering engine that constructs a visually aligned
#' ASCII table from a `data.frame`. Supports Unicode line-drawing
#' characters, ANSI colors via \pkg{crayon}, automatic
#' colored-text-aware width detection, configurable padding, and
#' per-column alignment.
#'
#' @details
#' Most users should not call this directly: it is wrapped by
#' [spicy_print_table()] and the internal `print.spicy_*` methods,
#' which add titles, notes, and table-type-aware alignment defaults.
#' Reach for `build_ascii_table()` only when you need to render an
#' arbitrary `data.frame` to a string with the same look as
#' spicy's tables.
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
#' @param row_total_line Logical. Controls the horizontal rule drawn
#'   before a total row. Defaults to `TRUE`.
#' @param bottom_line Logical. If `FALSE` (the default), no closing line is drawn.
#'   If `TRUE`, draws a closing line at the bottom of the table.
#' @param lines_color Character. Color used for table separators. Defaults to `"darkgrey"`.
#'   The color is applied only when ANSI color support is available
#'   (see [crayon::has_color()]).
#' @param align_left_cols Integer vector of column indices to
#'   left-align. Defaults to `c(1L, 2L)` (the layout used by
#'   `freq()`-style tables); pass an explicit vector for other
#'   layouts.
#' @param align_center_cols Integer vector of column indices to
#'   center-align. Defaults to `integer(0)` (no centered columns).
#'   Columns not in `align_left_cols` or `align_center_cols` are
#'   right-aligned.
#' @param center_headers Logical. When `TRUE`, column headers are
#'   centered above their column content even when the data itself
#'   is right-aligned (the publication convention for
#'   coefficient / summary tables; matches Stata `regress` and
#'   SPSS `REGRESSION` output). Left-aligned columns (per
#'   `align_left_cols`) keep their header on the left. Defaults to
#'   `FALSE` for backward compatibility; the
#'   `print.spicy_regression_table` method enables it.
#' @param spanners Optional named list defining a *column group row*
#'   drawn above the column headers (the "spanner" / "supra-header"
#'   convention; APA Manual 7 §7.13).
#'   Names are spanner labels; values are integer vectors of 1-based
#'   column indices the label spans (must be contiguous). A thin
#'   underline rule is drawn below each spanner across its span.
#'   Used by `print.spicy_regression_table()` to display the model
#'   name above each model's block of sub-columns. Defaults to
#'   `NULL` (no spanner row).
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
#' @param display_labels Optional character vector of length `ncol(x)`
#'   used to override `colnames(x)` for the rendered header row only.
#'   The data.frame's actual names are kept for indexing; only the
#'   visual header is swapped. Used by `print.spicy_regression_table()`
#'   so a `B + AME` table shows the bare label (`95% CI`, `p`) in both
#'   blocks rather than R's deduplicated `95% CI.2` / `p.2`. Defaults
#'   to `NULL` (use `colnames(x)` verbatim).
#' @param body_spanners Optional list of column group rows inside the
#'   body, each a list with `row` (the row index) and `spans` (a named
#'   list like `spanners`). The row is drawn as the spanner row is: each
#'   label centered over its columns, a thin rule under them. Its cells
#'   should be blank. Used by `print.spicy_stack_table()` for the column
#'   groups of a panel's own header. Defaults to `NULL`.
#' @param whole_spanners Logical. When `TRUE`, the columns of each
#'   spanner widen until its label fits over them, instead of the label
#'   being truncated. Defaults to `FALSE`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return
#' A single character string containing the full ASCII-formatted table,
#' suitable for direct printing with `cat()`.
#'
#' @seealso
#' [spicy_print_table()] for a user-facing wrapper that adds titles and notes.
#'
#' @keywords internal

build_ascii_table <- function(
  x,
  padding = 2L,
  first_column_line = TRUE,
  row_total_line = TRUE,
  bottom_line = FALSE,
  lines_color = "darkgrey",
  align_left_cols = c(1L, 2L),
  align_center_cols = integer(0),
  center_headers = FALSE,
  spanners = NULL,
  group_sep_rows = integer(0),
  total_row_idx = NULL,
  display_labels = NULL,
  body_spanners = NULL,
  whole_spanners = FALSE,
  ...
) {
  if (!is.data.frame(x)) {
    spicy_abort(
      "`x` must be a data.frame.",
      class = "spicy_invalid_data"
    )
  }
  padding <- .validate_padding(padding)
  spanners <- .validate_spanners(spanners, ncol(x))

  # The cells handed here, whatever the class. The `as.data.frame()`
  # method of a table of the family returns the table with its numbers
  # (`output = "data.frame"`), a frame of another shape than the one the
  # caller measured.
  df <- if (inherits(x, "spicy_table")) {
    structure(x, class = "data.frame")
  } else {
    as.data.frame(x, check.names = FALSE)
  }
  # A missing cell is an EMPTY cell. `stringr::str_pad(NA, w)` returns NA
  # rather than a padded blank, and `build_line()` then adds NA to its
  # running position, so a single missing value used to leave the row
  # unpadded AND put every separator bar of the table at NA -- a header
  # rule with two crossings, columns out of register. Normalized once,
  # here, for every family that reaches the ASCII renderer.
  df[] <- lapply(df, function(z) {
    z <- as.character(z)
    z[is.na(z)] <- ""
    z
  })

  # `display_labels`, when supplied, override `colnames(df)` for the
  # printed header text. The data.frame's actual colnames are still
  # used by the caller for indexing (`x[, cols]`); only the visual
  # header that the reader sees in the console is swapped. Lets
  # `print.spicy_regression_table` and `clipboard_payload` show the
  # bare label ("95% CI", "p") in both blocks of a B + AME table
  # rather than the data-layer's deduplicated `"95% CI.2"` / `"p.2"`.
  if (!is.null(display_labels)) {
    if (length(display_labels) != ncol(df)) {
      spicy_abort(
        sprintf(
          "`display_labels` must have one label per column of `x` (%d), not %d.",
          ncol(df),
          length(display_labels)
        ),
        class = "spicy_invalid_input"
      )
    }
    names(df) <- as.character(display_labels)
  }
  # The header text is padded by the same `str_pad()` as the cells and
  # breaks the same way, so it is normalized at the same door -- AFTER
  # `display_labels`, which is the second route in. A missing column
  # name (`names(x)` carrying NA, or an NA in `display_labels`) used to
  # reach `build_line()` and desync every rule of every panel, exactly
  # as a missing cell did. Audited alongside: the other cell routes are
  # already clean. The typed view's `display_cells` overrides and the
  # engine fill loops go through the families' own formatters, which
  # emit "" or the undefined-cell glyph, never NA -- measured on the
  # regression string body, the padded body, and the descriptive
  # display frames.
  names(df)[is.na(names(df))] <- ""

  w <- ascii_table_widths(df, padding)

  # Per-column data content width (max nchar across DATA cells only,
  # excluding the header). Used by the header-centering branch below
  # to center the header above the visual data region rather than the
  # padded cell width -- otherwise the header floats left of the
  # column's visual center because the data is right-aligned within
  # extra padding.
  data_widths <- vapply(
    seq_along(df),
    function(i) {
      if (nrow(df) == 0L) {
        return(0L)
      }
      max(crayon::col_nchar(df[[i]], type = "width"), na.rm = TRUE)
    },
    integer(1)
  )

  # Helper for cell alignment
  pad_cell <- function(txt, width, align = "right") {
    side <- switch(align, left = "right", center = "both", "left")
    stringr::str_pad(txt, width, side = side)
  }

  sep_after <- ascii_table_separators(df, first_column_line, row_total_line)
  if (isTRUE(whole_spanners)) {
    w <- ascii_spanner_widths(w, spanners, sep_after)
  }

  # Build line for header or data row. `is_header = TRUE` activates
  # `center_headers`: a column that would otherwise right-align its
  # data renders its header CENTERED ABOVE THE DATA REGION (not the
  # padded cell). For a decimal-aligned numeric column whose data
  # occupies the rightmost `data_widths[i]` chars of a `widths[i]`-
  # wide cell, the header is first centered in a `data_widths[i]`-
  # wide field, then right-padded with whitespace so it sits over
  # the data range, leaving the same left-padding that the data has.
  # Left-aligned columns (typically Variable / Category / Values)
  # keep their left header so the label hugs the column's start edge.
  build_line <- function(values, widths, is_header = FALSE) {
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
      } else if (isTRUE(is_header) && isTRUE(center_headers)) {
        "header-center"
      } else {
        "right"
      }
      cell <- if (
        identical(col_align, "header-center") &&
          data_widths[i] > 0L &&
          data_widths[i] <= widths[i]
      ) {
        # Center the header within the data region, then right-pad
        # to the full cell width so the data's left margin is
        # preserved. Visual result: header sits above the data's
        # geometric center, not the padded cell's center.
        centered_in_data <- pad_cell(
          values[i],
          data_widths[i],
          align = "center"
        )
        pad_cell(centered_in_data, widths[i], align = "right")
      } else {
        pad_cell(values[i], widths[i], align = col_align)
      }

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

  header_line <- build_line(colnames(df), w, is_header = TRUE)
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

  # --- Column groups (the spanner row above the header, and the rows of
  # column groups inside the body: `body_spanners`) ----------------------
  # Each label is centered across the cell-content range of the columns
  # it spans (start of leftmost cell -> end of rightmost cell, including
  # the inner gutters and any separators between them). A thin underline
  # rule (U+2500) is drawn just below it, covering the same range only, so
  # empty (un-spanned) columns get neither label nor underline. The
  # spanner row itself contains no vertical bars; a row of the body keeps
  # those of its line (`base`).
  # 1-based char-position ranges of each column's cell (the widths[i]-wide
  # region, excluding its left/right gutters and any separator).
  col_starts <- integer(length(w))
  col_ends <- integer(length(w))
  pos <- 0L
  for (i in seq_along(w)) {
    pos <- pos + 1L # left gutter
    col_starts[i] <- pos + 1L # next char is the first cell char
    pos <- pos + w[i] # cell content
    col_ends[i] <- pos
    pos <- pos + 1L # right gutter
    if (i %in% sep_after) pos <- pos + 1L
  }
  span_lines <- function(base, spans) {
    spanner_chars <- strsplit(base, "", fixed = TRUE)[[1]]
    underline_chars <- spanner_chars
    for (lbl in names(spans)) {
      cols <- spans[[lbl]]
      span_start <- col_starts[min(cols)]
      span_end <- col_ends[max(cols)]
      span_width <- span_end - span_start + 1L
      if (span_width <= 0L) {
        next
      }
      lab_disp <- if (nchar(lbl, type = "width") > span_width) {
        # Truncation must be VISIBLE: a silently cut label ("Inactive
        # vs Employ" for "Inactive vs Employed") reads as a complete
        # -- and wrong -- label. Trade one more character for an
        # ellipsis whenever there is room for it.
        if (span_width >= 2L) {
          paste0(
            ascii_trunc_width(lbl, span_width - 1L),
            spicy_str("marker_truncation_ellipsis")
          )
        } else {
          ascii_trunc_width(lbl, span_width)
        }
      } else {
        lbl
      }
      n_lab <- nchar(lab_disp, type = "width")
      left_pad <- (span_width - n_lab) %/% 2L
      lab_pos <- span_start + left_pad
      # One slot per console column: a wide character fills two (the
      # second left empty), a combining mark joins the character before
      # it. One slot per character put every label after a wide or an
      # accented one off by the difference.
      p <- lab_pos
      for (ch in strsplit(lab_disp, "", fixed = TRUE)[[1]]) {
        cw <- nchar(ch, type = "width")
        if (cw == 0L && p > lab_pos) {
          spanner_chars[p - 1L] <- paste0(spanner_chars[p - 1L], ch)
          next
        }
        spanner_chars[p] <- ch
        if (cw > 1L) {
          spanner_chars[p + seq_len(cw - 1L)] <- ""
        }
        p <- p + max(cw, 1L)
      }
      for (p in span_start:span_end) {
        underline_chars[p] <- "\u2500"
      }
    }
    c(
      paste(spanner_chars, collapse = ""),
      paste(underline_chars, collapse = "")
    )
  }
  for (b in body_spanners) {
    if (length(b$spans) && b$row >= 1L && b$row <= length(rows_txt)) {
      rows_txt[b$row] <- paste(
        span_lines(rows_txt[b$row], b$spans),
        collapse = "\n"
      )
    }
  }

  # --- Colorize vertical bars if supported
  if (crayon::has_color()) {
    # nocov start
    header_txt <- gsub("\u2502", style("\u2502"), header_txt, fixed = TRUE)
    rows_txt <- gsub("\u2502", style("\u2502"), rows_txt, fixed = TRUE)
    rows_txt <- gsub("\u2500", style("\u2500"), rows_txt, fixed = TRUE)
  } # nocov end

  out <- character(0)

  if (!is.null(spanners) && length(spanners)) {
    lines <- span_lines(strrep(" ", full_width), spanners)
    out <- c(out, lines[[1L]], style(lines[[2L]]))
  }
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
  } else if (length(total_idx) > 1L) {
    # Several totals rows, one per block (`table_categorical(total_row =
    # TRUE)`): the rule above each, the light separators between blocks.
    for (r in seq_along(rows_txt)) {
      if (r %in% total_idx && r > 1L) {
        out <- c(out, total_rule)
      } else if (r %in% sep_set) {
        out <- c(out, light_rule)
      }
      out <- c(out, rows_txt[r])
    }
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

# The column widths with each spanner label fitting over its columns
# (`whole_spanners`): a group narrower than its label shares the missing
# width out over its columns, the first ones taking the remainder. A span
# runs across the gutters between its columns and any vertical bar
# (`sep_after`).
ascii_spanner_widths <- function(widths, spanners, sep_after = integer(0)) {
  for (lbl in names(spanners)) {
    cols <- spanners[[lbl]]
    k <- length(cols)
    span <- sum(widths[cols]) + 2L * (k - 1L) + sum(cols[-k] %in% sep_after)
    short <- nchar(lbl, type = "width") - span
    if (short > 0L) {
      widths[cols] <- widths[cols] + short %/% k + (seq_len(k) <= short %% k)
    }
  }
  widths
}

# The leading characters of `x` that fill at most `width` console columns:
# a wide character takes two, a combining mark none.
ascii_trunc_width <- function(x, width) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  paste(chars[cumsum(nchar(chars, type = "width")) <= width], collapse = "")
}

# Column groups (label -> columns of the table) remapped to the indices of
# the panel of columns `cols`. When a group's columns are split across
# panels, each panel displays its label over the surviving subset
# (matches modelsummary panel-split behavior: the model name reappears at
# the top of every panel where any of its columns are visible). NULL when
# no group is left.
ascii_panel_spans <- function(spans, cols) {
  out <- list()
  for (lbl in names(spans)) {
    local_idx <- sort(match(intersect(spans[[lbl]], cols), cols))
    # Drop discontiguous slivers (e.g. sticky col 1 between data cols
    # of the same model would break contiguity -- that doesn't arise
    # in practice because the Variable column is never in a spanner).
    if (length(local_idx) && all(diff(local_idx) == 1L)) {
      out[[lbl]] <- local_idx
    }
  }
  if (length(out)) out
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
  row_total_line,
  spanners = NULL
) {
  separators <- ascii_table_separators(df, first_column_line, row_total_line)
  widths <- ascii_spanner_widths(
    ascii_table_widths(df, padding),
    spanners,
    separators
  )

  sum(widths + 2L) + length(separators)
}

# `groups` (label -> columns, the spanners of `whole_spanners`) are laid
# out as units: a panel takes a group whole, and the group's columns widen
# to its label, unless the group alone is wider than the console, when its
# columns split as single ones do. NULL lays out every column on its own.
ascii_table_panels <- function(
  df,
  console_width,
  padding,
  first_column_line,
  row_total_line,
  sticky_cols,
  groups = NULL
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

  width_of <- function(candidate) {
    sub <- df[, candidate, drop = FALSE]
    # `[` makes repeated names unique ("n", "%", "n.1", "%.1"): the
    # columns of groups, whose headers repeat by design, are measured
    # under the headers they print.
    if (length(groups)) {
      names(sub) <- names(df)[candidate]
    }
    ascii_table_total_width(
      sub,
      padding = padding,
      first_column_line = first_column_line,
      row_total_line = row_total_line,
      spanners = ascii_panel_spans(groups, candidate)
    )
  }

  if (
    ascii_table_total_width(
      df,
      padding = padding,
      first_column_line = first_column_line,
      row_total_line = row_total_line,
      spanners = groups
    ) <=
      console_width
  ) {
    return(list(cols))
  }

  remaining_cols <- setdiff(cols, sticky_cols)
  if (!length(remaining_cols)) {
    return(list(cols)) # nocov
  }

  units <- list()
  for (col in remaining_cols) {
    group <- Filter(function(g) col %in% g, groups)
    unit <- if (length(group)) intersect(group[[1L]], remaining_cols) else col
    if (col != unit[[1L]]) {
      next
    }
    if (length(unit) > 1L && width_of(c(sticky_cols, unit)) > console_width) {
      units <- c(units, as.list(unit))
    } else {
      units[[length(units) + 1L]] <- unit
    }
  }

  panels <- list()
  current <- sticky_cols

  for (unit in units) {
    candidate <- sort(c(current, unit))

    if (
      width_of(candidate) <= console_width ||
        identical(sort(current), sort(sticky_cols))
    ) {
      current <- candidate
      next
    }

    panels[[length(panels) + 1L]] <- current
    current <- sort(c(sticky_cols, unit))
  }

  panels[[length(panels) + 1L]] <- current
  panels
}


#' Print a spicy-formatted ASCII table
#'
#' @description
#' Prints a spicy-styled ASCII table to the
#' console with optional title and note, table-type-aware alignment
#' defaults, and automatic horizontal paneling when the table is
#' wider than the console. Wraps the internal renderer
#' [build_ascii_table()].
#'
#' Part of the internal API (see the API-stability tiers in [spicy]):
#' it stays exported for the print methods of the package, and its
#' arguments can change without notice.
#'
#' @details
#' Table type is auto-detected from `x` and drives the default
#' alignment when `align_left_cols = NULL`:
#' * **frequency table** (a `Category` column is present): the
#'   first two columns (`Category`, `Values`) are left-aligned.
#' * **cross table** (otherwise): only the first column (row
#'   variable) is left-aligned.
#'
#' If the table is wider than the console, it is split into stacked
#' horizontal panels with the left-most identifier columns repeated
#' on each panel. Unicode line-drawing characters are used by
#' default; colored separators are drawn when the terminal supports
#' ANSI color ([crayon::has_color()]) and fall back to monochrome
#' otherwise.
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
#' @details
#' The layout arguments `spanners`, `display_labels`,
#' `fit_stats_start`, `total_row_idx`, `group_sep_rows`,
#' `body_spanners`, `blank_row_groups` and `whole_spanners` are
#' plumbing consumed by spicy's own print methods; they are
#' documented for completeness and are rarely useful when calling
#' this function directly.
#'
#' @param row_total_line,bottom_line Logical flags controlling the
#'   horizontal line before a total row and the closing line at the
#'   bottom of the table. `row_total_line` defaults to `TRUE`;
#'   `bottom_line` defaults to `FALSE`.
#' @param lines_color Character. Color for table separators. Defaults to `"darkgrey"`.
#'   Only applied if the output supports ANSI colors (see [crayon::has_color()]).
#' @param align_left_cols Integer vector of column indices to left-align.
#'   If `NULL` (the default), alignment is auto-detected based on `x`:
#'   * For `freq` tables -> `c(1, 2)`
#'   * For `cross` tables -> `1`
#' @param align_center_cols Integer vector of column indices to
#'   center-align. Defaults to `integer(0)`.
#' @param center_headers Logical. When `TRUE`, column headers are
#'   centered above their column content even when the data itself
#'   is right-aligned. Passed through to [build_ascii_table()].
#'   Defaults to `FALSE`.
#' @param spanners Optional named list of column-group labels
#'   (label -> integer column indices). Passed through to
#'   [build_ascii_table()];
#'   when the table is split into horizontal panels each panel keeps
#'   only the spanners whose columns are fully contained in it.
#'   Defaults to `NULL` (no spanner row).
#' @param group_sep_rows Integer vector of row indices before which a
#'   light dashed separator line is drawn. Defaults to `integer(0)`.
#' @param total_row_idx Optional integer vector of 1-based row indices
#'   identifying the totals rows; defaults to the `"total_row_idx"`
#'   attribute of `x` (set by `cross_tab()`). See [build_ascii_table()].
#' @param display_labels Optional character vector of length `ncol(x)`
#'   used to override `colnames(x)` for the printed header text only.
#'   Sliced per panel when the table is split across stacked panels.
#'   Forwarded to [build_ascii_table()]; see that function for full
#'   semantics. Defaults to `NULL`.
#' @param fit_stats_start Optional 1-based index of the first
#'   model-level statistics row (the block below the dashed rule in
#'   regression tables). When the table splits into stacked panels,
#'   continuation panels drop the rows of that block whose every
#'   visible data cell is blank -- model-level statistics print once,
#'   under the panel that carries their values, instead of leaving
#'   empty `n` / `AIC` stub rows on every continuation panel. `NULL`
#'   (default) keeps all rows on all panels.
#' @param qualify_companions Logical. When the table splits into stacked
#'   panels, should a companion column (`SE`, `p`, an interval) that a
#'   width split separated from its carrier name that carrier in its
#'   header -- `95% CI` becoming `95% CI (B)`? Set it `TRUE` only for a
#'   layout where those headers really are companions of the estimate
#'   column on their left, as in a coefficient table. In a layout whose
#'   `p` is the omnibus test of a whole block it has no carrier, and
#'   qualifying it would attribute it to whichever column happened to
#'   sit before it. Defaults to `FALSE`.
#' @param body_spanners Optional list of column group rows inside the
#'   body (see [build_ascii_table()]), in the columns of `x`. When the
#'   table splits into stacked panels, each panel repeats a label over
#'   the columns of its group it holds. Defaults to `NULL`.
#' @param blank_row_groups Optional list of integer vectors, each a set
#'   of rows that go together (the rows of a panel of a stacked table,
#'   or one of its fit-statistic rows). When the table splits into
#'   stacked panels, continuation panels leave out every group none of
#'   whose rows has a data cell there. An alternative to
#'   `fit_stats_start`, not to be combined with it. Defaults to `NULL`.
#' @param whole_spanners Logical. When `TRUE`, each group of `spanners`
#'   is laid out as a unit: its columns widen until its label fits over
#'   them, and a split into panels keeps them together, unless the group
#'   alone is wider than the console. Used by the descriptive tables,
#'   whose spanners are the groups of `by`. When `FALSE` (the default), a
#'   label wider than its columns is truncated and a split may fall inside
#'   a group. `"fit"` keeps the split of `FALSE` and widens the columns of
#'   the groups of a panel to their labels when the panel, so widened,
#'   still fits the console; otherwise its labels are truncated as under
#'   `FALSE`. Used by [table_regression()], whose spanners are the models.
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
#' @keywords internal
#' @export

spicy_print_table <- function(
  x,
  title = attr(x, "title"),
  note = attr(x, "note"),
  padding = 2L,
  first_column_line = TRUE,
  row_total_line = TRUE,
  bottom_line = FALSE,
  lines_color = "darkgrey",
  align_left_cols = NULL,
  align_center_cols = integer(0),
  center_headers = FALSE,
  spanners = NULL,
  group_sep_rows = integer(0),
  total_row_idx = attr(x, "total_row_idx"),
  display_labels = NULL,
  fit_stats_start = NULL,
  qualify_companions = FALSE,
  body_spanners = NULL,
  blank_row_groups = NULL,
  whole_spanners = FALSE,
  ...
) {
  if (!is.data.frame(x)) {
    spicy_abort(
      "`x` must be a data.frame.",
      class = "spicy_invalid_data"
    )
  }
  padding <- .validate_padding(padding)
  spanners <- .validate_spanners(spanners, ncol(x))

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

  # Apply `display_labels` to x BEFORE the panel-split decision so
  # `ascii_table_panels()` measures column widths against the SHORT
  # printed labels (e.g. "B", "p"), not the deduplicated, model-
  # prefixed dataframe names (e.g. "Model 1: B", "Model 2: p"). Without
  # this, a multi-model table over-splits — the panel logic budgets
  # for "Model 2: SE"-wide columns even though the printed header only
  # shows "SE". Downstream `x[, cols]` slicing uses integer indices
  # (not name lookup), so the resulting duplicate names ("p", "p") are
  # harmless here.
  if (!is.null(display_labels)) {
    if (length(display_labels) != ncol(x)) {
      spicy_abort(
        sprintf(
          "`display_labels` must have one label per column of `x` (%d), not %d.",
          ncol(x),
          length(display_labels)
        ),
        class = "spicy_invalid_input"
      )
    }
    names(x) <- as.character(display_labels)
  }

  panel_cols <- ascii_table_panels(
    x,
    console_width = getOption("width", 80L),
    padding = padding,
    first_column_line = first_column_line,
    row_total_line = row_total_line,
    sticky_cols = align_left_cols,
    groups = if (isTRUE(whole_spanners)) spanners
  )

  txt <- vapply(
    seq_along(panel_cols),
    function(panel_i) {
      cols <- panel_cols[[panel_i]]
      panel_spanners <- ascii_panel_spans(spanners, cols)
      panel_body_spanners <- lapply(body_spanners, function(b) {
        list(row = b$row, spans = ascii_panel_spans(b$spans, cols))
      })
      # `x[, cols]` uniquifies duplicate names ("B"/"B" -> "B"/"B.1");
      # restore the originals so the header row prints the unprefixed
      # names that the print method handed us.
      sub <- x[, cols, drop = FALSE]
      names(sub) <- names(x)[cols]
      # Continuation panels: a companion column (SE / p / a CI label)
      # separated from its carrier by the width split used to repeat
      # only its generic header ("95% CI") -- silently ambiguous when
      # another CI column sits in the main panel (wave-2 review;
      # dev/registre_rendu_estimands_spec.md). Suffix the carrier's
      # name so the orphan column names its estimand. Companions whose
      # carrier made it into the same panel keep their short header.
      # Continuation panels: model-level statistics (n / AIC / R2 ...)
      # live once, under the columns that carry their values -- for a
      # single-model table split by width, that is panel 1 only. Drop
      # the fit-stat rows whose every visible data cell is blank here,
      # so continuation panels do not end with empty stub rows. The
      # block sits at the TAIL of the table, so dropping keeps every
      # other row index (group_sep_rows, spanners) stable; the dashed
      # boundary rule is removed with the block only when the whole
      # block goes.
      panel_group_sep <- group_sep_rows
      if (
        panel_i > 1L &&
          !is.null(fit_stats_start) &&
          fit_stats_start >= 1L &&
          fit_stats_start <= nrow(sub)
      ) {
        fit_rows <- fit_stats_start:nrow(sub)
        data_cols_local <- which(!(cols %in% align_left_cols))
        if (length(data_cols_local)) {
          blank <- vapply(
            fit_rows,
            function(r) {
              all(
                !nzchar(trimws(as.character(
                  unlist(sub[r, data_cols_local], use.names = FALSE)
                )))
              )
            },
            logical(1)
          )
          if (any(blank)) {
            sub <- sub[-fit_rows[blank], , drop = FALSE]
            if (all(blank)) {
              # group_sep_rows semantics: rule drawn BEFORE the given
              # row index, so the fit boundary is fit_stats_start
              # itself.
              panel_group_sep <-
                setdiff(panel_group_sep, fit_stats_start)
            }
          }
        }
      }
      # Slice display_labels to the same panel columns so each panel
      # gets its own header text.
      panel_display_labels <- if (!is.null(display_labels)) {
        display_labels[cols]
      } else {
        NULL
      }
      # Continuation panels: a companion column (SE / p / a CI label)
      # separated from its carrier by the width split used to repeat
      # only its generic header ("95% CI") -- silently ambiguous when
      # another CI column sits in the main panel (wave-2 review;
      # dev/registre_rendu_estimands_spec.md). Suffix the carrier's
      # display name so the orphan column names its estimand; a
      # companion whose carrier made it into the same panel keeps its
      # short header. Duplicate columns arrive uniquified ("95% CI.2"),
      # so matching goes through the base name; the display label (what
      # build_ascii_table actually prints) is edited when present.
      #
      # Opt-in, because "the column on my left" is only the carrier in a
      # coefficient layout. In a descriptive table the `p` is the
      # omnibus test of the whole block and belongs to no column at all,
      # and the search silently attributed it to whichever neighbor the
      # split had left behind -- "p (Total %)", "p (Test)",
      # "p (Total DEff)". A statistic of a block named after a column is
      # a false claim about what was tested, so those families leave
      # this off (register n. 89).
      if (panel_i > 1L && isTRUE(qualify_companions)) {
        base_nm <- function(nm) sub("[.][0-9]+$", "", nm)
        shown <- if (!is.null(panel_display_labels)) {
          as.character(panel_display_labels)
        } else {
          names(sub)
        }
        companion_rx <- .companion_header_pattern()
        is_companion <- function(nm) {
          grepl(companion_rx, base_nm(nm))
        }
        all_shown <- if (!is.null(display_labels)) {
          as.character(display_labels)
        } else {
          names(x)
        }
        for (k in which(is_companion(shown))) {
          carrier <- NA_integer_
          for (j in rev(seq_len(cols[k] - 1L))) {
            if (!is_companion(all_shown[j]) && !(j %in% align_left_cols)) {
              carrier <- j
              break
            }
          }
          if (!is.na(carrier) && !(carrier %in% cols)) {
            relabeled <- spicy_fmt(
              "header_companion_qualified",
              base_nm(shown[k]),
              base_nm(all_shown[carrier])
            )
            if (!is.null(panel_display_labels)) {
              panel_display_labels[k] <- relabeled
            } else {
              names(sub)[k] <- relabeled
            }
          }
        }
      }
      # Continuation panels of a table whose rows go in groups
      # (`blank_row_groups`, a stacked table): a group none of whose rows
      # has a cell in this panel is left out -- a panel of the stack whose
      # columns all sit in an earlier panel, a fit statistic whose value
      # does. A rule above a row left out passes to the next row kept.
      panel_total <- total_row_idx
      if (panel_i > 1L && length(blank_row_groups)) {
        data_cols_local <- which(!(cols %in% align_left_cols))
        has_cell <- vapply(
          seq_len(nrow(sub)),
          function(r) {
            any(nzchar(trimws(as.character(
              unlist(sub[r, data_cols_local], use.names = FALSE)
            ))))
          },
          logical(1)
        )
        for (b in panel_body_spanners) {
          if (length(b$spans)) has_cell[[b$row]] <- TRUE
        }
        gone <- unique(unlist(Filter(
          function(g) !any(has_cell[g]),
          blank_row_groups
        )))
        if (length(gone)) {
          kept <- setdiff(seq_len(nrow(sub)), gone)
          at <- function(r) match(r[r %in% kept], kept)
          panel_group_sep <- unique(match(
            vapply(panel_group_sep, function(r) kept[kept >= r][1L], 1L),
            kept
          ))
          panel_group_sep <- panel_group_sep[!is.na(panel_group_sep)]
          if (length(panel_total)) {
            panel_total <- at(panel_total)
          }
          panel_body_spanners <- lapply(
            Filter(function(b) b$row %in% kept, panel_body_spanners),
            function(b) list(row = at(b$row), spans = b$spans)
          )
          nm <- names(sub)
          sub <- sub[kept, , drop = FALSE]
          names(sub) <- nm
        }
      }
      # "fit": the labels of this panel's groups widen their columns only
      # when the panel, so widened, still fits the console.
      panel_whole <- if (identical(whole_spanners, "fit")) {
        shown <- sub
        if (!is.null(panel_display_labels)) {
          names(shown) <- panel_display_labels
        }
        ascii_table_total_width(
          shown,
          padding = padding,
          first_column_line = first_column_line,
          row_total_line = row_total_line,
          spanners = panel_spanners
        ) <=
          getOption("width", 80L)
      } else {
        whole_spanners
      }
      build_ascii_table(
        sub,
        padding = padding,
        first_column_line = first_column_line,
        row_total_line = row_total_line,
        bottom_line = bottom_line,
        lines_color = lines_color,
        align_left_cols = which(cols %in% align_left_cols),
        align_center_cols = which(cols %in% align_center_cols),
        center_headers = center_headers,
        spanners = panel_spanners,
        group_sep_rows = panel_group_sep,
        total_row_idx = panel_total,
        display_labels = panel_display_labels,
        body_spanners = panel_body_spanners,
        whole_spanners = panel_whole,
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

# The console header of a descriptive table whose groups of `by` own
# columns, from the header layer its rich engines read: `groups` are the
# records of that layer (`label`, `cols`, and `bounds`, the headers under
# the label, on a group of `by`), `labels` the full header of each column.
# A group of several columns becomes a spanner over them, each column
# headed by its own statistic; a group of one column is headed by the
# group alone, as `build_header_rows_from()` heads it in the rich engines,
# unless it keeps the header of its column (`.spans_columns()`).
# A group whose label cannot name a spanner (empty, or the label of another
# group) keeps the full headers. Returns the headers and the spanners (NULL
# when there is none).
ascii_group_header <- function(labels, groups) {
  spanners <- list()
  for (g in groups) {
    if (is.null(g$bounds) || !nzchar(g$label) || g$label %in% names(spanners)) {
      next
    }
    if (.spans_columns(g)) {
      labels[g$cols] <- g$bounds
      spanners[[g$label]] <- g$cols
    } else {
      labels[g$cols] <- g$label
    }
  }
  list(labels = labels, spanners = if (length(spanners)) spanners)
}
