# Helpers shared by every Excel (xlsx) export.
#
# openxlsx2 writes no `<cols>` element unless it is asked to, so a
# sheet opens with every column at Excel's default 8.43-character
# width. The row labels a spicy table carries ("  Lower secondary
# (ref.)", "WHO-5 wellbeing index (0-100)", "R2 (Nagelkerke)") are two
# to four times that, and because the next column is populated the
# label is clipped on open instead of overflowing. Excel's "AutoFit"
# is a viewer-side action that a written file cannot trigger, so the
# widths have to be computed here.

# Width, in Excel character units, for each column of an exported
# sheet. `cells` is a list of character vectors, one per column,
# holding the text actually written in that column (headers plus the
# DISPLAY strings of the body).
#
# Title and note lines are deliberately NOT measured: they are
# full-width paragraphs written in column A, and sizing the stub
# column to a note would push the table off the screen.
.spicy_xl_widths <- function(cells, min_width = 8.43, max_width = 60) {
  vapply(
    cells,
    function(x) {
      x <- x[!is.na(x)]
      w <- if (length(x) == 0L) 0 else max(nchar(x, type = "width"))
      # +2 for the cell padding Excel adds on both sides.
      min(max(w + 2, min_width), max_width)
    },
    numeric(1)
  )
}

# Set the computed widths on `sheet`. Returns the workbook, so it
# chains like the other openxlsx2 verbs.
.spicy_xl_set_widths <- function(wb, sheet, cells, ...) {
  widths <- .spicy_xl_widths(cells, ...)
  openxlsx2::wb_set_col_widths(
    wb,
    sheet = sheet,
    cols = seq_along(widths),
    widths = widths
  )
}

# Write a table note below the body, one worksheet row per line --
# the placement every Excel export uses. `note` may be NULL / ""
# (nothing is written), and may hold several lines separated by "\n".
# The prefix that opens the first line is set in italics (APA Manual 7
# Section 7.14), as in Word: that cell is written as rich text, an
# italic run and a regular one; it reads back as the same plain text.
# Returns the workbook.
.spicy_xl_add_note <- function(
  wb,
  note,
  start_row,
  sheet = openxlsx2::current_sheet()
) {
  if (is.null(note) || !any(nzchar(note))) {
    return(wb)
  }
  lines <- unlist(strsplit(note, "\n", fixed = TRUE), use.names = FALSE)
  wb <- .spicy_xl_add_data(
    wb,
    sheet = sheet,
    x = lines,
    start_row = start_row
  )
  split <- .note_prefix_split(lines[[1L]])
  if (is.null(split)) {
    return(wb)
  }
  rich <- openxlsx2::fmt_txt(split$marker, italic = TRUE)
  if (nzchar(split$rest)) {
    rich <- rich + openxlsx2::fmt_txt(split$rest)
  }
  openxlsx2::wb_add_data(wb, sheet = sheet, x = rich, start_row = start_row)
}

# Write `x` with an empty cell for every NA and every empty string. An NA
# is left unwritten (`na.strings = NULL`); an "" (the blank of a display
# grid: the gap of a spanner, a variable name on a continuation row, a
# p-value shown once per block) is turned into NA first. The default of
# openxlsx2 writes "#N/A" for an NA, and `na.strings = ""` an empty text
# cell, which Excel counts (COUNTA, filters) and refuses in arithmetic
# (#VALUE!). An unwritten cell reads back as NA, like the empty text did.
.spicy_xl_add_data <- function(wb, x, ...) {
  blank_to_na <- function(v) {
    if (is.character(v)) {
      v[!is.na(v) & !nzchar(v)] <- NA
    }
    v
  }
  if (is.data.frame(x)) {
    x[] <- lapply(x, blank_to_na)
  } else {
    x <- blank_to_na(x)
  }
  # In place: `wb_add_data()` copies the whole workbook at every call.
  wb$add_data(x = x, na = NULL, ...)
}

# Column-wise text of a data.frame plus its header row(s), in the
# shape `.spicy_xl_set_widths()` expects. `headers` is a list of
# character vectors, one per header row.
.spicy_xl_cells <- function(df, headers = list()) {
  lapply(seq_along(df), function(j) {
    c(
      vapply(headers, function(h) as.character(h[[j]]), character(1)),
      as.character(df[[j]])
    )
  })
}

# ---- Number cells ----------------------------------------------------------
# One rule for every Excel export. A body cell that shows one number is a
# number, under a format that shows exactly the console's text; any other
# cell is the console's text. The decimal mark is the reader's: a number
# shows the decimal separator of the reader's Excel (Excel's own setting,
# which can differ from the system's), and only a text cell keeps the
# mark the table was asked for.

# A number format as the console's text shows it: `d` decimals, the
# leading zero (dropped below 1 in the APA style), an explicit sign (the
# difference of a nested comparison) and a literal suffix (the degrees of
# freedom of a test). `.excel_numfmt()` writes its code. A `neg` format
# (set by `.xl_cell()`) adds a section for the negative values, whose
# minus then survives a rounding to zero: "-0.00".
.xl_spec <- function(d, lead_zero = TRUE, signed = FALSE, suffix = NULL) {
  d <- suppressWarnings(as.integer(d))
  if (length(d) != 1L || is.na(d) || d < 0L) {
    d <- 0L
  }
  if (length(suffix) != 1L || !nzchar(suffix)) {
    suffix <- NULL
  }
  apa <- if (!isTRUE(lead_zero) && d > 0L) "apa"
  list(decimals = d, apa = apa, signed = isTRUE(signed), suffix = suffix)
}

.xl_numfmt <- function(s) {
  f <- .excel_numfmt(s$decimals, s$apa, s$signed, s$suffix)
  if (isTRUE(s$neg)) paste0(f, ";-", f) else f
}

# The text Excel shows for `x` under the format `spec`. Excel reads the
# value at 15 significant digits and rounds it half away from zero, where
# R rounds the binary value (2.675 shows "2.68" in Excel, "2.67" in R); a
# value that rounds to zero loses its minus sign, except under a format
# whose sections carry the sign. Checked against Excel 16.0 (build 20527)
# on 2026-11-14, cell by cell (`test-excel_helpers.R`).
.xl_shown <- function(x, spec) {
  d <- spec$decimals
  s <- sprintf("%.14e", abs(x))
  digits <- paste0(substr(s, 1L, 1L), substr(s, 3L, 16L))
  k <- as.integer(substring(s, 18L)) + 1L + d
  n <- if (x == 0 || k < 0L) {
    "0"
  } else if (k >= 15L) {
    paste0(digits, strrep("0", k - 15L))
  } else {
    up <- as.integer(substr(digits, k + 1L, k + 1L)) >= 5L
    sprintf("%.0f", as.numeric(paste0("0", substr(digits, 1L, k))) + up)
  }
  n <- paste0(strrep("0", max(0L, d + 1L - nchar(n))), n)
  int <- substr(n, 1L, nchar(n) - d)
  if (!is.null(spec$apa) && !spec$signed && int == "0") {
    int <- ""
  }
  out <- if (d > 0L) paste0(int, ".", substring(n, nchar(n) - d + 1L)) else int
  minus <- x < 0 && (spec$signed || isTRUE(spec$neg) || grepl("[1-9]", n))
  pre <- if (minus) {
    "-"
  } else if (spec$signed && x > 0) {
    "+"
  } else {
    ""
  }
  paste0(pre, out, spec$suffix)
}

# How one body cell is written: `NULL` for a blank cell (nothing is
# written), `list(value, numfmt)` for a number, `list(text)` otherwise.
# The number is the typed `value` when Excel shows it as the console does,
# else the value the console shows (half way between two displays: 6.25
# printed "6.2"). The format is the column's (`spec`) when it shows the
# console's text, else the one the text shows. "<.001", "3.40e+07",
# "12.3 (4.5)" or a dash are text.
.xl_cell <- function(text, value = NA_real_, spec = NULL, mark = ".") {
  if (is.na(text) || !nzchar(trimws(text))) {
    return(NULL)
  }
  core <- trimws(text)
  suffix <- NULL
  if (!is.null(spec$suffix) && endsWith(core, spec$suffix)) {
    core <- substr(core, 1L, nchar(core) - nchar(spec$suffix))
    suffix <- spec$suffix
  }
  core <- gsub(mark, ".", core, fixed = TRUE)
  shown <- paste0(core, suffix)
  m <- regmatches(core, regexec("^([+-]?)([0-9]*)(\\.[0-9]+)?$", core))[[1L]]
  if (length(m) == 0L || !nzchar(paste0(m[[3L]], m[[4L]]))) {
    return(list(text = text))
  }
  d <- max(0L, nchar(m[[4L]]) - 1L)
  read <- .xl_spec(d, nzchar(m[[3L]]), m[[2L]] == "+", suffix)
  read$neg <- m[[2L]] == "-" && !grepl("[1-9]", core)
  for (v in c(value, as.numeric(core))) {
    for (s in list(spec, read)) {
      if (!is.null(s) && is.finite(v) && identical(.xl_shown(v, s), shown)) {
        return(list(value = v, numfmt = .xl_numfmt(s)))
      }
    }
  }
  list(text = text)
}

# Write the cells of a column that `.xl_cell()` makes numbers, from row
# `at[1]` of column `at[2]`; with `text_too`, its text cells as well.
# `value` holds the typed values (NA where none), `spec` one format or one
# per row. `old` is what the column of the sheet holds (by default
# `text`, written before) and `old_fmt` the formats its cells carry: a
# cell whose number and format are already right is left alone.
#
# Written in blocks, through the in-place methods of the workbook: each
# `wb_add_*()` call copies the whole workbook, which made a sheet of a
# few hundred cells take seconds. A block of values runs through the
# empty cells between two numbers (an NA written with `na = NULL` leaves
# an empty cell empty) and stops at a text cell; a format goes on all its
# cells of the column at once, or, given an environment `formats`, is
# kept there for one call per format over the whole body.
.xl_col <- function(
  wb,
  text,
  value,
  spec,
  at,
  mark = ".",
  text_too = FALSE,
  old = text,
  old_fmt = NULL,
  formats = NULL
) {
  n <- length(text)
  value <- rep_len(as.numeric(value %||% NA_real_), n)
  if (is.null(spec) || !is.null(spec$decimals)) {
    spec <- rep(list(spec), n)
  }
  cells <- lapply(seq_len(n), function(i) {
    .xl_cell(text[[i]], value[[i]], spec[[i]], mark)
  })
  fmt <- vapply(cells, function(x) x$numfmt %||% NA_character_, "")
  x <- vapply(cells, function(x) x$value %||% NA_real_, numeric(1))
  num <- !is.na(fmt)
  empty <- is.na(old) | (is.character(old) & !nzchar(old))
  same <- num & is.numeric(old) & !empty & x == old
  put <- num & !same
  # Rows a block may cover: the numbers, and the empty cells left empty.
  open <- num | (empty & vapply(cells, is.null, NA))
  block <- cumsum(!open)
  for (b in unique(block[put])) {
    rows <- which(block == b & open)
    rows <- seq.int(min(rows[put[rows]]), max(rows[put[rows]]))
    dims <- openxlsx2::wb_dims(rows = at[[1L]] + rows - 1L, cols = at[[2L]])
    wb$add_data(x = x[rows], dims = dims, na = NULL)
  }
  old_fmt <- rep_len(old_fmt %||% NA_character_, n)
  set <- num & (is.na(old_fmt) | fmt != old_fmt)
  for (code in unique(fmt[set])) {
    rows <- which(set & fmt == code)
    dims <- openxlsx2::wb_dims(rows = at[[1L]] + rows - 1L, cols = at[[2L]])
    if (is.environment(formats)) {
      formats[[code]] <- c(formats[[code]], dims)
    } else {
      wb$add_numfmt(dims = dims, numfmt = code)
    }
  }
  is_text <- !vapply(cells, function(x) is.null(x$text), NA)
  for (i in which(text_too & is_text)) {
    dims <- openxlsx2::wb_dims(rows = at[[1L]] + i - 1L, cols = at[[2L]])
    wb <- .spicy_xl_add_data(wb, x = cells[[i]]$text, dims = dims)
  }
  wb
}

# The number cells of a descriptive body already in the sheet as text,
# from row `row`: each column after the `stub` ones, its typed values and
# format from the structured view (`keys` the frozen keys of `text_df`).
# `old_df`, when given, is what the sheet holds instead of `text_df`.
.xl_body <- function(
  wb,
  text_df,
  keys,
  structured,
  row,
  stub,
  mark = ".",
  old_df = text_df
) {
  body <- structured$body
  formats <- new.env(parent = emptyenv())
  for (j in setdiff(seq_along(text_df), seq_len(stub))) {
    meta <- structured$col_meta[[keys[[j]]]]
    value <- if (NROW(body) == nrow(text_df)) body[[keys[[j]]]]
    spec <- if (!is.null(meta$precision)) {
      .xl_spec(meta$precision, !identical(meta$p_style, "apa"))
    }
    if (!is.numeric(value)) {
      value <- NULL
    }
    text <- as.character(text_df[[j]])
    old <- old_df[[j]]
    at <- c(row, j)
    wb <- .xl_col(wb, text, value, spec, at, mark, old = old, formats = formats)
  }
  for (code in names(formats)) {
    wb$add_numfmt(dims = paste(formats[[code]], collapse = ","), numfmt = code)
  }
  wb
}
