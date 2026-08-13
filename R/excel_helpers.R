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
# the placement `output_excel()` uses for regression tables. `note`
# may be NULL / "" (nothing is written), and may hold several lines
# separated by "\n". Returns the workbook.
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
  openxlsx2::wb_add_data(
    wb,
    sheet = sheet,
    x = lines,
    start_row = start_row
  )
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
