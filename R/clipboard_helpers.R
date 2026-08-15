# Helpers shared by every clipboard (delimited text) export.
#
# A clipboard payload is plain delimited text: the user pastes it into
# a text editor, a spreadsheet or a word processor. Three rules make
# that text survive the trip, and all three live here so that no
# engine can implement them differently:
#
#   * CELLS ARE ESCAPED. `paste(collapse = delim)` alone loses the
#     grid as soon as a cell holds the delimiter -- a level label with
#     a comma under `clipboard_delim = ","`, or EVERY number under
#     `decimal_mark = ","`. Cells holding the delimiter, a double
#     quote or a line break are quoted RFC 4180-style (embedded
#     quotes doubled), which every spreadsheet and CSV/TSV reader
#     understands. A quote left unescaped is not cosmetic: base R's
#     `read.delim()` silently DROPS it and the cell arrives altered.
#
#   * CELLS ARE PLAIN. The decimal-alignment padding of the console
#     and of the rich engines uses U+2007 FIGURE SPACE, which a
#     parser does not treat as whitespace: a padded number arrives as
#     text next to an unpadded sibling that arrives as a number, so a
#     single column silently splits into two types. Clipboard cells
#     ship unpadded -- alignment belongs to the fixed-width console
#     renderer, not to a delimited payload.
#
#   * CELLS ARE LITERAL. Nothing is wrapped in an Excel text formula
#     (`="..."`): that markup is meaningless in the two other
#     documented paste targets, where it shows up verbatim.
#
# The row layout (title row, header row(s), body, note rows -- each
# padded to the full column count) is the one `clipboard_payload()`
# established for regression tables; the descriptive tables reuse it
# through these helpers.

# One clipboard cell, ready to be joined: NA becomes empty, the
# decimal-alignment padding is stripped, and the cell is quoted when
# it would otherwise break the grid.
.spicy_clip_cell <- function(x, delim) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  # Leading / trailing U+2007 is padding added for the fixed-width
  # renderers; U+00A0 is NOT stripped -- it is the deliberate row-label
  # indentation of `table_categorical()` (Excel and other spreadsheets
  # discard leading ASCII spaces, so the indent has to be non-breaking).
  x <- gsub("^\u2007+|\u2007+$", "", x)
  if (!(length(delim) == 1L && !is.na(delim) && nzchar(delim))) {
    return(x)
  }
  needs_quote <- grepl(delim, x, fixed = TRUE) |
    grepl("\"", x, fixed = TRUE) |
    grepl("\n", x, fixed = TRUE) |
    grepl("\r", x, fixed = TRUE)
  x[needs_quote] <- paste0(
    "\"",
    gsub("\"", "\"\"", x[needs_quote], fixed = TRUE),
    "\""
  )
  x
}

# Assemble the payload: escape every cell, join cells with `delim`,
# join rows with a newline. `rows` is a list of character vectors,
# one per line.
.spicy_clip_payload <- function(rows, delim) {
  # Single validation point for every clipboard route: an empty or
  # non-string delimiter produces an unusable payload with no visible
  # failure at all (the incident the 2026-08 clipboard lot recorded).
  # Multi-character delimiters are legitimate (" | ") and pass.
  if (
    !is.character(delim) ||
      length(delim) != 1L ||
      is.na(delim) ||
      !nzchar(delim)
  ) {
    spicy_abort(
      paste0(
        "`clipboard_delim` must be a single non-empty string ",
        "(e.g. \"\\t\", \";\")."
      ),
      class = "spicy_invalid_input"
    )
  }
  if (length(rows) == 0L) {
    return("")
  }
  lines <- vapply(
    rows,
    function(r) paste(.spicy_clip_cell(r, delim), collapse = delim),
    character(1)
  )
  paste(lines, collapse = "\n")
}

# The rows of a matrix / data.frame as a list of character vectors.
.spicy_clip_rows <- function(x) {
  m <- as.matrix(x)
  if (nrow(m) == 0L) {
    return(list())
  }
  lapply(seq_len(nrow(m)), function(i) as.character(m[i, ]))
}

# The payload of a descriptive table: title row, the header + body
# matrix the engine built, then the note rows. `mat` already holds
# the header row(s) on top of the body -- the descriptive exporters
# assemble it that way for every engine. `title` / `note` come from
# the single-source helpers the console printer uses, so the pasted
# table carries the same caption and the same disclosure the screen
# shows. `clipboard_payload()` is the regression-table twin of this
# function (its header rows are derived from the structured
# contract, hence the separate builder).
.clipboard_payload_desc <- function(mat, delim, title = NULL, note = NULL) {
  n_cols <- ncol(mat)
  rows <- c(
    .spicy_clip_text_rows(title, n_cols),
    .spicy_clip_rows(mat),
    .spicy_clip_text_rows(note, n_cols)
  )
  .spicy_clip_payload(rows, delim)
}

# Title and note lines as full-width rows: the text in the first
# cell, empty cells across the rest of the grid, so the payload keeps
# a rectangular shape whatever the delimiter. `text` may be NULL, ""
# or hold several lines separated by "\n"; the result is a list of
# rows, possibly empty.
.spicy_clip_text_rows <- function(text, n_cols) {
  if (is.null(text) || !any(nzchar(text))) {
    return(list())
  }
  lines <- unlist(strsplit(text, "\n", fixed = TRUE), use.names = FALSE)
  lines <- lines[nzchar(lines)]
  lapply(lines, function(ln) c(ln, rep("", max(0L, n_cols - 1L))))
}
