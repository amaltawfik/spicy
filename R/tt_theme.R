# tinytable::theme_empty() clears ALL lazy slots of a tinytable --
# including `lazy_finalize`, where tinytable stores its OUTPUT-FORMAT
# hooks (e.g. the Typst finalizer that honors
# `options(tinytable_typst_multipage)`: without it a table taller than
# one page does not break, and its last rows can overprint). Wiping it
# silently disabled those document-level options for every spicy table
# (field report, dev/theme_empty_efface_les_finaliseurs.md). This bare
# variant strips only the APPEARANCE slots -- the actual intent of the
# five call sites -- and leaves the format finalizers alive. Every
# spicy tinytable path must use it instead of theme_empty(). `stub` is
# the number of label columns that open the table, `decimal_mark` the
# mark of its numbers.
.spicy_tt_bare <- function(x, stub = 1L, decimal_mark = ".") {
  x@lazy_format <- list()
  # A column spanner (`group_tt(j = )`) is merged across its columns by
  # styles tinytable registers itself, which the appearance wipe below
  # took with it. HTML and Typst build the merged cell structurally and
  # lost nothing; LaTeX merges only through the `colspan` style, so every
  # spicy spanner ("95% CI" over LL / UL, a group over its n and %) sat
  # over its first column alone. Those styles are kept, for LaTeX only.
  spans <- Filter(
    function(e) isTRUE(attr(e, "tt_group_j")) && !is.null(e[["colspan"]]),
    x@lazy_style
  )
  x@lazy_style <- lapply(spans, function(e) structure(e, output = "latex"))
  # The lead-in of the note: decided by the style in force in THIS call,
  # where the note was built (`.note_lead_keys()`, R/i18n.R), and handed
  # to the preparation and finalizing steps below, which run when the
  # table is printed or saved -- after the call, its style context gone.
  lead <- .note_lead()
  # The sign of the negative numbers, decided by the style in force in
  # this call too (`spicy_style(minus_sign = )`): set in the cells after
  # the label columns (`.spicy_tt_minus()`) and in the numbers of the
  # note (`.apply_minus_sign()`), for the outputs each step names.
  sign <- .style_minus_sign()
  minus <- function(n) .apply_minus_sign(n, sign, "note", decimal_mark)
  # The two preparation steps spicy registers itself: the note of the
  # markdown output, set before tinytable lays it out
  # (`.spicy_tt_note_markdown()`), and the fit of a LaTeX table to the
  # text block (`.spicy_tt_latex_fit`).
  x@lazy_prepare <- list(
    .spicy_tt_note_markdown(lead, minus),
    .spicy_tt_latex_fit
  )
  if (identical(sign, "typographic")) {
    x@lazy_prepare <- c(
      x@lazy_prepare,
      list(.spicy_tt_minus(stub, decimal_mark))
    )
  }
  # tinytable hard-codes `column-gutter: 5pt` into the Typst #table()
  # call whenever the table has column groups (group_tt(j = )) -- an
  # argument-level value no document `#set` rule can override. In a
  # real 34-table report, the 16 tables with grouped headers carried
  # a gutter the other 18 lacked (field report,
  # dev/gouttiere_tinytable_group_tt.md). Strip it on every spicy
  # table: geometry belongs to the receiving document, whose `#set`
  # rules become effective again once nothing overrides them.
  # Fixed upstream in tinytable 0.18.0 (issue #674): group_tt(j = )
  # no longer emits the gutter, so the sub() below matches nothing
  # there. It stays as a no-op safety for older tinytables -- a hard
  # >= 0.18.0 floor on an optional engine would refuse a whole output
  # over a cosmetic gutter. Drop it when something else raises the
  # tinytable floor past 0.18.0.
  #
  # The note rewriters find each note in the rendered string by its
  # TEXT, so they are handed the notes as the table was built with them:
  # by the time a finalizer runs, tinytable has rewritten the slot for
  # some outputs (in Typst it holds `text([...])`).
  notes <- x@notes
  # An invalid `options(spicy.note_style = )` is refused in this call,
  # which builds the table, not when the table is printed or saved.
  if (any(nzchar(.spicy_note_texts(notes)))) {
    .spicy_note_style()
  }
  tinytable::style_tt(x, finalize = function(tbl) {
    # The size every body cell shares, which the note follows in every
    # output (`.spicy_tt_note_scale()`).
    scale <- .spicy_tt_note_scale(tbl@style, tbl@nrow, tbl@ncol)
    if (identical(tbl@output, "typst")) {
      s <- sub("\\s*column-gutter: 5pt,", "", tbl@table_string)
      groups <- tryCatch(tbl@group_data_j, error = function(e) NULL)
      spans <- .spicy_tt_colspan_cells(tbl@style)
      s <- .spicy_tt_typst_layout(s, groups, spans, stub)
      tbl@table_string <- .spicy_tt_note_typst(s, notes, lead, minus, scale)
    } else if (identical(tbl@output, "html")) {
      tbl@table_string <- .spicy_tt_note_tfoot(
        tbl@table_string,
        notes,
        lead,
        minus,
        scale
      )
    } else if (identical(tbl@output, "latex")) {
      tbl@table_string <- .spicy_tt_note_latex(
        tbl@table_string,
        notes,
        lead,
        minus,
        scale
      )
    }
    tbl
  })
}

# Typst: the layout tinytable writes for a table with a note. Without a
# note, tinytable gives Typst `auto` columns, which Typst sizes itself,
# and none of this applies. With one, tinytable measures each column in
# a one-column grid, outside the table, and when their sum is wider than
# the space it shrinks every column in the same proportion. The grid
# meets none of the styles of the cells (`style_tt(fontsize = )`, bold,
# indent) nor the document's rules for tables (`show table: set text()`,
# `set table(inset: )`, a bold header row), so a column can come out
# narrower than its widest cell, which wraps; and the proportional shrink
# cuts short headers under their longest word and squeezes the columns of
# numbers.
#
# * Each column is measured alone, at its place, in a real table
#   (`.spicy_tt_typst_measure()`): the widths are those Typst gives
#   `auto` columns. A rule of tinytable's template pads an indented cell
#   even when it is empty; the cell is padded only when it has content
#   (`.spicy_tt_typst_indent_rule()`), so the empty cells of the measure
#   weigh nothing.
# * A cell that spans columns (the label of a group row) is left out of
#   the measure (`.spicy_tt_typst_colspans()`), and each column-group
#   label is measured over the columns it spans, which are widened until
#   it fits (`.spicy_tt_typst_spans()`).
# * A table wider than the space shrinks by the role of its columns, the
#   labels first and by whole lines (`.spicy_tt_typst_shrink()`).
# * Quarto's template justifies paragraphs, and Typst hyphenates
#   justified text, table cells and notes included. No other engine
#   justifies a cell. The table is set ragged right, which also stops the
#   hyphenation: a column narrower than its words wraps between words,
#   never inside one, and the note reads flush left as in HTML and Word.
#
# Each is a substitution on a line of tinytable's template; on another
# template it matches nothing and the table is left as it came. `stub`
# is the number of label columns that open the table.
.spicy_tt_typst_layout <- function(s, groups = NULL, spans = NULL, stub = 1L) {
  s <- .spicy_tt_typst_indent_rule(s)
  s <- .spicy_tt_typst_colspans(s, spans)
  s <- .spicy_tt_typst_measure(s, groups)
  s <- .spicy_tt_typst_spans(s, groups)
  s <- .spicy_tt_typst_shrink(s, stub)
  sub(
    "// start block\n",
    "// start block\n  #set par(justify: false)\n",
    s,
    fixed = TRUE
  )
}

# The line of tinytable's template that measures the natural widths of
# the columns, which `.spicy_tt_typst_measure()` replaces.
.spicy_tt_typst_naturals <- paste0(
  "    let tinytable-naturals = tinytable-coldata.map(col => ",
  "measure(grid(columns: 1, inset: 5pt, ..col)).width)"
)

# The line that takes the total width from the natural widths, before
# which the column-group labels and the shrink are inserted.
.spicy_tt_typst_total <- paste0(
  "    let tinytable-total = ",
  "calc.min(tinytable-naturals.sum(), size.width)"
)

# tinytable's `show table.cell` rule pads a cell with an `indent` style
# even when it is empty. The measure places each column in a table whose
# other cells are empty: an indented row would pad its empty label cell
# and widen every other column by the indent. Only a cell with content is
# padded, and only where the measure applies.
.spicy_tt_typst_indent_rule <- function(s) {
  if (!grepl(.spicy_tt_typst_naturals, s, fixed = TRUE)) {
    return(s)
  }
  sub(
    "if (\"indent\" in style) { tmp = pad(left: style.indent, tmp) }",
    paste0(
      "if (\"indent\" in style and it.body != []) ",
      "{ tmp = pad(left: style.indent, tmp) }"
    ),
    s,
    fixed = TRUE
  )
}

# The natural width of each column, measured in a real table with that
# column at its place and every other cell empty, so that tinytable's
# `show table.cell` rule (size, weight, indent of each cell) and the
# document's rules for tables apply as they do in the table. The column
# names sit under the `NROW(groups)` rows of column-group labels.
# `spicy_blank` is the width of the empty table, `spicy_inset` that of
# one empty column (its two insets): the measured width less the empty
# columns is the column's own. `spicy_width(x, col, true)` sets every
# space as a line break: the width of the longest word, which the shrink
# reads.
.spicy_tt_typst_measure <- function(s, groups = NULL) {
  if (!grepl(.spicy_tt_typst_naturals, s, fixed = TRUE)) {
    return(s)
  }
  code <- c(
    "    // spicy: each column measured at its place in a table",
    "    let spicy_n = tinytable-coldata.len()",
    paste0(
      "    let spicy_inset = measure(table(columns: 1, stroke: none, ",
      "table.cell(x: 0, y: 0, []))).width"
    ),
    paste0(
      "    let spicy_blank = measure(table(columns: spicy_n, stroke: none, ",
      "..range(spicy_n).map(spicy_x => table.cell(x: spicy_x, y: 0, []))))",
      ".width"
    ),
    "    let spicy_width(spicy_x, spicy_col, spicy_least) = {",
    paste0(
      "      let spicy_t = table(columns: spicy_n, stroke: none, ",
      "..spicy_col.enumerate().map(((spicy_k, spicy_cell)) => ",
      sprintf(
        "table.cell(x: spicy_x, y: spicy_k + %d, spicy_cell)))",
        NROW(groups)
      )
    ),
    paste0(
      "      measure(if spicy_least { show \" \": linebreak(); spicy_t } ",
      "else { spicy_t }).width - spicy_blank + spicy_inset"
    ),
    "    }",
    paste0(
      "    let tinytable-naturals = tinytable-coldata.enumerate().map(",
      "((spicy_x, spicy_col)) => spicy_width(spicy_x, spicy_col, false))"
    )
  )
  sub(
    .spicy_tt_typst_naturals,
    paste(code, collapse = "\n"),
    s,
    fixed = TRUE
  )
}

# The column-group labels of a tinytable (`groups`, its `group_data_j`:
# one row per header row of groups, the label in the first column of its
# span, blank in the columns the span continues over, NA where no group
# sits), each measured at its place in the header (its row, its first
# column, its span) as the measure reads the columns, and the columns
# under a label wider than they are widened, evenly, until it fits. Row
# `r` of `groups` is header row `r - 1`. The spans are read as tinytable
# reads them when it writes the header (`parse_group_spans()`); the
# labels are already escaped for Typst when the finalizer runs, exactly
# as the header prints them. Only after the measure, whose `spicy_blank`
# it reads.
.spicy_tt_typst_spans <- function(s, groups) {
  if (
    !is.data.frame(groups) ||
      nrow(groups) == 0L ||
      !grepl(.spicy_tt_typst_total, s, fixed = TRUE) ||
      !grepl("let spicy_blank = ", s, fixed = TRUE)
  ) {
    return(s)
  }
  spans <- character(0)
  for (r in seq_len(nrow(groups))) {
    row <- as.character(unlist(groups[r, ], use.names = FALSE))
    pos <- 1L
    while (pos <= length(row)) {
      label <- row[[pos]]
      if (is.na(label)) {
        pos <- pos + 1L
        next
      }
      start <- pos
      pos <- pos + 1L
      while (
        pos <= length(row) && !is.na(row[[pos]]) && !nzchar(trimws(row[[pos]]))
      ) {
        pos <- pos + 1L
      }
      if (nzchar(trimws(label)) && !grepl("\n", label, fixed = TRUE)) {
        spans <- c(
          spans,
          sprintf("(%d, %d, %d, [%s])", start - 1L, pos - 2L, r - 1L, label)
        )
      }
    }
  }
  if (length(spans) == 0L) {
    return(s)
  }
  code <- c(
    "    // spicy: a column-group label wider than its columns widens them",
    sprintf("    let spicy_spans = (%s,)", paste(spans, collapse = ", ")),
    paste0(
      "    let spicy_span_width(spicy_from, spicy_to, spicy_row, spicy_label, ",
      "spicy_least) = {"
    ),
    "      let spicy_span = spicy_to - spicy_from + 1",
    paste0(
      "      let spicy_t = table(columns: spicy_n, stroke: none, ",
      "table.cell(x: spicy_from, y: spicy_row, colspan: spicy_span, ",
      "spicy_label))"
    ),
    paste0(
      "      measure(if spicy_least { show \" \": linebreak(); spicy_t } ",
      "else { spicy_t }).width - spicy_blank + spicy_span * spicy_inset"
    ),
    "    }",
    "    for (spicy_from, spicy_to, spicy_row, spicy_label) in spicy_spans {",
    "      let spicy_span = spicy_to - spicy_from + 1",
    paste0(
      "      let spicy_need = spicy_span_width(spicy_from, spicy_to, ",
      "spicy_row, spicy_label, false)"
    ),
    paste0(
      "      let spicy_have = ",
      "tinytable-naturals.slice(spicy_from, spicy_to + 1).sum()"
    ),
    "      if spicy_need > spicy_have {",
    "        for k in range(spicy_from, spicy_to + 1) {",
    "          tinytable-naturals.at(k) += (spicy_need - spicy_have) / spicy_span",
    "        }",
    "      }",
    "    }",
    ""
  )
  sub(
    .spicy_tt_typst_total,
    paste0(paste(code, collapse = "\n"), .spicy_tt_typst_total),
    s,
    fixed = TRUE
  )
}

# A table wider than the space shrinks by the role of its columns: the
# `stub` label columns that open it, then the data columns. Each column
# has a natural width `nat`, a floor (its longest word, never more than
# `nat`; a column-group label raises the floors of its columns, up to
# `nat`), `two` (half-way between, at which its longest text takes two
# lines at most) and `three` (a third of the way up from the floor, three
# lines at most). A column of numbers has floor = nat: it never gives.
# The first step that fits applies:
#   1. the data keep `nat`; the labels share the rest, between `three`
#      and `nat`, so a label wraps before a header does;
#   2. data columns drop to `two` one at a time, largest gain first,
#      until the labels can have their `three`; the labels share the
#      rest, between `three` and `nat`;
#   3. the labels at `two`; the data share the rest, between floor and
#      `two`;
#   4. the data share the room the labels leave at `three`, between floor
#      and `two`; the labels share the rest, between `three` and `two`;
#   5. the data at their floors; the labels share the rest, between floor
#      and `two`;
#   6. nothing fits at the floors: tinytable's proportional shrink of the
#      natural widths.
# "Share" is Typst's shrink of `auto` columns held between two bounds:
# each column takes min(high, t), never under low, for the largest t that
# fits, found by bisection. With one block only (no stub, or every column
# a label), every column is a data column. The floors are measured only
# for a table wider than the space.
.spicy_tt_typst_shrink <- function(s, stub = 1L) {
  if (
    !grepl(.spicy_tt_typst_total, s, fixed = TRUE) ||
      !grepl("let spicy_width(", s, fixed = TRUE)
  ) {
    return(s)
  }
  stub <- suppressWarnings(as.integer(stub))
  if (length(stub) != 1L || is.na(stub) || stub < 0L) {
    stub <- 1L
  }
  raise <- c(
    "      for (spicy_from, spicy_to, spicy_row, spicy_label) in spicy_spans {",
    "        let spicy_span = spicy_to - spicy_from + 1",
    "        let spicy_need = spicy_span_width(spicy_from, spicy_to, spicy_row, spicy_label, true)",
    "        let spicy_have = spicy_floor.slice(spicy_from, spicy_to + 1).sum()",
    "        if spicy_need > spicy_have {",
    "          for k in range(spicy_from, spicy_to + 1) {",
    "            spicy_floor.at(k) = calc.min(spicy_nat.at(k), spicy_floor.at(k) + (spicy_need - spicy_have) / spicy_span)",
    "          }",
    "        }",
    "      }"
  )
  code <- c(
    "    // spicy: a table wider than the space shrinks by the role of its columns",
    "    if tinytable-naturals.sum() > size.width {",
    "      let spicy_nat = tinytable-naturals",
    "      let spicy_floor = tinytable-coldata.enumerate().map(((spicy_x, spicy_col)) =>",
    "        calc.min(spicy_width(spicy_x, spicy_col, true), spicy_nat.at(spicy_x)))",
    if (grepl("let spicy_spans = ", s, fixed = TRUE)) raise,
    "      let spicy_two = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + spicy_floor.at(spicy_k)) / 2)",
    "      let spicy_three = range(spicy_n).map(spicy_k => (spicy_nat.at(spicy_k) + 2 * spicy_floor.at(spicy_k)) / 3)",
    "      let spicy_sum(spicy_cols, spicy_w) = spicy_cols.map(spicy_k => spicy_w.at(spicy_k)).sum(default: 0pt)",
    "      let spicy_fit(spicy_cols, spicy_target, spicy_low, spicy_high) = {",
    "        if spicy_sum(spicy_cols, spicy_high) <= spicy_target {",
    "          return spicy_cols.map(spicy_k => spicy_high.at(spicy_k))",
    "        }",
    "        let spicy_at(spicy_t) = spicy_cols.map(spicy_k =>",
    "          calc.max(spicy_low.at(spicy_k), calc.min(spicy_high.at(spicy_k), spicy_t)))",
    "        let spicy_lo = 0pt",
    "        let spicy_hi = calc.max(..spicy_cols.map(spicy_k => spicy_high.at(spicy_k)))",
    "        for _ in range(30) {",
    "          let spicy_mid = (spicy_lo + spicy_hi) / 2",
    "          if spicy_at(spicy_mid).sum() > spicy_target { spicy_hi = spicy_mid } else { spicy_lo = spicy_mid }",
    "        }",
    "        spicy_at(spicy_lo)",
    "      }",
    sprintf(
      "      let spicy_stub = if %d > 0 and %d < spicy_n { %d } else { 0 }",
      stub,
      stub,
      stub
    ),
    "      let spicy_s = range(0, spicy_stub)",
    "      let spicy_d = range(spicy_stub, spicy_n)",
    "      let spicy_room = size.width",
    "      let spicy_three_s = spicy_sum(spicy_s, spicy_three)",
    "      let spicy_two_s = spicy_sum(spicy_s, spicy_two)",
    "      let spicy_floor_s = spicy_sum(spicy_s, spicy_floor)",
    "      let spicy_nat_d = spicy_sum(spicy_d, spicy_nat)",
    "      let spicy_two_d = spicy_sum(spicy_d, spicy_two)",
    "      let spicy_floor_d = spicy_sum(spicy_d, spicy_floor)",
    "      if spicy_room - spicy_nat_d >= spicy_three_s {",
    "        tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_nat_d, spicy_three, spicy_nat) + spicy_d.map(spicy_k => spicy_nat.at(spicy_k))",
    "      } else if spicy_room - spicy_two_d >= spicy_three_s {",
    "        let spicy_data = spicy_d.map(spicy_k => spicy_nat.at(spicy_k))",
    "        let spicy_order = range(spicy_d.len()).sorted(key: spicy_i =>",
    "          spicy_two.at(spicy_d.at(spicy_i)) - spicy_nat.at(spicy_d.at(spicy_i)))",
    "        for spicy_i in spicy_order {",
    "          if spicy_room - spicy_data.sum(default: 0pt) >= spicy_three_s { break }",
    "          spicy_data.at(spicy_i) = spicy_two.at(spicy_d.at(spicy_i))",
    "        }",
    "        tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_nat) + spicy_data",
    "      } else if spicy_room - spicy_floor_d >= spicy_two_s {",
    "        tinytable-naturals = spicy_s.map(spicy_k => spicy_two.at(spicy_k)) + spicy_fit(spicy_d, spicy_room - spicy_two_s, spicy_floor, spicy_two)",
    "      } else if spicy_room - spicy_floor_d >= spicy_three_s {",
    "        let spicy_data = spicy_fit(spicy_d, spicy_room - spicy_three_s, spicy_floor, spicy_two)",
    "        tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_data.sum(default: 0pt), spicy_three, spicy_two) + spicy_data",
    "      } else if spicy_room - spicy_floor_d >= spicy_floor_s {",
    "        tinytable-naturals = spicy_fit(spicy_s, spicy_room - spicy_floor_d, spicy_floor, spicy_two) + spicy_d.map(spicy_k => spicy_floor.at(spicy_k))",
    "      }",
    "    }",
    ""
  )
  sub(
    .spicy_tt_typst_total,
    paste0(paste(code, collapse = "\n"), .spicy_tt_typst_total),
    s,
    fixed = TRUE
  )
}

# The size every body cell of a built tinytable shares, from its styles
# (`style_tt(fontsize = )`, its `style` slot when it is finalized): the
# last size set on each cell; 1 when the cells differ, some carry none, or
# the styles cannot be read. Header rows (i < 1) and the notes are not
# body cells. The note is set at 0.9 of it in every output.
.spicy_tt_note_scale <- function(style, nrow, ncol) {
  if (
    !is.data.frame(style) ||
      !all(c("i", "j", "fontsize") %in% names(style)) ||
      length(nrow) != 1L ||
      length(ncol) != 1L ||
      is.na(nrow) ||
      is.na(ncol) ||
      nrow < 1L ||
      ncol < 1L
  ) {
    return(1)
  }
  size <- suppressWarnings(as.numeric(style$fontsize))
  i <- suppressWarnings(as.integer(style$i))
  j <- suppressWarnings(as.integer(style$j))
  cells <- matrix(NA_real_, nrow, ncol)
  for (k in which(!is.na(size))) {
    rows <- if (is.na(style$i[[k]])) seq_len(nrow) else i[[k]]
    cols <- if (is.na(style$j[[k]])) seq_len(ncol) else j[[k]]
    rows <- rows[!is.na(rows) & rows >= 1L & rows <= nrow]
    cols <- cols[!is.na(cols) & cols >= 1L & cols <= ncol]
    cells[rows, cols] <- size[[k]]
  }
  f <- unique(as.vector(cells))
  if (length(f) == 1L && !is.na(f) && f > 0) f else 1
}

# The size of a note in em of its table: 0.9 of the size of the body
# cells (`scale`, `.spicy_tt_note_scale()`), as a number for the markup.
.spicy_note_em <- function(scale = 1) {
  format(round(0.9 * scale, 4), scientific = FALSE)
}

# The cells of `tinytable-coldata` that span columns, out of the measure
# of the natural widths. tinytable measures such a cell as an entry of
# its first column: the label of a group row, which spans the columns
# of the groups (`.spicy_tt_span_label()`), kept the first column as
# wide as itself. A group label is a section head, which never sizes
# the columns: longer than the columns it spans, it wraps inside them.
# `cells` are those of the built table's styles
# (`.spicy_tt_colspan_cells()`), each found by its position: its column,
# and its body row, the row counted from the column names that open each
# column of the measure. Emptied, it weighs nothing. The cells it covers
# stay in the measure: blank in a spicy table, which spans no other row.
.spicy_tt_typst_colspans <- function(s, cells) {
  anchor <- "    let tinytable-naturals = tinytable-coldata.map("
  if (
    !is.data.frame(cells) ||
      nrow(cells) == 0L ||
      !grepl(anchor, s, fixed = TRUE)
  ) {
    return(s)
  }
  at <- sprintf("(%d, %d)", as.integer(cells$j) - 1L, as.integer(cells$i))
  code <- c(
    "    // spicy: a cell that spans columns is measured with none of them",
    paste0(
      "    let tinytable-coldata = tinytable-coldata.enumerate().map(",
      "((spicy_x, spicy_col)) => spicy_col.enumerate().map(",
      "((spicy_k, spicy_cell)) => {"
    ),
    sprintf(
      "      if (spicy_x, spicy_k) in (%s,) { [] } else { spicy_cell }",
      paste(at, collapse = ", ")
    ),
    "    }))",
    ""
  )
  sub(anchor, paste0(paste(code, collapse = "\n"), anchor), s, fixed = TRUE)
}

# The body cells of a built tinytable that span columns, from its
# styles (`style_tt(colspan = )`, its `style` slot when it is
# finalized): their body row `i` and their first column `j`. `NULL`
# when there is none.
.spicy_tt_colspan_cells <- function(style) {
  if (
    !is.data.frame(style) ||
      !all(c("i", "j", "colspan") %in% names(style))
  ) {
    return(NULL)
  }
  i <- suppressWarnings(as.integer(style$i))
  span <- suppressWarnings(as.integer(style$colspan))
  hit <- !is.na(i) & i > 0L & !is.na(span) & span > 1L
  if (!any(hit)) {
    return(NULL)
  }
  unique(data.frame(i = i[hit], j = as.integer(style$j[hit])))
}

# Indent the label cell of `rows`, the rows the console indents
# (`.struct_indent_rows()`), in a descriptive table.
#
# tinytable's `indent` is structural: a `\hspace` before the text in
# LaTeX, a `pad(left:)` around the cell in Typst. The builders also
# prefix each label with four non-breaking spaces, which HTML needs (it
# collapses the leading white space of a cell) and which LaTeX and
# markdown keep. In Typst the two stacked, and the spaces moved the
# first line only: a label that wrapped started its next lines at the
# pad, an em to the left of the text above them. The Typst cell keeps
# the pad alone, under which every line starts at the same x. A
# preparation step drops the spaces, for Typst only, from the cells as
# tinytable is about to write them: escaped, and placed by data row.
.spicy_tt_indent <- function(x, rows) {
  x <- tinytable::style_tt(x, i = rows, j = 1, indent = 1)
  prefix <- strrep("\u00A0", 4)
  drop_prefix <- function(tbl) {
    at <- tbl@index_body[rows]
    lab <- tbl@data_body[[1L]][at]
    hit <- which(startsWith(lab, prefix))
    tbl@data_body[[1L]][at[hit]] <- substring(lab[hit], nchar(prefix) + 1L)
    tbl
  }
  x@lazy_prepare <- c(
    x@lazy_prepare,
    list(structure(drop_prefix, output = "typst"))
  )
  x
}

# Span the label of a group row over the first `k` columns, in Typst.
#
# A group row opens the block of a variable in a table that crosses it:
# the variable's label in the first column, the statistics of the
# crossing (the p, the effect size, the SMD) in their own columns, and
# blank cells under the groups and their margin. Written as a cell of
# the first column, the label wrapped at the width of the levels under
# it, however wide the table: a section head is not an entry of the
# column. `rows` are the group rows and `k` the columns the label spans,
# the stub and the columns of the groups (their counts, percentages and
# intervals). A row whose cells under the label are not all blank -- a
# variable with no data, whose row carries the mark of each group --
# keeps its cells. The span is set for Typst only, the other outputs
# keep their cells, and the natural widths Typst measures leave the
# label out (`.spicy_tt_typst_colspans()`). The spanning cell carries no
# indent; the level rows keep theirs (`.spicy_tt_indent()`).
.spicy_tt_span_label <- function(x, rows, k) {
  if (length(rows) == 0L || k < 2L) {
    return(x)
  }
  under <- as.matrix(x@data[rows, seq_len(k)[-1L], drop = FALSE])
  blank <- !nzchar(trimws(under, whitespace = "[\\h\\v]"))
  dim(blank) <- dim(under)
  rows <- rows[rowSums(!blank) == 0L]
  if (length(rows) == 0L) {
    return(x)
  }
  # Typst only, tagged as `.spicy_tt_bare()` tags the LaTeX spans: every
  # tinytable this package runs on honors the tag, while the `output`
  # argument of `style_tt()` dates from tinytable 0.17.0.
  x <- tinytable::style_tt(x, i = rows, j = 1, colspan = k)
  last <- length(x@lazy_style)
  attr(x@lazy_style[[last]], "output") <- "typst"
  x
}

# The minus sign (U+2212) for the hyphen-minus of a negative number, in
# the cells `x` as tinytable is about to write them for `engine`.
#
# spicy formats a negative number with the hyphen-minus ("-0.8"), which
# every output whose text is copied or recomputed needs: a spreadsheet
# or a parser reads the minus sign as text. A printed table sets the
# minus sign, as wide as the plus sign, where the hyphen is short.
#
# A hyphen is a sign when a digit follows it, or a decimal mark and a
# digit (a bounded measure drops its leading zero: "-.25"), and no
# letter, digit or backslash precedes it: the start of the cell, a
# space, a bracket, the separator of an interval. So "-0.8",
# "[-0.8, 0.3]" and "(-1.2 to 0.4)" take the sign, "COVID-19", "1-5",
# "2026-10-24", "1e-05", "Non-smoker" and "Q3 - Q1" keep their hyphen.
# The marks are the point, the comma and the table's `decimal_mark`.
#
# The cells come escaped. tinytable writes the Typst hyphen as `\-`,
# which the sign replaces whole: the minus sign needs no escape. In
# LaTeX, `\-` is a discretionary hyphen and stays one; the minus sign
# is set in math mode, which every engine and font has, by the
# finalizer (`.spicy_latex_unicode()`).
.spicy_minus_sign <- function(
  x,
  engine = c("typst", "latex"),
  decimal_mark = "."
) {
  engine <- match.arg(engine)
  hyphen <- if (identical(engine, "typst")) "\\\\?-" else "-"
  marks <- paste(unique(c(".", ",", decimal_mark)), collapse = "")
  pattern <- paste0(
    "(?<![\\p{L}\\p{N}\\\\])",
    hyphen,
    "(?=[0-9]|[",
    gsub("([]\\\\^-])", "\\\\\\1", marks),
    "][0-9])"
  )
  # The replacement is UTF-8, which runs PCRE in UTF mode: `\p{L}` is
  # any letter, an accented one included.
  gsub(pattern, intToUtf8(0x2212), x, perl = TRUE)
}

# The minus sign of a printed table, for the engines that print one:
# flextable (and Word), gt, tinytable outside markdown. `style` is the
# `minus_sign` of the style in force where the table is built; under
# "ascii", or on anything but text, `x` comes back as it is.
#
# `where = "cell"` is the rule of `.spicy_minus_sign()`, on the cells as
# spicy formats them (unescaped). `where = "note"` is stricter, since a
# note is prose: a hyphen is a sign only at the start of the note or
# after a space, "=", "(", "[", "," or ";", before a digit (or a mark
# and a digit). So "-4.16", "= -0.5" and "(-1.2" take the sign, and
# "2026-10-28", "N-1", "two-sided" and "Holm-adjusted" keep their
# hyphen. A data frame has its text columns after the `stub` label
# columns rewritten, the labels left as typed.
#
# The outputs whose text is copied or read back (the data frame, the
# console, Excel, the clipboard, markdown, the long and tidy outputs)
# never call it: a spreadsheet reads U+2212 as text.
.apply_minus_sign <- function(
  x,
  style = .style_minus_sign(),
  where = c("cell", "note"),
  decimal_mark = ".",
  stub = 1L
) {
  where <- match.arg(where)
  if (!identical(style, "typographic") || is.null(x)) {
    return(x)
  }
  if (is.data.frame(x)) {
    for (j in setdiff(seq_along(x), seq_len(stub))) {
      if (is.character(x[[j]])) {
        x[[j]] <- .apply_minus_sign(x[[j]], style, where, decimal_mark)
      }
    }
    return(x)
  }
  if (!is.character(x)) {
    return(x)
  }
  if (identical(where, "cell")) {
    return(.spicy_minus_sign(x, "latex", decimal_mark))
  }
  marks <- paste(unique(c(".", ",", decimal_mark)), collapse = "")
  pattern <- paste0(
    "(?<![^\\s=(\\[,;])-(?=[0-9]|[",
    gsub("([]\\\\^-])", "\\\\\\1", marks),
    "][0-9])"
  )
  gsub(pattern, intToUtf8(0x2212), x, perl = TRUE)
}

# TRUE inside a knit to Word: tinytable writes its markdown grid there,
# which pandoc turns into a Word table, a printed table.
.spicy_knit_docx <- function() {
  isTRUE(getOption("knitr.in.progress")) &&
    requireNamespace("knitr", quietly = TRUE) &&
    identical(knitr::opts_knit$get("rmarkdown.pandoc.to"), "docx")
}

# The preparation step that sets the minus sign in the cells tinytable
# is about to write (`.spicy_minus_sign()`), after the `stub` label
# columns: the labels keep the text the user typed, as the headers do.
# Typst, LaTeX and HTML; markdown only inside a knit to Word
# (`.spicy_knit_docx()`), the markdown printed or saved as text keeping
# the hyphen. The cells come escaped: the Typst hyphen is `\-`, LaTeX
# and HTML leave it as it is. So the table and the natural widths Typst
# measures its columns with (`tinytable-coldata`) carry the same
# character, and no code around the cells is touched.
.spicy_tt_minus <- function(stub = 1L, decimal_mark = ".") {
  structure(
    function(tbl) {
      if (identical(tbl@output, "markdown") && !.spicy_knit_docx()) {
        return(tbl)
      }
      engine <- if (identical(tbl@output, "typst")) "typst" else "latex"
      for (j in setdiff(seq_along(tbl@data_body), seq_len(stub))) {
        cells <- tbl@data_body[[j]]
        if (is.character(cells)) {
          tbl@data_body[[j]] <- .spicy_minus_sign(
            cells,
            engine,
            decimal_mark
          )
        }
      }
      tbl
    },
    output = c("typst", "latex", "html", "markdown")
  )
}

# LaTeX: a table wider than the text block is scaled down to it, a
# narrower one is left at its size -- the `resize_direction = "down"` of
# `tinytable::theme_latex()`, whose `\resizebox` this writes. tabularray
# sets a table at its natural width, so a wide one ran off the page (73
# to 90pt for a categorical table by a four-level group), its note with
# it: the note of a `talltblr` is as wide as the table. Scaled with the
# table, the note wraps inside the page.
#
# Not through `theme_latex()` itself. Its finalizer would be registered
# when spicy builds the table, and run BEFORE a `theme_latex(multipage =
# TRUE)` the user adds afterwards: that conversion to a page-breaking
# `longtblr` drops the line that opens the box and keeps the one that
# closes it, and the document no longer compiles. Its finalizer also
# stops on a table it finds no `tblr` in. So this is a preparation step,
# which appends the finalizer when the table is built for output, after
# every finalizer registered by then; and the finalizer leaves alone a
# `longtblr` -- a table that breaks across pages is not one box, and
# tinytable refuses that combination itself -- and a table already in a
# `\resizebox`, which the user asked for.
.spicy_tt_latex_fit <- structure(
  function(tbl) {
    fit <- function(t) {
      s <- t@table_string
      open <- "(\\\\begin\\{(talltblr|tblr|tabular)\\})"
      close <- "(\\\\end\\{(talltblr|tblr|tabular)\\})"
      if (
        grepl("\\\\begin\\{longtblr\\}|\\\\resizebox", s) ||
          !grepl(open, s) ||
          !grepl(close, s)
      ) {
        return(t)
      }
      # `%` ends each added line, so no space enters the box.
      s <- sub(
        open,
        paste0(
          "\\\\resizebox{\\\\ifdim\\\\width>\\\\linewidth",
          "\\\\linewidth\\\\else\\\\width\\\\fi}{!}{%\n\\1"
        ),
        s
      )
      t@table_string <- sub(close, "\\1%\n}", s)
      t
    }
    tbl@lazy_finalize <- c(
      tbl@lazy_finalize,
      list(structure(fit, output = "latex"))
    )
    tbl
  },
  output = "latex"
)


# ---- Table notes: subordination by size ----------------------------------
#
# tinytable puts the note in a structural slot of the rendered table
# (`table.footer()` in Typst, a bare `<tfoot>` cell in HTML, the
# `note{}` of tabularray in LaTeX) that no document-level rule reaches,
# so a spicy note came out at body size and body color. A note is
# subordinate to the table it documents: every other spicy engine
# already says so typographically (gt 0.9em, Word 10pt against an 11pt
# body), and the typographic canon subordinates by SIZE, in black --
# gray is a screen convention that fails on a dark background. Default:
# 0.9em, black, on the tinytable engine's Typst and HTML output, and
# LaTeX's `\small`, the size one step down (0.9 of a 10pt body, 0.91 of
# an 11pt or 12pt one). When every body cell carries the same size `f`
# (`style_tt(fontsize = )`, `.spicy_tt_note_scale()`), the note follows
# it, at 0.9 * f em in all three outputs, unless the note was styled
# itself (`style_tt(i = "notes")`).
#
# `options(spicy.note_style)`:
#   * unset / NULL -- the 0.9em default;
#   * "none"       -- no intervention at all; the note renders exactly
#                     as the receiving template styles it;
#   * any other string -- extra Typst `text()` arguments appended to
#                     the size (e.g. "fill: luma(89)" for a gray note).
#                     Typst-only: HTML keeps the plain 0.9em.
.spicy_note_style <- function() {
  opt <- getOption("spicy.note_style", NULL)
  if (is.null(opt)) {
    return(list(resize = TRUE, typst_extra = NULL))
  }
  if (!is.character(opt) || length(opt) != 1L || is.na(opt)) {
    spicy_abort(
      c(
        "`options(spicy.note_style)` must be a single string or `NULL`.",
        "i" = paste(
          "`NULL` (the default) renders table notes at 0.9em;",
          "\"none\" leaves them to the output template;",
          "any other string is added to the Typst `text()` call",
          "(e.g. \"fill: luma(89)\")."
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  if (identical(opt, "none")) {
    return(list(resize = FALSE, typst_extra = NULL))
  }
  list(resize = TRUE, typst_extra = opt)
}

# CSS declaration for spicy's own HTML note container ("" when the
# user asked for no intervention), at 0.9 of the body size `scale`.
.spicy_note_css <- function(scale = 1) {
  if (isTRUE(.spicy_note_style()$resize)) {
    paste0("font-size: ", .spicy_note_em(scale), "em; ")
  } else {
    ""
  }
}

# Minimal HTML text-content escape: the three characters that change
# how a parser reads ELEMENT CONTENT. Quotes are deliberately absent --
# nothing here is interpolated into an attribute value, and escaping
# them would turn every apostrophe in a note into an entity.
.spicy_html_escape <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  s
}

# The emphasis of the note prefix in each markup engine, for
# `.note_markup()`.
.spicy_html_em <- function(p) paste0("<em>", p, "</em>")

# A note as the HTML of the display paths of gt and flextable, which
# lift it out of the table into a `<div>`: each line of the note
# (`.note_lines()`) escaped, the lead-in of the first in `<em>` (APA
# Manual 7 Section 7.14), the lines joined by a line break -- the legend
# of the stars on a line of its own after the general note.
.spicy_note_html <- function(lines, lead = .note_lead()) {
  paste(
    c(
      .note_markup(lines[[1L]], .spicy_html_escape, .spicy_html_em, lead),
      vapply(lines[-1L], .spicy_html_escape, character(1), USE.NAMES = FALSE)
    ),
    collapse = "<br>"
  )
}

# A note carries one disclosure per line, and HTML and LaTeX collapse
# the newlines that separate them into spaces -- the per-model standard-
# error attributions ran together into one sentence. Convert the line
# breaks to the engine's forced break (`br`: `<br>` in HTML, `\\` in
# LaTeX) and keep each continuation line's indentation with the
# engine's no-break space (`space`: `&nbsp;`, `~`).
.spicy_note_lines <- function(x, br = "<br>", space = "&nbsp;") {
  parts <- strsplit(x, "\n", fixed = TRUE)[[1L]]
  if (length(parts) <= 1L) {
    return(x)
  }
  parts[-1L] <- vapply(
    parts[-1L],
    function(p) {
      lead <- regmatches(p, regexpr("^[ \t]+", p))
      if (length(lead) == 1L && nzchar(lead)) {
        p <- paste0(
          strrep(space, nchar(lead)),
          substring(p, nchar(lead) + 1L)
        )
      }
      p
    },
    character(1),
    USE.NAMES = FALSE
  )
  paste(parts, collapse = br)
}

# Escape sets for LaTeX and Typst. The note must be escaped: it is
# package prose, but it interpolates user data (a variable name such as
# `life_sat_health`, a label holding `&`) and its own wording carries
# syntax characters (the `%` of "more than 100%", the stars of a
# significance legend), and tinytable escapes the cells, captions and
# headers of the table (`format_tt(escape = TRUE)`) but never the note.
# In LaTeX an unescaped `%` commented out the rest of the note with its
# closing brace, an `_` stopped the compilation; in Typst the legend
# `*** p < .001, ** p < .01, * p < .05.` lost its stars to bold markup,
# and the `//` of a URL opened a comment that broke the compilation. The
# two sets are tinytable's own, so a note escapes exactly like the text
# around it; each special character is replaced in one pass, so a
# replacement is never escaped twice.
.spicy_latex_escape <- function(x) {
  chars <- c(
    "\\" = "\\textbackslash{}",
    "~" = "\\textasciitilde{}",
    "^" = "\\textasciicircum{}",
    "&" = "\\&",
    "%" = "\\%",
    "$" = "\\$",
    "#" = "\\#",
    "_" = "\\_",
    "{" = "\\{",
    "}" = "\\}"
  )
  m <- gregexpr("[\\\\~^&%$#_{}]", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(s) unname(chars[s]))
  x
}

.spicy_typst_escape <- function(x) {
  gsub("([\\\\<>*_@=+/$#\\[\\]`~\"-])", "\\\\\\1", x, perl = TRUE)
}

# Apply `f` to every note string of a tinytable, keeping the shape the
# `notes` slot was given (a string, a possibly named list of strings, or
# `list(text = , i = , j = )` entries; see `.spicy_note_texts()`).
.spicy_map_notes <- function(notes, f) {
  if (is.character(notes)) {
    notes[] <- vapply(notes, f, character(1), USE.NAMES = FALSE)
    return(notes)
  }
  if (is.list(notes)) {
    if (!is.null(notes$text)) {
      notes$text <- .spicy_map_notes(notes$text, f)
      return(notes)
    }
    notes[] <- lapply(notes, .spicy_map_notes, f = f)
  }
  notes
}

# Markdown: tinytable wraps the note into the last rows of its grid table
# (the console print of a tinytable, and the table of a Quarto or R
# Markdown Word document, which pandoc reads as markdown). A grid cell
# holds no line break: the newline of a note of several lines was written
# into the grid as it was, broke the row that closes it, and pandoc left
# the note out of the Word document. So each line of the note
# (`.note_lines()`) is a note of its own, which tinytable sets in a row of
# its own: the general note, one paragraph, then the legend of the stars,
# as in Word and gt. A cell note gets the same rows, its marker on the
# first (`.spicy_split_note_lines()`). In a document, the lead-in `lead`
# is set in `*...*` on the note TEXT, before tinytable lays it out, so the
# grid stays aligned; printed in a terminal, the grid is plain text and
# the note stays as the console prints it, with no markup. "A document"
# is a knit in progress -- Quarto and R Markdown. Registered by
# `.spicy_tt_bare()` as a preparation step for the markdown output only.
.spicy_tt_note_markdown <- function(lead, minus = identity) {
  structure(
    function(tbl) {
      knit <- isTRUE(getOption("knitr.in.progress"))
      # The numbers of the note take the minus sign in a Word document
      # only, as the cells do (`.spicy_tt_minus()`).
      sign <- if (.spicy_knit_docx()) minus else identity
      tbl@notes <- .spicy_map_notes(
        .spicy_split_note_lines(tbl@notes),
        function(n) if (knit) .note_markdown(sign(n), lead = lead) else n
      )
      tbl
    },
    output = "markdown"
  )
}

# The notes of a tinytable with each line of a note made a note of its
# own (`.note_lines()`), for the markdown grid, whose cells hold no line
# break. The slot holds NULL or a list: `tt(notes = )` stores the string
# the package's builders give it as a list of one. An unnamed list of
# strings becomes a list with one string per line. A list with names also
# holds notes with a marker, a named string or a cell note
# `list(i = , j = , text = )`, such as a reader adds to a spicy table:
# each note is split the same way, its first line keeps its name and its
# cells, so tinytable still sets the marker in the cell and before that
# line, and each further line follows it as a note of its own, with no
# marker. The newline of a cell note used to reach the grid as it was and
# broke the row. Any other shape is left as it is.
.spicy_split_note_lines <- function(notes) {
  if (
    is.list(notes) &&
      is.null(names(notes)) &&
      all(vapply(notes, is.character, logical(1)))
  ) {
    return(as.list(unlist(lapply(notes, .note_lines), use.names = FALSE)))
  }
  if (is.list(notes) && !is.null(names(notes)) && is.null(notes$text)) {
    out <- list()
    for (k in seq_along(notes)) {
      note <- notes[k]
      cell <- is.list(note[[1L]])
      lines <- .note_lines(if (cell) note[[1L]]$text else note[[1L]])
      if (length(lines) > 1L) {
        if (cell) {
          note[[1L]]$text <- lines[[1L]]
        } else {
          note[[1L]] <- lines[[1L]]
        }
        note <- c(note, as.list(lines[-1L]))
      }
      out <- c(out, note)
    }
    return(out)
  }
  notes
}

# The note strings a tinytable was built with, as a plain character
# vector. `tt(notes = )` accepts a string, a (possibly named) list of
# strings, or a list of `list(text = , i = , j = )` entries; the slot
# keeps whichever shape it was given. Only the TEXT is of interest
# here, so recurse and keep the character leaves.
.spicy_note_texts <- function(notes) {
  if (is.null(notes)) {
    return(character(0))
  }
  if (is.character(notes)) {
    return(notes)
  }
  if (is.list(notes)) {
    if (!is.null(notes$text)) {
      return(.spicy_note_texts(notes$text))
    }
    return(unlist(lapply(notes, .spicy_note_texts), use.names = FALSE))
  }
  character(0)
}

# HTML: escape the note, then restyle the `<tfoot>` note cell tinytable
# renders. Every spicy tinytable takes this route (the finalizer of
# `.spicy_tt_bare()`), whatever the family.
#
# A `<td colspan="N">` cell gives its max-content width to the columns
# it spans, so a long note would stretch the table past its body. The text
# sits in a `<div>` with `width: min-content; min-width: 100%`: its
# contribution to the column widths falls to its longest word, then it
# fills the cell, so the note wraps at the width of the body. The note
# stays inside the `<table>`, where the PNG capture of
# `tinytable::save_tt()` finds it (`body > div > table`).
#
# The note is package prose, but it INTERPOLATES user data -- the
# outcome label, the grouping variable's name, the levels a measure was
# computed on -- so a label shaped like a tag reached the `<tfoot>`
# live. Escaping it upstream, where the note is composed, is not an
# option: the same string feeds the Typst, LaTeX and markdown backends,
# each with its own escape set, and the console prints it as it is. So
# the escape is ENGINE-AWARE: each finalizer branch treats the note for
# the one output it sees -- escaped for HTML here, for LaTeX in
# `.spicy_tt_note_latex()`, for Typst in `.spicy_tt_note_typst()`.
#
# It is applied by exact substitution of each note the table was built
# with, rather than to the rendered cell text: a label carrying
# `</td>` would otherwise be indistinguishable from the cell boundary
# the styling loop below splits on, and would survive the escape as
# the separator that re-joins the pieces.
#
# The same substitution sets the lead-in `lead` in italics (APA Manual 7
# Section 7.14) through `.note_markup()`, as every engine does. A note
# that does not open with the lead-in is escaped and left unmarked.
#
# The cell is set at 0.9 of the body size `scale`
# (`.spicy_tt_note_scale()`), and at 0.9em when the note was styled
# itself (`style_tt(i = "notes")`, which tinytable writes as a
# `<span style=>` around the text).
.spicy_tt_note_tfoot <- function(
  s,
  notes = NULL,
  lead = .note_lead(),
  minus = identity,
  scale = 1
) {
  m <- regexpr("<tfoot>[\\s\\S]*?</tfoot>", s, perl = TRUE)
  if (m < 0L) {
    return(s)
  }
  block <- regmatches(s, m)
  for (note in .spicy_note_texts(notes)) {
    if (nzchar(note)) {
      note_html <- .note_markup(
        minus(note),
        .spicy_html_escape,
        .spicy_html_em,
        lead
      )
      block <- sub(note, note_html, block, fixed = TRUE)
    }
  }
  # Split on cell boundaries so the transformation applies to note
  # TEXT only: everything after the last `<td ...>` of each piece.
  pieces <- strsplit(block, "</td>", fixed = TRUE)[[1L]]
  for (k in seq_along(pieces)) {
    opens <- gregexpr("<td [^>]*>", pieces[k])[[1L]]
    if (opens[1L] < 0L) {
      next
    }
    last <- length(opens)
    open_start <- opens[last]
    open_end <- open_start + attr(opens, "match.length")[last] - 1L
    open_tag <- substring(pieces[k], open_start, open_end)
    text <- substring(pieces[k], open_end + 1L)
    text <- .spicy_note_lines(text)
    css <- .spicy_note_css(if (startsWith(text, "<span style=")) 1 else scale)
    if (nzchar(css) && !grepl("style=", open_tag, fixed = TRUE)) {
      open_tag <- sub(
        ">$",
        paste0(" style=\"", css, "line-height: 1.25; \">"),
        open_tag
      )
    }
    pieces[k] <- paste0(
      substring(pieces[k], 1L, open_start - 1L),
      open_tag,
      "<div class=\"spicy-tt-note\" ",
      "style=\"width: min-content; min-width: 100%;\">",
      text,
      "</div>"
    )
  }
  regmatches(s, m) <- paste(pieces, collapse = "</td>")
  s
}

# Typst: tinytable emits the note as
# `table.cell(align: left, colspan: N, text([...])),`, the text exactly
# as given, so Typst read it as markup: the stars of the significance
# legend `*** p < .001, ** p < .01, * p < .05.` turned into bold and
# vanished, and the `//` of a URL opened a comment that broke the
# compilation. The note is plain text in every other engine (HTML and
# LaTeX escape it), and Typst now escapes it too: a note typed as Typst
# markup (`#text(8pt)[...]`) prints as typed, as it does in HTML.
#
# Three repairs, inside each note cell. The note is escaped and its
# lead-in `lead` set in `#emph[...]` (APA Manual 7 Section 7.14, as in
# HTML), found by exact substitution of each note the table was built
# with (`notes`), as the HTML branch does. The embedded newlines (Typst
# collapses them exactly as HTML does) become forced line breaks with
# their indentation kept as no-break spaces. And the whole note is
# wrapped at 0.9 of the body size `scale` (`.spicy_tt_note_scale()`), in
# em of the table, unless it was already styled through
# `tinytable::style_tt(i = "notes")` -- an explicit user style wins.
.spicy_tt_note_typst <- function(
  s,
  notes = NULL,
  lead = .note_lead(),
  minus = identity,
  scale = 1
) {
  lines <- strsplit(s, "\n", fixed = TRUE)[[1L]]
  cell_open <- "^\\s*table\\.cell\\(align: left, colspan: [0-9]+, "
  starts <- grep(cell_open, lines)
  if (length(starts) == 0L) {
    return(s)
  }
  st <- .spicy_note_style()
  texts <- .spicy_note_texts(notes)
  texts <- texts[nzchar(texts)]
  for (i0 in starts) {
    i1 <- i0
    while (i1 <= length(lines) && !grepl("\\]\\)\\),\\s*$", lines[i1])) {
      i1 <- i1 + 1L
    }
    if (i1 > length(lines)) {
      next
    }
    cell <- paste(lines[i0:i1], collapse = "\n")
    for (note in texts) {
      if (grepl(note, cell, fixed = TRUE)) {
        cell <- sub(
          note,
          .note_markup(
            minus(note),
            .spicy_typst_escape,
            .spicy_typst_emph,
            lead
          ),
          cell,
          fixed = TRUE
        )
        lines[i0:i1] <- strsplit(cell, "\n", fixed = TRUE)[[1L]]
        break
      }
    }
    if (i1 > i0) {
      for (k in seq.int(i0, i1 - 1L)) {
        lines[k] <- paste0(lines[k], " \\")
      }
      for (k in seq.int(i0 + 1L, i1)) {
        indent <- regmatches(lines[k], regexpr("^[ \t]+", lines[k]))
        if (length(indent) == 1L && nzchar(indent)) {
          lines[k] <- paste0(
            strrep("~", nchar(indent)),
            substring(lines[k], nchar(indent) + 1L)
          )
        }
      }
    }
    if (isTRUE(st$resize) && grepl("text\\(\\[", lines[i0])) {
      size <- paste0("size: ", .spicy_note_em(scale), "em")
      args <- if (is.null(st$typst_extra)) {
        size
      } else {
        paste0(size, ", ", st$typst_extra)
      }
      lines[i0] <- sub(
        "text\\(\\[",
        paste0("text(", args, ", ["),
        lines[i0]
      )
    }
  }
  paste(lines, collapse = "\n")
}

# The lead-in reaches it escaped (`.note_markup()`), as in the other
# engines.
.spicy_typst_emph <- function(p) paste0("#emph[", p, "]")

# LaTeX: tinytable's tabularray template writes each note as
# `note{}={...}`, the text exactly as given -- the `%` of a
# multiple-response note commented out the rest of it with its closing
# brace, and the `_` of a variable name stopped the compilation. The
# note is substituted there escaped for LaTeX, its lead-in `lead` in
# `\emph{}` (APA Manual 7 Section 7.14, as in HTML and Typst), one
# disclosure per line and one size down (`\small`) as in those two
# outputs; when every body cell has the size `scale`
# (`.spicy_tt_note_scale()`), 0.9 of it, written as tinytable writes the
# size of a cell, with its leading. A note styled itself
# (`style_tt(i = "notes")`) comes wrapped in a group of its own, which
# the key below does not match. Then the characters spicy writes that
# LaTeX cannot set with every engine and font are mapped, for the whole
# table (`.spicy_latex_unicode()`).
.spicy_tt_note_latex <- function(
  s,
  notes = NULL,
  lead = .note_lead(),
  minus = identity,
  scale = 1
) {
  size <- if (!isTRUE(.spicy_note_style()$resize)) {
    ""
  } else if (scale == 1) {
    "\\small "
  } else {
    sprintf(
      "\\fontsize{%sem}{%sem}\\selectfont ",
      .spicy_note_em(scale),
      format(round(0.9 * scale + 0.3, 4), scientific = FALSE)
    )
  }
  for (note in .spicy_note_texts(notes)) {
    key <- paste0("note{}={", note, "}")
    if (!nzchar(note) || !grepl(key, s, fixed = TRUE)) {
      next
    }
    tex <- .note_markup(
      minus(note),
      .spicy_latex_escape,
      .spicy_latex_emph,
      lead
    )
    tex <- .spicy_note_lines(tex, br = "\\\\\n", space = "~")
    # A paragraph takes the leading in force where it ends: tabularray
    # ends it outside the group of the note, so the note's lines kept the
    # leading of the document. `\endgraf` ends it inside, where the
    # `\par` of the key's argument would not be allowed.
    if (nzchar(size)) {
      tex <- paste0(tex, "\\endgraf")
    }
    s <- sub(key, paste0("note{}={", size, tex, "}"), s, fixed = TRUE)
  }
  .spicy_latex_unicode(s)
}

.spicy_latex_emph <- function(p) paste0("\\emph{", p, "}")

# The non-ASCII characters spicy writes into a table that LaTeX cannot
# set everywhere. Under xelatex and lualatex (the defaults of tinytable
# and of Quarto) with the default Latin Modern font, the lowercase Greek
# letters of the statistics (the chi-squared of a note, beta, eta,
# omega, sigma, the gamma of an association header, the zeta of a
# cut-point), the less-than-or-equal sign of a formula and the figure
# space of the decimal alignment are not in the font: they vanished from
# the PDF with no error. pdflatex refuses them, and Delta and the minus
# sign too, outright. Math mode has the Greek letters and the signs in
# every engine, a phantom zero is exactly the width the figure space
# stands for, and a squared letter is set as a superscript of its own --
# Cohen's f2 too, so that it reads like the eta2 and the omega2 beside
# it. The plus-minus sign, which every engine sets, is set in math too,
# from the font of the minus sign beside it. Every other character spicy
# writes (accents, the en dash, the no-break space, the middle dot, the
# multiplication sign, the superscript two of R2) is set by all three
# engines as it is.
.spicy_latex_unicode <- function(s) {
  greek <- c(
    alpha = 0x03B1,
    beta = 0x03B2,
    gamma = 0x03B3,
    delta = 0x03B4,
    varepsilon = 0x03B5,
    zeta = 0x03B6,
    eta = 0x03B7,
    theta = 0x03B8,
    rho = 0x03C1,
    sigma = 0x03C3,
    tau = 0x03C4,
    varphi = 0x03C6,
    chi = 0x03C7,
    omega = 0x03C9,
    Delta = 0x0394
  )
  # Accented first (sigma hat, chi bar), so the bare letter does not
  # take their base away from the combining mark.
  math <- c("\\hat{\\sigma}", "\\bar{\\chi}", paste0("\\", names(greek)))
  names(math) <- c(
    intToUtf8(c(0x03C3, 0x0302)),
    intToUtf8(c(0x03C7, 0x0304)),
    intToUtf8(greek, multiple = TRUE)
  )
  squared <- intToUtf8(0x00B2)
  for (k in names(math)) {
    s <- gsub(
      paste0(k, squared),
      paste0("\\ensuremath{", math[[k]], "^{2}}"),
      s,
      fixed = TRUE
    )
    s <- gsub(k, paste0("\\ensuremath{", math[[k]], "}"), s, fixed = TRUE)
  }
  # The f of Cohen's f2, as a symbol of its own: not the end of a word.
  s <- gsub(
    paste0("(?<![[:alpha:]])f", squared),
    "\\\\ensuremath{f^{2}}",
    s,
    perl = TRUE
  )
  text <- c(
    "\\hphantom{0}",
    "\\,",
    "\\ensuremath{-}",
    "\\ensuremath{\\pm}",
    "\\ensuremath{\\leq}"
  )
  names(text) <- intToUtf8(
    c(0x2007, 0x202F, 0x2212, 0x00B1, 0x2264),
    multiple = TRUE
  )
  for (k in names(text)) {
    s <- gsub(k, text[[k]], s, fixed = TRUE)
  }
  s
}


# HTML-escape the parts of a tinytable that carry USER DATA.
#
# tinytable passes cell text through to the backend unescaped, so a
# level label, a variable label or a caption that looks like markup is
# parsed as markup. The consequences are not cosmetic: a label holding
# `</td></tr><tr><td>` closes its own cell and row and the engine
# renders a table with MORE rows than the object has, silently
# redistributing the statistics; a label holding `<script>` is emitted
# live into the document. gt and flextable escape; tinytable is the
# one engine that does not.
#
# The slot list is `table_regression()`'s, and `"notes"` is left OUT of
# it on purpose: tinytable writes the note inside a Typst `text([...])`
# call and a LaTeX `note{}={...}` key, and its escaper cannot tell the
# note's text from that wrapper. The finalizer of `.spicy_tt_bare()`
# handles the note instead, for the one output it renders: escaped in
# HTML, LaTeX and Typst, its lead-in in italics in all three. Cell
# escaping still applies, so real CI brackets stay protected.
#
# `format_tt(escape = TRUE)` escapes for the output being rendered, so
# the same call protects the LaTeX and Typst cells (`wellbeing\_score`).
.spicy_tt_escape <- function(x) {
  tinytable::format_tt(
    x,
    i = c("colnames", "caption", "~groupi", "groupi", "groupj", "cells"),
    escape = TRUE
  )
}
