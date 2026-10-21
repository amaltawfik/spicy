# tinytable::theme_empty() clears ALL lazy slots of a tinytable --
# including `lazy_finalize`, where tinytable stores its OUTPUT-FORMAT
# hooks (e.g. the Typst finalizer that honors
# `options(tinytable_typst_multipage)`: without it a table taller than
# one page does not break, and its last rows can overprint). Wiping it
# silently disabled those document-level options for every spicy table
# (field report, dev/theme_empty_efface_les_finaliseurs.md). This bare
# variant strips only the APPEARANCE slots -- the actual intent of the
# five call sites -- and leaves the format finalizers alive. Every
# spicy tinytable path must use it instead of theme_empty().
.spicy_tt_bare <- function(x) {
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
  # The two preparation steps spicy registers itself: the note of the
  # markdown output, set before tinytable lays it out
  # (`.spicy_tt_note_markdown()`), and the fit of a LaTeX table to the
  # text block (`.spicy_tt_latex_fit`).
  x@lazy_prepare <- list(.spicy_tt_note_markdown(lead), .spicy_tt_latex_fit)
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
  tinytable::style_tt(x, finalize = function(tbl) {
    if (identical(tbl@output, "typst")) {
      s <- sub("\\s*column-gutter: 5pt,", "", tbl@table_string)
      groups <- tryCatch(tbl@group_data_j, error = function(e) NULL)
      s <- .spicy_tt_typst_layout(s, groups)
      tbl@table_string <- .spicy_tt_note_typst(s, notes, lead)
    } else if (identical(tbl@output, "html")) {
      tbl@table_string <- .spicy_tt_note_tfoot(tbl@table_string, notes, lead)
    } else if (identical(tbl@output, "latex")) {
      tbl@table_string <- .spicy_tt_note_latex(tbl@table_string, notes, lead)
    }
    tbl
  })
}

# Typst: three repairs of the layout tinytable writes for a table with a
# note, all behind words broken in the middle ("Fe-male", "(No item
# cho-sen)") or run into each other ("EmployeursParticipants").
#
# * tinytable sizes such a table's columns from their natural widths,
#   each measured as a one-column grid with `inset: 5pt`, Typst's default
#   -- while the table takes the inset of the document's `#set table()`
#   rule, and Quarto's Typst template sets 6pt. Every column came out 2pt
#   narrower than its widest cell, which wrapped. The measure now uses
#   the inset the table will have, `table.inset`, read in the `context`
#   the measure already runs in.
# * The same measure reads the cells of each column and its name, not
#   the column-group labels above them (`group_tt(j = )`: a `by` level
#   over its n and %, "95% CI" over LL and UL, "p" and "Cramer's V" in
#   the top header row). A label wider than the columns under it ran
#   into the next one, or wrapped. Each label is measured as well, and
#   the columns it spans are widened, evenly, until it fits
#   (`.spicy_tt_typst_spans()`). Without a note, tinytable gives Typst
#   `auto` columns, which Typst widens for a spanning cell itself.
# * Quarto's template justifies paragraphs, and Typst hyphenates
#   justified text, table cells and notes included. No other engine
#   justifies a cell. The table is set ragged right, which also stops the
#   hyphenation: a column narrower than its words -- a table wider than
#   the page, which tinytable shrinks to it -- wraps between words, never
#   inside one, and the note reads flush left as in HTML and Word.
#
# Each is a substitution on a line of tinytable's template; on another
# template it matches nothing and the table is left as it came.
.spicy_tt_typst_layout <- function(s, groups = NULL) {
  s <- gsub(
    "measure(grid(columns: 1, inset: 5pt, ..col))",
    "measure(grid(columns: 1, inset: table.inset, ..col))",
    s,
    fixed = TRUE
  )
  s <- .spicy_tt_typst_spans(s, groups)
  sub(
    "// start block\n",
    "// start block\n  #set par(justify: false)\n",
    s,
    fixed = TRUE
  )
}

# The column-group labels of a tinytable (`groups`, its `group_data_j`:
# one row per header row of groups, the label in the first column of
# its span, blank in the columns the span continues over, NA where no
# group sits) as Typst code that widens the measured natural widths
# until every label fits over its span. The spans are read as tinytable
# reads them when it writes the header (`parse_group_spans()`); the
# labels are already escaped for Typst when the finalizer runs, exactly
# as the header prints them. The code goes right after the line that
# measures the columns, before the total width is taken from them.
.spicy_tt_typst_spans <- function(s, groups) {
  anchor <- "let tinytable-naturals = tinytable-coldata.map("
  if (
    !is.data.frame(groups) ||
      nrow(groups) == 0L ||
      !grepl(anchor, s, fixed = TRUE)
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
          sprintf("(%d, %d, [%s])", start - 1L, pos - 2L, label)
        )
      }
    }
  }
  if (length(spans) == 0L) {
    return(s)
  }
  code <- c(
    "    // spicy: a column-group label wider than its columns widens them",
    sprintf(
      "    for (spicy_from, spicy_to, spicy_label) in (%s,) {",
      paste(spans, collapse = ", ")
    ),
    paste0(
      "      let spicy_need = measure(grid(columns: 1, inset: table.inset, ",
      "spicy_label)).width"
    ),
    paste0(
      "      let spicy_have = ",
      "tinytable-naturals.slice(spicy_from, spicy_to + 1).sum()"
    ),
    "      if spicy_need > spicy_have {",
    "        for k in range(spicy_from, spicy_to + 1) {",
    paste0(
      "          tinytable-naturals.at(k) += ",
      "(spicy_need - spicy_have) / (spicy_to - spicy_from + 1)"
    ),
    "        }",
    "      }",
    "    }"
  )
  lines <- strsplit(s, "\n", fixed = TRUE)[[1L]]
  at <- grep(anchor, lines, fixed = TRUE)[[1L]]
  paste(append(lines, code, after = at), collapse = "\n")
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
# an 11pt or 12pt one).
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
# user asked for no intervention).
.spicy_note_css <- function() {
  if (isTRUE(.spicy_note_style()$resize)) "font-size: 0.9em; " else ""
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
# is a knit in progress -- Quarto and R Markdown -- which is also how
# `print.spicy_gt()` tells the two apart. Registered by `.spicy_tt_bare()`
# as a preparation step for the markdown output only.
.spicy_tt_note_markdown <- function(lead) {
  structure(
    function(tbl) {
      knit <- isTRUE(getOption("knitr.in.progress"))
      tbl@notes <- .spicy_map_notes(
        .spicy_split_note_lines(tbl@notes),
        function(n) if (knit) .note_markdown(n, lead = lead) else n
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
# renders (the route taken by the descriptive families;
# table_regression() and table_continuous_lm() lift the note out of the
# table grid instead, see `.spicy_tt_note_div()`).
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
.spicy_tt_note_tfoot <- function(s, notes = NULL, lead = .note_lead()) {
  m <- regexpr("<tfoot>[\\s\\S]*?</tfoot>", s, perl = TRUE)
  if (m < 0L) {
    return(s)
  }
  css <- .spicy_note_css()
  block <- regmatches(s, m)
  for (note in .spicy_note_texts(notes)) {
    if (nzchar(note)) {
      note_html <- .note_markup(
        note,
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
    if (nzchar(css) && !grepl("style=", open_tag, fixed = TRUE)) {
      open_tag <- sub(">$", paste0(" style=\"", css, "\">"), open_tag)
    }
    pieces[k] <- paste0(
      substring(pieces[k], 1L, open_start - 1L),
      open_tag,
      text
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
# wrapped at 0.9em, unless it was already styled through
# `tinytable::style_tt(i = "notes")` -- an explicit user style wins.
.spicy_tt_note_typst <- function(s, notes = NULL, lead = .note_lead()) {
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
          .note_markup(note, .spicy_typst_escape, .spicy_typst_emph, lead),
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
        lead <- regmatches(lines[k], regexpr("^[ \t]+", lines[k]))
        if (length(lead) == 1L && nzchar(lead)) {
          lines[k] <- paste0(
            strrep("~", nchar(lead)),
            substring(lines[k], nchar(lead) + 1L)
          )
        }
      }
    }
    if (isTRUE(st$resize) && grepl("text\\(\\[", lines[i0])) {
      args <- if (is.null(st$typst_extra)) {
        "size: 0.9em"
      } else {
        paste0("size: 0.9em, ", st$typst_extra)
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
# outputs. Then the characters spicy writes that LaTeX cannot set with
# every engine and font are mapped, for the whole table
# (`.spicy_latex_unicode()`).
.spicy_tt_note_latex <- function(s, notes = NULL, lead = .note_lead()) {
  size <- if (isTRUE(.spicy_note_style()$resize)) "\\small " else ""
  for (note in .spicy_note_texts(notes)) {
    key <- paste0("note{}={", note, "}")
    if (!nzchar(note) || !grepl(key, s, fixed = TRUE)) {
      next
    }
    tex <- .note_markup(note, .spicy_latex_escape, .spicy_latex_emph, lead)
    tex <- .spicy_note_lines(tex, br = "\\\\\n", space = "~")
    s <- sub(key, paste0("note{}={", size, tex, "}"), s, fixed = TRUE)
  }
  .spicy_latex_unicode(s)
}

.spicy_latex_emph <- function(p) paste0("\\emph{", p, "}")

# The non-ASCII characters spicy writes into a table that LaTeX cannot
# set everywhere. Under xelatex and lualatex (the defaults of tinytable
# and of Quarto) with the default Latin Modern font, the lowercase Greek
# letters of the statistics (the chi-squared of a note, beta, eta,
# omega, sigma) and the figure space of the decimal alignment are not in
# the font: they vanished from the PDF with no error. pdflatex refuses
# them, and Delta and the minus sign too, outright. Math mode has the
# Greek letters in every engine, a phantom zero is exactly the width the
# figure space stands for, and a squared letter is set as a superscript
# of its own -- Cohen's f2 too, so that it reads like the eta2 and the
# omega2 beside it. Every other character spicy writes (accents, the en
# dash, the no-break space, the middle dot, the multiplication sign, the
# superscript two of R2) is set by all three engines as it is.
.spicy_latex_unicode <- function(s) {
  greek <- c(
    alpha = 0x03B1,
    beta = 0x03B2,
    varepsilon = 0x03B5,
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
  text <- c("\\hphantom{0}", "\\,", "\\ensuremath{-}")
  names(text) <- intToUtf8(c(0x2007, 0x202F, 0x2212), multiple = TRUE)
  for (k in names(text)) {
    s <- gsub(k, text[[k]], s, fixed = TRUE)
  }
  s
}


# ---- HTML: the note as a table-width sibling ------------------------------
#
# In `table-layout: auto`, a `<tfoot><td colspan="N">` cell contributes
# its max-content width to every column it spans, so a multi-line note
# (longest line unwrapped) forces the whole table wider than the body
# needs. `.spicy_tt_note_div()` + `.spicy_tt_wrap_html()` pull the note
# out of the table grid entirely and re-inject it as an inline-block
# sibling, so the body alone determines column widths.
#
# APA Manual 7 Section 7.14 expects the note flush-left with the table's
# left edge AND wrapped within the table's width. Pure CSS achieves
# this with the `width: min-content; min-width: 100%` trick: the note's
# *preferred* width is its longest word (negligible), so it does not
# push the inline-block wrapper wider than the table; `min-width: 100%`
# then forces the rendered note to fill the wrapper, which is exactly
# the table's content width.
.spicy_tt_note_div <- function(note, lead = .note_lead()) {
  # Escaped, with the lead-in in <em> (APA Manual 7 Section 7.14). Called
  # where the table is built, so the default is the lead-in of the note.
  note_html <- .note_markup(note, .spicy_html_escape, .spicy_html_em, lead)
  note_html <- .spicy_note_lines(note_html)
  paste0(
    "<div class=\"spicy-tt-note\" style=\"",
    "width: min-content; min-width: 100%; box-sizing: border-box; ",
    "padding: 0.5rem 0.5rem 0.2rem 0.5rem; ",
    .spicy_note_css(),
    "line-height: 1.25; ",
    "text-align: left;\">",
    note_html,
    "</div>"
  )
}

# Strip the rendered `<tfoot>` and wrap the table + note in the
# centering / inline-block pair. Operates on a rendered HTML string.
.spicy_tt_wrap_html <- function(html, note_div) {
  open_outer <- paste0(
    "<div class=\"spicy-tt-outer\" ",
    "style=\"text-align: center;\">"
  )
  open_inner <- paste0(
    "<div class=\"spicy-tt-wrap\" style=\"",
    "display: inline-block; max-width: 100%; ",
    "text-align: left; vertical-align: top;\">"
  )
  # 1. Drop the rendered tfoot. tinytable emits
  #    `<tfoot><tr><td colspan='N'>...</td></tr></tfoot>` as one piece
  #    (newlines inside survive as plain text); a perl multiline regex
  #    covers it.
  html <- sub("<tfoot>[\\s\\S]*?</tfoot>", "", html, perl = TRUE)
  # 2. Open the centering outer + inline-block inner wrapper just
  #    before `<table ...>`. We match the literal "<table " (with
  #    trailing space) so we hit the opening tag, not the closer.
  html <- sub(
    "<table ",
    paste0(open_outer, open_inner, "<table "),
    html,
    fixed = TRUE
  )
  # 3. Append the note div + close both wrappers right after
  #    `</table>`.
  sub(
    "</table>",
    paste0("</table>", note_div, "</div></div>"),
    html,
    fixed = TRUE
  )
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
