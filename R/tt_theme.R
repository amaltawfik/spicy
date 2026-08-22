# tinytable::theme_empty() clears ALL lazy slots of a tinytable --
# including `lazy_finalize`, where tinytable stores its OUTPUT-FORMAT
# hooks (e.g. the Typst finalizer that honours
# `options(tinytable_typst_multipage)`: without it a table taller than
# one page does not break, and its last rows can overprint). Wiping it
# silently disabled those document-level options for every spicy table
# (field report, dev/theme_empty_efface_les_finaliseurs.md). This bare
# variant strips only the APPEARANCE slots -- the actual intent of the
# five call sites -- and leaves the format finalizers alive. Every
# spicy tinytable path must use it instead of theme_empty().
.spicy_tt_bare <- function(x) {
  x@lazy_format <- list()
  x@lazy_style <- list()
  x@lazy_prepare <- list()
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
  tinytable::style_tt(x, finalize = function(tbl) {
    if (identical(tbl@output, "typst")) {
      tbl@table_string <- sub(
        "\\s*column-gutter: 5pt,",
        "",
        tbl@table_string
      )
      tbl@table_string <- .spicy_tt_note_typst(tbl@table_string)
    } else if (identical(tbl@output, "html")) {
      tbl@table_string <- .spicy_tt_note_tfoot(tbl@table_string)
    }
    tbl
  })
}


# ---- Table notes: subordination by size ----------------------------------
#
# tinytable puts the note in a structural slot of the rendered table
# (`table.footer()` in Typst, a bare `<tfoot>` cell in HTML) that no
# document-level rule reaches, so a spicy note came out at body size
# and body colour. A note is subordinate to the table it documents:
# every other spicy engine already says so typographically (gt 0.9em,
# Word 10pt against an 11pt body), and the typographic canon
# subordinates by SIZE, in black -- grey is a screen convention that
# fails on a dark background. Default: 0.9em, black, on the tinytable
# engine's Typst and HTML output.
#
# `options(spicy.note_style)`:
#   * unset / NULL -- the 0.9em default;
#   * "none"       -- no intervention at all; the note renders exactly
#                     as the receiving template styles it;
#   * any other string -- extra Typst `text()` arguments appended to
#                     the size (e.g. "fill: luma(89)" for a grey note).
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

# HTML: a note carries one disclosure per line, and HTML collapses the
# newlines that separate them into spaces -- the per-model standard-
# error attributions ran together into one sentence. Convert the line
# breaks to `<br>` and keep each continuation line's indentation with
# no-break spaces.
.spicy_note_html_lines <- function(x) {
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
          strrep("&nbsp;", nchar(lead)),
          substring(p, nchar(lead) + 1L)
        )
      }
      p
    },
    character(1),
    USE.NAMES = FALSE
  )
  paste(parts, collapse = "<br>")
}

# HTML: restyle the `<tfoot>` note cell tinytable renders (the route
# taken by the descriptive families; table_regression() and
# table_continuous_lm() lift the note out of the table grid instead,
# see `.spicy_tt_note_div()`).
.spicy_tt_note_tfoot <- function(s) {
  m <- regexpr("<tfoot>[\\s\\S]*?</tfoot>", s, perl = TRUE)
  if (m < 0L) {
    return(s)
  }
  css <- .spicy_note_css()
  block <- regmatches(s, m)
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
    text <- .spicy_note_html_lines(text)
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
# `table.cell(align: left, colspan: N, text([...])),`. Two repairs:
# the embedded newlines (Typst collapses them exactly as HTML does)
# become forced line breaks with their indentation kept as no-break
# spaces, and the whole note is wrapped at 0.9em. A note already
# styled through `tinytable::style_tt(i = "notes")` is left alone --
# an explicit user style wins.
.spicy_tt_note_typst <- function(s) {
  lines <- strsplit(s, "\n", fixed = TRUE)[[1L]]
  cell_open <- "^\\s*table\\.cell\\(align: left, colspan: [0-9]+, "
  starts <- grep(cell_open, lines)
  if (length(starts) == 0L) {
    return(s)
  }
  st <- .spicy_note_style()
  for (i0 in starts) {
    i1 <- i0
    while (i1 <= length(lines) && !grepl("\\]\\)\\),\\s*$", lines[i1])) {
      i1 <- i1 + 1L
    }
    if (i1 > length(lines)) {
      next
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
.spicy_tt_note_div <- function(note) {
  note_html <- .spicy_html_escape(note)
  # APA Manual 7 Section 7.14: the "Note." prefix is italicised. Wrap it
  # in <em> AFTER escaping (the prefix itself contains no special
  # chars, so the <em> is safe to insert). The substitution is anchored
  # to the start of the string; if a note doesn't begin with "Note."
  # (theoretically possible) the source string is unchanged.
  note_html <- sub(
    .note_prefix_pattern(),
    paste0("<em>", spicy_str("note_prefix_emphasis"), "</em>"),
    note_html
  )
  note_html <- .spicy_note_html_lines(note_html)
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
# centring / inline-block pair. Operates on a rendered HTML string.
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
# it on purpose. The note is package prose, not user data, and
# escaping it broke Typst compilation: the typst escape set covers
# `[` / `]`, so a note carrying markup such as `#text(8pt)[...]` came
# out as `\#text(8pt)\[...\]`, leaving `text()` without a body. Cell
# escaping still applies, so real CI brackets stay protected.
#
# LaTeX and Typst do their own escaping; this is a no-op there.
.spicy_tt_escape <- function(x) {
  tinytable::format_tt(
    x,
    i = c("colnames", "caption", "~groupi", "groupi", "groupj", "cells"),
    escape = TRUE
  )
}
