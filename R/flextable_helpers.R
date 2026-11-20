# Attach a table note to a flextable built by `table_categorical()` /
# `table_continuous()` as a footer line. Unlike the regression
# builders, these tables are NOT tagged with `spicy_note`: the shared
# `print.spicy_flextable()` post-processor that relocates the note out
# of the `<tfoot>` also injects header CSS written for the regression
# header, which replaces the rule under a spanner with a trimmed
# booktabs pseudo-rule -- the descriptive families draw that rule
# under their own group spanners and expect it full width. Keeping the
# native footer costs the note the out-of-grid placement and keeps the
# borders intact on every route (HTML, docx, console).
#
# The leading "Note." is set in italics and the rest in regular type
# (APA Manual 7 Section 7.14), as `output_flextable()` sets the
# regression note. `as_i()` sets only the italic: every other property
# of the two chunks is left to the footer's own formatting, since these
# builders force no font. A note that does not open with the prefix (a
# `note` the user typed) is one plain chunk. Each line of the note is a
# footer line of its own (`.note_lines()`).
.spicy_ft_attach_note <- function(ft, note) {
  if (is.null(note) || !nzchar(note)) {
    return(ft)
  }
  # The numbers of the note take the minus sign of the style in force.
  note_signed <- .apply_minus_sign(note, where = "note")
  lines <- .note_lines(note_signed)
  note_split <- .note_prefix_split(lines[[1L]])
  ft <- flextable::add_footer_lines(
    ft,
    top = FALSE,
    values = if (is.null(note_split)) {
      lines[[1L]]
    } else {
      flextable::as_paragraph(
        flextable::as_i(note_split$marker),
        note_split$rest
      )
    }
  )
  if (length(lines) > 1L) {
    ft <- flextable::add_footer_lines(ft, top = FALSE, values = lines[-1L])
  }
  ft
}

# HTML: the frame of every flextable the package returns. flextable
# writes the table in a `<div class="tabwid">` with no width rule, so
# in an R Markdown page 420px wide a two-model regression table (455px)
# ran past its 375px frame and widened the page. flextable's own scroll
# box (`opts_html = list(scroll = list())`, which writes
# `overflow-x:auto;width:100%;` on that div) makes it scroll within its
# frame, as gt and tinytable tables do. The layout is kept as built; the
# option touches HTML only. The option came with flextable 0.8.3 and
# DESCRIPTION pins no version: where the call fails, the table is
# returned as it was.
.spicy_ft_html_frame <- function(ft) {
  # Forced first: an error of the builder is not this call's to catch.
  force(ft)
  tryCatch(
    flextable::set_table_properties(
      ft,
      layout = ft$properties$layout,
      opts_html = list(scroll = list())
    ),
    error = function(e) ft
  )
}

# Every gt table of the package is built here: `gt::gt()` with an id of
# its own, and the rules of its source notes and of its header in HTML
# (`.spicy_gt_note_css()`, `.spicy_gt_header_css()`), which need that id
# to stay within the table.
#
# The id is a counter of the session, not gt's random one, so that
# building a table leaves the random number stream alone. The name of
# the session's temporary directory makes it unique across sessions:
# a page that mixes tables from two sessions (a chunk replayed from the
# knitr cache next to one run again) would otherwise give two tables
# the same id, and each the rules of the other.
.spicy_gt_ids <- new.env(parent = emptyenv())
.spicy_gt_ids$n <- 0L

.spicy_gt <- function(data, css = NULL) {
  if (is.null(.spicy_gt_ids$session)) {
    .spicy_gt_ids$session <- gsub("[^A-Za-z0-9]", "", basename(tempdir()))
  }
  .spicy_gt_ids$n <- .spicy_gt_ids$n + 1L
  id <- paste0("spicy-gt-", .spicy_gt_ids$session, "-", .spicy_gt_ids$n)
  # `css`: further rules, each scoped to the table by its id.
  rules <- paste(.spicy_gt_note_css(id), .spicy_gt_header_css(id), sep = "\n")
  if (length(css)) {
    rules <- paste(c(rules, paste0("#", id, " ", css)), collapse = "\n")
  }
  # Quarto re-parses an HTML table that does not opt out: it gave a gt
  # table of the package the width of the page, columns of equal widths,
  # Bootstrap's stripes, and kept one of the two alignments of a header
  # cell. Opted out, the table renders in Quarto as in R Markdown.
  # `knit_print.spicy_gt()` opts back in for a knit to another format,
  # where Quarto reads gt's HTML to write the table (Typst).
  tbl <- gt::tab_options(
    gt::gt(data, id = id),
    quarto.disable_processing = TRUE
  )
  gt::opt_css(tbl, rules)
}

# A source note is a `<td colspan="N">` in the `<tfoot>` gt writes, and a
# spanning cell gives its max-content width to the columns it spans: a
# note longer than the body stretched the table, in every route that
# writes HTML (`gtsave()` to .html or .png, `as_raw_html()`, the display
# methods). A cell with a set width gives no more than its longest word,
# so the table keeps the width of its body and the note wraps at it, as
# the note of a tinytable does (dev/decisions/2026-11-15-tinytable-note-html.md).
# The other rules set the note with an interline of 1.25 and the lines of
# a note close together, as a single block; gt sets its size, 0.9 of the
# table's. Its text starts 5px in, where the text of a body cell starts
# (the side padding gt gives it, `source_notes.padding.horizontal`). The
# rules are HTML only: gt writes `opt_css()` into no other output.
#
# gt compiles `opt_css()` with Sass, its options set as Sass variables:
# the vertical padding of spicy applies only while
# `source_notes.padding` keeps gt's default (4px), so that a value the
# user sets with `gt::tab_options()` wins.
.spicy_gt_note_css <- function(id) {
  paste0(
    "#",
    id,
    " .gt_sourcenote { width: 1px; line-height: 1.25; }\n",
    "@if $source_notes_padding == 4px {\n",
    "#",
    id,
    " .gt_sourcenote { padding-top: 0; padding-bottom: 0; }\n",
    "#",
    id,
    " .gt_sourcenotes:first-child .gt_sourcenote { padding-top: 0.5rem; }\n",
    "#",
    id,
    " .gt_sourcenotes:last-child .gt_sourcenote { padding-bottom: 0.2rem; }\n",
    "}"
  )
}

# The rules of the header, in every route that writes HTML, as the note's
# are. gt's theme draws a line under each spanner row and under the
# label of each spanner (B, p, Variable); spicy keeps one 1px #333333
# rule under the column labels (the body cells lose their transparent
# top border, which would otherwise win the collapsed border), the CI
# bracket (the inline top border of LL and UL), and a rule under each
# model spanner, inset 0.5em on each side so that the rules of adjacent
# models stay apart (booktabs' `\cmidrule(lr)`). `!important` wins over
# gt's own rules and over the inline styles of `tab_style()`.
#
# `as_raw_html(inline_css = TRUE)` inlines the rules into style attributes
# and drops what it cannot inline: the model rule is therefore the bottom
# border of the label's `<div>` inside the spanner cell, not a
# pseudo-element of the cell. The cell has 4px of side padding (none on
# the right of the last cell of the row), so the margins of the `<div>`
# give the 0.5em inset; in the last cell, a left padding keeps the label
# where gt sets it, 2px right of the centre. One pixel of the bottom
# padding makes room for the rule. gt gives the first cell of a spanner
# row no left padding, which set "Variable" flush with the edge of the
# table: it gets the 5px of a body cell, so the label of the first column
# starts where the labels under it start. The widths of zero carry a unit: Quarto
# writes them into the Typst of the table, where a bare 0 is no length
# and stopped the compilation.
.spicy_gt_header_css <- function(id) {
  s <- paste0("#", id, " ")
  model <- paste0(s, "th[id^=\"model_span_\"]")
  paste0(
    s,
    ".gt_col_headings.gt_spanner_row, ",
    s,
    ".gt_spanner_row { border-bottom-style: none !important; ",
    "border-bottom-width: 0px !important; }\n",
    s,
    ".gt_col_heading { border-bottom: 1px solid #333333 !important; }\n",
    s,
    ".gt_table tbody .gt_row { border-top-width: 0px !important; }\n",
    s,
    ".gt_column_spanner { border-bottom-width: 0px !important; ",
    "border-bottom-color: transparent !important; }\n",
    model,
    " > .gt_column_spanner { display: block !important; ",
    "width: auto !important; margin-left: calc(0.5em - 4px); ",
    "margin-right: calc(0.5em - 4px); padding-bottom: 4px !important; ",
    "border-bottom: 1px solid #333333 !important; }\n",
    model,
    ":last-child > .gt_column_spanner { margin-right: 0.5em; ",
    "padding-left: 4px; }\n",
    s,
    ".gt_column_spanner_outer:first-child { padding-left: 5px !important; }"
  )
}

# Attach a table note to a gt table built by `table_categorical()` /
# `table_continuous()`, as the native source notes of the table
# (`.spicy_gt_add_source_notes()`): every route to a rendered table
# (`gt::gtsave()`, `gt::as_raw_html()`, `as_latex()`, `as_word()`,
# `print()`, `knit_print()`) prints it, kept out of the width of the
# table in HTML by the rules `.spicy_gt()` set. The note is also kept in
# an attribute, for the knit to a non-HTML target of a table that lost
# its source notes (`knit_print.spicy_gt()`).
#
# `stub` and `decimal_mark` are the label columns and the mark of the
# table, for the minus sign of its LaTeX (`.spicy_gt_minus_attr()`).
#
# A table without a note takes the tag too (section 19 of
# dev/decisions/2026-10-23-sorties-publiables-fr.md), as every
# `table_regression()` gt table does: `knit_print.spicy_gt()` is where the
# LaTeX of a gt table gets its minus sign and its Unicode mapping, and a
# table of the family must not lose them because it has nothing to say.
.spicy_gt_attach_note <- function(tbl, note, stub = 1L, decimal_mark = ".") {
  note_signed <- .apply_minus_sign(
    note,
    where = "note",
    decimal_mark = decimal_mark
  )
  if (!is.null(note_signed) && nzchar(note_signed)) {
    tbl <- .spicy_gt_add_source_notes(tbl, note_signed)
    attr(tbl, "spicy_note") <- note_signed
    # The lead-in the note was built with, for the display methods, which
    # run after the table function returns (`.note_lead_keys()`, R/i18n.R).
    attr(tbl, "spicy_note_lead") <- .note_lead()
  }
  tbl <- .spicy_gt_minus_attr(tbl, stub, decimal_mark)
  class(tbl) <- c("spicy_gt", class(tbl))
  tbl
}

# The sign of the negative numbers of a gt table
# (`spicy_style(minus_sign = )`), decided by the style in force where the
# table is built. In HTML and Word, a formatter of the cells after the
# label columns (`.spicy_gt_minus_cells()`). In LaTeX, kept with the
# table, as the lead-in of its note is, for `knit_print.spicy_gt()`,
# which runs later: with the label columns that open the table (`stub`)
# and its `decimal_mark`, which the rewrite of its body needs
# (`.spicy_gt_latex_minus()`).
.spicy_gt_minus_attr <- function(tbl, stub = 1L, decimal_mark = ".") {
  sign <- .style_minus_sign()
  attr(tbl, "spicy_minus_sign") <- list(
    sign = sign,
    stub = stub,
    decimal_mark = decimal_mark
  )
  if (identical(sign, "typographic")) {
    tbl <- .spicy_gt_minus_cells(tbl, stub, decimal_mark)
  }
  tbl
}

# The minus sign in the cells of a gt table after its `stub` label
# columns, for the HTML, Word and RTF gt writes (`.apply_minus_sign()`):
# a formatter per output, which gt runs when it renders the table. A cell
# with no negative number, and every cell of the other outputs, gets NA,
# which leaves it to gt's own rendering, as before: the LaTeX keeps the
# hyphen for `.spicy_gt_latex_minus()`. A rewritten cell is escaped as
# gt escapes a plain one (HTML; RTF, where gt writes a formatter's text
# as it is) or left as text (Word, which gt escapes when it writes the
# XML).
.spicy_gt_minus_cells <- function(tbl, stub = 1L, decimal_mark = ".") {
  data <- tbl[["_data"]]
  cols <- names(data)[setdiff(seq_along(data), seq_len(stub))]
  cols <- cols[vapply(data[cols], is.character, logical(1))]
  if (length(cols) == 0L) {
    return(tbl)
  }
  sign <- function(escape) {
    function(x) {
      y <- .apply_minus_sign(x, "typographic", "cell", decimal_mark)
      out <- rep(NA_character_, length(x))
      hit <- !is.na(x) & y != x
      out[hit] <- escape(y[hit])
      out
    }
  }
  none <- function(x) rep(NA_character_, length(x))
  gt::fmt(
    tbl,
    columns = tidyselect::all_of(cols),
    fns = list(
      html = sign(.spicy_html_escape),
      word = sign(identity),
      rtf = sign(.spicy_rtf_escape),
      latex = none,
      default = none
    )
  )
}

# RTF text, escaped as gt escapes a plain cell: the backslash and the
# braces as hex escapes, a non-ASCII character as `\uN` (N signed on 16
# bits) closed by a space. gt's paragraphs set `\uc0`, so no fallback
# character follows.
.spicy_rtf_escape <- function(s) {
  s <- gsub("\\", "\\'5c", s, fixed = TRUE)
  s <- gsub("{", "\\'7b", s, fixed = TRUE)
  s <- gsub("}", "\\'7d", s, fixed = TRUE)
  vapply(
    s,
    function(x) {
      cp <- utf8ToInt(enc2utf8(x))
      ch <- vapply(cp, intToUtf8, character(1))
      hi <- cp > 127L
      ch[hi] <- paste0(
        "\\u",
        ifelse(cp[hi] > 32767L, cp[hi] - 65536L, cp[hi]),
        " "
      )
      paste(ch, collapse = "")
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# The note as native gt source notes, one per line of the note
# (`.note_lines()`): the general note, then the legend of the stars, each
# a row of its own in every output gt writes.
.spicy_gt_add_source_notes <- function(tbl, note, lead = .note_lead()) {
  for (line in .note_lines(note)) {
    tbl <- gt::tab_source_note(
      tbl,
      source_note = .spicy_gt_note_md(line, lead)
    )
  }
  tbl
}

# One line of a note as a native gt source note, in markdown: `md()` is
# the one text form gt renders in every output it writes (HTML, LaTeX,
# RTF, Word), so the lead-in `lead` is set in italics there (APA Manual 7
# Section 7.14). Every ASCII punctuation
# character is backslash-escaped, which CommonMark reads as that
# character, so the rest prints exactly as typed: the stars of a
# significance legend, the underscore of a variable name.
.spicy_gt_note_md <- function(note, lead = .note_lead()) {
  escape <- function(x) {
    gsub(
      "([\\x21-\\x2F\\x3A-\\x40\\x5B-\\x60\\x7B-\\x7E])",
      "\\\\\\1",
      x,
      perl = TRUE
    )
  }
  gt::md(.note_markdown(note, escape, lead))
}


# Set the title every gt route prints above its table: the same title
# the console announces, laid out the APA way.
#
# APA Manual 7 Section 7.10 (p. 200): the table number is flush left,
# and the italic title sits below it (Section 7.11, p. 201). gt's
# default centers the title; override via `cell_text(align = "left")`
# on `cells_title("title")`. Also drop the title's bottom border --
# the spanner row below already carries the outer top rule via
# `gt_columns_top_border`, so keeping the title's `gt_bottom_border`
# would render a redundant double line.
#
# Single source for `table_regression()` and the descriptive families,
# like `.spicy_ft_word_caption()` / `.spicy_ft_html_caption()` beside
# it: a table that announces itself on screen must announce itself in
# every engine, and the two layouts must not drift apart.
#
# The title is set at the size of the table text (`100%`), as in APA
# tables and as `table_regression()` already set it; the descriptive
# families kept gt's default `125%`. On the gt object, so every route
# (print, knit_print, gtsave, as_raw_html) carries it; the LaTeX route
# translates it in `knit_print.spicy_gt()`.
#
# `title = NULL` / "" returns the table untouched.
.spicy_gt_apa_title <- function(tbl, title) {
  if (is.null(title) || !nzchar(title)) {
    return(tbl)
  }
  tbl <- gt::tab_options(tbl, heading.title.font.size = "100%")
  tbl <- gt::tab_header(tbl, title = title)
  gt::tab_style(
    tbl,
    style = list(
      gt::cell_text(align = "left"),
      gt::cell_borders(
        sides = "bottom",
        color = "transparent",
        weight = gt::px(0)
      )
    ),
    locations = gt::cells_title(groups = "title")
  )
}

# The gt column id of every column KEY, named by the key.
#
# `gt::gt()` reads the data frame's column NAMES as the ids it writes
# into the rendered DOM, and in four of the families those names carry
# user data: a `by` level ("Q\"x_n"), a marginal mean ("M (Q\"x)"), a
# model name ("M\"1: B"). gt escapes the id where it lands in `id="..."`
# on the `<th>`, but the `headers="..."` attribute it writes on every
# body cell takes the name RAW. A level holding a double quote closes
# that attribute early and the remainder is re-parsed as bare
# attributes: `M (Q"x)` renders as `headers="M (Q" x)`. The same raw id
# is interpolated into the `th[id="%s"]` CSS selectors these branches
# build, where a double quote aborted sass ("unterminated attribute
# selector for id") and took the whole render down.
#
# So the KEY and the gt id part company here, exactly as the key and the
# header already have: the key stays the public column name
# `output = "data.frame"` publishes and `col_meta` is indexed by, the id
# is DOM state. Nothing a reader sees moves -- the visible text is
# carried by the spanner / column label above, which gt escapes
# correctly.
#
# Only the characters that cannot survive an HTML attribute value or a
# CSS string are replaced: the double quote, the two angle brackets, the
# backslash (CSS's own escape) and the control characters. Every other
# name is returned untouched, so a table of ordinary names renders byte
# for byte as it did before. `make.unique()` breaks a tie the
# replacement could create ("a\"b" and "a<b" both land as "a_b"); the
# keys are data frame column names, so a set with no collision comes
# back exactly as it went in.
# Second pass, added for n. 213 / n. 218: gt does not write our id into
# the DOM as we hand it over. `gt:::valid_html_id()` collapses every run
# of WHITESPACE to a single "-", which can RE-MERGE two ids make.unique()
# has just declared distinct. Two `by` levels "A B" and "A-B" both land on
# `A-B_n`: the two `<th>` share one DOM id and every body cell's
# `headers="A-B_n"` then points at both, so a screen reader announces the
# wrong group and a `th[id=]` rule styles two columns at once. (In the
# spanner position gt escapes the id instead of normalizing it, so
# spanners never collide this way -- measured on gt 1.3.0.)
#
# The tie is broken on a SHADOW of the id -- the form gt will actually
# emit -- and the suffix make.unique() would have given is transplanted
# onto the real id. A set whose shadows are already distinct comes back
# untouched, so an ordinary table (including every frozen key with a
# space in it, "95% CI LL" and friends) renders byte for byte as before.
# One pass converges: the suffix carries no whitespace, so the suffixed
# id's own shadow is exactly the unique shadow it was given.
.gt_safe_ids <- function(keys) {
  ids <- make.unique(gsub("[\"<>\\\\[:cntrl:]]", "_", keys), sep = "_")
  shadow <- gsub("[[:space:]]+", "-", ids)
  if (anyDuplicated(shadow) > 0L) {
    unique_shadow <- make.unique(shadow, sep = "_")
    moved <- unique_shadow != shadow
    ids[moved] <- paste0(
      ids[moved],
      substring(unique_shadow[moved], nchar(shadow[moved]) + 1L)
    )
  }
  stats::setNames(ids, keys)
}

# Escape a value for interpolation INSIDE a double-quoted CSS string,
# as in the `th[id="..."]` attribute selectors the gt branches build.
# Most of those ids are frozen ASCII keys, but the categorical group
# columns are named after the `by` levels, which are user data: a level
# holding a double quote closed the string early and gt's own sass
# compiler aborted the whole render ("unterminated attribute selector
# for id").
#
# CSS escapes with a backslash, like R, so the backslash goes first.
#
# The newline is escaped for SYNTAX only -- a literal newline cannot
# appear in a CSS string -- and not because the rule would then land.
# It does not, either way, and the six-hex-digit form is not the shield
# it looks like: sass re-serializes our `\00000A` to the short `\ab`
# form, which swallows the character that follows and compiles to
# `th[id="a<U+00AB>_n"]`. Measured.
#
# The target id is not the level in the first place. gt passes every id
# through `gt:::valid_html_id()`, which prefixes "a" to anything not
# starting `[A-z]` and then collapses each run of WHITESPACE to a single
# "-". The newline is not special to it: measured on gt 1.3.0, a level
# "a b" lands as `Upper-secondary_n` against our `th[id="Upper
# secondary_n"]` exactly as "a\nb" does, and an accented level lands as
# `aété_n` against `th[id="été_n"]`. So an ordinary SPACE in a `by`
# level already costs this rule -- the newline is one case of a general
# mismatch, not a corner. Mirroring the mapping means copying an
# unexported internal, `[A-z]` quirk included, into a selector that
# raises nothing when it stops matching; the row's border is drawn by
# gt's own `tab_style()` as well, so what is lost is the override that
# carries it into the viewers where gt's inline styles do not survive.
# Left to a decision, recorded in the register.
#
# What the escape does prevent is the stylesheet failing to parse, which
# for the double quote took the whole render down.
.css_escape_string <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x <- gsub("\n", "\\00000A", x, fixed = TRUE)
  gsub("\r", "\\00000D", x, fixed = TRUE)
}

# Set the caption every Word (.docx) route writes above its table: the
# APA auto-numbered "Table 1: <title>" paragraph. `run_autonum()`
# writes a Word SEQ field, so the numbers renumber themselves when the
# document gains a table and can be cross-referenced by bookmark; the
# paragraph lands in the docx "Table Caption" style.
#
# Single source for `table_regression()` and the descriptive families
# -- a table that announces itself on screen must announce itself in
# the document. `props` is the caption's text formatting: pass it when
# the builder pins a font for the whole table (table_regression() does,
# so its caption must match), leave NULL to inherit the table default
# (the descriptive builders force no font).
#
# `title = NULL` / "" returns the table untouched.
.spicy_ft_word_caption <- function(ft, title, props = NULL) {
  # Forced first, as in `.spicy_ft_html_caption()` below: `ft` may be
  # the builder call itself, which carries the flextable availability
  # guard, and that guard must fire before the officer check and before
  # anything here touches flextable.
  force(ft)
  if (is.null(title) || !nzchar(title)) {
    return(ft)
  }
  if (!spicy_pkg_available("officer")) {
    spicy_abort(
      c(
        "Output `\"word\"` requires the 'officer' package.",
        "i" = "Install it with `install.packages(\"officer\")`."
      ),
      class = "spicy_missing_pkg"
    )
  }
  chunk <- if (is.null(props)) {
    flextable::as_chunk(title)
  } else {
    flextable::as_chunk(title, props = props)
  }
  flextable::set_caption(
    ft,
    caption = flextable::as_paragraph(chunk),
    autonum = officer::run_autonum(
      seq_id = "tab",
      pre_label = "Table ",
      post_label = ": "
    ),
    align_with_table = FALSE,
    fp_p = officer::fp_par(text.align = "left", padding.bottom = 6)
  )
}

# The document a Word route writes into: the user's `word_template`, or
# officer's stock document. One helper for every table function that
# writes Word, so the check on the path and its message are the same.
.spicy_word_doc <- function(word_template = NULL) {
  if (is.null(word_template) || !nzchar(word_template)) {
    return(officer::read_docx())
  }
  if (!file.exists(word_template)) {
    spicy_abort(
      sprintf("`word_template` file does not exist: %s", word_template),
      class = "spicy_invalid_input"
    )
  }
  officer::read_docx(path = word_template)
}

# Write the flextable of a descriptive table to `word_path`: through
# `save_as_docx()` as before without a template, appended to the body
# of the template otherwise.
.spicy_save_docx <- function(ft, word_path, word_template = NULL) {
  if (is.null(word_template) || !nzchar(word_template)) {
    flextable::save_as_docx(ft, path = word_path)
  } else {
    doc <- flextable::body_add_flextable(
      .spicy_word_doc(word_template),
      value = ft,
      align = "center"
    )
    print(doc, target = word_path)
  }
  invisible(word_path)
}

# Set the caption every HTML (flextable) route prints above its
# table: the same title the console announces. Word gets its own
# auto-numbered caption from `.spicy_ft_word_caption()` above -- that
# route replaces this one, and the numbering it adds is a Word field
# with no HTML equivalent.
#
# Two non-obvious knobs in flextable's `set_caption()`:
#
#   * `align_with_table = FALSE`: flextable's default behavior
#     overrides the caller's `text.align` with the underlying table's
#     alignment (via `process_caption_fp_par()`); the table is centered
#     for HTML output, so the caption would be too. FALSE keeps the
#     APA flush-left caption (APA Manual 7 Sections 7.10-7.11,
#     pp. 200-201).
#   * `padding.bottom`: the rendered HTML caption otherwise has
#     `padding-bottom: 0pt` and sits flush against the table's top
#     border. 6pt gives a comfortable APA-style gap.
#
# `props` is the caption's text formatting: pass it when the builder
# pins a font for the whole table (`table_regression()` does, so its
# caption must match), leave NULL to inherit the table default (the
# descriptive builders force no font). `flextable::font(part = "all")`
# does NOT reach the caption, hence the explicit chunk properties.
#
# `title = NULL` / "" returns the table untouched.
.spicy_ft_html_caption <- function(ft, title, props = NULL) {
  # Forced first: `ft` may be the builder call itself, which carries the
  # flextable availability guard. Left lazy, `flextable::as_chunk()`
  # below would run first, and a user without flextable would get R's
  # raw loadNamespace() error instead of spicy's classed, actionable one.
  force(ft)
  if (is.null(title) || !nzchar(title)) {
    return(ft)
  }
  chunk <- if (is.null(props)) {
    flextable::as_chunk(title)
  } else {
    flextable::as_chunk(title, props = props)
  }
  flextable::set_caption(
    ft,
    caption = flextable::as_paragraph(chunk),
    align_with_table = FALSE,
    fp_p = officer::fp_par(text.align = "left", padding.bottom = 6)
  )
}

# Build a flextable/officer-compatible border object without forcing
# a hard dependency on officer for plain flextable output.
spicy_fp_border <- function(color = "black", width = 1, style = "solid") {
  structure(
    list(
      width = width,
      color = color,
      style = style
    ),
    class = "fp_border"
  )
}
