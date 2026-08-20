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
.spicy_ft_attach_note <- function(ft, note) {
  if (is.null(note) || !nzchar(note)) {
    return(ft)
  }
  flextable::add_footer_lines(
    ft,
    top = FALSE,
    values = gsub("\n", " ", note, fixed = TRUE)
  )
}

# Attach a table note to a gt table built by `table_categorical()` /
# `table_continuous()`. NOT via `gt::tab_source_note()`: its `<tfoot>`
# colspan cell widens the table in narrow viewports (see the comment
# in `output_gt()`). The `spicy_gt` tag routes the note through
# `print.spicy_gt()` / `knit_print.spicy_gt()`, which inject it as a
# `<div>` outside the table in HTML and fall back to the native source
# note for non-HTML knit targets.
# The note is attached TWICE, on purpose -- the same reasoning
# `output_gt()` records for the regression family.
#
# 1. `gt::tab_source_note()` puts it on the gt object itself, so it
#    survives every route to a rendered table: `gt::gtsave()`,
#    `gt::as_raw_html()`, `as_latex()`, `as_word()`, a non-interactive
#    `print()`. Without it those deliverables shipped a TITLED table
#    stripped of the disclosure the console prints -- which missing
#    values were removed, which test each block ran, that the blocks
#    are not adjusted for one another. The twin of the title hole, on
#    the same engine.
# 2. The `spicy_gt` tag routes it through `print.spicy_gt()` /
#    `knit_print.spicy_gt()`, which inject it as a `<div>` OUTSIDE the
#    table in HTML -- gt's own `<tfoot>` colspan cell widens the table
#    in narrow viewports. Those methods drop the native note when they
#    take over, so the two never print together.
.spicy_gt_attach_note <- function(tbl, note) {
  if (is.null(note) || !nzchar(note)) {
    return(tbl)
  }
  # One line, like the regression family: `.spicy_gt_drop_source_note()`
  # matches this row on its TEXT to remove it from the HTML path, and a
  # note carrying a newline would not match itself.
  tbl <- gt::tab_source_note(
    tbl,
    source_note = gsub("\n", " ", note, fixed = TRUE)
  )
  attr(tbl, "spicy_note") <- note
  class(tbl) <- c("spicy_gt", class(tbl))
  tbl
}


# Set the title every gt route prints above its table: the same title
# the console announces, laid out the APA way.
#
# APA Manual 7 Section 7.10-Section 7.11: caption is flush-left. gt's
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
# `title = NULL` / "" returns the table untouched.
.spicy_gt_apa_title <- function(tbl, title) {
  if (is.null(title) || !nzchar(title)) {
    return(tbl)
  }
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
# it looks like: sass re-serialises our `\00000A` to the short `\ab`
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

# Set the caption every HTML (flextable) route prints above its
# table: the same title the console announces. Word gets its own
# auto-numbered caption from `.spicy_ft_word_caption()` above -- that
# route replaces this one, and the numbering it adds is a Word field
# with no HTML equivalent.
#
# Two non-obvious knobs in flextable's `set_caption()`:
#
#   * `align_with_table = FALSE`: flextable's default behaviour
#     overrides the caller's `text.align` with the underlying table's
#     alignment (via `process_caption_fp_par()`); the table is centred
#     for HTML output, so the caption would be too. FALSE keeps the
#     APA flush-left caption (APA Manual 7 Section 7.10-7.11).
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
