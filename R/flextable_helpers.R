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
.spicy_gt_attach_note <- function(tbl, note) {
  if (is.null(note) || !nzchar(note)) {
    return(tbl)
  }
  attr(tbl, "spicy_note") <- note
  class(tbl) <- c("spicy_gt", class(tbl))
  tbl
}


# Escape a value for interpolation INSIDE a double-quoted CSS string,
# as in the `th[id="..."]` attribute selectors the gt branches build.
# Most of those ids are frozen ASCII keys, but the categorical group
# columns are named after the `by` levels, which are user data: a level
# holding a double quote closed the string early and gt's own sass
# compiler aborted the whole render ("unterminated attribute selector
# for id").
#
# CSS escapes with a backslash, like R, so the backslash goes first. A
# newline cannot appear literally in a CSS string; the six-hex-digit
# form needs no terminating space and so cannot swallow a following
# one.
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
