# Attach a table note to a flextable built by `table_categorical()` /
# `table_continuous()` as a footer line. Unlike the regression
# builders, these tables are NOT tagged with `spicy_note`: the shared
# `print.spicy_flextable()` post-processor that relocates the note out
# of the `<tfoot>` also injects header CSS written for a two-row
# header, which would erase the single header rule of the one-way
# categorical table. Keeping the native footer costs the note the
# out-of-grid placement and keeps the borders intact on every route
# (HTML, docx, console).
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
