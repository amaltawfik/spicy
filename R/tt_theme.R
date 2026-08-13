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
  # Reported upstream; drop this when tinytable makes the gutter an
  # option of group_tt().
  tinytable::style_tt(x, finalize = function(tbl) {
    if (identical(tbl@output, "typst")) {
      tbl@table_string <- sub(
        "\\s*column-gutter: 5pt,",
        "",
        tbl@table_string
      )
    }
    tbl
  })
}
