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
  x
}
