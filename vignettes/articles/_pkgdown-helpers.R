# Strip background colours from `gt` tables so they render correctly
# under both the light and dark pkgdown themes. Wrap any
# `output = "gt"` result with `pkgdown_dark_gt()` before rendering.
#
# Lives beside its only consumers, the articles in this directory, and
# is sourced once per article via `source("_pkgdown-helpers.R")` so the
# rendered-table articles stay byte-identical. The leading underscore
# keeps pkgdown from treating the file as an article of its own.
pkgdown_dark_gt <- function(tab) {
  tab |>
    gt::opt_css(
      css = paste(
        ".gt_table, .gt_heading, .gt_col_headings, .gt_col_heading,",
        ".gt_column_spanner_outer, .gt_column_spanner, .gt_title,",
        ".gt_subtitle, .gt_sourcenotes, .gt_sourcenote {",
        "  background-color: transparent !important;",
        "  color: currentColor !important;",
        "}",
        sep = "\n"
      )
    )
}
