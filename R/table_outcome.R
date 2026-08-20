# table_outcome(): ONE continuous outcome, described across the levels
# of SEVERAL categorical variables, one block per variable.
#
# The inverse layout of `table_continuous()`. Where that function puts
# several outcomes in rows and one grouping in columns (or in a second
# stub column), this one fixes the outcome and stacks the groupings:
#
#   Descriptive statistics of Body mass index
#
#    Variable         |   M      SD    95% CI LL  95% CI UL    n      p
#    Overall          |  25.93   3.73    25.72      26.14     1188
#    Sex              |                                             .018
#      Female         |  25.69   3.78    25.39      25.98      616
#      Male           |  26.20   3.64    25.90      26.50      572
#
# Every number is produced by the machinery `table_continuous()` uses
# -- `.continuous_compute_one()` for the statistics,
# `run_group_test()` / `compute_effect_size()` for the block
# comparison, `.continuous_stat_cells()` for the strings -- so the two
# tables can never word the same cell differently. What is local to
# this file is the GEOMETRY: a one-column stub, a header row per
# block carrying the block's own statistics, indented level rows, and
# a marginal row above them all.

# Internal: the title of an outcome table, from the outcome's label.
#
# Single source for the console header and the caption every rendering
# engine sets, like `.continuous_title()` / `.categorical_title()`.
#
# It names the OUTCOME only (decision 32). The grouping variables ARE
# the rows, so a title listing them would repeat the stub, and a table
# of six blocks would have no title left.
.outcome_title <- function(outcome_label) {
  spicy_fmt("title_outcome", outcome_label)
}

# Internal: the label of the marginal row.
#
# `"Overall"`, not `"Total"` (decision 32bis), and the two are separate
# registry keys because they are separate things. `label_total` /
# `header_margin_total` is the word of a COUNT margin -- the column of
# `table_categorical()` where frequencies add up. This row is the whole
# analytic sample: a mean is recomputed on it and nothing is added, so
# calling it a total would be a reading error a translator would then
# carry into every language.
.outcome_overall_label <- function() {
  spicy_str("row_overall")
}

# Internal: the default Excel sheet name (decision 16 -- `excel_sheet =
# NULL` in the signature, resolved from the registry here, so the
# \usage line stays clean and the name can follow the table language).
.outcome_excel_sheet <- function(excel_sheet) {
  if (is.null(excel_sheet)) spicy_str("excel_sheet_outcome") else excel_sheet
}

# Internal: the two sentences an outcome table owes its reader.
#
# The first is the honest one gtsummary's equivalent does not print:
# the blocks are separate one-way comparisons and the table adjusts
# none of them for any other. It is only owed when a comparison is
# actually shown.
#
# The second says what the marginal row is, so nobody reads it as a
# total of the block below.
.outcome_structure_notes <- function(
  outcome_label,
  show_comparison,
  overall
) {
  c(
    if (isTRUE(show_comparison)) {
      spicy_fmt("note_outcome_blocks", outcome_label)
    },
    if (isTRUE(overall)) spicy_str("note_outcome_overall")
  )
}
