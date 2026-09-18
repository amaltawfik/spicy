# Entry point of `R CMD check`. `devtools::test()` does not read this file.
#
# Two tiers, chosen by NOT_CRAN, the devtools / testthat convention that
# CRAN never sets:
#
# * NOT_CRAN = "true" (devtools::check(), GitHub Actions): the full suite,
#   18,000+ expectations -- numeric oracles, engine parity, per-class
#   frames, branch coverage.
# * otherwise (CRAN): the public contract of each exported family, a few
#   minutes of tests. The full suite needs about 20 minutes even with the
#   Stan fixtures skipped, which is not a cost to put on the check farm.
#
# The CRAN tier is an ALLOW-list on purpose. The suite doubled during the
# 0.13 cycle without anyone seeing the CRAN check grow with it. With an
# allow-list, a new test file cannot lengthen that check unless a line is
# added here. A new exported function adds its main test file below.

library(testthat)
library(spicy)

cran_tier <- c(
  # tabulation and association measures
  "freq", "freq_print", "cross_tab", "assoc", "cramer_v",
  # variable inspection and row-wise helpers
  "varlist", "code_book", "label_from_names", "copy_clipboard",
  "mean_n", "sum_n", "count_n", "user_na",
  # summary tables, and their survey-design twins
  "table_categorical", "table_continuous", "table_continuous_lm",
  "table_outcome", "table_categorical_svy", "table_continuous_svy",
  # regression tables
  "table_regression", "table_regression_models", "regression_uv",
  "regression_structured", "regression_broom", "inline",
  # styles and languages
  "spicy_style", "i18n"
)

if (isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))) {
  test_check("spicy")
} else {
  test_check(
    "spicy",
    filter = paste0("^(", paste(cran_tier, collapse = "|"), ")$")
  )
}
