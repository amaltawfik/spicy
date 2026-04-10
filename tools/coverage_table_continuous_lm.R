if (!requireNamespace("covr", quietly = TRUE)) {
  stop("Install package 'covr' to run this script.", call. = FALSE)
}
if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install package 'devtools' to run this script.", call. = FALSE)
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Install package 'testthat' to run this script.", call. = FALSE)
}

suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))

source_file <- "R/table_continuous_lm.R"
test_files <- c(
  "tests/testthat/test-table_continuous_lm.R",
  "tools/coverage_table_continuous_lm_probes.R"
)

cov <- covr::file_coverage(source_file, test_files)
print(cov)

zero <- covr::zero_coverage(cov)

if (nrow(zero) == 0L) {
  cat("\nNo uncovered lines reported by covr.\n")
} else {
  zero <- unique(zero[, c("functions", "line")])
  zero <- zero[order(zero$functions, zero$line), , drop = FALSE]
  cat("\nUncovered lines:\n")
  print(zero, row.names = FALSE)
}
