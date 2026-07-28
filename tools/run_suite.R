# Canonical full-suite runner. ALWAYS use this instead of piping
# devtools::test() output through head/tail: a truncated pipe once
# swallowed the FAIL/WARN verdict and cost a 40-minute rerun
# (2026-07-28). The verdict and the per-test table are written to
# files, so no console handling can lose them.
#
#   Rscript tools/run_suite.R [out_dir]
#
# Writes <out_dir>/suite_results.csv (one row per test block) and
# prints the one-line verdict; exits non-zero when any test fails.
# out_dir defaults to tools/ itself; suite_results.csv is gitignored.

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1L) args[[1L]] else "tools"

res <- as.data.frame(devtools::test(reporter = "silent"))
csv <- file.path(out_dir, "suite_results.csv")
utils::write.csv(
  res[, c("file", "test", "passed", "failed", "warning", "skipped")],
  csv,
  row.names = FALSE
)

n_fail <- sum(res$failed)
cat(sprintf(
  "FAIL: %d WARN: %d SKIP: %d PASS: %d (table: %s)\n",
  n_fail,
  sum(res$warning),
  sum(res$skipped),
  sum(res$passed),
  csv
))
bad <- res[res$failed > 0L, c("file", "test")]
if (nrow(bad) > 0L) {
  for (i in seq_len(nrow(bad))) {
    cat(sprintf("FAILED: %s -- %s\n", bad$file[i], bad$test[i]))
  }
}
quit(status = if (n_fail > 0L) 1L else 0L)
