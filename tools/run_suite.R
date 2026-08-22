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

# Before the suite, not after: a stray non-ASCII character in R/ is
# found by a byte scan in a second, and a 40-minute run is too long to
# spend learning it at the end. See tools/ascii_sentinel.R for the
# allowlist and why the rule is an allowlist.
source("tools/ascii_sentinel.R")
sentinel <- ascii_sentinel_sites("R")
if (nrow(sentinel) > 0L) {
  cat(ascii_sentinel_report(sentinel), sep = "\n")
  cat(sprintf(
    "ASCII SENTINEL: %d disallowed character(s) in R/. Use the ASCII equivalent, or write the character as a \\uXXXX escape. Suite not run.\n",
    nrow(sentinel)
  ))
  quit(status = 1L)
}

res <- as.data.frame(devtools::test(reporter = "silent"))
csv <- file.path(out_dir, "suite_results.csv")
utils::write.csv(
  res[, c("file", "test", "passed", "failed", "error", "warning", "skipped")],
  csv,
  row.names = FALSE
)

# `failed` counts expectation failures; a test that ERRORS before its
# first expectation lands in the logical `error` column instead. The
# 2026-08 CI red slipped through precisely because this script only
# summed `failed` -- both must gate the verdict.
n_fail <- sum(res$failed)
n_err <- sum(res$error)
cat(sprintf(
  "FAIL: %d ERR: %d WARN: %d SKIP: %d PASS: %d (table: %s)\n",
  n_fail,
  n_err,
  sum(res$warning),
  sum(res$skipped),
  sum(res$passed),
  csv
))
bad <- res[res$failed > 0L | res$error, c("file", "test")]
if (nrow(bad) > 0L) {
  for (i in seq_len(nrow(bad))) {
    cat(sprintf("BROKEN: %s -- %s\n", bad$file[i], bad$test[i]))
  }
}
quit(status = if (n_fail > 0L || n_err > 0L) 1L else 0L)
