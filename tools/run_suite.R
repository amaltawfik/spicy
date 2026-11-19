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
# out_dir defaults to tools/ itself (created if missing, validated
# before the run); suite_results.csv is gitignored.

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1L) args[[1L]] else "tools"

# The destination is validated HERE, before a single test runs. The CSV
# is written at the very end of a 40-minute suite, so an out_dir that is
# an existing FILE, a path that cannot be created, or a directory that
# cannot be written surfaced only as a write.csv() error after the whole
# run -- the verdict was computed and then thrown away (register n. 201).
# Same rule as the ASCII sentinel below: fail fast, and say why.
.suite_abort <- function(...) {
  cat(sprintf(...))
  quit(status = 1L)
}
if (!nzchar(out_dir)) {
  .suite_abort(
    "OUT DIR: the output directory is an empty string. Pass a writable directory as the first argument. Suite not run.\n"
  )
}
# A file-looking argument is a caller who meant the CSV itself: creating
# a directory literally named 'results.csv' would honor the letter of the
# request and betray its intent (the n. 201 incident passed exactly this).
if (grepl("\\.(csv|log|txt|md)$", out_dir, ignore.case = TRUE)) {
  .suite_abort(
    "OUT DIR: '%s' looks like a file name. The argument is the DIRECTORY that will receive suite_results.csv. Suite not run.\n",
    out_dir
  )
}
if (!dir.exists(out_dir)) {
  if (file.exists(out_dir)) {
    .suite_abort(
      "OUT DIR: '%s' is a FILE, not a directory. Pass a writable directory as the first argument. Suite not run.\n",
      out_dir
    )
  }
  tryCatch(
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE),
    error = function(e) NULL
  )
  if (!dir.exists(out_dir)) {
    .suite_abort(
      "OUT DIR: '%s' does not exist and could not be created. Pass a writable directory as the first argument. Suite not run.\n",
      out_dir
    )
  }
}
# Probe the write instead of asking file.access(): on Windows its mode = 2
# answer is documented as unreliable, and a lying "writable" here is
# exactly the 40 minutes this check exists to protect.
.probe <- file.path(out_dir, sprintf(".suite_write_probe_%d", Sys.getpid()))
.probe_ok <- tryCatch(
  suppressWarnings(file.create(.probe)),
  error = function(e) FALSE
)
if (!isTRUE(.probe_ok)) {
  .suite_abort(
    "OUT DIR: '%s' is not writable. Pass a writable directory as the first argument. Suite not run.\n",
    out_dir
  )
}
unlink(.probe)

# Before the suite, not after: a stray non-ASCII character in R/, or a
# raw control byte, is found by a byte scan in a second, and a
# 40-minute run is too long to spend learning it at the end. See
# tools/ascii_sentinel.R for the allowlist, why the rule is an
# allowlist above 0x7F, and why there is none below 0x20.
source("tools/ascii_sentinel.R")
sentinel <- ascii_sentinel_sites("R")
if (nrow(sentinel) > 0L) {
  cat(ascii_sentinel_report(sentinel), sep = "\n")
  cat(sprintf(
    "BYTE SENTINEL: %d disallowed character(s) in R/. Use the ASCII equivalent, or write the character as a \\uXXXX escape. Suite not run.\n",
    nrow(sentinel)
  ))
  quit(status = 1L)
}

# The package is loaded WITHOUT its test helpers and testthat sources
# them into the test environment, as R CMD check and covr do.
# devtools::test() and a plain load_all() source them into the global
# environment instead: a test that evaluated fixture code in globalenv()
# passed here and failed on the five CI platforms (2026-11-19). The
# environment variables are the ones devtools::test() sets.
Sys.setenv(
  NOT_CRAN = Sys.getenv("NOT_CRAN", "true"),
  R_BROWSER = "false",
  R_PDFVIEWER = "false",
  R_TESTS = ""
)
pkgload::load_all(".", helpers = FALSE, quiet = TRUE)
res <- as.data.frame(testthat::test_local(
  ".",
  reporter = "silent",
  load_package = "none",
  stop_on_failure = FALSE,
  stop_on_warning = FALSE
))
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
