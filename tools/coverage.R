# Local coverage measurement.
#
#   Rscript tools/coverage.R "<testthat filter or . for all>" <out.rds>
#
# THE INVOCATION CONTRACT, learned twice (2026-08-18 and -08-27): the
# `code =` handed to covr::package_coverage() must load the
# INSTRUMENTED INSTALLED copy -- testthat::test_dir(load_package =
# "installed") -- and never reload the package from source
# (devtools::load_all / test_local). A source reload hands
# covr:::exclude() traces with NA srcrefs and it dies with
#   Error in seq.default(df[i, "first_line"], df[i, "last_line"]) :
#     'from' must be a finite number
# That error is an invocation bug, not a covr or package bug: the
# 2026-08-27 full-suite run through THIS script completed with zero
# NA traces (18800 traces, register n-264 rectified).
#
# The exclude() wrapper below is kept as a SENTINEL: it drops any
# NA-srcref trace instead of dying and prints which files produced
# them, so a recurrence is named rather than fatal. Session-local --
# no project file is touched.

args <- commandArgs(trailingOnly = TRUE)
filter <- if (length(args) >= 1L) args[[1L]] else ""
outfile <- if (length(args) >= 2L) args[[2L]] else "cov_out.rds"

orig_exclude <- covr:::exclude
patched <- function(coverage, ...) {
  df <- as.data.frame(coverage, sort = FALSE)
  bad <- !is.finite(df$first_line) | !is.finite(df$last_line)
  if (any(bad)) {
    message(
      "NA_SRCREF_TRACES: ",
      sum(bad),
      " of ",
      length(coverage),
      "; files: ",
      paste(unique(as.character(df$filename[bad])), collapse = " | ")
    )
    cls <- class(coverage)
    att <- attributes(coverage)
    coverage <- coverage[!bad]
    att$names <- names(coverage)
    attributes(coverage) <- att
    class(coverage) <- cls
  }
  orig_exclude(coverage, ...)
}
environment(patched) <- asNamespace("covr")
utils::assignInNamespace("exclude", patched, ns = "covr")

# The instrumented package is INSTALLED into a temp lib by covr; the
# test code must load THAT copy (load_package = "installed"), never
# load_all() from source, or nothing is recorded.
code <- sprintf(
  paste0(
    'library(spicy); testthat::test_dir("tests/testthat", package = "spicy",',
    ' load_package = "installed", filter = %s, reporter = "silent",',
    " stop_on_failure = FALSE)"
  ),
  if (nzchar(filter)) sprintf('"%s"', filter) else "NULL"
)

cov <- covr::package_coverage(type = "none", code = code, quiet = TRUE)
saveRDS(cov, outfile)

tal <- covr::tally_coverage(cov)
hit <- stats::aggregate(
  tal$value,
  by = list(file = tal$filename),
  FUN = function(v) sum(v > 0)
)
tot <- stats::aggregate(tal$value, by = list(file = tal$filename), FUN = length)
res <- merge(hit, tot, by = "file")
names(res) <- c("file", "hit", "total")
res <- res[res$hit > 0, ]
res <- res[order(-res$hit), ]
cat("FILES TOUCHED BY THESE TESTS:\n")
print(utils::head(res, 40), row.names = FALSE)
