# ---------------------------------------------------------------------------
# The guard every oracle loop ends with.
#
# An oracle test walks spicy's terms, looks each one up in another
# package's parameter table, and compares. When the join misses on EVERY
# term, each iteration reaches its `next` and the loop asserts nothing at
# all -- testthat then reports the block as skipped, not failed, and the
# oracle reads green while checking nothing. That is not hypothetical:
# the pscl hurdle oracle sat dead exactly that way, because spicy's
# `(Intercept)` never met parameters' `count_(Intercept)` (register
# n. 243).
#
# So every oracle loop counts the comparisons it really made and ends
# here. Pass `n_expected` -- the number of rows the loop walked -- to
# make the stronger claim: not merely that something was compared, but
# that nothing was skipped.
# ---------------------------------------------------------------------------
expect_oracle_covered <- function(n_checked, n_expected = NULL) {
  testthat::expect_gt(n_checked, 0L)
  if (!is.null(n_expected)) {
    testthat::expect_identical(as.integer(n_checked), as.integer(n_expected))
  }
  invisible(n_checked)
}
