# Coverage top-up for `R/lm_helpers.R`. Targets two paths in
# `resolve_covariates_argument()` not exercised by the existing
# `test-lm_helpers.R` suite:
#
#   * line 226  -- the quosure is NOT a literal `NULL` (so the early
#     `quo_is_null()` guard is skipped) but `eval_tidy()` evaluates it
#     to `NULL` anyway (e.g. a variable holding NULL, or `c()`). The
#     helper must coerce this to `character()`.
#   * lines 235-238 -- the bare-name tidyselect branch where
#     `eval_tidy()` cannot resolve the expression (lands in the
#     sentinel), `eval_select()` is called, and it errors because the
#     referenced column does not exist. The error must be rethrown as
#     `spicy_missing_column` with the underlying message preserved.
#
# The existing suite's "nonexistent column" test uses
# `all_of("nonexistent")`, which `eval_tidy()` resolves to a literal
# character string -- that takes the literal-vector path and is caught
# by the later existence check, NOT the eval_select error handler. A
# *bare* name is required to reach lines 235-238.

# ---- line 226: non-literal quosure evaluating to NULL ----------------------

test_that("resolve_covariates_argument: variable holding NULL coerces to character(0)", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  null_cov <- NULL
  quo <- rlang::quo(null_cov)
  # Sanity: this is NOT a literal NULL quosure, so it bypasses the
  # early `quo_is_null()` return and must fall through to the
  # eval_tidy -> is.null(val) branch instead.
  expect_false(rlang::quo_is_null(quo))
  expect_identical(
    spicy:::resolve_covariates_argument(quo, data),
    character()
  )
})

test_that("resolve_covariates_argument: `c()` (empty combine -> NULL) coerces to character(0)", {
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  quo <- rlang::quo(c())
  expect_false(rlang::quo_is_null(quo))
  expect_identical(
    spicy:::resolve_covariates_argument(quo, data),
    character()
  )
})

# ---- lines 235-238: bare-name eval_select() failure -> spicy_missing_column

test_that("resolve_covariates_argument: bare nonexistent name errors via eval_select handler", {
  # A bare symbol cannot be evaluated outside a tidyselect data mask,
  # so `eval_tidy()` fails and we fall through to `eval_select()`,
  # which raises because the column does not exist. The helper rewraps
  # that as `spicy_missing_column`.
  data <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  err <- expect_error(
    spicy:::resolve_covariates_argument(
      rlang::quo(c(nonexistent_col)),
      data
    ),
    class = "spicy_missing_column"
  )
  # The wrapper prefixes with "Could not resolve `<arg>`:" and keeps
  # the underlying tidyselect message naming the missing column.
  expect_match(conditionMessage(err), "Could not resolve `covariates`")
  expect_match(conditionMessage(err), "nonexistent_col")
})

test_that("resolve_covariates_argument: eval_select handler honours custom `arg` label", {
  # The handler interpolates `arg` into the message; confirm a custom
  # label propagates so callers passing a different argument name get
  # an accurate diagnostic.
  data <- data.frame(age = 1:3)
  err <- expect_error(
    spicy:::resolve_covariates_argument(
      rlang::quo(c(missing_one, missing_two)),
      data,
      arg = "adjust_for"
    ),
    class = "spicy_missing_column"
  )
  expect_match(conditionMessage(err), "Could not resolve `adjust_for`")
})

test_that("resolve_covariates_argument: bare-name eval_select error inherits spicy_error", {
  data <- data.frame(age = 1:3)
  expect_error(
    spicy:::resolve_covariates_argument(rlang::quo(c(no_such_col)), data),
    class = "spicy_error"
  )
})
