# Coverage tests for the empty-glance path in R/regression_broom.R.
#
# The existing suite (test-regression_broom.R) exercises the empty
# tidy() path but not the parallel empty glance() path: line 105
# (`return(maybe_as_tibble(empty_glance()))`) and the `empty_glance()`
# helper body. These tests fill that gap by glancing a manually-built
# spicy_regression_table whose `spicy_fit_stats` attribute is NULL /
# zero-row, mirroring the "tidy – empty input" test.

empty_glance_table <- function(fit_stats) {
  structure(
    data.frame(Variable = character(0), stringsAsFactors = FALSE),
    title = NULL, note = NULL,
    spicy_long = NULL, spicy_fit_stats = fit_stats,
    class = c("spicy_regression_table", "spicy_table", "data.frame")
  )
}


test_that("glance – NULL fit-stats attr → empty broom-shaped tibble", {
  empty <- empty_glance_table(NULL)
  g <- broom::glance(empty)
  expect_equal(nrow(g), 0L)
  expect_true(all(c("model_id", "outcome", "nobs", "weighted_nobs",
                    "r.squared", "adj.r.squared", "omega2", "sigma",
                    "rmse", "f2", "AIC", "AICc", "BIC", "deviance",
                    "df.residual") %in% names(g)))
})

test_that("glance – zero-row fit-stats attr → empty broom-shaped tibble", {
  # An attribute present but with nrow == 0 hits the same guard branch.
  zero <- data.frame(
    model_id = character(0), outcome = character(0),
    nobs = integer(0), weighted_nobs = numeric(0),
    r2 = numeric(0), adj_r2 = numeric(0), omega2 = numeric(0),
    sigma = numeric(0), rmse = numeric(0), f2 = numeric(0),
    AIC = numeric(0), AICc = numeric(0), BIC = numeric(0),
    deviance = numeric(0), df_residual = numeric(0),
    stringsAsFactors = FALSE
  )
  empty <- empty_glance_table(zero)
  g <- broom::glance(empty)
  expect_equal(nrow(g), 0L)
})

test_that("glance – empty result column types are broom-canonical", {
  empty <- empty_glance_table(NULL)
  g <- broom::glance(empty)
  # df.residual kept numeric (double), nobs integer – see file header.
  expect_type(g$df.residual, "double")
  expect_type(g$nobs, "integer")
  expect_type(g$r.squared, "double")
  expect_type(g$model_id, "character")
})

test_that("glance – empty result is a tibble", {
  skip_if_not_installed("tibble")
  empty <- empty_glance_table(NULL)
  g <- broom::glance(empty)
  expect_s3_class(g, "tbl_df")
})
