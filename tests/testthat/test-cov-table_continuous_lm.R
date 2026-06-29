# Coverage tests for R/table_continuous_lm.R
#
# Target: the cluster-robust (CR*) vcov path in table_continuous_lm()
# that aborts when the optional 'clubSandwich' package is unavailable.
# This branch is only reachable once `cluster` IS supplied (otherwise an
# earlier "requires `cluster`" abort fires first), so we mock
# requireNamespace() to make clubSandwich appear missing while still
# passing a valid cluster.

test_that("table_continuous_lm aborts on CR* vcov when clubSandwich is unavailable", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "clubSandwich")) FALSE else TRUE
    },
    .package = "base"
  )

  expect_error(
    table_continuous_lm(
      sleep,
      select = extra,
      by = group,
      cluster = ID,
      vcov = "CR2"
    ),
    class = "spicy_invalid_input"
  )
})

test_that("CR* clubSandwich-missing abort message names the package and vcov", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "clubSandwich")) FALSE else TRUE
    },
    .package = "base"
  )

  err <- tryCatch(
    table_continuous_lm(
      sleep,
      select = extra,
      by = group,
      cluster = ID,
      vcov = "CR1"
    ),
    error = function(e) e
  )

  expect_s3_class(err, "spicy_invalid_input")
  msg <- conditionMessage(err)
  expect_match(msg, "clubSandwich", fixed = TRUE)
  expect_match(msg, "CR1", fixed = TRUE)
})
