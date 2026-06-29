# Coverage tests for R/table_continuous_lm_print.R.
#
# Targets the print/coercion/broom branches the existing
# test-table_continuous_lm.R + test-cov-table_continuous_lm*.R suites
# leave uncovered:
#   * the `align = "right"` else-arm of print.spicy_continuous_lm_table()
#   * the missing-'tibble' guard of as_tibble.spicy_continuous_lm_table()
#   * the no-'tibble' plain-data.frame return arms of
#     tidy() / glance().spicy_continuous_lm_table()
#
# Style mirrors the sibling suites: real fits via table_continuous_lm()
# plus local_mocked_bindings(requireNamespace = ...) to surface the
# missing-Suggests fallbacks that never fire when tibble is installed.

# ---- print: align = "right" else-arm ---------------------------------------

test_that("print.spicy_continuous_lm_table right-aligns numeric columns", {
  # `align = "right"` is a valid match.arg() value but the default
  # tests only exercise the "decimal" branch; this hits the final
  # else-arm (right_cols = all numeric cols, align_center = none) of
  # the print method. Assert the table renders with the expected
  # right-aligned values rather than erroring.
  out <- table_continuous_lm(
    iris,
    select = c(Sepal.Length, Petal.Width),
    by = Species,
    align = "right"
  )
  expect_equal(attr(out, "align"), "right")

  rendered <- paste(capture.output(print(out)), collapse = "\n")
  expect_match(rendered, "Continuous outcomes by Species", fixed = TRUE)
  expect_match(rendered, "Sepal.Length", fixed = TRUE)
  expect_match(rendered, "5.01", fixed = TRUE)
  # print() returns its input invisibly.
  expect_identical(withVisible(print(out))$visible, FALSE)
})

# ---- as_tibble: missing 'tibble' guard -------------------------------------

test_that("as_tibble.spicy_continuous_lm_table aborts when tibble is absent", {
  # The body of the method guards on requireNamespace("tibble").
  # tibble is installed in CI (so S3 dispatch resolves), but mocking
  # base::requireNamespace to report it missing surfaces the abort.
  out <- table_continuous_lm(
    iris,
    select = Sepal.Length,
    by = Species,
    output = "long"
  )
  class(out) <- c("spicy_continuous_lm_table", "spicy_table", "data.frame")

  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "tibble")) FALSE else TRUE
    },
    .package = "base"
  )

  expect_error(
    spicy:::as_tibble.spicy_continuous_lm_table(out),
    class = "spicy_missing_pkg"
  )
})

# ---- tidy / glance: no-'tibble' plain data.frame return arms ----------------

test_that("tidy.spicy_continuous_lm_table returns a plain data.frame without tibble", {
  out <- table_continuous_lm(
    iris,
    select = c(Sepal.Length, Petal.Width),
    by = Species
  )

  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "tibble")) FALSE else TRUE
    },
    .package = "base"
  )

  res <- spicy:::tidy.spicy_continuous_lm_table(out)
  # Falls through to `result` (the plain data.frame), not as_tibble().
  expect_s3_class(res, "data.frame")
  expect_false(inherits(res, "tbl_df"))
  expect_true(all(
    c("outcome", "term", "estimate_type", "estimate", "p.value") %in%
      names(res)
  ))
  expect_gt(nrow(res), 0L)
})

test_that("glance.spicy_continuous_lm_table returns a plain data.frame without tibble", {
  out <- table_continuous_lm(
    iris,
    select = c(Sepal.Length, Petal.Width),
    by = Species
  )

  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "tibble")) FALSE else TRUE
    },
    .package = "base"
  )

  res <- spicy:::glance.spicy_continuous_lm_table(out)
  expect_s3_class(res, "data.frame")
  expect_false(inherits(res, "tbl_df"))
  # One row per outcome (Sepal.Length, Petal.Width).
  expect_equal(nrow(res), 2L)
  expect_true(all(
    c("outcome", "predictor_type", "test_type", "r.squared", "nobs") %in%
      names(res)
  ))
})
