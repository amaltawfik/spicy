# Coverage tests for R/table_categorical_print.R
# These exercise S3-method paths not hit by the broom-method tests in
# tests/testthat/test-table_categorical.R / test-tables_ascii.R:
#   - tidy()  : spicy_abort when the `long_data` attribute is missing
#   - glance(): spicy_abort when the `long_data` attribute is missing
#   - tidy()  : the `percent`-column branch of the pct_col selection
#   - tidy()  : the neither-`pct`-nor-`percent` -> NA branch
#
# The two NA-handling branches operate purely on the object's
# `long_data` attribute (the contract that drives every broom method),
# so they are exercised on a minimal hand-built spicy_categorical_table
# in the same style as the print-method tests in test-tables_ascii.R.

# ---- tidy()/glance(): missing long_data attribute -> classed abort --------

test_that("tidy() errors with a clear class when long_data is absent", {
  invisible(capture.output(
    out <- table_categorical(sochealth, select = smoking, by = sex)
  ))
  attr(out, "long_data") <- NULL
  expect_error(
    broom::tidy(out),
    class = "spicy_invalid_data"
  )
  expect_error(
    broom::tidy(out),
    "tidy().+requires the long-format data"
  )
})

test_that("glance() errors with a clear class when long_data is absent", {
  invisible(capture.output(
    out <- table_categorical(sochealth, select = smoking, by = sex)
  ))
  attr(out, "long_data") <- NULL
  expect_error(
    broom::glance(out),
    class = "spicy_invalid_data"
  )
  expect_error(
    broom::glance(out),
    "glance().+requires the long-format data"
  )
})

# ---- tidy(): pct_col selection branches -----------------------------------

build_cat_table <- function(long) {
  x <- data.frame(Variable = "v", n = "1", check.names = FALSE)
  class(x) <- c("spicy_categorical_table", "spicy_table", "data.frame")
  attr(x, "long_data") <- long
  x
}

test_that("tidy() reads a 'percent' column when 'pct' is absent", {
  # The internal long format always names the column `pct`; the
  # `else if ("percent" ...)` arm is for an alternative naming. Drive
  # it with a hand-built long_data carrying `percent`, no `pct`.
  long <- data.frame(
    variable = c("v1", "v1"),
    level = c("a", "b"),
    n = c(3L, 7L),
    percent = c(30, 70),
    stringsAsFactors = FALSE
  )
  td <- broom::tidy(build_cat_table(long))
  expect_equal(td$proportion, c(0.3, 0.7))
  expect_equal(td$n, c(3L, 7L))
  expect_equal(td$level, c("a", "b"))
})

test_that("tidy() yields NA proportions when neither pct nor percent exist", {
  long <- data.frame(
    variable = c("v1", "v1"),
    level = c("a", "b"),
    n = c(3L, 7L),
    stringsAsFactors = FALSE
  )
  td <- broom::tidy(build_cat_table(long))
  expect_true(all(is.na(td$proportion)))
  expect_equal(td$n, c(3L, 7L))
})
