test_that("sum_n works on basic numeric data", {
  df <- tibble::tibble(
    a = c(1, 2, 3),
    b = c(4, NA, 6),
    c = c(NA, NA, 9)
  )

  # Default: all values must be valid
  expect_equal(sum_n(df), c(NA, NA, 18))

  # With min_valid = 1
  expect_equal(sum_n(df, min_valid = 1), c(5, 2, 18))

  # With min_valid = 2
  expect_equal(sum_n(df, min_valid = 2), c(5, NA, 18))

  # With min_valid as proportion
  expect_equal(sum_n(df, min_valid = 2 / 3), c(5, NA, 18))
})

test_that("sum_n handles column selection and exclusion", {
  df <- tibble::tibble(
    var1 = c(10, 20),
    var2 = c(30, 40),
    var3 = c(50, NA),
    other = c("A", "B")
  )

  # Select specific columns
  expect_equal(sum_n(df, select = c(var1, var2)), c(40, 60))

  # Exclude one column
  expect_equal(sum_n(df, exclude = "var3"), c(40, 60))

  # Regex selection
  expect_equal(sum_n(df, select = "^var", regex = TRUE), c(90, NA))
})

test_that("sum_n ignores non-numeric columns", {
  df <- tibble::tibble(
    num1 = c(1, 2),
    char = c("a", "b"),
    num2 = c(3, NA)
  )

  # Should ignore character column silently (with message)
  expect_equal(sum_n(df), c(4, NA))
})

test_that("sum_n returns NA when not enough valid values", {
  df <- tibble::tibble(
    x = c(1, NA, 3),
    y = c(NA, NA, NA),
    z = c(1, 2, 3)
  )

  expect_equal(sum_n(df, min_valid = 2), c(2, NA, 6))
})

test_that("sum_n works on matrices", {
  mat <- matrix(c(1, 2, NA, 4, 5, NA, 7, 8, 9), nrow = 3, byrow = TRUE)
  expect_equal(sum_n(mat, min_valid = 2), c(3, 9, 24))
})

test_that("sum_n handles all NA rows correctly", {
  df <- tibble::tibble(
    a = c(NA, NA),
    b = c(NA, NA)
  )

  expect_equal(sum_n(df), c(0, 0))
})
