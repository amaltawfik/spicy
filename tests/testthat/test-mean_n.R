test_that("mean_n works on basic numeric data", {
  df <- tibble::tibble(
    a = c(1, 2, 3),
    b = c(4, NA, 6),
    c = c(NA, NA, 9)
  )

  # Default: all values must be valid
  expect_equal(mean_n(df), c(NA, NA, 6))

  # With min_valid = 1
  expect_equal(mean_n(df, min_valid = 1), c(2.5, 2, 6))

  # With min_valid = 2
  expect_equal(mean_n(df, min_valid = 2), c(2.5, NA, 6))

  # With min_valid as proportion
  expect_equal(mean_n(df, min_valid = 2/3), c(2.5, NA, 6))

})

test_that("mean_n handles column selection and exclusion", {
  df <- tibble::tibble(
    var1 = c(10, 20),
    var2 = c(30, 40),
    var3 = c(50, NA),
    other = c("A", "B")
  )

  # Select specific columns
  expect_equal(mean_n(df, select = c(var1, var2)), c(20, 30))

  # Exclude one column
  expect_equal(mean_n(df, exclude = "var3"), c(20, 30))

  # Regex selection
  expect_equal(mean_n(df, select = "^var", regex = TRUE), c(30, NA))
})

test_that("mean_n ignores non-numeric columns", {
  df <- tibble::tibble(
    num1 = c(1, 2),
    char = c("a", "b"),
    num2 = c(3, NA)
  )

  # Should ignore character column silently (with message)
  expect_equal(mean_n(df), c(2, NA))
})

test_that("mean_n returns NA when not enough valid values", {
  df <- tibble::tibble(
    x = c(1, NA, 3),
    y = c(NA, NA, NA),
    z = c(1, 2, 3)
  )

  expect_equal(mean_n(df, min_valid = 2), c(1, NA, 3))
})

test_that("mean_n works on matrices", {
  mat <- matrix(c(1, 2, NA, 4, 5, NA, 7, 8, 9), nrow = 3, byrow = TRUE)
  expect_equal(mean_n(mat, min_valid = 2), c(1.5, 4.5, 8))
})

test_that("mean_n handles all NA rows correctly", {
  df <- tibble::tibble(
    a = c(NA, NA),
    b = c(NA, NA)
  )

  expect_true(all(is.na(mean_n(df))))
})
