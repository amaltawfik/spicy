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

  expect_warning(
    res <- sum_n(df),
    "No numeric columns selected",
    fixed = TRUE
  )
  expect_true(all(is.na(res)))
})

test_that("sum_n regex mode supports default select", {
  df <- tibble::tibble(var1 = c(1, 2), var2 = c(3, 4))
  expect_equal(sum_n(df, regex = TRUE), c(4, 6))
})

test_that("sum_n works with character vector select", {
  df <- tibble::tibble(a = c(1, 2), b = c(3, 4), c = c(5, 6))
  items <- c("a", "b")
  expect_equal(sum_n(df, select = items), c(4, 6))
})

test_that("sum_n digits argument rounds result", {
  df <- tibble::tibble(a = c(1.111, 2.222), b = c(2.222, 3.333))
  res <- sum_n(df, digits = 1)
  expect_equal(res, c(3.3, 5.6))
})

test_that("sum_n verbose prints info messages", {
  df <- tibble::tibble(num = c(1, 2), char = c("a", "b"))
  expect_message(sum_n(df, verbose = TRUE), "Ignored non-numeric")
  expect_message(sum_n(df, verbose = TRUE), "Row sums computed")
})

test_that("sum_n regex errors on non-character select", {
  df <- tibble::tibble(a = 1, b = 2)
  expect_error(sum_n(df, select = 1, regex = TRUE), "single character pattern")
})

test_that("sum_n works inside dplyr::mutate", {
  df <- tibble::tibble(x = c(1, 2), y = c(3, 4))
  res <- dplyr::mutate(df, s = sum_n(select = c(x, y)))
  expect_equal(res$s, c(4, 6))
})

test_that("sum_n with exclude drops columns", {
  df <- tibble::tibble(a = c(10, 20), b = c(30, 40), c = c(50, 60))
  expect_equal(sum_n(df, exclude = c("a", "c")), c(30, 40))
})
