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
  expect_equal(mean_n(df, min_valid = 2 / 3), c(2.5, NA, 6))
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

test_that("mean_n warns when no numeric columns are selected", {
  df <- tibble::tibble(
    a = c(NA, NA),
    b = c(NA, NA)
  )

  expect_warning(
    res <- mean_n(df),
    "No numeric columns selected",
    fixed = TRUE
  )
  expect_true(all(is.na(res)))
})

test_that("mean_n validates min_valid and digits", {
  df <- tibble::tibble(a = c(1, 2), b = c(3, 4))
  expect_error(mean_n(df, min_valid = -1), "non-negative")
  expect_error(mean_n(df, min_valid = "a"), "non-negative")
  expect_error(mean_n(df, digits = -1), "non-negative")
  expect_error(mean_n(df, digits = "a"), "non-negative")
})

test_that("mean_n regex mode supports default select", {
  df <- tibble::tibble(var1 = c(1, 2), var2 = c(3, 4))
  expect_equal(mean_n(df, regex = TRUE), c(2, 3))
})

test_that("mean_n works with character vector select", {
  df <- tibble::tibble(a = c(1, 2), b = c(3, 4), c = c(5, 6))
  items <- c("a", "b")
  expect_equal(mean_n(df, select = items), c(2, 3))
})

test_that("mean_n digits argument rounds result", {
  df <- tibble::tibble(a = c(1, 2), b = c(2, 3), c = c(3, 4))
  res <- mean_n(df, digits = 1)
  expect_equal(res, c(2.0, 3.0))
})

test_that("mean_n verbose prints info messages", {
  df <- tibble::tibble(num = c(1, 2), char = c("a", "b"))
  expect_message(mean_n(df, verbose = TRUE), "Ignored non-numeric")
  expect_message(mean_n(df, verbose = TRUE), "Row means computed")
})

test_that("mean_n regex errors on non-character select", {
  df <- tibble::tibble(a = 1, b = 2)
  expect_error(mean_n(df, select = 1, regex = TRUE), "single character pattern")
})

test_that("mean_n works inside dplyr::mutate", {
  df <- tibble::tibble(x = c(1, 2), y = c(3, 4))
  res <- dplyr::mutate(df, m = mean_n(select = c(x, y)))
  expect_equal(res$m, c(2, 3))
})

test_that("mean_n with exclude drops columns", {
  df <- tibble::tibble(a = c(10, 20), b = c(30, 40), c = c(50, 60))
  expect_equal(mean_n(df, exclude = c("a", "c")), c(30, 40))
})
