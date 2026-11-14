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

test_that("mean_n reads a name that is a column as that column", {
  df <- tibble::tibble(a = c(1, 2), b = c(3, 4), c = c(5, 6))
  a <- "c" # traps: same names as columns, holding another one
  b <- "c"
  expect_equal(mean_n(df, select = a), c(1, 2))
  expect_equal(mean_n(df, select = c(a, b)), c(2, 3))
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

test_that("mean_n() rejects non-integer / out-of-bounds `min_valid` (0.11.0)", {
  df <- tibble::tibble(a = c(1, 2), b = c(3, 4), c = c(5, 6))

  # non-integer count >= 1 is now rejected
  expect_error(
    mean_n(df, min_valid = 1.5),
    "proportion in \\(0, 1\\) or a non-negative integer count"
  )
  expect_error(
    mean_n(df, min_valid = 2.5),
    "proportion in \\(0, 1\\) or a non-negative integer count"
  )

  # count exceeding ncol now errors with a clear message instead of
  # silently making every row NA
  expect_error(
    mean_n(df, min_valid = 100),
    "exceeds the number of selected numeric columns"
  )

  # non-numeric / NA / negative still rejected
  expect_error(mean_n(df, min_valid = "two"), "single non-negative number")
  expect_error(mean_n(df, min_valid = NA_real_), "single non-negative number")
  expect_error(mean_n(df, min_valid = -1), "single non-negative number")

  # legit cases keep working
  expect_silent(mean_n(df, min_valid = 0))
  expect_silent(mean_n(df, min_valid = 1L))
  expect_silent(mean_n(df, min_valid = 0.5)) # proportion
  expect_silent(mean_n(df, min_valid = 3L))
})

test_that("mean_n masks all-NA rows to NA even with min_valid = 0 (0.13.0)", {
  df <- tibble::tibble(a = c(1, NA, 3), b = c(2, NA, NA))
  res <- mean_n(df, min_valid = 0)
  expect_equal(res, c(1.5, NA, 3))
  # the raw rowMeans identity (NaN) must not leak through
  expect_false(any(is.nan(res)))
})

test_that("mean_n() rejects non-integer `digits` (matches cross_tab / freq 0.11.0)", {
  df <- tibble::tibble(a = c(1, 2), b = c(3, 4))
  expect_error(mean_n(df, digits = 1.5), "non-negative integer")
  expect_error(mean_n(df, digits = -1), "non-negative integer")
  expect_error(mean_n(df, digits = NA_real_), "non-negative integer")
  expect_silent(mean_n(df, digits = 0L))
  expect_silent(mean_n(df, digits = 3))
})

test_that("mean_n() rejects bit64::integer64 columns with a classed error", {
  # Manually classed vector: inherits() is all the guard needs, and
  # this is exactly the shape a bare integer64 column has when bit64
  # is not loaded (raw int64 bit patterns in a double payload).
  df <- data.frame(a = c(1, 2))
  df$b <- structure(c(9.9e-324, 1.5e-323), class = "integer64")
  expect_error(mean_n(df), class = "spicy_invalid_data")
  expect_error(mean_n(df), "integer64")
  # excluding the integer64 column restores normal computation
  expect_equal(mean_n(df, select = a), c(1, 2))
})

test_that("mean_n() refuses a survey design and says how to add the score", {
  skip_on_cran()
  skip_if_not_installed("survey")
  d <- data.frame(a = c(1, 2, 3, 4), b = c(2, 3, 4, 5), w = c(1, 2, 1, 2))
  design <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  for (des in list(design, survey::as.svrepdesign(design))) {
    err <- expect_error(
      mean_n(des, select = c(a, b)),
      class = "spicy_wrong_regime"
    )
    msg <- conditionMessage(err)
    expect_match(
      msg,
      "update(design, score = mean_n(design$variables, select = ...))",
      fixed = TRUE
    )
    expect_no_match(msg, "standard errors", fixed = TRUE)
  }
  # The call the message gives.
  design <- update(design, score = mean_n(design$variables, select = c(a, b)))
  expect_equal(design$variables$score, c(1.5, 2.5, 3.5, 4.5))
})

test_that("mean_n() on a two-phase design names the items in update()", {
  skip_on_cran()
  skip_if_not_installed("survey")
  d <- data.frame(
    id = 1:8,
    a = c(1, 2, 3, 4, 5, 6, 7, 8),
    b = c(2, NA, 4, 5, 6, 7, 8, 9),
    in2 = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
  )
  score <- mean_n(d, select = c(a, b))
  for (method in c("approx", "full")) {
    design <- survey::twophase(
      id = list(~id, ~id),
      subset = ~in2,
      data = d,
      method = method
    )
    err <- expect_error(
      mean_n(design, select = c(a, b)),
      class = "spicy_wrong_regime"
    )
    expect_match(
      conditionMessage(err),
      "update(design, score = mean_n(data.frame(<item1>, <item2>, ...)))",
      fixed = TRUE
    )
    # The call the message gives fills both tables of the design.
    design <- update(design, score = mean_n(data.frame(a, b)))
    expect_equal(design$phase1$full$variables$score, score)
    expect_equal(design$phase1$sample$variables$score, score[d$in2])
    expect_s3_class(survey::svymean(~score, design, na.rm = TRUE), "svystat")
  }
})

test_that("mean_n() on a srvyr design: the generic call, none if two-phase", {
  skip_on_cran()
  skip_if_not_installed("survey")
  d <- data.frame(
    id = 1:8,
    a = c(1, 2, 3, 4, 5, 6, 7, 8),
    b = c(2, NA, 4, 5, 6, 7, 8, 9),
    in2 = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
  )
  # The srvyr class is simulated: srvyr is not a dependency. Checked by
  # hand with srvyr 1.3.1 on 2026-11-14: the generic update() call works
  # on an ordinary tbl_svy, and on a two-phase tbl_svy no single call
  # serves both svymean() and srvyr's survey_mean() (mutate() fills only
  # srvyr's copy of the phase-2 data, update() only survey's tables).
  as_tbl_svy <- function(x) structure(x, class = c("tbl_svy", class(x)))
  one <- survey::svydesign(ids = ~1, weights = ~a, data = d)
  err <- expect_error(
    mean_n(as_tbl_svy(one), select = c(a, b)),
    class = "spicy_wrong_regime"
  )
  expect_match(
    conditionMessage(err),
    "update(design, score = mean_n(design$variables, select = ...))",
    fixed = TRUE
  )
  two <- survey::twophase(id = list(~id, ~id), subset = ~in2, data = d)
  err <- expect_error(
    mean_n(as_tbl_svy(two), select = c(a, b)),
    class = "spicy_wrong_regime"
  )
  expect_match(
    conditionMessage(err),
    "on the data before building the two-phase design",
    fixed = TRUE
  )
  expect_no_match(conditionMessage(err), "design <-", fixed = TRUE)
})
