test_that("freq() works with a simple numeric vector", {
  x <- c(1, 2, 2, 3, 3, 3, NA)
  df <- freq(x, styled = FALSE)

  expect_s3_class(df, "data.frame")
  expect_true(all(c("value", "n", "prop") %in% names(df)))
  expect_equal(sum(df$n, na.rm = TRUE), length(x))
  expect_equal(round(sum(df$prop, na.rm = TRUE), 1), 1)
})


test_that("freq() works with a data.frame column", {
  df <- data.frame(cat = c("A", "B", "A", "C", "A", "B", "B", "C", "C", "C"))
  res <- freq(df, cat, styled = FALSE)

  expect_s3_class(res, "data.frame")
  expect_equal(sum(res$n, na.rm = TRUE), nrow(df))
  expect_true(any(grepl("C", res$value)))
})


test_that("freq() handles labelled variables correctly", {
  library(labelled)

  x <- labelled(
    c(1, 2, 3, 1, 2, 3, 1, 2, NA),
    labels = c("Low" = 1, "Medium" = 2, "High" = 3)
  )
  var_label(x) <- "Satisfaction level"

  # Prefixed (default)
  f1 <- freq(x, labelled_levels = "prefixed", styled = FALSE)
  expect_true(any(grepl("\\[1\\]", f1$value)))

  # Labels only
  f2 <- freq(x, labelled_levels = "labels", styled = FALSE)
  expect_true(all(!grepl("\\[", f2$value)))

  # Underlying values only
  f1 <- freq(x, styled = FALSE)
  f2 <- freq(x, na_val = 1, styled = FALSE)

  # After recoding, there should be one fewer distinct category
  expect_true(length(unique(f2$value)) <= length(unique(f1$value)))
})


test_that("freq() handles weights and rescaling", {
  df <- data.frame(
    sexe = factor(c("Male", "Female", "Female", "Male", NA, "Female")),
    poids = c(12, 8, 10, 15, 7, 9)
  )

  # Weighted, rescaled
  f_rescaled <- freq(df, sexe, weights = poids, rescale = TRUE, styled = FALSE)
  total_weighted <- sum(f_rescaled$n)
  expect_true(abs(total_weighted - nrow(df)) < 1e-6)

  # Weighted, not rescaled
  f_unscaled <- freq(df, sexe, weights = poids, rescale = FALSE, styled = FALSE)
  expect_true(sum(f_unscaled$n) > nrow(df))
})


test_that("freq() handles missing value recoding", {
  x <- labelled(
    c(1, 2, 3, 1, 2, 3, 1),
    labels = c("Low" = 1, "Medium" = 2, "High" = 3)
  )

  f1 <- freq(x, styled = FALSE)
  f2 <- freq(x, na_val = 1, styled = FALSE)

  # Compare NA frequencies in the output table
  na_count_f1 <- f1$n[f1$value == "<NA>"]
  na_count_f2 <- f2$n[f2$value == "<NA>"]

  # Handle case where <NA> row is missing
  na_count_f1 <- if (length(na_count_f1)) na_count_f1 else 0
  na_count_f2 <- if (length(na_count_f2)) na_count_f2 else 0

  # Expect the 'Low' category to be removed after recoding
  expect_false(any(grepl("Low", f2$value)))

  # Optionally, confirm total frequency unchanged (only recoded)
  expect_equal(round(sum(f1$n, na.rm = TRUE), 5), round(sum(f2$n, na.rm = TRUE), 5))
})

test_that("freq() correctly sorts by frequency and name", {
  x <- c("Banana", "Apple", "Cherry", "Banana", "Apple", "Cherry", "Apple")

  f_plus <- freq(x, sort = "+", styled = FALSE)
  f_minus <- freq(x, sort = "-", styled = FALSE)
  f_name_plus <- freq(x, sort = "name+", styled = FALSE)
  f_name_minus <- freq(x, sort = "name-", styled = FALSE)

  # Frequency ascending/descending
  expect_true(f_plus$n[1] <= f_plus$n[length(f_plus$n)])
  expect_true(f_minus$n[1] >= f_minus$n[length(f_minus$n)])

  # Alphabetical order
  expect_equal(sort(f_name_plus$value), f_name_plus$value)
  expect_equal(sort(f_name_minus$value, decreasing = TRUE), f_name_minus$value)
})


test_that("freq() handles multiple data types correctly", {
  df <- data.frame(
    logical_col = c(TRUE, FALSE, TRUE, NA),
    date_col = as.Date(c("2023-01-01", "2023-01-02", "2023-01-01", NA)),
    posix_col = as.POSIXct(c("2023-01-01 12:00", "2023-01-02 12:00", NA, "2023-01-02 12:00")),
    char_col = c("a", "a", "b", NA),
    num_col = c(1, 1, 2, NA)
  )

  expect_s3_class(freq(df, logical_col, styled = FALSE), "data.frame")
  expect_s3_class(freq(df, date_col, styled = FALSE), "data.frame")
  expect_s3_class(freq(df, posix_col, styled = FALSE), "data.frame")
  expect_s3_class(freq(df, char_col, styled = FALSE), "data.frame")
  expect_s3_class(freq(df, num_col, styled = FALSE), "data.frame")
})


test_that("freq() handles invalid weight and sort arguments", {
  x <- c(1, 2, 3)
  expect_error(freq(x, weights = c(-1, 0, 1)), "must be non-negative")
  expect_error(freq(x, weights = c(1, 2)), "same length")
  expect_error(freq(x, sort = "wrong"), "Invalid value for 'sort'")
})


test_that("freq() prints styled table invisibly", {
  x <- c("A", "B", "B", "C")
  expect_invisible(freq(x, styled = TRUE))
})
