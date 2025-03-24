test_that("freq works with a simple vector", {
  x <- c("A", "B", "A", "C", "A", "B", "B", "C", "C", "C")
  result <- freq(x)

  expect_s3_class(result, "spicy")
  expect_true(all(c("Values", "N", "%") %in% colnames(result)))
  expect_equal(nrow(result), 4)
  expect_equal(tail(as.numeric(result$N), 1), length(x))
})

test_that("freq works with a data frame and a column", {
  df <- data.frame(cat = c("A", "B", "A", "C", "A", "B", "B", "C", "C", "C"))
  result <- freq(df, cat)

  expect_s3_class(result, "spicy")
  expect_equal(nrow(result), 4)
  expect_equal(tail(as.numeric(result$N), 1), nrow(df))
})

test_that("freq works with weights", {
  x <- c("A", "B", "A", "C", "A", "B", "B", "C", "C", "C")
  weights <- c(1, 2, 1, 1, 3, 1, 1, 1, 2, 2)
  result <- freq(x, weights = weights)

  expect_equal(tail(as.numeric(result$N), 1), sum(weights))
})

test_that("freq returns appropriate errors", {
  expect_error(
    freq(matrix(1:9, nrow = 3)),
    "Matrix detected. Use `freq\\(x\\[, 1\\]\\)` or convert to a tibble first\\."
  )
  expect_error(
    freq(c(1, 2, 3), weights = c("a", "b", "c")),
    "'weights' must be a numeric vector."
  )
  expect_error(
    freq(c(1, 2, 3), sort = "invalid"),
    "Invalid value for 'sort'"
  )
})

test_that("freq correctly handles sorting by frequency and name", {
  x <- c("Banana", "Apple", "Cherry", "Banana", "Apple", "Cherry", "Apple")
  df <- data.frame(fruit = x)

  # --- Frequency ascending ---
  result_freq_asc <- freq(x, sort = "+")
  values <- trimws(result_freq_asc$Values)
  result_freq_asc <- result_freq_asc[values != "Total", ]
  values_sorted_asc <- trimws(result_freq_asc$Values)
  expected_order_asc <- names(sort(table(x)))
  expect_equal(values_sorted_asc, expected_order_asc)

  # --- Frequency descending ---
  result_freq_desc <- freq(x, sort = "-")
  values <- trimws(result_freq_desc$Values)
  result_freq_desc <- result_freq_desc[values != "Total", ]
  values_sorted_desc <- trimws(result_freq_desc$Values)
  expected_order_desc <- names(sort(table(x), decreasing = TRUE))
  expect_equal(values_sorted_desc, expected_order_desc)

  # --- Name ascending (A → Z) ---
  result_name_asc <- freq(x, sort = "name+")
  values <- trimws(result_name_asc$Values)
  values_name_asc <- values[values != "Total"]
  expect_equal(values_name_asc, sort(unique(x)))

  # --- Name descending (Z → A) ---
  result_name_desc <- freq(x, sort = "name-")
  values <- trimws(result_name_desc$Values)
  values_name_desc <- values[values != "Total"]
  expect_equal(values_name_desc, sort(unique(x), decreasing = TRUE))

  # --- Same tests with data frame column ---
  df_result_freq <- freq(df, fruit, sort = "-")
  values <- trimws(df_result_freq$Values)
  df_result_freq <- df_result_freq[values != "Total", ]
  df_values_sorted <- trimws(df_result_freq$Values)
  df_expected <- names(sort(table(df$fruit), decreasing = TRUE))
  expect_equal(df_values_sorted, df_expected)
})

test_that("freq works with logical, date, POSIXct, and character types", {
  expect_s3_class(freq(c(TRUE, TRUE, FALSE, NA, FALSE)), "spicy")
  expect_s3_class(freq(as.Date(c("2023-01-01", "2023-01-02", "2023-01-01"))), "spicy")
  expect_s3_class(freq(as.POSIXct(c("2023-01-01 12:00", "2023-01-01 12:00", "2023-02-01 14:00"))), "spicy")
  expect_s3_class(freq(c("red", "green", "blue", "red")), "spicy")
})

test_that("freq handles haven_labelled with labelled_levels", {
  x <- haven::labelled(
    c(1, 1, 2, 3),
    labels = c("Low" = 1, "Medium" = 2, "High" = 3)
  )

  expect_s3_class(freq(x, labelled_levels = "labels"), "spicy")
  expect_s3_class(freq(x, labelled_levels = "values"), "spicy")
  expect_s3_class(freq(x, labelled_levels = "prefixed"), "spicy")
})

test_that("freq works on data.frame columns with different types", {
  df <- tibble::tibble(
    factor_col    = factor(c("A", "A", "B", "C", "C", "C")),
    char_col      = c("a", "a", "b", "c", "c", "c"),
    num_col       = c(1, 1, 2, 3, 3, 3),
    logical_col   = c(TRUE, TRUE, FALSE, FALSE, TRUE, NA),
    date_col      = as.Date(c("2023-01-01", "2023-01-01", "2023-01-02", "2023-01-03", "2023-01-03", "2023-01-03")),
    posix_col     = as.POSIXct(c("2023-01-01 12:00", "2023-01-01 12:00", "2023-01-02 12:00", "2023-01-03 12:00", "2023-01-03 12:00", "2023-01-03 12:00")),
    labelled_col  = haven::labelled(
      c(1, 1, 2, 3, 3, 3),
      labels = c("Faible" = 1, "Moyen" = 2, "Élevé" = 3)
    )
  )

  expect_s3_class(freq(df, factor_col), "spicy")
  expect_s3_class(freq(df, char_col), "spicy")
  expect_s3_class(freq(df, num_col), "spicy")
  expect_s3_class(freq(df, logical_col), "spicy")
  expect_s3_class(freq(df, date_col), "spicy")
  expect_s3_class(freq(df, posix_col), "spicy")
  expect_s3_class(freq(df, labelled_col, labelled_levels = "labels"), "spicy")
})
