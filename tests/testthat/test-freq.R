test_that("freq works with a simple vector", {
  x <- c("A", "B", "A", "C", "A", "B", "B", "C", "C", "C")
  result <- freq(x)

  expect_s3_class(result, "spicy") # Check that the class is correctly assigned
  expect_true(all(c("Values", "N", "%") %in% colnames(result))) # Check column names
  expect_equal(nrow(result), 4) # 3 categories + the "Total" row
  expect_equal(tail(as.numeric(result$N), 1), length(x)) # Check total count
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

  expect_equal(tail(as.numeric(result$N), 1), sum(weights)) # Check weighted total
})

test_that("freq returns an error for invalid arguments", {
  expect_error(freq(matrix(1:9, nrow = 3)), "'data' must be a vector") # Invalid data type
  expect_error(freq(c(1, 2, 3), weights = c("a", "b", "c")), "'weights' must be a numeric vector.") # Non-numeric weights
  expect_error(freq(c(1, 2, 3), sort = "invalid"), "Invalid value for 'sort'") # Invalid sorting option
})

test_that("freq correctly handles sorting by frequency", {
  x <- c("Banana", "Apple", "Cherry", "Banana", "Apple", "Cherry", "Apple")

  result_freq_asc <- freq(x, sort = "+")
  result_freq_asc$Values <- trimws(result_freq_asc$Values) # Remove trailing spaces

  # Extract counts as numeric, excluding "Total" row
  values_sorted_asc <- result_freq_asc$Values[result_freq_asc$Values != "Total"]
  counts_sorted_asc <- as.numeric(result_freq_asc$N[result_freq_asc$Values != "Total"])

  # Sort manually by frequency to compare
  expected_order_asc <- values_sorted_asc[order(counts_sorted_asc)]
  expect_equal(values_sorted_asc, expected_order_asc)

  result_freq_desc <- freq(x, sort = "-")
  result_freq_desc$Values <- trimws(result_freq_desc$Values)

  # Extract counts as numeric, excluding "Total" row
  values_sorted_desc <- result_freq_desc$Values[result_freq_desc$Values != "Total"]
  counts_sorted_desc <- as.numeric(result_freq_desc$N[result_freq_desc$Values != "Total"])

  # Sort manually in descending order to compare
  expected_order_desc <- values_sorted_desc[order(counts_sorted_desc, decreasing = TRUE)]
  expect_equal(values_sorted_desc, expected_order_desc)
})


