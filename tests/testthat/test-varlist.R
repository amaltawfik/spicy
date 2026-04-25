test_that("varlist() returns a data.frame (tibble)", {
  result <- varlist(mtcars, tbl = TRUE)
  expect_s3_class(result, "data.frame")
  expect_s3_class(result, "tbl_df")
})

test_that("varlist()$Variable returns a character vector", {
  expect_type(varlist(mtcars, tbl = TRUE)$Variable, "character")
})

test_that("varlist() returns correct column names", {
  expect_named(
    varlist(mtcars, tbl = TRUE),
    c("Variable", "Label", "Values", "Class", "N_distinct", "N_valid", "NAs")
  )
})

test_that("varlist() works with tidyselect selectors", {
  result <- varlist(iris, starts_with("Sepal"), tbl = TRUE)
  expect_true(all(grepl("^Sepal", result$Variable)))
})

test_that("varlist() supports tidyselect helpers with local objects", {
  selected <- "Sepal.Width"
  result <- varlist(iris, all_of(selected), tbl = TRUE)
  expect_equal(result$Variable, selected)
})

test_that("varlist() does not expose internal raw expression argument", {
  expect_false(".raw_expr" %in% names(formals(varlist)))
})

test_that("vl() is an alias of varlist()", {
  expect_equal(vl(mtcars, tbl = TRUE), varlist(mtcars, tbl = TRUE))
})

test_that("varlist() throws error for non-data.frame input", {
  expect_error(varlist(1:10), "only works with named data frames")
})

test_that("varlist() validates missing column names", {
  df <- structure(
    list(1:3),
    class = "data.frame",
    row.names = c(NA_integer_, -3L)
  )

  expect_error(varlist(df, tbl = TRUE), "`x` must have column names")
})

test_that("varlist() validates NA column names", {
  df <- data.frame(x = 1:3)
  names(df) <- NA_character_

  expect_error(varlist(df, tbl = TRUE), "`x` must have non-empty column names")
})

test_that("varlist() validates empty column names", {
  df <- data.frame(x = 1:3)
  names(df) <- ""

  expect_error(varlist(df, tbl = TRUE), "`x` must have non-empty column names")
})

test_that("varlist() validates duplicate column names", {
  df <- data.frame(x = 1:3, y = 4:6)
  names(df) <- c("x", "x")

  expect_error(varlist(df, tbl = TRUE), "`x` must have unique column names")
})

test_that("varlist() validates logical arguments", {
  expect_error(varlist(mtcars, values = NA), "`values` must be TRUE or FALSE")
  expect_error(
    varlist(mtcars, values = c(TRUE, FALSE)),
    "`values` must be TRUE or FALSE"
  )
  expect_error(varlist(mtcars, tbl = NA), "`tbl` must be TRUE or FALSE")
  expect_error(
    varlist(mtcars, include_na = NA),
    "`include_na` must be TRUE or FALSE"
  )
  expect_error(vl(mtcars, values = NA), "`values` must be TRUE or FALSE")
})

test_that("varlist() validates factor_levels", {
  expect_error(
    varlist(mtcars, factor_levels = "bad"),
    '`factor_levels` must be "observed" or "all"'
  )
  expect_error(
    varlist(mtcars, factor_levels = NA_character_),
    '`factor_levels` must be "observed" or "all"'
  )
  expect_error(
    varlist(mtcars, factor_levels = c("observed", "bad")),
    '`factor_levels` must be "observed" or "all"'
  )
  expect_error(
    vl(mtcars, factor_levels = "bad"),
    '`factor_levels` must be "observed" or "all"'
  )
})

test_that("varlist_title() returns vl: name for simple object", {
  dummy_expr <- quote(iris)
  expect_equal(varlist_title(dummy_expr), "vl: iris")
})

test_that("varlist_title() adds * for transformed object", {
  transformed_expr <- quote(head(iris))
  expect_equal(varlist_title(transformed_expr), "vl: iris*")
})

test_that("varlist_title() adds * when selectors are used", {
  expect_equal(varlist_title(quote(iris), selectors_used = TRUE), "vl: iris*")
})

test_that("varlist_title() handles nested calls", {
  expect_equal(varlist_title(quote(dplyr::filter(df, x > 1))), "vl: df*")
})

test_that("varlist_title() extracts symbol from nested call arg", {
  expect_equal(varlist_title(quote(fun(subset(df, x > 1)))), "vl: df*")
})

test_that("varlist_title() returns fallback for non-symbol expressions", {
  expect_equal(varlist_title(1L), "vl: <data>")
})

test_that("varlist_title() returns fallback when deparse fails", {
  bad <- structure(list(), class = "weird")
  expect_equal(varlist_title(bad), "vl: <data>")
})

test_that("varlist() warns when no columns match", {
  expect_warning(
    varlist(mtcars, starts_with("zzz"), tbl = TRUE),
    "No columns selected"
  )
})

test_that("varlist() returns empty tibble when no columns match", {
  res <- suppressWarnings(varlist(mtcars, starts_with("zzz"), tbl = TRUE))
  expect_equal(nrow(res), 0L)
  expect_named(
    res,
    c("Variable", "Label", "Values", "Class", "N_distinct", "N_valid", "NAs")
  )
})

test_that("varlist() non-interactive with no tbl returns message", {
  expect_message(
    varlist(mtcars),
    "Non-interactive session"
  )
})

test_that("varlist() counts NAs correctly", {
  df <- data.frame(a = c(1, NA, 3), b = c("x", "y", NA))
  res <- varlist(df, tbl = TRUE)
  expect_equal(unname(res$NAs), c(1L, 1L))
  expect_equal(unname(res$N_valid), c(2L, 2L))
})

test_that("varlist() shows labels when available", {
  df <- data.frame(x = 1:3)
  attr(df$x, "label") <- "My label"
  res <- varlist(df, tbl = TRUE)
  expect_equal(unname(res$Label), "My label")
})

test_that("varlist() returns NA label when none set", {
  res <- varlist(data.frame(x = 1:3), tbl = TRUE)
  expect_true(is.na(res$Label))
})

test_that("varlist() values = TRUE shows all unique values", {
  df <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7))
  res <- varlist(df, tbl = TRUE, values = TRUE)
  expect_equal(unname(res$Values), "1, 2, 3, 4, 5, 6, 7")
})

test_that("varlist() values = FALSE truncates >4 unique values", {
  df <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7))
  res <- varlist(df, tbl = TRUE, values = FALSE)
  expect_match(res$Values, "\\.\\.\\.")
})

test_that("varlist() values = FALSE shows all when <= 4 values", {
  df <- data.frame(x = c(1, 2, 3))
  res <- varlist(df, tbl = TRUE, values = FALSE)
  expect_equal(unname(res$Values), "1, 2, 3")
})

test_that("varlist() preserves literal NA and empty string values", {
  df <- data.frame(x = c("", "NA", "b"), stringsAsFactors = FALSE)

  res <- varlist(df, tbl = TRUE)
  res_all <- varlist(df, tbl = TRUE, values = TRUE)

  expect_equal(unname(res$Values), "\"\", \"NA\", b")
  expect_equal(unname(res_all$Values), "\"\", \"NA\", b")
  expect_equal(unname(res$N_distinct), 3L)
})

test_that("varlist() include_na appends NA to values", {
  df <- data.frame(x = c(1, 2, NA))
  res <- varlist(df, tbl = TRUE, include_na = TRUE)
  expect_equal(unname(res$Values), "1, 2, <NA>")
})

test_that("varlist() include_na appends NaN for numeric", {
  df <- data.frame(x = c(1, 2, NaN))
  res <- varlist(df, tbl = TRUE, include_na = TRUE)
  expect_equal(unname(res$Values), "1, 2, <NaN>")
})

test_that("varlist() include_na distinguishes NA from NaN", {
  nan_only <- data.frame(x = c(1, NaN, 3))
  both <- data.frame(x = c(1, NA, NaN, 3))

  res_nan_only <- varlist(nan_only, tbl = TRUE, include_na = TRUE)
  res_both <- varlist(both, tbl = TRUE, include_na = TRUE)

  expect_equal(unname(res_nan_only$Values), "1, 3, <NaN>")
  expect_equal(unname(res_both$Values), "1, 3, <NA>, <NaN>")
  expect_equal(unname(res_both$NAs), 2L)
  expect_equal(unname(res_both$N_valid), 2L)
})

test_that("varlist() distinguishes literal NA strings from missing values", {
  df <- data.frame(x = c("NA", NA), stringsAsFactors = FALSE)
  res <- varlist(df, tbl = TRUE, include_na = TRUE)
  res_all <- varlist(df, tbl = TRUE, values = TRUE, include_na = TRUE)

  expect_equal(unname(res$Values), "\"NA\", <NA>")
  expect_equal(unname(res_all$Values), "\"NA\", <NA>")
})

test_that("varlist() quotes literal NaN strings", {
  df <- data.frame(x = c("NaN", NA), stringsAsFactors = FALSE)
  res <- varlist(df, tbl = TRUE, include_na = TRUE)

  expect_equal(unname(res$Values), "\"NaN\", <NA>")
})

test_that("varlist() distinguishes factor NA levels from missing values", {
  f <- factor(c("NA", NA), levels = "NA")
  res <- varlist(data.frame(x = f), tbl = TRUE, include_na = TRUE)

  expect_equal(unname(res$Values), "\"NA\", <NA>")
})

test_that("summarize_values_minmax handles factors", {
  f <- factor(c("a", "b", "c"), levels = c("c", "b", "a"))
  res <- varlist(data.frame(x = f), tbl = TRUE)
  expect_equal(unname(res$Values), "c, b, a")
})

test_that("summarize_values_minmax handles Date columns", {
  df <- data.frame(d = as.Date(c("2024-01-01", "2024-06-15", "2024-12-31")))
  res <- varlist(df, tbl = TRUE)
  expect_match(res$Values, "2024")
})

test_that("summarize_values_minmax handles POSIXct columns", {
  df <- data.frame(t = as.POSIXct(c("2024-01-01 10:00", "2024-06-15 12:00")))
  res <- varlist(df, tbl = TRUE)
  expect_match(res$Values, "2024")
})

test_that("varlist() summarizes matrix columns by rows", {
  mat <- rbind(c(1, 2), c(3, 4))
  df <- data.frame(x = I(mat))

  res <- varlist(df, tbl = TRUE)

  expect_equal(unname(res$Values), "Matrix(2 x 2)")
  expect_equal(unname(res$N_distinct), 2L)
  expect_equal(unname(res$N_valid), 2L)
  expect_equal(unname(res$NAs), 0L)
})

test_that("varlist() counts matrix rows with missing cells as missing", {
  mat <- rbind(c(1, 2), c(NA_real_, 4))
  df <- data.frame(x = I(mat))

  res <- varlist(df, tbl = TRUE, include_na = TRUE)

  expect_equal(unname(res$Values), "Matrix(2 x 2), <NA>")
  expect_equal(unname(res$N_distinct), 1L)
  expect_equal(unname(res$N_valid), 1L)
  expect_equal(unname(res$NAs), 1L)
})

test_that("varlist() counts distinct matrix rows", {
  mat <- rbind(c(1, 2), c(1, 2), c(3, 4))
  df <- data.frame(x = I(mat))

  res <- varlist(df, tbl = TRUE)

  expect_equal(unname(res$N_distinct), 2L)
  expect_equal(unname(res$N_valid), 3L)
})

test_that("varlist() values = TRUE keeps matrix columns structural", {
  mat <- rbind(c(1, 2), c(3, 4))
  df <- data.frame(x = I(mat))

  res <- varlist(df, tbl = TRUE, values = TRUE)

  expect_equal(unname(res$Values), "Matrix(2 x 2)")
})

test_that("varlist() summarizes array columns by dimensions", {
  arr <- array(1:8, dim = c(2, 2, 2))
  df <- tibble::tibble(x = I(arr))

  res <- varlist(df, tbl = TRUE)

  expect_equal(unname(res$Values), "Array(2 x 2 x 2)")
  expect_equal(unname(res$N_distinct), 2L)
  expect_equal(unname(res$N_valid), 2L)
})

test_that("summarize_values_minmax handles list columns", {
  df <- tibble::tibble(x = list(1:3, "a", TRUE))
  res <- varlist(df, tbl = TRUE)
  expect_match(res$Values, "List\\(3\\)")
})

test_that("summarize_values_all handles list columns", {
  df <- tibble::tibble(x = list(1:3, "a"))
  res <- varlist(df, tbl = TRUE, values = TRUE)
  expect_match(res$Values, "List\\(2\\)")
})

test_that("summarize_values_all handles logical columns", {
  df <- data.frame(x = c(TRUE, FALSE, TRUE))
  res <- varlist(df, tbl = TRUE, values = TRUE)
  expect_match(res$Values, "FALSE")
  expect_match(res$Values, "TRUE")
})

test_that("summarize_values_all handles character columns", {
  df <- data.frame(x = c("b", "a", "c"), stringsAsFactors = FALSE)
  res <- varlist(df, tbl = TRUE, values = TRUE)
  expect_equal(unname(res$Values), "a, b, c")
})

test_that("summarize_values_all handles factors with values = TRUE", {
  f <- factor(c("x", "y"), levels = c("y", "x"))
  res <- varlist(data.frame(v = f), tbl = TRUE, values = TRUE)
  expect_equal(unname(res$Values), "y, x")
})

test_that("varlist() displays observed factor levels by default", {
  f <- factor(c("x", "x"), levels = c("x", "y"))
  res <- varlist(data.frame(v = f), tbl = TRUE)
  res_all <- varlist(data.frame(v = f), tbl = TRUE, values = TRUE)

  expect_equal(unname(res$Values), "x")
  expect_equal(unname(res_all$Values), "x")
  expect_equal(unname(res$N_distinct), 1L)
})

test_that("varlist() can display all factor levels", {
  f <- factor(c("x", "x"), levels = c("x", "y"))
  res <- varlist(data.frame(v = f), tbl = TRUE, factor_levels = "all")
  res_all <- varlist(
    data.frame(v = f),
    tbl = TRUE,
    values = TRUE,
    factor_levels = "all"
  )

  expect_equal(unname(res$Values), "x, y")
  expect_equal(unname(res_all$Values), "x, y")
  expect_equal(unname(res$N_distinct), 1L)
})

test_that("summarize_values_minmax handles all-NA column", {
  df <- data.frame(x = c(NA_real_, NA_real_))
  res <- varlist(df, tbl = TRUE)
  expect_equal(unname(res$Values), "")
  expect_equal(unname(res$N_distinct), 0L)
})

test_that("summarize_values_minmax include_na with empty non-NA values", {
  df <- data.frame(x = c(NA_real_, NA_real_))
  res <- varlist(df, tbl = TRUE, include_na = TRUE)
  expect_equal(unname(res$Values), "<NA>")
})

test_that("varlist() handles labelled columns", {
  skip_if_not_installed("labelled")
  x <- labelled::labelled(c(1, 2, 3), labels = c(Low = 1, Mid = 2, High = 3))
  df <- data.frame(v = x)
  res <- varlist(df, tbl = TRUE)
  expect_match(res$Values, "Low")
  expect_match(res$Class, "labelled")
})

test_that("varlist() handles labelled columns with values = TRUE", {
  skip_if_not_installed("labelled")
  x <- labelled::labelled(c(1, 2, 3), labels = c(Low = 1, Mid = 2, High = 3))
  df <- data.frame(v = x)
  res <- varlist(df, tbl = TRUE, values = TRUE)
  expect_match(res$Values, "Low")
})

test_that("varlist() uses labelled value order consistently", {
  skip_if_not_installed("labelled")
  x <- labelled::labelled(c(2, 1, 3), labels = c(Low = 1, Mid = 2, High = 3))
  df <- data.frame(v = x)

  res <- varlist(df, tbl = TRUE)
  res_all <- varlist(df, tbl = TRUE, values = TRUE)

  expect_equal(unname(res$Values), "[1] Low, [2] Mid, [3] High")
  expect_equal(unname(res_all$Values), "[1] Low, [2] Mid, [3] High")
})

test_that("N_distinct counts correctly", {
  df <- data.frame(x = c(1, 1, 2, NA, 2))
  res <- varlist(df, tbl = TRUE)
  expect_equal(unname(res$N_distinct), 2L)
})

test_that("varlist() empty selection returns empty data.frame with tbl", {
  df <- data.frame(x = 1:3, y = 4:6)
  res <- suppressWarnings(varlist(df, starts_with("z"), tbl = TRUE))
  expect_equal(nrow(res), 0L)
})

test_that("varlist() non-interactive empty selection prints message", {
  df <- data.frame(x = 1:3)
  expect_message(
    suppressWarnings(varlist(df, starts_with("z"))),
    "No columns selected"
  )
})

test_that("varlist() non-interactive prints message", {
  expect_message(
    varlist(mtcars),
    "Non-interactive"
  )
})

test_that("varlist() values=TRUE with NaN shows NaN", {
  df <- data.frame(x = c(1, NaN, 3))
  res <- varlist(df, tbl = TRUE, values = TRUE, include_na = TRUE)
  expect_equal(unname(res$Values), "1, 3, <NaN>")
})
