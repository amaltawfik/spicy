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
    c("Variable", "Label", "Values", "Class", "Ndist_val", "N_valid", "NAs")
  )
})

test_that("varlist() works with tidyselect selectors", {
  result <- varlist(iris, starts_with("Sepal"), tbl = TRUE)
  expect_true(all(grepl("^Sepal", result$Variable)))
})

test_that("vl() is an alias of varlist()", {
  expect_equal(vl(mtcars, tbl = TRUE), varlist(mtcars, tbl = TRUE))
})

test_that("varlist() throws error for non-data.frame input", {
  expect_error(varlist(1:10), "only works with named data frames")
})

test_that("varlist_title() returns VARLIST name for simple object", {
  dummy_expr <- quote(iris)
  expect_equal(varlist_title(dummy_expr), "VARLIST iris")
})

test_that("varlist_title() adds * for transformed object", {
  transformed_expr <- quote(head(iris))
  expect_equal(varlist_title(transformed_expr), "VARLIST iris*")
})



