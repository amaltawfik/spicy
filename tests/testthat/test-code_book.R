test_that("code_book() runs without error on a simple data frame", {
  skip_if_not_installed("DT")

  df <- head(mtcars)

  expect_silent({
    cb <- suppressMessages(code_book(df))
  })

  expect_s3_class(cb, "datatables")
  expect_true(inherits(cb, "htmlwidget"))
})

test_that("code_book() works with values = TRUE", {
  skip_if_not_installed("DT")
  cb <- suppressMessages(code_book(head(iris), values = TRUE))
  expect_s3_class(cb, "datatables")
})

test_that("code_book() works with include_na = TRUE", {
  skip_if_not_installed("DT")
  df <- data.frame(x = c(1, NA, 3), y = c("a", "b", NA))
  cb <- suppressMessages(code_book(df, include_na = TRUE))
  expect_s3_class(cb, "datatables")
})

test_that("code_book() accepts custom title", {
  skip_if_not_installed("DT")
  cb <- suppressMessages(code_book(head(mtcars), title = "My Codebook"))
  expect_s3_class(cb, "datatables")
})

test_that("code_book() works with labelled data", {
  skip_if_not_installed("DT")
  skip_if_not_installed("labelled")
  df <- data.frame(x = labelled::labelled(1:3, labels = c(A = 1, B = 2, C = 3)))
  cb <- suppressMessages(code_book(df))
  expect_s3_class(cb, "datatables")
})

test_that("code_book() errors on non-data.frame", {
  skip_if_not_installed("DT")
  expect_error(code_book(1:10))
})

test_that("code_book() errors when DT is not available", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "DT") FALSE else TRUE,
    .package = "base"
  )
  expect_error(code_book(mtcars), "Package 'DT' is required")
})

test_that("code_book() passes values and include_na to varlist", {
  skip_if_not_installed("DT")
  df <- data.frame(x = c(1, NA, 3), y = c("a", "b", NA))
  cb <- suppressMessages(code_book(df, values = TRUE, include_na = TRUE))
  expect_s3_class(cb, "datatables")
})
