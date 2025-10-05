test_that("code_book() returns a valid interactive datatable", {
  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  df <- head(mtcars)

  cb <- suppressMessages(code_book(df, title = "Test Codebook"))

  expect_s3_class(cb, "datatables")
  expect_true(inherits(cb, "htmlwidget"))
  expect_true(all(c("Variable", "Label", "Values", "Class", "N_valid", "NAs") %in%
    names(cb$x$data)))

  if (!is.null(cb$x$options$caption)) {
    expect_match(as.character(cb$x$options$caption), "Test Codebook", fixed = TRUE)
  }
})

test_that("code_book() fails with invalid inputs", {
  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  # Invalid input type
  expect_error(code_book(1:10), "`x` must be a data frame or tibble")
})

test_that("code_book() handles NULL titles correctly", {
  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  df <- head(mtcars)
  cb <- suppressMessages(code_book(df, title = NULL))

  caption <- as.character(cb$x$options$caption)
  if (is.null(caption) || all(is.na(caption))) {
    expect_true(TRUE) # no caption, pass
  } else {
    expect_false(grepl("h3", caption))
  }
})
