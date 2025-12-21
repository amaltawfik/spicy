test_that("code_book() runs without error on a simple data frame", {
  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  df <- head(mtcars)

  expect_silent({
    cb <- suppressMessages(code_book(df))
  })

  expect_s3_class(cb, "datatables")
  expect_true(inherits(cb, "htmlwidget"))
})
