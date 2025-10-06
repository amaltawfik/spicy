# ---- Basic structure test -----------------------------------------------------
test_that("code_book() returns a valid interactive datatable", {
  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  df <- head(mtcars)

  cb <- suppressMessages(code_book(df, title = "Test Codebook"))

  expect_s3_class(cb, "datatables")
  expect_true(inherits(cb, "htmlwidget"))
  expect_true(all(c("Variable", "Label", "Values", "Class", "N_valid", "NAs") %in%
    names(cb$x$data)))

  # Caption should contain the provided title
  caption <- cb$x$caption
  expect_false(is.null(caption))
  expect_match(as.character(caption), "Test Codebook", fixed = TRUE)
})


# ---- Invalid input test -------------------------------------------------------
test_that("code_book() fails with invalid inputs", {
  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  expect_error(code_book(1:10), "`x` must be a data frame or tibble")
})


# ---- Null title test ----------------------------------------------------------
test_that("code_book() handles NULL titles correctly", {
  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  df <- head(mtcars)
  cb <- suppressMessages(code_book(df, title = NULL))

  # Caption should be NULL or empty when title is not provided
  caption <- cb$x$caption
  caption_text <- as.character(caption)
  expect_true(is.null(caption) || identical(caption, "") || !grepl("<h[1-6]>", caption_text))
})


# ---- Custom title test --------------------------------------------------------
test_that("code_book() displays caption when title is provided", {
  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  df <- head(mtcars)

  cb <- suppressMessages(code_book(df, title = "Custom Codebook"))
  expect_s3_class(cb, "datatables")
  expect_true(inherits(cb, "htmlwidget"))

  caption <- cb$x$caption
  expect_false(is.null(caption) || identical(caption, ""))

  caption_text <- as.character(caption)
  expect_true(grepl("Custom Codebook", caption_text))
  expect_true(!grepl("<h[1-6]>", caption_text) || grepl("<h3>Custom Codebook", caption_text))
})


# ---- Buttons configuration test ----------------------------------------------
test_that("code_book() includes expected Buttons configuration", {
  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  df <- head(mtcars)
  cb <- suppressMessages(code_book(df))

  btns <- cb$x$options$buttons

  # Ensure presence of basic and grouped download buttons
  expect_true(any(vapply(btns, function(b) {
    (is.character(b) && b %in% c("copy", "print")) ||
      (is.list(b) && identical(b$text, "Download"))
  }, logical(1))))
})
