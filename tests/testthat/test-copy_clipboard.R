test_that("copy_clipboard() works correctly", {
  df <- data.frame(
    col1 = c("A", "B", "C"),
    col2 = c(1, 2, 3)
  )

  mat <- matrix(1:9, nrow = 3, byrow = TRUE, dimnames = list(c("Row1", "Row2", "Row3"), c("Col1", "Col2", "Col3")))

  tab <- table(gender = c("Male", "Female", "Female", "Male", "Male", "Female"), city = c("Paris", "London", "Paris", "London", "Paris", "London"))

  expect_silent(copy_clipboard(df))
  expect_silent(copy_clipboard(df, row.names.as.col = TRUE))
  expect_silent(copy_clipboard(df, row.names.as.col = FALSE))
  expect_silent(copy_clipboard(df, col.names = FALSE))
  expect_silent(copy_clipboard(mat))
  expect_silent(copy_clipboard(mat, row.names.as.col = TRUE))
  expect_silent(copy_clipboard(mat, row.names.as.col = FALSE))
  expect_silent(copy_clipboard(mat, col.names = FALSE))
  expect_silent(copy_clipboard(mat))
  expect_silent(copy_clipboard(tab, row.names.as.col = TRUE))
  expect_silent(copy_clipboard(tab, row.names.as.col = FALSE))
  expect_silent(copy_clipboard(tab, col.names = FALSE))
})
