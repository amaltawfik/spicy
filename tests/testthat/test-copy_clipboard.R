test_that("copy_clipboard() works silently for different structures", {
  skip_if_not(clipr::clipr_available())

  df <- data.frame(
    col1 = c("A", "B", "C"),
    col2 = c(1, 2, 3)
  )

  mat <- matrix(1:9, nrow = 3, byrow = TRUE,
                dimnames = list(c("Row1", "Row2", "Row3"),
                                c("Col1", "Col2", "Col3")))

  tab <- table(
    gender = c("Male", "Female", "Female", "Male", "Male", "Female"),
    city = c("Paris", "London", "Paris", "London", "Paris", "London")
  )

  arr <- array(1:8, dim = c(2, 2, 2))

  vec_num <- c(5.5, 6.6, 7.7)
  vec_chr <- c("alpha", "beta", "gamma")

  # Silent execution
  expect_silent(copy_clipboard(df, quiet = TRUE))
  expect_silent(copy_clipboard(df, row.names.as.col = TRUE, quiet = TRUE))
  expect_silent(copy_clipboard(df, col.names = FALSE, quiet = TRUE))

  expect_silent(copy_clipboard(mat, quiet = TRUE))
  expect_silent(copy_clipboard(mat, row.names.as.col = TRUE, quiet = TRUE))
  expect_silent(copy_clipboard(mat, col.names = FALSE, quiet = TRUE))

  expect_silent(copy_clipboard(tab, quiet = TRUE))
  expect_silent(copy_clipboard(tab, row.names.as.col = TRUE, quiet = TRUE))

  expect_silent(copy_clipboard(arr, quiet = TRUE))

  expect_silent(copy_clipboard(vec_num, quiet = TRUE))
  expect_silent(copy_clipboard(vec_chr, quiet = TRUE))
})

test_that("copy_clipboard() copies expected content", {
  skip_if_not(clipr::clipr_available())

  # Data frame
  df <- data.frame(
    name = c("Alice", "Bob"),
    score = c(10, 15)
  )

  copy_clipboard(df, quiet = TRUE)
  clip <- clipr::read_clip()

  expect_length(clip, 3)
  expect_true(grepl("name\\tscore", clip[1]))
  expect_true(grepl("Alice\\t10", clip[2]))
  expect_true(grepl("Bob\\t15", clip[3]))

  # Matrix
  mat <- matrix(1:4, nrow = 2, byrow = TRUE,
                dimnames = list(c("r1", "r2"), c("c1", "c2")))

  copy_clipboard(mat, quiet = TRUE)
  mat_clip <- clipr::read_clip()
  expect_length(mat_clip, 3)
  expect_true(grepl("c1\\tc2", mat_clip[1]))

  # Numeric vector
  vec_num <- c(3.14, 2.71, 1.618)
  copy_clipboard(vec_num, quiet = TRUE)
  num_clip <- clipr::read_clip()
  expect_equal(num_clip, as.character(vec_num))

  # Character vector
  vec_chr <- c("apple", "banana", "cherry")
  copy_clipboard(vec_chr, quiet = TRUE)
  chr_clip <- clipr::read_clip()
  expect_equal(chr_clip, vec_chr)
})
