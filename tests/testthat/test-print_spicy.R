test_that("print.spicy prints a three-line table", {
  df <- data.frame(Values = c("A", "B", "C"), N = c(10, 20, 30))
  attr(df, "title") <- "Test Table"

  output <- capture.output(print.spicy(df))

  expect_true(any(grepl("Test Table", output)))

  expect_true(sum(grepl("\u2500", output)) >= 3)

  expect_true(any(grepl("Values", output)))
  expect_true(any(grepl("N", output)))

  expect_true(any(grepl("A", output)))
  expect_true(any(grepl("B", output)))
  expect_true(any(grepl("C", output)))
})

test_that("print.spicy aligns Values column correctly", {
  df <- data.frame(Values = c("Short", "A much longer value"), N = c(10, 20))

  output <- capture.output(print.spicy(df))

  table_rows <- output[!grepl("\u2500", output)]

  data_rows <- tail(table_rows, -2)

  values_column <- sub("\\s+.*", "", data_rows)
  expect_false(any(grepl("^\\s", values_column)))
})

test_that("print.spicy prints a note when present", {
  df <- data.frame(Values = c("X", "Y"), N = c(5, 10))
  attr(df, "title") <- "Test Table"
  attr(df, "note") <- "This is a note."

  output <- capture.output(print.spicy(df))

  expect_true(any(grepl("This is a note.", output)))
})

test_that("print.spicy works with matrices", {
  mat <- matrix(c("A", "B", "C", 10, 20, 30), ncol = 2)
  colnames(mat) <- c("Values", "N")

  output <- capture.output(print.spicy(mat))

  expect_true(any(grepl("Values", output)))
  expect_true(any(grepl("N", output)))
  expect_true(any(grepl("A", output)))
  expect_true(any(grepl("B", output)))
  expect_true(any(grepl("C", output)))
})
