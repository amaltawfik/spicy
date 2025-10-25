test_that("build_ascii_table produces aligned ASCII output", {
  df <- data.frame(A = 1:2, B = c("x", "yy"))
  txt <- build_ascii_table(df)

  expect_type(txt, "character")
  expect_true(any(grepl("\u2502", txt))) # has vertical bars
  expect_true(any(grepl("\u2500", txt))) # has horizontal lines
  expect_no_error(build_ascii_table(df))
})


test_that("spicy_print_table prints and returns invisibly", {
  df <- data.frame(Category = "Valid", Values = "A", Freq. = 1)

  # Test invisibility directly
  expect_invisible(spicy_print_table(df, title = "Test Title"))

  # Test capture of printed output (for robustness)
  output <- capture.output(spicy_print_table(df, title = "Test Title"))
  expect_true(any(grepl("Test Title", output)))
})



test_that("spicy_print_table aligns Category and Values left", {
  df <- data.frame(Category = c("Valid", "Total"), Values = c("A", "B"), Freq. = c(1, 2))
  output <- capture.output(spicy_print_table(df))

  # Rough alignment test (spaces after "Valid")
  expect_true(any(grepl("^ Valid", output)))
})
