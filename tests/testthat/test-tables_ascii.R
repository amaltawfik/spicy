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
  df <- data.frame(
    Category = c("Valid", "Total"),
    Values = c("A", "B"),
    Freq. = c(1, 2)
  )
  output <- capture.output(spicy_print_table(df))

  # Rough alignment test (spaces after "Valid")
  expect_true(any(grepl("^ Valid", output)))
})

test_that("build_ascii_table supports wide padding", {
  df <- data.frame(A = 1:2, B = c("x", "y"))
  txt <- build_ascii_table(df, padding = "wide")
  expect_type(txt, "character")
})

test_that("build_ascii_table supports bottom_line", {
  df <- data.frame(A = 1:2, B = c("x", "y"))
  txt <- build_ascii_table(df, bottom_line = TRUE)
  expect_true(grepl("\u2534", txt))
})

test_that("print.spicy_categorical_table falls back to x when display_df is absent", {
  x <- data.frame(
    Variable = c("Smoking", "  Yes"),
    n = c("10", "10"),
    check.names = FALSE
  )
  class(x) <- c("spicy_categorical_table", "spicy_table", "data.frame")
  attr(x, "data_name") <- "demo"
  attr(x, "indent_text") <- "  "

  output <- capture.output(print(x))

  expect_true(any(grepl("Categorical table", output, fixed = TRUE)))
})

test_that("print.spicy_categorical_table uses grouped title and compact padding", {
  withr::local_options(list(width = 18))

  x <- data.frame(
    Variable = c("Var 1", "  Yes", "Var 2"),
    n = c("12", "8", "9"),
    check.names = FALSE
  )
  class(x) <- c("spicy_categorical_table", "spicy_table", "data.frame")
  attr(x, "display_df") <- x
  attr(x, "data_name") <- "demo"
  attr(x, "group_var") <- "education"
  attr(x, "indent_text") <- "  "

  output <- capture.output(print(x))

  expect_true(any(grepl(
    "Categorical table by education",
    output,
    fixed = TRUE
  )))
})

test_that("spicy_print_table splits wide tables into stacked panels", {
  withr::local_options(list(width = 26))

  df <- data.frame(
    Variable = c("Smoking", "Activity"),
    Group = c("Women", "Men"),
    Count = c("120", "98"),
    Percent = c("52.3", "47.7"),
    check.names = FALSE
  )

  output <- capture.output(
    spicy_print_table(
      df,
      title = NULL,
      padding = "compact",
      align_left_cols = c(1L, 2L)
    )
  )

  expect_gt(sum(grepl("Variable", output, fixed = TRUE)), 1L)
  expect_true(any(grepl("Count", output, fixed = TRUE)))
  expect_true(any(grepl("Percent", output, fixed = TRUE)))
})
