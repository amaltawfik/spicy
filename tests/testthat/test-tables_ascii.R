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

test_that("build_ascii_table accepts a numeric `padding` of any non-negative value", {
  df <- data.frame(A = 1:2, B = c("x", "y"))
  txt0 <- build_ascii_table(df, padding = 0L)
  txt2 <- build_ascii_table(df, padding = 2L)
  txt9 <- build_ascii_table(df, padding = 9L)
  expect_type(txt0, "character")
  expect_type(txt2, "character")
  expect_type(txt9, "character")
  # Larger padding -> longer lines
  width0 <- max(nchar(strsplit(txt0, "\n", fixed = TRUE)[[1]]))
  width2 <- max(nchar(strsplit(txt2, "\n", fixed = TRUE)[[1]]))
  width9 <- max(nchar(strsplit(txt9, "\n", fixed = TRUE)[[1]]))
  expect_lt(width0, width2)
  expect_lt(width2, width9)
})

test_that("build_ascii_table rejects the legacy string `padding` choices with a migration error", {
  df <- data.frame(A = 1:2, B = c("x", "y"))
  expect_error(
    build_ascii_table(df, padding = "compact"),
    "non-negative integer.+removed in spicy 0\\.11\\.0"
  )
  expect_error(
    build_ascii_table(df, padding = "normal"),
    "non-negative integer.+removed in spicy 0\\.11\\.0"
  )
  expect_error(
    build_ascii_table(df, padding = "wide"),
    "non-negative integer.+removed in spicy 0\\.11\\.0"
  )
})

test_that("build_ascii_table rejects negative or non-finite `padding`", {
  df <- data.frame(A = 1:2, B = c("x", "y"))
  expect_error(
    build_ascii_table(df, padding = -1L),
    "non-negative integer"
  )
  expect_error(
    build_ascii_table(df, padding = NA_integer_),
    "non-negative integer"
  )
  expect_error(
    build_ascii_table(df, padding = c(2L, 3L)),
    "non-negative integer"
  )
})

test_that("spicy_print_table forwards the new `padding` semantics", {
  df <- data.frame(A = 1:2, B = c("x", "y"))
  expect_no_error(spicy_print_table(df, padding = 0L))
  expect_no_error(spicy_print_table(df, padding = 5L))
  expect_error(
    spicy_print_table(df, padding = "normal"),
    "non-negative integer.+removed in spicy 0\\.11\\.0"
  )
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

test_that("build_ascii_table honours `total_row_idx` and supports `group_sep_rows`", {
  df <- data.frame(
    Group = c("A1", "A2", "B1", "B2"),
    Count = c("10", "20", "5", "15")
  )
  # Explicit total_row_idx places the rule before row 4
  txt <- build_ascii_table(
    df,
    padding = 0L,
    total_row_idx = 4L,
    group_sep_rows = 3L
  )
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  # group_sep_rows = 3 inserts a light dashed rule before row 3
  expect_true(any(grepl("╌", lines)))
  # total_row_idx = 4 inserts a heavy rule before row 4 (the bottom group)
  rule_positions <- grep("^[─╌┼│\\[]+$", lines)
  expect_gte(length(rule_positions), 2L) # header rule + group rule + total rule
})

test_that("`total_row_idx = integer(0)` suppresses the regex fallback", {
  # When the user provides `total_row_idx` explicitly (even as empty),
  # the grep fallback never runs, so a category literally named "Total"
  # cannot trigger a stray separator line.
  df <- data.frame(
    Item = c("Sub Total", "Real total here"),
    Count = c("5", "10")
  )
  txt <- build_ascii_table(df, padding = 0L, total_row_idx = integer(0))
  expect_type(txt, "character")
  # The output has the header rule but no body separator rules
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  body_rule_count <- sum(grepl("^[─┼]+$", lines))
  expect_equal(body_rule_count, 1L) # header rule only
})

test_that("spicy_print_table reads `total_row_idx` from the input attribute", {
  df <- data.frame(
    Item = c("a", "b", "Total"),
    Count = c("1", "2", "3")
  )
  attr(df, "total_row_idx") <- 3L
  out <- capture.output(spicy_print_table(df))
  expect_type(out, "character")
  # Reading via attr should still draw the rule before row 3 ("Total")
  expect_true(any(grepl("^[─┼]+$", out)))
})

test_that("build_ascii_table handles single-column input", {
  df <- data.frame(Only = c("a", "b", "c"))
  txt <- build_ascii_table(df)
  expect_type(txt, "character")
  expect_no_error(spicy_print_table(df))
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
      padding = 0L,
      align_left_cols = c(1L, 2L)
    )
  )

  expect_gt(sum(grepl("Variable", output, fixed = TRUE)), 1L)
  expect_true(any(grepl("Count", output, fixed = TRUE)))
  expect_true(any(grepl("Percent", output, fixed = TRUE)))
})
