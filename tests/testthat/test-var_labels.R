# var_labels(): the fourth form of `labels`, the attribute base with a few
# renames whatever options(spicy.var_labels) says.

test_that("var_labels() returns a classed named character vector", {
  x <- var_labels(bmi = "IMC", age = "Age")
  expect_identical(class(x), c("spicy_var_labels", "character"))
  expect_identical(unclass(x), c(bmi = "IMC", age = "Age"))
  empty <- var_labels()
  expect_s3_class(empty, "spicy_var_labels")
  expect_length(empty, 0L)
  renames <- c(bmi = "IMC", age = "Age")
  expect_identical(var_labels(!!!renames), x)
})

test_that("print() shows the named vector without its class", {
  x <- var_labels(bmi = "IMC", age = "Age")
  out <- capture.output(res <- print(x))
  expect_identical(out, capture.output(print(c(bmi = "IMC", age = "Age"))))
  expect_false(any(grepl("spicy_var_labels", out, fixed = TRUE)))
  expect_identical(res, x)
})

test_that("var_labels() refuses what the labels contract refuses", {
  expect_error(var_labels("IMC"), class = "spicy_invalid_input")
  expect_error(var_labels(bmi = "IMC", "Age"), class = "spicy_invalid_input")
  expect_error(var_labels(bmi = 1), class = "spicy_invalid_input")
  expect_error(var_labels(bmi = c("a", "b")), class = "spicy_invalid_input")
  expect_error(var_labels(bmi = NA_character_), class = "spicy_invalid_input")
})

test_that("var_labels() reads the attributes whatever the option says", {
  df <- data.frame(a = 1:5, b = 6:10, n = 1:5)
  attr(df$a, "label") <- "Alpha"
  attr(df$b, "label") <- "Beta"
  lab <- function(...) table_continuous(df, output = "data.frame", ...)$label
  expect_identical(lab(labels = var_labels()), lab(labels = TRUE))
  expect_warning(
    expect_identical(
      lab(labels = var_labels(b = "B", zz = "ignored")),
      c("Alpha", "B", "n")
    ),
    class = "spicy_unknown_name"
  )
  withr::local_options(spicy.var_labels = FALSE)
  expect_identical(lab(labels = var_labels()), c("Alpha", "Beta", "n"))
  expect_identical(lab(labels = var_labels(b = "B")), c("Alpha", "B", "n"))
  # The option is not read, so an invalid one does not stand in the way.
  withr::local_options(spicy.var_labels = "yes")
  expect_identical(lab(labels = var_labels(b = "B")), c("Alpha", "B", "n"))
  # The option itself must stay TRUE or FALSE.
  withr::local_options(spicy.var_labels = var_labels(b = "B"))
  err <- expect_error(lab(), class = "spicy_invalid_input")
  expect_match(conditionMessage(err), "spicy.var_labels", fixed = TRUE)
})
