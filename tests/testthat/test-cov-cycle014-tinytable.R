# The tinytable finalizers: the Typst widening of the column-group labels,
# and the notes of every shape tinytable accepts.

test_that("Typst widens no column for labels that span two lines", {
  skip_if_not_installed("tinytable")
  m1 <- lm(wellbeing_score ~ age, data = sochealth)
  m2 <- lm(wellbeing_score ~ age + sex, data = sochealth)
  typst <- function(models) {
    tinytable::save_tt(
      table_regression(models, output = "tinytable"),
      output = "typst"
    )
  }
  one_line <- typst(list(A = m1, B = m2))
  two_lines <- typst(list("Crude\nmodel" = m1, "Adjusted\nmodel" = m2))
  # Both tables measure their columns ...
  anchor <- "let tinytable-naturals = tinytable-coldata.map("
  expect_match(one_line, anchor, fixed = TRUE)
  expect_match(two_lines, anchor, fixed = TRUE)
  # ... and only a label on one line is measured against them.
  expect_match(one_line, "((1, 3, [A]), (4, 6, [B]),)", fixed = TRUE)
  expect_no_match(two_lines, "spicy_label", fixed = TRUE)
})

test_that(".spicy_map_notes() maps the text of a note given as a list", {
  cell <- list(i = 1, j = 2, text = "First line\nsecond line")
  expect_identical(
    spicy:::.spicy_map_notes(list(star = cell, "Plain"), toupper),
    list(star = list(i = 1, j = 2, text = "FIRST LINE\nSECOND LINE"), "PLAIN")
  )
  expect_identical(
    spicy:::.spicy_map_notes(list(text = "a", i = 1, j = 1), toupper),
    list(text = "A", i = 1, j = 1)
  )
})

test_that("a cell note added to a spicy tinytable is one line in markdown", {
  skip_if_not_installed("tinytable")
  tt <- table_continuous(sochealth, select = bmi, output = "tinytable")
  tt@notes <- c(
    tt@notes,
    list(star = list(i = 1, j = 2, text = "First line\nsecond line"))
  )
  md <- tinytable::save_tt(tt, output = "markdown")
  expect_match(md, "First line second line", fixed = TRUE)
  expect_no_match(md, "First line\n", fixed = TRUE)
})
