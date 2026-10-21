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

test_that("a cell note added to a spicy tinytable is one grid row per line in markdown", {
  skip_if_not_installed("tinytable")
  cell <- list(star = list(i = 1, j = 2, text = "First line\nsecond line"))
  # The rows of the markdown grid, without their borders. The grid stays
  # valid: every rule opens and closes with "+", every row with "|".
  grid_rows <- function(tt) {
    md <- tinytable::save_tt(tt, output = "markdown")
    lines <- strsplit(md, "\n", fixed = TRUE)[[1L]]
    grid <- lines[nzchar(lines) & !startsWith(lines, "Table: ")]
    expect_true(all(grepl("^[+][-=+]+[+]$|^[|].*[|]$", grid)))
    trimws(sub("^[|](.*)[|]$", "\\1", grid[startsWith(grid, "|")]))
  }

  tc <- table_continuous(sochealth, select = bmi, output = "tinytable")
  tc@notes <- c(tc@notes, cell)
  rows <- grid_rows(tc)
  # Each line of the cell note is a row of its own, the first with the
  # marker, which tinytable also sets in the cell.
  expect_identical(
    utils::tail(rows, 2L),
    c("^star^ First line", "second line")
  )
  expect_identical(sum(grepl("^star^", rows, fixed = TRUE)), 2L)

  # Beside a table note of two lines, the legend of the stars on the
  # second, both notes get the same rows.
  tr <- table_regression(
    lm(wellbeing_score ~ age, data = sochealth),
    stars = TRUE,
    output = "tinytable"
  )
  legend <- utils::tail(spicy:::.note_lines(tr@notes[[1L]]), 1L)
  tr@notes <- c(tr@notes, cell)
  expect_identical(
    utils::tail(grid_rows(tr), 3L),
    c(legend, "^star^ First line", "second line")
  )
})

test_that("a spicy tinytable with no note gets no note row in markdown", {
  skip_if_not_installed("tinytable")
  # `ci = FALSE`: with an interval the note would define CI, LL and UL.
  tt <- table_continuous(
    data.frame(x = c(1, 2, 3, 4)),
    select = x,
    ci = FALSE,
    output = "tinytable"
  )
  expect_null(tt@notes)
  md <- tinytable::save_tt(tt, output = "markdown")
  lines <- strsplit(md, "\n", fixed = TRUE)[[1L]]
  # Every row of the grid is a row of the table: none spans the whole
  # width, as a note row does.
  rows <- lines[startsWith(lines, "|")]
  expect_true(all(lengths(gregexpr("|", rows, fixed = TRUE)) > 2L))
})

test_that("a typed note keeps the indentation of its lines in HTML and Typst", {
  skip_if_not_installed("tinytable")
  # A note the user types is printed as typed: a line indented under the
  # one before keeps its indentation, as no-break spaces, where HTML and
  # Typst would collapse it.
  tt <- table_regression(
    lm(wellbeing_score ~ age, data = sochealth),
    note = "First line\n  indented line",
    output = "tinytable"
  )
  expect_match(
    tinytable::save_tt(tt, output = "html"),
    "First line<br>&nbsp;&nbsp;indented line",
    fixed = TRUE
  )
  expect_match(
    tinytable::save_tt(tt, output = "typst"),
    "~~indented line",
    fixed = TRUE
  )
})
