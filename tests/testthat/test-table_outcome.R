# table_outcome(): one continuous outcome across the levels of several
# categorical variables, one block per variable.

test_that("the title names the outcome and nothing else", {
  # Decision 32: the grouping variables ARE the rows, so a title that
  # listed them would repeat the stub.
  expect_identical(
    spicy:::.outcome_title("Body mass index"),
    "Descriptive statistics of Body mass index"
  )
  # One `by` or six, the title is the same: the geometry is what
  # changes, not the subject of the table.
  expect_identical(
    spicy:::.outcome_title("Age (years)"),
    "Descriptive statistics of Age (years)"
  )
})

test_that("the Excel sheet name is resolved from the registry", {
  # Decision 16: `excel_sheet = NULL` keeps the \usage line clean.
  expect_identical(spicy:::.outcome_excel_sheet(NULL), "Outcome")
  expect_identical(spicy:::.outcome_excel_sheet("Mine"), "Mine")
})

test_that("the structure notes say what the table does not adjust", {
  notes <- spicy:::.outcome_structure_notes("Body mass index", TRUE, TRUE)
  expect_length(notes, 2L)
  expect_match(notes[[1L]], "Body mass index", fixed = TRUE)
  expect_match(notes[[1L]], "not adjusted for one another", fixed = TRUE)
  expect_match(notes[[2L]], "whole analytic sample", fixed = TRUE)

  # Owed only when there is something to disclose.
  expect_identical(
    spicy:::.outcome_structure_notes("x", FALSE, TRUE),
    spicy_str("note_outcome_overall")
  )
  expect_identical(
    spicy:::.outcome_structure_notes("x", FALSE, FALSE),
    NULL
  )
})

test_that("Overall is not the word Total, and that is deliberate", {
  # Decision 32bis. "Total" is the word of a COUNT margin, where
  # frequencies add up; this row is the whole analytic sample, where a
  # mean is recomputed and nothing is added.
  expect_identical(spicy:::.outcome_overall_label(), "Overall")
  expect_identical(spicy_str("label_total"), "Total")
  expect_false(identical(
    spicy_str("row_overall"),
    spicy_str("header_margin_total")
  ))
})
