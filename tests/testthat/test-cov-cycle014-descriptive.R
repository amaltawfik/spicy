# The shared checks and helpers of the descriptive tables: the file path
# of a written output, `verbose`, a missing column, the long and data-frame
# builders, the untitled table, and the degraded test of table_outcome().

test_that("table_continuous_lm() says that `contrast` reaches nothing with a numeric `by`", {
  w <- expect_warning(
    table_continuous_lm(sochealth, select = bmi, by = age, contrast = "none"),
    class = "spicy_ignored_arg"
  )
  expect_match(
    conditionMessage(w),
    "`contrast` is ignored when `by` is numeric: a slope has no contrast.",
    fixed = TRUE
  )
})

test_that("a file path that names a directory is refused", {
  dir <- withr::local_tempdir()
  xlsx <- file.path(dir, "table.xlsx")
  docx <- file.path(dir, "table.docx")
  dir.create(xlsx)
  dir.create(docx)
  err <- expect_error(
    table_regression(
      lm(mpg ~ wt, data = mtcars),
      output = "excel",
      excel_path = xlsx
    ),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "`excel_path` names a directory, not a file:",
    fixed = TRUE
  )
  err <- expect_error(
    table_continuous(
      sochealth,
      select = bmi,
      output = "word",
      word_path = docx
    ),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "`word_path` names a directory, not a file:",
    fixed = TRUE
  )
})

test_that("`verbose = TRUE` says nothing when every selected column is numeric", {
  expect_no_message(
    table_continuous(sochealth, select = c(bmi, age), verbose = TRUE)
  )
  expect_message(
    table_continuous(sochealth, select = c(bmi, sex), verbose = TRUE),
    "Ignored non-numeric columns: sex.",
    fixed = TRUE,
    class = "spicy_info"
  )
})

test_that("a column name that is empty or NA gets no suggestion", {
  err <- expect_error(
    table_continuous(sochealth, select = bmi, by = ""),
    class = "spicy_missing_column"
  )
  expect_match(
    conditionMessage(err),
    "`by` names \"\", which is not a column of `data`.",
    fixed = TRUE
  )
  expect_no_match(conditionMessage(err), "Did you mean", fixed = TRUE)
  err <- expect_error(
    table_continuous(sochealth, select = bmi, by = NA_character_),
    class = "spicy_missing_column"
  )
  expect_no_match(conditionMessage(err), "Did you mean", fixed = TRUE)
})

test_that(".insert_cols_after() keeps the frame and its attributes", {
  df <- structure(data.frame(a = 1:2, b = 3:4), note = "kept")
  expect_identical(spicy:::.insert_cols_after(df, list(), "a"), df)
  # An anchor the frame lacks puts the columns at the end.
  out <- spicy:::.insert_cols_after(df, list(z = c("p", "q")), "absent")
  expect_identical(names(out), c("a", "b", "z"))
  expect_identical(out$z, c("p", "q"))
  expect_identical(attr(out, "note"), "kept")
})

test_that(".continuous_df_parts() gives NA for a part the frame does not carry", {
  tbl <- table_continuous(
    sochealth,
    select = bmi,
    by = education,
    statistic = TRUE
  )
  st <- as_structured(tbl)
  key <- spicy:::.CON_KEY_TEST
  frame <- unclass(tbl)
  full <- spicy:::.continuous_df_parts(st, frame)[[key]]
  frame$df2 <- NULL
  part <- spicy:::.continuous_df_parts(st, frame)[[key]]
  # F(df1, df2): the second degrees of freedom are on the test cell.
  expect_false(all(is.na(full$df.residual)))
  expect_identical(part$df, full$df)
  expect_identical(part$df.residual, rep(NA_real_, nrow(st$body)))
})

test_that("an untitled table has no caption in a rendered output", {
  skip_if_not_installed("tinytable")
  d <- data.frame(
    a = c(TRUE, FALSE, TRUE, TRUE),
    b = c(FALSE, TRUE, TRUE, FALSE)
  )
  caption <- function(title) {
    table_multiresponse(
      d,
      select = c(a, b),
      title = title,
      output = "tinytable"
    )@caption
  }
  expect_identical(caption(NULL), "Multiple responses")
  expect_null(caption(FALSE))
  expect_null(caption(""))
})

test_that("table_outcome() turns a group test that errors into NA, with a warning", {
  # The outcome varies, but not within the groups: t.test() stops on it.
  d <- data.frame(
    y = c(1, 1, 1, 2, 2, 2),
    g = c("a", "a", "a", "b", "b", "b")
  )
  w <- expect_warning(
    tbl <- table_outcome(d, y, select = g),
    class = "spicy_undefined_stat"
  )
  expect_match(
    conditionMessage(w),
    "The group comparison failed for `g` (",
    fixed = TRUE
  )
  expect_match(conditionMessage(w), "); its test columns are NA.", fixed = TRUE)
  expect_true(all(is.na(as.data.frame(tbl)$p)))
})
