test_that("label_from_names splits at first sep and assigns labels", {
  df <- data.frame(a = 1:2, b = 3:4)
  names(df) <- c("age. Age", "score. Score total. Computed")

  out <- label_from_names(df)

  expect_equal(names(out), c("age", "score"))

  labs <- labelled::var_label(out)

  expect_equal(labs[["age"]], "Age")
  expect_equal(labs[["score"]], "Score total. Computed")
})

test_that("empty or missing labels are skipped", {
  df <- data.frame(x = 1:2, y = 3:4)
  names(df) <- c("x", "y. ")

  out <- label_from_names(df)
  labs <- labelled::var_label(out)

  expect_true(
    is.null(labs[["x"]]) ||
      length(labs[["x"]]) == 0 ||
      is.na(labs[["x"]]) ||
      identical(labs[["x"]], "NA") ||
      labs[["x"]] == ""
  )

  expect_true(
    is.null(labs[["y"]]) ||
      length(labs[["y"]]) == 0 ||
      labs[["y"]] == "" ||
      is.na(labs[["y"]]) ||
      identical(labs[["y"]], "NA")
  )
})

test_that("label_from_names errors on non-data.frame input", {
  expect_error(
    label_from_names(1:3),
    "`df` must be a data.frame or tibble.",
    fixed = TRUE
  )
})

test_that("label_from_names validates sep parameter", {
  df <- data.frame(x = 1)
  expect_error(label_from_names(df, sep = ""), "sep")
  expect_error(label_from_names(df, sep = NA_character_), "sep")
  expect_error(label_from_names(df, sep = 1), "sep")
  expect_error(label_from_names(df, sep = c(".", ",")), "sep")
})
