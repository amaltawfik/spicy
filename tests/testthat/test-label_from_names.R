# ---- happy path --------------------------------------------------------

test_that("label_from_names splits at first sep and assigns labels", {
  df <- data.frame(a = 1:2, b = 3:4)
  names(df) <- c("age. Age", "score. Score total. Computed")

  out <- label_from_names(df)

  expect_equal(names(out), c("age", "score"))
  expect_equal(attr(out[["age"]], "label"), "Age")
  # First-sep semantics: the second `. ` belongs to the label.
  expect_equal(attr(out[["score"]], "label"), "Score total. Computed")
})

test_that("`labelled::var_label()` reads the labels set via attr()", {
  skip_if_not_installed("labelled")
  df <- data.frame(a = 1:2)
  names(df) <- "age. Age of respondent"
  out <- label_from_names(df)
  expect_equal(
    labelled::var_label(out)[["age"]],
    "Age of respondent"
  )
})

# ---- edge cases on labels ----------------------------------------------

test_that("empty / whitespace-only labels are skipped (no `label` attribute)", {
  df <- data.frame(a = 1:2, b = 3:4, c = 5:6)
  names(df) <- c("x", "y. ", "z.    ")
  out <- label_from_names(df)
  expect_equal(names(out), c("x", "y", "z"))
  expect_null(attr(out[["x"]], "label"))
  expect_null(attr(out[["y"]], "label"))
  expect_null(attr(out[["z"]], "label"))
})

test_that("names without `sep` are passed through with no label", {
  df <- data.frame(plain = 1:2)
  out <- label_from_names(df)
  expect_equal(names(out), "plain")
  expect_null(attr(out[["plain"]], "label"))
})

test_that("first-`sep` semantics keep the rest of the label intact", {
  df <- data.frame(a = 1:2)
  names(df) <- "code. label . with . dots"
  out <- label_from_names(df)
  expect_equal(names(out), "code")
  expect_equal(attr(out[["code"]], "label"), "label . with . dots")
})

# ---- new name hygiene --------------------------------------------------

test_that("new column names are trimmed of trailing whitespace", {
  df <- data.frame(a = 1:2)
  names(df) <- "age . Age"
  out <- label_from_names(df)
  expect_equal(names(out), "age")
  expect_equal(attr(out[["age"]], "label"), "Age")
})

# ---- error paths -------------------------------------------------------

test_that("non-data.frame input errors clearly", {
  expect_error(
    label_from_names(1:3),
    "`df` must be a data.frame or tibble.",
    fixed = TRUE
  )
})

test_that("`sep` argument is validated", {
  df <- data.frame(x = 1)
  expect_error(label_from_names(df, sep = ""), "sep")
  expect_error(label_from_names(df, sep = NA_character_), "sep")
  expect_error(label_from_names(df, sep = 1), "sep")
  expect_error(label_from_names(df, sep = c(".", ",")), "sep")
})

test_that("split that produces duplicate names errors with an actionable message", {
  df <- data.frame(a = 1, b = 2)
  names(df) <- c("x. Foo", "x. Bar")
  expect_error(
    label_from_names(df),
    "duplicate column names.*x"
  )
})

test_that("split that produces an empty new name errors with an actionable message", {
  df <- data.frame(a = 1:2)
  names(df) <- ". Label"
  expect_error(
    label_from_names(df),
    "empty column name"
  )
})

# ---- input class preservation ------------------------------------------

test_that("base data.frame stays a base data.frame", {
  df <- data.frame(a = 1:2, check.names = FALSE)
  names(df) <- "age. Age"
  out <- label_from_names(df)
  expect_s3_class(out, "data.frame", exact = TRUE)
})

test_that("tibble input stays a tibble", {
  skip_if_not_installed("tibble")
  tb <- tibble::tibble("age. Age" = 1:2, .name_repair = "minimal")
  out <- label_from_names(tb)
  expect_s3_class(out, "tbl_df")
})

# ---- shape edge cases --------------------------------------------------

test_that("0-column data.frame is returned as-is", {
  df <- data.frame()
  out <- label_from_names(df)
  expect_equal(ncol(out), 0L)
  expect_s3_class(out, "data.frame", exact = TRUE)
})

test_that("0-row data.frame still relabels", {
  df <- data.frame(a = integer(0))
  names(df) <- "age. Age"
  out <- label_from_names(df)
  expect_equal(names(out), "age")
  expect_equal(attr(out[["age"]], "label"), "Age")
  expect_equal(nrow(out), 0L)
})

# ---- sep behaviour -----------------------------------------------------

test_that("custom separator is used as a literal (no regex meaning)", {
  df <- data.frame(a = 1:2, b = 3:4)
  names(df) <- c("id|Identifier", "score|Total score")
  out <- label_from_names(df, sep = "|")
  expect_equal(names(out), c("id", "score"))
  expect_equal(attr(out[["id"]], "label"), "Identifier")
  expect_equal(attr(out[["score"]], "label"), "Total score")
})

test_that("a regex metacharacter as `sep` matches literally, not as a regex", {
  df <- data.frame(a = 1:2)
  # If `sep = "."` were interpreted as a regex, every position would
  # match and the function would split at position 1. With `fixed =
  # TRUE` (used internally), the literal dot is required.
  names(df) <- "abc.def.ghi"
  out <- label_from_names(df, sep = ".")
  expect_equal(names(out), "abc")
  expect_equal(attr(out[["abc"]], "label"), "def.ghi")
})
