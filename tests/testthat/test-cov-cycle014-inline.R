# inline() on a table whose groups of `by` are rows (table_continuous()):
# `model` by label, by position and by role, and the refusals that name
# the call to use.

test_that("inline() refuses a `model` that is not a single value", {
  cn <- table_continuous(sochealth, select = bmi, by = sex)
  err <- expect_error(
    inline(cn, bmi, model = c("Female", "Male")),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "`model` must be a single value, not length 2.",
    fixed = TRUE
  )
})

test_that("inline() selects a group row by its position", {
  cn <- table_continuous(sochealth, select = bmi, by = sex)
  expect_identical(inline(cn, bmi, model = 2), "26.20")
  expect_identical(inline(cn, bmi, model = 2), inline(cn, bmi, model = "Male"))
  expect_identical(
    inline(cn, bmi, model = 1),
    inline(cn, bmi, model = "Female")
  )
})

test_that("inline() lists the groups when `model` names none of them", {
  cn <- table_continuous(sochealth, select = bmi, by = sex)
  err <- expect_error(
    inline(cn, bmi, model = "Other"),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "No group \"Other\" in this table.",
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err),
    "Available: \"Female\", \"Male\".",
    fixed = TRUE
  )
  # A position past the last group is refused the same way.
  err <- expect_error(inline(cn, bmi, model = 3), class = "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "No group \"3\" in this table.",
    fixed = TRUE
  )
})

test_that("`model = \"(Missing)\"` finds the translated missing group by its role", {
  d <- data.frame(
    g = factor(c("a", "a", "a", "b", "b", NA, NA)),
    y = c(1, 2, 3, 10, 20, 30, 40)
  )
  withr::local_options(spicy.language = "fr")
  cn <- table_continuous(
    d,
    select = y,
    by = g,
    p_value = FALSE,
    drop_na = FALSE
  )
  body <- as_structured(cn)$body
  shown <- body$.level[body$.row_role %in% "missing"]
  # The row carries the French label, so the documented address matches
  # no label and is resolved through the role.
  expect_length(shown, 1L)
  expect_false(identical(shown, "(Missing)"))
  expect_identical(inline(cn, y, column = "n", model = "(Missing)"), "2")
  expect_identical(
    inline(cn, y, column = "n", model = "(Missing)"),
    inline(cn, y, column = "n", model = shown)
  )
})

test_that("a statistic the table leaves blank on every group row is an empty cell", {
  d <- data.frame(
    y1 = c(1, 2, 3, 4, 5, 6, 7, 8),
    y2 = rep(5, 8),
    g = rep(c("a", "b"), each = 4)
  )
  expect_warning(
    cn <- table_continuous(d, select = c(y1, y2), by = g),
    class = "spicy_undefined_stat"
  )
  # y2 has no test: its p is blank on both of its group rows, so `model`
  # meets the refusal of an empty cell ...
  err <- expect_error(
    inline(cn, y2, column = "p", model = "a"),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "The \"p\" cell of this row is empty in the table.",
    fixed = TRUE
  )
  # ... while the p of y1, printed once, is a statistic of the variable.
  err <- expect_error(
    inline(cn, y1, column = "p", model = "a"),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "is a statistic of the variable \"y1\", not of a group of `by`",
    fixed = TRUE
  )
})

test_that("the refusals quote a variable whose name is not syntactic", {
  d <- data.frame(
    `my var` = c(1, 2, 3, 4, 5, 6, 7, 8),
    g = rep(c("a", "b"), each = 4),
    check.names = FALSE
  )
  cn <- table_continuous(d, select = `my var`, by = g)
  err <- expect_error(
    inline(cn, "my var", column = "p", model = "a"),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "Cite it without `model`: `inline(x, \"my var\", column = \"p\")`.",
    fixed = TRUE
  )
  err <- expect_error(
    inline(cn, "my var", level = "a"),
    class = "spicy_defunct"
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "Select it with `model`: `inline(x, \"my var\", model = \"a\")`.",
    fixed = TRUE
  )
})
