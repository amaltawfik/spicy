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

test_that("a comparison that could not run is refused as undefined", {
  d <- data.frame(
    y1 = c(1, 2, 3, 4, 5, 6, 7, 8),
    y2 = c(1, 2, 3, 4, 5, NA, NA, NA),
    k = rep(5, 8),
    g = rep(c("a", "b"), each = 4)
  )
  expect_warning(
    cn <- table_continuous(d, select = c(y1, y2, k), by = g),
    class = "spicy_undefined_stat"
  )
  # y2 has one observation in b, too few for a test, and k has no
  # variance: the p of each is printed once, on the row of the first
  # group, as undefined, and refused as such ...
  for (v in c("y2", "k")) {
    err <- expect_error(
      inline(cn, !!v, column = "p"),
      class = "spicy_invalid_input"
    )
    expect_match(
      conditionMessage(err),
      "The \"p\" cell of this row is undefined in the table (not estimable).",
      fixed = TRUE
    )
  }
  # ... and asked of a group, it is a statistic of the variable, like the
  # p of y1, whose test ran.
  for (v in c("y1", "y2")) {
    err <- expect_error(
      inline(cn, !!v, column = "p", model = "a"),
      class = "spicy_invalid_input"
    )
    expect_match(
      conditionMessage(err),
      sprintf("is a statistic of the variable \"%s\", not of a group", v),
      fixed = TRUE
    )
  }
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
