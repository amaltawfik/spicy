df <- data.frame(x = 1:3, y = 4:6, w = c(0.5, 1, 1.5))


test_that("resolve_multi_column_selection returns character() when quo evaluates to NULL", {
  null_obj <- NULL
  q <- rlang::quo(null_obj)
  expect_identical(
    spicy:::resolve_multi_column_selection(q, df, "exclude"),
    character()
  )
})


test_that("resolve_multi_column_selection names an unknown column", {
  q <- rlang::quo(c(x, x_does_not_exist))
  err <- expect_error(
    spicy:::resolve_multi_column_selection(q, df, "exclude"),
    class = "spicy_missing_column"
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "x_does_not_exist", fixed = TRUE)
  # A character vector is checked like any other selection.
  err <- expect_error(
    spicy:::resolve_multi_column_selection(
      rlang::quo(c("x", "nope", "zip")),
      df,
      "select"
    ),
    class = "spicy_missing_column"
  )
  expect_match(conditionMessage(err), "\"nope\", \"zip\"", fixed = TRUE)
  # A position past the end is an unknown column too.
  expect_error(
    spicy:::resolve_multi_column_selection(rlang::quo(c(1, 9)), df, "select"),
    class = "spicy_missing_column"
  )
  # A selection that fails for another reason is an invalid input.
  err <- expect_error(
    spicy:::resolve_multi_column_selection(
      rlang::quo(tidyselect::starts_with(1)),
      df,
      "select"
    ),
    class = "spicy_invalid_input"
  )
  expect_false(inherits(err, "spicy_missing_column"))
})

test_that("resolve_single_column_selection refuses two columns and names an unknown one", {
  err <- expect_error(
    spicy:::resolve_single_column_selection(rlang::quo(c(x, y)), df, "by"),
    class = "spicy_invalid_input"
  )
  expect_false(inherits(err, "spicy_missing_column"))
  expect_error(
    spicy:::resolve_single_column_selection(rlang::quo(c("x", "y")), df, "by"),
    class = "spicy_invalid_input"
  )
  err <- expect_error(
    spicy:::resolve_single_column_selection(rlang::quo(nope), df, "by"),
    class = "spicy_missing_column"
  )
  expect_match(conditionMessage(err), "nope", fixed = TRUE)
  err <- expect_error(
    spicy:::resolve_single_column_selection(
      rlang::quo("nope"),
      df,
      "by",
      where = "`design`"
    ),
    class = "spicy_missing_column"
  )
  expect_match(conditionMessage(err), "`design`", fixed = TRUE)
})

test_that(".resolve_selection_without skips a role column a helper reaches and refuses a named one", {
  roles <- c(by = "y")
  expect_identical(
    spicy:::.resolve_selection_without(
      rlang::quo(tidyselect::everything()),
      df,
      roles
    ),
    c("x", "w")
  )
  expect_identical(
    spicy:::.resolve_selection_without(rlang::quo(-w), df, roles),
    "x"
  )
  expect_identical(
    spicy:::.resolve_selection_without(rlang::quo(c(x, w)), df, roles),
    c("x", "w")
  )
  for (q in list(rlang::quo(c(x, y)), rlang::quo(c("x", "y")), rlang::quo(y))) {
    err <- expect_error(
      spicy:::.resolve_selection_without(q, df, roles),
      class = "spicy_invalid_input"
    )
    expect_false(inherits(err, "spicy_missing_column"))
  }
  # An unknown column is named first, whatever the roles.
  expect_error(
    spicy:::.resolve_selection_without(rlang::quo(c(y, nope)), df, roles),
    class = "spicy_missing_column"
  )
  # No role: the plain selection.
  expect_identical(
    spicy:::.resolve_selection_without(rlang::quo(c(x, y)), df, NULL),
    c("x", "y")
  )
})

test_that(".resolve_selection_without refuses a position that points at a role column", {
  d <- data.frame(x1 = 1:3, g = c("a", "b", "a"), x2 = 4:6, x3 = c(0.5, 1, 2))
  roles <- c(by = "g")
  sel <- function(q) spicy:::.resolve_selection_without(q, d, roles)
  # A position is an explicit choice, like a name.
  for (q in list(
    rlang::quo(1:2),
    rlang::quo(2),
    rlang::quo(c(x1, 2)),
    rlang::quo(1:4)
  )) {
    err <- expect_error(sel(q), class = "spicy_invalid_input")
    expect_match(conditionMessage(err), "`by` column (\"g\")", fixed = TRUE)
  }
  pos <- 2L
  expect_error(
    sel(rlang::quo(tidyselect::all_of(pos))),
    class = "spicy_invalid_input"
  )
  # Positions that leave it out select as in dplyr.
  expect_identical(sel(rlang::quo(c(1, 3))), c("x1", "x2"))
  # A negative position, a range of names around it, a helper: the role
  # column is reached, not pointed at, and is skipped.
  expect_identical(sel(rlang::quo(-1)), c("x2", "x3"))
  expect_identical(sel(rlang::quo(-3)), c("x1", "x3"))
  expect_identical(sel(rlang::quo(x1:x3)), c("x1", "x2", "x3"))
  expect_identical(sel(rlang::quo(tidyselect::last_col())), "x3")
  expect_identical(
    sel(rlang::quo(tidyselect::where(is.numeric))),
    c("x1", "x2", "x3")
  )
  # A range that starts at it names it.
  expect_error(sel(rlang::quo(g:x3)), class = "spicy_invalid_input")
})

test_that("an unknown column is named with the one it is likely a typo of", {
  d <- data.frame(sex = 1, age = 2, income = 3)
  msg <- function(q, arg = "select") {
    conditionMessage(tryCatch(
      spicy:::resolve_multi_column_selection(q, d, arg),
      error = identity
    ))
  }
  expect_match(msg(rlang::quo(sexx)), "Did you mean `sex`?", fixed = TRUE)
  expect_match(msg(rlang::quo("Age")), "Did you mean `age`?", fixed = TRUE)
  expect_match(msg(rlang::quo(incme)), "Did you mean `income`?", fixed = TRUE)
  err <- tryCatch(
    spicy:::resolve_single_column_selection(rlang::quo(sexx), d, "by"),
    error = identity
  )
  expect_s3_class(err, "spicy_missing_column")
  expect_match(conditionMessage(err), "Did you mean `sex`?", fixed = TRUE)
  # Conservative: no suggestion for a far name, a short one that differs
  # by more than its case, a tie, or several unknown columns.
  for (q in list(
    rlang::quo(nope),
    rlang::quo(ag),
    rlang::quo(c("sexx", "agee"))
  )) {
    expect_false(grepl("Did you mean", msg(q), fixed = TRUE))
  }
  d2 <- data.frame(abcd = 1, abce = 2)
  expect_false(grepl(
    "Did you mean",
    conditionMessage(tryCatch(
      spicy:::resolve_multi_column_selection(rlang::quo(abcf), d2, "select"),
      error = identity
    )),
    fixed = TRUE
  ))
})


test_that("resolve_weights_argument errors when the argument is an undefined symbol", {
  q <- rlang::quo(undefined_weights_var)
  expect_error(
    spicy:::resolve_weights_argument(q, df, "weights"),
    "NULL, numeric vector, or a single column name"
  )
})


test_that("resolve_weights_argument returns NULL when expression evaluates to NULL", {
  null_obj <- NULL
  q <- rlang::quo(null_obj)
  expect_null(spicy:::resolve_weights_argument(q, df, "weights"))
})


test_that("a column wins over a same-named environment variable (data-first)", {
  # Audit phase 2, finding 15: the environment value used to win, so
  # `by = g` silently grouped by ANOTHER column named in the trap
  # variable. The tidyselect / dplyr precedence is data-first.
  local({
    x <- "y" # trap: same name as a column, holds another column's name
    q <- rlang::quo(x)
    expect_identical(
      spicy:::resolve_single_column_selection(q, df, "by"),
      "x"
    )
    expect_identical(
      spicy:::resolve_multi_column_selection(q, df, "exclude"),
      "x"
    )
  })
  # A symbol that is NOT a column still resolves through the
  # environment (the `by = by_col` idiom).
  local({
    by_col <- "y"
    q <- rlang::quo(by_col)
    expect_identical(
      spicy:::resolve_single_column_selection(q, df, "by"),
      "y"
    )
  })
})


test_that("data-first precedence holds end-to-end in the three table functions", {
  # Audit phase 2, finding 15 (transversal check): table_continuous,
  # table_categorical, and table_continuous_lm all resolve `by`
  # through the shared helper; an environment variable named like the
  # `by` column must never redirect the grouping.
  d <- data.frame(
    y = c(1, 2, 3, 10, 11, 12),
    g = factor(c("a", "a", "a", "b", "b", "b")),
    sex = factor(c("m", "f", "m", "f", "m", "f"))
  )
  local({
    g <- "sex" # trap
    tc <- table_continuous(d, select = y, by = g, output = "data.frame")
    expect_identical(unique(tc$group), c("a", "b"))
    tcat <- table_categorical(d, select = sex, by = g, output = "long")
    expect_identical(setdiff(unique(tcat$group), "Total"), c("a", "b"))
    tclm <- table_continuous_lm(d, select = y, by = g, output = "long")
    expect_identical(tclm$level, c("a", "b"))
    expect_equal(tclm$emmean, c(2, 11), tolerance = 1e-12)
  })
})
