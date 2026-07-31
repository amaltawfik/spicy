df <- data.frame(x = 1:3, y = 4:6, w = c(0.5, 1, 1.5))


test_that("resolve_multi_column_selection returns character() when quo evaluates to NULL", {
  null_obj <- NULL
  q <- rlang::quo(null_obj)
  expect_identical(
    spicy:::resolve_multi_column_selection(q, df, "exclude"),
    character()
  )
})


test_that("resolve_multi_column_selection errors actionably on invalid tidyselect", {
  q <- rlang::quo(c(x, x_does_not_exist))
  expect_error(
    spicy:::resolve_multi_column_selection(q, df, "exclude"),
    "must select columns in `data`"
  )
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
