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
