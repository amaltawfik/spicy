test_that("table_apa returns expected long raw structure", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non"),
    v2 = c("Oui", "Oui", "Non", "Non", "Oui", "Non")
  )

  out <- table_apa(
    data = df,
    row_vars = c("v1", "v2"),
    group_var = "grp",
    labels = c("Var 1", "Var 2"),
    include_total = TRUE,
    simulate_p = FALSE,
    output = "long",
    style = "raw"
  )

  expect_s3_class(out, "data.frame")
  expect_true(all(c("variable", "level", "group", "n", "pct", "p", "Cramer's V") %in% names(out)))
  expect_true(nrow(out) > 0)
})

test_that("table_apa accepts weights as column name or numeric vector", {
  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non"),
    w = c(1, 2, 1, 3, 2, 1)
  )

  out_col <- table_apa(
    data = df,
    row_vars = "v1",
    group_var = "grp",
    labels = "Var 1",
    weights = "w",
    simulate_p = FALSE,
    output = "long",
    style = "raw"
  )

  out_vec <- table_apa(
    data = df,
    row_vars = "v1",
    group_var = "grp",
    labels = "Var 1",
    weights = df$w,
    simulate_p = FALSE,
    output = "long",
    style = "raw"
  )

  expect_equal(out_col$n, out_vec$n)
  expect_equal(out_col$pct, out_vec$pct)
})

test_that("table_apa validates weights and simulate_B", {
  df <- data.frame(
    grp = c("A", "A", "B", "B"),
    v1 = c("Oui", "Non", "Oui", "Non")
  )

  expect_error(
    table_apa(
      data = df,
      row_vars = "v1",
      group_var = "grp",
      labels = "Var 1",
      weights = c(1, 2),
      output = "long",
      style = "raw"
    ),
    "Numeric `weights` must have length `nrow(data)`.",
    fixed = TRUE
  )

  expect_error(
    table_apa(
      data = df,
      row_vars = "v1",
      group_var = "grp",
      labels = "Var 1",
      simulate_B = 0,
      output = "long",
      style = "raw"
    ),
    "`simulate_B` must be a positive integer.",
    fixed = TRUE
  )
})

test_that("table_apa returns tinytable object when requested", {
  skip_if_not_installed("tinytable")

  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )

  tt <- table_apa(
    data = df,
    row_vars = "v1",
    group_var = "grp",
    labels = "Var 1",
    simulate_p = FALSE,
    output = "tinytable"
  )

  expect_true(methods::is(tt, "tinytable"))
})
