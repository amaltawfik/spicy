test_that("table_categorical returns expected long raw structure", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non"),
    v2 = c("Oui", "Oui", "Non", "Non", "Oui", "Non")
  )

  out <- table_categorical(
    data = df,
    select = c(v1, v2),
    by = grp,
    labels = c("Var 1", "Var 2"),
    include_total = TRUE,
    simulate_p = FALSE,
    output = "long",
    style = "raw"
  )

  expect_s3_class(out, "data.frame")
  expect_true(all(
    c("variable", "level", "group", "n", "pct", "p", "Cramer's V") %in%
      names(out)
  ))
  expect_true(nrow(out) > 0)
})

test_that("table_categorical accepts weights as column name or numeric vector", {
  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non"),
    w = c(1, 2, 1, 3, 2, 1)
  )

  out_col <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = "Var 1",
    weights = "w",
    simulate_p = FALSE,
    output = "long",
    style = "raw"
  )

  out_vec <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = "Var 1",
    weights = df$w,
    simulate_p = FALSE,
    output = "long",
    style = "raw"
  )

  expect_equal(out_col$n, out_vec$n)
  expect_equal(out_col$pct, out_vec$pct)
})

test_that("table_categorical accepts tidyselect-style select and unquoted by", {
  df <- data.frame(
    grp = c("A", "A", "B", "B"),
    v1 = c("Oui", "Non", "Oui", "Non"),
    v2 = c("Oui", "Oui", "Non", "Non")
  )

  out <- table_categorical(
    data = df,
    select = tidyselect::starts_with("v"),
    by = grp,
    output = "wide",
    style = "raw"
  )

  expect_true(all(c("v1", "v2") %in% out$Variable))
})

test_that("table_categorical works without by in long raw output", {
  df <- data.frame(
    v1 = c("Oui", "Non", "Oui", NA),
    v2 = c("A", "A", "B", "B")
  )

  out <- table_categorical(
    data = df,
    select = c(v1, v2),
    drop_na = FALSE,
    output = "long",
    style = "raw"
  )

  expect_true(all(c("variable", "level", "n", "pct") %in% names(out)))
  expect_false("group" %in% names(out))
  expect_true(any(grepl("Missing", out$level)))
})

test_that("table_categorical default output prints ASCII and returns styled object", {
  printed <- capture.output(
    out <- table_categorical(
      sochealth,
      select = smoking,
      output = "default"
    )
  )

  expect_true(length(printed) > 0)
  expect_s3_class(out, "spicy_categorical_table")
})

test_that("table_categorical default output with styled = FALSE returns wide raw data", {
  out <- table_categorical(
    sochealth,
    select = smoking,
    output = "default",
    styled = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_true(all(c("Variable", "Level", "n", "%") %in% names(out)))
})

test_that("table_categorical validates weights and simulate_B", {
  df <- data.frame(
    grp = c("A", "A", "B", "B"),
    v1 = c("Oui", "Non", "Oui", "Non")
  )

  expect_error(
    table_categorical(
      data = df,
      select = "v1",
      by = "grp",
      labels = "Var 1",
      weights = c(1, 2),
      output = "long",
      style = "raw"
    ),
    "Numeric `weights` must have length `nrow(data)`.",
    fixed = TRUE
  )

  expect_error(
    table_categorical(
      data = df,
      select = "v1",
      by = "grp",
      labels = "Var 1",
      simulate_B = 0,
      output = "long",
      style = "raw"
    ),
    "`simulate_B` must be a positive integer.",
    fixed = TRUE
  )
})

test_that("table_categorical keeps missing values as explicit levels when drop_na is FALSE", {
  df <- data.frame(
    grp = c("A", "A", "B", NA),
    v1 = c("Oui", NA, "Non", "Oui"),
    stringsAsFactors = FALSE
  )

  out_keep <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = "Var 1",
    drop_na = FALSE,
    simulate_p = FALSE,
    output = "long",
    style = "raw"
  )

  out_drop <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = "Var 1",
    drop_na = TRUE,
    simulate_p = FALSE,
    output = "long",
    style = "raw"
  )

  expect_true(any(grepl("^\\(Missing", out_keep$level)))
  expect_true(any(grepl("^\\(Missing", out_keep$group)))
  expect_false(any(grepl("^\\(Missing", out_drop$level)))
  expect_false(any(grepl("^\\(Missing", out_drop$group)))
})

test_that("table_categorical returns tinytable object when requested", {
  skip_if_not_installed("tinytable")

  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )

  tt <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = "Var 1",
    simulate_p = FALSE,
    output = "tinytable"
  )

  expect_true(methods::is(tt, "tinytable"))
})

test_that("table_categorical returns gt object when requested", {
  skip_if_not_installed("gt")

  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )

  gt_tbl <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = "Var 1",
    simulate_p = FALSE,
    output = "gt"
  )

  expect_s3_class(gt_tbl, "gt_tbl")
})

# Ã¢â€â‚¬Ã¢â€â‚¬ Dynamic association measure column Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical default column is Cramer's V", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )
  out <- table_categorical(
    df,
    "v1",
    "grp",
    labels = "Var 1",
    output = "long",
    style = "raw"
  )
  expect_true("Cramer's V" %in% names(out))
})

test_that("table_categorical uses dynamic column name with assoc_measure = 'gamma'", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )
  out <- table_categorical(
    df,
    "v1",
    "grp",
    labels = "Var 1",
    assoc_measure = "gamma",
    output = "long",
    style = "raw"
  )
  expect_true("Goodman-Kruskal Gamma" %in% names(out))
  expect_false("Cramer's V" %in% names(out))
})

test_that("table_categorical wide report has dynamic column name", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )
  out <- table_categorical(
    df,
    "v1",
    "grp",
    labels = "Var 1",
    assoc_measure = "tau_b",
    output = "wide",
    style = "report"
  )
  expect_true("Kendall's Tau-b" %in% names(out))
})

test_that("assoc_ci adds CI columns in wide raw output", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "wide",
    style = "raw",
    assoc_ci = TRUE
  )
  expect_true("CI lower" %in% names(out))
  expect_true("CI upper" %in% names(out))
  expect_true(is.numeric(out[["CI lower"]]))
  expect_true(all(!is.na(out[["CI lower"]])))
})

test_that("assoc_ci = FALSE omits CI columns in wide raw output", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "wide",
    style = "raw",
    assoc_ci = FALSE
  )
  expect_false("CI lower" %in% names(out))
  expect_false("CI upper" %in% names(out))
})

test_that("assoc_ci adds CI columns in long raw output", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "long",
    style = "raw",
    assoc_ci = TRUE
  )
  expect_true("ci_lower" %in% names(out))
  expect_true("ci_upper" %in% names(out))
  expect_true(is.numeric(out$ci_lower))
})

test_that("assoc_ci shows inline CI in rendered formats", {
  skip_if_not_installed("gt")
  gt_out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "gt",
    assoc_ci = TRUE
  )
  dat <- gt_out[["_data"]]
  expect_match(dat$assoc_col[1], "\\[")
  expect_false("CI lower" %in% names(dat))
})

test_that("assoc_ci adds formatted CI columns in wide report", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "wide",
    style = "report",
    assoc_ci = TRUE
  )
  expect_true("CI lower" %in% names(out))
  expect_true("CI upper" %in% names(out))
  expect_match(out[["CI lower"]][1], "^\\.")
})

# Ã¢â€â‚¬Ã¢â€â‚¬ Long report output Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical long report returns formatted character columns", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "long",
    style = "report"
  )
  expect_s3_class(out, "data.frame")
  expect_type(out$n, "character")
  expect_type(out$pct, "character")
})

# Ã¢â€â‚¬Ã¢â€â‚¬ levels_keep Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical levels_keep filters and reorders levels", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    levels_keep = c("Yes"),
    output = "wide",
    style = "raw"
  )
  expect_true(all(out$Level == "Yes", na.rm = TRUE))
})

test_that("table_categorical levels_keep with (Missing)", {
  out <- table_categorical(
    sochealth,
    "income_group",
    "education",
    drop_na = FALSE,
    levels_keep = c("Low", "High", "(Missing)"),
    output = "wide",
    style = "raw"
  )
  lvls <- out$Level[!is.na(out$Level) & out$Level != ""]
  expect_equal(lvls, c("Low", "High", "(Missing)"))
})

# Ã¢â€â‚¬Ã¢â€â‚¬ decimal_mark Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical decimal_mark = ',' uses comma separator", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    decimal_mark = ",",
    output = "wide",
    style = "report"
  )
  pct_col <- out[[grep("%$", names(out))[1]]]
  pct_vals <- pct_col[!is.na(pct_col) & pct_col != ""]
  expect_true(any(grepl(",", pct_vals)))
})

# Ã¢â€â‚¬Ã¢â€â‚¬ blank_na_wide Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical blank_na_wide replaces NA with empty strings", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "wide",
    style = "raw",
    blank_na_wide = TRUE
  )
  chr_cols <- vapply(out, is.character, logical(1))
  if (any(chr_cols)) {
    expect_false(any(is.na(out[chr_cols])))
  }
})

# Ã¢â€â‚¬Ã¢â€â‚¬ Validation errors Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical validates data argument", {
  expect_error(
    table_categorical("not_df", "v1", "grp"),
    "`data` must be a data.frame"
  )
})

test_that("table_categorical validates select", {
  df <- data.frame(g = 1, v = 1)
  expect_error(
    table_categorical(df, character(0), "g"),
    "`select` must select at least one column"
  )
  expect_error(
    table_categorical(df, "missing", "g"),
    "Some `select` columns are missing"
  )
})

test_that("table_categorical validates by", {
  df <- data.frame(g = 1, v = 1)
  expect_error(
    table_categorical(df, "v", "missing"),
    "`by` must select exactly one column"
  )
})

test_that("table_categorical validates labels length", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_categorical(df, "v", "g", labels = c("a", "b")),
    "`labels` must have same length"
  )
})

test_that("table_categorical validates boolean arguments", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_categorical(df, "v", "g", include_total = NA),
    "`include_total` must be"
  )
  expect_error(
    table_categorical(df, "v", "g", drop_na = "yes"),
    "`drop_na` must be"
  )
  expect_error(
    table_categorical(df, "v", "g", rescale = NA),
    "`rescale` must be"
  )
  expect_error(
    table_categorical(df, "v", "g", correct = NA),
    "`correct` must be"
  )
  expect_error(
    table_categorical(df, "v", "g", simulate_p = NA),
    "`simulate_p` must be"
  )
  expect_error(
    table_categorical(df, "v", "g", blank_na_wide = NA),
    "`blank_na_wide` must be"
  )
  expect_error(
    table_categorical(df, "v", "g", add_multilevel_header = NA),
    "`add_multilevel_header` must be"
  )
})

test_that("table_categorical validates decimal_mark", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_categorical(df, "v", "g", decimal_mark = ";"),
    "`decimal_mark` must be"
  )
})

test_that("table_categorical validates weights type", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_categorical(df, "v", "g", weights = TRUE),
    "`weights` must be NULL"
  )
})

test_that("table_categorical validates weights column name", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_categorical(df, "v", "g", weights = "nonexistent"),
    "column name in `data`"
  )
})

test_that("table_categorical warns when rescale = TRUE without weights", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_warning(
    table_categorical(df, "v", "g", rescale = TRUE, output = "wide"),
    "rescale = TRUE.*no effect"
  )
})

# Ã¢â€â‚¬Ã¢â€â‚¬ Multiple select Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical handles multiple select in wide output", {
  out <- table_categorical(
    sochealth,
    c(smoking, physical_activity),
    education,
    output = "wide",
    style = "raw"
  )
  expect_true(all(c("smoking", "physical_activity") %in% out$Variable))
})

# Ã¢â€â‚¬Ã¢â€â‚¬ include_total = FALSE Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical include_total = FALSE omits Total column", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    include_total = FALSE,
    output = "wide",
    style = "raw"
  )
  expect_false(any(grepl("^Total", names(out))))
})

# Ã¢â€â‚¬Ã¢â€â‚¬ Flextable output Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical returns flextable object when requested", {
  skip_if_not_installed("flextable")
  ft <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "flextable"
  )
  expect_s3_class(ft, "flextable")
})

# Ã¢â€â‚¬Ã¢â€â‚¬ Excel output Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical writes excel file", {
  skip_if_not_installed("openxlsx")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "excel",
    excel_path = tmp
  )
  expect_true(file.exists(tmp))
})

# Ã¢â€â‚¬Ã¢â€â‚¬ assoc_measure = "none" Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical assoc_measure = 'none' returns NA for association", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    assoc_measure = "none",
    output = "long",
    style = "raw"
  )
  assoc_col <- out[["Cramer's V"]]
  expect_true(all(is.na(assoc_col)))
})

# Ã¢â€â‚¬Ã¢â€â‚¬ Report mode (no rendering dependency) Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical long report has formatted values", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "long",
    style = "report"
  )
  expect_s3_class(out, "data.frame")
  expect_true("variable" %in% names(out))
  expect_true(is.character(out$pct))
})

test_that("table_categorical with assoc_ci includes CI columns in raw long", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    assoc_ci = TRUE,
    output = "long",
    style = "raw"
  )
  expect_true("CI lower" %in% names(out) || "ci_lower" %in% names(out))
})

test_that("table_categorical fmt_p formats small p-values", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "long",
    style = "report"
  )
  p_col <- out$p
  p_vals <- p_col[p_col != "" & !is.na(p_col)]
  expect_true(length(p_vals) > 0)
})

test_that("table_categorical decimal_mark comma in long report output", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    decimal_mark = ",",
    output = "long",
    style = "report"
  )
  pct_vals <- out$pct[out$pct != "" & !is.na(out$pct)]
  expect_true(any(grepl(",", pct_vals)))
})

test_that("table_categorical simulate_p works in long output", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    simulate_p = TRUE,
    output = "long",
    style = "raw"
  )
  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) > 0)
})

test_that("table_categorical with drop_na = FALSE includes Missing level", {
  df <- sochealth
  df$smoking[1:5] <- NA
  out <- table_categorical(
    df,
    "smoking",
    "education",
    drop_na = FALSE,
    output = "long",
    style = "raw"
  )
  expect_true(any(grepl("Missing", out$level)))
})

# Ã¢â€â‚¬Ã¢â€â‚¬ Digit validation Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical rejects invalid digit arguments", {
  df <- data.frame(
    grp = c("A", "B", "A", "B"),
    v1 = c("x", "y", "x", "y")
  )

  expect_error(
    table_categorical(df, "v1", "grp", percent_digits = -1, output = "long"),
    "percent_digits"
  )
  expect_error(
    table_categorical(df, "v1", "grp", p_digits = "a", output = "long"),
    "p_digits"
  )
  expect_error(
    table_categorical(df, "v1", "grp", v_digits = NA, output = "long"),
    "v_digits"
  )
})

# Ã¢â€â‚¬Ã¢â€â‚¬ Level ordering Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical preserves factor level order in row variables", {
  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = factor(
      c("Low", "High", "Medium", "Low", "High", "Medium"),
      levels = c("Low", "Medium", "High")
    )
  )
  out <- table_categorical(
    df,
    "v1",
    "grp",
    include_total = FALSE,
    output = "long",
    style = "raw"
  )
  lvs <- unique(out$level)
  expect_equal(lvs, c("Low", "Medium", "High"))
})

test_that("table_categorical places (Missing) at end when drop_na = FALSE", {
  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = factor(
      c("Yes", NA, "No", "Yes", NA, "No"),
      levels = c("Yes", "No")
    )
  )
  out <- table_categorical(
    df,
    "v1",
    "grp",
    drop_na = FALSE,
    include_total = FALSE,
    output = "long",
    style = "raw"
  )
  lvs <- unique(out$level)
  expect_equal(lvs, c("Yes", "No", "(Missing)"))
})

test_that("table_categorical rescale warning includes call. = FALSE", {
  df <- data.frame(
    grp = c("A", "B", "A", "B"),
    v1 = c("x", "y", "x", "y")
  )
  w <- tryCatch(
    table_categorical(
      df,
      "v1",
      "grp",
      rescale = TRUE,
      output = "long",
      style = "raw"
    ),
    warning = function(w) w
  )
  expect_s3_class(w, "simpleWarning")
  expect_null(w$call)
})
