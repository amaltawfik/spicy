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
  expect_true(all(
    c("variable", "level", "group", "n", "pct", "p", "Cramer's V") %in%
      names(out)
  ))
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

test_that("table_apa keeps missing values as explicit levels when drop_na is FALSE", {
  df <- data.frame(
    grp = c("A", "A", "B", NA),
    v1 = c("Oui", NA, "Non", "Oui"),
    stringsAsFactors = FALSE
  )

  out_keep <- table_apa(
    data = df,
    row_vars = "v1",
    group_var = "grp",
    labels = "Var 1",
    drop_na = FALSE,
    simulate_p = FALSE,
    output = "long",
    style = "raw"
  )

  out_drop <- table_apa(
    data = df,
    row_vars = "v1",
    group_var = "grp",
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

test_that("table_apa returns gt object when requested", {
  skip_if_not_installed("gt")

  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )

  gt_tbl <- table_apa(
    data = df,
    row_vars = "v1",
    group_var = "grp",
    labels = "Var 1",
    simulate_p = FALSE,
    output = "gt"
  )

  expect_s3_class(gt_tbl, "gt_tbl")
})

# ── Dynamic association measure column ─────────────────────────────────────

test_that("table_apa default column is Cramer's V", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )
  out <- table_apa(
    df,
    "v1",
    "grp",
    labels = "Var 1",
    output = "long",
    style = "raw"
  )
  expect_true("Cramer's V" %in% names(out))
})

test_that("table_apa uses dynamic column name with assoc_measure = 'gamma'", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )
  out <- table_apa(
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

test_that("table_apa wide report has dynamic column name", {
  df <- data.frame(
    grp = factor(c("A", "A", "B", "B", "A", "B")),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non")
  )
  out <- table_apa(
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
  out <- table_apa(
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
  out <- table_apa(
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
  out <- table_apa(
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
  gt_out <- table_apa(
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
  out <- table_apa(
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

# ── Long report output ─────────────────────────────────────────────────────

test_that("table_apa long report returns formatted character columns", {
  out <- table_apa(
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

# ── levels_keep ────────────────────────────────────────────────────────────

test_that("table_apa levels_keep filters and reorders levels", {
  out <- table_apa(
    sochealth,
    "smoking",
    "education",
    levels_keep = c("Yes"),
    output = "wide",
    style = "raw"
  )
  expect_true(all(out$Level == "Yes", na.rm = TRUE))
})

test_that("table_apa levels_keep with (Missing)", {
  out <- table_apa(
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

# ── decimal_mark ───────────────────────────────────────────────────────────

test_that("table_apa decimal_mark = ',' uses comma separator", {
  out <- table_apa(
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

# ── blank_na_wide ──────────────────────────────────────────────────────────

test_that("table_apa blank_na_wide replaces NA with empty strings", {
  out <- table_apa(
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

# ── Validation errors ──────────────────────────────────────────────────────

test_that("table_apa validates data argument", {
  expect_error(table_apa("not_df", "v1", "grp"), "`data` must be a data.frame")
})

test_that("table_apa validates row_vars", {
  df <- data.frame(g = 1, v = 1)
  expect_error(table_apa(df, character(0), "g"), "`row_vars` must be non-empty")
  expect_error(table_apa(df, "missing", "g"), "missing in `data`")
})

test_that("table_apa validates group_var", {
  df <- data.frame(g = 1, v = 1)
  expect_error(table_apa(df, "v", "missing"), "`group_var` must be one valid")
})

test_that("table_apa validates labels length", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_apa(df, "v", "g", labels = c("a", "b")),
    "`labels` must have same length"
  )
})

test_that("table_apa validates boolean arguments", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_apa(df, "v", "g", include_total = NA),
    "`include_total` must be"
  )
  expect_error(table_apa(df, "v", "g", drop_na = "yes"), "`drop_na` must be")
  expect_error(table_apa(df, "v", "g", rescale = NA), "`rescale` must be")
  expect_error(table_apa(df, "v", "g", correct = NA), "`correct` must be")
  expect_error(table_apa(df, "v", "g", simulate_p = NA), "`simulate_p` must be")
  expect_error(
    table_apa(df, "v", "g", blank_na_wide = NA),
    "`blank_na_wide` must be"
  )
  expect_error(
    table_apa(df, "v", "g", add_multilevel_header = NA),
    "`add_multilevel_header` must be"
  )
})

test_that("table_apa validates decimal_mark", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_apa(df, "v", "g", decimal_mark = ";"),
    "`decimal_mark` must be"
  )
})

test_that("table_apa validates weights type", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_apa(df, "v", "g", weights = TRUE),
    "`weights` must be NULL"
  )
})

test_that("table_apa validates weights column name", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_error(
    table_apa(df, "v", "g", weights = "nonexistent"),
    "column name in `data`"
  )
})

test_that("table_apa warns when rescale = TRUE without weights", {
  df <- data.frame(g = c("A", "B"), v = c("x", "y"))
  expect_warning(
    table_apa(df, "v", "g", rescale = TRUE, output = "wide"),
    "rescale = TRUE.*no effect"
  )
})

# ── Multiple row_vars ──────────────────────────────────────────────────────

test_that("table_apa handles multiple row_vars in wide output", {
  out <- table_apa(
    sochealth,
    c("smoking", "physical_activity"),
    "education",
    output = "wide",
    style = "raw"
  )
  expect_true(all(c("smoking", "physical_activity") %in% out$Variable))
})

# ── include_total = FALSE ──────────────────────────────────────────────────

test_that("table_apa include_total = FALSE omits Total column", {
  out <- table_apa(
    sochealth,
    "smoking",
    "education",
    include_total = FALSE,
    output = "wide",
    style = "raw"
  )
  expect_false(any(grepl("^Total", names(out))))
})

# ── Flextable output ──────────────────────────────────────────────────────

test_that("table_apa returns flextable object when requested", {
  skip_if_not_installed("flextable")
  ft <- table_apa(
    sochealth,
    "smoking",
    "education",
    output = "flextable"
  )
  expect_s3_class(ft, "flextable")
})

# ── Excel output ──────────────────────────────────────────────────────────

test_that("table_apa writes excel file", {
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  table_apa(
    sochealth,
    "smoking",
    "education",
    output = "excel",
    excel_path = tmp
  )
  expect_true(file.exists(tmp))
})

# ── assoc_measure = "none" ────────────────────────────────────────────────

test_that("table_apa assoc_measure = 'none' returns NA for association", {
  out <- table_apa(
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
