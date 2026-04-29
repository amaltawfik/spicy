collect_warnings <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

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
    output = "long"
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
    output = "long"
  )

  out_vec <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = "Var 1",
    weights = df$w,
    simulate_p = FALSE,
    output = "long"
  )

  expect_equal(out_col$n, out_vec$n)
  expect_equal(out_col$pct, out_vec$pct)
})

test_that("table_categorical accepts weights as an unquoted column name", {
  df <- data.frame(
    grp = c("A", "A", "B", "B", "A", "B"),
    v1 = c("Oui", "Non", "Oui", "Non", "Oui", "Non"),
    w = c(1, 2, 1, 3, 2, 1)
  )

  expect_no_warning(
    out_bare <- table_categorical(
      data = df,
      select = "v1",
      by = "grp",
      labels = "Var 1",
      weights = w,
      simulate_p = FALSE,
      output = "long"
    )
  )

  out_char <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = "Var 1",
    weights = "w",
    simulate_p = FALSE,
    output = "long"
  )

  expect_equal(out_bare$n, out_char$n)
  expect_equal(out_bare$pct, out_char$pct)
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
    output = "data.frame"
  )

  expect_true(all(c("v1", "v2") %in% out$Variable))
})

test_that("table_categorical accepts by as a character object", {
  df <- data.frame(
    grp = c("A", "A", "B", "B"),
    v1 = c("Oui", "Non", "Oui", "Non")
  )

  by_col <- "grp"
  expect_no_warning(
    out <- table_categorical(
      data = df,
      select = "v1",
      by = by_col,
      output = "data.frame"
    )
  )

  expect_true("A n" %in% names(out))
  expect_true("B %" %in% names(out))
})

test_that("table_categorical validates by and select branches", {
  df <- data.frame(
    grp = c("A", "A", "B", "B"),
    v1 = c("Oui", "Non", "Oui", "Non"),
    v2 = c("x", "y", "x", "y")
  )

  expect_error(
    table_categorical(df, select = "v1", by = c(grp, v2)),
    "by"
  )

  expect_error(
    table_categorical(df, select = tidyselect::starts_with("zzz"), by = grp),
    "select"
  )
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
    output = "long"
  )

  expect_true(all(c("variable", "level", "n", "pct") %in% names(out)))
  expect_false("group" %in% names(out))
  expect_true(any(grepl("Missing", out$level)))
})

test_that("table_categorical renames generated missing labels when needed", {
  df <- data.frame(v1 = c("(Missing)", NA, "Yes"))

  out <- table_categorical(
    data = df,
    select = v1,
    drop_na = FALSE,
    output = "long"
  )

  expect_true("(Missing)" %in% out$level)
  expect_true("(Missing_1)" %in% out$level)
})

test_that("table_categorical handles one-way empty results after dropping missing", {
  df <- data.frame(v1 = c(NA, NA))

  out_long <- table_categorical(
    data = df,
    select = v1,
    drop_na = TRUE,
    output = "long"
  )
  out_wide <- table_categorical(
    data = df,
    select = v1,
    drop_na = TRUE,
    output = "data.frame"
  )

  expect_equal(nrow(out_long), 0L)
  expect_equal(nrow(out_wide), 0L)
  expect_named(out_wide, c("Variable", "Level", "n", "%"))
})

test_that("table_categorical handles grouped empty results after dropping missing", {
  df <- data.frame(
    grp = c("A", "B"),
    v1 = c(NA, NA)
  )

  out_long <- table_categorical(
    data = df,
    select = v1,
    by = grp,
    drop_na = TRUE,
    output = "long"
  )
  out_wide <- table_categorical(
    data = df,
    select = v1,
    by = grp,
    drop_na = TRUE,
    output = "data.frame"
  )
  out_default <- table_categorical(
    data = df,
    select = v1,
    by = grp,
    drop_na = TRUE,
    output = "data.frame"
  )

  expect_equal(nrow(out_long), 0L)
  expect_equal(nrow(out_wide), 0L)
  expect_equal(nrow(out_default), 0L)
  expect_true("Variable" %in% names(out_wide))
  expect_true("Variable" %in% names(out_default))
})

test_that("table_categorical warns about ignored grouped options without by", {
  df <- data.frame(v1 = c("Oui", "Non", "Oui"))

  res <- collect_warnings(
    table_categorical(
      data = df,
      select = "v1",
      include_total = FALSE,
      correct = TRUE,
      simulate_p = TRUE,
      assoc_measure = "phi",
      assoc_ci = TRUE,
      output = "long"
    )
  )

  expect_true(any(grepl("include_total", res$warnings)))
  expect_true(any(grepl("correct", res$warnings)))
  expect_true(any(grepl("simulate_p", res$warnings)))
  expect_true(any(grepl("assoc_measure", res$warnings)))
  expect_true(any(grepl("assoc_ci", res$warnings)))
  expect_s3_class(res$value, "data.frame")
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

test_that("table_categorical default output with output = 'data.frame' returns wide raw data", {
  out <- table_categorical(
    sochealth,
    select = smoking,
    output = "data.frame"
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
      output = "long"
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
      output = "long"
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
    output = "long"
  )

  out_drop <- table_categorical(
    data = df,
    select = "v1",
    by = "grp",
    labels = "Var 1",
    drop_na = TRUE,
    simulate_p = FALSE,
    output = "long"
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

test_that("table_categorical returns one-way rendered objects when requested", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("clipr")

  tt <- table_categorical(
    sochealth,
    select = smoking,
    output = "tinytable"
  )
  expect_true(methods::is(tt, "tinytable"))

  gt_tbl <- table_categorical(
    sochealth,
    select = smoking,
    output = "gt"
  )
  expect_s3_class(gt_tbl, "gt_tbl")

  ft <- table_categorical(
    sochealth,
    select = smoking,
    output = "flextable"
  )
  expect_s3_class(ft, "flextable")

  tmp_xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_xlsx), add = TRUE)
  expect_invisible(
    table_categorical(
      sochealth,
      select = smoking,
      output = "excel",
      excel_path = tmp_xlsx
    )
  )
  expect_true(file.exists(tmp_xlsx))

  tmp_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp_docx), add = TRUE)
  expect_identical(
    table_categorical(
      sochealth,
      select = smoking,
      output = "word",
      word_path = tmp_docx
    ),
    invisible(tmp_docx)
  )
  expect_true(file.exists(tmp_docx))

  clip_text <- NULL
  ns <- asNamespace("clipr")
  old_write <- get("write_clip", envir = ns)
  unlockBinding("write_clip", ns)
  assign(
    "write_clip",
    function(x, ...) {
      clip_text <<- x
      invisible(NULL)
    },
    envir = ns
  )
  lockBinding("write_clip", ns)
  on.exit(
    {
      unlockBinding("write_clip", ns)
      assign("write_clip", old_write, envir = ns)
      lockBinding("write_clip", ns)
    },
    add = TRUE
  )

  txt <- table_categorical(
    sochealth,
    select = smoking,
    output = "clipboard"
  )
  expect_type(txt, "character")
  expect_identical(txt, invisible(txt))
  expect_match(clip_text, "Variable")
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
    output = "long"
  )
  expect_true("Cramer's V" %in% names(out))
})

test_that("table_categorical drops association column when assoc_measure is none", {
  out_long <- table_categorical(
    sochealth,
    select = smoking,
    by = education,
    assoc_measure = "none",
    output = "long"
  )

  out_wide <- table_categorical(
    sochealth,
    select = smoking,
    by = education,
    assoc_measure = "none",
    output = "data.frame"
  )

  expect_false("Cramer's V" %in% names(out_long))
  expect_false(any(grepl("Cramer's V", names(out_wide), fixed = TRUE)))
  expect_true("p" %in% names(out_long))
  expect_true("p" %in% names(out_wide))
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
    output = "long"
  )
  expect_true("Goodman-Kruskal Gamma" %in% names(out))
  expect_false("Cramer's V" %in% names(out))
})

test_that("assoc_ci adds CI columns in wide raw output", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "data.frame",
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
    output = "data.frame",
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
# Ã¢â€â‚¬Ã¢â€â‚¬ levels_keep Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical levels_keep filters and reorders levels", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    levels_keep = c("Yes"),
    output = "data.frame"
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
    output = "data.frame"
  )
  lvls <- out$Level[!is.na(out$Level) & out$Level != ""]
  expect_equal(lvls, c("Low", "High", "(Missing)"))
})
# Ã¢â€â‚¬Ã¢â€â‚¬ blank_na_wide Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical blank_na_wide replaces NA with empty strings", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "data.frame",
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
    table_categorical(df, "v", "g", rescale = TRUE, output = "data.frame"),
    "rescale = TRUE.*no effect"
  )
})

# Ã¢â€â‚¬Ã¢â€â‚¬ Multiple select Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical handles multiple select in wide output", {
  out <- table_categorical(
    sochealth,
    c(smoking, physical_activity),
    education,
    output = "data.frame"
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
    output = "data.frame"
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

test_that("table_categorical grouped word and clipboard outputs work", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  skip_if_not_installed("clipr")

  tmp_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp_docx), add = TRUE)
  path <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "word",
    word_path = tmp_docx
  )
  expect_identical(path, invisible(tmp_docx))
  expect_true(file.exists(tmp_docx))

  clip_text <- NULL
  ns <- asNamespace("clipr")
  old_write <- get("write_clip", envir = ns)
  unlockBinding("write_clip", ns)
  assign(
    "write_clip",
    function(x, ...) {
      clip_text <<- x
      invisible(NULL)
    },
    envir = ns
  )
  lockBinding("write_clip", ns)
  on.exit(
    {
      unlockBinding("write_clip", ns)
      assign("write_clip", old_write, envir = ns)
      lockBinding("write_clip", ns)
    },
    add = TRUE
  )

  txt <- table_categorical(
    sochealth,
    "smoking",
    "education",
    output = "clipboard",
    assoc_ci = TRUE
  )
  expect_type(txt, "character")
  expect_match(clip_text, "Cramer's V")
  expect_match(clip_text, "CI lower")
})

test_that("table_categorical requires file paths for word and excel outputs", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  skip_if_not_installed("openxlsx2")

  expect_error(
    table_categorical(sochealth, "smoking", output = "word"),
    "word_path"
  )
  expect_error(
    table_categorical(sochealth, "smoking", output = "excel"),
    "excel_path"
  )
})

# Ã¢â€â‚¬Ã¢â€â‚¬ Excel output Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬Ã¢â€â‚¬

test_that("table_categorical writes excel file", {
  skip_if_not_installed("openxlsx2")
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

test_that("table_categorical assoc_measure = 'none' omits association column", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    assoc_measure = "none",
    output = "long"
  )
  expect_false("Cramer's V" %in% names(out))
})
test_that("table_categorical with assoc_ci includes CI columns in raw long", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    assoc_ci = TRUE,
    output = "long"
  )
  expect_true("CI lower" %in% names(out) || "ci_lower" %in% names(out))
})

test_that("table_categorical simulate_p works in long output", {
  out <- table_categorical(
    sochealth,
    "smoking",
    "education",
    simulate_p = TRUE,
    output = "long"
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
    output = "long"
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
    output = "long"
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
    output = "long"
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
      output = "long"
    ),
    warning = function(w) w
  )
  expect_s3_class(w, "simpleWarning")
  expect_null(w$call)
})

# ---- grouped empty data returns character columns, not logical ----

test_that("grouped table with empty data returns character(0) columns", {
  df <- data.frame(
    x = factor(levels = c("a", "b")),
    g = factor(levels = c("A", "B")),
    stringsAsFactors = FALSE
  )
  out <- table_categorical(df, select = "x", by = "g", output = "data.frame")
  # All columns should be character, not logical
  col_types <- vapply(out, typeof, character(1))
  expect_true(all(col_types == "character"))
})

test_that("table_categorical gt output omits association header when assoc_measure = 'none'", {
  skip_if_not_installed("gt")

  gt_tbl <- table_categorical(
    sochealth,
    "smoking",
    "education",
    assoc_measure = "none",
    output = "gt"
  )

  boxhead <- gt_tbl[["_boxhead"]]
  spanners <- gt_tbl[["_spanners"]]

  expect_false(any(boxhead$column_label == "Cramer's V"))
  expect_false(any(boxhead$var == "assoc_col"))
  expect_false(any(spanners$spanner_id == "spn_assoc"))
  expect_false(any(spanners$spanner_label == "Cramer's V"))
})

# --- Coverage tests: uncovered paths ---

test_that("table_categorical errors when select matches no columns", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_error(
    table_categorical(df, select = c(nonexistent_col)),
    "select"
  )
})

test_that("table_categorical one-way with weights", {
  df <- data.frame(
    x = factor(c("A", "B", "A", "B", "A")),
    w = c(2, 1, 3, 1, 2)
  )
  out <- table_categorical(df, select = x, weights = w, output = "data.frame")
  expect_s3_class(out, "data.frame")
  expect_true(all(c("Variable", "Level", "n", "%") %in% names(out)))
  # Weighted n should reflect weights
  expect_equal(sum(out$n), sum(df$w))
})

test_that("table_categorical one-way with levels_keep filters levels", {
  df <- data.frame(
    x = factor(c("A", "B", "C", "A", "B"), levels = c("A", "B", "C"))
  )
  out <- table_categorical(
    df,
    select = x,
    levels_keep = c("B", "A"),
    output = "data.frame"
  )
  expect_equal(as.character(out$Level), c("B", "A"))
})

test_that("table_categorical one-way with levels_keep and long output", {
  df <- data.frame(
    x = factor(c("A", "B", "C", "A", "B"), levels = c("A", "B", "C"))
  )
  out <- table_categorical(
    df,
    select = x,
    levels_keep = c("B", "C"),
    output = "long"
  )
  expect_true(all(out$level %in% c("B", "C")))
})

test_that("table_categorical one-way with decimal_mark comma", {
  df <- data.frame(x = factor(c("A", "B", "A", "B", "A")))
  out <- table_categorical(
    df,
    select = x,
    decimal_mark = ",",
    output = "default"
  )
  disp <- attr(out, "display_df")
  # Percentages should use comma as decimal separator
  pct_col <- disp[["%"]]
  expect_true(any(grepl(",", pct_col)))
})

test_that("table_categorical one-way with blank_na_wide", {
  df <- data.frame(
    x = factor(c("A", NA, "B", NA)),
    y = factor(c("C", "D", NA, NA))
  )
  out <- table_categorical(
    df,
    select = c(x, y),
    drop_na = TRUE,
    blank_na_wide = TRUE,
    output = "data.frame"
  )
  expect_s3_class(out, "data.frame")
})

test_that("table_categorical one-way empty after dropping NA produces 0-row data.frame", {
  df <- data.frame(x = factor(c(NA, NA, NA)))
  out_wide <- table_categorical(
    df,
    select = x,
    drop_na = TRUE,
    output = "data.frame"
  )
  expect_equal(nrow(out_wide), 0L)
  out_long <- table_categorical(df, select = x, drop_na = TRUE, output = "long")
  expect_equal(nrow(out_long), 0L)
})

test_that("table_categorical grouped default output prints and returns invisibly", {
  out <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex
  )
  expect_s3_class(out, "spicy_categorical_table")
  expect_equal(attr(out, "group_var"), "sex")
})

test_that("table_categorical grouped with levels_keep filters levels", {
  out <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    levels_keep = c("Yes"),
    output = "data.frame"
  )
  out_long <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    levels_keep = c("Yes"),
    output = "long"
  )
  expect_true(all(out_long$level == "Yes"))
})

test_that("table_categorical grouped with assoc_measure = none", {
  out <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    assoc_measure = "none",
    output = "long"
  )
  # No association measure column
  expect_false("Cramer's V" %in% names(out))
})

test_that("table_categorical grouped empty after dropping NA", {
  df <- data.frame(
    grp = factor(c("A", "B")),
    v = factor(c(NA, NA))
  )
  out_wide <- table_categorical(
    df,
    select = v,
    by = grp,
    drop_na = TRUE,
    output = "data.frame"
  )
  expect_equal(nrow(out_wide), 0L)
  out_long <- table_categorical(
    df,
    select = v,
    by = grp,
    drop_na = TRUE,
    output = "long"
  )
  expect_equal(nrow(out_long), 0L)
})

test_that("table_categorical one-way weighted with rescale", {
  df <- data.frame(
    x = factor(c("A", "B", "A", "B", "A")),
    w = c(10, 5, 10, 5, 10)
  )
  out <- table_categorical(
    df,
    select = x,
    weights = w,
    rescale = TRUE,
    output = "data.frame"
  )
  # After rescaling, total n should equal nrow(df)
  expect_equal(sum(out$n), nrow(df), tolerance = 0.01)
})

test_that("table_categorical handles Missing_ label collision", {
  df <- data.frame(
    x = factor(c("(Missing)", "(Missing_1)", NA, "B")),
    stringsAsFactors = FALSE
  )
  out <- table_categorical(df, select = x, drop_na = FALSE, output = "long")
  expect_true(any(grepl("Missing", out$level)))
  # Should not have duplicate level names
  expect_equal(length(unique(out$level)), nrow(out))
})

test_that("table_categorical grouped with levels_keep and default output", {
  out <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    levels_keep = c("Yes")
  )
  expect_s3_class(out, "spicy_categorical_table")
  disp <- attr(out, "display_df")
  # Only "Yes" level should appear in indented rows
  indented <- disp$Variable[startsWith(disp$Variable, "  ")]
  expect_true(all(trimws(indented) == "Yes"))
})

test_that("table_categorical grouped empty via levels_keep with non-matching levels", {
  df <- data.frame(
    grp = factor(c("A", "B", "A", "B")),
    v = factor(c("x", "y", "x", "y"))
  )
  out <- table_categorical(
    df,
    select = v,
    by = grp,
    levels_keep = c("nonexistent"),
    output = "data.frame"
  )
  expect_equal(nrow(out), 0L)
  out_long <- table_categorical(
    df,
    select = v,
    by = grp,
    levels_keep = c("nonexistent"),
    output = "long"
  )
  expect_equal(nrow(out_long), 0L)
  # Also with assoc_measure = "none" to cover L1196
  out_none <- table_categorical(
    df,
    select = v,
    by = grp,
    levels_keep = c("nonexistent"),
    assoc_measure = "none",
    output = "long"
  )
  expect_equal(nrow(out_none), 0L)
})

test_that("table_categorical grouped with assoc_measure = none and default output", {
  out <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    assoc_measure = "none"
  )
  expect_s3_class(out, "spicy_categorical_table")
})

test_that("table_categorical one-way with levels_keep that filters some levels", {
  df <- data.frame(
    x = factor(c("A", "B", "C", "D"), levels = c("A", "B", "C", "D")),
    y = factor(c("P", "Q", "P", "Q"))
  )
  out <- table_categorical(
    df,
    select = c(x, y),
    levels_keep = c("A", "C", "P"),
    output = "data.frame"
  )
  # Should only have matching levels
  expect_true(all(out$Level %in% c("A", "C", "P")))
})

test_that("table_categorical grouped with decimal_mark comma and default output", {
  out <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    decimal_mark = ","
  )
  expect_s3_class(out, "spicy_categorical_table")
  disp <- attr(out, "display_df")
  # p-value and percentage columns should use comma
  p_col <- disp$p
  non_empty_p <- p_col[nzchar(p_col)]
  if (length(non_empty_p) > 0) {
    expect_true(any(grepl(",", non_empty_p)) || any(grepl("<", non_empty_p)))
  }
})

test_that("table_categorical grouped tinytable with assoc_measure = none", {
  skip_if_not_installed("tinytable")
  tt <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    assoc_measure = "none",
    output = "tinytable"
  )
  expect_true(inherits(tt, "tinytable"))
})

test_that("table_categorical grouped excel with assoc_ci", {
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  path <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    assoc_ci = TRUE,
    output = "excel",
    excel_path = tmp
  )
  expect_true(file.exists(path))
})

test_that("table_categorical grouped excel with assoc_measure = none", {
  skip_if_not_installed("openxlsx2")
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  path <- table_categorical(
    data = sochealth,
    select = smoking,
    by = sex,
    assoc_measure = "none",
    output = "excel",
    excel_path = tmp
  )
  expect_true(file.exists(path))
})

test_that("table_categorical one-way all-NA renders empty default table", {
  df <- data.frame(x = factor(c(NA, NA, NA)))
  out <- table_categorical(df, select = x, drop_na = TRUE, output = "default")
  expect_s3_class(out, "spicy_categorical_table")
  expect_equal(nrow(out), 0L)
})

test_that("table_categorical grouped all-NA renders empty default table", {
  df <- data.frame(
    grp = factor(c("A", "B", "A")),
    v = factor(c(NA, NA, NA))
  )
  out <- table_categorical(
    df,
    select = v,
    by = grp,
    drop_na = TRUE,
    output = "default"
  )
  expect_s3_class(out, "spicy_categorical_table")
})

test_that("table_categorical one-way levels_keep with no match returns empty", {
  df <- data.frame(x = factor(c("A", "B"), levels = c("A", "B", "C")))
  out <- table_categorical(
    df,
    select = x,
    levels_keep = c("nonexistent"),
    output = "data.frame"
  )
  expect_equal(nrow(out), 0L)
  # levels_keep includes "C" which exists in factor levels but has 0 obs
  # → covers the `next` at match(lv, vals) returning NA
  out2 <- table_categorical(
    df,
    select = x,
    levels_keep = c("A", "C"),
    output = "data.frame"
  )
  expect_equal(nrow(out2), 1L)
  expect_equal(as.character(out2$Level), "A")
  # Also test default output path (covers make_report_wide_oneway empty path)
  out3 <- table_categorical(
    df,
    select = x,
    levels_keep = c("nonexistent"),
    output = "default"
  )
  expect_s3_class(out3, "spicy_categorical_table")
})

test_that("table_categorical errors for missing tinytable package", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "tinytable") {
        return(FALSE)
      }
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  expect_error(
    table_categorical(sochealth, select = smoking, output = "tinytable"),
    "tinytable"
  )
})

test_that("table_categorical errors for missing gt package", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "gt") {
        return(FALSE)
      }
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  expect_error(
    table_categorical(sochealth, select = smoking, output = "gt"),
    "gt"
  )
})

test_that("table_categorical errors for missing flextable package", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "flextable") {
        return(FALSE)
      }
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  expect_error(
    table_categorical(sochealth, select = smoking, output = "flextable"),
    "flextable"
  )
})

test_that("table_categorical errors for missing openxlsx2 package", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "openxlsx2") {
        return(FALSE)
      }
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      output = "excel",
      excel_path = tempfile(fileext = ".xlsx")
    ),
    "openxlsx2"
  )
})

test_that("table_categorical errors for missing clipr package", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "clipr") {
        return(FALSE)
      }
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  expect_error(
    table_categorical(sochealth, select = smoking, output = "clipboard"),
    "clipr"
  )
})

test_that("table_categorical one-way word output errors for missing officer package", {
  skip("Cannot mock officer requireNamespace without recursion")
})

test_that("table_categorical word output errors when word_path is missing", {
  expect_error(
    table_categorical(sochealth, select = smoking, by = sex, output = "word"),
    "word_path"
  )
})

test_that("table_categorical excel output errors when excel_path is missing", {
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      output = "excel"
    ),
    "excel_path"
  )
})

test_that("table_categorical grouped errors for missing tinytable", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "tinytable") {
        return(FALSE)
      }
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      output = "tinytable"
    ),
    "tinytable"
  )
})

test_that("table_categorical grouped errors for missing gt", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "gt") {
        return(FALSE)
      }
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  expect_error(
    table_categorical(sochealth, select = smoking, by = sex, output = "gt"),
    "gt"
  )
})

test_that("table_categorical grouped errors for missing flextable", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "flextable") {
        return(FALSE)
      }
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      output = "flextable"
    ),
    "flextable"
  )
})

test_that("table_categorical grouped errors for missing openxlsx2", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "openxlsx2") {
        return(FALSE)
      }
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      output = "excel",
      excel_path = tempfile(fileext = ".xlsx")
    ),
    "openxlsx2"
  )
})

test_that("table_categorical grouped errors for missing clipr", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "clipr") {
        return(FALSE)
      }
      base::requireNamespace(pkg, ...)
    },
    .package = "base"
  )
  expect_error(
    table_categorical(
      sochealth,
      select = smoking,
      by = sex,
      output = "clipboard"
    ),
    "clipr"
  )
})

test_that("table_categorical grouped word errors for missing officer", {
  skip("Cannot mock officer requireNamespace without recursion")
})

# ---- harmonisation with table_continuous() / _lm() (Phase 2) -------------

test_that("align argument validates and is stored as attribute", {
  for (a in c("decimal", "auto", "center", "right")) {
    out <- table_categorical(sochealth, select = smoking, by = sex, align = a)
    expect_equal(attr(out, "align"), a)
  }
  expect_error(
    table_categorical(sochealth, select = smoking, by = sex, align = "bogus"),
    "should be one of"
  )
})

test_that("align defaults to 'decimal' on the printed object", {
  out <- table_categorical(sochealth, select = smoking, by = sex)
  expect_equal(attr(out, "align"), "decimal")
  out_ow <- table_categorical(sochealth, select = smoking)
  expect_equal(attr(out_ow, "align"), "decimal")
})

test_that("align = 'decimal' produces gt and tinytable outputs", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tinytable")
  out_gt <- table_categorical(
    sochealth, select = smoking, by = sex, output = "gt", align = "decimal"
  )
  expect_s3_class(out_gt, "gt_tbl")

  out_tt <- table_categorical(
    sochealth, select = smoking, by = sex, output = "tinytable", align = "decimal"
  )
  expect_true(inherits(out_tt, "tinytable"))
})

test_that("align = 'center' / 'right' / 'auto' all render gt + tinytable", {
  skip_if_not_installed("gt")
  skip_if_not_installed("tinytable")
  for (a in c("center", "right", "auto")) {
    expect_s3_class(
      table_categorical(
        sochealth, select = smoking, by = sex, output = "gt", align = a
      ),
      "gt_tbl"
    )
    expect_true(inherits(
      table_categorical(
        sochealth, select = smoking, by = sex, output = "tinytable", align = a
      ),
      "tinytable"
    ))
  }
})

# ---- broom S3 methods -----------------------------------------------------

test_that("as.data.frame() strips spicy classes and rendering attrs", {
  out <- table_categorical(sochealth, select = smoking, by = sex)
  df <- as.data.frame(out)
  expect_true(inherits(df, "data.frame"))
  expect_false("spicy_categorical_table" %in% class(df))
  expect_false("spicy_table" %in% class(df))
  expect_null(attr(df, "display_df"))
  expect_null(attr(df, "long_data"))
  expect_null(attr(df, "align"))
  # group_var preserved as provenance
  expect_equal(attr(df, "group_var"), "sex")
})

test_that("as_tibble() returns a tbl_df", {
  skip_if_not_installed("tibble")
  out <- table_categorical(sochealth, select = smoking, by = sex)
  tb <- tibble::as_tibble(out)
  expect_s3_class(tb, "tbl_df")
})

test_that("tidy() returns long-format with broom-conventional columns (cross-tab)", {
  out <- table_categorical(
    sochealth, select = c(smoking, physical_activity), by = sex
  )
  td <- broom::tidy(out)
  expect_setequal(
    names(td),
    c("outcome", "level", "group", "n", "proportion")
  )
  expect_true(all(td$proportion >= 0 & td$proportion <= 1))
  expect_equal(unique(td$outcome), c("smoking", "physical_activity"))
  # All three sex groups appear (Female, Male, Total)
  expect_true(all(c("Female", "Male", "Total") %in% td$group))
})

test_that("tidy() returns no group column without by", {
  out <- table_categorical(sochealth, select = smoking)
  td <- broom::tidy(out)
  expect_false("group" %in% names(td))
  expect_setequal(names(td), c("outcome", "level", "n", "proportion"))
})

test_that("glance() returns chi-squared test + association measure (cross-tab)", {
  out <- table_categorical(
    sochealth, select = c(smoking, physical_activity), by = sex
  )
  gl <- broom::glance(out)
  expect_setequal(
    names(gl),
    c(
      "outcome",
      "test_type",
      "statistic",
      "df",
      "p.value",
      "assoc_type",
      "assoc_value",
      "assoc_ci_lower",
      "assoc_ci_upper",
      "n_total"
    )
  )
  expect_equal(nrow(gl), 2L)
  expect_true(all(gl$test_type == "chi_squared"))
  expect_true(all(is.finite(gl$statistic)))
  expect_true(all(gl$df >= 1L))
  expect_true(all(gl$p.value >= 0 & gl$p.value <= 1))
  expect_true(all(gl$assoc_type == "Cramer's V"))
})

test_that("glance() returns NA test/ES, populated n_total without by", {
  out <- table_categorical(sochealth, select = smoking)
  gl <- broom::glance(out)
  expect_equal(nrow(gl), 1L)
  expect_true(is.na(gl$test_type))
  expect_true(is.na(gl$statistic))
  expect_true(is.na(gl$p.value))
  expect_equal(gl$n_total, 1175L)  # observed n for smoking
})

test_that("glance() picks up assoc CIs when assoc_ci = TRUE", {
  out <- table_categorical(
    sochealth, select = smoking, by = sex, assoc_ci = TRUE
  )
  gl <- broom::glance(out)
  expect_true(is.finite(gl$assoc_ci_lower))
  expect_true(is.finite(gl$assoc_ci_upper))
  expect_true(gl$assoc_ci_lower <= gl$assoc_value)
  expect_true(gl$assoc_ci_upper >= gl$assoc_value)
})
