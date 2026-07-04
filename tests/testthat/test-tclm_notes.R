# table_continuous_lm() note plumbing: covariate-adjustment line +
# non-classical SE-estimator disclosure, shared between the console
# print method and every rich exporter (gt / flextable / tinytable /
# word / excel). Mirrors table_regression()'s footer doctrine: robust
# SEs are never silently labelled, and notes carry into every rich
# format.

# ---- shared note builder ----

test_that(".tclm_note_text composes lines from stored attributes", {
  x <- data.frame(a = 1)
  # Classical + unadjusted: nothing to disclose.
  expect_null(spicy:::.tclm_note_text(x))

  # Non-classical vcov alone: SE line only.
  attr(x, "vcov_type") <- "HC1"
  expect_identical(
    spicy:::.tclm_note_text(x),
    "Note. Std. errors: heteroskedasticity-robust (HC1)."
  )

  # Covariates + non-classical vcov: two lines, "Note. " on the first.
  attr(x, "covariates") <- c("age", "sex")
  attr(x, "adjustment") <- "balanced"
  expect_identical(
    spicy:::.tclm_note_text(x),
    paste0(
      "Note. Adjusted for age, sex (balanced).\n",
      "Std. errors: heteroskedasticity-robust (HC1)."
    )
  )

  # Covariates alone: existing wording, unchanged.
  attr(x, "vcov_type") <- "classical"
  expect_identical(
    spicy:::.tclm_note_text(x),
    "Note. Adjusted for age, sex (balanced)."
  )
})

test_that(".tclm_vcov_label covers all estimator branches", {
  expect_identical(
    spicy:::.tclm_vcov_label("HC3"),
    "heteroskedasticity-robust (HC3)"
  )
  expect_identical(
    spicy:::.tclm_vcov_label("HC0"),
    "heteroskedasticity-robust (HC0)"
  )
  expect_identical(
    spicy:::.tclm_vcov_label("CR2"),
    "cluster-robust (CR2)"
  )
  expect_identical(
    spicy:::.tclm_vcov_label("CR2", NA_character_),
    "cluster-robust (CR2)"
  )
  expect_identical(
    spicy:::.tclm_vcov_label("CR2", "id"),
    "cluster-robust (CR2), clusters by id"
  )
  expect_identical(
    spicy:::.tclm_vcov_label("bootstrap"),
    "nonparametric bootstrap"
  )
  expect_identical(
    spicy:::.tclm_vcov_label("jackknife"),
    "jackknife"
  )
})

# ---- console ----

test_that("console note discloses adjustment and robust SEs together", {
  txt <- paste(
    capture.output(
      table_continuous_lm(
        sochealth,
        select = wellbeing_score,
        by = physical_activity,
        covariates = c(age, sex),
        vcov = "HC3"
      )
    ),
    collapse = "\n"
  )
  expect_match(
    txt,
    "Note. Adjusted for age, sex (proportional).",
    fixed = TRUE
  )
  expect_match(
    txt,
    "Std. errors: heteroskedasticity-robust (HC3).",
    fixed = TRUE
  )
})

test_that("console note is absent for classical unadjusted tables", {
  txt <- paste(
    capture.output(
      table_continuous_lm(
        sochealth,
        select = wellbeing_score,
        by = physical_activity
      )
    ),
    collapse = "\n"
  )
  expect_false(grepl("Std. errors:", txt, fixed = TRUE))
  expect_false(grepl("Note.", txt, fixed = TRUE))
})

test_that("bootstrap SEs are disclosed in the console note", {
  set.seed(42)
  dat <- as.data.frame(
    sochealth[1:150, c("wellbeing_score", "physical_activity")]
  )
  txt <- paste(
    capture.output(
      table_continuous_lm(
        dat,
        select = wellbeing_score,
        by = physical_activity,
        vcov = "bootstrap",
        boot_n = 200
      )
    ),
    collapse = "\n"
  )
  expect_match(
    txt,
    "Note. Std. errors: nonparametric bootstrap.",
    fixed = TRUE
  )
})

# ---- cluster_name attribute ----

test_that("cluster_name attr is stored and disclosed for CR vcov", {
  skip_if_not_installed("clubSandwich")
  txt <- paste(
    capture.output(
      tbl <- table_continuous_lm(
        sochealth,
        select = wellbeing_score,
        by = physical_activity,
        vcov = "CR0",
        cluster = ~region
      )
    ),
    collapse = "\n"
  )
  expect_identical(attr(tbl, "cluster_name"), "region")
  expect_match(
    txt,
    "Std. errors: cluster-robust (CR0), clusters by region.",
    fixed = TRUE
  )
  # The coercion methods keep dropping the rendering-only attribute.
  expect_null(attr(as.data.frame(tbl), "cluster_name"))
})

test_that("cluster_name attr is NA for an anonymous cluster vector", {
  skip_if_not_installed("clubSandwich")
  cl <- sochealth$region
  # suppressWarnings: the pre-existing name-detection probe
  # (detect_weights_column_name -> tidyselect::eval_select) emits a
  # once-per-session "external vector in selections" deprecation
  # warning for raw-vector clusters; unrelated to the behavior under
  # test here (the NA fallback).
  txt <- paste(
    suppressWarnings(capture.output(
      tbl <- table_continuous_lm(
        sochealth,
        select = wellbeing_score,
        by = physical_activity,
        vcov = "CR2",
        cluster = cl
      )
    )),
    collapse = "\n"
  )
  expect_identical(attr(tbl, "cluster_name"), NA_character_)
  expect_match(txt, "Std. errors: cluster-robust (CR2).", fixed = TRUE)
  expect_false(grepl("clusters by", txt, fixed = TRUE))
})

test_that("cluster_name attr resolves string and bare-column forms", {
  skip_if_not_installed("clubSandwich")
  out_string <- capture.output(
    tbl_string <- table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = physical_activity,
      vcov = "CR0",
      cluster = "region"
    )
  )
  expect_identical(attr(tbl_string, "cluster_name"), "region")

  out_bare <- capture.output(
    tbl_bare <- table_continuous_lm(
      sochealth,
      select = wellbeing_score,
      by = physical_activity,
      vcov = "CR0",
      cluster = region
    )
  )
  expect_identical(attr(tbl_bare, "cluster_name"), "region")
})

# ---- gt ----

test_that("gt output stashes the note and injects it in rendered HTML", {
  skip_if_not_installed("gt")
  skip_if_not_installed("knitr")
  g <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = physical_activity,
    covariates = c(age, sex),
    vcov = "HC3",
    output = "gt"
  )
  expect_s3_class(g, "spicy_gt")
  note <- attr(g, "spicy_note")
  expect_match(note, "Adjusted for age, sex (proportional).", fixed = TRUE)
  expect_match(note, "heteroskedasticity-robust (HC3)", fixed = TRUE)
  # The note div is injected at render time by the shared spicy_gt
  # knit_print / print methods (gt::as_raw_html() alone renders the
  # bare table; the note deliberately lives OUTSIDE the table grid).
  html <- as.character(knit_print.spicy_gt(g))
  expect_match(html, "spicy-gt-note", fixed = TRUE)
  expect_match(html, "Adjusted for age, sex (proportional).", fixed = TRUE)
  expect_match(
    html,
    "Std. errors: heteroskedasticity-robust (HC3).",
    fixed = TRUE
  )
})

test_that("gt output without a note keeps the plain gt class", {
  skip_if_not_installed("gt")
  g <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = physical_activity,
    output = "gt"
  )
  expect_false(inherits(g, "spicy_gt"))
  expect_null(attr(g, "spicy_note"))
})

# ---- flextable ----

test_that("flextable output adds the note footer and HTML note div", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("knitr")
  ft <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = physical_activity,
    covariates = c(age, sex),
    vcov = "HC3",
    output = "flextable"
  )
  expect_s3_class(ft, "spicy_flextable")
  expect_match(
    attr(ft, "spicy_note"),
    "Adjusted for age, sex (proportional).",
    fixed = TRUE
  )
  # Footer paragraph: italic "Note." chunk + regular remainder (same
  # two-chunk split as table_regression()'s flextable footer).
  foot_cell <- ft$footer$content$data[[1, 1]]
  expect_identical(nrow(foot_cell), 2L)
  expect_identical(foot_cell$txt[[1L]], "Note.")
  expect_true(foot_cell$italic[[1L]])
  expect_match(
    foot_cell$txt[[2L]],
    "Adjusted for age, sex (proportional).",
    fixed = TRUE
  )
  expect_match(
    foot_cell$txt[[2L]],
    "Std. errors: heteroskedasticity-robust (HC3).",
    fixed = TRUE
  )
  # knit-rendered HTML strips the tfoot and re-injects the note div.
  html <- as.character(knit_print.spicy_flextable(ft))
  expect_match(html, "spicy-ft-note", fixed = TRUE)
  expect_match(html, "Adjusted for age, sex (proportional).", fixed = TRUE)
})

test_that("flextable output without a note keeps the plain class", {
  skip_if_not_installed("flextable")
  ft <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = physical_activity,
    output = "flextable"
  )
  expect_false(inherits(ft, "spicy_flextable"))
  expect_null(attr(ft, "spicy_note"))
  # No footer row was added.
  expect_identical(flextable::nrow_part(ft, part = "footer"), 0L)
})

# ---- tinytable ----

test_that("tinytable HTML output injects the note div outside the table", {
  skip_if_not_installed("tinytable")
  tt <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = physical_activity,
    covariates = c(age, sex),
    vcov = "HC3",
    output = "tinytable"
  )
  html <- tinytable::save_tt(tt, output = "html")
  expect_match(html, "spicy-tt-note", fixed = TRUE)
  expect_match(html, "Adjusted for age, sex (proportional).", fixed = TRUE)
  expect_match(
    html,
    "Std. errors: heteroskedasticity-robust (HC3).",
    fixed = TRUE
  )
  # The rendered tfoot was stripped (the note lives outside the grid).
  expect_false(grepl("<tfoot>", html, fixed = TRUE))
})

test_that("tinytable output without a note renders no note div", {
  skip_if_not_installed("tinytable")
  tt <- table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = physical_activity,
    output = "tinytable"
  )
  html <- tinytable::save_tt(tt, output = "html")
  expect_false(grepl("spicy-tt-note", html, fixed = TRUE))
  expect_match(html, "<table", fixed = TRUE)
})

# ---- word ----

test_that("word output carries the note into the docx", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  path <- withr::local_tempfile(fileext = ".docx")
  table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = physical_activity,
    covariates = c(age, sex),
    vcov = "HC3",
    output = "word",
    word_path = path
  )
  docx_txt <- paste(
    officer::docx_summary(officer::read_docx(path))$text,
    collapse = " "
  )
  expect_match(
    docx_txt,
    "Adjusted for age, sex (proportional).",
    fixed = TRUE
  )
  expect_match(
    docx_txt,
    "Std. errors: heteroskedasticity-robust (HC3).",
    fixed = TRUE
  )
})

# ---- excel ----

test_that("excel output writes the note lines below the table", {
  skip_if_not_installed("openxlsx2")
  path <- withr::local_tempfile(fileext = ".xlsx")
  table_continuous_lm(
    sochealth,
    select = wellbeing_score,
    by = physical_activity,
    covariates = c(age, sex),
    vcov = "HC3",
    output = "excel",
    excel_path = path
  )
  wb <- openxlsx2::wb_load(path)
  cells <- openxlsx2::wb_to_df(wb, sheet = 1, col_names = FALSE)
  all_txt <- paste(
    stats::na.omit(unlist(lapply(cells, as.character))),
    collapse = " "
  )
  expect_match(
    all_txt,
    "Note. Adjusted for age, sex (proportional).",
    fixed = TRUE
  )
  expect_match(
    all_txt,
    "Std. errors: heteroskedasticity-robust (HC3).",
    fixed = TRUE
  )
})
