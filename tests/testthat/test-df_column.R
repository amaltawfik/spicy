# `show_columns = "df"`: the degrees of freedom of each coefficient's
# test, the `df` of tidy(). Oracles are the packages that compute the
# tests (clubSandwich, nlme, survey, fixest, estimatr).

.dfc_cols <- c("b", "se", "t", "df", "p")

# The df of the coefficient rows of a one-model table, in their order
# (reference levels left out), from the typed body.
.dfc_values <- function(tbl) {
  s <- as_structured(tbl)$body
  s[["df"]][
    s[[".row_role"]] %in% c("coef", "level") &
      !s[[".variable"]] %in% "fixed_effects"
  ]
}

# The console cells of column `col` on the coefficient rows.
.dfc_console <- function(tbl, col = "df") {
  s <- as_structured(tbl)$body
  keep <- s[[".row_role"]] %in% c("coef", "level")
  trimws(gsub("\u2007", " ", .dfc_char(tbl)[[col]][keep], fixed = TRUE))
}

# The character body the console prints.
.dfc_char <- function(tbl) structure(tbl, class = "data.frame")

test_that("an lm shows its residual df on every coefficient row", {
  fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  tb <- table_regression(fit, show_columns = .dfc_cols)
  expect_equal(.dfc_values(tb), rep(df.residual(fit), 4L))
  expect_identical(.dfc_console(tb), rep("28", 4L))
  # The reference level is a dash, as in every other column.
  body <- .dfc_char(tb)
  expect_identical(trimws(body$df[grepl("ref", body$Variable)]),"\u2013")
  # The column header, and no change to the default columns.
  expect_true("df" %in% names(body))
  expect_false("df" %in% names(as.data.frame(table_regression(fit))))
})

test_that("an lm under CR2 shows the Satterthwaite df of clubSandwich", {
  skip_if_not_installed("clubSandwich")
  fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  tb <- table_regression(
    fit,
    show_columns = .dfc_cols,
    vcov = "CR2",
    cluster = ~gear
  )
  oracle <- clubSandwich::coef_test(
    fit,
    vcov = "CR2",
    cluster = mtcars$gear,
    test = "Satterthwaite"
  )$df_Satt
  expect_equal(.dfc_values(tb), oracle, tolerance = 1e-8)
  # A fractional df prints with one decimal.
  expect_identical(.dfc_console(tb), sprintf("%.1f", oracle))
  expect_identical(
    .dfc_values(table_regression(fit, show_columns = .dfc_cols)),
    .dfc_values(table_regression(fit, show_columns = c("b", "df")))
  )
})

test_that("lme shows its containment df, lmer its Satterthwaite df under CR2", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("lme4")
  skip_if_not_installed("clubSandwich")
  data("sleepstudy", package = "lme4", envir = environment())
  f_lme <- nlme::lme(Reaction ~ Days, random = ~ 1 | Subject, data = sleepstudy)
  tb <- table_regression(f_lme, show_columns = .dfc_cols)
  expect_equal(
    .dfc_values(tb),
    unname(summary(f_lme)$tTable[, "DF"]),
    tolerance = 1e-8
  )
  # The rows of the random effects have no test: a dash, as under t.
  body <- .dfc_char(tb)
  vc <-grepl("\u03C3", body$Variable, fixed = TRUE)
  expect_true(any(vc))
  expect_identical(unique(trimws(body$df[vc])), "\u2013")

  f_lmer <- suppressMessages(
    lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  )
  tb_cr2 <- table_regression(
    f_lmer,
    show_columns = .dfc_cols,
    vcov = "CR2",
    cluster = ~Subject
  )
  oracle <- clubSandwich::coef_test(
    f_lmer,
    vcov = "CR2",
    cluster = sleepstudy$Subject,
    test = "Satterthwaite"
  )$df_Satt
  expect_equal(.dfc_values(tb_cr2), oracle, tolerance = 1e-8)
})

test_that("a z test leaves its cell blank, beside a model with t tests", {
  skip_if_not_installed("lme4")
  data("sleepstudy", package = "lme4", envir = environment())
  f_lm <- lm(Reaction ~ Days, data = sleepstudy)
  f_lmer <- suppressMessages(
    lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  )
  tb <- suppressMessages(
    table_regression(list(f_lm, f_lmer), show_columns = .dfc_cols)
  )
  s <- as_structured(tb)
  rows <- s$body[[".row_role"]] %in% "coef"
  expect_equal(s$body[["Model 1: df"]][rows], rep(df.residual(f_lm), 2L))
  expect_true(all(is.na(s$body[["Model 2: df"]][rows])))
  # No status: an absent cell, not an undefined one.
  expect_identical(unique(s$cell_status[["Model 2: df"]][rows]), "")
  body <- .dfc_char(tb)
  expect_identical(unique(trimws(body[["Model 2: df"]][rows])), "")
  # tidy() keeps the Inf of the z test.
  td <- broom::tidy(tb)
  expect_true(all(is.infinite(td$df[td$model_id == "M2" & td$estimate_type == "B"])))
})

test_that("a table of z tests refuses the token with a classed error", {
  g <- glm(am ~ wt, data = mtcars, family = binomial)
  expect_error(
    table_regression(g, show_columns = .dfc_cols),
    class = "spicy_invalid_input"
  )
  expect_snapshot(
    table_regression(g, show_columns = .dfc_cols),
    error = TRUE
  )
  # Two z-test models: refused too.
  expect_error(
    table_regression(list(g, update(g, . ~ . + hp)), show_columns = .dfc_cols),
    class = "spicy_invalid_input"
  )
  # A Gamma glm estimates its dispersion: t tests.
  gg <- glm(mpg ~ wt, data = mtcars, family = Gamma)
  expect_equal(
    .dfc_values(table_regression(gg, show_columns = .dfc_cols)),
    rep(df.residual(gg), 2L)
  )
})

test_that("svyglm, feols and lm_robust show the df of their own tests", {
  skip_if_not_installed("survey")
  skip_if_not_installed("fixest")
  skip_if_not_installed("estimatr")
  data("api", package = "survey", envir = environment())
  des <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1,
    fpc = ~fpc
  )
  sv <- survey::svyglm(api00 ~ ell + meals, design = des)
  expect_equal(
    .dfc_values(table_regression(sv, show_columns = .dfc_cols)),
    rep(survey::degf(des) + 1 - length(coef(sv)), 3L)
  )
  expect_equal(
    .dfc_values(table_regression(sv, show_columns = .dfc_cols)),
    rep(sv$df.residual, 3L)
  )

  withr::local_envvar(
    R_USER_CONFIG_DIR = withr::local_tempdir()
  )
  fe <- fixest::feols(mpg ~ wt + hp | cyl, data = mtcars)
  expect_equal(
    .dfc_values(table_regression(fe, show_columns = .dfc_cols)),
    rep(fixest::degrees_freedom(fe, "t"), 2L)
  )
  fe_cl <- fixest::feols(mpg ~ wt + hp | cyl, data = mtcars, cluster = ~gear)
  expect_equal(
    .dfc_values(table_regression(fe_cl, show_columns = .dfc_cols)),
    rep(fixest::degrees_freedom(fe_cl, "t"), 2L)
  )

  lr <- estimatr::lm_robust(mpg ~ wt + hp, data = mtcars)
  expect_equal(
    .dfc_values(table_regression(lr, show_columns = .dfc_cols)),
    unname(lr$df)
  )
})

test_that("every engine prints the console's df cells", {
  skip_if_not_installed("clubSandwich")
  fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  build <- function(output, ...) {
    table_regression(
      fit,
      show_columns = .dfc_cols,
      vcov = "CR2",
      cluster = ~gear,
      output = output,
      ...
    )
  }
  norm <- function(x) {
    x <- gsub("\u2212", "-", as.character(x), fixed = TRUE)
    trimws(gsub("\u2007", " ", x, fixed = TRUE))
  }
  console <- norm(.dfc_char(build("default"))$df)
  structured <- norm(
    spicy:::.format_structured_to_string_body(as_structured(build("default")))$df
  )
  expect_identical(structured, console)
  expect_true(all(c("1.9", "1.5", "1.8") %in% console))
  if (requireNamespace("gt", quietly = TRUE)) {
    g <- build("gt")
    expect_identical(norm(g[["_data"]]$df), console)
  }
  if (requireNamespace("tinytable", quietly = TRUE)) {
    tt <- build("tinytable")
    expect_identical(norm(tt@data$df), console)
  }
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- build("flextable")
    expect_identical(norm(ft$body$dataset$df), console)
    path <- withr::local_tempfile(fileext = ".docx")
    build("word", word_path = path)
    xml <- paste(
      readLines(unz(path, "word/document.xml"), warn = FALSE),
      collapse = ""
    )
    expect_match(xml, ">df<", fixed = TRUE)
    expect_match(xml, ">1.9<", fixed = TRUE)
  }
  # output = "data.frame" keeps the numbers.
  df_out <- build("data.frame")
  expect_type(df_out$df, "double")
  expect_equal(
    df_out$df[df_out$.row_role %in% c("coef", "level")],
    clubSandwich::coef_test(
      fit,
      vcov = "CR2",
      cluster = mtcars$gear,
      test = "Satterthwaite"
    )$df_Satt,
    tolerance = 1e-8
  )
  # output = "long" has had the column all along.
  lg <- build("long")
  expect_true("df" %in% names(lg))
})

test_that("Excel writes the df as a number with one decimal when fractional", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("clubSandwich")
  fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  cell <- function(path, label) {
    wb <- openxlsx2::wb_load(path)
    m <- as.matrix(openxlsx2::wb_to_df(wb, col_names = FALSE))
    storage.mode(m) <- "character"
    hdr <- which(apply(m, 1L, function(r) any(trimws(r) %in% "df")))[1L]
    col <- colnames(m)[which(trimws(m[hdr, ]) %in% "df")]
    row <- rownames(m)[which(trimws(m[, "A"]) %in% label)]
    ref <- paste0(col, row)
    cc <- wb$worksheets[[1L]]$sheet_data$cc
    s <- cc$c_s[cc$r == ref]
    xf <- wb$styles_mgr$styles$cellXfs[[as.integer(s) + 1L]]
    id <- sub('.*numFmtId="([0-9]+)".*', "\\1", xf)
    fmts <- wb$styles_mgr$styles$numFmts
    hit <- grep(paste0('numFmtId="', id, '"'), fmts, value = TRUE)
    list(
      type = cc$c_t[cc$r == ref],
      value = as.numeric(cc$v[cc$r == ref]),
      fmt = if (length(hit)) sub('.*formatCode="([^"]*)".*', "\\1", hit) else id
    )
  }
  path <- withr::local_tempfile(fileext = ".xlsx")
  table_regression(
    fit,
    show_columns = .dfc_cols,
    vcov = "CR2",
    cluster = ~gear,
    output = "excel",
    excel_path = path
  )
  wt <- cell(path, "wt")
  expect_false(wt$type %in% c("inlineStr", "str", "s"))
  expect_equal(wt$value, 1.451446, tolerance = 1e-6)
  expect_identical(wt$fmt, "0.0")

  path2 <- withr::local_tempfile(fileext = ".xlsx")
  table_regression(
    fit,
    show_columns = .dfc_cols,
    output = "excel",
    excel_path = path2
  )
  wt2 <- cell(path2, "wt")
  expect_false(wt2$type %in% c("inlineStr", "str", "s"))
  expect_equal(wt2$value, 28)
  # "0" is Excel's built-in format 1.
  expect_true(wt2$fmt %in% c("0", "1"))
})

test_that("the stacked layout and table_stack() carry each panel's df", {
  skip_if_not_installed("clubSandwich")
  fa <- lm(mpg ~ wt, data = mtcars)
  fb <- lm(mpg ~ wt + hp, data = mtcars)
  st <- table_regression(
    list(A = fa, B = fb),
    layout = "stacked",
    show_columns = .dfc_cols
  )
  # The typed view of a stack is the list of its panels' views.
  panels <- as_structured(st)
  expect_equal(
    unlist(lapply(unname(panels), function(p) {
      p$body$df[p$body[[".row_role"]] %in% "coef"]
    })),
    c(rep(df.residual(fa), 2L), rep(df.residual(fb), 3L))
  )
  out_st <- paste(capture.output(print(st)), collapse = "\n")
  expect_match(out_st, " 30 ", fixed = TRUE)
  expect_match(out_st, " 29 ", fixed = TRUE)
  # A stack of a z-test model alone is refused, as the side-by-side one.
  g <- glm(am ~ wt, data = mtcars, family = binomial)
  expect_error(
    table_regression(
      list(A = g, B = update(g, . ~ . + hp)),
      layout = "stacked",
      show_columns = .dfc_cols
    ),
    class = "spicy_invalid_input"
  )
  # table_stack(): a fractional panel beside a whole one.
  ta <- table_regression(
    fa,
    show_columns = .dfc_cols,
    vcov = "CR2",
    cluster = ~gear
  )
  tb <- table_regression(fb, show_columns = .dfc_cols)
  ts <- table_stack(A = ta, B = tb)
  out <- paste(capture.output(print(ts)), collapse = "\n")
  oracle <- clubSandwich::coef_test(
    fa,
    vcov = "CR2",
    cluster = mtcars$gear,
    test = "Satterthwaite"
  )$df_Satt
  expect_match(out, sprintf("%.1f", oracle[[2L]]), fixed = TRUE)
  # The rich engines lay out the stack's own view: each panel's cells
  # keep their precision.
  if (requireNamespace("tinytable", quietly = TRUE)) {
    tt <- table_stack(A = ta, B = tb, output = "tinytable")
    cells <- trimws(gsub("\u2007", " ", tt@data$df, fixed = TRUE))
    expect_true(all(c(sprintf("%.1f", oracle), "29") %in% cells))
  }
  if (requireNamespace("openxlsx2", quietly = TRUE)) {
    path <- withr::local_tempfile(fileext = ".xlsx")
    table_stack(A = ta, B = tb, output = "excel", excel_path = path)
    m <- as.matrix(openxlsx2::wb_to_df(path, col_names = FALSE))
    vals <- suppressWarnings(as.numeric(m))
    expect_true(any(abs(vals - oracle[[2L]]) < 1e-6, na.rm = TRUE))
  }
})

test_that("the French table heads the column ddl, with a decimal comma", {
  skip_if_not_installed("clubSandwich")
  withr::local_options(spicy.language = "fr")
  fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
  tb <- table_regression(
    fit,
    show_columns = .dfc_cols,
    vcov = "CR2",
    cluster = ~gear
  )
  out <- paste(capture.output(print(tb)), collapse = "\n")
  expect_match(out, "ddl", fixed = TRUE)
  expect_match(out, "1,5", fixed = TRUE)
  # The column keeps its English name in the data frame.
  expect_true("df" %in% names(as.data.frame(tb)))
  expect_identical(as_structured(tb)$col_meta$df$display_label, "ddl")
})
