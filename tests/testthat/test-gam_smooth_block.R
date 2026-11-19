# The smooth terms of a GAM as a "Smooth terms (edf)" block
# (dev/decisions/2026-11-19-gam-smooth-terms-block.md): one row per term,
# the edf in the B column, the p-value of mgcv's approximate test, the
# statistic and Ref.df in tidy() and output = "long".

gam_pima <- function() {
  rbind(MASS::Pima.tr, MASS::Pima.te)
}

# The name of each row of output = "data.frame": its level inside a
# block or a factor, its label otherwise.
row_names <- function(tb) {
  d <- as.data.frame(tb)
  ifelse(is.na(d$level), d$label, d$level)
}

# The four numbers of every smooth row against summary(fit)$s.table.
expect_smooth_oracle <- function(fit) {
  st <- summary(fit)$s.table
  td <- broom::tidy(table_regression(fit))
  sm <- td[td$estimate_type == "edf", , drop = FALSE]
  expect_identical(sm$term, rownames(st))
  expect_identical(unique(sm$factor_term), "Smooth terms")
  expect_equal(sm$estimate, unname(st[, "edf"]), tolerance = 1e-10)
  expect_equal(sm$df, unname(st[, "Ref.df"]), tolerance = 1e-10)
  expect_equal(sm$statistic, unname(st[, 3L]), tolerance = 1e-10)
  expect_equal(sm$p.value, unname(st[, "p-value"]), tolerance = 1e-10)
  expect_identical(
    unique(sm$test_type),
    if (colnames(st)[3L] == "F") "F" else "X2"
  )
  invisible(sm)
}

test_that("the smooth rows equal summary.gam() for each family and term type", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("broom")
  pima <- gam_pima()
  pima$agegrp <- cut(
    pima$age,
    c(20, 25, 30, 40, 50, 90),
    include.lowest = TRUE
  )
  # binomial (chi-square), the reported defect
  expect_smooth_oracle(mgcv::gam(
    type ~ s(age) + bmi + glu + npreg,
    family = binomial,
    data = pima,
    method = "REML"
  ))
  # gaussian (F)
  sm <- expect_smooth_oracle(mgcv::gam(
    glu ~ s(age) + bmi,
    data = pima,
    method = "REML"
  ))
  expect_identical(sm$test_type, "F")
  # Poisson and nb()
  expect_smooth_oracle(mgcv::gam(
    npreg ~ s(age) + bmi,
    family = poisson,
    data = pima,
    method = "REML"
  ))
  expect_smooth_oracle(mgcv::gam(
    npreg ~ s(age) + bmi,
    family = mgcv::nb(),
    data = pima,
    method = "REML"
  ))
  # te(), by =, bs = "re", labeled as mgcv labels them
  sm <- expect_smooth_oracle(mgcv::gam(
    type ~ s(age) + s(glu) + te(bmi, ped),
    family = binomial,
    data = pima,
    method = "REML"
  ))
  expect_identical(sm$term, c("s(age)", "s(glu)", "te(bmi,ped)"))
  sm <- expect_smooth_oracle(mgcv::gam(
    glu ~ s(agegrp, bs = "re") + bmi + s(age, by = type) + type,
    data = pima,
    method = "REML"
  ))
  expect_identical(sm$term, c("s(agegrp)", "s(age):typeNo", "s(age):typeYes"))
  # bam()
  expect_smooth_oracle(mgcv::bam(glu ~ s(age) + bmi, data = pima))
})

test_that("a GAM without smooth terms has no block, row or note", {
  skip_if_not_installed("mgcv")
  pima <- gam_pima()
  fit <- mgcv::gam(type ~ age + bmi, family = binomial, data = pima)
  tb <- table_regression(fit)
  long <- table_regression(fit, output = "long")
  expect_false("edf" %in% long$estimate_type)
  out <- paste(capture.output(print(tb)), collapse = "\n")
  expect_false(grepl("Smooth terms", out, fixed = TRUE))
  expect_identical(
    row_names(tb),
    c(
      "(Intercept)",
      "age",
      "bmi",
      "n",
      "R\u00B2 (Tjur)",
      "R\u00B2 (McFadden)",
      "AIC"
    )
  )
})

test_that("the Pima GAM shows the age term it hid before", {
  skip_if_not_installed("mgcv")
  pima <- gam_pima()
  fit <- mgcv::gam(
    type ~ s(age) + bmi + glu + npreg,
    family = binomial,
    data = pima,
    method = "REML"
  )
  expect_snapshot(print(table_regression(fit)))
  expect_snapshot(print(table_regression(
    fit,
    show_columns = c("b", "se", "t", "p"),
    stars = TRUE
  )))
})

test_that("a model of smooth terms alone shows them, not only the constant", {
  skip_if_not_installed("mgcv")
  pima <- gam_pima()
  fit <- mgcv::gam(
    type ~ s(age) + s(glu) + te(bmi, ped),
    family = binomial,
    data = pima,
    method = "REML"
  )
  expect_identical(
    row_names(table_regression(fit))[1:5],
    c("(Intercept)", "Smooth terms (edf)", "s(age)", "s(glu)", "te(bmi,ped)")
  )
})

test_that("SE, CI and statistic cells are undefined, the edf is never starred or exponentiated", {
  skip_if_not_installed("mgcv")
  pima <- gam_pima()
  fit <- mgcv::gam(
    type ~ s(age) + bmi,
    family = binomial,
    data = pima,
    method = "REML"
  )
  edf <- summary(fit)$s.table[1L, "edf"]
  tb <- table_regression(
    fit,
    show_columns = c("b", "se", "ci", "t", "p"),
    stars = TRUE,
    exponentiate = TRUE
  )
  df <- as.data.frame(tb)
  row <- df[df$level %in% "s(age)", ]
  expect_equal(row$OR, edf, tolerance = 1e-10)
  s <- as_structured(tb)
  i <- which(s$body$Variable == "  s(age)" | s$body$Variable == "s(age)")
  expect_length(i, 1L)
  expect_identical(s$body$.row_role[i], "level")
  for (col in c("SE", "95% CI: LL", "95% CI: UL", "z")) {
    expect_identical(s$cell_status[[col]][i], "undefined")
  }
  expect_false(any(nzchar(s$stars$OR[i] %||% "")))
  out <- capture.output(print(tb))
  age_line <- out[grepl("s(age)", out, fixed = TRUE)]
  expect_false(grepl("*", age_line, fixed = TRUE))
})

test_that("a robust variance keeps the edf, drops the test and says why", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("broom")
  pima <- gam_pima()
  fit <- mgcv::gam(
    type ~ s(age) + bmi,
    family = binomial,
    data = pima,
    method = "REML"
  )
  tb <- table_regression(fit, vcov = "CR1", cluster = pima$npreg)
  td <- broom::tidy(tb)
  sm <- td[td$estimate_type == "edf", ]
  expect_equal(sm$estimate, summary(fit)$s.table[1L, "edf"], tolerance = 1e-10)
  expect_true(is.na(sm$statistic))
  expect_true(is.na(sm$p.value))
  out <- paste(capture.output(print(tb)), collapse = " ")
  expect_match(
    out,
    "The test of the smooth terms is not computed under a robust variance.",
    fixed = TRUE
  )
  # In a table where only one GAM is robust, the sentence names it.
  expect_warning(
    tb2 <- table_regression(
      list(fit, fit),
      vcov = list("classical", "CR1"),
      cluster = pima$npreg
    ),
    "cluster vector is ignored"
  )
  out2 <- paste(capture.output(print(tb2)), collapse = " ")
  expect_match(
    out2,
    "Model 2: The test of the smooth terms is not computed",
    fixed = TRUE
  )
})

test_that("keep and drop select smooth terms like coefficients", {
  skip_if_not_installed("mgcv")
  pima <- gam_pima()
  fit <- mgcv::gam(
    type ~ s(age) + s(glu) + bmi,
    family = binomial,
    data = pima,
    method = "REML"
  )
  kept <- row_names(table_regression(fit, keep = "bmi"))
  expect_false(any(grepl("^s\\(|Smooth", kept)))
  out <- paste(
    capture.output(print(table_regression(fit, drop = "^s\\("))),
    collapse = " "
  )
  expect_false(grepl("Smooth terms", out, fixed = TRUE))
  part <- row_names(table_regression(fit, drop = "glu"))
  expect_true("s(age)" %in% part)
  expect_false("s(glu)" %in% part)
  expect_true("Smooth terms (edf)" %in% part)
})

test_that("the smooth tests join the family of p_adjust", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("broom")
  pima <- gam_pima()
  fit <- mgcv::gam(
    type ~ s(age) + bmi + glu,
    family = binomial,
    data = pima,
    method = "REML"
  )
  sm <- summary(fit)
  raw <- c(
    sm$p.table[c("bmi", "glu"), "Pr(>|z|)"],
    sm$s.table[, "p-value"]
  )
  adj <- stats::p.adjust(raw, "holm")
  td <- broom::tidy(table_regression(fit, p_adjust = "holm"))
  got <- td$p.value[match(c("bmi", "glu", "s(age)"), td$term)]
  expect_equal(got, unname(adj), tolerance = 1e-12)
})

test_that("several models align the smooth rows by term", {
  skip_if_not_installed("mgcv")
  pima <- gam_pima()
  g0 <- stats::glm(type ~ age + bmi, family = binomial, data = pima)
  g1 <- mgcv::gam(type ~ s(age) + bmi, family = binomial, data = pima)
  g2 <- mgcv::gam(type ~ s(age) + s(glu), family = binomial, data = pima)
  long <- table_regression(list(g0, g1, g2), output = "long")
  sm <- long[long$estimate_type == "edf", ]
  expect_identical(sm$model_id, c("M2", "M3", "M3"))
  expect_identical(sm$term, c("s(age)", "s(age)", "s(glu)"))
  expect_identical(
    sum(row_names(table_regression(list(g0, g1, g2))) == "s(age)"),
    1L
  )
  # nested = TRUE: the change statistics stay those of the models.
  fa <- mgcv::gam(type ~ bmi, family = binomial, data = pima)
  tb <- table_regression(list(fa, g1), nested = TRUE)
  expect_true("s(age)" %in% row_names(tb))
})

test_that("the block reaches gt, tinytable, flextable and Excel", {
  skip_if_not_installed("mgcv")
  pima <- gam_pima()
  fit <- mgcv::gam(type ~ s(age) + bmi, family = binomial, data = pima)
  edf <- format_number(summary(fit)$s.table[1L, "edf"], 2L, ".")
  check <- function(df) {
    i <- which(trimws(df[[1L]]) == "s(age)")
    expect_length(i, 1L)
    expect_true("Smooth terms (edf):" %in% df[[1L]])
    # The engines pad their cells for alignment.
    expect_match(df[[2L]][i], edf, fixed = TRUE)
    expect_match(df[[3L]][i], spicy_str("cell_undefined"), fixed = TRUE)
  }
  skip_if_not_installed("gt")
  check(as.data.frame(table_regression(fit, output = "gt")[["_data"]]))
  skip_if_not_installed("tinytable")
  check(table_regression(fit, output = "tinytable")@data)
  skip_if_not_installed("flextable")
  check(table_regression(fit, output = "flextable")$body$dataset)
  skip_if_not_installed("openxlsx2")
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  table_regression(fit, output = "excel", excel_path = path)
  x <- openxlsx2::wb_to_df(
    openxlsx2::wb_load(path),
    sheet = 1,
    col_names = FALSE
  )
  i <- which(trimws(x[[1L]]) == "s(age)")
  expect_equal(
    as.numeric(x[[2L]][i]),
    summary(fit)$s.table[1L, "edf"],
    tolerance = 1e-10
  )
  expect_identical(x[[3L]][i], spicy_str("cell_undefined"))
  expect_true(any(grepl("Smooth terms: effective degrees", x[[1L]])))
})

test_that("the caption and the note are translated", {
  skip_if_not_installed("mgcv")
  pima <- gam_pima()
  fit <- mgcv::gam(type ~ s(age) + bmi, family = binomial, data = pima)
  withr::local_options(spicy.language = "fr")
  out <- paste(capture.output(print(table_regression(fit))), collapse = " ")
  expect_match(out, "Termes liss\u00E9s (ddl effectifs)", fixed = TRUE)
  expect_match(
    out,
    "degr\u00E9s de libert\u00E9 effectifs et test approch\u00E9",
    fixed = TRUE
  )
})

test_that("without the B column there is no block", {
  skip_if_not_installed("mgcv")
  pima <- gam_pima()
  fit <- mgcv::gam(type ~ s(age) + bmi, family = binomial, data = pima)
  long <- table_regression(fit, show_columns = "ame", output = "long")
  expect_false("edf" %in% long$estimate_type)
})
