# table_regression_outcomes(): one exposure, one linear model per
# outcome, the exposure rows of every model in one table.

.oc_soc <- function() {
  d <- spicy::sochealth
  d$income_k <- d$income / 1000
  d
}

.oc_note <- function(tbl) attr(tbl, "note", exact = TRUE)

.oc_print <- function(tbl) paste(capture.output(print(tbl)), collapse = "\n")

# The exposure rows of the lm() fits, one per outcome, and Holm across
# them all: the oracle of the table.
.oc_oracle <- function(d, outcomes, rhs, expo_terms, method = "holm") {
  rows <- do.call(
    rbind,
    lapply(outcomes, function(y) {
      f <- stats::lm(stats::reformulate(rhs, response = y), data = d)
      s <- summary(f)$coefficients
      ci <- stats::confint(f)
      data.frame(
        outcome = y,
        term = expo_terms,
        estimate = unname(s[expo_terms, 1L]),
        se = unname(s[expo_terms, 2L]),
        low = unname(ci[expo_terms, 1L]),
        high = unname(ci[expo_terms, 2L]),
        p_raw = unname(s[expo_terms, 4L]),
        n = stats::nobs(f),
        stringsAsFactors = FALSE
      )
    })
  )
  rows$p <- stats::p.adjust(rows$p_raw, method = method)
  rows
}


# ---- 1. The numbers, model by model ---------------------------------------

test_that("a numeric exposure gives each outcome's own lm() coefficient", {
  d <- .oc_soc()
  outs <- c("wellbeing_score", "bmi", "life_sat_health")
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi, life_sat_health),
    exposure = income_k,
    covariates = c(age, sex)
  )
  td <- broom::tidy(tbl)
  or <- .oc_oracle(d, outs, c("income_k", "age", "sex"), "income_k")
  expect_identical(td$outcome, outs)
  expect_identical(td$term, rep("income_k", 3L))
  expect_true(all(is.na(td$factor_term)))
  expect_equal(td$estimate, or$estimate, tolerance = 1e-10)
  expect_equal(td$std.error, or$se, tolerance = 1e-10)
  expect_equal(td$conf.low, or$low, tolerance = 1e-10)
  expect_equal(td$conf.high, or$high, tolerance = 1e-10)
  # The family is the three exposure rows, not each model's coefficients.
  expect_equal(td$p.value, or$p, tolerance = 1e-12)
  expect_true(any(td$p.value != or$p_raw))
})

test_that("a factor exposure adjusts its k x (L - 1) rows as one family", {
  d <- .oc_soc()
  outs <- c("wellbeing_score", "bmi")
  lv <- paste0("employment_status", c("Student", "Unemployed", "Inactive"))
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = employment_status,
    covariates = c(age, sex)
  )
  td <- broom::tidy(tbl)
  or <- .oc_oracle(d, outs, c("employment_status", "age", "sex"), lv)
  expect_identical(td$term, or$term)
  expect_identical(td$outcome, or$outcome)
  expect_identical(unique(td$factor_term), "employment_status")
  expect_identical(
    td$factor_level,
    rep(c("Student", "Unemployed", "Inactive"), 2L)
  )
  expect_equal(td$estimate, or$estimate, tolerance = 1e-10)
  expect_equal(td$p.value, or$p, tolerance = 1e-12)
  expect_match(
    .oc_note(tbl),
    "P-values adjusted across the 2 outcomes for 6 tests (Holm method).",
    fixed = TRUE
  )
})

test_that("any method of p.adjust() is taken, and none leaves the p-values raw", {
  d <- .oc_soc()
  outs <- c("wellbeing_score", "bmi")
  or <- .oc_oracle(d, outs, "income_k", "income_k", method = "BH")
  bh <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    p_adjust = "BH"
  )
  expect_equal(broom::tidy(bh)$p.value, or$p, tolerance = 1e-12)
  expect_match(.oc_note(bh), "(Benjamini-Hochberg method)", fixed = TRUE)
  raw <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    p_adjust = "none"
  )
  expect_equal(broom::tidy(raw)$p.value, or$p_raw, tolerance = 1e-12)
  expect_no_match(.oc_note(raw), "adjusted", fixed = TRUE)
})


# ---- 2. Sample sizes ------------------------------------------------------

test_that("each block reports its own N, and the note the range", {
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = employment_status
  )
  ca <- attr(tbl, "spicy_long")
  n <- ca$n_obs[!is.na(ca$n_obs)]
  expect_identical(
    n,
    as.numeric(c(
      stats::nobs(stats::lm(wellbeing_score ~ employment_status, data = d)),
      stats::nobs(stats::lm(bmi ~ employment_status, data = d))
    ))
  )
  # The data keep it on the first level of each block, not on its
  # reference.
  expect_identical(
    ca$term[!is.na(ca$n_obs)],
    rep("employment_statusStudent", 2L)
  )
  # The display shows it once, on the header that names the outcome, with
  # the R-squared; the level rows leave the cells empty.
  df <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = employment_status,
    show_columns = c("n", "b", "p", "r2"),
    output = "data.frame"
  )
  hdr <- df$.row_role == "factor_header"
  expect_identical(df$N[hdr], as.numeric(n))
  expect_true(all(is.na(df$N[!hdr])))
  expect_true(all(!is.na(df[[ncol(df)]][hdr])))
  expect_true(all(is.na(df[[ncol(df)]][!hdr])))
  expect_match(
    .oc_note(tbl),
    "Each model is fit on its own complete cases; N varies by outcome (1188–1200).",
    fixed = TRUE
  )
})

test_that("equal Ns need no sentence, and complete_cases gives the common N", {
  d <- .oc_soc()
  same <- table_regression_outcomes(
    d[!is.na(d$bmi), ],
    outcomes = c(wellbeing_score, bmi),
    exposure = age
  )
  expect_no_match(.oc_note(same), "complete cases", fixed = TRUE)
  cc <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    covariates = c(age, sex),
    complete_cases = TRUE
  )
  n_cc <- sum(stats::complete.cases(
    d[, c("wellbeing_score", "bmi", "income_k", "age", "sex")]
  ))
  expect_true(all(broom::tidy(cc)$term == "income_k"))
  expect_identical(
    attr(cc, "spicy_long")$n_obs,
    rep(as.numeric(n_cc), 2L)
  )
  expect_match(
    .oc_note(cc),
    sprintf("All models fit on the %d common complete cases.", n_cc),
    fixed = TRUE
  )
  # The models are fitted on those rows: the bmi model on the rows where
  # wellbeing_score is observed too.
  keep <- stats::complete.cases(d[, c(
    "wellbeing_score",
    "bmi",
    "income_k",
    "age",
    "sex"
  )])
  f <- stats::lm(bmi ~ income_k + age + sex, data = d[keep, ])
  expect_equal(
    broom::tidy(cc)$estimate[2L],
    unname(stats::coef(f)["income_k"]),
    tolerance = 1e-10
  )
})


# ---- 3. The note ----------------------------------------------------------

test_that("the note says the models are separate and names the covariates", {
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    covariates = c(age, sex, education)
  )
  note <- .oc_note(tbl)
  expect_match(
    note,
    "Linear regression. One model per outcome, fitted separately. Adjusted for age, sex, and education.",
    fixed = TRUE
  )
  plain <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k
  )
  expect_match(.oc_note(plain), "fitted separately.", fixed = TRUE)
  expect_no_match(.oc_note(plain), "Adjusted for", fixed = TRUE)
  # The family counts the exposure rows only, never a covariate.
  expect_match(note, "across the 2 outcomes for 2 tests", fixed = TRUE)
})

test_that("the covariates take their labels in the note", {
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    covariates = c(age, sex),
    labels = TRUE,
    note_labels = c(age = "age")
  )
  expect_match(.oc_note(tbl), "Adjusted for age and Sex.", fixed = TRUE)
})


# ---- 4. Layouts -----------------------------------------------------------

test_that("an ordered exposure keeps its trends, and names them once", {
  d <- .oc_soc()
  tbl <- suppressMessages(table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = education
  ))
  td <- broom::tidy(tbl)
  expect_identical(td$term, rep(c("education.L", "education.Q"), 2L))
  expect_identical(unique(td$factor_term), "education")
  f <- stats::lm(bmi ~ education, data = d)
  expect_equal(
    td$estimate[3:4],
    unname(stats::coef(f)[c("education.L", "education.Q")]),
    tolerance = 1e-10
  )
  expect_match(.oc_note(tbl), "Ordered factor `education`:", fixed = TRUE)
})

test_that("a logical exposure is a block with its reference", {
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = info_doctor
  )
  out <- .oc_print(tbl)
  expect_match(out, "wellbeing_score:", fixed = TRUE)
  expect_match(out, "FALSE (ref.)", fixed = TRUE)
  expect_identical(broom::tidy(tbl)$term, rep("info_doctorTRUE", 2L))
})

test_that("the reference footer names the exposure once", {
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = employment_status,
    reference_style = "footer"
  )
  expect_match(
    .oc_note(tbl),
    "Reference categories: employment_status = Employed.",
    fixed = TRUE
  )
})

test_that("show_intercept = TRUE opens each block with its intercept", {
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    show_intercept = TRUE
  )
  td <- broom::tidy(tbl)
  expect_identical(
    td$term,
    rep(c("(Intercept)", "income_k"), 2L)
  )
  expect_identical(td$is_intercept, rep(c(TRUE, FALSE), 2L))
  f <- stats::lm(bmi ~ income_k, data = d)
  expect_equal(
    td$estimate[3L],
    unname(stats::coef(f)["(Intercept)"]),
    tolerance = 1e-10
  )
  # The intercepts are not tested against the exposure family.
  expect_match(.oc_note(tbl), "for 2 tests", fixed = TRUE)
  out <- .oc_print(tbl)
  expect_match(out, "bmi:", fixed = TRUE)
  lines <- strsplit(out, "\n", fixed = TRUE)[[1L]]
  expect_lt(
    grep("(Intercept)", lines, fixed = TRUE)[1L],
    grep("income_k  ", lines, fixed = TRUE)[1L]
  )
  # A factor exposure: the intercept, then the reference.
  fac <- .oc_print(table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = sex,
    show_intercept = TRUE
  ))
  lines <- strsplit(fac, "\n", fixed = TRUE)[[1L]]
  expect_lt(
    grep("(Intercept)", lines, fixed = TRUE)[1L],
    grep("Female (ref.)", lines, fixed = TRUE)[1L]
  )
})

test_that("r2 gives each outcome its own model's R-squared", {
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    show_columns = c("n", "b", "p", "r2")
  )
  r2 <- attr(tbl, "spicy_long")$r2
  expect_equal(
    r2,
    c(
      summary(stats::lm(wellbeing_score ~ income_k, data = d))$r.squared,
      summary(stats::lm(bmi ~ income_k, data = d))$r.squared
    ),
    tolerance = 1e-10
  )
})


# ---- 5. Labels and titles -------------------------------------------------

test_that("the title names the exposure, and labels the rows", {
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = employment_status,
    labels = TRUE
  )
  out <- .oc_print(tbl)
  expect_match(
    out,
    "Regression of each outcome on Employment status",
    fixed = TRUE
  )
  expect_match(out, "Body mass index:", fixed = TRUE)
  expect_match(out, "WHO-5 wellbeing index (0-100):", fixed = TRUE)
  plain <- .oc_print(table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k
  ))
  expect_match(plain, "Regression of each outcome on income_k", fixed = TRUE)
  typed <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    title = "Table 2"
  )
  expect_match(.oc_print(typed), "^Table 2")
  expect_true(attr(typed, "spicy_title_typed"))
  expect_false(attr(tbl, "spicy_title_typed"))
})

test_that("a named label reaches an outcome, and a level in every block", {
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = employment_status,
    labels = c(bmi = "BMI", employment_statusStudent = "In education")
  )
  out <- .oc_print(tbl)
  expect_match(out, "BMI:", fixed = TRUE)
  expect_identical(lengths(regmatches(out, gregexpr("In education", out))), 2L)
  # A numeric exposure with the intercepts: its row in each block.
  num <- .oc_print(table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    show_intercept = TRUE,
    labels = c(income_k = "Income (kCHF)", wellbeing_score = "Wellbeing")
  ))
  expect_identical(
    lengths(regmatches(num, gregexpr("Income (kCHF)", num, fixed = TRUE))),
    3L
  )
  expect_match(num, "Wellbeing:", fixed = TRUE)
  # A flat row is the outcome's.
  flat <- .oc_print(table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    labels = c(bmi = "BMI")
  ))
  expect_match(flat, "\n BMI ", fixed = TRUE)
})

test_that("note_labels gives the exposure its label in the title", {
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = employment_status,
    labels = TRUE,
    note_labels = c(employment_status = "employment")
  )
  expect_match(
    .oc_print(tbl),
    "Regression of each outcome on employment",
    fixed = TRUE
  )
  expect_warning(
    table_regression_outcomes(
      d,
      outcomes = c(wellbeing_score, bmi),
      exposure = income_k,
      note_labels = c(nope = "x")
    ),
    class = "spicy_unknown_name"
  )
})

test_that("the French table translates its title and its note", {
  d <- .oc_soc()
  withr::local_options(spicy.language = "fr")
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    covariates = c(age, sex)
  )
  out <- .oc_print(tbl)
  expect_match(
    out,
    "Régression de chaque variable dépendante sur income_k",
    fixed = TRUE
  )
  note <- .oc_note(tbl)
  expect_match(note, spicy_str("note_outcomes_separate"), fixed = TRUE)
  expect_match(note, "Ajusté sur age et sex.", fixed = TRUE)
  expect_match(
    note,
    "N varie selon la variable dépendante (1188–1200).",
    fixed = TRUE
  )
  expect_match(
    note,
    "Valeurs p ajustées sur les 2 variables dépendantes pour 2 tests (méthode de Holm).",
    fixed = TRUE
  )
})


# ---- 6. Data outputs ------------------------------------------------------

test_that("output = 'long' names the rows as tidy() does, references kept", {
  d <- .oc_soc()
  lo <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = employment_status,
    output = "long"
  )
  expect_identical(
    lo$term,
    rep(paste0("employment_status", levels(d$employment_status)), 2L)
  )
  expect_identical(lo$outcome, rep(c("wellbeing_score", "bmi"), each = 4L))
  expect_identical(lo$factor_term, rep("employment_status", 8L))
  df <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    output = "data.frame"
  )
  expect_identical(df$variable, c("wellbeing_score", "bmi"))
})

test_that("inline() cites an outcome's row, and a level of its block", {
  d <- .oc_soc()
  num <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k
  )
  b <- unname(stats::coef(stats::lm(bmi ~ income_k, data = d))["income_k"])
  expect_identical(inline(num, "bmi"), sprintf("%.2f", b))
  fac <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = employment_status
  )
  b <- unname(stats::coef(
    stats::lm(bmi ~ employment_status, data = d)
  )["employment_statusStudent"])
  expect_identical(inline(fac, "bmi", level = "Student"), sprintf("%.2f", b))
})


# ---- 7. Variance and clusters ---------------------------------------------

test_that("a robust variance reaches every model", {
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    vcov = "HC3",
    p_adjust = "none"
  )
  f <- stats::lm(bmi ~ income_k, data = d)
  se <- sqrt(diag(sandwich::vcovHC(f, type = "HC3")))[["income_k"]]
  expect_equal(broom::tidy(tbl)$std.error[2L], se, tolerance = 1e-10)
  expect_match(.oc_note(tbl), "heteroskedasticity-robust (HC3)", fixed = TRUE)
})

test_that("cluster is aligned to each model's own estimation sample", {
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    vcov = "CR1",
    cluster = d$region
  )
  # bmi has missing values: its model takes the regions of its own rows,
  # as the one model of bmi does when given them.
  keep <- stats::complete.cases(d[, c("bmi", "income_k")])
  one <- table_regression(
    stats::lm(bmi ~ income_k, data = d),
    vcov = "CR1",
    cluster = d$region[keep]
  )
  td_one <- broom::tidy(one)
  expect_equal(
    broom::tidy(tbl)$std.error[2L],
    td_one$std.error[td_one$term == "income_k"],
    tolerance = 1e-12
  )
  expect_match(.oc_note(tbl), "clusters by region", fixed = TRUE)
  expect_error(
    table_regression_outcomes(
      d,
      outcomes = c(wellbeing_score, bmi),
      exposure = income_k,
      vcov = "CR1",
      cluster = d$region[-1L]
    ),
    "one value per row of `data` (1200), not 1199.",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_outcomes(
      d,
      outcomes = c(wellbeing_score, bmi),
      exposure = income_k,
      vcov = "CR1",
      cluster = list(d$region, d$region)
    ),
    "single vector for `table_regression_outcomes()`",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
})


# ---- 8. Refusals ----------------------------------------------------------

test_that("the arguments a table of outcomes has no use for are refused", {
  d <- .oc_soc()
  oc <- function(...) {
    table_regression_outcomes(
      d,
      outcomes = c(wellbeing_score, bmi),
      exposure = employment_status,
      ...
    )
  }
  expect_error(
    oc(nested = TRUE),
    "not meaningful for a table of outcomes",
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(layout = "stacked"),
    "`layout` does not apply",
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(keep = "Student"),
    "`keep` does not apply",
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(drop = "Student"),
    "`drop` does not apply",
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(factor_layout = "flat"),
    "factor_layout = \"flat\"",
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(outcome_labels = c("a", "b")),
    "`outcome_labels` does not apply",
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(standardized = "refit"),
    "`standardized` is not available",
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(show_columns = "none"),
    "show_columns = \"none\"",
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(show_columns = c("b", "ame")),
    "Average marginal effects",
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(expon = TRUE),
    "Did you mean `exponentiate`",
    class = "spicy_invalid_input"
  )
  # The error names the function the user called.
  e <- expect_error(oc(keep = "x"), class = "spicy_invalid_input")
  expect_identical(
    as.character(conditionCall(e)[[1L]]),
    "table_regression_outcomes"
  )
})

test_that("only linear models of numeric outcomes are fitted in this version", {
  d <- .oc_soc()
  expect_error(
    table_regression_outcomes(d, c(wellbeing_score, bmi), age, method = "glm"),
    "linear models only for now",
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_outcomes(
      d,
      c(wellbeing_score, bmi),
      age,
      family = stats::poisson()
    ),
    "linear models only for now",
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_outcomes(
      d,
      c(wellbeing_score, bmi),
      age,
      method = "probit"
    ),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_outcomes(d, c(wellbeing_score, smoking), age),
    "numeric outcomes only for now, not `smoking`.",
    fixed = TRUE,
    class = "spicy_invalid_data"
  )
  # method = "lm" typed is the default.
  expect_s3_class(
    table_regression_outcomes(d, c(wellbeing_score, bmi), age, method = "lm"),
    "spicy_regression_table"
  )
})

test_that("the exposure is no outcome or covariate, and two outcomes are needed", {
  d <- .oc_soc()
  expect_error(
    table_regression_outcomes(d, c(wellbeing_score, age), age),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_outcomes(
      d,
      c(wellbeing_score, bmi),
      age,
      covariates = c(age, sex)
    ),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_outcomes(
      d,
      c(wellbeing_score, bmi),
      age,
      covariates = c(bmi, sex)
    ),
    class = "spicy_invalid_input"
  )
  # A helper skips them.
  tbl <- table_regression_outcomes(
    d[, c("wellbeing_score", "bmi", "age")],
    outcomes = everything(),
    exposure = age
  )
  expect_identical(broom::tidy(tbl)$outcome, c("wellbeing_score", "bmi"))
  expect_error(
    table_regression_outcomes(d, bmi, age),
    "For the one model of `bmi`, use `table_regression()`.",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_outcomes(d, starts_with("zzz"), age),
    class = "spicy_invalid_input"
  )
})

test_that("a model that cannot be fitted is named by its outcome", {
  d <- .oc_soc()
  d$empty <- NA_real_
  expect_error(
    table_regression_outcomes(d, c(wellbeing_score, empty), age),
    "The model for the outcome `empty` failed to fit.",
    fixed = TRUE,
    class = "spicy_invalid_data"
  )
})

test_that("a survey design is refused", {
  skip_if_not_installed("survey")
  d <- .oc_soc()
  des <- survey::svydesign(ids = ~1, weights = ~weight, data = d)
  expect_error(
    table_regression_outcomes(des, c(wellbeing_score, bmi), age),
    class = "spicy_wrong_regime"
  )
})


# ---- 9. Engines and snapshots ---------------------------------------------

test_that("the rich engines render the blocks", {
  d <- .oc_soc()
  oc <- function(o) {
    table_regression_outcomes(
      d,
      outcomes = c(wellbeing_score, bmi),
      exposure = employment_status,
      output = o
    )
  }
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  tt <- oc("tinytable")
  expect_s4_class(tt, "tinytable")
  expect_true(any(grepl("bmi", unlist(tt@data), fixed = TRUE)))
  g <- oc("gt")
  expect_s3_class(g, "gt_tbl")
  expect_identical(
    as.character(g[["_data"]]$Variable)[c(1L, 6L)],
    c("wellbeing_score:", "bmi:")
  )
  ft <- oc("flextable")
  expect_s3_class(ft, "flextable")
  expect_identical(
    as.character(ft$body$dataset$Variable)[c(1L, 6L)],
    c("wellbeing_score:", "bmi:")
  )
})

test_that("console snapshot: a numeric exposure, one row per outcome", {
  skip_on_cran()
  d <- .oc_soc()
  withr::local_options(width = 100)
  expect_snapshot(print(table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi, life_sat_health),
    exposure = income_k,
    covariates = c(age, sex)
  )))
})

test_that("console snapshot: a factor exposure, one block per outcome", {
  skip_on_cran()
  d <- .oc_soc()
  withr::local_options(width = 100)
  expect_snapshot(print(table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = employment_status,
    covariates = c(age, sex)
  )))
})

test_that("console snapshot: the intercepts shown", {
  skip_on_cran()
  d <- .oc_soc()
  withr::local_options(width = 100)
  expect_snapshot(print(table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k,
    show_intercept = TRUE
  )))
})
