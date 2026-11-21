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
    "Linear regression models. One model per outcome, fitted separately. Adjusted for age, sex, and education.",
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
    "Linear regression of each outcome on Employment status",
    fixed = TRUE
  )
  expect_match(out, "Body mass index:", fixed = TRUE)
  expect_match(out, "WHO-5 wellbeing index (0-100):", fixed = TRUE)
  plain <- .oc_print(table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = income_k
  ))
  expect_match(
    plain,
    "Linear regression of each outcome on income_k",
    fixed = TRUE
  )
  # The model of one kind is named as the title of one such model names
  # it; models of several kinds are not.
  logit <- .oc_print(table_regression_outcomes(
    d,
    outcomes = c(smoking, dentist_12m),
    exposure = income_k,
    method = "glm"
  ))
  expect_match(
    logit,
    "^Logistic regression of each outcome on income_k"
  )
  mixed <- .oc_print(table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, smoking),
    exposure = income_k,
    family = list(smoking = stats::binomial())
  ))
  expect_match(mixed, "^Regression of each outcome on income_k")
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
    "Linear regression of each outcome on employment",
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
    "Régression linéaire de chaque variable dépendante sur income_k",
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


test_that("tidy() and the long output flag the exponentiated blocks", {
  d <- .oc_soc()
  args <- list(
    d,
    outcomes = quote(c(wellbeing_score, smoking)),
    exposure = quote(sex),
    family = list(smoking = stats::binomial()),
    exponentiate = TRUE,
    show_intercept = TRUE
  )
  tbl <- do.call(table_regression_outcomes, args)
  td <- broom::tidy(tbl)
  expect_identical(utils::tail(names(td), 1L), "exponentiated")
  expect_identical(td$exponentiated, td$outcome == "smoking")
  lo <- do.call(table_regression_outcomes, c(args, output = "long"))
  expect_identical(utils::tail(names(lo), 1L), "exponentiated")
  expect_identical(
    lo$exponentiated,
    lo$outcome == "smoking" & !lo$is_reference
  )
})

test_that("glance() gives one row per outcome, each its own model's", {
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi, smoking),
    exposure = employment_status,
    covariates = c(age, sex),
    family = list(smoking = stats::binomial())
  )
  gl <- broom::glance(tbl)
  expect_identical(gl$outcome, c("wellbeing_score", "bmi", "smoking"))
  expect_identical(unique(gl$model_id), "M1")
  f <- stats::lm(bmi ~ employment_status + age + sex, data = d)
  g <- stats::glm(smoking ~ employment_status + age + sex, binomial, data = d)
  expect_identical(gl$nobs[2:3], c(stats::nobs(f), stats::nobs(g)))
  expect_equal(gl$r.squared[2L], summary(f)$r.squared, tolerance = 1e-12)
  expect_true(is.na(gl$r.squared[3L]))
  expect_equal(gl$AIC[3L], stats::AIC(g), tolerance = 1e-10)
  expect_equal(gl$logLik[2L], as.numeric(stats::logLik(f)), tolerance = 1e-10)
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

test_that("the model of each outcome follows method and family as typed", {
  d <- .oc_soc()
  coef_of <- function(f) unname(stats::coef(f)[["income_k"]])
  # method = "glm": every outcome by a logistic model.
  g <- table_regression_outcomes(
    d,
    c(smoking, dentist_12m),
    income_k,
    method = "glm"
  )
  expect_equal(
    broom::tidy(g)$estimate,
    c(
      coef_of(stats::glm(smoking ~ income_k, binomial, data = d)),
      coef_of(stats::glm(dentist_12m ~ income_k, binomial, data = d))
    ),
    tolerance = 1e-8
  )
  # A family alone: every outcome by glm() with it.
  p <- table_regression_outcomes(
    d,
    c(life_sat_health, life_sat_work),
    income_k,
    family = stats::poisson()
  )
  expect_equal(
    broom::tidy(p)$estimate[2L],
    coef_of(stats::glm(life_sat_work ~ income_k, poisson, data = d)),
    tolerance = 1e-8
  )
  # Its name, as glm() takes it.
  p2 <- table_regression_outcomes(
    d,
    c(life_sat_health, life_sat_work),
    income_k,
    family = "poisson"
  )
  expect_equal(broom::tidy(p2)$estimate, broom::tidy(p)$estimate)
  # A named list: the named outcomes by glm(), the others by lm().
  m <- table_regression_outcomes(
    d,
    c(wellbeing_score, smoking, life_sat_work),
    income_k,
    family = list(smoking = stats::binomial(), life_sat_work = "poisson")
  )
  expect_equal(
    broom::tidy(m)$estimate,
    c(
      coef_of(stats::lm(wellbeing_score ~ income_k, data = d)),
      coef_of(stats::glm(smoking ~ income_k, binomial, data = d)),
      coef_of(stats::glm(life_sat_work ~ income_k, poisson, data = d))
    ),
    tolerance = 1e-8
  )
  # The z of a glm and the t of an lm keep their own reference law.
  td <- broom::tidy(m)
  expect_equal(
    td$p.value,
    stats::p.adjust(
      c(
        summary(stats::lm(wellbeing_score ~ income_k, data = d))$coefficients[
          "income_k",
          4L
        ],
        summary(stats::glm(
          smoking ~ income_k,
          binomial,
          data = d
        ))$coefficients[
          "income_k",
          4L
        ],
        summary(stats::glm(
          life_sat_work ~ income_k,
          poisson,
          data = d
        ))$coefficients[
          "income_k",
          4L
        ]
      ),
      "holm"
    ),
    tolerance = 1e-10
  )
})

test_that("a binary outcome of a linear model is a linear probability model", {
  d <- .oc_soc()
  expect_warning(
    lpm <- table_regression_outcomes(d, c(wellbeing_score, smoking), income_k),
    "linear probability model for P(smoking = Yes)",
    fixed = TRUE,
    class = "spicy_model_choice"
  )
  y <- as.integer(d$smoking == "Yes")
  expect_equal(
    broom::tidy(lpm)$estimate[2L],
    unname(stats::coef(stats::lm(y ~ d$income_k))[[2L]]),
    tolerance = 1e-10
  )
  # A logical outcome is one too, and each binary outcome is named.
  w <- character(0)
  withCallingHandlers(
    table_regression_outcomes(
      d,
      c(info_doctor, physical_activity),
      income_k
    ),
    spicy_model_choice = function(c) {
      w <<- c(w, conditionMessage(c))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(w, 2L)
  expect_match(w[[1L]], "P(info_doctor = TRUE)", fixed = TRUE)
  expect_match(
    w[[2L]],
    "family = list(physical_activity = binomial())",
    fixed = TRUE
  )
  # `method = "lm"` typed is a choice: no warning.
  expect_no_warning(
    table_regression_outcomes(
      d,
      c(wellbeing_score, smoking),
      income_k,
      method = "lm"
    )
  )
  expect_error(
    table_regression_outcomes(d, c(wellbeing_score, education), income_k),
    "`education` has 3 observed levels.",
    fixed = TRUE,
    class = "spicy_invalid_data"
  )
})

test_that("method and family are refused where they contradict", {
  d <- .oc_soc()
  oc <- function(...) {
    table_regression_outcomes(d, c(wellbeing_score, smoking), income_k, ...)
  }
  expect_error(
    oc(method = "glm", family = list(smoking = stats::binomial())),
    "A list of families and `method` cannot be combined.",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(family = list(smokin = stats::binomial())),
    "`family` names `smokin`, not among the outcomes.",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(family = list(stats::binomial())),
    "must name each family by its outcome",
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(family = list(smoking = "binomial", smoking = "poisson")),
    "must name each family by its outcome",
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(family = list(smoking = 3)),
    "must be a stats::family object",
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(family = list(wellbeing_score = stats::gaussian())),
    "Leave `wellbeing_score` out of `family`",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(family = stats::gaussian()),
    "`lm` by another name",
    class = "spicy_invalid_input"
  )
  expect_error(
    oc(method = "lm", family = stats::binomial()),
    "`family` is not meaningful for `method = \"lm\"`.",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
  expect_warning(
    oc(method = "lm", family = stats::gaussian()),
    class = "spicy_ignored_arg"
  )
  expect_error(oc(method = "probit"), class = "spicy_invalid_input")
  # The error names the function the user called.
  e <- expect_error(oc(family = list(x = "poisson")))
  expect_identical(
    as.character(conditionCall(e)[[1L]]),
    "table_regression_outcomes"
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


# ---- 9. Scales of the blocks ----------------------------------------------

test_that("exponentiate = TRUE exponentiates the glm blocks alone", {
  d <- .oc_soc()
  expect_no_warning(
    tbl <- table_regression_outcomes(
      d,
      c(wellbeing_score, bmi, smoking),
      income_k,
      covariates = c(age, sex),
      family = list(smoking = stats::binomial()),
      exponentiate = TRUE
    )
  )
  td <- broom::tidy(tbl)
  g <- stats::glm(smoking ~ income_k + age + sex, binomial, data = d)
  f <- stats::lm(bmi ~ income_k + age + sex, data = d)
  expect_equal(td$estimate[3L], exp(unname(stats::coef(g)[["income_k"]])))
  expect_equal(
    td$conf.low[3L],
    exp(unname(stats::confint.default(g)["income_k", 1L])),
    tolerance = 1e-8
  )
  expect_equal(td$estimate[2L], unname(stats::coef(f)[["income_k"]]))
  out <- .oc_print(tbl)
  # The column has no one measure: each outcome names its own.
  expect_match(out, "Estimate", fixed = TRUE)
  expect_match(out, "smoking (OR)", fixed = TRUE)
  expect_match(out, "bmi (B)", fixed = TRUE)
  note <- .oc_note(tbl)
  expect_match(
    note,
    "wellbeing_score, bmi: linear regression; smoking: logistic regression.",
    fixed = TRUE
  )
  expect_match(
    note,
    "smoking: coefficients exponentiated and displayed as OR",
    fixed = TRUE
  )
  expect_match(
    note,
    "Std. errors: wellbeing_score, bmi: classical (OLS); smoking: classical (Fisher information).",
    fixed = TRUE
  )
  # The models are named by their labels, without the measure.
  lab <- .oc_note(table_regression_outcomes(
    d,
    c(wellbeing_score, smoking),
    income_k,
    family = list(smoking = stats::binomial()),
    exponentiate = TRUE,
    labels = TRUE
  ))
  expect_match(lab, "Current smoker: logistic regression.", fixed = TRUE)
  # The note does not list the fit statistics the table does not show.
  expect_no_match(note, "AIC", fixed = TRUE)
})

test_that("blocks on one scale share its header", {
  d <- .oc_soc()
  or <- table_regression_outcomes(
    d,
    c(smoking, dentist_12m),
    employment_status,
    method = "glm",
    exponentiate = TRUE
  )
  out <- .oc_print(or)
  expect_match(out, " OR ", fixed = TRUE)
  expect_no_match(out, "(OR)", fixed = TRUE)
  expect_match(out, "smoking:", fixed = TRUE)
  expect_match(.oc_note(or), "Logistic regression models.", fixed = TRUE)
  # Unexponentiated, a logit and a linear model are both on the B scale.
  b <- .oc_print(table_regression_outcomes(
    d,
    c(wellbeing_score, smoking),
    income_k,
    family = list(smoking = stats::binomial())
  ))
  expect_no_match(b, "Estimate", fixed = TRUE)
  expect_no_match(b, "(B)", fixed = TRUE)
  # OR and IRR differ: the header is Estimate, each names its measure, and
  # the note glosses IRR.
  mix <- table_regression_outcomes(
    d,
    c(smoking, life_sat_work),
    income_k,
    family = list(smoking = stats::binomial(), life_sat_work = "poisson"),
    exponentiate = TRUE
  )
  out <- .oc_print(mix)
  expect_match(out, "life_sat_work (IRR)", fixed = TRUE)
  expect_match(.oc_note(mix), "IRR = incidence rate ratio", fixed = TRUE)
  # exponentiate = TRUE on linear models alone has no effect, and says so.
  expect_warning(
    table_regression_outcomes(
      d,
      c(wellbeing_score, bmi),
      income_k,
      exponentiate = TRUE
    ),
    class = "spicy_ignored_arg"
  )
})

test_that("the French table names the measure of each outcome", {
  d <- .oc_soc()
  withr::local_options(spicy.language = "fr")
  out <- .oc_print(table_regression_outcomes(
    d,
    c(wellbeing_score, smoking),
    income_k,
    family = list(smoking = stats::binomial()),
    exponentiate = TRUE
  ))
  expect_match(out, "Estimation", fixed = TRUE)
  expect_match(out, "smoking (OR)", fixed = TRUE)
})

test_that("the t column is refused when linear models and glm mix", {
  d <- .oc_soc()
  expect_error(
    table_regression_outcomes(
      d,
      c(wellbeing_score, smoking),
      income_k,
      family = list(smoking = stats::binomial()),
      show_columns = c("b", "t", "p")
    ),
    "mix t and z statistics",
    class = "spicy_invalid_input"
  )
  # One kind of model keeps it.
  tbl <- table_regression_outcomes(
    d,
    c(smoking, dentist_12m),
    income_k,
    method = "glm",
    show_columns = c("b", "t", "p")
  )
  expect_match(.oc_print(tbl), " z ", fixed = TRUE)
})


test_that("the partial tests of a factor exposure sit on the header of each block", {
  d <- .oc_soc()
  df <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = employment_status,
    show_columns = c("n", "b", "p", "partial_f", "partial_eta2"),
    output = "data.frame"
  )
  hdr <- df$.row_role == "factor_header"
  f_col <- grep("^F", names(df), value = TRUE)[1L]
  a <- car::Anova(stats::lm(bmi ~ employment_status, data = d), type = 2)
  expect_equal(
    df[[f_col]][hdr][2L],
    a[["F value"]][1L],
    tolerance = 1e-8
  )
  expect_true(all(is.na(df[[f_col]][!hdr])))
  out <- .oc_print(table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, smoking),
    exposure = employment_status,
    family = list(smoking = stats::binomial()),
    show_columns = c("n", "b", "p", "partial_f", "partial_chi2")
  ))
  expect_match(out, "smoking:[^\n]*[(]3[)]")
})

test_that("each partial test enters its family once", {
  # The partial F of a factor is one test per model: two outcomes, two
  # tests, whatever the number of levels. The value repeated on the rows
  # of the levels in the data must not count once per level.
  d <- .oc_soc()
  tbl <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi),
    exposure = employment_status,
    show_columns = c("n", "b", "p", "partial_f")
  )
  td <- broom::tidy(tbl)
  raw <- vapply(
    c("wellbeing_score", "bmi"),
    function(y) {
      a <- car::Anova(
        stats::lm(stats::reformulate("employment_status", y), data = d),
        type = 2
      )
      a[["Pr(>F)"]][1L]
    },
    numeric(1)
  )
  pf <- td[td$estimate_type == "partial_f", , drop = FALSE]
  for (y in names(raw)) {
    expect_equal(
      unique(pf$p.value[pf$outcome == y]),
      stats::p.adjust(raw, "holm")[[y]],
      tolerance = 1e-10
    )
  }
})

test_that("stacked tables of outcomes keep their own families in the note", {
  d <- .oc_soc()
  a <- table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi, life_sat_health),
    exposure = income_k,
    covariates = c(age, sex)
  )
  b <- table_regression_outcomes(
    d,
    outcomes = c(smoking, dentist_12m),
    exposure = employment_status,
    method = "glm",
    p_adjust = "BH"
  )
  s <- table_stack(a, b)
  out <- .oc_print(s)
  # A table of outcomes is no panel of one outcome: the panels take their
  # number, not the name of their first outcome.
  expect_match(out, "Panel 1:", fixed = TRUE)
  expect_match(out, "Panel 2:", fixed = TRUE)
  expect_true(is.na(attr(a, "outcome")))
  note <- attr(s, "note", exact = TRUE)
  expect_match(
    note,
    "Panel 1: P-values adjusted across the 3 outcomes for 3 tests (Holm method).",
    fixed = TRUE
  )
  expect_match(
    note,
    "Panel 2: P-values adjusted across the 2 outcomes for 6 tests (Benjamini-Hochberg method).",
    fixed = TRUE
  )
  # The stack keeps the header of their first column.
  expect_match(out, "(^|\n) Outcome +│")
})


# ---- 10. Engines and snapshots --------------------------------------------

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

test_that("every engine heads the first column Outcome", {
  d <- .oc_soc()
  oc <- function(o, ...) {
    table_regression_outcomes(
      d,
      outcomes = c(wellbeing_score, bmi),
      exposure = employment_status,
      output = o,
      ...
    )
  }
  console <- .oc_print(oc("default"))
  expect_match(console, "\n Outcome  ", fixed = TRUE)
  expect_no_match(console, "Variable", fixed = TRUE)
  # The table of one model keeps its header.
  expect_match(
    .oc_print(table_regression(stats::lm(bmi ~ sex, data = d))),
    "Variable",
    fixed = TRUE
  )
  clip <- spicy:::clipboard_payload(oc("default"), "\t")
  expect_match(clip, "\nOutcome\t", fixed = TRUE)
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  skip_if_not_installed("openxlsx2")
  # Without an interval the header is one row; with one, the label sits
  # on the row above the bounds.
  for (sc in list(c("n", "b", "p"), c("n", "b", "ci", "p"))) {
    md <- tinytable::save_tt(oc("tinytable", show_columns = sc), "markdown")
    expect_match(md, "| Outcome ", fixed = TRUE)
    html <- as.character(gt::as_raw_html(oc("gt", show_columns = sc)))
    expect_match(html, ">Outcome<", fixed = TRUE)
    chunks <- flextable::information_data_chunk(
      oc("flextable", show_columns = sc)
    )
    hdr <- chunks$txt[chunks$.part == "header"]
    expect_true("Outcome" %in% hdr)
    expect_false("Variable" %in% hdr)
  }
  path <- withr::local_tempfile(fileext = ".xlsx")
  oc("excel", excel_path = path)
  wb <- openxlsx2::read_xlsx(path, col_names = FALSE, skip_empty_rows = TRUE)
  expect_true("Outcome" %in% unlist(wb[, 1L]))
  # In French, the key that names the outcome rows.
  withr::local_options(spicy.language = "fr")
  expect_match(.oc_print(oc("default")), "Variable dépendante", fixed = TRUE)
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

test_that("console snapshot: scales mixed under exponentiate = TRUE", {
  skip_on_cran()
  d <- .oc_soc()
  withr::local_options(width = 100)
  expect_snapshot(print(table_regression_outcomes(
    d,
    outcomes = c(wellbeing_score, bmi, smoking),
    exposure = income_k,
    covariates = c(age, sex),
    family = list(smoking = stats::binomial()),
    exponentiate = TRUE
  )))
})
