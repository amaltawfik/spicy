# table_regression_uv(): the univariable screening table -- the
# gtsummary::tbl_uvregression + tbl_merge layout (spec:
# dev/uvregression_spec.md, validated 2026-07-09).
#
# Oracle provenance (constants pinned 2026-07-09): gtsummary 2.5.1,
#   tbl_uvregression(sochealth[, c("smoking", "age", "bmi", "sex")],
#     method = glm, y = smoking, method.args = list(family = binomial()),
#     exponentiate = TRUE, hide_n = FALSE)
# estimate and p.value agree with spicy to 12 decimals; N_obs matches
# (age 1175, bmi 1163, sex 1175). CIs are NOT cross-compared: gtsummary
# profiles the likelihood while spicy reports Wald intervals (the
# documented convention) -- they differ in the 5th decimal here.

.uv_soc <- function() {
  sochealth[, c("smoking", "age", "bmi", "sex", "region",
                "wellbeing_score")]
}


# ---- 1. Numeric oracle: per-predictor fits + pinned gtsummary ------------

test_that("univariable rows reproduce the per-predictor glm fits exactly", {
  d <- .uv_soc()
  t_uv <- table_regression_uv(d, outcome = smoking,
                              predictors = c(age, bmi, sex),
                              exponentiate = TRUE, multivariable = FALSE)
  td <- broom::tidy(t_uv)

  for (v in c("age", "bmi")) {
    f <- stats::glm(stats::reformulate(v, "smoking"),
                    family = stats::binomial(), data = d)
    sm <- summary(f)$coefficients
    expect_equal(td$estimate[td$term == v],
                 exp(unname(stats::coef(f)[v])), tolerance = 1e-12)
    # Wald CI: exp of the link-scale interval.
    ci <- exp(stats::confint.default(f)[v, ])
    expect_equal(td$conf.low[td$term == v],  unname(ci[1]), tolerance = 1e-9)
    expect_equal(td$conf.high[td$term == v], unname(ci[2]), tolerance = 1e-9)
    # SE on the exp scale is the delta-method OR-scale SE.
    expect_equal(td$std.error[td$term == v],
                 exp(unname(stats::coef(f)[v])) * unname(sm[v, 2]),
                 tolerance = 1e-9)
    expect_equal(td$p.value[td$term == v], unname(sm[v, 4]),
                 tolerance = 1e-12)
  }
  f_sex <- stats::glm(smoking ~ sex, family = stats::binomial(), data = d)
  expect_equal(td$estimate[td$term == "sexMale"],
               exp(unname(stats::coef(f_sex)["sexMale"])), tolerance = 1e-12)

  # gtsummary 2.5.1 pinned constants (provenance in the file header).
  expect_equal(td$estimate[td$term == "age"],     1.005119471382,
               tolerance = 1e-9)
  expect_equal(td$p.value[td$term == "age"],      0.291844336359,
               tolerance = 1e-9)
  expect_equal(td$estimate[td$term == "bmi"],     0.997710907231,
               tolerance = 1e-9)
  expect_equal(td$p.value[td$term == "bmi"],      0.905265160667,
               tolerance = 1e-9)
  expect_equal(td$estimate[td$term == "sexMale"], 0.948697550820,
               tolerance = 1e-9)
  expect_equal(td$p.value[td$term == "sexMale"],  0.712535321339,
               tolerance = 1e-9)
})


test_that("the multivariable column equals the full fit exactly", {
  d <- .uv_soc()
  t_uv <- table_regression_uv(d, outcome = smoking,
                              predictors = c(age, bmi, sex))
  td <- broom::tidy(t_uv)
  expect_setequal(unique(td$model_id), c("Univariable", "Multivariable"))

  f <- stats::glm(smoking ~ age + bmi + sex, family = stats::binomial(),
                  data = d)
  sm <- summary(f)$coefficients
  for (v in c("age", "bmi", "sexMale")) {
    row <- td$model_id == "Multivariable" & td$term == v
    expect_equal(td$estimate[row], unname(stats::coef(f)[v]),
                 tolerance = 1e-12)
    expect_equal(td$p.value[row], unname(sm[v, 4]), tolerance = 1e-12)
  }
})


# ---- 2. N column + sample-size disclosure --------------------------------

test_that("per-predictor N is shown and the differing-N note fires", {
  d <- .uv_soc()
  out <- paste(capture.output(print(
    table_regression_uv(d, outcome = smoking,
                        predictors = c(age, bmi, sex))
  )), collapse = "\n")
  # bmi has 12 missing values -> its own complete cases differ.
  expect_match(out, "N varies by predictor (1163-1175)", fixed = TRUE)
  expect_match(out, "1175")
  expect_match(out, "1163")
})


test_that("equal Ns produce no disclosure note", {
  d <- .uv_soc()
  out <- paste(capture.output(print(
    table_regression_uv(d, outcome = smoking, predictors = c(age, sex))
  )), collapse = "\n")
  expect_false(grepl("its own complete cases", out, fixed = TRUE))
  expect_false(grepl("common complete cases", out, fixed = TRUE))
})


test_that("complete_cases = TRUE forces the common sample and says so", {
  d <- .uv_soc()
  t_cc <- table_regression_uv(d, outcome = smoking,
                              predictors = c(age, bmi),
                              complete_cases = TRUE,
                              multivariable = FALSE)
  out <- paste(capture.output(print(t_cc)), collapse = "\n")
  expect_match(out, "All models fit on the 1163 common complete cases.",
               fixed = TRUE)
  # Each univariable fit now runs on the common sample, not its own.
  dcc <- d[stats::complete.cases(d[, c("smoking", "age", "bmi")]), ]
  f <- stats::glm(smoking ~ age, family = stats::binomial(), data = dcc)
  td <- broom::tidy(t_cc)
  expect_equal(td$estimate[td$term == "age"],
               unname(stats::coef(f)["age"]), tolerance = 1e-12)
})


# ---- 3. Layout contract ---------------------------------------------------

test_that("screen-only tables carry no empty fit-stat rows", {
  d <- .uv_soc()
  out <- paste(capture.output(print(
    table_regression_uv(d, outcome = smoking, predictors = c(age, bmi),
                        multivariable = FALSE)
  )), collapse = "\n")
  expect_false(grepl("AIC", out, fixed = TRUE))
  # With the multivariable merge the fit stats come back (its own n/AIC).
  out2 <- paste(capture.output(print(
    table_regression_uv(d, outcome = smoking, predictors = c(age, bmi))
  )), collapse = "\n")
  expect_match(out2, "AIC", fixed = TRUE)
})


test_that("intercepts are hidden by default; show_intercept shows only
           the multivariable one", {
  d <- .uv_soc()
  td <- broom::tidy(table_regression_uv(d, outcome = smoking,
                                        predictors = c(age, sex)))
  expect_false(any(td$is_intercept))
  td2 <- broom::tidy(table_regression_uv(d, outcome = smoking,
                                         predictors = c(age, sex),
                                         show_intercept = TRUE))
  ic <- td2[td2$is_intercept, ]
  # The univariable blocks keep only their own predictor's rows, so the
  # nuisance intercepts of the screen never enter the table.
  expect_identical(nrow(ic), 1L)
  expect_identical(ic$model_id, "Multivariable")
})


test_that("labels pass through to the row stubs", {
  d <- .uv_soc()
  out <- paste(capture.output(print(
    table_regression_uv(d, outcome = smoking, predictors = c(age, sex),
                        labels = c(age = "Age (years)"),
                        multivariable = FALSE)
  )), collapse = "\n")
  expect_match(out, "Age (years)", fixed = TRUE)
})


test_that("rank-deficient predictors stay visible as dropped rows", {
  d <- .uv_soc()
  d$const <- 1
  out <- paste(capture.output(print(
    table_regression_uv(d, outcome = smoking, predictors = c(age, const),
                        multivariable = FALSE)
  )), collapse = "\n")
  expect_match(out, "const")
  expect_match(out, "Rank-deficient")
})


# ---- 4. Multiplicity: the screen is ONE p-adjust family ------------------

test_that("p_adjust adjusts within the screen family and within the
           multivariable family separately", {
  d <- .uv_soc()
  raw <- broom::tidy(table_regression_uv(d, outcome = smoking,
                                         predictors = c(age, bmi, sex)))
  adj <- broom::tidy(table_regression_uv(d, outcome = smoking,
                                         predictors = c(age, bmi, sex),
                                         p_adjust = "holm"))
  for (g in c("Univariable", "Multivariable")) {
    p_r <- raw$p.value[raw$model_id == g]
    p_a <- adj$p.value[adj$model_id == g]
    expect_equal(p_a, stats::p.adjust(p_r, "holm"), tolerance = 1e-12)
  }
})


# ---- 5. Robust vcov passthrough ------------------------------------------

test_that("HC3 flows through to every univariable fit", {
  d <- .uv_soc()
  t_hc <- table_regression_uv(d, outcome = smoking,
                              predictors = c(age, bmi),
                              vcov = "HC3", multivariable = FALSE)
  td <- broom::tidy(t_hc)
  f <- stats::glm(smoking ~ age, family = stats::binomial(), data = d)
  se <- sqrt(diag(sandwich::vcovHC(f, type = "HC3")))["age"]
  expect_equal(td$std.error[td$term == "age"], unname(se),
               tolerance = 1e-9)
  out <- paste(capture.output(print(t_hc)), collapse = "\n")
  expect_match(out, "heteroskedasticity-robust (HC3)", fixed = TRUE)
})


test_that("CR2 clusters align to each fit's own sample (differing Ns)", {
  skip_if_not_installed("clubSandwich")
  d <- .uv_soc()
  t_cr <- table_regression_uv(d, outcome = smoking,
                              predictors = c(age, bmi),
                              vcov = "CR2", cluster = d$region)
  td <- broom::tidy(t_cr)
  # Univariable bmi: its own complete cases (12 bmi values are missing).
  cc_bmi <- !is.na(d$bmi) & !is.na(d$smoking)
  f_bmi <- stats::glm(smoking ~ bmi, family = stats::binomial(), data = d)
  se_uv <- sqrt(diag(clubSandwich::vcovCR(
    f_bmi, cluster = d$region[cc_bmi], type = "CR2")))["bmi"]
  expect_equal(
    td$std.error[td$term == "bmi" & td$model_id == "Univariable"],
    unname(se_uv), tolerance = 1e-9
  )
  # Multivariable: the full fit's complete cases.
  cc_m <- stats::complete.cases(d[, c("smoking", "age", "bmi")])
  f_m <- stats::glm(smoking ~ age + bmi, family = stats::binomial(),
                    data = d)
  se_m <- sqrt(diag(clubSandwich::vcovCR(
    f_m, cluster = d$region[cc_m], type = "CR2")))["bmi"]
  expect_equal(
    td$std.error[td$term == "bmi" & td$model_id == "Multivariable"],
    unname(se_m), tolerance = 1e-9
  )
})


test_that("cluster contract: one value per row of `data`", {
  d <- .uv_soc()
  expect_error(
    table_regression_uv(d, outcome = smoking, predictors = age,
                        vcov = "CR2", cluster = d$region[1:100]),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_uv(d, outcome = smoking, predictors = age,
                        vcov = "CR2",
                        cluster = list(d$region)),
    class = "spicy_invalid_input"
  )
  # complete_cases subsets the cluster vector alongside the data.
  skip_if_not_installed("clubSandwich")
  t_cc <- table_regression_uv(d, outcome = smoking,
                              predictors = c(age, bmi),
                              vcov = "CR2", cluster = d$region,
                              complete_cases = TRUE,
                              multivariable = FALSE)
  expect_s3_class(t_cc, "spicy_regression_table")
})


# ---- 6. method = "lm", titles ---------------------------------------------

test_that("method = 'lm' fits linear screens with the matching title", {
  d <- .uv_soc()
  t_lm <- table_regression_uv(d, outcome = wellbeing_score,
                              predictors = c(age, sex), method = "lm")
  out <- paste(capture.output(print(t_lm)), collapse = "\n")
  expect_match(
    out,
    "Univariable and multivariable linear regression: wellbeing_score",
    fixed = TRUE
  )
  td <- broom::tidy(t_lm)
  f <- stats::lm(wellbeing_score ~ age, data = d)
  expect_equal(
    td$estimate[td$term == "age" & td$model_id == "Univariable"],
    unname(stats::coef(f)["age"]), tolerance = 1e-12
  )
})


test_that("default titles follow the family; custom title wins", {
  d <- .uv_soc()
  out1 <- paste(capture.output(print(
    table_regression_uv(d, outcome = smoking, predictors = age,
                        multivariable = FALSE)
  )), collapse = "\n")
  expect_match(out1, "Univariable logistic regression screen: smoking",
               fixed = TRUE)
  out2 <- paste(capture.output(print(
    table_regression_uv(d, outcome = smoking, predictors = age,
                        family = stats::binomial("probit"))
  )), collapse = "\n")
  expect_match(out2,
               "Univariable and multivariable probit regression: smoking",
               fixed = TRUE)
  out3 <- paste(capture.output(print(
    table_regression_uv(d, outcome = smoking, predictors = age,
                        title = "Table 2. Smoking correlates")
  )), collapse = "\n")
  expect_match(out3, "Table 2. Smoking correlates", fixed = TRUE)
  # Poisson gets its proper name; families off the switch fall back to
  # the family string.
  d$visits <- round(d$wellbeing_score / 10)
  out4 <- paste(capture.output(print(
    table_regression_uv(d, outcome = visits, predictors = age,
                        family = stats::poisson())
  )), collapse = "\n")
  expect_match(out4,
               "Univariable and multivariable Poisson regression: visits",
               fixed = TRUE)
  # gaussian/identity glm is lm by another name: the screen points at
  # its own argument instead of the generic "refit with lm()" caveat.
  expect_error(
    table_regression_uv(d, outcome = wellbeing_score, predictors = age,
                        family = stats::gaussian()),
    class = "spicy_invalid_input"
  )
  # Families off the title switch fall back to the family name.
  d$wb1 <- d$wellbeing_score + 1
  out5 <- paste(capture.output(print(
    table_regression_uv(d, outcome = wb1, predictors = age,
                        family = stats::Gamma(link = "log"))
  )), collapse = "\n")
  expect_match(out5,
               "Univariable and multivariable Gamma regression: wb1",
               fixed = TRUE)
})


# ---- 7. Input validation ---------------------------------------------------

test_that("invalid inputs are refused with clear errors", {
  d <- .uv_soc()
  expect_error(
    table_regression_uv(1:5, outcome = smoking, predictors = age),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_uv(d, outcome = smoking, predictors = age,
                        multivariable = NA),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_uv(d, outcome = smoking, predictors = age,
                        complete_cases = "yes"),
    class = "spicy_invalid_input"
  )
  # The outcome is dropped from the predictor selection -> nothing left.
  expect_error(
    table_regression_uv(d, outcome = smoking, predictors = smoking),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_uv(d, outcome = smoking, predictors = age,
                        nested = TRUE),
    class = "spicy_invalid_input"
  )
})


test_that("a failing univariable fit is reported with the predictor name", {
  d <- .uv_soc()
  d$broken <- factor(rep("only-level", nrow(d)))
  expect_error(
    table_regression_uv(d, outcome = smoking, predictors = c(age, broken),
                        multivariable = FALSE),
    class = "spicy_invalid_data"
  )
})


test_that("a failing multivariable fit is reported as such", {
  d <- .uv_soc()
  n <- nrow(d)
  # Disjoint missingness: each univariable fit has ~n/2 complete cases,
  # the joint fit has zero.
  d$x1 <- d$age
  d$x2 <- d$age
  d$x1[seq_len(floor(n / 2))] <- NA
  d$x2[seq(floor(n / 2) + 1L, n)] <- NA
  expect_error(
    table_regression_uv(d, outcome = smoking, predictors = c(x1, x2)),
    class = "spicy_invalid_data"
  )
})


test_that("the outcome is auto-dropped from tidyselect predictors", {
  d <- .uv_soc()[, c("smoking", "age", "sex")]
  td <- broom::tidy(table_regression_uv(d, outcome = smoking,
                                        predictors = dplyr::everything(),
                                        multivariable = FALSE))
  expect_false(any(grepl("smoking", td$term)))
  expect_setequal(td$term, c("age", "sexMale"))
})


# ---- 8. Snapshot: the flagship layout -------------------------------------

test_that("console snapshot: screen + multivariable merge", {
  d <- .uv_soc()
  # Wide enough for the two column groups on one panel (the default
  # test width of 80 splits a continuation panel; panel splitting has
  # its own tests in test-ascii_panel_polish.R).
  withr::local_options(width = 130)
  expect_snapshot(print(
    table_regression_uv(d, outcome = smoking,
                        predictors = c(age, bmi, sex),
                        exponentiate = TRUE)
  ))
})


test_that("the footer names the cluster column through the wrapper", {
  skip_if_not_installed("clubSandwich")
  d <- .uv_soc()
  out <- paste(capture.output(print(
    table_regression_uv(d, outcome = smoking, predictors = c(age, bmi),
                        vcov = "CR2", cluster = d$region)
  )), collapse = "\n")
  expect_match(out, "clusters by region", fixed = TRUE)
})

test_that("family with method = 'lm': non-gaussian refused, gaussian ignored with a warning", {
  set.seed(42)
  df <- data.frame(
    y = rnorm(40),
    age = rnorm(40, 50, 10),
    sex = factor(rep(c("F", "M"), 20))
  )
  expect_error(
    table_regression_uv(df, outcome = y, predictors = c(age, sex),
                        method = "lm", family = binomial()),
    "not meaningful",
    class = "spicy_invalid_input"
  )
  expect_warning(
    out <- table_regression_uv(df, outcome = y, predictors = c(age, sex),
                               method = "lm", family = gaussian()),
    class = "spicy_ignored_arg"
  )
  expect_s3_class(out, "spicy_regression_table")
})
