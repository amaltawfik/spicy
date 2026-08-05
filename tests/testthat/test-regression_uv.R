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
  sochealth[, c("smoking", "age", "bmi", "sex", "region", "wellbeing_score")]
}


# ---- 1. Numeric oracle: per-predictor fits + pinned gtsummary ------------

test_that("univariable rows reproduce the per-predictor glm fits exactly", {
  d <- .uv_soc()
  t_uv <- table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = c(age, bmi, sex),
    exponentiate = TRUE,
    multivariable = FALSE
  )
  td <- broom::tidy(t_uv)

  for (v in c("age", "bmi")) {
    f <- stats::glm(
      stats::reformulate(v, "smoking"),
      family = stats::binomial(),
      data = d
    )
    sm <- summary(f)$coefficients
    expect_equal(
      td$estimate[td$term == v],
      exp(unname(stats::coef(f)[v])),
      tolerance = 1e-12
    )
    # Wald CI: exp of the link-scale interval.
    ci <- exp(stats::confint.default(f)[v, ])
    expect_equal(td$conf.low[td$term == v], unname(ci[1]), tolerance = 1e-9)
    expect_equal(td$conf.high[td$term == v], unname(ci[2]), tolerance = 1e-9)
    # SE on the exp scale is the delta-method OR-scale SE.
    expect_equal(
      td$std.error[td$term == v],
      exp(unname(stats::coef(f)[v])) * unname(sm[v, 2]),
      tolerance = 1e-9
    )
    expect_equal(td$p.value[td$term == v], unname(sm[v, 4]), tolerance = 1e-12)
  }
  f_sex <- stats::glm(smoking ~ sex, family = stats::binomial(), data = d)
  expect_equal(
    td$estimate[td$term == "sexMale"],
    exp(unname(stats::coef(f_sex)["sexMale"])),
    tolerance = 1e-12
  )

  # gtsummary 2.5.1 pinned constants (provenance in the file header).
  expect_equal(td$estimate[td$term == "age"], 1.005119471382, tolerance = 1e-9)
  expect_equal(td$p.value[td$term == "age"], 0.291844336359, tolerance = 1e-9)
  expect_equal(td$estimate[td$term == "bmi"], 0.997710907231, tolerance = 1e-9)
  expect_equal(td$p.value[td$term == "bmi"], 0.905265160667, tolerance = 1e-9)
  expect_equal(
    td$estimate[td$term == "sexMale"],
    0.948697550820,
    tolerance = 1e-9
  )
  expect_equal(
    td$p.value[td$term == "sexMale"],
    0.712535321339,
    tolerance = 1e-9
  )
})


test_that("the multivariable column equals the full fit exactly", {
  d <- .uv_soc()
  t_uv <- table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = c(age, bmi, sex)
  )
  td <- broom::tidy(t_uv)
  expect_setequal(unique(td$model_id), c("Univariable", "Multivariable"))

  f <- stats::glm(
    smoking ~ age + bmi + sex,
    family = stats::binomial(),
    data = d
  )
  sm <- summary(f)$coefficients
  for (v in c("age", "bmi", "sexMale")) {
    row <- td$model_id == "Multivariable" & td$term == v
    expect_equal(td$estimate[row], unname(stats::coef(f)[v]), tolerance = 1e-12)
    expect_equal(td$p.value[row], unname(sm[v, 4]), tolerance = 1e-12)
  }
})


# ---- 2. N column + sample-size disclosure --------------------------------

test_that("per-predictor N is shown and the differing-N note fires", {
  d <- .uv_soc()
  out <- paste(
    capture.output(print(
      table_regression_uv(
        d,
        outcome = smoking,
        method = "glm",
        predictors = c(age, bmi, sex)
      )
    )),
    collapse = "\n"
  )
  # bmi has 12 missing values -> its own complete cases differ.
  expect_match(out, "N varies by predictor (1163-1175)", fixed = TRUE)
  expect_match(out, "1175")
  expect_match(out, "1163")
})


test_that("equal Ns produce no disclosure note", {
  d <- .uv_soc()
  out <- paste(
    capture.output(print(
      table_regression_uv(
        d,
        outcome = smoking,
        method = "glm",
        predictors = c(age, sex)
      )
    )),
    collapse = "\n"
  )
  expect_false(grepl("its own complete cases", out, fixed = TRUE))
  expect_false(grepl("common complete cases", out, fixed = TRUE))
})


test_that("complete_cases = TRUE forces the common sample and says so", {
  d <- .uv_soc()
  t_cc <- table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = c(age, bmi),
    complete_cases = TRUE,
    multivariable = FALSE
  )
  out <- paste(capture.output(print(t_cc)), collapse = "\n")
  expect_match(
    out,
    "All models fit on the 1163 common complete cases.",
    fixed = TRUE
  )
  # Each univariable fit now runs on the common sample, not its own.
  dcc <- d[stats::complete.cases(d[, c("smoking", "age", "bmi")]), ]
  f <- stats::glm(smoking ~ age, family = stats::binomial(), data = dcc)
  td <- broom::tidy(t_cc)
  expect_equal(
    td$estimate[td$term == "age"],
    unname(stats::coef(f)["age"]),
    tolerance = 1e-12
  )
})


# ---- 3. Layout contract ---------------------------------------------------

test_that("screen-only tables carry no empty fit-stat rows", {
  d <- .uv_soc()
  out <- paste(
    capture.output(print(
      table_regression_uv(
        d,
        outcome = smoking,
        method = "glm",
        predictors = c(age, bmi),
        multivariable = FALSE
      )
    )),
    collapse = "\n"
  )
  expect_false(grepl("AIC", out, fixed = TRUE))
  # With the multivariable merge the fit stats come back (its own n/AIC).
  out2 <- paste(
    capture.output(print(
      table_regression_uv(
        d,
        outcome = smoking,
        method = "glm",
        predictors = c(age, bmi)
      )
    )),
    collapse = "\n"
  )
  expect_match(out2, "AIC", fixed = TRUE)
})


test_that("intercepts are hidden by default; show_intercept shows both
           sides", {
  d <- .uv_soc()
  td <- broom::tidy(table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = c(age, sex)
  ))
  expect_false(any(td$is_intercept))
  td2 <- broom::tidy(table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = c(age, sex),
    show_intercept = TRUE
  ))
  ic <- td2[td2$is_intercept, ]
  # One intercept per univariable block (unique per-block terms) plus
  # the multivariable model's own.
  expect_identical(nrow(ic), 3L)
  expect_identical(sum(ic$model_id == "Univariable"), 2L)
  expect_identical(sum(ic$model_id == "Multivariable"), 1L)
})


test_that("labels pass through to the row stubs", {
  d <- .uv_soc()
  out <- paste(
    capture.output(print(
      table_regression_uv(
        d,
        outcome = smoking,
        method = "glm",
        predictors = c(age, sex),
        labels = c(age = "Age (years)"),
        multivariable = FALSE
      )
    )),
    collapse = "\n"
  )
  expect_match(out, "Age (years)", fixed = TRUE)
})


test_that("rank-deficient predictors stay visible as dropped rows", {
  d <- .uv_soc()
  d$const <- 1
  out <- paste(
    capture.output(print(
      table_regression_uv(
        d,
        outcome = smoking,
        method = "glm",
        predictors = c(age, const),
        multivariable = FALSE
      )
    )),
    collapse = "\n"
  )
  expect_match(out, "const")
  expect_match(out, "Rank-deficient")
})


# ---- 4. Multiplicity: the screen is ONE p-adjust family ------------------

test_that("p_adjust adjusts within the screen family and within the
           multivariable family separately", {
  d <- .uv_soc()
  raw <- broom::tidy(table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = c(age, bmi, sex)
  ))
  adj <- broom::tidy(table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = c(age, bmi, sex),
    p_adjust = "holm"
  ))
  for (g in c("Univariable", "Multivariable")) {
    p_r <- raw$p.value[raw$model_id == g]
    p_a <- adj$p.value[adj$model_id == g]
    expect_equal(p_a, stats::p.adjust(p_r, "holm"), tolerance = 1e-12)
  }
})


# ---- 5. Robust vcov passthrough ------------------------------------------

test_that("HC3 flows through to every univariable fit", {
  d <- .uv_soc()
  t_hc <- table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = c(age, bmi),
    vcov = "HC3",
    multivariable = FALSE
  )
  td <- broom::tidy(t_hc)
  f <- stats::glm(smoking ~ age, family = stats::binomial(), data = d)
  se <- sqrt(diag(sandwich::vcovHC(f, type = "HC3")))["age"]
  expect_equal(td$std.error[td$term == "age"], unname(se), tolerance = 1e-9)
  out <- paste(capture.output(print(t_hc)), collapse = "\n")
  expect_match(out, "heteroskedasticity-robust (HC3)", fixed = TRUE)
})


test_that("CR2 clusters align to each fit's own sample (differing Ns)", {
  skip_if_not_installed("clubSandwich")
  d <- .uv_soc()
  t_cr <- table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = c(age, bmi),
    vcov = "CR2",
    cluster = d$region
  )
  td <- broom::tidy(t_cr)
  # Univariable bmi: its own complete cases (12 bmi values are missing).
  cc_bmi <- !is.na(d$bmi) & !is.na(d$smoking)
  f_bmi <- stats::glm(smoking ~ bmi, family = stats::binomial(), data = d)
  se_uv <- sqrt(diag(clubSandwich::vcovCR(
    f_bmi,
    cluster = d$region[cc_bmi],
    type = "CR2"
  )))["bmi"]
  expect_equal(
    td$std.error[td$term == "bmi" & td$model_id == "Univariable"],
    unname(se_uv),
    tolerance = 1e-9
  )
  # Multivariable: the full fit's complete cases.
  cc_m <- stats::complete.cases(d[, c("smoking", "age", "bmi")])
  f_m <- stats::glm(smoking ~ age + bmi, family = stats::binomial(), data = d)
  se_m <- sqrt(diag(clubSandwich::vcovCR(
    f_m,
    cluster = d$region[cc_m],
    type = "CR2"
  )))["bmi"]
  expect_equal(
    td$std.error[td$term == "bmi" & td$model_id == "Multivariable"],
    unname(se_m),
    tolerance = 1e-9
  )
})


test_that("cluster contract: one value per row of `data`", {
  d <- .uv_soc()
  expect_error(
    table_regression_uv(
      d,
      outcome = smoking,
      method = "glm",
      predictors = age,
      vcov = "CR2",
      cluster = d$region[1:100]
    ),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_uv(
      d,
      outcome = smoking,
      method = "glm",
      predictors = age,
      vcov = "CR2",
      cluster = list(d$region)
    ),
    class = "spicy_invalid_input"
  )
  # complete_cases subsets the cluster vector alongside the data.
  skip_if_not_installed("clubSandwich")
  t_cc <- table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = c(age, bmi),
    vcov = "CR2",
    cluster = d$region,
    complete_cases = TRUE,
    multivariable = FALSE
  )
  expect_s3_class(t_cc, "spicy_regression_table")
})


# ---- 6. method = "lm", titles ---------------------------------------------

test_that("method = 'lm' fits linear screens with the matching title", {
  d <- .uv_soc()
  t_lm <- table_regression_uv(
    d,
    outcome = wellbeing_score,
    predictors = c(age, sex),
    method = "lm"
  )
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
    unname(stats::coef(f)["age"]),
    tolerance = 1e-12
  )
})


test_that("default titles follow the family; custom title wins", {
  d <- .uv_soc()
  out1 <- paste(
    capture.output(print(
      table_regression_uv(
        d,
        outcome = smoking,
        method = "glm",
        predictors = age,
        multivariable = FALSE
      )
    )),
    collapse = "\n"
  )
  expect_match(
    out1,
    "Univariable logistic regression screen: smoking",
    fixed = TRUE
  )
  out2 <- paste(
    capture.output(print(
      table_regression_uv(
        d,
        outcome = smoking,
        method = "glm",
        predictors = age,
        family = stats::binomial("probit")
      )
    )),
    collapse = "\n"
  )
  expect_match(
    out2,
    "Univariable and multivariable probit regression: smoking",
    fixed = TRUE
  )
  out3 <- paste(
    capture.output(print(
      table_regression_uv(
        d,
        outcome = smoking,
        method = "glm",
        predictors = age,
        title = "Table 2. Smoking correlates"
      )
    )),
    collapse = "\n"
  )
  expect_match(out3, "Table 2. Smoking correlates", fixed = TRUE)
  # Poisson gets its proper name; families off the switch fall back to
  # the family string.
  d$visits <- round(d$wellbeing_score / 10)
  out4 <- paste(
    capture.output(print(
      table_regression_uv(
        d,
        outcome = visits,
        predictors = age,
        family = stats::poisson()
      )
    )),
    collapse = "\n"
  )
  expect_match(
    out4,
    "Univariable and multivariable Poisson regression: visits",
    fixed = TRUE
  )
  # gaussian/identity glm is lm by another name: the screen points at
  # its own argument instead of the generic "refit with lm()" caveat.
  expect_error(
    table_regression_uv(
      d,
      outcome = wellbeing_score,
      predictors = age,
      family = stats::gaussian()
    ),
    class = "spicy_invalid_input"
  )
  # Families off the title switch fall back to the family name.
  d$wb1 <- d$wellbeing_score + 1
  out5 <- paste(
    capture.output(print(
      table_regression_uv(
        d,
        outcome = wb1,
        predictors = age,
        family = stats::Gamma(link = "log")
      )
    )),
    collapse = "\n"
  )
  expect_match(
    out5,
    "Univariable and multivariable Gamma regression: wb1",
    fixed = TRUE
  )
})


# ---- 7. Input validation ---------------------------------------------------

test_that("invalid inputs are refused with clear errors", {
  d <- .uv_soc()
  expect_error(
    table_regression_uv(
      1:5,
      outcome = smoking,
      method = "glm",
      predictors = age
    ),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_uv(
      d,
      outcome = smoking,
      method = "glm",
      predictors = age,
      multivariable = NA
    ),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_uv(
      d,
      outcome = smoking,
      method = "glm",
      predictors = age,
      complete_cases = "yes"
    ),
    class = "spicy_invalid_input"
  )
  # The outcome is dropped from the predictor selection -> nothing left.
  expect_error(
    table_regression_uv(
      d,
      outcome = smoking,
      method = "glm",
      predictors = smoking
    ),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression_uv(
      d,
      outcome = smoking,
      method = "glm",
      predictors = age,
      nested = TRUE
    ),
    class = "spicy_invalid_input"
  )
})


test_that("a failing univariable fit is reported with the predictor name", {
  d <- .uv_soc()
  d$broken <- factor(rep("only-level", nrow(d)))
  expect_error(
    table_regression_uv(
      d,
      outcome = smoking,
      method = "glm",
      predictors = c(age, broken),
      multivariable = FALSE
    ),
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
    table_regression_uv(
      d,
      outcome = smoking,
      method = "glm",
      predictors = c(x1, x2)
    ),
    class = "spicy_invalid_data"
  )
})


test_that("the outcome is auto-dropped from tidyselect predictors", {
  d <- .uv_soc()[, c("smoking", "age", "sex")]
  td <- broom::tidy(table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = dplyr::everything(),
    multivariable = FALSE
  ))
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
    table_regression_uv(
      d,
      outcome = smoking,
      method = "glm",
      predictors = c(age, bmi, sex),
      exponentiate = TRUE
    )
  ))
})


test_that("the footer names the cluster column through the wrapper", {
  skip_if_not_installed("clubSandwich")
  d <- .uv_soc()
  out <- paste(
    capture.output(print(
      table_regression_uv(
        d,
        outcome = smoking,
        method = "glm",
        predictors = c(age, bmi),
        vcov = "CR2",
        cluster = d$region
      )
    )),
    collapse = "\n"
  )
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
    table_regression_uv(
      df,
      outcome = y,
      predictors = c(age, sex),
      method = "lm",
      family = binomial()
    ),
    "not meaningful",
    class = "spicy_invalid_input"
  )
  expect_warning(
    out <- table_regression_uv(
      df,
      outcome = y,
      predictors = c(age, sex),
      method = "lm",
      family = gaussian()
    ),
    class = "spicy_ignored_arg"
  )
  expect_s3_class(out, "spicy_regression_table")
})


# ---- 9. The 0.13 default: linear screen (Mood-aligned) --------------------

test_that("default method is the linear screen on a continuous outcome", {
  d <- sochealth[1:300, ]
  t_lin <- expect_no_warning(
    table_regression_uv(
      d,
      outcome = wellbeing_score,
      predictors = c(age, sex),
      multivariable = FALSE
    )
  )
  td <- broom::tidy(t_lin)
  f_age <- stats::lm(wellbeing_score ~ age, data = d)
  expect_equal(
    td$estimate[td$term == "age"][1],
    unname(stats::coef(f_age)[["age"]]),
    tolerance = 1e-10
  )
  expect_match(attr(t_lin, "title"), "linear regression screen", fixed = TRUE)
})

test_that("binary outcome under the default warns (LPM) and fits lm", {
  d <- sochealth[1:300, ]
  expect_warning(
    t_lpm <- table_regression_uv(
      d,
      outcome = smoking,
      predictors = age,
      multivariable = FALSE
    ),
    class = "spicy_model_choice"
  )
  td <- broom::tidy(t_lpm)
  f <- stats::lm(as.integer(smoking == "Yes") ~ age, data = d)
  expect_equal(
    abs(td$estimate[td$term == "age"][1]),
    abs(unname(stats::coef(f)[["age"]])),
    tolerance = 1e-10
  )
})

test_that("explicit method = 'lm' on a binary outcome stays silent", {
  d <- sochealth[1:300, ]
  expect_no_warning(
    table_regression_uv(
      d,
      outcome = smoking,
      predictors = age,
      method = "lm",
      multivariable = FALSE
    )
  )
})

test_that("family without method selects the glm screen (0.12 calls intact)", {
  d <- sochealth[1:300, ]
  t_fam <- expect_no_warning(
    table_regression_uv(
      d,
      outcome = smoking,
      predictors = age,
      family = stats::binomial(),
      multivariable = FALSE
    )
  )
  t_glm <- table_regression_uv(
    d,
    outcome = smoking,
    predictors = age,
    method = "glm",
    multivariable = FALSE
  )
  expect_equal(broom::tidy(t_fam)$estimate, broom::tidy(t_glm)$estimate)
  expect_match(attr(t_fam, "title"), "logistic", ignore.case = TRUE)
})

test_that("family is normalised like stats::glm(): object, name, constructor", {
  d <- sochealth[1:300, ]
  fit_with <- function(fam) {
    table_regression_uv(
      d,
      outcome = smoking,
      predictors = age,
      family = fam,
      multivariable = FALSE
    )
  }
  t_obj <- fit_with(stats::binomial())
  t_str <- fit_with("binomial")
  t_fun <- fit_with(stats::binomial)
  expect_equal(broom::tidy(t_str)$estimate, broom::tidy(t_obj)$estimate)
  expect_equal(broom::tidy(t_fun)$estimate, broom::tidy(t_obj)$estimate)
  # all three forms route the screen to glm, so the title is logistic
  expect_match(attr(t_str, "title"), "logistic", ignore.case = TRUE)
  expect_match(attr(t_fun, "title"), "logistic", ignore.case = TRUE)
})

test_that("an invalid family is refused up front with a classed error", {
  d <- sochealth[1:100, ]
  for (bad in list("not_a_family", 42, list(family = "binomial"), mean)) {
    expect_error(
      table_regression_uv(
        d,
        outcome = smoking,
        predictors = age,
        family = bad,
        multivariable = FALSE
      ),
      "must be a stats::family",
      class = "spicy_invalid_input"
    )
  }
})

test_that("a >2-level outcome under the default is refused with guidance", {
  expect_error(
    table_regression_uv(
      sochealth[1:200, ],
      outcome = education,
      predictors = age,
      multivariable = FALSE
    ),
    class = "spicy_invalid_data"
  )
})


# ============================================================================
# Phase 3 matrix – rd-uv-estimands: family gates, LPM coding, contract
# ============================================================================

test_that("gaussian identity is refused for the glm screen in all three forms", {
  # rd-uv-estimands:gaussian-identity-refused-glm
  d <- .uv_soc()
  for (ff in list(stats::gaussian(), "gaussian", stats::gaussian)) {
    expect_error(
      table_regression_uv(
        d,
        outcome = wellbeing_score,
        predictors = c(age, bmi),
        method = "glm",
        family = ff
      ),
      regexp = 'method = "lm"',
      fixed = TRUE,
      class = "spicy_invalid_input"
    )
  }
  # A non-identity gaussian link is NOT caught by this gate.
  set.seed(41)
  dpos <- data.frame(y = rexp(60) + 1, x = rnorm(60))
  out <- table_regression_uv(
    dpos,
    outcome = y,
    predictors = c(x),
    method = "glm",
    family = stats::gaussian("log"),
    output = "long"
  )
  expect_true("x" %in% out$term)
})

test_that("method = 'lm' + non-gaussian family points at the glm screen", {
  # rd-uv-estimands:lm-nongaussian-family-refused (message half; the
  # class half is pinned in "family with method = 'lm'" above)
  d <- .uv_soc()
  expect_error(
    table_regression_uv(
      d,
      outcome = wellbeing_score,
      predictors = c(age),
      method = "lm",
      family = stats::binomial()
    ),
    regexp = 'method = "glm"',
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
})

test_that("a 3-level factor with two observed levels passes the linear screen", {
  # rd-uv-estimands:lm-multilevel-outcome-refused ('observed levels'
  # nuance; the >2-observed refusal is pinned above)
  set.seed(31)
  n <- 80
  d <- data.frame(
    y = factor(sample(c("a", "b"), n, TRUE), levels = c("a", "b", "c")),
    x = rnorm(n)
  )
  out <- NULL
  expect_warning(
    out <- table_regression_uv(
      d,
      outcome = y,
      predictors = c(x),
      output = "long"
    ),
    class = "spicy_model_choice"
  )
  # The screen fit an LPM on the second OBSERVED level ("b").
  expect_equal(
    out$estimate[out$term == "x" & out$model_id == "Univariable"],
    unname(coef(lm(as.integer(y == "b") ~ x, data = d))["x"]),
    tolerance = 1e-10
  )
})

test_that("LPM codes 0/1 on the second level and the warning names it", {
  # rd-uv-estimands:lpm-01-coding-second-level-named
  set.seed(32)
  n <- 100
  d <- data.frame(
    yn = factor(sample(c("no", "yes"), n, TRUE)),
    x = rnorm(n)
  )
  wmsg <- NULL
  out <- withCallingHandlers(
    table_regression_uv(d, outcome = yn, predictors = c(x), output = "long"),
    spicy_model_choice = function(w) {
      wmsg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  # The warning names the modeled probability (second factor level).
  expect_match(wmsg, "P(yn = yes)", fixed = TRUE)
  # SIGNED equality against the explicit 0/1 recode: the coding
  # direction (second level = 1), not just the magnitude.
  expect_equal(
    out$estimate[out$model_id == "Univariable" & out$term == "x"],
    unname(coef(lm(as.integer(yn == "yes") ~ x, data = d))["x"]),
    tolerance = 1e-10
  )
  # Logical outcome: TRUE is the modeled level.
  d$lg <- d$yn == "yes"
  wmsg2 <- NULL
  out2 <- withCallingHandlers(
    table_regression_uv(d, outcome = lg, predictors = c(x), output = "long"),
    spicy_model_choice = function(w) {
      wmsg2 <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_match(wmsg2, "P(lg = TRUE)", fixed = TRUE)
  expect_equal(
    out2$estimate[out2$model_id == "Univariable" & out2$term == "x"],
    unname(coef(lm(as.integer(lg) ~ x, data = d))["x"]),
    tolerance = 1e-10
  )
})

test_that("tidyselect helpers work and the outcome is dropped from them", {
  # rd-uv-estimands:predictors-outcome-autodropped (where() half; the
  # everything() half is pinned above, the Surv half in the coxph file)
  set.seed(33)
  n <- 90
  d <- data.frame(
    y = rbinom(n, 1, 0.4),
    a = rnorm(n),
    b = rnorm(n),
    f = factor(sample(c("u", "v"), n, TRUE))
  )
  out <- table_regression_uv(
    d,
    outcome = y,
    predictors = where(is.numeric),
    method = "glm",
    output = "long"
  )
  # The numeric outcome sat inside the helper selection: no
  # outcome-on-outcome fit, no row block for it, and the non-numeric
  # column stays outside the selection.
  expect_setequal(unique(out$term), c("a", "b"))
})

test_that("show_intercept = TRUE shows the univariable intercepts too", {
  # rd-uv-estimands:show-intercept-default-false – the Rd Intercepts
  # section promises both sides under show_intercept = TRUE: each
  # univariable block opens with its own fit's intercept, rendered
  # under the standard "(Intercept)" label.
  d <- .uv_soc()
  scr <- table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = c(age, bmi),
    show_intercept = TRUE
  )
  # Body: one "(Intercept)" stub per univariable block + one for the
  # multivariable model.
  expect_identical(sum(trimws(scr$Variable) == "(Intercept)"), 3L)
  td <- broom::tidy(scr)
  ic <- td[td$is_intercept & td$model_id == "Univariable", ]
  expect_identical(nrow(ic), 2L)
  # Each univariable intercept equals its own fit's intercept, under
  # its per-block term.
  f_age <- stats::glm(
    smoking ~ age,
    data = d[stats::complete.cases(d[, c("smoking", "age")]), ],
    family = stats::binomial()
  )
  expect_equal(
    ic$estimate[ic$term == "age: (Intercept)"],
    unname(stats::coef(f_age)["(Intercept)"]),
    tolerance = 1e-10
  )
  # The default (FALSE) keeps the historical layout: no intercept row
  # anywhere.
  scr0 <- table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = c(age, bmi)
  )
  expect_identical(sum(trimws(scr0$Variable) == "(Intercept)"), 0L)
})

test_that("keep on a predictor name spares BOTH intercepts (uv + multivariable)", {
  # Regression (delta review D7): the uv intercepts are keyed
  # "<pred>: (Intercept)", so `keep = "age"` used to retain the age
  # block's intercept while filtering the multivariable "(Intercept)"
  # -- an asymmetric, layout-dependent result. Intercept rows are now
  # exempt from keep/drop; `show_intercept` alone governs them.
  d <- .uv_soc()
  scr <- table_regression_uv(
    d,
    outcome = smoking,
    method = "glm",
    predictors = c(age, bmi),
    show_intercept = TRUE,
    keep = "age"
  )
  td <- broom::tidy(scr)
  ic <- td[td$is_intercept, ]
  # One uv intercept (the surviving age block) + the multivariable one.
  expect_identical(sort(unique(ic$model_id)), c("Multivariable", "Univariable"))
  expect_true("age: (Intercept)" %in% ic$term)
  expect_true("(Intercept)" %in% ic$term)
  # The bmi block is gone -- including its own intercept (no orphan).
  expect_false(any(td$term == "bmi"))
  expect_false("bmi: (Intercept)" %in% ic$term)
  expect_true(any(td$term == "age" | grepl("^age", td$term)))
})

test_that("the documented example runs and yields the OR screen", {
  # rd-uv-estimands:example-runs (the \donttest body, verbatim
  # arguments)
  out <- table_regression_uv(
    sochealth,
    outcome = smoking,
    predictors = c(age, sex, education),
    family = binomial(),
    exponentiate = TRUE
  )
  expect_s3_class(out, "spicy_regression_table")
  expect_true(all(
    c("Univariable: OR", "Multivariable: OR") %in% names(out)
  ))
  expect_match(
    attr(out, "title"),
    "Univariable and multivariable logistic regression: smoking",
    fixed = TRUE
  )
})

test_that("the screen honours the table_regression output contract", {
  # rd-uv-estimands:value-same-output-contract
  d <- .uv_soc()
  scr <- table_regression_uv(
    d,
    outcome = smoking,
    predictors = c(age, bmi),
    method = "glm"
  )
  expect_identical(
    class(scr),
    c("spicy_regression_table", "spicy_table", "data.frame")
  )
  s <- as_structured(scr)
  expect_true(is.data.frame(s$body))
  expect_true("Univariable: B" %in% names(s$body))
  dd <- as.data.frame(scr)
  expect_identical(class(dd), "data.frame")
  td <- broom::tidy(scr)
  expect_s3_class(td, "tbl_df")
  expect_true(all(c("Univariable", "Multivariable") %in% td$model_id))
  expect_no_error(invisible(capture.output(knitr::knit_print(scr))))
})

test_that("the screen renders through the rich engines", {
  # rd-uv-estimands:value-same-output-contract (engine half)
  skip_if_not_installed("tinytable")
  skip_if_not_installed("flextable")
  d <- .uv_soc()
  tt <- table_regression_uv(
    d,
    outcome = smoking,
    predictors = c(age, bmi),
    method = "glm",
    output = "tinytable"
  )
  # tinytable mixes S3 and S4 across versions; inherits() works for both.
  expect_true(inherits(tt, "tinytable"))
  ft <- table_regression_uv(
    d,
    outcome = smoking,
    predictors = c(age, bmi),
    method = "glm",
    output = "flextable"
  )
  expect_s3_class(ft, "spicy_flextable")
  expect_s3_class(ft, "flextable")
})


# Phase 3 matrix – vignettes-news:uv-default-linear-screen (warning
# content half) (lot T4)

test_that("the LPM disclosure points at vcov = 'HC3' and method = 'glm'", {
  w <- NULL
  withCallingHandlers(
    table_regression_uv(
      sochealth,
      outcome = smoking,
      predictors = c(age, sex),
      multivariable = FALSE
    ),
    spicy_model_choice = function(c) {
      w <<- c
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(w, "spicy_model_choice")
  msg <- conditionMessage(w)
  expect_match(msg, "linear probability model", fixed = TRUE)
  expect_match(msg, "HC3", fixed = TRUE)
  expect_match(msg, "method = \"glm\"", fixed = TRUE)
})


# Phase 3 matrix – critic:s3-undoc-terms-uv-screen (lot T4)

test_that("terms.spicy_uv_screen returns outcome ~ all predictors, backtick-safe", {
  scr <- structure(
    list(
      fits = list(),
      outcome = "well being",
      predictors = c("age years", "sex")
    ),
    class = "spicy_uv_screen"
  )
  tt <- stats::terms(scr)
  expect_s3_class(tt, "terms")
  expect_identical(
    deparse(formula(tt)),
    "`well being` ~ `age years` + sex"
  )
  expect_identical(attr(tt, "term.labels"), c("`age years`", "sex"))
})
