# ---------------------------------------------------------------------------
# Phase 7c18 tests: Type-3 Wald chi^2 tests for fixed effects on mixed-
# effects fits. Matches the SAS PROC MIXED / SPSS GENLINMIXED / Stata
# `mixed, testparm` convention -- one joint test per term, with df equal
# to the number of coefficients spanned (k for a k-level factor's k-1
# dummies, 1 for a numeric predictor).
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_lmer_factor_pchi <- function() {
  skip_if_not_installed("lme4")
  d <- lme4::sleepstudy
  d$period <- factor(rep(c("a", "b", "c"), length.out = nrow(d)))
  lme4::lmer(Reaction ~ Days + period + (1 | Subject), data = d)
}

.fit_glmer_factor_pchi <- function() {
  skip_if_not_installed("lme4")
  set.seed(1)
  n <- 500
  g <- factor(rep(1:25, length.out = n))
  x <- rnorm(n)
  cat <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
  y <- rbinom(
    n,
    1,
    plogis(
      0.5 + 0.8 * x + (cat == "B") * 0.3 + (cat == "C") * -0.5 + rnorm(25)[g]
    )
  )
  lme4::glmer(y ~ x + cat + (1 | g), family = binomial)
}

.fit_glmmTMB_pchi <- function() {
  skip_if_not_installed("glmmTMB")
  d <- lme4::sleepstudy
  d$period <- factor(rep(c("a", "b", "c"), length.out = nrow(d)))
  glmmTMB::glmmTMB(Reaction ~ Days + period + (1 | Subject), data = d)
}

.fit_lme_factor_pchi <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ 1 | Subject
  )
}


# ---- 1. Schema: partial_chi2 rows are injected on token request ---------

test_that("lmer with factor: requesting partial_chi2 injects chi^2 rows", {
  fit <- .fit_lmer_factor_pchi()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "partial_chi2")
  )
  expect_true("partial_chi2" %in% fr$coefs$estimate_type)
})

test_that("glmer with factor: requesting partial_chi2 injects chi^2 rows", {
  fit <- .fit_glmer_factor_pchi()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "partial_chi2")
  )
  expect_true("partial_chi2" %in% fr$coefs$estimate_type)
})

test_that("glmmTMB with factor: requesting partial_chi2 injects chi^2 rows", {
  fit <- .fit_glmmTMB_pchi()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "partial_chi2")
  )
  expect_true("partial_chi2" %in% fr$coefs$estimate_type)
})

test_that("lme with factor: requesting partial_chi2 injects chi^2 rows", {
  fit <- .fit_lme_factor_pchi()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "partial_chi2")
  )
  expect_true("partial_chi2" %in% fr$coefs$estimate_type)
})

test_that("partial_chi2 is NOT injected when token absent", {
  fit <- .fit_lmer_factor_pchi()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "se", "ci", "p")
  )
  expect_false("partial_chi2" %in% fr$coefs$estimate_type)
})


# ---- 2. df matches the number of coefficients per term -----------------

test_that("lmer: factor df = (#levels - 1), numeric df = 1", {
  fit <- .fit_lmer_factor_pchi()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "partial_chi2")
  )
  pchi <- fr$coefs[fr$coefs$estimate_type == "partial_chi2", ]
  # `Days` is numeric -> df = 1.
  days_rows <- pchi[pchi$parent_var == "Days", ]
  expect_true(all(days_rows$df == 1))
  # `period` is a 3-level factor -> df = 2 on every level row.
  period_rows <- pchi[pchi$parent_var == "period", ]
  expect_true(all(period_rows$df == 2))
})


# ---- 3. Wald chi^2 formula: matches a manual computation to 1e-10 ------

test_that("lmer numeric term: chi^2 = (B / SE)^2 (single-df Wald)", {
  fit <- .fit_lmer_factor_pchi()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "partial_chi2")
  )
  # Days is a single-coef term -> chi^2 = z^2 = (B / SE)^2.
  bhat <- as.numeric(lme4::fixef(fit)["Days"])
  se_days <- sqrt(as.matrix(stats::vcov(fit))["Days", "Days"])
  expected_chi2 <- (bhat / se_days)^2
  pchi <- fr$coefs[
    fr$coefs$estimate_type == "partial_chi2" &
      fr$coefs$parent_var == "Days",
  ]
  expect_equal(pchi$estimate[1L], expected_chi2, tolerance = 1e-10)
})

test_that("lmer factor term: chi^2 matches t(b) Vinv b across (k-1) dummies", {
  fit <- .fit_lmer_factor_pchi()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "partial_chi2")
  )
  bhat <- lme4::fixef(fit)
  V <- as.matrix(stats::vcov(fit))
  idx <- grep("^period", names(bhat))
  b_sub <- bhat[idx]
  V_sub <- V[idx, idx, drop = FALSE]
  expected_chi2 <- as.numeric(t(b_sub) %*% solve(V_sub) %*% b_sub)
  pchi <- fr$coefs[
    fr$coefs$estimate_type == "partial_chi2" &
      fr$coefs$parent_var == "period",
  ]
  expect_equal(unique(pchi$estimate), expected_chi2, tolerance = 1e-10)
})


# ---- 4. p-value matches pchisq(chi2, df, lower.tail = FALSE) -----------

test_that("p-value is pchisq(chi2, df, lower.tail = FALSE)", {
  fit <- .fit_lmer_factor_pchi()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "partial_chi2")
  )
  pchi <- fr$coefs[fr$coefs$estimate_type == "partial_chi2", ]
  for (i in seq_len(nrow(pchi))) {
    expected_p <- stats::pchisq(
      pchi$estimate[i],
      df = pchi$df[i],
      lower.tail = FALSE
    )
    expect_equal(pchi$p_value[i], expected_p, tolerance = 1e-10)
  }
})


# ---- 5. End-to-end rendering: chi^2 column appears -------------------

test_that("table_regression(lmer, partial_chi2) renders chi^2 column", {
  fit <- .fit_lmer_factor_pchi()
  out <- capture.output(print(
    table_regression(fit, show_columns = c("b", "p", "partial_chi2"))
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "χ²", fixed = TRUE)
  # The chi^2 cell renders "value (df)" -- e.g. "167.67 (1)" for Days.
  expect_true(grepl("[0-9]+\\.[0-9]+ \\([0-9]+\\)", combined))
})

test_that("table_regression(glmer, partial_chi2) joint test on factor", {
  fit <- .fit_glmer_factor_pchi()
  out <- capture.output(print(
    table_regression(fit, show_columns = c("b", "p", "partial_chi2"))
  ))
  combined <- paste(out, collapse = "\n")
  expect_match(combined, "χ²", fixed = TRUE)
  # 3-level cat -> "k.kk (2)" on every level row of cat.
  expect_match(combined, "\\([2]\\)")
})


# ---- 6. Engine parity: all 4 produce the same chi^2 schema --------------

test_that("glmmTMB partial_chi2 rows have the same schema as lmer rows", {
  fit <- .fit_glmmTMB_pchi()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "partial_chi2")
  )
  pchi <- fr$coefs[fr$coefs$estimate_type == "partial_chi2", ]
  expect_true(all(
    c("term", "parent_var", "estimate", "df", "p_value", "test_type") %in%
      colnames(pchi)
  ))
  expect_true(all(pchi$test_type == "X2"))
})

test_that("lme partial_chi2 rows have finite chi^2 + p-value", {
  fit <- .fit_lme_factor_pchi()
  fr <- as_regression_frame(
    fit,
    model_id = "M1",
    show_columns = c("b", "partial_chi2")
  )
  pchi <- fr$coefs[fr$coefs$estimate_type == "partial_chi2", ]
  expect_true(all(is.finite(pchi$estimate)))
  expect_true(all(is.finite(pchi$p_value)))
})
