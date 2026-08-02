# ---------------------------------------------------------------------------
# Phase 7c18 tests: term-level Wald chi^2 tests for fixed effects on
# mixed-effects fits -- one joint test per term, with df equal to the
# number of coefficients spanned (k-1 for a k-level factor's dummies,
# 1 for a numeric predictor). The hypothesis is the marginality-
# respecting Type II of car::Anova(type = 2), a deliberate departure
# from the Type-III default of SAS PROC MIXED / lmerTest; for additive
# models and highest-order terms the two coincide (block Wald).
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


# ---- 7. Type-II under interactions: car oracle + contrast invariance ----

# Term-level (chi2, df, p) from the coefficient-level rows, keyed by
# the fixed-formula assign map (robust to the dummy naming, which
# changes across contrast codings). suppressWarnings absorbs the
# one-time lme4 "nobars moved to reformulas" deprecation notice.
.pchi_by_term <- function(fit) {
  rows <- suppressWarnings(spicy:::.compute_partial_chi2_rows_for_mixed(fit))
  is_lme <- inherits(fit, "lme")
  ff <- if (is_lme) {
    stats::formula(fit)
  } else {
    suppressWarnings(lme4::nobars(stats::formula(fit)))
  }
  mm <- if (is_lme) {
    stats::model.matrix(ff, data = nlme::getData(fit))
  } else {
    stats::model.matrix(fit)
  }
  asgn <- attr(mm, "assign")
  labs <- attr(stats::terms(ff), "term.labels")
  out <- lapply(seq_along(labs), function(k) {
    sub <- rows[rows$term %in% colnames(mm)[asgn == k], ]
    list(
      chi2 = unique(sub$estimate),
      df = unique(sub$df),
      p = unique(sub$p_value)
    )
  })
  setNames(out, labs)
}

.pchi_inter_lmer_data <- function() {
  set.seed(42)
  d <- lme4::sleepstudy
  d$A <- factor(sample(c("a", "b", "c"), nrow(d), TRUE, prob = c(.5, .3, .2)))
  d$B <- factor(sample(c("u", "v"), nrow(d), TRUE, prob = c(.6, .4)))
  d
}

.fit_lmer_inter_pchi <- function(d = .pchi_inter_lmer_data()) {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + A * B + (1 | Subject), data = d)
}

.pchi_inter_glmer_data <- function() {
  set.seed(2)
  n <- 500
  g <- factor(rep(1:25, length.out = n))
  x <- rnorm(n)
  A <- factor(sample(c("a", "b", "c"), n, TRUE, prob = c(.5, .3, .2)))
  eta <- 0.4 +
    0.8 * x +
    0.3 * (A == "b") -
    0.5 * (A == "c") +
    0.4 * x * (A == "b") +
    rnorm(25)[g]
  data.frame(y = rbinom(n, 1, plogis(eta)), x = x, A = A, g = g)
}

.fit_glmer_inter_pchi <- function(d = .pchi_inter_glmer_data()) {
  skip_if_not_installed("lme4")
  lme4::glmer(y ~ x * A + (1 | g), family = binomial, data = d)
}

test_that("lmer with interaction, unbalanced: chi2/df/p match car::Anova(type = 2)", {
  skip_if_not_installed("car")
  fit <- .fit_lmer_inter_pchi()
  ours <- .pchi_by_term(fit)
  ca <- car::Anova(fit, type = 2) # Type II Wald chisquare tests
  for (term in rownames(ca)) {
    expect_equal(ours[[term]]$chi2, ca[term, "Chisq"], tolerance = 1e-8)
    expect_equal(ours[[term]]$df, as.numeric(ca[term, "Df"]))
    expect_equal(ours[[term]]$p, ca[term, "Pr(>Chisq)"], tolerance = 1e-8)
  }
  # Discriminating: the old Type-III coefficient-block drop gives a
  # DIFFERENT main-effect value under treatment coding.
  b <- lme4::fixef(fit)
  V <- as.matrix(stats::vcov(fit))
  asgn <- attr(stats::model.matrix(fit), "assign")
  idx <- which(asgn == 2L) # term A
  block <- as.numeric(t(b[idx]) %*% solve(V[idx, idx]) %*% b[idx])
  expect_gt(abs(ours[["A"]]$chi2 - block), 1e-4)
})

test_that("glmer with interaction, unbalanced: chi2/df match car::Anova(type = 2)", {
  skip_if_not_installed("car")
  fit <- .fit_glmer_inter_pchi()
  ours <- .pchi_by_term(fit)
  ca <- car::Anova(fit, type = 2)
  for (term in rownames(ca)) {
    expect_equal(ours[[term]]$chi2, ca[term, "Chisq"], tolerance = 1e-8)
    expect_equal(ours[[term]]$df, as.numeric(ca[term, "Df"]))
    expect_equal(ours[[term]]$p, ca[term, "Pr(>Chisq)"], tolerance = 1e-8)
  }
})

test_that("lme with interaction: chi2/df match car::Anova(type = 2)", {
  skip_if_not_installed("car")
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    distance ~ age * Sex,
    data = nlme::Orthodont,
    random = ~ 1 | Subject
  )
  ours <- .pchi_by_term(fit)
  ca <- car::Anova(fit, type = 2)
  for (term in rownames(ca)) {
    expect_equal(ours[[term]]$chi2, ca[term, "Chisq"], tolerance = 1e-8)
    expect_equal(ours[[term]]$df, as.numeric(ca[term, "Df"]))
  }
})

test_that("highest-order term keeps the coefficient-block Wald chi^2 exactly", {
  fit <- .fit_lmer_inter_pchi()
  b <- lme4::fixef(fit)
  V <- as.matrix(stats::vcov(fit))
  idx <- grep(":", names(b))
  block <- as.numeric(t(b[idx]) %*% solve(V[idx, idx]) %*% b[idx])
  expect_equal(.pchi_by_term(fit)[["A:B"]]$chi2, block, tolerance = 1e-12)
})

test_that("additive lmer: every term keeps the coefficient-block Wald exactly", {
  # For additive models no term has a higher-order relative, so the
  # Type-II hypothesis matrix is the identity block and the statistic
  # must be bit-identical to the pre-Type-II block Wald.
  fit <- .fit_lmer_factor_pchi()
  b <- lme4::fixef(fit)
  V <- as.matrix(stats::vcov(fit))
  asgn <- attr(stats::model.matrix(fit), "assign")
  ours <- .pchi_by_term(fit)
  for (k in seq_along(ours)) {
    idx <- which(asgn == k)
    block <- as.numeric(t(b[idx]) %*% solve(V[idx, idx]) %*% b[idx])
    expect_identical(ours[[k]]$chi2, block)
  }
})

test_that("lmer partial_chi2 is contrast-invariant under interactions", {
  skip_if_not_installed("lme4")
  d <- .pchi_inter_lmer_data()
  fit_treat <- lme4::lmer(Reaction ~ Days + A * B + (1 | Subject), data = d)
  fit_sum <- withr::with_options(
    list(contrasts = c("contr.sum", "contr.poly")),
    lme4::lmer(Reaction ~ Days + A * B + (1 | Subject), data = d)
  )
  # For lmer the fixed effects are profiled out exactly given theta, so
  # the refit under a different coding reproduces the same model to
  # numerical precision and the Type-II statistic is invariant to 1e-8.
  ours_treat <- .pchi_by_term(fit_treat)
  ours_sum <- .pchi_by_term(fit_sum)
  for (term in names(ours_treat)) {
    expect_equal(
      ours_treat[[term]]$chi2,
      ours_sum[[term]]$chi2,
      tolerance = 1e-8
    )
    expect_identical(ours_treat[[term]]$df, ours_sum[[term]]$df)
    expect_equal(ours_treat[[term]]$p, ours_sum[[term]]$p, tolerance = 1e-8)
  }
})

test_that("glmer partial_chi2 is contrast-invariant under interactions", {
  skip_if_not_installed("lme4")
  d <- .pchi_inter_glmer_data()
  fit_treat <- lme4::glmer(y ~ x * A + (1 | g), family = binomial, data = d)
  fit_sum <- withr::with_options(
    list(contrasts = c("contr.sum", "contr.poly")),
    lme4::glmer(y ~ x * A + (1 | g), family = binomial, data = d)
  )
  # glmer re-optimizes (beta, theta) jointly under the new basis, so
  # the refit agrees only to optimizer precision (~1e-5 relative here;
  # cf. the 1e-6 IRLS bound in the glm invariance test). The exact
  # structural invariance is pinned by the car::Anova oracle above,
  # which this construction matches under BOTH codings.
  ours_treat <- .pchi_by_term(fit_treat)
  ours_sum <- .pchi_by_term(fit_sum)
  for (term in names(ours_treat)) {
    expect_equal(
      ours_treat[[term]]$chi2,
      ours_sum[[term]]$chi2,
      tolerance = 1e-3
    )
    expect_identical(ours_treat[[term]]$df, ours_sum[[term]]$df)
  }
})

test_that("glmer sum-coded refit still matches car::Anova(type = 2) exactly", {
  skip_if_not_installed("car")
  d <- .pchi_inter_glmer_data()
  fit_sum <- withr::with_options(
    list(contrasts = c("contr.sum", "contr.poly")),
    lme4::glmer(y ~ x * A + (1 | g), family = binomial, data = d)
  )
  ours <- .pchi_by_term(fit_sum)
  ca <- car::Anova(fit_sum, type = 2)
  for (term in rownames(ca)) {
    expect_equal(ours[[term]]$chi2, ca[term, "Chisq"], tolerance = 1e-8)
  }
})
