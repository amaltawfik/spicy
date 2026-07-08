# ---------------------------------------------------------------------------
# Truthfulness gate for `standardized` by model class (2026-07-04).
# ~20 classes used to accept standardized = "refit" and render the
# auto-injected beta column entirely empty. Now: hard refusal
# (spicy_unsupported_standardized) for every class without a real
# standardized-coefficients attach path; the supported classes keep
# producing populated beta rows.
# ---------------------------------------------------------------------------


# ---- Unsupported classes refuse fail-fast ---------------------------------

test_that("standardized is refused for classes without a beta path", {
  skip_if_not_installed("nnet")
  fit_mn <- nnet::multinom(employment_status ~ age + sex,
                           data = sochealth, trace = FALSE)
  err <- expect_error(
    table_regression(fit_mn, standardized = "refit"),
    class = "spicy_unsupported_standardized"
  )
  # The error names the class and the AME alternative.
  expect_match(conditionMessage(err), "multinom", fixed = TRUE)
  expect_match(conditionMessage(err), "ame", fixed = TRUE)

  skip_if_not_installed("survival")
  fit_cox <- survival::coxph(survival::Surv(time, status) ~ age + sex,
                             data = survival::lung)
  expect_error(table_regression(fit_cox, standardized = "refit"),
               class = "spicy_unsupported_standardized")

  skip_if_not_installed("MASS")
  fit_rlm <- MASS::rlm(mpg ~ wt + hp, data = mtcars)
  expect_error(table_regression(fit_rlm, standardized = "refit"),
               class = "spicy_unsupported_standardized")
})

test_that("every non-none method is gated, not just refit", {
  skip_if_not_installed("survival")
  fit <- survival::survreg(survival::Surv(time, status) ~ age,
                           data = survival::lung)
  for (m in c("refit", "posthoc", "basic", "smart")) {
    expect_error(table_regression(fit, standardized = m),
                 class = "spicy_unsupported_standardized")
  }
})

test_that("multi-model gate names the offending model", {
  skip_if_not_installed("survival")
  fit_lm  <- lm(mpg ~ wt + hp, data = mtcars)
  fit_cox <- survival::coxph(survival::Surv(time, status) ~ age,
                             data = survival::lung)
  err <- expect_error(
    table_regression(list(fit_lm, fit_cox), standardized = "refit"),
    class = "spicy_unsupported_standardized"
  )
  expect_match(conditionMessage(err), "model 2", fixed = TRUE)
  expect_match(conditionMessage(err), "coxph", fixed = TRUE)
})


# ---- Supported classes keep populated beta rows ----------------------------

test_that("supported classes still produce populated beta rows", {
  fit_lm <- lm(mpg ~ wt + hp, data = mtcars)
  long <- table_regression(fit_lm, standardized = "refit",
                           output = "long")
  b <- long[long$estimate_type == "beta", ]
  expect_identical(nrow(b), 3L)
  expect_true(all(is.finite(b$estimate[!b$is_intercept])))

  fit_glm <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  long2 <- table_regression(fit_glm, standardized = "refit",
                            output = "long")
  expect_true(any(long2$estimate_type == "beta" &
                    is.finite(long2$estimate)))

  skip_if_not_installed("MASS")
  fit_nb <- suppressWarnings(MASS::glm.nb(carb ~ hp + wt, data = mtcars))
  long3 <- table_regression(fit_nb, standardized = "refit",
                            output = "long")
  expect_true(any(long3$estimate_type == "beta" &
                    is.finite(long3$estimate)))

  skip_if_not_installed("lme4")
  fit_mer <- lme4::lmer(Reaction ~ Days + (Days | Subject),
                        data = lme4::sleepstudy)
  long4 <- table_regression(fit_mer, standardized = "refit",
                            output = "long")
  expect_true(any(long4$estimate_type == "beta" &
                    is.finite(long4$estimate)))
})

test_that("gls (no beta attach, unlike lme) is refused", {
  skip_if_not_installed("nlme")
  fit <- nlme::gls(distance ~ age, data = nlme::Orthodont)
  expect_error(table_regression(fit, standardized = "refit"),
               class = "spicy_unsupported_standardized")
})
