# exponentiate = TRUE must be honoured for EVERY model class, not only lm/glm,
# mixed, and survival. The transform is applied centrally in table_regression()
# via .apply_exp_to_frame(), driven by each frame's info$family / info$supports.
# Regression guard for the "exponentiate silently ignored for ~10 classes" bug.

# Helper: build a frame, apply the central exp transform the way the
# table_regression() orchestrator does, and return the post-exp frame.
.exp_frame <- function(fit) {
  fr <- as_regression_frame(fit, exponentiate = TRUE)
  out <- .apply_exp_to_frame(fr$coefs, fr$info, TRUE)
  list(coefs = out$coefs, info = out$info, raw = fr$coefs)
}

.b_estimates <- function(coefs) {
  keep <- coefs$estimate_type == "B" & !(coefs$is_ref %in% TRUE) &
    !is.na(coefs$estimate)
  coefs$estimate[keep]
}

test_that("exponentiate: polr (ordinal logit) -> OR, exp applied", {
  skip_if_not_installed("MASS")
  data(housing, package = "MASS")
  fit <- MASS::polr(Sat ~ Infl + Type, weights = Freq, data = housing,
                    Hess = TRUE)
  e <- .exp_frame(fit)
  expect_true(isTRUE(e$info$extras$exp_applied))
  expect_identical(e$info$extras$exp_header, "OR")
  expect_equal(.b_estimates(e$coefs), exp(.b_estimates(e$raw)),
               tolerance = 1e-6)
})

test_that("exponentiate: multinom (multinomial logit) -> OR", {
  skip_if_not_installed("nnet")
  fit <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width, data = iris,
                        trace = FALSE)
  e <- .exp_frame(fit)
  expect_true(isTRUE(e$info$extras$exp_applied))
  expect_identical(e$info$extras$exp_header, "OR")
  expect_equal(.b_estimates(e$coefs), exp(.b_estimates(e$raw)),
               tolerance = 1e-6)
})

test_that("exponentiate: MASS::glm.nb -> IRR (Negative Binomial family string)", {
  skip_if_not_installed("MASS")
  fit <- suppressWarnings(MASS::glm.nb(carb ~ hp + wt, data = mtcars))
  e <- .exp_frame(fit)
  expect_true(isTRUE(e$info$extras$exp_applied))
  expect_identical(e$info$extras$exp_header, "IRR")
})

test_that("exponentiate: mgcv gam (binomial logit) -> OR", {
  skip_if_not_installed("mgcv")
  fit <- mgcv::gam(am ~ hp + wt, family = binomial(), data = mtcars)
  e <- .exp_frame(fit)
  expect_true(isTRUE(e$info$extras$exp_applied))
  expect_identical(e$info$extras$exp_header, "OR")
  expect_equal(.b_estimates(e$coefs), exp(.b_estimates(e$raw)),
               tolerance = 1e-6)
})

test_that("exponentiate: fixest fepois (Poisson log) -> IRR", {
  skip_if_not_installed("fixest")
  fit <- fixest::fepois(carb ~ hp | cyl, data = mtcars)
  e <- .exp_frame(fit)
  expect_true(isTRUE(e$info$extras$exp_applied))
  expect_identical(e$info$extras$exp_header, "IRR")
  expect_equal(.b_estimates(e$coefs), exp(.b_estimates(e$raw)),
               tolerance = 1e-6)
})

test_that("exponentiate: betareg (logit mean link) -> OR", {
  skip_if_not_installed("betareg")
  data(GasolineYield, package = "betareg")
  fit <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
  e <- .exp_frame(fit)
  expect_true(isTRUE(e$info$extras$exp_applied))
  expect_identical(e$info$extras$exp_header, "OR")
})

test_that("exponentiate: survival coxph is NOT double-exponentiated", {
  skip_if_not_installed("survival")
  fit <- survival::coxph(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung
  )
  # coxph self-applies exp inside its method (exp_applied = TRUE); the central
  # .apply_exp_to_frame() guard must leave it untouched (no exp twice).
  e <- .exp_frame(fit)
  expect_true(isTRUE(e$info$extras$exp_applied))
  expect_identical(e$info$extras$exp_header, "HR")
  # Must equal exp(beta) once, i.e. summary()$conf.int hazard ratios.
  hr <- unname(summary(fit)$conf.int[, "exp(coef)"])
  expect_equal(sort(.b_estimates(e$coefs)), sort(hr), tolerance = 1e-6)
})

test_that("exponentiate: central guard makes a second application a no-op", {
  fit <- glm(am ~ hp, family = binomial(), data = mtcars)
  fr <- as_regression_frame(fit, exponentiate = TRUE)   # glm self-applies
  expect_true(isTRUE(fr$info$extras$exp_applied))
  before <- fr$coefs$estimate
  out <- .apply_exp_to_frame(fr$coefs, fr$info, TRUE)    # must NOT exp again
  expect_equal(out$coefs$estimate, before)
})

test_that("exponentiate: identity-link fit warns spicy_ignored_arg (post-extraction)", {
  expect_warning(
    table_regression(lm(mpg ~ wt, data = mtcars), exponentiate = TRUE,
                     output = "data.frame"),
    class = "spicy_ignored_arg"
  )
})
