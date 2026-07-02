# G1 exponentiate link gate (Group D): exponentiate = TRUE hard-errors
# (spicy_invalid_input) on links whose exponentiated coefficient has no
# ratio estimand. Ratio set: logit (OR), log (IRR / RR / MR / exp(B)),
# and cloglog for binomial / quasibinomial / cumulative families only
# (HR; Prentice & Gloeckler 1978). Identity keeps the warn + no-op.

.gate_err <- function(expr) {
  tryCatch(expr, spicy_invalid_input = function(e) e)
}

test_that("gate errors: glm cauchit", {
  fit <- glm(am ~ mpg, data = mtcars, family = binomial(link = "cauchit"))
  err <- .gate_err(table_regression(fit, exponentiate = TRUE))
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "cauchit", fixed = TRUE)
  expect_match(conditionMessage(err), "Cauchy", fixed = TRUE)
})

test_that("gate errors: Gamma() default inverse link, message points at log link", {
  fit <- glm(mpg ~ wt, data = mtcars, family = Gamma())
  err <- .gate_err(table_regression(fit, exponentiate = TRUE))
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), '"inverse"', fixed = TRUE)
  expect_match(conditionMessage(err), 'Gamma(link = "log")', fixed = TRUE)
})

test_that("gate errors: inverse.gaussian() default 1/mu^2 link", {
  set.seed(1)
  d <- data.frame(y = rgamma(100, 3, 0.5) + 0.1, x = rnorm(100))
  fit <- glm(y ~ x, data = d, family = inverse.gaussian())
  err <- .gate_err(table_regression(fit, exponentiate = TRUE))
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "1/mu^2", fixed = TRUE)
})

test_that("gate errors: poisson(sqrt)", {
  set.seed(2)
  d <- data.frame(y = rpois(100, 4), x = rnorm(100))
  fit <- glm(y ~ x, data = d, family = poisson(link = "sqrt"))
  err <- .gate_err(table_regression(fit, exponentiate = TRUE))
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "square-root", fixed = TRUE)
})

test_that("gate is family-aware for cloglog: betareg cloglog errors", {
  skip_if_not_installed("betareg")
  # A beta-mean cloglog coefficient is NOT a log hazard ratio -- the
  # grouped-time PH reading exists only for binomial-type families. At
  # HEAD this silently exponentiated to exp(B).
  data("GasolineYield", package = "betareg")
  fit <- betareg::betareg(yield ~ temp, data = GasolineYield,
                          link = "cloglog")
  err <- .gate_err(table_regression(fit, exponentiate = TRUE))
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "beta", fixed = TRUE)
})

test_that("gate errors: ordinal clm loglog (no misleading refit hint)", {
  skip_if_not_installed("ordinal")
  d <- data.frame(
    y = factor(rep(1:3, length.out = 60), ordered = TRUE),
    x = rnorm(60)
  )
  fit <- ordinal::clm(y ~ x, data = d, link = "loglog")
  err <- .gate_err(table_regression(fit, exponentiate = TRUE))
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "reversed response ordering",
               fixed = TRUE)
  # No naive "refit with cloglog" advice (a different model, not a
  # relabel).
  expect_no_match(conditionMessage(err), "refit with", ignore.case = TRUE)
})

test_that("gate errors: glmer probit (mixed path)", {
  skip_if_not_installed("lme4")
  fit <- suppressMessages(lme4::glmer(
    am ~ mpg + (1 | gear), data = mtcars,
    family = binomial(link = "probit")
  ))
  err <- .gate_err(table_regression(fit, exponentiate = TRUE))
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "probit", fixed = TRUE)
})

test_that("multi-model gate names the offending model", {
  logit <- glm(am ~ mpg, data = mtcars, family = binomial())
  probit <- glm(am ~ mpg, data = mtcars, family = binomial("probit"))
  err <- .gate_err(
    table_regression(list(Logit = logit, Probit = probit),
                     exponentiate = TRUE)
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "Probit", fixed = TRUE)
  expect_match(conditionMessage(err), "every model", fixed = TRUE)
})

# ---- Allowed links keep working (regression guard) ------------------------

test_that("ratio links stay green: logit OR, cloglog HR, gaussian-log exp(B)", {
  f_or <- glm(am ~ mpg, data = mtcars, family = binomial())
  expect_true("OR" %in% names(table_regression(f_or, exponentiate = TRUE)))

  f_hr <- glm(am ~ mpg, data = mtcars, family = binomial("cloglog"))
  expect_true("HR" %in% names(table_regression(f_hr, exponentiate = TRUE)))

  f_log <- glm(mpg ~ wt, data = mtcars, family = gaussian(link = "log"))
  expect_true("exp(B)" %in%
                names(table_regression(f_log, exponentiate = TRUE)))
})

test_that("identity links keep the warn + no-op; mixed lm + logit renders quietly", {
  f_lm <- lm(mpg ~ wt, data = mtcars)
  f_gl <- glm(am ~ mpg, data = mtcars, family = binomial())
  # Pure identity table: the argument had zero effect -> consolidated
  # spicy_ignored_arg warning.
  expect_warning(
    table_regression(f_lm, exponentiate = TRUE),
    class = "spicy_ignored_arg"
  )
  # Mixed lm + logit: exp applied to the logit model, lm untouched --
  # no warning, no error (the identity request is satisfied vacuously).
  expect_no_warning(
    out <- table_regression(list(f_lm, f_gl), exponentiate = TRUE)
  )
  expect_s3_class(out, "spicy_regression_table")
})

test_that("survreg time ratios are untouched by the gate", {
  skip_if_not_installed("survival")
  fit <- survival::survreg(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung, dist = "weibull"
  )
  out <- table_regression(fit, exponentiate = TRUE)
  expect_true("TR" %in% names(out))
})

test_that("component blocks stay stricter than the main gate (zeroinfl probit zero part)", {
  skip_if_not_installed("pscl")
  # The count part (log link) exponentiates to IRR; the probit ZERO part
  # stays on the link scale via the component-level logit-only gate --
  # the main G1 gate must not fire (count link is log).
  data("bioChemists", package = "pscl")
  fit <- pscl::zeroinfl(art ~ fem | ment, data = bioChemists,
                        link = "probit")
  out <- table_regression(fit, exponentiate = TRUE)
  expect_true("IRR" %in% names(out))
})

# ---- flexsurv: dist-aware location link + anc guard (audit follow-up) ------

test_that("flexsurvspline(scale = 'normal') is refused by the gate", {
  skip_if_not_installed("flexsurv")
  library(survival)
  fit <- flexsurv::flexsurvspline(Surv(futime, fustat) ~ age,
                                  data = ovarian, k = 1, scale = "normal")
  err <- .gate_err(table_regression(fit, exponentiate = TRUE))
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "probit", fixed = TRUE)
  # without exponentiate: renders fine
  expect_s3_class(table_regression(fit), "spicy_regression_table")
})

test_that("flexsurvspline hazard/odds scales and built-in dists still exponentiate", {
  skip_if_not_installed("flexsurv")
  library(survival)
  f_h <- flexsurv::flexsurvspline(Surv(futime, fustat) ~ age,
                                  data = ovarian, k = 1, scale = "hazard")
  expect_s3_class(table_regression(f_h, exponentiate = TRUE),
                  "spicy_regression_table")
  f_w <- flexsurv::flexsurvreg(Surv(futime, fustat) ~ age,
                               data = ovarian, dist = "weibull")
  expect_s3_class(table_regression(f_w, exponentiate = TRUE),
                  "spicy_regression_table")
})

test_that("flexsurv anc covariates + exponentiate are refused (identity-scale rows)", {
  skip_if_not_installed("flexsurv")
  library(survival)
  # Was: the Gompertz shape(rx) row exponentiated to '1.00 [1.00, 1.00]'.
  fit <- flexsurv::flexsurvreg(Surv(futime, fustat) ~ age,
                               anc = list(shape = ~ rx),
                               data = ovarian, dist = "gompertz")
  err <- .gate_err(table_regression(fit, exponentiate = TRUE))
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "ancillary", fixed = TRUE)
  expect_s3_class(table_regression(fit), "spicy_regression_table")
})
