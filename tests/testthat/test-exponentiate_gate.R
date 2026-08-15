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
  fit <- betareg::betareg(yield ~ temp, data = GasolineYield, link = "cloglog")
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
  expect_match(
    conditionMessage(err),
    "reversed response ordering",
    fixed = TRUE
  )
  # No naive "refit with cloglog" advice (a different model, not a
  # relabel).
  expect_no_match(conditionMessage(err), "refit with", ignore.case = TRUE)
})

test_that("gate errors: glmer probit (mixed path)", {
  skip_if_not_installed("lme4")
  fit <- suppressMessages(lme4::glmer(
    am ~ mpg + (1 | gear),
    data = mtcars,
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
    table_regression(list(Logit = logit, Probit = probit), exponentiate = TRUE)
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
  expect_true(
    "exp(B)" %in%
      names(table_regression(f_log, exponentiate = TRUE))
  )
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
    data = survival::lung,
    dist = "weibull"
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
  fit <- pscl::zeroinfl(art ~ fem | ment, data = bioChemists, link = "probit")
  out <- table_regression(fit, exponentiate = TRUE)
  expect_true("IRR" %in% names(out))
})

# ---- flexsurv: dist-aware location link + anc guard (audit follow-up) ------

test_that("flexsurvspline(scale = 'normal') is refused by the gate", {
  skip_if_not_installed("flexsurv")
  library(survival)
  fit <- flexsurv::flexsurvspline(
    Surv(futime, fustat) ~ age,
    data = ovarian,
    k = 1,
    scale = "normal"
  )
  err <- .gate_err(table_regression(fit, exponentiate = TRUE))
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "probit", fixed = TRUE)
  # without exponentiate: renders fine
  expect_s3_class(table_regression(fit), "spicy_regression_table")
})

test_that("flexsurvspline hazard/odds scales and built-in dists still exponentiate", {
  skip_if_not_installed("flexsurv")
  library(survival)
  f_h <- flexsurv::flexsurvspline(
    Surv(futime, fustat) ~ age,
    data = ovarian,
    k = 1,
    scale = "hazard"
  )
  expect_s3_class(
    table_regression(f_h, exponentiate = TRUE),
    "spicy_regression_table"
  )
  f_w <- flexsurv::flexsurvreg(
    Surv(futime, fustat) ~ age,
    data = ovarian,
    dist = "weibull"
  )
  expect_s3_class(
    table_regression(f_w, exponentiate = TRUE),
    "spicy_regression_table"
  )
})

test_that("flexsurv anc covariates + exponentiate are refused (identity-scale rows)", {
  skip_if_not_installed("flexsurv")
  library(survival)
  # Was: the Gompertz shape(rx) row exponentiated to '1.00 [1.00, 1.00]'.
  fit <- flexsurv::flexsurvreg(
    Surv(futime, fustat) ~ age,
    anc = list(shape = ~rx),
    data = ovarian,
    dist = "gompertz"
  )
  err <- .gate_err(table_regression(fit, exponentiate = TRUE))
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "ancillary", fixed = TRUE)
  expect_s3_class(table_regression(fit), "spicy_regression_table")
})

test_that("flexsurv factor predictors group with a reference row (xlevels fix)", {
  skip_if_not_installed("flexsurv")
  library(survival)
  d <- na.omit(lung[, c("time", "status", "age", "sex")])
  d$sex <- factor(d$sex, levels = 1:2, labels = c("Male", "Female"))
  fit <- flexsurv::flexsurvreg(
    Surv(time, status) ~ age + sex,
    data = d,
    dist = "weibull"
  )
  fr <- as_regression_frame(fit)
  cf <- fr$coefs
  # Grouped: the contrast row carries parent_var/label, and the dropped
  # reference level is synthesised as an is_ref row.
  ref <- cf[cf$is_ref, ]
  expect_identical(nrow(ref), 1L)
  expect_identical(ref$parent_var, "sex")
  expect_identical(ref$label, "Male")
  con <- cf[cf$term == "sexFemale", ]
  expect_identical(con$parent_var, "sex")
  expect_identical(con$label, "Female")
})


# Phase 3 matrix – vignettes-news:ordinal-exponentiate-links (lot T4)

test_that("ordinal exponentiate: cloglog reads HR, probit is refused, thresholds stay log-scale", {
  skip_if_not_installed("MASS")
  hh <- MASS::housing
  # cloglog link: the cumulative model is the grouped-time
  # proportional-hazards model, so the header reads HR.
  f_cll <- suppressMessages(MASS::polr(
    Sat ~ Infl,
    weights = Freq,
    data = hh,
    method = "cloglog",
    Hess = TRUE
  ))
  tbl <- table_regression(f_cll, exponentiate = TRUE)
  expect_true("HR" %in% names(tbl))
  # Thresholds rows are never exponentiated, and the footer flags the
  # scale under exponentiation.
  expect_match(
    paste(attr(tbl, "note"), collapse = "\n"),
    "not exponentiated",
    fixed = TRUE
  )
  s <- as_structured(tbl)
  thr <- s$body[grepl("|", s$body$Variable, fixed = TRUE), , drop = FALSE]
  expect_gt(nrow(thr), 0L)
  expect_equal(
    sort(thr$HR),
    sort(unname(f_cll$zeta)),
    tolerance = 1e-6
  )
  # probit link: refused with the classed gate error.
  f_pro <- suppressMessages(MASS::polr(
    Sat ~ Infl,
    weights = Freq,
    data = hh,
    method = "probit",
    Hess = TRUE
  ))
  expect_error(
    table_regression(f_pro, exponentiate = TRUE),
    class = "spicy_invalid_input"
  )
  # clm probit: same refusal through the ordinal package's engine.
  skip_if_not_installed("ordinal")
  d <- data.frame(
    y = MASS::housing$Sat[rep(
      seq_len(nrow(hh)),
      hh$Freq
    )]
  )
  d$x <- rnorm(nrow(d))
  f_clm <- ordinal::clm(y ~ x, data = d, link = "probit")
  expect_error(
    table_regression(f_clm, exponentiate = TRUE),
    class = "spicy_invalid_input"
  )
})

# Vignette-excellence campaign (wave 1): the cumulative-cloglog HR is the
# grouped-time proportional-hazards ratio exp(-B), NOT exp(B). Under the
# polr / clm parametrisation cloglog P(Y <= j) = zeta_j - xB the hazard
# sits on -B; exp(B) is the reciprocal of the HR. Direction pinned against
# a person-period discrete-time cloglog GLM oracle (2026-08-05).

test_that("cumulative cloglog HR is exp(-B): direction, delta SE, swapped CI", {
  skip_if_not_installed("MASS")
  # Grouped survival with known hazard ratio 2 for x = 1: the displayed
  # HR must land near 2 (the pre-fix exp(B) printed ~0.5).
  set.seed(7)
  n <- 4000
  x <- rbinom(n, 1, 0.5)
  tcont <- rexp(n, rate = 0.1 * exp(log(2) * x))
  d <- data.frame(
    y = cut(tcont, c(0, 3, 6, 10, 15, Inf), ordered_result = TRUE),
    x = x
  )
  fit <- suppressMessages(
    MASS::polr(y ~ x, data = d, method = "cloglog", Hess = TRUE)
  )
  t_exp <- broom::tidy(table_regression(fit, exponentiate = TRUE))
  t_raw <- broom::tidy(table_regression(fit))
  bx <- t_exp[t_exp$term == "x" & t_exp$estimate_type == "B", ]
  br <- t_raw[t_raw$term == "x" & t_raw$estimate_type == "B", ]
  # Point estimate: exp(-B), near the true HR of 2.
  expect_equal(bx$estimate, exp(-br$estimate), tolerance = 1e-10)
  expect_gt(bx$estimate, 1.5)
  # Delta-method SE on the displayed scale; CI endpoints negated + swapped.
  expect_equal(
    bx$std.error,
    exp(-br$estimate) * br$std.error,
    tolerance = 1e-10
  )
  expect_equal(bx$conf.low, exp(-br$conf.high), tolerance = 1e-10)
  expect_equal(bx$conf.high, exp(-br$conf.low), tolerance = 1e-10)
  # p-value invariant under the monotone transform.
  expect_equal(bx$p.value, br$p.value, tolerance = 1e-12)
  # The footer discloses the sign convention next to the HR definition.
  note <- paste(
    attr(table_regression(fit, exponentiate = TRUE), "note"),
    collapse = "\n"
  )
  expect_match(note, "exp(-B)", fixed = TRUE)
  expect_match(note, "Prentice", fixed = TRUE)
})

test_that("clm cloglog negates too; logit and binomial cloglog stay exp(+B)", {
  skip_if_not_installed("ordinal")
  fit_cll <- ordinal::clm(
    self_rated_health ~ age + smoking,
    data = sochealth,
    link = "cloglog"
  )
  t_exp <- broom::tidy(table_regression(fit_cll, exponentiate = TRUE))
  # coef(clm) includes the alpha thresholds (raw, never exponentiated);
  # the negation check targets the true beta terms only.
  bb <- t_exp[
    t_exp$estimate_type == "B" & t_exp$term %in% names(fit_cll$beta),
  ]
  expect_equal(
    bb$estimate,
    unname(exp(-fit_cll$beta[bb$term])),
    tolerance = 1e-10
  )
  # Regression guards: the negation is cloglog-cumulative ONLY.
  fit_logit <- ordinal::clm(self_rated_health ~ age, data = sochealth)
  t_or <- broom::tidy(table_regression(fit_logit, exponentiate = TRUE))
  b_or <- t_or[t_or$term == "age" & t_or$estimate_type == "B", ]
  expect_equal(
    b_or$estimate,
    unname(exp(coef(fit_logit)["age"])),
    tolerance = 1e-10
  )
  # Binomial cloglog (person-period style): exp(+B) IS the HR, unchanged.
  fit_glm <- glm(am ~ mpg, data = mtcars, family = binomial("cloglog"))
  t_glm <- broom::tidy(table_regression(fit_glm, exponentiate = TRUE))
  b_glm <- t_glm[t_glm$term == "mpg" & t_glm$estimate_type == "B", ]
  expect_equal(
    b_glm$estimate,
    unname(exp(coef(fit_glm)["mpg"])),
    tolerance = 1e-10
  )
  no_note <- paste(
    attr(table_regression(fit_glm, exponentiate = TRUE), "note"),
    collapse = "\n"
  )
  expect_false(grepl("exp(-B)", no_note, fixed = TRUE))
})

test_that("cloglog clm with nominal terms refuses exponentiate", {
  skip_if_not_installed("ordinal")
  fit <- ordinal::clm(
    self_rated_health ~ age,
    nominal = ~smoking,
    data = sochealth,
    link = "cloglog"
  )
  err <- .gate_err(table_regression(fit, exponentiate = TRUE))
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "nominal", fixed = TRUE)
})
