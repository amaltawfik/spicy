# Objects that reach a frame method by inheriting from a supported class
# (dev/decisions/2026-11-19-inherited-subclasses.md). The refused ones
# are refused by name, with spicy_unsupported_class, through
# table_regression() and through the frame method alike. The kept ones
# give the numbers of their own summary().

.inh_lung <- function() {
  stats::na.omit(survival::lung[, c("time", "status", "age", "inst", "sex")])
}

.expect_inherited_refusal <- function(
  fit,
  cls,
  head = sprintf("`%s` fits are not supported", cls)
) {
  expect_error(
    table_regression(fit),
    regexp = head,
    fixed = TRUE,
    class = "spicy_unsupported_class"
  )
  expect_error(as_regression_frame(fit), class = "spicy_unsupported_class")
}

# The coefficient rows of a frame against a coefficient matrix of
# summary(): estimate, standard error and p-value, by term.
.expect_frame_matches <- function(fit, sm, est, se, p) {
  fr <- as_regression_frame(fit)$coefs
  expect_identical(fr$term, rownames(sm))
  expect_equal(fr$estimate, unname(sm[, est]), tolerance = 1e-8)
  expect_equal(fr$std_error, unname(sm[, se]), tolerance = 1e-8)
  expect_equal(fr$p_value, unname(sm[, p]), tolerance = 1e-8)
  expect_s3_class(table_regression(fit), "spicy_table")
}


# ---- Refused --------------------------------------------------------------

test_that("survival::clogit() is refused", {
  skip_if_not_installed("survival")
  # clogit() calls coxph() and strata() by their bare names.
  coxph <- survival::coxph
  strata <- survival::strata
  fit <- survival::clogit(
    case ~ spontaneous + induced + strata(stratum),
    data = infert
  )
  .expect_inherited_refusal(fit, "clogit")
})

test_that("a multi-state coxph is refused", {
  skip_if_not_installed("survival")
  d <- survival::mgus2
  d$etime <- ifelse(d$pstat == 0, d$futime, d$ptime)
  d$event <- factor(
    ifelse(d$pstat == 0, 2 * d$death, 1),
    0:2,
    labels = c("censor", "pcm", "death")
  )
  fit <- survival::coxph(
    survival::Surv(etime, event) ~ age,
    data = d,
    id = id
  )
  .expect_inherited_refusal(fit, "coxphms")
})

test_that("a coxph with a pspline() or a non-sparse frailty() term is refused", {
  skip_if_not_installed("survival")
  d <- .inh_lung()
  pspline <- survival::pspline
  frailty <- survival::frailty
  .expect_inherited_refusal(
    survival::coxph(
      survival::Surv(time, status) ~ pspline(age) + sex,
      data = d
    ),
    "coxph.penal",
    head = "A `coxph` fit with the term `pspline(age)` is not supported"
  )
  fit <- suppressWarnings(survival::coxph(
    survival::Surv(time, status) ~ age + frailty(inst, sparse = FALSE),
    data = d
  ))
  # The class is not named: a sparse frailty and a ridge() term are
  # `coxph.penal` fits too, and they keep their table.
  .expect_inherited_refusal(
    fit,
    "coxph.penal",
    head = "A `coxph` fit with the term `frailty(inst, sparse = FALSE)` is not supported"
  )
})

test_that("survey::svysurvreg() is refused", {
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")
  d <- .inh_lung()
  d$w <- 1
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  fit <- survey::svysurvreg(survival::Surv(time, status) ~ age, design = des)
  .expect_inherited_refusal(fit, "svysurvreg")
})

test_that("a multivariate lm (mlm) and an aov fit are refused", {
  .expect_inherited_refusal(
    stats::lm(cbind(mpg, qsec) ~ wt + hp, data = mtcars),
    "mlm"
  )
  .expect_inherited_refusal(stats::aov(mpg ~ wt, data = mtcars), "aov")
})

test_that("nlme::nlme() and nlme::gnls() are refused", {
  skip_if_not_installed("nlme")
  fit <- nlme::nlme(
    height ~ SSasymp(age, Asym, R0, lrc),
    data = datasets::Loblolly,
    fixed = Asym + R0 + lrc ~ 1,
    random = Asym ~ 1,
    start = c(Asym = 103, R0 = -8.5, lrc = -3.3)
  )
  .expect_inherited_refusal(fit, "nlme")
  fit <- nlme::gnls(
    weight ~ SSlogis(Time, Asym, xmid, scal),
    data = nlme::Soybean,
    weights = nlme::varPower()
  )
  .expect_inherited_refusal(fit, "gnls")
})

test_that("rms::Rq() and rms::Gls() are refused", {
  skip_if_not_installed("rms")
  skip_if_not_installed("quantreg")
  skip_if_not_installed("nlme")
  .expect_inherited_refusal(rms::Rq(mpg ~ wt, data = mtcars), "Rq")
  .expect_inherited_refusal(rms::Gls(mpg ~ wt, data = mtcars), "Gls")
})

test_that("a refused object in a list of models is refused", {
  expect_error(
    table_regression(list(
      stats::lm(mpg ~ wt, data = mtcars),
      stats::aov(mpg ~ wt, data = mtcars)
    )),
    class = "spicy_unsupported_class"
  )
})


# ---- Kept -----------------------------------------------------------------

test_that("a coxph with a sparse frailty keeps its table, without RMST or risk difference", {
  skip_if_not_installed("survival")
  frailty <- survival::frailty
  fit <- survival::coxph(
    survival::Surv(time, status) ~ age + frailty(inst),
    data = .inh_lung()
  )
  sm <- summary(fit)$coefficients
  .expect_frame_matches(fit, sm["age", , drop = FALSE], "coef", "se(coef)", "p")
  expect_error(
    table_regression(
      fit,
      show_columns = c("b", "rmst"),
      tau = 300,
      boot_n = 10
    ),
    regexp = "frailty term",
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(
      fit,
      show_columns = c("b", "risk_diff"),
      at_time = 300,
      boot_n = 10
    ),
    regexp = "frailty term",
    class = "spicy_invalid_input"
  )
})

test_that("a coxph with a ridge() term keeps its table", {
  skip_if_not_installed("survival")
  ridge <- survival::ridge
  fit <- survival::coxph(
    survival::Surv(time, status) ~ ridge(age, sex, theta = 1),
    data = .inh_lung()
  )
  .expect_frame_matches(fit, summary(fit)$coefficients, "coef", "se(coef)", "p")
})

test_that("a glm with a family object from MASS or statmod keeps its table", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("statmod")
  fit <- stats::glm(
    carb ~ wt,
    family = MASS::negative.binomial(2),
    data = mtcars
  )
  .expect_frame_matches(
    fit,
    summary(fit)$coefficients,
    "Estimate",
    "Std. Error",
    "Pr(>|t|)"
  )
  fit <- stats::glm(
    mpg ~ wt,
    family = statmod::tweedie(var.power = 1.5, link.power = 0),
    data = mtcars
  )
  .expect_frame_matches(
    fit,
    summary(fit)$coefficients,
    "Estimate",
    "Std. Error",
    "Pr(>|t|)"
  )
})
