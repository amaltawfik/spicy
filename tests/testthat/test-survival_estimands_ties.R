# The tie convention of the absolute survival estimands.
#
# The rule, documented in ?table_regression and in
# vignette("table-regression-survival"): THE BASELINE HAZARD OF A
# SURVIVAL ESTIMAND FOLLOWS THE `ties` CONVENTION OF THE FIT IT WAS
# COMPUTED FROM. spicy does not fit, it reads a fit the user supplies,
# so it never overrides that choice -- .coxph_baseline() calls
# survival::basehaz(), which slaves ctype to fit$method.
#
# The rule is inherited rather than coded, which is exactly why it
# needs a witness: nothing in R/ would break if the baseline were
# reimplemented as a hand Breslow, or if ctype were forced. The
# fixture is therefore heavily TIED -- on a tie-free fixture Efron and
# Breslow coincide and every assertion below is vacuous (survival's
# aeqSurv()/timefix also fuses near-equal times, so "continuous times"
# is not a guarantee of tie-freedom: the tie count is asserted, not
# assumed).
#
# Efron is the better default, not the correct answer: the
# approximations are all biased at finite sample size and agree
# asymptotically, and Efron's virtue is a much smaller small-sample
# bias (Hertz-Picciotto & Rockhill 1997; Efron 1977).

# lung with the follow-up rounded up to 30-day blocks: 228 subjects,
# 28 distinct event times, up to 16 events sharing one.
.ties_lung <- function() {
  d <- survival::lung
  d$sex <- factor(d$sex, levels = c(1, 2), labels = c("Male", "Female"))
  d <- d[stats::complete.cases(d[, c("time", "status", "age", "sex")]), ]
  d$time <- 30 * ceiling(d$time / 30)
  d
}

# Breslow's baseline cumulative hazard, by hand, at the reference-zero
# linear predictor: sum over event times <= t of (events at that time)
# / (sum of exp(lp) over the risk set). Written out rather than read
# from survival, so that it is an independent statement of what
# "Breslow" means.
.hand_breslow <- function(fit, d, grid) {
  r <- exp(stats::predict(fit, newdata = d, type = "lp", reference = "zero"))
  ev <- d$time[d$status == 2]
  ut <- sort(unique(ev))
  jumps <- vapply(
    ut,
    function(t) sum(ev == t) / sum(r[d$time >= t]),
    numeric(1)
  )
  c(0, cumsum(jumps))[findInterval(grid, ut) + 1L]
}


test_that("the tie fixture really is tied", {
  skip_if_not_installed("survival")
  d <- .ties_lung()
  ev <- d$time[d$status == 2]
  expect_identical(nrow(d), 228L)
  expect_identical(length(unique(ev)), 28L)
  expect_identical(max(table(ev)), 16L)
})


test_that("the estimand baseline follows the fit's tie convention", {
  skip_if_not_installed("survival")
  d <- .ties_lung()
  f_efron <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  f_breslow <- survival::coxph(
    survival::Surv(time, status) ~ age + sex,
    data = d,
    ties = "breslow"
  )
  expect_identical(f_efron$method, "efron")
  expect_identical(f_breslow$method, "breslow")

  b_efron <- spicy:::.coxph_baseline(f_efron)
  b_breslow <- spicy:::.coxph_baseline(f_breslow)
  expect_identical(b_efron$times, b_breslow$times)

  # A Breslow fit gives a Breslow baseline -- against the hand
  # computation, not against survival's own answer.
  expect_equal(
    unname(b_breslow$H0[, 1L]),
    unname(.hand_breslow(f_breslow, d, b_breslow$times)),
    tolerance = 1e-12
  )
  # An Efron fit does NOT. This is the assertion that would fail if
  # the baseline were reimplemented as a hand Breslow, or ctype forced
  # to 1: the two would then agree.
  gap <- max(abs(b_efron$H0[, 1L] - .hand_breslow(f_efron, d, b_efron$times)))
  expect_gt(gap, 1e-2)
  # And the two baselines are materially apart from each other.
  expect_gt(max(abs(b_efron$H0[, 1L] - b_breslow$H0[, 1L])), 1e-3)

  # The documented equality: the estimand baseline is basehaz()'s, so
  # the convention is survfit()'s, not one spicy invents.
  bh <- survival::basehaz(f_breslow, centered = FALSE)
  expect_identical(b_breslow$times, bh$time)
  expect_identical(unname(b_breslow$H0[, 1L]), bh$hazard)
})


test_that("the estimands themselves carry the fit's tie convention", {
  skip_if_not_installed("survival")
  d <- .ties_lung()
  pts_of <- function(fit) {
    spicy:::.coxph_estimand_points(
      fit,
      spicy:::.coxph_estimand_data(fit),
      want_rmst = TRUE,
      want_risk = TRUE,
      tau = 360,
      at_time = 360
    )
  }
  p_efron <- pts_of(
    survival::coxph(survival::Surv(time, status) ~ age + sex, data = d)
  )
  p_breslow <- pts_of(survival::coxph(
    survival::Surv(time, status) ~ age + sex,
    data = d,
    ties = "breslow"
  ))
  expect_identical(p_efron$term, p_breslow$term)

  # Pinned separation on this fixture (spicy's own values, so a
  # regression pin rather than an oracle -- what they defend is that
  # the two conventions do not silently converge).
  i <- which(p_efron$term == "sexFemale")
  expect_equal(p_efron$rmst[i], 35.724867, tolerance = 1e-6)
  expect_equal(p_breslow$rmst[i], 34.410354, tolerance = 1e-6)
  expect_equal(p_efron$risk[i], -0.184342191, tolerance = 1e-6)
  expect_equal(p_breslow$risk[i], -0.179136871, tolerance = 1e-6)
  # Over a day of restricted mean survival between the conventions:
  # not a rounding difference, and a user reproducing a Stata table
  # would see it.
  expect_gt(abs(p_efron$rmst[i] - p_breslow$rmst[i]), 1)
})
