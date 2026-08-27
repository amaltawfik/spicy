# ---------------------------------------------------------------------------
# `nested = TRUE` over an rms hierarchy (register n. 244(d)).
#
# rms fits fit neither generic path: anova.rms is a single-fit Wald test,
# so lrm / cph hard-errored on the likelihood route ("an anova method
# exists and it refused") while ols, which carries "lm", rode the
# least-squares route and came back all-NA. They now compare through
# rms::lrtest(), which is also the ORACLE these tests hold spicy to.
# ---------------------------------------------------------------------------

rms_pairs <- function() {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  set.seed(20260827)
  n <- 240L
  d <- data.frame(x1 = stats::rnorm(n), x2 = stats::rnorm(n))
  d$yc <- 1 + 0.5 * d$x1 + 0.4 * d$x2 + stats::rnorm(n)
  d$yb <- stats::rbinom(n, 1L, stats::plogis(0.4 * d$x1 + 0.6 * d$x2))
  d$time <- stats::rexp(n, exp(0.2 * d$x1 + 0.3 * d$x2))
  d$event <- stats::rbinom(n, 1L, 0.7)
  list(
    data = d,
    ols1 = rms::ols(yc ~ x1, data = d),
    ols2 = rms::ols(yc ~ x1 + x2, data = d),
    lrm1 = rms::lrm(yb ~ x1, data = d),
    lrm2 = rms::lrm(yb ~ x1 + x2, data = d),
    cph1 = rms::cph(survival::Surv(time, event) ~ x1, data = d),
    cph2 = rms::cph(survival::Surv(time, event) ~ x1 + x2, data = d)
  )
}

# The p-value oracle, computed INDEPENDENTLY of rms. rms::lrtest() ends
# with `1 - pchisq(chisq, dof)`, which cancels to exactly 0 below
# p ~ 1.1e-16, so pinning spicy's p against rms's P compares two zeros
# and asserts nothing on precisely the hierarchies that matter. spicy
# keeps rms's chi-square and degrees of freedom -- those are exact --
# and recomputes the tail here, which is what this function reproduces.
rms_p_oracle <- function(stats) {
  stats::pchisq(
    unname(stats[["L.R. Chisq"]]),
    df = unname(stats[["d.f."]]),
    lower.tail = FALSE
  )
}


# ---- 1. The oracle: rms::lrtest(), to the digit -------------------------

test_that("lrm pair matches rms::lrtest() exactly", {
  p <- rms_pairs()
  got <- compute_nested_comparisons(list(p$lrm1, p$lrm2))
  oracle <- rms::lrtest(p$lrm1, p$lrm2)$stats
  expect_equal(
    got$lrt_change[1L],
    unname(oracle[["L.R. Chisq"]]),
    tolerance = 1e-12
  )
  expect_equal(got$p_change[1L], rms_p_oracle(oracle), tolerance = 1e-12)
  # Documented for the next reader: on lrm the generic logLik fallback
  # already produced these same numbers. The reroute is about REACHING a
  # comparison at all (see test 2), not about changing one.
  fb <- spicy:::loglik_lrt(p$lrm1, p$lrm2)
  expect_equal(fb$stat, unname(oracle[["L.R. Chisq"]]), tolerance = 1e-8)
  expect_equal(
    unname(oracle[["d.f."]]),
    spicy:::loglik_df_increase(p$lrm1, p$lrm2)
  )
})

test_that("cph pair matches rms::lrtest() exactly", {
  p <- rms_pairs()
  got <- compute_nested_comparisons(list(p$cph1, p$cph2))
  oracle <- rms::lrtest(p$cph1, p$cph2)$stats
  expect_equal(
    got$lrt_change[1L],
    unname(oracle[["L.R. Chisq"]]),
    tolerance = 1e-12
  )
  expect_equal(got$p_change[1L], rms_p_oracle(oracle), tolerance = 1e-12)
  fb <- spicy:::loglik_lrt(p$cph1, p$cph2)
  expect_equal(fb$stat, unname(oracle[["L.R. Chisq"]]), tolerance = 1e-8)
})

test_that("ols pair matches rms::lrtest() exactly -- the real gain", {
  p <- rms_pairs()
  # The baseline this replaces: anova.rms refuses a two-model call, so
  # the least-squares route produced nothing at all.
  expect_error(stats::anova(p$ols1, p$ols2))
  got <- compute_nested_comparisons(list(p$ols1, p$ols2))
  oracle <- rms::lrtest(p$ols1, p$ols2)$stats
  expect_equal(
    got$lrt_change[1L],
    unname(oracle[["L.R. Chisq"]]),
    tolerance = 1e-12
  )
  expect_equal(got$p_change[1L], rms_p_oracle(oracle), tolerance = 1e-12)
  expect_true(is.finite(got$aic_change[1L]))
  expect_equal(
    got$aic_change[1L],
    unname(stats::AIC(p$ols2) - stats::AIC(p$ols1)),
    tolerance = 1e-12
  )
})


# ---- 2. What the routing changed at the table level ---------------------

test_that("an lrm hierarchy renders instead of aborting", {
  p <- rms_pairs()
  out <- expect_no_error(
    table_regression(list(p$lrm1, p$lrm2), nested = TRUE, output = "data.frame")
  )
  expect_s3_class(out, "data.frame")
})

test_that("an ols hierarchy reports the LRT it now has", {
  p <- rms_pairs()
  expect_identical(
    default_nested_tokens(list(p$ols1, p$ols2)),
    c("lrt_change", "p_change")
  )
  expect_true(all_likelihood_path(list(p$ols1, p$ols2)))
  txt <- paste(
    capture.output(table_regression(list(p$ols1, p$ols2), nested = TRUE)),
    collapse = "\n"
  )
  oracle <- rms::lrtest(p$ols1, p$ols2)$stats
  expect_match(
    txt,
    formatC(unname(oracle[["L.R. Chisq"]]), format = "f", digits = 2),
    fixed = TRUE
  )
})

# The class-aware default and the token validator read ONE predicate, so
# a variance-explained change token on an ols hierarchy is refused with
# the reason rather than rendered as an empty row.
test_that("ols refuses the variance-explained change tokens", {
  p <- rms_pairs()
  err <- expect_error(
    table_regression(
      list(p$ols1, p$ols2),
      nested = TRUE,
      show_fit_stats = c("nobs", "r2_change")
    ),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "r2_change", fixed = TRUE)
  expect_match(conditionMessage(err), "likelihood-ratio", fixed = TRUE)
  # The headline names the ROUTE, not a property of the class. rms::ols
  # IS ordinary least squares and carries a real R-squared -- identical
  # to lm's on the same data -- so "not defined for `ols` models" was
  # simply false.
  expect_match(
    conditionMessage(err),
    "spicy compares a `ols` hierarchy through the likelihood-ratio",
    fixed = TRUE
  )
  expect_false(grepl("are not defined for", conditionMessage(err)))
  expect_equal(
    unname(p$ols2$stats[["R2"]]),
    summary(stats::lm(yc ~ x1 + x2, data = p$data))$r.squared,
    tolerance = 1e-12
  )
})


# ---- 3. The refusals, and the direction rms's abs() erases --------------

test_that("a non-nested rms pair relays rms's own refusal, cleanly", {
  p <- rms_pairs()
  other <- rms::ols(yc ~ x2, data = p$data)
  err <- expect_error(
    compute_nested_comparisons(list(p$ols1, other)),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "rms::lrtest()", fixed = TRUE)
  expect_match(conditionMessage(err), "models are not nested", fixed = TRUE)
  # No spicy-internal name reaches the reader (register n. 246(f)).
  expect_false(grepl("fit_curr|fit_prev", conditionMessage(err)))
})

test_that("a reversed pair is refused a statistic, not handed a positive one", {
  p <- rms_pairs()
  # rms::lrtest() takes abs() of both quantities, so it answers the same
  # positive chi-square either way round -- the trap this guards.
  expect_equal(
    unname(rms::lrtest(p$lrm2, p$lrm1)$stats[["L.R. Chisq"]]),
    unname(rms::lrtest(p$lrm1, p$lrm2)$stats[["L.R. Chisq"]]),
    tolerance = 1e-12
  )
  got <- compute_nested_comparisons(list(p$lrm2, p$lrm1))
  expect_true(is.na(got$lrt_change[1L]))
  expect_true(is.na(got$p_change[1L]))
})

# The direction rms's abs() erases is not only the reversed pair. A
# hierarchy that REPLACES a predictor instead of adding one gains
# parameters while fitting WORSE: the parameter count rises, so a guard
# that reads only the count passes it, and abs() then publishes the
# degradation as a highly significant improvement. The signed likelihood
# difference is what tells the two apart.
rms_replaced_predictor_data <- function() {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  set.seed(20260828)
  n <- 400L
  d <- data.frame(
    x1 = stats::rnorm(n),
    n1 = stats::rnorm(n),
    n2 = stats::rnorm(n)
  )
  d$yb <- stats::rbinom(n, 1L, stats::plogis(-0.3 + 0.9 * d$x1))
  d$time <- stats::rexp(n, exp(-2 + 0.9 * d$x1))
  d$event <- stats::rbinom(n, 1L, 0.8)
  d
}

test_that("a larger model that fits WORSE gets no chi-square (lrm)", {
  d <- rms_replaced_predictor_data()
  small <- rms::lrm(yb ~ x1, data = d)
  bigger <- rms::lrm(yb ~ n1 + n2, data = d)
  # The setup, spelled out: MORE parameters, LOWER likelihood -- the
  # shape a parameter-count guard cannot see.
  expect_gt(spicy:::loglik_df_increase(small, bigger), 0)
  signed <- spicy:::rms_signed_lr_change(small, bigger)
  expect_lt(signed, 0)
  # rms answers the ABSOLUTE difference, positive and significant.
  oracle <- rms::lrtest(small, bigger)$stats
  expect_equal(unname(oracle[["L.R. Chisq"]]), abs(signed), tolerance = 1e-8)
  expect_lt(rms_p_oracle(oracle), 0.001)
  got <- compute_nested_comparisons(list(small, bigger))
  expect_true(is.na(got$lrt_change[1L]))
  expect_true(is.na(got$p_change[1L]))
  # DeltaAIC is still reported, and still says the model got worse.
  expect_gt(got$aic_change[1L], 0)
})

test_that("a larger model that fits WORSE gets no chi-square (cph)", {
  d <- rms_replaced_predictor_data()
  small <- rms::cph(survival::Surv(time, event) ~ x1, data = d)
  bigger <- rms::cph(survival::Surv(time, event) ~ n1 + n2, data = d)
  expect_gt(spicy:::loglik_df_increase(small, bigger), 0)
  expect_lt(spicy:::rms_signed_lr_change(small, bigger), 0)
  got <- compute_nested_comparisons(list(small, bigger))
  expect_true(is.na(got$lrt_change[1L]))
  expect_true(is.na(got$p_change[1L]))
})

# rms ends lrtest() with `1 - pchisq(chisq, dof)`. spicy keeps its
# chi-square and its degrees of freedom and recomputes the tail, so a
# strong hierarchy reports a p-value instead of the zero the subtraction
# cancels to.
test_that("p_change is recomputed, not relayed, and never underflows to 0", {
  skip_if_not_installed("rms")
  set.seed(20260827)
  n <- 600L
  d <- data.frame(z1 = stats::rnorm(n), z2 = stats::rnorm(n))
  d$y <- stats::rbinom(
    n,
    1L,
    stats::plogis(-0.2 + 2.5 * d$z1 + 1.8 * d$z2)
  )
  q1 <- rms::lrm(y ~ z1, data = d)
  q2 <- rms::lrm(y ~ z1 + z2, data = d)
  oracle <- rms::lrtest(q1, q2)$stats
  # rms's own P has cancelled to exactly zero here.
  expect_identical(unname(oracle[["P"]]), 0)
  got <- compute_nested_comparisons(list(q1, q2))
  expect_gt(got$p_change[1L], 0)
  expect_equal(got$p_change[1L], rms_p_oracle(oracle), tolerance = 1e-12)
  # The chi-square is untouched: rms's, to the last digit.
  expect_identical(got$lrt_change[1L], unname(oracle[["L.R. Chisq"]]))
})

test_that("mismatched sample sizes return the all-NA contract", {
  p <- rms_pairs()
  d2 <- p$data[-(1:20), ]
  short <- rms::ols(yc ~ x1 + x2, data = d2)
  got <- spicy:::compute_one_pair_rms(p$ols1, short)
  expect_true(is.na(got$lrt_change))
  expect_true(is.na(got$p_change))
})


# ---- 4. The guard on rms itself -----------------------------------------

test_that("without rms the pair falls back to the generic likelihood route", {
  p <- rms_pairs()
  testthat::local_mocked_bindings(
    spicy_pkg_available = function(pkg) FALSE
  )
  got <- spicy:::rms_lrtest_stats(p$lrm1, p$lrm2)
  oracle <- rms::lrtest(p$lrm1, p$lrm2)$stats
  expect_equal(got$stat, unname(oracle[["L.R. Chisq"]]), tolerance = 1e-8)
  expect_equal(got$p, unname(oracle[["P"]]), tolerance = 1e-8)
})

test_that("the fallback returns NA when no likelihood route exists either", {
  p <- rms_pairs()
  testthat::local_mocked_bindings(
    spicy_pkg_available = function(pkg) FALSE
  )
  # Reversed: loglik_lrt() refuses a non-positive parameter increase and
  # returns NULL, which must surface as NA rather than an error.
  got <- spicy:::rms_lrtest_stats(p$lrm2, p$lrm1)
  expect_identical(got$stat, NA_real_)
  expect_identical(got$p, NA_real_)
})


# ---- 5. The rest of the family, and the pair that is NOT all-rms --------

# psm, orm and Glm route here too, and went from a hard refusal to a
# number. Each is held to the same two properties as ols / lrm / cph:
# the passing direction is rms's chi-square with the tail recomputed,
# and the reversed direction is refused a statistic.
test_that("psm, orm and Glm route through rms::lrtest() both ways round", {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  p <- rms_pairs()
  d <- p$data
  fams <- list(
    psm = list(
      rms::psm(
        survival::Surv(time, event) ~ x1,
        data = d,
        dist = "weibull"
      ),
      rms::psm(
        survival::Surv(time, event) ~ x1 + x2,
        data = d,
        dist = "weibull"
      )
    ),
    orm = list(
      rms::orm(yc ~ x1, data = d),
      rms::orm(yc ~ x1 + x2, data = d)
    ),
    Glm = list(
      rms::Glm(yb ~ x1, data = d, family = stats::binomial()),
      rms::Glm(yb ~ x1 + x2, data = d, family = stats::binomial())
    )
  )
  for (nm in names(fams)) {
    fits <- fams[[nm]]
    got <- suppressWarnings(compute_nested_comparisons(fits))
    oracle <- rms::lrtest(fits[[1L]], fits[[2L]])$stats
    expect_equal(
      got$lrt_change[1L],
      unname(oracle[["L.R. Chisq"]]),
      tolerance = 1e-12,
      info = nm
    )
    expect_equal(
      got$p_change[1L],
      rms_p_oracle(oracle),
      tolerance = 1e-12,
      info = nm
    )
    rev_got <- suppressWarnings(compute_nested_comparisons(rev(fits)))
    expect_true(is.na(rev_got$lrt_change[1L]), info = nm)
    expect_true(is.na(rev_got$p_change[1L]), info = nm)
  }
})

# A hierarchy with ONE rms fit is not an rms pair: rms::lrtest() would
# read a non-rms fit through its generic lr() / np() fallbacks and
# answer a number built from two different scales (measured on this
# pair: 308.05 where the honest likelihood difference is 12.13). The
# pair goes to the generic route instead, which relays anova.rms's own
# refusal -- and relays it WITHOUT the spicy-local name that method
# builds its sentence from (register n. 246(f), on the public path).
test_that("a hierarchy with only one rms fit is not routed to rms", {
  p <- rms_pairs()
  other <- stats::glm(
    yb ~ x1 + x2,
    data = p$data,
    family = stats::binomial()
  )
  expect_false(spicy:::is_rms_pair(p$lrm1, other))
  expect_false(spicy:::is_rms_pair(other, p$lrm1))
  err <- expect_error(
    compute_nested_comparisons(list(p$lrm1, other)),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "anova()", fixed = TRUE)
  expect_false(grepl("rms::lrtest", conditionMessage(err), fixed = TRUE))
})

test_that("the public path relays that refusal without an internal name", {
  p <- rms_pairs()
  other <- stats::glm(
    yb ~ x1 + x2,
    data = p$data,
    family = stats::binomial()
  )
  err <- expect_error(
    table_regression(list(p$lrm1, other), nested = TRUE),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_false(grepl("fit_curr", msg, fixed = TRUE))
  expect_false(grepl("fit_prev", msg, fixed = TRUE))
  # The engine's own words survive the scrub; only the names are gone.
  expect_match(msg, "factor names not in design", fixed = TRUE)
  expect_match(msg, "the models", fixed = TRUE)
})

# is_rms_pair() asks about the class and nothing else, so two rms fits
# of DIFFERENT responses reach compute_one_pair_rms(). The public path
# refuses such a hierarchy outright; the internal contract surface is
# pinned here, because the signed likelihood difference across two
# responses is meaningless and must not become a chi-square.
test_that("an rms pair of different responses gets no statistic", {
  p <- rms_pairs()
  got <- compute_nested_comparisons(list(p$lrm1, p$ols2))
  expect_true(is.na(got$lrt_change[1L]))
  expect_true(is.na(got$p_change[1L]))
  err <- expect_error(
    table_regression(list(p$lrm1, p$ols2), nested = TRUE),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "different response variables")
})
