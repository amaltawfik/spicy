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
  expect_equal(got$p_change[1L], unname(oracle[["P"]]), tolerance = 1e-12)
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
  expect_equal(got$p_change[1L], unname(oracle[["P"]]), tolerance = 1e-12)
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
  expect_equal(got$p_change[1L], unname(oracle[["P"]]), tolerance = 1e-12)
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
