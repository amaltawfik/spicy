# Resampling vcov on cbind(successes, failures) binomial fits.
#
# The jackknife / bootstrap refits used to pass the two-column response
# MATRIX back to glm.fit() together with the fit's POST-initialize
# weights (user weights times the row totals), so the binomial
# initialize multiplied the totals in again and every replicate ran on
# effective weights = totals^2 -- SEs off by -7% to +32% depending on
# the spread of the totals. The fix converts once to the stored-y
# representation (proportions + post-initialize weights,
# `.glm_stored_response()`), under which a refit reproduces the
# original fit exactly.
#
# Oracle values are the campaign pins (vcov_cbind_oracle_pins.csv, 17
# digits): the n = 6 hand oracle (n6_*) has zero RNG and its six
# leave-one-out refits are themselves pinned; the CORRECT side was
# cross-validated against sandwich::vcovJK (independent full-glm
# refits, exact agreement) before being pinned.

.cbind_d40 <- function() {
  set.seed(42)
  n <- 40
  d <- data.frame(
    x = rnorm(n),
    g = factor(rep(c("A", "B"), each = 20)),
    tot = sample(3:12, n, TRUE)
  )
  d$succ <- rbinom(n, d$tot, plogis(-0.2 + 0.6 * d$x + 0.5 * (d$g == "B")))
  d$prop <- d$succ / d$tot
  d
}

.strip_boot <- function(v) {
  attr(v, "beta_boot") <- NULL
  attr(v, "boot_n_valid") <- NULL
  unname(v)
}

test_that("jackknife on a cbind() fit matches the hand LOO oracle (n = 6)", {
  # Pin ids: n6_coef_*, n6_loo*_*, n6_jack_se_*_CORRECT,
  # n6_jack_cov_x_intercept_CORRECT; negative controls
  # n6_jack_se_*_spicy_ACTUEL (the totals^2 regime).
  d6 <- data.frame(
    x = c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5),
    tot = c(2L, 9L, 5L, 7L, 3L, 10L),
    succ = c(1L, 6L, 2L, 5L, 1L, 7L)
  )
  f6 <- glm(cbind(succ, tot - succ) ~ x, family = binomial, data = d6)
  expect_equal(coef(f6)[[1]], 0.43030177310086876, tolerance = 1e-12)
  expect_equal(coef(f6)[[2]], 0.069581719148557333, tolerance = 1e-12)

  # The six leave-one-out refits by FULL glm() calls (initialize runs
  # once per refit -- the definitionally correct resample), then the
  # (n-1)/n crossprod: the corrected internal refit is bit-identical
  # to this oracle.
  bj <- matrix(NA_real_, 6, 2)
  for (i in 1:6) {
    bj[i, ] <- coef(glm(
      cbind(succ, tot - succ) ~ x,
      family = binomial,
      data = d6[-i, ]
    ))
  }
  centered <- sweep(bj, 2, colMeans(bj))
  v_oracle <- (6 - 1) / 6 * crossprod(centered)

  v <- spicy:::compute_resample_vcov_jackknife(f6)
  expect_equal(unname(v), unname(v_oracle), tolerance = 1e-12)
  expect_equal(sqrt(v[1, 1]), 0.32114621813592009, tolerance = 1e-12)
  expect_equal(sqrt(v[2, 2]), 0.21261121662691423, tolerance = 1e-12)
  expect_equal(v[1, 2], -0.00052888967341415088, tolerance = 1e-12)
  # Negative control: the totals^2 values must be gone.
  expect_gt(abs(sqrt(v[1, 1]) - 0.31899664210235695), 1e-4)
  expect_gt(abs(sqrt(v[2, 2]) - 0.20284973031966222), 1e-4)
})

test_that("jackknife on a cbind() fit: n = 40 pins + sandwich::vcovJK", {
  # Pin ids: n40_jack_se{1,2,3}_CORRECT; negative controls
  # n40_jack_se{1,2,3}_spicy_ACTUEL.
  d <- .cbind_d40()
  fit_cb <- glm(cbind(succ, tot - succ) ~ x + g, family = binomial, data = d)
  fit_pr <- glm(prop ~ x + g, family = binomial, data = d, weights = tot)

  v_cb <- spicy:::compute_resample_vcov_jackknife(fit_cb)
  se <- sqrt(diag(v_cb))
  expect_equal(se[[1]], 0.16005689745894655, tolerance = 1e-12)
  expect_equal(se[[2]], 0.09493006117734365, tolerance = 1e-12)
  expect_equal(se[[3]], 0.18838628507234229, tolerance = 1e-12)

  # cbind and proportion-plus-weights are the same model stored the
  # same way after conversion: bit-identical vcov.
  v_pr <- spicy:::compute_resample_vcov_jackknife(fit_pr)
  expect_identical(unname(v_cb), unname(v_pr))

  # Independent package oracle: sandwich's jackknife refits by full
  # glm() calls and is immune to the totals^2 trap.
  skip_if_not_installed("sandwich")
  jk <- sandwich::vcovJK(fit_cb, center = "mean")
  expect_equal(unname(v_cb), unname(as.matrix(jk)), tolerance = 1e-8)
})

test_that("bootstrap on a cbind() fit is seed-matched bit-identical to the proportion form", {
  # Pin ids: n40_boot300_seed123_se{1,2,3}_CORRECT; negative controls
  # n40_boot300_seed123_se{1,2,3}_spicy_ACTUEL.
  d <- .cbind_d40()
  fit_cb <- glm(cbind(succ, tot - succ) ~ x + g, family = binomial, data = d)
  fit_pr <- glm(prop ~ x + g, family = binomial, data = d, weights = tot)

  set.seed(123)
  b_cb <- spicy:::compute_resample_vcov_bootstrap(fit_cb, boot_n = 300L)
  set.seed(123)
  b_pr <- spicy:::compute_resample_vcov_bootstrap(fit_pr, boot_n = 300L)

  se <- sqrt(diag(b_cb))
  expect_equal(se[[1]], 0.15359701943632581, tolerance = 1e-12)
  expect_equal(se[[2]], 0.095732805798778547, tolerance = 1e-12)
  expect_equal(se[[3]], 0.1879054239752474, tolerance = 1e-12)
  expect_gt(abs(se[[2]] - 0.093777572467427767), 1e-6)

  # Same seed, same rows, same stored representation: the replicate
  # matrix (which also feeds percentile CIs and the global Wald test)
  # and the covariance are bit-identical across the two forms.
  expect_identical(.strip_boot(b_cb), .strip_boot(b_pr))
  expect_identical(
    unname(attr(b_cb, "beta_boot")),
    unname(attr(b_pr, "beta_boot"))
  )
})

test_that("cluster jackknife / bootstrap on a cbind() fit match the proportion form", {
  d <- .cbind_d40()
  cl <- rep(1:10, each = 4)
  fit_cb <- glm(cbind(succ, tot - succ) ~ x + g, family = binomial, data = d)
  fit_pr <- glm(prop ~ x + g, family = binomial, data = d, weights = tot)

  vj_cb <- spicy:::compute_resample_vcov_jackknife(fit_cb, cluster = cl)
  vj_pr <- spicy:::compute_resample_vcov_jackknife(fit_pr, cluster = cl)
  expect_identical(unname(vj_cb), unname(vj_pr))

  set.seed(7)
  vb_cb <- spicy:::compute_resample_vcov_bootstrap(
    fit_cb,
    cluster = cl,
    boot_n = 100L
  )
  set.seed(7)
  vb_pr <- spicy:::compute_resample_vcov_bootstrap(
    fit_pr,
    cluster = cl,
    boot_n = 100L
  )
  expect_identical(.strip_boot(vb_cb), .strip_boot(vb_pr))
})

test_that("quasibinomial and user-weighted cbind() fits are converted too", {
  d <- .cbind_d40()
  # quasibinomial shares the multiplying initialize.
  fq_cb <- glm(
    cbind(succ, tot - succ) ~ x + g,
    family = quasibinomial,
    data = d
  )
  fq_pr <- glm(prop ~ x + g, family = quasibinomial, data = d, weights = tot)
  expect_identical(
    unname(spicy:::compute_resample_vcov_jackknife(fq_cb)),
    unname(spicy:::compute_resample_vcov_jackknife(fq_pr))
  )

  # User weights w: post-initialize weights are w * tot; the proportion
  # form with weights = w * tot is the same stored model.
  set.seed(99)
  d$w <- runif(nrow(d), 0.5, 2)
  fw_cb <- glm(
    cbind(succ, tot - succ) ~ x + g,
    family = binomial,
    data = d,
    weights = w
  )
  fw_pr <- suppressWarnings(
    glm(prop ~ x + g, family = binomial, data = d, weights = w * tot)
  )
  expect_identical(
    unname(spicy:::compute_resample_vcov_jackknife(fw_cb)),
    unname(spicy:::compute_resample_vcov_jackknife(fw_pr))
  )
})

test_that("the totals^2 signature no longer matches (negative control)", {
  # Before the fix the cbind jackknife was BIT-identical to a fit with
  # weights = totals^2 (proven with the matrix-branch mustart pairing;
  # ~1e-9 close without it). The corrected values must sit far from
  # that regime -- the n = 40 SEs differ from it by 2.7-6.3%.
  d <- .cbind_d40()
  fit_cb <- glm(cbind(succ, tot - succ) ~ x + g, family = binomial, data = d)
  fit_sq <- suppressWarnings(
    glm(prop ~ x + g, family = binomial, data = d, weights = tot^2)
  )
  v_cb <- spicy:::compute_resample_vcov_jackknife(fit_cb)
  v_sq <- spicy:::compute_resample_vcov_jackknife(fit_sq)
  expect_false(isTRUE(all.equal(unname(v_cb), unname(v_sq), tolerance = 1e-6)))
  expect_gt(abs(sqrt(v_cb[2, 2]) / sqrt(v_sq[2, 2]) - 1), 0.02)
})

test_that("table_regression resampling SEs and percentile CIs are invariant to the cbind form", {
  d <- .cbind_d40()
  fit_cb <- glm(cbind(succ, tot - succ) ~ x + g, family = binomial, data = d)
  fit_pr <- glm(prop ~ x + g, family = binomial, data = d, weights = tot)

  fr_j_cb <- as_regression_frame(fit_cb, vcov = "jackknife")
  fr_j_pr <- as_regression_frame(fit_pr, vcov = "jackknife")
  bc <- fr_j_cb$coefs[fr_j_cb$coefs$estimate_type == "B", ]
  bp <- fr_j_pr$coefs[fr_j_pr$coefs$estimate_type == "B", ]
  expect_equal(bc$std_error, bp$std_error, tolerance = 1e-12)
  expect_equal(bc$p_value, bp$p_value, tolerance = 1e-12)
  # Rendered surface: the SE column of the printed table is identical.
  tab_cb <- table_regression(fit_cb, vcov = "jackknife")
  tab_pr <- table_regression(fit_pr, vcov = "jackknife")
  expect_identical(tab_cb[["SE"]], tab_pr[["SE"]])

  # Percentile CIs come from the SAME replicates: seed-matched frames
  # must agree across the two forms, bounds included.
  set.seed(31)
  fr_cb <- as_regression_frame(
    fit_cb,
    vcov = "bootstrap",
    boot_n = 200L,
    ci_method = "boot_percentile"
  )
  set.seed(31)
  fr_pr <- as_regression_frame(
    fit_pr,
    vcov = "bootstrap",
    boot_n = 200L,
    ci_method = "boot_percentile"
  )
  cc_cb <- fr_cb$coefs[fr_cb$coefs$estimate_type == "B", ]
  cc_pr <- fr_pr$coefs[fr_pr$coefs$estimate_type == "B", ]
  expect_equal(cc_cb$std_error, cc_pr$std_error, tolerance = 1e-12)
  expect_equal(cc_cb$ci_lower, cc_pr$ci_lower, tolerance = 1e-12)
  expect_equal(cc_cb$ci_upper, cc_pr$ci_upper, tolerance = 1e-12)
})
