# G5 (Group D): ci_method = "boot_percentile" -- equal-tailed percentile
# CIs from the SAME bootstrap replicates that produce the bootstrap SEs
# (boot::boot.ci type = "perc" convention), plus the upgraded resampling
# footers (scheme + VALID replicate count).

.bp_data <- function(seed = 11, n = 150) {
  set.seed(seed)
  data.frame(y = rnorm(n, 2), x = rnorm(n), z = rnorm(n))
}

test_that("percentile interpolation matches boot::boot.ci type='perc' exactly", {
  skip_if_not_installed("boot")
  set.seed(3)
  t0 <- rnorm(437)  # deliberately not a round replicate count
  for (lvl in c(0.90, 0.95, 0.99)) {
    ours <- spicy:::.boot_percentile_ci(t0, lvl)
    b <- boot::boot.ci(
      structure(list(t0 = mean(t0), t = matrix(t0, ncol = 1), R = length(t0)),
                class = "boot"),
      conf = lvl, type = "perc"
    )$percent
    expect_equal(ours, unname(b[1, 4:5]), tolerance = 1e-12,
                 info = paste("level", lvl))
  }
})

test_that("boot_percentile: CI bounds are the replicate quantiles; SE/p stay Wald", {
  d <- .bp_data()
  fit <- lm(y ~ x + z, data = d)
  set.seed(42)
  frw <- as_regression_frame(fit, vcov = "bootstrap", boot_n = 400L,
                             ci_method = "wald")
  set.seed(42)
  frp <- as_regression_frame(fit, vcov = "bootstrap", boot_n = 400L,
                             ci_method = "boot_percentile")

  bw <- frw$coefs[frw$coefs$term == "x" & frw$coefs$estimate_type == "B", ]
  bp <- frp$coefs[frp$coefs$term == "x" & frp$coefs$estimate_type == "B", ]

  # Same replicates (same seed): estimate, SE, statistic, p identical.
  expect_equal(bp$estimate, bw$estimate, tolerance = 1e-12)
  expect_equal(bp$std_error, bw$std_error, tolerance = 1e-12)
  expect_equal(bp$p_value, bw$p_value, tolerance = 1e-12)
  # CI bounds differ (percentile vs Wald-symmetric).
  expect_gt(max(abs(c(bp$ci_lower - bw$ci_lower, bp$ci_upper - bw$ci_upper))),
            0)
  # And are NOT symmetric around the estimate in general.
  expect_identical(frp$info$ci_method, "boot_percentile")
})

test_that("boot_percentile CI matches a manual same-seed replicate quantile", {
  d <- .bp_data(7)
  fit <- lm(y ~ x, data = d)
  set.seed(9)
  vc <- spicy:::compute_model_vcov(fit, type = "bootstrap", boot_n = 300L)
  bb <- attr(vc, "beta_boot")
  expect_false(is.null(bb))
  expect_identical(attr(vc, "boot_n_valid"), nrow(bb))
  set.seed(9)
  fr <- as_regression_frame(fit, vcov = "bootstrap", boot_n = 300L,
                            ci_method = "boot_percentile")
  r <- fr$coefs[fr$coefs$term == "x" & fr$coefs$estimate_type == "B", ]
  expect_equal(c(r$ci_lower, r$ci_upper),
               spicy:::.boot_percentile_ci(bb[, "x"], 0.95),
               tolerance = 1e-12)
})

test_that("boot_percentile validation: requires bootstrap vcov and no standardized", {
  d <- .bp_data()
  fit <- lm(y ~ x, data = d)
  expect_error(
    table_regression(fit, vcov = "HC3", ci_method = "boot_percentile"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, vcov = "jackknife", ci_method = "boot_percentile"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, vcov = "bootstrap", ci_method = "boot_percentile",
                     standardized = "posthoc"),
    class = "spicy_invalid_input"
  )
})

test_that("footers: replicate count on the Std. errors line, percentile CI note", {
  d <- .bp_data()
  fit <- lm(y ~ x, data = d)
  set.seed(5)
  res <- table_regression(fit, vcov = "bootstrap", boot_n = 200L,
                          ci_method = "boot_percentile")
  note <- paste(attr(res, "note"), collapse = "\n")
  expect_match(note, "nonparametric bootstrap (200 replicates)", fixed = TRUE)
  expect_match(note, "95% CIs: bootstrap percentile.", fixed = TRUE)
  # Wald default: no CI note, count still displayed.
  set.seed(5)
  resw <- table_regression(fit, vcov = "bootstrap", boot_n = 200L)
  notew <- paste(attr(resw, "note"), collapse = "\n")
  expect_match(notew, "nonparametric bootstrap (200 replicates)", fixed = TRUE)
  expect_false(grepl("bootstrap percentile", notew, fixed = TRUE))
})

test_that("footer: jackknife names the scheme", {
  d <- .bp_data()
  fit <- lm(y ~ x, data = d)
  res <- table_regression(fit, vcov = "jackknife")
  expect_match(paste(attr(res, "note"), collapse = "\n"),
               "jackknife (leave-one-out)", fixed = TRUE)
})

test_that("exponentiate: percentile bounds are exponentiated (transformation-respecting)", {
  set.seed(21)
  n <- 200
  d <- data.frame(x = rnorm(n))
  d$y <- rbinom(n, 1, plogis(0.6 * d$x))
  fit <- glm(y ~ x, data = d, family = binomial())
  set.seed(33)
  fr_raw <- as_regression_frame(fit, vcov = "bootstrap", boot_n = 200L,
                                ci_method = "boot_percentile")
  raw <- fr_raw$coefs[fr_raw$coefs$term == "x" &
                        fr_raw$coefs$estimate_type == "B", ]
  # The displayed OR CI equals exp() of the link-scale percentile bounds.
  set.seed(33)
  tbl <- table_regression(fit, vcov = "bootstrap", boot_n = 200L,
                          ci_method = "boot_percentile", exponentiate = TRUE)
  st <- as_structured(tbl)
  i_x <- which(st$body$Variable == "x")
  ll <- st$body[["95% CI: LL"]][i_x]
  ul <- st$body[["95% CI: UL"]][i_x]
  expect_equal(ll, exp(raw$ci_lower), tolerance = 1e-9)
  expect_equal(ul, exp(raw$ci_upper), tolerance = 1e-9)
})
