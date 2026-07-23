# Profile-likelihood CIs for ordinal (polr / clm) via confint(), and the
# CI-method footer disclosure. Profile is a CI-only refinement: estimate / SE /
# statistic / p stay Wald; a robust vcov takes precedence; requesting profile
# for a class without a profile path errors (no silent Wald fallback).

make_ord <- function(seed = 1, n = 240) {
  set.seed(seed)
  data.frame(
    x1 = rnorm(n),
    smoke = factor(sample(c("no", "yes"), n, TRUE)),
    g = factor(sample(10, n, TRUE)),
    yc = factor(
      sample(c("Poor", "Fair", "Good", "Very good"), n, TRUE),
      levels = c("Poor", "Fair", "Good", "Very good"),
      ordered = TRUE
    )
  )
}
b_ci <- function(fr, term) {
  r <- fr$coefs[fr$coefs$term == term & fr$coefs$estimate_type == "B", ]
  c(r$ci_lower[1], r$ci_upper[1])
}
note_of <- function(res) paste(attr(res, "note"), collapse = "\n")

# ---- profile CIs match confint() ------------------------------------------

test_that("polr profile CIs match confint.polr; SE and thresholds stay Wald", {
  skip_if_not_installed("MASS")
  d <- make_ord()
  fit <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
  frw <- as_regression_frame(fit, ci_method = "wald")
  frp <- as_regression_frame(fit, ci_method = "profile")
  oracle <- suppressMessages(stats::confint(fit))

  expect_equal(b_ci(frp, "x1"), unname(oracle["x1", ]), tolerance = 1e-6)
  expect_equal(
    b_ci(frp, "smokeyes"),
    unname(oracle["smokeyes", ]),
    tolerance = 1e-6
  )
  expect_gt(max(abs(b_ci(frp, "x1") - b_ci(frw, "x1"))), 1e-5) # differs from Wald
  # SE is a CI-only refinement: unchanged
  se_w <- frw$coefs$std_error[frw$coefs$term == "x1"][1]
  se_p <- frp$coefs$std_error[frp$coefs$term == "x1"][1]
  expect_equal(se_w, se_p, tolerance = 1e-12)
})

test_that("clm profile CIs match confint.clm", {
  skip_if_not_installed("ordinal")
  d <- make_ord()
  fit <- ordinal::clm(yc ~ x1 + smoke, data = d)
  frp <- as_regression_frame(fit, ci_method = "profile")
  oracle <- suppressMessages(stats::confint(fit))
  expect_equal(b_ci(frp, "x1"), unname(oracle["x1", ]), tolerance = 1e-6)
  expect_equal(
    b_ci(frp, "smokeyes"),
    unname(oracle["smokeyes", ]),
    tolerance = 1e-6
  )
})

test_that("single-predictor polr profile handles confint's vector shape", {
  skip_if_not_installed("MASS")
  d <- make_ord()
  fit <- MASS::polr(yc ~ x1, data = d, Hess = TRUE)
  frp <- as_regression_frame(fit, ci_method = "profile")
  oracle <- suppressMessages(stats::confint(fit)) # a length-2 vector here
  expect_equal(b_ci(frp, "x1"), unname(oracle), tolerance = 1e-6)
})

# ---- footer disclosure -----------------------------------------------------

test_that("footer discloses profile CIs only when they are actually used", {
  skip_if_not_installed("MASS")
  d <- make_ord()
  fit <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)

  expect_match(
    note_of(table_regression(fit, ci_method = "profile")),
    "profile likelihood",
    fixed = TRUE
  )
  expect_false(grepl(
    "profile likelihood",
    note_of(table_regression(fit, ci_method = "wald")),
    fixed = TRUE
  ))
  # robust vcov takes precedence -> no profile note, and the CI is robust-Wald
  res_cr <- suppressWarnings(
    table_regression(fit, vcov = "CR2", cluster = d$g, ci_method = "profile")
  )
  expect_false(grepl("profile likelihood", note_of(res_cr), fixed = TRUE))
})

test_that("profile + robust vcov yields symmetric (robust-Wald) CIs", {
  skip_if_not_installed("MASS")
  d <- make_ord()
  fit <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
  fr <- suppressWarnings(
    as_regression_frame(fit, vcov = "CR2", cluster = d$g, ci_method = "profile")
  )
  r <- fr$coefs[fr$coefs$term == "x1" & fr$coefs$estimate_type == "B", ]
  expect_equal((r$ci_lower + r$ci_upper) / 2, r$estimate, tolerance = 1e-9)
})

# ---- validation: no silent Wald fallback ----------------------------------

test_that("profile is rejected for classes without a profile path", {
  d <- make_ord()
  skip_if_not_installed("survival")
  d$time <- rexp(nrow(d))
  d$status <- rbinom(nrow(d), 1, 0.7)
  fit <- survival::survreg(survival::Surv(time, status) ~ x1, data = d)
  expect_error(
    table_regression(fit, ci_method = "profile"),
    class = "spicy_invalid_input"
  )
})

test_that("glm profile still works and discloses in the footer (cross-class)", {
  d <- make_ord()
  d$y <- rbinom(nrow(d), 1, 0.5)
  fit <- glm(y ~ x1, data = d, family = binomial)
  frp <- as_regression_frame(fit, ci_method = "profile")
  expect_equal(
    b_ci(frp, "x1"),
    unname(suppressMessages(stats::confint(fit))["x1", ]),
    tolerance = 1e-6
  )
  expect_match(
    note_of(table_regression(fit, ci_method = "profile")),
    "profile likelihood",
    fixed = TRUE
  )
})

# ---- glm: profile x robust/resampling precedence (Group D companion) -------

test_that("glm profile + robust vcov: robust Wald CIs, not classical profile", {
  d <- make_ord()
  d$y <- rbinom(nrow(d), 1, plogis(0.8 * d$x1))
  fit <- glm(y ~ x1 + smoke, data = d, family = binomial)

  # Previously: classical profile bounds displayed silently next to
  # robust SEs. Now the robust vcov takes precedence (polr/clm
  # precedent): CIs are symmetric Wald from the robust SE.
  fr <- suppressWarnings(
    as_regression_frame(fit, vcov = "HC3", ci_method = "profile")
  )
  r <- fr$coefs[fr$coefs$term == "x1" & fr$coefs$estimate_type == "B", ]
  expect_equal(
    (r$ci_lower[1] + r$ci_upper[1]) / 2,
    r$estimate[1],
    tolerance = 1e-9
  )
  prof <- suppressMessages(stats::confint(fit))["x1", ]
  expect_gt(max(abs(c(r$ci_lower[1], r$ci_upper[1]) - unname(prof))), 1e-6)
  # The effective method is recorded (footer / as_structured stay truthful).
  expect_identical(fr$info$ci_method, "wald")
})

test_that("glm profile + robust vcov warns once (consolidated disclosure)", {
  d <- make_ord()
  d$y <- rbinom(nrow(d), 1, plogis(0.8 * d$x1))
  fit <- glm(y ~ x1, data = d, family = binomial)
  expect_warning(
    res <- table_regression(fit, vcov = "HC1", ci_method = "profile"),
    class = "spicy_ignored_arg"
  )
  expect_false(grepl("profile likelihood", note_of(res), fixed = TRUE))
})

test_that("glm profile + bootstrap vcov: same precedence", {
  d <- make_ord()
  d$y <- rbinom(nrow(d), 1, plogis(0.8 * d$x1))
  fit <- glm(y ~ x1, data = d, family = binomial)
  set.seed(4)
  fr <- suppressWarnings(
    as_regression_frame(
      fit,
      vcov = "bootstrap",
      boot_n = 50L,
      ci_method = "profile"
    )
  )
  r <- fr$coefs[fr$coefs$term == "x1" & fr$coefs$estimate_type == "B", ]
  expect_equal(
    (r$ci_lower[1] + r$ci_upper[1]) / 2,
    r$estimate[1],
    tolerance = 1e-9
  )
  expect_identical(fr$info$ci_method, "wald")
})
