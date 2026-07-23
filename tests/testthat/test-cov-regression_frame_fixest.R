# ---------------------------------------------------------------------------
# Coverage tests for R/regression_frame_fixest.R
#
# Targets uncovered-but-reachable regions not exercised by
# test-regression_frame_fixest.R:
#   * feglm binomial title-prefix switch arm ("Logistic regression")   (L288)
#   * no-fixed-effect feols -> n_groups NULL else-arm                   (L201)
#   * ordered (polynomial-coded) factor -> reference_dropped == FALSE
#     reaches the `next` skip and the empty-reference-rows fall-through (L149, L171)
#   * fenegbin (negative-binomial GLM) -> GLM-like family detection,
#     z-asymptotic inference, log link, IRR exponentiation, theta, and
#     the "Negative-binomial regression" title-prefix switch arm.
#
# The remaining uncovered lines (.check_fixest_available abort; the
# non-finite df.residual guard; the Gamma/inverse.gaussian/default
# switch arms) are handled separately: see the # nocov markers and the
# audit findings.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.cov_air <- function() {
  d <- na.omit(airquality)
  d$Month <- factor(d$Month)
  d
}


# ---- feglm binomial: "Logistic regression (fixed effects)" (L288) ---------

test_that("feglm binomial: title_prefix switch arm -> 'Logistic regression'", {
  skip_if_not_installed("fixest")
  d <- .cov_air()
  d$bin <- as.integer(d$Ozone > stats::median(d$Ozone))
  fit <- fixest::feglm(
    bin ~ Solar.R + Wind | Month,
    data = d,
    family = stats::binomial()
  )
  fr <- as_regression_frame(fit, model_id = "M1")

  expect_identical(
    fr$info$extras$title_prefix,
    "Logistic regression (fixed effects)"
  )
  expect_identical(fr$info$family$family, "binomial")
  expect_identical(fr$info$family$link, "logit")
  expect_invisible(spicy:::validate_regression_frame(fr))
  # z-asymptotic inference (binomial coeftable has "z value").
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "z"))
  expect_true(all(is.infinite(b_rows$df)))
})


# ---- no fixed effects: n_groups NULL else-arm (L201) ----------------------

test_that("feols without a fixed-effect block sets info$n_groups = NULL", {
  skip_if_not_installed("fixest")
  d <- .cov_air()
  fit <- fixest::feols(Ozone ~ Solar.R + Wind, data = d)
  fr <- as_regression_frame(fit, model_id = "M1")

  expect_null(fr$info$n_groups)
  expect_identical(length(fr$info$extras$fixef_sizes), 0L)
  expect_invisible(spicy:::validate_regression_frame(fr))
  # Estimates still extracted (intercept present without FE absorption).
  expect_true("(Intercept)" %in% fr$coefs$term)
})


# ---- ordered factor: reference_dropped == FALSE skip path (L149, L171) -----

test_that("feols with an ordered factor synthesises no reference row", {
  skip_if_not_installed("fixest")
  d <- .cov_air()
  d$Wind_ord <- cut(
    d$Wind,
    3,
    labels = c("low", "mid", "high"),
    ordered_result = TRUE
  )
  fit <- fixest::feols(Ozone ~ Solar.R + Wind_ord | Month, data = d)
  fr <- as_regression_frame(fit, model_id = "M1")

  # Polynomial contrasts (.L/.Q) have no dropped baseline -> the loop
  # skips them (next) and the empty-rows fall-through returns an empty
  # coefs frame, so zero is_ref rows are appended.
  expect_identical(sum(fr$coefs$is_ref), 0L)
  expect_true(any(grepl("\\.L$", fr$coefs$term)))
  expect_invisible(spicy:::validate_regression_frame(fr))
})


# ---- fenegbin: GLM-like negative-binomial detection (audit bug L43/L298) --
#
# fixest::fenegbin() stores fit$family as the character string "negbin"
# (NOT a glm-family list) with the dispersion theta in fit$theta. The old
# `is_glm <- !is.null(fit$family) && is.list(fit$family)` test was FALSE
# for such a fit, so it was silently processed as gaussian/identity OLS:
# wrong family, Wald-t labels (t / finite df), classical_r2 = TRUE,
# exponentiate disabled, and a "Linear regression" title.
#
# Correct behaviour, derived from first principles:
#   * family$family == "negbin", family$link == "log"
#   * coeftable carries "z value" / "Pr(>|z|)" -> test_type "z", df = Inf
#   * classical R^2 is undefined for a count GLM -> supports$classical_r2
#     FALSE; exponentiate enabled (log link -> incidence-rate ratios)
#   * title prefix "Negative-binomial regression (fixed effects)"
#   * the dispersion theta is surfaced in extras$theta and the auxiliary
#     ".theta" coeftable row is NOT a model coefficient (coef(fit) omits
#     it, so it must not appear in the coefs table).

# Deterministic overdispersed counts via a Poisson-Gamma (= negative
# binomial) mixture; avoids a hard MASS dependency.
.cov_nb_data <- function() {
  set.seed(101)
  n <- 400L
  g <- factor(sample(seq_len(8L), n, replace = TRUE))
  x <- stats::rnorm(n)
  mu <- exp(0.6 + 0.5 * x + as.numeric(g) * 0.05)
  th <- 1.5
  lambda <- stats::rgamma(n, shape = th, scale = mu / th)
  y <- stats::rpois(n, lambda)
  data.frame(y = y, x = x, g = g)
}

test_that("fenegbin is detected as GLM-like (z-asymptotic, log link, IRR)", {
  skip_if_not_installed("fixest")
  d <- .cov_nb_data()
  fit <- fixest::fenegbin(y ~ x | g, data = d)
  # Sanity: this fit really is the mishandled shape (string family, theta).
  expect_true(is.character(fit$family))
  expect_identical(fit$family[[1L]], "negbin")

  fr <- as_regression_frame(fit, model_id = "M1")

  # Family / link.
  expect_identical(fr$info$family$family, "negbin")
  expect_identical(fr$info$family$link, "log")

  # GLM-like inference: z value with infinite df, NOT Wald-t.
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "z"))
  expect_true(all(is.infinite(b_rows$df)))

  # The z statistic + p value must equal summary(fit)$coeftable.
  sm <- summary(fit)$coeftable
  expect_equal(b_rows$statistic[b_rows$term == "x"], unname(sm["x", "z value"]))
  expect_equal(b_rows$p_value[b_rows$term == "x"], unname(sm["x", "Pr(>|z|)"]))

  # The auxiliary ".theta" dispersion row is NOT a model coefficient.
  expect_false(".theta" %in% fr$coefs$term)
  # coef(fit) omits .theta; so does the frame.
  expect_setequal(b_rows$term, names(stats::coef(fit)))

  # supports flags: count GLM -> no classical R^2, exponentiation enabled.
  expect_false(fr$info$supports$classical_r2)
  expect_true(fr$info$supports$exponentiate)

  # Title prefix switch arm.
  expect_identical(
    fr$info$extras$title_prefix,
    "Negative-binomial regression (fixed effects)"
  )

  # Dispersion theta surfaced (matches fit$theta, a positive scalar).
  expect_equal(fr$info$extras$theta, unname(as.numeric(fit$theta)))
  expect_true(is.finite(fr$info$extras$theta) && fr$info$extras$theta > 0)

  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("fenegbin with a factor covariate keeps reference rows + z labels", {
  skip_if_not_installed("fixest")
  d <- .cov_nb_data()
  d$f <- factor(sample(c("a", "b", "c"), nrow(d), replace = TRUE))
  fit <- fixest::fenegbin(y ~ x + f | g, data = d)
  fr <- as_regression_frame(fit, model_id = "M1")

  # One synthesised reference row (baseline level "a" of f).
  ref <- fr$coefs[fr$coefs$is_ref, ]
  expect_identical(nrow(ref), 1L)
  expect_identical(ref$label, "a")
  expect_true(is.na(ref$estimate))

  # Non-reference rows are GLM-like (z, df = Inf) and exclude .theta.
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "z"))
  expect_true(all(is.infinite(b_rows$df)))
  expect_false(".theta" %in% fr$coefs$term)
  expect_setequal(b_rows$term, names(stats::coef(fit)))

  expect_invisible(spicy:::validate_regression_frame(fr))
})
