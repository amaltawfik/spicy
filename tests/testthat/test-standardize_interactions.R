# G2 (Group D): the algebraic standardisation convention on interaction
# terms, pinned to its cross-package oracles.
#
# spicy's posthoc / basic / smart scale an interaction row by the SD of
# the PRODUCT design column (SPSS beta / Stata `regress, beta` / SAS STB /
# lm.beta / effectsize "basic" convention) -- deliberately NOT the
# z-scored-components convention of "refit" / effectsize "posthoc"
# (Friedrich 1982). The source comments used to claim effectsize-posthoc
# equivalence; these oracles pin the TRUE equivalences.

.si_data <- function(seed = 5, n = 300) {
  set.seed(seed)
  x <- rnorm(n)
  z <- 0.6 * x + rnorm(n)  # correlated components: conventions differ
  y <- 1 + 0.5 * x + 0.3 * z + 0.4 * x * z + rnorm(n)
  data.frame(y = y, x = x, z = z)
}

test_that("refit interaction beta matches effectsize refit (1e-7)", {
  skip_if_not_installed("effectsize")
  d <- .si_data()
  fit <- lm(y ~ x * z, data = d)
  s <- suppressWarnings(spicy:::standardize_lm(fit, method = "refit"))
  es <- effectsize::standardize_parameters(fit, method = "refit")
  expect_equal(s$estimate[s$term == "x:z"],
               es$Std_Coefficient[es$Parameter == "x:z"],
               tolerance = 1e-7)
})

test_that("basic interaction beta matches effectsize basic AND lm.beta (1e-7)", {
  d <- .si_data()
  fit <- lm(y ~ x * z, data = d)
  s <- suppressWarnings(spicy:::standardize_lm(fit, method = "basic"))
  b_spicy <- s$estimate[s$term == "x:z"]

  if (requireNamespace("effectsize", quietly = TRUE)) {
    es <- effectsize::standardize_parameters(fit, method = "basic")
    expect_equal(b_spicy, es$Std_Coefficient[es$Parameter == "x:z"],
                 tolerance = 1e-7)
  }
  # Manual lm.beta convention: b * sd(product column) / sd(y).
  X <- model.matrix(fit)
  oracle <- coef(fit)[["x:z"]] * sd(X[, "x:z"]) / sd(d$y)
  expect_equal(b_spicy, oracle, tolerance = 1e-10)
  # And it deliberately DIFFERS from the refit convention here
  # (correlated components).
  s_refit <- suppressWarnings(spicy:::standardize_lm(fit, method = "refit"))
  expect_gt(abs(b_spicy - s_refit$estimate[s_refit$term == "x:z"]), 1e-4)
})

test_that("smart: binary x binary product column gets the 2 x SD rule", {
  set.seed(9)
  n <- 400
  d <- data.frame(b1 = rbinom(n, 1, 0.5), b2 = rbinom(n, 1, 0.4))
  d$y <- 1 + 0.5 * d$b1 + 0.3 * d$b2 + 0.6 * d$b1 * d$b2 + rnorm(n)
  fit <- lm(y ~ b1 * b2, data = d)
  s_smart <- suppressWarnings(spicy:::standardize_lm(fit, method = "smart"))
  s_ph    <- suppressWarnings(spicy:::standardize_lm(fit, method = "posthoc"))
  # The b1:b2 design column is itself 0/1 -> smart doubles posthoc.
  expect_equal(s_smart$estimate[s_smart$term == "b1:b2"],
               2 * s_ph$estimate[s_ph$term == "b1:b2"],
               tolerance = 1e-10)
})

test_that("fallback-aware footer: refit failure names the posthoc convention", {
  set.seed(31)
  xp <- exp(rnorm(200)); z <- rnorm(200)
  d <- data.frame(y = log(xp) * 0.5 + z + rnorm(200), xp = xp, z = z)
  fit <- lm(y ~ log(xp) * z, data = d)
  fb_seen <- FALSE
  out <- withCallingHandlers(
    table_regression(fit, standardized = "refit",
                     show_columns = c("b", "beta", "p")),
    spicy_fallback = function(c) { fb_seen <<- TRUE; invokeRestart("muffleWarning") },
    spicy_caveat = function(c) invokeRestart("muffleWarning")
  )
  expect_true(fb_seen)
  note <- paste(attr(out, "note"), collapse = "\n")
  expect_match(note, "\"refit\" failed; algebraic (posthoc) scaling applied",
               fixed = TRUE)
  expect_match(note, "lm.beta convention", fixed = TRUE)
})

test_that("non-fallback refit keeps the refit footer wording", {
  d <- .si_data()
  fit <- lm(y ~ x * z, data = d)
  out <- withCallingHandlers(
    table_regression(fit, standardized = "refit",
                     show_columns = c("b", "beta", "p")),
    spicy_caveat = function(c) invokeRestart("muffleWarning")
  )
  note <- paste(attr(out, "note"), collapse = "\n")
  expect_match(note, "after refit on z-scored data", fixed = TRUE)
  expect_false(grepl("refit\" failed", note, fixed = TRUE))
})
