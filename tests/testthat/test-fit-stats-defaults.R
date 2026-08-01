# Class-aware show_fit_stats default resolution, including the universal
# nobs + AIC safety net for classes not matched by a tailored branch.

fs_labels <- function(fit, ...) {
  df <- table_regression(fit, output = "data.frame", ...)
  trimws(df$Variable)
}

test_that("lm default fit-stats = nobs + R2 + adj_R2 (no AIC)", {
  v <- fs_labels(lm(mpg ~ wt + hp, data = mtcars))
  expect_true(any(v == "n"))
  expect_true(any(grepl("Adj", v))) # Adj.R2
  expect_false(any(v == "AIC"))
})

test_that("glm default fit-stats = nobs + McFadden + Nagelkerke + AIC", {
  set.seed(1)
  d <- data.frame(x = rnorm(120), y = rbinom(120, 1, 0.5))
  v <- fs_labels(glm(y ~ x, data = d, family = binomial))
  expect_true(any(v == "n"))
  expect_true(any(grepl("McFadden", v)))
  expect_true(any(grepl("Nagelkerke", v)))
  expect_true(any(v == "AIC"))
})

# ---- universal fallback (nobs + AIC) for otherwise-uncovered classes ------

test_that("betareg falls back to nobs + AIC (was a blank block)", {
  skip_if_not_installed("betareg")
  set.seed(1)
  n <- 150
  d <- data.frame(x = rnorm(n))
  d$yp <- pmin(pmax(plogis(0.5 * d$x + rnorm(n)), 1e-3), 1 - 1e-3)
  v <- fs_labels(betareg::betareg(yp ~ x, data = d))
  expect_true(any(v == "n"))
  expect_true(any(v == "AIC"))
})

test_that("survreg + coxph fall back to nobs + AIC", {
  skip_if_not_installed("survival")
  set.seed(1)
  n <- 150
  d <- data.frame(x = rnorm(n), time = rexp(n), status = rbinom(n, 1, 0.7))
  for (fit in list(
    survival::survreg(survival::Surv(time, status) ~ x, data = d),
    survival::coxph(survival::Surv(time, status) ~ x, data = d)
  )) {
    v <- fs_labels(fit)
    expect_true(any(v == "n"))
    expect_true(any(v == "AIC"))
  }
})

test_that("multinom falls back to nobs + AIC", {
  skip_if_not_installed("nnet")
  set.seed(1)
  n <- 180
  d <- data.frame(x = rnorm(n), g = factor(sample(letters[1:3], n, TRUE)))
  v <- fs_labels(nnet::multinom(g ~ x, data = d, trace = FALSE))
  expect_true(any(v == "n"))
  expect_true(any(v == "AIC"))
})

test_that("an explicit show_fit_stats still overrides the fallback", {
  skip_if_not_installed("survival")
  set.seed(1)
  n <- 120
  d <- data.frame(x = rnorm(n), time = rexp(n), status = rbinom(n, 1, 0.7))
  fit <- survival::coxph(survival::Surv(time, status) ~ x, data = d)
  v <- fs_labels(fit, show_fit_stats = "nobs")
  expect_true(any(v == "n"))
  expect_false(any(v == "AIC")) # AIC not requested -> absent
  # and FALSE suppresses the whole block
  v0 <- fs_labels(fit, show_fit_stats = FALSE)
  expect_false(any(v0 == "AIC"))
  expect_false(any(v0 == "n"))
})


# ============================================================================
# Phase 3 matrix – rd-core:fit-stats-default-mixed-union
# ============================================================================

test_that("mixed lm + glm default is the union of both class defaults", {
  m_lm <- lm(mpg ~ wt, data = mtcars)
  m_glm <- glm(am ~ mpg, data = mtcars, family = binomial)
  out <- table_regression(list(OLS = m_lm, Logit = m_glm))
  d <- as.data.frame(out, stringsAsFactors = FALSE)
  vars <- trimws(d$Variable)
  # Union of lm (n, R2, adj_R2) and glm (n, McFadden, Nagelkerke, AIC)
  expect_true(all(
    c("n", "R²", "Adj.R²", "R² (McFadden)", "R² (Nagelkerke)", "AIC") %in% vars
  ))
  s <- as_structured(out)
  b <- s$body
  # Each stat is populated under its own class's column ...
  expect_equal(
    b[["OLS: B"]][b$Variable == "R²"],
    summary(m_lm)$r.squared,
    tolerance = 1e-10
  )
  expect_false(is.na(b[["Logit: B"]][b$Variable == "R² (McFadden)"]))
  expect_false(is.na(b[["OLS: B"]][b$Variable == "AIC"]))
  # ... and stays empty (NA) under the class where it is undefined
  expect_true(is.na(b[["Logit: B"]][b$Variable == "R²"]))
  expect_true(is.na(b[["OLS: B"]][b$Variable == "R² (McFadden)"]))
})

test_that("mixed lm + glm alien fit-stat cells render an en-dash", {
  # FIXME matrix: rd-core:fit-stats-default-mixed-union –
  # man/table_regression.Rd 386-389 / 905-906 says the renderer
  # "en-dashes per cell the stat not defined for a given model class",
  # but the displayed cells are BLANK (empty strings), not en-dash
  # (repro: table_regression(list(lm(mpg ~ wt, mtcars), glm(am ~ mpg,
  # mtcars, family = binomial))) – the R² cell of the glm column and
  # the McFadden cell of the lm column are ""). Doc-vs-code discordance
  # recorded in the Phase 3 audit.
  skip(
    "rd-core:fit-stats-default-mixed-union – alien cells render blank, not en-dash"
  )
})
