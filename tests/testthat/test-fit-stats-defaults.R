# Class-aware show_fit_stats default resolution, including the universal
# nobs + AIC safety net for classes not matched by a tailored branch.

# The displayed row labels: the `Variable` column of the printed table.
fs_labels <- function(fit, ...) {
  df <- table_regression(fit, ...)
  trimws(df$Variable)
}

test_that("lm default fit-stats = nobs + R2 + adj_R2 (no AIC)", {
  v <- fs_labels(lm(mpg ~ wt + hp, data = mtcars))
  expect_true(any(v == "n"))
  expect_true(any(grepl("Adj", v))) # Adj. R2
  expect_false(any(v == "AIC"))
})

test_that("a binary logit defaults to n, Tjur, McFadden and AIC", {
  set.seed(1)
  d <- data.frame(x = rnorm(120), y = rbinom(120, 1, 0.5))
  v <- fs_labels(glm(y ~ x, data = d, family = binomial))
  expect_identical(
    v[(length(v) - 3L):length(v)],
    c("n", "R² (Tjur)", "R² (McFadden)", "AIC")
  )
  expect_false(any(grepl("Nagelkerke", v)))
})

# The fit-statistic tokens of a default table, in row order.
fs_tokens <- function(fit, ...) {
  df <- suppressWarnings(table_regression(fit, output = "data.frame", ...))
  df$variable[df$.row_role == "fit_stat"]
}

test_that("the default R-squared rows follow the family of each class", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  skip_if_not_installed("nnet")
  skip_if_not_installed("rms")
  set.seed(14)
  n <- 200
  d <- data.frame(x = rnorm(n), fw = sample(1:3, n, TRUE))
  d$y <- rbinom(n, 1, plogis(-0.3 + d$x))
  d$yf <- factor(d$y, labels = c("no", "yes"))
  d$yl <- d$y == 1
  d$cnt <- rpois(n, exp(0.4 + 0.3 * d$x))
  d$ord <- cut(
    d$x + rnorm(n),
    c(-Inf, -0.5, 0.5, Inf),
    labels = c("a", "b", "c"),
    ordered_result = TRUE
  )
  d$mn <- factor(sample(c("A", "B", "C"), n, TRUE))
  g <- data.frame(s = rbinom(30, 10, 0.4), m = 10, x = rnorm(30))
  tjur_mcf <- c("nobs", "pseudo_r2_tjur", "pseudo_r2_mcfadden", "aic")
  mcf <- c("nobs", "pseudo_r2_mcfadden", "aic")
  # A logit on one 0/1 outcome per row, whatever its type or weights.
  expect_identical(fs_tokens(glm(y ~ x, binomial, d)), tjur_mcf)
  expect_identical(fs_tokens(glm(yf ~ x, binomial, d)), tjur_mcf)
  expect_identical(fs_tokens(glm(yl ~ x, binomial, d)), tjur_mcf)
  expect_identical(
    fs_tokens(glm(y ~ x, binomial, d, weights = fw)),
    tjur_mcf
  )
  expect_identical(
    fs_tokens(rms::Glm(y ~ x, family = binomial, data = d)),
    tjur_mcf
  )
  # McFadden alone: the other links, the grouped forms, the counts and
  # the categorical families.
  expect_identical(fs_tokens(glm(y ~ x, binomial("probit"), d)), mcf)
  expect_identical(fs_tokens(glm(y ~ x, binomial("cloglog"), d)), mcf)
  expect_identical(fs_tokens(glm(cbind(s, m - s) ~ x, binomial, g)), mcf)
  expect_identical(
    fs_tokens(glm(I(s / m) ~ x, binomial, g, weights = m)),
    mcf
  )
  expect_identical(fs_tokens(glm(cnt ~ x, poisson, d)), mcf)
  expect_identical(fs_tokens(MASS::glm.nb(cnt ~ x, d)), mcf)
  expect_identical(fs_tokens(MASS::polr(ord ~ x, d, Hess = TRUE)), mcf)
  expect_identical(fs_tokens(ordinal::clm(ord ~ x, data = d)), mcf)
  expect_identical(fs_tokens(nnet::multinom(mn ~ x, d, trace = FALSE)), mcf)
  expect_identical(fs_tokens(rms::lrm(y ~ x, d)), mcf)
  expect_identical(fs_tokens(rms::lrm(ord ~ x, d)), mcf)
  # No likelihood, no pseudo-R-squared.
  expect_identical(fs_tokens(glm(cnt ~ x, quasipoisson, d)), "nobs")
  expect_identical(fs_tokens(glm(y ~ x, quasibinomial, d)), "nobs")
})

test_that("Nagelkerke's R2 stays available on request", {
  fit <- glm(am ~ mpg, data = mtcars, family = binomial)
  df <- table_regression(
    fit,
    show_fit_stats = c("nobs", "pseudo_r2_nagelkerke"),
    output = "data.frame"
  )
  expect_equal(
    df[["B"]][df$variable == "pseudo_r2_nagelkerke"],
    spicy:::compute_pseudo_r2_nagelkerke(fit),
    tolerance = 1e-12
  )
})

test_that("a table of several models shows the union of their rows", {
  set.seed(3)
  d <- data.frame(x = rnorm(150))
  d$y <- rbinom(150, 1, plogis(d$x))
  d$cnt <- rpois(150, exp(0.2 + 0.4 * d$x))
  logit <- glm(y ~ x, binomial, d)
  probit <- glm(y ~ x, binomial("probit"), d)
  pois <- glm(cnt ~ x, poisson, d)
  df <- table_regression(
    list(Logit = logit, Probit = probit, Poisson = pois),
    output = "data.frame"
  )
  fs <- df[df$.row_role == "fit_stat", ]
  expect_identical(
    fs$variable,
    c("nobs", "pseudo_r2_tjur", "pseudo_r2_mcfadden", "aic")
  )
  tjur <- fs[fs$variable == "pseudo_r2_tjur", ]
  expect_equal(
    tjur[["Logit: B"]],
    mean(fitted(logit)[d$y == 1]) - mean(fitted(logit)[d$y == 0]),
    tolerance = 1e-12
  )
  # A row shown for one model fills every model that defines it: the
  # probit has a Tjur R2 (not its default), the Poisson model has none.
  expect_equal(
    tjur[["Probit: B"]],
    mean(fitted(probit)[d$y == 1]) - mean(fitted(probit)[d$y == 0]),
    tolerance = 1e-12
  )
  expect_true(is.na(tjur[["Poisson: B"]]))
  out <- structure(
    table_regression(list(Logit = logit, Probit = probit, Poisson = pois)),
    class = "data.frame"
  )
  row <- out[trimws(out$Variable) == "R² (Tjur)", ]
  expect_identical(trimws(row[[grep("^Poisson", names(out))[1L]]]), "–")
})

test_that("the default rows hold under nested, show_columns = 'none' and French", {
  m1 <- glm(am ~ mpg, data = mtcars, family = binomial)
  m2 <- glm(am ~ mpg + wt, data = mtcars, family = binomial)
  expect_identical(
    fs_tokens(list(m1, m2), nested = TRUE),
    c(
      "nobs",
      "pseudo_r2_tjur",
      "pseudo_r2_mcfadden",
      "aic",
      "lrt_change",
      "p_change"
    )
  )
  df <- table_regression(m1, show_columns = "none", output = "data.frame")
  expect_identical(
    df$variable,
    c("nobs", "pseudo_r2_tjur", "pseudo_r2_mcfadden", "aic")
  )
  withr::local_options(spicy.language = "fr")
  v <- fs_labels(m1)
  expect_identical(
    v[(length(v) - 3L):length(v)],
    c("n", "R² (Tjur)", "R² (McFadden)", "AIC")
  )
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
  expect_true("x" %in% v0) # the rows are read
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
  vars <- trimws(out$Variable)
  # Union of lm (n, R2, adj_R2) and the binary logit (n, Tjur, McFadden, AIC)
  k <- length(vars)
  expect_identical(
    vars[(k - 5L):k],
    c("n", "R²", "Adj. R²", "R² (Tjur)", "R² (McFadden)", "AIC")
  )
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
  # rd-core:fit-stats-default-mixed-union (per-cell en-dash contract):
  # the renderer "en-dashes per cell the stat not defined for a given
  # model class" (man/table_regression.Rd).
  m_lm <- lm(mpg ~ wt, data = mtcars)
  m_glm <- glm(am ~ mpg, data = mtcars, family = binomial)
  out <- table_regression(list(OLS = m_lm, Logit = m_glm))
  # The printed table: its columns are the displayed cells.
  d <- structure(out, class = "data.frame")
  vars <- trimws(d$Variable)
  # First (display) sub-column of each model carries the fit stats.
  ols_col <- grep("^OLS", names(d), value = TRUE)[1L]
  logit_col <- grep("^Logit", names(d), value = TRUE)[1L]
  dash <- "–"
  expect_identical(trimws(d[[logit_col]][vars == "R²"]), dash)
  expect_identical(trimws(d[[logit_col]][vars == "Adj. R²"]), dash)
  expect_identical(trimws(d[[ols_col]][vars == "R² (McFadden)"]), dash)
  expect_identical(trimws(d[[ols_col]][vars == "R² (Tjur)"]), dash)
  # A stat BOTH classes define keeps its two values (no dash).
  # The row selector is a typed-out label: rename it and `vars == "AIC"`
  # selects nothing, `dash %in% character(0)` is FALSE, and the two
  # assertions below pass while testing nothing. Pin the row first.
  expect_true("AIC" %in% vars)
  expect_false(dash %in% trimws(d[[ols_col]][vars == "AIC"]))
  expect_false(dash %in% trimws(d[[logit_col]][vars == "AIC"]))
  # The body's absent-term cells stay BLANK (dash is a fit-stat signal,
  # not a term-absence one).
  expect_identical(trimws(d[[logit_col]][vars == "wt"]), "")
})


# ============================================================================
# Phase 3 matrix – rd-uv-estimands:n-events-fit-stat-cox-default
# ============================================================================

test_that("n_events fills for Cox and stays blank for glm in a mixed table", {
  # rd-uv-estimands:n-events-fit-stat-cox-default (mixed-table half:
  # the documented blank cell for other classes, a structural count
  # row, NOT the mixed-table en-dash)
  skip_if_not_installed("survival")
  set.seed(21)
  n <- 160
  d <- data.frame(x = rnorm(n), time = rexp(n), status = rbinom(n, 1, 0.7))
  cx <- survival::coxph(survival::Surv(time, status) ~ x, data = d)
  gl <- glm(status ~ x, data = d, family = binomial)
  out <- table_regression(
    list(Cox = cx, Logit = gl),
    show_fit_stats = c("nobs", "n_events", "aic")
  )
  # The printed table: its columns are the displayed cells.
  dd <- structure(out, class = "data.frame")
  vars <- trimws(dd$Variable)
  cox_col <- grep("^Cox", names(dd), value = TRUE)[1L]
  logit_col <- grep("^Logit", names(dd), value = TRUE)[1L]
  expect_identical(
    trimws(dd[[cox_col]][vars == "N events"]),
    as.character(cx$nevent)
  )
  expect_identical(trimws(dd[[logit_col]][vars == "N events"]), "")
  s <- as_structured(out)
  b <- s$body
  expect_equal(
    b[["Cox: B"]][trimws(b$Variable) == "N events"],
    as.numeric(cx$nevent)
  )
  expect_true(is.na(b[["Logit: B"]][trimws(b$Variable) == "N events"]))
  # The class-aware Cox default resolves to the documented triple.
  out_def <- table_regression(cx)
  v <- trimws(out_def$Variable)
  k <- length(v)
  expect_identical(v[(k - 2):k], c("n", "N events", "AIC"))
  s_def <- as_structured(out_def)
  expect_equal(
    s_def$body[[2L]][trimws(s_def$body$Variable) == "N events"],
    as.numeric(cx$nevent)
  )
})
