# C2 audit finding #1 (critical): rank-deficient fits under HC* / CR*.
# When lm()/glm() drops a perfectly collinear column it keeps an NA entry in
# stats::coef(fit), but sandwich::vcovHC() / clubSandwich::vcovCR() / coef_test()
# DROP that column -> the robust matrix is narrower than the full coef vector.
# compute_coef_inference() must index those by coefficient NAME (not full-vector
# position) so every non-dropped coef gets ITS OWN robust SE/df and no genuine
# coefficient is silently NA. Regression guard for the name-based indexing fix.

b_rows <- function(fr) {
  fr$coefs[fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE), ]
}

make_collinear_lm <- function(seed = 11, n = 200) {
  set.seed(seed)
  d <- data.frame(
    x1 = rnorm(n),
    g  = factor(sample(15, n, TRUE)),
    f3 = factor(sample(c("a", "b", "c"), n, TRUE))
  )
  d$x1copy <- d$x1                       # perfectly collinear -> dropped
  d$y <- 1 + 0.5 * d$x1 + as.integer(d$f3) * 0.3 + rnorm(n)
  list(fit = lm(y ~ x1 + x1copy + f3, data = d), data = d)
}

test_that("rank-deficient lm HC3: each non-dropped coef SE == vcovHC, dropped is NA", {
  skip_if_not_installed("sandwich")
  o <- make_collinear_lm()
  expect_true(any(is.na(stats::coef(o$fit))))      # x1copy dropped
  b <- b_rows(as_regression_frame(o$fit, vcov = "HC3"))

  # The dropped coefficient is a singular NA row (em-dash), as for the classical
  # path -- never a borrowed neighbour variance.
  dropped <- b[b$term == "x1copy", ]
  expect_identical(nrow(dropped), 1L)
  expect_true(is.na(dropped$std_error))

  # Every NON-dropped coefficient equals the sandwich oracle for the SAME coef.
  orc <- sqrt(diag(sandwich::vcovHC(o$fit, type = "HC3")))
  nondrop <- b[b$term != "x1copy", ]
  expect_false(any(is.na(nondrop$std_error)))
  for (nm in names(orc)) {
    expect_equal(nondrop$std_error[match(nm, nondrop$term)],
                 unname(orc[[nm]]), tolerance = 1e-9,
                 info = paste("coef", nm))
  }
})

test_that("rank-deficient lm CR2: each non-dropped coef SE+df == coef_test, dropped is NA", {
  skip_if_not_installed("clubSandwich")
  o <- make_collinear_lm()
  b <- b_rows(as_regression_frame(o$fit, vcov = "CR2", cluster = o$data$g))
  expect_true(is.na(b$std_error[b$term == "x1copy"]))

  ct <- clubSandwich::coef_test(
    o$fit,
    vcov = clubSandwich::vcovCR(o$fit, type = "CR2", cluster = o$data$g),
    cluster = o$data$g, test = "Satterthwaite"
  )
  nondrop <- b[b$term != "x1copy", ]
  expect_false(any(is.na(nondrop$std_error)))
  for (nm in rownames(ct)) {
    i <- match(nm, nondrop$term)
    expect_equal(nondrop$std_error[i], ct[nm, "SE"], tolerance = 1e-9, info = nm)
    expect_equal(nondrop$df[i],        ct[nm, "df_Satt"], tolerance = 1e-6, info = nm)
  }
})

test_that("rank-deficient glm (logistic) HC3 indexes robust SE by name", {
  skip_if_not_installed("sandwich")
  set.seed(21)
  n <- 240
  d <- data.frame(x1 = rnorm(n), f = factor(sample(c("a", "b", "c"), n, TRUE)))
  d$x1dup <- d$x1                                   # collinear -> dropped
  eta <- -0.2 + 0.7 * d$x1 + as.integer(d$f) * 0.2
  d$y <- rbinom(n, 1, plogis(eta))
  fit <- glm(y ~ x1 + x1dup + f, data = d, family = binomial)
  expect_true(any(is.na(stats::coef(fit))))
  b <- b_rows(as_regression_frame(fit, vcov = "HC3"))
  orc <- sqrt(diag(sandwich::vcovHC(fit, type = "HC3")))
  nondrop <- b[b$term != "x1dup", ]
  expect_false(any(is.na(nondrop$std_error)))
  for (nm in names(orc)) {
    expect_equal(nondrop$std_error[match(nm, nondrop$term)],
                 unname(orc[[nm]]), tolerance = 1e-9, info = nm)
  }
})

test_that("full-rank lm is unaffected by name-based indexing (no regression)", {
  skip_if_not_installed("clubSandwich")
  set.seed(12)
  d <- data.frame(y = rnorm(150), x1 = rnorm(150), x2 = rnorm(150),
                  g = factor(sample(10, 150, TRUE)))
  m <- lm(y ~ x1 + x2, data = d)
  b <- b_rows(as_regression_frame(m, vcov = "CR2", cluster = d$g))
  ct <- clubSandwich::coef_test(
    m, vcov = clubSandwich::vcovCR(m, type = "CR2", cluster = d$g),
    cluster = d$g, test = "Satterthwaite"
  )
  expect_equal(b$std_error, unname(ct[b$term, "SE"]), tolerance = 1e-12)
})
