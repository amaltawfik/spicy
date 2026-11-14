# The pseudo-R2 of table_regression() on the Bernoulli trials of a binomial
# fit, whatever the form of its response, and with weights
# (dev/decisions/2026-11-14-pseudo-r2-defaults.md). Every oracle is the
# same model refitted to the expanded 0/1 rows, computed from
# stats::logLik() of that fit and from the closed-form log-likelihood of
# its intercept-only model.

ctl <- glm.control(epsilon = 1e-14, maxit = 100)

# McFadden, Nagelkerke and Tjur of an unweighted fit to 0/1 rows, without
# an offset (its intercept-only model predicts mean(y)).
expanded_oracle <- function(fit) {
  y <- fit$y
  p <- fitted(fit)
  n <- length(y)
  ll1 <- as.numeric(logLik(fit))
  ll0 <- sum(dbinom(y, 1, mean(y), log = TRUE))
  cs <- 1 - exp(2 * (ll0 - ll1) / n)
  c(
    mcfadden = 1 - ll1 / ll0,
    nagelkerke = cs / (1 - exp(2 * ll0 / n)),
    tjur = mean(p[y == 1]) - mean(p[y == 0])
  )
}

spicy_pr2 <- function(fit) {
  c(
    mcfadden = spicy:::compute_pseudo_r2_mcfadden(fit),
    nagelkerke = spicy:::compute_pseudo_r2_nagelkerke(fit),
    tjur = spicy:::compute_pseudo_r2_tjur(fit)
  )
}

# 30 groups of 10 trials, the same data as 0/1 rows weighted by counts,
# and as 300 expanded 0/1 rows.
grouped_data <- function() {
  set.seed(9)
  g <- data.frame(x = rnorm(30), m = 10)
  g$s <- rbinom(30, 10, plogis(-0.4 + 0.8 * g$x))
  long <- data.frame(
    x = rep(g$x, 2),
    y = rep(c(1, 0), each = 30),
    w = c(g$s, g$m - g$s)
  )
  long <- long[long$w > 0, ]
  list(
    g = g,
    long = long,
    ex = long[rep(seq_len(nrow(long)), long$w), c("x", "y")]
  )
}

test_that("a grouped binomial gives the pseudo-R2 of its expanded 0/1 rows", {
  dd <- grouped_data()
  ex <- glm(y ~ x, binomial, dd$ex, control = ctl)
  oracle <- expanded_oracle(ex)
  fits <- list(
    cbind = glm(cbind(s, m - s) ~ x, binomial, dd$g, control = ctl),
    proportion = glm(I(s / m) ~ x, binomial, dd$g, weights = m, control = ctl),
    counts = glm(y ~ x, binomial, dd$long, weights = w, control = ctl),
    expanded = ex
  )
  for (nm in names(fits)) {
    expect_equal(spicy_pr2(fits[[nm]]), oracle, tolerance = 1e-10, info = nm)
  }
  # The values the grouped form used to show: McFadden and Nagelkerke on
  # the binomial log-likelihood of the 30 rows, Tjur absent.
  ll1 <- as.numeric(logLik(fits$cbind))
  ll0 <- as.numeric(logLik(update(fits$cbind, . ~ 1)))
  expect_gt(abs((1 - ll1 / ll0) - oracle[["mcfadden"]]), 0.1)
  skip_if_not_installed("performance")
  expect_equal(
    oracle,
    c(
      mcfadden = unname(performance::r2_mcfadden(ex)$R2),
      nagelkerke = unname(performance::r2_nagelkerke(ex)),
      tjur = unname(performance::r2_tjur(ex))
    ),
    tolerance = 1e-10
  )
})

test_that("the table rows of the grouped forms are those of the expanded fit", {
  dd <- grouped_data()
  toks <- c(
    "nobs",
    "pseudo_r2_tjur",
    "pseudo_r2_mcfadden",
    "pseudo_r2_nagelkerke"
  )
  rows <- function(fit) {
    df <- table_regression(fit, show_fit_stats = toks, output = "data.frame")
    stats::setNames(df[["B"]][df$.row_role == "fit_stat"], toks)
  }
  ex <- rows(glm(y ~ x, binomial, dd$ex, control = ctl))
  cb <- rows(glm(cbind(s, m - s) ~ x, binomial, dd$g, control = ctl))
  pw <- rows(glm(I(s / m) ~ x, binomial, dd$g, weights = m, control = ctl))
  # n stays the number of rows of each fit; the three R2 agree.
  expect_identical(unname(c(ex[1L], cb[1L], pw[1L])), c(300, 30, 30))
  expect_equal(cb[-1L], ex[-1L], tolerance = 1e-10)
  expect_equal(pw[-1L], ex[-1L], tolerance = 1e-10)
  # The default of a grouped fit is McFadden alone.
  df <- table_regression(
    glm(cbind(s, m - s) ~ x, binomial, dd$g),
    output = "data.frame"
  )
  expect_identical(
    df$variable[df$.row_role == "fit_stat"],
    c("nobs", "pseudo_r2_mcfadden", "aic")
  )
})

test_that("non-integer weights: the weighted Bernoulli likelihood, n = sum of weights", {
  set.seed(3)
  d <- data.frame(x = rnorm(300))
  d$y <- rbinom(300, 1, plogis(-0.3 + d$x))
  d$w <- runif(300, 0.2, 3)
  fit <- suppressWarnings(glm(y ~ x, binomial, d, weights = w, control = ctl))
  p <- fitted(fit)
  ll1 <- sum(d$w * dbinom(d$y, 1, p, log = TRUE))
  ll0 <- sum(d$w * dbinom(d$y, 1, weighted.mean(d$y, d$w), log = TRUE))
  n <- sum(d$w)
  cs <- 1 - exp(2 * (ll0 - ll1) / n)
  oracle <- c(
    mcfadden = 1 - ll1 / ll0,
    nagelkerke = cs / (1 - exp(2 * ll0 / n)),
    tjur = weighted.mean(p[d$y == 1], d$w[d$y == 1]) -
      weighted.mean(p[d$y == 0], d$w[d$y == 0])
  )
  expect_equal(spicy_pr2(fit), oracle, tolerance = 1e-10)
  # Rescaling the weights changes nothing.
  fit10 <- suppressWarnings(
    glm(y ~ x, binomial, d, weights = 10 * w, control = ctl)
  )
  expect_equal(spicy_pr2(fit10), oracle, tolerance = 1e-10)
  # stats::logLik() rounds the weights: it is not the likelihood maximized.
  expect_gt(abs(as.numeric(logLik(fit)) - ll1), 1)
})

test_that("count weights of Poisson, negative binomial and multinom fits give n", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("nnet")
  set.seed(11)
  d <- data.frame(x = rnorm(120), f = sample(1:4, 120, TRUE))
  d$cnt <- rpois(120, exp(0.4 + 0.3 * d$x))
  d$mn <- factor(sample(c("A", "B", "C"), 120, TRUE))
  ex <- d[rep(seq_len(nrow(d)), d$f), ]
  nag <- function(fit) {
    fs <- as_regression_frame(fit)$info$fit_stats
    c(fs$pseudo_r2_mcfadden, fs$pseudo_r2_nagelkerke)
  }
  # The oracle: the expanded fit, n = its rows.
  ll_oracle <- function(ll1, ll0, n) {
    cs <- 1 - exp(2 * (ll0 - ll1) / n)
    c(1 - ll1 / ll0, cs / (1 - exp(2 * ll0 / n)))
  }
  # Without an offset, the intercept-only Poisson and negative binomial
  # models (theta fixed at the fitted model's) predict the mean.
  pe <- glm(cnt ~ x, poisson, ex, control = ctl)
  expect_equal(
    nag(glm(cnt ~ x, poisson, d, weights = f, control = ctl)),
    ll_oracle(
      as.numeric(logLik(pe)),
      sum(dpois(ex$cnt, mean(ex$cnt), log = TRUE)),
      nrow(ex)
    ),
    tolerance = 1e-10
  )
  ne <- MASS::glm.nb(cnt ~ x, ex)
  expect_equal(
    nag(MASS::glm.nb(cnt ~ x, d, weights = f)),
    ll_oracle(
      as.numeric(logLik(ne)),
      sum(dnbinom(ex$cnt, size = ne$theta, mu = mean(ex$cnt), log = TRUE)),
      nrow(ex)
    ),
    tolerance = 1e-6
  )
  me <- nnet::multinom(mn ~ x, ex, trace = FALSE, reltol = 1e-14)
  me0 <- nnet::multinom(mn ~ 1, ex, trace = FALSE, reltol = 1e-14)
  expect_equal(
    nag(nnet::multinom(mn ~ x, d, weights = f, trace = FALSE, reltol = 1e-14)),
    ll_oracle(as.numeric(logLik(me)), as.numeric(logLik(me0)), nrow(ex)),
    tolerance = 1e-7
  )
})

test_that("a weighted binary lrm gives the R2 of its expanded data", {
  skip_if_not_installed("rms")
  set.seed(11)
  d <- data.frame(x = rnorm(120), f = sample(1:4, 120, TRUE))
  d$y <- rbinom(120, 1, plogis(0.3 + d$x))
  ex <- d[rep(seq_len(nrow(d)), d$f), ]
  rows <- function(fit) {
    df <- table_regression(
      fit,
      show_fit_stats = c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke"),
      output = "data.frame"
    )
    df[["B"]][df$.row_role == "fit_stat"]
  }
  w_lrm <- suppressWarnings(rms::lrm(y ~ x, d, weights = f))
  e_glm <- glm(y ~ x, binomial, ex, control = ctl)
  expect_equal(
    rows(w_lrm),
    unname(expanded_oracle(e_glm)[c("mcfadden", "nagelkerke")]),
    tolerance = 1e-6
  )
  # rms's own R2 is Nagelkerke's with n the sum of the weights.
  expect_equal(rows(w_lrm)[2L], unname(w_lrm$stats[["R2"]]), tolerance = 1e-12)
})

test_that("Tjur keeps its sign: a fixed offset can make it negative", {
  set.seed(3)
  d <- data.frame(x = rnorm(200))
  d$y <- rbinom(200, 1, plogis(d$x))
  fit <- glm(y ~ 1, binomial, d, offset = -3 * x)
  p <- fitted(fit)
  oracle <- mean(p[d$y == 1]) - mean(p[d$y == 0])
  expect_lt(oracle, 0)
  expect_equal(spicy:::compute_pseudo_r2_tjur(fit), oracle, tolerance = 1e-12)
  # Defined for other links too, as the same difference of means.
  pr <- glm(y ~ x, binomial("probit"), d)
  pp <- fitted(pr)
  expect_equal(
    spicy:::compute_pseudo_r2_tjur(pr),
    mean(pp[d$y == 1]) - mean(pp[d$y == 0]),
    tolerance = 1e-12
  )
})

test_that("na.exclude fits give the values of their na.omit twins", {
  d <- data.frame(
    x = c(1, 2, 3, 4, NA, 6, 7, 8, 9, 10, 2.5, 5.5),
    y = c(0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0)
  )
  ex <- glm(y ~ x, binomial, d, na.action = stats::na.exclude)
  om <- glm(y ~ x, binomial, d, na.action = stats::na.omit)
  expect_equal(spicy_pr2(ex), spicy_pr2(om), tolerance = 1e-12)
  expect_equal(spicy_pr2(om), expanded_oracle(om), tolerance = 1e-10)
})

test_that("a fit kept without its response (y = FALSE) gives the same values", {
  dd <- grouped_data()
  with_y <- glm(cbind(s, m - s) ~ x, binomial, dd$g, control = ctl)
  without_y <- glm(
    cbind(s, m - s) ~ x,
    binomial,
    dd$g,
    control = ctl,
    y = FALSE
  )
  expect_null(without_y$y)
  expect_equal(spicy_pr2(without_y), spicy_pr2(with_y), tolerance = 1e-12)
  d <- data.frame(x = dd$ex$x, yf = factor(dd$ex$y, labels = c("no", "yes")))
  ff <- glm(yf ~ x, binomial, d, control = ctl, y = FALSE)
  expect_equal(
    spicy_pr2(ff),
    expanded_oracle(glm(y ~ x, binomial, dd$ex, control = ctl)),
    tolerance = 1e-10
  )
})

test_that("McFadden and Nagelkerke are NA where undefined", {
  na2 <- list(mcfadden = NA_real_, nagelkerke = NA_real_)
  expect_identical(spicy:::.lr_pseudo_r2(-10, 0, 20), na2)
  expect_identical(spicy:::.lr_pseudo_r2(NA_real_, -20, 20), na2)
  expect_identical(spicy:::.lr_pseudo_r2(-10, -20, 0), na2)
  # A null log-likelihood above 0 (a density) leaves Nagelkerke's maximum
  # at or below 0: McFadden is a number, Nagelkerke is not.
  out <- spicy:::.lr_pseudo_r2(30, 20, 40)
  expect_equal(out$mcfadden, 1 - 30 / 20)
  expect_identical(out$nagelkerke, NA_real_)
})
