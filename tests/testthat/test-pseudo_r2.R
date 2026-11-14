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
  d$cnt <- rnbinom(120, mu = exp(0.4 + 0.3 * d$x), size = 2)
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

# ---- the deviance R2 of the other glm families -------------------------

continuous_data <- function() {
  set.seed(20261114)
  n <- 200
  d <- data.frame(x = rnorm(n), t = runif(n, 1, 3))
  d$yg <- rgamma(n, shape = 3, rate = 3 / exp(1 + 0.3 * d$x))
  d$yn <- 5 + 0.8 * d$x + rnorm(n)
  d$cnt <- rnbinom(n, mu = exp(0.3 + 0.5 * d$x) * d$t, size = 1.5)
  d$w <- runif(n, 0.5, 2)
  d
}

test_that("the deviance R2 is 1 - deviance / null deviance, family by family", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("statmod")
  d <- continuous_data()
  d$yi <- statmod::rinvgauss(nrow(d), mean = exp(1 + 0.3 * d$x), shape = 4)
  d$yb <- rbinom(nrow(d), 1, plogis(d$x))
  fits <- list(
    gamma_log = glm(yg ~ x, Gamma("log"), d),
    gamma_inverse = glm(yg ~ x, Gamma(), d),
    inverse_gaussian = glm(yi ~ x, inverse.gaussian("log"), d),
    gaussian_log = glm(yn ~ x, gaussian("log"), d),
    gaussian_weighted = glm(yn ~ x, gaussian, d, weights = w),
    tweedie = glm(
      yg ~ x,
      statmod::tweedie(var.power = 1.5, link.power = 0),
      d
    ),
    poisson_offset = glm(cnt ~ x + offset(log(t)), poisson, d),
    negative_binomial_fixed = glm(cnt ~ x, MASS::negative.binomial(2), d),
    quasipoisson = glm(cnt ~ x, quasipoisson, d),
    quasibinomial = glm(yb ~ x, quasibinomial, d)
  )
  for (nm in names(fits)) {
    f <- fits[[nm]]
    expect_equal(
      spicy:::compute_r2_deviance(f),
      1 - f$deviance / f$null.deviance,
      tolerance = 1e-10,
      info = nm
    )
  }
  # A gaussian glm with the identity link: the R2 of lm().
  expect_equal(
    spicy:::compute_r2_deviance(glm(yn ~ x, gaussian, d, weights = w)),
    summary(lm(yn ~ x, d, weights = w))$r.squared,
    tolerance = 1e-12
  )
  # McFadden and Nagelkerke are not computed for the continuous families.
  for (nm in c("gamma_log", "inverse_gaussian", "gaussian_log", "tweedie")) {
    expect_identical(spicy:::compute_pseudo_r2_mcfadden(fits[[nm]]), NA_real_)
    expect_identical(
      spicy:::compute_pseudo_r2_nagelkerke(fits[[nm]]),
      NA_real_
    )
  }
})

test_that("the deviance R2 does not move with the unit of y; McFadden did", {
  d <- continuous_data()
  r2 <- mcf <- numeric(0)
  for (k in c(1e-3, 0.1, 1, 1000)) {
    dd <- d
    dd$y <- d$yg * k
    f <- glm(y ~ x, Gamma("log"), dd)
    r2 <- c(r2, spicy:::compute_r2_deviance(f))
    ll0 <- as.numeric(logLik(glm(y ~ 1, Gamma("log"), dd)))
    mcf <- c(mcf, 1 - as.numeric(logLik(f)) / ll0)
  }
  expect_equal(r2, rep(r2[1L], 4L), tolerance = 1e-10)
  # The likelihood-ratio index of the same model: negative on one unit,
  # positive on another (Cameron & Windmeijer 1997, p. 333).
  expect_lt(min(mcf), 0)
  expect_gt(max(mcf), 0)
})

test_that("glm.nb: both deviances take the theta of the fitted model", {
  skip_if_not_installed("MASS")
  d <- continuous_data()
  for (f in list(
    MASS::glm.nb(cnt ~ x, d),
    MASS::glm.nb(cnt ~ x + offset(log(t)), d)
  )) {
    expect_equal(
      spicy:::compute_r2_deviance(f),
      1 - f$deviance / f$null.deviance,
      tolerance = 1e-8
    )
    # Independent: the two deviances recomputed with theta fixed at the
    # fitted model's estimate (Cameron & Windmeijer 1997, p. 339, note 4).
    fam <- MASS::negative.binomial(f$theta)
    dev1 <- sum(fam$dev.resids(f$y, fitted(f), rep(1, length(f$y))))
    null <- glm(
      f$y ~ 1,
      family = fam,
      offset = stats::model.offset(stats::model.frame(f))
    )
    expect_equal(
      spicy:::compute_r2_deviance(f),
      1 - dev1 / null$deviance,
      tolerance = 1e-6
    )
  }
})

test_that("a quasi-binomial grouped fit gives the deviance R2 of its trials", {
  dd <- grouped_data()
  qb <- glm(cbind(s, m - s) ~ x, quasibinomial, dd$g, control = ctl)
  ex <- glm(y ~ x, binomial, dd$ex, control = ctl)
  # On the expanded 0/1 rows, the deviance R2 is McFadden's.
  expect_equal(
    spicy:::compute_r2_deviance(qb),
    1 - ex$deviance / ex$null.deviance,
    tolerance = 1e-10
  )
  expect_equal(
    spicy:::compute_r2_deviance(qb),
    expanded_oracle(ex)[["mcfadden"]],
    tolerance = 1e-10
  )
  # The grouped deviances give another number.
  expect_gt(
    abs(spicy:::compute_r2_deviance(qb) - (1 - qb$deviance / qb$null.deviance)),
    0.01
  )
})

test_that("the continuous families default to the deviance R2 and refuse the others", {
  d <- continuous_data()
  g <- glm(yg ~ x, Gamma("log"), d)
  df <- table_regression(g, output = "data.frame")
  fs <- df[df$.row_role == "fit_stat", ]
  expect_identical(fs$variable, c("nobs", "r2_deviance", "aic"))
  expect_equal(
    fs[["B"]][fs$variable == "r2_deviance"],
    1 - g$deviance / g$null.deviance,
    tolerance = 1e-10
  )
  expect_identical(
    fs$label[fs$variable == "r2_deviance"],
    "R² (deviance)"
  )
  for (tok in c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke")) {
    expect_error(
      table_regression(g, show_fit_stats = c("nobs", tok)),
      class = "spicy_invalid_input"
    )
  }
  # The same rows under nested = TRUE and show_columns = "none".
  g0 <- glm(yg ~ 1, Gamma("log"), d)
  nest <- table_regression(list(g0, g), nested = TRUE, output = "data.frame")
  expect_identical(
    nest$variable[nest$.row_role == "fit_stat"],
    c("nobs", "r2_deviance", "aic", "f_change", "p_change")
  )
  expect_equal(nest[["Model 1: B"]][nest$variable == "r2_deviance"], 0)
  only <- table_regression(g, show_columns = "none", output = "data.frame")
  expect_identical(only$variable, c("nobs", "r2_deviance", "aic"))
  # A gaussian glm shows the R2 of its lm twin.
  gs <- suppressWarnings(table_regression(
    glm(yn ~ x, gaussian, d),
    output = "data.frame"
  ))
  expect_equal(
    gs[["B"]][gs$variable == "r2_deviance"],
    summary(lm(yn ~ x, d))$r.squared,
    tolerance = 1e-12
  )
  # Beside a logit, McFadden asked for is the logit's and dashes the Gamma.
  d$yb <- rbinom(nrow(d), 1, plogis(d$x))
  lg <- glm(yb ~ x, binomial, d)
  mix <- table_regression(
    list(Logit = lg, Gamma = g),
    show_fit_stats = c("nobs", "pseudo_r2_mcfadden"),
    output = "data.frame"
  )
  row <- mix[mix$variable == "pseudo_r2_mcfadden", ]
  expect_true(is.finite(row[["Logit: B"]]))
  expect_true(is.na(row[["Gamma: B"]]))
  # An rms::Glm follows its glm twin.
  skip_if_not_installed("rms")
  rg <- rms::Glm(yg ~ x, family = Gamma("log"), data = d)
  expect_error(
    table_regression(rg, show_fit_stats = c("nobs", "pseudo_r2_mcfadden")),
    class = "spicy_invalid_input"
  )
  rdf <- table_regression(rg, output = "data.frame")
  expect_equal(
    rdf[["B"]][rdf$variable == "r2_deviance"],
    1 - g$deviance / g$null.deviance,
    tolerance = 1e-8
  )
})

test_that("a glm object without a family reads as an unknown family", {
  expect_identical(
    spicy:::.glm_r2_kind(structure(list(), class = c("glm", "lm"))),
    "other"
  )
})

test_that("the deviance R2 is refused where it would repeat McFadden's", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("survival")
  d <- continuous_data()
  d$yb <- rbinom(nrow(d), 1, plogis(d$x))
  d$ord <- factor(
    cut(d$x + rnorm(nrow(d)), 3, labels = c("a", "b", "c")),
    ordered = TRUE
  )
  for (fit in list(
    glm(yb ~ x, binomial, d),
    MASS::polr(ord ~ x, d, Hess = TRUE)
  )) {
    err <- tryCatch(
      table_regression(fit, show_fit_stats = c("nobs", "r2_deviance")),
      error = function(e) e
    )
    expect_s3_class(err, "spicy_invalid_input")
    expect_match(conditionMessage(err), "pseudo_r2_mcfadden", fixed = TRUE)
  }
  # A class with no deviance R2 at all is refused too.
  expect_error(
    table_regression(
      survival::coxph(survival::Surv(yg, rep(1, nrow(d))) ~ x, data = d),
      show_fit_stats = c("nobs", "r2_deviance")
    ),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(lm(yn ~ x, d), show_fit_stats = c("nobs", "r2_deviance")),
    class = "spicy_invalid_input"
  )
  # Offered for counts and quasi families, on request.
  pois <- glm(cnt ~ x, poisson, d)
  qp <- glm(cnt ~ x, quasipoisson, d)
  df <- table_regression(
    list(pois, qp),
    show_fit_stats = c("nobs", "r2_deviance"),
    output = "data.frame"
  )
  row <- df[df$variable == "r2_deviance", ]
  expect_equal(
    unname(unlist(row[c("Model 1: B", "Model 2: B")])),
    rep(1 - pois$deviance / pois$null.deviance, 2L),
    tolerance = 1e-10
  )
})

test_that("a table mixing families shows the union of the default rows", {
  d <- continuous_data()
  d$yb <- rbinom(nrow(d), 1, plogis(d$x))
  lg <- glm(yb ~ x, binomial, d)
  gm <- glm(yg ~ x, Gamma("log"), d)
  pois <- glm(cnt ~ x, poisson, d)
  df <- table_regression(
    list(Logit = lg, Gamma = gm, Poisson = pois),
    output = "data.frame"
  )
  fs <- df[df$.row_role == "fit_stat", ]
  expect_identical(
    fs$variable,
    c("nobs", "pseudo_r2_tjur", "pseudo_r2_mcfadden", "r2_deviance", "aic")
  )
  dev <- fs[fs$variable == "r2_deviance", ]
  expect_true(is.na(dev[["Logit: B"]]))
  expect_equal(dev[["Gamma: B"]], 1 - gm$deviance / gm$null.deviance)
  # The Poisson fit defines the row too and fills it.
  expect_equal(dev[["Poisson: B"]], 1 - pois$deviance / pois$null.deviance)
  mcf <- fs[fs$variable == "pseudo_r2_mcfadden", ]
  expect_true(is.na(mcf[["Gamma: B"]]))
  withr::local_options(spicy.language = "fr")
  out <- table_regression(list(Logit = lg, Gamma = gm))
  expect_true("R² (déviance)" %in% trimws(out$Variable))
})
