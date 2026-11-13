# `ame_type`: the quantity an AME row reports for a continuous predictor.
# The slope is avg_slopes(); "unit" and "sd" are avg_comparisons() from
# each observation's own value, pinned to marginaleffects with an explicit
# step and, where one exists, to the closed form of the ADR
# dev/decisions/2026-09-05-ame-slope-vs-unit-change.md.

ame_long <- function(fit, type, ...) {
  out <- suppressWarnings(table_regression(
    fit,
    show_columns = c("b", "ame", "ame_se"),
    ame_type = type,
    output = "long",
    ...
  ))
  out[out$estimate_type == "ame" & !(out$is_reference %in% TRUE), ]
}
ame_of <- function(fit, type, term = "x", ...) {
  a <- ame_long(fit, type, ...)
  a$estimate[a$term == term]
}
gap_unit <- function(x) data.frame(lo = x, hi = x + 1)
gap_sd <- function(x) data.frame(lo = x, hi = x + stats::sd(x))
oracle_cmp <- function(fit, gap, term = "x", ...) {
  o <- suppressWarnings(marginaleffects::avg_comparisons(
    fit,
    variables = stats::setNames(list(gap), term),
    ...
  ))
  o$estimate
}

make_ame_d <- function(seed = 3, n = 300) {
  set.seed(seed)
  d <- data.frame(
    x = rnorm(n),
    z = rnorm(n),
    f = factor(sample(c("a", "b", "c"), n, TRUE)),
    g = factor(sample(1:15, n, TRUE)),
    cnt = rpois(n, 3),
    o = log(runif(n, 1, 3)),
    w = runif(n, 0.5, 2)
  )
  d$yb <- rbinom(
    n,
    1,
    plogis(-0.2 + 0.9 * d$x + 0.3 * d$z + 0.4 * (d$f == "b"))
  )
  d$yg <- rgamma(n, shape = 3, rate = 3 / exp(0.5 + 0.6 * d$x))
  d$yp <- rpois(n, exp(0.3 + 0.4 * d$x + d$o))
  d
}

test_that("ame_type on a Gamma log GLM matches the closed forms of the ADR", {
  skip_if_not_installed("marginaleffects")
  d <- make_ame_d()
  m <- glm(yg ~ x + f, family = Gamma(link = "log"), data = d)
  b <- unname(coef(m)["x"])
  mu <- fitted(m)
  expect_equal(ame_of(m, "slope"), b * mean(mu), tolerance = 1e-6)
  expect_equal(ame_of(m, "unit"), (exp(b) - 1) * mean(mu), tolerance = 1e-8)
  expect_equal(
    ame_of(m, "sd"),
    (exp(b * sd(d$x)) - 1) * mean(mu),
    tolerance = 1e-8
  )
  # marginaleffects' own forward step of 1, estimate and SE.
  orc <- marginaleffects::avg_comparisons(m, variables = list(x = 1))
  a <- ame_long(m, "unit")
  expect_equal(a$estimate[a$term == "x"], orc$estimate, tolerance = 1e-8)
  expect_equal(a$std.error[a$term == "x"], orc$std.error, tolerance = 1e-6)
  # The factor contrast is the same number under every value.
  fb <- vapply(
    c("slope", "unit", "sd"),
    function(t) ame_of(m, t, term = "fb"),
    numeric(1)
  )
  expect_equal(unname(fb[2:3]), unname(rep(fb[1], 2)), tolerance = 1e-10)
})

test_that("ame_type on a logit matches the closed form and marginaleffects", {
  skip_if_not_installed("marginaleffects")
  d <- make_ame_d()
  m <- glm(yb ~ x + z + f, family = binomial, data = d)
  eta <- predict(m)
  b <- unname(coef(m)["x"])
  s <- sd(d$x)
  expect_equal(
    ame_of(m, "unit"),
    mean(plogis(eta + b) - plogis(eta)),
    tolerance = 1e-8
  )
  expect_equal(
    ame_of(m, "sd"),
    mean(plogis(eta + b * s) - plogis(eta)),
    tolerance = 1e-8
  )
  expect_equal(ame_of(m, "sd"), oracle_cmp(m, gap_sd), tolerance = 1e-10)
  # The step is forward whatever the user's marginaleffects option says.
  old <- options(marginaleffects_contrast_direction = "center")
  on.exit(options(old), add = TRUE)
  expect_equal(
    ame_of(m, "unit"),
    mean(plogis(eta + b) - plogis(eta)),
    tolerance = 1e-8
  )
})

test_that("'sd' is avg_comparisons() with the SD of the estimation sample", {
  skip_if_not_installed("marginaleffects")
  d <- make_ame_d()
  # Rows the fit drops through another variable, x observed there.
  d$z[c(4, 9, 31)] <- NA
  d$x[c(4, 9, 31)] <- d$x[c(4, 9, 31)] + 6
  for (m in list(
    glm(yb ~ x + z, family = binomial, data = d),
    glm(yg ~ x + z, family = Gamma(link = "log"), data = d)
  )) {
    s <- sd(stats::model.frame(m)$x)
    orc <- marginaleffects::avg_comparisons(m, variables = list(x = s))
    a <- ame_long(m, "sd")
    expect_equal(a$estimate[a$term == "x"], orc$estimate, tolerance = 1e-10)
    expect_equal(a$std.error[a$term == "x"], orc$std.error, tolerance = 1e-8)
    # Neither the SD of the whole data nor marginaleffects' "sd" shortcut.
    whole <- marginaleffects::avg_comparisons(
      m,
      variables = list(x = sd(d$x))
    )
    short <- marginaleffects::avg_comparisons(m, variables = list(x = "sd"))
    expect_false(isTRUE(all.equal(orc$estimate, whole$estimate)))
    expect_false(isTRUE(all.equal(orc$estimate, short$estimate)))
  }
})

test_that("ame_type = 'unit' honors weights, interactions, transforms and counts", {
  skip_if_not_installed("marginaleffects")
  d <- make_ame_d()
  shift <- function(fit, v, by = 1) {
    nd <- d
    nd[[v]] <- nd[[v]] + by
    predict(fit, nd, type = "response") - fitted(fit)
  }
  mw <- suppressWarnings(glm(yb ~ x + z, binomial, d, weights = w))
  expect_equal(
    ame_of(mw, "unit"),
    stats::weighted.mean(shift(mw, "x"), d$w),
    tolerance = 1e-8
  )
  mi <- glm(yb ~ x * z, binomial, d)
  expect_equal(ame_of(mi, "unit"), mean(shift(mi, "x")), tolerance = 1e-8)
  d$xp <- exp(d$x)
  ml <- glm(yb ~ log(xp) + z, binomial, d)
  expect_equal(
    ame_of(ml, "unit", term = "log(xp)"),
    mean(shift(ml, "xp")),
    tolerance = 1e-8
  )
  mp <- glm(yb ~ poly(x, 2) + z, binomial, d)
  expect_equal(ame_of(mp, "unit"), mean(shift(mp, "x")), tolerance = 1e-8)
  mc <- glm(yb ~ cnt + z, binomial, d)
  expect_equal(
    ame_of(mc, "unit", term = "cnt"),
    mean(shift(mc, "cnt")),
    tolerance = 1e-8
  )
  # A variable present only in offset() has no AME row under any value.
  mo <- glm(yp ~ x + offset(o), poisson, d)
  expect_identical(ame_long(mo, "unit")$term, "x")
  expect_identical(ame_long(mo, "sd")$term, "x")
})

test_that("ame_type reaches mixed models, the robust glmmTMB path and multinom", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("nnet")
  d <- make_ame_d()
  mg <- lme4::glmer(yb ~ x + z + (1 | g), data = d, family = binomial)
  expect_equal(ame_of(mg, "unit"), oracle_cmp(mg, gap_unit), tolerance = 1e-8)
  mt <- glmmTMB::glmmTMB(yb ~ x + z + (1 | g), family = binomial, data = d)
  expect_equal(ame_of(mt, "sd"), oracle_cmp(mt, gap_sd), tolerance = 1e-8)
  # Under a cluster-robust matrix the estimate is the same number.
  expect_equal(
    ame_of(mt, "sd", vcov = "CR1", cluster = ~g),
    oracle_cmp(mt, gap_sd),
    tolerance = 1e-8
  )
  d$yo <- cut(
    0.8 * d$x + rlogis(nrow(d)),
    c(-Inf, -0.5, 0.7, Inf),
    labels = c("lo", "mid", "hi")
  )
  mn <- nnet::multinom(yo ~ x + z, data = d, trace = FALSE)
  orc <- marginaleffects::avg_comparisons(mn, variables = list(x = gap_unit))
  a <- ame_long(mn, "unit")
  a <- a[grepl("x$", a$term), ]
  expect_equal(
    sort(a$estimate),
    sort(orc$estimate[orc$term == "x"]),
    tolerance = 1e-8
  )
})

test_that("the closed-form lm path takes the step of ame_type", {
  skip_if_not_installed("clubSandwich")
  d <- make_ame_d()
  d$b01 <- as.numeric(d$z > 0)
  m <- lm(yg ~ x + b01 + f, data = d)
  b <- unname(coef(m)["x"])
  expect_equal(ame_of(m, "unit", vcov = "CR2", cluster = ~g), b)
  expect_equal(ame_of(m, "sd", vcov = "CR2", cluster = ~g), b * sd(d$x))
  # A 0/1 predictor stays the contrast of its two values.
  expect_equal(
    ame_of(m, "sd", term = "b01", vcov = "CR2", cluster = ~g),
    unname(coef(m)["b01"])
  )
})

test_that("a table without a continuous predictor, or a failure, under 'unit'", {
  skip_if_not_installed("marginaleffects")
  d <- make_ame_d()
  m <- glm(yb ~ f, family = binomial, data = d)
  expect_equal(
    ame_long(m, "unit")$estimate,
    ame_long(m, "slope")$estimate,
    tolerance = 1e-12
  )
  local_mocked_bindings(.spicy_avg_ame = function(...) stop("no step"))
  expect_warning(
    table_regression(m, show_columns = c("b", "ame"), ame_type = "unit"),
    "avg_comparisons",
    class = "spicy_fallback"
  )
})

test_that("ordinal fits with an offset step the predictor of ame_type", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("MASS")
  skip_if_not_installed("rms")
  d <- make_ame_d()
  d$yo <- cut(
    0.8 * d$x + rlogis(nrow(d)),
    c(-Inf, -0.5, 0.7, Inf),
    labels = c("lo", "mid", "hi"),
    ordered_result = TRUE
  )
  m <- MASS::polr(yo ~ x + offset(0.3 * z), data = d, Hess = TRUE)
  b <- unname(coef(m))
  eta <- b * d$x + 0.3 * d$z
  probs <- function(e) {
    cum <- t(plogis(outer(m$zeta, e, "-")))
    cbind(cum, 1) - cbind(0, cum)
  }
  expect_equal(
    ame_long(m, "unit")$estimate,
    unname(colMeans(probs(eta + b) - probs(eta))),
    tolerance = 1e-6
  )
  expect_equal(
    ame_long(m, "sd")$estimate,
    unname(colMeans(probs(eta + b * sd(d$x)) - probs(eta))),
    tolerance = 1e-6
  )
  # An ordinal lrm (category probabilities from its tails) gives polr's.
  ml <- rms::lrm(yo ~ x + z, data = d)
  mp <- MASS::polr(yo ~ x + z, data = d, Hess = TRUE)
  expect_equal(
    ame_long(ml, "unit")$estimate[1:3],
    ame_long(mp, "unit")$estimate[1:3],
    tolerance = 1e-5
  )
})

test_that("a Bayesian fit summarizes the draws of the requested quantity", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("collapse")
  d <- make_ame_d(n = 120)
  fit <- suppressWarnings(rstanarm::stan_glm(
    yb ~ x + z,
    family = binomial(),
    data = d,
    chains = 1,
    iter = 400,
    refresh = 0,
    seed = 11
  ))
  orc <- marginaleffects::posterior_draws(
    marginaleffects::avg_comparisons(fit, variables = list(x = gap_unit))
  )
  expect_equal(ame_of(fit, "unit"), stats::median(orc$draw), tolerance = 1e-8)
})

test_that("the note names the AME quantity under every ame_type", {
  skip_if_not_installed("marginaleffects")
  d <- make_ame_d()
  m <- glm(yb ~ x + f, family = binomial, data = d)
  note_of <- function(t, lang = "en") {
    old <- options(spicy.language = lang)
    on.exit(options(old))
    attr(
      table_regression(m, show_columns = c("b", "ame"), ame_type = t),
      "note"
    )
  }
  expect_match(note_of("slope"), "average slope (dY/dX)", fixed = TRUE)
  expect_match(note_of("unit"), "for a one-unit increase", fixed = TRUE)
  expect_match(
    note_of("sd"),
    "for a one-standard-deviation increase",
    fixed = TRUE
  )
  expect_match(
    note_of("unit", "fr"),
    "pour une unit\u00E9 de plus",
    fixed = TRUE
  )
  expect_match(
    note_of("sd", "fr"),
    "pour un \u00E9cart-type de plus",
    fixed = TRUE
  )
  # No continuous predictor, no sentence.
  mf <- glm(yb ~ f, family = binomial, data = d)
  nf <- attr(table_regression(mf, show_columns = c("b", "ame")), "note")
  expect_no_match(nf, "continuous predictor", fixed = TRUE)
})

test_that("ame_type without an AME column warns, and a bad value is refused", {
  d <- make_ame_d()
  m <- glm(yb ~ x, family = binomial, data = d)
  expect_warning(
    table_regression(m, ame_type = "unit"),
    class = "spicy_ignored_arg"
  )
  expect_error(
    table_regression(m, show_columns = c("b", "ame"), ame_type = "elasticity"),
    class = "spicy_invalid_input"
  )
})

test_that("a glmmTMB cbind() binomial AME is weighted by the trials", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  set.seed(7)
  n <- 200
  d <- data.frame(
    x = rnorm(n),
    g = factor(sample(1:10, n, TRUE)),
    size = sample(2:12, n, TRUE),
    pw = runif(n, 0.5, 2)
  )
  d$s <- rbinom(n, d$size, plogis(-0.3 + 0.7 * d$x))
  d$f <- d$size - d$s
  mt <- suppressWarnings(glmmTMB::glmmTMB(
    cbind(s, f) ~ x + (1 | g),
    family = binomial,
    data = d
  ))
  # The trials come from the response, whatever weights() returns.
  expect_identical(.spicy_ame_fit_wts(mt), as.numeric(d$size))
  # The random-effect variance is ~0 here: glm and glmer give the same
  # trial-weighted AME, and so does marginaleffects told the trials.
  ame_mt <- ame_of(mt, "slope")
  expect_equal(
    ame_mt,
    ame_of(glm(cbind(s, f) ~ x, family = binomial, data = d), "slope"),
    tolerance = 1e-8
  )
  mg <- suppressMessages(lme4::glmer(
    cbind(s, f) ~ x + (1 | g),
    family = binomial,
    data = d
  ))
  expect_equal(ame_mt, ame_of(mg, "slope"), tolerance = 1e-8)
  orc <- suppressWarnings(
    marginaleffects::avg_slopes(mt, variables = "x", wts = d$size)
  )
  expect_equal(ame_mt, orc$estimate, tolerance = 1e-10)
  # Prior weights multiply the trials.
  mw <- suppressWarnings(glmmTMB::glmmTMB(
    cbind(s, f) ~ x + (1 | g),
    family = binomial,
    data = d,
    weights = pw
  ))
  expect_equal(.spicy_ame_fit_wts(mw), d$size * d$pw)
  # A 0/1 response has no trials to read.
  d$yb <- as.integer(d$s > d$f)
  mb <- glmmTMB::glmmTMB(yb ~ x, family = binomial, data = d)
  expect_null(.glmmTMB_binomial_trials(mb))
})
