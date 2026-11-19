# AME of the survey and rms fits whose predictions on new data used to
# leave part of the fit out, checked against the hand oracle of
# helper-ame-oracle.R (point and SE at 1e-6), and the guard that refuses
# the AME when the model cannot predict its own fitted values.

skip_if_no_ame_survey <- function() {
  skip_if_not_installed("marginaleffects", minimum_version = "1.0.0")
  skip_if_not_installed("survey")
  skip_if_not_installed("numDeriv")
}

# Cumulative-logit category probabilities P(Y = j), from
# P(Y <= j) = plogis(zeta_j - eta), for the svyolr oracle.
ame_pf_cumlogit <- function(cuts, lev) {
  function(b, D) {
    X <- D$X[, colnames(D$X) != "(Intercept)", drop = FALSE]
    eta <- drop(X %*% b[colnames(X)])
    cum <- stats::plogis(outer(-eta, b[cuts], "+"))
    P <- cbind(cum, 1) - cbind(0, cum)
    colnames(P) <- lev
    P
  }
}

svyolr_oracle <- function(f, rhs, d) {
  ame_oracle(
    ame_prep_rhs(rhs, d),
    ame_pf_cumlogit(names(f$zeta), f$lev),
    c(f$coefficients, f$zeta),
    stats::vcov(f),
    d,
    c("x", "grp", "z"),
    d$sw
  )
}

svyglm_oracle <- function(f, rhs, d, linkinv, w = d$sw) {
  ame_oracle(
    ame_prep_rhs(rhs, d),
    ame_pf_link(linkinv),
    stats::coef(f),
    stats::vcov(f),
    d,
    c("x", "grp", "z"),
    w
  )
}


# ---- D1: svyolr, the variance aligned by name ------------------------------

test_that("the AME SE of a svyolr pairs each coefficient with its own variance", {
  skip_if_no_ame_survey()
  d <- ame_test_data()
  des <- survey::svydesign(id = ~id, weights = ~sw, data = d)
  f <- survey::svyolr(ord ~ x + grp + z, design = des)
  expect_ame_matches(ame_spicy(f), svyolr_oracle(f, ~ x + grp + z, d))
})

test_that("the AME SE of a svyolr on a replicate design is aligned by name", {
  skip_if_no_ame_survey()
  d <- ame_test_data()
  des <- survey::svydesign(id = ~id, weights = ~sw, data = d)
  rd <- survey::as.svrepdesign(des, type = "JK1")
  f <- survey::svyolr(ord ~ x + grp + z, design = rd)
  expect_ame_matches(ame_spicy(f), svyolr_oracle(f, ~ x + grp + z, d))
})


test_that("a svyolr whose variance names miss a coefficient gets no AME", {
  skip_if_no_ame_survey()
  d <- ame_test_data()
  des <- survey::svydesign(id = ~id, weights = ~sw, data = d)
  f <- survey::svyolr(ord ~ x + grp + z, design = des)
  nm <- colnames(f$var)
  nm[nm == "lo|mid"] <- "cut1"
  dimnames(f$var) <- list(nm, nm)
  expect_warning(
    sp <- ame_spicy(f),
    "cannot be matched to the coefficients by name",
    class = "spicy_fallback"
  )
  expect_identical(nrow(sp), 0L)
})


# ---- D2: a data-dependent basis in a design fit --------------------------

test_that("the AME of a svyolr with poly() is taken on the fitted basis", {
  skip_if_no_ame_survey()
  d <- ame_test_data()
  des <- survey::svydesign(id = ~id, weights = ~sw, data = d)
  f <- survey::svyolr(ord ~ x + grp + poly(z, 2), design = des)
  expect_ame_matches(ame_spicy(f), svyolr_oracle(f, ~ x + grp + poly(z, 2), d))
})

test_that("the AME of a gaussian svyglm with poly() is taken on the fitted basis", {
  skip_if_no_ame_survey()
  d <- ame_test_data()
  des <- survey::svydesign(id = ~id, weights = ~sw, data = d)
  rhs <- ~ x + grp + poly(z, 2)
  f <- survey::svyglm(y ~ x + grp + poly(z, 2), design = des)
  expect_ame_matches(ame_spicy(f), svyglm_oracle(f, rhs, d, identity))
  # The same on a replicate design.
  rd <- survey::as.svrepdesign(des, type = "JK1")
  f <- survey::svyglm(y ~ x + grp + poly(z, 2), design = rd)
  expect_ame_matches(ame_spicy(f), svyglm_oracle(f, rhs, d, identity))
})

test_that("the AME of a quasibinomial svyglm on a subpopulation keeps its basis", {
  skip_if_no_ame_survey()
  d <- ame_test_data()
  des <- survey::svydesign(id = ~id, weights = ~sw, data = d)
  f <- survey::svyglm(
    yb ~ x + grp + poly(z, 2),
    design = subset(des, x > -1),
    family = stats::quasibinomial()
  )
  ds <- d[d$x > -1, ]
  expect_ame_matches(
    ame_spicy(f),
    svyglm_oracle(f, ~ x + grp + poly(z, 2), ds, stats::plogis, w = ds$sw)
  )
})

test_that("the AME of a quasipoisson svyglm with scale() keeps the fitted centre", {
  skip_if_no_ame_survey()
  d <- ame_test_data()
  des <- survey::svydesign(id = ~id, weights = ~sw, data = d)
  f <- survey::svyglm(
    cnt ~ x + grp + scale(z),
    design = des,
    family = stats::quasipoisson()
  )
  # The AME row of z sits under its coefficient, `scale(z)`.
  or <- svyglm_oracle(f, ~ x + grp + scale(z), d, exp)
  or$key[or$key == "z|NA"] <- "scale(z)|NA"
  expect_ame_matches(ame_spicy(f), or)
})


# ---- D3: the offset of a svyglm --------------------------------------------

test_that("the AME of a svyglm with an offset includes the offset", {
  skip_if_no_ame_survey()
  d <- ame_test_data()
  des <- survey::svydesign(id = ~id, weights = ~sw, data = d)
  f <- survey::svyglm(
    cnt ~ x + grp + z + offset(log(expo)),
    design = des,
    family = stats::quasipoisson()
  )
  expect_ame_matches(
    ame_spicy(f),
    svyglm_oracle(f, ~ x + grp + z + offset(log(expo)), d, exp)
  )
})


# ---- The guard on a design fit -------------------------------------------

test_that("a design fit whose prediction does not reproduce the fit gets no AME", {
  skip_if_no_ame_survey()
  d <- ame_test_data()
  des <- survey::svydesign(id = ~id, weights = ~sw, data = d)
  # I(z - mean(z)) is centred again on the data it is evaluated on: a
  # predict() at z + h returns the prediction at z, and the AME of z is 0.
  f <- survey::svyglm(y ~ x + grp + I(z - mean(z)), design = des)
  expect_warning(
    sp <- ame_spicy(f),
    "does not give back its fitted values",
    class = "spicy_fallback"
  )
  expect_identical(nrow(sp), 0L)
  expect_warning(
    ame_spicy(f),
    "Build the column in the design with `update()`",
    fixed = TRUE
  )
})


# ---- rms -------------------------------------------------------------------

# rcs() found where model.frame() evaluates the formula, as library(rms)
# would make it.
rms_formula <- function(fo) {
  e <- new.env(parent = environment(fo))
  e$rcs <- rms::rcs
  environment(fo) <- e
  fo
}

test_that("the AME of an rms::Glm with rcs() is the AME of the same lrm", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("rms")
  skip_if_not_installed("numDeriv")
  d <- ame_test_data()
  fo <- rms_formula(yb ~ x + grp + rcs(z, 4))
  f <- rms::Glm(fo, family = stats::binomial(), data = d)
  L <- rms::lrm(fo, data = d)
  prep <- function(nd) {
    X <- stats::predict(f, nd, type = "x")
    colnames(X) <- names(stats::coef(f))[-1L]
    list(X = X, off = 0)
  }
  or <- ame_oracle(
    prep,
    function(b, D) stats::plogis(b[[1L]] + drop(D$X %*% b[colnames(D$X)])),
    stats::coef(f),
    stats::vcov(f),
    d,
    c("x", "grp", "z")
  )
  sp <- ame_spicy(f)
  expect_ame_matches(sp, or)
  expect_ame_matches(sp, ame_spicy(L))
})

test_that("an lrm or ols with a basis of stats gets no AME", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("rms")
  d <- ame_test_data()
  f <- rms::lrm(yb ~ x + grp + poly(z, 2), data = d)
  expect_warning(
    sp <- ame_spicy(f),
    "does not give back its fitted values",
    class = "spicy_fallback"
  )
  expect_identical(nrow(sp), 0L)
  f <- rms::ols(y ~ x + grp + scale(z), data = d)
  expect_warning(
    sp <- ame_spicy(f),
    "Use the transformations of rms",
    class = "spicy_fallback"
  )
  expect_identical(nrow(sp), 0L)
  # The coefficient rows stay.
  lo <- suppressWarnings(as.data.frame(table_regression(
    f,
    show_columns = c("b", "ame"),
    output = "long"
  )))
  expect_true(all(is.finite(lo$estimate[
    lo$estimate_type == "B" & lo$term == "x"
  ])))
})

test_that("the guard passes a fit it has nothing to compare with", {
  skip_if_not_installed("rms")
  d <- ame_test_data()
  f <- rms::ols(y ~ x + grp + scale(z), data = d)
  expect_type(.ame_prediction_check(f, d), "character")
  f$linear.predictors <- NULL
  expect_null(.ame_prediction_check(f, d))
  expect_null(.ame_prediction_check(stats::lm(y ~ x, d), d))
  # Tiny data: the subset falls back to one row in two, then to every row
  # but the first, and a single row leaves nothing to compare.
  for (n in 1:3) {
    dn <- d[seq_len(n), ]
    fit <- stats::glm(if (n == 1L) y ~ 1 else y ~ x, data = dn)
    expect_null(.ame_prediction_check(fit, dn))
  }
})

# ---- The guard with indicators and tied values ---------------------------

# A numeric 0/1 indicator holds its minimum or its maximum on every row,
# and an integer age has many ties: the check must still draw a subset on
# which a data-dependent basis moves.
ame_guard_data <- function(n = 300, seed = 20261120) {
  set.seed(seed)
  d <- data.frame(
    d01 = stats::rbinom(n, 1, 0.4),
    age = sample(20:70, n, TRUE),
    x = stats::rnorm(n),
    f3 = factor(sample(c("a", "b", "c"), n, TRUE)),
    sw = stats::runif(n, 0.5, 2)
  )
  d$id <- rep(seq_len(n / 5), each = 5)
  lin <- -0.5 + 0.6 * d$d01 + 0.8 * sin(d$age / 10) + 0.4 * d$x
  d$yb <- stats::rbinom(n, 1, stats::plogis(lin))
  d$y <- lin + stats::rnorm(n)
  d
}

test_that("an lrm with an indicator and ns() of a tied integer gets no AME", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("rms")
  d <- ame_guard_data()
  f <- rms::lrm(yb ~ d01 + splines::ns(age, 3), data = d)
  expect_warning(
    sp <- ame_spicy(f),
    "does not give back its fitted values",
    class = "spicy_fallback"
  )
  expect_identical(nrow(sp), 0L)
})

test_that("a svyglm with an indicator and scale() keeps its AME", {
  skip_if_no_ame_survey()
  d <- ame_guard_data()
  des <- survey::svydesign(id = ~id, weights = ~sw, data = d)
  f <- survey::svyglm(y ~ d01 + scale(x), design = des)
  or <- ame_oracle(
    ame_prep_rhs(~ d01 + scale(x), d),
    ame_pf_link(identity),
    stats::coef(f),
    stats::vcov(f),
    d,
    c("d01", "x"),
    d$sw
  )
  or$key[or$key == "x|NA"] <- "scale(x)|NA"
  expect_ame_matches(ame_spicy(f), or)
})

test_that("an lrm with an indicator and rcs() of a tied integer keeps its AME", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("rms")
  skip_if_not_installed("numDeriv")
  d <- ame_guard_data()
  f <- rms::lrm(rms_formula(yb ~ d01 + rcs(age, 4)), data = d)
  prep <- function(nd) {
    X <- stats::predict(f, nd, type = "x")
    colnames(X) <- names(stats::coef(f))[-1L]
    list(X = X, off = 0)
  }
  or <- ame_oracle(
    prep,
    function(b, D) stats::plogis(b[[1L]] + drop(D$X %*% b[colnames(D$X)])),
    stats::coef(f),
    stats::vcov(f),
    d,
    c("d01", "age")
  )
  expect_no_warning(sp <- ame_spicy(f))
  expect_ame_matches(sp, or)
})

test_that("a model of factors and indicators only keeps its AME", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("rms")
  d <- ame_guard_data()
  f <- rms::lrm(yb ~ d01 + f3, data = d)
  expect_no_warning(sp <- ame_spicy(f))
  expect_identical(nrow(sp), 3L)
})

test_that("a svyglm with the offset given as `offset =` keeps it in its AME", {
  skip_if_no_ame_survey()
  d <- ame_test_data()
  des <- survey::svydesign(id = ~id, weights = ~sw, data = d)
  f <- survey::svyglm(
    cnt ~ x + grp + z,
    design = des,
    family = stats::quasipoisson(),
    offset = log(expo)
  )
  prep <- ame_prep_rhs(~ x + grp + z, d)
  prep_off <- function(nd) {
    D <- prep(nd)
    D$off <- log(nd$expo)
    D
  }
  or <- ame_oracle(
    prep_off,
    ame_pf_link(exp),
    stats::coef(f),
    stats::vcov(f),
    d,
    c("x", "grp", "z"),
    d$sw
  )
  expect_ame_matches(ame_spicy(f), or)
})
