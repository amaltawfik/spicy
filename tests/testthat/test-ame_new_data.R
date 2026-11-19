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
