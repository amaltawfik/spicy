# ---------------------------------------------------------------------------
# The three published numbers a survey-design regression got wrong in
# 0.12.0, and the accessors that keep them right.
#
#   * the average marginal effect averaged over the SAMPLE, not the
#     population the design describes;
#   * the "AIC" row showing the effective number of design parameters;
#   * the weighted sample size of a replicate design reading the FIRST
#     REPLICATE's weights.
#
# Every witness here asserts on the RENDERED table, not on the helper:
# each of the three had a second route into the output (a mixed table,
# an explicit `show_fit_stats`) that a frame-level assertion misses.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.wn_apistrat_design <- function() {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = apistrat,
    fpc = ~fpc
  )
}

# apistrat, NOT apiclus1: every apiclus1 weight is 33.847, so a weighted
# and an unweighted average coincide there to the bit and no witness
# built on it can tell the two apart. apistrat carries 15.10 / 20.36 /
# 44.21.
.wn_fit_awards <- function() {
  d <- .wn_apistrat_design()
  suppressWarnings(survey::svyglm(
    awards ~ ell + meals + stype,
    design = d,
    family = stats::quasibinomial()
  ))
}

.wn_apiclus1_designs <- function() {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dclus1 <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1,
    fpc = ~fpc
  )
  list(
    linearized = dclus1,
    replicate = survey::as.svrepdesign(dclus1, type = "JK1"),
    data = apiclus1
  )
}


# ---- 1. The AME is averaged over the population ---------------------------

test_that("svyglm AME uses the sampling weights of the analytic sample", {
  skip_if_not_installed("marginaleffects")
  fit <- .wn_fit_awards()
  w <- .spicy_ame_fit_wts(fit)
  expect_length(w, nrow(stats::model.frame(fit)))
  expect_equal(sum(w), 6193.9999580383301, tolerance = 1e-12)
  # The weights are the DESIGN's, not the rescaled prior weights survey
  # hands the IWLS (which are all 1 on a constant-weight design).
  expect_equal(
    sort(unique(round(w, 2))),
    c(15.10, 20.36, 44.21),
    tolerance = 1e-9
  )
})

test_that("svyglm AME matches the Horvitz-Thompson oracle, pinned", {
  skip_if_not_installed("marginaleffects")
  fit <- .wn_fit_awards()
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  ame <- fr$coefs[fr$coefs$estimate_type == "ame" & !fr$coefs$is_ref, ]
  got <- stats::setNames(ame$estimate, ame$term)

  # Pinned to 17 digits at tolerance 1e-12: the design-weighted average
  # of the unit-level effects.
  expect_equal(
    got[["ell"]],
    0.00004379236074383509,
    tolerance = 1e-12
  )
  expect_equal(
    got[["meals"]],
    -0.00116102718012526195,
    tolerance = 1e-12
  )
  expect_equal(
    got[["stypeH"]],
    -0.43443483416860212420,
    tolerance = 1e-12
  )
  expect_equal(
    got[["stypeM"]],
    -0.25661639882911313482,
    tolerance = 1e-12
  )

  # And they are NOT the sample average -- the value 0.12.0 published.
  expect_false(isTRUE(all.equal(
    got[["ell"]],
    4.5301204761402507e-05,
    tolerance = 1e-6
  )))
  expect_false(isTRUE(all.equal(
    got[["meals"]],
    -1.2010297943971932e-03,
    tolerance = 1e-6
  )))
})

test_that("the design-weighted AME reproduces an independent HT oracle", {
  skip_if_not_installed("marginaleffects")
  data(api, package = "survey", envir = environment())
  fit <- .wn_fit_awards()
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  ame <- fr$coefs[fr$coefs$estimate_type == "ame" & !fr$coefs$is_ref, ]
  got <- stats::setNames(ame$estimate, ame$term)

  # Independent oracle: a weighted glm (survey fits the same IWLS), a
  # centred numeric derivative, and a weighted mean of the unit-level
  # effects. Finite differences, so the agreement is that of the
  # derivative, not of the arithmetic.
  w <- as.numeric(apistrat$pw)
  ref <- suppressWarnings(stats::glm(
    awards ~ ell + meals + stype,
    data = apistrat,
    weights = w,
    family = stats::quasibinomial()
  ))
  eps <- 1e-6
  ht_slope <- function(v) {
    lo <- apistrat
    hi <- apistrat
    lo[[v]] <- lo[[v]] - eps / 2
    hi[[v]] <- hi[[v]] + eps / 2
    de <- as.numeric(
      stats::predict(ref, newdata = hi, type = "response") -
        stats::predict(ref, newdata = lo, type = "response")
    ) /
      eps
    sum(de * w) / sum(w)
  }
  ht_contrast <- function(lvl) {
    a <- apistrat
    b <- apistrat
    a$stype <- factor(lvl, levels = levels(apistrat$stype))
    b$stype <- factor("E", levels = levels(apistrat$stype))
    de <- as.numeric(
      stats::predict(ref, newdata = a, type = "response") -
        stats::predict(ref, newdata = b, type = "response")
    )
    sum(de * w) / sum(w)
  }
  expect_equal(got[["ell"]], ht_slope("ell"), tolerance = 1e-6)
  expect_equal(got[["meals"]], ht_slope("meals"), tolerance = 1e-6)
  expect_equal(got[["stypeH"]], ht_contrast("H"), tolerance = 1e-9)
  expect_equal(got[["stypeM"]], ht_contrast("M"), tolerance = 1e-9)
})

test_that("the AME standard error reproduces the replicate variance", {
  skip_if_not_installed("marginaleffects")
  data(api, package = "survey", envir = environment())
  # The design-based variance oracle for a non-linear statistic is
  # survey's own: replicate the WHOLE estimator -- refit plus weighted
  # average -- on every replicate weight. Both sides use the same JKn
  # design, because as.svrepdesign() drops the finite population
  # correction and a converted design is not an oracle for a design
  # with one.
  dstrat <- .wn_apistrat_design()
  rstrat <- survey::as.svrepdesign(dstrat, type = "JKn", compress = FALSE)
  fit <- suppressWarnings(survey::svyglm(
    awards ~ ell + meals + stype,
    design = rstrat,
    family = stats::quasibinomial()
  ))
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  ame <- fr$coefs[fr$coefs$estimate_type == "ame" & !fr$coefs$is_ref, ]
  delta_se <- stats::setNames(ame$std_error, ame$term)

  eps <- 1e-6
  theta <- function(ww, dd) {
    f <- suppressWarnings(stats::glm(
      awards ~ ell + meals + stype,
      data = dd,
      weights = ww,
      family = stats::quasibinomial()
    ))
    out <- vapply(
      c("ell", "meals"),
      function(v) {
        lo <- dd
        hi <- dd
        lo[[v]] <- lo[[v]] - eps / 2
        hi[[v]] <- hi[[v]] + eps / 2
        de <- as.numeric(
          stats::predict(f, newdata = hi, type = "response") -
            stats::predict(f, newdata = lo, type = "response")
        ) /
          eps
        sum(de * ww) / sum(ww)
      },
      numeric(1)
    )
    out
  }
  repl <- suppressWarnings(survey::withReplicates(rstrat, theta))
  repl_se <- as.numeric(survey::SE(repl))
  # Point estimates agree to the derivative's own precision; the two
  # variances agree to under 1%, which is what makes the delta method on
  # the design vcov the right answer here.
  delta_est <- stats::setNames(ame$estimate, ame$term)[c("ell", "meals")]
  expect_equal(
    unname(stats::coef(repl)),
    unname(delta_est),
    tolerance = 1e-6
  )
  ratio <- repl_se / unname(delta_se[c("ell", "meals")])
  expect_true(all(abs(ratio - 1) < 0.01))
})

test_that("the AME row of a rendered svyglm table carries the weighted value", {
  skip_if_not_installed("marginaleffects")
  fit <- .wn_fit_awards()
  out <- paste(
    utils::capture.output(print(table_regression(
      fit,
      show_columns = c("b", "ame"),
      digits = 5
    ))),
    collapse = "\n"
  )
  # -0.43443 (design-weighted) and not -0.43423 (sample average).
  expect_match(out, "-0.43443", fixed = TRUE)
  expect_false(grepl("-0.43423", out, fixed = TRUE))
})


# ---- 2. The AIC row is the AIC ---------------------------------------------

test_that("svyglm fit_stats$aic is the criterion, as a scalar", {
  d <- .wn_apiclus1_designs()
  fit <- survey::svyglm(api00 ~ ell + meals, design = d$linearized)
  fr <- as_regression_frame(fit)
  expect_length(fr$info$fit_stats$aic, 1L)
  expect_equal(fr$info$fit_stats$aic, 2002.2129452231254, tolerance = 1e-12)
  # survey's own three-element return, for the record.
  raw <- suppressWarnings(stats::AIC(fit))
  expect_length(raw, 3L)
  expect_equal(unname(raw[["eff.p"]]), 4.57020356524324, tolerance = 1e-12)
  # BIC.svyglm needs a `maximal =` model and has no default: NA, not a
  # number.
  expect_true(is.na(fr$info$fit_stats$bic))
})

test_that("a mixed svyglm + glm table prints two comparable AIC values", {
  d <- .wn_apiclus1_designs()
  fit <- survey::svyglm(api00 ~ ell + meals, design = d$linearized)
  plain <- stats::glm(api00 ~ ell + meals, data = d$data)
  out <- paste(
    utils::capture.output(suppressWarnings(print(table_regression(list(
      fit,
      plain
    ))))),
    collapse = "\n"
  )
  expect_match(out, "2002.2", fixed = TRUE)
  expect_match(out, "2001.1", fixed = TRUE)
  # The effective number of design parameters is not an AIC.
  expect_false(grepl("AIC             │   4.6", out, fixed = TRUE))
})

test_that("an explicit aic token on a replicate design prints the criterion", {
  d <- .wn_apiclus1_designs()
  fit <- survey::svyglm(api00 ~ ell + meals, design = d$replicate)
  out <- paste(
    utils::capture.output(print(table_regression(
      fit,
      show_fit_stats = c("nobs", "aic")
    ))),
    collapse = "\n"
  )
  expect_match(out, "2003.4", fixed = TRUE)
})


# ---- 3. The weighted n is the sum of the SAMPLING weights ------------------

test_that("weighted_nobs of a replicate design renders the sampling total", {
  d <- .wn_apiclus1_designs()
  fit <- survey::svyglm(api00 ~ ell + meals, design = d$replicate)
  out <- paste(
    utils::capture.output(print(table_regression(
      fit,
      show_fit_stats = c("nobs", "weighted_nobs")
    ))),
    collapse = "\n"
  )
  expect_match(out, "6194", fixed = TRUE)
  # 2745 is the first replicate's total, with one cluster at weight zero.
  expect_false(grepl("2745", out, fixed = TRUE))
})

test_that("the linearised and replicate designs agree on the weighted n", {
  d <- .wn_apiclus1_designs()
  lin <- survey::svyglm(api00 ~ ell + meals, design = d$linearized)
  rep <- survey::svyglm(api00 ~ ell + meals, design = d$replicate)
  wn <- function(f) as_regression_frame(f)$info$fit_stats$weighted_nobs
  expect_equal(wn(lin), 6194.0003242492676, tolerance = 1e-12)
  expect_equal(wn(rep), 6194.0003242492676, tolerance = 1e-12)
  expect_equal(as_regression_frame(lin)$info$extras$weighted_n, wn(lin))
})


# ---- 4. The analytic sample, when rows are missing -------------------------

test_that(".design_analytic never drops the missing rows twice", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  d <- apistrat
  d$ell[1:20] <- NA
  des <- survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = d,
    fpc = ~fpc
  )
  fit <- survey::svyglm(api00 ~ ell + stype, design = des)
  # svyglm attaches the design ALREADY reduced, and still reports a
  # 20-element na.action: subsetting it again would leave 160 rows.
  expect_equal(nrow(fit$survey.design), 180L)
  expect_length(stats::na.action(fit), 20L)
  got <- .design_analytic(fit, 180L)
  expect_equal(nrow(got), 180L)
  expect_equal(
    .design_weighted_n(fit, 180L),
    5487.2699661254883,
    tolerance = 1e-12
  )
  expect_length(.design_analytic_weights(fit, 180L), 180L)
})

test_that("a calibrated design aligns on its non-zero weights", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  d <- apistrat
  d$ell[1:20] <- NA
  des <- survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = d,
    fpc = ~fpc
  )
  cal <- survey::calibrate(
    des,
    ~stype,
    pop = c(`(Intercept)` = 6194, stypeH = 755, stypeM = 1018)
  )
  fit <- survey::svyglm(api00 ~ ell, design = cal)
  # Subsetting a calibrated design zeroes weights instead of dropping
  # rows, so the attached object still has 200 of them for 180
  # observations.
  expect_equal(nrow(fit$survey.design), 200L)
  expect_equal(nrow(stats::model.frame(fit)), 180L)
  expect_length(.design_analytic_weights(fit, 180L), 180L)
  expect_equal(
    .design_weighted_n(fit, 180L),
    5487.2699999999995,
    tolerance = 1e-12
  )
})

test_that("an unalignable design yields NA, never a plausible total", {
  d <- .wn_apiclus1_designs()
  fit <- survey::svyglm(api00 ~ ell + meals, design = d$linearized)
  # A row count the design cannot possibly describe.
  expect_true(is.na(.design_weighted_n(fit, 5L)))
  expect_null(.design_analytic(fit, 5L))
  # A detached design is absent, not zero.
  detached <- fit
  detached$survey.design <- NULL
  expect_null(.design_analytic(detached, 183L))
  expect_true(is.na(.design_weighted_n(detached, 183L)))
})
