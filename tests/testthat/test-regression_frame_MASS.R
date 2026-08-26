# ---------------------------------------------------------------------------
# Phase 6d tests: as_regression_frame() methods for MASS::glm.nb / rlm.
# (polr is covered by test-regression_frame_ordinal.R from Phase 5b.)
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_glm_nb <- function() {
  skip_if_not_installed("MASS")
  data(quine, package = "MASS", envir = environment())
  MASS::glm.nb(Days ~ Age + Sex, data = quine)
}

.fit_rlm_basic <- function() {
  skip_if_not_installed("MASS")
  MASS::rlm(mpg ~ wt + cyl, data = mtcars)
}

.fit_rlm_bisquare <- function() {
  skip_if_not_installed("MASS")
  MASS::rlm(mpg ~ wt, data = mtcars, psi = MASS::psi.bisquare)
}

.fit_rlm_factor <- function() {
  skip_if_not_installed("MASS")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  MASS::rlm(mpg ~ wt + cyl_f, data = d)
}


# ---- 1. negbin: schema validity + delegation overlay ---------------------

test_that("as_regression_frame.negbin produces a schema-valid frame", {
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("negbin: info$class is 'negbin' (not 'glm')", {
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "negbin")
})

test_that("negbin: family normalised to 'negbin' (theta stripped)", {
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "negbin")
  expect_identical(fr$info$family$link, "log")
})

test_that("negbin: title_prefix names Negative-binomial", {
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Negative-binomial regression")
})

test_that("negbin: theta + se_theta surfaced in extras", {
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$extras$theta, fit$theta, tolerance = 1e-10)
  expect_equal(fr$info$extras$se_theta, fit$SE.theta, tolerance = 1e-10)
})

test_that("negbin: coef extraction matches stats::coef(fit)", {
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(legacy[nm]),
      tolerance = 1e-10
    )
  }
})


# ---- 2. rlm: schema validity + core fields -------------------------------

test_that("as_regression_frame.rlm produces a schema-valid frame", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("rlm: info$class is 'rlm'", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "rlm")
})

test_that("rlm: title_prefix = 'Robust linear regression (M-estimator)'", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Robust linear regression (M-estimator)"
  )
})

test_that("rlm: psi_function detected as 'Huber' for default", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$psi_function, "Huber")
})

test_that("rlm bisquare: psi_function detected as 'Bisquare'", {
  fit <- .fit_rlm_bisquare()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$psi_function, "Bisquare")
})

test_that("rlm: scale matches fit$s", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$extras$scale, as.numeric(fit$s), tolerance = 1e-10)
})


# ---- 3. rlm: Wald z asymptotic ------------------------------------------

test_that("rlm: Wald z (test_type='z', df=Inf, ci_method='wald')", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "z" | fr$coefs$is_ref))
  expect_true(all(is.infinite(fr$coefs$df) | fr$coefs$is_ref))
})

test_that("rlm: p-values derived from Wald z", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  non_ref <- fr$coefs[!fr$coefs$is_ref, ]
  expected_p <- 2 * stats::pnorm(-abs(non_ref$statistic))
  expect_equal(non_ref$p_value, expected_p, tolerance = 1e-12)
})


# ---- 4. rlm: coef extraction + supports ----------------------------------

test_that("rlm: coefs estimates match stats::coef(fit)", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(legacy[nm]),
      tolerance = 1e-10
    )
  }
})

test_that("rlm: supports flags are correct", {
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sp <- fr$info$supports
  expect_true(sp$ame)
  expect_false(sp$classical_r2)
  expect_false(sp$exponentiate)
})


# ---- 5. rlm: factor predictor reference row -----------------------------

test_that("rlm: factor predictor synthesises a reference row", {
  fit <- .fit_rlm_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  rows <- fr$coefs[fr$coefs$parent_var == "cyl_f", ]
  expect_identical(nrow(rows), 3L)
  expect_identical(sum(rows$is_ref), 1L)
})


# ---- 6. Oracle: parameters::model_parameters() --------------------------

test_that("negbin coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_glm_nb()
  fr <- as_regression_frame(fit, model_id = "M1")
  oracle <- parameters::model_parameters(fit, ci = 0.95, exponentiate = FALSE)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  expect_oracle_covered(length(oracle$Parameter))
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(spicy_row$estimate, oracle_row$Coefficient, tolerance = 1e-6)
    expect_equal(spicy_row$std_error, oracle_row$SE, tolerance = 1e-6)
  }
})

test_that("rlm coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_rlm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  oracle <- parameters::model_parameters(fit, ci = 0.95)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  expect_oracle_covered(length(oracle$Parameter))
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(spicy_row$estimate, oracle_row$Coefficient, tolerance = 1e-6)
    expect_equal(spicy_row$std_error, oracle_row$SE, tolerance = 1e-6)
  }
})


## ---- Phase 3 matrix (lot T2) ----------------------------------------------

# Phase 3 matrix: rd-vcov-classes:registry-negbin
test_that("glm.nb AME matches marginaleffects::avg_slopes (numeric + factor terms)", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("marginaleffects")
  set.seed(7)
  d <- data.frame(x1 = rnorm(200), f = factor(sample(c("a", "b"), 200, TRUE)))
  d$y <- stats::rnbinom(200, mu = exp(0.3 + 0.3 * d$x1), size = 1.5)
  fit <- suppressWarnings(MASS::glm.nb(y ~ x1 + f, data = d))
  fr <- suppressWarnings(as_regression_frame(fit, show_columns = c("b", "ame")))
  expect_true(isTRUE(fr$info$supports$ame))
  a <- fr$coefs[
    fr$coefs$estimate_type == "ame" & !(fr$coefs$is_ref %in% TRUE),
    ,
    drop = FALSE
  ]
  orc <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(fit, df = Inf)
  ))
  # Coef-style term id for the oracle rows ("<var><level>" for factor
  # contrasts, bare name for numerics).
  okey <- ifelse(
    !is.na(orc$contrast) & grepl(" - ", orc$contrast, fixed = TRUE),
    paste0(orc$term, sub(" - .*$", "", orc$contrast)),
    orc$term
  )
  expect_identical(nrow(a), nrow(orc))
  expect_setequal(a$term, okey)
  idx <- match(a$term, okey)
  expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-8)
  expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-8)
})


# ============================================================================
# Phase 3 matrix – rd-core:vcov-matrix-lm-glm-all (glm.nb half)
# ============================================================================

test_that("glm.nb supports classical / HC* / CR* with exact oracles", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("sandwich")
  set.seed(2610)
  n <- 80
  d <- data.frame(x = rnorm(n), g = factor(sample(letters[1:8], n, TRUE)))
  d$y <- MASS::rnegbin(n, mu = exp(0.6 + 0.3 * d$x), theta = 2)
  fit <- MASS::glm.nb(y ~ x, data = d)
  nm <- names(coef(fit))
  se_of <- function(out) {
    s <- as_structured(out)
    s$body$SE[match(nm, s$body$Variable)]
  }
  # classical = summary() model-based SEs
  expect_equal(
    se_of(table_regression(fit)),
    unname(summary(fit)$coefficients[, 2]),
    tolerance = 1e-8
  )
  # HC*: sandwich::vcovHC oracle, per variant
  for (hc in paste0("HC", 0:3)) {
    expect_equal(
      se_of(table_regression(fit, vcov = hc)),
      unname(sqrt(diag(sandwich::vcovHC(fit, type = hc)))),
      tolerance = 1e-8,
      info = hc
    )
  }
  # CR2: clubSandwich oracle
  skip_if_not_installed("clubSandwich")
  expect_equal(
    se_of(table_regression(fit, vcov = "CR2", cluster = d$g)),
    unname(sqrt(diag(as.matrix(
      clubSandwich::vcovCR(fit, cluster = d$g, type = "CR2")
    )))),
    tolerance = 1e-8
  )
})

test_that("glm.nb supports bootstrap and jackknife resampling", {
  # rd-core:vcov-matrix-lm-glm-all – the resamplers accept glm.nb and
  # return finite SEs. (That each bootstrap replicate holds theta at
  # the full-sample estimate is the lot-T2 instrumentation test.)
  skip_if_not_installed("MASS")
  set.seed(2611)
  n <- 80
  d <- data.frame(x = rnorm(n))
  d$y <- MASS::rnegbin(n, mu = exp(0.6 + 0.3 * d$x), theta = 2)
  fit <- MASS::glm.nb(y ~ x, data = d)
  nm <- names(coef(fit))
  set.seed(77)
  o_boot <- table_regression(fit, vcov = "bootstrap", boot_n = 25L)
  sb <- as_structured(o_boot)
  expect_true(all(is.finite(sb$body$SE[match(nm, sb$body$Variable)])))
  expect_match(attr(o_boot, "note"), "bootstrap", ignore.case = TRUE)
  o_jack <- table_regression(fit, vcov = "jackknife")
  sj <- as_structured(o_jack)
  expect_true(all(is.finite(sj$body$SE[match(nm, sj$body$Variable)])))
  expect_match(attr(o_jack, "note"), "jackknife", ignore.case = TRUE)
})
