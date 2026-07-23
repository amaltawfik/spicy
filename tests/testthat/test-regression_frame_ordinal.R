# ---------------------------------------------------------------------------
# Phase 5b tests: as_regression_frame() methods for ordinal regression.
#
# Coverage:
#   * polr (MASS) -- predictor coefs only (no intercept; thresholds in
#     extras), Wald z derived from t-value (polr does not compute p).
#   * clm (ordinal) -- predictor coefs only (no intercept; thresholds
#     in extras), Wald z + p from summary natively.
#   * Factor predictor -- reference-row synthesis per factor (NOT per
#     cumulative threshold; PO assumption).
#   * Schema validity in all paths.
#   * Oracle cross-validation against parameters::model_parameters().
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_polr_basic <- function() {
  skip_if_not_installed("MASS")
  MASS::polr(
    Sat ~ Infl + Type + Cont,
    weights = Freq,
    data = MASS::housing,
    Hess = TRUE
  )
}

.fit_clm_basic <- function() {
  skip_if_not_installed("ordinal")
  ordinal::clm(rating ~ temp + contact, data = ordinal::wine)
}


# ---- 1. polr: schema validity + core fields ------------------------------

test_that("as_regression_frame.polr produces a schema-valid frame", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("polr: required attributes are attached", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("polr: info$class is 'polr'", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "polr")
})

test_that("polr (logit): info$family is cumulative/logit", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "cumulative")
  expect_identical(fr$info$family$link, "logit")
})

test_that("polr: info$dv is the response variable", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "Sat")
})

test_that("polr: response levels surfaced in extras", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$response_levels, c("Low", "Medium", "High"))
})


# ---- 2. polr: no intercept in coefs --------------------------------------

test_that("polr: coefs table has no (Intercept) row", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false("(Intercept)" %in% fr$coefs$term)
})


# ---- 3. polr: coef extraction --------------------------------------------

test_that("polr: coefs estimates match stats::coef(fit)", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(legacy[nm]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})

test_that("polr: SE matches sqrt(diag(vcov[pred, pred]))", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  V <- as.matrix(stats::vcov(fit))
  preds <- names(stats::coef(fit))
  expected_se <- sqrt(diag(V[preds, preds]))
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in preds) {
    expect_equal(
      b_rows$std_error[b_rows$term == nm],
      unname(expected_se[nm]),
      tolerance = 1e-10
    )
  }
})


# ---- 4. polr: thresholds in extras ---------------------------------------

test_that("polr: thresholds tibble has (k - 1) rows with finite p-values", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  th <- fr$info$extras$thresholds
  expect_s3_class(th, "data.frame")
  expect_identical(nrow(th), length(fit$lev) - 1L)
  expect_setequal(
    colnames(th),
    c("term", "estimate", "std_error", "statistic", "p_value")
  )
  expect_true(all(is.finite(th$p_value)))
})

test_that("polr: threshold estimates match fit$zeta", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  th <- fr$info$extras$thresholds
  expect_equal(th$estimate, unname(fit$zeta), tolerance = 1e-10)
})


# ---- 5. polr: factor predictor reference rows ----------------------------

test_that("polr: factor predictor synthesises one ref row (PO -- shared)", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  # 3 factor predictors in the fixture -> 3 ref rows.
  expect_identical(sum(fr$coefs$is_ref), 3L)
  # Each factor's rows = (k-1) non-ref + 1 ref
  type_rows <- fr$coefs[fr$coefs$parent_var == "Type", ]
  expect_identical(nrow(type_rows), 4L)
  expect_identical(sum(type_rows$is_ref), 1L)
})


# ---- 6. polr: inference + supports ---------------------------------------

test_that("polr: Wald z asymptotic (test_type='z', df=Inf)", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "z" | fr$coefs$is_ref))
  expect_true(all(is.infinite(fr$coefs$df) | fr$coefs$is_ref))
})

test_that("polr: supports$exponentiate = TRUE (odds ratios)", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})


# ---- 7. polr: title ------------------------------------------------------

test_that("polr (logit): title_prefix names the cumulative logit family", {
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "Cumulative logit", fixed = TRUE)
  expect_match(fr$info$extras$title_prefix, "proportional odds", fixed = TRUE)
})


# ---- 8. clm: schema validity + core fields -------------------------------

test_that("as_regression_frame.clm produces a schema-valid frame", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("clm: info$class is 'clm'", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "clm")
})

test_that("clm: info$family is cumulative/logit", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "cumulative")
  expect_identical(fr$info$family$link, "logit")
})


# ---- 9. clm: coef extraction matches summary natively --------------------

test_that("clm: coefs estimates match fit$beta", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(fit$beta)) {
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(fit$beta[nm]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})

test_that("clm: p-values match summary(fit)$coefficients byte-for-byte", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$coefficients
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(fit$beta)) {
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(sm[nm, "Pr(>|z|)"]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})


# ---- 10. clm: thresholds in extras ---------------------------------------

test_that("clm: thresholds tibble has (k - 1) rows with finite p-values", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  th <- fr$info$extras$thresholds
  expect_s3_class(th, "data.frame")
  expect_identical(nrow(th), length(fit$y.levels) - 1L)
  expect_true(all(is.finite(th$p_value)))
})

test_that("clm: threshold estimates match fit$alpha", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  th <- fr$info$extras$thresholds
  expect_equal(th$estimate, unname(fit$alpha), tolerance = 1e-10)
})


# ---- 11. clm: factor predictor reference rows ----------------------------

test_that("clm: factor predictor synthesises one ref row (PO)", {
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(sum(fr$coefs$is_ref), 2L) # temp + contact each have ref
})


# ---- 12. Oracle: parameters::model_parameters() --------------------------

test_that("polr coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_polr_basic()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(fit, ci = 0.95, exponentiate = FALSE)

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in b_rows$term) {
    oracle_row <- oracle[oracle$Parameter == nm, ]
    if (nrow(oracle_row) == 0L) {
      next
    }
    spicy_row <- b_rows[b_rows$term == nm, ]
    expect_equal(
      spicy_row$estimate,
      oracle_row$Coefficient,
      tolerance = 1e-6,
      info = paste("oracle B mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$std_error,
      oracle_row$SE,
      tolerance = 1e-6,
      info = paste("oracle SE mismatch on term:", nm)
    )
  }
})

test_that("clm coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_clm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(fit, ci = 0.95, exponentiate = FALSE)

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in b_rows$term) {
    oracle_row <- oracle[oracle$Parameter == nm, ]
    if (nrow(oracle_row) == 0L) {
      next
    }
    spicy_row <- b_rows[b_rows$term == nm, ]
    expect_equal(
      spicy_row$estimate,
      oracle_row$Coefficient,
      tolerance = 1e-6,
      info = paste("oracle B mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$std_error,
      oracle_row$SE,
      tolerance = 1e-6,
      info = paste("oracle SE mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$p_value,
      oracle_row$p,
      tolerance = 1e-6,
      info = paste("oracle p mismatch on term:", nm)
    )
  }
})
