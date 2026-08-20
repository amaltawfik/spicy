# ---------------------------------------------------------------------------
# Phase 4b tests: as_regression_frame() methods for nlme fits.
#
# Coverage:
#   * lme (linear mixed-effects) -- fixed effects extraction, per-coef
#     Wald-t DF from summary(fit)$tTable, VarCorr parsing, ICC.
#   * gls (generalised least squares) -- Wald-t with df = nobs - p,
#     correlation structure label in vcov_label.
#   * Factor predictor -- reference-row synthesis.
#   * Schema validity in all paths.
#   * Oracle cross-validation against parameters::model_parameters()
#     (skipped if not installed).
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_lme_basic <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ 1 | Subject
  )
}

.fit_gls_corcs <- function() {
  skip_if_not_installed("nlme")
  nlme::gls(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    correlation = nlme::corCompSymm(form = ~ 1 | Subject)
  )
}

.fit_gls_plain <- function() {
  skip_if_not_installed("nlme")
  nlme::gls(distance ~ age + Sex, data = nlme::Orthodont)
}


# ---- 1. lme: schema validity + core fields -------------------------------

test_that("as_regression_frame.lme produces a schema-valid frame", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("lme: required attributes are attached", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("lme: info$class is 'lme'", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "lme")
})

test_that("lme: info$family is gaussian/identity (hardcoded; no family slot)", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "gaussian")
  expect_identical(fr$info$family$link, "identity")
})

test_that("lme: info$dv reads the response variable name", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "distance")
})

test_that("lme: info$n_groups reports primary grouping factor count", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$n_groups, c(Subject = 27L))
})


# ---- 2. lme: coef extraction ---------------------------------------------

test_that("lme: coefs estimates match nlme::fixef()", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- nlme::fixef(fit)
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

test_that("lme: coefs SE matches sqrt(diag(vcov))", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  V <- as.matrix(stats::vcov(fit))
  expected_se <- sqrt(diag(V))
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(expected_se)) {
    expect_equal(
      b_rows$std_error[b_rows$term == nm],
      unname(expected_se[nm]),
      tolerance = 1e-10
    )
  }
})

test_that("lme: per-coef df matches summary(fit)$tTable[, 'DF']", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  tT <- summary(fit)$tTable
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(tT)) {
    expect_equal(
      b_rows$df[b_rows$term == nm],
      unname(tT[nm, "DF"]),
      info = paste("term:", nm)
    )
  }
})

test_that("lme: p-values match summary(fit)$tTable", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  tT <- summary(fit)$tTable
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(tT)) {
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(tT[nm, "p-value"]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})


# ---- 3. lme: inference -- Wald-t with finite per-coef DF -----------------

test_that("lme: ci_method = 'wald'; test_type = 't'; df finite", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "t"))
  expect_true(all(is.finite(b_rows$df)))
})


# ---- 4. lme: random effects + ICC ----------------------------------------

test_that("lme: random_effects has one row per (group,term) + residual", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  expect_identical(nrow(vc), 2L)
  expect_setequal(vc$group, c("Subject", "Residual"))
})

test_that("lme: ICC is in (0,1) for a single random-intercept fit", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  icc <- fr$info$random_effects$icc
  expect_true(is.finite(icc))
  expect_true(icc > 0 & icc < 1)
})

test_that("lme: ICC matches var_random / (var_random + var_resid)", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  var_r <- vc$variance[vc$group == "Subject"]
  var_e <- vc$variance[vc$group == "Residual"]
  expect_equal(
    fr$info$random_effects$icc,
    var_r / (var_r + var_e),
    tolerance = 1e-10
  )
})


# ---- 5. lme: fit statistics ----------------------------------------------

test_that("lme: fit_stats$r_squared / adj_r_squared are NA", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.na(fr$info$fit_stats$r_squared))
  expect_true(is.na(fr$info$fit_stats$adj_r_squared))
})

test_that("lme: AIC/BIC/logLik/nobs match stats:: helpers", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$aic, stats::AIC(fit), tolerance = 1e-10)
  expect_equal(fr$info$fit_stats$bic, stats::BIC(fit), tolerance = 1e-10)
  expect_equal(
    fr$info$fit_stats$log_lik,
    as.numeric(stats::logLik(fit)),
    tolerance = 1e-10
  )
  expect_identical(fr$info$fit_stats$nobs, as.integer(stats::nobs(fit)))
})

test_that("lme: fit_stats$sigma matches stats::sigma()", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$sigma, stats::sigma(fit), tolerance = 1e-10)
})


# ---- 6. lme: supports + title --------------------------------------------

test_that("lme: supports flags are correct", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sp <- fr$info$supports
  expect_true(sp$ame)
  expect_false(sp$partial_effect_size)
  expect_false(sp$classical_r2)
  expect_true(sp$nested_lrt)
  expect_false(sp$exponentiate)
  expect_true(sp$standardise_refit)
})

test_that("lme: title_prefix = 'Linear mixed-effects regression (nlme)'", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Linear mixed-effects regression (nlme)"
  )
})


# ---- 7. lme: factor predictor reference rows -----------------------------

test_that("lme: factor predictor synthesises a reference row", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  sex_rows <- fr$coefs[fr$coefs$parent_var == "Sex", ]
  expect_identical(nrow(sex_rows), 2L)
  expect_identical(sum(sex_rows$is_ref), 1L)
  expect_true(all(is.na(sex_rows$estimate[sex_rows$is_ref])))
})


# ---- 8. gls: schema validity + core fields -------------------------------

test_that("as_regression_frame.gls produces a schema-valid frame", {
  fit <- .fit_gls_corcs()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("gls: info$class is 'gls'", {
  fit <- .fit_gls_corcs()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "gls")
})

test_that("gls: info$n_groups is NULL (no random effects)", {
  fit <- .fit_gls_corcs()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_null(fr$info$n_groups)
})

test_that("gls: random_effects$icc is NA (no random effects)", {
  fit <- .fit_gls_corcs()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.na(fr$info$random_effects$icc))
})


# ---- 9. gls: coef extraction + DF derivation -----------------------------

test_that("gls: coefs estimates match stats::coef()", {
  fit <- .fit_gls_corcs()
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

test_that("gls: df = nobs(fit) - length(coef(fit)) on all non-ref rows", {
  fit <- .fit_gls_corcs()
  fr <- as_regression_frame(fit, model_id = "M1")
  expected_df <- stats::nobs(fit) - length(stats::coef(fit))
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$df == expected_df))
})

test_that("gls: p-values match summary(fit)$tTable", {
  fit <- .fit_gls_corcs()
  fr <- as_regression_frame(fit, model_id = "M1")
  tT <- summary(fit)$tTable
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(tT)) {
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(tT[nm, "p-value"]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})


# ---- 10. gls: correlation structure label --------------------------------

test_that("gls with corCompSymm: vcov_label names the structure", {
  fit <- .fit_gls_corcs()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$vcov_label, "corCompSymm", fixed = TRUE)
  expect_identical(fr$info$extras$correlation_structure, "corCompSymm")
})

test_that("gls without correlation structure: vcov_label is plain Wald", {
  fit <- .fit_gls_plain()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$vcov_label, "Wald (model-based)")
  expect_null(fr$info$extras$correlation_structure)
})


# ---- 11. gls: supports + title -------------------------------------------

test_that("gls: title_prefix = 'Generalised least squares (nlme)'", {
  fit <- .fit_gls_corcs()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Generalised least squares (nlme)"
  )
})

test_that("gls: factor predictor synthesises a reference row", {
  fit <- .fit_gls_corcs()
  fr <- as_regression_frame(fit, model_id = "M1")
  sex_rows <- fr$coefs[fr$coefs$parent_var == "Sex", ]
  expect_identical(nrow(sex_rows), 2L)
  expect_identical(sum(sex_rows$is_ref), 1L)
})


# ---- 12. Oracle: parameters::model_parameters() --------------------------

test_that("lme coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci = 0.95,
    ci_method = "wald",
    effects = "fixed"
  )

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
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

test_that("gls coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_gls_corcs()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci = 0.95,
    ci_method = "wald"
  )

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
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


## ---- Phase 3 matrix (lot T2) ----------------------------------------------

# Phase 3 matrix: rd-vcov-classes:registry-gls
test_that("gls AME columns are populated (registry: AME yes)", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("marginaleffects")
  # The registry and man/table_regression_models.Rd promise AME for
  # nlme::gls (.gls_info() advertises supports$ame = TRUE):
  # as_regression_frame.gls() attaches the AME rows the way lme does.
  set.seed(7)
  d <- data.frame(x1 = rnorm(150), x2 = rnorm(150))
  d$y <- 1 + 0.5 * d$x1 - 0.2 * d$x2 + rnorm(150)
  fit <- nlme::gls(y ~ x1 + x2, data = d)
  fr <- as_regression_frame(fit, show_columns = c("b", "ame"))
  expect_true(isTRUE(fr$info$supports$ame))
  a <- fr$coefs[fr$coefs$estimate_type == "ame", , drop = FALSE]
  orc <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(fit, df = Inf)
  ))
  expect_identical(nrow(a), nrow(orc))
  idx <- match(a$term, orc$term)
  expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-8)
  expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-8)
})


# ---- 13. lme: boundary (singular) fits -----------------------------------

# nlme has no isSingular() of its own. The convention is
# performance::check_singularity()'s -- a random-effect variance below
# 1e-5 -- read off every nesting level's covariance block through
# det(), so that a boundary correlation counts and a nested fit still
# gets an answer. Verdicts are triangulated against that oracle wherever
# it can answer.

.fit_lme_singular <- function() {
  skip_if_not_installed("nlme")
  d <- mtcars
  d$gearf <- factor(d$gear)
  # Three gear groups, fully explained by weight and horsepower: the
  # random intercept collapses onto 0.
  nlme::lme(mpg ~ wt + hp, data = d, random = ~ 1 | gearf)
}

.fit_lme_nested <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(pixel ~ day, data = nlme::Pixel, random = ~ 1 | Dog / Side)
}

test_that("lme singular fit sets extras$has_singular (and a healthy one does not)", {
  fit <- .fit_lme_singular()
  expect_true(spicy:::.lme_is_singular(fit))
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$extras$has_singular)
  expect_identical(fr$info$extras$singular_terms, character(0))
  expect_invisible(spicy:::validate_regression_frame(fr))

  clean <- .fit_lme_basic()
  expect_false(spicy:::.lme_is_singular(clean))
  expect_false(
    as_regression_frame(clean, model_id = "M1")$info$extras$has_singular
  )
})

test_that("lme singularity matches performance::check_singularity (oracle)", {
  skip_if_not_installed("performance")
  fits <- list(
    singular = .fit_lme_singular(),
    basic = .fit_lme_basic(),
    slope = nlme::lme(
      distance ~ age,
      data = nlme::Orthodont,
      random = ~ age | Subject
    )
  )
  for (nm in names(fits)) {
    expect_identical(
      spicy:::.lme_is_singular(fits[[nm]]),
      isTRUE(performance::check_singularity(fits[[nm]])),
      info = paste("oracle mismatch on fixture:", nm)
    )
  }
})

test_that("lme singularity answers for nested fits, where the oracle cannot", {
  skip_if_not_installed("performance")
  fit <- .fit_lme_nested()
  # nlme::getVarCov() -- performance's route -- refuses more than one
  # level of nesting; the reStruct walk covers both levels.
  expect_error(nlme::getVarCov(fit))
  expect_length(as.matrix(fit$modelStruct$reStruct), 2L)
  expect_false(spicy:::.lme_is_singular(fit))
  expect_false(
    as_regression_frame(fit, model_id = "M1")$info$extras$has_singular
  )
})

test_that("lme singular fit: the boundary VC keeps no Wald SE or CI", {
  fit <- .fit_lme_singular()
  vc <- as_regression_frame(
    fit,
    model_id = "M1"
  )$info$random_effects$variance_components
  expect_true(all(is.na(vc$std_error)))
  expect_true(all(is.na(vc$ci_lower)))
  expect_true(all(is.na(vc$ci_upper)))
  expect_true(all(is.na(vc$ci_method)))
  # The estimate itself still renders: only its uncertainty is withheld.
  expect_true(all(is.finite(vc$variance)))
})

test_that("lme singularity reads the absolute (sigma-scaled) covariance", {
  fit <- .fit_lme_singular()
  # The reStruct blocks are relative to sigma^2; scaling them back is
  # what nlme::getVarCov() itself does, so the two agree exactly.
  block <- as.matrix(fit$modelStruct$reStruct)[[1L]] * fit$sigma^2
  expect_equal(
    as.numeric(block),
    as.numeric(nlme::getVarCov(fit)),
    tolerance = 1e-12
  )
  # Collapsed by orders of magnitude, not by a hair's breadth.
  expect_lt(det(block), 1e-6)
  clean <- .fit_lme_basic()
  expect_gt(det(as.matrix(clean$modelStruct$reStruct)[[1L]] * clean$sigma^2), 1)
  # An explicit tolerance argument is honoured in both directions.
  expect_false(spicy:::.lme_is_singular(fit, tolerance = 1e-20))
  expect_true(spicy:::.lme_is_singular(.fit_lme_basic(), tolerance = 1e6))
})

test_that("gls carries no singular flag (no random structure to collapse)", {
  fr <- as_regression_frame(.fit_gls_plain(), model_id = "M1")
  expect_false(fr$info$extras$has_singular)
})
