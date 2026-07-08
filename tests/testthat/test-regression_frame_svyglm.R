# ---------------------------------------------------------------------------
# Phase 2 tests: as_regression_frame() method for survey::svyglm() fits.
#
# Coverage:
#   * Gaussian svyglm (linear)
#   * Quasibinomial svyglm (logistic)
#   * Quasipoisson svyglm (count)
#   * Schema validity in all paths
#   * Design-specific metadata: vcov_kind, vcov_label, weights_kind,
#     weighted_n, design_class
#   * Factor predictor with reference rows
#   * Oracle cross-validation against parameters::model_parameters()
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.svy_design <- function() {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  survey::svydesign(id = ~1, strata = ~stype, weights = ~pw,
                    data = apistrat, fpc = ~fpc)
}

.fit_svyglm_gaussian <- function() {
  d <- .svy_design()
  survey::svyglm(api00 ~ ell + meals, design = d)
}

.fit_svyglm_factor <- function() {
  d <- .svy_design()
  survey::svyglm(api00 ~ ell + stype, design = d)
}

.fit_svyglm_logit <- function() {
  d <- .svy_design()
  survey::svyglm(I(api00 > mean(api00)) ~ ell + stype, design = d,
                 family = quasibinomial())
}

.fit_svyglm_poisson <- function() {
  d <- .svy_design()
  # `enroll` is a count (school enrolment); use a small subset to keep
  # the Poisson realistic on the API data.
  survey::svyglm(enroll ~ ell + meals, design = d,
                 family = quasipoisson())
}


# ---- 1. Gaussian svyglm: schema validity + core info ---------------------

test_that("as_regression_frame.svyglm produces a schema-valid frame", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("svyglm: required attributes round-trip", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("svyglm gaussian: info$class is 'svyglm' (not 'glm' / 'lm')", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "svyglm")
})

test_that("svyglm gaussian: info$family is gaussian/identity", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "gaussian")
  expect_identical(fr$info$family$link,   "identity")
})

test_that("svyglm: info$dv reads the response variable name", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$dv, "api00")
})

test_that("svyglm: vcov_kind = 'survey-Taylor' + design-based label", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$vcov_kind, "survey-Taylor")
  expect_match(fr$info$vcov_label, "Design-based", fixed = TRUE)
  expect_match(fr$info$vcov_label, "Taylor", fixed = TRUE)
})

test_that("svyglm: weights_kind = 'sampling' + has_weights = TRUE", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$weights_kind, "sampling")
  expect_true(fr$info$extras$has_weights)
})

test_that("svyglm: weighted_n = sum(weights(design))", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expected_wn <- sum(stats::weights(fit$survey.design))
  expect_equal(fr$info$extras$weighted_n, expected_wn, tolerance = 1e-10)
})


# ---- 2. Gaussian svyglm: coef extraction ---------------------------------

test_that("svyglm gaussian: coefs estimates match coef(fit)", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(b_rows$estimate[b_rows$term == nm],
                 unname(legacy[nm]), tolerance = 1e-10)
  }
})

test_that("svyglm: coefs SE matches sqrt(diag(vcov))", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  V <- as.matrix(stats::vcov(fit))
  expected_se <- sqrt(diag(V))
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(expected_se)) {
    expect_equal(b_rows$std_error[b_rows$term == nm],
                 unname(expected_se[nm]), tolerance = 1e-10)
  }
})

test_that("svyglm: ci_method = 'wald' and test_type = 't' with df = df.residual()", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "t" | is.na(fr$coefs$test_type)))
  expected_df <- as.numeric(stats::df.residual(fit))
  fixed_df <- fr$coefs$df[!fr$coefs$is_ref]
  expect_true(all(fixed_df == expected_df))
})


# ---- 3. Factor predictor: reference row ----------------------------------

test_that("svyglm with treatment factor: 1 ref row + non-ref levels", {
  fit <- .fit_svyglm_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  stype_rows <- fr$coefs[fr$coefs$parent_var == "stype", ]
  # `stype` has 3 levels (E / H / M); 1 ref + 2 non-ref
  expect_identical(nrow(stype_rows), 3L)
  expect_identical(sum(stype_rows$is_ref), 1L)
  expect_true(all(is.na(stype_rows$estimate[stype_rows$is_ref])))
})


# ---- 4. svyglm: supports capability flags --------------------------------

test_that("svyglm supports flags: ame TRUE, nested_lrt FALSE, classical_r2 FALSE", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  sp <- fr$info$supports
  expect_true(sp$ame)
  expect_false(sp$partial_effect_size)
  expect_false(sp$classical_r2)
  expect_false(sp$nested_lrt)
  expect_false(sp$exponentiate)   # identity link
  # svyglm has no standardized-coefficients attach path: the flag is
  # FALSE so table_regression() refuses `standardized` fail-fast
  # instead of rendering an empty beta column.
  expect_false(sp$standardise_refit)
})

test_that("svyglm logit: supports$exponentiate = TRUE (logit link)", {
  fit <- .fit_svyglm_logit()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})


# ---- 5. Family-aware title_prefix ---------------------------------------

test_that("svyglm gaussian: title_prefix names the linear context", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix,
                   "Survey-weighted linear regression")
})

test_that("svyglm logit: title_prefix names the logistic context", {
  fit <- .fit_svyglm_logit()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "logistic", fixed = TRUE)
  expect_match(fr$info$extras$title_prefix, "Survey-weighted",
               fixed = TRUE)
})

test_that("svyglm poisson: title_prefix names the Poisson context", {
  fit <- .fit_svyglm_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "Poisson", fixed = TRUE)
})


# ---- 6. Quasibinomial svyglm (logit) -------------------------------------

test_that("svyglm logit: family = quasibinomial / logit", {
  fit <- .fit_svyglm_logit()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "quasibinomial")
  expect_identical(fr$info$family$link,   "logit")
})

test_that("svyglm logit: schema validity", {
  fit <- .fit_svyglm_logit()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})


# ---- 7. Quasipoisson svyglm (log) ---------------------------------------

test_that("svyglm poisson: family = quasipoisson / log", {
  fit <- .fit_svyglm_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "quasipoisson")
  expect_identical(fr$info$family$link,   "log")
})

test_that("svyglm poisson: schema validity", {
  fit <- .fit_svyglm_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})


# ---- 8. design_class metadata in info$extras ----------------------------

test_that("svyglm: extras$design_class names the survey design class", {
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")
  # apistrat is a stratified design -> "survey.design2" or similar.
  expect_match(fr$info$extras$design_class, "design", fixed = TRUE)
})


# ---- 9. Oracle: parameters::model_parameters() --------------------------

test_that("svyglm gaussian coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_svyglm_gaussian()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci         = 0.95,
    ci_method  = "wald",
    test       = NULL,
    exponentiate = FALSE
  )

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row  <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(spicy_row$estimate,  oracle_row$Coefficient, tolerance = 1e-6,
                 info = paste("oracle B mismatch on term:", nm))
    expect_equal(spicy_row$std_error, oracle_row$SE,          tolerance = 1e-6,
                 info = paste("oracle SE mismatch on term:", nm))
    expect_equal(spicy_row$p_value,   oracle_row$p,           tolerance = 1e-6,
                 info = paste("oracle p mismatch on term:", nm))
  }
})

test_that("svyglm logit coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_svyglm_logit()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci         = 0.95,
    ci_method  = "wald",
    test       = NULL,
    exponentiate = FALSE
  )

  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row  <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(spicy_row$estimate,  oracle_row$Coefficient, tolerance = 1e-6,
                 info = paste("oracle B mismatch on term:", nm))
    expect_equal(spicy_row$std_error, oracle_row$SE,          tolerance = 1e-6,
                 info = paste("oracle SE mismatch on term:", nm))
    expect_equal(spicy_row$p_value,   oracle_row$p,           tolerance = 1e-6,
                 info = paste("oracle p mismatch on term:", nm))
  }
})
