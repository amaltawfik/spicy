# ---------------------------------------------------------------------------
# Phase 4b tests: as_regression_frame() methods for nlme fits.
#
# Coverage:
#   * lme (linear mixed-effects) -- fixed effects extraction, per-coef
#     Wald-t DF from summary(fit)$tTable, VarCorr parsing, ICC.
#   * gls (generalized least squares) -- Wald-t with df = nobs - p,
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
  expect_true(sp$partial_effect_size) # partial chi^2 path
  expect_false(sp$classical_r2)
  expect_true(sp$nested_lrt)
  expect_false(sp$exponentiate)
  expect_true(sp$standardise_refit)
})

test_that("lme: title_prefix = 'Linear mixed-effects regression', no engine", {
  fit <- .fit_lme_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Linear mixed-effects regression"
  )
  expect_no_match(
    fr$info$extras$title_prefix,
    "\\((glmmTMB|nlme|rms|mlogit|stanreg|brmsfit)\\)"
  )
})


# ---- 7. lme: factor predictor reference rows -----------------------------

test_that("lme: factor predictor synthesizes a reference row", {
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

test_that("gls: title_prefix = 'Generalized least squares', no engine", {
  fit <- .fit_gls_corcs()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Generalized least squares"
  )
  expect_no_match(
    fr$info$extras$title_prefix,
    "\\((glmmTMB|nlme|rms|mlogit|stanreg|brmsfit)\\)"
  )
})

test_that("gls: supports$partial_effect_size = FALSE (no attach helper)", {
  # The FALSE side of the same flag its lme sibling declares TRUE: only
  # as_regression_frame.lme() calls
  # .attach_partial_chi2_to_frame_coefs(). The two builders differ, so
  # the two declarations differ -- that is the flag doing its job.
  fit <- .fit_gls_corcs()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_false(fr$info$supports$partial_effect_size)
  expect_true(fr$info$supports$nested_lrt)
})

test_that("gls: factor predictor synthesizes a reference row", {
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
  n_checked <- 0L
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    # Both lookups must hit exactly one row: an unmatched term
    # would otherwise compare a zero-row frame and the counter
    # below would never see it.
    expect_identical(nrow(oracle_row), 1L, info = nm)
    expect_identical(nrow(spicy_row), 1L, info = nm)
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
    n_checked <- n_checked + 1L
  }
  expect_oracle_covered(n_checked, length(oracle$Parameter))
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
  n_checked <- 0L
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    # Both lookups must hit exactly one row: an unmatched term
    # would otherwise compare a zero-row frame and the counter
    # below would never see it.
    expect_identical(nrow(oracle_row), 1L, info = nm)
    expect_identical(nrow(spicy_row), 1L, info = nm)
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
    n_checked <- n_checked + 1L
  }
  expect_oracle_covered(n_checked, length(oracle$Parameter))
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

# nlme has no isSingular() of its own. The criterion is
# det(V / sigma^2) < 1e-5 per nesting level, read straight off the
# reStruct blocks (already relative to sigma^2). Being relative, it is
# invariant to the units of the response -- the property that ties the
# verdict to lme4::isSingular() rather than to
# performance::check_singularity.lme(), which tests absolute variances
# and is pinned as a documented divergence below.

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

test_that("lme singularity is invariant to the scale of the response", {
  # The property the absolute-variance criterion did not have. A boundary
  # fit stays a boundary fit when the response is multiplied by 100, and
  # a healthy one stays healthy when it is divided by 20 -- lme4 says the
  # same on the equivalent lmer fit at every scale.
  skip_if_not_installed("lme4")
  for (k in c(1, 100, 10000)) {
    d <- mtcars
    d$gearf <- factor(d$gear)
    d$mpg <- d$mpg * k
    fit <- nlme::lme(mpg ~ wt + hp, data = d, random = ~ 1 | gearf)
    twin <- suppressMessages(suppressWarnings(
      lme4::lmer(mpg ~ wt + hp + (1 | gearf), data = d)
    ))
    expect_true(
      spicy:::.lme_is_singular(fit),
      info = paste("collapsed component missed at scale", k)
    )
    expect_true(lme4::isSingular(twin), info = paste("twin at scale", k))
  }
  for (k in c(1, 1 / 20, 1 / 100)) {
    d <- nlme::Orthodont
    d$distance <- d$distance * k
    fit <- nlme::lme(distance ~ age, data = d, random = ~ age | Subject)
    twin <- suppressMessages(suppressWarnings(
      lme4::lmer(distance ~ age + (age | Subject), data = d)
    ))
    expect_false(
      spicy:::.lme_is_singular(fit),
      info = paste("healthy fit flagged at scale", k)
    )
    expect_false(lme4::isSingular(twin), info = paste("twin at scale", k))
  }
})

test_that("lme singularity agrees with the oracle where the scale is neutral", {
  skip_if_not_installed("performance")
  # On fits whose sigma is near 1 the relative and absolute rules
  # coincide, and performance::check_singularity() is a genuine oracle.
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

test_that("the lme singularity verdict does not move with the units of the response", {
  skip_if_not_installed("performance")
  skip_if_not_installed("lme4")
  # Before 0.19.0, performance tested absolute variances, so its verdict
  # moved with the units of the response; since 0.19.0 it reads the
  # relative reStruct diagonal and agrees with us. Both cases below are
  # pinned to OUR verdict, with lme4::isSingular() on the equivalent lmer
  # fit as the tiebreaker.
  old_performance <- utils::packageVersion("performance") < "0.19.0"

  # (a) Healthy fit, response divided by 100: old performance said singular.
  d <- nlme::Orthodont
  d$distance <- d$distance / 100
  fit <- nlme::lme(distance ~ age, data = d, random = ~ age | Subject)
  twin <- suppressMessages(suppressWarnings(
    lme4::lmer(distance ~ age + (age | Subject), data = d)
  ))
  expect_identical(isTRUE(performance::check_singularity(fit)), old_performance)
  expect_false(lme4::isSingular(twin))
  expect_false(spicy:::.lme_is_singular(fit))

  # (b) Collapsed component, response multiplied by 100: old performance
  # stopped seeing it, and the table would print the Wald SE of a boundary
  # estimate.
  d2 <- mtcars
  d2$gearf <- factor(d2$gear)
  d2$mpg <- d2$mpg * 100
  fit2 <- nlme::lme(mpg ~ wt + hp, data = d2, random = ~ 1 | gearf)
  twin2 <- suppressMessages(suppressWarnings(
    lme4::lmer(mpg ~ wt + hp + (1 | gearf), data = d2)
  ))
  expect_identical(
    isTRUE(performance::check_singularity(fit2)),
    !old_performance
  )
  expect_true(lme4::isSingular(twin2))
  expect_true(spicy:::.lme_is_singular(fit2))
})

test_that("lme singularity answers for nested fits, where the oracle cannot", {
  skip_if_not_installed("performance")
  fit <- .fit_lme_nested()
  # nlme::getVarCov() -- the route performance took before 0.19.0 --
  # refuses more than one level of nesting; the reStruct walk covers both.
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

test_that("lme singularity reads the reStruct blocks relative to sigma^2", {
  fit <- .fit_lme_singular()
  block <- as.matrix(fit$modelStruct$reStruct)[[1L]]
  # The blocks ARE the covariance relative to sigma^2: multiplying back
  # is what nlme::getVarCov() does to report absolute variances, and that
  # is precisely the step the criterion must not take.
  expect_equal(
    as.numeric(block * fit$sigma^2),
    as.numeric(nlme::getVarCov(fit)),
    tolerance = 1e-12
  )
  # Collapsed by orders of magnitude, not by a hair's breadth.
  expect_lt(det(block), 1e-6)
  clean <- .fit_lme_basic()
  expect_gt(det(as.matrix(clean$modelStruct$reStruct)[[1L]]), 1)
  # An explicit tolerance argument is honored. Only the loosening
  # direction is asserted: how far below the tolerance the optimizer
  # happened to stop is not a property of the fit.
  expect_true(spicy:::.lme_is_singular(clean, tolerance = 1e6))
})

test_that("an lme correlation pinned at 1 counts as singular (pdCompSymm)", {
  # pdCompSymm with a single covariate gives var == cov, i.e. a
  # correlation of exactly 1: a rank-1 block with healthy variances. No
  # variance is at 0, which is why the note speaks of the boundary of the
  # parameter space rather than of a zero variance.
  fit <- nlme::lme(
    distance ~ age,
    data = nlme::Orthodont,
    random = list(Subject = nlme::pdCompSymm(~age))
  )
  block <- as.matrix(fit$modelStruct$reStruct)[[1L]]
  expect_gt(min(diag(block)), 1e-3)
  expect_true(spicy:::.lme_is_singular(fit))
  expect_true(
    as_regression_frame(fit, model_id = "M1")$info$extras$has_singular
  )
})

test_that(".lme_blank_degenerate_vc drops non-finite Wald quantities", {
  # Independent of the boundary flag: whatever their origin, NaN or Inf
  # cells must not reach the table.
  vc <- data.frame(
    group = c("g", "g", "Residual"),
    term = c("(Intercept)", "x", ""),
    variance = c(1, 2, 3),
    std_error = c(NaN, 0.5, Inf),
    ci_lower = c(0, 0.1, 0),
    ci_upper = c(Inf, 0.9, 10),
    ci_method = c("wald", "wald", "wald"),
    stringsAsFactors = FALSE
  )
  out <- spicy:::.lme_blank_degenerate_vc(vc)
  expect_identical(is.na(out$std_error), c(TRUE, FALSE, TRUE))
  expect_identical(is.na(out$ci_upper), c(TRUE, FALSE, TRUE))
  expect_identical(out$ci_method, c(NA, "wald", NA))
  # The healthy row passes through untouched, estimates are never lost.
  expect_identical(out$std_error[2L], 0.5)
  expect_identical(out$variance, c(1, 2, 3))
  # Idempotent: a frame that is already clean comes back unchanged.
  expect_identical(spicy:::.lme_blank_degenerate_vc(out), out)
})

test_that("gls carries no singular flag (no random structure to collapse)", {
  fr <- as_regression_frame(.fit_gls_plain(), model_id = "M1")
  expect_false(fr$info$extras$has_singular)
})

test_that("an lme or gls fitted inside a function gives the table of the same model fitted outside", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("broom")
  # nlme::getData() looks for `dd` from its own frame: the reference row
  # of the factor, the conditional R2, the random-effect LR test and the
  # partial tests of the fit were lost.
  set.seed(7)
  d <- data.frame(
    y = rnorm(60),
    f = factor(rep(c("a", "b", "c"), 20)),
    x = rnorm(60),
    cl = factor(rep(1:10, 6))
  )
  d$y <- d$y + (d$f == "b") + as.numeric(d$cl) / 3
  # The oracle gls reads data that getData() finds: a global object (an
  # lme keeps its own data).
  assign(".nlme_d", d, envir = globalenv())
  withr::defer(rm(".nlme_d", envir = globalenv()))
  local_lme <- function(dd) {
    nlme::lme(y ~ f + x, random = ~ 1 | cl, data = dd, keep.data = FALSE)
  }
  local_gls <- function(dd) nlme::gls(y ~ f + x, data = dd)
  pairs <- list(
    lme = list(
      local_lme(d),
      nlme::lme(y ~ f + x, random = ~ 1 | cl, data = d)
    ),
    gls = list(local_gls(d), nlme::gls(y ~ f + x, data = .nlme_d))
  )
  expect_s3_class(nlme::getData(pairs$gls[[2L]]), "data.frame")
  args <- list(
    lme = list(
      default = list(),
      II = list(show_columns = c("b", "partial_chi2")),
      III = list(show_columns = c("b", "partial_chi2"), partial_type = "III")
    ),
    gls = list(default = list())
  )
  for (cls in names(pairs)) {
    for (k in names(args[[cls]])) {
      tab <- function(fit) {
        suppressWarnings(do.call(
          table_regression,
          c(list(fit), args[[cls]][[k]])
        ))
      }
      inside <- tab(pairs[[cls]][[1L]])
      outside <- tab(pairs[[cls]][[2L]])
      expect_identical(
        capture.output(print(inside)),
        capture.output(print(outside)),
        info = paste(cls, k)
      )
      expect_equal(
        as.data.frame(broom::tidy(inside)),
        as.data.frame(broom::tidy(outside)),
        info = paste(cls, k)
      )
    }
  }
  td <- suppressWarnings(table_regression(
    pairs$lme[[1L]],
    show_columns = c("b", "partial_chi2"),
    partial_type = "III",
    output = "long"
  ))
  expect_identical(td$term[td$is_reference %in% TRUE], "fa")
  expect_true(any(td$estimate_type == "partial_chi2" & !is.na(td$statistic)))
})

test_that("an lme whose data are gone keeps its coefficients, without partial tests", {
  skip_if_not_installed("nlme")
  skip_if_not_installed("broom")
  set.seed(7)
  d <- data.frame(
    yy_gone = rnorm(40),
    ff_gone = factor(rep(c("a", "b"), 20)),
    cl_gone = factor(rep(1:8, 5))
  )
  gone <- local({
    dd <- d
    fit <- nlme::lme(
      yy_gone ~ ff_gone,
      random = ~ 1 | cl_gone,
      data = dd,
      keep.data = FALSE
    )
    rm(dd)
    fit
  })
  expect_null(.nlme_get_data(gone))
  # Each reader of the data has no answer rather than an error.
  expect_null(.null_lrt_lme(gone))
  expect_identical(.lme_re_variance_contribution(gone, NULL), NA_real_)
  expect_null(.compute_partial_chi2_rows_for_mixed(gone))
  expect_null(.compute_beta_rows_for_mixed(gone))
  expect_error(.type3_terms_data(gone), "cannot be found")
  # Type III is not refused for lack of data: its cells stay empty.
  t3 <- broom::tidy(suppressWarnings(table_regression(
    gone,
    show_columns = c("b", "partial_chi2"),
    partial_type = "III"
  )))
  expect_false(any(t3$estimate_type == "partial_chi2" & !is.na(t3$statistic)))
  # One warning per table, which names every model whose data are gone.
  ok <- nlme::lme(yy_gone ~ ff_gone, random = ~ 1 | cl_gone, data = d)
  fallbacks <- function(models) {
    w <- character(0)
    withCallingHandlers(
      table_regression(models, show_columns = c("b", "partial_chi2")),
      spicy_fallback = function(cnd) {
        w <<- c(w, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      },
      warning = function(cnd) invokeRestart("muffleWarning")
    )
    w
  }
  w <- fallbacks(list(gone, ok, gone))
  expect_length(w, 1L)
  expect_match(w, "The data of Model 1, Model 3 cannot be found", fixed = TRUE)
  expect_match(w, "reference rows of the factors", fixed = TRUE)
  # None when the data are found.
  expect_length(fallbacks(ok), 0L)
  # The note names the test of the class, though the cells are empty.
  note <- paste(
    capture.output(print(suppressWarnings(table_regression(
      gone,
      show_columns = c("b", "partial_chi2")
    )))),
    collapse = " "
  )
  expect_match(note, "partial Wald chi-squared", fixed = TRUE)
  expect_no_match(note, "likelihood-ratio chi-squared", fixed = TRUE)
  td <- broom::tidy(suppressWarnings(table_regression(
    gone,
    show_columns = c("b", "partial_chi2")
  )))
  expect_equal(
    td$estimate[td$term == "ff_goneb" & td$estimate_type == "B"],
    unname(nlme::fixef(gone)[["ff_goneb"]])
  )
  expect_false(any(td$estimate_type == "partial_chi2" & !is.na(td$statistic)))
})
