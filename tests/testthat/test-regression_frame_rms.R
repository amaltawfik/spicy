# ---------------------------------------------------------------------------
# Phase 6g tests: as_regression_frame() methods for rms fits.
# ---------------------------------------------------------------------------


# ---- Fixtures -------------------------------------------------------------

.fit_ols_basic <- function() {
  skip_if_not_installed("rms")
  d <- mtcars
  d$am_f <- factor(d$am, labels = c("auto", "manual"))
  rms::ols(mpg ~ wt + cyl + am_f, data = d)
}

.fit_lrm_basic <- function() {
  skip_if_not_installed("rms")
  d <- mtcars
  d$am_num <- as.numeric(d$am)
  rms::lrm(am_num ~ wt + cyl, data = d)
}

.fit_cph_basic <- function() {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  rms::cph(survival::Surv(time, status) ~ age + sex,
           data = survival::lung)
}

.fit_Glm_poisson <- function() {
  skip_if_not_installed("rms")
  d <- mtcars; d$am_num <- as.numeric(d$am)
  rms::Glm(am_num ~ wt + cyl, data = d, family = poisson)
}


# ---- 1. ols: schema validity + core fields -------------------------------

test_that("as_regression_frame.ols produces a schema-valid frame", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("ols: info$class is 'ols'", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "ols")
})

test_that("ols: title_prefix = 'Linear regression (rms)'", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Linear regression (rms)")
})

test_that("ols: 'Intercept' renamed to '(Intercept)' for schema consistency", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true("(Intercept)" %in% fr$coefs$term)
  expect_false("Intercept" %in% fr$coefs$term)
})

test_that("ols: factor predictor parsed from 'varname=level' syntax", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  rows <- fr$coefs[fr$coefs$parent_var == "am_f", ]
  expect_identical(nrow(rows), 2L)
  expect_identical(sum(rows$is_ref), 1L)
  # Non-ref row: parent_var = "am_f", label = "manual"
  non_ref <- rows[!rows$is_ref, ]
  expect_identical(non_ref$label, "manual")
  # Ref row: label = "auto"
  ref <- rows[rows$is_ref, ]
  expect_identical(ref$label, "auto")
})

test_that("ols: coefs estimates match stats::coef(fit)", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (raw_nm in names(legacy)) {
    nm <- if (raw_nm == "Intercept") "(Intercept)" else raw_nm
    expect_equal(b_rows$estimate[b_rows$term == nm],
                 unname(legacy[raw_nm]),
                 tolerance = 1e-10)
  }
})

test_that("ols: r2 + sigma from fit$stats", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$fit_stats$r_squared,
               as.numeric(fit$stats["R2"]),
               tolerance = 1e-10)
  expect_equal(fr$info$fit_stats$sigma,
               as.numeric(fit$stats["Sigma"]),
               tolerance = 1e-10)
})


# ---- 2. ols: Wald-t inference ------------------------------------------

test_that("ols: Wald-t (test_type='t', finite df, ci_method='wald')", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "t"))
  expect_true(all(is.finite(b_rows$df)))
})


# ---- 3. lrm: schema validity --------------------------------------------

test_that("as_regression_frame.lrm produces a schema-valid frame", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("lrm: info$class is 'lrm'", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "lrm")
})

test_that("lrm: family is binomial/logit", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "binomial")
  expect_identical(fr$info$family$link,   "logit")
})

test_that("lrm: title_prefix = 'Logistic regression (rms)'", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Logistic regression (rms)")
})

test_that("lrm: pseudo_r2 carries Nagelkerke + C-index + Brier", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  pr <- fr$info$fit_stats$pseudo_r2
  expect_true(is.numeric(pr$nagelkerke))
  expect_true(is.numeric(pr$c_index))
  expect_true(is.numeric(pr$brier))
})

test_that("lrm: Wald z-asymptotic (test_type='z', df=Inf)", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "z"))
  expect_true(all(is.infinite(b_rows$df)))
})

test_that("lrm: supports$exponentiate = TRUE (odds ratios)", {
  fit <- .fit_lrm_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})


# ---- 4. cph: schema validity --------------------------------------------

test_that("as_regression_frame.cph produces a schema-valid frame", {
  fit <- .fit_cph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("cph: info$class is 'cph'", {
  fit <- .fit_cph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "cph")
})

test_that("cph: title_prefix names Cox PH", {
  fit <- .fit_cph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "Cox proportional hazards",
               fixed = TRUE)
})

test_that("cph: family is cox/log", {
  fit <- .fit_cph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "cox")
  expect_identical(fr$info$family$link,   "log")
})

test_that("cph: dv is full Surv(...) LHS expression", {
  fit <- .fit_cph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$dv, "Surv", fixed = TRUE)
})

test_that("cph: pseudo_r2 carries Nagelkerke + Dxy", {
  fit <- .fit_cph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  pr <- fr$info$fit_stats$pseudo_r2
  expect_true(is.numeric(pr$nagelkerke))
  expect_true(is.numeric(pr$dxy))
})


# ---- 5. Glm Poisson -----------------------------------------------------

test_that("Glm Poisson: schema valid; family poisson/log", {
  fit <- .fit_Glm_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  expect_identical(fr$info$class, "Glm")
  expect_identical(fr$info$family$family, "poisson")
  expect_identical(fr$info$family$link,   "log")
  expect_match(fr$info$extras$title_prefix, "Poisson", fixed = TRUE)
})

test_that("Glm Poisson: supports$exponentiate = TRUE", {
  fit <- .fit_Glm_poisson()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})


# ---- 6. info$extras$rms_stats carries the rms summary slot --------------

test_that("rms: info$extras$rms_stats is a list (the fit$stats summary)", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.list(fr$info$extras$rms_stats))
  expect_true("R2" %in% names(fr$info$extras$rms_stats))
})
