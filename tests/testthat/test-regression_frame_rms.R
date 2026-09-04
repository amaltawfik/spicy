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
  rms::cph(survival::Surv(time, status) ~ age + sex, data = survival::lung)
}

.fit_Glm_poisson <- function() {
  skip_if_not_installed("rms")
  d <- mtcars
  d$am_num <- as.numeric(d$am)
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
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(legacy[raw_nm]),
      tolerance = 1e-10
    )
  }
})

test_that("ols: r2 + sigma from fit$stats", {
  fit <- .fit_ols_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(
    fr$info$fit_stats$r_squared,
    as.numeric(fit$stats["R2"]),
    tolerance = 1e-10
  )
  expect_equal(
    fr$info$fit_stats$sigma,
    as.numeric(fit$stats["Sigma"]),
    tolerance = 1e-10
  )
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
  expect_identical(fr$info$family$link, "logit")
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
  expect_match(
    fr$info$extras$title_prefix,
    "Cox proportional hazards",
    fixed = TRUE
  )
})

test_that("cph: family is cox/log", {
  fit <- .fit_cph_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "cox")
  expect_identical(fr$info$family$link, "log")
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
  expect_identical(fr$info$family$link, "log")
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


## ---- Phase 3 matrix (lot T2) ----------------------------------------------

# Phase 3 matrix: rd-vcov-classes:registry-ols
test_that("ols AME matches avg_slopes; exponentiate on identity is a warned no-op", {
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  set.seed(3)
  d <- data.frame(x1 = rnorm(150), x2 = rnorm(150))
  d$y <- 1 + 0.5 * d$x1 - 0.2 * d$x2 + rnorm(150)
  fit <- rms::ols(y ~ x1 + x2, data = d, x = TRUE, y = TRUE)
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
  expect_identical(nrow(a), nrow(orc))
  idx <- match(a$term, orc$term)
  expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-8)
  expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-8)
  # exponentiate = TRUE on the identity link: loud no-op.
  expect_warning(
    table_regression(fit, exponentiate = TRUE, output = "data.frame"),
    class = "spicy_ignored_arg"
  )
  t_exp <- suppressWarnings(
    table_regression(fit, exponentiate = TRUE, output = "data.frame")
  )
  expect_identical(t_exp, table_regression(fit, output = "data.frame"))
})

# Phase 3 matrix: rd-vcov-classes:registry-lrm
test_that("lrm exponentiates to OR and its AME matches avg_slopes", {
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  set.seed(3)
  d <- data.frame(x1 = rnorm(150), x2 = rnorm(150))
  d$yb <- rbinom(150, 1, plogis(d$x1))
  fit <- rms::lrm(yb ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  # Rendered header: the B column is relabelled OR.
  t_or <- table_regression(fit, exponentiate = TRUE, output = "data.frame")
  expect_true("OR" %in% names(t_or))
  # exp applied once: OR rows equal exp(raw B).
  fr_raw <- as_regression_frame(fit)
  e <- spicy:::.apply_exp_to_frame(fr_raw$coefs, fr_raw$info, TRUE)
  expect_identical(e$info$extras$exp_header, "OR")
  expect_equal(
    e$coefs$estimate[e$coefs$estimate_type == "B"],
    exp(fr_raw$coefs$estimate[fr_raw$coefs$estimate_type == "B"]),
    tolerance = 1e-10
  )
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
  expect_identical(nrow(a), nrow(orc))
  idx <- match(a$term, orc$term)
  expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-8)
  expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-8)
})

# Phase 3 matrix: rd-vcov-classes:registry-Glm
test_that("Glm poisson exponentiates to IRR; AME matches the glm-equivalent oracle", {
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  set.seed(3)
  d <- data.frame(x1 = rnorm(150), x2 = rnorm(150))
  d$cnt <- stats::rpois(150, exp(0.3 + 0.3 * d$x1))
  fit <- rms::Glm(
    cnt ~ x1 + x2,
    data = d,
    family = poisson(),
    x = TRUE,
    y = TRUE
  )
  t_irr <- table_regression(fit, exponentiate = TRUE, output = "data.frame")
  expect_true("IRR" %in% names(t_irr))
  fr_raw <- as_regression_frame(fit)
  e <- spicy:::.apply_exp_to_frame(fr_raw$coefs, fr_raw$info, TRUE)
  expect_identical(e$info$extras$exp_header, "IRR")
  # AME rides the class-stripped glm path: marginaleffects cannot read the
  # rms Glm directly, so the oracle is avg_slopes() on the identical
  # stats::glm() fit.
  fr <- suppressWarnings(as_regression_frame(fit, show_columns = c("b", "ame")))
  expect_true(isTRUE(fr$info$supports$ame))
  a <- fr$coefs[
    fr$coefs$estimate_type == "ame" & !(fr$coefs$is_ref %in% TRUE),
    ,
    drop = FALSE
  ]
  glm_twin <- stats::glm(cnt ~ x1 + x2, data = d, family = poisson())
  orc <- as.data.frame(suppressWarnings(
    marginaleffects::avg_slopes(glm_twin, df = Inf)
  ))
  expect_identical(nrow(a), nrow(orc))
  idx <- match(a$term, orc$term)
  expect_equal(a$estimate, orc$estimate[idx], tolerance = 1e-8)
  # marginaleffects >= 1.0.0 computes glm standard errors through analytic
  # Jacobians while rms::Glm keeps the numerical path, so the two engines
  # agree to differentiation precision (~1e-6 relative), not 1e-8.
  expect_equal(a$std_error, orc$std.error[idx], tolerance = 1e-5)
})

# Phase 3 matrix: rd-vcov-classes:registry-cph
test_that("cph exponentiates to HR and refuses the ame column", {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  set.seed(3)
  d <- data.frame(x1 = rnorm(150), x2 = rnorm(150))
  d$time <- rexp(150, exp(-0.2 * d$x1))
  d$status <- rbinom(150, 1, 0.7)
  fit <- rms::cph(
    survival::Surv(time, status) ~ x1 + x2,
    data = d,
    x = TRUE,
    y = TRUE
  )
  fr_raw <- as_regression_frame(fit)
  fr_exp <- as_regression_frame(fit, exponentiate = TRUE)
  e <- spicy:::.apply_exp_to_frame(fr_exp$coefs, fr_exp$info, TRUE)
  expect_identical(e$info$extras$exp_header, "HR")
  expect_true(isTRUE(e$info$extras$exp_applied))
  expect_equal(
    e$coefs$estimate[e$coefs$estimate_type == "B"],
    exp(fr_raw$coefs$estimate[fr_raw$coefs$estimate_type == "B"]),
    tolerance = 1e-10
  )
  # AME is refused for Cox-family fits (ambiguous hazard scale).
  expect_false(isTRUE(fr_raw$info$supports$ame))
  expect_error(
    table_regression(fit, show_columns = c("b", "ame"), output = "data.frame"),
    class = "spicy_invalid_input"
  )
})
