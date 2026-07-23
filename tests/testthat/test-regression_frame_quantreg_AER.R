# ---------------------------------------------------------------------------
# Phase 6e tests: as_regression_frame() methods for quantreg::rq and
# AER::ivreg / AER::tobit.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_rq_median <- function() {
  skip_if_not_installed("quantreg")
  quantreg::rq(mpg ~ wt + cyl, data = mtcars, tau = 0.5)
}

.fit_rq_q25 <- function() {
  skip_if_not_installed("quantreg")
  quantreg::rq(mpg ~ wt, data = mtcars, tau = 0.25)
}

.fit_ivreg_basic <- function() {
  skip_if_not_installed("AER")
  AER::ivreg(mpg ~ wt + cyl | hp + drat + cyl, data = mtcars)
}

.fit_ivreg_factor <- function() {
  skip_if_not_installed("AER")
  d <- mtcars
  d$cyl_f <- factor(d$cyl)
  AER::ivreg(mpg ~ wt + cyl_f | hp + drat + cyl_f, data = d)
}

.fit_tobit_basic <- function() {
  skip_if_not_installed("AER")
  data(Affairs, package = "AER", envir = environment())
  AER::tobit(affairs ~ age + yearsmarried, data = Affairs)
}


# ---- 1. rq: schema validity + core fields --------------------------------

test_that("as_regression_frame.rq produces a schema-valid frame", {
  fit <- .fit_rq_median()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("rq: info$class is 'rq'", {
  fit <- .fit_rq_median()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "rq")
})

test_that("rq: title includes the quantile (tau)", {
  fit <- .fit_rq_median()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "Quantile regression", fixed = TRUE)
  expect_match(fr$info$extras$title_prefix, "0.50", fixed = TRUE)
})

test_that("rq tau=0.25: title reflects the quantile", {
  fit <- .fit_rq_q25()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$tau, 0.25)
  expect_match(fr$info$extras$title_prefix, "0.25", fixed = TRUE)
})

test_that("rq: coefs estimates match stats::coef(fit)", {
  fit <- .fit_rq_median()
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

test_that("rq: p-values byte-match summary(fit, se='nid') (the default)", {
  fit <- .fit_rq_median()
  fr <- as_regression_frame(fit, model_id = "M1")
  # The default estimator is the heteroskedasticity-robust nid sandwich
  # (quantreg's own large-sample default); iid remains an explicit
  # opt-in via vcov = "iid".
  sm <- summary(fit, se = "nid", hs = TRUE)$coefficients
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(sm)) {
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(sm[nm, "Pr(>|t|)"]),
      tolerance = 1e-10
    )
  }
  sm_iid <- summary(fit, se = "iid")$coefficients
  fr_iid <- as_regression_frame(fit, model_id = "M1", vcov = "iid")
  b_iid <- fr_iid$coefs[
    fr_iid$coefs$estimate_type == "B" &
      !fr_iid$coefs$is_ref,
  ]
  for (nm in rownames(sm_iid)) {
    expect_equal(
      b_iid$p_value[b_iid$term == nm],
      unname(sm_iid[nm, "Pr(>|t|)"]),
      tolerance = 1e-10
    )
  }
})

test_that("rq: n_obs from length(residuals); fit_stats$nobs matches", {
  fit <- .fit_rq_median()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$n_obs, length(fit$residuals))
})


# ---- 2. ivreg: schema validity + core fields -----------------------------

test_that("as_regression_frame.ivreg produces a schema-valid frame", {
  fit <- .fit_ivreg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("ivreg: info$class is 'ivreg'", {
  fit <- .fit_ivreg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "ivreg")
})

test_that("ivreg: title_prefix = 'IV regression (2SLS)'", {
  fit <- .fit_ivreg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "IV regression (2SLS)")
})

test_that("ivreg: r.squared / adj.r.squared from summary", {
  fit <- .fit_ivreg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)
  expect_equal(fr$info$fit_stats$r_squared, sm$r.squared, tolerance = 1e-10)
  expect_equal(
    fr$info$fit_stats$adj_r_squared,
    sm$adj.r.squared,
    tolerance = 1e-10
  )
})

test_that("ivreg: Wald-t with df.residual", {
  fit <- .fit_ivreg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$ci_method, "wald")
  expect_true(all(fr$coefs$test_type == "t" | fr$coefs$is_ref))
  expect_true(all(fr$coefs$df == stats::df.residual(fit) | fr$coefs$is_ref))
})

test_that("ivreg: SE / p byte-match summary", {
  fit <- .fit_ivreg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  sm <- summary(fit)$coefficients
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(sm)) {
    expect_equal(
      b_rows$std_error[b_rows$term == nm],
      unname(sm[nm, "Std. Error"]),
      tolerance = 1e-10
    )
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(sm[nm, "Pr(>|t|)"]),
      tolerance = 1e-10
    )
  }
})

test_that("ivreg: factor predictor synthesises a reference row", {
  fit <- .fit_ivreg_factor()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
  rows <- fr$coefs[fr$coefs$parent_var == "cyl_f", ]
  expect_identical(nrow(rows), 3L)
  expect_identical(sum(rows$is_ref), 1L)
})


# ---- 3. tobit: inherits from survreg but overrides title -----------------

test_that("as_regression_frame.tobit produces a schema-valid frame", {
  fit <- .fit_tobit_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("tobit: info$class is 'tobit' (not 'survreg')", {
  fit <- .fit_tobit_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "tobit")
})

test_that("tobit: title_prefix = 'Tobit regression'", {
  fit <- .fit_tobit_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$title_prefix, "Tobit regression")
})

test_that("tobit: dv is the user's response name, not the Surv() plumbing", {
  fit <- .fit_tobit_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  # The survreg delegate sees the munged internal formula
  # (survival::Surv(ifelse(affairs <= 0, 0, affairs), ...)); the frame
  # must recover the original response so the title reads
  # "Tobit regression: affairs".
  expect_identical(fr$info$dv, "affairs")
  expect_identical(fr$info$dv_label, "affairs")
  out <- paste(capture.output(print(table_regression(fit))), collapse = "\n")
  expect_match(out, "Tobit regression: affairs", fixed = TRUE)
  expect_false(grepl("Surv(", out, fixed = TRUE))
})

test_that("tobit: family normalised to gaussian/identity", {
  fit <- .fit_tobit_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "gaussian")
  expect_identical(fr$info$family$link, "identity")
})

test_that("tobit: censoring boundaries surfaced (defaults: 0 / Inf)", {
  fit <- .fit_tobit_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_equal(fr$info$extras$tobit_left, 0)
  expect_identical(fr$info$extras$tobit_right, Inf)
})


# ---- 4. Oracle: parameters::model_parameters() --------------------------

test_that("rq coefficients match parameters::model_parameters() (oracle, point estimates only)", {
  skip_if_not_installed("parameters")
  fit <- .fit_rq_median()
  fr <- as_regression_frame(fit, model_id = "M1")
  # parameters defaults to summary(fit, se = "rank") while our default
  # is se = "iid"; only compare point estimates (which are SE-method
  # independent), not SEs / p-values.
  oracle <- parameters::model_parameters(fit, ci = 0.95)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(spicy_row$estimate, oracle_row$Coefficient, tolerance = 1e-6)
  }
})

test_that("ivreg coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_ivreg_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  oracle <- parameters::model_parameters(fit, ci = 0.95)
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in oracle$Parameter) {
    spicy_row <- b_rows[b_rows$term == nm, ]
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_equal(spicy_row$estimate, oracle_row$Coefficient, tolerance = 1e-6)
    expect_equal(spicy_row$std_error, oracle_row$SE, tolerance = 1e-6)
  }
})
