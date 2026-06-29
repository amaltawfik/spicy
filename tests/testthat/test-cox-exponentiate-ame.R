# Cox / survreg exponentiation + AME-rejection behaviour.
#
# Cox PH (coxph / rms::cph) and AFT (survreg) fits have NO stats::family()
# method, so the exponentiate path reads the link from the frame's own
# info$family. coxph -> hazard ratio (HR); log-scale survreg -> time ratio
# (TR); identity-scale survreg (gaussian / logistic / t) is not exponentiable.
# AME is disabled for Cox (undefined / unreliable SEs).

.cox_fit <- function() {
  skip_if_not_installed("survival")
  survival::coxph(survival::Surv(time, status) ~ age + sex,
                  data = survival::lung)
}

shown_b <- function(tr) {
  lo <- attr(tr, "spicy_long")
  lo$estimate[lo$estimate_type == "B" & !(lo$is_ref %in% TRUE) & !is.na(lo$estimate)]
}


test_that("coxph exponentiate = TRUE yields hazard ratios (cross-checked vs marginaleffects coef)", {
  fit <- .cox_fit()
  tr <- table_regression(fit, exponentiate = TRUE)
  hr <- shown_b(tr)
  # exp(coef) are the hazard ratios; pin to the model's own coefficients.
  expect_equal(unname(hr), unname(exp(stats::coef(fit))), tolerance = 1e-6)
  # Header label + footnote say HR, not raw B.
  out <- paste(capture.output(print(tr)), collapse = "\n")
  expect_match(out, "HR", fixed = TRUE)
  expect_match(out, "hazard ratio", fixed = TRUE)
})

test_that("coxph exponentiate matches parameters::model_parameters (oracle)", {
  fit <- .cox_fit()
  skip_if_not_installed("parameters")
  oracle <- parameters::model_parameters(fit, exponentiate = TRUE)$Coefficient
  expect_equal(unname(shown_b(table_regression(fit, exponentiate = TRUE))),
               unname(oracle), tolerance = 1e-4)
})

test_that("coxph exponentiate does NOT emit the spurious identity-link warning", {
  fit <- .cox_fit()
  w <- character(0)
  withCallingHandlers(
    table_regression(fit, exponentiate = TRUE),
    warning = function(wn) { w <<- c(w, conditionMessage(wn)); invokeRestart("muffleWarning") }
  )
  expect_false(any(grepl("identity-link", w)))
})

test_that("survreg log-scale dist exponentiates to time ratios; gaussian does not", {
  skip_if_not_installed("survival")
  wb <- survival::survreg(survival::Surv(time, status) ~ age + sex,
                          data = survival::lung, dist = "weibull")
  tr <- table_regression(wb, exponentiate = TRUE)
  # survreg carries an intercept (coxph does not); all B rows exponentiate.
  expect_equal(unname(shown_b(tr)), unname(exp(stats::coef(wb))), tolerance = 1e-6)
  expect_match(paste(capture.output(print(tr)), collapse = "\n"), "TR", fixed = TRUE)

  # Identity-scale gaussian AFT: exp() has no effect -> warns, not applied.
  gg <- survival::survreg(survival::Surv(time, status) ~ age,
                          data = survival::lung, dist = "gaussian")
  expect_warning(table_regression(gg, exponentiate = TRUE), "identity-link")
})

test_that("AME tokens are rejected for Cox models with a clear error", {
  fit <- .cox_fit()
  err <- tryCatch(table_regression(fit, show_columns = c("b", "ame")),
                  error = function(e) e)
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "Cox", fixed = TRUE)
  expect_match(conditionMessage(err), "exponentiate", fixed = TRUE)
})

test_that("Cox frames advertise supports$ame = FALSE", {
  fit <- .cox_fit()
  expect_false(spicy:::as_regression_frame(fit, model_id = "M1")$info$supports$ame)
})
