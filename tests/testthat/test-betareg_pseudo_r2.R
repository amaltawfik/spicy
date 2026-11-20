# The pseudo-R2 of a beta regression (Ferrari & Cribari-Neto 2004): the
# squared correlation between the linear predictor of the mean and the
# response on the scale of the mean link, over the rows of positive
# weight (weighted by the case weights when there are any). A
# show_fit_stats token on request, for betareg fits only.

.bpr_fit <- function(...) {
  e <- new.env()
  utils::data("GasolineYield", package = "betareg", envir = e)
  betareg::betareg(yield ~ batch + temp, data = e$GasolineYield, ...)
}

.bpr_hand <- function(fit) {
  eta <- drop(
    stats::model.matrix(fit, model = "mean") %*%
      stats::coef(fit, model = "mean")
  )
  gy <- fit$link$mean$linkfun(fit$y)
  w <- fit$weights
  if (is.null(w)) {
    return(stats::cor(eta, gy)^2)
  }
  stats::cov.wt(cbind(eta, gy)[w > 0, ], wt = w[w > 0], cor = TRUE)$cor[1, 2]^2
}

test_that("the token gives summary()'s pseudo-R2, recomputed by hand", {
  skip_if_not_installed("betareg")
  for (fit in list(
    .bpr_fit(),
    .bpr_fit(link = "probit"),
    .bpr_fit(weights = rep(c(1, 2, 0.5, 3), length.out = 32))
  )) {
    ref <- summary(fit)$pseudo.r.squared
    expect_equal(.bpr_hand(fit), ref, tolerance = 1e-12)
    fr <- as_regression_frame(fit)
    expect_equal(fr$info$fit_stats$pseudo_r2_ferrari, ref, tolerance = 1e-12)
  }
})

test_that("the row is labelled after its authors and is not a default row", {
  skip_if_not_installed("betareg")
  fit <- .bpr_fit()
  lab <- "R\u00B2 (Ferrari & Cribari-Neto)"
  out_def <- paste(
    capture.output(print(table_regression(fit))),
    collapse = "\n"
  )
  expect_no_match(out_def, "Ferrari", fixed = TRUE)
  tb <- table_regression(fit, show_fit_stats = c("nobs", "pseudo_r2_ferrari"))
  out <- paste(capture.output(print(tb)), collapse = "\n")
  expect_match(out, lab, fixed = TRUE)
  expect_match(
    out,
    sprintf("%.2f", summary(fit)$pseudo.r.squared),
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  out_fr <- paste(
    capture.output(print(table_regression(
      fit,
      show_fit_stats = c("nobs", "pseudo_r2_ferrari")
    ))),
    collapse = "\n"
  )
  expect_match(out_fr, lab, fixed = TRUE)
})

test_that("the token is refused when no model is a betareg", {
  skip_if_not_installed("betareg")
  m_lm <- lm(mpg ~ wt, data = mtcars)
  m_glm <- glm(am ~ wt, data = mtcars, family = binomial)
  for (m in list(m_lm, m_glm)) {
    err <- expect_error(
      table_regression(m, show_fit_stats = c("nobs", "pseudo_r2_ferrari")),
      class = "spicy_invalid_input"
    )
    expect_match(conditionMessage(err), "betareg", fixed = TRUE)
  }
  # Beside a betareg the other columns take an empty cell.
  out <- capture.output(print(table_regression(
    list(.bpr_fit(), lm(mpg ~ wt, data = mtcars)),
    show_fit_stats = c("nobs", "pseudo_r2_ferrari")
  )))
  row <- grep("Ferrari", out, value = TRUE)
  expect_length(row, 1L)
  expect_match(row, "\u2013", fixed = TRUE)
})

test_that("a glmmTMB beta fit refuses the token", {
  skip_if_not_installed("glmmTMB")
  e <- new.env()
  utils::data("GasolineYield", package = "betareg", envir = e)
  m <- glmmTMB::glmmTMB(
    yield ~ temp,
    family = glmmTMB::beta_family(),
    data = e$GasolineYield
  )
  expect_error(
    table_regression(m, show_fit_stats = c("nobs", "pseudo_r2_ferrari")),
    class = "spicy_invalid_input"
  )
})
