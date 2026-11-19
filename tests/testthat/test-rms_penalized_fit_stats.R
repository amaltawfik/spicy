# Fit statistics of an rms fit estimated by penalized maximum likelihood.
# rms's logLik() of a penalized lrm is the penalized log-likelihood, so
# its AIC charges the penalty twice and ranks the fits against the
# criterion pentrace() chooses the penalty on (Model L.R. - 2 x effective
# d.f.). spicy reports the log-likelihood without the penalty term at the
# penalized estimates, with the effective d.f. plus the intercepts
# (dev/decisions/2026-11-19-rms-penalized-fit-statistics.md). The
# yardsticks below do not call logLik(): the binomial and ordinal
# likelihoods are recomputed from the fitted probabilities.

pima_rms <- function() rbind(MASS::Pima.tr, MASS::Pima.te)

rms_fs <- function(fit) as_regression_frame(fit, model_id = "M1")$info$fit_stats

test_that("a penalized binary lrm reports the likelihood without the penalty term", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  d <- pima_rms()
  f_mle <- rms::lrm(
    type ~ rms::rcs(age, 4) + bmi + glu + npreg + ped,
    data = d,
    x = TRUE,
    y = TRUE
  )
  f_pen <- stats::update(f_mle, penalty = 2)
  fs <- rms_fs(f_pen)

  p <- stats::plogis(f_pen$linear.predictors)
  y <- as.integer(f_pen$y) - 1L
  ll_hand <- sum(stats::dbinom(y, 1L, p, log = TRUE))
  edf <- unname(f_pen$stats["d.f."])
  lr <- unname(f_pen$stats["Model L.R."])
  k <- edf + 1

  expect_equal(fs$log_lik, ll_hand, tolerance = 1e-6)
  expect_equal(fs$log_lik, -227.9641, tolerance = 1e-6)
  expect_equal(fs$deviance, -2 * ll_hand, tolerance = 1e-6)
  expect_equal(fs$aic, f_pen$deviance[[1L]] - lr + 2 * k, tolerance = 1e-8)
  expect_equal(fs$bic, -2 * ll_hand + log(nrow(d)) * k, tolerance = 1e-6)
  expect_equal(
    fs$pseudo_r2_mcfadden,
    1 - (-2 * ll_hand) / f_pen$deviance[[1L]],
    tolerance = 1e-8
  )
  # Nagelkerke's R2 is rms's, already computed without the penalty.
  expect_identical(fs$pseudo_r2_nagelkerke, unname(f_pen$stats[["R2"]]))

  # The AICc row comes from the same likelihood and parameter count.
  tbl <- table_regression(
    f_pen,
    show_fit_stats = c("aic", "aicc"),
    output = "data.frame"
  )
  aicc <- tbl[tbl$variable == "aicc", ]
  expect_equal(
    aicc$B,
    fs$aic + 2 * k * (k + 1) / (nrow(d) - k - 1),
    tolerance = 1e-8
  )

  # The fit by maximum likelihood does not change.
  fs0 <- rms_fs(f_mle)
  expect_identical(fs0$log_lik, as.numeric(stats::logLik(f_mle)))
  expect_identical(fs0$aic, stats::AIC(f_mle))
  expect_identical(fs0$bic, stats::BIC(f_mle))
  expect_identical(fs0$deviance, f_mle$deviance[[2L]])
})

test_that("the AIC of penalized lrm fits ranks them as pentrace() does", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  d <- pima_rms()
  f_mle <- rms::lrm(
    type ~ rms::rcs(age, 4) + bmi + glu + npreg + ped,
    data = d,
    x = TRUE,
    y = TRUE
  )
  grid <- c(0, 1, 2, 4, 8)
  pt <- rms::pentrace(f_mle, grid)$results.all
  aic <- vapply(
    grid,
    function(p) rms_fs(stats::update(f_mle, penalty = p))$aic,
    numeric(1)
  )
  # pentrace()'s aic is LR - 2 x d.f., a larger value is better: the
  # order of spicy's AIC is its reverse, value for value.
  expect_identical(order(aic), order(-pt$aic))
  expect_equal(aic + pt$aic, rep(f_mle$deviance[[1L]] + 2, length(grid)))
})

test_that("a penalized ordinal lrm reports the likelihood without the penalty term", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  d <- pima_rms()
  d$gc <- cut(d$glu, c(0, 100, 130, 160, 300))
  f_pen <- rms::lrm(
    gc ~ rms::rcs(age, 4) + bmi + npreg + ped,
    data = d,
    x = TRUE,
    y = TRUE,
    penalty = 3
  )
  fs <- rms_fs(f_pen)
  P <- stats::predict(f_pen, type = "fitted.ind")
  ll_hand <- sum(log(P[cbind(seq_len(nrow(P)), as.integer(f_pen$y))]))
  k <- unname(f_pen$stats["d.f."]) + 3
  expect_equal(fs$log_lik, ll_hand, tolerance = 1e-6)
  expect_equal(fs$aic, -2 * ll_hand + 2 * k, tolerance = 1e-6)
  expect_equal(fs$bic, -2 * ll_hand + log(nrow(d)) * k, tolerance = 1e-6)
})

test_that("a penalized ols already reports the Gaussian likelihood without the penalty", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  d <- pima_rms()
  f_pen <- rms::ols(
    glu ~ rms::rcs(age, 4) + bmi + npreg,
    data = d,
    x = TRUE,
    y = TRUE,
    penalty = 2
  )
  fs <- rms_fs(f_pen)
  r <- f_pen$residuals
  n <- length(r)
  ll_hand <- sum(stats::dnorm(r, 0, sqrt(sum(r^2) / n), log = TRUE))
  # The effective d.f., the intercept and sigma.
  k <- unname(f_pen$stats["d.f."]) + 2
  expect_equal(fs$log_lik, ll_hand, tolerance = 1e-8)
  # rms names the AIC and BIC of an ols after its "d.f." entry.
  expect_equal(
    fs$aic,
    -2 * ll_hand + 2 * k,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
  expect_equal(
    fs$bic,
    -2 * ll_hand + log(n) * k,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
})

test_that("the note says which likelihood a penalized fit's rows use", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  d <- pima_rms()
  f_mle <- rms::lrm(type ~ bmi + glu + npreg, data = d)
  f_pen <- rms::lrm(type ~ bmi + glu + npreg, data = d, penalty = 2)
  note <- function(x) paste(capture.output(print(x)), collapse = " ")
  sentence <- paste0(
    "Fit statistics of the penalized model: log-likelihood without the ",
    "penalty term, effective degrees of freedom."
  )
  expect_match(
    note(table_regression(f_pen, show_fit_stats = c("nobs", "log_lik"))),
    paste("Penalized estimates (penalty: 2).", sentence),
    fixed = TRUE
  )
  # No likelihood row, no sentence; no penalty, no sentence.
  expect_false(grepl(
    sentence,
    note(table_regression(f_pen, show_fit_stats = "nobs")),
    fixed = TRUE
  ))
  expect_false(grepl(
    sentence,
    note(table_regression(f_mle, show_fit_stats = c("nobs", "aic"))),
    fixed = TRUE
  ))
  # Beside a fit by maximum likelihood, the model is named.
  expect_match(
    note(table_regression(
      list(f_mle, f_pen),
      show_fit_stats = c("nobs", "bic")
    )),
    paste("Model 2: Penalized estimates (penalty: 2).", sentence),
    fixed = TRUE
  )
  # A table of fit statistics only keeps the sentence.
  fit_only <- note(table_regression(
    f_pen,
    show_columns = "none",
    show_fit_stats = c("nobs", "aic")
  ))
  expect_match(fit_only, sentence, fixed = TRUE)
})

test_that("the sentence has its French translation", {
  skip_if_not_installed("rms")
  skip_if_not_installed("MASS")
  withr::local_options(spicy.language = "fr")
  d <- pima_rms()
  f_pen <- rms::lrm(type ~ bmi + glu + npreg, data = d, penalty = 2)
  out <- paste(
    capture.output(print(table_regression(f_pen, show_fit_stats = "aic"))),
    collapse = " "
  )
  expect_match(
    out,
    paste0(
      "Statistiques d'ajustement du mod\u00E8le p\u00E9nalis\u00E9\u00A0: ",
      "log-vraisemblance sans le terme de p\u00E9nalit\u00E9, ",
      "degr\u00E9s de libert\u00E9 effectifs."
    ),
    fixed = TRUE
  )
})
