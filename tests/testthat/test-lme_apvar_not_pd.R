# ---------------------------------------------------------------------------
# lme fits whose approximate covariance matrix of the variance parameters
# (fit$apVar) is not a matrix. nlme stores a character string there
# instead (nlme:::lmeApVar, lme.formula):
#   * "Non-positive definite approximate variance-covariance" when the
#     finite-difference Hessian is not negative definite;
#   * "Approximate variance-covariance matrix not available" under
#     lmeControl(apVar = FALSE).
# nlme::intervals() refuses both. The correlation row (rho) used to take
# its estimate from intervals() and vanished with it; the estimate now
# comes from the fit, and a note says why the SE and CI are missing.
#
# Whether apVar is positive definite depends on the platform's floating
# point (finite differences), so the state is built deterministically:
# a healthy fit whose apVar is replaced by nlme's own string. No number
# read from apVar is pinned here.
# ---------------------------------------------------------------------------

.apvar_not_pd_msg <- "Non-positive definite approximate variance-covariance"

.fit_lme_corr_healthy <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(distance ~ age, data = nlme::Orthodont, random = ~ age | Subject)
}

.fit_lme_corr_not_pd <- function() {
  fit <- .fit_lme_corr_healthy()
  fit$apVar <- .apvar_not_pd_msg
  fit
}

.lme_fit_cor <- function(fit) {
  stats::cov2cor(unclass(nlme::getVarCov(fit)))[1L, 2L]
}

.printed <- function(x) {
  gsub("\\s+", " ", paste(utils::capture.output(print(x)), collapse = " "))
}

.note_en <- paste0(
  "SE and CI of the random-effect variance components unavailable: the ",
  "approximate covariance matrix of the variance parameters is not ",
  "positive definite."
)


test_that("a non-positive-definite apVar keeps the rho row, from the fit", {
  fit <- .fit_lme_corr_not_pd()
  expect_error(nlme::intervals(fit, which = "var-cov"), "Non-positive")

  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  rho <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(nrow(rho), 1L)
  expect_identical(rho$group, "Subject")
  expect_identical(rho$term, "(Intercept), age")
  # Oracle: the correlation of the fitted random-effect covariance matrix,
  # and the rounded one nlme::VarCorr() prints.
  expect_equal(rho$corr, .lme_fit_cor(fit), tolerance = 1e-10)
  vc_raw <- unclass(nlme::VarCorr(fit))
  expect_equal(
    rho$corr,
    as.numeric(vc_raw["age", "Corr"]),
    tolerance = 1e-3
  )
  # Every row of the block is without SE and CI.
  expect_true(all(is.na(vc$std_error)))
  expect_true(all(is.na(vc$ci_lower)))
  expect_true(all(is.na(vc$ci_upper)))
  expect_true(fr$info$extras$re_apvar_not_pd)
  expect_false(fr$info$extras$has_singular)
})

test_that("the rho estimate is the one intervals() gives when it answers", {
  fit <- .fit_lme_corr_healthy()
  vc <- as_regression_frame(
    fit,
    model_id = "M1"
  )$info$random_effects$variance_components
  rho <- vc[vc$is_correlation %in% TRUE, ]
  ci <- nlme::intervals(fit, which = "var-cov")$reStruct$Subject
  expect_equal(rho$corr, ci["cor((Intercept),age)", "est."], tolerance = 1e-10)
  expect_true(is.finite(rho$std_error))
  expect_false(
    as_regression_frame(fit, model_id = "M1")$info$extras$re_apvar_not_pd
  )
})

test_that("the table shows rho, dashes and the note", {
  fit <- .fit_lme_corr_not_pd()
  out <- .printed(table_regression(fit))
  expect_match(out, "\u03C1 Subject ((Intercept), age)", fixed = TRUE)
  expect_match(out, .note_en, fixed = TRUE)

  healthy <- .printed(table_regression(.fit_lme_corr_healthy()))
  expect_false(grepl("not positive definite", healthy, fixed = TRUE))
})

test_that("apVar = FALSE keeps rho without SE and gets no note", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    distance ~ age,
    data = nlme::Orthodont,
    random = ~ age | Subject,
    control = nlme::lmeControl(apVar = FALSE)
  )
  expect_identical(
    fit$apVar,
    "Approximate variance-covariance matrix not available"
  )
  fr <- as_regression_frame(fit, model_id = "M1")
  vc <- fr$info$random_effects$variance_components
  rho <- vc[vc$is_correlation %in% TRUE, ]
  expect_equal(rho$corr, .lme_fit_cor(fit), tolerance = 1e-10)
  expect_true(all(is.na(vc$std_error)))
  expect_false(fr$info$extras$re_apvar_not_pd)
  expect_false(grepl(
    "not positive definite",
    .printed(table_regression(fit)),
    fixed = TRUE
  ))
})

test_that("a boundary fit keeps its singular note, not this one", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    distance ~ age,
    data = nlme::Orthodont,
    random = list(Subject = nlme::pdCompSymm(~age))
  )
  expect_true(spicy:::.lme_is_singular(fit))
  fit$apVar <- .apvar_not_pd_msg
  expect_false(spicy:::.lme_apvar_not_pd(fit))
  expect_false(
    as_regression_frame(fit, model_id = "M1")$info$extras$re_apvar_not_pd
  )
})

test_that("the note is prefixed with the model in a multi-model table", {
  bad <- .fit_lme_corr_not_pd()
  good <- .fit_lme_corr_healthy()
  fr_bad <- as_regression_frame(bad, model_id = "M1")
  fr_good <- as_regression_frame(good, model_id = "M2")
  expect_identical(
    spicy:::build_re_apvar_footer_block_from_frames(list(fr_bad)),
    .note_en
  )
  expect_identical(
    spicy:::build_re_apvar_footer_block_from_frames(list(fr_bad, fr_bad)),
    .note_en
  )
  expect_null(spicy:::build_re_apvar_footer_block_from_frames(list(fr_good)))
  expect_null(spicy:::build_re_apvar_footer_block_from_frames(list()))
  out <- .printed(table_regression(list(A = bad, B = good)))
  expect_match(out, paste0("A: ", .note_en), fixed = TRUE)
  expect_false(grepl(paste0("B: ", .note_en), out, fixed = TRUE))
})

test_that("the note follows the language", {
  withr::local_options(spicy.language = "fr")
  fr <- as_regression_frame(.fit_lme_corr_not_pd(), model_id = "M1")
  expect_identical(
    spicy:::build_re_apvar_footer_block_from_frames(list(fr)),
    paste0(
      "Erreur standard et IC des composantes de la variance des effets ",
      "al\u00E9atoires indisponibles\u00A0: la matrice de covariance ",
      "approch\u00E9e des param\u00E8tres de variance n'est pas ",
      "d\u00E9finie positive."
    )
  )
})

test_that("the model of the report, where the platform gives no matrix", {
  skip_if_not_installed("nlme")
  d <- subset(nlme::Orthodont, Sex == "Male")
  fit <- nlme::lme(
    distance ~ I(age - 11),
    data = d,
    random = ~ I(age - 11) | Subject
  )
  skip_if(is.matrix(fit$apVar), "apVar is positive definite on this platform")
  vc <- as_regression_frame(
    fit,
    model_id = "M1"
  )$info$random_effects$variance_components
  rho <- vc[vc$is_correlation %in% TRUE, ]
  expect_identical(nrow(rho), 1L)
  expect_equal(rho$corr, .lme_fit_cor(fit), tolerance = 1e-10)
  expect_match(
    .printed(table_regression(fit)),
    .note_en,
    fixed = TRUE
  )
})
