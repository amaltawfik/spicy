# ---------------------------------------------------------------------------
# Small fixes to the random-effects block of mixed models (2026-11-19):
#   1. the notes on the SE and CI of the random-effect rows appear only
#      when the table shows an SE or CI cell on those rows;
#   2. the correlation row of an lme fit under lmeControl(natural = FALSE)
#      and of a pdCompSymm block;
#   3. the `lme` part of a mgcv::gamm() fit is refused;
#   4. the correlation row of a pdSymm block inside a pdBlocked one.
# The "not positive definite" state is built as in test-lme_apvar_not_pd.R:
# a healthy fit whose apVar is replaced by nlme's own string.
# ---------------------------------------------------------------------------

.msf_printed <- function(x) {
  gsub("\\s+", " ", paste(utils::capture.output(print(x)), collapse = " "))
}

.msf_fit_not_pd <- function() {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    distance ~ age,
    data = nlme::Orthodont,
    random = ~ age | Subject
  )
  fit$apVar <- "Non-positive definite approximate variance-covariance"
  fit
}

.msf_rho_rows <- function(fit) {
  vc <- as_regression_frame(
    fit,
    model_id = "M1"
  )$info$random_effects$variance_components
  vc[vc$is_correlation %in% TRUE, , drop = FALSE]
}

.msf_rho <- intToUtf8(0x3C1)


# ---- 1. Notes only when an SE or CI cell is shown -------------------------

test_that("the apVar note appears only with an SE or CI cell on the rows", {
  fit <- .msf_fit_not_pd()
  key <- "not positive definite"
  has_note <- function(...) {
    grepl(key, .msf_printed(table_regression(fit, ...)), fixed = TRUE)
  }
  expect_true(has_note())
  expect_true(has_note(re_columns = c("est", "se")))
  expect_true(has_note(re_columns = c("est", "ci")))
  expect_false(has_note(show_re = FALSE))
  expect_false(has_note(re_columns = "est"))
  expect_false(has_note(show_columns = c("b", "p")))
  # An SE kept for the rows but no SE column, and the reverse.
  expect_false(has_note(
    show_columns = c("b", "ci", "p"),
    re_columns = c("est", "se")
  ))
  expect_true(has_note(
    show_columns = c("b", "se", "p"),
    re_columns = c("est", "se")
  ))
})

test_that("the size note appears only with an SE or CI cell on the rows", {
  skip_if_not_installed("lme4")
  withr::local_options(spicy.re_se_max_n = 10)
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  key <- "not computed for n above"
  has_note <- function(...) {
    grepl(
      key,
      .msf_printed(suppressWarnings(table_regression(fit, ...))),
      fixed = TRUE
    )
  }
  expect_true(has_note())
  expect_false(has_note(show_re = FALSE))
  expect_false(has_note(re_columns = "est"))
})

test_that("the profile note appears only with an SE or CI cell on the rows", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  key <- "profile likelihood"
  has_note <- function(...) {
    grepl(
      key,
      .msf_printed(suppressWarnings(table_regression(
        fit,
        re_ci = "profile",
        ...
      ))),
      fixed = TRUE
    )
  }
  expect_true(has_note())
  expect_false(has_note(re_columns = "est"))
})

test_that("the singular note drops its SE and CI clause when no cell shows", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    distance ~ age,
    data = nlme::Orthodont,
    random = list(Subject = nlme::pdCompSymm(~age))
  )
  expect_true(spicy:::.lme_is_singular(fit))
  clause <- "the Wald SE and CI of the variance components are omitted"
  note <- function(...) {
    .msf_printed(suppressWarnings(table_regression(fit, ...)))
  }
  expect_match(note(), clause, fixed = TRUE)
  for (out in list(note(re_columns = "est"), note(show_re = FALSE))) {
    expect_match(out, "Singular fit:", fixed = TRUE)
    expect_false(grepl(clause, out, fixed = TRUE))
  }
})
