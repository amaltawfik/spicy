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


# ---- 2. The rho row under natural = FALSE and with pdCompSymm ------------

test_that("lmeControl(natural = FALSE) keeps the rho row, without SE or CI", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    distance ~ age,
    data = nlme::Orthodont,
    random = ~ age | Subject,
    control = nlme::lmeControl(natural = FALSE)
  )
  # intervals() gives the covariance, not the correlation.
  ci <- nlme::intervals(fit, which = "var-cov")$reStruct$Subject
  expect_false(any(grepl("^cor\\(", rownames(ci))))

  rho <- .msf_rho_rows(fit)
  expect_identical(nrow(rho), 1L)
  expect_identical(rho$term, "(Intercept), age")
  expect_equal(
    rho$corr,
    stats::cov2cor(unclass(nlme::getVarCov(fit)))[1L, 2L],
    tolerance = 1e-10
  )
  expect_equal(
    rho$corr,
    as.numeric(unclass(nlme::VarCorr(fit))["age", "Corr"]),
    tolerance = 1e-3
  )
  expect_true(is.na(rho$std_error))
  expect_true(is.na(rho$ci_lower))
  expect_true(is.na(rho$ci_upper))
  expect_match(
    .msf_printed(table_regression(fit)),
    paste0(.msf_rho, " Subject ((Intercept), age)"),
    fixed = TRUE
  )
})

test_that("a pdCompSymm block shows one rho row per pair, with nlme's interval", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    yield ~ nitro,
    data = nlme::Oats,
    random = list(Block = nlme::pdCompSymm(~ Variety - 1))
  )
  expect_false(spicy:::.lme_is_singular(fit))
  rho <- .msf_rho_rows(fit)
  lv <- paste0("Variety", levels(nlme::Oats$Variety))
  expect_identical(
    rho$term,
    c(
      paste(lv[1:2], collapse = ", "),
      paste(lv[c(1, 3)], collapse = ", "),
      paste(lv[2:3], collapse = ", ")
    )
  )
  # Oracle for the estimate: the off-diagonal of the fitted matrix, the
  # value VarCorr() prints.
  r_fit <- stats::cov2cor(unclass(nlme::getVarCov(fit)))
  expect_equal(rho$corr, rep(r_fit[2L, 1L], 3L), tolerance = 1e-10)
  expect_equal(
    rho$corr[1L],
    as.numeric(unclass(nlme::VarCorr(fit))[2L, "Corr"]),
    tolerance = 1e-3
  )
  # The interval is nlme's "corr." row, the same for every pair.
  ci <- nlme::intervals(fit, which = "var-cov")$reStruct$Block
  expect_equal(rho$ci_lower, rep(ci["corr.", "lower"], 3L), tolerance = 1e-10)
  expect_equal(rho$ci_upper, rep(ci["corr.", "upper"], 3L), tolerance = 1e-10)
  # The SE: the delta method on nlme's scale for this correlation,
  # g(r) = log((r + 1 / (n - 1)) / (1 - r)), n = 3, with the SE on g
  # taken from the approximate covariance matrix nlme stores.
  n <- 3
  r <- r_fit[2L, 1L]
  se_g <- sqrt(diag(fit$apVar))[2L]
  expect_equal(
    rho$std_error,
    rep(unname(se_g / (1 / (r + 1 / (n - 1)) + 1 / (1 - r))), 3L),
    tolerance = 1e-6
  )
})

test_that("a pdCompSymm interval off nlme's scale leaves the cells empty", {
  expect_true(is.na(spicy:::.lme_cs_corr_se(0.5, 0.1, 0.6, 1.96, 3)))
  # Centred on g(0.5) = log(2), n = 3: SE = (1 / 1.96) / g'(0.5) = ... / 3.
  g_inv <- function(g) (exp(g) - 0.5) / (1 + exp(g))
  expect_equal(
    spicy:::.lme_cs_corr_se(0.5, g_inv(log(2) - 1), g_inv(log(2) + 1), 1.96, 3),
    (1 / 1.96) / 3,
    tolerance = 1e-12
  )
})


# ---- 3. The lme part of a gamm() fit --------------------------------------

test_that("the lme part of a gamm() fit is refused", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("nlme")
  set.seed(1)
  d <- data.frame(x = stats::runif(120), g = factor(rep(1:12, 10)))
  d$y <- sin(2 * pi * d$x) + stats::rnorm(12)[d$g] + stats::rnorm(120, sd = 0.3)
  smooth <- mgcv::gamm(y ~ s(x), random = list(g = ~1), data = d)
  no_smooth <- mgcv::gamm(y ~ x, random = list(g = ~1), data = d)
  for (m in list(smooth$lme, no_smooth$lme)) {
    expect_s3_class(m, "lme")
    expect_error(
      table_regression(m),
      "`lme` part of a `mgcv::gamm()` fit",
      fixed = TRUE,
      class = "spicy_unsupported_class"
    )
    expect_error(
      as_regression_frame(m, model_id = "M1"),
      class = "spicy_unsupported_class"
    )
  }
  # The supported route the message names.
  g2 <- mgcv::gam(y ~ s(x) + s(g, bs = "re"), data = d, method = "REML")
  expect_s3_class(table_regression(g2), "spicy_regression_table")
  # An lme fitted by the user is not caught.
  expect_false(spicy:::.is_gamm_lme(nlme::lme(
    y ~ x,
    random = ~ 1 | g,
    data = d
  )))
})


# ---- 4. A pdSymm block inside a pdBlocked one ----------------------------

test_that("a pdSymm block inside pdBlocked keeps its rho row, apVar sound or not", {
  skip_if_not_installed("nlme")
  set.seed(11)
  ng <- 40
  ni <- 12
  d <- data.frame(
    g = factor(rep(seq_len(ng), each = ni)),
    x = stats::rnorm(ng * ni),
    z = stats::rnorm(ng * ni)
  )
  b0 <- stats::rnorm(ng)
  b1 <- 0.5 * b0 + stats::rnorm(ng, sd = 0.6)
  bz <- stats::rnorm(ng, sd = 0.7)
  d$y <- 1 +
    b0[d$g] +
    (0.5 + b1[d$g]) * d$x +
    (0.3 + bz[d$g]) * d$z +
    stats::rnorm(nrow(d), sd = 0.8)
  fit <- nlme::lme(
    y ~ x + z,
    data = d,
    random = list(
      g = nlme::pdBlocked(list(nlme::pdSymm(~ 1 + x), nlme::pdIdent(~ z - 1)))
    )
  )
  expect_true(is.matrix(fit$apVar))
  # Oracle: the correlation of the fitted random-effect covariance matrix.
  r_fit <- stats::cov2cor(unclass(nlme::getVarCov(fit)))["x", "(Intercept)"]
  ci <- nlme::intervals(fit, which = "var-cov")$reStruct$g

  rho <- .msf_rho_rows(fit)
  expect_identical(nrow(rho), 1L)
  expect_identical(rho$term, "(Intercept), x")
  expect_equal(rho$corr, r_fit, tolerance = 1e-10)
  expect_equal(rho$corr, ci["cor((Intercept),x)", "est."], tolerance = 1e-10)
  expect_equal(
    rho$ci_lower,
    ci["cor((Intercept),x)", "lower"],
    tolerance = 1e-10
  )
  expect_true(is.finite(rho$std_error))

  fit$apVar <- "Non-positive definite approximate variance-covariance"
  rho_bad <- .msf_rho_rows(fit)
  expect_identical(nrow(rho_bad), 1L)
  expect_equal(rho_bad$corr, r_fit, tolerance = 1e-10)
  expect_true(is.na(rho_bad$std_error))
  expect_true(is.na(rho_bad$ci_lower))
})
