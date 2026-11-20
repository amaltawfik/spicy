# Coverage-targeted tests for branches of table_regression() that the CI
# coverage of 2026-11-20 reported without a test:
#   * the AME rows of an lm whose data rows were renamed after the fit
#   * the prediction check of a guarded class with no data to compare on
#   * the dependent variable of a formula with no left-hand side
#   * a beta regression precision factor coded without a reference level,
#     a precision link and a mean link outside the named ones
#   * an rms fit that carries no mmcolnames (an rms older than 5.0)
#
# Each block asserts a result; optional engines are gated by
# skip_if_not_installed().

# ---- AME --------------------------------------------------------------------

test_that("an lm whose data rows were renamed keeps the Satterthwaite AME", {
  # The CR2 Satterthwaite AME of an lm rebuilds its design from the
  # data of the call. Rows renamed after the fit hold the same values in
  # the same order (.ame_data_check()), so the design is rebuilt from
  # them by position, without a fallback.
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("marginaleffects")
  set.seed(1120)
  n <- 80L
  d_rows <- data.frame(
    y = rnorm(n),
    x = rnorm(n),
    z = rnorm(n),
    cl = rep(1:16, 5)
  )
  fit <- stats::lm(y ~ I(x^2) + z, data = d_rows)
  ref <- table_regression(
    fit,
    show_columns = c("b", "ame", "ame_p"),
    vcov = "CR2",
    cluster = ~cl,
    output = "data.frame"
  )
  rownames(d_rows) <- paste0("r", seq_len(n))
  expect_no_warning(
    tbl <- table_regression(
      fit,
      show_columns = c("b", "ame", "ame_p"),
      vcov = "CR2",
      cluster = ~cl,
      output = "data.frame"
    )
  )
  expect_identical(tbl, ref)
  expect_equal(
    tbl$AME[tbl$variable == "z"],
    unname(stats::coef(fit)["z"]),
    tolerance = 1e-6
  )
})

test_that("the prediction check of an rms fit lets an AME with no data fail on its own", {
  # With the data of the fit gone, marginaleffects cannot read them back:
  # the prediction check has nothing to compare and stands aside
  # (returns NULL), so the warning is the one of avg_slopes(), not a
  # refusal of the predictions.
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  set.seed(1121)
  d_gone_1120 <- data.frame(y = rnorm(60), x = rnorm(60))
  fit <- rms::ols(y ~ x, data = d_gone_1120)
  rm(d_gone_1120)
  expect_null(spicy:::.ame_prediction_check(fit, NULL))
  expect_warning(
    tbl <- table_regression(
      fit,
      show_columns = c("b", "ame"),
      output = "data.frame"
    ),
    "avg_slopes\\(\\)` failed",
    class = "spicy_fallback"
  )
  expect_true(all(is.na(tbl$AME[tbl$.row_role == "coef"])))
  expect_equal(
    tbl$B[tbl$variable == "x"],
    unname(stats::coef(fit)["x"]),
    tolerance = 1e-6
  )
})


# ---- Dependent variable -------------------------------------------------------

test_that("the dependent variable of a one-sided formula falls back to its first variable", {
  expect_identical(spicy:::.fit_dv(NULL, ~ x + z), "x")
  # A two-sided formula keeps its left-hand side as written.
  expect_identical(spicy:::.fit_dv(NULL, log(y) ~ x), "log(y)")
})


# ---- Beta regression ----------------------------------------------------------

test_that("a precision factor coded without a reference level gets no reference row", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg", envir = environment())
  fit <- betareg::betareg(yield ~ temp | batch - 1, data = GasolineYield)
  tbl <- table_regression(fit, output = "data.frame")
  prec <- tbl[tbl$variable == "Precision" & !is.na(tbl$level), ]
  # Every one of the ten batches has its own estimate.
  expect_identical(prec$level, paste0("batch: ", 1:10))
  expect_false(anyNA(prec$B))
})

test_that("a precision link outside log, identity and sqrt names its own scale", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg", envir = environment())
  fit <- betareg::betareg(
    yield ~ temp | temp,
    data = GasolineYield,
    link.phi = stats::make.link("inverse")
  )
  note <- attr(table_regression(fit, output = "data.frame"), "note")
  expect_match(note, "Precision component: inverse scale.", fixed = TRUE)
})

test_that("a mean link outside the named ones keeps the plain beta regression title", {
  skip_if_not_installed("betareg")
  data("GasolineYield", package = "betareg", envir = environment())
  fit <- betareg::betareg(
    yield ~ temp,
    data = GasolineYield,
    link = stats::make.link("identity")
  )
  expect_identical(fit$link$mean$name, "identity")
  note <- attr(table_regression(fit, output = "data.frame"), "note")
  expect_match(note, "^Note\\. Beta regression\\.")
})


# ---- rms ----------------------------------------------------------------------

test_that("an rms fit without mmcolnames keeps rms's name for a transformed term", {
  # rms records the formula's spelling of each column in
  # Design$mmcolnames since 5.0; a fit saved by an older rms has none,
  # and its log(w) row keeps the name rms gives it, w.
  skip_if_not_installed("rms")
  set.seed(1122)
  d <- data.frame(
    yb = rbinom(150, 1, 0.5),
    w = runif(150, 1, 5),
    x = rnorm(150)
  )
  fit <- rms::lrm(yb ~ log(w) + x, data = d)
  coef_rows <- function(f) {
    tbl <- table_regression(f, output = "data.frame")
    tbl$variable[tbl$.row_role == "coef"]
  }
  expect_identical(coef_rows(fit), c("(Intercept)", "log(w)", "x"))
  old <- fit
  old$Design$mmcolnames <- NULL
  expect_identical(spicy:::.rms_transformed_terms(old), character(0))
  expect_identical(coef_rows(old), c("(Intercept)", "w", "x"))
})
