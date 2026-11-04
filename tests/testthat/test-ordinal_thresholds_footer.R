# ---------------------------------------------------------------------------
# Phase 7c3 tests: ordinal-thresholds footer block for polr / clm.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_polr_th <- function() {
  skip_if_not_installed("MASS")
  data(housing, package = "MASS", envir = environment())
  MASS::polr(
    Sat ~ Infl + Type + Cont,
    weights = Freq,
    data = housing,
    Hess = TRUE
  )
}

.fit_clm_th <- function() {
  skip_if_not_installed("ordinal")
  ordinal::clm(rating ~ temp + contact, data = ordinal::wine)
}


# ---- 1. polr: thresholds line --------------------------------------------

test_that("ordinal thresholds footer fires for polr fits", {
  fit <- .fit_polr_th()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_ordinal_thresholds_footer_block_from_frames(list(fr))
  expect_match(out, "^Thresholds:")
  expect_match(out, "Low|Medium", fixed = TRUE)
  expect_match(out, "Medium|High", fixed = TRUE)
})

test_that("ordinal thresholds footer is silent for non-ordinal classes (lm)", {
  fr <- as_regression_frame(lm(mpg ~ wt, data = mtcars), model_id = "M1")
  out <- spicy:::build_ordinal_thresholds_footer_block_from_frames(list(fr))
  expect_null(out)
})


# ---- 2. clm: 4 thresholds inline ----------------------------------------

test_that("ordinal thresholds footer fires for clm fits", {
  fit <- .fit_clm_th()
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_ordinal_thresholds_footer_block_from_frames(list(fr))
  expect_match(out, "^Thresholds:")
  expect_match(out, "1|2 =", fixed = TRUE)
  expect_match(out, "4|5 =", fixed = TRUE)
})


# ---- 3. End-to-end through table_regression() ---------------------------

test_that("table_regression() footer carries Thresholds for polr", {
  fit <- .fit_polr_th()
  combined <- paste(
    capture.output(print(table_regression(fit))),
    collapse = "\n"
  )
  expect_match(combined, "Thresholds:", fixed = TRUE)
})

test_that("table_regression() footer does NOT carry Thresholds for lm", {
  fit <- lm(mpg ~ wt, data = mtcars)
  combined <- paste(
    capture.output(print(table_regression(fit))),
    collapse = "\n"
  )
  expect_false(grepl("Thresholds:", combined, fixed = TRUE))
})


# ---- 4. Empty / multi-model -------------------------------------------

test_that("ordinal thresholds footer is NULL on an empty frames list", {
  expect_null(spicy:::build_ordinal_thresholds_footer_block_from_frames(list()))
})

test_that("ordinal thresholds footer prefixes Model k for 2+ contributing fits", {
  fr1 <- as_regression_frame(.fit_polr_th(), model_id = "M1")
  fr2 <- as_regression_frame(.fit_clm_th(), model_id = "M2")
  out <- spicy:::build_ordinal_thresholds_footer_block_from_frames(list(
    fr1,
    fr2
  ))
  expect_match(out, "Model 1:", fixed = TRUE)
  expect_match(out, "Model 2:", fixed = TRUE)
})

test_that("ordinal thresholds footer skips non-ordinal in mixed lists (no prefix)", {
  fr_polr <- as_regression_frame(.fit_polr_th(), model_id = "M1")
  fr_lm <- as_regression_frame(lm(mpg ~ wt, data = mtcars), model_id = "M2")
  out <- spicy:::build_ordinal_thresholds_footer_block_from_frames(
    list(fr_polr, fr_lm)
  )
  expect_match(out, "^Thresholds:")
  expect_false(grepl("Model 1:", out, fixed = TRUE))
})


# ---- 5. Decimal comma: a separator no number can contain ----------------

test_that("under a decimal comma the listed cut-points are separated by semicolons", {
  skip_if_not_installed("rms")
  fit_p <- .fit_polr_th()
  set.seed(7)
  d <- data.frame(x1 = stats::rnorm(300))
  d$yc <- cut(
    0.8 * d$x1 + stats::rlogis(300),
    c(-Inf, -1, 0.3, 1.5, Inf),
    labels = c("Poor", "Fair", "Good", "Very good"),
    ordered_result = TRUE
  )
  fit_l <- rms::lrm(yc ~ x1, data = d)
  comma <- function(x) sub(".", ",", sprintf("%.2f", x), fixed = TRUE)
  listed <- function(terms, x, sep) {
    paste0(terms, " = ", comma(x), collapse = sep)
  }
  zeta_l <- -unname(stats::coef(fit_l)[seq_len(fit_l$non.slopes)])
  terms_l <- c("Poor|Fair", "Fair|Good", "Good|Very good")
  expect_match(
    attr(
      table_regression(fit_p, show_thresholds = FALSE, decimal_mark = ","),
      "note"
    ),
    paste0("Thresholds: ", listed(names(fit_p$zeta), fit_p$zeta, "; "), "."),
    fixed = TRUE
  )
  expect_match(
    attr(
      table_regression(fit_l, show_thresholds = FALSE, decimal_mark = ","),
      "note"
    ),
    paste0("Thresholds: ", listed(terms_l, zeta_l, "; "), "."),
    fixed = TRUE
  )
  # French typography: a no-break space before the semicolon and the colon.
  nbsp <- intToUtf8(160L)
  withr::local_options(spicy.language = "fr")
  expect_match(
    attr(
      table_regression(fit_l, show_thresholds = FALSE, decimal_mark = ","),
      "note"
    ),
    paste0(
      "Seuils",
      nbsp,
      ": ",
      listed(terms_l, zeta_l, paste0(nbsp, "; ")),
      "."
    ),
    fixed = TRUE
  )
})

test_that("a cut-point label holding a comma takes the semicolon under a point", {
  th <- data.frame(
    term = c("[0,1]|(1,2]", "(1,2]|(2,3]"),
    estimate = c(-1.7234, 0.5012),
    stringsAsFactors = FALSE
  )
  fr <- list(info = list(class = "polr", extras = list(thresholds = th)))
  expect_identical(
    spicy:::build_ordinal_thresholds_footer_block_from_frames(list(fr)),
    "Thresholds: [0,1]|(1,2] = -1.72; (1,2]|(2,3] = 0.50."
  )
})
