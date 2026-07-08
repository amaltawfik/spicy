# ---------------------------------------------------------------------------
# mlogit two-segment layout (dev/mlogit_two_segment_spec.md, option B
# validated 2026-07-04): generic (alternative-invariant) coefficients
# as a leading section, then one labelled section per non-reference
# alternative with bare row labels -- the Stata asclogit presentation.
# Terms keep mlogit's native "<term>:<alt>" names, so tidy() /
# output = "long" / keep / drop are untouched by the regrouping.
# ---------------------------------------------------------------------------


.fit_mlogit_fishing_2s <- function(formula = mode ~ price + catch | income_k,
                                   reflevel = NULL) {
  skip_if_not_installed("mlogit")
  data("Fishing", package = "mlogit", envir = environment())
  Fishing$income_k <- Fishing$income / 1000
  Fish <- mlogit::dfidx(Fishing, varying = 2:9, choice = "mode",
                        shape = "wide")
  if (is.null(reflevel)) {
    mlogit::mlogit(formula, data = Fish)
  } else {
    mlogit::mlogit(formula, data = Fish, reflevel = reflevel)
  }
}


test_that("two segments: generic block first, then per-alternative sections", {
  fit <- .fit_mlogit_fishing_2s()
  df <- as.data.frame(table_regression(fit))
  v <- trimws(df$Variable)
  # Section headers present, in order: generic block leads.
  idx <- vapply(c("Alternative-invariant:", "boat:", "charter:", "pier:"),
                function(h) which(v == h)[1L], integer(1))
  expect_true(all(is.finite(idx)))
  expect_true(all(diff(idx) > 0))
  # Bare row labels inside sections; generic rows sit under the
  # leading block.
  expect_lt(idx["Alternative-invariant:"], which(v == "price"))
  expect_identical(sum(v == "(Intercept)"), 3L)
  expect_identical(sum(v == "income_k"), 3L)
  # No predictor-grouped prefixed rows remain.
  expect_false(any(grepl("boat: ", df$Variable, fixed = TRUE)))
})


test_that("cells match the summary(fit) oracle after regrouping", {
  fit <- .fit_mlogit_fishing_2s()
  lg <- table_regression(fit, output = "long")
  sm <- summary(fit)$CoefTable
  # Native term names key both sides; every row pinned.
  expect_setequal(lg$term, rownames(sm))
  m <- match(lg$term, rownames(sm))
  expect_equal(lg$estimate, unname(sm[m, "Estimate"]), tolerance = 1e-10)
  expect_equal(lg$std.error, unname(sm[m, "Std. Error"]),
               tolerance = 1e-10)
  expect_equal(lg$statistic, unname(sm[m, "z-value"]), tolerance = 1e-10)
  # Long payload order mirrors the display: generic rows first.
  expect_identical(lg$term[1:2], c("price", "catch"))
})


test_that("'Reference alternative:' footer note names the base", {
  fit <- .fit_mlogit_fishing_2s()
  out <- paste(capture.output(print(table_regression(fit))),
               collapse = "\n")
  expect_match(out, "Reference alternative: beach.", fixed = TRUE)
  # reflevel changes the base; the note follows.
  fit_b <- .fit_mlogit_fishing_2s(reflevel = "boat")
  out_b <- paste(capture.output(print(table_regression(fit_b))),
                 collapse = "\n")
  expect_match(out_b, "Reference alternative: boat.", fixed = TRUE)
})


test_that("generic-only model: single block, no reference note", {
  fit <- .fit_mlogit_fishing_2s(mode ~ price + catch | 0)
  df <- as.data.frame(table_regression(fit))
  v <- trimws(df$Variable)
  expect_true("Alternative-invariant:" %in% v)
  expect_true(all(c("price", "catch") %in% v))
  out <- paste(capture.output(print(table_regression(fit))),
               collapse = "\n")
  # No alternative-specific rows -> the base cannot be identified from
  # the coefficients; the note is omitted, never guessed.
  expect_false(grepl("Reference alternative", out, fixed = TRUE))
})


test_that("keep/drop operate on the native '<term>:<alt>' names", {
  fit <- .fit_mlogit_fishing_2s()
  lg <- table_regression(fit, output = "long", keep = "^income_k:")
  expect_identical(sort(lg$term),
                   sort(c("income_k:boat", "income_k:charter",
                          "income_k:pier")))
  df <- as.data.frame(table_regression(fit, keep = "^income_k:"))
  expect_identical(sum(trimws(df$Variable) == "income_k"), 3L)
})


test_that("multi-model mlogit keeps the sectioned layout side by side", {
  fit  <- .fit_mlogit_fishing_2s()
  fit0 <- .fit_mlogit_fishing_2s(mode ~ price | 0)
  out <- capture.output(print(table_regression(list(fit0, fit))))
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "Alternative-invariant:", fixed = TRUE)
  expect_match(txt, "charter:", fixed = TRUE)
  # Shared reference note, single line (both models base = beach).
  expect_identical(
    sum(grepl("Reference alternative: beach.", out, fixed = TRUE)), 1L
  )
})


test_that("console snapshot of the two-segment layout", {
  fit <- .fit_mlogit_fishing_2s()
  expect_snapshot(print(table_regression(fit)))
})
