# The table of a sample-selection model (sampleSelection::selection(),
# heckit(), treatReg()): its error terms, its estimation method, its
# reference distribution, and the fits it refuses.
# dev/decisions/2026-11-19-selection-error-terms.md.

.mroz <- function() {
  skip_if_not_installed("sampleSelection")
  e <- new.env()
  utils::data("Mroz87", package = "sampleSelection", envir = e)
  d <- e$Mroz87
  d$kids <- d$kids5 + d$kids618 > 0
  d
}

.sel_ml <- function(d = .mroz()) {
  sampleSelection::selection(
    lfp ~ age + I(age^2) + faminc + kids + educ,
    wage ~ exper + I(exper^2) + educ + city,
    data = d
  )
}

.sel_2s <- function(d = .mroz()) {
  sampleSelection::heckit(
    lfp ~ age + I(age^2) + faminc + kids + educ,
    wage ~ exper + I(exper^2) + educ + city,
    data = d
  )
}

test_that("a tobit-5 fit is refused by name", {
  skip_if_not_installed("sampleSelection")
  skip_if_not_installed("MASS")
  set.seed(1)
  n <- 300
  d <- data.frame(z = rnorm(n), x1 = rnorm(n), x2 = rnorm(n))
  e <- MASS::mvrnorm(
    n,
    c(0, 0, 0),
    matrix(c(1, -0.5, 0.5, -0.5, 1, 0, 0.5, 0, 1), 3)
  )
  d$ys <- d$z + e[, 1] > 0
  d$y1 <- d$x1 + e[, 2]
  d$y2 <- d$x2 + e[, 3]
  t5 <- sampleSelection::selection(ys ~ z, list(y1 ~ x1, y2 ~ x2), data = d)
  err <- expect_error(table_regression(t5), class = "spicy_unsupported_class")
  expect_match(conditionMessage(err), "A tobit-5 `selection` fit", fixed = TRUE)
  expect_error(as_regression_frame(t5), class = "spicy_unsupported_class")
})
