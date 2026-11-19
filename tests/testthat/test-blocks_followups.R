# Follow-ups to the three blocks of 2026-11-19: the smooth terms of a GAM,
# the precision of a betareg and the error terms of a selection model
# (dev/decisions/2026-11-19-gam-smooth-terms-block.md,
# 2026-11-19-betareg-precision-block.md, 2026-11-19-selection-error-terms.md).

fu_pima <- function() {
  rbind(MASS::Pima.tr, MASS::Pima.te)
}

fu_gam <- function() {
  mgcv::gam(
    type ~ s(age) + bmi + glu + npreg,
    family = stats::binomial,
    data = fu_pima(),
    method = "REML"
  )
}

fu_mroz <- function() {
  e <- new.env()
  utils::data("Mroz87", package = "sampleSelection", envir = e)
  d <- e$Mroz87
  d$kids <- d$kids5 + d$kids618 > 0
  d
}

fu_selection <- function(two_step = FALSE) {
  f <- if (two_step) sampleSelection::heckit else sampleSelection::selection
  f(
    lfp ~ age + I(age^2) + faminc + kids + educ,
    wage ~ exper + I(exper^2) + educ + city,
    data = fu_mroz()
  )
}

fu_terms <- function(tb) {
  as.data.frame(tb)$term
}

# ---- 1. A pattern of `keep` / `drop` equal to a term name ---------------

test_that("keep and drop select a term by its exact name", {
  set.seed(1)
  d <- data.frame(
    bili = stats::rexp(60),
    age = stats::runif(60, 20, 80),
    cyl = factor(sample(c(4, 6, 8), 60, TRUE))
  )
  d$y <- stats::rnorm(60)
  fit <- lm(y ~ log(bili) + I(age^2) + factor(cyl), data = d)
  long <- function(...) {
    fu_terms(table_regression(fit, output = "long", ...))
  }
  expect_no_warning(kept <- long(keep = "log(bili)"))
  expect_setequal(setdiff(kept, "(Intercept)"), "log(bili)")
  expect_no_warning(kept <- long(keep = "I(age^2)"))
  expect_setequal(setdiff(kept, "(Intercept)"), "I(age^2)")
  expect_no_warning(kept <- long(keep = "factor(cyl)8"))
  expect_setequal(setdiff(kept, "(Intercept)"), "factor(cyl)8")
  expect_no_warning(dropped <- long(drop = "log(bili)"))
  expect_false("log(bili)" %in% dropped)
  expect_true("I(age^2)" %in% dropped)
  # An ordinary regular expression is unchanged.
  expect_setequal(
    setdiff(long(keep = "^factor"), "(Intercept)"),
    c("factor(cyl)4", "factor(cyl)6", "factor(cyl)8")
  )
  # A pattern that is a name and a regular expression finding other terms
  # keeps the union.
  fit2 <- lm(mpg ~ wt + I(wt^2), data = mtcars)
  expect_setequal(
    setdiff(
      fu_terms(table_regression(fit2, keep = "wt", output = "long")),
      "(Intercept)"
    ),
    c("wt", "I(wt^2)")
  )
})

test_that("keep selects a smooth term by its mgcv label", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("MASS")
  tb <- table_regression(fu_gam(), keep = "s(age)", output = "long")
  expect_setequal(setdiff(fu_terms(tb), "(Intercept)"), "s(age)")
})

test_that("a term name that is no regular expression selects its term", {
  d <- data.frame(y = stats::rnorm(20), x = stats::rnorm(20))
  names(d)[2] <- "a("
  fit <- lm(y ~ `a(`, data = d)
  nm <- names(stats::coef(fit))[2]
  expect_false(spicy:::.is_valid_perl_regex(nm))
  expect_no_warning(tb <- table_regression(fit, keep = nm, output = "long"))
  expect_true(nm %in% fu_terms(tb))
  expect_false(
    nm %in% fu_terms(table_regression(fit, drop = nm, output = "long"))
  )
  # Neither a name nor a regular expression: refused, in the name of the
  # function called.
  err <- expect_error(
    table_regression(fit, keep = "("),
    "term names or regular expressions",
    class = "spicy_invalid_input"
  )
  expect_identical(rlang::call_name(conditionCall(err)), "table_regression")
})

# ---- 2. `labels` renames the rows of a block -----------------------------

# The row names of the console table.
fu_rows <- function(tb) {
  out <- utils::capture.output(print(tb))
  trimws(sub("│.*$", "", out))
}

# The "The terms of this table: ..." sentence of a warning.
fu_terms_sentence <- function(w) {
  sub(".*(The terms of this table: .*)$", "\\1", conditionMessage(w))
}

test_that("labels renames a smooth term, and its warning lists keep's terms", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("MASS")
  fit <- fu_gam()
  tb <- expect_no_warning(table_regression(
    fit,
    labels = c("s(age)" = "Age (smooth)")
  ))
  expect_true("Age (smooth)" %in% fu_rows(tb))
  expect_false("s(age)" %in% fu_rows(tb))
  # One list of terms for the two warnings.
  w_lab <- expect_warning(
    table_regression(fit, labels = c(nope = "x")),
    class = "spicy_unknown_name"
  )
  w_keep <- expect_warning(
    table_regression(fit, keep = "nope"),
    class = "spicy_no_match"
  )
  expect_identical(fu_terms_sentence(w_lab), fu_terms_sentence(w_keep))
  expect_match(conditionMessage(w_lab), "\"s(age)\"", fixed = TRUE)
  expect_no_match(conditionMessage(w_lab), "s(age).1", fixed = TRUE)
})

test_that("labels renames the Precision and Error terms rows", {
  skip_if_not_installed("betareg")
  skip_if_not_installed("sampleSelection")
  e <- new.env()
  utils::data("FoodExpenditure", package = "betareg", envir = e)
  fe <- e$FoodExpenditure
  fe$share <- fe$food / fe$income
  beta <- betareg::betareg(share ~ income + persons | persons, data = fe)
  tb <- expect_no_warning(table_regression(
    beta,
    labels = c("Precision::persons" = "Household size", persons = "Persons")
  ))
  expect_true(all(c("Household size", "Persons") %in% fu_rows(tb)))
  for (two_step in c(FALSE, TRUE)) {
    fit <- fu_selection(two_step)
    lab <- c(rho = "Rho", sigma = "Sigma", invMillsRatio = "Lambda")
    if (!two_step) {
      lab <- lab[1:2]
    }
    tb <- expect_no_warning(table_regression(
      fit,
      labels = lab
    ))
    expect_true(all(unname(lab) %in% fu_rows(tb)))
  }
})
