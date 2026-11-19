# Corrections of the robust / IV / quantile / panel family found while
# preparing the article on these tables (2026-11-19).

.rf_wage_data <- function(n = 200, seed = 20261119) {
  set.seed(seed)
  d <- data.frame(
    education = round(runif(n, 8, 18)),
    experience = runif(n, 0, 30),
    region = factor(sample(c("ne", "mw", "s", "w"), n, TRUE)),
    parttime = factor(sample(c("no", "yes"), n, TRUE, c(0.8, 0.2)))
  )
  d$feducation <- d$education + rnorm(n, 0, 2)
  d$meducation <- d$education + rnorm(n, 0, 2)
  d$wage <- exp(
    0.5 + 0.08 * d$education + 0.03 * d$experience + rnorm(n, 0, 0.4)
  )
  d
}


# ---- The dependent variable keeps its transformation ---------------------

test_that("the frame of every class keeps a transformed dependent variable", {
  d <- .class_fixture_data()
  lhs <- "(?<![\\w.])(y|yb|cnt|pos|ynl|cnb|yiv|cens|ord|mn|zcnt|prop|mode|ysel) ~"
  checked <- character(0)
  for (id in names(.CLASS_FIXTURES)) {
    fx <- .CLASS_FIXTURES[[id]]
    if (isTRUE(fx$stan)) {
      next
    }
    pkgs <- as.character(fx$pkgs)
    named <- nzchar(names(fx$pkgs) %||% rep("", length(pkgs)))
    pkgs[named] <- names(fx$pkgs)[named]
    if (!all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE))) {
      next
    }
    src <- paste(deparse(fx$fit), collapse = "\n")
    src2 <- gsub(lhs, "identity(\\1) ~", src, perl = TRUE)
    if (identical(src, src2)) {
      next # a Surv() outcome, already deparsed whole
    }
    fit <- suppressWarnings(suppressMessages(
      eval(parse(text = src2), envir = globalenv())(d)
    ))
    dv <- suppressWarnings(suppressMessages(
      as_regression_frame(fit, model_id = "M1")
    ))$info$dv
    expect_true(startsWith(dv, "identity("), info = paste(id, dv))
    checked <- c(checked, id)
  }
  expect_gt(length(checked), 40L)
})

test_that("the title names log(wage) for lm_robust, iv_robust, ivreg, rq and fixest", {
  skip_if_not_installed("estimatr")
  skip_if_not_installed("AER")
  skip_if_not_installed("quantreg")
  skip_if_not_installed("fixest")
  d <- .rf_wage_data()
  fits <- list(
    estimatr::lm_robust(log(wage) ~ education, data = d),
    estimatr::iv_robust(
      log(wage) ~ education + experience | feducation + experience,
      data = d
    ),
    AER::ivreg(
      log(wage) ~ education + experience | feducation + experience,
      data = d
    ),
    suppressWarnings(quantreg::rq(log(wage) ~ education, data = d)),
    fixest::feols(log(wage) ~ education | region, d),
    fixest::feols(log(wage) ~ experience | education ~ feducation, d),
    fixest::feols(
      log(wage) ~ experience | region | education ~ feducation,
      d
    )
  )
  for (fit in fits) {
    fr <- as_regression_frame(fit, model_id = "M1")
    expect_identical(fr$info$dv, "log(wage)", info = class(fit)[1L])
    expect_match(
      capture.output(print(table_regression(fit)))[1L],
      ": log(wage)",
      fixed = TRUE
    )
  }
})

test_that("a logical outcome of feglm keeps its I() wrapper", {
  skip_if_not_installed("fixest")
  d <- .rf_wage_data()
  fit <- fixest::feglm(
    I(parttime == "yes") ~ education | region,
    d,
    family = binomial
  )
  expect_identical(
    as_regression_frame(fit, model_id = "M1")$info$dv,
    "I(parttime == \"yes\")"
  )
})

test_that("a mixed lm + lm_robust table shows one outcome for the same model", {
  skip_if_not_installed("estimatr")
  d <- .rf_wage_data()
  out <- capture.output(print(table_regression(list(
    lm(log(wage) ~ education, d),
    estimatr::lm_robust(log(wage) ~ education, data = d)
  ))))
  expect_false(any(grepl("Outcome", out)))
  expect_match(out[1L], "log(wage)", fixed = TRUE)
})

test_that("a tobit fit names its transformed outcome", {
  skip_if_not_installed("AER")
  d <- .rf_wage_data()
  d$h <- pmax(d$wage - 3, 0)
  fit <- AER::tobit(sqrt(h) ~ education, data = d)
  expect_identical(as_regression_frame(fit, model_id = "M1")$info$dv, "sqrt(h)")
})
