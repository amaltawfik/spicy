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


# ---- Rows of a fixest IV fit take the variable's name --------------------

test_that("an IV feols names its endogenous regressor as ivreg does", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("AER")
  skip_if_not_installed("estimatr")
  d <- .rf_wage_data()
  iv <- AER::ivreg(
    log(wage) ~
      education +
        experience +
        I(experience^2) |
        feducation + experience + I(experience^2),
    data = d
  )
  ivr <- estimatr::iv_robust(
    log(wage) ~
      education +
        experience +
        I(experience^2) |
        feducation + experience + I(experience^2),
    data = d
  )
  ivf <- fixest::feols(
    log(wage) ~ experience + I(experience^2) | education ~ feducation,
    d
  )
  terms <- as_regression_frame(ivf, model_id = "M1")$coefs$term
  expect_setequal(
    terms,
    c("(Intercept)", "education", "experience", "I(experience^2)")
  )
  tbl <- as.data.frame(table_regression(list(iv, ivr, ivf), digits = 3))
  rows <- trimws(tbl[[1L]])
  expect_false(any(grepl("fit_|I\\(I\\(", rows)))
  expect_identical(sum(rows == "education"), 1L)
  expect_identical(sum(rows == "I(experience^2)"), 1L)
  # The three estimates of education sit on one row.
  b <- unlist(tbl[rows == "education", grepl(": B$", names(tbl))])
  expect_length(b, 3L)
  expect_false(anyNA(b))
  expect_equal(unname(b), rep(b[[1L]], 3L), tolerance = 1e-8)
})

test_that("labels, keep and drop take the variable's name on an IV feols", {
  skip_if_not_installed("fixest")
  d <- .rf_wage_data()
  ivf <- fixest::feols(
    log(wage) ~ experience + I(experience^2) | education ~ feducation,
    d
  )
  # `keep` and `drop` are regular expressions over the row names.
  kept <- as.data.frame(table_regression(ivf, keep = "^education$"))
  kept <- setdiff(kept$variable[kept$.row_role == "coef"], "(Intercept)")
  expect_identical(kept, "education")
  dropped <- as.data.frame(table_regression(
    ivf,
    drop = c("^education$", "^I\\(experience\\^2\\)$")
  ))
  expect_identical(
    dropped$variable[dropped$.row_role == "coef"],
    c("(Intercept)", "experience")
  )
  lab <- as.data.frame(expect_no_warning(table_regression(
    ivf,
    labels = c(education = "Years of schooling", "I(experience^2)" = "Sq.")
  )))
  expect_true(all(c("Years of schooling", "Sq.") %in% lab$label))
})

test_that("the AME of an IV feols sits on the endogenous regressor's row", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("marginaleffects")
  d <- .rf_wage_data()
  ivf <- fixest::feols(
    log(wage) ~ experience + I(experience^2) | education ~ feducation,
    d
  )
  co <- as_regression_frame(
    ivf,
    model_id = "M1",
    show_columns = c("b", "se", "p", "ame", "ame_p")
  )$coefs
  ame <- co[co$estimate_type == "ame", ]
  expect_true("education" %in% ame$term)
  expect_false(any(grepl("fit_", co$term)))
  expect_equal(
    ame$estimate[ame$term == "education"],
    unname(stats::coef(ivf)[["fit_education"]]),
    tolerance = 1e-6
  )
})

test_that("a feols regressor really called fit_x keeps its name", {
  skip_if_not_installed("fixest")
  d <- .rf_wage_data()
  d$fit_x <- d$experience
  fit <- fixest::feols(log(wage) ~ fit_x + I(fit_x^2), d)
  expect_setequal(
    as_regression_frame(fit, model_id = "M1")$coefs$term,
    c("(Intercept)", "fit_x", "I(fit_x^2)")
  )
})


# ---- `tau` on an rq fit --------------------------------------------------

test_that("tau on an rq fit says where the quantile is set", {
  skip_if_not_installed("quantreg")
  d <- .rf_wage_data()
  q50 <- suppressWarnings(quantreg::rq(log(wage) ~ education, data = d))
  err <- expect_error(
    table_regression(q50, tau = 0.75),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "no RMST column is requested", fixed = TRUE)
  expect_match(msg, "quantreg::rq(tau = )", fixed = TRUE)
  expect_match(msg, "one per quantile", fixed = TRUE)
  # Without an rq fit, the message keeps to the RMST horizon.
  err <- expect_error(
    table_regression(lm(log(wage) ~ education, d), tau = 0.75),
    class = "spicy_invalid_input"
  )
  expect_no_match(conditionMessage(err), "quantreg", fixed = TRUE)
})


# ---- Robust or clustered SE on an ivreg fit: the route --------------------

test_that("the refusal of HC1 or CR1 on an ivreg fit names the routes", {
  skip_if_not_installed("AER")
  d <- .rf_wage_data()
  iv <- AER::ivreg(
    log(wage) ~ education + experience | feducation + experience,
    data = d
  )
  for (args in list(
    list(vcov = "HC1"),
    list(vcov = "CR1", cluster = ~region)
  )) {
    err <- expect_error(
      do.call(table_regression, c(list(iv), args)),
      class = "spicy_unsupported_vcov"
    )
    msg <- conditionMessage(err)
    expect_match(msg, "This class supports: classical.", fixed = TRUE)
    expect_match(
      msg,
      "estimatr::iv_robust(..., se_type = \"HC1\")",
      fixed = TRUE
    )
    expect_match(msg, "`clusters =`", fixed = TRUE)
    expect_match(
      msg,
      "fixest::feols(y ~ x | endo ~ inst, vcov = \"hetero\")",
      fixed = TRUE
    )
    expect_match(msg, "vcov = ~g", fixed = TRUE)
  }
})

test_that("the routes the ivreg refusal names give a table", {
  skip_if_not_installed("estimatr")
  skip_if_not_installed("fixest")
  d <- .rf_wage_data()
  fits <- list(
    estimatr::iv_robust(
      log(wage) ~ education + experience | feducation + experience,
      data = d,
      se_type = "HC1"
    ),
    estimatr::iv_robust(
      log(wage) ~ education + experience | feducation + experience,
      data = d,
      clusters = region
    ),
    fixest::feols(
      log(wage) ~ experience | education ~ feducation,
      d,
      vcov = "hetero"
    ),
    fixest::feols(
      log(wage) ~ experience | education ~ feducation,
      d,
      vcov = ~region
    )
  )
  tbl <- as.data.frame(table_regression(fits))
  expect_identical(sum(tbl$variable == "education"), 1L)
})
