# table_regression() and its screen: the data frame of a table without its
# long frame, the title of a glmer family with no title of its own, the
# sources of `labels = TRUE`, and the arguments of table_regression_uv().

test_that("as.data.frame() of a table without its long frame drops only the totals", {
  fit <- glm(smoking ~ age + sex, data = sochealth, family = binomial())
  tbl <- table_regression(fit, show_columns = c("n_events", "b", "ci", "p"))
  full <- as.data.frame(tbl)
  expect_true("Events/N: N" %in% names(full))
  # The N of "events/N" is read off the long frame, never off the string.
  for (long in list(NULL, attr(tbl, "spicy_long")[0L, ])) {
    bare <- tbl
    attr(bare, "spicy_long") <- long
    out <- as.data.frame(bare)
    expect_identical(names(out), setdiff(names(full), "Events/N: N"))
    expect_identical(c(out), c(full)[names(out)])
  }
})

test_that("a glmer family with no title of its own is titled by its name", {
  skip_if_not_installed("lme4")
  fit <- lme4::glmer(
    bmi ~ sex + (1 | education),
    data = as.data.frame(sochealth[1:300, ]),
    family = gaussian(link = "log")
  )
  expect_match(
    attr(table_regression(fit), "title"),
    "^Gaussian mixed-effects regression: bmi$"
  )
})

test_that("labels = TRUE reads an outcome label its frame builder does not read", {
  # The nls frame keeps the outcome's name as its label: the label comes
  # from the variables of the fit.
  d <- data.frame(x = 1:20)
  d$y <- 2 * exp(0.1 * d$x) + sin(d$x) / 5
  attr(d$y, "label") <- "Growth index"
  fit <- nls(y ~ a * exp(b * x), data = d, start = list(a = 1, b = 0.2))
  expect_match(attr(table_regression(fit), "title"), ": y$")
  expect_match(
    attr(table_regression(fit, labels = TRUE), "title"),
    ": Growth index$"
  )
})

test_that("labels = TRUE reads the data of the call when insight is not installed", {
  skip_if_not_installed("survival")
  sh <- as.data.frame(sochealth[!is.na(sochealth$bmi), ])
  sh$time <- sh$bmi
  sh$status <- as.integer(sh$age > 50)
  fit <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = sh)
  with_insight <- table_regression(fit, labels = TRUE)$Variable
  real <- base::requireNamespace
  asked <- 0L
  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "insight")) {
        asked <<- asked + 1L
        return(FALSE)
      }
      real(package, ...)
    },
    .package = "base"
  )
  without <- table_regression(fit, labels = TRUE)$Variable
  expect_gt(asked, 0L)
  # model.frame() rebuilt `sex` without its label; the call's data has it.
  expect_true(all(c("Age (years)", "Sex:", "  Male") %in% without))
  expect_identical(without, with_insight)
})

test_that("the data of the call of a fit whose formula has no environment is NULL", {
  # brms's formula() returns a brmsformula, a list with no environment:
  # the `data` of the call has nowhere to be evaluated.
  fit <- structure(
    list(
      formula = structure(list(formula = bmi ~ age), class = "bform_like"),
      call = quote(fit_model(bmi ~ age, data = sochealth))
    ),
    class = "spicy_formula_list_fit"
  )
  expect_null(environment(stats::formula(fit)))
  expect_null(spicy:::.reg_call_data(fit))
})

test_that("labels = TRUE reads past a data column that has no name", {
  expect_identical(
    spicy:::.reg_bare_var(c(NA, "", "age", "log(age)")),
    c(NA, NA, "age", NA)
  )
  # `x` has no label anywhere, so the data of the call is read, its
  # nameless column included.
  d <- data.frame(y = sochealth$bmi, x = as.numeric(sochealth$age))
  attr(d$y, "label") <- "Body mass index"
  d[[3L]] <- sochealth$income
  names(d)[3L] <- NA
  fit <- lm(y ~ x, data = d)
  expect_no_warning(out <- table_regression(fit, labels = TRUE))
  expect_match(attr(out, "title"), ": Body mass index$")
  expect_true("x" %in% out$Variable)
})

test_that("labels = TRUE reads the label of a variable whose name does not parse", {
  d <- data.frame(
    y = sochealth$bmi,
    `age years` = sochealth$age,
    check.names = FALSE
  )
  attr(d[["age years"]], "label") <- "Age in years"
  fit <- lm(y ~ `age years`, data = d)
  # model.frame() writes the name without its backticks.
  expect_true("age years" %in% names(stats::model.frame(fit)))
  out <- table_regression(fit, labels = TRUE)$Variable
  expect_true("Age in years" %in% out)
  expect_false(any(c("age years", "`age years`") %in% out))
})

test_that("table_regression_uv() refuses an unnamed argument passed through `...`", {
  # Every formal is filled by position, so the twelfth value lands in
  # `...` with no name.
  err <- expect_error(
    table_regression_uv(
      sochealth,
      wellbeing_score,
      age,
      "lm",
      stats::binomial(),
      FALSE,
      FALSE,
      "b",
      FALSE,
      FALSE,
      NULL,
      "HC3"
    ),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(err),
    "Every argument passed through `...` must be named.",
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err),
    "They go to `table_regression()`, by name.",
    fixed = TRUE
  )
})
