# Incidental fixes from the Group D verification pass:
#   * the statistic column header follows the model's actual reference
#     distribution ("z" for z-asymptotic classes; the "t" TOKEN stays
#     API-stable) -- a hardcoded "t" header over z statistics mislabels
#     the column (Stata prints z for logit, t for regress);
#   * table_regression(m1, m2) (forgotten list()) gets a clear error
#     instead of the baffling "`vcov` is a list of length 12".

test_that("statistic header: z for glm, t for lm, per-model in mixed tables", {
  g <- glm(am ~ mpg, data = mtcars, family = binomial())
  l <- lm(mpg ~ wt, data = mtcars)

  out_g <- table_regression(g, show_columns = c("b", "t", "p"))
  expect_true("z" %in% names(out_g))
  expect_false("t" %in% names(out_g))

  out_l <- table_regression(l, show_columns = c("b", "t", "p"))
  expect_true("t" %in% names(out_l))

  out_m <- table_regression(list(l, g), show_columns = c("b", "t", "p"))
  # Unnamed list with distinct DVs: bare DV names become the spanner
  # labels, and each model carries its own statistic letter (t for lm,
  # z for glm) -- pin the complete header row.
  expect_identical(
    names(out_m),
    c("Variable", "mpg: B", "mpg: t", "mpg: p", "am: B", "am: z", "am: p")
  )
})

test_that("statistic header: z for coxph and Gamma-log glm", {
  skip_if_not_installed("survival")
  cx <- survival::coxph(survival::Surv(time, status) ~ age,
                        data = survival::lung)
  expect_true("z" %in% names(table_regression(cx,
                                              show_columns = c("b", "t", "p"))))
  set.seed(1)
  d <- data.frame(y = rgamma(100, 2, 0.5), x = rnorm(100))
  gg <- glm(y ~ x, data = d, family = Gamma(link = "log"))
  expect_true("z" %in% names(table_regression(gg,
                                              show_columns = c("b", "t", "p"))))
})

test_that("forgotten list(): model passed as vcov gets a clear error", {
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ hp, data = mtcars)
  err <- tryCatch(
    table_regression(m1, m2),
    spicy_invalid_input = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "`vcov` must be a string or a list of strings, not a `lm` object.",
    fixed = TRUE
  )
  expect_match(
    conditionMessage(err),
    "Wrap the models instead: `table_regression(list(m1, m2))`.",
    fixed = TRUE
  )
})

test_that("vcov validation still accepts strings and string lists", {
  m1 <- lm(mpg ~ wt, data = mtcars)
  m2 <- lm(mpg ~ hp, data = mtcars)
  expect_s3_class(table_regression(m1, vcov = "HC3"),
                  "spicy_regression_table")
  expect_s3_class(
    table_regression(list(m1, m2), vcov = list("classical", "HC3")),
    "spicy_regression_table"
  )
})

test_that("AME request on an incapable class is refused, not an empty column", {
  skip_if_not_installed("survival")
  # coxph has its own earlier class-specific refusal (kept).
  cx <- survival::coxph(survival::Surv(time, status) ~ age,
                        data = survival::lung)
  err_cox <- tryCatch(
    table_regression(cx, show_columns = c("b", "ame")),
    spicy_invalid_input = function(e) e
  )
  expect_s3_class(err_cox, "spicy_invalid_input")
  # cli renders the token quotes per terminal capability ("ame" on
  # Windows locally, 'ame' on CI runners, curly quotes in fancy
  # terminals) -- match the full sentence with a one-character
  # wildcard per quote.
  expect_match(
    conditionMessage(err_cox),
    "Token\\(s\\) .ame. in `show_columns` are not defined for Cox models\\."
  )

  # Classes without a specific refusal hit the universal supports$ame
  # guard, which points at the registry (was: silently empty column).
  skip_if_not_installed("flexsurv")
  fs <- flexsurv::flexsurvreg(
    survival::Surv(futime, fustat) ~ age,
    data = survival::ovarian, dist = "weibull"
  )
  err_fs <- tryCatch(
    table_regression(fs, show_columns = c("b", "ame")),
    spicy_invalid_input = function(e) e
  )
  expect_s3_class(err_fs, "spicy_invalid_input")
  expect_match(
    conditionMessage(err_fs),
    paste0("No average-marginal-effects backend exists for this class ",
           "(see ?table_regression_models, column AME)."),
    fixed = TRUE
  )

  # Mixed table with one capable model still renders (incapable em-dashes).
  l <- lm(mpg ~ wt, data = mtcars)
  expect_s3_class(
    table_regression(list(l, cx), show_columns = c("b", "ame", "p")),
    "spicy_regression_table"
  )
})
