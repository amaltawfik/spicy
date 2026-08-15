# Weighted descriptives in table_continuous() -- decision 17.
#
# The convention is frequency-expansion, pinned here through its two
# exact acceptance properties (integer weights == expanded data,
# w = 1 == unweighted) and through the cross-software oracles of
# dev/weights_continuous_spec.md: Hmisc / matrixStats / DescTools
# agree on every default value below, PSPP 2.0 on mean / SD / N, and
# rescale = TRUE reproduces Stata's [aweight] SD (== survey::svyvar)
# to the sixteenth digit. The pinned dataset is the spec's:
# x = 1,2,3,4,5,7,9 with CASE A w = 0.5,1,1.5,2,1,0.5,2 and
# CASE B w = 1,2,3,1,2,1,2.

.wt_data <- function() {
  data.frame(
    x = c(1, 2, 3, 4, 5, 7, 9),
    wa = c(0.5, 1, 1.5, 2, 1, 0.5, 2),
    wb = c(1, 2, 3, 1, 2, 1, 2),
    one = 1
  )
}

test_that("integer weights equal the expanded data exactly (CASE B)", {
  d <- .wt_data()
  xe <- rep(d$x, d$wb)
  r <- table_continuous(
    d,
    select = x,
    weights = wb,
    show_columns = c("m", "sd", "med", "q1", "q3", "min", "max", "n"),
    output = "long"
  )
  # Equality to one ULP (tolerance 1e-15): the weighted formula and
  # the expanded computation take different floating-point routes to
  # the same real number.
  expect_equal(r$mean, mean(xe), tolerance = 1e-15)
  expect_equal(r$sd, sd(xe), tolerance = 1e-15)
  expect_equal(
    c(r$q1, r$median, r$q3),
    unname(quantile(xe, c(0.25, 0.5, 0.75), type = 7)),
    tolerance = 1e-15
  )
  expect_identical(c(r$min, r$max), c(min(xe), max(xe)))
  # n counts rows, weighted_n the expanded size.
  expect_identical(r$n, 7L)
  expect_identical(r$weighted_n, 12)
})

test_that("fractional weights hit the pinned cross-software oracles (CASE A)", {
  d <- .wt_data()
  r <- table_continuous(
    d,
    select = x,
    weights = wa,
    show_columns = c("m", "sd", "med", "q1", "q3", "n", "weighted_n"),
    output = "long"
  )
  # Hmisc::wtd.mean / every tool.
  expect_equal(r$mean, 4.882352941176471, tolerance = 1e-15)
  # Hmisc::wtd.var(normwt = FALSE) / matrixStats::weightedSd;
  # PSPP 2.0 DESCRIPTIVES (denominator sum(w) - 1 = 7.5).
  expect_equal(r$sd, 2.80196009824495, tolerance = 1e-14)
  # Hmisc::wtd.quantile default / DescTools::Quantile.
  expect_equal(c(r$q1, r$median, r$q3), c(3, 4, 7.5), tolerance = 1e-15)
  expect_identical(r$n, 7L)
  expect_equal(r$weighted_n, 8.5, tolerance = 1e-15)
})

test_that("rescale = TRUE reproduces the Stata aweight SD, scale-invariantly", {
  d <- .wt_data()
  r <- table_continuous(
    d,
    select = x,
    weights = wa,
    rescale = TRUE,
    output = "long"
  )
  # sqrt(survey::svyvar) == Stata [aweight] Std. dev. (hand value
  # 7007/867, dev/weights_continuous_spec.md).
  expect_equal(r$sd, 2.8428667890285464, tolerance = 1e-15)
  # The mean is a ratio: rescaling never moves it.
  expect_equal(r$mean, 4.882352941176471, tolerance = 1e-15)
  # Scale invariance: multiplying every weight by 1000 changes nothing.
  d2 <- transform(.wt_data(), wa = wa * 1000)
  r2 <- table_continuous(
    d2,
    select = x,
    weights = wa,
    rescale = TRUE,
    output = "long"
  )
  expect_identical(r$mean, r2$mean)
  expect_identical(r$sd, r2$sd)
  expect_identical(c(r$q1, r$median, r$q3), c(r2$q1, r2$median, r2$q3))
})

test_that("weights = 1 collapses to the unweighted table, plus its disclosure", {
  d <- .wt_data()
  u <- table_continuous(d, select = x, output = "long")
  w <- table_continuous(d, select = x, weights = one, output = "long")
  # Every statistic byte-identical; only weighted_n is filled.
  for (col in c(
    "mean",
    "sd",
    "min",
    "max",
    "ci_lower",
    "ci_upper",
    "median",
    "q1",
    "q3",
    "iqr",
    "n"
  )) {
    expect_identical(w[[col]], u[[col]])
  }
  expect_identical(w$weighted_n, 7)
  # The console differs by exactly one line: the weighting disclosure
  # (the family prints its notes bare, without the "Note." prefix).
  ou <- capture.output(print(table_continuous(d, select = x)))
  ow <- capture.output(print(table_continuous(d, select = x, weights = one)))
  expect_identical(setdiff(ow, ou), "Statistics weighted by one.")
})

test_that("a zero-weight row leaves every statistic, min and max included", {
  # The gtsummary {min}/{max} defect as a negative test: a weight-0
  # outlier must not set the extremes (dev/weights_continuous_spec.md,
  # annexe).
  d <- data.frame(
    x = c(1, 2, 3, 4, 5, 7, 9, 999),
    w = c(0.5, 1, 1.5, 2, 1, 0.5, 2, 0)
  )
  r <- table_continuous(
    d,
    select = x,
    weights = w,
    show_columns = c("min", "max", "n"),
    output = "long"
  )
  expect_identical(c(r$min, r$max), c(1, 9))
  expect_identical(r$n, 7L)
})

test_that("the weighted table names its weights and its exclusions", {
  d <- .wt_data()
  d$wa[2] <- NA
  txt <- paste(
    capture.output(print(table_continuous(d, select = x, weights = wa))),
    collapse = "\n"
  )
  expect_match(txt, "Statistics weighted by wa.", fixed = TRUE)
  expect_match(txt, "Rows with missing wa removed: 1.", fixed = TRUE)
})

test_that("the decision-17 refusals are hard and actionable", {
  d <- .wt_data()
  d$g <- rep(c("a", "b"), length.out = 7)
  # Group tests under weights: refused, pointing at the two ways out.
  expect_error(
    table_continuous(d, select = x, by = g, weights = wa),
    class = "spicy_not_implemented"
  )
  # ... and the p_value = FALSE way out works.
  r <- suppressWarnings(table_continuous(
    d,
    select = x,
    by = g,
    weights = wa,
    p_value = FALSE,
    output = "long"
  ))
  expect_true(all(c("a", "b") %in% r$group))
  # med_ci has no weighted version.
  expect_error(
    table_continuous(
      d,
      select = x,
      weights = wa,
      show_columns = c("med", "med_ci")
    ),
    class = "spicy_not_implemented"
  )
  # weighted_n without weights is a request for nothing.
  expect_error(
    table_continuous(d, select = x, show_columns = c("m", "weighted_n")),
    class = "spicy_invalid_input"
  )
  # Invalid weights are rejected up front.
  expect_error(
    table_continuous(d, select = x, weights = c(1, 1, 1, 1, 1, 1, -1)),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous(d, select = x, weights = c(1, 1, 1, 1, 1, 1, Inf)),
    class = "spicy_invalid_input"
  )
})

test_that("rescale under by normalises per variable, never per group", {
  # Doubling the weights of ONE group must not change that group's
  # rescaled statistics relative to a global rescale -- the relative
  # weights across groups are the information sampling weights carry.
  d <- .wt_data()
  d$g <- rep(c("a", "b"), length.out = 7)
  r <- suppressWarnings(table_continuous(
    d,
    select = x,
    by = g,
    weights = wa,
    rescale = TRUE,
    p_value = FALSE,
    output = "long"
  ))
  # The variable-level rescale keeps the group weighted_n in the raw
  # proportions: sum over groups == n used (7), split as the raw
  # weights split (wa: a-rows 0.5+1.5+1+2 = 5, b-rows 1+2+0.5 = 3.5,
  # scaled by 7/8.5).
  expect_equal(sum(r$weighted_n), 7, tolerance = 1e-12)
  expect_equal(
    r$weighted_n[r$group == "a"],
    5 * 7 / 8.5,
    tolerance = 1e-12
  )
})

test_that("the structured view carries the weighted_n token faithfully", {
  d <- .wt_data()
  tbl <- table_continuous(
    d,
    select = x,
    weights = wa,
    show_columns = c("m", "sd", "n", "weighted_n")
  )
  s <- as_structured(tbl)
  expect_identical(s$col_meta[["Weighted n"]]$token, "weighted_n")
  expect_equal(s$body[["Weighted n"]], 8.5, tolerance = 1e-15)
})
