# ---------------------------------------------------------------------------
# The branches of the two twins a normal table never takes: a domain
# with nothing in it, a delegation that fails, the alignments the
# console offers and the coercions of a one-way table.
#
# Not "coverage filler": each one is a path a real design reaches, and
# each says what the table does when it gets there.
# ---------------------------------------------------------------------------

.svyedge_design <- function() {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1,
    fpc = ~fpc
  )
}

test_that("a variable with no observation leaves every cell undefined", {
  skip_if_not_installed("survey")
  d <- data.frame(y = rep(NA_real_, 6), z = c(1, 2, 3, 4, 5, 6), w = rep(1, 6))
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  out <- suppressWarnings(table_continuous_svy(
    des,
    select = c(y, z),
    output = "long"
  ))
  expect_identical(out$n, c(0L, 6L))
  expect_true(is.na(out$mean[[1L]]))
  expect_true(is.na(out$weighted_n[[1L]]))
  expect_false(is.na(out$mean[[2L]]))
  # And the table still prints: an empty variable is a row of dashes,
  # not a crash.
  printed <- capture.output(print(suppressWarnings(
    table_continuous_svy(des, select = c(y, z))
  )))
  expect_true(any(grepl("y", printed, fixed = TRUE)))
})

test_that("a single observation has a mean and no standard deviation", {
  skip_if_not_installed("survey")
  d <- data.frame(y = c(5, NA, NA), w = c(1, 1, 1))
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  out <- suppressWarnings(table_continuous_svy(
    des,
    select = y,
    output = "long"
  ))
  expect_identical(out$n, 1L)
  expect_equal(out$mean, 5)
  expect_true(is.na(out$sd) || out$sd == 0)
})

test_that("a categorical variable with no observation is refused, not blank", {
  skip_if_not_installed("survey")
  d <- data.frame(
    g = factor(rep(NA_character_, 6), levels = c("a", "b")),
    w = rep(1, 6)
  )
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  # Every value missing and `drop_na = TRUE`: there is no row to draw.
  # A refusal naming the variable, rather than a block of dashes that
  # would read as "estimated, and undefined".
  err <- expect_error(
    table_categorical_svy(des, select = g, drop_na = TRUE),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "no level to display", fixed = TRUE)
  # With `drop_na = FALSE` the (Missing) level IS the table, and it is
  # 100% by ARITHMETIC: survey cannot say so (`svymean(~f)` aborts on a
  # one-level factor), and the interval and the design effect of a
  # degenerate proportion are not estimable, so they stay undefined.
  out <- table_categorical_svy(
    des,
    select = g,
    proportion_ci = TRUE,
    deff = TRUE,
    output = "long"
  )
  expect_identical(out$level, c(NA, "(Missing)"))
  expect_equal(out[["%"]][[2L]], 100)
  expect_identical(out$n[[2L]], 6L)
  expect_true(is.na(out[["% CI lower"]][[2L]]))
  expect_true(is.na(out$DEff[[2L]]))
})

test_that("a level nobody chose leaves the table with the others intact", {
  skip_if_not_installed("survey")
  d <- data.frame(
    g = factor(c("a", "a", "b", "b", "a", "b"), levels = c("a", "b", "c")),
    w = rep(1, 6)
  )
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  out <- table_categorical_svy(des, select = g, output = "long")
  # "c" is declared and unobserved: a domain with no observation has no
  # degrees of freedom, and every one of its cells would be a dash.
  expect_identical(out$level, c(NA, "a", "b"))
  expect_equal(out[["%"]][-1L], c(50, 50))
})

test_that("`show_columns` that prunes itself empty is refused", {
  d <- .svyedge_design()
  # "ci" alone is pruned (it is the interval OF THE MEAN, and the mean
  # is not displayed), and nothing is left to show.
  expect_warning(
    expect_error(
      table_continuous_svy(d, select = api00, show_columns = "ci"),
      class = "spicy_invalid_input"
    ),
    class = "spicy_ignored_arg"
  )
})

test_that("the console honours `align = \"center\"` and `\"right\"`", {
  d <- .svyedge_design()
  for (a in c("center", "right")) {
    expect_snapshot_output(
      print(table_continuous_svy(d, select = api00, align = a))
    )
    expect_snapshot_output(
      print(table_categorical_svy(d, select = stype, align = a))
    )
  }
})

test_that("a one-way table coerces without a grouping marker", {
  d <- .svyedge_design()
  cont <- as.data.frame(table_continuous_svy(d, select = api00))
  expect_null(attr(cont, "group_var"))
  expect_false(is.null(attr(cont, "design_meta")))
  cat_tbl <- as.data.frame(table_categorical_svy(d, select = stype))
  expect_null(attr(cat_tbl, "group_var"))
  expect_identical(nrow(cat_tbl), 4L)
})

test_that("a delegation that fails leaves an undefined cell, not an error", {
  d <- .svyedge_design()
  local_mocked_bindings(
    svyvar = function(...) stop("no variance here"),
    .package = "survey"
  )
  out <- table_continuous_svy(d, select = api00, output = "long")
  expect_true(is.na(out$sd))
  # Everything else still comes through: the guard is per delegation,
  # not per table.
  expect_equal(out$mean, 644.16939890710387, tolerance = 1e-12)
})

test_that("a failing quantile leaves the position statistics undefined", {
  d <- .svyedge_design()
  local_mocked_bindings(
    svyquantile = function(...) stop("singular"),
    .package = "survey"
  )
  out <- table_continuous_svy(
    d,
    select = api00,
    show_columns = c("m", "med", "q1", "q3", "iqr"),
    output = "long"
  )
  expect_true(all(is.na(c(out$median, out$q1, out$q3, out$iqr))))
  expect_false(is.na(out$mean))
})

test_that("a failing test leaves the p empty rather than the table broken", {
  d <- .svyedge_design()
  local_mocked_bindings(
    svychisq = function(...) stop("no test"),
    .package = "survey"
  )
  out <- table_categorical_svy(
    d,
    select = stype,
    by = sch.wide,
    output = "long"
  )
  expect_true(all(is.na(out$p)))
  expect_false(is.na(out[["Yes %"]][[2L]]))
})

test_that("a failing group comparison leaves the continuous test columns empty", {
  d <- .svyedge_design()
  local_mocked_bindings(
    svyttest = function(...) stop("no test"),
    .package = "survey"
  )
  out <- table_continuous_svy(
    d,
    select = api00,
    by = sch.wide,
    statistic = TRUE,
    output = "long"
  )
  expect_true(all(is.na(out$p.value)))
  expect_false(is.na(out$mean[[1L]]))
})

test_that("a failing design effect leaves its column undefined", {
  d <- .svyedge_design()
  local_mocked_bindings(
    deff = function(...) stop("no deff"),
    .package = "survey"
  )
  out <- table_continuous_svy(d, select = api00, deff = TRUE, output = "long")
  expect_true(is.na(out$deff))
  out2 <- table_categorical_svy(d, select = stype, deff = TRUE, output = "long")
  expect_true(all(is.na(out2$DEff)))
})
