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
  # No observed value at all: no data, so its count is undefined too.
  expect_identical(out$n, c(NA_integer_, 6L))
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
  # `svyvar()` on one observation divides by n - 1 = 0 and gives NaN,
  # which the guard turns into an absent number: the cell is the
  # undefined dash, not a zero that would read as "no dispersion".
  expect_true(is.na(out$sd))
})

test_that("a categorical variable with no observation has no data, not a row of 100%", {
  skip_if_not_installed("survey")
  d <- data.frame(
    g = factor(rep(NA_character_, 6), levels = c("a", "b")),
    w = rep(1, 6)
  )
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  # Every value missing: the variable has no data, which is not a missing
  # answer, whatever `drop_na`. It keeps its declared levels, every cell
  # undefined, and the note says so; nothing is counted as removed.
  for (dn in c(TRUE, FALSE)) {
    out <- table_categorical_svy(
      des,
      select = g,
      drop_na = dn,
      output = "data.frame"
    )
    expect_identical(out$level, c(NA, "a", "b"))
    expect_true(all(is.na(out$n[-1L]) & is.na(out[["%"]][-1L])))
    expect_match(attr(out, "note"), "No data for g.", fixed = TRUE)
    expect_false(grepl("removed", attr(out, "note"), fixed = TRUE))
  }
  # A selection of no level is refused, naming the variable, rather than
  # a variable with no row under its header.
  expect_error(
    table_categorical_svy(des, select = g, levels_keep = character(0)),
    class = "spicy_invalid_input"
  )
  # A variable observed at one level is 100% by ARITHMETIC: survey cannot
  # say so (`svymean(~f)` aborts on a one-level factor). The design effect
  # of a degenerate proportion is not estimable and stays undefined; its
  # interval is the exact binomial one of 6 events out of 6, as for any
  # percentage of 100.
  d$h <- factor(rep("a", 6), levels = c("a", "b"))
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  out <- table_categorical_svy(
    des,
    select = h,
    proportion_ci = TRUE,
    deff = TRUE,
    output = "data.frame"
  )
  expect_identical(out$level, c(NA, "a"))
  expect_equal(out[["%"]][[2L]], 100)
  expect_identical(out$n[[2L]], 6)
  expect_equal(
    c(out[["95% CI LL"]][[2L]], out[["95% CI UL"]][[2L]]),
    c(100 * stats::qbeta(0.025, 6, 1), 100),
    tolerance = 1e-10
  )
  expect_true(is.na(out$DEff[[2L]]))
})

test_that("a level nobody chose leaves the table with the others intact", {
  skip_if_not_installed("survey")
  d <- data.frame(
    g = factor(c("a", "a", "b", "b", "a", "b"), levels = c("a", "b", "c")),
    w = rep(1, 6)
  )
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  out <- table_categorical_svy(des, select = g, output = "data.frame")
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

test_that("the console honors `align = \"center\"` and `\"right\"`", {
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
  cont_tbl <- table_continuous_svy(d, select = api00)
  cont <- as.data.frame(cont_tbl)
  # No `by`: no group column, no row role, and of the attributes the
  # note alone.
  expect_identical(names(cont)[1:3], c("variable", "label", "M"))
  expect_null(attr(cont, "group_var"))
  expect_null(attr(cont, "design_meta"))
  expect_identical(attr(cont, "note"), attr(cont_tbl, "note"))
  cat_tbl <- as.data.frame(table_categorical_svy(d, select = stype))
  expect_null(attr(cat_tbl, "group_var"))
  expect_identical(nrow(cat_tbl), 4L)
  expect_identical(
    names(cat_tbl),
    c("variable", "label", "level", ".row_role", "n", "%")
  )
})

test_that("a delegation that fails leaves an undefined cell, not an error", {
  d <- .svyedge_design()
  local_mocked_bindings(
    svyvar = function(...) stop("no variance here"),
    .package = "survey"
  )
  # Not silent: one warning quotes survey's message and names the
  # function, and the note says what the dash is, in the general sentence
  # since the error is no lonely primary sampling unit.
  w <- expect_warning(
    out <- table_continuous_svy(d, select = c(api00, api99), output = "long"),
    class = "spicy_svy_estimation_failed"
  )
  expect_identical(w$survey_message, "no variance here")
  expect_identical(w$fn, list("survey::svyvar()"))
  # Its call is the table the user called.
  expect_true(startsWith(
    conditionMessage(w),
    "Some cells could not be estimated:\n"
  ))
  expect_identical(
    conditionCall(w),
    quote(table_continuous_svy(d, select = c(api00, api99), output = "long"))
  )
  expect_match(
    conditionMessage(w),
    "`survey::svyvar()` stopped: \"no variance here\"",
    fixed = TRUE
  )
  expect_false(grepl("survey.lonely.psu", conditionMessage(w), fixed = TRUE))
  expect_true(all(is.na(out$sd)))
  expect_match(
    attr(out, "note"),
    "A dash marks an estimate the design could not provide. ",
    fixed = TRUE
  )
  expect_false(grepl("primary sampling unit", attr(out, "note"), fixed = TRUE))
  # In French, the general sentence too.
  withr::with_options(list(spicy.language = "fr"), {
    note_fr <- attr(
      suppressWarnings(table_continuous_svy(d, select = api00)),
      "note"
    )
  })
  expect_match(note_fr, .SVY_NOT_ESTIMATED_FR, fixed = TRUE)
  expect_false(grepl("primaire", note_fr, fixed = TRUE))
  # Everything else still comes through: the guard is per delegation,
  # not per table.
  expect_equal(out$mean[[1L]], 644.16939890710387, tolerance = 1e-12)
})

test_that("a failing quantile leaves the position statistics undefined", {
  d <- .svyedge_design()
  local_mocked_bindings(
    svyquantile = function(...) stop("singular"),
    .package = "survey"
  )
  expect_warning(
    out <- table_continuous_svy(
      d,
      select = api00,
      show_columns = c("m", "med", "q1", "q3", "iqr"),
      output = "long"
    ),
    class = "spicy_svy_estimation_failed"
  )
  expect_true(all(is.na(c(out$median, out$q1, out$q3, out$iqr))))
  expect_false(is.na(out$mean))
})

test_that("a failing quantile is reported only when a quantile is shown", {
  d <- .svyedge_design()
  local_mocked_bindings(
    svyquantile = function(...) stop("no quantile"),
    .package = "survey"
  )
  # The default columns show no quantile: no dash, nothing to say.
  expect_no_warning(out <- table_continuous_svy(d, select = api00))
  expect_false(grepl("A dash marks", attr(out, "note"), fixed = TRUE))
  w <- expect_warning(
    out <- table_continuous_svy(
      d,
      select = api00,
      show_columns = c("m", "iqr")
    ),
    class = "spicy_svy_estimation_failed"
  )
  expect_identical(w$fn, list("survey::svyquantile()"))
  expect_match(attr(out, "note"), .SVY_NOT_ESTIMATED_EN, fixed = TRUE)
})

test_that("a failing test leaves the p empty rather than the table broken", {
  d <- .svyedge_design()
  local_mocked_bindings(
    svychisq = function(...) stop("no test"),
    .package = "survey"
  )
  w <- expect_warning(
    out <- table_categorical_svy(
      d,
      select = stype,
      by = sch.wide,
      output = "data.frame"
    ),
    class = "spicy_svy_estimation_failed"
  )
  expect_identical(w$fn, list("survey::svychisq()"))
  expect_true(startsWith(
    conditionMessage(w),
    "Some cells could not be estimated:\n"
  ))
  expect_identical(
    conditionCall(w),
    quote(table_categorical_svy(
      d,
      select = stype,
      by = sch.wide,
      output = "data.frame"
    ))
  )
  expect_identical(unique(out$p), NA_real_)
  expect_false(is.na(out[["Yes %"]][[2L]]))
  expect_match(
    attr(out, "note"),
    "A dash marks an estimate the design could not provide. ",
    fixed = TRUE
  )
})

test_that("a failing group comparison leaves the continuous test columns empty", {
  d <- .svyedge_design()
  local_mocked_bindings(
    svyttest = function(...) stop("no test"),
    .package = "survey"
  )
  w <- expect_warning(
    out <- table_continuous_svy(
      d,
      select = api00,
      by = sch.wide,
      statistic = TRUE,
      output = "long"
    ),
    class = "spicy_svy_estimation_failed"
  )
  expect_identical(w$fn, list("survey::svyttest()"))
  expect_true(all(is.na(out$p.value)))
  expect_false(is.na(out$mean[[1L]]))
})

test_that("a failing design effect leaves its column undefined", {
  d <- .svyedge_design()
  local_mocked_bindings(
    deff = function(...) stop("no deff"),
    .package = "survey"
  )
  expect_warning(
    out <- table_continuous_svy(
      d,
      select = api00,
      deff = TRUE,
      output = "long"
    ),
    class = "spicy_svy_estimation_failed"
  )
  expect_true(is.na(out$deff))
  expect_warning(
    out2 <- table_categorical_svy(
      d,
      select = stype,
      deff = TRUE,
      output = "data.frame"
    ),
    class = "spicy_svy_estimation_failed"
  )
  expect_identical(unique(out2$DEff), NA_real_)
})

test_that("a failing test of the battery says what its dash is", {
  d <- .svyedge_design()
  local_mocked_bindings(
    svychisq = function(...) stop("no test"),
    .package = "survey"
  )
  # Four items, four tests, one message: one warning.
  w <- expect_warning(
    out <- table_battery_svy(
      d,
      select = c(awards, sch.wide, comp.imp, both),
      by = stype,
      output = "long"
    ),
    class = "spicy_svy_estimation_failed"
  )
  expect_identical(w$survey_message, "no test")
  expect_identical(w$fn, list("survey::svychisq()"))
  expect_true(startsWith(
    conditionMessage(w),
    "Some cells could not be estimated:\n"
  ))
  expect_identical(
    conditionCall(w),
    quote(table_battery_svy(
      d,
      select = c(awards, sch.wide, comp.imp, both),
      by = stype,
      output = "long"
    ))
  )
  expect_true(all(is.na(out$p.value)))
  expect_false(anyNA(out$pct))
  expect_match(
    attr(out, "note"),
    "A dash marks an estimate the design could not provide. ",
    fixed = TRUE
  )
})

test_that("the warning log of a table counts survey's, not spicy's own", {
  skip_on_cran()
  log <- .svy_warning_log()
  # spicy's own warning goes through as raised; survey's are kept.
  w <- expect_warning(
    withCallingHandlers(
      {
        warning("twice")
        spicy_warn("own", class = "spicy_undefined_stat")
        warning("once")
        warning("twice")
      },
      warning = log$record
    ),
    class = "spicy_undefined_stat"
  )
  expect_identical(conditionMessage(w), "own")
  ws <- .svy_collect_warnings(log$resignal())$warnings
  expect_identical(
    vapply(ws, conditionMessage, character(1)),
    c("twice\n(raised 2 times for this table)", "once")
  )
  expect_identical(class(ws[[1L]]), c("simpleWarning", "warning", "condition"))
})

test_that("the note names the lonely unit only when it explains every dash", {
  lonely <- "Stratum (H) has only one PSU at stage 1"
  expect_null(.svy_failure_note(list(message = character(0), fn = list())))
  expect_identical(
    .svy_failure_note(list(
      message = c(
        lonely,
        "At least one stratum contains only one PSU at stage 1"
      ),
      fn = list("survey::svymean()", "survey::svyvar()")
    )),
    spicy_str("note_design_not_estimated_lonely_psu")
  )
  # A lonely unit at a later stage is no stratum with a single PSU.
  expect_identical(
    .svy_failure_note(list(
      message = "At least one stratum contains only one PSU at stage 2",
      fn = list("survey::svymean()")
    )),
    spicy_str("note_design_not_estimated")
  )
  # A second error of another kind: the dashes are not all the lonely
  # unit's, and the general sentence is the true one.
  expect_identical(
    .svy_failure_note(list(
      message = c(lonely, "singular"),
      fn = list("survey::svymean()", "survey::svyquantile()")
    )),
    spicy_str("note_design_not_estimated")
  )
})
