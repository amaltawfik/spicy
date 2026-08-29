# Coverage tests for the survey-family arms no test reached: the two
# domains that come back without an estimate (one that observed
# nothing, one survey declines to estimate), the `by` refusal, the
# one-way arms of the render and broom layers, the two `select` doors
# of the continuous twin, and the two helpers that read the WEIGHTS
# rather than the rows.
#
# Lines closed:
#   R/table_categorical_svy.R         100, 153, 538-541
#   R/table_categorical_svy_render.R  220-227
#   R/table_categorical_svy_print.R   234
#   R/table_continuous_svy.R          752, 768, 1197
#   R/table_continuous_svy_print.R    136
#   R/survey_helpers.R                267, 620-621

# The api cluster design the rest of the survey suite uses, with and
# without its finite-population correction.
.svycov_api <- function(fpc = TRUE) {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  if (fpc) {
    survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
  } else {
    survey::svydesign(id = ~dnum, weights = ~pw, data = apiclus1)
  }
}


# ---- table_categorical_svy.R 100: a domain with nothing observed ----

test_that("a domain that observed no value at all comes back empty", {
  # `v2` is a subgroup-specific question: every unit of group B was
  # routed past it, so B's domain holds no observed value carrying a
  # non-zero weight. `.cat_svy_level_stats()` hands back its
  # initialised NA there instead of asking survey for the mean of
  # nothing -- the table keeps its shape and the other blocks keep
  # their numbers.
  skip_if_not_installed("survey")
  d <- data.frame(
    id = 1:200,
    g = rep(c("A", "B"), each = 100),
    w = 1,
    v = factor(rep(c("x", "y", "z"), length.out = 200))
  )
  d$v2 <- factor(ifelse(d$g == "B", NA, as.character(d$v)))
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)

  tbl <- table_categorical_svy(
    des,
    v2,
    by = g,
    proportion_ci = TRUE,
    drop_na = TRUE
  )
  lv <- tbl$.row_role == "level"
  expect_identical(tbl$level[lv], c("x", "y", "z"))

  # B: an honest zero observations, and every estimate above it
  # undefined -- not a zero percentage, which would read as a measured
  # absence.
  expect_identical(tbl[["B n"]][lv], c(0L, 0L, 0L))
  expect_true(all(is.na(tbl[["B %"]][lv])))
  expect_true(all(is.na(tbl[["B % CI lower"]][lv])))
  expect_true(all(is.na(tbl[["B % CI upper"]][lv])))

  # A and the margin are untouched: 34 / 33 / 33 of the 100 values the
  # question was asked of, each inside its own interval.
  expect_identical(tbl[["A n"]][lv], c(34L, 33L, 33L))
  expect_equal(tbl[["A %"]][lv], c(34, 33, 33))
  expect_identical(tbl[["Total n"]][lv], c(34L, 33L, 33L))
  expect_equal(tbl[["Total %"]][lv], c(34, 33, 33))
  for (blk in c("A", "Total")) {
    pct <- tbl[[paste(blk, "%")]][lv]
    ll <- tbl[[paste(blk, "% CI lower")]][lv]
    ul <- tbl[[paste(blk, "% CI upper")]][lv]
    expect_true(all(is.finite(ll)))
    expect_true(all(is.finite(ul)))
    expect_true(all(ll < pct & pct < ul))
  }
})


# ---- table_categorical_svy.R 153: no interval where there is no % ----

test_that("a domain survey declines to estimate keeps its counts", {
  # Group C is a lone PSU in a stratum of its own. Under survey's own
  # default -- `survey.lonely.psu = "fail"` -- `svymean()` aborts on
  # that domain, `.svy_try()` returns NULL, and every percentage of C
  # is NA. The interval loop then has nothing to put an interval
  # around and skips the level rather than asking `svyciprop()` for
  # one anyway.
  #
  # MUTATION-EQUIVALENT GUARD (review of coverage-013): without the
  # skip, the doomed `svyciprop()` call is made, `.svy_try()` swallows
  # its failure, and the same NA comes out -- no assertion can tell
  # the two apart. The guard is kept for what it states at the source
  # (no interval where there is no percentage) and for not making a
  # call known to fail; this witness covers the line and the CASE.
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  d <- data.frame(
    psu = c(rep(1:6, each = 10), rep(7L, 4)),
    strat = c(rep(c("s1", "s2"), each = 30), rep("s3", 4)),
    g = c(rep(c("A", "B"), each = 30), rep("C", 4)),
    w = 1,
    v = factor(c(
      rep("x", 12),
      rep("y", 18),
      rep("x", 20),
      rep("y", 10),
      "x",
      "y",
      "x",
      "y"
    ))
  )
  des <- survey::svydesign(id = ~psu, strata = ~strat, weights = ~w, data = d)

  tbl <- table_categorical_svy(des, v, by = g, proportion_ci = TRUE)
  lv <- tbl$.row_role == "level"
  expect_identical(tbl$level[lv], c("x", "y"))

  # The counts survive: they are read off the rows, not estimated.
  expect_identical(tbl[["C n"]][lv], c(2L, 2L))
  expect_true(all(is.na(tbl[["C %"]][lv])))
  expect_true(all(is.na(tbl[["C % CI lower"]][lv])))
  expect_true(all(is.na(tbl[["C % CI upper"]][lv])))

  # A and B are unaffected -- a domain that fails does not take the
  # table down with it.
  expect_identical(tbl[["A n"]][lv], c(12L, 18L))
  expect_equal(tbl[["A %"]][lv], c(40, 60))
  expect_identical(tbl[["B n"]][lv], c(20L, 10L))
  expect_equal(tbl[["B %"]][lv], c(200 / 3, 100 / 3))
  expect_true(all(is.finite(tbl[["A % CI lower"]][lv])))
  expect_true(all(is.finite(tbl[["B % CI upper"]][lv])))

  # The margin is estimated on the WHOLE design, which contains the
  # same lone PSU, so it dashes for the same reason -- with its counts
  # standing.
  expect_identical(tbl[["Total n"]][lv], c(34L, 30L))
  expect_true(all(is.na(tbl[["Total %"]][lv])))
})


# ---- table_categorical_svy.R 538-541: `by` names ONE column ----

test_that("`by` must name a single column of the design", {
  des <- .svycov_api(fpc = FALSE)
  err <- expect_error(
    table_categorical_svy(des, stype, by = c(awards, sch.wide)),
    class = "spicy_invalid_input"
  )
  expect_identical(
    conditionMessage(err),
    "`by` must be a single column name in the design's variables."
  )
  # Every way the selection can fail answers with that one sentence:
  # the tryCatch replaces tidyselect's own message, which would name
  # a data frame the caller never saw.
  err2 <- expect_error(
    table_categorical_svy(des, stype, by = not_a_column),
    class = "spicy_invalid_input"
  )
  expect_identical(conditionMessage(err2), conditionMessage(err))
})


# ---- table_categorical_svy_render.R 220-227: the one-way CI spanner ----

test_that("a one-way table pairs its interval bounds under one spanner", {
  des <- .svycov_api(fpc = FALSE)
  tbl <- table_categorical_svy(des, c(stype, awards), proportion_ci = TRUE)
  # Without `by` there is no block spanner above the columns, so the
  # two bounds have nothing over them but their own coverage -- the
  # header the continuous family already renders.
  groups <- spicy:::.cat_svy_header_layout(tbl)$spanners(
    names(attr(tbl, "display_df"))
  )
  last <- groups[[length(groups)]]
  expect_identical(last$cols, c(4L, 5L))
  expect_identical(last$label, "95% CI")
  expect_identical(last$bounds, c("LL", "UL"))
  # Everything to its left spans itself alone: stub, n, %.
  expect_identical(
    vapply(groups[-length(groups)], function(g) length(g$cols), integer(1)),
    c(1L, 1L, 1L)
  )

  # The resolver only runs on an EXPORT, so the same table is built
  # through one, end to end.
  skip_if_not_installed("tinytable")
  tt <- table_categorical_svy(
    des,
    c(stype, awards),
    proportion_ci = TRUE,
    output = "tinytable"
  )
  expect_true(inherits(tt, "tinytable"))
})


# ---- table_categorical_svy_print.R 234: glance() with no p column ----

test_that("glance() gives one row per variable and no p without `by`", {
  des <- .svycov_api(fpc = FALSE)
  tbl <- table_categorical_svy(des, c(stype, awards), proportion_ci = TRUE)
  g <- broom::glance(tbl)
  expect_identical(nrow(g), 2L)
  expect_identical(g$variable, c("stype", "awards"))
  # The comparison columns are undefined; everything descriptive is
  # populated -- the fixed schema a pipeline indexes by name.
  expect_identical(g$p.value, c(NA_real_, NA_real_))
  expect_identical(g$n_levels, c(3L, 2L))
  expect_equal(g$nobs, c(183, 183))
  expect_equal(g$degf, c(14, 14))

  # That schema does not depend on the p column BEING there. A table
  # whose p a pipeline dropped still glances, with the same columns
  # and the same undefined p -- the guard behind the promise above.
  stripped <- tbl
  stripped$p <- NULL
  expect_false("p" %in% names(stripped))
  g2 <- broom::glance(stripped)
  expect_identical(names(g2), names(g))
  expect_identical(g2$p.value, c(NA_real_, NA_real_))
  expect_identical(g2$n_levels, c(3L, 2L))
  expect_equal(g2$nobs, c(183, 183))
})


# ---- table_continuous_svy.R 752: `regex = TRUE` with no `select` ----

test_that("`regex = TRUE` without `select` summarises every numeric column", {
  des <- .svycov_api()
  out <- table_continuous_svy(des, regex = TRUE)
  numeric_cols <- names(des$variables)[
    vapply(des$variables, is.numeric, logical(1))
  ]
  expect_identical(unique(as.character(out$variable)), numeric_cols)
  # The missing `select` becomes ".*", not "nothing selected": the
  # design's own bookkeeping columns are numeric and are summarised
  # with the rest.
  expect_true(all(c("fpc", "pw") %in% out$variable))
  # A pattern narrows that default, which is what makes it a default.
  narrowed <- table_continuous_svy(des, select = "^api", regex = TRUE)
  expect_identical(
    unique(as.character(narrowed$variable)),
    c("api00", "api99", "api.stu")
  )
})


# ---- table_continuous_svy.R 768: `select` as a character vector ----

test_that("`select` accepts a character vector held in a variable", {
  des <- .svycov_api()
  sel <- c("api00", "api99")
  out <- table_continuous_svy(des, select = sel)
  expect_identical(unique(as.character(out$variable)), c("api00", "api99"))
  # The character door and the bare tidyselect door build the SAME
  # table, attributes included.
  expect_identical(out, table_continuous_svy(des, c(api00, api99)))
  # And the vector's order is the table's order.
  rev_sel <- c("api99", "api00")
  expect_identical(
    unique(as.character(table_continuous_svy(des, select = rev_sel)$variable)),
    c("api99", "api00")
  )
})


# ---- table_continuous_svy.R 1197: the caller's `excel_sheet` wins ----

test_that("a caller-supplied `excel_sheet` names the worksheet", {
  skip_if_not_installed("openxlsx2")
  des <- .svycov_api()
  path <- withr::local_tempfile(fileext = ".xlsx")
  out <- table_continuous_svy(
    des,
    c(api00, api99),
    output = "excel",
    excel_path = path,
    excel_sheet = "Design means"
  )
  expect_identical(out, path)
  expect_identical(
    unname(openxlsx2::wb_load(path)$get_sheet_names()),
    "Design means"
  )

  # Left alone, the sheet takes the registry's own name -- so the
  # assertion above really is the caller's name displacing a default,
  # not a name the function had nowhere else to get.
  default_path <- withr::local_tempfile(fileext = ".xlsx")
  table_continuous_svy(
    des,
    c(api00, api99),
    output = "excel",
    excel_path = default_path
  )
  expect_identical(
    unname(openxlsx2::wb_load(default_path)$get_sheet_names()),
    spicy:::spicy_str("excel_sheet_continuous")
  )
  expect_false(
    identical(spicy:::spicy_str("excel_sheet_continuous"), "Design means")
  )
})


# ---- table_continuous_svy_print.R 136: tidy() without `by` ----

test_that("tidy() keeps the `group` column, empty, on a table without `by`", {
  des <- .svycov_api()
  tbl <- table_continuous_svy(des, c(api00, api99))
  td <- broom::tidy(tbl)
  # The schema is fixed: `group` is there whether or not the table has
  # groups, so a pipeline can index it by name either way.
  expect_true("group" %in% names(td))
  expect_identical(td$group, c(NA_character_, NA_character_))
  expect_identical(td$variable, c("api00", "api99"))
  expect_identical(td$n, c(183L, 183L))
  # The estimates beside it are survey's own.
  expect_equal(
    td$estimate,
    c(644.16939890710387, 606.97814207650276),
    tolerance = 1e-12
  )
  expect_equal(
    td$std.error,
    c(23.542240693781036, 24.225040906133380),
    tolerance = 1e-12
  )
})


# ---- survey_helpers.R 620-621: the "(Missing)" display label dedup ----

test_that("a real `(Missing)` level and the missing domain stay distinct", {
  # `g` carries a genuine level literally spelled "(Missing)" AND true
  # NAs. With `drop_na = FALSE` the NA domain needs a display name and
  # the one it wants is taken, so it is numbered until it is free. The
  # real level keeps its own name, and neither column is lost to the
  # collision.
  skip_if_not_installed("survey")
  d <- data.frame(
    id = 1:60,
    w = 1,
    g = factor(c(rep("(Missing)", 20), rep("real", 20), rep(NA, 20))),
    v = factor(rep(c("x", "y"), 30))
  )
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  tbl <- table_categorical_svy(des, v, by = g, drop_na = FALSE)

  expect_identical(
    attr(tbl, "blocks"),
    c("(Missing)", "real", "(Missing_1)", "Total")
  )
  # Three domains of twenty units each, separately readable, plus the
  # margin: the renaming cost no column.
  lv <- tbl$.row_role == "level"
  expect_identical(tbl[["(Missing) n"]][lv], c(10L, 10L))
  expect_identical(tbl[["real n"]][lv], c(10L, 10L))
  expect_identical(tbl[["(Missing_1) n"]][lv], c(10L, 10L))
  expect_identical(tbl[["Total n"]][lv], c(30L, 30L))

  # The helper renames the DOMAIN, never the observed level, and the
  # scan that finds the collision covers declared levels as well.
  lvl <- spicy:::.svy_by_levels(d$g, drop_na = FALSE)
  expect_identical(lvl$levels, c("(Missing)", "real", "(Missing_1)"))
  expect_identical(lvl$missing_label, "(Missing_1)")
  expect_identical(lvl$n_na, 20L)
})


# ---- survey_helpers.R 267: no level to weigh ----

test_that(".drop_unweighted_levels() reads the weights, not the rows", {
  # An all-missing variable is refused upstream ("has no level to
  # display"), so the three clauses of this helper's contract are
  # pinned on the helper itself.

  # a. Nothing observed: `droplevels()` already leaves a factor with
  #    no level at all, and it is returned as it stands rather than
  #    summing weights over an empty level set.
  none <- spicy:::.drop_unweighted_levels(rep(NA, 5), rep(1, 5))
  expect_true(is.factor(none))
  expect_identical(nlevels(none), 0L)
  expect_true(all(is.na(none)))

  # b. A level the design retains at weight ZERO -- what `[` leaves
  #    behind on a calibrated design -- is emptied and then dropped,
  #    so the table built on it carries no all-zero row.
  v <- factor(c("a", "a", "b", "b", "c"))
  emptied <- spicy:::.drop_unweighted_levels(v, c(1, 1, 0, 0, 2))
  expect_identical(levels(emptied), c("a", "c"))
  expect_identical(is.na(emptied), c(FALSE, FALSE, TRUE, TRUE, FALSE))

  # c. With every row weighted this is `droplevels()` and nothing
  #    more: a declared level nobody chose goes, an observed one
  #    stays, and no observation is emptied.
  kept <- factor(c("a", "b"), levels = c("a", "b", "z"))
  expect_identical(
    spicy:::.drop_unweighted_levels(kept, c(1, 3)),
    droplevels(kept)
  )
})
