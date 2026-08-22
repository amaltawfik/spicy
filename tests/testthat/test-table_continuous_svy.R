# ---------------------------------------------------------------------------
# table_continuous_svy(): the design twin of table_continuous().
#
# The witnesses fall in four groups:
#   * ORACLES -- every displayed number is survey's own, pinned at 17
#     significant digits to tolerance 1e-12 (survey 4.5, `api`);
#   * the CONTINUITY THEOREM of decision 17 -- on a design that
#     declares nothing but weights, this table and
#     `table_continuous(weights =, rescale = TRUE)` are algebraically
#     the same mean and the same standard deviation;
#   * an INDEPENDENT oracle -- a six-row two-stratum design whose
#     linearised variance is an exact rational;
#   * the REFUSALS, one per branch.
# ---------------------------------------------------------------------------

.svyc_design <- function(which = "clus1") {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  switch(
    which,
    clus1 = survey::svydesign(
      id = ~dnum,
      weights = ~pw,
      data = apiclus1,
      fpc = ~fpc
    ),
    strat = survey::svydesign(
      id = ~1,
      strata = ~stype,
      weights = ~pw,
      data = apistrat,
      fpc = ~fpc
    ),
    rep1 = survey::as.svrepdesign(survey::svydesign(
      id = ~dnum,
      weights = ~pw,
      data = apiclus1,
      fpc = ~fpc
    )),
    # Weights and nothing else: the design on which the continuity
    # theorem of decision 17 is stated.
    iid = survey::svydesign(id = ~1, weights = ~pw, data = apiclus1),
    micro = survey::svydesign(
      id = ~psu,
      strata = ~st,
      weights = ~w,
      data = data.frame(
        st = c("A", "A", "A", "B", "B", "B"),
        psu = 1:6,
        y = c(10, 14, 12, 22, 26, 30),
        w = c(5, 5, 5, 2, 2, 2)
      )
    )
  )
}

.svyc_long <- function(...) {
  suppressWarnings(table_continuous_svy(..., output = "long"))
}

# ---- oracles --------------------------------------------------------------

test_that("every displayed number is survey's own (stratified design)", {
  out <- .svyc_long(
    .svyc_design("strat"),
    select = api00,
    show_columns = c("m", "sd", "se", "ci", "n", "weighted_n", "deff"),
    deff = TRUE
  )
  expect_equal(out$mean, 662.28736315932076, tolerance = 1e-12)
  expect_equal(out$se, 9.4089408027845831, tolerance = 1e-12)
  # The interval is the t one at degf(design) = 197, NOT the normal
  # `confint.svystat()` takes by default: those give
  # [643.84617805319363, 680.72854826544790], different in the second
  # decimal. This pair is the witness of that choice.
  expect_equal(out$ci_lower, 643.73218827208620, tolerance = 1e-12)
  expect_equal(out$ci_upper, 680.84253804655532, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(out$ci_lower, 643.84617805319363)))
  expect_equal(out$sd, 123.25009616964741, tolerance = 1e-12)
  expect_identical(out$n, 200L)
  expect_equal(out$weighted_n, 6193.9999580383301, tolerance = 1e-12)
  expect_equal(out$deff, 1.2044572685377175, tolerance = 1e-12)
  expect_identical(out$degf, 197)
})

test_that("`deff = \"replace\"` is the other design effect, and says so", {
  d <- .svyc_design("strat")
  out <- .svyc_long(d, select = api00, deff = "replace")
  expect_equal(out$deff, 1.1655661714535266, tolerance = 1e-12)
  tbl <- table_continuous_svy(d, select = api00, deff = "replace")
  expect_match(
    attr(tbl, "missing_note"),
    "WITH replacement",
    fixed = TRUE
  )
  # And the default form does not carry that sentence.
  expect_false(grepl(
    "WITH replacement",
    attr(table_continuous_svy(d, select = api00, deff = TRUE), "missing_note"),
    fixed = TRUE
  ))
})

test_that("every displayed number is survey's own (cluster design)", {
  out <- .svyc_long(
    .svyc_design("clus1"),
    select = api00,
    show_columns = c("m", "sd", "se", "ci", "med", "q1", "q3", "iqr", "n")
  )
  expect_equal(out$mean, 644.16939890710387, tolerance = 1e-12)
  expect_equal(out$se, 23.542240693781036, tolerance = 1e-12)
  expect_equal(out$ci_lower, 593.67631446332553, tolerance = 1e-12)
  expect_equal(out$ci_upper, 694.66248335088221, tolerance = 1e-12)
  expect_equal(out$sd, 105.74886663549471, tolerance = 1e-12)
  expect_identical(out$degf, 14)
  # `qrule = "math"` estimates inf{x : F(x) >= p}, the POPULATION
  # quantile: integers on integer data, by construction.
  expect_equal(c(out$q1, out$median, out$q3), c(552, 652, 719))
  expect_equal(out$iqr, 167)
})

test_that("`qrule` reaches survey, and \"spicy\" is the type-7 rule", {
  d <- .svyc_design("clus1")
  math <- .svyc_long(
    d,
    select = api00,
    show_columns = c("m", "med", "q1", "q3")
  )
  hf7 <- .svyc_long(
    d,
    select = api00,
    show_columns = c("m", "med", "q1", "q3"),
    qrule = "hf7"
  )
  spicy <- .svyc_long(
    d,
    select = api00,
    show_columns = c("m", "med", "q1", "q3"),
    qrule = "spicy"
  )
  expect_equal(c(hf7$q1, hf7$median, hf7$q3), c(552.5, 652, 718))
  # `"spicy"` is `.wtd_quantile7()` handed to survey as a FUNCTION, so
  # it reproduces `table_continuous(weights =)` exactly -- which is
  # the whole point of offering it, and is not the same as `"hf7"`.
  ref <- .wtd_quantile7(
    d$variables$api00,
    stats::weights(d),
    probs = c(0.25, 0.5, 0.75)
  )
  expect_equal(c(spicy$q1, spicy$median, spicy$q3), ref, tolerance = 1e-12)
  # And it is NOT survey's own "hf7": `.wtd_quantile7()` interpolates
  # on the CUMULATIVE-WEIGHT scale (the Hmisc algorithm decision 17
  # pinned), while survey's hf7 interpolates on the estimated CDF.
  # 552 against 552.5 on this fixture.
  expect_false(isTRUE(all.equal(spicy$q1, hf7$q1)))
  expect_equal(math$q1, spicy$q1)
  # A user function travels untouched.
  mine <- .svyc_long(
    d,
    select = api00,
    show_columns = c("m", "med"),
    qrule = function(x, w, p) rep(-1, length(p))
  )
  expect_equal(mine$median, -1)
  expect_match(
    attr(
      table_continuous_svy(
        d,
        select = api00,
        show_columns = c("m", "med"),
        qrule = function(x, w, p) rep(-1, length(p))
      ),
      "missing_note"
    ),
    'qrule = "<function>"',
    fixed = TRUE
  )
})

# ---- the continuity theorem of decision 17 --------------------------------

test_that("D17: on a weights-only design the two regimes are the same estimand", {
  # THE witness of this chantier. `survey:::svyvar()` computes
  # sum(w (x - xbar)^2) / sum(w) * n / (n - 1); `.wtd_sd()` on weights
  # rescaled to sum to n computes sum(w' (x - xbar)^2) / (sum(w') - 1),
  # and sum(w') = n makes the two the same expression. So the design
  # table and the rescaled weighted table must agree -- not
  # approximately, algebraically.
  #
  # tolerance 1e-12 rather than `expect_identical`: the two sums are
  # associated in a different order and a 1-ULP difference is a
  # property of floating point, not of the theorem.
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  d <- .svyc_design("iid")
  svy <- .svyc_long(d, select = c(api00, api99, ell))
  wtd <- table_continuous(
    apiclus1,
    select = c(api00, api99, ell),
    weights = pw,
    rescale = TRUE,
    output = "long"
  )
  expect_equal(svy$mean, wtd$mean, tolerance = 1e-12)
  expect_equal(svy$sd, wtd$sd, tolerance = 1e-12)
  # The observed count is the same on both sides too: survey counts
  # rows with a value and a non-zero weight, spicy counts rows with a
  # value, a weight and a positive one -- the same rows.
  expect_identical(svy$n, wtd$n)

  # The DEFAULT weighted regime is a different estimand, not a bug:
  # frequency-expansion divides by sum(w) - 1. Pinned so the boundary
  # cannot be erased by accident.
  freq <- table_continuous(
    apiclus1,
    select = c(api00, api99, ell),
    weights = pw,
    output = "long"
  )
  expect_false(isTRUE(all.equal(svy$sd, freq$sd, tolerance = 1e-6)))
  # The MEAN, though, is continuous across both regimes: sum(w x) /
  # sum(w) does not move when the weights are rescaled.
  expect_equal(svy$mean, freq$mean, tolerance = 1e-12)
})

test_that("D17 holds where the alignment of n had no reason to", {
  # The theorem is stated for any design, but its `n` -- survey's
  # "weight non-zero and not NA" against spicy's "value, weight, and
  # weight > 0" -- coincides for a reason that is easy to break.
  # Two cases where it had to be checked rather than assumed: zero
  # weights in the data, and missing values.
  skip_if_not_installed("survey")
  d <- data.frame(
    y = c(1, 2, 3, 4, 5, NA, 7, 8),
    w = c(2, 0, 1, 3, 0, 4, 1, 2)
  )
  des <- suppressWarnings(survey::svydesign(
    id = ~1,
    weights = ~w,
    data = d[d$w > 0, ]
  ))
  svy <- .svyc_long(des, select = y)
  wtd <- suppressWarnings(table_continuous(
    d,
    select = y,
    weights = w,
    rescale = TRUE,
    output = "long"
  ))
  expect_identical(svy$n, wtd$n)
  expect_equal(svy$mean, wtd$mean, tolerance = 1e-12)
  expect_equal(svy$sd, wtd$sd, tolerance = 1e-12)
})

# ---- an oracle that does not come from survey -----------------------------

test_that("the two-stratum micro design matches a hand linearisation", {
  # Six PSU, two strata, weights 5 and 2. The linearised variance is
  # the exact rational 492/441, so this witness would catch a survey
  # regression as well as one of ours.
  out <- .svyc_long(
    .svyc_design("micro"),
    select = y,
    show_columns = c("m", "sd", "se", "ci", "n", "weighted_n")
  )
  expect_equal(out$mean, 16)
  expect_equal(out$se, sqrt(492) / 21, tolerance = 1e-12)
  expect_equal(out$se^2, 492 / 441, tolerance = 1e-12)
  expect_identical(out$degf, 4)
  expect_equal(
    c(out$ci_lower, out$ci_upper),
    c(13.067403257405918, 18.932596742594082),
    tolerance = 1e-12
  )
  # sqrt(svyvar) = 53.942857142857143, which is NOT
  # sum(w (y - ybar)^2) / (sum(w) - 1) = 47.2 -- the estimand boundary
  # of decision 17, on a design where the two are far apart.
  expect_equal(out$sd, sqrt(53.942857142857143), tolerance = 1e-12)
  expect_equal(out$sd^2, 944 * 1.2 / 21, tolerance = 1e-12)
  expect_identical(out$n, 6L)
  expect_equal(out$weighted_n, 21)
})

# ---- replicate weights ----------------------------------------------------

test_that("a replicate design gives the same point estimate, its own SE", {
  lin <- .svyc_long(.svyc_design("clus1"), select = api00)
  rep <- .svyc_long(.svyc_design("rep1"), select = api00)
  expect_equal(rep$mean, lin$mean, tolerance = 1e-12)
  expect_equal(rep$mean, 644.16939890710387, tolerance = 1e-12)
  # The standard errors differ BY CONSTRUCTION: never pin them equal.
  expect_equal(rep$se, 26.329360589527614, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(rep$se, lin$se, tolerance = 1e-6)))
  expect_identical(rep$degf, 14)
  # The weighted count is the SAMPLING weight sum, not the first
  # replicate's (which is 2745 on this fixture).
  expect_equal(rep$weighted_n, 6194.0003242492676, tolerance = 1e-12)
  note <- attr(
    table_continuous_svy(.svyc_design("rep1"), select = api00),
    "missing_note"
  )
  expect_match(note, "replicate weights (JK1), 15 replicates", fixed = TRUE)
  expect_match(
    note,
    "Std. errors: Design-based (replicate weights, JK1).",
    fixed = TRUE
  )
})

# ---- domains --------------------------------------------------------------

test_that("`by` cuts one domain per group, each with its own df", {
  out <- .svyc_long(.svyc_design("clus1"), select = api00, by = stype)
  expect_identical(out$group, c("E", "H", "M"))
  # Point estimates and standard errors identical to `svyby()` -- the
  # route this table does NOT take. The twin loops on `[` because it
  # needs each domain's own degrees of freedom, which `svyby()` does
  # not return; that the two agree to the bit is what makes the
  # substitution legitimate.
  expect_equal(
    out$mean,
    c(648.86805555555554, 618.57142857142856, 631.44000000000005),
    tolerance = 1e-12
  )
  expect_equal(
    out$se,
    c(22.362408893831887, 38.020249359407529, 31.609465227245526),
    tolerance = 1e-12
  )
  # 14, 7 and 11: survey recomputes the df on the PSU each domain
  # retains, so a grouped table does not have ONE df.
  expect_identical(out$degf, c(14, 7, 11))
  expect_identical(out$n, c(144L, 14L, 25L))
})

test_that("a calibrated domain counts only the rows it kept", {
  # `[` on a CALIBRATED design does not drop the excluded rows: it
  # sets their probability to Inf, i.e. their weight to zero, and the
  # domain still has 183 rows. Every count of the twin therefore
  # filters on a positive weight -- the definition `survey:::svyvar()`
  # uses for its own `n`. Without that filter `n` would read 183 on
  # every row of this table, and `weighted_n` would be the whole
  # population three times over.
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  cal <- survey::calibrate(
    survey::svydesign(
      id = ~dnum,
      weights = ~pw,
      data = apiclus1,
      fpc = ~fpc
    ),
    ~stype,
    pop = c(`(Intercept)` = 6194, stypeH = 755, stypeM = 1018)
  )
  sub <- .design_subset(cal, cal$variables$stype == "E")
  expect_identical(nrow(sub), 183L)
  expect_identical(sum(.design_weights(sub) > 0), 144L)

  out <- .svyc_long(cal, select = api00, by = stype)
  expect_identical(out$n, c(144L, 14L, 25L))
  # The calibration targets, recovered: this is what a post-stratified
  # domain's weights sum to.
  expect_equal(out$weighted_n, c(4421, 755, 1018), tolerance = 1e-8)
  expect_match(
    attr(table_continuous_svy(cal, select = api00), "missing_note"),
    "calibrated / post-stratified",
    fixed = TRUE
  )
})

test_that("a negative calibration weight is an observation, not a hole", {
  # `survey:::svyvar()` counts `weights(design, "sampling") != 0`, and
  # LINEAR calibration routinely drives weights below zero -- that is
  # what `calibrate(bounds = )` exists to prevent. A `> 0` filter drops
  # those rows: it reported n = 155 on this design, a `Weighted n` of
  # 6591.54 contradicting the "weighted 6194" of its own footer, and a
  # `Max` of 789 for a sample whose maximum is 905. This is the only
  # fixture that separates the two predicates.
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  cal <- survey::calibrate(
    survey::svydesign(
      id = ~dnum,
      weights = ~pw,
      data = apiclus1,
      fpc = ~fpc
    ),
    ~api99,
    c(`(Intercept)` = 6194, api99 = 6194 * 500),
    calfun = "linear"
  )
  w <- .design_weights(cal)
  expect_identical(sum(w < 0), 28L)
  expect_equal(min(w), -47.064232723491621, tolerance = 1e-12)

  out <- .svyc_long(
    cal,
    select = api00,
    show_columns = c("m", "sd", "se", "min", "max", "n", "weighted_n")
  )
  # survey's own n on this design is 183, and the twin now agrees.
  expect_identical(
    out$n,
    sum(w != 0 & !is.na(cal$variables$api00))
  )
  expect_identical(out$n, 183L)
  expect_equal(out$weighted_n, 6194.0000000000009, tolerance = 1e-12)
  expect_equal(out$max, 905)
  expect_equal(out$min, 411)
  # The moments never moved -- survey always computed them on all 183
  # rows, which is exactly why nothing alerted the reader.
  expect_equal(out$mean, 547.43008125342669, tolerance = 1e-12)
  expect_equal(out$sd, 43.545833331153375, tolerance = 1e-12)
  expect_equal(out$se, 4.3269670987437925, tolerance = 1e-12)
  # And the cell now agrees with the footer of its own table.
  expect_match(
    attr(table_continuous_svy(cal, select = api00), "missing_note"),
    "N = 183 (weighted 6194).",
    fixed = TRUE
  )
})

test_that("the footer gives the df span when the groups disagree", {
  tbl <- table_continuous_svy(.svyc_design("clus1"), select = api00, by = stype)
  expect_match(
    attr(tbl, "missing_note"),
    "degrees of freedom vary by group (7 to 14)",
    fixed = TRUE
  )
  # A one-way table has one df and must say so as a number.
  expect_match(
    attr(
      table_continuous_svy(.svyc_design("clus1"), select = api00),
      "missing_note"
    ),
    "14 degrees of freedom",
    fixed = TRUE
  )
})

test_that("`drop_na = FALSE` keeps the missing values as their own domain", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dat <- apiclus1
  dat$stype[1:3] <- NA
  des <- survey::svydesign(id = ~dnum, weights = ~pw, data = dat, fpc = ~fpc)
  out <- .svyc_long(des, select = api00, by = stype, drop_na = FALSE)
  expect_identical(out$group, c("E", "H", "M", "(Missing)"))
  expect_identical(out$n, c(142L, 13L, 25L, 3L))
  # The three rows fall in ONE cluster, so the domain has no degrees of
  # freedom and therefore no interval -- a dash, not an interval built
  # on `qt(p, df = 0)`.
  expect_identical(out$degf[[4L]], 0)
  expect_true(is.na(out$ci_lower[[4L]]))
  expect_false(is.na(out$mean[[4L]]))
  # The role is the KEY, not the label: a translated or auto-renamed
  # "(Missing)" still reads as `missing` in the typed view.
  tbl <- table_continuous_svy(des, select = api00, by = stype, drop_na = FALSE)
  expect_identical(
    as_structured(tbl)$body$.row_role,
    c("group", "group", "group", "missing")
  )
  # And the comparison runs on the OBSERVED groups only: the same
  # p-value as the `drop_na = TRUE` table.
  expect_equal(
    out$p.value[[1L]],
    .svyc_long(des, select = api00, by = stype)$p.value[[1L]],
    tolerance = 1e-12
  )
})

test_that("the sample-size sentence counts the analytic sample", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dat <- apiclus1
  dat$stype[1:3] <- NA
  des <- survey::svydesign(id = ~dnum, weights = ~pw, data = dat, fpc = ~fpc)
  # 180, not the 183 the design was built on: the three rows left.
  expect_match(
    attr(table_continuous_svy(des, select = api00, by = stype), "missing_note"),
    "N = 180 (weighted 6092).",
    fixed = TRUE
  )
})

test_that("rows with a missing `by` leave the table and the note says so", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dat <- apiclus1
  dat$stype[1:3] <- NA
  des <- survey::svydesign(id = ~dnum, weights = ~pw, data = dat, fpc = ~fpc)
  tbl <- table_continuous_svy(des, select = api00, by = stype)
  expect_match(
    attr(tbl, "missing_note"),
    "Rows with missing stype removed: 3.",
    fixed = TRUE
  )
  # No "(Missing)" domain: a level the sampler never assigned is not a
  # domain, and its degrees of freedom would be an artefact.
  expect_identical(sort(unique(tbl$group)), c("E", "H", "M"))
})

# ---- the group comparison -------------------------------------------------

test_that("two groups take the design-based t-test, three the Wald F", {
  d <- .svyc_design("clus1")
  two <- .svyc_long(d, select = api00, by = sch.wide, statistic = TRUE)
  expect_identical(two$test_type[[1L]], "design_t")
  expect_equal(two$statistic[[1L]], 2.108989847792919203, tolerance = 1e-12)
  expect_equal(two$df1[[1L]], 13)
  expect_true(is.na(two$df2[[1L]]))
  expect_equal(two$p.value[[1L]], 0.054908817089046651, tolerance = 1e-12)

  three <- .svyc_long(d, select = api00, by = stype, statistic = TRUE)
  expect_identical(three$test_type[[1L]], "design_f")
  expect_equal(three$statistic[[1L]], 1.27820074251777616, tolerance = 1e-12)
  expect_equal(c(three$df1[[1L]], three$df2[[1L]]), c(2, 12))
  expect_equal(three$p.value[[1L]], 0.31387976824321751, tolerance = 1e-12)

  # The two-group shortcut is a RENDERING choice, not a second method:
  # `regTermTest()` on the same design gives F = t^2 on (1, 13).
  fit <- survey::svyglm(api00 ~ sch.wide, design = d)
  rt <- survey::regTermTest(fit, ~sch.wide)
  expect_equal(
    as.numeric(rt$Ftest),
    two$statistic[[1L]]^2,
    tolerance = 1e-12
  )
  expect_equal(as.numeric(rt$p), two$p.value[[1L]], tolerance = 1e-12)

  # Only the first row of a variable block carries the comparison.
  expect_true(all(is.na(three$p.value[-1L])))
})

test_that("the rank tests are survey's, including the documented shape swap", {
  d <- .svyc_design("clus1")
  two <- .svyc_long(
    d,
    select = api00,
    by = sch.wide,
    test = "nonparametric",
    statistic = TRUE
  )
  expect_identical(two$test_type[[1L]], "design_t")
  expect_equal(two$statistic[[1L]], 2.229818667888981754, tolerance = 1e-12)
  expect_equal(two$df1[[1L]], 13)
  expect_equal(two$p.value[[1L]], 0.044015702394679203, tolerance = 1e-12)

  # Three groups: `?svyranktest` says `statistic` holds the NUMERATOR
  # DF and `parameter` the statistic. Reading `$statistic` naively
  # would put 2 in the statistic column and the statistic in the df --
  # the pin below is what makes that swap visible.
  three <- .svyc_long(
    d,
    select = api00,
    by = stype,
    test = "nonparametric",
    statistic = TRUE
  )
  expect_identical(three$test_type[[1L]], "design_f")
  expect_equal(
    three$statistic[[1L]],
    2.22397482137552860 / 2,
    tolerance = 1e-12
  )
  expect_equal(c(three$df1[[1L]], three$df2[[1L]]), c(2, 12))
  expect_equal(three$p.value[[1L]], 0.36054678485732611, tolerance = 1e-12)
})

test_that("`test = \"student\"` warns and behaves like the design t-test", {
  d <- .svyc_design("clus1")
  expect_warning(
    stu <- table_continuous_svy(
      d,
      select = api00,
      by = sch.wide,
      test = "student",
      output = "long"
    ),
    class = "spicy_ignored_arg"
  )
  wel <- .svyc_long(d, select = api00, by = sch.wide, test = "welch")
  expect_equal(stu$p.value, wel$p.value, tolerance = 1e-12)
})

test_that("a table showing a median without a mean takes the rank test", {
  d <- .svyc_design("clus1")
  out <- .svyc_long(
    d,
    select = api00,
    by = sch.wide,
    show_columns = c("med_iqr", "n")
  )
  # 2.2298... is svyranktest's; 2.1089... would be svyttest's.
  expect_equal(out$p.value[[1L]], 0.044015702394679203, tolerance = 1e-12)
})

test_that("a declared but unobserved `by` level does not reach the test", {
  # `droplevels()` in `.svy_group_test()` carries the whole "observed
  # groups" rule -- it is why `note_design_df_test_differs` exists --
  # and no fixture separated it from its absence: removing it stayed
  # green on all 90 blocks. A factor whose level nobody chose is the
  # only shape that tells the two apart, and without the call the
  # comparison sees three levels, one of them empty, and returns
  # nothing at all.
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dat <- apiclus1
  dat$g <- factor(
    ifelse(dat$sch.wide == "Yes", "a", "b"),
    levels = c("a", "b", "ghost")
  )
  des <- survey::svydesign(id = ~dnum, weights = ~pw, data = dat, fpc = ~fpc)
  out <- .svyc_long(des, select = api00, by = g, statistic = TRUE)

  # The ghost level is not a domain either: it has no observation, so
  # it is not a row.
  expect_identical(out$group, c("a", "b"))
  # And the test is the two-group one, identical to survey's on the
  # observed levels.
  ref <- survey::svyttest(
    api00 ~ droplevels(g),
    survey::svydesign(id = ~dnum, weights = ~pw, data = dat, fpc = ~fpc)
  )
  expect_identical(out$test_type[[1L]], "design_t")
  expect_equal(
    out$statistic[[1L]],
    as.numeric(ref$statistic),
    tolerance = 1e-12
  )
  expect_equal(out$statistic[[1L]], -2.108989847792919203, tolerance = 1e-12)
  expect_equal(out$df1[[1L]], 13)
  expect_equal(out$p.value[[1L]], 0.054908817089046651, tolerance = 1e-12)
  # The column is alive: without `droplevels()` it is entirely NA.
  expect_false(is.na(out$p.value[[1L]]))
})

test_that("a group too thin to compare leaves the test columns empty", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dat <- apiclus1
  # One school in its own group: no within-group variance to test.
  dat$g <- "a"
  dat$g[1L] <- "b"
  des <- survey::svydesign(id = ~dnum, weights = ~pw, data = dat, fpc = ~fpc)
  out <- suppressWarnings(.svyc_long(des, select = api00, by = g))
  expect_true(all(is.na(out$p.value)))
  expect_equal(nrow(out), 2L)
})

# ---- degrees of freedom supplied by the caller ----------------------------

test_that("`df` moves the intervals, and only the intervals", {
  # `df` reaches `confint()`. It cannot reach the group comparison:
  # `svyttest()` and `svyranktest()` have no `df` argument. The note
  # used to promise both, which put three different numbers in one
  # paragraph -- the supplied df, the domain's, and the one the cell
  # printed.
  d <- .svyc_design("clus1")
  out <- .svyc_long(d, select = api00, df = 100)
  expect_identical(out$degf, 100)
  ref <- as.numeric(stats::confint(survey::svymean(~api00, d), df = 100))
  expect_equal(c(out$ci_lower, out$ci_upper), ref, tolerance = 1e-12)

  # The CELL: the interval moves, the p does not.
  moved <- .svyc_long(
    d,
    select = api00,
    by = sch.wide,
    df = 3,
    statistic = TRUE
  )
  plain <- .svyc_long(d, select = api00, by = sch.wide, statistic = TRUE)
  expect_false(isTRUE(all.equal(moved$ci_lower[[1L]], plain$ci_lower[[1L]])))
  expect_equal(moved$p.value[[1L]], plain$p.value[[1L]], tolerance = 1e-12)
  expect_equal(
    moved$p.value[[1L]],
    0.054908817089046651,
    tolerance = 1e-12
  )

  # The FOOTER: three sentences, three truths. The design line states
  # the DESIGN's own degrees of freedom -- a fact the caller cannot
  # change -- the interval sentence names the supplied number, and the
  # comparison sentence names the df the test actually used, which is
  # the 13 the cell shows and not the domain's 14.
  note <- attr(
    table_continuous_svy(
      d,
      select = api00,
      by = sch.wide,
      df = 3,
      statistic = TRUE
    ),
    "missing_note"
  )
  expect_match(note, "degrees of freedom vary by group (9 to 14)", fixed = TRUE)
  expect_match(
    note,
    "Confidence intervals use 3 degrees of freedom (supplied in `df`); the tests use the design's own.",
    fixed = TRUE
  )
  expect_match(
    note,
    "The group comparison uses 13 degrees of freedom",
    fixed = TRUE
  )
  expect_false(grepl("use the design degrees of freedom", note, fixed = TRUE))
  expect_identical(moved$df1[[1L]], 13)
})

test_that("`df` does not reach the categorical test either", {
  d <- .svyc_design("clus1")
  a <- suppressWarnings(table_categorical_svy(
    d,
    select = stype,
    by = sch.wide,
    output = "long"
  ))
  b <- suppressWarnings(table_categorical_svy(
    d,
    select = stype,
    by = sch.wide,
    df = 3,
    output = "long"
  ))
  expect_equal(a$p[[1L]], b$p[[1L]], tolerance = 1e-12)
  expect_match(
    attr(
      table_categorical_svy(d, select = stype, by = sch.wide, df = 3),
      "note"
    ),
    "the tests use the design's own",
    fixed = TRUE
  )
})

test_that("a domain with no degrees of freedom has no estimable variance", {
  # `svymean()` returns SE = 0 on a single-PSU domain: there is no
  # between-unit variation to measure. Printed as "0.00" beside a
  # dashed interval it reads as a perfect estimate, which is the
  # opposite of the truth. The standard error, the interval and the
  # design effect go undefined together; the mean and the count stay.
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dat <- apiclus1
  dat$stype[1:3] <- NA
  des <- survey::svydesign(id = ~dnum, weights = ~pw, data = dat, fpc = ~fpc)
  out <- .svyc_long(
    des,
    select = api00,
    by = stype,
    drop_na = FALSE,
    deff = TRUE
  )
  expect_identical(out$degf[[4L]], 0)
  expect_false(is.na(out$mean[[4L]]))
  expect_identical(out$n[[4L]], 3L)
  expect_true(is.na(out$se[[4L]]))
  expect_true(is.na(out$ci_lower[[4L]]))
  expect_true(is.na(out$deff[[4L]]))
  # The estimable domains keep all three.
  expect_false(is.na(out$se[[1L]]))
  expect_false(is.na(out$deff[[1L]]))
})

# ---- refusals -------------------------------------------------------------

test_that("the design-only refusals fire, one per branch", {
  d <- .svyc_design("clus1")
  expect_error(
    table_continuous_svy(mtcars, select = mpg),
    class = "spicy_wrong_regime"
  )
  expect_error(
    table_continuous_svy(
      d,
      select = api00,
      show_columns = c("m", "med", "med_ci")
    ),
    class = "spicy_invalid_input"
  )
  expect_match(
    conditionMessage(expect_error(
      table_continuous_svy(
        d,
        select = api00,
        show_columns = c("m", "med", "med_ci")
      )
    )),
    "binomial sign test",
    fixed = TRUE
  )
  expect_error(
    table_continuous_svy(d, select = api00, show_columns = c("m", "deff")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = api00, deff = "nope"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = api00, df = -1),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = api00, qrule = "hf99"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = api00, qrule = 7),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = api00, show_columns = list(api00 = "m")),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = api00, ci_level = 2),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = api00, digits = -1),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = api00, p_digits = 0),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = api00, decimal_mark = "--"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = api00, labels = "x"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = api00, statistic = NA),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = api00, p_value = "yes"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = stype),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = api00, by = c(stype, sch.wide)),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(d, select = 1, regex = TRUE),
    class = "spicy_invalid_input"
  )
})

test_that("an unsupported design class is refused by name", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  tw <- survey::twophase(
    id = list(~1, ~1),
    subset = ~ I(comp.imp == "Yes"),
    data = apiclus1
  )
  expect_error(
    table_continuous_svy(tw, select = api00),
    class = "spicy_unsupported"
  )
})

test_that("the ignored-argument warnings fire where they should", {
  d <- .svyc_design("clus1")
  expect_warning(
    table_continuous_svy(d, select = api00, statistic = TRUE),
    class = "spicy_ignored_arg"
  )
  expect_warning(
    table_continuous_svy(
      d,
      select = api00,
      by = stype,
      test = "welch",
      p_value = FALSE
    ),
    class = "spicy_ignored_arg"
  )
  expect_warning(
    table_continuous_svy(
      d,
      select = api00,
      show_columns = c("m", "sd"),
      show_n = TRUE
    ),
    class = "spicy_ignored_arg"
  )
  expect_warning(
    table_continuous_svy(
      d,
      select = api00,
      show_columns = c("m", "sd"),
      ci = TRUE
    ),
    class = "spicy_ignored_arg"
  )
  # An interval without the statistic it bounds is pruned, loudly.
  expect_warning(
    out <- table_continuous_svy(
      d,
      select = api00,
      show_columns = c("sd", "ci"),
      output = "long"
    ),
    class = "spicy_ignored_arg"
  )
  # The pruned token leaves the SD alone on display, while `ci_lower`
  # stays a field of the compute schema -- a stable frame a pipeline
  # can index into, holding the value the table does not show.
  expect_true("ci_lower" %in% names(out))
  expect_identical(
    attr(
      suppressWarnings(table_continuous_svy(
        d,
        select = api00,
        show_columns = c("sd", "ci")
      )),
      "show_columns"
    ),
    "sd"
  )
})

test_that("`regex` and `exclude` select the way the sibling does", {
  d <- .svyc_design("clus1")
  out <- .svyc_long(d, select = "^api[0-9]", regex = TRUE)
  expect_identical(out$variable, c("api00", "api99"))
  out2 <- .svyc_long(d, select = c(api00, api99), exclude = api99)
  expect_identical(out2$variable, "api00")
  expect_message(
    table_continuous_svy(d, select = c(api00, stype), verbose = TRUE),
    "Ignored non-numeric columns"
  )
})

# ---- declared missing values ----------------------------------------------

test_that("declared missing codes leave the statistics and the note", {
  skip_if_not_installed("survey")
  skip_if_not_installed("haven")
  d <- data.frame(w = rep(1, 6))
  d$y <- haven::labelled_spss(
    c(1, 2, 3, 99, 5, 4),
    labels = c(missing = 99),
    na_values = 99
  )
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  out <- .svyc_long(des, select = y)
  expect_identical(out$n, 5L)
  expect_equal(out$mean, 3)
  tbl <- table_continuous_svy(des, select = y)
  expect_match(
    attr(tbl, "missing_note"),
    "Declared missing values removed: y (1).",
    fixed = TRUE
  )
  # `user_na = FALSE` keeps the code as an ordinary number.
  out2 <- .svyc_long(des, select = y, user_na = FALSE)
  expect_identical(out2$n, 6L)
  expect_equal(out2$mean, 19)
})

test_that("ordinary missing values are counted in the note", {
  skip_if_not_installed("survey")
  d <- data.frame(y = c(1, 2, NA, 4), w = rep(1, 4))
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  tbl <- table_continuous_svy(des, select = y)
  expect_match(
    attr(tbl, "missing_note"),
    "Missing values removed: y (1).",
    fixed = TRUE
  )
  expect_identical(tbl$n, 3L)
})

# ---- restitution ----------------------------------------------------------

test_that("the console table prints its title, its columns and its footer", {
  d <- .svyc_design("clus1")
  expect_snapshot(table_continuous_svy(d, select = c(api00, api99)))
  expect_snapshot(table_continuous_svy(
    d,
    select = api00,
    by = stype,
    statistic = TRUE
  ))
  expect_snapshot(
    table_continuous_svy(
      d,
      select = api00,
      show_columns = c("m", "se", "med_iqr", "n", "weighted_n", "deff"),
      deff = TRUE
    )
  )
})

test_that("every rendering engine accepts a design table", {
  d <- .svyc_design("clus1")
  for (eng in c("tinytable", "gt", "flextable")) {
    skip_if_not_installed(eng)
  }
  # tinytable is S4, unlike the other engines' objects.
  expect_s4_class(
    table_continuous_svy(d, select = api00, by = stype, output = "tinytable"),
    "tinytable"
  )
  expect_s3_class(
    table_continuous_svy(d, select = api00, by = stype, output = "gt"),
    "spicy_gt"
  )
  expect_s3_class(
    table_continuous_svy(d, select = api00, by = stype, output = "flextable"),
    "spicy_flextable"
  )
  skip_if_not_installed("openxlsx2")
  xl <- withr::local_tempfile(fileext = ".xlsx")
  expect_type(
    table_continuous_svy(d, select = api00, output = "excel", excel_path = xl),
    "character"
  )
  expect_true(file.exists(xl))
  skip_if_not_installed("officer")
  doc <- withr::local_tempfile(fileext = ".docx")
  expect_type(
    table_continuous_svy(d, select = api00, output = "word", word_path = doc),
    "character"
  )
  expect_true(file.exists(doc))
})

test_that("the clipboard route is reachable and refuses without clipr", {
  d <- .svyc_design("clus1")
  skip_if_not_installed("clipr")
  local_mocked_bindings(
    clipr_available = function(...) FALSE,
    .package = "clipr"
  )
  expect_error(
    table_continuous_svy(d, select = api00, output = "clipboard"),
    class = "spicy_unsupported"
  )
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(content, ...) {
      captured$text <- content
      invisible(content)
    },
    .package = "clipr"
  )
  table_continuous_svy(d, select = api00, output = "clipboard")
  expect_match(captured$text, "644", fixed = TRUE)
  expect_match(captured$text, "Descriptive statistics", fixed = TRUE)
})

test_that("the typed view carries the design tokens and the row identity", {
  d <- .svyc_design("clus1")
  tbl <- table_continuous_svy(d, select = api00, by = stype, deff = TRUE)
  s <- as_structured(tbl)
  expect_identical(s$version, 3L)
  expect_identical(s$body$.row_role, rep("group", 3L))
  expect_identical(s$body$.level, c("E", "H", "M"))
  expect_identical(s$body$.variable, rep("api00", 3L))
  expect_identical(s$col_meta[[.CON_KEY_DEFF]]$token, "deff")
  expect_identical(s$col_meta[[.CON_KEY_DEFF]]$display_label, "DEff")
  # Each group's DEff is the design effect of its own domain -- the
  # `svyby()` route would have needed `deff()` on an svyby object,
  # whose positional indexing is wrong as soon as "ci" is in vartype.
  expect_equal(s$body$DEff[[1L]], 6.5836735250278018, tolerance = 1e-12)
  expect_equal(
    s$body[["95% CI LL"]][[1L]],
    600.90545865029026,
    tolerance = 1e-12
  )
  expect_identical(s$ci_pairs[[1L]]$label, "95% CI")
})

test_that("`inline()` cites a cell of a design table", {
  d <- .svyc_design("clus1")
  tbl <- table_continuous_svy(
    d,
    select = api00,
    by = stype,
    show_columns = c("m", "se", "ci", "n"),
    deff = TRUE
  )
  expect_identical(inline(tbl, "api00", level = "E", column = "m"), "648.87")
  expect_identical(inline(tbl, "api00", level = "E", column = "se"), "22.36")
  expect_identical(
    inline(tbl, "api00", level = "E", column = "ci"),
    "[600.91, 696.83]"
  )
  # The omnibus p sits on the FIRST group row, so it is cited with
  # that level -- the same addressing `table_continuous()` needs, and
  # for the same reason: this family puts the comparison on a row of
  # the block rather than on a header of its own.
  expect_identical(inline(tbl, "api00", level = "E", column = "p"), ".314")
})

test_that("`output = \"data.frame\"` and `\"long\"` are the same frame", {
  d <- .svyc_design("clus1")
  a <- table_continuous_svy(
    d,
    select = api00,
    by = stype,
    output = "data.frame"
  )
  b <- table_continuous_svy(d, select = api00, by = stype, output = "long")
  expect_identical(a, b)
  expect_s3_class(a, "data.frame")
  expect_false(inherits(a, "spicy_continuous_svy_table"))
})

test_that("broom tidy() and glance() read a design table", {
  # The five parent classes register both; the twins registered
  # neither, so `broom::tidy()` fell through to the deprecated
  # `tidy.data.frame` and died with a message naming neither spicy nor
  # the class.
  skip_if_not_installed("broom")
  d <- .svyc_design("clus1")
  tbl <- table_continuous_svy(
    d,
    select = c(api00, api99),
    by = stype,
    deff = TRUE
  )
  td <- broom::tidy(tbl)
  expect_s3_class(td, "data.frame")
  expect_identical(nrow(td), 6L)
  expect_true(all(
    c(
      "variable",
      "label",
      "group",
      "estimate",
      "std.error",
      "conf.low",
      "conf.high",
      "df",
      "n",
      "weighted.n",
      "deff"
    ) %in%
      names(td)
  ))
  # `std.error` is survey's design-based SE, never `sd / sqrt(n)`:
  # under a design those are different quantities.
  expect_equal(td$std.error[[1L]], 22.362408893831887, tolerance = 1e-12)
  expect_equal(td$estimate[[1L]], 648.86805555555554, tolerance = 1e-12)
  expect_identical(td$df, c(14, 7, 11, 14, 7, 11))

  gl <- broom::glance(tbl)
  expect_identical(nrow(gl), 2L)
  expect_equal(gl$p.value[[1L]], 0.31387976824321751, tolerance = 1e-12)
  expect_identical(gl$test_type[[1L]], "design_f")
  expect_identical(gl$degf, c(14, 14))
  expect_identical(gl$nobs, c(183, 183))

  # A one-way table has the same schema with the comparison columns NA.
  gl1 <- broom::glance(table_continuous_svy(d, select = api00))
  expect_true(all(is.na(c(gl1$p.value, gl1$statistic, gl1$test_type))))
  expect_identical(gl1$nobs, 183)
})

test_that("coercion keeps the frame and the provenance markers", {
  d <- .svyc_design("clus1")
  tbl <- table_continuous_svy(d, select = api00, by = stype)
  df <- as.data.frame(tbl)
  expect_false(inherits(df, "spicy_continuous_svy_table"))
  expect_identical(attr(df, "group_var"), "stype")
  expect_identical(attr(df, "design_meta")$degf, 14)
  skip_if_not_installed("tibble")
  tb <- tibble::as_tibble(tbl)
  expect_s3_class(tb, "tbl_df")
  expect_identical(nrow(tb), 3L)
})

test_that("`labels` renames the stub, and a journal style reaches the cells", {
  d <- .svyc_design("clus1")
  tbl <- table_continuous_svy(
    d,
    select = api00,
    labels = c(api00 = "API 2000")
  )
  expect_true(any(grepl("API 2000", unlist(tbl), fixed = TRUE)))
  eu <- table_continuous_svy(
    d,
    select = api00,
    decimal_mark = ",",
    output = "long"
  )
  expect_equal(eu$mean, 644.16939890710387, tolerance = 1e-12)
  styled <- table_continuous_svy(d, select = api00, by = stype, style = "jama")
  expect_s3_class(styled, "spicy_continuous_svy_table")
})
