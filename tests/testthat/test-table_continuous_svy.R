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
#     linearized variance is an exact rational;
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
    ),
    # Raking-style calibration on a factor: every weight stays
    # positive. The control for the fixture below.
    cal = survey::calibrate(
      survey::svydesign(
        id = ~dnum,
        weights = ~pw,
        data = apiclus1,
        fpc = ~fpc
      ),
      ~stype,
      pop = c(`(Intercept)` = 6194, stypeH = 755, stypeM = 1018)
    )
  )
}

# LINEAR calibration on a continuous total: 28 of the 183 weights come
# out below zero. The only fixture that separates `w != 0` from
# `w > 0`, and the one the negative-weight disclosure is measured on.
.svycneg_design <- function() {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  survey::calibrate(
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
}

.svyc_long <- function(...) {
  suppressWarnings(table_continuous_svy(..., output = "long"))
}

# ---- oracles --------------------------------------------------------------

test_that("every displayed number is survey's own (stratified design)", {
  skip_if_not_installed("survey", "4.5")
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
    attr(tbl, "note"),
    "WITH replacement",
    fixed = TRUE
  )
  # And the default form does not carry that sentence.
  expect_false(grepl(
    "WITH replacement",
    attr(table_continuous_svy(d, select = api00, deff = TRUE), "note"),
    fixed = TRUE
  ))
})

test_that("every displayed number is survey's own (cluster design)", {
  skip_if_not_installed("survey", "4.5")
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
      "note"
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

test_that("the two-stratum micro design matches a hand linearization", {
  # Six PSU, two strata, weights 5 and 2. The linearized variance is
  # the exact rational 492/441, so this witness would catch a survey
  # regression as well as one of ours.
  skip_if_not_installed("survey", "4.5")
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
  skip_if_not_installed("survey", "4.5")
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
    "note"
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
  skip_if_not_installed("survey", "4.5")
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
    attr(table_continuous_svy(cal, select = api00), "note"),
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
    attr(table_continuous_svy(cal, select = api00), "note"),
    "N = 183 (weighted 6194).",
    fixed = TRUE
  )
})

test_that("negative weights refuse the test, keep the cells, and say so", {
  # Decision 36 / ARB-2 and ARB-3. The group comparison used to filter
  # on `w > 0`: the cells were computed on 183 rows and the p-value on
  # 96, under a footer saying "N = 183" and no word about either.
  skip_if_not_installed("survey")
  d <- .svycneg_design()
  w <- .design_weights(d)
  expect_identical(sum(w < 0), 28L)

  out <- .svyc_long(d, select = api00, by = stype, show_columns = c("m", "n"))
  # The cells are complete: every row with a non-zero weight.
  expect_identical(sum(out$n), 183L)
  # The test is refused, not computed on a sub-sample.
  expect_identical(unique(out$p.value), NA_real_)

  # The refusal is CLASSED (ARB-2): a note is read by whoever prints
  # the table, a condition is what a script can catch.
  expect_warning(
    tbl <- table_continuous_svy(d, select = api00, by = stype),
    class = "spicy_negative_weights_no_test"
  )
  note <- attr(tbl, "note")
  # One block: the exact count, the consequence, and the refusal.
  expect_match(note, "gave 28 of 183 rows a negative weight", fixed = TRUE)
  expect_match(note, "fall outside the observed range", fixed = TRUE)
  expect_match(note, "group comparison is not reported", fixed = TRUE)
  # And the test sentence a normal table carries is absent -- the note
  # never claims a comparison it refused.
  expect_false(grepl("Group comparison:|P-value", note))
})

test_that("the refusal clause says how far the refusal reached", {
  # The footer clause used to be decided per TABLE ("was a test asked
  # for?") while the refusal is decided per VARIABLE. A variable whose
  # missing values happen to cover the negatively weighted rows has a
  # testable domain and IS tested -- and the note carried the flat
  # "The group comparison is not reported" over a printed p-value.
  skip_if_not_installed("survey")
  d <- .svycneg_design()
  w <- .design_weights(d)
  # The second variable: api00 with NA on exactly the 28 negative rows.
  d$variables$api00b <- d$variables$api00
  d$variables$api00b[w < 0] <- NA

  # (c) MIXED: one comparison served, one refused. The condition says
  # the same scoped sentence the footer says -- not the flat one.
  expect_warning(
    mixed <- table_continuous_svy(
      d,
      select = c(api00, api00b),
      by = stype,
      output = "long"
    ),
    regexp = "For variables whose complete cases include negatively weighted rows",
    fixed = TRUE,
    class = "spicy_negative_weights_no_test"
  )
  served <- mixed$p.value[mixed$variable == "api00b"]
  expect_false(all(is.na(served)))
  expect_true(all(is.na(mixed$p.value[mixed$variable == "api00"])))
  note <- attr(mixed, "note")
  # The method line is TRUE of the comparison that ran, so it stays --
  # and the refusal names who it applies to instead of contradicting it.
  # One p-value shown: the singular.
  expect_match(note, "P-value: design-based", fixed = TRUE)
  expect_match(
    note,
    "For variables whose complete cases include negatively weighted rows",
    fixed = TRUE
  )
  expect_false(grepl(
    "The group comparison is not reported",
    note,
    fixed = TRUE
  ))

  # (b) ALL REFUSED, two variables: the flat sentence, no method line,
  # and ONE condition for the table -- not one per variable.
  n_signalled <- 0L
  all_ref <- withCallingHandlers(
    table_continuous_svy(
      d,
      select = c(api00, api99),
      by = stype,
      output = "long"
    ),
    spicy_negative_weights_no_test = function(cnd) {
      n_signalled <<- n_signalled + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(n_signalled, 1L)
  expect_true(all(is.na(all_ref$p.value)))
  note_all <- attr(all_ref, "note")
  expect_match(note_all, "The group comparison is not reported", fixed = TRUE)
  expect_false(grepl("Group comparison:|P-value", note_all))
  expect_false(grepl("For variables whose", note_all, fixed = TRUE))
})

test_that("a comparison absent for another reason is not blamed on the weights", {
  # The milder half of the same defect: with negative weights present
  # AND fewer than two usable groups, the comparison is missing for a
  # reason that has nothing to do with the sign of the weights. The
  # note used to explain it by them anyway.
  skip_if_not_installed("survey")
  d <- .svycneg_design()
  w <- .design_weights(d)
  # One group after the complete cases: nothing to compare.
  d$variables$one <- factor("only")
  d$variables$api00b <- d$variables$api00
  d$variables$api00b[w < 0] <- NA

  out <- withCallingHandlers(
    table_continuous_svy(d, select = api00b, by = one, output = "long"),
    spicy_negative_weights_no_test = function(cnd) {
      stop("the weights refused nothing here")
    }
  )
  expect_true(all(is.na(out$p.value)))
  note <- attr(out, "note")
  # The fact is still disclosed; the refusal clause is not.
  expect_match(note, "gave 28 of 183 rows a negative weight", fixed = TRUE)
  expect_false(grepl("is not reported", note, fixed = TRUE))
})

test_that("a calibrated design with no negative weight says nothing", {
  # The note is conditional on the FACT, not on the design being
  # calibrated: "calibrated" is already said by the design line.
  skip_if_not_installed("survey")
  d <- .svyc_design("cal")
  expect_identical(.design_negative_weights(d), 0L)
  note <- attr(
    table_continuous_svy(d, select = api00, by = stype),
    "note"
  )
  expect_match(note, "calibrated / post-stratified", fixed = TRUE)
  expect_false(grepl("negative weight", note, fixed = TRUE))
  # And the comparison still runs.
  expect_match(note, "P-value: design-based", fixed = TRUE)
  out <- .svyc_long(d, select = api00, by = stype, show_columns = c("m", "n"))
  expect_false(all(is.na(out$p.value)))
})

test_that("the footer gives the df span when the groups disagree", {
  skip_if_not_installed("survey", "4.5")
  tbl <- table_continuous_svy(.svyc_design("clus1"), select = api00, by = stype)
  expect_match(
    attr(tbl, "note"),
    "degrees of freedom vary by group (7 to 14)",
    fixed = TRUE
  )
  # A one-way table has one df and must say so as a number.
  expect_match(
    attr(
      table_continuous_svy(.svyc_design("clus1"), select = api00),
      "note"
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
    attr(table_continuous_svy(des, select = api00, by = stype), "note"),
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
    attr(tbl, "note"),
    "Missing values removed: stype (3).",
    fixed = TRUE
  )
  # A labelled `by` is named by its label under `labels = TRUE`, as in
  # the title.
  attr(dat$stype, "label") <- "School type"
  des_lab <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = dat,
    fpc = ~fpc
  )
  expect_identical(
    attr(
      table_continuous_svy(des_lab, select = api00, by = stype),
      "group_label"
    ),
    "stype"
  )
  tbl_lab <- table_continuous_svy(
    des_lab,
    select = api00,
    by = stype,
    labels = TRUE
  )
  expect_match(
    attr(tbl_lab, "note"),
    "Missing values removed: School type (3).",
    fixed = TRUE
  )
  # Dropping those rows does not cost the title its label.
  expect_identical(attr(tbl_lab, "group_label"), "School type")
  # No "(Missing)" domain: a level the sampler never assigned is not a
  # domain, and its degrees of freedom would be an artifact.
  expect_identical(sort(unique(tbl$group)), c("E", "H", "M"))
})

# ---- the group comparison -------------------------------------------------

test_that("two groups take the design-based t-test, three the Wald F", {
  d <- .svyc_design("clus1")
  two <- .svyc_long(d, select = api00, by = sch.wide, statistic = TRUE)
  expect_identical(two$test_type[[1L]], "design_t")
  expect_equal(two$statistic[[1L]], 2.108989847792919203, tolerance = 1e-12)
  expect_equal(two$df[[1L]], 13)
  expect_true(is.na(two$df.residual[[1L]]))
  expect_equal(two$p.value[[1L]], 0.054908817089046651, tolerance = 1e-12)

  three <- .svyc_long(d, select = api00, by = stype, statistic = TRUE)
  expect_identical(three$test_type[[1L]], "design_f")
  expect_equal(three$statistic[[1L]], 1.27820074251777616, tolerance = 1e-12)
  expect_equal(c(three$df[[1L]], three$df.residual[[1L]]), c(2, 12))
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
  expect_equal(two$df[[1L]], 13)
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
  expect_equal(c(three$df[[1L]], three$df.residual[[1L]]), c(2, 12))
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
  expect_equal(out$df[[1L]], 13)
  expect_equal(out$p.value[[1L]], 0.054908817089046651, tolerance = 1e-12)
  # The column is alive: without `droplevels()` it is entirely NA.
  expect_false(is.na(out$p.value[[1L]]))
})

test_that("the comparison reads its groups, whatever the type of `by`", {
  # An ordered factor entered `svyttest()` with a polynomial contrast,
  # which its 0/1 check refuses: a two-level ordered `by` was never
  # compared. A numeric `by` entered `svyglm()` as a SLOPE, a trend test
  # under the name of a group comparison.
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dat <- apiclus1
  dat$o2 <- factor(dat$sch.wide, ordered = TRUE)
  dat$g3 <- as.numeric(factor(dat$stype))
  dat$f3 <- factor(dat$g3)
  des <- survey::svydesign(id = ~dnum, weights = ~pw, data = dat, fpc = ~fpc)
  ordered <- .svyc_long(des, select = api00, by = o2, statistic = TRUE)
  unordered <- .svyc_long(des, select = api00, by = sch.wide, statistic = TRUE)
  expect_equal(
    ordered$p.value[[1L]],
    unordered$p.value[[1L]],
    tolerance = 1e-12
  )
  expect_equal(ordered$statistic[[1L]], 2.108989847792919203, tolerance = 1e-12)
  numeric <- .svyc_long(des, select = api00, by = g3, statistic = TRUE)
  factor3 <- .svyc_long(des, select = api00, by = f3, statistic = TRUE)
  expect_identical(c(numeric$df[[1L]], numeric$df.residual[[1L]]), c(2, 12))
  expect_equal(numeric$p.value[[1L]], factor3$p.value[[1L]], tolerance = 1e-12)
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
    "note"
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
  expect_identical(moved$df[[1L]], 13)
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
  expect_false(is.na(a$p.value[[1L]]))
  expect_equal(a$p.value[[1L]], b$p.value[[1L]], tolerance = 1e-12)
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
    attr(tbl, "note"),
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
    attr(tbl, "note"),
    "Missing values removed: y (1).",
    fixed = TRUE
  )
  expect_identical(tbl$n, 3L)
})

# ---- no data in a group (lot N, R1) -------------------------------------------

# `y` is not observed in group "c"; `x` is, with one ordinary missing
# value. An ORDERED `by`, like `education` in `sochealth`.
.svyc_nodata <- function(ordered = TRUE) {
  d <- data.frame(
    g = factor(
      rep(c("a", "b", "c"), each = 4),
      levels = c("a", "b", "c"),
      ordered = ordered
    ),
    x = c(1, 2, 3, 4, 3, 4, 5, 6, 5, 6, 7, NA),
    y = c(2, 4, 6, 8, 1, 3, 5, 7, NA, NA, NA, NA),
    w = rep(c(1, 2), 6)
  )
  survey::svydesign(id = ~1, weights = ~w, data = d)
}

test_that("a variable with no data in a domain shows the mark there", {
  skip_if_not_installed("survey")
  des <- .svyc_nodata()
  long <- .svyc_long(des, select = c(x, y), by = g)
  cell <- long$variable == "y" & long$group == "c"
  expect_identical(long$n[cell], NA_integer_)
  expect_true(is.na(long$mean[cell]))
  expect_identical(long$n[long$variable == "x"], c(4L, 4L, 3L))
  tbl <- table_continuous_svy(des, select = c(x, y), by = g)
  row <- which(tbl$variable == "y" & tbl$group == "c")
  st <- as_structured(tbl)
  expect_true(is.na(st$body$n[[row]]))
  expect_identical(st$cell_status$n[[row]], "undefined")
  expect_identical(
    st$col_meta$n$display_cells[[row]],
    spicy_str("cell_undefined")
  )
  # glance() sums the counts that exist.
  skip_if_not_installed("broom")
  expect_identical(broom::glance(tbl)$nobs, c(11, 8))
})

test_that("the comparison of a no-data variable runs on the domains with data", {
  skip_if_not_installed("survey")
  des <- .svyc_nodata()
  long <- .svyc_long(des, select = c(x, y), by = g)
  first <- long[!duplicated(long$variable), ]
  expect_identical(first$test_type, c("design_f", "design_t"))
  # survey's own two-group test on the domains with data.
  sub <- subset(des, !is.na(y))
  sub$variables$g <- factor(as.character(sub$variables$g))
  ref <- survey::svyttest(y ~ g, sub)
  expect_equal(first$p.value[[2L]], ref$p.value[[1L]], tolerance = 1e-12)
  note <- attr(table_continuous_svy(des, select = c(x, y), by = g), "note")
  expect_match(
    note,
    "Missing values removed: x (1). No data for y in c.",
    fixed = TRUE
  )
  expect_match(
    note,
    "P-values: design-based Wald test (x); design-based t-test (y).",
    fixed = TRUE
  )
  # Two domains, one without data: no comparison, and none named.
  two <- subset(des, g != "c")
  two$variables$y[two$variables$g == "b"] <- NA
  out <- .svyc_long(two, select = y, by = g)
  expect_true(all(is.na(out$p.value)))
  expect_false(grepl(
    "Group comparison|P-value",
    attr(table_continuous_svy(two, select = y, by = g), "note")
  ))
})


test_that("without `by`, a variable with no observed value has no data", {
  skip_if_not_installed("survey")
  d <- data.frame(x = c(1, 2, NA, 4), y = NA_real_, w = rep(1, 4))
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  long <- .svyc_long(des, select = c(x, y))
  expect_identical(long$n, c(3L, NA_integer_))
  note <- attr(table_continuous_svy(des, select = c(x, y)), "note")
  expect_match(
    note,
    "Missing values removed: x (1). No data for y.",
    fixed = TRUE
  )
  # Two such variables share the sentence.
  d$z <- NA_real_
  des2 <- survey::svydesign(id = ~1, weights = ~w, data = d)
  expect_match(
    attr(table_continuous_svy(des2, select = c(y, x, z)), "note"),
    "Missing values removed: x (1). No data for y and z.",
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  expect_match(
    attr(table_continuous_svy(des, select = c(x, y)), "note"),
    "Aucune donn\u00e9e pour y.",
    fixed = TRUE
  )
})

test_that("the note gives the design, then the missing data, then the inference", {
  skip_if_not_installed("survey")
  des <- .svyc_nodata(ordered = FALSE)
  note <- attr(
    table_continuous_svy(
      des,
      select = c(x, y),
      by = g,
      show_columns = c("m", "se", "med", "n"),
      p_adjust = "holm"
    ),
    "note"
  )
  sentences <- c(
    "N = 12",
    "Design: ",
    "Quantiles: ",
    "Missing values removed: x (1).",
    "No data for y in c.",
    "Std. errors: ",
    "Confidence intervals and tests use",
    "P-values: ",
    "P-values adjusted for 2 tests (Holm method).",
    "SE = "
  )
  at <- vapply(
    sentences,
    function(s) regexpr(s, note, fixed = TRUE)[[1L]],
    integer(1)
  )
  expect_true(all(at > 0L))
  expect_false(is.unsorted(at))
})

test_that("the object carries the title it prints", {
  skip_if_not_installed("survey")
  des <- .svyc_design("clus1")
  tbl <- table_continuous_svy(des, select = api00, by = stype)
  expect_identical(attr(tbl, "title"), "Descriptive statistics by stype")
  expect_identical(utils::capture.output(print(tbl))[[1L]], attr(tbl, "title"))
  expect_identical(
    attr(table_continuous_svy(des, select = api00), "title"),
    "Descriptive statistics"
  )
})


test_that("the note opens with \"Note.\" once, on the attribute and in print", {
  skip_if_not_installed("survey")
  d <- data.frame(y = c(1, 2, NA, 4), w = rep(1, 4))
  tbl <- table_continuous_svy(
    survey::svydesign(id = ~1, weights = ~w, data = d),
    select = y
  )
  note <- attr(tbl, "note")
  expect_true(startsWith(note, "Note. N = 4 (weighted 4). Design: "))
  expect_match(note, " Missing values removed: y (1). ", fixed = TRUE)
  expect_identical(lengths(gregexpr("Note.", note, fixed = TRUE)), 1L)
  printed <- utils::capture.output(print(tbl))
  expect_identical(sum(grepl("Note.", printed, fixed = TRUE)), 1L)
  expect_identical(sum(startsWith(printed, "Note. ")), 1L)
  expect_identical(
    attr(
      table_continuous_svy(
        survey::svydesign(id = ~1, weights = ~w, data = d),
        select = y,
        output = "data.frame"
      ),
      "note"
    ),
    note
  )
  withr::local_options(spicy.language = "fr")
  note_fr <- attr(
    table_continuous_svy(
      survey::svydesign(id = ~1, weights = ~w, data = d),
      select = y
    ),
    "note"
  )
  expect_true(startsWith(note_fr, "Note. N = 4 "))
  expect_match(
    note_fr,
    " Valeurs manquantes retir\u00e9es\u00a0: y (1). ",
    fixed = TRUE
  )
})

test_that("the missing-values note names each variable by its row label", {
  skip_if_not_installed("survey")
  d <- data.frame(
    y = c(1, 2, NA, 4, 5, 6),
    z = c(NA, 2, 3, 4, 5, 6),
    v = c(1, 2, 3, 4, 5, 7),
    e = c(2, 2, 3, 4, 5, 7),
    g = c("a", "b", "a", "b", "a", NA),
    w = rep(1, 6)
  )
  attr(d$y, "label") <- "Outcome Y"
  attr(d$v, "label") <- NA_character_
  attr(d$e, "label") <- ""
  attr(d$g, "label") <- "Group"
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  build <- function(...) {
    suppressWarnings(table_continuous_svy(des, select = c(y, z, v, e), ...))
  }
  # Rows and the values note on the one-way table; the `by` title and
  # the missing-`by` note on the grouped one.
  check <- function(by, rows, row_y, row_z, ...) {
    tbl <- build(...)
    expect_identical(unique(tbl$label), rows)
    expect_match(
      attr(tbl, "note"),
      sprintf("Missing values removed: %s (1), %s (1).", row_y, row_z),
      fixed = TRUE
    )
    grouped <- build(by = g, ...)
    expect_identical(attr(grouped, "group_label"), by)
    expect_match(
      attr(grouped, "note"),
      sprintf(
        "Missing values removed: %s (1), %s (1), %s (1).",
        row_y,
        row_z,
        by
      ),
      fixed = TRUE
    )
  }
  check("g", c("y", "z", "v", "e"), "y", "z")
  # No attribute, an NA one and an empty one fall back to the name.
  check("Group", c("Outcome Y", "z", "v", "e"), "Outcome Y", "z", labels = TRUE)
  check(
    "Arm",
    c("y", "Zed", "v", "e"),
    "y",
    "Zed",
    labels = c(z = "Zed", g = "Arm", nope = "X")
  )
  withr::local_options(spicy.var_labels = TRUE)
  check(
    "Group",
    c("Outcome Y", "Zed", "v", "e"),
    "Outcome Y",
    "Zed",
    labels = c(z = "Zed")
  )
  check("g", c("y", "z", "v", "e"), "y", "z", labels = FALSE)
  for (b in list(NULL, NA, c("a", "b"), list(z = "Zed"))) {
    expect_error(build(labels = b), class = "spicy_invalid_input")
  }
  withr::local_options(spicy.var_labels = c(TRUE, TRUE))
  expect_error(build(), class = "spicy_invalid_input")
})

test_that("var_labels() renames on the attribute base, whatever the option", {
  skip_if_not_installed("survey")
  des <- survey::svydesign(ids = ~1, weights = ~weight, data = sochealth)
  build <- function() {
    table_continuous_svy(
      des,
      select = c(bmi, age),
      by = sex,
      labels = var_labels(bmi = "IMC")
    )
  }
  check <- function(tbl) {
    expect_identical(attr(tbl, "group_label"), "Sex")
    expect_identical(unique(tbl$label), c("IMC", "Age (years)"))
  }
  check(build())
  withr::local_options(spicy.var_labels = FALSE)
  check(build())
})

# ---- restitution ----------------------------------------------------------

test_that("the design table comes back visibly, and assigning it says nothing", {
  # Decision 47, the family invariant: a bare call renders through the
  # print method (capture.output() evaluates as at top level, so the
  # visible return auto-prints), an assignment writes nothing. This
  # twin was already conformant; the witness pins it.
  d <- .svyc_design("clus1")
  bare <- suppressWarnings(
    utils::capture.output(table_continuous_svy(d, select = api00))
  )
  expect_true(length(bare) > 0)

  assigned <- suppressWarnings(
    utils::capture.output(tbl <- table_continuous_svy(d, select = api00))
  )
  expect_identical(assigned, character(0))
  expect_s3_class(tbl, "spicy_continuous_svy_table")
  expect_identical(utils::capture.output(print(tbl)), bare)
})

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
  expect_identical(s$version, 4L)
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
  expect_identical(inline(tbl, "api00", model = "E", column = "m"), "648.87")
  expect_identical(inline(tbl, "api00", model = "E", column = "se"), "22.36")
  expect_identical(
    inline(tbl, "api00", model = "E", column = "ci"),
    "[600.91, 696.83]"
  )
  # The omnibus p belongs to the variable, not to a group: it is cited
  # without `model`, as in `table_continuous()`, and refused with a
  # group, even the first one, whose row prints it.
  expect_identical(inline(tbl, "api00", column = "p"), ".314")
  expect_error(
    inline(tbl, "api00", model = "E", column = "p"),
    class = "spicy_invalid_input"
  )
})

test_that("`output = \"data.frame\"` is the displayed table, `\"long\"` every statistic", {
  d <- .svyc_design("clus1")
  tbl <- table_continuous_svy(d, select = api00, by = stype, statistic = TRUE)
  a <- table_continuous_svy(
    d,
    select = api00,
    by = stype,
    statistic = TRUE,
    output = "data.frame"
  )
  # The data frame is as.data.frame() of the table: the printed columns
  # under their keys, the test statistic with its degrees of freedom.
  expect_identical(a, as.data.frame(tbl))
  expect_s3_class(a, "data.frame")
  expect_false(inherits(a, "spicy_continuous_svy_table"))
  expect_identical(
    names(a),
    c(
      "variable",
      "label",
      "group",
      ".row_role",
      "M",
      "SD",
      "Min",
      "Max",
      "95% CI LL",
      "95% CI UL",
      "n",
      "Test",
      "df",
      "df.residual",
      "p"
    )
  )
  expect_identical(a$.row_role, rep("group", 3L))
  expect_equal(a$Test[[1L]], 1.27820074251777616, tolerance = 1e-12)
  expect_identical(c(a$df[[1L]], a$df.residual[[1L]]), c(2, 12))
  # The long frame: every statistic the table computes, in the
  # vocabulary of the family, the note its only attribute.
  b <- table_continuous_svy(
    d,
    select = api00,
    by = stype,
    statistic = TRUE,
    output = "long"
  )
  expect_false(inherits(b, "spicy_continuous_svy_table"))
  expect_identical(
    names(b)[1:5],
    c("variable", "label", "group", ".group_role", "mean")
  )
  expect_false(".row_role" %in% names(b))
  expect_true(all(c("se", "deff", "degf", "df", "df.residual") %in% names(b)))
  expect_false(any(c("df1", "df2") %in% names(b)))
  expect_equal(b$mean, a$M, tolerance = 1e-12)
  expect_equal(b$p.value, a$p, tolerance = 1e-12)
  for (x in list(a, b)) {
    expect_identical(
      setdiff(names(attributes(x)), c("names", "row.names", "class")),
      "note"
    )
  }
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
      ".group_role",
      "estimate",
      "std.error",
      "conf.low",
      "conf.high",
      "df",
      "n",
      "weighted_n",
      "deff"
    ) %in%
      names(td)
  ))
  expect_identical(td$.group_role, rep("group", 6L))
  expect_false(".row_role" %in% names(td))
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
  expect_equal(
    gl$weighted_nobs[[1L]],
    sum(td$weighted_n[td$variable == "api00"]),
    tolerance = 1e-12
  )

  # A one-way table has the same schema with the comparison columns NA.
  gl1 <- broom::glance(table_continuous_svy(d, select = api00))
  expect_true(all(is.na(c(gl1$p.value, gl1$statistic, gl1$test_type))))
  expect_identical(gl1$nobs, 183)
})

test_that("coercion returns the displayed table, with the note alone", {
  d <- .svyc_design("clus1")
  tbl <- table_continuous_svy(d, select = api00, by = stype)
  expect_identical(
    class(tbl),
    c("spicy_continuous_svy_table", "spicy_table", "data.frame")
  )
  df <- as.data.frame(tbl)
  expect_identical(class(df), "data.frame")
  expect_identical(
    df,
    table_continuous_svy(d, select = api00, by = stype, output = "data.frame")
  )
  # The provenance markers stay on the table; the coerced frame keeps
  # the note only.
  expect_null(attr(df, "group_var"))
  expect_null(attr(df, "design_meta"))
  expect_identical(attr(df, "note"), attr(tbl, "note"))
  expect_identical(attr(tbl, "group_var"), "stype")
  skip_if_not_installed("tibble")
  tb <- tibble::as_tibble(tbl)
  expect_s3_class(tb, "tbl_df")
  expect_identical(nrow(tb), 3L)
  expect_identical(names(tb), names(df))
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


# ---- "data.frame" and "long": one table, two shapes ---------------------

test_that("output = 'data.frame' and output = 'long' carry the same numbers", {
  # Two shapes since the ADR of 2026-10-20, as in table_continuous():
  # the displayed table under its printed keys, and the compute frame
  # under the names of the family. One row per (variable x group) in
  # both, in the same order.
  d <- .svyc_design("clus1")
  as_df <- suppressWarnings(table_continuous_svy(
    d,
    select = c(api00, api99),
    by = stype,
    output = "data.frame"
  ))
  as_long <- suppressWarnings(table_continuous_svy(
    d,
    select = c(api00, api99),
    by = stype,
    output = "long"
  ))
  expect_true(all(
    c("variable", "group", "M", "SD", "n", "p") %in% names(as_df)
  ))
  expect_true(all(
    c("variable", "group", "mean", "sd", "n") %in% names(as_long)
  ))
  expect_false(any(c("mean", "sd") %in% names(as_df)))
  for (k in c("variable", "label", "group")) {
    expect_identical(as_df[[k]], as_long[[k]])
  }
  # The role of the printed row is, in the long frame, that of its group.
  expect_identical(as_df$.row_role, as_long$.group_role)
  expect_equal(as_df$M, as_long$mean, tolerance = 1e-12)
  expect_equal(as_df$SD, as_long$sd, tolerance = 1e-12)
  expect_equal(as_df[["95% CI LL"]], as_long$ci_lower, tolerance = 1e-12)
  expect_equal(as_df$n, as_long$n)
  expect_equal(as_df$p, as_long$p.value, tolerance = 1e-12)
})

# ---- p_adjust ---------------------------------------------------------------

test_that("p_adjust adjusts the one design-based test of each variable", {
  d <- .svyc_design()
  raw <- table_continuous_svy(
    d,
    c(api00, api99, enroll),
    by = sch.wide,
    output = "long"
  )
  raw_p <- raw$p.value[!is.na(raw$p.value)]
  expect_length(raw_p, 3L)
  for (method in c("holm", "BH", "bonferroni")) {
    adj <- table_continuous_svy(
      d,
      c(api00, api99, enroll),
      by = sch.wide,
      p_adjust = method,
      output = "long"
    )
    expect_equal(
      adj$p.value[!is.na(adj$p.value)],
      stats::p.adjust(raw_p, method = method)
    )
    expect_identical(adj$p_unadjusted, raw$p.value)
  }
  tbl <- table_continuous_svy(
    d,
    c(api00, api99, enroll),
    by = sch.wide,
    p_adjust = "BH"
  )
  meta <- as_structured(tbl)$col_meta$p
  expect_identical(meta$p_adjust, "BH")
  expect_identical(meta$p_adjust_m, 3L)
  expect_equal(meta$p_unadjusted[!is.na(meta$p_unadjusted)], raw_p)
  expect_match(
    attr(tbl, "note"),
    'P-values adjusted for 3 tests (Benjamini-Hochberg method).',
    fixed = TRUE
  )
})

test_that("p_adjust needs `by` and the p-value column", {
  d <- .svyc_design()
  expect_error(
    table_continuous_svy(d, c(api00, api99), p_adjust = "holm"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_continuous_svy(
      d,
      c(api00, api99),
      by = sch.wide,
      p_value = FALSE,
      p_adjust = "holm"
    ),
    class = "spicy_invalid_input"
  )
})

test_that("statistic = TRUE, p_value = FALSE keeps the statistic, not the p (issue #9)", {
  d <- .svyc_design()
  ref <- .svyc_long(d, c(api00, api99), by = stype, statistic = TRUE)
  out <- .svyc_long(
    d,
    c(api00, api99),
    by = stype,
    statistic = TRUE,
    p_value = FALSE
  )
  expect_identical(names(out), setdiff(names(ref), "p.value"))
  expect_identical(out$statistic, ref$statistic)
  # glance() keeps its fixed schema, NA where the frame has no column.
  gl <- broom::glance(suppressWarnings(table_continuous_svy(
    d,
    c(api00, api99),
    by = stype,
    statistic = TRUE,
    p_value = FALSE
  )))
  expect_true("p.value" %in% names(gl))
  expect_true(all(is.na(gl$p.value)))
  expect_false(anyNA(gl$statistic))
})

test_that("a `by` no observation holds is refused, not a base R error", {
  d <- .svyc_design("iid")
  d$variables$none <- factor(NA, levels = c("a", "b"))
  err <- expect_error(
    table_continuous_svy(d, select = api00, by = none),
    class = "spicy_invalid_data"
  )
  expect_identical(rlang::call_name(conditionCall(err)), "table_continuous_svy")
  # Kept as a group of its own, the missing values are a domain.
  out <- table_continuous_svy(
    d,
    select = api00,
    by = none,
    drop_na = FALSE,
    output = "long"
  )
  expect_identical(out$group, "(Missing)")
})

test_that("a variable with no variance gets no comparison, with a warning", {
  # survey still returns a p-value for it (0.228 from svyttest() on the
  # sochealth constant): the cells are undefined, as in table_continuous().
  d <- .svyc_design("iid")
  d$variables$const <- 5
  expect_warning(
    out <- table_continuous_svy(
      d,
      select = c(const, api00),
      by = sch.wide,
      output = "long"
    ),
    class = "spicy_undefined_stat"
  )
  expect_true(all(is.na(out$p.value[out$variable == "const"])))
  expect_false(all(is.na(out$p.value[out$variable == "api00"])))
  for (tst in c("welch", "nonparametric")) {
    out <- suppressWarnings(table_continuous_svy(
      d,
      select = const,
      by = stype,
      test = tst,
      output = "long"
    ))
    expect_true(all(is.na(out$p.value)), info = tst)
    expect_false(
      grepl("Group comparison|P-value", attr(out, "note")),
      info = tst
    )
  }
})

test_that("the comparison cells of a variable with no variance are undefined", {
  d <- .svyc_design("iid")
  d$variables$const <- 5
  tc <- suppressWarnings(table_continuous_svy(
    d,
    select = c(const, api00),
    by = sch.wide,
    statistic = TRUE
  ))
  s <- as_structured(tc)
  shown <- spicy:::.format_structured_to_string_body(s)
  tok <- vapply(s$col_meta, `[[`, character(1), "token")
  rows <- which(s$body$.variable == "const")
  for (token in c("statistic", "p")) {
    nm <- names(tok)[tok == token]
    # On the row that carries the comparison, the undefined mark, with the
    # status of an undefined cell, where it was blank.
    expect_identical(shown[[nm]][rows], c(spicy_str("cell_undefined"), ""))
    expect_identical(s$cell_status[[nm]][rows], c("undefined", ""))
    expect_error(
      inline(tc, const, column = token),
      "undefined in the table (not estimable)",
      fixed = TRUE,
      class = "spicy_invalid_input"
    )
  }
  # The variable that varies keeps its comparison.
  expect_true(nzchar(inline(tc, api00, column = "p")))
  skip_if_not_installed("broom")
  gl <- broom::glance(tc)
  expect_true(is.na(gl$p.value[gl$variable == "const"]))
  expect_true(is.na(gl$statistic[gl$variable == "const"]))
})

test_that("every comparison that cannot run is undefined, its warnings unchanged", {
  skip_if_not_installed("survey")
  mark <- spicy_str("cell_undefined")
  g <- rep(c("a", "b"), each = 4)
  design <- function(v, w) {
    survey::svydesign(
      ids = ~1,
      weights = ~w,
      data = data.frame(v = v, g = g, w = w)
    )
  }
  causes <- list(
    # One observation in b, too few for a comparison: no warning.
    few = list(
      design = design(c(1, 2, 3, 4, 5, NA, NA, NA), rep(1, 8)),
      warned = character(0)
    ),
    # Weights that change sign: the comparison is refused, with its own
    # warning.
    refused = list(
      design = design(1:8 + 0.5, c(1, 1, -1, 1, 1, 1, 1, 1)),
      warned = "spicy_negative_weights_no_test"
    )
  )
  # Each domain constant on its own value: a residual variance of zero,
  # where the design-based t was rounding noise (2.4e15). One warning.
  causes$zero_resid <- list(
    design = design(c(4, 4, 4, 4, 6, 6, 6, 6), c(1, 2, 1, 2, 1, 2, 1, 2)),
    warned = "spicy_undefined_stat"
  )
  for (cause in names(causes)) {
    warned <- character(0)
    tc <- withCallingHandlers(
      table_continuous_svy(
        causes[[cause]]$design,
        select = v,
        by = g,
        statistic = TRUE
      ),
      warning = function(w) {
        warned <<- c(warned, class(w)[[1L]])
        invokeRestart("muffleWarning")
      }
    )
    expect_identical(warned, causes[[cause]]$warned, info = cause)
    s <- as_structured(tc)
    shown <- spicy:::.format_structured_to_string_body(s)
    tok <- vapply(s$col_meta, `[[`, character(1), "token")
    for (token in c("statistic", "p")) {
      nm <- names(tok)[tok == token]
      expect_identical(shown[[nm]], c(mark, ""), info = cause)
      expect_identical(s$cell_status[[nm]], c("undefined", ""), info = cause)
      expect_error(
        inline(tc, v, column = token),
        "undefined in the table (not estimable)",
        fixed = TRUE,
        class = "spicy_invalid_input"
      )
    }
  }
  expect_warning(
    table_continuous_svy(causes$zero_resid$design, select = v, by = g),
    "`v` (the residual variance is zero)",
    fixed = TRUE,
    class = "spicy_undefined_stat"
  )
  # The design-based rank test as well, unlike the rank tests of
  # table_continuous(): svyranktest() is a design-based t on the ranks,
  # constant within the domains too (its t was 8.2e15 on such data).
  expect_warning(
    np <- table_continuous_svy(
      causes$zero_resid$design,
      select = v,
      by = g,
      test = "nonparametric",
      output = "long"
    ),
    class = "spicy_undefined_stat"
  )
  expect_true(all(is.na(np$p.value)))
})

test_that("`by` named in `select` is refused; a helper skips it", {
  d <- .svyc_design("iid")
  expect_error(
    table_continuous_svy(d, select = c(api00, api99), by = api99),
    class = "spicy_invalid_input"
  )
  out <- table_continuous_svy(
    d,
    select = tidyselect::starts_with("api"),
    by = api99,
    output = "long"
  )
  expect_false("api99" %in% out$variable)
  expect_error(
    table_continuous_svy(d, select = c(api00, nope)),
    class = "spicy_missing_column"
  )
})

test_that("the design's weights, clusters and fpc are outside the selection", {
  skip_if_not_installed("survey")
  d <- data.frame(
    w = 1 + (1:40 %% 3) / 2,
    psu = rep(1:8, each = 5),
    st = rep(1:2, each = 20),
    N = 100,
    y = sin(1:40),
    z = cos(1:40),
    rep1 = rep(c(0.5, 1.5), 20),
    rep2 = rep(c(1.5, 0.5), 20)
  )
  des <- survey::svydesign(
    ids = ~psu,
    strata = ~st,
    weights = ~w,
    fpc = ~N,
    data = d[1:6],
    nest = TRUE
  )
  vars <- function(...) unique(.svyc_long(...)$variable)
  # A helper or a pattern skips them. The strata stay a variable, and a
  # design column is a `by` like any other.
  expect_identical(vars(des), c("st", "y", "z"))
  expect_identical(
    vars(des, select = tidyselect::where(is.numeric)),
    c("st", "y", "z")
  )
  expect_identical(vars(des, select = ".", regex = TRUE), c("st", "y", "z"))
  expect_identical(vars(des, select = c(y, z), by = st), c("y", "z"))
  expect_identical(vars(des, select = y, by = psu, p_value = FALSE), "y")
  # Named, as a name, a string or a position, one is refused, and the
  # error says where to describe it.
  cases <- list(
    list(
      quote(table_continuous_svy(des, select = c(y, w))),
      "sampling weights",
      "w"
    ),
    list(
      quote(table_continuous_svy(des, select = c("y", "psu"))),
      "cluster",
      "psu"
    ),
    list(
      quote(table_continuous_svy(des, select = c(5, 4))),
      "finite population correction",
      "N"
    )
  )
  for (case in cases) {
    err <- tryCatch(eval(case[[1L]]), error = identity)
    expect_s3_class(err, "spicy_invalid_input")
    expect_match(
      conditionMessage(err),
      sprintf(
        "`select` cannot contain the design's %s column (\"%s\").",
        case[[2L]],
        case[[3L]]
      ),
      fixed = TRUE
    )
    expect_match(
      conditionMessage(err),
      sprintf("`table_continuous(<data>, select = %s)`", case[[3L]]),
      fixed = TRUE
    )
  }
  # A column that is both is refused as the `by` the caller typed.
  expect_error(
    table_continuous_svy(des, select = c(y, psu), by = psu),
    "`select` cannot contain the `by` column (\"psu\").",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
  rep_des <- survey::svrepdesign(
    data = d[c("w", "y", "rep1", "rep2")],
    repweights = "rep[0-9]+",
    weights = ~w,
    type = "bootstrap"
  )
  expect_identical(vars(rep_des), "y")
  expect_error(
    table_continuous_svy(rep_des, select = rep2),
    "the design's replicate weights column (\"rep2\")",
    fixed = TRUE,
    class = "spicy_invalid_input"
  )
})
