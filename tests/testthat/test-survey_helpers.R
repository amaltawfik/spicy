# ---------------------------------------------------------------------------
# The survey-design socle of the descriptive twins (R/survey_helpers.R)
# and the regime gate the four plain builders now share
# (`.check_data_frame()`, R/abort.R).
#
# Every number pinned here was measured on survey 4.5 with the `api`
# fixtures, at 17 significant digits.
# ---------------------------------------------------------------------------

.svy_fixture <- function(which = "clus1") {
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
    clus2 = survey::svydesign(
      id = ~ dnum + snum,
      fpc = ~ fpc1 + fpc2,
      data = apiclus2
    ),
    strat = survey::svydesign(
      id = ~1,
      strata = ~stype,
      weights = ~pw,
      data = apistrat,
      fpc = ~fpc
    ),
    srs = survey::svydesign(id = ~1, weights = ~pw, data = apisrs),
    rep1 = survey::as.svrepdesign(survey::svydesign(
      id = ~dnum,
      weights = ~pw,
      data = apiclus1,
      fpc = ~fpc
    )),
    cal = survey::calibrate(
      survey::svydesign(
        id = ~dnum,
        weights = ~pw,
        data = apiclus1,
        fpc = ~fpc
      ),
      ~stype,
      pop = c(`(Intercept)` = 6194, stypeH = 755, stypeM = 1018)
    ),
    pps = survey::svydesign(
      id = ~dnum,
      fpc = ~ I(fpc / 6194),
      data = apiclus1,
      pps = "brewer"
    ),
    twophase = survey::twophase(
      id = list(~1, ~1),
      subset = ~ I(comp.imp == "Yes"),
      data = apiclus1
    )
  )
}

# ---- recognition ----------------------------------------------------------

test_that("`.is_survey_design()` recognises every design class it must", {
  skip_if_not_installed("survey")
  for (w in c("clus1", "strat", "rep1", "cal", "pps", "twophase")) {
    expect_true(.is_survey_design(.svy_fixture(w)), info = w)
  }
  # And nothing else. A data.frame is the case that matters: it decides
  # which of the two refusals a caller gets.
  expect_false(.is_survey_design(mtcars))
  expect_false(.is_survey_design(1:10))
  expect_false(.is_survey_design(NULL))
  expect_false(.is_survey_design(list(cluster = 1)))
})

test_that("P1 supports linearised and replicate designs, refuses the rest", {
  skip_if_not_installed("survey")
  for (w in c("clus1", "clus2", "strat", "srs", "rep1", "cal")) {
    expect_true(.is_supported_design(.svy_fixture(w)), info = w)
  }
  # A twophase design is class-marked; a without-replacement pps design
  # is NOT (it is a plain "survey.design2"), and only the `$pps` slot
  # tells it apart. That second line is the witness that would have gone
  # missing had the predicate been written on classes alone.
  expect_false(.is_supported_design(.svy_fixture("twophase")))
  pps <- .svy_fixture("pps")
  expect_identical(class(pps)[[1L]], "survey.design2")
  expect_false(.is_supported_design(pps))
})

test_that("an unsupported design gets a classed refusal naming its class", {
  skip_if_not_installed("survey")
  err <- expect_error(
    .abort_unsupported_design(.svy_fixture("twophase"), "table_continuous_svy"),
    class = "spicy_unsupported"
  )
  expect_match(conditionMessage(err), "twophase2", fixed = TRUE)
  expect_match(conditionMessage(err), "table_continuous_svy", fixed = TRUE)
})

test_that("the pps refusal names the specification, not a supported class", {
  # `pps = "brewer"` leaves a plain `survey.design2`, so printing
  # `class(design)[1L]` refused the very class the next line offered as
  # supported -- and told the caller nothing they could act on.
  skip_if_not_installed("survey")
  pps <- .svy_fixture("pps")
  expect_identical(class(pps)[[1L]], "survey.design2")
  err <- expect_error(
    table_continuous_svy(pps, select = api00),
    class = "spicy_unsupported"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "without-replacement pps design", fixed = TRUE)
  expect_false(grepl("of class `survey.design2`", msg, fixed = TRUE))
  # A design that DOES carry a class marker still shows it.
  expect_match(
    conditionMessage(expect_error(
      table_continuous_svy(.svy_fixture("twophase"), select = api00),
      class = "spicy_unsupported"
    )),
    "twophase2",
    fixed = TRUE
  )
})

test_that("the survey version floor is checked, not merely declared", {
  # A Suggests floor is not resolved at install time: `DESCRIPTION` can
  # say `>= 4.5` and the session still load 4.4. The two features the
  # floor exists for fail QUIETLY there -- `ci_method = "wilson"` would
  # come back as a column of dashes -- so the door has to refuse.
  skip_if_not_installed("survey")
  err <- with_mocked_bindings(
    expect_error(
      .require_survey("table_continuous_svy"),
      class = "spicy_unsupported"
    ),
    .SURVEY_MIN_VERSION = "99.0",
    .package = "spicy"
  )
  expect_match(conditionMessage(err), "requires survey >= 99.0", fixed = TRUE)
  expect_match(
    conditionMessage(err),
    format(utils::packageVersion("survey")),
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "wilson", fixed = TRUE)
  # The installed version passes the same door.
  expect_null(.require_survey("table_continuous_svy"))
})

test_that("a data.frame at a `_svy` entry point is a regime error", {
  err <- expect_error(
    .abort_needs_design("table_continuous_svy", "table_continuous"),
    class = "spicy_wrong_regime"
  )
  expect_match(conditionMessage(err), "svydesign", fixed = TRUE)
  expect_match(conditionMessage(err), "table_continuous()", fixed = TRUE)
})

test_that("the missing-Suggests guard fires for survey", {
  # `with_mocked_bindings()`, not the `local_` form: the expression
  # form is the package's pattern for a spicy binding, and covr's
  # exclusion pass chokes on a namespace binding left swapped for the
  # rest of a test file (registered as incident 88).
  err <- with_mocked_bindings(
    expect_error(
      .require_survey("table_categorical_svy"),
      class = "spicy_missing_pkg"
    ),
    spicy_pkg_available = function(pkg) FALSE,
    .package = "spicy"
  )
  expect_match(conditionMessage(err), "survey", fixed = TRUE)
  expect_match(conditionMessage(err), .SURVEY_MIN_VERSION, fixed = TRUE)
})

# ---- degrees of freedom ---------------------------------------------------

test_that("`.design_degf()` is survey's degf, on every fixture", {
  skip_if_not_installed("survey")
  expect_identical(.design_degf(.svy_fixture("clus1")), 14)
  expect_identical(.design_degf(.svy_fixture("clus2")), 39)
  expect_identical(.design_degf(.svy_fixture("strat")), 197)
  expect_identical(.design_degf(.svy_fixture("rep1")), 14)
})

test_that("a domain recomputes its own degrees of freedom", {
  skip_if_not_installed("survey")
  d <- .svy_fixture("clus1")
  # Named domain, so the witness is reproducible: the first ten clusters
  # of the design (14 df on the full design, 9 on the domain -- the
  # SUDAAN / Stata convention).
  dom <- d$variables$dnum %in% sort(unique(d$variables$dnum))[1:10]
  sub <- .design_subset(d, dom)
  expect_identical(.design_degf(sub), 9)
  expect_identical(nrow(sub), 120L)
  # And the SE the domain carries is the one the two other legitimate
  # routes give, to the bit -- while a rebuilt svydesign() gives another
  # number entirely. This is the guard on the "never rebuild" rule.
  se_sub <- as.numeric(survey::SE(survey::svymean(~api00, sub)))
  expect_equal(se_sub, 15.402503359507902, tolerance = 1e-12)
  d_zero <- d
  d_zero$prob[!dom] <- Inf
  expect_equal(
    as.numeric(survey::SE(survey::svymean(~api00, d_zero))),
    se_sub,
    tolerance = 1e-12
  )
  data(api, package = "survey", envir = environment())
  rebuilt <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1[dom, ],
    fpc = ~fpc
  )
  expect_equal(
    as.numeric(survey::SE(survey::svymean(~api00, rebuilt))),
    15.737900457093147,
    tolerance = 1e-12
  )
  expect_false(isTRUE(all.equal(
    as.numeric(survey::SE(survey::svymean(~api00, rebuilt))),
    se_sub,
    tolerance = 1e-8
  )))
})

test_that("`.design_subset()` keeps rows at weight zero on a calibrated design", {
  skip_if_not_installed("survey")
  d <- .svy_fixture("cal")
  keep <- d$variables$stype == "E"
  sub <- .design_subset(d, keep)
  # survey's own `[` branch: on a calibrated design the rows STAY and
  # their probability becomes Inf (weight zero). The row count is
  # therefore the full one, and the sum of weights is the domain's.
  expect_identical(nrow(sub), nrow(d))
  expect_equal(sum(stats::weights(sub)), 4420.99999999999, tolerance = 1e-8)
})

# ---- design metadata ------------------------------------------------------

test_that("`.design_meta()` reads a linearised design through public slots", {
  skip_if_not_installed("survey")
  m <- .design_meta(.svy_fixture("strat"))
  expect_identical(m$kind, "linearized")
  expect_true(m$has_strata)
  expect_identical(m$n_strata, 3L)
  expect_identical(m$strata_name, "stype")
  expect_identical(m$n_stages, 1L)
  expect_identical(m$n_psu, 200L)
  expect_true(m$has_fpc)
  expect_false(m$calibrated)
  expect_false(m$pps)
  expect_identical(m$degf, 197)
  expect_identical(m$n_obs, 200L)
  expect_equal(m$sum_weights, 6193.9999580383301, tolerance = 1e-12)

  m2 <- .design_meta(.svy_fixture("clus2"))
  expect_false(m2$has_strata)
  expect_identical(m2$n_strata, 0L)
  expect_true(is.na(m2$strata_name))
  expect_identical(m2$n_stages, 2L)
  expect_identical(m2$n_psu, 40L)
  expect_identical(m2$degf, 39)
  expect_identical(m2$n_obs, 126L)
  expect_equal(m2$sum_weights, 5128.6750000000002, tolerance = 1e-12)

  expect_true(.design_meta(.svy_fixture("cal"))$calibrated)
  expect_true(.design_meta(.svy_fixture("pps"))$pps)
})

test_that("`.design_meta()` reads a replicate design", {
  skip_if_not_installed("survey")
  m <- .design_meta(.svy_fixture("rep1"))
  expect_identical(m$kind, "replicate")
  expect_identical(m$rep_type, "JK1")
  expect_identical(m$n_rep, 15L)
  expect_false(m$mse)
  expect_identical(m$degf, 14)
  expect_identical(m$n_obs, 183L)
  expect_equal(m$sum_weights, 6194.0003242492676, tolerance = 1e-12)
})

# ---- the self-documenting footer ------------------------------------------

test_that("the design footer says what the design is, in three sentences", {
  skip_if_not_installed("survey")
  expect_identical(
    .design_note_lines(.design_meta(.svy_fixture("strat"))),
    c(
      "Design: stratified (stype), with finite population correction; 197 degrees of freedom.",
      "Std. errors: Design-based (Taylor linearisation).",
      "Confidence intervals and tests use the design degrees of freedom."
    )
  )
  expect_identical(
    .design_note_lines(.design_meta(.svy_fixture("clus1")))[[1L]],
    "Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom."
  )
  expect_identical(
    .design_note_lines(.design_meta(.svy_fixture("clus2")))[[1L]],
    "Design: cluster (dnum), 2 sampling stages, 40 PSU, with finite population correction; 39 degrees of freedom."
  )
  expect_identical(
    .design_note_lines(.design_meta(.svy_fixture("cal")))[[1L]],
    "Design: cluster (dnum), 15 PSU, with finite population correction, calibrated / post-stratified; 14 degrees of freedom."
  )
  rep_lines <- .design_note_lines(.design_meta(.svy_fixture("rep1")))
  expect_identical(
    rep_lines[[1L]],
    "Design: replicate weights (JK1), 15 replicates; 14 degrees of freedom."
  )
  # The variance sentence is the one thing that must differ between the
  # two regimes: a replicate design does not linearise anything. It
  # names the scheme, because the same sentence in a regression footer
  # does.
  expect_identical(
    rep_lines[[2L]],
    "Std. errors: Design-based (replicate weights, JK1)."
  )
})

test_that("both families print the SAME variance sentence for one design", {
  # A reader who puts a design table and a regression on the same page
  # sees one fact, said once. The two used to spell it two ways
  # ("Standard errors: Taylor linearisation (survey)." against "Std.
  # errors: Design-based (Taylor linearisation).") and the difference
  # carried no information.
  skip_if_not_installed("survey")
  for (nm in c("clus1", "rep1")) {
    des <- .svy_fixture(nm)
    twin <- .design_note_lines(.design_meta(des))[[2L]]
    fit <- suppressWarnings(survey::svyglm(api00 ~ ell, design = des))
    regression <- spicy_fmt("note_std_errors_single", .design_vcov_label(fit))
    expect_identical(twin, regression, info = nm)
  }
})

test_that("a design with neither strata nor clusters is named as such", {
  skip_if_not_installed("survey")
  expect_identical(
    .design_note_lines(.design_meta(.svy_fixture("srs")))[[1L]],
    "Design: simple random sample; 199 degrees of freedom."
  )
})

test_that("a varying-df table announces the span rather than one number", {
  skip_if_not_installed("survey")
  m <- .design_meta(.svy_fixture("clus1"))
  expect_match(
    .design_note_lines(m, degf_range = c(9, 14))[[1L]],
    "degrees of freedom vary by group (9 to 14)",
    fixed = TRUE
  )
  # A span whose ends coincide is one number, not a range: a two-group
  # table where both domains keep every cluster must not print
  # "vary by group (14 to 14)".
  expect_match(
    .design_note_lines(m, degf_range = c(14, 14))[[1L]],
    "14 degrees of freedom",
    fixed = TRUE
  )
})

test_that("the sample-size sentence carries both counts", {
  expect_identical(
    .design_n_note(183L, 6194.0003242492676),
    "N = 183 (weighted 6194)."
  )
  # Both numbers are DISPLAYED, so both follow `decimal_mark`.
  expect_identical(
    .design_n_note(183L, 6194.0003242492676, digits = 2, decimal_mark = ","),
    "N = 183 (weighted 6194,00)."
  )
})

# ---- the regime gate on the four plain builders ---------------------------

test_that("every plain builder refuses a design with a route out", {
  skip_if_not_installed("survey")
  d <- .svy_fixture("clus1")
  hints <- c(
    table_continuous = "table_continuous_svy(design, ...)",
    table_categorical = "table_categorical_svy(design, ...)",
    table_continuous_lm = "table_regression(survey::svyglm(",
    table_outcome = "table_continuous_svy(design, select ="
  )
  for (fn in names(hints)) {
    err <- expect_error(
      .check_data_frame(d, fn),
      class = "spicy_wrong_regime",
      info = fn
    )
    expect_match(conditionMessage(err), hints[[fn]], fixed = TRUE, info = fn)
    expect_match(
      conditionMessage(err),
      "survey.design2",
      fixed = TRUE,
      info = fn
    )
  }
  # And through the public entry points, which is where a user meets it.
  expect_error(
    table_continuous(d, select = api00),
    class = "spicy_wrong_regime"
  )
  expect_error(
    table_categorical(d, select = stype),
    class = "spicy_wrong_regime"
  )
})

test_that("the plain refusal is untouched for a non-design non-frame", {
  # The design branch sits ABOVE the historical one; that one must
  # still answer exactly as before, class and message.
  err <- expect_error(
    .check_data_frame(1:10, "table_continuous"),
    class = "spicy_invalid_data"
  )
  expect_identical(conditionMessage(err), "`data` must be a data.frame.")
  expect_identical(.check_data_frame(mtcars, "table_continuous"), mtcars)
})
