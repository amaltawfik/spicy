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

test_that("`.is_survey_design()` recognizes every design class it must", {
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

test_that("P1 supports linearized and replicate designs, refuses the rest", {
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

test_that("`.drop_unweighted_levels()` reads the weights, not the rows", {
  f <- factor(c("a", "b", "c", "c"), levels = c("a", "b", "c", "z"))
  # Never observed, and observed only at weight zero: both go, and the
  # rows behind the second become NA rather than a level nobody weighs.
  out <- .drop_unweighted_levels(f, c(2, 3, 0, 0))
  expect_identical(levels(out), c("a", "b"))
  expect_identical(as.character(out), c("a", "b", NA, NA))

  # With every weight positive it is `droplevels()` and nothing more,
  # which is why an ordinary design cannot see this function.
  same <- .drop_unweighted_levels(f, c(2, 3, 1, 1))
  expect_identical(levels(same), c("a", "b", "c"))
  expect_identical(as.character(same), as.character(f))

  # A character vector arrives here from a `by` column, and an
  # all-unweighted variable comes back empty rather than erroring.
  expect_identical(
    levels(.drop_unweighted_levels(c("x", "y"), c(1, 0))),
    "x"
  )
  expect_identical(nlevels(.drop_unweighted_levels(f, c(0, 0, 0, 0))), 0L)
})

test_that("`.design_weights()` asks by name, and survey still means it", {
  skip_if_not_installed("survey")
  # On a replicate design the argument is load-bearing: the bare
  # default is `type = "replication"` and returns the whole 183 x 15
  # replication matrix, which sums to 2745 -- not a weight total at
  # all.
  r <- .svy_fixture("rep1")
  expect_equal(sum(.design_weights(r)), 6194.0003242492676, tolerance = 1e-12)
  expect_identical(dim(stats::weights(r)), c(183L, 15L))
  expect_equal(sum(stats::weights(r)), 2745, tolerance = 1e-9)

  # On a `survey.design` it is right BY ACCIDENT: the method has no
  # `type` formal, so the request is swallowed by `...` and `1 / prob`
  # comes back whatever anyone asks for. This is the tripwire -- the
  # day survey gives that method a `type` vocabulary of its own, the
  # accessor changes meaning and this goes red first.
  m <- utils::getS3method(
    "weights",
    "survey.design",
    envir = asNamespace("survey")
  )
  expect_false("type" %in% names(formals(m)))
  d <- .svy_fixture("clus1")
  expect_equal(unname(.design_weights(d)), unname(1 / d$prob))
})

# ---- design metadata ------------------------------------------------------

test_that("`.design_meta()` reads a linearized design through public slots", {
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

test_that("`.design_meta()` counts the rows of non-zero weight, as survey does", {
  skip_if_not_installed("survey", "4.5")
  data(api, package = "survey", envir = environment())
  # survey's convention (`unwtd.count()`, `degf()`): a row at weight zero is
  # out of the sample. `n_rows` keeps every row for the scheme.
  d <- apistrat
  d$pw[c(1L, 2L, 150L)] <- 0
  zero <- survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = d,
    fpc = ~fpc
  )
  m <- .design_meta(zero)
  expect_identical(m$n_obs, 197L)
  expect_identical(m$n_rows, 200L)
  expect_identical(
    m$n_obs,
    as.integer(survey::unwtd.count(~api00, zero)[[1L]])
  )
  # The rows of zero weight invent no stage of clusters.
  expect_identical(
    .design_scheme_parts(m),
    .design_scheme_parts(.design_meta(.svy_fixture("strat")))
  )
  expect_identical(.design_meta(survey::as.svrepdesign(zero))$n_obs, 197L)
  # `subset()` keeps the rows of a calibrated design at weight zero.
  cal <- subset(.svy_fixture("cal"), stype != "H")
  m <- .design_meta(cal)
  expect_identical(m$n_rows, 183L)
  expect_identical(m$n_obs, 169L)
})

# ---- the primary units of the design line ---------------------------------

# Rows of weight zero that hold whole primary sampling units, and the
# shapes where they cannot. `subset()` drops the rows of an ordinary or a
# replicate design and keeps them at weight zero on a calibrated or
# post-stratified one (`[.survey.design2` and `[.svyrep.design`, survey
# 4.5).
.svy_psu_cases <- function() {
  skip_if_not_installed("survey", "4.5")
  data(api, package = "survey", envir = environment())
  pop <- data.frame(stype = c("E", "H", "M"), Freq = c(4421, 755, 1018))
  totals <- c(`(Intercept)` = 6194, stypeH = 755, stypeM = 1018)
  clus1 <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1,
    fpc = ~fpc
  )
  zs <- apistrat
  zs$pw[c(1L, 2L, 150L)] <- 0
  # District 61 at weight zero, and one school of district 716.
  zc <- apiclus1
  zc$pw[zc$dnum == 61] <- 0
  zc$pw[which(zc$dnum == 716)[[1L]]] <- 0
  clus2 <- survey::svydesign(
    id = ~ dnum + snum,
    fpc = ~ fpc1 + fpc2,
    data = apiclus2
  )
  srs <- survey::svydesign(id = ~1, weights = ~pw, data = apisrs, fpc = ~fpc)
  list(
    zero_strat = survey::svydesign(
      id = ~1,
      strata = ~stype,
      weights = ~pw,
      data = zs,
      fpc = ~fpc
    ),
    zero_clus = survey::svydesign(
      id = ~dnum,
      weights = ~pw,
      data = zc,
      fpc = ~fpc
    ),
    subset_plain = subset(clus1, stype == "H"),
    subset_post = subset(
      survey::postStratify(clus1, ~stype, pop),
      stype == "H"
    ),
    subset_cal = subset(survey::calibrate(clus1, ~stype, totals), stype == "H"),
    two_stage = subset(survey::postStratify(clus2, ~stype, pop), stype == "H"),
    strata = subset(
      survey::postStratify(.svy_psu_strata_clusters(), ~stype, pop),
      stype != "H"
    ),
    replicate = subset(
      survey::postStratify(survey::as.svrepdesign(clus1), ~stype, pop),
      stype == "H"
    ),
    no_clusters = subset(survey::postStratify(srs, ~stype, pop), stype == "H")
  )
}

# apistrat as districts within school types: strata and clusters at once.
.svy_psu_strata_clusters <- function() {
  data(api, package = "survey", envir = environment())
  survey::svydesign(
    id = ~dnum,
    strata = ~stype,
    weights = ~pw,
    data = apistrat,
    nest = TRUE
  )
}

# That design post-stratified, with stratum H cut down to its first
# district: a stratum left with a single primary unit, which survey
# reports only under `options(survey.adjust.domain.lonely = TRUE)`.
.svy_psu_lonely <- function() {
  data(api, package = "survey", envir = environment())
  pop <- data.frame(stype = c("E", "H", "M"), Freq = c(4421, 755, 1018))
  first_h <- apistrat$dnum[apistrat$stype == "H"][[1L]]
  subset(
    survey::postStratify(.svy_psu_strata_clusters(), ~stype, pop),
    stype != "H" | dnum == first_h
  )
}

test_that("the design line counts the primary units of non-zero weight", {
  cases <- .svy_psu_cases()
  lines <- vapply(
    cases,
    function(d) .design_note_lines(.design_meta(d))[[1L]],
    character(1)
  )
  expect_identical(
    unname(lines),
    c(
      "Design: stratified (stype), with finite population correction; 194 degrees of freedom.",
      "Design: cluster (dnum), 14 PSU, with finite population correction; 13 degrees of freedom.",
      "Design: cluster (dnum), 8 PSU, with finite population correction; 7 degrees of freedom.",
      "Design: cluster (dnum), 8 PSU, with finite population correction, calibrated / post-stratified; 7 degrees of freedom.",
      "Design: cluster (dnum), 8 PSU, with finite population correction, calibrated / post-stratified; 7 degrees of freedom.",
      "Design: cluster (dnum), 2 sampling stages, 14 PSU, with finite population correction, calibrated / post-stratified; 13 degrees of freedom.",
      "Design: stratified (stype), cluster (dnum), 120 PSU, calibrated / post-stratified; 118 degrees of freedom.",
      "Design: replicate weights (JK1), 15 replicates; 7 degrees of freedom.",
      "Design: simple random sample, with finite population correction, calibrated / post-stratified; 24 degrees of freedom."
    )
  )
  # PSU and strata counted on the rows `degf.survey.design2()` counts, so
  # that the line, the N and the df describe the same observations.
  for (d in cases[names(cases) != "replicate"]) {
    m <- .design_meta(d)
    expect_identical(m$n_psu - max(m$n_strata, 1L), as.integer(survey::degf(d)))
  }
  expect_identical(.design_meta(cases$strata)$n_strata, 2L)
  # A plain subset and a post-stratified one keep the same 8 districts.
  expect_identical(
    .design_meta(cases$subset_post)$n_psu,
    .design_meta(cases$subset_plain)$n_psu
  )
})

test_that("a stratum left with one primary unit is counted once", {
  withr::local_options(
    survey.lonely.psu = "adjust",
    survey.adjust.domain.lonely = TRUE
  )
  skip_if_not_installed("survey", "4.5")
  r <- .svy_collect_warnings(.svy_psu_lonely())
  expect_identical(
    vapply(r$warnings, conditionMessage, character(1)),
    "1 strata have only one PSU in this subset."
  )
  m <- .design_meta(r$value)
  expect_identical(c(m$n_psu, m$n_strata, m$n_obs), c(121L, 3L, 151L))
  expect_identical(
    .design_note_lines(m)[[1L]],
    "Design: stratified (stype), cluster (dnum), 121 PSU, calibrated / post-stratified; 118 degrees of freedom."
  )
})

test_that("every table and regression note carries the same count", {
  cases <- .svy_psu_cases()
  d <- cases$subset_post
  line <- .design_note_lines(.design_meta(d))[[1L]]
  has_line <- function(x) grepl(line, attr(x, "note"), fixed = TRUE)
  expect_true(has_line(table_continuous_svy(d, api00, output = "long")))
  expect_true(has_line(table_categorical_svy(d, awards, output = "long")))
  expect_true(has_line(table_battery_svy(
    d,
    c(sch.wide, awards),
    output = "long"
  )))
  d$variables$event <- as.integer(d$variables$sch.wide == "Yes")
  glm_fit <- suppressWarnings(survey::svyglm(api00 ~ ell, design = d))
  cox_fit <- survey::svycoxph(survival::Surv(api00, event) ~ ell, design = d)
  # The regression line names residual df, the rest of it is the same.
  scheme <- .design_scheme_parts(.design_meta(d))
  expect_identical(
    scheme,
    "cluster (dnum), 8 PSU, with finite population correction, calibrated / post-stratified"
  )
  for (fit in list(glm_fit, cox_fit)) {
    note <- attr(table_regression(fit, output = "long"), "note")
    expect_true(grepl(paste0("Design: ", scheme, ";"), note, fixed = TRUE))
  }
  withr::local_options(spicy.language = "fr")
  expect_match(
    attr(table_continuous_svy(d, api00, output = "long"), "note"),
    "8 UPS",
    fixed = TRUE
  )
  # survey 4.5 cannot fit svyolr() on rows of weight zero (`weights()[keep]`
  # against the full model matrix), so its line never meets them. The day
  # it can, this goes red and svyolr joins the cases above.
  d$variables$grade <- ordered(cut(d$variables$api00, c(0, 600, 700, 1000)))
  expect_error(suppressWarnings(survey::svyolr(grade ~ ell, design = d)))
})

test_that("the long outputs of these designs do not move with the count", {
  # Recorded before the count changed (main 18e6ec09), so a change that
  # moved an estimate, a standard error, a df or a test goes red here.
  cases <- .svy_psu_cases()
  withr::local_options(
    survey.lonely.psu = "adjust",
    survey.adjust.domain.lonely = TRUE
  )
  cases$lonely <- suppressWarnings(.svy_psu_lonely())
  numbers <- function(x) {
    x <- as.data.frame(x)
    keep <- vapply(x, function(v) is.numeric(v) && !all(is.na(v)), logical(1))
    lapply(x[keep], unname)
  }
  # The warnings these designs raise, and only those: survey's lonely
  # stratum and stats' zero weights in the group comparison's svyglm().
  expected <- c(
    "1 strata have only one PSU in this subset.",
    gettext(
      "observations with zero weight not used for calculating dispersion",
      domain = "R-stats"
    )
  )
  quiet <- function(expr) {
    r <- .svy_collect_warnings(expr)
    msgs <- vapply(r$warnings, conditionMessage, character(1))
    expect_true(all(sub("\n.*", "", msgs) %in% expected))
    numbers(r$value)
  }
  out <- lapply(cases, function(d) {
    d$variables$event <- as.integer(d$variables$sch.wide == "Yes")
    glm_fit <- suppressWarnings(survey::svyglm(api00 ~ ell + meals, design = d))
    list(
      continuous = quiet(table_continuous_svy(
        d,
        c(api00, api99),
        by = sch.wide,
        statistic = TRUE,
        output = "long"
      )),
      categorical = quiet(table_categorical_svy(
        d,
        sch.wide,
        by = awards,
        proportion_ci = TRUE,
        output = "long"
      )),
      battery = quiet(table_battery_svy(
        d,
        c(sch.wide, awards),
        output = "long"
      )),
      svyglm = quiet(table_regression(glm_fit, output = "long")),
      svycoxph = quiet(table_regression(
        survey::svycoxph(survival::Surv(api00, event) ~ ell, design = d),
        output = "long"
      ))
    )
  })
  # svyolr() on an ordinary subset, the only kind it fits (see above).
  olr <- subset(.svy_fixture("clus1"), stype != "H")
  olr$variables$grade <- ordered(cut(olr$variables$api00, c(0, 600, 700, 1000)))
  out$svyolr <- numbers(table_regression(
    survey::svyolr(grade ~ ell, design = olr),
    output = "long"
  ))
  expect_snapshot_value(out, style = "deparse")
})

# ---- the design's own columns ---------------------------------------------

.svy_columns_data <- function() {
  data.frame(
    id = 1000L + (1:40 * 7L) %% 40L,
    w = 1 + (1:40 %% 3) / 2,
    psu = rep(1:8, each = 5),
    st = rep(1:2, each = 20),
    sex = factor(rep(c("F", "M"), 20)),
    frac = 0.08,
    y = sin(1:40),
    rep1 = rep(c(0.5, 1.5), 20),
    rep2 = rep(c(1.5, 0.5), 20),
    p = 1 / (1 + (1:40 %% 3) / 2)
  )
}

test_that("`.design_columns()` reads the weights, clusters and fpc of a design", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  clus1 <- c(
    "sampling weights" = "pw",
    cluster = "dnum",
    "finite population correction" = "fpc"
  )
  des <- .svy_fixture("clus1")
  expect_identical(.design_columns(des), clus1)
  # The slots outlive the steps that overwrite `design$call`.
  pop <- data.frame(stype = c("E", "H", "M"), Freq = c(4421, 755, 1018))
  steps <- list(
    des[apiclus1$stype == "E", ],
    subset(des, stype == "E"),
    stats::update(des, gain = api00 - api99),
    survey::postStratify(des, ~stype, pop),
    .svy_fixture("cal"),
    survey::rake(des, list(~stype), list(pop)),
    survey::trimWeights(survey::postStratify(des, ~stype, pop), upper = 45)
  )
  for (step in steps) {
    expect_identical(.design_columns(step), clus1)
  }
  # Every stage. The strata are no design column.
  expect_identical(
    .design_columns(.svy_fixture("clus2")),
    c(
      "stage 1 cluster" = "dnum",
      "stage 2 cluster" = "snum",
      "stage 1 finite population correction" = "fpc1",
      "stage 2 finite population correction" = "fpc2"
    )
  )
  expect_identical(
    .design_columns(.svy_fixture("strat")),
    c("sampling weights" = "pw", "finite population correction" = "fpc")
  )
  # A replicate design made from one records none of them.
  expect_length(.design_columns(.svy_fixture("rep1")), 0L)
})

test_that("`.design_columns()` keeps a name survey made up out", {
  skip_if_not_installed("survey")
  d <- .svy_columns_data()
  # `ids = ~1` names its cluster `id`: the data's own `id` is a variable,
  # nested in the strata or not, and even when it is the row number. One
  # row per primary sampling unit is no clustering, declared or not.
  for (data in list(d, transform(d, id = seq_len(nrow(d))))) {
    expect_identical(
      .design_columns(survey::svydesign(ids = ~1, weights = ~w, data = data)),
      c("sampling weights" = "w")
    )
    expect_identical(
      .design_columns(survey::svydesign(
        ids = ~1,
        strata = ~st,
        weights = ~w,
        data = data,
        nest = TRUE
      )),
      c("sampling weights" = "w")
    )
  }
  expect_identical(
    .design_columns(survey::svydesign(ids = ~id, weights = ~w, data = d)),
    c("sampling weights" = "w")
  )
  expect_identical(
    .design_columns(survey::svydesign(
      ids = ~psu,
      strata = ~st,
      weights = ~w,
      data = d,
      nest = TRUE
    )),
    c("sampling weights" = "w", cluster = "psu")
  )
  # A vector of weights is no column; probabilities and a sampling
  # fraction are columns.
  expect_identical(
    .design_columns(survey::svydesign(ids = ~psu, weights = d$w, data = d)),
    c(cluster = "psu")
  )
  expect_identical(
    .design_columns(survey::svydesign(ids = ~1, probs = ~p, data = d)),
    c("sampling probabilities" = "p")
  )
  expect_identical(
    .design_columns(survey::svydesign(
      ids = ~psu,
      fpc = ~frac,
      weights = ~w,
      data = d
    )),
    c(
      "sampling weights" = "w",
      cluster = "psu",
      "finite population correction" = "frac"
    )
  )
})

test_that("`.design_columns()` reads the columns of a replicate design", {
  skip_if_not_installed("survey")
  d <- .svy_columns_data()
  reps <- c("replicate weights" = "rep1", "replicate weights" = "rep2")
  all_cols <- c(reps, "sampling weights" = "w")
  des <- survey::svrepdesign(
    data = d,
    repweights = "rep[0-9]+",
    weights = ~w,
    type = "bootstrap"
  )
  expect_identical(.design_columns(des), all_cols)
  expect_identical(.design_columns(des[d$y > 0, ]), all_cols)
  expect_identical(
    .design_columns(survey::svrepdesign(
      data = d,
      repweights = ~ rep1 + rep2,
      weights = ~w,
      type = "bootstrap"
    )),
    all_cols
  )
  expect_identical(
    .design_columns(survey::svrepdesign(
      data = d,
      repweights = survey::compressWeights(d[c("rep1", "rep2")]),
      weights = ~w,
      type = "bootstrap"
    )),
    all_cols
  )
  # The call is read as typed: a vector, or a formula held in an object,
  # names no column.
  expect_identical(
    .design_columns(survey::svrepdesign(
      data = d,
      repweights = "rep[0-9]+",
      weights = d$w,
      type = "bootstrap"
    )),
    reps
  )
  wts <- ~w
  expect_identical(
    .design_columns(survey::svrepdesign(
      data = d,
      repweights = "rep[0-9]+",
      weights = wts,
      type = "bootstrap"
    )),
    reps
  )
  # Every later step keeps the replicate weights. The sampling weights
  # are a bare vector, named in the call alone, which these steps
  # overwrite. (survey 4.5 calibrates replicate weights held in a data
  # frame only with `compress = FALSE`.)
  pop <- data.frame(sex = c("F", "M"), Freq = c(400, 600))
  steps <- list(
    subset(des, y > 0),
    stats::update(des, y2 = y^2),
    survey::postStratify(des, ~sex, pop),
    survey::calibrate(
      des,
      ~sex,
      c(`(Intercept)` = 1000, sexM = 600),
      compress = FALSE
    ),
    survey::rake(des, list(~sex), list(pop)),
    survey::trimWeights(des, upper = 1.5)
  )
  for (step in steps) {
    expect_identical(.design_columns(step), reps)
  }
})

# ---- the self-documenting footer ------------------------------------------

test_that("the design footer says what the design is, in three sentences", {
  skip_if_not_installed("survey")
  expect_identical(
    .design_note_lines(.design_meta(.svy_fixture("strat"))),
    c(
      "Design: stratified (stype), with finite population correction; 197 degrees of freedom.",
      "Std. errors: Design-based (Taylor linearization).",
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
  # two regimes: a replicate design does not linearize anything. It
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
  # ("Standard errors: Taylor linearization (survey)." against "Std.
  # errors: Design-based (Taylor linearization).") and the difference
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
    "degrees of freedom vary by group (9\u201314)",
    fixed = TRUE
  )
  # A span whose ends coincide is one number, not a range: a two-group
  # table where both domains keep every cluster must not print
  # "vary by group (14-14)" (with the en dash).
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
