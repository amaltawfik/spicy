# ---------------------------------------------------------------------------
# The footer names the variance estimator the DESIGN uses.
#
# The label was a literal, so a replicate-weight fit -- whose standard
# errors come from 15 or 104 refits and where no linearisation happens
# at all -- was footnoted "Design-based (Taylor linearisation)", while
# the descriptive twin on the SAME design said "replicate weights".
#
# The index is the variance mechanism, not the design's R class and not
# whether the twins support that design: a without-replacement pps
# design is linearised (ppsvar()) and unsupported there, so keying on
# support would have moved a correct label to a wrong one.
# ---------------------------------------------------------------------------

.lbl_designs <- function() {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dstrat <- survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = apistrat,
    fpc = ~fpc
  )
  dclus1 <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1,
    fpc = ~fpc
  )
  list(strat = dstrat, clus = dclus1, apistrat = apistrat)
}

.lbl_of <- function(design) {
  fit <- survey::svyglm(api00 ~ ell, design = design)
  as_regression_frame(fit)$info$vcov_label
}


test_that("a linearised design keeps the label it has always had", {
  d <- .lbl_designs()
  expect_identical(.lbl_of(d$strat), "Design-based (Taylor linearisation)")
  expect_identical(.lbl_of(d$clus), "Design-based (Taylor linearisation)")
  # And it is a registry key now, not a literal.
  expect_identical(
    spicy_str("note_vcov_design_taylor"),
    "Design-based (Taylor linearisation)"
  )
})

test_that("a replicate design is named by its scheme", {
  d <- .lbl_designs()
  for (scheme in c("JK1", "bootstrap", "subbootstrap", "mrbbootstrap")) {
    rep <- survey::as.svrepdesign(d$clus, type = scheme)
    expect_identical(
      .lbl_of(rep),
      sprintf("Design-based (replicate weights, %s)", scheme),
      info = scheme
    )
  }
  # JKn and BRR need strata.
  for (scheme in c("JKn", "BRR")) {
    rep <- suppressWarnings(survey::as.svrepdesign(d$strat, type = scheme))
    expect_identical(
      .lbl_of(rep),
      sprintf("Design-based (replicate weights, %s)", scheme),
      info = scheme
    )
  }
  # Fay's method is BRR with a shrinkage factor, and survey records it
  # as "BRR" unless a rho is supplied: the label follows what the design
  # says about itself, not what the caller typed.
  fay <- suppressWarnings(survey::as.svrepdesign(
    d$strat,
    type = "Fay",
    fay.rho = 0.3
  ))
  expect_identical(.lbl_of(fay), "Design-based (replicate weights, Fay)")
  plain_fay <- suppressWarnings(survey::as.svrepdesign(d$strat, type = "Fay"))
  expect_identical(.lbl_of(plain_fay), "Design-based (replicate weights, BRR)")
})

test_that("a replicate design with no usable scheme drops the parenthesis", {
  d <- .lbl_designs()
  data(api, package = "survey", envir = environment())
  # "other" is a LEGAL value of svrepdesign(type =) that identifies
  # nothing: printing it would be a label without information. Built
  # from a real jackknife design so the fit itself is sound and only the
  # scheme name is missing.
  jk <- survey::as.svrepdesign(d$clus, type = "JK1")
  other <- survey::svrepdesign(
    data = jk$variables,
    repweights = stats::weights(jk, "analysis"),
    weights = stats::weights(jk, "sampling"),
    type = "other",
    scale = jk$scale,
    rscales = jk$rscales,
    combined.weights = TRUE
  )
  expect_identical(.lbl_of(other), "Design-based (replicate weights)")
  # Same answer when the slot is absent altogether.
  rep <- survey::as.svrepdesign(d$clus, type = "JK1")
  fit <- survey::svyglm(api00 ~ ell, design = rep)
  fit$survey.design$type <- NULL
  expect_identical(
    as_regression_frame(fit)$info$vcov_label,
    "Design-based (replicate weights)"
  )
})

test_that("calibrated, post-stratified and pps designs stay linearised", {
  d <- .lbl_designs()
  cal <- survey::calibrate(
    d$strat,
    ~stype,
    pop = c(`(Intercept)` = 6194, stypeH = 755, stypeM = 1018)
  )
  expect_identical(.lbl_of(cal), "Design-based (Taylor linearisation)")
  ps <- survey::postStratify(
    d$strat,
    ~stype,
    data.frame(stype = c("E", "H", "M"), Freq = c(4421, 755, 1018))
  )
  expect_identical(.lbl_of(ps), "Design-based (Taylor linearisation)")
  # A without-replacement pps design leaves NO class marker -- it is a
  # plain survey.design2 -- and its variance IS a linearisation. Keying
  # the label on `.is_supported_design()` would have demoted it.
  strat <- d$apistrat
  strat$fpcfrac <- strat$pw / sum(strat$pw)
  pps <- survey::svydesign(
    id = ~1,
    fpc = ~fpcfrac,
    weights = ~pw,
    data = strat,
    pps = "brewer"
  )
  expect_identical(.lbl_of(pps), "Design-based (Taylor linearisation)")
  expect_false(spicy:::.is_supported_design(pps))
})

test_that("a two-phase design gets its own key, never a class name", {
  d <- .lbl_designs()
  tp <- survey::twophase(
    id = list(~1, ~1),
    subset = ~ I(comp.imp == "Yes"),
    data = d$apistrat
  )
  expect_identical(.lbl_of(tp), "Design-based (two-phase design)")
  # twophase2 also inherits survey.design, so the order of the tests is
  # load-bearing: read as a linearised design it would be mislabelled.
  expect_true(inherits(tp, "survey.design"))
})

test_that("a detached design degrades to the bare label", {
  d <- .lbl_designs()
  fit <- survey::svyglm(api00 ~ ell, design = d$strat)
  fit$survey.design <- NULL
  expect_identical(as_regression_frame(fit)$info$vcov_label, "Design-based")
  # An object that is a design of no known kind: still no R class name.
  fit$survey.design <- structure(list(), class = "some_future_design")
  expect_identical(as_regression_frame(fit)$info$vcov_label, "Design-based")
})

test_that("the rendered footer of a replicate fit names replicate weights", {
  d <- .lbl_designs()
  rep <- survey::as.svrepdesign(d$clus, type = "JK1")
  fit <- survey::svyglm(api00 ~ ell, design = rep)
  out <- paste(
    utils::capture.output(print(table_regression(fit))),
    collapse = "\n"
  )
  expect_match(
    out,
    "Std. errors: Design-based (replicate weights, JK1).",
    fixed = TRUE
  )
  expect_false(grepl("Taylor", out, fixed = TRUE))
})

test_that("an explicit vcov_label still wins over the design", {
  d <- .lbl_designs()
  fit <- survey::svyglm(api00 ~ ell, design = d$strat)
  fr <- as_regression_frame(fit, vcov_label = "Handed in by the caller")
  expect_identical(fr$info$vcov_label, "Handed in by the caller")
})
