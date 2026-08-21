# ---------------------------------------------------------------------------
# What a design table counts, and what it says about the design it was
# fitted under.
#
# Two changes that belong together:
#   * the default fit-statistic set. Design fits inherit "glm" (svyglm)
#     or "coxph" (svycoxph), so they were taking those classes' sets --
#     two pseudo-R-squareds and a Cox AIC, none of which exists without a
#     likelihood. The branches are independent PREDICATES that
#     concatenate, so a design branch placed "before" them removes
#     nothing: the design fits are EXCLUDED from any_glm / any_coxph, on
#     the is_bayes / is_gee idiom the file already uses.
#   * the footer. A regression under a design used to disclose neither
#     the sampling scheme nor the degrees of freedom its own t-tests use,
#     while the descriptive twin on the same design disclosed both.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fsf_designs <- function() {
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
  list(
    strat = dstrat,
    clus = dclus1,
    rep = survey::as.svrepdesign(dclus1, type = "JK1"),
    apistrat = apistrat,
    apiclus1 = apiclus1
  )
}

.fsf_render <- function(...) {
  paste(
    utils::capture.output(suppressWarnings(print(table_regression(...)))),
    collapse = "\n"
  )
}

.fsf_rows <- function(out) {
  strsplit(out, "\n", fixed = TRUE)[[1L]]
}

.fsf_has_row <- function(out, label) {
  any(grepl(paste0("^\\s*", label, "\\s"), .fsf_rows(out)))
}


# ---- 1. The default fit-statistic set of a design fit ---------------------

test_that("a svyglm reports n, the weighted n, and the design AIC", {
  d <- .fsf_designs()
  fit <- survey::svyglm(api00 ~ ell, design = d$strat)
  out <- .fsf_render(fit, show_columns = c("b", "p"))
  expect_true(.fsf_has_row(out, "n"))
  expect_true(.fsf_has_row(out, "Weighted n"))
  expect_true(.fsf_has_row(out, "AIC"))
  expect_match(out, "6194", fixed = TRUE)
  # NOT the glm set: there is no likelihood, so no pseudo-R-squared.
  expect_false(grepl("McFadden", out, fixed = TRUE))
  expect_false(grepl("Nagelkerke", out, fixed = TRUE))
})

test_that("a svyolr reports the two counts and nothing likelihood-shaped", {
  d <- .fsf_designs()
  dat <- d$apistrat
  dat$grade <- ordered(cut(dat$api00, c(0, 600, 700, 1000)))
  des <- survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = dat,
    fpc = ~fpc
  )
  fit <- survey::svyolr(grade ~ ell, design = des)
  out <- .fsf_render(fit, show_columns = c("b"), show_thresholds = FALSE)
  expect_true(.fsf_has_row(out, "n"))
  expect_true(.fsf_has_row(out, "Weighted n"))
  expect_match(out, "6194", fixed = TRUE)
  expect_false(grepl("AIC", out, fixed = TRUE))
  expect_false(grepl("McFadden", out, fixed = TRUE))
})

test_that("a svycoxph reports the events too, and no Cox AIC", {
  skip_if_not_installed("survival")
  data(pbc, package = "survival", envir = environment())
  pbc$randomized <- with(pbc, !is.na(trt) & trt > 0)
  bias <- stats::glm(
    randomized ~ age * edema,
    data = pbc,
    family = stats::binomial
  )
  pbc$sw <- 1 / stats::predict(bias, type = "response")
  des <- survey::svydesign(
    id = ~1,
    prob = ~sw,
    strata = ~edema,
    data = subset(pbc, randomized)
  )
  fit <- survey::svycoxph(
    survival::Surv(time, status > 0) ~ log(bili) + protime,
    design = des
  )
  out <- .fsf_render(fit, show_columns = c("b", "p"))
  expect_true(.fsf_has_row(out, "n"))
  expect_true(.fsf_has_row(out, "Weighted n"))
  expect_true(.fsf_has_row(out, "N events"))
  # The coxph branch would have queued an AIC that cannot be computed.
  expect_false(grepl("AIC", out, fixed = TRUE))
})

test_that("a plain glm or Cox in the same table keeps its own statistics", {
  d <- .fsf_designs()
  fit <- survey::svyglm(api00 ~ ell, design = d$strat)
  plain <- stats::glm(api00 ~ ell, data = d$apistrat)
  out <- .fsf_render(list(fit, plain), show_columns = c("b", "p"))
  # The design fit is excluded from any_glm; the plain glm is not, so its
  # pseudo-R-squareds are still queued and the design column en-dashes.
  expect_match(out, "McFadden", fixed = TRUE)
  expect_match(out, "Nagelkerke", fixed = TRUE)
  # And the two counts lead the block rather than trailing it: the
  # design branch runs last, so a bare append would strand them.
  rows <- .fsf_rows(out)
  i_n <- grep("^\\s*n\\s", rows)[1L]
  i_w <- grep("^\\s*Weighted n\\s", rows)[1L]
  i_r2 <- grep("McFadden", rows)[1L]
  expect_equal(i_w, i_n + 1L)
  expect_lt(i_w, i_r2)
})

test_that("a design fit alone never carries a statistic it cannot compute", {
  d <- .fsf_designs()
  fit <- survey::svyglm(api00 ~ ell, design = d$strat)
  fr <- as_regression_frame(fit)
  # Every one of these returns a NUMBER from survey, and none of them
  # describes the fit: the deviance is on the scale of the sum of the
  # weights, the log-likelihood is a weighted quasi-likelihood.
  expect_true(is.na(fr$info$fit_stats$deviance))
  expect_true(is.na(fr$info$fit_stats$log_lik))
  expect_true(is.na(fr$info$fit_stats$sigma))
  expect_true(is.finite(stats::deviance(fit)))
  expect_gt(stats::deviance(fit), 1e5)
  # And an explicit token cannot bring them back.
  out <- .fsf_render(
    fit,
    show_columns = c("b"),
    show_fit_stats = c("nobs", "deviance", "sigma")
  )
  expect_false(grepl("Deviance", out, fixed = TRUE))
})


# ---- 2. The effective number of design parameters, opt-in ----------------

test_that("eff_p is available under its own name, and only for svyglm", {
  d <- .fsf_designs()
  fit <- survey::svyglm(api00 ~ ell, design = d$strat)
  fr <- as_regression_frame(fit)
  # The number that used to be printed as "AIC".
  expect_equal(
    fr$info$fit_stats$eff_p,
    unname(suppressWarnings(stats::AIC(fit))[["eff.p"]]),
    tolerance = 1e-12
  )
  expect_false(isTRUE(all.equal(
    fr$info$fit_stats$eff_p,
    fr$info$fit_stats$aic
  )))
  out <- .fsf_render(
    fit,
    show_columns = c("b"),
    show_fit_stats = c("nobs", "aic", "eff_p")
  )
  expect_true(.fsf_has_row(out, "Effective parameters"))
  expect_true(.fsf_has_row(out, "AIC"))
  # Not in the default set: it is a property of the design, not a
  # summary of the fit.
  expect_false(grepl(
    "Effective parameters",
    .fsf_render(fit, show_columns = c("b")),
    fixed = TRUE
  ))
  # A class that has no such number renders no such row.
  out2 <- .fsf_render(
    stats::glm(api00 ~ ell, data = d$apistrat),
    show_columns = c("b"),
    show_fit_stats = c("nobs", "eff_p")
  )
  expect_false(grepl("Effective parameters", out2, fixed = TRUE))
})


# ---- 3. The design line in the footer -------------------------------------

test_that("the footer names the scheme and the model's own degrees of freedom", {
  d <- .fsf_designs()
  fit <- survey::svyglm(api00 ~ ell + meals + stype, design = d$strat)
  out <- .fsf_render(fit, show_columns = c("b", "p"))
  expect_match(
    out,
    paste0(
      "Design: stratified (stype), with finite population correction; ",
      "193 residual degrees of freedom."
    ),
    fixed = TRUE
  )
  # 197 is the DESIGN's own df, which this table does not test at.
  expect_equal(survey::degf(d$strat), 197)
  expect_equal(stats::df.residual(fit), 193)
  expect_false(grepl("197 residual", out, fixed = TRUE))
  expect_false(grepl("197 degrees", out, fixed = TRUE))
})

test_that("a replicate design names its scheme in the same sentence", {
  d <- .fsf_designs()
  fit <- survey::svyglm(api00 ~ ell, design = d$rep)
  out <- .fsf_render(fit, show_columns = c("b"))
  expect_match(out, "Design: replicate weights (JK1)", fixed = TRUE)
  expect_match(out, "residual degrees of freedom.", fixed = TRUE)
})

test_that("a two-phase design discloses the df without inventing a scheme", {
  d <- .fsf_designs()
  tp <- survey::twophase(
    id = list(~1, ~1),
    subset = ~ I(comp.imp == "Yes"),
    data = d$apistrat
  )
  fit <- survey::svyglm(api00 ~ ell, design = tp)
  # .design_meta() reads slots a two-phase design does not carry: it
  # returns a shape with an empty PSU name, and the sentence builder then
  # fails on it ("argument is of length zero"). Which is why the frame
  # asks through the guard instead.
  expect_error(
    spicy:::.design_scheme_parts(spicy:::.design_meta(tp)),
    "length zero"
  )
  expect_null(spicy:::.design_meta_or_null(tp))
  out <- .fsf_render(fit, show_columns = c("b"))
  expect_match(out, "Tests use ", fixed = TRUE)
  expect_match(out, "residual degrees of freedom.", fixed = TRUE)
  expect_false(grepl("Design: ", out, fixed = TRUE))
  # And no R class name reaches the reader.
  expect_false(grepl("twophase2", out, fixed = TRUE))
})

test_that("the scheme describes the analytic sample, not the attached design", {
  skip_if_not_installed("survival")
  data(api, package = "survey", envir = environment())
  dat <- apistrat
  dat$ell[1:20] <- NA
  dat$t <- pmax(dat$api00 - 400, 1)
  dat$ev <- as.integer(dat$api00 > 650)
  des <- survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = dat,
    fpc = ~fpc
  )
  fit <- survey::svycoxph(survival::Surv(t, ev) ~ ell + stype, design = des)
  # svycoxph attaches the COMPLETE design (200 rows for a fit on 180).
  expect_equal(nrow(fit$survey.design), 200L)
  fr <- as_regression_frame(fit)
  expect_equal(fr$info$extras$design_meta$n_obs, 180L)
  expect_equal(fr$info$extras$design_degf_resid, 175)
  out <- .fsf_render(fit, show_columns = c("b"))
  expect_match(out, "175 residual degrees of freedom.", fixed = TRUE)
})

test_that("a detached design leaves the design line out entirely", {
  d <- .fsf_designs()
  fit <- survey::svyglm(api00 ~ ell, design = d$strat)
  fr <- as_regression_frame(fit)
  fr$info$extras$design_degf_resid <- NULL
  expect_null(spicy:::.format_design_for_frame(fr))
  # And a frame from another family contributes nothing to the block.
  plain <- as_regression_frame(stats::lm(api00 ~ ell, data = d$apistrat))
  expect_null(spicy:::.format_design_for_frame(plain))
  expect_null(spicy:::build_design_footer_block_from_frames(list(plain)))
})

test_that("two models on different designs get one line each", {
  d <- .fsf_designs()
  f1 <- survey::svyglm(api00 ~ ell, design = d$strat)
  f2 <- survey::svyglm(api00 ~ ell, design = d$clus)
  out <- .fsf_render(list(f1, f2), show_columns = c("b"))
  expect_match(out, "Model 1: Design: stratified (stype)", fixed = TRUE)
  expect_match(out, "Model 2: Design: cluster (dnum)", fixed = TRUE)
  # ...and two models on the SAME design get one shared line.
  f3 <- survey::svyglm(api00 ~ meals, design = d$strat)
  out2 <- .fsf_render(list(f1, f3), show_columns = c("b"))
  expect_equal(
    length(grep("Design: stratified", .fsf_rows(out2))),
    1L
  )
  expect_false(grepl("Model 1: Design:", out2, fixed = TRUE))
})

test_that("a table with no design fit is untouched", {
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  out <- .fsf_render(fit)
  expect_false(grepl("Design:", out, fixed = TRUE))
  expect_false(grepl("residual degrees of freedom", out, fixed = TRUE))
  expect_false(grepl("Weighted n", out, fixed = TRUE))
})


test_that("a design Cox beside an lm does not drag in the Cox AIC", {
  # The observable half of the exclusion. Without it `any_coxph` is true
  # of the design fit, which queues "aic" -- NA for the svycoxph, but a
  # real number for the lm, so an AIC row appears in a table whose Cox
  # arm cannot have one.
  skip_if_not_installed("survival")
  data(pbc, package = "survival", envir = environment())
  pbc$randomized <- with(pbc, !is.na(trt) & trt > 0)
  bias <- stats::glm(
    randomized ~ age * edema,
    data = pbc,
    family = stats::binomial
  )
  pbc$sw <- 1 / stats::predict(bias, type = "response")
  des <- survey::svydesign(
    id = ~1,
    prob = ~sw,
    strata = ~edema,
    data = subset(pbc, randomized)
  )
  fit <- survey::svycoxph(
    survival::Surv(time, status > 0) ~ log(bili),
    design = des
  )
  plain <- stats::lm(time ~ log(bili), data = subset(pbc, randomized))
  out <- .fsf_render(list(fit, plain), show_columns = c("b"))
  expect_false(grepl("AIC", out, fixed = TRUE))
  expect_true(.fsf_has_row(out, "N events"))
  # A PLAIN Cox beside the same lm keeps its own branch, AIC included.
  plain_cox <- survival::coxph(
    survival::Surv(time, status > 0) ~ log(bili),
    data = subset(pbc, randomized)
  )
  out2 <- .fsf_render(list(plain_cox, plain), show_columns = c("b"))
  expect_true(.fsf_has_row(out2, "AIC"))
})

test_that("queuing a glm statistic on a design fit can never print one", {
  # The other half of the exclusion is defence in depth: the two
  # pseudo-R-squareds the glm branch queues are NULL in a svyglm frame,
  # so the rows are dropped whether or not the branch fires. This is the
  # invariant that makes that true -- if a future change gave them a
  # value, the exclusion would become the only thing standing between a
  # design table and a statistic that has no definition under a design.
  d <- .fsf_designs()
  fr <- as_regression_frame(survey::svyglm(api00 ~ ell, design = d$strat))
  expect_null(fr$info$fit_stats$pseudo_r2)
  expect_null(fr$info$fit_stats$pseudo_r2_mcfadden)
  expect_null(fr$info$fit_stats$pseudo_r2_nagelkerke)
  expect_true(is.na(fr$info$fit_stats$r_squared))
  expect_true(is.na(fr$info$fit_stats$adj_r_squared))
})
