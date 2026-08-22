# ---------------------------------------------------------------------------
# as_regression_frame() for survey::svycoxph() and its replicate-weight
# sibling svrepcoxph.
#
# The two engines differ on almost every slot, so every witness that
# touches one touches both:
#   * the residual degrees of freedom live under `degf.resid` on one and
#     `degf.residual` on the other, and `$` cannot tell them apart
#     (`degf.resid` is a unique PREFIX of `degf.residual`);
#   * `AIC()` stops with two different messages, and `logLik()` SUCCEEDS
#     on the replicate engine, returning NA (df = 3);
#   * `deviance()` returns a negative number on one and a bare 0 on the
#     other.
#
# Plus the trap that belongs to this class alone: svycoxph attaches the
# COMPLETE design, before the incomplete rows are dropped, so anything
# read off `fit$survey.design` describes 200 schools for a fit on 180.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

# survey's own vignette example: pbc restricted to the randomised
# patients, inverse-probability weights from a bias model.
.scox_design <- function() {
  skip_if_not_installed("survey")
  skip_if_not_installed("survival")
  data(pbc, package = "survival", envir = environment())
  pbc$randomized <- with(pbc, !is.na(trt) & trt > 0)
  bias <- stats::glm(
    randomized ~ age * edema,
    data = pbc,
    family = stats::binomial
  )
  pbc$sw <- 1 / stats::predict(bias, type = "response")
  survey::svydesign(
    id = ~1,
    prob = ~sw,
    strata = ~edema,
    data = subset(pbc, randomized)
  )
}

.scox_fit <- function() {
  survey::svycoxph(
    survival::Surv(time, status > 0) ~ log(bili) + protime + albumin,
    design = .scox_design()
  )
}

.scox_rep_fit <- function() {
  rep_design <- survey::as.svrepdesign(.scox_design(), type = "JKn")
  survey::svycoxph(
    survival::Surv(time, status > 0) ~ log(bili) + protime + albumin,
    design = rep_design
  )
}

# apistrat with 20 missing `ell`: the fixture that separates the attached
# design from the analytic sample.
.scox_na_fit <- function() {
  skip_if_not_installed("survey")
  skip_if_not_installed("survival")
  data(api, package = "survey", envir = environment())
  d <- apistrat
  d$ell[1:20] <- NA
  d$t <- pmax(d$api00 - 400, 1)
  d$ev <- as.integer(d$api00 > 650)
  des <- survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = d,
    fpc = ~fpc
  )
  survey::svycoxph(survival::Surv(t, ev) ~ ell + stype, design = des)
}

.scox_b <- function(fr) {
  fr$coefs[fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE), ]
}


# ---- 1. Schema, coefficients, and the design variance --------------------

test_that("svycoxph produces a schema-valid frame on both engines", {
  for (fit in list(.scox_fit(), .scox_rep_fit())) {
    fr <- as_regression_frame(fit, model_id = "M1")
    expect_invisible(spicy:::validate_regression_frame(fr))
    expect_identical(fr$info$class, "svycoxph")
    expect_identical(fr$info$weights_kind, "sampling")
    expect_identical(fr$info$family$family, "cox")
  }
})

test_that("coefficients and design standard errors match survey, pinned", {
  fr <- as_regression_frame(.scox_fit())
  b <- .scox_b(fr)
  expect_identical(b$term, c("log(bili)", "protime", "albumin"))
  expect_equal(
    b$estimate,
    c(0.88976550380432229, 0.25992232676154414, -1.07947760097235812),
    tolerance = 1e-12
  )
  # The `robust se` column of summary(), i.e. sqrt(diag(vcov(fit))) --
  # NOT the naive `se(coef)` of the partial likelihood.
  expect_equal(
    b$std_error,
    c(0.088837962909104257, 0.081408806085693439, 0.214312163683408996),
    tolerance = 1e-12
  )
})

test_that("the replicate engine carries its own variance", {
  fr <- as_regression_frame(.scox_rep_fit())
  b <- .scox_b(fr)
  # Same point estimates (the same weighted partial likelihood), a
  # different variance (replicate weights, no linearisation).
  expect_equal(
    b$estimate,
    c(0.88976550380432229, 0.25992232676154414, -1.07947760097235812),
    tolerance = 1e-12
  )
  expect_equal(
    b$std_error,
    c(0.094502526074347518, 0.089508293885211337, 0.231085028862318242),
    tolerance = 1e-12
  )
  expect_identical(fr$info$vcov_label, "Design-based (replicate weights, JKn)")
})


# ---- 2. Degrees of freedom: two slots, one per engine ---------------------

test_that("the residual df is read exactly, not by prefix match", {
  cx <- .scox_fit()
  rcx <- .scox_rep_fit()
  # The fact the accessor exists for: one slot per engine, two names.
  expect_true("degf.resid" %in% names(cx))
  expect_false("degf.residual" %in% names(cx))
  expect_false("degf.resid" %in% names(rcx))
  expect_true("degf.residual" %in% names(rcx))
  # `$` silently partial-matches on the replicate engine; `[[` does not.
  expect_null(rcx[["degf.resid"]])
  expect_equal(rcx$degf.resid, 307)
  # The accessor answers 307 on BOTH, which is what regTermTest() uses.
  expect_equal(spicy:::.design_model_df(cx), 307)
  expect_equal(spicy:::.design_model_df(rcx), 307)
  expect_equal(
    survey::regTermTest(cx, ~protime)$ddf,
    spicy:::.design_model_df(cx)
  )
  # df.residual() has no method for this class: the fallback inside the
  # accessor must never be what answers here.
  expect_null(stats::df.residual(cx))
})

test_that("the residual df is read by name, and never by prefix", {
  # The `[[` chain of `.design_model_df()` is not observable on survey
  # 4.5: `degf.resid` is a unique prefix of `degf.residual`, so `$`
  # partial-matches to the same number on a svrepcoxph and to NULL on the
  # two non-Cox classes, and a `$`-based accessor answers identically on
  # all four. What it protects against is the prefix ceasing to be
  # unique, which nothing in survey guarantees -- so it is pinned on
  # objects where the two operators genuinely disagree.
  #
  # (a) a slot that merely SHARES the prefix answers `$`, and a
  #     `$`-based accessor would publish it as the residual df:
  odd <- structure(
    list(degf.residuals.scaled = 999, df.residual = 42),
    class = c("svycoxph", "coxph")
  )
  expect_equal(odd$degf.resid, 999)
  expect_null(odd[["degf.resid"]])
  expect_null(odd[["degf.residual"]])
  expect_equal(spicy:::.design_model_df(odd), 42)
  # (b) two slots sharing it make `$` AMBIGUOUS, so it returns NULL and a
  #     `$`-based accessor loses the number survey did write:
  amb <- structure(
    list(degf.residual = 307, degf.residual.adj = 1),
    class = c("svycoxph", "coxph")
  )
  expect_null(amb$degf.resid)
  expect_equal(spicy:::.design_model_df(amb), 307)
})

test_that("the rows are t at that df, with the p recomputed", {
  fr <- as_regression_frame(.scox_fit())
  b <- .scox_b(fr)
  expect_true(all(b$test_type == "t"))
  expect_true(all(b$df == 307))
  expect_equal(
    b$p_value,
    c(1.2881848911625273e-20, 0.0015551269635508738, 8.0852680153444051e-07),
    tolerance = 1e-12
  )
  # summary.svycoxph reports a z and its own p; reading that column would
  # have put a normal p under a t label. The first coefficient differs by
  # three orders of magnitude.
  expect_gt(b$p_value[1L] / 1.3016960903118669e-23, 500)
  expect_equal(
    b$ci_lower,
    b$estimate - stats::qt(0.975, df = 307) * b$std_error,
    tolerance = 1e-12
  )
  expect_equal(
    b$ci_lower[1L],
    0.71495715299170992,
    tolerance = 1e-12
  )
})


# ---- 3. The analytic sample, when rows are missing ------------------------

test_that("counts and df follow the analytic sample, not the attached design", {
  fit <- .scox_na_fit()
  # svycoxph attaches the design BEFORE dropping the incomplete rows --
  # alone among the four design classes.
  expect_equal(nrow(fit$survey.design), 200L)
  expect_equal(fit$n, 180L)
  fr <- as_regression_frame(fit)
  expect_identical(fr$info$n_obs, 180L)
  # 6194 is the whole design; 5487.27 is the population the fit describes.
  expect_equal(
    fr$info$fit_stats$weighted_nobs,
    5487.2699661254883,
    tolerance = 1e-12
  )
  expect_false(isTRUE(all.equal(
    fr$info$fit_stats$weighted_nobs,
    6193.9999580383301,
    tolerance = 1e-6
  )))
  # degf of the REDUCED design (177) minus three coefficients, plus one.
  expect_equal(unique(.scox_b(fr)$df), 175)
  expect_equal(fit[["degf.resid"]], 175)
})

test_that("the weighted n reaches the rendered table", {
  fit <- .scox_na_fit()
  out <- paste(
    utils::capture.output(print(table_regression(
      fit,
      show_fit_stats = c("nobs", "weighted_nobs", "n_events")
    ))),
    collapse = "\n"
  )
  expect_match(out, "5487", fixed = TRUE)
  expect_false(grepl("6194", out, fixed = TRUE))
})


# ---- 4. Counts, and the statistics that do not exist ----------------------

test_that("n is the subject count and n_events the event count", {
  for (fit in list(.scox_fit(), .scox_rep_fit())) {
    fr <- as_regression_frame(fit)
    # nobs() on a Cox fit is the number of EVENTS (survival's deliberate
    # convention), which is not the sample size.
    expect_equal(stats::nobs(fit), 144)
    expect_identical(fr$info$n_obs, 312L)
    expect_identical(fr$info$fit_stats$nobs, 312L)
    expect_identical(fr$info$fit_stats$n_events, 144L)
  }
})

test_that("no likelihood statistic is invented, on either engine", {
  for (fit in list(.scox_fit(), .scox_rep_fit())) {
    fr <- as_regression_frame(fit)
    fs <- fr$info$fit_stats
    for (k in c("aic", "bic", "log_lik", "deviance", "sigma")) {
      expect_true(is.na(fs[[k]]), info = paste(class(fit)[1L], k))
    }
    expect_null(fs$pseudo_r2)
  }
})

test_that("the refusals survey raises differ by engine, and none is caught late", {
  cx <- .scox_fit()
  rcx <- .scox_rep_fit()
  # Two engines, two messages. survey owns both AIC methods.
  expect_error(stats::AIC(cx), "No AIC for survey models")
  expect_error(stats::AIC(rcx))
  # The log-likelihood asymmetry lives in the SLOTS, and is asserted
  # there rather than through logLik(): the linearised engine moves the
  # value to `ll` and blanks `loglik`, the replicate engine writes
  # `loglik <- c(NA, NA)` and never sets `ll`. Not through the generic,
  # because a third package can re-register the method for this class
  # and reverse which engine answers -- `performance` does exactly that
  # (its `logLik.svycoxph` reads `object$ll[2]`, so with it loaded the
  # linearised engine returns a number and the replicate one errors).
  # Which is the reason the extractor never calls logLik() here at all.
  expect_length(cx$ll, 2L)
  expect_true(all(is.finite(cx$ll)))
  expect_null(cx$loglik)
  expect_null(rcx$ll)
  expect_length(rcx$loglik, 2L)
  expect_true(all(is.na(rcx$loglik)))
  # deviance(): survey's own method, reading those slots -- a negative
  # number on one engine, a bare 0 on the other. The zero is the
  # dangerous one: it looks like a result.
  expect_lt(stats::deviance(cx), 0)
  expect_identical(stats::deviance(rcx), 0)
  # Whichever of them answers, and whatever it answers, the frame
  # publishes none of it.
  for (fit in list(cx, rcx)) {
    expect_true(is.na(as_regression_frame(fit)$info$fit_stats$log_lik))
  }
})

test_that("a third package re-registering logLik cannot move the frame", {
  # `performance::logLik.svycoxph` wins dispatch for this class as soon
  # as performance is loaded, and returns a finite number on the
  # linearised engine. The frame must not change when it is.
  skip_if_not_installed("performance")
  cx <- .scox_fit()
  before <- as_regression_frame(cx)$info$fit_stats
  loadNamespace("performance")
  after <- as_regression_frame(cx)$info$fit_stats
  expect_true(is.na(after$log_lik))
  expect_identical(before, after)
})

test_that("an explicit fit-stat token cannot republish an absent number", {
  for (fit in list(.scox_fit(), .scox_rep_fit())) {
    out <- paste(
      utils::capture.output(print(table_regression(
        fit,
        show_fit_stats = c("nobs", "deviance", "aic", "bic")
      ))),
      collapse = "\n"
    )
    expect_false(grepl("Deviance", out, fixed = TRUE), info = class(fit)[1L])
    expect_false(grepl("AIC", out, fixed = TRUE), info = class(fit)[1L])
    expect_false(grepl("BIC", out, fixed = TRUE), info = class(fit)[1L])
  }
})


# ---- 5. Silence ----------------------------------------------------------

test_that("building a design-based Cox table prints nothing", {
  # survey's summary.svycoxph is
  #   function(object, ...) { print(object$survey.design, ...); NextMethod() }
  # so it prints the design on EVERY call, including when a single field
  # is read. The number of lines is a property of the fixture (32 here,
  # 29 on the replicate engine), so the assertion is that there are none.
  for (fit in list(.scox_fit(), .scox_rep_fit())) {
    expect_silent(fr <- as_regression_frame(fit))
    expect_length(
      utils::capture.output(invisible(as_regression_frame(fit))),
      0L
    )
    expect_length(
      utils::capture.output(invisible(table_regression(
        fit,
        output = "data.frame"
      ))),
      0L
    )
    # And summary() really would have printed.
    expect_gt(length(utils::capture.output(summary(fit))), 0L)
  }
})


# ---- 6. Hazard ratios, concordance, and the footer -----------------------

test_that("exponentiate gives hazard ratios and the footer names concordance", {
  fit <- .scox_fit()
  fr <- as_regression_frame(fit)
  conc <- fr$info$extras$concordance
  expect_equal(conc$c, 0.814019575962434683, tolerance = 1e-12)
  expect_equal(conc$se, 0.018377926900599289, tolerance = 1e-12)
  out <- paste(
    utils::capture.output(print(table_regression(fit, exponentiate = TRUE))),
    collapse = "\n"
  )
  expect_match(out, "HR", fixed = TRUE)
  expect_match(out, "Concordance C = 0.81 (SE = 0.02).", fixed = TRUE)
  expect_match(out, "2.43", fixed = TRUE)
  expect_match(
    out,
    "Survey-weighted Cox proportional hazards regression",
    fixed = TRUE
  )
})

test_that("the n_events column is available like it is for a plain Cox fit", {
  fit <- .scox_fit()
  out <- table_regression(
    fit,
    show_columns = c("b", "n_events"),
    output = "data.frame"
  )
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0L)
})


# ---- 7. What a design-based Cox refuses ----------------------------------

test_that("the survival estimands are refused before any row is built", {
  fit <- .scox_fit()
  for (args in list(
    list(show_columns = c("b", "rmst"), tau = 1000),
    list(show_columns = c("b", "risk_diff"), at_time = 1000),
    list(show_columns = c("b", "rmst_ci"), tau = 1000)
  )) {
    err <- expect_error(
      do.call(table_regression, c(list(fit), args)),
      class = "spicy_unsupported"
    )
    msg <- conditionMessage(err)
    # The message names the CAUSE, not just the refusal.
    expect_match(msg, "resampling", fixed = TRUE)
    expect_match(msg, "strata and", fixed = TRUE)
    # It says the estimand is not what is in question,
    expect_match(msg, "not in question", fixed = TRUE)
    # and it hands over a route.
    expect_match(msg, "survey::svykm()", fixed = TRUE)
    expect_match(msg, "survey::regTermTest", fixed = TRUE)
    # The size of the problem is a MEASURED RANGE conditional on the
    # design and the contrast, never one figure: a between-cluster
    # contrast is understated 1.5x-6x, a within-cluster contrast is
    # not understated at all. A single number would be wrong on four
    # designs out of five.
    expect_match(msg, "1.5x-6x", fixed = TRUE)
    expect_match(msg, "30 x 25", fixed = TRUE)
    expect_match(msg, "within clusters", fixed = TRUE)
    expect_match(msg, "about right", fixed = TRUE)
    # A refusal carries no roadmap promise -- the survey vignette does.
    expect_false(grepl("is planned", msg, fixed = TRUE))
  }
  # A plain Cox fit still gets its estimand columns.
  plain <- survival::coxph(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung
  )
  expect_s3_class(
    suppressWarnings(table_regression(
      plain,
      show_columns = c("b", "rmst"),
      tau = 500,
      boot_n = 20L,
      output = "data.frame"
    )),
    "data.frame"
  )
})

test_that("AME is refused, and the hint points at survey rather than at estimands", {
  fit <- .scox_fit()
  err <- expect_error(
    table_regression(fit, show_columns = c("b", "ame")),
    class = "spicy_invalid_input"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "survey::regTermTest", fixed = TRUE)
  expect_match(msg, "survey::svykm()", fixed = TRUE)
  # NOT the plain-Cox hint, which offers columns this class also refuses.
  expect_false(grepl("with `tau = `", msg, fixed = TRUE))
  expect_false(as_regression_frame(fit)$info$supports$ame)
  # A plain Cox fit keeps the estimand hint.
  plain <- survival::coxph(
    survival::Surv(time, status) ~ age,
    data = survival::lung
  )
  err2 <- expect_error(
    table_regression(plain, show_columns = c("b", "ame")),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err2), "with `tau = `", fixed = TRUE)
})

test_that("marginaleffects still cannot compute an AME for this class", {
  # The refusal above stands on the model, not on the bug: a
  # proportional-hazards fit has no response scale to average on. But if
  # marginaleffects ever stops recursing without end here, this is the
  # assertion that says so -- otherwise the refusal quietly becomes a
  # lost capability. `nodeStackOverflowError` inherits from `error`, so
  # calling it is safe and the session survives.
  skip_if_not_installed("marginaleffects")
  fit <- .scox_fit()
  # tryCatch rather than expect_error: the overflow recurs while
  # testthat's own condition machinery unwinds, and escapes the
  # expectation. Caught in one frame, it is an ordinary classed error.
  res <- tryCatch(
    suppressWarnings(marginaleffects::avg_slopes(fit)),
    error = function(e) e
  )
  # The infinite recursion is a STACK-LIMIT failure, so whether it
  # overflows is platform-dependent: Windows and the Ubuntu releases
  # overflow, macOS and R-devel terminate (CI, 2026-08-21). Either
  # way spicy's refusal stands -- it is decision-based (no natural
  # response scale for a Cox model), not crash-based. The sentinel
  # only documents the upstream state where the crash occurs.
  if (inherits(res, "error")) {
    expect_s3_class(res, "nodeStackOverflowError")
  } else {
    skip(paste(
      "avg_slopes() terminates on this platform --",
      "revisit register n. 95 if this becomes the norm"
    ))
  }
  # The session is intact afterwards.
  expect_equal(
    unname(stats::coef(fit))[1L],
    0.88976550380432229,
    tolerance = 1e-12
  )
})

test_that("a robust vcov request gets the design message", {
  fit <- .scox_fit()
  err <- expect_error(
    table_regression(fit, vcov = "HC3"),
    class = "spicy_unsupported_vcov"
  )
  expect_match(conditionMessage(err), "svycoxph", fixed = TRUE)
  expect_match(conditionMessage(err), "svydesign", fixed = TRUE)
})


# ---- 8. Registry ---------------------------------------------------------

test_that("svycoxph appears in the supported-models registry", {
  tb <- table_regression_models()
  expect_true("svycoxph" %in% tb[[2L]])
  row <- tb[tb[[2L]] == "svycoxph", ]
  expect_identical(row[[1L]], "Survey-weighted")
  # No AME column, hazard ratios on exponentiate.
  expect_identical(row[[4L]], "no")
  expect_match(row[[5L]], "HR", fixed = TRUE)
})


test_that("a concordance vector missing an entry degrades, it does not throw", {
  # `[[` on a named atomic vector whose name is absent THROWS rather than
  # returning NULL, so `%||%` cannot intercept it and the exception would
  # have travelled out of as_regression_frame(). survival always sets
  # both names, which is what makes this latent rather than live.
  cc <- c(concordance = 0.81, std = 0.018)
  expect_error(cc[["nope"]], "out of bounds")
  fit <- .scox_fit()
  # (a) the standard error alone is missing: the C statistic still gets
  #     out, and its standard error is absent rather than invented.
  no_se <- fit
  no_se$concordance <- cc[c("concordance")]
  got <- spicy:::.svycoxph_concordance(no_se)
  expect_equal(got$c, 0.81, tolerance = 1e-12)
  expect_true(is.na(got$se))
  # (b) the C statistic itself is missing: no block at all.
  no_c <- fit
  no_c$concordance <- cc[c("std")]
  expect_null(spicy:::.svycoxph_concordance(no_c))
  # And both render: the footer carries C without a standard error in
  # the first case, and drops the sentence in the second.
  out <- paste(
    utils::capture.output(print(table_regression(no_se, show_columns = "b"))),
    collapse = "\n"
  )
  expect_match(out, "Concordance C = 0.81.", fixed = TRUE)
  expect_false(grepl("SE = NA", out, fixed = TRUE))
  out2 <- paste(
    utils::capture.output(print(table_regression(no_c, show_columns = "b"))),
    collapse = "\n"
  )
  expect_false(grepl("Concordance", out2, fixed = TRUE))
})
