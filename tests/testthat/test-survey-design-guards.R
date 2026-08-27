# ---------------------------------------------------------------------------
# Guards for survey-design fits: the design is the variance authority, and
# a design-weighted Cox fit is refused by class instead of dying inside the
# coxph extractor.
#
# Both guards close routes that produced a WRONG or an unusable answer:
#   * compute_model_vcov() returned a silently wrong matrix for every
#     model-derived estimator on svyglm / svrepglm (HC3: SE 51864 against
#     26.90 design-correct; jackknife: 10.89 -- the first ~1900x too big,
#     the second ~0.4x, i.e. anti-conservative);
#   * svycoxph dispatched to as_regression_frame.coxph() by inheritance and
#     failed with an unclassed simpleError ("No AIC for survey models")
#     after printing six lines of design description.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.svy_guard_b_rows <- function(fr) {
  fr$coefs[fr$coefs$estimate_type == "B" & !(fr$coefs$is_ref %in% TRUE), ]
}

.svy_guard_designs <- function() {
  skip_if_not_installed("survey")
  data("api", package = "survey", envir = environment())
  apiclus1 <- get("apiclus1", envir = environment())
  dclus1 <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1,
    fpc = ~fpc
  )
  list(taylor = dclus1, replicate = survey::as.svrepdesign(dclus1))
}

.svy_guard_fits <- function() {
  d <- .svy_guard_designs()
  list(
    svyglm = survey::svyglm(api00 ~ ell, design = d$taylor),
    svrepglm = survey::svyglm(api00 ~ ell, design = d$replicate)
  )
}

.svycoxph_guard_fit <- function() {
  skip_if_not_installed("survey")
  skip_if_not_installed("survival")
  data("pbc", package = "survival", envir = environment())
  pbc <- get("pbc", envir = environment())
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
  survey::svycoxph(
    survival::Surv(time, status > 0) ~ log(bili) + protime + albumin,
    design = des
  )
}


# ---- Guard A: every model-derived vcov is refused on a design fit ----------

test_that("compute_model_vcov() refuses HC* and the resamplers on design fits", {
  # 18 witnesses: 9 estimators x 2 design classes. None of them went
  # through a test before -- which is exactly why the hole survived CI.
  # svrepglm inherits "svyglm", so it reaches the guard by inheritance;
  # it is exercised explicitly because the two classes carry different
  # variance machinery (Taylor linearisation vs replicate weights) and
  # both returned wrong -- and DIFFERENT -- matrices.
  fits <- .svy_guard_fits()
  estimators <- c(
    "HC0",
    "HC1",
    "HC2",
    "HC3",
    "HC4",
    "HC5",
    "CR1S",
    "bootstrap",
    "jackknife"
  )
  for (cls in names(fits)) {
    for (est in estimators) {
      err <- expect_error(
        spicy:::compute_model_vcov(fits[[cls]], type = est),
        class = "spicy_unsupported_vcov",
        info = paste(cls, est)
      )
      msg <- conditionMessage(err)
      # The message must name the estimator refused, the actual class of
      # the fit (not the parent "svyglm" for a replicate fit), and the
      # design as the authority.
      expect_match(msg, sprintf('vcov = "%s"', est), fixed = TRUE)
      expect_match(msg, sprintf("(`%s`)", cls), fixed = TRUE)
      expect_match(msg, "variance authority", fixed = TRUE)
    }
  }
})

test_that("CR* on a design fit is refused by the design guard, cluster or not", {
  # Before the guard, CR* was only stopped by the missing-`cluster`
  # check, so a caller who DID supply a cluster reached a
  # class-specific message that never mentioned the design; a replicate
  # fit reached no class-specific message at all in compute_model_vcov().
  skip_if_not_installed("survey")
  data("api", package = "survey", envir = environment())
  apiclus1 <- get("apiclus1", envir = environment())
  fits <- .svy_guard_fits()
  for (cls in names(fits)) {
    for (cl in list(NULL, apiclus1$dnum)) {
      err <- expect_error(
        spicy:::compute_model_vcov(fits[[cls]], type = "CR2", cluster = cl),
        class = "spicy_unsupported_vcov",
        info = paste(cls, if (is.null(cl)) "no cluster" else "cluster")
      )
      expect_match(conditionMessage(err), "variance authority", fixed = TRUE)
    }
  }
})

test_that("svyolr and svycoxph are design fits too, on the same terms", {
  # These two never reach a spicy table (both are refused by class at the
  # frame), but they DO reach compute_model_vcov() from a direct or
  # internal caller, and each had its own way of going wrong:
  #   * svyolr HC3 / svycoxph HC3 -> sandwich::vcovHC() errors, the
  #     fallback catches it and returns stats::vcov(fit) -- i.e. the
  #     DESIGN variance, handed back under an "HC3" label;
  #   * svycoxph CR2 + cluster -> the Lin-Wei grouped-dfbeta sandwich it
  #     inherits from coxph, computed on the design-weighted residuals
  #     (SE 0.0901 / 0.0783 against 0.0903 / 0.0780 design-correct): a
  #     model-derived matrix, close enough to look right.
  # Naming them in the predicate makes all three answer the same way.
  skip_if_not_installed("survey")
  skip_if_not_installed("MASS")
  d <- .svy_guard_designs()
  so <- survey::svyolr(as.factor(stype) ~ ell + meals, design = d$taylor)
  for (est in c("HC3", "bootstrap")) {
    err <- expect_error(
      spicy:::compute_model_vcov(so, type = est),
      class = "spicy_unsupported_vcov",
      info = est
    )
    expect_match(conditionMessage(err), "(`svyolr`)", fixed = TRUE)
    expect_match(conditionMessage(err), "variance authority", fixed = TRUE)
  }
  sc <- .svycoxph_guard_fit()
  n <- nrow(stats::model.frame(sc))
  for (args in list(
    list(type = "HC3"),
    list(type = "CR2", cluster = seq_len(n))
  )) {
    err <- expect_error(
      do.call(spicy:::compute_model_vcov, c(list(sc), args)),
      class = "spicy_unsupported_vcov",
      info = args$type
    )
    expect_match(conditionMessage(err), "(`svycoxph`)", fixed = TRUE)
    expect_match(conditionMessage(err), "variance authority", fixed = TRUE)
  }
})

test_that("the design-based default path is untouched by the guard", {
  # "classical" resolves to stats::vcov(fit) -- the Taylor / replicate
  # design variance -- for both classes, and the frame default
  # ("survey-Taylor") still produces the design SE. This is the remedy
  # the refusal points at, so it must keep working.
  fits <- .svy_guard_fits()
  for (cls in names(fits)) {
    fit <- fits[[cls]]
    vc <- spicy:::compute_model_vcov(fit, type = "classical")
    expect_identical(vc, stats::vcov(fit))
    fr <- as_regression_frame(fit)
    b <- .svy_guard_b_rows(fr)
    oracle <- sqrt(diag(as.matrix(stats::vcov(fit))))[b$term]
    expect_equal(unname(b$std_error), unname(oracle), tolerance = 1e-12)
  }
  # The design-correct SE for the Taylor fit, pinned from survey itself.
  expect_equal(
    unname(sqrt(diag(stats::vcov(fits$svyglm)))),
    c(26.90022702256982612, 0.46695811918003072),
    tolerance = 1e-12
  )
  # "model" / "survey-Taylor" are frame-level aliases of that default:
  # they are short-circuited upstream and keep the answer they always
  # had here (they are not vcov-vocabulary tokens), rather than being
  # reported as refused by the design.
  for (alias in c("model", "survey-Taylor")) {
    expect_error(
      spicy:::compute_model_vcov(fits$svyglm, type = alias),
      class = "spicy_invalid_input"
    )
  }
})

test_that("a robust vcov on a design fit is refused before any consumer runs", {
  # The AME and standardized columns are the two consumers that would
  # otherwise compute on the refused matrix. The coefficient step
  # (.apply_robust_vcov_to_coefs) aborts first, so both refuse
  # transitively -- no design table is ever built on a model-derived
  # variance. Checked on the direct frame route, which bypasses the
  # public validate gate.
  fits <- .svy_guard_fits()
  for (cls in names(fits)) {
    expect_error(
      as_regression_frame(
        fits[[cls]],
        vcov = "HC3",
        show_columns = c("b", "ame", "ame_se", "ame_ci")
      ),
      class = "spicy_unsupported_vcov",
      info = cls
    )
  }
  # The public entry point refuses earlier still, in the validate gate,
  # with its own design-native message.
  err <- expect_error(
    table_regression(
      fits$svyglm,
      vcov = "HC3",
      show_columns = c("b", "ame"),
      output = "data.frame"
    ),
    class = "spicy_unsupported_vcov"
  )
  expect_match(conditionMessage(err), "svydesign", fixed = TRUE)
  # Standardized coefficients are refused for a design fit by class, so
  # that consumer cannot reach a variance matrix at all.
  expect_error(
    table_regression(fits$svyglm, standardized = "posthoc"),
    class = "spicy_unsupported_standardized"
  )
  # And the design-based AME still computes on the default path.
  out <- table_regression(
    fits$svyglm,
    show_columns = c("b", "ame"),
    output = "data.frame"
  )
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0L)
})

test_that("classes that are not design fits keep their own refusals", {
  # The guard keys on the survey classes only: a plain glm still reaches
  # the HC* branch and computes, and a non-lm/glm class still gets the
  # resampler message, not the design one.
  skip_if_not_installed("sandwich")
  skip_if_not_installed("survival")
  fit <- stats::glm(am ~ hp, data = mtcars, family = stats::binomial)
  expect_equal(
    spicy:::compute_model_vcov(fit, type = "HC3"),
    sandwich::vcovHC(fit, type = "HC3"),
    tolerance = 1e-12
  )
  cox <- survival::coxph(
    survival::Surv(time, status) ~ age,
    data = survival::lung
  )
  err <- expect_error(
    spicy:::compute_model_vcov(cox, type = "bootstrap"),
    class = "spicy_unsupported_vcov"
  )
  expect_match(conditionMessage(err), "resampling", fixed = TRUE)
  expect_false(grepl("variance authority", conditionMessage(err), fixed = TRUE))
})


# ---- Guard B: svycoxph never walks into the coxph extractor ----------------

test_that("svycoxph takes its own route, not the one it inherits", {
  # class(fit) is c("svycoxph", "coxph"): without a method of its own the
  # fit dispatched to as_regression_frame.coxph(), whose .coxph_info()
  # calls stats::AIC() bare -- survey answers "No AIC for survey models"
  # -- after two summary() calls had printed the design.
  fit <- .svycoxph_guard_fit()
  expect_s3_class(fit, "coxph")
  fr <- as_regression_frame(fit)
  expect_identical(fr$info$class, "svycoxph")
  # Nothing likelihood-shaped survives the crossing.
  expect_true(is.na(fr$info$fit_stats$aic))
  expect_error(stats::AIC(fit), "No AIC for survey models")
  # And the title is not the plain-Cox one.
  expect_match(
    fr$info$extras$title_prefix,
    "Survey-weighted",
    fixed = TRUE
  )
})

test_that("building a svycoxph table prints nothing on the way", {
  # survey's summary.svycoxph prints the design on every call; the coxph
  # extractor called it twice, so the old failure emitted six lines of
  # design description before erroring. Nothing on this route calls it.
  fit <- .svycoxph_guard_fit()
  expect_length(
    capture.output(invisible(as_regression_frame(fit))),
    0L
  )
  expect_length(
    capture.output(invisible(table_regression(fit, output = "data.frame"))),
    0L
  )
})

test_that("plain coxph is untouched by the svycoxph method", {
  # The refusal must not shadow the class it inherits from: a plain
  # coxph frame still builds and still reports survival's own numbers
  # exactly.
  skip_if_not_installed("survival")
  fit <- survival::coxph(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung
  )
  fr <- as_regression_frame(fit)
  b <- .svy_guard_b_rows(fr)
  sm <- summary(fit)$coefficients
  expect_identical(b$term, rownames(sm))
  expect_equal(b$estimate, unname(sm[, "coef"]), tolerance = 1e-14)
  expect_equal(b$std_error, unname(sm[, "se(coef)"]), tolerance = 1e-14)
  expect_equal(b$p_value, unname(sm[, "Pr(>|z|)"]), tolerance = 1e-14)
  expect_identical(fr$info$class, "coxph")
  expect_identical(fr$info$fit_stats$n_events, as.integer(fit$nevent))
  out <- table_regression(fit, output = "data.frame")
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0L)
})


# ---- Guard C: the refusal reaches every design class, and survives ---------

test_that("the vcov gate answers every design class with the design message", {
  # The gate used to key on `inherits(fit, "svyglm")`, so a design-based
  # Cox fell through to the generic "This class supports: classical.",
  # which names no remedy. `.is_design_fit()` gives all of them the
  # message that says where clustering belongs.
  sc <- .svycoxph_guard_fit()
  err <- expect_error(
    table_regression(sc, vcov = "HC3"),
    class = "spicy_unsupported_vcov"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "svycoxph", fixed = TRUE)
  expect_match(msg, "svydesign", fixed = TRUE)
  expect_false(grepl("This class supports", msg, fixed = TRUE))
  # And the svyglm message is the one it has always been.
  fits <- .svy_guard_fits()
  err2 <- expect_error(
    table_regression(fits$svyglm, vcov = "HC1"),
    class = "spicy_unsupported_vcov"
  )
  expect_match(
    conditionMessage(err2),
    "`vcov = \"HC1\"` is not available for `svyglm` models.",
    fixed = TRUE
  )
})

test_that("the AME vcov step re-raises a spicy refusal instead of degrading", {
  # `.attach_ame_to_frame_coefs()` wrapped compute_model_vcov() in a bare
  # `error = function(e) NULL`. A refusal swallowed there leaves `vc =
  # NULL`, avg_slopes() silently falls back to the fit's own variance --
  # the DESIGN one -- and the footer labels it "HC3". Unreachable today
  # only because the coefficient step aborts first; guarded so a future
  # reordering cannot reopen it.
  skip_if_not_installed("marginaleffects")
  fits <- .svy_guard_fits()
  coefs <- spicy:::.svyglm_coefs(fits$svyglm, ci_level = 0.95)
  expect_error(
    spicy:::.attach_ame_to_frame_coefs(
      coefs,
      fits$svyglm,
      ci_level = 0.95,
      show_columns = c("b", "ame"),
      vcov_type = "HC3"
    ),
    class = "spicy_unsupported_vcov"
  )
  # A failure that is NOT a refusal still degrades to the model-based
  # AME. Two of them: an unexpected engine error, and the classed
  # "unknown vcov type" a class whose vocabulary is its own reaches this
  # line with -- estimatr passes "robust", which IS the estimator its
  # own variance already carries.
  skip_if_not_installed("estimatr")
  est <- estimatr::lm_robust(mpg ~ wt + hp, data = mtcars)
  est_frame <- suppressWarnings(as_regression_frame(
    est,
    show_columns = c("b", "ame")
  ))
  expect_true(any(est_frame$coefs$estimate_type == "ame"))
  expect_error(
    spicy:::compute_model_vcov(est, type = "robust"),
    class = "spicy_invalid_input"
  )
  testthat::local_mocked_bindings(
    compute_model_vcov = function(...) stop("engine exploded")
  )
  out <- spicy:::.attach_ame_to_frame_coefs(
    coefs,
    fits$svyglm,
    ci_level = 0.95,
    show_columns = c("b", "ame"),
    vcov_type = "HC3"
  )
  expect_true(any(out$estimate_type == "ame"))
})

test_that(".is_design_fit names the replicate Cox sibling explicitly", {
  # Executable documentation, not a fix: `svrepcoxph` inherits
  # `svycoxph`, so the predicate was already TRUE for it. Naming it keeps
  # the list the statement the file says it is.
  skip_if_not_installed("survey")
  skip_if_not_installed("survival")
  expect_true(spicy:::.is_design_fit(structure(
    list(),
    class = c("svrepcoxph", "svycoxph", "coxph")
  )))
  expect_false(spicy:::.is_design_fit(structure(list(), class = "coxph")))
})


test_that("an unknown vcov token keeps its own answer on a design fit", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dclus1 <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    fpc = ~fpc,
    data = apiclus1
  )
  fit <- survey::svyglm(api00 ~ ell, design = dclus1)
  # The design guard fires on KNOWN estimators only. An unprefixed
  # unknown token keeps the historical "Unknown `vcov` type" error,
  # not the design message.
  err <- tryCatch(
    spicy:::compute_model_vcov(fit, "garbage"),
    error = identity
  )
  expect_match(conditionMessage(err), "Unknown `vcov` type", fixed = TRUE)
  expect_false(grepl("variance authority", conditionMessage(err)))
  # An HC-PREFIXED unknown used to satisfy startsWith() and reach the HC
  # arm, where sandwich rejected it in its own words about its own `type`
  # argument. The prefix is not the vocabulary: "HC9" is as unknown as
  # "garbage", and gets the same answer, from spicy (register n. 244(h)).
  err9 <- tryCatch(
    spicy:::compute_model_vcov(fit, "HC9"),
    error = identity
  )
  expect_s3_class(err9, "spicy_invalid_input")
  expect_match(conditionMessage(err9), "Unknown `vcov` type", fixed = TRUE)
  expect_match(conditionMessage(err9), "HC9", fixed = TRUE)
  expect_false(grepl(
    "sandwich::vcovHC()",
    conditionMessage(err9),
    fixed = TRUE
  ))
  # The message still names a remedy, as its predecessor did -- the
  # vocabulary itself, listed, rather than a backend to call.
  expect_match(conditionMessage(err9), "Valid types:", fixed = TRUE)
  expect_match(conditionMessage(err9), "\"HC3\"", fixed = TRUE)
  expect_identical(
    spicy:::compute_model_vcov(fit, "classical"),
    stats::vcov(fit)
  )
})


# HC4m is the one token the design guard gained when it widened from
# .VCOV_MODES to .VCOV_COMPUTE_MODES, and the widening corrected a
# NUMBER, not a label: the token used to fall through to sandwich, which
# built a heteroskedasticity-robust matrix out of the weighted working
# model and handed it back to be reported under the design's own
# "Taylor linearisation" heading. It is refused now, like every other
# estimator that re-derives a variance the design already carries.
test_that("HC4m on a design fit is refused, like every other estimator", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dclus1 <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    fpc = ~fpc,
    data = apiclus1
  )
  fit <- survey::svyglm(api00 ~ ell, design = dclus1)
  # In the vocabulary -- so this is the design guard's refusal, not the
  # unknown-token one.
  expect_true("HC4m" %in% spicy:::.VCOV_COMPUTE_MODES)
  err <- expect_error(
    spicy:::compute_model_vcov(fit, "HC4m"),
    class = "spicy_unsupported_vcov"
  )
  expect_match(conditionMessage(err), "HC4m", fixed = TRUE)
  expect_match(conditionMessage(err), "survey-design fit", fixed = TRUE)
  # The refusal is actionable: it names the design as the authority and
  # says where to go instead.
  expect_match(conditionMessage(err), "variance authority", fixed = TRUE)
  expect_match(conditionMessage(err), "svydesign", fixed = TRUE)
  # The frame route, which bypasses the public gate, refuses too.
  expect_error(
    spicy:::as_regression_frame(fit, vcov = "HC4m"),
    class = "spicy_unsupported_vcov"
  )
  # HC4m stays available where it is legitimate.
  expect_true(is.matrix(spicy:::compute_model_vcov(
    stats::lm(mpg ~ wt, data = mtcars),
    "HC4m"
  )))
})


# ---- The `cluster` hint on a design fit points at the design ------------
#
# `cluster` with a non-CR `vcov` warns that the cluster vector is
# ignored, and used to close with "Set `vcov` to CR0-CR3". On a design
# fit that advice cannot be followed: CR* is refused outright a step
# earlier (clubSandwich has no vcovCR.svyglm, so the call would land on
# vcovCR.glm and ignore strata, FPC and calibration). A user who did as
# they were told got an error, not a table. Same carve-out shape as the
# estimatr / fixest and rq arms: the class that carries its own
# clustering gets its own route named.

test_that("a design fit's cluster hint names the design, not CR*", {
  skip_if_not_installed("survey")
  data("api", package = "survey", envir = environment())
  apiclus1 <- get("apiclus1", envir = environment())
  fits <- .svy_guard_fits()

  for (cls in names(fits)) {
    w <- tryCatch(
      table_regression(
        fits[[cls]],
        cluster = apiclus1$dnum,
        output = "data.frame"
      ),
      spicy_ignored_arg = function(w) w
    )
    expect_s3_class(w, "spicy_ignored_arg")
    msg <- paste(conditionMessage(w), collapse = " ")
    expect_match(
      msg,
      "survey::svydesign(ids = ~cluster_var, ...)",
      fixed = TRUE
    )
    # The advice that errors is gone.
    expect_false(grepl("Set `vcov` to", msg, fixed = TRUE))
    expect_false(grepl("CR0", msg, fixed = TRUE))
  }

  # And following the hint's predecessor really did error -- the reason
  # the arm exists.
  expect_error(
    table_regression(
      fits$svyglm,
      cluster = apiclus1$dnum,
      vcov = "CR0",
      output = "data.frame"
    ),
    class = "spicy_unsupported_vcov"
  )
})

test_that("a class with a real CR* route keeps the generic hint", {
  w <- tryCatch(
    table_regression(
      lm(mpg ~ wt, data = mtcars),
      cluster = mtcars$cyl,
      output = "data.frame"
    ),
    spicy_ignored_arg = function(w) w
  )
  expect_s3_class(w, "spicy_ignored_arg")
  expect_match(
    paste(conditionMessage(w), collapse = " "),
    "Set `vcov` to",
    fixed = TRUE
  )
})
