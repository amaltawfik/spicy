# ---------------------------------------------------------------------------
# Guards for survey-design fits: the design is the variance authority.
#
# compute_model_vcov() returned a silently wrong matrix for every
# model-derived estimator on svyglm / svrepglm -- HC3 gave SE 51864 against
# 26.90 design-correct, jackknife 10.89: the first ~1900x too big, the
# second ~0.4x, i.e. anti-conservative. No test went through that route,
# which is why the hole survived CI.
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
  # 14 witnesses: 7 estimators x 2 design classes. None of them went
  # through a test before -- which is exactly why the hole survived CI.
  # svrepglm inherits "svyglm", so it reaches the guard by inheritance;
  # it is exercised explicitly because the two classes carry different
  # variance machinery (Taylor linearisation vs replicate weights) and
  # both returned wrong -- and DIFFERENT -- matrices.
  fits <- .svy_guard_fits()
  estimators <- c("HC0", "HC1", "HC2", "HC3", "HC4", "bootstrap", "jackknife")
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
