# Correctness/crash sweep from the 2026-07-01 findings re-triage
# (dev/findings_retriage.md, group A). Three independent fixes:
#   m1 -- AME of an aliased (perfectly collinear) predictor en-dashes
#         instead of a misleading "0.00".
#   M4 -- nested = TRUE tolerates a class whose nobs() returns double (coxph).
#   m5 -- a wide random-effect bar does not break the native Nakagawa null
#         refit (deparse1(), not deparse()).

## ---- m1: aliased predictor -> AME en-dash ---------------------------------

test_that("m1: an aliased predictor's AME is NA (en-dash), not 0.00", {
  d <- mtcars
  d$dup <- 2 * d$wt                       # perfectly collinear with wt
  m <- lm(mpg ~ wt + dup, data = d)       # `dup` coefficient is aliased (NA)
  expect_true(is.na(stats::coef(m)["dup"]))   # sanity: R aliases dup

  fr <- as_regression_frame(
    m, show_columns = expand_show_columns(c("b", "ame"))
  )
  ame_dup <- fr$coefs[fr$coefs$estimate_type == "ame" &
                        fr$coefs$term == "dup", , drop = FALSE]
  expect_equal(nrow(ame_dup), 1L)
  # the en-dash invariant: no misleading finite estimate for an undefined AME
  expect_true(is.na(ame_dup$estimate))
  expect_true(is.na(ame_dup$std_error))
  expect_true(is.na(ame_dup$ci_lower) && is.na(ame_dup$ci_upper))

  # a well-defined predictor keeps its AME
  ame_wt <- fr$coefs[fr$coefs$estimate_type == "ame" &
                       fr$coefs$term == "wt", , drop = FALSE]
  expect_true(is.finite(ame_wt$estimate))

  # rendered table: the AME cell for `dup` is an en-dash, not "0.00"
  df <- table_regression(m, show_columns = c("b", "ame"),
                         output = "data.frame")
  ame_col <- grep("AME", names(df), value = TRUE)[1]
  cell <- trimws(df[trimws(df$Variable) == "dup", ame_col])
  expect_true(cell %in% c("–", "-", ""))
  expect_false(grepl("0", cell))
})

## ---- M4: coxph nobs() is double under nested = TRUE -----------------------

test_that("M4: nested = TRUE supports coxph (double nobs) via the LRT path", {
  skip_if_not_installed("survival")
  set.seed(3)
  d <- data.frame(
    time   = stats::rexp(150),
    status = stats::rbinom(150, 1, 0.7),
    x1     = stats::rnorm(150),
    x2     = stats::rnorm(150)
  )
  c1 <- survival::coxph(survival::Surv(time, status) ~ x1, data = d)
  c2 <- survival::coxph(survival::Surv(time, status) ~ x1 + x2, data = d)
  expect_type(stats::nobs(c1), "double")   # sanity: the trigger

  # The nested LRT/p match the survival oracle (anova.coxph); R^2-family
  # tokens are NA (no classical R^2 for a Cox fit).
  nested <- spicy:::compute_nested_comparisons(list(c1, c2))
  oracle <- stats::anova(c1, c2, test = "LRT")   # dispatches to anova.coxph
  expect_equal(nested$lrt_change[1], oracle[["Chisq"]][2], tolerance = 1e-8)
  expect_equal(nested$p_change[1],   oracle[["Pr(>|Chi|)"]][2], tolerance = 1e-8)
  expect_true(is.na(nested$r2_change[1]))

  # and the full nested table renders without the old vapply / r.squared crash
  out <- table_regression(list(c1, c2), nested = TRUE, output = "data.frame")
  expect_s3_class(out, "data.frame")
})

## ---- m5: wide random-effect bar -> deparse1 in Nakagawa null refit --------

test_that("m5: a wide random-slope bar does not break the Nakagawa null refit", {
  skip_if_not_installed("lme4")
  # Long names force deparse() of the random-effect bar past width.cutoff (60),
  # so the OLD deparse() returned a length-2 vector and threw inside
  # vapply(..., character(1)). deparse1() collapses it to one string.
  set.seed(1)
  slope_nm <- paste(rep("s", 70), collapse = "")   # 70-char names force the
  group_nm <- paste(rep("g", 70), collapse = "")   # bar to wrap under deparse()
  d <- stats::setNames(
    data.frame(stats::rpois(240, 3), stats::rnorm(240),
               factor(sample(12, 240, TRUE))),
    c("y", slope_nm, group_nm)
  )
  f <- stats::reformulate(
    sprintf("%s + (1 + %s | %s)", slope_nm, slope_nm, group_nm),
    response = "y"
  )
  m <- suppressWarnings(
    lme4::glmer(f, data = d, family = stats::poisson())
  )
  # sanity: the bar really does wrap under deparse()
  bar <- suppressWarnings(lme4::findbars(stats::formula(m))[[1L]])
  expect_gt(length(deparse(bar)), 1L)

  # The native Poisson Nakagawa null params must compute (finite intercept),
  # not throw out of the (un-tryCatch'd) vapply.
  pr <- spicy:::.nakagawa_null_params(m, list(family = "poisson", link = "log"))
  expect_true(is.finite(pr$intercept))
})
