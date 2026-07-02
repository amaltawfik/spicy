# ---------------------------------------------------------------------------
# Coverage-targeted tests for R/regression_titlefooter.R
#
# These exercise edge branches of the frame-aware title / footer builders
# that the broader behavioural suite did not already reach: empty-frame
# guards, single-character helpers, the legacy random-effects sentence path,
# panel headers without a method tag, vcov label fall-throughs, polynomial
# `^k` suffix handling, the differing-size p.adjust branch, reference
# categories, and the survival distribution switch arms.
#
# Most assertions go through internal helpers via spicy:::, matching the
# convention in test-random_effects_footer.R / test-survival_footer.R /
# test-mixed_inference_footer.R.
# ---------------------------------------------------------------------------


# ---- Title builder: empty + single-frame-without-DV ----------------------

test_that("title builder returns bare 'Regression' for an empty frame list", {
  expect_identical(
    spicy:::build_regression_title_from_frames(list()),
    "Regression"
  )
})

test_that("title builder falls back to the prefix when single frame has no DV", {
  fr <- list(info = list(dv = NA_character_,
                         extras = list(title_prefix = "Linear regression")))
  expect_identical(
    spicy:::build_regression_title_from_frames(list(fr)),
    "Linear regression"
  )
})


# ---- Footer dispatcher: all blocks empty -> NULL -------------------------

test_that("footer dispatcher returns NULL when no block contributes", {
  expect_null(spicy:::build_regression_footer_from_frames(list()))
})


# ---- capitalize_first / lowercase_first single-character guards -----------

test_that("capitalize_first / lowercase_first pass empty input through", {
  expect_identical(spicy:::capitalize_first(""), "")
  expect_identical(spicy:::capitalize_first(character(0)), character(0))
  expect_identical(spicy:::lowercase_first(""), "")
  expect_identical(spicy:::lowercase_first(character(0)), character(0))
})


# ---- Per-block empty-frame guards ----------------------------------------

test_that("simple footer blocks return NULL on an empty frame list", {
  expect_null(spicy:::build_regression_type_footer_block_from_frames(list()))
  expect_null(spicy:::build_vcov_footer_block_from_frames(list()))
  expect_null(spicy:::build_exponentiate_footer_block_from_frames(list()))
  expect_null(spicy:::build_singular_footer_block_from_frames(list()))
})

test_that("AME-Satterthwaite footer returns NULL on empty frames (ame requested)", {
  expect_null(
    spicy:::build_ame_satterthwaite_footer_block_from_frames(list(), c("ame"))
  )
})

test_that("standardized-caveat footer returns NULL on empty frames", {
  expect_null(
    spicy:::build_standardized_caveat_footer_block_from_frames(list(), "std")
  )
})


# ---- vcov label: cluster-robust without a cluster name + unknown kind ----

test_that("vcov label says 'cluster vector supplied' when no cluster name", {
  frame <- list(info = list(class = "estimatr", vcov_kind = "CR2",
                            extras = list(cluster_name = NA_character_)))
  out <- spicy:::format_vcov_label_from_frame(frame)
  expect_match(out, "cluster-robust (CR2)", fixed = TRUE)
  expect_match(out, "cluster vector supplied", fixed = TRUE)
})

test_that("vcov label falls through to the raw kind for unrecognised types", {
  frame <- list(info = list(class = "lm", vcov_kind = "weirdtype",
                            extras = list()))
  expect_identical(spicy:::format_vcov_label_from_frame(frame), "weirdtype")
})


# ---- Abbreviations: partial f^2 and partial omega^2 legends ----------------

test_that("abbreviations footer defines partial f-squared", {
  out <- spicy:::build_abbreviations_footer_block_from_frames(
    c("partial_f2"), list())
  expect_match(out, "f² = Cohen's partial f²", fixed = TRUE)
})

test_that("abbreviations footer defines bias-corrected partial omega-squared", {
  out <- spicy:::build_abbreviations_footer_block_from_frames(
    c("partial_omega2"), list())
  expect_match(out, "bias-corrected partial omega-squared", fixed = TRUE)
})


# ---- Standardized caveat: fallback to detect_non_additive_terms(fit) ------
# When info$extras$non_additive is NOT pre-attached (the case for a plain
# as_regression_frame() of an interaction model), the builder reads
# attr(frame, "fit") and detects the interaction itself.

test_that("standardized caveat (refit) detects interaction from attached fit", {
  fit <- lm(mpg ~ wt * hp, data = mtcars)
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_null(fr$info$extras$non_additive)        # not pre-attached
  expect_false(is.null(attr(fr, "fit")))          # but the fit is reachable
  out <- spicy:::build_standardized_caveat_footer_block_from_frames(
    list(fr), "refit")
  expect_match(out, "after refit on z-scored data", fixed = TRUE)
})

test_that("standardized caveat (std) detects interaction from attached fit", {
  fit <- lm(mpg ~ wt * hp, data = mtcars)
  fr <- as_regression_frame(fit, model_id = "M1")
  out <- spicy:::build_standardized_caveat_footer_block_from_frames(
    list(fr), "std")
  expect_match(out, "SD of the product (or transformed) design column",
               fixed = TRUE)
  # G2: the footer names the convention and cites the caveat source.
  expect_match(out, "lm.beta convention", fixed = TRUE)
  expect_match(out, "Friedrich 1982", fixed = TRUE)
})


# ---- Stars footer: non-numeric / unnamed mapping -> NULL ------------------

test_that("stars footer returns NULL for a non-numeric mapping", {
  expect_null(spicy:::build_stars_footer_block("yes"))
})

test_that("stars footer returns NULL for an unnamed numeric vector", {
  expect_null(spicy:::build_stars_footer_block(c(0.05, 0.01)))
})


# ---- format_p_threshold: out-of-range / non-finite -----------------------

test_that("format_p_threshold passes through out-of-range and NA values", {
  expect_identical(spicy:::format_p_threshold(0),  format(0))
  expect_identical(spicy:::format_p_threshold(2),  format(2))
  expect_identical(spicy:::format_p_threshold(NA_real_), format(NA_real_))
})


# ---- Ordinal thresholds: class matches but no thresholds frame -----------

test_that("ordinal thresholds returns NULL when no thresholds are attached", {
  frame <- list(info = list(class = "polr", extras = list(thresholds = NULL)))
  expect_null(spicy:::.format_ordinal_thresholds_for_frame(frame))
})


# ---- Survival: multi-model 'Model k:' prefix -----------------------------

test_that("survival footer prefixes 'Model k:' for >1 contributing models", {
  skip_if_not_installed("survival")
  f1 <- survival::coxph(survival::Surv(time, status) ~ age,
                        data = survival::lung)
  f2 <- survival::survreg(survival::Surv(time, status) ~ age,
                          data = survival::lung, dist = "weibull")
  fr1 <- as_regression_frame(f1, model_id = "M1")
  fr2 <- as_regression_frame(f2, model_id = "M2")
  out <- spicy:::build_survival_footer_block_from_frames(list(fr1, fr2))
  expect_match(out, "Model 1:", fixed = TRUE)
  expect_match(out, "Model 2:", fixed = TRUE)
  expect_match(out, "Events:", fixed = TRUE)
  expect_match(out, "Distribution: Weibull", fixed = TRUE)
})


# ---- .surv_title_dist: every named switch arm + the default --------------

test_that(".surv_title_dist normalises all known distribution tokens", {
  expect_identical(spicy:::.surv_title_dist("weibullPH"),   "Weibull (PH)")
  expect_identical(spicy:::.surv_title_dist("gompertz"),    "Gompertz")
  expect_identical(spicy:::.surv_title_dist("gamma"),       "Gamma")
  expect_identical(spicy:::.surv_title_dist("exponential"), "Exponential")
  expect_identical(spicy:::.surv_title_dist("exp"),         "Exponential")
  expect_identical(spicy:::.surv_title_dist("llogis"),      "Log-logistic")
  expect_identical(spicy:::.surv_title_dist("loglogistic"), "Log-logistic")
  expect_identical(spicy:::.surv_title_dist("gengamma"),    "Generalised gamma")
  expect_identical(spicy:::.surv_title_dist("genf"),        "Generalised F")
  expect_identical(spicy:::.surv_title_dist("gaussian"),    "Gaussian")
  # Default: capitalise the first letter of an unknown token.
  expect_identical(spicy:::.surv_title_dist("customdist"),  "Customdist")
})


# ---- .re_components_on_scale: empty data.frame short-circuit --------------

test_that(".re_components_on_scale returns the frame unchanged when empty", {
  empty <- data.frame(group = character(0), variance = numeric(0))
  expect_identical(spicy:::.re_components_on_scale(empty, "sd"), empty)
})


# ---- Random-effects summary: method + LR test only (D4 amendment) --------
# The variance components render as table rows; N (groups) + ICC render as
# fit-stat rows. The footer keeps only the estimation method and the
# chi-bar-squared LR test; with NEITHER, the block is suppressed (NULL).

test_that("random-effects summary is NULL without a method and LR test", {
  vc <- data.frame(
    group     = c("Subject", "Residual"),
    term      = c("(Intercept)", ""),
    variance  = c(600, 650),
    sd        = c(24.5, 25.5),
    corr      = c(NA_real_, NA_real_),
    std_error = c(5.8, 1.5),
    ci_lower  = c(13, 23),
    ci_upper  = c(40, 28),
    stringsAsFactors = FALSE
  )
  frame <- list(coefs = data.frame(), info = list(
    class = "lmerMod",
    n_groups = c(Subject = 18L),
    random_effects = list(variance_components = vc, icc = 0.5)  # no method
  ))
  expect_null(spicy:::.format_random_effects_for_frame(frame))
})

test_that("random-effects summary shows the method alone when no LR test", {
  vc <- data.frame(
    group     = c("Subject", "Residual"),
    term      = c("(Intercept)", ""),
    variance  = c(600, 650),
    sd        = c(24.5, 25.5),
    corr      = c(NA_real_, NA_real_),
    stringsAsFactors = FALSE
  )
  frame <- list(coefs = data.frame(), info = list(
    class = "lmerMod",
    n_groups = c(Subject = 18L),
    random_effects = list(variance_components = vc, icc = 0.5,
                          method = "REML")
  ))
  out <- spicy:::.format_random_effects_for_frame(frame)
  expect_identical(out, "Random effects (REML).")
})


# ---- format_p_value_for_panel: NA / finite arms --------------------------

test_that("format_p_value_for_panel handles NA, finite, and tiny p", {
  expect_identical(spicy:::format_p_value_for_panel(NA_real_), "= NA")
  expect_identical(spicy:::format_p_value_for_panel(0.5),      "= 0.500")
  expect_identical(spicy:::format_p_value_for_panel(1e-5),     "< .001")
})


# ---- RE summary: variance components + N + ICC stay OUT of the footer ----
# The variance components render as table rows; N (groups) + ICC render as
# fit-stat rows. The footer never repeats any of them.

test_that("RE footer summary omits component values, N, and ICC", {
  vc <- data.frame(
    group = c("Subject", "Subject", "Subject", "Residual"),
    term  = c("(Intercept)", "Days", "Days", ""),
    variance = c(600, 35, NA_real_, 650),
    sd = c(24.5, 5.9, NA_real_, 25.5),
    corr = c(NA_real_, NA_real_, 0.07, NA_real_),
    is_correlation = c(FALSE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  frame <- list(coefs = data.frame(), info = list(
    class = "lmerMod",
    n_groups = c(Subject = 18L),
    random_effects = list(variance_components = vc, icc = 0.5,
                          method = "REML")
  ))
  out <- spicy:::.format_random_effects_for_frame(frame)
  expect_identical(out, "Random effects (REML).")
  expect_false(grepl("600", out, fixed = TRUE))   # variances live in the body
  expect_false(grepl("0.07", out, fixed = TRUE))  # so does the correlation
  expect_false(grepl("Subjects", out, fixed = TRUE))  # N -> fit-stat row
  expect_false(grepl("ICC", out, fixed = TRUE))       # ICC -> fit-stat row
})

test_that("RE footer summary returns NULL when nothing informative survives", {
  # Only a correlation row, no method, no n_groups, no icc, no LR test.
  vc <- data.frame(
    group = "Subject", term = "Days", variance = NA_real_,
    sd = NA_real_, corr = 0.07, is_correlation = TRUE,
    stringsAsFactors = FALSE
  )
  frame <- list(coefs = data.frame(), info = list(
    class = "lmerMod", n_groups = NULL,
    random_effects = list(variance_components = vc, icc = NULL)
  ))
  expect_null(spicy:::.format_random_effects_for_frame(frame))
})


# ---- p.adjust footer: empty coefs and differing per-model sizes ----------

test_that("p.adjust footer counts 0 for a frame with no coefs", {
  frame <- list(coefs = NULL)
  out <- spicy:::build_p_adjust_footer_block_from_frames(list(frame), "holm")
  expect_match(out, "m = 0 coefficient(s) per model", fixed = TRUE)
})

test_that("p.adjust footer lists per-model m when sizes differ", {
  mk <- function(n) data.frame(
    term = c("(Intercept)", paste0("x", seq_len(n))),
    estimate_type = rep("B", n + 1L),
    is_ref = rep(FALSE, n + 1L),
    p_value = rep(0.04, n + 1L),
    stringsAsFactors = FALSE
  )
  out <- spicy:::build_p_adjust_footer_block_from_frames(
    list(list(coefs = mk(2)), list(coefs = mk(3))), "holm")
  expect_match(out, "m = (2, 3) coefficient(s) per model", fixed = TRUE)
  expect_match(out, "method = \"holm\"", fixed = TRUE)
})


# ---- Polynomial contrasts footer: ^k suffixes + filters ------------------

test_that("polynomial contrasts footer labels higher-degree ^k suffixes", {
  coefs <- data.frame(
    term = c("(Intercept)", "g.L", "g.Q", "g.C", "g^4", "g^5"),
    label = c("(Intercept)", ".L", ".Q", ".C", "^4", "^5"),
    parent_var = c(NA, "g", "g", "g", "g", "g"),
    stringsAsFactors = FALSE
  )
  out <- suppressMessages(
    spicy:::build_polynomial_contrasts_footer_block_from_frames(
      list(list(coefs = coefs))))
  expect_match(out, "^4 = quartic", fixed = TRUE)
  expect_match(out, "^5 = quintic", fixed = TRUE)
  expect_match(out, "Ordered factor `g`", fixed = TRUE)
})

test_that("polynomial contrasts footer is NULL when no displayed var survives", {
  coefs <- data.frame(
    term = c("g.L", "g.Q"),
    label = c(".L", ".Q"),
    parent_var = c("g", "g"),
    stringsAsFactors = FALSE
  )
  out <- suppressMessages(
    spicy:::build_polynomial_contrasts_footer_block_from_frames(
      list(list(coefs = coefs)), displayed_parent_vars = c("other")))
  expect_null(out)
})

test_that("polynomial contrasts footer skips empty-coef frames and lists", {
  expect_null(
    spicy:::build_polynomial_contrasts_footer_block_from_frames(list()))
  expect_null(suppressMessages(
    spicy:::build_polynomial_contrasts_footer_block_from_frames(
      list(list(coefs = data.frame())))))
})


# ---- ordinal_label: named arms + default ---------------------------------

test_that("ordinal_label maps 4/5/6 to words and falls back for >6", {
  expect_identical(spicy:::ordinal_label(4), "quartic")
  expect_identical(spicy:::ordinal_label(5), "quintic")
  expect_identical(spicy:::ordinal_label(6), "sextic")
  expect_identical(spicy:::ordinal_label(7), "degree-7")
})


# ---- Reference categories footer -----------------------------------------

test_that("reference categories footer builds the pair sentence", {
  coefs <- data.frame(
    is_ref = c(TRUE, FALSE, TRUE),
    parent_var = c("cyl", "cyl", "gear"),
    label = c("4", "6", "3"),
    stringsAsFactors = FALSE
  )
  out <- spicy:::build_reference_categories_footer_block_from_frames(
    list(list(coefs = coefs)), "footer")
  expect_match(out, "Reference categories:", fixed = TRUE)
  expect_match(out, "cyl = 4", fixed = TRUE)
  expect_match(out, "gear = 3", fixed = TRUE)
})

test_that("reference categories footer NULL-guards empty / coef-less inputs", {
  expect_null(
    spicy:::build_reference_categories_footer_block_from_frames(list(), "footer"))
  expect_null(
    spicy:::build_reference_categories_footer_block_from_frames(
      list(list(coefs = NULL)), "footer"))
})

test_that("reference categories footer skips frames with no reference rows", {
  coefs <- data.frame(
    is_ref = c(FALSE, FALSE), parent_var = c("a", "b"),
    label = c("1", "2"), stringsAsFactors = FALSE
  )
  expect_null(
    spicy:::build_reference_categories_footer_block_from_frames(
      list(list(coefs = coefs)), "footer"))
})

test_that("reference categories footer skips ref rows with NA term / level", {
  coefs <- data.frame(
    is_ref = TRUE, parent_var = NA_character_, label = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_null(
    spicy:::build_reference_categories_footer_block_from_frames(
      list(list(coefs = coefs)), "footer"))
})
