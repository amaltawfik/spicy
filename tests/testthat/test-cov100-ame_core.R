# Coverage-closure tests (group g08_ame_core).
#
# Targets, by file:
#   * R/regression_ame.R
#       .aliased_coef_terms()        -- NULL/0-row guard (846), legacy
#                                       `is_reference` schema (851), and the
#                                       no-ref-column fallback (853).
#       .attach_ame_to_frame_coefs() -- NA-mirroring of AME cells for an
#                                       aliased (perfectly collinear)
#                                       predictor (888-892).
#   * R/table_regression.R
#       beta appended at the END when standardized != "none" and "b" is
#       absent from show_columns (1254); named-model labels in the
#       profile-CI-overridden warning (1379), the singular-fit warning
#       (1781), and the vc-SE-skipped-for-size warning (1810).
#   * R/regression_nested.R
#       the all-NA return contract when anova() fails for a pair of
#       lm (105) / glm (194) / mixed (280) fits.
#   * R/regression_extract.R
#       .spicy_get_terms() final NULL fallthrough (674) and the
#       extract_fit_stats() logLik-df fallback k (872).

# Collect warnings with their classes while muffling them, so the
# spicy_* condition class AND message fragments can both be asserted.
collect_warnings_cov100 <- function(expr) {
  conds <- list()
  val <- suppressWarnings(withCallingHandlers(
    suppressMessages(expr),
    warning = function(w) conds[[length(conds) + 1L]] <<- w
  ))
  list(value = val, warnings = conds)
}

has_warning_cov100 <- function(warnings, class, fragment) {
  any(vapply(warnings, function(w) {
    inherits(w, class) && grepl(fragment, conditionMessage(w), fixed = TRUE)
  }, logical(1)))
}


# ---- 1. .aliased_coef_terms(): guard + both schema branches ----------------

test_that(".aliased_coef_terms handles NULL and 0-row coefs", {
  expect_identical(spicy:::.aliased_coef_terms(NULL), character(0))
  zero <- data.frame(term = character(0), estimate_type = character(0),
                     estimate = numeric(0), stringsAsFactors = FALSE)
  expect_identical(spicy:::.aliased_coef_terms(zero), character(0))
})

test_that(".aliased_coef_terms reads the legacy `is_reference` column", {
  legacy <- data.frame(
    term          = c("(Intercept)", "x1", "x2", "catB"),
    estimate_type = c("B", "B", "B", "B"),
    estimate      = c(1, 2, NA, NA),
    is_reference  = c(FALSE, FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  # x2 is aliased (NA, non-reference); catB is a reference row and must
  # NOT be flagged even though its estimate is NA.
  expect_identical(spicy:::.aliased_coef_terms(legacy), "x2")
})

test_that(".aliased_coef_terms treats a schema without ref columns as no-ref", {
  bare <- data.frame(
    term          = c("x1", "x2"),
    estimate_type = c("B", "B"),
    estimate      = c(NA, 3),
    stringsAsFactors = FALSE
  )
  expect_identical(spicy:::.aliased_coef_terms(bare), "x1")
})


# ---- 2. .attach_ame_to_frame_coefs(): aliased AME rows are NA-mirrored -----

test_that("aliased predictor AME cells are NA-mirrored onto the frame coefs", {
  skip_if_not_installed("marginaleffects")
  d <- mtcars
  d$dup <- 2 * d$wt                        # perfectly collinear with wt
  m <- lm(mpg ~ wt + dup, data = d)
  expect_true(is.na(stats::coef(m)["dup"]))   # sanity: dup is aliased

  fr <- spicy:::as_regression_frame(
    m, show_columns = spicy:::expand_show_columns("b")
  )
  out <- spicy:::.attach_ame_to_frame_coefs(fr$coefs, m, 0.95, c("b", "ame"))

  # marginaleffects itself reports a FINITE (0) estimate for dup -- that is
  # exactly why the frame must overwrite it with NA.
  sl <- suppressWarnings(marginaleffects::avg_slopes(m, conf_level = 0.95))
  expect_true(is.finite(sl$estimate[sl$term == "dup"]))

  ame_dup <- out[out$estimate_type == "ame" & out$term == "dup", , drop = FALSE]
  expect_identical(nrow(ame_dup), 1L)
  for (col in c("estimate", "std_error", "ci_lower", "ci_upper",
                "statistic", "p_value")) {
    expect_true(is.na(ame_dup[[col]]), info = col)
  }

  # The well-defined predictor keeps its AME, oracle-pinned to avg_slopes().
  ame_wt <- out[out$estimate_type == "ame" & out$term == "wt", , drop = FALSE]
  i <- which(sl$term == "wt")
  expect_equal(ame_wt$estimate,  sl$estimate[i],  tolerance = 1e-8)
  expect_equal(ame_wt$std_error, sl$std.error[i], tolerance = 1e-8)
  expect_equal(ame_wt$ci_lower,  sl$conf.low[i],  tolerance = 1e-8)
  expect_equal(ame_wt$ci_upper,  sl$conf.high[i], tolerance = 1e-8)
  expect_equal(ame_wt$p_value,   sl$p.value[i],   tolerance = 1e-8)
})


# ---- 3. compute_one_pair_*(): all-NA contract when anova() fails -----------

# The all-NA return contract shared by the three per-pair functions.
na_pair_cov100 <- list(
  r2_change = NA_real_, adj_r2_change = NA_real_,
  f_change = NA_real_, f2_change = NA_real_,
  lrt_change = NA_real_,
  aic_change = NA_real_, aicc_change = NA_real_, bic_change = NA_real_,
  deviance_change = NA_real_, p_change = NA_real_
)

test_that("compute_one_pair_lm returns the all-NA contract when anova fails", {
  m_a <- lm(mpg ~ wt, data = mtcars[1:20, ])
  m_b <- lm(mpg ~ wt + hp, data = mtcars)     # different n -> anova stops
  expect_error(suppressWarnings(stats::anova(m_a, m_b)))   # sanity: trigger
  expect_identical(spicy:::compute_one_pair_lm(m_a, m_b), na_pair_cov100)
})

test_that("compute_one_pair_glm returns the all-NA contract when anova fails", {
  g_a <- suppressWarnings(
    glm(am ~ wt, data = mtcars[1:20, ], family = binomial)
  )
  g_b <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  expect_error(
    suppressWarnings(stats::anova(g_a, g_b, test = "LRT"))  # sanity: trigger
  )
  expect_identical(spicy:::compute_one_pair_glm(g_a, g_b), na_pair_cov100)
})

test_that("compute_one_pair_mixed returns the all-NA contract when anova fails", {
  skip_if_not_installed("lme4")
  data(sleepstudy, package = "lme4")
  l_a <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy[1:100, ])
  l_b <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  expect_error(suppressMessages(stats::anova(l_a, l_b)))    # sanity: trigger
  expect_identical(spicy:::compute_one_pair_mixed(l_a, l_b), na_pair_cov100)
})


# ---- 4. beta auto-append when "b" is absent from show_columns --------------

test_that("beta is appended LAST when standardized != none and no b column", {
  m <- lm(mpg ~ wt + hp, data = mtcars)
  df <- table_regression(m, standardized = "refit", show_columns = "p",
                         output = "data.frame")
  # "beta" was appended AFTER the user's tokens (no "b" anchor), so the
  # beta column renders last.
  expect_identical(names(df), c("Variable", "p", "\u03B2"))

  # Oracle: refit standardization == coefficients of the z-scored refit.
  dz <- data.frame(mpg = as.numeric(scale(mtcars$mpg)),
                   wt  = as.numeric(scale(mtcars$wt)),
                   hp  = as.numeric(scale(mtcars$hp)))
  bz <- stats::coef(lm(mpg ~ wt + hp, data = dz))
  cell_wt <- as.numeric(trimws(df[trimws(df$Variable) == "wt", 3]))
  cell_hp <- as.numeric(trimws(df[trimws(df$Variable) == "hp", 3]))
  expect_equal(cell_wt, round(unname(bz["wt"]), 2), tolerance = 1e-8)
  expect_equal(cell_hp, round(unname(bz["hp"]), 2), tolerance = 1e-8)
})


# ---- 5. named-model labels in orchestrator warnings ------------------------

test_that("profile-CI-overridden warning names the named model", {
  skip_if_not_installed("sandwich")
  g <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  res <- collect_warnings_cov100(
    table_regression(list(Solo = g), ci_method = "profile", vcov = "HC3",
                     output = "data.frame")
  )
  expect_true(has_warning_cov100(res$warnings, "spicy_ignored_arg",
                                 "`ci_method = \"profile\"` is ignored"))
  expect_true(has_warning_cov100(res$warnings, "spicy_ignored_arg", "Solo"))
  expect_s3_class(res$value, "data.frame")
})

test_that("singular-fit warning names the named mixed model", {
  skip_if_not_installed("lme4")
  set.seed(1)
  d <- data.frame(y = rnorm(60), g = factor(rep(1:6, each = 10)))
  m_sing <- suppressMessages(lme4::lmer(y ~ 1 + (1 | g), data = d))
  expect_true(lme4::isSingular(m_sing))          # sanity: the trigger
  res <- collect_warnings_cov100(
    table_regression(list(Alpha = m_sing), output = "data.frame")
  )
  expect_true(has_warning_cov100(res$warnings, "spicy_caveat",
                                 "Singular fit (Alpha)"))
  expect_s3_class(res$value, "data.frame")
})

test_that("vc-SE-skipped-for-size warning names the named mixed model", {
  skip_if_not_installed("lme4")
  withr::local_options(list(spicy.re_se_max_n = 10))
  data(sleepstudy, package = "lme4")
  m_re <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
  res <- collect_warnings_cov100(
    table_regression(list(Beta = m_re), output = "data.frame")
  )
  expect_true(has_warning_cov100(
    res$warnings, "spicy_caveat",
    "Variance-component SEs and CIs skipped for Beta"
  ))
  # the message states the active cap
  expect_true(has_warning_cov100(res$warnings, "spicy_caveat",
                                 "`options(\"spicy.re_se_max_n\")` = 10"))
  expect_s3_class(res$value, "data.frame")
})


# ---- 6. regression_extract helpers ------------------------------------------

test_that(".spicy_get_terms returns NULL when terms() fails on an odd class", {
  noterms <- structure(list(), class = "spicy_cov100_no_terms")
  expect_error(stats::terms(noterms))            # sanity: the trigger
  expect_null(spicy:::.spicy_get_terms(noterms))
  # positive control: the generic path still returns a terms object
  expect_s3_class(spicy:::.spicy_get_terms(lm(mpg ~ wt, data = mtcars)),
                  "terms")
})

test_that("extract_fit_stats falls back to k = length(coef) + 1 without df", {
  # A glm subclass whose logLik carries NO usable df attribute: the AICc
  # k must fall back to length(coef) + 1 instead of attr(logLik, "df").
  gfit <- glm(am ~ wt + hp, data = mtcars, family = binomial)
  fake <- gfit
  class(fake) <- c("spicy_cov100_nodf", class(gfit))
  .S3method("logLik", "spicy_cov100_nodf", function(object, ...) {
    val <- NextMethod()
    attr(val, "df") <- NA_integer_
    val
  })
  # AIC/BIC bypass the df-less logLik so the fallback's effect on AICc is
  # isolated (and numerically pinnable).
  .S3method("AIC", "spicy_cov100_nodf", function(object, ..., k = 2) {
    class(object) <- setdiff(class(object), "spicy_cov100_nodf")
    stats::AIC(object, k = k)
  })
  .S3method("BIC", "spicy_cov100_nodf", function(object, ...) {
    class(object) <- setdiff(class(object), "spicy_cov100_nodf")
    stats::BIC(object)
  })
  expect_true(is.na(attr(stats::logLik(fake), "df")))   # sanity: the trigger

  fs <- spicy:::extract_fit_stats(fake, show_fit_stats = c("AIC", "AICc"),
                                  weights = NULL, model_id = "M1",
                                  outcome = "am")
  aic_true <- stats::AIC(gfit)
  n <- stats::nobs(gfit)
  k_fb <- length(stats::coef(gfit)) + 1L    # fallback k = 4
  expect_equal(fs$AIC, aic_true, tolerance = 1e-10)
  expect_equal(fs$AICc,
               aic_true + (2 * k_fb * (k_fb + 1L)) / (n - k_fb - 1L),
               tolerance = 1e-10)

  # and it genuinely DIFFERS from the normal df-based path (binomial glm:
  # df = length(coef) = 3, not 4), proving the fallback branch fired.
  fs_norm <- spicy:::extract_fit_stats(gfit, show_fit_stats = c("AICc"),
                                       weights = NULL, model_id = "M1",
                                       outcome = "am")
  expect_false(isTRUE(all.equal(fs$AICc, fs_norm$AICc)))
})
