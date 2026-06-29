# Coverage-targeted tests for R/regression_ame.R.
#
# These exercise branches not hit by the existing AME test suite:
#   * empty / non-applicable predictor sets in the Satterthwaite path
#   * the bootstrap/jackknife `df = Inf` regime in the marginaleffects path
#   * the `avg_slopes()` failure fallbacks (lm / glm / mixed-frame)
#   * the inline-`factor()` grep fallbacks (glm + mixed-frame)
#   * the `compute_satt_df_per_coef()` NULL-on-failure guard
#   * the `.compute_ame_rows_for_frame()` / `.attach_ame_to_frame_coefs()`
#     no-op and schema-padding guards
#
# Each block makes a real assertion; engine-specific blocks are gated by
# skip_if_not_installed().

skip_if_no_me <- function() testthat::skip_if_not_installed("marginaleffects")
skip_if_no_cs <- function() testthat::skip_if_not_installed("clubSandwich")


# ---- Satterthwaite path: empty-contrast guards --------------------------

test_that("extract_ame_satterthwaite returns empty when the only predictor is logical", {
  # A logical predictor is a model term (in term.labels, in the model
  # frame, no function-call wrapper) but is neither numeric nor factor,
  # so build_ame_contrasts_for_predictor() yields no contrasts and the
  # `length(contrast_set) == 0L` guard returns an empty long frame.
  skip_if_no_cs()
  set.seed(1)
  n <- 120L
  df <- data.frame(
    y = rnorm(n),
    lg = sample(c(TRUE, FALSE), n, replace = TRUE),
    cluster = factor(sample(1:8, n, replace = TRUE))
  )
  fit <- lm(y ~ lg, data = df)
  rows <- spicy:::extract_ame_satterthwaite(
    fit, vcov_type = "CR2", cluster = df$cluster, ci_level = 0.95,
    model_id = "M1", outcome = "y"
  )
  expect_s3_class(rows, "data.frame")
  expect_equal(nrow(rows), 0L)
})


# ---- build_ame_contrasts_for_predictor: non-standard predictors ---------

test_that("build_ame_contrasts_for_predictor returns list() for an absent predictor", {
  fit <- lm(mpg ~ wt, data = mtcars)
  # "notthere" is not a column of model.frame(fit) -> early `list()`.
  out <- spicy:::build_ame_contrasts_for_predictor(fit, "notthere")
  expect_type(out, "list")
  expect_length(out, 0L)
})

test_that("build_ame_contrasts_for_predictor returns list() for a logical predictor", {
  set.seed(2)
  df <- data.frame(y = rnorm(60), lg = sample(c(TRUE, FALSE), 60, replace = TRUE))
  fit <- lm(y ~ lg, data = df)
  # Logical predictor: present in the model frame, neither numeric nor
  # factor -> falls through to the trailing `list()`.
  out <- spicy:::build_ame_contrasts_for_predictor(fit, "lg")
  expect_type(out, "list")
  expect_length(out, 0L)
})


# ---- Path B (marginaleffects): bootstrap/jackknife -> df = Inf ----------

test_that("extract_ame_marginaleffects uses z-inference under bootstrap vcov", {
  # vcov_type %in% c("bootstrap", "jackknife") selects df = Inf, which
  # marginaleffects renders as a z-statistic (test_type "z").
  skip_if_no_me()
  set.seed(3)
  df <- data.frame(y = rnorm(100), x = rnorm(100),
                   g = factor(sample(letters[1:4], 100, replace = TRUE)))
  fit <- lm(y ~ x + g, data = df)
  rows <- spicy:::extract_ame_marginaleffects(
    fit, vc = vcov(fit), vcov_type = "bootstrap", ci_level = 0.95,
    model_id = "M1", outcome = "y"
  )
  expect_gt(nrow(rows), 0L)
  expect_true(all(rows$test_type == "z"))

  rows_jk <- spicy:::extract_ame_marginaleffects(
    fit, vc = vcov(fit), vcov_type = "jackknife", ci_level = 0.95,
    model_id = "M1", outcome = "y"
  )
  expect_true(all(rows_jk$test_type == "z"))
})


# ---- Path B (marginaleffects): avg_slopes() failure fallback -----------

test_that("extract_ame_marginaleffects warns and returns empty when avg_slopes fails", {
  # A vcov matrix with the wrong dimensions / names makes avg_slopes()
  # error; the tryCatch arm warns spicy_fallback and returns
  # empty_coefs_long().
  skip_if_no_me()
  fit <- lm(mpg ~ wt, data = mtcars)
  bad_vc <- matrix(NA_real_, 3L, 3L,
                   dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  expect_warning(
    rows <- spicy:::extract_ame_marginaleffects(
      fit, vc = bad_vc, vcov_type = "classical", ci_level = 0.95,
      model_id = "M1", outcome = "mpg"
    ),
    class = "spicy_fallback"
  )
  expect_equal(nrow(rows), 0L)
})


# ---- glm path: avg_slopes() failure fallback ---------------------------

test_that("extract_ame_glm warns and returns empty when avg_slopes fails", {
  skip_if_no_me()
  fit <- glm(vs ~ wt, data = mtcars, family = binomial)
  bad_vc <- matrix(NA_real_, 3L, 3L,
                   dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  expect_warning(
    rows <- spicy:::extract_ame_glm(
      fit, vc = bad_vc, vcov_type = "classical", cluster = NULL,
      ci_level = 0.95, model_id = "M1", outcome = "vs"
    ),
    class = "spicy_fallback"
  )
  expect_equal(nrow(rows), 0L)
})


# ---- glm path: inline factor() grep fallback ---------------------------

test_that("extract_ame_glm resolves inline factor(x) to the model-frame column", {
  # marginaleffects reports the bare "cyl"; the model-frame column is
  # "factor(cyl)" and the coef names follow "factor(cyl)6". The grep
  # fallback (var_name not in mf_names) rebuilds the coef-style term id.
  skip_if_no_me()
  fit <- glm(vs ~ wt + factor(cyl), data = mtcars, family = binomial)
  rows <- suppressWarnings(spicy:::extract_ame_glm(
    fit, vc = vcov(fit), vcov_type = "classical", cluster = NULL,
    ci_level = 0.95, model_id = "M1", outcome = "vs"
  ))
  expect_true(any(rows$term == "wt"))
  expect_true(any(grepl("^factor\\(cyl\\)", rows$term)))
})


# ---- compute_satt_df_per_coef: NULL on failure ---------------------

test_that("compute_satt_df_per_coef returns NULL when coef_test errors", {
  # A malformed vcov makes clubSandwich::coef_test() error; the helper
  # catches it and returns NULL so the caller can fall back to z.
  skip_if_no_cs()
  fit <- glm(vs ~ wt, data = mtcars, family = binomial)
  bad_vc <- matrix(NA_real_, 3L, 3L,
                   dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  res <- spicy:::compute_satt_df_per_coef(
    fit, vc = bad_vc, cluster = mtcars$cyl
  )
  expect_null(res)
})


# ---- Mixed-frame: .compute_ame_rows_for_frame guards -------------------

test_that(".compute_ame_rows_for_frame returns NULL when marginaleffects is unavailable", {
  skip_if_not_installed("lme4")
  skip_if_no_me()
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  # Mock the package-availability wrapper so the guard fires even though
  # marginaleffects IS installed.
  res <- testthat::with_mocked_bindings(
    spicy:::.compute_ame_rows_for_frame(fit, ci_level = 0.95),
    spicy_pkg_available = function(pkg) FALSE,
    .package = "spicy"
  )
  expect_null(res)
})

test_that(".compute_ame_rows_for_frame warns + returns NULL when avg_slopes fails", {
  skip_if_not_installed("lme4")
  skip_if_no_me()
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  warned <- FALSE
  res <- withCallingHandlers(
    testthat::with_mocked_bindings(
      spicy:::.compute_ame_rows_for_frame(fit, ci_level = 0.95),
      avg_slopes = function(...) stop("synthetic avg_slopes failure"),
      .package = "marginaleffects"
    ),
    spicy_fallback = function(c) {
      warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  expect_true(warned)
  expect_null(res)
})

test_that(".compute_ame_rows_for_frame returns NULL on a zero-row avg_slopes table", {
  skip_if_not_installed("lme4")
  skip_if_no_me()
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  res <- testthat::with_mocked_bindings(
    spicy:::.compute_ame_rows_for_frame(fit, ci_level = 0.95),
    avg_slopes = function(...) {
      data.frame(term = character(0), estimate = numeric(0))
    },
    .package = "marginaleffects"
  )
  expect_null(res)
})


# ---- Mixed-frame: inline factor() grep fallback ------------------------

test_that(".compute_ame_rows_for_frame resolves inline factor(x) for a glmer fit", {
  skip_if_not_installed("lme4")
  skip_if_no_me()
  mt <- mtcars
  mt$grp <- factor(rep(1:8, length.out = nrow(mt)))
  fit <- suppressWarnings(lme4::glmer(
    vs ~ wt + factor(cyl) + (1 | grp), data = mt, family = binomial
  ))
  rows <- suppressWarnings(spicy:::.compute_ame_rows_for_frame(fit, ci_level = 0.95))
  expect_true(any(rows$term == "wt"))
  expect_true(any(grepl("^factor\\(cyl\\)", rows$term)))
})


# ---- .attach_ame_to_frame_coefs: no-op + padding guards ----------------

test_that(".attach_ame_to_frame_coefs is a no-op when compute returns NULL", {
  skip_if_not_installed("lme4")
  skip_if_no_me()
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  coefs <- spicy:::as_regression_frame(
    fit, model_id = "M1", show_columns = c("b", "se")
  )$coefs
  res <- testthat::with_mocked_bindings(
    spicy:::.attach_ame_to_frame_coefs(
      coefs, fit, ci_level = 0.95, show_columns = c("b", "ame")
    ),
    .compute_ame_rows_for_frame = function(...) NULL,
    .package = "spicy"
  )
  expect_identical(res, coefs)
})

test_that(".attach_ame_to_frame_coefs is a no-op when ame_rows shares no columns", {
  skip_if_not_installed("lme4")
  skip_if_no_me()
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  coefs <- spicy:::as_regression_frame(
    fit, model_id = "M1", show_columns = c("b", "se")
  )$coefs
  res <- testthat::with_mocked_bindings(
    spicy:::.attach_ame_to_frame_coefs(
      coefs, fit, ci_level = 0.95, show_columns = c("b", "ame")
    ),
    # Disjoint column names -> intersect() empty -> early return.
    .compute_ame_rows_for_frame = function(...) {
      data.frame(zzz = 1, www = 2)
    },
    .package = "spicy"
  )
  expect_identical(res, coefs)
})

test_that(".attach_ame_to_frame_coefs pads coefs-only columns with NA on AME rows", {
  # When coefs carries an extra column absent from ame_rows, the padding
  # loop fills it with a class-correct NA on the appended AME rows.
  skip_if_not_installed("lme4")
  skip_if_no_me()
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  coefs <- spicy:::as_regression_frame(
    fit, model_id = "M1", show_columns = c("b", "se")
  )$coefs
  coefs$legacy_col <- "carry-over"
  res <- spicy:::.attach_ame_to_frame_coefs(
    coefs, fit, ci_level = 0.95, show_columns = c("b", "ame")
  )
  # AME rows were appended.
  expect_true("AME" %in% res$estimate_type)
  expect_gt(nrow(res), nrow(coefs))
  # The carry-over column is NA on the appended AME rows and retains the
  # output schema (same columns, same order as the input coefs).
  expect_true(all(is.na(res$legacy_col[res$estimate_type == "AME"])))
  expect_identical(colnames(res), colnames(coefs))
})
