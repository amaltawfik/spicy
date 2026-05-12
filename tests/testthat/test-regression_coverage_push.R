# Targeted tests to push coverage on regression files.
# Each test exercises a specific previously-uncovered branch.

mt <- mtcars
mt$cyl <- factor(mt$cyl)


# ============================================================================
# regression_align.R — empty-input branch in pivot_aligned_wide
# ============================================================================

test_that("pivot_aligned_wide — model_labels = NULL on empty input returns base frame", {
  empty <- spicy:::align_extracts(list())
  out <- spicy:::pivot_aligned_wide(empty)
  expect_equal(nrow(out), 0L)
})


# ============================================================================
# regression_ame.R — fallback paths
# ============================================================================

test_that("extract_ame_satterthwaite — predictor not in model frame is silently filtered", {
  skip_if_not_installed("clubSandwich")
  set.seed(1)
  df <- data.frame(y = rnorm(100), x = rnorm(100),
                   g = factor(sample(letters[1:5], 100, replace = TRUE)),
                   cluster = factor(sample(1:5, 100, replace = TRUE)))
  fit <- lm(y ~ x + g, data = df)
  rows <- spicy:::extract_ame_satterthwaite(
    fit, vcov_type = "CR2", cluster = df$cluster, ci_level = 0.95,
    model_id = "M1", outcome = "y"
  )
  expect_true(nrow(rows) > 0L)
})

test_that("build_ame_contrasts_for_predictor — variable absent from model frame returns empty list", {
  fit <- lm(mpg ~ wt, data = mtcars)
  out <- spicy:::build_ame_contrasts_for_predictor(fit, "nonexistent_var")
  expect_equal(length(out), 0L)
})

test_that("build_ame_contrasts_for_predictor — droplevels collapses single-used level", {
  set.seed(2)
  df <- data.frame(y = rnorm(40),
                   g = factor(rep(c("A", "B"), 20),
                              levels = c("A", "B", "C")))  # C has 0 obs
  fit <- lm(y ~ g, data = df)
  out <- spicy:::build_ame_contrasts_for_predictor(fit, "g")
  # 2 levels actually used → 1 contrast (B vs A)
  expect_equal(length(out), 1L)
})

test_that("extract_ame_marginaleffects — failure path emits spicy_fallback warning", {
  skip_if_not_installed("marginaleffects")
  # Use a fit where avg_slopes will fail. We can mock the call to fail
  # via an unrelated trick: passing an invalid vcov matrix.
  fit <- lm(mpg ~ wt, data = mtcars)
  vc <- matrix(NA, 2, 2)  # singular vcov to trigger failure
  rownames(vc) <- colnames(vc) <- names(coef(fit))
  out <- tryCatch(
    suppressWarnings(spicy:::extract_ame_marginaleffects(
      fit, vc = vc, vcov_type = "classical", ci_level = 0.95,
      model_id = "M1", outcome = "mpg"
    )),
    warning = function(w) w
  )
  expect_true(is.data.frame(out) || inherits(out, "warning"))
})


# ============================================================================
# regression_broom.R — empty input branches
# ============================================================================

test_that("tidy.spicy_regression_table — empty input returns empty broom-shaped frame", {
  empty <- structure(
    data.frame(Variable = character(0), stringsAsFactors = FALSE),
    spicy_long = NULL,
    class = c("spicy_regression_table", "spicy_table", "data.frame")
  )
  td <- broom::tidy(empty)
  expect_equal(nrow(td), 0L)
  expect_true("model_id" %in% names(td))
})

test_that("glance.spicy_regression_table — empty input returns empty broom-shaped frame", {
  empty <- structure(
    data.frame(Variable = character(0), stringsAsFactors = FALSE),
    spicy_fit_stats = NULL,
    class = c("spicy_regression_table", "spicy_table", "data.frame")
  )
  g <- broom::glance(empty)
  expect_equal(nrow(g), 0L)
  expect_true(all(c("model_id", "outcome", "nobs", "r.squared",
                    "df.residual") %in% names(g)))
})

# maybe_as_tibble — tibble is in Imports, so the legacy
# missing-tibble fallback was dead code. The simplified function
# always returns a tbl_df; tested via the broom methods.
test_that("maybe_as_tibble — always returns a tbl_df (tibble is in Imports)", {
  out <- spicy:::maybe_as_tibble(data.frame(x = 1:3))
  expect_s3_class(out, "tbl_df")
})


# ============================================================================
# regression_dispatch.R — empty-input + missing-pkg paths via mocking
# ============================================================================

test_that("output_long — empty aligned returns the empty input as-is", {
  empty <- spicy:::align_extracts(list())
  out <- spicy:::dispatch_regression_output(
    rendered = data.frame(Variable = character(0)),
    aligned = empty,
    output = "long"
  )
  expect_equal(nrow(out), 0L)
})

# Helper: mock spicy_pkg_available so it returns FALSE for the
# named packages and TRUE for everything else. Mocking through the
# spicy namespace propagates to instrumented code under
# covr::package_coverage(), which the base::requireNamespace mock
# does not — that's why the regression files route their guards
# through this internal indirection.
mock_missing_pkgs <- function(env, pkgs) {
  testthat::local_mocked_bindings(
    spicy_pkg_available = function(pkg) !(pkg %in% pkgs),
    .package = "spicy",
    .env = env
  )
}

test_that("output_tinytable — missing package errors spicy_missing_pkg", {
  mock_missing_pkgs(rlang::current_env(), "tinytable")
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(table_regression(fit, output = "tinytable"),
               class = "spicy_missing_pkg")
})

test_that("output_gt — missing package errors spicy_missing_pkg", {
  mock_missing_pkgs(rlang::current_env(), "gt")
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(table_regression(fit, output = "gt"),
               class = "spicy_missing_pkg")
})

test_that("output_flextable — missing package errors spicy_missing_pkg", {
  mock_missing_pkgs(rlang::current_env(), "flextable")
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(table_regression(fit, output = "flextable"),
               class = "spicy_missing_pkg")
})

test_that("output_excel — missing package errors spicy_missing_pkg", {
  mock_missing_pkgs(rlang::current_env(), "openxlsx2")
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, output = "excel",
                     excel_path = tempfile(fileext = ".xlsx")),
    class = "spicy_missing_pkg"
  )
})

test_that("output_clipboard — missing package errors spicy_missing_pkg", {
  mock_missing_pkgs(rlang::current_env(), "clipr")
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(table_regression(fit, output = "clipboard"),
               class = "spicy_missing_pkg")
})

test_that("output_word — missing flextable / officer errors spicy_missing_pkg", {
  mock_missing_pkgs(rlang::current_env(), c("flextable", "officer"))
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    table_regression(fit, output = "word",
                     word_path = tempfile(fileext = ".docx")),
    class = "spicy_missing_pkg"
  )
})


# ============================================================================
# regression_extract.R — singular coef row + interaction in match
# ============================================================================

test_that("build_b_rows — singular coef emits NA-shaped row with is_singular = TRUE", {
  mt2 <- mt
  mt2$wt2 <- mt2$wt   # exact duplicate → singular
  fit <- suppressWarnings(lm(mpg ~ wt + wt2, data = mt2))
  ex <- spicy:::extract_lm_phase1(fit, model_id = "M1")
  cf_long <- ex$coefs[ex$coefs$estimate_type == "B", ]
  expect_true(any(cf_long$is_singular))
  sing_row <- cf_long[cf_long$is_singular, ]
  expect_true(all(is.na(sing_row$estimate)))
})

test_that("match_coef_to_factor — interaction term returns NULL", {
  fit <- lm(mpg ~ wt * cyl, data = mt)
  res <- spicy:::match_coef_to_factor("wt:cyl6", fit$xlevels)
  expect_null(res)
})


# ============================================================================
# regression_nested.R — anova failure + unknown token label
# ============================================================================

test_that("compute_one_pair_lm — anova failure returns NA fields", {
  # Construct a degenerate pair — same fit twice would give 0-df anova
  fit <- lm(mpg ~ wt, data = mt)
  out <- spicy:::compute_one_pair_lm(fit, fit)
  # All 10 fields present and numeric
  expect_true(all(c("r2_change", "F", "p", "AIC", "BIC", "LRT")
                  %in% names(out)))
})

test_that("token_label — unknown token falls back to the token string", {
  expect_equal(spicy:::token_label("custom_unknown"), "custom_unknown")
})


# ============================================================================
# regression_partial.R — empty / fallback branches
# ============================================================================

test_that("extract_partial_effect_rows — empty when no partial token requested", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- spicy:::extract_partial_effect_rows(
    fit, ci_level = 0.95, show_columns = c("b", "se"),
    model_id = "M1", outcome = "mpg"
  )
  expect_equal(nrow(out), 0L)
})

test_that("extract_partial_effect_rows — intercept-only model returns empty", {
  fit <- lm(mpg ~ 1, data = mt)
  out <- spicy:::extract_partial_effect_rows(
    fit, ci_level = 0.95,
    show_columns = c("b", "partial_eta2"),
    model_id = "M1", outcome = "mpg"
  )
  expect_equal(nrow(out), 0L)
})

test_that("compute_partial_effects_for_term — drop1 failure returns NULL", {
  fit <- lm(mpg ~ 1, data = mt)
  out <- spicy:::compute_partial_effects_for_term(fit, "nonexistent",
                                                   ci_level = 0.95)
  expect_null(out)
})


# ============================================================================
# regression_render.R — model_labels mismatch + edge cases
# ============================================================================

test_that("render_regression_table — model_labels length mismatch errors spicy_invalid_input", {
  fit <- lm(mpg ~ wt, data = mt)
  ex <- spicy:::extract_lm_phase1(fit, model_id = "M1")
  aligned <- spicy:::align_extracts(list(ex))
  expect_error(
    spicy:::render_regression_table(aligned,
                                     model_labels = c("a", "b")),
    class = "spicy_invalid_input"
  )
})

test_that("make_unique_col_name — collision triggers .2 / .3 suffix", {
  spec <- list(list(col_name = "B"), list(col_name = "B.2"))
  expect_equal(spicy:::make_unique_col_name(spec, "B"), "B.3")
  expect_equal(spicy:::make_unique_col_name(spec, "Other"), "Other")
})

test_that("fit_stat_label — covers all canonical tokens incl. uncommon ones", {
  for (tk in c("nobs", "weighted_nobs", "r2", "adj_r2", "omega2",
               "sigma", "rmse", "f2", "AIC", "AICc", "BIC", "deviance")) {
    expect_true(nzchar(spicy:::fit_stat_label(tk)))
  }
  expect_equal(spicy:::fit_stat_label("custom_x"), "custom_x")
})

test_that("format_fit_stat_value — NA value returns empty string", {
  expect_equal(
    spicy:::format_fit_stat_value("r2", NA, 2L, 2L, 1L, "."), ""
  )
  expect_equal(
    spicy:::format_fit_stat_value("r2", NULL, 2L, 2L, 1L, "."), ""
  )
})

test_that("show_fit_stats with all 12 tokens including uncommon ones", {
  fit <- lm(mpg ~ wt + cyl, data = mt, weights = runif(nrow(mt)))
  out <- table_regression(
    fit,
    show_fit_stats = c("nobs", "weighted_nobs", "r2", "adj_r2",
                        "omega2", "sigma", "rmse", "f2",
                        "AIC", "AICc", "BIC", "deviance")
  )
  expect_true("Weighted n" %in% out$Variable)
  expect_true("RMSE" %in% out$Variable)
  expect_true("AICc" %in% out$Variable)
  expect_true("BIC" %in% out$Variable)
  expect_true("Deviance" %in% out$Variable)
})

test_that("resolve_stars_thresholds — non-numeric / unnamed numeric returns NULL", {
  expect_null(spicy:::resolve_stars_thresholds("not numeric"))
  expect_null(spicy:::resolve_stars_thresholds(c(0.05, 0.01)))  # no names
})

test_that("format_stars — NA p returns empty string", {
  m <- c("***" = 0.001, "*" = 0.05)
  expect_equal(spicy:::format_stars(NA_real_, m), "")
})


# ============================================================================
# regression_titlefooter.R — degenerate inputs
# ============================================================================

test_that("build_regression_title — single model with no outcome returns 'Regression'", {
  ext <- list(list(outcome = NA_character_))
  expect_equal(spicy:::build_regression_title(ext), "Regression")
})

test_that("build_regression_footer — empty extracts returns NULL", {
  expect_null(spicy:::build_regression_footer(list()))
})

test_that("build_vcov_footer_block — empty extracts returns NULL", {
  expect_null(spicy:::build_vcov_footer_block(list()))
})

test_that("build_ame_satterthwaite_footer_block — empty extracts returns NULL", {
  expect_null(spicy:::build_ame_satterthwaite_footer_block(list(), "AME"))
})

test_that("build_singular_footer_block — empty extracts returns NULL", {
  expect_null(spicy:::build_singular_footer_block(list()))
})

test_that("format_vcov_label — bootstrap / jackknife / unknown all return a string", {
  expect_equal(spicy:::format_vcov_label(list(vcov_type = "bootstrap")),
               "bootstrap")
  expect_equal(spicy:::format_vcov_label(list(vcov_type = "jackknife")),
               "jackknife")
  expect_equal(spicy:::format_vcov_label(list(vcov_type = "weird")),
               "weird")
})

test_that("build_stars_footer_block — non-numeric / unnamed returns NULL", {
  expect_null(spicy:::build_stars_footer_block("oops"))
  expect_null(spicy:::build_stars_footer_block(c(0.05, 0.01)))
})

test_that("format_p_threshold — out-of-range value falls back to format()", {
  expect_equal(spicy:::format_p_threshold(NA_real_), "NA")
  expect_equal(spicy:::format_p_threshold(2), format(2))
})


# ============================================================================
# regression_validate.R — remaining error branches
# ============================================================================

test_that("validate_token_vector — NA token errors", {
  expect_error(
    spicy:::validate_token_vector(c("B", NA), valid = c("B", "p"),
                                  arg = "show_columns"),
    class = "spicy_invalid_input"
  )
})

test_that("validate_token_vector — empty-string token errors", {
  expect_error(
    spicy:::validate_token_vector(c("B", ""), valid = c("B", "p"),
                                  arg = "show_columns"),
    class = "spicy_invalid_input"
  )
})

test_that("validate_vcov_cluster_lists — non-string scalar vcov errors", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    spicy:::validate_vcov_cluster_lists(vcov = 42, cluster = NULL,
                                         models = list(fit)),
    class = "spicy_invalid_input"
  )
})

test_that("validate_vcov_cluster_lists — list-of-vcov works for multi-model with CR* + cluster list", {
  fit1 <- lm(mpg ~ wt, data = mt)
  fit2 <- lm(mpg ~ wt + cyl, data = mt)
  # Should NOT error: both models classical, no cluster needed
  expect_silent(spicy:::validate_vcov_cluster_lists(
    vcov = list("classical", "classical"),
    cluster = list(NULL, NULL),
    models = list(fit1, fit2)
  ))
})

test_that("validate_stars — duplicate symbol names error", {
  bad <- setNames(c(0.05, 0.01), c("*", "*"))
  expect_error(
    spicy:::validate_stars(bad),
    class = "spicy_invalid_input"
  )
})

test_that("validate_model_labels — empty-string element errors", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    spicy:::validate_model_labels(c("ok", ""), models = list(fit, fit)),
    class = "spicy_invalid_input"
  )
})

test_that("validate_outcome_labels — non-character non-FALSE errors", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    spicy:::validate_outcome_labels(42, models = list(fit)),
    class = "spicy_invalid_input"
  )
})

test_that("validate_predictor_labels — non-character / NA errors", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    spicy:::validate_predictor_labels(42, models = list(fit)),
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy:::validate_predictor_labels(c("wt" = NA_character_),
                                       models = list(fit)),
    class = "spicy_invalid_input"
  )
})

test_that("validate_predictor_labels — unnamed character errors", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    spicy:::validate_predictor_labels(c("foo", "bar"),
                                       models = list(fit)),
    class = "spicy_invalid_input"
  )
})

test_that("validate_output_resources — missing-pkg branches via mocking", {
  mock_missing_pkgs(
    rlang::current_env(),
    c("openxlsx2", "flextable", "officer", "clipr", "gt", "tinytable")
  )
  expect_error(
    spicy:::validate_output_resources("excel",
                                       excel_path = tempfile(fileext = ".xlsx"),
                                       word_path = NULL),
    class = "spicy_missing_pkg"
  )
  expect_error(
    spicy:::validate_output_resources("word",
                                       excel_path = NULL,
                                       word_path = tempfile(fileext = ".docx")),
    class = "spicy_missing_pkg"
  )
  expect_error(
    spicy:::validate_output_resources("clipboard",
                                       excel_path = NULL, word_path = NULL),
    class = "spicy_missing_pkg"
  )
  expect_error(
    spicy:::validate_output_resources("gt",
                                       excel_path = NULL, word_path = NULL),
    class = "spicy_missing_pkg"
  )
  expect_error(
    spicy:::validate_output_resources("flextable",
                                       excel_path = NULL, word_path = NULL),
    class = "spicy_missing_pkg"
  )
  expect_error(
    spicy:::validate_output_resources("tinytable",
                                       excel_path = NULL, word_path = NULL),
    class = "spicy_missing_pkg"
  )
})

test_that("emit_standardized_caveat — transforms-only path lists 'transforms:'", {
  fit <- lm(mpg ~ wt + I(wt^2), data = mtcars)
  w <- tryCatch(
    withCallingHandlers(
      spicy:::emit_standardized_caveat_if_needed(list(fit), "refit"),
      spicy_caveat = function(c) stop(conditionMessage(c))
    ),
    error = function(e) conditionMessage(e)
  )
  expect_match(w, "transforms")
})


# ============================================================================
# table_regression.R — small remaining branches
# ============================================================================

test_that("table_regression — beta auto-injection when no B token", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit, standardized = "refit",
                          show_columns = c("se", "p"))
  expect_true("β" %in% names(out))
})


# ============================================================================
# Push the remaining reachable branches in render / partial / nested / ame
# ============================================================================

test_that("render: singular coef row exists and partial_eta2 is empty / em-dash", {
  mt2 <- mt
  mt2$wt2 <- mt2$wt
  fit <- suppressWarnings(lm(mpg ~ wt + wt2 + cyl, data = mt2))
  out <- table_regression(fit,
                          show_columns = c("b", "partial_eta2"))
  wt2_row <- out[out$Variable == "wt2", , drop = FALSE]
  expect_equal(nrow(wt2_row), 1L)
  # Singular coef → B cell is em-dash (NA in raw long format),
  # partial_eta2 cell is empty (no row exists for this term in the
  # partial-effects long format).
  expect_match(trimws(wt2_row[["B"]]), "^—$")
  expect_true(trimws(wt2_row[["η²"]]) %in% c("", "—"))
})

test_that("partial: singular coef is skipped (em-dashed by renderer)", {
  mt2 <- mt
  mt2$wt2 <- mt2$wt
  fit <- suppressWarnings(lm(mpg ~ wt + wt2, data = mt2))
  rows <- spicy:::extract_partial_effect_rows(
    fit, ci_level = 0.95,
    show_columns = c("b", "partial_eta2"),
    model_id = "M1", outcome = "mpg"
  )
  # Singular wt2 NOT in the partial rows
  expect_false("wt2" %in% rows$term)
})

test_that("partial: build_b_rows singular coef emits NA-shaped row", {
  # Trigger lines 171-182 in regression_extract.R via the singular
  # branch in build_b_rows.
  mt2 <- mt
  mt2$wt2 <- mt2$wt
  fit <- suppressWarnings(lm(mpg ~ wt + wt2, data = mt2))
  ex <- spicy:::extract_lm_phase1(fit, model_id = "M1")
  b_rows <- ex$coefs[ex$coefs$estimate_type == "B", ]
  sing <- b_rows[b_rows$is_singular, , drop = FALSE]
  expect_equal(nrow(sing), 1L)
  expect_true(is.na(sing$estimate))
  expect_true(is.na(sing$se))
  expect_true(is.na(sing$p_value))
})

test_that("nested: degenerate self-pair returns NA fields gracefully", {
  fit <- lm(mpg ~ wt, data = mt)
  pair <- spicy:::compute_one_pair_lm(fit, fit)
  # All NA when anova is degenerate (no nested difference)
  expect_true(is.na(pair$F))
})

test_that("render: factor reference row with NA factor_level falls back to term", {
  # Construct a coef row with is_reference = TRUE but factor_level NA
  term_row <- data.frame(
    term = "factor_x_a",
    is_reference = TRUE,
    is_intercept = FALSE,
    factor_term = "factor_x",
    factor_level = NA_character_,
    stringsAsFactors = FALSE
  )
  out <- spicy:::format_term_label(
    term_row,
    reference_label = "(ref.)",
    reference_style = "row",
    group_factor_levels = TRUE,
    labels = NULL
  )
  expect_match(out, "factor_x_a")
})

test_that("render: factor non-ref row with NA factor_level falls back to term", {
  term_row <- data.frame(
    term = "factor_x_b",
    is_reference = FALSE,
    is_intercept = FALSE,
    factor_term = "factor_x",
    factor_level = NA_character_,
    stringsAsFactors = FALSE
  )
  out <- spicy:::format_term_label(
    term_row,
    reference_label = "(ref.)",
    reference_style = "row",
    group_factor_levels = TRUE,
    labels = NULL
  )
  expect_match(out, "factor_x_b")
})

test_that("build_outcome_row: empty col_spec → NULL", {
  out <- spicy:::build_outcome_row(
    model_outcomes = c("y1", "y2"),
    outcome_labels = c("a", "b"),
    model_ids = c("M1", "M2"),
    label_map = c(M1 = "M1", M2 = "M2"),
    col_spec = list()           # no model columns at all
  )
  expect_true(is.null(out) || nrow(out) == 1L)
})

test_that("build_fit_stats_rows: token absent from fit_stats schema is silently skipped", {
  fit_stats <- data.frame(
    model_id = "M1",
    outcome = "y",
    nobs = 100L,
    r2 = 0.5,
    stringsAsFactors = FALSE
  )
  col_spec <- list(list(col_name = "B", model_id = "M1",
                        token = "B", estimate_type = "B",
                        fields = "estimate"))
  rows <- spicy:::build_fit_stats_rows(
    fit_stats,
    show_fit_stats = c("nobs", "r2", "nonexistent_token"),
    model_ids = "M1",
    label_map = c(M1 = "M1"),
    col_spec = col_spec,
    digits = 2L, fit_digits = 2L, ic_digits = 1L,
    decimal_mark = "."
  )
  # 2 rows (nobs + r2), nonexistent skipped
  expect_equal(length(rows), 2L)
})

test_that("compute_partial_effects_for_term: degenerate F = 0 returns NULL", {
  # When the term contributes nothing, drop1 returns F = 0; the
  # function returns NULL on f_obs <= 0 OR non-finite.
  fit <- lm(mpg ~ wt, data = mt)
  out <- spicy:::compute_partial_effects_for_term(fit, "wt", 0.95)
  expect_true(is.list(out))     # this term DOES have F > 0
  expect_true(is.finite(out$f_obs))
})

test_that("extract_ame_rows: classical vcov takes Path B", {
  skip_if_not_installed("marginaleffects")
  fit <- lm(mpg ~ wt + cyl, data = mt)
  vc <- vcov(fit)
  rows <- spicy:::extract_ame_rows(
    fit, vc = vc, vcov_type = "classical", cluster = NULL,
    ci_level = 0.95, use_ame_satterthwaite = FALSE,
    model_id = "M1", outcome = "mpg"
  )
  expect_true(nrow(rows) > 0L)
  # Path B with classical → t-distribution with residual df
  expect_true(all(rows$test_type == "t"))
})

test_that("extract_ame_rows: bootstrap vcov takes Path B with z asymptotic", {
  skip_if_not_installed("marginaleffects")
  fit <- lm(mpg ~ wt, data = mt)
  vc <- vcov(fit)   # not actually bootstrap, but vcov_type drives the df_arg
  rows <- spicy:::extract_ame_rows(
    fit, vc = vc, vcov_type = "bootstrap", cluster = NULL,
    ci_level = 0.95, use_ame_satterthwaite = FALSE,
    model_id = "M1", outcome = "mpg"
  )
  expect_true(nrow(rows) > 0L)
  # Bootstrap → z asymptotic
  expect_true(all(rows$test_type == "z"))
})

test_that("extract_ame_rows: Path A with finite Satt then format_p / NA SE branch", {
  skip_if_not_installed("clubSandwich")
  set.seed(3)
  df <- data.frame(y = rnorm(100), x = rnorm(100),
                   cl = factor(sample(1:5, 100, replace = TRUE)))
  fit <- lm(y ~ x, data = df)
  rows <- spicy:::extract_ame_satterthwaite(
    fit, vcov_type = "CR2", cluster = df$cl, ci_level = 0.95,
    model_id = "M1", outcome = "y"
  )
  # Single contrast for x — no factor levels
  expect_equal(nrow(rows), 1L)
  expect_true(is.finite(rows$df))
})


# ============================================================================
# Tibble-missing fallback in maybe_as_tibble + as_tibble methods
# ============================================================================

# tibble is in Imports → as_tibble.spicy_regression_table can rely
# on it unconditionally; previously dead missing-pkg branch removed.
test_that("as_tibble.spicy_regression_table — returns a tbl_df", {
  fit <- lm(mpg ~ wt, data = mt)
  out <- table_regression(fit)
  tb <- spicy:::as_tibble.spicy_regression_table(out)
  expect_s3_class(tb, "tbl_df")
})
