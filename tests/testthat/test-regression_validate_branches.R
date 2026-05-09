# Tests for the residual error branches in regression_validate.R.
# Covers branches that are unreachable from typical successful calls
# but matter as part of the spicy_invalid_input / spicy_unsupported
# contract documented in ?table_regression.

mt <- mtcars
mt$cyl <- factor(mt$cyl)


# ============================================================================
# validate_models_input — empty list, data.frame, glm, merMod, other class
# ============================================================================

test_that("validate_models_input — empty list errors spicy_invalid_input", {
  expect_error(
    spicy:::validate_models_input(list()),
    class = "spicy_invalid_input"
  )
})

test_that("validate_models_input — data.frame errors spicy_unsupported with redirect to lm()", {
  expect_error(
    spicy:::validate_models_input(mt),
    class = "spicy_unsupported"
  )
})

test_that("validate_models_input — glm errors with 0.15.0 roadmap message", {
  fit <- glm(am ~ mpg, data = mtcars, family = binomial)
  err <- tryCatch(
    spicy:::validate_models_input(fit),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_unsupported")
  expect_match(conditionMessage(err), "0\\.15\\.0")
})

test_that("validate_models_input — merMod-like class errors with 0.16+ roadmap", {
  # Construct a minimal object inheriting from merMod without needing lme4
  fake <- structure(list(), class = c("lmerMod", "merMod"))
  err <- tryCatch(
    spicy:::validate_models_input(fake),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_unsupported")
  expect_match(conditionMessage(err), "0\\.16")
})

test_that("validate_models_input — other class errors with 'open an issue' hint", {
  fake <- structure(list(), class = "weird_unknown_class")
  err <- tryCatch(
    spicy:::validate_models_input(fake),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_unsupported")
  expect_match(conditionMessage(err), "open an issue")
})

test_that("validate_models_input — multi-position aggregate-fail lists ALL bad positions", {
  fit_ok <- lm(mpg ~ wt, data = mtcars)
  fit_bad <- glm(am ~ mpg, data = mtcars, family = binomial)
  err <- tryCatch(
    spicy:::validate_models_input(list(fit_ok, fit_bad, fit_bad)),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_unsupported")
  expect_match(conditionMessage(err), "Position 2")
  expect_match(conditionMessage(err), "Position 3")
})


# ============================================================================
# validate_token_vector — duplicate detection
# ============================================================================

test_that("validate_token_vector — duplicates error spicy_invalid_input", {
  expect_error(
    spicy:::validate_token_vector(
      c("B", "B", "p"),
      valid = c("B", "SE", "p"),
      arg = "show_columns"
    ),
    class = "spicy_invalid_input"
  )
})

test_that("validate_token_vector — non-character errors", {
  expect_error(
    spicy:::validate_token_vector(
      1:3, valid = c("B"), arg = "show_columns"
    ),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# validate_boot_n / validate_logical_scalar / validate_ci_level — error branches
# ============================================================================

test_that("validate_boot_n — non-integer / negative errors", {
  expect_error(
    spicy:::validate_boot_n("oops"),
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy:::validate_boot_n(-50),
    class = "spicy_invalid_input"
  )
})

test_that("validate_logical_scalar — vector / NA errors", {
  expect_error(
    spicy:::validate_logical_scalar(c(TRUE, FALSE), "show_intercept"),
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy:::validate_logical_scalar(NA, "nested"),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# validate_vcov_cluster_lists — list-of-non-strings, length mismatch
# ============================================================================

test_that("validate_vcov_cluster_lists — list element not a single string errors", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    spicy:::validate_vcov_cluster_lists(
      vcov = list(c("HC3", "HC0")), cluster = NULL, models = list(fit)
    ),
    class = "spicy_invalid_input"
  )
})

test_that("validate_vcov_cluster_lists — cluster list length mismatch errors", {
  fit1 <- lm(mpg ~ wt, data = mt)
  fit2 <- lm(mpg ~ cyl, data = mt)
  expect_error(
    spicy:::validate_vcov_cluster_lists(
      vcov = list("classical", "classical"),
      cluster = list(NULL),                # length 1 vs 2 models
      models = list(fit1, fit2)
    ),
    class = "spicy_invalid_input"
  )
})

test_that("validate_vcov_cluster_lists — cluster vector length mismatch errors", {
  fit <- lm(mpg ~ wt, data = mt)
  expect_error(
    spicy:::validate_vcov_cluster_lists(
      vcov = "CR2", cluster = 1:5, models = list(fit)
    ),
    class = "spicy_invalid_input"
  )
})


# ============================================================================
# detect_ame_satterthwaite_path — flag detection
# ============================================================================

test_that("detect_ame_satterthwaite_path — TRUE only when AME requested AND any vcov is CR*", {
  expect_true(spicy:::detect_ame_satterthwaite_path("CR2", c("B", "AME")))
  expect_true(spicy:::detect_ame_satterthwaite_path(
    list("CR2", "classical"), c("B", "AME")
  ))
  expect_false(spicy:::detect_ame_satterthwaite_path("HC3", c("B", "AME")))
  expect_false(spicy:::detect_ame_satterthwaite_path("CR2", c("B", "p")))
  expect_false(spicy:::detect_ame_satterthwaite_path(
    list("classical", "HC3"), c("B", "AME")
  ))
})


# ============================================================================
# emit_standardized_caveat_if_needed — posthoc / basic / smart caveat text
# ============================================================================

test_that("emit_standardized_caveat — posthoc / basic / smart use SD-of-product wording", {
  fit_int <- lm(mpg ~ wt * cyl, data = mt)
  for (method in c("posthoc", "basic", "smart")) {
    w <- tryCatch(
      withCallingHandlers(
        spicy:::emit_standardized_caveat_if_needed(list(fit_int), method),
        spicy_caveat = function(c) {
          # capture the warning message body and signal an error so the
          # outer tryCatch returns it
          stop(conditionMessage(c))
        }
      ),
      error = function(e) conditionMessage(e)
    )
    expect_match(w, "SD of the product")
  }
})

test_that("emit_standardized_caveat — refit method uses 'after refit' wording", {
  fit_int <- lm(mpg ~ wt * cyl, data = mt)
  w <- tryCatch(
    withCallingHandlers(
      spicy:::emit_standardized_caveat_if_needed(list(fit_int), "refit"),
      spicy_caveat = function(c) stop(conditionMessage(c))
    ),
    error = function(e) conditionMessage(e)
  )
  expect_match(w, "After refit on z-scored")
})

test_that("emit_standardized_caveat — silent when 'none' or no non-additive terms", {
  fit_add <- lm(mpg ~ wt + cyl, data = mt)
  expect_silent(spicy:::emit_standardized_caveat_if_needed(list(fit_add), "refit"))
  fit_int <- lm(mpg ~ wt * cyl, data = mt)
  expect_silent(spicy:::emit_standardized_caveat_if_needed(list(fit_int), "none"))
})


# ============================================================================
# validate_output_resources — clipboard branch + dir-not-exists branches
# ============================================================================

test_that("validate_output_resources — excel without path errors", {
  expect_error(
    spicy:::validate_output_resources("excel", excel_path = NULL,
                                       word_path = NULL),
    class = "spicy_invalid_input"
  )
})

test_that("validate_output_resources — word without path errors", {
  expect_error(
    spicy:::validate_output_resources("word", excel_path = NULL,
                                       word_path = NULL),
    class = "spicy_invalid_input"
  )
})

test_that("validate_output_resources — non-existent directory errors", {
  expect_error(
    spicy:::validate_output_resources("excel",
                                       excel_path = "/no/such/dir/x.xlsx",
                                       word_path = NULL),
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy:::validate_output_resources("word",
                                       excel_path = NULL,
                                       word_path = "/no/such/dir/x.docx"),
    class = "spicy_invalid_input"
  )
})

test_that("validate_output_resources — default / data.frame / long are no-ops", {
  expect_silent(spicy:::validate_output_resources("default", NULL, NULL))
  expect_silent(spicy:::validate_output_resources("data.frame", NULL, NULL))
  expect_silent(spicy:::validate_output_resources("long", NULL, NULL))
})


# ============================================================================
# detect_non_additive_terms — interaction + transform detection
# ============================================================================

test_that("detect_non_additive_terms — additive: has_problem = FALSE", {
  fit <- lm(mpg ~ wt + cyl + am, data = mt)
  out <- spicy:::detect_non_additive_terms(fit)
  expect_false(out$has_problem)
  expect_length(out$interactions, 0L)
  expect_length(out$transforms, 0L)
})

test_that("detect_non_additive_terms — interaction: has_problem TRUE + listed", {
  fit <- lm(mpg ~ wt * cyl, data = mt)
  out <- spicy:::detect_non_additive_terms(fit)
  expect_true(out$has_problem)
  expect_true("wt:cyl" %in% out$interactions)
})

test_that("detect_non_additive_terms — transform: I() / poly() listed", {
  fit <- lm(mpg ~ I(wt^2) + poly(hp, 2), data = mtcars)
  out <- spicy:::detect_non_additive_terms(fit)
  expect_true(out$has_problem)
  expect_true(any(grepl("I\\(", out$transforms)))
  expect_true(any(grepl("poly\\(", out$transforms)))
})
