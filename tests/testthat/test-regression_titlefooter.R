# Tests for table_regression() title + footer auto-generation (Step 7 / Q13).
# All builders are internal — accessed via spicy::: namespace.

# ---- Helper: build a minimal extract object for tests --------------------

mk_extract <- function(outcome = "y",
                       vcov_type = "classical",
                       cluster_name = NULL,
                       use_ame_satterthwaite = FALSE,
                       has_singular = FALSE,
                       fit = NULL,
                       non_additive = NULL,
                       title_prefix = "Linear regression") {
  list(
    outcome = outcome,
    vcov_type = vcov_type,
    cluster_name = cluster_name,
    use_ame_satterthwaite = use_ame_satterthwaite,
    has_singular = has_singular,
    fit = fit,
    non_additive = non_additive,
    title_prefix = title_prefix
  )
}


# ============================================================================
# Title (Q13)
# ============================================================================

test_that("title — single model uses '<type> regression: <DV>'", {
  expect_equal(
    spicy:::build_regression_title(list(mk_extract(outcome = "mpg"))),
    "Linear regression: mpg"
  )
})

test_that("title — nested = TRUE uses 'Hierarchical <type> regression: <DV>'", {
  ext <- list(mk_extract(outcome = "mpg"), mk_extract(outcome = "mpg"))
  expect_equal(
    spicy:::build_regression_title(ext, nested = TRUE),
    "Hierarchical linear regression: mpg"
  )
})

test_that("title — multi-model identical DV uses '<type> regression comparison: <DV>'", {
  ext <- list(
    mk_extract(outcome = "mpg"),
    mk_extract(outcome = "mpg"),
    mk_extract(outcome = "mpg")
  )
  expect_equal(
    spicy:::build_regression_title(ext, nested = FALSE),
    "Linear regression comparison: mpg"
  )
})

test_that("title — multi-model mixed DVs uses '<type> regression comparison'", {
  ext <- list(mk_extract(outcome = "mpg"), mk_extract(outcome = "hp"))
  expect_equal(
    spicy:::build_regression_title(ext, nested = FALSE),
    "Linear regression comparison"
  )
})

test_that("title — empty list returns plain 'Regression'", {
  expect_equal(spicy:::build_regression_title(list()), "Regression")
})


# ============================================================================
# Footer — vcov block (Q7 / Q13)
# ============================================================================

test_that("vcov block — uniform classical: single line", {
  ext <- list(mk_extract(), mk_extract())
  expect_equal(
    spicy:::build_vcov_footer_block(ext),
    "Std. errors: classical (OLS)."
  )
})

test_that("vcov block — uniform HC3: single line with HC3 label", {
  ext <- list(mk_extract(vcov_type = "HC3"),
              mk_extract(vcov_type = "HC3"))
  expect_equal(
    spicy:::build_vcov_footer_block(ext),
    "Std. errors: heteroskedasticity-robust (HC3)."
  )
})

test_that("vcov block — uniform CR2 with named cluster: 'clusters by <name>'", {
  ext <- list(mk_extract(vcov_type = "CR2", cluster_name = "clinic_id"))
  expect_equal(
    spicy:::build_vcov_footer_block(ext),
    "Std. errors: cluster-robust (CR2), clusters by clinic_id."
  )
})

test_that("vcov block — CR* with no detected name: 'cluster vector supplied'", {
  ext <- list(mk_extract(vcov_type = "CR1", cluster_name = NA_character_))
  expect_match(
    spicy:::build_vcov_footer_block(ext),
    "cluster-robust \\(CR1\\), cluster vector supplied"
  )
})

test_that("vcov block — heterogeneous: indented per-model enumeration", {
  ext <- list(
    mk_extract(vcov_type = "classical"),
    mk_extract(vcov_type = "CR2", cluster_name = "clinic_id"),
    mk_extract(vcov_type = "HC3")
  )
  out <- spicy:::build_vcov_footer_block(ext)
  expect_match(out, "^Std\\. errors:\n")
  expect_match(out, "Model 1: classical \\(OLS\\)")
  expect_match(out, "Model 2: cluster-robust \\(CR2\\), clusters by clinic_id")
  expect_match(out, "Model 3: heteroskedasticity-robust \\(HC3\\)")
})

test_that("vcov block — bootstrap label is bare", {
  ext <- list(mk_extract(vcov_type = "bootstrap"))
  expect_equal(
    spicy:::build_vcov_footer_block(ext),
    "Std. errors: bootstrap."
  )
})


# ============================================================================
# Footer — AME-Satterthwaite block (Q14b)
# ============================================================================

test_that("AME-Satt block — emitted only when ame in show_columns AND any model uses Satt", {
  ext_satt <- list(mk_extract(use_ame_satterthwaite = TRUE))
  ext_no   <- list(mk_extract(use_ame_satterthwaite = FALSE))
  cols     <- c("b", "ame")

  expect_match(
    spicy:::build_ame_satterthwaite_footer_block(ext_satt, cols),
    "Satterthwaite-corrected df.*Pustejovsky & Tipton 2018"
  )
  expect_null(spicy:::build_ame_satterthwaite_footer_block(ext_no, cols))
  expect_null(spicy:::build_ame_satterthwaite_footer_block(ext_satt,
                                                           c("b", "se")))
})


# ============================================================================
# Footer — standardized caveat (Q15)
# ============================================================================

test_that("standardized caveat — NULL when standardized = 'none'", {
  fit <- lm(mpg ~ wt * cyl, data = mtcars)        # has interaction
  ext <- list(mk_extract(fit = fit))
  expect_null(spicy:::build_standardized_caveat_footer_block(ext, "none"))
})

test_that("standardized caveat — NULL when no non-additive terms", {
  fit <- lm(mpg ~ wt + cyl, data = mtcars)        # additive only
  ext <- list(mk_extract(fit = fit))
  expect_null(spicy:::build_standardized_caveat_footer_block(ext, "refit"))
})

test_that("standardized caveat — refit text mentions z-scored interaction", {
  fit <- lm(mpg ~ wt * cyl, data = mtcars)
  ext <- list(mk_extract(fit = fit))
  out <- spicy:::build_standardized_caveat_footer_block(ext, "refit")
  expect_match(out, "after refit on z-scored data")
  expect_match(out, "interaction of z-scored variables")
})

test_that("standardized caveat — posthoc/basic/smart text mentions SD of product", {
  fit <- lm(mpg ~ wt * cyl, data = mtcars)
  ext <- list(mk_extract(fit = fit))
  out <- spicy:::build_standardized_caveat_footer_block(ext, "posthoc")
  expect_match(out, "SD of the product / transformed column")
  expect_match(out, "Cohen et al\\. 2003")
})

test_that("standardized caveat — uses pre-computed non_additive metadata when provided", {
  ext <- list(mk_extract(
    fit = NULL,
    non_additive = list(has_problem = TRUE,
                        interactions = "wt:cyl",
                        transforms = character(0))
  ))
  out <- spicy:::build_standardized_caveat_footer_block(ext, "refit")
  expect_true(!is.null(out))
  expect_match(out, "after refit on z-scored data")
})


# ============================================================================
# Footer — stars (Q12)
# ============================================================================

test_that("stars — FALSE returns NULL", {
  expect_null(spicy:::build_stars_footer_block(FALSE))
})

test_that("stars — TRUE renders APA preset, strictest first", {
  expect_equal(
    spicy:::build_stars_footer_block(TRUE),
    "*** p < .001, ** p < .01, * p < .05."
  )
})

test_that("stars — custom mapping renders in increasing-strictness order", {
  out <- spicy:::build_stars_footer_block(
    c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001)
  )
  expect_equal(
    out,
    paste0("*** p < .001, ** p < .01, * p < .05, † p < .10.")
  )
})


# ============================================================================
# Footer — singular / rank-deficient (Q22)
# ============================================================================

test_that("singular block — NULL when no model is rank-deficient", {
  ext <- list(mk_extract(), mk_extract())
  expect_null(spicy:::build_singular_footer_block(ext))
})

test_that("singular block — single model wording", {
  ext <- list(mk_extract(has_singular = TRUE))
  expect_match(
    spicy:::build_singular_footer_block(ext),
    "Rank-deficient model: dropped coefficient"
  )
})

test_that("singular block — multi-model lists affected indices", {
  ext <- list(
    mk_extract(has_singular = FALSE),
    mk_extract(has_singular = TRUE),
    mk_extract(has_singular = TRUE)
  )
  out <- spicy:::build_singular_footer_block(ext)
  expect_match(out, "Model 2, Model 3")
})


# ============================================================================
# Footer — nested declaration (Q6)
# ============================================================================

test_that("nested block — always NULL: the comparison block is self-explanatory", {
  # Phase 1 returns NULL in both branches; the dedicated
  # "── Model comparison ──" block from Step 9 already announces itself.
  expect_null(spicy:::build_nested_footer_block(FALSE))
  expect_null(spicy:::build_nested_footer_block(TRUE))
})


# ============================================================================
# Full footer composition
# ============================================================================

test_that("full footer — combines themes with leading 'Note. ' and \\n separators", {
  ext <- list(mk_extract(
    vcov_type = "CR2",
    cluster_name = "clinic_id",
    use_ame_satterthwaite = TRUE
  ))
  out <- spicy:::build_regression_footer(
    ext,
    standardized = "none",
    stars = TRUE,
    nested = FALSE,
    show_columns = c("b", "se", "ame", "p")
  )
  expect_match(out, "^Note\\. ")
  # type + vcov + AME abbrev + AME-Satt + stars => 5 themes, 4 newlines
  expect_equal(length(strsplit(out, "\n", fixed = TRUE)[[1]]), 5L)
  expect_match(out, "Std\\. errors: cluster-robust \\(CR2\\)")
  expect_match(out, "AME = average marginal effect")
  expect_match(out, "Satterthwaite")
  expect_match(out, "\\*\\*\\* p < \\.001")
})

test_that("full footer — always carries the regression-type declaration", {
  ext <- list(mk_extract(vcov_type = "classical"))
  # A regression table always has at minimum a type + vcov declaration.
  out <- spicy:::build_regression_footer(
    ext, standardized = "none", stars = FALSE, nested = FALSE,
    show_columns = c("b")
  )
  expect_match(out, "^Note\\. ")
  expect_match(out, "regression\\.")
  expect_match(out, "Std\\. errors:")
})


# ============================================================================
# Footer — reference categories (reference_style = "footer")
# ============================================================================

test_that("build_reference_categories_footer_block - NULL for non-footer styles", {
  expect_null(spicy:::build_reference_categories_footer_block(list(), "row"))
  expect_null(spicy:::build_reference_categories_footer_block(list(),
                                                                "annotation"))
  expect_null(spicy:::build_reference_categories_footer_block(list(), "none"))
})

test_that("build_reference_categories_footer_block - NULL for empty extracts", {
  expect_null(spicy:::build_reference_categories_footer_block(list(),
                                                                "footer"))
})

test_that("build_reference_categories_footer_block - skips models with no coefs / no ref rows", {
  empty <- list(coefs = NULL)
  expect_null(spicy:::build_reference_categories_footer_block(list(empty),
                                                                "footer"))
  zero_rows <- list(coefs = data.frame(
    is_reference = logical(0),
    factor_term = character(0),
    factor_level = character(0)
  ))
  expect_null(spicy:::build_reference_categories_footer_block(list(zero_rows),
                                                                "footer"))
})

test_that("build_reference_categories_footer_block - dedups identical (var, level) pairs across models", {
  set.seed(1)
  df <- data.frame(
    y = rnorm(60),
    sex = factor(c(rep("F", 30), rep("M", 30)))
  )
  m1 <- lm(y ~ sex, df)
  m2 <- lm(y ~ sex, df[sample(60, 50), ])
  ex1 <- spicy:::extract_lm_phase1(m1, model_id = "M1")
  ex2 <- spicy:::extract_lm_phase1(m2, model_id = "M2")
  out <- spicy:::build_reference_categories_footer_block(list(ex1, ex2),
                                                          "footer")
  # Only ONE "sex = F" entry (not two)
  expect_equal(length(gregexpr("sex = ", out)[[1]]), 1L)
})

test_that("build_reference_categories_footer_block - skips rows with NA or empty factor_term/level", {
  bad <- list(coefs = data.frame(
    is_reference = c(TRUE, TRUE, TRUE),
    factor_term  = c(NA_character_, "", "foo"),
    factor_level = c("a", "b", NA_character_)
  ))
  expect_null(spicy:::build_reference_categories_footer_block(list(bad),
                                                                "footer"))
})

test_that("isTRUE_vec - handles NA / FALSE / TRUE elements", {
  expect_equal(spicy:::isTRUE_vec(c(TRUE, FALSE, NA, TRUE)),
                c(TRUE, FALSE, FALSE, TRUE))
  expect_equal(spicy:::isTRUE_vec(logical(0)), logical(0))
})


# ============================================================================
# Polynomial-contrasts footer + once-per-session pedagogy
# ============================================================================

test_that("polynomial footer — suffix legend reflects only degrees present", {
  # 3-level ordered factor -> .L + .Q only (no .C in this table)
  set.seed(1)
  df3 <- data.frame(
    y = rnorm(150),
    f3 = ordered(sample(1:3, 150, replace = TRUE))
  )
  fit3 <- lm(y ~ f3, df3)
  ext3 <- spicy:::extract_lm_phase1(fit3, model_id = "M1")
  out3 <- spicy:::build_polynomial_contrasts_footer_block(list(ext3))
  expect_match(out3, ".L = linear", fixed = TRUE)
  expect_match(out3, ".Q = quadratic", fixed = TRUE)
  expect_no_match(out3, ".C = cubic", fixed = TRUE)

  # 5-level ordered factor -> .L, .Q, .C, ^4
  set.seed(2)
  df5 <- data.frame(
    y = rnorm(250),
    f5 = ordered(sample(1:5, 250, replace = TRUE))
  )
  fit5 <- lm(y ~ f5, df5)
  ext5 <- spicy:::extract_lm_phase1(fit5, model_id = "M1")
  out5 <- spicy:::build_polynomial_contrasts_footer_block(list(ext5))
  expect_match(out5, ".L = linear", fixed = TRUE)
  expect_match(out5, ".Q = quadratic", fixed = TRUE)
  expect_match(out5, ".C = cubic", fixed = TRUE)
  expect_match(out5, "^4 = quartic", fixed = TRUE)
})

test_that("polynomial footer — multiple ordered factors listed plural", {
  set.seed(3)
  df <- data.frame(
    y  = rnorm(200),
    f1 = ordered(sample(1:3, 200, replace = TRUE)),
    f2 = ordered(sample(1:3, 200, replace = TRUE))
  )
  fit <- lm(y ~ f1 + f2, df)
  ext <- spicy:::extract_lm_phase1(fit, model_id = "M1")
  out <- spicy:::build_polynomial_contrasts_footer_block(list(ext))
  expect_match(out, "^Ordered factors `f1`, `f2`: polynomial trends")
})

test_that("polynomial footer — no ordered factor returns NULL", {
  fit <- lm(mpg ~ wt, data = mtcars)
  ext <- spicy:::extract_lm_phase1(fit, model_id = "M1")
  expect_null(spicy:::build_polynomial_contrasts_footer_block(list(ext)))
})

test_that("polynomial pedagogy — second call is silent (once-per-session contract)", {
  # `rlang::inform(.frequency = "once")` may have already fired by
  # a prior test in this session, so we don't assume the FIRST call
  # emits. The contract is: after ANY emission, subsequent calls in
  # the same session are silent. We exercise the function twice
  # back-to-back and verify the second call produces no message.
  set.seed(4)
  df <- data.frame(
    y = rnorm(80),
    f = ordered(sample(1:3, 80, replace = TRUE))
  )
  fit <- lm(y ~ f, df)
  ext <- spicy:::extract_lm_phase1(fit, model_id = "M1")
  # Prime: ensure the message has fired at least once.
  invisible(testthat::capture_messages(
    spicy:::build_polynomial_contrasts_footer_block(list(ext))
  ))
  # Second call: must be silent.
  msgs2 <- testthat::capture_messages(
    spicy:::build_polynomial_contrasts_footer_block(list(ext))
  )
  expect_false(any(grepl("Polynomial contrasts", msgs2)))
})

test_that("ordinal_label — special-cases .L/.Q/.C then quartic/quintic/sextic, else degree-k", {
  expect_equal(spicy:::ordinal_label(4), "quartic")
  expect_equal(spicy:::ordinal_label(5), "quintic")
  expect_equal(spicy:::ordinal_label(6), "sextic")
  expect_equal(spicy:::ordinal_label(7), "degree-7")
})
