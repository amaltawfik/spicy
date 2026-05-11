# Tests for table_regression() title + footer auto-generation (Step 7 / Q13).
# All builders are internal — accessed via spicy::: namespace.

# ---- Helper: build a minimal extract object for tests --------------------

mk_extract <- function(outcome = "y",
                       vcov_type = "classical",
                       cluster_name = NULL,
                       use_ame_satterthwaite = FALSE,
                       has_singular = FALSE,
                       fit = NULL,
                       non_additive = NULL) {
  list(
    outcome = outcome,
    vcov_type = vcov_type,
    cluster_name = cluster_name,
    use_ame_satterthwaite = use_ame_satterthwaite,
    has_singular = has_singular,
    fit = fit,
    non_additive = non_additive
  )
}


# ============================================================================
# Title (Q13)
# ============================================================================

test_that("title — single model uses 'Regression: <DV>'", {
  expect_equal(
    spicy:::build_regression_title(list(mk_extract(outcome = "mpg"))),
    "Regression: mpg"
  )
})

test_that("title — nested = TRUE uses 'Hierarchical regression: <DV>'", {
  ext <- list(mk_extract(outcome = "mpg"), mk_extract(outcome = "mpg"))
  expect_equal(
    spicy:::build_regression_title(ext, nested = TRUE),
    "Hierarchical regression: mpg"
  )
})

test_that("title — multi-model identical DV uses 'Regression comparison: <DV>'", {
  ext <- list(
    mk_extract(outcome = "mpg"),
    mk_extract(outcome = "mpg"),
    mk_extract(outcome = "mpg")
  )
  expect_equal(
    spicy:::build_regression_title(ext, nested = FALSE),
    "Regression comparison: mpg"
  )
})

test_that("title — multi-model mixed DVs uses 'Regression comparison'", {
  ext <- list(mk_extract(outcome = "mpg"), mk_extract(outcome = "hp"))
  expect_equal(
    spicy:::build_regression_title(ext, nested = FALSE),
    "Regression comparison"
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

test_that("AME-Satt block — emitted only when AME ∈ show_columns AND any model uses Satt", {
  ext_satt <- list(mk_extract(use_ame_satterthwaite = TRUE))
  ext_no   <- list(mk_extract(use_ame_satterthwaite = FALSE))
  cols     <- c("B", "AME")

  expect_match(
    spicy:::build_ame_satterthwaite_footer_block(ext_satt, cols),
    "Satterthwaite-corrected df.*Pustejovsky & Tipton 2018"
  )
  expect_null(spicy:::build_ame_satterthwaite_footer_block(ext_no, cols))
  expect_null(spicy:::build_ame_satterthwaite_footer_block(ext_satt,
                                                           c("B", "SE")))
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
    show_columns = c("B", "SE", "AME", "p")
  )
  expect_match(out, "^Note\\. ")
  # vcov + AME-Satt + stars => 3 themes, 2 newlines
  expect_equal(length(strsplit(out, "\n", fixed = TRUE)[[1]]), 3L)
  expect_match(out, "Std\\. errors: cluster-robust \\(CR2\\)")
  expect_match(out, "Satterthwaite")
  expect_match(out, "\\*\\*\\* p < \\.001")
})

test_that("full footer — returns NULL when no theme applies", {
  ext <- list(mk_extract(vcov_type = "classical"))
  # classical vcov DOES emit a line ("Std. errors: classical (OLS).") so
  # we'd get a footer — that's correct: a regression table always has at
  # minimum a vcov declaration. Sanity-check that footer is non-NULL.
  out <- spicy:::build_regression_footer(
    ext, standardized = "none", stars = FALSE, nested = FALSE,
    show_columns = c("B")
  )
  expect_match(out, "^Note\\. Std\\. errors:")
})
