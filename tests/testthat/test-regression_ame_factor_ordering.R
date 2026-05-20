# Tests for AME factor-level ordering on ordered factors across glm
# families.
#
# Bug history: pre-fix, the glm AME path (`extract_ame_glm`) used
# marginaleffects' default ordering, which sorts factor contrasts
# alphabetically. For an ordered factor like `education`
# (Lower < Upper < Tertiary), the AME rows displayed as
# (Tertiary, Upper) -- inconsistent with the B-coefficient rows
# (which are .L / .Q polynomial trends in proper degree order) and
# with the lm AME path (which explicitly sorts by `levels(factor)`).
#
# The fix in `extract_ame_glm` stashes the factor-level position
# (`match(lvl, levels(mf[[var]]))`) per row and sorts rows by it.
# It is family-agnostic (binomial / poisson / gamma / quasi all go
# through the same code), and applies to ordered factors which are
# the case marginaleffects displays as treatment-style contrasts
# even though the model is fit with `contr.poly`.

skip_if_no_marginaleffects <- function() {
  testthat::skip_if_not_installed("marginaleffects")
}

# Extract AME row term names for a given factor variable, in order
# of appearance.
ame_factor_order <- function(out, factor_var) {
  long <- broom::tidy(out)
  ame_rows <- long[long$estimate_type == "AME" &
                     startsWith(long$term, factor_var), ]
  ame_rows$term
}


test_that("lm AME: ordered-factor rows in levels() order, not alphabetical", {
  skip_if_no_marginaleffects()
  # education levels: "Lower secondary" < "Upper secondary" < "Tertiary".
  # Alphabetical: Lower, Tertiary, Upper -> non-ref AME = (Tertiary, Upper).
  # Factor-level order: Upper, Tertiary.
  out <- table_regression(
    lm(age ~ sex + education, data = sochealth),
    show_columns = c("b", "ame", "ame_ci", "ame_p")
  )
  ord <- ame_factor_order(out, "education")
  expect_equal(ord, c("educationUpper secondary", "educationTertiary"))
})


test_that("glm(binomial) AME: ordered-factor rows in levels() order", {
  skip_if_no_marginaleffects()
  out <- table_regression(
    glm(smoking ~ sex + age + education, data = sochealth,
        family = binomial),
    show_columns = c("b", "ame", "ame_ci", "ame_p")
  )
  ord <- ame_factor_order(out, "education")
  expect_equal(ord, c("educationUpper secondary", "educationTertiary"))
})


test_that("glm(poisson) AME: ordered-factor rows in levels() order (family-agnostic)", {
  skip_if_no_marginaleffects()
  # Synthetic count outcome on an ordered factor with deliberately
  # non-alphabetical levels: Beta < Gamma < Alpha (otherwise alphabetical
  # would coincide with levels-of-construction order).
  set.seed(1)
  df <- data.frame(
    y = rpois(300, lambda = 2),
    x = rnorm(300),
    grp = ordered(sample(c("Beta", "Gamma", "Alpha"), 300, replace = TRUE),
                  levels = c("Beta", "Gamma", "Alpha"))
  )
  out <- table_regression(
    glm(y ~ x + grp, data = df, family = poisson),
    show_columns = c("b", "ame", "ame_p")
  )
  ord <- ame_factor_order(out, "grp")
  # Reference = first level = "Beta".
  # Levels order of non-ref: Gamma (pos 2), Alpha (pos 3).
  # Alphabetical would put Alpha first (A < G).
  expect_equal(ord, c("grpGamma", "grpAlpha"))
})


# ============================================================================
# Synthetic reference row for ordered factors when AME is requested
# ============================================================================
# Polynomial contrasts have no concept of a reference level (`.L` / `.Q`
# are orthogonal trends, not comparisons against a baseline). But when
# AME is requested alongside, marginaleffects emits per-level contrasts
# against `levels()[1]`, and the reader needs to see which level is the
# baseline. `build_reference_rows()` emits a synthetic ref row anchored
# on `levels()[1]` only when AME is in show_columns -- the row is
# em-dashed in both B and AME columns (consistent with how plain
# factors already display their reference row).

test_that("ordered factor + AME: synthetic reference row is emitted", {
  skip_if_no_marginaleffects()
  out <- table_regression(
    glm(smoking ~ sex + age + education, data = sochealth,
        family = binomial),
    show_columns = c("b", "p", "ame", "ame_ci", "ame_p")
  )
  body <- as.data.frame(out, stringsAsFactors = FALSE)
  vars <- trimws(body$Variable)
  # The reference row appears with the "(ref.)" annotation. For an
  # ordered factor whose first level is "Lower secondary", the row
  # label is "Lower secondary (ref.)".
  expect_true(any(grepl("Lower secondary \\(ref\\.\\)", vars)))
})

test_that("ordered factor WITHOUT AME: no synthetic reference row", {
  out <- table_regression(
    glm(smoking ~ sex + age + education, data = sochealth,
        family = binomial)
  )
  body <- as.data.frame(out, stringsAsFactors = FALSE)
  vars <- trimws(body$Variable)
  # When no AME is requested, the poly trends `.L` / `.Q` stand alone
  # with no reference anchor (they're not contrasts against a baseline).
  expect_false(any(grepl("Lower secondary \\(ref\\.\\)", vars)))
})
