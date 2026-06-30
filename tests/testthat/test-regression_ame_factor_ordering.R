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
  ame_rows <- long[long$estimate_type == "ame" &
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


# ============================================================================
# Edge case: 2-level ordered factor + AME (poly produces only .L)
# ============================================================================

test_that("AME extraction triggered by ame_ci / ame_p / ame_se (not only 'ame')", {
  skip_if_no_marginaleffects()
  # Pre-fix the orchestration gated the AME extraction on `"ame"`
  # alone, so requesting just `ame_ci` and `ame_p` (a legitimate
  # compact-table choice) produced empty columns in the header.
  # The fix accepts any token of the AME family.
  fit <- glm(smoking ~ age + education, data = sochealth,
             family = binomial)

  for (cols in list(
    c("b", "p", "ame_ci"),
    c("b", "p", "ame_p"),
    c("b", "p", "ame_se"),
    c("b", "p", "ame_ci", "ame_p")
  )) {
    out <- table_regression(fit, show_columns = cols)
    long <- broom::tidy(out)
    n_ame_rows <- sum(long$estimate_type == "ame", na.rm = TRUE)
    expect_gt(n_ame_rows, 0L,
              label = sprintf("show_columns = c(%s)",
                              paste(shQuote(cols), collapse = ", ")))
  }
})


test_that("ref-row em-dashes follow estimate_type semantics (plain factor: B + AME both em-dashed)", {
  skip_if_no_marginaleffects()
  # Plain treatment-coded factor with AME requested: the reference
  # level is the baseline for BOTH the B-coefficient block AND the
  # AME contrast block, so em-dashes must appear under both column
  # families. Test column ordering is mixed (ame tokens before b)
  # to confirm the em-dash follows the estimate_type semantics, not
  # the position in show_columns.
  fit <- glm(smoking ~ sex + age, data = sochealth, family = binomial)
  out <- table_regression(fit,
                          show_columns = c("ame_p", "ame", "b", "p"))
  body <- as.data.frame(out, stringsAsFactors = FALSE,
                         check.names = FALSE)
  ref_row <- body[trimws(body$Variable) == "Female (ref.)", ,
                   drop = FALSE]
  expect_equal(nrow(ref_row), 1L)
  # Every data column of the ref-row contains an em-dash, because
  # `Female` is the reference for both B and AME blocks.
  data_cells <- as.character(ref_row[, -1L])
  expect_true(all(grepl("–", data_cells)))
})


test_that("stars = TRUE suffixes stars to AME cells (independent of ame_p column being shown)", {
  skip_if_no_marginaleffects()
  # Stars on AME convey significance compactly without spending a
  # column on the p-value -- the same convention that already
  # applied to B. Verified across three configurations:
  #   * AME alone           -> stars on AME
  #   * B + AME (no p cols) -> stars on B AND on AME
  #   * B + AME + both p    -> stars on B AND on AME (independent of
  #                            p columns being displayed)
  fit <- glm(smoking ~ sex + education, data = sochealth,
             family = binomial)

  pull_cells <- function(out, col_name) {
    body <- as.data.frame(out, stringsAsFactors = FALSE,
                          check.names = FALSE)
    body[[col_name]]
  }

  # AME alone
  out1 <- table_regression(fit, stars = TRUE, show_columns = "ame")
  ame_cells_1 <- pull_cells(out1, "AME")
  expect_true(any(grepl("\\*", ame_cells_1)))

  # B + AME (no p columns)
  out2 <- table_regression(fit, stars = TRUE,
                           show_columns = c("b", "ame"))
  expect_true(any(grepl("\\*", pull_cells(out2, "B"))))
  expect_true(any(grepl("\\*", pull_cells(out2, "AME"))))

  # B + p + AME + ame_p (everything)
  out3 <- table_regression(fit, stars = TRUE,
                           show_columns = c("b", "p", "ame", "ame_p"))
  expect_true(any(grepl("\\*", pull_cells(out3, "B"))))
  expect_true(any(grepl("\\*", pull_cells(out3, "AME"))))
})


test_that("ref-row em-dashes follow estimate_type semantics (ordered factor: AME only)", {
  skip_if_no_marginaleffects()
  # Ordered factor with poly contrasts + AME requested: the .L /
  # .Q B coefficients are orthogonal polynomial TRENDS, they have
  # no per-level reference. The synthetic ref-row should em-dash
  # ONLY the AME columns and BLANK the B columns. This is the
  # pedagogical signal -- the em-dash placement tells the reader
  # WHICH estimate type the reference applies to.
  fit <- glm(smoking ~ sex + education, data = sochealth,
             family = binomial)
  out <- table_regression(fit,
                          show_columns = c("b", "p", "ame", "ame_p"))
  body <- as.data.frame(out, stringsAsFactors = FALSE,
                         check.names = FALSE)
  ref_row <- body[trimws(body$Variable) == "Lower secondary (ref.)",
                   , drop = FALSE]
  expect_equal(nrow(ref_row), 1L)
  # Columns 1 = Variable label; cols 2-3 = B / p of B; cols 4-5 =
  # AME / p of AME. B columns BLANK, AME columns em-dashed.
  cells <- as.character(ref_row[, -1L])
  expect_equal(length(cells), 4L)
  expect_true(trimws(cells[1]) == "")   # B blank
  expect_true(trimws(cells[2]) == "")   # p of B blank
  expect_match(cells[3], "–")            # AME em-dash
  expect_match(cells[4], "–")            # p of AME em-dash
})


test_that("multi-model: ref-row of a factor missing from a model is BLANK in that model's columns", {
  # Pre-fix, an `is_reference = TRUE` row was em-dashed across ALL
  # model columns regardless of whether the factor was in each
  # model. The non-reference rows were correctly blanked when the
  # factor was absent -- producing an asymmetric multi-model
  # display (ref-row em-dashes everywhere, non-ref rows blank when
  # absent). The fix aligns spicy with modelsummary / gtsummary /
  # Stata `esttab`: when a factor is absent from a model, ALL its
  # rows (ref + non-ref) are blank in that model's columns.
  sh <- na.omit(sochealth[, c("smoking", "sex", "physical_activity")])
  m1 <- glm(smoking ~ sex + physical_activity, data = sh,
            family = binomial)
  m2 <- glm(smoking ~ sex, data = sh, family = binomial)
  out <- table_regression(list(M1 = m1, M2 = m2))
  body <- as.data.frame(out, stringsAsFactors = FALSE,
                         check.names = FALSE)
  ref_row <- body[trimws(body$Variable) == "No (ref.)", , drop = FALSE]
  expect_equal(nrow(ref_row), 1L)
  # M1 columns: em-dash (factor present, reference level).
  m1_cells <- as.character(ref_row[grepl("^M1", names(ref_row))])
  expect_true(all(grepl("–", m1_cells)))
  # M2 columns: blank (factor absent from M2). The cells contain
  # only whitespace -- trimws() yields an empty string.
  m2_cells <- as.character(ref_row[grepl("^M2", names(ref_row))])
  expect_true(all(trimws(m2_cells) == ""))
})


test_that("2-level ordered factor + AME: ref row + .L + 1 AME row, in order", {
  skip_if_no_marginaleffects()
  # A binary ordered factor under contr.poly produces exactly one
  # polynomial contrast (.L). The AME block has one contrast row
  # (level 2 vs reference). Verifies the synthetic ref-row + sort
  # logic degrades gracefully on the minimal case.
  set.seed(1)
  df <- data.frame(
    y   = rnorm(200),
    grp = ordered(sample(c("Lo", "Hi"), 200, replace = TRUE),
                  levels = c("Lo", "Hi"))
  )
  out <- table_regression(
    lm(y ~ grp, data = df),
    show_columns = c("b", "p", "ame", "ame_p")
  )
  body <- as.data.frame(out, stringsAsFactors = FALSE)
  vars <- trimws(body$Variable)
  ref_pos  <- which(grepl("^Lo \\(ref\\.\\)$", vars))
  poly_pos <- which(vars == ".L")
  ame_pos  <- which(vars == "Hi")
  expect_length(ref_pos, 1L)
  expect_length(poly_pos, 1L)
  expect_length(ame_pos, 1L)
  # Display order: ref row, then .L, then the AME contrast row.
  expect_lt(ref_pos, poly_pos)
  expect_lt(poly_pos, ame_pos)
})
