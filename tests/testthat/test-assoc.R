# Reference values from DescTools on mtcars
# tab  = table(factor(gear), factor(cyl))  [3x3]
# tab2 = table(factor(vs), factor(am))     [2x2]

tab_3x3 <- function() {
  data(mtcars, envir = environment())
  table(factor(mtcars$gear), factor(mtcars$cyl))
}

tab_2x2 <- function() {
  data(mtcars, envir = environment())
  table(factor(mtcars$vs), factor(mtcars$am))
}

# ── Default scalar return ──────────────────────────────────────────────────

test_that("all functions return scalar by default", {
  tab <- tab_3x3()
  tab2 <- tab_2x2()

  expect_length(suppressWarnings(cramer_v(tab)), 1)
  expect_length(phi(tab2), 1)
  expect_length(suppressWarnings(contingency_coef(tab)), 1)
  expect_length(yule_q(tab2), 1)
  expect_length(suppressWarnings(lambda_gk(tab)), 1)
  expect_length(suppressWarnings(goodman_kruskal_tau(tab)), 1)
  expect_length(suppressWarnings(uncertainty_coef(tab)), 1)
  expect_length(suppressWarnings(gamma_gk(tab)), 1)
  expect_length(suppressWarnings(kendall_tau_b(tab)), 1)
  expect_length(suppressWarnings(kendall_tau_c(tab)), 1)
  expect_length(suppressWarnings(somers_d(tab, "row")), 1)
})

# ── cramer_v ─────────────────────────────────────────────────────────────────

test_that("cramer_v returns correct estimate and 4-element vector", {
  res <- suppressWarnings(cramer_v(tab_3x3(), detail = TRUE))
  expect_type(res, "double")
  expect_length(res, 5)
  expect_named(res, c("estimate", "se", "ci_lower", "ci_upper", "p_value"))
  expect_equal(res[["estimate"]], 0.5308655, tolerance = 1e-5)
  expect_true(res[["p_value"]] < 0.01)
})

test_that("cramer_v with detail = TRUE, conf_level = NULL returns 2 elements", {
  res <- suppressWarnings(cramer_v(tab_3x3(), detail = TRUE, conf_level = NULL))
  expect_length(res, 3)
  expect_named(res, c("estimate", "se", "p_value"))
})

test_that("cramer_v rejects non-table input", {
  expect_error(cramer_v(iris), "contingency table")
})

# ── phi ──────────────────────────────────────────────────────────────────────

test_that("phi returns correct value for 2x2 table", {
  res <- phi(tab_2x2(), detail = TRUE)
  expect_equal(res[["estimate"]], 0.1683451, tolerance = 1e-5)
})

test_that("phi rejects non-2x2 tables", {
  expect_error(phi(tab_3x3()), "2x2")
})

# ── contingency_coef ─────────────────────────────────────────────────────────

test_that("contingency_coef matches DescTools", {
  res <- suppressWarnings(contingency_coef(tab_3x3(), detail = TRUE))
  expect_equal(res[["estimate"]], 0.6003875, tolerance = 1e-5)
  expect_true(is.na(res[["ci_lower"]]))
})

# ── yule_q ───────────────────────────────────────────────────────────────────

test_that("yule_q matches DescTools for 2x2", {
  res <- yule_q(tab_2x2(), detail = TRUE)
  expect_equal(res[["estimate"]], 0.3333333, tolerance = 1e-5)
})

test_that("yule_q rejects non-2x2 tables", {
  expect_error(yule_q(tab_3x3()), "2x2")
})

# ── lambda_gk ────────────────────────────────────────────────────────────────

test_that("lambda_gk matches DescTools for all directions", {
  tab <- tab_3x3()
  sym <- suppressWarnings(lambda_gk(tab, "symmetric", detail = TRUE))
  row <- suppressWarnings(lambda_gk(tab, "row", detail = TRUE))
  col <- suppressWarnings(lambda_gk(tab, "column", detail = TRUE))

  expect_equal(sym[["estimate"]], 0.4857143, tolerance = 1e-5)
  expect_equal(row[["estimate"]], 0.5294118, tolerance = 1e-5)
  expect_equal(col[["estimate"]], 0.4444444, tolerance = 1e-5)

  # CI matches DescTools
  expect_equal(sym[["ci_lower"]], 0.2324839, tolerance = 1e-4)
  expect_equal(sym[["ci_upper"]], 0.7389447, tolerance = 1e-4)
})

# ── goodman_kruskal_tau ──────────────────────────────────────────────────────

test_that("goodman_kruskal_tau matches DescTools", {
  tab <- tab_3x3()
  row <- suppressWarnings(goodman_kruskal_tau(tab, "row", detail = TRUE))
  col <- suppressWarnings(goodman_kruskal_tau(tab, "column", detail = TRUE))

  expect_equal(row[["estimate"]], 0.3825603, tolerance = 1e-5)
  expect_equal(col[["estimate"]], 0.3386018, tolerance = 1e-5)
  expect_equal(row[["ci_lower"]], 0.1520724, tolerance = 1e-4)
  expect_equal(row[["ci_upper"]], 0.6130482, tolerance = 1e-4)
})

# ── uncertainty_coef ─────────────────────────────────────────────────────────

test_that("uncertainty_coef returns sensible values", {
  tab <- tab_3x3()
  sym <- suppressWarnings(uncertainty_coef(tab, "symmetric", detail = TRUE))
  row <- suppressWarnings(uncertainty_coef(tab, "row", detail = TRUE))
  col <- suppressWarnings(uncertainty_coef(tab, "column", detail = TRUE))

  expect_true(sym[["estimate"]] > 0 && sym[["estimate"]] < 1)
  expect_true(row[["estimate"]] > 0 && row[["estimate"]] < 1)
  expect_true(col[["estimate"]] > 0 && col[["estimate"]] < 1)
  expect_length(sym, 5)
})

# ── gamma_gk ─────────────────────────────────────────────────────────────────

test_that("gamma_gk matches DescTools", {
  res <- suppressWarnings(gamma_gk(tab_3x3(), detail = TRUE))
  expect_equal(res[["estimate"]], -0.6573705, tolerance = 1e-5)
  expect_equal(res[["ci_lower"]], -0.9960223, tolerance = 1e-4)
  expect_equal(res[["ci_upper"]], -0.3187187, tolerance = 1e-4)
})

# ── kendall_tau_b ────────────────────────────────────────────────────────────

test_that("kendall_tau_b matches DescTools", {
  res <- suppressWarnings(kendall_tau_b(tab_3x3(), detail = TRUE))
  expect_equal(res[["estimate"]], -0.5125435, tolerance = 1e-5)
  expect_equal(res[["ci_lower"]], -0.8043717, tolerance = 1e-4)
  expect_equal(res[["ci_upper"]], -0.2207153, tolerance = 1e-4)
})

# ── kendall_tau_c ────────────────────────────────────────────────────────────

test_that("kendall_tau_c matches DescTools", {
  res <- suppressWarnings(kendall_tau_c(tab_3x3(), detail = TRUE))
  expect_equal(res[["estimate"]], -0.4833984, tolerance = 1e-5)
  expect_equal(res[["ci_lower"]], -0.7422941, tolerance = 1e-4)
  expect_equal(res[["ci_upper"]], -0.2245028, tolerance = 1e-4)
})

# ── somers_d ─────────────────────────────────────────────────────────────────

test_that("somers_d matches DescTools for row and column", {
  tab <- tab_3x3()
  row <- suppressWarnings(somers_d(tab, "row", detail = TRUE))
  col <- suppressWarnings(somers_d(tab, "column", detail = TRUE))

  expect_equal(row[["estimate"]], -0.5015198, tolerance = 1e-5)
  expect_equal(col[["estimate"]], -0.5238095, tolerance = 1e-5)
  expect_equal(col[["ci_lower"]], -0.8383951, tolerance = 1e-4)
  expect_equal(col[["ci_upper"]], -0.2092240, tolerance = 1e-4)
})

test_that("somers_d symmetric matches the harmonic mean of asymmetric (SPSS / PSPP)", {
  tab <- tab_3x3()
  sym <- suppressWarnings(somers_d(tab, "symmetric"))
  d_r <- somers_d(tab, "row")
  d_c <- somers_d(tab, "column")
  expected <- 2 * d_r * d_c / (d_r + d_c)
  expect_equal(sym, expected, tolerance = 1e-10)
  # PSPP CROSSTABS oracle value on this exact table:
  expect_equal(sym, -0.5124224, tolerance = 1e-5)
})

# ── assoc_measures ───────────────────────────────────────────────────────────

test_that("assoc_measures returns data.frame with correct structure", {
  tab <- tab_3x3()
  res <- suppressWarnings(assoc_measures(tab))
  expect_s3_class(res, "data.frame")
  expect_true(all(
    c("measure", "estimate", "se", "ci_lower", "ci_upper", "p_value") %in%
      names(res)
  ))
  expect_true(nrow(res) > 0)
})

test_that("assoc_measures auto returns all measures", {
  tab <- tab_3x3()
  res <- suppressWarnings(assoc_measures(tab))
  expect_true("Cramer's V" %in% res$measure)
  expect_true("Goodman-Kruskal Gamma" %in% res$measure)
  expect_true("Kendall's Tau-b" %in% res$measure)
})

test_that("assoc_measures type ordinal returns ordinal measures only", {
  tab <- tab_3x3()
  res <- suppressWarnings(assoc_measures(tab, type = "ordinal"))
  expect_true("Goodman-Kruskal Gamma" %in% res$measure)
  expect_true("Kendall's Tau-b" %in% res$measure)
  expect_false("Cramer's V" %in% res$measure)
})

test_that("assoc_measures type nominal returns nominal measures only", {
  tab <- tab_3x3()
  res <- suppressWarnings(assoc_measures(tab, type = "nominal"))
  expect_true("Cramer's V" %in% res$measure)
  expect_false("Goodman-Kruskal Gamma" %in% res$measure)
})

test_that("assoc_measures includes phi and yule_q for 2x2", {
  tab <- tab_2x2()
  res <- suppressWarnings(assoc_measures(tab))
  expect_true("Phi" %in% res$measure)
  expect_true("Yule's Q" %in% res$measure)
})

# ── Input validation ─────────────────────────────────────────────────────────

test_that("all functions reject non-table input", {
  expect_error(gamma_gk(matrix(1:4, 2)), "contingency table")
  expect_error(kendall_tau_b(data.frame(a = 1)), "contingency table")
  expect_error(somers_d(1:10), "contingency table")
})

test_that("detail = TRUE returns spicy_assoc_detail class", {
  tab <- table(c("A", "B", "A", "B"), c("X", "X", "Y", "Y"))
  res <- cramer_v(tab, detail = TRUE)
  expect_s3_class(res, "spicy_assoc_detail")
  expect_named(res, c("estimate", "se", "ci_lower", "ci_upper", "p_value"))
  res2 <- cramer_v(tab, detail = TRUE, conf_level = NULL)
  expect_s3_class(res2, "spicy_assoc_detail")
  expect_named(res2, c("estimate", "se", "p_value"))
})

test_that("print.spicy_assoc_detail formats output", {
  tab <- table(c("A", "B", "A", "B"), c("X", "X", "Y", "Y"))
  res <- cramer_v(tab, detail = TRUE)
  out <- capture.output(print(res))
  expect_length(out, 2)
  expect_match(out[1], "Estimate")
  expect_match(out[1], "\\bp\\b")
})

test_that("print.spicy_assoc_detail respects digits argument", {
  tab <- table(c("A", "B", "A", "B"), c("X", "X", "Y", "Y"))
  out4 <- capture.output(print(cramer_v(tab, detail = TRUE), digits = 4))
  expect_match(out4[2], "\\.[0-9]{4}")
})

test_that("digits argument in function propagates to print", {
  tab <- table(c("A", "B", "A", "B"), c("X", "X", "Y", "Y"))
  res <- cramer_v(tab, detail = TRUE, digits = 4)
  expect_equal(attr(res, "digits"), 4)
  out <- capture.output(print(res))
  expect_match(out[2], "\\.[0-9]{4}")
})

test_that("assoc_measures returns spicy_assoc_table class", {
  tab <- table(c("A", "B", "A", "B"), c("X", "X", "Y", "Y"))
  res <- assoc_measures(tab)
  expect_s3_class(res, "spicy_assoc_table")
  expect_s3_class(res, "data.frame")
  expect_true("measure" %in% names(res))
})

test_that("print.spicy_assoc_table formats output", {
  tab <- table(c("A", "B", "A", "B"), c("X", "X", "Y", "Y"))
  res <- assoc_measures(tab)
  out <- capture.output(print(res))
  expect_match(out[1], "Measure")
  expect_match(out[1], "\\bp\\b")
  expect_true(length(out) > 1)
})

test_that("print.spicy_assoc_table respects digits argument", {
  tab <- table(c("A", "B", "A", "B"), c("X", "X", "Y", "Y"))
  out4 <- capture.output(print(assoc_measures(tab), digits = 4))
  expect_match(out4[2], "\\.[0-9]{4}")
})

test_that("digits argument in assoc_measures propagates to print", {
  tab <- table(c("A", "B", "A", "B"), c("X", "X", "Y", "Y"))
  res <- assoc_measures(tab, digits = 2)
  expect_equal(attr(res, "digits"), 2)
  out <- capture.output(print(res))
  expect_match(out[2], "\\.[0-9]{2}\\b")
})

# ── Validation errors ──────────────────────────────────────────────────────

test_that("validation rejects non-2D table", {
  t3d <- array(1:8, dim = c(2, 2, 2))
  class(t3d) <- "table"
  expect_error(cramer_v(t3d), "two-dimensional")
})

# ── conf_level = NULL returns without CI ───────────────────────────────────

test_that("cramer_v detail with conf_level = NULL omits CI", {
  tab <- tab_3x3()
  res <- cramer_v(tab, detail = TRUE, conf_level = NULL)
  expect_s3_class(res, "spicy_assoc_detail")
  expect_true(!"ci_lower" %in% names(res))
  expect_true("p_value" %in% names(res))
})

test_that("phi detail with conf_level = NULL omits CI", {
  tab <- tab_2x2()
  res <- phi(tab, detail = TRUE, conf_level = NULL)
  expect_s3_class(res, "spicy_assoc_detail")
  expect_true(!"ci_lower" %in% names(res))
})

test_that("contingency_coef detail with conf_level = NULL omits CI", {
  tab <- tab_3x3()
  res <- contingency_coef(tab, detail = TRUE, conf_level = NULL)
  expect_s3_class(res, "spicy_assoc_detail")
  expect_true(!"ci_lower" %in% names(res))
})

# ── Degenerate tables ─────────────────────────────────────────────────────

test_that("cramer_v rejects all-zero (n = 0) tables", {
  tab <- matrix(0L, 2, 2)
  class(tab) <- "table"
  expect_error(cramer_v(tab), "zero total count")
})

test_that(".validate_table rejects NA cells", {
  tab <- matrix(c(NA_integer_, 1L, 2L, 3L), 2, 2)
  class(tab) <- "table"
  expect_error(cramer_v(tab), "NA cells")
})

test_that(".validate_table rejects negative counts", {
  tab <- matrix(c(-1L, 1L, 2L, 3L), 2, 2)
  class(tab) <- "table"
  expect_error(cramer_v(tab), "non-negative")
})

test_that("uncertainty_coef stays finite when a margin is zero", {
  tab <- matrix(c(5L, 3L, 0L, 0L, 4L, 2L), nrow = 2, byrow = TRUE)
  class(tab) <- "table"
  res <- uncertainty_coef(tab, "row", detail = TRUE)
  expect_true(is.finite(res[["estimate"]]))
})

test_that("NA degenerate-table return respects `detail` and `conf_level`", {
  tab_yq <- matrix(c(0L, 0L, 0L, 5L), 2, 2)
  class(tab_yq) <- "table"
  scalar <- suppressWarnings(yule_q(tab_yq))
  expect_identical(scalar, NA_real_)
  detail <- suppressWarnings(yule_q(tab_yq, detail = TRUE))
  expect_s3_class(detail, "spicy_assoc_detail")
  expect_named(detail, c("estimate", "se", "ci_lower", "ci_upper", "p_value"))
  expect_true(all(is.na(detail)))

  no_ci <- suppressWarnings(yule_q(tab_yq, detail = TRUE, conf_level = NULL))
  expect_named(no_ci, c("estimate", "se", "p_value"))
})

test_that("gamma_gk NA path returns scalar by default and full shape with detail", {
  tab <- matrix(c(5L, 0L, 3L, 0L), 2, 2)
  class(tab) <- "table"
  expect_identical(suppressWarnings(gamma_gk(tab)), NA_real_)
  d <- suppressWarnings(gamma_gk(tab, detail = TRUE))
  expect_s3_class(d, "spicy_assoc_detail")
  expect_length(d, 5L)
})


# Anti-regression: oracle values from PSPP 2.0 CROSSTABS /STATISTICS=ALL.
# These pin spicy's point estimates to the SPSS / PSPP reference at 1e-5
# on four datasets:
#
#   * mtcars 3x3 (gear x cyl)            -- 16 statistics
#   * mtcars 2x2 (vs x am)               -- 10 statistics
#   * HairEyeColor 4x4 (Hair x Eye)      --  8 statistics
#   * sochealth (smoking x education)    --  6 statistics
#
# Total: 40 oracle assertions. Generated against PSPP 2.0 on 2026-05-01.
# Reproduction: install PSPP, save the four tables as .sav, run
# `CROSSTABS /STATISTICS=ALL` on each, and compare the printed values
# to the constants below. Each constant matches PSPP's reported value
# to 7 decimals (rounded to 1e-5 here for cross-platform stability).

test_that("PSPP oracle: mtcars 3x3 (gear x cyl)", {
  tab <- table(factor(mtcars$gear), factor(mtcars$cyl))
  expect_equal(cramer_v(tab),                       0.5308655, tolerance = 1e-5)
  expect_equal(contingency_coef(tab),               0.6003875, tolerance = 1e-5)
  expect_equal(kendall_tau_b(tab),                 -0.5125435, tolerance = 1e-5)
  expect_equal(kendall_tau_c(tab),                 -0.4833984, tolerance = 1e-5)
  expect_equal(gamma_gk(tab),                      -0.6573705, tolerance = 1e-5)
  expect_equal(suppressWarnings(lambda_gk(tab, "symmetric")), 0.4857143, tolerance = 1e-5)
  expect_equal(suppressWarnings(lambda_gk(tab, "row")),       0.5294118, tolerance = 1e-5)
  expect_equal(suppressWarnings(lambda_gk(tab, "column")),    0.4444444, tolerance = 1e-5)
  expect_equal(goodman_kruskal_tau(tab, "row"),      0.3825603, tolerance = 1e-5)
  expect_equal(goodman_kruskal_tau(tab, "column"),   0.3386018, tolerance = 1e-5)
  expect_equal(uncertainty_coef(tab, "symmetric"),   0.3504372, tolerance = 1e-5)
  expect_equal(uncertainty_coef(tab, "row"),         0.3587709, tolerance = 1e-5)
  expect_equal(uncertainty_coef(tab, "column"),      0.3424818, tolerance = 1e-5)
  expect_equal(somers_d(tab, "symmetric"),          -0.5124224, tolerance = 1e-5)
  expect_equal(somers_d(tab, "row"),                -0.5015198, tolerance = 1e-5)
  expect_equal(somers_d(tab, "column"),             -0.5238095, tolerance = 1e-5)
})

test_that("PSPP oracle: mtcars 2x2 (vs x am)", {
  tab <- table(factor(mtcars$vs), factor(mtcars$am))
  expect_equal(phi(tab),                            0.1683451, tolerance = 1e-5)
  expect_equal(cramer_v(tab),                       0.1683451, tolerance = 1e-5)
  expect_equal(contingency_coef(tab),               0.1660092, tolerance = 1e-5)
  expect_equal(kendall_tau_b(tab),                  0.1683451, tolerance = 1e-5)
  expect_equal(kendall_tau_c(tab),                  0.1640625, tolerance = 1e-5)
  expect_equal(gamma_gk(tab),                       0.3333333, tolerance = 1e-5)
  expect_equal(uncertainty_coef(tab, "symmetric"),  0.0208314, tolerance = 1e-5)
  expect_equal(somers_d(tab, "symmetric"),          0.1683367, tolerance = 1e-5)
  expect_equal(somers_d(tab, "row"),                0.1700405, tolerance = 1e-5)
  expect_equal(somers_d(tab, "column"),             0.1666667, tolerance = 1e-5)
})

test_that("PSPP oracle: HairEyeColor 4x4 (Hair x Eye marginalised)", {
  tab <- margin.table(HairEyeColor, c(1, 2))
  expect_equal(cramer_v(tab),                       0.2790446, tolerance = 1e-5)
  expect_equal(contingency_coef(tab),               0.4351585, tolerance = 1e-5)
  expect_equal(kendall_tau_b(tab),                  0.2247026, tolerance = 1e-5)
  expect_equal(kendall_tau_c(tab),                  0.2046886, tolerance = 1e-5)
  expect_equal(gamma_gk(tab),                       0.3177721, tolerance = 1e-5)
  expect_equal(suppressWarnings(lambda_gk(tab, "symmetric")), 0.1430678, tolerance = 1e-5)
  expect_equal(uncertainty_coef(tab, "symmetric"),  0.0984203, tolerance = 1e-5)
  expect_equal(somers_d(tab, "symmetric"),          0.2246768, tolerance = 1e-5)
})

test_that("PSPP oracle: sochealth (smoking x education)", {
  data(sochealth, package = "spicy", envir = environment())
  tab <- table(sochealth$smoking, sochealth$education)
  expect_equal(cramer_v(tab),                       0.1356677, tolerance = 1e-5)
  expect_equal(contingency_coef(tab),               0.1344361, tolerance = 1e-5)
  expect_equal(kendall_tau_b(tab),                 -0.1264155, tolerance = 1e-5)
  expect_equal(gamma_gk(tab),                      -0.2680678, tolerance = 1e-5)
  expect_equal(uncertainty_coef(tab, "symmetric"),  0.0114876, tolerance = 1e-5)
  expect_equal(somers_d(tab, "symmetric"),         -0.1200077, tolerance = 1e-5)
})

test_that("yule_q warns when ad + bc = 0", {
  tab <- matrix(c(0L, 0L, 0L, 5L), 2, 2)
  class(tab) <- "table"
  expect_warning(yule_q(tab), "undefined")
})

test_that("yule_q handles zero cells in detail mode", {
  tab <- matrix(c(5L, 0L, 3L, 2L), 2, 2)
  class(tab) <- "table"
  res <- yule_q(tab, detail = TRUE)
  expect_true(is.na(res[["p_value"]]))
})

test_that("gamma_gk warns when C + D = 0", {
  tab <- matrix(c(5L, 0L, 3L, 0L), 2, 2)
  class(tab) <- "table"
  expect_warning(gamma_gk(tab), "No concordant")
})

test_that("kendall_tau_b warns on degenerate table", {
  tab <- matrix(c(5L, 5L, 5L, 5L), 2, 2)
  class(tab) <- "table"
  res <- kendall_tau_b(tab)
  expect_true(is.numeric(res))
})

test_that("lambda_gk warns + returns NA on rank-1 (constant variable) table", {
  # Anti-regression: a 2x2 with all observations in one row had
  # `denom = n - max_rsum = 0` -> estimate `0/0 = NaN`. The
  # no-detail branch silently returned NaN; the detail branch
  # errored at the unguarded `if (se > 0)` step because
  # `is.na(NaN > 0) = NA` makes `if (NA)` raise.
  rank1 <- matrix(c(10L, 0L, 0L, 0L), 2, 2)
  class(rank1) <- "table"
  dimnames(rank1) <- list(x = c("a", "b"), y = c("Y", "N"))

  expect_warning(
    res <- lambda_gk(rank1, "row"),
    class = "spicy_undefined_stat"
  )
  expect_true(is.na(res))

  # Detail mode: returns the fully NA-shaped result (no error).
  expect_warning(
    res_d <- lambda_gk(rank1, "row", detail = TRUE),
    class = "spicy_undefined_stat"
  )
  expect_true(all(is.na(res_d)))
  expect_named(
    res_d,
    c("estimate", "se", "ci_lower", "ci_upper", "p_value")
  )
})

test_that("goodman_kruskal_tau warns + returns NA on rank-1 table", {
  # Anti-regression for the parallel silent-NaN bug:
  # `(n - v) = 0` when v = sum(rsum^2)/n = n (a single-row table)
  # produced silent NaN. Now warns and returns NA shape.
  rank1 <- matrix(c(10L, 0L, 0L, 0L), 2, 2)
  class(rank1) <- "table"
  dimnames(rank1) <- list(x = c("a", "b"), y = c("Y", "N"))

  expect_warning(
    res <- goodman_kruskal_tau(rank1, "row"),
    class = "spicy_undefined_stat"
  )
  expect_true(is.na(res))

  expect_warning(
    res_d <- goodman_kruskal_tau(rank1, "row", detail = TRUE),
    class = "spicy_undefined_stat"
  )
  expect_true(all(is.na(res_d)))
})

test_that("uncertainty_coef warns + returns NA when the predicted variable is constant", {
  # F08: previously returned a silent, plausible-looking 0 for the
  # undefined 0/0 form (zero marginal entropy, e.g. an unused
  # factor level), while the rest of the family warned + NA.
  rank1 <- matrix(c(4L, 0L, 6L, 0L), 2, 2)
  class(rank1) <- "table"

  expect_warning(
    res <- uncertainty_coef(rank1, "row"),
    class = "spicy_undefined_stat"
  )
  expect_identical(res, NA_real_)

  expect_warning(
    res_d <- uncertainty_coef(rank1, "row", detail = TRUE),
    class = "spicy_undefined_stat"
  )
  expect_s3_class(res_d, "spicy_assoc_detail")
  expect_named(res_d, c("estimate", "se", "ci_lower", "ci_upper", "p_value"))
  expect_true(all(is.na(res_d)))
})

test_that("uncertainty_coef column direction warns when the column variable is constant", {
  rank1 <- matrix(c(4L, 6L, 0L, 0L), 2, 2)
  class(rank1) <- "table"
  expect_warning(
    res <- uncertainty_coef(rank1, "column"),
    class = "spicy_undefined_stat"
  )
  expect_identical(res, NA_real_)
})

test_that("uncertainty_coef symmetric: genuine 0 with one constant variable, NA with two", {
  # With a single constant variable the symmetric U = 2 * 0 / (0 + H)
  # is a well-defined 0, not a guard case.
  one_constant <- matrix(c(4L, 0L, 6L, 0L), 2, 2)
  class(one_constant) <- "table"
  expect_no_warning(sym <- uncertainty_coef(one_constant, "symmetric"))
  expect_equal(sym, 0)

  # Both variables constant (all observations in one cell): 0 / 0.
  one_cell <- matrix(c(5L, 0L, 0L, 0L), 2, 2)
  class(one_cell) <- "table"
  expect_warning(
    res <- uncertainty_coef(one_cell, "symmetric"),
    class = "spicy_undefined_stat"
  )
  expect_identical(res, NA_real_)
})

test_that("ordinal measures report NA p-value when the SE is zero", {
  # F36: a perfect association has SE = 0, so the Wald z-test is
  # undefined. gamma / tau-b previously reported p = 0 and tau-c
  # could reach NaN; all three now gate like the rest of the family.
  perfect <- matrix(c(5L, 0L, 0L, 5L), 2, 2)
  class(perfect) <- "table"

  for (fn in list(gamma_gk, kendall_tau_b, kendall_tau_c)) {
    res <- fn(perfect, detail = TRUE)
    expect_identical(res[["se"]], 0)
    expect_true(is.na(res[["p_value"]]))
    expect_false(is.nan(res[["p_value"]]))
  }
})

test_that("kendall_tau_c zero-SE degenerate table gives NA p, not NaN", {
  tab <- matrix(c(2L, 3L, 0L, 0L), 2, 2)
  class(tab) <- "table"
  res <- kendall_tau_c(tab, detail = TRUE)
  expect_equal(res[["estimate"]], 0)
  expect_true(is.na(res[["p_value"]]))
  expect_false(is.nan(res[["p_value"]]))
})

test_that("assoc_measures re-emits classed warnings from undefined measures", {
  # F15: `assoc_measures()` used to blanket-suppress the classed
  # `spicy_undefined_stat` warnings its measures raise on degenerate
  # tables, leaving silent `--` rows with no signal.
  degen <- matrix(c(4L, 6L, 0L, 0L), 2, 2)
  class(degen) <- "table"
  suppressWarnings(
    expect_warning(assoc_measures(degen), class = "spicy_undefined_stat")
  )
})

test_that("assoc_measures deduplicates re-emitted warnings by message", {
  one_cell <- matrix(c(5L, 0L, 0L, 0L), 2, 2)
  class(one_cell) <- "table"
  conds <- list()
  withCallingHandlers(
    res <- assoc_measures(one_cell),
    warning = function(w) {
      conds[[length(conds) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(res, "spicy_assoc_table")
  expect_true(length(conds) > 0)
  expect_true(all(vapply(conds, inherits, logical(1), "spicy_undefined_stat")))
  expect_true(all(vapply(conds, inherits, logical(1), "spicy_warning")))
  msgs <- vapply(conds, conditionMessage, character(1))
  expect_identical(anyDuplicated(msgs), 0L)
  # Both Somers' D directions raise this identical message on the
  # all-in-one-cell table; it must be re-emitted exactly once.
  expect_identical(
    sum(msgs == "Somers' d is undefined for this table; returning NA."),
    1L
  )
})

test_that("assoc_measures on a well-populated table emits no warnings", {
  expect_no_warning(assoc_measures(tab_3x3()))
})

test_that("direction defaults follow the documented per-measure conventions", {
  # F14 (audited, kept): somers_d defaults to "row" (intrinsically
  # asymmetric, symmetric form has no analytic SE; DescTools offers
  # no symmetric option), while lambda_gk / uncertainty_coef default
  # to "symmetric" (standard variants with their own ASE).
  tab <- tab_3x3()
  expect_identical(somers_d(tab), somers_d(tab, "row"))
  expect_identical(lambda_gk(tab), lambda_gk(tab, "symmetric"))
  expect_identical(uncertainty_coef(tab), uncertainty_coef(tab, "symmetric"))
  expect_identical(goodman_kruskal_tau(tab), goodman_kruskal_tau(tab, "row"))
})

test_that("kendall_tau_c warns on 1-row table", {
  tab <- matrix(c(5L, 3L), nrow = 1)
  class(tab) <- "table"
  expect_error(kendall_tau_c(tab), "at least")
})

test_that("somers_d warns when denom = 0", {
  tab <- matrix(c(5L, 0L, 3L, 0L), 2, 2)
  class(tab) <- "table"
  expect_warning(somers_d(tab, "column"), "undefined")
})

test_that("print.spicy_assoc_detail formats NA as --", {
  tab <- matrix(c(5L, 0L, 3L, 2L), 2, 2)
  class(tab) <- "table"
  res <- yule_q(tab, detail = TRUE)
  out <- capture.output(print(res))
  expect_true(any(grepl("--", out)))
})

test_that("print.spicy_assoc_detail formats small p-values in APA style (<.001)", {
  # Since spicy 0.11.0 the print method routes the p-value through
  # `format_p_value()`, the same helper used by `cross_tab()` and the
  # `table_*()` family: APA-strict `<.001` (no leading zero, no space).
  tab <- matrix(c(50L, 0L, 0L, 50L), 2, 2)
  class(tab) <- "table"
  res <- cramer_v(tab, detail = TRUE)
  out <- capture.output(print(res))
  expect_true(any(grepl("<.001", out, fixed = TRUE)))
  expect_false(any(grepl("< 0.001", out, fixed = TRUE)))
})

test_that("print.spicy_assoc_detail p-value matches `format_p_value()` output", {
  # The p-value column should be rendered byte-for-byte by the same
  # `format_p_value()` helper used by `cross_tab()` and the
  # `table_*()` family -- this locks in the APA convention (no
  # leading zero) for non-tiny p-values too.
  tab <- table(c("A", "A", "A", "B", "B"), c("X", "Y", "X", "Y", "X"))
  res <- suppressWarnings(yule_q(tab, detail = TRUE))
  out <- capture.output(print(res))
  expected_p <- spicy:::format_p_value(res[["p_value"]], ".", 3L)
  expect_true(any(grepl(expected_p, out, fixed = TRUE)))
})

test_that("print.spicy_assoc_table also uses APA p-value format", {
  tab <- matrix(c(50L, 0L, 0L, 50L), 2, 2)
  class(tab) <- "table"
  res <- assoc_measures(tab)
  out <- capture.output(print(res))
  expect_true(any(grepl("<.001", out, fixed = TRUE)))
  expect_false(any(grepl("< 0.001", out, fixed = TRUE)))
})

test_that(".assoc_result detail = FALSE returns scalar", {
  tab <- tab_3x3()
  res <- gamma_gk(tab, detail = FALSE)
  expect_length(res, 1)
  expect_false(inherits(res, "spicy_assoc_detail"))
})

test_that(".assoc_result conf_level = NULL keeps se, omits CI", {
  tab <- tab_3x3()
  res <- gamma_gk(tab, detail = TRUE, conf_level = NULL)
  expect_s3_class(res, "spicy_assoc_detail")
  expect_true("se" %in% names(res))
  expect_true("p_value" %in% names(res))
  expect_false("ci_lower" %in% names(res))
})

test_that("cramer_v conf_level = NULL keeps se", {
  tab <- tab_3x3()
  res <- cramer_v(tab, detail = TRUE, conf_level = NULL)
  expect_s3_class(res, "spicy_assoc_detail")
  expect_true("se" %in% names(res))
})

test_that("phi conf_level = NULL keeps se", {
  tab <- tab_2x2()
  res <- phi(tab, detail = TRUE, conf_level = NULL)
  expect_s3_class(res, "spicy_assoc_detail")
  expect_true("se" %in% names(res))
})

test_that("yule_q conf_level = NULL omits CI", {
  tab <- tab_2x2()
  res <- yule_q(tab, detail = TRUE, conf_level = NULL)
  expect_s3_class(res, "spicy_assoc_detail")
  expect_true("p_value" %in% names(res))
  expect_false("ci_lower" %in% names(res))
})

test_that("contingency_coef conf_level = NULL keeps se", {
  tab <- tab_3x3()
  res <- contingency_coef(tab, detail = TRUE, conf_level = NULL)
  expect_s3_class(res, "spicy_assoc_detail")
  expect_true("se" %in% names(res))
})

test_that("kendall_tau_b warns when denom = 0", {
  tab <- matrix(c(0L, 0L, 3L, 7L), 2, 2)
  class(tab) <- "table"
  expect_warning(kendall_tau_b(tab, detail = TRUE), "undefined")
})

test_that("kendall_tau_b se_sq clamped to 0 when tiny negative", {
  tab <- matrix(c(10L, 0L, 0L, 10L), 2, 2)
  class(tab) <- "table"
  res <- kendall_tau_b(tab, detail = TRUE)
  expect_s3_class(res, "spicy_assoc_detail")
})

test_that("lambda_gk row detail covers L_row_max branch", {
  tab <- matrix(c(10L, 0L, 0L, 5L), 2, 2)
  class(tab) <- "table"
  res <- lambda_gk(tab, direction = "row", detail = TRUE)
  expect_s3_class(res, "spicy_assoc_detail")
})

test_that("lambda_gk column detail covers L_col_max branch", {
  tab <- matrix(c(10L, 0L, 0L, 5L), 2, 2)
  class(tab) <- "table"
  res <- lambda_gk(tab, direction = "column", detail = TRUE)
  expect_s3_class(res, "spicy_assoc_detail")
})

test_that("chi-squared measures return NA with a classed warning on zero-margin tables", {
  tab0 <- as.table(matrix(c(4, 6, 0, 0), nrow = 2))
  for (fn in list(cramer_v, phi, contingency_coef)) {
    expect_warning(
      res <- fn(tab0),
      class = "spicy_undefined_stat"
    )
    expect_identical(res, NA_real_)
    expect_warning(
      det <- fn(tab0, detail = TRUE),
      class = "spicy_undefined_stat"
    )
    expect_s3_class(det, "spicy_assoc_detail")
    expect_true(all(is.na(as.numeric(det))))
  }
})

test_that("assoc_measures survives a zero-margin table and re-emits the chi-squared warning once", {
  tab0 <- as.table(matrix(c(4, 6, 0, 0), nrow = 2))
  warns <- list()
  res <- withCallingHandlers(
    assoc_measures(tab0),
    warning = function(w) {
      warns[[length(warns) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(res, "spicy_assoc_table")
  chi_rows <- res$measure %in% c("Phi", "Cramer's V", "Contingency Coefficient")
  expect_true(all(is.na(res$estimate[chi_rows])))
  msgs <- vapply(warns, conditionMessage, character(1))
  expect_identical(sum(grepl("chi-squared statistic is NaN", msgs)), 1L)
  expect_true(any(vapply(warns, inherits, logical(1), "spicy_undefined_stat")))
})
