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
  expect_length(res, 4)
  expect_named(res, c("estimate", "ci_lower", "ci_upper", "p_value"))
  expect_equal(res[["estimate"]], 0.5308655, tolerance = 1e-5)
  expect_true(res[["p_value"]] < 0.01)
})

test_that("cramer_v with detail = TRUE, conf_level = NULL returns 2 elements", {
  res <- suppressWarnings(cramer_v(tab_3x3(), detail = TRUE, conf_level = NULL))
  expect_length(res, 2)
  expect_named(res, c("estimate", "p_value"))
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
  expect_length(sym, 4)
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

test_that("somers_d symmetric returns tau-b", {
  tab <- tab_3x3()
  sym <- suppressWarnings(somers_d(tab, "symmetric", detail = TRUE))
  tau <- suppressWarnings(kendall_tau_b(tab, detail = TRUE))
  expect_equal(sym[["estimate"]], tau[["estimate"]], tolerance = 1e-10)
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
  expect_named(res, c("estimate", "ci_lower", "ci_upper", "p_value"))
  res2 <- cramer_v(tab, detail = TRUE, conf_level = NULL)
  expect_s3_class(res2, "spicy_assoc_detail")
  expect_named(res2, c("estimate", "p_value"))
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
  res <- cramer_v(tab, detail = TRUE)
  out4 <- capture.output(print(res, digits = 4))
  expect_match(out4[2], "\\.[0-9]{4}")
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
  res <- assoc_measures(tab)
  out4 <- capture.output(print(res, digits = 4))
  expect_match(out4[2], "\\.[0-9]{4}")
})
