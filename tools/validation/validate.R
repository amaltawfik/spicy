## Cross-validation of spicy::* association measures
## vs DescTools, vcd, Hmisc, PSPP, and textbook reference values.
##
## Usage: Rscript tools/validation/validate.R
## Output: tools/validation/spicy_values.csv, *.csv per package, log

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
  library(DescTools)
  library(vcd)
  library(Hmisc)
})

set.seed(1)
out_dir <- "tools/validation"
dir.create(out_dir, showWarnings = FALSE)

# ── Datasets ────────────────────────────────────────────────────────────────

data(mtcars)
data(HairEyeColor)
data(sochealth, package = "spicy")

datasets <- list(
  mtcars_3x3 = table(factor(mtcars$gear), factor(mtcars$cyl)),
  mtcars_2x2 = table(factor(mtcars$vs), factor(mtcars$am)),
  HairEye_4x4 = margin.table(HairEyeColor, c(1, 2)),
  sochealth = table(sochealth$smoking, sochealth$education)
)

# Save each as plain matrix CSV for PSPP cell-by-cell ingest
for (nm in names(datasets)) {
  tab <- datasets[[nm]]
  df <- as.data.frame.table(tab, responseName = "freq")
  colnames(df)[1:2] <- c("row", "col")
  write.csv(df, file.path(out_dir, sprintf("data_%s.csv", nm)), row.names = FALSE)
}

# ── Compute spicy values ─────────────────────────────────────────────────────

spicy_row <- function(estimate, ci_lower = NA_real_, ci_upper = NA_real_,
                      p = NA_real_) {
  list(estimate = estimate, ci_lower = ci_lower, ci_upper = ci_upper, p = p)
}

extract <- function(res) {
  if (is.numeric(res) && is.null(names(res))) {
    return(spicy_row(unname(res)))
  }
  spicy_row(
    estimate = unname(res[["estimate"]]),
    ci_lower = if ("ci_lower" %in% names(res)) unname(res[["ci_lower"]]) else NA_real_,
    ci_upper = if ("ci_upper" %in% names(res)) unname(res[["ci_upper"]]) else NA_real_,
    p = if ("p_value" %in% names(res)) unname(res[["p_value"]]) else NA_real_
  )
}

is_2x2 <- function(t) nrow(t) == 2L && ncol(t) == 2L

compute_spicy <- function(tab) {
  out <- list()
  out[["Cramer V"]] <- extract(suppressWarnings(cramer_v(tab, detail = TRUE)))
  if (is_2x2(tab)) {
    out[["Phi"]] <- extract(suppressWarnings(phi(tab, detail = TRUE)))
    out[["Yule Q"]] <- extract(suppressWarnings(yule_q(tab, detail = TRUE)))
  }
  out[["Contingency C"]] <- extract(suppressWarnings(contingency_coef(tab, detail = TRUE)))
  out[["Lambda sym"]] <- extract(suppressWarnings(lambda_gk(tab, "symmetric", detail = TRUE)))
  out[["Lambda R|C"]] <- extract(suppressWarnings(lambda_gk(tab, "row", detail = TRUE)))
  out[["Lambda C|R"]] <- extract(suppressWarnings(lambda_gk(tab, "column", detail = TRUE)))
  out[["GK Tau R|C"]] <- extract(suppressWarnings(goodman_kruskal_tau(tab, "row", detail = TRUE)))
  out[["GK Tau C|R"]] <- extract(suppressWarnings(goodman_kruskal_tau(tab, "column", detail = TRUE)))
  out[["U sym"]] <- extract(suppressWarnings(uncertainty_coef(tab, "symmetric", detail = TRUE)))
  out[["U R|C"]] <- extract(suppressWarnings(uncertainty_coef(tab, "row", detail = TRUE)))
  out[["U C|R"]] <- extract(suppressWarnings(uncertainty_coef(tab, "column", detail = TRUE)))
  out[["Gamma"]] <- extract(suppressWarnings(gamma_gk(tab, detail = TRUE)))
  out[["Tau-b"]] <- extract(suppressWarnings(kendall_tau_b(tab, detail = TRUE)))
  out[["Tau-c"]] <- extract(suppressWarnings(kendall_tau_c(tab, detail = TRUE)))
  out[["Somers D R|C"]] <- extract(suppressWarnings(somers_d(tab, "row", detail = TRUE)))
  out[["Somers D C|R"]] <- extract(suppressWarnings(somers_d(tab, "column", detail = TRUE)))
  out
}

spicy_values <- lapply(datasets, compute_spicy)

# ── Compute DescTools equivalents ────────────────────────────────────────────

compute_desctools <- function(tab) {
  out <- list()
  out[["Cramer V"]] <- list(estimate = unname(DescTools::CramerV(tab)),
                            ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  if (is_2x2(tab)) {
    out[["Phi"]] <- list(estimate = unname(DescTools::Phi(tab)),
                         ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
    out[["Yule Q"]] <- list(estimate = unname(DescTools::YuleQ(tab)),
                            ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  }
  out[["Contingency C"]] <- list(estimate = unname(DescTools::ContCoef(tab)),
                                  ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out[["Lambda sym"]] <- list(estimate = unname(DescTools::Lambda(tab, direction = "symmetric")),
                              ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out[["Lambda R|C"]] <- list(estimate = unname(DescTools::Lambda(tab, direction = "row")),
                              ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out[["Lambda C|R"]] <- list(estimate = unname(DescTools::Lambda(tab, direction = "column")),
                              ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out[["GK Tau R|C"]] <- list(estimate = unname(DescTools::GoodmanKruskalTau(tab, direction = "row")),
                              ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out[["GK Tau C|R"]] <- list(estimate = unname(DescTools::GoodmanKruskalTau(tab, direction = "column")),
                              ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out[["U sym"]] <- list(estimate = unname(DescTools::UncertCoef(tab, direction = "symmetric")),
                         ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out[["U R|C"]] <- list(estimate = unname(DescTools::UncertCoef(tab, direction = "row")),
                         ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out[["U C|R"]] <- list(estimate = unname(DescTools::UncertCoef(tab, direction = "column")),
                         ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out[["Gamma"]] <- list(estimate = unname(DescTools::GoodmanKruskalGamma(tab)),
                         ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out[["Tau-b"]] <- list(estimate = unname(DescTools::KendallTauB(tab)),
                         ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out[["Tau-c"]] <- list(estimate = unname(DescTools::StuartTauC(tab)),
                         ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out[["Somers D R|C"]] <- list(estimate = unname(DescTools::SomersDelta(tab, direction = "row")),
                                ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out[["Somers D C|R"]] <- list(estimate = unname(DescTools::SomersDelta(tab, direction = "column")),
                                ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out
}

desc_values <- lapply(datasets, compute_desctools)

# ── Compute vcd::assocstats (nominal: Cramer V, Phi, Contingency C) ─────────

compute_vcd <- function(tab) {
  s <- vcd::assocstats(tab)
  out <- list()
  out[["Cramer V"]] <- list(estimate = s$cramer, ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  if (is_2x2(tab)) {
    out[["Phi"]] <- list(estimate = abs(s$phi), ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  }
  out[["Contingency C"]] <- list(estimate = s$contingency, ci_lower = NA_real_, ci_upper = NA_real_, p = NA_real_)
  out
}

vcd_values <- lapply(datasets, compute_vcd)

# ── Save spicy + Desc + vcd as flat csv ──────────────────────────────────────

flatten_one <- function(values, dataset_name, source_name) {
  do.call(rbind, lapply(names(values), function(meas) {
    v <- values[[meas]]
    data.frame(
      dataset = dataset_name,
      measure = meas,
      source = source_name,
      estimate = v$estimate,
      ci_lower = v$ci_lower,
      ci_upper = v$ci_upper,
      p = v$p,
      stringsAsFactors = FALSE
    )
  }))
}

flat <- do.call(rbind, c(
  lapply(names(datasets), function(nm) flatten_one(spicy_values[[nm]], nm, "spicy")),
  lapply(names(datasets), function(nm) flatten_one(desc_values[[nm]], nm, "DescTools")),
  lapply(names(datasets), function(nm) flatten_one(vcd_values[[nm]], nm, "vcd"))
))

write.csv(flat, file.path(out_dir, "values_long.csv"), row.names = FALSE)

# ── Convergence summary: spicy vs DescTools point estimate ───────────────────

cmp <- merge(
  flat[flat$source == "spicy", c("dataset", "measure", "estimate")],
  flat[flat$source == "DescTools", c("dataset", "measure", "estimate")],
  by = c("dataset", "measure"), suffixes = c("_spicy", "_desc")
)
cmp$diff <- abs(cmp$estimate_spicy - cmp$estimate_desc)
cmp$ok_1e4 <- !is.na(cmp$diff) & cmp$diff < 1e-4
cmp$ok_1e5 <- !is.na(cmp$diff) & cmp$diff < 1e-5

cat("\n── spicy vs DescTools (point estimate) ──\n")
print(cmp[order(-cmp$diff), ], row.names = FALSE)
cat(sprintf("\nAgreement at 1e-4: %d / %d\n", sum(cmp$ok_1e4), nrow(cmp)))
cat(sprintf("Agreement at 1e-5: %d / %d\n", sum(cmp$ok_1e5), nrow(cmp)))

# ── vcd vs spicy on nominal measures ─────────────────────────────────────────

cmp_vcd <- merge(
  flat[flat$source == "spicy", c("dataset", "measure", "estimate")],
  flat[flat$source == "vcd", c("dataset", "measure", "estimate")],
  by = c("dataset", "measure"), suffixes = c("_spicy", "_vcd")
)
cmp_vcd$diff <- abs(cmp_vcd$estimate_spicy - cmp_vcd$estimate_vcd)
cmp_vcd$ok_1e4 <- !is.na(cmp_vcd$diff) & cmp_vcd$diff < 1e-4

cat("\n── spicy vs vcd (nominal) ──\n")
print(cmp_vcd[order(-cmp_vcd$diff), ], row.names = FALSE)
cat(sprintf("Agreement at 1e-4: %d / %d\n", sum(cmp_vcd$ok_1e4), nrow(cmp_vcd)))

# ── Hmisc::somers2 cross-check on Somers' D ──────────────────────────────────
# somers2 takes a binary outcome y and a continuous/ordered group; we adapt
# from a 2x2 table by expanding it.

cat("\n── Hmisc::somers2 cross-check ──\n")
expand_table <- function(tab) {
  rows <- as.data.frame.table(tab)
  do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
    n <- rows$Freq[i]
    if (n == 0) return(NULL)
    data.frame(row = rep(rows[[1]][i], n), col = rep(rows[[2]][i], n))
  }))
}
if (is_2x2(datasets$mtcars_2x2)) {
  d <- expand_table(datasets$mtcars_2x2)
  d$row_n <- as.numeric(as.factor(d$row))  # binary 1/2
  d$col_n <- as.numeric(as.factor(d$col))
  s2 <- Hmisc::somers2(d$col_n, d$row_n - 1L)
  cat(sprintf("Hmisc::somers2 on mtcars_2x2: Dxy = %.6f\n", s2["Dxy"]))
  spicy_d <- somers_d(datasets$mtcars_2x2, "column", detail = TRUE)
  cat(sprintf("spicy::somers_d C|R       : %.6f\n", spicy_d[["estimate"]]))
}

# ── Export numerical values for the report ──────────────────────────────────

saveRDS(list(
  spicy = spicy_values,
  DescTools = desc_values,
  vcd = vcd_values,
  cmp_desc = cmp,
  cmp_vcd = cmp_vcd
), file.path(out_dir, "values.rds"))

cat("\nValues exported to tools/validation/values.rds and values_long.csv\n")
