## Build the spicy vs PSPP comparison matrix

suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
  library(DescTools)
})

data(mtcars)
data(HairEyeColor)
data(sochealth, package = "spicy")

datasets <- list(
  mtcars_3x3 = table(factor(mtcars$gear), factor(mtcars$cyl)),
  mtcars_2x2 = table(factor(mtcars$vs), factor(mtcars$am)),
  HairEye = margin.table(HairEyeColor, c(1, 2)),
  sochealth = table(sochealth$smoking, sochealth$education)
)

# PSPP oracle values, hand-extracted from CSVs (7-decimal precision)
pspp <- list(
  mtcars_3x3 = c(
    "Cramer V"    = 0.5308655,
    "Contingency C" = 0.6003875,
    "Tau-b"       = -0.5125435,
    "Tau-c"       = -0.4833984,
    "Gamma"       = -0.6573705,
    "Lambda sym"  = 0.4857143,
    "Lambda R|C"  = 0.5294118,
    "Lambda C|R"  = 0.4444444,
    "GK Tau R|C"  = 0.3825603,
    "GK Tau C|R"  = 0.3386018,
    "U sym"       = 0.3504372,
    "U R|C"       = 0.3587709,
    "U C|R"       = 0.3424818,
    "Somers D sym"= -0.5124224,
    "Somers D R|C"= -0.5015198,
    "Somers D C|R"= -0.5238095
  ),
  mtcars_2x2 = c(
    "Phi"         = 0.1683451,
    "Cramer V"    = 0.1683451,
    "Contingency C" = 0.1660092,
    "Tau-b"       = 0.1683451,
    "Tau-c"       = 0.1640625,
    "Gamma"       = 0.3333333,
    "Lambda sym"  = 0.0370370,
    "Lambda R|C"  = 0.0714286,
    "Lambda C|R"  = 0.0000000,
    "GK Tau R|C"  = 0.0283401,
    "GK Tau C|R"  = 0.0283401,
    "U sym"       = 0.0208314,
    "U R|C"       = 0.0206817,
    "U C|R"       = 0.0209833,
    "Somers D sym"= 0.1683367,
    "Somers D R|C"= 0.1700405,
    "Somers D C|R"= 0.1666667
  ),
  HairEye = c(
    "Cramer V"    = 0.2790446,
    "Contingency C" = 0.4351585,
    "Tau-b"       = 0.2247026,
    "Tau-c"       = 0.2046886,
    "Gamma"       = 0.3177721,
    "Lambda sym"  = 0.1430678,
    "Lambda R|C"  = 0.0326797,
    "Lambda C|R"  = 0.2338710,
    "GK Tau R|C"  = 0.0746087,
    "GK Tau C|R"  = 0.1136376,
    "U sym"       = 0.0984203,
    "U R|C"       = 0.0992313,
    "U C|R"       = 0.0976225,
    "Somers D sym"= 0.2246768,
    "Somers D R|C"= 0.2213218,
    "Somers D C|R"= 0.2281350
  ),
  sochealth = c(
    "Cramer V"    = 0.1356677,
    "Contingency C" = 0.1344361,
    "Tau-b"       = -0.1264155,
    "Tau-c"       = -0.1169210,
    "Gamma"       = -0.2680678,
    "Lambda sym"  = 0.0000000,
    "Lambda R|C"  = 0.0000000,
    "Lambda C|R"  = 0.0000000,
    "GK Tau R|C"  = 0.0184057,
    "GK Tau C|R"  = 0.0076095,
    "U sym"       = 0.0114876,
    "U R|C"       = 0.0175123,
    "U C|R"       = 0.0085472,
    "Somers D sym"= -0.1200077,
    "Somers D R|C"= -0.0913067,
    "Somers D C|R"= -0.1750241
  )
)

# Compute spicy values on each dataset
sp_val <- function(tab) {
  is2 <- nrow(tab) == 2L && ncol(tab) == 2L
  out <- list()
  if (is2) out[["Phi"]] <- phi(tab)
  out[["Cramer V"]] <- cramer_v(tab)
  out[["Contingency C"]] <- contingency_coef(tab)
  out[["Tau-b"]] <- kendall_tau_b(tab)
  out[["Tau-c"]] <- kendall_tau_c(tab)
  out[["Gamma"]] <- gamma_gk(tab)
  out[["Lambda sym"]] <- lambda_gk(tab, "symmetric")
  out[["Lambda R|C"]] <- lambda_gk(tab, "row")
  out[["Lambda C|R"]] <- lambda_gk(tab, "column")
  out[["GK Tau R|C"]] <- goodman_kruskal_tau(tab, "row")
  out[["GK Tau C|R"]] <- goodman_kruskal_tau(tab, "column")
  out[["U sym"]] <- uncertainty_coef(tab, "symmetric")
  out[["U R|C"]] <- uncertainty_coef(tab, "row")
  out[["U C|R"]] <- uncertainty_coef(tab, "column")
  out[["Somers D sym"]] <- somers_d(tab, "symmetric")
  out[["Somers D R|C"]] <- somers_d(tab, "row")
  out[["Somers D C|R"]] <- somers_d(tab, "column")
  out
}

spicy <- lapply(datasets, sp_val)

# Build comparison
rows <- list()
for (ds in names(pspp)) {
  for (m in names(pspp[[ds]])) {
    sp_v <- if (m %in% names(spicy[[ds]])) spicy[[ds]][[m]] else NA_real_
    pp_v <- pspp[[ds]][m]
    rows[[length(rows) + 1L]] <- data.frame(
      dataset = ds,
      measure = m,
      spicy = sp_v,
      pspp = pp_v,
      diff = abs(sp_v - pp_v),
      stringsAsFactors = FALSE
    )
  }
}
cmp <- do.call(rbind, rows)
rownames(cmp) <- NULL
cmp$ok_1e6 <- !is.na(cmp$diff) & cmp$diff < 1e-6
cmp$ok_1e4 <- !is.na(cmp$diff) & cmp$diff < 1e-4

cat("=== spicy vs PSPP ===\n")
print(cmp[order(-cmp$diff), ], row.names = FALSE)
cat(sprintf("\nAgreement at 1e-6: %d / %d\n", sum(cmp$ok_1e6), nrow(cmp)))
cat(sprintf("Agreement at 1e-4: %d / %d\n", sum(cmp$ok_1e4), nrow(cmp)))

write.csv(cmp, "tools/validation/spicy_vs_pspp.csv", row.names = FALSE)
