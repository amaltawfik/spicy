# contingencytables 3.1.0 : égalités manquées entre tableaux dans Exact_cond_midP_tests_rxc()

- **Cible** : contingencytables 3.1.0 (`develop` 2d5fad1 identique sur ces lignes)
- **Canal** : issue GitHub [ocbe-uio/contingencytables#71](https://github.com/ocbe-uio/contingencytables/issues/71)
- **Posté** : 2026-10-21
- **Statut** : ouvert
- **Côté spicy** : non exposé (spicy ne dépend pas de contingencytables).

---

*Dossier tel que posté, conservé verbatim ci-dessous.*

# Exact_cond_midP_tests_rxc() misses ties between tables, as in #62
contingencytables 3.1.0 from CRAN, R 4.6.1. The code quoted below is the same on `develop` (2d5fad1). This is one of the problems announced at the end of #62: the comparison of #62, in another function.

### Steps to reproduce

```r
library(contingencytables)  # 3.1.0
x <- matrix(c(0, 2, 0, 2, 1, 1), 3, byrow = TRUE)
Exact_cond_midP_tests_rxc(x)
y <- matrix(c(1, 0, 0, 1, 1, 0), 3, byrow = TRUE)
Exact_cond_midP_tests_rxc(y)
Exact_cond_midP_tests_rxc(floor(table_7.3 / 10))  # the table of test-ch7.R, line 21
```

### Actual behavior

```r
# x
#> Exact LR statistic:          P = 0.3333333
#> Mid-P LR statistic:          P = 0.1666667
# y
#> Mid-P Pearson statistic:     P = 0.6666667
# floor(table_7.3 / 10)
#> Exact Fisher-Freeman-Halton: P = 0.5882353
#> Mid-P Fisher-Freeman-Halton: P = 0.4595588
#> Exact LR statistic:          P = 0.4595588
#> Mid-P LR statistic:          P = 0.3952206
```

### Expected behavior

With the margins of `x`, and with those of `y`, the three possible tables are the observed table with its rows permuted. The Fisher-Freeman-Halton, Pearson, LR and Kruskal-Wallis statistics are therefore the same for the three tables, so P = 1 and mid-P = 0.5 for each of them, as the function gives for the others among these four.

In `floor(table_7.3 / 10)`, rows (4, 2), (5, 0), (6, 1), the observed table and the table with rows (6, 0), (4, 1), (5, 2) have the same six counts and the same margins, hence the same probability and the same LR statistic. An exact enumeration gives P = 0.5882353 and mid-P = 0.4595588 for the LR test, the values the function gives for the Fisher-Freeman-Halton test of this table.

The statistics differ in the last bits:

```r
s <- function(z) contingencytables:::test_statisticsEcmt(z, sum(z), 3, 2, rowSums(z), colSums(z))
tx <- list(x, x[c(3, 1, 2), ], x[c(1, 3, 2), ])  # x and its row permutations
for (z in tx) print(sprintf("%.20g", s(z)[[3]]))   # LR statistic
#> [1] "2.6341457841558750808"
#> [1] "2.6341457841558746367"
#> [1] "2.6341457841558746367"
ty <- list(y, y[c(2, 1, 3), ], y[c(1, 3, 2), ])
for (z in ty) print(sprintf("%.20g", s(z)[[2]]))   # Pearson statistic
#> [1] "3"
#> [1] "3.0000000000000004441"
#> [1] "3"
```

### Cause

Lines 69-85 of `R/Exact_cond_midP_tests_rxc.R` compare the statistic of each table with that of the observed table using `<=`, `<`, `==`, `>=` and `>`, as the two functions of #62 did.

### Suggested fix

The tolerance of `stats::fisher.test()`, as in #63:

```diff
--- a/R/Exact_cond_midP_tests_rxc.R
+++ b/R/Exact_cond_midP_tests_rxc.R
@@ -66,17 +66,22 @@ Exact_cond_midP_tests_rxc <- function(n) {
     T_lbl <- ans[[5]]
     T_JT <- ans[[6]]
 
-    P_FFH <- P_FFH + ifelse(f <= Tobs_FFH, 1, 0) * f
-    midP_FFH <- midP_FFH + ifelse(f < Tobs_FFH, 1, 0) * f + 0.5 * ifelse(f == Tobs_FFH, 1, 0) * f
+    # Statistics that agree to a relative 1e-7 are ties, as in stats::fisher.test()
+    tie_FFH <- abs(f - Tobs_FFH) <= 1e-7 * Tobs_FFH
+    P_FFH <- P_FFH + ifelse(f < Tobs_FFH | tie_FFH, 1, 0) * f
+    midP_FFH <- midP_FFH + ifelse(f < Tobs_FFH & !tie_FFH, 1, 0) * f + 0.5 * ifelse(tie_FFH, 1, 0) * f
 
-    P_Pearson <- P_Pearson + ifelse(T_Pearson >= Tobs_Pearson, 1, 0) * f
-    midP_Pearson <- midP_Pearson + ifelse(T_Pearson > Tobs_Pearson, 1, 0) * f + 0.5 * ifelse(T_Pearson == Tobs_Pearson, 1, 0) * f
+    tie_Pearson <- abs(T_Pearson - Tobs_Pearson) <= 1e-7 * Tobs_Pearson
+    P_Pearson <- P_Pearson + ifelse(T_Pearson > Tobs_Pearson | tie_Pearson, 1, 0) * f
+    midP_Pearson <- midP_Pearson + ifelse(T_Pearson > Tobs_Pearson & !tie_Pearson, 1, 0) * f + 0.5 * ifelse(tie_Pearson, 1, 0) * f
 
-    P_LR <- P_LR + ifelse(T_LR >= Tobs_LR, 1, 0) * f
-    midP_LR <- midP_LR + ifelse(T_LR > Tobs_LR, 1, 0) * f + 0.5 * ifelse(T_LR == Tobs_LR, 1, 0) * f
+    tie_LR <- abs(T_LR - Tobs_LR) <= 1e-7 * Tobs_LR
+    P_LR <- P_LR + ifelse(T_LR > Tobs_LR | tie_LR, 1, 0) * f
+    midP_LR <- midP_LR + ifelse(T_LR > Tobs_LR & !tie_LR, 1, 0) * f + 0.5 * ifelse(tie_LR, 1, 0) * f
 
-    P_KW <- P_KW + ifelse(T_KW >= Tobs_KW, 1, 0) * f
-    midP_KW <- midP_KW + ifelse(T_KW > Tobs_KW, 1, 0) * f + 0.5 * ifelse(T_KW == Tobs_KW, 1, 0) * f
+    tie_KW <- abs(T_KW - Tobs_KW) <= 1e-7 * Tobs_KW
+    P_KW <- P_KW + ifelse(T_KW > Tobs_KW | tie_KW, 1, 0) * f
+    midP_KW <- midP_KW + ifelse(T_KW > Tobs_KW & !tie_KW, 1, 0) * f + 0.5 * ifelse(tie_KW, 1, 0) * f
 
     P_lbl <- P_lbl + ifelse(T_lbl >= Tobs_lbl, 1, 0) * f
     midP_lbl <- midP_lbl + ifelse(T_lbl > Tobs_lbl, 1, 0) * f + 0.5 * ifelse(T_lbl == Tobs_lbl, 1, 0) * f
```

With this change, the eight outputs equal an exact enumeration on all 765 3 x 2 tables with row totals m, m, m (m = 2 to 6) and no empty column, on 150 random 3 x 2 tables and on 60 random 3 x 3 tables with equal row totals. 3.1.0 differs on 552, 48 and 47 of them, by up to 0.67. The enumeration finds ties exactly (integer keys for the Pearson and Kruskal-Wallis statistics, prime factorizations of prod(x_ij!) and prod(x_ij^x_ij) for the Fisher-Freeman-Halton and LR statistics), so it does not depend on a tolerance. `x` and `y` then give P = 1 and mid-P = 0.5 for the four statistics, and the LR test of `floor(table_7.3 / 10)` gives P = 0.5882353 and mid-P = 0.4595588. The values of Table 7.12 of the book (`table_7.3`) do not change, and no expectation of `tests/testthat` changes.

The linear-by-linear and Jonckheere-Terpstra lines are treated in #69. Applied together, with the change of line 213 from #68, the two changes give the same agreement on the same tables.
