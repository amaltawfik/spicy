# contingencytables 3.1.0 : p unilatéraux des tests exacts linéaire et de Jonckheere-Terpstra

- **Cible** : contingencytables 3.1.0 (`develop` 2d5fad1 identique sur ces lignes)
- **Canal** : issue GitHub [ocbe-uio/contingencytables#69](https://github.com/ocbe-uio/contingencytables/issues/69)
- **Posté** : 2026-10-21
- **Statut** : ouvert
- **Côté spicy** : non exposé (spicy ne dépend pas de contingencytables).

---

*Dossier tel que posté, conservé verbatim ci-dessous.*

# Exact_cond_midP_tests_rxc() gives one-sided p-values for the linear-by-linear and Jonckheere-Terpstra tests
contingencytables 3.1.0 from CRAN, R 4.6.1. The code quoted below is the same on `develop` (2d5fad1). This is one of the problems announced at the end of #62.

### Steps to reproduce

```r
library(contingencytables)  # 3.1.0
Exact_cond_midP_tests_rxc(table_7.3)          # help example
Exact_cond_midP_tests_rxc(table_7.3[3:1, ])   # the same table with its rows reversed
linear_by_linear_test_rxc(table_7.3, 1:3, 1:2)
JonckheereTerpstra_test_rxc(table_7.3)
```

### Actual behavior

The other eight lines of the output are the same for both tables.

```r
# table_7.3
#> Exact linear-by-linear:      P = 0.9998521
#> Mid-P linear-by-linear:      P = 0.9997592
#> Exact Jonckheere-Terpstra:   P = 0.9996747
#> Mid-P Jonckheere-Terpstra:   P = 0.9996710
# table_7.3[3:1, ]
#> Exact linear-by-linear:      P = 0.0003336
#> Mid-P linear-by-linear:      P = 0.0002408
#> Exact Jonckheere-Terpstra:   P = 0.0003327
#> Mid-P Jonckheere-Terpstra:   P = 0.0003290

#> The linear-by-linear test for association: P = 0.000508, Z = -3.477
#> The Jonckheere-Terpstra test for association: P = 0.000589, Z = -3.437
```

### Expected behavior

Section 7.8.3 (p. 313) defines the exact linear-by-linear p-value with T_linear = (N - 1) r^2, which does not depend on the direction of the association. The exact test of coin gives that value, the same for both row orders:

```r
library(coin)  # 1.4-6
d <- as.data.frame(as.table(table_7.3))
d$Var1 <- ordered(d$Var1, levels = rownames(table_7.3))
pvalue(lbl_test(Var1 ~ Var2, data = d, weights = ~Freq, distribution = "exact"))
#> [1] 0.0005126216
```

For the Jonckheere-Terpstra test, ordering the tables by (T_JT - E(T_JT))^2 is ordering them by T*_JT = Z_JT^2 of p. 313, since E(T_JT) and Var(T_JT) depend only on the margins. By enumeration (code below), this gives P = 0.0005177 and mid-P = 0.0005140 for both row orders. The exact p-value on p. 314 is written with T_JT itself, which gives the upper tail. Doubling the smaller tail would be the other usual two-sided choice.

<details>
<summary>Enumeration</summary>

```r
jt_exact <- function(n) {
  rs <- rowSums(n); c1 <- sum(n[, 1])
  g <- expand.grid(a = 0:rs[1], b = 0:rs[2]); g$c <- c1 - g$a - g$b
  g <- g[g$c >= 0 & g$c <= rs[3], ]
  f <- choose(rs[1], g$a) * choose(rs[2], g$b) * choose(rs[3], g$c) / choose(sum(rs), c1)
  jt <- function(x1) {  # pairs of rows i < k: the row-k observation in a higher column counts 1, a tie 1/2
    x2 <- rs - x1; s <- 0
    for (i in 1:2) for (k in (i + 1):3) s <- s + x1[i] * x2[k] + 0.5 * (x1[i] * x1[k] + x2[i] * x2[k])
    s
  }
  D <- (apply(g, 1, jt) - (sum(rs)^2 - sum(rs^2)) / 4)^2
  Do <- (jt(n[, 1]) - (sum(rs)^2 - sum(rs^2)) / 4)^2
  c(P = sum(f[D >= Do]), midP = sum(f[D > Do]) + 0.5 * sum(f[D == Do]))
}
jt_exact(table_7.3)
#>            P         midP
#> 0.0005177199 0.0005140285
jt_exact(table_7.3[3:1, ])
#>            P         midP
#> 0.0005177199 0.0005140285
```

</details>

### Cause

Lines 81-85 of `R/Exact_cond_midP_tests_rxc.R` compare signed statistics with `>=` and `>`: `sqrt(N - 1) * r` (line 221) and the sum of the Mann-Whitney U statistics (line 250). This gives P(T >= t), the upper tail. `Exact_cond_midP_tests_rxc.m` on the book's site has the same comparisons (lines 59-63).

### Suggested fix

Order the tables by the squared statistics:

```diff
--- a/R/Exact_cond_midP_tests_rxc.R
+++ b/R/Exact_cond_midP_tests_rxc.R
@@ -42,6 +42,12 @@ Exact_cond_midP_tests_rxc <- function(n) {
   Tobs_lbl <- ans[[5]]
   Tobs_JT <- ans[[6]]
 
+  # Two-sided linear-by-linear and Jonckheere-Terpstra tests: order the tables
+  # by T_linear = (N - 1) r^2 (Section 7.8.1) and by (T_JT - E(T_JT))^2
+  E_JT <- (N^2 - sum(nip^2)) / 4
+  Tobs_lbl <- Tobs_lbl^2
+  Tobs_JT <- (Tobs_JT - E_JT)^2
+
   # Calculate exact P- and mid-P values
   P_FFH <- 0
   P_Pearson <- 0
@@ -65,6 +71,8 @@ Exact_cond_midP_tests_rxc <- function(n) {
     T_KW <- ans[[4]]
     T_lbl <- ans[[5]]
     T_JT <- ans[[6]]
+    T_lbl <- T_lbl^2
+    T_JT <- (T_JT - E_JT)^2
 
     P_FFH <- P_FFH + ifelse(f <= Tobs_FFH, 1, 0) * f
     midP_FFH <- midP_FFH + ifelse(f < Tobs_FFH, 1, 0) * f + 0.5 * ifelse(f == Tobs_FFH, 1, 0) * f
@@ -78,8 +86,10 @@ Exact_cond_midP_tests_rxc <- function(n) {
     P_KW <- P_KW + ifelse(T_KW >= Tobs_KW, 1, 0) * f
     midP_KW <- midP_KW + ifelse(T_KW > Tobs_KW, 1, 0) * f + 0.5 * ifelse(T_KW == Tobs_KW, 1, 0) * f
 
-    P_lbl <- P_lbl + ifelse(T_lbl >= Tobs_lbl, 1, 0) * f
-    midP_lbl <- midP_lbl + ifelse(T_lbl > Tobs_lbl, 1, 0) * f + 0.5 * ifelse(T_lbl == Tobs_lbl, 1, 0) * f
+    # Mirror tables have equal T_linear up to rounding, also when T_linear is 0
+    tie_lbl <- abs(T_lbl - Tobs_lbl) <= 1e-7 * Tobs_lbl + 1e-12
+    P_lbl <- P_lbl + ifelse(T_lbl > Tobs_lbl | tie_lbl, 1, 0) * f
+    midP_lbl <- midP_lbl + ifelse(T_lbl > Tobs_lbl & !tie_lbl, 1, 0) * f + 0.5 * ifelse(tie_lbl, 1, 0) * f
 
     P_JT <- P_JT + ifelse(T_JT >= Tobs_JT, 1, 0) * f
     midP_JT <- midP_JT + ifelse(T_JT > Tobs_JT, 1, 0) * f + 0.5 * ifelse(T_JT == Tobs_JT, 1, 0) * f
```

T_JT is a multiple of 1/2, so its squared distance to E(T_JT) is exact and `==` is enough there. T_linear needs the tolerance: mirror tables tie under the squared statistic. Without the absolute 1e-12, one of the random 3 x 2 tables of the check below, whose observed T_linear is 0 up to rounding (2.9e-39), missed its ties.

With this change and the change of line 213 from #68, the four outputs equal the enumeration, to 1.2e-14, on all 765 3 x 2 tables with row totals m, m, m (m = 2 to 6) and no empty column, on 150 random 3 x 2 tables and on 40 random 3 x 3 tables. 3.1.0 differs from it on all of these tables. `table_7.3` gives P = 0.0005126 and mid-P = 0.0004197 (linear-by-linear), and P = 0.0005177 and mid-P = 0.0005140 (Jonckheere-Terpstra), in both row orders. With both changes, one expectation changes: `tests/testthat/test-ch7.R`, lines 20-23 (exact linear-by-linear, 0.9754902 becomes 0.4901961).
