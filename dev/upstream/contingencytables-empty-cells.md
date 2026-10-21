# contingencytables 3.1.0 : cellules vides comptées comme deux observations (cinq fonctions)

- **Cible** : contingencytables 3.1.0 (`develop` 2d5fad1 identique sur ces lignes)
- **Canal** : issue GitHub [ocbe-uio/contingencytables#68](https://github.com/ocbe-uio/contingencytables/issues/68)
- **Posté** : 2026-10-21
- **Statut** : ouvert
- **Côté spicy** : non exposé. spicy ne dépend pas de contingencytables ; le paquet servait d'oracle de triangulation pour les tests de Fisher (#62).

---

*Dossier tel que posté, conservé verbatim ci-dessous.*

# Empty cells are counted as two observations in linear_by_linear_test_rxc() and four other functions
contingencytables 3.1.0 from CRAN, R 4.6.1. The code quoted below is the same on `develop` (2d5fad1). This is one of the problems announced at the end of #62.

### Steps to reproduce

```r
library(contingencytables)  # 3.1.0
linear_by_linear_test_rxc(ritland_2007)
n3 <- floor(table_7.7 / 10)  # the table of test-ch7.R, four empty cells
Pearson_correlation_coefficient_rxc(n3)
Kendalls_tau_b_rxc(n3)
set.seed(1562)
Kendalls_tau_b_rxc_bca(n3, nboot = 200)  # as in test-ch7.R
linear_by_linear_test_rxc(table_7.8, 1:3, 1:5)  # help example, scores given
Pearson_correlation_coefficient_rxc(table_7.8)
```

### Actual behavior

```r
#> The linear-by-linear test for association: P = 0.354079, Z = -0.927
#> The Pearson correlation coefficient: r =  0.2019 (95% CI -0.0347 to  0.4171)
#> Kendalls tau-b w / Fieller CI: tau-b = -0.1235 (95% CI -0.2762 to  0.0354)
#> Kendalls tau-b w / BCa bootstrap CI: tau-b = -0.1235 (95% CI -0.0863 to  0.3904)
#> The linear-by-linear test for association: P = 0.000000, Z =  6.560
#> The Pearson correlation coefficient: r =  0.5710 (95% CI  0.4439 to  0.6756)
```

The Pearson correlation of `n3` is positive while its tau-b is negative, and the BCa interval of tau-b does not contain the estimate.

### Expected behavior

The same statistics, computed from the observations of each table:

```r
n <- ritland_2007
sqrt(sum(n) - 1) * cor(rep(row(n), n), rep(col(n), n))
#> [1] -1.993139
DescTools::MHChisqTest(n)$p.value
#>  X-squared
#> 0.04624624
cor(rep(row(n3), n3), rep(col(n3), n3))
#> [1] -0.1565616
n <- table_7.8
sqrt(sum(n) - 1) * cor(rep(row(n), n), rep(col(n), n))
#> [1] 7.571748
cor(rep(row(n), n), rep(col(n), n))
#> [1] 0.6590361
```

For `table_7.8`, the book prints 7.57 in Table 7.21 and r = 0.659 on p. 315.

### Cause

In R, `1:0` is `c(1, 0)`. The loop that expands the table into one row per observation runs twice for an empty cell and adds two observations. It is at:

- `R/linear_by_linear_test_rxc.R`, line 30
- `R/Pearson_correlation_coefficient_rxc.R`, line 34
- `R/Pearson_correlation_coefficient_rxc_bca.R`, line 38
- `R/Kendalls_tau_b_rxc_bca.R`, line 31 (the estimate comes from `Kendalls_tau_b_rxc()`, but the bootstrap data have two extra rows per empty cell: 78 rows for the 70 observations of `n3`)
- `R/Exact_cond_midP_tests_rxc.R`, line 213, in `linear_by_linear_statistic()`

For `ritland_2007` (N = 88, one empty cell), `cor()` receives 90 values. `gamma_coefficient_rxc_bca()` and `Spearman_correlation_coefficient_rxc_bca()` guard the same loop with `if (n[i, j] > 0)`.

### Suggested fix

In the first four files:

```diff
-      for (k in 1:n[i, j]) {
+      for (k in seq_len(n[i, j])) {
```

and at line 213 of `R/Exact_cond_midP_tests_rxc.R`:

```diff
-      for (k in 1:x[i, j]) {
+      for (k in seq_len(x[i, j])) {
```

With this change, Z^2 of `linear_by_linear_test_rxc()` equals the X-squared of `DescTools::MHChisqTest()` on all 17,775 2 x 2 tables with row totals 1 to 15 and no empty column. In 3.1.0, 6,730 of the 6,750 tables with an empty cell differ, and 2,008 p-values fall on the other side of 0.05. On 400 random r x c tables with an empty cell, Z and r equal `sqrt(N - 1) * cor()` and `cor()` (397 differ in 3.1.0). Tables without an empty cell give the same values as before. `table_7.8` gives Z = 7.572 and r = 0.6590.

Four expectations of `tests/testthat/test-ch7.R` change: lines 20-23 (exact linear-by-linear, 0.9754902 becomes 0.8835784), 60-66 (the tau-b interval becomes -0.3415 to 0.0690), 75-78 (r = 0.2019 becomes -0.1566, 95% CI -0.3776 to 0.0814) and 79-87 (r = -0.1566, 80% CI -0.2977 to -0.0020).
