# contingencytables 3.1.0 : scores par défaut inversés dans linear_by_linear_test_rxc()

- **Cible** : contingencytables 3.1.0 (`develop` 2d5fad1 identique sur ces lignes)
- **Canal** : issue GitHub [ocbe-uio/contingencytables#72](https://github.com/ocbe-uio/contingencytables/issues/72)
- **Posté** : 2026-10-21
- **Statut** : ouvert
- **Côté spicy** : non exposé (spicy ne dépend pas de contingencytables).

---

*Dossier tel que posté, conservé verbatim ci-dessous.*

# linear_by_linear_test_rxc() returns NA for non-square tables with the default scores
contingencytables 3.1.0 from CRAN, R 4.6.1. The code quoted below is the same on `develop` (2d5fad1). This is one of the problems announced at the end of #62.

### Steps to reproduce

```r
library(contingencytables)  # 3.1.0
linear_by_linear_test_rxc(table_7.8)            # help example, a 3 x 5 table
linear_by_linear_test_rxc(table_7.3)            # a 3 x 2 table
linear_by_linear_test_rxc(table_7.3, 1:3, 1:2)
```

### Actual behavior

```r
#> The linear-by-linear test for association: P =       NA, Z =     NA
#> The linear-by-linear test for association: P =       NA, Z =     NA
#> The linear-by-linear test for association: P = 0.000508, Z = -3.477
```

### Expected behavior

`a` holds the row scores and `b` the column scores, equally spaced when not given. With `a = 1:3` and `b = 1:2`, Z for `table_7.3` agrees with `DescTools::MHChisqTest()`:

```r
-sqrt(DescTools::MHChisqTest(table_7.3)$statistic)
#> X-squared
#> -3.476703
```

### Cause

Line 15 of `R/linear_by_linear_test_rxc.R` sets `a = seq_len(ncol(n))` and `b = seq_len(nrow(n))`, while `a` is used for the rows (`Y1[id] <- a[i]`, line 32) and `b` for the columns (line 33). For a 3 x 2 table, `a[3]` is `NA`. The defaults have been swapped since the first version of the file (308057a). `linear_by_linear_test_rxc.m` on the book's site sets `a = 1:r` and `b = 1:c` (lines 21-22). `the_rxc_table()` passes the scores (line 165) and is not affected.

### Suggested fix

```diff
-linear_by_linear_test_rxc <- function(n, a = seq_len(ncol(n)), b = seq_len(nrow(n))) {
+linear_by_linear_test_rxc <- function(n, a = seq_len(nrow(n)), b = seq_len(ncol(n))) {
```

and the same in the usage section of `man/linear_by_linear_test_rxc.Rd`. With this change, Z equals `sqrt(N - 1) * cor()` on 300 random non-square tables without an empty cell (NA for all of them in 3.1.0), square tables give the same values as before, and the tests of the package pass. `table_7.8` then gives Z = 6.560, and Z = 7.572 (the 7.57 of Table 7.21 of the book) once the change proposed in #68 is applied as well, since `table_7.8` has two empty cells.
