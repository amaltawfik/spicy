# contingencytables : égalités de tables manquées par Fisher_midP_test_2x2() et Fisher_exact_test_2x2()

- **Cible** : contingencytables 3.1.0 (CRAN), `develop` 3405f12f, R 4.6.1
- **Canal** : issue GitHub [ocbe-uio/contingencytables#62](https://github.com/ocbe-uio/contingencytables/issues/62)
  (le README demande une issue pour tout bug, et le guide de contribution
  de l'organisation a été suivi : recherche préalable, reproduction,
  comportement attendu et observé)
- **Envoyé** : 2026-10-20, par Amal (compte amaltawfik), texte ci-dessous tel quel
- **Statut** : ouvert
- **Côté spicy** : aucun contournement. spicy calcule son propre mid-p
  (`test = "midp"` de table_categorical() et table_multiresponse(),
  définition 1.12 du livre, égalités à la tolérance de fisher.test()) et
  n'importe pas contingencytables. Quatre autres signalements annoncés à la
  fin de l'issue (statistique LR, boucle sur les cellules vides et scores
  par défaut, tests exacts unilatéraux de Exact_cond_midP_tests_rxc(),
  facteur de Mantel-Haenszel), à rédiger au même format après la première
  réponse.

## Titre

Fisher_midP_test_2x2() and Fisher_exact_test_2x2() miss ties between tables

## Corps, tel qu'envoyé

contingencytables 3.1.0 from CRAN, R 4.6.1. The code quoted below is the same on `develop` (3405f12f). I came across this while checking reference values for a Fisher mid-P test. #51 fixed a different problem in `Fisher_midP_test_2x2()`.

### Steps to reproduce

```r
library(contingencytables)  # 3.1.0
x <- matrix(c(2, 36, 8, 30), 2, byrow = TRUE)
Fisher_midP_test_2x2(x)
Fisher_midP_test_2x2(x[2:1, ])  # the same table with its rows swapped

y <- matrix(c(8, 2, 30, 36), 2, byrow = TRUE)
Fisher_exact_test_2x2(y, "hypergeometric")
fisher.test(y)$p.value
```

### Actual behavior

```r
#> The Fisher mid-P test (Fisher-Irwin): P = 0.06800
#> The Fisher mid-P test (Fisher-Irwin): P = 0.03198
#> The Fisher exact test (Fisher-Irwin): P = 0.04999
#> [1] 0.08600352
```

### Expected behavior

A table and the same table with its rows swapped have the same mid-P value. By the definition of Section 4.4.6 (p. 97), applied as in the tea example (p. 100), it is 0.04998656 for both:

```r
f <- dhyper(0:10, 10, 66, 38)  # x11 = 0, ..., 10 under the margins of x
sum(f[c(1, 2, 10, 11)]) + 0.5 * (f[3] + f[9])
#> [1] 0.04998656
```

With `statistic = "hypergeometric"`, the exact p-value of `y` is that of `fisher.test()`, 0.08600352.

### Cause

Line 44 of `R/Fisher_midP_test_2x2.R` and line 44 of `R/Fisher_exact_test_2x2.R` compare the statistic of each table with that of the observed table, using `>` and `==` (mid-P test) or `>=` (exact test). Under the margins of `x`, the two tables are x11 = 2 and x11 = 8. They are equally probable, but their statistics differ in the last bits, so the comparison misses the tie:

```r
s <- 0:10
T <- contingencytables:::test_statistic_fisher_midp_test_2x2(s, 38 - s, 10 - s, 28 + s, "hypergeometric")
sprintf("%.20g", T[c(3, 9)])  # x11 = 2 and x11 = 8
#> [1] "-0.036016964060236342227" "-0.036016964060236307532"
```

Ties do not need symmetric margins. For `matrix(c(2, 1, 12, 3), 2, byrow = TRUE)`, the observed table (x11 = 2) and the table x11 = 3 are the two modes of the distribution, and `Fisher_midP_test_2x2()` gives P = 0.77696 instead of 0.55392.

Of the 23,731 tables with equal row totals from 3 to 40 and no empty column, 8,912 get a mid-P value that differs from the value of the definition by more than 1e-9. Of the 9,860 tables with equal column totals from 2 to 30, unequal row totals and no empty row, 1,960 get an exact p-value that differs from `fisher.test()` by more than 1e-9. The mid-P test shows no difference on the second set, and the exact test none on the first. Before 27e3ceb8 (#55), which changed the order of the `dhyper()` arguments, the exact test missed ties in 4,484 of the 23,731 tables with equal row totals and in none of the 9,860 others.

### Suggested fix

A relative tolerance of 1e-7, as `stats::fisher.test()` uses, fixes both. In `R/Fisher_midP_test_2x2.R`:

```diff
-  P <- sum(fvalues[Tvalues > Tobs]) + 0.5 * sum(fvalues[Tvalues == Tobs])
+  tol <- 1e-7 * abs(Tobs)
+  P <- sum(fvalues[Tvalues > Tobs + tol]) + 0.5 * sum(fvalues[abs(Tvalues - Tobs) <= tol])
```

In `R/Fisher_exact_test_2x2.R`:

```diff
-  P <- sum(fvalues[Tvalues >= Tobs])
+  P <- sum(fvalues[Tvalues >= Tobs - 1e-7 * abs(Tobs)])
```

With these two changes, no table of the two sets differs from the definition or from `fisher.test()`, the p-values with `statistic = "Pearson"` do not change on these tables, and the expectations of `tests/testthat/test-issue51.R` still hold. I can open a pull request with these changes and tests for the tables above.

While checking, I also found problems with `statistic = "LR"` in these two functions and problems in six other functions. I will report them in separate issues.
