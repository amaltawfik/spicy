# contingencytables 3.1.0 : statistique LR des tests de Fisher exact et mid-P

- **Cible** : contingencytables 3.1.0 (`develop` 2d5fad1 identique sur ces lignes)
- **Canal** : issue GitHub [ocbe-uio/contingencytables#70](https://github.com/ocbe-uio/contingencytables/issues/70)
- **Posté** : 2026-10-21
- **Statut** : ouvert
- **Côté spicy** : non exposé (spicy ne dépend pas de contingencytables).

---

*Dossier tel que posté, conservé verbatim ci-dessous.*

# Wrong p-values with statistic = "LR" in Fisher_exact_test_2x2() and Fisher_midP_test_2x2()
contingencytables 3.1.0 from CRAN, R 4.6.1. The code quoted below is the same on `develop` (2d5fad1). These are the problems with `statistic = "LR"` announced at the end of #62.

### Steps to reproduce

```r
library(contingencytables)  # 3.1.0
Fisher_exact_test_2x2(ritland_2007, "LR")
Fisher_midP_test_2x2(ritland_2007, "LR")
Fisher_exact_test_2x2(perondi_2004, "LR")
Fisher_exact_test_2x2(perondi_2004[, 2:1], "LR")  # the same table with its columns swapped
```

### Actual behavior

```r
#> The Fisher exact test (LR): P = 0.06287
#> The Fisher mid-P test (LR): P = 0.04466
#> The Fisher exact test (LR): P = 0.05440
#> Warning in log(x22/(n2p * np2/N)) : NaNs produced
#> The Fisher exact test (LR): P = 1.00000
```

### Expected behavior

The exact conditional p-value with the likelihood ratio statistic of Equation 4.8 ("a term is 0 if n_ij = 0", p. 95), summed over x11 = n0, ..., n1 with n0 = max(0, n+1 - n2+) (Equation 4.11, p. 96):

```r
p_lr <- function(x, mid = FALSE) {
  n1 <- sum(x[1, ]); c1 <- sum(x[, 1]); N <- sum(x)
  s <- max(0, c1 - (N - n1)):min(n1, c1)
  f <- dhyper(s, c1, N - c1, n1)
  G <- sapply(s, function(a) {
    z <- matrix(c(a, n1 - a, c1 - a, N - n1 - c1 + a), 2, byrow = TRUE)
    m <- outer(rowSums(z), colSums(z)) / N
    2 * sum(ifelse(z > 0, z * log(z / m), 0))
  })
  g <- G[s == x[1, 1]]; tie <- abs(G - g) <= 1e-7 * g
  if (mid) sum(f[G > g & !tie]) + 0.5 * sum(f[tie]) else sum(f[G > g | tie])
}
p_lr(ritland_2007)
#> [1] 0.04138663
p_lr(ritland_2007, mid = TRUE)
#> [1] 0.02317965
p_lr(perondi_2004[, 2:1])
#> [1] 0.05440013
```

### Cause

- Lines 82-86 of `R/Fisher_exact_test_2x2.R` and lines 73-77 of `R/Fisher_midP_test_2x2.R` replace a zero count by 1, so an empty cell adds 1 * log(1 / m_ij) to the statistic instead of 0. For `ritland_2007`, `test_statistic_fisher_exact_test_2x2(0, 16, 15, 57, "LR")` gives 4.66552, while `LR_test_2x2(ritland_2007)$T` and `DescTools::GTest(ritland_2007, correct = "none")` give 6.672124.
- Line 29 of `R/Fisher_exact_test_2x2.R` starts x11 at 0 even when it cannot be 0. For those tables x22 is negative, `log()` returns NaN and line 47 sets P = 1. This is the `P = 1.00000` shown in #60. `Fisher_midP_test_2x2()` starts at `max(c(0, np1 - n2p))` (line 27), and since #51 it computes `Tobs` from the observed table.

### Suggested fix

On top of #63:

```diff
--- a/R/Fisher_exact_test_2x2.R
+++ b/R/Fisher_exact_test_2x2.R
@@ -26,7 +26,7 @@ validateArguments(
   N <- sum(n)
 
   # The possible tables, expressed by their x_11 counts
-  x11values <- 0:min(c(n1p, np1))
+  x11values <- max(c(0, np1 - n2p)):min(c(n1p, np1))
 
   # The probabilities of the possible tables (from the hypergeomtric distribution)
   fvalues <- dhyper(x11values, np1, N - np1, n1p)
@@ -38,7 +38,7 @@ validateArguments(
   )
 
   # Observed value of the test statistic
-  Tobs <- Tvalues[n[1, 1] + 1]
+  Tobs <- Tvalues[n[1, 1] - x11values[1] + 1]
 
   # Two-sided P-value
   # Statistics that agree to a relative 1e-7 are ties, as in stats::fisher.test()
@@ -80,16 +80,12 @@ test_statistic_fisher_exact_test_2x2 <- function(x11, x12, x21, x22, statistic)
     n2p <- x21 + x22
     np1 <- x11 + x21
     np2 <- x12 + x22
-    # Make sure 0 is added to T for terms where x_ij = 0 (log(1) = 0)
-    x11[x11 == 0] <- 1
-    x12[x12 == 0] <- 1
-    x21[x21 == 0] <- 1
-    x22[x22 == 0] <- 1
+    # A term is 0 if x_ij = 0 (Equation 4.8)
     T0 <- 0
-    T0 <- T0 + x11 * log(x11 / (n1p * np1 / N))
-    T0 <- T0 + x12 * log(x12 / (n1p * np2 / N))
-    T0 <- T0 + x21 * log(x21 / (n2p * np1 / N))
-    T0 <- T0 + x22 * log(x22 / (n2p * np2 / N))
+    T0 <- T0 + ifelse(x11 > 0, x11 * log(x11 / (n1p * np1 / N)), 0)
+    T0 <- T0 + ifelse(x12 > 0, x12 * log(x12 / (n1p * np2 / N)), 0)
+    T0 <- T0 + ifelse(x21 > 0, x21 * log(x21 / (n2p * np1 / N)), 0)
+    T0 <- T0 + ifelse(x22 > 0, x22 * log(x22 / (n2p * np2 / N)), 0)
     T0 <- 2 * T0
   }
 
--- a/R/Fisher_midP_test_2x2.R
+++ b/R/Fisher_midP_test_2x2.R
@@ -72,16 +72,12 @@ test_statistic_fisher_midp_test_2x2 <- function(x11, x12, x21, x22, statistic) {
     n2p <- x21 + x22
     np1 <- x11 + x21
     np2 <- x12 + x22
-    # Make sure 0 is added to T for terms where x_ij = 0 (log(1) = 0)
-    x11[x11 == 0] <- 1
-    x12[x12 == 0] <- 1
-    x21[x21 == 0] <- 1
-    x22[x22 == 0] <- 1
+    # A term is 0 if x_ij = 0 (Equation 4.8)
     T0 <- 0
-    T0 <- T0 + x11 * log(x11 / (n1p * np1 / N))
-    T0 <- T0 + x12 * log(x12 / (n1p * np2 / N))
-    T0 <- T0 + x21 * log(x21 / (n2p * np1 / N))
-    T0 <- T0 + x22 * log(x22 / (n2p * np2 / N))
+    T0 <- T0 + ifelse(x11 > 0, x11 * log(x11 / (n1p * np1 / N)), 0)
+    T0 <- T0 + ifelse(x12 > 0, x12 * log(x12 / (n1p * np2 / N)), 0)
+    T0 <- T0 + ifelse(x21 > 0, x21 * log(x21 / (n2p * np1 / N)), 0)
+    T0 <- T0 + ifelse(x22 > 0, x22 * log(x22 / (n2p * np2 / N)), 0)
     T0 <- 2 * T0
   }
 
```

With these changes and #63, the exact and mid-P values with `"LR"` equal `p_lr()` on all 17,775 2 x 2 tables with row totals 1 to 15 and no empty column. With #63 alone, 6,916 exact and 736 mid-P values still differ, and P = 1 in all 8,155 tables where x11 cannot be 0. The values with `"hypergeometric"` and `"Pearson"` do not change. The four examples above give 0.04139, 0.02318, 0.05440 and 0.05440, and the table of #60 gives 0.02176 (`p_lr()`: 0.02176071). One expectation changes: `tests/testthat/test-ch4.R`, lines 85-88 (0.04466 becomes 0.02318).
