# surveytable 0.10.0 : svyciprop_adjusted() à une proportion de 1, tab() quand l'erreur type est nulle

- **Cible** : surveytable 0.10.0 (NCHS ; `master` b4130cf identique sur ces lignes)
- **Canal** : issue GitHub [CDCgov/surveytable#2](https://github.com/CDCgov/surveytable/issues/2)
- **Posté** : 2026-10-21
- **Statut** : ouvert
- **Côté spicy** : non exposé. `table_categorical_svy()` applique la règle de Korn-Graubard à 0 et à 100 % par son propre code (ADR 2026-10-20-survey-bounds-design-columns) et n'appelle pas surveytable.

---

*Dossier tel que posté, conservé verbatim ci-dessous.*

# svyciprop_adjusted() returns NaN at a proportion of 1, and tab() shows a zero-width interval when SE = 0
surveytable 0.10.0 from CRAN, survey 4.5, R 4.6.1. The code quoted below is the same on `master` (b4130cf).

## 1. svyciprop_adjusted() returns NaN at a proportion of 1

### Steps to reproduce

```r
library(surveytable)  # 0.10.0
male <- subset(namcs2019sv, SEX == "Male")  # 3,641 visits
svyciprop_adjusted(~MAMMO, male, adj = "NCHS")
svyciprop_adjusted(~I(!MAMMO), male, adj = "NCHS")
```

### Actual behavior

```r
#>                  2.5%   97.5%
#> MAMMO 0.00000 0.00000 0.00101
#>               2.5% 97.5%
#> I(!MAMMO)   1  NaN   NaN
```

`adj = "NHIS"` gives the same NaN.

### Expected behavior

The NCHS standards treat 0 events and its complement alike (Parker et al. 2017, *NCHS Data Presentation Standards for Proportions*, p. 3): "If the number of numerator events is 0 or equal to the denominator (the complement of 0 events), the estimated proportion will be 0 or 1, respectively. As a result, the estimated variance of the proportion will be 0, and the effective sample size will be undefined. In these cases, the sample size should be used to determine whether the minimum sample size criterion is met, and it should also be used for CIs and other computations that include the effective sample size." The NCHS SAS macro `KG_macro.sas` (https://wwwn.cdc.gov/nchs/data/Tutorials/Code/KG_macro.sas, lines 495-509) uses the sample size at p = 0 and at p = 1. With n = 3,641, the interval at 1 is the mirror of the one at 0:

```r
binom.test(3641, 3641)$conf.int
#> [1] 0.9989874 1.0000000
```

### Cause

In `R/svyciprop_adjusted.R`, `n.eff` is p (1 - p) / var, computed by the helper at line 137; at p = 1 it is 0/0 = NaN. Lines 108-112 replace it by `nrow(design)` only when `rval` is 0, and `min(nrow(design), NaN)` is NaN.

### Suggested fix

```diff
-  if (rval > 0) {  #Adjusted effective sample size
+  if (rval > 0 && rval < 1) {  #Adjusted effective sample size
```

With this change, the example gives 1 (0.999 to 1.000), the interval of `binom.test()`, with `adj = "NCHS"` and with `adj = "NHIS"`. With survey's `data(api)`, in the domain of the 100 elementary schools of `apistrat`, the proportion of elementary schools, which is 1, gives 0.9637833 to 1. The intervals at p = 0 do not change, nor do those of the 633 proportions strictly between 0 and 1 that I computed in the domains of `SEX`, `AGER` and `MSA` in `namcs2019sv`, nor the 11 `tab()` tables I compared.

## 2. tab() replaces the interval by [p, p] when SE = 0

### Steps to reproduce

```r
library(survey)
data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat)
set_survey(dstrat)
set_opts(adj = "NCHS")
tab("stype")
svyciprop_adjusted(~I(stype == "E"), dstrat, adj = "NCHS")
```

### Actual behavior

```r
#> │ Level │   n │ Number │ SE │    LL │    UL │ Percent │ SE │   LL │   UL │
#> │ E     │ 100 │  4,421 │  0 │ 4,421 │ 4,421 │    71.4 │  0 │ 71.4 │ 71.4 │
#> │ H     │  50 │    755 │  0 │   755 │   755 │    12.2 │  0 │ 12.2 │ 12.2 │
#> │ M     │  50 │  1,018 │  0 │ 1,018 │ 1,018 │    16.4 │  0 │ 16.4 │ 16.4 │
#>                        2.5% 97.5%
#> I(stype == "E") 0.714 0.646 0.775
```

### Expected behavior

The strata are the school types, so the percentages have SE 0 and an infinite effective sample size. Parker et al. (2017, p. 3): "When the effective sample size is greater than the sample size, the sample size should be used to determine whether the minimum sample size criterion is met, and it should also be used for CIs and other computations that include the effective sample size." `svyciprop_adjusted()` does this (`n.eff` = 200) and returns 0.646 to 0.775. `tab()` prints 71.4 to 71.4.

### Cause

Lines 241-244 of `R/tab.R` set LL = UL = Proportion whenever SE = 0. The rule came with c6e1a8f ("Fixed % CI error when SE = 0"). With `adj = "none"`, `survey::svyciprop(method = "beta")` returns 0.500 to 0.500 in this case: `n.eff` is infinite and `qbeta()` with infinite shapes returns 0.5. That interval does not contain the estimate, and the rule avoids the assertion of line 245. With `adj = "NCHS"` the interval is valid, and the rule replaces it as well.

### Suggested fix

Keep the rule for intervals that do not contain the estimate:

```diff
-		if (ret1$SE == 0) {
+		if (ret1$SE == 0 && !(ci$LL <= ret1$Proportion && ret1$Proportion <= ci$UL)) {
```

With this change, `tab("stype")` prints the intervals of `svyciprop_adjusted()` with `adj = "NCHS"` (64.6 to 77.5, 8 to 17.5 and 11.6 to 22.3) and the same table as before with `adj = "none"`. The 11 `tab()` tables I compared do not change.
