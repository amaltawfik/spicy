# Cross-validation of `spicy::*` association measures

**Date:** 2026-05-01
**spicy version:** 0.11.0 (commit pending)
**Author:** Validation track of the audit on `R/assoc.R`

## Executive summary

Every one of the 13 association measures exported by `spicy` was
cross-checked against four independent oracles on four datasets:

| Oracle | Coverage | Verdict |
|--------|----------|---------|
| **PSPP 2.0** (`CROSSTABS /STATISTICS=ALL`) | 13/13 measures | **65 / 65** point estimates agree at 1e-7 (the precision PSPP prints) |
| **DescTools 0.99.x** (Liebetrau formulas) | 13/13 measures | 62 / 65 agree at 1e-5; 3 differ on `uncertainty_coef` (DescTools applies a Laplace correction; spicy and PSPP do not) |
| **vcd::assocstats** (SPSS convention) | 3/3 nominal measures | 9 / 9 agree at floating-point precision |
| **Hmisc::somers2** (Stata convention) | 1 measure (Somers' D) | exact match on the test case |
| **Cover & Thomas (2006), Agresti (2002)** | conventions | spicy follows the textbook `0 log 0 = 0` convention |

**Conclusion:** spicy's point estimates are **identical to SPSS / PSPP**
on all 13 measures, all 4 datasets, to PSPP's printable precision. The
only divergence found is on `uncertainty_coef`, where DescTools differs
from SPSS by applying an undocumented Laplace correction; spicy
deliberately follows the SPSS / textbook convention.

The validation also surfaced **one bug** that was fixed during the
process: `somers_d(direction = "symmetric")` previously delegated to
`kendall_tau_b()`; SPSS / PSPP defines the symmetric form as the
**harmonic mean** of the two asymmetric values, which differs from
tau-b. The fix is included in 0.11.0.

## Setup

* PSPP `2.0.0-g5b54d1` (Free Software Foundation)
* R `4.6.0` on Windows 11
* Packages: `DescTools`, `vcd`, `Hmisc` (CRAN)
* `ryouready` is no longer available for R 4.6 (archived from CRAN);
  excluded from the triangulation. Replaced by direct PSPP runs.

### Datasets

1. **mtcars 3x3** — `gear` × `cyl`, n = 32, balanced
2. **mtcars 2x2** — `vs` × `am`, n = 32
3. **HairEyeColor 4x4** — `Hair` × `Eye` (marginalised over `Sex`), n = 592
4. **sochealth** — `smoking` × `education` (package data), n = 1175

The four datasets cover: small / large n, square / rectangular shape,
2x2 / multi-level, perfectly balanced / very imbalanced, all-positive
cells / cells containing zeros.

## Convergence matrix (spicy vs PSPP)

All 65 spicy estimates match PSPP within absolute difference < 5e-8
(the rounding error of PSPP's 7-decimal CSV format).

```
                   spicy        PSPP        |diff|       OK?
mtcars_3x3
  Cramer V         0.530866    0.5308655    2.6e-09       ✓
  Contingency C    0.600387    0.6003875    2.8e-08       ✓
  Tau-b           -0.512543   -0.5125435    1.4e-08       ✓
  Tau-c           -0.483398   -0.4833984    3.8e-08       ✓
  Gamma           -0.657371   -0.6573705    1.8e-08       ✓
  Lambda sym       0.485714    0.4857143    1.4e-08       ✓
  Lambda R|C       0.529412    0.5294118    3.5e-08       ✓
  Lambda C|R       0.444444    0.4444444    4.4e-08       ✓
  GK Tau R|C       0.382560    0.3825603    3.2e-09       ✓
  GK Tau C|R       0.338602    0.3386018    2.4e-08       ✓
  U sym            0.350437    0.3504372    4.6e-08       ✓
  U R|C            0.358771    0.3587709    2.0e-08       ✓
  U C|R            0.342482    0.3424818    5.3e-10       ✓
  Somers D sym    -0.512422   -0.5124224    4.0e-08       ✓
  Somers D R|C    -0.501520   -0.5015198    4.3e-08       ✓
  Somers D C|R    -0.523810   -0.5238095    2.4e-08       ✓
mtcars_2x2
  Phi              0.168345    0.1683451    2.5e-08       ✓
  Cramer V         0.168345    0.1683451    2.5e-08       ✓
  Contingency C    0.166009    0.1660092    1.9e-09       ✓
  Tau-b            0.168345    0.1683451    2.5e-08       ✓
  Tau-c            0.164063    0.1640625    0.0e+00       ✓
  Gamma            0.333333    0.3333333    3.3e-08       ✓
  Lambda sym       0.037037    0.0370370    3.7e-08       ✓
  Lambda R|C       0.071429    0.0714286    2.9e-08       ✓
  Lambda C|R       0.000000    0.0000000    0.0e+00       ✓
  GK Tau R|C       0.028340    0.0283401    1.9e-08       ✓
  GK Tau C|R       0.028340    0.0283401    1.9e-08       ✓
  U sym            0.020831    0.0208314    1.1e-08       ✓
  U R|C            0.020682    0.0206817    1.2e-08       ✓
  U C|R            0.020983    0.0209833    6.9e-09       ✓
  Somers D sym     0.168337    0.1683367    2.7e-08       ✓
  Somers D R|C     0.170040    0.1700405    1.4e-08       ✓
  Somers D C|R     0.166667    0.1666667    3.3e-08       ✓
HairEye
  Cramer V         0.279045    0.2790446    2.3e-08       ✓
  Contingency C    0.435159    0.4351585    3.9e-08       ✓
  Tau-b            0.224703    0.2247026    3.1e-09       ✓
  Tau-c            0.204689    0.2046886    4.1e-08       ✓
  Gamma            0.317772    0.3177721    3.4e-08       ✓
  Lambda sym       0.143068    0.1430678    4.7e-08       ✓
  Lambda R|C       0.032680    0.0326797    3.9e-08       ✓
  Lambda C|R       0.233871    0.2338710    3.2e-08       ✓
  GK Tau R|C       0.074609    0.0746087    1.5e-08       ✓
  GK Tau C|R       0.113638    0.1136376    4.1e-08       ✓
  U sym            0.098420    0.0984203    2.0e-08       ✓
  U R|C            0.099231    0.0992313    1.8e-09       ✓
  U C|R            0.097622    0.0976225    1.1e-08       ✓
  Somers D sym     0.224677    0.2246768    2.2e-08       ✓
  Somers D R|C     0.221322    0.2213218    6.8e-09       ✓
  Somers D C|R     0.228135    0.2281350    4.4e-08       ✓
sochealth
  Cramer V         0.135668    0.1356677    1.9e-08       ✓
  Contingency C    0.134436    0.1344361    2.5e-08       ✓
  Tau-b           -0.126415   -0.1264155    4.7e-08       ✓
  Tau-c           -0.116921   -0.1169210    4.0e-08       ✓
  Gamma           -0.268068   -0.2680678    7.4e-09       ✓
  Lambda sym       0.000000    0.0000000    0.0e+00       ✓
  Lambda R|C       0.000000    0.0000000    0.0e+00       ✓
  Lambda C|R       0.000000    0.0000000    0.0e+00       ✓
  GK Tau R|C       0.018406    0.0184057    2.0e-08       ✓
  GK Tau C|R       0.007609    0.0076095    4.0e-08       ✓
  U sym            0.011488    0.0114876    1.6e-08       ✓
  U R|C            0.017512    0.0175123    4.0e-10       ✓
  U C|R            0.008547    0.0085472    3.1e-08       ✓
  Somers D sym    -0.120008   -0.1200077    2.8e-08       ✓
  Somers D R|C    -0.091307   -0.0913067    2.1e-08       ✓
  Somers D C|R    -0.175024   -0.1750241    3.0e-08       ✓
```

## spicy vs DescTools

62 / 65 estimates agree at 1e-5 or better. Three disagree, all on
`uncertainty_coef` on `mtcars_3x3` (the dataset that has a zero cell):

| Direction | spicy     | DescTools | |diff|     |
|-----------|-----------|-----------|------------|
| symmetric | 0.350437  | 0.350146  | 2.9e-04    |
| R\|C      | 0.358771  | 0.358471  | 3.0e-04    |
| C\|R      | 0.342482  | 0.342198  | 2.8e-04    |

**Cause identified:** `DescTools::UncertCoef()` replaces every zero
cell with `1/sum(x)^2` (a Laplace-style correction) before computing
entropies — see lines 5-7 of its source:

```r
function (x, ..., p.zero.correction = 1/sum(x)^2, ...) {
  ...
  x[x == 0] <- p.zero.correction
  ...
}
```

spicy uses the standard mathematical convention `0 log 0 = 0`, which
matches:

* PSPP / SPSS `CROSSTABS` (verified)
* Stata `tab2` add-ons (`unctt`, `nclass`)
* R's `entropy::entropy.empirical()`
* Cover & Thomas (2006), *Elements of Information Theory*, eq. 2.2
* MacKay (2003), *Information Theory, Inference, and Learning Algorithms*

DescTools' Laplace correction is undocumented in the function help and
not endorsed by the major information-theory references. spicy's
documentation now states the convention explicitly so users can choose.

**Verdict:** spicy = SPSS/PSPP. DescTools is the outlier.

## spicy vs vcd::assocstats

All 9 nominal estimates (Cramer V, Phi, Contingency C × 4 datasets;
Phi only when 2x2) agree to floating-point precision (max diff
5.6e-17). vcd uses the SPSS convention, so this is consistent with the
PSPP comparison.

## spicy vs Hmisc::somers2

`Hmisc::somers2()` takes a continuous predictor and a binary outcome,
not a contingency table. We expanded `mtcars_2x2` into long form and
compared:

```
Hmisc::somers2 (Dxy)         : 0.166667
spicy::somers_d(C|R)         : 0.166667
PSPP                         : 0.166667
```

Exact match. (Stata's `somersd` package is the reference implementation
that `Hmisc::somers2` mirrors.)

## Bug found and fixed during validation

`somers_d(x, direction = "symmetric")` used to return
`kendall_tau_b(x)`. SPSS / PSPP define the symmetric Somers' D as
the **harmonic mean** of the two asymmetric values:

```
D_sym = 2 * D_R|C * D_C|R / (D_R|C + D_C|R)
```

For `mtcars_3x3`:

* old spicy (= tau-b): `-0.5125435`
* new spicy (= harmonic mean): `-0.5124224`
* PSPP: `-0.5124224` ✓

The two quantities are close on square tables (the tau-b denominator
is the geometric mean of the somers denominators) but conceptually
distinct. The fix is in 0.11.0; the old test was rewritten and an
anti-regression entry was added against PSPP.

## Reference values from R help / textbooks

In addition to PSPP, `DescTools` ships canonical examples lifted from
Agresti (2002) in its function help pages. We verified spicy reproduces
those values (e.g. `?DescTools::GoodmanKruskalGamma` example matches
`gamma_gk()` to 1e-6). Only PSPP-derived values are pinned in the test
suite (`tests/testthat/test-assoc.R`) since they are the strongest
oracle.

## Conventions explicitly documented in 0.11.0

* `uncertainty_coef()`: entropy uses `0 log 0 = 0`; explicit comparison
  with DescTools' Laplace correction in the `@details` section.
* `cramer_v()`, `phi()`: CI uses Fisher z-transformation (the point
  estimate matches DescTools / SPSS; the CI methodology differs).
* `somers_d(direction = "symmetric")`: harmonic mean of asymmetric
  values (SPSS / PSPP definition), not tau-b.

## Caveats

* PSPP CSV output is limited to 7 absolute decimal places. For
  measures with values < 0.01, this means oracle precision is
  effectively 1e-5, not 1e-7. The unit-test tolerance is set to
  1e-5 accordingly.
* `ryouready` (an explicit SPSS-clone R package, archived from CRAN
  for R 4.6) was excluded; PSPP fills its role.
* `Hmisc::somers2` was tested only on the 2x2 case; it requires a
  binary outcome and so is not directly applicable to 3x3+ tables.
* Asymptotic standard errors and confidence intervals were not
  exhaustively cross-checked against PSPP — only point estimates.
  The audit (see `R/assoc.R` docstrings) discusses the SE / CI
  conventions per measure.

## Reproducibility

The full pipeline lives in `tools/validation/` (versioned but
excluded from the CRAN build via `.Rbuildignore`):

* `gen_sps.R` — generates `.sps` syntax for the four datasets
* `*.sps` — PSPP CROSSTABS syntax (frozen inputs)
* `*_pspp.csv` — PSPP outputs (frozen oracle values)
* `validate.R` — multi-package R triangulation
* `compare_pspp.R` — builds the spicy-vs-PSPP convergence matrix
* `spicy_vs_pspp.csv` — flat numerical comparison snapshot

See `tools/validation/README.md` for the reproduction recipe.
