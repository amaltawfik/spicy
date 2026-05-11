## Test environments

* Local: Windows 11, R 4.6.0
* GitHub Actions: ubuntu-latest, windows-latest, macOS-latest (R release)
* win-builder: R-release, R-devel, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 1 NOTE on win-builder (R-release, R-devel,
R-oldrelease):

> Days since last update: 7
> Number of updates in past 6 months: 10

This is the standard CRAN incoming-feasibility check, addressed
below; no other notes, warnings or errors on any tested platform.

## Reverse dependencies

spicy has no reverse dependencies.

## Notes

This 0.12.0 release ships two user-visible features:

* New `table_regression()` -- publication-ready regression
  coefficient tables for one or more `lm` or `glm` fits side by
  side. Covers classical / HC* / cluster-robust (with
  Satterthwaite-corrected df) / bootstrap / jackknife variance,
  five standardisation methods (including Menard 2011 fully-
  standardised for `glm`), family-aware `exponentiate` (OR / IRR
  / HR / RR / MR header rebrand), Wald or profile-likelihood CIs,
  average marginal effects via `marginaleffects` (with
  Satterthwaite df under cluster-robust variance, a first in R),
  partial *f²* / *η²* / *ω²* / *χ²* effect sizes, McFadden /
  Nagelkerke / Tjur pseudo-*R²*, hierarchical comparison footers,
  multiple-comparison adjustment via `stats::p.adjust()`, regex
  `keep` / `drop` filters, and the same console / gt / tinytable
  / flextable / Excel / Word / clipboard outputs as the rest of
  the package.

* Additive covariate adjustment in `table_continuous_lm()` with
  two emmean estimands (`"proportional"` G-computation, matching
  Stata `margins` / `marginaleffects::avg_predictions(variables)`;
  `"balanced"` synthetic-grid, matching `emmeans::emmeans()`
  default / SPSS UNIANOVA / SAS LSMEANS) and partial *f²* / *ω²*
  dispatch under adjustment.

Plus a focused round of bug fixes (full list in `NEWS.md`) and
one opt-in breaking change: `code_book()` no longer silently
truncates the export filename to 120 characters (migration
recipe in `NEWS.md`).

Cross-software validation against PSPP 2.0, `parameters`,
`performance`, `MASS::confint.glm`, `effectsize`, `emmeans`, and
`marginaleffects` documented in `NEWS.md` and the function-
specific vignettes.
