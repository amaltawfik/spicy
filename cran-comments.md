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

This 0.12.0 release is targeted: it ships a single user-visible
feature (additive covariate adjustment in `table_continuous_lm()`
with two emmean estimands -- `"proportional"` G-computation and
`"balanced"` synthetic-grid -- plus partial *f²* / *ω²* dispatch
under adjustment) and a focused round of bug fixes uncovered by
the post-0.11.0 critical-read pass:

* `count_n(special = ...)` length-`nrow(data)` contract restored
  when no usable column survives the list-column filter.
* `table_categorical()` *p*-value rendering at `p_digits >= 4` no
  longer prints `"<.0001"` for a true value above the displayed
  threshold (e.g. `p = 0.000108` now correctly prints as `".0001"`
  at `p_digits = 4`).
* `lambda_gk()` and `goodman_kruskal_tau()` defended against
  rank-1 contingency tables (constant predicted variable) -- now
  emit `spicy_undefined_stat` and return the fully `NA`-shaped
  result instead of silent NaN / unguarded `if (NA)`.
* `cross_tab()` auto-rename pattern extended to detect collisions
  with the `"Values"` y-level (companion to the 0.11.0 `"N"` /
  `"Total"` row-level fix).
* `table_continuous_lm(covariates = ...)` now rejects covariate
  column names `"x"` or `"y"` reserved by the internal model fit.
* `broom::glance()` on a `spicy_continuous_lm_table` keeps
  `df.residual` numeric so Satterthwaite df from cluster-robust
  modes are preserved verbatim.

One opt-in breaking change: `code_book()` no longer silently
truncates the export filename to 120 characters. Migration is
documented in `NEWS.md` (shorten very long titles or pass an
explicit `filename =`).

Full details, breaking-change migration recipes, and
cross-software validation results (PSPP 2.0, `effectsize`,
`emmeans`, `marginaleffects`) are documented in `NEWS.md`.
