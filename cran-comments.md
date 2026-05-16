## Test environments

* Local: Windows 11, R 4.6.0
* GitHub Actions: ubuntu-latest, windows-latest, macOS-latest
  (R-release)
* win-builder: R-release, R-devel, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 1 NOTE on win-builder.

The NOTE is the standard CRAN incoming-feasibility check
("Days since last update", "Number of updates in past 6
months"). No other notes, warnings, or errors on any tested
platform.

## Reverse dependencies

spicy has no reverse dependencies.

## Notes on this submission

This release adds the `table_regression()` function for
publication-ready regression coefficient tables and an additive
`covariates` argument in `table_continuous_lm()`. It also bundles
a focused round of bug fixes and one opt-in breaking change
(`code_book()` no longer silently truncates very long export
filenames). Full details in `NEWS.md`.

The release frequency exceeds the soft CRAN target of one update
per ~2 months. The cadence reflects the rapid build-out of the
table-rendering layer (eight output engines, four standardisation
methods, cluster-robust + Satterthwaite-corrected inference for
average marginal effects) ahead of the 1.0 freeze, not bug-driven
churn. Each release passes `R CMD check --as-cran` clean and is
covered by 1500+ unit tests.
