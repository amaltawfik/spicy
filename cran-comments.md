## Test environments

* Local: Windows 11, R 4.6.0
* GitHub Actions: ubuntu-latest, windows-latest, macOS-latest (R release)
* win-builder: R-release, R-devel, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 1 NOTE on win-builder (R-release, R-devel,
R-oldrelease):

> Days since last update: 6
> Number of updates in past 6 months: 8

This is the standard CRAN incoming-feasibility check, addressed
below; no other notes, warnings or errors on any tested platform.

## Reverse dependencies

spicy has no reverse dependencies.

## Notes

This 0.11.0 release consolidates several months of pre-1.0 work
into one substantial update -- cluster-robust / bootstrap /
jackknife variance in `table_continuous_lm()`, harmonised
vocabulary across the table family, and classed `spicy_error` /
`spicy_warning` hierarchies. The previous 0.x.y releases addressed
a mix of incremental features and urgent fixes as the public API
took shape; 0.11.0 is the planned consolidation point and I expect
a longer interval before 0.12.0.

Full details, breaking changes with migration recipes, and
cross-software validation results (PSPP 2.0, `effectsize`,
`DescTools`) are documented in `NEWS.md`.
