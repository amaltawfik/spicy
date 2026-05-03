## Test environments

* Local: Windows 11, R 4.6.0
* GitHub Actions: ubuntu-latest, windows-latest, macOS-latest (R release)
* win-builder: R-release, R-devel, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

spicy has no reverse dependencies.

## Notes

Update of spicy 0.10.0. Adds substantial new functionality to
`table_continuous_lm()` (cluster-robust / bootstrap / jackknife
variance, three new effect-size families) and harmonises the
table-family reporting vocabulary. Full details, breaking changes
with migration recipes, and cross-software validation results
(PSPP 2.0, `effectsize`, `DescTools`) are documented in `NEWS.md`.
