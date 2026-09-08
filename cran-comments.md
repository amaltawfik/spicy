# cran-comments for spicy 0.13.0

## Test environments

* Local: Windows 11, R 4.6.1
* GitHub Actions: windows-latest (R-release), macOS-latest
  (R-release), ubuntu-latest (R-release, R-devel, R-oldrel-1)
* win-builder: R-release, R-devel

## R CMD check results

0 errors | 0 warnings | 1 NOTE

The NOTE is the standard CRAN incoming-feasibility check ("Number of
updates in past 6 months"). No other notes, warnings, or errors on
any tested platform.

## Reverse dependencies

spicy has no reverse dependencies.

## Notes on this submission

This release consolidates a full development cycle; the package has
moved to a slower release cadence. `table_regression()` grows from
`lm` / `glm` to more than thirty model classes, the summary tables
gain survey-design twins and a univariable screen, six journal
styles and a French output arrive with document-wide options, and
declared missing values (`na_values` / `na_range`, tagged NAs) are
honored across the descriptive functions. The package now ships a
single vignette; the twenty walk-throughs live as articles on the
package site. Full details in `NEWS.md`.

The test suite has 18,000+ unit tests, with estimates and standard
errors validated to the digit against independent implementations
(SPSS/PSPP, Stata conventions, and the reference R packages of each
model family).
