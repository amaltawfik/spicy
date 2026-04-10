## R CMD check results

0 errors | 0 warnings | 2 notes on R-oldrelease only

* On R-oldrelease (4.4.3), CRAN's spell checker flags "bivariate" in `DESCRIPTION` as a possibly misspelled word. This is an ordinary statistical term used in the package description.
* On R-oldrelease (4.4.3), a NOTE reports that the Author field differs from Authors@R. This is caused by the `ROR` (Research Organization Registry) identifier in the `comment` field of `Authors@R`, which older R versions do not fully expand into the plain-text Author field. The package metadata is correct; the discrepancy is cosmetic and does not appear on R-release or R-devel.

## Comments

This is version 0.8.0 of the **spicy** package.

### Main changes

* `table_continuous_lm()` is a new helper for article-style bivariate linear-model tables for continuous outcomes. It complements `table_continuous()` with a model-based workflow for fitted mean comparisons or slopes, optional case weights, classical or HC0-HC5 variance estimators, and multiple output formats (ASCII, tinytable, gt, flextable, Excel, clipboard, and Word).

* Installed package vignettes now avoid embedding heavy HTML table and codebook widgets during CRAN builds, which reduces installed size and resolves the previous installed-package-size NOTE while preserving rich pkgdown article rendering.

* The website and vignettes now document `table_continuous_lm()` directly, with a dedicated article and consistent `sochealth` examples across the summary-table documentation.

### Testing environments

* Windows 11 (local), R 4.5.3
* GitHub Actions: ubuntu-latest (R release), windows-latest (R release), macOS-latest (R release)
* win-builder: R-release, R-devel, R-oldrelease
* R-hub: linux (R-devel)
