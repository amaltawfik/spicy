## R CMD check results

0 errors | 0 warnings | 1-2 notes

* The spelling NOTE flags "Cramer's", "Kruskal", and "Somers" as possibly misspelled words in DESCRIPTION. All are proper names of statistical measures (Cramer's V, Goodman-Kruskal Gamma, and Somers' D).
* On R-oldrelease (4.4.3), a NOTE reports that the Author field differs from Authors@R. This is caused by the `ROR` (Research Organization Registry) identifier in the `comment` field of `Authors@R`, which older R versions do not fully expand into the plain-text Author field. The package metadata is correct; the discrepancy is cosmetic and does not appear on R-release or R-devel.

## Comments

This is version 0.7.1 of the **spicy** package.

### Main changes

* Installed package vignettes now avoid embedding heavy HTML table and codebook widgets during CRAN builds, which reduces installed size and resolves the previous installed-package-size NOTE while preserving rich pkgdown article rendering.

### Testing environments

* Windows 11 (local), R 4.5.3
* GitHub Actions: ubuntu-latest (R release), windows-latest (R release), macOS-latest (R release)
* win-builder: R-release, R-devel, R-oldrelease
* R-hub: linux (R-devel)
