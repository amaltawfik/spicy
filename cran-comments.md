## R CMD check results

0 errors | 0 warnings | 1-2 notes

* The spelling NOTE flags "Cramer's", "Kruskal", and "Somers" as possibly misspelled words in DESCRIPTION. All are proper names of statistical measures (Cramer's V, Goodman-Kruskal Gamma, and Somers' D).
* On R-oldrelease (4.4.3), a NOTE reports that the Author field differs from Authors@R. This is caused by the `ROR` (Research Organization Registry) identifier in the `comment` field of `Authors@R`, which older R versions do not fully expand into the plain-text Author field. The package metadata is correct; the discrepancy is cosmetic and does not appear on R-release or R-devel.

## Comments

This is version 0.7.0 of the **spicy** package.

### Main changes

* `table_categorical()` replaces `table_apa()` as the public helper for categorical summary tables, with `select`/`by` interface, one-way frequency support, and consolidated `output` argument. **Breaking:** migrate `table_apa()` calls to `table_categorical()`.
* New `table_continuous()` helper for continuous summary tables with descriptive statistics, tidyselect column selection, grouping via `by`, group-comparison tests (`test`), effect sizes, and multiple output formats (ASCII, tinytable, gt, flextable, Excel, clipboard, Word).
* ASCII console tables now split oversized outputs into stacked horizontal panels for narrow consoles.
* Excel export now uses `openxlsx2` (replacing `openxlsx`) for a lighter dependency footprint.

### Testing environments

* Windows 11 (local), R 4.5.3
* GitHub Actions: ubuntu-latest (R release), windows-latest (R release), macOS-latest (R release)
* win-builder: R-release, R-devel, R-oldrelease
* R-hub: linux (R-devel)
