## R CMD check results

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
  Number of updates in past 6 months: 7

* On R-oldrelease (4.4.3), a NOTE reports that the Author field differs
  from Authors@R. This is caused by the ROR (Research Organization
  Registry) identifier in the `comment` field of `Authors@R`, which older R
  versions do not fully expand into the plain-text Author field. The package
  metadata is correct; the discrepancy is cosmetic.

This release contains user-facing fixes and documentation improvements for
variable inspection and summary-table workflows. The additional submission is
intended to make the package behavior clearer and more robust before broader
use of version 0.9.0.

## Comments

This is version 0.9.0 of the **spicy** package.

### Main changes

* `table_continuous()` now enables inferential output by default when `by` is supplied: the `p` column from the chosen group-comparison test is shown automatically, aligning the function with `table_continuous_lm()`'s inferential default. Previous behavior is available via `p_value = FALSE`. `statistic` and `effect_size` remain `FALSE` by default.

* `varlist()` and `vl()` now display observed factor levels by default in `Values`, with `factor_levels = "all"` available to include unused levels. `code_book()` gains the same argument but defaults to `"all"` so exported codebooks continue to document all declared factor levels.

* `varlist()` now preserves literal `"NA"` and empty-string values, distinguishes actual `NA` values from `NaN` when `include_na = TRUE`, preserves factor level order, and validates logical arguments up front with clear error messages.

* `freq()` gains two bug fixes: qualified weight expressions such as `weights = other$w` are now always evaluated in the calling environment, so a column-name collision with `data` can no longer pull the weight vector from the wrong data frame; and missing-value rows are kept at the end of the tabulation after `sort`, so the printed `Cum. Percent` and `Cum. Valid Percent` columns remain monotonic and match the Valid → Missing → Total layout.

* `freq()` now prints the `Freq.` column as integers regardless of `digits` (which continues to control percentage precision), matching the convention of SPSS, Stata, and SAS `PROC FREQ` for weighted counts. `freq(..., styled = FALSE)` also returns a genuinely plain `data.frame` with no rendering metadata clinging to it, so programmatic consumers see only the tabulation columns.

* `table_continuous_lm()` documentation clarifies why `p_value = TRUE` and `r2 = "r2"` are the default display, and `compute_lm_vcov()` now warns explicitly when a robust variance estimator falls back to the classical OLS variance on a singular model matrix.

### Testing environments

* Windows 11 (local), R 4.5.3
* GitHub Actions: ubuntu-latest (R release), windows-latest (R release), macOS-latest (R release)
* win-builder: R-release, R-devel, R-oldrelease
* R-hub: linux (R-devel)
