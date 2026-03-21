## R CMD check results

0 errors | 0 warnings | 1-2 notes

* The spelling NOTE flags "Cramer's", "Kruskal", and "Somers" as possibly misspelled words in DESCRIPTION. All are proper names of statistical measures (Cramer's V, Goodman-Kruskal Gamma, and Somers' D).
* On R-oldrelease (4.4.3), an additional NOTE reports that the Author field differs from Authors@R. This is caused by the `ROR` identifier in the `comment` field, which older R versions do not parse. The NOTE does not appear on R-release or R-devel.

## Comments

This is version 0.6.0 of the **spicy** package.

### Main changes

* New family of association measure functions for contingency tables: `assoc_measures()`, `contingency_coef()`, `gamma_gk()`, `goodman_kruskal_tau()`, `kendall_tau_b()`, `kendall_tau_c()`, `lambda_gk()`, `phi()`, `somers_d()`, `uncertainty_coef()`, and `yule_q()`.
* `cross_tab()` gains `assoc_measure` and `assoc_ci` arguments with automatic ordinal detection.
* `table_apa()` gains `output = "gt"` and dynamic association measure labeling.
* New bundled dataset `sochealth` (simulated social-health survey, n = 1200, 24 variables).
* `cramer_v()` gains a `detail` argument (breaking: returns a named vector when `detail = TRUE`).

### Bug fixes

* `count_n()` NA-in-count bug fixed; list columns reported in verbose mode.
* `cross_tab()` rescale logic now operates on complete cases only.
* `freq()` consistent NA representation, validated `digits`/`sort`, correct `cum_valid_prop` for missing rows.
* `mean_n()`, `sum_n()` now validate `min_valid` and `digits` arguments.
* `mean_n()`, `sum_n()`, `count_n()` no longer trigger a tidyselect deprecation warning with character vector input.
* `table_apa()` preserves factor level order, places `(Missing)` at bottom, validates digit arguments.
* `table_apa()` p-values no longer wrap across lines in `tinytable` HTML output.

### Testing environments

* Windows 11 (local), R 4.5.3
* GitHub Actions: ubuntu-latest (R release), windows-latest (R release), macOS-latest (R release)
* win-builder: R-release, R-devel, R-oldrelease
* R-hub: linux, windows
