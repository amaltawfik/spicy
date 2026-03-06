## R CMD check results

0 errors | 0 warnings | 0 notes

## Comments

This is version 0.4.2 of the **spicy** package.

### Main changes

* `cross_tab()` hardening: improved vector-mode detection (including labelled vectors), stricter weight validation, safer rescaling, and clearer early errors.
* `cross_tab()` statistics are now computed on non-empty margins in grouped tables, avoiding spurious `NA` results.
* Internal refactor of `cross_tab()` core path to remove `dplyr`/`tibble` from the computation path while preserving API and behavior.
* Robustness improvements in `freq()`, `count_n()`, `mean_n()`, `sum_n()`, `label_from_names()`, and `cramer_v()` for edge cases and clearer diagnostics.
* Dependency optimization: `DT` and `clipr` moved to `Suggests`, with optional runtime checks added where needed.
* Regression tests expanded for the above cases.

There are no breaking changes to the public API.

### Testing environments

Checks were run on:

* Local Windows (R 4.5.2)
* GitHub Actions (R-CMD-check)
* R-hub (linux, windows)
