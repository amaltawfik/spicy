## R CMD check results

0 errors | 0 warnings | 0 notes

## Comments

This is version 0.5.0 of the **spicy** package.

### Main changes

* Added new `table_apa()` helper to build APA-ready cross-tab reports.
* `table_apa()` supports multiple output formats: `wide`, `long`, `tinytable`, `flextable`, `excel`, `clipboard`, and `word`.
* Added key `cross_tab()` controls to `table_apa()`: `weights`, `rescale`, `correct`, `simulate_p`, and `simulate_B`.
* Improved handling of missing values in `table_apa()` when `drop_na = FALSE`.
* Added documentation, examples, and tests for the new functionality.

There are no breaking changes for users.

### Testing environments
