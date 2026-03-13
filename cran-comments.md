## R CMD check results

0 errors | 0 warnings | 0 notes

## Comments

This is version 0.5.0 of the **spicy** package.

### Main changes

* Added new `table_apa()` helper to build APA-ready cross-tab reports with multiple output formats (`wide`, `long`, `tinytable`, `flextable`, `excel`, `clipboard`, `word`).
* Added key `cross_tab()` controls to `table_apa()`: `weights`, `rescale`, `correct`, `simulate_p`, and `simulate_B`.
* Improved handling of missing values in `table_apa()` when `drop_na = FALSE`.

### Bug fixes and improvements

* Fixed `cross_tab()` rescale warning firing incorrectly for explicit all-ones weights.
* Fixed `count_n()` crash when `special = "NaN"` is used with non-numeric columns.
* Fixed `table_apa()` leaking global options on error (now protected with `on.exit()`).
* `freq()` now assigns proper S3 class and dispatches printing correctly.
* Removed unused `collapse` and `stringi` from `Imports`.
* `copy_clipboard()` parameter `message` renamed to `show_message`.
* Documentation harmonized: all `@param` entries now document default values.

There are no breaking changes for users besides the `copy_clipboard(message=)` rename to `show_message`.

### Testing environments
