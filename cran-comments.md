## R CMD check results

0 errors | 0 warnings | 0 notes

## Comments

This is version 0.5.0 of the **spicy** package.

### Main changes

* Added new `table_apa()` helper to build APA-ready cross-tab reports with multiple output formats (`wide`, `long`, `tinytable`, `flextable`, `excel`, `clipboard`, `word`).
* Added key `cross_tab()` controls to `table_apa()`: `weights`, `rescale`, `correct`, `simulate_p`, and `simulate_B`.
* Improved handling of missing values in `table_apa()` when `drop_na = FALSE`.

### Bug fixes and improvements

* Fixed `count_n()` crash when `special = "NaN"` is used with non-numeric columns.
* Fixed `cross_tab()` spurious rescale warning for explicit all-ones weights.
* Fixed `table_apa()` leaking global options on error.
* `varlist()` title generation no longer crashes on unrecognizable expressions.
* `freq()` now dispatches printing correctly via S3.
* `copy_clipboard()` parameter `message` renamed to `show_message`.
* Removed unused `collapse` and `stringi` from `Imports`.

There are no breaking changes besides the `copy_clipboard(message=)` rename to `show_message`.

### Testing environments

* Windows 11 (local), R 4.5.3
* GitHub Actions: ubuntu-latest (R release), windows-latest (R release), macOS-latest (R release)
