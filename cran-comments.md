## R CMD check results

0 errors | 0 warnings | 0 notes

## Comments

This is version 0.2.1 of the `spicy` package, a patch release following 0.2.0.

This release fixes a bug in the newly introduced `label_from_names()` function:

* Correct handling of column names with missing or malformed separators.
* Improved robustness for cases where labels are empty or contain the separator itself.

No changes to the public API or package dependencies.
R CMD check passes cleanly on local (macOS) and win-builder (release, devel, oldrelease).
