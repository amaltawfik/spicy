## Test environments

* Local: Windows 11, R 4.6.0
* GitHub Actions: ubuntu-latest, windows-latest, macOS-latest
  (R-release)
* win-builder: R-release, R-devel, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 1 NOTE on win-builder.

The NOTE is the standard CRAN incoming-feasibility check
("Number of updates in past 6 months: 9"). No other notes,
warnings, or errors on any tested platform.

## Reverse dependencies

spicy has no reverse dependencies.

## Notes on this submission

This release adds the `table_regression()` function for
publication-ready regression coefficient tables (`lm` and `glm`
fits side by side, multiple standardisation methods, average
marginal effects, partial effect sizes with noncentral CIs,
hierarchical model comparisons, and the same output engines as
the rest of the package) and an additive `covariates` argument
in `table_continuous_lm()` (G-computation and equal-weight
marginal-mean estimands). It also bundles a focused round of bug
fixes and one opt-in breaking change (`code_book()` no longer
silently truncates very long export filenames). Full details in
`NEWS.md`.

The release frequency reflects the rapid build-out of the
table-rendering layer ahead of the 1.0 freeze, not bug-driven
churn. Each release passes `R CMD check --as-cran` clean on all
three win-builder R lines (R-release, R-devel, R-oldrelease) and
the test suite has 3600+ unit tests.
