# cran-comments for spicy 0.14.0

## Test environments

* Local: Windows 11, R 4.6.1
* Local, without the suggested packages
  (`_R_CHECK_DEPENDS_ONLY_=true`): Windows 11, R 4.6.1
* GitHub Actions: windows-latest (R-release), macOS-latest
  (R-release), ubuntu-latest (R-release, R-devel, R-oldrel-1)
* win-builder: R-release, R-devel, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 1 NOTE

The NOTE comes from the spell check of the CRAN incoming-feasibility
step (win-builder, R-devel and R-release):

    Possibly misspelled words in DESCRIPTION:
      JAMA, NEJM, univariable

All three are spelled as intended: JAMA and NEJM name reporting styles
(Journal of the American Medical Association, New England Journal of
Medicine), and "univariable" is the standard term. The published 0.13.0
carried the same NOTE. No other notes, warnings, or errors on any
tested platform.

## Reverse dependencies

spicy has no reverse dependencies.

## Notes on this submission

This release follows 0.13.0 by five weeks, within the slower cadence
the package adopted in 2026. It corrects an error of 0.13.0: the robust
and cluster-robust standard errors of `mgcv::gam()` and `mgcv::bam()`
fits were wrong for families with a free dispersion (divided by it), so
the package now forms that sandwich itself. The release also makes
`code_book()` a deliverable codebook (an object, an Excel workbook, or a
PDF), adds the multiple-comparison adjustment to the descriptive summary
tables, supports cumulative-link mixed models and cluster-robust
standard errors for `glmmTMB` fits, and completes the French output.
Full details in `NEWS.md`.

The full test suite has 19,900+ expectations, with estimates and
standard errors validated to the digit against independent
implementations (SPSS/PSPP, Stata conventions, and the reference R
packages of each model family). It runs on GitHub Actions on every
push, across five platforms. To keep the CRAN check within a few
minutes, `tests/testthat.R` runs a contract tier on CRAN: the main test
file of each exported family, about 5,700 expectations. Fixtures that
compile or sample Stan models never run on CRAN.
