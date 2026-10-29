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

The NOTE comes from the CRAN incoming-feasibility step and has two
parts, neither of which concerns the code.

* On win-builder (R-devel and R-release), the spell check of DESCRIPTION:

      Possibly misspelled words in DESCRIPTION:
        JAMA, NEJM, univariable

  All three are spelled as intended: JAMA and NEJM name reporting styles
  (Journal of the American Medical Association, New England Journal of
  Medicine), and "univariable" is the standard term. The published
  0.13.0 carried the same NOTE.

* Locally, the URL check of the maintainer's affiliation,
  `https://ror.org/04j47fz63` (DESCRIPTION and the package help page),
  returns 429 Too Many Requests: ror.org rate-limits automated requests.
  The identifier is valid and resolves in a browser.

No other notes, warnings, or errors on any tested platform.

## Reverse dependencies

spicy has no reverse dependencies.

## Notes on this submission

This release follows 0.13.0 by five weeks and is the third release in
six months, down from nine in the window of May 2026; the package keeps
the slower cadence it adopted then.

It corrects an error of 0.13.0: the robust and cluster-robust standard
errors of `mgcv::gam()` and `mgcv::bam()` fits were wrong for families
with a free dispersion (divided by it), so the package now forms that
sandwich itself. The rest of the release makes the tables publishable
without a second pass: the note of a descriptive table opens with the
sample size, states the direction and the base of its percentages (which
are now computed on the observed values, the missing values counted but
left out of the base), and can be replaced or extended; the heads are
decked; printed outputs set the minus sign; decimal places can be set
per variable. It also makes `code_book()` a deliverable codebook (an
object, an Excel workbook, or a PDF), adds a multiple-response table, the
multiple-comparison adjustment of the descriptive tables, cumulative-link
mixed models and cluster-robust standard errors for `glmmTMB` fits, and
completes the French output. Several defaults of the descriptive tables
change as a result; each is listed under Breaking changes in `NEWS.md`,
with the argument that restores the previous behavior. The package has
no reverse dependencies.

The full test suite has about 28,000 expectations, with estimates and
standard errors validated to the digit against independent
implementations (SPSS/PSPP, Stata conventions, and the reference R
packages of each model family). It runs on GitHub Actions on every push,
across five platforms. To keep the CRAN check within a few minutes,
`tests/testthat.R` runs a contract tier on CRAN: the main test file of
each exported family, about 9,700 expectations in about six minutes.
Fixtures that compile or sample Stan models never run on CRAN.
