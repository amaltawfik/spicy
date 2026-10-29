# cran-comments for spicy 0.14.0

## Test environments

* Local: Windows 11, R 4.6.1 (with and without the suggested packages)
* GitHub Actions: Windows (R-release), macOS (R-release), Ubuntu
  (R-release, R-devel, R-oldrel-1)
* win-builder: R-release, R-devel, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 1 NOTE

The NOTE is the incoming-feasibility check. Its spell check flags JAMA,
NEJM and univariable in DESCRIPTION: all three are intended (two
reporting styles, one standard term), as in the published 0.13.0. Its
URL check reports 429 for the maintainer's ROR identifier,
https://ror.org/04j47fz63, which rate-limits automated requests; the
identifier is valid.

## Reverse dependencies

None.

## Notes on this submission

This release follows 0.13.0 by five weeks, the third release in six
months; the package keeps the slower cadence adopted in May 2026. It
corrects the robust standard errors of mgcv fits with a free dispersion
(wrong in 0.13.0) and changes several defaults of the descriptive
tables, each listed in NEWS.md with the argument that restores the
previous behavior. The CRAN tests run a contract tier of about 9,700
expectations in about six minutes; the full suite (about 28,000) runs on
GitHub Actions.
