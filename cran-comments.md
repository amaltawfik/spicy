## Test environments

* Local: Windows 11, R 4.6.1 (with and without the suggested packages)
* GitHub Actions: Windows (R-release), macOS (R-release), Ubuntu
  (R-release, R-devel, R-oldrel-1)
* win-builder: R-release, R-devel, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 1 NOTE

The NOTE is from the incoming-feasibility check. The spell check flags
JAMA, NEJM and univariable in DESCRIPTION; all three are intended, as in
0.13.0. The URL check reports 429 for the maintainer's ROR identifier
(https://ror.org/04j47fz63), which answers 429 to automated requests;
the identifier resolves in a browser.

## Reverse dependencies

None.

## Notes on this submission

Third release in six months, five weeks after 0.13.0. It fixes the
robust standard errors of mgcv fits with a free dispersion, which were
wrong in 0.13.0, and changes some defaults of the descriptive tables;
NEWS.md lists each with the argument that restores the previous
behavior. The tests run on CRAN take about six minutes.
