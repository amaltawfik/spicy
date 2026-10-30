## Test environments

* Local: Windows 11, R 4.6.1 (with and without the suggested packages)
* GitHub Actions: Windows (R-release), macOS (R-release), Ubuntu
  (R-release, R-devel, R-oldrel-1)
* win-builder: R-release (4.6.2), R-devel, R-oldrelease (4.5.3)
* R-hub: atlas

## R CMD check results

0 errors | 0 warnings | 1 NOTE

The NOTE is from the incoming-feasibility check. The spell check of
DESCRIPTION reports "r's", the end of "Cramér's V" split at the accent.
The URL check reports 429 for the maintainer's ROR identifier
(https://ror.org/04j47fz63), which answers 429 to automated requests;
the identifier resolves in a browser.

## Reverse dependencies

None.

## Notes on this submission

This release fixes the robust standard errors of mgcv fits with a free
dispersion, which were wrong in 0.13.0, and changes some defaults of
the descriptive tables; NEWS.md lists each with the argument that
restores the previous behavior. Following the reviewer's request, the
tests run on CRAN were reduced to about a minute and a half; the full
suite runs on GitHub Actions.
