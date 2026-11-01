## Resubmission

This patch release answers the CRAN team's message of 2026-11-01 about
the MKL additional check: one test compared two independent
computations of a test statistic exactly, and MKL gives the same value
up to the last bit. That comparison, and two others of the same form,
now use a tolerance. There is no change to the code.

## Test environments

* Local: Windows 11, R 4.6.1
* GitHub Actions: Windows (R-release), macOS (R-release), Ubuntu
  (R-release, R-devel, R-oldrel-1)
* win-builder: R-release (4.6.2), R-devel; tests take about 75 s

## R CMD check results

0 errors | 0 warnings | 1 NOTE

The NOTE is from the incoming-feasibility check. "Days since last
update: 1": this release is the correction the CRAN team asked for.
The URL check reports 429 for the maintainer's ROR identifier
(https://ror.org/04j47fz63), which answers 429 to automated requests;
the identifier is valid, as for 0.14.0.

## Reverse dependencies

None.
