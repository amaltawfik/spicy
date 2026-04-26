## R CMD check results

0 errors | 0 warnings | 0 notes

* On R-oldrelease (4.4.3), a NOTE may report that the Author field differs
  from Authors@R. This is caused by the ROR (Research Organization
  Registry) identifier in the `comment` field of `Authors@R`, which older
  R versions do not fully expand into the plain-text Author field. The
  package metadata is correct; the discrepancy is cosmetic.

* CRAN incoming feasibility may report a NOTE about update frequency
  (number of updates in the past 6 months). This release follows shortly
  after 0.9.0 to deliver user-facing improvements and bug fixes for
  variable inspection and codebook workflows. No further updates are
  planned in the immediate term.

## Comments

This is version 0.10.0 of the **spicy** package.

This release extends `code_book()` and `varlist()` with new selectors,
arguments, and column-type support, refines how missing values and
list-columns are summarized, and improves error reporting when a column
cannot be summarized. See `NEWS.md` for the full list of user-facing
changes.

### Testing environments

* Windows 11 (local), R 4.6.0
* GitHub Actions: ubuntu-latest (R release), windows-latest (R release), macOS-latest (R release)
* win-builder: R-release, R-devel, R-oldrelease
* R-hub: linux (R-devel)
