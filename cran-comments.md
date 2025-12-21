## R CMD check results

0 errors | 0 warnings | 0 notes

## Comments

This is version 0.4.0 of the **spicy** package, a minor feature and refinement
release extending the descriptive statistics and table rendering capabilities
introduced in previous versions.

### Main changes

* Enhanced printing of frequency tables and cross-tabulations with a redesigned
  ASCII rendering engine, providing cleaner alignment, clearer totals, and more
  consistent spacing across outputs.
* `cross_tab()` gained improved handling of percentages, grouping, and statistical
  diagnostics, along with clearer documentation and examples.
* Minor cosmetic and consistency improvements across table-related outputs
  (including titles, notes, and column naming).
* Documentation was refined and harmonized for clarity and consistency.

There are no breaking changes to the public API.

### Testing environments

R CMD check results were verified on:

- Local macOS (R 4.4.x)
- Win-builder (devel, release, oldrelease)
- R-hub (Windows, Linux). R-hub checks on macOS (R-devel) were cancelled due to
  temporary capacity constraints on the R-hub infrastructure.

- Fixed CRAN incoming check notes
