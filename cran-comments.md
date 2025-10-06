## R CMD check results

0 errors \| 0 warnings \| 0 notes

## Comments

This is version 0.3.0 of the `spicy` package, a minor feature release following 0.2.1.

This release introduces a new function:

-   `code_book()` generates an interactive and exportable variable codebook for data frames.
    -   Builds on varlist() to summarize variable names, labels, types, and representative values.
    -   Displays results as an interactive DT::datatable() with built-in search, sort, and export tools (copy, print, CSV, Excel, PDF).
    -   Designed for exploratory data inspection and improved dataset documentation workflows.

Additional improvements:

-   Minor documentation updates and internal consistency enhancements.
-   No breaking changes or modifications to existing APIs.

R CMD check passes cleanly on local (macOS) and win-builder (release, devel, oldrelease).\
No changes to package dependencies.
