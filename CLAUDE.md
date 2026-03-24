# R package development

## Package architecture

- `R/freq.R` / `R/cross_tab.R` - core tabulation functions
- `R/tables_ascii.R` / `R/freq_print.R` - ASCII rendering and print
  methods
- `R/varlist.R` / `R/code_book.R` - variable inspection tools
- `R/mean_n.R`, `R/sum_n.R`, `R/count_n.R` - row-wise descriptive
  summaries
- `R/table_categorical.R` / `R/table_continuous.R` - summary table
  helpers (depend on `tinytable`/`flextable` via Suggests)
- `R/globals.R` - package-level constants and globals

Optional dependencies (Suggests): `DT`, `clipr`, `tinytable`,
`flextable`, `openxlsx2`, `officer`. Guard all usage with
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html) and a clear,
actionable error.

## Working style

- For any change touching more than one file or affecting user-facing
  behavior, describe the plan before writing code.
- Prefer minimal, focused changes. Do not refactor surrounding code
  unless asked.

## Git

- Do not commit unless explicitly asked.
- Do not push unless explicitly asked.

## Key commands

``` sh
# Load package and run code
Rscript -e "devtools::load_all(); code"

# Run all tests
Rscript -e "devtools::test()"

# Run tests matching a filter
Rscript -e "devtools::test(filter = '^cross_tab')"

# Run a single test file
Rscript -e "testthat::test_file('tests/testthat/test-cross_tab.R')"

# Redocument the package
Rscript -e "devtools::document()"
```
