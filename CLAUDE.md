# R package development

For design principles, coding conventions, testing rules,
documentation rules, and the full release workflow, see
[`AGENTS.md`](AGENTS.md). The notes below are Claude-specific
overrides that take precedence within Claude Code sessions.

## Package architecture

- `R/freq.R` / `R/cross_tab.R` - core tabulation functions
- `R/tables_ascii.R` / `R/freq_print.R` - ASCII rendering and print methods
- `R/varlist.R` / `R/code_book.R` - variable inspection tools
- `R/mean_n.R`, `R/sum_n.R`, `R/count_n.R` - row-wise descriptive summaries
- `R/table_categorical.R` / `R/table_continuous.R` - summary table helpers
  (depend on `tinytable`/`flextable` via Suggests)
- `R/globals.R` - package-level constants and globals

Optional dependencies (Suggests): `clipr`, `tinytable`,
`flextable`, `openxlsx2`, `officer`, `quarto` (the PDF of `code_book()`).
Guard all usage with `requireNamespace()` and a clear, actionable error.

## Working style

- For any change touching more than one file or affecting user-facing
  behavior, describe the plan before writing code.
- Prefer minimal, focused changes. Do not refactor surrounding code
  unless asked.
- Size the fix to the problem. Before proposing a change, say what it
  adds (lines, files, dependencies) next to the size of the feature it
  touches, and put the smallest fix that works first, as a real option
  and never as a foil for a larger one.
- A report about a failing test or a secondary platform calls for the
  smallest fix. A redesign needs a defect a user can see, named and
  measured on the platforms users work on, and it is a separate decision
  for the maintainer. "The most robust solution" does not suspend this:
  robustness is weighed against size and upkeep, and the weighing is
  shown.
- When a fix goes beyond what a report asked for, the public reply gives
  the reason in one sentence. Without it the reader sees only the size.
- Anything published under the maintainer's name (issue replies, commit
  messages, pull requests) is short and plain: what was wrong, what
  changed. Volume and polish read as delegated work, and the maintainer
  answers for it.

These four rules come from issue #8 (2026-10-02). Two tests failed on
Alpine Linux, and the fix replaced an `iconv()` call with a 1,348-entry
table, a generator and an oracle test: the codebook code went from 342
to 896 lines. The reporter, an R core contributor, answered that he had
not meant to triple the code base, and that the defect was in the test.
He was right. The derived file name is a convenience, `filename` lets
the user set it, and nobody types the `œ` ligature in an R script. The
table was removed the same day: the code went back to what was released
and the test was corrected instead.

## Git

- Do not commit unless explicitly asked.
- Do not push unless explicitly asked.

## Key commands

```sh
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
