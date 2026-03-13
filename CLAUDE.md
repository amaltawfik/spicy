# R package development

## Package architecture

- `R/freq.R` / `R/cross_tab.R` — core tabulation functions
- `R/tables_ascii.R` / `R/freq_print.R` — ASCII rendering and print methods
- `R/varlist.R` / `R/code_book.R` — variable inspection tools
- `R/mean_n.R`, `R/sum_n.R`, `R/count_n.R` — row-wise descriptive summaries
- `R/table_apa.R` — APA reporting (depends on `tinytable`/`flextable` via Suggests)
- `R/globals.R` — package-level constants and globals

Optional dependencies (Suggests): `DT`, `clipr`, `tinytable`, `flextable`, `openxlsx2`, `officer`.
Guard all usage with `requireNamespace()` and a clear, actionable error.

## Working style

- For any change touching more than one file or affecting user-facing behavior, describe the plan before writing code.
- Prefer minimal, focused changes. Do not refactor surrounding code unless asked.

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

# Rebuild README after editing README.Rmd
Rscript -e "devtools::build_readme()"

# Check pkgdown documentation
Rscript -e "pkgdown::check_pkgdown()"

# Rebuild the pkgdown site locally
Rscript -e "pkgdown::build_site()"

# Check URLs in docs
Rscript -e "urlchecker::url_check()"

# Spell-check docs
Rscript -e "devtools::spell_check()"

# R CMD check
Rscript -e "devtools::check()"

# Format code
air format .
```

## Coding

- Always run `air format .` after generating or editing R code.
- Use the base pipe `|>`, never the magrittr pipe `%>%`.
- R 4.1 compatibility is required. Do not use `_$x`, `_$[["x"]]`, or the `_` placeholder syntax — these require R 4.2+.
- Use `\(...) ...` for single-line anonymous functions; use `function(...) {}` otherwise.
- Preserve the console-first design: output must be readable in plain ASCII without color.
- Prefer minimal dependencies. Guard any suggested-package feature with `requireNamespace()` and a clear, actionable error.
- Treat changes to table structure, attributes, or printed layout as behavioral changes, not cosmetic refactors.
- Follow existing file boundaries: core user-facing functions in `R/*.R`, print/ASCII helpers in printing-related files, package globals in `R/globals.R`.

## Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
- All new code must have an accompanying test.
- Place new tests next to similar existing ones.
- Keep tests minimal and focused, with few comments.
- For tabulation and printing code, test returned objects with `styled = FALSE`; assert printed output only when formatting itself is the feature under test.
- Cover optional-dependency paths with guarded tests or explicit error-message expectations.
- When fixing a bug, add a regression test that would have failed before the fix.

## Documentation

- Every user-facing function must be exported with roxygen2 documentation.
- Internal functions should not have standalone roxygen2 topics.
- Wrap roxygen comments at 80 characters.
- Run `devtools::document()` after any roxygen change.
- Add every new user-facing topic to `_pkgdown.yml`; verify with `pkgdown::check_pkgdown()`.
- Rebuild `README.md` with `devtools::build_readme()` after editing `README.Rmd`.
- Guard interactive or optional-dependency examples with `\donttest{}`, `\dontrun{}`, or `requireNamespace()`.

## NEWS.md

- Add a bullet for every user-facing change; skip small doc changes and internal refactors.
- State the end-user impact; mention the affected function early.
- One paragraph per bullet, no manual line wrapping.
- Order bullets alphabetically by function name within a release block; general bullets first.

## Repository workflow

- The `dev/` scripts are the reference for local development, release prep, and post-release cleanup — keep them aligned when the workflow changes.
- CI covers `R CMD check`, `rhub`, `pkgdown`, and coverage. Do not rely on CI to catch documentation or test regressions that can be avoided locally.

## Writing

- Use sentence case for headings.
- Use US English.
- Keep error messages, warnings, and documentation direct and practical.

## Proofreading

When asked to proofread a file, act as an expert proofreader with a deep understanding of clear, engaging, and well-structured writing.

Work paragraph by paragraph, starting with a TODO list that includes one item per top-level heading.

Fix spelling, grammar, and minor issues without asking. Label unclear or ambiguous sentences with a `FIXME` comment.

Report only what was changed.
