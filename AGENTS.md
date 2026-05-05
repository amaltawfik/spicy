# R package development

## Design principles

These two principles override convenience when they conflict.
Reach for them first when reviewing any change to user-facing
input or output.

* **Verbatim user identifiers.** Never silently mutate
  user-supplied filenames, column names, labels, or titles.
  Convention follows Stata `label variable` and SPSS
  `VARIABLE LABELS`: input is preserved exactly as the user
  typed it. The only sanctioned exception is whitespace
  stripping on R column names (where leading or trailing
  whitespace would force backtick quoting throughout downstream
  user code). Labels, titles, and filenames are kept verbatim.
* **Loud signals over silent mutations.** Silent mutation is
  forbidden -- the user must always be able to see what the
  package did to their input. The visible signal scales with
  the response, in this order of preference:

    1. **Succeed silently** when the request is fulfilled
       exactly as stated, with no compromise (e.g.
       `label_from_names()` preserves labels verbatim).
    2. **Succeed with a `spicy_warning`** when there is a
       small unambiguous adjustment that lets the function
       continue (e.g. `cross_tab()` auto-renames a margin
       column that would collide with a y-level named "N" or
       "Total", and emits `spicy_renamed_column` pointing at
       the rename). Prefer this over option 3 whenever the
       adjustment is mechanical and reversible by the user.
    3. **Fail with `spicy_abort()`** when there is no clean
       adjustment -- the user must correct the input. Use a
       classed condition (the `spicy_error` parent plus a
       leaf class) so downstream code can dispatch
       (e.g. `code_book()` does not truncate over-long
       filenames; the browser download error surfaces and the
       user shortens the title).

  Tests that observe these signals must dispatch on **class**,
  not on regex over the message string. `sQuote()`, `cli`
  formatting, and locale settings vary across platforms; a
  test that asserts `'N'` will fail under Windows fancy-quote
  output where the same test using
  `class = "spicy_renamed_column"` passes.

## Key commands

```sh
# To run code
Rscript -e "devtools::load_all(); code"

# To run all tests
Rscript -e "devtools::test()"

# To run tests matching a function or file prefix
Rscript -e "devtools::test(filter = '^cross_tab')"

# To run a single test file
Rscript -e "testthat::test_file('tests/testthat/test-cross_tab.R')"

# To redocument the package
Rscript -e "devtools::document()"

# To rebuild the README after editing README.Rmd
Rscript -e "devtools::build_readme()"

# To check pkgdown documentation
Rscript -e "pkgdown::check_pkgdown()"

# To rebuild the pkgdown site locally
Rscript -e "pkgdown::build_site()"

# To check URLs in docs
Rscript -e "urlchecker::url_check()"

# To spell-check docs
Rscript -e "devtools::spell_check()"

# To check the package with R CMD check
Rscript -e "devtools::check()"

# To format code
air format .
```

## Coding

* Always run `air format .` after generating or editing R code.
* Use the base pipe operator (`|>`) not the magrittr pipe (`%>%`).
* Do not use `_$x` or `_$[["x"]]` because this package must work on R 4.1.
* Use `\(...) ...` for single-line anonymous functions. For all other cases, use `function(...) {}`.
* Preserve the package's console-first design: outputs should stay readable in plain ASCII and should not depend on color.
* Prefer minimal dependencies. Any feature backed by a suggested package must be guarded with `requireNamespace()` and fail with a clear, actionable error.
* Keep user-facing behavior stable for tabulation and printing helpers. Changes to table structure, attributes, or printed layout should be treated as behavioral changes, not cosmetic refactors.
* Follow existing file boundaries when possible: core user-facing functions live in `R/*.R`, print methods and ASCII table helpers live in the current printing-related files, and package-level globals stay in `R/globals.R`.

## Testing

* Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
* All new code should have an accompanying test.
* If there are existing tests, place new tests next to similar existing tests.
* Keep tests minimal and focused, with few comments.
* For tabulation and printing code, prefer testing returned objects, columns, and attributes with `styled = FALSE`; only assert printed output when the formatting itself is the feature under test.
* Cover optional dependency paths with either guarded tests or explicit expectations for the error message when the dependency is unavailable.
* When fixing a bug, add a regression test that would have failed before the fix.

## Documentation

* Every user-facing function should be exported and have roxygen2 documentation.
* Internal functions should not have standalone roxygen2 documentation topics unless there is a clear package-level reason.
* Wrap roxygen comments at 80 characters.
* Always run `devtools::document()` after changing a roxygen comment.
* Whenever you add a new user-facing documentation topic, also add it to `_pkgdown.yml`.
* Use `pkgdown::check_pkgdown()` to verify that all topics are included in the reference index.
* If you edit `README.Rmd`, rebuild `README.md` with `devtools::build_readme()`.
* Examples that need interactivity, the clipboard, the Viewer pane, or optional packages should be guarded appropriately with `\donttest{}`, `\dontrun{}`, and `requireNamespace()` checks.

## NEWS.md

* Every user-facing change should get a bullet in `NEWS.md`. Do not add bullets for small documentation changes or internal refactors.
* Each bullet should describe the end-user impact and mention the affected function early when relevant.
* Keep each bullet to a single paragraph with no manual line wrapping.
* Order bullets alphabetically by function name within a release block. Put general bullets that do not name a function first.

## Repository workflow

* The `dev/` scripts are the reference workflow for local development, release preparation, and post-release cleanup. Keep them aligned if the package workflow changes.
* CI already covers `R CMD check`, `rhub`, `pkgdown`, and test coverage. Local changes should not assume CI will catch avoidable documentation or test regressions.

## Writing

* Use sentence case for headings.
* Use US English.
* Keep error messages, warnings, and documentation direct and practical.

## Proofreading

If the user asks you to proofread a file, act as an expert proofreader and
editor with a deep understanding of clear, engaging, and well-structured
writing.

Work paragraph by paragraph, always starting by making a TODO list that
includes individual items for each top-level heading.

Fix spelling, grammar, and other minor problems without asking the user.
Label any unclear, confusing, or ambiguous sentences with a FIXME comment.

Only report what you have changed.
