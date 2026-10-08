# Contributing to spicy

Thank you for taking the time. spicy is maintained by one person, so a
report that is easy to act on is the most valuable contribution.

## Reporting a problem

Open an issue at <https://github.com/amaltawfik/spicy/issues> with:

- a minimal example that reproduces the problem (a few lines on
  `sochealth` or on
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html), no private
  data);
- what you expected and what you got, pasted as text;
- the output of
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html) or
  [`sessioninfo::session_info()`](https://sessioninfo.r-lib.org/reference/session_info.html).

A wrong number in a table is the most serious kind of report. If you
can, say what another software (SPSS, Stata, another package) gives for
the same data.

## Proposing a change

Open an issue before a pull request for anything beyond a typo, so that
the scope is agreed first. spicy sizes every change to the problem it
solves: a failing test or a platform quirk calls for the smallest fix, a
redesign needs a defect users can see. A pull request should say what
was wrong and what changed, in a few lines.

## Working on the code

- Install the development dependencies with `pak::pak("local::.")` or
  `devtools::install_dev_deps()`.
- Run the tests with `devtools::test()`, or one file with
  `testthat::test_file("tests/testthat/test-freq.R")`.
- Format R code with [air](https://posit-dev.github.io/air/)
  (`air format .`), and keep R sources ASCII (write accents as `\u`
  escapes).
- Documentation is written with roxygen2 (`devtools::document()`), in US
  English. Every user-facing word goes through the language registry
  (`R/i18n.R`, `R/i18n_fr.R`) so that the French output stays complete.
- New code arrives with tests, and the package aims at full coverage.
- Do not add a package to Imports; optional features live in Suggests
  behind [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html)
  with a clear error.

Before a pull request, `devtools::check()` must pass with no error,
warning or note.

## Conduct

This project follows the [code of
conduct](https://amaltawfik.github.io/spicy/CODE_OF_CONDUCT.md).
