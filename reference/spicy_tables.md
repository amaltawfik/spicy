# Spicy Table Engine: Frequency and Cross-tabulation Rendering

The *spicy table engine* provides a cohesive set of tools for creating
and printing formatted ASCII tables in R, designed for descriptive
statistics.

Functions in this family include:

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) —
  frequency tables with support for weights, labelled data, and
  cumulative percentages

- [`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
  — general-purpose ASCII table printer

- [`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md)
  — internal rendering engine for column alignment and formatting

## Details

All functions in this family share a common philosophy:

- Console-friendly display with Unicode box-drawing characters

- Consistent alignment and spacing across outputs

- Automatic detection of variable type (`factor`, `labelled`, `numeric`)

- Optional integration of variable labels and weighting information

## Core functions

- **[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)** —
  Main entry point for generating frequency tables.

- **[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)**
  — Applies formatting and optional titles or notes.

- **[`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md)**
  — Internal engine handling padding, alignment, and box rules.

## Output styling

The spicy table engine supports multiple padding options via `padding`:
`"compact"` (default), `"normal"`, and `"wide"`. Horizontal and vertical
rules can be customized, and colors are supported when the terminal
allows ANSI color output (via the **crayon** package).

## See also

[`print.spicy_freq_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_freq_table.md)
for the specialized frequency display method.
[`labelled::to_factor()`](https://larmarange.github.io/labelled/reference/to_factor.html)
and [`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html)
for data transformations.

Other spicy tables:
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
