# Cross-tabulation

Computes a two-way cross-tabulation with optional weights, grouping
(including combinations of multiple variables), percentage displays, and
inferential statistics.

`cross_tab()` produces weighted or unweighted contingency tables with
row or column percentages, optional grouping via `by`, and associated
Chi-squared tests with an association measure and diagnostic
information.

Both `x` and `y` variables are required. For one-way frequency tables,
use [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
instead.

## Usage

``` r
cross_tab(
  data,
  x,
  y = NULL,
  by = NULL,
  weights = NULL,
  rescale = FALSE,
  percent = c("none", "column", "row"),
  include_stats = TRUE,
  assoc_measure = "auto",
  assoc_ci = FALSE,
  correct = FALSE,
  simulate_p = FALSE,
  simulate_B = 2000,
  digits = NULL,
  styled = TRUE,
  show_n = TRUE
)

# S3 method for class 'spicy_cross_table_list'
print(x, ...)
```

## Arguments

- data:

  A data frame. Alternatively, a vector when using the vector-based
  interface.

- x:

  Row variable (unquoted).

- y:

  Column variable (unquoted). Mandatory; for one-way tables, use
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).

- by:

  Optional grouping variable or expression. Can be a single variable or
  a combination of multiple variables (e.g. `interaction(vs, am)`).

- weights:

  Optional numeric weights.

- rescale:

  Logical. If `FALSE` (the default), weights are used as-is. If `TRUE`,
  rescales weights so total weighted N matches raw N.

- percent:

  One of `"none"` (the default), `"row"`, `"column"`. Unique
  abbreviations are accepted (e.g. `"n"`, `"r"`, `"c"`).

- include_stats:

  Logical. If `TRUE` (the default), computes Chi-squared and an
  association measure (see `assoc_measure`).

- assoc_measure:

  Character. Which association measure to report. `"auto"` (default)
  selects Kendall's Tau-b when both variables are ordered factors and
  Cramer's V otherwise. Other choices: `"cramer_v"`, `"phi"`, `"gamma"`,
  `"tau_b"`, `"tau_c"`, `"somers_d"`, `"lambda"`, `"none"`.

- assoc_ci:

  Logical. If `TRUE`, includes the 95 percent confidence interval of the
  association measure in the note. Defaults to `FALSE`.

- correct:

  Logical. If `FALSE` (the default), no continuity correction is
  applied. If `TRUE`, applies Yates correction (only for 2x2 tables).

- simulate_p:

  Logical. If `FALSE` (the default), uses asymptotic p-values. If
  `TRUE`, uses Monte Carlo simulation.

- simulate_B:

  Integer. Number of replicates for Monte Carlo simulation. Defaults to
  `2000`.

- digits:

  Number of decimals. Defaults to `1` for percentages, `0` for counts.

- styled:

  Logical. If `TRUE` (the default), returns a `spicy_cross_table` object
  (for formatted printing). If `FALSE`, returns a plain `data.frame`.

- show_n:

  Logical. If `TRUE` (the default), adds marginal N totals when
  `percent != "none"`.

- ...:

  Additional arguments passed to individual print methods.

## Value

A `data.frame`, list of data.frames, or `spicy_cross_table` object. When
`by` is used, returns a `spicy_cross_table_list`.

## Global Options

The function recognizes the following global options that modify its
default behavior:

- **`options(spicy.percent = "column")`** Sets the default percentage
  mode for all calls to `cross_tab()`. Valid values are `"none"`,
  `"row"`, and `"column"`. Equivalent to setting `percent = "column"`
  (or another choice) in each call.

- **`options(spicy.simulate_p = TRUE)`** Enables Monte Carlo simulation
  for all Chi-squared tests by default. Equivalent to setting
  `simulate_p = TRUE` in every call.

- **`options(spicy.rescale = TRUE)`** Automatically rescales weights so
  that total weighted N equals the raw N. Equivalent to setting
  `rescale = TRUE` in each call.

These options are convenient for users who wish to enforce consistent
behavior across multiple calls to `cross_tab()` and other spicy table
functions. They can be disabled or reset by setting them to `NULL`:
`options(spicy.percent = NULL, spicy.simulate_p = NULL, spicy.rescale = NULL)`.

Example:

    options(spicy.simulate_p = TRUE, spicy.rescale = TRUE)
    cross_tab(mtcars, cyl, gear, weights = mtcars$mpg)

## Examples

``` r
# Basic crosstab
cross_tab(mtcars, cyl, gear)
#> Crosstable: cyl x gear (N)
#> 
#>  Values      │       3        4       5 │      Total 
#> ─────────────┼──────────────────────────┼────────────
#>  4           │       1        8       2 │         11 
#>  6           │       2        4       1 │          7 
#>  8           │      12        0       2 │         14 
#> ─────────────┼──────────────────────────┼────────────
#>  Total       │      15       12       5 │         32 
#> 
#> Chi-2(4) = 18.0, p = 0.001
#> Cramer's V = 0.53
#> Warning: 6 expected cells < 5 (66.7%). Minimum expected = 1.09. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.

# Weighted (rescaled)
cross_tab(mtcars, cyl, gear, weights = mtcars$mpg, rescale = TRUE)
#> Crosstable: cyl x gear (N)
#> 
#>  Values      │       3        4       5 │      Total 
#> ─────────────┼──────────────────────────┼────────────
#>  4           │       1       11       3 │         15 
#>  6           │       2        4       1 │          7 
#>  8           │       9        0       2 │         11 
#> ─────────────┼──────────────────────────┼────────────
#>  Total       │      12       15       6 │         32 
#> 
#> Chi-2(4) = 17.7, p = 0.001
#> Cramer's V = 0.53
#> Warning: 7 expected cells < 5 (77.8%). Minimum expected = 1.14. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.
#> Weight: mpg (rescaled)

# Grouped
cross_tab(mtcars, cyl, gear, by = am)
#> Crosstable: cyl x gear (N) | am = 0
#> 
#>  Values      │       3       4       5 │      Total 
#> ─────────────┼─────────────────────────┼────────────
#>  4           │       1       2       0 │          3 
#>  6           │       2       2       0 │          4 
#>  8           │      12       0       0 │         12 
#> ─────────────┼─────────────────────────┼────────────
#>  Total       │      15       4       0 │         19 
#> 
#> Chi-2(2) = 9.0, p = 0.011
#> Cramer's V = 0.69
#> Warning: 5 expected cells < 5 (83.3%). 2 expected cells < 1. Minimum expected = 0.63. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.
#> 
#> Crosstable: cyl x gear (N) | am = 1
#> 
#>  Values      │      3       4       5 │      Total 
#> ─────────────┼────────────────────────┼────────────
#>  4           │      0       6       2 │          8 
#>  6           │      0       2       1 │          3 
#>  8           │      0       0       2 │          2 
#> ─────────────┼────────────────────────┼────────────
#>  Total       │      0       8       5 │         13 
#> 
#> Chi-2(2) = 3.8, p = 0.146
#> Cramer's V = 0.54
#> Warning: 6 expected cells < 5 (100%). 1 expected cell < 1. Minimum expected = 0.77. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.

# Grouped by an interaction
cross_tab(mtcars, cyl, gear, by = interaction(vs, am))
#> Crosstable: cyl x gear (N) | vs x am = 0.0
#> 
#>  Values      │       3       4       5 │      Total 
#> ─────────────┼─────────────────────────┼────────────
#>  4           │       0       0       0 │          0 
#>  6           │       0       0       0 │          0 
#>  8           │      12       0       0 │         12 
#> ─────────────┼─────────────────────────┼────────────
#>  Total       │      12       0       0 │         12 
#> 
#> Crosstable: cyl x gear (N) | vs x am = 1.0
#> 
#>  Values      │      3       4       5 │      Total 
#> ─────────────┼────────────────────────┼────────────
#>  4           │      1       2       0 │          3 
#>  6           │      2       2       0 │          4 
#>  8           │      0       0       0 │          0 
#> ─────────────┼────────────────────────┼────────────
#>  Total       │      3       4       0 │          7 
#> 
#> Chi-2(1) = 0.2, p = 0.659
#> Cramer's V = 0.17
#> Warning: 4 expected cells < 5 (100%). Minimum expected = 1.29. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.
#> 
#> Crosstable: cyl x gear (N) | vs x am = 0.1
#> 
#>  Values      │      3       4       5 │      Total 
#> ─────────────┼────────────────────────┼────────────
#>  4           │      0       0       1 │          1 
#>  6           │      0       2       1 │          3 
#>  8           │      0       0       2 │          2 
#> ─────────────┼────────────────────────┼────────────
#>  Total       │      0       2       4 │          6 
#> 
#> Chi-2(2) = 3.0, p = 0.223
#> Cramer's V = 0.71
#> Warning: 6 expected cells < 5 (100%). 3 expected cells < 1. Minimum expected = 0.33. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.
#> 
#> Crosstable: cyl x gear (N) | vs x am = 1.1
#> 
#>  Values      │      3       4       5 │      Total 
#> ─────────────┼────────────────────────┼────────────
#>  4           │      0       6       1 │          7 
#>  6           │      0       0       0 │          0 
#>  8           │      0       0       0 │          0 
#> ─────────────┼────────────────────────┼────────────
#>  Total       │      0       6       1 │          7 

# Vector interface
cross_tab(mtcars$cyl, mtcars$gear, percent = "c")
#> Crosstable: cyl x gear (Column %)
#> 
#>  Values      │          3           4           5 │      Total 
#> ─────────────┼────────────────────────────────────┼────────────
#>  4           │        6.7        66.7        40.0 │       34.4 
#>  6           │       13.3        33.3        20.0 │       21.9 
#>  8           │       80.0         0.0        40.0 │       43.8 
#> ─────────────┼────────────────────────────────────┼────────────
#>  Total       │      100.0       100.0       100.0 │      100.0 
#>  N           │         15          12           5 │         32 
#> 
#> Chi-2(4) = 18.0, p = 0.001
#> Cramer's V = 0.53
#> Warning: 6 expected cells < 5 (66.7%). Minimum expected = 1.09. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.

# Set default percent mode globally
options(spicy.percent = "column")

# Now this will display column percentages by default
cross_tab(mtcars, cyl, gear)
#> Crosstable: cyl x gear (Column %)
#> 
#>  Values      │          3           4           5 │      Total 
#> ─────────────┼────────────────────────────────────┼────────────
#>  4           │        6.7        66.7        40.0 │       34.4 
#>  6           │       13.3        33.3        20.0 │       21.9 
#>  8           │       80.0         0.0        40.0 │       43.8 
#> ─────────────┼────────────────────────────────────┼────────────
#>  Total       │      100.0       100.0       100.0 │      100.0 
#>  N           │         15          12           5 │         32 
#> 
#> Chi-2(4) = 18.0, p = 0.001
#> Cramer's V = 0.53
#> Warning: 6 expected cells < 5 (66.7%). Minimum expected = 1.09. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.

# Reset to default behavior
options(spicy.percent = NULL)

# 2x2 table with Yates correction
cross_tab(mtcars, vs, am, correct = TRUE)
#> Crosstable: vs x am (N)
#> 
#>  Values      │       0        1 │      Total 
#> ─────────────┼──────────────────┼────────────
#>  0           │      12        6 │         18 
#>  1           │       7        7 │         14 
#> ─────────────┼──────────────────┼────────────
#>  Total       │      19       13 │         32 
#> 
#> Chi-2(1) = 0.3, p = 0.556
#> Cramer's V = 0.17
#> Yates continuity correction applied.
```
