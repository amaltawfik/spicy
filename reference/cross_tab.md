# Cross-tabulation (SPSS-like)

Computes a cross-tabulation with optional weights, grouping, and
percentages. diagnostics, and modern ASCII formatting. Note:
`cross_tab()` requires both `x` and `y` variables. For one-way frequency
tables, use
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
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
  simulate_p = FALSE,
  simulate_B = 2000,
  digits = NULL,
  styled = TRUE,
  show_n = TRUE
)
```

## Arguments

- data:

  A data frame.

- x:

  Row variable (unquoted).

- y:

  Column variable (unquoted). Mandatory; for one-way tables, use
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).

- by:

  Optional grouping variable or expression (e.g. `interaction(vs, am)`).

- weights:

  Optional numeric weights.

- rescale:

  Logical. If TRUE, rescales weights so total weighted N matches raw N
  (default FALSE).

- percent:

  One of `"none"`, `"row"`, `"column"`.

- include_stats:

  Logical; compute Chi-squared and Cramer's V (default TRUE).

- simulate_p:

  Logical; use Monte Carlo p-value simulation (default FALSE).

- simulate_B:

  Integer; number of replicates for Monte Carlo (default 2000).

- digits:

  Number of decimals (default 1 for percentages, 0 for counts).

- styled:

  Logical; if TRUE, returns a "spicy_cross_table" object (for printing).

- show_n:

  Logical; if TRUE, adds marginal N totals when percent != "none".

## Value

A `data.frame`, list of data.frames, or `spicy_cross_table` object. When
`by` is used, returns a `spicy_cross_table_list`.

## Global Options

The function recognizes the following global options that modify its
default behavior:

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
#>  Values      │       3        4       5 │      Total 
#> ─────────────┼──────────────────────────┼────────────
#>  4           │       1        8       2 │         11 
#>  6           │       2        4       1 │          7 
#>  8           │      12        0       2 │         14 
#> ─────────────┼──────────────────────────┼────────────
#>  Total       │      15       12       5 │         32 
#> ─────────────┴──────────────────────────┴────────────
#> Chi-2: 18.0 (df = 4), p = 0.001
#> Cramer's V: 0.53
#> Warning: 6 expected cells < 5 (66.7%). Minimum expected = 1.09. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.

# Weighted (rescaled)
cross_tab(mtcars, cyl, gear, weights = mtcars$mpg, rescale = TRUE)
#> Crosstable: cyl x gear (N)
#>  Values      │       3        4       5 │      Total 
#> ─────────────┼──────────────────────────┼────────────
#>  4           │       1       11       3 │         15 
#>  6           │       2        4       1 │          7 
#>  8           │       9        0       2 │         11 
#> ─────────────┼──────────────────────────┼────────────
#>  Total       │      12       15       6 │         32 
#> ─────────────┴──────────────────────────┴────────────
#> Chi-2: 17.7 (df = 4), p = 0.001
#> Cramer's V: 0.53
#> Warning: 7 expected cells < 5 (77.8%). Minimum expected = 1.14. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.
#> Weight: mpg (rescaled)

# Grouped
cross_tab(mtcars, cyl, gear, by = am)
#> Crosstable: cyl x gear (N) | am = 0
#>  Values      │       3       4       5 │      Total 
#> ─────────────┼─────────────────────────┼────────────
#>  4           │       1       2       0 │          3 
#>  6           │       2       2       0 │          4 
#>  8           │      12       0       0 │         12 
#> ─────────────┼─────────────────────────┼────────────
#>  Total       │      15       4       0 │         19 
#> ─────────────┴─────────────────────────┴────────────
#> Chi-2: NA (df = 4), p = NA
#> Cramer's V: NA
#> Warning: 8 expected cells < 5 (88.9%). 5 expected cells < 1. Minimum expected = 0. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.
#> 
#> Crosstable: cyl x gear (N) | am = 1
#>  Values      │      3       4       5 │      Total 
#> ─────────────┼────────────────────────┼────────────
#>  4           │      0       6       2 │          8 
#>  6           │      0       2       1 │          3 
#>  8           │      0       0       2 │          2 
#> ─────────────┼────────────────────────┼────────────
#>  Total       │      0       8       5 │         13 
#> ─────────────┴────────────────────────┴────────────
#> Chi-2: NA (df = 4), p = NA
#> Cramer's V: NA
#> Warning: 9 expected cells < 5 (100%). 4 expected cells < 1. Minimum expected = 0. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.

# Grouped by an interaction
cross_tab(mtcars, cyl, gear, by = interaction(vs, am))
#> Crosstable: cyl x gear (N) | vs x am = 0.0
#>  Values      │       3       4       5 │      Total 
#> ─────────────┼─────────────────────────┼────────────
#>  4           │       0       0       0 │          0 
#>  6           │       0       0       0 │          0 
#>  8           │      12       0       0 │         12 
#> ─────────────┼─────────────────────────┼────────────
#>  Total       │      12       0       0 │         12 
#> ─────────────┴─────────────────────────┴────────────
#> Chi-2 and Cramer's V not computed: insufficient data (only one non-empty row/column).
#> 
#> Crosstable: cyl x gear (N) | vs x am = 1.0
#>  Values      │      3       4       5 │      Total 
#> ─────────────┼────────────────────────┼────────────
#>  4           │      1       2       0 │          3 
#>  6           │      2       2       0 │          4 
#>  8           │      0       0       0 │          0 
#> ─────────────┼────────────────────────┼────────────
#>  Total       │      3       4       0 │          7 
#> ─────────────┴────────────────────────┴────────────
#> Chi-2: NA (df = 4), p = NA
#> Cramer's V: NA
#> Warning: 9 expected cells < 5 (100%). 5 expected cells < 1. Minimum expected = 0. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.
#> 
#> Crosstable: cyl x gear (N) | vs x am = 0.1
#>  Values      │      3       4       5 │      Total 
#> ─────────────┼────────────────────────┼────────────
#>  4           │      0       0       1 │          1 
#>  6           │      0       2       1 │          3 
#>  8           │      0       0       2 │          2 
#> ─────────────┼────────────────────────┼────────────
#>  Total       │      0       2       4 │          6 
#> ─────────────┴────────────────────────┴────────────
#> Chi-2: NA (df = 4), p = NA
#> Cramer's V: NA
#> Warning: 9 expected cells < 5 (100%). 6 expected cells < 1. Minimum expected = 0. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.
#> 
#> Crosstable: cyl x gear (N) | vs x am = 1.1
#>  Values      │      3       4       5 │      Total 
#> ─────────────┼────────────────────────┼────────────
#>  4           │      0       6       1 │          7 
#>  6           │      0       0       0 │          0 
#>  8           │      0       0       0 │          0 
#> ─────────────┼────────────────────────┼────────────
#>  Total       │      0       6       1 │          7 
#> ─────────────┴────────────────────────┴────────────
#> Chi-2 and Cramer's V not computed: insufficient data (only one non-empty row/column).

# Set default percent mode globally
options(spicy.percent = "column")

# Now this will display column percentages by default
cross_tab(mtcars, cyl, gear)
#> Crosstable: cyl x gear (Column %)
#>  Values      │          3           4           5 │      Total 
#> ─────────────┼────────────────────────────────────┼────────────
#>  4           │        6.7        66.7        40.0 │       34.4 
#>  6           │       13.3        33.3        20.0 │       21.9 
#>  8           │       80.0         0.0        40.0 │       43.8 
#> ─────────────┼────────────────────────────────────┼────────────
#>  Total       │      100.0       100.0       100.0 │      100.0 
#>  N           │         15          12           5 │         32 
#> ─────────────┴────────────────────────────────────┴────────────
#> Chi-2: 18.0 (df = 4), p = 0.001
#> Cramer's V: 0.53
#> Warning: 6 expected cells < 5 (66.7%). Minimum expected = 1.09. Consider `simulate_p = TRUE` or set globally via `options(spicy.simulate_p = TRUE)`.

# Reset to default behavior
options(spicy.percent = NULL)
```
