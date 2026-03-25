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
  assoc_measure = c("auto", "cramer_v", "phi", "gamma", "tau_b", "tau_c", "somers_d",
    "lambda", "none"),
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
    cross_tab(sochealth, smoking, education, weights = weight)

## Examples

``` r
# Basic crosstab
cross_tab(sochealth, smoking, education)
#> Crosstable: smoking x education (N)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                  179                   415            332 
#>  Yes         │                   78                   112             59 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  257                   527            391 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        926 
#>  Yes         │        249 
#> ─────────────┼────────────
#>  Total       │       1175 
#> 
#> Chi-2(2) = 21.6, p < 0.001
#> Cramer's V = 0.14

# Column percentages
cross_tab(sochealth, smoking, education, percent = "column")
#> Crosstable: smoking x education (Column %)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                 69.6                  78.7           84.9 
#>  Yes         │                 30.4                  21.3           15.1 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                100.0                 100.0          100.0 
#>  N           │                  257                   527            391 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │       78.8 
#>  Yes         │       21.2 
#> ─────────────┼────────────
#>  Total       │      100.0 
#>  N           │       1175 
#> 
#> Chi-2(2) = 21.6, p < 0.001
#> Cramer's V = 0.14

# Weighted (rescaled)
cross_tab(sochealth, smoking, education, weights = weight, rescale = TRUE)
#> Crosstable: smoking x education (N)
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                  176                   419            325 
#>  Yes         │                   79                   115             60 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  255                   534            385 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        921 
#>  Yes         │        254 
#> ─────────────┼────────────
#>  Total       │       1175 
#> 
#> Chi-2(2) = 21.4, p < 0.001
#> Cramer's V = 0.13
#> Weight: weight (rescaled)

# Grouped by sex
cross_tab(sochealth, smoking, education, by = sex)
#> Crosstable: smoking x education (N) | sex = Female
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   95                   220            160 
#>  Yes         │                   38                    62             31 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  133                   282            191 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        475 
#>  Yes         │        131 
#> ─────────────┼────────────
#>  Total       │        606 
#> 
#> Chi-2(2) = 7.1, p = 0.029
#> Cramer's V = 0.11
#> 
#> Crosstable: smoking x education (N) | sex = Male
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   84                   195            172 
#>  Yes         │                   40                    50             28 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                  124                   245            200 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        451 
#>  Yes         │        118 
#> ─────────────┼────────────
#>  Total       │        569 
#> 
#> Chi-2(2) = 15.6, p < 0.001
#> Cramer's V = 0.17

# Grouped by combination of variables
cross_tab(sochealth, smoking, education, by = interaction(sex, age_group))
#> Crosstable: smoking x education (N) | sex x age_group = Female.25-34
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   23                    49             29 
#>  Yes         │                    9                     9              7 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   32                    58             36 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        101 
#>  Yes         │         25 
#> ─────────────┼────────────
#>  Total       │        126 
#> 
#> Chi-2(2) = 2.1, p = 0.356
#> Cramer's V = 0.13
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Male.25-34
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                    9                    42             32 
#>  Yes         │                   11                    11              4 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   20                    53             36 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │         83 
#>  Yes         │         26 
#> ─────────────┼────────────
#>  Total       │        109 
#> 
#> Chi-2(2) = 14.2, p < 0.001
#> Cramer's V = 0.36
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Female.35-49
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   24                    73             48 
#>  Yes         │                   10                    20              8 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   34                    93             56 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        145 
#>  Yes         │         38 
#> ─────────────┼────────────
#>  Total       │        183 
#> 
#> Chi-2(2) = 3.0, p = 0.223
#> Cramer's V = 0.13
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Male.35-49
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   33                    59             60 
#>  Yes         │                   14                    17              7 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   47                    76             67 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        152 
#>  Yes         │         38 
#> ─────────────┼────────────
#>  Total       │        190 
#> 
#> Chi-2(2) = 6.9, p = 0.032
#> Cramer's V = 0.19
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Female.50-64
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   28                    63             45 
#>  Yes         │                    8                    16              6 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   36                    79             51 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        136 
#>  Yes         │         30 
#> ─────────────┼────────────
#>  Total       │        166 
#> 
#> Chi-2(2) = 2.0, p = 0.360
#> Cramer's V = 0.11
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Male.50-64
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   28                    58             42 
#>  Yes         │                    8                    13              5 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   36                    71             47 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │        128 
#>  Yes         │         26 
#> ─────────────┼────────────
#>  Total       │        154 
#> 
#> Chi-2(2) = 2.1, p = 0.343
#> Cramer's V = 0.12
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Female.65-75
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   20                    35             38 
#>  Yes         │                   11                    17             10 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   31                    52             48 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │         93 
#>  Yes         │         38 
#> ─────────────┼────────────
#>  Total       │        131 
#> 
#> Chi-2(2) = 2.5, p = 0.282
#> Cramer's V = 0.14
#> 
#> Crosstable: smoking x education (N) | sex x age_group = Male.65-75
#> 
#>  Values      │      Lower secondary       Upper secondary       Tertiary 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  No          │                   14                    36             38 
#>  Yes         │                    7                     9             12 
#> ─────────────┼───────────────────────────────────────────────────────────
#>  Total       │                   21                    45             50 
#> 
#>  Values      │      Total 
#> ─────────────┼────────────
#>  No          │         88 
#>  Yes         │         28 
#> ─────────────┼────────────
#>  Total       │        116 
#> 
#> Chi-2(2) = 1.4, p = 0.499
#> Cramer's V = 0.11

# Ordinal variables: auto-selects Kendall's Tau-b
cross_tab(sochealth, education, self_rated_health)
#> Crosstable: education x self_rated_health (N)
#> 
#>  Values               │      Poor       Fair       Good       Very good 
#> ──────────────────────┼─────────────────────────────────────────────────
#>  Lower secondary      │        28         86        102              44 
#>  Upper secondary      │        28        118        263             118 
#>  Tertiary             │         5         62        193             133 
#> ──────────────────────┼─────────────────────────────────────────────────
#>  Total                │        61        266        558             295 
#> 
#>  Values               │      Total 
#> ──────────────────────┼────────────
#>  Lower secondary      │        260 
#>  Upper secondary      │        527 
#>  Tertiary             │        393 
#> ──────────────────────┼────────────
#>  Total                │       1180 
#> 
#> Chi-2(6) = 73.2, p < 0.001
#> Kendall's Tau-b = 0.20

# 2x2 table with Yates correction
cross_tab(sochealth, smoking, physical_activity, correct = TRUE)
#> Crosstable: smoking x physical_activity (N)
#> 
#>  Values      │       No       Yes │      Total 
#> ─────────────┼────────────────────┼────────────
#>  No          │      505       421 │        926 
#>  Yes         │      134       115 │        249 
#> ─────────────┼────────────────────┼────────────
#>  Total       │      639       536 │       1175 
#> 
#> Chi-2(1) = 0.0, p = 0.896
#> Cramer's V = 0.01
#> Yates continuity correction applied.
```
