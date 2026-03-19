# Build APA-Style Cross-Tabulation Tables

`table_apa()` builds a publication-ready table by crossing one grouping
variable (`group_var`) with one or many row variables (`row_vars`),
using
[`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
internally.

## Usage

``` r
table_apa(
  data,
  row_vars,
  group_var,
  labels = NULL,
  levels_keep = NULL,
  include_total = TRUE,
  drop_na = TRUE,
  weights = NULL,
  rescale = FALSE,
  correct = FALSE,
  simulate_p = FALSE,
  simulate_B = 2000,
  percent_digits = 1,
  p_digits = 3,
  v_digits = 2,
  assoc_measure = "auto",
  assoc_ci = FALSE,
  decimal_mark = ".",
  output = c("wide", "long", "tinytable", "gt", "flextable", "excel", "clipboard",
    "word"),
  style = c("auto", "raw", "report"),
  indent_text = "  ",
  indent_text_excel_clipboard = "      ",
  add_multilevel_header = TRUE,
  blank_na_wide = FALSE,
  excel_path = NULL,
  excel_sheet = "APA",
  clipboard_delim = "\t",
  word_path = NULL
)
```

## Arguments

- data:

  A data frame.

- row_vars:

  Character vector of variable names to place in rows.

- group_var:

  Single character variable name used for columns/groups.

- labels:

  Optional character labels for `row_vars` (same length).

- levels_keep:

  Optional character vector of levels to keep/order for row modalities.
  If `NULL`, all observed levels are kept.

- include_total:

  Logical. If `TRUE` (the default), includes a `Total` group when
  available.

- drop_na:

  Logical. If `TRUE` (the default), removes rows with `NA` in the
  row/group variable before each cross-tabulation. If `FALSE`, missing
  values are displayed as a dedicated `"(Missing)"` level.

- weights:

  Optional weights. Either `NULL` (the default), a numeric vector of
  length `nrow(data)`, or a single column name in `data`.

- rescale:

  Logical. If `FALSE` (the default), weights are used as-is. If `TRUE`,
  rescales weights so total weighted N matches raw N. Passed to
  [`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).

- correct:

  Logical. If `FALSE` (the default), no continuity correction is
  applied. If `TRUE`, applies Yates correction in 2x2 chi-squared
  contexts. Passed to
  [`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).

- simulate_p:

  Logical. If `FALSE` (the default), uses asymptotic p-values. If
  `TRUE`, uses Monte Carlo simulation. Passed to
  [`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).

- simulate_B:

  Integer. Number of Monte Carlo replicates when `simulate_p = TRUE`.
  Defaults to `2000`.

- percent_digits:

  Number of digits for percentages in report outputs. Defaults to `1`.

- p_digits:

  Number of digits for p-values (except `< .001`). Defaults to `3`.

- v_digits:

  Number of digits for the association measure. Defaults to `2`.

- assoc_measure:

  Passed to
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
  Which association measure to report (`"auto"`, `"cramer_v"`, `"phi"`,
  `"gamma"`, `"tau_b"`, `"tau_c"`, `"somers_d"`, `"lambda"`, `"none"`).
  Defaults to `"auto"`.

- assoc_ci:

  Passed to
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
  If `TRUE`, includes the confidence interval. Defaults to `FALSE`.

- decimal_mark:

  Decimal separator (`"."` or `","`). Defaults to `"."`.

- output:

  Output format: `"wide"` (the default), `"long"`, `"tinytable"`,
  `"gt"`, `"flextable"`, `"excel"`, `"clipboard"`, `"word"`.

- style:

  `"auto"` (the default) to select by output type, `"raw"` for
  machine-friendly outputs, `"report"` for formatted outputs.

- indent_text:

  Prefix used for modality labels in report table building. Defaults to
  `" "` (two spaces).

- indent_text_excel_clipboard:

  Stronger indentation used in Excel and clipboard exports. Defaults to
  six non-breaking spaces.

- add_multilevel_header:

  Logical. If `TRUE` (the default), merges top headers in Excel export.

- blank_na_wide:

  Logical. If `FALSE` (the default), `NA` values are kept as-is in wide
  raw output. If `TRUE`, replaces them with empty strings.

- excel_path:

  Path for `output = "excel"`. Defaults to `NULL`.

- excel_sheet:

  Sheet name for Excel export. Defaults to `"APA"`.

- clipboard_delim:

  Delimiter for clipboard text export. Defaults to `"\t"`.

- word_path:

  Path for `output = "word"` or optional save path when
  `output = "flextable"`. Defaults to `NULL`.

## Value

Depends on `output` and `style`:

- `"long"` + `"raw"`: long numeric data frame.

- `"wide"` + `"raw"`: wide numeric data frame.

- `"long"` + `"report"`: long formatted character data frame.

- `"wide"` + `"report"`: wide formatted character data frame.

- `"tinytable"`: a `tinytable` object.

- `"gt"`: a `gt_tbl` object.

- `"flextable"`: a `flextable` object.

- `"excel"` / `"clipboard"` / `"word"`: invisibly returns written
  object/path.

## Details

It supports raw data outputs (`wide`, `long`) and report-oriented
outputs (`tinytable`, `flextable`, `excel`, `clipboard`, `word`) with
multi-level headers, p-values, and an association measure.

Optional output engines require suggested packages:

- `tinytable` for `output = "tinytable"`

- `gt` for `output = "gt"`

- `flextable` + `officer` for `output = "flextable"`/`"word"`

- `openxlsx` for `output = "excel"`

- `clipr` for `output = "clipboard"`

## Examples

``` r
# Build a dataset from mtcars
d <- transform(
  mtcars,
  transmission = factor(am, labels = c("Automatic", "Manual")),
  engine = factor(vs, labels = c("V-shaped", "Straight")),
  cylinders = factor(cyl),
  w = mpg
)

# Raw long output (machine-friendly)
table_apa(
  data = d,
  row_vars = c("transmission", "engine"),
  group_var = "cylinders",
  labels = c("Transmission", "Engine type"),
  output = "long",
  style = "raw"
)
#>        variable     level group  n   pct            p Cramer's V
#> 1  Transmission Automatic     4  3  27.3 1.264661e-02  0.5226355
#> 2  Transmission Automatic     6  4  57.1 1.264661e-02  0.5226355
#> 3  Transmission Automatic     8 12  85.7 1.264661e-02  0.5226355
#> 4  Transmission Automatic Total 19  59.4 1.264661e-02  0.5226355
#> 5  Transmission    Manual     4  8  72.7 1.264661e-02  0.5226355
#> 6  Transmission    Manual     6  3  42.9 1.264661e-02  0.5226355
#> 7  Transmission    Manual     8  2  14.3 1.264661e-02  0.5226355
#> 8  Transmission    Manual Total 13  40.6 1.264661e-02  0.5226355
#> 9   Engine type  Straight     4 10  90.9 2.323235e-05  0.8166228
#> 10  Engine type  Straight     6  4  57.1 2.323235e-05  0.8166228
#> 11  Engine type  Straight     8  0   0.0 2.323235e-05  0.8166228
#> 12  Engine type  Straight Total 14  43.8 2.323235e-05  0.8166228
#> 13  Engine type  V-shaped     4  1   9.1 2.323235e-05  0.8166228
#> 14  Engine type  V-shaped     6  3  42.9 2.323235e-05  0.8166228
#> 15  Engine type  V-shaped     8 14 100.0 2.323235e-05  0.8166228
#> 16  Engine type  V-shaped Total 18  56.2 2.323235e-05  0.8166228

# Raw wide output
table_apa(
  data = d,
  row_vars = c("transmission", "engine"),
  group_var = "cylinders",
  labels = c("Transmission", "Engine type"),
  output = "wide",
  style = "raw"
)
#>       Variable     Level 4 n  4 % 6 n  6 % 8 n   8 % Total n Total %
#> 1 Transmission Automatic   3 27.3   4 57.1  12  85.7      19    59.4
#> 2 Transmission    Manual   8 72.7   3 42.9   2  14.3      13    40.6
#> 3  Engine type  Straight  10 90.9   4 57.1   0   0.0      14    43.8
#> 4  Engine type  V-shaped   1  9.1   3 42.9  14 100.0      18    56.2
#>              p Cramer's V
#> 1 1.264661e-02  0.5226355
#> 2 1.264661e-02  0.5226355
#> 3 2.323235e-05  0.8166228
#> 4 2.323235e-05  0.8166228

# Weighted example
table_apa(
  data = d,
  row_vars = c("transmission", "engine"),
  group_var = "cylinders",
  labels = c("Transmission", "Engine type"),
  weights = "w",
  rescale = TRUE,
  simulate_p = FALSE,
  output = "long",
  style = "raw"
)
#>        variable     level group        n   pct           p Cramer's V
#> 1  Transmission Automatic     4  3.00000  23.4 0.008725743  0.5443734
#> 2  Transmission Automatic     6  4.00000  55.4 0.008725743  0.5443734
#> 3  Transmission Automatic     8  9.00000  85.4 0.008725743  0.5443734
#> 4  Transmission Automatic Total 16.21652  50.7 0.008725743  0.5443734
#> 5  Transmission    Manual     4 11.00000  76.6 0.008725743  0.5443734
#> 6  Transmission    Manual     6  3.00000  44.6 0.008725743  0.5443734
#> 7  Transmission    Manual     8  2.00000  14.6 0.008725743  0.5443734
#> 8  Transmission    Manual Total 15.78348  49.3 0.008725743  0.5443734
#> 9   Engine type  Straight     4 13.00000  91.1 0.000036682  0.7989534
#> 10  Engine type  Straight     6  4.00000  55.4 0.000036682  0.7989534
#> 11  Engine type  Straight     8  0.00000   0.0 0.000036682  0.7989534
#> 12  Engine type  Straight Total 17.11246  53.5 0.000036682  0.7989534
#> 13  Engine type  V-shaped     4  1.00000   8.9 0.000036682  0.7989534
#> 14  Engine type  V-shaped     6  3.00000  44.6 0.000036682  0.7989534
#> 15  Engine type  V-shaped     8 11.00000 100.0 0.000036682  0.7989534
#> 16  Engine type  V-shaped Total 14.88754  46.5 0.000036682  0.7989534

# \donttest{
# Optional output: tinytable
if (requireNamespace("tinytable", quietly = TRUE)) {
  tt_ex <- table_apa(
    data = d,
    row_vars = c("transmission", "engine"),
    group_var = "cylinders",
    labels = c("Transmission", "Engine type"),
    output = "tinytable"
  )
}

# Optional output: Excel
if (requireNamespace("openxlsx", quietly = TRUE)) {
  table_apa(
    data = d,
    row_vars = c("transmission", "engine"),
    group_var = "cylinders",
    labels = c("Transmission", "Engine type"),
    output = "excel",
    excel_path = tempfile(fileext = ".xlsx")
  )
}
# }
```
