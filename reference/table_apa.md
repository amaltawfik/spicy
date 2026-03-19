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
# Raw long output (machine-friendly)
table_apa(
  data = sochealth,
  row_vars = c("smoking", "physical_activity"),
  group_var = "education",
  labels = c("Current smoker", "Physical activity"),
  output = "long",
  style = "raw"
)
#>             variable level           group   n  pct            p Cramer's V
#> 1     Current smoker    No Lower secondary 179 69.6 2.012877e-05  0.1356677
#> 2     Current smoker    No Upper secondary 415 78.7 2.012877e-05  0.1356677
#> 3     Current smoker    No        Tertiary 332 84.9 2.012877e-05  0.1356677
#> 4     Current smoker    No           Total 926 78.8 2.012877e-05  0.1356677
#> 5     Current smoker   Yes Lower secondary  78 30.4 2.012877e-05  0.1356677
#> 6     Current smoker   Yes Upper secondary 112 21.3 2.012877e-05  0.1356677
#> 7     Current smoker   Yes        Tertiary  59 15.1 2.012877e-05  0.1356677
#> 8     Current smoker   Yes           Total 249 21.2 2.012877e-05  0.1356677
#> 9  Physical activity    No Lower secondary 177 67.8 8.333584e-12  0.2061986
#> 10 Physical activity    No Upper secondary 310 57.5 8.333584e-12  0.2061986
#> 11 Physical activity    No        Tertiary 163 40.8 8.333584e-12  0.2061986
#> 12 Physical activity    No           Total 650 54.2 8.333584e-12  0.2061986
#> 13 Physical activity   Yes Lower secondary  84 32.2 8.333584e-12  0.2061986
#> 14 Physical activity   Yes Upper secondary 229 42.5 8.333584e-12  0.2061986
#> 15 Physical activity   Yes        Tertiary 237 59.2 8.333584e-12  0.2061986
#> 16 Physical activity   Yes           Total 550 45.8 8.333584e-12  0.2061986

# Raw wide output
table_apa(
  data = sochealth,
  row_vars = c("smoking", "physical_activity"),
  group_var = "education",
  labels = c("Current smoker", "Physical activity"),
  output = "wide",
  style = "raw"
)
#>            Variable Level Lower secondary n Lower secondary % Upper secondary n
#> 1    Current smoker    No               179              69.6               415
#> 2    Current smoker   Yes                78              30.4               112
#> 3 Physical activity    No               177              67.8               310
#> 4 Physical activity   Yes                84              32.2               229
#>   Upper secondary % Tertiary n Tertiary % Total n Total %            p
#> 1              78.7        332       84.9     926    78.8 2.012877e-05
#> 2              21.3         59       15.1     249    21.2 2.012877e-05
#> 3              57.5        163       40.8     650    54.2 8.333584e-12
#> 4              42.5        237       59.2     550    45.8 8.333584e-12
#>   Cramer's V
#> 1  0.1356677
#> 2  0.1356677
#> 3  0.2061986
#> 4  0.2061986

# Weighted example
table_apa(
  data = sochealth,
  row_vars = c("smoking", "physical_activity"),
  group_var = "education",
  labels = c("Current smoker", "Physical activity"),
  weights = "weight",
  rescale = TRUE,
  simulate_p = FALSE,
  output = "long",
  style = "raw"
)
#>             variable level           group        n  pct            p
#> 1     Current smoker    No Lower secondary 177.0000 69.1 1.866089e-06
#> 2     Current smoker    No Upper secondary 427.0000 79.8 1.866089e-06
#> 3     Current smoker    No        Tertiary 329.0000 85.9 1.866089e-06
#> 4     Current smoker    No           Total 933.4983 79.4 1.866089e-06
#> 5     Current smoker   Yes Lower secondary  79.0000 30.9 1.866089e-06
#> 6     Current smoker   Yes Upper secondary 108.0000 20.2 1.866089e-06
#> 7     Current smoker   Yes        Tertiary  54.0000 14.1 1.866089e-06
#> 8     Current smoker   Yes           Total 241.5017 20.6 1.866089e-06
#> 9  Physical activity    No Lower secondary 177.0000 68.0 7.411895e-11
#> 10 Physical activity    No Upper secondary 324.0000 59.0 7.411895e-11
#> 11 Physical activity    No        Tertiary 166.0000 42.4 7.411895e-11
#> 12 Physical activity    No           Total 666.2816 55.5 7.411895e-11
#> 13 Physical activity   Yes Lower secondary  83.0000 32.0 7.411895e-11
#> 14 Physical activity   Yes Upper secondary 225.0000 41.0 7.411895e-11
#> 15 Physical activity   Yes        Tertiary 226.0000 57.6 7.411895e-11
#> 16 Physical activity   Yes           Total 533.7184 44.5 7.411895e-11
#>    Cramer's V
#> 1   0.1498463
#> 2   0.1498463
#> 3   0.1498463
#> 4   0.1498463
#> 5   0.1498463
#> 6   0.1498463
#> 7   0.1498463
#> 8   0.1498463
#> 9   0.1971689
#> 10  0.1971689
#> 11  0.1971689
#> 12  0.1971689
#> 13  0.1971689
#> 14  0.1971689
#> 15  0.1971689
#> 16  0.1971689

# \donttest{
# Optional output: tinytable
if (requireNamespace("tinytable", quietly = TRUE)) {
  table_apa(
    data = sochealth,
    row_vars = c("smoking", "physical_activity"),
    group_var = "education",
    labels = c("Current smoker", "Physical activity"),
    output = "tinytable"
  )
}
#> +-------------------+--------+---------+--------+---------+-----+------+-----+------+--------+------------+
#> | Variable          | Lower secondary  | Upper secondary  | Tertiary   | Total      | p      | Cramer's V |
#> +-------------------+--------+---------+--------+---------+-----+------+-----+------+--------+------------+
#> |                   | n      | %       | n      | %       | n   | %    | n   | %    |        |            |
#> +===================+========+=========+========+=========+=====+======+=====+======+========+============+
#> | Current smoker    |        |         |        |         |     |      |     |      | < .001 | .14        |
#> +-------------------+--------+---------+--------+---------+-----+------+-----+------+--------+------------+
#> |        No         | 179    | 69.6    | 415    | 78.7    | 332 | 84.9 | 926 | 78.8 |        |            |
#> +-------------------+--------+---------+--------+---------+-----+------+-----+------+--------+------------+
#> |        Yes        | 78     | 30.4    | 112    | 21.3    | 59  | 15.1 | 249 | 21.2 |        |            |
#> +-------------------+--------+---------+--------+---------+-----+------+-----+------+--------+------------+
#> | Physical activity |        |         |        |         |     |      |     |      | < .001 | .21        |
#> +-------------------+--------+---------+--------+---------+-----+------+-----+------+--------+------------+
#> |        No         | 177    | 67.8    | 310    | 57.5    | 163 | 40.8 | 650 | 54.2 |        |            |
#> +-------------------+--------+---------+--------+---------+-----+------+-----+------+--------+------------+
#> |        Yes        | 84     | 32.2    | 229    | 42.5    | 237 | 59.2 | 550 | 45.8 |        |            |
#> +-------------------+--------+---------+--------+---------+-----+------+-----+------+--------+------------+ 

# Optional output: Excel
if (requireNamespace("openxlsx", quietly = TRUE)) {
  table_apa(
    data = sochealth,
    row_vars = c("smoking", "physical_activity"),
    group_var = "education",
    labels = c("Current smoker", "Physical activity"),
    output = "excel",
    excel_path = tempfile(fileext = ".xlsx")
  )
}
# }
```
