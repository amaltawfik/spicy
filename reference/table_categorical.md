# Categorical summary table

`table_categorical()` builds a publication-ready table for one or many
selected categorical variables. With `by`, it produces grouped
cross-tabulation summaries using
[`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
internally. Without `by`, it produces one-way frequency-style summaries.

## Usage

``` r
table_categorical(
  data,
  select,
  by = NULL,
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
  output = c("wide", "default", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  styled = TRUE,
  style = c("auto", "raw", "report"),
  indent_text = "  ",
  indent_text_excel_clipboard = strrep(" ", 6),
  add_multilevel_header = TRUE,
  blank_na_wide = FALSE,
  excel_path = NULL,
  excel_sheet = "Categorical",
  clipboard_delim = "\t",
  word_path = NULL
)
```

## Arguments

- data:

  A data frame.

- select:

  Columns to include as row variables. Supports tidyselect syntax and
  character vectors of column names.

- by:

  Optional grouping column used for columns/groups. Accepts an unquoted
  column name or a single character column name.

- labels:

  Optional character labels for `select` (same length).

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
  length `nrow(data)`, or a single column in `data` supplied as an
  unquoted name or a character string.

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
  If `TRUE`, includes the confidence interval. In data/export formats
  (`wide`, `long`, `excel`, `clipboard`), two extra columns `CI lower`
  and `CI upper` are added. In rendered formats (`gt`, `tinytable`,
  `flextable`, `word`), the CI is shown inline as `.14 [.08, .19]` in
  the association measure column. Defaults to `FALSE`.

- decimal_mark:

  Decimal separator (`"."` or `","`). Defaults to `"."`.

- output:

  Output format: `"wide"` (the default), `"default"` for an ASCII
  console table, `"long"`, `"tinytable"`, `"gt"`, `"flextable"`,
  `"excel"`, `"clipboard"`, `"word"`.

- styled:

  Logical. Used only when `output = "default"`. If `TRUE` (the default),
  prints a styled ASCII table and returns it invisibly. If `FALSE`,
  returns the underlying wide raw `data.frame`.

- style:

  `"auto"` (the default) to select by output type, `"raw"` for plain
  outputs, `"report"` for formatted outputs.

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

  Sheet name for Excel export. Defaults to `"Categorical"`.

- clipboard_delim:

  Delimiter for clipboard text export. Defaults to `"\t"`.

- word_path:

  Path for `output = "word"` or optional save path when
  `output = "flextable"`. Defaults to `NULL`.

## Value

Depends on `output` and `style`:

- `"default"` + `styled = TRUE`: prints a styled ASCII table and returns
  the underlying data frame invisibly.

- `"default"` + `styled = FALSE`: wide numeric data frame.

- `"long"` + `"raw"`: long numeric data frame.

- `"wide"` + `"raw"`: wide numeric data frame.

- `"long"` + `"report"`: long formatted character data frame.

- `"wide"` + `"report"`: wide formatted character data frame.

- `"tinytable"`: a `tinytable` object.

- `"gt"`: a `gt_tbl` object.

- `"flextable"`: a `flextable` object.

- `"excel"` / `"word"`: writes to disk and returns the file path
  invisibly.

- `"clipboard"`: copies the table and returns the text invisibly.

## Details

It supports raw data outputs (`wide`, `long`) and report-oriented
outputs (`default`, `tinytable`, `gt`, `flextable`, `excel`,
`clipboard`, `word`) with multi-level headers and, when `by` is used,
p-values and optional association measures for publication tables and
APA-style reporting workflows.

Optional output engines require suggested packages:

- `tinytable` for `output = "tinytable"`

- `gt` for `output = "gt"`

- `flextable` for `output = "flextable"`

- `flextable` + `officer` for `output = "word"`

- `openxlsx` for `output = "excel"`

- `clipr` for `output = "clipboard"`

## Examples

``` r
# Raw long output
table_categorical(
  data = sochealth,
  select = c(smoking, physical_activity),
  by = education,
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

# ASCII console output
table_categorical(
  data = sochealth,
  select = c(smoking, physical_activity),
  by = sex,
  output = "default"
)
#> Categorical table by sex
#> 
#>  Variable          │ Female n  Female %  Male n  Male %  Total n  Total %     p 
#> ───────────────────┼────────────────────────────────────────────────────────────
#>  smoking           │                                                       .713 
#>    No              │      475      78.4     451    79.3      926     78.8       
#>    Yes             │      131      21.6     118    20.7      249     21.2       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │                                                       .832 
#>    No              │      334      53.9     316    54.5      650     54.2       
#>    Yes             │      286      46.1     264    45.5      550     45.8       
#> 
#>  Variable          │ Cramer's V 
#> ───────────────────┼────────────
#>  smoking           │        .01 
#>    No              │            
#>    Yes             │            
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │        .01 
#>    No              │            
#>    Yes             │            

# One-way frequency-style table
table_categorical(
  data = sochealth,
  select = c(smoking, physical_activity),
  output = "default"
)
#> Categorical table
#> 
#>  Variable               │        n          % 
#> ────────────────────────┼─────────────────────
#>  smoking                │                     
#>    No                   │      926       78.8 
#>    Yes                  │      249       21.2 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity      │                     
#>    No                   │      650       54.2 
#>    Yes                  │      550       45.8 

# Keep missing values as an explicit level
table_categorical(
  data = sochealth,
  select = income_group,
  by = sex,
  drop_na = FALSE,
  output = "wide",
  style = "report"
)
#>         Variable Female n Female % Male n Male % Total n Total %    p
#> 1   income_group                                                 .698
#> 2            Low      137     22.1    110   19.0     247    20.6     
#> 3   Lower middle      198     31.9    190   32.8     388    32.3     
#> 4   Upper middle      162     26.1    166   28.6     328    27.3     
#> 5           High      114     18.4    105   18.1     219    18.2     
#> 6      (Missing)        9      1.5      9    1.6      18     1.5     
#>   Cramer's V
#> 1        .04
#> 2           
#> 3           
#> 4           
#> 5           
#> 6           

# Raw wide output
table_categorical(
  data = sochealth,
  select = c(smoking, physical_activity),
  by = education,
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
table_categorical(
  data = sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Current smoker", "Physical activity"),
  weights = "weight",
  rescale = TRUE,
  simulate_p = FALSE,
  output = "long",
  style = "raw"
)
#>             variable level           group        n  pct            p
#> 1     Current smoker    No Lower secondary 176.0000 69.0 2.306438e-05
#> 2     Current smoker    No Upper secondary 419.0000 78.5 2.306438e-05
#> 3     Current smoker    No        Tertiary 325.0000 84.4 2.306438e-05
#> 4     Current smoker    No           Total 920.8641 78.4 2.306438e-05
#> 5     Current smoker   Yes Lower secondary  79.0000 31.0 2.306438e-05
#> 6     Current smoker   Yes Upper secondary 115.0000 21.5 2.306438e-05
#> 7     Current smoker   Yes        Tertiary  60.0000 15.6 2.306438e-05
#> 8     Current smoker   Yes           Total 254.1359 21.6 2.306438e-05
#> 9  Physical activity    No Lower secondary 174.0000 67.2 2.269974e-10
#> 10 Physical activity    No Upper secondary 315.0000 57.7 2.269974e-10
#> 11 Physical activity    No        Tertiary 166.0000 41.9 2.269974e-10
#> 12 Physical activity    No           Total 654.7619 54.6 2.269974e-10
#> 13 Physical activity   Yes Lower secondary  85.0000 32.8 2.269974e-10
#> 14 Physical activity   Yes Upper secondary 231.0000 42.3 2.269974e-10
#> 15 Physical activity   Yes        Tertiary 229.0000 58.1 2.269974e-10
#> 16 Physical activity   Yes           Total 545.2381 45.4 2.269974e-10
#>    Cramer's V
#> 1   0.1348110
#> 2   0.1348110
#> 3   0.1348110
#> 4   0.1348110
#> 5   0.1348110
#> 6   0.1348110
#> 7   0.1348110
#> 8   0.1348110
#> 9   0.1923802
#> 10  0.1923802
#> 11  0.1923802
#> 12  0.1923802
#> 13  0.1923802
#> 14  0.1923802
#> 15  0.1923802
#> 16  0.1923802

# \donttest{
# Optional output: tinytable
if (requireNamespace("tinytable", quietly = TRUE)) {
  table_categorical(
    data = sochealth,
    select = c(smoking, physical_activity),
    by = sex,
    labels = c("Current smoker", "Physical activity"),
    output = "tinytable"
  )
}
#> +-------------------+-----+------+-----+------+-----+------+------+------------+
#> | Variable          | Female     | Male       | Total      | p    | Cramer's V |
#> +-------------------+-----+------+-----+------+-----+------+------+------------+
#> |                   | n   | %    | n   | %    | n   | %    |      |            |
#> +===================+=====+======+=====+======+=====+======+======+============+
#> | Current smoker    |     |      |     |      |     |      | .713 | .01        |
#> +-------------------+-----+------+-----+------+-----+------+------+------------+
#> |      No           | 475 | 78.4 | 451 | 79.3 | 926 | 78.8 |      |            |
#> +-------------------+-----+------+-----+------+-----+------+------+------------+
#> |      Yes          | 131 | 21.6 | 118 | 20.7 | 249 | 21.2 |      |            |
#> +-------------------+-----+------+-----+------+-----+------+------+------------+
#> | Physical activity |     |      |     |      |     |      | .832 | .01        |
#> +-------------------+-----+------+-----+------+-----+------+------+------------+
#> |      No           | 334 | 53.9 | 316 | 54.5 | 650 | 54.2 |      |            |
#> +-------------------+-----+------+-----+------+-----+------+------+------------+
#> |      Yes          | 286 | 46.1 | 264 | 45.5 | 550 | 45.8 |      |            |
#> +-------------------+-----+------+-----+------+-----+------+------+------------+ 

# Optional output: Excel
if (requireNamespace("openxlsx", quietly = TRUE)) {
  table_categorical(
    data = sochealth,
    select = c(smoking, physical_activity),
    by = education,
    labels = c("Current smoker", "Physical activity"),
    output = "excel",
    excel_path = tempfile(fileext = ".xlsx")
  )
}
# }
```
