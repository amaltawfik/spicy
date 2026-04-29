# Categorical summary table

Builds a publication-ready frequency or cross-tabulation table for one
or many categorical variables selected with tidyselect syntax.

With `by`, produces grouped cross-tabulation summaries (using
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
internally) with Chi-squared *p*-values and optional association
measures. Without `by`, produces one-way frequency-style summaries.

Multiple output formats are available via `output`: a printed ASCII
table (`"default"`), a wide or long numeric `data.frame`
(`"data.frame"`, `"long"`), or publication-ready tables (`"tinytable"`,
`"gt"`, `"flextable"`, `"excel"`, `"clipboard"`, `"word"`).

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
  align = c("decimal", "auto", "center", "right"),
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
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

  Optional display labels for the variables. Two forms are accepted
  (matching
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  and
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)):

  - A **named character vector** whose names match column names in
    `data` (e.g. `c(bmi = "Body mass index")`); only listed columns are
    relabelled, others fall back to attribute-based labels or the column
    name. **Recommended form**.

  - A **positional character vector** of the same length as `select`, in
    the same order. Backward-compatible with the spicy \< 0.11.0 API.

  When `NULL` (the default), column names are used as-is. If a variable
  label attribute is present (e.g. from `haven`), it is *not* picked up
  here – pass `labels = c(...)` explicitly. (The continuous companions
  auto-detect attribute labels; the categorical function is conservative
  because the indented row labels expect predictable text.)

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
  If `TRUE`, includes the confidence interval of the association
  measure. In wide raw outputs (`"data.frame"`, `"excel"`,
  `"clipboard"`), two extra columns `CI lower` / `CI upper` are added;
  in the long raw output (`"long"`) the bounds appear as `ci_lower` /
  `ci_upper`. In rendered formats (`"gt"`, `"tinytable"`, `"flextable"`,
  `"word"`), the CI is shown inline (e.g., `.14 [.08, .19]`). Defaults
  to `FALSE`.

- decimal_mark:

  Decimal separator (`"."` or `","`). Defaults to `"."`.

- align:

  Horizontal alignment of numeric columns in the printed ASCII table and
  in the `tinytable` and `gt` outputs. The first column (`Variable`) is
  always left-aligned. One of:

  - `"decimal"` (default): align numeric columns on the decimal mark,
    the standard scientific-publication convention used by SPSS, SAS,
    LaTeX `siunitx`,
    [`gt::cols_align_decimal()`](https://gt.rstudio.com/reference/cols_align_decimal.html)
    and `tinytable::style_tt(align = "d")`. For ASCII print, values are
    pre-padded with leading and trailing spaces so the dots line up
    vertically.

  - `"center"` / `"right"`: literal alignment of numeric columns.

  - `"auto"`: legacy uniform right-alignment.

  The `flextable`, `word`, `excel`, and `clipboard` engines currently
  fall back to right-aligned numerics regardless of `align`; full
  decimal alignment for those engines is a planned follow-up. Same
  default and semantics as
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  /
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).

- output:

  Output format. One of:

  - `"default"` (a printed ASCII table, returned invisibly)

  - `"data.frame"` (a wide numeric `data.frame`)

  - `"long"` (a long numeric `data.frame`)

  - `"tinytable"` (requires `tinytable`)

  - `"gt"` (requires `gt`)

  - `"flextable"` (requires `flextable`)

  - `"excel"` (requires `openxlsx2`)

  - `"clipboard"` (requires `clipr`)

  - `"word"` (requires `flextable` and `officer`)

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

Depends on `output`:

- `"default"`: prints a styled ASCII table and returns the underlying
  `data.frame` invisibly (S3 class `"spicy_categorical_table"`).

- `"data.frame"`: a wide `data.frame` with one row per variable–level
  combination. When `by` is used, the columns are `Variable`, `Level`,
  and one pair of `n` / `\%` columns per group level (plus `Total` when
  `include_total = TRUE`), followed by `Chi2`, `df`, `p`, and the
  association measure column. When `by = NULL`, the columns are
  `Variable`, `Level`, `n`, `\%`.

- `"long"`: a long `data.frame` with columns `variable`, `level`,
  `group`, `n`, `percent` (and `chi2`, `df`, `p`, association measure
  columns when `by` is used).

- `"tinytable"`: a `tinytable` object.

- `"gt"`: a `gt_tbl` object.

- `"flextable"`: a `flextable` object.

- `"excel"` / `"word"`: writes to disk and returns the file path
  invisibly.

- `"clipboard"`: copies the table and returns the display `data.frame`
  invisibly.

## Tests

When `by` is used, each selected variable is cross-tabulated against the
grouping variable with
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
The omnibus chi-squared test (with optional Yates continuity correction
or Monte Carlo *p*-value, see `correct` / `simulate_p`) is computed and
reported in the `p` column. The chosen association measure
(`assoc_measure`, with `"auto"` selecting Cramer's V for nominal
variables and Kendall's Tau-b when both are ordered) is reported
alongside, with optional CI via `assoc_ci`. Without `by`, the table
reports the marginal frequency distribution of each variable with no
inferential statistics.

For model-based comparisons (cluster-robust SE, weighted contrasts,
fitted means) on continuous outcomes, see
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).
For descriptive (empirical) comparisons on continuous outcomes, see
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).

## Display conventions

By default (`align = "decimal"`) numeric columns are aligned on the
decimal mark, the standard scientific-publication convention used by
SPSS, SAS, LaTeX `siunitx`, and the native primitives of
[`gt::cols_align_decimal()`](https://gt.rstudio.com/reference/cols_align_decimal.html)
/ `tinytable::style_tt(align = "d")`. For the printed ASCII table the
alignment is achieved by padding numeric cells with leading and trailing
spaces so dots line up vertically. Pass `align = "auto"` to revert to
the legacy uniform right-alignment used in spicy \< 0.11.0.

*p*-values are formatted with `p_digits` decimal places (default 3, the
APA standard). Leading zeros on *p* are always stripped (`.045`, not
`0.045`).

Optional output engines require suggested packages:

- tinytable for `output = "tinytable"`

- gt for `output = "gt"`

- flextable for `output = "flextable"`

- flextable + officer for `output = "word"`

- openxlsx2 for `output = "excel"`

- clipr for `output = "clipboard"`

## See also

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
for empirical comparisons on continuous outcomes;
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
for the model-based companion (heteroskedasticity-consistent /
cluster-robust / bootstrap / jackknife SE, fitted means, weighted
contrasts);
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
for two-way cross-tabulations;
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) for
one-way frequency tables.

Other spicy tables:
[`spicy_tables`](https://amaltawfik.github.io/spicy/reference/spicy_tables.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)

## Examples

``` r
# Long numeric output
table_categorical(
  data = sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Current smoker", "Physical activity"),
  output = "long"
)
#>             variable level           group   n  pct     chi2 df            p
#> 1     Current smoker    No Lower secondary 179 69.6 21.62672  2 2.012877e-05
#> 2     Current smoker    No Upper secondary 415 78.7 21.62672  2 2.012877e-05
#> 3     Current smoker    No        Tertiary 332 84.9 21.62672  2 2.012877e-05
#> 4     Current smoker    No           Total 926 78.8 21.62672  2 2.012877e-05
#> 5     Current smoker   Yes Lower secondary  78 30.4 21.62672  2 2.012877e-05
#> 6     Current smoker   Yes Upper secondary 112 21.3 21.62672  2 2.012877e-05
#> 7     Current smoker   Yes        Tertiary  59 15.1 21.62672  2 2.012877e-05
#> 8     Current smoker   Yes           Total 249 21.2 21.62672  2 2.012877e-05
#> 9  Physical activity    No Lower secondary 177 67.8 51.02146  2 8.333584e-12
#> 10 Physical activity    No Upper secondary 310 57.5 51.02146  2 8.333584e-12
#> 11 Physical activity    No        Tertiary 163 40.8 51.02146  2 8.333584e-12
#> 12 Physical activity    No           Total 650 54.2 51.02146  2 8.333584e-12
#> 13 Physical activity   Yes Lower secondary  84 32.2 51.02146  2 8.333584e-12
#> 14 Physical activity   Yes Upper secondary 229 42.5 51.02146  2 8.333584e-12
#> 15 Physical activity   Yes        Tertiary 237 59.2 51.02146  2 8.333584e-12
#> 16 Physical activity   Yes           Total 550 45.8 51.02146  2 8.333584e-12
#>    Cramer's V
#> 1   0.1356677
#> 2   0.1356677
#> 3   0.1356677
#> 4   0.1356677
#> 5   0.1356677
#> 6   0.1356677
#> 7   0.1356677
#> 8   0.1356677
#> 9   0.2061986
#> 10  0.2061986
#> 11  0.2061986
#> 12  0.2061986
#> 13  0.2061986
#> 14  0.2061986
#> 15  0.2061986
#> 16  0.2061986

# ASCII console output (default)
table_categorical(
  data = sochealth,
  select = c(smoking, physical_activity),
  by = sex
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
  select = c(smoking, physical_activity)
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

# Wide numeric data.frame
table_categorical(
  data = sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Current smoker", "Physical activity"),
  output = "data.frame"
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
  output = "long"
)
#>             variable level           group        n  pct     chi2 df
#> 1     Current smoker    No Lower secondary 176.0000 69.0 21.35444  2
#> 2     Current smoker    No Upper secondary 419.0000 78.5 21.35444  2
#> 3     Current smoker    No        Tertiary 325.0000 84.4 21.35444  2
#> 4     Current smoker    No           Total 920.8641 78.4 21.35444  2
#> 5     Current smoker   Yes Lower secondary  79.0000 31.0 21.35444  2
#> 6     Current smoker   Yes Upper secondary 115.0000 21.5 21.35444  2
#> 7     Current smoker   Yes        Tertiary  60.0000 15.6 21.35444  2
#> 8     Current smoker   Yes           Total 254.1359 21.6 21.35444  2
#> 9  Physical activity    No Lower secondary 174.0000 67.2 44.41217  2
#> 10 Physical activity    No Upper secondary 315.0000 57.7 44.41217  2
#> 11 Physical activity    No        Tertiary 166.0000 41.9 44.41217  2
#> 12 Physical activity    No           Total 654.7619 54.6 44.41217  2
#> 13 Physical activity   Yes Lower secondary  85.0000 32.8 44.41217  2
#> 14 Physical activity   Yes Upper secondary 231.0000 42.3 44.41217  2
#> 15 Physical activity   Yes        Tertiary 229.0000 58.1 44.41217  2
#> 16 Physical activity   Yes           Total 545.2381 45.4 44.41217  2
#>               p Cramer's V
#> 1  2.306438e-05  0.1348110
#> 2  2.306438e-05  0.1348110
#> 3  2.306438e-05  0.1348110
#> 4  2.306438e-05  0.1348110
#> 5  2.306438e-05  0.1348110
#> 6  2.306438e-05  0.1348110
#> 7  2.306438e-05  0.1348110
#> 8  2.306438e-05  0.1348110
#> 9  2.269974e-10  0.1923802
#> 10 2.269974e-10  0.1923802
#> 11 2.269974e-10  0.1923802
#> 12 2.269974e-10  0.1923802
#> 13 2.269974e-10  0.1923802
#> 14 2.269974e-10  0.1923802
#> 15 2.269974e-10  0.1923802
#> 16 2.269974e-10  0.1923802

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
if (requireNamespace("openxlsx2", quietly = TRUE)) {
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
