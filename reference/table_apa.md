# Build APA-Style Cross-Tabulation Tables From Multiple Row Variables

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
  simulate_p = TRUE,
  simulate_B = 2000,
  percent_digits = 1,
  p_digits = 3,
  v_digits = 2,
  decimal_mark = ".",
  output = c("wide", "long", "tinytable", "flextable", "excel", "clipboard", "word"),
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

  Logical; include `Total` group if available.

- drop_na:

  Logical; if `TRUE`, remove rows with NA in row/group variable before
  each cross-tabulation. If `FALSE`, missing values are displayed as a
  dedicated `"(Missing)"` level.

- weights:

  Optional weights. Either `NULL`, a numeric vector of length
  `nrow(data)`, or a single column name in `data`.

- rescale:

  Logical; passed to
  [`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  to rescale weights.

- correct:

  Logical; passed to
  [`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  (Yates correction in 2x2 chi-squared contexts).

- simulate_p:

  Logical; passed to
  [`spicy::cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).

- simulate_B:

  Integer; Monte Carlo replicates when `simulate_p = TRUE`.

- percent_digits:

  Number of digits for percentages in report outputs.

- p_digits:

  Number of digits for p-values (except `< .001`).

- v_digits:

  Number of digits for Cramer's V.

- decimal_mark:

  Decimal separator (`"."` or `","`).

- output:

  Output format: `"wide"`, `"long"`, `"tinytable"`, `"flextable"`,
  `"excel"`, `"clipboard"`, `"word"`.

- style:

  `"raw"` for machine-friendly outputs, `"report"` for formatted
  outputs, `"auto"` to select by output type.

- indent_text:

  Prefix used for modality labels in report table building.

- indent_text_excel_clipboard:

  Stronger indentation used in Excel and clipboard exports.

- add_multilevel_header:

  Logical; merge top headers in Excel export.

- blank_na_wide:

  Logical; replace NA by empty strings in wide raw output.

- excel_path:

  Path for `output = "excel"`.

- excel_sheet:

  Sheet name for Excel export.

- clipboard_delim:

  Delimiter for clipboard text export.

- word_path:

  Path for `output = "word"` or optional save path when
  `output = "flextable"`.

## Value

Depends on `output` and `style`:

- `"long"` + `"raw"`: long numeric data frame.

- `"wide"` + `"raw"`: wide numeric data frame.

- `"long"` + `"report"`: long formatted character data frame.

- `"wide"` + `"report"`: wide formatted character data frame.

- `"tinytable"`: a `tinytable` object.

- `"flextable"`: a `flextable` object.

- `"excel"` / `"clipboard"` / `"word"`: invisibly returns written
  object/path.

## Details

It supports raw data outputs (`wide`, `long`) and report-oriented
outputs (`tinytable`, `flextable`, `excel`, `clipboard`, `word`) with
multi-level headers, p-values, and Cramer's V.

Optional output engines require suggested packages:

- `tinytable` for `output = "tinytable"`

- `flextable` + `officer` for `output = "flextable"`/`"word"`

- `openxlsx` for `output = "excel"`

- `clipr` for `output = "clipboard"`

## Examples

``` r
# Build a minimal reproducible dataset
d_ex <- transform(
  mtcars,
  hes = factor(gear, labels = c("BFH", "HEdS-Geneve", "HESAV")),
  emploi_sf = ifelse(vs == 1, "Oui", "Non"),
  role_prof_recherche = ifelse(am == 1, "Oui", "Non"),
  w = mpg
)

# Raw long output (machine-friendly)
table_apa(
  data = d_ex,
  row_vars = c("emploi_sf", "role_prof_recherche"),
  group_var = "hes",
  labels = c("Emploi SF", "Role recherche"),
  output = "long",
  style = "raw"
)
#>          variable level       group  n   pct     p Cramer's V
#> 1       Emploi SF   Non         BFH 12  80.0 0.001       0.62
#> 2       Emploi SF   Non HEdS-Geneve  2  16.7 0.001       0.62
#> 3       Emploi SF   Non       HESAV  4  80.0 0.001       0.62
#> 4       Emploi SF   Non       Total 18  56.2 0.001       0.62
#> 5       Emploi SF   Oui         BFH  3  20.0 0.001       0.62
#> 6       Emploi SF   Oui HEdS-Geneve 10  83.3 0.001       0.62
#> 7       Emploi SF   Oui       HESAV  1  20.0 0.001       0.62
#> 8       Emploi SF   Oui       Total 14  43.8 0.001       0.62
#> 9  Role recherche   Non         BFH 15 100.0 0.001       0.81
#> 10 Role recherche   Non HEdS-Geneve  4  33.3 0.001       0.81
#> 11 Role recherche   Non       HESAV  0   0.0 0.001       0.81
#> 12 Role recherche   Non       Total 19  59.4 0.001       0.81
#> 13 Role recherche   Oui         BFH  0   0.0 0.001       0.81
#> 14 Role recherche   Oui HEdS-Geneve  8  66.7 0.001       0.81
#> 15 Role recherche   Oui       HESAV  5 100.0 0.001       0.81
#> 16 Role recherche   Oui       Total 13  40.6 0.001       0.81

# Raw wide output
table_apa(
  data = d_ex,
  row_vars = c("emploi_sf", "role_prof_recherche"),
  group_var = "hes",
  labels = c("Emploi SF", "Role recherche"),
  output = "wide",
  style = "raw"
)
#>         Variable Level BFH n BFH % HEdS-Geneve n HEdS-Geneve % HESAV n HESAV %
#> 1      Emploi SF   Non    12    80             2          16.7       4      80
#> 2      Emploi SF   Oui     3    20            10          83.3       1      20
#> 3 Role recherche   Non    15   100             4          33.3       0       0
#> 4 Role recherche   Oui     0     0             8          66.7       5     100
#>   Total n Total %     p Cramer's V
#> 1      18    56.2 0.002       0.62
#> 2      14    43.8 0.002       0.62
#> 3      19    59.4 0.001       0.81
#> 4      13    40.6 0.001       0.81

# Weighted example
table_apa(
  data = d_ex,
  row_vars = c("emploi_sf", "role_prof_recherche"),
  group_var = "hes",
  labels = c("Emploi SF", "Role recherche"),
  weights = "w",
  rescale = TRUE,
  simulate_p = FALSE,
  output = "long",
  style = "raw"
)
#>          variable level       group        n   pct     p Cramer's V
#> 1       Emploi SF   Non         BFH  9.00000  74.8 0.003       0.59
#> 2       Emploi SF   Non HEdS-Geneve  2.00000  14.3 0.003       0.59
#> 3       Emploi SF   Non       HESAV  4.00000  71.6 0.003       0.59
#> 4       Emploi SF   Non       Total 14.88754  46.5 0.003       0.59
#> 5       Emploi SF   Oui         BFH  3.00000  25.2 0.003       0.59
#> 6       Emploi SF   Oui HEdS-Geneve 13.00000  85.7 0.003       0.59
#> 7       Emploi SF   Oui       HESAV  2.00000  28.4 0.003       0.59
#> 8       Emploi SF   Oui       Total 17.11246  53.5 0.003       0.59
#> 9  Role recherche   Non         BFH 12.00000 100.0 0.001       0.79
#> 10 Role recherche   Non HEdS-Geneve  4.00000  28.6 0.001       0.79
#> 11 Role recherche   Non       HESAV  0.00000   0.0 0.001       0.79
#> 12 Role recherche   Non       Total 16.21652  50.7 0.001       0.79
#> 13 Role recherche   Oui         BFH  0.00000   0.0 0.001       0.79
#> 14 Role recherche   Oui HEdS-Geneve 10.00000  71.4 0.001       0.79
#> 15 Role recherche   Oui       HESAV  5.00000 100.0 0.001       0.79
#> 16 Role recherche   Oui       Total 15.78348  49.3 0.001       0.79

# \donttest{
# Optional output: tinytable
if (requireNamespace("tinytable", quietly = TRUE)) {
  tt_ex <- table_apa(
    data = d_ex,
    row_vars = c("emploi_sf", "role_prof_recherche"),
    group_var = "hes",
    labels = c("Emploi SF", "Role recherche"),
    output = "tinytable"
  )
}

# Optional output: Excel
if (requireNamespace("openxlsx", quietly = TRUE)) {
  table_apa(
    data = d_ex,
    row_vars = c("emploi_sf", "role_prof_recherche"),
    group_var = "hes",
    labels = c("Emploi SF", "Role recherche"),
    output = "excel",
    excel_path = tempfile(fileext = ".xlsx")
  )
}
# }
```
