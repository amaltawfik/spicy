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
  each cross-tabulation.

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
if (FALSE) { # \dontrun{
# Raw long output
table_apa(
  data = d,
  row_vars = c("emploi_sf", "role_prof_recherche"),
  group_var = "hes",
  labels = c("Emploi SF", "Role recherche"),
  output = "long",
  style = "raw"
)

# Publication-ready tinytable
tt <- table_apa(
  data = d,
  row_vars = c("emploi_sf", "role_prof_recherche"),
  group_var = "hes",
  labels = c("Emploi SF", "Role recherche"),
  output = "tinytable"
)
tt

# Excel export
table_apa(
  data = d,
  row_vars = c("emploi_sf", "role_prof_recherche"),
  group_var = "hes",
  labels = c("Emploi SF", "Role recherche"),
  output = "excel",
  excel_path = "apa_table.xlsx"
)
} # }
```
