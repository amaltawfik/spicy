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
  in the `tinytable`, `gt`, `flextable`, `word`, and `clipboard`
  outputs. The first column (`Variable`) is always left-aligned. One of:

  - `"decimal"` (default): align numeric columns on the decimal mark,
    the standard scientific-publication convention used by SPSS, SAS,
    LaTeX `siunitx`, and the native primitives of
    [`gt::cols_align_decimal()`](https://gt.rstudio.com/reference/cols_align_decimal.html)
    and `tinytable::style_tt(align = "d")`. For engines without a native
    primitive (`flextable`, `word`, `clipboard`, ASCII print), numeric
    cells are pre-padded with leading and trailing spaces so the dots
    line up vertically; the body of the `flextable`/`word` output
    additionally uses a monospace font (`Consolas`) to make character
    widths uniform.

  - `"center"`: center-align all numeric columns.

  - `"right"`: right-align all numeric columns.

  - `"auto"`: legacy uniform right-alignment used in spicy \< 0.11.0.

  The `excel` output uses the engine's default alignment in any case:
  cell-string padding does not align decimals under proportional fonts,
  and Excel's native right-alignment combined with the per-column
  `numfmt` already produces dot-aligned columns. Same default and
  semantics as
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
# --- Basic usage ---------------------------------------------------------

# Default: ASCII console table grouped by sex.
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex
)
#> Categorical table by sex
#> 
#>  Variable          │ Female n  Female %  Male n  Male %  Total n  Total %   p   
#> ───────────────────┼────────────────────────────────────────────────────────────
#>  smoking           │                                                       .713 
#>    No              │   475       78.4     451     79.3     926     78.8         
#>    Yes             │   131       21.6     118     20.7     249     21.2         
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │                                                       .832 
#>    No              │   334       53.9     316     54.5     650     54.2         
#>    Yes             │   286       46.1     264     45.5     550     45.8         
#> 
#>  Variable          │ Cramer's V 
#> ───────────────────┼────────────
#>  smoking           │    .01     
#>    No              │            
#>    Yes             │            
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │    .01     
#>    No              │            
#>    Yes             │            

# One-way frequency-style table (no `by`).
table_categorical(
  sochealth,
  select = c(smoking, physical_activity)
)
#> Categorical table
#> 
#>  Variable            │   n      %    
#> ─────────────────────┼───────────────
#>  smoking             │               
#>    No                │  926    78.8  
#>    Yes               │  249    21.2  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity   │               
#>    No                │  650    54.2  
#>    Yes               │  550    45.8  

# Pretty labels keyed by column name.
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c(
    smoking           = "Current smoker",
    physical_activity = "Physical activity"
  )
)
#> Categorical table by education
#> 
#>  Variable          │ Lower secondary n  Lower secondary %  Upper secondary n 
#> ───────────────────┼─────────────────────────────────────────────────────────
#>  Current smoker    │                                                         
#>    No              │        179               69.6                415        
#>    Yes             │         78               30.4                112        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity │                                                         
#>    No              │        177               67.8                310        
#>    Yes             │         84               32.2                229        
#> 
#>  Variable          │ Upper secondary %  Tertiary n  Tertiary %  Total n 
#> ───────────────────┼────────────────────────────────────────────────────
#>  Current smoker    │                                                    
#>    No              │       78.7            332         84.9       926   
#>    Yes             │       21.3             59         15.1       249   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity │                                                    
#>    No              │       57.5            163         40.8       650   
#>    Yes             │       42.5            237         59.2       550   
#> 
#>  Variable          │ Total %    p    Cramer's V 
#> ───────────────────┼────────────────────────────
#>  Current smoker    │          <.001     .14     
#>    No              │  78.8                      
#>    Yes             │  21.2                      
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Physical activity │          <.001     .21     
#>    No              │  54.2                      
#>    Yes             │  45.8                      

# Survey weights with rescaling.
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  weights = "weight",
  rescale = TRUE
)
#> Categorical table by education
#> 
#>  Variable          │ Lower secondary n  Lower secondary %  Upper secondary n 
#> ───────────────────┼─────────────────────────────────────────────────────────
#>  smoking           │                                                         
#>    No              │        176               69.0                419        
#>    Yes             │         79               31.0                115        
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │                                                         
#>    No              │        174               67.2                315        
#>    Yes             │         85               32.8                231        
#> 
#>  Variable          │ Upper secondary %  Tertiary n  Tertiary %  Total n 
#> ───────────────────┼────────────────────────────────────────────────────
#>  smoking           │                                                    
#>    No              │       78.5            325         84.4      920.9  
#>    Yes             │       21.5             60         15.6      254.1  
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │                                                    
#>    No              │       57.7            166         41.9      654.8  
#>    Yes             │       42.3            229         58.1      545.2  
#> 
#>  Variable          │ Total %    p    Cramer's V 
#> ───────────────────┼────────────────────────────
#>  smoking           │          <.001     .13     
#>    No              │  78.4                      
#>    Yes             │  21.6                      
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity │          <.001     .19     
#>    No              │  54.6                      
#>    Yes             │  45.4                      

# Confidence interval for the association measure.
table_categorical(
  sochealth,
  select = smoking,
  by = education,
  assoc_ci = TRUE
)
#> Categorical table by education
#> 
#>  Variable │ Lower secondary n  Lower secondary %  Upper secondary n 
#> ──────────┼─────────────────────────────────────────────────────────
#>  smoking  │                                                         
#>    No     │        179               69.6                415        
#>    Yes    │         78               30.4                112        
#> 
#>  Variable │ Upper secondary %  Tertiary n  Tertiary %  Total n  Total %    p   
#> ──────────┼────────────────────────────────────────────────────────────────────
#>  smoking  │                                                              <.001 
#>    No     │       78.7            332         84.9       926     78.8          
#>    Yes    │       21.3             59         15.1       249     21.2          
#> 
#>  Variable │ Cramer's V  CI lower  CI upper 
#> ──────────┼────────────────────────────────
#>  smoking  │    .14        .08       .19    
#>    No     │                                
#>    Yes    │                                

# --- Output formats -----------------------------------------------------

# The rendered outputs below all wrap the same call:
#   table_categorical(sochealth,
#                     select = c(smoking, physical_activity),
#                     by = sex)
# only `output` changes. Assign to a variable to avoid the
# console-friendly text fallback that some engines fall back to
# when printed directly in `?` help.

# Wide data.frame (one row per modality).
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex,
  output = "data.frame"
)
#>            Variable Level Female n Female % Male n Male % Total n Total %
#> 1           smoking    No      475     78.4    451   79.3     926    78.8
#> 2           smoking   Yes      131     21.6    118   20.7     249    21.2
#> 3 physical_activity    No      334     53.9    316   54.5     650    54.2
#> 4 physical_activity   Yes      286     46.1    264   45.5     550    45.8
#>           p  Cramer's V
#> 1 0.7125196 0.010749501
#> 2 0.7125196 0.010749501
#> 3 0.8316763 0.006135851
#> 4 0.8316763 0.006135851

# Long data.frame (one row per (modality x group)).
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex,
  output = "long"
)
#>             variable level  group   n  pct      chi2 df         p  Cramer's V
#> 1            smoking    No Female 475 78.4 0.1357733  1 0.7125196 0.010749501
#> 2            smoking    No   Male 451 79.3 0.1357733  1 0.7125196 0.010749501
#> 3            smoking    No  Total 926 78.8 0.1357733  1 0.7125196 0.010749501
#> 4            smoking   Yes Female 131 21.6 0.1357733  1 0.7125196 0.010749501
#> 5            smoking   Yes   Male 118 20.7 0.1357733  1 0.7125196 0.010749501
#> 6            smoking   Yes  Total 249 21.2 0.1357733  1 0.7125196 0.010749501
#> 7  physical_activity    No Female 334 53.9 0.0451784  1 0.8316763 0.006135851
#> 8  physical_activity    No   Male 316 54.5 0.0451784  1 0.8316763 0.006135851
#> 9  physical_activity    No  Total 650 54.2 0.0451784  1 0.8316763 0.006135851
#> 10 physical_activity   Yes Female 286 46.1 0.0451784  1 0.8316763 0.006135851
#> 11 physical_activity   Yes   Male 264 45.5 0.0451784  1 0.8316763 0.006135851
#> 12 physical_activity   Yes  Total 550 45.8 0.0451784  1 0.8316763 0.006135851

# \donttest{
# Rendered HTML / docx objects -- best viewed inside a
# Quarto / R Markdown document or a pkgdown article.
if (requireNamespace("tinytable", quietly = TRUE)) {
  tt <- table_categorical(
    sochealth, select = c(smoking, physical_activity), by = sex,
    output = "tinytable"
  )
}
if (requireNamespace("gt", quietly = TRUE)) {
  tbl <- table_categorical(
    sochealth, select = c(smoking, physical_activity), by = sex,
    output = "gt"
  )
}
if (requireNamespace("flextable", quietly = TRUE)) {
  ft <- table_categorical(
    sochealth, select = c(smoking, physical_activity), by = sex,
    output = "flextable"
  )
}

# Excel and Word: write to a temporary file.
if (requireNamespace("openxlsx2", quietly = TRUE)) {
  tmp <- tempfile(fileext = ".xlsx")
  table_categorical(
    sochealth, select = c(smoking, physical_activity), by = sex,
    output = "excel", excel_path = tmp
  )
  unlink(tmp)
}
if (
  requireNamespace("flextable", quietly = TRUE) &&
    requireNamespace("officer", quietly = TRUE)
) {
  tmp <- tempfile(fileext = ".docx")
  table_categorical(
    sochealth, select = c(smoking, physical_activity), by = sex,
    output = "word", word_path = tmp
  )
  unlink(tmp)
}
# }

if (FALSE) { # \dontrun{
# Clipboard: writes to the system clipboard.
table_categorical(
  sochealth, select = c(smoking, physical_activity), by = sex,
  output = "clipboard"
)
} # }
```
