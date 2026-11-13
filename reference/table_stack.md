# Stack tables one under the other

`table_stack()` sets regression tables one under the other. Each table
becomes a **panel**, opened by a row with its label, and keeps its own
columns, rows, fit statistics and number formats: the cells of a panel
have the content of the table passed, in every engine. The panels share
one column header when they have the same columns, and each opens with
its own header otherwise. Their notes are merged into one.

Build each table with its own arguments (`vcov`, `cluster`,
`exponentiate`, `p_adjust`, `show_columns`, ...), then stack them: a
grid of models is
`table_stack(table_regression(list(m1, m2)), table_regression(list(m3, m4)))`.
For one panel per model of a list,
`table_regression(models, layout = "stacked")` is the shortcut.

## Usage

``` r
table_stack(
  ...,
  panel_labels = NULL,
  title = NULL,
  note = NULL,
  note_add = NULL,
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  word_template = NULL
)
```

## Arguments

- ...:

  Tables returned by
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  or
  [`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)
  with `output = "default"`, one per panel, in the order of the panels.
  A name given here labels its panel:
  `table_stack(Women = t1, Men = t2)`. A list of tables is spliced with
  `!!!`: `table_stack(!!!tables)`.

- panel_labels:

  Character vector, one non-empty label per table, in the order of the
  tables, unnamed: the labels of the panels not named in `...` (a name
  given there takes precedence, with a warning). `NULL` (default) labels
  such a panel by the title you typed for its table, else by its outcome
  when the panels have different outcomes, else "Panel k". Two panels
  cannot share a label.

- title:

  Title of the stacked table. `NULL` (default) keeps the title of the
  tables when they all have the same one, and gives none otherwise;
  `FALSE` gives none; a string sets it.

- note:

  `NULL` (default) merges the notes of the panels; `FALSE` removes the
  note; a string replaces the general note, printed after the lead-in
  ("Note."), with the legend of the stars kept below it.

- note_add:

  Sentences added at the end of the general note.

- output:

  Output format, as in
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md):
  `"default"` (a printable data frame), `"data.frame"`, `"long"`,
  `"tinytable"`, `"gt"`, `"flextable"`, `"excel"`, `"clipboard"` or
  `"word"`.

- excel_path:

  File path for `output = "excel"`, ending in `".xlsx"`, in a directory
  that exists. Default `NULL` (required when `output = "excel"`).

- excel_sheet:

  Sheet name when writing to Excel. `NULL` (the default) uses
  `"Regression"`.

- clipboard_delim:

  Field delimiter for `output = "clipboard"`. Default `"\t"`
  (tab-separated, pastes cleanly into Excel / Google Sheets / Word). The
  clipboard payload mirrors the Excel layout (title row, spanner row,
  header, body, footer note) but is plain text – horizontal rules, cell
  merging, decimal alignment and the monospace font cannot be encoded in
  delimited text and are therefore absent from the paste. A cell holding
  the delimiter itself (a label with a comma under
  `clipboard_delim = ","`, or any number under `decimal_mark = ","`), a
  double quote or a line break is quoted RFC 4180-style, so the grid
  survives whatever delimiter you choose.

  Paste behavior by target:

  - **Excel / Google Sheets:** numerics are auto-detected and
    right-aligned; text cells stay left-aligned. (P-values such as
    `.005` get re-parsed as `0.005` by Excel's auto-format – to preserve
    the APA leading-zero-dropped display, prefer `output = "excel"`.)

  - **Word:** the paste is converted to a Word table; all cells start
    left-aligned. Apply a Table Style (Insert \> Table \> Design) for
    APA-style borders, and set right-alignment on numeric columns
    (Layout \> Align Right). For a self-contained Word file with borders
    and alignment pre-applied, use `output = "word"` instead.

- word_path:

  File path for `output = "word"`, ending in `".docx"`, in a directory
  that exists. Default `NULL` (required when `output = "word"`). The
  Word table inherits the flextable styling (Calibri font, APA borders,
  decimal-aligned numerics) and adds Word-specific features: an
  auto-numbered caption ("Table 1: ...", "Table 2: ...") via Word's
  `SEQ` field so multiple
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  calls in one document number consecutively; a re-printed header row on
  each page break; row split prevention so a single coefficient row
  never wraps across two pages; and an APA-styled note line (`*Note.*`
  italic prefix per APA Manual 7 §7.14).

  **R Markdown / Quarto:** for embedded use, prefer
  `output = "flextable"` (returns the flextable object that knits to
  docx/HTML/PDF natively). `output = "word"` writes a standalone .docx
  file, suited to scripted exports rather than chunk-level rendering.

- word_template:

  Optional path to a custom .docx file used as the template for
  `output = "word"`. The template's header, footer, page size, margins,
  and named styles ("Table Caption" in particular) are honored; the
  table is appended to the template body. Useful for institutional
  templates with pre-set headers ("APA Style", "Manuscript Submission
  Template", etc.). Default `NULL` (uses flextable's stock template).

  **Customizing the caption appearance:** the table caption is tagged
  with the Word named style `"Table Caption"`. The visual rendering
  (italic / bold / color / font) follows whatever that style is set to
  in the docx template. The stock Word template renders
  `"Table Caption"` in italic — the APA Manual 7 §7.10 condensed
  convention. For a different appearance (Nature-style bold non-italic,
  APA-strict 2-line bold-number / italic-title, etc.), edit the
  `"Table Caption"` style in a docx template and pass it via
  `word_template = "your_template.docx"`. Style-based delegation keeps
  the rendered caption consistent with the surrounding document and lets
  editorial conventions (Nature, APA-strict, journal-specific) be
  applied without modifying the call site.

## Value

With `output = "default"`, a `spicy_stack_table` (classes
`c("spicy_stack_table", "spicy_table", "data.frame")`): the cells as
printed, one row per printed row, plus a blank row for the bounds of an
interval in a panel's header. Its attributes `panels` (the tables
stacked, named by their labels), `panel_rows` (the rows of each panel in
it, its label and header rows excluded), `title` and `note` are
documented; the others are internal.
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
returns the typed views of the panels, as a named list.

With `output = "data.frame"` (and from
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)), the
data frames of the panels bound by rows; with `output = "long"` (and
from [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
and
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)),
the outputs of their models. Each opens with a `panel` column holding
the label of the panel; the columns are the union of the panels' and the
note, as printed, is the `note` attribute. In the output of
`table_stack()`, a row is keyed by `panel`, `model_id` (the model inside
its panel), `term` and `estimate_type`. With
`table_regression(layout = "stacked")`, these three outputs are those of
the same models side by side, `panel` aside: one row per
`(model_id, term, estimate_type)`, `model_id` being the identifier of
the model in the list. With `"tinytable"`, `"gt"` or `"flextable"`, the
object of that package; with `"excel"` or `"word"`, the path of the
file, invisibly; with `"clipboard"`, the stacked table, invisibly.

## Panels

A row with the label of the panel opens each panel ("Women:"), in the
typography of the block headers. The label of a panel is its name in
`...`, else its entry in `panel_labels`, else the title you typed for
its table (`title = "..."`), else its outcome when every panel has an
outcome of its own, else "Panel k". A title spicy built is not a label:
it describes the model ("Linear regression: bmi"), not the panel. The
rows of a panel follow, in the order of its table: coefficients, blocks
(thresholds, random effects, components), fit statistics.

When every panel has the same column headers, one header heads the whole
table. Otherwise the header of the first panel heads it and each later
panel opens with its own header rows, below its label row: an
`exponentiate = TRUE` glm panel reads "OR" where an lm panel reads "B",
and a multinomial panel keeps its outcome categories in columns. A panel
narrower than the widest one leaves the extra columns blank. The header
of a panel has the rows of the header of its table: its column groups,
ruled under, its labels, and the bounds of its interval ("LL", "UL"),
which the console leaves out since it prints an interval in one column.

In the console, the columns are matched by position and every panel
shares their widths: a panel with other columns can widen a column of
the first. When the panels have the same columns and decimals, the
decimals line up from one panel to the next, only the padding of a cell
changing; otherwise each panel keeps the padding of its table. When the
console splits a wide stack by width, a column group cut by the split
keeps its label over each part, and a continuation leaves out the panels
with no cell in it. The other engines write each cell as its table does.

A table of
[`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)
is a panel like any other, its univariable models and their own sample
sizes included.

## Notes

The notes are merged sentence by sentence. The type of each model and
its standard errors are worded as in a table of the models side by side:
"Linear regression models.", or "Women: linear regression; Men: logistic
regression." (in English; another language gives each panel its own
sentence, as below), and "Std. errors: classical (OLS)." or one
estimator per panel. So is an adjustment of the p-values ("P-values
adjusted within each model ..."). Any other sentence that every panel
carries is printed once, first; one that only some panels carry follows,
opening with their labels ("Women, Men: ..."), panel after panel. The
abbreviations are defined once. The legend of the stars is printed once,
and when panels have different legends, each opens with the labels of
its panels. A panel built with `note = FALSE` contributes no sentence,
and one built with a typed `note` contributes that note.

## Compatibility

The panels must be built in the same language
(`options(spicy.language)`), with the same `decimal_mark` and `align`,
and under styles that format a cell alike: the stacked table has one of
each. Two styles that print the same table are compatible (no style and
`style = "apa"`); a lever that differs (the decimals of a p-value, the
separator or brackets of an interval, the minus sign, the lead-in of the
note, the abbreviations) is not, nor is `p_digits` typed for one table
of a theme that sets its own p-values. Otherwise `table_stack()` stops
with `spicy_stack_mismatch`, naming what differs.

Only the tables of
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
and
[`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)
can be panels for now; the descriptive tables will follow.

## Classed conditions

Errors carry `spicy_invalid_input` (no table, an object that is not a
table built with `output = "default"`, a stack passed as a panel, a
table whose rows were changed after it was built, invalid or duplicated
labels), `spicy_empty_table` (a table without rows),
`spicy_not_implemented` with `spicy_unsupported` (a descriptive table)
and `spicy_stack_mismatch` with `spicy_invalid_input` (panels built in
different languages, with different decimal marks or alignments, or
under styles that format a cell differently). Warnings carry
`spicy_ignored_arg` (an entry of `panel_labels` overridden by a name in
`...`).

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
whose `layout = "stacked"` stacks one panel per model;
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md).

## Examples

``` r
women <- sochealth[sochealth$sex == "Female", ]
men <- sochealth[sochealth$sex == "Male", ]
table_stack(
  Women = table_regression(lm(bmi ~ age + smoking, data = women)),
  Men = table_regression(lm(bmi ~ age + smoking, data = men))
)
#> Linear regression: bmi
#> 
#>  Variable    │   B      SE       95% CI        p   
#> ─────────────┼─────────────────────────────────────
#>  Women:      │                                     
#>  (Intercept) │  24.04  0.53  [23.00, 25.09]  <.001 
#>  age         │   0.03  0.01  [ 0.01,  0.05]  <.001 
#>  smoking:    │                                     
#>    No (ref.) │    –     –          –          –    
#>    Yes       │  -0.18  0.37  [-0.90,  0.55]   .637 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 602                                 
#>  R²          │   0.02                              
#>  Adj. R²     │   0.02                              
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Men:        │                                     
#>  (Intercept) │  24.41  0.55  [23.33, 25.48]  <.001 
#>  age         │   0.04  0.01  [ 0.02,  0.06]  <.001 
#>  smoking:    │                                     
#>    No (ref.) │    –     –          –          –    
#>    Yes       │   0.07  0.38  [-0.67,  0.81]   .850 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 561                                 
#>  R²          │   0.02                              
#>  Adj. R²     │   0.02                              
#> 
#> Note. Linear regression models. Std. errors: classical (OLS). CI = confidence interval.

# Panels with their own columns: the second opens with its header.
fit_bmi <- lm(bmi ~ age + sex, data = sochealth)
fit_dentist <- glm(dentist_12m ~ age + sex, data = sochealth,
                   family = binomial)
table_stack(
  BMI = table_regression(fit_bmi),
  Dentist = table_regression(fit_dentist, exponentiate = TRUE)
)
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  BMI:            │                                      
#>  (Intercept)     │   23.95  0.39  [23.18, 24.71]  <.001 
#>  age             │    0.04  0.01  [ 0.02,  0.05]  <.001 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    0.52  0.21  [ 0.10,  0.94]   .015 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1188                                 
#>  R²              │    0.02                              
#>  Adj. R²         │    0.02                              
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Dentist:        │                                      
#>                  │   OR      SE       95% CI        p   
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  (Intercept)     │    1.56  0.36  [ 1.00,  2.44]   .052 
#>  age             │    1.01  0.00  [ 1.00,  1.02]   .044 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    1.00  0.13  [ 0.78,  1.29]   .978 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1200                                 
#>  R² (McFadden)   │    0.00                              
#>  R² (Nagelkerke) │    0.00                              
#>  AIC             │ 1457.7                               
#> 
#> Note. BMI: linear regression; Dentist: logistic regression. Dentist: coefficients exponentiated and displayed as OR; SE on the OR scale (delta method); CI bounds exponentiated (asymmetric). Dentist: the (Intercept) row estimates the baseline odds. Std. errors: BMI: classical (OLS), Dentist: classical (Fisher information). CI = confidence interval; AIC = Akaike information criterion.

# One panel per model.
table_regression(list(BMI = fit_bmi, Dentist = fit_dentist),
                 layout = "stacked")
#> Regression comparison
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  BMI:            │                                      
#>  (Intercept)     │   23.95  0.39  [23.18, 24.71]  <.001 
#>  age             │    0.04  0.01  [ 0.02,  0.05]  <.001 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    0.52  0.21  [ 0.10,  0.94]   .015 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1188                                 
#>  R²              │    0.02                              
#>  Adj. R²         │    0.02                              
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Dentist:        │                                      
#>  (Intercept)     │    0.44  0.23  [-0.00,  0.89]   .052 
#>  age             │    0.01  0.00  [ 0.00,  0.02]   .044 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    0.00  0.13  [-0.25,  0.25]   .978 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1200                                 
#>  R² (McFadden)   │    0.00                              
#>  R² (Nagelkerke) │    0.00                              
#>  AIC             │ 1457.7                               
#> 
#> Note. BMI: linear regression; Dentist: logistic regression. Std. errors: BMI: classical (OLS), Dentist: classical (Fisher information). CI = confidence interval; AIC = Akaike information criterion.
```
