# Categorical summary tables with table_categorical()

``` r
library(spicy)
```

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
builds publication-ready categorical tables suitable for APA-style
reporting in social science and data science research. With `by`, it
produces grouped cross-tabulation tables with chi-squared \\p\\-values,
effect sizes, confidence intervals, and multi-level headers. Without
`by`, it produces one-way frequency-style tables for the selected
variables. Export to gt, tinytable, flextable, Excel, or Word. This
vignette walks through the main features.

## Basic usage

For grouped tables, provide a data frame, one or more selected
variables, and a grouping variable:

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education
)
#>            Variable Level Lower secondary n Lower secondary % Upper secondary n
#> 1           smoking    No               179              69.6               415
#> 2           smoking   Yes                78              30.4               112
#> 3 physical_activity    No               177              67.8               310
#> 4 physical_activity   Yes                84              32.2               229
#> 5       dentist_12m    No               113              43.3               174
#> 6       dentist_12m   Yes               148              56.7               365
#>   Upper secondary % Tertiary n Tertiary % Total n Total %            p
#> 1              78.7        332       84.9     926    78.8 2.012877e-05
#> 2              21.3         59       15.1     249    21.2 2.012877e-05
#> 3              57.5        163       40.8     650    54.2 8.333584e-12
#> 4              42.5        237       59.2     550    45.8 8.333584e-12
#> 5              32.3         67       16.8     354    29.5 3.883413e-13
#> 6              67.7        333       83.2     846    70.5 3.883413e-13
#>   Cramer's V
#> 1  0.1356677
#> 2  0.1356677
#> 3  0.2061986
#> 4  0.2061986
#> 5  0.2182388
#> 6  0.2182388
```

The default output is `"wide"` with `style = "raw"`, which returns a
data frame with numeric values suitable for further processing.

## One-way tables

Omit `by` to build a frequency-style table for the selected variables:

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  output = "default"
)
#> Categorical table: sochealth
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
```

## Output formats

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
supports several output formats. The table below summarizes the options:

| Format        | Description                                |
|---------------|--------------------------------------------|
| `"wide"`      | Data frame, one row per modality (default) |
| `"default"`   | Styled ASCII table in the console          |
| `"long"`      | Data frame, one row per modality x group   |
| `"gt"`        | Formatted gt table                         |
| `"tinytable"` | Formatted tinytable                        |
| `"flextable"` | Formatted flextable                        |
| `"excel"`     | Excel file (requires `excel_path`)         |
| `"clipboard"` | Copy to clipboard                          |
| `"word"`      | Word document (requires `word_path`)       |

### gt output

The `"gt"` format produces a table with APA-style borders, column
spanners, and proper alignment:

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education,
  output = "gt"
)
```

[TABLE]

### tinytable output

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = sex,
  output = "tinytable"
)
```

| Variable          | Female |      | Male |      | Total |      | p    | Cramer's V |
|-------------------|--------|------|------|------|-------|------|------|------------|
|                   | n      | %    | n    | %    | n     | %    |      |            |
| smoking           |        |      |      |      |       |      | .713 | .01        |
|      No           | 475    | 78.4 | 451  | 79.3 | 926   | 78.8 |      |            |
|      Yes          | 131    | 21.6 | 118  | 20.7 | 249   | 21.2 |      |            |
| physical_activity |        |      |      |      |       |      | .832 | .01        |
|      No           | 334    | 53.9 | 316  | 54.5 | 650   | 54.2 |      |            |
|      Yes          | 286    | 46.1 | 264  | 45.5 | 550   | 45.8 |      |            |

### Data frame output

Use `style = "report"` with `"wide"` or `"long"` to get formatted
character columns (ready for display), or `style = "raw"` for numeric
values (ready for analysis):

``` r
table_categorical(
  sochealth,
  select = smoking,
  by = education,
  output = "wide",
  style = "report"
)
#>   Variable Lower secondary n Lower secondary % Upper secondary n
#> 1  smoking                                                      
#> 2       No               179              69.6               415
#> 3      Yes                78              30.4               112
#>   Upper secondary % Tertiary n Tertiary % Total n Total %      p Cramer's V
#> 1                                                         < .001        .14
#> 2              78.7        332       84.9     926    78.8                  
#> 3              21.3         59       15.1     249    21.2
```

Use `output = "default"` for a styled ASCII table in the console. Set
`styled = FALSE` with `output = "default"` to recover the underlying
wide raw `data.frame`.

## Custom labels

By default,
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
uses variable names as row headers. Use the `labels` argument to provide
human-readable labels:

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Smoking status", "Regular physical activity"),
  output = "gt"
)
```

[TABLE]

## Association measures and confidence intervals

By default,
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
reports Cramer’s V for nominal variables and automatically switches to
Kendall’s Tau-b when both variables are ordered factors. Override with
`assoc_measure`:

``` r
table_categorical(
  sochealth,
  select = smoking,
  by = education,
  assoc_measure = "phi",
  output = "tinytable"
)
```

| Variable | Lower secondary |      | Upper secondary |      | Tertiary |      | Total |      | p       | Cramer's V |
|----------|-----------------|------|-----------------|------|----------|------|-------|------|---------|------------|
|          | n               | %    | n               | %    | n        | %    | n     | %    |         |            |
| smoking  |                 |      |                 |      |          |      |       |      | \< .001 |            |
|      No  | 179             | 69.6 | 415             | 78.7 | 332      | 84.9 | 926   | 78.8 |         |            |
|      Yes | 78              | 30.4 | 112             | 21.3 | 59       | 15.1 | 249   | 21.2 |         |            |

Add confidence intervals with `assoc_ci = TRUE`. In rendered formats
(gt, tinytable, flextable), the CI is shown inline:

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  assoc_ci = TRUE,
  output = "gt"
)
```

[TABLE]

In data formats (wide, long, excel, clipboard), separate `CI lower` and
`CI upper` columns are added:

``` r
table_categorical(
  sochealth,
  select = smoking,
  by = education,
  assoc_ci = TRUE,
  output = "wide",
  style = "report"
)
#>   Variable Lower secondary n Lower secondary % Upper secondary n
#> 1  smoking                                                      
#> 2       No               179              69.6               415
#> 3      Yes                78              30.4               112
#>   Upper secondary % Tertiary n Tertiary % Total n Total %      p Cramer's V
#> 1                                                         < .001        .14
#> 2              78.7        332       84.9     926    78.8                  
#> 3              21.3         59       15.1     249    21.2                  
#>   CI lower CI upper
#> 1      .08      .19
#> 2                  
#> 3
```

## Weighted tables

Pass survey weights with the `weights` argument. Use `rescale = TRUE` so
the total weighted N matches the unweighted N:

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  weights = "weight",
  rescale = TRUE,
  output = "gt"
)
```

[TABLE]

## Handling missing values

By default, rows with missing values are dropped (`drop_na = TRUE`). Set
`drop_na = FALSE` to display them as a “(Missing)” category:

``` r
table_categorical(
  sochealth,
  select = income_group,
  by = education,
  drop_na = FALSE,
  output = "gt"
)
```

[TABLE]

## Filtering and reordering levels

Use `levels_keep` to display only specific modalities. The order you
specify controls the display order, which is useful for placing
“(Missing)” last:

``` r
table_categorical(
  sochealth,
  select = income_group,
  by = education,
  drop_na = FALSE,
  levels_keep = c("Low", "High", "(Missing)"),
  output = "gt"
)
```

[TABLE]

## Formatting options

Control the number of digits for percentages, p-values, and the
association measure:

``` r
table_categorical(
  sochealth,
  select = smoking,
  by = education,
  percent_digits = 2,
  p_digits = 4,
  v_digits = 3,
  output = "gt"
)
```

[TABLE]

## Exporting to Excel, Word, or clipboard

For Excel export, provide a file path:

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education,
  output = "excel",
  excel_path = "my_table.xlsx"
)
```

For Word, use `output = "word"`:

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity, dentist_12m),
  by = education,
  output = "word",
  word_path = "my_table.docx"
)
```

You can also copy directly to the clipboard for pasting into a
spreadsheet or a text editor:

``` r
table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  output = "clipboard"
)
```
