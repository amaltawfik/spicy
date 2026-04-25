# Generate a comprehensive summary of the variables

`varlist()` lists the variables of a data frame and extracts essential
metadata, including variable names, labels, summary values, classes,
number of distinct values, number of valid (non-missing) observations,
and number of missing values.

`vl()` is a convenient shorthand for `varlist()` that offers identical
functionality with a shorter name.

## Usage

``` r
varlist(
  x,
  ...,
  values = FALSE,
  tbl = FALSE,
  include_na = FALSE,
  factor_levels = c("observed", "all")
)

vl(
  x,
  ...,
  values = FALSE,
  tbl = FALSE,
  include_na = FALSE,
  factor_levels = c("observed", "all")
)
```

## Arguments

- x:

  A data frame, or a transformation of one.

- ...:

  Optional tidyselect-style column selectors (e.g. `starts_with("var")`,
  `where(is.numeric)`, etc.). Columns can be selected or reordered, but
  renaming selections is not supported.

- values:

  Logical. If `FALSE` (the default), displays a compact summary of the
  variable's values. For numeric, character, date/time, labelled, and
  factor variables, up to four unique non-missing values are shown: the
  first three values, followed by an ellipsis (`...`), and the last
  value. Values are sorted when appropriate (e.g., numeric, character,
  date). For factors, `factor_levels` controls whether observed or all
  declared levels are shown; level order is preserved. For labelled
  variables, prefixed labels are displayed via
  `labelled::to_factor(levels = "prefixed")`. If `TRUE`, all unique
  non-missing values are displayed.

- tbl:

  Logical. If `FALSE` (the default), opens the summary in the Viewer if
  the session is interactive. If `TRUE`, returns a tibble.

- include_na:

  Logical. If `TRUE`, unique missing value markers (`<NA>`, `<NaN>`) are
  explicitly appended at the end of the `Values` summary when present in
  the variable. This applies to all variable types. If `FALSE` (the
  default), missing values are omitted from `Values` but still counted
  in the `NAs` column.

- factor_levels:

  Character. Controls how factor values are displayed in `Values`.
  `"observed"` (the default) shows only levels present in the data,
  preserving factor level order. `"all"` shows all declared levels,
  including unused levels.

## Value

A tibble with one row per selected variable, containing the following
columns:

- `Variable`: variable names

- `Label`: variable labels (if available via the `label` attribute)

- `Values`: a summary of the variable's values, depending on the
  `values` and `include_na` arguments. If `values = FALSE`, a compact
  summary (max 4 values: 3 + ... + last) is shown. If `values = TRUE`,
  all unique non-missing values are displayed. For labelled variables,
  **prefixed labels** are displayed using
  `labelled::to_factor(levels = "prefixed")`. For factors, levels are
  displayed according to `factor_levels`. Matrix and array columns are
  summarized by their dimensions. Missing value markers (`<NA>`,
  `<NaN>`) are optionally appended at the end (controlled via
  `include_na`). Literal strings `"NA"`, `"NaN"`, and `""` are quoted to
  distinguish them from missing markers.

- `Class`: the class of each variable (possibly multiple, e.g.
  `"labelled", "numeric"`)

- `N_distinct`: number of distinct non-missing values

- `N_valid`: number of non-missing observations

- `NAs`: number of missing observations

If `tbl = TRUE`, the tibble is returned. If `tbl = FALSE` and the
session is interactive, the summary is displayed in the Viewer pane and
the function returns invisibly. In non-interactive sessions, a message
is displayed and the function returns invisibly.

## Details

The function can also apply tidyselect-style variable selectors to
select or reorder columns dynamically.

If used interactively (e.g. in RStudio), the summary is displayed in the
Viewer pane with a contextual title like `vl: sochealth`. If the data
frame has been transformed or subsetted, the title will display an
asterisk (`*`), e.g. `vl: sochealth*`. Anonymous or ambiguous calls use
`vl: <data>`.

## Examples

``` r
varlist(sochealth, tbl = TRUE)
#> # A tibble: 24 × 7
#>    Variable          Label                 Values Class N_distinct N_valid   NAs
#>    <chr>             <chr>                 <chr>  <chr>      <int>   <int> <int>
#>  1 sex               Sex                   Femal… fact…          2    1200     0
#>  2 age               Age (years)           25, 2… nume…         51    1200     0
#>  3 age_group         Age group             25-34… orde…          4    1200     0
#>  4 education         Highest education le… Lower… orde…          3    1200     0
#>  5 social_class      Subjective social cl… Lower… orde…          5    1200     0
#>  6 region            Region of residence   Centr… fact…          6    1200     0
#>  7 employment_status Employment status     Emplo… fact…          4    1200     0
#>  8 income_group      Household income gro… Low, … orde…          4    1182    18
#>  9 income            Monthly household in… 1000,… nume…       1052    1200     0
#> 10 smoking           Current smoker        No, Y… fact…          2    1175    25
#> # ℹ 14 more rows
sochealth |> varlist(tbl = TRUE)
#> # A tibble: 24 × 7
#>    Variable          Label                 Values Class N_distinct N_valid   NAs
#>    <chr>             <chr>                 <chr>  <chr>      <int>   <int> <int>
#>  1 sex               Sex                   Femal… fact…          2    1200     0
#>  2 age               Age (years)           25, 2… nume…         51    1200     0
#>  3 age_group         Age group             25-34… orde…          4    1200     0
#>  4 education         Highest education le… Lower… orde…          3    1200     0
#>  5 social_class      Subjective social cl… Lower… orde…          5    1200     0
#>  6 region            Region of residence   Centr… fact…          6    1200     0
#>  7 employment_status Employment status     Emplo… fact…          4    1200     0
#>  8 income_group      Household income gro… Low, … orde…          4    1182    18
#>  9 income            Monthly household in… 1000,… nume…       1052    1200     0
#> 10 smoking           Current smoker        No, Y… fact…          2    1175    25
#> # ℹ 14 more rows
varlist(sochealth, where(is.numeric), values = TRUE, tbl = TRUE)
#> # A tibble: 10 × 7
#>    Variable               Label            Values Class N_distinct N_valid   NAs
#>    <chr>                  <chr>            <chr>  <chr>      <int>   <int> <int>
#>  1 age                    Age (years)      25, 2… nume…         51    1200     0
#>  2 income                 Monthly househo… 1000,… nume…       1052    1200     0
#>  3 wellbeing_score        WHO-5 wellbeing… 18.7,… nume…        517    1200     0
#>  4 bmi                    Body mass index  16, 1… nume…        177    1188    12
#>  5 political_position     Political posit… 0, 1,… nume…         11    1185    15
#>  6 life_sat_health        Satisfaction wi… 1, 2,… inte…          5    1192     8
#>  7 life_sat_work          Satisfaction wi… 1, 2,… inte…          5    1192     8
#>  8 life_sat_relationships Satisfaction wi… 1, 2,… inte…          5    1192     8
#>  9 life_sat_standard      Satisfaction wi… 1, 2,… inte…          5    1192     8
#> 10 weight                 Survey design w… 0.294… nume…        794    1200     0
varlist(sochealth, starts_with("bmi"), tbl = TRUE)
#> # A tibble: 2 × 7
#>   Variable     Label           Values             Class N_distinct N_valid   NAs
#>   <chr>        <chr>           <chr>              <chr>      <int>   <int> <int>
#> 1 bmi          Body mass index 16, 16.6, 16.8, .… nume…        177    1188    12
#> 2 bmi_category BMI category    Normal weight, Ov… orde…          3    1188    12

vl(sochealth, tbl = TRUE)
#> # A tibble: 24 × 7
#>    Variable          Label                 Values Class N_distinct N_valid   NAs
#>    <chr>             <chr>                 <chr>  <chr>      <int>   <int> <int>
#>  1 sex               Sex                   Femal… fact…          2    1200     0
#>  2 age               Age (years)           25, 2… nume…         51    1200     0
#>  3 age_group         Age group             25-34… orde…          4    1200     0
#>  4 education         Highest education le… Lower… orde…          3    1200     0
#>  5 social_class      Subjective social cl… Lower… orde…          5    1200     0
#>  6 region            Region of residence   Centr… fact…          6    1200     0
#>  7 employment_status Employment status     Emplo… fact…          4    1200     0
#>  8 income_group      Household income gro… Low, … orde…          4    1182    18
#>  9 income            Monthly household in… 1000,… nume…       1052    1200     0
#> 10 smoking           Current smoker        No, Y… fact…          2    1175    25
#> # ℹ 14 more rows
sochealth |> vl(tbl = TRUE)
#> # A tibble: 24 × 7
#>    Variable          Label                 Values Class N_distinct N_valid   NAs
#>    <chr>             <chr>                 <chr>  <chr>      <int>   <int> <int>
#>  1 sex               Sex                   Femal… fact…          2    1200     0
#>  2 age               Age (years)           25, 2… nume…         51    1200     0
#>  3 age_group         Age group             25-34… orde…          4    1200     0
#>  4 education         Highest education le… Lower… orde…          3    1200     0
#>  5 social_class      Subjective social cl… Lower… orde…          5    1200     0
#>  6 region            Region of residence   Centr… fact…          6    1200     0
#>  7 employment_status Employment status     Emplo… fact…          4    1200     0
#>  8 income_group      Household income gro… Low, … orde…          4    1182    18
#>  9 income            Monthly household in… 1000,… nume…       1052    1200     0
#> 10 smoking           Current smoker        No, Y… fact…          2    1175    25
#> # ℹ 14 more rows
vl(sochealth, starts_with("bmi"), tbl = TRUE)
#> # A tibble: 2 × 7
#>   Variable     Label           Values             Class N_distinct N_valid   NAs
#>   <chr>        <chr>           <chr>              <chr>      <int>   <int> <int>
#> 1 bmi          Body mass index 16, 16.6, 16.8, .… nume…        177    1188    12
#> 2 bmi_category BMI category    Normal weight, Ov… orde…          3    1188    12
vl(sochealth, where(is.numeric), values = TRUE, tbl = TRUE)
#> # A tibble: 10 × 7
#>    Variable               Label            Values Class N_distinct N_valid   NAs
#>    <chr>                  <chr>            <chr>  <chr>      <int>   <int> <int>
#>  1 age                    Age (years)      25, 2… nume…         51    1200     0
#>  2 income                 Monthly househo… 1000,… nume…       1052    1200     0
#>  3 wellbeing_score        WHO-5 wellbeing… 18.7,… nume…        517    1200     0
#>  4 bmi                    Body mass index  16, 1… nume…        177    1188    12
#>  5 political_position     Political posit… 0, 1,… nume…         11    1185    15
#>  6 life_sat_health        Satisfaction wi… 1, 2,… inte…          5    1192     8
#>  7 life_sat_work          Satisfaction wi… 1, 2,… inte…          5    1192     8
#>  8 life_sat_relationships Satisfaction wi… 1, 2,… inte…          5    1192     8
#>  9 life_sat_standard      Satisfaction wi… 1, 2,… inte…          5    1192     8
#> 10 weight                 Survey design w… 0.294… nume…        794    1200     0
```
