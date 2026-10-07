# Explore variables and build codebooks

``` r

library(spicy)
```

Before you build frequency tables or cross-tabulations, it is often
worth checking how your variables are named, labelled, and coded.

spicy provides a simple workflow for variable exploration and
documentation in R. You can derive labels from imported column names,
inspect variables with
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
or [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md),
and build a codebook with
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md).

This article focuses on three common tasks:

- clean imported column names and recover variable labels with
  [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
- inspect variables, labels, values, classes, and missing data with
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  and [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
- build a codebook, in the console, an Excel file, or a PDF, with
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)

These tools are especially useful for survey datasets, labelled data,
and imported files where variable names and labels need to be checked
before analysis.

## Why inspect variables before analysis?

Variable inspection helps catch common problems early: unclear names,
missing labels, unexpected coding, and variables with many missing
values. A quick review of your dataset also makes it easier to choose
which variables to tabulate, summarize, or report later.

## Recover labels from imported column names

Some imported files store both a variable name and a variable label in
the column header.
[`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
splits names of the form `name<sep>label`, renames the columns, and
stores the label as a proper variable label.

``` r

df <- tibble::tibble(
  "age. Age of respondent" = c(25, 30, 41),
  "edu. Highest education level" = c("Lower", "Upper", "Tertiary"),
  "smoke. Current smoker" = c("No", "Yes", "No")
)

out <- label_from_names(df)
labelled::var_label(out)
#> $age
#> [1] "Age of respondent"
#> 
#> $edu
#> [1] "Highest education level"
#> 
#> $smoke
#> [1] "Current smoker"
```

The default separator `". "` was chosen to match **LimeSurvey CSV
exports** taken with *Export results -\> Export format: CSV -\>
Headings: Question code & question text*, which produce column names of
the form `"code. question text"`. Pass `sep =` to use any other literal
separator.

## Inspect variables with varlist()

[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
gives a compact summary of each variable, including its name, label,
representative values, class, number of distinct values, number of valid
observations, and missing values.

In RStudio or Positron, the main way to use
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
is interactively. With its default behavior, it opens a searchable,
sortable variable overview in the Viewer, which makes it easy to scan
labels, look for specific variables, filter what you want to inspect,
and review the structure of a dataset before analysis.

``` r

varlist(sochealth)
```

If you prefer a shorter call in interactive work,
[`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md) is a
shortcut for
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md):

``` r

vl(sochealth)
```

If you want the same summary returned as a tibble, use `tbl = TRUE`:

``` r

varlist(sochealth, tbl = TRUE)
#> # A tibble: 24 × 7
#>    Variable          Label                          Values            Class N_distinct N_valid   NAs
#>    <chr>             <chr>                          <chr>             <chr>      <int>   <int> <int>
#>  1 sex               Sex                            Female, Male      fact…          2    1200     0
#>  2 age               Age (years)                    25, 26, 27, ...,… nume…         51    1200     0
#>  3 age_group         Age group                      25-34, 35-49, 50… orde…          4    1200     0
#>  4 education         Highest education level        Lower secondary,… orde…          3    1200     0
#>  5 social_class      Subjective social class        Lower, Working, … orde…          5    1200     0
#>  6 region            Region of residence            Central, East, N… fact…          6    1200     0
#>  7 employment_status Employment status              Employed, Studen… fact…          4    1200     0
#>  8 income_group      Household income group         Low, Lower middl… orde…          4    1182    18
#>  9 income            Monthly household income (CHF) 1000, 1001, 1024… nume…       1052    1200     0
#> 10 smoking           Current smoker                 No, Yes           fact…          2    1175    25
#> # ℹ 14 more rows
```

If you want the `Values` column to include explicit missing values, use
`include_na = TRUE`. Selecting a few variables and keeping only the
relevant columns lets the `Values` strings print in full, so the added
`<NA>` entry is actually visible:

``` r

varlist(sochealth, smoking, income_group, self_rated_health,
  include_na = TRUE, tbl = TRUE
)[, c("Variable", "Values", "NAs")]
#> # A tibble: 3 × 3
#>   Variable          Values                                        NAs
#>   <chr>             <chr>                                       <int>
#> 1 smoking           No, Yes, <NA>                                  25
#> 2 income_group      Low, Lower middle, Upper middle, High, <NA>    18
#> 3 self_rated_health Poor, Fair, Good, Very good, <NA>              20
```

By default, the `Values` column elides longer value lists with `...`. If
you want to display all unique non-missing values instead, use
`values = TRUE`. This is especially useful for variables with a small
number of distinct values, where the full list fits on one line. Compare
the default display with the `values = TRUE` display for the same two
variables:

``` r

varlist(sochealth, life_sat_health, social_class,
  tbl = TRUE
)[, c("Variable", "Values", "N_distinct")]
#> # A tibble: 2 × 3
#>   Variable        Values                                          N_distinct
#>   <chr>           <chr>                                                <int>
#> 1 life_sat_health 1, 2, 3, ..., 5                                          5
#> 2 social_class    Lower, Working, Lower middle, ..., Upper middle          5

varlist(sochealth, life_sat_health, social_class,
  values = TRUE, tbl = TRUE
)[, c("Variable", "Values", "N_distinct")]
#> # A tibble: 2 × 3
#>   Variable        Values                                             N_distinct
#>   <chr>           <chr>                                                   <int>
#> 1 life_sat_health 1, 2, 3, 4, 5                                               5
#> 2 social_class    Lower, Working, Lower middle, Middle, Upper middle          5
```

For a focused inspection, select only the variables you want to review:

``` r

varlist(sochealth, smoking, education, income_group, tbl = TRUE)
#> # A tibble: 3 × 7
#>   Variable     Label                   Values                         Class N_distinct N_valid   NAs
#>   <chr>        <chr>                   <chr>                          <chr>      <int>   <int> <int>
#> 1 smoking      Current smoker          No, Yes                        fact…          2    1175    25
#> 2 education    Highest education level Lower secondary, Upper second… orde…          3    1200     0
#> 3 income_group Household income group  Low, Lower middle, Upper midd… orde…          4    1182    18
```

Declared missing values (haven’s `na_values`/`na_range`, tagged NAs)
count as missing here too: `N_valid`, `NAs`, and `N_distinct` all honor
the declaration, consistently with the tabulation functions (see the
“Declared missing values” section of
[`?freq`](https://amaltawfik.github.io/spicy/reference/freq.md)).

This is often enough to confirm that labels, factor levels, and missing
values look correct before moving on to tabulations.

## Select subsets of variables

[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
supports tidyselect, which makes it easy to inspect a subset of
variables by name pattern or type.

``` r

varlist(sochealth, starts_with("life_sat"), tbl = TRUE)
#> # A tibble: 4 × 7
#>   Variable               Label                                 Values Class N_distinct N_valid   NAs
#>   <chr>                  <chr>                                 <chr>  <chr>      <int>   <int> <int>
#> 1 life_sat_health        Satisfaction with health (1-5)        1, 2,… inte…          5    1192     8
#> 2 life_sat_work          Satisfaction with work (1-5)          1, 2,… inte…          5    1192     8
#> 3 life_sat_relationships Satisfaction with relationships (1-5) 1, 2,… inte…          5    1192     8
#> 4 life_sat_standard      Satisfaction with standard of living… 1, 2,… inte…          5    1192     8
```

``` r

varlist(sochealth, where(is.numeric), tbl = TRUE)
#> # A tibble: 10 × 7
#>    Variable               Label                                Values Class N_distinct N_valid   NAs
#>    <chr>                  <chr>                                <chr>  <chr>      <int>   <int> <int>
#>  1 age                    Age (years)                          25, 2… nume…         51    1200     0
#>  2 income                 Monthly household income (CHF)       1000,… nume…       1052    1200     0
#>  3 wellbeing_score        WHO-5 wellbeing index (0-100)        18.7,… nume…        517    1200     0
#>  4 bmi                    Body mass index                      16, 1… nume…        177    1188    12
#>  5 political_position     Political position (0 = left, 10 = … 0, 1,… nume…         11    1185    15
#>  6 life_sat_health        Satisfaction with health (1-5)       1, 2,… inte…          5    1192     8
#>  7 life_sat_work          Satisfaction with work (1-5)         1, 2,… inte…          5    1192     8
#>  8 life_sat_relationships Satisfaction with relationships (1-… 1, 2,… inte…          5    1192     8
#>  9 life_sat_standard      Satisfaction with standard of livin… 1, 2,… inte…          5    1192     8
#> 10 weight                 Survey design weight                 0.294… nume…        794    1200     0
```

[`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md) also
works with tidyselect in the same way:

``` r

vl(sochealth, starts_with("bmi"), tbl = TRUE)
#> # A tibble: 2 × 7
#>   Variable     Label           Values                             Class     N_distinct N_valid   NAs
#>   <chr>        <chr>           <chr>                              <chr>          <int>   <int> <int>
#> 1 bmi          Body mass index 16, 16.6, 16.8, ..., 38.9          numeric          177    1188    12
#> 2 bmi_category BMI category    Normal weight, Overweight, Obesity ordered,…          3    1188    12
```

## Build a codebook

A codebook is the document that travels with a data file: what each
variable measures, how it is coded, and how many observations carry each
value.
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
builds it from the data frame itself and returns it as an object you can
keep, inspect, or write to Excel or PDF, which prints as the list of
variables. The counts are unweighted: they describe the file, not the
population.

`code_book(sochealth)` documents every variable. The same tidyselect
selectors as in
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
narrow it down:

``` r

code_book(sochealth, sex, age, income_group, starts_with("bmi"))
#> Codebook
#> 
#> Date: 2026-10-07
#> Observations: 1200
#> Variables: 5
#> 
#>    Pos. │ Variable        Label                     Type                      Valid    Missing 
#> ────────┼──────────────────────────────────────────────────────────────────────────────────────
#>       1 │ sex             Sex                       categorical (levels)       1200          0 
#>       2 │ age             Age (years)               numeric                    1200          0 
#>       8 │ income_group    Household income group    ordinal (levels)           1182         18 
#>      15 │ bmi             Body mass index           numeric                    1188         12 
#>      16 │ bmi_category    BMI category              ordinal (levels)           1188         12
```

The type is read off the R class, never guessed: a factor is
*categorical*, an ordered factor *ordinal*, a labelled vector
*categorical (labelled codes)*, an integer or double vector *numeric*,
and a logical, character, `Date`, or `POSIXct` vector *logical*, *text*,
*date*, or *date-time*. The R class itself stays in the object.

### The codebook object

Keep the result to work with its parts:

``` r

cb <- code_book(sochealth, sex, age, income_group, starts_with("bmi"))
```

`variables` has one row per variable: its position in the data frame,
name, label, type, R class, valid and missing counts, number of distinct
values, and for numeric variables the minimum, maximum, mean, standard
deviation, and median (dates get their earliest and latest value
instead):

``` r

cb$variables[, c("name", "type", "n_valid", "n_missing", "n_distinct", "min", "max", "mean")]
#> # A tibble: 5 × 8
#>   name         type                 n_valid n_missing n_distinct   min   max  mean
#>   <chr>        <chr>                  <int>     <int>      <int> <dbl> <dbl> <dbl>
#> 1 sex          categorical (levels)    1200         0          2    NA  NA    NA  
#> 2 age          numeric                 1200         0         51    25  75    49.3
#> 3 income_group ordinal (levels)        1182        18          4    NA  NA    NA  
#> 4 bmi          numeric                 1188        12        177    16  38.9  25.9
#> 5 bmi_category ordinal (levels)        1188        12          3    NA  NA    NA
```

`values` has one row per category of the categorical, ordinal, and
logical variables, with its count and its percentages of all
observations and of the valid ones, plus a row for the missing values:

``` r

cb$values
#> # A tibble: 11 × 7
#>    variable     code          label declared_missing     n pct_total pct_valid
#>    <chr>        <chr>         <chr> <lgl>            <int>     <dbl>     <dbl>
#>  1 sex          Female        NA    FALSE              620      51.7      51.7
#>  2 sex          Male          NA    FALSE              580      48.3      48.3
#>  3 income_group Low           NA    FALSE              247      20.6      20.9
#>  4 income_group Lower middle  NA    FALSE              388      32.3      32.8
#>  5 income_group Upper middle  NA    FALSE              328      27.3      27.7
#>  6 income_group High          NA    FALSE              219      18.2      18.5
#>  7 income_group NA            NA    FALSE               18       1.5      NA  
#>  8 bmi_category Normal weight NA    FALSE              465      38.8      39.1
#>  9 bmi_category Overweight    NA    FALSE              569      47.4      47.9
#> 10 bmi_category Obesity       NA    FALSE              154      12.8      13.0
#> 11 bmi_category NA            NA    FALSE               12       1        NA
```

Numeric variables are summarized in `variables` and have no rows in
`values`; text variables and dates have none either.

### Title, authors, and notes

The header carries what a reader needs to cite and trust the document: a
title, the authors with their affiliation, the date, the numbers of
observations and variables, and your notes on the data (source,
exclusions, coding rules), one note per element:

``` r

code_book(
  sochealth,
  starts_with("bmi"),
  title = "Social health survey: body mass index",
  authors = c("Jane Doe" = "University of Somewhere"),
  notes = c(
    "Fictitious data shipped with spicy.",
    "BMI computed from self-reported height and weight."
  )
)
#> Social health survey: body mass index
#> Jane Doe — University of Somewhere
#> 
#> Date: 2026-10-07
#> Observations: 1200
#> Variables: 2
#> Note: Fictitious data shipped with spicy.
#> Note: BMI computed from self-reported height and weight.
#> 
#>    Pos. │ Variable        Label              Type                  Valid    Missing 
#> ────────┼───────────────────────────────────────────────────────────────────────────
#>      15 │ bmi             Body mass index    numeric                1188         12 
#>      16 │ bmi_category    BMI category       ordinal (levels)       1188         12
```

`authors` also accepts a list of lists with `name`, `affiliation`, and
`orcid`. When the variables were renamed after import, `source` records
the code each one had in the source file, such as a LimeSurvey question
code: `source = c(sex = "Q1", age = "Q2")` fills the `source` column of
`variables`.

The words the codebook adds (column headers, types, header fields)
follow `options(spicy.language)`: set it to `"fr"` for a French
codebook. The labels of the data are never translated.

### Declared missing values

Data imported from SPSS or Stata often declare missing codes, such as
99998 = *Don’t know* and 99999 = *Refused*.
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
honours the declaration as the tabulation functions do (see the
“Declared missing values” section of
[`?freq`](https://amaltawfik.github.io/spicy/reference/freq.md)): the
declared codes count as missing, are listed in `values` with
`declared_missing = TRUE`, and are summarized in the header with the
variables that carry them.

``` r

income <- haven::labelled_spss(
  c(3200, 4100, 99998, 5600, 99999, 2900, NA, 7400),
  labels = c("Don't know" = 99998, "Refused" = 99999),
  na_values = c(99998, 99999)
)
attr(income, "label") <- "Monthly household income (CHF)"

cbi <- code_book(tibble::tibble(income))
cbi$values
#> # A tibble: 3 × 7
#>   variable code  label      declared_missing     n pct_total pct_valid
#>   <chr>    <chr> <chr>      <lgl>            <int>     <dbl>     <dbl>
#> 1 income   99998 Don't know TRUE                 1      12.5        NA
#> 2 income   99999 Refused    TRUE                 1      12.5        NA
#> 3 income   NA    NA         FALSE                1      12.5        NA
```

Here every value label sits on a declared missing code, and the valid
values are measurements.
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
therefore reads `income` as numeric, not as categorical: `variables`
gives its minimum, maximum, mean, and median on the valid values. A
labelled vector with at least one label on a valid code (`1 = Yes`,
`2 = No`) stays categorical. With `user_na = FALSE`, the declaration is
ignored and the declared codes count as valid values.

### Write the codebook to Excel

`output = "<path>.xlsx"` writes the codebook to an Excel workbook (this
needs the `openxlsx2` package): a first sheet with the header, then the
`variables` and `values` tables. Each table starts on row 1 with a
frozen header and filters, so it sorts cleanly and reads back without
skipping rows. Numbers stay numeric cells, and dates are ISO text.

``` r

code_book(
  sochealth,
  title = "Social health survey",
  authors = c("Jane Doe" = "University of Somewhere"),
  output = "sochealth_codebook.xlsx"
)
```

### Write the codebook to PDF

`output = "<path>.pdf"` writes the codebook as a document to share: a
cover with the header, the list of variables with their pages, one sheet
per variable, and an alphabetical index. It needs Quarto 1.7 or later,
whose bundled Typst compiles the PDF; without Quarto,
`output = "<path>.typ"` writes the Typst source, to compile with
`typst compile` on another machine.

``` r

code_book(
  sochealth,
  title = "Social health survey",
  authors = c("Jane Doe" = "University of Somewhere"),
  output = "sochealth_codebook.pdf"
)
```

## When to use varlist() and code_book()

Use
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
when you want a quick summary in a script or a tibble you can inspect
directly.

Use [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
when you want the same summary with a shorter call in interactive work.

Use
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
when you want a codebook to keep or share: the counts of every value, in
an object, an Excel file, or a PDF.

The two tools differ in one default:
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
shows only the **observed** factor levels in its `Values` column (the
convention of Stata `tab` and of the SPSS `FREQUENCIES` default, which
both list only values present in the data), whereas
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
lists **all declared** levels in `values`, unused ones with a count of 0
– the full coding scheme, as a data dictionary or SPSS `CTABLES` would
report it. Pass `factor_levels =` explicitly to either function to
override the default.
