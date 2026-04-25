# Explore variables and build codebooks in R

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
and build an interactive codebook with
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md).

This vignette focuses on three common tasks:

- clean imported column names and recover variable labels with
  [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
- inspect variables, labels, values, classes, and missing data with
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  and [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
- generate an interactive codebook for review or export with
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

This is especially useful for LimeSurvey CSV exports when using Export
results -\> Export format: CSV -\> Headings: Question code & question
text, where column names look like `"code. question text"`. In this case
the default separator is `". "`.

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
```

If you want the `Values` column to include explicit missing values, use
`include_na = TRUE`:

``` r
head(subset(varlist(sochealth, include_na = TRUE, tbl = TRUE), NAs > 0))
#> # A tibble: 6 × 7
#>   Variable           Label                 Values Class N_distinct N_valid   NAs
#>   <chr>              <chr>                 <chr>  <chr>      <int>   <int> <int>
#> 1 income_group       Household income gro… Low, … orde…          4    1182    18
#> 2 smoking            Current smoker        No, Y… fact…          2    1175    25
#> 3 self_rated_health  Self-rated health     Poor,… orde…          4    1180    20
#> 4 bmi                Body mass index       16, 1… nume…        177    1188    12
#> 5 bmi_category       BMI category          Norma… orde…          3    1188    12
#> 6 political_position Political position (… 0, 1,… nume…         11    1185    15
```

If you want to display all unique non-missing values in the `Values`
column, use `values = TRUE`. This is especially useful for variables
with a small number of distinct values:

``` r
head(subset(varlist(sochealth, values = TRUE, tbl = TRUE), N_distinct <= 5))
#> # A tibble: 6 × 7
#>   Variable          Label                  Values Class N_distinct N_valid   NAs
#>   <chr>             <chr>                  <chr>  <chr>      <int>   <int> <int>
#> 1 sex               Sex                    Femal… fact…          2    1200     0
#> 2 age_group         Age group              25-34… orde…          4    1200     0
#> 3 education         Highest education lev… Lower… orde…          3    1200     0
#> 4 social_class      Subjective social cla… Lower… orde…          5    1200     0
#> 5 employment_status Employment status      Emplo… fact…          4    1200     0
#> 6 income_group      Household income group Low, … orde…          4    1182    18
```

For a focused inspection, select only the variables you want to review:

``` r
varlist(sochealth, smoking, education, income_group, tbl = TRUE)
#> # A tibble: 3 × 7
#>   Variable     Label                   Values     Class N_distinct N_valid   NAs
#>   <chr>        <chr>                   <chr>      <chr>      <int>   <int> <int>
#> 1 smoking      Current smoker          No, Yes    fact…          2    1175    25
#> 2 education    Highest education level Lower sec… orde…          3    1200     0
#> 3 income_group Household income group  Low, Lowe… orde…          4    1182    18
```

This is often enough to confirm that labels, factor levels, and missing
values look correct before moving on to tabulations.

## Select subsets of variables

[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
supports tidyselect, which makes it easy to inspect a subset of
variables by name pattern or type.

``` r
varlist(sochealth, starts_with("life_sat"), tbl = TRUE)
#> # A tibble: 4 × 7
#>   Variable               Label             Values Class N_distinct N_valid   NAs
#>   <chr>                  <chr>             <chr>  <chr>      <int>   <int> <int>
#> 1 life_sat_health        Satisfaction wit… 1, 2,… inte…          5    1192     8
#> 2 life_sat_work          Satisfaction wit… 1, 2,… inte…          5    1192     8
#> 3 life_sat_relationships Satisfaction wit… 1, 2,… inte…          5    1192     8
#> 4 life_sat_standard      Satisfaction wit… 1, 2,… inte…          5    1192     8
```

``` r
varlist(sochealth, where(is.numeric), tbl = TRUE)
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

[`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md) also
works with tidyselect in the same way:

``` r
vl(sochealth, starts_with("bmi"), tbl = TRUE)
#> # A tibble: 2 × 7
#>   Variable     Label           Values             Class N_distinct N_valid   NAs
#>   <chr>        <chr>           <chr>              <chr>      <int>   <int> <int>
#> 1 bmi          Body mass index 16, 16.6, 16.8, .… nume…        177    1188    12
#> 2 bmi_category BMI category    Normal weight, Ov… orde…          3    1188    12
```

## Build an interactive codebook

When you want a searchable and exportable overview of the whole dataset
or a selected set of variables,
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
builds an interactive codebook in the Viewer.

``` r
if (requireNamespace("DT", quietly = TRUE)) {
  code_book(sochealth)
}
```

Use the same tidyselect-style selectors as
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
to build a focused codebook:

``` r
if (requireNamespace("DT", quietly = TRUE)) {
  code_book(
    sochealth,
    starts_with("bmi"),
    values = TRUE,
    title = "BMI codebook",
    filename = "bmi_codebook"
  )
}
```

You can also request a fuller display of values or include missing
values explicitly in the summary:

``` r
if (requireNamespace("DT", quietly = TRUE)) {
  code_book(sochealth, values = TRUE, include_na = TRUE)
}
```

This is useful when reviewing a dataset with collaborators or preparing
documentation before analysis.

## When to use varlist() and code_book()

Use
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
when you want a quick summary in a script or a tibble you can inspect
directly.

Use [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
when you want the same summary with a shorter call in interactive work.

Use
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
when you want a searchable, interactive codebook for review or export.
