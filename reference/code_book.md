# Build a codebook of a data frame

`code_book()` documents the variables of a data frame: one row per
variable (position, name, label, type, valid and missing counts, summary
statistics), and one row per category of its categorical and logical
variables (code, label, count, percentages). The codebook prints as the
list of variables, and `output = "<path>.xlsx"` writes it to an Excel
file.

The counts are unweighted: they describe the file, not a population.

## Usage

``` r
code_book(
  x,
  ...,
  title = "Codebook",
  authors = NULL,
  notes = NULL,
  source = NULL,
  values = 100,
  range = TRUE,
  factor_levels = c("all", "observed"),
  user_na = TRUE,
  decimal_mark = NULL,
  output = NULL
)

# S3 method for class 'spicy_codebook'
print(x, ...)
```

## Arguments

- x:

  A data frame or tibble. For
  [`print()`](https://rdrr.io/r/base/print.html), a `spicy_codebook`.

- ...:

  Optional tidyselect-style column selectors (e.g. `starts_with("bmi")`,
  `where(is.numeric)`). Columns can be selected or reordered, but
  renaming selections is not supported.

- title:

  Title of the codebook. Defaults to `"Codebook"`; `NULL` removes it.

- authors:

  Authors of the codebook: `NULL` (the default), a character vector
  whose names are the authors and whose values are their affiliations
  (`c("Jane Doe" = "University of Somewhere")`; an unnamed element is a
  name without affiliation), or a list of lists with `name` and the
  optional `affiliation` and `orcid`.

- notes:

  Character vector of notes on the data (source, exclusions, coding
  rules, ...), one note per element.

- source:

  Named character vector mapping the current column names to the codes
  they had in the source file, the vector `dplyr::rename(all_of())`
  takes. Its names must be selected columns.

- values:

  The maximum number of categories listed per variable in `values`. A
  variable with more keeps its count of distinct values in `variables`
  and has no rows in `values`. Defaults to `100`; `Inf` lists them all.

- range:

  Logical. If `TRUE` (the default), `variables` gives the minimum and
  maximum of each numeric variable and the earliest and latest date of
  each date; `FALSE` drops those four columns.

- factor_levels:

  Character. `"all"` (the default;
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  uses `"observed"`) lists every declared level of a factor, every
  labelled code and both values of a logical, with a count of 0 when
  unused. `"observed"` lists only the values present in the data.

- user_na:

  Logical. If `TRUE` (the default), declared missing values count as
  missing: they are left out of `n_valid`, counted in `n_missing` and
  `n_declared_missing`, and listed in `values` with
  `declared_missing = TRUE`. If `FALSE`, they count as valid values. See
  the "Declared missing values" section.

- decimal_mark:

  Decimal mark of the numbers the codebook prints, a single character
  such as `"."` or `","`. `NULL` (the default) takes the mark of
  `options(spicy.style)`, then the one of the language
  (`options(spicy.language = "fr")` gives the comma), then `"."`. The
  console list prints counts only, so the mark shows in the PDF output
  (planned). The Excel file keeps numbers as numbers.

- output:

  `NULL` (the default) returns the codebook, which prints as the list of
  variables. A path ending in `.xlsx` writes the codebook to that Excel
  file and returns it invisibly; this requires `openxlsx2`. A PDF output
  is planned.

## Value

A `spicy_codebook` object, returned invisibly when `output` is given: a
list with

- `header`:

  A list: `title`, `authors` (a tibble with `name`, `affiliation` and
  `orcid`), `date`, `n_obs`, `n_vars`, `notes`, and `declared_missing`,
  a tibble of the declared missing values found in the data (`code`,
  `label`, `variables`, `n_variables`).

- `variables`:

  A tibble, one row per variable: `position` (the column's position in
  `x`), `name`, `label`, `type`, `class`, `source`, `n_valid`,
  `n_missing`, `n_declared_missing`, `n_distinct`, then `min`, `max`,
  `mean`, `sd` and `median` for numeric variables and `earliest` and
  `latest` for dates.

- `values`:

  A tibble, one row per value: `variable`, `code`, `label`,
  `declared_missing`, `n`, `pct_total` and `pct_valid`.

The attributes `language` and `decimal_mark` record the language and the
decimal mark the codebook was built with.

## Details

The type of a variable is read off its R class, never guessed: a factor
is categorical, an ordered factor ordinal, a `haven_labelled` vector
categorical with labelled codes, an integer or double vector numeric,
and a logical, character, `Date` or `POSIXct` vector logical, text, date
or date-time. Any other class is shown as the class itself. The R class
stays in its own column. With `user_na = TRUE`, a `haven_labelled`
vector whose value labels all sit on declared missing codes, or that has
no labels, is numeric, or text when it stores characters.

`values` lists the categories of factors and labelled vectors and the
two values of a logical, then the declared missing values of the
variable and a row for its system missing values (`code = "NA"`).
Numeric, text and date variables have no category rows: they appear in
`values` only through their declared missing values. The codes of a
labelled vector come in code order, the levels of a factor in level
order.

Dates are written in ISO 8601: a date-time in the time zone the variable
carries, and in UTC when it carries none, the zone being named in either
case.

The language of the labels follows `options(spicy.language)` when the
codebook is built (see
[`spicy_labels()`](https://amaltawfik.github.io/spicy/reference/spicy_labels.md));
the labels of the data are never translated.

[`print()`](https://rdrr.io/r/base/print.html) shortens long variable
labels when that makes the list fit the console; the object keeps them
whole.

## Excel output

The workbook has three sheets, named in the language of the codebook.
The first, `codebook`, holds the header as field-value pairs: title, one
row per author, date, numbers of observations and variables, declared
missing values, notes, and the versions of spicy and R that wrote it.
The other two, `variables` and `values`, are the two tables of the
object from the first row, with a frozen header and filters: numbers
stay numeric cells and dates are ISO text.

## Declared missing values

Survey files imported with **haven** often carry *declared missing
values*: codes such as `8 = Don't know` or `9 = Refused` that the source
file marks as missing while keeping them distinct from a plain `NA`. Two
kinds of declaration exist: `na_values` / `na_range` metadata on
[`haven::labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html)
vectors, and tagged missing values created by
[`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
(the Stata `.a`, `.b`, ... convention).

spicy honors the declaration by default (`user_na = TRUE`): declared
missing values are excluded from every statistic exactly like `NA` –
valid percentages, means, chi-squared tests, association measures,
row-wise summaries, and group definitions – but they are not erased from
display.
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) lists
each observed declared value as its own row of the Missing block, with
its value label;
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
and
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
disclose the exclusion in the table note
(`Declared missing values removed: x (2).`);
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
counts them as missing in `N_valid` / `NAs` / `N_distinct` while still
listing the declared codes in `Values`; `code_book()` counts them in
`n_declared_missing` and flags them in its `values` table.

Every function involved offers the same escape hatch: set
`user_na = FALSE` to ignore the declaration and treat the declared codes
as valid values (the behavior of spicy before 0.13.0). Tagged missing
values are genuine `NA`s either way; for them, `user_na = FALSE` only
collapses the per-tag breakdown back into the regular `NA` count.

## See also

[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
to explore the variables in the Viewer;
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) for the
frequency table of one variable.

Other variable inspection:
[`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md),
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)

## Examples

``` r
code_book(sochealth)
#> Codebook
#> 
#> Date: 2026-10-07
#> Observations: 1200
#> Variables: 24
#> 
#>    Pos. │ Variable                  Label                                         Type                      Valid    Missing 
#> ────────┼────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#>       1 │ sex                       Sex                                           categorical (levels)       1200          0 
#>       2 │ age                       Age (years)                                   numeric                    1200          0 
#>       3 │ age_group                 Age group                                     ordinal (levels)           1200          0 
#>       4 │ education                 Highest education level                       ordinal (levels)           1200          0 
#>       5 │ social_class              Subjective social class                       ordinal (levels)           1200          0 
#>       6 │ region                    Region of residence                           categorical (levels)       1200          0 
#>       7 │ employment_status         Employment status                             categorical (levels)       1200          0 
#>       8 │ income_group              Household income group                        ordinal (levels)           1182         18 
#>       9 │ income                    Monthly household income (CHF)                numeric                    1200          0 
#>      10 │ smoking                   Current smoker                                categorical (levels)       1175         25 
#>      11 │ physical_activity         Regular physical activity                     categorical (levels)       1200          0 
#>      12 │ dentist_12m               Dentist visit in last 12 months               categorical (levels)       1200          0 
#>      13 │ self_rated_health         Self-rated health                             ordinal (levels)           1180         20 
#>      14 │ wellbeing_score           WHO-5 wellbeing index (0-100)                 numeric                    1200          0 
#>      15 │ bmi                       Body mass index                               numeric                    1188         12 
#>      16 │ bmi_category              BMI category                                  ordinal (levels)           1188         12 
#>      17 │ institutional_trust       Trust in institutions                         ordinal (levels)           1200          0 
#>      18 │ political_position        Political position (0 = left, 10 = right)     numeric                    1185         15 
#>      19 │ life_sat_health           Satisfaction with health (1-5)                numeric                    1192          8 
#>      20 │ life_sat_work             Satisfaction with work (1-5)                  numeric                    1192          8 
#>      21 │ life_sat_relationships    Satisfaction with relationships (1-5)         numeric                    1192          8 
#>      22 │ life_sat_standard         Satisfaction with standard of living (1-5)    numeric                    1192          8 
#>      23 │ response_date             Survey response date                          date-time                  1200          0 
#>      24 │ weight                    Survey design weight                          numeric                    1200          0 

cb <- code_book(
  sochealth,
  sex,
  starts_with("bmi"),
  title = "Body mass index",
  authors = c("Jane Doe" = "University of Somewhere"),
  notes = "BMI computed from self-reported height and weight."
)
cb$variables
#> # A tibble: 3 × 17
#>   position name    label type  class source n_valid n_missing n_declared_missing
#>      <int> <chr>   <chr> <chr> <chr> <chr>    <int>     <int>              <int>
#> 1        1 sex     Sex   cate… fact… NA        1200         0                  0
#> 2       15 bmi     Body… nume… nume… NA        1188        12                  0
#> 3       16 bmi_ca… BMI … ordi… orde… NA        1188        12                  0
#> # ℹ 8 more variables: n_distinct <int>, min <dbl>, max <dbl>, mean <dbl>,
#> #   sd <dbl>, median <dbl>, earliest <chr>, latest <chr>
cb$values
#> # A tibble: 6 × 7
#>   variable     code          label declared_missing     n pct_total pct_valid
#>   <chr>        <chr>         <chr> <lgl>            <int>     <dbl>     <dbl>
#> 1 sex          Female        NA    FALSE              620      51.7      51.7
#> 2 sex          Male          NA    FALSE              580      48.3      48.3
#> 3 bmi_category Normal weight NA    FALSE              465      38.8      39.1
#> 4 bmi_category Overweight    NA    FALSE              569      47.4      47.9
#> 5 bmi_category Obesity       NA    FALSE              154      12.8      13.0
#> 6 bmi_category NA            NA    FALSE               12       1        NA  

if (requireNamespace("openxlsx2", quietly = TRUE)) {
  path <- tempfile(fileext = ".xlsx")
  code_book(sochealth, output = path)
}
```
