# Build a codebook of a data frame

`code_book()` documents the variables of a data frame: one row per
variable (position, name, label, type, valid and missing counts, summary
statistics), and one row per category of its categorical and logical
variables (code, label, count, percentages). The codebook prints as the
list of variables, and `output` writes it to an Excel workbook or to a
PDF.

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
  font = NULL,
  font_code = NULL,
  colors = NULL,
  paper = c("a4", "letter"),
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

  Decimal mark of the numbers in the PDF, a single character such as
  `"."` or `","`. `NULL` (the default) takes the mark of
  `options(spicy.style)`, then the one of the language
  (`options(spicy.language = "fr")` gives the comma), then `"."`. The
  console list prints counts only, and the Excel file keeps numbers as
  numbers.

- font, font_code:

  Fonts of the PDF, for the text and for the names and codes. `NULL`
  (the default) uses New Computer Modern and DejaVu Sans Mono, which
  Typst embeds, so the PDF looks the same whatever the machine. Any
  other font must be one Typst finds, named exactly as
  `quarto typst fonts` lists it; a `.typ` output keeps the name as
  given, unchecked. A `font` also sets the font of the Excel file, which
  otherwise keeps its default font.

- colors:

  Named character vector of `"#RRGGBB"` colors replacing part of the
  palette of the PDF: `primary` (title, headings and the text of table
  headers), `accent` (links and the declared missing marker), `band`
  (behind table headers), `band_dark` (the band of each variable, under
  white text), `zebra`, `grid` (rules), `text` and `muted`. The headers
  of the Excel file take `primary` and `band` too.

- paper:

  Paper size of the PDF: `"a4"` (the default) or `"letter"`.

- output:

  `NULL` (the default) returns the codebook, which prints as the list of
  variables. A path writes the codebook to that file, in the format of
  its extension, and returns it invisibly: `.xlsx` for an Excel workbook
  (this requires `openxlsx2`), `.pdf` for a PDF (this requires the
  `quarto` package and Quarto 1.7 or later, found on the PATH or through
  the `QUARTO_PATH` environment variable), `.typ` for the Typst source
  of that PDF.

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
decimal mark the codebook was built with, and `appearance` the look of
its PDF: a list of `font`, `font_code`, `colors` (all eight) and
`paper`.

## Details

The type of a variable is read off its R class, never guessed: a factor
is categorical (nominal), an ordered factor categorical (ordinal), a
`haven_labelled` vector categorical (labelled codes), an integer or
double vector numeric, and a logical, character, `Date` or `POSIXct`
vector logical, text, date or date-time. The level of measurement comes
from the declaration alone: a factor whose order was not declared with
[`ordered()`](https://rdrr.io/r/base/factor.html) is nominal. Any other
class is shown as the class itself. The R class stays in its own column.
With `user_na = TRUE`, a `haven_labelled` vector whose value labels all
sit on declared missing codes, or that has no labels, is numeric, or
text when it stores characters.

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

## PDF output

The PDF opens on a cover (title, authors, date, numbers of observations
and variables, notes), lists the variables with the page of each, and
summarizes the declared missing values. One sheet per variable follows
(counts, statistics, and the table of its values, where `M` marks a
declared missing value), then an alphabetical index. A sheet breaks
across pages only when it does not fit on one. `code_book()` writes the
Typst source and compiles it with the Typst that Quarto bundles. Without
Quarto, `output = "<path>.typ"` writes the same source, self-contained:
`typst compile` makes the PDF on any machine.

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
#>    Pos. │ Variable                  Label                                         Type                       Valid    Missing 
#> ────────┼─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#>       1 │ sex                       Sex                                           categorical (nominal)       1200          0 
#>       2 │ age                       Age (years)                                   numeric                     1200          0 
#>       3 │ age_group                 Age group                                     categorical (ordinal)       1200          0 
#>       4 │ education                 Highest education level                       categorical (ordinal)       1200          0 
#>       5 │ social_class              Subjective social class                       categorical (ordinal)       1200          0 
#>       6 │ region                    Region of residence                           categorical (nominal)       1200          0 
#>       7 │ employment_status         Employment status                             categorical (nominal)       1200          0 
#>       8 │ income_group              Household income group                        categorical (ordinal)       1182         18 
#>       9 │ income                    Monthly household income (CHF)                numeric                     1200          0 
#>      10 │ smoking                   Current smoker                                categorical (nominal)       1175         25 
#>      11 │ physical_activity         Regular physical activity                     categorical (nominal)       1200          0 
#>      12 │ dentist_12m               Dentist visit in last 12 months               categorical (nominal)       1200          0 
#>      13 │ self_rated_health         Self-rated health                             categorical (ordinal)       1180         20 
#>      14 │ wellbeing_score           WHO-5 wellbeing index (0-100)                 numeric                     1200          0 
#>      15 │ bmi                       Body mass index                               numeric                     1188         12 
#>      16 │ bmi_category              BMI category                                  categorical (ordinal)       1188         12 
#>      17 │ institutional_trust       Trust in institutions                         categorical (ordinal)       1200          0 
#>      18 │ political_position        Political position (0 = left, 10 = right)     numeric                     1185         15 
#>      19 │ life_sat_health           Satisfaction with health (1-5)                numeric                     1192          8 
#>      20 │ life_sat_work             Satisfaction with work (1-5)                  numeric                     1192          8 
#>      21 │ life_sat_relationships    Satisfaction with relationships (1-5)         numeric                     1192          8 
#>      22 │ life_sat_standard         Satisfaction with standard of living (1-5)    numeric                     1192          8 
#>      23 │ response_date             Survey response date                          date-time                   1200          0 
#>      24 │ weight                    Survey design weight                          numeric                     1200          0 

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
#> 3       16 bmi_ca… BMI … cate… orde… NA        1188        12                  0
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

# The Typst source of the PDF, which `output = "<path>.pdf"` compiles
# when Quarto is installed.
code_book(sochealth, output = tempfile(fileext = ".typ"))
```
