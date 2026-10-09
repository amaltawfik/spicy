# Build a codebook of a data frame

`code_book()` documents the variables of a data frame: one row per
variable (position, name, label, type, valid and missing counts, summary
statistics), and one row per category of its categorical and logical
variables (code, label, count, percentages). The codebook prints as the
list of variables, and `output` writes it to an Excel workbook or to a
PDF.

Counts and percentages are unweighted: they describe the data file and
are not estimates for a population.

## Usage

``` r
code_book(
  x,
  ...,
  title = "Codebook",
  subtitle = NULL,
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
  font_size = 10,
  colors = NULL,
  paper = c("a4", "letter"),
  index_columns = NULL,
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

  Title of the codebook, such as the name of the study. The PDF adds the
  word "Codebook" above it on the cover and before it in the page
  header, unless the title already contains the word. Defaults to
  `"Codebook"`. `NULL` leaves the console and the Excel file without a
  title; the cover of the PDF then shows "Codebook" once.

- subtitle:

  Subtitle of the codebook, under the title: the wave, the edition, the
  extract (`"Wave 3, 2026, public-use file"`).

- authors:

  Authors of the codebook: `NULL` (the default), a character vector
  whose names are the authors and whose values are their affiliations
  (`c("Jane Doe" = "University of Somewhere")`; an unnamed element is a
  name without affiliation), or a list of lists with `name` and the
  optional `affiliation` and `orcid`. An ORCID given as its
  `https://orcid.org/` address is kept as the identifier alone.

- notes:

  Character vector of notes on the data (source, exclusions, coding
  rules, ...), one note per element; blank notes are dropped. In the
  PDF, an element that starts with `"- "` or `"* "` is a list item,
  consecutive items making one list, and any other element is a
  paragraph:
  `notes = c("Wave 3 only.", "- Weight: design weight.", "- BMI: kg/m2.")`.
  A note may carry `*italics*`, `**bold**`, and `` `code` `` (not
  nested), and a web address or a `doi:` becomes a link. The PDF renders
  these, turns straight apostrophes into typographic ones and, in
  French, the space before `:`, `;`, `!`, or `?` into a non-breaking
  space; the console and the Excel file show the words without the
  marks.

- source:

  Named character vector mapping the current column names to the codes
  they had in the source file, the vector `dplyr::rename(all_of())`
  takes. Its names must be selected columns, and each code must be
  non-empty.

- values:

  The maximum number of categories listed per variable in the `values`
  table; under `factor_levels = "all"`, unused levels and codes count. A
  variable with more lists its first `values` categories in their order,
  with their counts and percentages of the whole variable, then its
  declared and system missing values; `n_categories` in `variables`
  gives the total, and the PDF adds a row for the categories not listed.
  Defaults to `100`; `Inf` lists them all.

- range:

  Logical. If `TRUE` (the default), `variables` gives the minimum and
  maximum of each numeric variable and the earliest and latest date of
  each date; `FALSE` drops those four columns.

- factor_levels:

  Character. `"all"` (the default;
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  uses `"observed"`) lists every declared level of a factor, every
  labelled code, every code of `na_values`, and both values of a
  logical, with a count of 0 when unused. `"observed"` lists only the
  values present in the data.

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

  Fonts of the PDF: `font` for the text, the values and their codes
  included, and `font_code` for the variable names and the source codes.
  `NULL` (the default) uses New Computer Modern and DejaVu Sans Mono,
  which Typst embeds, so the PDF looks the same on every machine for the
  characters these fonts cover; any other character falls back to a font
  found on the machine. Any other font must be one Typst finds, named
  exactly as `quarto typst fonts` lists it; a `.typ` output keeps the
  name as given, unchecked. A `font` also sets the font of the Excel
  file, which otherwise keeps its default font.

- font_size:

  Size of the text of the PDF, in points: `10` (the default), or a
  number from 6 to 24. The title of the cover and the small size of a
  long name follow it; the margins do not.

- colors:

  Named character vector of `"#RRGGBB"` colors replacing part of the
  palette of the PDF: `primary` (title, headings, and the text of table
  headers), `accent` (the word "Codebook" above the title, and the
  links), `band` (behind table headers), `band_dark` (the band of each
  variable, under white text), `zebra` (behind the label rows of a
  variable), `grid` (rules), `text`, and `muted`. A `band` given without
  `zebra` brings a lighter tint of itself as `zebra`, and a `band_dark`
  given without `grid` a light tint of itself as `grid`. The headers of
  the Excel file take `primary` and `band` too.

- paper:

  Paper size of the PDF: `"a4"` (the default) or `"letter"`.

- index_columns:

  Columns of the index of variables at the end of the PDF: `1` or `2`.
  `NULL` (the default) sets two columns past 40 variables when no name
  exceeds 40 characters, one column otherwise. A name longer than that
  may overflow a column of two.

- output:

  `NULL` (the default) returns the codebook, which prints as the list of
  variables. A path writes the codebook to that file, in the format of
  its extension, and returns it invisibly: `.xlsx` for an Excel workbook
  (this requires `openxlsx2`), `.pdf` for a PDF (this requires the
  `quarto` package and Quarto 1.7 or later, found on the PATH or through
  the `QUARTO_PATH` environment variable), `.typ` for the Typst source
  of that PDF. The path names a file, in a directory that exists.

## Value

A `spicy_codebook` object, returned invisibly when `output` is given: a
list with

- `header`:

  A list: `title`, `subtitle`, `authors` (a tibble with `name`,
  `affiliation`, and `orcid`), `date`, `n_obs`, `n_vars`, `notes` (blank
  notes dropped), and `declared_missing`, a tibble of the declared
  missing values found in the data and, under `factor_levels = "all"`,
  of the declared codes no observation carries, sorted by code then
  label (`code`, `label`, `variables`, `n_variables`).

- `variables`:

  A tibble, one row per variable: `position` (the column's position in
  `x`), `name`, `label`, `type`, `class`, `source`, `n_valid`,
  `n_missing`, `n_declared_missing`, `declared_codes` (the `na_values`
  and `na_range` of a `haven_labelled_spss` vector, as text, the two
  parts separated by a semicolon; `NA` without them or under
  `user_na = FALSE`), `n_distinct`, `n_categories` (the categories of a
  categorical or logical variable, listed or not; `NA` otherwise), then
  `min`, `max`, `mean`, `sd`, and `median` for numeric variables and
  `earliest` and `latest` for dates and times. `range = FALSE` drops
  `min`, `max`, `earliest`, and `latest`.

- `values`:

  A tibble, one row per value: `variable`, `code`, `label`,
  `declared_missing`, `n`, `pct_total`, and `pct_valid`. The row of the
  system missing values (`code = "NA"`) exists only for a variable that
  has missing values.

The attributes `language` and `decimal_mark` record the language and the
decimal mark the codebook was built with, and `appearance` the look of
its PDF: a list of `font`, `font_code`, `font_size`, `colors` (all
eight), `paper`, and `index_columns`.

## Details

The type of a variable is read off its R class, never guessed: a factor
is categorical (nominal), an ordered factor categorical (ordinal), a
`haven_labelled` vector categorical (labelled codes), an integer or
double vector numeric, a logical, character, or `Date` vector logical,
text, or date, a `POSIXct` or `POSIXlt` vector date-time, and an `hms`
vector time. The level of measurement comes from the declaration alone:
a factor whose order was not declared with
[`ordered()`](https://rdrr.io/r/base/factor.html) is nominal. A vector
of any other class is shown by its first class (`difftime`, ...),
without statistics. The R class stays in its own column. A
`haven_labelled` vector without value labels is numeric, or text when it
stores characters, and so is one whose value labels all sit on declared
missing codes, with `user_na = TRUE`.

`values` lists the categories of factors and labelled vectors and the
two values of a logical, then the declared missing values of the
variable and, when it has any, a row for its system missing values
(`code = "NA"`). Numeric, text, and date variables have no category
rows: they appear in `values` only through their declared missing
values. The codes of a labelled vector come in code order, the levels of
a factor in level order. An explicit `NA` level of a factor (from
[`addNA()`](https://rdrr.io/r/base/factor.html)) counts as missing, in
the row of the system missing values.

Dates are written as `YYYY-MM-DD`, date-times as `YYYY-MM-DD HH:MM:SS`
followed by the name of the time zone the variable carries, or `UTC`
when it carries none, and times as `HH:MM:SS`.

The words the codebook adds (column headers, types, field and sheet
names) follow `options(spicy.language)` when the codebook is built (see
[`spicy_labels()`](https://amaltawfik.github.io/spicy/reference/spicy_labels.md));
the variable and value labels of the data are never translated.

[`print()`](https://rdrr.io/r/base/print.html) shows a line break or a
tab in a label as a space, and shortens long variable labels, counting
display columns, when that makes the list fit the console; the object
keeps the labels whole.

## Excel output

The workbook has three worksheets, named in the language of the
codebook. The first, `codebook`, holds the header as field-value pairs:
title, subtitle, one row per author, date, numbers of observations and
variables, declared missing values, notes, and the versions of spicy and
R that wrote it, with the date and time of writing. The other two,
`variables` and `values`, hold the two tables of the object from the
first row, under the column headers the console and the PDF show (`code`
is "Value", `n_valid` "Valid"), with a frozen header and filters.
Numbers stay numeric cells, the percentages shown to one decimal; a
statistic that is not finite (of a column holding `Inf`) is an empty
cell, and dates stay text, written as above.

## PDF output

The PDF opens on a cover: the word "Codebook" above the title (unless
the title already contains it), the subtitle, the authors with their
affiliations and ORCID addresses, the date, and at the foot the versions
of spicy and R that made it. A page about the data follows (numbers of
observations and variables, then the notes and the declared missing
values), then the list of variables with the page of each.

One sheet per variable comes next, under the heading "Variable sheets":
a dark band with the position, name, and type (a name over 45 characters
takes a row of its own), rows for the label, the declared missing codes,
and the source code, then the counts, the statistics, and the table of
the values. On a sheet that lists declared missing codes, a Missing
column marks them with `M`; the row of the system missing values reads
"System missing". Minimum and maximum are written at the precision of
the data; mean, SD, and median at three significant digits of the SD. A
sheet breaks across pages only when it does not fit on one; its table of
values then repeats its header under "name (continued)". An index of the
variables closes the document, sorted by name in byte order, where
uppercase letters come before lowercase ones.

`code_book()` writes the Typst source and compiles it with the Typst
that Quarto bundles; Typst warnings, such as a character missing from
the fonts, arrive as one R warning of class `spicy_typst_warning`.
Without Quarto, `output = "<path>.typ"` writes the same source,
self-contained: `typst compile` makes the PDF on any machine with Typst
0.12 or later.

## Declared missing values

Survey files imported with **haven** often carry *declared missing
values*: codes such as `8 = Don't know` or `9 = Refused` that the source
file marks as missing while keeping them distinct from a plain `NA`. Two
kinds of declaration exist: `na_values` / `na_range` metadata on
[`haven::labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html)
vectors, and tagged missing values created by
[`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
(the Stata `.a`, `.b`, ... convention).
[`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html)
keeps the `na_values` / `na_range` declaration only with
`user_na = TRUE`: its default turns the declared codes into `NA` on
import.

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
frequency table of one variable; the article [Explore variables and
build
codebooks](https://amaltawfik.github.io/spicy/articles/variable-exploration.html).

Other variable inspection:
[`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md),
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)

## Examples

``` r
code_book(sochealth)
#> Codebook
#> 
#> Date: 2026-10-09
#> Observations: 1200
#> Variables: 24
#> Note: Counts and percentages are unweighted: they describe the data file and are not estimates for a population.
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
  starts_with("bmi"),
  title = "Social health survey",
  subtitle = "Body mass index",
  authors = c("Jane Doe" = "University of Somewhere"),
  notes = "Simulated data (see ?sochealth)."
)
cb
#> Social health survey
#> Body mass index
#> Jane Doe – University of Somewhere
#> 
#> Date: 2026-10-09
#> Observations: 1200
#> Variables: 2
#> Note: Counts and percentages are unweighted: they describe the data file and are not estimates for a population.
#> Note: Simulated data (see ?sochealth).
#> 
#>    Pos. │ Variable        Label              Type                       Valid    Missing 
#> ────────┼────────────────────────────────────────────────────────────────────────────────
#>      15 │ bmi             Body mass index    numeric                     1188         12 
#>      16 │ bmi_category    BMI category       categorical (ordinal)       1188         12 
cb$variables
#> # A tibble: 2 × 19
#>   position name    label type  class source n_valid n_missing n_declared_missing
#>      <int> <chr>   <chr> <chr> <chr> <chr>    <int>     <int>              <int>
#> 1       15 bmi     Body… nume… nume… NA        1188        12                  0
#> 2       16 bmi_ca… BMI … cate… orde… NA        1188        12                  0
#> # ℹ 10 more variables: declared_codes <chr>, n_distinct <int>,
#> #   n_categories <int>, min <dbl>, max <dbl>, mean <dbl>, sd <dbl>,
#> #   median <dbl>, earliest <chr>, latest <chr>
cb$values
#> # A tibble: 4 × 7
#>   variable     code          label declared_missing     n pct_total pct_valid
#>   <chr>        <chr>         <chr> <lgl>            <int>     <dbl>     <dbl>
#> 1 bmi_category Normal weight NA    FALSE              465      38.8      39.1
#> 2 bmi_category Overweight    NA    FALSE              569      47.4      47.9
#> 3 bmi_category Obesity       NA    FALSE              154      12.8      13.0
#> 4 bmi_category NA            NA    FALSE               12       1        NA  

# Labelled survey data: declared missing codes count as missing and are
# flagged in `values`.
if (requireNamespace("haven", quietly = TRUE)) {
  trust <- haven::labelled_spss(
    c(1, 2, 2, 3, 4, 8, 9, 1, NA, 2),
    labels = c(
      "Not at all" = 1, "A little" = 2, "Somewhat" = 3, "A lot" = 4,
      "Don't know" = 8, "Refused" = 9
    ),
    na_values = c(8, 9),
    label = "Trust in the health system"
  )
  code_book(tibble::tibble(trust))$values
}
#> # A tibble: 7 × 7
#>   variable code  label      declared_missing     n pct_total pct_valid
#>   <chr>    <chr> <chr>      <lgl>            <int>     <dbl>     <dbl>
#> 1 trust    1     Not at all FALSE                2        20      28.6
#> 2 trust    2     A little   FALSE                3        30      42.9
#> 3 trust    3     Somewhat   FALSE                1        10      14.3
#> 4 trust    4     A lot      FALSE                1        10      14.3
#> 5 trust    8     Don't know TRUE                 1        10      NA  
#> 6 trust    9     Refused    TRUE                 1        10      NA  
#> 7 trust    NA    NA         FALSE                1        10      NA  

if (requireNamespace("openxlsx2", quietly = TRUE)) {
  path <- tempfile(fileext = ".xlsx")
  code_book(sochealth, output = path)
}

# The Typst source of the PDF, which `output = "<path>.pdf"` compiles
# when Quarto is installed.
code_book(sochealth, output = tempfile(fileext = ".typ"))
```
