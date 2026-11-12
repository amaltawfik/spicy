# Item battery table from a survey design

The design twin of
[`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md):
a battery of items answered on one common scale, one row per item and
one column per level, with row percentages estimated from a
[`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
or
[`survey::as.svrepdesign()`](https://rdrr.io/pkg/survey/man/as.svrepdesign.html)
object instead of a data frame.

Every statistic is survey's.
[`survey::svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
estimates the percentages of each item, and, with `by`,
[`survey::svychisq()`](https://rdrr.io/pkg/survey/man/svychisq.html)
tests each item against the group, Rao-Scott corrected by default, the
p-values adjusted for the family of the items by Holm's method.

The rows, the columns, `levels`, `top`, `sort` and the outputs are those
of
[`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md).

## Usage

``` r
table_battery_svy(
  design,
  select,
  by = NULL,
  levels = NULL,
  top = NULL,
  sort = "",
  drop_na = TRUE,
  show_n = TRUE,
  chisq_statistic = c("F", "Chisq", "Wald", "adjWald", "saddlepoint"),
  p_value = NULL,
  p_adjust = "holm",
  labels = getOption("spicy.var_labels", FALSE),
  note_labels = NULL,
  percent_digits = 1,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  title = NULL,
  note = NULL,
  note_add = NULL,
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  style = NULL
)
```

## Arguments

- design:

  A survey design:
  [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
  or
  [`survey::as.svrepdesign()`](https://rdrr.io/pkg/survey/man/as.svrepdesign.html).

- select:

  The items of the battery, one column of the design per item, in
  tidyselect syntax or as a character vector of names. Their order is
  the order of the rows, unless `sort` sorts them. The `by` column and
  the design's own columns (its weights, replicate weights, clusters and
  finite population corrections) are not part of the selection: a
  selection helper skips them, and naming one is an error. The items
  share one scale, read as in
  [`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md).

- by:

  Optional grouping column, unquoted or as a string. Factor levels keep
  their declared order; any other column is sorted, as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
  Rows with a missing `by` are removed from the whole table, and the
  note gives their count ("Missing values removed: sex (2)."); a `by`
  that no observation holds is an error (class `spicy_invalid_data`).

- levels:

  The scale: the levels to show, in the order of the columns, as in
  [`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md).
  `NULL` (the default) reads the scale from the items, under the rules
  of the section *Items and levels* of
  [`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md).
  Given, `levels` is the scale: a level that no item holds shows at 0,
  and any other answer is left out of the domain, and the note names it.

- top:

  The levels the summary column adds up, for example
  `top = c("Strongly agree", "Agree")`. The column follows the levels;
  its header is the list of the levels joined by `" + "`, in the order
  of `top`, and its value is the sum of their percentages. Each entry
  must be one of the levels shown (see `levels`); another value is an
  error. Without `by`, a `top` of one level would repeat the column of
  that level, and is an error. `NULL` (the default) adds no column.

- sort:

  The order of the rows, in the suffix grammar of
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) and
  [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md),
  `+` for increasing and `-` for decreasing order:

  - `""` (the default): the order of `select`;

  - `"+"` / `"-"`: by the summary column of `top`, or, without `top`, by
    the first level shown;

  - `"top+"` / `"top-"`: by the summary column (needs `top`);

  - `"name+"` / `"name-"`: by the label of the item, in the C locale,
    the same on every platform (capitals before lower case);

  - `"<level>+"` / `"<level>-"`: by the percentage of a level shown, for
    example `sort = "Strongly agree-"`.

  With `by`, the percentages are those of all the groups together. Items
  with the same value keep the order of `select`, and an item without
  data sorts last. The keywords are read first: `"name+"` sorts on the
  labels even when a level is called "name", and `"top+"` on the summary
  column even when a level is called "top". A level or a keyword without
  its direction (`"Agree"`, `"top"`) has two readings, and is an error
  that gives both. Any other value is an error.

- drop_na:

  Logical. If `TRUE` (the default), the missing answers to an item leave
  its domain: its percentages are estimated on its valid answers, and
  its `n` counts them. If `FALSE`, a last column of missing answers
  (`"(Missing)"`) follows the levels, and the domain of each item takes
  every row of non-zero weight, its missing answers included.

- show_n:

  Logical. If `TRUE` (the default), each row (each group block under
  `by`) ends with `n`, the observed (unweighted) count of the
  respondents its percentages are estimated on.

- chisq_statistic:

  Statistic for
  [`survey::svychisq()`](https://rdrr.io/pkg/survey/man/svychisq.html):
  `"F"` (default), `"Chisq"`, `"Wald"`, `"adjWald"` or `"saddlepoint"`.
  `"saddlepoint"` is refused on a replicate-weights design: survey
  computes its p-value without the denominator degrees of freedom there,
  so it comes out too small.

- p_value:

  Logical or `NULL`. With `by`, `NULL` (the default) and `TRUE` show the
  `p` column of the item tests; `FALSE` removes it from every output.
  Ignored without `by`, with a warning when `TRUE`.

- p_adjust:

  Multiple-comparison adjustment of the `p` column, applied to the
  family of the items tested: `"holm"` (the default), `"none"`, or
  another method of
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html)
  (`"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`, `"BY"`).
  See the section *Multiple-comparison adjustment* of
  [`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md).
  Typing it when the table has no test to adjust (no `by`, or
  `p_value = FALSE`) is an error.

- labels:

  How each variable is named in the table: in the rows, in the title,
  and in the notes. Four forms:

  - `labels = FALSE` shows the column name.

  - `labels = TRUE` shows the column's `label` attribute (as set by
    `haven`, `labelled`, or
    [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md))
    when it is present, not `NA`, and not empty, and the column name
    otherwise.

  - `labels = c(bmi = "Body mass index")`, a named character vector,
    gives the columns it names these labels. Every other column follows
    `options(spicy.var_labels)`: its name when the option is unset or
    `FALSE`, its `label` attribute when it is `TRUE`.

  - `labels = var_labels(bmi = "BMI (kg/m2)")` gives the columns it
    names these labels, and every other column its `label` attribute,
    whatever the option says.
    [`var_labels()`](https://amaltawfik.github.io/spicy/reference/var_labels.md)
    alone is `labels = TRUE`.

  A name that matches no column of the table warns (class
  `spicy_unknown_name`) and is left out, so one vector can still serve
  every table of a project. The default reads
  `options(spicy.var_labels)`, and a `TRUE` or `FALSE` typed in the call
  wins over it. Any other value, `NULL` included, is an error. A
  variable label documents the data file; a published table carries the
  wording you set here. `note_labels` gives a variable a shorter label
  in the title and the notes.

- note_labels:

  The label a variable takes where the table names it in a sentence, in
  the title and in the notes, in place of the one `labels` gives it: a
  named character vector, `note_labels = c(sex = "sex at birth")`, or a
  [`var_labels()`](https://amaltawfik.github.io/spicy/reference/var_labels.md)
  object, which reads as one. `labels` can then name a row with a whole
  question while the note says "Missing values removed: sex at birth
  (2).". The rows, the column headers and the spanners keep the labels
  of `labels`, and so do the title and the notes for a variable
  `note_labels` does not name. A name that matches no column warns and
  is left out, as in `labels`, and `NULL` (the default) renames nothing.
  A note label that another variable of the table shows is an error
  (class `spicy_label_collision`): choose another one, or relabel the
  other variable.

- percent_digits:

  Number of digits for percentages in report outputs. Defaults to `1`. A
  whole number: a fraction is an error, as for every `*_digits`
  argument.

- p_digits:

  Integer \>= 1. Number of decimal places used to render *p*-values in
  the `p` column (default: `3`, the APA Publication Manual standard).
  Both the displayed precision and the small-*p* threshold derive from
  this argument: `p_digits = 3` prints `.045` and `<.001`;
  `p_digits = 4` prints `.0451` and `<.0001`. Leading zeros are always
  stripped, following APA convention.

- decimal_mark:

  The decimal mark, a single character: `"."` (the default) or `","` in
  most styles; the Lancet style of
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  uses the middle dot.

- align:

  Horizontal alignment of the numeric columns in the console and in the
  rendered outputs: `"decimal"` (the default), `"center"`, or `"right"`,
  as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).

- title:

  The table title. `NULL` (the default) gives "Item battery", followed
  by "by" and the `by` variable under the `labels` contract (or its
  label in `note_labels`); a single string replaces it, and `FALSE`
  removes it, as in
  [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md).

- note:

  The table note. `NULL` (the default) builds it, opening with "Note.":
  the design, the direction of the percentages, the base, the levels
  left out, the rows removed and the items without data, then the tests
  and the sentences of `note_add`. A single string replaces it, and
  `FALSE` removes it, as in
  [`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md).

- note_add:

  Sentences to add to the note: a character vector of one or more
  strings, each printed as typed (no period is added) and joined to the
  note by a space. They close its paragraph, after the definitions of
  the abbreviations and before the legend of the stars, in every output
  and in `attr(x, "note")`, and the sentences the table writes stay.
  With a string in `note`, they follow it, so a wrapper can add a house
  sentence to every table while one table replaces its note. An `NA` or
  an empty string is an error, and so is `note_add` with `note = FALSE`,
  which leaves no note to add to.

- output:

  Output format. One of:

  - `"default"` (an ASCII table object, printed when the call is bare)

  - `"data.frame"` (the displayed table, numeric)

  - `"long"` (a long numeric `data.frame`)

  - `"tinytable"` (requires `tinytable`)

  - `"gt"` (requires `gt`)

  - `"flextable"` (requires `flextable`)

  - `"excel"` (requires `openxlsx2`)

  - `"clipboard"` (requires `clipr`)

  - `"word"` (requires `flextable` and `officer`)

- excel_path:

  Path for `output = "excel"`, ending in `".xlsx"`, in a directory that
  exists. Defaults to `NULL`.

- excel_sheet:

  Sheet name for Excel export. `NULL` (the default) uses
  `"Item battery"`.

- clipboard_delim:

  Delimiter for clipboard text export. Defaults to `"\t"`. A cell
  holding the delimiter itself, a double quote or a line break is quoted
  RFC 4180-style, so the grid survives whatever delimiter you choose.

- word_path:

  File path for `output = "word"`. Defaults to `NULL`.

- style:

  A journal style: a theme name (`"jama"`, `"nejm"`, `"lancet"`,
  `"annals"`, `"apa"`, `"aer"`), a
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  object, or `NULL` (the default). A style only changes DEFAULTS – any
  argument you pass explicitly wins over it. Set
  `options(spicy.style = )` for document-wide scope. A theme covers
  numeric formatting, the word that opens the note and the abbreviations
  the note defines, not full editorial conformity;
  [`?spicy_style`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  lists the exact rules each one encodes and the official document they
  come from. An unknown name is an error.

## Value

Depends on `output`, as in
[`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md):

- `"default"`: a `spicy_battery_svy_table` (a `data.frame` subclass),
  printed when the call is bare.

- `"data.frame"`: the displayed table with its numbers, the shape of
  [`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md):
  `variable`, `label`, one percentage per level shown, `(Missing)` under
  `drop_na = FALSE`, the summary column of `top`, `n`, each prefixed
  with the group under `by`, and `p`.

- `"long"`: one row per item, group and level, with `variable`, `label`,
  `group` (with `by`), `level`, `.row_role`, `n` (the observed count)
  and `pct` (the estimated percentage), and, when the tests ran,
  `test_type` (the statistic of
  [`survey::svychisq()`](https://rdrr.io/pkg/survey/man/svychisq.html)),
  `p.value` and `p_unadjusted` under an adjustment.

- `"tinytable"`, `"gt"`, `"flextable"`: the rendered table object.

- `"excel"` / `"word"`: writes to disk and returns the file path
  invisibly.

- `"clipboard"`: copies the table and returns the display `data.frame`
  invisibly.

[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
and [`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md)
read the default object as they read that of
[`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md):
`inline(x, item, "Yes", "pct")`, with `model = "<group>"` under `by`.

## Estimation

The percentage of a level is
[`survey::svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
of the item on its domain: within the group under `by`, the rows of
non-zero weight whose answer is a level shown. The domain is cut from
the design with `[`, which keeps its strata, clusters and finite
population corrections. Under `drop_na = FALSE`, the missing answers are
a level of their own and stay in the domain. An answer that `levels`
leaves out is outside the domain either way. An item that offers one
level is 100% by arithmetic: survey does not estimate a variable with
one level.

A cell that survey cannot estimate, most often because a stratum holds a
single primary sampling unit under its default
`options(survey.lonely.psu = "fail")`, shows a dash: the function warns
once, quoting survey's messages (class `spicy_svy_estimation_failed`),
and the note says what the dash means. A warning of survey's own, such
as the one on a stratum with a single primary sampling unit in a domain
under `options(survey.adjust.domain.lonely = TRUE)`, is said once per
table, with the number of times survey raised it.

`n` is the observed count of the domain, the respondents and not the
estimated population, as in
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md).
A row with a missing `by` leaves the table, and the note gives their
count.

## Tests

With `by`, each item is tested against the group by
[`survey::svychisq()`](https://rdrr.io/pkg/survey/man/svychisq.html)
with `chisq_statistic`, on its answers on the levels shown, the missing
answers left out whatever `drop_na`; the levels and the groups without
an answer leave the test, as in
[`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md).
The default `"F"` is the Pearson chi-squared with the Rao-Scott
second-order correction, referred to the design degrees of freedom. The
p-values are adjusted for the family of the items, by Holm's method by
default (`p_adjust`). An item left with one level is not tested: its `p`
cell shows a dash, and the note names it.

A design whose weights go negative, as linear calibration can make them,
keeps its percentages, and its tests are withheld, with a warning (class
`spicy_negative_weights_no_test`), as in
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md).

## What is absent

No interval or design effect per cell: a battery of five levels in three
groups would carry thirty of each, and
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
gives them item by item. No column percentages, and no mean of the
scale.

## See also

[`table_battery()`](https://amaltawfik.github.io/spicy/reference/table_battery.md)
for the data-frame sibling,
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
for one block of rows per variable.

## Examples

``` r
data(api, package = "survey")
dstrat <- survey::svydesign(
  id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc
)
# Four yes-no items of the schools, one row each.
table_battery_svy(dstrat, select = c(awards, sch.wide, comp.imp, both))
#> Item battery
#> 
#>  Variable   │   No     Yes      n   
#> ────────────┼───────────────────────
#>  awards     │  36.1    63.9    200  
#>  sch.wide   │  17.2    82.8    200  
#>  comp.imp   │  34.4    65.6    200  
#>  both       │  36.1    63.9    200  
#> 
#> Note. Design: stratified (stype), with finite population correction; 197 degrees of freedom. Percentages are row percentages estimated under the design. Base: respondents who answered the item. n = observed (unweighted) count.

# By school type: the estimated percentages of each type and a
# design-based test per item.
table_battery_svy(
  dstrat,
  select = c(awards, sch.wide, comp.imp, both),
  by = stype
)
#> Item battery by stype
#> 
#>  Variable │ E No  E Yes  E n  H No  H Yes  H n  M No  M Yes  M n    p   
#> ──────────┼─────────────────────────────────────────────────────────────
#>  awards   │ 27.0  73.0   100  68.0  32.0   50   52.0  48.0   50   <.001 
#>  sch.wide │  9.0  91.0   100  48.0  52.0   50   30.0  70.0   50   <.001 
#>  comp.imp │ 25.0  75.0   100  66.0  34.0   50   52.0  48.0   50   <.001 
#>  both     │ 27.0  73.0   100  68.0  32.0   50   52.0  48.0   50   <.001 
#> 
#> Note. Design: stratified (stype), with finite population correction; 197 degrees of freedom. Percentages are row percentages estimated under the design. Base: respondents who answered the item. Item p-values, each item by group: design-based Pearson chi-squared (Rao-Scott second-order correction). Adjusted for 4 tests (Holm method). n = observed (unweighted) count.

# Long data frame.
head(table_battery_svy(
  dstrat,
  select = c(awards, sch.wide),
  by = stype,
  output = "long"
))
#>   variable  label group level .row_role  n pct test_type      p.value
#> 1   awards awards     E    No     level 27  27         F 2.082109e-06
#> 2   awards awards     E   Yes     level 73  73         F 2.082109e-06
#> 3   awards awards     H    No     level 34  68         F 2.082109e-06
#> 4   awards awards     H   Yes     level 16  32         F 2.082109e-06
#> 5   awards awards     M    No     level 26  52         F 2.082109e-06
#> 6   awards awards     M   Yes     level 24  48         F 2.082109e-06
#>   p_unadjusted
#> 1 2.082109e-06
#> 2 2.082109e-06
#> 3 2.082109e-06
#> 4 2.082109e-06
#> 5 2.082109e-06
#> 6 2.082109e-06
```
