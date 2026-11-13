# Item battery table

Tabulates a battery of items answered on one common scale, such as a
Likert grid or the "array" question of a survey tool: one row per item
and one column per level of the scale, with the row percentages of each
item, an optional column adding up some levels (`top`), and the base of
each row (`n`). The items are selected with tidyselect syntax.

With `by`, each group gets a block of columns, and each item gets a test
of independence of the item and the group (the chi-squared, or Fisher's
exact test when an expected count is below 5), adjusted for the family
of the items by Holm's method.

Multiple output formats are available via `output`: a printed ASCII
table (`"default"`), a wide or long numeric `data.frame`
(`"data.frame"`, `"long"`), or publication-ready tables (`"tinytable"`,
`"gt"`, `"flextable"`, `"excel"`, `"clipboard"`, `"word"`).

## Usage

``` r
table_battery(
  data,
  select,
  by = NULL,
  levels = NULL,
  top = NULL,
  sort = "",
  drop_na = TRUE,
  show_n = TRUE,
  weights = NULL,
  test = c("auto", "chisq", "fisher", "midp", "n1"),
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

- data:

  A data frame, one row per respondent.

- select:

  The items of the battery, one column per item, in tidyselect syntax or
  as a character vector of column names. Their order is the order of the
  rows, unless `sort` sorts them. The `by` and `weights` columns are not
  part of the selection: a selection helper skips them, and naming one,
  or giving its position, is an error. A name that is not a column of
  `data` is an error (class `spicy_missing_column`), and so is an empty
  selection. The items share one scale; see *Items and levels*.

- by:

  Optional grouping column, unquoted or as a string. Factor levels keep
  their declared order; any other column is sorted, as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
  Rows with a missing `by` are removed from the whole table, and the
  note gives their count ("Missing values removed: sex (2)."); a `by`
  that no observation holds is an error (class `spicy_invalid_data`).

- levels:

  The scale: the levels to show, in the order of the columns. `NULL`
  (the default) reads the scale from the items (see *Items and levels*).
  Given, `levels` is the scale whatever the items hold: a level that no
  item holds shows at 0, and any other answer, observed or declared, is
  left out of the percentages and the base, and the note names it.
  `levels` is the way to keep a "Don't know" answer out of the base, and
  to give numeric or text items a level nobody chose (`levels = 1:5`).
  The entries are written as the table shows the answers: for a labelled
  column, the `"[code] label"` string. An entry that no item declares or
  holds may be a typo: it keeps its column, at 0, with a warning (class
  `spicy_unknown_name`). A level that a factor declares and nobody chose
  does not warn, nor does a number within the values the numeric items
  hold (3 on a 1-5 scale); a number outside them does.

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
  its base: its percentages are on its valid answers, and its `n` counts
  them. If `FALSE`, a last column of missing answers (`"(Missing)"`)
  follows the levels, and the base of each item is every row, its
  missing answers included.

- show_n:

  Logical. If `TRUE` (the default), each row (each group block under
  `by`) ends with `n`, the base of its percentages.

- weights:

  Optional frequency weights, as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md):
  `NULL` (the default), a numeric vector of length `nrow(data)`, or a
  single column of `data`, unquoted or as a string. The counts, the
  percentages, `n` and the tests are weighted. Rows with an `NA` weight
  leave the table, and the note says how many. Fisher's exact test needs
  whole numbers: see *Tests*. For survey weights, the percentages are
  right but the tests are not, which need the design: see
  [`table_battery_svy()`](https://amaltawfik.github.io/spicy/reference/table_battery_svy.md).

- test:

  The test of each item's p-value, as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md):
  `"auto"` (the default) takes Pearson's chi-squared test when every
  expected count of the item's table is at least 5 and Fisher's exact
  test otherwise; `"chisq"` and `"fisher"` take one test for every item;
  `"midp"` takes the Fisher mid-p test for an item whose table is 2x2;
  `"n1"` takes the N-1 chi-squared test for a 2x2 table whose expected
  counts are all at least 1, and Fisher's exact test otherwise. A table
  that is not 2x2 follows `"auto"` under `"midp"` and `"n1"`. The note
  names the test of each item. Ignored, with a warning, when the table
  runs no test.

- p_value:

  Logical or `NULL`. With `by`, `NULL` (the default) and `TRUE` show the
  `p` column of the item tests; `FALSE` removes it from every output,
  the test columns of the long output included. Ignored without `by`,
  with a warning when `TRUE`: the table runs no test without groups to
  compare.

- p_adjust:

  Multiple-comparison adjustment of the `p` column, applied to the
  family of the items tested: `"holm"` (the default), `"none"`, or
  another method of
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html)
  (`"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`, `"BY"`),
  as in
  [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md).
  See *Multiple-comparison adjustment*. Typing it when the table has no
  test to adjust (no `by`, or `p_value = FALSE`) is an error.

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
  the direction of the percentages, the base, the levels left out, the
  weights, the rows removed and the items without data, then the tests,
  and the sentences of `note_add`. A single string replaces it, printed
  as typed after the lead-in ("Note.", unless the string opens with
  one), `note_add` after it, and `FALSE` removes it, as in every table
  function.

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

Depends on `output`:

- `"default"`: a `spicy_battery_table` (a `data.frame` subclass), with
  the rendering metadata as attributes, returned visibly, so a bare call
  prints the ASCII table.

- `"data.frame"`: the displayed table with its numbers, what
  [as.data.frame()](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_battery_table.md)
  of the default object returns: one row per item, with columns
  `variable` (the item's column), `label` (the label the row displays),
  then one column per displayed column: one percentage per level shown,
  `(Missing)` under `drop_na = FALSE`, the summary column of `top`
  (named by its header, `"Strongly agree + Agree"`) and `n`, each
  prefixed under `by` with the group printed above it (`"Female Agree"`,
  `"Female n"`), and `p` with `p_value`.

- `"long"`: one row per item, group and level, with columns `variable`,
  `label`, `group` (with `by`), `level` (the level, or the label of the
  column of missing answers), `.row_role` (`"level"`, or `"missing"` for
  the missing answers), `n` (the count, weighted under `weights`) and
  `pct` (its percentage of the base), and, when the tests ran,
  `test_type` (`"chi_squared"`, `"n1_chi_squared"`, `"fisher_exact"`,
  `"fisher_midp"`, `"fisher_exact_mc"`), `statistic` and `df` (`NA` for
  an exact test), `p.value`, and `p_unadjusted` under an adjustment. The
  summary column is not part of it: it is the sum of the levels of
  `top`.

- `"tinytable"`, `"gt"`, `"flextable"`: the rendered table object.

- `"excel"` / `"word"`: writes to disk and returns the file path
  invisibly.

- `"clipboard"`: copies the table and returns the display `data.frame`
  invisibly.

The default object and the data frames carry the note as printed in
`attr(x, "note")` (its lead-in included, `NULL` under `note = FALSE`).
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
reads the default object: one `"summary"` row per item, token `"pct"`
for each percentage column (with `scale_level`, the level it shows,
`missing` on the column of missing answers, and `top` with `top_levels`
on the summary column), `"n"` and `"p"`.
[`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md)
cites a cell: `inline(x, item, "Agree", "pct")`, with `model = "Female"`
under `by`.

## Items and levels

The items of a battery share one scale. Without `levels`, the scale is
read from the items:

- items that declare their levels, factors and labelled columns with
  value labels (as `"[code] label"`, the conversion of
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) and
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)),
  must declare the same set. An item whose levels differ is an error
  (class `spicy_invalid_input`) that names it with its levels. The same
  set in another order is accepted, and the columns follow the order of
  the first item;

- items that declare none, numeric, character and logical columns, take
  the union of the values they hold: numbers sorted, text in the order
  of first appearance across the items, so a level that one item alone
  holds is a column of every item;

- a mix of the two kinds is an error that names the first item of the
  other kind.

Given, `levels` is the scale, and nothing is refused: a level that no
item holds shows at 0, and every other answer, observed or declared, is
left out of the percentages and the base and named in the note. An item
without any answer shows a dash. Declared missing values of a labelled
column (`na_values`, `na_range`) are missing.

A level that no respondent chose keeps its column, at 0, as a level of
the scale that was offered and not chosen. A dash marks a cell with no
base: an item without a valid answer, in the table or in a group.

The percentages are row percentages: those of an item sum to 100 over
the levels shown (the column of missing answers included under
`drop_na = FALSE`). Its base, `n`, is its valid answers on the levels
shown, or every row under `drop_na = FALSE`; an answer that `levels`
leaves out is out of the base either way. A percentage carries no more
digits than its denominator, so a base of fewer than 100 respondents
takes whole percentages: `percent_digits = 0` prints them.

The table computes no mean or score of the scale: the levels of a rating
scale are ordered, not spaced on an interval.

## Tests

With `by`, each item gets a test of independence of the item and the
group, on its table of the levels shown by the groups, its missing
answers left out whatever `drop_na`. The test is the one `test` chooses,
with the rule of
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md):
under `"auto"`, the Pearson chi-squared test, without continuity
correction, when every expected count is at least 5, and Fisher's exact
test otherwise; when the exact enumeration fails, the p-value is its
Monte Carlo estimate over 2000 tables, which draws from the session's
RNG. A level or a group that holds no answer to the item leaves its
table. An item left with one level has no test: its `p` cell shows a
dash and the note names it. An item with answers in fewer than two
groups has no test either, and its `p` cell is blank. Without `by`, the
table runs no test.

Fisher's exact test and its mid-p version condition on counts. Under
weights that are not whole numbers, `test = "fisher"` and
`test = "midp"` are an error (class `spicy_unsupported`), and an item
that `"auto"` or `"n1"` would send to Fisher's exact test keeps the
chi-squared test, with a warning (class `spicy_fallback`).

## Multiple-comparison adjustment

The items of a battery are read together, as a family of tests, so the
`p` column is adjusted by Holm's method by default, as for the items of
[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md).
Holm's method controls the family-wise error rate whatever the
dependence between the items. The raw p-values stay in the long output
as `p_unadjusted`, and in `col_meta$p$p_unadjusted` of
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md).
The note names the method and the number of items adjusted together.

## See also

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
for one block of rows per variable,
[`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md)
for a "check all that apply" question,
[`table_battery_svy()`](https://amaltawfik.github.io/spicy/reference/table_battery_svy.md)
for a survey design; the article [Item
batteries](https://amaltawfik.github.io/spicy/articles/table-battery.html).

## Examples

``` r
# The four life_sat_* items of sochealth rate satisfaction with four
# domains of life on the same scale, from 1 to 5.
sat <- sochealth[c("sex", "age_group")]
scale_labels <- c(
  "Very dissatisfied", "Dissatisfied", "Neutral",
  "Satisfied", "Very satisfied"
)
for (v in grep("^life_sat_", names(sochealth), value = TRUE)) {
  sat[[v]] <- factor(sochealth[[v]], levels = 1:5, labels = scale_labels)
  attr(sat[[v]], "label") <- attr(sochealth[[v]], "label")
}

# One row per item, one column per level, row percentages and the base.
table_battery(sat, select = starts_with("life_sat_"), labels = TRUE)
#> Item battery
#> 
#>  Variable                                   │ Very dissatisfied  Dissatisfied 
#> ────────────────────────────────────────────┼─────────────────────────────────
#>  Satisfaction with health (1-5)             │        7.4             14.2     
#>  Satisfaction with work (1-5)               │        7.6             16.3     
#>  Satisfaction with relationships (1-5)      │        3.1             10.9     
#>  Satisfaction with standard of living (1-5) │        6.4             17.1     
#> 
#>  Variable                                   │ Neutral  Satisfied 
#> ────────────────────────────────────────────┼────────────────────
#>  Satisfaction with health (1-5)             │  24.0      25.1    
#>  Satisfaction with work (1-5)               │  26.4      30.3    
#>  Satisfaction with relationships (1-5)      │  26.3      29.7    
#>  Satisfaction with standard of living (1-5) │  26.0      31.5    
#> 
#>  Variable                                   │ Very satisfied   n   
#> ────────────────────────────────────────────┼──────────────────────
#>  Satisfaction with health (1-5)             │      29.4       1192 
#>  Satisfaction with work (1-5)               │      19.5       1192 
#>  Satisfaction with relationships (1-5)      │      29.9       1192 
#>  Satisfaction with standard of living (1-5) │      19.0       1192 
#> 
#> Note. Percentages are row percentages. Base: respondents who answered the item.

# The top of the scale, the items sorted on it.
table_battery(
  sat,
  select = starts_with("life_sat_"),
  top = c("Very satisfied", "Satisfied"),
  sort = "top-",
  percent_digits = 0
)
#> Item battery
#> 
#>  Variable               │ Very dissatisfied  Dissatisfied  Neutral  Satisfied 
#> ────────────────────────┼─────────────────────────────────────────────────────
#>  life_sat_relationships │         3               11         26        30     
#>  life_sat_health        │         7               14         24        25     
#>  life_sat_standard      │         6               17         26        31     
#>  life_sat_work          │         8               16         26        30     
#> 
#>  Variable               │ Very satisfied  Very satisfied + Satisfied   n   
#> ────────────────────────┼──────────────────────────────────────────────────
#>  life_sat_relationships │       30                    60              1192 
#>  life_sat_health        │       29                    54              1192 
#>  life_sat_standard      │       19                    51              1192 
#>  life_sat_work          │       19                    50              1192 
#> 
#> Note. Percentages are row percentages. Base: respondents who answered the item.

# By sex: the top of the scale in each group, and a test per item.
table_battery(
  sat,
  select = starts_with("life_sat_"),
  by = sex,
  top = c("Very satisfied", "Satisfied")
)
#> Item battery by sex
#> 
#>                                       Female              
#>                           ─────────────────────────────── 
#>  Variable               │ Very satisfied + Satisfied   n  
#> ────────────────────────┼─────────────────────────────────
#>  life_sat_health        │            52.9             616 
#>  life_sat_work          │            49.1             615 
#>  life_sat_relationships │            59.8             615 
#>  life_sat_standard      │            50.9             615 
#> 
#>                                        Male                     
#>                           ───────────────────────────────       
#>  Variable               │ Very satisfied + Satisfied   n    p   
#> ────────────────────────┼───────────────────────────────────────
#>  life_sat_health        │            56.1             576  .798 
#>  life_sat_work          │            50.4             577  .052 
#>  life_sat_relationships │            59.4             577  .798 
#>  life_sat_standard      │            50.1             577  .397 
#> 
#> Note. Percentages are row percentages. Base: respondents who answered the item. Item p-values: Pearson chi-squared test of each item by group. Adjusted for 4 tests (Holm method).

# The neutral level left out of the base, missing answers shown.
table_battery(
  sat,
  select = starts_with("life_sat_"),
  levels = scale_labels[-3],
  drop_na = FALSE
)
#> Item battery
#> 
#>  Variable               │ Very dissatisfied  Dissatisfied  Satisfied 
#> ────────────────────────┼────────────────────────────────────────────
#>  life_sat_health        │        9.6             18.5        32.7    
#>  life_sat_work          │       10.2             21.9        40.8    
#>  life_sat_relationships │        4.2             14.7        40.0    
#>  life_sat_standard      │        8.5             22.9        42.1    
#> 
#>  Variable               │ Very satisfied  (Missing)   n  
#> ────────────────────────┼────────────────────────────────
#>  life_sat_health        │      38.3          0.9     914 
#>  life_sat_work          │      26.2          0.9     885 
#>  life_sat_relationships │      40.3          0.9     886 
#>  life_sat_standard      │      25.5          0.9     890 
#> 
#> Note. Percentages are row percentages. Base: all respondents but those whose answer is left out, including those who did not answer the item. Answers left out of the percentages and the base: Neutral.

# The integer items as they are: `levels` gives the scale.
table_battery(sochealth, select = starts_with("life_sat_"), levels = 1:5)
#> Item battery
#> 
#>  Variable                 │   1      2       3       4       5       n    
#> ──────────────────────────┼───────────────────────────────────────────────
#>  life_sat_health          │  7.4    14.2    24.0    25.1    29.4    1192  
#>  life_sat_work            │  7.6    16.3    26.4    30.3    19.5    1192  
#>  life_sat_relationships   │  3.1    10.9    26.3    29.7    29.9    1192  
#>  life_sat_standard        │  6.4    17.1    26.0    31.5    19.0    1192  
#> 
#> Note. Percentages are row percentages. Base: respondents who answered the item.

# Long data frame.
head(table_battery(sat, select = starts_with("life_sat_"), output = "long"))
#>          variable           label             level .row_role   n       pct
#> 1 life_sat_health life_sat_health Very dissatisfied     level  88  7.382550
#> 2 life_sat_health life_sat_health      Dissatisfied     level 169 14.177852
#> 3 life_sat_health life_sat_health           Neutral     level 286 23.993289
#> 4 life_sat_health life_sat_health         Satisfied     level 299 25.083893
#> 5 life_sat_health life_sat_health    Very satisfied     level 350 29.362416
#> 6   life_sat_work   life_sat_work Very dissatisfied     level  90  7.550336

# \donttest{
if (requireNamespace("tinytable", quietly = TRUE)) {
  tt <- table_battery(
    sat,
    select = starts_with("life_sat_"),
    by = sex,
    labels = TRUE,
    output = "tinytable"
  )
}
# }
```
