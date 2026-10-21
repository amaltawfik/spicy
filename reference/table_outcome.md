# Describe one continuous outcome across several groupings

Summarizes *one* continuous outcome across the levels of *several*
categorical variables, one block of rows per variable. It is the inverse
layout of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
which puts several outcomes in rows and one grouping in columns.

Each block reports the outcome's statistics level by level, plus its own
group comparison on the block's header row, and an `Overall` row gives
the marginal summary of the whole analytic sample.

## Usage

``` r
table_outcome(
  data,
  outcome,
  select,
  labels = getOption("spicy.var_labels", FALSE),
  overall = TRUE,
  drop_na = FALSE,
  weights = NULL,
  rescale = getOption("spicy.rescale", FALSE),
  test = c("welch", "student", "nonparametric"),
  p_value = NULL,
  p_adjust = "none",
  statistic = FALSE,
  show_n = TRUE,
  show_columns = NULL,
  effect_size = c("none", "auto", "hedges_g", "eta2", "r_rb", "epsilon2"),
  effect_size_ci = FALSE,
  ci = TRUE,
  ci_level = 0.95,
  digits = 2,
  effect_size_digits = 2,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  indent_text = "  ",
  indent_text_excel_clipboard = strrep(" ", 6),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  user_na = TRUE,
  style = NULL,
  by
)
```

## Arguments

- data:

  A data frame.

- outcome:

  The continuous outcome, unquoted or as a string. Exactly one column.

- select:

  The grouping characteristics to describe the outcome across, as a
  tidyselect expression or a character vector of column names. One block
  of rows per variable, in the order given, and in each block one row
  per level: the levels of a factor in their order, any other variable
  sorted, as the groups of `by` are in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  (text in the C locale, `FALSE` before `TRUE`, numbers and codes
  ascending), whatever the order of the rows. As everywhere in the
  family, `select` is what structures the rows. The outcome and the
  `weights` column are not part of the selection: a selection helper
  skips them, and naming one in `select`, or giving its position, is an
  error. A name that is not a column of `data` is an error (class
  `spicy_missing_column`), and so is an empty selection.

- labels:

  How each variable is named in the table: in the rows, in the title,
  and in the notes. The outcome follows the same rule as the `select`
  variables. Four forms:

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

  Names that match no column are ignored, so one vector can serve every
  table of a project. The default reads `options(spicy.var_labels)`, and
  a `TRUE` or `FALSE` typed in the call wins over it. Any other value,
  `NULL` included, is an error. A variable label documents the data
  file; a published table carries the wording you set here.

- overall:

  Show the marginal `Overall` row (default `TRUE`).

- drop_na:

  Drop rows with a missing `select` value from that block. The rule of
  the family: the categories a table counts keep their missing values as
  a row by default, as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md);
  the values it computes on lose them, counted in the note, as in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).
  The blocks of this table are categories, so `FALSE` (the default)
  shows the missing values of a `select` variable as a `(Missing)`
  level, excluded from the comparison, and a level displayed with that
  label is an error, as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md);
  the missing values of the outcome are always removed and counted in
  the note. Under `drop_na = TRUE`, a grouping that no observation holds
  is an error (class `spicy_invalid_data`).

- weights:

  Optional case weights: an unquoted column name, a character column
  name, or a numeric vector of length `nrow(data)`. Weights must be
  non-negative and finite; rows with `NA` or zero weight leave every
  statistic (including `Min` / `Max`) and `NA` weights are disclosed in
  the table note. The weighted formulas follow the frequency-expansion
  convention – see the **Weights** section. The weighted table names its
  weights in the note ("Statistics weighted by ..."). Group tests and
  effect sizes are not computed under weights: use
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  for weighted comparisons.

- rescale:

  Logical. If `TRUE`, weights are first normalized so that they sum to
  the number of observations used for each variable – the same `rescale`
  grammar as
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
  This is the sampling-weights reading: results become invariant to the
  scale of the weights, and the weighted SD then equals Stata's
  `[aweight]` /
  [`survey::svyvar()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
  value exactly. `FALSE` uses the weights as given (frequency reading).
  The default reads `options(spicy.rescale)`, `FALSE` when it is unset.

- test:

  Group comparison for every block: `"welch"` (default), `"student"` or
  `"nonparametric"`. An outcome with no variance in a block has no test
  there, whichever is chosen: the block's test cells are left blank,
  with a warning (class `spicy_undefined_stat`).

- p_value:

  Show the p-value column: `TRUE`, `FALSE`, or `NULL` (the default),
  which shows it, every block being a comparison.

- p_adjust:

  Multiple-comparison adjustment of the p-value column, applied to the
  family of every test in the table (one p-value per block). One of
  `"none"` (default), `"holm"`, `"hochberg"`, `"hommel"`,
  `"bonferroni"`, `"BH"` / `"fdr"`, or `"BY"`, as in
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
  delegated to
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html). Needs
  the p-value column. See *Multiple-comparison adjustment*.

- statistic:

  Logical. If `TRUE`, the test statistic of each block is shown in an
  additional column, with or without the *p*-value: `p_value` and
  `statistic` are independent. Defaults to `FALSE`.

- show_n:

  Logical. If `TRUE`, includes an unweighted `n` column in the printed
  ASCII table and in every rendered output (`tinytable`, `gt`,
  `flextable`, `word`, `excel`, `clipboard`). Set to `FALSE` to drop the
  `n` column structurally from those outputs (no empty placeholder, no
  spanner). The `n` column is always present in `output = "long"` for
  downstream programmatic access. Defaults to `TRUE`. Ignored (with a
  warning) when `show_columns` is supplied.

- show_columns:

  Statistics to display, as a character vector of the tokens of
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  (see *Choosing the statistics*). `NULL` (the default) shows the mean,
  SD, min, max, the mean CI (see `ci`) and `n` (see `show_n`).

- effect_size, effect_size_ci:

  Effect size per block and its confidence interval, as in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  (a logical `effect_size` is an error there and here).

- ci:

  Logical. If `TRUE`, includes the mean confidence interval columns
  (`<level>% CI LL` / `<level>% CI UL`) and their spanner in the printed
  ASCII table and in every rendered output (`tinytable`, `gt`,
  `flextable`, `word`, `excel`, `clipboard`). Set to `FALSE` to drop
  both columns and the CI spanner structurally from those outputs (no
  empty placeholders, no border lines under an empty header). The CI
  bounds are always present as `ci_lower` / `ci_upper` in
  `output = "long"` for downstream programmatic access. Defaults to
  `TRUE`. The CI level is taken from `ci_level`. Ignored (with a
  warning) when `show_columns` is supplied.

- ci_level:

  Confidence level for the mean confidence interval (default: `0.95`): a
  number strictly between 0 and 1.

- digits:

  Number of decimal places for descriptive values and test statistics
  (default: `2`). A whole number: a fraction is an error, as for every
  `*_digits` argument.

- effect_size_digits:

  Number of decimal places for effect-size values in formatted displays
  (default: `2`).

- p_digits:

  Integer \>= 1. Number of decimal places used to render *p*-values in
  the `p` column (default: `3`, the APA Publication Manual standard).
  Both the displayed precision and the small-*p* threshold derive from
  this argument: `p_digits = 3` prints `.045` and `<.001`;
  `p_digits = 4` prints `.0451` and `<.0001`; `p_digits = 2` prints
  `.05` and `<.01`. Useful for genomics / GWAS contexts with very small
  *p*-values, or for journals using a coarser convention. Leading zeros
  are always stripped, following APA convention.

- decimal_mark:

  The decimal mark, a single character: `"."` (the default) or `","` in
  most styles; the Lancet style of
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  uses the middle dot.

- align:

  Horizontal alignment of numeric columns in the printed ASCII table and
  in the `tinytable`, `gt`, `flextable`, `word`, `excel`, and
  `clipboard` outputs. The first column (`Variable`) and `Group` (when
  present) are always left-aligned. One of:

  - `"decimal"` (default): align numeric columns on the decimal mark,
    the standard scientific-publication convention used by SPSS, SAS,
    and LaTeX `siunitx`. Numeric cells are pre-padded with figure-spaces
    (U+2007, digit-width) so every string in a column has the same width
    with the decimal mark at the same internal position; centering those
    uniform-width strings then stacks the decimal points vertically. The
    same pad-then-center strategy is applied on every rendering engine
    (`gt`, `tinytable`, `flextable`, `word`, ASCII print) for a
    homogeneous rendering, matching
    [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
    and
    [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md).
    The `clipboard` output is delimited text meant to be parsed rather
    than read at a fixed width, so its cells travel unpadded (a padded
    number pastes as text next to an unpadded number).

  - `"center"`: center-align all numeric columns.

  - `"right"`: right-align all numeric columns.

  `"center"` and `"right"` reach the `excel` output too. `"decimal"`
  does not: Excel cells are written unpadded, because cell-string
  padding does not align decimals under a proportional font, so the
  workbook keeps the engine's own convention instead – counts and the
  *p*-value right-aligned, the other numeric columns centered. Same
  default and same three values as
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  whose `excel` output still uses that convention at every `align`.

- output:

  One of `"default"`, `"data.frame"`, `"long"`, or a rendering engine:
  `"tinytable"`, `"gt"`, `"flextable"`, `"excel"`, `"clipboard"`,
  `"word"`. `"data.frame"` is the displayed table with its numbers,
  `"long"` the compute frame with every statistic, as in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md);
  see *Value*.

- indent_text:

  Prefix used for modality labels in report table building. Defaults to
  `" "` (two spaces).

- indent_text_excel_clipboard:

  Stronger indentation used in Excel and clipboard exports. Defaults to
  six non-breaking spaces.

- excel_path:

  File path for `output = "excel"`, ending in `".xlsx"`, in a directory
  that exists.

- excel_sheet:

  Sheet name for `output = "excel"`. `NULL` (the default) uses
  `"Outcome"`.

- clipboard_delim:

  Delimiter for `output = "clipboard"` (default: `"\t"`). A cell holding
  the delimiter itself, a double quote or a line break is quoted RFC
  4180-style, so the grid survives whatever delimiter you choose.

- word_path:

  File path for `output = "word"`, ending in `".docx"`, in a directory
  that exists.

- user_na:

  Logical. If `TRUE` (the default), declared missing values are treated
  as missing: those of the outcome leave its statistics and are
  disclosed in the table note (`Declared missing values removed: ...`),
  those of a `select` variable join its `(Missing)` level under
  `drop_na = FALSE`. If `FALSE`, the declared codes are ordinary values.
  See the "Declared missing values" section of
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).

- style:

  A journal style: a theme name (`"jama"`, `"nejm"`, `"lancet"`,
  `"annals"`, `"apa"`, `"aer"`), a
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  object, or `NULL` (the default). A style only changes DEFAULTS – any
  argument you pass explicitly wins over it. Set
  `options(spicy.style = )` for document-wide scope. A theme covers
  numeric formatting conformity only, not full editorial conformity;
  [`?spicy_style`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  lists the exact rules each one encodes and the official document they
  come from. An unknown name is an error.

- by:

  Defunct. The grouping characteristics are selected with `select` since
  spicy 0.13.0; supplying `by` is an error.

## Value

Depends on `output`:

- `"default"`: a `spicy_outcome_table` (a `data.frame` subclass, with
  the parent class `"spicy_table"`), printed when the call is bare; its
  columns are the table's compute frame, and the display frame and the
  typed view are attached.

- `"data.frame"`: the displayed table with its numbers, what
  [as.data.frame()](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_outcome_table.md)
  of the default object returns: one row per printed row, the key
  columns `variable` (the outcome on the `Overall` row, the grouping
  variable elsewhere), `label`, `level` and `.row_role` (`"summary"` for
  the `Overall` row, `"factor_header"`, `"level"`, or `"missing"`), then
  the printed columns under their names (`"M"`, `"SD"`, ..., `"n"`,
  `"p"`), a cell that prints several numbers keeping the others beside
  it as in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).

- `"long"`: the compute frame, one row per printed row with every
  statistic the table computes, whatever it displays: `variable`,
  `label`, `level`, `.row_role`, `mean`, `sd`, `min`, `max`, `ci_lower`,
  `ci_upper`, `median`, `q1`, `q3`, `iqr`, `med_ci_lower`,
  `med_ci_upper`, `n`, `weighted_n`, the test of each block on its
  header row (`test_type`, `statistic`, `df`, `df.residual`, `p.value`,
  and `p_unadjusted` under `p_adjust`) and its effect size (`es_type`,
  `es_value`, `es_ci_lower`, `es_ci_upper`, `NA` when none was
  requested).

- `"tinytable"`, `"gt"`, `"flextable"`: the rendered table.

- `"excel"` / `"word"`: writes the file and returns its path invisibly.

- `"clipboard"`: copies the table and returns the display `data.frame`
  invisibly.

The default object and the two data frames carry the whole note as
printed, "Note." included, in `attr(x, "note")` (`NULL` when there is
nothing to say).

## Which shape do I need?

Several continuous variables across *one* grouping is
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
(`select = `, `by = `). One continuous variable across one or several
groupings is this function. A single `select` is legitimate here – it is
the natural way in when you know more groupings are coming – but with
several outcomes and one grouping, the sibling is the table you want.

## Choosing the statistics

`show_columns` takes the same tokens as
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
with the same meanings; see the `show_columns` section of
[`?table_continuous`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
for the vocabulary. Only the character-vector form is accepted here:
there is one outcome, so a per-variable list would name nothing.

## Weights

`weights` applies the frequency-expansion convention of the family: all
weights 1 reproduces the unweighted table, and integer weights reproduce
the STATISTICS of the data duplicated that many times – `n` stays the
raw count of rows that carried the weights. Rows with a missing or zero
weight leave the analytic sample; the note counts the missing ones.

`rescale` is the switch between the two readings of a weight: the
frequency reading above, and the sampling-weight reading, where the
weights are normalized to sum to the sample size. See the Weights
section of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
for the choice in full.

`rescale = TRUE` normalizes the weights over the outcome's whole
surviving sample, once, never per level – a per-level rescale would
destroy the relative weights across levels, which is the entire
information a sampling weight carries into this table. The means are
unchanged by it; the standard deviations move, because their denominator
is `sum(w) - 1`.

A weighted table refuses the group comparison. The estimates and their
interval have no weighted version here, and a p-value or an effect size
silently computed unweighted beside weighted descriptives is the one
thing that must not happen: set `p_value = FALSE` (and
`statistic = FALSE`, `effect_size = "none"`), or use
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
for a weighted comparison. The order-statistic median interval is
refused for the same reason.

## Blocks and the group comparison

Every block is a separate one-way comparison of the outcome across the
levels of that variable. Nothing in this table adjusts one block for
another, and the table note says so. Read the blocks as a set of
bivariate descriptions, not as a model.

Each block chooses its test independently: with two observed levels
`test = "welch"` is the Welch t-test, with three or more it is the Welch
one-way ANOVA, and `test = "nonparametric"` is the Wilcoxon rank-sum or
the Kruskal-Wallis test on the same rule. A block with fewer than two
observed levels, or with a level holding a single observation, is not
tested; its statistics stay empty and the other blocks are unaffected.

## The `Overall` row

`overall = TRUE` puts the marginal summary of the whole analytic sample
on the first row. Under the default `drop_na = FALSE` the levels of
every block partition that sample – the `(Missing)` display level
included – so each block's counts add up to the `Overall` count exactly,
which is what makes it a usable denominator.

The row reads **Overall**, not *Total*, and the distinction is
deliberate. *Total* is the word of a COUNT margin: the column of
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
where frequencies add up. This row is the whole analytic sample, where a
mean is recomputed over every observation and nothing is added. A mean
is not a total.

A `select` variable displayed as `"Overall"` is an error (class
`spicy_label_collision`): choose another label in `labels`, or give the
row another label with
`options(spicy.labels = list(row_overall = "Whole sample"))`.

## Choosing the `select` columns

The canonical form is `select = where(is.factor)`, or an explicit
enumeration. Negation (`select = -c(x, y)`) is not recommended: it
sweeps in every remaining column, and a numeric one becomes a block with
one LEVEL per distinct value, in ascending order. A variable producing
more than 20 levels raises a warning for that reason – an arbitrary
threshold, but a sixty-row block where a reader expects a handful of
categories is not a table.

A `haven_labelled` column used as `select` shows its numeric CODES, not
its value labels, as it does in
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).
Convert it first
([`haven::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html))
to get the labels in the stub.

## The table note

One note sits under the table and states what left the analytic sample,
which group comparison ran in each block, what the displayed columns
mean, and how the blocks and the `Overall` row are to be read. The
rendering engines carry the same sentence as a table note. On the
`"tinytable"` route it is set one size down; `options(spicy.note_style)`
governs that (see
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)).

## Multiple-comparison adjustment

The p-values of a descriptive table describe: they test no prespecified
hypothesis. STROBE (explanation of item 14) and the SAMPL guidelines
advise against significance tests in descriptive and baseline tables,
hence the default `p_adjust = "none"`.

When a journal or a protocol asks for adjusted p-values anyway,
`p_adjust` applies
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) to the
family of every test in the table: one p-value per variable (one per
block in `table_outcome()`), never the rows of one variable, which share
one test. `"holm"` suits a confirmatory set of comparisons, `"BH"` a
screening one;
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
lists the methods. A test that returned no p-value stays out of the
family.

The adjustment runs before any formatting, so the `p` column and its
`<.001` threshold show the adjusted values, and the table note gives the
number of tests and the method. The raw p-values stay in the object: a
`p_unadjusted` column next to the p-value column of the returned data
frames, and `col_meta$p$p_unadjusted` (with the method in `p_adjust` and
the family size in `p_adjust_m`) in
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md).
Without `by`, or with `p_value = FALSE`, the table shows no p-value to
adjust, and a `p_adjust` other than `"none"` is an error.

## See also

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
for the transposed shape,
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
for categorical outcomes.

## Examples

``` r
table_outcome(sochealth, bmi, select = c(sex, smoking))
#> Descriptive statistics of bmi
#> 
#>  Variable    │   M     SD    Min    Max   95% CI LL  95% CI UL   n     p   
#> ─────────────┼─────────────────────────────────────────────────────────────
#>  Overall     │ 25.93  3.72  16.00  38.90    25.72      26.14    1188       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  sex         │                                                        .018 
#>    Female    │ 25.69  3.78  16.00  38.90    25.39      25.98     616       
#>    Male      │ 26.20  3.64  16.00  37.70    25.90      26.50     572       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  smoking     │                                                        .903 
#>    No        │ 25.96  3.76  16.00  38.90    25.72      26.21     915       
#>    Yes       │ 25.93  3.58  16.80  35.30    25.48      26.38     248       
#>    (Missing) │ 24.74  3.63  17.60  32.50    23.24      26.23      25       
#> 
#> Note. Missing values removed: bmi (12). Group comparison: Welch t-test. Each block compares bmi across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample.
table_outcome(sochealth, wellbeing_score, select = where(is.factor))
#> Descriptive statistics of wellbeing_score
#> 
#>  Variable            │   M     SD     Min    Max    95% CI LL  95% CI UL   n   
#> ─────────────────────┼─────────────────────────────────────────────────────────
#>  Overall             │ 69.04  15.62  18.70  100.00    68.16      69.93    1200 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  sex                 │                                                         
#>    Female            │ 67.16  14.80  19.60  100.00    65.99      68.33     620 
#>    Male              │ 71.05  16.23  18.70  100.00    69.73      72.37     580 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  age_group           │                                                         
#>    25-34             │ 67.55  15.36  24.60  100.00    65.60      69.49     242 
#>    35-49             │ 69.48  15.36  19.60  100.00    67.94      71.03     383 
#>    50-64             │ 69.06  15.94  18.70  100.00    67.33      70.80     327 
#>    65-75             │ 69.78  15.84  26.60  100.00    67.80      71.76     248 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  education           │                                                         
#>    Lower secondary   │ 57.22  15.44  18.70   97.90    55.33      59.10     261 
#>    Upper secondary   │ 68.97  13.62  26.70  100.00    67.82      70.12     539 
#>    Tertiary          │ 76.85  13.23  40.40  100.00    75.55      78.15     400 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  social_class        │                                                         
#>    Lower             │ 64.26  14.61  21.10  100.00    61.85      66.66     144 
#>    Working           │ 65.24  15.39  19.60  100.00    63.43      67.06     278 
#>    Lower middle      │ 69.17  16.58  23.70  100.00    67.37      70.98     326 
#>    Middle            │ 72.82  14.13  32.10  100.00    71.23      74.41     306 
#>    Upper middle      │ 72.78  14.95  18.70  100.00    70.33      75.22     146 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  region              │                                                         
#>    Central           │ 68.59  15.64  23.70  100.00    66.62      70.56     245 
#>    East              │ 69.69  16.00  30.40  100.00    67.07      72.32     145 
#>    North             │ 69.70  16.04  21.10  100.00    67.48      71.92     203 
#>    Other             │ 70.50  15.07  19.60  100.00    68.39      72.62     198 
#>    South             │ 67.57  15.70  18.70  100.00    65.38      69.75     201 
#>    West              │ 68.51  15.36  29.00  100.00    66.41      70.61     208 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  employment_status   │                                                         
#>    Employed          │ 69.42  15.07  19.60  100.00    68.35      70.49     762 
#>    Student           │ 70.42  16.16  18.70  100.00    67.75      73.09     143 
#>    Unemployed        │ 65.78  17.17  23.20  100.00    63.21      68.35     174 
#>    Inactive          │ 69.72  15.64  21.10  100.00    66.90      72.54     121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  income_group        │                                                         
#>    Low               │ 67.66  15.78  21.10  100.00    65.68      69.64     247 
#>    Lower middle      │ 68.78  15.21  23.70  100.00    67.26      70.29     388 
#>    Upper middle      │ 69.18  16.16  19.60  100.00    67.42      70.93     328 
#>    High              │ 70.95  15.51  18.70  100.00    68.89      73.02     219 
#>    (Missing)         │ 67.94  12.41  41.00   93.20    61.77      74.11      18 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  smoking             │                                                         
#>    No                │ 69.36  15.62  18.70  100.00    68.36      70.37     926 
#>    Yes               │ 67.65  15.66  21.10  100.00    65.69      69.60     249 
#>    (Missing)         │ 70.99  15.05  42.00  100.00    64.77      77.20      25 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  physical_activity   │                                                         
#>    No                │ 67.71  15.92  18.70  100.00    66.48      68.94     650 
#>    Yes               │ 70.61  15.13  21.10  100.00    69.35      71.88     550 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  dentist_12m         │                                                         
#>    No                │ 66.15  14.79  19.60  100.00    64.61      67.70     354 
#>    Yes               │ 70.25  15.81  18.70  100.00    69.18      71.32     846 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  self_rated_health   │                                                         
#>    Poor              │ 43.60  11.15  19.60   70.70    40.74      46.45      61 
#>    Fair              │ 58.54  13.49  18.70   94.30    56.92      60.17     266 
#>    Good              │ 71.58  12.58  26.60  100.00    70.53      72.62     558 
#>    Very good         │ 78.67  12.54  44.20  100.00    77.24      80.11     295 
#>    (Missing)         │ 73.42  11.32  56.30  100.00    68.13      78.72      20 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  bmi_category        │                                                         
#>    Normal weight     │ 70.47  15.36  18.70  100.00    69.07      71.87     465 
#>    Overweight        │ 68.78  15.65  19.60  100.00    67.49      70.06     569 
#>    Obesity           │ 65.43  15.84  29.00  100.00    62.91      67.95     154 
#>    (Missing)         │ 72.61  14.84  50.70   97.60    63.18      82.04      12 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  institutional_trust │                                                         
#>    Very low          │ 69.84  15.35  23.20  100.00    68.26      71.42     366 
#>    Low               │ 69.35  15.40  24.60  100.00    67.95      70.74     472 
#>    High              │ 67.92  16.17  19.60  100.00    65.83      70.00     234 
#>    Very high         │ 67.69  16.15  18.70  100.00    64.87      70.51     128 
#> 
#>  Variable            │   p   
#> ─────────────────────┼───────
#>  Overall             │       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  sex                 │ <.001 
#>    Female            │       
#>    Male              │       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  age_group           │  .372 
#>    25-34             │       
#>    35-49             │       
#>    50-64             │       
#>    65-75             │       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  education           │ <.001 
#>    Lower secondary   │       
#>    Upper secondary   │       
#>    Tertiary          │       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  social_class        │ <.001 
#>    Lower             │       
#>    Working           │       
#>    Lower middle      │       
#>    Middle            │       
#>    Upper middle      │       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  region              │  .461 
#>    Central           │       
#>    East              │       
#>    North             │       
#>    Other             │       
#>    South             │       
#>    West              │       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  employment_status   │  .049 
#>    Employed          │       
#>    Student           │       
#>    Unemployed        │       
#>    Inactive          │       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  income_group        │  .149 
#>    Low               │       
#>    Lower middle      │       
#>    Upper middle      │       
#>    High              │       
#>    (Missing)         │       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  smoking             │  .125 
#>    No                │       
#>    Yes               │       
#>    (Missing)         │       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  physical_activity   │  .001 
#>    No                │       
#>    Yes               │       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  dentist_12m         │ <.001 
#>    No                │       
#>    Yes               │       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  self_rated_health   │ <.001 
#>    Poor              │       
#>    Fair              │       
#>    Good              │       
#>    Very good         │       
#>    (Missing)         │       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  bmi_category        │  .002 
#>    Normal weight     │       
#>    Overweight        │       
#>    Obesity           │       
#>    (Missing)         │       
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  institutional_trust │  .366 
#>    Very low          │       
#>    Low               │       
#>    High              │       
#>    Very high         │       
#> 
#> Note. Group comparison: Welch one-way ANOVA (age_group, education, social_class, region, employment_status, income_group, self_rated_health, bmi_category, institutional_trust); Welch t-test (sex, smoking, physical_activity, dentist_12m). Each block compares wellbeing_score across the levels of one variable; blocks are not adjusted for one another. Overall = the whole analytic sample.
```
