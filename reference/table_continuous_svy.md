# Descriptive statistics from a survey design

The design twin of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md):
the same table of means, standard deviations, intervals and counts,
computed from a
[`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
or
[`survey::as.svrepdesign()`](https://rdrr.io/pkg/survey/man/as.svrepdesign.html)
object instead of a data frame.

Not one statistic is computed here.
[`survey::svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
gives the mean, its standard error and its design effect,
[`survey::svyvar()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
the standard deviation,
[`survey::svyquantile()`](https://rdrr.io/pkg/survey/man/svyquantile.html)
the quantiles, and
[`survey::svyttest()`](https://rdrr.io/pkg/survey/man/svyttest.html) /
[`survey::regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html)
/
[`survey::svyranktest()`](https://rdrr.io/pkg/survey/man/svyranktest.html)
the group comparison. Every interval and every test is referred to
`survey::degf(design)`.

## Usage

``` r
table_continuous_svy(
  design,
  select = tidyselect::everything(),
  by = NULL,
  exclude = NULL,
  regex = FALSE,
  drop_na = TRUE,
  deff = FALSE,
  qrule = "math",
  df = NULL,
  test = c("welch", "student", "nonparametric"),
  p_value = NULL,
  p_adjust = "none",
  statistic = FALSE,
  show_n = TRUE,
  show_columns = NULL,
  ci = TRUE,
  labels = getOption("spicy.var_labels", FALSE),
  note_labels = NULL,
  ci_level = 0.95,
  digits = 2,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  note = NULL,
  note_add = NULL,
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  verbose = FALSE,
  user_na = TRUE,
  style = NULL
)
```

## Arguments

- design:

  A survey design:
  [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
  or
  [`survey::as.svrepdesign()`](https://rdrr.io/pkg/survey/man/as.svrepdesign.html).
  Two-phase, pps, database-backed and multiframe designs are refused
  with a classed error.

- select:

  Columns to summarize, as a tidyselect expression on the design's
  variables. As in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
  the `by` column is not part of the selection (naming it, or giving its
  position, is an error), a name that is not a column is an error, and
  so is a selection with no numeric column. The design's own columns are
  not part of it either, since they are not variables to describe: its
  sampling weights, replicate weights, cluster identifiers (every stage,
  unless each cluster of a one-stage design is a single observation) and
  finite population corrections.
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html),
  the other selection helpers and `regex` skip them, and naming one, or
  giving its position, is an error;
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  describes it from the data. Strata stay variables, since a
  stratification variable such as a region is often one of interest.
  These columns are read from the design object. A design made by
  [`survey::as.svrepdesign()`](https://rdrr.io/pkg/survey/man/as.svrepdesign.html)
  records none of them, and one made by
  [`survey::svrepdesign()`](https://rdrr.io/pkg/survey/man/svrepdesign.html)
  names its sampling weights only in its call, as a formula, until
  [`subset()`](https://rdrr.io/r/base/subset.html),
  [`update()`](https://rdrr.io/r/stats/update.html), `postStratify()`,
  `calibrate()`, `rake()` or `trimWeights()` replaces the call: leave
  such a column out with a negation (`select = !w`).

- by:

  A single grouping column. One domain per level, in the order of
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md):
  a factor's levels, any other column sorted (text in the C locale,
  `FALSE` before `TRUE`, numbers ascending). A `by` that no observation
  holds is an error under `drop_na = TRUE`.

- exclude:

  Columns to exclude. Supports tidyselect syntax and character vectors
  of column names.

- regex:

  Logical. If `FALSE` (the default), uses tidyselect helpers. If `TRUE`,
  the `select` argument is treated as a regular expression.

- drop_na:

  Drop observations with a missing `by` value. A table that computes
  removes the missing values by default and counts them in the note, the
  rule of
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md):
  `TRUE` (the default) removes them, and the missing values of each
  summarized variable are always removed. The note lists both, `by`
  last: a missing `by` takes the observation out of the whole table, a
  missing value only out of its own variable. With `FALSE` they form a
  `(Missing)` domain of their own – an ordinary subpopulation, with its
  own degrees of freedom – which is excluded from the group comparison.
  Ignored (with a warning) when `by` is not used.

- deff:

  Show the design effect: `FALSE` (default), `TRUE` (against sampling
  without replacement) or `"replace"` (against sampling with
  replacement, ignoring the finite population correction).

- qrule:

  Quantile rule: `"math"` (default), `"spicy"`, or anything
  [`survey::svyquantile()`](https://rdrr.io/pkg/survey/man/svyquantile.html)
  accepts, including a function.

- df:

  Degrees of freedom for the intervals. `NULL` (default) uses
  [`survey::degf()`](https://rdrr.io/pkg/survey/man/svychisq.html) on
  each domain. It does not reach the group comparison:
  [`survey::svyttest()`](https://rdrr.io/pkg/survey/man/svyttest.html)
  and
  [`survey::svyranktest()`](https://rdrr.io/pkg/survey/man/svyranktest.html)
  have no `df` argument, so the test keeps the design's own degrees of
  freedom and the note says so.

- test:

  Group comparison: `"welch"` (default), `"student"` (warns; identical
  under a design) or `"nonparametric"`. A variable whose complete cases
  include negatively weighted rows is not compared: a design-based test
  is not defined when the weights change sign. Its estimates are still
  reported, the note says which comparisons were withheld, and the call
  warns (`spicy_negative_weights_no_test`).

- p_value:

  Logical or `NULL`. If `TRUE` and `by` is used, adds a *p*-value column
  from the test specified by `test`. When `NULL` (the default), the
  *p*-value is shown automatically whenever `by` is supplied, and hidden
  otherwise. Pass `p_value = FALSE` to suppress the column explicitly.
  Ignored when `by` is not used.

- p_adjust:

  Multiple-comparison adjustment of the p-value column, applied to the
  family of every test in the table (one design-based p-value per
  variable; a withheld test stays out). One of `"none"` (default),
  `"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`,
  or `"BY"`, as in
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
  delegated to
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html). Needs
  `by` and the p-value column. See *Multiple-comparison adjustment*.

- statistic:

  Logical. If `TRUE` and `by` is used, the test statistic is shown in an
  additional column (e.g., `t(df) = ...`, `F(df1, df2) = ...`,
  `W = ...`, or `H(df) = ...`). Both `p_value` and `statistic` are
  independent; either or both can be enabled. Defaults to `FALSE`.
  Ignored when `by` is not used.

- show_n:

  Logical. If `TRUE`, includes an unweighted `n` column in the printed
  ASCII table and in every rendered output (`tinytable`, `gt`,
  `flextable`, `word`, `excel`, `clipboard`). Set to `FALSE` to drop the
  `n` column structurally from those outputs (no empty placeholder, no
  spanner). The `n` column is always present in `output = "long"` for
  downstream programmatic access. Defaults to `TRUE`. Ignored (with a
  warning) when `show_columns` is supplied.

- show_columns:

  Character vector of statistic tokens; `NULL` keeps the default
  display.

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

- ci_level:

  Confidence level for the mean confidence interval (default: `0.95`): a
  number strictly between 0 and 1.

- digits:

  Number of decimal places of the statistics on each variable's own
  scale: the mean, SD, standard error, median, IQR, quartiles, minimum
  and maximum, and the confidence interval of the mean. One non-negative
  whole number sets them for every variable (default: `2`), and the
  weighted *n* and the design effect with them. A vector named by
  column, not by label, gives each variable it names its own,
  `digits = c(height = 0, bmi = 1)`; the other variables, the weighted
  *n* and the design effect keep the default, the `digits` of a
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  in force included. A name that matches no variable warns and is left
  out, as in `labels`. A fraction is an error, as for every `*_digits`
  argument. The *p*-values (`p_digits`), the test statistics (two
  decimals) and the counts keep their own decimals.

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

- note:

  The note under the table. `NULL` (the default) builds it: the general
  note, one paragraph opened by the lead-in of the style ("Note." by
  default, none under the `jama` and `lancet` themes; see `note_label`
  in
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)),
  then, in a
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  with `stars`, the legend of the stars on a line of its own. A single
  string replaces the general note, the sentences the table writes about
  its data included. It is printed as typed, line breaks (`"\n"`)
  included, after the lead-in of the style, which a string opening with
  a lead-in of its own does not repeat
  (`note = "Note. Data weighted."`), and the legend of the stars stays
  below it. `""` keeps that legend alone, and no note at all without
  stars. `FALSE` removes the note from every output, the legend
  included.

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

  One of `"default"`, `"data.frame"`, `"long"`, or a rendering engine:
  `"tinytable"`, `"gt"`, `"flextable"`, `"excel"`, `"clipboard"`,
  `"word"`. `"data.frame"` is the displayed table with its numbers,
  `"long"` one row per `(variable x group)` with every statistic, as in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md);
  see *Value*.

- excel_path:

  File path for `output = "excel"`, ending in `".xlsx"`, in a directory
  that exists.

- excel_sheet:

  Sheet name for `output = "excel"`. `NULL` (the default) uses
  `"Descriptives"`.

- clipboard_delim:

  Delimiter for `output = "clipboard"` (default: `"\t"`). A cell holding
  the delimiter itself, a double quote or a line break is quoted RFC
  4180-style, so the grid survives whatever delimiter you choose.

- word_path:

  File path for `output = "word"`, ending in `".docx"`, in a directory
  that exists.

- verbose:

  Logical. If `TRUE`, a message (class `spicy_info`) names the
  non-numeric columns left out (default: `FALSE`).

- user_na:

  Logical. If `TRUE` (the default), declared missing values never reach
  the numeric summaries: they are excluded like `NA` and disclosed in
  the table note (`Declared missing values removed: ...`);
  declared-missing `by` values form no group. If `FALSE`, the declared
  codes are summarized as ordinary numbers. See the "Declared missing
  values" section of
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).

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

- `"default"`: a `spicy_continuous_svy_table` (a `data.frame` subclass,
  with the parent class `"spicy_table"`), printed when the call is bare;
  its columns are the table's compute frame, and the display frame and
  the typed view are attached.

- `"data.frame"`: the displayed table with its numbers, what
  [as.data.frame()](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_svy_table.md)
  of the default object returns: one row per printed row, the key
  columns `variable`, `label` and, with `by`, `group` and `.row_role`
  (`"group"`, or `"missing"` for the group of missing `by` values), then
  the printed columns under their names (`"M"`, `"SD"`, `"SE"`,
  `"95% CI LL"`, ..., `"n"`, `"p"`), as in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).

- `"long"`: one row per `(variable x group)` (per variable without `by`)
  with every statistic the table computes, whatever it displays:
  `variable`, `label`, `group` and `.group_role` (with `by`, as in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)),
  `mean`, `sd`, `se`, `min`, `max`, `ci_lower`, `ci_upper`, `median`,
  `q1`, `q3`, `iqr`, `med_ci_lower`, `med_ci_upper`, `n` (observed),
  `weighted_n` (the estimated population), `deff`, `degf` (the degrees
  of freedom of the domain), and, with `by`, the test on the first row
  of each variable: `test_type` (`"design_t"` or `"design_f"`),
  `statistic`, `df`, `df.residual`, `p.value` (with the *p*-value) and,
  under `p_adjust`, `p_unadjusted`.

- `"tinytable"`, `"gt"`, `"flextable"`: the rendered table.

- `"excel"` / `"word"`: writes the file and returns its path invisibly.

- `"clipboard"`: copies the table and returns the display `data.frame`
  invisibly.

The default object and the two data frames carry the whole note as
printed, its lead-in included, in `attr(x, "note")`. The default object
also keeps the title it prints in `attr(x, "title")`.

## Which function do I need?

A data frame with a column of weights is
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
(`weights = `). A survey design object – strata, clusters, finite
population correction, calibration, replicate weights – is this
function. Passing one to the other errors with the name of the right
one; there is no silent coercion, because the design-based standard
errors, degrees of freedom and tests cannot be recovered from the
weights alone.

## Two conventions, one bridge

`table_continuous(weights = )` implements the **frequency-expansion**
convention: a weight is a number of copies, and `SD` has denominator
`sum(w) - 1`. This function implements the **sampling-weight**
convention: a weight is a number of units represented, and `SD` is
`sqrt(survey::svyvar())`, whose denominator is `n - 1` on weights
normalized to sum to `n`. These are two estimands, not two
approximations of one.

`rescale = TRUE` is the bridge, and it is an identity rather than a
coincidence. Writing `w' = w * n / sum(w)`, the rescaled weighted
variance is
`sum(w' (x - xbar)^2) / (sum(w') - 1) = n / (n - 1) * sum(w (x - xbar)^2) / sum(w)`,
which is what
[`survey::svyvar()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
computes. So on a design that declares nothing but weights,
`table_continuous(weights = w, rescale = TRUE)` and this function return
the same mean and the same standard deviation. The default
`rescale = FALSE` does not, and that is the estimand boundary, not a
bug.

The mean is continuous across both regimes: `sum(w x) / sum(w)` does not
move when the weights are rescaled.

## Choosing the statistics

`show_columns` takes the tokens of
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
with two additions and one removal:

- `"se"` – the design-based standard error of the mean;

- `"deff"` – the design effect (requires `deff = TRUE`);

- `"med_ci"` is refused. The exact interval of the sibling inverts a
  binomial sign test on independent observations, which a clustered or
  stratified sample is not.

## Quantiles

`qrule = "math"` is the default and estimates `inf{x : F(x) >= p}`, the
quantile of the *population*. `qrule = "spicy"` switches to the type-7
interpolation
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
uses, for a reader who needs the two tables to agree cell for cell; any
other value – including a function – is handed to
[`survey::svyquantile()`](https://rdrr.io/pkg/survey/man/svyquantile.html)
untouched. The note always says which rule produced the numbers.

## Groups and degrees of freedom

`by = ` cuts one domain per group with `[` on the design. survey
recomputes the degrees of freedom on the primary sampling units and
strata each domain retains, so a grouped table generally carries a
*different* df per row; the note gives the span when they differ.

A group with a missing value is a domain like any other:
`drop_na = FALSE` gives it a `(Missing)` row, with its own degrees of
freedom. A domain reduced to one primary sampling unit has none, and its
interval shows the undefined dash rather than an interval built on
`qt(p, df = 0)`.

The comparison is a single test on the whole design, not a set of
pairwise ones:
[`survey::svyttest()`](https://rdrr.io/pkg/survey/man/svyttest.html)
with two observed groups,
[`survey::regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html)
on [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) with
three or more, or
[`survey::svyranktest()`](https://rdrr.io/pkg/survey/man/svyranktest.html)
under `test = "nonparametric"`. Under a design the Welch / Student
distinction does not exist – the variance is the design's – so
`test = "student"` warns and behaves like `"welch"`. A variable with no
variance, or whose domains are each constant on their own value (a
residual variance of zero), has no comparison, with a warning (class
`spicy_undefined_stat`), where survey would still return a p-value.
Under `test = "nonparametric"` too, unlike
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md):
[`survey::svyranktest()`](https://rdrr.io/pkg/survey/man/svyranktest.html)
is a design-based test of the ranks, which are then constant within the
domains as well. Its test cells show the undefined mark, as do those of
any comparison that cannot run (too few observations in a domain, a test
refused under weights that change sign). The note names the comparison
of each variable.

A variable with no observed value in a domain (a question not asked in
that group) has no data in it, as in
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md):
its cells there show an en dash, *n* included, those rows are not
counted in the missing values of the note, the comparison uses the
domains with data, and the note says so ("No data for X in G.").

## Stability

This function is **stabilizing** in the sense
[`?spicy`](https://amaltawfik.github.io/spicy/reference/spicy-package.md)
defines: the names of its design-specific arguments may still be
tightened before 1.0 – with a `NEWS.md` entry – but the behavior does
not change silently. It was experimental through 0.13; the shape of the
table has held across the cycle, so it now moves on the parent family's
clock rather than its own. The numbers themselves are survey's and do
not move with it.

## What is absent, and why

`weights` and `rescale` (the weighting *is* the design), `effect_size`
and `smd` (no established design-based variance), and `data`.

## Multiple-comparison adjustment

The p-values of a descriptive table describe: they test no prespecified
hypothesis. STROBE (explanation of item 14) and the SAMPL guidelines
advise against significance tests in descriptive and baseline tables,
hence the default `p_adjust = "none"`.

When a journal or a protocol asks for adjusted p-values anyway,
`p_adjust` applies
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) to the
family of every test in the table: one p-value per variable (one per
block in
[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md)),
never the rows of one variable, which share one test. `"holm"` suits a
confirmatory set of comparisons, `"BH"` a screening one;
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
for the data-frame sibling,
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
for categorical variables,
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
on a [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)
fit for a model.

## Examples

``` r
data(api, package = "survey")
dclus1 <- survey::svydesign(
  id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc
)
table_continuous_svy(dclus1, select = c(api00, api99))
#> Descriptive statistics
#> 
#>  Variable │   M       SD     Min     Max    95% CI LL  95% CI UL   n  
#> ──────────┼───────────────────────────────────────────────────────────
#>  api00    │ 644.17  105.75  411.00  905.00   593.68     694.66    183 
#>  api99    │ 606.98  112.85  365.00  890.00   555.02     658.94    183 
#> 
#> Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Std. errors: Design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. CI = confidence interval; LL = lower limit; UL = upper limit.
table_continuous_svy(dclus1, select = api00, by = stype)
#> Descriptive statistics by stype
#> 
#>  Variable │ Group    M       SD     Min     Max    95% CI LL  95% CI UL   n  
#> ──────────┼──────────────────────────────────────────────────────────────────
#>  api00    │ E      648.87  106.16  436.00  905.00   600.91     696.83    144 
#>           │ H      618.57   96.74  443.00  724.00   528.67     708.48     14 
#>           │ M      631.44  109.06  411.00  847.00   561.87     701.01     25 
#> 
#>  Variable │ Group   p   
#> ──────────┼─────────────
#>  api00    │ E      .314 
#>           │ H           
#>           │ M           
#> 
#> Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; degrees of freedom vary by group (7 to 14). Std. errors: Design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. P-value: design-based Wald test. The group comparison uses 12 degrees of freedom (observed groups only). CI = confidence interval; LL = lower limit; UL = upper limit.
table_continuous_svy(
  dclus1,
  select = api00,
  show_columns = c("m", "se", "ci", "deff", "n"),
  deff = TRUE
)
#> Descriptive statistics
#> 
#>  Variable   │    M        SE      95% CI LL    95% CI UL     n     DEff  
#> ────────────┼────────────────────────────────────────────────────────────
#>  api00      │  644.17    23.54     593.68       694.66      183    9.35  
#> 
#> Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Std. errors: Design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. SE = design-based standard error of the mean; CI = confidence interval; LL = lower limit; UL = upper limit; DEff = design effect.
```
