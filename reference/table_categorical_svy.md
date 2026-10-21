# Categorical summary table from a survey design

The design twin of
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md):
counts and estimated percentages of categorical variables computed from
a [`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
or
[`survey::as.svrepdesign()`](https://rdrr.io/pkg/survey/man/as.svrepdesign.html)
object instead of a data frame.

Every statistic is survey's.
[`survey::svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html)
estimates the percentages and their design effects,
[`survey::svyciprop()`](https://rdrr.io/pkg/survey/man/svyciprop.html)
their confidence intervals, and
[`survey::svychisq()`](https://rdrr.io/pkg/survey/man/svychisq.html)
tests the association, Rao-Scott corrected and referred to the design
degrees of freedom.

## Usage

``` r
table_categorical_svy(
  design,
  select = tidyselect::everything(),
  by = NULL,
  labels = getOption("spicy.var_labels", FALSE),
  levels_keep = NULL,
  include_total = TRUE,
  drop_na = FALSE,
  proportion_ci = FALSE,
  ci_method = c("logit", "likelihood", "asin", "beta", "mean", "xlogit", "wilson"),
  ci_level = 0.95,
  chisq_statistic = c("F", "Chisq", "Wald", "adjWald", "saddlepoint"),
  deff = FALSE,
  df = NULL,
  p_value = NULL,
  p_adjust = "none",
  percent_digits = 1,
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

  Columns to tabulate, as a tidyselect expression on the design's
  variables. The design's own columns are not variables to describe: its
  sampling weights, replicate weights, cluster identifiers (every stage,
  unless each cluster of a one-stage design is a single observation) and
  finite population corrections.
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html)
  and the other selection helpers skip them, and naming one, or giving
  its position, is an error;
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
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

  A single grouping column: one column block per level, in the order of
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  (a factor's levels, any other column sorted: text in the C locale,
  `FALSE` before `TRUE`, numbers ascending), the order the levels of
  each variable follow too.

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

  Names that match no column are ignored, so one vector can serve every
  table of a project. The default reads `options(spicy.var_labels)`, and
  a `TRUE` or `FALSE` typed in the call wins over it. Any other value,
  `NULL` included, is an error. A variable label documents the data
  file; a published table carries the wording you set here.

- levels_keep:

  Levels to keep, as a character vector (all variables) or a named list
  (per variable).

- include_total:

  Add a `Total` column block with the whole design's percentages
  (default `TRUE`, only with `by`). A group displayed with the label of
  that block is an error, as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).

- drop_na:

  Drop missing values. A table that counts shows them by default, the
  rule of
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md):
  `FALSE` (the default) shows them as a `(Missing)` level, and a level
  displayed with that label beside them is an error, as there. Shown or
  dropped, they never enter the test: the p-value is computed on the
  complete cases either way, which is the convention
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  applies. Under `drop_na = TRUE`, a `by` that no observation holds is
  an error (class `spicy_invalid_data`).

- proportion_ci:

  Add the confidence interval of each percentage.

- ci_method:

  Interval method passed to
  [`survey::svyciprop()`](https://rdrr.io/pkg/survey/man/svyciprop.html):
  `"logit"` (default), `"likelihood"`, `"asin"`, `"beta"`, `"mean"`,
  `"xlogit"` or `"wilson"`.

- ci_level:

  Confidence level of the intervals of `proportion_ci` (default `0.95`):
  a number strictly between 0 and 1.

- chisq_statistic:

  Statistic for
  [`survey::svychisq()`](https://rdrr.io/pkg/survey/man/svychisq.html):
  `"F"` (default), `"Chisq"`, `"Wald"`, `"adjWald"` or `"saddlepoint"`.
  `"saddlepoint"` is refused on a replicate-weights design: survey
  computes its p-value without the denominator degrees of freedom there,
  so it comes out too small.

- deff:

  Show the design effect of each percentage: `FALSE` (default), `TRUE`
  or `"replace"`.

- df:

  Degrees of freedom for the intervals. `NULL` (default) uses
  [`survey::degf()`](https://rdrr.io/pkg/survey/man/svychisq.html) on
  each domain. It does not reach the test:
  [`survey::svychisq()`](https://rdrr.io/pkg/survey/man/svychisq.html)
  has no `df` argument, so the Rao-Scott reference distribution keeps
  the design's own degrees of freedom and the note says so.

- p_value:

  Show the p-value column (defaults to `TRUE` with `by`). A variable
  whose complete cases include negatively weighted rows is not tested:
  the Rao-Scott correction is a function of the design variance, which
  is not defined when the weights change sign. Its percentages are still
  reported, the note says which tests were withheld, and the call warns
  (`spicy_negative_weights_no_test`).

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

  Horizontal alignment of the numeric columns: `"decimal"` (the
  default), `"center"` or `"right"`, as in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
  whose rendering engines this table shares, its Excel output included.

- output:

  One of `"default"`, `"data.frame"`, `"long"`, or a rendering engine:
  `"tinytable"`, `"gt"`, `"flextable"`, `"excel"`, `"clipboard"`,
  `"word"`. `"data.frame"` is the displayed table with its numbers,
  `"long"` one row per variable, level and group, as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md);
  see *Value*.

- indent_text:

  Prefix used for modality labels in report table building. Defaults to
  `" "` (two spaces).

- indent_text_excel_clipboard:

  Stronger indentation used in Excel and clipboard exports. Defaults to
  six non-breaking spaces.

- excel_path:

  Path for `output = "excel"`, ending in `".xlsx"`, in a directory that
  exists. Defaults to `NULL`.

- excel_sheet:

  Sheet name for Excel export. `NULL` (the default) uses
  `"Categorical"`.

- clipboard_delim:

  Delimiter for clipboard text export. Defaults to `"\t"`. A cell
  holding the delimiter itself, a double quote or a line break is quoted
  RFC 4180-style, so the grid survives whatever delimiter you choose.

- word_path:

  File path for `output = "word"`, ending in `".docx"`, in a directory
  that exists. Defaults to `NULL`.

- user_na:

  Logical. If `TRUE` (the default), declared missing values in the row
  variables and in `by` are treated as missing: they join the
  `"(Missing)"` level under `drop_na = FALSE`, and under
  `drop_na = TRUE` they are removed with a dedicated disclosure line
  (`Declared missing values removed: ...`). If `FALSE`, the declared
  codes stay valid categories. See the "Declared missing values" section
  of [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).

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

## Value

Depends on `output`:

- `"default"`: a `spicy_categorical_svy_table` (a `data.frame` subclass,
  with the parent class `"spicy_table"`), printed when the call is bare;
  its columns are the table's compute frame, and the display frame and
  the typed view are attached.

- `"data.frame"`: the displayed table with its numbers, what
  [as.data.frame()](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_categorical_svy_table.md)
  of the default object returns: one row per printed row (the header row
  of each variable, which carries its *p*-value, then its levels), the
  key columns `variable` (the column name), `label` (the label
  displayed), `level` and `.row_role` (`"factor_header"`, `"level"`, or
  `"missing"` on the row of missing values the table adds), then the
  printed columns in their order: per block of `by` (and the `Total`
  margin) `<group> n`, `<group> \%`, with `proportion_ci` the interval
  bounds `<group> \% CI lower` and `<group> \% CI upper`, with `deff`
  `<group> DEff`, and last `p`. Without `by`, the columns of the one
  block are `n`, `\%`, ...

- `"long"`: one row per variable, level and group, in the order of the
  table, with `variable`, `label`, `level`, `.row_role` (the role of
  `level`: `"level"`, or `"missing"`), with `by` `group`, `.group_role`
  (the role of `group`: `"group"`, `"missing"` for the column of missing
  `by` values, or `"total"`) and `total` (`TRUE` on the rows of the
  margin), then `n` (observed), `pct` (the estimated percentage),
  `ci_lower`, `ci_upper` (its interval, `NA` without `proportion_ci`),
  `deff` (`NA` without `deff`), and, with `by` and the *p*-value,
  `test_type` (the statistic of
  [`survey::svychisq()`](https://rdrr.io/pkg/survey/man/svychisq.html),
  `chisq_statistic`), `p.value` and, under `p_adjust`, `p_unadjusted`.

- `"tinytable"`, `"gt"`, `"flextable"`: the rendered table.

- `"excel"` / `"word"`: writes the file and returns its path invisibly.

- `"clipboard"`: copies the table and returns the display `data.frame`
  invisibly.

The default object and the two data frames carry the whole note as
printed, "Note." included, in `attr(x, "note")`.
[tidy()](https://amaltawfik.github.io/spicy/reference/tidy.spicy_categorical_svy_table.md)
reports the proportions between 0 and 1.

## What the columns are

`n` is the *observed* count – the number of rows in the sample, not an
estimated population size. `%` is the estimated percentage *within* its
column: without `by` it is the distribution of the variable in the
population, with `by` the distribution inside that domain. The table
note gives the sample size and the estimated population together,
because neither alone tells the reader what they are looking at.

`proportion_ci = TRUE` adds the interval of each percentage. `ci_method`
chooses among the seven
[`survey::svyciprop()`](https://rdrr.io/pkg/survey/man/svyciprop.html)
offers; the default `"logit"` is bounded inside 0 to 100, which the Wald
interval (`"mean"`) is not. The percentage itself always comes from
[`survey::svymean()`](https://rdrr.io/pkg/survey/man/surveysummary.html),
so it does not move when `ci_method` does.

A percentage of 0 or 100 has no design-based interval: its estimated
variance is zero. Whatever `ci_method`, such a cell gets the exact
binomial (Clopper-Pearson) interval on the unweighted count of its
column, with no degrees-of-freedom factor. This is the rule of Korn and
Graubard (1998, p. 194) for an estimate of 0, which the NCHS data
presentation standards for proportions apply to its complement as well
(Parker et al. 2017, pp. 2-3). The note says so when such a cell is
shown.

## The test

`svychisq()` with `chisq_statistic = "F"` (the default) is the Pearson
chi-square with the Rao-Scott second-order correction, referred to
F(ndf, `survey::degf(design)`). It is survey's own default and the one
Stata's `svy: tabulate` reports. The design-based test has no exact
counterpart, so the switch to Fisher's exact test that `test` makes in
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
when an expected count is below 5 has no equivalent here.

It runs on the *complete cases* of the two variables, and on their
observed levels: a `(Missing)` row and a declared-but-unobserved level
are descriptive, and neither belongs to the null hypothesis. The p-value
is therefore the same whether `drop_na` shows those rows or removes
them, and the intervals beside it describe the same domain – the two
families test the same table.

`"Chisq"` shows the p-value only: survey adjusts the statistic in the
`"F"` branch and only the p-value in the `"Chisq"` one, so the statistic
there is not the one the p-value came from. `"Wald"`, `"adjWald"` and
`"saddlepoint"` are available; `"lincom"` and `"wls-score"` are refused,
the first because its integration is documented as failing in the far
tail (`?pchisqsum`), the second because it has no reporting convention
here.

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

`weights` and `rescale` (the weighting *is* the design). `correct`
(Yates), `simulate_p` and `simulate_B`, which have no meaning once the
reference distribution is Rao-Scott's. And the association measures:
Cramer's V, phi, tau-b/c, gamma, Somers' D and lambda have no
established design-based variance, and the intervals
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
gives them assume simple random sampling. The design-based measure of
association here is the Rao-Scott test in the `p` column; for an effect
size, model it with `table_regression(survey::svyglm(...))`.

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

## References

Korn, E. L., & Graubard, B. I. (1998). Confidence intervals for
proportions with small expected number of positive counts estimated from
survey data. *Survey Methodology*, 24(2), 193-201.

Parker, J. D., Talih, M., Malec, D. J., et al. (2017). *National Center
for Health Statistics data presentation standards for proportions*.
Vital and Health Statistics, Series 2, No. 175.

## See also

[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
for the data-frame sibling,
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
for continuous variables.

## Examples

``` r
data(api, package = "survey")
dclus1 <- survey::svydesign(
  id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc
)
table_categorical_svy(dclus1, select = c(stype, awards))
#> Categorical table
#> 
#>  Variable   │   n      %    
#> ────────────┼───────────────
#>  stype      │               
#>    E        │  144    78.7  
#>    H        │   14     7.7  
#>    M        │   25    13.7  
#> ╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  awards     │               
#>    No       │   53    29.0  
#>    Yes      │  130    71.0  
#> 
#> Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Std. errors: Design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. % = estimated percentage within the column (survey::svymean). n = observed (unweighted) count.
table_categorical_svy(dclus1, select = stype, by = sch.wide)
#> Categorical table by sch.wide
#> 
#>  Variable   │  No n    No %    Yes n    Yes %    Total n    Total %     p    
#> ────────────┼────────────────────────────────────────────────────────────────
#>  stype      │                                                          .022  
#>    E        │   12     52.2     132     82.5       144       78.7            
#>    H        │    3     13.0      11      6.9        14        7.7            
#>    M        │    8     34.8      17     10.6        25       13.7            
#> 
#> Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; degrees of freedom vary by group (9 to 14). Std. errors: Design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. Group comparison: design-based Pearson chi-square (Rao-Scott second-order correction). % = estimated percentage within the column (survey::svymean). n = observed (unweighted) count.
table_categorical_svy(
  dclus1,
  select = stype,
  proportion_ci = TRUE,
  deff = TRUE
)
#> Categorical table
#> 
#>  Variable   │   n      %      95% CI LL    95% CI UL    DEff  
#> ────────────┼─────────────────────────────────────────────────
#>  stype      │                                                 
#>    E        │  144    78.7      67.1         87.0       2.40  
#>    H        │   14     7.7       3.5         15.8       1.91  
#>    M        │   25    13.7       8.4         21.3       1.40  
#> 
#> Note. N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Std. errors: Design-based (Taylor linearization). Confidence intervals and tests use the design degrees of freedom. Percentage CIs: logit (survey::svyciprop). % = estimated percentage within the column (survey::svymean). n = observed (unweighted) count. DEff = design effect (design-based variance / simple-random-sample variance at the same n).
```
