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
  labels = NULL,
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
  variables.

- by:

  A single grouping column: one column block per level.

- labels:

  Named character vector of display labels.

- levels_keep:

  Levels to keep, as a character vector (all variables) or a named list
  (per variable).

- include_total:

  Add a `Total` column block with the whole design's percentages
  (default `TRUE`, only with `by`).

- drop_na:

  Drop missing values (default `FALSE`: they show as a `(Missing)`
  level). Shown or dropped, they never enter the test: the p-value is
  computed on the complete cases either way, which is the convention
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  applies.

- proportion_ci:

  Add the confidence interval of each percentage.

- ci_method:

  Interval method passed to
  [`survey::svyciprop()`](https://rdrr.io/pkg/survey/man/svyciprop.html):
  `"logit"` (default), `"likelihood"`, `"asin"`, `"beta"`, `"mean"`,
  `"xlogit"` or `"wilson"`.

- ci_level:

  Coverage of the interval.

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

- percent_digits, p_digits, decimal_mark:

  Number formatting.

- align:

  Numeric-cell alignment: `"decimal"`, `"center"` or `"right"`.

- output:

  One of `"default"`, `"data.frame"`, `"long"`, or a rendering engine:
  `"tinytable"`, `"gt"`, `"flextable"`, `"excel"`, `"clipboard"`,
  `"word"`. `"data.frame"` and `"long"` are synonyms here and return the
  same object: the wide compute frame, one row per level with a pair of
  columns per group. Note the difference from
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
  where the two tokens return genuinely different shapes – this design
  carries a single compute frame, and both names reach it.

- indent_text, indent_text_excel_clipboard:

  Level-row indentation, for the console and for the plain-text engines.

- excel_path, excel_sheet, clipboard_delim, word_path:

  Output destinations, as in
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).

- user_na:

  Honour declared missing values (see
  [`?freq`](https://amaltawfik.github.io/spicy/reference/freq.md)).

- style:

  A journal style; see
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md).

## Value

A `spicy_categorical_svy_table`: the wide compute frame, with the
display frame and the typed view attached. `output = "data.frame"` /
`"long"` returns the compute frame unclassed – the two tokens are
synonyms and return identical objects.

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

## The test

`svychisq()` with `chisq_statistic = "F"` (the default) is the Pearson
chi-square with the Rao-Scott second-order correction, referred to
F(ndf, `survey::degf(design)`). It is survey's own default and the one
Stata's `svy: tabulate` reports.

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

This function is **experimental** in the sense
[`?spicy`](https://amaltawfik.github.io/spicy/reference/spicy-package.md)
defines: it is new in this cycle, and the shape of the table and the
names of its design-specific arguments may still move – with a `NEWS.md`
entry – on their own clock rather than the parent family's. The numbers
themselves are survey's and do not move with it.

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
#> N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Std. errors: Design-based (Taylor linearisation). Confidence intervals and tests use the design degrees of freedom. % = estimated percentage within the column (survey::svymean). n = observed (unweighted) count.
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
#> N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; degrees of freedom vary by group (9 to 14). Std. errors: Design-based (Taylor linearisation). Confidence intervals and tests use the design degrees of freedom. Group comparison: design-based Pearson chi-square (Rao-Scott second-order correction). % = estimated percentage within the column (survey::svymean). n = observed (unweighted) count.
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
#> N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Std. errors: Design-based (Taylor linearisation). Confidence intervals and tests use the design degrees of freedom. Percentage CIs: logit (survey::svyciprop). % = estimated percentage within the column (survey::svymean). n = observed (unweighted) count. DEff = design effect (design-based variance / simple-random-sample variance at the same n).
```
