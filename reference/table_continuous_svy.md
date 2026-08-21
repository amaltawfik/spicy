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
  statistic = FALSE,
  show_n = TRUE,
  show_columns = NULL,
  ci = TRUE,
  labels = NULL,
  ci_level = 0.95,
  digits = 2,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
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
  variables.

- by:

  A single grouping column. One domain per level.

- exclude:

  Columns to drop from `select`.

- regex:

  Treat `select` as a regular expression.

- drop_na:

  Drop observations with a missing `by` value (default `TRUE`). With
  `FALSE` they form a `(Missing)` domain of their own – an ordinary
  subpopulation, with its own degrees of freedom – which is excluded
  from the group comparison.

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
  under a design) or `"nonparametric"`.

- p_value:

  Show the p-value column (defaults to `TRUE` with `by`).

- statistic:

  Show the test-statistic column.

- show_n:

  Show the count column.

- show_columns:

  Character vector of statistic tokens; `NULL` keeps the default
  display.

- ci, ci_level:

  The mean's confidence interval and its level.

- labels:

  Named character vector of display labels.

- digits, p_digits, decimal_mark:

  Number formatting.

- align:

  Numeric-cell alignment: `"decimal"`, `"center"` or `"right"`.

- output:

  One of `"default"`, `"data.frame"`, `"long"`, or a rendering engine:
  `"tinytable"`, `"gt"`, `"flextable"`, `"excel"`, `"clipboard"`,
  `"word"`.

- excel_path, excel_sheet, clipboard_delim, word_path:

  Output destinations, as in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).

- verbose:

  Report the columns skipped as non-numeric.

- user_na:

  Honour declared missing values (see
  [`?freq`](https://amaltawfik.github.io/spicy/reference/freq.md)).

- style:

  A journal style; see
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md).

## Value

A `spicy_continuous_svy_table`: the compute frame, with the display
frame and the typed view attached. `output = "data.frame"` / `"long"`
returns the compute frame unclassed.

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
normalised to sum to `n`. These are two estimands, not two
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
`test = "student"` warns and behaves like `"welch"`.

## Stability

This function is **experimental** in the sense
[`?spicy`](https://amaltawfik.github.io/spicy/reference/spicy-package.md)
defines: it is new in this cycle, and the shape of the table and the
names of its design-specific arguments may still move – with a `NEWS.md`
entry – on their own clock rather than the parent family's. The numbers
themselves are survey's and do not move with it.

## What is absent, and why

`weights` and `rescale` (the weighting *is* the design), `effect_size`
and `smd` (no established design-based variance), and `data`.

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
#> N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Standard errors: Taylor linearisation (survey). Confidence intervals and tests use the design degrees of freedom.
table_continuous_svy(dclus1, select = api00, by = stype)
#> Descriptive statistics by stype
#> 
#>  Variable │ Group    M       SD     Min     Max    95% CI LL  95% CI UL   n  
#> ──────────┼──────────────────────────────────────────────────────────────────
#>  api00    │ E      648.87  106.16  436.00  905.00   600.91     696.83    144 
#>           │ H      618.57   96.74  443.00  724.00   528.67     708.48     14 
#>           │ M      631.44  109.06  411.00  847.00   561.87     701.01     25 
#> 
#>  Variable │ Group  p (n) 
#> ──────────┼──────────────
#>  api00    │ E      .314  
#>           │ H            
#>           │ M            
#> 
#> N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; degrees of freedom vary by group (7 to 14). Standard errors: Taylor linearisation (survey). Confidence intervals and tests use the design degrees of freedom. Group comparison: design-based Wald test. The group comparison uses 12 degrees of freedom (observed groups only).
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
#> N = 183 (weighted 6194). Design: cluster (dnum), 15 PSU, with finite population correction; 14 degrees of freedom. Standard errors: Taylor linearisation (survey). Confidence intervals and tests use the design degrees of freedom. DEff = design effect (design-based variance / simple-random-sample variance at the same n). SE = design-based standard error of the mean.
```
