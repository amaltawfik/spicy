# One exposure across several outcomes

Fits one model per outcome, each with the same exposure and the same
covariates, and shows the exposure's coefficients in one table: one row
per outcome for a numeric exposure, one block of rows per outcome for a
factor. The covariates enter every model and are named in the note, not
shown in the table. Each outcome is fitted by
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
[`stats::glm()`](https://rdrr.io/r/stats/glm.html), as `method` and
`family` say.

## Usage

``` r
table_regression_outcomes(
  data,
  outcomes,
  exposure,
  covariates = NULL,
  method = c("lm", "glm"),
  family = stats::binomial(),
  complete_cases = FALSE,
  p_adjust = "holm",
  show_columns = c("n", "b", "ci", "p"),
  show_intercept = FALSE,
  labels = getOption("spicy.var_labels", FALSE),
  title = NULL,
  ...,
  note_labels = NULL,
  note = NULL,
  note_add = NULL
)
```

## Arguments

- data:

  A data frame. A survey design is refused with a pointer to the
  design-based route (class `spicy_wrong_regime`).

- outcomes:

  The outcome columns, two at least (tidyselect, e.g.
  `c(bmi, wellbeing_score)` or `starts_with("life_sat")`). The exposure
  is not part of the selection:
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html)
  and the other helpers skip it, and naming it is an error.

- exposure:

  The exposure column (unquoted name, tidyselect): numeric, logical, a
  factor or a character column.

- covariates:

  Covariate columns entered in every model (tidyselect), or `NULL`
  (default) for unadjusted models. The exposure and the outcomes are not
  part of the selection.

- method:

  The model fitted for every outcome: `"lm"` (default) or `"glm"`. Not
  combined with a list of families. See *Which model for which outcome*.

- family:

  The family of the glm: one family for every outcome (default
  [`binomial()`](https://rdrr.io/r/stats/family.html), used when
  `method = "glm"`; typing a family alone selects
  [`glm()`](https://rdrr.io/r/stats/glm.html)), or a named list giving a
  family to some outcomes, the others fitted by
  [`lm()`](https://rdrr.io/r/stats/lm.html). See *Which model for which
  outcome*.

- complete_cases:

  Logical, default `FALSE`. `TRUE` fits every model on the rows complete
  on all the outcomes, the exposure and the covariates. See *Sample
  sizes*.

- p_adjust:

  The adjustment of the p-values for multiple testing, across the
  exposure rows of all the models: `"holm"` (default), `"none"`, or any
  other method of
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html). See
  *Multiplicity*.

- show_columns:

  Passed to
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).
  Default `c("n", "b", "ci", "p")`; `"n"` is the sample size of each
  model. `"r2"` and `"adj_r2"` give each outcome's own model R-squared
  (linear models), next to its N. The AME columns and `"none"` are
  refused.

- show_intercept:

  Display the `(Intercept)` row of each model? Default `FALSE`. See
  *Intercepts*.

- labels:

  How the variables are named: an outcome in its row or at the head of
  its block, the exposure in the title and on its rows, a covariate in
  the note. The four forms of
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md):
  `FALSE` (the column names), `TRUE` (each column's `label` attribute
  when it has one), a named character vector, or
  [`var_labels()`](https://amaltawfik.github.io/spicy/reference/var_labels.md).
  The names are the columns and the coefficient names (`"sexMale"` names
  the row of one level, in every block). The default reads
  `options(spicy.var_labels)`.

- title:

  Table title; `NULL` (default) names the model when every outcome has
  the same (`"Linear regression of each outcome on <exposure>"`,
  `"Logistic regression of each outcome on <exposure>"`), else
  `"Regression of each outcome on <exposure>"`.

- ...:

  Passed to
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  (`exponentiate`, `vcov`, `cluster`, `ci_level`, `digits`, `output`,
  ...), each by its full name: an unknown or abbreviated name is an
  error. `nested`, `layout`, `keep`, `drop`, `factor_layout = "flat"`,
  `outcome_labels` and `standardized` do not apply to a table of
  outcomes and are refused. `cluster` must be a single vector with one
  value per row of `data`; it is aligned to each model's own estimation
  sample.

- note_labels:

  A named character vector giving the exposure (in the title) and the
  covariates (in the note) a label of their own. The outcomes are named
  in the rows and take none.

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

## Value

See
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
(same output contract). The first column is headed "Outcome". `tidy()`
and `output = "long"` give one row per outcome and coefficient of the
exposure, named as R names the coefficient, with the outcome in the
`outcome` column and whether the estimate is exponentiated in
`exponentiated`. `glance()` gives one row per outcome, the fit
statistics of its model.

## Which model for which outcome

The function does not guess a model from the type of an outcome: it fits
what `method` and `family` say.

|  |  |
|----|----|
| You type | Models |
| neither `method` nor `family` | every outcome by [`lm()`](https://rdrr.io/r/stats/lm.html) |
| `method = "glm"` | every outcome by [`glm()`](https://rdrr.io/r/stats/glm.html), `family = binomial()` |
| `family = poisson()` (one family) | every outcome by [`glm()`](https://rdrr.io/r/stats/glm.html) with that family |
| `family = list(smoking = binomial(), visits = poisson())` | the named outcomes by [`glm()`](https://rdrr.io/r/stats/glm.html), the others by [`lm()`](https://rdrr.io/r/stats/lm.html) |
| a list and `method` | an error: the list already says which outcomes are glm |

A family is given in any form
[`stats::glm()`](https://rdrr.io/r/stats/glm.html) takes: the object,
its name or its constructor, in a list as alone. A name of the list that
is not an outcome is an error, and so is
[`gaussian()`](https://rdrr.io/r/stats/family.html) with the identity
link (leave that outcome out of the list: it is then fitted by
[`lm()`](https://rdrr.io/r/stats/lm.html)). A two-level outcome (a
factor, a logical, or 0/1) fitted by
[`lm()`](https://rdrr.io/r/stats/lm.html) is a linear probability model
of its second level, and a warning says so unless `method = "lm"` was
typed.

## Scales

`exponentiate = TRUE` exponentiates the blocks of the glm with a log or
logit link (odds ratios, rate ratios) and leaves the linear models on
their scale. When every block has the same measure, the column is headed
by it ("B", "OR", "IRR"). When the measures differ, the column is headed
"Estimate" and each outcome names its own ("smoking (OR)", "bmi (B)");
the note says which models are exponentiated. `tidy()` and
`output = "long"` say it row by row in their `exponentiated` column. The
`"t"` column is refused when linear models and glm mix: it would hold a
t for some rows and a z for others.

## Sample sizes

By default each model is fit on its **own complete cases** (its outcome,
the exposure and the covariates), so N can vary across outcomes. The `N`
column shows each model's N on the row of its outcome, or on the header
of its block (the line that names the outcome) when the outcome has
several rows, and the note gives the range whenever the Ns differ. The
R-squared columns are placed the same way. Pass `complete_cases = TRUE`
to fit every model on the rows complete on all the outcomes, the
exposure and the covariates; the note then gives that common N.

## Multiplicity

The p-values are adjusted by default (`p_adjust = "holm"`), across the
exposure rows of every model taken as one family: k tests for a numeric
exposure and k outcomes, k x (L - 1) for a factor of L levels. The
covariates and the intercepts are not part of the family. The note
counts the outcomes and the tests. `p_adjust = "none"` gives the
unadjusted p-values, and any method of
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) is
accepted. A term-level test (`"partial_f"`, `"partial_chi2"`) counts
once per outcome, in a family of its own.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
leaves the coefficients of one model unadjusted by default, each testing
its own hypothesis. One exposure tested on several outcomes is the other
case its help names, the design with several endpoints, where the
adjustment is the convention: see *Multiple-comparison adjustment*
there.

## Intercepts

Hidden by default: each model has its own intercept, which says nothing
about the exposure. `show_intercept = TRUE` shows them, and the table
then gives each outcome a block of rows, headed by the outcome, opening
with its model's `(Intercept)` row.

## Refused

`nested`, `layout`, `keep`, `drop`, `factor_layout = "flat"`,
`outcome_labels`, `standardized`, the AME columns,
`show_columns = "none"` and a survey design are refused, each with what
to do instead; so are an exposure that is also an outcome or a
covariate, and a single outcome (use
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)).

## See also

[`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)
for one outcome and several candidate predictors,
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
for models fitted beforehand.

## Examples

``` r
# A numeric exposure: one row per outcome.
table_regression_outcomes(
  sochealth,
  outcomes   = c(wellbeing_score, bmi, life_sat_health),
  exposure   = age,
  covariates = sex
)
#> Linear regression of each outcome on age
#> 
#>  Outcome         │  N     B       95% CI        p   
#> ─────────────────┼──────────────────────────────────
#>  wellbeing_score │ 1200  0.04  [-0.02, 0.10]   .327 
#>  bmi             │ 1188  0.04  [ 0.02, 0.05]  <.001 
#>  life_sat_health │ 1192  0.00  [-0.00, 0.01]   .647 
#> 
#> Note. Linear regression models. One model per outcome, fitted separately. Adjusted for sex. Each model is fit on its own complete cases; N varies by outcome (1188–1200). Std. errors: classical (OLS). P-values adjusted across the 3 outcomes for 3 tests (Holm method). CI = confidence interval.

# A factor exposure: one block of levels per outcome.
table_regression_outcomes(
  sochealth,
  outcomes   = c(wellbeing_score, bmi),
  exposure   = employment_status,
  covariates = c(age, sex),
  labels     = TRUE
)
#> Linear regression of each outcome on Employment status
#> 
#>  Outcome                        │  N      B        95% CI        p   
#> ────────────────────────────────┼────────────────────────────────────
#>  WHO-5 wellbeing index (0-100): │ 1200                               
#>    Employed (ref.)              │         –          –          –    
#>    Student                      │        0.99  [-1.78,  3.75]  1.000 
#>    Unemployed                   │       -3.82  [-6.37, -1.27]   .020 
#>    Inactive                     │        0.14  [-2.83,  3.11]  1.000 
#>  Body mass index:               │ 1188                               
#>    Employed (ref.)              │         –          –          –    
#>    Student                      │       -0.07  [-0.74,  0.59]  1.000 
#>    Unemployed                   │        0.14  [-0.48,  0.75]  1.000 
#>    Inactive                     │        0.16  [-0.55,  0.88]  1.000 
#> 
#> Note. Linear regression models. One model per outcome, fitted separately. Adjusted for Age (years) and Sex. Each model is fit on its own complete cases; N varies by outcome (1188–1200). Std. errors: classical (OLS). P-values adjusted across the 2 outcomes for 6 tests (Holm method). CI = confidence interval.

# A binary outcome in a logistic model, beside two linear ones: its
# odds ratio next to their coefficients.
table_regression_outcomes(
  sochealth,
  outcomes     = c(wellbeing_score, bmi, smoking),
  exposure     = age,
  covariates   = sex,
  family       = list(smoking = binomial()),
  exponentiate = TRUE
)
#> Regression of each outcome on age
#> 
#>  Outcome             │  N    Estimate     95% CI        p   
#> ─────────────────────┼──────────────────────────────────────
#>  wellbeing_score (B) │ 1200      0.04  [-0.02, 0.10]   .327 
#>  bmi (B)             │ 1188      0.04  [ 0.02, 0.05]  <.001 
#>  smoking (OR)        │ 1175      1.01  [ 1.00, 1.01]   .327 
#> 
#> Note. wellbeing_score, bmi: linear regression; smoking: logistic regression. One model per outcome, fitted separately. Adjusted for sex. smoking: coefficients exponentiated and displayed as OR; CI bounds exponentiated. Each model is fit on its own complete cases; N varies by outcome (1175–1200). Std. errors: wellbeing_score, bmi: classical (OLS); smoking: classical (Fisher information). P-values adjusted across the 3 outcomes for 3 tests (Holm method). CI = confidence interval.
```
