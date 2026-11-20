# Categorical (multinomial) models in table_regression()

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reads the baseline-category logit models of
[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) and the
discrete-choice models of
[`mlogit::mlogit()`](https://rdrr.io/pkg/mlogit/man/mlogit.html). A
single `multinom` fit sets predictors as rows and one column group per
non-reference outcome category, so the effect of a predictor can be
compared across equations along its row. An `mlogit` fit renders
per-alternative rows in two segments: the alternative-invariant
coefficients, then one section per non-reference alternative. Under
`exponentiate = TRUE` both give odds ratios against the reference
outcome or alternative.

## Usage

    table_regression(
      models,
      exponentiate = FALSE,
      show_columns = NULL,
      outcome_labels = NULL,
      ...  # the arguments of every class: see ?table_regression
    )

This family has no argument of its own. The other arguments are
documented in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
the values they take for categorical models are listed below.

## Engines

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Class | Engine | AME | Exponentiate | Blocks | vcov |
| `multinom` | [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) | per outcome | OR | per-outcome blocks | classical, CR1 |
| `mlogit` | [`mlogit::mlogit()`](https://rdrr.io/pkg/mlogit/man/mlogit.html) | no | OR | per-alternative rows | classical, CR1 |

AME: whether the average-marginal-effects columns are available;
`per outcome` is one effect per outcome category. Exponentiate: what
`exponentiate = TRUE` gives. Blocks: how the class lays out its
coefficient rows. vcov: the values of `vcov` the class accepts. The same
rows are returned by
`subset(table_regression_models(), family == "Categorical")`.

## What the table shows

**A single `multinom` model.** Predictors as rows, one column group per
non-reference outcome category (spanner = category name). The note
`Reference outcome: <level>.` names the base category (Stata's "base
outcome" line), and `outcome_labels` relabels the spanners (e.g.
`"Student vs Employed"`). The fit statistics print once, under the first
group. When per-category AMEs are requested, the reference category
appears as a last, AME-only group: its coefficient cells are empty,
since the reference has no equation, and its AMEs complete the zero sum
across categories. The default `show_columns` compacts to B, SE and p as
for several models; request atomic tokens to restore the intervals.

**Several `multinom` models, or `nested = TRUE`.** One row per
(category, predictor) pair: categories within models would need two
spanner levels. `tidy()` and `output = "long"` always return the long
form (`"<category>: <term>"` rows), whatever the display;
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
and `output = "data.frame"` follow the displayed table (one column set
per category), as they do for every other layout.

**An `mlogit` model.** Two segments, the presentation of Stata's
`asclogit`. The `Alternative-invariant` section holds the coefficients
of the alternative attributes, one row each, shared across alternatives.
Then one section per non-reference alternative collects its intercept
and its case-specific coefficients, each against the reference that the
note names (`Reference alternative: <level>.`). The `n` row counts
choice situations, not the rows of the long-format data.

**Fit statistics.** See *Fit statistics* below.

## Values of the shared arguments

- `exponentiate`:

  Odds ratios of each outcome against the reference outcome: the
  baseline-category logits are log-odds (Agresti 2002; SAS prints "Odds
  Ratio Estimates" under its generalized-logit link; Stata's
  `mlogit, rrr` labels the same quantity a relative-risk ratio).

- `show_columns`:

  The AME tokens give one effect per outcome category for `multinom`.
  They are refused for `mlogit`: marginaleffects computes predictions
  but not slopes for its one-row-per-choice data structure.

- `outcome_labels`:

  For a single `multinom` model, the labels of the category spanners:
  one per non-reference outcome category, unique, in model order. The
  AME-only group of the reference category, when shown, keeps its own
  name, and `FALSE` changes nothing, as there is no Outcome row to
  remove.

- `model_labels`:

  Refused for a single multinomial model: its column groups are outcome
  categories, relabeled by `outcome_labels`.

- `vcov`, `cluster`:

  `"classical"` and `"CR1"` for both classes. `cluster` has one entry
  per observation for `multinom` and one per choice situation for
  `mlogit`. See *Standard errors, degrees of freedom and intervals*.

- `nested`:

  `TRUE` compares nested `multinom` fits by likelihood-ratio test, the
  convention of `anova.multinom()`.

- `ci_method`:

  `"wald"` only.

- `standardized`:

  Refused.

## Standard errors, degrees of freedom and intervals

The coefficients are tested by Wald z. Cluster-robust `"CR1"` comes from
[`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html):
the CR0 sandwich scaled by \\G/(G-1)\\, `G` the number of clusters. For
`multinom` it needs sandwich \>= 3.1-2, which added its `estfun()`
method, and the AME columns follow it. Under `"CR1"`, a nested hierarchy
is tested by the Wald chi-square of the added coefficients on the robust
matrix.

## Fit statistics

A `multinom` fit defaults to `c("nobs", "pseudo_r2_mcfadden", "aic")`,
an `mlogit` fit to `c("nobs", "aic")`, its `n` counting choice
situations. See *R-squared of glm and categorical models* in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

## What is refused, and why

- `HC*` for `multinom`: a multi-equation model has no working residuals
  or hat values, so the HC meat cannot be formed.

- `HC*` for `mlogit`:
  [`sandwich::vcovHC()`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html)
  computes a result for it but mis-scales the meat for its per-chooser
  score structure.

- `"CR0"`, `"CR2"`, `"CR3"`, `"bootstrap"` and `"jackknife"`: `"CR1"` is
  the one robust estimator of these classes.

- The AME columns for `mlogit` (see `show_columns` above).

- `model_labels` for a single `multinom` model, `ci_method = "profile"`
  and `standardized`.

## References

Agresti, A. (2002). *Categorical data analysis* (2nd ed.). Wiley.
[doi:10.1002/0471249688](https://doi.org/10.1002/0471249688)

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
for the arguments shared by all classes;
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
for the registry of supported classes and the other families; the
[Multinomial regression
tables](https://amaltawfik.github.io/spicy/articles/table-regression-multinomial.html)
article.

## Examples

``` r
if (requireNamespace("nnet", quietly = TRUE)) {
  fit <- nnet::multinom(employment_status ~ age + sex,
                        data = sochealth, trace = FALSE)
  print(table_regression(fit, exponentiate = TRUE))
}
#> Multinomial logistic regression: employment_status
#> 
#>                          Student            Unemployed          Inactive      
#>                    ────────────────────  ─────────────────  ───────────────── 
#>  Variable        │   OR      SE     p     OR    SE     p     OR    SE     p   
#> ─────────────────┼────────────────────────────────────────────────────────────
#>  (Intercept)     │    0.30  0.10  <.001  0.25  0.08  <.001  0.13  0.05  <.001 
#>  age             │    0.99  0.01   .091  1.00  0.01   .453  1.00  0.01   .731 
#>  sex:            │                                                            
#>    Female (ref.) │     –     –     –      –     –     –      –     –     –    
#>    Male          │    1.12  0.20   .545  1.24  0.21   .197  1.15  0.23   .465 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1200                                                       
#>  R² (McFadden)   │    0.00                                                    
#>  AIC             │ 2540.1                                                     
#> 
#> Note. Multinomial logistic regression. Coefficients exponentiated and displayed as OR; SE on the OR scale (delta method). The (Intercept) row estimates the baseline odds. Reference outcome: Employed. Std. errors: classical (Fisher information). AIC = Akaike information criterion.
```
