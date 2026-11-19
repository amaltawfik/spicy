# Ordinal models in table_regression()

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reads the cumulative-link models of
[`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) and
[`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html), and the
cumulative-link mixed models of
[`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html). The
predictor coefficients fill the rows, as proportional odds ratios under
`exponentiate = TRUE` with the logit link. The cut-points follow them as
a `Thresholds` block of rows, and the partial-proportional-odds terms of
a `clm` fit as a `Non-proportional effects` block. A `clmm` fit adds the
`Random effects` block of the mixed models, described in
[table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md),
as does
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html)
with `family = glmmTMB::ordinal()`, which follows the rules of this page
for its thresholds. The ordinal fits of
[`survey::svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) and
[`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) take the same
`Thresholds` block.

## Usage

    table_regression(
      models,
      show_thresholds = TRUE,
      show_components = TRUE,
      ...  # the arguments of every class: see ?table_regression
    )

The other arguments are documented in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
the values they take for ordinal models are listed below.

## Engines

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Class | Engine | AME | Exponentiate | Blocks | vcov |
| `polr` | [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) | per category | OR (logit) | Thresholds | classical, CR1 |
| `clm` | [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) | per category | OR (logit) | Thresholds; Non-proportional effects | classical, CR1 (classical only with scale or nominal terms) |
| `clmm` | [`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html) | no | OR (logit) | Thresholds; Random effects | classical |

AME: whether the average-marginal-effects columns are available;
`per category` is the marginal effect on each probability \\P(Y = k)\\.
Exponentiate: what `exponentiate = TRUE` gives. Blocks: the labeled
blocks of rows the class adds below its coefficients. vcov: the values
of `vcov` the class accepts. The same rows are returned by
`subset(table_regression_models(), family == "Ordinal")`.

## What the table shows

**Coefficient rows.** One row per predictor coefficient, on the log-odds
scale under the logit link. Under `exponentiate = TRUE` they are
proportional odds ratios (logit), or hazard ratios of the grouped event
under the cloglog link (see `exponentiate` below).

**Thresholds.** The cut-points render as a `Thresholds` block, with B,
SE, CI and p like the predictor rows (`show_thresholds`). They are
reported on the log-odds scale and never exponentiated.

**Non-proportional effects.** The terms of a `clm` fit given in
`nominal =` (partial proportional odds) render as a
`Non-proportional effects` block, one coefficient per cut-point. The
covariate effects of `scale =` render as a `Scale effects` block
(`show_components`).

**AME.** Per category: the marginal effect of the predictor on each
\\P(Y = k)\\, for `polr` and `clm` fits.

**Fit statistics.** By default, `n`, McFadden's pseudo-R-squared and
`AIC`. See *R-squared of glm and categorical models* in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

## Arguments for this family

- `show_thresholds`:

  Logical. `TRUE` (default) displays the cut-points as a `Thresholds`
  block of rows below the predictors, carrying B, SE, CI and p like the
  predictor rows. `FALSE` collapses them to a one-line note instead.
  Thresholds are reported on the log-odds (B) scale and are never
  exponentiated. They follow the `polr` convention, link(P(Y \<= j)) =
  threshold - linear predictor: an
  [`rms::lrm`](https://rdrr.io/pkg/rms/man/lrm.html) fit, which models
  P(Y \>= j), shows its intercepts with the sign reversed, named `"3|4"`
  (displayed `"3 | 4"`) rather than `"y>=4"`. The rows are shown only
  when a coefficient column (`"b"` or `"beta"`) is in `show_columns`;
  otherwise the cut-points go to the note. Applies to `polr`, `clm`,
  `clmm`, `svyolr`, `glmmTMB` with the ordinal family, and
  [`rms::lrm`](https://rdrr.io/pkg/rms/man/lrm.html) with more than two
  outcome levels.

- `show_components`:

  Logical. `TRUE` (default) displays the `scale = ~` effects of a `clm`
  fit as a `Scale effects` block: the covariate effects on the log
  standard deviation of the latent response. They are never
  exponentiated: their exponential is a ratio of latent standard
  deviations, not an odds ratio. `FALSE` omits the block. The argument
  also governs the zero-inflation and hurdle blocks of the count models
  ([table_regression_counts](https://amaltawfik.github.io/spicy/reference/table_regression_counts.md)).

The arguments shared with other classes take these values for ordinal
models:

- `vcov`, `cluster`:

  `polr` and `clm`: `"classical"` and `"CR1"`. A `clm` fit with a scale
  or nominal component, and `clmm`: `"classical"` only. See *Standard
  errors, degrees of freedom and intervals*.

- `ci_method`:

  `"wald"` or `"profile"` for `polr` and `clm`: `"profile"` uses
  `confint.polr()` or `confint.clm()` for the predictor coefficients;
  the thresholds stay Wald. Only the bounds change: the estimate, SE,
  statistic and p-value remain Wald. A robust `vcov` takes precedence:
  its Wald intervals are used, with a warning. `clmm` takes `"wald"`
  only.

- `exponentiate`:

  Odds ratios under the logit link. Under the cloglog link, the
  displayed `HR` is `exp(-B)`, not `exp(B)`: the cumulative
  parametrization `cloglog P(Y <= j) = zeta_j - xB` places the hazard of
  the grouped event on `-B`, so `exp(B)` would be the reciprocal of the
  hazard ratio. The CI endpoints are negated and swapped accordingly,
  and the note states the convention. A cloglog `clm` with `nominal =`
  terms refuses `exponentiate`, because threshold-side coefficients have
  no covariate-HR reading. Other links (probit, cauchit, loglog) refuse
  it. A `clmm` fit with the cloglog link and a single random intercept
  refuses it too: on the current `ordinal`, `clmm()` fits such a model
  with the loglog link, and the reverse.

- `standardized`:

  Refused: an ordinal outcome has no scale to standardize.

- `show_columns`:

  The AME tokens give one effect per category for `polr` and `clm`;
  `clmm` refuses them.

## Standard errors, degrees of freedom and intervals

The coefficients and thresholds are tested by Wald z. Cluster-robust
variance is `"CR1"`, from
[`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html):
the CR0 sandwich scaled by \\G/(G-1)\\, `G` the number of clusters, the
one cluster estimator of these classes; `"CR0"`, `"CR2"` and `"CR3"` are
refused rather than mapped. The thresholds take their uncertainty from
the same clustered matrix. A `clm` fit with a scale or nominal component
has no estimating functions in `ordinal` for those parts, so no sandwich
can be built for it.

Hierarchical comparison (`nested = TRUE`). Nested `polr` or `clm` fits
are compared by likelihood-ratio test, `c("lrt_change", "p_change")`;
under `"CR1"` the change test is the Wald chi-square of the added
coefficients on the robust matrix. See *Hierarchical (nested) model
comparison* in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

## Fit statistics

`polr`, `clm` and an ordinal
[`rms::lrm`](https://rdrr.io/pkg/rms/man/lrm.html) default to
`c("nobs", "pseudo_r2_mcfadden", "aic")`. `"nobs"` and the BIC follow
the engine's [`stats::nobs()`](https://rdrr.io/r/stats/nobs.html), which
for a weighted `polr` or `clm` fit is the sum of the weights. The
deviance R-squared is refused for ordinal models, where it equals
McFadden's. Cumulative-link mixed models show the fit statistics of the
mixed models (n, groups, AIC, BIC), without an R-squared; see
[table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).

## Cumulative-link mixed models

[`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html) fits, and
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html)
fits with `family = glmmTMB::ordinal()`, keep the `Thresholds` block and
add the `Random effects` block described in
[table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).
Their inference is Wald only: the AME columns, a robust `vcov`,
`ci_method = "profile"` and `standardized` are refused, as is
`nested = TRUE` for `clmm`. The random-effects likelihood-ratio test of
the note refits its null model with the link actually fitted.

## What is refused, and why

- `HC*`, `"bootstrap"`, `"jackknife"`, and the `CR*` tokens other than
  `"CR1"`: `"CR1"` is the one robust estimator of these classes.

- A robust `vcov` for a `clm` fit with a scale or nominal component, and
  for the cumulative-link mixed models.

- `exponentiate = TRUE` under a link whose exponential is not a ratio,
  and in the cloglog cases above.

- `standardized`, for every class of this family.

- For the cumulative-link mixed models, the AME,
  `ci_method = "profile"`, and `nested = TRUE` for `clmm`.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
for the arguments shared by all classes;
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
for the registry of supported classes and the other families;
[table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md)
for the random-effects rows of `clmm` and of the ordinal family of
`glmmTMB`;
[table_regression_survey](https://amaltawfik.github.io/spicy/reference/table_regression_survey.md)
for [`survey::svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html);
the [Ordinal regression
tables](https://amaltawfik.github.io/spicy/articles/table-regression-ordinal.html)
article.

## Examples

``` r
if (requireNamespace("MASS", quietly = TRUE)) {
  fit <- MASS::polr(Sat ~ Infl + Type, weights = Freq,
                    data = MASS::housing, Hess = TRUE)
  print(table_regression(fit, exponentiate = TRUE))

  # Cut-points in the note instead of a block of rows.
  print(table_regression(fit, show_thresholds = FALSE))
}
#> Cumulative logit regression (proportional odds): Sat
#> 
#>  Variable        │   OR      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  Infl:           │                                      
#>    Low (ref.)    │     –     –          –          –    
#>    Medium        │    1.73  0.18  [ 1.41,  2.12]  <.001 
#>    High          │    3.45  0.43  [ 2.69,  4.41]  <.001 
#>  Type:           │                                      
#>    Tower (ref.)  │     –     –          –          –    
#>    Apartment     │    0.59  0.07  [ 0.47,  0.75]  <.001 
#>    Atrium        │    0.75  0.11  [ 0.55,  1.01]   .059 
#>    Terrace       │    0.36  0.05  [ 0.27,  0.49]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Thresholds:     │                                      
#>    Low | Medium  │   -0.67  0.12  [-0.90, -0.45]  <.001 
#>    Medium | High │    0.51  0.12  [ 0.28,  0.73]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1681                                 
#>  R² (McFadden)   │    0.04                              
#>  AIC             │ 3507.5                               
#> 
#> Note. Cumulative logit regression (proportional odds). Coefficients exponentiated and displayed as OR; SE on the OR scale (delta method); CI bounds exponentiated (asymmetric). Std. errors: Wald asymptotic (z). Thresholds: latent-scale category cut-points (log-odds scale, not exponentiated). CI = confidence interval; AIC = Akaike information criterion.
#> Cumulative logit regression (proportional odds): Sat
#> 
#>  Variable       │    B      SE       95% CI        p   
#> ────────────────┼──────────────────────────────────────
#>  Infl:          │                                      
#>    Low (ref.)   │     –     –          –          –    
#>    Medium       │    0.55  0.10  [ 0.34,  0.75]  <.001 
#>    High         │    1.24  0.13  [ 0.99,  1.48]  <.001 
#>  Type:          │                                      
#>    Tower (ref.) │     –     –          –          –    
#>    Apartment    │   -0.52  0.12  [-0.75, -0.29]  <.001 
#>    Atrium       │   -0.29  0.15  [-0.59,  0.01]   .059 
#>    Terrace      │   -1.01  0.15  [-1.31, -0.72]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n              │ 1681                                 
#>  R² (McFadden)  │    0.04                              
#>  AIC            │ 3507.5                               
#> 
#> Note. Cumulative logit regression (proportional odds). Thresholds: Low|Medium = -0.67, Medium|High = 0.51. Std. errors: Wald asymptotic (z). CI = confidence interval; AIC = Akaike information criterion.
```
