# Count and two-part models in table_regression()

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reads the zero-inflated and hurdle models of
[`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) and
[`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html). The count
coefficients fill the rows, and the zero component follows them as a
labeled block of rows in the same columns. The zero-inflation, hurdle
and dispersion components of
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html)
render as the same blocks; the other rules of `glmmTMB` fits, random
effects included, are in
[table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).
Poisson and negative-binomial models fitted by
[`glm()`](https://rdrr.io/r/stats/glm.html) or
[`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) follow the
rules of
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
their dispersion rows are described below.

## Usage

    table_regression(
      models,
      show_components = TRUE,
      exponentiate = FALSE,
      ...  # the arguments of every class: see ?table_regression
    )

The other arguments are documented in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
the values they take for count models are listed below.

## Engines

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Class | Engine | AME | Exponentiate | Blocks | vcov |
| `zeroinfl` | [`pscl::zeroinfl()`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero-inflation | classical, CR1 (classical only for a negative-binomial part) |
| `hurdle` | [`pscl::hurdle()`](https://rdrr.io/pkg/pscl/man/hurdle.html) | yes (combined response) | IRR (count) + OR (logit zero part) | Zero hurdle | classical, CR1 (classical only for a negative-binomial part) |

AME: whether the average-marginal-effects columns are available; the AME
of a two-part model is the effect on the overall expected count E(Y).
Exponentiate: what `exponentiate = TRUE` gives. Blocks: the labeled
blocks of rows the class adds below its coefficients. vcov: the values
of `vcov` the class accepts. The same rows are returned by
`subset(table_regression_models(), family == "Counts, two-part")`.

## What the table shows

**Count rows.** One row per coefficient of the count component. Under
`exponentiate = TRUE` they are rate ratios of the count process: for
`zeroinfl`, the rate among units not in the structural-zero class; for
`hurdle`, the latent untruncated rate. They are not multiplicative
effects on the overall expected count E(Y).

**Zero component.** Two-part models show their full model, and the note
says what each block models:

- `zeroinfl`, and `glmmTMB` with `ziformula`: a `Zero-inflation` block,
  the model for the probability of a structural zero.

- `hurdle`: a `Zero hurdle, Pr(y > 0)` block, the model for the
  probability of a nonzero count, the opposite direction, hence the
  distinct label.

- A `glmmTMB` hurdle model (a zero-truncated family, `ziGamma()`,
  `lognormal()` or `beta_family()` with `ziformula`): a
  `Zero hurdle, Pr(y = 0)` block, the probability of a zero.

- `glmmTMB` with covariates in `dispformula`: a `Dispersion` block, on
  the log scale, never exponentiated.

The component rows carry Wald inference (B, SE, z, p, CI), join the
`p_adjust` family and take significance stars.

**AME.** The effect on the overall expected count E(Y), combining the
count and zero processes, computed by
[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)
on the full model. The zero-component rows carry no AME of their own,
since the combined effect includes them.

## Arguments for this family

- `show_components`:

  Logical. `TRUE` (default) displays the zero-inflation, zero-hurdle and
  dispersion components as labeled blocks of rows below the count
  coefficients. `FALSE` omits the blocks; the title still names the
  model type. The argument also governs the `Scale effects` block of an
  [`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) fit
  ([table_regression_ordinal](https://amaltawfik.github.io/spicy/reference/table_regression_ordinal.md)).

The arguments shared with other classes take these values for count
models:

- `exponentiate`:

  Rate ratios for the count rows. A zero component is exponentiated only
  when its link makes the result an odds ratio (the logit zero
  components). Probit, cauchit and cloglog zero links, and count-type
  hurdle zero parts, stay on the link scale, and the note says so.

- `vcov`, `cluster`:

  Poisson and geometric `zeroinfl` and `hurdle` fits: `"classical"` and
  `"CR1"`. A fit with a negative-binomial part: `"classical"` only.
  `glmmTMB`: `"classical"`, `"CR0"` and `"CR1"`. See *Standard errors,
  degrees of freedom and intervals*.

- `show_fit_stats`:

  The dispersion rows below.

## Standard errors, degrees of freedom and intervals

The coefficients of both components are tested by Wald z.

`"CR1"` for Poisson and geometric `pscl` fits comes from
[`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html),
which covers both components at once: the CR0 sandwich scaled by
\\G/(G-1)\\, `G` the number of clusters, the one cluster estimator of
these classes. A negative-binomial `zeroinfl` or `hurdle` keeps its
model-based standard errors: the sandwich of these fits leaves out the
score of theta. When the count part is negative binomial, fit the model
with `glmmTMB` instead (family `nbinom2` with `ziformula`, or
`truncated_nbinom2` for a hurdle), whose sandwich includes it; glmmTMB's
zero hurdle is binomial, as with `zero.dist = "binomial"`. A `hurdle`
whose zero part alone is negative binomial (`zero.dist = "negbin"`) is
refused too; refit it with the default `zero.dist = "binomial"`, which
keeps `"CR1"`.

`"CR0"` and `"CR1"` for `glmmTMB` fits come from glmmTMB's own sandwich
and cover the zero-inflation and dispersion blocks; see
[table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).

## Fit statistics

The dispersion of a negative-binomial fit is an opt-in fit-statistic
row: `"theta"` and `"alpha"` (= 1/theta, the Stata `nbreg` convention)
for NB2 fits, among them `zeroinfl` and `hurdle` with `dist = "negbin"`,
whose theta is that of the count part; `"delta"` for NB1 fits (glmmTMB
`nbinom1`). A Poisson column in the same table is blank on these rows.
See *Vocabulary tokens* in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
for the classes each token covers.

## What is refused, and why

- `HC*`: the `pscl` fits have no hat values, so the HC machinery of
  sandwich fails on them.

- A robust `vcov` for a `pscl` fit with a negative-binomial part (see
  above).

- `"CR0"`, `"CR2"`, `"CR3"`, `"bootstrap"` and `"jackknife"` for the
  `pscl` fits: `"CR1"` is their one robust estimator.

- `standardized`, for `zeroinfl` and `hurdle`.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
for the arguments shared by all classes;
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
for the registry of supported classes and the other families;
[table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md)
for `glmmTMB` fits; the [Count and two-part regression
tables](https://amaltawfik.github.io/spicy/articles/table-regression-counts.html)
article.

## Examples

``` r
if (requireNamespace("pscl", quietly = TRUE)) {
  data("bioChemists", package = "pscl")
  fit <- pscl::zeroinfl(art ~ fem + kid5 | kid5, data = bioChemists)
  print(table_regression(fit, exponentiate = TRUE))

  # Count rows only.
  print(table_regression(fit, show_components = FALSE))
}
#> Zero-inflated Poisson regression: art
#> 
#>  Variable        │   IRR     SE      95% CI       p   
#> ─────────────────┼────────────────────────────────────
#>  (Intercept)     │    2.50  0.11  [2.29, 2.72]  <.001 
#>  fem:            │                                    
#>    Men (ref.)    │     –     –         –         –    
#>    Women         │    0.75  0.04  [0.67, 0.84]  <.001 
#>  kid5            │    0.91  0.04  [0.84, 0.99]   .033 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Zero-inflation: │                                    
#>    (Intercept)   │    0.23  0.03  [0.17, 0.30]  <.001 
#>    kid5          │    1.16  0.17  [0.86, 1.56]   .322 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │  915                               
#>  AIC             │ 3341.9                             
#> 
#> Note. Zero-inflated Poisson regression. Coefficients exponentiated and displayed as IRR; SE on the IRR scale (delta method); CI bounds exponentiated (asymmetric). The (Intercept) row estimates the baseline rate. Std. errors: Wald asymptotic (z). Zero-inflation component: log-odds of a structural (excess) zero. Coefficients exponentiated and displayed as odds ratios. The (Intercept) row estimates the baseline odds. IRR = incidence rate ratio; CI = confidence interval; AIC = Akaike information criterion.
#> Zero-inflated Poisson regression: art
#> 
#>  Variable     │    B      SE       95% CI        p   
#> ──────────────┼──────────────────────────────────────
#>  (Intercept)  │    0.92  0.04  [ 0.83,  1.00]  <.001 
#>  fem:         │                                      
#>    Men (ref.) │     –     –          –          –    
#>    Women      │   -0.29  0.06  [-0.40, -0.17]  <.001 
#>  kid5         │   -0.09  0.04  [-0.18, -0.01]   .033 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n            │  915                                 
#>  AIC          │ 3341.9                               
#> 
#> Note. Zero-inflated Poisson regression. Std. errors: Wald asymptotic (z). CI = confidence interval; AIC = Akaike information criterion.
```
