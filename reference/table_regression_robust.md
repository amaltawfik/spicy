# Robust, IV, quantile and panel models in table_regression()

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reads the fits of
[`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html)
and
[`estimatr::iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.html),
the instrumental-variable and censored models of
[`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html) and
[`AER::tobit()`](https://rdrr.io/pkg/AER/man/tobit.html), the quantile
regressions of
[`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html), and the
fixed-effects estimators of `fixest` (`feols()`, `feglm()`, `fepois()`,
`femlm()`, `fenegbin()`). The `estimatr` and `fixest` fits carry the
variance they were computed with, and the table reports it unchanged.
[`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) has an
estimator family of its own, chosen through `vcov`. Absorbed fixed
effects render as a `Fixed effects:` block at the top of the fit
statistics.

## Usage

    table_regression(
      models,
      vcov = "classical",
      cluster = NULL,
      show_fit_stats = NULL,
      ...  # the arguments of every class: see ?table_regression
    )

This family has no argument of its own. The other arguments are
documented in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
the values they take for these models are listed below.

## Engines

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Class | Engine | AME | Exponentiate | Blocks | vcov |
| `lm_robust` | [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html) | yes | \- | \- | classical (the fit's own) |
| `iv_robust` | [`estimatr::iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.html) | yes | \- | \- | classical (the fit's own) |
| `ivreg` | [`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html) | yes | \- | \- | classical |
| `tobit` | [`AER::tobit()`](https://rdrr.io/pkg/AER/man/tobit.html) | yes | \- | \- | classical, CR1 |
| `rq` | [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) | yes | \- | \- | classical (= nid), nid, iid, ker, rank, bootstrap |
| `fixest` | [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html), [`fixest::feglm()`](https://lrberge.github.io/fixest/reference/feglm.html), [`fixest::fepois()`](https://lrberge.github.io/fixest/reference/feglm.html), [`fixest::femlm()`](https://lrberge.github.io/fixest/reference/femlm.html), [`fixest::fenegbin()`](https://lrberge.github.io/fixest/reference/femlm.html) | yes (no for a GLM with fixed effects) | OR / IRR (link) | \- | classical (the fit's own) |

AME: whether the average-marginal-effects columns are available.
Exponentiate: what `exponentiate = TRUE` gives. Blocks: the labeled
blocks of rows the class adds below its coefficients. vcov: the values
of `vcov` the class accepts. The same rows are returned by
`subset(table_regression_models(), family == "Robust, IV, quantile, panel")`.

The AME is refused for a `fixest` GLM with fixed effects (`feglm()`,
`fepois()`, `femlm()`, `fenegbin()`, whatever the family):
marginaleffects, which computes it, cannot take into account the
uncertainty of the fixed effects. It is refused too for a `feols()` with
fixed effects fitted with `lean = TRUE`, which marginaleffects cannot
predict from (re-estimate it with `lean = FALSE`). Other `feols()` fits
and a GLM without fixed effects have it. The AME of a factor in `i()`
sits on the rows of its coefficients, against the reference `i()` gives
them (`ref =`). A level of a factor in `i()`, or a numeric variable that
`i()` enters as categories, with no coefficient row gets no AME, nor
does a factor that `i()` enters only in an interaction.

## What the table shows

**Coefficient rows.** One row per coefficient, with the standard errors
the fit carries (`estimatr`, `fixest`) or those of the `vcov` requested
(`rq`). The note names the estimator.

**Fixed effects.** `fixest` fits, and `estimatr` fits built with
`fixed_effects =`, disclose their absorbed fixed effects as a
`Fixed effects:` block at the top of the fit statistics: one Yes / No
row per factor, blank for the models without the concept in a mixed
table. Varying-slope-only factors (`Origin[[x]]`) absorb no intercept:
they read No when another model absorbs that factor, and add no row
otherwise. The block is on by default (`"fixed_effects"` in
`show_fit_stats`).

**Fit statistics.** See *Fit statistics* below.

## Values of the shared arguments

- `vcov`, `cluster`:

  `estimatr`, `fixest` and `ivreg`: `"classical"` only, which for
  `estimatr` and `fixest` is the variance the fit carries. `tobit`:
  `"classical"` and `"CR1"`, as for `survreg`. `rq`: its own estimator
  family. See *Standard errors, degrees of freedom and intervals*.

- `show_fit_stats`:

  The defaults of each class, and the `"fixed_effects"`, `"within_r2"`
  and `"n_groups"` tokens; see *Fit statistics*.

- `exponentiate`:

  Odds or rate ratios for the GLM-family `fixest` fits, as the link
  says.

- `nested`:

  See *Standard errors, degrees of freedom and intervals*.

## Standard errors, degrees of freedom and intervals

**`estimatr` and `fixest`.** These fits carry the variance they were
computed with, estimatr's `se_type =` and whatever fixest's own `vcov`
interface produced, and spicy never overwrites it. A `vcov` request
other than `"classical"` is refused; set the estimator through the
fitting package instead: `se_type =` (with `clusters =`) for estimatr,
`vcov =` at estimation or in
[`summary()`](https://rdrr.io/r/base/summary.html) for fixest. The note
carries fixest's own label of the estimator (clustered, Newey-West,
Conley, ...), fixest's "IID" written as "Classical". With IID errors,
fixest's default small-sample correction
([`fixest::ssc()`](https://lrberge.github.io/fixest/reference/ssc.html))
multiplies the standard errors of a `feglm()` fit by sqrt((n - 1) / (n -
K)) over those of its [`glm()`](https://rdrr.io/r/stats/glm.html) twin
with the fixed effects as dummies (a `feols()` fit already matches
[`lm()`](https://rdrr.io/r/stats/lm.html)), and
`summary(fit, ssc = fixest::ssc(K.adj = FALSE))` gives the
[`glm()`](https://rdrr.io/r/stats/glm.html) values.

**[`quantreg::rq`](https://rdrr.io/pkg/quantreg/man/rq.html).** Its own
estimator family, not the sandwich vocabulary:

- `"classical"` resolves to the heteroskedasticity-robust `"nid"`
  sandwich (Hendricks-Koenker, Hall-Sheather bandwidth), quantreg's own
  large-sample default and the analog of Stata's `vce(robust)`.

- `"iid"`: the Koenker-Bassett homoskedastic estimator, as Stata's
  `vce(iid)`.

- `"ker"`: the Powell kernel sandwich.

- `"rank"`: rank-score inversion, which gives intervals only: the SE, t
  and p cells render as dashes, and the AME columns, which need a
  variance matrix, are refused.

- `"bootstrap"`: quantreg's native `boot.rq` (`bsmethod = "xy"`). With
  `cluster =`, the wild gradient cluster bootstrap of Hagemann (2017),
  the one cluster-robust route for `rq`. A weighted `rq` fit refuses it:
  the resamplers do not carry the weights.

`cluster` with any other `rq` estimator is refused.

**[`AER::tobit`](https://rdrr.io/pkg/AER/man/tobit.html).** A `tobit`
fit is a `survreg` model, and takes the same estimator: `"CR1"` from
[`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html),
the CR0 sandwich scaled by \\G/(G-1)\\, `G` the number of clusters. The
other `CR*` tokens and `HC*` are refused. Under `"CR1"`, a nested
hierarchy is tested by the Wald chi-square of the added coefficients on
the robust matrix. The RMST and risk-difference columns of `survreg` are
refused for it: a tobit models a censored response, not a time to event.

**Hierarchical comparison** (`nested = TRUE`). A quantile hierarchy gets
`c("f_change", "p_change")`, the Wald-type test
[`quantreg::anova.rq()`](https://rdrr.io/pkg/quantreg/man/anova.rq.html)
reports, under `"nid"` and `"rank"`; under `"iid"` and `"ker"` it is a
Wald F of the added coefficients, and under `"bootstrap"` a Wald
chi-square. A `fixest` hierarchy is compared by likelihood-ratio test; a
fit estimated with a non-iid vcov gets a Wald F when its coefficient
tests are t (`feols`, and `feglm` with a family whose dispersion is
estimated) and a Wald chi-square when they are z (`fepois`, `femlm`,
`fenegbin`, and `feglm` with the binomial or Poisson family). See
*Hierarchical (nested) model comparison* in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

## Fit statistics

`lm_robust` defaults to `c("nobs", "r2", "adj_r2")`, `iv_robust` to
`"nobs"` alone, and an IV
[`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html)
shows neither R-squared nor AIC: the 2SLS R-squared is not the classical
one. Asked for, it is the R-squared of `ivreg()`, on the structural
residuals. The AIC and BIC of a `fixest` model are those of its
[`lm()`](https://rdrr.io/r/stats/lm.html),
[`glm()`](https://rdrr.io/r/stats/glm.html) or
[`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) twin with
the fixed effects as dummies; see *Vocabulary tokens* in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

- `"fixed_effects"`: the `Fixed effects:` block, on by default.

- `"within_r2"`: the R-squared of the model with the fixed effects
  partialed out, a default for `feols` except an IV one, on request for
  `estimatr`. GLM-family `fixest` fits report fixest's McFadden `pr2`
  instead.

- `"n_groups"`: one `N (<factor>)` row per absorbed factor, on request.

Named in the call, `"fixed_effects"`, `"within_r2"` and `"n_groups"` are
refused when no model of the table absorbs fixed effects, a `fixest` fit
without `| fe` included. `"within_r2"` is refused too when no model of
the table has a value for it (an IV `feols()`, a GLM-family `fixest`
fit).

## What is refused, and why

- Every `vcov` other than `"classical"` for `estimatr` and `fixest`
  fits: the variance is chosen at fit time (see above).

- For `rq`: `HC*`, since quantile regression has no working residuals or
  hat values (`"nid"` is the heteroskedasticity-robust estimator here, a
  sparsity-based quantile sandwich, not an HC variant); `CR*`, since
  there is no cluster-robust sandwich for it (use `"bootstrap"` with
  `cluster =`); and `"jackknife"`, whose leave-one-out form is
  inconsistent for quantiles: the quantile objective is not
  differentiable, so dropping one observation can move the fit
  discontinuously.

- `nested = TRUE` across two `fixest` fits that absorbed different fixed
  effects. fixest counts every absorbed level as an estimated parameter,
  so a test across two `| fe` structures measures the change in the
  absorbed effects together with the change in the coefficients, and the
  two cannot be separated. Hold the `| fe` part fixed across the
  hierarchy.

- The AME of a `fixest` GLM with fixed effects, and of a `feols()` with
  fixed effects fitted with `lean = TRUE` (see *Engines*).

- A `fixest` fit with fixed effects only: it has no coefficient row to
  show.

## References

Hagemann, A. (2017). Cluster-robust bootstrap inference in quantile
regression models. *Journal of the American Statistical Association*,
112(517), 446-456.
[doi:10.1080/01621459.2016.1148610](https://doi.org/10.1080/01621459.2016.1148610)

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
for the arguments shared by all classes;
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
for the registry of supported classes and the other families; the
[Publication-ready regression
tables](https://amaltawfik.github.io/spicy/articles/table-regression.html)
article.

## Examples

``` r
if (requireNamespace("quantreg", quietly = TRUE)) {
  fit <- quantreg::rq(wellbeing_score ~ age + sex, tau = 0.5,
                      data = sochealth)
  print(table_regression(fit))

  # The homoskedastic estimator instead of the default "nid".
  print(table_regression(fit, vcov = "iid"))
}
#> Warning: Solution may be nonunique
#> Quantile regression (τ = 0.50): wellbeing_score
#> 
#>  Variable        │    B       SE       95% CI        p   
#> ─────────────────┼───────────────────────────────────────
#>  (Intercept)     │    65.63  1.98  [61.74, 69.52]  <.001 
#>  age             │     0.05  0.04  [-0.02,  0.12]   .187 
#>  sex:            │                                       
#>    Female (ref.) │      –     –          –          –    
#>    Male          │     3.96  1.09  [ 1.83,  6.09]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │  1200                                 
#>  AIC             │ 10082.1                               
#> 
#> Note. Quantile regression (τ = 0.50). Std. errors: Quantile sandwich (nid, Hall-Sheather). CI = confidence interval; AIC = Akaike information criterion.
#> Quantile regression (τ = 0.50): wellbeing_score
#> 
#>  Variable        │    B       SE       95% CI        p   
#> ─────────────────┼───────────────────────────────────────
#>  (Intercept)     │    65.63  1.89  [61.93, 69.34]  <.001 
#>  age             │     0.05  0.04  [-0.02,  0.12]   .168 
#>  sex:            │                                       
#>    Female (ref.) │      –     –          –          –    
#>    Male          │     3.96  1.04  [ 1.92,  6.00]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │  1200                                 
#>  AIC             │ 10082.1                               
#> 
#> Note. Quantile regression (τ = 0.50). Std. errors: Quantile (iid). CI = confidence interval; AIC = Akaike information criterion.
```
