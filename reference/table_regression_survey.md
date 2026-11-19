# Survey-design models in table_regression()

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reads the fits of a
[`survey::svydesign()`](https://rdrr.io/pkg/survey/man/svydesign.html)
or
[`survey::as.svrepdesign()`](https://rdrr.io/pkg/survey/man/as.svrepdesign.html)
design: [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html)
(and its replicate sibling `svrepglm`),
[`survey::svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html), and
[`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html)
(and `svrepcoxph`). They are read as design-based throughout: the
coefficients, the variance and the reference distribution all come from
survey. The note names the design, the number of primary sampling units
and the residual degrees of freedom.

## Usage

    table_regression(
      models,
      vcov = "classical",
      nested = FALSE,
      show_fit_stats = NULL,
      ...  # the arguments of every class: see ?table_regression
    )

This family has no argument of its own. The other arguments are
documented in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
the values they take for survey-design models are listed below.

## Engines

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Class | Engine | AME | Exponentiate | Blocks | vcov |
| `svyglm` | [`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) | yes (design-based) | OR / IRR | \- | classical (design-based) |
| `svyolr` | [`survey::svyolr()`](https://rdrr.io/pkg/survey/man/svyolr.html) | per category (design-based) | OR (logit) | Thresholds | classical (design-based) |
| `svycoxph` | [`survey::svycoxph()`](https://rdrr.io/pkg/survey/man/svycoxph.html) | no | HR | \- | classical (design-based) |

AME: whether the average-marginal-effects columns are available.
Exponentiate: what `exponentiate = TRUE` gives. Blocks: the labeled
blocks of rows the class adds below its coefficients. vcov: the values
of `vcov` the class accepts. The same rows are returned by
`subset(table_regression_models(), family == "Survey-weighted")`.

## What the table shows

**Coefficient rows.** One row per coefficient, as for the matching model
without a design: `svyglm` as `glm`, `svyolr` as
[`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) (with its
`Thresholds` block,
[table_regression_ordinal](https://amaltawfik.github.io/spicy/reference/table_regression_ordinal.md)),
`svycoxph` as
[`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)
([table_regression_survival](https://amaltawfik.github.io/spicy/reference/table_regression_survival.md)).

**AME.** The Horvitz-Thompson estimator: the mean unit-level effect
weighted by the sampling weights of the analytic sample, with its
variance from the delta method on the design vcov. It is tested on the
same distribution as the coefficient rows. `svyolr` gives one effect per
category.

**Counts.** Both are reported: the observed `n` and the `Weighted n` the
estimates describe. `svycoxph` adds the number of events.

## Values of the shared arguments

- `vcov`, `cluster`:

  `"classical"` only, which here is the design-based variance of the fit
  (Taylor linearization or replicates). Clustering belongs in the
  design, `survey::svydesign(ids = ~cluster, ...)`, not in the table
  call.

- `nested`:

  `FALSE` only. See *What is refused, and why*.

- `show_fit_stats`:

  By default `n` and `Weighted n`, with `AIC` for `svyglm` and the
  number of events for `svycoxph`. On request, `"eff_p"` and
  `"concordance"`; see *Fit statistics*.

- `exponentiate`:

  Odds or rate ratios for `svyglm` under the logit or log link, odds
  ratios for `svyolr` (logit), hazard ratios for `svycoxph`.

- `standardized`:

  Refused.

## Standard errors, degrees of freedom and intervals

Inference is Wald t at the degrees of freedom survey writes on the fit:
`df.residual` for `svyglm` and `svyolr`, `degf.resid` or `degf.residual`
for the two Cox engines. This is what
[`survey::regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html)
takes as its denominator. It is not `survey::degf(design)`, and it is
not re-derived here: the engines of survey do not share one expression
(a Cox fit carries `degf(design) - p + 1` although it has no intercept
for the `+ 1` to cancel, so it ends one above the two other classes).
The value is read off the object, and the note prints it. The interval
uses the same distribution, and so do the average marginal effects.

## Fit statistics

- `"aic"`: `svyglm` keeps an `AIC` row. survey's `extractAIC.svyglm`
  computes the design-based AIC of Lumley & Scott (2015).

- `"eff_p"`: the effective number of parameters of the design, the first
  element of survey's `extractAIC` (the sum of the Rao-Scott
  eigenvalues), reported beside the AIC on request.

- `"concordance"`: the concordance of a `svycoxph` fit goes to the note,
  or to a row on request.

## What is refused, and why

- Every model-derived variance (`HC*`, `CR*`, `"bootstrap"`,
  `"jackknife"`). The design is the variance authority: the fit's Taylor
  or replicate variance is already the robust variance for the declared
  design. The way to change the estimator is to change the design.

- `"bic"`, `"aicc"`, `"deviance"`, `"log_lik"`, the pseudo-R-squared
  tokens (`"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`,
  `"pseudo_r2_tjur"`) and `"r2_deviance"` for a table of design fits,
  and `"aic"` when no model of the table is a `svyglm`. A design-based
  fit is estimated without a likelihood, and survey's own
  [`deviance()`](https://rdrr.io/r/stats/deviance.html) returns a
  sign-flipped likelihood-ratio statistic on one Cox engine and a bare
  zero on the other. `BIC.svyglm` requires a maximal model and has no
  default.

- `nested = TRUE`. There is no likelihood to compare, so every change
  statistic would be empty, and a block of empty rows reads like an
  answer. Put the models side by side with `nested = FALSE`, and test a
  term under the design with
  [`survey::regTermTest()`](https://rdrr.io/pkg/survey/man/regTermTest.html).

- The AME for `svycoxph`, on the same ground as for a Cox model without
  a design
  ([table_regression_survival](https://amaltawfik.github.io/spicy/reference/table_regression_survival.md)).

- The `"rmst"` and `"risk_diff"` columns for `svycoxph`: their
  uncertainty comes from resampling subjects, which ignores the strata
  and clusters the design declares. Use
  [`survey::svykm()`](https://rdrr.io/pkg/survey/man/svykm.html) for a
  marginal curve.

- `standardized`.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
for the arguments shared by all classes;
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
for the registry of supported classes and the other families;
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
and
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md)
for descriptive tables of a design; the [Summary tables from a survey
design](https://amaltawfik.github.io/spicy/articles/survey-tables.html)
article.

## Examples

``` r
if (requireNamespace("survey", quietly = TRUE)) {
  data("api", package = "survey")
  des <- survey::svydesign(ids = ~dnum, weights = ~pw, data = apiclus1,
                           fpc = ~fpc)
  fit <- survey::svyglm(api00 ~ ell + meals, design = des)
  print(table_regression(fit))
}
#> Survey-weighted linear regression: api00
#> 
#>  Variable    │    B      SE         95% CI         p   
#> ─────────────┼─────────────────────────────────────────
#>  (Intercept) │  817.18  18.67  [776.50, 857.86]  <.001 
#>  ell         │   -0.51   0.33  [ -1.22,   0.20]   .144 
#>  meals       │   -3.15   0.30  [ -3.80,  -2.49]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │  183                                    
#>  Weighted n  │ 6194                                    
#>  AIC         │ 2002.2                                  
#> 
#> Note. Survey-weighted linear regression. Design: cluster (dnum), 15 PSU, with finite population correction; 12 residual degrees of freedom. Std. errors: Design-based (Taylor linearization). CI = confidence interval; AIC = Akaike information criterion.
```
