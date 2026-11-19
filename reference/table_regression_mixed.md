# Mixed-effects models in table_regression()

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reads the mixed-effects fits of
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html),
[`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html),
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) and
[`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html), and the
generalized least squares fits of
[`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html). The fixed
effects fill the coefficient rows. The variance components follow them
as a `Random effects` block of rows, in the same columns. The number of
groups, the ICC and the marginal and conditional R-squared of Nakagawa
are fit-statistic rows, and the note gives the likelihood-ratio test of
the whole random part. Cumulative-link mixed models
([`ordinal::clmm()`](https://rdrr.io/pkg/ordinal/man/clmm.html), and
`glmmTMB()` with `family = glmmTMB::ordinal()`) get the same block of
rows; their other rules are those of the ordinal models, in
[table_regression_ordinal](https://amaltawfik.github.io/spicy/reference/table_regression_ordinal.md).

## Usage

    table_regression(
      models,
      show_re = TRUE,
      re_scale = c("sd", "variance"),
      re_columns = c("est", "se", "ci"),
      re_test = c("none", "lrt", "rlrt"),
      re_ci = c("wald", "profile"),
      ...  # the arguments of every class: see ?table_regression
    )

The other arguments are documented in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md);
the values they take for mixed models are listed below.

## Engines

|  |  |  |  |  |  |
|----|----|----|----|----|----|
| Class | Engine | AME | Exponentiate | Blocks | vcov |
| `lmerMod` | [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) | yes | \- | Random effects | classical, CR0-CR3 |
| `glmerMod` | [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html) | yes | OR / IRR (link) | Random effects | classical |
| `glmmTMB` | [`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html) | yes (no for the ordinal family) | link-dependent (IRR for count families, OR for ordinal logit) | Random effects; Zero-inflation; Zero hurdle, Pr(y = 0) for a zero-truncated family, ziGamma, lognormal or beta_family; Dispersion; Thresholds (ordinal family) | classical, CR0, CR1 (classical only for REML or the ordinal family) |
| `lme` | [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) | yes | \- | Random effects | classical, CR0-CR3 |
| `gls` | [`nlme::gls()`](https://rdrr.io/pkg/nlme/man/gls.html) | yes | \- | \- | classical |

AME: whether the average-marginal-effects columns are available.
Exponentiate: what `exponentiate = TRUE` gives. Blocks: the labeled
blocks of rows the class adds below its coefficients. vcov: the values
of `vcov` the class accepts. The same rows are returned by
`subset(table_regression_models(), family == "Mixed effects")`.

## What the table shows

**Coefficient rows.** One row per fixed-effect coefficient, as for `lm`
and `glm`. Under `exponentiate = TRUE` a `glmer` or `glmmTMB` fit
follows the link rules of `glm`: odds ratios under the logit link, rate
ratios under the log link, an error for a link whose exponential is not
a ratio. The rows are conditional on a random effect of zero: an
exponentiated intercept is the baseline odds or rate of a group whose
random effect is zero, not the average over groups.

**Random-effects rows.** One row per standard deviation and per
correlation of each grouping factor, then the residual of a Gaussian
model, each with its estimate, SE and CI in the coefficient columns. The
rows of a `glmer` or `glmmTMB` fit are on the scale of the linear
predictor (log-odds, log-rate). The rows carry no p-value: a Wald test
of a variance is invalid at the boundary of its parameter space (Self &
Liang 1987), and `re_test` adds a test that is valid there. The note
gives the estimation method (REML or ML) and the likelihood-ratio test
of the whole random part against the model without random effects, with
its boundary-corrected chi-bar-squared p-value (Self & Liang 1987; Stram
& Lee 1994). For an `lme` fit the null model keeps the residual variance
and correlation structure of the fit. A `gls` fit has no random effects
and no block. `glmmTMB` zero-inflation, hurdle and dispersion components
render as further blocks, described in
[table_regression_counts](https://amaltawfik.github.io/spicy/reference/table_regression_counts.md)
(`show_components`).

In [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) the
variance-component rows have `estimate_type = "vc"`, their terms are
prefixed `re::`, and the correlations carry a `::cor` suffix.

**Fit statistics.** By default, `n`, one `N (<group>)` row per grouping
factor (`"n_groups"`), the `ICC`, the marginal and conditional
R-squared, `AIC` and `BIC`; see *Fit statistics* below.

## Arguments for this family

These arguments act on the random-effects rows only. They have no effect
on fits without random effects.

- `show_re`:

  Logical. `TRUE` (default) renders the `Random effects` block for
  `lmer`, `glmer`, `glmmTMB`, `lme` and `clmm` fits. `FALSE` suppresses
  it. `re_test` and `re_ci = "profile"` need the block and are refused
  with `show_re = FALSE`.

- `re_scale`:

  `"sd"` (default) or `"variance"`. `"sd"` reports the standard
  deviation \\\sigma\\, in the units of the linear predictor (those of
  the outcome for a Gaussian model). `"variance"` reports \\\sigma^2\\,
  in squared units. The standard error is a delta-method one on the
  scale the engine estimates on, and \\SE(\sigma) = SE(\sigma^2) /
  (2\sigma)\\. For `lmer` and `glmer` it comes from the covariance
  matrix of the variances (merDeriv). For `lme`, `glmmTMB` and `clmm`
  the engine estimates \\\log\sigma\\ and the SE is \\\sigma \\
  SE(\log\sigma)\\. The Wald CI of every engine is built on
  \\\log\sigma\\, \\\sigma \exp(\pm z \\ SE(\log\sigma))\\, and squared
  for \\\sigma^2\\: by spicy for `lmer` and `glmer`, by the engine
  otherwise
  ([`nlme::intervals()`](https://rdrr.io/pkg/nlme/man/intervals.html),
  `glmmTMB::confint()`). The residual row of a Gaussian `glmmTMB` fit
  has its SE and CI too. Correlation rows (\\\rho\\) have no unit and
  are unchanged. Their SE is a delta-method one too: from the variances
  for `lmer` (CI \\\rho \pm z \\ SE\\, clamped to \\\[-1, 1\]\\), from
  \\\mathrm{atanh}(\rho)\\ for `lme`, and from glmmTMB's \\\rho /
  \sqrt{1 - \rho^2}\\ in a two-term block, with the engine's CI. In a
  `glmmTMB` block of three terms or more it is the width of glmmTMB's
  interval divided by \\2z\\.

- `re_columns`:

  Subset of `c("est", "se", "ci")`: the cells of the random-effects rows
  that are displayed (`"est"` is mandatory). Deselected cells render as
  an en dash on those rows only: `re_columns = "est"` for estimates
  alone, `c("est", "se")` for a journal that wants standard errors only.
  Display only:
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
  keep the SE and CI.

- `re_test`:

  `"none"` (default), `"lrt"` or `"rlrt"`. A per-term test of the
  variance components, which fills the p column of the random-effects
  rows. Both are likelihood-ratio tests; a Wald test of a variance is
  refused (see *What is refused, and why*).

  - `"lrt"`: likelihood-ratio test of each random term against the model
    refitted without it (its variance and its covariances with the other
    terms of its bar), referred to the chi-bar-squared mixture
    `0.5 chi2(q-1) + 0.5 chi2(q)` (Self & Liang 1987; Stram & Lee 1994).
    The reduction scheme is that of
    [`lmerTest::ranova()`](https://rdrr.io/pkg/lmerTest/man/ranova.html),
    and the mixture makes the p-value asymptotically exact instead of
    conservative. The intercept of a bar is tested only when it is the
    only term of the bar. Available for `lmer`, `glmer`, `glmmTMB`, and
    `lme` with a simple `random = ~ terms | group` structure.

  - `"rlrt"`: exact restricted likelihood-ratio test with a simulated
    finite-sample null
    ([`RLRsim::exactRLRT()`](https://rdrr.io/pkg/RLRsim/man/exactRLRT.html);
    Crainiceanu & Ruppert 2004). Defined only for a Gaussian `lmer` or
    `lme` fit with a single variance component; other fits are refused
    with a pointer to `"lrt"`.

  The statistic and its df stay out of the t/z column (they are on the
  chi-square scale) and are kept in
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  (`test_type` `"chibar2"` or `"rlrt"`). The test of the whole block in
  the note is unchanged. Correlation and residual rows are never tested.
  Each random term costs one refit; a refit that fails leaves its row
  untested, with a `spicy_fallback` warning. For other structures,
  [`pbkrtest::PBmodcomp()`](https://rdrr.io/pkg/pbkrtest/man/pb__modcomp.html)
  gives a parametric-bootstrap test of the same pair of fits (Halekoh &
  Højsgaard 2014), to report beside the table.

- `re_ci`:

  `"wald"` (default) or `"profile"`: the uncertainty of the
  random-effects rows of `lmer` and `glmer` fits.

  - `"wald"`: SE from the information matrix (merDeriv, expected
    information by default) and a Wald CI symmetric on the log of the
    standard deviation, which stays above 0 (see `re_scale`). Wald can
    be optimistic near the boundary of a variance; `"profile"` respects
    it. Its cost grows faster than the number of observations (about a
    minute at n of roughly 2,700). Above `options("spicy.re_se_max_n")`
    (default `1000`) the SE and CI are skipped: the rows keep their
    estimates, the cells show an en dash, a note states the omission and
    a `spicy_caveat` warning is raised. Raise the cap
    (`options(spicy.re_se_max_n = Inf)`) to force the computation, or
    use `re_ci = "profile"`.

  - `"profile"`: profile-likelihood intervals from
    `confint(fit, method = "profile")`, the route lme4 documents. They
    are asymmetric and respect the boundary at 0. No SE is shown, since
    a symmetric SE misdescribes the skewed sampling distribution of a
    variance. There is no size cap (about two seconds per variance
    parameter at n of 7,000; more for `glmer`). The note names the
    method, and `re_scale = "variance"` shows the profile interval of
    the variance itself.

  `glmmTMB`, `lme` and `clmm` fits keep the intervals of their own
  engine under `"wald"` (see `re_scale`) and refuse `"profile"`.

The arguments shared with other classes take these values for mixed
models:

- `vcov`, `cluster`:

  `lmer` and `lme`: `"classical"` and `"CR0"` to `"CR3"`. `glmmTMB`:
  `"classical"`, `"CR0"` and `"CR1"`. `glmer` and `gls`: `"classical"`
  only. See *Standard errors, degrees of freedom and intervals*.

- `ci_method`:

  `"wald"` only. `"profile"` is defined for `glm` and the ordinal
  models, and `"boot_percentile"` needs `vcov = "bootstrap"`, which
  mixed models refuse. The intervals of the random-effects rows are set
  by `re_ci`.

- `standardized`:

  `"refit"`: the model is refitted on z-scored data with its random
  structure, the one approach that is well posed for a mixed model,
  since the marginal SD(y) has no unique decomposition across levels.
  `"posthoc"`, `"basic"`, `"smart"` and `"pseudo"` fall back to
  `"refit"` with a `spicy_fallback` warning. The \\\beta\\ rows take the
  reference distribution of their B rows, so B and \\\beta\\ carry the
  same p-value, and a robust `vcov` is applied to the refit too. A
  formula with an inline transform (`log(x)`,
  [`poly()`](https://rdrr.io/r/stats/poly.html)) cannot be refitted: the
  \\\beta\\ rows are left out, with a warning; build the transformed
  column in `data` before fitting. Refused for `glmmTMB` with the
  ordinal family, whose outcome has no scale.

- `show_columns`:

  `"partial_chi2"` is the Wald chi-square of each term, Type II by
  default and Type III under `partial_type = "III"` (the type SAS PROC
  MIXED and `lmerTest` report; the two agree for a model without
  interactions). It follows the robust `vcov` when one is set.
  `"partial_f"` and the variance-explained partials (`"partial_f2"`,
  `"partial_eta2"`, `"partial_omega2"`) are not defined. The AME tokens
  are available for every engine of the table above except `glmmTMB`
  with the ordinal family; no column token is reserved to mixed models.

- `show_fit_stats`:

  Four tokens belong to this family: `"n_groups"` (also used by GEE and
  `fixest` fits), `"icc"`, `"r2_marginal"` and `"r2_conditional"`. See
  *Fit statistics*.

## Standard errors, degrees of freedom and intervals

The fixed effects are tested as each engine tests them:

- A fit of
  [`lmerTest::lmer()`](https://rdrr.io/pkg/lmerTest/man/lmer.html): t
  with Satterthwaite degrees of freedom.

- A plain [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) fit:
  Wald z. A message, once per session, suggests refitting with
  [`lmerTest::lmer()`](https://rdrr.io/pkg/lmerTest/man/lmer.html); the
  note names the method only.

- `glmer` and `glmmTMB`: Wald z.

- `lme`: t with the containment degrees of freedom of `nlme`, named in
  the note. `gls`: t on n minus the number of coefficients.

The confidence interval uses the same reference distribution as the
test. The AME rows take the variance matrix of the coefficient rows,
model-based or robust, and are tested by Wald z. Beside the t rows of an
`lmerTest` or `lme` fit, or of a `CR*` variance, an identity-link AME
can therefore carry another p-value than the B of the same row; read it
as a large-sample approximation.

Cluster-robust variance. `lmer` and `lme` fits take `"CR0"` to `"CR3"`
from
[`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md),
with Satterthwaite degrees of freedom computed from the robust matrix.
An `lme` fitted with other contrasts than those of the session is
refused: clubSandwich rebuilds its design with the session contrasts, so
its variance would not match the coefficients. `glmmTMB` fits take
`"CR0"` and `"CR1"` from glmmTMB's own sandwich (glmmTMB \>= 1.1.15),
which covers the coefficients, the zero-inflation and dispersion blocks
and the AME. `"CR0"` is the sandwich as glmmTMB computes it, with one
score per cluster (the gradient of the marginal log-likelihood of the
cluster); `"CR1"` scales it by \\G/(G-1)\\, `G` the number of clusters.
Both use Wald z. Without `cluster`, the cluster is the top-level
grouping factor of the random effects. The estimator needs independent
clusters that contain the random effects, every group within one
cluster, so crossed random effects need a `cluster` that contains them
all. It keeps the standard errors valid when the random-effects
distribution or the conditional variance is misspecified, provided the
estimates stay consistent (Liang & Zeger 1986 for a misspecified
variance and correlation, Verbeke & Lesaffre 1997 for the random-effects
distribution). With few clusters it understates the variance, which the
factor of `"CR1"` offsets only in part: this class has no bias-reduced
correction (Cameron & Miller 2015).

The robust estimator covers the fixed effects: their coefficients, their
joint tests (`"partial_chi2"`), the standardized coefficients and the
AME. The variance components keep their model-based inference. glmmTMB's
sandwich takes one likelihood score per cluster over the fixed and
variance parameters, while the `CR*` estimators of `lmer` and `lme` come
from clubSandwich over the fixed effects alone: on the same model the
two agree as the clusters grow, but not exactly.

Hierarchical comparison (`nested = TRUE`). The default change rows are
`c("aic_change", "bic_change", "lrt_change", "p_change")` (Pinheiro &
Bates 2000, Section 2.4.1), from the engine's own two-model
[`anova()`](https://rdrr.io/r/stats/anova.html). Under `"CR0"` to
`"CR3"`, `lmer` and `lme` hierarchies use the HTZ test of
[`clubSandwich::Wald_test()`](http://jepusto.github.io/clubSandwich/reference/Wald_test.md);
under `"CR0"` and `"CR1"`, a `glmmTMB` hierarchy uses a Wald chi-square.
See *Hierarchical (nested) model comparison* in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

## Fit statistics

By default,
`c("nobs", "n_groups", "icc", "r2_marginal", "r2_conditional", "aic", "bic")`,
for `lmer`, `glmer`, `glmmTMB`, `lme` and `clmm` fits. A row of the
default that no model of the table fills is left out. A token named in
the call that no model fills is refused. A `gls` fit gets `n` and `AIC`.

- `"n_groups"`: one `N (<group>)` row per grouping factor.

- `"icc"`: the intraclass correlation of a model with one random
  intercept and no other random term: \\\sigma^2_u / (\sigma^2_u +
  \sigma^2_e)\\. For a non-Gaussian `glmer` fit the residual variance is
  the distribution variance of the link scale (the adjusted ICC of
  Nakagawa, Johnson & Schielzeth 2017): \\\pi^2/3\\ for the logit, 1 for
  the probit, \\\pi^2/6\\ for the cloglog, and \\\log(1 + 1/\lambda)\\
  for a Poisson log fit. A binomial
  [`cbind()`](https://rdrr.io/r/base/cbind.html) response, whose rows
  are not single trials, has no ICC. For `glmmTMB` the ICC is computed
  for the Gaussian identity family only. A model with random slopes has
  no ICC: its cell is empty. With several grouping factors there is no
  single ICC, and the note says so.

- `"r2_marginal"`, `"r2_conditional"`: the R-squared of Nakagawa &
  Schielzeth (2013), the variance explained by the fixed effects alone
  and by the fixed and random effects together; the values match
  [`performance::r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.html).
  Empty for `gls`.

- On request: `"deviance"`, `"log_lik"` (the restricted log-likelihood
  of a REML fit), `"sigma"` (see *Vocabulary tokens* in
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  for its `glmmTMB` families), and the dispersion rows of a
  negative-binomial or beta family (`"theta"`, `"alpha"`, `"delta"`,
  `"phi"`).

- Refused when every model of the table is mixed: `"r2"`, `"adj_r2"`,
  `"omega2"`, `"f2"`, the pseudo-R-squared tokens and `"r2_deviance"`.
  They have no definition for a mixed model; the error points to
  `"r2_marginal"` and `"r2_conditional"`.

## What is refused, and why

- `HC*`, `"bootstrap"` and `"jackknife"` for every mixed engine: these
  estimators assume independent observations or refit `lm` and `glm`,
  and the random effects exist to model the dependence.

- `CR*` for `glmer` (clubSandwich has no working backend for it), and
  `"CR2"` and `"CR3"` for `glmmTMB` (its sandwich has one score per
  cluster, with no residuals and no hat values). A robust `vcov` for a
  `glmmTMB` fit by REML or with the ordinal family.

- A Wald test of a variance component, on any engine: the null value 0
  lies on the boundary of the parameter space.

- `re_ci = "profile"` outside lme4, and `re_test = "rlrt"` outside a
  Gaussian fit with one variance component.

- Under `nested = TRUE`:

  - A REML pair whose fixed effects differ
    (`nlme::lme(method = "REML")`, `glmmTMB(REML = TRUE)`). The
    restricted likelihood depends on the fixed-effects design, so the
    likelihood-ratio test is not valid (Pinheiro & Bates 2000, Section
    2.4.2). Refit both by maximum likelihood. A REML pair that differs
    only in its random structure stays valid.

  - Fits from different engines. No engine's
    [`anova()`](https://rdrr.io/r/stats/anova.html) accepts a fit of
    another; refit one with the other's engine.

  - The variance-explained change tokens (`"r2_change"`,
    `"adj_r2_change"`, `"f_change"`, `"f2_change"`). The comparison is a
    chi-square.

  `nested = FALSE` sets the models side by side instead.

## References

Cameron, A.C. & Miller, D.L. (2015). A practitioner's guide to
cluster-robust inference. *Journal of Human Resources*, 50(2), 317-372.

Crainiceanu, C.M. & Ruppert, D. (2004). Likelihood ratio tests in linear
mixed models with one variance component. *Journal of the Royal
Statistical Society B*, 66(1), 165-185.

Halekoh, U. & Højsgaard, S. (2014). A Kenward-Roger approximation and
parametric bootstrap methods for tests in linear mixed models: The R
package pbkrtest. *Journal of Statistical Software*, 59(9).
[doi:10.18637/jss.v059.i09](https://doi.org/10.18637/jss.v059.i09)

Liang, K.-Y. & Zeger, S.L. (1986). Longitudinal data analysis using
generalized linear models. *Biometrika*, 73(1), 13-22.

Nakagawa, S. & Schielzeth, H. (2013). A general and simple method for
obtaining R2 from generalized linear mixed-effects models. *Methods in
Ecology and Evolution*, 4(2), 133-142.

Nakagawa, S., Johnson, P.C.D. & Schielzeth, H. (2017). The coefficient
of determination R2 and intra-class correlation coefficient from
generalized linear mixed-effects models revisited and expanded. *Journal
of the Royal Society Interface*, 14(134), 20170213.
[doi:10.1098/rsif.2017.0213](https://doi.org/10.1098/rsif.2017.0213)

Pinheiro, J.C. & Bates, D.M. (2000). *Mixed-effects models in S and
S-PLUS*. Springer.

Self, S.G. & Liang, K.-Y. (1987). Asymptotic properties of maximum
likelihood estimators and likelihood ratio tests under nonstandard
conditions. *Journal of the American Statistical Association*, 82(398),
605-610.

Stram, D.O. & Lee, J.W. (1994). Variance components testing in the
longitudinal mixed effects model. *Biometrics*, 50(4), 1171-1177.

Verbeke, G. & Lesaffre, E. (1997). The effect of misspecifying the
random-effects distribution in linear mixed models for longitudinal
data. *Computational Statistics & Data Analysis*, 23(4), 541-556.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
for the arguments shared by all classes;
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
for the registry of supported classes and the other families;
[table_regression_ordinal](https://amaltawfik.github.io/spicy/reference/table_regression_ordinal.md)
for the thresholds of `clmm` and of the ordinal family of `glmmTMB`;
[table_regression_counts](https://amaltawfik.github.io/spicy/reference/table_regression_counts.md)
for the zero-inflation, hurdle and dispersion blocks of `glmmTMB`; the
[Mixed-effects regression
tables](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.html)
article.

## Examples

``` r
# \donttest{
if (requireNamespace("lme4", quietly = TRUE)) {
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject),
                    data = lme4::sleepstudy)
  print(table_regression(fit))

  # Variances instead of standard deviations, estimates only.
  print(table_regression(fit, re_scale = "variance", re_columns = "est"))

  # Nested comparison, fitted by maximum likelihood.
  m1 <- lme4::lmer(Reaction ~ 1 + (1 | Subject),
                   data = lme4::sleepstudy, REML = FALSE)
  m2 <- lme4::lmer(Reaction ~ Days + (1 | Subject),
                   data = lme4::sleepstudy, REML = FALSE)
  print(table_regression(list(m1, m2), nested = TRUE))
}
#> P-values of the `lme4::lmer()` model are Wald z-tests, a large-sample approximation.
#> ℹ For Satterthwaite t-tests, which suit models with few groups, refit with `lmerTest::lmer()` (install lmerTest if needed).
#> This message is displayed once per session.
#> Registered S3 method overwritten by 'merDeriv':
#>   method        from        
#>   bread.lmerMod clubSandwich
#> Linear mixed-effects regression: Reaction
#> 
#>  Variable                        │    B      SE        95% CI         p   
#> ─────────────────────────────────┼────────────────────────────────────────
#>  (Intercept)                     │  251.41  6.82  [238.03, 264.78]  <.001 
#>  Days                            │   10.47  1.55  [  7.44,  13.50]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:                 │                                        
#>    σ Subject (Intercept)         │   24.74  5.84  [ 15.58,  39.28]   –    
#>    σ Subject Days                │    5.92  1.25  [  3.92,   8.95]   –    
#>    ρ Subject ((Intercept), Days) │    0.07  0.33  [ -0.57,   0.70]   –    
#>    σ (Residual)                  │   25.59  1.51  [ 22.80,  28.72]   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                               │  180                                   
#>  N (Subject)                     │   18                                   
#>  R² (marginal)                   │    0.28                                
#>  R² (conditional)                │    0.80                                
#>  AIC                             │ 1755.6                                 
#>  BIC                             │ 1774.8                                 
#> 
#> Note. Linear mixed-effects regression. Std. errors: Wald (model-based). P-values: Wald z-test, large-sample approximation. Random effects (REML): LR test vs linear regression, χ̄²(3) = 150.04, p < .001. CI = confidence interval; AIC = Akaike information criterion; BIC = Bayesian information criterion.
#> Linear mixed-effects regression: Reaction
#> 
#>  Variable                        │    B      SE        95% CI         p   
#> ─────────────────────────────────┼────────────────────────────────────────
#>  (Intercept)                     │  251.41  6.82  [238.03, 264.78]  <.001 
#>  Days                            │   10.47  1.55  [  7.44,  13.50]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:                 │                                        
#>    σ² Subject (Intercept)        │  612.10   –           –           –    
#>    σ² Subject Days               │   35.07   –           –           –    
#>    ρ Subject ((Intercept), Days) │    0.07   –           –           –    
#>    σ² (Residual)                 │  654.94   –           –           –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                               │  180                                   
#>  N (Subject)                     │   18                                   
#>  R² (marginal)                   │    0.28                                
#>  R² (conditional)                │    0.80                                
#>  AIC                             │ 1755.6                                 
#>  BIC                             │ 1774.8                                 
#> 
#> Note. Linear mixed-effects regression. Std. errors: Wald (model-based). P-values: Wald z-test, large-sample approximation. Random effects (REML): LR test vs linear regression, χ̄²(3) = 150.04, p < .001. CI = confidence interval; AIC = Akaike information criterion; BIC = Bayesian information criterion.
#> Hierarchical linear mixed-effects regression: Reaction
#> 
#>                                  Model 1                 Model 2          
#>                            ────────────────────  ──────────────────────── 
#>  Variable                │    B      SE     p         B        SE     p   
#> ─────────────────────────┼────────────────────────────────────────────────
#>  (Intercept)             │  298.51  8.79  <.001   251.41      9.51  <.001 
#>  Days                    │                         10.47      0.80  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:         │                                                
#>    σ Subject (Intercept) │   34.59  6.72   –       36.01      6.45   –    
#>    σ (Residual)          │   44.26  2.46   –       30.90      1.72   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                       │  180                   180                     
#>  N (Subject)             │   18                    18                     
#>  ICC                     │    0.38                  0.58                  
#>  R² (marginal)           │    0.00                  0.29                  
#>  R² (conditional)        │    0.38                  0.70                  
#>  AIC                     │ 1916.5                1802.1                   
#>  BIC                     │ 1926.1                1814.9                   
#>  ΔAIC                    │     –                 -114.5                   
#>  ΔBIC                    │     –                 -111.3                   
#>  Δχ²                     │     –                  116.46 (1)              
#>  p (change)              │     –                    <.001                 
#> 
#> Note. Linear mixed-effects regression models. Std. errors: Wald (model-based). P-values: Wald z-test, large-sample approximation. Model 1: Random effects (ML): LR test vs linear regression, χ̄²(1) = 50.51, p < .001. Model 2: Random effects (ML): LR test vs linear regression, χ̄²(1) = 106.21, p < .001. ICC = intraclass correlation coefficient; AIC = Akaike information criterion; BIC = Bayesian information criterion.
# }
```
