# Publication-ready regression tables

``` r

library(spicy)
```

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
produces a coefficient summary table from one or several fitted
[`lm()`](https://rdrr.io/r/stats/lm.html) or
[`glm()`](https://rdrr.io/r/stats/glm.html) models. The output is
publication-ready by default and follows APA Manual 7 (American
Psychological Association 2020, Tables 7.13–7.15) formatting
conventions: paired estimate-and-CI columns, APA p-values without
leading zero, factor levels grouped under their parent variable, fit
statistics at the foot of the table, and a self-documenting note line
that names the variance estimator and any methodological choice that
affected the rendered values.

The function is **fit-first**: you pass already-fitted models, not raw
data and a formula. The same object exports cleanly to a long
broom-canonical frame
([`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)) and
to a one-row-per-model glance summary
([`broom::glance()`](https://generics.r-lib.org/reference/glance.html)).

`lm` and `glm` are both supported. The *Generalised linear models*
section covers the glm-specific argument semantics; everything else
applies to both. Mixed-effects models are on the roadmap.

## Basic usage

Pass a fitted [`lm()`](https://rdrr.io/r/stats/lm.html) object. The
default rendering returns a single-model table with `B`, `SE`, `95% CI`,
and `p` columns and a fit-statistics footer (`n`, `R²`, `Adj.R²`):

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(fit)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20  1.66  [61.95, 68.45]  <.001 
#>  age             │    0.05  0.03  [-0.01,  0.11]   .130 
#>  sex:            │                                      
#>    Female (ref.) │     —     —          —          —    
#>    Male          │    3.86  0.91  [ 2.08,  5.63]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │     —     —          —          —    
#>    Yes           │   -1.72  1.11  [-3.89,  0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
```

Reading the table:

- Each predictor occupies one row; factor predictors are grouped under
  their parent variable name with one row per level. The reference level
  carries the `(ref.)` annotation and an em-dash across the statistic
  columns, so the substantive comparison is visible in a single glance
  (NEJM / BMJ clinical convention).
- The `95% CI` column reports a symmetric Wald interval at the level set
  by `ci_level` (default 0.95). The CI header tracks the chosen level —
  switching to `ci_level = 0.99` re-labels the column accordingly.
- The footer line names the variance estimator in plain English so the
  reader can find the inferential regime without leaving the table. The
  estimator switches with `vcov` (covered in *Robust variance* below).

## Standardised coefficients

Standardised coefficients (`β`) make predictors with different natural
scales comparable: a one-standard-deviation increase in `X` predicts a
`β`-standard-deviation change in `Y`. APA Manual 7 §7.13 recommends
reporting both `B` and `β` so the unstandardised effect (natural units,
interpretable) stays alongside the standardised effect (comparable
across predictors).

`standardized` selects the method. Four are available; the choice is
consequential and well-documented (Cohen, Cohen, West, and Aiken 2003
§3.4; Gelman 2008):

- `"refit"` — refit the model on z-scored outcome and predictors.
  Gold-standard convention, used by SPSS `REGRESSION` and Stata
  `regress, beta`. Both numeric and dummy-coded predictors enter the
  refit on the same scale.
- `"posthoc"` — algebraic rescaling `β = B × SD(X) / SD(Y)`, applied to
  the original fit. Numerically identical to `"refit"` for purely
  linear-additive Gaussian models; preferred when refitting is expensive
  or when [`lm()`](https://rdrr.io/r/stats/lm.html) was wrapped in a
  pipeline that resists re-execution.
- `"basic"` — algebraic, but factor dummies keep their 0/1 scale rather
  than being z-scored. Useful when factor levels carry meaningful base
  rates that scale-free standardisation would obscure.
- `"smart"` — Gelman’s (2008) recommendation: numeric predictors divided
  by `2 × SD(X)`; binary predictors centred only. The resulting `β` is
  the predicted change in `Y` for one within- sample standard deviation
  of `X`, on the original `Y` scale, which keeps factor and numeric
  coefficients on roughly comparable footing.

When `standardized != "none"`, the `"beta"` token is auto-injected into
`show_columns` immediately after `"b"`:

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(fit, standardized = "refit")
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B       β     SE       95% CI        p   
#> ─────────────────┼─────────────────────────────────────────────
#>  (Intercept)     │   65.20  -0.10  1.66  [61.95, 68.45]  <.001 
#>  age             │    0.05   0.04  0.03  [-0.01,  0.11]   .130 
#>  sex:            │                                             
#>    Female (ref.) │     —      —     —          —          —    
#>    Male          │    3.86   0.25  0.91  [ 2.08,  5.63]  <.001 
#>  smoking:        │                                             
#>    No (ref.)     │     —      —     —          —          —    
#>    Yes           │   -1.72  -0.11  1.11  [-3.89,  0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                        
#>  R²              │    0.02                                     
#>  Adj.R²          │    0.02                                     
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> β = standardised coefficient.
```

**Caveat: standardised coefficients with interactions or transformed
terms.** When the model contains a product term, an
[`I()`](https://rdrr.io/r/base/AsIs.html),
[`poly()`](https://rdrr.io/r/stats/poly.html),
[`log()`](https://rdrr.io/r/base/Log.html), or
[`splines::ns()`](https://rdrr.io/r/splines/ns.html) wrapper, the
standardised coefficient of the non-additive term has no closed-form
“one-SD change in X” reading (Aiken and West 1991; Cohen et al. 2003
§7.7). The function emits a classed `spicy_caveat` warning at runtime
AND prints a method-specific caveat line in the table footer, so the
limitation is exposed at the point of use without blocking the table.

## Per-coefficient effect sizes

For each predictor, you can request a partial effect-size column. Three
measures are available — Cohen’s `f²` (`partial_f2`), Pearson’s partial
`η²` (`partial_eta2`), and the Olejnik–Algina bias-corrected partial
`ω²` (`partial_omega2`). Each carries a confidence interval derived from
noncentral-`F` inversion (Smithson 2003; Steiger 2004), exposed as a
separate `<token>_ci` column. The shortcuts `"all_f2"`, `"all_eta2"`,
`"all_omega2"` expand to the point estimate and its CI pair in one go:

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking + education,
          data = sochealth)
table_regression(
  fit,
  show_columns = c("b", "p", "all_eta2", "all_omega2")
)
#> Ordered factor(s) detected. Polynomial contrasts (the R default for `ordered()`) decompose the factor into orthogonal trend components: `.L` = linear, `.Q` = quadratic, `.C` = cubic, `^k` = degree k. Coefficients are trends across the ordered levels, NOT per-level effects against a reference.
#> ℹ To display per-level (treatment) effects, refit with `factor(x, ordered = FALSE)` or set `options(contrasts = c("contr.treatment", "contr.treatment"))`.
#> This message is displayed once per session.
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B       p     η²    η² 95% CI     ω²    ω² 95% CI   
#> ─────────────────┼────────────────────────────────────────────────────────
#>  (Intercept)     │   64.49  <.001                                         
#>  age             │    0.03   .344  0.00  [0.00, 0.01]  0.00  [0.00, 0.01] 
#>  sex:            │                                                        
#>    Female (ref.) │     —     —                                            
#>    Male          │    3.57  <.001  0.02  [0.01, 0.03]  0.02  [0.01, 0.03] 
#>  smoking:        │                                                        
#>    No (ref.)     │     —     —                                            
#>    Yes           │    0.68   .496  0.00  [0.00, 0.01]  0.00  [0.00, 0.01] 
#>  education:      │                                                        
#>    .L            │   13.94  <.001  0.21  [0.17, 0.25]  0.21  [0.17, 0.25] 
#>    .Q            │   -1.66   .013  0.21  [0.17, 0.25]  0.21  [0.17, 0.25] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                                   
#>  R²              │    0.22                                                
#>  Adj.R²          │    0.22                                                
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> η² = partial eta-squared; ω² = bias-corrected partial omega-squared.
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).
```

Methodology notes:

- The partial F-test is computed on a Type-II ANOVA reference
  (`car::Anova`), which respects the principle of marginality and is the
  SAS / SPSS default for unbalanced designs.
- The `ω²` point estimate is bias-corrected via the Olejnik and
  Algina (2003) formula `((F − 1) × df1) / (F × df1 + N − df1)`,
  yielding a less-biased small-sample estimator than partial `η²`.
- The CI bounds come from inverting the noncentrality parameter of the
  F-distribution at the lower and upper confidence levels (Steiger 2004
  §4). The Steiger inversion always brackets the bias-corrected point
  estimate, even when the lower bound clips at zero (common for
  near-null terms).
- For factor predictors with `k` levels, the partial F-test is the joint
  `(k − 1)` df Wald test, so the same effect-size value is broadcast
  across all non-reference dummy rows; the reference row shows an
  em-dash.

## Average marginal effects (AME)

The `B` coefficient is the model’s *structural* effect — the change in
the linear predictor `Xβ` for a one-unit change in `x`, holding the
other predictors constant. The **average marginal effect (AME)** is the
*observable* effect on the response: the average partial derivative
`dE[Y|X] / dx` over the sample. For a purely linear-additive
[`lm()`](https://rdrr.io/r/stats/lm.html) the two coincide. For models
with interactions or transformed terms, and for any
[`glm()`](https://rdrr.io/r/stats/glm.html) with a non-identity link,
the AME is the quantity the substantive reader actually needs.

Add `"ame"` to `show_columns` for the point estimate. Companion tokens
display the SE, CI, and p-value: `"ame_se"`, `"ame_ci"`, `"ame_p"`. The
shortcut `"all_ame"` expands to the full set.

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(
  fit,
  show_columns = c("b", "ame", "ame_ci", "ame_p")
)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      AME      95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20                              
#>  age             │    0.05   0.05  [-0.01, 0.11]   .130 
#>  sex:            │                                      
#>    Female (ref.) │     —      —          —         —    
#>    Male          │    3.86   3.86  [ 2.08, 5.63]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │     —      —          —         —    
#>    Yes           │   -1.72  -1.72  [-3.89, 0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> AME = average marginal effect.
```

Reading conventions:

- `"p"` always refers to the `B` (or `β`) coefficient, never to the AME.
  Use `"ame_p"` for the AME-specific p-value.
- Placing `"ame"` after `"p"` keeps the “which p belongs to what”
  reading unambiguous — the B-block (`B / SE / p`) is closed before the
  AME-block opens.
- For non-linear models the AME is reported on the **response scale**,
  not the link scale. The *Generalised linear models* section below
  works out a logistic-regression example.

Inference is delegated to
\[[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)\]\[marginaleffects::avg_slopes\]
and respects the chosen variance estimator. Under cluster-robust
variance (covered in *Robust variance*), the AME inference shares the
coefficient’s t-distribution and Satterthwaite-corrected degrees of
freedom via
[`clubSandwich::linear_contrast()`](http://jepusto.github.io/clubSandwich/reference/linear_contrast.md),
so `B` and AME are reported on the same inferential footing in the same
table.

## Multiple models side by side

Pass a list of [`lm()`](https://rdrr.io/r/stats/lm.html) fits. The
default column layout places each model in its own panel under a
centered **spanner label** showing the model name; sub-columns
(`B / SE / p`) are repeated under the spanner. When dependent variables
differ across models and the user did not supply labels
(`model_labels =` or named list), the bare DV name is lifted into the
spanner automatically:

``` r

m_wellbeing <- lm(wellbeing_score ~ age + sex + smoking,
                   data = sochealth)
m_bmi       <- lm(bmi             ~ age + sex + smoking,
                   data = sochealth)
table_regression(list(m_wellbeing, m_bmi))
#> Linear regression comparison
#> 
#>                      wellbeing_score             bmi          
#>                    ────────────────────  ──────────────────── 
#>  Variable        │    B      SE     p       B      SE     p   
#> ─────────────────┼────────────────────────────────────────────
#>  (Intercept)     │   65.20  1.66  <.001    23.98  0.40  <.001 
#>  age             │    0.05  0.03   .130     0.04  0.01  <.001 
#>  sex:            │                                            
#>    Female (ref.) │     —     —     —         —     —     —    
#>    Male          │    3.86  0.91  <.001     0.51  0.22   .018 
#>  smoking:        │                                            
#>    No (ref.)     │     —     —     —         —     —     —    
#>    Yes           │   -1.72  1.11   .121    -0.06  0.26   .822 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                  1163                 
#>  R²              │    0.02                  0.02              
#>  Adj.R²          │    0.02                  0.02              
#> 
#> Note. Linear regression models.
#> Std. errors: classical (OLS).
```

When all models share the same DV, the DV appears in the title. Use a
named list — e.g. `list(Crude = m1, Adjusted = m2)` — to set the spanner
labels explicitly; pass `model_labels = c(...)` to override the names
from the list.

## Hierarchical / nested regression

Set `nested = TRUE` to add **in-table change-statistic rows** (APA Table
7.13 / Stata `esttab` / SPSS Model Summary convention). Each adjacent
pair (M2 vs M1, M3 vs M2, …) contributes one column of change stats
below `R² / Adj.R²`; the FIRST model column gets em-dashes (no previous
model to compare to).

Hierarchical regression requires every model in the sequence to share
the same analytic sample: identical `nobs` AND identical response
variable. R’s listwise deletion otherwise produces different `n` per
model as soon as a predictor introduced by M2 or M3 has missing values
that M1 did not see — a silent bias on the change statistics. We
restrict to complete cases on the union of predictors up front:

``` r

sochealth_cc <- sochealth |>
  dplyr::select(wellbeing_score, age, sex, smoking, bmi, region) |>
  na.omit()
```

``` r

m1 <- lm(wellbeing_score ~ age + sex,                 data = sochealth_cc)
m2 <- lm(wellbeing_score ~ age + sex + smoking,       data = sochealth_cc)
m3 <- lm(wellbeing_score ~ age + sex + smoking + bmi, data = sochealth_cc)
table_regression(list(m1, m2, m3), nested = TRUE)
#> Hierarchical linear regression: wellbeing_score
#> 
#>                          Model 1                Model 2            Model 3     
#>                    ────────────────────  ─────────────────────  ────────────── 
#>  Variable        │    B      SE     p       B       SE     p       B       SE  
#> ─────────────────┼─────────────────────────────────────────────────────────────
#>  (Intercept)     │   64.70  1.66  <.001    65.00   1.67  <.001    80.57   3.37 
#>  age             │    0.05  0.03   .118     0.05   0.03   .109     0.07   0.03 
#>  sex:            │                                                             
#>    Female (ref.) │     —     —     —         —      —     —         —      —   
#>    Male          │    3.89  0.91  <.001     3.88   0.91  <.001     4.21   0.90 
#>  smoking:        │                                                             
#>    No (ref.)     │                           —      —     —         —      —   
#>    Yes           │                         -1.68   1.11   .132    -1.71   1.10 
#>  bmi             │                                                -0.65   0.12 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1163                  1163                   1163           
#>  R²              │    0.02                  0.02                   0.04        
#>  Adj.R²          │    0.02                  0.02                   0.04        
#>  ΔR²             │     —                   +0.00                  +0.02        
#>  F-change        │     —                   +2.28                 +28.13        
#>  p (change)      │     —                     .132                  <.001       
#> 
#>                    Model 
#>                    ───── 
#>  Variable        │   p   
#> ─────────────────┼───────
#>  (Intercept)     │ <.001 
#>  age             │  .019 
#>  sex:            │       
#>    Female (ref.) │  —    
#>    Male          │ <.001 
#>  smoking:        │       
#>    No (ref.)     │  —    
#>    Yes           │  .119 
#>  bmi             │ <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  n               │       
#>  R²              │       
#>  Adj.R²          │       
#>  ΔR²             │       
#>  F-change        │       
#>  p (change)      │       
#> 
#> Note. Linear regression models.
#> Std. errors: classical (OLS).
```

Default change tokens auto-injected:
`c("r2_change", "f_change", "p_change")` for `lm` (APA
hierarchical-regression standard), `c("lrt_change", "p_change")` for
`glm` (Hosmer & Lemeshow §3.5). Customise via `show_fit_stats`; the
order of tokens controls the order of the rows. Other change tokens are
available: `"adj_r2_change"`, `"f2_change"`, `"deviance_change"`,
`"aic_change"` / `"aicc_change"` / `"bic_change"`.

Validation is strict: identical `nobs` AND identical response across all
models, otherwise a `spicy_invalid_input` error explains the
listwise-deletion trap and suggests refitting on the common subset.

## Robust variance

The default `vcov = "classical"` reports the OLS standard error, valid
under homoskedastic, independent errors. Two robust alternatives are
first-class arguments; bootstrap and jackknife variants are also
available via `vcov = "bootstrap"` / `"jackknife"`.

### Heteroskedasticity-consistent (HC)

When error variance plausibly depends on the predictors (a ubiquitous
concern in cross-sectional social-science data), set `vcov = "HC*"` for
sandwich-style standard errors via
\[[`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)\]\[sandwich::vcovHC\].
The valid types are `HC0` through `HC5`; `HC3` is the
small-sample-friendly default (Long and Ervin 2000):

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(fit, vcov = "HC3")
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20  1.61  [62.05, 68.35]  <.001 
#>  age             │    0.05  0.03  [-0.01,  0.11]   .127 
#>  sex:            │                                      
#>    Female (ref.) │     —     —          —          —    
#>    Male          │    3.86  0.91  [ 2.07,  5.64]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │     —     —          —          —    
#>    Yes           │   -1.72  1.11  [-3.91,  0.47]   .123 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: heteroskedasticity-robust (HC3).
```

The footer reads “*Std. errors: heteroskedasticity-robust (HC3)*”; the
column header for the confidence interval automatically tracks
`ci_level`.

### Cluster-robust (CR)

For clustered observations (repeated measures on the same person,
students within schools, observations within regions), `vcov = "CR*"`
requests cluster-robust variance via
\[[`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)\]\[clubSandwich::vcovCR\],
with the cluster identifier supplied through `cluster`. Three forms are
accepted:

- **Formula** — `cluster = ~region`. The variables are looked up in
  `model.frame(fit)` first, then in the model’s original `data`
  argument. Recommended: independent of the dataset’s name, composable
  for multi-way clustering (`cluster = ~region:year`).
- **String** — `cluster = "region"`. Single column name resolved the
  same way. Convenient but cannot express interactions.
- **Vector** — `cluster = df$region`. Atomic vector of length
  `nobs(fit)`. Use this when the cluster key is derived on the fly
  (`cluster = interaction(df$region, df$year)`) or pulled from a
  different dataset with matching row order.

Bare unquoted names (`cluster = region`) are **not** accepted — they
would require non-standard evaluation that breaks under programmatic use
(function wrapping, loops, dynamic column choice).

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth_cc)
table_regression(
  fit,
  vcov = "CR2",
  cluster = ~region,
  show_columns = c("b", "se", "ci", "p", "ame", "ame_p")
)
#> Registered S3 method overwritten by 'clubSandwich':
#>   method    from    
#>   bread.mlm sandwich
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p     AME     p   
#> ─────────────────┼────────────────────────────────────────────────────
#>  (Intercept)     │   65.00  1.74  [60.49, 69.51]  <.001               
#>  age             │    0.05  0.04  [-0.05,  0.15]   .247   0.05   .247 
#>  sex:            │                                                    
#>    Female (ref.) │     —     —          —          —       —     —    
#>    Male          │    3.88  0.85  [ 1.68,  6.07]   .006   3.88   .006 
#>  smoking:        │                                                    
#>    No (ref.)     │     —     —          —          —       —     —    
#>    Yes           │   -1.68  1.55  [-5.72,  2.37]   .331  -1.68   .331 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1163                                               
#>  R²              │    0.02                                            
#>  Adj.R²          │    0.02                                            
#> 
#> Note. Linear regression.
#> Std. errors: cluster-robust (CR2), clusters by region.
#> AME = average marginal effect.
#> AME inference: t-distribution with Satterthwaite-corrected df (Pustejovsky & Tipton 2018) via `clubSandwich::linear_contrast()`.
```

`CR2` (the Bell-McCaffrey adjustment) is the recommended default under
few clusters (Pustejovsky and Tipton 2018; Imbens and Kolesár 2016).
Coefficient inference uses
\[[`clubSandwich::coef_test()`](http://jepusto.github.io/clubSandwich/reference/coef_test.md)\]\[clubSandwich::coef_test\]
with Satterthwaite-corrected degrees of freedom. When AME columns are
requested, the same Satterthwaite framework is applied to the AME
contrast via
[`clubSandwich::linear_contrast()`](http://jepusto.github.io/clubSandwich/reference/linear_contrast.md),
so `B` and AME share the same t-distribution with the same df — visible
in the footer.

## Multiple-comparison adjustment

`p_adjust` applies a family-wise or false-discovery-rate adjustment to
the p-values via
\[[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html)\]\[stats::p.adjust\].
The family is the model’s full coefficient set (intercept and reference
rows excluded); `B` and `AME` are adjusted as independent families
within the same call. Available methods are the same as in
[`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html): `"none"`
(default), `"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"` /
`"fdr"`, `"BY"`.

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking + education,
          data = sochealth)
table_regression(fit, p_adjust = "bonferroni")
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   64.49  1.48  [61.58, 67.40]  <.001 
#>  age             │    0.03  0.03  [-0.03,  0.08]  1.000 
#>  sex:            │                                      
#>    Female (ref.) │     —     —          —          —    
#>    Male          │    3.57  0.81  [ 1.99,  5.15]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │     —     —          —          —    
#>    Yes           │    0.68  0.99  [-1.27,  2.63]  1.000 
#>  education:      │                                      
#>    .L            │   13.94  0.79  [12.38, 15.49]  <.001 
#>    .Q            │   -1.66  0.67  [-2.97, -0.35]   .065 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.22                              
#>  Adj.R²          │    0.22                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> P-values adjusted via stats::p.adjust(method = 'bonferroni'); m = 5 coefficient(s) per model.
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).
```

The footer documents the chosen method and the family size; the SE
column is unchanged. `p_adjust` is orthogonal to `vcov`: combining a
robust SE with a family-wise correction is fully supported,
e.g. `vcov = "HC3", p_adjust = "holm"`. The adjustment is applied
**before** `keep` / `drop` filtering, so the family stays the model’s
full coefficient set and the displayed adjusted p-values reflect the
right denominator regardless of which subset is shown.

A methodological note. Adjusting the p-values of every coefficient of a
single regression model is *not* the standard convention in
social-science or clinical reporting (Rothman 1990; Gelman, Hill, and
Yajima 2012; Greenland 2017; Harrell 2015 §5.4; APA Manual 7 §6.46).
Each coefficient tests a scientifically distinct hypothesis on a
distinct predictor, which is not the situation that family-wise
procedures were designed for. The default `"none"` reflects this
consensus.

Adjustment is nonetheless legitimate in three contexts:

1.  **Mass screening** with many candidate predictors and no prior
    hypothesis — typically `"BH"` (Benjamini–Hochberg, false discovery
    rate).
2.  **Pre-registered multi-endpoint confirmatory designs** — typically
    `"holm"` (Holm’s step-down, strong family-wise error rate control).
3.  **When a reviewer, an editor, or a statistical analysis plan
    explicitly requests it** — apply the requested method and document
    it in the footer.

The argument is exposed under the same *transparency* rule used for
`standardized`: the tool is here, the methodological choice is yours,
and the footer makes the choice visible to the reader.

## Filtering displayed coefficients

`keep` and `drop` accept regular expressions matched against coefficient
names (as returned by \[stats::coef()\]). They are mutually exclusive —
pick `keep` to whitelist focal predictors, or `drop` to hide a few
control variables. Multiple patterns combine with logical OR.

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking + bmi + education,
          data = sochealth)
table_regression(fit, keep = c("^smoking", "^bmi$"))
#> Linear regression: wellbeing_score
#> 
#>  Variable    │    B      SE      95% CI        p   
#> ─────────────┼─────────────────────────────────────
#>  smoking:    │                                     
#>    No (ref.) │     —     —          —         —    
#>    Yes       │    0.79  1.00  [-1.17, 2.75]   .428 
#>  bmi         │    0.10  0.12  [-0.14, 0.33]   .418 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n           │ 1163                                
#>  R²          │    0.23                             
#>  Adj.R²      │    0.22                             
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).
```

``` r

table_regression(fit, drop = "^education")
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   62.12  3.23  [55.78, 68.45]  <.001 
#>  age             │    0.02  0.03  [-0.03,  0.08]   .407 
#>  sex:            │                                      
#>    Female (ref.) │     —     —          —          —    
#>    Male          │    3.50  0.81  [ 1.91,  5.10]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │     —     —          —          —    
#>    Yes           │    0.79  1.00  [-1.17,  2.75]   .428 
#>  bmi             │    0.10  0.12  [-0.14,  0.33]   .418 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1163                                 
#>  R²              │    0.23                              
#>  Adj.R²          │    0.22                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).
```

Together with `p_adjust`, this is the article-ready workflow: adjust on
the full model, display only the rows the reader cares about —
`table_regression(fit, p_adjust = "BH", keep = "^treatment")`.

## Generalised linear models (glm)

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
accepts any [`glm()`](https://rdrr.io/r/stats/glm.html) fit. Inference
defaults to the `z`-asymptotic Wald regime — the convention used by
[`summary.glm()`](https://rdrr.io/r/stats/summary.glm.html), Stata’s
`logit, or`, and SPSS `LOGISTIC REGRESSION`. The table title becomes
family-aware (“Logistic regression”, “Poisson regression”, “Probit
regression”, …) and the default fit-statistics block swaps in `nobs`,
`pseudo_r2_mcfadden`, `pseudo_r2_nagelkerke`, and `AIC` instead of `R²`
and `Adj.R²`.

We illustrate with a logistic regression of `smoking` on `sex`, `age`,
and `education` from `sochealth`. The `education` variable is an ordered
factor in the source data; we convert it to a plain factor so contrasts
are dummy-coded (one row per level) rather than the polynomial trends
(`.L`, `.Q`, …) R applies by default to ordered factors. The model mixes
a binary factor (`sex`), a numeric predictor (`age`), and a 3-level
factor (`education`) — enough to exercise every glm-specific feature
below.

``` r

sh <- sochealth |>
  dplyr::mutate(education = factor(education, ordered = FALSE))

fit <- glm(smoking ~ sex + age + education, data = sh,
           family = binomial)
table_regression(fit)
#> Logistic regression: smoking
#> 
#>  Variable                 │    B      SE       95% CI        p   
#> ──────────────────────────┼──────────────────────────────────────
#>  (Intercept)              │   -1.11  0.29  [-1.67, -0.55]  <.001 
#>  sex:                     │                                      
#>    Female (ref.)          │     —     —          —          —    
#>    Male                   │   -0.04  0.14  [-0.32,  0.25]   .800 
#>  age                      │    0.01  0.00  [-0.00,  0.02]   .214 
#>  education:               │                                      
#>    Lower secondary (ref.) │     —     —          —          —    
#>    Upper secondary        │   -0.48  0.17  [-0.82, -0.14]   .005 
#>    Tertiary               │   -0.91  0.20  [-1.29, -0.52]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                                 
#>  R² (McFadden)            │    0.02                              
#>  R² (Nagelkerke)          │    0.03                              
#>  AIC                      │ 1200.9                               
#> 
#> Note. Logistic regression.
#> Std. errors: classical (MLE inverse Hessian).
```

### Response-scale display: `exponentiate = TRUE`

Set `exponentiate = TRUE` to switch the `B` column to the response scale
and rebrand its header per family and link: `OR` for `binomial(logit)`,
`IRR` for `poisson(log)`, `HR` for `binomial(cloglog)`, `RR` for
`binomial(log)`, `MR` for `Gamma(log)`, and the generic `exp(B)`
otherwise. The standard error follows the delta-method approximation
`SE_OR = OR × SE_log-odds` (the Stata `logit, or` convention). The test
statistic and the p-value are invariant under any monotone
transformation, so they remain on the link scale and match the
unexponentiated table verbatim:

``` r

table_regression(fit, exponentiate = TRUE)
#> Logistic regression: smoking
#> 
#>  Variable                 │   OR      SE      95% CI       p   
#> ──────────────────────────┼────────────────────────────────────
#>  (Intercept)              │    0.33  0.09  [0.19, 0.58]  <.001 
#>  sex:                     │                                    
#>    Female (ref.)          │     —     —         —         —    
#>    Male                   │    0.96  0.14  [0.73, 1.28]   .800 
#>  age                      │    1.01  0.00  [1.00, 1.02]   .214 
#>  education:               │                                    
#>    Lower secondary (ref.) │     —     —         —         —    
#>    Upper secondary        │    0.62  0.11  [0.44, 0.87]   .005 
#>    Tertiary               │    0.40  0.08  [0.27, 0.59]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                               
#>  R² (McFadden)            │    0.02                            
#>  R² (Nagelkerke)          │    0.03                            
#>  AIC                      │ 1200.9                             
#> 
#> Note. Logistic regression.
#> Std. errors: classical (MLE inverse Hessian).
#> OR = odds ratio.
#> Coefficients exponentiated and displayed as OR; CI bounds exponentiated; SE delta-method approximation: SE_OR = OR × SE_link.
```

### Term-level partial chi-square

`partial_chi2` is the glm analog of `partial_f2`: for each model term,
the partial likelihood-ratio chi-square via `drop1(test = "LRT")` (SAS
PROC LOGISTIC `TYPE3`; Long & Freese 2014 §3.5). Rendered as
`value (df)` so factor terms (`k − 1` df) and numeric terms (1 df) read
at a glance:

``` r

fit2 <- glm(smoking ~ age + education, data = sh, family = binomial)
table_regression(fit2, show_columns = c("b", "partial_chi2", "p"))
#> Logistic regression: smoking
#> 
#>  Variable                 │    B        χ²        p   
#> ──────────────────────────┼───────────────────────────
#>  (Intercept)              │   -1.13             <.001 
#>  age                      │    0.01   1.55 (1)   .213 
#>  education:               │                           
#>    Lower secondary (ref.) │     —                —    
#>    Upper secondary        │   -0.48  21.70 (2)   .005 
#>    Tertiary               │   -0.91  21.70 (2)  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                      
#>  R² (McFadden)            │    0.02                   
#>  R² (Nagelkerke)          │    0.03                   
#>  AIC                      │ 1198.9                    
#> 
#> Note. Logistic regression.
#> Std. errors: classical (MLE inverse Hessian).
#> χ² = partial likelihood-ratio chi-squared.
```

### Standardised coefficients: `refit` and the new `pseudo`

For `glm`, `standardized = "refit"` z-scores numeric *predictors* only
and refits the model — the response stays on its observed scale because
the link function is fixed. This is the “x-standardization” convention
(Long and Freese 2014 §4.3.4). The other algebraic methods (`"posthoc"`,
`"basic"`, `"smart"`) apply X-only scaling using the same algebra as in
the `lm` case.

`standardized = "pseudo"` (`glm` only) is the Menard (2004, 2011)
*fully* standardised coefficient, scaling by `SD(X) / SD(Y*)` where `Y*`
is the latent variable on the link scale and
`SD(Y*) = sqrt(var(linear-predictor) + var_link)` with `var_link` = π²/3
for logit, 1 for probit, π²/6 for cloglog. Defined for binomial
families; non-binomial returns NA with a `spicy_caveat`:

``` r

table_regression(fit, standardized = "pseudo")
#> Logistic regression: smoking
#> 
#>  Variable                 │    B       β     SE       95% CI        p   
#> ──────────────────────────┼─────────────────────────────────────────────
#>  (Intercept)              │   -1.11    —    0.29  [-1.67, -0.55]  <.001 
#>  sex:                     │                                             
#>    Female (ref.)          │     —      —     —          —          —    
#>    Male                   │   -0.04  -0.02  0.14  [-0.32,  0.25]   .800 
#>  age                      │    0.01   0.05  0.00  [-0.00,  0.02]   .214 
#>  education:               │                                             
#>    Lower secondary (ref.) │     —      —     —          —          —    
#>    Upper secondary        │   -0.48  -0.26  0.17  [-0.82, -0.14]   .005 
#>    Tertiary               │   -0.91  -0.49  0.20  [-1.29, -0.52]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                                        
#>  R² (McFadden)            │    0.02                                     
#>  R² (Nagelkerke)          │    0.03                                     
#>  AIC                      │ 1200.9                                      
#> 
#> Note. Logistic regression.
#> Std. errors: classical (MLE inverse Hessian).
#> β = standardised coefficient.
```

### Average marginal effects: probability units, not log-odds

For `glm`, AME is computed as the average of `dE[Y|X] / dx` over the
observed sample — **on the response scale, not the link scale**. For
logistic regression this means the displayed AME is in **probability
units, not log-odds**: an AME of `-0.15` for `education = Tertiary`
reads “on average, holding sex and age constant, a respondent with
tertiary education is 15 percentage points less likely to be a current
smoker than one with lower secondary education”. This is almost always
the quantity a substantive reader wants from a logistic regression, and
it is the reason AME is increasingly recommended over odds ratios for
communicating logistic effects (Mood 2010; Long and Freese 2014 §5.3).

``` r

table_regression(fit, show_columns = c("b", "p", "ame", "ame_ci", "ame_p"))
#> Logistic regression: smoking
#> 
#>  Variable                 │    B       p     AME       95% CI        p   
#> ──────────────────────────┼──────────────────────────────────────────────
#>  (Intercept)              │   -1.11  <.001                               
#>  sex:                     │                                              
#>    Female (ref.)          │     —     —       —          —          —    
#>    Male                   │   -0.04   .800  -0.01  [-0.05,  0.04]   .800 
#>  age                      │    0.01   .214   0.00  [-0.00,  0.00]   .214 
#>  education:               │                                              
#>    Lower secondary (ref.) │     —     —       —          —          —    
#>    Upper secondary        │   -0.48   .005  -0.09  [-0.16, -0.02]   .007 
#>    Tertiary               │   -0.91  <.001  -0.15  [-0.22, -0.09]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                                         
#>  R² (McFadden)            │    0.02                                      
#>  R² (Nagelkerke)          │    0.03                                      
#>  AIC                      │ 1200.9                                       
#> 
#> Note. Logistic regression.
#> Std. errors: classical (MLE inverse Hessian).
#> AME = average marginal effect.
```

Point estimate and inference are delegated to
\[[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)\]\[marginaleffects::avg_slopes\].
Under cluster-robust variance, the Satterthwaite-df handling described
in the *Robust variance* section applies to `glm` AME as well.

### Profile-likelihood CIs: `ci_method = "profile"`

The default is `ci_method = "wald"` (symmetric `estimate ± z × SE`,
matching `summary.glm`). For `glm` you can also request
`ci_method = "profile"`, which calls
[`MASS::confint.glm()`](https://rdrr.io/pkg/MASS/man/confint.html) to
compute the profile-likelihood CI — asymmetric, exact for
likelihood-based inference, and the gold standard under sparse data or
near-boundary estimates (Venables & Ripley *MASS* §7.2). The estimate,
SE, statistic, and p-value all remain Wald; `"profile"` only refines the
CI bounds:

``` r

table_regression(fit, ci_method = "profile")
#> Logistic regression: smoking
#> 
#>  Variable                 │    B      SE       95% CI        p   
#> ──────────────────────────┼──────────────────────────────────────
#>  (Intercept)              │   -1.11  0.29  [-1.68, -0.55]  <.001 
#>  sex:                     │                                      
#>    Female (ref.)          │     —     —          —          —    
#>    Male                   │   -0.04  0.14  [-0.32,  0.25]   .800 
#>  age                      │    0.01  0.00  [-0.00,  0.02]   .214 
#>  education:               │                                      
#>    Lower secondary (ref.) │     —     —          —          —    
#>    Upper secondary        │   -0.48  0.17  [-0.82, -0.14]   .005 
#>    Tertiary               │   -0.91  0.20  [-1.29, -0.52]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                                 
#>  R² (McFadden)            │    0.02                              
#>  R² (Nagelkerke)          │    0.03                              
#>  AIC                      │ 1200.9                               
#> 
#> Note. Logistic regression.
#> Std. errors: classical (MLE inverse Hessian).
```

### Hierarchical glm (LRT)

`nested = TRUE` on a list of nested glm fits adds a “Model comparison”
block whose default tokens are `c("LRT", "p")` — the APA-conventional
layout for hierarchical logistic regression (Hosmer & Lemeshow §3.5;
Long & Freese 2014 §3.6). The chi-square statistic comes from
`anova(test = "LRT")`. `r2_change` and `F` are not defined for glm and
return em-dashes if explicitly requested:

``` r

m1 <- glm(smoking ~ sex,                   data = sh, family = binomial)
m2 <- glm(smoking ~ sex + age,             data = sh, family = binomial)
m3 <- glm(smoking ~ sex + age + education, data = sh, family = binomial)
table_regression(list(m1, m2, m3), nested = TRUE)
#> Hierarchical logistic regression: smoking
#> 
#>                                   Model 1                Model 2        
#>                             ────────────────────  ───────────────────── 
#>  Variable                 │    B      SE     p       B       SE     p   
#> ──────────────────────────┼─────────────────────────────────────────────
#>  (Intercept)              │   -1.29  0.10  <.001    -1.54   0.26  <.001 
#>  sex:                     │                                             
#>    Female (ref.)          │     —     —     —         —      —     —    
#>    Male                   │   -0.05  0.14   .713    -0.05   0.14   .722 
#>  age                      │                          0.01   0.00   .294 
#>  education:               │                                             
#>    Lower secondary (ref.) │                                             
#>    Upper secondary        │                                             
#>    Tertiary               │                                             
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                  1175                  
#>  R² (McFadden)            │    0.00                  0.00               
#>  R² (Nagelkerke)          │    0.00                  0.00               
#>  AIC                      │ 1217.6                1218.5                
#>  Δχ²                      │     —                   +1.10               
#>  p (change)               │     —                     .294              
#> 
#>                                    Model 3        
#>                             ───────────────────── 
#>  Variable                 │    B       SE     p   
#> ──────────────────────────┼───────────────────────
#>  (Intercept)              │   -1.11   0.29  <.001 
#>  sex:                     │                       
#>    Female (ref.)          │     —      —     —    
#>    Male                   │   -0.04   0.14   .800 
#>  age                      │    0.01   0.00   .214 
#>  education:               │                       
#>    Lower secondary (ref.) │     —      —     —    
#>    Upper secondary        │   -0.48   0.17   .005 
#>    Tertiary               │   -0.91   0.20  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                        │ 1175                  
#>  R² (McFadden)            │    0.02               
#>  R² (Nagelkerke)          │    0.03               
#>  AIC                      │ 1200.9                
#>  Δχ²                      │  +21.64               
#>  p (change)               │    <.001              
#> 
#> Note. Logistic regression models.
#> Std. errors: classical (MLE inverse Hessian).
```

### Gaussian glm caveat

A [`glm()`](https://rdrr.io/r/stats/glm.html) with `family = gaussian`
and `link = "identity"` is mathematically equivalent to
[`lm()`](https://rdrr.io/r/stats/lm.html) but lacks the variance-
explained effect-size family (`partial_f2 / η² / ω²`) and the
Satterthwaite-corrected AME path. Following the *transparency over
rejection* rule, spicy accepts the fit and emits a `spicy_caveat`
suggesting a refit with [`lm()`](https://rdrr.io/r/stats/lm.html).

## Significance stars

Stars are off by default. APA 7 §6.46 explicitly discourages
asterisks-only reporting in favour of exact p-values, and ASA’s
post-2019 guidance (Wasserstein, Schirm, and Lazar 2019) reinforces the
same point. Set `stars = TRUE` for the APA preset
(`*** p < .001, ** p < .01, * p < .05`) or pass a named numeric vector
for custom thresholds:

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
table_regression(fit, stars = TRUE)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │     B        SE       95% CI        p   
#> ─────────────────┼─────────────────────────────────────────
#>  (Intercept)     │   65.20***  1.66  [61.95, 68.45]  <.001 
#>  age             │    0.05     0.03  [-0.01,  0.11]   .130 
#>  sex:            │                                         
#>    Female (ref.) │     —        —          —          —    
#>    Male          │    3.86***  0.91  [ 2.08,  5.63]  <.001 
#>  smoking:        │                                         
#>    No (ref.)     │     —        —          —          —    
#>    Yes           │   -1.72     1.11  [-3.89,  0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                    
#>  R²              │    0.02                                 
#>  Adj.R²          │    0.02                                 
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> *** p < .001, ** p < .01, * p < .05.
```

Stars suffix the `B` column (or `β` when standardisation is requested);
the threshold mapping is auto-documented in the footer. The `p` column
itself remains unstarred so the numeric value stays readable.

## Display knobs

`labels` accepts a named character vector keyed by either formula term
labels (`"sex"`) or coefficient names (`"sexMale"`), so individual
contrast rows can be relabelled. `intercept_position` switches the
intercept to the bottom (Stata convention).
`reference_style = "annotation"` lifts the reference level into the
factor header (`"sex: [ref: Female]"`) and drops the explicit reference
row, for compact tables. `decimal_mark = ","` switches to European
decimal notation and automatically changes the CI separator to `";"` to
avoid ambiguity:

``` r

fit <- lm(wellbeing_score ~ age + sex + education, data = sochealth)
table_regression(
  fit,
  labels = c(
    age            = "Age (years)",
    sex            = "Sex",
    education      = "Education"
  ),
  reference_style = "annotation",
  intercept_position = "last",
  decimal_mark = ","
)
#> Linear regression: wellbeing_score
#> 
#>  Variable           │    B      SE       95% CI        p   
#> ────────────────────┼──────────────────────────────────────
#>  Age (years)        │    0,03  0,03  [-0,03;  0,08]   ,343 
#>  Sex: [ref: Female] │                                      
#>    Male             │    3,65  0,80  [ 2,09;  5,22]  <,001 
#>  Education:         │                                      
#>    .L               │   13,80  0,78  [12,28; 15,32]  <,001 
#>    .Q               │   -1,71  0,66  [-3,00; -0,41]   ,010 
#>  (Intercept)        │   64,63  1,46  [61,78; 67,49]  <,001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                  │ 1200                                 
#>  R²                 │    0,22                              
#>  Adj.R²             │    0,22                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).
```

## Output formats

The default print method draws an ASCII table to the console. The same
content is available as a plain `data.frame` (`output = "data.frame"`),
a long broom-style data.frame (`output = "long"`), or as a rich-format
table for `gt`, `tinytable`, `flextable`, `excel`, `word`, or
`clipboard`:

``` r

out <- table_regression(
  lm(wellbeing_score ~ age + sex + smoking, data = sochealth),
  output = "data.frame"
)
str(out)
#> 'data.frame':    11 obs. of  5 variables:
#>  $ Variable: chr  "(Intercept)" "age" "sex:" "  Female (ref.)" ...
#>  $ B       : chr  "  65.20" "   0.05" "       " "    —  " ...
#>  $ SE      : chr  "1.66" "0.03" "    " " —  " ...
#>  $ 95% CI  : chr  "[61.95, 68.45]" "[-0.01,  0.11]" "              " "      —       " ...
#>  $ p       : chr  "<.001" " .130" "     " " —   " ...
#>  - attr(*, "title")= chr "Linear regression: wellbeing_score"
#>  - attr(*, "note")= chr "Note. Linear regression.\nStd. errors: classical (OLS)."
#>  - attr(*, "col_spec")=List of 4
#>   ..$ :List of 6
#>   .. ..$ col_name     : chr "B"
#>   .. ..$ display_label: chr "B"
#>   .. ..$ token        : chr "b"
#>   .. ..$ model_id     : chr "M1"
#>   .. ..$ estimate_type: chr "B"
#>   .. ..$ fields       : chr "estimate"
#>   ..$ :List of 6
#>   .. ..$ col_name     : chr "SE"
#>   .. ..$ display_label: chr "SE"
#>   .. ..$ token        : chr "se"
#>   .. ..$ model_id     : chr "M1"
#>   .. ..$ estimate_type: chr "B"
#>   .. ..$ fields       : chr "se"
#>   ..$ :List of 6
#>   .. ..$ col_name     : chr "95% CI"
#>   .. ..$ display_label: chr "95% CI"
#>   .. ..$ token        : chr "ci"
#>   .. ..$ model_id     : chr "M1"
#>   .. ..$ estimate_type: chr "B"
#>   .. ..$ fields       : chr [1:2] "ci_low" "ci_high"
#>   ..$ :List of 6
#>   .. ..$ col_name     : chr "p"
#>   .. ..$ display_label: chr "p"
#>   .. ..$ token        : chr "p"
#>   .. ..$ model_id     : chr "M1"
#>   .. ..$ estimate_type: chr "B"
#>   .. ..$ fields       : chr "p_value"
#>  - attr(*, "group_sep_rows")= int 9
#>  - attr(*, "align")= chr "decimal"
#>  - attr(*, "decimal_mark")= chr "."
#>  - attr(*, "structured")=List of 10
#>   ..$ body              :'data.frame':   11 obs. of  6 variables:
#>   .. ..$ Variable  : chr [1:11] "(Intercept)" "age" "sex:" "  Female (ref.)" ...
#>   .. ..$ B         : num [1:11] 65.2009 0.0465 NA NA 3.8558 ...
#>   .. ..$ SE        : num [1:11] 1.6567 0.0307 NA NA 0.9053 ...
#>   .. ..$ 95% CI: LL: num [1:11] 61.9504 -0.0137 NA NA 2.0796 ...
#>   .. ..$ 95% CI: UL: num [1:11] 68.451 0.107 NA NA 5.632 ...
#>   .. ..$ p         : num [1:11] 1.59e-216 1.30e-01 NA NA 2.22e-05 ...
#>   ..$ reference_rows    : int [1:2] 4 7
#>   ..$ factor_header_rows: int [1:2] 3 6
#>   ..$ fit_stat_rows     : int [1:3] 9 10 11
#>   ..$ level_rows        : int [1:4] 4 5 7 8
#>   ..$ outcome_row       : int(0) 
#>   ..$ col_meta          :List of 5
#>   .. ..$ B         :List of 12
#>   .. .. ..$ token             : chr "b"
#>   .. .. ..$ model_id          : chr "M1"
#>   .. .. ..$ source_field      : chr "estimate"
#>   .. .. ..$ precision         : int 2
#>   .. .. ..$ p_style           : NULL
#>   .. .. ..$ threshold         : NULL
#>   .. .. ..$ ci_role           : NULL
#>   .. .. ..$ ci_pair           : NULL
#>   .. .. ..$ ci_label          : NULL
#>   .. .. ..$ is_df             : logi FALSE
#>   .. .. ..$ display_label     : chr "B"
#>   .. .. ..$ fit_stat_overrides:List of 3
#>   .. .. .. ..$ :List of 5
#>   .. .. .. .. ..$ fit_stat : chr "nobs"
#>   .. .. .. .. ..$ precision: int 0
#>   .. .. .. .. ..$ p_style  : NULL
#>   .. .. .. .. ..$ threshold: NULL
#>   .. .. .. .. ..$ row      : int 9
#>   .. .. .. ..$ :List of 5
#>   .. .. .. .. ..$ fit_stat : chr "r2"
#>   .. .. .. .. ..$ precision: int 2
#>   .. .. .. .. ..$ p_style  : NULL
#>   .. .. .. .. ..$ threshold: NULL
#>   .. .. .. .. ..$ row      : int 10
#>   .. .. .. ..$ :List of 5
#>   .. .. .. .. ..$ fit_stat : chr "adj_r2"
#>   .. .. .. .. ..$ precision: int 2
#>   .. .. .. .. ..$ p_style  : NULL
#>   .. .. .. .. ..$ threshold: NULL
#>   .. .. .. .. ..$ row      : int 11
#>   .. ..$ SE        :List of 11
#>   .. .. ..$ token        : chr "se"
#>   .. .. ..$ model_id     : chr "M1"
#>   .. .. ..$ source_field : chr "se"
#>   .. .. ..$ precision    : int 2
#>   .. .. ..$ p_style      : NULL
#>   .. .. ..$ threshold    : NULL
#>   .. .. ..$ ci_role      : NULL
#>   .. .. ..$ ci_pair      : NULL
#>   .. .. ..$ ci_label     : NULL
#>   .. .. ..$ is_df        : logi FALSE
#>   .. .. ..$ display_label: chr "SE"
#>   .. ..$ 95% CI: LL:List of 11
#>   .. .. ..$ token        : chr "ci"
#>   .. .. ..$ model_id     : chr "M1"
#>   .. .. ..$ source_field : chr "ci_low"
#>   .. .. ..$ precision    : int 2
#>   .. .. ..$ p_style      : NULL
#>   .. .. ..$ threshold    : NULL
#>   .. .. ..$ ci_role      : chr "LL"
#>   .. .. ..$ ci_pair      : chr "95% CI: UL"
#>   .. .. ..$ ci_label     : chr "95% CI"
#>   .. .. ..$ is_df        : logi FALSE
#>   .. .. ..$ display_label: chr "95% CI"
#>   .. ..$ 95% CI: UL:List of 11
#>   .. .. ..$ token        : chr "ci"
#>   .. .. ..$ model_id     : chr "M1"
#>   .. .. ..$ source_field : chr "ci_high"
#>   .. .. ..$ precision    : int 2
#>   .. .. ..$ p_style      : NULL
#>   .. .. ..$ threshold    : NULL
#>   .. .. ..$ ci_role      : chr "UL"
#>   .. .. ..$ ci_pair      : chr "95% CI: LL"
#>   .. .. ..$ ci_label     : chr "95% CI"
#>   .. .. ..$ is_df        : logi FALSE
#>   .. .. ..$ display_label: chr "95% CI"
#>   .. ..$ p         :List of 11
#>   .. .. ..$ token        : chr "p"
#>   .. .. ..$ model_id     : chr "M1"
#>   .. .. ..$ source_field : chr "p_value"
#>   .. .. ..$ precision    : int 3
#>   .. .. ..$ p_style      : chr "apa"
#>   .. .. ..$ threshold    : num 0.001
#>   .. .. ..$ ci_role      : NULL
#>   .. .. ..$ ci_pair      : NULL
#>   .. .. ..$ ci_label     : NULL
#>   .. .. ..$ is_df        : logi FALSE
#>   .. .. ..$ display_label: chr "p"
#>   ..$ spanners          : NULL
#>   ..$ ci_pairs          :List of 1
#>   .. ..$ :List of 2
#>   .. .. ..$ label: chr "95% CI"
#>   .. .. ..$ cols : int [1:2] 4 5
#>   ..$ format_spec       :List of 9
#>   .. ..$ decimal_mark      : chr "."
#>   .. ..$ digits            : int 2
#>   .. ..$ p_digits          : int 3
#>   .. ..$ effect_size_digits: int 2
#>   .. ..$ fit_digits        : int 2
#>   .. ..$ ic_digits         : int 1
#>   .. ..$ p_style           : chr "apa"
#>   .. ..$ p_threshold       : num 0.001
#>   .. ..$ ci_level          : num 0.95
#>  - attr(*, "padding")= int 0
#>  - attr(*, "fit_stats_layout")= chr "first_col"
```

``` r

out <- table_regression(
  lm(wellbeing_score ~ age + sex + smoking, data = sochealth),
  output = "long"
)
out[, c("model_id", "term", "estimate_type", "estimate",
        "std.error", "p.value")]
#>   model_id        term estimate_type    estimate  std.error       p.value
#> 1       M1 (Intercept)             B 65.20085505 1.65670747 1.591088e-216
#> 2       M1         age             B  0.04649213 0.03069709  1.301575e-01
#> 3       M1   sexFemale             B          NA         NA            NA
#> 4       M1     sexMale             B  3.85579323 0.90528970  2.216170e-05
#> 5       M1   smokingNo             B          NA         NA            NA
#> 6       M1  smokingYes             B -1.71871310 1.10751281  1.209641e-01
```

``` r

pkgdown_dark_gt(
  table_regression(
    lm(wellbeing_score ~ age + sex + smoking, data = sochealth),
    output = "gt"
  )
)
```

[TABLE]

*Note.* Linear regression. Std. errors: classical (OLS).

## broom integration

[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
returns a long tibble with one row per `(model_id, term, estimate_type)`
and broom-canonical column names (`estimate`, `std.error`, `conf.low`,
`conf.high`, `statistic`, `p.value`).
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
returns one row per `(model_id, outcome)` with model-level statistics;
`df.residual` is preserved as a numeric so cluster-robust Satterthwaite
df flows through verbatim:

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
out <- table_regression(fit, standardized = "refit")

broom::tidy(out)
#> # A tibble: 8 × 15
#>   model_id outcome     term  estimate_type estimate std.error conf.low conf.high
#>   <chr>    <chr>       <chr> <chr>            <dbl>     <dbl>    <dbl>     <dbl>
#> 1 M1       wellbeing_… (Int… B              65.2       1.66    62.0      68.5   
#> 2 M1       wellbeing_… (Int… beta           -0.0961    0.0431  -0.181    -0.0116
#> 3 M1       wellbeing_… age   B               0.0465    0.0307  -0.0137    0.107 
#> 4 M1       wellbeing_… age   beta            0.0439    0.0290  -0.0130    0.101 
#> 5 M1       wellbeing_… sexM… B               3.86      0.905    2.08      5.63  
#> 6 M1       wellbeing_… sexM… beta            0.247     0.0579   0.133     0.360 
#> 7 M1       wellbeing_… smok… B              -1.72      1.11    -3.89      0.454 
#> 8 M1       wellbeing_… smok… beta           -0.110     0.0708  -0.249     0.0290
#> # ℹ 7 more variables: statistic <dbl>, df <dbl>, p.value <dbl>,
#> #   test_type <chr>, is_intercept <lgl>, factor_term <chr>, factor_level <chr>
broom::glance(out)
#> # A tibble: 1 × 15
#>   model_id outcome       nobs weighted_nobs r.squared adj.r.squared omega2 sigma
#>   <chr>    <chr>        <int>         <dbl>     <dbl>         <dbl>  <dbl> <dbl>
#> 1 M1       wellbeing_s…  1175            NA    0.0190        0.0165 0.0165  15.5
#> # ℹ 7 more variables: rmse <dbl>, f2 <dbl>, AIC <dbl>, AICc <dbl>, BIC <dbl>,
#> #   deviance <dbl>, df.residual <dbl>
```

The long format is the right entry point when the table is one step in a
larger pipeline — saving to disk for the manuscript appendix, faceting
by subgroup, or feeding a downstream post-estimation analysis. The
`tidy()` output keeps the `estimate_type` column so the same data frame
can hold rows for B, β, AME, and per-coefficient effect-size estimates
without ambiguity.

## See also

- [`vignette("table-continuous-lm", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/table-continuous-lm.md)
  for the one-predictor-by-many-outcomes counterpart (estimated marginal
  means, contrast or slope, four effect-size families).
- [`vignette("summary-tables-reporting", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.md)
  for an end-to-end reporting workflow combining the spicy summary-
  table helpers.

## References

Aiken, L. S., and West, S. G. (1991). *Multiple Regression: Testing and
Interpreting Interactions*. Sage.

American Psychological Association (2020). *Publication Manual of the
American Psychological Association* (7th ed.). Section 6.46.

Cohen, J., Cohen, P., West, S. G., and Aiken, L. S. (2003). *Applied
Multiple Regression / Correlation Analysis for the Behavioral Sciences*
(3rd ed.). Lawrence Erlbaum.

Gelman, A. (2008). Scaling regression inputs by dividing by two standard
deviations. *Statistics in Medicine*, 27(15), 2865–2873.

Gelman, A., Hill, J., and Yajima, M. (2012). Why we (usually) don’t have
to worry about multiple comparisons. *Journal of Research on Educational
Effectiveness*, 5(2), 189–211.

Greenland, S. (2017). For and against methodologies: Some perspectives
on recent causal and statistical inference debates. *European Journal of
Epidemiology*, 32(1), 3–20.

Harrell, F. E. (2015). *Regression Modeling Strategies* (2nd ed.).
Springer. Section 5.4 on multiplicity.

Hosmer, D. W., Lemeshow, S., and Sturdivant, R. X. (2013). *Applied
Logistic Regression* (3rd ed.). Wiley.

Imbens, G. W., and Kolesár, M. (2016). Robust standard errors in small
samples: Some practical advice. *Review of Economics and Statistics*,
98(4), 701–712.

Long, J. S., and Ervin, L. H. (2000). Using heteroscedasticity
consistent standard errors in the linear regression model. *The American
Statistician*, 54(3), 217–224.

Long, J. S., and Freese, J. (2014). *Regression Models for Categorical
Dependent Variables Using Stata* (3rd ed.). Stata Press.

McFadden, D. (1974). Conditional logit analysis of qualitative choice
behavior. In P. Zarembka (Ed.), *Frontiers in Econometrics*
(pp. 105–142). Academic Press.

Menard, S. (2004). Six approaches to calculating standardized logistic
regression coefficients. *The American Statistician*, 58(3), 218–223.

Menard, S. (2011). Standards for standardized logistic regression
coefficients. *Social Forces*, 89(4), 1409–1428.

Mood, C. (2010). Logistic regression: Why we cannot do what we think we
can do, and what we can do about it. *European Sociological Review*,
26(1), 67–82.

Nagelkerke, N. J. D. (1991). A note on a general definition of the
coefficient of determination. *Biometrika*, 78(3), 691–692.

Olejnik, S., and Algina, J. (2003). Generalized eta and omega squared
statistics: Measures of effect size for some common research designs.
*Psychological Methods*, 8(4), 434–447.

Pustejovsky, J. E., and Tipton, E. (2018). Small-sample methods for
cluster-robust variance estimation and hypothesis testing in fixed
effects models. *Journal of Business & Economic Statistics*, 36(4),
672–683.

Rothman, K. J. (1990). No adjustments are needed for multiple
comparisons. *Epidemiology*, 1(1), 43–46.

Smithson, M. (2003). *Confidence Intervals*. Sage.

Steiger, J. H. (2004). Beyond the F test: Effect size confidence
intervals and tests of close fit in the analysis of variance and
contrast analysis. *Psychological Methods*, 9(2), 164–182.

Tjur, T. (2009). Coefficients of determination in logistic regression
models — A new proposal: The coefficient of discrimination. *The
American Statistician*, 63(4), 366–372.

Venables, W. N., and Ripley, B. D. (2002). *Modern Applied Statistics
with S* (4th ed.). Springer. Section 7.2 on profile likelihood.

Wasserstein, R. L., Schirm, A. L., and Lazar, N. A. (2019). Moving to a
world beyond “p \< 0.05”. *The American Statistician*, 73(sup1), 1–19.
