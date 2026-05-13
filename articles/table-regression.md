# Regression coefficient tables in R

``` r

library(spicy)

# Complete-case subset for the cluster-robust examples — the cluster
# vector must align with the rows actually used by `lm()`, so we
# drop the 25 rows with at least one NA in the predictor set up front.
sochealth_cc <- na.omit(
  sochealth[, c("wellbeing_score", "bmi", "age", "sex",
                "smoking", "education", "region")]
)
```

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
produces a publication-ready coefficient summary for one or several
[`lm()`](https://rdrr.io/r/stats/lm.html) fits, side-by-side, with the
inferential machinery and the formatting conventions normally found in
Stata’s `regress` / `eststo` workflow or SPSS’s `REGRESSION` output. The
function is fit-first (you pass already-fitted models, not raw data +
formula), the long-format internal representation is built around the
`(model_id, term, estimate_type)` triplet for clean
\[[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)\]\[broom::tidy\]
/
\[[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)\]\[broom::glance\]
exports, and the rendered table accepts heteroskedasticity-consistent,
cluster-robust, bootstrap, or jackknife variance, four standardisation
methods, per-coefficient effect sizes with noncentral CIs, and a
hierarchical-comparison footer.

Two design choices position
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
against the existing R landscape (`modelsummary`, `gtsummary`,
`parameters`, `marginaleffects`):

- **AME with Satterthwaite-corrected degrees of freedom under CR\*
  variance.** When the user requests both Average Marginal Effects and a
  cluster-robust variance estimator,
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  builds the closed-form linear contrast representing each AME and
  passes it to
  \[[`clubSandwich::linear_contrast()`](http://jepusto.github.io/clubSandwich/reference/linear_contrast.md)\]\[clubSandwich::linear_contrast\]
  with `test = "Satterthwaite"`. Existing R tools default to a
  z-asymptotic AME under CR\*, which is anti-conservative for few
  clusters (Pustejovsky and Tipton 2018). The B coefficient and the AME
  therefore share the same inferential regime in the same table.
- **Transparent caveat on standardised coefficients with non-additive
  terms.** When `standardized != "none"` and the model contains an
  interaction or a transformed term
  ([`I()`](https://rdrr.io/r/base/AsIs.html),
  [`poly()`](https://rdrr.io/r/stats/poly.html),
  [`log()`](https://rdrr.io/r/base/Log.html),
  [`splines::ns()`](https://rdrr.io/r/splines/ns.html)), the function
  emits a classed `spicy_caveat` warning at runtime AND auto-documents
  the caveat in the table footer, with method-specific wording. Existing
  tools either compute silently or document only in `?` —
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  is “more pro via transparency, not rejection” (Cohen, Cohen, West, and
  Aiken 2003 §7.7).

Supports `lm` and `glm` (binomial / poisson / Gamma / inverse.gaussian /
quasi families with any link). Mixed-effects models (`merMod`,
`lmerModLmerTest`) are on the roadmap for spicy 0.16+. See the
*Generalised linear models* section below for the glm-specific argument
semantics.

## Basic usage

Pass a fitted [`lm()`](https://rdrr.io/r/stats/lm.html) object. The
default rendering returns a single-model table with `B`, `SE`, `95% CI`,
and `p`, plus a fit-stats footer (`n`, `R²`, `Adj.R²`):

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
#>    Female (ref.) │    —     —           —         —     
#>    Male          │    3.86  0.91  [ 2.08,  5.63]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │    —     —           —         —     
#>    Yes           │   -1.72  1.11  [-3.89,  0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
```

Reference levels of factor predictors carry the `(ref.)` annotation and
an em-dash in the statistic columns. The vcov footer line names the
variance estimator in plain English.

## Heteroskedasticity-consistent variance

Set `vcov = "HC*"` for sandwich-style standard errors via
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
#>    Female (ref.) │    —     —           —         —     
#>    Male          │    3.86  0.91  [ 2.07,  5.64]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │    —     —           —         —     
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

## Cluster-robust variance

For clustered observations, `vcov = "CR*"` requests cluster-robust
variance via
\[[`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)\]\[clubSandwich::vcovCR\],
with the cluster identifier supplied through `cluster`. The orchestrator
detects the column name and prints it in the footer:

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth_cc)
table_regression(
  fit,
  vcov = "CR2",
  cluster = sochealth_cc$region
)
#> Registered S3 method overwritten by 'clubSandwich':
#>   method    from    
#>   bread.mlm sandwich
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.00  1.74  [60.49, 69.51]  <.001 
#>  age             │    0.05  0.04  [-0.05,  0.15]   .247 
#>  sex:            │                                      
#>    Female (ref.) │    —     —           —         —     
#>    Male          │    3.88  0.85  [ 1.68,  6.07]   .006 
#>  smoking:        │                                      
#>    No (ref.)     │    —     —           —         —     
#>    Yes           │   -1.68  1.55  [-5.72,  2.37]   .331 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1163                                 
#>  R²              │    0.02                              
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: cluster-robust (CR2), clusters by region.
```

`CR2` (the Bell-McCaffrey adjustment) is the recommended default under
few clusters (Pustejovsky and Tipton 2018; Imbens and Kolesár 2016).
Coefficient inference uses
\[[`clubSandwich::coef_test()`](http://jepusto.github.io/clubSandwich/reference/coef_test.md)\]\[clubSandwich::coef_test\]
with Satterthwaite-corrected degrees of freedom — visible in the footer
when AME is also requested (next section).

## Average Marginal Effects with Satterthwaite df

Add `"ame"` to `show_columns` to display the average marginal effect of
each predictor. Use `"ame_ci"` and `"ame_p"` for the corresponding CI
and p-value columns; the shortcut `"all_ame"` expands to
`c("ame", "ame_se", "ame_ci", "ame_p")`. When the variance estimator is
cluster-robust,
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
constructs the linear contrast representing each AME and inverts it
through
[`clubSandwich::linear_contrast()`](http://jepusto.github.io/clubSandwich/reference/linear_contrast.md)
with the Satterthwaite framework, so B and AME share the same
t-distribution with the same df:

``` r

fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth_cc)
table_regression(
  fit,
  vcov = "CR2",
  cluster = sochealth_cc$region,
  show_columns = c("b", "se", "ci", "p", "ame", "ame_ci", "ame_p")
)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p     AME    AME 95% CI   
#> ─────────────────┼────────────────────────────────────────────────────────────
#>  (Intercept)     │   65.00  1.74  [60.49, 69.51]  <.001                       
#>  age             │    0.05  0.04  [-0.05,  0.15]   .247   0.05  [-0.05, 0.15] 
#>  sex:            │                                                            
#>    Female (ref.) │    —     —           —         —       —           —       
#>    Male          │    3.88  0.85  [ 1.68,  6.07]   .006   3.88  [ 1.68, 6.07] 
#>  smoking:        │                                                            
#>    No (ref.)     │    —     —           —         —       —           —       
#>    Yes           │   -1.68  1.55  [-5.72,  2.37]   .331  -1.68  [-5.72, 2.37] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1163                                                       
#>  R²              │    0.02                                                    
#>  Adj.R²          │    0.02                                                    
#> 
#>  Variable        │ AME p 
#> ─────────────────┼───────
#>  (Intercept)     │       
#>  age             │  .247 
#>  sex:            │       
#>    Female (ref.) │ —     
#>    Male          │  .006 
#>  smoking:        │       
#>    No (ref.)     │ —     
#>    Yes           │  .331 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  n               │       
#>  R²              │       
#>  Adj.R²          │       
#> 
#> Note. Linear regression.
#> Std. errors: cluster-robust (CR2), clusters by region.
#> AME inference: t-distribution with Satterthwaite-corrected df (Pustejovsky & Tipton 2018) via `clubSandwich::linear_contrast()`.
```

Note that `"p"` always refers to the B (or β) coefficient, never to the
AME. To display the AME-specific p-value, include `"ame_p"` in
`show_columns`. Placing `"ame"` after `"p"` makes the “which p belongs
to what” reading unambiguous.

The footer now reads “*AME inference: t-distribution with
Satterthwaite-corrected df (Pustejovsky and Tipton 2018) via
[`clubSandwich::linear_contrast()`](http://jepusto.github.io/clubSandwich/reference/linear_contrast.md)*”.
For non-CR variance estimators the AME column delegates to
\[[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)\]\[marginaleffects::avg_slopes\]
with a df argument coherent with B (residual df for classical and HC,
asymptotic z for bootstrap and jackknife).

## Standardised coefficients

`standardized` controls the standardisation method. Four are available;
the choice is consequential and the differences are documented in the
literature (Cohen et al. 2003 §3.4; Gelman 2008):

- `"refit"` — refit on z-scored outcome and predictors. The gold
  standard; produces β identical to `lm.beta` and to SPSS / Stata
  `Beta`.
- `"posthoc"` — post-hoc rescaling `β = B × SD(X) / SD(Y)`. Matches
  `effectsize::standardize_parameters(method = "basic")` and
  `parameters::model_parameters(standardize = "basic")`.
- `"basic"` — like posthoc but factor dummies stay on the 0/1 scale
  rather than being z-scored.
- `"smart"` — Gelman’s (2008) recommendation: numeric predictors divided
  by `2 × SD(X)`; binary predictors centred only.

When non-`"none"`, the `"beta"` token is auto-injected into
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
#>    Female (ref.) │    —      —     —           —         —     
#>    Male          │    3.86   0.25  0.91  [ 2.08,  5.63]  <.001 
#>  smoking:        │                                             
#>    No (ref.)     │    —      —     —           —         —     
#>    Yes           │   -1.72  -0.11  1.11  [-3.89,  0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                        
#>  R²              │    0.02                                     
#>  Adj.R²          │    0.02                                     
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
```

For models with interactions or transformed predictors, the function
emits a `spicy_caveat` warning AND prints a method-specific caveat in
the footer (Aiken and West 1991; Cohen et al. 2003 §7.7). This is the
“transparency over rejection” choice — the table is computed and
displayed, with the limitation made explicit at the point of use.

## Per-coefficient effect sizes

Three partial effect-size tokens are available — Cohen’s f²
(`partial_f2`), Pearson’s partial η² (`partial_eta2`), and the
Olejnik-Algina bias-corrected partial ω² (`partial_omega2`). Each
estimate has a CI derived from noncentral-F inversion (Steiger 2004;
Smithson 2003), exposed as a separate `<token>_ci` column
(`partial_f2_ci`, `partial_eta2_ci`, `partial_omega2_ci`). The group
shortcuts `"all_f2"`, `"all_eta2"`, `"all_omega2"` expand to the pair in
one go:

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
#>    Female (ref.) │    —     —      —          —        —          —       
#>    Male          │    3.57  <.001  0.02  [0.01, 0.03]  0.02  [0.01, 0.03] 
#>  smoking:        │                                                        
#>    No (ref.)     │    —     —      —          —        —          —       
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
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).
```

The η² point estimate matches `effectsize::eta_squared(partial = TRUE)`
on a Type-II reference (`car::Anova`) to machine epsilon. The ω² point
estimate matches `effectsize::omega_squared(partial = TRUE)` to machine
epsilon, using the Olejnik and Algina (2003) formula
`((F-1) × df1) / (F × df1 + N - df1)`. CIs use the Steiger (2004)
inversion bounds, which always bracket the corresponding bias-corrected
point estimate.

For factor predictors with k levels, the partial F-test is the joint
(k-1) df Wald test, so the same effect-size value is broadcast across
all non-reference dummies (and reference rows show an em-dash).

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
#>    Female (ref.) │    —     —           —         —     
#>    Male          │    3.57  0.81  [ 1.99,  5.15]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │    —     —           —         —     
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
robust SE with a family-wise correction is fully supported, e.g.
`vcov = "HC3", p_adjust = "holm"`. The adjustment is applied **before**
`keep` / `drop` filtering, so the family stays the model’s full
coefficient set and the displayed adjusted p-values reflect the right
denominator regardless of which subset is shown (matching the
`modelsummary` and `parameters` convention).

A methodological note. Adjusting the p-values of every coefficient of a
single regression model is *not* the standard convention in
social-science or clinical reporting (Rothman 1990; Greenland 2017;
Harrell *Regression Modeling Strategies* §5.4; Gelman, Hill & Yajima
2012; APA Manual 7 §6.46). Each coefficient tests a scientifically
distinct hypothesis on a distinct predictor, which is not the situation
that family-wise procedures were designed for. The default `"none"`
reflects this consensus. Adjustment is nonetheless legitimate in three
contexts: (i) mass screening with many candidate predictors and no prior
hypothesis (typically `"BH"` / FDR), (ii) pre-registered multi-endpoint
confirmatory designs (typically `"holm"`), or (iii) when a reviewer or
SAP explicitly requests it. spicy exposes the argument under the same
“transparency over rejection” rule used for `standardized`: the tool is
here, the methodological choice is yours, and the footer makes the
choice visible to the reader.

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
#>    No (ref.) │    —     —           —        —     
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
#>    Female (ref.) │    —     —           —         —     
#>    Male          │    3.50  0.81  [ 1.91,  5.10]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │    —     —           —         —     
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

## Multiple models side by side

Pass a list of [`lm()`](https://rdrr.io/r/stats/lm.html) fits. The
default column layout puts each model in its own panel with its own
`B / SE / CI / p`. When the dependent variables differ across models, an
“Outcome” row is auto-added at the top of the body so each model is
unambiguously labelled:

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
#>    Female (ref.) │    —     —     —         —     —     —     
#>    Male          │    3.86  0.91  <.001     0.51  0.22   .018 
#>  smoking:        │                                            
#>    No (ref.)     │    —     —     —         —     —     —     
#>    Yes           │   -1.72  1.11   .121    -0.06  0.26   .822 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                  1163                 
#>  R²              │    0.02                  0.02              
#>  Adj.R²          │    0.02                  0.02              
#> 
#> Note. Linear regression models.
#> Std. errors: classical (OLS).
```

When all models share the dependent variable, the outcome row is
suppressed (the DV appears in the title). Use a named list — e.g.
`list(Crude = m1, Adjusted = m2)` — to control the column-header labels;
pass `model_labels = c(...)` to override the names; pass
`outcome_labels = FALSE` to suppress the outcome row regardless.

## Hierarchical / nested regression

Set `nested = TRUE` to add a “Model comparison” footer block with the
change statistics for each adjacent pair (Cohen et al. 2003 §5.4). The
default tokens for `lm` are `r2_change`, `F`, and `p` — the
APA-conventional layout for hierarchical regression:

``` r

m1 <- lm(wellbeing_score ~ age + sex,                 data = sochealth_cc)
m2 <- lm(wellbeing_score ~ age + sex + smoking,       data = sochealth_cc)
m3 <- lm(wellbeing_score ~ age + sex + smoking + bmi, data = sochealth_cc)
table_regression(list(m1, m2, m3), nested = TRUE)
#> Hierarchical linear regression: wellbeing_score
#> 
#>                          Model 1               Model 2            Model 3    
#>                    ────────────────────  ────────────────────  ───────────── 
#>  Variable        │    B      SE     p       B      SE     p       B      SE  
#> ─────────────────┼───────────────────────────────────────────────────────────
#>  (Intercept)     │   64.70  1.66  <.001    65.00  1.67  <.001    80.57  3.37 
#>  age             │    0.05  0.03   .118     0.05  0.03   .109     0.07  0.03 
#>  sex:            │                                                           
#>    Female (ref.) │    —     —     —         —     —     —         —     —    
#>    Male          │    3.89  0.91  <.001     3.88  0.91  <.001     4.21  0.90 
#>  smoking:        │                                                           
#>    No (ref.)     │    —     —     —         —     —     —         —     —    
#>    Yes           │                         -1.68  1.11   .132    -1.71  1.10 
#>  bmi             │                                               -0.65  0.12 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1163                  1163                  1163          
#>  R²              │    0.02                  0.02                  0.04       
#>  Adj.R²          │    0.02                  0.02                  0.04       
#> 
#>                    Model 
#>                    ───── 
#>  Variable        │   p   
#> ─────────────────┼───────
#>  (Intercept)     │ <.001 
#>  age             │  .019 
#>  sex:            │       
#>    Female (ref.) │ —     
#>    Male          │ <.001 
#>  smoking:        │       
#>    No (ref.)     │ —     
#>    Yes           │  .119 
#>  bmi             │ <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌
#>  n               │       
#>  R²              │       
#>  Adj.R²          │       
#> 
#> Note. Linear regression models.
#> Std. errors: classical (OLS).
#> 
#> ── Model comparison ──
#> Model 2 vs Model 1: ΔR² = +0.00, F = +2.28, p = .132
#> Model 3 vs Model 2: ΔR² = +0.02, F = +28.13, p = <.001
```

Validation is strict: identical `nobs` AND identical response variable
across all models, otherwise a `spicy_invalid_input` error explains that
R’s listwise deletion may produce different `n` per model and suggests
refitting on the common subset (the reason for `sochealth_cc` being
prepared at the top of the vignette). Other comparison tokens are
available — `LRT` (likelihood-ratio χ²), `AIC` / `AICc` / `BIC`
(deltas), `f2_change`, `deviance_change` — selectable via
`nested_stats`.

## Generalised linear models (`glm`)

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
accepts any [`glm()`](https://rdrr.io/r/stats/glm.html) fit. Inference
defaults to the z-asymptotic Wald regime that matches `summary.glm`,
[`parameters::model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.html),
Stata’s `logit, or` and SPSS `LOGISTIC REGRESSION`. The title becomes
family-aware (“Logistic regression”, “Poisson regression”, “Probit
regression”, …) and the default footer block for
`nobs / pseudo_r2_mcfadden / pseudo_r2_nagelkerke / AIC` swaps in
instead of `R² / Adj.R²`:

``` r

fit <- glm(am ~ mpg + wt, data = mtcars, family = binomial)
table_regression(fit)
#> Logistic regression: am
#> 
#>  Variable        │   B     SE        95% CI        p   
#> ─────────────────┼─────────────────────────────────────
#>  (Intercept)     │ 25.89  12.19  [  1.99, 49.79]  .034 
#>  mpg             │ -0.32   0.24  [ -0.79,  0.15]  .176 
#>  wt              │ -6.42   2.55  [-11.41, -1.42]  .012 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 32                                  
#>  R² (McFadden)   │  0.60                               
#>  R² (Nagelkerke) │  0.75                               
#>  AIC             │ 23.2                                
#> 
#> Note. Logistic regression.
#> Std. errors: classical (MLE inverse Hessian).
```

### Response-scale display: `exponentiate = TRUE`

Set `exponentiate = TRUE` to switch the `B` column to the response scale
and rebrand its header per family / link: `OR` for `binomial(logit)`,
`IRR` for `poisson(log)`, `HR` for `binomial(cloglog)`, `RR` for
`binomial(log)`, `MR` for `Gamma(log)`, and the generic `exp(B)`
otherwise. The standard error follows the delta-method approximation
`SE_OR = OR × SE_log-odds` (Stata `logit, or`;
[`parameters::model_parameters()`](https://easystats.github.io/parameters/reference/model_parameters.html));
the test statistic and the p-value are invariant under monotone
transformation:

``` r

table_regression(fit, exponentiate = TRUE)
#> Logistic regression: am
#> 
#>  Variable        │    OR         SE          95% CI        p   
#> ─────────────────┼─────────────────────────────────────────────
#>  (Intercept)     │  1.75e+11  2.13e+12  [7.30, 4.18e+21]  .034 
#>  mpg             │  0.72      0.17      [0.45, 1.16    ]  .176 
#>  wt              │  0.00      0.00      [0.00, 0.24    ]  .012 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 32                                          
#>  R² (McFadden)   │  0.60                                       
#>  R² (Nagelkerke) │  0.75                                       
#>  AIC             │ 23.2                                        
#> 
#> Note. Logistic regression.
#> Std. errors: classical (MLE inverse Hessian).
#> Coefficients exponentiated and displayed as OR; CI bounds exponentiated; SE delta-method approximation: SE_OR = OR × SE_link.
```

### Term-level partial chi-square

`partial_chi2` is the glm analog of `partial_f2`: for each model term,
the partial likelihood-ratio chi-square via `drop1(test = "LRT")` (SAS
PROC LOGISTIC `TYPE3`; Long & Freese 2014 §3.5; Allison “TYPE3”).
Rendered as `value (df)` so factor terms (k−1 df) and numeric terms (1
df) read at a glance:

``` r

mt <- mtcars; mt$cyl <- factor(mt$cyl)
fit2 <- glm(am ~ mpg + cyl, data = mt, family = binomial)
table_regression(fit2, show_columns = c("b", "partial_chi2", "p"))
#> Logistic regression: am
#> 
#>  Variable        │   B       χ²       p   
#> ─────────────────┼────────────────────────
#>  (Intercept)     │ -8.34             .104 
#>  mpg             │  0.37  4.53 (1)   .080 
#>  cyl:            │                        
#>    4 (ref.)      │  —     —         —     
#>    6             │  0.73  0.27 (2)   .605 
#>    8             │  0.70  0.27 (2)   .720 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 32                     
#>  R² (McFadden)   │  0.32                  
#>  R² (Nagelkerke) │  0.47                  
#>  AIC             │ 37.4                   
#> 
#> Note. Logistic regression.
#> Std. errors: classical (MLE inverse Hessian).
```

### Standardised coefficients: `refit` and the new `pseudo`

For `glm`, `standardized = "refit"` z-scores numeric *predictors* only
and refits the model (the response stays on its observed scale since the
link is fixed). This is Long & Freese 2014 §4.3.4 “x-standardization”.
The other algebraic methods (`"posthoc"`, `"basic"`, `"smart"`) apply
X-only scaling per the `parameters` / `effectsize` convention.

`standardized = "pseudo"` (`glm` only) is the Menard (2004, 2011)
*fully* standardised coefficient, scaling by `SD(X) / SD(Y*)` where `Y*`
is the latent variable on the link scale and
`SD(Y*) = sqrt(var(linear-predictor) + var_link)` with `var_link` = π²/3
for logit, 1 for probit, π²/6 for cloglog. Defined for binomial
families; non-binomial returns NA with a `spicy_caveat`:

``` r

table_regression(fit, standardized = "pseudo")
```

### Average Marginal Effects (AME)

The `ame` token returns response-scale marginal effects via
[`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html).
For `glm`, AME is computed as the average of `dE[Y|X]/dx` over the
observed sample (so for logistic regression the displayed AME is in
probability units, not log-odds). Under cluster-robust variance
(`vcov = "CR2"` etc.), the inference uses Satterthwaite df from
[`clubSandwich::coef_test()`](http://jepusto.github.io/clubSandwich/reference/coef_test.md)
on the dominant underlying coefficient — Pustejovsky & Tipton (2018) §4
approximation for nonlinear contrasts:

``` r

table_regression(fit, show_columns = c("b", "p", "ame", "ame_ci", "ame_p"))
```

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
```

### Hierarchical glm (LRT)

`nested = TRUE` on a list of nested glm fits adds a “Model comparison”
block whose default tokens are `c("LRT", "p")` — the APA-conventional
layout for hierarchical logistic regression (Hosmer & Lemeshow §3.5;
Long & Freese 2014 §3.6). The chi-square statistic comes from
`anova(test = "LRT")`. `r2_change` and `F` are not defined for glm and
return em-dashes if explicitly requested:

``` r

m1 <- glm(am ~ mpg,                 data = mtcars, family = binomial)
m2 <- glm(am ~ mpg + wt,            data = mtcars, family = binomial)
m3 <- glm(am ~ mpg + wt + factor(cyl), data = mtcars, family = binomial)
table_regression(list(m1, m2, m3), nested = TRUE)
```

### Gaussian glm caveat

A [`glm()`](https://rdrr.io/r/stats/glm.html) with `family = gaussian`
and `link = "identity"` is mathematically equivalent to
[`lm()`](https://rdrr.io/r/stats/lm.html) but lacks the
variance-explained effect-size family (`partial_f2 / η² / ω²`) and the
Satterthwaite- corrected AME path. Following the *transparency over
rejection* rule, spicy accepts the fit and emits a `spicy_caveat`
suggesting a refit with [`lm()`](https://rdrr.io/r/stats/lm.html).

## Significance stars

Stars are off by default. APA 7 §6.46 explicitly discourages them, and
the modern R consensus (`modelsummary`, `gtsummary`, `parameters`,
`fixest`) defaults likewise OFF. Set `stars = TRUE` for the APA preset
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
#>    Female (ref.) │    —        —           —         —     
#>    Male          │    3.86***  0.91  [ 2.08,  5.63]  <.001 
#>  smoking:        │                                         
#>    No (ref.)     │    —        —           —         —     
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

Stars suffix the B column (or β when standardisation is requested); the
threshold mapping is auto-documented in the footer. The `p` column
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
#>  $ B       : chr  "  65.20" "   0.05" "       " "   —   " ...
#>  $ SE      : chr  "1.66" "0.03" "    " "—   " ...
#>  $ 95% CI  : chr  "[61.95, 68.45]" "[-0.01,  0.11]" "              " "      —       " ...
#>  $ p       : chr  "<.001" " .130" "     " "—    " ...
#>  - attr(*, "title")= chr "Linear regression: wellbeing_score"
#>  - attr(*, "note")= chr "Note. Linear regression.\nStd. errors: classical (OLS)."
#>  - attr(*, "col_spec")=List of 4
#>   ..$ :List of 5
#>   .. ..$ col_name     : chr "B"
#>   .. ..$ token        : chr "b"
#>   .. ..$ model_id     : chr "M1"
#>   .. ..$ estimate_type: chr "B"
#>   .. ..$ fields       : chr "estimate"
#>   ..$ :List of 5
#>   .. ..$ col_name     : chr "SE"
#>   .. ..$ token        : chr "se"
#>   .. ..$ model_id     : chr "M1"
#>   .. ..$ estimate_type: chr "B"
#>   .. ..$ fields       : chr "se"
#>   ..$ :List of 5
#>   .. ..$ col_name     : chr "95% CI"
#>   .. ..$ token        : chr "ci"
#>   .. ..$ model_id     : chr "M1"
#>   .. ..$ estimate_type: chr "B"
#>   .. ..$ fields       : chr [1:2] "ci_low" "ci_high"
#>   ..$ :List of 5
#>   .. ..$ col_name     : chr "p"
#>   .. ..$ token        : chr "p"
#>   .. ..$ model_id     : chr "M1"
#>   .. ..$ estimate_type: chr "B"
#>   .. ..$ fields       : chr "p_value"
#>  - attr(*, "group_sep_rows")= int 9
#>  - attr(*, "align")= chr "decimal"
#>  - attr(*, "padding")= int 0
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

| Linear regression: wellbeing_score |  |  |  |  |
|----|----|----|----|----|
| Variable | B | SE | 95% CI | p |
| (Intercept) | 65.20 | 1.66 | \[61.95, 68.45\] | \<.001 |
| age | 0.05 | 0.03 | \[-0.01, 0.11\] | .130 |
| sex: |  |  |  |  |
| Female (ref.) | — | — | — | — |
| Male | 3.86 | 0.91 | \[ 2.08, 5.63\] | \<.001 |
| smoking: |  |  |  |  |
| No (ref.) | — | — | — | — |
| Yes | -1.72 | 1.11 | \[-3.89, 0.45\] | .121 |
| n | 1175 |  |  |  |
| R² | 0.02 |  |  |  |
| Adj.R² | 0.02 |  |  |  |
| Note. Linear regression. Std. errors: classical (OLS). |  |  |  |  |

## broom integration

[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
returns a long tibble with one row per `(model_id, term, estimate_type)`
and broom-canonical column names (`estimate`, `std.error`, `conf.low`,
`conf.high`, `statistic`, `p.value`).
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
returns one row per `(model_id, outcome)` with model-level statistics,
with `df.residual` kept numeric so cluster-robust Satterthwaite df is
preserved verbatim:

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

This makes
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
outputs ready for downstream pipelines — `gtsummary`, `modelsummary`,
`parameters` — without bespoke glue.

## See also

- [`vignette("table-continuous-lm", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/table-continuous-lm.md)
  for the one-predictor-by-many-outcomes counterpart (estimated marginal
  means, contrast or slope, four effect-size families).
- [`vignette("summary-tables-reporting", package = "spicy")`](https://amaltawfik.github.io/spicy/articles/summary-tables-reporting.md)
  for an end-to-end reporting workflow combining the spicy summary-table
  helpers.

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
