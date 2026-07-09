# Univariable screening table (with optional multivariable merge)

Fits one model per candidate predictor (the *univariable screen*) and
renders them as a single table with one row block per predictor. With
`multivariable = TRUE` (default), the full model containing all
predictors is merged side by side under `"Univariable"` /
`"Multivariable"` column groups – the standard presentation of applied
epidemiology (the `gtsummary::tbl_uvregression()` + `tbl_merge()`
workflow).

## Usage

``` r
table_regression_uv(
  data,
  outcome,
  predictors,
  method = c("glm", "lm"),
  family = stats::binomial(),
  multivariable = TRUE,
  complete_cases = FALSE,
  show_columns = c("n", "b", "ci", "p"),
  title = NULL,
  ...
)
```

## Arguments

- data:

  A data frame.

- outcome:

  The outcome column (unquoted name, tidyselect).

- predictors:

  Candidate predictor columns (tidyselect, e.g. `c(age, sex, education)`
  or `where(is.numeric)`). The outcome is dropped from the selection
  automatically.

- method:

  `"glm"` (default) or `"lm"`. Cox screening is planned with the
  survival estimands work.

- family:

  A [stats::family](https://rdrr.io/r/stats/family.html) object for
  `method = "glm"`. Default
  [`binomial()`](https://rdrr.io/r/stats/family.html).

- multivariable:

  Logical, default `TRUE`: merge the full model (all predictors
  together) as a second column group.

- complete_cases:

  Logical, default `FALSE`. `TRUE` restricts ALL models to the rows
  complete on outcome + every predictor (common-sample comparison); the
  reduction is disclosed in the table note.

- show_columns:

  Passed to
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).
  Default `c("n", "b", "ci", "p")` – the `tbl_uvregression` column set;
  `"n"` is the per-predictor sample size. The multivariable group
  carries no `N` column (its single `n` is a fit-statistics row, as in
  the reference layouts).

- title:

  Table title; `NULL` (default) builds
  `"Univariable and multivariable <type> regression: <outcome>"`.

- ...:

  Passed to
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  (`exponentiate`, `vcov`, `cluster`, `p_adjust`, `digits`, `labels`,
  `output`, ...). `show_intercept` defaults to `FALSE` here; `nested` is
  not meaningful for a screen and is refused. `cluster` must be a single
  vector with one value per row of `data`; it is aligned to each fit's
  own estimation sample automatically.

## Value

See
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
(same output contract).

## Sample sizes

By default each univariable model is fit on its **own complete cases**,
so N varies across predictors – that is what the `N` column discloses
(shown on the first row of each block), and a table note states it
whenever the Ns differ. The multivariable model is fit on the complete
cases of **all** its variables (its `n` appears in the fit-statistics
rows). Pass `complete_cases = TRUE` to restrict every model –
univariable and multivariable – to the common complete-case sample.

## Multiplicity

`p_adjust` (passed through to
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md))
treats the whole univariable screen as ONE family (all screened
coefficients together); the multivariable model is its own family, as in
any multi-model table.

## Intercepts

Hidden by default on both sides (each univariable fit has its own
nuisance intercept), matching `gtsummary::tbl_regression()`'s
`intercept = FALSE` default. Pass `show_intercept = TRUE` to display
them.

## References

Batra, N. et al. (Eds.) (2021). *The Epidemiologist R Handbook*,
Univariate and multivariable regression.
<https://epirhandbook.com/en/new_pages/regression.html>

Sjoberg, D.D., Whiting, K., Curry, M., Lavery, J.A., & Larmarange, J.
(2021). Reproducible summary tables with the gtsummary package. *The R
Journal*, 13(1), 570-580.

## Examples

``` r
# \donttest{
table_regression_uv(
  sochealth,
  outcome    = smoking,
  predictors = c(age, sex, education),
  family     = binomial(),
  exponentiate = TRUE
)
#> Ordered factor(s) detected. Polynomial contrasts (the R default for `ordered()`) decompose the factor into orthogonal trend components: `.L` = linear, `.Q` = quadratic, `.C` = cubic, `^k` = degree k. Coefficients are trends across the ordered levels, NOT per-level effects against a reference.
#> ℹ To display per-level (treatment) effects, refit with `factor(x, ordered = FALSE)` or set `options(contrasts = c("contr.treatment", "contr.treatment"))`.
#> This message is displayed once per session.
#> Univariable and multivariable logistic regression: smoking
#> 
#>                              Univariable                Multivariable     
#>                    ───────────────────────────────  ───────────────────── 
#>  Variable        │  N     OR      95% CI       p      OR        95% CI    
#> ─────────────────┼────────────────────────────────────────────────────────
#>  age             │ 1175  1.01  [1.00, 1.01]   .292     1.01  [1.00, 1.02] 
#>  sex:            │                                                        
#>    Female (ref.) │    –   –         –         –         –         –       
#>    Male          │ 1175  0.95  [0.72, 1.26]   .713     0.96  [0.73, 1.28] 
#>  education:      │                                                        
#>    .L            │ 1175  0.53  [0.40, 0.70]  <.001     0.53  [0.40, 0.69] 
#>    .Q            │       1.03  [0.81, 1.29]   .832     1.02  [0.81, 1.29] 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │                                  1175                  
#>  R² (McFadden)   │                                     0.02               
#>  R² (Nagelkerke) │                                     0.03               
#>  AIC             │                                  1200.9                
#> 
#>                    Mult… 
#>                    ───── 
#>  Variable        │   p   
#> ─────────────────┼───────
#>  age             │  .214 
#>  sex:            │       
#>    Female (ref.) │  –    
#>    Male          │  .800 
#>  education:      │       
#>    .L            │ <.001 
#>    .Q            │  .852 
#> 
#> Note. Logistic regression models.
#> Std. errors: classical (Fisher information).
#> OR = odds ratio.
#> Coefficients exponentiated and displayed as OR; CI bounds exponentiated.
#> Ordered factor `education`: polynomial trends (.L = linear, .Q = quadratic).
# }
```
