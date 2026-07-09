# Summary tables for APA-style reporting

``` r

library(spicy)
```

spicy’s four reporting helpers cover the full APA Manual 7 table
sequence used in empirical articles:

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  and
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  build **Table 1** (sample characteristics) and **Table 2** (group
  comparisons);
- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  extends Table 2 to the linear-model regime when group means need
  robust SE, weights, or covariate adjustment;
- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  builds **Table 3** (the coefficient table) from one or several fitted
  [`lm()`](https://rdrr.io/r/stats/lm.html) /
  [`glm()`](https://rdrr.io/r/stats/glm.html) models.

The four functions share the same output grammar — the same `output`
formats (`gt`, `tinytable`, `flextable`, `word`, `excel`, `clipboard`),
the same `decimal_mark`, `digits`, `p_digits`, `labels`, and `align`
arguments — so a single reporting workflow can move smoothly from
descriptive to inferential without juggling different APIs. This
vignette focuses on that shared logic; the function-specific articles
cover the methodological options in depth.

## Choose the right function

Use the function that matches the unit you want to report:

| Function | Reports | Selection grammar | Typical additions |
|:---|:---|:---|:---|
| [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md) | Categorical variables (factors, labelled) | `select`, `by` | Chi-squared test, association measure (`phi`, `cramer_v`, `tau_b`, …), confidence interval |
| [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md) | Numeric / continuous variables | `select`, `by` | Group-comparison test (Student / Welch *t*, Wilcoxon, ANOVA, Kruskal–Wallis), effect size (`d`, `g`, `r`, `eta²`, `omega²`) |
| [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md) | Numeric outcomes through one linear model per outcome | `select`, `by` (single predictor) | Robust / cluster-robust / bootstrap / jackknife SE, case weights, additive covariate adjustment, four effect-size families with noncentral CIs |
| [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md) | One or several fitted [`lm()`](https://rdrr.io/r/stats/lm.html) / [`glm()`](https://rdrr.io/r/stats/glm.html) models | Fit-first: pass the model object(s) directly, no `select` / `by` | APA-aligned coefficient table with `B`, `β`, `95% CI`, `p`, AME, robust variance, side-by-side and hierarchical layouts |

In practice, follow the APA sequence:

- start with
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  for smoking, education, or activity — APA Table 1 categorical
  descriptors;
- use
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  for BMI, well-being, or income — Table 1 continuous descriptors and
  Table 2 unadjusted group comparisons;
- switch to
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  when the same comparison must account for survey weights, robust SE,
  or covariate adjustment;
- finish with
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  once the substantive model is fitted — APA Table 3 with all
  predictors, factor groupings, reference rows, and (optionally)
  standardised coefficients, marginal effects, or nested model
  comparisons.

The first three functions live inside a `select` / `by` data-frame
grammar;
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
is **fit-first** — you build the model the usual R way
([`lm()`](https://rdrr.io/r/stats/lm.html) or
[`glm()`](https://rdrr.io/r/stats/glm.html)) and hand the object in. All
four share the post-construction grammar (`output`, `labels`, `digits`,
`decimal_mark`, `align`), so swapping functions never breaks your
rendering pipeline.

## A shared interface

The three descriptive functions share the same core arguments:

``` r

table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Smoking status", "Regular physical activity"),
  output = "tinytable"
)
```

| Variable | Lower secondary |  | Upper secondary |  | Tertiary |  | Total |  | p | Cramer's V |
|----|----|----|----|----|----|----|----|----|----|----|
|  | n | % | n | % | n | % | n | % |  |  |
| Smoking status |     |      |     |      |     |      |     |      | \<.001 | .14 |
|     No | 179 | 68.6 | 415 | 77.0 | 332 | 83.0 | 926 | 77.2 |       |     |
|     Yes |  78 | 29.9 | 112 | 20.8 |  59 | 14.8 | 249 | 20.8 |       |     |
|     (Missing) |   4 |  1.5 |  12 |  2.2 |   9 |  2.2 |  25 |  2.1 |       |     |
| Regular physical activity |     |      |     |      |     |      |     |      | \<.001 | .21 |
|     No | 177 | 67.8 | 310 | 57.5 | 163 | 40.8 | 650 | 54.2 |       |     |
|     Yes |  84 | 32.2 | 229 | 42.5 | 237 | 59.2 | 550 | 45.8 |       |     |

``` r

table_continuous(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  labels = c(
    bmi = "Body mass index",
    wellbeing_score = "Well-being score",
    life_sat_health = "Satisfaction with health"
  ),
  output = "tinytable"
)
```

| Variable | Group | M | SD | Min | Max | 95% CI |  | n | p |
|----|----|----|----|----|----|----|----|----|----|
|  |  |  |  |  |  | LL | UL |  |  |
| Body mass index | Lower secondary | 28.09 |  3.47 | 18.20 |  38.90 | 27.66 | 28.51 | 260 | \<.001 |
|  | Upper secondary | 26.02 |  3.43 | 16.00 |  37.10 | 25.73 | 26.31 | 534 |       |
|  | Tertiary | 24.39 |  3.52 | 16.00 |  33.00 | 24.04 | 24.74 | 394 |       |
| Well-being score | Lower secondary | 57.22 | 15.44 | 18.70 |  97.90 | 55.33 | 59.10 | 261 | \<.001 |
|  | Upper secondary | 68.97 | 13.62 | 26.70 | 100.00 | 67.82 | 70.12 | 539 |       |
|  | Tertiary | 76.85 | 13.23 | 40.40 | 100.00 | 75.55 | 78.15 | 400 |       |
| Satisfaction with health | Lower secondary |  2.71 |  1.20 |  1.00 |   5.00 |  2.57 |  2.86 | 259 | \<.001 |
|  | Upper secondary |  3.53 |  1.19 |  1.00 |   5.00 |  3.43 |  3.63 | 534 |       |
|  | Tertiary |  4.11 |  1.04 |  1.00 |   5.00 |  4.01 |  4.21 | 399 |       |

``` r

table_continuous_lm(
  sochealth,
  select = c(bmi, wellbeing_score, life_sat_health),
  by = education,
  weights = weight
)
#> Continuous outcomes by Highest education level
#> 
#>  Variable                       │ M (Lower secondary)  M (Upper secondary) 
#> ────────────────────────────────┼──────────────────────────────────────────
#>  Body mass index                │        25.96                23.39        
#>  WHO-5 wellbeing index (0-100)  │        67.55                80.88        
#>  Satisfaction with health (1-5) │         3.45                 4.39        
#> 
#>  Variable                       │ M (Tertiary)    p     R²    n   
#> ────────────────────────────────┼─────────────────────────────────
#>  Body mass index                │    26.16      <.001  0.13  1188 
#>  WHO-5 wellbeing index (0-100)  │    66.52      <.001  0.19  1200 
#>  Satisfaction with health (1-5) │     3.39      <.001  0.15  1192
```

The same argument pattern is used in all three cases:

- `select` chooses the reported variables;
- `by` defines the grouping structure;
- `labels` cleans up the row labels;
- `output` decides how the result is rendered or exported.

For model-based continuous tables, the same pattern applies, but `by`
must be a single predictor because one linear model is fit per outcome.

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
joins the same `labels` / `output` / `decimal_mark` / `digits` grammar
but is **fit-first**: rather than expressing model structure inline
through `select` and `by`, you pass one or several already-fitted
[`lm()`](https://rdrr.io/r/stats/lm.html) or
[`glm()`](https://rdrr.io/r/stats/glm.html) objects:

``` r

fit <- lm(
  wellbeing_score ~ age + sex + smoking + physical_activity,
  data = sochealth
)
table_regression(
  fit,
  labels = c(
    age               = "Age (years)",
    sex               = "Sex",
    smoking           = "Smoking status",
    physical_activity = "Regular physical activity"
  ),
  output = "tinytable"
)
```

| Variable                   | B       | SE   | 95% CI |       | p      |
|----------------------------|---------|------|--------|-------|--------|
|                            |         |      | LL     | UL    |        |
| (Intercept)                |   64.18 | 1.69 | 60.87  | 67.49 | \<.001 |
| Age (years)                |    0.04 | 0.03 | -0.02  |  0.10 |  .171  |
| Sex:                       |         |      |        |       |        |
| Female (ref.)              |    –    | –    |  –     |  –    | –      |
| Male                       |    3.88 | 0.90 |  2.11  |  5.65 | \<.001 |
| Smoking status:            |         |      |        |       |        |
| No (ref.)                  |    –    | –    |  –     |  –    | –      |
| Yes                        |   -1.73 | 1.10 | -3.90  |  0.43 |  .117  |
| Regular physical activity: |         |      |        |       |        |
| No (ref.)                  |    –    | –    |  –     |  –    | –      |
| Yes                        |    2.70 | 0.91 |  0.93  |  4.48 |  .003  |
| n                          | 1175    |      |        |       |        |
| R²                         |    0.03 |      |        |       |        |
| Adj.R²                     |    0.02 |      |        |       |        |

Linear regression: wellbeing_score {#tinytable_89eecxpavj13sbxk9hgi
.table .tinytable
style="width: auto; margin-left: auto; margin-right: auto;"
quarto-disable-processing="true"}

*Note.* Linear regression. Std. errors: classical (OLS).

This split is intentional. The descriptive trio (categorical,
continuous, continuous_lm) reports the *data* — `select` and `by`
describe what you want to see.
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
reports the *model* — the model formula has already declared which
predictors, interactions, polynomials, transformations, splines, and
contrasts to report, so passing those again through `select` / `by`
would duplicate the model object’s information and risk diverging from
it.

## A practical reporting sequence

A common report contains both table types, often with the same grouping
variable. For example, you might first summarize categorical health
behaviors, then summarize continuous well-being indicators.

### Categorical variables

``` r

pkgdown_dark_gt(
  table_categorical(
    sochealth,
    select = c(smoking, physical_activity, dentist_12m),
    by = education,
    labels = c(
      "Smoking status",
      "Regular physical activity",
      "Visited a dentist in the last 12 months"
    ),
    output = "gt"
  )
)
```

[TABLE]

### Continuous variables

``` r

pkgdown_dark_gt(
  table_continuous(
    sochealth,
    select = c(bmi, wellbeing_score, life_sat_health),
    by = education,
    labels = c(
      bmi = "Body mass index",
      wellbeing_score = "Well-being score",
      life_sat_health = "Satisfaction with health"
    ),
    p_value = TRUE,
    effect_size = TRUE,
    output = "gt"
  )
)
```

[TABLE]

This keeps the reporting structure consistent while still using the
function that fits each variable type.

### Model-based continuous variables

``` r

pkgdown_dark_gt(
  table_continuous_lm(
    sochealth,
    select = c(bmi, wellbeing_score, life_sat_health),
    by = sex,
    vcov = "HC3",
    statistic = TRUE,
    output = "gt"
  )
)
```

[TABLE]

*Note.* Std. errors: heteroskedasticity-robust (HC3).

This is the better summary-table path when the article is already
organized around simple linear models, weighted analyses, or robust
standard errors.

### The coefficient table

Once the substantive model is fitted,
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
produces the APA Table 3 coefficient summary. The same `output` argument
controls rendering, so the regression table sits in the same reporting
pipeline as the descriptive ones above:

``` r

fit <- lm(
  wellbeing_score ~ age + sex + smoking + physical_activity,
  data = sochealth
)
pkgdown_dark_gt(
  table_regression(
    fit,
    standardized = "refit",
    show_columns = c("b", "beta", "ci", "p"),
    vcov = "HC3",
    output = "gt"
  )
)
```

[TABLE]

*Note.* Linear regression. Std. errors: heteroskedasticity-robust (HC3).
β = standardised coefficient.

The default footer documents the variance estimator and any
methodological choice that affected the rendered values (robust SE,
standardisation method, multiplicity correction) so the inferential
regime is visible without leaving the table.

Side-by-side reporting of competing specifications (e.g., unadjusted
vs. covariate-adjusted, or `lm` vs. `glm`) is supported by passing a
list of fits:

``` r

fit_unadj <- lm(wellbeing_score ~ smoking, data = sochealth)
fit_adj   <- lm(
  wellbeing_score ~ smoking + age + sex + physical_activity,
  data = sochealth
)
pkgdown_dark_gt(
  table_regression(
    list("Unadjusted" = fit_unadj, "Adjusted" = fit_adj),
    show_columns = c("b", "ci", "p"),
    output = "gt"
  )
)
```

[TABLE]

*Note.* Linear regression models. Std. errors: classical (OLS).

For binary or count outcomes, swap
[`lm()`](https://rdrr.io/r/stats/lm.html) for
[`glm()`](https://rdrr.io/r/stats/glm.html) and request response-scale
reporting (odds ratios, incidence rate ratios, etc.):

``` r

fit_glm <- glm(
  smoking ~ age + sex + physical_activity,
  data = sochealth,
  family = binomial()
)
pkgdown_dark_gt(
  table_regression(
    fit_glm,
    exponentiate = TRUE,
    show_columns = c("b", "ci", "p", "ame", "ame_ci"),
    output = "gt"
  )
)
#> Warning: `"ame"` and `"p"` shown without `"ame_p"`: the `p` column is for B (or beta), not the AME. They can differ under non-linear links or interactions.
#> ℹ Add `"ame_p"` to display the AME-specific p-value.
```

[TABLE]

*Note.* Logistic regression. Std. errors: classical (Fisher
information). AME = average marginal effect; OR = odds ratio.
Coefficients exponentiated and displayed as OR; CI bounds exponentiated.

Average marginal effects (`ame`) are useful next to the odds ratio
because they report a probability-scale change for each predictor — the
quantity most reviewers want to interpret directly.

## Choose the output format

All four functions support the same reporting formats:

| Output        | Best use                                     |
|:--------------|:---------------------------------------------|
| `"default"`   | Quick console review in plain ASCII          |
| `"tinytable"` | Quarto or R Markdown documents               |
| `"gt"`        | HTML output with styled reporting tables     |
| `"flextable"` | Office-first workflows; also renders in HTML |
| `"excel"`     | Spreadsheet handoff or downstream editing    |
| `"word"`      | Direct `.docx` export                        |
| `"clipboard"` | Fast pasting into another application        |

Pick the output based on where the table is going, not on the analysis
itself. The underlying selection and grouping pattern stays the same.

If you want an object that fits naturally into Word and PowerPoint
workflows but can also be rendered in HTML documents, `flextable` is a
good choice:

``` r

if (requireNamespace("flextable", quietly = TRUE)) {
  table_continuous(
    sochealth,
    select = c(bmi, wellbeing_score, life_sat_health),
    by = education,
    output = "flextable"
  )
}
```

## Post-process the returned table object

All four summary-table helpers return regular `gt`, `tinytable`, or
`flextable` objects, so you can keep styling them with the native
package API. This includes
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md):
nothing about the fit-first interface changes what the rendering engine
produces.

Use `gt::` functions when you want to keep the `gt` workflow:

``` r

tab <- pkgdown_dark_gt(table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Smoking status", "Regular physical activity"),
  output = "gt"
))

tab |>
  gt::tab_header(
    title = "Health behaviors by education",
    subtitle = "Categorical summary table"
  ) |>
  gt::tab_source_note(
    gt::md("*Percentages are computed within each education group.*")
  )
```

[TABLE]

Use `tinytable::` functions when you want lightweight table-specific
styling:

``` r

tab <- table_categorical(
  sochealth,
  select = c(smoking, physical_activity),
  by = education,
  labels = c("Smoking status", "Regular physical activity"),
  output = "tinytable"
)

tab |>
  tinytable::style_tt(
    i = 2:3,
    j = 2:5,
    background = "red",
    color = "white",
    bold = TRUE
  )
```

| Variable | Lower secondary |  | Upper secondary |  | Tertiary |  | Total |  | p | Cramer's V |
|----|----|----|----|----|----|----|----|----|----|----|
|  | n | % | n | % | n | % | n | % |  |  |
| Smoking status |     |      |     |      |     |      |     |      | \<.001 | .14 |
|     No | 179 | 68.6 | 415 | 77.0 | 332 | 83.0 | 926 | 77.2 |       |     |
|     Yes |  78 | 29.9 | 112 | 20.8 |  59 | 14.8 | 249 | 20.8 |       |     |
|     (Missing) |   4 |  1.5 |  12 |  2.2 |   9 |  2.2 |  25 |  2.1 |       |     |
| Regular physical activity |     |      |     |      |     |      |     |      | \<.001 | .21 |
|     No | 177 | 67.8 | 310 | 57.5 | 163 | 40.8 | 650 | 54.2 |       |     |
|     Yes |  84 | 32.2 | 229 | 42.5 | 237 | 59.2 | 550 | 45.8 |       |     |

Use `flextable::` functions when you want to keep working toward Office
or HTML document output. The example is shown as code here because the
dark pkgdown theme is not a reliable preview of the final `flextable`
HTML rendering:

``` r

if (requireNamespace("flextable", quietly = TRUE)) {
  tab <- table_continuous(
    sochealth,
    select = c(bmi, wellbeing_score),
    by = education,
    output = "flextable"
  )

  tab |>
    flextable::theme_booktabs() |>
    flextable::autofit() |>
    flextable::fontsize(size = 10, part = "all")
}
```

## Keep the detailed options in the function-specific articles

The dedicated articles go deeper into each function:

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  covers missing values, level filtering, association measures, and
  one-way frequency-style tables.
- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  covers grouped descriptive statistics, parametric and nonparametric
  tests, and effect sizes.
- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  covers estimated marginal means or slopes from linear models, robust /
  cluster-robust / bootstrap / jackknife variance, case weights,
  additive covariate adjustment (G-computation or equal-weight), and
  four effect-size families with noncentral CIs.
- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  covers single- and multi-model coefficient tables for `lm` / `glm`,
  four standardisation methods, partial effect sizes with noncentral-F
  CIs, average marginal effects, hierarchical (`nested = TRUE`)
  comparisons, multiplicity correction, and response-scale reporting for
  GLMs.

Use this vignette as the final reporting overview, then consult the
function-specific articles when you need the detailed controls.
