# Label attributes with a few renames

Builds a value for the `labels` argument of the descriptive tables
([`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
[`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md),
[`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md)
and
[`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md))
and of the regression tables
([`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
and
[`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)).
The variables it names take the labels given here, and every other
variable shows its `label` attribute (as set by `haven`, `labelled` or
[`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)),
or its name when it has none, whatever `options(spicy.var_labels)` says.
`var_labels()` with no argument is the same as `labels = TRUE`.

A plain named vector, `labels = c(bmi = "Body mass index")`, differs in
one point: the variables it leaves out follow
`options(spicy.var_labels)`, which shows their names unless it is
`TRUE`.

In a regression table, the names are the model's: the outcome, the term
labels (`age`, or `sex` for the header of a factor), and the coefficient
names (`sexMale`, `(Intercept)`). The attribute names the outcome, the
numeric terms, and the factor headers. Factor levels, interactions, and
transformed terms keep their names.

## Usage

``` r
var_labels(...)

# S3 method for class 'spicy_var_labels'
print(x, ...)
```

## Arguments

- ...:

  `name = "label"` pairs, one per variable to rename, each label a
  single string. A named character vector can be spliced with `!!!`.
  Names that match nothing in a table are ignored. For
  [`print()`](https://rdrr.io/r/base/print.html), passed on to
  [`print()`](https://rdrr.io/r/base/print.html).

- x:

  A `spicy_var_labels` object, for
  [`print()`](https://rdrr.io/r/base/print.html).

## Value

A named character vector of class `spicy_var_labels`, which prints as
the plain named vector. [`print()`](https://rdrr.io/r/base/print.html)
returns `x` invisibly.

## See also

The `labels` argument of
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
and its siblings, and of
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
which lists the four forms it takes.

## Examples

``` r
# The label attribute of every column, one rename.
table_continuous(
  sochealth,
  select = c(bmi, age),
  labels = var_labels(bmi = "BMI (kg/m2)")
)
#> Descriptive statistics
#> 
#>  Variable    │   M     SD     Min    Max   95% CI LL  95% CI UL   n   
#> ─────────────┼────────────────────────────────────────────────────────
#>  BMI (kg/m2) │ 25.93   3.72  16.00  38.90    25.72      26.14    1188 
#>  Age (years) │ 49.26  14.70  25.00  75.00    48.43      50.10    1200 
#> 
#> Note. Missing values removed: BMI (kg/m2) (12).

# A vector of renames kept for a whole project, spliced.
renames <- c(bmi = "BMI (kg/m2)", smoking = "Smoker")
tbl <- table_categorical(
  sochealth,
  select = smoking,
  by = sex,
  labels = var_labels(!!!renames)
)

# A regression table: the outcome and the factor header keep their
# label attribute, the numeric term takes the one given here.
table_regression(
  lm(wellbeing_score ~ age + sex, data = sochealth),
  labels = var_labels(age = "Age in years")
)
#> Linear regression: WHO-5 wellbeing index (0-100)
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.07  1.63  [61.87, 68.26]  <.001 
#>  Age in years    │    0.04  0.03  [-0.02,  0.10]   .163 
#>  Sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    3.90  0.90  [ 2.14,  5.65]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1200                                 
#>  R²              │    0.02                              
#>  Adj. R²         │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
```
