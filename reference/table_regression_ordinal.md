# Ordinal models in table_regression()

Reporting behaviour for
[`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html) and
[`ordinal::clm()`](https://rdrr.io/pkg/ordinal/man/clm.html) fits
(cumulative link models), including partial-proportional-odds `clm` fits
(`nominal = ~`).

## Coefficients and thresholds

Predictor coefficients report Wald z inference. The estimated category
cut-points render as a subordinate `Thresholds` block of rows (they are
intercepts, with SE and p), always on the log-odds scale – never
exponentiated. Opt out with `show_thresholds = FALSE` (compact footer
note instead). Partial-proportional-odds terms render as a
`Non-proportional effects` block, one coefficient per cut-point.

## Exponentiate and CIs

`exponentiate = TRUE` yields proportional odds ratios under the logit
link (`exp(B)` labels under other links – never a mislabelled "OR").
`ci_method = "profile"` gives profile-likelihood CIs for the predictor
coefficients via `confint.polr()` / `confint.clm()` (thresholds stay
Wald; a robust `vcov` takes precedence); the footer discloses
`95% CIs: profile likelihood.` when used.

## Average marginal effects

Per-category AME: one column per response category (the marginal effect
on P(Y = k)), the field-standard matrix layout.

## Robust variance and fit statistics

`CR*` via
[`sandwich::vcovCL`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html)
for plain proportional-odds fits; refused (no estimating functions) for
`scale = ~` / `nominal = ~` fits. Defaults: `nobs`, McFadden and
Nagelkerke pseudo-R-squared (matching Stata `ologit` and SPSS PLUM;
closed-form null likelihood), `AIC`.

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
[table_regression_models](https://amaltawfik.github.io/spicy/reference/table_regression_models.md);
[`vignette("table-regression-ordinal")`](https://amaltawfik.github.io/spicy/articles/table-regression-ordinal.md)
for the full walk-through.
