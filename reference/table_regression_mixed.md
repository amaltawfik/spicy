# Mixed-effects models in table_regression()

Reporting behaviour for
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) /
[`glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html),
[`glmmTMB::glmmTMB()`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html),
and [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) / `gls()`
fits.

## Fixed effects

- `lmer` with lmerTest loaded: Satterthwaite t-tests (recommended below
  ~30 groups); plain `lmer`: Wald z with a footer suggestion to load
  lmerTest.

- `glmer`, `glmmTMB`: Wald z (asymptotic).

- `lme`: t-tests with nlme's containment degrees of freedom.

## Random effects

Variance components render as a subordinate `Random effects` block of
rows: one row per standard deviation, correlation, and the residual,
each with its estimate, SE, and CI (`merDeriv` for lme4; native
[`confint()`](https://rdrr.io/r/stats/confint.html) / `intervals()` for
glmmTMB / nlme; identical random structures from different engines align
on the same rows). `re_scale` switches between the SD (default) and
variance scales; `re_columns` controls which cells display.

There is deliberately **no per-row p-value** on variance components: the
null sigma = 0 sits on the boundary of the parameter space, where a Wald
test is invalid (Self & Liang 1987), and no reporting guideline requests
one. The significance signal for the random part is the footer's
likelihood-ratio test against the no-random-effects model, with the
boundary-corrected chi-bar-squared p-value (Stram & Lee 1994) – the same
line Stata's `mixed` prints. `re_test = "lrt"` / `"rlrt"` adds an opt-in
per-term test (LR vs the reduced random structure with the
chi-bar-squared reference, or `RLRsim`'s exact restricted LRT) – never a
Wald z.

## Fit statistics

Defaults: `nobs`, `n_groups`, `icc`, `r2_marginal`, `r2_conditional`,
`AIC`, `BIC`. The Nakagawa marginal / conditional R-squared are
cross-validated against
[`performance::r2_nakagawa()`](https://easystats.github.io/performance/reference/r2_nakagawa.html);
the ICC row is dropped automatically when the ICC is not defined (random
slopes, crossed factors, families without a link-scale variance).

## Robust variance

`CR*` cluster-robust via clubSandwich (with Satterthwaite df for
`lmer`). For `glmmTMB` the CR backend covers the conditional fixed
effects only; component rows stay model-based, disclosed in the footer.
`HC*` is refused (a single-level concept).

## Zero-inflation and dispersion (glmmTMB)

See
[table_regression_counts](https://amaltawfik.github.io/spicy/reference/table_regression_counts.md).

## See also

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
[table_regression_models](https://amaltawfik.github.io/spicy/reference/table_regression_models.md);
the *Mixed-effects models* section of
[`vignette("table-regression")`](https://amaltawfik.github.io/spicy/articles/table-regression.md).
