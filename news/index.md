# Changelog

## spicy (development version)

### Breaking changes

- `table_regression(exponentiate = TRUE)` now errors
  (`spicy_invalid_input`) on links whose exponentiated coefficient has
  no ratio interpretation: probit, cauchit, inverse (the
  [`Gamma()`](https://rdrr.io/r/stats/family.html) default), `1/mu^2`,
  sqrt, ordinal `loglog`, and cloglog outside the binomial / ordinal
  families. Previously these silently printed a meaningless `exp(B)`
  column. The ratio links (logit, log, binomial / ordinal cloglog) are
  unchanged; identity keeps its warn-and-skip; the error names the
  offending model and points at the AME column for response-scale
  effects.
- `align = "auto"` removed from all `table_*` functions; use `"decimal"`
  (default), `"center"`, or `"right"`.
- `table_regression(show_fit_stats = character(0))` now errors; use
  `FALSE` to suppress the block.
- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  SE footer `"classical (MLE inverse Hessian)"` renamed to
  `"classical (Fisher information)"`.
- `table_regression(list(...), show_columns = "all_b" | "all_ame")`
  auto-compacts (drops CIs) in multi-model layouts; use atomic tokens to
  keep them.
- [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) on a
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  result labels AME rows `estimate_type = "ame"` (was `"AME"`).
- `table_categorical(drop_na = )` now defaults to `FALSE`: missing
  values display as a dedicated `"(Missing)"` level (and a `"(Missing)"`
  group column under `by`) instead of being silently removed – the
  cross-package field convention (gtsummary’s “Unknown” row, janitor’s
  `NA` row; Epidemiologist R Handbook, Descriptive tables). Opting back
  into `drop_na = TRUE` now DISCLOSES the removal in a table note
  (“Missing values removed: income_group (18).”; plus the `by`
  variable’s removed rows), so a reader can always see what left the
  table. Pass `drop_na = TRUE` for the previous display; the silent
  variant is gone.
- `standardized = "smart"` now applies Gelman (2008) as the paper states
  it: **continuous** numeric inputs are scaled by `2 * SD(X)` (a +/-1 SD
  swing spans the same range as a binary’s 0 to 1 step) and **binary**
  inputs – numeric 0/1 and factor dummies – are left unscaled. Since
  0.12.0 the rule was applied inverted (binaries `2 * SD`, continuous
  `1 * SD`) while citing the same paper, so every continuous “smart”
  beta was half the documented value; caught by the PSPP
  cross-validation pass. `"refit"` / `"posthoc"` / `"basic"` /
  `"pseudo"` are unchanged.

### New supported models

[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
gains first-class support for ~30 model classes beyond `lm` / `glm`:

- Mixed effects: [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html)
  / `glmer`,
  [`glmmTMB::glmmTMB`](https://rdrr.io/pkg/glmmTMB/man/glmmTMB.html),
  [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) – random effects
  as a block of rows (σ, ρ, residual with SE + CI;
  `estimate_type = "vc"` in `tidy()`, no p-value: boundary, Self & Liang
  1987), ICC + per-group N + Nakagawa R² as fit stats, and a
  chi-bar-squared LR test vs the no-random model. The LR test follows
  the FIT’S OWN estimator (a REML fit is compared on the REML likelihood
  to the fixed-effects-only model, matching
  [`lmerTest::ranova()`](https://rdrr.io/pkg/lmerTest/man/ranova.html),
  Stata `mixed`, and RLRsim; ML fits compare on ML), so the footer’s
  `(REML)`/`(ML)` tag describes the statistic, not just the model. The
  null refit inherits the full fit’s specification: prior weights
  (weighted `lmer` fits reproduce
  [`ranova()`](https://rdrr.io/pkg/lmerTest/man/ranova.html) exactly;
  the REML null uses `gls` + `varFixed(~1/w)`), `nlme`
  variance/correlation structures (an `lme` with `varPower`/`corAR1`
  compares against a `gls` null carrying the same structure, matching
  `anova(gls, lme)`), and for `glmmTMB` the null is engine-native (same
  TMB likelihood, same frequency-weight convention, zi/dispersion kept)
  – which also gives `nbinom` and zero-inflated fits a valid LR test.
- Bayesian: `rstanarm`, `brms` – posterior median / SD / equal-tailed
  `95% CrI`, no p-value.
- Survey:
  [`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html).
- Survival:
  [`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html) /
  `survreg`, [`rms::cph`](https://rdrr.io/pkg/rms/man/cph.html),
  [`flexsurv::flexsurvreg`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md).
  Cox tables report `n` and `N events` as fit-stat rows (the field
  convention – EpiRHandbook survival chapter, Stata `stcox`;
  `show_fit_stats` token `"n_events"`), with the concordance kept as a
  footer note.
- Categorical:
  [`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html),
  [`mlogit::mlogit`](https://rdrr.io/pkg/mlogit/man/mlogit.html),
  [`MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html),
  [`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html). A single
  `multinom` model renders the publication layout – predictors as rows,
  one column group per non-reference outcome category (`outcome_labels`
  relabels the spanners, e.g. `"Student vs Employed"`), compact B / SE /
  p by default, and a `Reference outcome: <level>.` footer note;
  multi-model and `nested = TRUE` tables keep one row per (category,
  predictor), and `tidy()` / `output = "long"` always return the long
  form. `mlogit` tables use the two-segment Stata `asclogit`
  presentation: an `Alternative-invariant` section for the attribute
  coefficients, then one section per non-reference alternative
  (intercept + case-specific covariates, bare row labels) and a
  `Reference alternative: <base>.` footer note; terms keep mlogit’s
  native `<term>:<alt>` names in `tidy()` / `keep` / `drop`. `mlogit`
  robust SEs are cluster-robust only (one cluster per choice situation;
  `HC*` is refused –
  [`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)
  mis-scales the long-format sandwich), and its `n` counts choice
  situations (Stata `asclogit`’s “Number of cases”).
- Robust / IV / panel:
  [`estimatr::lm_robust`](https://declaredesign.org/r/estimatr/reference/lm_robust.html)
  / `iv_robust`, [`AER::ivreg`](https://rdrr.io/pkg/AER/man/ivreg.html),
  [`fixest::feols`](https://lrberge.github.io/fixest/reference/feols.html)
  / `feglm` / `fepois`.
- Beta / Tobit / count: `betareg`,
  [`AER::tobit`](https://rdrr.io/pkg/AER/man/tobit.html),
  [`pscl::hurdle`](https://rdrr.io/pkg/pscl/man/hurdle.html) /
  `zeroinfl`.
- Other: [`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html) /
  `glm.nb`, [`quantreg::rq`](https://rdrr.io/pkg/quantreg/man/rq.html),
  [`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html) / `bam`,
  [`stats::nls`](https://rdrr.io/r/stats/nls.html),
  [`rms::ols`](https://rdrr.io/pkg/rms/man/ols.html) / `lrm` / `Glm`,
  [`sampleSelection::selection`](https://rdrr.io/pkg/sampleSelection/man/selection.html).

Arguments whose method is not defined for a class are refused with a
classed error rather than silently ignored or rendered empty: robust
`vcov` requests raise `spicy_unsupported_vcov` where the estimator does
not exist for the class, and `standardized` raises
`spicy_unsupported_standardized` outside the classes with a real
standardized-coefficients path (`lm`, `glm`
incl. [`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html), and
the mixed engines) – the error points at AMEs for cross-predictor
comparison elsewhere.

See
[`vignette("table-regression")`](https://amaltawfik.github.io/spicy/articles/table-regression.md)
for the walk-throughs.

### New features

- New
  [`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md):
  the univariate screening table of applied epidemiology (the
  `gtsummary::tbl_uvregression()` + `tbl_merge()` layout; Epidemiologist
  R Handbook, Univariate and multivariable regression). One `glm` / `lm`
  fit per candidate predictor rendered as one row block each, merged
  side by side with the full multivariable model under “Univariate” /
  “Multivariable” column groups. A per-predictor `N` column is shown by
  default and, whenever the univariate complete-case samples differ, a
  table note discloses the range (“N varies by predictor (1163-1175)”);
  `complete_cases = TRUE` forces every fit onto the common sample (also
  disclosed). Intercepts are hidden by default (gtsummary convention),
  `p_adjust` treats the whole screen as one family, and `exponentiate`,
  `vcov` / `cluster` (one value per row of `data`, aligned to each fit
  automatically), `labels`, engines, and `tidy()` all pass through the
  ordinary
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  machinery. Estimates and p-values cross-validated against
  `gtsummary::tbl_uvregression()` (agreement to 12 decimals; CIs are
  Wald where gtsummary profiles the likelihood) and robust SEs against
  `sandwich` / `clubSandwich` per fit.
- `table_regression(re_ci = "profile")`: the random-effect
  variance-component rows of `lmer` / `glmer` fits can now carry
  **profile-likelihood CIs** (`confint(fit, method = "profile")`, lme4’s
  own supported route) instead of the merDeriv Wald SE + CI. The
  intervals are asymmetric, respect the boundary at 0, transform exactly
  to the variance scale under `re_scale = "variance"`, and sidestep the
  `spicy.re_se_max_n` size cap; no SE is shown (the footer discloses the
  method). `glmmTMB` /
  [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) keep their
  engine-native CIs and refuse the option with a clear error.
- New vignette *Count and two-part regression tables*: the
  Poisson-to-negative-binomial-to-two-part escalation on real data, the
  zero-inflation vs hurdle reading (and how to choose), per-block
  exponentiation, combined AME, cluster-robust variance across both
  components, and zero-inflated mixed models.
- New vignette *Mixed-effects regression tables*: the textbook
  model-building sequence on the High School & Beyond data (OLS to empty
  model / ICC, contextual within-between split, random slope,
  cross-level interaction), the random-effects block row by row, why
  variance components carry no p-value, the boundary-corrected tests
  (`re_test`), model comparison, three-level and crossed designs,
  `glmer` / `glmmTMB` / `nlme`, cluster-robust variance, and
  standardized coefficients.
- New vignette *Multinomial regression tables*: baseline-category logits
  per outcome, odds ratios and the Stata RRR terminology note, changing
  the reference category, the coefficient-sign vs probability-effect
  trap with per-category AME, the ordinal-predictor scores-vs-dummies
  LRT workflow, IIA in brief, and the `mlogit` discrete-choice engine.
- `vcov` computes heteroskedasticity- and cluster-robust SEs for the
  supported classes, each via its field-standard backend
  (`clubSandwich`, Lin-Wei dfbeta,
  [`sandwich::vcovCL`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html),
  [`rms::robcov`](https://rdrr.io/pkg/rms/man/robcov.html)).
- `"ame"` columns populate for `betareg`,
  [`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html), `svyglm`,
  `survreg`, and per-outcome-category for `polr` / `clm` / `multinom`;
  the ordinal per-category AME renders as a probability matrix.
- AME standard errors, CIs, and p-values honour a robust `vcov`
  (`glmmTMB` falls back to model-based with a warning).
- Ordinal (`polr` / `clm`) thresholds render as a labelled `Thresholds`
  block of rows below the predictors, on the log-odds scale (never
  exponentiated); opt out with `show_thresholds = FALSE`.
- Ordinal fits get a class-aware fit-stats default (`nobs`, McFadden +
  Nagelkerke pseudo-R², `AIC`); any other class falls back to `nobs` +
  `AIC` (was a blank block).
- `ci_method = "profile"` gives profile-likelihood CIs for `glm`,
  `polr`, and `clm`; the footer discloses `95% CIs: profile likelihood.`
  when used.
- New `ci_method = "boot_percentile"` (with `vcov = "bootstrap"`): the
  coefficient CI bounds become equal-tailed percentile intervals of the
  bootstrap replicates (the `boot::boot.ci(type = "perc")` convention,
  cross-validated to machine precision), reusing the same resamples as
  the bootstrap SEs. Estimate, SE, statistic and p stay Wald from the
  bootstrap covariance – the Stata layering (normal-based table CIs by
  default, percentile on request). Footer:
  `95% CIs: bootstrap percentile.`
- Resampling footers now name the estimator actually applied:
  `Std. errors: nonparametric bootstrap (N replicates).` (N = VALID
  replicates, as in Stata’s completed-replications header; cluster
  variant named too) and `jackknife (leave-one-out)` /
  `(leave-one-cluster-out)`. A bootstrap / jackknife whose replicates
  nearly all fail now raises `spicy_resampling_failed` instead of
  silently reporting classical SEs under a “bootstrap” footer (also
  applies to
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  which shares the resamplers).
- [`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html)
  partial-proportional-odds fits (`nominal = ~`) are now supported
  (previously refused): the non-proportional terms render as a
  `Non-proportional effects` block, one coefficient per cut-point.
  Robust SEs are not available for them; profile CIs cover the
  proportional terms.
- `re_test = "lrt"` / `"rlrt"` adds an opt-in per-term significance test
  for the random-effect variance components (never Wald): a
  likelihood-ratio test vs the reduced random structure with the
  chi-bar-squared boundary correction, or `RLRsim`’s exact restricted
  LRT for a single variance component. The p fills the `Random effects`
  rows; a footer line names the test. Supports `lmer` / `glmer` /
  `glmmTMB` and simple
  [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) structures.
- The `N (groups)` fit-stat row upgrades to `N (Subject)` with plain
  counts when every model shares a single grouping factor (crossed
  structures keep the generic label with descriptive counts).
- Two-part count models now show their FULL model: the zero component of
  [`pscl::zeroinfl`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) /
  `hurdle` and the `ziformula` / `dispformula` components of `glmmTMB`
  render as labelled subordinate row blocks (`Zero-inflation`,
  `Zero hurdle`, `Dispersion`) with full Wald inference – previously
  only the count/conditional coefficients rendered. The two zero blocks
  are labelled by what they model (structural zero vs nonzero count); a
  footer line names each block’s meaning and scale. Opt out with
  `show_components = FALSE`. Under `exponentiate = TRUE` a component is
  exponentiated only when its link yields an odds ratio (logit); probit
  / count-type zero parts and the dispersion model stay on the link
  scale.
- Cluster-robust `vcov` (`CR*`, via
  [`sandwich::vcovCL`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html))
  now supported for
  [`pscl::zeroinfl`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) /
  `hurdle`, covering both components with one estimator.
- The AME column now populates for `pscl` fits (combined-response
  `avg_slopes()`; it was silently empty).
- New
  [`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md):
  the machine-readable registry of the supported model classes (family,
  engine, AME estimand, exponentiate semantics, labelled blocks). Its
  documentation page is the per-family behaviour reference (also
  reachable as
  [`?table_regression_mixed`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md),
  [`?table_regression_ordinal`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md),
  …); a test guards the registry against drifting from the actual
  dispatch methods.

### Minor improvements

- Console rendering polish for wide tables split into stacked panels:
  continuation panels no longer end with empty `n` / `AIC` stub rows
  (model-level statistics print once, under the panel that carries their
  values), and an over-wide column-group spanner now truncates with a
  visible ellipsis instead of silently reading as a complete – wrong –
  label (“Unemployed vs Emp…”, not “Unemployed vs Empl”).
- Cross-software validation extended from the contingency-table measures
  to the regression paths: OLS (B / SE / t / R² / adjusted R² / SPSS
  Beta) and binary logistic (B / SE / Wald / Exp(B) / CI / -2LL /
  Nagelkerke) are now pinned to PSPP 2.0 (SPSS clone) oracle values on
  the bundled `sochealth` data – the first reference for these
  quantities from outside the R ecosystem. The pinning also documents
  the Beta convention split: SPSS standardises factor dummies
  (`standardized = "basic"` reproduces it exactly), while the default
  `"refit"` keeps dummies at 0/1.
- The weak-assertion audit’s top-20 test files now pin their numeric
  outputs to runtime oracles
  ([`anova()`](https://rdrr.io/r/stats/anova.html), `merDeriv`,
  [`boot::boot.ci`](https://rdrr.io/pkg/boot/man/boot.ci.html),
  [`performance::r2_nakagawa`](https://easystats.github.io/performance/reference/r2_nakagawa.html),
  [`marginaleffects::avg_slopes`](https://rdrr.io/pkg/marginaleffects/man/slopes.html),
  [`summary()`](https://rdrr.io/r/base/summary.html) /
  [`confint()`](https://rdrr.io/r/stats/confint.html), hand-derived
  closed forms) instead of finiteness-only checks; no disagreement
  surfaced anywhere a value was pinned.
- Test coverage raised to 100% of reachable lines: every previously
  uncovered line now carries either a behavioral test (10 new
  coverage-closure test files, oracle-pinned where numeric) or a
  `# nocov` annotation with a written justification (9 defensive guards
  unreachable through the public API). Along the way,
  `.aliased_coef_terms()` now tells the frame and legacy reference
  columns apart with `[[` – `$` partial matching silently resolved
  `is_ref` to a legacy `is_reference` column, leaving the documented
  legacy branch unreachable (same results by accident; now exact).
- Under `exponentiate = TRUE` with a visible SE column, the footer now
  states the SE scale (“SE on the OR scale (delta method)”; the Stata
  `[R] logistic` convention `se(OR) = OR x se(b)`) and that the CI is
  asymmetric (the exponential of the link-scale bounds). The
  documentation explains why such a CI cannot be reconstructed as
  `estimate +/- z x SE`. In tables mixing exponentiated and
  identity-link models the sentence is scoped to the exponentiated
  models.
- `show_fit_stats = FALSE` suppresses the fit-stats block.
- En-dash placeholder cells decimal-align in `gt` / `flextable` /
  `tinytable` / Word / Excel outputs.
- `"deviance"` prints at 1 decimal (matching `AIC` / `BIC`).
- `table_continuous*()` /
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  use a single font in `flextable` / Word outputs.

### Bug fixes

- `gt` and `flextable` outputs now render in Quarto / R Markdown
  **Word** (and PowerPoint / PDF) documents. `knit_print` always emitted
  raw HTML, which pandoc drops in non-HTML targets – the table silently
  disappeared from docx reports (reported from a real Quarto -\> Word
  workflow). The methods are now format-aware: HTML targets keep the
  styled table note; every other target delegates to the engine’s native
  rendering, with the note carried natively (flextable footer /
  [`gt::tab_source_note()`](https://gt.rstudio.com/reference/tab_source_note.html)).
  A new `as_flextable()` method also returns the clean underlying
  flextable for manual composition, mirroring
  `gtsummary::as_flex_table()`.
- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  now discloses a non-classical SE estimator in its table note (“Std.
  errors: heteroskedasticity-robust (HC3).” / “cluster-robust (CR2),
  clusters by region.” / “nonparametric bootstrap (200 replicates).” –
  the bootstrap line reports the VALID replicate count per fit, as a
  shared value or a range, matching table_regression’s disclosure; the
  counts are also carried in the long output as `boot_n_valid`) – it was
  the one table that silently labelled robust and resampling standard
  errors – and its notes (the adjustment estimand and the SE disclosure)
  now carry into every rich output (`gt`, `flextable`, `tinytable`,
  Excel, Word), which previously dropped them entirely. `cluster` also
  accepts the one-sided-formula form (`cluster = ~region`), matching
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).
- [`AER::tobit`](https://rdrr.io/pkg/AER/man/tobit.html) tables title
  the actual response (“Tobit regression: affairs”), not the internal
  `survival::Surv(ifelse(...), ...)` construction the survreg delegate
  sees.
- Ordinal titles name the shared-slopes assumption by its link: logit
  keeps “(proportional odds)”, cloglog reads “(proportional hazards)”
  (the grouped proportional-hazards model; McCullagh 1980), and probit /
  loglog / cauchit read “(parallel slopes)” – a probit fit has no odds
  for the old suffix to be proportional about. `clm` `nominal = ~` fits
  keep the “partial” prefix on the same mapping.
- `ci_level` now reaches the random-effect variance-component CIs. All
  three mixed engines hardcoded 95% for those rows (`lmer` / `glmer` via
  merDeriv, `glmmTMB` via `confint`,
  [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) via
  `intervals()`), so a `ci_level = 0.90` table showed 95% bounds on its
  σ rows under a 90% header. The derived variance-scale SEs stay coupled
  to the same level.
- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  on a large `lmer` / `glmer` fit no longer spends minutes (or worse)
  computing the random-effect variance-component SEs: `merDeriv`’s cost
  grows superlinearly with n (about a minute at n ≈ 2,700). Above
  `options("spicy.re_se_max_n")` (default 1,000) the SE / CI cells of
  the `Random effects` rows are now omitted (em-dash), a table note
  states the omission, and a `spicy_caveat` warning gives the override
  (`options(spicy.re_se_max_n = Inf)`) and the `re_test` alternative.
  Estimates, N (groups), ICC, R², and the LR-test footer are unaffected.
- Multi-factor group counts no longer pluralize grouping-variable names
  (“30 cask:batchs, 10 batchs”): the `N (groups)` cell now reads “30
  (cask:batch), 10 (batch)”. The dominant single-factor case
  (`N (Subject) | 18`) is unchanged.
- `nested = TRUE` now works for
  [`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html) fits:
  the comparison rows report the likelihood-ratio chi-square (matching
  `anova.multinom()`), not `lm`’s R²/F-change. It previously crashed –
  `nnet` registers no [`nobs()`](https://rdrr.io/r/stats/nobs.html)
  method – before reaching any comparison.
- A cluster-robust `vcov` with a formula / string `cluster` naming a
  variable outside the model formula no longer crashes with
  `missing value where TRUE/FALSE needed` on classes without a
  [`nobs()`](https://rdrr.io/r/stats/nobs.html) method: `multinom` now
  gets the same clean `spicy_unsupported_vcov` refusal as `HC*`, and
  [`pscl::zeroinfl`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) /
  `hurdle` – which do support `CR*` – now compute it instead of
  crashing.
- Fix decimal-point alignment in `gt` / `tinytable` / `flextable` / Word
  outputs of
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  /
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  /
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
- The polynomial-trends footer note no longer fires when the ordered
  factor is filtered out by `keep` / `drop`.
- The mixed-inference footer no longer claims “Wald-z … Load `lmerTest`”
  over Satterthwaite rows:
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  passed its default `ci_method = "wald"` down to the frame, overriding
  the Satterthwaite regime that the coefficient rows actually carry on
  `lmerTest` fits. The footer now reports
  `p-values: Satterthwaite t-test (lmerTest).` Under a `CR*` vcov the
  same line attributes the Satterthwaite df to their actual source:
  `cluster-robust df (clubSandwich)`.
- AME columns are now genuinely wired for `fixest` (`feols`; `feglm`
  with fixed effects is refused by `marginaleffects` and em-dashes with
  a warning), `estimatr` (`lm_robust`, `iv_robust`),
  [`quantreg::rq`](https://rdrr.io/pkg/quantreg/man/rq.html),
  [`AER::ivreg`](https://rdrr.io/pkg/AER/man/ivreg.html), and `rms`
  (`ols`, `lrm`, `Glm`) – these classes declared AME support but
  rendered an entirely empty column (finding M2 residual; oracle-matched
  to
  [`marginaleffects::avg_slopes()`](https://rdrr.io/pkg/marginaleffects/man/slopes.html)).
  Classes with no AME backend (`flexsurvreg`, `sampleSelection`,
  Bayesian fits) now REFUSE the request with a pointer to
  [`?table_regression_models`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
  instead of the empty column, and the registry’s AME column was
  corrected accordingly.
- Standardised beta rows on mixed fits now inherit the B rows’ reference
  distribution: on an `lmerTest` fit, beta carried Wald z (`df = Inf`)
  next to B’s Satterthwaite t – the same statistic printed two different
  p-values in one table. Beta now reports the same t / df / p, with the
  CI rebuilt from the t critical value.
- `flexsurv` exponentiate hardening: `flexsurvspline(scale = "normal")`
  (probit-like location scale) is now refused by the link gate instead
  of silently exponentiating, and covariates on ancillary parameters
  (`anc =`) refuse `exponentiate = TRUE` (their identity-scale rows
  previously rendered meaningless `1.00 [1.00, 1.00]` “ratios”).
- `polr` / `clm` fits now detect non-uniform prior weights (the footer’s
  weights disclosure): neither class stores `$weights`, so the previous
  checks never fired.
- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md):
  the statistic column header follows the model’s actual reference
  distribution – `z` for z-asymptotic classes (`glm`, Cox, ordinal,
  `glmmTMB`, resampling vcov), `t` for t-referenced ones (`lm`,
  Satterthwaite mixed) – per model in multi-model tables. It was
  hardcoded to `t` (the `"t"` token in `show_columns` is unchanged).
- `table_regression(m1, m2)` (a forgotten
  [`list()`](https://rdrr.io/r/base/list.html)) now errors with “Wrap
  the models instead: `table_regression(list(m1, m2))`” instead of the
  baffling “`vcov` is a list of length 12”.
- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md):
  factor coefficient and AME rows follow
  [`levels()`](https://rdrr.io/r/base/levels.html) order (was
  alphabetical).
- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md):
  AME rows for ordered factors nest under the factor header with bare
  level labels.
- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md):
  `ame_ci` / `ame_p` / `ame_se` without the bare `ame` token now
  populate their columns.
- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md):
  ordered factors with AME columns show a reference row; in multi-model
  layouts a factor’s reference row is blank in models that lack it.
- `table_regression(stars = TRUE)`: stars anchor on B (and AME), not β.
- `table_regression(standardize = TRUE)`: y\* standardized coefficients
  return `NA` for log-link binomial models (undefined; was an
  unjustified value).
- `table_regression(list(...))`: no stray zero-width spaces in
  multi-model `tinytable` headers.
- The singular-fit table note now states only the fact (“variance
  component(s) estimated at the boundary (0); their Wald SE and CI are
  omitted”); the actionable advice (simplify the random structure, test
  with `re_test`) moved to a consolidated `spicy_caveat` warning at
  build time – advice addresses the analyst, not the reader of a
  published table.
- New vignette *Survival regression tables*: Cox hazard ratios with
  events and concordance, the proportional-hazards check, cluster-robust
  Lin-Wei variance, hierarchical Cox comparisons, AFT time ratios and
  the Weibull HR-TR duality,
  [`rms::cph`](https://rdrr.io/pkg/rms/man/cph.html), and `flexsurv`.
- Multi-model and hierarchical titles keep proper nouns capitalized
  (“Hierarchical Cox proportional hazards regression”, “Model 1: Poisson
  regression” – both previously dropped the capital of the surname).
- `flexsurvreg` factor predictors now group under their parent variable
  with a synthetic reference row, like every other engine (the fit has
  no [`terms()`](https://rdrr.io/r/stats/terms.html) method; the
  metadata now comes from its model frame – they previously rendered as
  bare contrast names such as `sexFemale`).
- Hierarchical (`nested = TRUE`) Cox comparisons now default to the LRT
  change rows (chi-squared + p) instead of lm’s Delta-R-squared and
  F-change rows, which have no definition for a partial likelihood and
  rendered as all-dash rows.
- The `p_adjust` footer’s family size m no longer counts component-block
  intercepts (`zero_(Intercept)`, …) that the adjustment itself excludes
  – the reported m now matches the adjustment performed.
- [`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
  and the rich engines (`gt`, `tinytable`, `flextable`, Excel,
  clipboard) now match the console body exactly: multi-model reference
  rows stay blank in models that lack the factor (was em-dash
  everywhere), and the multi-outcome `Outcome` row is carried through.
  The structured schema gains `reference_models_by_row` and
  `outcome_labels_by_col` (both documented in
  [`?as_structured`](https://amaltawfik.github.io/spicy/reference/as_structured.md)).
- `table_regression(output = "gt")` no longer errors when two model
  labels collide under
  [`make.names()`](https://rdrr.io/r/base/make.names.html)
  (e.g. `"Step 1"` and `"Step.1"`): spanner ids are assigned by
  position.
- `table_regression(p_adjust = ...)`: the method name is double-quoted
  on every platform.
- Standardised-coefficient disclosure fixed on three counts: the table
  note now states the algebraic interaction convention in method terms
  (scaled by the SD of the product design column; differs from `"refit"`
  when components are correlated) – the convention’s genealogy (SPSS,
  Stata `regress, beta`, `lm.beta`, `effectsize` `"basic"`) and the
  literature (Friedrich 1982; Cohen et al. 2003) are documented in
  [`?table_regression`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
  since publication table notes cite no literature and name no other
  software. The footer is fallback-aware (a failed `"refit"` that fell
  back to `posthoc` says so instead of printing the refit wording over
  `posthoc` numbers), and the source comments claiming `effectsize`
  `posthoc` equivalence on interaction rows – false – were corrected,
  with the true equivalences pinned by cross-package oracle tests. The
  same no-citations rule now holds across every table note
  (random-effect test lines, singular-fit note).
- [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md):
  `ci_method = "profile"` combined with a robust or resampling `vcov` no
  longer silently displays classical profile CIs next to robust SEs. The
  `vcov` takes precedence (its Wald CIs are reported, matching the
  documented `polr` / `clm` behaviour) and a consolidated
  `spicy_ignored_arg` warning discloses the override.
- Refits no longer leak the caller’s environment (silent wrong output on
  formulas with inline transforms such as `log(x)` when the raw variable
  was visible in the calling environment): `vcov = "bootstrap"` /
  `"jackknife"` now resample rows of the fixed evaluated design
  (`lm.wfit` / `glm.fit`) – which also makes them **work** on
  [`factor()`](https://rdrr.io/r/base/factor.html) /
  [`log()`](https://rdrr.io/r/base/Log.html) /
  [`poly()`](https://rdrr.io/r/stats/poly.html) formulas that previously
  failed on every replicate – and the `standardized = "refit"` paths
  (lm, glm, mixed) fall back with a `spicy_fallback` warning instead of
  silently refitting on raw unscaled data.
- `standardized = "refit"` on a
  [`poly()`](https://rdrr.io/r/stats/poly.html) /
  [`splines::ns()`](https://rdrr.io/r/splines/ns.html) formula no longer
  crashes (matrix-valued model-frame columns are skipped; the refit
  falls back to `posthoc` with a warning), and a failed mixed-effects
  refit now warns instead of silently omitting the requested beta rows.
- Binomial model titles are link-aware for `glmer` / `glmmTMB` /
  `svyglm` fits: a probit fit is no longer titled “Logistic …
  regression”.
- Mixed models with a transformed or matrix response (`cbind(...)`
  binomial `glmer` / `glmmTMB`) now get their LR test vs the
  no-random-effects model (the null refit silently failed before).
- `labels` accepts coefficient-level keys (e.g. `period2`) on mixed
  fits; the valid-key universe previously listed the grouping factors
  instead.

## spicy 0.12.0

CRAN release: 2026-05-19

### New features

- New
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md):
  publication-ready coefficient summary for one or more fitted `lm` or
  `glm` models, side by side. APA Manual 7 formatting is the default.
  Highlights:

  - Robust variance: classical, HC, cluster-robust (CR) with
    Satterthwaite df, bootstrap, jackknife. Per-model `vcov` accepted
    for SE-comparison tables.
  - Standardisation: `refit`, `posthoc`, `basic`, `smart`, `pseudo` (the
    last `glm` only).
  - Average marginal effects (AME) as separate columns; AME inference
    shares the coefficient’s variance estimator so B and AME are
    reported on the same inferential footing.
  - Partial effect sizes: f², η², ω² for `lm` (noncentral-F CIs);
    partial χ² for `glm`.
  - GLM response-scale reporting via `exponentiate = TRUE`, with
    family-appropriate labels (OR, IRR, HR, RR, MR, exp(B)) and optional
    profile-likelihood CIs (`ci_method = "profile"`).
  - Multiplicity correction via `p_adjust` (any
    [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html)
    method).
  - Hierarchical comparison via `nested = TRUE` (ΔR² / F-change for
    `lm`; LRT for `glm`).
  - Display controls: variable filtering, intercept and factor
    placement, reference-row styles, multi-model labels, stars, decimal
    mark, per-column digits.
  - Outputs: console, `data.frame`, long tibble, `gt`, `flextable`,
    `tinytable`, Excel, Word, clipboard.
    [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
    and
    [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
    methods supported.

  See
  [`?table_regression`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  and
  [`vignette("table-regression")`](https://amaltawfik.github.io/spicy/articles/table-regression.md).

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  gains additive covariate adjustment via the new `covariates` argument.
  Two estimands for the per-group adjusted means: `"proportional"`
  (G-computation, default) and `"balanced"` (equal-weight synthetic
  grid). Under adjustment, `f²` and `ω²` become partial effect sizes;
  `d` and `g` raise an explanatory error. The auto-built footer
  documents the covariates and the estimand. See
  [`vignette("table-continuous-lm")`](https://amaltawfik.github.io/spicy/articles/table-continuous-lm.md).

- New exported
  [`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
  accessor returns a typed view of a
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  result for programmatic use: raw numerics, CI split into `LL` / `UL`
  columns, and a column-level format specification.

### Breaking changes

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  no longer silently truncates the export filename to 120 characters.
  Very long titles now surface a clear OS-level error. **Migration**:
  shorten the title or pass an explicit `filename =` argument.

### Bug fixes

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  no longer over-truncates a *p*-value in the interval
  `(10^-p_digits, 0.001)` when `p_digits >= 4`. Example: `p = 0.000108`
  now correctly prints as `".0001"` at `p_digits = 4` (was `"<.0001"`).
- `count_n(special = ...)` returns a length-`nrow(data)` zero vector
  when no usable column survives the list-column filter, matching the
  documented contract and the `count = ...` branch (was `numeric(0)`,
  which broke
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
  pipelines).
- [`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md)
  and
  [`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md)
  emit `spicy_undefined_stat` and return a fully-`NA` result on rank-1
  contingency tables (constant predicted variable), matching the
  existing pattern in
  [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
  [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
  [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md),
  and
  [`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md).
- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  no longer silently overwrites a user’s y-variable level named `"N"`,
  `"Total"` or `"Values"`. The conflicting reserved column is
  auto-renamed with a numbered suffix and a single
  `spicy_renamed_column` warning is emitted.
- [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  on a `spicy_continuous_lm_table` keeps `df.residual` numeric, so
  Satterthwaite degrees of freedom from `vcov = "CR2"` / `"CR3"` are
  preserved verbatim instead of being truncated through
  [`as.integer()`](https://rdrr.io/r/base/integer.html).

### Minor improvements

- Console em-dash alignment: non-numeric placeholders (em-dash, “NA”)
  sit at the decimal-mark column instead of the integer- part column
  (APA Manual 7 §7.13). Integer cells in mixed- precision columns (`n`
  row alongside `R²`) keep their right- aligned placement.
- `R/` source is byte-pure ASCII
  ([`tools::showNonASCIIfile()`](https://rdrr.io/r/tools/showNonASCII.html)
  reports zero hits package-wide).
- [`openxlsx2::wb_add_border()`](https://janmarvin.github.io/openxlsx2/reference/wb_add_border.html)
  calls now pass `NULL` on unused sides, preventing the default `"thin"`
  from being applied to all four sides of a cell when only one rule is
  intended.

## spicy 0.11.0

CRAN release: 2026-05-04

### New features

#### `table_continuous_lm()`

- Cluster-robust SEs via `cluster` and four `vcov` choices
  (`"CR0"`–`"CR3"`), dispatched to `clubSandwich` with Satterthwaite df
  (`clubSandwich` in `Suggests`).
- `vcov = "bootstrap"` (nonparametric or cluster) and
  `vcov = "jackknife"` (leave-one-out / leave-one-cluster-out) variance
  estimators in pure base R, controlled by `boot_n`.
- Three new `effect_size` choices alongside `"f2"`: Cohen’s `"d"`,
  Hedges’ `"g"` (two-group only), Hays’ `"omega2"`. New `effect_size_ci`
  adds noncentral *t* / *F* CIs rendered inline as `0.18 [0.07, 0.30]`.
- `HC*` estimators delegate to
  [`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html);
  rank-deficient fits return a clean rank-by-rank covariance.

#### Harmonisation across the table family

- Shared reporting vocabulary (`decimal_mark`, `p_digits`, `align`,
  named-`labels`) now spans
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) and
  the three `table_*()` helpers, including APA-style p-value notation
  (`<.001` / `.045`, no leading zero).
- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)’s
  `assoc_measure` accepts a per-variable spec. When measures differ
  across rows the column collapses to `"Effect size"` and an APA-style
  `Note.` line documents the per-variable measure; `phi` on a non-2x2
  errors.
- All three `table_*()` functions gain
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
  methods (`broom` in `Suggests`).

### Quality and robustness

- **Classed conditions.** Errors and warnings now carry stable classes
  (`spicy_error` / `spicy_warning` plus 11 leaf classes documented in
  [`?spicy`](https://amaltawfik.github.io/spicy/reference/spicy-package.md)),
  so downstream code can dispatch via
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) /
  [`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html)
  instead of matching message strings. `rlang (>= 1.1.0)` required.
- **Structured cli messages.** Multi-line errors and warnings (vcov
  fallbacks, bootstrap/jackknife failures, `padding` migration, `labels`
  length mismatch) render as cli bullets.
- **Locale-deterministic ordering.** Sorts in
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md),
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md),
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  and `table_*()` use `method = "radix"`. Output is byte-stable across
  locales and platforms, matching Stata / SPSS guarantees.
- **Edge-case hardening.** A new length-guarded sort helper makes
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  /
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  /
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  / [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
  survive zero-length or all-NA `Date` / `POSIXct` / `character` columns
  and factors with no observed levels.
- **Snapshot-locked rendering.** `tests/testthat/test-snapshots.R` pins
  the exact console output of every spicy print method, so any
  unintended formatting drift surfaces as a PR diff.
- **API stability contract.**
  [`?spicy`](https://amaltawfik.github.io/spicy/reference/spicy-package.md)
  documents which exports are stable, stabilising or internal. pkgdown
  reference groups exports via four `@family` tags.
- **Cross-software validation.** All 13 association measures agree with
  PSPP 2.0 (`CROSSTABS /STATISTICS=ALL`, 65 / 65 statistics on four
  datasets); Cohen’s *d* and Hedges’ *g* noncentral CIs are tested
  numerically against
  [`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html)
  /
  [`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html)
  (`tolerance = 1e-6`); point-estimate formulas and asymptotic standard
  errors follow `DescTools` (Signorell et al.).

### Improvements

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  warns when `correct = TRUE` is ignored on a non-2x2 sub-table, when
  `weights` contains `NA`, and notes statistics computed on a sub-table
  after empty rows / columns are pruned.
- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  validates `decimal_mark`, `p_digits` and `simulate_B` up front;
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
  validates `decimal_mark` and tightens `digits` to a non-negative
  integer.
- A user category literally named `"N"` or `"Total"` is no longer
  mis-rendered as the totals row in
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
- `table_continuous_lm(output = "long")` returns `n`, `df1`, `df2` as
  integer columns; `predictor_label` preserved on the degenerate-model
  fallback path.
- [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
  / [`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md) doc
  states the CI uses the Fisher z-transformation (point estimate and
  p-value identical to `DescTools` / SPSS).
- [`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md)
  doc states entropy uses `0 log 0 = 0` (matching SPSS, PSPP, Stata,
  Cover & Thomas).

### Bug fixes

- [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  raises actionable errors on duplicate or empty new column names; trims
  whitespace and preserves the input class.
- `table_continuous_lm(output = "data.frame")` names contrast CI columns
  from `ci_level` (was hardcoded to 95 %).
- The categorical-predictor global Wald *F* degrades to `NA` on a
  singular coefficient covariance submatrix.
- The degenerate-table branch of
  [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md),
  [`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md),
  [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
  [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md)
  and
  [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md)
  respects `detail`: scalar `NA_real_` by default, fully shaped
  `spicy_assoc_detail` when `detail = TRUE`.
- [`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md)
  returns a finite estimate (was `NaN`) when a marginal is zero.
- `somers_d(direction = "symmetric")` returns the harmonic mean of the
  two asymmetric values, matching SPSS / PSPP `CROSSTABS`.
- [`print.spicy_assoc_detail()`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_detail.md)
  /
  [`print.spicy_assoc_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_table.md)
  use APA-strict `<.001` / `.045` notation, matching the rest of the
  package.
- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  /
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  honour `factor_levels = "all"` for `haven_labelled` columns:
  declared-but-unobserved labels appear in the `Values` summary.
- [`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md)
  rejects `row.names.as.col` vectors of length ≠ 1 and empty strings;
  accumulates all messages from
  [`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md)
  instead of overwriting.
- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md) /
  [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)
  reject non-integer `min_valid >= 1` and `min_valid > ncol`; their
  `digits` requires a non-negative integer.

### Breaking changes

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  and
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  default to decimal-point alignment for numeric columns
  (`align = "decimal"`). Pass `align = "auto"` for the previous
  behaviour.
- [`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md)
  /
  [`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md):
  `padding` switches from a string enum to a non-negative integer.
  Default `2L` (was `+5L`); printed tables are roughly 40 % narrower.
  **Migration**: `"compact" -> 0L`, `"normal" -> 2L`, `"wide" -> 4L`.
- `table_categorical(assoc_measure = "auto")` on a 2x2 table picks `phi`
  instead of `cramer_v`. Numeric value unchanged (\|phi\| = V on 2x2);
  only the column label changes.
- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) drops
  observations with `NA` weights (with a warning) instead of recoding
  them to zero. Aligns with
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).
- `table_continuous_lm(output = "long")` returns `NA` in `es_type` /
  `es_value` when `effect_size = "none"` (was `"f2"`), and renames
  `sum_w` to `weighted_n`.

## spicy 0.10.0

CRAN release: 2026-04-27

### New features

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  now accepts tidyselect-style variable selectors through `...`,
  matching
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  and [`vl()`](https://amaltawfik.github.io/spicy/reference/varlist.md).

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  gains a `filename` argument for the base name of CSV, Excel, and PDF
  exports. When `NULL` (the default), the filename is derived from
  `title` and falls back to `"Codebook"` when needed. Filenames are
  sanitized to portable ASCII consistently across platforms.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now summarizes matrix and array columns by their dimensions, and
  counts valid, missing, and distinct observations by rows.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) gains
  a `factor_levels` argument that mirrors
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  and
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md).
  With `factor_levels = "all"`, declared-but-unobserved factor and
  labelled levels appear in the output with `n = 0`, matching SPSS
  `FREQUENCIES`; the default `"observed"` preserves the previous Stata
  `tab`-style behavior.

### Improvements

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now displays missing values as `<NA>` and `<NaN>` in the `Values`
  summary when `include_na = TRUE`, and quotes literal `"NA"`, `"NaN"`,
  and empty-string values so they cannot be confused with the missing
  markers.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now emits a column-named warning and marks the failing cell as
  `<error: ...>` when a column cannot be summarized, instead of silently
  writing `"Invalid or unsupported format"`. Remaining columns are
  unaffected.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  produces more precise Viewer titles for extraction, pipe, and literal
  `get("name")` expressions, while keeping ambiguous dynamic calls
  anonymous (`vl: <data>`).

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  now rejects partial-match names in `...` (e.g. `val = TRUE`,
  `tit = "x"`) that would otherwise be silently treated as tidyselect
  expressions, and surfaces
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  selection errors directly.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  resolves the `weights` argument via tidy-eval, so column references
  nested in compound expressions
  (e.g. `weights = if (use_w) col else NULL`) work as expected.
  Qualified expressions like `weights = df2$w` continue to take
  precedence over column lookup.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)
  validates `digits`, `sort`, `weights`, and the logical scalar
  arguments (`valid`, `cum`, `rescale`, `styled`) more strictly at the
  public boundary, with clearer error messages for non-finite values,
  `NA`, multi-element inputs, and non-numeric weight vectors.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  documents the interaction of `weights` containing `NA` with
  `rescale = TRUE` (Stata `pweight` semantics) and the dropping of
  unused factor / labelled levels (Stata `tab` semantics, with
  `code_book(factor_levels = "all")` as the schema-style alternative).

### Bug fixes

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now displays labelled values in the same prefixed-label order for
  compact and `values = TRUE` summaries; previously the compact summary
  used data order.

- `varlist(values = TRUE)` now deduplicates element types when
  summarizing list-columns. Previously `list(1L, 2L, "a")` produced
  `"List(3): character, integer, integer"`; now produces
  `"List(3): character, integer"`.

- `include_na = TRUE` now correctly appends `<NA>` markers for
  list-columns in both
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  modes; previously it had no effect on this column type.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now validates column names up front and gives clearer errors for
  missing, empty, `NA`, or duplicate names.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now errors clearly when tidyselect expressions try to rename columns;
  `...` is for selecting variables, not renaming.

- `freq(data, x, weights = NULL)` now correctly treats the explicit
  `NULL` as “no weighting” instead of emitting a misleading
  `"variable 'NULL' not found"` error. Parameterized patterns like
  `weights = if (use_w) wts else NULL` are now supported.

- [`print()`](https://rdrr.io/r/base/print.html) for `spicy_freq_table`
  no longer crashes when the `var_label` attribute is `NA_character_`,
  numeric, or multi-element; the `Label:` line is silently skipped for
  any value that is not a single non-empty string.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) no
  longer surfaces the name of the ignored `data` vector in the printed
  footer when both `data` and `x` are passed as vectors. The footer now
  consistently shows the analyzed vector’s name.

## spicy 0.9.0

CRAN release: 2026-04-20

### Breaking changes

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  now enables inferential output by default when `by` is supplied. With
  a grouping variable, the `p` column from `test` is shown automatically
  (previous default hid it). This aligns the two table helpers:
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  stays descriptive when `by` is absent, and reports the test *p*-value
  when `by` is supplied, matching
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)’s
  inferential default. To preserve the previous behavior, pass
  `p_value = FALSE` explicitly. `statistic` and `effect_size` remain
  `FALSE` by default and must still be enabled consciously.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now displays observed factor levels by default in `Values`, matching
  its role as a quick inspection of the current data. Use
  `factor_levels = "all"` to display unused factor levels as well, which
  was the previous default behavior and remains the default in
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md).

### Minor improvements

- [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  gains a `factor_levels` argument. It defaults to `"all"` so exported
  codebooks continue to document all declared factor levels, including
  unused levels; use `"observed"` to mirror
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  output.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  prints the `Freq.` column as integers regardless of `digits`, which
  continues to control percentage precision. This matches the convention
  of SPSS, Stata, and SAS `PROC FREQ` for weighted counts and keeps the
  two numeric concepts (discrete counts vs. continuous percentages)
  visually distinct.

- `freq(..., styled = FALSE)` now returns a genuinely plain `data.frame`
  with no `spicy_freq_table` rendering metadata clinging to it, so
  [`str()`](https://rdrr.io/r/utils/str.html),
  [`dput()`](https://rdrr.io/r/base/dput.html), and downstream
  programmatic use see only the tabulation columns. The metadata
  attributes (`digits`, `data_name`, `var_name`, `var_label`,
  `class_name`, `n_total`, `n_valid`, `weighted`, `rescaled`,
  `weight_var`) are now documented in `@return` and remain available on
  the invisibly returned `spicy_freq_table` object when `styled = TRUE`
  (the default).

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  documentation now clarifies why `p_value = TRUE` and `r2 = "r2"` are
  the defaults, and robust-variance fallback warnings are now more
  explicit when a model matrix is singular.

### Bug fixes

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  correctly resolves qualified weight expressions such as
  `weights = other$w` or `weights = other[["w"]]` even when the
  referenced column name also exists in `data`. Previously the bare-name
  fallback could silently pull the weight vector from the wrong data
  frame when column names collided.

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) with
  `sort` and missing values now keeps the `NA` row at the end of the
  tabulation so the printed `Cum. Percent` and `Cum. Valid Percent`
  columns stay monotonic and match the Valid → Missing → Total display
  layout. Sorting previously could push the `NA` row between valid rows
  and make cumulative percentages appear to jump.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now preserves literal `"NA"` and empty-string values in the `Values`
  summary instead of removing them as if they were missing values.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now distinguishes actual `NA` values from `NaN` in the `Values`
  summary when `include_na = TRUE`.

- `varlist(values = TRUE)` now preserves factor level order in the
  `Values` summary, matching the default compact factor display.

- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  now validates `values`, `tbl`, and `include_na` up front and gives a
  clear error when one of them is not `TRUE` or `FALSE`.

## spicy 0.8.0

CRAN release: 2026-04-10

### New features

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  adds APA-style bivariate linear-model tables for continuous outcomes.
  It acts as the model-based companion to
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  for reporting fitted mean comparisons or slopes in an `lm` framework,
  with one predictor per model, model-based means for categorical
  predictors, optional case weights, classical or HC0-HC5 variance
  estimators, multiple output formats (ASCII, tinytable, gt, flextable,
  Excel, clipboard, and Word), `output = "data.frame"` for the wide raw
  table, `output = "long"` for the analytic long table, and configurable
  display of tests, confidence intervals, fit statistics, and effect
  sizes.

### Minor improvements

- Installed package vignettes now avoid embedding heavy HTML table and
  codebook widgets during CRAN builds, reducing package size while
  preserving rich pkgdown article rendering.

- Website and vignette coverage now includes
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  using the bundled `sochealth` data throughout and adding a dedicated
  article for model-based continuous summary tables.

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  and
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  now support dedicated display precision for effect-size columns, and
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  also supports separate precision for `R²` columns, so model fit and
  effect sizes can be formatted independently from descriptive values
  and test statistics.

- [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  now keeps `n` as the unweighted analytic sample size in wide and
  rendered outputs, and can optionally add a separate `Weighted n`
  column reporting the sum of case weights.

## spicy 0.7.0

CRAN release: 2026-03-30

### New features

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  is a new helper for continuous summary tables. It computes descriptive
  statistics (mean, SD, min, max, confidence interval of the mean,
  and n) for numeric variables, with tidyselect column selection,
  optional grouping via `by`, and multiple output formats (ASCII,
  tinytable, gt, flextable, Excel, clipboard, and Word).

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  gains `effect_size` and `effect_size_ci` arguments. When `by` is used,
  `effect_size = TRUE` adds an “ES” column with the appropriate measure
  (Hedges’ g, eta-squared, rank-biserial `r_rb`, or epsilon-squared)
  chosen automatically based on the test method and number of groups,
  and `effect_size_ci = TRUE` appends the confidence interval in
  brackets.

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  gains a `test` argument (`"welch"`, `"student"`, or `"nonparametric"`)
  to choose the group-comparison method, along with independent
  `p_value` and `statistic` display toggles so users can request either
  or both outputs when `by` is used.

- ASCII console tables now split oversized outputs into stacked
  horizontal panels, repeating the left-most identifier columns so wide
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md),
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
  and
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  prints stay readable in narrow consoles.

### Breaking changes

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  replaces `table_apa()` as the public helper for categorical summary
  tables. It uses `select` and `by`, supports grouped cross-tabulation
  or one-way frequency-style tables when `by = NULL`, and consolidates
  output formats under a single `output` argument. Migrate existing
  `table_apa()` calls to
  [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
  use `output = "default"` for ASCII tables and `output = "data.frame"`
  for plain data frames, and replace former `output = "wide"` /
  `style = "report"` paths with the formatted output engines.

- Excel export now uses `openxlsx2` instead of `openxlsx` for a lighter
  dependency footprint (no Rcpp compilation required).

### Minor improvements

- Package citation metadata now uses the current package title and CRAN
  DOI, so `citation("spicy")` matches `DESCRIPTION` and points to the
  package DOI.

- [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
  and
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  now print shorter ASCII titles without appending the input data frame
  name, and no longer require `officer` for `output = "flextable"`
  alone; `officer` is now required only for Word export paths that
  actually write `.docx` files.

- [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
  now accepts tidyselect syntax in `exclude` in addition to character
  vectors, and no longer warns that `test` is ignored when it is still
  needed to compute effect sizes.

## spicy 0.6.0

CRAN release: 2026-03-23

### New features

- New family of association measure functions for contingency tables:
  [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md),
  [`contingency_coef()`](https://amaltawfik.github.io/spicy/reference/contingency_coef.md),
  [`gamma_gk()`](https://amaltawfik.github.io/spicy/reference/gamma_gk.md),
  [`goodman_kruskal_tau()`](https://amaltawfik.github.io/spicy/reference/goodman_kruskal_tau.md),
  [`kendall_tau_b()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_b.md),
  [`kendall_tau_c()`](https://amaltawfik.github.io/spicy/reference/kendall_tau_c.md),
  [`lambda_gk()`](https://amaltawfik.github.io/spicy/reference/lambda_gk.md),
  [`phi()`](https://amaltawfik.github.io/spicy/reference/phi.md),
  [`somers_d()`](https://amaltawfik.github.io/spicy/reference/somers_d.md),
  [`uncertainty_coef()`](https://amaltawfik.github.io/spicy/reference/uncertainty_coef.md),
  and
  [`yule_q()`](https://amaltawfik.github.io/spicy/reference/yule_q.md).
  Each returns a numeric scalar by default; pass `detail = TRUE` for a
  named vector with estimate, confidence interval, and p-value.

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  gains `assoc_measure` and `assoc_ci` arguments. When both variables
  are ordered factors, it automatically selects Kendall’s Tau-b instead
  of Cramer’s V. The note format changes from `Chi-2: 18.0 (df = 4)` to
  `Chi-2(4) = 18.0`. Numeric attributes (`chi2`, `df`, `p_value`,
  `assoc_measure`, `assoc_value`, `assoc_result`) are now attached to
  the output data frame.

- `table_apa()` now dynamically labels the association measure column
  based on the measure used, instead of always showing “Cramer’s V”. New
  `assoc_measure` and `assoc_ci` arguments are passed through to
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md).

- `table_apa()` gains `output = "gt"` to produce a `gt_tbl` object with
  APA-style formatting, column spanners, and alignment.

- `table_apa()` now correctly centers spanner labels over their column
  pairs in `tinytable` and `flextable` output.

- All association measure functions and
  [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
  gain a `digits` argument (default 3) that controls the number of
  decimal places when printed. The p-value always uses 3 decimal places
  or `< 0.001`.

- `detail = TRUE` results now print with formatted output (aligned
  columns, fixed decimal places) via a new
  [`print.spicy_assoc_detail()`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_detail.md)
  method.
  [`assoc_measures()`](https://amaltawfik.github.io/spicy/reference/assoc_measures.md)
  output uses a new
  [`print.spicy_assoc_table()`](https://amaltawfik.github.io/spicy/reference/print.spicy_assoc_table.md)
  method with the same formatting.

- New bundled dataset `sochealth`: a simulated social-health survey (n =
  1200, 24 variables) with variable labels, ordered factors, survey
  weights, and missing values. Includes four Likert-scaled life
  satisfaction items (`life_sat_health`, `life_sat_work`,
  `life_sat_relationships`, `life_sat_standard`) for demonstrating
  [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
  [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md),
  and
  [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md).

### Bug fixes

- [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
  now correctly counts `NA` values when `count = NA` and `strict = TRUE`
  are both used. List columns are now reported in verbose mode instead
  of causing silent errors.

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  rescale logic now operates on complete cases only, so the weighted
  total N matches the unweighted N when missing values are present
  (consistent with Stata behavior).

- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  uses true `NA` consistently (instead of the `"<NA>"` string) in both
  weighted and unweighted paths. `cum_valid_prop` is now correctly `NA`
  for missing rows. Invalid `digits` and `sort` values are rejected with
  clear error messages.

- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md)
  and [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)
  now validate `min_valid` and `digits` arguments, rejecting
  non-numeric, negative, or multi-element values.

- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
  [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md),
  and
  [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
  no longer trigger a tidyselect deprecation warning when `select`
  receives a character vector. Character vectors are now automatically
  wrapped with
  [`all_of()`](https://tidyselect.r-lib.org/reference/all_of.html).

- `table_apa()` now preserves the original factor level order in row
  variables instead of sorting alphabetically. When `drop_na = FALSE`,
  the `(Missing)` category is placed at the bottom of each variable’s
  levels. `percent_digits`, `p_digits`, and `v_digits` are now
  validated.

- `table_apa()` p-values no longer wrap across lines in `tinytable` HTML
  output.

### Breaking changes

- [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
  now accepts a `detail` argument. By default it returns a numeric
  scalar (as before). Pass `detail = TRUE` to get a 4-element named
  vector (`estimate`, `ci_lower`, `ci_upper`, `p_value`), or
  `detail = TRUE, conf_level = NULL` for a 2-element vector (`estimate`,
  `p_value`) without CI.

## spicy 0.5.0

CRAN release: 2026-03-14

### New features

- New `table_apa()` helper to build APA-ready cross-tab reports with
  multiple output formats (`wide`, `long`, `tinytable`, `flextable`,
  `excel`, `clipboard`, `word`).
- `table_apa()` exposes key
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  controls for weighting and inference (`weights`, `rescale`, `correct`,
  `simulate_p`, `simulate_B`) and now handles missing values explicitly
  when `drop_na = FALSE`.

### Bug fixes

- [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md)
  no longer crashes when `special = "NaN"` is used with non-numeric
  columns. Passing `count = NA` now errors with a message directing to
  `special = "NA"`.
- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  fixes a spurious rescale warning for explicit all-ones weights and
  aligns the Cramer’s V formula with
  [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md).
- `table_apa()` no longer leaks global options on error. The
  `simulate_p` default is aligned to `FALSE`.
- [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  title generation no longer crashes on unrecognizable expressions.

### Minor improvements

- [`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md)
  parameter `message` renamed to `show_message`.
- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  dispatches printing correctly via S3.
- Removed unused `collapse` and `stringi` from `Imports`.

## spicy 0.4.2

CRAN release: 2026-03-06

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  hardening: improved vector-mode detection (including labelled
  vectors), stricter weight validation, safer rescaling, and clearer
  early errors (e.g., explicit `y = NULL`).
- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  statistics are now computed on non-empty margins in grouped tables,
  avoiding spurious `NA` results; internal core path refactored to
  remove `dplyr`/`tibble` from computation while preserving user-facing
  behavior.
- [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) now
  errors clearly when `x` is missing for data.frame input and validates
  rescaling when weight sums are zero/non-finite.
- [`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md),
  [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md),
  and [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)
  regex mode is hardened (`regex = TRUE` now validates/defaults `select`
  safely).
- [`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md)
  and [`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)
  now return `NA` (with warning) when no numeric columns are selected.
- [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  now validates input type (`data.frame`/tibble required).
- [`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)
  now returns `NA` with warning for degenerate tables.
- Dependency optimization: `DT` and `clipr` moved to `Suggests`;
  optional runtime checks added in
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
  and
  [`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md).
- Tests expanded with regression coverage for all the above edge cases.

## spicy 0.4.1

CRAN release: 2025-12-21

- Fixed CRAN incoming check notes by removing non-standard top-level
  files.

## spicy 0.4.0

- Print methods have been fully redesigned to produce clean, aligned
  ASCII tables inspired by Stata’s layout. The new implementation
  improves formatting, adds optional color support, and provides more
  consistent handling of totals and column spacing.

- Output from
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) and
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  now benefits from the enhanced `print.spicy()` formatting, offering
  clearer, more readable summary tables.

- Documentation and internal tests were updated for clarity and
  consistency.

- [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  gains an explicit `correct` argument to control the use of Yates’
  continuity correction for Chi-squared tests in 2x2 tables. The default
  behavior remains unchanged.

- The documentation of
  [`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)
  was refined and harmonized, with a clearer high-level description,
  improved parameter wording, and expanded examples.

- Minor cosmetic improvements were made to
  [`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
  output: the title prefix now uses `vl:` instead of `VARLIST`, and the
  column name `Ndist_val` was renamed to `N_distinct` for improved
  readability and consistency.

- Minor cosmetic improvement: ASCII table output no longer includes a
  closing bottom rule by default.

## spicy 0.3.0

CRAN release: 2025-10-22

- New function
  [`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md),
  which generates a comprehensive variable codebook that can be viewed
  interactively and exported to multiple formats (copy, print, CSV,
  Excel, PDF).

## spicy 0.2.1

CRAN release: 2025-10-04

- [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  now correctly handles edge cases when the separator appears in the
  label or is missing.

## spicy 0.2.0

CRAN release: 2025-09-25

- New function
  [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md)
  to derive and assign variable labels from headers of the form
  `"name<sep>label"` (e.g. `"name. label"`). Especially useful for
  LimeSurvey CSV exports (*Export results* -\> *CSV* -\> *Headings:
  Question code & question text*), where the default separator is
  `". "`.

## spicy 0.1.0

CRAN release: 2025-05-05

### Initial release

- Introduces a collection of tools for variable inspection, descriptive
  summaries, and data exploration.
- Provides functions to:
  - Extract variable metadata and display compact summaries
    ([`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)).
  - Compute frequency tables
    ([`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md)),
    cross-tabulations
    ([`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md)),
    and Cramer’s V for categorical associations
    ([`cramer_v()`](https://amaltawfik.github.io/spicy/reference/cramer_v.md)).
  - Generate descriptive statistics such as means
    ([`mean_n()`](https://amaltawfik.github.io/spicy/reference/mean_n.md)),
    sums
    ([`sum_n()`](https://amaltawfik.github.io/spicy/reference/sum_n.md)),
    and counts
    ([`count_n()`](https://amaltawfik.github.io/spicy/reference/count_n.md))
    with automatic handling of missing data.
  - Copy data
    ([`copy_clipboard()`](https://amaltawfik.github.io/spicy/reference/copy_clipboard.md))
    directly to the clipboard for quick export.
