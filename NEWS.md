# spicy (development version)

## Breaking changes

* `table_regression(exponentiate = TRUE)` now errors on links whose
  exponentiated coefficient is not a ratio (probit, cauchit, inverse,
  sqrt, ...). Ratio links (logit, log, binomial / ordinal cloglog) are
  unchanged; identity links keep the warn-and-skip.
* `table_categorical()` defaults to `drop_na = FALSE`: missing values
  display as a `"(Missing)"` level instead of being silently removed.
  With `drop_na = TRUE`, a table note now reports what was removed.
* `standardized = "smart"` scales continuous inputs by 2 SD and leaves
  binary inputs (0/1 and factor dummies) unscaled, as Gelman (2008)
  defines it. The rule was applied inverted since 0.12.0, halving every
  continuous "smart" beta.
* `align = "auto"` is removed from all `table_*()` functions; use
  `"decimal"` (default), `"center"`, or `"right"`.
* `table_regression(show_fit_stats = character(0))` errors; use `FALSE`
  to suppress the block.
* Multi-model `show_columns = "all_b"` / `"all_ame"` auto-compact
  (CIs dropped); request atomic tokens to keep them.
* `tidy()` labels AME rows `estimate_type = "ame"` (was `"AME"`).
* The `show_fit_stats` information criteria are lowercase tokens like
  everything else: `"aic"`, `"aicc"`, `"bic"` (was `"AIC"` / `"AICc"` /
  `"BIC"`). Uppercase errors with the replacement; rendered row labels
  are unchanged.
* The SE footer reads `"classical (Fisher information)"` (was
  `"classical (MLE inverse Hessian)"`).

## New supported models

`table_regression()` gains support for some 30 model classes beyond
`lm` / `glm`. See `?table_regression_models` for the full registry and
per-family behaviour, and the new vignettes for walk-throughs.

* Mixed effects (`lme4::lmer` / `glmer`, `glmmTMB`, `nlme::lme`):
  random effects as a block of rows (SD, correlations, residual, with
  SE and CI), ICC, per-group N and marginal / conditional R² as fit
  statistics, and a boundary-corrected LR test against the model
  without random effects. The test follows the fit's own estimator
  (REML or ML) and its full specification (prior weights, variance and
  correlation structures, zero-inflation).
* Bayesian (`rstanarm`, `brms`): posterior median, MAD SD, and
  equal-tailed credible intervals (header `95% CrI`;
  `ci_method = "hdi"` opts into the highest-density interval); no
  p-values — the probability of direction is opt-in
  (`show_columns = "pd"`). `R² (Bayes)` in the default fit statistics;
  `"elpd_loo"` / `"looic"` / `"waic"` opt-in with their standard
  errors in the footer and reliability caveats when the diagnostics
  flag them. Every table runs a sampler-diagnostics guard (R-hat, ESS,
  divergences, E-BFMI): problems add a footer line and a
  `spicy_bayes_diagnostics` warning, and per-coefficient `"rhat"` /
  `"ess_bulk"` / `"ess_tail"` columns are available -- plus `"mcse"`,
  the Monte Carlo standard error of the displayed posterior median
  (the criterion for how many digits the table can honestly show). Under
  `exponentiate = TRUE` all quantities come from the exponentiated
  draws (no delta method). The AME columns are draws-native too:
  `avg_slopes()` runs per posterior draw and the table reports the
  posterior median, MAD SD and credible interval of those draws (no
  `"ame_p"` -- same policy as the p column). Standardized betas work
  for the algebraic flavors (`"posthoc"` / `"basic"` / `"smart"`,
  exact affine rescales of the draws) on fixed-effects
  `stan_glm`-style fits; multilevel (`stan_glmer`) and non-GLM
  (`stan_polr`, `stan_betareg`) fits and `brmsfit` models (no
  design-matrix metadata) are refused with a pre-standardization
  hint, as are `"refit"` and `"pseudo"`.
  Variational / optimizing fits are refused with a refit hint.
* Survival (`survival::coxph` / `survreg`, `rms::cph`,
  `flexsurv::flexsurvreg`): Cox tables report `n` and `N events` as fit
  statistics and the concordance as a footer note.
* Categorical and ordinal (`nnet::multinom`, `mlogit::mlogit`,
  `MASS::polr`, `ordinal::clm`): a single `multinom` renders outcome
  categories as column groups (`outcome_labels` relabels them);
  `mlogit` uses a two-segment alternative-specific layout; ordinal
  thresholds render as a labelled block (`show_thresholds = FALSE` to
  opt out), and partial-proportional-odds `clm` fits render their
  non-proportional terms as their own block.
* Survey (`survey::svyglm`); robust / IV / panel (`estimatr`,
  `AER::ivreg`, `fixest`); beta, Tobit, and two-part counts
  (`betareg`, `AER::tobit`, `pscl::zeroinfl` / `hurdle`); plus
  `MASS::rlm` / `glm.nb`, `quantreg::rq`, `mgcv::gam` / `bam`,
  `stats::nls`, `rms::ols` / `lrm` / `Glm`,
  `sampleSelection::selection`.

Requests a class cannot honour are refused with a classed error
(`spicy_unsupported_vcov`, `spicy_unsupported_standardized`) instead of
rendering an empty column.

## New functions

* `table_regression_uv()`: univariable screening tables. One fit per
  candidate predictor, rendered as one table with a row block per
  predictor and merged side by side with the multivariable model.
  Supports `glm` (default), `lm`, and `coxph`
  (`outcome = Surv(time, status)`). A per-predictor `N` column is shown
  by default and a note discloses when Ns differ across fits;
  `complete_cases = TRUE` forces the common sample. Intercepts are
  hidden by default and `p_adjust` treats the whole screen as one
  family. `exponentiate`, `vcov` / `cluster` (the footer names the
  cluster column), `labels`, the output engines, and `tidy()` work
  as in `table_regression()`.
* `table_regression_models()`: the machine-readable registry of
  supported model classes (family, engine, AME, exponentiate
  semantics); its help page is the per-family reference.

## New features

* New `show_columns` token families `"rmst"` and `"risk_diff"` for
  `coxph` fits: covariate-adjusted differences in restricted mean
  survival time over `[0, tau]` and in cumulative incidence at
  `at_time`, computed by g-computation from the fitted model, with
  bootstrap SEs, CIs, and p-values. Factor predictors get one row per
  level (vs the reference); continuous predictors report the +1-unit
  contrast. The horizon is explicit and required -- `tau = "minmax"`
  picks the smallest per-group maximum follow-up -- and the table
  note states the estimand and the replicate count.
* New `show_columns` token `"n_events"`: outcome event counts displayed
  as `events/N` next to the estimates -- per factor level (reference
  row included) with model totals on continuous rows, computed on each
  model's own estimation sample. Available for binomial outcomes --
  `glm`, `glmer`, and `glmmTMB` fits alike -- and right-censored
  `coxph` fits; opt-in everywhere.
* `re_ci = "profile"`: profile-likelihood CIs for the variance
  components of `lmer` / `glmer` fits (asymmetric, boundary-respecting;
  no SE column, the footer discloses the method).
* `re_test = "lrt"` / `"rlrt"`: opt-in per-term tests for the variance
  components -- a boundary-corrected LR test or the exact restricted
  LRT -- filling the otherwise empty test columns of the
  `Random effects` rows.
* `ci_method = "profile"` gives profile-likelihood CIs for `glm`,
  `polr`, and `clm`; new `ci_method = "boot_percentile"` (with
  `vcov = "bootstrap"`) reports percentile bootstrap CIs from the same
  replicates as the bootstrap SEs. The footer names the method.
* Heteroskedasticity- and cluster-robust `vcov` for the supported
  classes. Resampling footers report the valid replicate count, and a
  bootstrap / jackknife whose replicates nearly all fail raises
  `spicy_resampling_failed` instead of silently reporting classical
  SEs.
* Cluster-robust `vcov` (`"CR0"`-`"CR3"`) for `nnet::multinom`,
  including its AME columns (needs sandwich >= 3.1-2, which added the
  `estfun()` method). `HC*` stays refused: a multi-equation model has
  no working residuals.
* Under a cluster-robust `vcov`, the ordinal Thresholds block
  (`polr` / `clm`) now takes its SEs, z, p and CIs from the same
  cluster sandwich as the slopes -- previously the cut-point rows
  kept model-based SEs under a footer describing the table as
  cluster-robust.
* AME columns are available for `betareg`, `mgcv::gam`, `svyglm`,
  `survreg`, `fixest`, `estimatr`, `quantreg::rq`, `AER::ivreg`, and
  `rms` fits, and per outcome category for `polr` / `clm` / `multinom`.
  AME SEs, CIs, and p-values honour a robust `vcov`.
* Two-part models show their full model: the zero component of
  `pscl::zeroinfl` / `hurdle` and the `ziformula` / `dispformula`
  components of `glmmTMB` render as labelled row blocks with full
  inference (`show_components = FALSE` to opt out). A component is
  exponentiated only when its link yields a ratio.
* Class-aware fit-statistics defaults: pseudo-R² for ordinal and
  multinomial fits;
  every other class falls back to `nobs` + `AIC` instead of a blank
  block. The `N (groups)` row upgrades to plain counts (e.g.
  `N (Subject)`) when models share a single grouping factor.
* Six new vignettes: *Mixed-effects*, *Multinomial*, *Count and
  two-part*, *Survival*, and *Ordinal regression tables*, plus
  *Categorical predictors* — a cross-cutting guide to dummy coding
  and reference levels, joint tests of a factor, ordinal predictors
  (scores vs dummies), successive-difference contrasts, and why
  continuous predictors should not be categorized.

## Minor improvements

* Wide multi-model tables split into stacked panels more cleanly:
  continuation panels carry no empty stub rows, and over-wide column
  spanners truncate with a visible ellipsis.
* Under `exponentiate = TRUE` with a visible SE column, the footer
  states the SE scale (delta method) and that the CI bounds are
  asymmetric.
* `show_fit_stats = FALSE` suppresses the fit-statistics block.
* Placeholder cells decimal-align in the `gt` / `flextable` /
  `tinytable` / Word / Excel outputs; `"deviance"` prints at 1 decimal;
  the descriptive tables use a single font in Word outputs.

## Bug fixes

* `gt` and `flextable` outputs now render in Quarto / R Markdown
  **Word**, PowerPoint, and PDF documents (they silently disappeared
  from non-HTML targets). A new `as_flextable()` method returns the
  underlying flextable for manual composition.
* `table_continuous_lm()` now discloses robust and resampling SEs in
  its table note, carries its notes into every rich output, and
  accepts `cluster = ~region`.
* The RMST and risk-difference columns extend to parametric survival
  models (`survival::survreg`): the AFT curves are closed-form, so
  the standardized survival is exact up to numerical integration.
  Cross-validated against `flexsurv::standsurv()` (exact) and the
  closed-form exponential RMST (machine precision). Stratified
  `survreg` fits (per-stratum scale) are refused.
* The univariable screen (`table_regression_uv(method = "coxph")`)
  accepts the RMST and risk-difference columns: unadjusted per-predictor
  estimands next to the covariate-adjusted ones of the multivariable
  column, all at one shared horizon (`tau = "minmax"` is refused
  there -- per-fit horizons would not be comparable).
* The RMST and risk-difference columns now support stratified Cox
  models (`strata()`): g-computation keeps each subject's own stratum
  baseline, and the table note says so. The strata variable itself
  gets no contrast row. Cross-validated against
  `adjustedCurves::adjusted_rmst()` (exact).
* `MASS::glm.nb` fits gain opt-in dispersion fit-stat rows:
  `show_fit_stats` tokens `"theta"` (the NB2 dispersion) and
  `"alpha"` (its reciprocal, the Stata convention). Refused with a
  clear error for other families.
* `betareg` fits gain the matching opt-in precision row: the
  `show_fit_stats` token `"phi"`, back-transformed from the
  precision link (`y ~ x | 1` reports the same phi as `y ~ x`).
  Refused for other families and when the precision has covariates
  (`y ~ x | z`), so phi is not a single number.
* `nnet::multinom` fits now report McFadden's and Nagelkerke's
  pseudo-R² (as the other categorical families do). The tokens were
  silently dropped from `show_fit_stats` before, and a weighted fit's
  null log-likelihood ignored the weights.
* Bayesian tables get their fit statistics: `"r2_bayes"` (the
  posterior-median Bayesian R², now in the all-Bayesian default
  block) and the opt-in `"elpd_loo"` / `"looic"` / `"waic"` tokens
  (PSIS-LOO / Watanabe-Akaike; the footer discloses the elpd
  standard error). Refused for frequentist fits; Bayes factors stay
  out by design.
* All-Bayesian tables drop the p column from the defaults (a dash
  column carries no information), refuse an explicit `"p"` / `"t"`
  request, expand the `"all_b*"` presets without them, and label the
  interval header `95% CrI`. Mixed frequentist + Bayesian tables
  keep the shared `95% CI` label and dash the Bayesian p cells.
* Bayesian tables gain the `"pd"` column (`show_columns`): the
  posterior probability of direction, the reporting-guideline-aligned
  reading of "is there an effect" -- computed at extraction since
  0.12 but never exposed. Refused for frequentist fits.
* Bayesian tables tighten up. `stan_glmer` / multilevel `brm` fits
  report their random effects as a proper block (posterior median SD,
  credible interval from the draws) instead of one flat row per group
  deviation; the footer reads `Random effects (MCMC)` and no
  likelihood-ratio line is printed. `p_adjust` and likelihood-based
  fit-statistic tokens (`AIC`, pseudo-R², ...) are refused with clear
  errors for all-Bayesian tables -- both were silent no-ops -- and
  the class-aware defaults report `n` only. Mixed frequentist +
  Bayesian tables keep the frequentist columns filled.
* Factors under non-default contrast codings (successive differences,
  sum-to-zero, Helmert, custom matrices) now group under their parent
  variable like treatment and polynomial codings, labelled by the
  contrast-matrix column names. No reference row is shown -- none
  exists under those codings.
* `ordinal::clm(scale = ~)` fits now render their scale (dispersion)
  coefficients as a `Scale effects` block. They were silently absent
  from the table: an estimated component of the model went unreported.
  The rows stay on the log scale under `exponentiate = TRUE` (their
  exponential is a ratio of latent standard deviations, not an odds
  ratio) and the footer says so.
* `broom::tidy()` gains an `outcome_level` column naming the response
  category of per-category rows (ordinal and multinomial average
  marginal effects). Those rows were previously indistinguishable --
  four `age` AME rows sharing one term, with nothing to identify the
  category.
* The random-effects LR test no longer prints a negative statistic
  (`-0.00`) on singular fits: the statistic is clamped at zero and the
  boundary p-value is now 1 -- half the chi-bar-squared null
  distribution is a point mass at zero, so a zero statistic carries no
  evidence (it printed 0.500, the ceiling of the halved-chi-squared
  formula).
* `ci_level` now reaches the random-effect variance-component CIs; all
  three mixed engines hardcoded 95% for those rows.
* Large mixed fits no longer spend minutes on variance-component SEs:
  above `options("spicy.re_se_max_n")` (default 1000) those cells are
  omitted, with a note and a warning giving the override.
* `nested = TRUE` now works for `multinom` (LR chi-square rows) and
  defaults to LRT rows for Cox comparisons (`lm`'s R² / F-change rows
  are undefined for a partial likelihood).
* Quantile regression (`quantreg::rq`) gets its own `vcov` estimator
  family: the default is now the heteroskedasticity-robust `"nid"`
  sandwich (quantreg's own large-sample default; the previous iid
  default understated SEs under heteroskedasticity and disagreed with
  the AME columns, which already used `"nid"`), with `"iid"`, `"ker"`,
  `"rank"` (rank-inversion CIs, no SE / t / p) and a native
  `"bootstrap"` as opt-ins. `cluster` works through the wild gradient
  cluster bootstrap (`vcov = "bootstrap"`); `HC*`, `CR*` and
  `"jackknife"` are refused with the reason. The footer names the
  estimator; AME columns share the coefficient rows' matrix.
* AME columns that silently rendered empty are now populated (`fixest`,
  `estimatr`, `quantreg`, `AER::ivreg`, `rms`, `pscl`) or refused with
  a pointer to `?table_regression_models` (classes with no AME
  backend).
* The statistic column header follows each model's actual reference
  distribution (`z` or `t`); it was hardcoded to `t`.
* Factor coefficient and AME rows follow `levels()` order (was
  alphabetical); ordered factors with AME columns show a reference row;
  `ame_ci` / `ame_p` / `ame_se` populate without the bare `"ame"`
  token; stars anchor on B (and AME), never on beta.
* Bootstrap / jackknife and `standardized = "refit"` refits no longer
  leak the caller's environment and now work on `factor()` / `log()` /
  `poly()` formulas; a failed refit falls back with a warning instead
  of silently changing method.
* Standardized beta rows on mixed fits inherit B's reference
  distribution (they showed a second p-value for the same test); the
  standardized-coefficient table note states the interaction
  convention and is fallback-aware.
* Titles: binomial mixed / survey fits are link-aware (a probit fit is
  no longer titled "Logistic"); Tobit titles name the response; ordinal
  titles name the shared-slopes assumption by its link; proper nouns
  keep their capitals in multi-model titles.
* Mixed fits with `cbind()` responses get their LR test against the
  no-random-effects model; `labels` accepts coefficient-level keys on
  mixed fits; `polr` / `clm` detect non-uniform prior weights.
* `flexsurv`: probit-scale splines and ancillary-parameter covariates
  refuse `exponentiate = TRUE`; factor predictors group under their
  parent variable with a reference row.
* `as_structured()` and the rich output engines match the console body
  exactly (blank vs em-dash reference cells, the multi-outcome
  `Outcome` row); the structured schema gains
  `reference_models_by_row` and `outcome_labels_by_col`.
* `table_regression(m1, m2)` without `list()` errors with a helpful
  message; colliding model labels no longer break `output = "gt"`;
  the `p_adjust` footer's family size matches the adjustment
  performed; `ci_method = "profile"` with a robust `vcov` defers to
  the `vcov` and warns; the singular-fit note states the fact and
  leaves the advice to a build-time warning.

# spicy 0.12.0

## New features

* New `table_regression()`: publication-ready coefficient summary
  for one or more fitted `lm` or `glm` models, side by side. APA
  Manual 7 formatting is the default. Highlights:

  * Robust variance: classical, HC, cluster-robust (CR) with
    Satterthwaite df, bootstrap, jackknife. Per-model `vcov`
    accepted for SE-comparison tables.
  * Standardisation: `refit`, `posthoc`, `basic`, `smart`,
    `pseudo` (the last `glm` only).
  * Average marginal effects (AME) as separate columns; AME
    inference shares the coefficient's variance estimator so B
    and AME are reported on the same inferential footing.
  * Partial effect sizes: f², η², ω² for `lm` (noncentral-F CIs);
    partial χ² for `glm`.
  * GLM response-scale reporting via `exponentiate = TRUE`, with
    family-appropriate labels (OR, IRR, HR, RR, MR, exp(B)) and
    optional profile-likelihood CIs (`ci_method = "profile"`).
  * Multiplicity correction via `p_adjust` (any
    `stats::p.adjust()` method).
  * Hierarchical comparison via `nested = TRUE` (ΔR² / F-change
    for `lm`; LRT for `glm`).
  * Display controls: variable filtering, intercept and factor
    placement, reference-row styles, multi-model labels, stars,
    decimal mark, per-column digits.
  * Outputs: console, `data.frame`, long tibble, `gt`,
    `flextable`, `tinytable`, Excel, Word, clipboard.
    `broom::tidy()` and `broom::glance()` methods supported.

  See `?table_regression` and `vignette("table-regression")`.

* `table_continuous_lm()` gains additive covariate adjustment via
  the new `covariates` argument. Two estimands for the per-group
  adjusted means: `"proportional"` (G-computation, default) and
  `"balanced"` (equal-weight synthetic grid). Under adjustment,
  `f²` and `ω²` become partial effect sizes; `d` and `g` raise an
  explanatory error. The auto-built footer documents the
  covariates and the estimand. See `vignette("table-continuous-lm")`.

* New exported `as_structured()` accessor returns a typed view of
  a `table_regression()` result for programmatic use: raw
  numerics, CI split into `LL` / `UL` columns, and a column-level
  format specification.

## Breaking changes

* `code_book()` no longer silently truncates the export filename
  to 120 characters. Very long titles now surface a clear
  OS-level error. **Migration**: shorten the title or pass an
  explicit `filename =` argument.

## Bug fixes

* `table_categorical()` no longer over-truncates a *p*-value in
  the interval `(10^-p_digits, 0.001)` when `p_digits >= 4`.
  Example: `p = 0.000108` now correctly prints as `".0001"` at
  `p_digits = 4` (was `"<.0001"`).
* `count_n(special = ...)` returns a length-`nrow(data)` zero
  vector when no usable column survives the list-column filter,
  matching the documented contract and the `count = ...` branch
  (was `numeric(0)`, which broke `dplyr::mutate()` pipelines).
* `lambda_gk()` and `goodman_kruskal_tau()` emit
  `spicy_undefined_stat` and return a fully-`NA` result on
  rank-1 contingency tables (constant predicted variable),
  matching the existing pattern in `gamma_gk()`,
  `kendall_tau_b()`, `somers_d()`, and `yule_q()`.
* `cross_tab()` no longer silently overwrites a user's y-variable
  level named `"N"`, `"Total"` or `"Values"`. The conflicting
  reserved column is auto-renamed with a numbered suffix and a
  single `spicy_renamed_column` warning is emitted.
* `broom::glance()` on a `spicy_continuous_lm_table` keeps
  `df.residual` numeric, so Satterthwaite degrees of freedom
  from `vcov = "CR2"` / `"CR3"` are preserved verbatim instead
  of being truncated through `as.integer()`.

## Minor improvements

* Console em-dash alignment: non-numeric placeholders (em-dash,
  "NA") sit at the decimal-mark column instead of the integer-
  part column (APA Manual 7 §7.13). Integer cells in mixed-
  precision columns (`n` row alongside `R²`) keep their right-
  aligned placement.
* `R/` source is byte-pure ASCII (`tools::showNonASCIIfile()`
  reports zero hits package-wide).
* `openxlsx2::wb_add_border()` calls now pass `NULL` on unused
  sides, preventing the default `"thin"` from being applied to
  all four sides of a cell when only one rule is intended.

# spicy 0.11.0

## New features

### `table_continuous_lm()`

* Cluster-robust SEs via `cluster` and four `vcov` choices
  (`"CR0"`–`"CR3"`), dispatched to `clubSandwich` with
  Satterthwaite df (`clubSandwich` in `Suggests`).
* `vcov = "bootstrap"` (nonparametric or cluster) and
  `vcov = "jackknife"` (leave-one-out / leave-one-cluster-out)
  variance estimators in pure base R, controlled by `boot_n`.
* Three new `effect_size` choices alongside `"f2"`: Cohen's
  `"d"`, Hedges' `"g"` (two-group only), Hays' `"omega2"`. New
  `effect_size_ci` adds noncentral *t* / *F* CIs rendered inline
  as `0.18 [0.07, 0.30]`.
* `HC*` estimators delegate to `sandwich::vcovHC()`;
  rank-deficient fits return a clean rank-by-rank covariance.

### Harmonisation across the table family

* Shared reporting vocabulary (`decimal_mark`, `p_digits`,
  `align`, named-`labels`) now spans `cross_tab()`, `freq()` and
  the three `table_*()` helpers, including APA-style p-value
  notation (`<.001` / `.045`, no leading zero).
* `table_categorical()`'s `assoc_measure` accepts a per-variable
  spec. When measures differ across rows the column collapses to
  `"Effect size"` and an APA-style `Note.` line documents the
  per-variable measure; `phi` on a non-2x2 errors.
* All three `table_*()` functions gain `as.data.frame()`,
  `tibble::as_tibble()`, `broom::tidy()` and `broom::glance()`
  methods (`broom` in `Suggests`).

## Quality and robustness

* **Classed conditions.** Errors and warnings now carry stable
  classes (`spicy_error` / `spicy_warning` plus 11 leaf classes
  documented in `?spicy`), so downstream code can dispatch via
  `tryCatch()` / `withCallingHandlers()` instead of matching
  message strings. `rlang (>= 1.1.0)` required.
* **Structured cli messages.** Multi-line errors and warnings
  (vcov fallbacks, bootstrap/jackknife failures, `padding`
  migration, `labels` length mismatch) render as cli bullets.
* **Locale-deterministic ordering.** Sorts in `varlist()`,
  `freq()`, `cross_tab()` and `table_*()` use
  `method = "radix"`. Output is byte-stable across locales and
  platforms, matching Stata / SPSS guarantees.
* **Edge-case hardening.** A new length-guarded sort helper makes
  `varlist()` / `code_book()` / `cross_tab()` / `freq()` survive
  zero-length or all-NA `Date` / `POSIXct` / `character` columns
  and factors with no observed levels.
* **Snapshot-locked rendering.** `tests/testthat/test-snapshots.R`
  pins the exact console output of every spicy print method, so
  any unintended formatting drift surfaces as a PR diff.
* **API stability contract.** `?spicy` documents which exports
  are stable, stabilising or internal. pkgdown reference groups
  exports via four `@family` tags.
* **Cross-software validation.** All 13 association measures
  agree with PSPP 2.0 (`CROSSTABS /STATISTICS=ALL`, 65 / 65
  statistics on four datasets); Cohen's *d* and Hedges' *g*
  noncentral CIs are tested numerically against
  `effectsize::cohens_d()` / `effectsize::hedges_g()`
  (`tolerance = 1e-6`); point-estimate formulas and asymptotic
  standard errors follow `DescTools` (Signorell et al.).

## Improvements

* `cross_tab()` warns when `correct = TRUE` is ignored on a
  non-2x2 sub-table, when `weights` contains `NA`, and notes
  statistics computed on a sub-table after empty rows / columns
  are pruned.
* `cross_tab()` validates `decimal_mark`, `p_digits` and
  `simulate_B` up front; `freq()` validates `decimal_mark` and
  tightens `digits` to a non-negative integer.
* A user category literally named `"N"` or `"Total"` is no longer
  mis-rendered as the totals row in `cross_tab()`.
* `table_continuous_lm(output = "long")` returns `n`, `df1`, `df2`
  as integer columns; `predictor_label` preserved on the
  degenerate-model fallback path.
* `cramer_v()` / `phi()` doc states the CI uses the Fisher
  z-transformation (point estimate and p-value identical to
  `DescTools` / SPSS).
* `uncertainty_coef()` doc states entropy uses `0 log 0 = 0`
  (matching SPSS, PSPP, Stata, Cover & Thomas).

## Bug fixes

* `label_from_names()` raises actionable errors on duplicate or
  empty new column names; trims whitespace and preserves the
  input class.
* `table_continuous_lm(output = "data.frame")` names contrast CI
  columns from `ci_level` (was hardcoded to 95 %).
* The categorical-predictor global Wald *F* degrades to `NA` on
  a singular coefficient covariance submatrix.
* The degenerate-table branch of `cramer_v()`, `yule_q()`,
  `gamma_gk()`, `kendall_tau_b()` and `somers_d()` respects
  `detail`: scalar `NA_real_` by default, fully shaped
  `spicy_assoc_detail` when `detail = TRUE`.
* `uncertainty_coef()` returns a finite estimate (was `NaN`) when
  a marginal is zero.
* `somers_d(direction = "symmetric")` returns the harmonic mean
  of the two asymmetric values, matching SPSS / PSPP `CROSSTABS`.
* `print.spicy_assoc_detail()` / `print.spicy_assoc_table()` use
  APA-strict `<.001` / `.045` notation, matching the rest of the
  package.
* `varlist()` / `code_book()` honour `factor_levels = "all"` for
  `haven_labelled` columns: declared-but-unobserved labels appear
  in the `Values` summary.
* `copy_clipboard()` rejects `row.names.as.col` vectors of length
  ≠ 1 and empty strings; accumulates all messages from
  `clipr::write_clip()` instead of overwriting.
* `mean_n()` / `sum_n()` reject non-integer `min_valid >= 1` and
  `min_valid > ncol`; their `digits` requires a non-negative
  integer.

## Breaking changes

* `table_continuous_lm()` and `table_categorical()` default to
  decimal-point alignment for numeric columns
  (`align = "decimal"`). Pass `align = "auto"` for the previous
  behaviour.
* `build_ascii_table()` / `spicy_print_table()`: `padding`
  switches from a string enum to a non-negative integer.
  Default `2L` (was `+5L`); printed tables are roughly 40 %
  narrower. **Migration**: `"compact" -> 0L`, `"normal" -> 2L`,
  `"wide" -> 4L`.
* `table_categorical(assoc_measure = "auto")` on a 2x2 table
  picks `phi` instead of `cramer_v`. Numeric value unchanged
  (|phi| = V on 2x2); only the column label changes.
* `freq()` drops observations with `NA` weights (with a warning)
  instead of recoding them to zero. Aligns with `cross_tab()`.
* `table_continuous_lm(output = "long")` returns `NA` in
  `es_type` / `es_value` when `effect_size = "none"` (was
  `"f2"`), and renames `sum_w` to `weighted_n`.

# spicy 0.10.0

## New features

* `code_book()` now accepts tidyselect-style variable selectors through `...`, matching `varlist()` and `vl()`.

* `code_book()` gains a `filename` argument for the base name of CSV, Excel, and PDF exports. When `NULL` (the default), the filename is derived from `title` and falls back to `"Codebook"` when needed. Filenames are sanitized to portable ASCII consistently across platforms.

* `varlist()` now summarizes matrix and array columns by their dimensions, and counts valid, missing, and distinct observations by rows.

* `freq()` gains a `factor_levels` argument that mirrors `varlist()` and `code_book()`. With `factor_levels = "all"`, declared-but-unobserved factor and labelled levels appear in the output with `n = 0`, matching SPSS `FREQUENCIES`; the default `"observed"` preserves the previous Stata `tab`-style behavior.

## Improvements

* `varlist()` now displays missing values as `<NA>` and `<NaN>` in the `Values` summary when `include_na = TRUE`, and quotes literal `"NA"`, `"NaN"`, and empty-string values so they cannot be confused with the missing markers.

* `varlist()` now emits a column-named warning and marks the failing cell as `<error: ...>` when a column cannot be summarized, instead of silently writing `"Invalid or unsupported format"`. Remaining columns are unaffected.

* `varlist()` produces more precise Viewer titles for extraction, pipe, and literal `get("name")` expressions, while keeping ambiguous dynamic calls anonymous (`vl: <data>`).

* `code_book()` now rejects partial-match names in `...` (e.g. `val = TRUE`, `tit = "x"`) that would otherwise be silently treated as tidyselect expressions, and surfaces `varlist()` selection errors directly.

* `freq()` now resolves the `weights` argument via tidy-eval, so column references nested in compound expressions (e.g. `weights = if (use_w) col else NULL`) work as expected. Qualified expressions like `weights = df2$w` continue to take precedence over column lookup.

* `freq()` validates `digits`, `sort`, `weights`, and the logical scalar arguments (`valid`, `cum`, `rescale`, `styled`) more strictly at the public boundary, with clearer error messages for non-finite values, `NA`, multi-element inputs, and non-numeric weight vectors.

* `freq()` now documents the interaction of `weights` containing `NA` with `rescale = TRUE` (Stata `pweight` semantics) and the dropping of unused factor / labelled levels (Stata `tab` semantics, with `code_book(factor_levels = "all")` as the schema-style alternative).

## Bug fixes

* `varlist()` now displays labelled values in the same prefixed-label order for compact and `values = TRUE` summaries; previously the compact summary used data order.

* `varlist(values = TRUE)` now deduplicates element types when summarizing list-columns. Previously `list(1L, 2L, "a")` produced `"List(3): character, integer, integer"`; now produces `"List(3): character, integer"`.

* `include_na = TRUE` now correctly appends `<NA>` markers for list-columns in both `varlist()` modes; previously it had no effect on this column type.

* `varlist()` now validates column names up front and gives clearer errors for missing, empty, `NA`, or duplicate names.

* `varlist()` now errors clearly when tidyselect expressions try to rename columns; `...` is for selecting variables, not renaming.

* `freq(data, x, weights = NULL)` now correctly treats the explicit `NULL` as "no weighting" instead of emitting a misleading `"variable 'NULL' not found"` error. Parameterized patterns like `weights = if (use_w) wts else NULL` are now supported.

* `print()` for `spicy_freq_table` no longer crashes when the `var_label` attribute is `NA_character_`, numeric, or multi-element; the `Label:` line is silently skipped for any value that is not a single non-empty string.

* `freq()` no longer surfaces the name of the ignored `data` vector in the printed footer when both `data` and `x` are passed as vectors. The footer now consistently shows the analyzed vector's name.

# spicy 0.9.0

## Breaking changes

* `table_continuous()` now enables inferential output by default when `by` is
  supplied. With a grouping variable, the `p` column from `test` is shown
  automatically (previous default hid it). This aligns the two table helpers:
  `table_continuous()` stays descriptive when `by` is absent, and reports the
  test *p*-value when `by` is supplied, matching `table_continuous_lm()`'s
  inferential default. To preserve the previous behavior, pass
  `p_value = FALSE` explicitly. `statistic` and `effect_size` remain `FALSE`
  by default and must still be enabled consciously.

* `varlist()` now displays observed factor levels by default in `Values`,
  matching its role as a quick inspection of the current data. Use
  `factor_levels = "all"` to display unused factor levels as well, which was
  the previous default behavior and remains the default in `code_book()`.

## Minor improvements

* `code_book()` gains a `factor_levels` argument. It defaults to `"all"` so
  exported codebooks continue to document all declared factor levels,
  including unused levels; use `"observed"` to mirror `varlist()` output.

* `freq()` now prints the `Freq.` column as integers regardless of
  `digits`, which continues to control percentage precision. This matches
  the convention of SPSS, Stata, and SAS `PROC FREQ` for weighted counts
  and keeps the two numeric concepts (discrete counts vs. continuous
  percentages) visually distinct.

* `freq(..., styled = FALSE)` now returns a genuinely plain `data.frame`
  with no `spicy_freq_table` rendering metadata clinging to it, so
  `str()`, `dput()`, and downstream programmatic use see only the
  tabulation columns. The metadata attributes (`digits`, `data_name`,
  `var_name`, `var_label`, `class_name`, `n_total`, `n_valid`,
  `weighted`, `rescaled`, `weight_var`) are now documented in
  `@return` and remain available on the invisibly returned
  `spicy_freq_table` object when `styled = TRUE` (the default).

* `table_continuous_lm()` documentation now clarifies why `p_value = TRUE`
  and `r2 = "r2"` are the defaults, and robust-variance fallback warnings
  are now more explicit when a model matrix is singular.

## Bug fixes

* `freq()` now correctly resolves qualified weight expressions such as
  `weights = other$w` or `weights = other[["w"]]` even when the referenced
  column name also exists in `data`. Previously the bare-name fallback
  could silently pull the weight vector from the wrong data frame when
  column names collided.

* `freq()` with `sort` and missing values now keeps the `NA` row at the
  end of the tabulation so the printed `Cum. Percent` and
  `Cum. Valid Percent` columns stay monotonic and match the
  Valid → Missing → Total display layout. Sorting previously could push
  the `NA` row between valid rows and make cumulative percentages appear
  to jump.

* `varlist()` now preserves literal `"NA"` and empty-string values in the
  `Values` summary instead of removing them as if they were missing values.

* `varlist()` now distinguishes actual `NA` values from `NaN` in the
  `Values` summary when `include_na = TRUE`.

* `varlist(values = TRUE)` now preserves factor level order in the
  `Values` summary, matching the default compact factor display.

* `varlist()` now validates `values`, `tbl`, and `include_na` up front and
  gives a clear error when one of them is not `TRUE` or `FALSE`.

# spicy 0.8.0

## New features

* `table_continuous_lm()` adds APA-style bivariate linear-model tables for continuous outcomes. It acts as the model-based companion to `table_continuous()` for reporting fitted mean comparisons or slopes in an `lm` framework, with one predictor per model, model-based means for categorical predictors, optional case weights, classical or HC0-HC5 variance estimators, multiple output formats (ASCII, tinytable, gt, flextable, Excel, clipboard, and Word), `output = "data.frame"` for the wide raw table, `output = "long"` for the analytic long table, and configurable display of tests, confidence intervals, fit statistics, and effect sizes.

## Minor improvements

* Installed package vignettes now avoid embedding heavy HTML table and codebook widgets during CRAN builds, reducing package size while preserving rich pkgdown article rendering.

* Website and vignette coverage now includes `table_continuous_lm()`, using the bundled `sochealth` data throughout and adding a dedicated article for model-based continuous summary tables.

* `table_continuous()` and `table_continuous_lm()` now support dedicated display precision for effect-size columns, and `table_continuous_lm()` also supports separate precision for `R²` columns, so model fit and effect sizes can be formatted independently from descriptive values and test statistics.

* `table_continuous_lm()` now keeps `n` as the unweighted analytic sample size in wide and rendered outputs, and can optionally add a separate `Weighted n` column reporting the sum of case weights.

# spicy 0.7.0

## New features

* `table_continuous()` is a new helper for continuous summary tables. It computes descriptive statistics (mean, SD, min, max, confidence interval of the mean, and n) for numeric variables, with tidyselect column selection, optional grouping via `by`, and multiple output formats (ASCII, tinytable, gt, flextable, Excel, clipboard, and Word).

* `table_continuous()` gains `effect_size` and `effect_size_ci` arguments. When `by` is used, `effect_size = TRUE` adds an "ES" column with the appropriate measure (Hedges' g, eta-squared, rank-biserial `r_rb`, or epsilon-squared) chosen automatically based on the test method and number of groups, and `effect_size_ci = TRUE` appends the confidence interval in brackets.

* `table_continuous()` gains a `test` argument (`"welch"`, `"student"`, or `"nonparametric"`) to choose the group-comparison method, along with independent `p_value` and `statistic` display toggles so users can request either or both outputs when `by` is used.

* ASCII console tables now split oversized outputs into stacked horizontal panels, repeating the left-most identifier columns so wide `freq()`, `cross_tab()`, `table_categorical()`, and `table_continuous()` prints stay readable in narrow consoles.

## Breaking changes

* `table_categorical()` replaces `table_apa()` as the public helper for categorical summary tables. It uses `select` and `by`, supports grouped cross-tabulation or one-way frequency-style tables when `by = NULL`, and consolidates output formats under a single `output` argument. Migrate existing `table_apa()` calls to `table_categorical()`, use `output = "default"` for ASCII tables and `output = "data.frame"` for plain data frames, and replace former `output = "wide"` / `style = "report"` paths with the formatted output engines.

* Excel export now uses `openxlsx2` instead of `openxlsx` for a lighter dependency footprint (no Rcpp compilation required).

## Minor improvements

* Package citation metadata now uses the current package title and CRAN DOI, so `citation("spicy")` matches `DESCRIPTION` and points to the package DOI.

* `table_categorical()` and `table_continuous()` now print shorter ASCII titles without appending the input data frame name, and no longer require `officer` for `output = "flextable"` alone; `officer` is now required only for Word export paths that actually write `.docx` files.

* `table_continuous()` now accepts tidyselect syntax in `exclude` in addition to character vectors, and no longer warns that `test` is ignored when it is still needed to compute effect sizes.

# spicy 0.6.0

## New features

* New family of association measure functions for contingency tables: `assoc_measures()`, `contingency_coef()`, `gamma_gk()`, `goodman_kruskal_tau()`, `kendall_tau_b()`, `kendall_tau_c()`, `lambda_gk()`, `phi()`, `somers_d()`, `uncertainty_coef()`, and `yule_q()`. Each returns a numeric scalar by default; pass `detail = TRUE` for a named vector with estimate, confidence interval, and p-value.

* `cross_tab()` gains `assoc_measure` and `assoc_ci` arguments. When both variables are ordered factors, it automatically selects Kendall's Tau-b instead of Cramer's V. The note format changes from `Chi-2: 18.0 (df = 4)` to `Chi-2(4) = 18.0`. Numeric attributes (`chi2`, `df`, `p_value`, `assoc_measure`, `assoc_value`, `assoc_result`) are now attached to the output data frame.

* `table_apa()` now dynamically labels the association measure column based on the measure used, instead of always showing "Cramer's V". New `assoc_measure` and `assoc_ci` arguments are passed through to `cross_tab()`.

* `table_apa()` gains `output = "gt"` to produce a `gt_tbl` object with APA-style formatting, column spanners, and alignment.

* `table_apa()` now correctly centers spanner labels over their column pairs in `tinytable` and `flextable` output.

* All association measure functions and `assoc_measures()` gain a `digits` argument (default 3) that controls the number of decimal places when printed. The p-value always uses 3 decimal places or `< 0.001`.

* `detail = TRUE` results now print with formatted output (aligned columns, fixed decimal places) via a new `print.spicy_assoc_detail()` method. `assoc_measures()` output uses a new `print.spicy_assoc_table()` method with the same formatting.

* New bundled dataset `sochealth`: a simulated social-health survey (n = 1200, 24 variables) with variable labels, ordered factors, survey weights, and missing values. Includes four Likert-scaled life satisfaction items (`life_sat_health`, `life_sat_work`, `life_sat_relationships`, `life_sat_standard`) for demonstrating `mean_n()`, `sum_n()`, and `count_n()`.

## Bug fixes

* `count_n()` now correctly counts `NA` values when `count = NA` and `strict = TRUE` are both used. List columns are now reported in verbose mode instead of causing silent errors.

* `cross_tab()` rescale logic now operates on complete cases only, so the weighted total N matches the unweighted N when missing values are present (consistent with Stata behavior).

* `freq()` now uses true `NA` consistently (instead of the `"<NA>"` string) in both weighted and unweighted paths. `cum_valid_prop` is now correctly `NA` for missing rows. Invalid `digits` and `sort` values are rejected with clear error messages.

* `mean_n()` and `sum_n()` now validate `min_valid` and `digits` arguments, rejecting non-numeric, negative, or multi-element values.

* `mean_n()`, `sum_n()`, and `count_n()` no longer trigger a tidyselect deprecation warning when `select` receives a character vector. Character vectors are now automatically wrapped with `all_of()`.

* `table_apa()` now preserves the original factor level order in row variables instead of sorting alphabetically. When `drop_na = FALSE`, the `(Missing)` category is placed at the bottom of each variable's levels. `percent_digits`, `p_digits`, and `v_digits` are now validated.

* `table_apa()` p-values no longer wrap across lines in `tinytable` HTML output.

## Breaking changes

* `cramer_v()` now accepts a `detail` argument. By default it returns a numeric scalar (as before). Pass `detail = TRUE` to get a 4-element named vector (`estimate`, `ci_lower`, `ci_upper`, `p_value`), or `detail = TRUE, conf_level = NULL` for a 2-element vector (`estimate`, `p_value`) without CI.

# spicy 0.5.0

## New features

* New `table_apa()` helper to build APA-ready cross-tab reports with multiple output formats (`wide`, `long`, `tinytable`, `flextable`, `excel`, `clipboard`, `word`).
* `table_apa()` exposes key `cross_tab()` controls for weighting and inference (`weights`, `rescale`, `correct`, `simulate_p`, `simulate_B`) and now handles missing values explicitly when `drop_na = FALSE`.

## Bug fixes

* `count_n()` no longer crashes when `special = "NaN"` is used with non-numeric columns. Passing `count = NA` now errors with a message directing to `special = "NA"`.
* `cross_tab()` fixes a spurious rescale warning for explicit all-ones weights and aligns the Cramer's V formula with `cramer_v()`.
* `table_apa()` no longer leaks global options on error. The `simulate_p` default is aligned to `FALSE`.
* `varlist()` title generation no longer crashes on unrecognizable expressions.

## Minor improvements

* `copy_clipboard()` parameter `message` renamed to `show_message`.
* `freq()` now dispatches printing correctly via S3.
* Removed unused `collapse` and `stringi` from `Imports`.

# spicy 0.4.2

* `cross_tab()` hardening: improved vector-mode detection (including labelled vectors), stricter weight validation, safer rescaling, and clearer early errors (e.g., explicit `y = NULL`).
* `cross_tab()` statistics are now computed on non-empty margins in grouped tables, avoiding spurious `NA` results; internal core path refactored to remove `dplyr`/`tibble` from computation while preserving user-facing behavior.
* `freq()` now errors clearly when `x` is missing for data.frame input and validates rescaling when weight sums are zero/non-finite.
* `count_n()`, `mean_n()`, and `sum_n()` regex mode is hardened (`regex = TRUE` now validates/defaults `select` safely).
* `mean_n()` and `sum_n()` now return `NA` (with warning) when no numeric columns are selected.
* `label_from_names()` now validates input type (`data.frame`/tibble required).
* `cramer_v()` now returns `NA` with warning for degenerate tables.
* Dependency optimization: `DT` and `clipr` moved to `Suggests`; optional runtime checks added in `code_book()` and `copy_clipboard()`.
* Tests expanded with regression coverage for all the above edge cases.

# spicy 0.4.1

* Fixed CRAN incoming check notes by removing non-standard top-level files.

# spicy 0.4.0

* Print methods have been fully redesigned to produce clean, aligned ASCII tables inspired by Stata's layout. The new implementation improves formatting, adds optional color support, and provides more consistent handling of totals and column spacing.

* Output from `freq()` and `cross_tab()` now benefits from the enhanced
  `print.spicy()` formatting, offering clearer, more readable summary tables.

* Documentation and internal tests were updated for clarity and consistency.

* `cross_tab()` gains an explicit `correct` argument to control the use of
  Yates' continuity correction for Chi-squared tests in 2x2 tables. The default
  behavior remains unchanged.

* The documentation of `cross_tab()` was refined and harmonized, with a clearer
  high-level description, improved parameter wording, and expanded examples.

* Minor cosmetic improvements were made to `varlist()` output: the title prefix
  now uses `vl:` instead of `VARLIST`, and the column name `Ndist_val` was renamed
  to `N_distinct` for improved readability and consistency.

* Minor cosmetic improvement: ASCII table output no longer includes a closing
  bottom rule by default.

# spicy 0.3.0

* New function `code_book()`, which generates a comprehensive variable
  codebook that can be viewed interactively and exported to multiple
  formats (copy, print, CSV, Excel, PDF).

# spicy 0.2.1

* `label_from_names()` now correctly handles edge cases when the
  separator appears in the label or is missing.

# spicy 0.2.0

* New function `label_from_names()` to derive and assign variable labels
  from headers of the form `"name<sep>label"` (e.g. `"name. label"`).
  Especially useful for LimeSurvey CSV exports (*Export results* ->
  *CSV* -> *Headings: Question code & question text*), where the default
  separator is `". "`.

# spicy 0.1.0

## Initial release

* Introduces a collection of tools for variable inspection, descriptive
  summaries, and data exploration.
* Provides functions to:
  * Extract variable metadata and display compact summaries (`varlist()`).
  * Compute frequency tables (`freq()`), cross-tabulations (`cross_tab()`),
    and Cramer's V for categorical associations (`cramer_v()`).
  * Generate descriptive statistics such as means (`mean_n()`), sums
    (`sum_n()`), and counts (`count_n()`) with automatic handling of
    missing data.
  * Copy data (`copy_clipboard()`) directly to the clipboard for quick export.
