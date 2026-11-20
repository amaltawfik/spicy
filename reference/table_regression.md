# Regression coefficient summary table

Publication-ready coefficient table from one or more fitted regression
models: linear and generalized linear models, robust,
instrumental-variable, quantile and fixed-effects estimators,
mixed-effects and GEE models, ordinal, multinomial, count and two-part
models, survival models, survey-design fits, additive, beta-regression
and sample-selection models, and Bayesian fits, 39 model classes listed
by
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md).
Several fits, of one class or of several, go side by side in one table.
For `lm` and `glm`, it supports standardized coefficients (\\\beta\\),
average marginal effects (AME), partial effect sizes (*\\f^2\\* /
*\\\eta^2\\* / *\\\omega^2\\* and partial *F* for `lm`; partial
*\\\chi^2\\* or partial *F* for `glm`), pseudo-\\R^2\\ and deviance
\\R^2\\ (`glm`), and a full vocabulary of variance estimators (classical
/ HC\* / cluster-robust with Satterthwaite-corrected df / bootstrap /
jackknife). `glm` covers binomial / poisson / Gamma / inverse.gaussian /
quasi families with any link.

## Usage

``` r
table_regression(
  models,
  vcov = "classical",
  cluster = NULL,
  ci_level = 0.95,
  ci_method = c("wald", "profile", "boot_percentile", "hdi"),
  boot_n = 1000L,
  tau = NULL,
  at_time = NULL,
  standardized = c("none", "refit", "posthoc", "basic", "smart", "pseudo"),
  exponentiate = FALSE,
  ame_type = c("slope", "unit", "sd"),
  partial_type = c("II", "III"),
  p_adjust = "none",
  show_columns = NULL,
  keep = NULL,
  drop = NULL,
  show_intercept = TRUE,
  show_thresholds = TRUE,
  show_components = TRUE,
  intercept_position = c("first", "last"),
  factor_layout = c("grouped", "flat"),
  reference_style = c("row", "annotation", "footer", "none"),
  reference_label = NULL,
  show_fit_stats = NULL,
  fit_stats_layout = c("first_col", "merged"),
  layout = c("columns", "stacked"),
  show_re = TRUE,
  re_scale = c("sd", "variance"),
  re_columns = c("est", "se", "ci"),
  re_test = c("none", "lrt", "rlrt"),
  re_ci = c("wald", "profile"),
  model_labels = NULL,
  outcome_labels = NULL,
  stars = FALSE,
  nested = FALSE,
  digits = 2L,
  p_digits = 3L,
  effect_size_digits = 2L,
  fit_digits = 2L,
  ic_digits = 1L,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  padding = 0L,
  labels = getOption("spicy.var_labels", FALSE),
  note_labels = NULL,
  title = NULL,
  note = NULL,
  note_add = NULL,
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  word_template = NULL,
  style = NULL
)
```

## Arguments

- models:

  A fitted model object, or a list of such fits (named or unnamed;
  classes may be mixed). Single fits are auto-promoted to a 1-element
  list. A broad set of model classes is supported – linear / generalized
  linear (`lm`, `glm`,
  [`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html)),
  mixed-effects (`lmer`, `lme`, `glmmTMB`), survival (`coxph`,
  `survreg`), ordinal (`polr`, `clm`, and the cumulative-link mixed
  models [`ordinal::clmm`](https://rdrr.io/pkg/ordinal/man/clmm.html)
  and `glmmTMB` with `family = glmmTMB::ordinal()`),
  [`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html)/`bam`, `betareg`,
  `mlogit`,
  [`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html), `rms`
  (`ols`/`lrm`/`cph`/`Glm`), and Bayesian (`rstanarm`/`brms`), among
  others. An unsupported class raises `spicy_unsupported`. Raw data +
  formula is not accepted – fit-only API.

- vcov:

  Variance-covariance estimator: `"classical"`, `"HC0"`-`"HC5"`,
  `"CR0"`-`"CR3"`, `"bootstrap"`, or `"jackknife"`. A scalar is recycled
  to all models; a list (one string per model) allows mixed estimators.
  Default `"classical"`. The resampling estimators (`lm` / `glm`,
  including [`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html))
  refit each replicate on resampled rows of the fixed evaluated design;
  for `glm.nb` the dispersion parameter theta is held at its full-sample
  estimate (Stata's `nbreg, vce(bootstrap)` re-estimates it per
  replicate – the two conventions differ slightly). Resampling that
  fails on nearly every replicate raises `spicy_resampling_failed`
  rather than silently reporting a different estimator. Quantile
  regression
  ([`quantreg::rq`](https://rdrr.io/pkg/quantreg/man/rq.html)) uses its
  own estimator family instead (`"nid"`, the default, `"iid"`, `"ker"`,
  `"rank"` and `"bootstrap"`); see
  [table_regression_robust](https://amaltawfik.github.io/spicy/reference/table_regression_robust.md).
  The values each class accepts are in *Robust SE availability by model
  class*. With `nested = TRUE`, the change test of each pair uses the
  same estimator (a Wald test; see *Hierarchical (nested) model
  comparison*). See *Inference and standard errors*.

- cluster:

  Cluster identifier for cluster-robust variance (used when `vcov` is
  `"CR0"`-`"CR3"` or a cluster-bootstrap / cluster-jackknife). Three
  accepted forms (see *How to specify `cluster`* in the details):

  - Formula: `~region`, `~region:year` (recommended).

  - String column name: `"region"`.

  - Atomic vector of length `nobs(fit)`: `df$region`,
    `interaction(df$region, df$year)`, ... (for keys derived on the
    fly).

  For multi-model use, pass a list of one form per model (mix-and-match
  allowed). A bare unquoted name (`cluster = region`) is not column
  selection: it is evaluated as an ordinary variable (the vector form
  when `region` exists in the calling environment, a migration error
  pointing at `~region` / `"region"` otherwise). Default `NULL` (no
  clustering).

- ci_level:

  Confidence level for all reported CIs (B, \\\beta\\, AME, partial
  effect sizes). Default `0.95`.

- ci_method:

  CI construction. `"wald"` (default) uses `estimate +/- z x SE`
  (`t x SE` for `lm`). `"profile"` (`glm` and ordinal
  [`MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html) /
  [`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html)) uses the
  profile-likelihood CI from
  [`stats::confint()`](https://rdrr.io/r/stats/confint.html) –
  [`MASS::confint.glm()`](https://rdrr.io/pkg/MASS/man/confint.html) for
  `glm`, `confint.polr()` / `confint.clm()` for ordinal fits –
  asymmetric, exact for likelihood-based inference (Venables & Ripley
  2002, Section 7.2, p. 198, and Section 8.4, pp. 220-221). Only the CI
  bounds change; estimate, SE, statistic and p-value remain Wald. For
  ordinal fits, see
  [table_regression_ordinal](https://amaltawfik.github.io/spicy/reference/table_regression_ordinal.md).
  `"profile"` with `lm` raises `spicy_invalid_input`.
  `"boot_percentile"` (requires `vcov = "bootstrap"` for every model)
  replaces the coefficient CI bounds with equal-tailed percentile
  intervals of the bootstrap replicates (the
  [`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html)
  `type = "perc"` convention; Davison & Hinkley 1997, ch. 5), reusing
  the same resamples as the bootstrap SEs. Only the CI bounds change;
  estimate, SE, statistic and p-value remain Wald from the bootstrap
  covariance – the Stata convention (normal-based table CIs by default,
  percentile on request via `estat bootstrap`). AME and
  standardized-beta CIs are not covered. With `exponentiate = TRUE` the
  percentile bounds are exponentiated (percentile intervals are
  transformation-respecting). `"hdi"` (Bayesian `stanreg` / `brmsfit`
  fits only) replaces the default equal-tailed credible interval with
  the highest-density interval; see
  [table_regression_bayesian](https://amaltawfik.github.io/spicy/reference/table_regression_bayesian.md).
  Requesting `"hdi"` for a frequentist fit, or `"profile"` /
  `"boot_percentile"` for a Bayesian fit, raises `spicy_invalid_input`.

- boot_n:

  Number of bootstrap replicates when `vcov = "bootstrap"`, and of the
  bootstrap of the RMST and risk-difference columns: a positive whole
  number, as in
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
  and
  [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md).
  Default `1000L`, the default of
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  which computes the same bootstrap variance. That variance, of a `lm`
  or `glm` fit, needs at least 10 valid replicates: a smaller `boot_n`
  is an error. Typed in a table that runs no bootstrap, it is ignored
  with a warning (class `spicy_ignored_arg`).

- tau:

  Survival models only: the RMST horizon of the `"rmst"` columns, a
  positive time or `"minmax"`, with no default. See
  [table_regression_survival](https://amaltawfik.github.io/spicy/reference/table_regression_survival.md).

- at_time:

  Survival models only: the landmark time of the `"risk_diff"` columns,
  a positive time, with no default. See
  [table_regression_survival](https://amaltawfik.github.io/spicy/reference/table_regression_survival.md).

- standardized:

  Standardization method for the `"beta"` column. One of `"none"`
  (default), `"refit"`, `"posthoc"`, `"basic"`, `"smart"`, `"pseudo"`.
  `"pseudo"` is *glm only* (Menard 2011 fully-standardized); using it
  with [`lm()`](https://rdrr.io/r/stats/lm.html) raises
  `spicy_invalid_input`. Supported classes: `lm`, `glm` (incl.
  [`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html)), the mixed
  engines (`"refit"` only; see
  [table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md)),
  and fixed-effects Bayesian fits (`"posthoc"`, `"basic"` and `"smart"`
  only; see
  [table_regression_bayesian](https://amaltawfik.github.io/spicy/reference/table_regression_bayesian.md)).
  Any other class raises `spicy_unsupported_standardized` rather than
  rendering an empty beta column. See the *Standardized coefficients*
  section.

- exponentiate:

  Logical. When `TRUE`, `B`, the CI bounds, and the SE (delta method:
  `SE_OR = OR x SE_log-odds`) are
  [`exp()`](https://rdrr.io/r/base/Log.html)-transformed for links where
  the result is a ratio: logit (`OR`), log (`IRR` / `RR` / `MR` per
  family, generic `exp(B)` for other log-link families – a genuine ratio
  of means), and binomial / ordinal cloglog (`HR`; grouped-time
  proportional hazards, Prentice & Gloeckler 1978). For the *ordinal*
  (cumulative) cloglog model the displayed `HR` is `exp(-B)`, not
  `exp(B)`; see
  [table_regression_ordinal](https://amaltawfik.github.io/spicy/reference/table_regression_ordinal.md).
  The statistic and p-value stay on the link scale (invariant under
  monotone transformation). Identity-link fits are left untouched (a
  `spicy_ignored_arg` warning fires when no model in the table
  exponentiates), so mixed `lm` + logit tables keep working. Any other
  link (probit, cauchit, inverse – the
  [`Gamma()`](https://rdrr.io/r/stats/family.html) default –, `1/mu^2`,
  sqrt, ordinal `loglog`, ...) raises `spicy_invalid_input`: the
  exponential of such a coefficient has no ratio interpretation, and
  reporting it would mislabel the estimate. Report response-scale
  effects for those models via the AME column instead. Note that the
  displayed CI is the link-scale CI with exponentiated endpoints (Wald
  or profile per `ci_method`), so it is asymmetric around the ratio and
  cannot be reconstructed as `estimate ± q × SE`; the delta-method SE
  follows the Stata convention (`[R] logistic`, Methods and formulas:
  `se(OR) = OR × se(b)`) and the log-scale CI endpoints carry the
  uncertainty more faithfully than this SE. The footer states the SE
  scale and the CI asymmetry whenever an SE column is displayed. It also
  names what a displayed intercept estimates once exponentiated, which
  is no ratio: the baseline odds (logit), risk (binomial log), rate
  (count families, log) or mean (other log-link families) at the
  reference profile. Default `FALSE`.

- ame_type:

  The quantity the AME columns report for a continuous predictor.
  `"slope"` (default): the average slope dY/dX, the derivative of the
  prediction averaged over the observations (Stata's `margins, dydx()`).
  `"unit"`: the average change in the prediction when the predictor
  rises by one unit from each observation's own value. `"sd"`: the same
  for a rise of one standard deviation, the unweighted standard
  deviation of the predictor in the estimation sample, again from each
  observation's own value. These are the uncentered unit and
  standard-deviation changes of Long and Freese (2014), averaged over
  the observations as in Mize, Doan and Long (2019). The step is taken
  on the variable as it is in the data, before any transformation the
  formula applies (`log(x)`, `poly(x, 2)`). For the observations with
  the highest values, it can carry the predictor beyond the values
  observed: the prediction there is an extrapolation of the model.
  Gelman and Pardoe (2007) consider changes only in the region of
  support of the data. `"sd"` is not the `"sd"` shortcut of
  [`marginaleffects::avg_comparisons()`](https://rdrr.io/pkg/marginaleffects/man/comparisons.html),
  which moves every observation between the same two values, the mean
  minus and the mean plus half a standard deviation. For an unweighted
  fit and marginaleffects' default forward contrast, it equals
  `avg_comparisons(fit, variables = list(x = sd(x)))`, the standard
  deviation taken over the rows the fit used. The three agree when the
  prediction is linear in the predictor (an `lm` without transformed
  terms: `"unit"` equals the slope, `"sd"` the slope times the standard
  deviation). Under a non-identity link they differ, the more so as the
  coefficient grows: with a log link and no interaction, the slope is
  `b * mean(mu)` and the change for one unit `(exp(b) - 1) * mean(mu)`.
  `"unit"` is the quantity the sentence "one more unit of X goes with
  ... more of Y" describes; the slope only approximates it. Categorical,
  logical and 0/1 predictors are contrasts with their reference level
  under every value. Weighted fits average with the same weights under
  every value, per-category models (ordinal, multinomial) report the
  quantity for each category probability, and Bayesian fits summarize
  its draws. The table note names the quantity. The AME is averaged over
  the data the model was fitted on, read again from the environment of
  the fit when the table is built: when they have changed since (rows
  dropped, values recoded), the AME cells of that model stay empty, with
  a warning. Ignored, with a warning, when no AME column is shown.

- partial_type:

  The hypothesis of the term-level tests and of the partial effect sizes
  built on them: every `partial_*` column (`"partial_f"`,
  `"partial_chi2"`, `"partial_f2"`, `"partial_eta2"`, `"partial_omega2"`
  and their intervals), for the models that have a term-level test
  (`lm`, `glm`,
  [`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html), `lmer` /
  `glmer`, `glmmTMB`,
  [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html)). `"II"`
  (default) tests a term against the model without it and without the
  interactions that contain it, the convention of `car::Anova(type = 2)`
  that respects marginality: a main effect is tested as if its
  interactions were absent. `"III"` tests a term's own coefficients with
  every other term kept, its interactions included, the default of SPSS
  UNIANOVA, SAS PROC GLM and `afex`: a main effect is tested at the
  average of the levels of the factors it interacts with. The two give
  the same test for a term contained in no interaction. The numbers `2`
  and `3` (or `"2"` and `"3"`), as
  [`car::Anova()`](https://rdrr.io/pkg/car/man/Anova.html), `afex` and
  `lmerTest` take them, name the same two types.

  A Type III test depends on how the factors are coded, so it is
  computed with every unordered factor (character and logical variables
  included) coded to sum to zero and every ordered factor by orthogonal
  polynomials, whatever contrasts the model was fitted with: the table
  shows the coefficients as fitted, and the test is that of
  `car::Anova(type = 3)` on the model refitted with
  `contrasts = list(A = "contr.sum", ...)`. For `lm` and the mixed
  models it is the Wald test of the term's coefficients in that coding
  (the F over its df for `lm`, the chi-square for the mixed models,
  under a robust `vcov` too); for `glm`, the likelihood-ratio chi-square
  or the F of the refit without the term's columns. A model with an
  aliased coefficient (an empty cell of an interaction) has no Type III
  test and is refused. So is a factor fitted with a contrast matrix of
  fewer columns than its levels minus one, a model that does not hold
  the whole factor. A numeric covariate that interacts with a factor
  makes the Type III test of that factor a test where the covariate is
  0, as in SPSS and SAS: a warning says so, and centering the covariate
  tests the factor at its mean. Nothing is centered for you. The note
  ends the definition of each partial statistic with "(Type II)" or
  "(Type III)", and `tidy()` and `output = "long"` carry the type in a
  `partial_type` column. Ignored, with a warning, when no partial column
  is shown.

- p_adjust:

  Multiple-comparison adjustment method applied to the family of
  estimated coefficient p-values within each model (intercept and
  reference rows excluded). One of `"none"` (default), `"holm"`,
  `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`, or `"BY"`.
  Delegated to
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html); applied
  per-model and per `estimate_type` (B and AME p-values are adjusted
  independently within their own families). The tests of the smooth
  terms of a GAM join the B family. Active adjustments are documented in
  the footer (method + family size). See the *Multiple-comparison
  adjustment* section for when this is and is not appropriate.

- show_columns:

  Character vector of tokens selecting the per-coefficient columns and
  their display order. Accepts **atomic tokens** (`"b"`, `"se"`, `"ci"`,
  `"t"`, `"p"`, `"df"` (the degrees of freedom of each coefficient's t
  test, blank under a z test; refused when every test of the table is
  z), `"beta"`,`"n"`, `"n_events"`, `"pd"` (probability of direction,
  Bayesian fits only), `"rhat"` / `"ess_bulk"` / `"ess_tail"` / `"mcse"`
  (per-coefficient sampler diagnostics and the Monte Carlo standard
  error of the displayed posterior median, all-Bayesian tables only),
  `"ame"`, `"ame_se"`, `"ame_ci"`, `"ame_p"`, `"ame_df"` (the `"df"` of
  the AMEs), `"rmst"` + `"rmst_se"` / `"rmst_ci"` / `"rmst_p"`,
  `"risk_diff"` + its `_se` / `_ci` / `_p` companions, `"partial_f2"` +
  `"partial_f2_ci"`, `"partial_eta2"` + `"partial_eta2_ci"`,
  `"partial_omega2"` + `"partial_omega2_ci"`, `"partial_chi2"`,
  `"partial_f"` (`lm`, and `glm` whose dispersion is estimated), `"r2"`
  / `"adj_r2"` (per-fit variance explained, `lm` screens)) and **group
  tokens** (`"all_b"`, `"all_b_compact"`, `"all_b_full"`, `"all_beta"`,
  `"all_ame"`, `"all_ame_compact"`, `"all_f2"`, `"all_eta2"`,
  `"all_omega2"`). See
  [table_regression_tokens](https://amaltawfik.github.io/spicy/reference/table_regression_tokens.md)
  for the full enumeration. Default `NULL` selects a context-aware
  layout: `"all_b"` (single model) or `"all_b_compact"` (multi-model,
  and the single-multinomial outcome-as-columns layout, which has the
  same width pressure – restore CIs with atomic tokens, e.g.
  `c("b", "se", "ci", "p")`). The `"p"` token is always the B / beta
  p-value; for the AME-specific p-value use `"ame_p"`. `"none"` shows
  the fit statistics alone, one column per model (see *Fit statistics
  alone* in
  [table_regression_tokens](https://amaltawfik.github.io/spicy/reference/table_regression_tokens.md)).

- keep:

  Character vector of term names or regexes. Only coefficient rows whose
  term name (as in [`stats::coef()`](https://rdrr.io/r/stats/coef.html)
  – e.g. `"wt"`, `"cyl6"`, `"factor(cyl)8"`; the terms of an `rms` fit
  in the spelling of [`lm()`](https://rdrr.io/r/stats/lm.html), `"cyl6"`
  for rms's `"cyl=6"` and `"wt:cyl6"` for `"wt * cyl=6"`) matches at
  least one pattern are kept. A pattern is a Perl regular expression,
  and one identical to a term name also selects that term as written,
  with no escaping (`"log(bili)"`). The intercept and the subordinate
  blocks (thresholds, random effects) are exempt from `keep` and `drop`:
  `show_intercept`, `show_thresholds` and `show_re` govern them. The
  smooth terms of a GAM are predictors and are not exempt: they match by
  their mgcv label (`"s(age)"`, `"te(bmi,ped)"`), and `drop = "^s\\("`
  removes the `Smooth terms` block with its note. The `Error terms` of a
  sample-selection model are coefficients of
  [`coef()`](https://rdrr.io/r/stats/coef.html) and follow `keep` and
  `drop` under those names (`"sigma"`, `"rho"`, `"invMillsRatio"`). A
  multinomial or sample-selection model alone, whose categories or
  equations are column groups, matches its terms without their prefix
  (`"educ"`, `"cityyes"`); beside other models, with it
  (`"Student: age"`, `"outcome: educ"`). Mutually exclusive with `drop`.
  Filtering is a display choice; `p_adjust` runs against the full
  coefficient family before filtering. Default `NULL` (no filter).

- drop:

  Character vector of regexes. Coefficient rows matching any pattern are
  removed, with the exemptions of `keep`. Mutually exclusive with
  `keep`. Default `NULL`.

- show_intercept:

  Whether to display the intercept row. Default `TRUE` (APA convention).
  Hide via `FALSE`.

- show_thresholds:

  Ordinal models only: `TRUE` (default) shows the cut-points as a
  `Thresholds` block of rows, `FALSE` gives them in the note. See
  [table_regression_ordinal](https://amaltawfik.github.io/spicy/reference/table_regression_ordinal.md).

- show_components:

  Models with secondary components only (the `pscl` and `glmmTMB`
  two-part and dispersion models, `clm` with `scale =`, the precision
  sub-model of a `betareg` `y ~ x | z`, the error terms of a
  sample-selection model): `TRUE` (default) shows the components as
  labeled blocks of rows, `FALSE` omits them. A `Precision` block stays
  on the scale of its link (log by default), under `exponentiate = TRUE`
  too. See
  [table_regression_counts](https://amaltawfik.github.io/spicy/reference/table_regression_counts.md),
  [table_regression_ordinal](https://amaltawfik.github.io/spicy/reference/table_regression_ordinal.md)
  and
  [table_regression_models](https://amaltawfik.github.io/spicy/reference/table_regression_models.md).

- intercept_position:

  Where to place the intercept when shown. `"first"` (default, APA) or
  `"last"` (Stata-style, intercept just above the fit-stats footer).
  Ignored when `show_intercept = FALSE` (with `spicy_ignored_arg`
  warning).

- factor_layout:

  Layout of factor predictors. Applies to **any categorical predictor**
  – `factor`, `ordered`, `character`, or `logical` (R coerces the latter
  two to factors at fit time). Two options:

  - `"grouped"` (default): the variable name on its own header row
    ending with `:` (e.g., `education:`); each level follows as an
    indented sub-row with the bare level name. APA convention.

  - `"flat"`: each non-reference dummy is one row with the
    `<variable><level>` form (e.g., `educationUpper`); no header, no
    indent. Econometrics convention.

- reference_style:

  Rendering of factor reference levels. Four modes, distinguishing WHERE
  the reference information is exposed (in a row, inline, in the footer,
  or nowhere):

  - `"row"` (default): explicit row `Female (ref.)` with en-dashes in
    all stat columns (NEJM / BMJ clinical convention). `reference_label`
    controls the suffix.

  - `"annotation"`: the row is dropped and the reference is shown
    inline. Under `factor_layout = "grouped"` the factor header reads
    `education: [ref: Lower]`; under `factor_layout = "flat"` the marker
    `[vs Lower]` is attached to the **first non-reference dummy** of
    each factor (subsequent dummies inherit the same reference).

  - `"footer"`: the row is dropped and a sentence
    `Reference categories: education = Lower; sex = Female.` is added to
    the footer note. SAS `PROC LOGISTIC` / SPSS "Categorical Variables
    Codings" convention. Best for publication-grade dense multi-factor
    tables.

  - `"none"`: the row is dropped and no reference information is
    displayed anywhere. The user is responsible for stating the
    reference convention elsewhere (article text, table caption). Under
    `factor_layout = "flat"`, an informational message is emitted to
    flag the silent omission.

  **Ordered factors with AME**: under R's default `contr.poly`, ordered
  factors have B coefficients named `.L` / `.Q` / `.C` (orthogonal
  polynomial trends) which have no per-level reference semantics. When
  `"ame"` is in `show_columns`, however, the AME block is per-level
  contrasts against `levels()[1]`. A synthetic reference row anchored on
  `levels()[1]` is therefore emitted so the reader sees the AME baseline
  explicitly, with the same `reference_style` handling as plain
  treatment-coded factors. The `[vs <ref>]` annotation in `"annotation"`
  mode is attached to the first AME row, not to the polynomial-trend
  rows.

- reference_label:

  Suffix shown after the reference level in `reference_style = "row"`
  mode. `NULL` (default) prints `"(ref.)"`, translated under
  `options(spicy.language = )`; a string you supply is printed as typed.
  Ignored by the other three modes, whose fixed wording follows the
  language: `[ref: ...]`, `[vs ...]`, and the footer line
  `Reference categories: ...`. The suffix follows the level after a
  space, unless it opens on `,`, `;` or `:`, which attach to the level
  directly: a level such as `T0 (2018)` reads `T0 (2018) (ref.)`;
  `reference_label = ", ref."` gives `T0 (2018), ref.`;
  `reference_style = "annotation"` moves the reference out of the row.

- show_fit_stats:

  Character vector of tokens for the model-level rows below the
  coefficients; row order follows token order. `NULL` (default) resolves
  class-aware:

  - `lm`,
    [`estimatr::lm_robust()`](https://declaredesign.org/r/estimatr/reference/lm_robust.html):
    `c("nobs", "r2", "adj_r2")`.
    [`estimatr::iv_robust()`](https://declaredesign.org/r/estimatr/reference/iv_robust.html)
    gets `"nobs"` alone, and an IV
    [`fixest::feols()`](https://lrberge.github.io/fixest/reference/feols.html)
    neither R-squared nor AIC – the 2SLS R-squared is not the classical
    one. Asked for, it is the R-squared of `ivreg()`, on the structural
    residuals.

  - a logit on a binary outcome (`glm` or binary
    [`rms::lrm`](https://rdrr.io/pkg/rms/man/lrm.html)):
    `c("nobs", "pseudo_r2_tjur", "pseudo_r2_mcfadden", "aic")`.

  - the other binomial, Poisson and negative binomial `glm` fits,
    `polr`, `clm`, `multinom`, ordinal
    [`rms::lrm`](https://rdrr.io/pkg/rms/man/lrm.html):
    `c("nobs", "pseudo_r2_mcfadden", "aic")`.

  - gaussian, Gamma, inverse Gaussian and Tweedie `glm` fits:
    `c("nobs", "r2_deviance", "aic")`.

  - mixed `lm` + `glm`: the union of the two (the renderer en-dashes per
    cell the stat not defined for a given model class).

  - mixed-effects, survival, survey-design, Bayesian, robust and panel
    models: see the page of the family (*Pages by model family*).

  Under `nested = TRUE` the default is extended with the
  class-appropriate change-stat tokens (e.g. `"r2_change"`, `"f_change"`
  for `lm`). See
  [table_regression_tokens](https://amaltawfik.github.io/spicy/reference/table_regression_tokens.md)
  (`show_fit_stats` section) and *Hierarchical (nested) model
  comparison* in the details for the full vocabulary. A token you name
  must have a value for at least one model of the table, or the call is
  refused with an error that names it (a change token also needs
  `nested = TRUE`). A token of the default that no model has is left
  out.

- fit_stats_layout:

  Layout of the fit-stat values (`n`, `R^2`, `AIC`, ...) within each
  model's column group. Two options:

  - `"first_col"` (default): the value is placed in the FIRST numeric
    sub-column of each model (typically `B`); the model's remaining
    sub-columns (`SE`, `LL`, `UL`, `p`, ...) are left empty for that
    row. The APA Manual 7 Table 7.15 layout (p. 219).

  - `"merged"`: the model's numeric sub-columns are merged into a single
    wide cell containing the fit-stat value, centered under the model
    spanner. Stata `esttab` layout / *Econometrica* and *AER* journal
    convention. Resolves the mixed-precision look of `"first_col"` (an
    integer `n` row sharing the B column with two-decimal coefficients).

  `excel`, `flextable`, `word` and `tinytable` (in HTML, LaTeX and
  Typst) merge the cells. The markdown output of `tinytable`, `gt` (with
  a warning), `clipboard` and `default` (console) keep the `"first_col"`
  layout. In `"first_col"` mode the `B` column aligns the fit-stat
  values on the decimal mark with its coefficients. In `"merged"` mode
  the fit-stat values move out of the `B` column, which aligns its
  coefficients alone, and the merged values of a model align on the
  decimal mark among themselves, one centered block under the model
  (Excel centers each number as it is). Under `nested = TRUE` the change
  rows carry their degrees of freedom (`F-change` prints
  `7.29 (2, 28)`), which widens the `B` column in the `"first_col"`
  layout; `"merged"` spans them over the model's columns in the engines
  that merge.

- layout:

  How several models are laid out. `"columns"` (default) sets them side
  by side, one group of columns per model. `"stacked"` sets them one
  under the other, one panel per model: each panel is the table of that
  model alone (its default columns, the confidence interval included,
  its blocks and fit statistics), opened by a row with its label, then
  by an Outcome row where *Stacked layout* says. See *Multi-model
  semantics* and
  [`table_stack()`](https://amaltawfik.github.io/spicy/reference/table_stack.md),
  which stacks tables built separately.

- show_re:

  Mixed models only: `TRUE` (default) renders the variance components as
  a `Random effects` block of rows, `FALSE` suppresses it. See
  [table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).

- re_scale:

  Mixed models only: `"sd"` (default) or `"variance"`, the scale of the
  random-effects rows. See
  [table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).

- re_columns:

  Mixed models only: the cells of the random-effects rows that are
  displayed, a subset of `c("est", "se", "ci")`. See
  [table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).

- re_test:

  Mixed models only: `"none"` (default), `"lrt"` or `"rlrt"`, a per-term
  test of the variance components. See
  [table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).

- re_ci:

  Mixed models only: `"wald"` (default) or `"profile"`, the intervals of
  the random-effects rows of `lmer` and `glmer` fits. See
  [table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).
  `re_scale`, `re_columns`, `re_test` or `re_ci` set to other than its
  default on a table where no model has random effects raises one
  `spicy_ignored_arg` warning.

- model_labels:

  Per-model labels used as the **column-group spanner** above each
  model's sub-columns (console + gt / flextable / tinytable / Excel /
  Word renderers). `NULL` (default) resolves automatically; see
  *Multi-model semantics* for the full rule. A character vector of
  length `length(models)` overrides. Refused (error) for a single
  multinomial or sample-selection model: there the column groups are
  outcome categories or equations, relabeled via `outcome_labels`
  ([table_regression_categorical](https://amaltawfik.github.io/spicy/reference/table_regression_categorical.md)).

- outcome_labels:

  The **Outcome body row**, which names the outcome of each model under
  the column headers. `NULL` (default) shows it when the models do not
  all have the same outcome and the column headers do not name them:
  models named in the list or by `model_labels`, or an outcome that
  several of the models share. Otherwise the title (one outcome) or the
  column headers (the outcomes of unnamed models, all different) name
  them, and the row is left out. A character vector of length
  `length(models)` forces the row with those values (the spanner stays
  as `"Model 1, ..."` unless `model_labels` is also supplied), a single
  model included. `FALSE` removes the row. With `layout = "stacked"`,
  the row opens each panel, under the same rule, the labels of the
  panels taking the place of the column headers; a vector gives each
  panel its label.
  [`table_stack()`](https://amaltawfik.github.io/spicy/reference/table_stack.md)
  applies the rule to tables built separately, a table built with
  `FALSE` taking no row. A multinomial model alone has no Outcome row,
  its title naming the outcome; in a stack, its panel takes one by this
  rule, the label once, across its columns. The console, flextable,
  Word, Excel and tinytable (HTML, LaTeX, Typst) print each label across
  the columns of its model. gt, which merges no cells of the body, moves
  the row into the header: each label spans the columns of its model,
  under the model's name. A stacked gt table and the markdown grid of
  tinytable keep the label in the first column of the model, so a long
  label widens that column. For a single multinomial model, the labels
  of the category spanners; see
  [table_regression_categorical](https://amaltawfik.github.io/spicy/reference/table_regression_categorical.md).
  For a single sample-selection model, the labels of its two equation
  groups, in their order (outcome, selection).

- stars:

  Significance asterisks. `FALSE` (default; the APA Manual 7, Section
  7.14, p. 204, prefers exact p-values) – no stars. `TRUE` – APA cutoffs
  `c("*" = 0.05, "**" = 0.01, "***" = 0.001)`. A named numeric vector
  specifies custom thresholds, e.g.
  `c("+" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001)`. With
  `output = "excel"` a marked estimate is written as text (`"64.63***"`
  is not a number); the cells with no marker stay numeric.

- nested:

  Whether to inject pairwise change-statistic rows for adjacent models
  (M2 vs M1, M3 vs M2, ...). `FALSE` (default) – pure side-by-side
  display. `TRUE` – requires identical `nobs` and identical response
  variable across all models. Under a non-classical `vcov`, each pair is
  tested by a Wald test of the coefficients the current model adds, on
  that model's matrix, shown as "Wald F-change" or "Wald \\\chi^2\\
  (change)" by the regime of its coefficient tests. See *Hierarchical
  (nested) model comparison* for the rule, the classes it covers and its
  small-sample caution.

- digits:

  Decimal places for general numeric tokens (`b`, `beta`, `se`, `ci`,
  `t`, `f_change`, `lrt_change`, `ame`, `ame_se`, `weighted_nobs`; the
  Wald change statistics under a robust `vcov` follow `f_change`) and
  for the term-level tests (`partial_chi2`, `partial_f`). Default `2L`.

- p_digits:

  Decimal places for p-values (`p`, `ame_p`, `p_change`). APA-strict:
  leading zero stripped, `<.001` (or `<.0001` etc. depending on
  `p_digits`) for small values. Default `3L`; at least `1L`, as in the
  descriptive tables. Every `*_digits` argument is a whole number: a
  fraction is an error.

- effect_size_digits:

  Decimals for per-coefficient effect sizes (`partial_f2`,
  `partial_eta2`, `partial_omega2`) and their intervals. Default `2L`.

- fit_digits:

  Decimals for variance-explained / model-level effect-size fit stats
  (`r2`, `adj_r2`, `r2_change`, `adj_r2_change`, `omega2`, `f2`,
  `f2_change`, `sigma`, `rmse`). Default `2L`.

- ic_digits:

  Decimals for information criteria and deviance (`AIC`, `AICc`, `BIC`,
  `deviance`, and their `_change` form). Default `1L`.

- decimal_mark:

  The decimal mark, a single character: `"."` (the default) or `","` in
  most styles; the Lancet style of
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  uses the middle dot. When `","` is used, the CI bracket separator
  switches to `"; "` automatically to avoid `"0,18 [0,07, 0,30]"`
  ambiguity. With `output = "excel"`, a cell that shows one number is a
  number, under any mark, which shows the decimal separator of the
  reader's Excel; the mark applies to the text cells (`<,001`). See
  *Excel output* in
  [spicy-package](https://amaltawfik.github.io/spicy/reference/spicy-package.md).

- align:

  Numeric column alignment. `"decimal"` (default) – pre-pad cells so
  decimal marks line up vertically (publication-style). For CI cells
  (`[LL, UL]`) the left bracket, the LL decimal point, the comma
  separator, the UL decimal point, and the right bracket are
  independently aligned across rows. `"center"` and `"right"` apply the
  same alignment to every numeric column.

- padding:

  Non-negative integer giving the extra characters added to each data
  column's auto-computed width when the default `print` method renders
  the table. Default `0L` (compact – fits more models in the same
  console / page width). Use `2L` (Stata-like) or `4L` for a more
  spacious look. Headers stay centered above the data region regardless
  of padding.

- labels:

  How the variables are named in the table: the outcome in the title
  (and above its columns when the models have different outcomes), a
  numeric term in its row, and a factor in its header. Four forms:

  - `labels = FALSE` shows the names the model gives them (`bmi`, `age`,
    `sex`).

  - `labels = TRUE` shows each variable's `label` attribute (as set by
    `haven`, `labelled`, or
    [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md))
    when it is present, not `NA`, and not empty, and the name otherwise:
    "Linear regression: Body mass index", "Age (years)", "Sex:". The
    attribute is read off the model frame, or off the data the model was
    fit on where the model frame lost it. Factor levels, interactions,
    and transformed terms (`log(income)`, `poly(age, 2)`) are not
    variables and keep their names, as do the coefficient rows of
    `factor_layout = "flat"`.

  - `labels = c(age = "Age (years)", sexMale = "Men")`, a named
    character vector, gives the variables and rows it names these
    labels. Every other variable follows `options(spicy.var_labels)`:
    its name when the option is unset or `FALSE`, its `label` attribute
    when it is `TRUE`.

  - `labels = var_labels(age = "Age in years")` gives the variables and
    rows it names these labels, and every other variable its `label`
    attribute, whatever the option says.
    [`var_labels()`](https://amaltawfik.github.io/spicy/reference/var_labels.md)
    alone is `labels = TRUE`.

  The names are model identifiers, not display text: the outcome, the
  term labels of [`stats::terms()`](https://rdrr.io/r/stats/terms.html)
  (`"age"`, `"sex"`, `"age:sex"`, a factor's term naming its header, an
  interaction's term its row when it has one row), and the coefficient
  names (`"(Intercept)"`, `"sexMale"`; those of an `rms` fit in the
  spelling of [`lm()`](https://rdrr.io/r/stats/lm.html), `"sexMale"` for
  rms's `"sex=Male"` and `"age:sexMale"` for `"age * sex=Male"`), each
  naming one row. The reference level of a factor takes the name its
  coefficient would have: `c(sexFemale = "Women")` renames the row
  `Female (ref.)` to `Women (ref.)` when `Female` is the reference, the
  mark of `reference_label` kept. They keep that form whatever the
  language of the table. A class with no `terms` component
  ([`nls()`](https://rdrr.io/r/stats/nls.html)) is keyed on its
  parameter names alone. A sample-selection model is keyed on the term
  labels of its two equations (`"city"` for the header of a factor) and
  on its coefficient names (`"cityyes"`). With several models, a name is
  looked up in the terms of all of them and lands on the rows where it
  exists. A name that matches nothing in the table warns (class
  `spicy_unknown_name`), with the list of its terms, and is left out, so
  one vector can still serve every table of a project. The headers of
  the subordinate blocks (`Random effects`, `Thresholds`,
  `Zero-inflation`, ...) are set by the package and cannot be relabeled
  here.

  The default reads `options(spicy.var_labels)`, and a `TRUE` or `FALSE`
  typed in the call wins over it. Any other value, `NULL` included, is
  an error. A variable label documents the data file; a published table
  carries the wording you set here. `note_labels` gives the outcome a
  shorter label in the title.

- note_labels:

  The label a variable takes where the table names it in a sentence, in
  the title and in the notes, in place of the one `labels` gives it: a
  named character vector, `note_labels = c(sex = "sex at birth")`, or a
  [`var_labels()`](https://amaltawfik.github.io/spicy/reference/var_labels.md)
  object, which reads as one. `labels` can then name a row with a whole
  question while the note says "Missing values removed: sex at birth
  (2).". The rows, the column headers and the spanners keep the labels
  of `labels`, and so do the title and the notes for a variable
  `note_labels` does not name. A name that matches no column warns and
  is left out, as in `labels`, and `NULL` (the default) renames nothing.
  A note label that another variable of the table shows is an error
  (class `spicy_label_collision`): choose another one, or relabel the
  other variable.

- title, note:

  Override or suppress the auto-built caption / methodological footer.
  Three modes per argument, those `note` has in every table function:

  - `NULL` (default): the package builds the standard caption ("Linear
    regression on `<DV>`" / "Hierarchical linear regression on `<DV>`" /
    ..., the outcome named by its label in `note_labels`, else in
    `labels`) and a methodological note, one paragraph in the order of
    every table of the package: what the model is (its family, design,
    scale of the coefficients, reference categories, ...), the cases a
    univariable screen fits on, the inference (VCV type, p-adjust
    method, ...), the glosses of the blocks, then the abbreviations the
    column headers and the rows of the fit statistics and of the random
    effects show ("AIC = Akaike information criterion"), defined as the
    style in force asks (`spicy_style(abbreviations = )`; under `"jama"`
    they open the note on a line of their own), and the sentences of
    `note_add`. Under `stars`, the legend of the stars follows on a line
    of its own.

  - `FALSE`: the corresponding banner row is omitted from every output
    engine, for `note` the legend of the stars included. Use when the
    surrounding manuscript provides its own caption / note.

  - character string (length 1): replaces the auto-built text. A `title`
    is printed verbatim. A `note` replaces the general note and is
    printed as typed (multi-line notes accepted via embedded `"\n"`)
    after the lead-in of the style, which a string opening with a
    lead-in of its own does not repeat, the sentences of `note_add`
    after it; the legend of the stars stays on its line below, and
    `note = ""` keeps that legend alone.

  Validation messages, the spanner row, and the in-body change- stat
  rows are *not* affected – they belong to the table structure, not to
  the banner.

- note_add:

  Sentences to add to the note: a character vector of one or more
  strings, each printed as typed (no period is added) and joined to the
  note by a space. They close its paragraph, after the definitions of
  the abbreviations and before the legend of the stars, in every output
  and in `attr(x, "note")`, and the sentences the table writes stay.
  With a string in `note`, they follow it, so a wrapper can add a house
  sentence to every table while one table replaces its note. An `NA` or
  an empty string is an error, and so is `note_add` with `note = FALSE`,
  which leaves no note to add to.

- output:

  Output type. `"default"` (a printable `spicy_regression_table`);
  `"data.frame"` (the displayed table with its numbers); `"long"` (one
  row per coefficient, broom's names); `"tinytable"` / `"gt"` /
  `"flextable"` (rich-format tables); `"excel"` (writes to
  `excel_path`); `"clipboard"` (copies to system clipboard); `"word"`
  (writes flextable to `word_path`).

- excel_path:

  File path for `output = "excel"`, ending in `".xlsx"`, in a directory
  that exists. Default `NULL` (required when `output = "excel"`).

- excel_sheet:

  Sheet name when writing to Excel. `NULL` (the default) uses
  `"Regression"`.

- clipboard_delim:

  Field delimiter for `output = "clipboard"`. Default `"\t"`
  (tab-separated, pastes cleanly into Excel / Google Sheets / Word). The
  clipboard payload mirrors the Excel layout (title row, spanner row,
  header, body, footer note) but is plain text – horizontal rules, cell
  merging, decimal alignment and the monospace font cannot be encoded in
  delimited text and are therefore absent from the paste. A cell holding
  the delimiter itself (a label with a comma under
  `clipboard_delim = ","`, or any number under `decimal_mark = ","`), a
  double quote or a line break is quoted RFC 4180-style, so the grid
  survives whatever delimiter you choose.

  Paste behavior by target:

  - **Excel / Google Sheets:** numerics are auto-detected and
    right-aligned; text cells stay left-aligned. (P-values such as
    `.005` get re-parsed as `0.005` by Excel's auto-format – to preserve
    the APA leading-zero-dropped display, prefer `output = "excel"`.)

  - **Word:** the paste is converted to a Word table; all cells start
    left-aligned. Apply a Table Style (Insert \> Table \> Design) for
    APA-style borders, and set right-alignment on numeric columns
    (Layout \> Align Right). For a self-contained Word file with borders
    and alignment pre-applied, use `output = "word"` instead.

- word_path:

  File path for `output = "word"`, ending in `".docx"`, in a directory
  that exists. Default `NULL` (required when `output = "word"`). The
  Word table inherits the flextable styling (Calibri font, APA borders,
  decimal-aligned numerics) and adds Word-specific features: an
  auto-numbered caption ("Table 1: ...", "Table 2: ...") via Word's
  `SEQ` field so multiple `table_regression()` calls in one document
  number consecutively; a re-printed header row on each page break; row
  split prevention so a single coefficient row never wraps across two
  pages; and an APA-styled note line (`*Note.*` italic prefix per APA
  Manual 7 §7.14).

  **R Markdown / Quarto:** for embedded use, prefer
  `output = "flextable"` (returns the flextable object that knits to
  docx/HTML/PDF natively). `output = "word"` writes a standalone .docx
  file, suited to scripted exports rather than chunk-level rendering.

- word_template:

  Optional path to a custom .docx file used as the template for
  `output = "word"`. The template's header, footer, page size, margins,
  and named styles ("Table Caption" in particular) are honored; the
  table is appended to the template body. Useful for institutional
  templates with pre-set headers ("APA Style", "Manuscript Submission
  Template", etc.). Default `NULL` (uses flextable's stock template).

  **Customizing the caption appearance:** the table caption is tagged
  with the Word named style `"Table Caption"`. The visual rendering
  (italic / bold / color / font) follows whatever that style is set to
  in the docx template. The stock Word template renders
  `"Table Caption"` in italic, the type of the APA table title (APA
  Manual 7 section 7.11, p. 201), where the APA puts the bold table
  number on its own line above it (section 7.10, p. 200). For a
  different appearance (Nature-style bold non-italic, APA-strict 2-line
  bold-number / italic-title, etc.), edit the `"Table Caption"` style in
  a docx template and pass it via
  `word_template = "your_template.docx"`. Style-based delegation keeps
  the rendered caption consistent with the surrounding document and lets
  editorial conventions (Nature, APA-strict, journal-specific) be
  applied without modifying the call site.

- style:

  A journal style: a theme name (`"jama"`, `"nejm"`, `"lancet"`,
  `"annals"`, `"apa"`, `"aer"`), a
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  object, or `NULL` (the default). A style only changes DEFAULTS – any
  argument you pass explicitly wins over it. Set
  `options(spicy.style = )` for document-wide scope. A theme covers
  numeric formatting, the word that opens the note and the abbreviations
  the note defines, not full editorial conformity;
  [`?spicy_style`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  lists the exact rules each one encodes and the official document they
  come from. An unknown name is an error.

## Value

A `spicy_regression_table` object (a `data.frame` subclass with classes
`c("spicy_regression_table", "spicy_table", "data.frame")`) when
`output = "default"`. The result carries rendering attributes (`title`,
`note`, `align`, `padding`) and provenance attributes (`outcome`,
`model_ids`) consumed by the print method and the broom methods. For
other `output` values, returns the format-specific object: `gt_tbl`,
`flextable`, `tinytable`; for `"data.frame"` the displayed table as a
`data.frame` (see
[`as.data.frame.spicy_regression_table()`](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_regression_table.md));
for `"long"` a `tbl_df` (`model_id`, `outcome`, `term`, `estimate_type`,
`estimate`, `std.error`, `conf.low`, `conf.high`, `statistic`, `df`,
`df.residual` (the second df of an F test, `NA` elsewhere), `p.value`,
`test_type`, `nobs`, `r.squared`, `adj.r.squared`, ..., and last
`partial_type`, the hypothesis of a partial row's test, `"II"` or
`"III"`, `NA` elsewhere); the file path, invisibly, for `"excel"` and
`"word"`; the table, invisibly, for `"clipboard"`. The two data frames
carry the note as printed in `attr(x, "note")`. With
`layout = "stacked"`, the stacked table of
[`table_stack()`](https://amaltawfik.github.io/spicy/reference/table_stack.md),
whose value is described there.

## Pages by model family

This page documents every argument. What is specific to one family of
models (its own arguments and the values the shared ones take for it) is
on the page of that family:

- [table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md):
  `lmer`, `glmer`, `glmmTMB`, `lme` and `gls` fits.

- [table_regression_survival](https://amaltawfik.github.io/spicy/reference/table_regression_survival.md):
  `coxph`, `survreg`, `cph` and `flexsurvreg` fits, the RMST and
  risk-difference columns.

- [table_regression_ordinal](https://amaltawfik.github.io/spicy/reference/table_regression_ordinal.md):
  `polr`, `clm` and `clmm` fits.

- [table_regression_categorical](https://amaltawfik.github.io/spicy/reference/table_regression_categorical.md):
  `multinom` and `mlogit` fits.

- [table_regression_counts](https://amaltawfik.github.io/spicy/reference/table_regression_counts.md):
  `zeroinfl` and `hurdle` fits, and the zero-inflation, hurdle and
  dispersion blocks of `glmmTMB`.

- [table_regression_survey](https://amaltawfik.github.io/spicy/reference/table_regression_survey.md):
  `svyglm`, `svyolr` and `svycoxph` fits.

- [table_regression_bayesian](https://amaltawfik.github.io/spicy/reference/table_regression_bayesian.md):
  `stanreg` and `brmsfit` fits.

- [table_regression_robust](https://amaltawfik.github.io/spicy/reference/table_regression_robust.md):
  `lm_robust`, `iv_robust`, `ivreg`, `tobit`, `rq` and `fixest` fits.

The other families (linear and generalized linear models, additive
models, beta regression and sample selection, and the `rms` fits) follow
the rules of this page. GEE fits are described in
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md),
which lists every supported class. The tokens of `show_columns` and
`show_fit_stats` are on
[table_regression_tokens](https://amaltawfik.github.io/spicy/reference/table_regression_tokens.md).

## Vocabulary tokens

`show_columns` and `show_fit_stats` take tokens that select what the
table shows and in what order. Every token, and the fit statistics of
each model class, are on
[table_regression_tokens](https://amaltawfik.github.io/spicy/reference/table_regression_tokens.md).

## R-squared of glm and categorical models

Outside least squares there is no variance to partition, and several
\\R^2\\ measures compete. They answer different questions and give
different numbers on the same fit, so the row label names the measure,
and a report should too (Long & Freese 2014, p. 126). A threshold quoted
for one measure, such as 0.2 to 0.4 for McFadden's, does not carry over
to another (Hemmert et al. 2018).

- `"pseudo_r2_tjur"`, Tjur's coefficient of discrimination: the mean
  predicted probability of the observations with the event minus that of
  the observations without it (Tjur 2009). Binomial models only.

- `"pseudo_r2_mcfadden"`, McFadden's \\R^2\\: \\1 - \log L / \log L_0\\,
  the proportional reduction of the log-likelihood from that of the
  intercept-only model, \\L_0\\ (McFadden 1974).

- `"pseudo_r2_nagelkerke"`, Nagelkerke's \\R^2\\: Cox and Snell's \\1 -
  (L_0 / L)^{2/n}\\ divided by its largest possible value, \\1 -
  L_0^{2/n}\\ (Nagelkerke 1991). It is the max-rescaled R-square of SAS
  and the Nagelkerke R square of SPSS.

- `"r2_deviance"`, the deviance \\R^2\\: \\1 - D / D_0\\, the
  proportional reduction of the deviance from that of the intercept-only
  model (Cameron & Windmeijer 1997). It lies between 0 and 1, and within
  a family it cannot decrease when a predictor is added. For a gaussian
  [`glm()`](https://rdrr.io/r/stats/glm.html) with the identity link it
  is the \\R^2\\ of [`lm()`](https://rdrr.io/r/stats/lm.html). For
  [`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html) and `bam` fits of
  the same families (and of mgcv's `nb()` and `tw()`), it is the
  deviance explained of their
  [`summary()`](https://rdrr.io/r/base/summary.html), as a proportion.
  Two exceptions follow the [`glm()`](https://rdrr.io/r/stats/glm.html):
  a quasi-binomial GAM is measured on its trials, and the null model of
  a `bam()` keeps its offset, which the null deviance of its
  [`summary()`](https://rdrr.io/r/base/summary.html) leaves out.

A GAM ([`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html) or `bam`)
shows the \\R^2\\ rows of the
[`glm()`](https://rdrr.io/r/stats/glm.html) of its family and link,
computed by the same rules and with the same defaults: Tjur's then
McFadden's for a logit on a binary outcome, McFadden's for the other
binomial, Poisson and `nb()` GAMs, the deviance \\R^2\\ for the
continuous families. The likelihood is that of the fitted GAM, smooth
terms included, and a GAM without smooth terms gives the numbers of its
[`glm()`](https://rdrr.io/r/stats/glm.html). For `nb()`, the
intercept-only model re-estimates theta, as for
[`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html). Under
mgcv's default REML, the theta of an `nb()` GAM is not the
maximum-likelihood one of `glm.nb()`: `method = "ML"` gives its numbers.
mgcv's other extended families (`betar()`, `ocat()`, `scat()`, `ziP()`,
`cox.ph()` and the like) have no such row. mgcv reports only an adjusted
\\R^2\\: it is the `"adj_r2"` row of a GAM, shown on request, in a table
of GAMs or beside an [`lm()`](https://rdrr.io/r/stats/lm.html). The
`"r2"` row of a GAM is dashed beside other models, and refused for a
table of GAMs.

The smooth terms of a GAM form a `Smooth terms (edf)` block below its
parametric coefficients, one row per term, labeled as `summary.gam()`
labels it (`s(age)`, `te(bmi,ped)`, `s(age):sexF`). The B column carries
the effective degrees of freedom and the p column the p-value of mgcv's
approximate test of the term. The SE, CI and statistic cells are dashed:
an edf has no standard error, and the chi-square or F of the test has no
place under a t or z header. `exponentiate` leaves the edf as it is, and
no star marks it. Under a robust `vcov` the p-value is dashed too, since
mgcv's test rests on its own covariance matrix, and the note says so.
`tidy()` and `output = "long"` carry the rows with
`estimate_type = "edf"`, the reference df in `df` and the test in
`statistic` and `p.value`.

The intercept-only model keeps the offset and the weights of the fitted
model. For a
[`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) fit it
re-estimates theta in McFadden's and Nagelkerke's \\R^2\\, as Stata's
`nbreg` does, and keeps the theta of the fitted model in the deviance
\\R^2\\ (Cameron & Windmeijer 1997). By default each model shows the
rows of its family:

- A logit on a binary outcome (0/1, a factor or a logical), a
  [`glm()`](https://rdrr.io/r/stats/glm.html) or
  [`rms::Glm()`](https://rdrr.io/pkg/rms/man/Glm.html), or a binary
  [`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html): Tjur's \\R^2\\,
  then McFadden's. Tjur's reads on the scale of the predicted
  probabilities; McFadden's cannot decrease when a predictor is added.
  An `lrm` fitted without `y = TRUE` reads its outcome from the data, as
  a [`glm()`](https://rdrr.io/r/stats/glm.html) does; when the data are
  gone, its Tjur's cell is a dash and the row leaves its default.

- The other binomial models (probit, cloglog, a grouped response),
  Poisson and negative binomial models, `polr()`, `clm()`, `multinom()`
  and an ordinal [`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html):
  McFadden's \\R^2\\, which depends less than Cox and Snell's or
  Nagelkerke's on the proportion of events (Menard 2000) and is the
  Pseudo R2 of Stata's estimation output (Long & Freese 2014, p. 126).

- The continuous families of [`glm()`](https://rdrr.io/r/stats/glm.html)
  (gaussian, Gamma, inverse Gaussian, Tweedie): the deviance \\R^2\\.
  McFadden's and Nagelkerke's are refused for them: the log-likelihood
  of a density changes with the unit of the outcome, so the same model
  gets other values, even negative ones, when the outcome is rescaled.
  For a continuous outcome, the likelihood-ratio index can take large
  values, positive or negative (Cameron & Windmeijer 1997, p. 333).

- The quasi families have no likelihood and no default row.

A table of several models shows the union of their rows. A row is filled
for every model that defines the measure, and dashed for the others.
Nagelkerke's \\R^2\\ is shown on request only: its rescaling is ad hoc
and its values run high (Allison 2014). The deviance \\R^2\\ is
available on request for Poisson, negative binomial and quasi models,
where McFadden's is lower (Cameron & Windmeijer 1997). For a
[`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html) fit both
deviances take the theta of the fitted model. It is refused for
binomial, ordinal and multinomial models, where it equals McFadden's.

A binomial model is measured on its individual trials, whatever the
layout of its data: a `cbind(successes, failures)` response, a
proportion with the trials as `weights`, and 0/1 rows weighted by counts
give the McFadden, Nagelkerke and Tjur \\R^2\\ of the same model fitted
to the expanded 0/1 rows. The likelihood is that of the trials, n is
their number, and Tjur's means are weighted by them. For every family,
the n of Nagelkerke's formula is the sum of the weights. Weights that
are counts thus give the values of the expanded data, and non-integer
weights give those of the weighted log-likelihood
[`glm()`](https://rdrr.io/r/stats/glm.html) maximizes, which do not
change when the weights are rescaled. The `n` row still counts rows.

Tjur's \\R^2\\ depends more than McFadden's on the proportion of events:
in a simulated logit with fixed slopes, taking that proportion from 0.50
to 0.05 moves Tjur's from 0.21 to 0.07 and McFadden's from 0.16 to 0.13.
It can decrease when a predictor is added (Tjur 2009, p. 370; Allison
2014). It is at least 0 for a logit with an intercept, but a fixed
offset can make it negative, and it is shown with its sign. On request
it is also computed for the other links, as the same difference of
means, although the mean predicted probability equals the proportion of
events for a logit with an intercept (Tjur 2009, p. 366), and not in
general under the other links.

## Multi-model semantics

Pass a single fit or a [`list()`](https://rdrr.io/r/base/list.html) of
fits. Multi-model layout draws a centered **spanner label** above each
model's sub-columns:

- `list("Naive" = m1, "Adjusted" = m2)` -\> spanner labels `"Naive"` /
  `"Adjusted"`. Partial naming (`list("Naive" = m1, m2)`) auto-fills
  missing slots as `"Model <position>"`.

- `list(m1, m2)` (unnamed) -\> if all response variables differ, each
  outcome becomes the spanner label of its model and the redundant
  Outcome body row is suppressed. The outcome is named as in the title:
  its name (from `formula(fit)[[2]]`), or its `label` attribute under
  `labels = TRUE`. If DVs match, the labels default to
  `"Model 1, 2, ..."`.

- Models with different outcomes that the spanner labels do not name
  (named models, or an outcome that several models share) get an Outcome
  body row under the headers; `outcome_labels = FALSE` removes it.

- `model_labels = c("A", "B")` overrides everything.

A default label is worded in the language of the table
(`options(spicy.language)`), over the columns and in the notes that cite
the model; the column names of
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
keep the English one (`"Model 1: B"`).

Duplicate explicit names in the list are rejected
(`spicy_invalid_input`) – they would silently collide in the internal
model_id key. So is a name that collides with the auto-filled label of
another slot (`list("Model 2" = m1, m2)`): two models under one spanner
cannot be told apart in the table, nor addressed by
[`inline()`](https://amaltawfik.github.io/spicy/reference/inline.md).
Rename the model, or pass `model_labels`.

### Stacked layout

`layout = "stacked"` sets the models one under the other, one panel per
model, through
[`table_stack()`](https://amaltawfik.github.io/spicy/reference/table_stack.md).
Each panel is exactly the table `table_regression()` gives that model
alone, with the same arguments: its default columns (`"all_b"`, the
confidence interval included), its reference rows, blocks and fit
statistics. A row with the label of the model opens each panel:
`model_labels`, else the names of `models`, else the outcomes when they
all differ, else "Model k". The panels share one column header when they
have the same columns, and each opens with its own header otherwise (an
odds ratio beside a coefficient, a multinomial model with its outcome
categories in columns). Models with different outcomes that the labels
of the panels do not name open each panel on an Outcome row, under its
label row (`outcome_labels`), as the tables built alone do in
[`table_stack()`](https://amaltawfik.github.io/spicy/reference/table_stack.md).
`vcov` and `cluster` lists give each panel its own estimator. Under
`nested = TRUE`, the change statistics of each model sit in its own
panel, labeled "vs" the model before it. The type of each model, its
standard errors and the adjustment of its p-values are worded as in the
note of the models side by side, each panel named by its label; the
other sentences each open with the labels of the panels they concern. A
warning or an error names a model as the side-by-side table does.
`tidy()`, `glance()` and `output = "long"` return the rows of the models
side by side, under a leading `panel` column, with one exception: under
`keep` or `drop`, a multinomial model returns the rows its panel shows,
selected by its terms as in its table alone (`"age"`), where side by
side the patterns meet terms named with their outcome
(`"Student: age"`). `output = "data.frame"` binds the data frames of the
panels by rows. A `keep`, `drop` or `show_intercept = FALSE` that leaves
a panel without a coefficient row is an error: side by side, such a
model keeps its fit statistics beside the others.

## Inference and standard errors

`vcov` selects the variance-covariance estimator:

- `"classical"` – OLS, or WLS for a weighted fit (lm) / Fisher
  information (glm).

- `"HC0"` to `"HC5"` – heteroskedasticity-consistent (via
  [`sandwich::vcovHC()`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html)).

- `"CR0"` to `"CR3"` – cluster-robust with Satterthwaite-corrected df
  (via
  [`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)).
  Requires `cluster`.

- `"bootstrap"` – nonparametric or cluster bootstrap (`boot_n`
  replicates).

- `"jackknife"` – leave-one-out / leave-one-cluster-out.

For multi-model use, both `vcov` and `cluster` accept a single value
(recycled to all models) or a list (one per model). The same fit can
appear several times with different estimators to compare standard
errors side-by-side.

The note names the estimator of each model under the name every class
shares, "classical" followed by what it rests on ("classical (OLS)",
"classical (Fisher information)" for every fit by maximum likelihood,
"classical (model-based)" for `lmer`, `lme` and `gls`),
"heteroskedasticity-robust (HC3)" or "cluster-robust (CR2), clusters by
id". The test is named once, by a sentence of its own, where it is not
read off the class: "P-values: Satterthwaite t-test, cluster-robust df."
under `"CR0"` to `"CR3"`. In a table of several models, a sentence that
holds for some of them only opens with their names ("Model 1: P-values:
..."). An [`rms::Glm`](https://rdrr.io/pkg/rms/man/Glm.html) with an
estimated dispersion keeps the z test rms gives it, where its `glm` twin
is tested on t.

Inferential regimes for `lm` / `glm` (B and AME share the same regime):

- `classical`, `HC*` -\> t with `df.residual` (`lm`). For `glm`, the
  test [`summary()`](https://rdrr.io/r/base/summary.html) of the model
  reports: t on the residual degrees of freedom when the dispersion is
  estimated (gaussian, Gamma, inverse Gaussian, Tweedie,
  `negative.binomial(theta)` and the quasi families), z otherwise
  (binomial, Poisson,
  [`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html)).

- `bootstrap`, `jackknife` -\> z asymptotic.

- `CR0`-`CR3` -\> t with **Satterthwaite-corrected df** (B via
  [`clubSandwich::coef_test()`](http://jepusto.github.io/clubSandwich/reference/coef_test.md);
  Pustejovsky & Tipton 2018). The AME of an `lm` is a linear contrast of
  the coefficients whatever the formula
  ([`poly()`](https://rdrr.io/r/stats/poly.html),
  [`I()`](https://rdrr.io/r/base/AsIs.html),
  [`log()`](https://rdrr.io/r/base/Log.html),
  [`splines::ns()`](https://rdrr.io/r/splines/ns.html) and interactions
  included), tested by
  [`clubSandwich::linear_contrast()`](http://jepusto.github.io/clubSandwich/reference/linear_contrast.md)
  with its own Satterthwaite df. The AME of a `glm` is computed by
  marginaleffects under the same variance, and each row takes the
  Satterthwaite df of the coefficient of the same name (a predictor
  entered as itself, a factor level). A row without such a coefficient
  (a transformed predictor, `poly(x, 2)`) keeps the test of the `glm`: t
  on the residual df when the dispersion is estimated, z otherwise. The
  df differ from one coefficient to the next: `show_columns = "df"`
  prints them in a column.

For other classes the t-vs-z axis follows the estimator's native
reference distribution (e.g. `glmmTMB`, survival, ordinal, `betareg`,
`mlogit` use z; `lm`, `lme`, `lmer`,
[`rms::ols`](https://rdrr.io/pkg/rms/man/ols.html) use t).

A single [`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html)
model sets its outcome categories as column groups; see
[table_regression_categorical](https://amaltawfik.github.io/spicy/reference/table_regression_categorical.md).
A single sample-selection model
([`sampleSelection::selection()`](https://rdrr.io/pkg/sampleSelection/man/selection.html),
`heckit()`, `treatReg()`) sets its two equations as column groups,
"Outcome" then "Selection", in the language of the table, with its
`Error terms`, n and AIC under the first; beside other models, or under
`nested = TRUE`, each term has a `selection` and an `outcome` row; see
[table_regression_models](https://amaltawfik.github.io/spicy/reference/table_regression_models.md).

### Robust SE availability by model class

Not every estimator is defined for every class. A robust `vcov` the
class cannot honor is refused with `spicy_unsupported_vcov`, instead of
a model-based result under a robust label. The values each class
accepts, the `vcov` column of
[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md):

|  |  |  |
|----|----|----|
| Family | Class | vcov |
| Linear and generalized linear | `lm` | classical, HC0-HC5, CR0-CR3, CR1S, bootstrap, jackknife |
| Linear and generalized linear | `glm` | classical, HC0-HC5, CR0-CR3, bootstrap, jackknife |
| Linear and generalized linear | `negbin` | classical, HC0-HC5, CR0-CR3, bootstrap, jackknife |
| Linear and generalized linear | `rlm` | classical |
| Linear and generalized linear | `nls` | classical |
| Robust, IV, quantile, panel | `lm_robust` | classical (the fit's own) |
| Robust, IV, quantile, panel | `iv_robust` | classical (the fit's own) |
| Robust, IV, quantile, panel | `ivreg` | classical, HC0, HC1, CR0-CR3, CR1S |
| Robust, IV, quantile, panel | `tobit` | classical, CR1 |
| Robust, IV, quantile, panel | `rq` | classical (= nid), nid, iid, ker, rank, bootstrap |
| Robust, IV, quantile, panel | `fixest` | classical (the fit's own) |
| Mixed effects | `lmerMod` | classical, CR0-CR3 |
| Mixed effects | `glmerMod` | classical |
| Mixed effects | `glmmTMB` | classical, CR0, CR1 (classical only for REML or the ordinal family) |
| Mixed effects | `lme` | classical, CR0-CR3 (classical only with contrasts other than the session's) |
| Mixed effects | `gls` | classical |
| Ordinal | `polr` | classical, CR1 |
| Ordinal | `clm` | classical, CR1 (classical only with scale or nominal terms) |
| Ordinal | `clmm` | classical |
| Categorical | `multinom` | classical, CR1 |
| Categorical | `mlogit` | classical, CR1 |
| Counts, two-part | `zeroinfl` | classical, CR1 (classical only for a negative-binomial part) |
| Counts, two-part | `hurdle` | classical, CR1 (classical only for a negative-binomial part) |
| Survival | `coxph` | classical, CR0 |
| Survival | `survreg` | classical, CR1 |
| Survival | `cph` | classical, CR0 (classical only without x = TRUE, y = TRUE) |
| Survival | `flexsurvreg` | classical |
| Survey-weighted | `svyglm` | classical (design-based) |
| Survey-weighted | `svyolr` | classical (design-based) |
| Survey-weighted | `svycoxph` | classical (design-based) |
| Population-averaged (GEE) | `geeglm` | classical (the fit's own) |
| Additive, proportions, selection | `gam` | classical, CR1 (classical only for an mgcv extended family other than nb) |
| Additive, proportions, selection | `betareg` | classical, CR1 |
| Additive, proportions, selection | `selection` | classical |
| rms | `ols` | classical, CR0 (classical only without x = TRUE, y = TRUE) |
| rms | `lrm` | classical, CR0 (classical only without x = TRUE, y = TRUE) |
| rms | `Glm` | classical, CR0 (classical only without x = TRUE, y = TRUE) |
| Bayesian | `stanreg` | classical (the posterior) |
| Bayesian | `brmsfit` | classical (the posterior) |

`"classical"` is the model-based variance, or the variance the fit
carries for the classes marked so: `estimatr` and `fixest` fits keep the
estimator they were computed with, a GEE fit its own sandwich, a survey
fit its design-based variance, a Bayesian fit its posterior. Why the
other tokens are refused, and the backend of each class, is on the page
of its family (see *Pages by model family*); for `lm`, `glm`, GEE,
`gam`, `betareg` and the `rms` fits, it is below.

`lm` also takes `"CR1S"`, the full Stata `regress, vce(cluster)`
convention: the CR1S small-sample scaling (equal to
[`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html)
with `type = "HC1"`) with `t(G - 1)` inference, `G` the number of
clusters. Use it to reproduce Stata tables; `"CR2"` (Bell-McCaffrey,
Satterthwaite df) remains the recommended modern choice.
[`AER::ivreg()`](https://rdrr.io/pkg/AER/man/ivreg.html) takes it too
(see
[table_regression_robust](https://amaltawfik.github.io/spicy/reference/table_regression_robust.md)).
`"CR1S"` is refused for `glm` with an explanation: Stata's ML commands
use a different convention (`G/(G-1)` scaling, z), so the label would
not match Stata output there. For
[`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html), the sandwich
holds theta at its estimate, so robust standard errors can differ by a
few percent from software that includes the score of theta.

A GEE fit
([`geepack::geeglm`](https://rdrr.io/pkg/geepack/man/geeglm.html)) is
robust by construction: the fit's own sandwich (or jackknife) standard
errors, chosen by geeglm's `std.err =` option and clustered on its
`id =`, are the displayed inference. `HC*`, `CR*` and `cluster` are
refused with a pointer to those fit options.

Cluster-robust backends differ by class but are each cross-validated to
the field-standard oracle: `lm`/`glm`/`lmer`/`lme`/`ivreg` use
clubSandwich (CR2 = Bell-McCaffrey, with Satterthwaite df for
`lm`/`lme`/`lmer`/`ivreg`); `coxph`/`cph` use the Lin-Wei grouped-dfbeta
sandwich (identical to `coxph(..., cluster=)`);
`survreg`/`tobit`/`gam`/`polr`/`clm`/`betareg`/`mlogit`/`multinom` and
the Poisson and geometric `pscl` two-part fits use
[`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html).
For `gam`/`bam`, spicy forms the sandwich itself, from the GLM score and
the penalized covariance of the fit, and lets `vcovCL()` do only the
clustering arithmetic: the sandwich methods for these fits are wrong for
a free dispersion, a non-canonical link and a non-Gaussian `bam()`.
Extended families other than `nb()` (`betar`, `ocat`, `scat`, `tw`, ...)
have another score and are refused. For all of these `vcovCL()` classes,
the one estimator is CR1, requested as `"CR1"`; the other `CR*` tokens
are refused rather than mapped. `rms` fits use
[`rms::robcov()`](https://rdrr.io/pkg/rms/man/robcov.html) (which needs
the fit's `x = TRUE, y = TRUE`). The Lin-Wei and `robcov()` sandwiches
have no small-sample factor: their one estimator is requested as
`"CR0"`, and the other `CR*` tokens are refused rather than mapped. A
fit already passed through `robcov()` keeps its variance, which the note
names; for an `ols` it says "sandwich" rather than "Huber", because the
fit does not record whether `robcov()` used Huber's or Efron's method.
`glmmTMB` fits use
[`glmmTMB::sandwich()`](https://rdrr.io/pkg/glmmTMB/man/sandwich.html)
(see
[table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md)).
`cluster` length is one entry per observation, except `mlogit` (one per
choice situation) and censored `coxph` (one per subject).

### How to specify `cluster`

Three accepted forms, in order of preference:

1.  **Formula** – `cluster = ~region` (or `cluster = ~region:year` for
    the interaction of two variables). The variables are looked up in
    `model.frame(fit)` first, then in the original `data` argument
    captured by the fit. **Recommended**: independent of the dataset's
    name, composable for multi-way clustering, consistent with
    [`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html)
    /
    [`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md).

2.  **String** – `cluster = "region"`. A single column name resolved the
    same way as the formula. Convenient but cannot express interactions.

3.  **Vector** – `cluster = df$region`. An atomic vector of length
    `nobs(fit)`. Use this when the cluster key is **derived on the fly**
    (`cluster = interaction(df$region, df$year)`,
    `cluster = as.integer(format(df$date, "%Y"))`), comes from a
    **different dataset** with matching row order, or is otherwise not a
    column of the model's `data`.

A bare unquoted name (`cluster = region`) is **not** column selection:
it is evaluated as an ordinary R variable. If an object `region` exists
in the calling environment, its value is used as the **vector** form
above; if it does not (the typical "unquoted column name" intent), the
call fails with a migration error pointing at `~region` / `"region"`.
Bare-name column selection is deliberately unsupported – it would
require non-standard evaluation magic that breaks under programmatic use
(function wrapping, dynamic column choice, loops).

For multi-model use, mix forms freely:
`cluster = list(~region, "region", df$region)`.

## Hierarchical (nested) model comparison

`nested = TRUE` adds **per-pair change statistics as in-table rows**
(APA Manual 7 Table 7.15, p. 219 / Stata `esttab` / SPSS Model Summary
convention). Each adjacent pair (M2 vs M1, M3 vs M2, ...) contributes
one column of change stats; the FIRST model column gets en-dashes (no
previous model to compare to). Validation requires identical `nobs` and
identical response variable across all models.

**Structural refusals.** Four hierarchies carry no valid change
statistic whatever their `nobs` and response, and are refused with
`spicy_invalid_input` rather than rendered with empty or meaningless
cells:

- *A REML pair whose fixed effects differ*
  (`nlme::lme(method = "REML")`, `glmmTMB(REML = TRUE)`); see
  [table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).

- *Two `fixest` fits that absorbed different fixed effects*; see
  [table_regression_robust](https://amaltawfik.github.io/spicy/reference/table_regression_robust.md).

- *Mixed-effects fits from different engines* (`lme4`, `glmmTMB`,
  [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html)); see
  [table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).

- *A [`MASS::rlm`](https://rdrr.io/pkg/MASS/man/rlm.html) hierarchy*.
  M-estimation minimizes a bounded loss on the scaled residuals: it
  partitions no sums of squares and it is not a likelihood, so there is
  neither a likelihood-ratio test nor a partial F to report. Every
  change token is refused – including on a single fit or a
  `nested = FALSE` pair, where the token is the explicit request.
  Compare the least-squares fits
  ([`lm()`](https://rdrr.io/r/stats/lm.html)) if the hierarchy itself is
  the question.

In the first three, `nested = FALSE` renders the models side by side
instead. A hierarchy holding an
[`rms::ols()`](https://rdrr.io/pkg/rms/man/ols.html) or
[`rms::lrm()`](https://rdrr.io/pkg/rms/man/lrm.html) fit estimated with
`penalty =` is rendered side by side, without change statistics and with
a warning: neither the partial F nor the likelihood-ratio test is valid
for penalized estimates.

Default change tokens auto-injected when `show_fit_stats` is `NULL`:

- All-lm ([`rms::ols`](https://rdrr.io/pkg/rms/man/ols.html) and `nls`
  included): `c("r2_change", "f_change", "p_change")` – APA hierarchical
  regression standard.

- All-glm: `c("lrt_change", "p_change")` – Hosmer, Lemeshow & Sturdivant
  2013 Section 2.4; Long & Freese 2014 Section 3.2.4.

- All-glm whose dispersion is estimated (gaussian, Gamma, inverse
  Gaussian, Tweedie, quasi families), and the `feglm` fits of these
  families: `c("f_change", "p_change")`, the F test `anova(test = "F")`
  reports, \\F = (\Delta D / q) / \hat\phi\\ on \\(q, df)\\, with
  \\\hat\phi\\ the dispersion and \\df\\ the residual degrees of freedom
  of the larger model. `"lrt_change"` is refused for such a hierarchy.

- Any other hierarchy that carries a likelihood (`survreg`, `polr`,
  `clm`, `coxph`, `gls`, `betareg`, `multinom`, `fixest`, ...): the same
  `c("lrt_change", "p_change")`.

- Mixed-effects: see
  [table_regression_mixed](https://amaltawfik.github.io/spicy/reference/table_regression_mixed.md).

- Quantile (`rq`): `c("f_change", "p_change")` – the Wald-type test
  [`quantreg::anova.rq()`](https://rdrr.io/pkg/quantreg/man/anova.rq.html)
  reports.

To customize, pass the change tokens directly to `show_fit_stats`. The
variance-explained change tokens (`"r2_change"`, `"adj_r2_change"`,
`"f_change"`, `"f2_change"`) raise `spicy_invalid_input` on any
hierarchy whose nested comparison is a likelihood-ratio test – `glm`
(`"f_change"` excepted when its dispersion is estimated), and equally
every other likelihood class and the mixed-effects families: that
comparison reports a chi-square, not a variance-explained change, and
the message points at `"lrt_change"` + `"p_change"`. A quantile
hierarchy refuses them too, and `"lrt_change"` with them, pointing at
`"f_change"` + `"p_change"` instead. `lm`,
[`rms::ols`](https://rdrr.io/pkg/rms/man/ols.html) and `nls` keep the
least-squares tokens.

**The change test follows `vcov`.** Under the classical `vcov` the
change test is the partial F, the likelihood-ratio test, the F test
[`anova()`](https://rdrr.io/r/stats/anova.html) reports for a pair whose
dispersion is estimated, or
[`quantreg::anova.rq()`](https://rdrr.io/pkg/quantreg/man/anova.rq.html)'s
Wald test, as above. Under any other `vcov`, and under the classical
token when the fit carries a robust variance of its own, it is the Wald
test of the coefficients the current model adds, on the matrix its
coefficient rows were computed from: \\W = b' V^{-1} b\\, with \\b\\ the
\\q\\ added coefficients (aliased ones left out) and \\V\\ their block
of the matrix. The test takes the regime of the current model's
coefficient tests:

- *F*, in a row labeled "Wald F-change": \\W / q\\ on \\(q, df)\\
  degrees of freedom, the convention of `lmtest::waldtest(test = "F")`
  and
  [`car::linearHypothesis()`](https://rdrr.io/pkg/car/man/linearHypothesis.html).
  It applies to `lm` under `"HC0"`-`"HC5"` (and `"CR1S"`, on \\G - 1\\
  degrees of freedom), `glm` with an estimated dispersion under
  `"HC0"`-`"HC5"`, [`rms::ols`](https://rdrr.io/pkg/rms/man/ols.html)
  under `"CR0"` or after
  [`rms::robcov()`](https://rdrr.io/pkg/rms/man/robcov.html),
  [`quantreg::rq`](https://rdrr.io/pkg/quantreg/man/rq.html) under
  `"iid"` and `"ker"`, and a fixest fit estimated with a non-iid vcov
  whose coefficient tests are t (on fixest's own degrees of freedom, as
  [`fixest::wald()`](https://lrberge.github.io/fixest/reference/wald.html)):
  `feols`, and `feglm` with a family whose dispersion is estimated
  (gaussian, Gamma, inverse Gaussian and the quasi families). Under
  `"CR0"`-`"CR3"`, `lm`, `glm`
  ([`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html) included),
  [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) and
  [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) use the HTZ test
  of
  [`clubSandwich::Wald_test()`](http://jepusto.github.io/clubSandwich/reference/Wald_test.md),
  the small-sample correction of their coefficient tests.

- *Chi-square*, in a row labeled "Wald \\\chi^2\\ (change)" that
  replaces the likelihood-ratio row: \\W\\ on \\q\\ degrees of freedom,
  the convention of `lmtest::waldtest(test = "Chisq")`. It applies to
  `glm` with a fixed dispersion and `glm.nb` under `"HC0"`-`"HC5"`, `lm`
  and `glm` under `"bootstrap"` and `"jackknife"`, `rq` under
  `"bootstrap"`, `glmmTMB` under `"CR0"` and `"CR1"`, under `"CR0"` to
  `coxph` and [`rms::lrm`](https://rdrr.io/pkg/rms/man/lrm.html) / `cph`
  / `Glm`, and under `"CR1"` to `survreg`,
  [`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html) and `bam`,
  `polr`, `clm`, `betareg`, `multinom`, `mlogit`, `zeroinfl` and
  `hurdle` (not negative binomial); also to `survreg(robust = TRUE)`, a
  fixest fit with a non-iid vcov whose coefficient tests are z
  (`fepois`, `femlm`, `fenegbin`, and `feglm` with the binomial or
  Poisson family), and an `rms` fit of these classes passed through
  `robcov()`.

\\\Delta R^2\\, the information criteria and the deviance change do not
depend on the vcov and are unchanged, and a `vcov` list tests each pair
with the current model's estimator. The previous model's coefficients
must all appear, under the same names, in the current one, and a smooth
term added to a `gam` under a robust `vcov` is refused
(`spicy_invalid_input` in both cases). `rq` keeps `anova.rq()`'s test
under `"nid"`, its default, and under `"rank"`, which yields no matrix.
A Wald test of many constraints in a small sample is liberal: on 219
observations, a block of 19 predictors gave F(19, 178) = 6.91
classically and 9.06 as a Wald HC3 test. The HTZ correction under
`"CR0"`-`"CR3"` mitigates it.

## Standardized coefficients

`standardized` controls the method when `"beta"` is in `show_columns`:

- `"refit"` – refit on z-scored data. For `lm` both X and Y are z-scored
  (Cohen et al. 2003); for `glm` only numeric X (Long & Freese 2014
  Section 4.7.2 "x-standardization").

- `"posthoc"` – post-hoc scaling. lm: \\\beta = B \times SD(X) /
  SD(Y)\\; glm: X-only \\\beta = B \times SD(X)\\ (Y is undefined on the
  link scale).

- `"basic"` – like `"posthoc"` but factor dummies are scaled by their
  column SD.

- `"smart"` – Gelman (2008): continuous numeric inputs are scaled by
  `2 * SD(X)` (a +/-1 SD swing then spans the same range as a binary's 0
  to 1 step); binary inputs – numeric 0/1 and factor dummies – are left
  unscaled. (Before 0.13.0 the rule was applied inverted; see NEWS.)

- `"pseudo"` – *glm only*. Menard (2004, 2011) fully-standardized
  \\\beta = B \times SD(X) / SD(Y^\*)\\, with \\Y^\*\\ the latent
  variable on the link scale and \\SD(Y^\*) = \sqrt{Var(\hat{\eta}) +
  Var\_{link}}\\ (\\\pi^2/3\\ logit, `1` probit, \\\pi^2/6\\ cloglog).
  Binomial families only; non-binomial returns NA with a `spicy_caveat`.

- `"none"` (default) – no \\\beta\\ computed.

**Interactions and transforms.** Under `"refit"`, an interaction's
\\\beta\\ is the coefficient of the product of the z-scored components –
the recommended treatment for such models (Cohen et al. 2003 Section
7.5; Aiken & West 1991; Friedrich 1982). Under `"posthoc"`, `"basic"`,
and `"smart"`, the product / transformed design column is treated as a
single numeric column and scaled by its own SD (under `"smart"`, by
`2 * SD` when the product column is continuous; a binary product column
– e.g. a binary-by-binary interaction – stays unscaled like any binary
input) – the convention of SPSS beta, Stata `regress, beta`, SAS PROC
REG `STB`, and `lm.beta::lm.beta()`, identical to
`effectsize::standardize_parameters(method = "basic")` on those columns.
The two conventions differ whenever the components are correlated; a
`spicy_caveat` warns and the footer names the convention actually used.
`"refit"` declines formulas with inline transforms (`log(x)`,
[`poly()`](https://rdrr.io/r/stats/poly.html),
[`factor()`](https://rdrr.io/r/base/factor.html) written in the
formula): the model frame's evaluated columns cannot be re-evaluated on
z-scored data, so spicy falls back to `"posthoc"` with a warning
(`effectsize`'s refit instead standardizes *before* the transform – a
different estimand). Pre-build transformed columns in `data` for an
exact refit.

## Multiple-comparison adjustment

Adjusting the p-values of all coefficients of a single regression model
is **not** the standard convention. Each coefficient tests a distinct
hypothesis on a distinct predictor – not the situation multiple-testing
procedures were designed for (Rothman 1990; Greenland 2017; see also
Gelman, Hill & Yajima 2012). Hence the default `p_adjust = "none"`.

Adjustment is appropriate for: mass screening with no prior hypothesis
(typically `"BH"` / FDR), pre-registered multi-endpoint confirmatory
designs (typically `"holm"`), or when a journal / SAP explicitly
requests it.

The adjustment runs **before** any `keep` / `drop` filtering, so the
family is the model's full coefficient set (intercept and reference rows
excluded), not the displayed subset – filtering is a display choice and
must not change the inferential family.

## Output formats and broom integration

`output` selects the return type:

- `"default"` – a `spicy_regression_table` (`data.frame` subclass)
  printed via
  [`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md).

- `"data.frame"` – the displayed table with its numbers,
  [as.data.frame()](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_regression_table.md)
  of the default object: one row per printed row, the key columns
  `variable`, `label`, `level` and `.row_role`, then the printed
  columns, numeric, each interval split into its two bounds.

- `"long"` – the long-format tibble, one row per
  `(model_id, term, estimate_type)`, with broom's column names.

- `"tinytable"` / `"gt"` / `"flextable"` – rich-format HTML / Word / PDF
  tables (require the corresponding Suggests package). In a PDF, a
  `"flextable"` table wider than the page can be fitted with
  [`flextable::fit_to_width()`](https://davidgohel.github.io/flextable/reference/fit_to_width.html).

- `"excel"` – writes to `excel_path` via
  [`openxlsx2::wb_save()`](https://janmarvin.github.io/openxlsx2/reference/wb_save.html)
  and returns the path invisibly.

- `"word"` – writes to `word_path` via
  [`flextable::save_as_docx()`](https://davidgohel.github.io/flextable/reference/save_as_docx.html)
  and returns the path invisibly.

- `"clipboard"` – copies to the system clipboard via
  [`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md).

[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
returns a long tibble with one row per `(model_id, term, estimate_type)`
and broom-canonical column names (`estimate`, `std.error`, `conf.low`,
`conf.high`, `statistic`, `p.value`).
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
returns one row per model with the model-level statistics; `df.residual`
is kept numeric so cluster-robust Satterthwaite df is preserved.

## Global options

- **`options(spicy.style = )`** – the journal style applied by default
  to all nine table functions
  ([`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
  [`table_categorical_svy()`](https://amaltawfik.github.io/spicy/reference/table_categorical_svy.md),
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
  [`table_continuous_svy()`](https://amaltawfik.github.io/spicy/reference/table_continuous_svy.md),
  [`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md),
  [`table_outcome()`](https://amaltawfik.github.io/spicy/reference/table_outcome.md),
  [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md),
  `table_regression()` and
  [`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)).
  Takes a theme name (`"jama"`, `"nejm"`, `"lancet"`, `"annals"`,
  `"apa"`, `"aer"`) or a
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  object. The style of a report is a property of the document, so it is
  set once in the setup chunk; the `style` argument overrides it per
  call, and any formatting argument you type overrides both.
  `options(spicy.style = NULL)` restores spicy's own defaults. See
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  for the exact rules each theme encodes and the official document they
  come from.

- **`options(spicy.note_style = )`** – how the table note is rendered by
  the `"tinytable"` engine (every table function). A note is subordinate
  to the table it documents, so it is set one size down, in black:
  `0.9em` (`\small` in LaTeX), matching the `"gt"` engine and the
  10pt-on-11pt of the Word engine. When
  `tinytable::style_tt(fontsize = )` resizes the whole body, the note
  takes 0.9 of that size. Two opt-ins:

  - `"none"` – no intervention; the note is rendered exactly as the
    receiving document template styles it.

  - any other string – extra arguments for the Typst
    [`text()`](https://rdrr.io/r/graphics/text.html) call around the
    note, appended to the size (e.g.
    `options(spicy.note_style = "fill: luma(89)")` for a gray note).
    Typst only: the HTML note keeps its size.

  `options(spicy.note_style = NULL)` restores the default.

## Weights

No `weights` argument: weights are a property of the fit (extracted via
[`stats::weights()`](https://rdrr.io/r/stats/weights.html)). Pass them
when fitting: `lm(y ~ x, data = df, weights = w)`. All downstream
computations (vcov, AME, standardization, `weighted_nobs`) extract them
automatically.

## Internationalization

Output follows `options(spicy.language)` (see
[`spicy_labels()`](https://amaltawfik.github.io/spicy/reference/spicy_labels.md));
the column names of
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
stay English in every language. Override user-facing strings via
`reference_label`, `model_labels`, `outcome_labels`, and `labels`. The
title and footer are post-processable via `attr(result, "title")` and
`attr(result, "note")`.

## Classed conditions

Every error and warning emitted by `table_regression()` carries a
classed condition for programmatic dispatch via
[`tryCatch()`](https://rdrr.io/r/base/conditions.html) or
[`withCallingHandlers()`](https://rdrr.io/r/base/conditions.html).
Errors inherit from `spicy_error` (root); warnings from `spicy_warning`.
Specific leaves used by this function include `spicy_invalid_input`,
`spicy_invalid_data`, `spicy_unsupported`, `spicy_unsupported_vcov`,
`spicy_unsupported_standardized`, `spicy_missing_pkg`,
`spicy_missing_column`, `spicy_ignored_arg`, `spicy_caveat`,
`spicy_fallback`. See
[`spicy`](https://amaltawfik.github.io/spicy/reference/spicy-package.md)
for the full taxonomy.

## References

American Psychological Association (2020). *Publication manual of the
American Psychological Association* (7th ed.). American Psychological
Association.
[doi:10.1037/0000165-000](https://doi.org/10.1037/0000165-000)

Aiken, L.S. & West, S.G. (1991). *Multiple regression: Testing and
interpreting interactions*.

Allison, P.D. (2014). Measures of fit for logistic regression. *SAS
Global Forum 2014*, Paper 1485-2014. SAS Institute.

Cameron, A.C. & Windmeijer, F.A.G. (1997). An R-squared measure of
goodness of fit for some common nonlinear regression models. *Journal of
Econometrics*, 77(2), 329-342.
[doi:10.1016/S0304-4076(96)01818-0](https://doi.org/10.1016/S0304-4076%2896%2901818-0)

Cohen, J., Cohen, P., West, S.G., & Aiken, L.S. (2003). *Applied
multiple regression / correlation analysis for the behavioral sciences*
(3rd ed.). Lawrence Erlbaum.

Davison, A.C. & Hinkley, D.V. (1997). *Bootstrap methods and their
application*. Cambridge University Press.

Ferrari, S. & Cribari-Neto, F. (2004). Beta regression for modelling
rates and proportions. *Journal of Applied Statistics*, 31(7), 799-815.
[doi:10.1080/0266476042000214501](https://doi.org/10.1080/0266476042000214501)

Friedrich, R.J. (1982). In defense of multiplicative terms in multiple
regression equations. *American Journal of Political Science*, 26(4),
797-833.

Gelman, A. (2008). Scaling regression inputs by dividing by two standard
deviations. *Statistics in Medicine*, 27(15), 2865-2873.
[doi:10.1002/sim.3107](https://doi.org/10.1002/sim.3107)

Gelman, A., Goodrich, B., Gabry, J. & Vehtari, A. (2019). R-squared for
Bayesian regression models. *The American Statistician*, 73(3), 307-309.

Gelman, A., Hill, J. & Yajima, M. (2012). Why we (usually) don't have to
worry about multiple comparisons. *Journal of Research on Educational
Effectiveness*, 5(2), 189-211.
[doi:10.1080/19345747.2011.618213](https://doi.org/10.1080/19345747.2011.618213)

Gelman, A. & Pardoe, I. (2007). Average predictive comparisons for
models with nonlinearity, interactions, and variance components.
*Sociological Methodology*, 37(1), 23-51.
[doi:10.1111/j.1467-9531.2007.00181.x](https://doi.org/10.1111/j.1467-9531.2007.00181.x)

Greenland, S. (2017). For and against methodologies: Some perspectives
on recent causal and statistical inference debates. *European Journal of
Epidemiology*, 32(1), 3-20.
[doi:10.1007/s10654-017-0230-6](https://doi.org/10.1007/s10654-017-0230-6)

Hemmert, G.A.J., Schons, L.M., Wieseke, J., & Schimmelpfennig, H.
(2018). Log-likelihood-based pseudo-\\R^2\\ in logistic regression:
Deriving sample-sensitive benchmarks. *Sociological Methods & Research*,
47(3), 507-531.
[doi:10.1177/0049124116638107](https://doi.org/10.1177/0049124116638107)

Hosmer, D.W., Lemeshow, S. & Sturdivant, R.X. (2013). *Applied logistic
regression* (3rd ed.). Wiley.
[doi:10.1002/9781118548387](https://doi.org/10.1002/9781118548387)

Long, J.S. & Freese, J. (2014). *Regression models for categorical
dependent variables using Stata* (3rd ed.). Stata Press.

Lumley, T. & Scott, A. (2015). AIC and BIC for modeling with complex
survey data. *Journal of Survey Statistics and Methodology*, 3(1), 1-18.
[doi:10.1093/jssam/smu021](https://doi.org/10.1093/jssam/smu021)

McFadden, D. (1974). Conditional logit analysis of qualitative choice
behavior. In P. Zarembka (Ed.), *Frontiers in econometrics* (pp.
105-142). Academic Press.

Menard, S. (2000). Coefficients of determination for multiple logistic
regression analysis. *The American Statistician*, 54(1), 17-24.
[doi:10.1080/00031305.2000.10474502](https://doi.org/10.1080/00031305.2000.10474502)

Menard, S. (2004). Six approaches to calculating standardized logistic
regression coefficients. *The American Statistician*, 58(3), 218-223.
[doi:10.1198/000313004x946](https://doi.org/10.1198/000313004x946)

Menard, S. (2011). Standards for standardized logistic regression
coefficients. *Social Forces*, 89(4), 1409-1428.
[doi:10.1093/sf/89.4.1409](https://doi.org/10.1093/sf/89.4.1409)

Mize, T.D., Doan, L., & Long, J.S. (2019). A general framework for
comparing predictions and marginal effects across models. *Sociological
Methodology*, 49(1), 152-189.
[doi:10.1177/0081175019852763](https://doi.org/10.1177/0081175019852763)

Nagelkerke, N.J.D. (1991). A note on a general definition of the
coefficient of determination. *Biometrika*, 78(3), 691-692.
[doi:10.1093/biomet/78.3.691](https://doi.org/10.1093/biomet/78.3.691)

Pan, W. (2001). Akaike's information criterion in generalized estimating
equations. *Biometrics*, 57(1), 120-125.
[doi:10.1111/j.0006-341X.2001.00120.x](https://doi.org/10.1111/j.0006-341X.2001.00120.x)

Prentice, R.L. & Gloeckler, L.A. (1978). Regression analysis of grouped
survival data with application to breast cancer data. *Biometrics*,
34(1), 57. [doi:10.2307/2529588](https://doi.org/10.2307/2529588)

Pustejovsky, J.E. & Tipton, E. (2018). Small-sample methods for
cluster-robust variance estimation and hypothesis testing in fixed
effects models. *Journal of Business & Economic Statistics*, 36(4),
672-683.

Rothman, K.J. (1990). No adjustments are needed for multiple
comparisons. *Epidemiology*, 1(1), 43-46.
[doi:10.1097/00001648-199001000-00010](https://doi.org/10.1097/00001648-199001000-00010)

Tjur, T. (2009). Coefficients of determination in logistic regression
models – A new proposal: The coefficient of discrimination. *The
American Statistician*, 63(4), 366-372.
[doi:10.1198/tast.2009.08210](https://doi.org/10.1198/tast.2009.08210)

Vandenbroucke, J.P., von Elm, E., Altman, D.G., Gøtzsche, P.C., Mulrow,
C.D., Pocock, S.J., Poole, C., Schlesselman, J.J. & Egger, M. (2007).
Strengthening the reporting of observational studies in epidemiology
(STROBE): Explanation and elaboration. *PLoS Medicine*, 4(10), e297.
[doi:10.1371/journal.pmed.0040297](https://doi.org/10.1371/journal.pmed.0040297)

Vehtari, A., Gelman, A. & Gabry, J. (2017). Practical Bayesian model
evaluation using leave-one-out cross-validation and WAIC. *Statistics
and Computing*, 27(5), 1413-1432.
[doi:10.1007/s11222-016-9696-4](https://doi.org/10.1007/s11222-016-9696-4)

Venables, W.N. & Ripley, B.D. (2002). *Modern applied statistics with S*
(4th ed.). Springer.
[doi:10.1007/978-0-387-21706-2](https://doi.org/10.1007/978-0-387-21706-2)

## See also

[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
for the registry of supported model classes; the pages of the model
families listed under *Pages by model family*;
[table_regression_tokens](https://amaltawfik.github.io/spicy/reference/table_regression_tokens.md)
for the tokens of `show_columns` and `show_fit_stats`. If a class is not
listed there, try `table_regression(fit)` anyway – unsupported classes
error with a clear message. Other regression-table functions:
[`table_stack()`](https://amaltawfik.github.io/spicy/reference/table_stack.md)
to stack regression tables one under the other;
[`table_continuous_lm()`](https://amaltawfik.github.io/spicy/reference/table_continuous_lm.md)
for one-predictor-by-many-outcomes descriptive tables. Other spicy table
functions:
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md),
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md).
Underlying machinery:
[`spicy_print_table()`](https://amaltawfik.github.io/spicy/reference/spicy_print_table.md)
for ASCII rendering;
[`build_ascii_table()`](https://amaltawfik.github.io/spicy/reference/build_ascii_table.md)
for the low-level renderer. Inferential infrastructure (internal):
`compute_model_vcov()`, `compute_coef_inference()`,
`compute_wald_test()`. broom integration:
[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html).

## Examples

``` r
# ---- Single-model usage ------------------------------------------
fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)

# Default APA layout: B / SE / 95% CI / p plus the n / R^2 /
# Adj. R^2 fit-stats footer. Factor reference level is annotated
# with `(ref.)` and shows an en dash in the statistic columns.
table_regression(fit)
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20  1.66  [61.95, 68.45]  <.001 
#>  age             │    0.05  0.03  [-0.01,  0.11]   .130 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    3.86  0.91  [ 2.08,  5.63]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │     –     –          –          –    
#>    Yes           │   -1.72  1.11  [-3.89,  0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj. R²         │    0.02                              
#> 
#> Note. Linear regression. Std. errors: classical (OLS). CI = confidence interval.

# \donttest{
# Standardized coefficients (beta) injected next to B. "refit"
# is the Cohen et al. (2003) refit-on-z-scores convention;
# "basic" reproduces the SPSS / Stata regress, beta definition.
table_regression(fit, standardized = "refit")
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B       β     SE       95% CI        p   
#> ─────────────────┼─────────────────────────────────────────────
#>  (Intercept)     │   65.20  -0.10  1.66  [61.95, 68.45]  <.001 
#>  age             │    0.05   0.04  0.03  [-0.01,  0.11]   .130 
#>  sex:            │                                             
#>    Female (ref.) │     –      –     –          –          –    
#>    Male          │    3.86   0.25  0.91  [ 2.08,  5.63]  <.001 
#>  smoking:        │                                             
#>    No (ref.)     │     –      –     –          –          –    
#>    Yes           │   -1.72  -0.11  1.11  [-3.89,  0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                        
#>  R²              │    0.02                                     
#>  Adj. R²         │    0.02                                     
#> 
#> Note. Linear regression. Std. errors: classical (OLS). β = standardized coefficient ("refit": outcome and numeric predictors z-scored, factor dummies on 0/1); CI = confidence interval.
# }

# \donttest{
# Custom column set: B + AME + AME-specific p-value. Note that
# the `p` token always belongs to B, never to AME -- use the
# explicit `ame_p` token for AME inference.
if (requireNamespace("marginaleffects", quietly = TRUE)) {
  table_regression(
    fit,
    show_columns = c("b", "p", "ame", "ame_ci", "ame_p")
  )
}
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B       p     AME      95% CI        p   
#> ─────────────────┼─────────────────────────────────────────────
#>  (Intercept)     │   65.20  <.001                              
#>  age             │    0.05   .130   0.05  [-0.01, 0.11]   .130 
#>  sex:            │                                             
#>    Female (ref.) │     –     –       –          –         –    
#>    Male          │    3.86  <.001   3.86  [ 2.08, 5.63]  <.001 
#>  smoking:        │                                             
#>    No (ref.)     │     –     –       –          –         –    
#>    Yes           │   -1.72   .121  -1.72  [-3.89, 0.45]   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                        
#>  R²              │    0.02                                     
#>  Adj. R²         │    0.02                                     
#> 
#> Note. Linear regression. Std. errors: classical (OLS). AME of a continuous predictor: average slope (dY/dX). AME = average marginal effect; CI = confidence interval.
# }

# \donttest{
# Group-token shortcut: "all_b" + "all_ame" expands to the full
# B / AME column families side by side.
if (requireNamespace("marginaleffects", quietly = TRUE)) {
  table_regression(fit, show_columns = c("all_b", "all_ame"))
}
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p     AME    SE  
#> ─────────────────┼───────────────────────────────────────────────────
#>  (Intercept)     │   65.20  1.66  [61.95, 68.45]  <.001              
#>  age             │    0.05  0.03  [-0.01,  0.11]   .130   0.05  0.03 
#>  sex:            │                                                   
#>    Female (ref.) │     –     –          –          –       –     –   
#>    Male          │    3.86  0.91  [ 2.08,  5.63]  <.001   3.86  0.91 
#>  smoking:        │                                                   
#>    No (ref.)     │     –     –          –          –       –     –   
#>    Yes           │   -1.72  1.11  [-3.89,  0.45]   .121  -1.72  1.11 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                              
#>  R²              │    0.02                                           
#>  Adj. R²         │    0.02                                           
#> 
#>  Variable        │ 95% CI (AME)   p (AME) 
#> ─────────────────┼────────────────────────
#>  (Intercept)     │                        
#>  age             │ [-0.01, 0.11]     .130 
#>  sex:            │                        
#>    Female (ref.) │       –           –    
#>    Male          │ [ 2.08, 5.63]    <.001 
#>  smoking:        │                        
#>    No (ref.)     │       –           –    
#>    Yes           │ [-3.89, 0.45]     .121 
#> 
#> Note. Linear regression. Std. errors: classical (OLS). AME of a continuous predictor: average slope (dY/dX). CI = confidence interval; AME = average marginal effect.
# }

# ---- Cluster-robust variance -------------------------------------
# CR2 (Bell-McCaffrey) with Satterthwaite-corrected df is the
# recommended default under few clusters. Three forms are accepted
# for `cluster`; the formula is preferred for composability with
# multi-way clustering and for programmatic robustness.
# \donttest{
if (requireNamespace("clubSandwich", quietly = TRUE)) {
  table_regression(fit, vcov = "CR2", cluster = ~region)
}
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20  1.76  [60.63, 69.78]  <.001 
#>  age             │    0.05  0.04  [-0.05,  0.15]   .285 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    3.86  0.85  [ 1.66,  6.05]   .007 
#>  smoking:        │                                      
#>    No (ref.)     │     –     –          –          –    
#>    Yes           │   -1.72  1.52  [-5.70,  2.27]   .313 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj. R²         │    0.02                              
#> 
#> Note. Linear regression. Std. errors: cluster-robust (CR2), clusters by region. P-values: Satterthwaite t-test, cluster-robust df. CI = confidence interval.
if (requireNamespace("clubSandwich", quietly = TRUE)) {
  table_regression(fit, vcov = "CR2", cluster = "region")
}
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20  1.76  [60.63, 69.78]  <.001 
#>  age             │    0.05  0.04  [-0.05,  0.15]   .285 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    3.86  0.85  [ 1.66,  6.05]   .007 
#>  smoking:        │                                      
#>    No (ref.)     │     –     –          –          –    
#>    Yes           │   -1.72  1.52  [-5.70,  2.27]   .313 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj. R²         │    0.02                              
#> 
#> Note. Linear regression. Std. errors: cluster-robust (CR2), clusters by region. P-values: Satterthwaite t-test, cluster-robust df. CI = confidence interval.
if (requireNamespace("clubSandwich", quietly = TRUE)) {
  table_regression(fit, vcov = "CR2", cluster = ~region:age_group)
}
#> Linear regression: wellbeing_score
#> 
#>  Variable        │    B      SE       95% CI        p   
#> ─────────────────┼──────────────────────────────────────
#>  (Intercept)     │   65.20  1.54  [61.91, 68.49]  <.001 
#>  age             │    0.05  0.03  [-0.01,  0.10]   .107 
#>  sex:            │                                      
#>    Female (ref.) │     –     –          –          –    
#>    Male          │    3.86  0.87  [ 2.05,  5.67]  <.001 
#>  smoking:        │                                      
#>    No (ref.)     │     –     –          –          –    
#>    Yes           │   -1.72  1.25  [-4.31,  0.88]   .183 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                                 
#>  R²              │    0.02                              
#>  Adj. R²         │    0.02                              
#> 
#> Note. Linear regression. Std. errors: cluster-robust (CR2), clusters by region:age_group. P-values: Satterthwaite t-test, cluster-robust df. CI = confidence interval.
# }

# \donttest{
# ---- Hierarchical (nested) regression ----------------------------
# Adds in-table change-statistic rows (Delta R^2 / F-change /
# p-change for lm; LRT / p-change for glm) below the fit-stats.
# Note: hierarchical comparison requires identical observations
# across all models -- prepare a complete-case subset first so
# R's listwise deletion does not produce different `nobs` per
# model (which the function rejects).
sochealth_cc <- na.omit(
  sochealth[, c("wellbeing_score", "age", "sex", "smoking")]
)
m1 <- lm(wellbeing_score ~ age,                  data = sochealth_cc)
m2 <- lm(wellbeing_score ~ age + sex,            data = sochealth_cc)
m3 <- lm(wellbeing_score ~ age + sex + smoking,  data = sochealth_cc)
table_regression(
  list("Step 1" = m1, "Step 2" = m2, "Step 3" = m3),
  nested = TRUE
)
#> Hierarchical linear regression: wellbeing_score
#> 
#>                           Step 1                     Step 2             
#>                    ────────────────────  ────────────────────────────── 
#>  Variable        │    B      SE     p            B           SE     p   
#> ─────────────────┼──────────────────────────────────────────────────────
#>  (Intercept)     │   66.85  1.59  <.001    64.90            1.65  <.001 
#>  age             │    0.04  0.03   .160     0.05            0.03   .143 
#>  sex:            │                                                      
#>    Female (ref.) │                           –               –     –    
#>    Male          │                          3.87            0.91  <.001 
#>  smoking:        │                                                      
#>    No (ref.)     │                                                      
#>    Yes           │                                                      
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                  1175                           
#>  R²              │    0.00                  0.02                        
#>  Adj. R²         │    0.00                  0.02                        
#>  ΔR²             │     –                   +0.02                        
#>  F-change        │     –                   18.26 (1, 1172)              
#>  p (change)      │     –                    <.001                       
#> 
#>                                Step 3             
#>                    ────────────────────────────── 
#>  Variable        │         B           SE     p   
#> ─────────────────┼────────────────────────────────
#>  (Intercept)     │   65.20            1.66  <.001 
#>  age             │    0.05            0.03   .130 
#>  sex:            │                                
#>    Female (ref.) │     –               –     –    
#>    Male          │    3.86            0.91  <.001 
#>  smoking:        │                                
#>    No (ref.)     │     –               –     –    
#>    Yes           │   -1.72            1.11   .121 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                           
#>  R²              │    0.02                        
#>  Adj. R²         │    0.02                        
#>  ΔR²             │   +0.00                        
#>  F-change        │    2.41 (1, 1171)              
#>  p (change)      │     .121                       
#> 
#> Note. Linear regression models. Std. errors: classical (OLS).

# The same comparison by its fit statistics alone.
table_regression(
  list("Step 1" = m1, "Step 2" = m2, "Step 3" = m3),
  show_columns = "none",
  nested = TRUE
)
#> Hierarchical linear regression: wellbeing_score
#> 
#>  Variable   │ Step 1        Step 2             Step 3       
#> ────────────┼───────────────────────────────────────────────
#>  n          │ 1175     1175               1175              
#>  R²         │    0.00     0.02               0.02           
#>  Adj. R²    │    0.00     0.02               0.02           
#>  ΔR²        │     –      +0.02              +0.00           
#>  F-change   │     –      18.26 (1, 1172)     2.41 (1, 1171) 
#>  p (change) │     –       <.001               .121          
#> 
#> Note. Linear regression models.
# }

# \donttest{
# ---- Side-by-side variance comparison ----------------------------
# Same fit, three vcovs in one wide table. Useful for showing the
# sensitivity of inference to the variance assumption.
if (requireNamespace("clubSandwich", quietly = TRUE)) {
  table_regression(
    list("Classical" = fit, "HC3" = fit, "CR2" = fit),
    vcov    = list("classical", "HC3", "CR2"),
    cluster = list(NULL, NULL, ~region)
  )
}
#> Linear regression comparison: wellbeing_score
#> 
#>                         Classical                HC3                CR2      
#>                    ────────────────────  ────────────────────  ───────────── 
#>  Variable        │    B      SE     p       B      SE     p       B      SE  
#> ─────────────────┼───────────────────────────────────────────────────────────
#>  (Intercept)     │   65.20  1.66  <.001    65.20  1.61  <.001    65.20  1.76 
#>  age             │    0.05  0.03   .130     0.05  0.03   .127     0.05  0.04 
#>  sex:            │                                                           
#>    Female (ref.) │     –     –     –         –     –     –         –     –   
#>    Male          │    3.86  0.91  <.001     3.86  0.91  <.001     3.86  0.85 
#>  smoking:        │                                                           
#>    No (ref.)     │     –     –     –         –     –     –         –     –   
#>    Yes           │   -1.72  1.11   .121    -1.72  1.11   .123    -1.72  1.52 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                  1175                  1175          
#>  R²              │    0.02                  0.02                  0.02       
#>  Adj. R²         │    0.02                  0.02                  0.02       
#> 
#>                     CR2  
#>                    ───── 
#>  Variable        │ p (B) 
#> ─────────────────┼───────
#>  (Intercept)     │ <.001 
#>  age             │  .285 
#>  sex:            │       
#>    Female (ref.) │  –    
#>    Male          │  .007 
#>  smoking:        │       
#>    No (ref.)     │  –    
#>    Yes           │  .313 
#> 
#> Note. Linear regression models. Std. errors: Classical: classical (OLS); HC3: heteroskedasticity-robust (HC3); CR2: cluster-robust (CR2), clusters by region. CR2: P-values: Satterthwaite t-test, cluster-robust df.
# }

# ---- Tidy long format for downstream pipelines -------------------
if (requireNamespace("broom", quietly = TRUE)) {
  broom::tidy(table_regression(fit))
}
#> # A tibble: 4 × 18
#>   model_id outcome outcome_level term  estimate_type estimate std.error conf.low
#>   <chr>    <chr>   <chr>         <chr> <chr>            <dbl>     <dbl>    <dbl>
#> 1 M1       wellbe… NA            (Int… B              65.2       1.66    62.0   
#> 2 M1       wellbe… NA            age   B               0.0465    0.0307  -0.0137
#> 3 M1       wellbe… NA            sexM… B               3.86      0.905    2.08  
#> 4 M1       wellbe… NA            smok… B              -1.72      1.11    -3.89  
#> # ℹ 10 more variables: conf.high <dbl>, statistic <dbl>, df <dbl>,
#> #   df.residual <dbl>, p.value <dbl>, test_type <chr>, is_intercept <lgl>,
#> #   factor_term <chr>, factor_level <chr>, partial_type <chr>

if (FALSE) { # \dontrun{
# ---- Rich-format outputs (require optional Suggests packages) ----
table_regression(fit, output = "gt")
table_regression(fit, output = "flextable")
table_regression(fit, output = "tinytable")

# ---- File outputs ------------------------------------------------
table_regression(fit, output = "excel",
                 excel_path = tempfile(fileext = ".xlsx"))
table_regression(fit, output = "word",
                 word_path  = tempfile(fileext = ".docx"))

# ---- System clipboard (interactive use) --------------------------
table_regression(fit, output = "clipboard")
} # }
```
