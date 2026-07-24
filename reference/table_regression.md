# Regression coefficient summary table

Publication-ready coefficient table from one or more fitted `lm` / `glm`
models. Supports standardised coefficients (\\\beta\\), average marginal
effects (AME), partial effect sizes (*\\f^2\\* / *\\\eta^2\\* /
*\\\omega^2\\* for `lm`; partial *\\\chi^2\\* for `glm`), pseudo-\\R^2\\
(`glm`), and a full vocabulary of variance estimators (classical / HC\*
/ cluster-robust with Satterthwaite-corrected df / bootstrap /
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
  reference_label = "(ref.)",
  show_fit_stats = NULL,
  fit_stats_layout = c("first_col", "merged"),
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
  labels = NULL,
  title = NULL,
  note = NULL,
  output = c("default", "data.frame", "long", "gt", "flextable", "tinytable", "excel",
    "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = "Regression",
  clipboard_delim = "\t",
  word_path = NULL,
  word_template = NULL
)
```

## Arguments

- models:

  A fitted model object, or a list of such fits (named or unnamed;
  classes may be mixed). Single fits are auto-promoted to a 1-element
  list. A broad set of model classes is supported – linear / generalised
  linear (`lm`, `glm`,
  [`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html)),
  mixed-effects (`lmer`, `lme`, `glmmTMB`), survival (`coxph`,
  `survreg`), ordinal (`polr`, `clm`),
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
  own estimator family instead: `"classical"` resolves to the
  heteroskedasticity-robust `"nid"` sandwich (Hendricks-Koenker,
  Hall-Sheather bandwidth – quantreg's own large-sample default and the
  Stata `vce(robust)` analogue), with `"iid"` (Koenker-Bassett
  homoskedastic, Stata `vce(iid)` parity), `"ker"` (Powell kernel) and
  `"rank"` (rank-score inversion: genuine CIs, no SE / t / p – those
  cells render as dashes) as opt-ins, and `"bootstrap"` running
  quantreg's native `boot.rq` (`bsmethod = "xy"`; with `cluster =`, the
  Hagemann 2017 wild gradient cluster bootstrap – the one cluster-robust
  route for `rq`; `CR*` and `HC*` are refused for `rq`, as is
  `"jackknife"`, whose leave-one-out form is inconsistent for
  quantiles). See *Inference and standard errors*.

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
  *MASS* Section 7.2). Only the CI bounds change; estimate, SE,
  statistic and p-value remain Wald. For ordinal fits the profile covers
  the predictor coefficients (the cut-point thresholds stay Wald), and a
  robust `vcov` takes precedence (its Wald-robust CIs are used instead,
  with a consolidated warning). `"profile"` with `lm` raises
  `spicy_invalid_input`. `"boot_percentile"` (requires
  `vcov = "bootstrap"` for every model) replaces the coefficient CI
  bounds with equal-tailed percentile intervals of the bootstrap
  replicates (the
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
  the highest-density interval – the shortest interval containing
  `ci_level` of the posterior draws (Kruschke 2015), relabelling the
  column header `95% HDI`. Unlike the equal-tailed interval the HDI is
  not transformation-invariant, so under `exponentiate = TRUE` it is
  recomputed on the exponentiated draws rather than transformed.
  Requesting `"hdi"` for a frequentist fit, or `"profile"` /
  `"boot_percentile"` for a Bayesian fit, raises `spicy_invalid_input`.

- boot_n:

  Number of bootstrap replicates when `vcov = "bootstrap"`. Single
  positive integer. Default `1000L`.

- tau:

  RMST horizon: the `"rmst"` column family reports the
  restricted-mean-survival-time difference over `[0, tau]` (`coxph` and
  `survreg` fits). A positive time on the outcome's scale, or `"minmax"`
  for the smallest per-group maximum follow-up (the resolved value is
  disclosed in the table note). No default: the horizon defines the
  estimand. Stratified fits (`strata()`) are supported: standardization
  keeps each subject's own stratum baseline, disclosed in the table
  note.

- at_time:

  Landmark time for the `"risk_diff"` column family: the difference in
  cumulative incidence at `at_time` (`coxph` and `survreg` fits). A
  positive time on the outcome's scale; no default.

- standardized:

  Standardisation method for the `"beta"` column. One of `"none"`
  (default), `"refit"`, `"posthoc"`, `"basic"`, `"smart"`, `"pseudo"`.
  `"pseudo"` is *glm only* (Menard 2011 fully-standardised); using it
  with [`lm()`](https://rdrr.io/r/stats/lm.html) raises
  `spicy_invalid_input`. Supported classes: `lm`, `glm` (incl.
  [`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html)), the mixed
  engines (`lmer` / `glmer` / `glmmTMB` /
  [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html)), and
  fixed-effects `stan_glm`-style `stanreg` fits for the algebraic
  flavors `"posthoc"` / `"basic"` / `"smart"` – exact affine rescales of
  the posterior draws (median, MAD SD and credible bounds all scale by
  the same positive SD ratio; the beta stays on the link scale under
  `exponentiate = TRUE`). Standard-formula fixed-effects `brmsfit`
  models are supported too (their design matrix is recovered through
  insight); the scale factors are engine-invariant, so the same model
  fit with rstanarm or brms gets identical betas up to sampling noise.
  Bayesian `"refit"` (would re-run the sampler) and `"pseudo"` (per-draw
  latent variance; planned) are refused. Also refused: multilevel fits
  (`stan_glmer` / `stan_lmer` and `brm()` with group-level terms; a
  Bayesian beta would need an explicit sd(Y) decomposition that the
  frequentist mixed engines resolve by refitting on z-scored data),
  non-GLM `stanreg` subclasses (`stan_polr`, `stan_betareg`), and brms
  formulas with distributional, multivariate or special terms (`mo()`,
  `s()`, ...). In all refused Bayesian cases, standardize predictors
  before fitting instead. Any other class raises
  `spicy_unsupported_standardized` rather than rendering an empty beta
  column. See the *Standardised coefficients* section.

- exponentiate:

  Logical. When `TRUE`, `B`, the CI bounds, and the SE (delta method:
  `SE_OR = OR x SE_log-odds`) are
  [`exp()`](https://rdrr.io/r/base/Log.html)-transformed for links where
  the result is a ratio: logit (`OR`), log (`IRR` / `RR` / `MR` per
  family, generic `exp(B)` for other log-link families – a genuine ratio
  of means), and binomial / ordinal cloglog (`HR`; grouped-time
  proportional hazards, Prentice & Gloeckler 1978). The statistic and
  p-value stay on the link scale (invariant under monotone
  transformation). Identity-link fits are left untouched (a
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
  cannot be reconstructed as `estimate ± z × SE`; the delta-method SE
  follows the Stata convention (`[R] logistic`, Methods and formulas:
  `se(OR) = OR × se(b)`) and the log-scale CI endpoints carry the
  uncertainty more faithfully than this SE. The footer states the SE
  scale and the CI asymmetry whenever an SE column is displayed. Default
  `FALSE`.

- p_adjust:

  Multiple-comparison adjustment method applied to the family of
  estimated coefficient p-values within each model (intercept and
  reference rows excluded). One of `"none"` (default), `"holm"`,
  `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`, or `"BY"`.
  Delegated to
  [`stats::p.adjust()`](https://rdrr.io/r/stats/p.adjust.html); applied
  per-model and per `estimate_type` (B and AME p-values are adjusted
  independently within their own families). Active adjustments are
  documented in the footer (method + family size). See the
  *Multiple-comparison adjustment* section for when this is and is not
  appropriate.

- show_columns:

  Character vector of tokens selecting the per-coefficient columns and
  their display order. Accepts **atomic tokens** (`"b"`, `"se"`, `"ci"`,
  `"t"`, `"p"`, `"beta"`, `"n"`, `"n_events"`, `"pd"` (probability of
  direction, Bayesian fits only), `"rhat"` / `"ess_bulk"` / `"ess_tail"`
  / `"mcse"` (per-coefficient sampler diagnostics and the Monte Carlo
  standard error of the displayed posterior median, all-Bayesian tables
  only), `"ame"`, `"ame_se"`, `"ame_ci"`, `"ame_p"`, `"rmst"` +
  `"rmst_se"` / `"rmst_ci"` / `"rmst_p"`, `"risk_diff"` + its `_se` /
  `_ci` / `_p` companions, `"partial_f2"` + `"partial_f2_ci"`,
  `"partial_eta2"` + `"partial_eta2_ci"`, `"partial_omega2"` +
  `"partial_omega2_ci"`, `"partial_chi2"`) and **group tokens**
  (`"all_b"`, `"all_b_compact"`, `"all_b_full"`, `"all_beta"`,
  `"all_ame"`, `"all_ame_compact"`, `"all_f2"`, `"all_eta2"`,
  `"all_omega2"`). See *Vocabulary tokens* in the details for the full
  enumeration. Default `NULL` selects a context-aware layout: `"all_b"`
  (single model) or `"all_b_compact"` (multi-model, and the
  single-multinomial outcome-as-columns layout, which has the same width
  pressure – restore CIs with atomic tokens, e.g.
  `c("b", "se", "ci", "p")`). The `"p"` token is always the B / beta
  p-value; for the AME-specific p-value use `"ame_p"`.

- keep:

  Character vector of regexes. Only coefficient rows whose term name (as
  in [`stats::coef()`](https://rdrr.io/r/stats/coef.html) – e.g. `"wt"`,
  `"cyl6"`, `"factor(cyl)8"`) matches at least one pattern are kept.
  Mutually exclusive with `drop`. Filtering is a display choice;
  `p_adjust` runs against the full coefficient family before filtering.
  Default `NULL` (no filter).

- drop:

  Character vector of regexes. Coefficient rows matching any pattern are
  removed. Mutually exclusive with `keep`. Default `NULL`.

- show_intercept:

  Whether to display the intercept row. Default `TRUE` (APA convention).
  Hide via `FALSE`.

- show_thresholds:

  For ordinal cumulative-link models
  ([`MASS::polr`](https://rdrr.io/pkg/MASS/man/polr.html),
  [`ordinal::clm`](https://rdrr.io/pkg/ordinal/man/clm.html)), whether
  to display the estimated category thresholds (cut-points) as a
  subordinate `"Thresholds"` block of rows below the predictors,
  carrying B / SE / CI / p like the predictor rows. Default `TRUE`.
  `FALSE` collapses them to a compact one-line footer note instead.
  Thresholds are reported on the log-odds (B) scale and are **never
  exponentiated** (under `exponentiate = TRUE` their rows stay on the
  log-odds scale). Has no effect on non-ordinal models, and the rows are
  shown only when a coefficient column (`"b"`/`"beta"`) is in
  `show_columns`.

- show_components:

  For models with secondary components, whether to display them as
  labelled subordinate blocks of rows below the primary (count /
  conditional / location) coefficients. Default `TRUE`:

  - [`pscl::zeroinfl`](https://rdrr.io/pkg/pscl/man/zeroinfl.html) and
    `glmmTMB(ziformula = )`: a `Zero-inflation` block – the model for
    the probability of a **structural (excess) zero**.

  - [`pscl::hurdle`](https://rdrr.io/pkg/pscl/man/hurdle.html): a
    `Zero hurdle` block – the model for the probability of a **nonzero
    count** (note the opposite direction vs zero-inflation; the footer
    names each block's meaning).

  - `glmmTMB(dispformula = )`: a `Dispersion` block (only when
    dispersion was actually modelled; log scale, never exponentiated).

  - `ordinal::clm(scale = ~)`: a `Scale effects` block – the covariate
    effects on the **log standard deviation of the latent response**.
    Never exponentiated: their exponential is a ratio of latent standard
    deviations, not an odds ratio.

  Component rows carry full Wald inference (B / SE / z / p / CI), join
  the `p_adjust` family, and take significance stars. Under
  `exponentiate = TRUE` a component is exponentiated **only when its
  link makes the result an odds ratio** (the logit zero components);
  probit / cauchit / cloglog zero links and count-type hurdle zero parts
  stay on the link scale, disclosed in the footer. `FALSE` omits the
  blocks (the title still names the model type).

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

  - `"footer"`: the row is dropped and a single line
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
  mode. Default `"(ref.)"`. Ignored by the other three modes (which use
  structural English wording – "ref:", "vs", "Reference categories:").

- show_fit_stats:

  Character vector of tokens for the model-level rows below the
  coefficients; row order follows token order. `NULL` (default) resolves
  class-aware:

  - `lm`: `c("nobs", "r2", "adj_r2")`.

  - `glm`, ordinal `polr` / `clm`:
    `c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "aic")`
    (McFadden = Stata `ologit` default, Nagelkerke = SPSS PLUM).

  - mixed `lm` + `glm`: the union of the two (the renderer en-dashes per
    cell the stat not defined for a given model class).

  Under `nested = TRUE` the default is extended with the
  class-appropriate change-stat tokens (e.g. `"r2_change"`, `"f_change"`
  for `lm`). See *Vocabulary tokens* (`show_fit_stats` subsection) and
  *Hierarchical (nested) model comparison* in the details for the full
  vocabulary.

- fit_stats_layout:

  Layout of the fit-stat values (`n`, `R^2`, `AIC`, ...) within each
  model's column group. Two options:

  - `"first_col"` (default): the value is placed in the FIRST numeric
    sub-column of each model (typically `B`); the model's remaining
    sub-columns (`SE`, `LL`, `UL`, `p`, ...) are left empty for that
    row. The APA Manual 7 Table 7.13 layout.

  - `"merged"`: the model's numeric sub-columns are merged into a single
    wide cell containing the fit-stat value, centred under the model
    spanner. Stata `esttab` layout / *Econometrica* and *AER* journal
    convention. Resolves the mixed-precision look of `"first_col"` (an
    integer `n` row sharing the B column with two-decimal coefficients).

  Cell merging is supported by `excel`, `flextable`, and `word` (via
  flextable). `gt`, `tinytable`, `clipboard`, and `default` (console)
  always render in `"first_col"` mode regardless of this setting:

  - `gt` lacks a native row-spanning cell-merge API (`tab_spanner`
    covers columns, not row-cell ranges).

  - `tinytable`'s `style_tt(colspan = N)` emits HTML `colspan` only on
    header rows, not on body cells.

  - `clipboard` ships TSV plaintext.

  - `default` ships fixed-width ASCII.

  Decimal alignment of every numeric column is preserved in both modes:
  the `B` column decimal-aligns its coefficient values plus any fit-stat
  value(s) in `"first_col"` mode (native primitives handle the
  mixed-precision case), and trivially decimal-aligns in `"merged"` mode
  (the fit-stat values move out of the B column into the merged cell).

- show_re:

  Logical. `TRUE` (default) renders the random-effects variance
  components of a mixed-effects fit (`lmer`, `glmer`, `glmmTMB`, `lme`)
  as a subordinate **"Random effects" block of table rows** below the
  fixed effects: one row per standard deviation / correlation per
  grouping factor, plus the residual, each with its estimate, SE, and CI
  in the shared coefficient columns. The group sizes (`N (groups)`) and
  the ICC render as fit-statistic rows; the footer reports the
  estimation method (`REML` / `ML`) and the likelihood-ratio test of the
  whole random part against the no-random-effects model, with the
  boundary-corrected chi-bar-squared p-value (Self & Liang 1987; Stram &
  Lee 1994). Variance-component rows deliberately carry **no per-row
  p-value**: a Wald test of a variance is invalid at the boundary of the
  parameter space, and no reporting guideline requests one (see the
  *Mixed-effects models* section of
  [`vignette("table-regression")`](https://amaltawfik.github.io/spicy/articles/table-regression.md)).
  `FALSE` suppresses the block. No effect on fits without random effects
  (`lm`, `glm`, `coxph`, ...).

- re_scale:

  One of `"sd"` (default) or `"variance"`. Controls the display scale of
  the random-effects rows:

  - `"sd"`: report the random-effect standard deviation \\\sigma\\
    (Gelman 2005, *Technometrics*: "*directly interpretable as the size
    of the variation across groups*"). Standard error and CI converted
    via the Delta method: \\SE(\sigma) = SE(\sigma^2) / (2\sigma)\\;
    \\CI(\sigma) = \sqrt{CI(\sigma^2)}\\.

  - `"variance"`: report \\\sigma^2\\ (the canonical internal scale; SE
    and CI come straight from the Hessian /
    [`nlme::intervals()`](https://rdrr.io/pkg/nlme/man/intervals.html) /
    `glmmTMB::confint()` without rescaling).

  Correlation rows (\\\rho\\) are unitless and pass through either way.

- re_columns:

  Character vector. Subset of `c("est", "se", "ci")` controlling which
  cells of the random-effects rows are **displayed** (`"est"` is
  mandatory); deselected SE / CI cells render as an en-dash on those
  rows only. Useful for slimming output (`re_columns = "est"`) or for
  journals that want only standard errors
  (`re_columns = c("est", "se")`). Display-only: the underlying data
  ([`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html),
  [`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md))
  always carries the full SE + CI.

  Note. Under the default `re_ci = "wald"` the SE and CI are Wald
  (`est ± z * SE`, clamped at 0 for variances). Wald can be optimistic
  near the variance boundary (Self & Liang 1987 chi-bar-squared);
  request boundary-respecting profile-likelihood intervals with
  `re_ci = "profile"` when robustness is critical. See the
  *Mixed-effects models* section of
  [`vignette("table-regression")`](https://amaltawfik.github.io/spicy/articles/table-regression.md).

  For `lmer` / `glmer` fits these SEs come from `merDeriv`, whose cost
  grows superlinearly with the number of observations (about a minute at
  n ≈ 2,700). Above `options("spicy.re_se_max_n")` (default `1000`) they
  are skipped: the rows keep their estimates, the SE / CI cells render
  as en-dashes, a table note states the omission, and a `spicy_caveat`
  warning points here. Raise the cap (e.g.
  `options(spicy.re_se_max_n = Inf)`) to force the computation, or test
  the random terms with `re_test = "lrt"`.

- re_test:

  One of `"none"` (default), `"lrt"`, or `"rlrt"`. Opt-in **per-term
  significance test** for the random-effect variance components, filling
  the otherwise-empty p column of the Random effects rows. Never a Wald
  test (invalid at the boundary sigma = 0):

  - `"lrt"`: likelihood-ratio test of each random term vs the model
    refitted without it (the term's variance plus its covariances with
    the other terms of its bar), referred to the boundary-corrected
    chi-bar-squared mixture `0.5 chi2(q-1) + 0.5 chi2(q)` (Self & Liang
    1987; Stram & Lee 1994). The reduction scheme matches
    [`lmerTest::ranova()`](https://rdrr.io/pkg/lmerTest/man/ranova.html);
    the mixture reference makes the p-value exact-asymptotic rather than
    conservative. A bar's intercept is tested only when it is the bar's
    single term. Supported: `lmer`, `glmer`, `glmmTMB`, and `lme` with a
    simple `random = ~ terms | group` structure.

  - `"rlrt"`: exact restricted likelihood-ratio test with a simulated
    finite-sample null
    ([`RLRsim::exactRLRT()`](https://rdrr.io/pkg/RLRsim/man/exactRLRT.html);
    Crainiceanu & Ruppert 2004). Only defined for a Gaussian `lmer` /
    `lme` fit with a single variance component.

  The test statistic and df stay out of the displayed t/z column (they
  are chi-square-scale, not t/z) but are carried in
  [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
  (`test_type` `"chibar2"` / `"rlrt"`). The whole-block LR test in the
  footer is unaffected. Correlation and residual rows are never tested
  (a correlation is tested jointly with its slope; the residual has no
  zero-variance null). Refits happen once per random term: expect a
  noticeable cost on large models. For structures outside these two
  routes (e.g. multiple variance components needing a finite-sample
  null),
  [`pbkrtest::PBmodcomp()`](https://rdrr.io/pkg/pbkrtest/man/pb__modcomp.html)
  offers a parametric-bootstrap LR test on the same nested pair of fits
  (Halekoh & Hojsgaard 2014) – run it directly and report its p-value
  alongside the table.

- re_ci:

  One of `"wald"` (default) or `"profile"`. Uncertainty route for the
  random-effect variance-component rows of `lmer` / `glmer` fits:

  - `"wald"`: SE and symmetric CI from the observed information
    (`merDeriv`), subject to the `options("spicy.re_se_max_n")` size cap
    (see `re_columns`).

  - `"profile"`: **profile-likelihood CIs** via
    `confint(fit, method = "profile")` – the route lme4 itself documents
    and defaults to. The intervals are asymmetric and respect the
    boundary at 0; no SE is shown (lme4's position: a symmetric SE
    misdescribes the skewed sampling distribution of a variance).
    Sidesteps the size cap entirely (roughly two seconds per variance
    parameter even at n ~ 7,000; `glmer` profiles cost more). The footer
    discloses the method, and likelihood intervals transform exactly, so
    `re_scale = "variance"` shows the profile CI of the variance itself.

  `glmmTMB` and [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html)
  fits keep their engine-native CIs (TMB's `sdreport`; nlme's `apVar`)
  and refuse `"profile"`.

- model_labels:

  Per-model labels used as the **column-group spanner** above each
  model's sub-columns (console + gt / flextable / tinytable / Excel /
  Word renderers). `NULL` (default) resolves automatically; see
  *Multi-model semantics* for the full rule. A character vector of
  length `length(models)` overrides. Refused (error) for a single
  multinomial model: there the column groups are outcome categories,
  relabelled via `outcome_labels`.

- outcome_labels:

  Optional **Outcome body row** override. `NULL` (default) hides the row
  entirely – under the multi-model spanner the DV is already visible
  above the data. A character vector of length `length(models)` forces
  an explicit Outcome row with those values (the spanner stays as
  `"Model 1, ..."` unless `model_labels` is also supplied). `FALSE` also
  suppresses the row. **Single multinomial model** (outcome-as-columns
  layout): repurposed as the category spanner override – a character
  vector with one label per non-reference outcome category (unique, in
  model order), e.g. `c("Student vs Employed", ...)`; the reference
  category's AME-only group (when displayed) keeps its own name, and
  `FALSE` is a no-op (there is no Outcome row to suppress).

- stars:

  Significance asterisks. `FALSE` (default, APA 7 Section 6.46) – no
  stars. `TRUE` – APA cutoffs
  `c("*" = 0.05, "**" = 0.01, "***" = 0.001)`. A named numeric vector
  specifies custom thresholds, e.g.
  `c("+" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001)`.

- nested:

  Whether to inject pairwise change-statistic rows for adjacent models
  (M2 vs M1, M3 vs M2, ...). `FALSE` (default) – pure side-by-side
  display. `TRUE` – requires identical `nobs` and identical response
  variable across all models. See *Hierarchical (nested) model
  comparison*.

- digits:

  Decimal places for general numeric tokens (`b`, `beta`, `se`, `ci`,
  `t`, `f_change`, `lrt_change`, `deviance`, `deviance_change`, `ame`,
  `ame_se`, `weighted_nobs`). Default `2L`.

- p_digits:

  Decimal places for p-values (`p`, `ame_p`, `p_change`). APA-strict:
  leading zero stripped, `<.001` (or `<.0001` etc. depending on
  `p_digits`) for small values. Default `3L`.

- effect_size_digits:

  Decimals for per-coefficient effect sizes (`partial_f2`,
  `partial_eta2`, `partial_omega2`). Default `2L`.

- fit_digits:

  Decimals for variance-explained / model-level effect-size fit stats
  (`r2`, `adj_r2`, `r2_change`, `adj_r2_change`, `omega2`, `f2`,
  `f2_change`, `sigma`, `rmse`). Default `2L`.

- ic_digits:

  Decimals for information criteria (`AIC`, `AICc`, `BIC`, and their
  `_change` form). Default `1L`.

- decimal_mark:

  Decimal mark used in numeric display. `"."` (default) or `","`
  (European convention). When `","` is used, the CI bracket separator
  switches to `"; "` automatically to avoid `"0,18 [0,07, 0,30]"`
  ambiguity.

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

  Named character vector overriding per-coefficient row labels. Names
  are coefficient term names (from
  [`stats::terms()`](https://rdrr.io/r/stats/terms.html)); values are
  the displayed labels. E.g.
  `c("age" = "Age (years)", "sexM" = "Male (vs Female)")`. Default
  `NULL` (use raw term names).

- title, note:

  Override or suppress the auto-built caption / methodological footer.
  Three modes per argument:

  - `NULL` (default): the package builds the standard caption ("Linear
    regression on `<DV>`" / "Hierarchical linear regression on `<DV>`" /
    ...) and a methodological note (VCV type, p-adjust method, reference
    categories, ...).

  - `FALSE`: the corresponding banner row is omitted from every output
    engine. Use when the surrounding manuscript provides its own caption
    / note.

  - character string (length 1): replaces the auto-built text verbatim.
    The renderer applies no APA formatting on top – supply the exact
    string you want displayed (multi- line notes accepted via embedded
    `"\n"`).

  Validation messages, the spanner row, and the in-body change- stat
  rows are *not* affected – they belong to the table structure, not to
  the banner.

- output:

  Output type. `"default"` (a printable `spicy_regression_table`);
  `"data.frame"` / `"long"` (raw data); `"gt"` / `"flextable"` /
  `"tinytable"` (rich-format tables); `"excel"` (writes to
  `excel_path`); `"clipboard"` (copies to system clipboard); `"word"`
  (writes flextable to `word_path`).

- excel_path:

  File path for `output = "excel"`. Default `NULL` (required when
  `output = "excel"`).

- excel_sheet:

  Sheet name when writing to Excel. Default `"Regression"`.

- clipboard_delim:

  Field delimiter for `output = "clipboard"`. Default `"\t"`
  (tab-separated, pastes cleanly into Excel / Google Sheets / Word). The
  clipboard payload mirrors the Excel layout (title row, spanner row,
  header, body, footer note) but is plain text – horizontal rules, cell
  merging, decimal alignment, monospace font, and factor-level
  indentation cannot be encoded in TSV and are therefore absent from the
  paste.

  Paste behaviour by target:

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

  File path for `output = "word"`. Default `NULL` (required when
  `output = "word"`). The Word table inherits the flextable styling
  (Calibri font, APA borders, decimal-aligned numerics) and adds
  Word-specific features: an auto-numbered caption ("Table 1: ...",
  "Table 2: ...") via Word's `SEQ` field so multiple
  `table_regression()` calls in one document number consecutively; a
  re-printed header row on each page break; row split prevention so a
  single coefficient row never wraps across two pages; and an APA-styled
  note line (`*Note.*` italic prefix per APA Manual 7 §7.14).

  **R Markdown / Quarto:** for embedded use, prefer
  `output = "flextable"` (returns the flextable object that knits to
  docx/HTML/PDF natively). `output = "word"` writes a standalone .docx
  file, suited to scripted exports rather than chunk-level rendering.

- word_template:

  Optional path to a custom .docx file used as the template for
  `output = "word"`. The template's header, footer, page size, margins,
  and named styles ("Table Caption" in particular) are honoured; the
  table is appended to the template body. Useful for institutional
  templates with pre-set headers ("APA Style", "Manuscript Submission
  Template", etc.). Default `NULL` (uses flextable's stock template).

  **Customising the caption appearance:** the table caption is tagged
  with the Word named style `"Table Caption"`. The visual rendering
  (italic / bold / colour / font) follows whatever that style is set to
  in the docx template. The stock Word template renders
  `"Table Caption"` in italic — the APA Manual 7 §7.10 condensed
  convention. For a different appearance (Nature-style bold non-italic,
  APA-strict 2-line bold-number / italic-title, etc.), edit the
  `"Table Caption"` style in a docx template and pass it via
  `word_template = "your_template.docx"`. Style-based delegation keeps
  the rendered caption consistent with the surrounding document and lets
  editorial conventions (Nature, APA-strict, journal-specific) be
  applied without modifying the call site.

## Value

A `spicy_regression_table` object (a `data.frame` subclass with classes
`c("spicy_regression_table", "spicy_table", "data.frame")`) when
`output = "default"`. The result carries rendering attributes (`title`,
`note`, `align`, `padding`) and provenance attributes (`outcome`,
`model_ids`) consumed by the print method and the broom methods. For
other `output` values, returns the format-specific object (`gt_tbl`,
`flextable`, `tinytable`, `data.frame`, `tbl_df`, or `invisible(x)` for
side-effect outputs).

## Vocabulary tokens

Two vector arguments – `show_columns` and `show_fit_stats` – accept
named tokens that select **what** to display and in **what order**. All
tokens are lowercase (snake_case for compound tokens). Group tokens
(`"all_b"`, `"all_ame"`, ...) expand to a fixed vector of atomic tokens;
see `show_columns` below.

### `show_columns` – per-coefficient columns

Each token = one displayed column.

- Coefficient family: `"b"`, `"beta"` (standardised), `"se"`, `"ci"`,
  `"t"`, `"p"`.

- Marginal effects: `"ame"`, `"ame_se"`, `"ame_ci"`, `"ame_p"`. `"p"`
  always refers to the B-coefficient p-value; for the AME-specific
  p-value use `"ame_p"`.

- Partial effect sizes – `lm` only: `"partial_f2"`, `"partial_eta2"`,
  `"partial_omega2"`, each with a paired `_ci` companion
  (`"partial_f2_ci"`, ...).

- Partial effect size – `glm` only: `"partial_chi2"` (likelihood-ratio
  chi-square via `drop1(test = "LRT")`; SAS PROC LOGISTIC `TYPE3`; Long
  & Freese 2014 Section 3.5). Rendered as `value (df)` to disambiguate
  factor terms (k-1 df) from numeric terms (1 df).

- Sample columns: `"n"` – per-row N, populated by
  [`table_regression_uv()`](https://amaltawfik.github.io/spicy/reference/table_regression_uv.md)
  screens (each predictor block is its own fit); models without per-row
  N data drop the column. `"n_events"` – outcome event counts as
  `events/N`, per factor level (reference row included) and model totals
  on continuous rows, computed on each model's own estimation sample
  (STROBE item 16's "data behind the association"; NEJM-style
  `no. of events/total no.`). Binary (binomial) outcomes and
  right-censored `coxph` fits – other models raise
  `spicy_invalid_input`.

- Survival estimands – `coxph` only: `"rmst"`, `"rmst_se"`, `"rmst_ci"`,
  `"rmst_p"` (restricted-mean- survival-time difference over `[0, tau]`)
  and `"risk_diff"`, `"risk_diff_se"`, `"risk_diff_ci"`, `"risk_diff_p"`
  (cumulative-incidence difference at `at_time`), by g-computation from
  the fitted model with bootstrap inference. The horizon arguments are
  required; see `tau` / `at_time`.

**Group tokens** (presets) expand to a fixed atomic vector before
validation:

- `"all_b"` -\> `c("b", "se", "ci", "p")`

- `"all_b_compact"` -\> `c("b", "se", "p")`

- `"all_b_full"` -\> `c("b", "se", "ci", "t", "p")`

- `"all_beta"` -\> `c("b", "beta", "se", "ci", "p")`

- `"all_ame"` -\> `c("ame", "ame_se", "ame_ci", "ame_p")`

- `"all_ame_compact"` -\> `c("ame", "ame_p")`

- `"all_f2"` / `"all_eta2"` / `"all_omega2"` -\> `partial_*` + its `_ci`
  companion.

Mix groups and atomic tokens:
`show_columns = c("all_b", "ame", "ame_p")`. Duplicates after expansion
are deduplicated; the order of tokens controls the order of the
displayed columns. If `standardized != "none"` and `"beta"` is not
already requested, it is auto-injected after `"b"`. Asking for `"beta"`
while `standardized = "none"` raises `spicy_invalid_input`.

**Default** (`show_columns = NULL`) is context-aware: `"all_b"` for a
single model (APA-7 Section 6.46 publication layout), `"all_b_compact"`
for two or more models (CI dropped to fit the side-by-side layout;
restore it explicitly when needed).

### `show_fit_stats` – model-level rows below the coefficients

- Counts: `"nobs"`, `"weighted_nobs"`, `"n_events"` (Cox models: number
  of events, reported alongside `n` per the field convention; blank for
  other classes).

- Variance explained (`lm` only): `"r2"`, `"adj_r2"`, `"omega2"`.

- Pseudo-\\R^2\\ (`glm` and ordinal `polr` / `clm`):
  `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"`
  (Nagelkerke 1991), `"pseudo_r2_tjur"` (Tjur 2009; binomial only).

- Residual scale: `"sigma"` (lm \\\hat{\sigma}\\ / glm dispersion),
  `"rmse"`.

- Bayesian fits only: `"r2_bayes"` (posterior-median Bayesian \\R^2\\,
  Gelman et al. 2019 – in the all-Bayesian default), `"elpd_loo"` and
  `"looic"` (PSIS-LOO expected log predictive density and its
  deviance-scale twin, Vehtari et al. 2017; opt-in, a few seconds per
  model; the footer discloses the elpd standard error), and `"waic"`
  (Watanabe-Akaike; PSIS-LOO is generally preferred).

- Negative-binomial dispersion
  ([`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html) only):
  `"theta"` (\\V = \mu + \mu^2/\theta\\) and `"alpha"` (\\= 1/\theta\\,
  the Stata `nbreg` convention). Refused for other families.

- Beta-regression precision
  ([`betareg::betareg`](https://rdrr.io/pkg/betareg/man/betareg.html)
  only): `"phi"` (\\Var(y) = \mu(1-\mu)/(1+\phi)\\, Ferrari &
  Cribari-Neto 2004; higher \\\phi\\ = less dispersion). A constant
  precision is back-transformed from its link, so `y ~ x | 1` reports
  the same \\\phi\\ as `y ~ x`. Refused for other families and when the
  precision has covariates (`y ~ x | z`), so it is not a single number.

- fixest absorbed fixed effects (`fixest` only, both on by default for
  fixest tables): `"fixed_effects"` renders a `Fixed effects:` block at
  the top of the fit statistics – one Yes / No row per absorbed factor
  (the `etable` / `esttab` convention), blank cells for non-fixest
  models in mixed tables. Varying-slope-only factors (`Origin[[x]]`)
  absorb no intercept: they read No when another model absorbs that
  factor, and contribute no row otherwise. `"within_r2"` is the
  FE-partialled within R-squared (`feols`; GLM-family fixest fits report
  fixest's McFadden `pr2` instead). Both tokens are refused when no
  model in the table is a fixest fit.

- Effect size: `"f2"`.

- Information criteria: `"aic"`, `"aicc"`, `"bic"`, `"deviance"`
  (lowercase like every other token; the rendered row labels stay `AIC`
  / `AICc` / `BIC`).

- Change-stats for hierarchical comparison (active under
  `nested = TRUE`; see *Hierarchical comparison* below): `"r2_change"`,
  `"adj_r2_change"`, `"f_change"`, `"f2_change"`, `"lrt_change"`,
  `"aic_change"`, `"aicc_change"`, `"bic_change"`, `"deviance_change"`,
  `"p_change"`.

Default (resolved when `NULL`) is class-aware: lm fits get
`c("nobs", "r2", "adj_r2")`; glm and ordinal `polr` / `clm` fits get
`c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "aic")`; mixed
lm + glm sets union both groups (the renderer per-row en-dashes the
inappropriate cell); Cox fits get `c("nobs", "n_events", "aic")`. When
`nested = TRUE`, the class-aware default is extended with change tokens
(`c("r2_change", "f_change", "p_change")` for lm,
`c("lrt_change", "p_change")` for glm). The order of tokens in
`show_fit_stats` controls the order of the rows.

## Multi-model semantics

Pass a single fit or a [`list()`](https://rdrr.io/r/base/list.html) of
fits. Multi-model layout draws a centred **spanner label** above each
model's sub-columns:

- `list("Naive" = m1, "Adjusted" = m2)` -\> spanner labels `"Naive"` /
  `"Adjusted"`. Partial naming (`list("Naive" = m1, m2)`) auto-fills
  missing slots as `"Model <position>"`.

- `list(m1, m2)` (unnamed) -\> if all response variables differ, the
  bare DV name (from `formula(fit)[[2]]`) becomes the spanner label and
  the redundant Outcome body row is suppressed. If DVs match, the labels
  default to `"Model 1, 2, ..."`.

- `model_labels = c("A", "B")` overrides everything.

Duplicate explicit names in the list are rejected
(`spicy_invalid_input`) – they would silently collide in the internal
model_id key.

## Inference and standard errors

`vcov` selects the variance-covariance estimator:

- `"classical"` – OLS (lm) / Fisher information (glm).

- `"HC0"` to `"HC5"` – heteroskedasticity-consistent (via
  [`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)).

- `"CR0"` to `"CR3"` – cluster-robust with Satterthwaite-corrected df
  (via
  [`clubSandwich::vcovCR()`](https://rdrr.io/pkg/clubSandwich/man/vcovCR.html)).
  Requires `cluster`.

- `"bootstrap"` – nonparametric or cluster bootstrap (`boot_n`
  replicates).

- `"jackknife"` – leave-one-out / leave-one-cluster-out.

For multi-model use, both `vcov` and `cluster` accept a single value
(recycled to all models) or a list (one per model). The same fit can
appear several times with different estimators to compare standard
errors side-by-side.

Inferential regimes for `lm` / `glm` (B and AME share the same regime):

- `classical`, `HC*` -\> t with `df.residual` (`lm`) / z-asymptotic
  (`glm`).

- `bootstrap`, `jackknife` -\> z asymptotic.

- `CR0`-`CR3` -\> t with **Satterthwaite-corrected df** (B via
  [`clubSandwich::coef_test()`](https://rdrr.io/pkg/clubSandwich/man/coef_test.html);
  AME via
  [`clubSandwich::linear_contrast()`](https://rdrr.io/pkg/clubSandwich/man/linear_contrast.html);
  Pustejovsky & Tipton 2018). Under non-linear terms
  ([`poly()`](https://rdrr.io/r/stats/poly.html),
  [`I()`](https://rdrr.io/r/base/AsIs.html),
  [`log()`](https://rdrr.io/r/base/Log.html),
  [`splines::ns()`](https://rdrr.io/r/splines/ns.html)), AME falls back
  to z-asymptotic with a `spicy_fallback` warning.

For other classes the t-vs-z axis follows the estimator's native
reference distribution (e.g. `glm`, `glmmTMB`, survival, ordinal,
`betareg`, `mlogit` use z; `lm`, `lme`, `lmer`,
[`rms::ols`](https://rdrr.io/pkg/rms/man/ols.html) use t).

### Multinomial models: outcome categories as columns

A single [`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html)
model renders the publication layout: predictors as rows, one column
group per **non-reference outcome category** (spanner = category name),
so a predictor's effect can be compared across equations along its row.
The mandatory footer note `Reference outcome: <level>.` names the base
category (Stata's "base outcome" line); `outcome_labels` relabels the
spanners (e.g. `"Student vs Employed"`). Fit statistics print once,
under the first group. When per-category AMEs are requested the
reference category appears as a last, AME-only group (its coefficient
cells are empty – the reference has no equation; its AMEs complete the
zero-sum across categories). The default `show_columns` compacts to B /
SE / p exactly like multi-model tables; request atomic tokens to restore
CIs. Multi-model and `nested = TRUE` multinomial tables keep the
one-row-per-(category, predictor) layout – categories-within-models
would need two spanner levels. `tidy()` and `output = "long"` always
return the long form (`"<category>: <term>"` rows), whatever the
display;
[`as_structured()`](https://amaltawfik.github.io/spicy/reference/as_structured.md)
mirrors the displayed table (one column set per category), as it does
for every other layout.

### Robust SE availability by model class

Not every estimator is defined for every class. A robust `vcov` the
class cannot honour fails fast with `spicy_unsupported_vcov` – never a
silent model-based result under a robust label:

- `lm`, `glm`,
  [`MASS::glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html):

  all of `classical`, `HC*`, `CR*`, `bootstrap`, `jackknife`.

- `mlogit`:

  `classical` + `CR*` only (cluster at the choice-situation level) –
  [`sandwich::vcovHC()`](https://sandwich.R-Forge.R-project.org/reference/vcovHC.html)
  mis-scales the sandwich for mlogit's per-choice-situation scores, so
  `HC*` is refused.

- `lmer`, `lme`, `glmmTMB`, `coxph`, `survreg`,
  [`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html)/`bam`, `polr`,
  `clm`, `betareg`,
  [`survey::svyglm`](https://rdrr.io/pkg/survey/man/svyglm.html),
  [`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html), `rms`
  (`ols`/`lrm`/`cph`/`Glm`):

  `classical` + `CR*` only – `HC*` and the resamplers (which refit
  `lm`/`glm`) are not defined for these. `clm` with a scale / nominal
  (partial-PO) component is `classical` only. `multinom` needs sandwich
  \>= 3.1-2 (which added its `estfun()` method); its `cluster` is one
  entry per observation.

- Other classes (`glmer`, `rstanarm`/`brms`, ...):

  `classical` (model-based) only.

Cluster-robust backends differ by class but are each cross-validated to
the field-standard oracle: `lm`/`glm`/`lmer`/`lme`/`glmmTMB` use
clubSandwich (CR2 = Bell-McCaffrey, with Satterthwaite df for
`lm`/`lme`/`lmer`); `coxph`/`cph` use the Lin-Wei grouped-dfbeta
sandwich (identical to `coxph(..., cluster=)`);
`survreg`/`gam`/`polr`/`clm`/`betareg`/`mlogit`/`multinom` use
[`sandwich::vcovCL()`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html);
`svyglm` uses the design-aware clubSandwich estimator; `rms` fits use
[`rms::robcov()`](https://rdrr.io/pkg/rms/man/robcov.html) (which needs
the fit's `x = TRUE, y = TRUE`). These single cluster sandwiches have no
CR0-CR3 bias-reduction variants, so the requested `CR*` maps to the one
available estimator. `cluster` length is one entry per observation,
except `mlogit` (one per choice situation) and censored `coxph` (one per
subject).

### How to specify `cluster`

Three accepted forms, in order of preference:

1.  **Formula** – `cluster = ~region` (or `cluster = ~region:year` for
    the interaction of two variables). The variables are looked up in
    `model.frame(fit)` first, then in the original `data` argument
    captured by the fit. **Recommended**: independent of the dataset's
    name, composable for multi-way clustering, consistent with
    [`sandwich::vcovCL()`](https://sandwich.R-Forge.R-project.org/reference/vcovCL.html)
    /
    [`clubSandwich::vcovCR()`](https://rdrr.io/pkg/clubSandwich/man/vcovCR.html).

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

## Bayesian fits (rstanarm / brms)

`stanreg` and `brmsfit` models are summarized from their posterior
draws: `B` is the posterior median, `SE` the posterior MAD SD (the
scaled median absolute deviation – the pairing of *Regression and Other
Stories* and rstanarm's own print; equal to the posterior SD for a
normal posterior), and the interval an equal-tailed credible interval
(header `95% CrI`; `ci_method = "hdi"` opts into the highest-density
interval, header `95% HDI`). There is no p-value or t-statistic; group
presets (`"all_b"`, ...) expand without them, and the opt-in `"pd"`
column reports the probability of direction with a footer definition
(Makowski et al. 2019). Under `exponentiate = TRUE` all quantities are
computed from the exponentiated draws directly (the SE is the posterior
MAD SD on the ratio scale, not a delta-method approximation). The AME
columns are equally draws-native: `avg_slopes()` computes the marginal
effects per posterior draw, and the table reports their posterior
median, MAD SD and credible interval (equal-tailed or HDI per
`ci_method`); `"ame_p"` has nothing to fill and follows the p-column
policy (dropped from presets, refused as an atomic token, dashed in
mixed tables). Standardized betas follow the same draws logic:
`"posthoc"` / `"basic"` / `"smart"` are exact affine rescales of the
link-scale summaries (the Gaussian family divides by SD(y); every other
family standardizes predictors only, the frequentist glm convention);
`"refit"` and `"pseudo"` are refused.

Fit statistics: `"r2_bayes"` (in the Bayesian default) plus the opt-in
`"elpd_loo"` / `"looic"` / `"waic"`, whose standard errors are disclosed
in the footer; unreliable estimates (PSIS-LOO Pareto k above the
sample-size-specific threshold \\\min(1 - 1/\log\_{10} S, 0.7)\\ of
Vehtari et al. 2024 – the same bound
[`loo::loo()`](https://mc-stan.org/loo/reference/loo.html) prints – and
WAIC p_waic \> 0.4) add a footer caveat instead of being silenced. Every
table is backed by an automatic sampler-diagnostics guard (R-hat \>=
1.01, ESS below 100 per chain – floored at 400 so fewer chains never
weaken the bar –, divergent transitions, E-BFMI \< 0.2, per Vehtari et
al. 2021): problems add a footer line and raise a warning classed
`spicy_bayes_diagnostics` (nested under `spicy_caveat`, so
`withCallingHandlers(spicy_bayes_diagnostics = ...)` mutes the guard
selectively); clean fits print nothing. Per-coefficient `"rhat"` /
`"ess_bulk"` / `"ess_tail"` columns are available in all-Bayesian
tables, as is `"mcse"` – the Monte Carlo standard error of the displayed
posterior median, the criterion for how many digits a table can honestly
show (a displayed digit is Monte-Carlo stable when twice the MCSE stays
below it; Gelman, Vehtari, McElreath et al. 2026, sec. 11.6). Under
`exponentiate = TRUE` the MCSE is recomputed on the exponentiated draws.

Refused on principle (classed error, never a silent fallback):
likelihood-based fit statistics (`"aic"`, pseudo-R², ...), `p_adjust`,
robust / cluster `vcov` (model the clustering with group-level terms
instead), `ci_method = "profile"` / `"boot_percentile"`, and non-MCMC
fits (`algorithm = "meanfield"` / `"optimizing"` – refit with
`algorithm = "sampling"`). See
[`vignette("table-regression-bayesian")`](https://amaltawfik.github.io/spicy/articles/table-regression-bayesian.md).

## Hierarchical (nested) model comparison

`nested = TRUE` adds **per-pair change statistics as in-table rows**
(APA Table 7.13 / Stata `esttab` / SPSS Model Summary convention). Each
adjacent pair (M2 vs M1, M3 vs M2, ...) contributes one column of change
stats; the FIRST model column gets en-dashes (no previous model to
compare to). Validation requires identical `nobs` and identical response
variable across all models.

Default change tokens auto-injected when `show_fit_stats` is `NULL`:

- All-lm: `c("r2_change", "f_change", "p_change")` – APA hierarchical
  regression standard.

- All-glm: `c("lrt_change", "p_change")` – Hosmer & Lemeshow Section
  3.5; Long & Freese 2014 Section 3.6.

To customise, pass the change tokens directly to `show_fit_stats`.
Variance-explained change tokens on an all-glm hierarchy raise
`spicy_invalid_input` (the residual-sum-of-squares partition does not
apply outside the least-squares framework – the renderer points the user
at `lrt_change`).

## Standardised coefficients

`standardized` controls the method when `"beta"` is in `show_columns`:

- `"refit"` – refit on z-scored data. For `lm` both X and Y are z-scored
  (Cohen et al. 2003 gold standard); for `glm` only numeric X (Long &
  Freese 2014 Section 4.7.2 "x-standardization").

- `"posthoc"` – post-hoc scaling. lm: \\\beta = B \times SD(X) /
  SD(Y)\\; glm: X-only \\\beta = B \times SD(X)\\ (Y is undefined on the
  link scale).

- `"basic"` – like `"posthoc"` but factor dummies are scaled by their
  column SD.

- `"smart"` – Gelman (2008): continuous numeric inputs are scaled by
  `2 * SD(X)` (a +/-1 SD swing then spans the same range as a binary's 0
  to 1 step); binary inputs – numeric 0/1 and factor dummies – are left
  unscaled. (Before 0.13.0 the rule was applied inverted; see NEWS.)

- `"pseudo"` – *glm only*. Menard (2004, 2011) fully-standardised
  \\\beta = B \times SD(X) / SD(Y^\*)\\, with \\Y^\*\\ the latent
  variable on the link scale and \\SD(Y^\*) = \sqrt{Var(\hat{\eta}) +
  Var\_{link}}\\ (\\\pi^2/3\\ logit, `1` probit, \\\pi^2/6\\ cloglog).
  Binomial families only; non-binomial returns NA with a `spicy_caveat`.

- `"none"` (default) – no \\\beta\\ computed.

**Interactions and transforms.** Under `"refit"`, an interaction's
\\\beta\\ is the coefficient of the product of the z-scored components –
the recommended treatment for such models (Cohen et al. 2003 Section
7.7; Aiken & West 1991; Friedrich 1982). Under `"posthoc"`, `"basic"`,
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
procedures were designed for (Rothman 1990; Greenland 2017; APA Manual 7
Section 6.46; Harrell *Regression Modeling Strategies* Section 5.4;
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

- `"data.frame"` / `"long"` – raw data.frame / long-format tibble.

- `"gt"` / `"flextable"` / `"tinytable"` – rich-format HTML / Word / PDF
  tables (require the corresponding Suggests package).

- `"excel"` – writes to `excel_path` via
  [`openxlsx2::write_xlsx()`](https://janmarvin.github.io/openxlsx2/reference/write_xlsx.html).

- `"word"` – writes to `word_path` via
  [`flextable::save_as_docx()`](https://davidgohel.github.io/flextable/reference/save_as_docx.html).

- `"clipboard"` – copies to the system clipboard via
  [`clipr::write_clip()`](http://matthewlincoln.net/clipr/reference/write_clip.md).

[`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
returns a long tibble with one row per `(model_id, term, estimate_type)`
and broom-canonical column names (`estimate`, `std.error`, `conf.low`,
`conf.high`, `statistic`, `p.value`).
[`broom::glance()`](https://generics.r-lib.org/reference/glance.html)
returns one row per model with the model-level statistics; `df.residual`
is kept numeric so cluster-robust Satterthwaite df is preserved.

## Weights

No `weights` argument: weights are a property of the fit (extracted via
[`stats::weights()`](https://rdrr.io/r/stats/weights.html)). Pass them
when fitting: `lm(y ~ x, data = df, weights = w)`. All downstream
computations (vcov, AME, standardisation, `weighted_nobs`) extract them
automatically.

## Internationalisation

Output is in English. Override user-facing strings via
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

APA Manual 7 (American Psychological Association, 2020), Tables
7.13-7.15.

Aiken, L.S. & West, S.G. (1991). *Multiple regression: Testing and
interpreting interactions*.

Cohen, J., Cohen, P., West, S.G., & Aiken, L.S. (2003). *Applied
multiple regression / correlation analysis for the behavioral sciences*
(3rd ed.). Lawrence Erlbaum.

Davison, A.C. & Hinkley, D.V. (1997). *Bootstrap methods and their
application*. Cambridge University Press.

Friedrich, R.J. (1982). In defense of multiplicative terms in multiple
regression equations. *American Journal of Political Science*, 26(4),
797-833.

Pustejovsky, J.E. & Tipton, E. (2018). Small-sample methods for
cluster-robust variance estimation and hypothesis testing in fixed
effects models. *Journal of Business & Economic Statistics*, 36(4),
672-683.

Wasserstein, R.L., Schirm, A.L., & Lazar, N.A. (2019). Moving to a world
beyond "p \< 0.05". *The American Statistician*, 73(sup1), 1-19.

## See also

[`table_regression_models()`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md)
for the registry of supported model classes and the per-family behaviour
reference (also reachable as
[`?table_regression_mixed`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md),
[`?table_regression_ordinal`](https://amaltawfik.github.io/spicy/reference/table_regression_models.md),
...). If a class is not listed there, try `table_regression(fit)` anyway
– unsupported classes error with a clear message. Other regression-table
functions:
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
# Adj.R^2 fit-stats footer. Factor reference level is annotated
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
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).

# Standardised coefficients (beta) injected next to B. Four
# methods available; "refit" is the SPSS / Stata regress, beta
# gold standard.
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
#>  Adj.R²          │    0.02                                     
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> β = standardised coefficient.

# Custom column set: B + AME + AME-specific p-value. Note that
# the `p` token always belongs to B, never to AME -- use the
# explicit `ame_p` token for AME inference.
table_regression(
  fit,
  show_columns = c("b", "p", "ame", "ame_ci", "ame_p")
)
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
#>  Adj.R²          │    0.02                                     
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> AME = average marginal effect.

# Group-token shortcut: "all_b" + "all_ame" expands to the full
# B / AME column families side by side.
table_regression(fit, show_columns = c("all_b", "all_ame"))
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
#>  Adj.R²          │    0.02                                           
#> 
#>  Variable        │    95% CI        p   
#> ─────────────────┼──────────────────────
#>  (Intercept)     │                      
#>  age             │ [-0.01, 0.11]   .130 
#>  sex:            │                      
#>    Female (ref.) │       –         –    
#>    Male          │ [ 2.08, 5.63]  <.001 
#>  smoking:        │                      
#>    No (ref.)     │       –         –    
#>    Yes           │ [-3.89, 0.45]   .121 
#> 
#> Note. Linear regression.
#> Std. errors: classical (OLS).
#> AME = average marginal effect.

# ---- Cluster-robust variance -------------------------------------
# CR2 (Bell-McCaffrey) with Satterthwaite-corrected df is the
# recommended default under few clusters. Three forms are accepted
# for `cluster`; the formula is preferred for composability with
# multi-way clustering and for programmatic robustness.
table_regression(fit, vcov = "CR2", cluster = ~region)
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
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: cluster-robust (CR2), clusters by region.
table_regression(fit, vcov = "CR2", cluster = "region")
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
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: cluster-robust (CR2), clusters by region.
table_regression(fit, vcov = "CR2", cluster = ~region:age_group)
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
#>  Adj.R²          │    0.02                              
#> 
#> Note. Linear regression.
#> Std. errors: cluster-robust (CR2), clusters by region:age_group.

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
#>                           Step 1                Step 2              Step 3     
#>                    ────────────────────  ─────────────────────  ────────────── 
#>  Variable        │    B      SE     p       B       SE     p       B       SE  
#> ─────────────────┼─────────────────────────────────────────────────────────────
#>  (Intercept)     │   66.85  1.59  <.001    64.90   1.65  <.001    65.20   1.66 
#>  age             │    0.04  0.03   .160     0.05   0.03   .143     0.05   0.03 
#>  sex:            │                                                             
#>    Female (ref.) │                           –      –     –         –      –   
#>    Male          │                          3.87   0.91  <.001     3.86   0.91 
#>  smoking:        │                                                             
#>    No (ref.)     │                                                  –      –   
#>    Yes           │                                                -1.72   1.11 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n               │ 1175                  1175                   1175           
#>  R²              │    0.00                  0.02                   0.02        
#>  Adj.R²          │    0.00                  0.02                   0.02        
#>  ΔR²             │     –                   +0.02                  +0.00        
#>  F-change        │     –                  +18.26                  +2.41        
#>  p (change)      │     –                    <.001                   .121       
#> 
#>                    Step… 
#>                    ───── 
#>  Variable        │   p   
#> ─────────────────┼───────
#>  (Intercept)     │ <.001 
#>  age             │  .130 
#>  sex:            │       
#>    Female (ref.) │  –    
#>    Male          │ <.001 
#>  smoking:        │       
#>    No (ref.)     │  –    
#>    Yes           │  .121 
#> 
#> Note. Linear regression models.
#> Std. errors: classical (OLS).

# ---- Side-by-side variance comparison ----------------------------
# Same fit, three vcovs in one wide table. Useful for showing the
# sensitivity of inference to the variance assumption.
table_regression(
  list("Classical" = fit, "HC3" = fit, "CR2" = fit),
  vcov    = list("classical", "HC3", "CR2"),
  cluster = list(NULL, NULL, ~region)
)
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
#>  Adj.R²          │    0.02                  0.02                  0.02       
#> 
#>                     CR2  
#>                    ───── 
#>  Variable        │   p   
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
#> Note. Linear regression models.
#> Std. errors:
#>   Model 1: classical (OLS)
#>   Model 2: heteroskedasticity-robust (HC3)
#>   Model 3: cluster-robust (CR2), clusters by region

# ---- Tidy long format for downstream pipelines -------------------
broom::tidy(table_regression(fit))
#> # A tibble: 4 × 16
#>   model_id outcome outcome_level term  estimate_type estimate std.error conf.low
#>   <chr>    <chr>   <chr>         <chr> <chr>            <dbl>     <dbl>    <dbl>
#> 1 M1       wellbe… NA            (Int… B              65.2       1.66    62.0   
#> 2 M1       wellbe… NA            age   B               0.0465    0.0307  -0.0137
#> 3 M1       wellbe… NA            sexM… B               3.86      0.905    2.08  
#> 4 M1       wellbe… NA            smok… B              -1.72      1.11    -3.89  
#> # ℹ 8 more variables: conf.high <dbl>, statistic <dbl>, df <dbl>,
#> #   p.value <dbl>, test_type <chr>, is_intercept <lgl>, factor_term <chr>,
#> #   factor_level <chr>

# ---- Mixed-effects models ----------------------------------------
# Linear mixed-effects (lme4). The footer adds a random-effects
# panel with sigma + Wald SE / CI from `merDeriv`, the Nakagawa
# marginal / conditional R^2 fit-stats, and a per-class p-value
# annotation line.
if (requireNamespace("lme4", quietly = TRUE)) {
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject),
                     data = lme4::sleepstudy)
  table_regression(fit)

  # Switch to the variance scale (sigma^2 instead of sigma).
  table_regression(fit, re_scale = "variance")

  # Minimal random-effects display: estimates only, no SE / CI.
  table_regression(fit, re_columns = "est")

  # Suppress the random-effects panel entirely.
  table_regression(fit, show_re = FALSE)
}
#> Registered S3 method overwritten by 'merDeriv':
#>   method        from        
#>   bread.lmerMod clubSandwich
#> Linear mixed-effects regression: Reaction
#> 
#>  Variable         │    B      SE        95% CI         p   
#> ──────────────────┼────────────────────────────────────────
#>  (Intercept)      │  251.41  6.82  [238.03, 264.78]  <.001 
#>  Days             │   10.47  1.55  [  7.44,  13.50]  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                │  180                                   
#>  N (Subject)      │   18                                   
#>  R² (marginal)    │    0.28                                
#>  R² (conditional) │    0.80                                
#>  AIC              │ 1755.6                                 
#>  BIC              │ 1774.8                                 
#> 
#> Note. Linear mixed-effects regression.
#> Std. errors: Wald (model-based).
#> p-values: Wald-z, large-sample approximation. Load `lmerTest` for Satterthwaite t-tests.

# Hierarchical mixed-effects comparison (nested LRT).
if (requireNamespace("lme4", quietly = TRUE)) {
  m1 <- lme4::lmer(Reaction ~ 1     + (1 | Subject),
                    data = lme4::sleepstudy, REML = FALSE)
  m2 <- lme4::lmer(Reaction ~ Days  + (1 | Subject),
                    data = lme4::sleepstudy, REML = FALSE)
  m3 <- lme4::lmer(Reaction ~ Days  + (Days | Subject),
                    data = lme4::sleepstudy, REML = FALSE)
  table_regression(list(m1, m2, m3), nested = TRUE)
}
#> Hierarchical linear mixed-effects regression: Reaction
#> 
#>                                          Model 1                Model 2        
#>                                    ────────────────────  ───────────────────── 
#>  Variable                        │    B      SE     p       B       SE     p   
#> ─────────────────────────────────┼─────────────────────────────────────────────
#>  (Intercept)                     │  298.51  8.79  <.001   251.41   9.51  <.001 
#>  Days                            │                         10.47   0.80  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:                 │                                             
#>    σ Subject (Intercept)         │   34.59  6.72   –       36.01   6.45   –    
#>    σ Subject Days                │                                             
#>    ρ Subject ((Intercept), Days) │                                             
#>    σ (Residual)                  │   44.26  2.46   –       30.90   1.72   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                               │  180                   180                  
#>  N (Subject)                     │   18                    18                  
#>  ICC                             │    0.38                  0.58               
#>  R² (marginal)                   │    0.00                  0.29               
#>  R² (conditional)                │    0.38                  0.70               
#>  AIC                             │ 1916.5                1802.1                
#>  BIC                             │ 1926.1                1814.9                
#>  ΔAIC                            │     –                 -114.5                
#>  ΔBIC                            │     –                 -111.3                
#>  Δχ²                             │     –                 +116.46               
#>  p (change)                      │     –                    <.001              
#> 
#>                                           Model 3        
#>                                    ───────────────────── 
#>  Variable                        │    B       SE     p   
#> ─────────────────────────────────┼───────────────────────
#>  (Intercept)                     │  251.41   6.63  <.001 
#>  Days                            │   10.47   1.50  <.001 
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  Random effects:                 │                       
#>    σ Subject (Intercept)         │   23.78   5.58   –    
#>    σ Subject Days                │    5.72   1.19   –    
#>    ρ Subject ((Intercept), Days) │    0.08   0.32   –    
#>    σ (Residual)                  │   25.59   1.51   –    
#> ╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌┼╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌
#>  n                               │  180                  
#>  N (Subject)                     │   18                  
#>  R² (marginal)                   │    0.29               
#>  R² (conditional)                │    0.79               
#>  AIC                             │ 1763.9                
#>  BIC                             │ 1783.1                
#>  ΔAIC                            │  -38.1                
#>  ΔBIC                            │  -31.8                
#>  Δχ²                             │  +42.14               
#>  p (change)                      │    <.001              
#> 
#> Note. Linear mixed-effects regression models.
#> Std. errors: Wald (model-based).
#> p-values: Wald-z, large-sample approximation. Load `lmerTest` for Satterthwaite t-tests.
#> Model 1: Random effects (ML): LR test vs linear regression, χ̄²(1) = 50.51, p < .001.
#> Model 2: Random effects (ML): LR test vs linear regression, χ̄²(1) = 106.21, p < .001.
#> Model 3: Random effects (ML): LR test vs linear regression, χ̄²(3) = 148.35, p < .001.

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
