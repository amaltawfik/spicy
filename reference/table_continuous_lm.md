# Continuous-outcome linear-model table

Builds publication-ready summary tables from a series of linear models
for one or many continuous outcomes selected with tidyselect syntax.

A single focal predictor is supplied with `by`; each selected numeric
outcome is fit as `lm(outcome ~ by, ...)`, optionally extended with
additive covariates via `covariates` and case weights via `weights`.
Categorical `by` produces model-based estimated marginal means by level
(covariate-adjusted via `adjustment` when covariates are present), plus
an optional single difference for dichotomous predictors. Numeric `by`
produces the slope and its confidence interval.

Inference adapts via `vcov`: classical OLS, `"HC0"`-`"HC5"`
(heteroscedasticity-consistent), `"CR0"`-`"CR3"` (cluster-robust,
requires `cluster`), or `"bootstrap"` / `"jackknife"` resampling. Effect
sizes (Cohen's *d* `"cohens_d"`, Hedges' *g* `"hedges_g"`, Hays'
`"omega2"`, Cohen's `"f2"`) are reported with optional noncentral *t* /
*F* confidence intervals via `effect_size_ci`, and adapt under covariate
adjustment (see `effect_size`).

Multiple output formats are available via `output`: a printed ASCII
table (`"default"`), a plain wide `data.frame` (`"data.frame"`), a raw
long `data.frame` (`"long"`), or rendered outputs (`"tinytable"`,
`"gt"`, `"flextable"`, `"excel"`, `"clipboard"`, `"word"`).

## Usage

``` r
table_continuous_lm(
  data,
  select = tidyselect::everything(),
  by,
  covariates = NULL,
  adjustment = c("proportional", "balanced"),
  exclude = NULL,
  regex = FALSE,
  weights = NULL,
  vcov = c("classical", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5", "CR0", "CR1",
    "CR2", "CR3", "bootstrap", "jackknife"),
  cluster = NULL,
  boot_n = 1000,
  contrast = c("auto", "none"),
  statistic = FALSE,
  p_value = NULL,
  show_n = TRUE,
  show_weighted_n = FALSE,
  effect_size = c("none", "f2", "cohens_d", "hedges_g", "omega2"),
  effect_size_ci = FALSE,
  r2 = c("r2", "adj_r2", "none"),
  ci = TRUE,
  labels = getOption("spicy.var_labels", FALSE),
  ci_level = 0.95,
  digits = 2,
  fit_digits = 2,
  effect_size_digits = 2,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  verbose = FALSE,
  user_na = TRUE,
  style = NULL
)
```

## Arguments

- data:

  A `data.frame`.

- select:

  Outcome columns to include. If `regex = FALSE`, use tidyselect syntax
  or a character vector of column names (default:
  [`tidyselect::everything()`](https://tidyselect.r-lib.org/reference/everything.html)).
  If `regex = TRUE`, provide a regular expression pattern (character
  string). The `by` column and the `covariates` are not part of the
  selection:
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html)
  and the other helpers skip them, and naming one in `select` is an
  error. A name that is not a column of `data` is an error (class
  `spicy_missing_column`), and so is a selection with no numeric column.

- by:

  A single predictor column. Accepts an unquoted column name or a single
  character column name. The predictor can be:

  - **numeric** (continuous): treated as a continuous regressor. The
    table reports the slope of `by` and its CI from `lm(y ~ by, ...)`.

  - **factor** or **ordered factor**: treated as categorical. Level
    order is preserved as declared; the **first level** is the reference
    for the displayed contrast (R's default treatment-contrast
    convention). An ordered factor is refit as an unordered factor with
    the same level order, so the model uses treatment contrasts – not
    the polynomial contrasts (`contr.poly`)
    [`lm()`](https://rdrr.io/r/stats/lm.html) would apply by default.
    The ordering only determines the display order and the reference
    level; per-level means, the displayed difference, and its CI are
    identical to those of an unordered factor with the same levels.

  - **character**: coerced to factor with its values sorted in the C
    locale (capitals before lower case, the same on every machine), the
    order of the groups in the whole family
    ([`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
    [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
    ...): the first value in that order is the reference level, whatever
    the order of the rows. To control the reference level, supply `by`
    as an explicit factor with the desired level ordering (e.g. via
    [`forcats::fct_relevel()`](https://forcats.tidyverse.org/reference/fct_relevel.html)
    or `factor(..., levels = ...)`).

  - **logical**: coerced to factor with levels `"FALSE"`, `"TRUE"` (in
    that order, since `FALSE < TRUE`). The reference level is `"FALSE"`,
    so a binary contrast displays as `Delta (TRUE - FALSE)`.

  - **haven labelled with value labels** (an SPSS / Stata import):
    treated as categorical over the raw codes, in ascending code order
    (the reference level is the lowest code; the model needs a fixed
    level order), the order in which
    [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
    and
    [`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md)
    display the same groups. A labelled vector without value labels is
    treated as a continuous regressor. Declared missing values follow
    `user_na` as usual.

  Rows with `NA` in `by` are excluded from the analytic sample for each
  outcome (NAs in `y` and `weights` are also excluded; see Details), the
  rule of a table that computes (see `drop_na` in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md));
  this table has no `drop_na`.

  An outcome with no observed value in a level of a categorical `by`
  (typically a question not asked in that group) has no data there: it
  is fit on the other levels, the cells of that level show a dash, its
  rows are not counted as missing values, and the note says so. With
  fewer than two levels with data, the outcome has no model and no test.

- covariates:

  Optional additive covariates to adjust each per-outcome linear model
  for. Accepts a tidyselect expression (e.g. `covariates = c(age, sex)`,
  `covariates = tidyselect::all_of(cov_vec)`,
  `covariates = tidyselect::starts_with("control_")`) or a literal
  character vector of column names. Each covariate must be numeric,
  integer, logical, factor, or character. A variable cannot be both
  outcome and adjustment: the covariates are left out of the outcome
  selection (`select = everything()` skips them), and naming one in
  `select`, or giving its position, raises an error; a covariate that
  equals `by` raises an error too (a variable cannot be both predictor
  and adjustment).

  When non-empty, each model is fitted as
  `lm(y ~ by + cov1 + cov2 + ...)` and the reported estimate / SE /
  p-value / CI on `by` are covariate-adjusted via the focal coefficient.
  For categorical `by`, the displayed `emmean` is the covariate-adjusted
  estimated marginal mean – see `adjustment` for the choice of estimand
  (G-computation by default vs. equal-weight averaging). The omnibus
  test of `by` is the Wald *F* restricted to the focal coefficients
  (computed via `sandwich` / `clubSandwich` for HC\* / CR\* mode), so
  adding covariates does not contaminate the omnibus statistic with
  covariate contributions. Effect sizes adapt automatically – see
  `effect_size`.

  v1 supports additive covariates only. Formula syntax with interactions
  or transforms (`covariates = ~ age * sex`, `covariates = ~ I(age^2)`)
  is reserved for a future release; passing a formula raises a
  `spicy_unsupported` error with a migration hint.

  Rows with `NA` in any covariate are dropped from the analytic sample
  for each outcome (complete-cases per outcome, matching the existing
  `by` / `weights` NA handling).

- adjustment:

  How the covariate-adjusted estimated marginal means (the `emmean` /
  `emmean_se` / `emmean_ci_*` columns) are computed when `covariates` is
  non-empty (typed without covariates, it is ignored with a warning,
  class `spicy_ignored_arg`). One of:

  - `"proportional"` (the default; matches Stata `margins` and
    [`marginaleffects::avg_predictions()`](https://rdrr.io/pkg/marginaleffects/man/predictions.html)):
    G-computation on the observed sample. For each focal level of `by`,
    the model predicts at every observation with `by` set to that level
    (covariates kept at their observed values), and the predictions are
    averaged. Population-weighted by construction – the empirical joint
    distribution of covariates is the reference. Under `weights`, the
    averaging uses the case weights (the Stata `margins` convention
    after a weighted regression; equivalent to
    `marginaleffects::avg_predictions(wts = )`), so the reference
    distribution is the weighted one. Best when the goal is "what is the
    predicted mean in *this* population if everyone had `by = lvl`".

  - `"balanced"` (matches
    [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
    default and the SPSS UNIANOVA EMMEANS / SAS LSMEANS conventions):
    synthetic grid of factor-covariate level combinations x numeric
    covariates fixed at their sample mean, with each grid cell weighted
    equally. Treats the design as if covariates were balanced – the
    "marginal mean assuming a balanced design" estimand. Best when the
    goal is to report a covariate-purified comparison independent of the
    empirical covariate distribution.

  Both methods reduce to the same linear-contrast formula
  `emmean = avg_row %*% beta` and inherit the spicy variance pipeline
  (HC\* / CR\* / bootstrap / jackknife). They give the same answer when
  there are no covariates, and also when all covariates are numeric
  (fixed at their means either way). The two estimands diverge when a
  factor, character, or logical covariate has non-uniform observed
  proportions: those are levels to balance, and `"balanced"` weights
  them equally (a logical is a two-level factor to
  [`lm()`](https://rdrr.io/r/stats/lm.html), balanced FALSE/TRUE – the
  `emmeans` convention).

- exclude:

  Columns to exclude from `select`. Supports tidyselect syntax and
  character vectors of column names.

- regex:

  Logical. If `FALSE` (the default), uses tidyselect helpers. If `TRUE`,
  the `select` argument is treated as a regular expression.

- weights:

  Optional case weights. Accepts:

  - `NULL` (default): an ordinary unweighted
    [`lm()`](https://rdrr.io/r/stats/lm.html) is fit.

  - an **unquoted numeric column name** present in `data`.

  - a **single character column name** present in `data`.

  - a **numeric vector of length `nrow(data)`** evaluated in the calling
    environment.

  Validation: weights must be finite, non-negative, and contain at least
  one positive value (otherwise the function errors). Rows with `NA` in
  `weights` are excluded from the analytic sample for each outcome,
  alongside rows with `NA` in `y` or `by`. A weights column is not an
  outcome: `select = everything()` skips it, and naming it in `select`,
  or giving its position, is an error. When supplied, weights are passed
  to `lm(..., weights = ...)`, so coefficients become weighted
  least-squares estimates and `\eqn{R^2}{R^2}`, adjusted
  `\eqn{R^2}{R^2}`, and the four effect sizes are computed from the
  corresponding weighted sums of squares (see the *Weights* section in
  Details).

- vcov:

  Variance estimator used for standard errors, confidence intervals, and
  Wald test statistics. One of:

  - `"classical"` (default): the ordinary OLS/WLS variance from
    `vcov(lm)`, which assumes homoscedastic errors.

  - `"HC0"`: the original Eicker–White heteroskedasticity-consistent
    sandwich estimator (White 1980), with no finite-sample correction.

  - `"HC1"`: HC0 multiplied by `n / (n - p)` (MacKinnon and White 1985).
    Matches Stata's `, robust` default.

  - `"HC2"`: residuals divided by `sqrt(1 - h_ii)` (MacKinnon and White
    1985).

  - `"HC3"`: residuals divided by `(1 - h_ii)` (MacKinnon and White
    1985). A common default for small to moderate samples (Long and
    Ervin 2000).

  - `"HC4"`: leverage-adaptive variant designed for influential
    observations (Cribari-Neto 2004).

  - `"HC4m"`: refinement of HC4 with a modified leverage exponent
    (Cribari-Neto and da Silva 2011).

  - `"HC5"`: alternative leverage-adaptive variant designed for
    leveraged data (Cribari-Neto, Souza and Vasconcellos 2007).

  - `"CR0"`, `"CR1"`, `"CR2"`, `"CR3"`: cluster-robust sandwich
    estimators for non-independent observations (Liang and Zeger 1986);
    requires `cluster`. `"CR2"` is the modern default (Bell and
    McCaffrey 2002; Pustejovsky and Tipton 2018), with Satterthwaite
    degrees of freedom for inference; the fractional df is reported in
    the long output (in `df` beside a *t*, in `df.residual` beside an
    *F*) and in the `t(df)` / `F(df1, df2)` test header. `"CR1"` applies
    the G/(G-1) correction only; Stata's `, vce(cluster id)` uses the
    larger G(N-1)/((G-1)(N-p)) factor with t(G-1) inference
    (clubSandwich's `"CR1S"`; exposed for `lm` fits in
    [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
    not here), so `"CR1"` does not reproduce Stata. Cluster-robust
    variants are dispatched to
    [`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)
    and inference uses
    [`clubSandwich::coef_test()`](http://jepusto.github.io/clubSandwich/reference/coef_test.md)
    /
    [`clubSandwich::Wald_test()`](http://jepusto.github.io/clubSandwich/reference/Wald_test.md);
    install `clubSandwich` to use them.

  - `"bootstrap"`: nonparametric (resampling cases) or cluster bootstrap
    variance, depending on whether `cluster` is supplied (Davison and
    Hinkley 1997; Cameron, Gelbach and Miller 2008). The number of
    replicates is set by `boot_n`. Inference is asymptotic (`z` for
    single contrasts, `chi^2(q)` for the global Wald test); CIs are
    Wald-type around the point estimate.

  - `"jackknife"`: leave-one-out variance, or leave-one-cluster-out when
    `cluster` is supplied (Quenouille 1956; MacKinnon and White 1985).
    Inference is asymptotic (`z` / `chi^2(q)`).

  The `HC*` variants are computed via
  [`sandwich::vcovHC()`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html).
  Coefficients (means, contrasts, slopes), `\eqn{R^2}{R^2}`, and the
  standardized effect sizes (`f2`, `cohens_d`, `hedges_g`, `omega2`) are
  point estimates from the OLS/WLS fit and are not affected by `vcov`;
  only their standard errors, CIs, and the test statistic of the
  contrast change.

- cluster:

  Cluster identifier for cluster-aware variance estimators. Required
  when `vcov` is one of the `CR*` variants; optional and triggers a
  cluster bootstrap or leave-one-cluster-out jackknife when `vcov` is
  `"bootstrap"` / `"jackknife"`; forbidden for the other
  (independent-observation) variants. Accepts:

  - `NULL` (default): no cluster structure.

  - an unquoted column name in `data`.

  - a single character column name in `data`.

  - a one-sided formula naming a column in `data` (`~ region`), the
    `sandwich` / `fixest` convention shared with
    [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).

  - an atomic vector of length `nrow(data)` evaluated in the calling
    environment (factor, character, integer, etc.).

  Rows with `NA` in `cluster` are excluded from the analytic sample for
  each outcome (alongside rows with `NA` in `y`, `by`, or `weights`). A
  cluster column is not an outcome: `select = everything()` skips it,
  and naming it in `select`, or giving its position, is an error. At
  least two distinct non-missing cluster values are required. Multi-way
  clustering (a list / data.frame of multiple cluster vectors) is not
  supported; use
  [`sandwich::vcovCL()`](https://zeileis.codeberg.page/sandwich/reference/vcovCL.html)
  or
  [`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)
  directly on the fitted model for that case.

- boot_n:

  Number of bootstrap replicates used when `vcov = "bootstrap"`: a
  positive whole number, as in
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)
  and
  [`table_multiresponse()`](https://amaltawfik.github.io/spicy/reference/table_multiresponse.md)
  (a fraction is an error). Defaults to `1000`, the default of
  [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
  which computes the same bootstrap variance. That variance needs at
  least 10 valid replicates: a smaller `boot_n` is an error. Larger
  values reduce Monte-Carlo error in the bootstrap variance; typical
  values for inference are `500`-`2000`. Typed with another `vcov`, it
  is ignored with a warning (class `spicy_ignored_arg`).

- contrast:

  Contrast display for categorical predictors (typed with a numeric
  `by`, it is ignored with a warning, class `spicy_ignored_arg`). One
  of:

  - `"auto"` (default): show a single reference contrast
    `Delta (level2 - level1)` only when `by` has exactly two non-empty
    levels. The reference level is the **first level of the factor**
    (R's default treatment-contrast convention,
    `getOption("contrasts")[1]`). To change which level acts as the
    reference, re-level `by` upstream (for example with
    [`forcats::fct_relevel()`](https://forcats.tidyverse.org/reference/fct_relevel.html)
    or [`stats::relevel()`](https://rdrr.io/r/stats/relevel.html)).

  - `"none"`: suppress the contrast column for categorical predictors.
    Level-specific means are still displayed.

- statistic:

  Logical. If `TRUE`, includes a test-statistic column in the wide and
  rendered outputs. Defaults to `FALSE`.

- p_value:

  Logical or `NULL`. If `TRUE`, includes a `p` column in the wide and
  rendered outputs. `NULL` (the default) shows it whenever `by` is
  supplied, as in the rest of the family; `by` is required here, so
  `NULL` means `TRUE`. Pass `p_value = FALSE` to suppress it.

- show_n:

  Logical. If `TRUE`, includes an unweighted `n` column in the wide and
  rendered outputs. Defaults to `TRUE`.

- show_weighted_n:

  Logical. If `TRUE` and `weights` is supplied, includes a `Weighted n`
  column equal to the sum of case weights in the analytic sample.
  Defaults to `FALSE`.

- effect_size:

  Character. Effect-size column to include in the wide and rendered
  outputs (a logical is an error, class `spicy_defunct`). An estimator
  named after its author carries the name, as `"hedges_g"` does in
  [`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md):
  `"d"` and `"g"`, the spellings before 0.14.0, are an error of the same
  class that gives the new one. One of:

  - `"none"` (the default): no effect-size column.

  - `"f2"`: Cohen's
    `\eqn{f^2}{f^2} = \eqn{R^2}{R^2} / (1 - \eqn{R^2}{R^2})`. Defined
    for any predictor type. Familiar from Cohen (1988); standard input
    for a-priori power analysis. Note that for a single-predictor model,
    `\eqn{f^2}{f^2}` is a monotone transform of `\eqn{R^2}{R^2}` and
    adds no information beyond it.

  - `"cohens_d"`: Cohen's `d = beta_hat / sigma_hat`, where `beta_hat`
    is the model coefficient (the displayed difference) and `sigma_hat`
    is the residual standard deviation from the fitted model. Defined
    only when `by` has exactly two non-empty levels; otherwise the
    function errors. The sign matches the displayed
    `Delta (level2 - level1)`.

  - `"hedges_g"`: Hedges' `g = J * d` with the small-sample correction
    `J = 1 - 3 / (4 * df_resid - 1)`. Same domain as `"cohens_d"`.

  - `"omega2"`: Hays' `omega-squared`, a bias-corrected estimator of the
    population variance explained, less optimistic than `\eqn{R^2}{R^2}`
    for small samples. Defined for any predictor type and truncated at
    0.

  When `weights` is supplied, `"cohens_d"`, `"hedges_g"`, and `"omega2"`
  are derived from the weighted least-squares fit (using weighted sums
  of squares and the model's weighted residual standard deviation),
  keeping them consistent with the weighted contrast and its CI shown in
  the table. All effect sizes are point estimates derived from the
  OLS/WLS fit and are **not** affected by `vcov`.

  **Under covariate adjustment** (`covariates` non-empty):

  - `"f2"` and `"omega2"` become the **partial** *\\f^2\\* / partial
    *\\\omega^2\\*, derived from the partial *F* of `by` (the Type-II
    test of `by` after all covariates, equal to
    [`stats::drop1()`](https://rdrr.io/r/stats/add1.html) in this
    additive setting) – the correctly-defined effect size when the model
    is adjusted. For numeric `by`, partial *\\f^2\\* equals the squared
    partial correlation of `by` with the outcome, divided by
    `(1 - r^2_partial)`.

  - `"cohens_d"` and `"hedges_g"` raise a `spicy_unsupported` error:
    Cohen's *d* and Hedges' *g* have no canonical extension to adjusted
    models (the pooled SD is undefined under adjustment). Use `"f2"` or
    `"omega2"` instead – both generalize via partial *F*.

- effect_size_ci:

  Logical. If `TRUE` and `effect_size != "none"`, adds a confidence
  interval for the effect size derived from inversion of the appropriate
  noncentral distribution (noncentral t for `"cohens_d"` / `"hedges_g"`;
  noncentral F for `"omega2"` / `"f2"`). The CI level is taken from
  `ci_level`. In the long output (`output = "long"`), the bounds are
  always present in `es_ci_lower` / `es_ci_upper` (numeric). In the wide
  raw output (`output = "data.frame"`), the bounds appear under the same
  names, `es_ci_lower` / `es_ci_upper` (numeric). In the printed ASCII
  table and rendered outputs (`"tinytable"`, `"gt"`, `"flextable"`,
  `"word"`, `"excel"`, `"clipboard"`), the effect-size column shows the
  value followed by the CI in brackets (e.g. `0.18 [0.07, 0.30]`), under
  a header that states its level (`d [95\% CI]`). Defaults to `FALSE`.
  When `effect_size = "none"`, this argument is ignored with a warning.

- r2:

  Character. Fit statistic to include in the wide and rendered outputs.
  One of:

  - `"r2"` (default): the model `\eqn{R^2}{R^2}`
    (`summary(lm)$r.squared`).

  - `"adj_r2"`: adjusted `\eqn{R^2}{R^2}`, penalizing for `df_effect`
    relative to the residual degrees of freedom.

  - `"none"`: omit the fit-statistic column.

  When `weights` is supplied, `\eqn{R^2}{R^2}` and adjusted
  `\eqn{R^2}{R^2}` are the weighted least-squares versions reported by
  `summary(lm(..., weights = ...))`.

- ci:

  Logical. If `TRUE`, includes contrast confidence-interval columns in
  the wide and rendered outputs when a single contrast is shown.
  Defaults to `TRUE`.

- labels:

  How each variable is named in the table: in the rows, in the title,
  and in the notes, the covariates included. Four forms:

  - `labels = FALSE` shows the column name.

  - `labels = TRUE` shows the column's `label` attribute (as set by
    `haven`, `labelled`, or
    [`label_from_names()`](https://amaltawfik.github.io/spicy/reference/label_from_names.md))
    when it is present, not `NA`, and not empty, and the column name
    otherwise.

  - `labels = c(bmi = "Body mass index")`, a named character vector,
    gives the columns it names these labels. Every other column follows
    `options(spicy.var_labels)`: its name when the option is unset or
    `FALSE`, its `label` attribute when it is `TRUE`.

  - `labels = var_labels(bmi = "BMI (kg/m2)")` gives the columns it
    names these labels, and every other column its `label` attribute,
    whatever the option says.
    [`var_labels()`](https://amaltawfik.github.io/spicy/reference/var_labels.md)
    alone is `labels = TRUE`.

  Names that match no column are ignored, so one vector can serve every
  table of a project. The default reads `options(spicy.var_labels)`, and
  a `TRUE` or `FALSE` typed in the call wins over it. Any other value,
  `NULL` included, is an error. A variable label documents the data
  file; a published table carries the wording you set here.

- ci_level:

  Confidence level for coefficient and model-based mean intervals
  (default: `0.95`): a number strictly between 0 and 1.

- digits:

  Number of decimal places for descriptive values, regression
  coefficients, and test statistics (default: `2`). Must be a single
  non-negative whole number: a fraction (`2.9`) is an error. Same
  constraint for `fit_digits` and `effect_size_digits`.

- fit_digits:

  Number of decimal places for model-fit columns (`\eqn{R^2}{R^2}` or
  adjusted `\eqn{R^2}{R^2}`) in wide and rendered outputs (default:
  `2`).

- effect_size_digits:

  Number of decimal places for the effect-size column (`f2`, `d`, `g`,
  or `omega2`) in wide and rendered outputs (default: `2`).

- p_digits:

  Integer \>= 1. Number of decimal places used to render *p*-values in
  the `p` column (default: `3`, the APA Publication Manual standard).
  Both the displayed precision and the small-*p* threshold derive from
  this argument: `p_digits = 3` prints `.045` and `<.001`;
  `p_digits = 4` prints `.0451` and `<.0001`; `p_digits = 2` prints
  `.05` and `<.01`. Useful for genomics / GWAS contexts where adjusted
  *p*-values can be very small, or for journals using a coarser
  convention. Leading zeros are always stripped, following APA
  convention. Values below `1` and fractions (`3.7`) raise an error.

- decimal_mark:

  The decimal mark, a single character: `"."` (the default) or `","` in
  most styles; the Lancet style of
  [`spicy_style()`](https://amaltawfik.github.io/spicy/reference/spicy_style.md)
  uses the middle dot.

- align:

  Horizontal alignment of numeric columns in the printed ASCII table and
  in the `tinytable`, `gt`, `flextable`, `word`, and `clipboard`
  outputs. The first column (`Variable`) is always left-aligned. One of:

  - `"decimal"` (default): align numeric columns on the decimal mark,
    the standard scientific-publication convention used by SPSS, SAS,
    and LaTeX `siunitx`. Numeric cells are pre-padded with figure-spaces
    (U+2007, digit-width) so every string in a column has the same width
    with the decimal mark at the same internal position; centering those
    uniform-width strings then stacks the decimal points vertically. The
    same pad-then-center strategy is applied on every rendering engine
    (`gt`, `tinytable`, `flextable`, `word`, ASCII print) for a
    homogeneous rendering, matching
    [`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md).
    The `clipboard` output is delimited text meant to be parsed rather
    than read at a fixed width, so its cells travel unpadded (a padded
    number pastes as text next to an unpadded number).

  - `"center"`: center-align all numeric columns.

  - `"right"`: right-align all numeric columns.

  The `excel` output uses the engine's default alignment in any case:
  cell-string padding does not align decimals under proportional fonts,
  and writing raw numbers with a numeric format would require a separate
  refactor.

- output:

  Output format. One of:

  - `"default"`: an ASCII table object, printed when the call is bare

  - `"data.frame"`: the displayed table with its numbers, one row per
    outcome

  - `"long"`: one row per level of `by` and outcome (one row per outcome
    with a numeric `by`), with every estimate

  - `"tinytable"` (requires `tinytable`)

  - `"gt"` (requires `gt`)

  - `"flextable"` (requires `flextable`)

  - `"excel"` (requires `openxlsx2`)

  - `"clipboard"` (requires `clipr`)

  - `"word"` (requires `flextable` and `officer`)

- excel_path:

  File path for `output = "excel"`, ending in `".xlsx"`, in a directory
  that exists.

- excel_sheet:

  Sheet name for `output = "excel"`. `NULL` (the default) uses
  `"Linear models"`.

- clipboard_delim:

  Delimiter for `output = "clipboard"` (default: `"\t"`). A cell holding
  the delimiter itself, a double quote or a line break is quoted RFC
  4180-style, so the grid survives whatever delimiter you choose.

- word_path:

  File path for `output = "word"`, ending in `".docx"`, in a directory
  that exists.

- verbose:

  Logical. If `TRUE`, a message (class `spicy_info`) names the
  non-numeric columns left out of the outcomes (default: `FALSE`).

- user_na:

  Logical. If `TRUE` (the default), declared missing values in the
  outcomes, in `by`, and in `covariates` are treated as missing and
  excluded from the fitted models (reflected in the per-group `n`). If
  `FALSE`, the declared codes enter the fits as ordinary values. See the
  "Declared missing values" section of
  [`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md).

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

Depends on `output`:

- `"default"`: the compute frame with class
  `"spicy_continuous_lm_table"` / `"spicy_table"`, carrying the
  rendering metadata as attributes (the `by` column and its label as
  `group_var` and `group_label`, the title as printed as `title`). The
  object is returned visibly, so a bare `table_continuous_lm(...)` call
  auto-prints the styled ASCII table at the console while
  `t <- table_continuous_lm(...)` stays silent (print `t` to display the
  table).

- `"data.frame"`: the displayed table with its numbers, what
  [as.data.frame()](https://amaltawfik.github.io/spicy/reference/as.data.frame.spicy_continuous_lm_table.md)
  of the default object returns: one row per outcome, the key columns
  `variable` (the column name of the outcome) and `label` (the label
  displayed), then the printed columns under their names – the means
  (categorical `by`) or the slope (numeric `by`), the contrast and its
  interval bounds, the test statistic, `p`, the fit statistic, the
  effect size (with `es_ci_lower` / `es_ci_upper` beside it when
  `effect_size_ci = TRUE`), `n`, and `Weighted n`.

- `"long"`: one block per outcome, one row per level of `by` (one row,
  `estimate_type` `"slope"` and `level` `NA`, with a numeric `by`), with
  every estimate: identification (`variable`, `label`, `predictor_type`,
  `predictor_label`, `level`, `reference`), fitted means and their CI
  (`emmean`, `emmean_se`, `emmean_ci_lower`, `emmean_ci_upper`),
  contrast or slope estimates and CI (`estimate_type`, `estimate`,
  `estimate_se`, `estimate_ci_lower`, `estimate_ci_upper`), inferential
  output (`test_type`, `statistic`, `df`, `df.residual`, `p.value`: the
  *F* of a factor has both degrees of freedom, the *t* of a contrast or
  a slope has its own in `df` and none in `df.residual`), effect size
  with its CI (`es_type`, `es_value`, `es_ci_lower`, `es_ci_upper`), fit
  (`r.squared`, `adj.r.squared`, on the first row of an outcome), the
  size of the model (`nobs`, `weighted_nobs`, repeated on each row of an
  outcome), and `boot_n_valid`, the number of bootstrap replicates whose
  refit gave every coefficient (`NA` without `vcov = "bootstrap"`).

- `"tinytable"`: a `tinytable` object.

- `"gt"`: a `gt_tbl` object.

- `"flextable"`: a `flextable` object.

- `"excel"` / `"word"`: writes to disk and returns the file path
  invisibly.

- `"clipboard"`: copies the wide table and returns it invisibly.

The Excel sheet carries the same title the console prints on its first
row; the table itself starts on row 3, and the note lines sit below the
body. The default object and the data frames of `"data.frame"` and
`"long"` carry the whole note as printed, "Note." included, in
`attr(x, "note")` (`NULL` when there is nothing to say).

If no numeric outcome columns remain after applying `select`, `exclude`,
and `regex`, the function stops with an error (class
`spicy_invalid_input`), as the other tables of the family do.

## Model and outputs

`table_continuous_lm()` is designed for article-style reporting around a
single focal predictor: one model per selected continuous outcome,
fitted as `lm(outcome ~ by, ...)` and optionally extended with case
`weights` and additive `covariates` (`lm(outcome ~ by + cov1 + ...)`).
For categorical `by`, the reported means are model-based fitted means
(or covariate-adjusted estimated marginal means; see `adjustment`) for
each level, and contrasts come from the same fitted linear model. For an
unweighted `lm(y ~ factor)` with classical variance and no covariates,
the fitted means coincide numerically with empirical subgroup means; the
*model-based* qualifier matters because (a) under `weights` the means
become weighted least-squares estimates, (b) their CIs derive from the
model `vcov` (classical, `HC*`, `CR*`, bootstrap or jackknife), (c)
under `covariates` they become adjusted marginal means, and (d) tests,
*p*-values and effect sizes all come from the same fitted model, keeping
the table internally consistent.

Compared with
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
this function is the model-based companion: choose it when you want
heteroskedasticity-consistent standard errors (`vcov = "HC*"`), model
fit statistics, or case weights via `lm(..., weights = ...)`. Because
the function exists to report a fitted model, its inferential output is
on by default: the *p*-value is shown (`p_value = NULL`, the default,
shows it with `by`, which is required here) and `r2 = "r2"`; set
`p_value = FALSE` or `r2 = "none"` to suppress them.

An outcome with no variance in its fitted sample (every observation the
same value) leaves nothing to infer, as in
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md):
its test, its interval, its effect size and its \\R^2\\ are undefined
cells (`NA` in the long output and in `glance()`), its means and its
contrast or slope (0) are shown, and a warning (class
`spicy_undefined_stat`) names it. So does an outcome constant within
each group of a categorical `by` (a residual variance of zero): its
means, its contrast and its \\R^2\\ of 1 are shown, and its test,
interval and effect size are undefined. Every cell of an outcome applies
to it, so a cell with no value shows the undefined mark, never a blank:
a saturated fit (one observation per group) has no interval, test or
effect size, and a model that could not be fit has no cell at all.

## Effect sizes

Effect size is selected explicitly via `effect_size` (defaults to
`"none"`). All variants are derived from the same fitted model as the
displayed coefficients, `\eqn{R^2}{R^2}`, and CIs, so the effect size
stays internally consistent with the rest of the table.

- `"f2"`: Cohen's
  `\eqn{f^2}{f^2} = \eqn{R^2}{R^2} / (1 - \eqn{R^2}{R^2})` (Cohen 1988).
  Defined for any predictor type. For a single-predictor model,
  `\eqn{f^2}{f^2}` is a monotone transform of `\eqn{R^2}{R^2}` and adds
  no information beyond it; its primary use is in *a priori* power
  analysis (e.g. G\*Power).

- `"cohens_d"`, `"hedges_g"`: standardized mean difference (Cohen's *d*
  or Hedges' *g*), defined only when `by` has exactly two non-empty
  levels. `d = beta_hat / sigma_hat` with
  `sigma_hat = summary(fit)$sigma` (the pooled within-group SD for the
  unweighted two-group case); `g = J * d` with
  `J = 1 - 3 / (4 * df_resid - 1)` (Hedges and Olkin 1985). The sign
  matches the displayed `Delta (level2 - level1)`. For published reports
  of two-group comparisons, *g* is the convention recommended by Hedges
  and Olkin (1985).

- `"omega2"`: Hays' \\\omega^2\\, computed from weighted sums of squares
  as `(SS_effect - df_effect * MSE) / (SS_total + MSE)` and truncated at
  0 for small or null effects (Hays 1963; Olejnik and Algina 2003). Less
  biased than \\\eta^2\\ (which equals `R^2` in this single-predictor
  design) and recommended for reporting variance explained in
  ANOVA-style designs (Olejnik and Algina 2003).

All four effect sizes are point estimates derived from the OLS/WLS fit
and are **invariant to `vcov`**: choosing `HC*` changes the SE, CI, and
test statistic of the contrast but not the standardized magnitude
itself.

**Under covariate adjustment** (`covariates` non-empty), `"f2"` and
`"omega2"` become the partial *\\f^2\\* / partial *\\\omega^2\\* of
`by`, derived from the partial *F* restricted to the focal term (the
Type-II test of `by` after all covariates, equal to
[`stats::drop1()`](https://rdrr.io/r/stats/add1.html) in this additive
setting). `"cohens_d"` and `"hedges_g"` raise a `spicy_unsupported`
error: the pooled standard deviation has no canonical extension under
adjustment, so Cohen's *d* and Hedges' *g* are undefined for adjusted
models. See `effect_size` for the full dispatch.

Confidence intervals for the effect size are available via
`effect_size_ci = TRUE` and use the modern noncentral-distribution
inversion approach, the consensus standard in commercial statistical
software (Stata `esize` / `estat esize`, SAS `PROC TTEST` and
`PROC GLM EFFECTSIZE` 14.2+) and in mainstream R packages (`effectsize`,
`MOTE`, `TOSTER`, `effsize`):

- `"cohens_d"`, `"hedges_g"`: noncentral *t* inversion (Steiger and
  Fouladi 1997; Goulet-Pelletier and Cousineau 2018). Empirical coverage
  is nominal across sample sizes (Fitts 2021), unlike the older
  Hedges-Olkin normal approximation which is biased for small samples.
  For Hedges' *g* the bounds inherit the *J* small-sample correction.

- `"omega2"`, `"f2"`: noncentral *F* inversion (Steiger 2004). Without
  covariates (model-level effect sizes), bounds are converted from the
  noncentrality parameter using `omega^2 = ncp / (ncp + N)` and
  `\eqn{f^2}{f^2} = ncp / N` respectively, with `N = df1 + df2 + 1`
  (total sample size). Under covariate adjustment (partial effect
  sizes), the bounds use the partial transforms instead: partial
  `\eqn{f^2}{f^2} = ncp / df2`, and for partial `omega^2` the inversion
  runs at the *F*-value equivalent of the omega-squared point estimate
  with bounds `ncp / (ncp + df2)` – the
  [`effectsize::omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html)
  `partial = TRUE` convention, which is narrower than the partial
  eta-squared CI because omega-squared shrinks the point estimate.

For the weighted case, the CI uses raw (unweighted) group counts and
`df.residual(fit) = n - p`, consistent with the WLS reporting convention
(DuMouchel and Duncan 1983). For propensity-score balance assessment or
complex-survey designs, dedicated packages (`cobalt::bal.tab()` for the
Austin and Stuart 2015 formulation; `survey` for design-based effect
sizes) are more appropriate.

## Robust standard errors

When `vcov` is one of the `HC*` variants, the standard errors, CIs, and
Wald test statistics use a heteroskedasticity-consistent sandwich
estimator computed via
[`sandwich::vcovHC()`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html)
(Zeileis 2004), the canonical R implementation. For a brief guide:

- `"HC0"` is the original White (1980) form; `"HC1"` adds the
  `n / (n - p)` correction (MacKinnon and White 1985), Stata's
  `, robust` default.

- `"HC2"` and `"HC3"` use leverage-based residual rescalings (MacKinnon
  and White 1985); `"HC3"` is the
  [`sandwich::vcovHC()`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html)
  default for small to moderate samples (Long and Ervin 2000).

- `"HC4"` adapts the leverage exponent for influential observations
  (Cribari-Neto 2004); `"HC4m"` is a modified-exponent refinement
  (Cribari-Neto and da Silva 2011); `"HC5"` is an alternative
  leverage-adaptive variant (Cribari-Neto, Souza and Vasconcellos 2007).

When observations are not independent (repeated measurements per
individual, students nested in classes, patients in hospitals,
country-year panels), classical and `HC*` standard errors are biased
downward. Use the `CR*` variants together with `cluster = id_var` to get
cluster-robust inference (Liang and Zeger 1986). The implementation
dispatches to
[`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)
for the variance and to
[`clubSandwich::coef_test()`](http://jepusto.github.io/clubSandwich/reference/coef_test.md)
(single-coefficient, Satterthwaite *t*) and
[`clubSandwich::Wald_test()`](http://jepusto.github.io/clubSandwich/reference/Wald_test.md)
(multi-coefficient Hotelling-T-squared with Satterthwaite df, "HTZ") for
inference. `"CR2"` (Bell and McCaffrey 2002; Pustejovsky and Tipton
2018) is the modern recommended default; it generally produces
fractional Satterthwaite degrees of freedom (in `df` beside a *t*, in
`df.residual` beside an *F*), which the displayed `t(df)` /
`F(df1, df2)` header renders to one decimal. `"CR1"` applies the G/(G-1)
correction only – Stata's `, vce(cluster id)` uses the larger
G(N-1)/((G-1)(N-p)) factor with t(G-1) inference (clubSandwich's
`"CR1S"`; exposed for `lm` fits in
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md),
not here), so `"CR1"` does not reproduce Stata. Effect sizes remain
invariant to `vcov` (including `CR*`); only the SE, CI, test statistic,
and degrees of freedom of the contrast change.

Two resampling-based estimators are also available without adding any
dependency: `vcov = "bootstrap"` (nonparametric resampling-cases
bootstrap; Davison and Hinkley 1997) and `vcov = "jackknife"`
(leave-one-out delete-1; Quenouille 1956; MacKinnon and White 1985).
Supplying `cluster` switches both to their cluster-aware variants
(cluster bootstrap, Cameron, Gelbach and Miller 2008;
leave-one-cluster-out jackknife). The number of bootstrap replicates is
controlled by `boot_n` (default `1000`); replicates that fail to fit on
rank-deficient resamples are dropped, with an explicit warning if more
than half fail. Fewer than 10 valid bootstrap replicates (or fewer than
2 jackknife leave-outs) raises `spicy_resampling_failed` rather than
silently reporting a different variance estimator. Inference for both
estimators is asymptotic (`z` for single-coefficient contrasts,
`chi^2(q)` for the multi-coefficient global Wald test on `k > 2`
categorical predictors), reflected in the displayed test header. Use the
bootstrap when the residual distribution is non-standard or the sample
is small; use the jackknife as a closed-form, deterministic alternative.

`\eqn{R^2}{R^2}`, adjusted `\eqn{R^2}{R^2}`, and the effect sizes remain
ordinary least-squares (or weighted least-squares) statistics regardless
of `vcov`.

## Weights

When `weights` is supplied, `table_continuous_lm()` fits weighted linear
models via `lm(..., weights = ...)`. Means become weighted least-squares
estimates and contrasts and slopes are weighted. The fit statistics
`\eqn{R^2}{R^2}` and adjusted `\eqn{R^2}{R^2}`, as well as Hays'
`omega^2` and Cohen's `\eqn{f^2}{f^2}`, use the corresponding **weighted
sums of squares** from the WLS fit. Cohen's `d` and Hedges' `g` use the
**WLS coefficient and the model's weighted residual standard deviation**
(`summary(fit)$sigma`), which is the standard convention for
case-weighted regression-style reporting (DuMouchel and Duncan 1983);
the noncentral *t* CI for `d` / `g` uses the raw (unweighted) group
counts and the residual degrees of freedom of the WLS fit (`n - p`).
This case-weighted workflow is appropriate for weighted article tables,
but is **not** a substitute for a full complex-survey design (see e.g.
the `survey` package), nor for propensity-score balance assessment under
the Austin and Stuart (2015) convention (see e.g. `cobalt::bal.tab()`).

The `n` column always reports the unweighted analytic sample size for
each outcome. When `show_weighted_n = TRUE`, an additional `Weighted n`
column reports the sum of case weights in the same analytic sample.

## Display conventions

For dichotomous categorical predictors, the wide outputs report fitted
means in reference-level order and label the contrast column explicitly
as `Delta (level2 - level1)`. For categorical predictors with more than
two levels, no single contrast or contrast CI is shown in the wide
outputs; instead, the table reports level-specific means plus the
overall `F` test when `statistic = TRUE` (or `F(df1, df2)` when the
degrees of freedom are constant across outcomes).

When `covariates` is non-empty, the printed ASCII table appends an
APA-style footer naming the covariates and the chosen estimand, e.g.
`Note. Adjusted for age, education (proportional).` With p-values, the
note also names the test behind them, read off each outcome's model:
`Group comparison: t-test from the linear model.` (the F-test for a `by`
with more than two levels), or `Slope test: ...` for a numeric `by`.

The rendering engines carry that same footer as a table note. On the
`"tinytable"` route the note is set one size down;
`options(spicy.note_style)` governs that (see
[`table_regression()`](https://amaltawfik.github.io/spicy/reference/table_regression.md)).

Optional output engines require the corresponding suggested packages:

- tinytable for `output = "tinytable"`

- gt for `output = "gt"`

- flextable for `output = "flextable"`

- flextable + officer for `output = "word"`

- openxlsx2 for `output = "excel"`

- clipr for `output = "clipboard"`

## Declared missing values

Survey files imported with **haven** often carry *declared missing
values*: codes such as `8 = Don't know` or `9 = Refused` that the source
file marks as missing while keeping them distinct from a plain `NA`. Two
kinds of declaration exist: `na_values` / `na_range` metadata on
[`haven::labelled_spss()`](https://haven.tidyverse.org/reference/labelled_spss.html)
vectors, and tagged missing values created by
[`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
(the Stata `.a`, `.b`, ... convention).
[`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html)
keeps the `na_values` / `na_range` declaration only with
`user_na = TRUE`: its default turns the declared codes into `NA` on
import.

spicy honors the declaration by default (`user_na = TRUE`): declared
missing values are excluded from every statistic exactly like `NA` –
valid percentages, means, chi-squared tests, association measures,
row-wise summaries, and group definitions – but they are not erased from
display.
[`freq()`](https://amaltawfik.github.io/spicy/reference/freq.md) lists
each observed declared value as its own row of the Missing block, with
its value label;
[`cross_tab()`](https://amaltawfik.github.io/spicy/reference/cross_tab.md),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
and
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)
disclose the exclusion in the table note
(`Declared missing values removed: x (2).`);
[`varlist()`](https://amaltawfik.github.io/spicy/reference/varlist.md)
counts them as missing in `N_valid` / `NAs` / `N_distinct` while still
listing the declared codes in `Values`;
[`code_book()`](https://amaltawfik.github.io/spicy/reference/code_book.md)
counts them in `n_declared_missing` and flags them in its `values`
table.

Every function involved offers the same escape hatch: set
`user_na = FALSE` to ignore the declaration and treat the declared codes
as valid values (the behavior of spicy before 0.13.0). Tagged missing
values are genuine `NA`s either way; for them, `user_na = FALSE` only
collapses the per-tag breakdown back into the regular `NA` count.

## References

Austin, P. C., & Stuart, E. A. (2015). Moving towards best practice when
using inverse probability of treatment weighting (IPTW) using the
propensity score to estimate causal treatment effects in observational
studies. *Statistics in Medicine*, **34**(28), 3661–3679.
[doi:10.1002/sim.6607](https://doi.org/10.1002/sim.6607)

Bell, R. M., & McCaffrey, D. F. (2002). Bias reduction in standard
errors for linear regression with multi-stage samples. *Survey
Methodology*, **28**(2), 169–181.

Cameron, A. C., Gelbach, J. B., & Miller, D. L. (2008). Bootstrap-based
improvements for inference with clustered errors. *Review of Economics
and Statistics*, **90**(3), 414–427.
[doi:10.1162/rest.90.3.414](https://doi.org/10.1162/rest.90.3.414)

Cohen, J. (1988). *Statistical Power Analysis for the Behavioral
Sciences* (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum.

Cribari-Neto, F. (2004). Asymptotic inference under heteroskedasticity
of unknown form. *Computational Statistics & Data Analysis*, **45**(2),
215–233.
[doi:10.1016/S0167-9473(02)00366-3](https://doi.org/10.1016/S0167-9473%2802%2900366-3)

Cribari-Neto, F., Souza, T. C., & Vasconcellos, K. L. P. (2007).
Inference under heteroskedasticity and leveraged data. *Communications
in Statistics – Theory and Methods*, **36**(10), 1877–1888.
[doi:10.1080/03610920601126589](https://doi.org/10.1080/03610920601126589)

Cribari-Neto, F., & da Silva, W. B. (2011). A new
heteroskedasticity-consistent covariance matrix estimator for the linear
regression model. *AStA Advances in Statistical Analysis*, **95**(2),
129–146.
[doi:10.1007/s10182-010-0141-2](https://doi.org/10.1007/s10182-010-0141-2)

Davison, A. C., & Hinkley, D. V. (1997). *Bootstrap Methods and Their
Application*. Cambridge: Cambridge University Press.
[doi:10.1017/CBO9780511802843](https://doi.org/10.1017/CBO9780511802843)

DuMouchel, W. H., & Duncan, G. J. (1983). Using sample survey weights in
multiple regression analyses of stratified samples. *Journal of the
American Statistical Association*, **78**(383), 535–543.
[doi:10.1080/01621459.1983.10478006](https://doi.org/10.1080/01621459.1983.10478006)

Fitts, D. A. (2021). Expected and empirical coverages of different
methods for generating noncentral *t* confidence intervals for a
standardized mean difference. *Behavior Research Methods*, **53**(6),
2412–2429.
[doi:10.3758/s13428-021-01550-4](https://doi.org/10.3758/s13428-021-01550-4)

Goulet-Pelletier, J.-C., & Cousineau, D. (2018). A review of effect
sizes and their confidence intervals, Part I: The Cohen's *d* family.
*The Quantitative Methods for Psychology*, **14**(4), 242–265.
[doi:10.20982/tqmp.14.4.p242](https://doi.org/10.20982/tqmp.14.4.p242)

Hays, W. L. (1963). *Statistics for Psychologists*. New York: Holt,
Rinehart and Winston.

Hedges, L. V., & Olkin, I. (1985). *Statistical Methods for
Meta-Analysis*. Orlando, FL: Academic Press.

Long, J. S., & Ervin, L. H. (2000). Using heteroscedasticity consistent
standard errors in the linear regression model. *The American
Statistician*, **54**(3), 217–224.
[doi:10.1080/00031305.2000.10474549](https://doi.org/10.1080/00031305.2000.10474549)

Liang, K.-Y., & Zeger, S. L. (1986). Longitudinal data analysis using
generalized linear models. *Biometrika*, **73**(1), 13–22.
[doi:10.1093/biomet/73.1.13](https://doi.org/10.1093/biomet/73.1.13)

MacKinnon, J. G., & White, H. (1985). Some heteroskedasticity-consistent
covariance matrix estimators with improved finite sample properties.
*Journal of Econometrics*, **29**(3), 305–325.
[doi:10.1016/0304-4076(85)90158-7](https://doi.org/10.1016/0304-4076%2885%2990158-7)

Olejnik, S., & Algina, J. (2003). Generalized eta and omega squared
statistics: Measures of effect size for some common research designs.
*Psychological Methods*, **8**(4), 434–447.
[doi:10.1037/1082-989X.8.4.434](https://doi.org/10.1037/1082-989X.8.4.434)

Pustejovsky, J. E., & Tipton, E. (2018). Small-sample methods for
cluster-robust variance estimation and hypothesis testing in fixed
effects models. *Journal of Business & Economic Statistics*, **36**(4),
672–683.
[doi:10.1080/07350015.2016.1247004](https://doi.org/10.1080/07350015.2016.1247004)

Quenouille, M. H. (1956). Notes on bias in estimation. *Biometrika*,
**43**(3/4), 353–360.
[doi:10.1093/biomet/43.3-4.353](https://doi.org/10.1093/biomet/43.3-4.353)

Steiger, J. H. (2004). Beyond the *F* test: Effect size confidence
intervals and tests of close fit in the analysis of variance and
contrast analysis. *Psychological Methods*, **9**(2), 164–182.
[doi:10.1037/1082-989X.9.2.164](https://doi.org/10.1037/1082-989X.9.2.164)

Steiger, J. H., & Fouladi, R. T. (1997). Noncentrality interval
estimation and the evaluation of statistical models. In L. L. Harlow, S.
A. Mulaik, & J. H. Steiger (Eds.), *What if there were no significance
tests?* (pp. 221–257). Mahwah, NJ: Lawrence Erlbaum.

White, H. (1980). A heteroskedasticity-consistent covariance matrix
estimator and a direct test for heteroskedasticity. *Econometrica*,
**48**(4), 817–838.
[doi:10.2307/1912934](https://doi.org/10.2307/1912934)

Zeileis, A. (2004). Econometric computing with HC and HAC covariance
matrix estimators. *Journal of Statistical Software*, **11**(10), 1–17.
[doi:10.18637/jss.v011.i10](https://doi.org/10.18637/jss.v011.i10)

## See also

[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md).
For broader workflows on the same statistical building blocks:
[`sandwich::vcovHC()`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html)
(the canonical R implementation of the `HC*` sandwich estimators, used
internally for `vcov = "HC*"`);
[`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md),
[`clubSandwich::coef_test()`](http://jepusto.github.io/clubSandwich/reference/coef_test.md)
and
[`clubSandwich::Wald_test()`](http://jepusto.github.io/clubSandwich/reference/Wald_test.md)
(the canonical R implementation of cluster-robust variance and
Satterthwaite-style inference, used internally for `vcov = "CR*"`);
[`effectsize::cohens_d()`](https://easystats.github.io/effectsize/reference/cohens_d.html),
[`effectsize::hedges_g()`](https://easystats.github.io/effectsize/reference/cohens_d.html),
and
[`effectsize::omega_squared()`](https://easystats.github.io/effectsize/reference/eta_squared.html)
(alternative effect-size computations and CIs); `cobalt::bal.tab()` for
propensity-score covariate balance with weighted standardized mean
differences (Austin and Stuart 2015); the
[`survey`](https://CRAN.R-project.org/package=survey) package for
design-based inference on complex-survey samples.

Other spicy tables:
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)

## Examples

``` r
# --- Basic usage ---------------------------------------------------------

# Default: ASCII table with model-based means, p, and \eqn{R^2}{R^2}.
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex
)
#> Continuous outcomes by sex
#> 
#>  Variable        │ M (Female)  M (Male)  Δ (Male - Female)  95% CI LL 
#> ─────────────────┼────────────────────────────────────────────────────
#>  wellbeing_score │   67.16      71.05          3.89           2.13    
#>  bmi             │   25.69      26.20          0.51           0.09    
#> 
#>  Variable        │ 95% CI UL    p     R²    n   
#> ─────────────────┼──────────────────────────────
#>  wellbeing_score │   5.64     <.001  0.02  1200 
#>  bmi             │   0.93      .018  0.00  1188 
#> 
#> Note. Missing values removed: bmi (12). Group comparison: t-test from the linear model. CI = confidence interval; LL = lower limit; UL = upper limit.

# --- Effect sizes -------------------------------------------------------

# Cohen's d (binary by required).
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  effect_size = "cohens_d"
)
#> Continuous outcomes by sex
#> 
#>  Variable        │ M (Female)  M (Male)  Δ (Male - Female)  95% CI LL 
#> ─────────────────┼────────────────────────────────────────────────────
#>  wellbeing_score │   67.16      71.05          3.89           2.13    
#>  bmi             │   25.69      26.20          0.51           0.09    
#> 
#>  Variable        │ 95% CI UL    p     R²    d     n   
#> ─────────────────┼────────────────────────────────────
#>  wellbeing_score │   5.64     <.001  0.02  0.25  1200 
#>  bmi             │   0.93      .018  0.00  0.14  1188 
#> 
#> Note. Missing values removed: bmi (12). Group comparison: t-test from the linear model. CI = confidence interval; LL = lower limit; UL = upper limit.

# Hedges' g with weighted analysis and weighted n column.
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  weights = weight,
  statistic = TRUE,
  effect_size = "hedges_g",
  show_weighted_n = TRUE
)
#> Continuous outcomes by sex
#> 
#>  Variable        │ M (Female)  M (Male)  Δ (Male - Female)  95% CI LL 
#> ─────────────────┼────────────────────────────────────────────────────
#>  wellbeing_score │   67.01      70.88          3.87           2.11    
#>  bmi             │   25.51      25.98          0.47           0.05    
#> 
#>  Variable        │ 95% CI UL   t      p     R²    g     n    Weighted n 
#> ─────────────────┼──────────────────────────────────────────────────────
#>  wellbeing_score │   5.62     4.33  <.001  0.02  0.25  1200   1196.47   
#>  bmi             │   0.89     2.18   .030  0.00  0.13  1188   1183.32   
#> 
#> Note. Statistics weighted by weight. Missing values removed: bmi (12). Group comparison: t-test from the linear model. CI = confidence interval; LL = lower limit; UL = upper limit.

# Hedges' g with noncentral t confidence interval (bracket notation).
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  effect_size = "hedges_g",
  effect_size_ci = TRUE
)
#> Continuous outcomes by sex
#> 
#>  Variable        │ M (Female)  M (Male)  Δ (Male - Female)  95% CI LL 
#> ─────────────────┼────────────────────────────────────────────────────
#>  wellbeing_score │   67.16      71.05          3.89           2.13    
#>  bmi             │   25.69      26.20          0.51           0.09    
#> 
#>  Variable        │ 95% CI UL    p     R²      g [95% CI]       n   
#> ─────────────────┼─────────────────────────────────────────────────
#>  wellbeing_score │   5.64     <.001  0.02  0.25 [0.14, 0.36]  1200 
#>  bmi             │   0.93      .018  0.00  0.14 [0.02, 0.25]  1188 
#> 
#> Note. Missing values removed: bmi (12). Group comparison: t-test from the linear model. CI = confidence interval; LL = lower limit; UL = upper limit.

# Cohen's \eqn{f^2}{f^2} alongside \eqn{R^2}{R^2} (familiar power-analysis effect size).
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  effect_size = "f2"
)
#> Continuous outcomes by sex
#> 
#>  Variable        │ M (Female)  M (Male)  Δ (Male - Female)  95% CI LL 
#> ─────────────────┼────────────────────────────────────────────────────
#>  wellbeing_score │   67.16      71.05          3.89           2.13    
#>  bmi             │   25.69      26.20          0.51           0.09    
#> 
#>  Variable        │ 95% CI UL    p     R²    f²    n   
#> ─────────────────┼────────────────────────────────────
#>  wellbeing_score │   5.64     <.001  0.02  0.02  1200 
#>  bmi             │   0.93      .018  0.00  0.00  1188 
#> 
#> Note. Missing values removed: bmi (12). Group comparison: t-test from the linear model. CI = confidence interval; LL = lower limit; UL = upper limit; f² = Cohen's f².

# Hays' omega-squared for a 3-level predictor ("cohens_d" and
# "hedges_g" need two groups).
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = education,
  effect_size = "omega2"
)
#> Continuous outcomes by education
#> 
#>  Variable        │ M (Lower secondary)  M (Upper secondary)  M (Tertiary) 
#> ─────────────────┼────────────────────────────────────────────────────────
#>  wellbeing_score │        57.22                68.97            76.85     
#>  bmi             │        28.09                26.02            24.39     
#> 
#>  Variable        │   p     R²    ω²    n   
#> ─────────────────┼─────────────────────────
#>  wellbeing_score │ <.001  0.21  0.21  1200 
#>  bmi             │ <.001  0.13  0.13  1188 
#> 
#> Note. Missing values removed: bmi (12). Group comparison: F-test from the linear model.

# --- Robust SE for a numeric predictor ----------------------------------

# HC3 standard errors for the slope of a continuous predictor.
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = age,
  vcov = "HC3",
  ci = FALSE
)
#> Continuous outcomes by age
#> 
#>  Variable          │   B        p       R²      n    
#> ───────────────────┼─────────────────────────────────
#>  wellbeing_score   │  0.04     .176    0.00    1200  
#>  bmi               │  0.04    <.001    0.02    1188  
#> 
#> Note. Missing values removed: bmi (12). Slope test: t-test from the linear model. Std. errors: heteroskedasticity-robust (HC3).

# Cluster-robust SE for repeated-measures data: the `sleep` dataset
# has 10 subjects measured twice (one observation per group).
if (requireNamespace("clubSandwich", quietly = TRUE)) {
  table_continuous_lm(
    sleep,
    select = extra,
    by = group,
    cluster = ID,
    vcov = "CR2"
  )
}
#> Registered S3 methods overwritten by 'clubSandwich':
#>   method                from    
#>   bread.mlm             sandwich
#>   model.frame.iv_robust estimatr
#> Continuous outcomes by group
#> 
#>  Variable │ M (1)  M (2)  Δ (2 - 1)  95% CI LL  95% CI UL   p     R²   n  
#> ──────────┼───────────────────────────────────────────────────────────────
#>  extra    │ 0.75   2.33     1.58       0.70       2.46     .003  0.16  20 
#> 
#> Note. Group comparison: t-test from the linear model. Std. errors: cluster-robust (CR2), clusters by ID. CI = confidence interval; LL = lower limit; UL = upper limit.

# --- Covariate adjustment ----------------------------------------------

# Adjust the comparison of `wellbeing_score` and `bmi` by `sex` for `age`
# and `education`. The footer surfaces the adjustment estimand
# ("proportional" by default = G-computation, matching Stata `margins`).
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  covariates = c(age, education),
  vcov = "HC3"
)
#> Continuous outcomes by sex
#> 
#>  Variable        │ M (Female)  M (Male)  Δ (Male - Female)  95% CI LL 
#> ─────────────────┼────────────────────────────────────────────────────
#>  wellbeing_score │   67.27      70.93          3.65           2.08    
#>  bmi             │   25.65      26.23          0.58           0.19    
#> 
#>  Variable        │ 95% CI UL    p     R²    n   
#> ─────────────────┼──────────────────────────────
#>  wellbeing_score │   5.23     <.001  0.22  1200 
#>  bmi             │   0.97      .004  0.16  1188 
#> 
#> Note. Adjusted for age, education (proportional). Missing values removed: bmi (12). Group comparison: t-test from the linear model. Std. errors: heteroskedasticity-robust (HC3). CI = confidence interval; LL = lower limit; UL = upper limit.

# Same model with the emmeans / SPSS UNIANOVA convention (equal-weight
# marginal means on a synthetic covariate grid).
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  covariates = c(age, education),
  adjustment = "balanced",
  vcov = "HC3"
)
#> Continuous outcomes by sex
#> 
#>  Variable        │ M (Female)  M (Male)  Δ (Male - Female)  95% CI LL 
#> ─────────────────┼────────────────────────────────────────────────────
#>  wellbeing_score │   65.90      69.56          3.65           2.08    
#>  bmi             │   25.89      26.46          0.58           0.19    
#> 
#>  Variable        │ 95% CI UL    p     R²    n   
#> ─────────────────┼──────────────────────────────
#>  wellbeing_score │   5.23     <.001  0.22  1200 
#>  bmi             │   0.97      .004  0.16  1188 
#> 
#> Note. Adjusted for age, education (balanced). Missing values removed: bmi (12). Group comparison: t-test from the linear model. Std. errors: heteroskedasticity-robust (HC3). CI = confidence interval; LL = lower limit; UL = upper limit.

# Effect sizes adjust automatically: f2 / omega2 become partial
# effect sizes via the partial F restricted to the focal `by`.
# d / g are undefined under adjustment and raise spicy_unsupported.
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  covariates = c(age, education),
  effect_size = "f2",
  effect_size_ci = TRUE
)
#> Continuous outcomes by sex
#> 
#>  Variable        │ M (Female)  M (Male)  Δ (Male - Female)  95% CI LL 
#> ─────────────────┼────────────────────────────────────────────────────
#>  wellbeing_score │   67.27      70.93          3.65           2.09    
#>  bmi             │   25.65      26.23          0.58           0.19    
#> 
#>  Variable        │ 95% CI UL    p     R²      f² [95% CI]      n   
#> ─────────────────┼─────────────────────────────────────────────────
#>  wellbeing_score │   5.22     <.001  0.22  0.02 [0.01, 0.04]  1200 
#>  bmi             │   0.97      .004  0.16  0.01 [0.00, 0.02]  1188 
#> 
#> Note. Adjusted for age, education (proportional). Missing values removed: bmi (12). Group comparison: t-test from the linear model. CI = confidence interval; LL = lower limit; UL = upper limit; f² = Cohen's partial f².

# --- Article-style polish -----------------------------------------------

# Pretty outcome labels and adjusted \eqn{R^2}{R^2}.
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  labels = c(
    wellbeing_score = "WHO-5 wellbeing (0-100)",
    bmi = "Body-mass index (kg/m^2)"
  ),
  r2 = "adj_r2"
)
#> Continuous outcomes by sex
#> 
#>  Variable                 │ M (Female)  M (Male)  Δ (Male - Female)  95% CI LL 
#> ──────────────────────────┼────────────────────────────────────────────────────
#>  WHO-5 wellbeing (0-100)  │   67.16      71.05          3.89           2.13    
#>  Body-mass index (kg/m^2) │   25.69      26.20          0.51           0.09    
#> 
#>  Variable                 │ 95% CI UL    p    Adj. R²   n   
#> ──────────────────────────┼─────────────────────────────────
#>  WHO-5 wellbeing (0-100)  │   5.64     <.001   0.01    1200 
#>  Body-mass index (kg/m^2) │   0.93      .018   0.00    1188 
#> 
#> Note. Missing values removed: Body-mass index (kg/m^2) (12). Group comparison: t-test from the linear model. CI = confidence interval; LL = lower limit; UL = upper limit.

# European decimal comma.
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  decimal_mark = ","
)
#> Continuous outcomes by sex
#> 
#>  Variable        │ M (Female)  M (Male)  Δ (Male - Female)  95% CI LL 
#> ─────────────────┼────────────────────────────────────────────────────
#>  wellbeing_score │   67,16      71,05          3,89           2,13    
#>  bmi             │   25,69      26,20          0,51           0,09    
#> 
#>  Variable        │ 95% CI UL    p      R²    n   
#> ─────────────────┼───────────────────────────────
#>  wellbeing_score │   5,64     <0,001  0,02  1200 
#>  bmi             │   0,93      0,018  0,00  1188 
#> 
#> Note. Missing values removed: bmi (12). Group comparison: t-test from the linear model. CI = confidence interval; LL = lower limit; UL = upper limit.

# Regex selection of all columns starting with "life_sat".
table_continuous_lm(
  sochealth,
  select = "^life_sat",
  by = sex,
  regex = TRUE
)
#> Continuous outcomes by sex
#> 
#>  Variable               │ M (Female)  M (Male)  Δ (Male - Female)  95% CI LL 
#> ────────────────────────┼────────────────────────────────────────────────────
#>  life_sat_health        │    3.51       3.59          0.08           -0.06   
#>  life_sat_work          │    3.32       3.44          0.12           -0.01   
#>  life_sat_relationships │    3.71       3.74          0.04           -0.09   
#>  life_sat_standard      │    3.37       3.42          0.05           -0.08   
#> 
#>  Variable               │ 95% CI UL   p     R²    n   
#> ────────────────────────┼─────────────────────────────
#>  life_sat_health        │   0.22     .267  0.00  1192 
#>  life_sat_work          │   0.26     .073  0.00  1192 
#>  life_sat_relationships │   0.16     .570  0.00  1192 
#>  life_sat_standard      │   0.18     .453  0.00  1192 
#> 
#> Note. Missing values removed: life_sat_health (8), life_sat_work (8), life_sat_relationships (8), life_sat_standard (8). Group comparison: t-test from the linear model. CI = confidence interval; LL = lower limit; UL = upper limit.

# --- Output formats -----------------------------------------------------

# The rendered outputs below all wrap the same call:
#   table_continuous_lm(sochealth,
#                       select = c(wellbeing_score, bmi),
#                       by = sex)
# only `output` changes. Assign to a variable to avoid the
# console-friendly text fallback that some engines fall back to
# when printed directly in `?` help.

# Wide data.frame (one row per outcome).
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  output = "data.frame"
)
#>          variable           label M (Female) M (Male) Δ (Male - Female)
#> 1 wellbeing_score wellbeing_score   67.16194 71.04879         3.8868576
#> 2             bmi             bmi   25.68506 26.19685         0.5117882
#>    95% CI LL 95% CI UL            p          R²    n
#> 1 2.12952733 5.6441879 1.548898e-05 0.015475137 1200
#> 2 0.08879857 0.9347778 1.776254e-02 0.004728908 1188

# Raw long data.frame (one block per outcome).
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  output = "long"
)
#>          variable           label predictor_type predictor_label  level
#> 1 wellbeing_score wellbeing_score    categorical             sex Female
#> 2 wellbeing_score wellbeing_score    categorical             sex   Male
#> 3             bmi             bmi    categorical             sex Female
#> 4             bmi             bmi    categorical             sex   Male
#>   reference estimate_type   emmean emmean_se emmean_ci_lower emmean_ci_upper
#> 1    Female          <NA> 67.16194 0.6227155        65.94020        68.38367
#> 2    Female    difference 71.04879 0.6438305        69.78563        72.31195
#> 3    Female          <NA> 25.68506 0.1495988        25.39156        25.97857
#> 4    Female    difference 26.19685 0.1552460        25.89227        26.50144
#>    estimate estimate_se estimate_ci_lower estimate_ci_upper test_type statistic
#> 1        NA          NA                NA                NA         F 18.830621
#> 2 3.8868576   0.8957077        2.12952733         5.6441879         t  4.339426
#> 3        NA          NA                NA                NA         F  5.635133
#> 4 0.5117882   0.2155948        0.08879857         0.9347778         t  2.373843
#>     df df.residual      p.value es_type es_value es_ci_lower es_ci_upper
#> 1    1        1198 1.548898e-05    <NA>       NA          NA          NA
#> 2 1198          NA 1.548898e-05    <NA>       NA          NA          NA
#> 3    1        1186 1.776254e-02    <NA>       NA          NA          NA
#> 4 1186          NA 1.776254e-02    <NA>       NA          NA          NA
#>     r.squared adj.r.squared nobs weighted_nobs boot_n_valid
#> 1 0.015475137   0.014653330 1200            NA           NA
#> 2          NA            NA 1200            NA           NA
#> 3 0.004728908   0.003889725 1188            NA           NA
#> 4          NA            NA 1188            NA           NA

# \donttest{
# Rendered HTML / docx objects -- best viewed inside a
# Quarto / R Markdown document or a pkgdown article.
if (requireNamespace("tinytable", quietly = TRUE)) {
  tt <- table_continuous_lm(
    sochealth, select = c(wellbeing_score, bmi), by = sex,
    output = "tinytable"
  )
}
if (requireNamespace("gt", quietly = TRUE)) {
  tbl <- table_continuous_lm(
    sochealth, select = c(wellbeing_score, bmi), by = sex,
    output = "gt"
  )
}
if (requireNamespace("flextable", quietly = TRUE)) {
  ft <- table_continuous_lm(
    sochealth, select = c(wellbeing_score, bmi), by = sex,
    output = "flextable"
  )
}

# Excel and Word: write to a temporary file.
if (requireNamespace("openxlsx2", quietly = TRUE)) {
  tmp <- tempfile(fileext = ".xlsx")
  table_continuous_lm(
    sochealth, select = c(wellbeing_score, bmi), by = sex,
    output = "excel", excel_path = tmp
  )
  unlink(tmp)
}
if (
  requireNamespace("flextable", quietly = TRUE) &&
    requireNamespace("officer", quietly = TRUE)
) {
  tmp <- tempfile(fileext = ".docx")
  table_continuous_lm(
    sochealth, select = c(wellbeing_score, bmi), by = sex,
    output = "word", word_path = tmp
  )
  unlink(tmp)
}
# }

if (FALSE) { # \dontrun{
# Clipboard: writes to the system clipboard.
table_continuous_lm(
  sochealth, select = c(wellbeing_score, bmi), by = sex,
  output = "clipboard"
)
} # }
```
