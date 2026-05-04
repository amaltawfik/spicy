# Continuous-outcome linear-model table

Builds APA-style summary tables from a series of simple linear models
for one or many continuous outcomes selected with tidyselect syntax.

A single predictor is supplied with `by`, and each selected numeric
outcome is fit as `lm(outcome ~ by, ...)`. When `by` is categorical, the
function returns a model-based mean-comparison table with fitted means
by level derived from the linear model, plus an optional single
difference for dichotomous predictors. When `by` is numeric, the table
reports the slope and its confidence interval.

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
  exclude = NULL,
  regex = FALSE,
  weights = NULL,
  vcov = c("classical", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5", "CR0", "CR1",
    "CR2", "CR3", "bootstrap", "jackknife"),
  cluster = NULL,
  boot_n = 1000,
  contrast = c("auto", "none"),
  statistic = FALSE,
  p_value = TRUE,
  show_n = TRUE,
  show_weighted_n = FALSE,
  effect_size = c("none", "f2", "d", "g", "omega2"),
  effect_size_ci = FALSE,
  r2 = c("r2", "adj_r2", "none"),
  ci = TRUE,
  labels = NULL,
  ci_level = 0.95,
  digits = 2,
  fit_digits = 2,
  effect_size_digits = 2,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "auto", "center", "right"),
  output = c("default", "data.frame", "long", "tinytable", "gt", "flextable", "excel",
    "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = "Linear models",
  clipboard_delim = "\t",
  word_path = NULL,
  verbose = FALSE
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
  string).

- by:

  A single predictor column. Accepts an unquoted column name or a single
  character column name. The predictor can be:

  - **numeric** (continuous): treated as a covariate. The table reports
    the slope and its CI from `lm(y ~ by)`.

  - **factor** or **ordered factor**: treated as categorical. Level
    order is preserved as declared; the **first level** is the reference
    for the displayed contrast (R's default treatment-contrast
    convention).

  - **character**: coerced to factor with `factor(by)`, which orders the
    levels alphabetically. To control the reference level, supply `by`
    as an explicit factor with the desired level ordering (e.g. via
    [`forcats::fct_relevel()`](https://forcats.tidyverse.org/reference/fct_relevel.html)
    or `factor(..., levels = ...)`).

  - **logical**: coerced to factor with levels `"FALSE"`, `"TRUE"` (in
    that order, since `FALSE < TRUE`). The reference level is `"FALSE"`,
    so a binary contrast displays as `Delta (TRUE - FALSE)`.

  Rows with `NA` in `by` are excluded from the analytic sample for each
  outcome (NAs in `y` and `weights` are also excluded; see Details).

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
  alongside rows with `NA` in `y` or `by`. When supplied, weights are
  passed to `lm(..., weights = ...)`, so coefficients become weighted
  least-squares estimates and `R²`, adjusted `R²`, and the four effect
  sizes are computed from the corresponding weighted sums of squares
  (see the *Weights* section in Details).

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
    the `df2` column and in the `t(df)` / `F(df1, df2)` test header.
    `"CR1"` corresponds to Stata's `, vce(cluster id)` default.
    Cluster-robust variants are dispatched to
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
  [`sandwich::vcovHC()`](https://rdrr.io/pkg/sandwich/man/vcovHC.html).
  Coefficients (means, contrasts, slopes), `R²`, and the standardized
  effect sizes (`f2`, `d`, `g`, `omega2`) are point estimates from the
  OLS/WLS fit and are not affected by `vcov`; only their standard
  errors, CIs, and the test statistic of the contrast change.

- cluster:

  Cluster identifier for cluster-aware variance estimators. Required
  when `vcov` is one of the `CR*` variants; optional and triggers a
  cluster bootstrap or leave-one-cluster-out jackknife when `vcov` is
  `"bootstrap"` / `"jackknife"`; forbidden for the other
  (independent-observation) variants. Accepts:

  - `NULL` (default): no cluster structure.

  - an unquoted column name in `data`.

  - a single character column name in `data`.

  - an atomic vector of length `nrow(data)` evaluated in the calling
    environment (factor, character, integer, etc.).

  Rows with `NA` in `cluster` are excluded from the analytic sample for
  each outcome (alongside rows with `NA` in `y`, `by`, or `weights`). At
  least two distinct non-missing cluster values are required. Multi-way
  clustering (a list / data.frame of multiple cluster vectors) is not
  supported; use
  [`sandwich::vcovCL()`](https://rdrr.io/pkg/sandwich/man/vcovCL.html)
  or
  [`clubSandwich::vcovCR()`](http://jepusto.github.io/clubSandwich/reference/vcovCR.md)
  directly on the fitted model for that case.

- boot_n:

  Integer. Number of bootstrap replicates used when
  `vcov = "bootstrap"`. Defaults to `1000`. Ignored otherwise. Larger
  values reduce Monte-Carlo error in the bootstrap variance; typical
  values for inference are `500`-`2000`.

- contrast:

  Contrast display for categorical predictors. One of:

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

  Logical. If `TRUE`, includes a `p` column in the wide and rendered
  outputs. Defaults to `TRUE`.

- show_n:

  Logical. If `TRUE`, includes an unweighted `n` column in the wide and
  rendered outputs. Defaults to `TRUE`.

- show_weighted_n:

  Logical. If `TRUE` and `weights` is supplied, includes a `Weighted n`
  column equal to the sum of case weights in the analytic sample.
  Defaults to `FALSE`.

- effect_size:

  Character. Effect-size column to include in the wide and rendered
  outputs. One of:

  - `"none"` (the default): no effect-size column.

  - `"f2"`: Cohen's `f² = R² / (1 - R²)`. Defined for any predictor
    type. Familiar from Cohen (1988); standard input for a-priori power
    analysis. Note that for a single-predictor model, `f²` is a monotone
    transform of `R²` and adds no information beyond it.

  - `"d"`: Cohen's `d = beta_hat / sigma_hat`, where `beta_hat` is the
    model coefficient (the displayed difference) and `sigma_hat` is the
    residual standard deviation from the fitted model. Defined only when
    `by` has exactly two non-empty levels; otherwise the function
    errors. The sign matches the displayed `Delta (level2 - level1)`.

  - `"g"`: Hedges' `g = J * d` with the small-sample correction
    `J = 1 - 3 / (4 * df_resid - 1)`. Same domain as `"d"`.

  - `"omega2"`: Hays' `omega-squared`, a bias-corrected estimator of the
    population variance explained, less optimistic than `R²` for small
    samples. Defined for any predictor type and truncated at 0.

  When `weights` is supplied, `"d"`, `"g"`, and `"omega2"` are derived
  from the weighted least-squares fit (using weighted sums of squares
  and the model's weighted residual standard deviation), keeping them
  consistent with the weighted contrast and its CI shown in the table.
  All effect sizes are point estimates derived from the OLS/WLS fit and
  are **not** affected by `vcov`.

- effect_size_ci:

  Logical. If `TRUE` and `effect_size != "none"`, adds a confidence
  interval for the effect size derived from inversion of the appropriate
  noncentral distribution (noncentral t for `"d"` / `"g"`; noncentral F
  for `"omega2"` / `"f2"`). The CI level is taken from `ci_level`. In
  the long output (`output = "long"`), the bounds are always present in
  `es_ci_lower` / `es_ci_upper` (numeric). In the wide raw output
  (`output = "data.frame"`), the bounds appear as numeric columns
  `effect_size_ci_lower` / `effect_size_ci_upper`. In the printed ASCII
  table and rendered outputs (`"tinytable"`, `"gt"`, `"flextable"`,
  `"word"`, `"excel"`, `"clipboard"`), the effect-size column shows the
  value followed by the CI in brackets (e.g. `0.18 [0.07, 0.30]`).
  Defaults to `FALSE`. When `effect_size = "none"`, this argument is
  ignored with a warning.

- r2:

  Character. Fit statistic to include in the wide and rendered outputs.
  One of:

  - `"r2"` (default): the model `R²` (`summary(lm)$r.squared`).

  - `"adj_r2"`: adjusted `R²`, penalising for `df_effect` relative to
    the residual degrees of freedom.

  - `"none"`: omit the fit-statistic column.

  When `weights` is supplied, `R²` and adjusted `R²` are the weighted
  least-squares versions reported by `summary(lm(..., weights = ...))`.

- ci:

  Logical. If `TRUE`, includes contrast confidence-interval columns in
  the wide and rendered outputs when a single contrast is shown.
  Defaults to `TRUE`.

- labels:

  An optional named character vector of outcome labels. Names must match
  column names in `data`. When `NULL` (the default), labels are
  auto-detected from variable attributes; if none are found, the column
  name is used.

- ci_level:

  Confidence level for coefficient and model-based mean intervals
  (default: `0.95`). Must be between 0 and 1 exclusive.

- digits:

  Number of decimal places for descriptive values, regression
  coefficients, and test statistics (default: `2`).

- fit_digits:

  Number of decimal places for model-fit columns (`R²` or adjusted `R²`)
  in wide and rendered outputs (default: `2`).

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
  convention.

- decimal_mark:

  Character used as decimal separator. Either `"."` (default) or `","`.

- align:

  Horizontal alignment of numeric columns in the printed ASCII table and
  in the `tinytable`, `gt`, `flextable`, `word`, and `clipboard`
  outputs. The first column (`Variable`) is always left-aligned. One of:

  - `"decimal"` (default): align numeric columns on the decimal mark,
    the standard scientific-publication convention used by SPSS, SAS,
    LaTeX `siunitx`,
    [`gt::cols_align_decimal()`](https://gt.rstudio.com/reference/cols_align_decimal.html)
    and `tinytable::style_tt(align = "d")`. For engines without a native
    decimal-alignment primitive (`flextable`, `word`, `clipboard`, ASCII
    print), values are pre-padded with leading and trailing spaces so
    the dots line up vertically; the body of the `flextable`/`word`
    output additionally uses a monospace font to make character widths
    uniform.

  - `"center"`: center-align all numeric columns.

  - `"right"`: right-align all numeric columns.

  - `"auto"`: legacy per-column rule used in spicy \< 0.11.0 (center for
    the descriptive / inferential columns; right for `n`, `Weighted n`,
    and `p`).

  The `excel` output uses the engine's default alignment in any case:
  cell-string padding does not align decimals under proportional fonts,
  and writing raw numbers with a numeric format would require a separate
  refactor.

- output:

  Output format. One of:

  - `"default"`: a printed ASCII table, returned invisibly

  - `"data.frame"`: a plain wide `data.frame`

  - `"long"`: a raw long `data.frame`

  - `"tinytable"` (requires `tinytable`)

  - `"gt"` (requires `gt`)

  - `"flextable"` (requires `flextable`)

  - `"excel"` (requires `openxlsx2`)

  - `"clipboard"` (requires `clipr`)

  - `"word"` (requires `flextable` and `officer`)

- excel_path:

  File path for `output = "excel"`.

- excel_sheet:

  Sheet name for `output = "excel"` (default: `"Linear models"`).

- clipboard_delim:

  Delimiter for `output = "clipboard"` (default: `"\t"`).

- word_path:

  File path for `output = "word"`.

- verbose:

  Logical. If `TRUE`, prints messages about ignored non-numeric selected
  outcomes (default: `FALSE`).

## Value

Depends on `output`:

- `"default"`: prints a styled ASCII table and invisibly returns the
  underlying long `data.frame` with class `"spicy_continuous_lm_table"`
  / `"spicy_table"`.

- `"data.frame"`: a plain wide `data.frame` with one row per outcome and
  numeric columns for means (categorical `by`) or slope (numeric `by`),
  optional contrast and CI, optional test statistic, `p`, fit statistic
  (`R²` or adjusted `R²`), effect size, optional `effect_size_ci_lower`
  / `effect_size_ci_upper` (when `effect_size_ci = TRUE`), `n`, and
  `Weighted n`.

- `"long"`: a raw `data.frame` with one block per outcome and 28 columns
  covering identification (`variable`, `label`, `predictor_type`,
  `predictor_label`, `level`, `reference`), fitted means and their CI
  (`emmean`, `emmean_se`, `emmean_ci_lower`, `emmean_ci_upper`),
  contrast or slope estimates and CI (`estimate_type`, `estimate`,
  `estimate_se`, `estimate_ci_lower`, `estimate_ci_upper`), inferential
  output (`test_type`, `statistic`, `df1`, `df2`, `p.value`), effect
  size with its CI (`es_type`, `es_value`, `es_ci_lower`,
  `es_ci_upper`), fit (`r2`, `adj_r2`), and sample size (`n`,
  `weighted_n`).

- `"tinytable"`: a `tinytable` object.

- `"gt"`: a `gt_tbl` object.

- `"flextable"`: a `flextable` object.

- `"excel"` / `"word"`: writes to disk and returns the file path.

- `"clipboard"`: copies the wide table and returns it invisibly.

If no numeric outcome columns remain after applying `select`, `exclude`,
and `regex`, the function emits a warning and returns an empty
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) regardless of
`output`.

## Model and outputs

`table_continuous_lm()` is designed for article-style bivariate
reporting: a single predictor supplied with `by`, and one simple model
per selected continuous outcome. The model fit is always
`lm(outcome ~ by, ...)`, optionally with `weights`. For categorical
predictors, the reported means are model-based fitted means for each
level of `by`, and contrasts are derived from the same fitted linear
model. For an unweighted `lm(y ~ factor)` with classical variance, the
fitted means coincide numerically with empirical subgroup means; the
*model-based* qualifier matters because (a) under `weights` the means
become weighted least-squares estimates, (b) their CIs are derived from
the model `vcov` (classical or `HC*`), and (c) tests, *p*-values, and
effect sizes all come from the same fitted model, keeping the table
internally consistent.

Compared with
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md),
this function is the model-based companion: choose it when you want
heteroskedasticity-consistent standard errors (`vcov = "HC*"`), model
fit statistics, or case weights via `lm(..., weights = ...)`. Because
the function exists to report a fitted model, its inferential output is
on by default: `p_value = TRUE` and `r2 = "r2"` are the defaults; set
`p_value = FALSE` or `r2 = "none"` to suppress them.

## Effect sizes

Effect size is selected explicitly via `effect_size` (defaults to
`"none"`). All variants are derived from the same fitted model as the
displayed coefficients, `R²`, and CIs, so the effect size stays
internally consistent with the rest of the table.

- `"f2"`: Cohen's `f² = R² / (1 - R²)` (Cohen 1988). Defined for any
  predictor type. For a single-predictor model, `f²` is a monotone
  transform of `R²` and adds no information beyond it; its primary use
  is in *a priori* power analysis (e.g. G\*Power).

- `"d"`, `"g"`: standardized mean difference (Cohen's *d* or Hedges'
  *g*), defined only when `by` has exactly two non-empty levels.
  `d = beta_hat / sigma_hat` with `sigma_hat = summary(fit)$sigma` (the
  pooled within-group SD for the unweighted two-group case); `g = J * d`
  with `J = 1 - 3 / (4 * df_resid - 1)` (Hedges and Olkin 1985). The
  sign matches the displayed `Delta (level2 - level1)`. For published
  reports of two-group comparisons, *g* is the convention recommended by
  Hedges and Olkin (1985).

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

Confidence intervals for the effect size are available via
`effect_size_ci = TRUE` and use the modern noncentral-distribution
inversion approach, the consensus standard in commercial statistical
software (Stata `esize` / `estat esize`, SAS `PROC TTEST` and
`PROC GLM EFFECTSIZE` 14.2+) and in mainstream R packages (`effectsize`,
`MOTE`, `TOSTER`, `effsize`):

- `"d"`, `"g"`: noncentral *t* inversion (Steiger and Fouladi 1997;
  Goulet-Pelletier and Cousineau 2018). Empirical coverage is nominal
  across sample sizes (Cousineau and Goulet-Pelletier 2021), unlike the
  older Hedges-Olkin normal approximation which is biased for small
  samples. For Hedges' *g* the bounds inherit the *J* small-sample
  correction.

- `"omega2"`, `"f2"`: noncentral *F* inversion (Steiger 2004). Bounds
  are converted from the noncentrality parameter using
  `omega² = ncp / (ncp + N)` and `f² = ncp / N` respectively, with
  `N = df1 + df2 + 1` (total sample size).

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
[`sandwich::vcovHC()`](https://rdrr.io/pkg/sandwich/man/vcovHC.html)
(Zeileis 2004), the canonical R implementation. For a brief guide:

- `"HC0"` is the original White (1980) form; `"HC1"` adds the
  `n / (n - p)` correction (MacKinnon and White 1985), Stata's
  `, robust` default.

- `"HC2"` and `"HC3"` use leverage-based residual rescalings (MacKinnon
  and White 1985); `"HC3"` is the
  [`sandwich::vcovHC()`](https://rdrr.io/pkg/sandwich/man/vcovHC.html)
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
fractional Satterthwaite degrees of freedom in `df2`, which the
displayed `t(df)` / `F(df1, df2)` header renders to one decimal. `"CR1"`
matches Stata's `, vce(cluster id)`. Effect sizes remain invariant to
`vcov` (including `CR*`); only the SE, CI, test statistic, and `df2` of
the contrast change.

Two resampling-based estimators are also available without adding any
dependency: `vcov = "bootstrap"` (nonparametric resampling-cases
bootstrap; Davison and Hinkley 1997) and `vcov = "jackknife"`
(leave-one-out delete-1; Quenouille 1956; MacKinnon and White 1985).
Supplying `cluster` switches both to their cluster-aware variants
(cluster bootstrap, Cameron, Gelbach and Miller 2008;
leave-one-cluster-out jackknife). The number of bootstrap replicates is
controlled by `boot_n` (default `1000`); replicates that fail to fit on
rank-deficient resamples are dropped, with an explicit warning if more
than half fail and a fallback to the classical OLS variance below 10
valid replicates. Inference for both estimators is asymptotic (`z` for
single-coefficient contrasts, `chi^2(q)` for the multi-coefficient
global Wald test on `k > 2` categorical predictors), reflected in the
displayed test header. Use the bootstrap when the residual distribution
is non-standard or the sample is small; use the jackknife as a
closed-form, deterministic alternative.

`R²`, adjusted `R²`, and the effect sizes remain ordinary least-squares
(or weighted least-squares) statistics regardless of `vcov`.

## Weights

When `weights` is supplied, `table_continuous_lm()` fits weighted linear
models via `lm(..., weights = ...)`. Means become weighted least-squares
estimates and contrasts and slopes are weighted. The fit statistics `R²`
and adjusted `R²`, as well as Hays' `omega²` and Cohen's `f²`, use the
corresponding **weighted sums of squares** from the WLS fit. Cohen's `d`
and Hedges' `g` use the **WLS coefficient and the model's weighted
residual standard deviation** (`summary(fit)$sigma`), which is the
standard convention for case-weighted regression-style reporting
(DuMouchel and Duncan 1983); the noncentral *t* CI for `d` / `g` uses
the raw (unweighted) group counts and the residual degrees of freedom of
the WLS fit (`n - p`). This case-weighted workflow is appropriate for
weighted article tables, but is **not** a substitute for a full
complex-survey design (see e.g. the `survey` package), nor for
propensity-score balance assessment under the Austin and Stuart (2015)
convention (see e.g. `cobalt::bal.tab()`).

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

Optional output engines require the corresponding suggested packages:

- tinytable for `output = "tinytable"`

- gt for `output = "gt"`

- flextable for `output = "flextable"`

- flextable + officer for `output = "word"`

- openxlsx2 for `output = "excel"`

- clipr for `output = "clipboard"`

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

Cousineau, D., & Goulet-Pelletier, J.-C. (2021). Expected and empirical
coverages of different methods for generating noncentral *t* confidence
intervals for a standardized mean difference. *Behavior Research
Methods*, **53**, 2376–2394.
[doi:10.3758/s13428-021-01550-4](https://doi.org/10.3758/s13428-021-01550-4)

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
[`sandwich::vcovHC()`](https://rdrr.io/pkg/sandwich/man/vcovHC.html)
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
[`spicy_tables`](https://amaltawfik.github.io/spicy/reference/spicy_tables.md),
[`table_categorical()`](https://amaltawfik.github.io/spicy/reference/table_categorical.md),
[`table_continuous()`](https://amaltawfik.github.io/spicy/reference/table_continuous.md)

## Examples

``` r
# --- Basic usage ---------------------------------------------------------

# Default: ASCII table with model-based means, p, and R².
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex
)
#> Continuous outcomes by Sex
#> 
#>  Variable                      │ M (Female)  M (Male)  Δ (Male - Female) 
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   67.16      71.05          3.89        
#>  Body mass index               │   25.69      26.20          0.51        
#> 
#>  Variable                      │ 95% CI LL  95% CI UL    p     R²    n   
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   2.13       5.64     <.001  0.02  1200 
#>  Body mass index               │   0.09       0.93      .018  0.00  1188 

# --- Effect sizes -------------------------------------------------------

# Cohen's d (binary by required).
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  effect_size = "d"
)
#> Continuous outcomes by Sex
#> 
#>  Variable                      │ M (Female)  M (Male)  Δ (Male - Female) 
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   67.16      71.05          3.89        
#>  Body mass index               │   25.69      26.20          0.51        
#> 
#>  Variable                      │ 95% CI LL  95% CI UL    p     R²    d     n   
#> ───────────────────────────────┼───────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   2.13       5.64     <.001  0.02  0.25  1200 
#>  Body mass index               │   0.09       0.93      .018  0.00  0.14  1188 

# Hedges' g with weighted analysis and weighted n column.
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  weights = weight,
  statistic = TRUE,
  effect_size = "g",
  show_weighted_n = TRUE
)
#> Continuous outcomes by Sex
#> 
#>  Variable                      │ M (Female)  M (Male)  Δ (Male - Female) 
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   67.01      70.88          3.87        
#>  Body mass index               │   25.51      25.98          0.47        
#> 
#>  Variable                      │ 95% CI LL  95% CI UL   t      p     R²    g   
#> ───────────────────────────────┼───────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   2.11       5.62     4.33  <.001  0.02  0.25 
#>  Body mass index               │   0.05       0.89     2.18   .030  0.00  0.13 
#> 
#>  Variable                      │  n    Weighted n 
#> ───────────────────────────────┼──────────────────
#>  WHO-5 wellbeing index (0-100) │ 1200   1196.47   
#>  Body mass index               │ 1188   1183.32   

# Hedges' g with noncentral t confidence interval (bracket notation).
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  effect_size = "g",
  effect_size_ci = TRUE
)
#> Continuous outcomes by Sex
#> 
#>  Variable                      │ M (Female)  M (Male)  Δ (Male - Female) 
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   67.16      71.05          3.89        
#>  Body mass index               │   25.69      26.20          0.51        
#> 
#>  Variable                      │ 95% CI LL  95% CI UL    p     R²  
#> ───────────────────────────────┼───────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   2.13       5.64     <.001  0.02 
#>  Body mass index               │   0.09       0.93      .018  0.00 
#> 
#>  Variable                      │         g           n   
#> ───────────────────────────────┼─────────────────────────
#>  WHO-5 wellbeing index (0-100) │ 0.25 [0.14, 0.36]  1200 
#>  Body mass index               │ 0.14 [0.02, 0.25]  1188 

# Cohen's f² alongside R² (familiar power-analysis effect size).
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  effect_size = "f2"
)
#> Continuous outcomes by Sex
#> 
#>  Variable                      │ M (Female)  M (Male)  Δ (Male - Female) 
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   67.16      71.05          3.89        
#>  Body mass index               │   25.69      26.20          0.51        
#> 
#>  Variable                      │ 95% CI LL  95% CI UL    p     R²    f²    n   
#> ───────────────────────────────┼───────────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   2.13       5.64     <.001  0.02  0.02  1200 
#>  Body mass index               │   0.09       0.93      .018  0.00  0.00  1188 

# Hays' omega-squared for a 3-level predictor (d / g would error here).
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = education,
  effect_size = "omega2"
)
#> Continuous outcomes by Highest education level
#> 
#>  Variable                      │ M (Lower secondary)  M (Upper secondary) 
#> ───────────────────────────────┼──────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │        67.68                81.56        
#>  Body mass index               │        26.17                23.55        
#> 
#>  Variable                      │ M (Tertiary)    p     R²    ω²    n   
#> ───────────────────────────────┼───────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │    66.10      <.001  0.21  0.21  1200 
#>  Body mass index               │    26.35      <.001  0.13  0.13  1188 

# --- Robust SE for a numeric predictor ----------------------------------

# HC3 standard errors for the slope of a continuous predictor.
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = age,
  vcov = "HC3",
  ci = FALSE
)
#> Continuous outcomes by Age (years)
#> 
#>  Variable                        │   B        p       R²      n    
#> ─────────────────────────────────┼─────────────────────────────────
#>  WHO-5 wellbeing index (0-100)   │  0.04     .176    0.00    1200  
#>  Body mass index                 │  0.04    <.001    0.02    1188  

# Cluster-robust SE for repeated-measures data: the `sleep` dataset
# has 10 subjects measured twice (one observation per group).
table_continuous_lm(
  sleep,
  select = extra,
  by = group,
  cluster = ID,
  vcov = "CR2"
)
#> Registered S3 method overwritten by 'clubSandwich':
#>   method    from    
#>   bread.mlm sandwich
#> Continuous outcomes by group
#> 
#>  Variable │ M (1)  M (2)  Δ (2 - 1)  95% CI LL  95% CI UL   p     R²   n  
#> ──────────┼───────────────────────────────────────────────────────────────
#>  extra    │ 0.75   2.33     1.58       0.70       2.46     .003  0.16  20 

# --- Article-style polish -----------------------------------------------

# Pretty outcome labels and adjusted R².
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  labels = c(
    wellbeing_score = "WHO-5 wellbeing (0-100)",
    bmi = "Body-mass index (kg/m²)"
  ),
  r2 = "adj_r2"
)
#> Continuous outcomes by Sex
#> 
#>  Variable                │ M (Female)  M (Male)  Δ (Male - Female)  95% CI LL 
#> ─────────────────────────┼────────────────────────────────────────────────────
#>  WHO-5 wellbeing (0-100) │   67.16      71.05          3.89           2.13    
#>  Body-mass index (kg/m²) │   25.69      26.20          0.51           0.09    
#> 
#>  Variable                │ 95% CI UL    p    Adj. R²   n   
#> ─────────────────────────┼─────────────────────────────────
#>  WHO-5 wellbeing (0-100) │   5.64     <.001   0.01    1200 
#>  Body-mass index (kg/m²) │   0.93      .018   0.00    1188 

# European decimal comma.
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  decimal_mark = ","
)
#> Continuous outcomes by Sex
#> 
#>  Variable                      │ M (Female)  M (Male)  Δ (Male - Female) 
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   67,16      71,05          3,89        
#>  Body mass index               │   25,69      26,20          0,51        
#> 
#>  Variable                      │ 95% CI LL  95% CI UL    p     R²    n   
#> ───────────────────────────────┼─────────────────────────────────────────
#>  WHO-5 wellbeing index (0-100) │   2,13       5,64     <,001  0,02  1200 
#>  Body mass index               │   0,09       0,93      ,018  0,00  1188 

# Regex selection of all columns starting with "life_sat".
table_continuous_lm(
  sochealth,
  select = "^life_sat",
  by = sex,
  regex = TRUE
)
#> Continuous outcomes by Sex
#> 
#>  Variable                                   │ M (Female)  M (Male) 
#> ────────────────────────────────────────────┼──────────────────────
#>  Satisfaction with health (1-5)             │    3.51       3.59   
#>  Satisfaction with work (1-5)               │    3.32       3.44   
#>  Satisfaction with relationships (1-5)      │    3.71       3.74   
#>  Satisfaction with standard of living (1-5) │    3.37       3.42   
#> 
#>  Variable                                   │ Δ (Male - Female)  95% CI LL 
#> ────────────────────────────────────────────┼──────────────────────────────
#>  Satisfaction with health (1-5)             │       0.08           -0.06   
#>  Satisfaction with work (1-5)               │       0.12           -0.01   
#>  Satisfaction with relationships (1-5)      │       0.04           -0.09   
#>  Satisfaction with standard of living (1-5) │       0.05           -0.08   
#> 
#>  Variable                                   │ 95% CI UL   p     R²    n   
#> ────────────────────────────────────────────┼─────────────────────────────
#>  Satisfaction with health (1-5)             │   0.22     .267  0.00  1192 
#>  Satisfaction with work (1-5)               │   0.26     .073  0.00  1192 
#>  Satisfaction with relationships (1-5)      │   0.16     .570  0.00  1192 
#>  Satisfaction with standard of living (1-5) │   0.18     .453  0.00  1192 

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
#>                                      Variable M (Female) M (Male)
#> wellbeing_score WHO-5 wellbeing index (0-100)   67.16194 71.04879
#> bmi                           Body mass index   25.68506 26.19685
#>                 Δ (Male - Female)  95% CI LL 95% CI UL            p          R²
#> wellbeing_score         3.8868576 2.12952733 5.6441879 1.548898e-05 0.015475137
#> bmi                     0.5117882 0.08879857 0.9347778 1.776254e-02 0.004728908
#>                    n
#> wellbeing_score 1200
#> bmi             1188

# Raw long data.frame (one block per outcome).
table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  output = "long"
)
#>          variable                         label predictor_type predictor_label
#> 1 wellbeing_score WHO-5 wellbeing index (0-100)    categorical             Sex
#> 2 wellbeing_score WHO-5 wellbeing index (0-100)    categorical             Sex
#> 3             bmi               Body mass index    categorical             Sex
#> 4             bmi               Body mass index    categorical             Sex
#>    level reference estimate_type   emmean emmean_se emmean_ci_lower
#> 1 Female    Female          <NA> 67.16194 0.6227155        65.94020
#> 2   Male    Female    difference 71.04879 0.6438305        69.78563
#> 3 Female    Female          <NA> 25.68506 0.1495988        25.39156
#> 4   Male    Female    difference 26.19685 0.1552460        25.89227
#>   emmean_ci_upper  estimate estimate_se estimate_ci_lower estimate_ci_upper
#> 1        68.38367        NA          NA                NA                NA
#> 2        72.31195 3.8868576   0.8957077        2.12952733         5.6441879
#> 3        25.97857        NA          NA                NA                NA
#> 4        26.50144 0.5117882   0.2155948        0.08879857         0.9347778
#>   test_type statistic df1  df2      p.value es_type es_value es_ci_lower
#> 1         F 18.830621   1 1198 1.548898e-05    <NA>       NA          NA
#> 2         t  4.339426   1 1198 1.548898e-05    <NA>       NA          NA
#> 3         F  5.635133   1 1186 1.776254e-02    <NA>       NA          NA
#> 4         t  2.373843   1 1186 1.776254e-02    <NA>       NA          NA
#>   es_ci_upper          r2      adj_r2    n weighted_n
#> 1          NA 0.015475137 0.014653330 1200         NA
#> 2          NA          NA          NA 1200         NA
#> 3          NA 0.004728908 0.003889725 1188         NA
#> 4          NA          NA          NA 1188         NA

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
