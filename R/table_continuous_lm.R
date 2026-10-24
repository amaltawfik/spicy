#' Continuous-outcome linear-model table
#'
#' @description
#' Builds publication-ready summary tables from a series of linear
#' models for one or many continuous outcomes selected with tidyselect
#' syntax.
#'
#' A single focal predictor is supplied with `by`; each selected numeric
#' outcome is fit as `lm(outcome ~ by, ...)`, optionally extended with
#' additive covariates via `covariates` and case weights via `weights`.
#' Categorical `by` produces model-based estimated marginal means by
#' level (covariate-adjusted via `adjustment` when covariates are
#' present), plus an optional single difference for dichotomous
#' predictors. Numeric `by` produces the slope and its confidence
#' interval.
#'
#' Inference adapts via `vcov`: classical OLS, `"HC0"`-`"HC5"`
#' (heteroscedasticity-consistent), `"CR0"`-`"CR3"` (cluster-robust,
#' requires `cluster`), or `"bootstrap"` / `"jackknife"` resampling.
#' Effect sizes (Cohen's *d* `"cohens_d"`, Hedges's *g* `"hedges_g"`,
#' Hays's `"omega2"`, Cohen's `"f2"`) are reported with optional
#' noncentral *t* / *F*
#' confidence intervals via `effect_size_ci`, and adapt under
#' covariate adjustment (see `effect_size`).
#'
#' Multiple output formats are available via `output`: a printed ASCII table
#' (`"default"`), a plain wide `data.frame` (`"data.frame"`), a raw long
#' `data.frame` (`"long"`), or rendered outputs (`"tinytable"`, `"gt"`,
#' `"flextable"`, `"excel"`, `"clipboard"`, `"word"`).
#'
#' @param data A `data.frame`.
#' @param select Outcome columns to include. If `regex = FALSE`, use tidyselect
#'   syntax or a character vector of column names (default:
#'   `tidyselect::everything()`). If `regex = TRUE`, provide a regular expression
#'   pattern (character string). The `by` column and the `covariates` are
#'   not part of the selection: `everything()` and the other helpers skip
#'   them, and naming one in `select` is an error. A name that is not a
#'   column of `data` is an error (class `spicy_missing_column`), and so
#'   is a selection with no numeric column.
#' @param by A single predictor column. Accepts an unquoted column name or a
#'   single character column name. The predictor can be:
#'   - **numeric** (continuous): treated as a continuous regressor. The
#'     table reports the slope of `by` and its CI from `lm(y ~ by, ...)`.
#'   - **factor** or **ordered factor**: treated as categorical. Level order
#'     is preserved as declared; the **first level** is the reference for
#'     the displayed contrast (R's default treatment-contrast convention).
#'     An ordered factor is refit as an unordered factor with the same
#'     level order, so the model uses treatment contrasts -- not the
#'     polynomial contrasts (`contr.poly`) `lm()` would apply by default.
#'     The ordering only determines the display order and the reference
#'     level; per-level means, the displayed difference, and its CI are
#'     identical to those of an unordered factor with the same levels.
#'   - **character**: coerced to factor with its values sorted in the C
#'     locale (capitals before lower case, the same on every machine), the
#'     order of the groups in the whole family ([table_categorical()],
#'     [table_continuous()], ...): the first value in that order is the
#'     reference level, whatever the order of the rows. To control the
#'     reference level, supply `by` as an explicit factor with the desired
#'     level ordering (e.g. via [forcats::fct_relevel()] or
#'     `factor(..., levels = ...)`).
#'   - **logical**: coerced to factor with levels `"FALSE"`, `"TRUE"` (in
#'     that order, since `FALSE < TRUE`). The reference level is `"FALSE"`,
#'     so the binary contrast is `TRUE` minus `FALSE`.
#'   - **haven labelled with value labels** (an SPSS / Stata import):
#'     treated as categorical, one level per observed code named by its
#'     value label (`"[1] Women"`, as the rows of a labelled variable of
#'     [table_categorical()] are), in the order of its `labels` attribute
#'     when every observed value carries a label and by ascending code
#'     otherwise (the first level is the reference; the model needs a
#'     fixed level order), the order in which [table_continuous()] and
#'     [table_categorical()] display the same groups. A
#'     labelled vector without value labels is treated as a continuous
#'     regressor. Declared missing values follow `user_na` as usual.
#'
#'   Rows with `NA` in `by` are excluded from the analytic sample for each
#'   outcome (NAs in `y` and `weights` are also excluded; see Details), the
#'   rule of a table that computes (see `drop_na` in
#'   [table_continuous()]); this table has no `drop_na`. The note lists
#'   `by` after the outcomes and covariates ("Missing values removed: bmi
#'   (3), sex (2)."): a missing `by` or covariate takes the row out of the
#'   model of every outcome, a missing outcome only out of its own.
#'
#'   An outcome with no observed value in a level of a categorical `by`
#'   (typically a question not asked in that group) has no data there: it
#'   is fit on the other levels, the cells of that level show a dash, its
#'   rows are not counted as missing values, and the note says so. With
#'   fewer than two levels with data, the outcome has no model and no
#'   test.
#' @param covariates Optional additive covariates to adjust each per-outcome
#'   linear model for. Accepts a tidyselect expression (e.g.
#'   `covariates = c(age, sex)`, `covariates = tidyselect::all_of(cov_vec)`,
#'   `covariates = tidyselect::starts_with("control_")`) or a literal
#'   character vector of column names. Each covariate must be numeric,
#'   integer, logical, factor, or character. A variable cannot be both
#'   outcome and adjustment: the covariates are left out of the outcome
#'   selection (`select = everything()` skips them), and naming one in
#'   `select`, or giving its position, raises an error; a covariate that
#'   equals `by` raises an error too (a variable cannot be both predictor
#'   and adjustment).
#'
#'   When non-empty, each model is fitted as `lm(y ~ by + cov1 + cov2 + ...)`
#'   and the reported estimate / SE / p-value / CI on `by` are
#'   covariate-adjusted via the focal coefficient. For categorical `by`,
#'   the displayed `emmean` is the covariate-adjusted estimated marginal
#'   mean -- see `adjustment` for the choice of estimand
#'   (G-computation by default vs. equal-weight averaging). The omnibus
#'   test of `by` is the Wald *F* restricted to the focal coefficients
#'   (computed via `sandwich` / `clubSandwich` for HC* / CR* mode), so
#'   adding covariates does not contaminate the omnibus statistic with
#'   covariate contributions. Effect sizes adapt automatically -- see
#'   `effect_size`.
#'
#'   v1 supports additive covariates only. Formula syntax with
#'   interactions or transforms (`covariates = ~ age * sex`,
#'   `covariates = ~ I(age^2)`) is reserved for a future release; passing
#'   a formula raises a `spicy_unsupported` error with a migration hint.
#'
#'   Rows with `NA` in any covariate are dropped from the analytic
#'   sample for each outcome (complete-cases per outcome, matching the
#'   existing `by` / `weights` NA handling).
#' @param adjustment How the covariate-adjusted estimated marginal
#'   means (the `emmean` / `emmean_se` / `emmean_ci_*` columns) are
#'   computed when `covariates` is non-empty (typed without covariates,
#'   it is ignored with a warning, class `spicy_ignored_arg`). One of:
#'   - `"proportional"` (the default; matches Stata `margins` and
#'     [marginaleffects::avg_predictions()]): G-computation on the
#'     observed sample. For each focal level of `by`, the model
#'     predicts at every observation with `by` set to that level
#'     (covariates kept at their observed values), and the
#'     predictions are averaged. Population-weighted by construction
#'     -- the empirical joint distribution of covariates is the
#'     reference. Under `weights`, the averaging uses the case
#'     weights (the Stata `margins` convention after a weighted
#'     regression; equivalent to
#'     `marginaleffects::avg_predictions(wts = )`), so the reference
#'     distribution is the weighted one. Best when the goal is "what
#'     is the predicted mean in *this* population if everyone had
#'     `by = lvl`".
#'   - `"balanced"` (matches [emmeans::emmeans()] default and the
#'     SPSS UNIANOVA EMMEANS / SAS LSMEANS conventions): synthetic
#'     grid of factor-covariate level combinations x numeric
#'     covariates fixed at their sample mean, with each grid cell
#'     weighted equally. Treats the design as if covariates were
#'     balanced -- the "marginal mean assuming a balanced design"
#'     estimand. Best when the goal is to report a covariate-purified
#'     comparison independent of the empirical covariate distribution.
#'
#'   Both methods reduce to the same linear-contrast formula
#'   `emmean = avg_row %*% beta` and inherit the spicy variance
#'   pipeline (HC* / CR* / bootstrap / jackknife). They give the
#'   same answer when there are no covariates, and also when all
#'   covariates are numeric (fixed at their means either way). The
#'   two estimands diverge when a factor, character, or logical
#'   covariate has non-uniform observed proportions: those are
#'   levels to balance, and `"balanced"` weights them equally
#'   (a logical is a two-level factor to `lm()`, balanced
#'   FALSE/TRUE -- the `emmeans` convention).
#' @param exclude Columns to exclude from `select`. Supports tidyselect syntax
#'   and character vectors of column names.
#' @param regex Logical. If `FALSE` (the default), uses tidyselect helpers. If
#'   `TRUE`, the `select` argument is treated as a regular expression.
#' @param weights Optional case weights. Accepts:
#'   - `NULL` (default): an ordinary unweighted `lm()` is fit.
#'   - an **unquoted numeric column name** present in `data`.
#'   - a **single character column name** present in `data`.
#'   - a **numeric vector of length `nrow(data)`** evaluated in the calling
#'     environment.
#'
#'   Validation: weights must be finite, non-negative, and contain at least
#'   one positive value (otherwise the function errors). Rows with `NA` in
#'   `weights` are excluded from the analytic sample for each outcome,
#'   alongside rows with `NA` in `y` or `by`. A weights column is not an
#'   outcome: `select = everything()` skips it, and naming it in
#'   `select`, or giving its position, is an error. When supplied, weights are
#'   passed to `lm(..., weights = ...)`, so coefficients become weighted
#'   least-squares estimates and \eqn{R^2}{R^2}, adjusted \eqn{R^2}{R^2}, and the four effect
#'   sizes are computed from the corresponding weighted sums of squares
#'   (see the *Weights* section in Details).
#' @param vcov Variance estimator used for standard errors, confidence
#'   intervals, and Wald test statistics. One of:
#'   - `"classical"` (default): the ordinary OLS/WLS variance from
#'     `vcov(lm)`, which assumes homoscedastic errors.
#'   - `"HC0"`: the original Eicker--White heteroskedasticity-consistent
#'     sandwich estimator (White 1980), with no finite-sample correction.
#'   - `"HC1"`: HC0 multiplied by `n / (n - p)` (MacKinnon and White 1985).
#'     Matches Stata's `, robust` default.
#'   - `"HC2"`: residuals divided by `sqrt(1 - h_ii)`
#'     (MacKinnon and White 1985).
#'   - `"HC3"`: residuals divided by `(1 - h_ii)` (MacKinnon and White 1985).
#'     A common default for small to moderate samples (Long and Ervin 2000).
#'   - `"HC4"`: leverage-adaptive variant designed for influential
#'     observations (Cribari-Neto 2004).
#'   - `"HC4m"`: refinement of HC4 with a modified leverage exponent
#'     (Cribari-Neto and da Silva 2011).
#'   - `"HC5"`: alternative leverage-adaptive variant designed for
#'     leveraged data (Cribari-Neto, Souza and Vasconcellos 2007).
#'   - `"CR0"`, `"CR1"`, `"CR2"`, `"CR3"`: cluster-robust sandwich
#'     estimators for non-independent observations (Liang and Zeger
#'     1986); requires `cluster`. `"CR2"` is the modern default
#'     (Bell and McCaffrey 2002; Pustejovsky and Tipton 2018), with
#'     Satterthwaite degrees of freedom for inference; the
#'     fractional df is reported in the long output (in `df` beside a
#'     *t*, in `df.residual` beside an *F*) and in the `t(df)` /
#'     `F(df1, df2)` test header. `"CR1"` applies the
#'     G/(G-1) correction only; Stata's `, vce(cluster id)` uses the
#'     larger G(N-1)/((G-1)(N-p)) factor with t(G-1) inference
#'     (clubSandwich's `"CR1S"`; exposed for `lm` fits in
#'     [table_regression()], not here), so `"CR1"` does
#'     not reproduce Stata. Cluster-robust variants
#'     are dispatched to [clubSandwich::vcovCR()] and inference uses
#'     [clubSandwich::coef_test()] / [clubSandwich::Wald_test()];
#'     install `clubSandwich` to use them.
#'   - `"bootstrap"`: nonparametric (resampling cases) or cluster
#'     bootstrap variance, depending on whether `cluster` is supplied
#'     (Davison and Hinkley 1997; Cameron, Gelbach and Miller 2008).
#'     The number of replicates is set by `boot_n`. Inference is
#'     asymptotic (`z` for single contrasts, `chi^2(q)` for the global
#'     Wald test); CIs are Wald-type around the point estimate.
#'   - `"jackknife"`: leave-one-out variance, or leave-one-cluster-out
#'     when `cluster` is supplied (Quenouille 1956; MacKinnon and White
#'     1985). Inference is asymptotic (`z` / `chi^2(q)`).
#'
#'   The `HC*` variants are computed via [sandwich::vcovHC()].
#'   Coefficients (means, contrasts, slopes), \eqn{R^2}{R^2}, and the standardized
#'   effect sizes (`f2`, `cohens_d`, `hedges_g`, `omega2`) are point
#'   estimates from the
#'   OLS/WLS fit and are not affected by `vcov`; only their standard errors,
#'   CIs, and the test statistic of the contrast change.
#' @param cluster Cluster identifier for cluster-aware variance
#'   estimators. Required when `vcov` is one of the `CR*` variants;
#'   optional and triggers a cluster bootstrap or leave-one-cluster-out
#'   jackknife when `vcov` is `"bootstrap"` / `"jackknife"`; forbidden
#'   for the other (independent-observation) variants. Accepts:
#'   - `NULL` (default): no cluster structure.
#'   - an unquoted column name in `data`.
#'   - a single character column name in `data`.
#'   - a one-sided formula naming a column in `data` (`~ region`), the
#'     `sandwich` / `fixest` convention shared with [table_regression()].
#'   - an atomic vector of length `nrow(data)` evaluated in the calling
#'     environment (factor, character, integer, etc.).
#'
#'   Rows with `NA` in `cluster` are excluded from the analytic sample
#'   for each outcome (alongside rows with `NA` in `y`, `by`, or
#'   `weights`). A cluster column is not an outcome: `select =
#'   everything()` skips it, and naming it in `select`, or giving its
#'   position, is an error. At least two distinct non-missing cluster values are
#'   required. Multi-way clustering (a list / data.frame of multiple
#'   cluster vectors) is not supported; use [sandwich::vcovCL()] or
#'   [clubSandwich::vcovCR()] directly on the fitted model for that case.
#' @param boot_n Number of bootstrap replicates used when
#'   `vcov = "bootstrap"`: a positive whole number, as in
#'   [table_regression()] and [table_multiresponse()] (a fraction is an
#'   error). Defaults to `1000`, the default of [table_regression()],
#'   which computes the same bootstrap variance. That variance needs at
#'   least 10 valid replicates: a smaller `boot_n` is an error.
#'   Larger values reduce Monte-Carlo error in the bootstrap variance;
#'   typical values for inference are `500`-`2000`. Typed with another
#'   `vcov`, it is ignored with a warning (class `spicy_ignored_arg`).
#' @param contrast Contrast display for categorical predictors (typed with
#'   a numeric `by`, it is ignored with a warning, class
#'   `spicy_ignored_arg`). One of:
#'   - `"auto"` (default): show a single reference contrast, the second
#'     level minus the first, only when `by` has exactly two non-empty
#'     levels. The reference level is the **first level of the factor**
#'     (R's default treatment-contrast convention,
#'     `getOption("contrasts")[1]`). To change which level acts as the
#'     reference, re-level `by` upstream (for example with
#'     [forcats::fct_relevel()] or [stats::relevel()]).
#'   - `"none"`: suppress the contrast column for categorical predictors.
#'     Level-specific means are still displayed.
#' @param statistic Logical. If `TRUE`, includes a test-statistic column in the
#'   wide and rendered outputs. Defaults to `FALSE`.
#' @param p_value Logical or `NULL`. If `TRUE`, includes a `p` column in
#'   the wide and rendered outputs. `NULL` (the default) shows it whenever
#'   `by` is supplied, as in the rest of the family; `by` is required
#'   here, so `NULL` means `TRUE`. Pass `p_value = FALSE` to suppress it:
#'   `output = "long"`, `tidy()` and `glance()` then keep their columns
#'   with `p.value` `NA`, and `test_type`, `statistic`, `df` and
#'   `df.residual` too unless `statistic = TRUE`.
#' @param show_n Logical. If `TRUE`, includes an unweighted `n` column in the
#'   wide and rendered outputs. Defaults to `TRUE`.
#' @param show_weighted_n Logical. If `TRUE` and `weights` is supplied,
#'   includes a `Weighted n` column equal to the sum of case weights in the
#'   analytic sample. Defaults to `FALSE`.
#' @param effect_size Character. Effect-size column to include in the wide and
#'   rendered outputs (a logical is an error, class `spicy_defunct`). An
#'   estimator named after its author carries the name, as `"hedges_g"`
#'   does in [table_continuous()]: `"d"` and `"g"`, the spellings before
#'   0.14.0, are an error of the same class that gives the new one. One
#'   of:
#'   - `"none"` (the default): no effect-size column.
#'   - `"f2"`: Cohen's `f^2 = R^2 / (1 - R^2)`. Defined for any predictor type.
#'     Familiar from Cohen (1988); standard input for a-priori power analysis.
#'     Note that for a single-predictor model, \eqn{f^2}{f^2} is a monotone transform of
#'     \eqn{R^2}{R^2} and adds no information beyond it.
#'   - `"cohens_d"`: Cohen's `d = beta_hat / sigma_hat`, where `beta_hat` is
#'     the model coefficient (the displayed difference) and `sigma_hat` is the
#'     residual standard deviation from the fitted model. Defined only when
#'     `by` has exactly two non-empty levels; otherwise the function errors.
#'     The sign matches the displayed difference, the second level minus
#'     the first.
#'   - `"hedges_g"`: Hedges's `g = J * d` with the small-sample correction
#'     `J = 1 - 3 / (4 * df_resid - 1)`. Same domain as `"cohens_d"`.
#'   - `"omega2"`: Hays's `omega-squared`, a bias-corrected estimator of the
#'     population variance explained, less optimistic than \eqn{R^2}{R^2} for small
#'     samples. Defined for any predictor type and truncated at 0.
#'
#'   When `weights` is supplied, `"cohens_d"`, `"hedges_g"`, and `"omega2"`
#'   are derived from
#'   the weighted least-squares fit (using weighted sums of squares and the
#'   model's weighted residual standard deviation), keeping them consistent
#'   with the weighted contrast and its CI shown in the table. All effect
#'   sizes are point estimates derived from the OLS/WLS fit and are **not**
#'   affected by `vcov`.
#'
#'   **Under covariate adjustment** (`covariates` non-empty):
#'   - `"f2"` and `"omega2"` become the **partial** *\eqn{f^2}{f^2}* / partial *\eqn{\omega^2}{omega^2}*,
#'     derived from the partial *F* of `by` (the Type-II test of `by`
#'     after all covariates, equal to [stats::drop1()] in this
#'     additive setting) --
#'     the correctly-defined effect size when the model is adjusted.
#'     For numeric `by`, partial *\eqn{f^2}{f^2}* equals the squared partial
#'     correlation of `by` with the outcome, divided by `(1 - r^2_partial)`.
#'   - `"cohens_d"` and `"hedges_g"` raise a `spicy_unsupported` error:
#'     Cohen's *d* and Hedges's *g* have no canonical extension to adjusted
#'     models
#'     (the pooled SD is undefined under adjustment). Use `"f2"` or
#'     `"omega2"` instead -- both generalize via partial *F*.
#' @param effect_size_ci Logical. If `TRUE` and `effect_size != "none"`, adds
#'   a confidence interval for the effect size derived from inversion of the
#'   appropriate noncentral distribution (noncentral t for `"cohens_d"` /
#'   `"hedges_g"`; noncentral F for `"omega2"` / `"f2"`). The CI level is
#'   taken from
#'   `ci_level`. In the long output (`output = "long"`), the bounds are always
#'   present in `es_ci_lower` / `es_ci_upper` (numeric). In the wide raw
#'   output (`output = "data.frame"`), the bounds appear under the same
#'   names, `es_ci_lower` / `es_ci_upper` (numeric). In the printed ASCII
#'   table and rendered outputs (`"tinytable"`, `"gt"`, `"flextable"`,
#'   `"word"`, `"excel"`, `"clipboard"`), the effect-size column shows the
#'   value followed by the CI in brackets (e.g. `0.18 [0.07, 0.30]`), under
#'   a header that states its level (`d [95% CI]`). Defaults to `FALSE`.
#'   When `effect_size = "none"`, this argument is ignored with a warning.
#' @param r2 Character. Fit statistic to include in the wide and rendered
#'   outputs. One of:
#'   - `"r2"` (default): the model \eqn{R^2}{R^2} (`summary(lm)$r.squared`).
#'   - `"adj_r2"`: adjusted \eqn{R^2}{R^2}, penalizing for `df_effect` relative to the
#'     residual degrees of freedom.
#'   - `"none"`: omit the fit-statistic column.
#'
#'   When `weights` is supplied, \eqn{R^2}{R^2} and adjusted \eqn{R^2}{R^2} are the weighted
#'   least-squares versions reported by `summary(lm(..., weights = ...))`.
#' @param ci Logical. If `TRUE`, includes contrast confidence-interval columns
#'   in the wide and rendered outputs when a single contrast is shown.
#'   Defaults to `TRUE`.
#' @param labels How each variable is named in the table: in the rows,
#'   in the title, and in the notes, the covariates included. Four forms:
#'   * `labels = FALSE` shows the column name.
#'   * `labels = TRUE` shows the column's `label` attribute (as set by
#'     `haven`, `labelled`, or [label_from_names()]) when it is present,
#'     not `NA`, and not empty, and the column name otherwise.
#'   * `labels = c(bmi = "Body mass index")`, a named character vector,
#'     gives the columns it names these labels. Every other column
#'     follows `options(spicy.var_labels)`: its name when the option is
#'     unset or `FALSE`, its `label` attribute when it is `TRUE`.
#'   * `labels = var_labels(bmi = "BMI (kg/m2)")` gives the columns it
#'     names these labels, and every other column its `label` attribute,
#'     whatever the option says. `var_labels()` alone is `labels = TRUE`.
#'
#'   Names that match no column are ignored, so one vector can serve
#'   every table of a project. The default reads
#'   `options(spicy.var_labels)`, and a `TRUE` or `FALSE` typed in the
#'   call wins over it. Any other value, `NULL` included, is an error. A
#'   variable label documents the data file; a published table carries
#'   the wording you set here. `note_labels` gives a variable a shorter
#'   label in the title and the notes.
#' @param ci_level Confidence level for coefficient and model-based mean
#'   intervals (default: `0.95`): a number strictly between 0 and 1.
#' @param digits Number of decimal places of the numbers on each outcome's
#'   own scale: the means of the groups, adjusted or not, the difference or
#'   the slope, and its confidence interval. One non-negative whole number
#'   sets them for every outcome (default: `2`), and the test statistic and
#'   the weighted *n* with them. A vector named by column, not by label,
#'   gives each outcome it names its own, `digits = c(height = 0, bmi = 1)`;
#'   the other outcomes, the test statistic and the weighted *n* keep the
#'   default, the `digits` of a [spicy_style()] in force included. Names
#'   that match no outcome are ignored, as in `labels`. A fraction (`2.9`)
#'   is an error, as for `fit_digits` and `effect_size_digits`. The
#'   *p*-value (`p_digits`), the \eqn{R^2}{R^2} (`fit_digits`), the effect
#'   size (`effect_size_digits`) and the counts keep their own decimals.
#' @param fit_digits Number of decimal places for model-fit columns (\eqn{R^2}{R^2} or
#'   adjusted \eqn{R^2}{R^2}) in wide and rendered outputs (default: `2`).
#' @param effect_size_digits Number of decimal places for the effect-size
#'   column (`f2`, `d`, `g`, or `omega2`) in wide and rendered outputs
#'   (default: `2`).
#' @param p_digits Integer >= 1. Number of decimal places used to render
#'   *p*-values in the `p` column (default: `3`, the APA Publication
#'   Manual standard). Both the displayed precision and the
#'   small-*p* threshold derive from this argument: `p_digits = 3`
#'   prints `.045` and `<.001`; `p_digits = 4` prints `.0451` and
#'   `<.0001`; `p_digits = 2` prints `.05` and `<.01`. Useful for
#'   genomics / GWAS contexts where adjusted *p*-values can be very
#'   small, or for journals using a coarser convention. Leading zeros
#'   are always stripped, following APA convention. Values below `1`
#'   and fractions (`3.7`) raise an error.
#' @param decimal_mark The decimal mark, a single character: `"."` (the
#'   default) or `","` in most styles; the Lancet style of
#'   [spicy_style()] uses the middle dot.
#' @param align Horizontal alignment of numeric columns in the printed
#'   ASCII table and in the `tinytable`, `gt`, `flextable`, `word`, and
#'   `clipboard` outputs. The first column (`Variable`) is always
#'   left-aligned. One of:
#'   - `"decimal"` (default): align numeric columns on the decimal
#'     mark, the standard scientific-publication convention used by
#'     SPSS, SAS, and LaTeX `siunitx`. Numeric cells are pre-padded
#'     with figure-spaces (U+2007, digit-width) so every string in a
#'     column has the same width with the decimal mark at the same
#'     internal position; centering those uniform-width strings then
#'     stacks the decimal points vertically. The same pad-then-center
#'     strategy is applied on every rendering engine (`gt`,
#'     `tinytable`, `flextable`, `word`, ASCII print) for a
#'     homogeneous rendering, matching `table_regression()`. The
#'     `clipboard` output is delimited text meant to be parsed rather
#'     than read at a fixed width, so its cells travel unpadded (a
#'     padded number pastes as text next to an unpadded number).
#'   - `"center"`: center-align all numeric columns.
#'   - `"right"`: right-align all numeric columns.
#'
#'   The `excel` output uses the engine's default alignment in any
#'   case: cell-string padding does not align decimals under
#'   proportional fonts, and writing raw numbers with a numeric
#'   format would require a separate refactor.
#' @param output Output format. One of:
#'   - `"default"`: an ASCII table object, printed when the call is bare
#'   - `"data.frame"`: the displayed table with its numbers, one row per
#'     outcome
#'   - `"long"`: one row per level of `by` and outcome (one row per
#'     outcome with a numeric `by`), with every estimate
#'   - `"tinytable"` (requires `tinytable`)
#'   - `"gt"` (requires `gt`)
#'   - `"flextable"` (requires `flextable`)
#'   - `"excel"` (requires `openxlsx2`)
#'   - `"clipboard"` (requires `clipr`)
#'   - `"word"` (requires `flextable` and `officer`)
#' @param excel_path File path for `output = "excel"`, ending in
#'   `".xlsx"`, in a directory that exists.
#' @param excel_sheet Sheet name for `output = "excel"`. `NULL` (the
#'   default) uses `"Linear models"`.
#' @param clipboard_delim Delimiter for `output = "clipboard"` (default:
#'   `"\t"`). A cell holding the delimiter itself, a double quote or a
#'   line break is quoted RFC 4180-style, so the grid survives whatever
#'   delimiter you choose.
#' @param word_path File path for `output = "word"`, ending in `".docx"`,
#'   in a directory that exists.
#' @param verbose Logical. If `TRUE`, a message (class `spicy_info`)
#'   names the non-numeric columns left out of the outcomes (default:
#'   `FALSE`).
#' @param user_na Logical. If `TRUE` (the default), declared missing
#'   values in the outcomes, in `by`, and in `covariates` are treated
#'   as missing and excluded from the fitted models (reflected in the
#'   per-group `n`). If `FALSE`, the declared codes enter the fits as
#'   ordinary values. See the "Declared missing values" section of
#'   [freq()].
#'
#' @param style A journal style: a theme name (`"jama"`, `"nejm"`,
#'   `"lancet"`, `"annals"`, `"apa"`, `"aer"`), a
#'   [spicy_style()] object, or `NULL` (the default). A style only
#'   changes DEFAULTS -- any argument you pass explicitly wins over it.
#'   Set `options(spicy.style = )` for document-wide scope. A theme
#'   covers numeric formatting, the word that opens the note and the
#'   abbreviations the note defines, not full editorial conformity;
#'   `?spicy_style` lists the exact rules each one encodes and the
#'   official document they come from. An unknown name is an error.
#' @inheritParams table_categorical
#'
#' @inheritSection freq Declared missing values
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: the compute frame with class
#'     `"spicy_continuous_lm_table"` / `"spicy_table"`, carrying the
#'     rendering metadata as attributes (the `by` column and its label as
#'     `group_var` and `group_label`, the title as printed as `title`).
#'     The object is
#'     returned visibly, so a bare `table_continuous_lm(...)` call
#'     auto-prints the styled ASCII table at the console while
#'     `t <- table_continuous_lm(...)` stays silent (print `t` to
#'     display the table).
#'   \item `"data.frame"`: the displayed table with its numbers, what
#'     [as.data.frame()][as.data.frame.spicy_continuous_lm_table] of the
#'     default object returns: one row per outcome, the key columns
#'     `variable` (the column name of the outcome) and `label` (the label
#'     displayed), then the printed columns under their names -- the
#'     means (categorical `by`) or the slope (numeric `by`), the contrast
#'     and its interval bounds, the test statistic, `p`, the fit
#'     statistic, the effect size (with `es_ci_lower` / `es_ci_upper`
#'     beside it when `effect_size_ci = TRUE`), `n`, and `Weighted n`.
#'   \item `"long"`: one block per outcome, one row per level of `by`
#'     (one row, `estimate_type` `"slope"` and `level` `NA`, with a
#'     numeric `by`), with every estimate: identification (`variable`,
#'     `label`,
#'     `predictor_type`, `predictor_label`, `level`, `reference`),
#'     fitted means and their CI (`emmean`, `emmean_se`, `emmean_ci_lower`,
#'     `emmean_ci_upper`), contrast or slope estimates and CI
#'     (`estimate_type`, `estimate`, `estimate_se`, `estimate_ci_lower`,
#'     `estimate_ci_upper`), inferential output (`test_type`, `statistic`,
#'     `df`, `df.residual`, `p.value`: the *F* of a factor has both
#'     degrees of freedom, the *t* of a contrast or a slope has its own in
#'     `df` and none in `df.residual`), effect size with its CI
#'     (`es_type`, `es_value`, `es_ci_lower`, `es_ci_upper`), fit
#'     (`r.squared`, `adj.r.squared`, on the first row of an outcome),
#'     the size of the model (`nobs`, `weighted_nobs`, repeated on each
#'     row of an outcome), and `boot_n_valid`, the number of bootstrap
#'     replicates whose refit gave every coefficient (`NA` without
#'     `vcov = "bootstrap"`).
#'   \item `"tinytable"`: a `tinytable` object.
#'   \item `"gt"`: a `gt_tbl` object.
#'   \item `"flextable"`: a `flextable` object.
#'   \item `"excel"` / `"word"`: writes to disk and returns the file path
#'     invisibly.
#'   \item `"clipboard"`: copies the wide table and returns it invisibly.
#' }
#'
#' The Excel sheet carries the same title the console prints on its
#' first row; the table itself starts on row 3, and the note lines sit
#' below the body. The default object and the data frames of
#' `"data.frame"` and `"long"` carry the whole note as printed, its
#' lead-in included, in `attr(x, "note")` (`NULL` when there is nothing to
#' say).
#'
#' If no numeric outcome columns remain after applying `select`, `exclude`,
#' and `regex`, the function stops with an error (class
#' `spicy_invalid_input`), as the other tables of the family do.
#'
#' @details
#' # Model and outputs
#'
#' `table_continuous_lm()` is designed for article-style reporting around
#' a single focal predictor: one model per selected continuous outcome,
#' fitted as `lm(outcome ~ by, ...)` and optionally extended with case
#' `weights` and additive `covariates` (`lm(outcome ~ by + cov1 + ...)`).
#' For categorical `by`, the reported means are model-based fitted means
#' (or covariate-adjusted estimated marginal means; see `adjustment`) for
#' each level, and contrasts come from the same fitted linear model. For
#' an unweighted `lm(y ~ factor)` with classical variance and no
#' covariates, the fitted means coincide numerically with empirical
#' subgroup means; the *model-based* qualifier matters because (a) under
#' `weights` the means become weighted least-squares estimates, (b) their
#' CIs derive from the model `vcov` (classical, `HC*`, `CR*`,
#' bootstrap or jackknife), (c) under `covariates` they become
#' adjusted marginal means, and (d) tests, *p*-values and effect sizes
#' all come from the same fitted model, keeping the table internally
#' consistent.
#'
#' Compared with [table_continuous()], this function is the model-based
#' companion: choose it when you want heteroskedasticity-consistent standard
#' errors (`vcov = "HC*"`), model fit statistics, or case weights via
#' `lm(..., weights = ...)`. Because the function exists to report a fitted
#' model, its inferential output is on by default: the *p*-value is shown
#' (`p_value = NULL`, the default, shows it with `by`, which is required
#' here) and `r2 = "r2"`; set `p_value = FALSE` or `r2 = "none"` to
#' suppress them.
#'
#' An outcome with no variance in its fitted sample (every observation the
#' same value) leaves nothing to infer, as in [table_continuous()]: its
#' test, its interval, its effect size and its \eqn{R^2}{R^2} are undefined
#' cells (`NA` in the long output and in `glance()`), its means and its
#' contrast or slope (0) are shown, and a warning (class
#' `spicy_undefined_stat`) names it. So does a residual variance of zero,
#' an outcome constant within each group of a categorical `by` or an exact
#' linear function of `by` and the covariates: its means (adjusted for the
#' covariates), its contrast or slope and its \eqn{R^2}{R^2} of 1 are
#' shown, and its test, interval and effect size are undefined. Every cell
#' of an outcome applies to it, so a cell with no value shows the undefined
#' mark, never a blank: a saturated fit (one observation per group) has no
#' interval, test or effect size, and a model that could not be fit has no
#' cell at all.
#'
#' # Effect sizes
#'
#' Effect size is selected explicitly via `effect_size` (defaults to
#' `"none"`). All variants are derived from the same fitted model as the
#' displayed coefficients, \eqn{R^2}{R^2}, and CIs, so the effect size stays
#' internally consistent with the rest of the table.
#'
#' \itemize{
#'   \item `"f2"`: Cohen's `f^2 = R^2 / (1 - R^2)` (Cohen 1988). Defined
#'     for any predictor type. For a single-predictor model, \eqn{f^2}{f^2} is a
#'     monotone transform of \eqn{R^2}{R^2} and adds no information beyond it; its
#'     primary use is in *a priori* power analysis (e.g. G*Power).
#'   \item `"cohens_d"`, `"hedges_g"`: standardized mean difference
#'     (Cohen's *d* or Hedges's *g*), defined only when `by` has exactly two
#'     non-empty levels.
#'     `d = beta_hat / sigma_hat` with `sigma_hat = summary(fit)$sigma` (the
#'     pooled within-group SD for the unweighted two-group case);
#'     `g = J * d` with `J = 1 - 3 / (4 * df_resid - 1)` (Hedges and Olkin
#'     1985). The sign matches the displayed difference, the second level
#'     minus the first.
#'     For published reports of two-group comparisons, *g* is the
#'     convention recommended by Hedges and Olkin (1985).
#'   \item `"omega2"`: Hays's \eqn{\omega^2}, computed from weighted
#'     sums of squares as `(SS_effect - df_effect * MSE) / (SS_total
#'     + MSE)` and truncated at 0 for small or null effects (Hays
#'     1963; Olejnik and Algina 2003). Less biased than
#'     \eqn{\eta^2} (which equals `R^2` in this single-predictor
#'     design) and recommended for reporting variance explained in
#'     ANOVA-style designs (Olejnik and Algina 2003).
#' }
#'
#' All four effect sizes are point estimates derived from the OLS/WLS fit
#' and are **invariant to `vcov`**: choosing `HC*` changes the SE, CI, and
#' test statistic of the contrast but not the standardized magnitude
#' itself.
#'
#' **Under covariate adjustment** (`covariates` non-empty), `"f2"` and
#' `"omega2"` become the partial *\eqn{f^2}{f^2}* / partial *\eqn{\omega^2}{omega^2}* of `by`, derived
#' from the partial *F* restricted to the focal term (the Type-II
#' test of `by` after all covariates, equal to [stats::drop1()] in
#' this additive setting). `"cohens_d"` and `"hedges_g"` raise a
#' `spicy_unsupported` error: the pooled
#' standard deviation has no canonical extension under adjustment, so
#' Cohen's *d* and Hedges's *g* are undefined for adjusted models. See
#' `effect_size` for the full dispatch.
#'
#' Confidence intervals for the effect size are available via
#' `effect_size_ci = TRUE` and use the modern noncentral-distribution
#' inversion approach, the consensus standard in commercial statistical
#' software (Stata `esize` / `estat esize`, SAS `PROC TTEST` and
#' `PROC GLM EFFECTSIZE` 14.2+) and in mainstream R packages
#' (`effectsize`, `MOTE`, `TOSTER`, `effsize`):
#' \itemize{
#'   \item `"cohens_d"`, `"hedges_g"`: noncentral *t* inversion (Steiger
#'     and Fouladi 1997;
#'     Goulet-Pelletier and Cousineau 2018). Empirical coverage is nominal
#'     across sample sizes (Fitts 2021), unlike the
#'     older Hedges-Olkin normal approximation which is biased for small
#'     samples. For Hedges's *g* the bounds inherit the *J* small-sample
#'     correction.
#'   \item `"omega2"`, `"f2"`: noncentral *F* inversion (Steiger 2004).
#'     Without covariates (model-level effect sizes), bounds are
#'     converted from the noncentrality parameter using
#'     `omega^2 = ncp / (ncp + N)` and `f^2 = ncp / N` respectively, with
#'     `N = df1 + df2 + 1` (total sample size). Under covariate
#'     adjustment (partial effect sizes), the bounds use the partial
#'     transforms instead: partial `f^2 = ncp / df2`, and for partial
#'     `omega^2` the inversion runs at the *F*-value equivalent of the
#'     omega-squared point estimate with bounds `ncp / (ncp + df2)` --
#'     the [effectsize::omega_squared()] `partial = TRUE` convention,
#'     which is narrower than the partial eta-squared CI because
#'     omega-squared shrinks the point estimate.
#' }
#' For the weighted case, the CI uses raw (unweighted) group counts and
#' `df.residual(fit) = n - p`, consistent with the WLS reporting convention
#' (DuMouchel and Duncan 1983). For propensity-score balance assessment or
#' complex-survey designs, dedicated packages ([cobalt::bal.tab()] for the
#' Austin and Stuart 2015 formulation; `survey` for design-based effect
#' sizes) are more appropriate.
#'
#' # Robust standard errors
#'
#' When `vcov` is one of the `HC*` variants, the standard errors, CIs, and
#' Wald test statistics use a heteroskedasticity-consistent sandwich
#' estimator computed via [sandwich::vcovHC()] (Zeileis 2004), the
#' canonical R implementation. For a brief guide:
#' \itemize{
#'   \item `"HC0"` is the original White (1980) form; `"HC1"` adds the
#'     `n / (n - p)` correction (MacKinnon and White 1985), Stata's
#'     `, robust` default.
#'   \item `"HC2"` and `"HC3"` use leverage-based residual rescalings
#'     (MacKinnon and White 1985); `"HC3"` is the [sandwich::vcovHC()]
#'     default for small to moderate samples (Long and Ervin 2000).
#'   \item `"HC4"` adapts the leverage exponent for influential
#'     observations (Cribari-Neto 2004); `"HC4m"` is a modified-exponent
#'     refinement (Cribari-Neto and da Silva 2011); `"HC5"` is an
#'     alternative leverage-adaptive variant (Cribari-Neto, Souza and
#'     Vasconcellos 2007).
#' }
#'
#' When observations are not independent (repeated measurements per
#' individual, students nested in classes, patients in hospitals,
#' country-year panels), classical and `HC*` standard errors are biased
#' downward. Use the `CR*` variants together with `cluster = id_var` to
#' get cluster-robust inference (Liang and Zeger 1986). The
#' implementation dispatches to [clubSandwich::vcovCR()] for the
#' variance and to [clubSandwich::coef_test()] (single-coefficient,
#' Satterthwaite *t*) and [clubSandwich::Wald_test()] (multi-coefficient
#' Hotelling-T-squared with Satterthwaite df, "HTZ") for inference.
#' `"CR2"` (Bell and McCaffrey 2002; Pustejovsky and Tipton 2018) is the
#' modern recommended default; it generally produces fractional
#' Satterthwaite degrees of freedom (in `df` beside a *t*, in
#' `df.residual` beside an *F*), which the displayed `t(df)` /
#' `F(df1, df2)` header renders to one decimal. `"CR1"`
#' applies the G/(G-1) correction only -- Stata's `, vce(cluster id)`
#' uses the larger G(N-1)/((G-1)(N-p)) factor with t(G-1) inference
#' (clubSandwich's `"CR1S"`; exposed for `lm` fits in
#' [table_regression()], not here), so `"CR1"` does not
#' reproduce Stata. Effect sizes remain invariant
#' to `vcov` (including `CR*`); only the SE, CI, test statistic, and
#' degrees of freedom of the contrast change.
#'
#' Two resampling-based estimators are also available without adding
#' any dependency: `vcov = "bootstrap"` (nonparametric resampling-cases
#' bootstrap; Davison and Hinkley 1997) and `vcov = "jackknife"`
#' (leave-one-out delete-1; Quenouille 1956; MacKinnon and White 1985).
#' Supplying `cluster` switches both to their cluster-aware variants
#' (cluster bootstrap, Cameron, Gelbach and Miller 2008;
#' leave-one-cluster-out jackknife). The number of bootstrap replicates
#' is controlled by `boot_n` (default `1000`); replicates that fail to
#' fit on rank-deficient resamples are dropped, with an explicit warning
#' if more than half fail. Fewer than 10 valid bootstrap replicates (or
#' fewer than 2 jackknife leave-outs) raises `spicy_resampling_failed`
#' rather than silently reporting a different variance estimator.
#' Inference for both estimators is
#' asymptotic (`z` for single-coefficient contrasts, `chi^2(q)` for the
#' multi-coefficient global Wald test on `k > 2` categorical
#' predictors), reflected in the displayed test header. Use the
#' bootstrap when the residual distribution is non-standard or the
#' sample is small; use the jackknife as a closed-form, deterministic
#' alternative.
#'
#' \eqn{R^2}{R^2}, adjusted \eqn{R^2}{R^2}, and the effect sizes remain ordinary
#' least-squares (or weighted least-squares) statistics regardless of
#' `vcov`.
#'
#' # Weights
#'
#' When `weights` is supplied, `table_continuous_lm()` fits weighted
#' linear models via `lm(..., weights = ...)`. Means become weighted
#' least-squares estimates and contrasts and slopes are weighted. The
#' fit statistics \eqn{R^2}{R^2} and adjusted \eqn{R^2}{R^2}, as well as Hays's `omega^2`
#' and Cohen's \eqn{f^2}{f^2}, use the corresponding **weighted sums of squares**
#' from the WLS fit. Cohen's `d` and Hedges's `g` use the **WLS
#' coefficient and the model's weighted residual standard deviation**
#' (`summary(fit)$sigma`), which is the standard convention for
#' case-weighted regression-style reporting (DuMouchel and Duncan
#' 1983); the noncentral *t* CI for `d` / `g` uses the raw (unweighted)
#' group counts and the residual degrees of freedom of the WLS fit
#' (`n - p`). This case-weighted workflow is appropriate for weighted
#' article tables, but is **not** a substitute for a full complex-survey
#' design (see e.g. the `survey` package), nor for propensity-score
#' balance assessment under the Austin and Stuart (2015) convention
#' (see e.g. [cobalt::bal.tab()]).
#'
#' The `n` column always reports the unweighted analytic sample size for
#' each outcome. When `show_weighted_n = TRUE`, an additional
#' `Weighted n` column reports the sum of case weights in the same
#' analytic sample.
#'
#' # Display conventions
#'
#' With a categorical `by`, the headers have two levels (decked heads, APA
#' Publication Manual, 7th ed., section 7.12): the mean symbol `M` spans
#' the columns of the fitted means, each headed by its level alone, in
#' reference-level order, in the console as in every rendering engine.
#' For a dichotomous `by`, the contrast column is headed
#' \eqn{\Delta}{Delta} alone, and a sentence of the note, after those on
#' the test and the standard errors, gives the order of the subtraction:
#' the second level minus the first, written with the minus sign. Its
#' confidence interval keeps its spanner, `95% CI` over `LL` and `UL`,
#' which the console folds into the headers `95% CI LL` and `95% CI UL`,
#' as in the other tables. In `output = "data.frame"`, a mean keeps both
#' levels in its name, `M (<level>)`, and the contrast is named
#' \eqn{\Delta}{Delta} (the Greek capital delta, U+0394). For categorical
#' predictors with more than two levels, no single contrast or contrast
#' CI is shown in the wide outputs; instead, the table reports
#' level-specific means plus the overall `F` test when
#' `statistic = TRUE` (or `F(df1, df2)` when the degrees of freedom are
#' constant across outcomes).
#'
#' When `covariates` is non-empty, the printed ASCII table appends an
#' APA-style footer naming the covariates and the chosen estimand, e.g.
#' `Note. Adjusted for age, education (proportional).` With p-values, the
#' note also names the test behind them, read off each outcome's model:
#' `P-values: t-test from the linear model.` (the F-test for a `by` with
#' more than two levels, the t-test of the slope for a numeric `by`), in
#' the singular, `P-value:`, when the table shows one.
#'
#' The rendering engines carry that same footer as a table note. On the
#' `"tinytable"` route the note is set one size down;
#' `options(spicy.note_style)` governs that (see [table_regression()]).
#'
#' Optional output engines require the corresponding suggested packages:
#' \itemize{
#'   \item \pkg{tinytable} for `output = "tinytable"`
#'   \item \pkg{gt} for `output = "gt"`
#'   \item \pkg{flextable} for `output = "flextable"`
#'   \item \pkg{flextable} + \pkg{officer} for `output = "word"`
#'   \item \pkg{openxlsx2} for `output = "excel"`
#'   \item \pkg{clipr} for `output = "clipboard"`
#' }
#'
#' @references
#' Austin, P. C., & Stuart, E. A. (2015).
#'   Moving towards best practice when using inverse probability of
#'   treatment weighting (IPTW) using the propensity score to estimate
#'   causal treatment effects in observational studies.
#'   *Statistics in Medicine*, **34**(28), 3661--3679.
#'   \doi{10.1002/sim.6607}
#'
#' Bell, R. M., & McCaffrey, D. F. (2002).
#'   Bias reduction in standard errors for linear regression with
#'   multi-stage samples. *Survey Methodology*, **28**(2), 169--181.
#'
#' Cameron, A. C., Gelbach, J. B., & Miller, D. L. (2008).
#'   Bootstrap-based improvements for inference with clustered errors.
#'   *Review of Economics and Statistics*, **90**(3), 414--427.
#'   \doi{10.1162/rest.90.3.414}
#'
#' Cohen, J. (1988).
#'   *Statistical Power Analysis for the Behavioral Sciences*
#'   (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum.
#'
#' Cribari-Neto, F. (2004).
#'   Asymptotic inference under heteroskedasticity of unknown form.
#'   *Computational Statistics & Data Analysis*, **45**(2), 215--233.
#'   \doi{10.1016/S0167-9473(02)00366-3}
#'
#' Cribari-Neto, F., Souza, T. C., & Vasconcellos, K. L. P. (2007).
#'   Inference under heteroskedasticity and leveraged data.
#'   *Communications in Statistics -- Theory and Methods*,
#'   **36**(10), 1877--1888.
#'   \doi{10.1080/03610920601126589}
#'
#' Cribari-Neto, F., & da Silva, W. B. (2011).
#'   A new heteroskedasticity-consistent covariance matrix estimator
#'   for the linear regression model.
#'   *AStA Advances in Statistical Analysis*, **95**(2), 129--146.
#'   \doi{10.1007/s10182-010-0141-2}
#'
#' Davison, A. C., & Hinkley, D. V. (1997).
#'   *Bootstrap Methods and Their Application*.
#'   Cambridge: Cambridge University Press.
#'   \doi{10.1017/CBO9780511802843}
#'
#' DuMouchel, W. H., & Duncan, G. J. (1983).
#'   Using sample survey weights in multiple regression analyses of
#'   stratified samples. *Journal of the American Statistical
#'   Association*, **78**(383), 535--543.
#'   \doi{10.1080/01621459.1983.10478006}
#'
#' Fitts, D. A. (2021).
#'   Expected and empirical coverages of different methods for generating
#'   noncentral *t* confidence intervals for a standardized mean
#'   difference. *Behavior Research Methods*, **53**(6), 2412--2429.
#'   \doi{10.3758/s13428-021-01550-4}
#'
#' Goulet-Pelletier, J.-C., & Cousineau, D. (2018).
#'   A review of effect sizes and their confidence intervals, Part I:
#'   The Cohen's *d* family. *The Quantitative Methods for Psychology*,
#'   **14**(4), 242--265. \doi{10.20982/tqmp.14.4.p242}
#'
#' Hays, W. L. (1963).
#'   *Statistics for Psychologists*. New York: Holt, Rinehart and Winston.
#'
#' Hedges, L. V., & Olkin, I. (1985).
#'   *Statistical Methods for Meta-Analysis*. Orlando, FL: Academic Press.
#'
#' Long, J. S., & Ervin, L. H. (2000).
#'   Using heteroscedasticity consistent standard errors in the linear
#'   regression model. *The American Statistician*, **54**(3), 217--224.
#'   \doi{10.1080/00031305.2000.10474549}
#'
#' Liang, K.-Y., & Zeger, S. L. (1986).
#'   Longitudinal data analysis using generalized linear models.
#'   *Biometrika*, **73**(1), 13--22.
#'   \doi{10.1093/biomet/73.1.13}
#'
#' MacKinnon, J. G., & White, H. (1985).
#'   Some heteroskedasticity-consistent covariance matrix estimators with
#'   improved finite sample properties.
#'   *Journal of Econometrics*, **29**(3), 305--325.
#'   \doi{10.1016/0304-4076(85)90158-7}
#'
#' Olejnik, S., & Algina, J. (2003).
#'   Generalized eta and omega squared statistics: Measures of effect
#'   size for some common research designs.
#'   *Psychological Methods*, **8**(4), 434--447.
#'   \doi{10.1037/1082-989X.8.4.434}
#'
#' Pustejovsky, J. E., & Tipton, E. (2018).
#'   Small-sample methods for cluster-robust variance estimation and
#'   hypothesis testing in fixed effects models.
#'   *Journal of Business & Economic Statistics*, **36**(4), 672--683.
#'   \doi{10.1080/07350015.2016.1247004}
#'
#' Quenouille, M. H. (1956).
#'   Notes on bias in estimation. *Biometrika*, **43**(3/4), 353--360.
#'   \doi{10.1093/biomet/43.3-4.353}
#'
#' Steiger, J. H. (2004).
#'   Beyond the *F* test: Effect size confidence intervals and tests of
#'   close fit in the analysis of variance and contrast analysis.
#'   *Psychological Methods*, **9**(2), 164--182.
#'   \doi{10.1037/1082-989X.9.2.164}
#'
#' Steiger, J. H., & Fouladi, R. T. (1997).
#'   Noncentrality interval estimation and the evaluation of statistical
#'   models. In L. L. Harlow, S. A. Mulaik, & J. H. Steiger (Eds.),
#'   *What if there were no significance tests?* (pp. 221--257).
#'   Mahwah, NJ: Lawrence Erlbaum.
#'
#' White, H. (1980).
#'   A heteroskedasticity-consistent covariance matrix estimator and a
#'   direct test for heteroskedasticity.
#'   *Econometrica*, **48**(4), 817--838.
#'   \doi{10.2307/1912934}
#'
#' Zeileis, A. (2004).
#'   Econometric computing with HC and HAC covariance matrix estimators.
#'   *Journal of Statistical Software*, **11**(10), 1--17.
#'   \doi{10.18637/jss.v011.i10}
#'
#' @family spicy tables
#' @seealso [table_continuous()], [table_categorical()].
#'   For broader workflows on the same statistical building blocks:
#'   [sandwich::vcovHC()] (the canonical R implementation of the `HC*`
#'   sandwich estimators, used internally for `vcov = "HC*"`);
#'   [clubSandwich::vcovCR()], [clubSandwich::coef_test()] and
#'   [clubSandwich::Wald_test()] (the canonical R implementation of
#'   cluster-robust variance and Satterthwaite-style inference, used
#'   internally for `vcov = "CR*"`); [effectsize::cohens_d()],
#'   [effectsize::hedges_g()], and [effectsize::omega_squared()]
#'   (alternative effect-size computations and CIs); [cobalt::bal.tab()]
#'   for propensity-score covariate balance with weighted standardized
#'   mean differences (Austin and Stuart 2015); the
#'   [`survey`](https://CRAN.R-project.org/package=survey) package for
#'   design-based inference on complex-survey samples.
#'
#' @examples
#' # --- Basic usage ---------------------------------------------------------
#'
#' # Default: ASCII table with model-based means, p, and R-squared.
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex
#' )
#'
#' # --- Effect sizes -------------------------------------------------------
#'
#' # Cohen's d (binary by required).
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   effect_size = "cohens_d"
#' )
#'
#' # Hedges' g with weighted analysis and weighted n column.
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   weights = weight,
#'   statistic = TRUE,
#'   effect_size = "hedges_g",
#'   show_weighted_n = TRUE
#' )
#'
#' # Hedges' g with noncentral t confidence interval (bracket notation).
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   effect_size = "hedges_g",
#'   effect_size_ci = TRUE
#' )
#'
#' # Cohen's f-squared alongside R-squared (familiar power-analysis effect size).
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   effect_size = "f2"
#' )
#'
#' # Hays's omega-squared for a 3-level predictor ("cohens_d" and
#' # "hedges_g" need two groups).
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = education,
#'   effect_size = "omega2"
#' )
#'
#' # --- Robust SE for a numeric predictor ----------------------------------
#'
#' # HC3 standard errors for the slope of a continuous predictor.
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = age,
#'   vcov = "HC3",
#'   ci = FALSE
#' )
#'
#' # Cluster-robust SE for repeated-measures data: the `sleep` dataset
#' # has 10 subjects measured twice (one observation per group).
#' if (requireNamespace("clubSandwich", quietly = TRUE)) {
#'   table_continuous_lm(
#'     sleep,
#'     select = extra,
#'     by = group,
#'     cluster = ID,
#'     vcov = "CR2"
#'   )
#' }
#'
#' # --- Covariate adjustment ----------------------------------------------
#'
#' # Adjust the comparison of `wellbeing_score` and `bmi` by `sex` for `age`
#' # and `education`. The footer surfaces the adjustment estimand
#' # ("proportional" by default = G-computation, matching Stata `margins`).
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   covariates = c(age, education),
#'   vcov = "HC3"
#' )
#'
#' # Same model with the emmeans / SPSS UNIANOVA convention (equal-weight
#' # marginal means on a synthetic covariate grid).
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   covariates = c(age, education),
#'   adjustment = "balanced",
#'   vcov = "HC3"
#' )
#'
#' # Effect sizes adjust automatically: f2 / omega2 become partial
#' # effect sizes via the partial F restricted to the focal `by`.
#' # d / g are undefined under adjustment and raise spicy_unsupported.
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   covariates = c(age, education),
#'   effect_size = "f2",
#'   effect_size_ci = TRUE
#' )
#'
#' # --- Article-style polish -----------------------------------------------
#'
#' # Pretty outcome labels and adjusted R-squared.
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   labels = c(
#'     wellbeing_score = "WHO-5 wellbeing (0-100)",
#'     bmi = "Body-mass index (kg/m^2)"
#'   ),
#'   r2 = "adj_r2"
#' )
#'
#' # European decimal comma.
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   decimal_mark = ","
#' )
#'
#' # Regex selection of all columns starting with "life_sat".
#' table_continuous_lm(
#'   sochealth,
#'   select = "^life_sat",
#'   by = sex,
#'   regex = TRUE
#' )
#'
#' # --- Output formats -----------------------------------------------------
#'
#' # The rendered outputs below all wrap the same call:
#' #   table_continuous_lm(sochealth,
#' #                       select = c(wellbeing_score, bmi),
#' #                       by = sex)
#' # only `output` changes. Assign to a variable to avoid the
#' # console-friendly text fallback that some engines fall back to
#' # when printed directly in `?` help.
#'
#' # Wide data.frame (one row per outcome).
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   output = "data.frame"
#' )
#'
#' # Raw long data.frame (one block per outcome).
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   output = "long"
#' )
#'
#' \donttest{
#' # Rendered HTML / docx objects -- best viewed inside a
#' # Quarto / R Markdown document or a pkgdown article.
#' if (requireNamespace("tinytable", quietly = TRUE)) {
#'   tt <- table_continuous_lm(
#'     sochealth, select = c(wellbeing_score, bmi), by = sex,
#'     output = "tinytable"
#'   )
#' }
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   tbl <- table_continuous_lm(
#'     sochealth, select = c(wellbeing_score, bmi), by = sex,
#'     output = "gt"
#'   )
#' }
#' if (requireNamespace("flextable", quietly = TRUE)) {
#'   ft <- table_continuous_lm(
#'     sochealth, select = c(wellbeing_score, bmi), by = sex,
#'     output = "flextable"
#'   )
#' }
#'
#' # Excel and Word: write to a temporary file.
#' if (requireNamespace("openxlsx2", quietly = TRUE)) {
#'   tmp <- tempfile(fileext = ".xlsx")
#'   table_continuous_lm(
#'     sochealth, select = c(wellbeing_score, bmi), by = sex,
#'     output = "excel", excel_path = tmp
#'   )
#'   unlink(tmp)
#' }
#' if (
#'   requireNamespace("flextable", quietly = TRUE) &&
#'     requireNamespace("officer", quietly = TRUE)
#' ) {
#'   tmp <- tempfile(fileext = ".docx")
#'   table_continuous_lm(
#'     sochealth, select = c(wellbeing_score, bmi), by = sex,
#'     output = "word", word_path = tmp
#'   )
#'   unlink(tmp)
#' }
#' }
#'
#' \dontrun{
#' # Clipboard: writes to the system clipboard.
#' table_continuous_lm(
#'   sochealth, select = c(wellbeing_score, bmi), by = sex,
#'   output = "clipboard"
#' )
#' }
#'
#' @export
table_continuous_lm <- function(
  data,
  select = tidyselect::everything(),
  by,
  covariates = NULL,
  adjustment = c("proportional", "balanced"),
  exclude = NULL,
  regex = FALSE,
  weights = NULL,
  vcov = c(
    "classical",
    "HC0",
    "HC1",
    "HC2",
    "HC3",
    "HC4",
    "HC4m",
    "HC5",
    "CR0",
    "CR1",
    "CR2",
    "CR3",
    "bootstrap",
    "jackknife"
  ),
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
  note_labels = NULL,
  ci_level = 0.95,
  digits = 2,
  fit_digits = 2,
  effect_size_digits = 2,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  note = NULL,
  note_add = NULL,
  output = c(
    "default",
    "data.frame",
    "long",
    "tinytable",
    "gt",
    "flextable",
    "excel",
    "clipboard",
    "word"
  ),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  verbose = FALSE,
  user_na = TRUE,
  style = NULL
) {
  # A journal / locale style only moves DEFAULTS (see `?spicy_style`).
  .style_pushed <- .style_begin(style, match.call(), environment())
  on.exit(.style_end(.style_pushed), add = TRUE)
  # The call the errors of the fits name (`.with_public_call()`).
  public_call <- sys.call()

  .check_data_frame(data, "table_continuous_lm")
  validate_ci_level(ci_level)
  # One number of decimals for the table, or one per outcome: `digits` is
  # then the table's number, the one the outcomes not named keep.
  digits_arg <- .continuous_digits_arg(
    digits,
    style,
    formals(table_continuous_lm)$digits
  )
  digits <- digits_arg$table
  fit_digits <- validate_digit_arg(fit_digits, "fit_digits")
  effect_size_digits <- validate_digit_arg(
    effect_size_digits,
    "effect_size_digits"
  )
  p_digits <- validate_digit_arg(p_digits, "p_digits", min = 1L)
  boot_n_explicit <- !missing(boot_n)
  boot_n <- validate_boot_n(boot_n)
  validate_decimal_mark(decimal_mark)
  .check_labels_arg(labels)
  .check_note_labels_arg(note_labels)
  .check_note_args(note, note_add)
  # `p_value = NULL` is the family's automatic default: the p-value is
  # shown whenever `by` is supplied, and `by` is required here.
  if (
    !is.null(p_value) &&
      (!is.logical(p_value) || length(p_value) != 1L || is.na(p_value))
  ) {
    spicy_abort(
      "`p_value` must be TRUE, FALSE, or NULL.",
      class = "spicy_invalid_input"
    )
  }
  if (is.null(p_value)) {
    p_value <- TRUE
  }
  for (.arg in c(
    "regex",
    "verbose",
    "statistic",
    "show_n",
    "show_weighted_n",
    "ci",
    "effect_size_ci",
    "user_na"
  )) {
    validate_logical_scalar(get(.arg), .arg)
  }

  output <- spicy_match_arg(output)
  .warn_ignored_output_args(output, names(match.call()))
  # Decision 16: NULL resolves to the family's registry sheet name,
  # keeping the usage line of the Rd clean of a display string.
  if (is.null(excel_sheet)) {
    excel_sheet <- spicy_str("excel_sheet_continuous_lm")
  }
  vcov <- spicy_match_arg(vcov)
  if (boot_n_explicit && !identical(vcov, "bootstrap")) {
    .warn_boot_n_ignored("`vcov = \"bootstrap\"`")
  }
  contrast_explicit <- !missing(contrast)
  adjustment_explicit <- !missing(adjustment)
  contrast <- spicy_match_arg(contrast)
  # `effect_size` names a measure; a logical is refused, as in the
  # siblings that once took one, and so are the spellings renamed in
  # 0.14.0.
  .check_effect_size_not_logical(
    effect_size,
    "one of `\"f2\"`, `\"cohens_d\"`, `\"hedges_g\"`, `\"omega2\"`"
  )
  .check_effect_size_renamed(effect_size, .ES_RENAMED_LM)
  effect_size <- spicy_match_arg(effect_size)
  r2 <- spicy_match_arg(r2)
  align <- spicy_match_arg(align)
  adjustment <- spicy_match_arg(adjustment)

  # Declared missing values (see the "Declared missing values" section
  # of ?freq): with `user_na = TRUE` declared codes become regular NA
  # before any model is fitted -- `complete.cases()` and `lm()` do not
  # dispatch haven's `is.na()`, so the conversion must happen up
  # front. With `user_na = FALSE` the declaration is dropped and the
  # codes enter the fits as ordinary values.
  resolve_user_na <- function(v) {
    if (isTRUE(user_na)) .user_na_to_na(v) else .user_na_zap(v)
  }

  by_quo <- rlang::enquo(by)
  by_name <- resolve_single_column_selection(by_quo, data, "by")
  # bit64::integer64 passes is_supported_lm_predictor() (is.numeric()
  # is TRUE on the raw int64 payload) but lm() then fits garbage
  # denormal doubles and every estimate row comes back blank. Refuse
  # loudly before any model sees it.
  .check_integer64_columns(data, by_name, "table_continuous_lm")
  by_vector <- resolve_user_na(data[[by_name]])
  # A haven labelled vector with value labels is the categorical
  # signal of an SPSS / Stata import, and the sibling tables
  # (table_continuous(), table_categorical()) already group by such
  # columns. Convert it to a factor so it takes the categorical path
  # instead of silently fitting a slope on the codes: its groups are
  # named and ordered as in the sibling tables (`.group_display()`), the
  # first one the reference. A labelled vector without value labels stays
  # continuous.
  # `resolve_user_na()` has already applied the `user_na` contract, so
  # declared-missing codes are NA (default) or ordinary values
  # (user_na = FALSE) here.
  by_vector <- .group_display(by_vector)
  if (!is_supported_lm_predictor(by_vector)) {
    spicy_abort(
      "`by` must be numeric, logical, character, or factor.",
      class = "spicy_invalid_input"
    )
  }
  # A categorical `by` needs at least two observed groups: with a
  # single observed level there is nothing to compare and every fit
  # would degenerate to an all-NA row. Fail up front with the reason
  # instead of printing that row silently (audit phase 2, finding 32).
  if (!is.numeric(by_vector)) {
    n_observed_by <- length(unique(by_vector[!is.na(by_vector)]))
    if (n_observed_by < 2L) {
      spicy_abort(
        c(
          sprintf(
            "`by` (`%s`) has %d observed non-missing level%s; a group comparison needs at least two.",
            by_name,
            n_observed_by,
            if (n_observed_by == 1L) "" else "s"
          ),
          "i" = "Check the grouping column, or use a `by` with at least two observed groups."
        ),
        class = "spicy_invalid_data"
      )
    }
  }

  if (effect_size %in% c("cohens_d", "hedges_g")) {
    is_two_level <- !is.numeric(by_vector) &&
      nlevels(droplevels(coerce_lm_factor(by_vector))) == 2L
    if (!is_two_level) {
      spicy_abort(
        sprintf(
          paste0(
            "`effect_size = \"%s\"` requires `by` to be a categorical ",
            "predictor with exactly two non-empty levels."
          ),
          effect_size
        ),
        class = "spicy_invalid_input"
      )
    }
  }

  if (isTRUE(effect_size_ci) && identical(effect_size, "none")) {
    spicy_warn(
      "`effect_size_ci` is ignored when `effect_size = \"none\"`.",
      class = "spicy_ignored_arg"
    )
    effect_size_ci <- FALSE
  }

  weights_quo <- rlang::enquo(weights)
  weights_name <- detect_weights_column_name(weights_quo, data)
  weights_vec <- resolve_weights_argument(weights_quo, data, "weights")
  # NA weights are legal: the documented contract excludes those rows
  # from the analytic sample (alongside NA in `y` / `by`) and discloses
  # them in the table note. Only genuinely non-finite values (Inf, -Inf,
  # NaN) are rejected -- `!is.finite(NA)` is TRUE, so the finiteness
  # check must not sweep NA along with them (audit phase 2, finding 14).
  .check_weights_values(weights_vec)
  if (isTRUE(show_weighted_n) && is.null(weights_vec)) {
    spicy_warn(
      "`show_weighted_n` is ignored when `weights` is not supplied.",
      class = "spicy_ignored_arg"
    )
    show_weighted_n <- FALSE
  }

  cluster_quo <- rlang::enquo(cluster)
  # Accept the one-sided-formula idiom `cluster = ~g` (the sandwich /
  # fixest convention) by translating it to the column name before
  # resolution. Anything else (bare column, string, raw vector) flows
  # to the shared resolver unchanged; a formula that does not name a
  # single column in `data` also falls through and is rejected there.
  cluster_expr <- rlang::quo_get_expr(cluster_quo)
  if (rlang::is_call(cluster_expr, "~", n = 1L)) {
    cluster_var <- all.vars(cluster_expr)
    if (length(cluster_var) == 1L && cluster_var %in% names(data)) {
      cluster_quo <- rlang::new_quosure(
        cluster_var,
        env = rlang::quo_get_env(cluster_quo)
      )
    }
  }
  cluster_name <- detect_weights_column_name(cluster_quo, data)
  cluster_vec <- resolve_cluster_argument(cluster_quo, data, "cluster")

  is_cr_vcov <- startsWith(vcov, "CR")
  is_resampling_vcov <- vcov %in% c("bootstrap", "jackknife")
  cluster_allowed <- is_cr_vcov || is_resampling_vcov

  if (is_cr_vcov && is.null(cluster_vec)) {
    spicy_abort(
      sprintf(
        paste0(
          "`vcov = \"%s\"` requires `cluster` to be specified ",
          "(an atomic vector or a single column name in `data`)."
        ),
        vcov
      ),
      class = "spicy_invalid_input"
    )
  }
  if (!cluster_allowed && !is.null(cluster_vec)) {
    spicy_abort(
      sprintf(
        paste0(
          "`cluster` is only used when `vcov` is one of the cluster-",
          "robust variants (\"CR0\", \"CR1\", \"CR2\", \"CR3\"), ",
          "\"bootstrap\", or \"jackknife\". Got `vcov = \"%s\"`."
        ),
        vcov
      ),
      class = "spicy_invalid_input"
    )
  }
  if (is_cr_vcov && !requireNamespace("clubSandwich", quietly = TRUE)) {
    spicy_abort(
      sprintf(
        paste0(
          "`vcov = \"%s\"` requires the 'clubSandwich' package. ",
          "Install it with install.packages(\"clubSandwich\")."
        ),
        vcov
      ),
      class = "spicy_invalid_input"
    )
  }
  if (
    !is.null(cluster_vec) &&
      length(unique(stats::na.omit(cluster_vec))) < 2L
  ) {
    spicy_abort(
      "`cluster` must contain at least two distinct non-missing values.",
      class = "spicy_invalid_input"
    )
  }

  available_names <- names(data)
  # Resolve `covariates` (additive only in v1; tidyselect / character)
  # before the outcomes: a variable cannot be both outcome and
  # adjustment, so the covariates are outside the outcome selection.
  covariates_names <- resolve_covariates_argument(
    rlang::enquo(covariates),
    data,
    by_name = by_name
  )
  # Arguments of one setting only, typed in a call without it, reach
  # nothing: `adjustment` chooses how the means are adjusted for the
  # covariates, `contrast` the contrast of a categorical `by`.
  if (adjustment_explicit && length(covariates_names) == 0L) {
    .warn_ignored("adjustment", "when `covariates` is empty")
  }
  if (contrast_explicit && is.numeric(by_vector)) {
    .warn_ignored("contrast", "when `by` is numeric: a slope has no contrast")
  }

  excluded_names <- resolve_multi_column_selection(
    rlang::enquo(exclude),
    data,
    "exclude"
  )

  # `by`, the covariates, and the weights and cluster columns are outside
  # the selection: `everything()` and the other helpers skip them, and
  # pointing at one in `select`, by name or by position, is an error.
  roles <- c(
    by = by_name,
    stats::setNames(
      covariates_names,
      rep("covariates", length(covariates_names))
    ),
    weights = weights_name,
    cluster = cluster_name
  )
  if (isTRUE(regex)) {
    select_val <- tryCatch(
      rlang::eval_tidy(select),
      error = function(e) NULL
    )
    if (
      !is.character(select_val) || length(select_val) != 1L || is.na(select_val)
    ) {
      spicy_abort(
        "When `regex = TRUE`, `select` must be a single regex pattern.",
        class = "spicy_invalid_input"
      )
    }
    selected_names <- grep(
      select_val,
      setdiff(available_names, roles),
      value = TRUE
    )
  } else {
    selected_names <- .resolve_selection_without(
      rlang::enquo(select),
      data,
      roles = roles
    )
  }

  selected_names <- setdiff(selected_names, excluded_names)
  numeric_outcomes <- selected_names[vapply(
    selected_names,
    function(nm) is.numeric(data[[nm]]),
    logical(1)
  )]
  if (verbose) {
    .inform_ignored_non_numeric(
      "table_continuous_lm",
      setdiff(selected_names, numeric_outcomes)
    )
  }

  # Same integer64 contract as the `by` guard above, applied to the
  # outcome and covariate columns entering the fits.
  .check_integer64_columns(
    data,
    c(numeric_outcomes, covariates_names),
    "table_continuous_lm"
  )

  # Cohen's d / Hedges' g are undefined under covariate adjustment.
  # Reject up front so the user gets a clean error rather than a
  # silent dispatch to an inappropriate formula.
  if (
    length(covariates_names) > 0L && effect_size %in% c("cohens_d", "hedges_g")
  ) {
    spicy_abort(
      c(
        sprintf(
          "`effect_size = \"%s\"` is undefined for covariate-adjusted models.",
          effect_size
        ),
        "i" = "Use `effect_size = \"f2\"` or `\"omega2\"` instead (both generalize to partial effect sizes via partial F)."
      ),
      class = "spicy_unsupported"
    )
  }

  if (length(numeric_outcomes) == 0L) {
    .abort_empty_selection("select", "numeric")
  }
  digits_by_var <- .continuous_digits_by_var(
    digits_arg$named,
    numeric_outcomes,
    digits
  )

  covariates_df <- if (length(covariates_names) > 0L) {
    cdf <- data[, covariates_names, drop = FALSE]
    cdf[] <- lapply(cdf, resolve_user_na)
    cdf
  } else {
    NULL
  }

  outcome_labels <- resolve_variable_labels(data, numeric_outcomes, labels)
  by_label <- resolve_variable_labels(data, by_name, labels)
  # A covariate takes the label `labels` gives it, as an outcome does: the
  # "Adjusted for" line names it.
  covariate_labels <- resolve_variable_labels(data, covariates_names, labels)
  # The labels of the apparatus: the title and the note name a variable as
  # `note_labels` names it, else as the rows do.
  outcome_notes <- .note_labels_of(
    numeric_outcomes,
    outcome_labels,
    note_labels
  )
  by_note <- .note_labels_of(by_name, by_label, note_labels)
  covariate_notes <- .note_labels_of(
    covariates_names,
    covariate_labels,
    note_labels
  )
  .check_note_label_collisions(
    c(numeric_outcomes, by_name, covariates_names),
    c(outcome_labels, by_label, covariate_labels),
    c(outcome_notes, by_note, covariate_notes),
    note_labels
  )
  outcome_values <- lapply(numeric_outcomes, function(nm) {
    resolve_user_na(data[[nm]])
  })

  # The levels of a categorical `by` the table shows, the same for every
  # outcome, and the levels among them in which an outcome has no data:
  # no observed value at all there, typically a question not asked in
  # that group. Such an outcome is fit on the levels with data; the
  # cells of the others show the undefined mark, their rows are not
  # missing values, and the note says so (`.no_data_sentences()`).
  by_levels <- .tclm_by_levels(
    by_vector,
    weights_vec,
    cluster_vec,
    covariates_df
  )
  no_data_levels <- if (!is.null(by_levels)) {
    stats::setNames(
      lapply(outcome_values, function(y) {
        .no_data_groups(y, coerce_lm_factor(by_vector), by_levels)
      }),
      numeric_outcomes
    )
  }

  # The fits compute the variances (`R/vcov.R`): an estimator that fails
  # there is refused in the name of this function.
  rows <- .with_public_call(
    lapply(
      seq_along(numeric_outcomes),
      function(i) {
        fit_outcome_lm_rows(
          y = outcome_values[[i]],
          predictor = by_vector,
          covariates = covariates_df,
          weights = weights_vec,
          cluster = cluster_vec,
          outcome_name = numeric_outcomes[i],
          outcome_label = outcome_labels[i],
          predictor_label = by_label,
          vcov_type = vcov,
          contrast = contrast,
          ci_level = ci_level,
          effect_size = effect_size,
          boot_n = boot_n,
          adjustment = adjustment,
          by_levels = by_levels,
          no_data = no_data_levels[[numeric_outcomes[i]]] %||% character(0)
        )
      }
    ),
    public_call
  )
  result <- do.call(rbind, rows)
  rownames(result) <- NULL

  attr(result, "ci_level") <- ci_level
  attr(result, "digits") <- digits
  attr(result, "digits_by_var") <- digits_by_var
  attr(result, "fit_digits") <- fit_digits
  attr(result, "effect_size_digits") <- effect_size_digits
  attr(result, "p_digits") <- p_digits
  attr(result, "decimal_mark") <- decimal_mark
  # See `.style_stamp()`: this table re-formats at print time, so the
  # argument-less style levers travel with it.
  result <- .style_stamp(result)
  # The names of the family (decision 1 of the ADR of 2026-10-20): the
  # five sibling tables carry the `by` column and its label as
  # `group_var` / `group_label`, the label the title gives it.
  attr(result, "group_var") <- by_name
  attr(result, "group_label") <- by_note
  # The title the console prints and every engine sets, as
  # table_multiresponse() and table_regression() store theirs.
  attr(result, "title") <- .continuous_lm_title(by_note)
  # Read by the display frame: the cells of a level in which an outcome
  # has no data show the undefined mark (NULL for a numeric `by`).
  attr(result, "no_data_levels") <- no_data_levels
  attr(result, "vcov_type") <- vcov
  # Cluster variable NAME (for the SE-estimator note); NA when the
  # cluster was supplied as a raw vector with no recoverable name.
  attr(result, "cluster_name") <- cluster_name %||% NA_character_
  attr(result, "contrast") <- contrast
  attr(result, "covariates") <- covariates_names
  attr(result, "adjustment") <- if (length(covariates_names) > 0L) {
    adjustment
  } else {
    NA_character_
  }
  attr(result, "weights_used") <- !is.null(weights_vec)
  attr(result, "show_statistic") <- statistic
  attr(result, "show_p_value") <- p_value
  attr(result, "show_n") <- show_n
  attr(result, "show_weighted_n") <- show_weighted_n
  attr(result, "effect_size") <- effect_size
  attr(result, "show_effect_size_ci") <- effect_size_ci
  attr(result, "r2_type") <- r2
  attr(result, "show_ci") <- ci
  attr(result, "align") <- align

  # Truthfulness ledger (the family convention of table_continuous()
  # / table_categorical()): what left the analytic sample is disclosed
  # in the table note. Per-variable NA counts (outcomes and
  # covariates, regular vs declared missing -- the same split
  # table_continuous() reports; decision 14, 2026-08-15: the `n`
  # column shows the effect but not the cause), the rows dropped for a
  # missing `by` value closing the list, then those of a missing weight.
  na_dropped <- integer(0)
  user_na_dropped <- integer(0)
  for (.nm in c(numeric_outcomes, covariates_names)) {
    .col <- data[[.nm]]
    # The rows of a level in which the outcome has no data are not
    # missing answers: they leave its count.
    .rows <- !(as.character(by_vector) %in% no_data_levels[[.nm]])
    .n_user <- if (isTRUE(user_na)) sum(.user_na_mask(.col)[.rows]) else 0L
    if (.n_user > 0L) {
      user_na_dropped[[.nm]] <- .n_user
    }
    .nd <- sum(is.na(resolve_user_na(.col))[.rows]) - .n_user
    if (.nd > 0L) {
      na_dropped[[.nm]] <- .nd
    }
  }
  # An outcome and a covariate are named by their labels in the apparatus,
  # as in the "Adjusted for" line.
  label_of <- stats::setNames(
    c(outcome_notes, covariate_notes),
    c(numeric_outcomes, covariates_names)
  )
  # `by` closes the list, named as the title names it.
  na_dropped <- .with_group_counts(
    .label_counts(na_dropped, label_of),
    by_note,
    sum(is.na(by_vector))
  )
  user_na_dropped <- .label_counts(user_na_dropped, label_of)
  missing_parts <- character(0)
  if (length(na_dropped)) {
    missing_parts <- c(
      missing_parts,
      paste0(
        spicy_str("note_missing_removed"),
        .join_note_vars(
          spicy_fmt("note_missing_item", names(na_dropped), na_dropped)
        ),
        "."
      )
    )
  }
  if (length(user_na_dropped)) {
    missing_parts <- c(
      missing_parts,
      paste0(
        spicy_str("note_declared_missing_removed"),
        .join_note_vars(
          spicy_fmt(
            "note_missing_item",
            names(user_na_dropped),
            user_na_dropped
          )
        ),
        "."
      )
    )
  }
  if (!is.null(weights_vec)) {
    n_na_weights <- sum(is.na(weights_vec))
    if (n_na_weights > 0L) {
      missing_parts <- c(
        missing_parts,
        spicy_fmt(
          "note_rows_missing_weights",
          weights_name %||% spicy_str("note_weights_fallback"),
          n_na_weights
        )
      )
    }
  }
  # Then the levels in which an outcome has no data, one sentence per set
  # of levels, each outcome named by its label in the apparatus.
  if (!is.null(no_data_levels)) {
    missing_parts <- c(
      missing_parts,
      .no_data_sentences(outcome_notes, unname(no_data_levels))
    )
  }
  # The ordered columns of this table -- frozen key, displayed header,
  # semantic token -- resolved ONCE and handed to every consumer below,
  # the note first, which defines the abbreviations of the headers.
  # `col_meta$display_label` cannot be the carrier: the typed view is
  # built only for `output = "default"`, and the exporters receive the
  # display frame alone.
  spec <- .lm_column_spec(
    result,
    ci_level = ci_level,
    show_statistic = statistic,
    show_p_value = p_value,
    show_n = show_n,
    show_weighted_n = show_weighted_n,
    effect_size = effect_size,
    effect_size_ci = effect_size_ci,
    r2_type = r2,
    ci = ci,
    decimal_mark = decimal_mark
  )

  # The whole note, for every route: print() shows it as it is here, and
  # `note` replaces or removes it.
  table_note <- .table_note(
    .tclm_note_text(
      result,
      missing_note = missing_parts,
      covariates = covariate_notes,
      weighted_by = if (!is.null(weights_vec)) {
        weights_name %||% spicy_str("note_weights_fallback")
      },
      spec = spec,
      outcome_labels = stats::setNames(outcome_notes, numeric_outcomes),
      add = note_add
    ),
    note,
    note_add
  )
  attr(result, "note") <- table_note

  if (identical(output, "long")) {
    return(.continuous_lm_publish_long(result))
  }

  wide_raw <- build_wide_raw_continuous_lm(
    result,
    show_statistic = statistic,
    show_p_value = p_value,
    show_n = show_n,
    show_weighted_n = show_weighted_n,
    effect_size = effect_size,
    effect_size_ci = effect_size_ci,
    r2_type = r2,
    ci = ci,
    ci_level = ci_level,
    spec = spec
  )

  wide_df <- build_wide_display_df_continuous_lm(
    result,
    digits = digits,
    fit_digits = fit_digits,
    effect_size_digits = effect_size_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    ci_level = ci_level,
    show_statistic = statistic,
    show_p_value = p_value,
    show_n = show_n,
    show_weighted_n = show_weighted_n,
    effect_size = effect_size,
    effect_size_ci = effect_size_ci,
    r2_type = r2,
    ci = ci,
    spec = spec,
    digits_by_var = digits_by_var
  )

  if (output %in% c("default", "data.frame")) {
    # Typed view for `as_structured()`: `wide_raw` and `wide_df` are
    # the raw and displayed frames already built above, so the typed
    # body and the printed body come from one computation each.
    attr(result, "structured") <- .build_continuous_lm_structured(
      result = result,
      wide_raw = wide_raw,
      wide_display = wide_df,
      digits = digits,
      fit_digits = fit_digits,
      effect_size_digits = effect_size_digits,
      p_digits = p_digits,
      decimal_mark = decimal_mark,
      ci_level = ci_level,
      show_statistic = statistic,
      effect_size = effect_size,
      effect_size_ci = effect_size_ci,
      r2_type = r2,
      spec = spec,
      digits_by_var = digits_by_var
    )
    class(result) <- c(
      "spicy_continuous_lm_table",
      "spicy_table",
      class(result)
    )
    # `output = "data.frame"` is the displayed table with its numbers,
    # `as.data.frame()` of the object the call would print (decision 5
    # of the ADR of 2026-10-20).
    if (identical(output, "data.frame")) {
      return(as.data.frame(result))
    }
    # Return VISIBLY and let standard auto-print dispatch to
    # print.spicy_continuous_lm_table(): a bare table_continuous_lm(...)
    # call still displays the table, while `t <- table_continuous_lm(...)`
    # is silent (the freq() / cross_tab() model).
    return(result)
  }

  export_continuous_lm_table(
    wide_df,
    output = output,
    ci_level = ci_level,
    align = align,
    decimal_mark = decimal_mark,
    excel_path = excel_path,
    excel_sheet = excel_sheet,
    clipboard_delim = clipboard_delim,
    word_path = word_path,
    note = table_note,
    title = .continuous_lm_title(by_note),
    labels = .lm_spec_labels(spec),
    mean_keys = .lm_spec_mean_keys(spec)
  )
}

# The levels of a categorical `by` the table shows, in their order: those
# holding at least one row complete on the columns every outcome's model
# shares (`by`, `weights`, `cluster`, the covariates). A level no model
# can reach is not a group of the table, as before; a level that one
# outcome cannot reach -- it has no data there -- keeps its place in that
# outcome's rows. NULL for a numeric `by`.
.tclm_by_levels <- function(
  predictor,
  weights = NULL,
  cluster = NULL,
  covariates = NULL
) {
  if (is.numeric(predictor)) {
    return(NULL)
  }
  shared <- !is.na(predictor)
  if (!is.null(weights)) {
    shared <- shared & !is.na(weights)
  }
  if (!is.null(cluster)) {
    shared <- shared & !is.na(cluster)
  }
  if (!is.null(covariates) && ncol(covariates) > 0L) {
    shared <- shared & stats::complete.cases(covariates)
  }
  levels(droplevels(coerce_lm_factor(predictor)[shared]))
}

fit_outcome_lm_rows <- function(
  y,
  predictor,
  covariates = NULL,
  weights,
  cluster = NULL,
  outcome_name,
  outcome_label,
  predictor_label,
  vcov_type,
  contrast,
  ci_level,
  effect_size = "none",
  boot_n = 1000L,
  adjustment = c("proportional", "balanced"),
  by_levels = NULL,
  no_data = character(0)
) {
  adjustment <- match.arg(adjustment)
  # Level template for a categorical predictor, captured BEFORE the
  # per-outcome complete-case filtering (`.tclm_by_levels()`): every
  # outcome's rows span these levels, so the wide `M (<level>)` columns
  # are the same for all of them (no spurious `M (NA)` column, no level
  # of one outcome missing from another).
  by_levels <- by_levels %||%
    .tclm_by_levels(predictor, weights, cluster, covariates)
  keep <- !is.na(y) & !is.na(predictor)
  if (!is.null(weights)) {
    keep <- keep & !is.na(weights)
  }
  if (!is.null(cluster)) {
    keep <- keep & !is.na(cluster)
  }
  if (!is.null(covariates) && ncol(covariates) > 0L) {
    keep <- keep & stats::complete.cases(covariates)
  }

  y <- y[keep]
  predictor <- predictor[keep]
  weights <- if (is.null(weights)) NULL else weights[keep]
  cluster <- if (is.null(cluster)) NULL else cluster[keep]
  covariates <- if (is.null(covariates)) {
    NULL
  } else {
    covariates[keep, , drop = FALSE]
  }
  if (!is.null(covariates) && ncol(covariates) > 0L) {
    # Drop declared-but-unobserved factor levels BEFORE the fit:
    # lm() drops them from the coefficients anyway (model.frame's
    # drop.unused.levels), but the emmean prediction grids expand
    # the original level set, so the design would gain columns the
    # fit has no coefficients for (audit phase 2, finding 22). The
    # `ordered` class survives droplevels(), so contrast coding is
    # unaffected. align_design_to_coef() remains the last-resort
    # guard for any residual mismatch.
    covariates[] <- lapply(covariates, function(col) {
      if (is.factor(col)) droplevels(col) else col
    })
  }

  if (is.numeric(predictor)) {
    return(
      fit_numeric_predictor_lm_rows(
        y = y,
        x = predictor,
        covariates = covariates,
        weights = weights,
        cluster = cluster,
        outcome_name = outcome_name,
        outcome_label = outcome_label,
        predictor_label = predictor_label,
        vcov_type = vcov_type,
        ci_level = ci_level,
        effect_size = effect_size,
        boot_n = boot_n
      )
    )
  }

  fit_categorical_predictor_lm_rows(
    y = y,
    x = predictor,
    covariates = covariates,
    weights = weights,
    cluster = cluster,
    outcome_name = outcome_name,
    outcome_label = outcome_label,
    predictor_label = predictor_label,
    vcov_type = vcov_type,
    contrast = contrast,
    ci_level = ci_level,
    effect_size = effect_size,
    boot_n = boot_n,
    adjustment = adjustment,
    by_levels = by_levels,
    no_data = no_data
  )
}

# Internal: quote non-syntactic covariate names for
# stats::reformulate() so a column like "co var" survives formula
# construction instead of raising a raw parse error (audit phase 2,
# finding 28). Syntactic names pass through unquoted, so coefficient
# names are unchanged for them.
backtick_nonsyntactic <- function(nms) {
  needs <- make.names(nms) != nms
  nms[needs] <- sprintf("`%s`", nms[needs])
  nms
}

# Internal: a saturated fit (zero residual df, e.g. one observation
# per group) has no residual variance: every SE, CI, test statistic
# and p-value is NaN, and the t/z fallback used to label the row with
# a misleading "z" test. Blank the inferential columns to NA -- the
# point estimates (means, differences, slopes) are still exact and
# stay -- and disclose why with a classed warning (audit phase 2,
# finding 32). NaN must never be displayed as if it were a computed
# result.
degrade_saturated_lm_rows <- function(out, fit, outcome_name) {
  df_resid <- stats::df.residual(fit)
  if (is.finite(df_resid) && df_resid > 0) {
    return(out)
  }
  spicy_warn(
    c(
      sprintf(
        "The model for `%s` has zero residual degrees of freedom (too few observations per group); SEs, CIs, test statistics, and p-values are NA.",
        outcome_name
      ),
      "i" = "Inference needs at least one residual degree of freedom (e.g. two observations in some group)."
    ),
    class = "spicy_undefined_stat"
  )
  na_cols <- c(
    "emmean_se",
    "emmean_ci_lower",
    "emmean_ci_upper",
    "estimate_se",
    "estimate_ci_lower",
    "estimate_ci_upper",
    "statistic",
    "df2",
    "p.value",
    "es_value",
    "es_ci_lower",
    "es_ci_upper",
    "adj_r2"
  )
  for (nm in na_cols) {
    out[[nm]] <- NA_real_
  }
  out$test_type <- NA_character_
  out$df1 <- NA_integer_
  out
}

# Internal: an outcome the model has nothing to infer about. Two causes,
# both read from the data before any test:
#   * no variance in the fitted sample (every observation the same value):
#     the test, the intervals, the effect size and R^2 are 0/0, yet lm()
#     returned numbers for them, made of rounding noise (for y = 5 a t of
#     1, a p of .331 and an R^2 of 0.49, other values for y = 3.7, NaN for
#     y = 0.1);
#   * a residual variance of zero, each group of a categorical `by`
#     constant on its own value (`.within_groups_constant()`): the t of
#     the contrast was 3.32e+15; or an outcome that is an exact linear
#     function of `by` and the covariates (`.lm_exact_fit()`): the t of a
#     slope was 5.20e+16, that of a contrast adjusted for a covariate
#     1.31e+15. There `means` are the model's adjusted means. R^2 is 1
#     there, and stays: it is defined (the outcome varies) and exact, not
#     rounding noise.
# Both came with base R's "essentially perfect fit" warning. The rule of
# `table_continuous()` applies instead: the means (the constant of each
# group) and the contrast (their difference) or the slope (0, or the
# fitted slope of an exact fit) are kept, every inferential column is NA,
# the test is none (so the note names none for it), and one classed
# warning names the outcome. The display shows the undefined mark in
# those cells, as in every cell with no value. `x` is the fitted `by`, a
# factor for a categorical one.
no_inference_lm_rows <- function(
  y,
  x,
  weights,
  outcome_name,
  outcome_label,
  predictor_label,
  effect_size = "none",
  contrast_shown = FALSE,
  by_levels = NULL,
  slope = 0,
  adjusted = FALSE,
  means = NULL
) {
  constant <- length(unique(y)) < 2L
  spicy_warn(
    c(
      if (constant) {
        sprintf(
          "The outcome `%s` has no variance, so it has no test, interval, effect size, or R-squared; those cells are undefined.",
          outcome_name
        )
      } else {
        sprintf(
          "The model for `%s` has no inference (the residual variance is zero: %s), so it has no test, interval, or effect size; those cells are undefined.",
          outcome_name,
          if (adjusted) {
            "the outcome is an exact linear function of `by` and the covariates"
          } else if (is.factor(x)) {
            "each group of `by` is constant"
          } else {
            "the outcome is an exact linear function of `by`"
          }
        )
      },
      "i" = "Other outcomes are unaffected."
    ),
    class = "spicy_undefined_stat"
  )
  categorical <- is.factor(x)
  out <- make_empty_lm_rows(
    outcome_name,
    outcome_label,
    if (categorical) "categorical" else "continuous",
    predictor_label = predictor_label,
    levels = if (categorical) levels(x) else NA_character_
  )
  if (categorical) {
    means <- means %||%
      vapply(
        levels(x),
        function(lv) y[x == lv][[1L]],
        numeric(1),
        USE.NAMES = FALSE
      )
    out$emmean <- means
    if (contrast_shown) {
      out$estimate_type[2L] <- "difference"
      out$estimate[2L] <- means[[2L]] - means[[1L]]
    }
  } else {
    out$estimate_type <- "slope"
    out$estimate <- slope
  }
  if (!constant) {
    # The groups (or the exact line) explain all of the variance: R^2 is
    # 1, with residual degrees of freedom to spare (the caller's
    # condition).
    out$r2[[1L]] <- 1
    out$adj_r2[[1L]] <- 1
  }
  out$es_type[1L] <- pick_es_type_lm(effect_size)
  out$n <- length(y)
  out$weighted_n <- if (is.null(weights)) NA_real_ else sum(weights)
  if (categorical) {
    out <- .tclm_expand_levels(out, by_levels %||% levels(x))
  }
  out
}

# TRUE when the outcome lies in the column space of the design: appending
# it to the (weighted) model matrix leaves the rank unchanged, at the
# tolerance lm() itself uses to decide the rank of the design. A property
# of the data, read like lm() reads aliasing, never from the size of a
# residual or of a statistic. The rows are weighted as lm.wfit() weights
# them, so a zero weight removes its row.
#
# The outcome is centered on its (weighted) mean first. The design has an
# intercept, so the answer is the same, but qr() judges a column by its
# norm: on the raw outcome the tolerance was relative to its LEVEL, and
# 1000 + 3x with noise of SD 1e-4 read as exact. Centered, it is relative
# to the variation of the outcome.
.lm_exact_fit <- function(fit) {
  X <- stats::model.matrix(fit)
  y <- stats::model.response(stats::model.frame(fit))
  w <- stats::weights(fit) %||% rep(1, length(y))
  y <- y - sum(w * y) / sum(w)
  X <- X * sqrt(w)
  y <- y * sqrt(w)
  qr(cbind(X, y), tol = 1e-7)$rank == qr(X, tol = 1e-7)$rank
}

# The rows that turn the coefficients into the mean of each level of a
# categorical `by`, one per level, aligned with `cf`: the means are
# `L %*% cf` and their variance `L V L'`. Without covariates, the design
# row of the level: `contrasts.arg = fit$contrasts` pins the grid to the
# coding the model was fitted with, and the columns are aligned to the
# coefficients BY NAME, since a positional product would silently mismatch
# if the codings ever diverged (audit phase 2). With covariates, the
# averaged row of `build_emmean_avg_row()`. Shared by the fit with
# inference and by an exact fit, which keeps the means without intervals.
.tclm_emmean_rows <- function(fit, cf, levs, covariates, adjustment) {
  if (is.null(covariates)) {
    design <- stats::model.matrix(
      stats::delete.response(stats::terms(fit)),
      data.frame(x = factor(levs, levels = levs)),
      contrasts.arg = fit$contrasts
    )
    return(align_design_to_coef(design, cf))
  }
  do.call(
    rbind,
    lapply(levs, function(lv) {
      build_emmean_avg_row(
        fit,
        x_focal_level = lv,
        x_levels = levs,
        covariates_observed = covariates,
        method = adjustment
      )
    })
  )
}

fit_numeric_predictor_lm_rows <- function(
  y,
  x,
  covariates = NULL,
  weights,
  cluster = NULL,
  outcome_name,
  outcome_label,
  predictor_label,
  vcov_type,
  ci_level,
  effect_size = "none",
  boot_n = 1000L
) {
  if (length(y) < 2L || stats::sd(x, na.rm = TRUE) == 0) {
    # Per-outcome degradation (mirrors the categorical branch): a
    # slope cannot be estimated for THIS outcome, so return an all-NA
    # row with a classed warning naming the outcome instead of a
    # silent blank line (audit phase 2, finding 32).
    spicy_warn(
      c(
        sprintf(
          "The model for `%s` could not be fit: %s after removing incomplete rows; its cells are NA.",
          outcome_name,
          if (length(y) < 2L) {
            "fewer than two complete observations remain"
          } else {
            "`by` is constant on the remaining observations"
          }
        ),
        "i" = "Rows with missing values in the outcome, `by`, `weights`, `cluster`, or covariates are excluded per outcome."
      ),
      class = "spicy_undefined_stat"
    )
    return(make_empty_lm_rows(
      outcome_name,
      outcome_label,
      "continuous",
      predictor_label = predictor_label
    ))
  }
  if (all(y == y[[1L]])) {
    return(no_inference_lm_rows(
      y,
      x,
      weights,
      outcome_name,
      outcome_label,
      predictor_label,
      effect_size = effect_size
    ))
  }

  has_covs <- !is.null(covariates) && ncol(covariates) > 0L
  model_df <- if (has_covs) {
    cbind(data.frame(y = y, x = x), covariates)
  } else {
    data.frame(y = y, x = x)
  }
  rhs_terms <- if (has_covs) {
    c("x", backtick_nonsyntactic(names(covariates)))
  } else {
    "x"
  }
  formula <- stats::reformulate(rhs_terms, response = "y")

  fit <- if (is.null(weights)) {
    stats::lm(formula, data = model_df)
  } else {
    stats::lm(formula, data = model_df, weights = weights)
  }

  # The outcome an exact linear function of `by` (and the covariates),
  # with residual degrees of freedom to spare: the residual variance is
  # zero, as for a categorical `by` constant within each group. Decided
  # on the design before any test; a saturated fit keeps its rule.
  if (stats::df.residual(fit) > 0 && .lm_exact_fit(fit)) {
    return(no_inference_lm_rows(
      y,
      x,
      weights,
      outcome_name,
      outcome_label,
      predictor_label,
      effect_size = effect_size,
      slope = unname(stats::coef(fit)[[2L]]),
      adjusted = has_covs
    ))
  }

  vc <- compute_model_vcov(
    fit,
    vcov_type,
    cluster = cluster,
    weights = weights,
    boot_n = boot_n
  )

  # `focal_term = "x"` activates the partial-F path in
  # compute_lm_model_stats / compute_es_ci_lm so f^2 and omega^2 are
  # restricted to x and ignore covariate contributions to R^2. When
  # there are no covariates the model is bivariate and focal_term
  # is NULL; the model-level F coincides with the focal F.
  focal_term <- if (has_covs) "x" else NULL
  model_stats <- compute_lm_model_stats(fit, focal_term = focal_term)

  # `coef_idx = 2L`: x is always the 2nd coefficient (after the
  # intercept). When covariates are present they come AFTER x in the
  # design matrix because `reformulate(c("x", cov_names))` orders
  # terms left-to-right, so position 2 still picks out x.
  inf <- compute_coef_inference(
    fit,
    coef_idx = 2L,
    vc = vc,
    vcov_type = vcov_type,
    cluster = cluster,
    ci_level = ci_level
  )

  es_ci <- compute_es_ci_lm(fit, effect_size, ci_level, focal_term = focal_term)

  out <- data.frame(
    variable = outcome_name,
    label = outcome_label,
    predictor_type = "continuous",
    predictor_label = predictor_label,
    level = NA_character_,
    reference = NA_character_,
    estimate_type = "slope",
    emmean = NA_real_,
    emmean_se = NA_real_,
    emmean_ci_lower = NA_real_,
    emmean_ci_upper = NA_real_,
    estimate = inf$estimate,
    estimate_se = inf$se,
    estimate_ci_lower = inf$ci_lower,
    estimate_ci_upper = inf$ci_upper,
    test_type = inf$test_type %||% "t",
    statistic = inf$statistic,
    df1 = 1L,
    df2 = inf$df,
    p.value = inf$p.value,
    es_type = pick_es_type_lm(effect_size),
    es_value = pick_es_value_lm(model_stats, effect_size),
    es_ci_lower = es_ci[1],
    es_ci_upper = es_ci[2],
    r2 = model_stats$r2,
    adj_r2 = model_stats$adj_r2,
    n = length(y),
    weighted_n = if (is.null(weights)) NA_real_ else sum(weights),
    # Valid bootstrap replicate count (NA outside vcov = "bootstrap"):
    # surfaces in the table note, mirroring table_regression's
    # "bootstrap (N replicates)" disclosure.
    boot_n_valid = as.integer(attr(vc, "boot_n_valid") %||% NA_integer_),
    stringsAsFactors = FALSE
  )
  degrade_saturated_lm_rows(out, fit, outcome_name)
}

fit_categorical_predictor_lm_rows <- function(
  y,
  x,
  covariates = NULL,
  weights,
  cluster = NULL,
  outcome_name,
  outcome_label,
  predictor_label,
  vcov_type,
  contrast,
  ci_level,
  effect_size = "none",
  boot_n = 1000L,
  adjustment = c("proportional", "balanced"),
  by_levels = NULL,
  no_data = character(0)
) {
  adjustment <- match.arg(adjustment)
  x <- droplevels(coerce_lm_factor(x))
  # The documented contract for categorical `by` is R's default
  # treatment-contrast convention: first level = reference, per-level
  # means, and Delta = level - reference. An ordered factor would
  # instead be fitted with polynomial contrasts (contr.poly), while
  # the prediction grids below rebuild `x` as a plain factor --
  # design columns and coefficients would silently diverge (audit
  # phase 2, findings 11/19/20). Refit as an unordered factor with
  # the same level order so the fit itself matches the documented
  # convention in both the covariate-free and the covariate-adjusted
  # branches.
  if (is.ordered(x)) {
    x <- factor(x, levels = levels(x), ordered = FALSE)
  }
  # The levels of the table, which every outcome's rows span (see
  # `.tclm_expand_levels()`); the fitted levels when called alone.
  by_levels <- by_levels %||% levels(x)
  if (length(y) < 2L || nlevels(x) < 2L) {
    # Per-outcome degradation: table_continuous_lm() already rejects a
    # `by` with fewer than two observed levels overall, so reaching
    # this branch means THIS outcome's complete-case filtering (NA in
    # y, weights, cluster, or covariates) removed every observation of
    # all but (at most) one group. Degrade loudly -- a classed warning
    # naming the outcome -- and return all-NA rows keyed to the
    # regular `by` levels so the wide header stays clean (audit phase
    # 2, finding 32). Unless the cause is a level with no data
    # (`no_data`, typically a question not asked there): when the table
    # has two levels or more and the outcome has data in fewer than two,
    # the note says so ("No data for X in G."), which is ordinary data,
    # and no warning repeats it.
    no_data_cause <- length(by_levels) >= 2L &&
      length(setdiff(by_levels, no_data)) < 2L
    # The one level with data keeps its mean, its n and the outcome's n:
    # only the cells of the levels with no data and the comparison are
    # undefined.
    if (no_data_cause && nlevels(x) == 1L) {
      return(.tclm_one_level_rows(
        y,
        x,
        covariates = covariates,
        weights = weights,
        cluster = cluster,
        outcome_name = outcome_name,
        outcome_label = outcome_label,
        predictor_label = predictor_label,
        vcov_type = vcov_type,
        ci_level = ci_level,
        effect_size = effect_size,
        boot_n = boot_n,
        adjustment = adjustment,
        by_levels = by_levels
      ))
    }
    if (!no_data_cause) {
      spicy_warn(
        c(
          sprintf(
            "The model for `%s` could not be fit: %d observed non-missing group%s left after removing incomplete rows, and a group comparison needs at least two; its cells are NA.",
            outcome_name,
            nlevels(x),
            if (nlevels(x) == 1L) " is" else "s are"
          ),
          "i" = "Rows with missing values in the outcome, `by`, `weights`, `cluster`, or covariates are excluded per outcome."
        ),
        class = "spicy_undefined_stat"
      )
    }
    empty_levels <- by_levels %||% levels(x)
    if (length(empty_levels) == 0L) {
      empty_levels <- NA_character_
    }
    return(make_empty_lm_rows(
      outcome_name,
      outcome_label,
      "categorical",
      predictor_label = predictor_label,
      levels = empty_levels
    ))
  }

  # The contrast of a two-group table: a table of more groups shows none,
  # even for an outcome fit on two of them.
  show_reference <- identical(contrast, "auto") &&
    nlevels(x) == 2L &&
    length(by_levels) == 2L

  if (all(y == y[[1L]])) {
    return(no_inference_lm_rows(
      y,
      x,
      weights,
      outcome_name,
      outcome_label,
      predictor_label,
      effect_size = effect_size,
      contrast_shown = show_reference,
      by_levels = by_levels
    ))
  }

  has_covs <- !is.null(covariates) && ncol(covariates) > 0L
  model_df <- if (has_covs) {
    cbind(data.frame(y = y, x = x), covariates)
  } else {
    data.frame(y = y, x = x)
  }
  rhs_terms <- if (has_covs) {
    c("x", backtick_nonsyntactic(names(covariates)))
  } else {
    "x"
  }
  formula <- stats::reformulate(rhs_terms, response = "y")

  # Pin the focal factor to EXPLICIT treatment contrasts. lm() takes
  # the coding of an unordered factor from the session-wide
  # `options(contrasts = )`, so an afex-style session
  # (`options(contrasts = c("contr.sum", "contr.poly"))`) would
  # silently fit sum-coded dummies named "x1", "x2", ... -- breaking
  # the by-name coefficient lookup below and the documented
  # treatment-contrast convention (reference = first level). The
  # local `contrasts` argument guarantees dummies named "x<level>"
  # whatever the session options are. Covariate factors keep the
  # session coding: the reported quantities (emmeans, focal Wald F,
  # R^2) are invariant to covariate coding, and the prediction grids
  # reuse `fit$contrasts` so design and coefficients always agree.
  fit <- if (is.null(weights)) {
    stats::lm(
      formula,
      data = model_df,
      contrasts = list(x = "contr.treatment")
    )
  } else {
    stats::lm(
      formula,
      data = model_df,
      weights = weights,
      contrasts = list(x = "contr.treatment")
    )
  }

  # Each group constant on its own value, with residual degrees of freedom
  # to spare: the residual variance is zero and the model has nothing to
  # infer. Decided on the data and before any test, ahead of the
  # computations that would warn of an "essentially perfect fit"; lm()
  # itself does not. A saturated fit, with none to spare, keeps its rule.
  if (stats::df.residual(fit) > 0 && .within_groups_constant(y, x)) {
    return(no_inference_lm_rows(
      y,
      x,
      weights,
      outcome_name,
      outcome_label,
      predictor_label,
      effect_size = effect_size,
      contrast_shown = show_reference,
      by_levels = by_levels
    ))
  }
  # The same with covariates: the outcome an exact linear function of `by`
  # and the covariates. The means are the model's adjusted means, those the
  # table shows when it can infer, and the contrast their difference.
  if (stats::df.residual(fit) > 0 && .lm_exact_fit(fit)) {
    L <- .tclm_emmean_rows(
      fit,
      stats::coef(fit),
      levels(x),
      if (has_covs) covariates,
      adjustment
    )
    return(no_inference_lm_rows(
      y,
      x,
      weights,
      outcome_name,
      outcome_label,
      predictor_label,
      effect_size = effect_size,
      contrast_shown = show_reference,
      by_levels = by_levels,
      means = as.vector(L %*% stats::coef(fit)),
      adjusted = has_covs
    ))
  }

  vc <- compute_model_vcov(
    fit,
    vcov_type,
    cluster = cluster,
    weights = weights,
    boot_n = boot_n
  )
  cf <- stats::coef(fit)
  df_resid <- stats::df.residual(fit)
  crit <- if (is.finite(df_resid) && df_resid > 0) {
    stats::qt(1 - (1 - ci_level) / 2, df = df_resid)
  } else {
    stats::qnorm(1 - (1 - ci_level) / 2)
  }
  focal_term <- if (has_covs) "x" else NULL
  model_stats <- compute_lm_model_stats(fit, focal_term = focal_term)

  # Covariate-adjusted estimated marginal means.
  #
  # Without covariates: bivariate fast path -- a single `newdata`
  # containing one row per level of `x`, vectorized through
  # `model.matrix()` and a single matrix multiplication. emmean =
  # group mean.
  #
  # With covariates: the per-level `avg_row` is built by
  # `build_emmean_avg_row()`, which dispatches on `adjustment`:
  #   * `"proportional"` (default; G-computation) averages the
  #     observed covariate distribution with `x` set to the focal
  #     level. Matches Stata `margins` and
  #     `marginaleffects::avg_predictions()`.
  #   * `"balanced"` averages a synthetic grid of factor-covariate
  #     levels x numeric covariates at the sample mean. Matches
  #     `emmeans::emmeans()` default and SPSS UNIANOVA EMMEANS.
  # Both reduce to the same linear-contrast formula
  # (`avg_row %*% cf`); only `avg_row` differs.
  levs <- levels(x)
  L <- .tclm_emmean_rows(fit, cf, levs, if (has_covs) covariates, adjustment)
  emmean <- as.vector(L %*% cf)
  emmean_se <- sqrt(rowSums((L %*% vc) * L))

  # Global Wald F restricted to the focal term `x`. With covariates,
  # `seq_along(cf)[-1]` would also pick up covariate coefficients,
  # producing an omnibus F that mixes the focal effect with covariate
  # nuisance -- wrong. Restrict to coefficients whose term assignment
  # equals the position of `x` in the model's term list.
  x_coef_idx <- which(
    stats::model.matrix(fit) |>
      attr("assign") ==
      which(attr(stats::terms(fit), "term.labels") == "x")
  )
  if (length(x_coef_idx) == 0L) {
    # nocov start: defensive fallback. The model formula always includes
    # the focal term "x" (see `rhs_terms`/`reformulate` above) and
    # `nlevels(x) >= 2` is guaranteed by the early return, so "x" always
    # owns at least one design-matrix column -- x_coef_idx is never empty.
    x_coef_idx <- seq_along(cf)[-1]
    # nocov end
  }
  wald <- compute_wald_test(
    fit,
    coef_idx_set = x_coef_idx,
    vc = vc,
    vcov_type = vcov_type,
    cluster = cluster
  )

  es_ci <- compute_es_ci_lm(fit, effect_size, ci_level, focal_term = focal_term)

  out <- data.frame(
    variable = rep(outcome_name, length(levs)),
    label = rep(outcome_label, length(levs)),
    predictor_type = rep("categorical", length(levs)),
    predictor_label = rep(predictor_label, length(levs)),
    level = levs,
    reference = rep(levs[1], length(levs)),
    estimate_type = rep(NA_character_, length(levs)),
    emmean = emmean,
    emmean_se = emmean_se,
    emmean_ci_lower = emmean - crit * emmean_se,
    emmean_ci_upper = emmean + crit * emmean_se,
    estimate = rep(NA_real_, length(levs)),
    estimate_se = rep(NA_real_, length(levs)),
    estimate_ci_lower = rep(NA_real_, length(levs)),
    estimate_ci_upper = rep(NA_real_, length(levs)),
    test_type = c(
      wald$test_type %||% "F",
      rep(NA_character_, length(levs) - 1L)
    ),
    statistic = c(wald$statistic, rep(NA_real_, length(levs) - 1L)),
    df1 = c(as.integer(wald$df1), rep(NA_integer_, length(levs) - 1L)),
    df2 = c(wald$df2, rep(NA_real_, length(levs) - 1L)),
    p.value = c(wald$p.value, rep(NA_real_, length(levs) - 1L)),
    es_type = c(
      pick_es_type_lm(effect_size),
      rep(NA_character_, length(levs) - 1L)
    ),
    es_value = c(
      pick_es_value_lm(model_stats, effect_size),
      rep(NA_real_, length(levs) - 1L)
    ),
    es_ci_lower = c(es_ci[1], rep(NA_real_, length(levs) - 1L)),
    es_ci_upper = c(es_ci[2], rep(NA_real_, length(levs) - 1L)),
    r2 = c(model_stats$r2, rep(NA_real_, length(levs) - 1L)),
    adj_r2 = c(model_stats$adj_r2, rep(NA_real_, length(levs) - 1L)),
    n = rep(length(y), length(levs)),
    weighted_n = rep(
      if (is.null(weights)) NA_real_ else sum(weights),
      length(levs)
    ),
    boot_n_valid = rep(
      as.integer(attr(vc, "boot_n_valid") %||% NA_integer_),
      length(levs)
    ),
    stringsAsFactors = FALSE
  )

  if (isTRUE(show_reference)) {
    # Per-level contrast (binary case): use the same single-coef
    # inference helper as for numeric predictors so CR mode picks
    # up Satterthwaite df automatically. Under treatment coding the
    # focal dummies are named "x<level>"; look each coefficient up
    # BY NAME -- positional indexing is exactly the silent
    # misalignment behind the ordered-`by` audit findings (a
    # contr.poly ".L" coefficient read as the displayed difference).
    for (i in seq_len(nlevels(x) - 1L)) {
      coef_idx <- match(paste0("x", levs[i + 1L]), names(cf))
      if (is.na(coef_idx)) {
        # nocov start: defensive. The fit is constructed with an
        # explicit `contrasts = list(x = "contr.treatment")` above,
        # so its dummies are named "x<level>" regardless of the
        # session's global `options(contrasts = )` (being an
        # unordered factor alone would NOT suffice: lm() reads the
        # global option for unordered factors too).
        spicy_abort(
          c(
            sprintf(
              "Internal invariant failed: coefficient `%s` not found in the fitted model.",
              paste0("x", levs[i + 1L])
            ),
            "i" = "Please report this at https://github.com/amaltawfik/spicy/issues."
          ),
          class = "spicy_internal_invariant"
        )
        # nocov end
      }
      row_idx <- i + 1L
      inf <- compute_coef_inference(
        fit,
        coef_idx = coef_idx,
        vc = vc,
        vcov_type = vcov_type,
        cluster = cluster,
        ci_level = ci_level
      )
      out$estimate_type[row_idx] <- "difference"
      out$estimate[row_idx] <- inf$estimate
      out$estimate_se[row_idx] <- inf$se
      out$estimate_ci_lower[row_idx] <- inf$ci_lower
      out$estimate_ci_upper[row_idx] <- inf$ci_upper
      out$test_type[row_idx] <- inf$test_type %||% "t"
      out$statistic[row_idx] <- inf$statistic
      out$df1[row_idx] <- 1L
      out$df2[row_idx] <- inf$df
      out$p.value[row_idx] <- inf$p.value
    }
  }

  .tclm_expand_levels(
    degrade_saturated_lm_rows(out, fit, outcome_name),
    by_levels
  )
}

# An outcome with data in one level of a categorical `by` only, the other
# levels holding no observed value (`no_data`, a question not asked
# there). Nothing is compared, yet the mean of that level is estimated as
# its cell asks: by the model of the one level (`y ~ 1`, or `y ~
# covariates`), under the variance estimator and the covariate adjustment
# the call asked for, as the fit of two levels or more gives every level
# its mean (`build_emmean_avg_row()`). The contrast, the test, the effect
# size and R^2 stay NA, shown undefined; `n` is the outcome's, the rows of
# that level. With one observation the mean has no standard error.
.tclm_one_level_rows <- function(
  y,
  x,
  covariates,
  weights,
  cluster,
  outcome_name,
  outcome_label,
  predictor_label,
  vcov_type,
  ci_level,
  effect_size,
  boot_n,
  adjustment,
  by_levels
) {
  lev <- levels(x)
  has_covs <- !is.null(covariates) && ncol(covariates) > 0L
  model_df <- if (has_covs) {
    cbind(data.frame(y = y), covariates)
  } else {
    data.frame(y = y)
  }
  rhs <- if (has_covs) backtick_nonsyntactic(names(covariates)) else "1"
  formula <- stats::reformulate(rhs, response = "y")
  fit <- if (is.null(weights)) {
    stats::lm(formula, data = model_df)
  } else {
    stats::lm(formula, data = model_df, weights = weights)
  }
  avg_row <- build_emmean_avg_row(
    fit,
    x_focal_level = lev,
    x_levels = lev,
    covariates_observed = covariates,
    method = adjustment
  )
  out <- make_empty_lm_rows(
    outcome_name,
    outcome_label,
    "categorical",
    predictor_label = predictor_label,
    levels = lev
  )
  out$emmean <- sum(avg_row * stats::coef(fit))
  df_resid <- stats::df.residual(fit)
  if (is.finite(df_resid) && df_resid > 0) {
    vc <- compute_model_vcov(
      fit,
      vcov_type,
      cluster = cluster,
      weights = weights,
      boot_n = boot_n
    )
    se <- sqrt(sum((avg_row %*% vc) * avg_row))
    crit <- stats::qt(1 - (1 - ci_level) / 2, df = df_resid)
    out$emmean_se <- se
    out$emmean_ci_lower <- out$emmean - crit * se
    out$emmean_ci_upper <- out$emmean + crit * se
    out$boot_n_valid <- as.integer(attr(vc, "boot_n_valid") %||% NA_integer_)
  }
  out$es_type <- pick_es_type_lm(effect_size)
  out$n <- length(y)
  out$weighted_n <- if (is.null(weights)) NA_real_ else sum(weights)
  out <- .tclm_expand_levels(out, by_levels)
  # The reference of the table, as the rows of an outcome with no fit
  # name it: no contrast is estimated.
  out$reference <- by_levels[[1L]]
  out
}

# The rows of an outcome span the levels of the table (`by_levels`), in
# their order, even when its model was fit on fewer: a level in which the
# outcome has no data, or whose every row a covariate, the weights or the
# cluster left out, gets a row with no estimate, so that the blocks of
# all the outcomes line up with the `M (<level>)` columns. The outcome's
# own statistics (the test, the fit, the effect size) stay on the FIRST
# row of the block, where the wide frames, `get_test_row_index_lm()` and
# glance() read them. A block with its levels already in place is
# returned as it is; one being expanded has no contrast row, since the
# table then has more than two levels.
.tclm_expand_levels <- function(out, by_levels) {
  if (identical(out$level, by_levels)) {
    return(out)
  }
  at <- match(by_levels, out$level)
  res <- out[rep(1L, length(by_levels)), , drop = FALSE]
  res$level <- by_levels
  per_level <- c(
    "estimate_type",
    "emmean",
    "emmean_se",
    "emmean_ci_lower",
    "emmean_ci_upper",
    "estimate",
    "estimate_se",
    "estimate_ci_lower",
    "estimate_ci_upper"
  )
  for (nm in per_level) {
    res[[nm]] <- out[[nm]][at]
  }
  first <- c(1L, rep(NA_integer_, length(by_levels) - 1L))
  per_outcome <- c(
    "test_type",
    "statistic",
    "df1",
    "df2",
    "p.value",
    "es_type",
    "es_value",
    "es_ci_lower",
    "es_ci_upper",
    "r2",
    "adj_r2"
  )
  for (nm in per_outcome) {
    res[[nm]] <- out[[nm]][first]
  }
  rownames(res) <- NULL
  res
}

# `levels`: the display levels of a categorical `by` (one all-NA row
# per level). They keep the degenerate outcome's rows aligned with the
# `M (<level>)` columns the other outcomes render -- a single
# `level = NA` row would grow a spurious `M (NA)` column in the wide
# frame. The default single NA row is for continuous predictors,
# whose wide layout has no per-level columns.
make_empty_lm_rows <- function(
  outcome_name,
  outcome_label,
  predictor_type,
  predictor_label = NA_character_,
  levels = NA_character_
) {
  data.frame(
    variable = outcome_name,
    label = outcome_label,
    predictor_type = predictor_type,
    predictor_label = predictor_label,
    level = as.character(levels),
    reference = if (anyNA(levels)) NA_character_ else levels[[1L]],
    estimate_type = NA_character_,
    emmean = NA_real_,
    emmean_se = NA_real_,
    emmean_ci_lower = NA_real_,
    emmean_ci_upper = NA_real_,
    estimate = NA_real_,
    estimate_se = NA_real_,
    estimate_ci_lower = NA_real_,
    estimate_ci_upper = NA_real_,
    test_type = NA_character_,
    statistic = NA_real_,
    df1 = NA_integer_,
    df2 = NA_real_,
    p.value = NA_real_,
    es_type = NA_character_,
    es_value = NA_real_,
    es_ci_lower = NA_real_,
    es_ci_upper = NA_real_,
    r2 = NA_real_,
    adj_r2 = NA_real_,
    n = NA_integer_,
    weighted_n = NA_real_,
    boot_n_valid = NA_integer_,
    stringsAsFactors = FALSE
  )
}
