#' Continuous-outcome linear-model table
#'
#' @description
#' Builds APA-style summary tables from a series of simple linear models for
#' one or many continuous outcomes selected with tidyselect syntax.
#'
#' A single predictor is supplied with `by`, and each selected numeric
#' outcome is fit as `lm(outcome ~ by, ...)`. When `by` is categorical, the
#' function returns a model-based mean-comparison table with fitted means by
#' level derived from the linear model, plus an optional single difference for
#' dichotomous predictors. When `by` is numeric, the table reports the slope
#' and its confidence interval.
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
#'   pattern (character string).
#' @param by A single predictor column. Accepts an unquoted column name or a
#'   single character column name. The predictor can be:
#'   - **numeric** (continuous): treated as a covariate. The table reports
#'     the slope and its CI from `lm(y ~ by)`.
#'   - **factor** or **ordered factor**: treated as categorical. Level order
#'     is preserved as declared; the **first level** is the reference for
#'     the displayed contrast (R's default treatment-contrast convention).
#'   - **character**: coerced to factor with `factor(by)`, which orders the
#'     levels alphabetically. To control the reference level, supply `by`
#'     as an explicit factor with the desired level ordering (e.g. via
#'     [forcats::fct_relevel()] or `factor(..., levels = ...)`).
#'   - **logical**: coerced to factor with levels `"FALSE"`, `"TRUE"` (in
#'     that order, since `FALSE < TRUE`). The reference level is `"FALSE"`,
#'     so a binary contrast displays as `Delta (TRUE - FALSE)`.
#'
#'   Rows with `NA` in `by` are excluded from the analytic sample for each
#'   outcome (NAs in `y` and `weights` are also excluded; see Details).
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
#'   alongside rows with `NA` in `y` or `by`. When supplied, weights are
#'   passed to `lm(..., weights = ...)`, so coefficients become weighted
#'   least-squares estimates and `R²`, adjusted `R²`, and the four effect
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
#'     fractional df is reported in the `df2` column and in the
#'     `t(df)` / `F(df1, df2)` test header. `"CR1"` corresponds to
#'     Stata's `, vce(cluster id)` default. Cluster-robust variants
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
#'   Coefficients (means, contrasts, slopes), `R²`, and the standardized
#'   effect sizes (`f2`, `d`, `g`, `omega2`) are point estimates from the
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
#'   - an atomic vector of length `nrow(data)` evaluated in the calling
#'     environment (factor, character, integer, etc.).
#'
#'   Rows with `NA` in `cluster` are excluded from the analytic sample
#'   for each outcome (alongside rows with `NA` in `y`, `by`, or
#'   `weights`). At least two distinct non-missing cluster values are
#'   required. Multi-way clustering (a list / data.frame of multiple
#'   cluster vectors) is not supported; use [sandwich::vcovCL()] or
#'   [clubSandwich::vcovCR()] directly on the fitted model for that case.
#' @param boot_n Integer. Number of bootstrap replicates used when
#'   `vcov = "bootstrap"`. Defaults to `1000`. Ignored otherwise.
#'   Larger values reduce Monte-Carlo error in the bootstrap variance;
#'   typical values for inference are `500`-`2000`.
#' @param contrast Contrast display for categorical predictors. One of:
#'   - `"auto"` (default): show a single reference contrast
#'     `Delta (level2 - level1)` only when `by` has exactly two non-empty
#'     levels. The reference level is the **first level of the factor**
#'     (R's default treatment-contrast convention,
#'     `getOption("contrasts")[1]`). To change which level acts as the
#'     reference, re-level `by` upstream (for example with
#'     [forcats::fct_relevel()] or [stats::relevel()]).
#'   - `"none"`: suppress the contrast column for categorical predictors.
#'     Level-specific means are still displayed.
#' @param statistic Logical. If `TRUE`, includes a test-statistic column in the
#'   wide and rendered outputs. Defaults to `FALSE`.
#' @param p_value Logical. If `TRUE`, includes a `p` column in the wide and
#'   rendered outputs. Defaults to `TRUE`.
#' @param show_n Logical. If `TRUE`, includes an unweighted `n` column in the
#'   wide and rendered outputs. Defaults to `TRUE`.
#' @param show_weighted_n Logical. If `TRUE` and `weights` is supplied,
#'   includes a `Weighted n` column equal to the sum of case weights in the
#'   analytic sample. Defaults to `FALSE`.
#' @param effect_size Character. Effect-size column to include in the wide and
#'   rendered outputs. One of:
#'   - `"none"` (the default): no effect-size column.
#'   - `"f2"`: Cohen's `f² = R² / (1 - R²)`. Defined for any predictor type.
#'     Familiar from Cohen (1988); standard input for a-priori power analysis.
#'     Note that for a single-predictor model, `f²` is a monotone transform of
#'     `R²` and adds no information beyond it.
#'   - `"d"`: Cohen's `d = beta_hat / sigma_hat`, where `beta_hat` is the
#'     model coefficient (the displayed difference) and `sigma_hat` is the
#'     residual standard deviation from the fitted model. Defined only when
#'     `by` has exactly two non-empty levels; otherwise the function errors.
#'     The sign matches the displayed `Delta (level2 - level1)`.
#'   - `"g"`: Hedges' `g = J * d` with the small-sample correction
#'     `J = 1 - 3 / (4 * df_resid - 1)`. Same domain as `"d"`.
#'   - `"omega2"`: Hays' `omega-squared`, a bias-corrected estimator of the
#'     population variance explained, less optimistic than `R²` for small
#'     samples. Defined for any predictor type and truncated at 0.
#'
#'   When `weights` is supplied, `"d"`, `"g"`, and `"omega2"` are derived from
#'   the weighted least-squares fit (using weighted sums of squares and the
#'   model's weighted residual standard deviation), keeping them consistent
#'   with the weighted contrast and its CI shown in the table. All effect
#'   sizes are point estimates derived from the OLS/WLS fit and are **not**
#'   affected by `vcov`.
#' @param effect_size_ci Logical. If `TRUE` and `effect_size != "none"`, adds
#'   a confidence interval for the effect size derived from inversion of the
#'   appropriate noncentral distribution (noncentral t for `"d"` / `"g"`;
#'   noncentral F for `"omega2"` / `"f2"`). The CI level is taken from
#'   `ci_level`. In the long output (`output = "long"`), the bounds are always
#'   present in `es_ci_lower` / `es_ci_upper` (numeric). In the wide raw
#'   output (`output = "data.frame"`), the bounds appear as numeric columns
#'   `effect_size_ci_lower` / `effect_size_ci_upper`. In the printed ASCII
#'   table and rendered outputs (`"tinytable"`, `"gt"`, `"flextable"`,
#'   `"word"`, `"excel"`, `"clipboard"`), the effect-size column shows the
#'   value followed by the CI in brackets (e.g. `0.18 [0.07, 0.30]`). Defaults
#'   to `FALSE`. When `effect_size = "none"`, this argument is ignored with a
#'   warning.
#' @param r2 Character. Fit statistic to include in the wide and rendered
#'   outputs. One of:
#'   - `"r2"` (default): the model `R²` (`summary(lm)$r.squared`).
#'   - `"adj_r2"`: adjusted `R²`, penalising for `df_effect` relative to the
#'     residual degrees of freedom.
#'   - `"none"`: omit the fit-statistic column.
#'
#'   When `weights` is supplied, `R²` and adjusted `R²` are the weighted
#'   least-squares versions reported by `summary(lm(..., weights = ...))`.
#' @param ci Logical. If `TRUE`, includes contrast confidence-interval columns
#'   in the wide and rendered outputs when a single contrast is shown.
#'   Defaults to `TRUE`.
#' @param labels An optional named character vector of outcome labels. Names
#'   must match column names in `data`. When `NULL` (the default), labels are
#'   auto-detected from variable attributes; if none are found, the column name
#'   is used.
#' @param ci_level Confidence level for coefficient and model-based mean
#'   intervals (default: `0.95`). Must be between 0 and 1 exclusive.
#' @param digits Number of decimal places for descriptive values, regression
#'   coefficients, and test statistics (default: `2`).
#' @param fit_digits Number of decimal places for model-fit columns (`R²` or
#'   adjusted `R²`) in wide and rendered outputs (default: `2`).
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
#'   are always stripped, following APA convention.
#' @param decimal_mark Character used as decimal separator. Either `"."`
#'   (default) or `","`.
#' @param align Horizontal alignment of numeric columns in the printed
#'   ASCII table and in the `tinytable`, `gt`, `flextable`, `word`, and
#'   `clipboard` outputs. The first column (`Variable`) is always
#'   left-aligned. One of:
#'   - `"decimal"` (default): align numeric columns on the decimal
#'     mark, the standard scientific-publication convention used by
#'     SPSS, SAS, LaTeX `siunitx`, [gt::cols_align_decimal()] and
#'     `tinytable::style_tt(align = "d")`. For engines without a
#'     native decimal-alignment primitive (`flextable`, `word`,
#'     `clipboard`, ASCII print), values are pre-padded with leading
#'     and trailing spaces so the dots line up vertically; the body
#'     of the `flextable`/`word` output additionally uses a monospace
#'     font to make character widths uniform.
#'   - `"center"`: center-align all numeric columns.
#'   - `"right"`: right-align all numeric columns.
#'   - `"auto"`: legacy per-column rule used in spicy < 0.11.0
#'     (center for the descriptive / inferential columns; right for
#'     `n`, `Weighted n`, and `p`).
#'
#'   The `excel` output uses the engine's default alignment in any
#'   case: cell-string padding does not align decimals under
#'   proportional fonts, and writing raw numbers with a numeric
#'   format would require a separate refactor.
#' @param output Output format. One of:
#'   - `"default"`: a printed ASCII table, returned invisibly
#'   - `"data.frame"`: a plain wide `data.frame`
#'   - `"long"`: a raw long `data.frame`
#'   - `"tinytable"` (requires `tinytable`)
#'   - `"gt"` (requires `gt`)
#'   - `"flextable"` (requires `flextable`)
#'   - `"excel"` (requires `openxlsx2`)
#'   - `"clipboard"` (requires `clipr`)
#'   - `"word"` (requires `flextable` and `officer`)
#' @param excel_path File path for `output = "excel"`.
#' @param excel_sheet Sheet name for `output = "excel"` (default:
#'   `"Linear models"`).
#' @param clipboard_delim Delimiter for `output = "clipboard"` (default:
#'   `"\t"`).
#' @param word_path File path for `output = "word"`.
#' @param verbose Logical. If `TRUE`, prints messages about ignored
#'   non-numeric selected outcomes (default: `FALSE`).
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: prints a styled ASCII table and invisibly returns
#'     the underlying long `data.frame` with class
#'     `"spicy_continuous_lm_table"` / `"spicy_table"`.
#'   \item `"data.frame"`: a plain wide `data.frame` with one row per
#'     outcome and numeric columns for means (categorical `by`) or slope
#'     (numeric `by`), optional contrast and CI, optional test statistic,
#'     `p`, fit statistic (`R²` or adjusted `R²`), effect size, optional
#'     `effect_size_ci_lower` / `effect_size_ci_upper` (when
#'     `effect_size_ci = TRUE`), `n`, and `Weighted n`.
#'   \item `"long"`: a raw `data.frame` with one block per outcome and 28
#'     columns covering identification (`variable`, `label`,
#'     `predictor_type`, `predictor_label`, `level`, `reference`),
#'     fitted means and their CI (`emmean`, `emmean_se`, `emmean_ci_lower`,
#'     `emmean_ci_upper`), contrast or slope estimates and CI
#'     (`estimate_type`, `estimate`, `estimate_se`, `estimate_ci_lower`,
#'     `estimate_ci_upper`), inferential output (`test_type`, `statistic`,
#'     `df1`, `df2`, `p.value`), effect size with its CI (`es_type`,
#'     `es_value`, `es_ci_lower`, `es_ci_upper`), fit (`r2`, `adj_r2`),
#'     and sample size (`n`, `weighted_n`).
#'   \item `"tinytable"`: a `tinytable` object.
#'   \item `"gt"`: a `gt_tbl` object.
#'   \item `"flextable"`: a `flextable` object.
#'   \item `"excel"` / `"word"`: writes to disk and returns the file path.
#'   \item `"clipboard"`: copies the wide table and returns it invisibly.
#' }
#'
#' If no numeric outcome columns remain after applying `select`, `exclude`,
#' and `regex`, the function emits a warning and returns an empty
#' `data.frame()` regardless of `output`.
#'
#' @details
#' # Model and outputs
#'
#' `table_continuous_lm()` is designed for article-style bivariate reporting:
#' a single predictor supplied with `by`, and one simple model per selected
#' continuous outcome. The model fit is always `lm(outcome ~ by, ...)`,
#' optionally with `weights`. For categorical predictors, the reported means
#' are model-based fitted means for each level of `by`, and contrasts are
#' derived from the same fitted linear model. For an unweighted
#' `lm(y ~ factor)` with classical variance, the fitted means coincide
#' numerically with empirical subgroup means; the *model-based* qualifier
#' matters because (a) under `weights` the means become weighted
#' least-squares estimates, (b) their CIs are derived from the model `vcov`
#' (classical or `HC*`), and (c) tests, *p*-values, and effect sizes all
#' come from the same fitted model, keeping the table internally consistent.
#'
#' Compared with [table_continuous()], this function is the model-based
#' companion: choose it when you want heteroskedasticity-consistent standard
#' errors (`vcov = "HC*"`), model fit statistics, or case weights via
#' `lm(..., weights = ...)`. Because the function exists to report a fitted
#' model, its inferential output is on by default: `p_value = TRUE` and
#' `r2 = "r2"` are the defaults; set `p_value = FALSE` or `r2 = "none"` to
#' suppress them.
#'
#' # Effect sizes
#'
#' Effect size is selected explicitly via `effect_size` (defaults to
#' `"none"`). All variants are derived from the same fitted model as the
#' displayed coefficients, `R²`, and CIs, so the effect size stays
#' internally consistent with the rest of the table.
#'
#' \itemize{
#'   \item `"f2"`: Cohen's `f² = R² / (1 - R²)` (Cohen 1988). Defined
#'     for any predictor type. For a single-predictor model, `f²` is a
#'     monotone transform of `R²` and adds no information beyond it; its
#'     primary use is in *a priori* power analysis (e.g. G*Power).
#'   \item `"d"`, `"g"`: standardized mean difference (Cohen's *d* or Hedges'
#'     *g*), defined only when `by` has exactly two non-empty levels.
#'     `d = beta_hat / sigma_hat` with `sigma_hat = summary(fit)$sigma` (the
#'     pooled within-group SD for the unweighted two-group case);
#'     `g = J * d` with `J = 1 - 3 / (4 * df_resid - 1)` (Hedges and Olkin
#'     1985). The sign matches the displayed `Delta (level2 - level1)`.
#'     For published reports of two-group comparisons, *g* is the
#'     convention recommended by Hedges and Olkin (1985).
#'   \item `"omega2"`: Hays' \eqn{\omega^2}, computed from weighted
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
#' Confidence intervals for the effect size are available via
#' `effect_size_ci = TRUE` and use the modern noncentral-distribution
#' inversion approach, the consensus standard in commercial statistical
#' software (Stata `esize` / `estat esize`, SAS `PROC TTEST` and
#' `PROC GLM EFFECTSIZE` 14.2+) and in mainstream R packages
#' (`effectsize`, `MOTE`, `TOSTER`, `effsize`):
#' \itemize{
#'   \item `"d"`, `"g"`: noncentral *t* inversion (Steiger and Fouladi 1997;
#'     Goulet-Pelletier and Cousineau 2018). Empirical coverage is nominal
#'     across sample sizes (Cousineau and Goulet-Pelletier 2021), unlike the
#'     older Hedges-Olkin normal approximation which is biased for small
#'     samples. For Hedges' *g* the bounds inherit the *J* small-sample
#'     correction.
#'   \item `"omega2"`, `"f2"`: noncentral *F* inversion (Steiger 2004).
#'     Bounds are converted from the noncentrality parameter using
#'     `omega² = ncp / (ncp + N)` and `f² = ncp / N` respectively, with
#'     `N = df1 + df2 + 1` (total sample size).
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
#' Satterthwaite degrees of freedom in `df2`, which the displayed
#' `t(df)` / `F(df1, df2)` header renders to one decimal. `"CR1"`
#' matches Stata's `, vce(cluster id)`. Effect sizes remain invariant
#' to `vcov` (including `CR*`); only the SE, CI, test statistic, and
#' `df2` of the contrast change.
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
#' if more than half fail and a fallback to the classical OLS variance
#' below 10 valid replicates. Inference for both estimators is
#' asymptotic (`z` for single-coefficient contrasts, `chi^2(q)` for the
#' multi-coefficient global Wald test on `k > 2` categorical
#' predictors), reflected in the displayed test header. Use the
#' bootstrap when the residual distribution is non-standard or the
#' sample is small; use the jackknife as a closed-form, deterministic
#' alternative.
#'
#' `R²`, adjusted `R²`, and the effect sizes remain ordinary
#' least-squares (or weighted least-squares) statistics regardless of
#' `vcov`.
#'
#' # Weights
#'
#' When `weights` is supplied, `table_continuous_lm()` fits weighted
#' linear models via `lm(..., weights = ...)`. Means become weighted
#' least-squares estimates and contrasts and slopes are weighted. The
#' fit statistics `R²` and adjusted `R²`, as well as Hays' `omega²`
#' and Cohen's `f²`, use the corresponding **weighted sums of squares**
#' from the WLS fit. Cohen's `d` and Hedges' `g` use the **WLS
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
#' For dichotomous categorical predictors, the wide outputs report fitted
#' means in reference-level order and label the contrast column
#' explicitly as `Delta (level2 - level1)`. For categorical predictors
#' with more than two levels, no single contrast or contrast CI is shown
#' in the wide outputs; instead, the table reports level-specific means
#' plus the overall `F` test when `statistic = TRUE` (or `F(df1, df2)`
#' when the degrees of freedom are constant across outcomes).
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
#' Cousineau, D., & Goulet-Pelletier, J.-C. (2021).
#'   Expected and empirical coverages of different methods for generating
#'   noncentral *t* confidence intervals for a standardized mean
#'   difference. *Behavior Research Methods*, **53**, 2376--2394.
#'   \doi{10.3758/s13428-021-01550-4}
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
#' # Default: ASCII table with model-based means, p, and R².
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
#'   effect_size = "d"
#' )
#'
#' # Hedges' g with weighted analysis and weighted n column.
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   weights = weight,
#'   statistic = TRUE,
#'   effect_size = "g",
#'   show_weighted_n = TRUE
#' )
#'
#' # Hedges' g with noncentral t confidence interval (bracket notation).
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   effect_size = "g",
#'   effect_size_ci = TRUE
#' )
#'
#' # Cohen's f² alongside R² (familiar power-analysis effect size).
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   effect_size = "f2"
#' )
#'
#' # Hays' omega-squared for a 3-level predictor (d / g would error here).
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
#' table_continuous_lm(
#'   sleep,
#'   select = extra,
#'   by = group,
#'   cluster = ID,
#'   vcov = "CR2"
#' )
#'
#' # --- Article-style polish -----------------------------------------------
#'
#' # Pretty outcome labels and adjusted R².
#' table_continuous_lm(
#'   sochealth,
#'   select = c(wellbeing_score, bmi),
#'   by = sex,
#'   labels = c(
#'     wellbeing_score = "WHO-5 wellbeing (0-100)",
#'     bmi = "Body-mass index (kg/m²)"
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
  exclude = NULL,
  regex = FALSE,
  weights = NULL,
  vcov = c(
    "classical",
    "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5",
    "CR0", "CR1", "CR2", "CR3",
    "bootstrap", "jackknife"
  ),
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
  excel_sheet = "Linear models",
  clipboard_delim = "\t",
  word_path = NULL,
  verbose = FALSE
) {
  if (!is.data.frame(data)) {
    spicy_abort("`data` must be a data.frame.", class = "spicy_invalid_data")
  }
  if (
    !is.numeric(ci_level) ||
      length(ci_level) != 1L ||
      is.na(ci_level) ||
      ci_level <= 0 ||
      ci_level >= 1
  ) {
    spicy_abort("`ci_level` must be a single number between 0 and 1.", class = "spicy_invalid_input")
  }
  if (
    !is.numeric(digits) ||
      length(digits) != 1L ||
      is.na(digits) ||
      digits < 0
  ) {
    spicy_abort("`digits` must be a single non-negative number.", class = "spicy_invalid_input")
  }
  digits <- as.integer(digits)
  if (
    !is.numeric(fit_digits) ||
      length(fit_digits) != 1L ||
      is.na(fit_digits) ||
      fit_digits < 0
  ) {
    spicy_abort("`fit_digits` must be a single non-negative number.", class = "spicy_invalid_input")
  }
  fit_digits <- as.integer(fit_digits)
  if (
    !is.numeric(effect_size_digits) ||
      length(effect_size_digits) != 1L ||
      is.na(effect_size_digits) ||
      effect_size_digits < 0
  ) {
    spicy_abort(
      "`effect_size_digits` must be a single non-negative number.", class = "spicy_invalid_input")
  }
  effect_size_digits <- as.integer(effect_size_digits)
  if (
    !is.numeric(p_digits) ||
      length(p_digits) != 1L ||
      is.na(p_digits) ||
      p_digits < 1
  ) {
    spicy_abort(
      "`p_digits` must be a single positive integer (>= 1).", class = "spicy_invalid_input")
  }
  p_digits <- as.integer(p_digits)
  if (
    !is.numeric(boot_n) ||
      length(boot_n) != 1L ||
      is.na(boot_n) ||
      boot_n < 50
  ) {
    spicy_abort(
      "`boot_n` must be a single positive integer (>= 50).", class = "spicy_invalid_input")
  }
  boot_n <- as.integer(boot_n)
  if (!decimal_mark %in% c(".", ",")) {
    spicy_abort('`decimal_mark` must be "." or ",".', class = "spicy_invalid_input")
  }
  if (!is.null(labels) && (!is.character(labels) || is.null(names(labels)))) {
    spicy_abort("`labels` must be a named character vector.", class = "spicy_invalid_input")
  }
  for (.arg in c(
    "regex",
    "verbose",
    "statistic",
    "p_value",
    "show_n",
    "show_weighted_n",
    "ci",
    "effect_size_ci"
  )) {
    .val <- get(.arg)
    if (!is.logical(.val) || length(.val) != 1L || is.na(.val)) {
      spicy_abort(sprintf("`%s` must be TRUE/FALSE.", .arg), class = "spicy_invalid_input")
    }
  }

  output <- match.arg(output)
  vcov <- match.arg(vcov)
  contrast <- match.arg(contrast)
  effect_size <- match.arg(effect_size)
  r2 <- match.arg(r2)
  align <- match.arg(align)

  by_quo <- rlang::enquo(by)
  by_name <- resolve_single_column_selection(by_quo, data, "by")
  by_vector <- data[[by_name]]
  if (!is_supported_lm_predictor(by_vector)) {
    spicy_abort(
      "`by` must be numeric, logical, character, or factor.", class = "spicy_invalid_input")
  }

  if (effect_size %in% c("d", "g")) {
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
        ), class = "spicy_invalid_input")
    }
  }

  if (isTRUE(effect_size_ci) && identical(effect_size, "none")) {
    spicy_warn(
      "`effect_size_ci` is ignored when `effect_size = \"none\"`.", class = "spicy_ignored_arg")
    effect_size_ci <- FALSE
  }

  weights_quo <- rlang::enquo(weights)
  weights_name <- detect_weights_column_name(weights_quo, data)
  weights_vec <- resolve_weights_argument(weights_quo, data, "weights")
  if (!is.null(weights_vec)) {
    if (any(!is.finite(weights_vec), na.rm = TRUE)) {
      spicy_abort("`weights` must contain only finite values.", class = "spicy_invalid_input")
    }
    if (any(weights_vec < 0, na.rm = TRUE)) {
      spicy_abort("`weights` must be non-negative.", class = "spicy_invalid_input")
    }
    if (all(is.na(weights_vec) | weights_vec == 0)) {
      spicy_abort("`weights` must contain at least one positive value.", class = "spicy_invalid_input")
    }
  }
  if (isTRUE(show_weighted_n) && is.null(weights_vec)) {
    spicy_warn(
      "`show_weighted_n` is ignored when `weights` is not supplied.", class = "spicy_ignored_arg")
    show_weighted_n <- FALSE
  }

  cluster_quo <- rlang::enquo(cluster)
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
      ), class = "spicy_invalid_input")
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
      ), class = "spicy_invalid_input")
  }
  if (is_cr_vcov && !requireNamespace("clubSandwich", quietly = TRUE)) {
    spicy_abort(
      sprintf(
        paste0(
          "`vcov = \"%s\"` requires the 'clubSandwich' package. ",
          "Install it with install.packages(\"clubSandwich\")."
        ),
        vcov
      ), class = "spicy_invalid_input")
  }
  if (!is.null(cluster_vec) &&
        length(unique(stats::na.omit(cluster_vec))) < 2L) {
    spicy_abort(
      "`cluster` must contain at least two distinct non-missing values.", class = "spicy_invalid_input")
  }

  available_names <- names(data)
  excluded_names <- resolve_multi_column_selection(
    rlang::enquo(exclude),
    data,
    "exclude"
  )
  excluded_names <- unique(c(excluded_names, by_name, weights_name, cluster_name))

  if (isTRUE(regex)) {
    select_val <- tryCatch(
      rlang::eval_tidy(select),
      error = function(e) NULL
    )
    if (
      !is.character(select_val) || length(select_val) != 1L || is.na(select_val)
    ) {
      spicy_abort(
        "When `regex = TRUE`, `select` must be a single regex pattern.", class = "spicy_invalid_input")
    }
    selected_names <- grep(select_val, available_names, value = TRUE)
  } else {
    select_quo <- rlang::enquo(select)
    selected_pos <- tryCatch(
      tidyselect::eval_select(select_quo, data),
      error = function(e) {
        spicy_abort("`select` must select columns in `data`.", class = "spicy_invalid_input")
      }
    )
    selected_names <- names(selected_pos)
  }

  selected_names <- setdiff(selected_names, excluded_names)
  numeric_outcomes <- selected_names[vapply(
    selected_names,
    function(nm) is.numeric(data[[nm]]),
    logical(1)
  )]
  ignored_names <- setdiff(selected_names, numeric_outcomes)
  if (length(ignored_names) > 0L && isTRUE(verbose)) {
    rlang::inform(
      paste0(
        "Ignoring non-numeric selected outcomes: ",
        paste(ignored_names, collapse = ", ")
      )
    )
  }

  if (length(numeric_outcomes) == 0L) {
    spicy_warn("No numeric outcome columns selected.", class = "spicy_no_selection")
    return(data.frame())
  }

  outcome_labels <- vapply(
    numeric_outcomes,
    function(nm) {
      if (!is.null(labels) && nm %in% names(labels)) {
        return(labels[[nm]])
      }
      lab <- attr(data[[nm]], "label", exact = TRUE)
      if (is.null(lab) || !nzchar(lab)) nm else lab
    },
    character(1)
  )
  by_label <- attr(data[[by_name]], "label", exact = TRUE)
  if (is.null(by_label) || !nzchar(by_label)) {
    by_label <- by_name
  }

  rows <- lapply(
    seq_along(numeric_outcomes),
    function(i) {
      fit_outcome_lm_rows(
        y = data[[numeric_outcomes[i]]],
        predictor = by_vector,
        weights = weights_vec,
        cluster = cluster_vec,
        outcome_name = numeric_outcomes[i],
        outcome_label = outcome_labels[i],
        predictor_label = by_label,
        vcov_type = vcov,
        contrast = contrast,
        ci_level = ci_level,
        effect_size = effect_size,
        boot_n = boot_n
      )
    }
  )
  result <- do.call(rbind, rows)
  rownames(result) <- NULL

  attr(result, "ci_level") <- ci_level
  attr(result, "digits") <- digits
  attr(result, "fit_digits") <- fit_digits
  attr(result, "effect_size_digits") <- effect_size_digits
  attr(result, "p_digits") <- p_digits
  attr(result, "decimal_mark") <- decimal_mark
  attr(result, "by_var") <- by_name
  attr(result, "by_label") <- by_label
  attr(result, "vcov_type") <- vcov
  attr(result, "contrast") <- contrast
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

  if (identical(output, "long")) {
    return(result)
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
    ci_level = ci_level
  )
  if (identical(output, "data.frame")) {
    return(wide_raw)
  }

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
    ci = ci
  )

  if (identical(output, "default")) {
    class(result) <- c(
      "spicy_continuous_lm_table",
      "spicy_table",
      class(result)
    )
    print(result)
    return(invisible(result))
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
    word_path = word_path
  )
}

fit_outcome_lm_rows <- function(
  y,
  predictor,
  weights,
  cluster = NULL,
  outcome_name,
  outcome_label,
  predictor_label,
  vcov_type,
  contrast,
  ci_level,
  effect_size = "none",
  boot_n = 1000L
) {
  keep <- !is.na(y) & !is.na(predictor)
  if (!is.null(weights)) {
    keep <- keep & !is.na(weights)
  }
  if (!is.null(cluster)) {
    keep <- keep & !is.na(cluster)
  }

  y <- y[keep]
  predictor <- predictor[keep]
  weights <- if (is.null(weights)) NULL else weights[keep]
  cluster <- if (is.null(cluster)) NULL else cluster[keep]

  if (is.numeric(predictor)) {
    return(
      fit_numeric_predictor_lm_rows(
        y = y,
        x = predictor,
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
    weights = weights,
    cluster = cluster,
    outcome_name = outcome_name,
    outcome_label = outcome_label,
    predictor_label = predictor_label,
    vcov_type = vcov_type,
    contrast = contrast,
    ci_level = ci_level,
    effect_size = effect_size,
    boot_n = boot_n
  )
}

fit_numeric_predictor_lm_rows <- function(
  y,
  x,
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
    return(make_empty_lm_rows(
      outcome_name,
      outcome_label,
      "continuous",
      predictor_label = predictor_label
    ))
  }

  model_df <- data.frame(y = y, x = x)
  fit <- if (is.null(weights)) {
    stats::lm(y ~ x, data = model_df)
  } else {
    stats::lm(y ~ x, data = model_df, weights = weights)
  }

  vc <- compute_lm_vcov(
    fit,
    vcov_type,
    cluster = cluster,
    weights = weights,
    boot_n = boot_n
  )
  model_stats <- compute_lm_model_stats(fit)

  inf <- compute_lm_coef_inference(
    fit,
    coef_idx = 2L,
    vc = vc,
    vcov_type = vcov_type,
    cluster = cluster,
    ci_level = ci_level
  )

  es_ci <- compute_es_ci_lm(fit, effect_size, ci_level)

  data.frame(
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
    stringsAsFactors = FALSE
  )
}

fit_categorical_predictor_lm_rows <- function(
  y,
  x,
  weights,
  cluster = NULL,
  outcome_name,
  outcome_label,
  predictor_label,
  vcov_type,
  contrast,
  ci_level,
  effect_size = "none",
  boot_n = 1000L
) {
  x <- droplevels(coerce_lm_factor(x))
  if (length(y) < 2L || nlevels(x) < 2L) {
    return(make_empty_lm_rows(
      outcome_name,
      outcome_label,
      "categorical",
      predictor_label = predictor_label
    ))
  }

  model_df <- data.frame(y = y, x = x)
  fit <- if (is.null(weights)) {
    stats::lm(y ~ x, data = model_df)
  } else {
    stats::lm(y ~ x, data = model_df, weights = weights)
  }

  vc <- compute_lm_vcov(
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
  model_stats <- compute_lm_model_stats(fit)

  levs <- levels(x)
  newdata <- data.frame(x = factor(levs, levels = levs))
  design <- stats::model.matrix(
    stats::delete.response(stats::terms(fit)),
    newdata
  )
  emmean <- as.vector(design %*% cf)
  emmean_se <- sqrt(rowSums((design %*% vc) * design))

  # Global Wald F (k > 2 ANOVA-style, or t² for binary). Dispatches
  # to clubSandwich::Wald_test() with HTZ/Satterthwaite for CR mode.
  wald <- compute_lm_wald_test(
    fit,
    coef_idx_set = seq_along(cf)[-1],
    vc = vc,
    vcov_type = vcov_type,
    cluster = cluster
  )

  show_reference <- identical(contrast, "auto") && nlevels(x) == 2L

  es_ci <- compute_es_ci_lm(fit, effect_size, ci_level)

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
    weighted_n = rep(if (is.null(weights)) NA_real_ else sum(weights), length(levs)),
    stringsAsFactors = FALSE
  )

  if (isTRUE(show_reference)) {
    # Per-level contrast (binary case): use the same single-coef
    # inference helper as for numeric predictors so CR mode picks
    # up Satterthwaite df automatically.
    for (i in seq_len(nlevels(x) - 1L)) {
      coef_idx <- i + 1L
      row_idx <- i + 1L
      inf <- compute_lm_coef_inference(
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

  out
}

make_empty_lm_rows <- function(
  outcome_name,
  outcome_label,
  predictor_type,
  predictor_label = NA_character_
) {
  data.frame(
    variable = outcome_name,
    label = outcome_label,
    predictor_type = predictor_type,
    predictor_label = predictor_label,
    level = NA_character_,
    reference = NA_character_,
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
    stringsAsFactors = FALSE
  )
}
