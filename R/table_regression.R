#' Regression coefficient summary table
#'
#' @description
#' Build a publication-ready summary table from one or more fitted
#' regression models. Designed for **APA-strict** reporting in
#' psychology, public health, sociology, and biostatistics, with
#' optional opt-in conventions for econometrics and clinical trials.
#'
#' Supports `lm` and `glm` (binomial / poisson / Gamma /
#' inverse.gaussian / quasi families with any link). Mixed-effects
#' models (`lmerMod`, `glmerMod`) are planned for spicy 0.16+.
#'
#' @details
#' # Vocabulary tokens
#'
#' Three vector arguments — `show_columns`, `show_fit_stats`, and
#' `nested_stats` — accept named tokens that select **what** to
#' display and in **what order**. Tokens use snake_case (lowercase),
#' with capitalisation preserved only for canonical statistical
#' abbreviations (`AIC`, `BIC`, `F`, `LRT`, `B`, `SE`, `CI`, `AME`).
#'
#' ## `show_columns` — per-coefficient columns (in main table)
#'
#' Estimate-related: `B`, `beta`, `SE`, `CI`, `t`, `p`.
#' Effect sizes (lm only): `partial_f2`, `partial_eta2`,
#' `partial_omega2` (compact `value [CI]` rendering).
#' Effect sizes (glm only): `partial_chi2` — term-level partial
#' likelihood-ratio chi-square via `drop1(test = "LRT")`, the
#' generalised analog of the partial F-test (SAS PROC LOGISTIC
#' `TYPE3`; Long & Freese 2014 §3.5; Allison "TYPE3"). Compact
#' `value (df)` rendering; the df slot disambiguates factor terms
#' (k-1 df) from numeric terms (1 df).
#' Marginal effects: `AME`, `AME_p`, `AME_SE` (compact for `AME`).
#'
#' Default: `c("B", "SE", "CI", "p")`.
#' If `standardized != "none"` and `"beta"` is not in `show_columns`,
#' it is auto-injected after `B`. Asking for `"beta"` while
#' `standardized = "none"` raises `spicy_invalid_input`.
#'
#' ## `show_fit_stats` — model-level statistics (in footer)
#'
#' Counts: `nobs`, `weighted_nobs`.
#' Variance explained (`lm` only): `r2`, `adj_r2`, `omega2`.
#' Pseudo-\eqn{R^2}{R^2} (`glm` only): `pseudo_r2_mcfadden` (McFadden 1974),
#' `pseudo_r2_nagelkerke` (Nagelkerke 1991),
#' `pseudo_r2_tjur` (Tjur 2009; binomial only).
#' Residual scale: `sigma` (lm \eqn{\hat{\sigma}}{sigma-hat} / glm dispersion), `rmse`.
#' Effect size: `f2`.
#' Information criteria: `AIC`, `AICc`, `BIC`, `deviance`.
#'
#' Default is class-aware (resolved when `NULL`): for `lm`,
#' `c("nobs", "r2", "adj_r2")`; for `glm`,
#' `c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "AIC")`.
#'
#' ## `nested_stats` — hierarchical comparison footer
#'
#' Activates when `nested = TRUE`. Tokens: `r2_change`,
#' `adj_r2_change`, `F`, `f2_change` (`lm` only), `LRT`, `AIC`,
#' `AICc`, `BIC`, `deviance_change`, `p`.
#'
#' Default is class-aware (resolved when `NULL`): for `lm`
#' hierarchies, `c("r2_change", "F", "p")` (APA hierarchical
#' regression standard); for `glm` hierarchies, `c("LRT", "p")`
#' (APA hierarchical-logistic standard; Hosmer & Lemeshow §3.5;
#' Long & Freese 2014 §3.6). Variance-explained tokens (`r2_change`,
#' `adj_r2_change`, `F`, `f2_change`) are not defined for `glm` and
#' raise `spicy_invalid_input` if requested for an all-`glm`
#' hierarchy.
#'
#' # Multi-model semantics
#'
#' Pass a single `lm` fit or a `list()` of fits. A named list
#' (`list("Naive" = m1, "Adjusted" = m2)`) provides column headers
#' automatically; an unnamed list defaults to `"Model 1, 2, ..."`.
#' `model_labels` overrides both with explicit per-column labels.
#'
#' For models with **different response variables**, the DV name is
#' shown automatically in a header row (controllable via
#' `outcome_labels`). For models sharing the same DV, the DV is
#' mentioned in the table title only.
#'
#' # Inference and standard errors
#'
#' `vcov` selects the variance-covariance estimator: `"classical"`
#' (OLS), `"HC0"` to `"HC5"` (heteroskedasticity-consistent via
#' [sandwich::vcovHC()]), `"CR0"` to `"CR3"` (cluster-robust via
#' [clubSandwich::vcovCR()] with Satterthwaite-corrected df),
#' `"bootstrap"` (nonparametric or cluster bootstrap), or
#' `"jackknife"` (leave-one-out / leave-one-cluster-out).
#'
#' For multi-model use, both `vcov` and `cluster` accept either a
#' single value (recycled to all models) **or** a list (one per
#' model). The pedagogical use case `list(fit, fit, fit)` with
#' `vcov = list("classical", "HC3", "CR2")` enables side-by-side
#' SE-comparison in a single table.
#'
#' Inferential regimes (B and AME share the same regime by design):
#'   * `classical`, `HC*` → t with `df.residual`
#'   * `bootstrap`, `jackknife` → z asymptotic
#'   * `CR0–CR3` → t with **Satterthwaite-corrected df** (B via
#'     [clubSandwich::coef_test()]; AME via
#'     [clubSandwich::linear_contrast()] — see
#'     Pustejovsky & Tipton 2018). spicy is the first R package to
#'     offer AME-Satterthwaite under cluster-robust variance for
#'     `lm`. For models with non-linear formulas (`poly()`, `I()`,
#'     `log()`, `splines::ns()`), AME inference falls back to
#'     z-asymptotic with a `spicy_fallback` warning.
#'
#' # Standardised coefficients
#'
#' `standardized` controls the standardisation method when `"beta"`
#' is in `show_columns`:
#'   * `"refit"` — refit the model on z-scored predictors. For `lm`,
#'     the response is also z-scored (Cohen et al. 2003 gold standard);
#'     for `glm`, the response stays on its observed scale and only
#'     numeric predictors are standardised (Long & Freese 2014 §4.3.4
#'     "x-standardization").
#'   * `"posthoc"` — post-hoc scaling. For `lm`:
#'     `\eqn{\beta}{beta} = B × SD(X) / SD(Y)`; for `glm`: X-only `\eqn{\beta}{beta} = B × SD(X)` (the
#'     response side is undefined on the link scale —
#'     `parameters` / `effectsize` convention).
#'   * `"basic"` — like posthoc but factor-derived dummies are scaled
#'     by their column SD (treated as numeric).
#'   * `"smart"` — Gelman (2008) recommendation: divide binary
#'     predictors by `2 × SD` instead of `SD`.
#'   * `"pseudo"` — *glm only*. Menard (2004, 2011) fully-standardised
#'     `\eqn{\beta}{beta} = B × SD(X) / SD(Y*)`, where `Y*` is the latent variable on
#'     the link scale and
#'     `SD(Y*) = sqrt(var(\eqn{\hat{\eta}}{eta-hat}) + var_link)` with `var_link` =
#'     \eqn{\pi}{pi}²/3 (logit), 1 (probit), \eqn{\pi}{pi}²/6 (cloglog). Defined for binomial
#'     families; non-binomial returns NA with a `spicy_caveat`.
#'   * `"none"` (default) — no \eqn{\beta}{beta} computed.
#'
#' For models with interactions or transformed predictors (`I()`,
#' `poly()`, `log()`, `splines::ns()`), a `spicy_caveat` warning is
#' emitted reminding that standardised coefficients on such terms
#' should be interpreted with care (Cohen et al. 2003 §7.7;
#' Aiken & West 1991). The footer auto-documents the caveat.
#'
#' # Hierarchical (nested) model comparison
#'
#' Set `nested = TRUE` to add a footer block comparing each model
#' to the previous one. spicy validates that all models share
#' identical `nobs` AND identical response variable, raising
#' `spicy_invalid_input` otherwise (with a remediation snippet).
#'
#' Class-aware default `nested_stats` for `lm`:
#' `c("r2_change", "F", "p")`. Customise via the `nested_stats`
#' argument with any combination of supported tokens.
#'
#' # Output formats and broom integration
#'
#' `output` selects the return type:
#'   * `"default"` — a `spicy_regression_table` object (a
#'     `data.frame` subclass) printed via [spicy_print_table()].
#'   * `"data.frame"` / `"long"` — raw data.frame / long-format
#'     tibble (the underlying analytic representation).
#'   * `"gt"` / `"flextable"` / `"tinytable"` — rich-format HTML /
#'     Word / PDF tables (requires the corresponding package).
#'   * `"excel"` — writes to `excel_path` via
#'     [openxlsx2::write_xlsx()] (returns `invisible(x)`).
#'   * `"clipboard"` — copies to system clipboard via
#'     [clipr::write_clip()] (returns `invisible(x)`).
#'
#' [broom::tidy()] returns a long tibble with one row per
#' `(model_id, term, estimate_type)` and broom-canonical column
#' names (`estimate`, `std.error`, `conf.low`, `conf.high`,
#' `statistic`, `p.value`). [broom::glance()] returns one row per
#' model with the model-level statistics. `df.residual` is kept
#' numeric (not integer) so cluster-robust Satterthwaite df is
#' preserved verbatim.
#'
#' # Output encoding
#'
#' `table_regression()` produces UTF-8 output with Unicode
#' box-drawing for table layout and Greek / mathematical symbols
#' (\eqn{\beta}{beta}, \eqn{\omega^2}{omega^2}, \eqn{\chi^2}{chi-squared}, \eqn{f^2}{f^2}, \eqn{\Delta}{Delta}, †, em-dash). A UTF-8 capable terminal is
#' required (default in RStudio, R \eqn{\ge}{>=} 4.0 on Windows 10+, macOS,
#' modern Linux).
#'
#' # Internationalisation
#'
#' Output is in English. To localise visible text:
#'   * Use `reference_label`, `model_labels`, `outcome_labels`,
#'     and `labels` to override user-customisable strings.
#'   * For the title and footer, post-process the result via
#'     `attr(result, "title") <- "..."` and
#'     `attr(result, "note") <- "..."`.
#'
#' # Weights
#'
#' There is **no** `weights` argument. Weights are a property of
#' the fitted model (extracted via [stats::weights()]); pass them
#' when fitting:
#' \preformatted{
#' fit <- lm(y ~ x, data = df, weights = w)
#' table_regression(fit)
#' }
#' All downstream computations (vcov, AME, standardisation,
#' `weighted_nobs`) extract the weights automatically.
#'
#' @param models An `lm` or `glm` fitted model **or** a list of
#'   such fits (named or unnamed; lm and glm fits may be mixed in
#'   the same list). Single fits are auto-promoted to a one-element
#'   list internally. `merMod` and other classes are rejected with
#'   an actionable error. Raw data + formula is **not** accepted
#'   (fit-only API, matching `broom`, `modelsummary`, `gtsummary`,
#'   and `marginaleffects`).
#' @param vcov Variance-covariance estimator. A single string
#'   (one of `"classical"`, `"HC0"` to `"HC5"`, `"CR0"` to
#'   `"CR3"`, `"bootstrap"`, `"jackknife"`) recycled to all models,
#'   **or** a list of strings (one per model) for the pedagogical
#'   side-by-side SE-comparison use case. Default `"classical"`.
#' @param cluster Cluster identifier for cluster-robust variance
#'   (`vcov` in `"CR0"` to `"CR3"` or `"bootstrap"` /
#'   `"jackknife"` cluster forms). Either an unquoted column name,
#'   a single string column name, an atomic vector of length
#'   `nobs(model)`, or a list (one per model). Default `NULL`
#'   (no clustering).
#' @param ci_level Confidence level for all reported CIs (B, \eqn{\beta}{beta},
#'   AME, partial effect sizes). Default `0.95`.
#' @param boot_n Number of bootstrap replicates when
#'   `vcov = "bootstrap"`. Single positive integer. Default
#'   `1000L`.
#' @param ci_method CI construction method. `"wald"` (default) uses
#'   the symmetric Wald formula `estimate ± z × SE` (or `t × SE` for
#'   `lm`); for `glm` this matches `summary.glm` and `confint.default`.
#'   `"profile"` (*glm only*) uses the profile-likelihood CI from
#'   `MASS::confint.glm()` — asymmetric, exact for likelihood-based
#'   inference, and the gold standard under sparse data or near-
#'   boundary estimates (Venables & Ripley *MASS* §7.2). The estimate,
#'   SE, statistic and p-value remain Wald; `"profile"` only refines
#'   the CI bounds. Using `"profile"` with `lm` raises
#'   `spicy_invalid_input`.
#' @param standardized Standardisation method for the `"beta"`
#'   column. One of `"none"` (default), `"refit"`, `"posthoc"`,
#'   `"basic"`, `"smart"`, `"pseudo"`. `"pseudo"` is *glm only*
#'   (Menard 2011 fully-standardised); using it with `lm()` raises
#'   `spicy_invalid_input`. See the *Standardised coefficients*
#'   section.
#' @param exponentiate Logical. When `TRUE` and the model is a
#'   `glm()` with a non-identity link, the displayed coefficient
#'   (`B`), confidence interval bounds, and standard error are
#'   transformed to the response scale via `exp()`. The column
#'   header is rebranded per family / link: `OR` for binomial(logit),
#'   `IRR` for poisson(log), `HR` for binomial(cloglog), `RR` for
#'   binomial(log), `MR` for Gamma(log), and the generic `exp(B)`
#'   otherwise. The standard error follows the delta-method
#'   approximation `SE_OR = OR × SE_log-odds` (Stata `logit, or`,
#'   `parameters::model_parameters()`); the test statistic and the
#'   p-value remain on the link scale (both are invariant under
#'   monotone transformation). Default `FALSE` (parameters /
#'   modelsummary convention — explicit opt-in over silent transform).
#'   Has no effect on `lm()` fits or on identity-link glm; emits a
#'   `spicy_ignored_arg` warning in those cases.
#' @param p_adjust Multiple-comparison adjustment method applied to
#'   the family of estimated coefficient p-values within each model
#'   (intercept and reference rows excluded by convention). One of
#'   `"none"` (default — no adjustment), `"holm"`, `"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`, or `"BY"`. The
#'   adjustment is delegated to [stats::p.adjust()] and applied
#'   per-model and per `estimate_type` (so B and AME p-values are
#'   adjusted independently within their own families). When active,
#'   a footer note documents the method and the family size. The
#'   adjustment runs **before** any `keep` / `drop` filtering so the
#'   family is the model's full coefficient set, not just the
#'   displayed subset (modelsummary convention).
#'
#'   **Methodological note.** Adjusting the p-values of all
#'   coefficients of a single regression model is *not* the standard
#'   convention in social-science / clinical reporting. Each
#'   coefficient tests a scientifically distinct hypothesis on a
#'   distinct predictor, which is not the situation multiple-testing
#'   procedures were designed for (Rothman 1990; Greenland 2017).
#'   The default `"none"` reflects this: APA Manual 7 §6.46,
#'   Harrell (*Regression Modeling Strategies*, §5.4), and Gelman,
#'   Hill & Yajima (2012) all recommend reporting unadjusted
#'   p-values for the coefficients of a pre-specified regression
#'   model. Cases where adjustment IS appropriate include mass
#'   screening with many candidate predictors and no prior
#'   hypothesis (typically `"BH"` / FDR), pre-registered
#'   multi-endpoint confirmatory designs (typically `"holm"`), or
#'   when a journal / SAP explicitly requests it. spicy exposes the
#'   argument under the same "transparency over rejection" rule
#'   used for `standardized`: the tool is available, the
#'   methodological choice is yours.
#' @param show_columns Character vector of tokens controlling
#'   which per-coefficient columns to display, **and** in which
#'   order. See the *Vocabulary tokens* section. Default
#'   `c("B", "SE", "CI", "p")`.
#' @param keep Character vector of regular expressions; when
#'   supplied, only coefficient rows whose term name matches at
#'   least one of the patterns are kept. Useful when a model has
#'   many control variables and you want to display only the focal
#'   predictors. Mutually exclusive with `drop`. Matching is
#'   applied to the coefficient name as it appears in
#'   [stats::coef()] (e.g., `"wt"`, `"cyl6"`, `"factor(cyl)8"`).
#'   `p_adjust` is computed before the filter, so adjusted p-values
#'   reflect the model's full coefficient family. Default `NULL`
#'   (no filter).
#' @param drop Character vector of regular expressions; when
#'   supplied, coefficient rows whose term name matches any of the
#'   patterns are removed. Mutually exclusive with `keep`. Useful
#'   for hiding control variables from a publication-ready table.
#'   Same matching semantics as `keep`. Default `NULL` (no filter).
#' @param show_intercept Whether to display the intercept row.
#'   Default `TRUE` (APA convention). Hide via `FALSE`.
#' @param intercept_position Where to place the intercept when
#'   shown. `"first"` (default, APA) or `"last"` (Stata-style,
#'   intercept just above the fit-stats footer). Ignored when
#'   `show_intercept = FALSE` (with `spicy_ignored_arg` warning).
#' @param group_factor_levels Whether to render factor predictors
#'   as a header row + indented contrast levels. `TRUE` (default,
#'   APA / gtsummary style) or `FALSE` (flat `factor: level` rows).
#' @param reference_style Rendering of factor reference levels.
#'   `"row"` (default) shows an explicit row `Lower (ref.)` with
#'   em-dashes in all stat columns (gtsummary / clinical style).
#'   `"annotation"` puts `[ref: Lower]` in the factor header
#'   (compact mode).
#' @param reference_label String used to mark the reference level
#'   in `reference_style = "row"` mode. Default `"(ref.)"`.
#'   Customise to `"(reference)"` or `"(réf.)"` etc.
#' @param show_fit_stats Character vector of tokens for the
#'   model-level fit-stats footer, or `NULL` (default) to apply
#'   the class-aware default. For all-`lm` models:
#'   `c("nobs", "r2", "adj_r2")`. For all-`glm` models:
#'   `c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "AIC")`.
#'   Mixed `lm` + `glm` sets: union of both groups (renderer
#'   per-row em-dashes the inappropriate cell). See
#'   *Vocabulary tokens* for the full token list.
#' @param model_labels Column / model labels for multi-model
#'   tables. `NULL` (default) uses smart auto-generation: hidden
#'   for a single model, `"Model 1, 2, 3"` or `names(list)` for
#'   multiple models. A character vector of length `length(models)`
#'   forces explicit per-column labels.
#' @param outcome_labels Per-model response-variable row.
#'   `NULL` (default) uses smart auto: row hidden when all DVs
#'   identical, row shown when DVs differ. Auto-derived from
#'   `attr(data[[dv]], "label")` if present, else from
#'   `formula(fit)[[2]]`. A character vector of length
#'   `length(models)` forces explicit labels. `FALSE` suppresses
#'   the row entirely.
#' @param stars Significance asterisk display.
#'   `FALSE` (default, APA-aligned) — no stars; p-values reported
#'   as numbers only. APA 7 §6.46 explicitly discourages stars.
#'   `TRUE` — APA-conventional cutoffs
#'   `c("*" = 0.05, "**" = 0.01, "***" = 0.001)`.
#'   A named numeric vector — custom thresholds and symbols (e.g.
#'   `c("†" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001)`).
#' @param nested Whether to add a hierarchical-comparison footer
#'   block (each model vs the previous via [stats::anova()] or
#'   equivalent). `FALSE` (default) — pure side-by-side display
#'   without comparison statistics. `TRUE` — adds the comparison
#'   footer; requires identical `nobs` and identical response
#'   variable across all models.
#' @param nested_stats Tokens controlling which comparison
#'   statistics to display when `nested = TRUE`. `NULL` (default)
#'   selects the class-aware default (`c("r2_change", "F", "p")`
#'   for `lm`). See *Vocabulary tokens*.
#' @param digits Number of decimal places for general numeric
#'   tokens (`B`, `beta`, `SE`, `CI`, `t`, `F`, `LRT`, `deviance`,
#'   `deviance_change`, `AME`, `AME_SE`, `weighted_nobs`).
#'   Default `2L`.
#' @param p_digits Decimal places for p-values (`p`, `AME_p`, and
#'   `p` inside `nested_stats`). APA-strict: leading zero stripped,
#'   `<.001` (or `<.0001` etc. depending on `p_digits`) for small
#'   values. Default `3L`.
#' @param effect_size_digits Decimals for per-coefficient effect
#'   sizes (`partial_f2`, `partial_eta2`, `partial_omega2`).
#'   Default `2L`.
#' @param fit_digits Decimals for variance-explained / model-level
#'   effect-size fit stats (`r2`, `adj_r2`, `r2_change`,
#'   `adj_r2_change`, `omega2`, `f2`, `f2_change`, `sigma`, `rmse`).
#'   Default `2L`.
#' @param ic_digits Decimals for information criteria (`AIC`,
#'   `AICc`, `BIC` in both `show_fit_stats` and `nested_stats`,
#'   plus their \eqn{\Delta}{Delta}-form). Default `1L`.
#' @param decimal_mark Decimal mark used in numeric display.
#'   `"."` (default) or `","` (European convention). When
#'   `","` is used, the CI bracket separator switches to `"; "`
#'   automatically to avoid `"0,18 [0,07, 0,30]"` ambiguity.
#' @param align Numeric column alignment.
#'   `"decimal"` (default) — pre-pad cells so decimal marks line
#'   up vertically (publication-style).
#'   `"center"`, `"right"`, or `"auto"` for legacy per-column
#'   alignment.
#' @param labels Named character vector overriding per-coefficient
#'   row labels. Names are coefficient term names (from
#'   [stats::terms()]); values are the displayed labels. E.g.
#'   `c("age" = "Age (years)", "sexM" = "Male (vs Female)")`.
#'   Default `NULL` (use raw term names).
#' @param output Output type. `"default"` (a printable
#'   `spicy_regression_table`); `"data.frame"` / `"long"` (raw
#'   data); `"gt"` / `"flextable"` / `"tinytable"` (rich-format
#'   tables); `"excel"` (writes to `excel_path`); `"clipboard"`
#'   (copies to system clipboard); `"word"` (writes flextable to
#'   `word_path`).
#' @param excel_path File path for `output = "excel"`. Default
#'   `NULL` (required when `output = "excel"`).
#' @param excel_sheet Sheet name when writing to Excel. Default
#'   `"Regression"`.
#' @param clipboard_delim Field delimiter for
#'   `output = "clipboard"`. Default `"\t"` (tab-separated, pastes
#'   cleanly into Excel / Google Sheets).
#' @param word_path File path for `output = "word"`. Default
#'   `NULL` (required when `output = "word"`).
#'
#' @return A `spicy_regression_table` object (a `data.frame`
#'   subclass with classes `c("spicy_regression_table",
#'   "spicy_table", "data.frame")`) when `output = "default"`.
#'   The result carries rendering attributes (`title`, `note`,
#'   `align`, `padding`) and provenance attributes (`outcome`,
#'   `model_ids`) consumed by the print method and the broom
#'   methods. For other `output` values, returns the
#'   format-specific object (`gt_tbl`, `flextable`, `tinytable`,
#'   `data.frame`, `tbl_df`, or `invisible(x)` for side-effect
#'   outputs).
#'
#' @section Classed conditions:
#' Every error and warning emitted by `table_regression()` carries
#' a classed condition for programmatic dispatch via [tryCatch()]
#' or [withCallingHandlers()]. Errors inherit from `spicy_error`
#' (root); warnings from `spicy_warning`. Specific leaves used by
#' this function include `spicy_invalid_input`,
#' `spicy_invalid_data`, `spicy_unsupported`,
#' `spicy_missing_pkg`, `spicy_missing_column`,
#' `spicy_ignored_arg`, `spicy_caveat`, `spicy_fallback`. See
#' [`spicy`][spicy::spicy-package] for the full taxonomy.
#'
#' @seealso
#' Other regression-table functions:
#' [table_continuous_lm()] for one-predictor-by-many-outcomes
#' descriptive tables.
#' Other spicy table functions:
#' [freq()], [cross_tab()], [table_categorical()],
#' [table_continuous()].
#' Underlying machinery:
#' [spicy_print_table()] for ASCII rendering;
#' [build_ascii_table()] for the low-level renderer.
#' Inferential infrastructure (internal):
#' `compute_lm_vcov()`, `compute_lm_coef_inference()`,
#' `compute_lm_wald_test()`.
#' broom integration:
#' [broom::tidy()], [broom::glance()].
#'
#' @examples
#' \dontrun{
#' library(spicy)
#'
#' # Single model, default APA layout
#' fit <- lm(wellbeing_score ~ age + sex + education,
#'           data = sochealth)
#' table_regression(fit)
#'
#' # Hierarchical regression: nested = TRUE adds the comparison
#' # footer (\eqn{\Delta}{Delta}\eqn{R^2}{R^2}, partial F, p)
#' m1 <- lm(wellbeing_score ~ age, data = sochealth)
#' m2 <- lm(wellbeing_score ~ age + sex, data = sochealth)
#' m3 <- lm(wellbeing_score ~ age + sex + education,
#'          data = sochealth)
#' table_regression(
#'   list("Step 1" = m1, "Step 2" = m2, "Step 3" = m3),
#'   nested = TRUE
#' )
#'
#' # Cluster-robust SEs with Satterthwaite df
#' fit_cl <- lm(wellbeing_score ~ age + sex,
#'              data = sochealth, weights = weight)
#' table_regression(
#'   fit_cl,
#'   vcov = "CR2",
#'   cluster = clinic_id
#' )
#'
#' # Standardised coefficients (\eqn{\beta}{beta}) alongside B
#' table_regression(fit, standardized = "refit")
#'
#' # Custom column set: B, partial \eqn{f^2}{f^2}, AME with CI, p
#' table_regression(
#'   fit,
#'   show_columns = c("B", "partial_f2", "AME", "p")
#' )
#'
#' # Pedagogical side-by-side SE comparison (same fit, three vcovs)
#' table_regression(
#'   list("Classical" = fit, "HC3" = fit, "CR2" = fit),
#'   vcov = list("classical", "HC3", "CR2"),
#'   cluster = list(NULL, NULL, sochealth$clinic_id)
#' )
#'
#' # Output to gt
#' table_regression(fit, output = "gt")
#'
#' # Tidy long format for downstream pipelines
#' broom::tidy(table_regression(fit))
#' }
#'
#' @references
#' APA Manual 7 (American Psychological Association, 2020),
#' Tables 7.13–7.15.
#'
#' Aiken, L.S. & West, S.G. (1991). *Multiple regression: Testing
#' and interpreting interactions*.
#'
#' Cohen, J., Cohen, P., West, S.G., & Aiken, L.S. (2003).
#' *Applied multiple regression / correlation analysis for the
#' behavioral sciences* (3rd ed.). Lawrence Erlbaum.
#'
#' Pustejovsky, J.E. & Tipton, E. (2018). Small-sample methods for
#' cluster-robust variance estimation and hypothesis testing in
#' fixed effects models. *Journal of Business & Economic
#' Statistics*, 36(4), 672–683.
#'
#' Wasserstein, R.L., Schirm, A.L., & Lazar, N.A. (2019). Moving
#' to a world beyond "p < 0.05". *The American Statistician*,
#' 73(sup1), 1–19.
#'
#' @export
table_regression <- function(
  models,
  vcov = "classical",
  cluster = NULL,
  ci_level = 0.95,
  ci_method = c("wald", "profile"),
  boot_n = 1000L,
  standardized = c("none", "refit", "posthoc", "basic", "smart", "pseudo"),
  exponentiate = FALSE,
  p_adjust = "none",
  show_columns = c("B", "SE", "CI", "p"),
  keep = NULL,
  drop = NULL,
  show_intercept = TRUE,
  intercept_position = c("first", "last"),
  group_factor_levels = TRUE,
  reference_style = c("row", "annotation"),
  reference_label = "(ref.)",
  show_fit_stats = NULL,
  model_labels = NULL,
  outcome_labels = NULL,
  stars = FALSE,
  nested = FALSE,
  nested_stats = NULL,
  digits = 2L,
  p_digits = 3L,
  effect_size_digits = 2L,
  fit_digits = 2L,
  ic_digits = 1L,
  decimal_mark = ".",
  align = c("decimal", "center", "right", "auto"),
  labels = NULL,
  output = c("default", "data.frame", "long", "gt", "flextable",
             "tinytable", "excel", "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = "Regression",
  clipboard_delim = "\t",
  word_path = NULL
) {
  # Capture the cluster expression BEFORE evaluation so we can
  # derive a friendly name for the footer ("clusters by clinic_id"
  # rather than "cluster vector supplied"). Done first thing, since
  # downstream uses of `cluster` evaluate the symbol away.
  cluster_expr <- substitute(cluster)

  # Resolve enum args
  standardized <- match.arg(standardized)
  ci_method <- match.arg(ci_method)
  intercept_position <- match.arg(intercept_position)
  reference_style <- match.arg(reference_style)
  align <- match.arg(align)
  output <- match.arg(output)

  # ====================================================================
  # Validation cascade (Q21 — 6 phases, ~29 steps, fail-fast).
  # Each helper lives in R/regression_validate.R and is called in the
  # documented order. Errors carry classed conditions
  # (spicy_invalid_input / spicy_unsupported); cross-arg semantic
  # warnings (Phase E) carry spicy_caveat or spicy_ignored_arg.
  # ====================================================================

  # Phase A — input class (steps 1–3)
  models <- validate_models_input(models)
  n_models <- length(models)

  # Phase B — multi-model alignment (steps 4–8)
  validate_nested_alignment(models, nested)
  validate_vcov_cluster_lists(vcov, cluster, models)

  # Phase C — vocabulary tokens (steps 9–12). validate_show_columns
  # also rejects "beta" combined with `standardized = "none"` (Q3).
  validate_show_columns(show_columns, standardized)
  validate_show_fit_stats(show_fit_stats)
  if (!is.null(nested_stats)) {
    validate_nested_stats(nested_stats)
    # Warn if `nested_stats` is supplied but `nested = FALSE` — the
    # token vector would otherwise be silently ignored. Cross-arg
    # semantic check (Phase E equivalent).
    if (!isTRUE(nested)) {
      spicy_warn(
        c(
          paste0(
            "`nested_stats` is supplied but `nested = FALSE`; the ",
            "tokens are ignored."
          ),
          "i" = "Set `nested = TRUE` to display the model-comparison footer."
        ),
        class = "spicy_ignored_arg"
      )
    }
  }
  # Q3 — auto-inject "beta" right after "B" when standardized != "none"
  # AND beta is not already requested. Done after validation so the
  # reject-beta-without-method branch fires before the orchestrator
  # mutates the user's show_columns.
  if (!identical(standardized, "none") && !"beta" %in% show_columns) {
    b_idx <- which(show_columns == "B")
    show_columns <- if (length(b_idx)) {
      append(show_columns, "beta", after = b_idx[1])
    } else {
      c(show_columns, "beta")
    }
  }

  # show_fit_stats class-aware default: when NULL, pick the
  # appropriate token set per model class. Mixed lm + glm sets
  # union both groups; the renderer's "skip token absent from
  # fit_stats schema" branch handles the per-row NA gracefully.
  if (is.null(show_fit_stats)) {
    any_glm <- any(vapply(models, inherits, logical(1), "glm"))
    any_lm_only <- any(vapply(models, function(f) {
      inherits(f, "lm") && !inherits(f, "glm")
    }, logical(1)))
    show_fit_stats <- character(0)
    if (any_lm_only) {
      show_fit_stats <- c(show_fit_stats, "nobs", "r2", "adj_r2")
    }
    if (any_glm) {
      show_fit_stats <- c(show_fit_stats,
                            if (!any_lm_only) "nobs",
                            "pseudo_r2_mcfadden",
                            "pseudo_r2_nagelkerke",
                            "AIC")
    }
    show_fit_stats <- unique(show_fit_stats)
  }

  # Class-aware token compatibility — variance-explained tokens are
  # rejected on glm with a hint to the partial_chi2 / pseudo_r2_*
  # substitutes; pseudo_r2_* is rejected on lm. The check runs on
  # the resolved (class-aware default OR user-supplied) vector.
  validate_class_appropriate_tokens(models, show_columns, show_fit_stats)
  validate_class_appropriate_nested_stats(models, nested_stats, nested)

  # `ci_method = "profile"` is glm only. Profile-likelihood CIs are
  # standard for glm (MASS::confint.glm); for lm, Wald CIs are exact
  # under the normal-error assumption, so a profile path would be
  # equivalent to Wald with extra cost.
  if (identical(ci_method, "profile")) {
    any_lm_only <- any(vapply(models, function(f) {
      inherits(f, "lm") && !inherits(f, "glm")
    }, logical(1)))
    if (any_lm_only) {
      spicy_abort(
        c(
          "`ci_method = \"profile\"` is defined for `glm` only.",
          "i" = paste0(
            "For `lm`, Wald CIs are exact under the normal-error ",
            "assumption (no profile refinement available)."
          ),
          "i" = "Use `ci_method = \"wald\"` (default) for `lm`."
        ),
        class = "spicy_invalid_input"
      )
    }
  }

  # `standardized = "pseudo"` (Menard 2011 fully-standardised) is glm
  # only — it derives SD(Y*) from the link-scale latent variance,
  # which is undefined for lm.
  if (identical(standardized, "pseudo")) {
    any_lm_only <- any(vapply(models, function(f) {
      inherits(f, "lm") && !inherits(f, "glm")
    }, logical(1)))
    if (any_lm_only) {
      spicy_abort(
        c(
          paste0(
            "`standardized = \"pseudo\"` (Menard 2011 fully-",
            "standardised) is defined for `glm` only."
          ),
          "i" = paste0(
            "Use `standardized = \"refit\"` for `lm` (Cohen et al. ",
            "2003 gold standard) or one of `\"posthoc\"` / ",
            "`\"basic\"` / `\"smart\"` (algebraic scaling on the ",
            "fitted model)."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
  }

  # exponentiate: warn if requested but no glm-with-non-identity-link
  # is present — the transform would be a no-op everywhere.
  if (isTRUE(exponentiate)) {
    has_non_identity_glm <- any(vapply(models, function(f) {
      if (!inherits(f, "glm")) return(FALSE)
      !identical(stats::family(f)$link, "identity")
    }, logical(1)))
    if (!has_non_identity_glm) {
      spicy_warn(
        c(
          "`exponentiate = TRUE` has no effect on `lm()` or identity-link `glm()` fits.",
          "i" = paste0(
            "exp() is only applied to glm fits whose link is non-",
            "identity (logit, probit, log, cloglog, inverse). Drop ",
            "the argument or remove the check."
          )
        ),
        class = "spicy_ignored_arg"
      )
    }
  }

  # gaussian-glm caveat: the user fitted glm() with the default
  # (gaussian / identity) family, which is mathematically equivalent
  # to lm() but loses access to the variance-explained effect sizes
  # (partial_f2 / \u03B7\u00B2 / \u03C9\u00B2) and to the AME-Satterthwaite path A.
  # Following the "transparency over rejection" rule, we accept the
  # fit and surface the suggestion via spicy_caveat.
  for (i in seq_along(models)) {
    f <- models[[i]]
    if (inherits(f, "glm")) {
      fam <- stats::family(f)
      if (identical(fam$family, "gaussian") &&
            identical(fam$link, "identity")) {
        spicy_warn(
          c(
            sprintf(
              "Model %d is a gaussian / identity glm.",
              i
            ),
            "i" = paste0(
              "This is mathematically equivalent to `lm()`. Refitting ",
              "with `lm()` gives access to the variance-explained ",
              "partial effect sizes (partial_f2 / \u03B7\u00B2 / \u03C9\u00B2) and to the ",
              "Satterthwaite-corrected AME path under CR* variance, ",
              "neither of which is defined for the glm route."
            )
          ),
          class = "spicy_caveat"
        )
      }
    }
  }

  # Phase D — argument values (steps 13–24)
  validate_ci_level(ci_level)
  validate_boot_n(boot_n)
  validate_logical_scalar(show_intercept, "show_intercept")
  validate_logical_scalar(group_factor_levels, "group_factor_levels")
  validate_logical_scalar(nested, "nested")
  validate_digit_arg(digits, "digits")
  validate_digit_arg(p_digits, "p_digits")
  validate_digit_arg(effect_size_digits, "effect_size_digits")
  validate_digit_arg(fit_digits, "fit_digits")
  validate_digit_arg(ic_digits, "ic_digits")
  validate_decimal_mark(decimal_mark)
  validate_reference_label(reference_label)
  validate_stars(stars)
  validate_p_adjust(p_adjust)
  validate_keep_drop(keep, drop)
  validate_logical_scalar(exponentiate, "exponentiate")
  validate_model_labels(model_labels, models)
  validate_outcome_labels(outcome_labels, models)
  validate_predictor_labels(labels, models)

  # Phase E — cross-arg semantic warnings (no errors). The
  # standardized × non-additive caveat is emitted later (after
  # extracts) since it inspects each fit. Q1 / Q2 conflict warnings
  # are runtime / orchestrator-only and emitted here.
  if (!is.null(model_labels) &&
      is.list(models) && !is.null(names(models)) &&
      all(nzchar(names(models)))) {
    spicy_warn(
      c(
        paste0("Both `names(models)` and `model_labels` were supplied; ",
               "the explicit `model_labels` takes precedence (Q1)."),
        "i" = "Drop one of the two to silence this warning."
      ),
      class = "spicy_ignored_arg"
    )
  }
  if (isFALSE(show_intercept) && !identical(intercept_position, "first")) {
    spicy_warn(
      c(
        paste0("`intercept_position = \"", intercept_position,
               "\"` is ignored when `show_intercept = FALSE` (Q2)."),
        "i" = "Drop `intercept_position` or set `show_intercept = TRUE`."
      ),
      class = "spicy_ignored_arg"
    )
  }

  # Phase F — output-dependent resource validation (file paths,
  # optional packages). Fires only for the selected output.
  validate_output_resources(output, excel_path, word_path)

  # ====================================================================
  # End of validation cascade. Recycle vcov / cluster across models.
  # ====================================================================

  vcov_list <- if (is.list(vcov)) vcov else rep(list(vcov), n_models)
  cluster_list <- if (is.list(cluster) && !is.atomic(cluster)) {
    cluster
  } else {
    rep(list(cluster), n_models)
  }

  # Friendly cluster-column names per model for the footer. When the
  # user passed `cluster = df$col`, we extract "col"; when a list of
  # clusters was passed, we walk the captured expression element by
  # element. Falls back to NA → footer renders "cluster vector
  # supplied" the way it did before.
  cluster_name_list <- if (is.call(cluster_expr) &&
                            identical(as.character(cluster_expr[[1]]), "list")) {
    elems <- as.list(cluster_expr)[-1L]
    if (length(elems) == n_models) {
      vapply(elems, extract_arg_column_name, character(1))
    } else {
      rep(NA_character_, n_models)
    }
  } else {
    rep(extract_arg_column_name(cluster_expr), n_models)
  }

  # Detect AME-Satterthwaite path activation (Q14b)
  any_cr <- any(vapply(vcov_list,
                       function(v) is.character(v) && startsWith(v, "CR"),
                       logical(1)))
  use_ame_satt <- any_cr && "AME" %in% show_columns

  # ---- Per-model extraction (Layer 1) ------------------------------------
  default_id <- function(i) paste0("M", i)
  model_ids <- if (!is.null(names(models)) && all(nzchar(names(models)))) {
    names(models)
  } else {
    vapply(seq_len(n_models), default_id, character(1))
  }

  extracts <- vector("list", n_models)
  for (i in seq_len(n_models)) {
    extracts[[i]] <- extract_lm_phase1(
      models[[i]],
      model_id = model_ids[i],
      vcov_type = vcov_list[[i]],
      cluster = cluster_list[[i]],
      boot_n = boot_n,
      ci_level = ci_level,
      ci_method = ci_method,
      standardized = standardized,
      exponentiate = exponentiate,
      show_columns = show_columns,
      show_fit_stats = show_fit_stats,
      use_ame_satterthwaite = use_ame_satt,
      cluster_name = cluster_name_list[i]
    )
    # Attach precomputed non-additivity metadata so the standardized
    # caveat footer (build_standardized_caveat_footer_block()) can
    # decide without reaching back to the live fit.
    extracts[[i]][["non_additive"]] <- detect_non_additive_terms(models[[i]])

    # p_adjust runs per model BEFORE alignment / keep-drop filtering
    # so the family is the model's full coefficient set (intercept
    # and reference rows excluded), not just the displayed subset.
    if (!identical(p_adjust, "none")) {
      extracts[[i]]$coefs <- apply_p_adjust(extracts[[i]]$coefs,
                                            p_adjust)
    }
  }

  # Standardized-on-non-additive caveat (Q15, Phase E)
  emit_standardized_caveat_if_needed(models, standardized)

  # ---- Multi-model alignment + wide pivot (Layer 2) ----------------------
  aligned <- align_extracts(
    extracts,
    show_intercept = show_intercept,
    intercept_position = intercept_position,
    group_factor_levels = group_factor_levels,
    reference_style = reference_style
  )

  # `keep` / `drop` filter — runs AFTER alignment (so canonical term
  # ordering is preserved for the surviving terms) and AFTER
  # p_adjust (so adjusted p-values reflect the model's full
  # coefficient family, not just the displayed subset).
  aligned <- apply_keep_drop_filter(aligned, keep = keep, drop = drop)

  # ---- Title + footer (Step 7) -------------------------------------------
  title <- build_regression_title(extracts, nested = nested)
  footer_main <- build_regression_footer(
    extracts,
    standardized = standardized,
    p_adjust = p_adjust,
    stars = stars,
    nested = nested,
    show_columns = show_columns
  )

  # ---- Nested comparison (Step 9) ----------------------------------------
  nested_footer <- NULL
  if (isTRUE(nested)) {
    nested_comp <- compute_nested_comparisons_lm(models, nested_stats)
    nested_footer <- format_nested_comparison_footer(
      nested_comp,
      digits = digits, p_digits = p_digits,
      fit_digits = fit_digits, ic_digits = ic_digits
    )
  }

  full_footer <- c(footer_main, nested_footer)
  full_footer <- full_footer[!is.null(full_footer) &
                              vapply(full_footer, nzchar, logical(1))]
  full_footer_str <- if (length(full_footer)) {
    paste(full_footer, collapse = "\n\n")
  } else {
    NULL
  }

  # ---- Render (Layer 3) --------------------------------------------------
  # Q1: model_labels precedence is explicit > names(list) > default.
  effective_model_labels <- if (!is.null(model_labels)) {
    model_labels
  } else if (!is.null(names(models)) && all(nzchar(names(models)))) {
    names(models)
  } else {
    NULL  # render_regression_table generates "Model 1", ... as fallback
  }
  rendered <- render_regression_table(
    aligned,
    show_columns = show_columns,
    show_fit_stats = show_fit_stats,
    model_labels = effective_model_labels,
    reference_label = reference_label,
    reference_style = reference_style,
    group_factor_levels = group_factor_levels,
    stars = stars,
    ci_level = ci_level,
    digits = digits,
    p_digits = p_digits,
    effect_size_digits = effect_size_digits,
    fit_digits = fit_digits,
    ic_digits = ic_digits,
    decimal_mark = decimal_mark,
    align = align,
    labels = labels,
    outcome_labels = outcome_labels,
    title = title,
    note = full_footer_str
  )

  # ---- Output dispatch (Step 11) -----------------------------------------
  dispatch_regression_output(
    rendered = rendered,
    aligned = aligned,
    output = output,
    excel_path = excel_path,
    excel_sheet = excel_sheet,
    clipboard_delim = clipboard_delim,
    word_path = word_path
  )
}
