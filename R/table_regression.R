#' Regression coefficient summary table
#'
#' @description
#' Publication-ready coefficient table from one or more fitted
#' `lm` / `glm` models. Supports standardised coefficients
#' (\eqn{\beta}{beta}), average marginal effects (AME), partial
#' effect sizes (*\eqn{f^2}{f^2}* / *\eqn{\eta^2}{eta^2}* /
#' *\eqn{\omega^2}{omega^2}* for `lm`; partial
#' *\eqn{\chi^2}{chi-squared}* for `glm`), pseudo-\eqn{R^2}{R^2}
#' (`glm`), and a full vocabulary of variance estimators
#' (classical / HC* / cluster-robust with Satterthwaite-corrected
#' df / bootstrap / jackknife). `glm` covers binomial / poisson /
#' Gamma / inverse.gaussian / quasi families with any link.
#'
#' @details
#' # Vocabulary tokens
#'
#' Two vector arguments -- `show_columns` and `show_fit_stats` --
#' accept named tokens that select **what** to display and in
#' **what order**. All tokens are lowercase
#' (snake_case for compound tokens). Group tokens
#' (`"all_b"`, `"all_ame"`, ...) expand to a fixed vector of
#' atomic tokens; see `show_columns` below.
#'
#' ## `show_columns` -- per-coefficient columns
#'
#' Each token = one displayed column.
#'
#' \itemize{
#'   \item Coefficient family: `"b"`, `"beta"` (standardised),
#'     `"se"`, `"ci"`, `"t"`, `"p"`.
#'   \item Marginal effects: `"ame"`, `"ame_se"`, `"ame_ci"`,
#'     `"ame_p"`. `"p"` always refers to the B-coefficient
#'     p-value; for the AME-specific p-value use `"ame_p"`.
#'   \item Partial effect sizes -- `lm` only: `"partial_f2"`,
#'     `"partial_eta2"`, `"partial_omega2"`, each with a paired
#'     `_ci` companion (`"partial_f2_ci"`, ...).
#'   \item Partial effect size -- `glm` only: `"partial_chi2"`
#'     (likelihood-ratio chi-square via `drop1(test = "LRT")`;
#'     SAS PROC LOGISTIC `TYPE3`; Long & Freese 2014 Section 3.5).
#'     Rendered as `value (df)` to disambiguate factor terms
#'     (k-1 df) from numeric terms (1 df).
#' }
#'
#' **Group tokens** (presets) expand to a fixed atomic vector
#' before validation:
#'
#' \itemize{
#'   \item `"all_b"`         -> `c("b", "se", "ci", "p")`
#'   \item `"all_b_compact"` -> `c("b", "se", "p")`
#'   \item `"all_b_full"`    -> `c("b", "se", "ci", "t", "p")`
#'   \item `"all_beta"`      -> `c("b", "beta", "se", "ci", "p")`
#'   \item `"all_ame"`       -> `c("ame", "ame_se", "ame_ci", "ame_p")`
#'   \item `"all_ame_compact"` -> `c("ame", "ame_p")`
#'   \item `"all_f2"` / `"all_eta2"` / `"all_omega2"` -> `partial_*`
#'     + its `_ci` companion.
#' }
#'
#' Mix groups and atomic tokens:
#' `show_columns = c("all_b", "ame", "ame_p")`. Duplicates after
#' expansion are deduplicated; the order of tokens controls the
#' order of the displayed columns. If `standardized != "none"` and
#' `"beta"` is not already requested, it is auto-injected after
#' `"b"`. Asking for `"beta"` while `standardized = "none"` raises
#' `spicy_invalid_input`.
#'
#' **Default** (`show_columns = NULL`) is context-aware:
#' `"all_b"` for a single model (APA-7 Section 6.46 publication layout),
#' `"all_b_compact"` for two or more models (CI dropped to fit the
#' side-by-side layout; restore it explicitly when needed).
#'
#' ## `show_fit_stats` -- model-level rows below the coefficients
#'
#' \itemize{
#'   \item Counts: `"nobs"`, `"weighted_nobs"`.
#'   \item Variance explained (`lm` only): `"r2"`, `"adj_r2"`,
#'     `"omega2"`.
#'   \item Pseudo-\eqn{R^2}{R^2} (`glm` only): `"pseudo_r2_mcfadden"`
#'     (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991),
#'     `"pseudo_r2_tjur"` (Tjur 2009; binomial only).
#'   \item Residual scale: `"sigma"` (lm \eqn{\hat{\sigma}}{sigma-hat}
#'     / glm dispersion), `"rmse"`.
#'   \item Effect size: `"f2"`.
#'   \item Information criteria: `"AIC"`, `"AICc"`, `"BIC"`,
#'     `"deviance"`.
#'   \item Change-stats for hierarchical comparison
#'     (active under `nested = TRUE`; see *Hierarchical
#'     comparison* below): `"r2_change"`, `"adj_r2_change"`,
#'     `"f_change"`, `"f2_change"`, `"lrt_change"`,
#'     `"aic_change"`, `"aicc_change"`, `"bic_change"`,
#'     `"deviance_change"`, `"p_change"`.
#' }
#'
#' Default (resolved when `NULL`) is class-aware: lm fits get
#' `c("nobs", "r2", "adj_r2")`; glm fits get
#' `c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "AIC")`;
#' mixed lm + glm sets union both groups (the renderer per-row
#' em-dashes the inappropriate cell). When `nested = TRUE`, the
#' class-aware default is extended with change tokens
#' (`c("r2_change", "f_change", "p_change")` for lm,
#' `c("lrt_change", "p_change")` for glm). The order of tokens in
#' `show_fit_stats` controls the order of the rows.
#'
#' # Multi-model semantics
#'
#' Pass a single fit or a `list()` of fits. Multi-model layout
#' draws a centred **spanner label** above each model's
#' sub-columns:
#'
#' \itemize{
#'   \item `list("Naive" = m1, "Adjusted" = m2)` -> spanner labels
#'     `"Naive"` / `"Adjusted"`. Partial naming (`list("Naive" = m1, m2)`)
#'     auto-fills missing slots as `"Model <position>"`.
#'   \item `list(m1, m2)` (unnamed) -> if all response variables
#'     differ, the bare DV name (from `formula(fit)[[2]]`) becomes
#'     the spanner label and the redundant Outcome body row is
#'     suppressed. If DVs match, the labels default to
#'     `"Model 1, 2, ..."`.
#'   \item `model_labels = c("A", "B")` overrides everything.
#' }
#'
#' Duplicate explicit names in the list are rejected
#' (`spicy_invalid_input`) -- they would silently collide in the
#' internal model_id key.
#'
#' # Inference and standard errors
#'
#' `vcov` selects the variance-covariance estimator:
#'
#' \itemize{
#'   \item `"classical"` -- OLS (lm) / MLE inverse Hessian (glm).
#'   \item `"HC0"` to `"HC5"` -- heteroskedasticity-consistent
#'     (via [sandwich::vcovHC()]).
#'   \item `"CR0"` to `"CR3"` -- cluster-robust with
#'     Satterthwaite-corrected df (via [clubSandwich::vcovCR()]).
#'     Requires `cluster`.
#'   \item `"bootstrap"` -- nonparametric or cluster bootstrap
#'     (`boot_n` replicates).
#'   \item `"jackknife"` -- leave-one-out / leave-one-cluster-out.
#' }
#'
#' For multi-model use, both `vcov` and `cluster` accept a single
#' value (recycled to all models) or a list (one per model). The
#' same fit can appear several times with different estimators to
#' compare standard errors side-by-side.
#'
#' Inferential regimes (B and AME share the same regime):
#' \itemize{
#'   \item `classical`, `HC*` -> t with `df.residual`.
#'   \item `bootstrap`, `jackknife` -> z asymptotic.
#'   \item `CR0`-`CR3` -> t with **Satterthwaite-corrected df** (B
#'     via [clubSandwich::coef_test()]; AME via
#'     [clubSandwich::linear_contrast()]; Pustejovsky & Tipton
#'     2018). Under non-linear terms (`poly()`, `I()`, `log()`,
#'     `splines::ns()`), AME falls back to z-asymptotic with a
#'     `spicy_fallback` warning.
#' }
#'
#' ## How to specify `cluster`
#'
#' Three accepted forms, in order of preference:
#'
#' \enumerate{
#'   \item **Formula** -- `cluster = ~region` (or
#'     `cluster = ~region:year` for the interaction of two
#'     variables). The variables are looked up in
#'     `model.frame(fit)` first, then in the original `data`
#'     argument captured by the fit. **Recommended**: independent
#'     of the dataset's name, composable for multi-way clustering,
#'     consistent with [sandwich::vcovCL()] /
#'     [clubSandwich::vcovCR()].
#'   \item **String** -- `cluster = "region"`. A single column
#'     name resolved the same way as the formula. Convenient but
#'     cannot express interactions.
#'   \item **Vector** -- `cluster = df$region`. An atomic vector of
#'     length `nobs(fit)`. Use this when the cluster key is
#'     **derived on the fly** (`cluster = interaction(df$region, df$year)`,
#'     `cluster = as.integer(format(df$date, "%Y"))`), comes from
#'     a **different dataset** with matching row order, or is
#'     otherwise not a column of the model's `data`.
#' }
#'
#' Bare unquoted names (`cluster = region`) are **not** accepted --
#' they would require non-standard evaluation magic that breaks
#' under programmatic use (function wrapping, dynamic column
#' choice, loops). Use `~region` or `"region"` instead.
#'
#' For multi-model use, mix forms freely:
#' `cluster = list(~region, "region", df$region)`.
#'
#' # Hierarchical (nested) model comparison
#'
#' `nested = TRUE` adds **per-pair change statistics as in-table
#' rows** (APA Table 7.13 / Stata `esttab` / SPSS Model Summary
#' convention). Each adjacent pair (M2 vs M1, M3 vs M2, ...)
#' contributes one column of change stats; the FIRST model column
#' gets em-dashes (no previous model to compare to). Validation
#' requires identical `nobs` and identical response variable
#' across all models.
#'
#' Default change tokens auto-injected when `show_fit_stats` is
#' `NULL`:
#' \itemize{
#'   \item All-lm: `c("r2_change", "f_change", "p_change")` --
#'     APA hierarchical regression standard.
#'   \item All-glm: `c("lrt_change", "p_change")` -- Hosmer &
#'     Lemeshow Section 3.5; Long & Freese 2014 Section 3.6.
#' }
#' To customise, pass the change tokens directly to
#' `show_fit_stats`. Variance-explained change tokens on an
#' all-glm hierarchy raise `spicy_invalid_input` (the
#' residual-sum-of-squares partition does not apply outside the
#' least-squares framework -- the renderer points the user at
#' `lrt_change`).
#'
#' # Standardised coefficients
#'
#' `standardized` controls the method when `"beta"` is in
#' `show_columns`:
#' \itemize{
#'   \item `"refit"` -- refit on z-scored data. For `lm` both X
#'     and Y are z-scored (Cohen et al. 2003 gold standard); for
#'     `glm` only numeric X (Long & Freese 2014 Section 4.3.4
#'     "x-standardization").
#'   \item `"posthoc"` -- post-hoc scaling. lm:
#'     \eqn{\beta = B \times SD(X) / SD(Y)}{beta = B * SD(X) / SD(Y)};
#'     glm: X-only \eqn{\beta = B \times SD(X)}{beta = B * SD(X)}
#'     (Y is undefined on the link scale).
#'   \item `"basic"` -- like `"posthoc"` but factor dummies are
#'     scaled by their column SD.
#'   \item `"smart"` -- Gelman (2008): divide binary predictors
#'     by `2 * SD` instead of `SD`.
#'   \item `"pseudo"` -- *glm only*. Menard (2004, 2011)
#'     fully-standardised \eqn{\beta = B \times SD(X) / SD(Y^*)}{beta = B * SD(X) / SD(Y*)},
#'     with \eqn{Y^*}{Y*} the latent variable on the link scale and
#'     \eqn{SD(Y^*) = \sqrt{Var(\hat{\eta}) + Var_{link}}}{SD(Y*) = sqrt(Var(eta-hat) + Var_link)}
#'     (\eqn{\pi^2/3}{pi^2/3} logit, `1` probit,
#'     \eqn{\pi^2/6}{pi^2/6} cloglog). Binomial families only;
#'     non-binomial returns NA with a `spicy_caveat`.
#'   \item `"none"` (default) -- no \eqn{\beta}{beta} computed.
#' }
#'
#' Under interactions or transformed predictors (`I()`, `poly()`,
#' `log()`, `splines::ns()`), a `spicy_caveat` warns that
#' standardised coefficients on such terms are subtle to interpret
#' (Cohen et al. 2003 Section 7.7; Aiken & West 1991). The caveat is
#' auto-documented in the footer.
#'
#' # Multiple-comparison adjustment
#'
#' Adjusting the p-values of all coefficients of a single
#' regression model is **not** the standard convention. Each
#' coefficient tests a distinct hypothesis on a distinct
#' predictor -- not the situation multiple-testing procedures
#' were designed for (Rothman 1990; Greenland 2017; APA Manual 7
#' Section 6.46; Harrell *Regression Modeling Strategies* Section 5.4; Gelman,
#' Hill & Yajima 2012). Hence the default `p_adjust = "none"`.
#'
#' Adjustment is appropriate for: mass screening with no prior
#' hypothesis (typically `"BH"` / FDR), pre-registered
#' multi-endpoint confirmatory designs (typically `"holm"`), or
#' when a journal / SAP explicitly requests it.
#'
#' The adjustment runs **before** any `keep` / `drop` filtering,
#' so the family is the model's full coefficient set (intercept
#' and reference rows excluded), not the displayed subset --
#' filtering is a display choice and must not change the
#' inferential family.
#'
#' # Output formats and broom integration
#'
#' `output` selects the return type:
#' \itemize{
#'   \item `"default"` -- a `spicy_regression_table`
#'     (`data.frame` subclass) printed via [spicy_print_table()].
#'   \item `"data.frame"` / `"long"` -- raw data.frame /
#'     long-format tibble.
#'   \item `"gt"` / `"flextable"` / `"tinytable"` -- rich-format
#'     HTML / Word / PDF tables (require the corresponding
#'     Suggests package).
#'   \item `"excel"` -- writes to `excel_path` via
#'     [openxlsx2::write_xlsx()].
#'   \item `"word"` -- writes to `word_path` via
#'     `flextable::save_as_docx()`.
#'   \item `"clipboard"` -- copies to the system clipboard via
#'     [clipr::write_clip()].
#' }
#'
#' [broom::tidy()] returns a long tibble with one row per
#' `(model_id, term, estimate_type)` and broom-canonical column
#' names (`estimate`, `std.error`, `conf.low`, `conf.high`,
#' `statistic`, `p.value`). [broom::glance()] returns one row per
#' model with the model-level statistics; `df.residual` is kept
#' numeric so cluster-robust Satterthwaite df is preserved.
#'
#' # Weights
#'
#' No `weights` argument: weights are a property of the fit
#' (extracted via [stats::weights()]). Pass them when fitting:
#' `lm(y ~ x, data = df, weights = w)`. All downstream
#' computations (vcov, AME, standardisation, `weighted_nobs`)
#' extract them automatically.
#'
#' # Internationalisation
#'
#' Output is in English. Override user-facing strings via
#' `reference_label`, `model_labels`, `outcome_labels`, and
#' `labels`. The title and footer are post-processable via
#' `attr(result, "title")` and `attr(result, "note")`.
#'
#' @param models An `lm` or `glm` fitted model, or a list of such
#'   fits (named or unnamed; `lm` and `glm` may be mixed). Single
#'   fits are auto-promoted to a 1-element list. `merMod` and
#'   other classes raise `spicy_unsupported`. Raw data + formula
#'   is not accepted -- fit-only API.
#' @param vcov Variance-covariance estimator: `"classical"`,
#'   `"HC0"`-`"HC5"`, `"CR0"`-`"CR3"`, `"bootstrap"`, or
#'   `"jackknife"`. A scalar is recycled to all models; a list
#'   (one string per model) allows mixed estimators. Default
#'   `"classical"`. See *Inference and standard errors*.
#' @param cluster Cluster identifier for cluster-robust variance
#'   (used when `vcov` is `"CR0"`-`"CR3"` or a cluster-bootstrap /
#'   cluster-jackknife). Three accepted forms (see *How to specify
#'   `cluster`* in the details):
#'   \itemize{
#'     \item Formula: `~region`, `~region:year` (recommended).
#'     \item String column name: `"region"`.
#'     \item Atomic vector of length `nobs(fit)`: `df$region`,
#'       `interaction(df$region, df$year)`, ... (for keys derived
#'       on the fly).
#'   }
#'   For multi-model use, pass a list of one form per model
#'   (mix-and-match allowed). Bare unquoted names
#'   (`cluster = region`) are NOT accepted -- use `~region` or
#'   `"region"`. Default `NULL` (no clustering).
#' @param ci_level Confidence level for all reported CIs (B, \eqn{\beta}{beta},
#'   AME, partial effect sizes). Default `0.95`.
#' @param boot_n Number of bootstrap replicates when
#'   `vcov = "bootstrap"`. Single positive integer. Default
#'   `1000L`.
#' @param ci_method CI construction. `"wald"` (default) uses
#'   `estimate +/- z x SE` (`t x SE` for `lm`). `"profile"`
#'   (*glm only*) uses the profile-likelihood CI from
#'   [MASS::confint.glm()] -- asymmetric, exact for
#'   likelihood-based inference (Venables & Ripley *MASS* Section 7.2).
#'   Only the CI bounds change; estimate, SE, statistic and
#'   p-value remain Wald. `"profile"` with `lm` raises
#'   `spicy_invalid_input`.
#' @param standardized Standardisation method for the `"beta"`
#'   column. One of `"none"` (default), `"refit"`, `"posthoc"`,
#'   `"basic"`, `"smart"`, `"pseudo"`. `"pseudo"` is *glm only*
#'   (Menard 2011 fully-standardised); using it with `lm()` raises
#'   `spicy_invalid_input`. See the *Standardised coefficients*
#'   section.
#' @param exponentiate Logical. When `TRUE` and the model is a
#'   `glm` with a non-identity link, `B`, the CI bounds, and the
#'   SE are transformed via `exp()` (delta method:
#'   `SE_OR = OR x SE_log-odds`). The column header is rebranded
#'   per family / link: `OR` (binomial logit), `IRR` (poisson
#'   log), `HR` (binomial cloglog), `RR` (binomial log), `MR`
#'   (Gamma log), else `exp(B)`. The statistic and p-value stay
#'   on the link scale (invariant under monotone transformation).
#'   Default `FALSE`. No effect on `lm` or identity-link glm;
#'   emits a `spicy_ignored_arg` warning in those cases.
#' @param p_adjust Multiple-comparison adjustment method applied
#'   to the family of estimated coefficient p-values within each
#'   model (intercept and reference rows excluded). One of
#'   `"none"` (default), `"holm"`, `"hochberg"`, `"hommel"`,
#'   `"bonferroni"`, `"BH"` / `"fdr"`, or `"BY"`. Delegated to
#'   [stats::p.adjust()]; applied per-model and per `estimate_type`
#'   (B and AME p-values are adjusted independently within their
#'   own families). Active adjustments are documented in the
#'   footer (method + family size). See the *Multiple-comparison
#'   adjustment* section for when this is and is not appropriate.
#' @param show_columns Character vector of tokens selecting the
#'   per-coefficient columns and their display order. Accepts
#'   **atomic tokens** (`"b"`, `"se"`, `"ci"`, `"t"`, `"p"`,
#'   `"beta"`, `"ame"`, `"ame_se"`, `"ame_ci"`, `"ame_p"`,
#'   `"partial_f2"` + `"partial_f2_ci"`, `"partial_eta2"` +
#'   `"partial_eta2_ci"`, `"partial_omega2"` +
#'   `"partial_omega2_ci"`, `"partial_chi2"`) and **group tokens**
#'   (`"all_b"`, `"all_b_compact"`, `"all_b_full"`, `"all_beta"`,
#'   `"all_ame"`, `"all_ame_compact"`, `"all_f2"`, `"all_eta2"`,
#'   `"all_omega2"`). See *Vocabulary tokens* in the details for
#'   the full enumeration. Default `NULL` selects a context-aware
#'   layout: `"all_b"` (single model) or `"all_b_compact"` (multi-
#'   model). The `"p"` token is always the B / beta p-value; for
#'   the AME-specific p-value use `"ame_p"`.
#' @param keep Character vector of regexes. Only coefficient rows
#'   whose term name (as in [stats::coef()] -- e.g. `"wt"`,
#'   `"cyl6"`, `"factor(cyl)8"`) matches at least one pattern are
#'   kept. Mutually exclusive with `drop`. Filtering is a display
#'   choice; `p_adjust` runs against the full coefficient family
#'   before filtering. Default `NULL` (no filter).
#' @param drop Character vector of regexes. Coefficient rows
#'   matching any pattern are removed. Mutually exclusive with
#'   `keep`. Default `NULL`.
#' @param show_intercept Whether to display the intercept row.
#'   Default `TRUE` (APA convention). Hide via `FALSE`.
#' @param intercept_position Where to place the intercept when
#'   shown. `"first"` (default, APA) or `"last"` (Stata-style,
#'   intercept just above the fit-stats footer). Ignored when
#'   `show_intercept = FALSE` (with `spicy_ignored_arg` warning).
#' @param factor_layout Layout of factor predictors. Applies to
#'   **any categorical predictor** -- `factor`, `ordered`,
#'   `character`, or `logical` (R coerces the latter two to
#'   factors at fit time). Two options:
#'   \itemize{
#'     \item `"grouped"` (default): the variable name on its own
#'       header row ending with `:` (e.g., `education:`); each
#'       level follows as an indented sub-row with the bare level
#'       name. APA convention.
#'     \item `"flat"`: each non-reference dummy is one row with
#'       the `<variable><level>` form (e.g., `educationUpper`); no
#'       header, no indent. Econometrics convention.
#'   }
#' @param reference_style Rendering of factor reference levels.
#'   Four modes, distinguishing WHERE the reference information is
#'   exposed (in a row, inline, in the footer, or nowhere):
#'   \itemize{
#'     \item `"row"` (default): explicit row `Female (ref.)` with
#'       em-dashes in all stat columns (NEJM / BMJ clinical
#'       convention). `reference_label` controls the suffix.
#'     \item `"annotation"`: the row is dropped and the reference
#'       is shown inline. Under `factor_layout = "grouped"` the
#'       factor header reads `education: [ref: Lower]`; under
#'       `factor_layout = "flat"` the marker `[vs Lower]` is
#'       attached to the **first non-reference dummy** of each
#'       factor (subsequent dummies inherit the same reference).
#'     \item `"footer"`: the row is dropped and a single line
#'       `Reference categories: education = Lower; sex = Female.`
#'       is added to the footer note. SAS `PROC LOGISTIC` / SPSS
#'       "Categorical Variables Codings" convention. Best for
#'       publication-grade dense multi-factor tables.
#'     \item `"none"`: the row is dropped and no reference
#'       information is displayed anywhere. The user is
#'       responsible for stating the reference convention
#'       elsewhere (article text, table caption). Under
#'       `factor_layout = "flat"`, an informational message is
#'       emitted to flag the silent omission.
#'   }
#'   **Ordered factors with AME**: under R's default `contr.poly`,
#'   ordered factors have B coefficients named `.L` / `.Q` / `.C`
#'   (orthogonal polynomial trends) which have no per-level reference
#'   semantics. When `"ame"` is in `show_columns`, however, the AME
#'   block is per-level contrasts against `levels()[1]`. A synthetic
#'   reference row anchored on `levels()[1]` is therefore emitted so
#'   the reader sees the AME baseline explicitly, with the same
#'   `reference_style` handling as plain treatment-coded factors. The
#'   `[vs <ref>]` annotation in `"annotation"` mode is attached to the
#'   first AME row, not to the polynomial-trend rows.
#' @param reference_label Suffix shown after the reference level in
#'   `reference_style = "row"` mode. Default `"(ref.)"`. Ignored
#'   by the other three modes (which use structural English
#'   wording -- "ref:", "vs", "Reference categories:").
#' @param show_fit_stats Character vector of tokens for the
#'   model-level rows below the coefficients; row order follows
#'   token order. `NULL` (default) resolves class-aware:
#'   \itemize{
#'     \item `lm`: `c("nobs", "r2", "adj_r2")`.
#'     \item `glm`: `c("nobs", "pseudo_r2_mcfadden",
#'       "pseudo_r2_nagelkerke", "AIC")`.
#'     \item mixed `lm` + `glm`: the union of the two (the
#'       renderer em-dashes per cell the stat not defined for a
#'       given model class).
#'   }
#'   Under `nested = TRUE` the default is extended with the
#'   class-appropriate change-stat tokens (e.g. `"r2_change"`,
#'   `"f_change"` for `lm`). See *Vocabulary tokens*
#'   (`show_fit_stats` subsection) and *Hierarchical (nested)
#'   model comparison* in the details for the full vocabulary.
#' @param fit_stats_layout Layout of the fit-stat values (`n`, `R^2`,
#'   `AIC`, ...) within each model's column group. Two options:
#'   \itemize{
#'     \item `"first_col"` (default): the value is placed in the
#'       FIRST numeric sub-column of each model (typically `B`);
#'       the model's remaining sub-columns (`SE`, `LL`, `UL`, `p`,
#'       ...) are left empty for that row. The APA Manual 7
#'       Table 7.13 layout.
#'     \item `"merged"`: the model's numeric sub-columns are
#'       merged into a single wide cell containing the fit-stat
#'       value, centred under the model spanner. Stata `esttab`
#'       layout / *Econometrica* and *AER* journal convention.
#'       Resolves the mixed-precision look of `"first_col"` (an
#'       integer `n` row sharing the B column with two-decimal
#'       coefficients).
#'   }
#'   Cell merging is supported by `excel`, `flextable`, and
#'   `word` (via flextable). `gt`, `tinytable`, `clipboard`, and
#'   `default` (console) always render in `"first_col"` mode
#'   regardless of this setting:
#'   \itemize{
#'     \item `gt` lacks a native row-spanning cell-merge API
#'       (`tab_spanner` covers columns, not row-cell ranges).
#'     \item `tinytable`'s `style_tt(colspan = N)` emits HTML
#'       `colspan` only on header rows, not on body cells.
#'     \item `clipboard` ships TSV plaintext.
#'     \item `default` ships fixed-width ASCII.
#'   }
#'   Decimal alignment of every numeric column is preserved in
#'   both modes: the `B` column decimal-aligns its coefficient
#'   values plus any fit-stat value(s) in `"first_col"` mode
#'   (native primitives handle the mixed-precision case), and
#'   trivially decimal-aligns in `"merged"` mode (the fit-stat
#'   values move out of the B column into the merged cell).
#' @param show_re Logical. `TRUE` (default) prints the
#'   random-effects panel below the fit-statistics footer for
#'   mixed-effects fits (`lmer`, `glmer`, `glmmTMB`, `lme`).
#'   `FALSE` suppresses the panel entirely. No effect on fits
#'   without random effects (`lm`, `glm`, `coxph`, ...). The
#'   panel header carries the estimator label (`(REML)` or
#'   `(ML)`); see the *Mixed-effects models* section of
#'   `vignette("table-regression")` for the methodological
#'   rationale (Gelman 2005; Bates et al. 2015; Bolker FAQ).
#' @param re_scale One of `"sd"` (default) or `"variance"`.
#'   Controls the display scale of the random-effects panel:
#'   \itemize{
#'     \item `"sd"`: report the random-effect standard
#'       deviation \eqn{\sigma} (Gelman 2005, *Technometrics*:
#'       "*directly interpretable as the size of the variation
#'       across groups*"). Standard error and CI converted via
#'       the Delta method:
#'       \eqn{SE(\sigma) = SE(\sigma^2) / (2\sigma)};
#'       \eqn{CI(\sigma) = \sqrt{CI(\sigma^2)}}.
#'     \item `"variance"`: report \eqn{\sigma^2} (the canonical
#'       internal scale; SE and CI come straight from the
#'       Hessian / `nlme::intervals()` / `glmmTMB::confint()`
#'       without rescaling).
#'   }
#'   Correlation rows (\eqn{\rho}) are unitless and pass through
#'   either way.
#' @param re_columns Character vector. Subset of
#'   `c("est", "se", "ci")` controlling which columns of the
#'   random-effects panel are rendered. `"est"` is mandatory.
#'   Useful for slimming output (`re_columns = "est"`) or
#'   for journals that want only standard errors
#'   (`re_columns = c("est", "se")`).
#'
#'   Note. Standard errors and CIs are Wald (`est ± z * SE`,
#'   clamped at 0 for variances). Wald can be optimistic near
#'   the variance boundary (Self & Liang 1987 chi-bar-squared);
#'   profile-likelihood intervals are available directly on the
#'   fitted model (`confint(fit, method = "profile")` for
#'   `lmer`) when robustness is critical. See the
#'   *Mixed-effects models* section of `vignette("table-regression")`.
#' @param model_labels Per-model labels used as the **column-group
#'   spanner** above each model's sub-columns (console + gt /
#'   flextable / tinytable / Excel / Word renderers). `NULL`
#'   (default) resolves automatically; see *Multi-model semantics*
#'   for the full rule. A character vector of length
#'   `length(models)` overrides.
#' @param outcome_labels Optional **Outcome body row** override.
#'   `NULL` (default) hides the row entirely -- under the
#'   multi-model spanner the DV is already visible above the data.
#'   A character vector of length `length(models)` forces an
#'   explicit Outcome row with those values (the spanner stays as
#'   `"Model 1, ..."` unless `model_labels` is also supplied).
#'   `FALSE` also suppresses the row.
#' @param stars Significance asterisks. `FALSE` (default, APA 7
#'   Section 6.46) -- no stars. `TRUE` -- APA cutoffs
#'   `c("*" = 0.05, "**" = 0.01, "***" = 0.001)`. A named numeric
#'   vector specifies custom thresholds, e.g.
#'   `c("+" = 0.10, "*" = 0.05, "**" = 0.01, "***" = 0.001)`.
#' @param nested Whether to inject pairwise change-statistic rows
#'   for adjacent models (M2 vs M1, M3 vs M2, ...). `FALSE`
#'   (default) -- pure side-by-side display. `TRUE` -- requires
#'   identical `nobs` and identical response variable across all
#'   models. See *Hierarchical (nested) model comparison*.
#' @param digits Decimal places for general numeric tokens
#'   (`b`, `beta`, `se`, `ci`, `t`, `f_change`, `lrt_change`,
#'   `deviance`, `deviance_change`, `ame`, `ame_se`,
#'   `weighted_nobs`). Default `2L`.
#' @param p_digits Decimal places for p-values (`p`, `ame_p`,
#'   `p_change`). APA-strict: leading zero stripped, `<.001` (or
#'   `<.0001` etc. depending on `p_digits`) for small values.
#'   Default `3L`.
#' @param effect_size_digits Decimals for per-coefficient effect
#'   sizes (`partial_f2`, `partial_eta2`, `partial_omega2`).
#'   Default `2L`.
#' @param fit_digits Decimals for variance-explained / model-level
#'   effect-size fit stats (`r2`, `adj_r2`, `r2_change`,
#'   `adj_r2_change`, `omega2`, `f2`, `f2_change`, `sigma`,
#'   `rmse`). Default `2L`.
#' @param ic_digits Decimals for information criteria (`AIC`,
#'   `AICc`, `BIC`, and their `_change` form). Default `1L`.
#' @param decimal_mark Decimal mark used in numeric display.
#'   `"."` (default) or `","` (European convention). When
#'   `","` is used, the CI bracket separator switches to `"; "`
#'   automatically to avoid `"0,18 [0,07, 0,30]"` ambiguity.
#' @param align Numeric column alignment.
#'   `"decimal"` (default) -- pre-pad cells so decimal marks line
#'   up vertically (publication-style). For CI cells (`[LL, UL]`)
#'   the left bracket, the LL decimal point, the comma separator,
#'   the UL decimal point, and the right bracket are independently
#'   aligned across rows.
#'   `"center"`, `"right"`, or `"auto"` for legacy per-column
#'   alignment.
#' @param padding Non-negative integer giving the extra characters
#'   added to each data column's auto-computed width when the
#'   default `print` method renders the table. Default `0L`
#'   (compact -- fits more models in the same console / page
#'   width). Use `2L` (Stata-like) or `4L` for a more spacious
#'   look. Headers stay centered above the data region regardless
#'   of padding.
#' @param labels Named character vector overriding per-coefficient
#'   row labels. Names are coefficient term names (from
#'   [stats::terms()]); values are the displayed labels. E.g.
#'   `c("age" = "Age (years)", "sexM" = "Male (vs Female)")`.
#'   Default `NULL` (use raw term names).
#' @param title,note Override or suppress the auto-built caption /
#'   methodological footer. Three modes per argument:
#'   \itemize{
#'     \item `NULL` (default): the package builds the standard caption
#'       ("Linear regression on `<DV>`" / "Hierarchical linear
#'       regression on `<DV>`" / ...) and a methodological note
#'       (VCV type, p-adjust method, reference categories, ...).
#'     \item `FALSE`: the corresponding banner row is omitted from
#'       every output engine. Use when the surrounding manuscript
#'       provides its own caption / note.
#'     \item character string (length 1): replaces the auto-built
#'       text verbatim. The renderer applies no APA formatting on
#'       top -- supply the exact string you want displayed (multi-
#'       line notes accepted via embedded `"\n"`).
#'   }
#'   Validation messages, the spanner row, and the in-body change-
#'   stat rows are *not* affected -- they belong to the table
#'   structure, not to the banner.
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
#'   cleanly into Excel / Google Sheets / Word). The clipboard
#'   payload mirrors the Excel layout (title row, spanner row,
#'   header, body, footer note) but is plain text -- horizontal
#'   rules, cell merging, decimal alignment, monospace font, and
#'   factor-level indentation cannot be encoded in TSV and are
#'   therefore absent from the paste.
#'
#'   Paste behaviour by target:
#'   * **Excel / Google Sheets:** numerics are auto-detected and
#'     right-aligned; text cells stay left-aligned. (P-values such
#'     as `.005` get re-parsed as `0.005` by Excel's auto-format
#'     -- to preserve the APA leading-zero-dropped display, prefer
#'     `output = "excel"`.)
#'   * **Word:** the paste is converted to a Word table; all
#'     cells start left-aligned. Apply a Table Style
#'     (Insert > Table > Design) for APA-style borders, and
#'     set right-alignment on numeric columns
#'     (Layout > Align Right). For a self-contained Word file
#'     with borders and alignment pre-applied, use
#'     `output = "word"` instead.
#' @param word_path File path for `output = "word"`. Default
#'   `NULL` (required when `output = "word"`). The Word table inherits
#'   the flextable styling (Calibri font, APA borders, decimal-aligned
#'   numerics) and adds Word-specific features: an auto-numbered
#'   caption ("Table 1: ...", "Table 2: ...") via Word's `SEQ`
#'   field so multiple `table_regression()` calls in one document
#'   number consecutively; a re-printed header row on each page break;
#'   row split prevention so a single coefficient row never wraps
#'   across two pages; and an APA-styled note line (`*Note.*` italic
#'   prefix per APA Manual 7 §7.14).
#'
#'   **R Markdown / Quarto:** for embedded use, prefer `output = "flextable"`
#'   (returns the flextable object that knits to docx/HTML/PDF natively).
#'   `output = "word"` writes a standalone .docx file, suited to scripted
#'   exports rather than chunk-level rendering.
#' @param word_template Optional path to a custom .docx file used as the
#'   template for `output = "word"`. The template's header, footer,
#'   page size, margins, and named styles ("Table Caption" in
#'   particular) are honoured; the table is appended to the template
#'   body. Useful for institutional templates with pre-set headers
#'   ("APA Style", "Manuscript Submission Template", etc.). Default
#'   `NULL` (uses flextable's stock template).
#'
#'   **Customising the caption appearance:** the table caption is
#'   tagged with the Word named style `"Table Caption"`. The visual
#'   rendering (italic / bold / colour / font) follows whatever that
#'   style is set to in the docx template. The stock Word template
#'   renders `"Table Caption"` in italic — the APA Manual 7 §7.10
#'   condensed convention. For a different appearance (Nature-style
#'   bold non-italic, APA-strict 2-line bold-number / italic-title,
#'   etc.), edit the `"Table Caption"` style in a docx template and
#'   pass it via `word_template = "your_template.docx"`. Style-based
#'   delegation keeps the rendered caption consistent with the
#'   surrounding document and lets editorial conventions (Nature,
#'   APA-strict, journal-specific) be applied without modifying the
#'   call site.
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
#' # ---- Single-model usage ------------------------------------------
#' fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
#'
#' # Default APA layout: B / SE / 95% CI / p plus the n / R^2 /
#' # Adj.R^2 fit-stats footer. Factor reference level is annotated
#' # with `(ref.)` and shows an em-dash in the statistic columns.
#' table_regression(fit)
#'
#' # Standardised coefficients (beta) injected next to B. Four
#' # methods available; "refit" is the SPSS / Stata regress, beta
#' # gold standard.
#' table_regression(fit, standardized = "refit")
#'
#' # Custom column set: B + AME + AME-specific p-value. Note that
#' # the `p` token always belongs to B, never to AME -- use the
#' # explicit `ame_p` token for AME inference.
#' table_regression(
#'   fit,
#'   show_columns = c("b", "p", "ame", "ame_ci", "ame_p")
#' )
#'
#' # Group-token shortcut: "all_b" + "all_ame" expands to the full
#' # B / AME column families side by side.
#' table_regression(fit, show_columns = c("all_b", "all_ame"))
#'
#' # ---- Cluster-robust variance -------------------------------------
#' # CR2 (Bell-McCaffrey) with Satterthwaite-corrected df is the
#' # recommended default under few clusters. Three forms are accepted
#' # for `cluster`; the formula is preferred for composability with
#' # multi-way clustering and for programmatic robustness.
#' table_regression(fit, vcov = "CR2", cluster = ~region)
#' table_regression(fit, vcov = "CR2", cluster = "region")
#' table_regression(fit, vcov = "CR2", cluster = ~region:age_group)
#'
#' # ---- Hierarchical (nested) regression ----------------------------
#' # Adds in-table change-statistic rows (Delta R^2 / F-change /
#' # p-change for lm; LRT / p-change for glm) below the fit-stats.
#' # Note: hierarchical comparison requires identical observations
#' # across all models -- prepare a complete-case subset first so
#' # R's listwise deletion does not produce different `nobs` per
#' # model (which the function rejects).
#' sochealth_cc <- na.omit(
#'   sochealth[, c("wellbeing_score", "age", "sex", "smoking")]
#' )
#' m1 <- lm(wellbeing_score ~ age,                  data = sochealth_cc)
#' m2 <- lm(wellbeing_score ~ age + sex,            data = sochealth_cc)
#' m3 <- lm(wellbeing_score ~ age + sex + smoking,  data = sochealth_cc)
#' table_regression(
#'   list("Step 1" = m1, "Step 2" = m2, "Step 3" = m3),
#'   nested = TRUE
#' )
#'
#' # ---- Side-by-side variance comparison ----------------------------
#' # Same fit, three vcovs in one wide table. Useful for showing the
#' # sensitivity of inference to the variance assumption.
#' table_regression(
#'   list("Classical" = fit, "HC3" = fit, "CR2" = fit),
#'   vcov    = list("classical", "HC3", "CR2"),
#'   cluster = list(NULL, NULL, ~region)
#' )
#'
#' # ---- Tidy long format for downstream pipelines -------------------
#' broom::tidy(table_regression(fit))
#'
#' # ---- Mixed-effects models ----------------------------------------
#' # Linear mixed-effects (lme4). The footer adds a random-effects
#' # panel with sigma + Wald SE / CI from `merDeriv`, the Nakagawa
#' # marginal / conditional R^2 fit-stats, and a per-class p-value
#' # annotation line.
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   fit <- lme4::lmer(Reaction ~ Days + (Days | Subject),
#'                      data = lme4::sleepstudy)
#'   table_regression(fit)
#'
#'   # Switch to the variance scale (sigma^2 instead of sigma).
#'   table_regression(fit, re_scale = "variance")
#'
#'   # Minimal random-effects display: estimates only, no SE / CI.
#'   table_regression(fit, re_columns = "est")
#'
#'   # Suppress the random-effects panel entirely.
#'   table_regression(fit, show_re = FALSE)
#' }
#'
#' # Hierarchical mixed-effects comparison (nested LRT).
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   m1 <- lme4::lmer(Reaction ~ 1     + (1 | Subject),
#'                     data = lme4::sleepstudy, REML = FALSE)
#'   m2 <- lme4::lmer(Reaction ~ Days  + (1 | Subject),
#'                     data = lme4::sleepstudy, REML = FALSE)
#'   m3 <- lme4::lmer(Reaction ~ Days  + (Days | Subject),
#'                     data = lme4::sleepstudy, REML = FALSE)
#'   table_regression(list(m1, m2, m3), nested = TRUE)
#' }
#'
#' \dontrun{
#' # ---- Rich-format outputs (require optional Suggests packages) ----
#' table_regression(fit, output = "gt")
#' table_regression(fit, output = "flextable")
#' table_regression(fit, output = "tinytable")
#'
#' # ---- File outputs ------------------------------------------------
#' table_regression(fit, output = "excel",
#'                  excel_path = tempfile(fileext = ".xlsx"))
#' table_regression(fit, output = "word",
#'                  word_path  = tempfile(fileext = ".docx"))
#'
#' # ---- System clipboard (interactive use) --------------------------
#' table_regression(fit, output = "clipboard")
#' }
#'
#' @references
#' APA Manual 7 (American Psychological Association, 2020),
#' Tables 7.13-7.15.
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
#' Statistics*, 36(4), 672-683.
#'
#' Wasserstein, R.L., Schirm, A.L., & Lazar, N.A. (2019). Moving
#' to a world beyond "p < 0.05". *The American Statistician*,
#' 73(sup1), 1-19.
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
  show_columns = NULL,
  keep = NULL,
  drop = NULL,
  show_intercept = TRUE,
  intercept_position = c("first", "last"),
  factor_layout = c("grouped", "flat"),
  reference_style = c("row", "annotation", "footer", "none"),
  reference_label = "(ref.)",
  show_fit_stats = NULL,
  fit_stats_layout = c("first_col", "merged"),
  show_re = TRUE,
  re_scale = c("sd", "variance"),
  re_columns = c("est", "se", "ci"),
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
  output = c("default", "data.frame", "long", "gt", "flextable",
             "tinytable", "excel", "clipboard", "word"),
  excel_path = NULL,
  excel_sheet = "Regression",
  clipboard_delim = "\t",
  word_path = NULL,
  word_template = NULL
) {
  # Capture the cluster expression BEFORE evaluation so we can
  # (a) derive a friendly name for the footer ("clusters by
  # clinic_id" rather than "cluster vector supplied"), and
  # (b) intercept the bare-name NSE form (`cluster = region`)
  # before R's lazy-evaluation step fails with an unhelpful
  # "object 'region' not found" -- emit our migration error
  # pointing at `~region` / `"region"` instead.
  cluster_expr <- substitute(cluster)
  if (is.symbol(cluster_expr) &&
        !identical(as.character(cluster_expr), "cluster")) {
    sym <- as.character(cluster_expr)
    # Only complain if R would FAIL to evaluate the symbol --
    # `cluster = region` where `region` is a local variable IS
    # valid (vector form). We check existence in the caller frame.
    exists_local <- exists(sym, envir = parent.frame(), inherits = TRUE)
    if (!exists_local) {
      spicy_abort(
        c(sprintf(paste0("`cluster = %s`: unquoted bare names are not ",
                          "supported."), sym),
          "i" = sprintf("Use `cluster = ~%s` (formula) or `cluster = \"%s\"` (string).",
                         sym, sym),
          "i" = paste0("Bare-name NSE breaks under programmatic use ",
                        "(function wrapping, dynamic column choice, ",
                        "loops). The formula form is composable for ",
                        "multi-way clustering: `~region:year`.")),
        class = "spicy_invalid_input"
      )
    }
  }

  # Resolve enum args
  standardized <- match.arg(standardized)
  ci_method <- match.arg(ci_method)
  intercept_position <- match.arg(intercept_position)
  reference_style <- match.arg(reference_style)
  factor_layout <- match.arg(factor_layout)
  fit_stats_layout <- match.arg(fit_stats_layout)
  align <- match.arg(align)
  output <- match.arg(output)

  # ====================================================================
  # Validation cascade (Q21 -- 6 phases, ~29 steps, fail-fast).
  # Each helper lives in R/regression_validate.R and is called in the
  # documented order. Errors carry classed conditions
  # (spicy_invalid_input / spicy_unsupported); cross-arg semantic
  # warnings (Phase E) carry spicy_caveat or spicy_ignored_arg.
  # ====================================================================

  # Phase A -- input class (steps 1-3)
  models <- validate_models_input(models)
  n_models <- length(models)

  # Resolve `cluster` to a list of vectors (one per model) so the
  # rest of the pipeline sees a uniform atomic form. `cluster` may
  # be a formula (`~region`), a string (`"region"`), a vector
  # (`df$region`), a list of any of these (per-model), or NULL.
  # Vectors flow through unchanged; formulas / strings are looked
  # up in `model.frame(fit)`. See `?table_regression` @param
  # cluster for the full spec.
  cluster <- resolve_cluster_arg(cluster, models)

  # Phase B -- multi-model alignment (steps 4-8)
  validate_nested_alignment(models, nested)
  validate_vcov_cluster_lists(vcov, cluster, models)

  # Context-aware `show_columns` default. APA-7 Section 6.46 recommends
  # CIs for inference, so a single-model table (the publication
  # workflow) keeps CI in the default ("all_b"). Multi-model tables
  # drop CI to fit the side-by-side layout in a typical console /
  # page width ("all_b_compact"). The user can pass
  # `show_columns = "all_b"` (or atomic tokens) to restore CI.
  if (is.null(show_columns)) {
    is_multi <- is.list(models) && !inherits(models, "lm") &&
                  length(models) >= 2L
    show_columns <- if (is_multi) "all_b_compact" else "all_b"
  }
  # Expand group tokens (`"all_b"`, `"all_ame"`, ...) to atomic
  # tokens before validation; also raises an actionable migration
  # error if a legacy uppercase token (`"B"`, `"AME"`, ...) slips
  # through from < 0.12 code.
  show_columns <- expand_show_columns(show_columns)

  # Phase C -- vocabulary tokens (steps 9-12). validate_show_columns
  # also rejects "beta" combined with `standardized = "none"` (Q3).
  validate_show_columns(show_columns, standardized)
  validate_show_fit_stats(show_fit_stats)
  # Q3 -- auto-inject "beta" right after "B" when standardized != "none"
  # AND beta is not already requested. Done after validation so the
  # reject-beta-without-method branch fires before the orchestrator
  # mutates the user's show_columns.
  if (!identical(standardized, "none") && !"beta" %in% show_columns) {
    b_idx <- which(show_columns == "b")
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
  # When `nested = TRUE`, change-stat tokens (`r2_change` / `f_change`
  # / `p_change` for lm; `lrt_change` / `p_change` for glm) are
  # injected RIGHT AFTER `r2` / `adj_r2` / `AIC` so the table reads
  # "n / R^2 / Adj.R^2 / DeltaR^2 / F-change / p" as in APA Table 7.13.
  user_set_fit_stats <- !is.null(show_fit_stats)
  if (!user_set_fit_stats) {
    any_glm <- any(vapply(models, inherits, logical(1), "glm"))
    any_lm_only <- any(vapply(models, function(f) {
      inherits(f, "lm") && !inherits(f, "glm")
    }, logical(1)))
    # Phase 7c9a: mixed-effects class-aware default. lmer / glmer /
    # glmmTMB / lme get nobs + Nakagawa marginal/conditional R^2 + AIC
    # + BIC. classical R^2 is not defined, and the two R^2 of Nakagawa
    # are the standard publication statistic for mixed models (Nakagawa
    # & Schielzeth 2013; widely required by APA / journal templates).
    any_mixed <- any(vapply(models, function(f) {
      inherits(f, c("merMod", "lmerModLmerTest", "glmmTMB", "lme"))
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
    if (any_mixed) {
      show_fit_stats <- c(show_fit_stats,
                            if (!any_lm_only && !any_glm) "nobs",
                            "r2_marginal",
                            "r2_conditional",
                            "AIC", "BIC")
    }
    show_fit_stats <- unique(show_fit_stats)
    if (isTRUE(nested) && length(models) >= 2L) {
      show_fit_stats <- c(show_fit_stats, default_nested_tokens(models))
    }
  }

  # Class-aware token compatibility -- variance-explained tokens are
  # rejected on glm with a hint to the partial_chi2 / pseudo_r2_*
  # substitutes; pseudo_r2_* is rejected on lm. The check runs on
  # the resolved (class-aware default OR user-supplied) vector.
  validate_class_appropriate_tokens(models, show_columns, show_fit_stats)

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
  # only -- it derives SD(Y*) from the link-scale latent variance,
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

  # exponentiate: warn only when NO fit in the list has a non-identity
  # link. The transform applies wherever `family(fit)$link != "identity"`,
  # including glmer / glmmTMB binomial / poisson -- not just classical
  # glm. Phase 7c16: extended the check from `inherits(f, "glm")` to a
  # link-based check that covers any fit class with a `family()` method.
  if (isTRUE(exponentiate)) {
    has_non_identity <- any(vapply(models, function(f) {
      fam <- tryCatch(stats::family(f), error = function(e) NULL)
      if (is.null(fam) || is.null(fam$link)) return(FALSE)
      !identical(fam$link, "identity")
    }, logical(1)))
    if (!has_non_identity) {
      spicy_warn(
        c(
          "`exponentiate = TRUE` has no effect on identity-link fits.",
          "i" = paste0(
            "exp() is only applied to fits whose link is non-identity ",
            "(logit, probit, log, cloglog, inverse). Drop the argument ",
            "or remove the check."
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

  # Phase D -- argument values (steps 13-24)
  validate_ci_level(ci_level)
  validate_boot_n(boot_n)
  validate_logical_scalar(show_intercept, "show_intercept")
  validate_logical_scalar(nested, "nested")
  validate_caption_arg(title, "title")
  validate_caption_arg(note,  "note")
  validate_digit_arg(digits, "digits")
  validate_digit_arg(p_digits, "p_digits")
  validate_digit_arg(effect_size_digits, "effect_size_digits")
  validate_digit_arg(fit_digits, "fit_digits")
  validate_digit_arg(ic_digits, "ic_digits")
  validate_digit_arg(padding, "padding")
  validate_decimal_mark(decimal_mark)
  validate_reference_label(reference_label)
  validate_stars(stars)
  validate_p_adjust(p_adjust)
  validate_keep_drop(keep, drop)
  validate_logical_scalar(exponentiate, "exponentiate")
  validate_model_labels(model_labels, models)
  validate_outcome_labels(outcome_labels, models)
  validate_predictor_labels(labels, models)

  # Phase E -- cross-arg semantic warnings (no errors). The
  # standardized x non-additive caveat is emitted later (after
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
  # AME visibility caveat: when `ame` and `p` are both shown but
  # `ame_p` is not, the rendered `p` column is the B (or beta) p-
  # value, not the AME p-value. The two are **mathematically
  # identical** for `lm` with main effects only (no interactions,
  # no non-linear transforms), so emitting the caveat in that case
  # is a false alarm. Fire only when divergence is plausible:
  # any glm in the set, or any model with a non-additive term
  # (interaction / transform) detected by detect_non_additive_terms().
  if (all(c("ame", "p") %in% show_columns) &&
        !("ame_p" %in% show_columns)) {
    any_glm <- any(vapply(models, inherits, logical(1), "glm"))
    any_non_additive <- any(vapply(models, function(f) {
      detect_non_additive_terms(f)$has_problem
    }, logical(1)))
    if (any_glm || any_non_additive) {
      spicy_warn(
        c(
          paste0("`\"ame\"` and `\"p\"` shown without `\"ame_p\"`: the ",
                 "`p` column is for B (or beta), not the AME. They can ",
                 "differ under non-linear links or interactions."),
          "i" = "Add `\"ame_p\"` to display the AME-specific p-value."
        ),
        class = "spicy_caveat"
      )
    }
  }

  # Phase F -- output-dependent resource validation (file paths,
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
  # element. Falls back to NA -> footer renders "cluster vector
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
  use_ame_satt <- any_cr && "ame" %in% show_columns

  # ---- Per-model extraction (Layer 1) ------------------------------------
  default_id <- function(i) paste0("M", i)
  model_ids <- if (!is.null(names(models)) && all(nzchar(names(models)))) {
    names(models)
  } else {
    vapply(seq_len(n_models), default_id, character(1))
  }

  # Phase 0c sub-step C5: orchestrator now builds ONLY the frames list.
  # The legacy `extracts` list, the `.frame_to_legacy_extract()` adapter,
  # the legacy `apply_p_adjust()` -- all gone. Every downstream renderer
  # (title, footer, alignment, nested LRT) consumes frames directly.
  frames <- vector("list", n_models)
  for (i in seq_len(n_models)) {
    frames[[i]] <- as_regression_frame(
      models[[i]],
      model_id              = model_ids[i],
      vcov                  = vcov_list[[i]],
      cluster               = cluster_list[[i]],
      boot_n                = boot_n,
      ci_level              = ci_level,
      ci_method             = ci_method,
      standardized          = standardized,
      exponentiate          = exponentiate,
      show_columns          = show_columns,
      show_fit_stats        = show_fit_stats,
      use_ame_satterthwaite = use_ame_satt,
      cluster_name          = cluster_name_list[i]
    )
    # Attach precomputed non-additivity metadata so the standardized
    # caveat footer (build_standardized_caveat_footer_block_from_frames())
    # can decide without reaching back to the live fit.
    frames[[i]]$info$extras$non_additive <-
      detect_non_additive_terms(models[[i]])

    # p_adjust runs per model BEFORE alignment / keep-drop filtering
    # so the family is the model's full coefficient set (intercept
    # and reference rows excluded), not just the displayed subset.
    if (!identical(p_adjust, "none")) {
      frames[[i]]$coefs <- apply_p_adjust_to_frame_coefs(
        frames[[i]]$coefs, p_adjust)
    }
  }

  # Standardized-on-non-additive caveat (Q15, Phase E)
  emit_standardized_caveat_if_needed(models, standardized)

  # Nested-comparison change stats (APA Table 7.13 in-table rows).
  # Augments each `frames[[i]]$info$fit_stats` list with `r2_change`,
  # `f_change`, `p_change`, etc. (NA for Model 1). MUST run BEFORE
  # align_frames() because align_frames() rbinds the per-model
  # fit_stats and the column schemas have to be uniform.
  # Phase 0c sub-step C3: attach to frames; the legacy path
  # (attach_nested_stats_to_extracts) is no longer called because
  # align_frames() reads from frames directly.
  if (isTRUE(nested)) {
    frames <- attach_nested_stats_to_frames(frames, models)
  }

  # ---- Multi-model alignment + wide pivot (Layer 2) ----------------------
  # align_frames() consumes the frames list directly and produces the
  # aligned long-format data structure consumed by the body builder.
  aligned <- align_frames(
    frames,
    model_ids = model_ids,
    show_intercept = show_intercept,
    intercept_position = intercept_position,
    reference_style = reference_style
  )

  # `keep` / `drop` filter -- runs AFTER alignment (so canonical term
  # ordering is preserved for the surviving terms) and AFTER
  # p_adjust (so adjusted p-values reflect the model's full
  # coefficient family, not just the displayed subset).
  aligned <- apply_keep_drop_filter(aligned, keep = keep, drop = drop)

  # ---- Title + footer (Step 7) -------------------------------------------
  # `title` / `note` resolution: NULL -> auto, FALSE -> suppress,
  # character -> override. The auto-build is gated by `is.null(title)`
  # only; the footer side of the resolution lives below (`footer_main`
  # is always built so its diagnostic side-effects still fire, e.g. the
  # "none" + flat informational message tied to reference_style; the
  # override / suppression happens last).
  title <- if (is.null(title)) {
    # Phase 0c sub-step C1: title is now built from frames directly,
    # bypassing the legacy-extract adapter for this code path. The
    # other renderers (footer / body / alignment) continue to consume
    # the legacy extract shape until sub-steps C2-C4 land.
    build_regression_title_from_frames(frames, nested = nested)
  } else if (isFALSE(title)) {
    NULL
  } else {
    as.character(title)
  }
  # Phase 0c sub-step C2.last: the footer dispatcher now reads from
  # frames directly. Each builder calls its _from_frames sibling (added
  # in C1, C2.a, C2.b, C2.c). The legacy build_regression_footer() is
  # kept in the codebase for C5 cleanup so we can revert quickly if a
  # corner case slips through the byte-equivalence gates.
  # Phase 7c7d: validate + thread random-effects display args through
  # the footer dispatcher.
  re_scale_val <- match.arg(re_scale)
  re_columns_val <- .validate_re_columns(re_columns)

  footer_main <- build_regression_footer_from_frames(
    frames,
    standardized = standardized,
    p_adjust = p_adjust,
    stars = stars,
    nested = nested,
    show_columns = show_columns,
    reference_style = reference_style,
    show_re = isTRUE(show_re),
    re_scale = re_scale_val,
    re_columns = re_columns_val
  )

  # "none" + flat: silent information loss flag. Inform-level
  # message (not a warning), once per call. Reference convention
  # must be carried by the surrounding article / caption. Only
  # emitted when at least one factor was detected (otherwise
  # there's nothing to be silent about).
  # Phase 0c sub-step C5: factor-detection scan now reads from frames
  # directly. Frame coefs have parent_var = term for non-factor predictors
  # (no NA fallback), so the "any factor" test is parent_var != term on
  # any non-intercept row.
  if (identical(reference_style, "none") &&
        identical(factor_layout, "flat") &&
        any(vapply(frames, function(f) {
          coefs <- f$coefs
          !is.null(coefs) && nrow(coefs) > 0L &&
            any(coefs$parent_var != coefs$term & coefs$term != "(Intercept)")
        }, logical(1)))) {
    spicy_inform(
      c(paste0("`reference_style = \"none\"` with `factor_layout = ",
               "\"flat\"`: reference levels are not displayed anywhere."),
        "i" = paste0("State the reference convention in the surrounding ",
                     "text or table caption.")),
      class = "spicy_silent_reference"
    )
  }

  full_footer_str <- if (is.null(note)) {
    if (length(footer_main)) footer_main else NULL
  } else if (isFALSE(note)) {
    NULL
  } else {
    as.character(note)
  }

  # ---- Render (Layer 3) --------------------------------------------------
  # Q1: model_labels precedence is explicit > names(list) > default.
  # Partial-name auto-fill: `list("Step 1" = m1, m2)` becomes
  # `c("Step 1", "Model 2")`. validate_models_input() rejects
  # duplicates but accepts partial naming since user intent is
  # unambiguous (named slots win, unnamed slots default).
  effective_model_labels <- if (!is.null(model_labels)) {
    model_labels
  } else if (!is.null(names(models)) && any(nzchar(names(models)))) {
    nms <- names(models)
    missing_idx <- which(!nzchar(nms))
    nms[missing_idx] <- paste0("Model ", missing_idx)
    nms
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
    factor_layout = factor_layout,
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
  # Stash the print-time padding as an attribute so the print
  # method can honour the call-site choice (and tooling like
  # `as.data.frame()` / broom continues to see the raw rendered
  # cells unchanged).
  attr(rendered, "padding") <- as.integer(padding)
  attr(rendered, "fit_stats_layout") <- fit_stats_layout

  # ---- Output dispatch (Step 11) -----------------------------------------
  dispatch_regression_output(
    rendered = rendered,
    aligned = aligned,
    output = output,
    excel_path = excel_path,
    excel_sheet = excel_sheet,
    clipboard_delim = clipboard_delim,
    word_path = word_path,
    word_template = word_template
  )
}
