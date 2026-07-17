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
#'   \item Sample columns: `"n"` -- per-row N, populated by
#'     [table_regression_uv()] screens (each predictor block is its
#'     own fit); models without per-row N data drop the column.
#'     `"n_events"` -- outcome event counts as `events/N`, per
#'     factor level (reference row included) and model totals on
#'     continuous rows, computed on each model's own estimation
#'     sample (STROBE item 16's "data behind the association";
#'     NEJM-style `no. of events/total no.`). Binary (binomial)
#'     outcomes and right-censored `coxph` fits -- other models
#'     raise `spicy_invalid_input`.
#'   \item Survival estimands -- `coxph` only: `"rmst"`,
#'     `"rmst_se"`, `"rmst_ci"`, `"rmst_p"` (restricted-mean-
#'     survival-time difference over `[0, tau]`) and
#'     `"risk_diff"`, `"risk_diff_se"`, `"risk_diff_ci"`,
#'     `"risk_diff_p"` (cumulative-incidence difference at
#'     `at_time`), by g-computation from the fitted model with
#'     bootstrap inference. The horizon arguments are required;
#'     see `tau` / `at_time`.
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
#'   \item Counts: `"nobs"`, `"weighted_nobs"`, `"n_events"` (Cox
#'     models: number of events, reported alongside `n` per the
#'     field convention; blank for other classes).
#'   \item Variance explained (`lm` only): `"r2"`, `"adj_r2"`,
#'     `"omega2"`.
#'   \item Pseudo-\eqn{R^2}{R^2} (`glm` and ordinal `polr` / `clm`):
#'     `"pseudo_r2_mcfadden"` (McFadden 1974),
#'     `"pseudo_r2_nagelkerke"` (Nagelkerke 1991),
#'     `"pseudo_r2_tjur"` (Tjur 2009; binomial only).
#'   \item Residual scale: `"sigma"` (lm \eqn{\hat{\sigma}}{sigma-hat}
#'     / glm dispersion), `"rmse"`.
#'   \item Bayesian fits only: `"r2_bayes"` (posterior-median
#'     Bayesian \eqn{R^2}{R^2}, Gelman et al. 2019 -- in the
#'     all-Bayesian default), `"elpd_loo"` and `"looic"` (PSIS-LOO
#'     expected log predictive density and its deviance-scale twin,
#'     Vehtari et al. 2017; opt-in, a few seconds per model; the
#'     footer discloses the elpd standard error), and `"waic"`
#'     (Watanabe-Akaike; PSIS-LOO is generally preferred).
#'   \item Negative-binomial dispersion (`MASS::glm.nb` only):
#'     `"theta"` (\eqn{V = \mu + \mu^2/	heta}{V = mu + mu^2/theta})
#'     and `"alpha"` (\eqn{= 1/	heta}{= 1/theta}, the Stata `nbreg`
#'     convention). Refused for other families.
#'   \item Effect size: `"f2"`.
#'   \item Information criteria: `"aic"`, `"aicc"`, `"bic"`,
#'     `"deviance"` (lowercase like every other token; the rendered
#'     row labels stay `AIC` / `AICc` / `BIC`).
#'   \item Change-stats for hierarchical comparison
#'     (active under `nested = TRUE`; see *Hierarchical
#'     comparison* below): `"r2_change"`, `"adj_r2_change"`,
#'     `"f_change"`, `"f2_change"`, `"lrt_change"`,
#'     `"aic_change"`, `"aicc_change"`, `"bic_change"`,
#'     `"deviance_change"`, `"p_change"`.
#' }
#'
#' Default (resolved when `NULL`) is class-aware: lm fits get
#' `c("nobs", "r2", "adj_r2")`; glm and ordinal `polr` / `clm` fits get
#' `c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "aic")`;
#' mixed lm + glm sets union both groups (the renderer per-row
#' em-dashes the inappropriate cell); Cox fits get
#' `c("nobs", "n_events", "aic")`. When `nested = TRUE`, the
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
#'   \item `"classical"` -- OLS (lm) / Fisher information (glm).
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
#' Inferential regimes for `lm` / `glm` (B and AME share the same
#' regime):
#' \itemize{
#'   \item `classical`, `HC*` -> t with `df.residual` (`lm`) /
#'     z-asymptotic (`glm`).
#'   \item `bootstrap`, `jackknife` -> z asymptotic.
#'   \item `CR0`-`CR3` -> t with **Satterthwaite-corrected df** (B
#'     via [clubSandwich::coef_test()]; AME via
#'     [clubSandwich::linear_contrast()]; Pustejovsky & Tipton
#'     2018). Under non-linear terms (`poly()`, `I()`, `log()`,
#'     `splines::ns()`), AME falls back to z-asymptotic with a
#'     `spicy_fallback` warning.
#' }
#' For other classes the t-vs-z axis follows the estimator's native
#' reference distribution (e.g. `glm`, `glmmTMB`, survival, ordinal,
#' `betareg`, `mlogit` use z; `lm`, `lme`, `lmer`, `rms::ols` use t).
#'
#' ## Multinomial models: outcome categories as columns
#'
#' A single `nnet::multinom` model renders the publication layout:
#' predictors as rows, one column group per **non-reference outcome
#' category** (spanner = category name), so a predictor's effect can
#' be compared across equations along its row. The mandatory footer
#' note `Reference outcome: <level>.` names the base category (Stata's
#' "base outcome" line); `outcome_labels` relabels the spanners (e.g.
#' `"Student vs Employed"`). Fit statistics print once, under the
#' first group. When per-category AMEs are requested the reference
#' category appears as a last, AME-only group (its coefficient cells
#' are empty -- the reference has no equation; its AMEs complete the
#' zero-sum across categories). The default `show_columns` compacts
#' to B / SE / p exactly like multi-model tables; request atomic
#' tokens to restore CIs. Multi-model and `nested = TRUE` multinomial
#' tables keep the one-row-per-(category, predictor) layout --
#' categories-within-models would need two spanner levels.
#' `tidy()` and `output = "long"` always return the long form
#' (`"<category>: <term>"` rows), whatever the display;
#' `as_structured()` mirrors the displayed table (one column set per
#' category), as it does for every other layout.
#'
#' ## Robust SE availability by model class
#'
#' Not every estimator is defined for every class. A robust `vcov`
#' the class cannot honour fails fast with `spicy_unsupported_vcov`
#' -- never a silent model-based result under a robust label:
#'
#' \describe{
#'   \item{`lm`, `glm`, `MASS::glm.nb`}{all of `classical`, `HC*`,
#'     `CR*`, `bootstrap`, `jackknife`.}
#'   \item{`mlogit`}{`classical` + `CR*` only (cluster at the
#'     choice-situation level) -- `sandwich::vcovHC()` mis-scales the
#'     sandwich for mlogit's per-choice-situation scores, so `HC*` is
#'     refused.}
#'   \item{`lmer`, `lme`, `glmmTMB`, `coxph`, `survreg`,
#'     `mgcv::gam`/`bam`, `polr`, `clm`, `betareg`,
#'     `survey::svyglm`, `nnet::multinom`,
#'     `rms` (`ols`/`lrm`/`cph`/`Glm`)}{`classical`
#'     + `CR*` only -- `HC*` and the resamplers (which refit
#'     `lm`/`glm`) are not defined for these. `clm` with a scale /
#'     nominal (partial-PO) component is `classical` only.
#'     `multinom` needs \pkg{sandwich} >= 3.1-2 (which added its
#'     `estfun()` method); its `cluster` is one entry per
#'     observation.}
#'   \item{Other classes (`glmer`,
#'     `rstanarm`/`brms`, ...)}{`classical` (model-based) only.}
#' }
#'
#' Cluster-robust backends differ by class but are each cross-validated
#' to the field-standard oracle: `lm`/`glm`/`lmer`/`lme`/`glmmTMB` use
#' \pkg{clubSandwich} (CR2 = Bell-McCaffrey, with Satterthwaite df for
#' `lm`/`lme`/`lmer`); `coxph`/`cph` use the Lin-Wei grouped-dfbeta
#' sandwich (identical to `coxph(..., cluster=)`);
#' `survreg`/`gam`/`polr`/`clm`/`betareg`/`mlogit`/`multinom` use
#' [sandwich::vcovCL()]; `svyglm` uses the design-aware
#' \pkg{clubSandwich} estimator; `rms` fits use [rms::robcov()] (which
#' needs the fit's `x = TRUE, y = TRUE`). These single cluster
#' sandwiches have no CR0-CR3 bias-reduction variants, so the requested
#' `CR*` maps to the one available estimator. `cluster` length is one
#' entry per observation, except `mlogit` (one per choice situation)
#' and censored `coxph` (one per subject).
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
#' # Bayesian fits (rstanarm / brms)
#'
#' `stanreg` and `brmsfit` models are summarized from their posterior
#' draws: `B` is the posterior median, `SE` the posterior MAD SD (the
#' scaled median absolute deviation -- the pairing of *Regression and
#' Other Stories* and rstanarm's own print; equal to the posterior SD
#' for a normal posterior), and the interval an equal-tailed credible
#' interval (header `95% CrI`; `ci_method = "hdi"` opts into the
#' highest-density interval, header `95% HDI`). There is no p-value
#' or t-statistic; group presets (`"all_b"`, ...) expand without
#' them, and the opt-in `"pd"` column reports the probability of
#' direction with a footer definition (Makowski et al. 2019). Under
#' `exponentiate = TRUE` all quantities are computed from the
#' exponentiated draws directly (the SE is the posterior MAD SD on
#' the ratio scale, not a delta-method approximation).
#'
#' Fit statistics: `"r2_bayes"` (in the Bayesian default) plus the
#' opt-in `"elpd_loo"` / `"looic"` / `"waic"`, whose standard errors
#' are disclosed in the footer; unreliable estimates (PSIS-LOO Pareto
#' k above the sample-size-specific threshold
#' \eqn{\min(1 - 1/\log_{10} S, 0.7)}{min(1 - 1/log10(S), 0.7)} of
#' Vehtari et al. 2024 -- the same bound `loo::loo()` prints -- and
#' WAIC p_waic > 0.4) add a footer caveat instead of being
#' silenced. Every table is backed by an automatic sampler-diagnostics
#' guard (R-hat >= 1.01, ESS below 100 per chain -- floored at 400 so
#' fewer chains never weaken the bar --, divergent
#' transitions, E-BFMI < 0.2, per Vehtari et al. 2021): problems add
#' a footer line and raise a warning classed
#' `spicy_bayes_diagnostics` (nested under `spicy_caveat`, so
#' `withCallingHandlers(spicy_bayes_diagnostics = ...)` mutes the
#' guard selectively); clean fits print nothing. Per-coefficient
#' `"rhat"` / `"ess_bulk"` / `"ess_tail"` columns are available in
#' all-Bayesian tables, as is `"mcse"` -- the Monte Carlo standard
#' error of the displayed posterior median, the criterion for how
#' many digits a table can honestly show (a displayed digit is
#' Monte-Carlo stable when twice the MCSE stays below it; Gelman,
#' Vehtari, McElreath et al. 2026, sec. 11.6). Under
#' `exponentiate = TRUE` the MCSE is recomputed on the exponentiated
#' draws.
#'
#' Refused on principle (classed error, never a silent fallback):
#' likelihood-based fit statistics (`"aic"`, pseudo-R², ...),
#' `p_adjust`, robust / cluster `vcov` (model the clustering with
#' group-level terms instead), `ci_method = "profile"` /
#' `"boot_percentile"`, and non-MCMC fits
#' (`algorithm = "meanfield"` / `"optimizing"` -- refit with
#' `algorithm = "sampling"`). See
#' `vignette("table-regression-bayesian")`.
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
#'     `glm` only numeric X (Long & Freese 2014 Section 4.7.2
#'     "x-standardization").
#'   \item `"posthoc"` -- post-hoc scaling. lm:
#'     \eqn{\beta = B \times SD(X) / SD(Y)}{beta = B * SD(X) / SD(Y)};
#'     glm: X-only \eqn{\beta = B \times SD(X)}{beta = B * SD(X)}
#'     (Y is undefined on the link scale).
#'   \item `"basic"` -- like `"posthoc"` but factor dummies are
#'     scaled by their column SD.
#'   \item `"smart"` -- Gelman (2008): continuous numeric inputs
#'     are scaled by `2 * SD(X)` (a +/-1 SD swing then spans the
#'     same range as a binary's 0 to 1 step); binary inputs --
#'     numeric 0/1 and factor dummies -- are left unscaled. (Before
#'     0.13.0 the rule was applied inverted; see NEWS.)
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
#' **Interactions and transforms.** Under `"refit"`, an interaction's
#' \eqn{\beta}{beta} is the coefficient of the product of the z-scored
#' components -- the recommended treatment for such models (Cohen et
#' al. 2003 Section 7.7; Aiken & West 1991; Friedrich 1982). Under
#' `"posthoc"`, `"basic"`, and `"smart"`, the product / transformed
#' design column is treated as a single numeric column and scaled by
#' its own SD (under `"smart"`, by `2 * SD` when the product column is
#' continuous; a binary product column -- e.g. a binary-by-binary
#' interaction -- stays unscaled like any binary input) -- the
#' convention of SPSS beta, Stata `regress, beta`, SAS PROC REG `STB`, and
#' `lm.beta::lm.beta()`, identical to
#' `effectsize::standardize_parameters(method = "basic")` on those
#' columns. The two conventions differ whenever the components are
#' correlated; a `spicy_caveat` warns and the footer names the
#' convention actually used. `"refit"` declines formulas with inline
#' transforms (`log(x)`, `poly()`, `factor()` written in the formula):
#' the model frame's evaluated columns cannot be re-evaluated on
#' z-scored data, so spicy falls back to `"posthoc"` with a warning
#' (`effectsize`'s refit instead standardizes *before* the transform
#' -- a different estimand). Pre-build transformed columns in `data`
#' for an exact refit.
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
#' @param models A fitted model object, or a list of such fits
#'   (named or unnamed; classes may be mixed). Single fits are
#'   auto-promoted to a 1-element list. A broad set of model classes
#'   is supported -- linear / generalised linear (`lm`, `glm`,
#'   `MASS::glm.nb`), mixed-effects (`lmer`, `lme`, `glmmTMB`),
#'   survival (`coxph`, `survreg`), ordinal (`polr`, `clm`),
#'   `mgcv::gam`/`bam`, `betareg`, `mlogit`, `survey::svyglm`, `rms`
#'   (`ols`/`lrm`/`cph`/`Glm`), and Bayesian (`rstanarm`/`brms`),
#'   among others. An unsupported class raises
#'   `spicy_unsupported_class`. Raw data + formula is not accepted
#'   -- fit-only API.
#' @param vcov Variance-covariance estimator: `"classical"`,
#'   `"HC0"`-`"HC5"`, `"CR0"`-`"CR3"`, `"bootstrap"`, or
#'   `"jackknife"`. A scalar is recycled to all models; a list
#'   (one string per model) allows mixed estimators. Default
#'   `"classical"`. The resampling estimators (`lm` / `glm`,
#'   including `MASS::glm.nb`) refit each replicate on resampled rows
#'   of the fixed evaluated design; for `glm.nb` the dispersion
#'   parameter theta is held at its full-sample estimate (Stata's
#'   `nbreg, vce(bootstrap)` re-estimates it per replicate -- the two
#'   conventions differ slightly). Resampling that fails on nearly
#'   every replicate raises `spicy_resampling_failed` rather than
#'   silently reporting a different estimator. See *Inference and
#'   standard errors*.
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
#'   (`glm` and ordinal `MASS::polr` / `ordinal::clm`) uses the
#'   profile-likelihood CI from [stats::confint()] --
#'   [MASS::confint.glm()] for `glm`, `confint.polr()` / `confint.clm()`
#'   for ordinal fits -- asymmetric, exact for likelihood-based
#'   inference (Venables & Ripley *MASS* Section 7.2). Only the CI bounds
#'   change; estimate, SE, statistic and p-value remain Wald. For ordinal
#'   fits the profile covers the predictor coefficients (the cut-point
#'   thresholds stay Wald), and a robust `vcov` takes precedence (its
#'   Wald-robust CIs are used instead, with a consolidated warning).
#'   `"profile"` with `lm` raises `spicy_invalid_input`.
#'   `"boot_percentile"` (requires `vcov = "bootstrap"` for every
#'   model) replaces the coefficient CI bounds with equal-tailed
#'   percentile intervals of the bootstrap replicates (the
#'   [boot::boot.ci()] `type = "perc"` convention; Davison & Hinkley
#'   1997, ch. 5), reusing the same resamples as the bootstrap SEs.
#'   Only the CI bounds change; estimate, SE, statistic and p-value
#'   remain Wald from the bootstrap covariance -- the Stata convention
#'   (normal-based table CIs by default, percentile on request via
#'   `estat bootstrap`). AME and standardized-beta CIs are not
#'   covered. With `exponentiate = TRUE` the percentile bounds are
#'   exponentiated (percentile intervals are
#'   transformation-respecting).
#'   `"hdi"` (Bayesian `stanreg` / `brmsfit` fits only) replaces the
#'   default equal-tailed credible interval with the highest-density
#'   interval -- the shortest interval containing `ci_level` of the
#'   posterior draws (Kruschke 2015), relabelling the column header
#'   `95% HDI`. Unlike the equal-tailed interval the HDI is not
#'   transformation-invariant, so under `exponentiate = TRUE` it is
#'   recomputed on the exponentiated draws rather than transformed.
#'   Requesting `"hdi"` for a frequentist fit, or `"profile"` /
#'   `"boot_percentile"` for a Bayesian fit, raises
#'   `spicy_invalid_input`.
#' @param standardized Standardisation method for the `"beta"`
#'   column. One of `"none"` (default), `"refit"`, `"posthoc"`,
#'   `"basic"`, `"smart"`, `"pseudo"`. `"pseudo"` is *glm only*
#'   (Menard 2011 fully-standardised); using it with `lm()` raises
#'   `spicy_invalid_input`. Supported classes: `lm`, `glm`
#'   (incl. `MASS::glm.nb`), and the mixed engines (`lmer` /
#'   `glmer` / `glmmTMB` / `nlme::lme`); any other class raises
#'   `spicy_unsupported_standardized` rather than rendering an
#'   empty beta column. See the *Standardised coefficients*
#'   section.
#' @param exponentiate Logical. When `TRUE`, `B`, the CI bounds, and
#'   the SE (delta method: `SE_OR = OR x SE_log-odds`) are
#'   `exp()`-transformed for links where the result is a ratio:
#'   logit (`OR`), log (`IRR` / `RR` / `MR` per family, generic
#'   `exp(B)` for other log-link families -- a genuine ratio of
#'   means), and binomial / ordinal cloglog (`HR`; grouped-time
#'   proportional hazards, Prentice & Gloeckler 1978). The statistic
#'   and p-value stay on the link scale (invariant under monotone
#'   transformation). Identity-link fits are left untouched (a
#'   `spicy_ignored_arg` warning fires when no model in the table
#'   exponentiates), so mixed `lm` + logit tables keep working. Any
#'   other link (probit, cauchit, inverse -- the
#'   `Gamma()` default --, `1/mu^2`, sqrt, ordinal `loglog`, ...)
#'   raises `spicy_invalid_input`: the exponential of such a
#'   coefficient has no ratio interpretation, and reporting it
#'   would mislabel the estimate. Report response-scale effects for
#'   those models via the AME column instead. Note that the displayed
#'   CI is the link-scale CI with exponentiated endpoints (Wald or
#'   profile per `ci_method`), so it is asymmetric around the ratio
#'   and cannot be reconstructed as `estimate ± z × SE`; the
#'   delta-method SE follows the Stata convention (`[R] logistic`,
#'   Methods and formulas: `se(OR) = OR × se(b)`) and the log-scale
#'   CI endpoints carry the uncertainty more faithfully than this SE.
#'   The footer states the SE scale and the CI asymmetry whenever an
#'   SE column is displayed. Default `FALSE`.
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
#' @param tau RMST horizon: the `"rmst"` column family reports the
#'   restricted-mean-survival-time difference over `[0, tau]`
#'   (`coxph` and `survreg` fits). A positive time on the outcome's scale, or
#'   `"minmax"` for the smallest per-group maximum follow-up (the
#'   resolved value is disclosed in the table note). No default: the
#'   horizon defines the estimand. Stratified fits (`strata()`) are
#'   supported: standardization keeps each subject's own stratum
#'   baseline, disclosed in the table note.
#' @param at_time Landmark time for the `"risk_diff"` column family:
#'   the difference in cumulative incidence at `at_time` (`coxph`
#'   and `survreg` fits). A positive time on the outcome's scale; no default.
#' @param show_columns Character vector of tokens selecting the
#'   per-coefficient columns and their display order. Accepts
#'   **atomic tokens** (`"b"`, `"se"`, `"ci"`, `"t"`, `"p"`,
#'   `"beta"`, `"n"`, `"n_events"`, `"pd"` (probability of
#'   direction, Bayesian fits only), `"rhat"` / `"ess_bulk"` /
#'   `"ess_tail"` / `"mcse"` (per-coefficient sampler diagnostics and
#'   the Monte Carlo standard error of the displayed posterior
#'   median, all-Bayesian tables only), `"ame"`, `"ame_se"`, `"ame_ci"`,
#'   `"ame_p"`, `"rmst"` + `"rmst_se"` / `"rmst_ci"` / `"rmst_p"`,
#'   `"risk_diff"` + its `_se` / `_ci` / `_p` companions,
#'   `"partial_f2"` + `"partial_f2_ci"`, `"partial_eta2"` +
#'   `"partial_eta2_ci"`, `"partial_omega2"` +
#'   `"partial_omega2_ci"`, `"partial_chi2"`) and **group tokens**
#'   (`"all_b"`, `"all_b_compact"`, `"all_b_full"`, `"all_beta"`,
#'   `"all_ame"`, `"all_ame_compact"`, `"all_f2"`, `"all_eta2"`,
#'   `"all_omega2"`). See *Vocabulary tokens* in the details for
#'   the full enumeration. Default `NULL` selects a context-aware
#'   layout: `"all_b"` (single model) or `"all_b_compact"`
#'   (multi-model, and the single-multinomial outcome-as-columns
#'   layout, which has the same width pressure -- restore CIs with
#'   atomic tokens, e.g. `c("b", "se", "ci", "p")`). The `"p"`
#'   token is always the B / beta p-value; for the AME-specific
#'   p-value use `"ame_p"`.
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
#' @param show_thresholds For ordinal cumulative-link models
#'   (`MASS::polr`, `ordinal::clm`), whether to display the estimated
#'   category thresholds (cut-points) as a subordinate `"Thresholds"`
#'   block of rows below the predictors, carrying B / SE / CI / p like
#'   the predictor rows. Default `TRUE`. `FALSE` collapses them to a
#'   compact one-line footer note instead. Thresholds are reported on
#'   the log-odds (B) scale and are **never exponentiated** (under
#'   `exponentiate = TRUE` their rows stay on the log-odds scale).
#'   Has no effect on non-ordinal models, and the rows are shown only
#'   when a coefficient column (`"b"`/`"beta"`) is in `show_columns`.
#' @param show_components For models with secondary components,
#'   whether to display them as labelled subordinate blocks of rows
#'   below the primary (count / conditional / location) coefficients.
#'   Default `TRUE`:
#'   \itemize{
#'     \item `pscl::zeroinfl` and `glmmTMB(ziformula = )`: a
#'       `Zero-inflation` block -- the model for the probability of a
#'       **structural (excess) zero**.
#'     \item `pscl::hurdle`: a `Zero hurdle` block -- the model for the
#'       probability of a **nonzero count** (note the opposite direction
#'       vs zero-inflation; the footer names each block's meaning).
#'     \item `glmmTMB(dispformula = )`: a `Dispersion` block (only when
#'       dispersion was actually modelled; log scale, never
#'       exponentiated).
#'     \item `ordinal::clm(scale = ~)`: a `Scale effects` block -- the
#'       covariate effects on the **log standard deviation of the
#'       latent response**. Never exponentiated: their exponential is a
#'       ratio of latent standard deviations, not an odds ratio.
#'   }
#'   Component rows carry full Wald inference (B / SE / z / p / CI),
#'   join the `p_adjust` family, and take significance stars. Under
#'   `exponentiate = TRUE` a component is exponentiated **only when its
#'   link makes the result an odds ratio** (the logit zero components);
#'   probit / cauchit / cloglog zero links and count-type hurdle zero
#'   parts stay on the link scale, disclosed in the footer. `FALSE`
#'   omits the blocks (the title still names the model type).
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
#'     \item `glm`, ordinal `polr` / `clm`:
#'       `c("nobs", "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke",
#'       "aic")` (McFadden = Stata `ologit` default,
#'       Nagelkerke = SPSS PLUM).
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
#' @param show_re Logical. `TRUE` (default) renders the random-effects
#'   variance components of a mixed-effects fit (`lmer`, `glmer`,
#'   `glmmTMB`, `lme`) as a subordinate **"Random effects" block of
#'   table rows** below the fixed effects: one row per standard
#'   deviation / correlation per grouping factor, plus the residual,
#'   each with its estimate, SE, and CI in the shared coefficient
#'   columns. The group sizes (`N (groups)`) and the ICC render as
#'   fit-statistic rows; the footer reports the estimation method
#'   (`REML` / `ML`) and the likelihood-ratio test of the whole
#'   random part against the no-random-effects model, with the
#'   boundary-corrected chi-bar-squared p-value (Self & Liang 1987;
#'   Stram & Lee 1994). Variance-component rows deliberately carry
#'   **no per-row p-value**: a Wald test of a variance is invalid at
#'   the boundary of the parameter space, and no reporting guideline
#'   requests one (see the *Mixed-effects models* section of
#'   `vignette("table-regression")`). `FALSE` suppresses the block.
#'   No effect on fits without random effects (`lm`, `glm`,
#'   `coxph`, ...).
#' @param re_scale One of `"sd"` (default) or `"variance"`.
#'   Controls the display scale of the random-effects rows:
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
#'   `c("est", "se", "ci")` controlling which cells of the
#'   random-effects rows are **displayed** (`"est"` is mandatory);
#'   deselected SE / CI cells render as an em-dash on those rows
#'   only. Useful for slimming output (`re_columns = "est"`) or
#'   for journals that want only standard errors
#'   (`re_columns = c("est", "se")`). Display-only: the underlying
#'   data (`broom::tidy()`, [as_structured()]) always carries the
#'   full SE + CI.
#'
#'   Note. Under the default `re_ci = "wald"` the SE and CI are Wald
#'   (`est ± z * SE`, clamped at 0 for variances). Wald can be
#'   optimistic near the variance boundary (Self & Liang 1987
#'   chi-bar-squared); request boundary-respecting profile-likelihood
#'   intervals with `re_ci = "profile"` when robustness is critical.
#'   See the *Mixed-effects models* section of
#'   `vignette("table-regression")`.
#'
#'   For `lmer` / `glmer` fits these SEs come from `merDeriv`, whose
#'   cost grows superlinearly with the number of observations (about a
#'   minute at n ≈ 2,700). Above `options("spicy.re_se_max_n")`
#'   (default `1000`) they are skipped: the rows keep their estimates,
#'   the SE / CI cells render as em-dashes, a table note states the
#'   omission, and a `spicy_caveat` warning points here. Raise the cap
#'   (e.g. `options(spicy.re_se_max_n = Inf)`) to force the
#'   computation, or test the random terms with `re_test = "lrt"`.
#' @param re_test One of `"none"` (default), `"lrt"`, or `"rlrt"`.
#'   Opt-in **per-term significance test** for the random-effect
#'   variance components, filling the otherwise-empty p column of
#'   the Random effects rows. Never a Wald test (invalid at the
#'   boundary sigma = 0):
#'   \itemize{
#'     \item `"lrt"`: likelihood-ratio test of each random term vs
#'       the model refitted without it (the term's variance plus its
#'       covariances with the other terms of its bar), referred to
#'       the boundary-corrected chi-bar-squared mixture
#'       `0.5 chi2(q-1) + 0.5 chi2(q)` (Self & Liang 1987; Stram &
#'       Lee 1994). The reduction scheme matches
#'       `lmerTest::ranova()`; the mixture reference makes the
#'       p-value exact-asymptotic rather than conservative. A bar's
#'       intercept is tested only when it is the bar's single term.
#'       Supported: `lmer`, `glmer`, `glmmTMB`, and `lme` with a
#'       simple `random = ~ terms | group` structure.
#'     \item `"rlrt"`: exact restricted likelihood-ratio test with a
#'       simulated finite-sample null (`RLRsim::exactRLRT()`;
#'       Crainiceanu & Ruppert 2004). Only defined for a Gaussian
#'       `lmer` / `lme` fit with a single variance component.
#'   }
#'   The test statistic and df stay out of the displayed t/z column
#'   (they are chi-square-scale, not t/z) but are carried in
#'   `broom::tidy()` (`test_type` `"chibar2"` / `"rlrt"`). The
#'   whole-block LR test in the footer is unaffected. Correlation
#'   and residual rows are never tested (a correlation is tested
#'   jointly with its slope; the residual has no zero-variance
#'   null). Refits happen once per random term: expect a
#'   noticeable cost on large models. For structures outside these
#'   two routes (e.g. multiple variance components needing a
#'   finite-sample null), `pbkrtest::PBmodcomp()` offers a
#'   parametric-bootstrap LR test on the same nested pair of fits
#'   (Halekoh & Hojsgaard 2014) -- run it directly and report its
#'   p-value alongside the table.
#' @param re_ci One of `"wald"` (default) or `"profile"`. Uncertainty
#'   route for the random-effect variance-component rows of `lmer` /
#'   `glmer` fits:
#'   \itemize{
#'     \item `"wald"`: SE and symmetric CI from the observed
#'       information (`merDeriv`), subject to the
#'       `options("spicy.re_se_max_n")` size cap (see `re_columns`).
#'     \item `"profile"`: **profile-likelihood CIs** via
#'       `confint(fit, method = "profile")` -- the route lme4 itself
#'       documents and defaults to. The intervals are asymmetric and
#'       respect the boundary at 0; no SE is shown (lme4's position:
#'       a symmetric SE misdescribes the skewed sampling distribution
#'       of a variance). Sidesteps the size cap entirely (roughly two
#'       seconds per variance parameter even at n ~ 7,000; `glmer`
#'       profiles cost more). The footer discloses the method, and
#'       likelihood intervals transform exactly, so
#'       `re_scale = "variance"` shows the profile CI of the variance
#'       itself.
#'   }
#'   `glmmTMB` and `nlme::lme` fits keep their engine-native CIs
#'   (TMB's `sdreport`; nlme's `apVar`) and refuse `"profile"`.
#' @param model_labels Per-model labels used as the **column-group
#'   spanner** above each model's sub-columns (console + gt /
#'   flextable / tinytable / Excel / Word renderers). `NULL`
#'   (default) resolves automatically; see *Multi-model semantics*
#'   for the full rule. A character vector of length
#'   `length(models)` overrides. Refused (error) for a single
#'   multinomial model: there the column groups are outcome
#'   categories, relabelled via `outcome_labels`.
#' @param outcome_labels Optional **Outcome body row** override.
#'   `NULL` (default) hides the row entirely -- under the
#'   multi-model spanner the DV is already visible above the data.
#'   A character vector of length `length(models)` forces an
#'   explicit Outcome row with those values (the spanner stays as
#'   `"Model 1, ..."` unless `model_labels` is also supplied).
#'   `FALSE` also suppresses the row. **Single multinomial model**
#'   (outcome-as-columns layout): repurposed as the category
#'   spanner override -- a character vector with one label per
#'   non-reference outcome category (unique, in model order), e.g.
#'   `c("Student vs Employed", ...)`; the reference category's
#'   AME-only group (when displayed) keeps its own name, and `FALSE`
#'   is a no-op (there is no Outcome row to suppress).
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
#' `spicy_unsupported_vcov`, `spicy_unsupported_standardized`,
#' `spicy_missing_pkg`, `spicy_missing_column`,
#' `spicy_ignored_arg`, `spicy_caveat`, `spicy_fallback`. See
#' [`spicy`][spicy::spicy-package] for the full taxonomy.
#'
#' @seealso
#' [table_regression_models()] for the registry of supported model
#' classes and the per-family behaviour reference (also reachable as
#' `?table_regression_mixed`, `?table_regression_ordinal`, ...). If a
#' class is not listed there, try `table_regression(fit)` anyway --
#' unsupported classes error with a clear message.
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
#' `compute_model_vcov()`, `compute_coef_inference()`,
#' `compute_wald_test()`.
#' broom integration:
#' [broom::tidy()], [broom::glance()].
#'
#' @examples
#' # ---- Single-model usage ------------------------------------------
#' fit <- lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
#'
#' # Default APA layout: B / SE / 95% CI / p plus the n / R^2 /
#' # Adj.R^2 fit-stats footer. Factor reference level is annotated
#' # with `(ref.)` and shows an en dash in the statistic columns.
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
#' Davison, A.C. & Hinkley, D.V. (1997). *Bootstrap methods and
#' their application*. Cambridge University Press.
#'
#' Friedrich, R.J. (1982). In defense of multiplicative terms in
#' multiple regression equations. *American Journal of Political
#' Science*, 26(4), 797-833.
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

  # Multinomial outcome-as-columns gate (single nnet::multinom model,
  # no nested comparison). Computed early because it drives the
  # `show_columns` compaction default below and refuses
  # `model_labels` fail-fast: in this layout the column groups are
  # outcome categories, not models, and their spanners are relabelled
  # via `outcome_labels`. See R/regression_multinom_layout.R.
  mn_columns_active <- .multinom_columns_active(models, nested)
  if (mn_columns_active && !is.null(model_labels)) {
    spicy_abort(
      c(paste0("`model_labels` does not apply to a single multinomial ",
               "model: the column groups are outcome categories, ",
               "not models."),
        "i" = paste0("Use `outcome_labels` to relabel the category ",
                     "spanners.")),
      class = "spicy_invalid_input"
    )
  }

  # Context-aware `show_columns` default. APA-7 Section 6.46 recommends
  # CIs for inference, so a single-model table (the publication
  # workflow) keeps CI in the default ("all_b"). Multi-model tables
  # drop CI to fit the side-by-side layout in a typical console /
  # page width ("all_b_compact"); the multinomial columns layout has
  # the same width pressure (one column set per outcome category) and
  # compacts identically. The user can pass atomic tokens
  # (e.g. `c("b", "se", "ci", "p")`) to restore CI.
  if (is.null(show_columns)) {
    is_multi <- is.list(models) && !inherits(models, "lm") &&
                  length(models) >= 2L
    show_columns <- if (is_multi || mn_columns_active) {
      "all_b_compact"
    } else {
      "all_b"
    }
  }
  # Phase 7c23 (item d): explicit `"all_b"` / `"all_ame"` group tokens
  # auto-compact in multi-model layouts -- parity with the lm default
  # which already auto-switches NULL -> "all_b_compact" in the branch
  # above. Without this, a user passing `show_columns = "all_ame"` to
  # a 5-model side-by-side table got 4 cells per model (AME + SE + CI
  # + p) for 20 columns total, while the lm equivalent `"all_b"` would
  # auto-drop CI for the same multi-model context.
  is_multi <- is.list(models) && !inherits(models, "lm") &&
                length(models) >= 2L
  if (is_multi || mn_columns_active) {
    if ("all_b"   %in% show_columns) {
      show_columns <- sub("^all_b$",   "all_b_compact",   show_columns)
    }
    if ("all_ame" %in% show_columns) {
      show_columns <- sub("^all_ame$", "all_ame_compact", show_columns)
    }
  }
  # Expand group tokens (`"all_b"`, `"all_ame"`, ...) to atomic
  # tokens before validation; also raises an actionable migration
  # error if a legacy uppercase token (`"B"`, `"AME"`, ...) slips
  # through from < 0.12 code.
  all_bayes_models <- length(models) > 0L &&
    all(vapply(models, inherits, logical(1), c("stanreg", "brmsfit")))
  show_columns <- expand_show_columns(show_columns,
                                      bayesian = all_bayes_models)

  # Phase C -- vocabulary tokens (steps 9-12). validate_show_columns
  # also rejects "beta" combined with `standardized = "none"` (Q3).
  validate_show_columns(show_columns, standardized)
  validate_show_fit_stats(show_fit_stats)
  # Survival estimand horizons: the horizon defines the estimand, so
  # it is explicit and mandatory -- and refused when unused.
  validate_estimand_horizons(show_columns, tau, at_time)
  # Phase 7c23 (item a): coerce `FALSE` to `character(0)` so the
  # downstream class-aware default branch (line ~ 996) treats the
  # explicit "suppress" as "no tokens to inject", instead of leaving
  # FALSE in the path and tripping `inherits(show_fit_stats, "character")`
  # checks further down.
  if (isFALSE(show_fit_stats)) show_fit_stats <- character(0)
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
    # Bayesian fits inherit from lm / glm (stanreg subclasses both), but
    # their defaults are their own: no likelihood-based information
    # criteria, no classical or pseudo R^2 -- nobs only. Exclude them
    # from the frequentist class buckets so the glm branch does not
    # inject AIC / pseudo-R^2 tokens the Bayesian gate would refuse.
    is_bayes <- vapply(models, inherits, logical(1),
                       c("stanreg", "brmsfit"))
    any_glm <- any(vapply(models, inherits, logical(1), "glm") & !is_bayes)
    any_lm_only <- any(vapply(models, function(f) {
      inherits(f, "lm") && !inherits(f, "glm")
    }, logical(1)) & !is_bayes)
    # Phase 7c9a: mixed-effects class-aware default. lmer / glmer /
    # glmmTMB / lme get nobs + Nakagawa marginal/conditional R^2 + AIC
    # + BIC. classical R^2 is not defined, and the two R^2 of Nakagawa
    # are the standard publication statistic for mixed models (Nakagawa
    # & Schielzeth 2013; widely required by APA / journal templates).
    any_mixed <- any(vapply(models, function(f) {
      inherits(f, c("merMod", "lmerModLmerTest", "glmmTMB", "lme"))
    }, logical(1)))
    # Ordinal cumulative-link (polr / clm): nobs + the two pseudo-R^2 that
    # generalise the binary-logit default (McFadden = Stata ologit default,
    # Nagelkerke = SPSS PLUM) + AIC. summary.polr reports Residual Deviance +
    # AIC; Stata/SPSS lead with the pseudo-R^2, so those are the headline here.
    any_ordinal <- any(vapply(models, function(f) {
      inherits(f, c("polr", "clm"))
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
                            "aic")
    }
    if (any_mixed) {
      # n_groups + icc render as fit-stat ROWS (aligned per model), like
      # sjPlot / modelsummary -- not footer prose. ICC is blank when not
      # computable (random slopes, multiple grouping factors, non-Gaussian
      # without a link-scale formula).
      show_fit_stats <- c(show_fit_stats,
                            if (!any_lm_only && !any_glm) "nobs",
                            "n_groups", "icc",
                            "r2_marginal",
                            "r2_conditional",
                            "aic", "bic")
    }
    if (any_ordinal) {
      show_fit_stats <- c(show_fit_stats,
                            if (!any_lm_only && !any_glm && !any_mixed) "nobs",
                            "pseudo_r2_mcfadden",
                            "pseudo_r2_nagelkerke",
                            "aic")
    }
    if (any(is_bayes)) {
      # Bayesian default: n + the posterior-median Bayesian R^2 --
      # the R^2 every other family's default carries, in its
      # draws-based form (Gelman et al. 2019). elpd_loo / looic stay
      # opt-in (~2 s of PSIS-LOO per model). In a MIXED table the
      # frequentist models simply render a blank r2_bayes cell,
      # mirroring how "r2" is blank for the Bayesian side -- silently
      # dropping the Bayesian model's default R^2 would contradict
      # the never-silently-wrong policy.
      show_fit_stats <- c(show_fit_stats,
                            if (!any_lm_only && !any_glm) "nobs",
                            "r2_bayes")
    }
    # multinom: same pseudo-R2 pair as the other categorical families
    # (dev/fit_stats_by_class.md). Its null model is intercept-only, whose
    # log-likelihood has the ordinal path's closed form.
    any_multinom <- any(vapply(models, function(f) {
      inherits(f, "multinom")
    }, logical(1)))
    if (any_multinom) {
      show_fit_stats <- c(show_fit_stats,
                            if (!any_lm_only && !any_glm && !any_mixed &&
                                  !any_ordinal) "nobs",
                            "pseudo_r2_mcfadden",
                            "pseudo_r2_nagelkerke",
                            "aic")
    }
    # Cox proportional hazards (survival::coxph and rms::cph, which
    # inherits from it): the field convention reports n AND the number
    # of events (EpiRHandbook survival chapter; Stata stcox header).
    # n_events is NA outside the coxph frame, and the renderer skips
    # the row when no model carries a value.
    any_coxph <- any(vapply(models, function(f) {
      inherits(f, "coxph")
    }, logical(1)))
    if (any_coxph) {
      show_fit_stats <- c(show_fit_stats,
                            if (!any_lm_only && !any_glm && !any_mixed &&
                                  !any_ordinal) "nobs",
                            "n_events", "aic")
    }
    # Universal safety net: a class matched by none of the branches above
    # (betareg, survreg, coxph, multinom, mlogit, fixest, rms, stan, ...) still
    # gets N + AIC, so no fit ever renders a blank model-fit block. The
    # per-class passes refine this to each family's field-standard set (e.g.
    # concordance for coxph, deviance-explained for gam). nobs / AIC are defined
    # for essentially every likelihood fit; an absent token is skipped per row.
    if (length(show_fit_stats) == 0L) {
      show_fit_stats <- c("nobs", "aic")
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

  # Bayesian fits and ci_method. The posterior interval is the only
  # interval a posterior has: `"hdi"` is the Bayesian-only opt-in
  # (highest-density interval, the Kruschke-school convention; the
  # equal-tailed interval stays the default), while `"profile"` and
  # `"boot_percentile"` are likelihood / resampling constructions with
  # no posterior analogue -- both are refused HERE with the Bayesian
  # alternative, not routed into a downstream generic error.
  is_bayes_ci <- vapply(models, inherits, logical(1),
                        c("stanreg", "brmsfit"))
  if (identical(ci_method, "hdi") && !all(is_bayes_ci)) {
    offending <- class(models[[which(!is_bayes_ci)[1]]])[1]
    spicy_abort(
      c(
        paste0("`ci_method = \"hdi\"` is defined only for Bayesian ",
               "fits (`stanreg` / `brmsfit`)."),
        "x" = sprintf(paste0("`%s` has no posterior draws to take a ",
                             "highest-density interval of."), offending),
        "i" = paste0("Frequentist fits use `ci_method = \"wald\"` ",
                     "(default), `\"profile\"`, or `\"boot_percentile\"`.")
      ),
      class = "spicy_invalid_input"
    )
  }
  if (ci_method %in% c("profile", "boot_percentile") && any(is_bayes_ci)) {
    spicy_abort(
      c(
        sprintf("`ci_method = \"%s\"` is not defined for Bayesian fits.",
                ci_method),
        "i" = paste0("A posterior has no profile likelihood or bootstrap ",
                     "replicates; the credible interval is computed from ",
                     "the posterior draws."),
        "i" = paste0("Use the default equal-tailed credible interval, ",
                     "or `ci_method = \"hdi\"` for the highest-density ",
                     "interval.")
      ),
      class = "spicy_invalid_input"
    )
  }

  # `ci_method = "profile"` is available for glm and ordinal (polr / clm),
  # which have a genuine profile-likelihood CI (confint.glm / confint.polr /
  # confint.clm). It is rejected for lm, where Wald CIs are exact under the
  # normal-error assumption so a profile path would just be Wald with extra
  # cost. (Other classes silently keep Wald -- the frame method ignores the
  # request when it has no profile path.)
  if (identical(ci_method, "profile")) {
    # Profile-likelihood CIs are genuine only where confint() profiles the
    # coefficients: plain `glm` (MASS::confint.glm), `polr` (confint.polr) and
    # `clm` (confint.clm). class(f)[1] pins the PLAIN glm and excludes glm
    # subclasses whose confint() is not profile (svyglm design-based, gam).
    # Any other class is rejected rather than silently returning Wald.
    supports_profile <- vapply(models, function(f) {
      class(f)[1] %in% c("glm", "polr", "clm")
    }, logical(1))
    if (!all(supports_profile)) {
      offending <- class(models[[which(!supports_profile)[1]]])[1]
      spicy_abort(
        c(
          paste0("`ci_method = \"profile\"` is available only for `glm` and ",
                 "ordinal (`polr` / `clm`) fits."),
          "x" = sprintf(paste0("`%s` has no profile-likelihood path; its CIs ",
                               "would silently fall back to Wald."), offending),
          "i" = paste0("Use `ci_method = \"wald\"` (default), or a robust ",
                       "`vcov` for robust (Wald) CIs.")
        ),
        class = "spicy_invalid_input"
      )
    }

    # Profile CIs are model-based likelihood quantities; a robust or
    # resampling vcov takes precedence (its Wald CIs are used). The frame
    # methods (lm / glm / polr / clm) resolve the request themselves --
    # this is the single consolidated disclosure of that override.
    vlist <- if (is.list(vcov)) vcov else rep(list(vcov), length(models))
    overridden <- vapply(vlist, function(v) {
      !(is.character(v) && length(v) == 1L &&
          v %in% c("model", "classical"))
    }, logical(1))
    if (any(overridden)) {
      mod_labels <- if (!is.null(names(models)) && all(nzchar(names(models)))) {
        names(models)
      } else {
        paste("Model", seq_along(models))
      }
      spicy_warn(
        c(
          paste0("`ci_method = \"profile\"` is ignored under a robust or ",
                 "resampling `vcov`: ",
                 paste(mod_labels[overridden], collapse = ", "), "."),
          "i" = paste0(
            "Profile CIs are model-based likelihood quantities; the ",
            "requested `vcov` takes precedence and its Wald CIs are ",
            "reported (the footer names the estimator)."
          )
        ),
        class = "spicy_ignored_arg"
      )
    }
  }

  # `ci_method = "boot_percentile"` is a CI-only refinement of the
  # bootstrap: equal-tailed percentile intervals of the SAME replicates
  # that produced the bootstrap covariance (boot::boot.ci type = "perc"
  # convention). It therefore requires vcov = "bootstrap" for every
  # model (jackknife has no replicate distribution to take quantiles
  # of), and is not defined for standardized betas (their CIs rescale
  # the raw Wald machinery, not the replicates).
  if (identical(ci_method, "boot_percentile")) {
    vlist <- if (is.list(vcov)) vcov else rep(list(vcov), length(models))
    all_boot <- all(vapply(vlist, function(v) {
      is.character(v) && length(v) == 1L && identical(v, "bootstrap")
    }, logical(1)))
    if (!all_boot) {
      spicy_abort(
        c(
          paste0("`ci_method = \"boot_percentile\"` requires ",
                 "`vcov = \"bootstrap\"` for every model."),
          "i" = paste0("Percentile CIs are quantiles of the bootstrap ",
                       "replicates; they reuse the same resamples as the ",
                       "bootstrap SEs (no second resampling pass)."),
          "i" = paste0("Set `vcov = \"bootstrap\"` (tune with `boot_n`), ",
                       "or drop `ci_method = \"boot_percentile\"`.")
        ),
        class = "spicy_invalid_input"
      )
    }
    if (!identical(standardized, "none")) {
      spicy_abort(
        c(
          paste0("`ci_method = \"boot_percentile\"` is not available ",
                 "with `standardized`."),
          "i" = paste0("Percentile CIs are quantiles of the raw-",
                       "coefficient replicates; standardised betas would ",
                       "need their own resampling scheme.")
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

  # exponentiate: the "no effect" warning is emitted AFTER frame extraction
  # (see below, just after the frames loop), keyed on each frame's own
  # info$extras$exp_applied. Doing it post-extraction lets one check cover
  # every model class -- including polr / clm / multinom / fixest / betareg,
  # which have no stats::family() method -- instead of guessing the link of
  # each raw fit up front.

  # gaussian-glm caveat: the user fitted glm() with the default
  # (gaussian / identity) family, which is mathematically equivalent
  # to lm() but loses access to the variance-explained effect sizes
  # (partial_f2 / \u03B7\u00B2 / \u03C9\u00B2) and to the AME-Satterthwaite path A.
  # Following the "transparency over rejection" rule, we accept the
  # fit and surface the suggestion via spicy_caveat.
  for (i in seq_along(models)) {
    f <- models[[i]]
    # Only a PLAIN glm() earns the "use lm() instead" caveat. svyglm, gam/bam,
    # and other glm subclasses inherit "glm" but the refit-with-lm() suggestion
    # is wrong for them (it would drop the survey design / the smooth terms), so
    # gate on the leading class being exactly "glm".
    if (identical(class(f)[1L], "glm")) {
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
  # Finding (a) of the Bayesian recon: a posterior has no p-values, so a
  # p_adjust request was a silent no-op. Refuse it when every model is
  # Bayesian (mixed-class tables keep it: the frequentist columns adjust).
  if (!identical(p_adjust, "none") &&
      length(models) > 0L &&
      all(vapply(models, inherits, logical(1),
                 c("stanreg", "brmsfit")))) {
    spicy_abort(
      c("`p_adjust` is not available for Bayesian fits: there are no p-values to adjust.",
        "i" = paste0("Bayesian tables report posterior medians and ",
                     "credible intervals; the probability-of-direction ",
                     "column (`show_columns = \"pd\"`) is the closest ",
                     "posterior summary.")),
      class = "spicy_invalid_input"
    )
  }
  validate_keep_drop(keep, drop)
  validate_logical_scalar(exponentiate, "exponentiate")
  validate_model_labels(model_labels, models)
  if (!mn_columns_active) {
    # Multinomial columns layout: `outcome_labels` relabels the
    # category spanners instead of the per-model Outcome row; its
    # length (= number of non-reference categories) is validated in
    # .multinom_columns_spanners() at the explode.
    validate_outcome_labels(outcome_labels, models)
  }
  validate_predictor_labels(labels, models)
  # Random-effects display args: validate fail-fast (before the extraction
  # loop), so a bad value errors immediately instead of after the expensive
  # per-model extraction, and with a spicy_invalid_input class (finding m3).
  # Reused both to materialise the RE rows (in the loop) and by the footer.
  validate_logical_scalar(show_re, "show_re")
  validate_logical_scalar(show_components, "show_components")
  re_scale_val <- .validate_re_scale(re_scale)
  re_columns_val <- .validate_re_columns(re_columns)
  re_test_val <- .validate_re_test(re_test)
  if (!identical(re_test_val, "none")) {
    if (!isTRUE(show_re)) {
      spicy_abort(
        c(
          "`re_test` requires the random-effects rows (`show_re = TRUE`).",
          "i" = "The per-term test fills the p column of those rows."
        ),
        class = "spicy_invalid_input"
      )
    }
  }
  re_ci_val <- .validate_re_ci(re_ci, models)
  if (identical(re_ci_val, "profile") && !isTRUE(show_re)) {
    spicy_abort(
      c(
        "`re_ci` requires the random-effects rows (`show_re = TRUE`).",
        "i" = "The profile CIs fill the CI cells of those rows."
      ),
      class = "spicy_invalid_input"
    )
  }

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
      rep(NA_character_, n_models)   # nocov -- validate_vcov_cluster_lists() Step 7 aborts on length mismatch
    }
  } else {
    rep(extract_arg_column_name(cluster_expr), n_models)
  }
  # Wrappers (table_regression_uv) pass an already-evaluated cluster
  # through do.call, so the expression walk above yields NA. They stamp
  # the user-facing column name as an attribute instead; it wins.
  attr_names <- if (is.list(cluster) && !is.atomic(cluster)) {
    vapply(cluster, function(x) {
      attr(x, "spicy_cluster_name", exact = TRUE) %||% NA_character_
    }, character(1))
  } else {
    rep(attr(cluster, "spicy_cluster_name", exact = TRUE) %||%
          NA_character_, n_models)
  }
  if (length(attr_names) == n_models) {
    cluster_name_list <- ifelse(is.na(attr_names), cluster_name_list,
                                attr_names)
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
      cluster_name          = cluster_name_list[i],
      re_ci                 = re_ci_val,
      tau                   = tau,
      at_time               = at_time
    )
    # Truthfulness gate: `standardized` requires a class with a real
    # standardized-coefficients path (the beta attach). Classes without
    # one used to accept the argument and render the auto-injected beta
    # column entirely EMPTY -- silent wrong output under a requested
    # label. supports$standardise_refit is declared TRUE only where the
    # attach exists: lm / glm (incl. glm.nb) and the mixed engines
    # (lmer / glmer / glmmTMB / lme).
    if (!identical(standardized, "none") &&
        !isTRUE(frames[[i]]$info$supports$standardise_refit)) {
      model_tag <- if (n_models > 1L) {
        sprintf("model %d (class %s)", i, class(models[[i]])[1L])
      } else {
        sprintf("this model class (%s)", class(models[[i]])[1L])
      }
      spicy_abort(
        c(
          sprintf("`standardized = \"%s\"` is not supported for %s.",
                  standardized, model_tag),
          "i" = paste0("Standardized coefficients are available for ",
                       "`lm`, `glm` (incl. `MASS::glm.nb`), and mixed ",
                       "models (`lmer` / `glmer` / `glmmTMB` / ",
                       "`nlme::lme`)."),
          "i" = paste0("To compare predictor effects in other classes, ",
                       "use average marginal effects where supported: ",
                       "`show_columns = c(\"b\", \"ame\")`.")
        ),
        class = "spicy_unsupported_standardized"
      )
    }
    # Truthfulness gate: RMST / risk-difference columns exist only for
    # coxph fits (the g-computation engine); any other class would
    # render the requested columns empty.
    wants_estimand <- any(c("rmst", "rmst_se", "rmst_ci", "rmst_p",
                            "risk_diff", "risk_diff_se",
                            "risk_diff_ci", "risk_diff_p") %in%
                            show_columns)
    if (wants_estimand &&
        !any(frames[[i]]$coefs$estimate_type %in%
               c("rmst", "risk_diff"))) {
      model_tag <- if (n_models > 1L) {
        sprintf("model %d (class %s)", i, class(models[[i]])[1L])
      } else {
        sprintf("this model class (%s)", class(models[[i]])[1L])
      }
      spicy_abort(
        c(
          sprintf(
            "RMST / risk-difference columns are not available for %s.",
            model_tag
          ),
          "i" = paste0("They are computed by g-computation from a ",
                       "`survival::coxph` fit on right-censored ",
                       "single-record data, or a `survival::survreg` ",
                       "fit.")
        ),
        class = "spicy_invalid_input"
      )
    }
    # Truthfulness gate: `show_columns = "n_events"` requires per-row
    # outcome event counts, which only binomial fits with an ungrouped
    # binary response provide (.attach_event_counts). A frame without
    # them would render a requested column entirely empty -- error
    # instead, naming the model.
    if ("n_events" %in% show_columns &&
        is.null(frames[[i]]$coefs$events)) {
      model_tag <- if (n_models > 1L) {
        sprintf("model %d (class %s)", i, class(models[[i]])[1L])
      } else {
        sprintf("this model class (%s)", class(models[[i]])[1L])
      }
      spicy_abort(
        c(
          sprintf(
            "`show_columns = \"n_events\"` is not available for %s.",
            model_tag
          ),
          "i" = paste0("Outcome event counts need a binary (binomial ",
                       "family) outcome with an ungrouped 0/1, logical, ",
                       "or two-level factor response -- or a coxph fit ",
                       "on right-censored single-record data."),
          "i" = paste0("The model-total event count is also available ",
                       "as a fit statistic for survival models: ",
                       "`show_fit_stats = c(\"nobs\", \"n_events\")`.")
        ),
        class = "spicy_invalid_input"
      )
    }
    # Exponentiate centrally so EVERY model class honours `exponentiate = TRUE`
    # (OR / IRR / HR / RR / MR / TR), driven by each frame's own info$family /
    # info$supports rather than a per-class `family()` lookup. Classes that
    # exponentiate internally (lm/glm legacy, mixed, survival) set
    # info$extras$exp_applied = TRUE, so .apply_exp_to_frame() no-ops for them;
    # polr / clm / multinom / mlogit / betareg / fixest / pscl / rms / mgcv /
    # svyglm / stan -- which never applied exp -- are handled here.
    exp_out <- .apply_exp_to_frame(
      frames[[i]]$coefs, frames[[i]]$info, exponentiate
    )
    frames[[i]]$coefs <- exp_out$coefs
    frames[[i]]$info  <- exp_out$info

    # Attach precomputed non-additivity metadata so the standardized
    # caveat footer (build_standardized_caveat_footer_block_from_frames())
    # can decide without reaching back to the live fit.
    frames[[i]]$info$extras$non_additive <-
      detect_non_additive_terms(models[[i]])

    # Component blocks (zero-inflation / zero hurdle / dispersion) as labelled
    # subordinate rows blocks. Materialised AFTER the central exponentiate --
    # each block applies its OWN link-gated exp (logit -> OR; probit /
    # count-type / dispersion stay on the link scale) -- and BEFORE p_adjust:
    # component coefficients are substantive hypotheses and join the p-adjust
    # family (unlike ordinal thresholds).
    cb_i <- frames[[i]]$info$extras$component_blocks
    if (isTRUE(show_components) &&
        "b" %in% show_columns &&
        !is.null(cb_i) && length(cb_i) > 0L) {
      frames[[i]]$coefs <- .append_component_rows(
        frames[[i]]$coefs, cb_i, exponentiate)
      # Record whether each block's exp actually applied (footer gloss).
      frames[[i]]$info$extras$component_blocks <- lapply(cb_i, function(b) {
        b$exp_applied <- isTRUE(exponentiate) && isTRUE(b$exp_ok)
        b
      })
    }

    # p_adjust runs per model BEFORE alignment / keep-drop filtering
    # so the family is the model's full coefficient set (intercept
    # and reference rows excluded), not just the displayed subset.
    if (!identical(p_adjust, "none")) {
      frames[[i]]$coefs <- apply_p_adjust_to_frame_coefs(
        frames[[i]]$coefs, p_adjust)
    }

    # Ordinal thresholds as a subordinate "Thresholds" block of rows
    # (show_thresholds = TRUE, the default). The (k - 1) cut-points stashed in
    # info$extras$thresholds become coefs rows HERE -- after exp + p_adjust, so
    # neither touches them: cut-points are never exponentiated, and they are not
    # part of the p-adjust family. Gated on a coefficient column being shown
    # (else the rows would render empty); when thresholds are not promoted to
    # rows they fall back to the compact footer line (titlefooter layer).
    thr_i <- frames[[i]]$info$extras$thresholds
    if (isTRUE(show_thresholds) &&
        any(c("b", "beta") %in% show_columns) &&
        !is.null(thr_i) && is.data.frame(thr_i) && nrow(thr_i) > 0L) {
      frames[[i]]$coefs <- .append_threshold_rows(
        frames[[i]]$coefs, thr_i, ci_level)
    }

    # Scale (dispersion) coefficients of a `scale = ~` clm: their own
    # "Scale effects" block, materialised here -- after exp + p_adjust, like
    # the thresholds -- because they act on log(sigma) of the latent variable
    # (exp(zeta) is a ratio of latent SDs, never an odds ratio). Gated on a
    # coefficient column being shown, else the rows would render empty.
    # Without the block the fit rendered as if its scale part did not exist.
    sc_i <- frames[[i]]$info$extras$scale_effects
    if (isTRUE(show_components) &&
        any(c("b", "beta") %in% show_columns) &&
        !is.null(sc_i) && is.data.frame(sc_i) && nrow(sc_i) > 0L) {
      frames[[i]]$coefs <- .rbind_union(frames[[i]]$coefs, sc_i)
    }

    # Random effects as a subordinate "Random effects" block of rows (show_re =
    # TRUE, the default). Materialised HERE -- after exp + p_adjust, so variance
    # components are never exponentiated or p-adjusted. Gated on a coefficient
    # column being shown (else the rows render empty). ICC / N (groups) / the
    # method tag and the chi-bar-squared LR test stay in the footer (they are
    # summaries, not estimated variance parameters).
    # Gated on the RAW coefficient column ("b"): a beta-only display has no
    # column a variance component can honestly fill (a vc is never
    # standardized), so the block is omitted rather than rendered empty.
    re_i <- frames[[i]]$info$random_effects
    if (isTRUE(show_re) &&
        "b" %in% show_columns &&
        !is.null(re_i) && is.data.frame(re_i$variance_components) &&
        nrow(re_i$variance_components) > 0L) {
      # Opt-in per-term significance tests (LRT with the chi-bar-squared
      # boundary correction, or the exact RLRT): fill the otherwise-NA test
      # columns of the vc rows. Refits happen here, once per random term.
      term_tests <- if (!identical(re_test_val, "none")) {
        .compute_re_term_tests(models[[i]], re_test_val)
      } else {
        NULL
      }
      frames[[i]]$coefs <- .append_random_effects_rows(
        frames[[i]]$coefs, re_i, re_scale = re_scale_val,
        term_tests = term_tests, re_test = re_test_val)
    }
  }

  # Singular mixed fits: the table note states the FACT (boundary
  # estimate, SE/CI omitted); the actionable advice belongs here, to the
  # analyst, once per table -- not in a published table note.
  singular_mixed <- vapply(frames, function(fr) {
    isTRUE(fr$info$extras$has_singular) &&
      (fr$info$class %||% "") %in%
        c("lmerMod", "lmerModLmerTest", "glmerMod", "glmmTMB", "lme")
  }, logical(1))
  if (any(singular_mixed)) {
    labels <- if (!is.null(names(models)) && all(nzchar(names(models)))) {
      names(models)
    } else {
      paste("Model", seq_along(models))
    }
    spicy_warn(
      c(
        sprintf(
          "Singular fit (%s): a random-effect variance component is estimated at exactly zero.",
          paste(labels[singular_mixed], collapse = ", ")
        ),
        "i" = paste0(
          "Consider simplifying the random structure (drop the ",
          "offending term), or test whether it belongs with ",
          "`re_test = \"lrt\"`. See `help(\"isSingular\", package = ",
          "\"lme4\")`."
        )
      ),
      class = "spicy_caveat"
    )
  }

  # Variance-component SE / CI skipped for size: the note states the fact;
  # the advice (raise the cap or test the random terms) belongs here, to
  # the analyst, once per table.
  re_se_skipped <- vapply(frames, function(fr) {
    !is.na(fr$info$extras$re_se_skipped_n %||% NA_integer_)
  }, logical(1))
  if (any(re_se_skipped)) {
    labels <- if (!is.null(names(models)) && all(nzchar(names(models)))) {
      names(models)
    } else {
      paste("Model", seq_along(models))
    }
    spicy_warn(
      c(
        sprintf(
          paste0("Variance-component SEs and CIs skipped for %s: n exceeds ",
                 "`options(\"spicy.re_se_max_n\")` = %s."),
          paste(labels[re_se_skipped], collapse = ", "),
          format(.re_se_size_cap(), big.mark = ",")
        ),
        "i" = paste0(
          "Their computation (merDeriv) grows superlinearly with n ",
          "(roughly a minute at n \u2248 2,700). Raise the cap, e.g. ",
          "`options(spicy.re_se_max_n = Inf)`, to compute them anyway, ",
          "or test the random terms with `re_test = \"lrt\"`."
        )
      ),
      class = "spicy_caveat"
    )
  }

  # AME capability guard (finding M2): a class with no
  # average-marginal-effects backend (supports$ame = FALSE: mlogit, coxph,
  # flexsurv, selection, nls, Bayesian) must REFUSE the request rather
  # than render an entirely empty column. In mixed tables where at least
  # one model can produce AME, the capable models populate their columns
  # and the incapable ones em-dash -- so only the all-incapable case
  # errors.
  if (any(c("ame", "ame_se", "ame_ci", "ame_p") %in% show_columns)) {
    ame_ok <- vapply(frames, function(fr) {
      isTRUE(fr$info$supports$ame)
    }, logical(1))
    if (!any(ame_ok)) {
      classes <- unique(vapply(frames, function(fr) {
        fr$info$class %||% "?"
      }, character(1)))
      # Cox models have the ABSOLUTE estimand family instead: point
      # there rather than leaving the reader without a next step.
      hint_main <- if (any(classes == "coxph")) {
        paste0(
          "For a Cox model, the absolute-effect columns are the RMST ",
          "and risk differences: `show_columns = c(\"b\", \"rmst\")` ",
          "with `tau = `, or `\"risk_diff\"` with `at_time = `."
        )
      } else {
        paste0(
          "No average-marginal-effects backend exists for this class ",
          "(see ?table_regression_models, column AME)."
        )
      }
      spicy_abort(
        c(
          sprintf(
            "AME columns are not available for %s.",
            paste0("`", classes, "`", collapse = " / ")
          ),
          "i" = hint_main,
          "i" = "Drop the AME token(s) from `show_columns`."
        ),
        class = "spicy_invalid_input"
      )
    }
  }

  # `exponentiate = TRUE` no-op detection (relocated from before extraction):
  # now that every frame is built, warn only when NO model actually applied
  # exp() -- i.e. every link is identity or otherwise non-exponentiable. Keyed
  # on the frames' own info$extras$exp_applied, so it covers classes with no
  # stats::family() method (polr / clm / multinom / fixest / betareg / ...).
  if (isTRUE(exponentiate) &&
      !any(vapply(frames,
                  function(fr) isTRUE(fr$info$extras$exp_applied),
                  logical(1)))) {
    spicy_warn(
      c(
        "`exponentiate = TRUE` has no effect on identity-link fits.",
        "i" = paste0(
          "exp() is only applied to fits whose coefficients are on a log / ",
          "logit scale (odds, rate, risk, hazard, or time ratios). Drop the ",
          "argument for identity-link (gaussian) fits."
        )
      ),
      class = "spicy_ignored_arg"
    )
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

  # ---- Multinomial outcome-as-columns explode (display only) -------------
  # The publication layout for a single multinom model: one column
  # group per outcome category. The explode swaps in per-category
  # pseudo-model frames for ALIGN + RENDER (and the display-facing
  # footer themes); the title builder keeps reading the ORIGINAL
  # frames (one model), and so does the dispatch payload below --
  # tidy() and output = "long" keep the long frame with outcome_level
  # and prefixed terms. See R/regression_multinom_layout.R.
  frames_render    <- frames
  model_ids_render <- model_ids
  mn_spanners      <- NULL
  mn_exploded      <- FALSE
  if (mn_columns_active) {
    ex <- .explode_multinom_frame(frames[[1L]])
    if (!is.null(ex)) {
      frames_render    <- ex$frames
      model_ids_render <- ex$model_ids
      # `outcome_labels = FALSE` is documented as "suppress the
      # Outcome body row"; the columns layout has no such row, so
      # FALSE is a no-op here (not an override).
      if (isFALSE(outcome_labels)) outcome_labels <- NULL
      mn_spanners <- .multinom_columns_spanners(
        ex$model_ids,
        frames[[1L]]$info$extras$reference_outcome %||% NA_character_,
        outcome_labels
      )
      # `outcome_labels` is consumed as the spanner override in this
      # layout: the renderer's per-model Outcome row must not also
      # fire on the pseudo-models.
      outcome_labels <- NULL
      mn_exploded <- TRUE
    }
  }

  # ---- Multi-model alignment + wide pivot (Layer 2) ----------------------
  # align_frames() consumes the frames list directly and produces the
  # aligned long-format data structure consumed by the body builder.
  aligned <- align_frames(
    frames_render,
    model_ids = model_ids_render,
    show_intercept = show_intercept,
    intercept_position = intercept_position,
    reference_style = reference_style
  )

  # `keep` / `drop` filter -- runs AFTER alignment (so canonical term
  # ordering is preserved for the surviving terms) and AFTER
  # p_adjust (so adjusted p-values reflect the model's full
  # coefficient family, not just the displayed subset).
  aligned <- apply_keep_drop_filter(aligned, keep = keep, drop = drop)

  # Dispatch payload: under the columns layout, tidy() and
  # `output = "long"` must keep the original long frame (a
  # DISPLAY-only change), so the data-facing alignment is rebuilt
  # from the un-exploded frames. Row selection (keep / drop /
  # show_intercept) already ran on the DISPLAY alignment against
  # BARE terms; one namespace must govern both views, so each
  # prefixed row is mapped to its bare term and kept exactly when
  # that term survived. Re-running the user regexes against the
  # prefixed terms instead would silently select different rows
  # (`keep = "^age$"` matches "age" but not "Student: age"), and
  # align_frames()'s own intercept filter never matches
  # "Student: (Intercept)".
  aligned_data <- if (mn_exploded) {
    ad <- align_frames(
      frames,
      model_ids = model_ids,
      show_intercept = show_intercept,
      intercept_position = intercept_position,
      reference_style = reference_style
    )
    ca <- ad$coefs_aligned
    lvl <- ca$outcome_level
    bare <- ifelse(
      !is.na(lvl) & startsWith(ca$term, paste0(lvl, ": ")),
      substring(ca$term, nchar(lvl) + 3L),
      ca$term
    )
    ad$coefs_aligned <- ca[bare %in% unique(aligned$coefs_aligned$term), ,
                           drop = FALSE]
    ad
  } else {
    aligned
  }

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
  # `re_scale_val` / `re_columns_val` were validated fail-fast up top (finding
  # m3) and reused to materialise the RE rows; thread them through the footer.

  # Phase 7c22 (item f): pass the surviving parent_vars (post keep /
  # drop filter) so the polynomial-trends footer note suppresses when
  # the ordered factor isn't actually displayed in the table.
  # `aligned$coefs_aligned` has `factor_term` (parent name for factor
  # rows, NA for non-factor rows) instead of the frame's `parent_var`
  # column. Coalesce to recover the parent-var view of the display.
  ca <- aligned$coefs_aligned
  displayed_parent_vars <- unique(ifelse(is.na(ca$factor_term),
                                            ca$term, ca$factor_term))

  # Footer: model-facing themes read `frames` (one entry per model);
  # the label-reading themes (reference categories, polynomial
  # trends) read the DISPLAY frames -- under the columns layout the
  # original prefixed labels would print pseudo-levels like
  # "sex = Student: Female" once per equation.
  footer_main <- build_regression_footer_from_frames(
    frames,
    frames_display = frames_render,
    standardized = standardized,
    p_adjust = p_adjust,
    stars = stars,
    nested = nested,
    show_columns = show_columns,
    reference_style = reference_style,
    show_re = isTRUE(show_re),
    re_scale = re_scale_val,
    re_columns = re_columns_val,
    re_test = re_test_val,
    displayed_parent_vars = displayed_parent_vars
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
        any(vapply(frames_render, function(f) {
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
    if (length(footer_main)) footer_main else NULL   # nocov -- NULL arm unreachable: the type footer block always emits
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
  effective_model_labels <- if (mn_exploded) {
    # Columns layout: the "model" labels are the category spanners
    # (already outcome_labels-overridden); `model_labels` itself was
    # refused fail-fast up top.
    mn_spanners
  } else if (!is.null(model_labels)) {
    model_labels
  } else if (!is.null(names(models)) && any(nzchar(names(models)))) {
    nms <- names(models)
    missing_idx <- which(!nzchar(nms))
    nms[missing_idx] <- paste0("Model ", missing_idx)
    nms
  } else {
    NULL  # render_regression_table generates "Model 1", ... as fallback
  }
  # "95% CrI" (equal-tailed) or "95% HDI" header when EVERY frame is a
  # posterior interval (the design promise in regression_frame_stan.R;
  # the hdi gate guarantees posterior_hdi is all-Bayesian): mixed
  # frequentist + Bayesian tables keep the shared "CI" label, with a
  # per-model footer disclosure from the CI-method block.
  frame_ci_methods <- vapply(frames, function(f) {
    f$info$ci_method %||% ""
  }, character(1))
  ci_label_val <- if (length(frames) > 0L &&
                        all(frame_ci_methods == "posterior_quantile")) {
    "CrI"
  } else if (length(frames) > 0L &&
               all(frame_ci_methods == "posterior_hdi")) {
    "HDI"
  } else {
    "CI"
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
    ci_label = ci_label_val,
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
    note = full_footer_str,
    re_columns = re_columns_val
  )
  # Stash the print-time padding as an attribute so the print
  # method can honour the call-site choice (and tooling like
  # `as.data.frame()` / broom continues to see the raw rendered
  # cells unchanged).
  attr(rendered, "padding") <- as.integer(padding)
  attr(rendered, "fit_stats_layout") <- fit_stats_layout

  # ---- Output dispatch (Step 11) -----------------------------------------
  # `aligned_data` (not `aligned`): under the multinomial columns
  # layout the display alignment is the exploded pseudo-model one,
  # while tidy() / output = "long" read the original long frame
  # (as_structured() mirrors the display, via the rendered table).
  dispatch_regression_output(
    rendered = rendered,
    aligned = aligned_data,
    output = output,
    excel_path = excel_path,
    excel_sheet = excel_sheet,
    clipboard_delim = clipboard_delim,
    word_path = word_path,
    word_template = word_template
  )
}
