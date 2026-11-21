#' Column and fit-statistic tokens in table_regression()
#'
#' @description
#' Two vector arguments -- `show_columns` and `show_fit_stats` --
#' accept named tokens that select **what** to display and in
#' **what order**. All tokens are lowercase
#' (snake_case for compound tokens). Group tokens
#' (`"all_b"`, `"all_ame"`, ...) expand to a fixed vector of
#' atomic tokens; see `show_columns` below.
#'
#' @details
#' # `show_columns` -- per-coefficient columns
#'
#' \if{latex}{\out{\raggedright{}}}Each token = one displayed column.
#'
#' \itemize{
#'   \item Coefficient family: `"b"`, `"beta"` (standardized),
#'     `"se"`, `"ci"`, `"t"`, `"p"`.
#'   \item Degrees of freedom: `"df"`, those of each coefficient's t
#'     test, the `df` of `tidy()`. They can differ from row to row: the
#'     Satterthwaite df of a cluster-robust `vcov = "CR2"` or of an
#'     `lmerTest` fit, the containment df of an `lme` fit. A whole df
#'     prints without decimals, another with one (`1.9`). A z test leaves
#'     its cell blank, and so does a model of z tests beside one of t
#'     tests; the token is refused when no model of the table has a t
#'     test. The rows of a random effect or of a smooth term, which have
#'     no t, show a dash, as in the `"t"` column. The header reads df,
#'     ddl in French. Opt-in: no group token holds it.
#'   \item Marginal effects: `"ame"`, `"ame_se"`, `"ame_ci"`,
#'     `"ame_p"`, `"ame_df"`. `"p"` always refers to the B-coefficient
#'     p-value; for the AME-specific p-value use `"ame_p"`. `"ame_df"`
#'     gives the degrees of freedom of each AME's t test (the
#'     Satterthwaite df under `vcov = "CR2"`), under the header of
#'     `"df"` and with its rules: blank under a z test, refused when no
#'     AME of the table has a t test.
#'   \item Partial effect sizes -- `lm` only: `"partial_f2"`,
#'     `"partial_eta2"`, `"partial_omega2"`, each with a paired
#'     `_ci` companion (`"partial_f2_ci"`, `"partial_eta2_ci"`,
#'     `"partial_omega2_ci"`).
#'   \item Term-level test -- `lm`, `glm` and mixed-effects models:
#'     `"partial_chi2"` and its twin `"partial_f"`, the term-level
#'     test. By default (`partial_type = "II"`) each term is tested
#'     under the marginality-respecting hypothesis that excludes any
#'     higher-order interaction containing it (the
#'     `car::Anova(type = 2)` convention), so the value does not
#'     depend on the factor coding. `partial_type = "III"` keeps the
#'     interactions and tests the term in a sum-to-zero coding (see
#'     `partial_type` in [table_regression()]). For `glm` the term-level
#'     test is
#'     the F that `anova(reduced, full, test = "F")` reports when the
#'     dispersion is estimated, the likelihood-ratio chi-square
#'     otherwise: `"partial_f"`,
#'     \eqn{F = (\Delta D / q) / \hat\phi}{F = (Delta D / q) / phi}
#'     on \eqn{(q, df)} degrees of freedom, \eqn{q} the term's and
#'     \eqn{df} the residual degrees of freedom of the model, with
#'     \eqn{\hat\phi}{phi} the dispersion `summary()` estimates, for
#'     gaussian, Gamma, inverse Gaussian, Tweedie,
#'     `MASS::negative.binomial(theta)` fitted by `glm()` and the quasi
#'     families (the value `car::Anova(type = 2, test.statistic = "F",
#'     error.estimate = "dispersion")` reports; car's default Pearson
#'     estimate differs by the IRLS convergence error only, and
#'     `drop1(test = "F")` divides by the deviance dispersion instead);
#'     `"partial_chi2"`, the likelihood-ratio chi-square (Long & Freese
#'     2014 Sections 3.2.2, 3.2.4), for binomial, Poisson and
#'     `MASS::glm.nb`. For `lm`, `"partial_f"` is the F of
#'     `car::Anova(type = 2)` (`type = 3` under `partial_type = "III"`),
#'     the F the variance-explained partials are built on, so a gaussian `glm` and its `lm()` twin print the same
#'     value. A token is refused when no model of the table carries it,
#'     and when a model carries the other one while that one is not
#'     requested (each test has its own column: ask for both). Both come
#'     from the refits and the model's own dispersion (the residual
#'     variance of an `lm`) whatever `vcov`.
#'     For the mixed models, `"partial_chi2"` is the Wald chi-square
#'     of the same hypothesis; see [table_regression_mixed]. Rendered as `value (df)` to
#'     disambiguate factor terms (k-1 df) from numeric terms (1 df),
#'     and the partial F as `F (q, df)` with both of its degrees of
#'     freedom, `3.45 (2, 28)`, as Stata and SAS report them: each cell
#'     carries the residual df of its own model, and the note defines
#'     the statistic only. The console and the rich outputs (tinytable,
#'     gt, flextable, Word, the clipboard) print the test in one column
#'     under the statistic's header, and Excel writes the statistic as a
#'     number with its df in the number format (`0.00" (2, 28)"`).
#'     `output = "data.frame"` and [as_structured()] keep the statistic
#'     and its df as numbers, in the statistic's column, then
#'     `"<column>: df"` and, for the F, `"<column>: residual df"`.
#'     `tidy()` has them in `df` and `df.residual`, and `inline()` cites
#'     the statistic alone. The test and the partial effect sizes belong
#'     to the whole term: under the header row of a factor (the grouped
#'     `factor_layout`) they are shown once, on that row, and the level
#'     rows leave the cell empty. In the flat layout each level row
#'     carries them. `tidy()` and the long output keep them
#'     on every level row, and `inline()` reads them by the variable, or
#'     by the variable and any of its levels.
#'   \item Variance explained per fit -- `lm` only: `"r2"`,
#'     `"adj_r2"`. Like `"n"`, these are *per-fit* columns: they are
#'     populated by [table_regression_uv()] screens, where each
#'     predictor block is its own model (its value on the predictor's
#'     row, or on the header row of a factor) and the \eqn{R^2}{R^2}
#'     answers "how much of the outcome does this predictor explain
#'     on its own". A model whose \eqn{R^2}{R^2} is a single
#'     model-level number drops the column and reports it in the
#'     fit-statistics rows instead (the `"r2"` / `"adj_r2"` tokens of
#'     `show_fit_stats`, on by default for `lm`). Refused for classes
#'     with no least-squares variance partition (`glm`, `coxph`,
#'     ...), where only competing pseudo-\eqn{R^2}{R^2} measures
#'     exist: name the one you want in `show_fit_stats`.
#'   \item Sample columns: `"n"` -- per-row N, populated by
#'     [table_regression_uv()] screens (each predictor block is its
#'     own fit); models without per-row N data drop the column.
#'     `"n_events"` -- outcome event counts as `events/N`, per
#'     factor level (reference row included) and model totals on
#'     continuous rows, computed on each model's own estimation
#'     sample (STROBE item 16's "data behind the measures of association";
#'     NEJM-style `no. of events/total no.`). Binary (binomial)
#'     outcomes and right-censored `coxph` fits -- other models
#'     raise `spicy_invalid_input`.
#'   \item Survival estimands -- `coxph` and `survreg`: `"rmst"`,
#'     `"rmst_se"`, `"rmst_ci"`, `"rmst_p"` (restricted-mean-
#'     survival-time difference over `[0, tau]`) and
#'     `"risk_diff"`, `"risk_diff_se"`, `"risk_diff_ci"`,
#'     `"risk_diff_p"` (cumulative-incidence difference at
#'     `at_time`), by g-computation from the fitted model with
#'     bootstrap inference. The horizon arguments are required;
#'     see `tau` / `at_time` in [table_regression()] and
#'     [table_regression_survival].
#'   \item Bayesian fits: `"pd"` (probability of direction) and
#'     `"rhat"`, `"ess_bulk"`, `"ess_tail"`, `"mcse"` (per-coefficient
#'     sampler diagnostics and the Monte Carlo standard error of the
#'     displayed posterior median), all-Bayesian tables only; see
#'     [table_regression_bayesian].
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
#' `"all_b"` for a single model (the layout of the APA Manual 7
#' sample regression tables, Tables 7.16 and 7.17, pp. 219-220),
#' `"all_b_compact"` for two or more models (CI dropped to fit the
#' side-by-side layout; restore it explicitly when needed).
#'
#' **Fit statistics alone** (`show_columns = "none"`): no coefficient
#' row or column. Each model has one column, headed by its label (the
#' outcome, "Model 1", a name of the list or `model_labels`, as for the
#' column groups of the full table; "Model 1" for a single model), and
#' the rows are the fit statistics of `show_fit_stats`, in its order,
#' the change rows of `nested = TRUE` included. The title is the one of
#' the full table; the note keeps what describes the models and their
#' fit, and leaves out what describes coefficients. Models with
#' different outcomes get an Outcome row when the column headers do not
#' name them (`outcome_labels = FALSE` removes it). `"none"` goes alone, and
#' `show_fit_stats = FALSE` is refused with it, as is `layout =
#' "stacked"`; an empty `show_columns` (`character(0)`, `FALSE`) is an
#' error that points to `"none"`. The arguments that act on coefficients
#' only are ignored, with one `spicy_ignored_arg` warning naming those
#' set to other than their default: `ci_level`, `ci_method`, `tau`,
#' `at_time`, `standardized`, `exponentiate`, `ame_type`, `p_adjust`,
#' `keep`, `drop`, `show_intercept`, `show_thresholds`,
#' `show_components`, `intercept_position`, `factor_layout`,
#' `reference_style`, `reference_label`, `fit_stats_layout`, `show_re`,
#' `re_scale`, `re_columns`, `re_test`, `re_ci`, `stars`,
#' `effect_size_digits`, and `vcov`, `cluster` and `boot_n` unless
#' `nested = TRUE` (where a robust `vcov` gives the Wald change rows).
#' An information criterion (AIC, BIC, deviance, ...) shown for models
#' fitted to different numbers of observations or to different outcomes
#' raises a `spicy_caveat` warning: its differences compare nothing.
#' `tidy()` returns no row, `glance()` one row per model, and
#' `inline()` cites a statistic of a model: `inline(tbl, "aic", model =
#' 2)`. [table_regression_uv()] refuses the token.
#'
#' # `show_fit_stats` -- model-level rows below the coefficients
#'
#' \if{latex}{\out{\raggedright{}}}\itemize{
#'   \item Counts: `"nobs"`, `"weighted_nobs"`, `"n_events"` (Cox
#'     models: number of events, reported alongside `n` per the
#'     field convention; blank for other classes). `"nobs"` and the
#'     BIC follow the engine's [stats::nobs()], which for a weighted
#'     `polr` or `clm` fit is the sum of the weights.
#'   \item Variance explained: `"r2"` and `"adj_r2"` (least-squares fits:
#'     linear, robust, instrumental-variable and `feols`; `"r2"` alone
#'     for `rms::ols`, `"adj_r2"` alone for GAMs), `"omega2"` (`lm` only).
#'   \item \eqn{R^2}{R^2} outside least squares (`glm`, `polr`, `clm`,
#'     `multinom` and `rms::lrm` fits): `"pseudo_r2_tjur"` (Tjur 2009;
#'     binomial models only), `"pseudo_r2_mcfadden"` (McFadden 1974),
#'     `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) and `"r2_deviance"`
#'     (Cameron & Windmeijer 1997; `glm` families other than the
#'     binomial). What each one computes, and which a family shows by
#'     default, is in *R-squared of glm and categorical models* in
#'     [table_regression()].
#'   \item Pseudo-\eqn{R^2}{R^2} of a beta regression
#'     (`betareg::betareg` only, on request): `"pseudo_r2_ferrari"`, the
#'     squared correlation between the linear predictor of the mean and
#'     the response on the scale of the mean link (Ferrari &
#'     Cribari-Neto 2004), the value `summary()` reports.
#'   \item Residual scale: `"sigma"`, `"rmse"`. `"sigma"` is the lm
#'     \eqn{\hat{\sigma}}{sigma-hat}. For glm and the GLM-like `fixest`
#'     fits, it is the square root of the dispersion (so a gaussian glm
#'     reads as lm), and 1 when the family fixes it (binomial, Poisson).
#'     For `survreg`, it is the scale \eqn{\sigma}{sigma} of
#'     \eqn{\log T = X'\beta + \sigma W}{log T = X'beta + sigma W} for the
#'     distributions fitted on \eqn{\log T}{log T} (the standard deviation
#'     of \eqn{\log T}{log T} for the log-normal), and of
#'     \eqn{T = X'\beta + \sigma W}{T = X'beta + sigma W} for those that
#'     model \eqn{T}{T} itself (gaussian, logistic, extreme value, t),
#'     blank when strata give one scale per stratum. For `glmmTMB`, it is
#'     the same square root for the gaussian, Gamma and Tweedie families,
#'     and the fixed 1 of Poisson and binomial. It is blank for the
#'     negative-binomial and beta fits below, whose dispersion has its
#'     own row, and for the other `glmmTMB` families, whose `sigma()` is
#'     a family-specific parameter. For `mgcv::gam` and `bam`, a glm
#'     family follows the glm rule; an extended family shows the square
#'     root of its scale only when the GAM estimates one (`tw()`), and is
#'     blank otherwise (an estimated `nb()`, `betar()`, `scat()`, `ziP()`,
#'     `ocat()`):
#'     mgcv fixes their scale at 1.
#'   \item Survey designs (`survey::svyglm` only): `"eff_p"`, the
#'     effective number of parameters of the design (the first element
#'     of survey's `extractAIC`, the sum of the Rao-Scott eigenvalues).
#'     Opt-in and distinct from `"aic"`, which reports the design-based
#'     AIC of Lumley & Scott (2015). Blank for other classes.
#'   \item Bayesian fits only: `"r2_bayes"` (posterior-median
#'     Bayesian \eqn{R^2}{R^2}, Gelman et al. 2019 -- in the
#'     all-Bayesian default), `"elpd_loo"` and `"looic"` (PSIS-LOO
#'     expected log predictive density and its deviance-scale twin,
#'     Vehtari et al. 2017; opt-in, a few seconds per model; the
#'     footer discloses the elpd standard error), and `"waic"`
#'     (Watanabe-Akaike; PSIS-LOO is generally preferred). See
#'     [table_regression_bayesian].
#'   \item Negative-binomial dispersion. NB2 fits (`MASS::glm.nb`,
#'     `lme4::glmer.nb`, `fixest::fenegbin`, `mgcv::gam` with `nb()`,
#'     `pscl::zeroinfl` and `pscl::hurdle` with `dist = "negbin"`, whose
#'     theta is that of the count part, and `glmmTMB` with `nbinom2` or
#'     `truncated_nbinom2`): `"theta"`
#'     (\eqn{V = \mu + \mu^2/\theta}{V = mu + mu^2/theta}) and `"alpha"`
#'     (\eqn{= 1/\theta}{= 1/theta}, the Stata `nbreg` convention).
#'     NB1 fits (`glmmTMB` with `nbinom1` or `truncated_nbinom1`):
#'     `"delta"` (\eqn{V = \mu(1 + \delta)}{V = mu(1 + delta)}, the
#'     \eqn{\delta}{delta} of Stata's `nbreg, dispersion(constant)`).
#'   \item Beta precision: `"phi"`. In beta regression
#'     (`betareg::betareg`, `mgcv::gam` with `betar()`, and `glmmTMB`
#'     with `beta_family()`), \eqn{Var(y) = \mu(1-\mu)/(1+\phi)}{Var(y) =
#'     mu(1-mu)/(1+phi)} (Ferrari & Cribari-Neto 2004; higher
#'     \eqn{\phi}{phi} = less dispersion). For `glmmTMB` with
#'     `betabinomial()` or `ordbeta()`, \eqn{\phi}{phi} is the precision
#'     parameter of the beta part. A constant precision is
#'     back-transformed from its link, so `y ~ x | 1` reports the
#'     same \eqn{\phi}{phi} as `y ~ x`.
#'   \item For these dispersion rows (`"theta"`, `"alpha"`, `"delta"`,
#'     `"phi"`), a column whose model lacks the parameter is blank, so a
#'     Poisson model can stand beside an NB2 model, or NB1 beside NB2.
#'     A parameter fixed in the call is not an estimate and has no row:
#'     `MASS::negative.binomial(theta)` in `glm()`, mgcv's
#'     `negbin(theta)`, and `nb(theta)` or `betar(theta)` with
#'     `theta > 0`. Such a negative-binomial model keeps the glm rule for
#'     `"sigma"`. A negative-binomial `glmer()` always shows its theta,
#'     which lme4 counts in the degrees of freedom of the model: a
#'     `glmer.nb()` fit, its `update()` refits, and a `glmer()` with
#'     `negative.binomial(theta)`.
#'     For `mgcv::gam` and `bam`, theta and phi are estimated with the
#'     smoothing parameters (REML by default), so they differ from the
#'     maximum-likelihood values of `glm.nb()`, betareg or `glmmTMB`.
#'     `method = "ML"` gives those.
#'     A token is refused only when no model in the table has the
#'     parameter, or when a model of that family has dispersion
#'     covariates (a `glmmTMB` `dispformula`, or a betareg `y ~ x | z`),
#'     so the parameter is not a single number. Those covariates are
#'     shown as a block of rows instead (`show_components`): `Precision`
#'     for a betareg `y ~ x | z` and a `beta_family()` `dispformula`,
#'     `Dispersion` for the other `glmmTMB` families.
#'   \item GEE (`geepack::geeglm` only): `"qic"` and `"qicu"` -- the
#'     quasi-likelihood information criteria (Pan 2001; QIC for
#'     working-correlation choice, QICu for covariate choice) via
#'     [geepack::QIC()], computed only when requested (QIC silently
#'     refits the independence working model); `"scale"` -- the
#'     estimated scale (dispersion) parameter, shown only when the
#'     fit estimated it (blank under `scale.fix = TRUE`, matching
#'     geepack's own summary); `"max_cluster_size"` -- the largest
#'     cluster in the estimation sample (on by default with
#'     `"n_groups"`, which renders the `N (<id>)` cluster count).
#'     Cluster statistics count the clusters geepack itself used
#'     (`geese$clusz`, consecutive runs of `id =` values), so they
#'     always describe the displayed sandwich inference.
#'     Refused when no model in the table is a geeglm fit; the
#'     likelihood-based tokens (`"aic"`, pseudo-\eqn{R^2}{R^2}, ...)
#'     are refused in return for all-GEE tables -- quasi-likelihood
#'     has no likelihood.
#'   \item Absorbed fixed effects (`fixest` fits, and `estimatr` fits
#'     built with `fixed_effects =`): `"fixed_effects"` renders a
#'     `Fixed effects:` block at the top of the fit statistics --
#'     one Yes / No row per absorbed factor (the `etable` / `esttab`
#'     convention), blank cells for models without the concept in
#'     mixed tables -- and is on by default for both.
#'     Varying-slope-only factors (`Origin[[x]]`) absorb no
#'     intercept: they read No when another model absorbs that
#'     factor, and contribute no row otherwise.
#'     `"within_r2"` is the FE-partialed within
#'     R-squared (a default for `feols`, except an IV one; opt-in
#'     for `estimatr`;
#'     GLM-family fixest fits report fixest's McFadden `pr2`
#'     instead). Both tokens are refused when no model in the table
#'     absorbs fixed effects.
#'   \item Effect size: `"f2"`.
#'   \item Information criteria: `"aic"`, `"aicc"`, `"bic"`,
#'     `"deviance"` (lowercase like every other token; the rendered
#'     row labels stay `AIC` / `AICc` / `BIC`). The AIC and BIC of a
#'     `fixest` model are those of its `lm()`, `glm()` or
#'     `MASS::glm.nb()` twin with the fixed effects as dummies: `glm()`'s
#'     log-likelihood for a Gaussian, Gamma or inverse-Gaussian `feglm()`,
#'     with the residual variance, dispersion or theta counted, which
#'     fixest's `AIC()` leaves out. A Gaussian `femlm()` follows the same
#'     rule; Poisson, binomial and IV fits keep fixest's values. A lean
#'     fit (`lean = TRUE`) that lost what they need, its weights or the
#'     fitted values of a Gamma or inverse-Gaussian `feglm()`, shows no
#'     AIC, BIC or nested rows. A survey-design fit has no likelihood:
#'     `"deviance"`, `"bic"`, `"aicc"`, the pseudo-R-squared tokens and
#'     `"r2_deviance"` are refused for a table of design fits, and `"aic"`
#'     when no model of the table is a `svyglm`.
#'   \item Log-likelihood: `"log_lik"`, the value of [stats::logLik()],
#'     from which the AIC row is computed. It is the restricted
#'     log-likelihood of a model fitted by REML, and for a binomial `glm()`
#'     it is `logLik()`'s value whatever the layout of the data: it includes
#'     the binomial coefficients of a grouped response and rounds
#'     non-integer weights, unlike the pseudo-\eqn{R^2}{R^2} rows. A column
#'     whose model has no likelihood (a quasi family, a survey design, GEE,
#'     two-stage least squares) shows a dash; the token is refused when no
#'     model of the table has one, and for Bayesian fits.
#'   \item Cox models (`survival::coxph` and `survey::svycoxph`):
#'     `"concordance"`, Harrell's C, the value `summary()` reports. When
#'     the row is shown, the concordance leaves the note, which otherwise
#'     gives it with its standard error. `rms::cph` fits have no
#'     concordance row or note.
#'   \item Change-stats for hierarchical comparison
#'     (active under `nested = TRUE`; see *Hierarchical
#'     (nested) model comparison* in [table_regression()]):
#'     `"r2_change"`, `"adj_r2_change"`,
#'     `"f_change"`, `"f2_change"`, `"lrt_change"`,
#'     `"aic_change"`, `"aicc_change"`, `"bic_change"`,
#'     `"deviance_change"`, `"p_change"`. The differences, current
#'     model minus previous (`"r2_change"`, `"adj_r2_change"`,
#'     `"f2_change"`, and the AIC, AICc and BIC changes), print with an
#'     explicit sign (`+0.07`, `-9.4`) in every output. The test
#'     statistics and `"p_change"` are never signed. The test statistics
#'     carry their degrees of freedom, as the partial F cell does:
#'     `12.38 (1, 29)` for an F, the number of added parameters and the
#'     residual df of the larger model (the denominator df for the Wald F
#'     of a robust `vcov`), `9.12 (1)` for a chi-square. A df that is not
#'     a whole number (the HTZ denominator under `"CR2"`, the df of a
#'     smooth term) prints with one decimal, `174.77 (2, 0.8)`.
#'     The rich outputs print the same cell, `inline()` cites the
#'     statistic alone, `output = "data.frame"` keeps it as a number,
#'     and Excel writes the number with the df in its number format.
#'     `"deviance_change"`
#'     is the drop in deviance (previous minus current), positive when
#'     the larger model fits better, and is not signed either.
#' }
#'
#' Default (resolved when `NULL`) is class-aware: lm and
#' `lm_robust` fits get
#' `c("nobs", "r2", "adj_r2")`; glm, `mgcv::gam`, `polr`, `clm`,
#' `multinom`, `rms::lrm` and `rms::Glm` fits get `"nobs"`, the \eqn{R^2}{R^2} rows of their family
#' (see *R-squared of glm and categorical models*) and `"aic"`;
#' mixed lm + glm sets union both groups (the renderer per-row
#' en-dashes the inappropriate cell); `coxph` and `cph` fits get
#' `c("nobs", "n_events", "aic")`, and a `svycoxph` fit gets
#' `c("nobs", "weighted_nobs", "n_events")`, since a design-based fit has
#' no AIC; GEE fits get
#' `c("nobs", "n_groups", "max_cluster_size")` (the cluster
#' structure, Stata `xtgee`-header style). When `nested = TRUE`, the
#' class-aware default is extended with change tokens
#' (`c("r2_change", "f_change", "p_change")` for lm,
#' `c("lrt_change", "p_change")` for glm, `c("f_change", "p_change")`
#' for a glm whose dispersion is estimated). The order of tokens in
#' `show_fit_stats` controls the order of the rows.
#'
#' # Fit statistics by model class
#'
#' For each class of [table_regression_models()], the rows a one-model
#' table shows under the default `show_fit_stats` (Default), in their
#' order, then every token that gives a row when named (Available). A
#' parenthesis after a token gives the fits it is limited to. The change
#' tokens need
#' `nested = TRUE` and are not listed. The same lists are the columns
#' `fit_stats_default` and `fit_stats` of [table_regression_models()].
#'
#' `r spicy:::.render_fit_stats_by_class_md()`
#'
#' @references
#' \if{latex}{\out{\raggedright{}}}American Psychological Association (2020). *Publication manual of the
#' American Psychological Association* (7th ed.). American Psychological
#' Association. \doi{10.1037/0000165-000}
#'
#' Cameron, A.C. & Windmeijer, F.A.G. (1997). An R-squared measure of
#' goodness of fit for some common nonlinear regression models.
#' *Journal of Econometrics*, 77(2), 329-342.
#' \doi{10.1016/S0304-4076(96)01818-0}
#'
#' Ferrari, S. & Cribari-Neto, F. (2004). Beta regression for modelling
#' rates and proportions. *Journal of Applied Statistics*, 31(7),
#' 799-815. \doi{10.1080/0266476042000214501}
#'
#' Gelman, A., Goodrich, B., Gabry, J. & Vehtari, A. (2019). R-squared
#' for Bayesian regression models. *The American Statistician*, 73(3),
#' 307-309.
#'
#' Long, J.S. & Freese, J. (2014). *Regression models for categorical
#' dependent variables using Stata* (3rd ed.). Stata Press.
#'
#' Lumley, T. & Scott, A. (2015). AIC and BIC for modeling with complex
#' survey data. *Journal of Survey Statistics and Methodology*, 3(1),
#' 1-18. \doi{10.1093/jssam/smu021}
#'
#' McFadden, D. (1974). Conditional logit analysis of qualitative
#' choice behavior. In P. Zarembka (Ed.), *Frontiers in econometrics*
#' (pp. 105-142). Academic Press.
#'
#' Nagelkerke, N.J.D. (1991). A note on a general definition of the
#' coefficient of determination. *Biometrika*, 78(3), 691-692.
#' \doi{10.1093/biomet/78.3.691}
#'
#' Pan, W. (2001). Akaike's information criterion in generalized
#' estimating equations. *Biometrics*, 57(1), 120-125.
#' \doi{10.1111/j.0006-341X.2001.00120.x}
#'
#' Tjur, T. (2009). Coefficients of determination in logistic
#' regression models -- A new proposal: The coefficient of
#' discrimination. *The American Statistician*, 63(4), 366-372.
#' \doi{10.1198/tast.2009.08210}
#'
#' Vandenbroucke, J.P., von Elm, E., Altman, D.G., Gøtzsche, P.C.,
#' Mulrow, C.D., Pocock, S.J., Poole, C., Schlesselman, J.J. & Egger, M.
#' (2007). Strengthening the reporting of observational studies in
#' epidemiology (STROBE): Explanation and elaboration. *PLoS Medicine*,
#' 4(10), e297. \doi{10.1371/journal.pmed.0040297}
#'
#' Vehtari, A., Gelman, A. & Gabry, J. (2017). Practical Bayesian model
#' evaluation using leave-one-out cross-validation and WAIC. *Statistics
#' and Computing*, 27(5), 1413-1432. \doi{10.1007/s11222-016-9696-4}
#'
#' @seealso \if{latex}{\out{\raggedright{}}}[table_regression()] for the
#'   arguments;
#'   [table_regression_models()] for the registry of supported classes;
#'   the pages of the model families listed under *Pages by model family*
#'   in [table_regression()].
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' print(table_regression(
#'   fit,
#'   show_columns = c("all_b", "ame"),
#'   show_fit_stats = c("nobs", "r2", "sigma", "aic")
#' ))
#'
#' @name table_regression_tokens
NULL
