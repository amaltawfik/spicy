# ---------------------------------------------------------------------------
# Supported-models registry for table_regression().
#
# ONE machine-readable source of truth (the registry data frame below) feeds:
#   * the exported accessor table_regression_models() (insight-style
#     supported_models() pattern -- the published list can never drift from
#     the code, unlike a hand-maintained Rd table);
#   * the capability table on its documentation page (rendered from the
#     registry at roxygen time via inline R);
#   * the pkgdown "Supported models" article.
#
# The documentation page doubles as the CONCEPT page for per-family
# behaviour: short sections per model family, reachable under the aliases
# table_regression_mixed / _ordinal / ... so `?table_regression_mixed` keeps
# working in the console while the reference index carries a single, genuine
# function entry.
# ---------------------------------------------------------------------------


# The registry. One row per supported engine call. Kept internal; exposed
# through table_regression_models().
.supported_models_registry <- function() {
  df <- rbind(
    c("Linear and generalized linear", "lm",       "stats::lm()",              "yes", "-",                    "-"),
    c("Linear and generalized linear", "glm",      "stats::glm()",             "yes", "OR / IRR / RR (link)", "-"),
    c("Linear and generalized linear", "negbin",   "MASS::glm.nb()",           "yes", "IRR",                  "-"),
    c("Linear and generalized linear", "rlm",      "MASS::rlm()",              "yes", "-",                    "-"),
    c("Linear and generalized linear", "nls",      "stats::nls()",             "no",  "-",                    "-"),
    c("Robust, IV, quantile, panel",   "lm_robust","estimatr::lm_robust()",    "yes", "-",                    "-"),
    c("Robust, IV, quantile, panel",   "iv_robust","estimatr::iv_robust()",    "yes", "-",                    "-"),
    c("Robust, IV, quantile, panel",   "ivreg",    "AER::ivreg()",             "yes", "-",                    "-"),
    c("Robust, IV, quantile, panel",   "tobit",    "AER::tobit()",             "yes", "-",                    "-"),
    c("Robust, IV, quantile, panel",   "rq",       "quantreg::rq()",           "yes", "-",                    "-"),
    c("Robust, IV, quantile, panel",   "fixest",   "fixest::feols(), fixest::feglm(), fixest::fepois(), fixest::fenegbin()", "yes", "`feglm`: OR / IRR", "-"),
    c("Mixed effects",                 "lmerMod",  "lme4::lmer()",             "yes", "-",                    "Random effects"),
    c("Mixed effects",                 "glmerMod", "lme4::glmer()",            "yes", "OR / IRR (link)",      "Random effects"),
    c("Mixed effects",                 "glmmTMB",  "glmmTMB::glmmTMB()",       "yes", "link-dependent (IRR for count families)", "Random effects; Zero-inflation; Dispersion"),
    c("Mixed effects",                 "lme",      "nlme::lme()",              "yes", "-",                    "Random effects"),
    c("Mixed effects",                 "gls",      "nlme::gls()",              "yes", "-",                    "-"),
    c("Ordinal",                       "polr",     "MASS::polr()",             "per category", "OR (logit)", "Thresholds"),
    c("Ordinal",                       "clm",      "ordinal::clm()",           "per category", "OR (logit)", "Thresholds; Non-proportional effects"),
    c("Categorical",                   "multinom", "nnet::multinom()",         "per outcome",  "OR",         "per-outcome blocks"),
    c("Categorical",                   "mlogit",   "mlogit::mlogit()",         "no",  "OR",                  "per-alternative rows"),
    c("Counts, two-part",              "zeroinfl", "pscl::zeroinfl()",         "yes (combined response)", "IRR (count) + OR (logit zero part)", "Zero-inflation"),
    c("Counts, two-part",              "hurdle",   "pscl::hurdle()",           "yes (combined response)", "IRR (count) + OR (logit zero part)", "Zero hurdle"),
    c("Survival",                      "coxph",    "survival::coxph()",        "RMST / risk diff", "HR",   "-"),
    c("Survival",                      "survreg",  "survival::survreg()",      "yes + RMST / risk diff", "TR (log-scale distributions)","-"),
    c("Survival",                      "cph",      "rms::cph()",               "no",  "HR",                  "-"),
    c("Survival",                      "flexsurvreg", "flexsurv::flexsurvreg()", "no",  "TR / HR (dist)",    "distribution parameters"),
    c("Survey-weighted",               "svyglm",   "survey::svyglm()",         "yes (design-based)", "OR / IRR", "-"),
    c("Additive, proportions, selection", "gam",   "mgcv::gam(), mgcv::bam()",        "yes", "OR / IRR (link)",     "-"),
    c("Additive, proportions, selection", "betareg", "betareg::betareg()",     "yes", "OR (mean link)",      "-"),
    c("Additive, proportions, selection", "selection", "sampleSelection::selection()", "no", "-",           "selection component"),
    c("rms",                           "ols",      "rms::ols()",               "yes", "-",                    "-"),
    c("rms",                           "lrm",      "rms::lrm()",               "yes", "OR",                   "-"),
    c("rms",                           "Glm",      "rms::Glm()",               "yes", "link-dependent",       "-"),
    c("Bayesian",                      "stanreg",  "rstanarm::stan_glm(), rstanarm::stan_glmer()", "yes (draws)",  "link-dependent", "Random effects (if multilevel)"),
    c("Bayesian",                      "brmsfit",  "brms::brm()",              "yes (draws)",  "link-dependent",       "Random effects (if multilevel)")
  )
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  names(out) <- c("family", "class", "engine", "ame", "exponentiate",
                  "blocks")
  out
}


# Render the registry as a markdown pipe table for the roxygen page below
# (evaluated at document() time -- the published table can never drift from
# the registry).
.render_supported_models_md <- function() {
  reg <- .supported_models_registry()
  # Class and engine are code: backtick them (correct typography, and the
  # spell checker skips code spans). Engines are backticked ONE FUNCTION PER
  # SPAN: downlit auto-links an inline code span only when the whole span is
  # a single `pkg::fun()` call, so each function becomes clickable on the
  # pkgdown site.
  reg$class  <- paste0("`", reg$class, "`")
  reg$engine <- vapply(strsplit(reg$engine, ", ", fixed = TRUE),
                       function(fns) {
                         paste0("`", fns, "`", collapse = ", ")
                       }, character(1))
  header <- "| Family | Class | Engine | AME | Exponentiate | Blocks |"
  sep    <- "|---|---|---|---|---|---|"
  rows <- vapply(seq_len(nrow(reg)), function(i) {
    paste0("| ", paste(unlist(reg[i, ]), collapse = " | "), " |")
  }, character(1))
  paste(c(header, sep, rows), collapse = "\n")
}


#' Supported models and per-family behaviour of table_regression()
#'
#' @description
#' `table_regression_models()` returns the registry of model classes
#' supported by [table_regression()], one row per engine, with each class's
#' family, average-marginal-effects estimand, `exponentiate` semantics, and
#' labelled table blocks. The same registry drives this page's table, so the
#' published list cannot drift from the code.
#'
#' This page is also the reference for **per-family behaviour** (the
#' sections below). It is reachable as `?table_regression_models`,
#' `?table_regression_mixed`, `?table_regression_ordinal`,
#' `?table_regression_counts`, `?table_regression_categorical`,
#' `?table_regression_survival`, `?table_regression_robust`, or
#' `?table_regression_bayesian`.
#'
#' If a class is not listed: fit the model and call `table_regression(fit)`
#' anyway -- unsupported classes error with a clear message naming the
#' supported set. Feature requests are welcome on the issue tracker.
#'
#' @section Supported classes:
#'
#' `r spicy:::.render_supported_models_md()`
#'
#' @section Shared semantics (all classes):
#' \itemize{
#'   \item A robust `vcov` request is honoured through the class's
#'     field-standard backend, or **refused with a clear error** naming the
#'     supported set; the footer always names the estimator actually
#'     applied.
#'   \item `exponentiate = TRUE` is link-gated: it produces a labelled ratio
#'     (OR / IRR / HR / RR / MR / TR) only where the link warrants
#'     one. Identity-link fits warn and are left untouched; non-ratio
#'     links (probit, cauchit, inverse, ...) are **refused with a clear
#'     error**.
#'   \item Class-specific structure renders as labelled subordinate blocks
#'     of rows in the same table, each explained by a footer line.
#'   \item Fit statistics default to the family's field standard
#'     (`show_fit_stats` overrides; class-inappropriate tokens are rejected
#'     with a pointer to the right ones).
#'   \item Everything is available programmatically: `broom::tidy()`,
#'     `glance()`, [as_structured()], `as.data.frame()`.
#' }
#'
#' @section Mixed effects:
#' Fixed effects: Satterthwaite t (`lmer` + \pkg{lmerTest}), Wald z
#' (`glmer`, `glmmTMB`), containment-df t (`lme`). Random effects render as
#' a `Random effects` block of rows (SD / correlation / residual with SE and
#' CI; `re_scale`, `re_columns`), deliberately with **no per-row p-value**
#' (boundary-invalid Wald; Self & Liang 1987) -- the footer carries the
#' chi-bar-squared LR test of the whole random part, and
#' `re_test = "lrt"` / `"rlrt"` adds an opt-in boundary-correct per-term
#' test. `N (groups)` and `ICC` are fit-stat rows; Nakagawa marginal /
#' conditional R-squared are the default R-squared family. `CR*` robust via
#' \pkg{clubSandwich} (glmmTMB: conditional part only, disclosed).
#'
#' @section Ordinal models:
#' Cut-points render as a `Thresholds` block (log-odds scale, never
#' exponentiated; `show_thresholds`). Partial-proportional-odds `clm` terms
#' render as a `Non-proportional effects` block, one coefficient per
#' cut-point. `exponentiate` yields proportional odds ratios under logit;
#' `ci_method = "profile"` profiles the predictor coefficients. AME is
#' per-category (the marginal effect on each P(Y = k)). Defaults include
#' McFadden and Nagelkerke pseudo-R-squared. See
#' `vignette("table-regression-ordinal")`.
#'
#' @section Counts and two-part models:
#' Two-part models show their full model: the zero component renders as a
#' `Zero-inflation` block (`zeroinfl`, glmmTMB `ziformula`: probability of a
#' structural zero) or a `Zero hurdle` block (`hurdle`: probability of a
#' nonzero count -- the opposite direction, hence the distinct label), and a
#' `Dispersion` block when `dispformula` has covariates. Component
#' coefficients join the `p_adjust` family and take stars; a zero component
#' is exponentiated only under a logit link (odds ratio). AME is the
#' combined-response effect on E(Y). `CR*` for `pscl` fits covers both components
#' via `sandwich::vcovCL()`. Opt out with `show_components = FALSE`.
#'
#' @section Categorical outcomes:
#' `multinom` renders per non-reference outcome; `exponentiate` yields
#' odds ratios of each outcome against the reference outcome -- the
#' baseline-category logits are log-odds (Agresti; SAS prints
#' "Odds Ratio Estimates" under its generalized-logit link; Stata's `mlogit, rrr` labels
#' the same quantity a relative-risk ratio). AME is per-outcome.
#' `nested = TRUE` compares nested `multinom` fits by likelihood-ratio test
#' (the `anova.multinom()` convention). Cluster-robust `CR*` is
#' available (one cluster value per observation; sandwich >= 3.1-2)
#' and the AME columns honour it; `HC*` is refused -- a multi-equation
#' model has no working residuals.
#' `mlogit` renders
#' per-alternative rows; AME is refused (no `slopes()` method exists for
#' its data format). `CR*` is available with one cluster value per choice
#' situation, and `n` counts choice situations; `HC*` is refused
#' (`sandwich::vcovHC()` mis-scales the meat for mlogit's per-chooser
#' score structure).
#'
#' @section Survival models:
#' Cox models exponentiate to hazard ratios; `survreg` log-scale
#' distributions to time ratios (identity-scale distributions are left
#' untouched). AME is refused for Cox fits (no marginal-probability effect
#' on the hazard scale); their absolute-effect columns are the
#' `"rmst"` and `"risk_diff"` families instead -- covariate-adjusted
#' RMST and cumulative-incidence differences by g-computation, with
#' the mandatory `tau` / `at_time` horizons. For `coxph`:
#' right-censored single-record fits, `strata()` supported
#' (within-stratum baselines), `tt()` refused. For `survreg`: the
#' closed-form AFT curves are standardized directly (stratified
#' `survreg` refused).
#' `CR*` uses the Lin-Wei grouped-dfbeta sandwich
#' (`coxph`) or `rms::robcov()` (`cph`, needs `x = TRUE, y = TRUE`).
#' `nested = TRUE` compares nested Cox fits by likelihood-ratio test.
#'
#' @section Robust, IV, quantile and panel models:
#' `estimatr` fits keep their own robust SEs (never overwritten);
#' `quantreg::rq()` defaults to the heteroskedasticity-robust `"nid"`
#' sandwich (quantreg's own large-sample default), with `"iid"`,
#' `"ker"`, `"rank"` (CIs only) and a native `"bootstrap"` --
#' clustered via the wild gradient bootstrap -- as `vcov` options
#' (the footer names the estimator);
#' `fixest` fits flag their absorbed fixed effects in the table title,
#' with per-factor group counts as the opt-in `n_groups` fit
#' statistic.
#'
#' @section Bayesian models:
#' Posterior median, posterior MAD SD, and equal-tailed credible
#' intervals (`ci_method = "hdi"` opts into the highest-density
#' interval); deliberately no p-value column and no stars -- the
#' probability of direction (`"pd"`) is the opt-in posterior summary.
#' A sampler-diagnostics guard checks every fit (R-hat, ESS,
#' divergences, E-BFMI) and per-coefficient `"rhat"` / `"ess_bulk"` /
#' `"ess_tail"` / `"mcse"` columns are available. The AME columns are
#' draws-native (posterior median, MAD SD and credible interval of
#' the per-draw `avg_slopes()`; no `"ame_p"`), and so are the
#' standardized betas (`"posthoc"` / `"basic"` / `"smart"`, exact
#' affine rescales of the draws; fixed-effects `stan_glm`-style fits
#' only -- multilevel and `stan_polr` / `stan_betareg` fits are
#' refused, `brmsfit` carries no recoverable design matrix;
#' `"refit"` / `"pseudo"` refused).
#' Multilevel fits
#' (`stan_glmer`, `brm` with grouping terms) report their random
#' effects as a block -- posterior median SD and credible interval per
#' component, from the draws -- with no likelihood-ratio line.
#' `p_adjust` and likelihood-based fit-statistic tokens are refused
#' (no p-values, no likelihood-based information criteria in a
#' posterior); `"r2_bayes"` is in the default fit statistics and
#' `"elpd_loo"` / `"looic"` / `"waic"` are opt-in, with standard
#' errors and reliability caveats in the footer; compare models with
#' `loo::loo_compare()` outside the table.
#'
#' @return A data frame with one row per supported engine and columns
#'   `family`, `class`, `engine`, `ame`, `exponentiate`, `blocks`.
#'
#' @seealso [table_regression()]; `vignette("table-regression")` and
#'   `vignette("table-regression-ordinal")`.
#'
#' @aliases table_regression_mixed table_regression_ordinal table_regression_categorical table_regression_counts table_regression_survival table_regression_robust table_regression_bayesian
#'
#' @examples
#' table_regression_models()
#'
#' # All engines of one family:
#' subset(table_regression_models(), family == "Mixed effects")
#' @export
table_regression_models <- function() {
  .supported_models_registry()
}
