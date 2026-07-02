# ---------------------------------------------------------------------------
# Per-family reference pages for table_regression().
#
# Documentation-only topics (no code): one page per model family, in the
# spirit of parameters::model_parameters()'s per-class reference pages. Each
# page states, for its family: the supported engines, the inference behind
# the SE / df / p columns, the robust-vcov backends, ci_method availability,
# exponentiate semantics, the AME estimand, the default fit statistics, and
# the family's labelled blocks. Content is anchored on the internal
# capability matrix (dev/fit_stats_by_class.md) and the per-class test
# suites; every numeric behaviour stated here is oracle-tested.
# ---------------------------------------------------------------------------


#' Supported model classes in table_regression()
#'
#' @description
#' [table_regression()] accepts a single fitted model or a list of fitted
#' models from 35+ classes, and renders each with the reporting conventions
#' of its model family. This page is the map; the per-family pages listed
#' below document the class-specific behaviour in detail.
#'
#' @section Supported classes by family:
#' \describe{
#'   \item{Linear and generalized linear}{`stats::lm`, `stats::glm`,
#'     `MASS::glm.nb`, `MASS::rlm`, `stats::nls`.}
#'   \item{Robust, IV, quantile, panel}{`estimatr::lm_robust` /
#'     `iv_robust`, `AER::ivreg` / `tobit`, `quantreg::rq`,
#'     `fixest::feols` / `feglm` / `fepois` / `fenegbin`. See
#'     [table_regression_robust].}
#'   \item{Mixed effects}{`lme4::lmer` / `glmer`, `glmmTMB::glmmTMB`,
#'     `nlme::lme` / `gls`. See [table_regression_mixed].}
#'   \item{Ordinal}{`MASS::polr`, `ordinal::clm` (including partial
#'     proportional odds via `nominal = ~`). See
#'     [table_regression_ordinal].}
#'   \item{Categorical}{`nnet::multinom`, `mlogit::mlogit`. See
#'     [table_regression_categorical].}
#'   \item{Counts and two-part models}{`pscl::hurdle` / `zeroinfl`, and
#'     `glmmTMB` zero-inflation / dispersion components. See
#'     [table_regression_counts].}
#'   \item{Survival}{`survival::coxph` / `survreg`, `rms::cph`,
#'     `flexsurv::flexsurvreg`. See [table_regression_survival].}
#'   \item{Survey-weighted}{`survey::svyglm` (design-based SEs are the
#'     default and never overwritten).}
#'   \item{Additive, proportions, selection}{`mgcv::gam` / `bam`,
#'     `betareg::betareg`, `sampleSelection::selection`.}
#'   \item{rms}{`rms::ols` / `lrm` / `Glm` (cluster-robust via
#'     `rms::robcov`).}
#'   \item{Bayesian}{`rstanarm`, `brms`. See
#'     [table_regression_bayesian].}
#' }
#'
#' @section Shared semantics:
#' Whatever the class, the same rules hold:
#' \itemize{
#'   \item A robust `vcov` request is honoured through the class's
#'     field-standard backend, or **refused with a clear error** naming the
#'     supported set -- never silently ignored, and the footer always names
#'     the estimator actually applied.
#'   \item `exponentiate = TRUE` is link-gated: it produces a labelled ratio
#'     (OR / IRR / HR / RR / MR / RRR / TR) only where the link warrants
#'     one; identity-link fits warn and are left untouched.
#'   \item Class-specific structure renders as labelled subordinate blocks
#'     of rows in the same table (ordinal `Thresholds`, mixed
#'     `Random effects`, two-part `Zero-inflation` / `Zero hurdle` /
#'     `Dispersion`), with a footer line explaining each block.
#'   \item Fit statistics default to the family's field standard and can be
#'     overridden with `show_fit_stats`; class-inappropriate tokens are
#'     rejected with a pointer to the right ones.
#'   \item Everything is available programmatically: `broom::tidy()`,
#'     `glance()`, [as_structured()], `as.data.frame()`.
#' }
#'
#' @seealso [table_regression()] for the full argument reference;
#'   `vignette("table-regression")` for a narrative introduction.
#' @name table_regression_models
NULL


#' Mixed-effects models in table_regression()
#'
#' @description
#' Reporting behaviour for `lme4::lmer()` / `glmer()`, `glmmTMB::glmmTMB()`,
#' and `nlme::lme()` / `gls()` fits.
#'
#' @section Fixed effects:
#' \itemize{
#'   \item `lmer` with \pkg{lmerTest} loaded: Satterthwaite t-tests
#'     (recommended below ~30 groups); plain `lmer`: Wald z with a footer
#'     suggestion to load \pkg{lmerTest}.
#'   \item `glmer`, `glmmTMB`: Wald z (asymptotic).
#'   \item `lme`: t-tests with nlme's containment degrees of freedom.
#' }
#'
#' @section Random effects:
#' Variance components render as a subordinate `Random effects` block of
#' rows: one row per standard deviation, correlation, and the residual, each
#' with its estimate, SE, and CI (`merDeriv` for lme4; native
#' `confint()` / `intervals()` for glmmTMB / nlme; identical random
#' structures from different engines align on the same rows). `re_scale`
#' switches between the SD (default) and variance scales; `re_columns`
#' controls which cells display.
#'
#' There is deliberately **no per-row p-value** on variance components: the
#' null sigma = 0 sits on the boundary of the parameter space, where a Wald
#' test is invalid (Self & Liang 1987), and no reporting guideline requests
#' one. The significance signal for the random part is the footer's
#' likelihood-ratio test against the no-random-effects model, with the
#' boundary-corrected chi-bar-squared p-value (Stram & Lee 1994) -- the same
#' line Stata's `mixed` prints. `re_test = "lrt"` / `"rlrt"` adds an opt-in
#' per-term test (LR vs the reduced random structure with the
#' chi-bar-squared reference, or `RLRsim`'s exact restricted LRT) -- never a
#' Wald z.
#'
#' @section Fit statistics:
#' Defaults: `nobs`, `n_groups`, `icc`, `r2_marginal`, `r2_conditional`,
#' `AIC`, `BIC`. The Nakagawa marginal / conditional R-squared are
#' cross-validated against `performance::r2_nakagawa()`; the ICC row is
#' dropped automatically when the ICC is not defined (random slopes,
#' crossed factors, families without a link-scale variance).
#'
#' @section Robust variance:
#' `CR*` cluster-robust via \pkg{clubSandwich} (with Satterthwaite df for
#' `lmer`). For `glmmTMB` the CR backend covers the conditional fixed
#' effects only; component rows stay model-based, disclosed in the footer.
#' `HC*` is refused (a single-level concept).
#'
#' @section Zero-inflation and dispersion (glmmTMB):
#' See [table_regression_counts].
#'
#' @seealso [table_regression()], [table_regression_models];
#'   the *Mixed-effects models* section of `vignette("table-regression")`.
#' @name table_regression_mixed
NULL


#' Ordinal models in table_regression()
#'
#' @description
#' Reporting behaviour for `MASS::polr()` and `ordinal::clm()` fits
#' (cumulative link models), including partial-proportional-odds `clm` fits
#' (`nominal = ~`).
#'
#' @section Coefficients and thresholds:
#' Predictor coefficients report Wald z inference. The estimated category
#' cut-points render as a subordinate `Thresholds` block of rows (they are
#' intercepts, with SE and p), always on the log-odds scale -- never
#' exponentiated. Opt out with `show_thresholds = FALSE` (compact footer
#' note instead). Partial-proportional-odds terms render as a
#' `Non-proportional effects` block, one coefficient per cut-point.
#'
#' @section Exponentiate and CIs:
#' `exponentiate = TRUE` yields proportional odds ratios under the logit
#' link (`exp(B)` labels under other links -- never a mislabelled "OR").
#' `ci_method = "profile"` gives profile-likelihood CIs for the predictor
#' coefficients via `confint.polr()` / `confint.clm()` (thresholds stay
#' Wald; a robust `vcov` takes precedence); the footer discloses
#' `95% CIs: profile likelihood.` when used.
#'
#' @section Average marginal effects:
#' Per-category AME: one column per response category (the marginal effect
#' on P(Y = k)), the field-standard matrix layout.
#'
#' @section Robust variance and fit statistics:
#' `CR*` via `sandwich::vcovCL` for plain proportional-odds fits; refused
#' (no estimating functions) for `scale = ~` / `nominal = ~` fits. Defaults:
#' `nobs`, McFadden and Nagelkerke pseudo-R-squared (matching Stata `ologit`
#' and SPSS PLUM; closed-form null likelihood), `AIC`.
#'
#' @seealso [table_regression()], [table_regression_models];
#'   `vignette("table-regression-ordinal")` for the full walk-through.
#' @name table_regression_ordinal
NULL


#' Categorical (multinomial) models in table_regression()
#'
#' @description
#' Reporting behaviour for `nnet::multinom()` and `mlogit::mlogit()` fits.
#'
#' @section Layout and semantics:
#' `multinom` coefficients render per non-reference outcome (one labelled
#' block per outcome level); `exponentiate = TRUE` yields relative-risk
#' ratios (RRR, the Stata `mlogit, rrr` convention). AME reports the
#' marginal effect on each outcome's probability (per-outcome columns).
#' `mlogit` renders per-alternative rows; AME is refused
#' (\pkg{marginaleffects} has no `slopes()` method for its
#' one-row-per-alternative data format).
#'
#' @section Robust variance:
#' `multinom`: classical only. `mlogit`: `HC*` (it provides `estfun`) and
#' `CR*` via `sandwich::vcovCL`; the cluster vector must be at the
#' choice-situation level (one entry per individual), and a long-format
#' cluster errors with a clear message.
#'
#' @seealso [table_regression()], [table_regression_models].
#' @name table_regression_categorical
NULL


#' Count and two-part models in table_regression()
#'
#' @description
#' Reporting behaviour for `pscl::hurdle()` / `pscl::zeroinfl()` and for
#' `glmmTMB` fits with `ziformula =` / `dispformula =` components.
#'
#' @section Component blocks:
#' Two-part models show their FULL model. The count / conditional
#' coefficients render as the main block; the other components render as
#' labelled subordinate blocks, with full Wald inference from the
#' per-component covariance:
#' \itemize{
#'   \item `Zero-inflation` (`zeroinfl`, `glmmTMB` zi): the model for the
#'     probability of a **structural (excess) zero**.
#'   \item `Zero hurdle` (`hurdle`): the model for the probability of a
#'     **nonzero count** -- the opposite direction of zero-inflation. The
#'     distinct labels and the footer gloss keep the two apart (they are
#'     conflated under one header in some other packages).
#'   \item `Dispersion` (`glmmTMB`): only when dispersion was actually
#'     modelled (`dispformula` with covariates); log scale.
#' }
#' Component coefficients are substantive hypotheses: they join the
#' `p_adjust` family and take significance stars. Opt out of the blocks
#' with `show_components = FALSE`.
#'
#' @section Exponentiate:
#' The count component exponentiates to incidence-rate ratios (IRR). A zero
#' component is exponentiated **only when its link makes the result an odds
#' ratio** (the default logit); probit / cauchit / cloglog zero links,
#' count-type hurdle zero parts, and the dispersion model stay on the link
#' scale, disclosed in the footer. (Stata's `irr` similarly leaves the
#' inflate equation untransformed.)
#'
#' @section Average marginal effects:
#' The AME column reports the combined-response marginal effect on E(Y),
#' flowing through both components (the `marginaleffects::avg_slopes()`
#' default) -- the policy-relevant quantity for a two-part model.
#'
#' @section Robust variance:
#' `CR*` for `zeroinfl` / `hurdle` via `sandwich::vcovCL`, covering **both
#' components** with one estimator. `HC*` is refused (no hatvalues
#' machinery). For `glmmTMB`, see [table_regression_mixed].
#'
#' @seealso [table_regression()], [table_regression_models].
#' @name table_regression_counts
NULL


#' Survival models in table_regression()
#'
#' @description
#' Reporting behaviour for `survival::coxph()` / `survreg()`, `rms::cph()`,
#' and `flexsurv::flexsurvreg()` fits.
#'
#' @section Semantics:
#' Cox models report hazard ratios under `exponentiate = TRUE` (the
#' canonical report; the footer carries the event count and concordance).
#' `survreg` accelerated-failure-time fits exponentiate to time ratios for
#' log-scale distributions; identity-scale distributions (gaussian,
#' logistic, t) are left untouched. AME is refused for Cox fits (no single
#' marginal-probability effect exists on the hazard scale; an
#' RMST-difference estimand is on the roadmap).
#'
#' @section Robust variance and nested comparison:
#' `coxph`: `CR*` via the Lin-Wei (1989) grouped-dfbeta sandwich, the
#' field-standard Cox robust SE, with subject-level cluster length checks
#' under censoring. `rms::cph` routes through `rms::robcov` (requires
#' `x = TRUE, y = TRUE`). `nested = TRUE` compares nested Cox models with
#' the likelihood-ratio test (matching `anova.coxph`); R-squared-family
#' change statistics are not defined and stay empty.
#'
#' @seealso [table_regression()], [table_regression_models].
#' @name table_regression_survival
NULL


#' Robust, IV, quantile and panel models in table_regression()
#'
#' @description
#' Reporting behaviour for `estimatr::lm_robust()` / `iv_robust()`,
#' `AER::ivreg()` / `tobit()`, `quantreg::rq()`, and `fixest::feols()` /
#' `feglm()` / `fepois()` / `fenegbin()` fits.
#'
#' @section Semantics:
#' \itemize{
#'   \item `estimatr` fits carry their own robust SEs (including
#'     per-coefficient CR2 Satterthwaite df); spicy reports them as fitted
#'     and never overwrites them.
#'   \item `quantreg::rq` honours the `se =` method used at summary time;
#'     rank-inversion CIs (`se = "rank"`) are carried into the CI columns.
#'   \item `fixest` fits report their fixed-effects structure in the
#'     footer; `feglm` / `fepois` / `fenegbin` exponentiate to OR / IRR.
#' }
#'
#' @seealso [table_regression()], [table_regression_models].
#' @name table_regression_robust
NULL


#' Bayesian models in table_regression()
#'
#' @description
#' Reporting behaviour for \pkg{rstanarm} (`stanreg`) and \pkg{brms}
#' (`brmsfit`) fits.
#'
#' @section Semantics:
#' Coefficient rows report the posterior median, the posterior SD in the SE
#' column, and equal-tailed credible intervals at `ci_level`. There is
#' deliberately **no p-value column** (no null-hypothesis test statistic is
#' computed); significance stars do not apply. `exponentiate = TRUE`
#' follows the model's link, as for frequentist fits. Random-effect /
#' hierarchical structure reporting follows the mixed-models conventions
#' where applicable.
#'
#' @seealso [table_regression()], [table_regression_models].
#' @name table_regression_bayesian
NULL
