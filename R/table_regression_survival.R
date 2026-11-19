#' Survival models in table_regression()
#'
#' @description
#' [table_regression()] reads the Cox models of `survival::coxph()` and
#' `rms::cph()`, the parametric accelerated failure time models of
#' `survival::survreg()`, and the parametric models of
#' `flexsurv::flexsurvreg()`. The coefficients fill the rows, as hazard
#' ratios or time ratios under `exponentiate = TRUE`. For `coxph` and
#' `survreg` fits, two families of columns give absolute effects: the
#' difference in restricted mean survival time (RMST) over `[0, tau]` and
#' the difference in cumulative incidence at `at_time`, by g-computation
#' from the fitted model. The Cox model of a survey design,
#' `survey::svycoxph()`, is described in [table_regression_survey].
#'
#' @section Usage:
#' `r spicy:::.render_family_usage("Survival")`
#'
#' The other arguments are documented in [table_regression()]; the values
#' they take for survival models are listed below.
#'
#' @section Engines:
#' `r spicy:::.render_supported_models_md("Survival")`
#'
#' AME: whether the average-marginal-effects columns are available; for
#' `coxph`, the RMST and risk-difference columns stand in their place.
#' Exponentiate: what `exponentiate = TRUE` gives. Blocks: the labeled
#' blocks of rows the class adds below its coefficients. vcov: the values
#' of `vcov` the class accepts. The same rows are
#' returned by `subset(table_regression_models(), family == "Survival")`.
#'
#' The AME of a Cox model, `coxph` or `cph`, is refused: average marginal
#' effects on a proportional-hazards fit are on an ambiguous survival or
#' hazard scale, with unreliable standard errors. A `flexsurvreg` fit has
#' no AME either: marginaleffects returns one row per time for it, not one
#' effect per predictor. Neither `cph` nor `flexsurvreg` has the RMST and
#' risk-difference columns, which are computed from a `coxph` or a
#' `survreg` fit.
#'
#' @section What the table shows:
#' **Coefficient rows.** One row per coefficient, on the log-hazard scale
#' for a Cox model and on the scale of the survival time (its log for the
#' log-scale distributions) for `survreg`. Under `exponentiate = TRUE`,
#' Cox models give hazard ratios and the log-scale distributions of
#' `survreg` give time ratios. Identity-scale distributions are left
#' untouched. The distribution parameters of a `flexsurvreg` fit render
#' as a block of their own.
#'
#' **Absolute effects.** The RMST difference and the risk difference are
#' computed by g-computation (regression standardization): the survival
#' curve of every subject is predicted under each level of the predictor,
#' the other covariates kept as observed, the curves are averaged, and the
#' two standardized curves are integrated up to `tau` (RMST) or read at
#' `at_time` and complemented (risk). Their standard errors and intervals
#' come from a nonparametric bootstrap of `boot_n` replicates. For
#' `coxph`, the columns are defined for right-censored single-record fits.
#' `strata()` is supported: standardization keeps each subject's own
#' stratum baseline, and the note says so. For `survreg`, the closed-form
#' curves of the AFT model are standardized directly.
#'
#' **Fit statistics.** By default, `n`, the number of events and `AIC` for
#' Cox fits; see *Fit statistics* below.
#'
#' @section Arguments for this family:
#' \describe{
#'   \item{`tau`}{The RMST horizon: the `"rmst"` columns report the
#'     restricted-mean-survival-time difference over `[0, tau]`. A positive
#'     time on the scale of the outcome, or `"minmax"` for the smallest
#'     per-group maximum follow-up; the note gives the value used. No
#'     default: the horizon defines the estimand. Supplied while no RMST
#'     column is requested, it is an error.}
#'   \item{`at_time`}{The landmark time of the `"risk_diff"` columns: the
#'     difference in cumulative incidence at `at_time`. A positive time on
#'     the scale of the outcome; no default. Supplied while no
#'     risk-difference column is requested, it is an error.}
#' }
#'
#' **Column tokens of this family.** These `show_columns` tokens are
#' defined for `coxph` and `survreg` fits only:
#' \itemize{
#'   \item `"rmst"`, `"rmst_se"`, `"rmst_ci"`, `"rmst_p"`: the RMST
#'     difference over `[0, tau]`, its standard error, interval and
#'     p-value. They need `tau`.
#'   \item `"risk_diff"`, `"risk_diff_se"`, `"risk_diff_ci"`,
#'     `"risk_diff_p"`: the difference in cumulative incidence at
#'     `at_time`. They need `at_time`.
#'   \item `"n_events"`: the event counts as `events/N`, per factor level
#'     and as model totals on continuous rows, for right-censored `coxph`
#'     fits (and binary outcomes outside this family).
#' }
#'
#' The arguments shared with other classes take these values for survival
#' models:
#' \describe{
#'   \item{`vcov`, `cluster`}{`coxph` and `cph`: `"classical"` and `"CR0"`.
#'     `survreg`: `"classical"` and `"CR1"`. `flexsurvreg`: `"classical"`
#'     only. See *Standard errors, degrees of freedom and intervals*.}
#'   \item{`boot_n`}{The number of bootstrap replicates of the RMST and
#'     risk-difference columns, `1000L` by default.}
#'   \item{`exponentiate`}{Hazard ratios for Cox models, time ratios for
#'     the log-scale distributions of `survreg`. An exponentiated
#'     intercept is no ratio, and the note says what it estimates.}
#'   \item{`standardized`}{Refused: standardized coefficients are not
#'     defined for these classes.}
#'   \item{`show_fit_stats`}{`"n_events"` and `"concordance"` belong to
#'     the Cox models. See *Fit statistics*.}
#' }
#'
#' @section Standard errors, degrees of freedom and intervals:
#' The coefficients are tested by Wald z. A robust `vcov` is the one
#' cluster estimator of each class:
#' \itemize{
#'   \item `coxph`: `"CR0"`, the Lin-Wei grouped-dfbeta sandwich, the
#'     variance of `coxph(cluster = )`.
#'   \item `cph`: `"CR0"`, from [rms::robcov()], which needs the fit's
#'     `x = TRUE, y = TRUE`.
#'   \item `survreg`: `"CR1"`, from [sandwich::vcovCL()]: the CR0 sandwich
#'     scaled by \eqn{G/(G-1)}{G/(G-1)}, `G` the number of clusters.
#' }
#' The Lin-Wei and `robcov()` sandwiches have no small-sample factor,
#' hence `"CR0"`; the other `CR*` tokens are refused rather than mapped.
#' For a censored `coxph` fit, `cluster` has one entry per subject.
#'
#' Hierarchical comparison (`nested = TRUE`). Nested Cox fits, and nested
#' `survreg` fits, are compared by likelihood-ratio test,
#' `c("lrt_change", "p_change")`. Under `"CR0"` (`coxph`, `cph`) or
#' `"CR1"` (`survreg`), and for `survreg(robust = TRUE)`, the change test
#' is the Wald chi-square of the added coefficients on the robust matrix.
#' A Cox model without a coefficient (`~ 1`, or `strata()` alone) can be
#' the first model of the hierarchy: its column shows the fit statistics,
#' and the next model its likelihood-ratio test against it. Alone, it is
#' refused, since it has no row to show. See *Hierarchical (nested) model
#' comparison* in [table_regression()].
#'
#' @section Fit statistics:
#' Cox fits default to `r spicy:::.render_fit_stats_default("coxph")`: the
#' number of events is reported beside `n`. `survreg` and `flexsurvreg`
#' fits default to `r spicy:::.render_fit_stats_default("survreg")`. On
#' request:
#' \itemize{
#'   \item `"concordance"`: Harrell's C of a `coxph` fit, the value
#'     `summary()` reports. When the row is shown, the concordance leaves
#'     the note, which otherwise gives it with its standard error. `cph`
#'     fits have no concordance row or note.
#'   \item `"sigma"`: for `survreg`, the scale \eqn{\sigma}{sigma} of the
#'     model, blank when strata give one scale per stratum. Asked for in a
#'     table where every `survreg` fit is stratified, it is refused. See
#'     [table_regression_tokens].
#' }
#'
#' @section Tied event times and the survival estimands:
#' A Cox model has to decide what to do when several events share the same
#' time, and `survival::coxph()` decides with `ties = "efron"` by default.
#' The absolute estimands do not make a second decision: the baseline
#' hazard behind the RMST-difference and risk-difference columns follows
#' the tie-handling convention of the fit, as [survival::survfit()] and
#' [survival::basehaz()] do. A `ties = "breslow"` fit gives a Breslow
#' baseline; the default Efron fit gives an Efron baseline. The
#' hazard-ratio column and the RMST column thus come from the same
#' likelihood. To change the convention, change the fit.
#'
#' The number of ties alone does not say whether the choice matters. What
#' governs it is the fraction of tied events within the risk set, together
#' with the effect size and the spread of the covariates: a large risk set
#' dilutes a tied block, a small one (fine strata, matched or nested
#' case-control designs) does not. A file with hundreds of tied days can
#' be insensitive to the convention while a small stratified one with a
#' handful of ties is not.
#'
#' The approximations are all biased at finite sample size and agree
#' asymptotically. Efron's has a much smaller small-sample bias
#' (Hertz-Picciotto & Rockhill 1997), which makes it the default. Efron
#' (1977), after Peto, expected the choice to make little difference in
#' practice.
#'
#' @section What is refused, and why:
#' \itemize{
#'   \item The AME columns for `coxph` and `cph` (see *Engines*), and for
#'     `flexsurvreg`.
#'   \item The RMST and risk-difference columns for `cph` and
#'     `flexsurvreg`, for a `coxph` fit with time-transform `tt()` terms,
#'     for a stratified `survreg` fit, whose per-stratum scale
#'     parameters are not standardized (refit without `strata()`), and
#'     for a `coxph` fit with a `frailty()` term, whose random effect the
#'     estimand would ignore.
#'   \item The objects that inherit from `coxph` or `survreg` without
#'     being that model: `survival::clogit()` fits, multi-state `coxph`
#'     fits, `coxph` fits with a `pspline()` or a non-sparse `frailty()`
#'     term, and `survey::svysurvreg()` fits.
#'   \item `"rmst"` columns without `tau`, and `"risk_diff"` columns
#'     without `at_time`.
#'   \item `HC*`, `"bootstrap"` and `"jackknife"` for every class, and
#'     the `CR*` tokens other than the one estimator of the class.
#'   \item `standardized` for every class of this family.
#' }
#'
#' @seealso [table_regression()] for the arguments shared by all classes;
#'   [table_regression_models()] for the registry of supported classes and
#'   the other families; [table_regression_survey] for
#'   `survey::svycoxph()`; the
#'   [Survival regression tables](https://amaltawfik.github.io/spicy/articles/table-regression-survival.html)
#'   article.
#'
#' @references
#' Efron, B. (1977). The efficiency of Cox's likelihood function
#' for censored data. *Journal of the American Statistical
#' Association*, 72(359), 557-565.
#'
#' Hertz-Picciotto, I. & Rockhill, B. (1997). Validity and
#' efficiency of approximation methods for tied survival times in
#' Cox regression. *Biometrics*, 53(3), 1151-1156.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("survival", quietly = TRUE)) {
#'   library(survival)
#'   lung$sex <- factor(lung$sex, labels = c("Male", "Female"))
#'   fit <- coxph(Surv(time, status) ~ age + sex, data = lung)
#'   print(table_regression(fit, exponentiate = TRUE))
#'
#'   # RMST difference over the first year, from 200 bootstrap replicates.
#'   set.seed(1)
#'   print(table_regression(fit, show_columns = c("b", "rmst", "rmst_ci"),
#'                          tau = 365, boot_n = 200))
#' }
#' }
#'
#' @name table_regression_survival
NULL
