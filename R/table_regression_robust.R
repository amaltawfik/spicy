#' Robust, IV, quantile and panel models in table_regression()
#'
#' @description
#' [table_regression()] reads the fits of `estimatr::lm_robust()` and
#' `estimatr::iv_robust()`, the instrumental-variable and censored models
#' of `AER::ivreg()` and `AER::tobit()`, the quantile regressions of
#' `quantreg::rq()`, and the fixed-effects estimators of `fixest`
#' (`feols()`, `feglm()`, `fepois()`, `femlm()`, `fenegbin()`). The
#' `estimatr` and `fixest` fits carry the variance they were computed
#' with, and the table reports it unchanged. `quantreg::rq()` has an
#' estimator family of its own, chosen through `vcov`. Absorbed fixed
#' effects render as a `Fixed effects:` block at the top of the fit
#' statistics.
#'
#' @section Usage:
#' `r spicy:::.render_family_usage("Robust, IV, quantile, panel", c("vcov", "cluster", "show_fit_stats"))`
#'
#' This family has no argument of its own. The other arguments are
#' documented in [table_regression()]; the values they take for these
#' models are listed below.
#'
#' @section Engines:
#' `r spicy:::.render_supported_models_md("Robust, IV, quantile, panel")`
#'
#' AME: whether the average-marginal-effects columns are available.
#' Exponentiate: what `exponentiate = TRUE` gives. Blocks: the labeled
#' blocks of rows the class adds below its coefficients. vcov: the values
#' of `vcov` the class accepts. The same rows are
#' returned by
#' `subset(table_regression_models(), family == "Robust, IV, quantile, panel")`.
#'
#' The AME is refused for a `fixest` GLM with fixed effects (`feglm()`,
#' `fepois()`, `femlm()`, `fenegbin()`, whatever the family):
#' marginaleffects, which computes it, cannot take into account the
#' uncertainty of the fixed effects. It is refused too for a `feols()`
#' with fixed effects fitted with `lean = TRUE`, which marginaleffects
#' cannot predict from (re-estimate it with `lean = FALSE`). Other
#' `feols()` fits and a GLM without fixed effects have it. The AME of a
#' factor in `i()` sits on the rows of its coefficients, against the
#' reference `i()` gives them (`ref =`). A level of a factor in `i()`, or
#' a numeric variable that `i()` enters as categories, with no coefficient
#' row gets no AME, nor does a factor that `i()` enters only in an
#' interaction.
#'
#' @section What the table shows:
#' **Coefficient rows.** One row per coefficient, with the standard errors
#' the fit carries (`estimatr`, `fixest`) or those of the `vcov` requested
#' (`rq`). The note names the estimator.
#'
#' **Fixed effects.** `fixest` fits, and `estimatr` fits built with
#' `fixed_effects =`, disclose their absorbed fixed effects as a `Fixed
#' effects:` block at the top of the fit statistics: one Yes / No row per
#' factor, blank for the models without the concept in a mixed table.
#' Varying-slope-only factors (`Origin[[x]]`) absorb no intercept: they read
#' No when another model absorbs that factor, and add no row otherwise. The
#' block is on by default (`"fixed_effects"` in `show_fit_stats`).
#'
#' **Fit statistics.** See *Fit statistics* below.
#'
#' @section Values of the shared arguments:
#' \describe{
#'   \item{`vcov`, `cluster`}{`estimatr`, `fixest` and `ivreg`:
#'     `"classical"` only, which for `estimatr` and `fixest` is the variance
#'     the fit carries. `tobit`: `"classical"` and `"CR1"`, as for
#'     `survreg`. `rq`: its own estimator family. See *Standard errors,
#'     degrees of freedom and intervals*.}
#'   \item{`show_fit_stats`}{The defaults of each class, and the
#'     `"fixed_effects"`, `"within_r2"` and `"n_groups"` tokens; see *Fit
#'     statistics*.}
#'   \item{`exponentiate`}{Odds or rate ratios for the GLM-family `fixest`
#'     fits, as the link says.}
#'   \item{`nested`}{See *Standard errors, degrees of freedom and
#'     intervals*.}
#' }
#'
#' @section Standard errors, degrees of freedom and intervals:
#' **`estimatr` and `fixest`.** These fits carry the variance they were
#' computed with, estimatr's `se_type =` and whatever fixest's own `vcov`
#' interface produced, and spicy never overwrites it. A `vcov` request
#' other than `"classical"` is refused; set the estimator through the
#' fitting package instead: `se_type =` (with `clusters =`) for estimatr,
#' `vcov =` at estimation or in `summary()` for fixest. The note carries
#' fixest's own label of the estimator (clustered, Newey-West, Conley,
#' ...), fixest's "IID" written as "Classical". With IID errors, fixest's
#' default small-sample correction ([fixest::ssc()]) multiplies the
#' standard errors of a `feglm()` fit by sqrt((n - 1) / (n - K)) over
#' those of its `glm()` twin with the fixed effects as dummies (a `feols()`
#' fit already matches `lm()`), and
#' `summary(fit, ssc = fixest::ssc(K.adj = FALSE))` gives the `glm()`
#' values.
#'
#' **`quantreg::rq`.** Its own estimator family, not the sandwich
#' vocabulary:
#' \itemize{
#'   \item `"classical"` resolves to the heteroskedasticity-robust `"nid"`
#'     sandwich (Hendricks-Koenker, Hall-Sheather bandwidth), quantreg's
#'     own large-sample default and the analog of Stata's `vce(robust)`.
#'   \item `"iid"`: the Koenker-Bassett homoskedastic estimator, as
#'     Stata's `vce(iid)`.
#'   \item `"ker"`: the Powell kernel sandwich.
#'   \item `"rank"`: rank-score inversion, which gives intervals only: the
#'     SE, t and p cells render as dashes, and the AME columns, which need
#'     a variance matrix, are refused.
#'   \item `"bootstrap"`: quantreg's native `boot.rq` (`bsmethod = "xy"`).
#'     With `cluster =`, the wild gradient cluster bootstrap of Hagemann
#'     (2017), the one cluster-robust route for `rq`. A weighted `rq` fit
#'     refuses it: the resamplers do not carry the weights.
#' }
#' `cluster` with any other `rq` estimator is refused.
#'
#' **`AER::tobit`.** A `tobit` fit is a `survreg` model, and takes the
#' same estimator: `"CR1"` from [sandwich::vcovCL()], the CR0 sandwich
#' scaled by \eqn{G/(G-1)}{G/(G-1)}, `G` the number of clusters. The other
#' `CR*` tokens and `HC*` are refused. Under `"CR1"`, a nested hierarchy
#' is tested by the Wald chi-square of the added coefficients on the
#' robust matrix. The RMST and risk-difference columns of `survreg` are
#' refused for it: a tobit models a censored response, not a time to
#' event.
#'
#' **Hierarchical comparison** (`nested = TRUE`). A quantile hierarchy
#' gets `c("f_change", "p_change")`, the Wald-type test
#' `quantreg::anova.rq()` reports, under `"nid"` and `"rank"`; under
#' `"iid"` and `"ker"` it is a Wald F of the added coefficients, and under
#' `"bootstrap"` a Wald chi-square. A `fixest` hierarchy is compared by
#' likelihood-ratio test; a fit estimated with a non-iid vcov gets a Wald
#' F when its coefficient tests are t (`feols`, and `feglm` with a family
#' whose dispersion is estimated) and a Wald chi-square when they are z
#' (`fepois`, `femlm`, `fenegbin`, and `feglm` with the binomial or Poisson
#' family). See *Hierarchical (nested) model comparison* in
#' [table_regression()].
#'
#' @section Fit statistics:
#' `lm_robust` defaults to
#' `r spicy:::.render_fit_stats_default("lm_robust")`, `iv_robust` to
#' `r spicy:::.render_fit_stats_default("iv_robust")` alone, and an IV `fixest::feols()` shows neither R-squared nor
#' AIC: the 2SLS R-squared is not the classical one. Asked for, it is the
#' R-squared of `ivreg()`, on the structural residuals. The AIC and BIC of
#' a `fixest` model are those of its `lm()`, `glm()` or `MASS::glm.nb()`
#' twin with the fixed effects as dummies; see *Vocabulary tokens* in
#' [table_regression()].
#' \itemize{
#'   \item `"fixed_effects"`: the `Fixed effects:` block, on by default.
#'   \item `"within_r2"`: the R-squared of the model with the fixed effects
#'     partialed out, a default for `feols` except an IV one, on request
#'     for `estimatr`. GLM-family `fixest` fits report fixest's McFadden
#'     `pr2` instead.
#'   \item `"n_groups"`: one `N (<factor>)` row per absorbed factor, on
#'     request.
#' }
#' Named in the call, `"fixed_effects"`, `"within_r2"` and `"n_groups"`
#' are refused when no model of the table absorbs fixed effects, a
#' `fixest` fit without `| fe` included. `"within_r2"` is refused too when
#' no model of the table has a value for it (an IV `feols()`, a GLM-family
#' `fixest` fit).
#'
#' @section What is refused, and why:
#' \itemize{
#'   \item Every `vcov` other than `"classical"` for `estimatr` and
#'     `fixest` fits: the variance is chosen at fit time (see above).
#'   \item For `rq`: `HC*`, since quantile regression has no working
#'     residuals or hat values (`"nid"` is the heteroskedasticity-robust
#'     estimator here, a sparsity-based quantile sandwich, not an HC
#'     variant); `CR*`, since there is no cluster-robust sandwich for it
#'     (use `"bootstrap"` with `cluster =`); and `"jackknife"`, whose
#'     leave-one-out form is inconsistent for quantiles: the quantile
#'     objective is not differentiable, so dropping one observation can
#'     move the fit discontinuously.
#'   \item `nested = TRUE` across two `fixest` fits that absorbed
#'     different fixed effects. fixest counts every absorbed level as an
#'     estimated parameter, so a test across two `| fe` structures
#'     measures the change in the absorbed effects together with the
#'     change in the coefficients, and the two cannot be separated. Hold
#'     the `| fe` part fixed across the hierarchy.
#'   \item The AME of a `fixest` GLM with fixed effects, and of a `feols()`
#'     with fixed effects fitted with `lean = TRUE` (see *Engines*).
#'   \item A `fixest` fit with fixed effects only: it has no coefficient
#'     row to show.
#' }
#'
#' @seealso [table_regression()] for the arguments shared by all classes;
#'   [table_regression_models()] for the registry of supported classes and
#'   the other families; the
#'   [Publication-ready regression tables](https://amaltawfik.github.io/spicy/articles/table-regression.html)
#'   article.
#'
#' @references
#' Hagemann, A. (2017). Cluster-robust bootstrap inference in quantile
#' regression models. *Journal of the American Statistical Association*,
#' 112(517), 446-456. \doi{10.1080/01621459.2016.1148610}
#'
#' @examples
#' if (requireNamespace("quantreg", quietly = TRUE)) {
#'   fit <- quantreg::rq(wellbeing_score ~ age + sex, tau = 0.5,
#'                       data = sochealth)
#'   print(table_regression(fit))
#'
#'   # The homoskedastic estimator instead of the default "nid".
#'   print(table_regression(fit, vcov = "iid"))
#' }
#'
#' @name table_regression_robust
NULL
