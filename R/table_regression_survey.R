#' Survey-design models in table_regression()
#'
#' @description
#' [table_regression()] reads the fits of a `survey::svydesign()` or
#' `survey::as.svrepdesign()` design: `survey::svyglm()` (and its replicate
#' sibling `svrepglm`), `survey::svyolr()`, and `survey::svycoxph()` (and
#' `svrepcoxph`). They are read as design-based throughout: the
#' coefficients, the variance and the reference distribution all come
#' from survey. The note names the design, the number of primary sampling
#' units and the residual degrees of freedom.
#'
#' @section Usage:
#' `r spicy:::.render_family_usage(c("vcov", "nested", "show_fit_stats"))`
#'
#' This family has no argument of its own. The other arguments are
#' documented in [table_regression()]; the values they take for
#' survey-design models are listed below.
#'
#' @section Engines:
#' `r spicy:::.render_supported_models_md("Survey-weighted")`
#'
#' AME: whether the average-marginal-effects columns are available.
#' Exponentiate: what `exponentiate = TRUE` gives. Blocks: the labeled
#' blocks of rows the class adds below its coefficients. vcov: the values
#' of `vcov` the class accepts. The same rows are
#' returned by
#' `subset(table_regression_models(), family == "Survey-weighted")`.
#'
#' @section What the table shows:
#' **Coefficient rows.** One row per coefficient, as for the matching
#' model without a design: `svyglm` as `glm`, `svyolr` as `MASS::polr()`
#' (with its `Thresholds` block, [table_regression_ordinal]), `svycoxph` as
#' `survival::coxph()` ([table_regression_survival]).
#'
#' **AME.** The Horvitz-Thompson estimator: the mean unit-level effect
#' weighted by the sampling weights of the analytic sample, with its
#' variance from the delta method on the design vcov. It is tested on the
#' same distribution as the coefficient rows. `svyolr` gives one effect per
#' category.
#'
#' **Counts.** Both are reported: the observed `n` and the `Weighted n`
#' the estimates describe. `svycoxph` adds the number of events.
#'
#' @section Values of the shared arguments:
#' \describe{
#'   \item{`vcov`, `cluster`}{`"classical"` only, which here is the
#'     design-based variance of the fit (Taylor linearization or
#'     replicates). Clustering belongs in the design,
#'     `survey::svydesign(ids = ~cluster, ...)`, not in the table call.}
#'   \item{`nested`}{`FALSE` only. See *What is refused, and why*.}
#'   \item{`show_fit_stats`}{By default `n` and `Weighted n`, with `AIC`
#'     for `svyglm` and the number of events for `svycoxph`. On request,
#'     `"eff_p"` and `"concordance"`; see *Fit statistics*.}
#'   \item{`exponentiate`}{Odds or rate ratios for `svyglm` under the logit
#'     or log link, odds ratios for `svyolr` (logit), hazard ratios for
#'     `svycoxph`.}
#'   \item{`standardized`}{Refused.}
#' }
#'
#' @section Standard errors, degrees of freedom and intervals:
#' Inference is Wald t at the degrees of freedom survey writes on the fit:
#' `df.residual` for `svyglm` and `svyolr`, `degf.resid` or
#' `degf.residual` for the two Cox engines. This is what
#' [survey::regTermTest()] takes as its denominator. It is not
#' `survey::degf(design)`, and it is not re-derived here: the engines of
#' survey do not share one expression (a Cox fit carries
#' `degf(design) - p + 1` although it has no intercept for the `+ 1` to
#' cancel, so it ends one above the two other classes). The value is read
#' off the object, and the note prints it. The interval uses the same
#' distribution, and so do the average marginal effects.
#'
#' @section Fit statistics:
#' \itemize{
#'   \item `"aic"`: `svyglm` keeps an `AIC` row. survey's
#'     `extractAIC.svyglm` computes the design-based AIC of Lumley & Scott
#'     (2015).
#'   \item `"eff_p"`: the effective number of parameters of the design,
#'     the first element of survey's `extractAIC` (the sum of the
#'     Rao-Scott eigenvalues), reported beside the AIC on request.
#'   \item `"concordance"`: the concordance of a `svycoxph` fit goes to the
#'     note, or to a row on request.
#' }
#'
#' @section What is refused, and why:
#' \itemize{
#'   \item Every model-derived variance (`HC*`, `CR*`, `"bootstrap"`,
#'     `"jackknife"`). The design is the variance authority: the fit's
#'     Taylor or replicate variance is already the robust variance for the
#'     declared design. The way to change the estimator is to change the
#'     design.
#'   \item `"bic"`, `"deviance"` and `"log_lik"` for a table of design
#'     fits, and every likelihood statistic (AIC, BIC, log-likelihood,
#'     deviance, pseudo-R-squared) for `svyolr` and `svycoxph`. A
#'     design-based fit is estimated without a likelihood, and survey's own
#'     `deviance()` returns a sign-flipped likelihood-ratio statistic on
#'     one Cox engine and a bare zero on the other. `BIC.svyglm` requires a
#'     maximal model and has no default.
#'   \item `nested = TRUE`. There is no likelihood to compare, so every
#'     change statistic would be empty, and a block of empty rows reads
#'     like an answer. Put the models side by side with `nested = FALSE`,
#'     and test a term under the design with [survey::regTermTest()].
#'   \item The AME for `svycoxph`, on the same ground as for a Cox model
#'     without a design ([table_regression_survival]).
#'   \item The `"rmst"` and `"risk_diff"` columns for `svycoxph`: their
#'     uncertainty comes from resampling subjects, which ignores the strata
#'     and clusters the design declares. Use [survey::svykm()] for a
#'     marginal curve.
#'   \item `standardized`.
#' }
#'
#' @seealso [table_regression()] for the arguments shared by all classes;
#'   [table_regression_models()] for the registry of supported classes and
#'   the other families; [table_categorical_svy()] and
#'   [table_continuous_svy()] for descriptive tables of a design; the
#'   [Summary tables from a survey design](https://amaltawfik.github.io/spicy/articles/survey-tables.html)
#'   article.
#'
#' @examples
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   data("api", package = "survey")
#'   des <- survey::svydesign(ids = ~dnum, weights = ~pw, data = apiclus1,
#'                            fpc = ~fpc)
#'   fit <- survey::svyglm(api00 ~ ell + meals, design = des)
#'   print(table_regression(fit))
#' }
#'
#' @name table_regression_survey
NULL
