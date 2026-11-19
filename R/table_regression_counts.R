#' Count and two-part models in table_regression()
#'
#' @description
#' [table_regression()] reads the zero-inflated and hurdle models of
#' `pscl::zeroinfl()` and `pscl::hurdle()`. The count coefficients fill
#' the rows, and the zero component follows them as a labeled block of
#' rows in the same columns. The zero-inflation, hurdle and dispersion
#' components of `glmmTMB::glmmTMB()` render as the same blocks; the other
#' rules of `glmmTMB` fits, random effects included, are in
#' [table_regression_mixed]. Poisson and negative-binomial models fitted
#' by `glm()` or `MASS::glm.nb()` follow the rules of [table_regression()];
#' their dispersion rows are described below.
#'
#' @section Usage:
#' `r spicy:::.render_family_usage("Counts, two-part", "exponentiate")`
#'
#' The other arguments are documented in [table_regression()]; the values
#' they take for count models are listed below.
#'
#' @section Engines:
#' `r spicy:::.render_supported_models_md("Counts, two-part")`
#'
#' AME: whether the average-marginal-effects columns are available; the
#' AME of a two-part model is the effect on the overall expected count
#' E(Y). Exponentiate: what `exponentiate = TRUE` gives. Blocks: the
#' labeled blocks of rows the class adds below its coefficients. vcov: the
#' values of `vcov` the class accepts. The same rows are returned by
#' `subset(table_regression_models(), family == "Counts, two-part")`.
#'
#' @section What the table shows:
#' **Count rows.** One row per coefficient of the count component. Under
#' `exponentiate = TRUE` they are rate ratios of the count process: for
#' `zeroinfl`, the rate among units not in the structural-zero class; for
#' `hurdle`, the latent untruncated rate. They are not multiplicative
#' effects on the overall expected count E(Y).
#'
#' **Zero component.** Two-part models show their full model, and the
#' note says what each block models:
#' \itemize{
#'   \item `zeroinfl`, and `glmmTMB` with `ziformula`: a `Zero-inflation`
#'     block, the model for the probability of a structural zero.
#'   \item `hurdle`: a `Zero hurdle, Pr(y > 0)` block, the model for the
#'     probability of a nonzero count, the opposite direction, hence the
#'     distinct label.
#'   \item A `glmmTMB` hurdle model (a zero-truncated family, `ziGamma()`,
#'     `lognormal()` or `beta_family()` with `ziformula`): a
#'     `Zero hurdle, Pr(y = 0)` block, the probability of a zero.
#'   \item `glmmTMB` with covariates in `dispformula`: a `Dispersion`
#'     block, on the log scale, never exponentiated.
#' }
#' The component rows carry Wald inference (B, SE, z, p, CI), join the
#' `p_adjust` family and take significance stars.
#'
#' **AME.** The effect on the overall expected count E(Y), combining the
#' count and zero processes, computed by `marginaleffects::avg_slopes()` on
#' the full model. The zero-component rows carry no AME of their own,
#' since the combined effect includes them.
#'
#' @section Arguments for this family:
#' \describe{
#'   \item{`show_components`}{Logical. `TRUE` (default) displays the
#'     zero-inflation, zero-hurdle and dispersion components as labeled
#'     blocks of rows below the count coefficients. `FALSE` omits the
#'     blocks; the title still names the model type. The argument also
#'     governs the `Scale effects` block of an `ordinal::clm()` fit
#'     ([table_regression_ordinal]).}
#' }
#'
#' The arguments shared with other classes take these values for count
#' models:
#' \describe{
#'   \item{`exponentiate`}{Rate ratios for the count rows. A zero
#'     component is exponentiated only when its link makes the result an
#'     odds ratio (the logit zero components). Probit, cauchit and cloglog
#'     zero links, and count-type hurdle zero parts, stay on the link
#'     scale, and the note says so.}
#'   \item{`vcov`, `cluster`}{Poisson and geometric `zeroinfl` and
#'     `hurdle` fits: `"classical"` and `"CR1"`. A fit with a
#'     negative-binomial part: `"classical"` only. `glmmTMB`: `"classical"`,
#'     `"CR0"` and `"CR1"`. See *Standard errors, degrees of freedom and
#'     intervals*.}
#'   \item{`show_fit_stats`}{The dispersion rows below.}
#' }
#'
#' @section Standard errors, degrees of freedom and intervals:
#' The coefficients of both components are tested by Wald z.
#'
#' `"CR1"` for Poisson and geometric `pscl` fits comes from
#' [sandwich::vcovCL()], which covers both components at once: the CR0
#' sandwich scaled by \eqn{G/(G-1)}{G/(G-1)}, `G` the number of clusters,
#' the one cluster estimator of these classes. A negative-binomial
#' `zeroinfl` or `hurdle` keeps its model-based standard errors: the
#' sandwich of these fits leaves out the score of theta. When the count
#' part is negative binomial, fit the model with `glmmTMB` instead (family
#' `nbinom2` with `ziformula`, or `truncated_nbinom2` for a hurdle), whose
#' sandwich includes it; glmmTMB's zero hurdle is binomial, as with
#' `zero.dist = "binomial"`. A `hurdle` whose zero part alone is negative
#' binomial (`zero.dist = "negbin"`) is refused too; refit it with the
#' default `zero.dist = "binomial"`, which keeps `"CR1"`.
#'
#' `"CR0"` and `"CR1"` for `glmmTMB` fits come from glmmTMB's own sandwich
#' and cover the zero-inflation and dispersion blocks; see
#' [table_regression_mixed].
#'
#' @section Fit statistics:
#' The dispersion of a negative-binomial fit is an opt-in fit-statistic
#' row: `"theta"` and `"alpha"` (= 1/theta, the Stata `nbreg` convention)
#' for NB2 fits, among them `zeroinfl` and `hurdle` with
#' `dist = "negbin"`, whose theta is that of the count part; `"delta"` for
#' NB1 fits (glmmTMB `nbinom1`). A Poisson column in the same table is
#' blank on these rows. See *Vocabulary tokens* in [table_regression()]
#' for the classes each token covers.
#'
#' @section What is refused, and why:
#' \itemize{
#'   \item `HC*`: the `pscl` fits have no hat values, so the HC
#'     machinery of \pkg{sandwich} fails on them.
#'   \item A robust `vcov` for a `pscl` fit with a negative-binomial part
#'     (see above).
#'   \item `"CR0"`, `"CR2"`, `"CR3"`, `"bootstrap"` and `"jackknife"` for
#'     the `pscl` fits: `"CR1"` is their one robust estimator.
#'   \item `standardized`, for `zeroinfl` and `hurdle`.
#' }
#'
#' @seealso [table_regression()] for the arguments shared by all classes;
#'   [table_regression_models()] for the registry of supported classes and
#'   the other families; [table_regression_mixed] for `glmmTMB` fits; the
#'   [Count and two-part regression tables](https://amaltawfik.github.io/spicy/articles/table-regression-counts.html)
#'   article.
#'
#' @examples
#' if (requireNamespace("pscl", quietly = TRUE)) {
#'   data("bioChemists", package = "pscl")
#'   fit <- pscl::zeroinfl(art ~ fem + kid5 | kid5, data = bioChemists)
#'   print(table_regression(fit, exponentiate = TRUE))
#'
#'   # Count rows only.
#'   print(table_regression(fit, show_components = FALSE))
#' }
#'
#' @name table_regression_counts
NULL
