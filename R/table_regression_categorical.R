#' Categorical (multinomial) models in table_regression()
#'
#' @description
#' [table_regression()] reads the baseline-category logit models of
#' `nnet::multinom()` and the discrete-choice models of `mlogit::mlogit()`.
#' A single `multinom` fit sets predictors as rows and one column group per
#' non-reference outcome category, so the effect of a predictor can be
#' compared across equations along its row. An `mlogit` fit renders
#' per-alternative rows in two segments: the alternative-invariant
#' coefficients, then one section per non-reference alternative. Under
#' `exponentiate = TRUE` both give odds ratios against the reference
#' outcome or alternative.
#'
#' @section Usage:
#' `r spicy:::.render_family_usage(c("exponentiate", "show_columns", "outcome_labels"))`
#'
#' This family has no argument of its own. The other arguments are
#' documented in [table_regression()]; the values they take for
#' categorical models are listed below.
#'
#' @section Engines:
#' `r spicy:::.render_supported_models_md("Categorical")`
#'
#' AME: whether the average-marginal-effects columns are available;
#' `per outcome` is one effect per outcome category. Exponentiate: what
#' `exponentiate = TRUE` gives. Blocks: how the class lays out its
#' coefficient rows. vcov: the values of `vcov` the class accepts. The same
#' rows are returned by
#' `subset(table_regression_models(), family == "Categorical")`.
#'
#' @section What the table shows:
#' **A single `multinom` model.** Predictors as rows, one column group per
#' non-reference outcome category (spanner = category name). The note
#' `Reference outcome: <level>.` names the base category (Stata's "base
#' outcome" line), and `outcome_labels` relabels the spanners (e.g.
#' `"Student vs Employed"`). The fit statistics print once, under the
#' first group. When per-category AMEs are requested, the reference
#' category appears as a last, AME-only group: its coefficient cells are
#' empty, since the reference has no equation, and its AMEs complete the
#' zero sum across categories. The default `show_columns` compacts to B,
#' SE and p as for several models; request atomic tokens to restore the
#' intervals.
#'
#' **Several `multinom` models, or `nested = TRUE`.** One row per
#' (category, predictor) pair: categories within models would need two
#' spanner levels. `tidy()` and `output = "long"` always return the long
#' form (`"<category>: <term>"` rows), whatever the display;
#' [as_structured()] and `output = "data.frame"` follow the displayed
#' table (one column set per category), as they do for every other layout.
#'
#' **An `mlogit` model.** Two segments, the presentation of Stata's
#' `asclogit`. The `Alternative-invariant` section holds the coefficients
#' of the alternative attributes, one row each, shared across
#' alternatives. Then one section per non-reference alternative collects
#' its intercept and its case-specific coefficients, each against the
#' reference that the note names (`Reference alternative: <level>.`). The
#' `n` row counts choice situations, not the rows of the long-format data.
#'
#' **Fit statistics.** See *Fit statistics* below.
#'
#' @section Values of the shared arguments:
#' \describe{
#'   \item{`exponentiate`}{Odds ratios of each outcome against the
#'     reference outcome: the baseline-category logits are log-odds
#'     (Agresti 2002; SAS prints "Odds Ratio Estimates" under its
#'     generalized-logit link; Stata's `mlogit, rrr` labels the same
#'     quantity a relative-risk ratio).}
#'   \item{`show_columns`}{The AME tokens give one effect per outcome
#'     category for `multinom`. They are refused for `mlogit`:
#'     marginaleffects computes predictions but not slopes for its
#'     one-row-per-choice data structure.}
#'   \item{`outcome_labels`}{For a single `multinom` model, the labels of
#'     the category spanners: one per non-reference outcome category,
#'     unique, in model order. The AME-only group of the reference
#'     category, when shown, keeps its own name, and `FALSE` changes
#'     nothing, as there is no Outcome row to remove.}
#'   \item{`model_labels`}{Refused for a single multinomial model: its
#'     column groups are outcome categories, relabeled by
#'     `outcome_labels`.}
#'   \item{`vcov`, `cluster`}{`"classical"` and `"CR1"` for both classes.
#'     `cluster` has one entry per observation for `multinom` and one per
#'     choice situation for `mlogit`. See *Standard errors, degrees of
#'     freedom and intervals*.}
#'   \item{`nested`}{`TRUE` compares nested `multinom` fits by
#'     likelihood-ratio test, the convention of `anova.multinom()`.}
#'   \item{`ci_method`}{`"wald"` only.}
#'   \item{`standardized`}{Refused.}
#' }
#'
#' @section Standard errors, degrees of freedom and intervals:
#' The coefficients are tested by Wald z. Cluster-robust `"CR1"` comes
#' from [sandwich::vcovCL()]: the CR0 sandwich scaled by
#' \eqn{G/(G-1)}{G/(G-1)}, `G` the number of clusters. For `multinom` it
#' needs \pkg{sandwich} >= 3.1-2, which added its `estfun()` method, and
#' the AME columns follow it. Under `"CR1"`, a nested hierarchy is tested
#' by the Wald chi-square of the added coefficients on the robust matrix.
#'
#' @section Fit statistics:
#' A `multinom` fit defaults to `c("nobs", "pseudo_r2_mcfadden", "aic")`,
#' an `mlogit` fit to `c("nobs", "aic")`, its `n` counting choice
#' situations. See *R-squared of glm and categorical models* in
#' [table_regression()].
#'
#' @section What is refused, and why:
#' \itemize{
#'   \item `HC*` for `multinom`: a multi-equation model has no working
#'     residuals or hat values, so the HC meat cannot be formed.
#'   \item `HC*` for `mlogit`: [sandwich::vcovHC()] computes a result for
#'     it but mis-scales the meat for its per-chooser score structure.
#'   \item `"CR0"`, `"CR2"`, `"CR3"`, `"bootstrap"` and `"jackknife"`:
#'     `"CR1"` is the one robust estimator of these classes.
#'   \item The AME columns for `mlogit` (see `show_columns` above).
#'   \item `model_labels` for a single `multinom` model,
#'     `ci_method = "profile"` and `standardized`.
#' }
#'
#' @seealso [table_regression()] for the arguments shared by all classes;
#'   [table_regression_models()] for the registry of supported classes and
#'   the other families; the
#'   [Multinomial regression tables](https://amaltawfik.github.io/spicy/articles/table-regression-multinomial.html)
#'   article.
#'
#' @references
#' Agresti, A. (2002). *Categorical data analysis* (2nd ed.). Wiley.
#' \doi{10.1002/0471249688}
#'
#' @examples
#' if (requireNamespace("nnet", quietly = TRUE)) {
#'   fit <- nnet::multinom(employment_status ~ age + sex,
#'                         data = sochealth, trace = FALSE)
#'   print(table_regression(fit, exponentiate = TRUE))
#' }
#'
#' @name table_regression_categorical
NULL
