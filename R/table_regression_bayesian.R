#' Bayesian models in table_regression()
#'
#' @description
#' [table_regression()] reads the `stanreg` fits of `rstanarm` and the
#' `brmsfit` fits of `brms::brm()`, and summarizes them from their
#' posterior draws: the posterior median, the posterior MAD SD and a
#' credible interval. The table has no p-value and no t-statistic. The
#' probability of direction is an opt-in column, and every table is backed
#' by a check of the sampler diagnostics. Multilevel fits report their
#' random effects as a block of rows, summarized from the draws.
#'
#' @section Usage:
#' `r spicy:::.render_family_usage("Bayesian", c("ci_method", "standardized", "exponentiate", "show_columns"))`
#'
#' This family has no argument of its own. The other arguments are
#' documented in [table_regression()]; the values they take for Bayesian
#' models are listed below.
#'
#' @section Engines:
#' `r spicy:::.render_supported_models_md("Bayesian")`
#'
#' AME: whether the average-marginal-effects columns are available;
#' `yes (draws)` means the effect is computed per posterior draw. Exponentiate:
#' what `exponentiate = TRUE` gives. Blocks: the labeled blocks of rows the
#' class adds below its coefficients. vcov: the values of `vcov` the class
#' accepts. The same rows are returned by
#' `subset(table_regression_models(), family == "Bayesian")`.
#'
#' @section What the table shows:
#' **Coefficient rows.** `B` is the posterior median, `SE` the posterior
#' MAD SD, and the interval an equal-tailed credible interval (header
#' `95% CrI`). See *Posterior summaries and intervals*.
#'
#' **Random-effects rows.** Multilevel fits (`stan_glmer`, `brm` with
#' grouping terms) report their random effects as a block: the posterior
#' median SD and credible interval of each component, from the draws, with
#' no likelihood-ratio line in the note.
#'
#' **AME.** Draws-native: [marginaleffects::avg_slopes()] computes the
#' marginal effects per posterior draw, and the table reports their
#' posterior median, MAD SD and credible interval (equal-tailed or HDI, as
#' `ci_method` says).
#'
#' **Fit statistics.** By default, `n` and the Bayesian R-squared; see
#' *Fit statistics*.
#'
#' @section Values of the shared arguments:
#' \describe{
#'   \item{`ci_method`}{The default gives the equal-tailed credible
#'     interval. `"hdi"` (Bayesian fits only) gives the highest-density
#'     interval instead, the shortest interval that holds `ci_level` of the
#'     posterior draws (Kruschke 2015), with the header `95% HDI`. Unlike
#'     the equal-tailed interval the HDI is not transformation-invariant,
#'     so under `exponentiate = TRUE` it is recomputed on the exponentiated
#'     draws rather than transformed. `"profile"` and `"boot_percentile"`
#'     are refused: a posterior has no profile likelihood and no bootstrap
#'     replicates.}
#'   \item{`standardized`}{`"posthoc"`, `"basic"` and `"smart"`, for
#'     fixed-effects fits: `stan_glm`-style `stanreg` models and
#'     standard-formula `brm()` models, whose design matrix is recovered
#'     through \pkg{insight}. They are exact affine rescales of the
#'     link-scale summaries: the median, MAD SD and credible bounds all
#'     scale by the same positive SD ratio, and the beta stays on the link
#'     scale under `exponentiate = TRUE`. The Gaussian family divides by
#'     SD(y); every other family standardizes the predictors only, the
#'     frequentist `glm` convention. The scale factors do not depend on the
#'     engine, so the same model fitted with rstanarm or brms gets the same
#'     betas up to sampling noise. See *What is refused, and why* for the
#'     other cases.}
#'   \item{`exponentiate`}{All quantities are computed from the
#'     exponentiated draws directly: the SE is the posterior MAD SD on the
#'     ratio scale, not a delta-method approximation.}
#'   \item{`show_columns`}{The group presets (`"all_b"`, ...) expand
#'     without the p-value and t columns. `"pd"` adds the probability of
#'     direction (Makowski et al. 2019), defined in the note. `"rhat"`,
#'     `"ess_bulk"`, `"ess_tail"` and `"mcse"` add per-coefficient
#'     diagnostics in an all-Bayesian table; see *Diagnostics*. `"p"` and
#'     `"ame_p"` have nothing to fill: refused as atomic tokens, dashed in
#'     a table that mixes Bayesian and other fits.}
#'   \item{`show_fit_stats`}{`"r2_bayes"`, `"elpd_loo"`, `"looic"` and
#'     `"waic"`. See *Fit statistics*.}
#'   \item{`vcov`, `cluster`}{`"classical"` only: the displayed
#'     uncertainty comes from the posterior draws. Model the clustering
#'     with group-level terms instead.}
#' }
#'
#' @section Posterior summaries and intervals:
#' `B` is the posterior median and `SE` the posterior MAD SD, the scaled
#' median absolute deviation: the pairing of Gelman, Hill & Vehtari (2020)
#' and of rstanarm's own print, equal to the posterior SD for a normal
#' posterior. The interval is an equal-tailed credible interval
#' (`95% CrI`), or the highest-density interval under `ci_method = "hdi"`
#' (`95% HDI`). There is no p-value and no t-statistic, and so no stars
#' and no `p_adjust`. The probability of direction, `"pd"`, is the opt-in
#' posterior summary closest to them.
#'
#' The AME and the standardized betas follow the same logic: each is
#' summarized from its draws, by the median, the MAD SD and the credible
#' interval of `ci_method`.
#'
#' @section Diagnostics:
#' Every table is backed by an automatic check of the sampler: R-hat >=
#' 1.01, ESS below 100 per chain, floored at 400 so that fewer chains
#' never weaken the bar, divergent transitions, E-BFMI < 0.2. The R-hat
#' and ESS bars of 1.01 and 400 are those of Vehtari et al. (2021,
#' pp. 671-672). A problem adds a line to the note and raises a warning of
#' class `spicy_bayes_diagnostics`, nested under `spicy_caveat`, so
#' `withCallingHandlers(spicy_bayes_diagnostics = ...)` mutes the check
#' alone. A clean fit prints nothing.
#'
#' The columns `"rhat"`, `"ess_bulk"` and `"ess_tail"` report the
#' diagnostics of each coefficient in an all-Bayesian table, as does
#' `"mcse"`: the Monte Carlo standard error of the displayed posterior
#' median. It tells how many digits a table can show: a displayed digit is
#' Monte-Carlo stable when twice the MCSE stays below it (Gelman, Vehtari
#' & McElreath 2026, Section 11.6). Under `exponentiate = TRUE` the MCSE
#' is recomputed on the exponentiated draws.
#'
#' Only MCMC fits are read: a fit with `algorithm = "meanfield"` or
#' `"optimizing"` is refused; refit with `algorithm = "sampling"`.
#'
#' @section Fit statistics:
#' \itemize{
#'   \item `"r2_bayes"`: the posterior-median Bayesian R-squared (Gelman et
#'     al. 2019), shown by default with `n`.
#'   \item `"elpd_loo"` and `"looic"`: the PSIS-LOO expected log
#'     predictive density and its deviance-scale twin (Vehtari et al.
#'     2017), on request, a few seconds per model. The note gives the
#'     standard error.
#'   \item `"waic"`: the Watanabe-Akaike criterion, on request; PSIS-LOO
#'     is generally preferred.
#' }
#' An unreliable estimate adds a caveat to the note instead of being
#' silenced: a PSIS-LOO Pareto k above the sample-size-specific threshold
#' \eqn{\min(1 - 1/\log_{10} S, 0.7)}{min(1 - 1/log10(S), 0.7)} of Vehtari
#' et al. 2024, the bound `loo::loo()` prints, or a WAIC p_waic above 0.4.
#' Compare models with `loo::loo_compare()` outside the table.
#'
#' @section What is refused, and why:
#' Each refusal is a classed error.
#' \itemize{
#'   \item Likelihood-based fit statistics (`"aic"`, the pseudo-R-squared
#'     rows, ...): a posterior has no likelihood-based information
#'     criterion.
#'   \item `p_adjust`, and the `"p"` column: there are no p-values.
#'   \item A robust or cluster `vcov`: a posterior has no sandwich analog.
#'     Model the clustering or the heteroskedasticity instead
#'     (`rstanarm::stan_glmer()`, brms group-level or distributional
#'     terms).
#'   \item `ci_method = "profile"` and `"boot_percentile"`.
#'   \item `nested = TRUE`.
#'   \item `standardized = "refit"`, which would re-run the sampler, and
#'     `"pseudo"`. Also refused: multilevel fits (`stan_glmer`, `stan_lmer`
#'     and `brm()` with group-level terms), whose beta would need an
#'     explicit decomposition of SD(y); the `stanreg` subclasses that are
#'     not GLMs (`stan_polr`, `stan_betareg`); and brms formulas with
#'     distributional, multivariate or special terms (`mo()`, `s()`, ...).
#'     In these cases, standardize the predictors before fitting.
#'   \item Non-MCMC fits (see *Diagnostics*).
#' }
#'
#' @seealso [table_regression()] for the arguments shared by all classes;
#'   [table_regression_models()] for the registry of supported classes and
#'   the other families; the
#'   [Bayesian regression tables](https://amaltawfik.github.io/spicy/articles/table-regression-bayesian.html)
#'   article.
#'
#' @references
#' Gelman, A., Goodrich, B., Gabry, J. & Vehtari, A. (2019). R-squared
#' for Bayesian regression models. *The American Statistician*, 73(3),
#' 307-309.
#'
#' Gelman, A., Hill, J. & Vehtari, A. (2020). *Regression and other
#' stories*. Cambridge University Press. \doi{10.1017/9781139161879}
#'
#' Gelman, A., Vehtari, A. & McElreath, R. (2026). *Bayesian workflow*.
#' Chapman and Hall/CRC.
#'
#' Kruschke, J.K. (2015). *Doing Bayesian data analysis: A tutorial with
#' R, JAGS, and Stan* (2nd ed.). Academic Press.
#'
#' Makowski, D., Ben-Shachar, M.S., Chen, S.H.A. & Lüdecke, D. (2019).
#' Indices of effect existence and significance in the Bayesian
#' framework. *Frontiers in Psychology*, 10, 2767.
#' \doi{10.3389/fpsyg.2019.02767}
#'
#' Vehtari, A., Gelman, A. & Gabry, J. (2017). Practical Bayesian model
#' evaluation using leave-one-out cross-validation and WAIC. *Statistics
#' and Computing*, 27(5), 1413-1432. \doi{10.1007/s11222-016-9696-4}
#'
#' Vehtari, A., Gelman, A., Simpson, D., Carpenter, B. & Bürkner, P.-C.
#' (2021). Rank-normalization, folding, and localization: An improved
#' R-hat for assessing convergence of MCMC. *Bayesian Analysis*, 16(2),
#' 667-718. \doi{10.1214/20-BA1221}
#'
#' Vehtari, A., Simpson, D., Gelman, A., Yao, Y. & Gabry, J. (2024).
#' Pareto smoothed importance sampling. *Journal of Machine Learning
#' Research*, 25(72), 1-58.
#'
#' @examples
#' # rstanarm samples the posterior, which takes a few seconds. The
#' # example runs in an interactive session only, so that the package
#' # checks never run a sampler.
#' if (interactive() && requireNamespace("rstanarm", quietly = TRUE)) {
#'   fit <- rstanarm::stan_glm(mpg ~ wt + hp, data = mtcars,
#'                             refresh = 0, seed = 1)
#'   print(table_regression(fit))
#'
#'   # Highest-density intervals and the probability of direction.
#'   print(table_regression(fit, ci_method = "hdi",
#'                          show_columns = c("b", "se", "ci", "pd")))
#' }
#'
#' @name table_regression_bayesian
NULL
