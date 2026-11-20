#' Mixed-effects models in table_regression()
#'
#' @description
#' [table_regression()] reads the mixed-effects fits of `lme4::lmer()`,
#' `lme4::glmer()`, `glmmTMB::glmmTMB()` and `nlme::lme()`, and the
#' generalized least squares fits of `nlme::gls()`. The fixed effects fill
#' the coefficient rows. The variance components follow them as a
#' `Random effects` block of rows, in the same columns. The number of
#' groups, the ICC and the marginal and conditional R-squared of Nakagawa
#' are fit-statistic rows, and the note gives the likelihood-ratio test of
#' the whole random part. Cumulative-link mixed models (`ordinal::clmm()`,
#' and `glmmTMB()` with `family = glmmTMB::ordinal()`) get the same block
#' of rows; their other rules are those of the ordinal models, in
#' [table_regression_ordinal].
#'
#' @section Usage:
#' `r spicy:::.render_family_usage("Mixed effects")`
#'
#' The other arguments are documented in [table_regression()]; the values
#' they take for mixed models are listed below.
#'
#' @section Engines:
#' `r spicy:::.render_supported_models_md("Mixed effects")`
#'
#' AME: whether the average-marginal-effects columns are available.
#' Exponentiate: what `exponentiate = TRUE` gives. Blocks: the labeled
#' blocks of rows the class adds below its coefficients. vcov: the values
#' of `vcov` the class accepts. The same rows are
#' returned by `subset(table_regression_models(), family == "Mixed effects")`.
#'
#' @section What the table shows:
#' **Coefficient rows.** One row per fixed-effect coefficient, as for
#' `lm` and `glm`. Under `exponentiate = TRUE` a `glmer` or `glmmTMB` fit
#' follows the link rules of `glm`: odds ratios under the logit link, rate
#' ratios under the log link, an error for a link whose exponential is not
#' a ratio. The rows are conditional on a random effect of zero: an
#' exponentiated intercept is the baseline odds or rate of a group whose
#' random effect is zero, not the average over groups.
#'
#' **Random-effects rows.** One row per standard deviation and per
#' correlation of each grouping factor, then the residual of a Gaussian
#' model, each with its estimate, SE and CI in the coefficient columns. The
#' rows of a `glmer` or `glmmTMB` fit are on the scale of the linear
#' predictor (log-odds, log-rate). The rows carry no p-value: a Wald test
#' of a variance is invalid at the boundary of its parameter space (Self &
#' Liang 1987), and `re_test` adds a test that is valid there. The note
#' gives the estimation method (REML or ML) and the likelihood-ratio test
#' of the whole random part against the model without random effects, with
#' its boundary-corrected chi-bar-squared p-value (Self & Liang 1987; Stram
#' & Lee 1994). For an `lme` fit the null model keeps the residual
#' variance and correlation structure of the fit. A `gls` fit has no random
#' effects and no block. `glmmTMB` zero-inflation, hurdle and dispersion
#' components render as further blocks, described in
#' [table_regression_counts] (`show_components`).
#'
#' In `broom::tidy()` the variance-component rows have
#' `estimate_type = "vc"`, their terms are prefixed `re::`, and the
#' correlations carry a `::cor` suffix.
#'
#' **Fit statistics.** By default, `n`, one `N (<group>)` row per grouping
#' factor (`"n_groups"`), the `ICC`, the marginal and conditional
#' R-squared, `AIC` and `BIC`; see *Fit statistics* below.
#'
#' @section Arguments for this family:
#' These arguments act on the random-effects rows only. They have no
#' effect on fits without random effects.
#'
#' \describe{
#'   \item{`show_re`}{Logical. `TRUE` (default) renders the
#'     `Random effects` block for `lmer`, `glmer`, `glmmTMB`, `lme` and
#'     `clmm` fits. `FALSE` suppresses it. `re_test` and
#'     `re_ci = "profile"` need the block and are refused with
#'     `show_re = FALSE`.}
#'   \item{`re_scale`}{`"sd"` (default) or `"variance"`. `"sd"` reports
#'     the standard deviation \eqn{\sigma}{sigma}, in the units of the
#'     linear predictor (those of the outcome for a Gaussian model).
#'     `"variance"` reports \eqn{\sigma^2}{sigma^2}, in squared units.
#'     The standard error is a delta-method one on the scale the engine
#'     estimates on, and
#'     \eqn{SE(\sigma) = SE(\sigma^2) / (2\sigma)}{SE(sigma) = SE(sigma^2) / (2 sigma)}.
#'     For `lmer` and `glmer` it comes from the numerical Hessian of the
#'     deviance (the REML criterion for a REML fit) in the standard
#'     deviations and correlations, i.e. the observed information; for a
#'     `glmer` fit spicy uses the Hessian lme4 stores at the optimum when
#'     there is one. The prior weights of an `lmer` fit are taken into
#'     account. A negative-binomial `glmer`, a `glmer` family with a
#'     scale parameter and an lme4 covariance structure other than the
#'     unstructured one keep their rows without SE or CI, and a note under
#'     the table says why. So does a Hessian that is not positive definite.
#'     For `lme`, `glmmTMB` and `clmm` the engine estimates \eqn{\log\sigma}{log(sigma)} and the SE is
#'     \eqn{\sigma \, SE(\log\sigma)}{sigma SE(log(sigma))}. The Wald CI of
#'     every engine is built on \eqn{\log\sigma}{log(sigma)},
#'     \eqn{\sigma \exp(\pm z \, SE(\log\sigma))}{sigma exp(+/- z SE(log(sigma)))},
#'     and squared for \eqn{\sigma^2}{sigma^2}: by spicy for `lmer` and
#'     `glmer`, by the engine otherwise (`nlme::intervals()`,
#'     `glmmTMB::confint()`). The residual row of a Gaussian `glmmTMB` fit
#'     has its SE and CI too. Correlation rows (\eqn{\rho}{rho}) have no unit and are unchanged.
#'     For `lmer` and `glmer` their SE comes from the same Hessian, and
#'     the CI is the Wald interval on
#'     \eqn{\mathrm{atanh}(\rho)}{atanh(rho)},
#'     \eqn{\tanh(\mathrm{atanh}(\rho) \pm z \, SE / (1 - \rho^2))}{tanh(atanh(rho) +/- z SE / (1 - rho^2))},
#'     which stays inside \eqn{(-1, 1)}{(-1, 1)}. For the other engines
#'     the SE is a delta-method one: from
#'     \eqn{\mathrm{atanh}(\rho)}{atanh(rho)} for `lme`, and from glmmTMB's
#'     \eqn{\rho / \sqrt{1 - \rho^2}}{rho / sqrt(1 - rho^2)} in a two-term
#'     block, with the engine's CI. In a `glmmTMB` block of three terms or
#'     more it is the width of glmmTMB's interval divided by \eqn{2z}{2z}.
#'     nlme computes the covariance matrix behind the SE of an `lme` fit
#'     by finite differences. With nlme's default step and a
#'     random-effect correlation close to zero, the SE can be off by
#'     several percent and differ from one platform to another, or the
#'     matrix is not positive definite and the rows have no SE and no CI,
#'     which a note under the table says.
#'     A larger step, `control = nlme::lmeControl(.relStep = 1e-4)`,
#'     gives a stable matrix. spicy displays what nlme estimates.}
#'   \item{`re_columns`}{Subset of `c("est", "se", "ci")`: the cells of the
#'     random-effects rows that are displayed (`"est"` is mandatory).
#'     Deselected cells render as an en dash on those rows only:
#'     `re_columns = "est"` for estimates alone, `c("est", "se")` for a
#'     journal that wants standard errors only. Display only:
#'     `broom::tidy()` and [as_structured()] keep the SE and CI.}
#'   \item{`re_test`}{`"none"` (default), `"lrt"` or `"rlrt"`. A per-term
#'     test of the variance components, which fills the p column of the
#'     random-effects rows. Both are likelihood-ratio tests; a Wald test of
#'     a variance is refused (see *What is refused, and why*).
#'     \itemize{
#'       \item `"lrt"`: likelihood-ratio test of each random term against
#'         the model refitted without it (its variance and its covariances
#'         with the other terms of its bar), referred to the chi-bar-squared
#'         mixture `0.5 chi2(q-1) + 0.5 chi2(q)` (Self & Liang 1987; Stram
#'         & Lee 1994). The reduction scheme is that of
#'         `lmerTest::ranova()`, and the mixture makes the p-value
#'         asymptotically exact instead of conservative. The intercept of a
#'         bar is tested only when it is the only term of the bar.
#'         Available for `lmer`, `glmer`, `glmmTMB`, and `lme` with a simple
#'         `random = ~ terms | group` structure.
#'       \item `"rlrt"`: exact restricted likelihood-ratio test with a
#'         simulated finite-sample null (`RLRsim::exactRLRT()`; Crainiceanu
#'         & Ruppert 2004). Defined only for a Gaussian `lmer` or `lme` fit
#'         with a single variance component; other fits are refused with a
#'         pointer to `"lrt"`.
#'     }
#'     The statistic and its df stay out of the t/z column (they are on the
#'     chi-square scale) and are kept in `broom::tidy()` (`test_type`
#'     `"chibar2"` or `"rlrt"`). The test of the whole block in the note is
#'     unchanged. Correlation and residual rows are never tested. Each
#'     random term costs one refit; a refit that fails leaves its row
#'     untested, with a `spicy_fallback` warning. For other structures,
#'     `pbkrtest::PBmodcomp()` gives a parametric-bootstrap test of the
#'     same pair of fits (Halekoh & Højsgaard 2014), to report beside the
#'     table.}
#'   \item{`re_ci`}{`"wald"` (default) or `"profile"`: the uncertainty of
#'     the random-effects rows of `lmer` and `glmer` fits.
#'     \itemize{
#'       \item `"wald"`: SE from the observed information (the Hessian
#'         of the deviance, see `re_scale`) and a Wald CI symmetric on the
#'         log of the standard deviation, which stays above 0, and on
#'         \eqn{\mathrm{atanh}(\rho)}{atanh(rho)} for a correlation. Wald
#'         can be optimistic near the boundary of a variance; `"profile"`
#'         respects it. The Hessian takes a few dozen evaluations of the
#'         deviance: under a second for an `lmer` fit of 20,000
#'         observations. A `glmer` fit above 10,000 observations has no
#'         Hessian stored by lme4, and its computation then takes about a
#'         quarter of the time of the fit.
#'       \item `"profile"`: profile-likelihood intervals from
#'         `confint(fit, method = "profile")`, the route lme4 documents.
#'         They are asymmetric and respect the boundary at 0. No SE is
#'         shown, since a symmetric SE misdescribes the skewed sampling
#'         distribution of a variance. It takes longer than `"wald"`
#'         (about two seconds per variance parameter at n of 7,000; more
#'         for `glmer`). lme4 profiles the ML deviance, also for a REML
#'         fit: the interval of a REML fit is that of its ML refit, and
#'         the note says so. The note names the method, and
#'         `re_scale = "variance"` shows the profile interval of the
#'         variance itself. A negative-binomial `glmer`, which has no Wald
#'         SE, gets its profile interval this way. lme4 does not profile a
#'         `glmer` whose scale is estimated (Gamma, inverse Gaussian): its
#'         rows keep no interval and a warning says so.
#'     }
#'     `glmmTMB`, `lme` and `clmm` fits keep the intervals of their own
#'     engine under `"wald"` (see `re_scale`) and refuse `"profile"`.}
#' }
#'
#' The arguments shared with other classes take these values for mixed
#' models:
#' \describe{
#'   \item{`vcov`, `cluster`}{`lmer` and `lme`: `"classical"` and `"CR0"`
#'     to `"CR3"`. `glmmTMB`: `"classical"`, `"CR0"` and `"CR1"`. `glmer`
#'     and `gls`: `"classical"` only. See *Standard errors, degrees of
#'     freedom and intervals*.}
#'   \item{`ci_method`}{`"wald"` only. `"profile"` is defined for `glm` and
#'     the ordinal models, and `"boot_percentile"` needs
#'     `vcov = "bootstrap"`, which mixed models refuse. The intervals of
#'     the random-effects rows are set by `re_ci`.}
#'   \item{`standardized`}{`"refit"`: the model is refitted on z-scored
#'     data with its random structure, the one approach that is well posed
#'     for a mixed model, since the marginal SD(y) has no unique
#'     decomposition across levels. `"posthoc"`, `"basic"`, `"smart"` and
#'     `"pseudo"` fall back to `"refit"` with a `spicy_fallback` warning.
#'     The \eqn{\beta}{beta} rows take the reference distribution of their B
#'     rows, so B and \eqn{\beta}{beta} carry the same p-value, and a robust
#'     `vcov` is applied to the refit too. A formula with an inline
#'     transform (`log(x)`, `poly()`) cannot be refitted: the
#'     \eqn{\beta}{beta} rows are left out, with a warning; build the
#'     transformed column in `data` before fitting. Refused for `glmmTMB`
#'     with the ordinal family, whose outcome has no scale.}
#'   \item{`show_columns`}{`"partial_chi2"` is the Wald chi-square of each
#'     term, Type II by default and Type III under `partial_type = "III"`
#'     (the type SAS PROC MIXED and `lmerTest` report; the two agree for a
#'     model without interactions). It follows the robust `vcov` when one
#'     is set. `"partial_f"` and the variance-explained partials
#'     (`"partial_f2"`, `"partial_eta2"`, `"partial_omega2"`) are not
#'     defined. The AME tokens are available for every engine of the table
#'     above except `glmmTMB` with the ordinal family or fitted by REML,
#'     whose fixed effects the AME cannot perturb; no column token is
#'     reserved to mixed models.}
#'   \item{`show_fit_stats`}{Four tokens belong to this family:
#'     `"n_groups"` (also used by GEE and `fixest` fits), `"icc"`,
#'     `"r2_marginal"` and `"r2_conditional"`. See *Fit statistics*.}
#' }
#'
#' @section Standard errors, degrees of freedom and intervals:
#' The fixed effects are tested as each engine tests them:
#' \itemize{
#'   \item A fit of `lmerTest::lmer()`: t with Satterthwaite degrees of
#'     freedom.
#'   \item A plain `lme4::lmer()` fit: Wald z. A message, once per
#'     session, suggests refitting with `lmerTest::lmer()`; the note names
#'     the method only.
#'   \item `glmer` and `glmmTMB`: Wald z.
#'   \item `lme`: t with the containment degrees of freedom of `nlme`,
#'     named in the note. `gls`: t on n minus the number of coefficients.
#' }
#' `show_columns = "df"` prints the degrees of freedom of each t test in
#' a column, blank for a z test.
#' The confidence interval uses the same reference distribution as the
#' test. The AME rows take the variance matrix of the coefficient rows,
#' model-based or robust, and are tested by Wald z. Beside the t rows of
#' an `lmerTest` or `lme` fit, or of a `CR*` variance, an identity-link
#' AME can therefore carry another p-value than the B of the same row;
#' read it as a large-sample approximation.
#'
#' For a mixed model with a non-identity link, the AME averages the slopes
#' of the conditional predictions: each observation is predicted with the
#' estimated random effects of its groups. It is neither the effect for a
#' typical group (random effects at zero) nor the population-averaged
#' effect, which integrates over the distribution of the random effects.
#' In a linear mixed model with random intercepts only, the three are
#' equal.
#'
#' Cluster-robust variance. `lmer` and `lme` fits take `"CR0"` to `"CR3"`
#' from [clubSandwich::vcovCR()], with Satterthwaite degrees of freedom
#' computed from the robust matrix. An `lme` fitted with other contrasts
#' than those of the session is refused: \pkg{clubSandwich} rebuilds its
#' design with the session contrasts, so its variance would not match the
#' coefficients. `glmmTMB` fits take `"CR0"` and `"CR1"` from
#' glmmTMB's own sandwich (\pkg{glmmTMB} >= 1.1.15), which covers the
#' coefficients, the zero-inflation and dispersion blocks and the AME.
#' `"CR0"` is the sandwich as glmmTMB computes it, with one score per
#' cluster (the gradient of the marginal log-likelihood of the cluster);
#' `"CR1"` scales it by \eqn{G/(G-1)}{G/(G-1)}, `G` the number of
#' clusters. Both use Wald z. Without `cluster`, the cluster is the
#' top-level grouping factor of the random effects. The estimator needs
#' independent clusters that contain the random effects, every group
#' within one cluster, so crossed random effects need a `cluster` that
#' contains them all. It keeps the standard errors valid when the
#' random-effects distribution or the conditional variance is
#' misspecified, provided the estimates stay consistent (Liang & Zeger
#' 1986 for a misspecified variance and correlation, Verbeke & Lesaffre
#' 1997 for the random-effects distribution). With few clusters it understates the
#' variance, which the factor of `"CR1"` offsets only in part: this class
#' has no bias-reduced correction (Cameron & Miller 2015).
#'
#' The robust estimator covers the fixed effects: their coefficients,
#' their joint tests (`"partial_chi2"`), the standardized coefficients and
#' the AME. The variance components keep their model-based inference.
#' glmmTMB's sandwich takes one likelihood score per cluster over the
#' fixed and variance parameters, while the `CR*` estimators of `lmer` and
#' `lme` come from \pkg{clubSandwich} over the fixed effects alone: on the
#' same model the two agree as the clusters grow, but not exactly.
#'
#' Hierarchical comparison (`nested = TRUE`). The default change rows are
#' `c("aic_change", "bic_change", "lrt_change", "p_change")` (Pinheiro &
#' Bates 2000, Section 2.4.1), from the engine's own two-model `anova()`.
#' Under `"CR0"` to `"CR3"`, `lmer` and `lme` hierarchies use the HTZ
#' test of `clubSandwich::Wald_test()`; under `"CR0"` and `"CR1"`, a
#' `glmmTMB` hierarchy uses a Wald chi-square. See *Hierarchical (nested)
#' model comparison* in [table_regression()].
#'
#' @section Fit statistics:
#' By default, `r spicy:::.render_fit_stats_default("lmerMod")`, for
#' `lmer`, `glmer`, `glmmTMB`, `lme` and `clmm` fits. A row of the default that no model of the table fills
#' is left out. A token named in the call that no model fills is refused.
#' A
#' `gls` fit gets `n` and `AIC`.
#' \itemize{
#'   \item `"n_groups"`: one `N (<group>)` row per grouping factor.
#'   \item `"icc"`: the intraclass correlation of a model with one random
#'     intercept and no other random term:
#'     \eqn{\sigma^2_u / (\sigma^2_u + \sigma^2_e)}{var_u / (var_u + var_e)}.
#'     For a non-Gaussian `glmer` fit the residual variance is the
#'     distribution variance of the link scale (the adjusted ICC of
#'     Nakagawa, Johnson & Schielzeth 2017): \eqn{\pi^2/3}{pi^2/3} for the
#'     logit, 1 for the probit, \eqn{\pi^2/6}{pi^2/6} for the cloglog, and
#'     \eqn{\log(1 + 1/\lambda)}{log(1 + 1/lambda)} for a Poisson log fit.
#'     A binomial `cbind()` response, whose rows are not single trials, has
#'     no ICC. For `glmmTMB` the ICC is computed for the Gaussian identity
#'     family only. A model with random slopes has no ICC: its cell is
#'     empty. With several
#'     grouping factors there is no single ICC, and the note says so.
#'   \item `"r2_marginal"`, `"r2_conditional"`: the R-squared of Nakagawa &
#'     Schielzeth (2013), the variance explained by the fixed effects
#'     alone and by the fixed and random effects together; the values
#'     match `performance::r2_nakagawa()`. Empty for `gls`.
#'   \item On request: `"deviance"`, `"log_lik"` (the restricted
#'     log-likelihood of a REML fit), `"sigma"` (see
#'     [table_regression_tokens] for its `glmmTMB` families), and the dispersion
#'     rows of a negative-binomial or beta family (`"theta"`, `"alpha"`,
#'     `"delta"`, `"phi"`).
#'   \item Refused when every model of the table is mixed: `"r2"`,
#'     `"adj_r2"`, `"omega2"`, `"f2"`, the pseudo-R-squared tokens and
#'     `"r2_deviance"`. They have no definition for a mixed model; the
#'     error points to `"r2_marginal"` and `"r2_conditional"`.
#' }
#'
#' @section What is refused, and why:
#' \itemize{
#'   \item `HC*`, `"bootstrap"` and `"jackknife"` for every mixed engine:
#'     these estimators assume independent observations or refit `lm` and
#'     `glm`, and the random effects exist to model the dependence.
#'   \item `CR*` for `glmer` (\pkg{clubSandwich} has no working backend
#'     for it), and `"CR2"` and `"CR3"` for `glmmTMB` (its sandwich has one
#'     score per cluster, with no residuals and no hat values). A robust
#'     `vcov` for a `glmmTMB` fit by REML or with the ordinal family.
#'   \item A Wald test of a variance component, on any engine: the null
#'     value 0 lies on the boundary of the parameter space.
#'   \item `re_ci = "profile"` outside lme4, and `re_test = "rlrt"`
#'     outside a Gaussian fit with one variance component.
#'   \item Under `nested = TRUE`:
#'     \itemize{
#'       \item A REML pair whose fixed effects differ
#'         (`nlme::lme(method = "REML")`, `glmmTMB(REML = TRUE)`). The
#'         restricted likelihood depends on the fixed-effects design, so
#'         the likelihood-ratio test is not valid (Pinheiro & Bates 2000,
#'         Section 2.4.2). Refit both by maximum likelihood. A REML pair
#'         that differs only in its random structure stays valid.
#'       \item Fits from different engines. No engine's `anova()` accepts
#'         a fit of another; refit one with the other's engine.
#'       \item The variance-explained change tokens (`"r2_change"`,
#'         `"adj_r2_change"`, `"f_change"`, `"f2_change"`). The comparison
#'         is a chi-square.
#'     }
#'     `nested = FALSE` sets the models side by side instead.
#' }
#'
#' @seealso [table_regression()] for the arguments shared by all classes;
#'   [table_regression_models()] for the registry of supported classes and
#'   the other families; [table_regression_ordinal] for the thresholds
#'   of `clmm` and of the ordinal family of `glmmTMB`;
#'   [table_regression_counts] for the zero-inflation, hurdle and
#'   dispersion blocks of `glmmTMB`; the
#'   [Mixed-effects regression tables](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.html)
#'   article.
#'
#' @references
#' Cameron, A.C. & Miller, D.L. (2015). A practitioner's guide to
#' cluster-robust inference. *Journal of Human Resources*, 50(2),
#' 317-372.
#'
#' Crainiceanu, C.M. & Ruppert, D. (2004). Likelihood ratio tests in
#' linear mixed models with one variance component. *Journal of the Royal
#' Statistical Society B*, 66(1), 165-185.
#'
#' Halekoh, U. & Højsgaard, S. (2014). A Kenward-Roger approximation and
#' parametric bootstrap methods for tests in linear mixed models: The R
#' package pbkrtest. *Journal of Statistical Software*, 59(9).
#' \doi{10.18637/jss.v059.i09}
#'
#' Liang, K.-Y. & Zeger, S.L. (1986). Longitudinal data analysis
#' using generalized linear models. *Biometrika*, 73(1), 13-22.
#'
#' Nakagawa, S. & Schielzeth, H. (2013). A general and simple method for
#' obtaining R2 from generalized linear mixed-effects models. *Methods in
#' Ecology and Evolution*, 4(2), 133-142.
#'
#' Nakagawa, S., Johnson, P.C.D. & Schielzeth, H. (2017). The coefficient
#' of determination R2 and intra-class correlation coefficient from
#' generalized linear mixed-effects models revisited and expanded.
#' *Journal of the Royal Society Interface*, 14(134), 20170213.
#' \doi{10.1098/rsif.2017.0213}
#'
#' Pinheiro, J.C. & Bates, D.M. (2000). *Mixed-effects models in S and
#' S-PLUS*. Springer.
#'
#' Self, S.G. & Liang, K.-Y. (1987). Asymptotic properties of maximum
#' likelihood estimators and likelihood ratio tests under nonstandard
#' conditions. *Journal of the American Statistical Association*, 82(398),
#' 605-610.
#'
#' Stram, D.O. & Lee, J.W. (1994). Variance components testing in the
#' longitudinal mixed effects model. *Biometrics*, 50(4), 1171-1177.
#'
#' Verbeke, G. & Lesaffre, E. (1997). The effect of misspecifying
#' the random-effects distribution in linear mixed models for
#' longitudinal data. *Computational Statistics & Data Analysis*,
#' 23(4), 541-556.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   fit <- lme4::lmer(Reaction ~ Days + (Days | Subject),
#'                     data = lme4::sleepstudy)
#'   print(table_regression(fit))
#'
#'   # Variances instead of standard deviations, estimates only.
#'   print(table_regression(fit, re_scale = "variance", re_columns = "est"))
#'
#'   # Nested comparison, fitted by maximum likelihood.
#'   m1 <- lme4::lmer(Reaction ~ 1 + (1 | Subject),
#'                    data = lme4::sleepstudy, REML = FALSE)
#'   m2 <- lme4::lmer(Reaction ~ Days + (1 | Subject),
#'                    data = lme4::sleepstudy, REML = FALSE)
#'   print(table_regression(list(m1, m2), nested = TRUE))
#' }
#' }
#'
#' @name table_regression_mixed
NULL
