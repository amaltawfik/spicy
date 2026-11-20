# the refusals of the vcov sweep keep their text and class

    Code
      cat(.refusal_render(rows), sep = "\n")
    Output
      ----
      error: spicy_invalid_input, spicy_error
        Cluster-robust SE for an rms `Glm` fit need the model matrices.
        i Refit with `x = TRUE, y = TRUE` (e.g. `Glm(..., x = TRUE, y = TRUE)`) so rms::robcov() can form the sandwich.
      Received by 2:
        vcov = "CR0": rms_Glm, rms_Glm_logit
      ----
      error: spicy_invalid_input, spicy_error
        Cluster-robust SE for an rms `cph` fit need the model matrices.
        i Refit with `x = TRUE, y = TRUE` (e.g. `cph(..., x = TRUE, y = TRUE)`) so rms::robcov() can form the sandwich.
      Received by 1:
        vcov = "CR0": cph
      ----
      error: spicy_invalid_input, spicy_error
        Cluster-robust SE for an rms `lrm` fit need the model matrices.
        i Refit with `x = TRUE, y = TRUE` (e.g. `lrm(..., x = TRUE, y = TRUE)`) so rms::robcov() can form the sandwich.
      Received by 2:
        vcov = "CR0": lrm, lrm_ordinal
      ----
      error: spicy_invalid_input, spicy_error
        Cluster-robust SE for an rms `ols` fit need the model matrices.
        i Refit with `x = TRUE, y = TRUE` (e.g. `ols(..., x = TRUE, y = TRUE)`) so rms::robcov() can form the sandwich.
      Received by 1:
        vcov = "CR0": ols
      ----
      error: spicy_invalid_input, spicy_error
        `vcov = "CR0"` is not available for this `lme` fit.
        x The fit uses other contrasts than the session default, and `clubSandwich::vcovCR()` rebuilds its design with the default ones: the variance would not match the coefficients.
        i Refit with the default contrasts, or set the same contrasts with `options(contrasts = )` before calling.
      Received by 1:
        vcov = "CR0": lme_contrasts
      ----
      error: spicy_invalid_input, spicy_error
        `vcov = "CR1"` is not available for this `lme` fit.
        x The fit uses other contrasts than the session default, and `clubSandwich::vcovCR()` rebuilds its design with the default ones: the variance would not match the coefficients.
        i Refit with the default contrasts, or set the same contrasts with `options(contrasts = )` before calling.
      Received by 1:
        vcov = "CR1": lme_contrasts
      ----
      error: spicy_invalid_input, spicy_error
        `vcov = "CR2"` is not available for this `lme` fit.
        x The fit uses other contrasts than the session default, and `clubSandwich::vcovCR()` rebuilds its design with the default ones: the variance would not match the coefficients.
        i Refit with the default contrasts, or set the same contrasts with `options(contrasts = )` before calling.
      Received by 1:
        vcov = "CR2": lme_contrasts
      ----
      error: spicy_invalid_input, spicy_error
        `vcov = "CR3"` is not available for this `lme` fit.
        x The fit uses other contrasts than the session default, and `clubSandwich::vcovCR()` rebuilds its design with the default ones: the variance would not match the coefficients.
        i Refit with the default contrasts, or set the same contrasts with `options(contrasts = )` before calling.
      Received by 1:
        vcov = "CR3": lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `bam` models.
        i spicy computes one cluster-robust estimator for mgcv fits, from their GLM score and penalized covariance: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR0": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `betareg` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR0": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `clm` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR0": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "CR0": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "CR0": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "CR0": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `gam` models.
        i spicy computes one cluster-robust estimator for mgcv fits, from their GLM score and penalized covariance: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 2:
        vcov = "CR0": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "CR0": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "CR0": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "CR0": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `hurdle` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR0": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "CR0": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "CR0": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `mlogit` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR0": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `multinom` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR0": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR0": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `polr` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR0": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR0": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `rq` models.
        i There is no cluster-robust sandwich for quantile regression: clubSandwich has no method for `rq`, and no estimating functions are available to build one.
        i Use `vcov = "bootstrap"` with `cluster =`: the wild gradient cluster bootstrap, the route available for clustered data.
      Received by 1:
        vcov = "CR0": rq
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR0": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `survreg` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 2:
        vcov = "CR0": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "CR0": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "CR0": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "CR0": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `tobit` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR0": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for `zeroinfl` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR0": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "CR0": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for a negative-binomial `hurdle` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR0": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for a negative-binomial `zeroinfl` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR0": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "CR0": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR0": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `Glm` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 3:
        vcov = "CR1": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "CR1": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `coxph` models.
        i The Lin-Wei sandwich of `coxph(cluster = )` is one cluster-robust estimator, with no small-sample factor: CR0.
        i This class supports: classical, CR0.
      Received by 2:
        vcov = "CR1": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `cph` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 2:
        vcov = "CR1": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "CR1": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "CR1": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "CR1": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "CR1": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "CR1": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "CR1": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "CR1": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `lrm` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 3:
        vcov = "CR1": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR1": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `ols` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 2:
        vcov = "CR1": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR1": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `rq` models.
        i There is no cluster-robust sandwich for quantile regression: clubSandwich has no method for `rq`, and no estimating functions are available to build one.
        i Use `vcov = "bootstrap"` with `cluster =`: the wild gradient cluster bootstrap, the route available for clustered data.
      Received by 1:
        vcov = "CR1": rq
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR1": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "CR1": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "CR1": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "CR1": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "CR1": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for a negative-binomial `hurdle` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR1": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for a negative-binomial `zeroinfl` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR1": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "CR1": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR1": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is available for `lm` fits only.
        i "CR1S" reproduces Stata's `regress, vce(cluster)` (CR1S scaling, t(G-1)). Stata's ML commands use a different convention (G/(G-1) scaling, z), so the label would not match Stata output for a glm.
        i Use `vcov = "CR2"` (Bell-McCaffrey with Satterthwaite df, the recommended default) or "CR0"-"CR3" with `cluster`.
      Received by 10:
        vcov = "CR1S": glm.nb, glm_gamma, glm_gaussian, glm_logit, glm_poisson,
            glm_probit, glm_quasi, svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `Glm` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 3:
        vcov = "CR1S": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `bam` models.
        i spicy computes one cluster-robust estimator for mgcv fits, from their GLM score and penalized covariance: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR1S": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `betareg` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR1S": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `clm` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR1S": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "CR1S": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `coxph` models.
        i The Lin-Wei sandwich of `coxph(cluster = )` is one cluster-robust estimator, with no small-sample factor: CR0.
        i This class supports: classical, CR0.
      Received by 2:
        vcov = "CR1S": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `cph` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 2:
        vcov = "CR1S": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "CR1S": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "CR1S": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `gam` models.
        i spicy computes one cluster-robust estimator for mgcv fits, from their GLM score and penalized covariance: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 2:
        vcov = "CR1S": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "CR1S": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "CR1S": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "CR1S": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `hurdle` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR1S": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "CR1S": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "CR1S": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `lme` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "CR1S": lme, lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `lmerMod` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "CR1S": lmer, lmer_ml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `lrm` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 3:
        vcov = "CR1S": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `mlogit` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR1S": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `multinom` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR1S": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR1S": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `ols` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 2:
        vcov = "CR1S": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `polr` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR1S": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR1S": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `rq` models.
        i There is no cluster-robust sandwich for quantile regression: clubSandwich has no method for `rq`, and no estimating functions are available to build one.
        i Use `vcov = "bootstrap"` with `cluster =`: the wild gradient cluster bootstrap, the route available for clustered data.
      Received by 1:
        vcov = "CR1S": rq
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR1S": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `survreg` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 2:
        vcov = "CR1S": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "CR1S": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "CR1S": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `tobit` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR1S": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for `zeroinfl` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR1S": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "CR1S": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for a negative-binomial `hurdle` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR1S": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for a negative-binomial `zeroinfl` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR1S": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for this `glmmTMB` fit.
        i This class supports `vcov = "classical"`, `"CR0"` and `"CR1"`.
      Received by 4:
        vcov = "CR1S": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "CR1S": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR1S": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `Glm` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 3:
        vcov = "CR2": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `bam` models.
        i spicy computes one cluster-robust estimator for mgcv fits, from their GLM score and penalized covariance: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR2": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `betareg` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR2": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `clm` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR2": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "CR2": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `coxph` models.
        i The Lin-Wei sandwich of `coxph(cluster = )` is one cluster-robust estimator, with no small-sample factor: CR0.
        i This class supports: classical, CR0.
      Received by 2:
        vcov = "CR2": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `cph` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 2:
        vcov = "CR2": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "CR2": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "CR2": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `gam` models.
        i spicy computes one cluster-robust estimator for mgcv fits, from their GLM score and penalized covariance: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 2:
        vcov = "CR2": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "CR2": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "CR2": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "CR2": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `hurdle` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR2": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "CR2": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "CR2": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `lrm` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 3:
        vcov = "CR2": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `mlogit` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR2": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `multinom` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR2": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR2": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `ols` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 2:
        vcov = "CR2": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `polr` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR2": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR2": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `rq` models.
        i There is no cluster-robust sandwich for quantile regression: clubSandwich has no method for `rq`, and no estimating functions are available to build one.
        i Use `vcov = "bootstrap"` with `cluster =`: the wild gradient cluster bootstrap, the route available for clustered data.
      Received by 1:
        vcov = "CR2": rq
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR2": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `survreg` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 2:
        vcov = "CR2": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "CR2": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "CR2": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "CR2": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `tobit` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR2": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for `zeroinfl` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR2": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "CR2": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for a negative-binomial `hurdle` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR2": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for a negative-binomial `zeroinfl` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR2": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "CR2": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich has one score per cluster, the gradient of the cluster's marginal log-likelihood: with no residuals and no hat values, the CR2 and CR3 corrections cannot be formed.
        i Use `vcov = "CR0"` or `"CR1"`.
      Received by 4:
        vcov = "CR2": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR2": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `Glm` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 3:
        vcov = "CR3": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `bam` models.
        i spicy computes one cluster-robust estimator for mgcv fits, from their GLM score and penalized covariance: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR3": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `betareg` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR3": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `clm` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR3": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "CR3": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `coxph` models.
        i The Lin-Wei sandwich of `coxph(cluster = )` is one cluster-robust estimator, with no small-sample factor: CR0.
        i This class supports: classical, CR0.
      Received by 2:
        vcov = "CR3": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `cph` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 2:
        vcov = "CR3": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "CR3": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "CR3": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `gam` models.
        i spicy computes one cluster-robust estimator for mgcv fits, from their GLM score and penalized covariance: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 2:
        vcov = "CR3": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "CR3": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "CR3": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "CR3": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `hurdle` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR3": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "CR3": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "CR3": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `lrm` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 3:
        vcov = "CR3": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `mlogit` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR3": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `multinom` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR3": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR3": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `ols` models.
        i spicy computes the cluster-robust variance of an rms fit with `rms::robcov()`, whose Huber estimator is CR0.
        i This class supports: classical, CR0.
      Received by 2:
        vcov = "CR3": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `polr` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR3": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR3": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `rq` models.
        i There is no cluster-robust sandwich for quantile regression: clubSandwich has no method for `rq`, and no estimating functions are available to build one.
        i Use `vcov = "bootstrap"` with `cluster =`: the wild gradient cluster bootstrap, the route available for clustered data.
      Received by 1:
        vcov = "CR3": rq
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "CR3": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `survreg` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 2:
        vcov = "CR3": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "CR3": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "CR3": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "CR3": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `tobit` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR3": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for `zeroinfl` models.
        i sandwich::vcovCL() computes one cluster-robust estimator for this class: CR1, the CR0 sandwich scaled by G/(G-1).
        i This class supports: classical, CR1.
      Received by 1:
        vcov = "CR3": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "CR3": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for a negative-binomial `hurdle` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR3": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for a negative-binomial `zeroinfl` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR3": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "CR3": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich has one score per cluster, the gradient of the cluster's marginal log-likelihood: with no residuals and no hat values, the CR2 and CR3 corrections cannot be formed.
        i Use `vcov = "CR0"` or `"CR1"`.
      Received by 4:
        vcov = "CR3": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "CR3": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `Glm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "HC0": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `bam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "HC0": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `betareg` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "HC0": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `clm` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC0": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC0": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `coxph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "HC0": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `cph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "HC0": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "HC0": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "HC0": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `gam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 2:
        vcov = "HC0": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "HC0": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "HC0": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "HC0": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `hurdle` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "HC0": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "HC0": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "HC0": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `lme` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "HC0": lme, lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `lmerMod` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "HC0": lmer, lmer_ml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `lrm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "HC0": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `mlogit` models.
        i sandwich::vcovHC() computes a result for mlogit but silently mis-scales the meat for its per-chooser score structure.
        i Cluster-robust "CR1" with `cluster` is supported for this class.
      Received by 1:
        vcov = "HC0": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `multinom` models.
        i A multi-equation model has no working residuals or hat values, so the HC* meat cannot be formed.
        i Cluster-robust "CR1" with `cluster` is supported for this class.
      Received by 1:
        vcov = "HC0": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC0": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `ols` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 2:
        vcov = "HC0": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `polr` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC0": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC0": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `rq` models.
        i Quantile regression has no working residuals or hat values, so the HC* meat cannot be formed.
        i It carries its own sandwich family instead: "nid" (the default) is the heteroskedasticity-robust estimator here -- a sparsity-based quantile sandwich, not an HC variant under another name. "iid", "ker" and "rank" are the other estimators.
      Received by 1:
        vcov = "HC0": rq
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC0": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `survreg` models.
        i This class supports: classical, CR1. See ?table_regression_survival.
      Received by 2:
        vcov = "HC0": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "HC0": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "HC0": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "HC0": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `tobit` models.
        i This class supports: classical, CR1. See ?table_regression_robust.
      Received by 1:
        vcov = "HC0": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for `zeroinfl` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "HC0": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "HC0": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for a negative-binomial `hurdle` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC0": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for a negative-binomial `zeroinfl` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC0": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for this `glmmTMB` fit.
        i Observations that share random effects have no scores of their own, so there is no per-observation (HC) sandwich.
        i Use `vcov = "CR0"` or `"CR1"`, clustered by default on the top-level grouping factor.
      Received by 4:
        vcov = "HC0": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "HC0": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC0": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `Glm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "HC1": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `bam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "HC1": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `betareg` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "HC1": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `clm` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC1": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC1": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `coxph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "HC1": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `cph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "HC1": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "HC1": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "HC1": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `gam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 2:
        vcov = "HC1": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "HC1": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "HC1": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "HC1": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `hurdle` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "HC1": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "HC1": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "HC1": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `lme` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "HC1": lme, lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `lmerMod` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "HC1": lmer, lmer_ml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `lrm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "HC1": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `mlogit` models.
        i sandwich::vcovHC() computes a result for mlogit but silently mis-scales the meat for its per-chooser score structure.
        i Cluster-robust "CR1" with `cluster` is supported for this class.
      Received by 1:
        vcov = "HC1": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `multinom` models.
        i A multi-equation model has no working residuals or hat values, so the HC* meat cannot be formed.
        i Cluster-robust "CR1" with `cluster` is supported for this class.
      Received by 1:
        vcov = "HC1": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC1": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `ols` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 2:
        vcov = "HC1": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `polr` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC1": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC1": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `rq` models.
        i Quantile regression has no working residuals or hat values, so the HC* meat cannot be formed.
        i It carries its own sandwich family instead: "nid" (the default) is the heteroskedasticity-robust estimator here -- a sparsity-based quantile sandwich, not an HC variant under another name. "iid", "ker" and "rank" are the other estimators.
      Received by 1:
        vcov = "HC1": rq
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC1": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `survreg` models.
        i This class supports: classical, CR1. See ?table_regression_survival.
      Received by 2:
        vcov = "HC1": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "HC1": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "HC1": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "HC1": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `tobit` models.
        i This class supports: classical, CR1. See ?table_regression_robust.
      Received by 1:
        vcov = "HC1": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for `zeroinfl` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "HC1": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "HC1": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for a negative-binomial `hurdle` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC1": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for a negative-binomial `zeroinfl` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC1": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for this `glmmTMB` fit.
        i Observations that share random effects have no scores of their own, so there is no per-observation (HC) sandwich.
        i Use `vcov = "CR0"` or `"CR1"`, clustered by default on the top-level grouping factor.
      Received by 4:
        vcov = "HC1": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "HC1": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC1": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `Glm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "HC2": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `bam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "HC2": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `betareg` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "HC2": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `clm` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC2": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC2": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `coxph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "HC2": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `cph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "HC2": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "HC2": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "HC2": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `gam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 2:
        vcov = "HC2": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "HC2": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "HC2": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "HC2": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `hurdle` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "HC2": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "HC2": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `ivreg` models.
        i HC2-HC5 weight each residual by a leverage, which two-stage least squares does not define uniquely: `AER::ivreg()` and `estimatr::iv_robust()` use different hat values. For the second, refit with `estimatr::iv_robust(..., se_type = "HC2")` or "HC3".
        i This class supports: "classical", "HC0", "HC1", "CR0"-"CR3" and "CR1S" with `cluster`.
      Received by 1:
        vcov = "HC2": ivreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "HC2": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `lme` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "HC2": lme, lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `lmerMod` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "HC2": lmer, lmer_ml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `lrm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "HC2": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `mlogit` models.
        i sandwich::vcovHC() computes a result for mlogit but silently mis-scales the meat for its per-chooser score structure.
        i Cluster-robust "CR1" with `cluster` is supported for this class.
      Received by 1:
        vcov = "HC2": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `multinom` models.
        i A multi-equation model has no working residuals or hat values, so the HC* meat cannot be formed.
        i Cluster-robust "CR1" with `cluster` is supported for this class.
      Received by 1:
        vcov = "HC2": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC2": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `ols` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 2:
        vcov = "HC2": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `polr` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC2": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC2": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `rq` models.
        i Quantile regression has no working residuals or hat values, so the HC* meat cannot be formed.
        i It carries its own sandwich family instead: "nid" (the default) is the heteroskedasticity-robust estimator here -- a sparsity-based quantile sandwich, not an HC variant under another name. "iid", "ker" and "rank" are the other estimators.
      Received by 1:
        vcov = "HC2": rq
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC2": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `survreg` models.
        i This class supports: classical, CR1. See ?table_regression_survival.
      Received by 2:
        vcov = "HC2": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "HC2": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "HC2": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "HC2": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `tobit` models.
        i This class supports: classical, CR1. See ?table_regression_robust.
      Received by 1:
        vcov = "HC2": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for `zeroinfl` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "HC2": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "HC2": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for a negative-binomial `hurdle` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC2": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for a negative-binomial `zeroinfl` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC2": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for this `glmmTMB` fit.
        i Observations that share random effects have no scores of their own, so there is no per-observation (HC) sandwich.
        i Use `vcov = "CR0"` or `"CR1"`, clustered by default on the top-level grouping factor.
      Received by 4:
        vcov = "HC2": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "HC2": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC2": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `Glm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "HC3": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `bam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "HC3": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `betareg` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "HC3": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `clm` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC3": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC3": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `coxph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "HC3": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `cph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "HC3": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "HC3": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "HC3": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `gam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 2:
        vcov = "HC3": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "HC3": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "HC3": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "HC3": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `hurdle` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "HC3": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "HC3": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `ivreg` models.
        i HC2-HC5 weight each residual by a leverage, which two-stage least squares does not define uniquely: `AER::ivreg()` and `estimatr::iv_robust()` use different hat values. For the second, refit with `estimatr::iv_robust(..., se_type = "HC2")` or "HC3".
        i This class supports: "classical", "HC0", "HC1", "CR0"-"CR3" and "CR1S" with `cluster`.
      Received by 1:
        vcov = "HC3": ivreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "HC3": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `lme` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "HC3": lme, lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `lmerMod` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "HC3": lmer, lmer_ml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `lrm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "HC3": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `mlogit` models.
        i sandwich::vcovHC() computes a result for mlogit but silently mis-scales the meat for its per-chooser score structure.
        i Cluster-robust "CR1" with `cluster` is supported for this class.
      Received by 1:
        vcov = "HC3": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `multinom` models.
        i A multi-equation model has no working residuals or hat values, so the HC* meat cannot be formed.
        i Cluster-robust "CR1" with `cluster` is supported for this class.
      Received by 1:
        vcov = "HC3": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC3": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `ols` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 2:
        vcov = "HC3": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `polr` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC3": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC3": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `rq` models.
        i Quantile regression has no working residuals or hat values, so the HC* meat cannot be formed.
        i It carries its own sandwich family instead: "nid" (the default) is the heteroskedasticity-robust estimator here -- a sparsity-based quantile sandwich, not an HC variant under another name. "iid", "ker" and "rank" are the other estimators.
      Received by 1:
        vcov = "HC3": rq
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC3": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `survreg` models.
        i This class supports: classical, CR1. See ?table_regression_survival.
      Received by 2:
        vcov = "HC3": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "HC3": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "HC3": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "HC3": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `tobit` models.
        i This class supports: classical, CR1. See ?table_regression_robust.
      Received by 1:
        vcov = "HC3": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for `zeroinfl` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "HC3": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "HC3": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for a negative-binomial `hurdle` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC3": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for a negative-binomial `zeroinfl` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC3": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for this `glmmTMB` fit.
        i Observations that share random effects have no scores of their own, so there is no per-observation (HC) sandwich.
        i Use `vcov = "CR0"` or `"CR1"`, clustered by default on the top-level grouping factor.
      Received by 4:
        vcov = "HC3": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "HC3": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC3": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `Glm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "HC4": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `bam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "HC4": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `betareg` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "HC4": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `clm` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC4": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC4": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `coxph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "HC4": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `cph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "HC4": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "HC4": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "HC4": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `gam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 2:
        vcov = "HC4": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "HC4": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "HC4": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "HC4": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `hurdle` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "HC4": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "HC4": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `ivreg` models.
        i HC2-HC5 weight each residual by a leverage, which two-stage least squares does not define uniquely: `AER::ivreg()` and `estimatr::iv_robust()` use different hat values. For the second, refit with `estimatr::iv_robust(..., se_type = "HC2")` or "HC3".
        i This class supports: "classical", "HC0", "HC1", "CR0"-"CR3" and "CR1S" with `cluster`.
      Received by 1:
        vcov = "HC4": ivreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "HC4": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `lme` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "HC4": lme, lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `lmerMod` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "HC4": lmer, lmer_ml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `lrm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "HC4": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `mlogit` models.
        i sandwich::vcovHC() computes a result for mlogit but silently mis-scales the meat for its per-chooser score structure.
        i Cluster-robust "CR1" with `cluster` is supported for this class.
      Received by 1:
        vcov = "HC4": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `multinom` models.
        i A multi-equation model has no working residuals or hat values, so the HC* meat cannot be formed.
        i Cluster-robust "CR1" with `cluster` is supported for this class.
      Received by 1:
        vcov = "HC4": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC4": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `ols` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 2:
        vcov = "HC4": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `polr` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC4": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC4": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `rq` models.
        i Quantile regression has no working residuals or hat values, so the HC* meat cannot be formed.
        i It carries its own sandwich family instead: "nid" (the default) is the heteroskedasticity-robust estimator here -- a sparsity-based quantile sandwich, not an HC variant under another name. "iid", "ker" and "rank" are the other estimators.
      Received by 1:
        vcov = "HC4": rq
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC4": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `survreg` models.
        i This class supports: classical, CR1. See ?table_regression_survival.
      Received by 2:
        vcov = "HC4": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "HC4": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "HC4": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "HC4": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `tobit` models.
        i This class supports: classical, CR1. See ?table_regression_robust.
      Received by 1:
        vcov = "HC4": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for `zeroinfl` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "HC4": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "HC4": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for a negative-binomial `hurdle` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC4": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for a negative-binomial `zeroinfl` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC4": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for this `glmmTMB` fit.
        i Observations that share random effects have no scores of their own, so there is no per-observation (HC) sandwich.
        i Use `vcov = "CR0"` or `"CR1"`, clustered by default on the top-level grouping factor.
      Received by 4:
        vcov = "HC4": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "HC4": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC4": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `Glm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "HC5": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `bam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "HC5": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `betareg` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "HC5": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `clm` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC5": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC5": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `coxph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "HC5": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `cph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "HC5": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "HC5": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "HC5": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `gam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 2:
        vcov = "HC5": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "HC5": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "HC5": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "HC5": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `hurdle` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "HC5": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "HC5": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `ivreg` models.
        i HC2-HC5 weight each residual by a leverage, which two-stage least squares does not define uniquely: `AER::ivreg()` and `estimatr::iv_robust()` use different hat values. For the second, refit with `estimatr::iv_robust(..., se_type = "HC2")` or "HC3".
        i This class supports: "classical", "HC0", "HC1", "CR0"-"CR3" and "CR1S" with `cluster`.
      Received by 1:
        vcov = "HC5": ivreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "HC5": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `lme` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "HC5": lme, lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `lmerMod` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "HC5": lmer, lmer_ml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `lrm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "HC5": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `mlogit` models.
        i sandwich::vcovHC() computes a result for mlogit but silently mis-scales the meat for its per-chooser score structure.
        i Cluster-robust "CR1" with `cluster` is supported for this class.
      Received by 1:
        vcov = "HC5": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `multinom` models.
        i A multi-equation model has no working residuals or hat values, so the HC* meat cannot be formed.
        i Cluster-robust "CR1" with `cluster` is supported for this class.
      Received by 1:
        vcov = "HC5": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC5": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `ols` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 2:
        vcov = "HC5": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `polr` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "HC5": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC5": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `rq` models.
        i Quantile regression has no working residuals or hat values, so the HC* meat cannot be formed.
        i It carries its own sandwich family instead: "nid" (the default) is the heteroskedasticity-robust estimator here -- a sparsity-based quantile sandwich, not an HC variant under another name. "iid", "ker" and "rank" are the other estimators.
      Received by 1:
        vcov = "HC5": rq
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "HC5": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `survreg` models.
        i This class supports: classical, CR1. See ?table_regression_survival.
      Received by 2:
        vcov = "HC5": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "HC5": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "HC5": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "HC5": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `tobit` models.
        i This class supports: classical, CR1. See ?table_regression_robust.
      Received by 1:
        vcov = "HC5": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for `zeroinfl` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "HC5": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "HC5": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for a negative-binomial `hurdle` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC5": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for a negative-binomial `zeroinfl` fit.
        i The sandwich of these fits leaves out the score of theta, so it is not the full sandwich of the fit.
        i Fit the model with glmmTMB, family `nbinom2` with `ziformula` (`truncated_nbinom2` for a hurdle), for which spicy computes the full sandwich with `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC5": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for this `glmmTMB` fit.
        i Observations that share random effects have no scores of their own, so there is no per-observation (HC) sandwich.
        i Use `vcov = "CR0"` or `"CR1"`, clustered by default on the top-level grouping factor.
      Received by 4:
        vcov = "HC5": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "HC5": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "HC5": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `Glm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "bootstrap": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `bam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "bootstrap": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `betareg` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "bootstrap": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `clm` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "bootstrap": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "bootstrap": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `coxph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "bootstrap": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `cph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "bootstrap": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "bootstrap": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "bootstrap": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `gam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 2:
        vcov = "bootstrap": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "bootstrap": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "bootstrap": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "bootstrap": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `hurdle` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "bootstrap": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `hurdle` models.
        i This class supports: classical. See ?table_regression_counts.
      Received by 1:
        vcov = "bootstrap": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "bootstrap": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `ivreg` models.
        i The resamplers refit an `lm()`, not the two-stage least-squares model.
        i This class supports: "classical", "HC0", "HC1", "CR0"-"CR3" and "CR1S" with `cluster`.
      Received by 1:
        vcov = "bootstrap": ivreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "bootstrap": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `lme` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "bootstrap": lme, lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `lmerMod` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "bootstrap": lmer, lmer_ml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `lrm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "bootstrap": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `mlogit` models.
        i This class supports: classical, CR1. See ?table_regression_categorical.
      Received by 1:
        vcov = "bootstrap": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `multinom` models.
        i This class supports: classical, CR1. See ?table_regression_categorical.
      Received by 1:
        vcov = "bootstrap": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "bootstrap": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `ols` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 2:
        vcov = "bootstrap": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `polr` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "bootstrap": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "bootstrap": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "bootstrap": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `survreg` models.
        i This class supports: classical, CR1. See ?table_regression_survival.
      Received by 2:
        vcov = "bootstrap": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "bootstrap": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "bootstrap": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "bootstrap": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `tobit` models.
        i This class supports: classical, CR1. See ?table_regression_robust.
      Received by 1:
        vcov = "bootstrap": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `zeroinfl` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "bootstrap": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for `zeroinfl` models.
        i This class supports: classical. See ?table_regression_counts.
      Received by 1:
        vcov = "bootstrap": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "bootstrap": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for this `glmmTMB` fit.
        i This class supports `vcov = "classical"`, `"CR0"` and `"CR1"`.
      Received by 4:
        vcov = "bootstrap": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "bootstrap": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "bootstrap": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `Glm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "iid": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `bam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "iid": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `betareg` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "iid": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `clm` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "iid": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "iid": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `coxph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "iid": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `cph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "iid": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "iid": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "iid": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `gam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 2:
        vcov = "iid": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "iid": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `glm` models.
        i This class supports: classical, HC0, HC1, HC2, HC3, HC4, HC5, CR0, CR1, CR2, CR3, bootstrap, jackknife. See ?table_regression.
      Received by 6:
        vcov = "iid": glm_gamma, glm_gaussian, glm_logit, glm_poisson,
            glm_probit, glm_quasi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "iid": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "iid": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `hurdle` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "iid": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `hurdle` models.
        i This class supports: classical. See ?table_regression_counts.
      Received by 1:
        vcov = "iid": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "iid": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `ivreg` models.
        i This class supports: classical, HC0, HC1, CR0, CR1, CR2, CR3, CR1S. See ?table_regression.
      Received by 1:
        vcov = "iid": ivreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "iid": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `lm` models.
        i This class supports: classical, HC0, HC1, HC2, HC3, HC4, HC5, CR0, CR1, CR2, CR3, bootstrap, jackknife, CR1S. See ?table_regression.
      Received by 2:
        vcov = "iid": lm, lm_weighted
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `lme` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "iid": lme, lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `lmerMod` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "iid": lmer, lmer_ml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `lrm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "iid": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `mlogit` models.
        i This class supports: classical, CR1. See ?table_regression_categorical.
      Received by 1:
        vcov = "iid": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `multinom` models.
        i This class supports: classical, CR1. See ?table_regression_categorical.
      Received by 1:
        vcov = "iid": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `negbin` models.
        i This class supports: classical, HC0, HC1, HC2, HC3, HC4, HC5, CR0, CR1, CR2, CR3, bootstrap, jackknife. See ?table_regression.
      Received by 1:
        vcov = "iid": glm.nb
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "iid": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `ols` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 2:
        vcov = "iid": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `polr` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "iid": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "iid": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "iid": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `survreg` models.
        i This class supports: classical, CR1. See ?table_regression_survival.
      Received by 2:
        vcov = "iid": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "iid": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "iid": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "iid": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `tobit` models.
        i This class supports: classical, CR1. See ?table_regression_robust.
      Received by 1:
        vcov = "iid": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `zeroinfl` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "iid": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for `zeroinfl` models.
        i This class supports: classical. See ?table_regression_counts.
      Received by 1:
        vcov = "iid": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "iid": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for this `glmmTMB` fit.
        i This class supports `vcov = "classical"`, `"CR0"` and `"CR1"`.
      Received by 4:
        vcov = "iid": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "iid": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "iid": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `Glm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "jackknife": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `bam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "jackknife": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `betareg` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "jackknife": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `clm` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "jackknife": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "jackknife": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `coxph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "jackknife": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `cph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "jackknife": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "jackknife": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "jackknife": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `gam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 2:
        vcov = "jackknife": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "jackknife": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "jackknife": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "jackknife": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `hurdle` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "jackknife": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `hurdle` models.
        i This class supports: classical. See ?table_regression_counts.
      Received by 1:
        vcov = "jackknife": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "jackknife": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `ivreg` models.
        i The resamplers refit an `lm()`, not the two-stage least-squares model.
        i This class supports: "classical", "HC0", "HC1", "CR0"-"CR3" and "CR1S" with `cluster`.
      Received by 1:
        vcov = "jackknife": ivreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "jackknife": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `lme` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "jackknife": lme, lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `lmerMod` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "jackknife": lmer, lmer_ml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `lrm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "jackknife": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `mlogit` models.
        i This class supports: classical, CR1. See ?table_regression_categorical.
      Received by 1:
        vcov = "jackknife": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `multinom` models.
        i This class supports: classical, CR1. See ?table_regression_categorical.
      Received by 1:
        vcov = "jackknife": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "jackknife": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `ols` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 2:
        vcov = "jackknife": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `polr` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "jackknife": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "jackknife": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `rq` models.
        i The leave-one-out jackknife is inconsistent for non-smooth estimators: the quantile objective is not differentiable, so dropping one observation can move the fit discontinuously.
        i Use `vcov = "bootstrap"` (quantreg's own xy-pair resampling), or the analytic "nid" / "ker" sandwiches.
      Received by 1:
        vcov = "jackknife": rq
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "jackknife": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `survreg` models.
        i This class supports: classical, CR1. See ?table_regression_survival.
      Received by 2:
        vcov = "jackknife": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "jackknife": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "jackknife": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "jackknife": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `tobit` models.
        i This class supports: classical, CR1. See ?table_regression_robust.
      Received by 1:
        vcov = "jackknife": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `zeroinfl` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "jackknife": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for `zeroinfl` models.
        i This class supports: classical. See ?table_regression_counts.
      Received by 1:
        vcov = "jackknife": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "jackknife": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for this `glmmTMB` fit.
        i This class supports `vcov = "classical"`, `"CR0"` and `"CR1"`.
      Received by 4:
        vcov = "jackknife": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "jackknife": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "jackknife": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `Glm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "ker": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `bam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "ker": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `betareg` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "ker": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `clm` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "ker": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "ker": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `coxph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "ker": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `cph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "ker": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "ker": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "ker": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `gam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 2:
        vcov = "ker": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "ker": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `glm` models.
        i This class supports: classical, HC0, HC1, HC2, HC3, HC4, HC5, CR0, CR1, CR2, CR3, bootstrap, jackknife. See ?table_regression.
      Received by 6:
        vcov = "ker": glm_gamma, glm_gaussian, glm_logit, glm_poisson,
            glm_probit, glm_quasi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "ker": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "ker": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `hurdle` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "ker": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `hurdle` models.
        i This class supports: classical. See ?table_regression_counts.
      Received by 1:
        vcov = "ker": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "ker": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `ivreg` models.
        i This class supports: classical, HC0, HC1, CR0, CR1, CR2, CR3, CR1S. See ?table_regression.
      Received by 1:
        vcov = "ker": ivreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "ker": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `lm` models.
        i This class supports: classical, HC0, HC1, HC2, HC3, HC4, HC5, CR0, CR1, CR2, CR3, bootstrap, jackknife, CR1S. See ?table_regression.
      Received by 2:
        vcov = "ker": lm, lm_weighted
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `lme` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "ker": lme, lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `lmerMod` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "ker": lmer, lmer_ml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `lrm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "ker": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `mlogit` models.
        i This class supports: classical, CR1. See ?table_regression_categorical.
      Received by 1:
        vcov = "ker": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `multinom` models.
        i This class supports: classical, CR1. See ?table_regression_categorical.
      Received by 1:
        vcov = "ker": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `negbin` models.
        i This class supports: classical, HC0, HC1, HC2, HC3, HC4, HC5, CR0, CR1, CR2, CR3, bootstrap, jackknife. See ?table_regression.
      Received by 1:
        vcov = "ker": glm.nb
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "ker": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `ols` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 2:
        vcov = "ker": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `polr` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "ker": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "ker": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "ker": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `survreg` models.
        i This class supports: classical, CR1. See ?table_regression_survival.
      Received by 2:
        vcov = "ker": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "ker": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "ker": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "ker": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `tobit` models.
        i This class supports: classical, CR1. See ?table_regression_robust.
      Received by 1:
        vcov = "ker": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `zeroinfl` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "ker": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for `zeroinfl` models.
        i This class supports: classical. See ?table_regression_counts.
      Received by 1:
        vcov = "ker": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "ker": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for this `glmmTMB` fit.
        i This class supports `vcov = "classical"`, `"CR0"` and `"CR1"`.
      Received by 4:
        vcov = "ker": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "ker": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "ker": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `Glm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "nid": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `bam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "nid": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `betareg` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "nid": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `clm` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "nid": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "nid": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `coxph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "nid": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `cph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "nid": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "nid": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "nid": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `gam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 2:
        vcov = "nid": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "nid": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `glm` models.
        i This class supports: classical, HC0, HC1, HC2, HC3, HC4, HC5, CR0, CR1, CR2, CR3, bootstrap, jackknife. See ?table_regression.
      Received by 6:
        vcov = "nid": glm_gamma, glm_gaussian, glm_logit, glm_poisson,
            glm_probit, glm_quasi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "nid": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "nid": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `hurdle` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "nid": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `hurdle` models.
        i This class supports: classical. See ?table_regression_counts.
      Received by 1:
        vcov = "nid": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "nid": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `ivreg` models.
        i This class supports: classical, HC0, HC1, CR0, CR1, CR2, CR3, CR1S. See ?table_regression.
      Received by 1:
        vcov = "nid": ivreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "nid": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `lm` models.
        i This class supports: classical, HC0, HC1, HC2, HC3, HC4, HC5, CR0, CR1, CR2, CR3, bootstrap, jackknife, CR1S. See ?table_regression.
      Received by 2:
        vcov = "nid": lm, lm_weighted
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `lme` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "nid": lme, lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `lmerMod` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "nid": lmer, lmer_ml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `lrm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "nid": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `mlogit` models.
        i This class supports: classical, CR1. See ?table_regression_categorical.
      Received by 1:
        vcov = "nid": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `multinom` models.
        i This class supports: classical, CR1. See ?table_regression_categorical.
      Received by 1:
        vcov = "nid": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `negbin` models.
        i This class supports: classical, HC0, HC1, HC2, HC3, HC4, HC5, CR0, CR1, CR2, CR3, bootstrap, jackknife. See ?table_regression.
      Received by 1:
        vcov = "nid": glm.nb
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "nid": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `ols` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 2:
        vcov = "nid": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `polr` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "nid": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "nid": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "nid": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `survreg` models.
        i This class supports: classical, CR1. See ?table_regression_survival.
      Received by 2:
        vcov = "nid": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "nid": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "nid": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "nid": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `tobit` models.
        i This class supports: classical, CR1. See ?table_regression_robust.
      Received by 1:
        vcov = "nid": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `zeroinfl` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "nid": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for `zeroinfl` models.
        i This class supports: classical. See ?table_regression_counts.
      Received by 1:
        vcov = "nid": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "nid": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for this `glmmTMB` fit.
        i This class supports `vcov = "classical"`, `"CR0"` and `"CR1"`.
      Received by 4:
        vcov = "nid": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "nid": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "nid": glmmTMB_reml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `Glm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "rank": rms_Glm, rms_Glm_logit, rms_Glm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `bam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "rank": bam
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `betareg` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 1:
        vcov = "rank": betareg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `clm` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "rank": clm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `clmm` models.
        i This class supports: classical. See ?table_regression_ordinal.
      Received by 1:
        vcov = "rank": clmm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `coxph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "rank": coxph, coxph_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `cph` models.
        i This class supports: classical, CR0. See ?table_regression_survival.
      Received by 2:
        vcov = "rank": cph, cph_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `fixest` models.
        i fixest computes its own standard errors, which spicy reports unchanged; the footer names the estimator the fit carries.
        i Choose it through fixest: at estimation with `fixest::feols(..., vcov = ~cluster_var)` or `vcov = "hetero"`, or afterwards with `summary(fit, vcov = ~cluster_var)`.
      Received by 5:
        vcov = "rank": feols, feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `flexsurvreg` models.
        i This class supports: classical. See ?table_regression_survival.
      Received by 1:
        vcov = "rank": flexsurvreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `gam` models.
        i This class supports: classical, CR1. See ?table_regression.
      Received by 2:
        vcov = "rank": gam, gam_logit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `geeglm` models.
        i GEE inference is robust by construction: the fit's own sandwich standard errors, clustered on its `id =` variable, are the default display.
        i To change the estimator, refit with geeglm's `std.err =` option ("san.se", "jack", "j1s", "fij"); spicy reads the fit's choice.
      Received by 1:
        vcov = "rank": geeglm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `glm` models.
        i This class supports: classical, HC0, HC1, HC2, HC3, HC4, HC5, CR0, CR1, CR2, CR3, bootstrap, jackknife. See ?table_regression.
      Received by 6:
        vcov = "rank": glm_gamma, glm_gaussian, glm_logit, glm_poisson,
            glm_probit, glm_quasi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `glmerMod` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "rank": glmer
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `gls` models.
        i This class supports: classical. See ?table_regression_mixed.
      Received by 1:
        vcov = "rank": gls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `hurdle` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "rank": hurdle
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `hurdle` models.
        i This class supports: classical. See ?table_regression_counts.
      Received by 1:
        vcov = "rank": hurdle_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `iv_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::iv_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 1:
        vcov = "rank": iv_robust
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `ivreg` models.
        i This class supports: classical, HC0, HC1, CR0, CR1, CR2, CR3, CR1S. See ?table_regression.
      Received by 1:
        vcov = "rank": ivreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `lm_robust` models.
        i estimatr fixes the variance estimator at fit time and spicy reports the fit's own standard errors unchanged.
        i Refit with the standard errors you want: `estimatr::lm_robust(..., se_type = "HC3")`, or `se_type = "CR2"` with `clusters =`.
      Received by 2:
        vcov = "rank": lm_robust, lm_robust_fe
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `lm` models.
        i This class supports: classical, HC0, HC1, HC2, HC3, HC4, HC5, CR0, CR1, CR2, CR3, bootstrap, jackknife, CR1S. See ?table_regression.
      Received by 2:
        vcov = "rank": lm, lm_weighted
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `lme` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "rank": lme, lme_contrasts
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `lmerMod` models.
        i This class supports: classical, CR0, CR1, CR2, CR3. See ?table_regression_mixed.
      Received by 2:
        vcov = "rank": lmer, lmer_ml
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `lrm` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 3:
        vcov = "rank": lrm, lrm_ordinal, lrm_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `mlogit` models.
        i This class supports: classical, CR1. See ?table_regression_categorical.
      Received by 1:
        vcov = "rank": mlogit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `multinom` models.
        i This class supports: classical, CR1. See ?table_regression_categorical.
      Received by 1:
        vcov = "rank": multinom
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `negbin` models.
        i This class supports: classical, HC0, HC1, HC2, HC3, HC4, HC5, CR0, CR1, CR2, CR3, bootstrap, jackknife. See ?table_regression.
      Received by 1:
        vcov = "rank": glm.nb
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `nls` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "rank": nls
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `ols` models.
        i This class supports: classical, CR0. See ?table_regression.
      Received by 2:
        vcov = "rank": ols, ols_xy
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `polr` models.
        i This class supports: classical, CR1. See ?table_regression_ordinal.
      Received by 1:
        vcov = "rank": polr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `rlm` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "rank": rlm
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `selection` models.
        i This class supports: classical. See ?table_regression.
      Received by 1:
        vcov = "rank": selection
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `survreg` models.
        i This class supports: classical, CR1. See ?table_regression_survival.
      Received by 2:
        vcov = "rank": survreg, survreg_strata
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `svycoxph` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "rank": svycoxph
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `svyglm` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 3:
        vcov = "rank": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `svyolr` models.
        i The fit's own design-based (Taylor / replicate) variance is already the robust variance for the declared design.
        i To account for clustering, declare it in the design: survey::svydesign(ids = ~cluster, ...), then refit.
      Received by 1:
        vcov = "rank": svyolr
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `tobit` models.
        i This class supports: classical, CR1. See ?table_regression_robust.
      Received by 1:
        vcov = "rank": tobit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `zeroinfl` models.
        i This class supports: classical, CR1. See ?table_regression_counts.
      Received by 1:
        vcov = "rank": zeroinfl
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for `zeroinfl` models.
        i This class supports: classical. See ?table_regression_counts.
      Received by 1:
        vcov = "rank": zeroinfl_negbin
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for a `clm` fit with a scale or nominal component.
        i `ordinal` provides no estimating functions for the scale / nominal parts, so no sandwich can be built. A plain proportional-odds `clm` supports "CR1" with `cluster`.
        i Use the model-based default, or refit without `scale` / `nominal`.
      Received by 2:
        vcov = "rank": clm_nominal, clm_scale
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for this `glmmTMB` fit.
        i This class supports `vcov = "classical"`, `"CR0"` and `"CR1"`.
      Received by 4:
        vcov = "rank": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_zi
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich does not cover the cut-points of the ordinal family.
      Received by 1:
        vcov = "rank": glmmTMB_ordinal
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not available for this `glmmTMB` fit.
        i glmmTMB's sandwich is defined for maximum-likelihood fits only.
        i Refit with `REML = FALSE` to use `vcov = "CR0"` or `"CR1"`.
      Received by 1:
        vcov = "rank": glmmTMB_reml

# the refusals of the vcov sweep keep their text and class: Stan

    Code
      cat(.refusal_render(rows), sep = "\n")
    Output
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "CR0": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR0"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "CR0": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "CR1": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "CR1": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "CR1S": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR1S"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "CR1S": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "CR2": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR2"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "CR2": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "CR3": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "CR3"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "CR3": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "HC0": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC0"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "HC0": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "HC1": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC1"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "HC1": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "HC2": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC2"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "HC2": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "HC3": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC3"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "HC3": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "HC4": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC4"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "HC4": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "HC5": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "HC5"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "HC5": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "bootstrap": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "bootstrap"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "bootstrap": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "iid": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "iid"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "iid": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "jackknife": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "jackknife"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "jackknife": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "ker": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "ker"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "ker": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "nid": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "nid"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "nid": stanreg
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not defined for Bayesian fits (`brmsfit`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "rank": brmsfit
      ----
      error: spicy_unsupported_vcov, spicy_error
        `vcov = "rank"` is not defined for Bayesian fits (`stanreg`).
        i A posterior has no sandwich analog: the displayed uncertainty comes from the posterior draws, not from an estimating equation.
        i Model the clustering or heteroskedasticity instead (rstanarm::stan_glmer(), brms group-level or distributional terms).
      Received by 1:
        vcov = "rank": stanreg

