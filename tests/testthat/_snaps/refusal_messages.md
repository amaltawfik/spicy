# the refusals and warnings of the argument sweep keep their text and class

    Code
      cat(.refusal_render(rows), sep = "\n")
    Output
      ----
      error: spicy_empty_table, spicy_error
        Model 1 has no rows to show.
        i Every row the model could contribute has been switched off.
        i Re-enable one of the row blocks (`show_thresholds`, `show_re`, `show_components`).
      Received by 1:
        nested = TRUE: flexsurvreg
      ----
      error: spicy_empty_table, spicy_error
        Model 1 has no rows to show: it has fixed effects only.
        i fixest absorbs fixed effects instead of reporting them as coefficients.
        i Add a predictor before the `|` of the formula.
      Received by 2:
        nested = TRUE: feols, fepois
      ----
      error: spicy_invalid_frame, spicy_error
        Invalid regression frame: `coefs$parent_var` has wrong type.
        x expected character, got logical
      Received by 1:
        nested = TRUE: lm_robust_fe
      ----
      error: spicy_invalid_frame, spicy_error
        Invalid regression frame: `coefs` missing required column(s).
        x missing: term, estimate
      Received by 1:
        nested = TRUE: svycoxph
      ----
      error: spicy_invalid_input, spicy_error
        AME columns are not available for `clmm`.
        i No average-marginal-effects backend exists for this class (see ?table_regression_models, column AME).
        i Drop the AME token(s) from `show_columns`.
      Received by 4:
        show_columns = "ame": clmm
        show_columns = "ame_ci": clmm
        show_columns = "ame_p": clmm
        show_columns = "ame_se": clmm
      ----
      error: spicy_invalid_input, spicy_error
        AME columns are not available for `fixest`.
        i Model 1: marginaleffects, which computes the AME, refuses a fixest GLM with fixed effects (`feglm()`, `fepois()`, `femlm()`, `fenegbin()`, whatever the family): it cannot take into account the uncertainty of the fixed effects, which fixest does not estimate. The AME is available for `feols()` and for a GLM without fixed effects.
        i Drop the AME token(s) from `show_columns`.
      Received by 4:
        show_columns = "ame": fepois
        show_columns = "ame_ci": fepois
        show_columns = "ame_p": fepois
        show_columns = "ame_se": fepois
      ----
      error: spicy_invalid_input, spicy_error
        AME columns are not available for `flexsurvreg`.
        i No average-marginal-effects backend exists for this class (see ?table_regression_models, column AME).
        i Drop the AME token(s) from `show_columns`.
      Received by 4:
        show_columns = "ame": flexsurvreg
        show_columns = "ame_ci": flexsurvreg
        show_columns = "ame_p": flexsurvreg
        show_columns = "ame_se": flexsurvreg
      ----
      error: spicy_invalid_input, spicy_error
        AME columns are not available for `glmmTMB`.
        i No average-marginal-effects backend exists for this class (see ?table_regression_models, column AME).
        i Drop the AME token(s) from `show_columns`.
      Received by 4:
        show_columns = "ame": glmmTMB_ordinal
        show_columns = "ame_ci": glmmTMB_ordinal
        show_columns = "ame_p": glmmTMB_ordinal
        show_columns = "ame_se": glmmTMB_ordinal
      ----
      error: spicy_invalid_input, spicy_error
        AME columns are not available for `nls`.
        i No average-marginal-effects backend exists for this class (see ?table_regression_models, column AME).
        i Drop the AME token(s) from `show_columns`.
      Received by 4:
        show_columns = "ame": nls
        show_columns = "ame_ci": nls
        show_columns = "ame_p": nls
        show_columns = "ame_se": nls
      ----
      error: spicy_invalid_input, spicy_error
        AME columns are not available for `selection`.
        i No average-marginal-effects backend exists for this class (see ?table_regression_models, column AME).
        i Drop the AME token(s) from `show_columns`.
      Received by 4:
        show_columns = "ame": selection
        show_columns = "ame_ci": selection
        show_columns = "ame_p": selection
        show_columns = "ame_se": selection
      ----
      error: spicy_invalid_input, spicy_error
        Models have different response variables: yiv ~ 1 - x | xen, yiv ~ 1 | xen.
        i Hierarchical comparison requires the same DV across all models. For side-by-side multi-DV display, use `nested = FALSE`.
      Received by 1:
        nested = TRUE: feols_iv
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `Glm`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 2:
        show_columns = "partial_chi2": rms_Glm
        show_columns = "partial_f": rms_Glm
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `betareg`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": betareg
        show_columns = "partial_eta2": betareg
        show_columns = "partial_eta2_ci": betareg
        show_columns = "partial_f": betareg
        show_columns = "partial_f2": betareg
        show_columns = "partial_f2_ci": betareg
        show_columns = "partial_omega2": betareg
        show_columns = "partial_omega2_ci": betareg
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `clm`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": clm
        show_columns = "partial_eta2": clm
        show_columns = "partial_eta2_ci": clm
        show_columns = "partial_f": clm
        show_columns = "partial_f2": clm
        show_columns = "partial_f2_ci": clm
        show_columns = "partial_omega2": clm
        show_columns = "partial_omega2_ci": clm
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `clmm`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 2:
        show_columns = "partial_chi2": clmm
        show_columns = "partial_f": clmm
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `coxph`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": coxph
        show_columns = "partial_eta2": coxph
        show_columns = "partial_eta2_ci": coxph
        show_columns = "partial_f": coxph
        show_columns = "partial_f2": coxph
        show_columns = "partial_f2_ci": coxph
        show_columns = "partial_omega2": coxph
        show_columns = "partial_omega2_ci": coxph
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `cph`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": cph
        show_columns = "partial_eta2": cph
        show_columns = "partial_eta2_ci": cph
        show_columns = "partial_f": cph
        show_columns = "partial_f2": cph
        show_columns = "partial_f2_ci": cph
        show_columns = "partial_omega2": cph
        show_columns = "partial_omega2_ci": cph
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `fixest`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": feols
        show_columns = "partial_eta2": feols
        show_columns = "partial_eta2_ci": feols
        show_columns = "partial_f": feols
        show_columns = "partial_f2": feols
        show_columns = "partial_f2_ci": feols
        show_columns = "partial_omega2": feols
        show_columns = "partial_omega2_ci": feols
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `flexsurvreg`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": flexsurvreg
        show_columns = "partial_eta2": flexsurvreg
        show_columns = "partial_eta2_ci": flexsurvreg
        show_columns = "partial_f": flexsurvreg
        show_columns = "partial_f2": flexsurvreg
        show_columns = "partial_f2_ci": flexsurvreg
        show_columns = "partial_omega2": flexsurvreg
        show_columns = "partial_omega2_ci": flexsurvreg
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `gam`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 6:
        show_columns = "partial_chi2": bam, gam, gam_logit
        show_columns = "partial_f": bam, gam, gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `gls`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": gls
        show_columns = "partial_eta2": gls
        show_columns = "partial_eta2_ci": gls
        show_columns = "partial_f": gls
        show_columns = "partial_f2": gls
        show_columns = "partial_f2_ci": gls
        show_columns = "partial_omega2": gls
        show_columns = "partial_omega2_ci": gls
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `hurdle`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": hurdle
        show_columns = "partial_eta2": hurdle
        show_columns = "partial_eta2_ci": hurdle
        show_columns = "partial_f": hurdle
        show_columns = "partial_f2": hurdle
        show_columns = "partial_f2_ci": hurdle
        show_columns = "partial_omega2": hurdle
        show_columns = "partial_omega2_ci": hurdle
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `iv_robust`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": iv_robust
        show_columns = "partial_eta2": iv_robust
        show_columns = "partial_eta2_ci": iv_robust
        show_columns = "partial_f": iv_robust
        show_columns = "partial_f2": iv_robust
        show_columns = "partial_f2_ci": iv_robust
        show_columns = "partial_omega2": iv_robust
        show_columns = "partial_omega2_ci": iv_robust
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `ivreg`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": ivreg
        show_columns = "partial_eta2": ivreg
        show_columns = "partial_eta2_ci": ivreg
        show_columns = "partial_f": ivreg
        show_columns = "partial_f2": ivreg
        show_columns = "partial_f2_ci": ivreg
        show_columns = "partial_omega2": ivreg
        show_columns = "partial_omega2_ci": ivreg
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `lm_robust`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": lm_robust
        show_columns = "partial_eta2": lm_robust
        show_columns = "partial_eta2_ci": lm_robust
        show_columns = "partial_f": lm_robust
        show_columns = "partial_f2": lm_robust
        show_columns = "partial_f2_ci": lm_robust
        show_columns = "partial_omega2": lm_robust
        show_columns = "partial_omega2_ci": lm_robust
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `lrm`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 2:
        show_columns = "partial_chi2": lrm
        show_columns = "partial_f": lrm
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `mlogit`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": mlogit
        show_columns = "partial_eta2": mlogit
        show_columns = "partial_eta2_ci": mlogit
        show_columns = "partial_f": mlogit
        show_columns = "partial_f2": mlogit
        show_columns = "partial_f2_ci": mlogit
        show_columns = "partial_omega2": mlogit
        show_columns = "partial_omega2_ci": mlogit
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `multinom`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": multinom
        show_columns = "partial_eta2": multinom
        show_columns = "partial_eta2_ci": multinom
        show_columns = "partial_f": multinom
        show_columns = "partial_f2": multinom
        show_columns = "partial_f2_ci": multinom
        show_columns = "partial_omega2": multinom
        show_columns = "partial_omega2_ci": multinom
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `nls`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": nls
        show_columns = "partial_eta2": nls
        show_columns = "partial_eta2_ci": nls
        show_columns = "partial_f": nls
        show_columns = "partial_f2": nls
        show_columns = "partial_f2_ci": nls
        show_columns = "partial_omega2": nls
        show_columns = "partial_omega2_ci": nls
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `ols`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": ols
        show_columns = "partial_eta2": ols
        show_columns = "partial_eta2_ci": ols
        show_columns = "partial_f": ols
        show_columns = "partial_f2": ols
        show_columns = "partial_f2_ci": ols
        show_columns = "partial_omega2": ols
        show_columns = "partial_omega2_ci": ols
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `polr`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": polr
        show_columns = "partial_eta2": polr
        show_columns = "partial_eta2_ci": polr
        show_columns = "partial_f": polr
        show_columns = "partial_f2": polr
        show_columns = "partial_f2_ci": polr
        show_columns = "partial_omega2": polr
        show_columns = "partial_omega2_ci": polr
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `rlm`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": rlm
        show_columns = "partial_eta2": rlm
        show_columns = "partial_eta2_ci": rlm
        show_columns = "partial_f": rlm
        show_columns = "partial_f2": rlm
        show_columns = "partial_f2_ci": rlm
        show_columns = "partial_omega2": rlm
        show_columns = "partial_omega2_ci": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `rq`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": rq
        show_columns = "partial_eta2": rq
        show_columns = "partial_eta2_ci": rq
        show_columns = "partial_f": rq
        show_columns = "partial_f2": rq
        show_columns = "partial_f2_ci": rq
        show_columns = "partial_omega2": rq
        show_columns = "partial_omega2_ci": rq
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `selection`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": selection
        show_columns = "partial_eta2": selection
        show_columns = "partial_eta2_ci": selection
        show_columns = "partial_f": selection
        show_columns = "partial_f2": selection
        show_columns = "partial_f2_ci": selection
        show_columns = "partial_omega2": selection
        show_columns = "partial_omega2_ci": selection
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `survreg`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": survreg
        show_columns = "partial_eta2": survreg
        show_columns = "partial_eta2_ci": survreg
        show_columns = "partial_f": survreg
        show_columns = "partial_f2": survreg
        show_columns = "partial_f2_ci": survreg
        show_columns = "partial_omega2": survreg
        show_columns = "partial_omega2_ci": survreg
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `svycoxph`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": svycoxph
        show_columns = "partial_eta2": svycoxph
        show_columns = "partial_eta2_ci": svycoxph
        show_columns = "partial_f": svycoxph
        show_columns = "partial_f2": svycoxph
        show_columns = "partial_f2_ci": svycoxph
        show_columns = "partial_omega2": svycoxph
        show_columns = "partial_omega2_ci": svycoxph
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `svyglm`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 2:
        show_columns = "partial_chi2": svyglm
        show_columns = "partial_f": svyglm
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `svyolr`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": svyolr
        show_columns = "partial_eta2": svyolr
        show_columns = "partial_eta2_ci": svyolr
        show_columns = "partial_f": svyolr
        show_columns = "partial_f2": svyolr
        show_columns = "partial_f2_ci": svyolr
        show_columns = "partial_omega2": svyolr
        show_columns = "partial_omega2_ci": svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `tobit`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": tobit
        show_columns = "partial_eta2": tobit
        show_columns = "partial_eta2_ci": tobit
        show_columns = "partial_f": tobit
        show_columns = "partial_f2": tobit
        show_columns = "partial_f2_ci": tobit
        show_columns = "partial_omega2": tobit
        show_columns = "partial_omega2_ci": tobit
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `zeroinfl`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": zeroinfl
        show_columns = "partial_eta2": zeroinfl
        show_columns = "partial_eta2_ci": zeroinfl
        show_columns = "partial_f": zeroinfl
        show_columns = "partial_f2": zeroinfl
        show_columns = "partial_f2_ci": zeroinfl
        show_columns = "partial_omega2": zeroinfl
        show_columns = "partial_omega2_ci": zeroinfl
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for a stratified survreg fit (M1).
        i Per-stratum scale parameters are not standardized; refit without `strata()`.
      Received by 8:
        show_columns = "risk_diff": survreg_strata
        show_columns = "risk_diff_ci": survreg_strata
        show_columns = "risk_diff_p": survreg_strata
        show_columns = "risk_diff_se": survreg_strata
        show_columns = "rmst": survreg_strata
        show_columns = "rmst_ci": survreg_strata
        show_columns = "rmst_p": survreg_strata
        show_columns = "rmst_se": survreg_strata
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for a tobit fit (M1).
        i A tobit models a censored response, not a time to event.
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": tobit
        show_columns = "risk_diff_ci": tobit
        show_columns = "risk_diff_p": tobit
        show_columns = "risk_diff_se": tobit
        show_columns = "rmst": tobit
        show_columns = "rmst_ci": tobit
        show_columns = "rmst_p": tobit
        show_columns = "rmst_se": tobit
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (Glm).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": rms_Glm
        show_columns = "risk_diff_ci": rms_Glm
        show_columns = "risk_diff_p": rms_Glm
        show_columns = "risk_diff_se": rms_Glm
        show_columns = "rmst": rms_Glm
        show_columns = "rmst_ci": rms_Glm
        show_columns = "rmst_p": rms_Glm
        show_columns = "rmst_se": rms_Glm
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (bam).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": bam
        show_columns = "risk_diff_ci": bam
        show_columns = "risk_diff_p": bam
        show_columns = "risk_diff_se": bam
        show_columns = "rmst": bam
        show_columns = "rmst_ci": bam
        show_columns = "rmst_p": bam
        show_columns = "rmst_se": bam
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (betareg).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": betareg
        show_columns = "risk_diff_ci": betareg
        show_columns = "risk_diff_p": betareg
        show_columns = "risk_diff_se": betareg
        show_columns = "rmst": betareg
        show_columns = "rmst_ci": betareg
        show_columns = "rmst_p": betareg
        show_columns = "rmst_se": betareg
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (clm).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": clm
        show_columns = "risk_diff_ci": clm
        show_columns = "risk_diff_p": clm
        show_columns = "risk_diff_se": clm
        show_columns = "rmst": clm
        show_columns = "rmst_ci": clm
        show_columns = "rmst_p": clm
        show_columns = "rmst_se": clm
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (clmm).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": clmm
        show_columns = "risk_diff_ci": clmm
        show_columns = "risk_diff_p": clmm
        show_columns = "risk_diff_se": clmm
        show_columns = "rmst": clmm
        show_columns = "rmst_ci": clmm
        show_columns = "rmst_p": clmm
        show_columns = "rmst_se": clmm
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (cph).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": cph
        show_columns = "risk_diff_ci": cph
        show_columns = "risk_diff_p": cph
        show_columns = "risk_diff_se": cph
        show_columns = "rmst": cph
        show_columns = "rmst_ci": cph
        show_columns = "rmst_p": cph
        show_columns = "rmst_se": cph
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (fixest).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": feols
        show_columns = "risk_diff_ci": feols
        show_columns = "risk_diff_p": feols
        show_columns = "risk_diff_se": feols
        show_columns = "rmst": feols
        show_columns = "rmst_ci": feols
        show_columns = "rmst_p": feols
        show_columns = "rmst_se": feols
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (flexsurvreg).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": flexsurvreg
        show_columns = "risk_diff_ci": flexsurvreg
        show_columns = "risk_diff_p": flexsurvreg
        show_columns = "risk_diff_se": flexsurvreg
        show_columns = "rmst": flexsurvreg
        show_columns = "rmst_ci": flexsurvreg
        show_columns = "rmst_p": flexsurvreg
        show_columns = "rmst_se": flexsurvreg
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (gam).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 16:
        show_columns = "risk_diff": gam, gam_logit
        show_columns = "risk_diff_ci": gam, gam_logit
        show_columns = "risk_diff_p": gam, gam_logit
        show_columns = "risk_diff_se": gam, gam_logit
        show_columns = "rmst": gam, gam_logit
        show_columns = "rmst_ci": gam, gam_logit
        show_columns = "rmst_p": gam, gam_logit
        show_columns = "rmst_se": gam, gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (geeglm).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": geeglm
        show_columns = "risk_diff_ci": geeglm
        show_columns = "risk_diff_p": geeglm
        show_columns = "risk_diff_se": geeglm
        show_columns = "rmst": geeglm
        show_columns = "rmst_ci": geeglm
        show_columns = "rmst_p": geeglm
        show_columns = "rmst_se": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (glm).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": glm_gaussian
        show_columns = "risk_diff_ci": glm_gaussian
        show_columns = "risk_diff_p": glm_gaussian
        show_columns = "risk_diff_se": glm_gaussian
        show_columns = "rmst": glm_gaussian
        show_columns = "rmst_ci": glm_gaussian
        show_columns = "rmst_p": glm_gaussian
        show_columns = "rmst_se": glm_gaussian
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (glmerMod).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": glmer
        show_columns = "risk_diff_ci": glmer
        show_columns = "risk_diff_p": glmer
        show_columns = "risk_diff_se": glmer
        show_columns = "rmst": glmer
        show_columns = "rmst_ci": glmer
        show_columns = "rmst_p": glmer
        show_columns = "rmst_se": glmer
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (glmmTMB).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": glmmTMB
        show_columns = "risk_diff_ci": glmmTMB
        show_columns = "risk_diff_p": glmmTMB
        show_columns = "risk_diff_se": glmmTMB
        show_columns = "rmst": glmmTMB
        show_columns = "rmst_ci": glmmTMB
        show_columns = "rmst_p": glmmTMB
        show_columns = "rmst_se": glmmTMB
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (gls).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": gls
        show_columns = "risk_diff_ci": gls
        show_columns = "risk_diff_p": gls
        show_columns = "risk_diff_se": gls
        show_columns = "rmst": gls
        show_columns = "rmst_ci": gls
        show_columns = "rmst_p": gls
        show_columns = "rmst_se": gls
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (hurdle).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": hurdle
        show_columns = "risk_diff_ci": hurdle
        show_columns = "risk_diff_p": hurdle
        show_columns = "risk_diff_se": hurdle
        show_columns = "rmst": hurdle
        show_columns = "rmst_ci": hurdle
        show_columns = "rmst_p": hurdle
        show_columns = "rmst_se": hurdle
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (iv_robust).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": iv_robust
        show_columns = "risk_diff_ci": iv_robust
        show_columns = "risk_diff_p": iv_robust
        show_columns = "risk_diff_se": iv_robust
        show_columns = "rmst": iv_robust
        show_columns = "rmst_ci": iv_robust
        show_columns = "rmst_p": iv_robust
        show_columns = "rmst_se": iv_robust
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (ivreg).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": ivreg
        show_columns = "risk_diff_ci": ivreg
        show_columns = "risk_diff_p": ivreg
        show_columns = "risk_diff_se": ivreg
        show_columns = "rmst": ivreg
        show_columns = "rmst_ci": ivreg
        show_columns = "rmst_p": ivreg
        show_columns = "rmst_se": ivreg
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (lm).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": lm
        show_columns = "risk_diff_ci": lm
        show_columns = "risk_diff_p": lm
        show_columns = "risk_diff_se": lm
        show_columns = "rmst": lm
        show_columns = "rmst_ci": lm
        show_columns = "rmst_p": lm
        show_columns = "rmst_se": lm
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (lm_robust).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": lm_robust
        show_columns = "risk_diff_ci": lm_robust
        show_columns = "risk_diff_p": lm_robust
        show_columns = "risk_diff_se": lm_robust
        show_columns = "rmst": lm_robust
        show_columns = "rmst_ci": lm_robust
        show_columns = "rmst_p": lm_robust
        show_columns = "rmst_se": lm_robust
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (lme).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": lme
        show_columns = "risk_diff_ci": lme
        show_columns = "risk_diff_p": lme
        show_columns = "risk_diff_se": lme
        show_columns = "rmst": lme
        show_columns = "rmst_ci": lme
        show_columns = "rmst_p": lme
        show_columns = "rmst_se": lme
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (lmerMod).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": lmer
        show_columns = "risk_diff_ci": lmer
        show_columns = "risk_diff_p": lmer
        show_columns = "risk_diff_se": lmer
        show_columns = "rmst": lmer
        show_columns = "rmst_ci": lmer
        show_columns = "rmst_p": lmer
        show_columns = "rmst_se": lmer
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (lrm).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": lrm
        show_columns = "risk_diff_ci": lrm
        show_columns = "risk_diff_p": lrm
        show_columns = "risk_diff_se": lrm
        show_columns = "rmst": lrm
        show_columns = "rmst_ci": lrm
        show_columns = "rmst_p": lrm
        show_columns = "rmst_se": lrm
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (mlogit).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": mlogit
        show_columns = "risk_diff_ci": mlogit
        show_columns = "risk_diff_p": mlogit
        show_columns = "risk_diff_se": mlogit
        show_columns = "rmst": mlogit
        show_columns = "rmst_ci": mlogit
        show_columns = "rmst_p": mlogit
        show_columns = "rmst_se": mlogit
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (multinom).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": multinom
        show_columns = "risk_diff_ci": multinom
        show_columns = "risk_diff_p": multinom
        show_columns = "risk_diff_se": multinom
        show_columns = "rmst": multinom
        show_columns = "rmst_ci": multinom
        show_columns = "rmst_p": multinom
        show_columns = "rmst_se": multinom
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (negbin).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": glm.nb
        show_columns = "risk_diff_ci": glm.nb
        show_columns = "risk_diff_p": glm.nb
        show_columns = "risk_diff_se": glm.nb
        show_columns = "rmst": glm.nb
        show_columns = "rmst_ci": glm.nb
        show_columns = "rmst_p": glm.nb
        show_columns = "rmst_se": glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (nls).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": nls
        show_columns = "risk_diff_ci": nls
        show_columns = "risk_diff_p": nls
        show_columns = "risk_diff_se": nls
        show_columns = "rmst": nls
        show_columns = "rmst_ci": nls
        show_columns = "rmst_p": nls
        show_columns = "rmst_se": nls
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (ols).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": ols
        show_columns = "risk_diff_ci": ols
        show_columns = "risk_diff_p": ols
        show_columns = "risk_diff_se": ols
        show_columns = "rmst": ols
        show_columns = "rmst_ci": ols
        show_columns = "rmst_p": ols
        show_columns = "rmst_se": ols
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (polr).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": polr
        show_columns = "risk_diff_ci": polr
        show_columns = "risk_diff_p": polr
        show_columns = "risk_diff_se": polr
        show_columns = "rmst": polr
        show_columns = "rmst_ci": polr
        show_columns = "rmst_p": polr
        show_columns = "rmst_se": polr
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (rlm).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": rlm
        show_columns = "risk_diff_ci": rlm
        show_columns = "risk_diff_p": rlm
        show_columns = "risk_diff_se": rlm
        show_columns = "rmst": rlm
        show_columns = "rmst_ci": rlm
        show_columns = "rmst_p": rlm
        show_columns = "rmst_se": rlm
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (rq).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": rq
        show_columns = "risk_diff_ci": rq
        show_columns = "risk_diff_p": rq
        show_columns = "risk_diff_se": rq
        show_columns = "rmst": rq
        show_columns = "rmst_ci": rq
        show_columns = "rmst_p": rq
        show_columns = "rmst_se": rq
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (selection).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": selection
        show_columns = "risk_diff_ci": selection
        show_columns = "risk_diff_p": selection
        show_columns = "risk_diff_se": selection
        show_columns = "rmst": selection
        show_columns = "rmst_ci": selection
        show_columns = "rmst_p": selection
        show_columns = "rmst_se": selection
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (svyglm).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": svyglm
        show_columns = "risk_diff_ci": svyglm
        show_columns = "risk_diff_p": svyglm
        show_columns = "risk_diff_se": svyglm
        show_columns = "rmst": svyglm
        show_columns = "rmst_ci": svyglm
        show_columns = "rmst_p": svyglm
        show_columns = "rmst_se": svyglm
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (svyolr).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": svyolr
        show_columns = "risk_diff_ci": svyolr
        show_columns = "risk_diff_p": svyolr
        show_columns = "risk_diff_se": svyolr
        show_columns = "rmst": svyolr
        show_columns = "rmst_ci": svyolr
        show_columns = "rmst_p": svyolr
        show_columns = "rmst_se": svyolr
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (zeroinfl).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": zeroinfl
        show_columns = "risk_diff_ci": zeroinfl
        show_columns = "risk_diff_p": zeroinfl
        show_columns = "risk_diff_se": zeroinfl
        show_columns = "rmst": zeroinfl
        show_columns = "rmst_ci": zeroinfl
        show_columns = "rmst_p": zeroinfl
        show_columns = "rmst_se": zeroinfl
      ----
      error: spicy_invalid_input, spicy_error
        Token "pd" in `show_columns` is defined only for Bayesian fits.
        i The probability of direction is a posterior-draw summary (share of draws on the dominant side of zero); frequentist fits have none.
      Received by 39:
        show_columns = "pd": bam, betareg, clm, clmm, coxph, cph, feols,
            flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gaussian, glmer,
            glmmTMB, gls, hurdle, iv_robust, ivreg, lm, lm_robust, lme, lmer,
            lrm, mlogit, multinom, nls, ols, polr, rlm, rms_Glm, rq, selection,
            survreg, svycoxph, svyglm, svyolr, tobit, zeroinfl
      ----
      error: spicy_invalid_input, spicy_error
        Token `"partial_chi2"` in `show_columns` is not defined for `lm` models.
        i `"partial_chi2"` is the likelihood-ratio chi-square of a `glm` whose dispersion is fixed (binomial, Poisson, `MASS::glm.nb()`) and the Wald chi-square of a mixed-effects model.
        i Use `"partial_f"`, or `"partial_f2"`, `"partial_eta2"` or `"partial_omega2"` for the variance-explained partials of an `lm`.
      Received by 1:
        show_columns = "partial_chi2": lm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"partial_chi2"` in `show_columns` is not defined for the model (`glm`, Gamma).
        i `"partial_chi2"` is the likelihood-ratio chi-square of a `glm` whose dispersion is fixed (binomial, Poisson, `MASS::glm.nb()`) and the Wald chi-square of a mixed-effects model.
        i Use `"partial_f"`.
      Received by 1:
        show_columns = "partial_chi2": glm_gamma
      ----
      error: spicy_invalid_input, spicy_error
        Token `"partial_chi2"` in `show_columns` is not defined for the model (`glm`, gaussian).
        i `"partial_chi2"` is the likelihood-ratio chi-square of a `glm` whose dispersion is fixed (binomial, Poisson, `MASS::glm.nb()`) and the Wald chi-square of a mixed-effects model.
        i Use `"partial_f"`.
      Received by 1:
        show_columns = "partial_chi2": glm_gaussian
      ----
      error: spicy_invalid_input, spicy_error
        Token `"partial_chi2"` in `show_columns` is not defined for the model (`glm`, quasipoisson).
        i `"partial_chi2"` is the likelihood-ratio chi-square of a `glm` whose dispersion is fixed (binomial, Poisson, `MASS::glm.nb()`) and the Wald chi-square of a mixed-effects model.
        i Use `"partial_f"`.
      Received by 1:
        show_columns = "partial_chi2": glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"partial_f"` in `show_columns` is not defined for the model (`glm`, binomial).
        i `"partial_f"` is the partial F of an `lm` and of a `glm` whose dispersion is estimated (gaussian, Gamma, inverse Gaussian, Tweedie, `negative.binomial(theta)` fitted by `glm()`, the quasi families), the F that `anova(reduced, full, test = "F")` reports.
        i Use `"partial_chi2"`.
      Received by 2:
        show_columns = "partial_f": glm_logit, glm_probit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"partial_f"` in `show_columns` is not defined for the model (`glm`, poisson).
        i `"partial_f"` is the partial F of an `lm` and of a `glm` whose dispersion is estimated (gaussian, Gamma, inverse Gaussian, Tweedie, `negative.binomial(theta)` fitted by `glm()`, the quasi families), the F that `anova(reduced, full, test = "F")` reports.
        i Use `"partial_chi2"`.
      Received by 1:
        show_columns = "partial_f": glm_poisson
      ----
      error: spicy_invalid_input, spicy_error
        Token `"partial_f"` in `show_columns` is not defined for the model (`glmerMod`).
        i `"partial_f"` is the partial F of an `lm` and of a `glm` whose dispersion is estimated (gaussian, Gamma, inverse Gaussian, Tweedie, `negative.binomial(theta)` fitted by `glm()`, the quasi families), the F that `anova(reduced, full, test = "F")` reports.
        i Use `"partial_chi2"`.
      Received by 1:
        show_columns = "partial_f": glmer
      ----
      error: spicy_invalid_input, spicy_error
        Token `"partial_f"` in `show_columns` is not defined for the model (`glmmTMB`).
        i `"partial_f"` is the partial F of an `lm` and of a `glm` whose dispersion is estimated (gaussian, Gamma, inverse Gaussian, Tweedie, `negative.binomial(theta)` fitted by `glm()`, the quasi families), the F that `anova(reduced, full, test = "F")` reports.
        i Use `"partial_chi2"`.
      Received by 1:
        show_columns = "partial_f": glmmTMB
      ----
      error: spicy_invalid_input, spicy_error
        Token `"partial_f"` in `show_columns` is not defined for the model (`lme`).
        i `"partial_f"` is the partial F of an `lm` and of a `glm` whose dispersion is estimated (gaussian, Gamma, inverse Gaussian, Tweedie, `negative.binomial(theta)` fitted by `glm()`, the quasi families), the F that `anova(reduced, full, test = "F")` reports.
        i Use `"partial_chi2"`.
      Received by 1:
        show_columns = "partial_f": lme
      ----
      error: spicy_invalid_input, spicy_error
        Token `"partial_f"` in `show_columns` is not defined for the model (`lmerMod`).
        i `"partial_f"` is the partial F of an `lm` and of a `glm` whose dispersion is estimated (gaussian, Gamma, inverse Gaussian, Tweedie, `negative.binomial(theta)` fitted by `glm()`, the quasi families), the F that `anova(reduced, full, test = "F")` reports.
        i Use `"partial_chi2"`.
      Received by 1:
        show_columns = "partial_f": lmer
      ----
      error: spicy_invalid_input, spicy_error
        Token `"partial_f"` in `show_columns` is not defined for the model (`negbin`, Negative Binomial(1.4352)).
        i `"partial_f"` is the partial F of an `lm` and of a `glm` whose dispersion is estimated (gaussian, Gamma, inverse Gaussian, Tweedie, `negative.binomial(theta)` fitted by `glm()`, the quasi families), the F that `anova(reduced, full, test = "F")` reports.
        i Use `"partial_chi2"`.
      Received by 1:
        show_columns = "partial_f": glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `Glm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "adj_r2": rms_Glm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `betareg` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": betareg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `clm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": clm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `clmm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": clmm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `coxph` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": coxph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `cph` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": cph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `fixest` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": feols
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `flexsurvreg` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": flexsurvreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `geeglm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "adj_r2": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `glm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "adj_r2": glm_gaussian
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `glmerMod` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": glmer
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `glmmTMB` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": glmmTMB
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `gls` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": gls
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `hurdle` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": hurdle
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `iv_robust` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": iv_robust
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `ivreg` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": ivreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `lm_robust` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": lm_robust
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `lme` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": lme
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `lmerMod` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": lmer
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `lrm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "adj_r2": lrm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `mgcv` GAMs.
        i The column reports the R<U+00B2> of each fit of a univariable screen of least-squares models. For a GAM, ask for a fit-statistics row in `show_fit_stats`.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`, `"r2_deviance"`.
      Received by 1:
        show_columns = "adj_r2": bam
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `mgcv` GAMs.
        i The column reports the R<U+00B2> of each fit of a univariable screen of least-squares models. For a GAM, ask for a fit-statistics row in `show_fit_stats`.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_tjur"`, `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`.
      Received by 1:
        show_columns = "adj_r2": gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `mgcv` GAMs.
        i The column reports the R<U+00B2> of each fit of a univariable screen of least-squares models. For a GAM, ask for a fit-statistics row in `show_fit_stats`.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"r2_deviance"`.
      Received by 1:
        show_columns = "adj_r2": gam
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `mlogit` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": mlogit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `multinom` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": multinom
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `negbin` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "adj_r2": glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `nls` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": nls
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `polr` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": polr
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `rq` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": rq
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `selection` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": selection
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `survreg` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": survreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `svycoxph` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": svycoxph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `svyglm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "adj_r2": svyglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `svyolr` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `tobit` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": tobit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `zeroinfl` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": zeroinfl
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ame" in `show_columns` are not available for mlogit models.
        i marginaleffects computes predictions but not slopes (average marginal effects) for mlogit's one-row-per-choice data structure.
      Received by 1:
        show_columns = "ame": mlogit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ame" in `show_columns` are not defined for Cox models.
        i Average marginal effects on a Cox proportional-hazards fit are on an ambiguous survival / hazard scale with unreliable standard errors. Report hazard ratios instead with `exponentiate = TRUE`.
        i For a design-based Cox model, test a term with `survey::regTermTest(fit, ~term)` and read a marginal survival curve with `survey::svykm()`.
      Received by 1:
        show_columns = "ame": svycoxph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ame" in `show_columns` are not defined for Cox models.
        i Average marginal effects on a Cox proportional-hazards fit are on an ambiguous survival / hazard scale with unreliable standard errors. Report hazard ratios instead with `exponentiate = TRUE`.
        i For absolute effects, use the survival estimand columns: `show_columns = c("b", "rmst")` with `tau = `, or `"risk_diff"` with `at_time = ` (coxph).
      Received by 2:
        show_columns = "ame": coxph, cph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ame_ci" in `show_columns` are not available for mlogit models.
        i marginaleffects computes predictions but not slopes (average marginal effects) for mlogit's one-row-per-choice data structure.
      Received by 1:
        show_columns = "ame_ci": mlogit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ame_ci" in `show_columns` are not defined for Cox models.
        i Average marginal effects on a Cox proportional-hazards fit are on an ambiguous survival / hazard scale with unreliable standard errors. Report hazard ratios instead with `exponentiate = TRUE`.
        i For a design-based Cox model, test a term with `survey::regTermTest(fit, ~term)` and read a marginal survival curve with `survey::svykm()`.
      Received by 1:
        show_columns = "ame_ci": svycoxph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ame_ci" in `show_columns` are not defined for Cox models.
        i Average marginal effects on a Cox proportional-hazards fit are on an ambiguous survival / hazard scale with unreliable standard errors. Report hazard ratios instead with `exponentiate = TRUE`.
        i For absolute effects, use the survival estimand columns: `show_columns = c("b", "rmst")` with `tau = `, or `"risk_diff"` with `at_time = ` (coxph).
      Received by 2:
        show_columns = "ame_ci": coxph, cph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ame_p" in `show_columns` are not available for mlogit models.
        i marginaleffects computes predictions but not slopes (average marginal effects) for mlogit's one-row-per-choice data structure.
      Received by 1:
        show_columns = "ame_p": mlogit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ame_p" in `show_columns` are not defined for Cox models.
        i Average marginal effects on a Cox proportional-hazards fit are on an ambiguous survival / hazard scale with unreliable standard errors. Report hazard ratios instead with `exponentiate = TRUE`.
        i For a design-based Cox model, test a term with `survey::regTermTest(fit, ~term)` and read a marginal survival curve with `survey::svykm()`.
      Received by 1:
        show_columns = "ame_p": svycoxph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ame_p" in `show_columns` are not defined for Cox models.
        i Average marginal effects on a Cox proportional-hazards fit are on an ambiguous survival / hazard scale with unreliable standard errors. Report hazard ratios instead with `exponentiate = TRUE`.
        i For absolute effects, use the survival estimand columns: `show_columns = c("b", "rmst")` with `tau = `, or `"risk_diff"` with `at_time = ` (coxph).
      Received by 2:
        show_columns = "ame_p": coxph, cph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ame_se" in `show_columns` are not available for mlogit models.
        i marginaleffects computes predictions but not slopes (average marginal effects) for mlogit's one-row-per-choice data structure.
      Received by 1:
        show_columns = "ame_se": mlogit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ame_se" in `show_columns` are not defined for Cox models.
        i Average marginal effects on a Cox proportional-hazards fit are on an ambiguous survival / hazard scale with unreliable standard errors. Report hazard ratios instead with `exponentiate = TRUE`.
        i For a design-based Cox model, test a term with `survey::regTermTest(fit, ~term)` and read a marginal survival curve with `survey::svykm()`.
      Received by 1:
        show_columns = "ame_se": svycoxph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ame_se" in `show_columns` are not defined for Cox models.
        i Average marginal effects on a Cox proportional-hazards fit are on an ambiguous survival / hazard scale with unreliable standard errors. Report hazard ratios instead with `exponentiate = TRUE`.
        i For absolute effects, use the survival estimand columns: `show_columns = c("b", "rmst")` with `tau = `, or `"risk_diff"` with `at_time = ` (coxph).
      Received by 2:
        show_columns = "ame_se": coxph, cph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ess_bulk" in `show_columns` are shown only when every model is Bayesian.
        i R-hat, ESS and MCSE are sampler diagnostics of the posterior draws; frequentist fits have none.
        i In a mixed table the automatic convergence guard already reports sampler problems per model in the footer.
      Received by 39:
        show_columns = "ess_bulk": bam, betareg, clm, clmm, coxph, cph, feols,
            flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gaussian, glmer,
            glmmTMB, gls, hurdle, iv_robust, ivreg, lm, lm_robust, lme, lmer,
            lrm, mlogit, multinom, nls, ols, polr, rlm, rms_Glm, rq, selection,
            survreg, svycoxph, svyglm, svyolr, tobit, zeroinfl
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ess_tail" in `show_columns` are shown only when every model is Bayesian.
        i R-hat, ESS and MCSE are sampler diagnostics of the posterior draws; frequentist fits have none.
        i In a mixed table the automatic convergence guard already reports sampler problems per model in the footer.
      Received by 39:
        show_columns = "ess_tail": bam, betareg, clm, clmm, coxph, cph, feols,
            flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gaussian, glmer,
            glmmTMB, gls, hurdle, iv_robust, ivreg, lm, lm_robust, lme, lmer,
            lrm, mlogit, multinom, nls, ols, polr, rlm, rms_Glm, rq, selection,
            survreg, svycoxph, svyglm, svyolr, tobit, zeroinfl
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "mcse" in `show_columns` are shown only when every model is Bayesian.
        i R-hat, ESS and MCSE are sampler diagnostics of the posterior draws; frequentist fits have none.
        i In a mixed table the automatic convergence guard already reports sampler problems per model in the footer.
      Received by 39:
        show_columns = "mcse": bam, betareg, clm, clmm, coxph, cph, feols,
            flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gaussian, glmer,
            glmmTMB, gls, hurdle, iv_robust, ivreg, lm, lm_robust, lme, lmer,
            lrm, mlogit, multinom, nls, ols, polr, rlm, rms_Glm, rq, selection,
            survreg, svycoxph, svyglm, svyolr, tobit, zeroinfl
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_chi2" in `show_columns` are not defined for GEE fits (`geeglm`).
        i The partial likelihood-ratio chi-square, the partial F and the variance-explained partials need a true likelihood, a deviance or an OLS variance partition; GEE has none of them.
      Received by 1:
        show_columns = "partial_chi2": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_eta2" in `show_columns` are not defined for GEE fits (`geeglm`).
        i The partial likelihood-ratio chi-square, the partial F and the variance-explained partials need a true likelihood, a deviance or an OLS variance partition; GEE has none of them.
      Received by 1:
        show_columns = "partial_eta2": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_eta2" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The `glm`-appropriate analog is `"partial_chi2"` (Type-II partial likelihood-ratio chi-square; Long & Freese 2014 <U+00A7>3.2.2, <U+00A7>3.2.4).
        i Replace with `"partial_chi2"` or drop the token.
      Received by 7:
        show_columns = "partial_eta2": glm.nb, glm_logit, glm_poisson,
            glm_probit, lrm, rms_Glm, svyglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_eta2" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The analog for a `glm` whose dispersion is estimated is `"partial_f"`, the partial F test that `anova(reduced, full, test = "F")` reports.
        i Replace with `"partial_f"` or drop the token.
      Received by 3:
        show_columns = "partial_eta2": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_eta2" in `show_columns` are not defined for `mgcv` GAMs.
        i spicy computes no term-level effect size or test for a GAM: `summary()` of the GAM gives the tests of its terms.
      Received by 3:
        show_columns = "partial_eta2": bam, gam, gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_eta2" in `show_columns` are not defined for mixed-effects models.
        i Variance-explained partials do not apply to mixed models. For a partial Wald chi-square test of each term, use `"partial_chi2"`.
      Received by 5:
        show_columns = "partial_eta2": clmm, glmer, glmmTMB, lme, lmer
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_eta2_ci" in `show_columns` are not defined for GEE fits (`geeglm`).
        i The partial likelihood-ratio chi-square, the partial F and the variance-explained partials need a true likelihood, a deviance or an OLS variance partition; GEE has none of them.
      Received by 1:
        show_columns = "partial_eta2_ci": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_eta2_ci" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The `glm`-appropriate analog is `"partial_chi2"` (Type-II partial likelihood-ratio chi-square; Long & Freese 2014 <U+00A7>3.2.2, <U+00A7>3.2.4).
        i Replace with `"partial_chi2"` or drop the token.
      Received by 7:
        show_columns = "partial_eta2_ci": glm.nb, glm_logit, glm_poisson,
            glm_probit, lrm, rms_Glm, svyglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_eta2_ci" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The analog for a `glm` whose dispersion is estimated is `"partial_f"`, the partial F test that `anova(reduced, full, test = "F")` reports.
        i Replace with `"partial_f"` or drop the token.
      Received by 3:
        show_columns = "partial_eta2_ci": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_eta2_ci" in `show_columns` are not defined for `mgcv` GAMs.
        i spicy computes no term-level effect size or test for a GAM: `summary()` of the GAM gives the tests of its terms.
      Received by 3:
        show_columns = "partial_eta2_ci": bam, gam, gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_eta2_ci" in `show_columns` are not defined for mixed-effects models.
        i Variance-explained partials do not apply to mixed models. For a partial Wald chi-square test of each term, use `"partial_chi2"`.
      Received by 5:
        show_columns = "partial_eta2_ci": clmm, glmer, glmmTMB, lme, lmer
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_f" in `show_columns` are not defined for GEE fits (`geeglm`).
        i The partial likelihood-ratio chi-square, the partial F and the variance-explained partials need a true likelihood, a deviance or an OLS variance partition; GEE has none of them.
      Received by 1:
        show_columns = "partial_f": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_f2" in `show_columns` are not defined for GEE fits (`geeglm`).
        i The partial likelihood-ratio chi-square, the partial F and the variance-explained partials need a true likelihood, a deviance or an OLS variance partition; GEE has none of them.
      Received by 1:
        show_columns = "partial_f2": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_f2" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The `glm`-appropriate analog is `"partial_chi2"` (Type-II partial likelihood-ratio chi-square; Long & Freese 2014 <U+00A7>3.2.2, <U+00A7>3.2.4).
        i Replace with `"partial_chi2"` or drop the token.
      Received by 7:
        show_columns = "partial_f2": glm.nb, glm_logit, glm_poisson, glm_probit,
            lrm, rms_Glm, svyglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_f2" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The analog for a `glm` whose dispersion is estimated is `"partial_f"`, the partial F test that `anova(reduced, full, test = "F")` reports.
        i Replace with `"partial_f"` or drop the token.
      Received by 3:
        show_columns = "partial_f2": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_f2" in `show_columns` are not defined for `mgcv` GAMs.
        i spicy computes no term-level effect size or test for a GAM: `summary()` of the GAM gives the tests of its terms.
      Received by 3:
        show_columns = "partial_f2": bam, gam, gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_f2" in `show_columns` are not defined for mixed-effects models.
        i Variance-explained partials do not apply to mixed models. For a partial Wald chi-square test of each term, use `"partial_chi2"`.
      Received by 5:
        show_columns = "partial_f2": clmm, glmer, glmmTMB, lme, lmer
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_f2_ci" in `show_columns` are not defined for GEE fits (`geeglm`).
        i The partial likelihood-ratio chi-square, the partial F and the variance-explained partials need a true likelihood, a deviance or an OLS variance partition; GEE has none of them.
      Received by 1:
        show_columns = "partial_f2_ci": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_f2_ci" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The `glm`-appropriate analog is `"partial_chi2"` (Type-II partial likelihood-ratio chi-square; Long & Freese 2014 <U+00A7>3.2.2, <U+00A7>3.2.4).
        i Replace with `"partial_chi2"` or drop the token.
      Received by 7:
        show_columns = "partial_f2_ci": glm.nb, glm_logit, glm_poisson,
            glm_probit, lrm, rms_Glm, svyglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_f2_ci" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The analog for a `glm` whose dispersion is estimated is `"partial_f"`, the partial F test that `anova(reduced, full, test = "F")` reports.
        i Replace with `"partial_f"` or drop the token.
      Received by 3:
        show_columns = "partial_f2_ci": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_f2_ci" in `show_columns` are not defined for `mgcv` GAMs.
        i spicy computes no term-level effect size or test for a GAM: `summary()` of the GAM gives the tests of its terms.
      Received by 3:
        show_columns = "partial_f2_ci": bam, gam, gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_f2_ci" in `show_columns` are not defined for mixed-effects models.
        i Variance-explained partials do not apply to mixed models. For a partial Wald chi-square test of each term, use `"partial_chi2"`.
      Received by 5:
        show_columns = "partial_f2_ci": clmm, glmer, glmmTMB, lme, lmer
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_omega2" in `show_columns` are not defined for GEE fits (`geeglm`).
        i The partial likelihood-ratio chi-square, the partial F and the variance-explained partials need a true likelihood, a deviance or an OLS variance partition; GEE has none of them.
      Received by 1:
        show_columns = "partial_omega2": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_omega2" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The `glm`-appropriate analog is `"partial_chi2"` (Type-II partial likelihood-ratio chi-square; Long & Freese 2014 <U+00A7>3.2.2, <U+00A7>3.2.4).
        i Replace with `"partial_chi2"` or drop the token.
      Received by 7:
        show_columns = "partial_omega2": glm.nb, glm_logit, glm_poisson,
            glm_probit, lrm, rms_Glm, svyglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_omega2" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The analog for a `glm` whose dispersion is estimated is `"partial_f"`, the partial F test that `anova(reduced, full, test = "F")` reports.
        i Replace with `"partial_f"` or drop the token.
      Received by 3:
        show_columns = "partial_omega2": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_omega2" in `show_columns` are not defined for `mgcv` GAMs.
        i spicy computes no term-level effect size or test for a GAM: `summary()` of the GAM gives the tests of its terms.
      Received by 3:
        show_columns = "partial_omega2": bam, gam, gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_omega2" in `show_columns` are not defined for mixed-effects models.
        i Variance-explained partials do not apply to mixed models. For a partial Wald chi-square test of each term, use `"partial_chi2"`.
      Received by 5:
        show_columns = "partial_omega2": clmm, glmer, glmmTMB, lme, lmer
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_omega2_ci" in `show_columns` are not defined for GEE fits (`geeglm`).
        i The partial likelihood-ratio chi-square, the partial F and the variance-explained partials need a true likelihood, a deviance or an OLS variance partition; GEE has none of them.
      Received by 1:
        show_columns = "partial_omega2_ci": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_omega2_ci" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The `glm`-appropriate analog is `"partial_chi2"` (Type-II partial likelihood-ratio chi-square; Long & Freese 2014 <U+00A7>3.2.2, <U+00A7>3.2.4).
        i Replace with `"partial_chi2"` or drop the token.
      Received by 7:
        show_columns = "partial_omega2_ci": glm.nb, glm_logit, glm_poisson,
            glm_probit, lrm, rms_Glm, svyglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_omega2_ci" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The analog for a `glm` whose dispersion is estimated is `"partial_f"`, the partial F test that `anova(reduced, full, test = "F")` reports.
        i Replace with `"partial_f"` or drop the token.
      Received by 3:
        show_columns = "partial_omega2_ci": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_omega2_ci" in `show_columns` are not defined for `mgcv` GAMs.
        i spicy computes no term-level effect size or test for a GAM: `summary()` of the GAM gives the tests of its terms.
      Received by 3:
        show_columns = "partial_omega2_ci": bam, gam, gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_omega2_ci" in `show_columns` are not defined for mixed-effects models.
        i Variance-explained partials do not apply to mixed models. For a partial Wald chi-square test of each term, use `"partial_chi2"`.
      Received by 5:
        show_columns = "partial_omega2_ci": clmm, glmer, glmmTMB, lme, lmer
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `Glm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "r2": rms_Glm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `betareg` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": betareg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `clm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": clm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `clmm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": clmm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `coxph` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": coxph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `cph` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": cph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `fixest` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": feols
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `flexsurvreg` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": flexsurvreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `geeglm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "r2": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `glm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "r2": glm_gaussian
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `glmerMod` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": glmer
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `glmmTMB` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": glmmTMB
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `gls` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": gls
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `hurdle` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": hurdle
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `iv_robust` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": iv_robust
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `ivreg` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": ivreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `lm_robust` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": lm_robust
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `lme` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": lme
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `lmerMod` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": lmer
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `lrm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "r2": lrm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `mgcv` GAMs.
        i The column reports the R<U+00B2> of each fit of a univariable screen of least-squares models. For a GAM, ask for a fit-statistics row in `show_fit_stats`.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`, `"r2_deviance"`.
      Received by 1:
        show_columns = "r2": bam
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `mgcv` GAMs.
        i The column reports the R<U+00B2> of each fit of a univariable screen of least-squares models. For a GAM, ask for a fit-statistics row in `show_fit_stats`.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_tjur"`, `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`.
      Received by 1:
        show_columns = "r2": gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `mgcv` GAMs.
        i The column reports the R<U+00B2> of each fit of a univariable screen of least-squares models. For a GAM, ask for a fit-statistics row in `show_fit_stats`.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"r2_deviance"`.
      Received by 1:
        show_columns = "r2": gam
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `mlogit` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": mlogit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `multinom` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": multinom
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `negbin` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "r2": glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `nls` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": nls
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `polr` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": polr
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `rq` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": rq
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `selection` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": selection
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `survreg` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": survreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `svycoxph` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": svycoxph
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `svyglm` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "r2": svyglm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `svyolr` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `tobit` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": tobit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `zeroinfl` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": zeroinfl
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2_change", "f_change", "p_change" in `show_fit_stats` are not defined for `rlm` models.
        i M-estimation minimizes a bounded loss rather than a likelihood or a residual sum of squares, so an rlm hierarchy has neither a likelihood-ratio test nor a partial F to report.
        i Drop the token, or compare the least-squares fits (`lm()`) if the hierarchy itself is the question.
      Received by 1:
        nested = TRUE: rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "rhat" in `show_columns` are shown only when every model is Bayesian.
        i R-hat, ESS and MCSE are sampler diagnostics of the posterior draws; frequentist fits have none.
        i In a mixed table the automatic convergence guard already reports sampler problems per model in the footer.
      Received by 39:
        show_columns = "rhat": bam, betareg, clm, clmm, coxph, cph, feols,
            flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gaussian, glmer,
            glmmTMB, gls, hurdle, iv_robust, ivreg, lm, lm_robust, lme, lmer,
            lrm, mlogit, multinom, nls, ols, polr, rlm, rms_Glm, rq, selection,
            survreg, svycoxph, svyglm, svyolr, tobit, zeroinfl
      ----
      error: spicy_invalid_input, spicy_error
        `"beta"` is in `show_columns` but `standardized = "none"`.
        i Set `standardized` to one of: "refit", "posthoc", "basic", "smart", "pseudo" ("pseudo" is `glm` only).
        i Or remove `"beta"` from `show_columns`.
      Received by 39:
        show_columns = "beta": bam, betareg, clm, clmm, coxph, cph, feols,
            flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gaussian, glmer,
            glmmTMB, gls, hurdle, iv_robust, ivreg, lm, lm_robust, lme, lmer,
            lrm, mlogit, multinom, nls, ols, polr, rlm, rms_Glm, rq, selection,
            survreg, svycoxph, svyglm, svyolr, tobit, zeroinfl
      ----
      error: spicy_invalid_input, spicy_error
        `exponentiate = TRUE` is not meaningful for a model with link "probit" (Model 1).
        i Probit coefficients are shifts on a latent standard-normal scale; their exponential has no interpretation.
        i exp(B) is a ratio only for logit (odds ratio), log (rate / risk / mean ratio), and binomial or ordinal cloglog (hazard ratio) links.
        i Drop `exponentiate = TRUE` (it applies to every model in the table) or report response-scale effects via the AME column (`show_columns = c("b", "ame")`).
      Received by 1:
        exponentiate = TRUE: glm_probit
      ----
      error: spicy_invalid_input, spicy_error
        `nested = TRUE` cannot compare REML fits whose fixed effects differ.
        i The restricted likelihood depends on the fixed-effects design matrix, so the two criteria are not on a common scale and the likelihood-ratio test is not valid (Pinheiro & Bates 2000, Section 2.4.2).
        i Refit both models by maximum likelihood, e.g. `update(m1, REML = FALSE)`, then compare.
        i REML comparisons stay valid when only the random structure differs.
      Received by 1:
        nested = TRUE: glmmTMB_reml
      ----
      error: spicy_invalid_input, spicy_error
        `nested = TRUE` cannot compare REML fits whose fixed effects differ.
        i The restricted likelihood depends on the fixed-effects design matrix, so the two criteria are not on a common scale and the likelihood-ratio test is not valid (Pinheiro & Bates 2000, Section 2.4.2).
        i Refit both models by maximum likelihood, e.g. `update(m1, method = "ML")`, then compare.
        i REML comparisons stay valid when only the random structure differs.
      Received by 3:
        nested = TRUE: gls, lme, lme_contrasts
      ----
      error: spicy_invalid_input, spicy_error
        `nested = TRUE` is not available for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: there is no likelihood-ratio change test.
        i Compare working models with QIC (`show_fit_stats = c("nobs", "qic")`), or use `geepack::anova.geeglm()` Wald tests outside the table.
      Received by 1:
        nested = TRUE: geeglm
      ----
      error: spicy_invalid_input, spicy_error
        `nested = TRUE` is not available for `clmm`.
        i The change statistics of a hierarchy (<U+0394>R<U+00B2>, partial F, LRT, <U+0394>AIC) all need a comparison of nested fits that this class does not provide here, and a hierarchy compares adjacent PAIRS -- one model that cannot be compared removes the comparison, it does not blank one column.
        i Render the models side by side with `nested = FALSE`.
      Received by 1:
        nested = TRUE: clmm
      ----
      error: spicy_invalid_input, spicy_error
        `nested = TRUE` is not available for `iv_robust`.
        i The change statistics of a hierarchy (<U+0394>R<U+00B2>, partial F, LRT, <U+0394>AIC) all need a comparison of nested fits that this class does not provide here, and a hierarchy compares adjacent PAIRS -- one model that cannot be compared removes the comparison, it does not blank one column.
        i Render the models side by side with `nested = FALSE`.
      Received by 1:
        nested = TRUE: iv_robust
      ----
      error: spicy_invalid_input, spicy_error
        `nested = TRUE` is not available for `ivreg`.
        i The change statistics of a hierarchy (<U+0394>R<U+00B2>, partial F, LRT, <U+0394>AIC) all need a comparison of nested fits that this class does not provide here, and a hierarchy compares adjacent PAIRS -- one model that cannot be compared removes the comparison, it does not blank one column.
        i Render the models side by side with `nested = FALSE`.
      Received by 1:
        nested = TRUE: ivreg
      ----
      error: spicy_invalid_input, spicy_error
        `nested = TRUE` is not available for `lm_robust`.
        i The change statistics of a hierarchy (<U+0394>R<U+00B2>, partial F, LRT, <U+0394>AIC) all need a comparison of nested fits that this class does not provide here, and a hierarchy compares adjacent PAIRS -- one model that cannot be compared removes the comparison, it does not blank one column.
        i Render the models side by side with `nested = FALSE`.
      Received by 1:
        nested = TRUE: lm_robust
      ----
      error: spicy_invalid_input, spicy_error
        `nested = TRUE` is not available for `svyglm`.
        i The change statistics of a hierarchy (<U+0394>R<U+00B2>, partial F, LRT, <U+0394>AIC) all need a comparison of nested fits that this class does not provide here, and a hierarchy compares adjacent PAIRS -- one model that cannot be compared removes the comparison, it does not blank one column.
        i Render the models side by side with `nested = FALSE`.
        i For a design-based fit, test a term with `survey::regTermTest(fit, ~term)`. Two nested `svyglm()` or `svycoxph()` fits are compared by `anova(fit0, fit)`.
      Received by 3:
        nested = TRUE: svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        `re_ci = "profile"` is available for lme4 fits (`lmer` / `glmer`) only.
        x Model 1 is not fitted with lme4.
        i glmmTMB, nlme::lme and ordinal::clmm variance-component CIs already come from their own engines (TMB's sdreport; nlme's apVar; the clmm Hessian) under the default `re_ci = "wald"`.
      Received by 3:
        re_ci = "profile": clmm, glmmTMB, lme
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (Glm).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": rms_Glm
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (bam).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": bam
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (betareg).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": betareg
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (clm).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": clm
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (clmm).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": clmm
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (cph).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": cph
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (fixest).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": feols
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (flexsurvreg).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": flexsurvreg
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (gam).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 2:
        show_columns = "n_events": gam, gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (geeglm).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (glm).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 4:
        show_columns = "n_events": glm_gamma, glm_gaussian, glm_poisson,
            glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (glmmTMB).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": glmmTMB
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (gls).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": gls
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (hurdle).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": hurdle
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (iv_robust).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": iv_robust
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (ivreg).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": ivreg
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (lm).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": lm
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (lm_robust).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": lm_robust
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (lme).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": lme
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (lmerMod).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": lmer
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (lrm).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": lrm
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (mlogit).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": mlogit
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (multinom).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": multinom
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (negbin).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (nls).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": nls
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (ols).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": ols
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (polr).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": polr
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (rlm).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": rlm
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (rq).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": rq
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (selection).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": selection
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (survreg).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": survreg
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (svyglm).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": svyglm
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (svyolr).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": svyolr
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (tobit).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": tobit
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (zeroinfl).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": zeroinfl
      ----
      error: spicy_invalid_input, spicy_error
        `standardized = "pseudo"` (Menard 2011 fully-standardized) is defined for `glm` only.
        i Use `standardized = "refit"` for `lm` (refit on z-scored data, Cohen et al. 2003) or one of `"posthoc"` / `"basic"` / `"smart"` (algebraic scaling on the fitted model).
      Received by 3:
        standardized = "pseudo" (with beta): lm, ols, rlm
      ----
      error: spicy_unsupported, spicy_error
        RMST / risk-difference columns are not available for a design-based Cox fit.
        x Their uncertainty comes from resampling subjects to rebuild the baseline hazard, and a row bootstrap ignores the strata and clusters the survey design declares. For a contrast between clusters, that understates the standard error by roughly 1.5x-6x depending on cluster count and size (~3x on a 30 x 25 design with moderate ICC); for a contrast within clusters, or with no clustering, it is about right.
        i The estimands themselves are not in question: the RMST and risk differences are validated against exact oracles for an unweighted Cox fit.
        i For a marginal survival curve under the design, use `survey::svykm()`; for a design-based test of a term, `survey::regTermTest(fit, ~term)`.
      Received by 8:
        show_columns = "risk_diff": svycoxph
        show_columns = "risk_diff_ci": svycoxph
        show_columns = "risk_diff_p": svycoxph
        show_columns = "risk_diff_se": svycoxph
        show_columns = "rmst": svycoxph
        show_columns = "rmst_ci": svycoxph
        show_columns = "rmst_p": svycoxph
        show_columns = "rmst_se": svycoxph
      ----
      error: spicy_unsupported, spicy_error
        `re_test = "rlrt"` (exact restricted LRT) is only defined for a Gaussian `lmer` / `lme` fit with a single variance component.
        i Use `re_test = "lrt"` (chi-bar-squared LR test) for richer random structures.
      Received by 3:
        re_test = "rlrt": clmm, glmer, glmmTMB
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not available for GEE fits (`geeglm`).
        i There is no established convention for standardizing population-averaged coefficients: the response SD mixes within- and between-cluster variance.
        i Standardize predictors before fitting to compare effect magnitudes.
      Received by 1:
        standardized = "basic" (with beta): geeglm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not available for a `glmmTMB` fit with the ordinal family.
        i An ordinal outcome has no scale to standardize, as for `ordinal::clm()` and `ordinal::clmm()` fits.
      Received by 1:
        standardized = "basic" (with beta): glmmTMB_ordinal
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (Glm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): rms_Glm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (bam).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): bam
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (betareg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): betareg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (clm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): clm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (clmm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): clmm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (coxph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): coxph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (cph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): cph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (fixest).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): feols
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (flexsurvreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): flexsurvreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (gam).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 2:
        standardized = "basic" (with beta): gam, gam_logit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (gls).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): gls
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (hurdle).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): hurdle
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (iv_robust).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): iv_robust
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (ivreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): ivreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (lm_robust).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): lm_robust
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (lrm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): lrm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (mlogit).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): mlogit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (multinom).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): multinom
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (nls).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): nls
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (ols).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): ols
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (polr).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): polr
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (rlm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): rlm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (rq).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): rq
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (selection).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): selection
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (survreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): survreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (svycoxph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): svycoxph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (svyglm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): svyglm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (svyolr).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): svyolr
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (tobit).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): tobit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "basic"` is not supported for this model class (zeroinfl).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "basic" (with beta): zeroinfl
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not available for GEE fits (`geeglm`).
        i There is no established convention for standardizing population-averaged coefficients: the response SD mixes within- and between-cluster variance.
        i Standardize predictors before fitting to compare effect magnitudes.
      Received by 1:
        standardized = "posthoc" (with beta): geeglm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not available for a `glmmTMB` fit with the ordinal family.
        i An ordinal outcome has no scale to standardize, as for `ordinal::clm()` and `ordinal::clmm()` fits.
      Received by 1:
        standardized = "posthoc" (with beta): glmmTMB_ordinal
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (Glm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): rms_Glm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (bam).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): bam
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (betareg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): betareg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (clm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): clm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (clmm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): clmm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (coxph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): coxph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (cph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): cph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (fixest).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): feols
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (flexsurvreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): flexsurvreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (gam).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 2:
        standardized = "posthoc" (with beta): gam, gam_logit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (gls).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): gls
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (hurdle).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): hurdle
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (iv_robust).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): iv_robust
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (ivreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): ivreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (lm_robust).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): lm_robust
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (lrm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): lrm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (mlogit).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): mlogit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (multinom).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): multinom
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (nls).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): nls
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (ols).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): ols
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (polr).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): polr
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (rlm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): rlm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (rq).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): rq
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (selection).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): selection
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (survreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): survreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (svycoxph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): svycoxph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (svyglm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): svyglm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (svyolr).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): svyolr
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (tobit).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): tobit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "posthoc"` is not supported for this model class (zeroinfl).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "posthoc" (with beta): zeroinfl
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not available for GEE fits (`geeglm`).
        i There is no established convention for standardizing population-averaged coefficients: the response SD mixes within- and between-cluster variance.
        i Standardize predictors before fitting to compare effect magnitudes.
      Received by 1:
        standardized = "pseudo" (with beta): geeglm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not available for a `glmmTMB` fit with the ordinal family.
        i An ordinal outcome has no scale to standardize, as for `ordinal::clm()` and `ordinal::clmm()` fits.
      Received by 1:
        standardized = "pseudo" (with beta): glmmTMB_ordinal
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (Glm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): rms_Glm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (bam).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): bam
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (betareg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): betareg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (clm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): clm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (clmm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): clmm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (coxph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): coxph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (cph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): cph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (fixest).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): feols
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (flexsurvreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): flexsurvreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (gam).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 2:
        standardized = "pseudo" (with beta): gam, gam_logit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (gls).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): gls
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (hurdle).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): hurdle
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (iv_robust).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): iv_robust
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (ivreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): ivreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (lm_robust).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): lm_robust
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (lrm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): lrm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (mlogit).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): mlogit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (multinom).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): multinom
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (nls).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): nls
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (polr).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): polr
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (rq).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): rq
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (selection).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): selection
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (survreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): survreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (svycoxph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): svycoxph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (svyglm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): svyglm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (svyolr).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): svyolr
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (tobit).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): tobit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not supported for this model class (zeroinfl).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "pseudo" (with beta): zeroinfl
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not available for GEE fits (`geeglm`).
        i There is no established convention for standardizing population-averaged coefficients: the response SD mixes within- and between-cluster variance.
        i Standardize predictors before fitting to compare effect magnitudes.
      Received by 1:
        standardized = "refit" (with beta): geeglm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not available for a `glmmTMB` fit with the ordinal family.
        i An ordinal outcome has no scale to standardize, as for `ordinal::clm()` and `ordinal::clmm()` fits.
      Received by 1:
        standardized = "refit" (with beta): glmmTMB_ordinal
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (Glm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): rms_Glm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (bam).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): bam
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (betareg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): betareg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (clm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): clm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (clmm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): clmm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (coxph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): coxph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (cph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): cph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (fixest).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): feols
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (flexsurvreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): flexsurvreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (gam).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 2:
        standardized = "refit" (with beta): gam, gam_logit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (gls).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): gls
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (hurdle).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): hurdle
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (iv_robust).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): iv_robust
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (ivreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): ivreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (lm_robust).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): lm_robust
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (lrm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): lrm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (mlogit).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): mlogit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (multinom).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): multinom
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (nls).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): nls
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (ols).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): ols
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (polr).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): polr
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (rlm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): rlm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (rq).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): rq
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (selection).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): selection
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (survreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): survreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (svycoxph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): svycoxph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (svyglm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): svyglm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (svyolr).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): svyolr
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (tobit).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): tobit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not supported for this model class (zeroinfl).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "refit" (with beta): zeroinfl
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not available for GEE fits (`geeglm`).
        i There is no established convention for standardizing population-averaged coefficients: the response SD mixes within- and between-cluster variance.
        i Standardize predictors before fitting to compare effect magnitudes.
      Received by 1:
        standardized = "smart" (with beta): geeglm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not available for a `glmmTMB` fit with the ordinal family.
        i An ordinal outcome has no scale to standardize, as for `ordinal::clm()` and `ordinal::clmm()` fits.
      Received by 1:
        standardized = "smart" (with beta): glmmTMB_ordinal
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (Glm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): rms_Glm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (bam).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): bam
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (betareg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): betareg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (clm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): clm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (clmm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): clmm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (coxph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): coxph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (cph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): cph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (fixest).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): feols
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (flexsurvreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): flexsurvreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (gam).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 2:
        standardized = "smart" (with beta): gam, gam_logit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (gls).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): gls
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (hurdle).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): hurdle
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (iv_robust).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): iv_robust
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (ivreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): ivreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (lm_robust).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): lm_robust
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (lrm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): lrm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (mlogit).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): mlogit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (multinom).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): multinom
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (nls).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): nls
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (ols).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): ols
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (polr).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): polr
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (rlm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): rlm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (rq).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): rq
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (selection).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): selection
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (survreg).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): survreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (svycoxph).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): svycoxph
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (svyglm).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): svyglm
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (svyolr).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): svyolr
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (tobit).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): tobit
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "smart"` is not supported for this model class (zeroinfl).
        i Standardized coefficients are available for `lm`, `glm` (incl. `MASS::glm.nb`), mixed models (`lmer` / `glmer` / `glmmTMB` / `nlme::lme`), and fixed-effects `stan_glm`-style fits ("posthoc" / "basic" / "smart").
        i To compare predictor effects in other classes, use average marginal effects where supported: `show_columns = c("b", "ame")`.
      Received by 1:
        standardized = "smart" (with beta): zeroinfl
      ----
      warning: spicy_caveat, spicy_warning
        Model 2 is a gaussian / identity glm.
        i This is mathematically equivalent to `lm()`. Refitting with `lm()` gives access to the variance-explained partial effect sizes (partial_f2 / <U+03B7><U+00B2> / <U+03C9><U+00B2>) and to the Satterthwaite-corrected AME path under CR* variance, neither of which is defined for the glm route.
      Received by 1:
        nested = TRUE: glm_gaussian
      ----
      warning: spicy_caveat, spicy_warning
        Singular fit (Model 1, Model 2): the random-effect structure is estimated at or near the boundary of the parameter space -- a variance of 0, or a correlation of <U+00B1>1.
        i Consider simplifying the random structure (drop the offending term), or test whether it belongs with `re_test = "lrt"`.
      Received by 1:
        nested = TRUE: glmmTMB_zi
      ----
      warning: spicy_caveat, spicy_warning
        Singular fit (Model 1, Model 2): the random-effect structure is estimated at or near the boundary of the parameter space -- a variance of 0, or a correlation of <U+00B1>1.
        i Consider simplifying the random structure (drop the offending term), or test whether it belongs with `re_test = "lrt"`. See `help("isSingular", package = "lme4")`.
      Received by 1:
        nested = TRUE: glmer
      ----
      warning: spicy_caveat, spicy_warning
        Singular fit (Model 2): the random-effect structure is estimated at or near the boundary of the parameter space -- a variance of 0, or a correlation of <U+00B1>1.
        i Consider simplifying the random structure (drop the offending term), or test whether it belongs with `re_test = "lrt"`.
      Received by 1:
        nested = TRUE: glmmTMB_hurdle
      ----
      warning: spicy_caveat, spicy_warning
        `standardized = "pseudo"` (Menard fully-standardized) is defined for binomial families with logit / probit / cloglog / log links.
        i Family `Gamma` (link `log`) is outside this scope; <U+03B2> returned as NA.
        i For non-binomial glms, use `standardized = "refit"` (x-standardization, Long & Freese 2014 <U+00A7>4.7.2) or one of `"posthoc"` / `"basic"` / `"smart"` (X-only scaling).
      Received by 1:
        standardized = "pseudo" (with beta): glm_gamma
      ----
      warning: spicy_caveat, spicy_warning
        `standardized = "pseudo"` (Menard fully-standardized) is defined for binomial families with logit / probit / cloglog / log links.
        i Family `Negative Binomial(1.4352)` (link `log`) is outside this scope; <U+03B2> returned as NA.
        i For non-binomial glms, use `standardized = "refit"` (x-standardization, Long & Freese 2014 <U+00A7>4.7.2) or one of `"posthoc"` / `"basic"` / `"smart"` (X-only scaling).
      Received by 1:
        standardized = "pseudo" (with beta): glm.nb
      ----
      warning: spicy_caveat, spicy_warning
        `standardized = "pseudo"` (Menard fully-standardized) is defined for binomial families with logit / probit / cloglog / log links.
        i Family `gaussian` (link `identity`) is outside this scope; <U+03B2> returned as NA.
        i For non-binomial glms, use `standardized = "refit"` (x-standardization, Long & Freese 2014 <U+00A7>4.7.2) or one of `"posthoc"` / `"basic"` / `"smart"` (X-only scaling).
      Received by 1:
        standardized = "pseudo" (with beta): glm_gaussian
      ----
      warning: spicy_caveat, spicy_warning
        `standardized = "pseudo"` (Menard fully-standardized) is defined for binomial families with logit / probit / cloglog / log links.
        i Family `poisson` (link `log`) is outside this scope; <U+03B2> returned as NA.
        i For non-binomial glms, use `standardized = "refit"` (x-standardization, Long & Freese 2014 <U+00A7>4.7.2) or one of `"posthoc"` / `"basic"` / `"smart"` (X-only scaling).
      Received by 1:
        standardized = "pseudo" (with beta): glm_poisson
      ----
      warning: spicy_caveat, spicy_warning
        `standardized = "pseudo"` (Menard fully-standardized) is defined for binomial families with logit / probit / cloglog / log links.
        i Family `quasipoisson` (link `log`) is outside this scope; <U+03B2> returned as NA.
        i For non-binomial glms, use `standardized = "refit"` (x-standardization, Long & Freese 2014 <U+00A7>4.7.2) or one of `"posthoc"` / `"basic"` / `"smart"` (X-only scaling).
      Received by 1:
        standardized = "pseudo" (with beta): glm_quasi
      ----
      warning: spicy_fallback, spicy_warning
        `standardized = "basic"` is not yet implemented for mixed-effects fits; falling back to "refit".
        i Refit standardizes the variables and refits the model, which avoids the ambiguous sd(Y) decomposition that the algebraic methods rely on.
      Received by 4:
        standardized = "basic" (with beta): glmer, glmmTMB, lme, lmer
      ----
      warning: spicy_fallback, spicy_warning
        `standardized = "posthoc"` is not yet implemented for mixed-effects fits; falling back to "refit".
        i Refit standardizes the variables and refits the model, which avoids the ambiguous sd(Y) decomposition that the algebraic methods rely on.
      Received by 4:
        standardized = "posthoc" (with beta): glmer, glmmTMB, lme, lmer
      ----
      warning: spicy_fallback, spicy_warning
        `standardized = "pseudo"` is not yet implemented for mixed-effects fits; falling back to "refit".
        i Refit standardizes the variables and refits the model, which avoids the ambiguous sd(Y) decomposition that the algebraic methods rely on.
      Received by 4:
        standardized = "pseudo" (with beta): glmer, glmmTMB, lme, lmer
      ----
      warning: spicy_fallback, spicy_warning
        `standardized = "smart"` is not yet implemented for mixed-effects fits; falling back to "refit".
        i Refit standardizes the variables and refits the model, which avoids the ambiguous sd(Y) decomposition that the algebraic methods rely on.
      Received by 4:
        standardized = "smart" (with beta): glmer, glmmTMB, lme, lmer
      ----
      warning: spicy_ignored_arg, spicy_warning
        `exponentiate = TRUE` has no effect on a sample-selection model.
        i The selection equation of a sample-selection model is a probit and its outcome equation is linear (a probit for a binary outcome): exp(B) is no ratio on either, so its coefficients stay on their own scale.
      Received by 1:
        exponentiate = TRUE: selection
      ----
      warning: spicy_ignored_arg, spicy_warning
        `exponentiate = TRUE` has no effect on identity-link fits.
        i exp() is only applied to fits whose coefficients are on a log / logit scale (odds, rate, risk, hazard, or time ratios). Drop the argument for identity-link (gaussian) fits.
      Received by 17:
        exponentiate = TRUE: feols, gam, glm_gaussian, glmmTMB, gls, iv_robust,
            ivreg, lm, lm_robust, lme, lmer, nls, ols, rlm, rq, svyglm, tobit
      ----
      warning: spicy_ignored_arg, spicy_warning
        `re_ci = "profile"` has no effect: no model in the table has random effects.
        i The `re_*` arguments act on the random-effects rows of mixed models. Drop it for this table.
      Received by 34:
        re_ci = "profile": bam, betareg, clm, coxph, cph, feols, flexsurvreg,
            gam, gam_logit, geeglm, glm.nb, glm_gaussian, gls, hurdle, iv_robust,
            ivreg, lm, lm_robust, lrm, mlogit, multinom, nls, ols, polr, rlm,
            rms_Glm, rq, selection, survreg, svycoxph, svyglm, svyolr, tobit,
            zeroinfl
      ----
      warning: spicy_ignored_arg, spicy_warning
        `re_test = "lrt"` has no effect: no model in the table has random effects.
        i The `re_*` arguments act on the random-effects rows of mixed models. Drop it for this table.
      Received by 34:
        re_test = "lrt": bam, betareg, clm, coxph, cph, feols, flexsurvreg, gam,
            gam_logit, geeglm, glm.nb, glm_gaussian, gls, hurdle, iv_robust,
            ivreg, lm, lm_robust, lrm, mlogit, multinom, nls, ols, polr, rlm,
            rms_Glm, rq, selection, survreg, svycoxph, svyglm, svyolr, tobit,
            zeroinfl
      ----
      warning: spicy_ignored_arg, spicy_warning
        `re_test = "rlrt"` has no effect: no model in the table has random effects.
        i The `re_*` arguments act on the random-effects rows of mixed models. Drop it for this table.
      Received by 34:
        re_test = "rlrt": bam, betareg, clm, coxph, cph, feols, flexsurvreg, gam,
            gam_logit, geeglm, glm.nb, glm_gaussian, gls, hurdle, iv_robust,
            ivreg, lm, lm_robust, lrm, mlogit, multinom, nls, ols, polr, rlm,
            rms_Glm, rq, selection, survreg, svycoxph, svyglm, svyolr, tobit,
            zeroinfl

# the refusals and warnings of the argument sweep keep their text and class: Stan

    Code
      cat(.refusal_render(rows), sep = "\n")
    Output
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `brmsfit`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 8:
        show_columns = "partial_chi2": brmsfit
        show_columns = "partial_eta2": brmsfit
        show_columns = "partial_eta2_ci": brmsfit
        show_columns = "partial_f": brmsfit
        show_columns = "partial_f2": brmsfit
        show_columns = "partial_f2_ci": brmsfit
        show_columns = "partial_omega2": brmsfit
        show_columns = "partial_omega2_ci": brmsfit
      ----
      error: spicy_invalid_input, spicy_error
        Partial effect-size columns are not available for `stanreg`.
        i The term-level partial effect size is defined here for the least-squares families (`partial_f` / `partial_f2` / `partial_eta2` / `partial_omega2`) and, as a partial likelihood-ratio or Wald chi-square, for `glm` and the mixed-effects families (`partial_chi2`), or as a partial F test for a `glm` whose dispersion is estimated (`partial_f`). No such decomposition exists for this class (see ?table_regression_models).
        i Drop the partial effect-size token(s) from `show_columns`.
      Received by 2:
        show_columns = "partial_chi2": stanreg
        show_columns = "partial_f": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (brmsfit).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": brmsfit
        show_columns = "risk_diff_ci": brmsfit
        show_columns = "risk_diff_p": brmsfit
        show_columns = "risk_diff_se": brmsfit
        show_columns = "rmst": brmsfit
        show_columns = "rmst_ci": brmsfit
        show_columns = "rmst_p": brmsfit
        show_columns = "rmst_se": brmsfit
      ----
      error: spicy_invalid_input, spicy_error
        RMST / risk-difference columns are not available for this model class (stanreg).
        i They are computed by g-computation from a `survival::coxph` fit on right-censored single-record data, or a `survival::survreg` fit.
      Received by 8:
        show_columns = "risk_diff": stanreg
        show_columns = "risk_diff_ci": stanreg
        show_columns = "risk_diff_p": stanreg
        show_columns = "risk_diff_se": stanreg
        show_columns = "rmst": stanreg
        show_columns = "rmst_ci": stanreg
        show_columns = "rmst_p": stanreg
        show_columns = "rmst_se": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `brmsfit` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "adj_r2": brmsfit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "adj_r2" in `show_columns` are not defined for `stanreg` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "adj_r2": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "ame_p" in `show_columns` are not defined for Bayesian fits.
        i A posterior has no p-value or t-statistic (for coefficients or AMEs). The probability of direction (`"pd"`) is the closest posterior summary.
      Received by 2:
        show_columns = "ame_p": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_eta2" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The `glm`-appropriate analog is `"partial_chi2"` (Type-II partial likelihood-ratio chi-square; Long & Freese 2014 <U+00A7>3.2.2, <U+00A7>3.2.4).
        i Replace with `"partial_chi2"` or drop the token.
      Received by 1:
        show_columns = "partial_eta2": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_eta2_ci" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The `glm`-appropriate analog is `"partial_chi2"` (Type-II partial likelihood-ratio chi-square; Long & Freese 2014 <U+00A7>3.2.2, <U+00A7>3.2.4).
        i Replace with `"partial_chi2"` or drop the token.
      Received by 1:
        show_columns = "partial_eta2_ci": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_f2" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The `glm`-appropriate analog is `"partial_chi2"` (Type-II partial likelihood-ratio chi-square; Long & Freese 2014 <U+00A7>3.2.2, <U+00A7>3.2.4).
        i Replace with `"partial_chi2"` or drop the token.
      Received by 1:
        show_columns = "partial_f2": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_f2_ci" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The `glm`-appropriate analog is `"partial_chi2"` (Type-II partial likelihood-ratio chi-square; Long & Freese 2014 <U+00A7>3.2.2, <U+00A7>3.2.4).
        i Replace with `"partial_chi2"` or drop the token.
      Received by 1:
        show_columns = "partial_f2_ci": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_omega2" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The `glm`-appropriate analog is `"partial_chi2"` (Type-II partial likelihood-ratio chi-square; Long & Freese 2014 <U+00A7>3.2.2, <U+00A7>3.2.4).
        i Replace with `"partial_chi2"` or drop the token.
      Received by 1:
        show_columns = "partial_omega2": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "partial_omega2_ci" in `show_columns` are not defined for `glm` models.
        i Variance-explained partition (R<U+00B2> / partial f<U+00B2> / <U+03B7><U+00B2> / <U+03C9><U+00B2>) does not apply outside the least-squares framework. The `glm`-appropriate analog is `"partial_chi2"` (Type-II partial likelihood-ratio chi-square; Long & Freese 2014 <U+00A7>3.2.2, <U+00A7>3.2.4).
        i Replace with `"partial_chi2"` or drop the token.
      Received by 1:
        show_columns = "partial_omega2_ci": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `brmsfit` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Report the model's own summary instead (for `coxph`, the likelihood-ratio test, and the concordance, a row with `show_fit_stats = "concordance"`).
      Received by 1:
        show_columns = "r2": brmsfit
      ----
      error: spicy_invalid_input, spicy_error
        Token(s) "r2" in `show_columns` are not defined for `stanreg` models.
        i The column reports the classical R<U+00B2> of the least-squares variance partition. Outside it only pseudo-R<U+00B2> measures exist (McFadden 1974, Cox & Snell 1989, Nagelkerke 1991, Tjur 2009); they disagree with one another, so spicy does not print one under an R<U+00B2> header without naming it.
        i Name the one you want as a fit-statistics row, e.g. `show_fit_stats = c("nobs", "pseudo_r2_mcfadden")`.
      Received by 1:
        show_columns = "r2": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        `"beta"` is in `show_columns` but `standardized = "none"`.
        i Set `standardized` to one of: "refit", "posthoc", "basic", "smart", "pseudo" ("pseudo" is `glm` only).
        i Or remove `"beta"` from `show_columns`.
      Received by 2:
        show_columns = "beta": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        `nested = TRUE` is not available for `stanreg`.
        i The change statistics of a hierarchy (<U+0394>R<U+00B2>, partial F, LRT, <U+0394>AIC) all need a comparison of nested fits that this class does not provide here, and a hierarchy compares adjacent PAIRS -- one model that cannot be compared removes the comparison, it does not blank one column.
        i Render the models side by side with `nested = FALSE`.
      Received by 1:
        nested = TRUE: stanreg
      ----
      error: spicy_invalid_input, spicy_error
        `p_adjust` is not available for Bayesian fits: there are no p-values to adjust.
        i Bayesian tables report posterior medians and credible intervals; the probability-of-direction column (`show_columns = "pd"`) is the closest posterior summary.
      Received by 2:
        p_adjust = "holm": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (brmsfit).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": brmsfit
      ----
      error: spicy_invalid_input, spicy_error
        `show_columns = "n_events"` is not available for this model class (stanreg).
        i Outcome event counts need a binary (binomial family) outcome with an ungrouped 0/1, logical, or two-level factor response -- or a coxph fit on right-censored single-record data.
        i The model-total event count is also available as a fit statistic for survival models: `show_fit_stats = c("nobs", "n_events")`.
      Received by 1:
        show_columns = "n_events": stanreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "pseudo"` is not available for Bayesian fits.
        i "pseudo" needs the latent variance Var(eta-hat), which varies per posterior draw; its draws-native version is planned.
        i Use "posthoc", "basic" or "smart" (exact affine rescales of the posterior draws), or standardize predictors before fitting (Gelman, Hill & Vehtari 2020, ch. 12).
      Received by 2:
        standardized = "pseudo" (with beta): brmsfit, stanreg
      ----
      error: spicy_unsupported_standardized, spicy_error
        `standardized = "refit"` is not available for Bayesian fits.
        i "refit" would re-run the MCMC sampler on z-scored data inside a table call (minutes per model).
        i Use "posthoc", "basic" or "smart" (exact affine rescales of the posterior draws), or standardize predictors before fitting (Gelman, Hill & Vehtari 2020, ch. 12).
      Received by 2:
        standardized = "refit" (with beta): brmsfit, stanreg
      ----
      warning: spicy_ignored_arg, spicy_warning
        `exponentiate = TRUE` has no effect on identity-link fits.
        i exp() is only applied to fits whose coefficients are on a log / logit scale (odds, rate, risk, hazard, or time ratios). Drop the argument for identity-link (gaussian) fits.
      Received by 2:
        exponentiate = TRUE: brmsfit, stanreg
      ----
      warning: spicy_ignored_arg, spicy_warning
        `re_ci = "profile"` has no effect: no model in the table has random effects.
        i The `re_*` arguments act on the random-effects rows of mixed models. Drop it for this table.
      Received by 2:
        re_ci = "profile": brmsfit, stanreg
      ----
      warning: spicy_ignored_arg, spicy_warning
        `re_test = "lrt"` has no effect: no model in the table has random effects.
        i The `re_*` arguments act on the random-effects rows of mixed models. Drop it for this table.
      Received by 2:
        re_test = "lrt": brmsfit, stanreg
      ----
      warning: spicy_ignored_arg, spicy_warning
        `re_test = "rlrt"` has no effect: no model in the table has random effects.
        i The `re_*` arguments act on the random-effects rows of mixed models. Drop it for this table.
      Received by 2:
        re_test = "rlrt": brmsfit, stanreg

