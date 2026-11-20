# the refusals of the show_fit_stats sweep keep their text and class

    Code
      cat(.refusal_render(rows), sep = "\n")
    Output
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 30:
        "adj_r2": betareg, clm, clm_nominal, clm_scale, coxph, coxph_strata, cph,
            cph_xy, fepois, fepois_nofe, flexsurvreg, gls, hurdle, hurdle_negbin,
            mlogit, multinom, nls, ols, ols_xy, polr, rlm, rq, selection,
            survreg, survreg_strata, svycoxph, svyolr, tobit, zeroinfl,
            zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: no likelihood, so no (pseudo-)R<U+00B2> and no likelihood-based information criteria.
        i Use `"qic"` / `"qicu"` (Pan 2001) instead.
      Received by 1:
        "adj_r2": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "adj_r2": rms_Glm_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "adj_r2": rms_Glm, rms_Glm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "adj_r2": glm_logit, glm_probit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "adj_r2": glm_poisson
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"f_change"` + `"p_change"` (F test of the deviance change).
      Received by 3:
        "adj_r2": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974) or `"pseudo_r2_nagelkerke"` (Nagelkerke 1991).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "adj_r2": lrm_ordinal
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "adj_r2": lrm, lrm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for `negbin` models.
        i Use the R<U+00B2> of `negbin` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "adj_r2": glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for `svyglm` models.
      Received by 3:
        "adj_r2": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
      Received by 4:
        "adj_r2": clmm, glmmTMB_disp, glmmTMB_ordinal, glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` (Nakagawa et al. 2017).
      Received by 1:
        "adj_r2": glmmTMB_hurdle
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` or `"r2_conditional"` (Nakagawa et al. 2017).
      Received by 7:
        "adj_r2": glmer, glmmTMB, glmmTMB_reml, lme, lme_contrasts, lmer, lmer_ml
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 11:
        "adj_r2_change": geeglm, iv_robust, ivreg, lm, lm_robust, lm_robust_fe,
            lm_weighted, nls, ols, ols_xy, svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "adj_r2_change": rms_Glm_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "adj_r2_change": rms_Glm, rms_Glm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "adj_r2_change": glm_logit, glm_probit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "adj_r2_change": glm_poisson
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"f_change"` + `"p_change"` (F test of the deviance change).
      Received by 3:
        "adj_r2_change": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974) or `"pseudo_r2_nagelkerke"` (Nagelkerke 1991).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "adj_r2_change": lrm_ordinal
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "adj_r2_change": lrm, lrm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`, `"r2_deviance"`.
      Received by 1:
        "adj_r2_change": bam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_tjur"`, `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`.
      Received by 1:
        "adj_r2_change": gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"r2_deviance"`.
      Received by 1:
        "adj_r2_change": gam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `negbin` models.
        i Use the R<U+00B2> of `negbin` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "adj_r2_change": glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `rlm` models.
        i M-estimation minimizes a bounded loss rather than a likelihood or a residual sum of squares, so an rlm hierarchy has neither a likelihood-ratio test nor a partial F to report.
        i Drop the token, or compare the least-squares fits (`lm()`) if the hierarchy itself is the question.
      Received by 1:
        "adj_r2_change": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `rq` models.
        i Quantile regression partitions no sums of squares, and `logLik.rq()` is a pseudo-likelihood on the check-loss objective rather than one a ratio test can be built from.
        i Use `"f_change"` + `"p_change"` -- the Wald-type test `quantreg::anova.rq()` reports, and what `nested = TRUE` selects for a quantile hierarchy by default.
      Received by 1:
        "adj_r2_change": rq
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `svyglm` models.
      Received by 3:
        "adj_r2_change": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aic"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 5:
        "aic": glm_quasi, iv_robust, ivreg, lm_robust, lm_robust_fe
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aic"` in `show_fit_stats` is not defined for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: no likelihood, so no (pseudo-)R<U+00B2> and no likelihood-based information criteria.
        i Use `"qic"` / `"qicu"` (Pan 2001) instead.
      Received by 1:
        "aic": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aic"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood.
      Received by 2:
        "aic": svycoxph, svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aic_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 70:
        "aic_change": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aic_change"` in `show_fit_stats` is not defined for `rlm` models.
        i M-estimation minimizes a bounded loss rather than a likelihood or a residual sum of squares, so an rlm hierarchy has neither a likelihood-ratio test nor a partial F to report.
        i Drop the token, or compare the least-squares fits (`lm()`) if the hierarchy itself is the question.
      Received by 1:
        "aic_change": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aicc"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 5:
        "aicc": glm_quasi, iv_robust, ivreg, lm_robust, lm_robust_fe
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aicc"` in `show_fit_stats` is not defined for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: no likelihood, so no (pseudo-)R<U+00B2> and no likelihood-based information criteria.
        i Use `"qic"` / `"qicu"` (Pan 2001) instead.
      Received by 1:
        "aicc": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aicc"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood.
      Received by 2:
        "aicc": svycoxph, svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aicc"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood. For `svyglm()`, `"aic"` gives the design-based AIC of survey's `extractAIC()`.
      Received by 3:
        "aicc": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aicc_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 70:
        "aicc_change": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aicc_change"` in `show_fit_stats` is not defined for `rlm` models.
        i M-estimation minimizes a bounded loss rather than a likelihood or a residual sum of squares, so an rlm hierarchy has neither a likelihood-ratio test nor a partial F to report.
        i Drop the token, or compare the least-squares fits (`lm()`) if the hierarchy itself is the question.
      Received by 1:
        "aicc_change": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"alpha"` in `show_fit_stats` is defined only for NB2 negative-binomial fits (`MASS::glm.nb()`, `lme4::glmer.nb()`, `fixest::fenegbin()`, mgcv `nb()`, pscl `zeroinfl()` or `hurdle()` with `dist = "negbin"`, glmmTMB `nbinom2`), and no model in the table is one.
        i theta is the NB2 dispersion (V = mu + mu<U+00B2>/theta) and alpha its reciprocal. An NB1 fit (glmmTMB `nbinom1`) reports `"delta"` instead; other families have no such parameter.
        i A theta fixed in the call (`negative.binomial(theta)` in `glm()`, mgcv `negbin(theta)` or `nb(theta)`) is not an estimate and has no row.
      Received by 68:
        "alpha": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, iv_robust, ivreg, lm, lm_robust,
            lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer, lmer_ml, lrm,
            lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols, ols_xy, polr, rlm,
            rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection, survreg,
            survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl
      ----
      error: spicy_invalid_input, spicy_error
        Token `"bic"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 6:
        "bic": glm_quasi, iv_robust, ivreg, lm_robust, lm_robust_fe, rq
      ----
      error: spicy_invalid_input, spicy_error
        Token `"bic"` in `show_fit_stats` is not defined for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: no likelihood, so no (pseudo-)R<U+00B2> and no likelihood-based information criteria.
        i Use `"qic"` / `"qicu"` (Pan 2001) instead.
      Received by 1:
        "bic": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"bic"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood.
      Received by 2:
        "bic": svycoxph, svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"bic"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood. For `svyglm()`, `"aic"` gives the design-based AIC of survey's `extractAIC()`.
      Received by 3:
        "bic": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"bic_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 70:
        "bic_change": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"bic_change"` in `show_fit_stats` is not defined for `rlm` models.
        i M-estimation minimizes a bounded loss rather than a likelihood or a residual sum of squares, so an rlm hierarchy has neither a likelihood-ratio test nor a partial F to report.
        i Drop the token, or compare the least-squares fits (`lm()`) if the hierarchy itself is the question.
      Received by 1:
        "bic_change": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"concordance"` in `show_fit_stats` is defined only for `survival::coxph()` and `survey::svycoxph()` fits, and no model in the table is one.
        i It is Harrell's C of the fit, the value `summary()` of a `coxph` model reports.
      Received by 68:
        "concordance": bam, betareg, clm, clm_nominal, clm_scale, clmm, cph,
            cph_xy, feols, feols_iv, feols_nofe, fepois, fepois_nofe,
            flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma, glm_gaussian,
            glm_logit, glm_poisson, glm_probit, glm_quasi, glmer, glmmTMB,
            glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"delta"` in `show_fit_stats` is defined only for NB1 negative-binomial fits (glmmTMB `nbinom1`, `truncated_nbinom1`), and no model in the table is one.
        i delta is the NB1 dispersion (V = mu(1 + delta), Stata's `nbreg, dispersion(constant)`). An NB2 fit reports `"theta"` and `"alpha"` instead; other families have no such parameter.
      Received by 71:
        "delta": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"deviance"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 29:
        "deviance": betareg, clm, clm_nominal, clm_scale, coxph, coxph_strata,
            cph, cph_xy, flexsurvreg, glmmTMB_hurdle, glmmTMB_ordinal, gls,
            hurdle, hurdle_negbin, iv_robust, ivreg, lm_robust, lm_robust_fe,
            lme, lme_contrasts, mlogit, rlm, rq, selection, survreg,
            survreg_strata, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"deviance"` in `show_fit_stats` is not defined for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: no likelihood, so no (pseudo-)R<U+00B2> and no likelihood-based information criteria.
        i Use `"qic"` / `"qicu"` (Pan 2001) instead.
      Received by 1:
        "deviance": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"deviance"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood.
      Received by 2:
        "deviance": svycoxph, svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"deviance"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood. For `svyglm()`, `"aic"` gives the design-based AIC of survey's `extractAIC()`.
      Received by 3:
        "deviance": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"deviance_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 70:
        "deviance_change": bam, betareg, clm, clm_nominal, clm_scale, clmm,
            coxph, coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe,
            fepois, fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb,
            glm_gamma, glm_gaussian, glm_logit, glm_poisson, glm_probit,
            glm_quasi, glmer, glmmTMB, glmmTMB_disp, glmmTMB_hurdle,
            glmmTMB_ordinal, glmmTMB_reml, glmmTMB_zi, gls, hurdle,
            hurdle_negbin, iv_robust, ivreg, lm, lm_robust, lm_robust_fe,
            lm_weighted, lme, lme_contrasts, lmer, lmer_ml, lrm, lrm_ordinal,
            lrm_xy, mlogit, multinom, nls, ols, ols_xy, polr, rms_Glm,
            rms_Glm_logit, rms_Glm_xy, rq, selection, survreg, survreg_strata,
            svycoxph, svyglm, svyglm_binomial, svyglm_quasibinomial, svyolr,
            tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"deviance_change"` in `show_fit_stats` is not defined for `rlm` models.
        i M-estimation minimizes a bounded loss rather than a likelihood or a residual sum of squares, so an rlm hierarchy has neither a likelihood-ratio test nor a partial F to report.
        i Drop the token, or compare the least-squares fits (`lm()`) if the hierarchy itself is the question.
      Received by 1:
        "deviance_change": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"eff_p"` in `show_fit_stats` has no value for any model in the table.
        i `"eff_p"` is the effective number of parameters of a `svyglm()` design.
      Received by 68:
        "eff_p": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyolr, tobit, zeroinfl,
            zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"elpd_loo"` in `show_fit_stats` is defined only for Bayesian fits.
        i They are computed from the posterior draws (Bayesian R<U+00B2>; PSIS-LOO).
      Received by 71:
        "elpd_loo": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 37:
        "f2": betareg, clm, clm_nominal, clm_scale, coxph, coxph_strata, cph,
            cph_xy, feols, feols_iv, feols_nofe, fepois, fepois_nofe,
            flexsurvreg, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm_robust,
            lm_robust_fe, mlogit, multinom, nls, ols, ols_xy, polr, rlm, rq,
            selection, survreg, survreg_strata, svycoxph, svyolr, tobit,
            zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: no likelihood, so no (pseudo-)R<U+00B2> and no likelihood-based information criteria.
        i Use `"qic"` / `"qicu"` (Pan 2001) instead.
      Received by 1:
        "f2": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "f2": rms_Glm_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "f2": rms_Glm, rms_Glm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "f2": glm_logit, glm_probit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "f2": glm_poisson
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"f_change"` + `"p_change"` (F test of the deviance change).
      Received by 3:
        "f2": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974) or `"pseudo_r2_nagelkerke"` (Nagelkerke 1991).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "f2": lrm_ordinal
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "f2": lrm, lrm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`, `"r2_deviance"`.
      Received by 1:
        "f2": bam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_tjur"`, `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`.
      Received by 1:
        "f2": gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"r2_deviance"`.
      Received by 1:
        "f2": gam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for `negbin` models.
        i Use the R<U+00B2> of `negbin` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "f2": glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for `svyglm` models.
      Received by 3:
        "f2": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
      Received by 4:
        "f2": clmm, glmmTMB_disp, glmmTMB_ordinal, glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` (Nakagawa et al. 2017).
      Received by 1:
        "f2": glmmTMB_hurdle
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` or `"r2_conditional"` (Nakagawa et al. 2017).
      Received by 7:
        "f2": glmer, glmmTMB, glmmTMB_reml, lme, lme_contrasts, lmer, lmer_ml
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 11:
        "f2_change": geeglm, iv_robust, ivreg, lm, lm_robust, lm_robust_fe,
            lm_weighted, nls, ols, ols_xy, svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "f2_change": rms_Glm_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "f2_change": rms_Glm, rms_Glm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "f2_change": glm_logit, glm_probit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "f2_change": glm_poisson
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"f_change"` + `"p_change"` (F test of the deviance change).
      Received by 3:
        "f2_change": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974) or `"pseudo_r2_nagelkerke"` (Nagelkerke 1991).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "f2_change": lrm_ordinal
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "f2_change": lrm, lrm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`, `"r2_deviance"`.
      Received by 1:
        "f2_change": bam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_tjur"`, `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`.
      Received by 1:
        "f2_change": gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"r2_deviance"`.
      Received by 1:
        "f2_change": gam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `negbin` models.
        i Use the R<U+00B2> of `negbin` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "f2_change": glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `rlm` models.
        i M-estimation minimizes a bounded loss rather than a likelihood or a residual sum of squares, so an rlm hierarchy has neither a likelihood-ratio test nor a partial F to report.
        i Drop the token, or compare the least-squares fits (`lm()`) if the hierarchy itself is the question.
      Received by 1:
        "f2_change": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `rq` models.
        i Quantile regression partitions no sums of squares, and `logLik.rq()` is a pseudo-likelihood on the check-loss objective rather than one a ratio test can be built from.
        i Use `"f_change"` + `"p_change"` -- the Wald-type test `quantreg::anova.rq()` reports, and what `nested = TRUE` selects for a quantile hierarchy by default.
      Received by 1:
        "f2_change": rq
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `svyglm` models.
      Received by 3:
        "f2_change": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 15:
        "f_change": geeglm, glm_gamma, glm_gaussian, glm_quasi, iv_robust, ivreg,
            lm, lm_robust, lm_robust_fe, lm_weighted, nls, ols, ols_xy, rq,
            svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "f_change": rms_Glm_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "f_change": rms_Glm, rms_Glm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "f_change": glm_logit, glm_probit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "f_change": glm_poisson
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974) or `"pseudo_r2_nagelkerke"` (Nagelkerke 1991).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "f_change": lrm_ordinal
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "f_change": lrm, lrm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`, `"r2_deviance"`.
      Received by 1:
        "f_change": bam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_tjur"`, `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`.
      Received by 1:
        "f_change": gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"r2_deviance"`.
      Received by 1:
        "f_change": gam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` is not defined for `negbin` models.
        i Use the R<U+00B2> of `negbin` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "f_change": glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` is not defined for `rlm` models.
        i M-estimation minimizes a bounded loss rather than a likelihood or a residual sum of squares, so an rlm hierarchy has neither a likelihood-ratio test nor a partial F to report.
        i Drop the token, or compare the least-squares fits (`lm()`) if the hierarchy itself is the question.
      Received by 1:
        "f_change": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` is not defined for `svyglm` models.
      Received by 3:
        "f_change": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"fixed_effects"` in `show_fit_stats` has no value for any model in the table.
        i `"fixed_effects"` lists the fixed effects a model absorbs, and no model in the table absorbs any.
      Received by 3:
        "fixed_effects": feols_iv, feols_nofe, fepois_nofe
      ----
      error: spicy_invalid_input, spicy_error
        Token `"fixed_effects"` in `show_fit_stats` is defined only for fits that absorb fixed effects (fixest, or estimatr with `fixed_effects =`).
        i `"fixed_effects"` discloses the absorbed fixed effects (one Yes/No row per factor) and `"within_r2"` is the FE-partialed R<U+00B2>; other classes have neither.
      Received by 65:
        "fixed_effects": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, flexsurvreg, gam, gam_logit, geeglm,
            glm.nb, glm_gamma, glm_gaussian, glm_logit, glm_poisson, glm_probit,
            glm_quasi, glmer, glmmTMB, glmmTMB_disp, glmmTMB_hurdle,
            glmmTMB_ordinal, glmmTMB_reml, glmmTMB_zi, gls, hurdle,
            hurdle_negbin, iv_robust, ivreg, lm, lm_robust, lm_weighted, lme,
            lme_contrasts, lmer, lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit,
            multinom, nls, ols, ols_xy, polr, rlm, rms_Glm, rms_Glm_logit,
            rms_Glm_xy, rq, selection, survreg, survreg_strata, svycoxph, svyglm,
            svyglm_binomial, svyglm_quasibinomial, svyolr, tobit, zeroinfl,
            zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"icc"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 4:
        "icc": glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"icc"` in `show_fit_stats` has no value for any model in the table.
        i `"icc"` is computed for the mixed models of lme4, glmmTMB and nlme.
      Received by 60:
        "icc": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, gls,
            hurdle, hurdle_negbin, iv_robust, ivreg, lm, lm_robust, lm_robust_fe,
            lm_weighted, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"log_lik"` in `show_fit_stats` is not defined for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: no likelihood, so no (pseudo-)R<U+00B2> and no likelihood-based information criteria.
        i Use `"qic"` / `"qicu"` (Pan 2001) instead.
      Received by 1:
        "log_lik": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"log_lik"` in `show_fit_stats` needs a likelihood, and no model in the table has one.
        i Quasi families, survey designs, GEE, two-stage least squares and posterior fits are estimated without a maximized likelihood.
      Received by 10:
        "log_lik": glm_quasi, iv_robust, ivreg, lm_robust, lm_robust_fe,
            svycoxph, svyglm, svyglm_binomial, svyglm_quasibinomial, svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"looic"` in `show_fit_stats` is defined only for Bayesian fits.
        i They are computed from the posterior draws (Bayesian R<U+00B2>; PSIS-LOO).
      Received by 71:
        "looic": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"lrt_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 66:
        "lrt_change": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_logit,
            glm_poisson, glm_probit, glmer, glmmTMB, glmmTMB_disp,
            glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml, glmmTMB_zi, gls,
            hurdle, hurdle_negbin, iv_robust, ivreg, lm, lm_robust, lm_robust_fe,
            lm_weighted, lme, lme_contrasts, lmer, lmer_ml, lrm, lrm_ordinal,
            lrm_xy, mlogit, multinom, nls, ols, ols_xy, polr, rms_Glm,
            rms_Glm_logit, rms_Glm_xy, selection, survreg, survreg_strata,
            svycoxph, svyglm, svyglm_binomial, svyglm_quasibinomial, svyolr,
            tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"lrt_change"` in `show_fit_stats` is not computed for models whose dispersion is estimated.
        i Their nested test is the F test `anova()` reports, on the deviance change over the estimated dispersion.
        i Use `"f_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 3:
        "lrt_change": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"lrt_change"` in `show_fit_stats` is not defined for `rlm` models.
        i M-estimation minimizes a bounded loss rather than a likelihood or a residual sum of squares, so an rlm hierarchy has neither a likelihood-ratio test nor a partial F to report.
        i Drop the token, or compare the least-squares fits (`lm()`) if the hierarchy itself is the question.
      Received by 1:
        "lrt_change": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"lrt_change"` in `show_fit_stats` is not defined for `rq` models.
        i Quantile regression partitions no sums of squares, and `logLik.rq()` is a pseudo-likelihood on the check-loss objective rather than one a ratio test can be built from.
        i Use `"f_change"` + `"p_change"` -- the Wald-type test `quantreg::anova.rq()` reports, and what `nested = TRUE` selects for a quantile hierarchy by default.
      Received by 1:
        "lrt_change": rq
      ----
      error: spicy_invalid_input, spicy_error
        Token `"max_cluster_size"` in `show_fit_stats` is defined only for GEE fits (`geepack::geeglm`).
        i `"qic"` / `"qicu"` are the quasi-likelihood information criteria (Pan 2001), `"scale"` the estimated scale (dispersion) parameter, and `"max_cluster_size"` the largest cluster in the estimation sample.
      Received by 70:
        "max_cluster_size": bam, betareg, clm, clm_nominal, clm_scale, clmm,
            coxph, coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe,
            fepois, fepois_nofe, flexsurvreg, gam, gam_logit, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"n_events"` in `show_fit_stats` has no value for any model in the table.
        i `"n_events"` counts the events of a Cox model.
      Received by 66:
        "n_events": bam, betareg, clm, clm_nominal, clm_scale, clmm, feols,
            feols_iv, feols_nofe, fepois, fepois_nofe, flexsurvreg, gam,
            gam_logit, geeglm, glm.nb, glm_gamma, glm_gaussian, glm_logit,
            glm_poisson, glm_probit, glm_quasi, glmer, glmmTMB, glmmTMB_disp,
            glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml, glmmTMB_zi, gls,
            hurdle, hurdle_negbin, iv_robust, ivreg, lm, lm_robust, lm_robust_fe,
            lm_weighted, lme, lme_contrasts, lmer, lmer_ml, lrm, lrm_ordinal,
            lrm_xy, mlogit, multinom, nls, ols, ols_xy, polr, rlm, rms_Glm,
            rms_Glm_logit, rms_Glm_xy, rq, selection, survreg, survreg_strata,
            svyglm, svyglm_binomial, svyglm_quasibinomial, svyolr, tobit,
            zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"n_groups"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 3:
        "n_groups": feols_iv, feols_nofe, fepois_nofe
      ----
      error: spicy_invalid_input, spicy_error
        Token `"n_groups"` in `show_fit_stats` has no value for any model in the table.
        i `"n_groups"` counts the groups of a mixed model or a GEE, or the fixed effects absorbed by a fixest model or by an estimatr model with `fixed_effects =`.
      Received by 52:
        "n_groups": bam, betareg, clm, clm_nominal, clm_scale, coxph,
            coxph_strata, cph, cph_xy, flexsurvreg, gam, gam_logit, glm.nb,
            glm_gamma, glm_gaussian, glm_logit, glm_poisson, glm_probit,
            glm_quasi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_weighted, lrm, lrm_ordinal, lrm_xy, mlogit, multinom,
            nls, ols, ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq,
            selection, survreg, survreg_strata, svycoxph, svyglm,
            svyglm_binomial, svyglm_quasibinomial, svyolr, tobit, zeroinfl,
            zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 37:
        "omega2": betareg, clm, clm_nominal, clm_scale, coxph, coxph_strata, cph,
            cph_xy, feols, feols_iv, feols_nofe, fepois, fepois_nofe,
            flexsurvreg, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm_robust,
            lm_robust_fe, mlogit, multinom, nls, ols, ols_xy, polr, rlm, rq,
            selection, survreg, survreg_strata, svycoxph, svyolr, tobit,
            zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: no likelihood, so no (pseudo-)R<U+00B2> and no likelihood-based information criteria.
        i Use `"qic"` / `"qicu"` (Pan 2001) instead.
      Received by 1:
        "omega2": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "omega2": rms_Glm_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "omega2": rms_Glm, rms_Glm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "omega2": glm_logit, glm_probit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "omega2": glm_poisson
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"f_change"` + `"p_change"` (F test of the deviance change).
      Received by 3:
        "omega2": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974) or `"pseudo_r2_nagelkerke"` (Nagelkerke 1991).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "omega2": lrm_ordinal
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "omega2": lrm, lrm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`, `"r2_deviance"`.
      Received by 1:
        "omega2": bam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_tjur"`, `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`.
      Received by 1:
        "omega2": gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"r2_deviance"`.
      Received by 1:
        "omega2": gam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for `negbin` models.
        i Use the R<U+00B2> of `negbin` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "omega2": glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for `svyglm` models.
      Received by 3:
        "omega2": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
      Received by 4:
        "omega2": clmm, glmmTMB_disp, glmmTMB_ordinal, glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` (Nakagawa et al. 2017).
      Received by 1:
        "omega2": glmmTMB_hurdle
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` or `"r2_conditional"` (Nakagawa et al. 2017).
      Received by 7:
        "omega2": glmer, glmmTMB, glmmTMB_reml, lme, lme_contrasts, lmer, lmer_ml
      ----
      error: spicy_invalid_input, spicy_error
        Token `"p_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 70:
        "p_change": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"p_change"` in `show_fit_stats` is not defined for `rlm` models.
        i M-estimation minimizes a bounded loss rather than a likelihood or a residual sum of squares, so an rlm hierarchy has neither a likelihood-ratio test nor a partial F to report.
        i Drop the token, or compare the least-squares fits (`lm()`) if the hierarchy itself is the question.
      Received by 1:
        "p_change": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"phi"` in `show_fit_stats` is defined only for fits with a beta precision (`betareg::betareg()`, mgcv `betar()`, glmmTMB `beta_family()`, `betabinomial()`, `ordbeta()`), and no model in the table is one.
        i In beta regression, Var(y) = mu(1-mu)/(1+phi): higher phi = less dispersion. For `betabinomial()` and `ordbeta()`, phi is the precision parameter of the beta part. Other families have no such parameter.
        i A phi fixed in the call (mgcv `betar(theta)` with theta > 0) is not an estimate and has no row.
      Received by 70:
        "phi": bam, clm, clm_nominal, clm_scale, clmm, coxph, coxph_strata, cph,
            cph_xy, feols, feols_iv, feols_nofe, fepois, fepois_nofe,
            flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma, glm_gaussian,
            glm_logit, glm_poisson, glm_probit, glm_quasi, glmer, glmmTMB,
            glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_ferrari"` in `show_fit_stats` is defined only for beta regressions fitted with `betareg::betareg()`.
        i It is the squared correlation between the linear predictor and the link-transformed response (Ferrari & Cribari-Neto 2004), which `summary()` of a betareg fit reports.
      Received by 70:
        "pseudo_r2_ferrari": bam, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_mcfadden"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 26:
        "pseudo_r2_mcfadden": betareg, coxph, coxph_strata, cph, cph_xy, feols,
            feols_iv, feols_nofe, flexsurvreg, glm_quasi, gls, hurdle,
            hurdle_negbin, iv_robust, ivreg, lm_robust, lm_robust_fe, mlogit,
            nls, rq, selection, survreg, survreg_strata, tobit, zeroinfl,
            zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_mcfadden"` in `show_fit_stats` is for `glm` models only.
      Received by 1:
        "pseudo_r2_mcfadden": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_mcfadden"` in `show_fit_stats` is for `glm` models only.
        i Use `"r2"` for these models.
      Received by 2:
        "pseudo_r2_mcfadden": ols, ols_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_mcfadden"` in `show_fit_stats` is for `glm` models only.
        i Use `"r2"`, `"adj_r2"`, `"omega2"` or `"f2"` for these models.
      Received by 2:
        "pseudo_r2_mcfadden": lm, lm_weighted
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_mcfadden"` in `show_fit_stats` is not defined for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: no likelihood, so no (pseudo-)R<U+00B2> and no likelihood-based information criteria.
        i Use `"qic"` / `"qicu"` (Pan 2001) instead.
      Received by 1:
        "pseudo_r2_mcfadden": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_mcfadden"` in `show_fit_stats` is not defined for a `glm` or GAM of a continuous family (gaussian, Gamma, inverse Gaussian, Tweedie).
        i They are built on the log-likelihood of a density, which changes with the unit of the outcome: the same model gives other values when y is rescaled.
        i Use `"r2_deviance"`, 1 - deviance / null deviance, which does not depend on the unit (for a gaussian `glm` with the identity link, it is the R<U+00B2> of `lm()`).
      Received by 2:
        "pseudo_r2_mcfadden": glm_gamma, glm_gaussian
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_mcfadden"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
      Received by 4:
        "pseudo_r2_mcfadden": clmm, glmmTMB_disp, glmmTMB_ordinal, glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_mcfadden"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` (Nakagawa et al. 2017).
      Received by 1:
        "pseudo_r2_mcfadden": glmmTMB_hurdle
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_mcfadden"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` or `"r2_conditional"` (Nakagawa et al. 2017).
      Received by 7:
        "pseudo_r2_mcfadden": glmer, glmmTMB, glmmTMB_reml, lme, lme_contrasts,
            lmer, lmer_ml
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_mcfadden"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood.
      Received by 2:
        "pseudo_r2_mcfadden": svycoxph, svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_mcfadden"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood. For `svyglm()`, `"aic"` gives the design-based AIC of survey's `extractAIC()`.
      Received by 3:
        "pseudo_r2_mcfadden": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_mcfadden"` in `show_fit_stats` is not defined for the families of these `mgcv` GAMs.
        i A GAM has the R<U+00B2> of the glm of its family. Tjur's needs a binomial family, McFadden's and Nagelkerke's a binomial, Poisson or negative-binomial one.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"r2_deviance"`.
      Received by 1:
        "pseudo_r2_mcfadden": gam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_nagelkerke"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 28:
        "pseudo_r2_nagelkerke": betareg, coxph, coxph_strata, cph, cph_xy, feols,
            feols_iv, feols_nofe, fepois, fepois_nofe, flexsurvreg, glm_quasi,
            gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm_robust,
            lm_robust_fe, mlogit, nls, rq, selection, survreg, survreg_strata,
            tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_nagelkerke"` in `show_fit_stats` is for `glm` models only.
      Received by 1:
        "pseudo_r2_nagelkerke": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_nagelkerke"` in `show_fit_stats` is for `glm` models only.
        i Use `"r2"` for these models.
      Received by 2:
        "pseudo_r2_nagelkerke": ols, ols_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_nagelkerke"` in `show_fit_stats` is for `glm` models only.
        i Use `"r2"`, `"adj_r2"`, `"omega2"` or `"f2"` for these models.
      Received by 2:
        "pseudo_r2_nagelkerke": lm, lm_weighted
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_nagelkerke"` in `show_fit_stats` is not defined for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: no likelihood, so no (pseudo-)R<U+00B2> and no likelihood-based information criteria.
        i Use `"qic"` / `"qicu"` (Pan 2001) instead.
      Received by 1:
        "pseudo_r2_nagelkerke": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_nagelkerke"` in `show_fit_stats` is not defined for a `glm` or GAM of a continuous family (gaussian, Gamma, inverse Gaussian, Tweedie).
        i They are built on the log-likelihood of a density, which changes with the unit of the outcome: the same model gives other values when y is rescaled.
        i Use `"r2_deviance"`, 1 - deviance / null deviance, which does not depend on the unit (for a gaussian `glm` with the identity link, it is the R<U+00B2> of `lm()`).
      Received by 2:
        "pseudo_r2_nagelkerke": glm_gamma, glm_gaussian
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_nagelkerke"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
      Received by 4:
        "pseudo_r2_nagelkerke": clmm, glmmTMB_disp, glmmTMB_ordinal, glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_nagelkerke"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` (Nakagawa et al. 2017).
      Received by 1:
        "pseudo_r2_nagelkerke": glmmTMB_hurdle
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_nagelkerke"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` or `"r2_conditional"` (Nakagawa et al. 2017).
      Received by 7:
        "pseudo_r2_nagelkerke": glmer, glmmTMB, glmmTMB_reml, lme, lme_contrasts,
            lmer, lmer_ml
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_nagelkerke"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood.
      Received by 2:
        "pseudo_r2_nagelkerke": svycoxph, svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_nagelkerke"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood. For `svyglm()`, `"aic"` gives the design-based AIC of survey's `extractAIC()`.
      Received by 3:
        "pseudo_r2_nagelkerke": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_nagelkerke"` in `show_fit_stats` is not defined for the families of these `mgcv` GAMs.
        i A GAM has the R<U+00B2> of the glm of its family. Tjur's needs a binomial family, McFadden's and Nagelkerke's a binomial, Poisson or negative-binomial one.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"r2_deviance"`.
      Received by 1:
        "pseudo_r2_nagelkerke": gam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is defined only for binomial models (binary outcomes), and no model in the table is a binomial-family `glm` or a binary `rms::lrm()`.
        i Tjur's R<U+00B2> (2009) is mean(fitted | y = 1) - mean(fitted | y = 0), which needs a binary outcome (the trials of a grouped binomial response count as such).
      Received by 25:
        "pseudo_r2_tjur": betareg, coxph, coxph_strata, cph, cph_xy, feols,
            feols_iv, feols_nofe, flexsurvreg, gls, hurdle, hurdle_negbin,
            iv_robust, ivreg, lm_robust, lm_robust_fe, mlogit, nls, rq,
            selection, survreg, survreg_strata, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is defined only for binomial models (binary outcomes), and no model in the table is a binomial-family `glm` or a binary `rms::lrm()`.
        i Tjur's R<U+00B2> (2009) is mean(fitted | y = 1) - mean(fitted | y = 0), which needs a binary outcome (the trials of a grouped binomial response count as such).
        i Use `"pseudo_r2_mcfadden"` for these models.
      Received by 8:
        "pseudo_r2_tjur": clm, clm_nominal, clm_scale, fepois, fepois_nofe,
            lrm_ordinal, multinom, polr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is defined only for binomial models (binary outcomes), and no model in the table is a binomial-family `glm` or a binary `rms::lrm()`.
        i Tjur's R<U+00B2> (2009) is mean(fitted | y = 1) - mean(fitted | y = 0), which needs a binary outcome (the trials of a grouped binomial response count as such).
        i Use `"pseudo_r2_mcfadden"` or `"r2_deviance"` for these models.
      Received by 4:
        "pseudo_r2_tjur": glm.nb, glm_poisson, rms_Glm, rms_Glm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is defined only for binomial models (binary outcomes), and no model in the table is a binomial-family `glm` or a binary `rms::lrm()`.
        i Tjur's R<U+00B2> (2009) is mean(fitted | y = 1) - mean(fitted | y = 0), which needs a binary outcome (the trials of a grouped binomial response count as such).
        i Use `"r2_deviance"` for these models.
      Received by 3:
        "pseudo_r2_tjur": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is for `glm` models only.
      Received by 1:
        "pseudo_r2_tjur": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is for `glm` models only.
        i Use `"r2"` for these models.
      Received by 2:
        "pseudo_r2_tjur": ols, ols_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is for `glm` models only.
        i Use `"r2"`, `"adj_r2"`, `"omega2"` or `"f2"` for these models.
      Received by 2:
        "pseudo_r2_tjur": lm, lm_weighted
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is not defined for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: no likelihood, so no (pseudo-)R<U+00B2> and no likelihood-based information criteria.
        i Use `"qic"` / `"qicu"` (Pan 2001) instead.
      Received by 1:
        "pseudo_r2_tjur": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
      Received by 4:
        "pseudo_r2_tjur": clmm, glmmTMB_disp, glmmTMB_ordinal, glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` (Nakagawa et al. 2017).
      Received by 1:
        "pseudo_r2_tjur": glmmTMB_hurdle
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` or `"r2_conditional"` (Nakagawa et al. 2017).
      Received by 7:
        "pseudo_r2_tjur": glmer, glmmTMB, glmmTMB_reml, lme, lme_contrasts, lmer,
            lmer_ml
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood.
      Received by 2:
        "pseudo_r2_tjur": svycoxph, svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood. For `svyglm()`, `"aic"` gives the design-based AIC of survey's `extractAIC()`.
      Received by 3:
        "pseudo_r2_tjur": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is not defined for the families of these `mgcv` GAMs.
        i A GAM has the R<U+00B2> of the glm of its family. Tjur's needs a binomial family, McFadden's and Nagelkerke's a binomial, Poisson or negative-binomial one.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`, `"r2_deviance"`.
      Received by 1:
        "pseudo_r2_tjur": bam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is not defined for the families of these `mgcv` GAMs.
        i A GAM has the R<U+00B2> of the glm of its family. Tjur's needs a binomial family, McFadden's and Nagelkerke's a binomial, Poisson or negative-binomial one.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"r2_deviance"`.
      Received by 1:
        "pseudo_r2_tjur": gam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"qic"` in `show_fit_stats` is defined only for GEE fits (`geepack::geeglm`).
        i `"qic"` / `"qicu"` are the quasi-likelihood information criteria (Pan 2001), `"scale"` the estimated scale (dispersion) parameter, and `"max_cluster_size"` the largest cluster in the estimation sample.
      Received by 70:
        "qic": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"qicu"` in `show_fit_stats` is defined only for GEE fits (`geepack::geeglm`).
        i `"qic"` / `"qicu"` are the quasi-likelihood information criteria (Pan 2001), `"scale"` the estimated scale (dispersion) parameter, and `"max_cluster_size"` the largest cluster in the estimation sample.
      Received by 70:
        "qicu": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 28:
        "r2": betareg, clm, clm_nominal, clm_scale, coxph, coxph_strata, cph,
            cph_xy, fepois, fepois_nofe, flexsurvreg, gls, hurdle, hurdle_negbin,
            mlogit, multinom, nls, polr, rlm, rq, selection, survreg,
            survreg_strata, svycoxph, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: no likelihood, so no (pseudo-)R<U+00B2> and no likelihood-based information criteria.
        i Use `"qic"` / `"qicu"` (Pan 2001) instead.
      Received by 1:
        "r2": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "r2": rms_Glm_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "r2": rms_Glm, rms_Glm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "r2": glm_logit, glm_probit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "r2": glm_poisson
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"f_change"` + `"p_change"` (F test of the deviance change).
      Received by 3:
        "r2": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974) or `"pseudo_r2_nagelkerke"` (Nagelkerke 1991).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "r2": lrm_ordinal
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "r2": lrm, lrm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`, `"r2_deviance"`.
      Received by 1:
        "r2": bam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_tjur"`, `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`.
      Received by 1:
        "r2": gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"r2_deviance"`.
      Received by 1:
        "r2": gam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for `negbin` models.
        i Use the R<U+00B2> of `negbin` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "r2": glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for `svyglm` models.
      Received by 3:
        "r2": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
      Received by 4:
        "r2": clmm, glmmTMB_disp, glmmTMB_ordinal, glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` (Nakagawa et al. 2017).
      Received by 1:
        "r2": glmmTMB_hurdle
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` or `"r2_conditional"` (Nakagawa et al. 2017).
      Received by 7:
        "r2": glmer, glmmTMB, glmmTMB_reml, lme, lme_contrasts, lmer, lmer_ml
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_bayes"` in `show_fit_stats` is defined only for Bayesian fits.
        i They are computed from the posterior draws (Bayesian R<U+00B2>; PSIS-LOO).
      Received by 71:
        "r2_bayes": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 11:
        "r2_change": geeglm, iv_robust, ivreg, lm, lm_robust, lm_robust_fe,
            lm_weighted, nls, ols, ols_xy, svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "r2_change": rms_Glm_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `Glm` models.
        i Use the R<U+00B2> of `Glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "r2_change": rms_Glm, rms_Glm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "r2_change": glm_logit, glm_probit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "r2_change": glm_poisson
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `glm` models.
        i Use the R<U+00B2> of `glm` fits instead: `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"f_change"` + `"p_change"` (F test of the deviance change).
      Received by 3:
        "r2_change": glm_gamma, glm_gaussian, glm_quasi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974) or `"pseudo_r2_nagelkerke"` (Nagelkerke 1991).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "r2_change": lrm_ordinal
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `lrm` models.
        i Use the R<U+00B2> of `lrm` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"pseudo_r2_tjur"` (Tjur 2009).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 2:
        "r2_change": lrm, lrm_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`, `"r2_deviance"`.
      Received by 1:
        "r2_change": bam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"pseudo_r2_tjur"`, `"pseudo_r2_mcfadden"`, `"pseudo_r2_nagelkerke"`.
      Received by 1:
        "r2_change": gam_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `mgcv` GAMs.
        i A GAM shows mgcv's adjusted R<U+00B2>, `"adj_r2"`, and the R<U+00B2> of the glm of its family: `"r2_deviance"`.
      Received by 1:
        "r2_change": gam
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `negbin` models.
        i Use the R<U+00B2> of `negbin` fits instead: `"pseudo_r2_mcfadden"` (McFadden 1974), `"pseudo_r2_nagelkerke"` (Nagelkerke 1991) or `"r2_deviance"` (1 - deviance / null deviance).
        i For a nested glm comparison, use `"lrt_change"` + `"p_change"` (likelihood-ratio chi-square).
      Received by 1:
        "r2_change": glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `rlm` models.
        i M-estimation minimizes a bounded loss rather than a likelihood or a residual sum of squares, so an rlm hierarchy has neither a likelihood-ratio test nor a partial F to report.
        i Drop the token, or compare the least-squares fits (`lm()`) if the hierarchy itself is the question.
      Received by 1:
        "r2_change": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `rq` models.
        i Quantile regression partitions no sums of squares, and `logLik.rq()` is a pseudo-likelihood on the check-loss objective rather than one a ratio test can be built from.
        i Use `"f_change"` + `"p_change"` -- the Wald-type test `quantreg::anova.rq()` reports, and what `nested = TRUE` selects for a quantile hierarchy by default.
      Received by 1:
        "r2_change": rq
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `svyglm` models.
      Received by 3:
        "r2_change": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_conditional"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 4:
        "r2_conditional": glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal,
            glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_conditional"` in `show_fit_stats` has no value for any model in the table.
        i `"r2_conditional"` is computed for the mixed models of lme4, glmmTMB and nlme.
      Received by 60:
        "r2_conditional": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, gls,
            hurdle, hurdle_negbin, iv_robust, ivreg, lm, lm_robust, lm_robust_fe,
            lm_weighted, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is defined only for `glm` and `mgcv` GAM families other than the binomial, and no model in the table is one.
        i For a binomial, ordinal or multinomial response, the deviance R<U+00B2> equals McFadden's R<U+00B2>: use `"pseudo_r2_mcfadden"`.
      Received by 12:
        "r2_deviance": clm, clm_nominal, clm_scale, gam_logit, glm_logit,
            glm_probit, lrm, lrm_ordinal, lrm_xy, multinom, polr, rms_Glm_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is defined only for `glm` and `mgcv` GAM families other than the binomial, and no model in the table is one.
        i It is 1 - deviance / null deviance of a `glm` or a GAM of a gaussian, Gamma, inverse Gaussian, Tweedie, Poisson, negative binomial or quasi family.
      Received by 27:
        "r2_deviance": betareg, coxph, coxph_strata, cph, cph_xy, feols,
            feols_iv, feols_nofe, fepois, fepois_nofe, flexsurvreg, gls, hurdle,
            hurdle_negbin, iv_robust, ivreg, lm_robust, lm_robust_fe, mlogit,
            nls, rq, selection, survreg, survreg_strata, tobit, zeroinfl,
            zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is for `glm` models only.
      Received by 1:
        "r2_deviance": rlm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is for `glm` models only.
        i Use `"r2"` for these models.
      Received by 2:
        "r2_deviance": ols, ols_xy
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is for `glm` models only.
        i Use `"r2"`, `"adj_r2"`, `"omega2"` or `"f2"` for these models.
      Received by 2:
        "r2_deviance": lm, lm_weighted
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is not defined for GEE fits (`geeglm`).
        i GEE is estimated by quasi-likelihood: no likelihood, so no (pseudo-)R<U+00B2> and no likelihood-based information criteria.
        i Use `"qic"` / `"qicu"` (Pan 2001) instead.
      Received by 1:
        "r2_deviance": geeglm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
      Received by 4:
        "r2_deviance": clmm, glmmTMB_disp, glmmTMB_ordinal, glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` (Nakagawa et al. 2017).
      Received by 1:
        "r2_deviance": glmmTMB_hurdle
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` or `"r2_conditional"` (Nakagawa et al. 2017).
      Received by 7:
        "r2_deviance": glmer, glmmTMB, glmmTMB_reml, lme, lme_contrasts, lmer,
            lmer_ml
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood.
      Received by 2:
        "r2_deviance": svycoxph, svyolr
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood. For `svyglm()`, `"aic"` gives the design-based AIC of survey's `extractAIC()`.
      Received by 3:
        "r2_deviance": svyglm, svyglm_binomial, svyglm_quasibinomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_marginal"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 3:
        "r2_marginal": glmmTMB_disp, glmmTMB_ordinal, glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_marginal"` in `show_fit_stats` has no value for any model in the table.
        i `"r2_marginal"` is computed for the mixed models of lme4, glmmTMB and nlme.
      Received by 60:
        "r2_marginal": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, gls,
            hurdle, hurdle_negbin, iv_robust, ivreg, lm, lm_robust, lm_robust_fe,
            lm_weighted, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"rmse"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 62:
        "rmse": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glmer, glmmTMB,
            glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm_robust,
            lm_robust_fe, lme, lme_contrasts, lmer, lmer_ml, lrm, lrm_ordinal,
            lrm_xy, mlogit, multinom, nls, ols, ols_xy, polr, rlm, rms_Glm,
            rms_Glm_logit, rms_Glm_xy, rq, selection, survreg, survreg_strata,
            svycoxph, svyglm, svyglm_binomial, svyglm_quasibinomial, svyolr,
            tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"scale"` in `show_fit_stats` is defined only for GEE fits (`geepack::geeglm`).
        i `"qic"` / `"qicu"` are the quasi-likelihood information criteria (Pan 2001), `"scale"` the estimated scale (dispersion) parameter, and `"max_cluster_size"` the largest cluster in the estimation sample.
      Received by 70:
        "scale": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"sigma"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 39:
        "sigma": betareg, clm, clm_nominal, clm_scale, clmm, coxph, coxph_strata,
            cph, cph_xy, flexsurvreg, geeglm, glm.nb, glmmTMB_disp,
            glmmTMB_ordinal, hurdle, hurdle_negbin, iv_robust, ivreg, lm_robust,
            lm_robust_fe, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, polr,
            rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection, survreg_strata,
            svycoxph, svyglm, svyglm_binomial, svyglm_quasibinomial, svyolr,
            zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"theta"` in `show_fit_stats` is defined only for NB2 negative-binomial fits (`MASS::glm.nb()`, `lme4::glmer.nb()`, `fixest::fenegbin()`, mgcv `nb()`, pscl `zeroinfl()` or `hurdle()` with `dist = "negbin"`, glmmTMB `nbinom2`), and no model in the table is one.
        i theta is the NB2 dispersion (V = mu + mu<U+00B2>/theta) and alpha its reciprocal. An NB1 fit (glmmTMB `nbinom1`) reports `"delta"` instead; other families have no such parameter.
        i A theta fixed in the call (`negative.binomial(theta)` in `glm()`, mgcv `negbin(theta)` or `nb(theta)`) is not an estimate and has no row.
      Received by 68:
        "theta": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, iv_robust, ivreg, lm, lm_robust,
            lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer, lmer_ml, lrm,
            lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols, ols_xy, polr, rlm,
            rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection, survreg,
            survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl
      ----
      error: spicy_invalid_input, spicy_error
        Token `"waic"` in `show_fit_stats` is defined only for Bayesian fits.
        i They are computed from the posterior draws (Bayesian R<U+00B2>; PSIS-LOO).
      Received by 71:
        "waic": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lm_weighted, lme, lme_contrasts, lmer,
            lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols,
            ols_xy, polr, rlm, rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection,
            survreg, survreg_strata, svycoxph, svyglm, svyglm_binomial,
            svyglm_quasibinomial, svyolr, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"weighted_nobs"` in `show_fit_stats` has no value for any model in the table.
        i `"weighted_nobs"` is the weighted count of a model fitted with weights or on a survey design.
      Received by 65:
        "weighted_nobs": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, feols, feols_iv, feols_nofe, fepois,
            fepois_nofe, flexsurvreg, gam, gam_logit, geeglm, glm.nb, glm_gamma,
            glm_gaussian, glm_logit, glm_poisson, glm_probit, glm_quasi, glmer,
            glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal, glmmTMB_reml,
            glmmTMB_zi, gls, hurdle, hurdle_negbin, iv_robust, ivreg, lm,
            lm_robust, lm_robust_fe, lme, lme_contrasts, lmer, lmer_ml, lrm,
            lrm_ordinal, lrm_xy, mlogit, multinom, nls, ols, ols_xy, polr, rlm,
            rms_Glm, rms_Glm_logit, rms_Glm_xy, rq, selection, survreg,
            survreg_strata, tobit, zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"within_r2"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 4:
        "within_r2": feols_iv, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_invalid_input, spicy_error
        Token `"within_r2"` in `show_fit_stats` is defined only for fits that absorb fixed effects (fixest, or estimatr with `fixed_effects =`).
        i `"fixed_effects"` discloses the absorbed fixed effects (one Yes/No row per factor) and `"within_r2"` is the FE-partialed R<U+00B2>; other classes have neither.
      Received by 65:
        "within_r2": bam, betareg, clm, clm_nominal, clm_scale, clmm, coxph,
            coxph_strata, cph, cph_xy, flexsurvreg, gam, gam_logit, geeglm,
            glm.nb, glm_gamma, glm_gaussian, glm_logit, glm_poisson, glm_probit,
            glm_quasi, glmer, glmmTMB, glmmTMB_disp, glmmTMB_hurdle,
            glmmTMB_ordinal, glmmTMB_reml, glmmTMB_zi, gls, hurdle,
            hurdle_negbin, iv_robust, ivreg, lm, lm_robust, lm_weighted, lme,
            lme_contrasts, lmer, lmer_ml, lrm, lrm_ordinal, lrm_xy, mlogit,
            multinom, nls, ols, ols_xy, polr, rlm, rms_Glm, rms_Glm_logit,
            rms_Glm_xy, rq, selection, survreg, survreg_strata, svycoxph, svyglm,
            svyglm_binomial, svyglm_quasibinomial, svyolr, tobit, zeroinfl,
            zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `betareg` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "adj_r2_change": betareg
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `betareg` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f2_change": betareg
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `betareg` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f_change": betareg
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `betareg` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "r2_change": betareg
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `clm` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 3:
        "adj_r2_change": clm, clm_nominal, clm_scale
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `clm` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 3:
        "f2_change": clm, clm_nominal, clm_scale
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `clm` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 3:
        "f_change": clm, clm_nominal, clm_scale
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `clm` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 3:
        "r2_change": clm, clm_nominal, clm_scale
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `clmm` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "adj_r2_change": clmm
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `clmm` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "f2_change": clmm
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `clmm` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "f_change": clmm
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `clmm` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "r2_change": clmm
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `coxph` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "adj_r2_change": coxph, coxph_strata
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `coxph` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "f2_change": coxph, coxph_strata
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `coxph` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "f_change": coxph, coxph_strata
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `coxph` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "r2_change": coxph, coxph_strata
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `cph` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "adj_r2_change": cph, cph_xy
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `cph` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "f2_change": cph, cph_xy
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `cph` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "f_change": cph, cph_xy
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `cph` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "r2_change": cph, cph_xy
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `fixest` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "adj_r2_change": feols_iv
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `fixest` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 4:
        "adj_r2_change": feols, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `fixest` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "f2_change": feols_iv
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `fixest` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 4:
        "f2_change": feols, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `fixest` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "f_change": feols_iv
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `fixest` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 4:
        "f_change": feols, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `fixest` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "r2_change": feols_iv
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `fixest` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 4:
        "r2_change": feols, feols_nofe, fepois, fepois_nofe
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `flexsurvreg` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "adj_r2_change": flexsurvreg
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `flexsurvreg` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f2_change": flexsurvreg
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `flexsurvreg` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f_change": flexsurvreg
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `flexsurvreg` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "r2_change": flexsurvreg
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `glmerMod` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "adj_r2_change": glmer
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `glmerMod` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f2_change": glmer
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `glmerMod` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f_change": glmer
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `glmerMod` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "r2_change": glmer
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `glmmTMB` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "adj_r2_change": glmmTMB_reml
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `glmmTMB` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 5:
        "adj_r2_change": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal,
            glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `glmmTMB` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "f2_change": glmmTMB_reml
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `glmmTMB` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 5:
        "f2_change": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal,
            glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `glmmTMB` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "f_change": glmmTMB_reml
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `glmmTMB` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 5:
        "f_change": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal,
            glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `glmmTMB` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "r2_change": glmmTMB_reml
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `glmmTMB` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 5:
        "r2_change": glmmTMB, glmmTMB_disp, glmmTMB_hurdle, glmmTMB_ordinal,
            glmmTMB_zi
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `gls` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "adj_r2_change": gls
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `gls` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "f2_change": gls
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `gls` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "f_change": gls
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `gls` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "r2_change": gls
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `hurdle` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "adj_r2_change": hurdle, hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `hurdle` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "f2_change": hurdle, hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `hurdle` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "f_change": hurdle, hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `hurdle` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "r2_change": hurdle, hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `lme` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 2:
        "adj_r2_change": lme, lme_contrasts
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `lme` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 2:
        "f2_change": lme, lme_contrasts
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `lme` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 2:
        "f_change": lme, lme_contrasts
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `lme` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 2:
        "r2_change": lme, lme_contrasts
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `lmerMod` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "adj_r2_change": lmer, lmer_ml
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `lmerMod` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "f2_change": lmer, lmer_ml
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `lmerMod` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "f_change": lmer, lmer_ml
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `lmerMod` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "r2_change": lmer, lmer_ml
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `mlogit` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "adj_r2_change": mlogit
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `mlogit` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f2_change": mlogit
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `mlogit` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f_change": mlogit
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `mlogit` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "r2_change": mlogit
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `multinom` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "adj_r2_change": multinom
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `multinom` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f2_change": multinom
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `multinom` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f_change": multinom
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `multinom` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "r2_change": multinom
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `polr` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "adj_r2_change": polr
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `polr` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f2_change": polr
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `polr` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f_change": polr
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `polr` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "r2_change": polr
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `selection` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "adj_r2_change": selection
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `selection` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f2_change": selection
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `selection` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f_change": selection
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `selection` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "r2_change": selection
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `survreg` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "adj_r2_change": survreg, survreg_strata
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `survreg` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "f2_change": survreg, survreg_strata
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `survreg` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "f_change": survreg, survreg_strata
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `survreg` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "r2_change": survreg, survreg_strata
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `svycoxph` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "adj_r2_change": svycoxph
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `svycoxph` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "f2_change": svycoxph
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `svycoxph` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "f_change": svycoxph
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `svycoxph` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
      Received by 1:
        "r2_change": svycoxph
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `tobit` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "adj_r2_change": tobit
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `tobit` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f2_change": tobit
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `tobit` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "f_change": tobit
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `tobit` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 1:
        "r2_change": tobit
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `zeroinfl` fits through the likelihood-ratio chi-square, so token `"adj_r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "adj_r2_change": zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `zeroinfl` fits through the likelihood-ratio chi-square, so token `"f2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "f2_change": zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `zeroinfl` fits through the likelihood-ratio chi-square, so token `"f_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "f_change": zeroinfl, zeroinfl_negbin
      ----
      error: spicy_invalid_input, spicy_error
        spicy compares hierarchies of `zeroinfl` fits through the likelihood-ratio chi-square, so token `"r2_change"` in `show_fit_stats` is not computed on this route.
        i The nested comparison for these fits is a likelihood-ratio chi-square, which reports no variance-explained change.
        i Use `"lrt_change"` + `"p_change"`, which is also what `nested = TRUE` selects for this hierarchy by default.
      Received by 2:
        "r2_change": zeroinfl, zeroinfl_negbin

# the refusals of the show_fit_stats sweep keep their text and class: Stan

    Code
      cat(.refusal_render(rows), sep = "\n")
    Output
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 1:
        "adj_r2": brmsfit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2"` in `show_fit_stats` is not defined for `stanreg` models.
      Received by 1:
        "adj_r2": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 1:
        "adj_r2_change": brmsfit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"adj_r2_change"` in `show_fit_stats` is not defined for `stanreg` models.
      Received by 1:
        "adj_r2_change": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aic"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 2:
        "aic": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aic_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 2:
        "aic_change": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aicc"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 2:
        "aicc": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"aicc_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 2:
        "aicc_change": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"alpha"` in `show_fit_stats` is defined only for NB2 negative-binomial fits (`MASS::glm.nb()`, `lme4::glmer.nb()`, `fixest::fenegbin()`, mgcv `nb()`, pscl `zeroinfl()` or `hurdle()` with `dist = "negbin"`, glmmTMB `nbinom2`), and no model in the table is one.
        i theta is the NB2 dispersion (V = mu + mu<U+00B2>/theta) and alpha its reciprocal. An NB1 fit (glmmTMB `nbinom1`) reports `"delta"` instead; other families have no such parameter.
        i A theta fixed in the call (`negative.binomial(theta)` in `glm()`, mgcv `negbin(theta)` or `nb(theta)`) is not an estimate and has no row.
      Received by 2:
        "alpha": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"bic"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 2:
        "bic": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"bic_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 2:
        "bic_change": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"concordance"` in `show_fit_stats` is defined only for `survival::coxph()` and `survey::svycoxph()` fits, and no model in the table is one.
        i It is Harrell's C of the fit, the value `summary()` of a `coxph` model reports.
      Received by 2:
        "concordance": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"delta"` in `show_fit_stats` is defined only for NB1 negative-binomial fits (glmmTMB `nbinom1`, `truncated_nbinom1`), and no model in the table is one.
        i delta is the NB1 dispersion (V = mu(1 + delta), Stata's `nbreg, dispersion(constant)`). An NB2 fit reports `"theta"` and `"alpha"` instead; other families have no such parameter.
      Received by 2:
        "delta": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"deviance"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 2:
        "deviance": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"deviance_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 2:
        "deviance_change": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"eff_p"` in `show_fit_stats` has no value for any model in the table.
        i `"eff_p"` is the effective number of parameters of a `svyglm()` design.
      Received by 2:
        "eff_p": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 1:
        "f2": brmsfit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2"` in `show_fit_stats` is not defined for `stanreg` models.
      Received by 1:
        "f2": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 1:
        "f2_change": brmsfit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f2_change"` in `show_fit_stats` is not defined for `stanreg` models.
      Received by 1:
        "f2_change": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 1:
        "f_change": brmsfit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"f_change"` in `show_fit_stats` is not defined for `stanreg` models.
      Received by 1:
        "f_change": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"fixed_effects"` in `show_fit_stats` is defined only for fits that absorb fixed effects (fixest, or estimatr with `fixed_effects =`).
        i `"fixed_effects"` discloses the absorbed fixed effects (one Yes/No row per factor) and `"within_r2"` is the FE-partialed R<U+00B2>; other classes have neither.
      Received by 2:
        "fixed_effects": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"icc"` in `show_fit_stats` has no value for any model in the table.
        i `"icc"` is computed for the mixed models of lme4, glmmTMB and nlme.
      Received by 2:
        "icc": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"log_lik"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 2:
        "log_lik": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"lrt_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 2:
        "lrt_change": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"max_cluster_size"` in `show_fit_stats` is defined only for GEE fits (`geepack::geeglm`).
        i `"qic"` / `"qicu"` are the quasi-likelihood information criteria (Pan 2001), `"scale"` the estimated scale (dispersion) parameter, and `"max_cluster_size"` the largest cluster in the estimation sample.
      Received by 2:
        "max_cluster_size": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"n_events"` in `show_fit_stats` has no value for any model in the table.
        i `"n_events"` counts the events of a Cox model.
      Received by 2:
        "n_events": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"n_groups"` in `show_fit_stats` has no value for any model in the table.
        i `"n_groups"` counts the groups of a mixed model or a GEE, or the fixed effects absorbed by a fixest model or by an estimatr model with `fixed_effects =`.
      Received by 2:
        "n_groups": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 1:
        "omega2": brmsfit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"omega2"` in `show_fit_stats` is not defined for `stanreg` models.
      Received by 1:
        "omega2": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"p_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 2:
        "p_change": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"phi"` in `show_fit_stats` is defined only for fits with a beta precision (`betareg::betareg()`, mgcv `betar()`, glmmTMB `beta_family()`, `betabinomial()`, `ordbeta()`), and no model in the table is one.
        i In beta regression, Var(y) = mu(1-mu)/(1+phi): higher phi = less dispersion. For `betabinomial()` and `ordbeta()`, phi is the precision parameter of the beta part. Other families have no such parameter.
        i A phi fixed in the call (mgcv `betar(theta)` with theta > 0) is not an estimate and has no row.
      Received by 2:
        "phi": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_ferrari"` in `show_fit_stats` is defined only for beta regressions fitted with `betareg::betareg()`.
        i It is the squared correlation between the linear predictor and the link-transformed response (Ferrari & Cribari-Neto 2004), which `summary()` of a betareg fit reports.
      Received by 2:
        "pseudo_r2_ferrari": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_mcfadden"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 2:
        "pseudo_r2_mcfadden": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_nagelkerke"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 2:
        "pseudo_r2_nagelkerke": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 2:
        "pseudo_r2_tjur": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"qic"` in `show_fit_stats` is defined only for GEE fits (`geepack::geeglm`).
        i `"qic"` / `"qicu"` are the quasi-likelihood information criteria (Pan 2001), `"scale"` the estimated scale (dispersion) parameter, and `"max_cluster_size"` the largest cluster in the estimation sample.
      Received by 2:
        "qic": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"qicu"` in `show_fit_stats` is defined only for GEE fits (`geepack::geeglm`).
        i `"qic"` / `"qicu"` are the quasi-likelihood information criteria (Pan 2001), `"scale"` the estimated scale (dispersion) parameter, and `"max_cluster_size"` the largest cluster in the estimation sample.
      Received by 2:
        "qicu": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 1:
        "r2": brmsfit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2"` in `show_fit_stats` is not defined for `stanreg` models.
      Received by 1:
        "r2": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` compares adjacent models: it needs `nested = TRUE` and at least two models.
        i Pass the models from the smallest to the largest, with `nested = TRUE`.
      Received by 1:
        "r2_change": brmsfit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_change"` in `show_fit_stats` is not defined for `stanreg` models.
      Received by 1:
        "r2_change": stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_conditional"` in `show_fit_stats` has no value for any model in the table.
        i `"r2_conditional"` is computed for the mixed models of lme4, glmmTMB and nlme.
      Received by 2:
        "r2_conditional": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 2:
        "r2_deviance": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_marginal"` in `show_fit_stats` has no value for any model in the table.
        i `"r2_marginal"` is computed for the mixed models of lme4, glmmTMB and nlme.
      Received by 2:
        "r2_marginal": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"rmse"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 2:
        "rmse": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"scale"` in `show_fit_stats` is defined only for GEE fits (`geepack::geeglm`).
        i `"qic"` / `"qicu"` are the quasi-likelihood information criteria (Pan 2001), `"scale"` the estimated scale (dispersion) parameter, and `"max_cluster_size"` the largest cluster in the estimation sample.
      Received by 2:
        "scale": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"sigma"` in `show_fit_stats` is not defined for Bayesian fits.
        i Posterior fits have no likelihood-based information criteria or classical R<U+00B2>; compare models with `loo::loo()` / `loo::loo_compare()` outside the table.
      Received by 2:
        "sigma": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"theta"` in `show_fit_stats` is defined only for NB2 negative-binomial fits (`MASS::glm.nb()`, `lme4::glmer.nb()`, `fixest::fenegbin()`, mgcv `nb()`, pscl `zeroinfl()` or `hurdle()` with `dist = "negbin"`, glmmTMB `nbinom2`), and no model in the table is one.
        i theta is the NB2 dispersion (V = mu + mu<U+00B2>/theta) and alpha its reciprocal. An NB1 fit (glmmTMB `nbinom1`) reports `"delta"` instead; other families have no such parameter.
        i A theta fixed in the call (`negative.binomial(theta)` in `glm()`, mgcv `negbin(theta)` or `nb(theta)`) is not an estimate and has no row.
      Received by 2:
        "theta": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"weighted_nobs"` in `show_fit_stats` has no value for any model in the table.
        i `"weighted_nobs"` is the weighted count of a model fitted with weights or on a survey design.
      Received by 2:
        "weighted_nobs": brmsfit, stanreg
      ----
      error: spicy_invalid_input, spicy_error
        Token `"within_r2"` in `show_fit_stats` is defined only for fits that absorb fixed effects (fixest, or estimatr with `fixed_effects =`).
        i `"fixed_effects"` discloses the absorbed fixed effects (one Yes/No row per factor) and `"within_r2"` is the FE-partialed R<U+00B2>; other classes have neither.
      Received by 2:
        "within_r2": brmsfit, stanreg

