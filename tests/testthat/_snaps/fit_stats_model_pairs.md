# the tokens that give a row for a pair keep their list

    Code
      cat(sprintf("%s: %s", fp_ids, vapply(fp_ids, function(id) paste(fp_rows[[id]],
      collapse = ", "), character(1))), sep = "\n")
    Output
      lm + glm_logit: pseudo_r2_tjur
      glm_poisson + glm.nb: theta, alpha, r2_deviance
      glmmTMB_nbinom1 + glmmTMB_nbinom2: theta, alpha, delta
      betareg + gam_betar: pseudo_r2_ferrari, phi
      coxph + cph: concordance
      svycoxph + svyglm_binomial: concordance
      lm + svyglm_binomial: 
      feols_nofe + lm_robust_fe: fixed_effects, within_r2
      fepois + lm: fixed_effects
      geeglm + glm_poisson: qic, qicu, scale, max_cluster_size, r2_deviance
      lrm_ordinal + rms_Glm_logit: pseudo_r2_tjur
      gam + glmmTMB_beta: phi, r2_deviance
      zeroinfl + hurdle_negbin: theta, alpha

# the refusals of the pairs keep their text and class

    Code
      cat(.refusal_render(rows), sep = "\n")
    Output
      ----
      error: spicy_invalid_input, spicy_error
        Token `"alpha"` in `show_fit_stats` is defined only for NB2 negative-binomial fits (`MASS::glm.nb()`, `lme4::glmer.nb()`, `fixest::fenegbin()`, mgcv `nb()`, pscl `zeroinfl()` or `hurdle()` with `dist = "negbin"`, glmmTMB `nbinom2`), and no model in the table is one.
        i theta is the NB2 dispersion (V = mu + mu<U+00B2>/theta) and alpha its reciprocal. An NB1 fit (glmmTMB `nbinom1`) reports `"delta"` instead; other families have no such parameter.
        i A theta fixed in the call (`negative.binomial(theta)` in `glm()`, mgcv `negbin(theta)` or `nb(theta)`) is not an estimate and has no row.
      Received by 10:
        "alpha": betareg + gam_betar, coxph + cph, feols_nofe + lm_robust_fe,
            fepois + lm, gam + glmmTMB_beta, geeglm + glm_poisson, lm +
            glm_logit, lm + svyglm_binomial, lrm_ordinal + rms_Glm_logit,
            svycoxph + svyglm_binomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"concordance"` in `show_fit_stats` is defined only for `survival::coxph()` and `survey::svycoxph()` fits, and no model in the table is one.
        i It is Harrell's C of the fit, the value `summary()` of a `coxph` model reports.
      Received by 11:
        "concordance": betareg + gam_betar, feols_nofe + lm_robust_fe, fepois +
            lm, gam + glmmTMB_beta, geeglm + glm_poisson, glm_poisson + glm.nb,
            glmmTMB_nbinom1 + glmmTMB_nbinom2, lm + glm_logit, lm +
            svyglm_binomial, lrm_ordinal + rms_Glm_logit, zeroinfl +
            hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"delta"` in `show_fit_stats` is defined only for NB1 negative-binomial fits (glmmTMB `nbinom1`, `truncated_nbinom1`), and no model in the table is one.
        i delta is the NB1 dispersion (V = mu(1 + delta), Stata's `nbreg, dispersion(constant)`). An NB2 fit reports `"theta"` and `"alpha"` instead; other families have no such parameter.
      Received by 12:
        "delta": betareg + gam_betar, coxph + cph, feols_nofe + lm_robust_fe,
            fepois + lm, gam + glmmTMB_beta, geeglm + glm_poisson, glm_poisson +
            glm.nb, lm + glm_logit, lm + svyglm_binomial, lrm_ordinal +
            rms_Glm_logit, svycoxph + svyglm_binomial, zeroinfl + hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"fixed_effects"` in `show_fit_stats` is defined only for fits that absorb fixed effects (fixest, or estimatr with `fixed_effects =`).
        i `"fixed_effects"` discloses the absorbed fixed effects (one Yes/No row per factor) and `"within_r2"` is the FE-partialed R<U+00B2>; other classes have neither.
      Received by 11:
        "fixed_effects": betareg + gam_betar, coxph + cph, gam + glmmTMB_beta,
            geeglm + glm_poisson, glm_poisson + glm.nb, glmmTMB_nbinom1 +
            glmmTMB_nbinom2, lm + glm_logit, lm + svyglm_binomial, lrm_ordinal +
            rms_Glm_logit, svycoxph + svyglm_binomial, zeroinfl + hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"max_cluster_size"` in `show_fit_stats` is defined only for GEE fits (`geepack::geeglm`).
        i `"qic"` / `"qicu"` are the quasi-likelihood information criteria (Pan 2001), `"scale"` the estimated scale (dispersion) parameter, and `"max_cluster_size"` the largest cluster in the estimation sample.
      Received by 12:
        "max_cluster_size": betareg + gam_betar, coxph + cph, feols_nofe +
            lm_robust_fe, fepois + lm, gam + glmmTMB_beta, glm_poisson + glm.nb,
            glmmTMB_nbinom1 + glmmTMB_nbinom2, lm + glm_logit, lm +
            svyglm_binomial, lrm_ordinal + rms_Glm_logit, svycoxph +
            svyglm_binomial, zeroinfl + hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"phi"` in `show_fit_stats` is defined only for fits with a beta precision (`betareg::betareg()`, mgcv `betar()`, glmmTMB `beta_family()`, `betabinomial()`, `ordbeta()`), and no model in the table is one.
        i In beta regression, Var(y) = mu(1-mu)/(1+phi): higher phi = less dispersion. For `betabinomial()` and `ordbeta()`, phi is the precision parameter of the beta part. Other families have no such parameter.
        i A phi fixed in the call (mgcv `betar(theta)` with theta > 0) is not an estimate and has no row.
      Received by 11:
        "phi": coxph + cph, feols_nofe + lm_robust_fe, fepois + lm, geeglm +
            glm_poisson, glm_poisson + glm.nb, glmmTMB_nbinom1 + glmmTMB_nbinom2,
            lm + glm_logit, lm + svyglm_binomial, lrm_ordinal + rms_Glm_logit,
            svycoxph + svyglm_binomial, zeroinfl + hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_ferrari"` in `show_fit_stats` is defined only for beta regressions fitted with `betareg::betareg()`.
        i It is the squared correlation between the linear predictor and the link-transformed response (Ferrari & Cribari-Neto 2004), which `summary()` of a betareg fit reports.
      Received by 12:
        "pseudo_r2_ferrari": coxph + cph, feols_nofe + lm_robust_fe, fepois + lm,
            gam + glmmTMB_beta, geeglm + glm_poisson, glm_poisson + glm.nb,
            glmmTMB_nbinom1 + glmmTMB_nbinom2, lm + glm_logit, lm +
            svyglm_binomial, lrm_ordinal + rms_Glm_logit, svycoxph +
            svyglm_binomial, zeroinfl + hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 1:
        "pseudo_r2_tjur": lm + svyglm_binomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is defined only for binomial models (binary outcomes), and no model in the table is a binomial-family `glm` or a binary `rms::lrm()`.
        i Tjur's R<U+00B2> (2009) is mean(fitted | y = 1) - mean(fitted | y = 0), which needs a binary outcome (the trials of a grouped binomial response count as such).
      Received by 4:
        "pseudo_r2_tjur": betareg + gam_betar, coxph + cph, feols_nofe +
            lm_robust_fe, zeroinfl + hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is defined only for binomial models (binary outcomes), and no model in the table is a binomial-family `glm` or a binary `rms::lrm()`.
        i Tjur's R<U+00B2> (2009) is mean(fitted | y = 1) - mean(fitted | y = 0), which needs a binary outcome (the trials of a grouped binomial response count as such).
        i Use `"pseudo_r2_mcfadden"` for these models.
      Received by 1:
        "pseudo_r2_tjur": fepois + lm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is defined only for binomial models (binary outcomes), and no model in the table is a binomial-family `glm` or a binary `rms::lrm()`.
        i Tjur's R<U+00B2> (2009) is mean(fitted | y = 1) - mean(fitted | y = 0), which needs a binary outcome (the trials of a grouped binomial response count as such).
        i Use `"pseudo_r2_mcfadden"` or `"r2_deviance"` for these models.
      Received by 2:
        "pseudo_r2_tjur": geeglm + glm_poisson, glm_poisson + glm.nb
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is defined only for binomial models (binary outcomes), and no model in the table is a binomial-family `glm` or a binary `rms::lrm()`.
        i Tjur's R<U+00B2> (2009) is mean(fitted | y = 1) - mean(fitted | y = 0), which needs a binary outcome (the trials of a grouped binomial response count as such).
        i Use `"r2_deviance"` for these models.
      Received by 1:
        "pseudo_r2_tjur": gam + glmmTMB_beta
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` or `"r2_conditional"` (Nakagawa et al. 2017).
      Received by 1:
        "pseudo_r2_tjur": glmmTMB_nbinom1 + glmmTMB_nbinom2
      ----
      error: spicy_invalid_input, spicy_error
        Token `"pseudo_r2_tjur"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood. For `svyglm()`, `"aic"` gives the design-based AIC of survey's `extractAIC()`.
      Received by 1:
        "pseudo_r2_tjur": svycoxph + svyglm_binomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"qic"` in `show_fit_stats` is defined only for GEE fits (`geepack::geeglm`).
        i `"qic"` / `"qicu"` are the quasi-likelihood information criteria (Pan 2001), `"scale"` the estimated scale (dispersion) parameter, and `"max_cluster_size"` the largest cluster in the estimation sample.
      Received by 12:
        "qic": betareg + gam_betar, coxph + cph, feols_nofe + lm_robust_fe,
            fepois + lm, gam + glmmTMB_beta, glm_poisson + glm.nb,
            glmmTMB_nbinom1 + glmmTMB_nbinom2, lm + glm_logit, lm +
            svyglm_binomial, lrm_ordinal + rms_Glm_logit, svycoxph +
            svyglm_binomial, zeroinfl + hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"qicu"` in `show_fit_stats` is defined only for GEE fits (`geepack::geeglm`).
        i `"qic"` / `"qicu"` are the quasi-likelihood information criteria (Pan 2001), `"scale"` the estimated scale (dispersion) parameter, and `"max_cluster_size"` the largest cluster in the estimation sample.
      Received by 12:
        "qicu": betareg + gam_betar, coxph + cph, feols_nofe + lm_robust_fe,
            fepois + lm, gam + glmmTMB_beta, glm_poisson + glm.nb,
            glmmTMB_nbinom1 + glmmTMB_nbinom2, lm + glm_logit, lm +
            svyglm_binomial, lrm_ordinal + rms_Glm_logit, svycoxph +
            svyglm_binomial, zeroinfl + hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is defined only for `glm` and `mgcv` GAM families other than the binomial, and no model in the table is one.
        i For a binomial, ordinal or multinomial response, the deviance R<U+00B2> equals McFadden's R<U+00B2>: use `"pseudo_r2_mcfadden"`.
      Received by 3:
        "r2_deviance": lm + glm_logit, lm + svyglm_binomial, lrm_ordinal +
            rms_Glm_logit
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is defined only for `glm` and `mgcv` GAM families other than the binomial, and no model in the table is one.
        i It is 1 - deviance / null deviance of a `glm` or a GAM of a gaussian, Gamma, inverse Gaussian, Tweedie, Poisson, negative binomial or quasi family.
      Received by 5:
        "r2_deviance": betareg + gam_betar, coxph + cph, feols_nofe +
            lm_robust_fe, fepois + lm, zeroinfl + hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is not defined for mixed-effects models.
        i Classical / pseudo R<U+00B2> do not apply to mixed-effects models.
        i Use the Nakagawa R<U+00B2> instead: `"r2_marginal"` or `"r2_conditional"` (Nakagawa et al. 2017).
      Received by 1:
        "r2_deviance": glmmTMB_nbinom1 + glmmTMB_nbinom2
      ----
      error: spicy_invalid_input, spicy_error
        Token `"r2_deviance"` in `show_fit_stats` is not defined for survey-design fits.
        i A design-based fit is estimated without a likelihood. For `svyglm()`, `"aic"` gives the design-based AIC of survey's `extractAIC()`.
      Received by 1:
        "r2_deviance": svycoxph + svyglm_binomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"scale"` in `show_fit_stats` is defined only for GEE fits (`geepack::geeglm`).
        i `"qic"` / `"qicu"` are the quasi-likelihood information criteria (Pan 2001), `"scale"` the estimated scale (dispersion) parameter, and `"max_cluster_size"` the largest cluster in the estimation sample.
      Received by 12:
        "scale": betareg + gam_betar, coxph + cph, feols_nofe + lm_robust_fe,
            fepois + lm, gam + glmmTMB_beta, glm_poisson + glm.nb,
            glmmTMB_nbinom1 + glmmTMB_nbinom2, lm + glm_logit, lm +
            svyglm_binomial, lrm_ordinal + rms_Glm_logit, svycoxph +
            svyglm_binomial, zeroinfl + hurdle_negbin
      ----
      error: spicy_invalid_input, spicy_error
        Token `"theta"` in `show_fit_stats` is defined only for NB2 negative-binomial fits (`MASS::glm.nb()`, `lme4::glmer.nb()`, `fixest::fenegbin()`, mgcv `nb()`, pscl `zeroinfl()` or `hurdle()` with `dist = "negbin"`, glmmTMB `nbinom2`), and no model in the table is one.
        i theta is the NB2 dispersion (V = mu + mu<U+00B2>/theta) and alpha its reciprocal. An NB1 fit (glmmTMB `nbinom1`) reports `"delta"` instead; other families have no such parameter.
        i A theta fixed in the call (`negative.binomial(theta)` in `glm()`, mgcv `negbin(theta)` or `nb(theta)`) is not an estimate and has no row.
      Received by 10:
        "theta": betareg + gam_betar, coxph + cph, feols_nofe + lm_robust_fe,
            fepois + lm, gam + glmmTMB_beta, geeglm + glm_poisson, lm +
            glm_logit, lm + svyglm_binomial, lrm_ordinal + rms_Glm_logit,
            svycoxph + svyglm_binomial
      ----
      error: spicy_invalid_input, spicy_error
        Token `"within_r2"` in `show_fit_stats` has no value for any model in the table.
        i The `show_fit_stats` section of ?table_regression lists the models each token is computed for.
      Received by 1:
        "within_r2": fepois + lm
      ----
      error: spicy_invalid_input, spicy_error
        Token `"within_r2"` in `show_fit_stats` is defined only for fits that absorb fixed effects (fixest, or estimatr with `fixed_effects =`).
        i `"fixed_effects"` discloses the absorbed fixed effects (one Yes/No row per factor) and `"within_r2"` is the FE-partialed R<U+00B2>; other classes have neither.
      Received by 11:
        "within_r2": betareg + gam_betar, coxph + cph, gam + glmmTMB_beta, geeglm
            + glm_poisson, glm_poisson + glm.nb, glmmTMB_nbinom1 +
            glmmTMB_nbinom2, lm + glm_logit, lm + svyglm_binomial, lrm_ordinal +
            rms_Glm_logit, svycoxph + svyglm_binomial, zeroinfl + hurdle_negbin

