# ---------------------------------------------------------------------------
# Supported-models registry for table_regression().
#
# ONE machine-readable source of truth (the registry data frame below) feeds:
#   * the exported accessor table_regression_models() (insight-style
#     supported_models() pattern -- the published list can never drift from
#     the code, unlike a hand-maintained Rd table);
#   * the capability table on its documentation page (rendered from the
#     registry at roxygen time via inline R);
#   * the pkgdown "Supported models" article.
#
# The documentation page is the overview of the supported classes; each
# model family has its own concept page (?table_regression_mixed, ...),
# with an Engines table rendered from the same registry and a Usage
# block rendered from the signature of table_regression().
# ---------------------------------------------------------------------------

# The registry. One row per supported engine call. Kept internal; exposed
# through table_regression_models().
.supported_models_registry <- function() {
  df <- rbind(
    c("Linear and generalized linear", "lm", "stats::lm()", "yes", "-", "-"),
    c(
      "Linear and generalized linear",
      "glm",
      "stats::glm()",
      "yes",
      "OR / IRR / RR / MR / HR (link)",
      "-"
    ),
    c(
      "Linear and generalized linear",
      "negbin",
      "MASS::glm.nb()",
      "yes",
      "IRR",
      "-"
    ),
    c("Linear and generalized linear", "rlm", "MASS::rlm()", "yes", "-", "-"),
    c("Linear and generalized linear", "nls", "stats::nls()", "no", "-", "-"),
    c(
      "Robust, IV, quantile, panel",
      "lm_robust",
      "estimatr::lm_robust()",
      "yes",
      "-",
      "-"
    ),
    c(
      "Robust, IV, quantile, panel",
      "iv_robust",
      "estimatr::iv_robust()",
      "yes",
      "-",
      "-"
    ),
    c("Robust, IV, quantile, panel", "ivreg", "AER::ivreg()", "yes", "-", "-"),
    c("Robust, IV, quantile, panel", "tobit", "AER::tobit()", "yes", "-", "-"),
    c("Robust, IV, quantile, panel", "rq", "quantreg::rq()", "yes", "-", "-"),
    c(
      "Robust, IV, quantile, panel",
      "fixest",
      "fixest::feols(), fixest::feglm(), fixest::fepois(), fixest::femlm(), fixest::fenegbin()",
      "yes (no for a GLM with fixed effects)",
      "OR / IRR (link)",
      "-"
    ),
    c("Mixed effects", "lmerMod", "lme4::lmer()", "yes", "-", .REG_BLOCK_RE),
    c(
      "Mixed effects",
      "glmerMod",
      "lme4::glmer()",
      "yes",
      "OR / IRR (link)",
      .REG_BLOCK_RE
    ),
    c(
      "Mixed effects",
      "glmmTMB",
      "glmmTMB::glmmTMB()",
      "yes (no for the ordinal family)",
      "link-dependent (IRR for count families, OR for ordinal logit)",
      paste(
        c(
          .REG_BLOCK_RE,
          .REG_BLOCK_ZI,
          paste0(
            .REG_BLOCK_HURDLE_ZERO,
            " for a zero-truncated family, ziGamma, lognormal or beta_family"
          ),
          .REG_BLOCK_DISP,
          paste0(.REG_BLOCK_THRESH, " (ordinal family)")
        ),
        collapse = "; "
      )
    ),
    c("Mixed effects", "lme", "nlme::lme()", "yes", "-", .REG_BLOCK_RE),
    c("Mixed effects", "gls", "nlme::gls()", "yes", "-", "-"),
    c(
      "Ordinal",
      "polr",
      "MASS::polr()",
      "per category",
      "OR (logit)",
      .REG_BLOCK_THRESH
    ),
    c(
      "Ordinal",
      "clm",
      "ordinal::clm()",
      "per category",
      "OR (logit)",
      paste(c(.REG_BLOCK_THRESH, .REG_BLOCK_NPO), collapse = "; ")
    ),
    c(
      "Ordinal",
      "clmm",
      "ordinal::clmm()",
      "no",
      "OR (logit)",
      paste(c(.REG_BLOCK_THRESH, .REG_BLOCK_RE), collapse = "; ")
    ),
    c(
      "Categorical",
      "multinom",
      "nnet::multinom()",
      "per outcome",
      "OR",
      "per-outcome blocks"
    ),
    c(
      "Categorical",
      "mlogit",
      "mlogit::mlogit()",
      "no",
      "OR",
      "per-alternative rows"
    ),
    c(
      "Counts, two-part",
      "zeroinfl",
      "pscl::zeroinfl()",
      "yes (combined response)",
      "IRR (count) + OR (logit zero part)",
      .REG_BLOCK_ZI
    ),
    c(
      "Counts, two-part",
      "hurdle",
      "pscl::hurdle()",
      "yes (combined response)",
      "IRR (count) + OR (logit zero part)",
      .REG_BLOCK_HURDLE
    ),
    c("Survival", "coxph", "survival::coxph()", "RMST / risk diff", "HR", "-"),
    c(
      "Survival",
      "survreg",
      "survival::survreg()",
      "yes + RMST / risk diff",
      "TR (log-scale distributions)",
      "-"
    ),
    c("Survival", "cph", "rms::cph()", "no", "HR", "-"),
    c(
      "Survival",
      "flexsurvreg",
      "flexsurv::flexsurvreg()",
      "no",
      "TR / HR (dist)",
      "distribution parameters"
    ),
    c(
      "Survey-weighted",
      "svyglm",
      "survey::svyglm()",
      "yes (design-based)",
      "OR / IRR",
      "-"
    ),
    c(
      "Survey-weighted",
      "svyolr",
      "survey::svyolr()",
      "per category (design-based)",
      "OR (logit)",
      .REG_BLOCK_THRESH
    ),
    c(
      "Survey-weighted",
      "svycoxph",
      "survey::svycoxph()",
      "no",
      "HR",
      "-"
    ),
    c(
      "Population-averaged (GEE)",
      "geeglm",
      "geepack::geeglm()",
      "yes",
      "OR / IRR / RR / MR / HR (link)",
      "-"
    ),
    c(
      "Additive, proportions, selection",
      "gam",
      "mgcv::gam(), mgcv::bam()",
      "yes",
      "OR / IRR (link)",
      "-"
    ),
    c(
      "Additive, proportions, selection",
      "betareg",
      "betareg::betareg()",
      "yes",
      "OR (mean link)",
      "-"
    ),
    c(
      "Additive, proportions, selection",
      "selection",
      "sampleSelection::selection()",
      "no",
      "-",
      "selection component"
    ),
    c("rms", "ols", "rms::ols()", "yes", "-", "-"),
    c(
      "rms",
      "lrm",
      "rms::lrm()",
      "yes (per category for an ordinal outcome)",
      "OR",
      paste0(.REG_BLOCK_THRESH, " (ordinal outcome)")
    ),
    c("rms", "Glm", "rms::Glm()", "yes", "link-dependent", "-"),
    c(
      "Bayesian",
      "stanreg",
      "rstanarm::stan_glm(), rstanarm::stan_glmer()",
      "yes (draws)",
      "link-dependent",
      paste0(.REG_BLOCK_RE, " (if multilevel)")
    ),
    c(
      "Bayesian",
      "brmsfit",
      "brms::brm()",
      "yes (draws)",
      "link-dependent",
      paste0(.REG_BLOCK_RE, " (if multilevel)")
    )
  )
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  names(out) <- c("family", "class", "engine", "ame", "exponentiate", "blocks")
  out$vcov <- unname(.REG_VCOV[out$class])
  out$fit_stats_default <- unname(.REG_FIT_STATS_DEFAULT[out$class])
  out$fit_stats <- unname(.REG_FIT_STATS[out$class])
  stopifnot(
    !anyNA(out$vcov),
    !anyNA(out$fit_stats_default),
    !anyNA(out$fit_stats)
  )
  out
}

# The tokens of a registry cell (`vcov`, `fit_stats_default`, `fit_stats`),
# in their order. A cell is a comma-separated list; a parenthesis after an
# item gives the condition under which it holds, and its own commas do not
# split the cell. `conditional = FALSE` keeps the items without a
# condition. A range such as "HC0-HC5" gives every token of it.
.reg_cell_tokens <- function(cell, conditional = TRUE) {
  items <- trimws(strsplit(cell, ",(?![^()]*\\))", perl = TRUE)[[1L]])
  if (!conditional) {
    items <- items[!grepl("(", items, fixed = TRUE)]
  }
  items <- trimws(sub("\\s*\\(.*$", "", items))
  items <- items[nzchar(items)]
  unlist(lapply(items, function(s) {
    m <- regmatches(s, regexec("^([A-Z]+)([0-9])-([A-Z]+)([0-9])$", s))[[1L]]
    if (length(m) == 5L && identical(m[2L], m[4L])) {
      paste0(m[2L], seq(as.integer(m[3L]), as.integer(m[5L])))
    } else {
      s
    }
  }))
}

# The `vcov` values each class accepts, one string per registry class:
# the column `vcov` of the registry. Written by hand from the capability
# gate (.robust_vcov_support() in R/vcov.R, and the refusals of
# validate_vcov_cluster_lists()); each cell was checked on 2026-11-19 by
# fitting each class and calling table_regression() with every token of
# .VCOV_MODES. A parenthesis gives the condition under which the set
# shrinks, or what "classical" stands for when the class carries its own
# variance.
.REG_VCOV <- c(
  lm = "classical, HC0-HC5, CR0-CR3, CR1S, bootstrap, jackknife",
  glm = "classical, HC0-HC5, CR0-CR3, bootstrap, jackknife",
  negbin = "classical, HC0-HC5, CR0-CR3, bootstrap, jackknife",
  rlm = "classical",
  nls = "classical",
  lm_robust = "classical (the fit's own)",
  iv_robust = "classical (the fit's own)",
  ivreg = "classical",
  tobit = "classical, CR1",
  rq = "classical (= nid), nid, iid, ker, rank, bootstrap",
  fixest = "classical (the fit's own)",
  lmerMod = "classical, CR0-CR3",
  glmerMod = "classical",
  glmmTMB = "classical, CR0, CR1 (classical only for REML or the ordinal family)",
  lme = "classical, CR0-CR3 (classical only with contrasts other than the session's)",
  gls = "classical",
  geeglm = "classical (the fit's own)",
  polr = "classical, CR1",
  clm = "classical, CR1 (classical only with scale or nominal terms)",
  clmm = "classical",
  multinom = "classical, CR1",
  mlogit = "classical, CR1",
  zeroinfl = "classical, CR1 (classical only for a negative-binomial part)",
  hurdle = "classical, CR1 (classical only for a negative-binomial part)",
  coxph = "classical, CR0",
  survreg = "classical, CR1",
  cph = "classical, CR0 (classical only without x = TRUE, y = TRUE)",
  flexsurvreg = "classical",
  svyglm = "classical (design-based)",
  svyolr = "classical (design-based)",
  svycoxph = "classical (design-based)",
  gam = "classical, CR1",
  betareg = "classical, CR1",
  selection = "classical",
  ols = "classical, CR0 (classical only without x = TRUE, y = TRUE)",
  lrm = "classical, CR0 (classical only without x = TRUE, y = TRUE)",
  Glm = "classical, CR0 (classical only without x = TRUE, y = TRUE)",
  stanreg = "classical (the posterior)",
  brmsfit = "classical (the posterior)"
)

# The fit statistics of a one-model table, one string per registry class:
# the columns `fit_stats_default` (the rows the default
# `show_fit_stats = NULL` shows, in their order) and `fit_stats` (the
# tokens that give a row when named in `show_fit_stats`, in the order of
# the vocabulary; the change tokens, which need `nested = TRUE`, are left
# out). Read from the behavior on 2026-11-19: one light fit per class and
# the variants of tests/testthat/helper-class-fixtures.R, every token of
# the vocabulary. A parenthesis gives the fits a token is limited to, as
# measured on those fixtures; tests/testthat/test-fit_stats_requested_rows.R
# names the fixture behind each one.
.REG_FIT_STATS_DEFAULT <- c(
  lm = "nobs, r2, adj_r2",
  glm = paste(
    "nobs, pseudo_r2_tjur (binomial logit, binary outcome),",
    "pseudo_r2_mcfadden (binomial, Poisson), r2_deviance (Gaussian, Gamma),",
    "aic (not quasi)"
  ),
  negbin = "nobs, pseudo_r2_mcfadden, aic",
  rlm = "nobs",
  nls = "nobs, aic",
  lm_robust = "fixed_effects (with fixed_effects =), nobs, r2, adj_r2",
  iv_robust = "nobs",
  ivreg = "nobs",
  tobit = "nobs, aic",
  rq = "nobs, aic",
  fixest = paste(
    "fixed_effects (with fixed effects), nobs, r2 (feols, not IV),",
    "within_r2 (feols with fixed effects, not IV),",
    "pseudo_r2_mcfadden (GLM family), aic (not IV)"
  ),
  lmerMod = "nobs, n_groups, icc, r2_marginal, r2_conditional, aic, bic",
  glmerMod = "nobs, n_groups, icc, r2_marginal, r2_conditional, aic, bic",
  glmmTMB = paste(
    "nobs, n_groups,",
    "icc (Gaussian, no zero-inflation or dispersion formula),",
    "r2_marginal (no ordinal family, zero-inflation or dispersion formula),",
    "r2_conditional (Gaussian, no zero-inflation or dispersion formula),",
    "aic, bic"
  ),
  lme = "nobs, n_groups, icc, r2_marginal, r2_conditional, aic, bic",
  gls = "nobs, aic",
  polr = "nobs, pseudo_r2_mcfadden, aic",
  clm = "nobs, pseudo_r2_mcfadden, aic",
  clmm = "nobs, n_groups, aic, bic",
  multinom = "nobs, pseudo_r2_mcfadden, aic",
  mlogit = "nobs, aic",
  zeroinfl = "nobs, aic",
  hurdle = "nobs, aic",
  coxph = "nobs, n_events, aic",
  survreg = "nobs, aic",
  cph = "nobs, n_events, aic",
  flexsurvreg = "nobs, aic",
  svyglm = "nobs, weighted_nobs, aic",
  svyolr = "nobs, weighted_nobs",
  svycoxph = "nobs, weighted_nobs, n_events",
  geeglm = "nobs, n_groups, max_cluster_size",
  gam = paste(
    "nobs, pseudo_r2_tjur (binomial logit, binary outcome),",
    "pseudo_r2_mcfadden (binomial, Poisson, negative binomial),",
    "r2_deviance (Gaussian, Gamma, Tweedie), aic (not quasi)"
  ),
  betareg = "nobs, aic",
  selection = "nobs, aic",
  ols = "nobs, r2",
  lrm = paste(
    "nobs, pseudo_r2_tjur (binary outcome), pseudo_r2_mcfadden, aic"
  ),
  Glm = "nobs, pseudo_r2_mcfadden, aic",
  stanreg = "nobs, r2_bayes",
  brmsfit = "nobs, r2_bayes"
)

.REG_FIT_STATS <- c(
  lm = paste(
    "nobs, weighted_nobs (weighted fit), r2, adj_r2, omega2, sigma, rmse,",
    "f2, aic, aicc, bic, deviance, log_lik"
  ),
  glm = paste(
    "nobs, pseudo_r2_mcfadden (binomial, Poisson),",
    "pseudo_r2_nagelkerke (binomial, Poisson), pseudo_r2_tjur (binomial),",
    "r2_deviance (not binomial), sigma, rmse, aic (not quasi),",
    "aicc (not quasi), bic (not quasi), deviance, log_lik (not quasi)"
  ),
  negbin = paste(
    "nobs, pseudo_r2_mcfadden, pseudo_r2_nagelkerke, r2_deviance, theta,",
    "alpha, rmse, aic, aicc, bic, deviance, log_lik"
  ),
  rlm = "nobs, sigma, aic, aicc, bic, log_lik",
  nls = "nobs, sigma, aic, aicc, bic, deviance, log_lik",
  lm_robust = paste(
    "nobs, r2, adj_r2, fixed_effects (with fixed_effects =),",
    "within_r2 (with fixed_effects =), n_groups (with fixed_effects =)"
  ),
  iv_robust = "nobs, r2, adj_r2",
  ivreg = "nobs, r2, adj_r2",
  tobit = "nobs, sigma, aic, aicc, bic, log_lik",
  rq = "nobs, aic, aicc, log_lik",
  fixest = paste(
    "nobs, r2 (feols), adj_r2 (feols), pseudo_r2_mcfadden (GLM family),",
    "fixed_effects (with fixed effects),",
    "within_r2 (feols with fixed effects), n_groups (with fixed effects),",
    "sigma, aic, aicc, bic, deviance, log_lik"
  ),
  lmerMod = paste(
    "nobs, r2_marginal, r2_conditional, n_groups, icc, sigma, aic, aicc,",
    "bic, deviance, log_lik"
  ),
  glmerMod = paste(
    "nobs, r2_marginal, r2_conditional, n_groups, icc, sigma, aic, aicc,",
    "bic, deviance, log_lik"
  ),
  glmmTMB = paste(
    "nobs,",
    "r2_marginal (no ordinal family, zero-inflation or dispersion formula),",
    "r2_conditional (Gaussian, no zero-inflation or dispersion formula),",
    "n_groups, icc (Gaussian, no zero-inflation or dispersion formula),",
    "sigma (no ordinal family or dispersion formula), aic, aicc, bic,",
    "deviance (no ordinal or truncated family), log_lik"
  ),
  lme = paste(
    "nobs, r2_marginal, r2_conditional, n_groups, icc, sigma, aic, aicc,",
    "bic, log_lik"
  ),
  gls = "nobs, sigma, aic, aicc, bic, log_lik",
  polr = paste(
    "nobs, pseudo_r2_mcfadden, pseudo_r2_nagelkerke, aic, aicc, bic,",
    "deviance, log_lik"
  ),
  clm = paste(
    "nobs, pseudo_r2_mcfadden, pseudo_r2_nagelkerke, aic, aicc, bic,",
    "log_lik"
  ),
  clmm = "nobs, n_groups, aic, aicc, bic, deviance, log_lik",
  multinom = paste(
    "nobs, pseudo_r2_mcfadden, pseudo_r2_nagelkerke, aic, aicc, bic,",
    "deviance, log_lik"
  ),
  mlogit = "nobs, aic, aicc, bic, log_lik",
  zeroinfl = paste(
    "nobs, theta (negative-binomial), alpha (negative-binomial), aic, aicc,",
    "bic, log_lik"
  ),
  hurdle = paste(
    "nobs, theta (negative-binomial), alpha (negative-binomial), aic, aicc,",
    "bic, log_lik"
  ),
  coxph = "nobs, n_events, aic, aicc, bic, log_lik, concordance",
  survreg = "nobs, sigma (no strata), aic, aicc, bic, log_lik",
  cph = "nobs, n_events, aic, aicc, bic, log_lik",
  flexsurvreg = "nobs, aic, aicc, bic, log_lik",
  svyglm = "nobs, weighted_nobs, aic, eff_p",
  svyolr = "nobs, weighted_nobs",
  svycoxph = "nobs, weighted_nobs, n_events, concordance",
  geeglm = "nobs, qic, qicu, scale, max_cluster_size, n_groups",
  gam = paste(
    "nobs, adj_r2,",
    "pseudo_r2_mcfadden (binomial, Poisson, negative binomial),",
    "pseudo_r2_nagelkerke (binomial, Poisson, negative binomial),",
    "pseudo_r2_tjur (binomial), r2_deviance (not binomial), sigma, aic,",
    "aicc, bic, deviance, log_lik"
  ),
  betareg = "nobs, phi, aic, aicc, bic, log_lik",
  selection = "nobs, aic, aicc, bic, log_lik",
  ols = "nobs, r2, sigma, aic, aicc, bic, deviance, log_lik",
  lrm = paste(
    "nobs, pseudo_r2_mcfadden, pseudo_r2_nagelkerke,",
    "pseudo_r2_tjur (binary outcome), aic, aicc, bic, deviance, log_lik"
  ),
  Glm = paste(
    "nobs, pseudo_r2_mcfadden, pseudo_r2_nagelkerke, r2_deviance, aic,",
    "aicc, bic, deviance, log_lik"
  ),
  stanreg = "nobs, r2_bayes, elpd_loo, looic, waic",
  brmsfit = "nobs, r2_bayes, elpd_loo, looic, waic"
)

# The arguments of table_regression() that belong to some families only,
# each with the families whose page documents it. Every other argument is
# common to all classes (.REG_COMMON_ARGS). The Usage block of a family
# page lists these arguments first (.render_family_usage()).
.REG_FAMILY_ARGS <- list(
  tau = "Survival",
  at_time = "Survival",
  show_thresholds = "Ordinal",
  show_components = c("Ordinal", "Counts, two-part"),
  show_re = "Mixed effects",
  re_scale = "Mixed effects",
  re_columns = "Mixed effects",
  re_test = "Mixed effects",
  re_ci = "Mixed effects"
)

.REG_COMMON_ARGS <- c(
  "models",
  "vcov",
  "cluster",
  "ci_level",
  "ci_method",
  "boot_n",
  "standardized",
  "exponentiate",
  "ame_type",
  "partial_type",
  "p_adjust",
  "show_columns",
  "keep",
  "drop",
  "show_intercept",
  "intercept_position",
  "factor_layout",
  "reference_style",
  "reference_label",
  "show_fit_stats",
  "fit_stats_layout",
  "layout",
  "model_labels",
  "outcome_labels",
  "stars",
  "nested",
  "digits",
  "p_digits",
  "effect_size_digits",
  "fit_digits",
  "ic_digits",
  "decimal_mark",
  "align",
  "padding",
  "labels",
  "note_labels",
  "title",
  "note",
  "note_add",
  "output",
  "excel_path",
  "excel_sheet",
  "clipboard_delim",
  "word_path",
  "word_template",
  "style"
)

# The help page of each family that has one.
.REG_FAMILY_PAGE <- c(
  "Robust, IV, quantile, panel" = "table_regression_robust",
  "Mixed effects" = "table_regression_mixed",
  "Ordinal" = "table_regression_ordinal",
  "Categorical" = "table_regression_categorical",
  "Counts, two-part" = "table_regression_counts",
  "Survival" = "table_regression_survival",
  "Survey-weighted" = "table_regression_survey",
  "Bayesian" = "table_regression_bayesian"
)

# The other families a class also serves, with what it brings to each:
# glmmTMB is a mixed engine (its row of the registry) that also fits
# ordinal models and zero-inflated, hurdle and dispersion models. One
# entry per class, a named vector family = what the class covers there.
.REG_ALSO <- list(
  glmmTMB = c(
    "Ordinal" = "ordinal family",
    "Counts, two-part" = "zero-inflation, hurdle and dispersion components"
  )
)

# What a family has in common across its engines, for the summary table
# of the README (one line per family).
.REG_FAMILY_NOTE <- c(
  "Survey-weighted" = "design-based SEs",
  "Population-averaged (GEE)" = "native sandwich SEs, working correlation disclosed",
  "Bayesian" = "posterior median, credible intervals, no p-values"
)

# One row per family, in the order of the registry: its engines, each
# function backticked, an engine whose class has a
# partial-proportional-odds block marked so, the engines another family
# lends it (.REG_ALSO), then the note of the family (.REG_FAMILY_NOTE).
# A markdown pipe table, for the README.
.render_family_engines_md <- function() {
  reg <- .supported_models_registry()
  stopifnot(
    all(names(.REG_ALSO) %in% reg$class),
    all(unlist(lapply(.REG_ALSO, names)) %in% reg$family),
    all(names(.REG_FAMILY_NOTE) %in% reg$family)
  )
  fams <- unique(reg$family)
  cells <- vapply(
    fams,
    function(f) {
      rows <- reg[reg$family == f, , drop = FALSE]
      own <- unlist(lapply(seq_len(nrow(rows)), function(i) {
        fns <- paste0(
          "`",
          strsplit(rows$engine[i], ", ", fixed = TRUE)[[1]],
          "`"
        )
        if (grepl(.REG_BLOCK_NPO, rows$blocks[i], fixed = TRUE)) {
          fns[length(fns)] <- paste(
            fns[length(fns)],
            "(incl. partial proportional odds)"
          )
        }
        fns
      }))
      lent <- unlist(lapply(names(.REG_ALSO), function(cl) {
        what <- .REG_ALSO[[cl]][f]
        if (is.na(what)) {
          return(NULL)
        }
        fns <- strsplit(reg$engine[reg$class == cl], ", ", fixed = TRUE)[[1]]
        paste0(paste0("`", fns, "`", collapse = ", "), " (", what, ")")
      }))
      out <- paste(unique(c(own, lent)), collapse = ", ")
      if (!is.na(.REG_FAMILY_NOTE[f])) {
        out <- paste0(out, " (", .REG_FAMILY_NOTE[[f]], ")")
      }
      out
    },
    character(1)
  )
  paste(
    c(
      "| Family | Engines |",
      "|---|---|",
      paste0("| ", fams, " | ", cells, " |")
    ),
    collapse = "\n"
  )
}


# Render the registry as a markdown pipe table for the roxygen page below
# (evaluated at document() time -- the published table can never drift from
# the registry).
# `family` keeps the rows of one family and drops the Family column, for
# the page of that family (?table_regression_mixed); a name that is not a
# family of the registry stops document(). `columns` keeps those columns
# only, for the table of the variance estimators on ?table_regression; by
# default every column but the two of the fit statistics, too long for a
# table.
.render_supported_models_md <- function(
  family = NULL,
  columns = c(
    "family",
    "class",
    "engine",
    "ame",
    "exponentiate",
    "blocks",
    "vcov"
  )
) {
  reg <- .supported_models_registry()
  if (!is.null(family)) {
    if (!family %in% reg$family) {
      stop("Not a family of the registry: ", family, call. = FALSE)
    }
    reg <- reg[reg$family == family, setdiff(names(reg), "family")]
  }
  if (!is.null(columns)) {
    reg <- reg[, intersect(columns, names(reg)), drop = FALSE]
  }
  # Class and engine are code: backtick them (correct typography, and the
  # spell checker skips code spans). Engines are backticked ONE FUNCTION PER
  # SPAN: downlit auto-links an inline code span only when the whole span is
  # a single `pkg::fun()` call, so each function becomes clickable on the
  # pkgdown site.
  if ("class" %in% names(reg)) {
    reg$class <- paste0("`", reg$class, "`")
  }
  if ("engine" %in% names(reg)) {
    reg$engine <- vapply(
      strsplit(reg$engine, ", ", fixed = TRUE),
      function(fns) {
        paste0("`", fns, "`", collapse = ", ")
      },
      character(1)
    )
  }
  labels <- c(
    family = "Family",
    class = "Class",
    engine = "Engine",
    ame = "AME",
    exponentiate = "Exponentiate",
    blocks = "Blocks",
    vcov = "vcov",
    fit_stats_default = "Default fit statistics",
    fit_stats = "Fit statistics"
  )[names(reg)]
  header <- paste0("| ", paste(labels, collapse = " | "), " |")
  sep <- paste0("|", strrep("---|", length(labels)))
  rows <- vapply(
    seq_len(nrow(reg)),
    function(i) {
      paste0("| ", paste(unlist(reg[i, ]), collapse = " | "), " |")
    },
    character(1)
  )
  paste(c(header, sep, rows), collapse = "\n")
}


# The Usage block of a family page (?table_regression_mixed, ...),
# rendered at document() time: the call with `models`, the arguments that
# matter for the family with their defaults as formals() gives them, then
# `...`. A family page carries no real \usage: codoc() compares a \usage
# with the whole signature and reports every argument left out, and
# checkDocFiles() asks for an alias of table_regression(), which belongs
# to the page of the function. The arguments of the family come from
# .REG_FAMILY_ARGS, in the order of the signature; `also` adds the common
# arguments the page discusses. Every name is checked against formals(),
# so an argument renamed or removed stops document() here and the block
# cannot drift from the signature.
.render_family_usage <- function(family, also = character()) {
  f <- formals(table_regression)
  if (!family %in% names(.REG_FAMILY_PAGE)) {
    stop("Not a family with a page: ", family, call. = FALSE)
  }
  own <- names(.REG_FAMILY_ARGS)[vapply(
    .REG_FAMILY_ARGS,
    function(fams) family %in% fams,
    logical(1)
  )]
  args <- c(intersect(names(f), own), also)
  unknown <- setdiff(args, names(f))
  if (length(unknown) > 0L) {
    stop(
      "Not an argument of table_regression(): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  arg_line <- function(a) {
    paste0(
      "  ",
      a,
      " = ",
      paste(deparse(f[[a]], width.cutoff = 500L), collapse = " "),
      ","
    )
  }
  paste(
    c(
      "```r",
      "table_regression(",
      "  models,",
      vapply(args, arg_line, character(1), USE.NAMES = FALSE),
      "  ...  # the arguments of every class: see ?table_regression",
      ")",
      "```"
    ),
    collapse = "\n"
  )
}

# The default fit statistics of a class as a code span for a family page,
# from the registry column `fit_stats_default`: the tokens that hold for
# every fit of the class (an item with a condition is left out), as
# `c("nobs", "aic")`, or `"nobs"` alone.
.render_fit_stats_default <- function(class) {
  reg <- .supported_models_registry()
  if (!class %in% reg$class) {
    stop("Not a class of the registry: ", class, call. = FALSE)
  }
  toks <- .reg_cell_tokens(
    reg$fit_stats_default[reg$class == class],
    conditional = FALSE
  )
  q <- paste0("\"", toks, "\"")
  if (length(q) == 1L) {
    paste0("`", q, "`")
  } else {
    paste0("`c(", paste(q, collapse = ", "), ")`")
  }
}


#' Supported models of table_regression()
#'
#' @description
#' `table_regression_models()` returns the registry of model classes
#' supported by [table_regression()], one row per engine, with each class's
#' family, average-marginal-effects estimand, `exponentiate` semantics,
#' labeled table blocks, accepted `vcov` values and fit statistics. The
#' same registry
#' drives the table of this page and those of the family pages, so the
#' published list cannot drift from the code.
#'
#' This page is the overview: the table of every supported class, the
#' rules all classes share, and the page of each family, which gives its
#' arguments, the values the shared arguments take for it, and what it
#' refuses.
#'
#' If a class is not listed: fit the model and call `table_regression(fit)`
#' anyway -- unsupported classes error with a clear message naming the
#' supported set. Feature requests are welcome on the issue tracker.
#'
#' @section Supported classes:
#'
#' `r spicy:::.render_supported_models_md()`
#'
#' AME: whether the average-marginal-effects columns are available, and
#' what they estimate (`per category` for ordinal models, `per outcome`
#' for multinomial models, `yes (draws)` for Bayesian fits). Exponentiate:
#' what `exponentiate = TRUE` gives. Blocks: the labeled blocks of rows the
#' class adds below its coefficients. vcov: the values of `vcov` the class
#' accepts; see *Robust SE availability by model class* in
#' [table_regression()].
#'
#' @section Pages by model family:
#' \itemize{
#'   \item [table_regression_mixed]: `lmer`, `glmer`, `glmmTMB`, `lme`
#'     and `gls` fits.
#'   \item [table_regression_survival]: `coxph`, `survreg`, `cph` and
#'     `flexsurvreg` fits.
#'   \item [table_regression_ordinal]: `polr`, `clm` and `clmm` fits.
#'   \item [table_regression_categorical]: `multinom` and `mlogit` fits.
#'   \item [table_regression_counts]: `zeroinfl` and `hurdle` fits, and
#'     the zero-inflation, hurdle and dispersion blocks of `glmmTMB`.
#'   \item [table_regression_survey]: `svyglm`, `svyolr` and `svycoxph`
#'     fits.
#'   \item [table_regression_bayesian]: `stanreg` and `brmsfit` fits.
#'   \item [table_regression_robust]: `lm_robust`, `iv_robust`, `ivreg`,
#'     `tobit`, `rq` and `fixest` fits.
#' }
#' The linear and generalized linear models are documented on
#' [table_regression()] itself. GEE fits are described below. The
#' additive, beta-regression, sample-selection and `rms` fits follow the
#' rules of [table_regression()] and the table above.
#'
#' @section Shared semantics (all classes):
#' \itemize{
#'   \item A robust `vcov` request is honored through the class's
#'     backend, or refused with an error that names the supported set; the
#'     footer names the estimator applied.
#'   \item `exponentiate = TRUE` is link-gated: it produces a labeled ratio
#'     (OR / IRR / HR / RR / MR / TR) only where the link warrants
#'     one. Identity-link fits warn and are left untouched; non-ratio
#'     links (probit, cauchit, inverse, ...) are refused with an error.
#'   \item Class-specific structure renders as labeled subordinate blocks
#'     of rows in the same table, each explained by a footer line.
#'   \item Fit statistics default to the family's field standard
#'     (`show_fit_stats` overrides; class-inappropriate tokens are rejected
#'     with a pointer to the right ones).
#'   \item Everything is available programmatically: `broom::tidy()`,
#'     `glance()`, [as_structured()], `as.data.frame()`.
#' }
#'
#' @section Population-averaged (GEE) models:
#' `geepack::geeglm()` fits are read on their own terms: the sandwich
#' standard errors the fit computed (its `std.err =` option, clustered
#' on its `id =`) are the displayed inference -- GEE is robust by
#' construction, so spicy's `HC*` and `CR*` tokens, the resamplers and
#' `cluster` are refused with a pointer to the fit options. Coefficients
#' are population-averaged (marginal) effects; the footer discloses the
#' working correlation structure with its estimated alpha. Wald z
#' inference; `exponentiate` follows the usual link gates (OR / IRR /
#' RR). Default fit statistics report the cluster structure (n,
#' `N (<id>)`, largest cluster); the quasi-likelihood information
#' criteria `"qic"` / `"qicu"` (Pan 2001) and the `"scale"`
#' (dispersion) parameter are opt-in -- there is no likelihood, so
#' AIC, pseudo-R-squared, `nested = TRUE`, and `standardized` are
#' refused. See the
#' [GEE regression tables](https://amaltawfik.github.io/spicy/articles/table-regression-gee.html)
#' article, and the population-averaged section of the
#' [Mixed-effects regression tables](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.html)
#' article for the contrast with subject-specific mixed models.
#'
#' @references
#' Pan, W. (2001). Akaike's information criterion in generalized
#' estimating equations. *Biometrics*, 57(1), 120-125.
#' \doi{10.1111/j.0006-341X.2001.00120.x}
#'
#' @return A data frame with one row per supported engine and columns
#'   `family`, `class`, `engine`, `ame`, `exponentiate`, `blocks`, `vcov`
#'   (the values of `vcov` the class accepts), `fit_stats_default` (the
#'   fit-statistic rows of a one-model table under the default
#'   `show_fit_stats`, in their order) and `fit_stats` (the tokens of
#'   `show_fit_stats` that give a row for one model; the change tokens,
#'   which need `nested = TRUE`, are not listed). A parenthesis in a cell
#'   gives the fits an item is limited to.
#'
#' @seealso [table_regression()]; the family pages listed under *Pages by
#'   model family*; the
#'   [Supported models](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.html)
#'   and
#'   [Publication-ready regression tables](https://amaltawfik.github.io/spicy/articles/table-regression.html)
#'   articles.
#'
#' @examples
#' table_regression_models()
#'
#' # All engines of one family:
#' subset(table_regression_models(), family == "Mixed effects")
#' @export
table_regression_models <- function() {
  .supported_models_registry()
}
