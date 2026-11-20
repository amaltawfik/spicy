# Internal validators for table_regression().
#
# Coverage convention:
#   The `# nocov start` / `# nocov end` pragmas in
#   `validate_output_resources()` mark missing-Suggests guards that
#   only fire when the optional output engine package is uninstalled.
#   See R/regression_dispatch.R for the same convention applied to
#   the dispatcher's own engine guards.
#
# Implements the validation cascade specified in
# dev/table_regression_design.md Q21: 6 phases, 29 steps, fail-fast
# at first error except step 3 (multi-model class aggregate-fail per
# Q9). Each step is a separate `validate_*()` helper, called in
# deterministic order by table_regression() itself.
#
# Phase A \u2013 input class            (steps 1-3)
# Phase B \u2013 multi-model alignment  (steps 4-8)
# Phase C \u2013 vocabulary tokens      (steps 9-12)
# Phase D \u2013 argument values        (steps 13-24)
# Phase E \u2013 cross-arg semantic     (steps 25-26, warnings only)
# Phase F \u2013 output-dependent       (steps 27-29)

# ---- Token vocabularies (canonical) ---------------------------------------

# Token vocabularies (lm + glm). Match dev/table_regression_design.md \u00A74.
# Centralized here so validators, the rendering layer, and the test suite
# share a single source of truth. Class-aware token compatibility is
# checked separately, by `validate_class_appropriate_tokens()` -- one
# validator, not two: the nested-stats half was folded into it when the
# change statistics became fit-statistic ROWS (feb7f529).
# The AME columns: any one of them makes the table compute the AMEs, and
# the classes without AMEs refuse them all.
.AME_COLUMN_TOKENS <- c("ame", "ame_se", "ame_ci", "ame_p", "ame_df")

.regression_tokens <- list(
  # ATOMIC tokens for `show_columns`: one token = one displayed
  # column. All lowercase (idiomatic R / broom convention). Group
  # tokens like "all_b", "all_ame" expand to a fixed subset of
  # these (see `.show_columns_groups` below).
  show_columns_atomic = c(
    # B-coefficient family
    "b",
    "beta",
    "se",
    "ci",
    "t",
    "p",
    # The df of each coefficient's test (t tests; blank under z)
    "df",
    # Per-row N (populated by univariable-screening frames, where each
    # predictor block is its own fit; blank for ordinary models)
    "n",
    # Outcome event counts "events/N" per factor level, model totals
    # on continuous rows (binomial outcomes only; STROBE item 16)
    "n_events",
    # Average marginal effects (AME); ame_df is the df of their t tests
    .AME_COLUMN_TOKENS,
    # Survival estimands (coxph): RMST difference over [0, tau] and
    # cumulative-incidence difference at `at_time`, by g-computation
    "rmst",
    "rmst_se",
    "rmst_ci",
    "rmst_p",
    "risk_diff",
    "risk_diff_se",
    "risk_diff_ci",
    "risk_diff_p",
    # Model-level variance explained, as a per-fit COLUMN (lm only).
    # Populated by univariable-screening frames -- each predictor block
    # reports its OWN model's R^2 / adjusted R^2, on the same first-row
    # convention as `n`. A single multivariable model reports them as
    # fit-statistics ROWS instead (`show_fit_stats`), where the same two
    # token names live.
    "r2",
    "adj_r2",
    # Term-level partial effect sizes, spliced from their single
    # producer (.PARTIAL_ES_TOKENS, R/regression_partial.R -- sourced
    # before this file, alphabetically, so the constant exists here):
    # the variance-explained partials for lm, split into estimate-only
    # + CI-only to match the b / ci asymmetry-free convention, then the
    # term-level tests, the LRT-based partial chi-square of glm and the
    # partial F of lm and of a glm whose dispersion is estimated, kept
    # BUNDLED as "value (df)" because that is the standard
    # statistical-reporting convention (e.g. "chi2(2) = 5.34").
    .PARTIAL_ES_TOKENS,
    # Probability of direction (Bayesian fits only).
    "pd",
    # Per-parameter sampler diagnostics (Bayesian fits only), and the
    # Monte Carlo standard error of the displayed posterior median.
    "rhat",
    "ess_bulk",
    "ess_tail",
    "mcse"
  ),
  show_fit_stats = c(
    "nobs",
    "weighted_nobs",
    # Variance-explained (lm only)
    "r2",
    "adj_r2",
    "omega2",
    # Pseudo-R\u00B2 family (glm only)
    "pseudo_r2_mcfadden",
    "pseudo_r2_nagelkerke",
    "pseudo_r2_tjur",
    # Pseudo-R\u00B2 of a beta regression (betareg only; Ferrari &
    # Cribari-Neto 2004): summary()'s pseudo.r.squared.
    "pseudo_r2_ferrari",
    # Deviance R\u00B2 of a glm, 1 - D / D0 (Cameron & Windmeijer 1997):
    # every family but the binomial.
    "r2_deviance",
    # NB2 negative-binomial dispersion (glm.nb, glmer.nb, fixest
    # fenegbin, mgcv nb(), pscl negbin, glmmTMB nbinom2): theta
    # (V = mu + mu^2/theta) and alpha = 1/theta.
    "theta",
    "alpha",
    # NB1 negative-binomial dispersion (glmmTMB nbinom1): delta, with
    # V = mu(1 + delta). glmmTMB's help calls it alpha after Hardin &
    # Hilbe, but "alpha" is already NB2's 1/theta here, so it takes the
    # name Stata gives it (`nbreg, dispersion(constant)`).
    "delta",
    # Beta precision phi: of beta regression (betareg, mgcv betar(),
    # glmmTMB beta; Ferrari & Cribari-Neto 2004, Var(y) =
    # mu(1-mu)/(1+phi)), and of the beta part of glmmTMB's betabinomial
    # and ordbeta.
    "phi",
    # GEE (geepack::geeglm) only: quasi-likelihood information
    # criteria (Pan 2001), the estimated scale (dispersion)
    # parameter, and the largest cluster in the estimation sample.
    "qic",
    "qicu",
    "scale",
    "max_cluster_size",
    # fixest only: the absorbed-fixed-effects Yes/No disclosure block
    # (one row per factor) and the within (FE-partialled) R-squared.
    "fixed_effects",
    "within_r2",
    # Bayesian fits only: posterior-median Bayesian R^2 (Gelman et
    # al. 2019), and PSIS-LOO elpd / LOOIC (Vehtari et al. 2017).
    "r2_bayes",
    "elpd_loo",
    "looic",
    "waic",
    # Mixed-effects R\u00B2 (Nakagawa & Schielzeth 2013; Nakagawa,
    # Johnson & Schielzeth 2017). marginal = variance explained by
    # fixed effects alone; conditional = variance explained by
    # fixed + random.
    "r2_marginal",
    "r2_conditional",
    # Mixed-effects group structure: N per grouping factor + intraclass
    # correlation, as fit-stat rows (sjPlot / modelsummary convention).
    "n_events",
    "n_groups",
    "icc",
    "sigma",
    "rmse",
    "f2",
    # Lowercase like every other token (the 0.13 rename; renders as
    # "AIC" / "AICc" / "BIC" row labels unchanged) -- coherent with
    # the aic_change / aicc_change / bic_change tokens below.
    "aic",
    "aicc",
    "bic",
    "deviance",
    # The log-likelihood the AIC row is computed from (stats::logLik()).
    "log_lik",
    # Harrell's concordance of a Cox model (coxph, svycoxph).
    "concordance",
    # survey::svyglm only: the effective number of parameters of the
    # design (the first element of survey's extractAIC), beside the
    # design-based AIC of Lumley & Scott rather than instead of it.
    "eff_p",
    # Nested-comparison change stats (APA Table 7.15 in-table rows).
    # `nested = TRUE` auto-injects a class-aware subset; the user
    # can request any of these explicitly in `show_fit_stats`.
    "r2_change",
    "adj_r2_change",
    "f_change",
    "f2_change",
    "lrt_change",
    "aic_change",
    "aicc_change",
    "bic_change",
    "deviance_change",
    "p_change"
  )
)

# Predefined group tokens for `show_columns`. Each group expands
# to a fixed vector of atomic tokens. Inspired by Stata / SPSS /
# SAS keyword presets \u2013 lets the user write
# `show_columns = c("all_b", "all_ame")` instead of enumerating 8
# atomic tokens. The expansion runs once, before validation, so
# downstream code only ever sees atomic tokens.
.show_columns_groups <- list(
  all_b = c("b", "se", "ci", "p"),
  all_b_compact = c("b", "se", "p"),
  all_b_full = c("b", "se", "ci", "t", "p"),
  all_beta = c("b", "beta", "se", "ci", "p"),
  all_ame = c("ame", "ame_se", "ame_ci", "ame_p"),
  all_ame_compact = c("ame", "ame_p"),
  all_f2 = c("partial_f2", "partial_f2_ci"),
  all_eta2 = c("partial_eta2", "partial_eta2_ci"),
  all_omega2 = c("partial_omega2", "partial_omega2_ci")
)

# Migration map for the spicy 0.11.x -> 0.12 token rename. When a
# user passes an old-style uppercase token we throw an actionable
# error pointing at the replacement instead of silently aliasing
# (per the pre-1.0 "hard errors over silent changes" policy).
.show_columns_legacy <- list(
  B = c("b"),
  SE = c("se"),
  CI = c("ci"),
  AME = c("ame", "ame_ci"), # old AME bundled estimate + CI
  AME_p = c("ame_p"),
  AME_SE = c("ame_se")
)


# ---- Phase A \u2013 input class ------------------------------------------------

# Steps 1-3: validate `models` is lm OR list of lm; aggregate-fail on
# any non-lm position.
#
# Detection of "single fit vs list of fits" is non-trivial because R's
# lm objects ARE lists internally (with $coefficients, $residuals,
# etc.). The trick: a *plain* list has class identical to `"list"`;
# a model fit has model classes (lm, glm, merMod, ...) layered on top.
# Canonical vcov vocabulary (Q7), single producer: consumed by the
# upfront validation below and by the survey-design guard in
# compute_model_vcov() (which must fire only on KNOWN estimators so an
# unknown token keeps its "Unknown `vcov` type" answer).
.VCOV_MODES <- c(
  "classical",
  paste0("HC", 0:5),
  paste0("CR", 0:3),
  # Stata `regress, vce(cluster)` convention: CR1S scaling with
  # t(G - 1) inference. lm only (Stata's ML commands use a
  # DIFFERENT convention -- G/(G-1) scaling with z -- so claiming
  # the Stata name for glm would be a misattribution).
  "CR1S",
  "bootstrap",
  "jackknife",
  # quantreg::rq estimator family (refused for every
  # other class by the Step-6c capability gate).
  "nid",
  "iid",
  "ker",
  "rank"
)

# The vocabulary of the COMPUTE layer, which serves two entry points with
# two argument vocabularies: table_regression() (.VCOV_MODES above) and
# table_continuous_lm(), whose formal additionally admits "HC4m" (the
# modified-exponent HC4 of Cribari-Neto, Souza & Vasconcellos 2007).
# compute_model_vcov() asserts THIS set before dispatching, so no token
# outside it can reach sandwich / clubSandwich and be answered in their
# words instead of spicy's; the per-entry-point vocabularies stay
# narrower, and each keeps refusing what it does not offer.
.VCOV_COMPUTE_MODES <- unique(c(.VCOV_MODES, "HC4m"))

validate_models_input <- function(models, call = rlang::caller_env()) {
  # Step 1: handle NULL upfront (R's `inherits` and `is.list` both
  # return FALSE on NULL, so it would otherwise slip through)
  if (is.null(models)) {
    spicy_abort(
      c(
        "`models` cannot be NULL.",
        "i" = "Pass an `lm()` or `glm()` fit, or a list of such fits."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  # Step 1 (cont.): promote single object to 1-element list. A *plain*
  # list (one whose class is exactly "list") is treated as already-a-
  # list-of-fits; anything else (lm with extra classes, glm, vector,
  # data.frame, formula, ...) becomes the single element of a 1-list,
  # to be class-checked per element below.
  is_plain_list <- is.list(models) && identical(class(models), "list")
  if (!is_plain_list) {
    models <- list(models)
  }

  if (length(models) == 0L) {
    spicy_abort(
      c(
        "`models` is an empty list.",
        "i" = "Pass at least one `lm()` or `glm()` fit."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  # An NA name is an unnamed slot. nzchar(NA) is TRUE, so every
  # downstream `all(nzchar(names(models)))` predicate would otherwise
  # accept it -- as a display label, and worse, as a model id.
  if (!is.null(names(models))) {
    names(models)[is.na(names(models))] <- ""
  }

  # Step 1c: when the list is named, names must be unique. Duplicate
  # names would silently collide in the long-format model_id key,
  # causing one fit to overwrite the other in extract / align /
  # render \u2013 a silent data loss. Caught upfront with an actionable
  # message.
  #
  # Partial naming (some elements named, others not) is accepted
  # and auto-filled downstream: `list("Step 1" = m1, m2)` becomes
  # `list("Step 1" = m1, "Model 2" = m2)` for label purposes. The
  # rationale: a partially-named list is unambiguous about user
  # intent, and forcing all-or-nothing forced the user to either
  # repeat work or drop a meaningful name they already typed.
  nms <- names(models)
  if (!is.null(nms)) {
    if (any(nzchar(nms))) {
      dupes <- unique(nms[duplicated(nms) & nzchar(nms)])
      if (length(dupes) > 0L) {
        spicy_abort(
          c(
            sprintf(
              "Duplicate name(s) in `models` list: %s.",
              paste(.quote_val(dupes), collapse = ", ")
            ),
            "i" = paste0(
              "Names of the models list are used as model IDs and ",
              "must be unique. Drop the names (positional list) or ",
              "rename the duplicates."
            )
          ),
          class = "spicy_invalid_input",
          call = call
        )
      }
    }
  }

  # Steps 2-3: per-element class check, aggregate-fail
  problems <- vapply(
    seq_along(models),
    function(i) {
      msg <- classify_unsupported_lm_class(models[[i]], position = i)
      if (is.null(msg)) "" else msg
    },
    character(1)
  )
  bad <- problems[nzchar(problems)]
  if (length(bad) > 0L) {
    spicy_abort(
      c(
        "Some `models` are not supported by `table_regression()`.",
        bad,
        "i" = paste0(
          "Supported model classes: ",
          paste(.supported_models_registry()$class, collapse = ", "),
          ". `table_regression_models()` lists them with their engines."
        )
      ),
      class = "spicy_unsupported",
      call = call
    )
  }

  models
}

# The scope refusals of the model frames, run by table_regression() on
# every model before it resolves any argument against them: an ordinal
# or categorical Bayesian fit (.stan_refuse_ordinal(),
# .brms_refuse_ordinal()) and a GAM with several linear predictors
# (.gam_refuse_multi_formula()). Run later, the refusal of an ordinal
# stan_polr fit was hidden by the Bayesian fit-statistics gate, which
# refused the ordinal defaults the user never asked for. Each frame
# method runs its own as well, so as_regression_frame() refuses alike.
.refuse_out_of_scope_model <- function(fit) {
  if (inherits(fit, "stanreg")) {
    .stan_refuse_ordinal(fit)
  }
  if (inherits(fit, "brmsfit")) {
    .brms_refuse_ordinal(fit)
  }
  if (inherits(fit, "gam")) {
    .gam_refuse_multi_formula(fit)
  }
  .refuse_inherited_subclass(fit)
  invisible(NULL)
}

# Objects that reach a frame method by inheriting from a supported class
# without being that model. Measured on 2026-11-19
# (dev/decisions/2026-11-19-inherited-subclasses.md): each one was either
# rendered under the parent's title, rows and note, or failed with an
# error from deep inside the parent's code. Each is refused by name, with
# one sentence on what it is. Run by table_regression() before any
# argument is resolved, and by the frame methods of the parent classes.
.refuse_inherited_subclass <- function(fit) {
  what <- .inherited_subclass_reason(fit)
  if (is.null(what)) {
    return(invisible(NULL))
  }
  spicy_abort(
    c(
      if (is.na(what[["head"]])) {
        sprintf(
          "`%s` fits are not supported by `table_regression()`.",
          what[["class"]]
        )
      } else {
        what[["head"]]
      },
      "i" = what[["is"]],
      if (!is.na(what[["do"]])) c("i" = what[["do"]])
    ),
    class = "spicy_unsupported_class"
  )
}

# NULL for a fit that is the model its frame method describes; otherwise
# the class to name, what the object is and, when there is one, the
# supported way to fit it.
.inherited_subclass_reason <- function(fit) {
  # `head` replaces the first line when the class alone would say too
  # much: only some `coxph.penal` fits are refused.
  out <- function(class, is, do = NA_character_, head = NA_character_) {
    c(class = class, is = is, do = do, head = head)
  }
  if (inherits(fit, "clogit")) {
    return(out(
      "clogit",
      paste0(
        "A `survival::clogit()` fit is a conditional logistic regression, ",
        "estimated as a stratified Cox model: the Cox title, event count ",
        "and concordance would describe the wrong model."
      )
    ))
  }
  if (inherits(fit, "coxphms")) {
    return(out(
      "coxphms",
      paste0(
        "A multi-state `coxph` fit has one coefficient per transition and ",
        "no single survival curve."
      )
    ))
  }
  if (inherits(fit, "coxph.penal")) {
    pt <- fit$pterms %||% numeric(0)
    # A sparse frailty (pterms 2) and a ridge() term leave one coefficient
    # per covariate, the rows of summary(). A pspline() term or a
    # non-sparse frailty puts its basis or per-cluster coefficients in
    # coef(), where summary() gives one test per term.
    expanded <- pt == 1 & !grepl("^ridge\\(", names(pt))
    if (any(expanded)) {
      term <- names(pt)[expanded][1L]
      return(out(
        "coxph.penal",
        sprintf(
          paste0(
            "Its coefficients are one per %s, where `summary()` reports ",
            "one test for the term."
          ),
          if (grepl("^frailty", term)) "cluster" else "spline basis"
        ),
        head = sprintf(
          "A `coxph` fit with the term `%s` is not supported by `table_regression()`.",
          term
        )
      ))
    }
  }
  # A tobit-5 fit has two outcome equations (`betaO1`, `betaO2`); the
  # selection table reads one (`betaO`) and printed 2 of its 10 rows.
  if (
    inherits(fit, "selection") &&
      identical(as.character(fit$tobitType), "5")
  ) {
    return(out(
      "selection",
      paste0(
        "A tobit-5 (switching regression) fit has two outcome equations, ",
        "one per regime; the table shows the sample-selection (tobit-2) ",
        "model and the treatment-effects model of `treatReg()`."
      ),
      head = paste0(
        "A tobit-5 `selection` fit is not supported by ",
        "`table_regression()`."
      )
    ))
  }
  if (inherits(fit, "svysurvreg")) {
    return(out(
      "svysurvreg",
      paste0(
        "A `survey::svysurvreg()` fit is a survey-weighted parametric ",
        "survival model: the `survreg` table would give neither its ",
        "design nor the design-based nature of its standard errors."
      )
    ))
  }
  if (inherits(fit, "mlm")) {
    return(out(
      "mlm",
      paste0(
        "An `lm()` fit with a matrix response is a multivariate linear ",
        "model, with one set of coefficients per response."
      ),
      "Fit one `lm()` per response."
    ))
  }
  if (inherits(fit, "aov")) {
    return(out(
      "aov",
      paste0(
        "An `aov()` fit is an analysis of variance: its `summary()` is an ",
        "ANOVA table, not a table of coefficients."
      ),
      "Fit the same model with `lm()` for a table of coefficients."
    ))
  }
  if (inherits(fit, "nlme")) {
    return(out(
      "nlme",
      paste0(
        "An `nlme::nlme()` fit is a nonlinear mixed-effects model, not the ",
        "linear one the `lme` table describes."
      )
    ))
  }
  if (inherits(fit, "lme") && .is_gamm_lme(fit)) {
    return(out(
      "lme",
      paste0(
        "Its random-effect rows are the smooth terms of the GAM, written as ",
        "random effects, and its coefficients are those of the spline basis."
      ),
      paste0(
        "Fit the model with `mgcv::gam()`, a random intercept written as ",
        "`s(g, bs = \"re\")`, which is supported."
      ),
      head = paste0(
        "The `lme` part of a `mgcv::gamm()` fit is not supported by ",
        "`table_regression()`."
      )
    ))
  }
  # The `gam` part of a gamm() (or gamm4()) fit is a "gam" object that no
  # longer inherits from "glm": its standard errors come from the `lme`
  # part, and its likelihood is not that of the model.
  if (inherits(fit, "gam") && !inherits(fit, "glm")) {
    return(out(
      "gam",
      paste0(
        "Its standard errors come from the mixed model of the `lme` part, ",
        "and its likelihood and information criteria are not those of the ",
        "model."
      ),
      paste0(
        "Fit the model with `mgcv::gam()`, a random intercept written as ",
        "`s(g, bs = \"re\")`, which is supported, or report ",
        "`summary(fit$gam)`."
      ),
      head = paste0(
        "The `gam` part of a `mgcv::gamm()` fit is not supported by ",
        "`table_regression()`."
      )
    ))
  }
  if (inherits(fit, "gnls")) {
    return(out(
      "gnls",
      paste0(
        "An `nlme::gnls()` fit is a nonlinear model fitted by generalized ",
        "least squares, not the linear one the `gls` table describes."
      )
    ))
  }
  if (inherits(fit, "Rq")) {
    return(out(
      "Rq",
      "An `rms::Rq()` fit is a quantile regression wrapped by rms.",
      "Fit the model with `quantreg::rq()`, which is supported."
    ))
  }
  if (inherits(fit, "Gls")) {
    return(out(
      "Gls",
      "An `rms::Gls()` fit is a generalized least squares model wrapped by rms.",
      "Fit the model with `nlme::gls()`, which is supported."
    ))
  }
  NULL
}

# The `lme` part of a mgcv::gamm() fit is a plain "lme" object. Two marks
# set by mgcv alone (mgcv 1.9-5, gamm() and gammPQL()): a random-effect
# block of class pdIdnot or pdTens, mgcv's own pdMat classes, which carry
# its smooth terms; and, for a Gaussian gamm, the call gamm() builds,
# `data = strip.offset(mf)`, which also marks a gamm without smooths.
.is_gamm_lme <- function(fit) {
  re <- fit$modelStruct$reStruct
  smooth_block <- any(vapply(
    re,
    function(b) inherits(b, c("pdIdnot", "pdTens")),
    logical(1)
  ))
  smooth_block || identical(fit$call$data, quote(strip.offset(mf)))
}

# Per-fit class classifier: NULL if OK, message string otherwise.
# Used by validate_models_input() to build the aggregate-fail message.
#
# A class is "supported" if either (a) it is `lm` or `glm` (the
# historical core) or (b) an `as_regression_frame` S3 method is
# registered for one of its classes. The method registry expands as
# new model classes ship; new methods become user-visible automatically
# without touching this gate.
classify_unsupported_lm_class <- function(fit, position = NULL) {
  pos_prefix <- if (!is.null(position)) {
    sprintf("Position %d: ", position)
  } else {
    ""
  }

  # Common user mistake: passing raw data instead of a fit (Q10).
  if (is.data.frame(fit)) {
    return(paste0(
      pos_prefix,
      "data.frame supplied where a model fit is expected. ",
      "Fit a model first: ",
      "`fit <- lm(y ~ x, data = your_data); table_regression(fit)`."
    ))
  }
  if (is.null(fit)) {
    return(paste0(
      pos_prefix,
      "NULL element \u2013 the list of models contains a NULL slot. ",
      "Drop the NULL or replace it with a model fit."
    ))
  }
  # Accept any class that has a registered as_regression_frame method.
  if (.has_as_regression_frame_method(fit)) {
    return(NULL)
  }
  paste0(
    pos_prefix,
    sprintf(
      "`%s` \u2013 not a supported model class. ",
      class(fit)[1L]
    ),
    "If support would be useful, please open an issue: ",
    "https://github.com/amaltawfik/spicy/issues"
  )
}


# Return TRUE if any class of `fit` has an `as_regression_frame` S3
# method registered in this package. Walks class(fit) in dispatch
# order; the default method is excluded so unregistered classes still
# error.
.has_as_regression_frame_method <- function(fit) {
  cls <- class(fit)
  for (cl in cls) {
    if (
      !is.null(utils::getS3method("as_regression_frame", cl, optional = TRUE))
    ) {
      return(TRUE)
    }
  }
  FALSE
}


# ---- Phase B \u2013 multi-model alignment --------------------------------------

# Steps 4-5: nested = TRUE requires identical nobs and identical DV
validate_nested_alignment <- function(
  models,
  nested,
  call = rlang::caller_env()
) {
  if (!isTRUE(nested)) {
    return(invisible(NULL))
  }
  # Nested = TRUE with a single fit is a no-op (nothing to compare).
  # Warn rather than silently render a regular table -- the user almost
  # certainly meant to pass a list of nested fits and the silent no-op
  # would mask the mistake.
  if (length(models) <= 1L) {
    spicy_warn(
      c(
        paste0(
          "`nested = TRUE` requires a list of at least 2 models; ",
          "the single fit was rendered without a comparison footer."
        ),
        "i" = paste0(
          "Pass `list(m1, m2, ...)` of nested models with identical ",
          "DV and identical n to enable the hierarchical-comparison ",
          "footer."
        )
      ),
      class = "spicy_ignored_arg"
    )
    return(invisible(NULL))
  }

  # GEE fits have no likelihood, so none of the change statistics
  # (Delta R-squared, partial F, LRT, Delta AIC) is defined; the glm
  # pair path they would inherit reads logLik / anova(test = "LRT"),
  # which geeglm does not provide. Refuse with the QIC alternative.
  if (any(vapply(models, inherits, logical(1), "geeglm"))) {
    spicy_abort(
      c(
        "`nested = TRUE` is not available for GEE fits (`geeglm`).",
        "i" = paste0(
          "GEE is estimated by quasi-likelihood: there is no ",
          "likelihood-ratio change test."
        ),
        "i" = paste0(
          "Compare working models with QIC ",
          "(`show_fit_stats = c(\"nobs\", \"qic\")`), or use ",
          "`geepack::anova.geeglm()` Wald tests outside the table."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  # Quantile regression pairs compare through anova.rq()'s Wald-type F
  # (compute_one_pair_rq). Two guards keep the comparison meaningful:
  # rq cannot mix with other classes (different loss functions -- the
  # lm / glm pair paths would read R-squared / logLik that rq does not
  # define), and all fits must share ONE tau (anova.rq across taus is
  # a joint equality-of-slopes analysis, not a nested comparison).
  is_rq <- vapply(models, inherits, logical(1), "rq")
  if (any(is_rq)) {
    if (!all(is_rq)) {
      spicy_abort(
        c(
          paste0(
            "`nested = TRUE` cannot mix quantile regression with ",
            "other model classes."
          ),
          "i" = paste0(
            "The check-loss objective has no R\u00B2 or ",
            "likelihood shared with the other fits; compare ",
            "rq models with rq models."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    taus <- vapply(models, function(m) as.numeric(m$tau %||% 0.5), numeric(1))
    if (length(unique(taus)) > 1L) {
      spicy_abort(
        c(
          "`nested = TRUE` needs all rq fits at the SAME tau.",
          "i" = paste0(
            "Comparing quantiles is a different analysis ",
            "(joint equality of slopes across taus); see ",
            "`quantreg::anova.rq()` on an rq fit with ",
            "multiple taus."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # Sample-selection models compare by their likelihoods, with one
  # another only, and a two-step fit has none (logLik() refuses it).
  is_sel <- vapply(models, inherits, logical(1), "selection")
  if (any(is_sel)) {
    two_step <- vapply(
      models[is_sel],
      function(m) !identical(m$method %||% "ml", "ml"),
      logical(1)
    )
    if (any(two_step) || !all(is_sel)) {
      spicy_abort(
        c(
          if (any(two_step)) {
            paste0(
              "`nested = TRUE` is not available for a two-step selection ",
              "model (`heckit()`, `method = \"2step\"`)."
            )
          } else {
            paste0(
              "`nested = TRUE` cannot mix a selection model with other ",
              "model classes."
            )
          },
          "i" = paste0(
            "The likelihood-ratio test compares two selection models ",
            "estimated by maximum likelihood (`selection()`, the default ",
            "`method = \"ml\"`); a two-step fit has no likelihood."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # Step 4: nobs identical. numeric(1), not integer(1): nobs() returns a
  # double for some supported classes (e.g. coxph), which would make an
  # integer(1) vapply throw under `nested = TRUE`. .spicy_nobs() covers
  # classes without a registered nobs method (nnet::multinom).
  # A Cox model is also compared by its events: two fits with as many
  # observations can hold different ones.
  nobs_vec <- vapply(models, .spicy_nobs, numeric(1))
  nev_vec <- vapply(models, .spicy_nevent, numeric(1))
  nev_cox <- nev_vec[!is.na(nev_vec)]
  differ <- c(
    if (length(unique(nobs_vec)) > 1L) {
      sprintf(
        "Models have different `nobs` (observations): %s.",
        paste(nobs_vec, collapse = ", ")
      )
    },
    if (length(unique(nev_cox)) > 1L) {
      sprintf(
        "Cox models have different numbers of events: %s.",
        paste(nev_cox, collapse = ", ")
      )
    }
  )
  if (length(differ) > 0L) {
    spicy_abort(
      c(
        differ,
        "i" = paste0(
          "Hierarchical comparison statistics (\u0394R\u00B2, partial F, ",
          "\u0394AIC, LRT, etc.) require identical observations across ",
          "all models. A difference typically results from R's ",
          "listwise deletion on different rows for each model."
        ),
        "i" = paste0(
          "Refit every model on the rows they all use: drop the rows ",
          "with a missing value on any variable of the largest model ",
          "(`tidyr::drop_na(data, <its variables>)`), then fit each ",
          "model on that data."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  # Step 5: identical DV (response side of the formula)
  dvs <- vapply(
    models,
    function(fit) {
      # A selection fit has no formula() method; the response of its
      # outcome equation is in its outcome terms.
      if (inherits(fit, "selection")) {
        return(deparse1(fit$termsO[[2L]]))
      }
      # The response as the title reads it: a fixest IV formula is
      # `y ~ x | fe | endo ~ inst`, whose second element is not `y`.
      .fit_dv(fit)
    },
    character(1)
  )
  if (length(unique(dvs)) > 1L) {
    spicy_abort(
      c(
        sprintf(
          "Models have different response variables: %s.",
          paste(unique(dvs), collapse = ", ")
        ),
        "i" = paste0(
          "Hierarchical comparison requires the same DV across all ",
          "models. For side-by-side multi-DV display, use ",
          "`nested = FALSE`."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  invisible(NULL)
}

# Steps 6-8: vcov + cluster list/scalar coordination
#
# ---- Where a capability may be read from, and where it may not -------
#
# Decision 41, acted here once for the whole validation layer. This
# function -- and every validator table_regression() calls before its
# frame loop: validate_nested_alignment(), validate_show_columns(),
# validate_class_appropriate_tokens() -- sniffs the FIT with
# inherits(). That is legitimate, and it is not a gap left by the frame
# refactor: these checks exist precisely so that a request a class
# cannot honor dies before any fitting-adjacent work starts (C2
# increment 1, fail fast). A frame flag cannot govern a check that runs
# BEFORE the frame exists, so asking these sites to read one would mean
# building the frames first and losing the fail-fast they were written
# for.
#
# The rule the two layers split on:
#
#   * PRE-frame  -- inherits() on the fit. Refuses the impossible
#                   before any frame is built. Keeps its class-specific
#                   message (the GEE / QIC hint, the rq tau guard, the
#                   lm-vs-glm partial substitutions).
#   * POST-frame -- `info$supports$*` on the built frame, and nothing
#                   else. The AME guard, the partial-effect-size guard
#                   and the nested-comparison guard in
#                   table_regression() all read a declaration; a
#                   builder that mis-declares changes what the table
#                   does, which is what makes the declaration worth
#                   trusting.
#
# So `supports` is the truth about what a frame CAN render, never a
# substitute for the earlier refusal, and nothing below should be
# rewritten to consult it.
validate_vcov_cluster_lists <- function(
  vcov,
  cluster,
  models,
  call = rlang::caller_env()
) {
  validate_vcov_arg(vcov, models, call = call)
  validate_cluster_lists(vcov, cluster, models, call = call)
}

# Steps 6-6c: `vcov` alone, its form and each model's capability. Run
# before `cluster` is resolved too, so that a class that refuses the
# estimator (a design fit, a posterior, selection) says why before a
# cluster variable it would not use is looked up.
validate_vcov_arg <- function(vcov, models, call = rlang::caller_env()) {
  n_models <- length(models)

  # Forgot `list()`? `table_regression(m1, m2)` binds the second model
  # to `vcov`; lm-like objects ARE lists, so the generic length check
  # below would emit a baffling "`vcov` is a list of length 12" error.
  # Catch any classed non-character object here and name the likely
  # cause.
  if (is.object(vcov) && !is.character(vcov)) {
    hint <- if (!is.atomic(vcov)) {
      c(
        "i" = paste0(
          "A model object here usually means a forgotten `list()`: ",
          "`table_regression(m1, m2)` binds the second model to `vcov`."
        ),
        "i" = "Wrap the models instead: `table_regression(list(m1, m2))`."
      )
    } else {
      c(
        "i" = paste0(
          "Pass a single string (\"classical\", \"HC3\", ...) or a ",
          "list of strings."
        )
      )
    }
    spicy_abort(
      c(
        sprintf(
          "`vcov` must be a string or a list of strings, not an object of class `%s`.",
          class(vcov)[1]
        ),
        hint
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  # Canonical vcov vocabulary (Q7): the shared constant. Validated
  # upfront so an unknown type is caught with a clear error instead of
  # letting `sandwich` / `clubSandwich` warn-and-fallback at compute
  # time.
  valid_vcov <- .VCOV_MODES

  # Step 6: vcov list length + element type
  if (is.list(vcov)) {
    if (length(vcov) != n_models) {
      spicy_abort(
        c(
          sprintf(
            "`vcov` is a list of length %d but `models` has length %d.",
            length(vcov),
            n_models
          ),
          "i" = paste0(
            "Pass a single string (recycled to all models) or a ",
            "list of length(models)."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    if (
      !all(vapply(
        vcov,
        function(v) is.character(v) && length(v) == 1L && !is.na(v),
        logical(1)
      ))
    ) {
      spicy_abort(
        "Each element of `vcov` (when a list) must be a single string.",
        class = "spicy_invalid_input",
        call = call
      )
    }
  } else {
    if (!is.character(vcov) || length(vcov) != 1L || is.na(vcov)) {
      spicy_abort(
        c(
          "`vcov` must be a single string or a list of strings.",
          "i" = paste0(
            "Valid scalars: \"classical\", \"HC0\"\u2013\"HC5\", ",
            "\"CR0\"\u2013\"CR3\", \"bootstrap\", \"jackknife\", plus ",
            "class-specific estimators (quantile regression: ",
            "\"nid\" / \"iid\" / \"ker\" / \"rank\")."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # Step 6b: vocabulary check (each element must be a known type).
  # The CHECK is class-independent -- an unknown token is unknown to
  # every class -- but the hint is not: listing the whole vocabulary
  # offered "nid" / "iid" / "ker" / "rank" to an lm user (rq-only, and
  # refused one step later by 6c) and "HC0"-"HC5" / "CR0"-"CR3" /
  # "jackknife" to an rq user (refused there for statistical reasons).
  # A typo should be answered with the tokens THIS call can actually
  # use, so the hint is the union of the models' own capabilities,
  # falling back to the full vocabulary if none can be determined.
  vcov_check <- if (is.list(vcov)) unlist(vcov) else vcov
  bad <- setdiff(vcov_check, valid_vcov)
  if (length(bad) > 0L) {
    cited <- lapply(models, function(m) {
      tryCatch(.vcov_cited(m), error = function(e) list())
    })
    usable <- intersect(valid_vcov, unlist(lapply(cited, `[[`, "values")))
    # One model: the values of its class it cannot take, with the
    # condition the registry gives them.
    others <- if (length(models) == 1L) cited[[1L]]$others
    spicy_abort(
      c(
        sprintf(
          "Unknown `vcov` type(s): %s.",
          paste(.quote_val(bad), collapse = ", ")
        ),
        "i" = sprintf(
          "Valid types for %s: %s.%s",
          if (length(models) == 1L) {
            sprintf("`%s`", class(models[[1L]])[1L])
          } else {
            "these models"
          },
          paste(
            .quote_val(if (length(usable) > 0L) usable else valid_vcov),
            collapse = ", "
          ),
          if (is.null(others)) {
            ""
          } else {
            sprintf(" Its class also supports %s.", others)
          }
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  # Step 6c: per-class capability. A robust vcov must be one the model's class
  # can actually compute, else fail fast (spicy_unsupported_vcov) instead of
  # silently returning model-based SEs under a robust label (audit finding C2).
  # "classical" is supported by every class, so default calls never trip this.
  vcov_per_model <- if (is.list(vcov)) vcov else rep(list(vcov), n_models)
  for (i in seq_len(n_models)) {
    vt <- vcov_per_model[[i]]
    supported <- .robust_vcov_support(models[[i]])
    if (!vt %in% supported) {
      # Bayesian fits are refused on principle, not because support is
      # pending: a posterior has no sandwich analog. The generic list
      # of supported estimators below would not say so.
      if (inherits(models[[i]], c("stanreg", "brmsfit"))) {
        spicy_abort(
          c(
            sprintf(
              "`vcov = \"%s\"` is not defined for Bayesian fits (`%s`).",
              vt,
              class(models[[i]])[1L]
            ),
            "i" = paste0(
              "A posterior has no sandwich analog: the ",
              "displayed uncertainty comes from the posterior ",
              "draws, not from an estimating equation."
            ),
            "i" = paste0(
              "Model the clustering or heteroskedasticity ",
              "instead (rstanarm::stan_glmer(), brms ",
              "group-level or distributional terms)."
            )
          ),
          class = "spicy_unsupported_vcov",
          call = call
        )
      }
      # GEE fits are refused on principle too: the sandwich IS the
      # fit's own default inference, so spicy's HC* / CR* tokens have
      # nothing to add -- the estimator choice lives on the fit
      # (geeglm's `std.err =`), not in the table call.
      if (inherits(models[[i]], "geeglm")) {
        .gee_refuse_vcov(vt, call = call)
      }
      # Survey fits are refused on the same principle: the design-based
      # Taylor / replicate variance IS the robust variance, and clustering
      # belongs in the design (clubSandwich has no vcovCR.svyglm; the call
      # would silently dispatch to vcovCR.glm, ignoring strata, FPC and
      # calibration).
      # `.is_design_fit()`, not `inherits(fit, "svyglm")`: svyolr and
      # svycoxph are refused on exactly the same principle and used to
      # fall through to the generic "This class supports: classical."
      # The class named is the fit's own, so a `svyglm` request keeps
      # the message it has always had.
      # Before the cluster branches below: their advice (CR2, CR1) does
      # not apply to a design fit.
      if (.is_design_fit(models[[i]])) {
        spicy_abort(
          c(
            sprintf(
              "`vcov = \"%s\"` is not available for `%s` models.",
              vt,
              class(models[[i]])[1L]
            ),
            "i" = paste0(
              "The fit's own design-based (Taylor / replicate) ",
              "variance is already the robust variance for the ",
              "declared design."
            ),
            "i" = paste0(
              "To account for clustering, declare it in the design: ",
              "survey::svydesign(ids = ~cluster, ...), then refit."
            )
          ),
          class = "spicy_unsupported_vcov",
          call = call
        )
      }
      # A class that computes one cluster estimator (CR1 for the
      # sandwich::vcovCL() classes, CR0 for coxph and the rms fits) refuses
      # any other CR* token with that reason, before the glm "CR1S" branch
      # below, which a gam or an rms Glm (they inherit "glm") would reach
      # with advice that does not apply to them.
      one_token <- .single_cluster_token(models[[i]])
      if (
        startsWith(vt, "CR") &&
          !is.null(one_token) &&
          one_token %in% supported
      ) {
        .single_cluster_refuse_vcov(models[[i]], vt, call = call)
      }
      # glm + "CR1S": refuse with the reason. Stata's ML commands
      # (logit, poisson, ...) use a DIFFERENT vce(cluster) convention
      # -- G/(G-1) scaling with z inference -- so a glm "CR1S" would
      # print numbers under a Stata label that Stata itself does not
      # produce.
      if (identical(vt, "CR1S") && inherits(models[[i]], "glm")) {
        spicy_abort(
          c(
            "`vcov = \"CR1S\"` is available for `lm` fits only.",
            "i" = paste0(
              "\"CR1S\" reproduces Stata's `regress, vce(cluster)` ",
              "(CR1S scaling, t(G-1)). Stata's ML commands use a ",
              "different convention (G/(G-1) scaling, z), so the ",
              "label would not match Stata output for a glm."
            ),
            "i" = paste0(
              "Use `vcov = \"CR2\"` (Bell-McCaffrey with ",
              "Satterthwaite df, the recommended default) or ",
              "\"CR0\"-\"CR3\" with `cluster`."
            )
          ),
          class = "spicy_unsupported_vcov",
          call = call
        )
      }
      # clm with a scale / nominal component: the restriction is
      # STRUCTURAL, not class-wide. The generic message below would
      # read "clm supports: classical", wrongly implying every clm is
      # classical-only when a plain proportional-odds clm takes CR*.
      if (
        inherits(models[[i]], "clm") &&
          .clm_has_scale_or_nominal(models[[i]])
      ) {
        spicy_abort(
          c(
            sprintf(
              paste0(
                "`vcov = \"%s\"` is not available for a `clm` fit ",
                "with a scale or nominal component."
              ),
              vt
            ),
            "i" = paste0(
              "`ordinal` provides no estimating functions for the ",
              "scale / nominal parts, so no sandwich can be built. ",
              "A plain proportional-odds `clm` supports ",
              "\"CR1\" with `cluster`."
            ),
            "i" = paste0(
              "Use the model-based default, or refit without ",
              "`scale` / `nominal`."
            )
          ),
          class = "spicy_unsupported_vcov",
          call = call
        )
      }
      # Negative-binomial pscl fits: the sandwich has no score for theta
      # (R/vcov.R). Sandwich tokens only: the resamplers are refused
      # below for a reason of their own.
      if (.pscl_is_negbin(models[[i]]) && grepl("^(HC|CR)", vt)) {
        .pscl_negbin_refuse_vcov(models[[i]], vt, call = call)
      }
      # multinom / mlogit HC*: name the reason instead of a bare
      # supported-types list -- the refusal is statistical, not a
      # not-yet-wired gap.
      if (
        inherits(models[[i]], c("multinom", "mlogit")) &&
          grepl("^HC", vt)
      ) {
        why_hc <- if (inherits(models[[i]], "multinom")) {
          paste0(
            "A multi-equation model has no working residuals or ",
            "hat values, so the HC* meat cannot be formed."
          )
        } else {
          paste0(
            "sandwich::vcovHC() computes a result for mlogit but ",
            "silently mis-scales the meat for its per-chooser ",
            "score structure."
          )
        }
        spicy_abort(
          c(
            sprintf(
              "`vcov = \"%s\"` is not available for `%s` models.",
              vt,
              class(models[[i]])[1L]
            ),
            "i" = why_hc,
            "i" = paste0(
              "Cluster-robust \"CR1\" with `cluster` is ",
              "supported for this class."
            )
          ),
          class = "spicy_unsupported_vcov",
          call = call
        )
      }
      # quantreg::rq: every refusal here is STATISTICAL, so each names
      # its own reason rather than the generic list of supported
      # estimators, which gives none. The three
      # families fail for three different reasons, and two of them have
      # an actionable route:
      #   * HC*: no working residuals and no hat values, so the meat
      #     cannot be formed. The message must not let "nid" read as an
      #     HC variant under another name -- it is a different estimator
      #     family (sparsity-based, Hendricks-Koenker), which happens to
      #     be the heteroskedasticity-robust choice for this class.
      #   * CR*: no clubSandwich / estfun backend. The cluster route
      #     available is the wild gradient bootstrap, so point at it
      #     (the Step-8 carve-out below blesses that exact pair).
      #   * jackknife: leave-one-out is inconsistent for non-smooth
      #     estimators (Efron 1982; Shao & Wu 1989) -- the quantile
      #     objective is not differentiable, so deleting one observation
      #     moves the fit discontinuously.
      if (inherits(models[[i]], "rq")) {
        why_rq <- if (grepl("^HC", vt)) {
          c(
            "i" = paste0(
              "Quantile regression has no working residuals or hat ",
              "values, so the HC* meat cannot be formed."
            ),
            "i" = paste0(
              "It carries its own sandwich family instead: \"nid\" ",
              "(the default) is the heteroskedasticity-robust ",
              "estimator here -- a sparsity-based quantile sandwich, ",
              "not an HC variant under another name. \"iid\", ",
              "\"ker\" and \"rank\" are the other estimators."
            )
          )
        } else if (grepl("^CR", vt)) {
          c(
            "i" = paste0(
              "There is no cluster-robust sandwich for quantile ",
              "regression: clubSandwich has no method for `rq`, and ",
              "no estimating functions are available to build one."
            ),
            "i" = paste0(
              "Use `vcov = \"bootstrap\"` with `cluster =`: the wild ",
              "gradient cluster bootstrap, the route available for ",
              "clustered data."
            )
          )
        } else {
          c(
            "i" = paste0(
              "The leave-one-out jackknife is inconsistent for ",
              "non-smooth estimators: the quantile objective is not ",
              "differentiable, so dropping one observation can move ",
              "the fit discontinuously."
            ),
            "i" = paste0(
              "Use `vcov = \"bootstrap\"` (quantreg's own xy-pair ",
              "resampling), or the analytic \"nid\" / \"ker\" ",
              "sandwiches."
            )
          )
        }
        spicy_abort(
          c(
            sprintf(
              "`vcov = \"%s\"` is not available for `rq` models.",
              vt
            ),
            why_rq
          ),
          class = "spicy_unsupported_vcov",
          call = call
        )
      }
      # estimatr and fixest carry the variance they were COMPUTED with:
      # estimatr bakes se_type into the fit object, and fixest owns its
      # own vcov interface (IID unless the user asked otherwise at
      # estimation or in summary()). spicy never overwrites either.
      # The refusal is a settled policy, so it gives its own reason
      # rather than the generic list of supported estimators below, which
      # would hide the one thing the user can actually do: choose the
      # estimator at fit time.
      if (inherits(models[[i]], c("lm_robust", "iv_robust"))) {
        # Name the fit's OWN estimatr function. Pointing an iv_robust
        # user at lm_robust() would swap the estimator (2SLS -> OLS) to
        # change the variance -- a different model, not a different
        # standard error.
        estimatr_fn <- if (inherits(models[[i]], "iv_robust")) {
          "estimatr::iv_robust"
        } else {
          "estimatr::lm_robust"
        }
        spicy_abort(
          c(
            sprintf(
              "`vcov = \"%s\"` is not available for `%s` models.",
              vt,
              class(models[[i]])[1L]
            ),
            "i" = paste0(
              "estimatr fixes the variance estimator at fit time and ",
              "spicy reports the fit's own standard errors unchanged."
            ),
            "i" = sprintf(
              paste0(
                "Refit with the standard errors you want: ",
                "`%s(..., se_type = \"HC3\")`, or ",
                "`se_type = \"CR2\"` with `clusters =`."
              ),
              estimatr_fn
            )
          ),
          class = "spicy_unsupported_vcov",
          call = call
        )
      }
      # glmmTMB: CR0 through glmmTMB's own sandwich, and CR1, its G/(G-1)
      # scaling; every other token is refused with its reason
      # (R/vcov_glmmTMB.R).
      if (inherits(models[[i]], "glmmTMB")) {
        .glmmTMB_refuse_vcov(models[[i]], vt, call = call)
      }
      if (inherits(models[[i]], "fixest")) {
        spicy_abort(
          c(
            sprintf(
              "`vcov = \"%s\"` is not available for `%s` models.",
              vt,
              class(models[[i]])[1L]
            ),
            "i" = paste0(
              "fixest computes its own standard errors, which spicy ",
              "reports unchanged; the footer names the estimator the ",
              "fit carries."
            ),
            "i" = paste0(
              "Choose it through fixest: at estimation with ",
              "`fixest::feols(..., vcov = ~cluster_var)` or ",
              "`vcov = \"hetero\"`, or afterwards with ",
              "`summary(fit, vcov = ~cluster_var)`."
            )
          ),
          class = "spicy_unsupported_vcov",
          call = call
        )
      }
      # ivreg: HC2-HC5 and the resamplers, each with its reason
      # (.robust_vcov_support()). The quantreg tokens take the generic
      # message below.
      if (
        inherits(models[[i]], "ivreg") &&
          (startsWith(vt, "HC") || vt %in% c("bootstrap", "jackknife"))
      ) {
        spicy_abort(
          c(
            sprintf(
              "`vcov = \"%s\"` is not available for `ivreg` models.",
              vt
            ),
            "i" = if (startsWith(vt, "HC")) {
              paste0(
                "HC2-HC5 weight each residual by a leverage, which ",
                "two-stage least squares does not define uniquely: ",
                "`AER::ivreg()` and `estimatr::iv_robust()` use ",
                "different hat values. For the second, refit with ",
                "`estimatr::iv_robust(..., se_type = \"HC2\")` or `se_type = \"HC3\"`."
              )
            } else {
              paste0(
                "The resamplers refit an `lm()`, not the two-stage ",
                "least-squares model."
              )
            },
            "i" = paste(
              .vcov_supports_sentence(models[[i]]),
              sprintf("See %s.", .reg_help_page(models[[i]]))
            )
          ),
          class = "spicy_unsupported_vcov",
          call = call
        )
      }
      spicy_abort(
        c(
          sprintf(
            "`vcov = \"%s\"` is not available for `%s` models.",
            vt,
            class(models[[i]])[1L]
          ),
          "i" = paste(
            .vcov_supports_sentence(models[[i]]),
            sprintf("See %s.", .reg_help_page(models[[i]]))
          )
        ),
        class = "spicy_unsupported_vcov",
        call = call
      )
    }
  }
  invisible(NULL)
}

# Steps 7-8: the resolved `cluster` against `vcov` and the models.
validate_cluster_lists <- function(
  vcov,
  cluster,
  models,
  call = rlang::caller_env()
) {
  n_models <- length(models)

  # Step 7: cluster list length (only when an actual list, not an atomic vector)
  if (is.list(cluster) && !is.atomic(cluster)) {
    if (length(cluster) != n_models) {
      spicy_abort(
        c(
          sprintf(
            "`cluster` is a list of length %d but `models` has length %d.",
            length(cluster),
            n_models
          ),
          "i" = paste0(
            "Pass a single vector / column name (recycled to all ",
            "models) or a list of length(models)."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # Step 8: per-model coherence (CR* requires cluster, length matches nobs)
  vcov_per <- if (is.list(vcov)) {
    vcov
  } else {
    replicate(n_models, vcov, simplify = FALSE)
  }
  cluster_per <- if (is.list(cluster) && !is.atomic(cluster)) {
    cluster
  } else {
    replicate(n_models, cluster, simplify = FALSE)
  }

  for (i in seq_len(n_models)) {
    v_i <- vcov_per[[i]]
    c_i <- cluster_per[[i]]

    # GEE: clustering is defined by the model's own `id =` argument
    # (the sandwich over those clusters is already the default
    # inference), so a spicy-side `cluster` is refused outright --
    # the generic "set vcov to CR0-CR3" hint below would be wrong
    # guidance.
    if (inherits(models[[i]], "geeglm") && !is.null(c_i)) {
      .gee_refuse_cluster(model_index = i, call = call)
    }

    # quantreg::rq has no analytic cluster-robust estimator; its one
    # defensible cluster route is the wild gradient cluster bootstrap
    # (Hagemann 2017, JASA; quantreg's boot.rq cluster method). So for
    # rq, cluster + "bootstrap" is the BLESSED pair (no CR* warning),
    # and cluster with any other estimator is a hard error -- the
    # generic "set vcov to CR0-CR3" hint below would be wrong guidance.
    if (inherits(models[[i]], "rq")) {
      if (!is.null(c_i) && !identical(v_i, "bootstrap")) {
        spicy_abort(
          c(
            sprintf(
              paste0(
                "Model %d: `cluster` with `vcov = \"%s\"` is not ",
                "available for quantile regression."
              ),
              i,
              v_i
            ),
            "i" = paste0(
              "rq has no analytic cluster-robust estimator; use ",
              "`vcov = \"bootstrap\"` with `cluster =` (wild gradient ",
              "cluster bootstrap)."
            )
          ),
          class = "spicy_unsupported_vcov",
          call = call
        )
      }
      if (!is.null(c_i) && is.atomic(c_i)) {
        .check_cluster_length(
          models[[i]],
          c_i,
          label = sprintf(
            "`cluster%s`",
            if (n_models > 1L) sprintf("[[%d]]", i) else ""
          ),
          call = call
        )
      }
      next
    }

    is_cr <- startsWith(v_i, "CR")
    # A glmmTMB fit clusters by default on its top-level grouping factor;
    # R/vcov_glmmTMB.R says so, or why there is none.
    if (is_cr && is.null(c_i) && !inherits(models[[i]], "glmmTMB")) {
      spicy_abort(
        c(
          sprintf(
            "Model %d uses `vcov = \"%s\"` but no cluster is supplied.",
            i,
            v_i
          ),
          "i" = if (n_models == 1L) {
            "Pass a cluster vector / column name, or use a non-cluster `vcov`."
          } else {
            paste0(
              "Pass `cluster = list(<cluster_vec_1>, <cluster_vec_2>, ...)` ",
              "with the corresponding cluster vectors."
            )
          }
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }

    # Cluster length must match what this fit's cluster-robust vcov requires.
    # Delegated to the shared .check_cluster_length() (the same guard the compute
    # path uses) so the required-length logic and the class-aware message live in
    # ONE place. The label carries the per-model `[[i]]` index for multi-model.
    if (!is.null(c_i) && is.atomic(c_i)) {
      .check_cluster_length(
        models[[i]],
        c_i,
        label = sprintf(
          "`cluster%s`",
          if (n_models > 1L) sprintf("[[%d]]", i) else ""
        ),
        call = call
      )
    }

    # Cluster supplied but vcov is not CR* -- silent ignore would be a
    # forgotten-vcov mistake. Warn explicitly so the user notices.
    if (!is.null(c_i) && !is_cr) {
      # Own-estimator classes get their own route, like the rq carve-out
      # above. The generic "set `vcov` to CR0-CR3" advice would send an
      # estimatr / fixest / survey user straight into the hard refusal
      # that Step 6c raises for exactly those tokens -- advice that
      # cannot be followed. Their clustering lives in the fitting call,
      # or, for a design fit, in the design.
      hint <- if (.is_design_fit(models[[i]])) {
        # There is no CR* route for a design fit at all: clubSandwich has
        # no vcovCR.svyglm, so the token is refused outright above. The
        # design already carries the clustering it was declared with, and
        # its Taylor / replicate variance IS the cluster-robust one --
        # the same sentence Step 6c uses when the user follows the old
        # hint and lands on the refusal.
        paste0(
          "The survey design carries the clustering: declare it with ",
          "survey::svydesign(ids = ~cluster_var, ...) and refit. The ",
          "fit's own design-based variance is already cluster-robust ",
          "for the declared design."
        )
      } else if (inherits(models[[i]], c("lm_robust", "iv_robust"))) {
        estimatr_fn <- if (inherits(models[[i]], "iv_robust")) {
          "estimatr::iv_robust"
        } else {
          "estimatr::lm_robust"
        }
        sprintf(
          paste0(
            "estimatr clusters at fit time: ",
            "`%s(..., clusters = <var>, se_type = \"CR2\")`."
          ),
          estimatr_fn
        )
      } else if (inherits(models[[i]], "fixest")) {
        paste0(
          "fixest clusters through its own `vcov` interface: ",
          "`fixest::feols(..., vcov = ~cluster_var)`, or ",
          "`summary(fit, vcov = ~cluster_var)`."
        )
      } else if (.pscl_is_negbin(models[[i]])) {
        # Its one cluster token, CR1, is refused (R/vcov.R).
        if (.pscl_count_is_negbin(models[[i]])) {
          paste0(
            "This negative-binomial fit has no cluster-robust `vcov`: fit ",
            "it with glmmTMB to use `vcov = \"CR1\"` with the cluster vector.",
            .pscl_glmmTMB_zero_note(models[[i]])
          )
        } else {
          paste0(
            "This negative-binomial zero hurdle has no cluster-robust ",
            "`vcov`: refit with `zero.dist = \"binomial\"` to use ",
            "`vcov = \"CR1\"` with the cluster vector."
          )
        }
      } else {
        # The cluster values this fit accepts (CR1S, Stata's variant of
        # CR1, left out), or why it has none.
        v <- .vcov_cited(models[[i]])
        cr <- sprintf("`\"%s\"`", grep("^CR[0-9]$", v$values, value = TRUE))
        n <- length(cr)
        if (n > 0L) {
          sprintf(
            "Set `vcov` to %s to use the cluster vector for cluster-robust SE.",
            if (n < 3L) {
              paste(cr, collapse = " or ")
            } else {
              paste0(paste(cr[-n], collapse = ", "), ", or ", cr[n])
            }
          )
        } else if (!is.null(v$others)) {
          sprintf(
            "This fit has no cluster-robust `vcov`. Its class supports %s.",
            v$others
          )
        } else {
          sprintf(
            "`%s` models have no cluster-robust `vcov`.",
            class(models[[i]])[1L]
          )
        }
      }
      spicy_warn(
        c(
          sprintf(
            paste0(
              "Model %d: `cluster` supplied but `vcov = \"%s\"` ",
              "is not cluster-robust; the cluster vector is ",
              "ignored."
            ),
            i,
            v_i
          ),
          "i" = hint
        ),
        class = "spicy_ignored_arg"
      )
    }
  }

  invisible(NULL)
}


# ---- Phase C \u2013 vocabulary token validation --------------------------------

# `show_columns = "none"`: the fit statistics alone, one column per model
# and no coefficient. TRUE for that token, FALSE for any other request,
# which validate_show_columns() then checks. "none" goes alone. An empty
# request (`character(0)`, `FALSE`) stays an error: an empty vector
# reaches a script by accident, so the table meant is asked for by name.
.reg_show_columns_none <- function(show_columns, call = rlang::caller_env()) {
  empty <- is.character(show_columns) && length(show_columns) == 0L
  if (empty || isFALSE(show_columns)) {
    spicy_abort(
      c(
        sprintf(
          "`show_columns` cannot be %s.",
          if (empty) "empty" else "`FALSE`"
        ),
        "i" = "For the fit statistics alone, use `show_columns = \"none\"`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (!is.character(show_columns) || !"none" %in% show_columns) {
    return(FALSE)
  }
  others <- setdiff(show_columns, "none")
  if (length(show_columns) > 1L) {
    spicy_abort(
      c(
        "`show_columns = \"none\"` cannot be combined with other tokens.",
        "x" = if (length(others)) {
          sprintf(
            "Also requested: %s.",
            .fmt_tokens(others)
          )
        },
        "i" = "Use `\"none\"` alone for the fit statistics without coefficients."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  TRUE
}

# Step 9: show_columns (+ Step 12: beta requires standardized != "none")
#
# `show_columns` accepts both atomic tokens ("b", "se", "ci", "p", ...)
# and group tokens ("all_b", "all_ame", ...). Group tokens are
# pre-expanded by `expand_show_columns()` before this validator runs
# so the only thing left to check here is membership + the
# beta-without-method rule.
validate_show_columns <- function(
  show_columns,
  standardized,
  call = rlang::caller_env()
) {
  validate_token_vector(
    show_columns,
    .regression_tokens$show_columns_atomic,
    arg = "show_columns",
    call = call
  )

  # Step 12: beta-without-method
  if ("beta" %in% show_columns && identical(standardized, "none")) {
    spicy_abort(
      c(
        "`\"beta\"` is in `show_columns` but `standardized = \"none\"`.",
        "i" = paste0(
          "Set `standardized` to one of: \"refit\", \"posthoc\", ",
          "\"basic\", \"smart\", \"pseudo\" (\"pseudo\" is `glm` only)."
        ),
        "i" = "Or remove `\"beta\"` from `show_columns`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# Replace every univariable-screen bundle in `models` by the fits it
# wraps, so a class gate sees the models that will actually be
# estimated. Everything else passes through untouched.
.unwrap_screen_bundles <- function(models) {
  out <- list()
  for (m in models) {
    if (inherits(m, "spicy_uv_screen")) {
      out <- c(out, m$fits)
    } else {
      out <- c(out, list(m))
    }
  }
  out
}

# Fits whose dispersion is the NB2 theta (the "theta" / "alpha" rows),
# the NB1 delta (the "delta" row), and fits whose precision is the beta
# phi (the "phi" row). A pre-frame class sniff, like
# absorbs_fixed_effects() below. A theta or phi fixed in the call is not
# an estimate and has no row: glm's negative.binomial(theta), mgcv's
# negbin(theta), nb(theta) and betar(theta). A negative-binomial glmer
# always has the row (.glmer_is_nb()).
.is_nb2_fit <- function(model) {
  if (inherits(model, "negbin")) {
    return(TRUE)
  }
  # fenegbin() and femlm(family = "negbin") store the family as the
  # string "negbin", with the NB2 theta in fit$theta.
  if (inherits(model, "fixest")) {
    return(
      is.character(model$family) && identical(model$family[[1L]], "negbin")
    )
  }
  # pscl: the theta of a negative-binomial count part.
  if (inherits(model, c("zeroinfl", "hurdle"))) {
    return(.pscl_count_is_negbin(model))
  }
  if (inherits(model, "gam")) {
    return(.gam_is_nb_estimated(model))
  }
  if (inherits(model, "glmerMod")) {
    return(.glmer_is_nb(model))
  }
  inherits(model, "glmmTMB") &&
    isTRUE(
      tryCatch(stats::family(model)$family, error = function(e) "") %in%
        c("nbinom2", "truncated_nbinom2")
    )
}

.is_nb1_fit <- function(model) {
  inherits(model, "glmmTMB") &&
    isTRUE(
      tryCatch(stats::family(model)$family, error = function(e) "") %in%
        c("nbinom1", "truncated_nbinom1")
    )
}

# glmmTMB's sigma() is the beta precision for beta_family() and, by its
# help page, for betabinomial() ("the same parameterization ... as beta")
# and ordbeta() ("see details for beta"). mgcv's betar() keeps it in
# its theta.
.is_beta_fit <- function(model) {
  if (inherits(model, "gam")) {
    return(.gam_is_betar_estimated(model))
  }
  inherits(model, "betareg") ||
    (inherits(model, "glmmTMB") &&
      isTRUE(
        tryCatch(stats::family(model)$family, error = function(e) "") %in%
          c("beta", "betabinomial", "ordbeta")
      ))
}

# A dispersion token on a glmmTMB fit of the token's family whose
# dispersion has covariates: not one number, refused as betareg's phi
# is. `models` holds the fits of that family only, so a gaussian
# glmmTMB with a `dispformula` beside an NB2 fit leaves theta defined.
.abort_modeled_dispersion <- function(models, tokens, param, call) {
  modeled <- vapply(
    models,
    function(m) inherits(m, "glmmTMB") && !.glmmTMB_constant_dispersion(m),
    logical(1)
  )
  if (any(modeled)) {
    spicy_abort(
      c(
        .token_msg(
          "{Tokens} in `show_fit_stats` {need} a constant dispersion, but a model has covariates in its `dispformula`.",
          tokens
        ),
        "i" = paste0(
          "With dispersion covariates, ",
          param,
          " is not a single number; ",
          "inspect the dispersion coefficients with `summary(fit)` instead."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# Fits that absorb fixed effects, and so can disclose them: every
# fixest estimator, plus an estimatr fit built with `fixed_effects =`
# (which sets `fes`). A pre-frame class sniff, like its callers.
absorbs_fixed_effects <- function(model) {
  inherits(model, "fixest") ||
    (inherits(model, c("lm_robust", "iv_robust")) && isTRUE(model$fes))
}


# A `glm` (not a feglm: fixest has no term-level test here) whose
# dispersion is estimated, the models whose term-level test is the
# partial F (`partial_f`). summary() is asked quietly: the coefficient
# rows report its warnings (zero prior weights) once already.
.is_dispersion_glm <- function(fit) {
  !inherits(fit, "fixest") && suppressWarnings(has_estimated_dispersion(fit))
}

# The facts of the term-level tests, one row per fit of `eff_models`
# (screens unwrapped), computed once: which fits carry `partial_f` (an
# lm, a glm whose dispersion is estimated), which carry `partial_chi2` (a
# glm whose dispersion is fixed, glm.nb, the mixed models), and how a
# message names each one, by its position among `models` and its class
# (and family). A fit in neither set has no term-level test (coxph,
# GEE, svyglm, gam, fixest, rlm, ...).
.term_test_facts <- function(models, eff_models) {
  owner <- unlist(lapply(seq_along(models), function(i) {
    m <- models[[i]]
    rep(i, if (inherits(m, "spicy_uv_screen")) length(m$fits) else 1L)
  }))
  disp <- vapply(eff_models, .is_dispersion_glm, logical(1))
  lm_fit <- vapply(
    eff_models,
    function(f) {
      inherits(f, "lm") && !inherits(f, c("glm", "rlm", "rms"))
    },
    logical(1)
  )
  chi2 <- !disp &
    vapply(
      eff_models,
      function(f) {
        glm_or_nb <- class(f)[1L] %in% c("glm", "negbin")
        glm_or_nb || inherits(f, c("merMod", "glmmTMB", "lme"))
      },
      logical(1)
    )
  label <- vapply(
    seq_along(eff_models),
    function(j) {
      f <- eff_models[[j]]
      fam <- if (inherits(f, "glm")) {
        tryCatch(stats::family(f)$family, error = function(e) NULL)
      }
      .reg_model_tag(
        models,
        owner[j],
        cls = class(f)[1L],
        detail = if (length(fam)) .family_label(fam)
      )
    },
    character(1)
  )
  list(
    carries_f = disp | lm_fit,
    carries_chi2 = chi2,
    lm = lm_fit,
    label = label
  )
}

# The two term-level tests of `table_regression()`, each refused where
# it would lose or invent a test. A requested token is refused when
# (A) no model of the set carries it while some model carries the other
# one -- its column would be blank for every model -- or (B) some model
# carries the other token but not this one, and the other token is not
# requested -- that model's test would vanish without a word. A set
# where no model has a term-level test at all is left to the GEE arm and
# to the capability guard of table_regression(), which name the class.
.check_term_test_tokens <- function(show_columns, facts, call) {
  q <- function(tok) paste0("`\"", tok, "\"`")
  cap <- function(x) paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))
  for (tok in intersect(c("partial_chi2", "partial_f"), show_columns)) {
    other <- setdiff(c("partial_chi2", "partial_f"), tok)
    has <- if (tok == "partial_f") facts$carries_f else facts$carries_chi2
    has_other <- if (tok == "partial_f") facts$carries_chi2 else facts$carries_f
    test_of <- if (tok == "partial_f") {
      paste0(
        q("partial_f"),
        " is the partial F of an `lm` and of a `glm` whose ",
        "dispersion is estimated (gaussian, Gamma, inverse Gaussian, ",
        "Tweedie, `negative.binomial(theta)` fitted by `glm()`, the quasi ",
        "families), the F that `anova(reduced, full, test = \"F\")` reports."
      )
    } else {
      paste0(
        q("partial_chi2"),
        " is the likelihood-ratio chi-square of a `glm` whose dispersion ",
        "is fixed (binomial, Poisson, `MASS::glm.nb()`) and the Wald ",
        "chi-square of a mixed-effects model."
      )
    }
    if (!any(has) && any(has_other)) {
      what <- if (all(facts$lm)) {
        "`lm` models"
      } else {
        .fmt_and(unique(facts$label))
      }
      spicy_abort(
        c(
          sprintf(
            "Token %s in `show_columns` is not defined for %s.",
            q(tok),
            what
          ),
          "i" = test_of,
          "i" = if (tok == "partial_chi2" && any(facts$lm)) {
            paste0(
              "Use ",
              q("partial_f"),
              ", or ",
              q("partial_f2"),
              ", ",
              q("partial_eta2"),
              " or ",
              q("partial_omega2"),
              " for the ",
              "variance-explained partials of an `lm`."
            )
          } else {
            paste0("Use ", q(other), ".")
          }
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    lost <- unique(facts$label[has_other & !has])
    if (length(lost) > 0L && !other %in% show_columns) {
      spicy_abort(
        c(
          sprintf(
            "%s %s no %s: %s term-level test is %s.",
            cap(.fmt_and(lost)),
            if (length(lost) > 1L) "have" else "has",
            q(tok),
            if (length(lost) > 1L) "their" else "its",
            q(other)
          ),
          "i" = test_of,
          "i" = paste0(
            "Each test has its own column: add ",
            q(other),
            " to `show_columns`, or drop the model."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }
  invisible(TRUE)
}

# `partial_type = "III"` and a partial column shown. The Type III test of a
# term is that of its own coefficients in the sum-to-zero
# parametrization: a model with an aliased coefficient (an empty cell of
# an interaction) has no such parametrization, and is refused. A factor
# that interacts with a numeric covariate is tested where the covariate
# is 0, as SPSS and SAS test it: one warning names every such factor,
# and says to center the covariate. Nothing is centered for the user.
.check_partial_type <- function(
  models,
  show_columns,
  partial_type,
  panel = NULL,
  call = rlang::caller_env()
) {
  if (
    !identical(partial_type, "III") ||
      !any(.PARTIAL_ES_TOKENS %in% show_columns)
  ) {
    return(invisible(NULL))
  }
  eff_models <- .unwrap_screen_bundles(models)
  facts <- .term_test_facts(models, eff_models)
  tested <- facts$carries_f | facts$carries_chi2
  label <- facts$label
  if (!is.null(panel$name)) {
    label <- sub("^the model", panel$name, label)
  }
  aliased <- tested & vapply(eff_models, .type3_aliased, logical(1))
  if (any(aliased)) {
    lost <- unique(label[aliased])
    spicy_abort(
      c(
        sprintf(
          "`partial_type = \"III\"` needs every coefficient to be estimable, but %s %s an aliased coefficient.",
          .fmt_and(lost),
          if (length(lost) > 1L) "have" else "has"
        ),
        "i" = paste0(
          "An empty cell of an interaction leaves a coefficient ",
          "undetermined (`NA`), and the Type III test of a term is that ",
          "of its own coefficients."
        ),
        "i" = paste0(
          "Use `partial_type = \"II\"` (the default), which is defined ",
          "here, or drop the empty cell."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  reduced <- lapply(seq_along(eff_models), function(i) {
    if (!tested[[i]]) {
      return(character(0))
    }
    r <- tryCatch(
      .type3_reduced_contrasts(eff_models[[i]]),
      error = function(e) {
        character(0)
      }
    )
    if (length(r)) paste0(r, " in ", label[[i]]) else character(0)
  })
  reduced <- unique(unlist(reduced))
  if (length(reduced) > 0L) {
    spicy_abort(
      c(
        paste0(
          "`partial_type = \"III\"` tests the equality of every level of a ",
          "factor, but ",
          if (length(reduced) > 1L) "the contrasts of " else "the contrast of ",
          .fmt_and(reduced),
          if (length(reduced) > 1L) " have" else " has",
          " fewer columns than levels minus one."
        ),
        "i" = paste0(
          "Such a model constrains the factor to fewer directions than its ",
          "levels span, so its Type III test is not defined."
        ),
        "i" = paste0(
          "Use `partial_type = \"II\"` (the default), or fit the factor ",
          "with a complete contrast (one column per level but one)."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  pairs <- unique(unlist(lapply(
    eff_models[tested],
    function(f) tryCatch(.type3_covariate_pairs(f), error = function(e) NULL)
  )))
  if (length(pairs) > 0L) {
    spicy_warn(
      c(
        paste0(
          "Under `partial_type = \"III\"`, a factor that interacts with a ",
          "numeric covariate is tested where the covariate is 0: ",
          .fmt_and(pairs),
          "."
        ),
        "i" = paste0(
          "Center the covariate (subtract its mean) to test the factor ",
          "at the mean of the covariate."
        )
      ),
      class = "spicy_caveat",
      spicy_gather = .reg_gather(panel, "partial_type_covariate")
    )
  }
  invisible(NULL)
}

# A fit whose design has an aliased column: an `NA` coefficient (lm,
# glm, glmmTMB), or a column lme4 dropped. nlme refuses such a fit.
.type3_aliased <- function(fit) {
  if (inherits(fit, "merMod")) {
    return(!is.null(attr(lme4::getME(fit, "X"), "col.dropped")))
  }
  if (inherits(fit, "glmmTMB")) {
    return(anyNA(glmmTMB::fixef(fit)$cond))
  }
  if (inherits(fit, "lme")) {
    return(FALSE)
  }
  anyNA(stats::coef(fit))
}

# The factors of a fit coded by a user contrast of fewer columns than
# levels minus one, as "`f` (3 levels, 1 column)", when the design they
# give has fewer columns than the sum-to-zero design of the Type III test.
# A factor whose contrast the design does not use (the first factor of a
# model without intercept) is not one: its design is complete.
.type3_reduced_contrasts <- function(fit) {
  if (inherits(fit, "merMod")) {
    x_fit <- lme4::getME(fit, "X")
    ctr <- attr(x_fit, "contrasts")
  } else if (inherits(fit, "glmmTMB")) {
    x_fit <- glmmTMB::getME(fit, "X")
    ctr <- attr(x_fit, "contrasts")
  } else if (inherits(fit, "lme")) {
    ctr <- fit$contrasts
    td <- .type3_terms_data(fit)
    x_fit <- stats::model.matrix(td$terms, td$data, contrasts.arg = ctr)
  } else {
    x_fit <- stats::model.matrix(fit)
    ctr <- fit$contrasts
  }
  short <- vapply(
    ctr,
    function(m) is.matrix(m) && ncol(m) < nrow(m) - 1L,
    logical(1)
  )
  if (!any(short)) {
    return(character(0))
  }
  td <- .type3_terms_data(fit)
  if (ncol(x_fit) >= ncol(.type3_sum_design(td$terms, td$data))) {
    return(character(0))
  }
  vapply(
    names(ctr)[short],
    function(v) {
      m <- ctr[[v]]
      sprintf(
        "`%s` (%d levels, %d %s)",
        v,
        nrow(m),
        ncol(m),
        if (ncol(m) == 1L) "column" else "columns"
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# The factor terms of a fit that interact with a numeric covariate, as
# "`A` (with `x`)": a term whose variables are all categorical, contained
# in a higher-order term that has a numeric variable.
.type3_covariate_pairs <- function(fit) {
  td <- .type3_terms_data(fit)
  fac <- attr(td$terms, "factors")
  if (!length(fac)) {
    return(character(0))
  }
  vars <- rownames(fac)
  is_cat <- vapply(
    vars,
    function(v) {
      x <- td$data[[v]]
      is.factor(x) || is.character(x) || is.logical(x)
    },
    logical(1)
  )
  labs <- colnames(fac)
  out <- character(0)
  for (k in seq_along(labs)) {
    in_k <- fac[, k] != 0
    if (!all(is_cat[in_k])) {
      next
    }
    for (j in setdiff(seq_along(labs), k)) {
      if (!all(fac[in_k, j] != 0)) {
        next
      }
      cov <- vars[fac[, j] != 0 & !is_cat]
      if (length(cov)) {
        out <- c(
          out,
          sprintf(
            "`%s` (with %s)",
            labs[k],
            paste0("`", cov, "`", collapse = ", ")
          )
        )
      }
    }
  }
  unique(out)
}

# Class-aware token validation. Rejects tokens that are
# mathematically inappropriate for the model class with a clear
# remediation pointer toward the right substitute. Called AFTER
# validate_show_columns / validate_show_fit_stats (which only
# check vocabulary membership) so the user always sees the
# generic-error first when a typo is present, the class-specific
# error second when the typo is absent.
#
# Substitution policy (closest analog per discipline convention):
#   * lm  -> no  partial_chi2          (use partial_f, the Type-II F,
#                                      or partial_f2 / \u03B7\u00B2 / \u03C9\u00B2)
#   * lm  -> no  pseudo_r2_*           (use r2 / adj_r2 / omega2)
#   * glm -> no  partial_f2 / \u03B7\u00B2 / \u03C9\u00B2  (use partial_chi2; Type-II LRT;
#                                      Long & Freese 2014 \u00A73.2.2, \u00A73.2.4;
#                                      partial_f, the Type-II F, when the
#                                      dispersion is estimated)
#   * partial_chi2 / partial_f: one rule, `.check_term_test_tokens()`
#   * glm -> no  r2 / adj_r2 / omega2  (use pseudo_r2_*; McFadden
#                                      1974 / Nagelkerke 1991 / Tjur 2009)
# The fit-statistic tokens of `tokens` that some model of `models` shows a
# row for, to name in a remedy: a remedy never names a token the table
# would refuse. Read from the column `fit_stats` of the registry
# (.reg_offers(), R/table_regression_models.R).
.fit_stats_offered <- function(models, tokens) {
  tokens[vapply(tokens, .reg_any_offers, logical(1), models = models)]
}

# TRUE when the registry offers `token` for some model of `models`.
.reg_any_offers <- function(models, token) {
  any(vapply(models, .reg_offers, logical(1), token = token))
}

# The line of a refusal that proposes other fit statistics: `fmt` with the
# tokens the models show (each followed by its note, if any), or NULL --
# no line -- when they show none.
.fit_stats_remedy <- function(models, tokens, fmt, notes = NULL) {
  ok <- .fit_stats_offered(models, tokens)
  if (length(ok) == 0L) {
    return(NULL)
  }
  items <- paste0("`", .quote_val(ok), "`")
  if (!is.null(notes)) {
    items <- paste(items, notes[ok])
  }
  sprintf(fmt, .fmt_and(items, "or"))
}

# The nested test a table of glm fits offers: the likelihood-ratio
# chi-square, or the F test of the deviance change when the dispersion is
# estimated; nothing for a table that mixes the two.
.glm_nested_remedy <- function(models) {
  # A design-based or a Bayesian glm has no nested test (the nested guard
  # of table_regression()).
  if (
    any(vapply(
      models,
      function(m) .is_design_fit(m) || inherits(m, c("stanreg", "brmsfit")),
      logical(1)
    ))
  ) {
    return(NULL)
  }
  disp <- vapply(models, has_estimated_dispersion, logical(1))
  if (all(disp)) {
    c(
      "i" = paste0(
        "For a nested glm comparison, use `\"f_change\"` + ",
        "`\"p_change\"` (F test of the deviance change)."
      )
    )
  } else if (!any(disp)) {
    c(
      "i" = paste0(
        "For a nested glm comparison, use `\"lrt_change\"` + ",
        "`\"p_change\"` (likelihood-ratio chi-square)."
      )
    )
  }
}

# The classes of the fits a message is about, as it names them: "`glm`",
# "`stanreg`", "`glm` and `negbin`". A glm-family refusal reaches
# every fit that inherits from glm (stan_glm, glm.nb, ...): it names
# their own class, not the parent's.
.fit_classes_label <- function(models) {
  .fmt_and(sprintf(
    "`%s`",
    unique(vapply(models, function(m) class(m)[1L], ""))
  ))
}

validate_class_appropriate_tokens <- function(
  models,
  show_columns,
  show_fit_stats,
  call = rlang::caller_env()
) {
  # TRUE "all" over the model set, not the historical any/any proxy:
  # the proxy classified a {glm, fixest} set as "all glm" (fixest
  # inherits neither lm nor glm) and rejected the package's OWN
  # class-aware default vector ("r2" from the fixest arm) -- the
  # 2026-07 FE-lot review caught the default glm + fixest table
  # hard-erroring. Mixed sets render class-inappropriate cells blank,
  # which is the intended behavior; only homogeneous sets refuse.
  # geeglm inherits from glm but is quasi-likelihood: it gets its own
  # gate below (no likelihood -> no pseudo-R^2 / IC / partial LRT),
  # so it must not be claimed by the glm bucket -- the glm messages
  # would suggest substitutes GEE cannot compute.
  #
  # A univariable-screen bundle stands for the fits it wraps, here as
  # in the r2 gate below: the bundle is a plain list carrying class
  # `spicy_uv_screen`, so reading its class alone made every screen a
  # mixed set, and the substitution gates never fired. A linear screen
  # asking for `partial_chi2` then rendered a fully blank column --
  # exactly what a solo lm() is refused for.
  eff_models <- .unwrap_screen_bundles(models)
  gee_flags <- vapply(eff_models, inherits, logical(1), "geeglm")
  glm_flags <- vapply(eff_models, inherits, logical(1), "glm") & !gee_flags
  lm_only_flags <- vapply(
    eff_models,
    function(f) {
      inherits(f, "lm") && !inherits(f, "glm")
    },
    logical(1)
  )
  all_glm <- length(eff_models) > 0L && all(glm_flags)
  # mgcv's gam inherits glm but has its own frame: a table of GAMs is
  # answered in its own words, never pointed at a glm-only token.
  all_gam <- length(eff_models) > 0L &&
    all(vapply(eff_models, inherits, logical(1), "gam"))
  # The term-level test facts, once per fit and only when a partial token
  # is requested (summary() per glm).
  facts <- if (any(.PARTIAL_ES_TOKENS %in% show_columns)) {
    .term_test_facts(models, eff_models)
  }
  # glm / feglm fits whose dispersion is estimated: their nested test is
  # the F test of anova(), carried by f_change, and not a chi-square.
  # Asked only when one of the two tokens is requested (summary() per fit).
  all_disp <- any(c("lrt_change", "f_change") %in% show_fit_stats) &&
    length(eff_models) > 0L &&
    all(vapply(eff_models, has_estimated_dispersion, logical(1)))
  if (all_disp && "lrt_change" %in% show_fit_stats) {
    spicy_abort(
      c(
        paste0(
          "Token `\"lrt_change\"` in `show_fit_stats` is not computed for ",
          "models whose dispersion is estimated."
        ),
        "i" = paste0(
          "Their nested test is the F test `anova()` reports, on the ",
          "deviance change over the estimated dispersion."
        ),
        "i" = paste0(
          "Use `\"f_change\"` + `\"p_change\"`, which is also what ",
          "`nested = TRUE` selects for this hierarchy by default."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  all_lm <- length(eff_models) > 0L && all(lm_only_flags)

  # Variance-explained partial tokens. Reject only when ALL models
  # are glm \u2013 in mixed sets, the renderer en-dashes glm rows and
  # populates lm rows, which is the right behavior.
  if (all_glm) {
    bad <- intersect(show_columns, .PARTIAL_VARIANCE_ES_TOKENS)
    if (length(bad) > 0L && all_gam) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_columns` {are} not defined for `mgcv` GAMs.",
            bad
          ),
          "i" = paste0(
            "spicy computes no term-level effect size or test for a GAM: ",
            "`summary()` of the GAM gives the tests of its terms."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    if (length(bad) > 0L) {
      # The analog of each model: the term-level test it carries.
      analog_tok <- ifelse(
        facts$carries_f,
        "partial_f",
        ifelse(facts$carries_chi2, "partial_chi2", NA_character_)
      )
      analog <- if (length(unique(stats::na.omit(analog_tok))) > 1L) {
        per <- unique(paste0(
          "`\"",
          analog_tok,
          "\"` for ",
          facts$label
        )[!is.na(analog_tok)])
        c(
          "i" = paste0(
            "Variance-explained partition (R\u00B2 / partial f\u00B2 / \u03B7\u00B2 / \u03C9\u00B2) ",
            "does not apply outside the least-squares framework. The ",
            "analog of each model is its term-level test: ",
            paste(per, collapse = ", "),
            "."
          ),
          "i" = paste0(
            "Replace with `\"partial_chi2\"` and `\"partial_f\"` (each ",
            "model fills its own column) or drop the token."
          )
        )
      } else if (identical(unique(stats::na.omit(analog_tok)), "partial_f")) {
        c(
          "i" = paste0(
            "Variance-explained partition (R\u00B2 / partial f\u00B2 / \u03B7\u00B2 / \u03C9\u00B2) ",
            "does not apply outside the least-squares framework. The ",
            "analog for a `glm` whose dispersion is estimated is ",
            "`\"partial_f\"`, the partial F test that ",
            "`anova(reduced, full, test = \"F\")` reports."
          ),
          "i" = "Replace with `\"partial_f\"` or drop the token."
        )
      } else {
        c(
          "i" = paste0(
            "Variance-explained partition (R\u00B2 / partial f\u00B2 / \u03B7\u00B2 / \u03C9\u00B2) ",
            "does not apply outside the least-squares framework. The ",
            "`glm`-appropriate analog is `\"partial_chi2\"` (Type-II ",
            "partial likelihood-ratio chi-square; ",
            "Long & Freese 2014 \u00A73.2.2, \u00A73.2.4)."
          ),
          "i" = "Replace with `\"partial_chi2\"` or drop the token."
        )
      }
      spicy_abort(
        c(
          sprintf(
            .token_msg(
              "{Tokens} in `show_columns` {are} not defined for %s models.",
              bad
            ),
            .fit_classes_label(eff_models)
          ),
          analog
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    bad_fit <- intersect(
      show_fit_stats,
      c(
        "r2",
        # mgcv's adjusted R^2 (r.sq), which summary() prints for every GAM.
        if (!all_gam) "adj_r2",
        "omega2",
        "f2",
        # Nested change variants of the above
        "r2_change",
        "adj_r2_change",
        if (!all_disp) "f_change",
        "f2_change"
      )
    )
    if (length(bad_fit) > 0L && all_gam) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_fit_stats` {are} not defined for `mgcv` GAMs.",
            bad_fit
          ),
          .gam_r2_hint(eff_models)
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    if (length(bad_fit) > 0L) {
      spicy_abort(
        c(
          sprintf(
            .token_msg(
              "{Tokens} in `show_fit_stats` {are} not defined for %s models.",
              bad_fit
            ),
            .fit_classes_label(eff_models)
          ),
          "i" = .fit_stats_remedy(
            eff_models,
            c(
              "pseudo_r2_mcfadden",
              "pseudo_r2_nagelkerke",
              "pseudo_r2_tjur",
              "r2_deviance"
            ),
            sprintf(
              "Use the R\u00B2 of %s fits instead: %%s.",
              .fit_classes_label(eff_models)
            ),
            notes = c(
              pseudo_r2_mcfadden = "(McFadden 1974)",
              pseudo_r2_nagelkerke = "(Nagelkerke 1991)",
              pseudo_r2_tjur = "(Tjur 2009)",
              r2_deviance = "(1 - deviance / null deviance)"
            )
          ),
          .glm_nested_remedy(eff_models)
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # The term-level tests: `partial_f`, the F that `anova(reduced, full,
  # test = "F")` reports for an lm and for a glm whose dispersion is
  # estimated, and `partial_chi2`, the likelihood-ratio chi-square of a
  # glm whose dispersion is fixed and the Wald chi-square of the mixed
  # models. One rule for both, `.check_term_test_tokens()`.
  if (!is.null(facts)) {
    .check_term_test_tokens(show_columns, facts, call)
  }

  # The same refusal, for the same reason, for every OTHER class whose
  # nested comparison is a likelihood-ratio test. glm has had it since
  # the beginning; survreg, polr, gls, clm, coxph, betareg, the mixed
  # families and the rest got silence -- `compute_one_pair_lrt()` returns
  # NA for the four least-squares change tokens, the renderer drops a
  # fit-stat row that is NA across the whole table, and the row the user
  # asked for was simply not there. Two users, one mistake, two answers.
  #
  # Only the CHANGE tokens: the absolute r2 / adj_r2 / omega2 / f2 rows
  # are a wider question (betareg has a pseudo-R2, gam a real one) and
  # stay out of this arm. `all_likelihood_path()` is the predicate the
  # class-aware default already uses, so a default can never name a
  # token this refuses -- such a hierarchy defaults to lrt_change +
  # p_change, which is exactly what the message points at.
  #
  # The TOKENS are tested first and the predicate only afterwards, and
  # that order is load-bearing, not style: `all_likelihood_path()` calls
  # stats::logLik() on every model, and logLik() is not free on every
  # class -- logLik.brmsfit() builds the whole ndraws x nobs pointwise
  # matrix. Asked the other way round, every plain table_regression()
  # call paid for a question about change tokens that were never
  # requested (measured: one has_usable_loglik() per model on a call
  # with no change token in it).
  bad_fit <- intersect(
    show_fit_stats,
    c("r2_change", "adj_r2_change", if (!all_disp) "f_change", "f2_change")
  )
  if (length(bad_fit) > 0L && !all_glm && all_likelihood_path(eff_models)) {
    spicy_abort(
      c(
        # The headline names the ROUTE, not a property of the class.
        # "not defined for `ols` models" was simply false: rms::ols IS
        # ordinary least squares, its R^2 matches lm's to 1e-12, and
        # DeltaR^2 is perfectly well defined on such a pair --
        # fixest::feols is the same case. What is true of every class
        # that reaches here is that spicy compares the hierarchy
        # through the likelihood, and that route carries no
        # variance-explained change.
        sprintf(
          paste0(
            "spicy compares hierarchies of `%s` fits through the ",
            "likelihood-ratio chi-square, so %s in ",
            "`show_fit_stats` %s not computed on this route."
          ),
          class(eff_models[[1L]])[1L],
          .token_msg("{tokens}", bad_fit),
          .token_msg("{are}", bad_fit)
        ),
        "i" = paste0(
          "The nested comparison for these fits is a ",
          "likelihood-ratio chi-square, which reports no ",
          "variance-explained change."
        ),
        # Not for a class whose hierarchy has no test at all (design-based
        # fits and clmm, refused by the nested guard of
        # table_regression()), an nlme or glmmTMB fit by REML, whose
        # hierarchy across fixed effects is refused
        # (check_nested_reml_pair()), or a fixest IV fit, whose
        # likelihood-ratio row stays empty: no remedy the table would refuse.
        "i" = if (
          !any(vapply(
            eff_models,
            function(m) {
              .is_design_fit(m) ||
                inherits(m, "clmm") ||
                is_reml_pair_fit(m) ||
                (inherits(m, "fixest") && .fixest_is_iv_second_stage(m))
            },
            logical(1)
          ))
        ) {
          paste0(
            "Use `\"lrt_change\"` + `\"p_change\"`, which is also what ",
            "`nested = TRUE` selects for this hierarchy by default."
          )
        }
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  # Quantile hierarchies, which `all_likelihood_path()` excludes BY
  # CLASS and which therefore kept the exact silence the arm above ends.
  # Measured on an rq pair: r2_change, adj_r2_change, f2_change and
  # lrt_change were all accepted and all rendered nothing --
  # `compute_nested_comparisons()` routes any rq pair to
  # `compute_one_pair_rq()`, which hard-codes those four to NA_real_,
  # and the renderer drops a fit-stat row that is NA across the table.
  # The exception belongs on the TOKEN, not on the class: rq refuses the
  # variance-explained three like every other class, and lrt_change with
  # them, because an rq pair never reaches the likelihood route at all --
  # logLik.rq() is a pseudo-likelihood on the check-loss objective, not
  # a likelihood a ratio test can be built from. What rq DOES have is
  # anova.rq()'s Wald-type F, so the message points there, which is also
  # its class-aware default.
  #
  # Tokens first, predicate second, for the reason given above.
  bad_rq <- intersect(
    show_fit_stats,
    c("r2_change", "adj_r2_change", "f2_change", "lrt_change")
  )
  if (
    length(bad_rq) > 0L &&
      length(eff_models) > 0L &&
      all(vapply(eff_models, inherits, logical(1), "rq"))
  ) {
    spicy_abort(
      c(
        .token_msg(
          "{Tokens} in `show_fit_stats` {are} not defined for `rq` models.",
          bad_rq
        ),
        "i" = paste0(
          "Quantile regression partitions no sums of squares, and ",
          "`logLik.rq()` is a pseudo-likelihood on the check-loss ",
          "objective rather than one a ratio test can be built from."
        ),
        "i" = paste0(
          "Use `\"f_change\"` + `\"p_change\"` -- the Wald-type test ",
          "`quantreg::anova.rq()` reports, and what `nested = TRUE` ",
          "selects for a quantile hierarchy by default."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  # Robust M-estimation (MASS::rlm), which has no nested comparison of
  # ANY kind -- unlike rq, which at least owns a Wald-type F. The
  # objective is a bounded loss on the scaled residuals: it partitions no
  # sums of squares (summary()$r.squared is a logical NA and
  # adj.r.squared is absent altogether) and it is not a likelihood, so
  # anova.rlm returns its table with the F and Pr(>F) cells deliberately
  # empty. `stats::AIC()` does answer -- rlm inherits "lm", so logLik.lm
  # computes the GAUSSIAN likelihood of the residuals, which is the
  # criterion the M-estimator exists to avoid. Every change token is
  # therefore refused, not a subset: an rlm hierarchy used to render with
  # no change rows at all and no word about why (register n. 244(c)).
  # The `supports$nested_lrt = FALSE` flag catches `nested = TRUE`; this
  # arm catches the explicit request, and fires first because it can name
  # the tokens.
  bad_rlm <- intersect(show_fit_stats, .NESTED_CHANGE_TOKENS)
  if (
    length(bad_rlm) > 0L &&
      length(eff_models) > 0L &&
      all(vapply(eff_models, inherits, logical(1), "rlm"))
  ) {
    spicy_abort(
      c(
        .token_msg(
          "{Tokens} in `show_fit_stats` {are} not defined for `rlm` models.",
          bad_rlm
        ),
        "i" = paste0(
          "M-estimation minimizes a bounded loss rather than a ",
          "likelihood or a residual sum of squares, so an rlm ",
          "hierarchy has neither a likelihood-ratio test nor a partial ",
          "F to report."
        ),
        # The remedy has to be something the caller has not already
        # done: this arm is not conditioned on `nested`, so it fires on
        # a single fit and on a `nested = FALSE` pair, and telling
        # either of those to pass `nested = FALSE` is no remedy at all.
        # Dropping the token is, and it is the same one the homologous
        # glm arm offers.
        "i" = paste0(
          "Drop the token, or compare the least-squares fits (`lm()`) ",
          "if the hierarchy itself is the question."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  if (all_lm) {
    # `partial_chi2` on an lm: `.check_term_test_tokens()` above.
    bad_fit <- intersect(
      show_fit_stats,
      c(
        "pseudo_r2_mcfadden",
        "pseudo_r2_nagelkerke",
        "pseudo_r2_tjur",
        "r2_deviance"
      )
    )
    if (length(bad_fit) > 0L) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_fit_stats` {are} for `glm` models only.",
            bad_fit
          ),
          "i" = .fit_stats_remedy(
            eff_models,
            c("r2", "adj_r2", "omega2", "f2"),
            "Use %s for these models."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # `r2` / `adj_r2` as COLUMNS (the univariable-screen per-fit R^2).
  # The quantity is the least-squares variance partition, so the token
  # needs at least one OLS fit in the table -- one is enough (a mixed
  # set leaves the other groups' cells blank, like every other
  # class-specific column). The refusal is explicit rather than a
  # silently substituted pseudo-R^2: outside least squares there are
  # several, they disagree, and none of them is "the" R^2.
  r2_cols <- intersect(show_columns, c("r2", "adj_r2"))
  if (length(r2_cols) > 0L) {
    # A univariable-screen bundle stands for the fits it wraps: with
    # `multivariable = FALSE` the bundle is the ONLY entry in `models`,
    # so reading its class alone would refuse the linear screen -- the
    # very table the column is for. Same unwrapped set as the class
    # buckets above.
    eff_fits <- eff_models
    any_ols <- any(vapply(
      eff_fits,
      function(f) inherits(f, "lm") && !inherits(f, "glm"),
      logical(1)
    ))
    if (!any_ols && all_gam) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_columns` {are} not defined for `mgcv` GAMs.",
            r2_cols
          ),
          "i" = paste0(
            "The column reports the R\u00B2 of each fit of a univariable ",
            "screen of least-squares models. For a GAM, ask for a ",
            "fit-statistics row in `show_fit_stats`."
          ),
          .gam_r2_hint(eff_fits)
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    if (!any_ols) {
      eff_classes <- unique(vapply(eff_fits, function(f) class(f)[1L], ""))
      any_glm_eff <- any(vapply(eff_fits, inherits, logical(1), "glm"))
      spicy_abort(
        c(
          sprintf(
            .token_msg(
              "{Tokens} in `show_columns` {are} not defined for %s models.",
              r2_cols
            ),
            .fmt_and(sprintf("`%s`", eff_classes))
          ),
          "i" = paste0(
            "The column reports the classical R\u00B2 of the ",
            "least-squares variance partition. Outside it only ",
            "pseudo-R\u00B2 measures exist (McFadden 1974, Cox & Snell ",
            "1989, Nagelkerke 1991, Tjur 2009); they disagree with one ",
            "another, so spicy does not print one under an R\u00B2 ",
            "header without naming it."
          ),
          "i" = if (any_glm_eff) {
            paste0(
              "Name the one you want as a fit-statistics row, e.g. ",
              "`show_fit_stats = c(\"nobs\", \"pseudo_r2_mcfadden\")`."
            )
          } else {
            paste0(
              "Report the model's own summary instead (for `coxph`, the ",
              "likelihood-ratio test, and the concordance, a row with ",
              "`show_fit_stats = \"concordance\"`)."
            )
          }
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # Bayesian fit statistics need at least one Bayesian model to
  # compute from; in a MIXED table the frequentist models render a
  # blank cell, mirroring how "r2" renders blank for the Bayesian
  # side (the shipped p-dash policy, applied in both directions).
  bayes_fit_req <- intersect(
    show_fit_stats,
    c("r2_bayes", "elpd_loo", "looic", "waic")
  )
  if (length(bayes_fit_req) > 0L) {
    none_bayes <- length(models) == 0L ||
      !any(vapply(models, inherits, logical(1), c("stanreg", "brmsfit")))
    if (none_bayes) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_fit_stats` {are} defined only for Bayesian fits.",
            bayes_fit_req
          ),
          "i" = paste0(
            "They are computed from the posterior draws ",
            "(Bayesian R\u00B2; PSIS-LOO)."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    # The predictive-accuracy trio computes through the loo package
    # (Suggests). Refuse upfront rather than letting the frame's
    # tryCatch degrade the request to a silently absent row.
    loo_req <- intersect(bayes_fit_req, c("elpd_loo", "looic", "waic"))
    if (length(loo_req) > 0L && !spicy_pkg_available("loo")) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_fit_stats` {need} the loo package.",
            loo_req
          ),
          "i" = "Install loo: `install.packages(\"loo\")`."
        ),
        class = "spicy_missing_pkg",
        call = call
      )
    }
  }

  # p / t on an all-Bayesian table: there is nothing to fill the
  # column with (a dash column carries no information). Group tokens
  # ("all_b", ...) expand WITHOUT p for Bayesian fits, so reaching
  # here means the user typed the atomic token.
  all_bayes_cols <- length(models) > 0L &&
    all(vapply(models, inherits, logical(1), c("stanreg", "brmsfit")))
  if (all_bayes_cols && any(c("p", "t", "ame_p") %in% show_columns)) {
    spicy_abort(
      c(
        .token_msg(
          "{Tokens} in `show_columns` {are} not defined for Bayesian fits.",
          intersect(c("p", "t", "ame_p"), show_columns)
        ),
        "i" = paste0(
          "A posterior has no p-value or t-statistic (for ",
          "coefficients or AMEs). The probability of ",
          "direction (`\"pd\"`) is the closest posterior ",
          "summary."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  # Probability of direction: needs at least one Bayesian model; in a
  # mixed table the frequentist cells dash, exactly mirroring how the
  # p column dashes for the Bayesian model (D2/D3 policy).
  if ("pd" %in% show_columns) {
    any_bayes_pd <- length(models) > 0L &&
      any(vapply(models, inherits, logical(1), c("stanreg", "brmsfit")))
    if (!any_bayes_pd) {
      spicy_abort(
        c(
          'Token `"pd"` in `show_columns` is defined only for Bayesian fits.',
          "i" = paste0(
            "The probability of direction is a posterior-draw ",
            "summary (share of draws on the dominant side of ",
            "zero); frequentist fits have none."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # Per-parameter sampler diagnostics: all-Bayesian tables only. In a
  # mixed table the columns would dash for most rows while the
  # automatic convergence guard ALREADY reports per-model sampler
  # problems in the footer, so the strict gate points there.
  diag_cols <- intersect(
    show_columns,
    c("rhat", "ess_bulk", "ess_tail", "mcse")
  )
  if (length(diag_cols) > 0L) {
    all_bayes_diag <- length(models) > 0L &&
      all(vapply(models, inherits, logical(1), c("stanreg", "brmsfit")))
    if (!all_bayes_diag) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_columns` {are} shown only when every model is Bayesian.",
            diag_cols
          ),
          "i" = paste0(
            "R-hat, ESS and MCSE are sampler diagnostics of ",
            "the posterior draws; frequentist fits have none."
          ),
          "i" = paste0(
            "In a mixed table the automatic convergence guard ",
            "already reports sampler problems per model in ",
            "the footer."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # Bayesian fits (finding a + b of the Bayesian-vignette recon):
  # a posterior has no p-values to adjust and no likelihood-based
  # information criteria; both requests were silently ignored before.
  all_bayes <- length(models) > 0L &&
    all(vapply(models, inherits, logical(1), c("stanreg", "brmsfit")))
  if (all_bayes) {
    bad_fit <- intersect(
      show_fit_stats,
      c(
        "aic",
        "aicc",
        "bic",
        "deviance",
        "log_lik",
        "r2",
        "adj_r2",
        "omega2",
        "f2",
        "pseudo_r2_mcfadden",
        "pseudo_r2_nagelkerke",
        "pseudo_r2_tjur",
        "r2_deviance",
        "sigma",
        "rmse"
      )
    )
    if (length(bad_fit) > 0L) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_fit_stats` {are} not defined for Bayesian fits.",
            bad_fit
          ),
          "i" = paste0(
            "Posterior fits have no likelihood-based information ",
            "criteria or classical R\u00B2; compare models with ",
            "`loo::loo()` / `loo::loo_compare()` outside the table."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # Dispersion and precision tokens: theta / alpha for NB2 fits
  # (MASS::glm.nb, lme4::glmer.nb, fixest fenegbin, mgcv nb(), pscl
  # negbin, glmmTMB nbinom2 / truncated_nbinom2), delta for NB1 fits
  # (glmmTMB nbinom1 / truncated_nbinom1), phi for beta fits (betareg,
  # mgcv betar(), glmmTMB beta / betabinomial / ordbeta). ANY-gates,
  # like the GEE tokens below: Poisson beside NB2, or NB1 beside NB2, is the
  # comparison these rows are for, and a column whose model lacks the
  # parameter renders blank. Refused when no model in the table carries
  # it -- silently dropping the row would hide the request (pre-1.0
  # policy) -- and when a fit of the token's family has dispersion
  # covariates, so its parameter is not one number.
  nb2_tokens <- intersect(show_fit_stats, c("theta", "alpha"))
  if (length(nb2_tokens) > 0L) {
    # A fit of the family whose dispersion has covariates is refused for
    # that reason first; otherwise the registry says which models carry
    # the rows (.reg_offers(); theta and alpha share their conditions).
    is_nb2 <- vapply(models, .is_nb2_fit, logical(1))
    .abort_modeled_dispersion(models[is_nb2], nb2_tokens, "theta", call)
    if (!.reg_any_offers(models, nb2_tokens[1L])) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_fit_stats` {are} defined only for NB2 negative-binomial fits (`MASS::glm.nb()`, `lme4::glmer.nb()`, `fixest::fenegbin()`, mgcv `nb()`, pscl `zeroinfl()` or `hurdle()` with `dist = \"negbin\"`, glmmTMB `nbinom2`), and no model in the table is one.",
            nb2_tokens
          ),
          "i" = paste0(
            "theta is the NB2 dispersion (V = mu + mu\u00B2/theta) and ",
            "alpha its reciprocal. An NB1 fit (glmmTMB `nbinom1`) ",
            "reports `\"delta\"` instead; other families have no such ",
            "parameter."
          ),
          "i" = paste0(
            "A theta fixed in the call (`negative.binomial(theta)` in ",
            "`glm()`, mgcv `negbin(theta)` or `nb(theta)`) is not an ",
            "estimate and has no row."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  if ("delta" %in% show_fit_stats) {
    # The dispersion with covariates first, then the registry, as for
    # theta above.
    is_nb1 <- vapply(models, .is_nb1_fit, logical(1))
    .abort_modeled_dispersion(models[is_nb1], "delta", "delta", call)
    if (!.reg_any_offers(models, "delta")) {
      spicy_abort(
        c(
          paste0(
            "Token `\"delta\"` in `show_fit_stats` is defined only for NB1 ",
            "negative-binomial fits (glmmTMB `nbinom1`, ",
            "`truncated_nbinom1`), and no model in the table is one."
          ),
          "i" = paste0(
            "delta is the NB1 dispersion (V = mu(1 + delta), Stata's ",
            "`nbreg, dispersion(constant)`). An NB2 fit reports `\"theta\"` ",
            "and `\"alpha\"` instead; other families have no such parameter."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  if ("phi" %in% show_fit_stats) {
    # The precision with covariates first, then the registry, as for
    # theta above.
    is_beta <- vapply(models, .is_beta_fit, logical(1))
    variable_prec <- vapply(
      models[is_beta],
      function(m) {
        if (inherits(m, "glmmTMB")) {
          return(!.glmmTMB_constant_dispersion(m))
        }
        # mgcv's betar() has one precision, never covariates.
        if (inherits(m, "gam")) {
          return(FALSE)
        }
        length(tryCatch(
          stats::coef(m, model = "precision"),
          error = function(e) numeric(0)
        )) !=
          1L
      },
      logical(1)
    )
    if (any(variable_prec)) {
      spicy_abort(
        c(
          paste0(
            "Token `\"phi\"` needs a constant precision, but a ",
            "model has covariates on the precision ",
            "(`y ~ x | z`, or a glmmTMB `dispformula`)."
          ),
          "i" = paste0(
            "With precision covariates, phi is not a single number; ",
            "inspect the precision coefficients with ",
            "`summary(fit)` instead."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    if (!.reg_any_offers(models, "phi")) {
      spicy_abort(
        c(
          paste0(
            "Token `\"phi\"` in `show_fit_stats` is defined only ",
            "for fits with a beta precision (`betareg::betareg()`, ",
            "mgcv `betar()`, glmmTMB `beta_family()`, `betabinomial()`, ",
            "`ordbeta()`), and no model in the table is one."
          ),
          "i" = paste0(
            "In beta regression, Var(y) = mu(1-mu)/(1+phi): higher phi = ",
            "less dispersion. For `betabinomial()` and `ordbeta()`, phi is ",
            "the precision parameter of the beta part. Other families have ",
            "no such parameter."
          ),
          "i" = paste0(
            "A phi fixed in the call (mgcv `betar(theta)` with theta > 0) ",
            "is not an estimate and has no row."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # Absorbed-fixed-effects disclosure tokens. Deliberately an ANY-gate
  # (like the dispersion tokens above; n_groups is ungated): the Fixed
  # effects block is a cross-model design disclosure, so a mixed lm +
  # fixest table is exactly where it is most useful -- columns without
  # the concept render blank cells.
  fixest_tokens <- intersect(show_fit_stats, c("fixed_effects", "within_r2"))
  if (length(fixest_tokens) > 0L) {
    any_fe <- length(models) > 0L &&
      any(vapply(models, absorbs_fixed_effects, logical(1)))
    if (!any_fe) {
      spicy_abort(
        c(
          .token_msg(
            paste0(
              "{Tokens} in `show_fit_stats` {are} defined only for fits ",
              "that absorb fixed effects (fixest, or estimatr with ",
              "`fixed_effects =`)."
            ),
            fixest_tokens
          ),
          "i" = paste0(
            "`\"fixed_effects\"` discloses the absorbed fixed effects ",
            "(one Yes/No row per factor) and `\"within_r2\"` is the ",
            "FE-partialed R\u00B2; other classes have neither."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # GEE fit-stat tokens. ANY-gate like the fixest disclosure tokens:
  # in a mixed glm + GEE table, QIC next to the glm's AIC is exactly
  # the comparison the table is for -- non-GEE columns render blank.
  gee_tokens <- intersect(
    show_fit_stats,
    c("qic", "qicu", "scale", "max_cluster_size")
  )
  if (length(gee_tokens) > 0L) {
    # The registry says which models carry them (.reg_offers()); the four
    # tokens are listed together, by the geeglm cell alone.
    if (!.reg_any_offers(eff_models, gee_tokens[1L])) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_fit_stats` {are} defined only for GEE fits (`geepack::geeglm`).",
            gee_tokens
          ),
          "i" = paste0(
            "`\"qic\"` / `\"qicu\"` are the quasi-likelihood information ",
            "criteria (Pan 2001), `\"scale\"` the estimated scale ",
            "(dispersion) parameter, and `\"max_cluster_size\"` the ",
            "largest cluster in the estimation sample."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # GEE is quasi-likelihood: no likelihood, so no (pseudo-)R-squared,
  # no likelihood-based information criteria, and no partial LRT /
  # variance-explained columns. Reject only when ALL models are GEE
  # (a mixed table renders the GEE cells blank for the glm's stats).
  all_gee <- length(models) > 0L && all(gee_flags)
  if (all_gee) {
    bad_fit <- intersect(
      show_fit_stats,
      c(
        "r2",
        "adj_r2",
        "omega2",
        "f2",
        "pseudo_r2_mcfadden",
        "pseudo_r2_nagelkerke",
        "pseudo_r2_tjur",
        "r2_deviance",
        "aic",
        "aicc",
        "bic",
        "deviance",
        "log_lik"
      )
    )
    if (length(bad_fit) > 0L) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_fit_stats` {are} not defined for GEE fits (`geeglm`).",
            bad_fit
          ),
          "i" = paste0(
            "GEE is estimated by quasi-likelihood: no likelihood, so ",
            "no (pseudo-)R\u00B2 and no likelihood-based ",
            "information criteria."
          ),
          "i" = "Use `\"qic\"` / `\"qicu\"` (Pan 2001) instead."
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    bad_cols <- intersect(show_columns, .PARTIAL_ES_TOKENS)
    if (length(bad_cols) > 0L) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_columns` {are} not defined for GEE fits (`geeglm`).",
            bad_cols
          ),
          "i" = paste0(
            "The partial likelihood-ratio chi-square, the partial F and ",
            "the variance-explained partials need a true likelihood, a ",
            "deviance or an OLS variance partition; GEE has none of them."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # Mixed-effects fits match neither all_glm nor all_lm, so lm-only tokens would
  # otherwise slip through and render an empty row (finding m4). Classical /
  # pseudo R^2 and the least-squares partial-variance tokens are undefined for
  # mixed models; point the user at the Nakagawa marginal / conditional R^2.
  # Reject only when ALL models are mixed (a mixed set en-dashes the RE rows).
  all_mixed <- length(models) > 0L &&
    all(vapply(
      models,
      inherits,
      logical(1),
      c("merMod", "lmerModLmerTest", "glmmTMB", "lme", "clmm")
    ))
  if (all_mixed) {
    bad_fit <- intersect(
      show_fit_stats,
      c(
        "r2",
        "adj_r2",
        "omega2",
        "f2",
        "pseudo_r2_mcfadden",
        "pseudo_r2_nagelkerke",
        "pseudo_r2_tjur",
        "r2_deviance"
      )
    )
    if (length(bad_fit) > 0L) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_fit_stats` {are} not defined for mixed-effects models.",
            bad_fit
          ),
          "i" = "Classical / pseudo R\u00B2 do not apply to mixed-effects models.",
          "i" = .fit_stats_remedy(
            eff_models,
            c("r2_marginal", "r2_conditional"),
            "Use the Nakagawa R\u00B2 instead: %s (Nakagawa et al. 2017)."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    # partial_chi2 (the Type-II Wald chi-square for mixed fits) IS defined
    # here, so it is deliberately NOT rejected -- only the variance-explained
    # (least-squares) partials are undefined for mixed models.
    bad_cols <- intersect(show_columns, .PARTIAL_VARIANCE_ES_TOKENS)
    if (length(bad_cols) > 0L) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_columns` {are} not defined for mixed-effects models.",
            bad_cols
          ),
          "i" = "Variance-explained partials do not apply to mixed models.",
          # clmm has no term-level test: no remedy it would refuse.
          "i" = if (!any(vapply(eff_models, inherits, logical(1), "clmm"))) {
            paste0(
              "For a partial Wald chi-square test of each term, use ",
              "`\"partial_chi2\"`."
            )
          }
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # AME is not defined for Cox proportional hazards models: avg_slopes() on a
  # coxph / cph fit returns effects on an ambiguous survival/hazard scale, and
  # marginaleffects itself warns its delta-method standard errors are
  # unreliable for Cox. The canonical Cox report is the hazard ratio
  # (exponentiate = TRUE). Reject only when ALL models are Cox; a mixed set
  # en-dashes the Cox AME rows (the AME extraction skips Cox fits).
  all_cox <- length(models) > 0L &&
    all(vapply(models, inherits, logical(1), c("coxph", "cph")))
  if (all_cox) {
    bad <- intersect(show_columns, .AME_COLUMN_TOKENS)
    if (length(bad) > 0L) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_columns` {are} not defined for Cox models.",
            bad
          ),
          "i" = paste0(
            "Average marginal effects on a Cox proportional-hazards fit are ",
            "on an ambiguous survival / hazard scale with unreliable standard ",
            "errors. Report hazard ratios instead with `exponentiate = TRUE`."
          ),
          # A DESIGN-based Cox has no absolute-effect columns either --
          # their bootstrap resamples rows and so ignores the strata and
          # clusters -- so it is pointed at survey's own tools instead of
          # at a second refusal.
          "i" = if (all(vapply(models, .is_design_fit, logical(1)))) {
            paste0(
              "For a design-based Cox model, test a term with ",
              "`survey::regTermTest(fit, ~term)` and read a marginal ",
              "survival curve with `survey::svykm()`."
            )
          } else if (any(vapply(models, inherits, logical(1), "rms"))) {
            # rms::cph has no estimand columns either: no remedy it would
            # refuse.
            NULL
          } else {
            paste0(
              "For absolute effects, use the survival estimand columns: ",
              "`show_columns = c(\"b\", \"rmst\")` with `tau = `, or ",
              "`\"risk_diff\"` with `at_time = ` (coxph)."
            )
          }
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # A GAM has the pseudo-R^2 of its glm twin (`.gam_pseudo_r2()`): Tjur's
  # for a binomial, McFadden's and Nagelkerke's for a binomial, Poisson
  # or nb() family. A table of GAMs none of which has the measure is
  # answered here in its own words, before the glm gates below, which
  # name a glm; beside other models the GAM cell is dashed.
  if (all_gam) {
    kinds <- vapply(eff_models, .glm_r2_kind, character(1))
    bad_fit <- c(
      if (!any(kinds == "binomial")) {
        intersect(show_fit_stats, "pseudo_r2_tjur")
      },
      if (!any(kinds %in% c("binomial", "count"))) {
        intersect(
          show_fit_stats,
          c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke")
        )
      }
    )
    if (length(bad_fit) > 0L) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_fit_stats` {are} not defined for the families of these `mgcv` GAMs.",
            bad_fit
          ),
          "i" = paste0(
            "A GAM has the R\u00B2 of the glm of its family. Tjur's needs ",
            "a binomial family, McFadden's and Nagelkerke's a binomial, ",
            "Poisson or negative-binomial one."
          ),
          .gam_r2_hint(eff_models)
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # A design-based fit has no likelihood: its frame posts the deviance,
  # the BIC and the AICc as NA (survey's own deviance() is a sign-flipped
  # LRT on one engine and 0 on the other), and the AIC too outside
  # svyglm, so the rows used to render nothing. Refused when every model
  # is a design fit. The pseudo-R^2 and the deviance R^2 likewise: the
  # frame computes none, and the glm path would treat the design weights
  # as frequency weights.
  if (
    length(eff_models) > 0L &&
      all(vapply(eff_models, .is_design_fit, logical(1)))
  ) {
    # The AIC is the design-based one of survey's extractAIC(), which only
    # svyglm has; none of the design classes carries an AICc.
    any_svyglm <- any(vapply(eff_models, inherits, logical(1), "svyglm"))
    bad_fit <- intersect(
      show_fit_stats,
      c(
        "deviance",
        "bic",
        "aicc",
        if (!any_svyglm) "aic",
        "pseudo_r2_mcfadden",
        "pseudo_r2_nagelkerke",
        "pseudo_r2_tjur",
        "r2_deviance"
      )
    )
    if (length(bad_fit) > 0L) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_fit_stats` {are} not defined for survey-design fits.",
            bad_fit
          ),
          "i" = paste0(
            "A design-based fit is estimated without a likelihood.",
            if (any_svyglm) {
              paste0(
                " For `svyglm()`, `\"aic\"` gives the design-based AIC of ",
                "survey's `extractAIC()`."
              )
            }
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # The concordance of a Cox model, from `survival`'s own computation.
  # ANY-gate, like Tjur's below. rms::cph inherits coxph but is not
  # covered: neither survival::concordance() on a cph fit nor
  # (Dxy + 1) / 2 gives the C of its coxph twin when the model has strata.
  # The registry says which models carry it (.reg_offers()).
  if (
    "concordance" %in%
      show_fit_stats &&
      !.reg_any_offers(eff_models, "concordance")
  ) {
    spicy_abort(
      c(
        paste0(
          "Token `\"concordance\"` in `show_fit_stats` is defined only for ",
          "`survival::coxph()` and `survey::svycoxph()` fits, and no ",
          "model in the table is one."
        ),
        "i" = paste0(
          "It is Harrell's C of the fit, the value `summary()` of a ",
          "`coxph` model reports."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  # Tjur's R^2 is defined only for binary outcomes (binomial-family glm;
  # Tjur 2009 "coefficient of discrimination"). ANY-gate, aligned on the
  # theta / alpha dispersion precedent: when NO model in the set can
  # carry the statistic, an explicit request is refused up front --
  # silently dropping the row (the pre-2026-08 behavior) hid the
  # request. A mixed set with at least one binomial glm passes: the
  # non-binomial columns render the per-cell en-dash. Runs AFTER the
  # family gates above so homogeneous non-glm sets (all-lm, all-GEE,
  # all-mixed, all-Bayesian) keep their more specific refusal wording.
  if ("pseudo_r2_tjur" %in% show_fit_stats) {
    # A binomial GAM carries Tjur, as its glm twin.
    is_binom_glm <- glm_flags &
      !vapply(
        eff_models,
        inherits,
        logical(1),
        c("stanreg", "brmsfit")
      ) &
      vapply(
        eff_models,
        function(m) {
          fam <- tryCatch(
            stats::family(m)$family,
            error = function(e) NA_character_
          )
          identical(fam, "binomial")
        },
        logical(1)
      )
    # A binary rms::lrm, the logit its glm twin is.
    is_binary_lrm <- vapply(
      eff_models,
      function(m) inherits(m, "lrm") && length(.lrm_levels(m)) == 2L,
      logical(1)
    )
    if (!any(is_binom_glm | is_binary_lrm)) {
      spicy_abort(
        c(
          paste0(
            "Token `\"pseudo_r2_tjur\"` in `show_fit_stats` is defined only ",
            "for binomial models (binary outcomes), and no model in the ",
            "table is a binomial-family `glm` or a binary `rms::lrm()`."
          ),
          "i" = paste0(
            "Tjur's R\u00B2 (2009) is mean(fitted | y = 1) - ",
            "mean(fitted | y = 0), which needs a binary outcome (the ",
            "trials of a grouped binomial response count as such)."
          ),
          "i" = .fit_stats_remedy(
            eff_models,
            c("pseudo_r2_mcfadden", "r2_deviance"),
            "Use %s for these models."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }

  # The pseudo-R^2 of a beta regression, which betareg alone computes.
  # ANY-gate: the other columns of a mixed set take an empty cell. The
  # registry says which models carry it (.reg_offers()).
  if (
    "pseudo_r2_ferrari" %in%
      show_fit_stats &&
      !.reg_any_offers(eff_models, "pseudo_r2_ferrari")
  ) {
    spicy_abort(
      c(
        paste0(
          "Token `\"pseudo_r2_ferrari\"` in `show_fit_stats` is defined only ",
          "for beta regressions fitted with `betareg::betareg()`."
        ),
        "i" = paste0(
          "It is the squared correlation between the linear predictor ",
          "and the link-transformed response (Ferrari & Cribari-Neto ",
          "2004), which `summary()` of a betareg fit reports."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  # McFadden's and Nagelkerke's R^2 of a continuous family (gaussian,
  # Gamma, inverse Gaussian, Tweedie) change with the unit of y: the
  # log-likelihood of a density moves when y is rescaled, and their ratio
  # and Nagelkerke's bound move with it (Cameron & Windmeijer 1997,
  # p. 333). Refused when every model of the table is such a glm; beside
  # other models the cell is dashed, as for the other class refusals.
  lr_req <- intersect(
    show_fit_stats,
    c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke")
  )
  if (
    length(lr_req) > 0L &&
      length(eff_models) > 0L &&
      all(vapply(eff_models, .glm_is_continuous, logical(1)))
  ) {
    spicy_abort(
      c(
        .token_msg(
          paste0(
            "{Tokens} in `show_fit_stats` {are} not defined for a `glm` ",
            "or GAM of a continuous family (gaussian, Gamma, inverse ",
            "Gaussian, Tweedie)."
          ),
          lr_req
        ),
        "i" = paste0(
          "They are built on the log-likelihood of a density, which ",
          "changes with the unit of the outcome: the same model gives ",
          "other values when y is rescaled."
        ),
        "i" = paste0(
          "Use `\"r2_deviance\"`, 1 - deviance / null deviance, which does ",
          "not depend on the unit (for a gaussian `glm` with the identity ",
          "link, it is the R\u00B2 of `lm()`)."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  # The deviance R^2 needs a glm family other than the binomial. On
  # Bernoulli trials, and for ordinal and multinomial responses, the
  # saturated log-likelihood is 0 and the deviance R^2 is McFadden's
  # (Cameron & Windmeijer 1997, p. 333 and 337): refused there rather
  # than printed twice under two names. ANY-gate, like Tjur's above. An
  # mgcv GAM follows its glm twin: a binomial GAM is pointed at McFadden.
  if (
    "r2_deviance" %in%
      show_fit_stats &&
      !any(vapply(eff_models, .glm_carries_r2_deviance, logical(1)))
  ) {
    categorical <- any(vapply(
      eff_models,
      function(m) {
        inherits(m, c("polr", "clm", "multinom", "lrm")) ||
          (inherits(m, "glm") &&
            identical(.glm_r2_kind(m), "binomial"))
      },
      logical(1)
    ))
    spicy_abort(
      c(
        paste0(
          "Token `\"r2_deviance\"` in `show_fit_stats` is defined only for ",
          "`glm` and `mgcv` GAM families other than the binomial, and no ",
          "model in the table is one."
        ),
        "i" = if (categorical) {
          paste0(
            "For a binomial, ordinal or multinomial response, the deviance ",
            "R\u00B2 equals McFadden's R\u00B2: use `\"pseudo_r2_mcfadden\"`."
          )
        } else {
          paste0(
            "It is 1 - deviance / null deviance of a `glm` or a GAM of a ",
            "gaussian, Gamma, inverse Gaussian, Tweedie, Poisson, negative ",
            "binomial or quasi family."
          )
        }
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  # AME is not computable for mlogit: marginaleffects supports predictions()
  # but NOT slopes() for its one-row-per-choice data structure, so avg_slopes()
  # errors. Reject "ame" tokens up front (when ALL models are mlogit) instead of
  # rendering a blank column.
  all_mlogit <- length(models) > 0L &&
    all(vapply(models, inherits, logical(1), "mlogit"))
  if (all_mlogit) {
    bad <- intersect(show_columns, .AME_COLUMN_TOKENS)
    if (length(bad) > 0L) {
      spicy_abort(
        c(
          .token_msg(
            "{Tokens} in `show_columns` {are} not available for `mlogit` models.",
            bad
          ),
          "i" = paste0(
            "marginaleffects computes predictions but not slopes (average ",
            "marginal effects) for mlogit's one-row-per-choice data structure."
          )
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }
  invisible(NULL)
}

# Step 10: show_fit_stats. `FALSE` drops the fit-statistic rows -- a
# legitimate rendering choice (some users prefer the body alone) --
# and `NULL` is the class-aware-default sentinel; the two are not
# interchangeable, and `character(0)` is neither, having been removed.
# The body below is where that contract is stated and enforced.
# show_columns has no analogous escape hatch, because a table with
# zero data columns is nonsensical.
validate_show_fit_stats <- function(
  show_fit_stats,
  call = rlang::caller_env()
) {
  # Phase 7c23 (item a): the explicit "suppress fit-stats" alias is
  # `FALSE` (parity with `show_re = FALSE` / `outcome_labels = FALSE`).
  # `NULL` is the "class-aware default" sentinel. The previous
  # `character(0)` synonym was removed (no back-compat carried at the
  # pre-1.0 stage per the "hard errors over silent changes" policy).
  if (is.null(show_fit_stats) || isFALSE(show_fit_stats)) {
    return(invisible(NULL))
  }
  # Migration error for the 0.12 uppercase information-criterion
  # tokens (renamed in 0.13 for coherence with the lowercase
  # aic_change / aicc_change / bic_change tokens). Hard error, not a
  # silent alias, per the pre-1.0 policy; rendered row labels are
  # unchanged ("AIC" / "AICc" / "BIC").
  legacy_ic <- intersect(as.character(show_fit_stats), c("AIC", "AICc", "BIC"))
  if (length(legacy_ic) > 0L) {
    spicy_abort(
      c(
        paste0(
          "Uppercase `show_fit_stats` token(s) used; spicy 0.13 ",
          "switched the information criteria to lowercase."
        ),
        "i" = paste0(
          "Replacement(s): ",
          paste(
            sprintf("`\"%s\"` -> `\"%s\"`", legacy_ic, tolower(legacy_ic)),
            collapse = ", "
          ),
          "."
        ),
        "i" = "Rendered row labels are unchanged (AIC / AICc / BIC)."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  validate_token_vector(
    show_fit_stats,
    .regression_tokens$show_fit_stats,
    arg = "show_fit_stats",
    call = call
  )
}

# Generic token-vector validator used by show_*, nested_stats.
# Checks: character non-empty + no duplicates + all tokens in `valid`.
# Expand `show_columns` group tokens to atomic tokens in place. Runs
# before validation so the rest of the pipeline sees only atomic
# tokens. Emits a migration error for legacy uppercase tokens from
# spicy <= 0.11 (`"B"`, `"AME"`, ...) pointing at the new name.
expand_show_columns <- function(
  tokens,
  bayesian = FALSE,
  call = rlang::caller_env()
) {
  if (is.null(tokens) || length(tokens) == 0L) {
    return(tokens)
  }
  tokens <- as.character(tokens)
  # Migration error: uppercase legacy tokens.
  legacy_used <- intersect(tokens, names(.show_columns_legacy))
  if (length(legacy_used)) {
    msgs <- vapply(
      legacy_used,
      function(old) {
        new_tok <- .show_columns_legacy[[old]]
        sprintf(
          "  `\"%s\"` -> %s",
          old,
          paste0("`", .quote_val(new_tok), "`", collapse = " + ")
        )
      },
      character(1)
    )
    spicy_abort(
      c(
        paste0(
          "Legacy uppercase `show_columns` token(s) used; ",
          "spicy 0.12 switched to lowercase atomic tokens."
        ),
        "i" = "Replacement(s):",
        msgs,
        "i" = paste0(
          "Or use a group token: `\"all_b\"` / ",
          "`\"all_b_compact\"` / `\"all_ame\"` / ",
          "`\"all_f2\"` / `\"all_eta2\"` / `\"all_omega2\"`."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  # Group expansion.
  group_idx <- which(tokens %in% names(.show_columns_groups))
  if (length(group_idx)) {
    expanded <- list()
    for (i in seq_along(tokens)) {
      tok <- tokens[i]
      if (tok %in% names(.show_columns_groups)) {
        grp <- .show_columns_groups[[tok]]
        # All-Bayesian tables have no p-values or t-statistics -- for
        # coefficients or for AMEs: the group presets expand WITHOUT
        # them (BARG default: estimate + credible interval). An
        # explicitly typed atomic "p" / "ame_p" still hard-errors
        # downstream.
        if (isTRUE(bayesian)) {
          grp <- setdiff(grp, c("p", "t", "ame_p"))
        }
        expanded[[i]] <- grp
      } else {
        expanded[[i]] <- tok
      }
    }
    tokens <- unlist(expanded, use.names = FALSE)
  }
  # Dedup while preserving first-occurrence order.
  tokens[!duplicated(tokens)]
}

validate_token_vector <- function(x, valid, arg, call = rlang::caller_env()) {
  if (!is.character(x) || length(x) == 0L) {
    spicy_abort(
      sprintf("`%s` must be a non-empty character vector.", arg),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (anyNA(x) || any(!nzchar(x))) {
    spicy_abort(
      sprintf(
        "`%s` must not contain NA or empty strings.",
        arg
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (anyDuplicated(x)) {
    dups <- unique(x[duplicated(x)])
    spicy_abort(
      sprintf(
        "`%s` contains the duplicate %s.",
        arg,
        .token_msg("{tokens}", dups)
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  bad <- setdiff(x, valid)
  if (length(bad) > 0L) {
    spicy_abort(
      c(
        sprintf(
          "%s in `%s`.",
          .token_msg("Unknown {tokens}", bad),
          arg
        ),
        "i" = sprintf("Valid tokens: %s.", .fmt_tokens(valid))
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}


# ---- Phase D \u2013 argument value validation ----------------------------------

# Steps 13-14: enum args. table_regression() resolves `standardized`,
# `intercept_position`, `align`, `output`, `reference_style`, ... via
# spicy_match_arg() (R/abort.R): same matching semantics as
# match.arg(), but an invalid value raises a classed
# `spicy_invalid_input` naming the argument and its valid values, so
# the documented `tryCatch(spicy_error = ...)` catch-all applies.

# Phase 7c7d: validate the `re_columns` argument of table_regression().
# Accepts a character vector with elements from c("est", "se", "ci");
# duplicates and unknown tokens error early. "est" is always required
# (the table without an estimate column is meaningless).
.validate_re_columns <- function(x, call = rlang::caller_env()) {
  valid <- c("est", "se", "ci")
  validate_token_vector(x, valid, arg = "re_columns", call = call)
  if (!"est" %in% x) {
    spicy_abort(
      "`re_columns` must include \"est\" (the random-effect estimate).",
      class = "spicy_invalid_input",
      call = call
    )
  }
  x
}


# Validate the `re_scale` argument of table_regression(): "sd" (default) or
# "variance". Errors early with a spicy_invalid_input (not a raw base-R
# match.arg message), and BEFORE the expensive extraction loop (finding m3).
.validate_re_scale <- function(x, call = rlang::caller_env()) {
  choices <- c("sd", "variance")
  if (identical(x, choices)) {
    return("sd")
  } # unset default vector
  if (length(x) == 1L && !is.na(x) && x %in% choices) {
    return(x)
  }
  spicy_abort(
    c(
      "`re_scale` must be one of \"sd\" or \"variance\".",
      "x" = sprintf(
        "You supplied %s.",
        paste(.quote_val(x), collapse = ", ")
      )
    ),
    class = "spicy_invalid_input",
    call = call
  )
}


# Step 15: digit args (whole-number scalar). The one validator of every
# number of decimals in the table family: `digits`, `p_digits`,
# `effect_size_digits`, `fit_digits`, `ic_digits`, `percent_digits`.
# A fraction is refused rather than truncated, and a name rather than
# dropped (`.digit_arg_message()`). `min = 1` for
# `p_digits`: a p-value printed with no decimal reads ".015" in the table
# and "<1" in `inline()`. Returns the value as an integer.
validate_digit_arg <- function(
  x,
  name,
  min = 0L,
  call = rlang::caller_env()
) {
  ok <- length(x) == 1L &&
    is.numeric(x) &&
    is.null(names(x)) &&
    !is.na(x) &&
    is.finite(x) &&
    x >= min &&
    x <= .Machine$integer.max &&
    x == trunc(x)
  if (!ok) {
    spicy_abort(
      .digit_arg_message(name, min, x),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(as.integer(x))
}

# The one wording of a number-of-decimals check, shared with the levers of
# `spicy_style()` (`.style_check_count()`): only `min = 0` and `min = 1`
# occur. `x`, the value refused, adds what a name means. A named `digits`
# gives each variable its own decimals in the three continuous tables
# (`.continuous_digits_arg()`) and nowhere else: read as one number with
# its name dropped, it would mean the opposite of what it says.
.digit_arg_message <- function(name, min = 0L, x = NULL) {
  msg <- if (min == 0L) {
    sprintf("`%s` must be a non-negative whole number.", name)
  } else {
    sprintf("`%s` must be a positive whole number.", name)
  }
  if (is.null(names(x))) {
    return(msg)
  }
  c(
    msg,
    "x" = sprintf("`%s` is named; give one unnamed number.", name),
    "i" = if (identical(name, "digits")) .DIGITS_NAMED_HINT
  )
}

.DIGITS_NAMED_HINT <- paste(
  "A named `digits`, one value per variable, is taken only by",
  "`table_continuous()`, `table_continuous_svy()` and `table_continuous_lm()`."
)

# Step 16: ci_level. The one check of a confidence level in the table
# family, the descriptive tables included.
validate_ci_level <- function(ci_level, call = rlang::caller_env()) {
  ok <- length(ci_level) == 1L &&
    is.numeric(ci_level) &&
    !is.na(ci_level) &&
    is.finite(ci_level) &&
    ci_level > 0 &&
    ci_level < 1
  if (!ok) {
    spicy_abort(
      "`ci_level` must be a number strictly between 0 and 1.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# Step 17: boot_n. The one check of a number of bootstrap replicates, in
# `table_regression()` (and `table_regression_uv()` through it),
# `table_continuous_lm()` and `table_multiresponse()`: a whole number of
# at least 1, a fraction refused rather than truncated, as for the
# numbers of decimals. No function has a floor of its own: the 50 that
# `table_continuous_lm()` required came with its bootstrap (d84ae365)
# and stated no constraint of the code. The constraint the code does
# have belongs to the bootstrap VARIANCE, whatever the function:
# `compute_resample_vcov_bootstrap()` needs 10 valid replicates.
# Returns the value as an integer.
validate_boot_n <- function(boot_n, call = rlang::caller_env()) {
  ok <- length(boot_n) == 1L &&
    is.numeric(boot_n) &&
    !is.na(boot_n) &&
    is.finite(boot_n) &&
    boot_n >= 1 &&
    boot_n <= .Machine$integer.max &&
    boot_n == trunc(boot_n)
  if (!ok) {
    spicy_abort(
      paste(
        "`boot_n` must be a positive whole number:",
        "the number of bootstrap replicates."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(as.integer(boot_n))
}

# The one wording of a `boot_n` typed in a call that runs no bootstrap.
# `applies_to` names what would use it in the calling function.
.warn_boot_n_ignored <- function(applies_to, ...) {
  .warn_ignored(
    "boot_n",
    sprintf("when no bootstrap runs: it applies to %s", applies_to),
    ...
  )
}

# Step 18: logical scalar args. The one check of a TRUE / FALSE switch in
# the table family: `show_intercept`, `nested`, and the switches of the
# descriptive tables.
validate_logical_scalar <- function(x, name, call = rlang::caller_env()) {
  ok <- length(x) == 1L && is.logical(x) && !is.na(x)
  if (!ok) {
    spicy_abort(
      sprintf("`%s` must be TRUE or FALSE.", name),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# Caption-style args (`title`, `note`). Three accepted forms:
#   NULL              -> auto-build (default)
#   FALSE             -> suppress
#   single character  -> override verbatim
# TRUE is rejected explicitly: it is grammatically suggestive of "yes
# show it" but carries no string payload, and silently mapping it to
# NULL would mask user typos (e.g. `title = TURE`).
validate_caption_arg <- function(x, name, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  if (isFALSE(x)) {
    return(invisible(NULL))
  }
  ok <- length(x) == 1L && is.character(x) && !is.na(x)
  if (!ok) {
    spicy_abort(
      c(
        sprintf("`%s` must be NULL, FALSE, or a single string.", name),
        "i" = "NULL = auto-build; FALSE = suppress; \"...\" = override."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# `p_adjust` validation. Must be one of stats::p.adjust.methods.
# We accept "fdr" as an alias for "BH" (matching stats::p.adjust
# itself) but normalize back to the canonical name in the orchestrator
# so the footer wording is consistent.
.spicy_p_adjust_methods <- c(
  "none",
  "holm",
  "hochberg",
  "hommel",
  "bonferroni",
  "BH",
  "BY",
  "fdr"
)

validate_p_adjust <- function(p_adjust, call = rlang::caller_env()) {
  if (length(p_adjust) != 1L || !is.character(p_adjust) || is.na(p_adjust)) {
    spicy_abort(
      "`p_adjust` must be a single string.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (!p_adjust %in% .spicy_p_adjust_methods) {
    spicy_abort(
      c(
        sprintf("Unknown `p_adjust` method: %s.", .quote_val(p_adjust)),
        "i" = sprintf(
          "Valid methods: %s.",
          paste(.quote_val(.spicy_p_adjust_methods), collapse = ", ")
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# `keep` / `drop` validation. Each is NULL (no filter) or a non-empty
# character vector with no NA / empty-string elements. They are
# mutually exclusive \u2013 supplying both raises an error rather than
# silently picking one.
validate_keep_drop <- function(keep, drop, call = rlang::caller_env()) {
  for (pair in list(c("keep", "keep"), c("drop", "drop"))) {
    arg_name <- pair[[1L]]
    val <- get(arg_name)
    if (is.null(val)) {
      next
    }
    if (
      !is.character(val) ||
        length(val) == 0L ||
        any(is.na(val)) ||
        any(!nzchar(val))
    ) {
      spicy_abort(
        sprintf(
          paste0(
            "`%s` must be NULL or a non-empty character vector ",
            "with no NA or empty-string elements."
          ),
          arg_name
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    # A pattern that does not compile as a regular expression may still
    # be the name of a term ("`a(`"): it is refused once the terms are
    # known, by `.reg_warn_dead_patterns()`.
  }
  if (!is.null(keep) && !is.null(drop)) {
    spicy_abort(
      c(
        "`keep` and `drop` are mutually exclusive.",
        "i" = paste0(
          "Pick one: `keep` to whitelist focal predictors, ",
          "`drop` to hide a few control variables."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# TRUE when `pattern` compiles as a Perl regular expression. grepl()
# warns before it errors on one that does not, so both are caught.
.is_valid_perl_regex <- function(pattern) {
  tryCatch(
    {
      grepl(pattern, "", perl = TRUE)
      TRUE
    },
    warning = function(w) FALSE,
    error = function(e) FALSE
  )
}

# Step 19: stars (FALSE | TRUE | named numeric vector with thresholds in (0,1])
validate_stars <- function(stars, call = rlang::caller_env()) {
  if (isFALSE(stars) || isTRUE(stars)) {
    return(invisible(NULL))
  }
  if (!is.numeric(stars) || length(stars) == 0L) {
    spicy_abort(
      c(
        "`stars` must be FALSE, TRUE, or a named numeric vector.",
        "i" = paste0(
          "Example: c(\"*\" = 0.05, \"**\" = 0.01, \"***\" = 0.001)."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  nms <- names(stars)
  if (is.null(nms) || any(!nzchar(nms)) || any(is.na(nms))) {
    spicy_abort(
      paste0(
        "When `stars` is a numeric vector, all elements must have ",
        "non-empty names (the symbols)."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (anyDuplicated(nms)) {
    spicy_abort(
      "Symbols in `stars` (vector names) must be unique.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (any(is.na(stars)) || any(stars <= 0) || any(stars > 1)) {
    spicy_abort(
      "Thresholds in `stars` must all be in (0, 1].",
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# Step 20: decimal_mark
validate_decimal_mark <- function(decimal_mark, call = rlang::caller_env()) {
  ok <- length(decimal_mark) == 1L &&
    is.character(decimal_mark) &&
    !is.na(decimal_mark) &&
    nchar(decimal_mark) == 1L
  if (!ok) {
    spicy_abort(
      "`decimal_mark` must be a single character (e.g. \".\" or \",\").",
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# Step 21: reference_label
validate_reference_label <- function(
  reference_label,
  call = rlang::caller_env()
) {
  # NULL is the default: the registry label, resolved by the caller.
  if (is.null(reference_label)) {
    return(invisible(NULL))
  }
  ok <- length(reference_label) == 1L &&
    is.character(reference_label) &&
    !is.na(reference_label) &&
    nzchar(reference_label)
  if (!ok) {
    spicy_abort(
      "`reference_label` must be a single non-empty string.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# Step 22: model_labels
validate_model_labels <- function(
  model_labels,
  models,
  call = rlang::caller_env()
) {
  if (is.null(model_labels)) {
    return(invisible(NULL))
  }
  if (
    !is.character(model_labels) ||
      any(is.na(model_labels)) ||
      any(!nzchar(model_labels))
  ) {
    spicy_abort(
      paste0(
        "`model_labels` must be NULL or a character vector with ",
        "non-empty values."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (length(model_labels) != length(models)) {
    spicy_abort(
      sprintf(
        "`model_labels` has length %d but `models` has length %d.",
        length(model_labels),
        length(models)
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (anyDuplicated(model_labels)) {
    dupes <- unique(model_labels[duplicated(model_labels)])
    spicy_abort(
      c(
        sprintf(
          "Duplicate value(s) in `model_labels`: %s.",
          paste(.quote_val(dupes), collapse = ", ")
        ),
        "i" = paste0(
          "Each model column needs a distinct label so the wide ",
          "rendering can name its sub-columns unambiguously."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# The Q1 precedence (explicit `model_labels` > `names(models)` > default),
# resolved in ONE place so the guard below, the column spanners and the
# table footer all read the same vector. NULL means "no user labels" and
# leaves the renderer free to generate its own ("Model 1, ...", or the
# response-variable names when those are distinct).
#
# Partial naming auto-fills the unnamed slots from the registry, which is
# the only way a duplicate can survive the two guards above: the name the
# user typed and the label the auto-fill writes are compared against each
# other nowhere else. `fmt` words the auto-fill: the language of the
# table for the spanners and the notes, `.spicy_fmt_en()` for the column
# names, which keep the English default.
.resolve_model_labels <- function(models, model_labels, fmt = spicy_fmt) {
  if (!is.null(model_labels)) {
    return(model_labels)
  }
  nms <- names(models)
  # An NA name is an unnamed slot: nzchar(NA) is TRUE, so without this
  # the NA survives as a label and breaks subscripting downstream.
  nms[is.na(nms)] <- ""
  if (is.null(nms) || !any(nzchar(nms))) {
    return(NULL)
  }
  missing_idx <- which(!nzchar(nms))
  nms[missing_idx] <- fmt("label_model_name", missing_idx)
  nms
}

# Step 22b: the RESOLVED labels, one spanner per label. `model_labels`
# (step 22) and `names(models)` (step 1c) are each deduplicated on their
# own, but neither sees the other: `list("Model 2" = m1, m2)` auto-fills
# slot 2 to the label slot 1 was typed with. Two models then land under
# one spanner, where the char body overwrites, the typed view unions, and
# `inline(model = )` cites whichever the lookup happens to reach.
#
# Refused rather than rendered, on the jurisprudence of the three guards
# it joins: two indistinguishable column groups cannot be addressed, and
# `render_regression_table()` already gives up the response-variable
# labels rather than print an ambiguous "mpg / mpg / hp".
validate_resolved_model_labels <- function(
  resolved,
  models,
  call = rlang::caller_env()
) {
  if (is.null(resolved) || !anyDuplicated(resolved)) {
    return(invisible(NULL))
  }
  dupe <- resolved[[anyDuplicated(resolved)]]
  hit <- which(resolved == dupe)
  nms <- names(models) %||% character(length(models))
  spicy_abort(
    c(
      sprintf("Model labels must be unique; %s repeats.", .quote_val(dupe)),
      "x" = sprintf(
        "It is %s.",
        paste(
          sprintf(
            "the %s of model %d",
            ifelse(nzchar(nms[hit]), "name", "default label"),
            hit
          ),
          collapse = " and "
        )
      ),
      "i" = paste0(
        "Name every model, or pass `model_labels`, so that each ",
        "column group carries a distinct label."
      )
    ),
    class = "spicy_invalid_input",
    call = call
  )
}

# Step 23: outcome_labels (NULL | FALSE | character vector of length(models))
validate_outcome_labels <- function(
  outcome_labels,
  models,
  call = rlang::caller_env()
) {
  if (is.null(outcome_labels) || isFALSE(outcome_labels)) {
    return(invisible(NULL))
  }
  if (!is.character(outcome_labels) || any(is.na(outcome_labels))) {
    spicy_abort(
      "`outcome_labels` must be NULL, FALSE, or a character vector.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (length(outcome_labels) != length(models)) {
    spicy_abort(
      sprintf(
        "`outcome_labels` has length %d but `models` has length %d.",
        length(outcome_labels),
        length(models)
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# Step 24: labels. Its form (TRUE, FALSE, a named character vector,
# `var_labels()`) is checked at the top of table_regression() by
# `.check_labels_arg()`, the validator the descriptive tables share, and a
# key that names nothing in the table is ignored there (decision 54,
# `.reg_variable_labels()`). What is left for this step: a label is a
# string, never NA, which the stub would print as "NA".
validate_predictor_labels <- function(labels, call = rlang::caller_env()) {
  if (is.character(labels) && anyNA(labels)) {
    spicy_abort(
      "Every `labels` entry must be a string, not NA.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}

# The keys a `labels` entry can take to relabel a row of a regression
# table, over every model of it. BOTH flavors:
#   * formula term labels  (`attr(terms(fit), "term.labels")`)
#     -> e.g. "wt", "cyl", "wt:cyl", "I(x^2)"
#   * coefficient names    (`names(coef(fit))`)
#     -> e.g. "(Intercept)", "cyl6", "factor(cyl)8", "wt:cyl6"
# The renderer tries the per-row label first (coef name), then
# falls back to the per-term label (factor variable). So both
# flavors of key are useful: term keys rename factor headers,
# coef keys rename individual contrast rows. Any other key is ignored,
# which keeps the package-set block headers (`Random effects`,
# `Thresholds`, ...) out of reach: they share the lookup of the factor
# headers.
# .spicy_get_terms(), not stats::terms(): the renderer detects factor
# headers through that helper, so the keys must be exactly the terms the
# display shows -- flexsurvreg carries its terms on the model.frame and
# brmsfit on the unwrapped brmsformula, and a bare stats::terms() call
# missed the only label those headers can take.
# Classes with no terms at all (nls, selection) return NULL here and
# are keyed on the coefficient names alone (below).
.reg_label_keys <- function(models) {
  all_terms <- unlist(lapply(models, function(fit) {
    tryCatch(
      attr(.spicy_get_terms(fit), "term.labels"),
      error = function(e) character(0)
    )
  }))
  # Fixed-effect coefficient names via the polymorphic helper:
  # names(coef(fit)) is wrong for several classes (merMod returns the
  # per-GROUP coefficient list, so its names are the grouping factors;
  # lme returns random-effect-augmented coefficients; clm mixes
  # thresholds in). .spicy_fixed_coef_names() knows each class.
  all_coefs <- unlist(lapply(models, .reg_coef_keys_of))
  unique(c(all_terms, all_coefs))
}

# The coefficient names one fit's rows are keyed by.
.reg_coef_keys_of <- function(fit) {
  nm <- tryCatch(.spicy_fixed_coef_names(fit), error = function(e) {
    names(stats::coef(fit))
  })
  # An rms fit names its rows in lm's spelling (.rms_row_terms()), and
  # the cut-points of an ordinal lrm are its Thresholds block.
  if (inherits(fit, "rms")) {
    nm <- .rms_row_terms(
      setdiff(nm, .lrm_cut_names(fit)),
      .rms_logical_vars(fit),
      .rms_transformed_terms(fit)
    )
  }
  # A fixest fit names its rows as lm and ivreg do (.fixest_row_terms()).
  if (inherits(fit, "fixest")) {
    nm <- .fixest_row_terms(nm, fit)
  }
  nm
}

# The coefficient rows each interaction term label of `keys` names, over
# every model: "age:wc" names "age:wcyes", and "wc:g3" names "wcyes:g3b"
# and "wcyes:g3c". A row belongs to the term when its ":"-pieces open, in
# order, with the pieces of the term, each piece read as the longest
# piece of the model's terms that opens it ("age2:wcyes" belongs to
# "age2:wc", not to "age:wc"). A named list, one entry per key that is an
# interaction term of some model.
.reg_interaction_rows <- function(models, keys) {
  keys <- unique(keys[grepl(":", keys, fixed = TRUE)])
  out <- list()
  if (length(keys) == 0L) {
    return(out)
  }
  for (fit in models) {
    tl <- tryCatch(
      attr(.spicy_get_terms(fit), "term.labels"),
      error = function(e) character(0)
    )
    cand <- intersect(keys, tl)
    if (length(cand) == 0L) {
      next
    }
    nm <- .reg_coef_keys_of(fit)
    pieces <- unique(unlist(strsplit(tl, ":", fixed = TRUE)))
    pieces <- pieces[order(-nchar(pieces))]
    owner <- function(p) {
      hit <- pieces[startsWith(p, pieces)]
      if (length(hit) > 0L) hit[[1L]] else NA_character_
    }
    nm_pieces <- strsplit(nm, ":", fixed = TRUE)
    for (k in cand) {
      kp <- strsplit(k, ":", fixed = TRUE)[[1L]]
      hit <- vapply(
        nm_pieces,
        function(s) {
          length(s) == length(kp) &&
            identical(unname(vapply(s, owner, character(1))), kp)
        },
        logical(1)
      )
      out[[k]] <- unique(c(out[[k]], nm[hit]))
    }
  }
  out
}


# ---- Phase E \u2013 cross-arg semantic warnings (no errors) --------------------

# Step 25: standardized != "none" x non-additive terms -> spicy_caveat warning
#
# `panel`, the request of a panel of a stacked table: its model is
# numbered as in the whole list, and the driver gathers the lines of
# every panel into the one warning the side-by-side table gives.
emit_standardized_caveat_if_needed <- function(
  models,
  standardized,
  panel = NULL
) {
  if (identical(standardized, "none")) {
    return(invisible(NULL))
  }

  nonadditive <- lapply(models, detect_non_additive_terms)
  any_problem <- any(vapply(nonadditive, `[[`, logical(1), "has_problem"))
  if (!any_problem) {
    return(invisible(NULL))
  }

  lines <- character(0)
  for (i in seq_along(nonadditive)) {
    info <- nonadditive[[i]]
    if (info$has_problem) {
      parts <- character(0)
      if (length(info$interactions) > 0L) {
        parts <- c(
          parts,
          sprintf(
            "interactions: %s",
            paste(info$interactions, collapse = ", ")
          )
        )
      }
      if (length(info$transforms) > 0L) {
        parts <- c(
          parts,
          sprintf(
            "transforms: %s",
            paste(info$transforms, collapse = ", ")
          )
        )
      }
      lines <- c(
        lines,
        sprintf(
          "Model %d: %s",
          .reg_model_index(i, panel),
          paste(parts, collapse = "; ")
        )
      )
    }
  }

  caveat_msg <- if (identical(standardized, "refit")) {
    paste0(
      "After refit on z-scored data, \u03B2 for these terms reflects ",
      "the interaction of z-scored variables, not the standardization ",
      "of the original term."
    )
  } else {
    paste0(
      "\u03B2 scales these terms by the SD of the product / transformed ",
      "design column (the SPSS / Stata `regress, beta` / lm.beta ",
      "convention); components are not standardized first, so results ",
      "differ from \"refit\" when components are correlated."
    )
  }

  msg <- function(it) {
    c(
      sprintf(
        paste0(
          "Standardized coefficients (`standardized = \"%s\"`) requested ",
          "on models with non-additive terms."
        ),
        standardized
      ),
      it$lines,
      "i" = caveat_msg,
      "i" = "Friedrich (1982); Cohen, Cohen, West & Aiken (2003) \u00A77.7."
    )
  }
  items <- list(lines = lines)
  spicy_warn(
    msg(items),
    class = "spicy_caveat",
    spicy_gather = .reg_gather(panel, "standardized_caveat", items, msg)
  )
}

# Detect interactions and transforms in a fitted model.
# Used by Q15 caveat emission (Step 25) and by the rendering layer
# to tag affected rows when needed. Falls back to the model formula's
# RHS for fits that do not register a `terms()` method (e.g. nls,
# flexsurvreg, sampleSelection's selection). When no term labels are
# extractable, returns has_problem = FALSE.
detect_non_additive_terms <- function(fit) {
  trms <- tryCatch(attr(stats::terms(fit), "term.labels"), error = function(e) {
    NULL
  })
  if (is.null(trms)) {
    # Fallback path: build a terms object from formula(fit)'s RHS.
    # For multi-equation fits (sampleSelection) and parameter-based
    # fits (nls), this may still error; in that case skip the check.
    trms <- tryCatch(
      {
        f <- stats::formula(fit)
        # The is.list() predicate below is reached (and FALSE) for every
        # supported terms()-less fit, e.g. nls, whose formula() is a single
        # formula. Only the inner subset body is narrowly nocov'd: the
        # multi-equation fits whose formula() is list-valued (sampleSelection)
        # error in formula() itself and are caught by the surrounding
        # tryCatch, so no installed class reaches f <- f[[1L]].
        if (is.list(f) && length(f) > 0L) {
          f <- f[[1L]] # nocov
        }
        attr(stats::terms(f), "term.labels")
      },
      error = function(e) NULL
    )
  }
  if (is.null(trms)) {
    return(list(
      has_problem = FALSE,
      interactions = character(0),
      transforms = character(0)
    ))
  }
  interactions <- trms[grepl(":", trms, fixed = TRUE)]
  transforms <- trms[grepl("(", trms, fixed = TRUE)]
  list(
    has_problem = length(interactions) > 0L || length(transforms) > 0L,
    interactions = interactions,
    transforms = transforms
  )
}

# Step 26: detect AME-Satterthwaite path activation. Returns TRUE iff
# any vcov is CR* AND "AME" is requested. Rendering layer uses this
# flag to dispatch to clubSandwich::linear_contrast() (Q14b).
detect_ame_satterthwaite_path <- function(vcov, show_columns) {
  if (!any(.AME_COLUMN_TOKENS %in% show_columns)) {
    return(FALSE)
  }
  vcov_per <- if (is.list(vcov)) vcov else list(vcov)
  any(vapply(vcov_per, function(v) startsWith(v, "CR"), logical(1)))
}


# ---- Phase F \u2013 output-dependent resource validation -----------------------

# Steps 27-29: file paths and package availability for the selected
# output format. Fires only when the user picked the corresponding
# output, so users with `output = "default"` pay nothing for
# excel/word/clipboard validation.
validate_output_resources <- function(
  output,
  excel_path,
  word_path,
  call = rlang::caller_env()
) {
  if (identical(output, "excel")) {
    .check_output_path(excel_path, output, call = call)
    if (!spicy_pkg_available("openxlsx2")) {
      # nocov start
      spicy_abort(
        c(
          "`output = \"excel\"` requires the 'openxlsx2' package.",
          "i" = "Install it with `install.packages(\"openxlsx2\")`."
        ),
        class = "spicy_missing_pkg",
        call = call
      )
      # nocov end
    }
  }

  if (identical(output, "word")) {
    .check_output_path(word_path, output, call = call)
    if (!spicy_pkg_available("flextable")) {
      # nocov start
      spicy_abort(
        c(
          "`output = \"word\"` requires the 'flextable' package.",
          "i" = "Install it with `install.packages(\"flextable\")`."
        ),
        class = "spicy_missing_pkg",
        call = call
      )
      # nocov end
    }
    if (!spicy_pkg_available("officer")) {
      # nocov start
      spicy_abort(
        c(
          "`output = \"word\"` requires the 'officer' package.",
          "i" = "Install it with `install.packages(\"officer\")`."
        ),
        class = "spicy_missing_pkg",
        call = call
      )
      # nocov end
    }
  }

  if (identical(output, "clipboard")) {
    if (!spicy_pkg_available("clipr")) {
      # nocov start
      spicy_abort(
        c(
          "`output = \"clipboard\"` requires the 'clipr' package.",
          "i" = "Install it with `install.packages(\"clipr\")`."
        ),
        class = "spicy_missing_pkg",
        call = call
      )
      # nocov end
    }
    if (!clipr::clipr_available()) {
      spicy_abort(
        "Clipboard is not available on this system.",
        class = "spicy_unsupported",
        call = call
      )
    }
  }

  if (identical(output, "gt") && !spicy_pkg_available("gt")) {
    # nocov start
    spicy_abort(
      c(
        "`output = \"gt\"` requires the 'gt' package.",
        "i" = "Install it with `install.packages(\"gt\")`."
      ),
      class = "spicy_missing_pkg",
      call = call
    )
    # nocov end
  }
  if (
    identical(output, "flextable") &&
      !spicy_pkg_available("flextable")
  ) {
    # nocov start
    spicy_abort(
      c(
        "`output = \"flextable\"` requires the 'flextable' package.",
        "i" = "Install it with `install.packages(\"flextable\")`."
      ),
      class = "spicy_missing_pkg",
      call = call
    )
    # nocov end
  }
  if (
    identical(output, "tinytable") &&
      !spicy_pkg_available("tinytable")
  ) {
    # nocov start
    spicy_abort(
      c(
        "`output = \"tinytable\"` requires the 'tinytable' package.",
        "i" = "Install it with `install.packages(\"tinytable\")`."
      ),
      class = "spicy_missing_pkg",
      call = call
    )
    # nocov end
  }

  invisible(NULL)
}


# Survival estimand horizons (tau for RMST, at_time for the risk
# difference). The horizon DEFINES the estimand, so it is explicit and
# mandatory when its column family is requested -- and refused when
# passed without a use. tau additionally accepts "minmax" (resolved
# per fit and disclosed in the footer).
validate_estimand_horizons <- function(
  show_columns,
  tau,
  at_time,
  models = list(),
  call = rlang::caller_env()
) {
  wants_rmst <- any(
    c("rmst", "rmst_se", "rmst_ci", "rmst_p") %in%
      show_columns
  )
  wants_risk <- any(
    c("risk_diff", "risk_diff_se", "risk_diff_ci", "risk_diff_p") %in%
      show_columns
  )
  ok_scalar <- function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0
  }
  if (wants_rmst) {
    if (!(ok_scalar(tau) || identical(tau, "minmax"))) {
      spicy_abort(
        c(
          "RMST columns need an explicit horizon: pass `tau`.",
          "i" = paste0(
            "A positive time on the outcome's scale (e.g. ",
            "`tau = 365`), or `tau = \"minmax\"` for the ",
            "smallest per-group maximum follow-up."
          ),
          "i" = "The horizon defines the estimand; there is no default."
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  } else if (!is.null(tau)) {
    # A quantreg user reads `tau` as the quantile.
    rq_hint <- if (any(vapply(models, inherits, logical(1), "rq"))) {
      c(
        "i" = paste0(
          "`tau` is the horizon of the RMST columns. The quantile of an ",
          "`rq` fit is set in `quantreg::rq(tau = )`; a table of several ",
          "quantiles takes a list of fits, one per quantile."
        )
      )
    }
    spicy_abort(
      c("`tau` was supplied but no RMST column is requested.", rq_hint),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (wants_risk) {
    if (!ok_scalar(at_time)) {
      spicy_abort(
        c(
          "Risk-difference columns need an explicit landmark: pass `at_time`.",
          "i" = paste0(
            "A positive time on the outcome's scale (e.g. ",
            "`at_time = 365`)."
          ),
          "i" = "The landmark defines the estimand; there is no default."
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
  } else if (!is.null(at_time)) {
    spicy_abort(
      "`at_time` was supplied but no risk-difference column is requested.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(NULL)
}
