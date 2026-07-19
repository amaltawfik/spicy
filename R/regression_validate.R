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
# Centralised here so validators, the rendering layer, and the test suite
# share a single source of truth. Class-aware token compatibility is
# checked separately by `validate_class_appropriate_tokens()` and
# `validate_class_appropriate_nested_stats()`.
.regression_tokens <- list(
  # ATOMIC tokens for `show_columns`: one token = one displayed
  # column. All lowercase (idiomatic R / broom convention). Group
  # tokens like "all_b", "all_ame" expand to a fixed subset of
  # these (see `.show_columns_groups` below).
  show_columns_atomic = c(
    # B-coefficient family
    "b", "beta", "se", "ci", "t", "p",
    # Per-row N (populated by univariable-screening frames, where each
    # predictor block is its own fit; blank for ordinary models)
    "n",
    # Outcome event counts "events/N" per factor level, model totals
    # on continuous rows (binomial outcomes only; STROBE item 16)
    "n_events",
    # Average marginal effects (AME)
    "ame", "ame_se", "ame_ci", "ame_p",
    # Survival estimands (coxph): RMST difference over [0, tau] and
    # cumulative-incidence difference at `at_time`, by g-computation
    "rmst", "rmst_se", "rmst_ci", "rmst_p",
    "risk_diff", "risk_diff_se", "risk_diff_ci", "risk_diff_p",
    # Variance-explained partials (lm only) \u2013 split into
    # estimate-only + CI-only, matching the b / ci asymmetry-free
    # convention.
    "partial_f2",     "partial_f2_ci",
    "partial_eta2",   "partial_eta2_ci",
    "partial_omega2", "partial_omega2_ci",
    # LRT-based partial chi-square (glm; analog of partial F) \u2013
    # kept BUNDLED as "value (df)" because that is the standard
    # statistical-reporting convention (e.g. "chi2(2) = 5.34").
    "partial_chi2",
    # Probability of direction (Bayesian fits only).
    "pd",
    # Per-parameter sampler diagnostics (Bayesian fits only), and the
    # Monte Carlo standard error of the displayed posterior median.
    "rhat", "ess_bulk", "ess_tail", "mcse"
  ),
  show_fit_stats = c(
    "nobs", "weighted_nobs",
    # Variance-explained (lm only)
    "r2", "adj_r2", "omega2",
    # Pseudo-R\u00B2 family (glm only)
    "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "pseudo_r2_tjur",
    # Negative-binomial dispersion (MASS::glm.nb only): theta
    # (V = mu + mu^2/theta) and alpha = 1/theta (Stata nbreg).
    "theta", "alpha",
    # Beta-regression precision (betareg only): phi (Ferrari &
    # Cribari-Neto 2004; Var(y) = mu(1-mu)/(1+phi)).
    "phi",
    # Bayesian fits only: posterior-median Bayesian R^2 (Gelman et
    # al. 2019), and PSIS-LOO elpd / LOOIC (Vehtari et al. 2017).
    "r2_bayes", "elpd_loo", "looic", "waic",
    # Mixed-effects R\u00B2 (Nakagawa & Schielzeth 2013; Nakagawa,
    # Johnson & Schielzeth 2017). marginal = variance explained by
    # fixed effects alone; conditional = variance explained by
    # fixed + random.
    "r2_marginal", "r2_conditional",
    # Mixed-effects group structure: N per grouping factor + intraclass
    # correlation, as fit-stat rows (sjPlot / modelsummary convention).
    "n_events",
    "n_groups", "icc",
    "sigma", "rmse",
    "f2",
    # Lowercase like every other token (the 0.13 rename; renders as
    # "AIC" / "AICc" / "BIC" row labels unchanged) -- coherent with
    # the aic_change / aicc_change / bic_change tokens below.
    "aic", "aicc", "bic", "deviance",
    # Nested-comparison change stats (APA Table 7.13 in-table rows).
    # `nested = TRUE` auto-injects a class-aware subset; the user
    # can request any of these explicitly in `show_fit_stats`.
    "r2_change", "adj_r2_change",
    "f_change", "f2_change",
    "lrt_change",
    "aic_change", "aicc_change", "bic_change",
    "deviance_change", "p_change"
  )
)

# Predefined group tokens for `show_columns`. Each group expands
# to a fixed vector of atomic tokens. Inspired by Stata / SPSS /
# SAS keyword presets \u2013 lets the user write
# `show_columns = c("all_b", "all_ame")` instead of enumerating 8
# atomic tokens. The expansion runs once, before validation, so
# downstream code only ever sees atomic tokens.
.show_columns_groups <- list(
  all_b         = c("b", "se", "ci", "p"),
  all_b_compact = c("b", "se", "p"),
  all_b_full    = c("b", "se", "ci", "t", "p"),
  all_beta      = c("b", "beta", "se", "ci", "p"),
  all_ame         = c("ame", "ame_se", "ame_ci", "ame_p"),
  all_ame_compact = c("ame", "ame_p"),
  all_f2     = c("partial_f2",     "partial_f2_ci"),
  all_eta2   = c("partial_eta2",   "partial_eta2_ci"),
  all_omega2 = c("partial_omega2", "partial_omega2_ci")
)

# Migration map for the spicy 0.11.x -> 0.12 token rename. When a
# user passes an old-style uppercase token we throw an actionable
# error pointing at the replacement instead of silently aliasing
# (per the pre-1.0 "hard errors over silent changes" policy).
.show_columns_legacy <- list(
  B       = c("b"),
  SE      = c("se"),
  CI      = c("ci"),
  AME     = c("ame", "ame_ci"),     # old AME bundled estimate + CI
  AME_p   = c("ame_p"),
  AME_SE  = c("ame_se")
)


# ---- Phase A \u2013 input class ------------------------------------------------

# Steps 1-3: validate `models` is lm OR list of lm; aggregate-fail on
# any non-lm position.
#
# Detection of "single fit vs list of fits" is non-trivial because R's
# lm objects ARE lists internally (with $coefficients, $residuals,
# etc.). The trick: a *plain* list has class identical to `"list"`;
# a model fit has model classes (lm, glm, merMod, ...) layered on top.
validate_models_input <- function(models) {
  # Step 1: handle NULL upfront (R's `inherits` and `is.list` both
  # return FALSE on NULL, so it would otherwise slip through)
  if (is.null(models)) {
    spicy_abort(
      c(
        "`models` cannot be NULL.",
        "i" = "Pass an `lm()` or `glm()` fit, or a list of such fits."
      ),
      class = "spicy_invalid_input"
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
      class = "spicy_invalid_input"
    )
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
              paste(shQuote(dupes), collapse = ", ")
            ),
            "i" = paste0(
              "Names of the models list are used as model IDs and ",
              "must be unique. Drop the names (positional list) or ",
              "rename the duplicates."
            )
          ),
          class = "spicy_invalid_input"
        )
      }
    }
  }

  # Steps 2-3: per-element class check, aggregate-fail
  problems <- vapply(seq_along(models), function(i) {
    msg <- classify_unsupported_lm_class(models[[i]], position = i)
    if (is.null(msg)) "" else msg
  }, character(1))
  bad <- problems[nzchar(problems)]
  if (length(bad) > 0L) {
    spicy_abort(
      c(
        "Some `models` are not supported by `table_regression()`.",
        bad,
        "i" = paste0(
          "Run `methods('as_regression_frame')` to see all currently ",
          "supported model classes."
        )
      ),
      class = "spicy_unsupported"
    )
  }

  models
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
  pos_prefix <- if (!is.null(position)) sprintf("Position %d: ", position) else ""

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
  if (.has_as_regression_frame_method(fit)) return(NULL)
  paste0(
    pos_prefix,
    sprintf("`%s` \u2013 no `as_regression_frame()` method registered. ",
            class(fit)[1L]),
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
    if (!is.null(utils::getS3method("as_regression_frame", cl,
                                     optional = TRUE))) {
      return(TRUE)
    }
  }
  FALSE
}


# ---- Phase B \u2013 multi-model alignment --------------------------------------

# Steps 4-5: nested = TRUE requires identical nobs and identical DV
validate_nested_alignment <- function(models, nested) {
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

  # Step 4: nobs identical. numeric(1), not integer(1): nobs() returns a
  # double for some supported classes (e.g. coxph), which would make an
  # integer(1) vapply throw under `nested = TRUE`. .spicy_nobs() covers
  # classes without a registered nobs method (nnet::multinom).
  nobs_vec <- vapply(models, .spicy_nobs, numeric(1))
  if (length(unique(nobs_vec)) > 1L) {
    spicy_abort(
      c(
        sprintf(
          "Models have different `nobs`: %s.",
          paste(nobs_vec, collapse = ", ")
        ),
        "i" = paste0(
          "Hierarchical comparison statistics (\u0394R\u00B2, partial F, ",
          "\u0394AIC, LRT, etc.) require identical observations across ",
          "all models. Different `nobs` typically results from R's ",
          "listwise deletion on different rows for each model."
        ),
        "i" = paste0(
          "Refit all models on the common subset, e.g.:\n",
          "  df_common <- tidyr::drop_na(your_data, ",
          "<all-predictors-and-DV>)\n",
          "  m1 <- lm(y ~ x1, data = df_common)\n",
          "  m2 <- lm(y ~ x1 + x2, data = df_common)"
        )
      ),
      class = "spicy_invalid_input"
    )
  }

  # Step 5: identical DV (response side of the formula)
  dvs <- vapply(models, function(fit) {
    deparse1(stats::formula(fit)[[2]])
  }, character(1))
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
      class = "spicy_invalid_input"
    )
  }

  invisible(NULL)
}

# Steps 6-8: vcov + cluster list/scalar coordination
validate_vcov_cluster_lists <- function(vcov, cluster, models) {
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
      c("i" = paste0(
          "Pass a single string (\"classical\", \"HC3\", ...) or a ",
          "list of strings."
        ))
    }
    spicy_abort(
      c(
        sprintf(
          "`vcov` must be a string or a list of strings, not a `%s` object.",
          class(vcov)[1]
        ),
        hint
      ),
      class = "spicy_invalid_input"
    )
  }

  # Canonical vcov vocabulary (Q7). Validated upfront so an unknown
  # type is caught with a clear error instead of letting `sandwich` /
  # `clubSandwich` warn-and-fallback at compute time.
  valid_vcov <- c("classical",
                   paste0("HC", 0:5),
                   paste0("CR", 0:3),
                   "bootstrap", "jackknife")

  # Step 6: vcov list length + element type
  if (is.list(vcov)) {
    if (length(vcov) != n_models) {
      spicy_abort(
        c(
          sprintf(
            "`vcov` is a list of length %d but `models` has length %d.",
            length(vcov), n_models
          ),
          "i" = paste0(
            "Pass a single string (recycled to all models) or a ",
            "list of length(models)."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
    if (!all(vapply(
      vcov,
      function(v) is.character(v) && length(v) == 1L && !is.na(v),
      logical(1)
    ))) {
      spicy_abort(
        "Each element of `vcov` (when a list) must be a single string.",
        class = "spicy_invalid_input"
      )
    }
  } else {
    if (!is.character(vcov) || length(vcov) != 1L || is.na(vcov)) {
      spicy_abort(
        c(
          "`vcov` must be a single string or a list of strings.",
          "i" = paste0(
            "Valid scalars: \"classical\", \"HC0\"\u2013\"HC5\", ",
            "\"CR0\"\u2013\"CR3\", \"bootstrap\", \"jackknife\"."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
  }

  # Step 6b: vocabulary check (each element must be a known type)
  vcov_check <- if (is.list(vcov)) unlist(vcov) else vcov
  bad <- setdiff(vcov_check, valid_vcov)
  if (length(bad) > 0L) {
    spicy_abort(
      c(
        sprintf(
          "Unknown `vcov` type(s): %s.",
          paste(shQuote(bad), collapse = ", ")
        ),
        "i" = sprintf(
          "Valid types: %s.",
          paste(shQuote(valid_vcov), collapse = ", ")
        )
      ),
      class = "spicy_invalid_input"
    )
  }

  # Step 6c: per-class capability. A robust vcov must be one the model's class
  # can actually compute, else fail fast (spicy_unsupported_vcov) instead of
  # silently returning model-based SEs under a robust label (audit finding C2).
  # "classical" is supported by every class, so default calls never trip this.
  vcov_per_model <- if (is.list(vcov)) vcov else rep(list(vcov), n_models)
  for (i in seq_len(n_models)) {
    vt        <- vcov_per_model[[i]]
    supported <- .robust_vcov_support(models[[i]])
    if (!vt %in% supported) {
      # Bayesian fits are refused on principle, not because support is
      # pending: a posterior has no sandwich analogue. The generic
      # "being added" wording below would misdescribe the design.
      if (inherits(models[[i]], c("stanreg", "brmsfit"))) {
        spicy_abort(
          c(
            sprintf("`vcov = \"%s\"` is not defined for Bayesian fits (`%s`).",
                    vt, class(models[[i]])[1L]),
            "i" = paste0("A posterior has no sandwich analogue: the ",
                         "displayed uncertainty comes from the posterior ",
                         "draws, not from an estimating equation."),
            "i" = paste0("Model the clustering or heteroskedasticity ",
                         "instead (rstanarm::stan_glmer(), brms ",
                         "group-level or distributional terms).")
          ),
          class = "spicy_unsupported_vcov"
        )
      }
      spicy_abort(
        c(
          sprintf("`vcov = \"%s\"` is not available for `%s` models.",
                  vt, class(models[[i]])[1L]),
          "i" = sprintf(
            "This class supports: %s. Robust standard errors for more model classes are being added; see ?table_regression.",
            paste(supported, collapse = ", ")
          )
        ),
        class = "spicy_unsupported_vcov"
      )
    }
  }

  # Step 7: cluster list length (only when an actual list, not an atomic vector)
  if (is.list(cluster) && !is.atomic(cluster)) {
    if (length(cluster) != n_models) {
      spicy_abort(
        c(
          sprintf(
            "`cluster` is a list of length %d but `models` has length %d.",
            length(cluster), n_models
          ),
          "i" = paste0(
            "Pass a single vector / column name (recycled to all ",
            "models) or a list of length(models)."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
  }

  # Step 8: per-model coherence (CR* requires cluster, length matches nobs)
  vcov_per <- if (is.list(vcov)) vcov else replicate(n_models, vcov, simplify = FALSE)
  cluster_per <- if (is.list(cluster) && !is.atomic(cluster)) {
    cluster
  } else {
    replicate(n_models, cluster, simplify = FALSE)
  }

  for (i in seq_len(n_models)) {
    v_i <- vcov_per[[i]]
    c_i <- cluster_per[[i]]

    is_cr <- startsWith(v_i, "CR")
    if (is_cr && is.null(c_i)) {
      spicy_abort(
        c(
          sprintf(
            "Model %d uses `vcov = \"%s\"` but no cluster is supplied.",
            i, v_i
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
        class = "spicy_invalid_input"
      )
    }

    # Cluster length must match what this fit's cluster-robust vcov requires.
    # Delegated to the shared .check_cluster_length() (the same guard the compute
    # path uses) so the required-length logic and the class-aware message live in
    # ONE place. The label carries the per-model `[[i]]` index for multi-model.
    if (!is.null(c_i) && is.atomic(c_i)) {
      .check_cluster_length(
        models[[i]], c_i,
        label = sprintf("`cluster%s`",
                        if (n_models > 1L) sprintf("[[%d]]", i) else "")
      )
    }

    # Cluster supplied but vcov is not CR* -- silent ignore would be a
    # forgotten-vcov mistake. Warn explicitly so the user notices.
    if (!is.null(c_i) && !is_cr) {
      spicy_warn(
        c(
          sprintf(
            paste0("Model %d: `cluster` supplied but `vcov = \"%s\"` ",
                    "is not cluster-robust; the cluster vector is ",
                    "ignored."),
            i, v_i
          ),
          "i" = paste0(
            "Set `vcov` to `\"CR0\"`, `\"CR1\"`, `\"CR2\"`, or `\"CR3\"` ",
            "to use the cluster vector for cluster-robust SE."
          )
        ),
        class = "spicy_ignored_arg"
      )
    }
  }

  invisible(NULL)
}


# ---- Phase C \u2013 vocabulary token validation --------------------------------

# Step 9: show_columns (+ Step 12: beta requires standardized != "none")
#
# `show_columns` accepts both atomic tokens ("b", "se", "ci", "p", ...)
# and group tokens ("all_b", "all_ame", ...). Group tokens are
# pre-expanded by `expand_show_columns()` before this validator runs
# so the only thing left to check here is membership + the
# beta-without-method rule.
validate_show_columns <- function(show_columns, standardized) {
  validate_token_vector(
    show_columns,
    .regression_tokens$show_columns_atomic,
    arg = "show_columns"
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
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
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
#   * lm  -> no  partial_chi2          (use partial_f2 / \u03B7\u00B2 / \u03C9\u00B2)
#   * lm  -> no  pseudo_r2_*           (use r2 / adj_r2 / omega2)
#   * glm -> no  partial_f2 / \u03B7\u00B2 / \u03C9\u00B2  (use partial_chi2; Long & Freese
#                                      2014 \u00A73.5; Allison "TYPE3")
#   * glm -> no  r2 / adj_r2 / omega2  (use pseudo_r2_*; McFadden
#                                      1974 / Nagelkerke 1991 / Tjur 2009)
validate_class_appropriate_tokens <- function(models,
                                                show_columns,
                                                show_fit_stats) {
  any_glm <- any(vapply(models, inherits, logical(1), "glm"))
  any_lm_only <- any(vapply(models, function(f) {
    inherits(f, "lm") && !inherits(f, "glm")
  }, logical(1)))
  all_glm <- any_glm && !any_lm_only
  all_lm  <- any_lm_only && !any_glm

  # Variance-explained partial tokens. Reject only when ALL models
  # are glm \u2013 in mixed sets, the renderer em-dashes glm rows and
  # populates lm rows, which is the right behaviour.
  if (all_glm) {
    bad <- intersect(show_columns,
                     c("partial_f2",     "partial_f2_ci",
                       "partial_eta2",   "partial_eta2_ci",
                       "partial_omega2", "partial_omega2_ci"))
    if (length(bad) > 0L) {
      spicy_abort(
        c(
          sprintf(
            "Token(s) %s in `show_columns` are not defined for `glm` models.",
            paste(shQuote(bad), collapse = ", ")
          ),
          "i" = paste0(
            "Variance-explained partition (R\u00B2 / partial f\u00B2 / \u03B7\u00B2 / \u03C9\u00B2) ",
            "does not apply outside the least-squares framework. The ",
            "`glm`-appropriate analog is `partial_chi2` (partial ",
            "likelihood-ratio chi-square via `drop1(test = \"LRT\")`; ",
            "Long & Freese 2014 \u00A73.5; Allison \"TYPE3\")."
          ),
          "i" = "Replace with `\"partial_chi2\"` or drop the token."
        ),
        class = "spicy_invalid_input"
      )
    }
    bad_fit <- intersect(show_fit_stats,
                          c("r2", "adj_r2", "omega2", "f2",
                            # Nested change variants of the above
                            "r2_change", "adj_r2_change",
                            "f_change", "f2_change"))
    if (length(bad_fit) > 0L) {
      spicy_abort(
        c(
          sprintf(
            "Token(s) %s in `show_fit_stats` are not defined for `glm` models.",
            paste(shQuote(bad_fit), collapse = ", ")
          ),
          "i" = paste0(
            "Use the pseudo-R\u00B2 family instead: `\"pseudo_r2_mcfadden\"` ",
            "(McFadden 1974), `\"pseudo_r2_nagelkerke\"` (Nagelkerke ",
            "1991), or `\"pseudo_r2_tjur\"` (Tjur 2009; binomial only). ",
            "For nested glm comparison, use `\"lrt_change\"` + ",
            "`\"p_change\"` (likelihood-ratio chi-square)."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
  }

  if (all_lm) {
    bad <- intersect(show_columns, "partial_chi2")
    if (length(bad) > 0L) {
      spicy_abort(
        c(
          "Token \"partial_chi2\" in `show_columns` is for `glm` models only.",
          "i" = paste0(
            "For `lm`, use `\"partial_f2\"`, `\"partial_eta2\"`, or ",
            "`\"partial_omega2\"` \u2013 the variance-explained analogs."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
    bad_fit <- intersect(show_fit_stats,
                          c("pseudo_r2_mcfadden",
                             "pseudo_r2_nagelkerke",
                             "pseudo_r2_tjur"))
    if (length(bad_fit) > 0L) {
      spicy_abort(
        c(
          sprintf(
            "Token(s) %s in `show_fit_stats` are for `glm` models only.",
            paste(shQuote(bad_fit), collapse = ", ")
          ),
          "i" = "For `lm`, use `\"r2\"`, `\"adj_r2\"`, `\"omega2\"`, or `\"f2\"`."
        ),
        class = "spicy_invalid_input"
      )
    }
  }

  # Bayesian fit statistics need at least one Bayesian model to
  # compute from; in a MIXED table the frequentist models render a
  # blank cell, mirroring how "r2" renders blank for the Bayesian
  # side (the shipped p-dash policy, applied in both directions).
  bayes_fit_req <- intersect(show_fit_stats,
                             c("r2_bayes", "elpd_loo", "looic", "waic"))
  if (length(bayes_fit_req) > 0L) {
    none_bayes <- length(models) == 0L ||
      !any(vapply(models, inherits, logical(1), c("stanreg", "brmsfit")))
    if (none_bayes) {
      spicy_abort(
        c(sprintf(
            "Token(s) %s in `show_fit_stats` are defined only for Bayesian fits.",
            paste(shQuote(bayes_fit_req), collapse = ", ")
          ),
          "i" = paste0("They are computed from the posterior draws ",
                       "(Bayesian R\u00B2; PSIS-LOO).")),
        class = "spicy_invalid_input"
      )
    }
    # The predictive-accuracy trio computes through the loo package
    # (Suggests). Refuse upfront rather than letting the frame's
    # tryCatch degrade the request to a silently absent row.
    loo_req <- intersect(bayes_fit_req, c("elpd_loo", "looic", "waic"))
    if (length(loo_req) > 0L && !spicy_pkg_available("loo")) {
      spicy_abort(
        c(sprintf(
            "Token(s) %s in `show_fit_stats` need the loo package.",
            paste(shQuote(loo_req), collapse = ", ")
          ),
          "i" = "Install loo: `install.packages(\"loo\")`."),
        class = "spicy_missing_pkg"
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
      c(paste0("Token(s) ",
               paste(shQuote(intersect(c("p", "t", "ame_p"),
                                       show_columns)),
                     collapse = ", "),
               " in `show_columns` are not defined for Bayesian fits."),
        "i" = paste0("A posterior has no p-value or t-statistic (for ",
                     "coefficients or AMEs). The probability of ",
                     "direction (`\"pd\"`) is the closest posterior ",
                     "summary.")),
      class = "spicy_invalid_input"
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
        c('Token "pd" in `show_columns` is defined only for Bayesian fits.',
          "i" = paste0("The probability of direction is a posterior-draw ",
                       "summary (share of draws on the dominant side of ",
                       "zero); frequentist fits have none.")),
        class = "spicy_invalid_input"
      )
    }
  }

  # Per-parameter sampler diagnostics: all-Bayesian tables only. In a
  # mixed table the columns would dash for most rows while the
  # automatic convergence guard ALREADY reports per-model sampler
  # problems in the footer, so the strict gate points there.
  diag_cols <- intersect(show_columns,
                         c("rhat", "ess_bulk", "ess_tail", "mcse"))
  if (length(diag_cols) > 0L) {
    all_bayes_diag <- length(models) > 0L &&
      all(vapply(models, inherits, logical(1), c("stanreg", "brmsfit")))
    if (!all_bayes_diag) {
      spicy_abort(
        c(sprintf(
            "Token(s) %s in `show_columns` are shown only when every model is Bayesian.",
            paste(shQuote(diag_cols), collapse = ", ")
          ),
          "i" = paste0("R-hat, ESS and MCSE are sampler diagnostics of ",
                       "the posterior draws; frequentist fits have none."),
          "i" = paste0("In a mixed table the automatic convergence guard ",
                       "already reports sampler problems per model in ",
                       "the footer.")),
        class = "spicy_invalid_input"
      )
    }
  }

  # Bayesian fits (finding a + b of the Bayesian-vignette recon):
  # a posterior has no p-values to adjust and no likelihood-based
  # information criteria; both requests were silently ignored before.
  all_bayes <- length(models) > 0L &&
    all(vapply(models, inherits, logical(1), c("stanreg", "brmsfit")))
  if (all_bayes) {
    bad_fit <- intersect(show_fit_stats,
                         c("aic", "aicc", "bic", "deviance",
                           "r2", "adj_r2", "omega2", "f2",
                           "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke",
                           "pseudo_r2_tjur", "sigma", "rmse"))
    if (length(bad_fit) > 0L) {
      spicy_abort(
        c(
          sprintf(
            "Token(s) %s in `show_fit_stats` are not defined for Bayesian fits.",
            paste(shQuote(bad_fit), collapse = ", ")
          ),
          "i" = paste0(
            "Posterior fits have no likelihood-based information ",
            "criteria or classical R\u00B2; compare models with ",
            "`loo::loo()` / `loo::loo_compare()` outside the table."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
  }

  # Negative-binomial dispersion tokens: defined only for MASS::glm.nb
  # fits (fit$theta). Hard error elsewhere -- silently dropping the row
  # (or rendering a blank) would hide the request (pre-1.0 policy).
  disp_tokens <- intersect(show_fit_stats, c("theta", "alpha"))
  if (length(disp_tokens) > 0L) {
    all_negbin <- length(models) > 0L &&
      all(vapply(models, inherits, logical(1), "negbin"))
    if (!all_negbin) {
      spicy_abort(
        c(
          sprintf(
            "Token(s) %s in `show_fit_stats` are defined only for negative-binomial fits (`MASS::glm.nb`).",
            paste(shQuote(disp_tokens), collapse = ", ")
          ),
          "i" = paste0(
            "theta is the NB2 dispersion (V = mu + mu\u00B2/theta) and ",
            "alpha its reciprocal; other families have no such parameter."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
  }

  # Beta-regression precision token: defined only for betareg fits
  # with a constant precision. Hard error elsewhere, mirroring the
  # negbin theta / alpha policy above.
  if ("phi" %in% show_fit_stats) {
    all_betareg <- length(models) > 0L &&
      all(vapply(models, inherits, logical(1), "betareg"))
    if (!all_betareg) {
      spicy_abort(
        c(
          paste0("Token 'phi' in `show_fit_stats` is defined only ",
                 "for beta-regression fits (`betareg::betareg`)."),
          "i" = paste0(
            "phi is the beta-distribution precision ",
            "(Var(y) = mu(1-mu)/(1+phi); higher phi = less ",
            "dispersion); other families have no such parameter."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
    variable_prec <- vapply(models, function(m) {
      length(tryCatch(stats::coef(m, model = "precision"),
                      error = function(e) numeric(0))) != 1L
    }, logical(1))
    if (any(variable_prec)) {
      spicy_abort(
        c(
          paste0("Token 'phi' needs a constant precision, but a ",
                 "model has covariates on the precision ",
                 "(`y ~ x | z`)."),
          "i" = paste0(
            "With precision covariates, phi is not a single number; ",
            "inspect the precision coefficients with ",
            "`summary(fit)` instead."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
  }

  # Mixed-effects fits match neither all_glm nor all_lm, so lm-only tokens would
  # otherwise slip through and render an empty row (finding m4). Classical /
  # pseudo R^2 and the least-squares partial-variance tokens are undefined for
  # mixed models; point the user at the Nakagawa marginal / conditional R^2.
  # Reject only when ALL models are mixed (a mixed set em-dashes the RE rows).
  all_mixed <- length(models) > 0L &&
    all(vapply(models, inherits, logical(1),
               c("merMod", "lmerModLmerTest", "glmmTMB", "lme")))
  if (all_mixed) {
    bad_fit <- intersect(show_fit_stats,
                         c("r2", "adj_r2", "omega2", "f2",
                           "pseudo_r2_mcfadden", "pseudo_r2_nagelkerke",
                           "pseudo_r2_tjur"))
    if (length(bad_fit) > 0L) {
      spicy_abort(
        c(
          sprintf(
            "Token(s) %s in `show_fit_stats` are not defined for mixed-effects models.",
            paste(shQuote(bad_fit), collapse = ", ")
          ),
          "i" = paste0(
            "Classical / pseudo R^2 do not apply to mixed-effects models. ",
            "Use the Nakagawa marginal / conditional R^2 instead: ",
            "`\"r2_marginal\"` and `\"r2_conditional\"` (Nakagawa et al. 2017)."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
    # partial_chi2 (LRT-based, drop1 test = "LRT") IS defined for mixed fits, so
    # it is deliberately NOT rejected here -- only the variance-explained
    # (least-squares) partials are undefined for mixed models.
    bad_cols <- intersect(show_columns,
                          c("partial_f2", "partial_f2_ci",
                            "partial_eta2", "partial_eta2_ci",
                            "partial_omega2", "partial_omega2_ci"))
    if (length(bad_cols) > 0L) {
      spicy_abort(
        c(
          sprintf(
            "Token(s) %s in `show_columns` are not defined for mixed-effects models.",
            paste(shQuote(bad_cols), collapse = ", ")
          ),
          "i" = paste0(
            "Variance-explained partials do not apply to mixed models. For a ",
            "partial likelihood-ratio test use `\"partial_chi2\"`."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
  }

  # AME is not defined for Cox proportional hazards models: avg_slopes() on a
  # coxph / cph fit returns effects on an ambiguous survival/hazard scale, and
  # marginaleffects itself warns its delta-method standard errors are
  # unreliable for Cox. The canonical Cox report is the hazard ratio
  # (exponentiate = TRUE). Reject only when ALL models are Cox; a mixed set
  # em-dashes the Cox AME rows (the AME extraction skips Cox fits).
  all_cox <- length(models) > 0L &&
    all(vapply(models, inherits, logical(1), c("coxph", "cph")))
  if (all_cox) {
    bad <- intersect(show_columns, c("ame", "ame_se", "ame_ci", "ame_p"))
    if (length(bad) > 0L) {
      spicy_abort(
        c(
          sprintf(
            "Token(s) %s in `show_columns` are not defined for Cox models.",
            paste(shQuote(bad), collapse = ", ")
          ),
          "i" = paste0(
            "Average marginal effects on a Cox proportional-hazards fit are ",
            "on an ambiguous survival / hazard scale with unreliable standard ",
            "errors. Report hazard ratios instead with `exponentiate = TRUE`."
          ),
          "i" = paste0(
            "For absolute effects, use the survival estimand columns: ",
            "`show_columns = c(\"b\", \"rmst\")` with `tau = `, or ",
            "`\"risk_diff\"` with `at_time = ` (coxph)."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
  }

  # AME is not computable for mlogit: marginaleffects supports predictions()
  # but NOT slopes() for its one-row-per-choice data structure, so avg_slopes()
  # errors. Reject "ame" tokens up front (when ALL models are mlogit) instead of
  # rendering a blank column.
  all_mlogit <- length(models) > 0L &&
    all(vapply(models, inherits, logical(1), "mlogit"))
  if (all_mlogit) {
    bad <- intersect(show_columns, c("ame", "ame_se", "ame_ci", "ame_p"))
    if (length(bad) > 0L) {
      spicy_abort(
        c(
          sprintf(
            "Token(s) %s in `show_columns` are not available for mlogit models.",
            paste(shQuote(bad), collapse = ", ")
          ),
          "i" = paste0(
            "marginaleffects computes predictions but not slopes (average ",
            "marginal effects) for mlogit's one-row-per-choice data structure."
          )
        ),
        class = "spicy_invalid_input"
      )
    }
  }
  invisible(NULL)
}

# Class-aware validation of `nested_stats`. Variance-explained
# tokens (r2_change, adj_r2_change, F, f2_change) require an OLS
# residual-sum-of-squares partition and are NA for glm. Reject
# explicitly when ALL nested models are glm; for mixed lm + glm
# hierarchies the renderer em-dashes the glm side, which is the
# right behaviour. NULL or empty `nested_stats` is a no-op (the
# class-aware default in compute_nested_comparisons_lm() picks the
# right tokens automatically).
# Step 10: show_fit_stats. Empty character or NULL means "drop the
# fit-stats footer block" -- a legitimate rendering choice (some
# users prefer the body alone). show_columns has no analogous
# escape hatch because a table with zero data columns is
# nonsensical.
validate_show_fit_stats <- function(show_fit_stats) {
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
  legacy_ic <- intersect(as.character(show_fit_stats),
                         c("AIC", "AICc", "BIC"))
  if (length(legacy_ic) > 0L) {
    spicy_abort(
      c(
        paste0("Uppercase `show_fit_stats` token(s) used; spicy 0.13 ",
               "switched the information criteria to lowercase."),
        "i" = paste0("Replacement(s): ",
                     paste(sprintf("`\"%s\"` -> `\"%s\"`",
                                   legacy_ic, tolower(legacy_ic)),
                           collapse = ", "), "."),
        "i" = "Rendered row labels are unchanged (AIC / AICc / BIC)."
      ),
      class = "spicy_invalid_input"
    )
  }
  validate_token_vector(
    show_fit_stats,
    .regression_tokens$show_fit_stats,
    arg = "show_fit_stats"
  )
}

# Generic token-vector validator used by show_*, nested_stats.
# Checks: character non-empty + no duplicates + all tokens in `valid`.
# Expand `show_columns` group tokens to atomic tokens in place. Runs
# before validation so the rest of the pipeline sees only atomic
# tokens. Emits a migration error for legacy uppercase tokens from
# spicy <= 0.11 (`"B"`, `"AME"`, ...) pointing at the new name.
expand_show_columns <- function(tokens, bayesian = FALSE) {
  if (is.null(tokens) || length(tokens) == 0L) return(tokens)
  tokens <- as.character(tokens)
  # Migration error: uppercase legacy tokens.
  legacy_used <- intersect(tokens, names(.show_columns_legacy))
  if (length(legacy_used)) {
    msgs <- vapply(legacy_used, function(old) {
      new_tok <- .show_columns_legacy[[old]]
      sprintf("  `\"%s\"` -> %s", old,
              paste(shQuote(new_tok), collapse = " + "))
    }, character(1))
    spicy_abort(
      c(
        paste0("Legacy uppercase `show_columns` token(s) used; ",
                "spicy 0.12 switched to lowercase atomic tokens."),
        "i" = "Replacement(s):",
        msgs,
        "i" = paste0("Or use a group token: \"all_b\" / ",
                      "\"all_b_compact\" / \"all_ame\" / ",
                      "\"all_f2\" / \"all_eta2\" / \"all_omega2\".")
      ),
      class = "spicy_invalid_input"
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
        if (isTRUE(bayesian)) grp <- setdiff(grp, c("p", "t", "ame_p"))
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

validate_token_vector <- function(x, valid, arg) {
  if (!is.character(x) || length(x) == 0L) {
    spicy_abort(
      sprintf("`%s` must be a non-empty character vector.", arg),
      class = "spicy_invalid_input"
    )
  }
  if (anyNA(x) || any(!nzchar(x))) {
    spicy_abort(
      sprintf(
        "`%s` must not contain NA or empty strings.", arg
      ),
      class = "spicy_invalid_input"
    )
  }
  if (anyDuplicated(x)) {
    dups <- unique(x[duplicated(x)])
    spicy_abort(
      sprintf(
        "`%s` contains duplicate token(s): %s.",
        arg, paste(shQuote(dups), collapse = ", ")
      ),
      class = "spicy_invalid_input"
    )
  }
  bad <- setdiff(x, valid)
  if (length(bad) > 0L) {
    spicy_abort(
      c(
        sprintf(
          "Unknown token(s) in `%s`: %s.",
          arg, paste(shQuote(bad), collapse = ", ")
        ),
        "i" = sprintf(
          "Valid tokens: %s.",
          paste(shQuote(valid), collapse = ", ")
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}


# ---- Phase D \u2013 argument value validation ----------------------------------

# Steps 13-14: enum args. table_regression() invokes match.arg() directly
# on `standardized`, `intercept_position`, `align`, `output`,
# `reference_style` \u2013 match.arg() raises a clear base-R error on
# invalid values. No spicy-specific helper needed.

# Phase 7c7d: validate the `re_columns` argument of table_regression().
# Accepts a character vector with elements from c("est", "se", "ci");
# duplicates and unknown tokens error early. "est" is always required
# (the table without an estimate column is meaningless).
.validate_re_columns <- function(x) {
  valid <- c("est", "se", "ci")
  validate_token_vector(x, valid, arg = "re_columns")
  if (!"est" %in% x) {
    spicy_abort(
      "`re_columns` must include \"est\" (the random-effect estimate).",
      class = "spicy_invalid_input"
    )
  }
  x
}


# Validate the `re_scale` argument of table_regression(): "sd" (default) or
# "variance". Errors early with a spicy_invalid_input (not a raw base-R
# match.arg message), and BEFORE the expensive extraction loop (finding m3).
.validate_re_scale <- function(x) {
  choices <- c("sd", "variance")
  if (identical(x, choices)) return("sd")            # unset default vector
  if (length(x) == 1L && !is.na(x) && x %in% choices) return(x)
  spicy_abort(
    c(
      "`re_scale` must be one of \"sd\" or \"variance\".",
      "x" = sprintf(
        "You supplied %s.",
        paste(encodeString(as.character(x), quote = "\""), collapse = ", ")
      )
    ),
    class = "spicy_invalid_input"
  )
}


# Step 15: digit args (non-negative integer scalar). Reused for
# `digits`, `p_digits`, `effect_size_digits`, `fit_digits`, `ic_digits`.
validate_digit_arg <- function(x, name) {
  ok <- length(x) == 1L && is.numeric(x) && !is.na(x) &&
    is.finite(x) && x >= 0 && x == as.integer(x)
  if (!ok) {
    spicy_abort(
      sprintf("`%s` must be a single non-negative integer.", name),
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}

# Step 16: ci_level
validate_ci_level <- function(ci_level) {
  ok <- length(ci_level) == 1L && is.numeric(ci_level) &&
    !is.na(ci_level) && is.finite(ci_level) &&
    ci_level > 0 && ci_level < 1
  if (!ok) {
    spicy_abort(
      "`ci_level` must be a single number in (0, 1) exclusive.",
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}

# Step 17: boot_n
validate_boot_n <- function(boot_n) {
  ok <- length(boot_n) == 1L && is.numeric(boot_n) && !is.na(boot_n) &&
    is.finite(boot_n) && boot_n >= 1 && boot_n == as.integer(boot_n)
  if (!ok) {
    spicy_abort(
      paste0(
        "`boot_n` must be a single positive integer ",
        "(number of bootstrap replicates)."
      ),
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}

# Step 18: logical scalar args. Reused for `show_intercept`,
# `group_factor_levels`, `nested`.
validate_logical_scalar <- function(x, name) {
  ok <- length(x) == 1L && is.logical(x) && !is.na(x)
  if (!ok) {
    spicy_abort(
      sprintf("`%s` must be a single TRUE or FALSE.", name),
      class = "spicy_invalid_input"
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
validate_caption_arg <- function(x, name) {
  if (is.null(x)) return(invisible(NULL))
  if (isFALSE(x)) return(invisible(NULL))
  ok <- length(x) == 1L && is.character(x) && !is.na(x)
  if (!ok) {
    spicy_abort(
      c(
        sprintf("`%s` must be NULL, FALSE, or a single string.", name),
        "i" = "NULL = auto-build; FALSE = suppress; \"...\" = override."
      ),
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}

# `p_adjust` validation. Must be one of stats::p.adjust.methods.
# We accept "fdr" as an alias for "BH" (matching stats::p.adjust
# itself) but normalise back to the canonical name in the orchestrator
# so the footer wording is consistent.
.spicy_p_adjust_methods <- c("none", "holm", "hochberg", "hommel",
                              "bonferroni", "BH", "BY", "fdr")

validate_p_adjust <- function(p_adjust) {
  if (length(p_adjust) != 1L || !is.character(p_adjust) ||
        is.na(p_adjust)) {
    spicy_abort(
      "`p_adjust` must be a single string.",
      class = "spicy_invalid_input"
    )
  }
  if (!p_adjust %in% .spicy_p_adjust_methods) {
    spicy_abort(
      c(
        sprintf("Unknown `p_adjust` method: %s.", shQuote(p_adjust)),
        "i" = sprintf(
          "Valid methods: %s.",
          paste(shQuote(.spicy_p_adjust_methods), collapse = ", ")
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}

# `keep` / `drop` validation. Each is NULL (no filter) or a non-empty
# character vector with no NA / empty-string elements. They are
# mutually exclusive \u2013 supplying both raises an error rather than
# silently picking one.
validate_keep_drop <- function(keep, drop) {
  for (pair in list(c("keep", "keep"), c("drop", "drop"))) {
    arg_name <- pair[[1L]]
    val <- get(arg_name)
    if (is.null(val)) next
    if (!is.character(val) || length(val) == 0L ||
          any(is.na(val)) || any(!nzchar(val))) {
      spicy_abort(
        sprintf(
          paste0("`%s` must be NULL or a non-empty character vector ",
                  "with no NA or empty-string elements."),
          arg_name
        ),
        class = "spicy_invalid_input"
      )
    }
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
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}

# Step 19: stars (FALSE | TRUE | named numeric vector with thresholds in (0,1])
validate_stars <- function(stars) {
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
      class = "spicy_invalid_input"
    )
  }
  nms <- names(stars)
  if (is.null(nms) || any(!nzchar(nms)) || any(is.na(nms))) {
    spicy_abort(
      paste0(
        "When `stars` is a numeric vector, all elements must have ",
        "non-empty names (the symbols)."
      ),
      class = "spicy_invalid_input"
    )
  }
  if (anyDuplicated(nms)) {
    spicy_abort(
      "Symbols in `stars` (vector names) must be unique.",
      class = "spicy_invalid_input"
    )
  }
  if (any(is.na(stars)) || any(stars <= 0) || any(stars > 1)) {
    spicy_abort(
      "Thresholds in `stars` must all be in (0, 1].",
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}

# Step 20: decimal_mark
validate_decimal_mark <- function(decimal_mark) {
  ok <- length(decimal_mark) == 1L && is.character(decimal_mark) &&
    !is.na(decimal_mark) && nchar(decimal_mark) == 1L
  if (!ok) {
    spicy_abort(
      "`decimal_mark` must be a single character (e.g. \".\" or \",\").",
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}

# Step 21: reference_label
validate_reference_label <- function(reference_label) {
  ok <- length(reference_label) == 1L && is.character(reference_label) &&
    !is.na(reference_label) && nzchar(reference_label)
  if (!ok) {
    spicy_abort(
      "`reference_label` must be a single non-empty string.",
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}

# Step 22: model_labels
validate_model_labels <- function(model_labels, models) {
  if (is.null(model_labels)) {
    return(invisible(NULL))
  }
  if (!is.character(model_labels) || any(is.na(model_labels)) ||
        any(!nzchar(model_labels))) {
    spicy_abort(
      paste0(
        "`model_labels` must be NULL or a character vector with ",
        "non-empty values."
      ),
      class = "spicy_invalid_input"
    )
  }
  if (length(model_labels) != length(models)) {
    spicy_abort(
      sprintf(
        "`model_labels` has length %d but `models` has length %d.",
        length(model_labels), length(models)
      ),
      class = "spicy_invalid_input"
    )
  }
  if (anyDuplicated(model_labels)) {
    dupes <- unique(model_labels[duplicated(model_labels)])
    spicy_abort(
      c(
        sprintf(
          "Duplicate value(s) in `model_labels`: %s.",
          paste(shQuote(dupes), collapse = ", ")
        ),
        "i" = paste0(
          "Each model column needs a distinct label so the wide ",
          "rendering can name its sub-columns unambiguously."
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}

# Step 23: outcome_labels (NULL | FALSE | character vector of length(models))
validate_outcome_labels <- function(outcome_labels, models) {
  if (is.null(outcome_labels) || isFALSE(outcome_labels)) {
    return(invisible(NULL))
  }
  if (!is.character(outcome_labels) || any(is.na(outcome_labels))) {
    spicy_abort(
      "`outcome_labels` must be NULL, FALSE, or a character vector.",
      class = "spicy_invalid_input"
    )
  }
  if (length(outcome_labels) != length(models)) {
    spicy_abort(
      sprintf(
        "`outcome_labels` has length %d but `models` has length %d.",
        length(outcome_labels), length(models)
      ),
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}

# Step 24: labels (named character with keys in any model's term labels)
validate_predictor_labels <- function(labels, models) {
  if (is.null(labels)) {
    return(invisible(NULL))
  }
  if (!is.character(labels) || any(is.na(labels))) {
    spicy_abort(
      "`labels` must be NULL or a named character vector.",
      class = "spicy_invalid_input"
    )
  }
  nms <- names(labels)
  if (is.null(nms) || any(!nzchar(nms))) {
    spicy_abort(
      paste0(
        "`labels` must be a NAMED character vector ",
        "(names = term names; values = displayed labels)."
      ),
      class = "spicy_invalid_input"
    )
  }

  # Accept BOTH:
  #   * formula term labels  (`attr(terms(fit), "term.labels")`)
  #     -> e.g. "wt", "cyl", "wt:cyl", "I(x^2)"
  #   * coefficient names    (`names(coef(fit))`)
  #     -> e.g. "(Intercept)", "cyl6", "factor(cyl)8", "wt:cyl6"
  # The renderer tries the per-row label first (coef name), then
  # falls back to the per-term label (factor variable). So both
  # flavours of key are useful: term keys rename factor headers,
  # coef keys rename individual contrast rows.
  all_terms <- unique(unlist(lapply(models, function(fit) {
    attr(stats::terms(fit), "term.labels")
  })))
  # Fixed-effect coefficient names via the polymorphic helper:
  # names(coef(fit)) is wrong for several classes (merMod returns the
  # per-GROUP coefficient list, so its names are the grouping factors;
  # lme returns random-effect-augmented coefficients; clm mixes
  # thresholds in). .spicy_fixed_coef_names() knows each class.
  all_coefs <- unique(unlist(lapply(models, function(fit) {
    tryCatch(.spicy_fixed_coef_names(fit),
             error = function(e) names(stats::coef(fit)))
  })))
  valid_keys <- unique(c(all_terms, all_coefs))
  unknown <- setdiff(nms, valid_keys)
  if (length(unknown) > 0L) {
    spicy_abort(
      c(
        sprintf(
          "Some `labels` keys are not term or coefficient names: %s.",
          paste(shQuote(unknown), collapse = ", ")
        ),
        "i" = sprintf(
          "Available term labels: %s.",
          paste(shQuote(all_terms), collapse = ", ")
        ),
        "i" = sprintf(
          "Available coefficient names: %s.",
          paste(shQuote(all_coefs), collapse = ", ")
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}


# ---- Phase E \u2013 cross-arg semantic warnings (no errors) --------------------

# Step 25: standardized != "none" x non-additive terms -> spicy_caveat warning
emit_standardized_caveat_if_needed <- function(models, standardized) {
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
        parts <- c(parts, sprintf(
          "interactions: %s",
          paste(info$interactions, collapse = ", ")
        ))
      }
      if (length(info$transforms) > 0L) {
        parts <- c(parts, sprintf(
          "transforms: %s",
          paste(info$transforms, collapse = ", ")
        ))
      }
      lines <- c(lines, sprintf(
        "Model %d: %s", i, paste(parts, collapse = "; ")
      ))
    }
  }

  caveat_msg <- if (identical(standardized, "refit")) {
    paste0(
      "After refit on z-scored data, \u03B2 for these terms reflects ",
      "the interaction of z-scored variables, not the standardisation ",
      "of the original term."
    )
  } else {
    paste0(
      "\u03B2 scales these terms by the SD of the product / transformed ",
      "design column (the SPSS / Stata `regress, beta` / lm.beta ",
      "convention); components are not standardised first, so results ",
      "differ from \"refit\" when components are correlated."
    )
  }

  spicy_warn(
    c(
      sprintf(
        paste0(
          "Standardised coefficients (`standardized = \"%s\"`) requested ",
          "on models with non-additive terms."
        ),
        standardized
      ),
      lines,
      "i" = caveat_msg,
      "i" = "Friedrich (1982); Cohen, Cohen, West & Aiken (2003) \u00A77.7."
    ),
    class = "spicy_caveat"
  )
}

# Detect interactions and transforms in a fitted model.
# Used by Q15 caveat emission (Step 25) and by the rendering layer
# to tag affected rows when needed. Falls back to the model formula's
# RHS for fits that do not register a `terms()` method (e.g. nls,
# flexsurvreg, sampleSelection's selection). When no term labels are
# extractable, returns has_problem = FALSE.
detect_non_additive_terms <- function(fit) {
  trms <- tryCatch(attr(stats::terms(fit), "term.labels"),
                   error = function(e) NULL)
  if (is.null(trms)) {
    # Fallback path: build a terms object from formula(fit)'s RHS.
    # For multi-equation fits (sampleSelection) and parameter-based
    # fits (nls), this may still error; in that case skip the check.
    trms <- tryCatch({
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
    }, error = function(e) NULL)
  }
  if (is.null(trms)) {
    return(list(has_problem = FALSE,
                interactions = character(0),
                transforms   = character(0)))
  }
  interactions <- trms[grepl(":", trms, fixed = TRUE)]
  transforms   <- trms[grepl("(", trms, fixed = TRUE)]
  list(
    has_problem  = length(interactions) > 0L || length(transforms) > 0L,
    interactions = interactions,
    transforms   = transforms
  )
}

# Step 26: detect AME-Satterthwaite path activation. Returns TRUE iff
# any vcov is CR* AND "AME" is requested. Rendering layer uses this
# flag to dispatch to clubSandwich::linear_contrast() (Q14b).
detect_ame_satterthwaite_path <- function(vcov, show_columns) {
  if (!"ame" %in% show_columns) {
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
validate_output_resources <- function(output, excel_path, word_path) {
  if (identical(output, "excel")) {
    if (is.null(excel_path)) {
      spicy_abort(
        c(
          "`output = \"excel\"` requires `excel_path` to be supplied.",
          "i" = "Pass a file path: `excel_path = \"results/regression.xlsx\"`."
        ),
        class = "spicy_invalid_input"
      )
    }
    parent <- dirname(excel_path)
    if (!dir.exists(parent)) {
      spicy_abort(
        sprintf("Directory does not exist: `%s`.", parent),
        class = "spicy_invalid_input"
      )
    }
    if (!spicy_pkg_available("openxlsx2")) {
      # nocov start
      spicy_abort(
        c(
          "`output = \"excel\"` requires the 'openxlsx2' package.",
          "i" = "Install it with `install.packages(\"openxlsx2\")`."
        ),
        class = "spicy_missing_pkg"
      )
      # nocov end
    }
  }

  if (identical(output, "word")) {
    if (is.null(word_path)) {
      spicy_abort(
        c(
          "`output = \"word\"` requires `word_path` to be supplied.",
          "i" = "Pass a file path: `word_path = \"results/regression.docx\"`."
        ),
        class = "spicy_invalid_input"
      )
    }
    parent <- dirname(word_path)
    if (!dir.exists(parent)) {
      spicy_abort(
        sprintf("Directory does not exist: `%s`.", parent),
        class = "spicy_invalid_input"
      )
    }
    if (!spicy_pkg_available("flextable")) {
      # nocov start
      spicy_abort(
        c(
          "`output = \"word\"` requires the 'flextable' package.",
          "i" = "Install it with `install.packages(\"flextable\")`."
        ),
        class = "spicy_missing_pkg"
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
        class = "spicy_missing_pkg"
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
        class = "spicy_missing_pkg"
      )
      # nocov end
    }
    if (!clipr::clipr_available()) {
      spicy_abort(
        "Clipboard is not available on this system.",
        class = "spicy_unsupported"
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
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  if (identical(output, "flextable") &&
        !spicy_pkg_available("flextable")) {
    # nocov start
    spicy_abort(
      c(
        "`output = \"flextable\"` requires the 'flextable' package.",
        "i" = "Install it with `install.packages(\"flextable\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  if (identical(output, "tinytable") &&
        !spicy_pkg_available("tinytable")) {
    # nocov start
    spicy_abort(
      c(
        "`output = \"tinytable\"` requires the 'tinytable' package.",
        "i" = "Install it with `install.packages(\"tinytable\")`."
      ),
      class = "spicy_missing_pkg"
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
validate_estimand_horizons <- function(show_columns, tau, at_time) {
  wants_rmst <- any(c("rmst", "rmst_se", "rmst_ci", "rmst_p") %in%
                      show_columns)
  wants_risk <- any(c("risk_diff", "risk_diff_se", "risk_diff_ci",
                      "risk_diff_p") %in% show_columns)
  ok_scalar <- function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0
  }
  if (wants_rmst) {
    if (!(ok_scalar(tau) || identical(tau, "minmax"))) {
      spicy_abort(
        c("RMST columns need an explicit horizon: pass `tau`.",
          "i" = paste0("A positive time on the outcome's scale (e.g. ",
                       "`tau = 365`), or `tau = \"minmax\"` for the ",
                       "smallest per-group maximum follow-up."),
          "i" = "The horizon defines the estimand; there is no default."),
        class = "spicy_invalid_input"
      )
    }
  } else if (!is.null(tau)) {
    spicy_abort(
      "`tau` was supplied but no RMST column is requested.",
      class = "spicy_invalid_input"
    )
  }
  if (wants_risk) {
    if (!ok_scalar(at_time)) {
      spicy_abort(
        c("Risk-difference columns need an explicit landmark: pass `at_time`.",
          "i" = paste0("A positive time on the outcome's scale (e.g. ",
                       "`at_time = 365`)."),
          "i" = "The landmark defines the estimand; there is no default."),
        class = "spicy_invalid_input"
      )
    }
  } else if (!is.null(at_time)) {
    spicy_abort(
      "`at_time` was supplied but no risk-difference column is requested.",
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}
