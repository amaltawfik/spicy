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
# Phase A — input class            (steps 1–3)
# Phase B — multi-model alignment  (steps 4–8)
# Phase C — vocabulary tokens      (steps 9–12)
# Phase D — argument values        (steps 13–24)
# Phase E — cross-arg semantic     (steps 25–26, warnings only)
# Phase F — output-dependent       (steps 27–29)


# ---- Token vocabularies (canonical) ---------------------------------------

# Phase 1 lm token vocabularies. Match dev/table_regression_design.md §4.
# Centralised here so validators, the rendering layer, and the test suite
# share a single source of truth.
.regression_tokens <- list(
  show_columns = c(
    "B", "beta", "SE", "CI", "t", "p",
    "partial_f2", "partial_eta2", "partial_omega2",
    "AME", "AME_p", "AME_SE"
  ),
  show_fit_stats = c(
    "nobs", "weighted_nobs",
    "r2", "adj_r2", "omega2",
    "sigma", "rmse",
    "f2",
    "AIC", "AICc", "BIC", "deviance"
  ),
  nested_stats_lm = c(
    "r2_change", "adj_r2_change",
    "F", "f2_change",
    "LRT", "AIC", "AICc", "BIC",
    "deviance_change", "p"
  )
)


# ---- Phase A — input class ------------------------------------------------

# Steps 1–3: validate `models` is lm OR list of lm; aggregate-fail on
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
        "i" = "Pass an `lm()` fit or a list of `lm()` fits."
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
        "i" = "Pass at least one lm fit."
      ),
      class = "spicy_invalid_input"
    )
  }

  # Step 1c: when the list is named, names must be unique. Duplicate
  # names would silently collide in the long-format model_id key,
  # causing one fit to overwrite the other in extract / align /
  # render — a silent data loss that's worth catching upfront.
  nms <- names(models)
  if (!is.null(nms) && any(nzchar(nms))) {
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

  # Steps 2–3: per-element class check, aggregate-fail
  problems <- vapply(seq_along(models), function(i) {
    msg <- classify_unsupported_lm_class(models[[i]], position = i)
    if (is.null(msg)) "" else msg
  }, character(1))
  bad <- problems[nzchar(problems)]
  if (length(bad) > 0L) {
    spicy_abort(
      c(
        "Some `models` are not supported in spicy 0.13 (Phase 1: lm only).",
        bad,
        "i" = "All models must be `lm()` fits."
      ),
      class = "spicy_unsupported"
    )
  }

  models
}

# Per-fit class classifier: NULL if OK, message string otherwise.
# Used by validate_models_input() to build the aggregate-fail message.
classify_unsupported_lm_class <- function(fit, position = NULL) {
  pos_prefix <- if (!is.null(position)) sprintf("Position %d: ", position) else ""

  if (inherits(fit, "glm")) {
    return(paste0(
      pos_prefix,
      "`glm()` model \u2014 supported in spicy 0.15.0. ",
      "If your model is OLS (gaussian / identity link), refit with `lm()` directly."
    ))
  }
  if (inherits(fit, c("lmerMod", "glmerMod", "merMod"))) {
    return(paste0(
      pos_prefix,
      sprintf(
        "`%s` mixed-effects model \u2014 supported in spicy 0.16+.",
        class(fit)[1]
      )
    ))
  }
  # Common user mistake: passing raw data instead of a fit (Q10).
  if (is.data.frame(fit)) {
    return(paste0(
      pos_prefix,
      "data.frame supplied where an `lm` fit is expected. ",
      "Fit a model first: ",
      "`fit <- lm(y ~ x, data = your_data); table_regression(fit)`."
    ))
  }
  if (!inherits(fit, "lm")) {
    return(paste0(
      pos_prefix,
      sprintf("`%s` \u2014 not on the roadmap. ", class(fit)[1]),
      "If support would be useful, please open an issue: ",
      "https://github.com/amaltawfik/spicy/issues"
    ))
  }
  NULL
}


# ---- Phase B — multi-model alignment --------------------------------------

# Steps 4–5: nested = TRUE requires identical nobs and identical DV
validate_nested_alignment <- function(models, nested) {
  if (!isTRUE(nested) || length(models) <= 1L) {
    return(invisible(NULL))
  }

  # Step 4: nobs identical
  nobs_vec <- vapply(models, stats::nobs, integer(1))
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

# Steps 6–8: vcov + cluster list/scalar coordination
validate_vcov_cluster_lists <- function(vcov, cluster, models) {
  n_models <- length(models)

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

    if (!is.null(c_i) && is.atomic(c_i)) {
      n_obs_i <- stats::nobs(models[[i]])
      if (length(c_i) != n_obs_i) {
        spicy_abort(
          sprintf(
            "`cluster%s` has length %d but model %d has %d observations.",
            if (n_models > 1L) sprintf("[[%d]]", i) else "",
            length(c_i), i, n_obs_i
          ),
          class = "spicy_invalid_input"
        )
      }
    }
  }

  invisible(NULL)
}


# ---- Phase C — vocabulary token validation --------------------------------

# Step 9: show_columns (+ Step 12: beta requires standardized != "none")
validate_show_columns <- function(show_columns, standardized) {
  validate_token_vector(
    show_columns,
    .regression_tokens$show_columns,
    arg = "show_columns"
  )

  # Step 12: beta-without-method
  if ("beta" %in% show_columns && identical(standardized, "none")) {
    spicy_abort(
      c(
        "`\"beta\"` is in `show_columns` but `standardized = \"none\"`.",
        "i" = paste0(
          "Set `standardized` to one of: \"refit\", \"posthoc\", ",
          "\"basic\", \"smart\"."
        ),
        "i" = "Or remove `\"beta\"` from `show_columns`."
      ),
      class = "spicy_invalid_input"
    )
  }
  invisible(NULL)
}

# Step 10: show_fit_stats. Empty character or NULL means "drop the
# fit-stats footer block" — a legitimate rendering choice (some
# users prefer the body alone). show_columns has no analogous
# escape hatch because a table with zero data columns is
# nonsensical.
validate_show_fit_stats <- function(show_fit_stats) {
  if (is.null(show_fit_stats) || length(show_fit_stats) == 0L) {
    return(invisible(NULL))
  }
  validate_token_vector(
    show_fit_stats,
    .regression_tokens$show_fit_stats,
    arg = "show_fit_stats"
  )
}

# Step 11: nested_stats (NULL = class-aware default applied later;
# when supplied, validate against Phase 1 lm tokens)
validate_nested_stats <- function(nested_stats) {
  if (is.null(nested_stats)) {
    return(invisible(NULL))
  }
  validate_token_vector(
    nested_stats,
    .regression_tokens$nested_stats_lm,
    arg = "nested_stats"
  )
}

# Generic token-vector validator used by show_*, nested_stats.
# Checks: character non-empty + no duplicates + all tokens in `valid`.
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


# ---- Phase D — argument value validation ----------------------------------

# Steps 13–14: enum args. table_regression() invokes match.arg() directly
# on `standardized`, `intercept_position`, `align`, `output`,
# `reference_style` — match.arg() raises a clear base-R error on
# invalid values. No spicy-specific helper needed.

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
  #     → e.g. "wt", "cyl", "wt:cyl", "I(x^2)"
  #   * coefficient names    (`names(coef(fit))`)
  #     → e.g. "(Intercept)", "cyl6", "factor(cyl)8", "wt:cyl6"
  # The renderer tries the per-row label first (coef name), then
  # falls back to the per-term label (factor variable). So both
  # flavours of key are useful: term keys rename factor headers,
  # coef keys rename individual contrast rows.
  all_terms <- unique(unlist(lapply(models, function(fit) {
    attr(stats::terms(fit), "term.labels")
  })))
  all_coefs <- unique(unlist(lapply(models, function(fit) {
    names(stats::coef(fit))
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


# ---- Phase E — cross-arg semantic warnings (no errors) --------------------

# Step 25: standardized != "none" × non-additive terms → spicy_caveat warning
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
      "\u03B2 uses SD of the product / transformed column, which differs ",
      "from SD(x) \u00D7 SD(z) and may be unstable."
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
      "i" = "Cohen, Cohen, West & Aiken (2003) \u00A77.7."
    ),
    class = "spicy_caveat"
  )
}

# Detect interactions and transforms in a fitted lm.
# Used by Q15 caveat emission (Step 25) and by the rendering layer
# to tag affected rows when needed.
detect_non_additive_terms <- function(fit) {
  trms <- attr(stats::terms(fit), "term.labels")
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
  if (!"AME" %in% show_columns) {
    return(FALSE)
  }
  vcov_per <- if (is.list(vcov)) vcov else list(vcov)
  any(vapply(vcov_per, function(v) startsWith(v, "CR"), logical(1)))
}


# ---- Phase F — output-dependent resource validation -----------------------

# Steps 27–29: file paths and package availability for the selected
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
      # nocov start — system clipboard is environment-dependent.
      spicy_abort(
        "Clipboard is not available on this system.",
        class = "spicy_unsupported"
      )
      # nocov end
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
