

# Frame-aware sibling of build_regression_title(). Reads from each
# frame's info$dv (DV name) and info$extras$title_prefix (family-aware
# label) instead of the legacy extract's $outcome and $title_prefix
# fields. Logic is identical -- byte-equivalence is tested in
# tests/testthat/test-renderer_migration_title.R.
#
# Phase 0c sub-step C1: first renderer migrated to consume frames
# directly. The flip in table_regression.R bypasses the
# .frame_to_legacy_extract() adapter for the title path; the other
# renderers (footer blocks, body builder, alignment) continue to
# consume the legacy extract shape until sub-steps C2-C4 land.
build_regression_title_from_frames <- function(frames, nested = FALSE) {
  if (!is.list(frames) || length(frames) == 0L) {
    return("Regression")
  }
  outcomes <- vapply(frames, function(f) {
    f$info$dv %||% NA_character_
  }, character(1))
  outcomes <- outcomes[!is.na(outcomes)]
  n <- length(frames)

  prefixes <- vapply(frames, function(f) {
    f$info$extras$title_prefix %||% "Regression"
  }, character(1))
  prefix <- if (length(unique(prefixes)) == 1L) {
    prefixes[1L]
  } else {
    "Regression"
  }

  if (n == 1L) {
    if (length(outcomes) == 0L) return(prefix)
    return(sprintf("%s: %s", prefix, outcomes[1]))
  }

  identical_dv <- length(unique(outcomes)) == 1L

  if (isTRUE(nested)) {
    lower_prefix <- paste0(
      tolower(substr(prefix, 1L, 1L)),
      substr(prefix, 2L, nchar(prefix))
    )
    return(sprintf("Hierarchical %s: %s", lower_prefix, outcomes[1]))
  }
  if (identical_dv) {
    return(sprintf("%s comparison: %s", prefix, outcomes[1]))
  }
  sprintf("%s comparison", prefix)
}


# Frame-aware dispatcher. Same signature as build_regression_footer()
# but reads frames instead of extracts. Calls each builder's
# _from_frames sibling (added in Phase 0c sub-steps C1, C2.a, C2.b,
# C2.c). build_stars_footer_block() and build_nested_footer_block()
# don't take an extracts arg -- they are reused unchanged.
#
# Phase 0c sub-step C2.last: orchestrator (table_regression.R) flips
# to this dispatcher. The legacy build_regression_footer() stays in
# the codebase until C5 cleanup so we can revert quickly if a corner
# case slips through the byte-equivalence gates.
build_regression_footer_from_frames <- function(
    frames,
    standardized = "none",
    p_adjust = "none",
    stars = FALSE,
    nested = FALSE,
    show_columns = character(0),
    reference_style = "row",
    show_re = TRUE,
    re_scale = "sd",
    re_columns = c("est", "se", "ci")) {
  themes <- list(
    build_regression_type_footer_block_from_frames(frames),
    build_vcov_footer_block_from_frames(frames),
    build_mixed_inference_footer_block_from_frames(frames),
    build_random_effects_footer_block_from_frames(frames,
                                                   show_re = show_re,
                                                   re_scale = re_scale,
                                                   re_columns = re_columns),
    build_survival_footer_block_from_frames(frames),
    build_ordinal_thresholds_footer_block_from_frames(frames),
    build_abbreviations_footer_block_from_frames(show_columns, frames,
                                                  standardized),
    build_ame_satterthwaite_footer_block_from_frames(frames, show_columns),
    build_exponentiate_footer_block_from_frames(frames),
    build_standardized_caveat_footer_block_from_frames(frames, standardized),
    build_p_adjust_footer_block_from_frames(frames, p_adjust),
    build_stars_footer_block(stars),
    build_singular_footer_block_from_frames(frames),
    build_polynomial_contrasts_footer_block_from_frames(frames),
    build_reference_categories_footer_block_from_frames(frames,
                                                        reference_style),
    build_nested_footer_block(nested)
  )
  themes <- Filter(function(x) !is.null(x) && nzchar(x), themes)
  if (length(themes) == 0L) return(NULL)
  paste0("Note. ", paste(themes, collapse = "\n"))
}

capitalize_first <- function(s) {
  if (!length(s) || !nzchar(s)) return(s)
  paste0(toupper(substr(s, 1L, 1L)), substring(s, 2L))
}


# Frame-aware sibling of build_regression_type_footer_block().
# Reads from frame$info$extras$title_prefix instead of extract$title_prefix.
# Logic and output are identical; byte-equivalence is tested in
# tests/testthat/test-renderer_migration_footer_simple.R.
#
# Phase 0c sub-step C2.a: migrating one footer-block builder at a time so
# each gets its own byte-equivalence gate. The dispatcher
# build_regression_footer() will keep consuming legacy extracts until
# every builder has a _from_frames sibling.
build_regression_type_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  # Phase 7b: preserve the engine-supplied acronym casing. The legacy
  # tolower() clobbered "(GAM)" / "(M-estimator)" / "(OLS)" that
  # per-class methods set deliberately. For sentence-leading positions
  # we apply capitalize_first(); for mid-sentence positions (after
  # "Model 1:") we only lowercase the FIRST character so the title
  # reads naturally without losing acronyms further in.
  types <- vapply(frames, function(f) {
    f$info$extras$title_prefix %||% "Regression"
  }, character(1))
  if (length(types) == 1L) {
    return(paste0(capitalize_first(types[1]), "."))
  }
  if (length(unique(types)) == 1L) {
    return(paste0(capitalize_first(types[1]), " models."))
  }
  per <- vapply(seq_along(types), function(i) {
    sprintf("Model %d: %s", i, lowercase_first(types[i]))
  }, character(1))
  paste0(paste(per, collapse = "; "), ".")
}


# Lowercase only the first character of a string (the inverse of
# capitalize_first()). Used to make a title read naturally in
# mid-sentence position ("Model 1: linear regression") while
# preserving any acronyms further in the string.
lowercase_first <- function(s) {
  if (!length(s) || !nzchar(s)) return(s)
  paste0(tolower(substr(s, 1L, 1L)), substring(s, 2L))
}


# Frame-aware sibling of format_vcov_label(). Reads from
#   frame$info$vcov_label          (Phase 1+: the engine-/class-specific
#                                   already-formatted label, e.g.
#                                   "Wald (model-based)", "Robust (HC2)",
#                                   "Bayesian (REML-implied)", ...)
#   frame$info$vcov_kind           (legacy: vcov-token from the
#                                   lm/glm path -- "classical" / "HC*"
#                                   / "CR*" / "bootstrap" / etc.)
#   frame$info$extras$cluster_name (was extract$cluster_name)
#   frame$info$class               (was extract$is_glm; derived)
#
# Routing: lm / glm go through the legacy classical/HC*/CR* derivation
# below (so historical snapshots stay byte-stable). All other classes
# use the engine-supplied `vcov_label` verbatim (which the Phase 1+
# methods set thoughtfully -- e.g. "Wald asymptotic (z)" for coxph,
# "Robust (HC2)" / "Cluster-robust (CR2)" for estimatr).
format_vcov_label_from_frame <- function(frame) {
  cls <- frame$info$class %||% ""

  # For non-lm/glm classes, the engine-supplied label is authoritative.
  if (!cls %in% c("lm", "glm")) {
    custom_label <- frame$info$vcov_label
    if (!is.null(custom_label) && is.character(custom_label) &&
        length(custom_label) == 1L && nzchar(custom_label)) {
      return(custom_label)
    }
  }

  vt <- frame$info$vcov_kind %||% "classical"
  cn <- frame$info$extras$cluster_name %||% NA_character_
  is_glm <- identical(cls, "glm")
  if (vt == "classical") {
    return(if (is_glm) "classical (MLE inverse Hessian)" else "classical (OLS)")
  }
  if (startsWith(vt, "HC")) {
    return(sprintf("heteroskedasticity-robust (%s)", vt))
  }
  if (startsWith(vt, "CR")) {
    cluster_part <- if (is.na(cn) || !nzchar(cn)) {
      "cluster vector supplied"
    } else {
      sprintf("clusters by %s", cn)
    }
    return(sprintf("cluster-robust (%s), %s", vt, cluster_part))
  }
  if (vt %in% c("bootstrap", "jackknife")) return(vt)
  vt
}


# Frame-aware sibling of build_vcov_footer_block().
build_vcov_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  labels <- vapply(frames, format_vcov_label_from_frame, character(1))
  if (all(labels == labels[1])) {
    return(paste0("Std. errors: ", labels[1], "."))
  }
  per <- vapply(seq_along(labels), function(i) {
    sprintf("  Model %d: %s", i, labels[i])
  }, character(1))
  paste0("Std. errors:\n", paste(per, collapse = "\n"))
}


# Frame-aware sibling of build_abbreviations_footer_block().
# Reads frame$info$extras$exp_applied (was extract$exp_applied) and
# frame$info$extras$exp_header (was extract$exp_header). show_columns
# and standardized stay scalar args (not per-fit), unchanged from the
# legacy signature.
build_abbreviations_footer_block_from_frames <- function(show_columns,
                                                          frames = list(),
                                                          standardized = "none") {
  defs <- character(0)

  if (any(c("ame", "ame_se", "ame_ci", "ame_p") %in% show_columns)) {
    defs <- c(defs, "AME = average marginal effect")
  }

  if (!identical(standardized, "none")) {
    defs <- c(defs, "\u03B2 = standardised coefficient")
  }

  if (is.list(frames) && length(frames) > 0L) {
    applied <- vapply(frames,
                      function(f) isTRUE(f$info$extras$exp_applied),
                      logical(1))
    if (any(applied)) {
      hdrs <- unique(vapply(frames[applied],
                            function(f) f$info$extras$exp_header,
                            character(1)))
      exp_defs <- c(
        "OR" = "OR = odds ratio",
        "IRR" = "IRR = incidence rate ratio",
        "HR" = "HR = hazard ratio",
        "RR" = "RR = risk ratio",
        "MR" = "MR = mean ratio",
        "exp(B)" = "exp(B) = exponentiated coefficient"
      )
      for (h in hdrs) {
        if (h %in% names(exp_defs)) defs <- c(defs, exp_defs[[h]])
      }
    }
  }

  if (any(c("partial_f2", "partial_f2_ci") %in% show_columns)) {
    defs <- c(defs, "f\u00B2 = Cohen's partial f\u00B2")
  }
  if (any(c("partial_eta2", "partial_eta2_ci") %in% show_columns)) {
    defs <- c(defs, "\u03B7\u00B2 = partial eta-squared")
  }
  if (any(c("partial_omega2", "partial_omega2_ci") %in% show_columns)) {
    defs <- c(defs, "\u03C9\u00B2 = bias-corrected partial omega-squared")
  }
  if ("partial_chi2" %in% show_columns) {
    defs <- c(defs, "\u03C7\u00B2 = partial likelihood-ratio chi-squared")
  }

  if (length(defs) == 0L) return(NULL)
  paste0(paste(defs, collapse = "; "), ".")
}


# Frame-aware sibling of build_ame_satterthwaite_footer_block(). Reads
#   frame$info$extras$use_ame_satterthwaite (was extract$use_ame_satterthwaite)
#   frame$info$class                         (was extract$is_glm; derived
#                                             as class == "glm")
build_ame_satterthwaite_footer_block_from_frames <- function(frames, show_columns) {
  if (!"ame" %in% show_columns) return(NULL)
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  any_satt <- any(vapply(frames,
                         function(f) isTRUE(f$info$extras$use_ame_satterthwaite),
                         logical(1)))
  if (!any_satt) return(NULL)
  any_lm  <- any(vapply(frames,
                        function(f) !identical(f$info$class, "glm"),
                        logical(1)))
  any_glm <- any(vapply(frames,
                        function(f) identical(f$info$class, "glm"),
                        logical(1)))
  mechanism <- if (any_lm && any_glm) {
    paste0(
      "via `clubSandwich` (closed-form `linear_contrast()` for `lm`; ",
      "dominant-coef `coef_test()` approximation for `glm`)."
    )
  } else if (any_glm) {
    paste0(
      "via `clubSandwich::coef_test()` on the dominant underlying ",
      "coefficient (response-scale AME is non-linear in \u03B2)."
    )
  } else {
    "via `clubSandwich::linear_contrast()`."
  }
  paste0(
    "AME inference: t-distribution with Satterthwaite-corrected df ",
    "(Pustejovsky & Tipton 2018) ", mechanism
  )
}


# Frame-aware sibling of build_standardized_caveat_footer_block().
# The fit lives at attr(frame, "fit") per Phase 0b sub-step 3; the
# non_additive metadata is attached by the orchestrator at
# table_regression.R:1234 to both frames[[i]]$info$extras$non_additive
# and extracts[[i]][["non_additive"]].
build_standardized_caveat_footer_block_from_frames <- function(frames,
                                                                standardized) {
  if (identical(standardized, "none")) return(NULL)
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  any_problem <- FALSE
  for (f in frames) {
    nonadd <- f$info$extras$non_additive
    if (is.null(nonadd)) {
      fit <- attr(f, "fit")
      if (!is.null(fit)) {
        nonadd <- detect_non_additive_terms(fit)
      }
    }
    if (isTRUE(nonadd$has_problem)) {
      any_problem <- TRUE
      break
    }
  }
  if (!any_problem) return(NULL)

  caveat <- if (identical(standardized, "refit")) {
    paste0(
      "Standardised \u03B2: after refit on z-scored data, \u03B2 for interaction ",
      "and transform terms reflects the interaction of z-scored ",
      "variables, not the standardisation of the original term."
    )
  } else {
    paste0(
      "Standardised \u03B2: for interaction and transform terms, \u03B2 uses ",
      "SD of the product / transformed column, which differs from ",
      "SD(x) \u00D7 SD(z) and may be unstable (Cohen et al. 2003 \u00A77.7)."
    )
  }
  caveat
}


# ---- Theme: stars mapping (Q12) ------------------------------------------

# stars = FALSE                  -> NULL (no footer note)
# stars = TRUE                   -> APA preset:
#   "*** p < .001, ** p < .01, * p < .05."
# stars = c("+"=.10, "*"=.05)    -> "* p < .05, + p < .10."
# Mapping rendered in increasing-strictness order (smallest p first
# at the lead) mirroring stargazer / modelsummary convention.
build_stars_footer_block <- function(stars) {
  if (isFALSE(stars) || is.null(stars)) return(NULL)
  if (isTRUE(stars)) {
    stars <- c("*" = 0.05, "**" = 0.01, "***" = 0.001)
  }
  if (!is.numeric(stars) || is.null(names(stars))) return(NULL)
  ord <- order(stars)               # smallest p first => strictest symbol first
  sym <- names(stars)[ord]
  thr <- stars[ord]
  parts <- vapply(seq_along(sym), function(i) {
    sprintf("%s p < %s", sym[i], format_p_threshold(thr[i]))
  }, character(1))
  paste0(paste(parts, collapse = ", "), ".")
}

# APA-style threshold formatting: leading dot, minimum 2 decimal
# digits, trailing zeros trimmed beyond that. Examples:
#   0.001 -> ".001",  0.01 -> ".01",  0.05 -> ".05",  0.10 -> ".10".
format_p_threshold <- function(p) {
  if (!is.finite(p) || p <= 0 || p > 1) return(format(p))
  s <- formatC(p, format = "f", digits = 3)   # always 3 decimals first
  s <- sub("^0", "", s)                       # ".050" / ".100" / ".001"
  # Trim trailing zeros but keep at least 2 decimal digits ("." + 2 chars).
  while (nchar(s) > 3L && substr(s, nchar(s), nchar(s)) == "0") {
    s <- substr(s, 1L, nchar(s) - 1L)
  }
  s
}


# Frame-aware sibling of build_singular_footer_block().
# Reads frame$info$extras$has_singular instead of extract$has_singular.
# Phase 7c3: ordinal-thresholds footer block for cumulative-link
# proportional-odds fits (polr, clm). Surfaces the (k - 1) cut-points
# from info$extras$thresholds (a data.frame with term / estimate /
# std_error / statistic / p_value).
#
# Single-model output:
#   "Thresholds: 1|2 = -1.34, 2|3 = 1.25, 3|4 = 3.47, 4|5 = 5.01."
#
# Multi-model: "Model k:" prefix only when more than one frame
# contributes content.
build_ordinal_thresholds_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)

  per_model <- lapply(seq_along(frames), function(i) {
    f <- frames[[i]]
    txt <- .format_ordinal_thresholds_for_frame(f)
    if (is.null(txt)) NULL else list(idx = i, text = txt)
  })
  per_model <- Filter(Negate(is.null), per_model)
  if (length(per_model) == 0L) return(NULL)

  if (length(per_model) == 1L) return(per_model[[1L]]$text)
  lines <- vapply(per_model, function(pm) {
    sprintf("Model %d: %s", pm$idx, pm$text)
  }, character(1))
  paste(lines, collapse = "\n")
}


.format_ordinal_thresholds_for_frame <- function(frame) {
  cls <- frame$info$class %||% ""
  if (!cls %in% c("polr", "clm")) return(NULL)
  th <- frame$info$extras$thresholds
  if (is.null(th) || !is.data.frame(th) || nrow(th) == 0L) return(NULL)
  parts <- vapply(seq_len(nrow(th)), function(i) {
    sprintf("%s = %.2f", th$term[i], th$estimate[i])
  }, character(1))
  paste0("Thresholds: ", paste(parts, collapse = ", "), ".")
}


# Phase 7c2: survival-specific footer block for coxph / cph / survreg /
# flexsurvreg. Surfaces events count + concordance C (Cox) or
# distribution + scale (parametric AFT).
#
# Single-model output examples:
#   coxph / cph: "Events: 165 of 228; Concordance C = 0.60 (SE = 0.03)."
#   survreg:    "Distribution: Weibull; scale = 0.75."
#   flexsurv:   "Distribution: Weibull; shape = 1.33, scale = 531.05."
build_survival_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)

  per_model <- lapply(seq_along(frames), function(i) {
    f <- frames[[i]]
    txt <- .format_survival_for_frame(f)
    if (is.null(txt)) NULL else list(idx = i, text = txt)
  })
  per_model <- Filter(Negate(is.null), per_model)
  if (length(per_model) == 0L) return(NULL)

  # Prefix with "Model k:" only when MORE THAN ONE frame contributes
  # content; a single contributing model gets the line bare (no
  # misleading "Model 1:" when its peers are silent on this block).
  if (length(per_model) == 1L) return(per_model[[1L]]$text)
  lines <- vapply(per_model, function(pm) {
    sprintf("Model %d: %s", pm$idx, pm$text)
  }, character(1))
  paste(lines, collapse = "\n")
}


.format_survival_for_frame <- function(frame) {
  cls <- frame$info$class %||% ""
  if (cls %in% c("coxph", "cph")) {
    .format_coxph_survival(frame)
  } else if (cls == "survreg") {
    .format_survreg_survival(frame)
  } else if (cls == "flexsurvreg") {
    .format_flexsurv_survival(frame)
  } else {
    NULL
  }
}


.format_coxph_survival <- function(frame) {
  ev    <- frame$info$extras$n_events
  n_obs <- frame$info$n_obs
  conc  <- frame$info$extras$concordance
  parts <- character(0)
  if (!is.null(ev) && is.finite(ev) && !is.null(n_obs)) {
    parts <- c(parts, sprintf("Events: %d of %d", as.integer(ev),
                              as.integer(n_obs)))
  }
  if (!is.null(conc) && is.list(conc) &&
      !is.null(conc$c) && is.finite(conc$c)) {
    if (!is.null(conc$se) && is.finite(conc$se)) {
      parts <- c(parts, sprintf("Concordance C = %.2f (SE = %.2f)",
                                conc$c, conc$se))
    } else {
      parts <- c(parts, sprintf("Concordance C = %.2f", conc$c))    # nocov
    }
  }
  if (length(parts) == 0L) return(NULL)                              # nocov
  paste0(paste(parts, collapse = "; "), ".")
}


.format_survreg_survival <- function(frame) {
  dist  <- frame$info$extras$distribution
  scale <- frame$info$extras$scale_parameter
  parts <- character(0)
  if (!is.null(dist) && is.character(dist) && nzchar(dist)) {
    parts <- c(parts, sprintf("Distribution: %s",
                              .surv_title_dist(dist)))
  }
  if (!is.null(scale) && is.finite(scale)) {
    parts <- c(parts, sprintf("scale = %.2f", scale))
  }
  if (length(parts) == 0L) return(NULL)                              # nocov
  paste0(paste(parts, collapse = "; "), ".")
}


.format_flexsurv_survival <- function(frame) {
  dist <- frame$info$extras$distribution
  aux  <- frame$info$extras$aux_parameters
  parts <- character(0)
  if (!is.null(dist) && is.character(dist) && nzchar(dist)) {
    parts <- c(parts, sprintf("Distribution: %s",
                              .surv_title_dist(dist)))
  }
  if (!is.null(aux) && is.numeric(aux) && length(aux) > 0L) {
    aux_str <- paste(sprintf("%s = %.2f", names(aux), aux),
                     collapse = ", ")
    parts <- c(parts, aux_str)
  }
  if (length(parts) == 0L) return(NULL)                              # nocov
  paste0(paste(parts, collapse = "; "), ".")
}


.surv_title_dist <- function(dist) {
  switch(dist,
    weibull       = "Weibull",
    weibullPH     = "Weibull (PH)",
    lognormal     = "Log-normal",
    lnorm         = "Log-normal",
    gompertz      = "Gompertz",
    gamma         = "Gamma",
    exponential   = "Exponential",
    exp           = "Exponential",
    llogis        = "Log-logistic",
    loglogistic   = "Log-logistic",
    gengamma      = "Generalised gamma",
    genf          = "Generalised F",
    gaussian      = "Gaussian",
    paste0(toupper(substr(dist, 1L, 1L)), substring(dist, 2L))
  )
}


# Phase 7c1: random-effects footer block for mixed-effects fits.
# Reads from frame$info$random_effects$variance_components (data.frame
# with group / term / variance / sd / corr columns) and
# frame$info$random_effects$icc (scalar). Skipped silently when no
# mixed-effects component is present (variance_components is empty).
#
# Single-model output:
#   "Random effects: 18 Subjects; intercept variance = 1296.87,
#    residual variance = 954.53; ICC = 0.58."
# Multi-model: one line per affected model, prefixed "Model k:".
build_random_effects_footer_block_from_frames <- function(
    frames,
    show_re = TRUE,
    re_scale = "sd",
    re_columns = c("est", "se", "ci")) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  # Phase 7c7d: user-disabled panel returns NULL early -- the random-
  # effects block is fully suppressed from the footer.
  if (!isTRUE(show_re)) return(NULL)

  per_model <- lapply(seq_along(frames), function(i) {
    f <- frames[[i]]
    txt <- .format_random_effects_for_frame(f, re_scale = re_scale,
                                              re_columns = re_columns)
    if (is.null(txt)) NULL else list(idx = i, text = txt)
  })
  per_model <- Filter(Negate(is.null), per_model)
  if (length(per_model) == 0L) return(NULL)

  # Prefix with "Model k:" only when more than one frame contributes
  # content (parity with the survival footer's convention).
  if (length(per_model) == 1L) return(per_model[[1L]]$text)
  lines <- vapply(per_model, function(pm) {
    sprintf("Model %d: %s", pm$idx, pm$text)
  }, character(1))
  paste(lines, collapse = "\n")
}


# Phase 7c8a: per-frame fixed-effect inference annotation for mixed-
# effects fits. Returns NULL for non-mixed classes; otherwise a single
# line stating the distribution + df strategy + source package, so a
# publication-quality reader can tell at a glance whether the
# p-values / CIs are Satterthwaite (the recommended default for
# lmer when sample sizes are small), Wald-z (large-sample
# approximation), or nlme's containment df.
#
# Single-model output (no prefix):
#   "p-values: Satterthwaite t-test (lmerTest)."
# Multi-model: prefixed "Model k: ...".
build_mixed_inference_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)

  per_model <- lapply(seq_along(frames), function(i) {
    f <- frames[[i]]
    txt <- .mixed_inference_label_for_frame(f)
    if (is.null(txt)) NULL else list(idx = i, text = txt)
  })
  per_model <- Filter(Negate(is.null), per_model)
  if (length(per_model) == 0L) return(NULL)

  if (length(per_model) == 1L) return(per_model[[1L]]$text)
  # Consolidate when every model produced the SAME annotation -- avoids
  # printing three identical "p-values: Wald-z, ..." lines when the
  # whole list is, say, lme4::lmer without lmerTest. Mirrors the
  # consolidation done by build_regression_type_footer_block_from_frames().
  texts <- vapply(per_model, function(pm) pm$text, character(1))
  if (length(unique(texts)) == 1L) return(texts[1L])
  lines <- vapply(per_model, function(pm) {
    sprintf("Model %d: %s", pm$idx, pm$text)
  }, character(1))
  paste(lines, collapse = "\n")
}


# Pick the right one-line annotation for a single frame. NULL when
# the frame is not a mixed-effects fit.
#
# Dispatch on (class, ci_method) instead of just class: lmer fits
# are normalised to class == "lmerMod" regardless of whether
# lmerTest decorated the fit, so we read ci_method to tell apart
# Satterthwaite ("satterthwaite") from Wald-z fallback ("wald").
.mixed_inference_label_for_frame <- function(frame) {
  cls <- frame$info$class %||% ""
  ci  <- frame$info$ci_method %||% ""
  if (cls == "lmerMod" && identical(ci, "satterthwaite")) {
    return("p-values: Satterthwaite t-test (lmerTest).")
  }
  switch(
    cls,
    "lmerMod" =
      paste0("p-values: Wald-z, large-sample approximation. ",
             "Load `lmerTest` for Satterthwaite t-tests."),
    "glmerMod" =
      "p-values: Wald-z asymptotic (lme4).",
    "glmmTMB" =
      "p-values: Wald-z asymptotic (glmmTMB).",
    "lme" =
      "p-values: t-test with containment df (nlme).",
    NULL
  )
}


# Phase 7c7b: convert a variance_components data.frame between the
# variance scale (canonical storage in info$random_effects, where SE
# and CI live on sigma^2) and the SD scale (the publication-friendly
# default per Gelman, ARM). Returns a NEW data.frame with the requested
# scale applied to the `variance` / `sd` / `std_error` / `ci_lower` /
# `ci_upper` columns.
#
# Delta-method derivations:
#   * SD <- sqrt(variance) (monotonic, fine for boundary at 0)
#   * SE(sigma)  = SE(sigma^2) / (2 * sigma)
#   * CI(sigma)  = sqrt(CI(sigma^2))  -- monotonic on sigma^2 >= 0
#   * For correlation rows (is_correlation == TRUE), no scale change
#     applies: \u03C1 is unitless, so the values pass through unchanged.
.re_components_on_scale <- function(vc_df, target_scale = c("sd", "variance")) {
  target_scale <- match.arg(target_scale)
  if (!is.data.frame(vc_df) || nrow(vc_df) == 0L) return(vc_df)
  if (identical(target_scale, "variance")) return(vc_df)

  out <- vc_df
  is_corr <- if ("is_correlation" %in% colnames(vc_df)) {
    isTRUE(vc_df$is_correlation) | vc_df$is_correlation %in% TRUE
  } else {
    rep(FALSE, nrow(vc_df))
  }
  # For non-correlation rows: convert variance -> sd via sqrt + Delta-method.
  for (i in seq_len(nrow(vc_df))) {
    if (isTRUE(is_corr[i])) next  # correlation row, leave as-is

    var_est <- vc_df$variance[i]
    if (is.finite(var_est) && var_est >= 0) {
      out$variance[i] <- sqrt(var_est)  # column repurposed as "estimate"
      out$sd[i]       <- sqrt(var_est)
    } else {
      out$variance[i] <- NA_real_                                       # nocov
      out$sd[i]       <- NA_real_                                       # nocov
    }

    if ("std_error" %in% colnames(vc_df)) {
      se_var <- vc_df$std_error[i]
      sd_val <- out$sd[i]
      if (is.finite(se_var) && is.finite(sd_val) && sd_val > 0) {
        out$std_error[i] <- se_var / (2 * sd_val)
      } else {
        out$std_error[i] <- NA_real_
      }
    }
    for (col in c("ci_lower", "ci_upper")) {
      if (col %in% colnames(vc_df)) {
        v <- vc_df[[col]][i]
        out[[col]][i] <- if (is.finite(v) && v >= 0) sqrt(v) else NA_real_
      }
    }
  }
  out
}


# Phase 7c7c: structured multi-line panel for the random-effects
# footer block. Replaces the legacy one-line sentence when SE / CI
# are populated. Format is sjPlot / MLwiN-inspired, on the SD scale
# (per Gelman 2005), aligned with whitespace padding so each column
# (label, estimate, SE, CI) lines up vertically.
#
# Layout (single grouping factor, random intercept + slope):
#
#   Random effects (REML):
#     sigma Subject (Intercept)  37.12  (5.84)  [27.2, 51.1]
#     sigma Subject Days          5.92  (1.25)  [ 4.3,  7.9]
#     rho Subject                 0.07     \u2013     \u2013
#     sigma (Residual)           30.99  (1.51)  [28.5, 33.7]
#     ICC                         0.59
#     N (Subject)                   18
#
# (Greek letters rendered as their unicode glyphs.)
#
# Returns NULL when no rows would render (graceful skip), so the
# caller can fall back to the legacy sentence.
.format_random_effects_panel <- function(
    frame,
    vc_var_scale,
    re_scale = "sd",
    re_columns = c("est", "se", "ci")) {
  re <- frame$info$random_effects
  # Phase 7c7d: convert to the user-requested display scale.
  vc <- .re_components_on_scale(vc_var_scale, re_scale)
  if (nrow(vc) == 0L) return(NULL)                                     # nocov
  show_se <- "se" %in% re_columns
  show_ci <- "ci" %in% re_columns

  is_corr <- if ("is_correlation" %in% colnames(vc)) {
    vc$is_correlation %in% TRUE
  } else {
    rep(FALSE, nrow(vc))
  }
  is_resid <- !is.na(vc$group) & vc$group == "Residual"

  # Build per-row (label, value, se, ci) text tuples.
  rows <- list()
  for (i in seq_len(nrow(vc))) {
    label <- .re_panel_label(vc$group[i], vc$term[i],
                              is_corr[i], is_resid[i])
    est   <- if (isTRUE(is_corr[i])) vc$corr[i] else vc$variance[i]
    se    <- vc$std_error[i]
    cilo  <- vc$ci_lower[i]
    cihi  <- vc$ci_upper[i]
    rows[[length(rows) + 1L]] <- list(
      label = label,
      val   = if (is.finite(est))  sprintf("%.2f", est) else "\u2013",
      se    = if (is.finite(se))   sprintf("(%.2f)", se) else "\u2013",
      ci    = if (is.finite(cilo) && is.finite(cihi)) {
        sprintf("[%.2f, %.2f]", cilo, cihi)
      } else "\u2013"
    )
  }

  # ICC row (no SE/CI -- it's a derived quantity).
  icc_val <- re$icc
  if (!is.null(icc_val) && is.finite(icc_val)) {
    rows[[length(rows) + 1L]] <- list(
      label = "ICC",
      val   = sprintf("%.2f", icc_val),
      se    = "",
      ci    = ""
    )
  }

  # N (group) row -- integer counts.
  ng <- frame$info$n_groups
  if (!is.null(ng) && length(ng) > 0L) {
    for (k in seq_along(ng)) {
      rows[[length(rows) + 1L]] <- list(
        label = sprintf("N (%s)", names(ng)[k]),
        val   = sprintf("%d", as.integer(ng[[k]])),
        se    = "",
        ci    = ""
      )
    }
  }

  if (length(rows) == 0L) return(NULL)                                 # nocov

  # Column widths for alignment.
  labels <- vapply(rows, function(r) r$label, character(1))
  vals   <- vapply(rows, function(r) r$val,   character(1))
  ses    <- vapply(rows, function(r) r$se,    character(1))
  cis    <- vapply(rows, function(r) r$ci,    character(1))
  w_lab  <- max(nchar(labels))
  w_val  <- max(nchar(vals))
  w_se   <- max(nchar(ses))
  w_ci   <- max(nchar(cis))

  # Header line with method tag.
  method_tag <- re$method
  header <- if (!is.null(method_tag) && is.character(method_tag) &&
                length(method_tag) == 1L && !is.na(method_tag) &&
                nzchar(method_tag)) {
    sprintf("Random effects (%s):", method_tag)
  } else {
    "Random effects:"
  }

  # Phase 7c7d: drop SE / CI columns from the rendered rows when the
  # user opted out via re_columns. Estimate is always present.
  body_lines <- vapply(seq_along(rows), function(i) {
    parts <- c(
      sprintf("  %-*s", w_lab, labels[i]),
      sprintf("%*s",    w_val, vals[i])
    )
    if (isTRUE(show_se)) parts <- c(parts, sprintf("%-*s", w_se, ses[i]))
    if (isTRUE(show_ci)) parts <- c(parts, sprintf("%-*s", w_ci, cis[i]))
    paste(parts, collapse = "  ")
  }, character(1))
  # Trim trailing whitespace for ICC / N rows (which have empty se / ci).
  body_lines <- sub("\\s+$", "", body_lines)

  paste(c(header, body_lines), collapse = "\n")
}


# Label for a single row of the random-effects panel.
.re_panel_label <- function(group, term, is_correlation, is_residual) {
  if (isTRUE(is_residual)) return("\u03C3 (Residual)")
  if (isTRUE(is_correlation)) {
    return(sprintf("\u03C1 %s (%s)", group, term))
  }
  if (is.na(term) || !nzchar(term) || term == "(Intercept)") {
    return(sprintf("\u03C3 %s (Intercept)", group))
  }
  sprintf("\u03C3 %s %s", group, term)
}


# Format the per-frame random-effects sentence. Returns NULL when the
# frame has no random-effects content (lm / glm / coxph / etc.).
.format_random_effects_for_frame <- function(
    frame,
    re_scale = "sd",
    re_columns = c("est", "se", "ci")) {
  re <- frame$info$random_effects
  if (is.null(re)) return(NULL)
  vc <- re$variance_components
  if (is.null(vc) || !is.data.frame(vc) || nrow(vc) == 0L) return(NULL)

  # Phase 7c7c: when SE / CI are populated (Phase 7c7a+b), render a
  # multi-line aligned panel instead of the legacy one-line sentence.
  # The panel matches sjPlot / MLwiN-style publication layout per
  # Gelman 2005 (SD scale) + Bates' guidance on profile-CI absence
  # of p-values for variance components.
  has_se <- "std_error" %in% colnames(vc) && any(is.finite(vc$std_error))
  if (isTRUE(has_se)) {
    panel <- .format_random_effects_panel(frame, vc,
                                            re_scale = re_scale,
                                            re_columns = re_columns)
    if (!is.null(panel)) return(panel)
  }

  # Identify the (single) primary grouping factor row(s) vs the residual row.
  is_resid <- !is.na(vc$group) & vc$group == "Residual"
  group_rows <- vc[!is_resid, , drop = FALSE]
  resid_rows <- vc[is_resid,  , drop = FALSE]

  # Sample-size sentence: "18 Subjects" (or "5 Months and 18 Subjects" for
  # multiple grouping factors). Derived from frame$info$n_groups since
  # this is more reliable than the var-components nrow. Phase 7c6:
  # append "(REML)" or "(ML)" when the per-class method has set
  # re$method -- clarifies the estimator without implying inference
  # (variance-component p-values are non-standard at the boundary;
  # see Self & Liang 1987 + Brauer & Curtin 2018).
  ng <- frame$info$n_groups
  n_groups_part <- if (!is.null(ng) && length(ng) > 0L) {
    parts <- vapply(seq_along(ng), function(k) {
      sprintf("%d %s%s", ng[[k]], names(ng)[k],
              if (ng[[k]] > 1L) "s" else "")
    }, character(1))
    base <- paste(parts, collapse = " and ")
    method_tag <- re$method
    if (!is.null(method_tag) && is.character(method_tag) &&
        length(method_tag) == 1L && !is.na(method_tag) && nzchar(method_tag)) {
      paste0(base, " (", method_tag, ")")
    } else {
      base
    }
  } else NA_character_

  # Variance components sentence: list each non-residual row + residual.
  # Phase 7c7b: skip correlation rows (is_correlation == TRUE); the
  # rho is unitless and doesn't fit the "X variance = Y" sentence
  # shape. The full panel rendering (Phase 7c7c) surfaces rho on its
  # own row when present.
  comp_parts <- character(0)
  if (nrow(group_rows) > 0L) {
    is_corr <- if ("is_correlation" %in% colnames(group_rows)) {
      group_rows$is_correlation %in% TRUE
    } else {
      rep(FALSE, nrow(group_rows))
    }
    for (i in seq_len(nrow(group_rows))) {
      if (isTRUE(is_corr[i])) next
      term_label <- if (!is.na(group_rows$term[i]) && nzchar(group_rows$term[i])) {
        tolower(group_rows$term[i])
      } else NA_character_
      v <- .fmt_var(group_rows$variance[i])
      if (is.na(term_label) || term_label == "(intercept)") {
        comp_parts <- c(comp_parts, paste0("intercept variance = ", v))
      } else {
        comp_parts <- c(comp_parts,
                        sprintf("%s slope variance = %s", term_label, v))
      }
    }
  }
  if (nrow(resid_rows) > 0L && is.finite(resid_rows$variance[1L])) {
    comp_parts <- c(comp_parts,
                    paste0("residual variance = ",
                           .fmt_var(resid_rows$variance[1L])))
  }
  components_part <- if (length(comp_parts) > 0L) {
    paste(comp_parts, collapse = ", ")
  } else NA_character_

  # ICC sentence.
  icc_val <- re$icc
  icc_part <- if (!is.null(icc_val) && is.finite(icc_val)) {
    sprintf("ICC = %.2f", icc_val)
  } else NA_character_

  # Assemble. Skip parts that are NA (e.g. icc on non-Gaussian glmer).
  segments <- c(n_groups_part, components_part, icc_part)
  segments <- segments[!is.na(segments)]
  if (length(segments) == 0L) return(NULL)
  paste0("Random effects: ", paste(segments, collapse = "; "), ".")
}


# Format a variance estimate for the random-effects sentence. Switches
# to scientific notation when the value is very small (< 0.001) to
# avoid printing "0.00" for boundary-singular fits where the variance
# is near zero but informatively non-zero.
.fmt_var <- function(x) {
  if (!is.finite(x)) return("NA")
  if (x < 0.001 && x > 0) return(formatC(x, format = "e", digits = 2))
  sprintf("%.2f", x)
}


build_singular_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  flags <- vapply(frames, function(f) {
    isTRUE(f$info$extras$has_singular)
  }, logical(1))
  if (!any(flags)) return(NULL)
  affected <- which(flags)
  if (length(frames) == 1L) {
    return("Rank-deficient model: dropped coefficient(s) shown as \u2013.")
  }
  paste0(
    "Rank-deficient model(s) ",
    paste(sprintf("Model %d", affected), collapse = ", "),
    ": dropped coefficient(s) shown as \u2013."
  )
}


# Frame-aware sibling of build_exponentiate_footer_block(). Reads
#   frame$info$extras$exp_applied (was extract$exp_applied)
#   frame$info$extras$exp_header  (was extract$exp_header)
build_exponentiate_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  applied <- vapply(frames,
                    function(f) isTRUE(f$info$extras$exp_applied),
                    logical(1))
  if (!any(applied)) return(NULL)
  hdrs <- unique(vapply(frames[applied],
                        function(f) f$info$extras$exp_header,
                        character(1)))
  # The SE column is reported on the exp() scale via the Delta
  # method (SE_exp = exp(B) * SE_link). That mechanic is documented
  # in the `?table_regression` help; the footer keeps a one-line
  # "what the column shows" rather than a "how it was computed"
  # methods sentence that would otherwise clutter the table note.
  if (length(hdrs) == 1L) {
    hdr <- hdrs[1L]
    return(sprintf(
      "Coefficients exponentiated and displayed as %s; CI bounds exponentiated.",
      hdr
    ))
  }
  paste0(
    "Coefficients exponentiated and displayed as ",
    paste(hdrs, collapse = " / "),
    " (per family); CI bounds exponentiated."
  )
}


# Frame-aware sibling of build_p_adjust_footer_block().
# Column mapping (legacy -> frame, established in Phase 0b sub-step 2):
#   cf$estimate_type -> coefs$estimate_type   (same column name)
#   cf$is_intercept  -> derived (term == "(Intercept)")
#   cf$is_reference  -> coefs$is_ref          (renamed)
#   cf$p_value       -> coefs$p_value         (same)
build_p_adjust_footer_block_from_frames <- function(frames, p_adjust) {
  if (identical(p_adjust, "none") || is.null(p_adjust) ||
        !is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  sizes <- vapply(frames, function(f) {
    cf <- f$coefs
    if (is.null(cf) || nrow(cf) == 0L) return(0L)
    sum(cf$estimate_type == "B" &
          cf$term != "(Intercept)" &
          !cf$is_ref &
          !is.na(cf$p_value))
  }, integer(1))
  size_part <- if (length(unique(sizes)) == 1L) {
    sprintf("m = %d coefficient(s) per model", sizes[1L])
  } else {
    sprintf("m = (%s) coefficient(s) per model",
            paste(sizes, collapse = ", "))
  }
  sprintf(
    "P-values adjusted via stats::p.adjust(method = %s); %s.",
    shQuote(p_adjust), size_part
  )
}


# Frame-aware sibling of build_polynomial_contrasts_footer_block().
# Column mapping:
#   coefs$factor_level -> coefs$label   (renamed; never NA in frame --
#                                        fallback to term for non-factor
#                                        predictors). The startsWith
#                                        check on "." or "^" still
#                                        correctly identifies poly rows.
#   coefs$factor_term  -> coefs$parent_var
build_polynomial_contrasts_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  poly_vars <- character(0)
  poly_suffixes <- character(0)
  for (f in frames) {
    coefs <- f$coefs
    if (is.null(coefs) || nrow(coefs) == 0L) next
    is_poly <- !is.na(coefs$label) &
      (startsWith(coefs$label, ".") |
         startsWith(coefs$label, "^"))
    if (any(is_poly)) {
      poly_vars <- union(poly_vars,
                         unique(coefs$parent_var[is_poly]))
      poly_suffixes <- union(poly_suffixes,
                             unique(coefs$label[is_poly]))
    }
  }
  if (length(poly_vars) == 0L) return(NULL)
  inform_polynomial_pedagogy()

  vars_str <- paste0("`", poly_vars, "`", collapse = ", ")
  plural <- length(poly_vars) > 1L

  suffix_rank <- function(s) {
    if (s == ".L") return(1)
    if (s == ".Q") return(2)
    if (s == ".C") return(3)
    if (startsWith(s, "^")) {
      n <- suppressWarnings(as.integer(substring(s, 2L)))
      if (!is.na(n)) return(as.numeric(n))
    }
    NA_real_
  }
  suffix_label <- function(s) {
    switch(s,
      ".L" = "linear",
      ".Q" = "quadratic",
      ".C" = "cubic",
      if (startsWith(s, "^")) {
        n <- suppressWarnings(as.integer(substring(s, 2L)))
        if (!is.na(n)) ordinal_label(n) else s
      } else s
    )
  }
  suff_ord <- order(vapply(poly_suffixes, suffix_rank, numeric(1)))
  poly_suffixes <- poly_suffixes[suff_ord]
  legend <- paste(
    vapply(poly_suffixes,
            function(s) sprintf("%s = %s", s, suffix_label(s)),
            character(1)),
    collapse = ", "
  )

  paste0(
    if (plural) "Ordered factors " else "Ordered factor ",
    vars_str, ": polynomial trends (", legend, ")."
  )
}


# English ordinal labels for polynomial degrees >= 4 (.L .Q .C are
# special-cased in suffix_label). "^4" -> "quartic", "^5" -> "quintic",
# "^k" for k > 6 falls back to "degree-k".
ordinal_label <- function(k) {
  switch(as.character(k),
    "4" = "quartic",
    "5" = "quintic",
    "6" = "sextic",
    sprintf("degree-%d", k)
  )
}

# Pedagogical inform fired ONCE per R session the first time any
# ordered (contr.poly) factor is rendered by table_regression().
# Uses rlang::inform's built-in frequency mechanism: subsequent
# calls in the same session with the same `.frequency_id` are
# muffled. Resets at the start of a new R session, which is the
# correct cadence for a "did you know" hint.
inform_polynomial_pedagogy <- function() {
  rlang::inform(
    c(paste0("Ordered factor(s) detected. Polynomial contrasts ",
             "(the R default for `ordered()`) decompose the factor ",
             "into orthogonal trend components: `.L` = linear, ",
             "`.Q` = quadratic, `.C` = cubic, `^k` = degree k. ",
             "Coefficients are trends across the ordered levels, ",
             "NOT per-level effects against a reference."),
      "i" = paste0("To display per-level (treatment) effects, refit ",
                    "with `factor(x, ordered = FALSE)` or set ",
                    "`options(contrasts = c(\"contr.treatment\", ",
                    "\"contr.treatment\"))`.")),
    class = "spicy_polynomial_contrasts_info",
    .frequency = "once",
    .frequency_id = "spicy_polynomial_contrasts_info"
  )
}


# Frame-aware sibling of build_reference_categories_footer_block().
# Column mapping:
#   coefs$is_reference -> coefs$is_ref       (renamed)
#   coefs$factor_term  -> coefs$parent_var   (renamed; fallback to term
#                                             handled at reshape time;
#                                             for is_ref rows the
#                                             parent_var IS the factor
#                                             variable name)
#   coefs$factor_level -> coefs$label        (renamed; for is_ref rows
#                                             the label IS the reference
#                                             level name)
build_reference_categories_footer_block_from_frames <- function(frames,
                                                                reference_style) {
  if (!identical(reference_style, "footer")) return(NULL)
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  pairs <- character(0)
  seen <- character(0)
  for (f in frames) {
    coefs <- f$coefs
    if (is.null(coefs) || nrow(coefs) == 0L) next
    ref_rows <- coefs[isTRUE_vec(coefs$is_ref), , drop = FALSE]
    if (nrow(ref_rows) == 0L) next
    for (i in seq_len(nrow(ref_rows))) {
      ft  <- ref_rows$parent_var[i]
      lvl <- ref_rows$label[i]
      if (is.na(ft) || !nzchar(ft) || is.na(lvl) || !nzchar(lvl)) next
      key <- paste0(ft, "=", lvl)
      if (key %in% seen) next
      seen <- c(seen, key)
      pairs <- c(pairs, sprintf("%s = %s", ft, lvl))
    }
  }
  if (!length(pairs)) return(NULL)
  paste0("Reference categories: ", paste(pairs, collapse = "; "), ".")
}


# Vectorised isTRUE: maps each element to TRUE iff it is TRUE
# (length-1, non-NA, non-FALSE). Used in the footer collector
# above where `coefs$is_reference` may carry NAs for non-factor
# rows in some bind paths.
isTRUE_vec <- function(x) {
  !is.na(x) & as.logical(x)
}


# ---- Theme: nested declaration (Q6) --------------------------------------

# Returns NULL by design: the nested comparison block emitted by
# format_nested_comparison_footer() (Step 9) is self-explanatory under
# its "-- Model comparison --" header. Adding a paragraph that
# announces it would just be redundant noise above the block. The
# function is kept as a hook so Phase 2/3 can reintroduce
# class-specific declarations (LRT for glm, REML for merMod, etc.)
# without changing the dispatcher.
build_nested_footer_block <- function(nested) {
  NULL
}
