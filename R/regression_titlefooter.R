

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
    reference_style = "row") {
  themes <- list(
    build_regression_type_footer_block_from_frames(frames),
    build_vcov_footer_block_from_frames(frames),
    build_random_effects_footer_block_from_frames(frames),
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
    defs <- c(defs, "β = standardised coefficient")
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
    defs <- c(defs, "f² = Cohen's partial f²")
  }
  if (any(c("partial_eta2", "partial_eta2_ci") %in% show_columns)) {
    defs <- c(defs, "η² = partial eta-squared")
  }
  if (any(c("partial_omega2", "partial_omega2_ci") %in% show_columns)) {
    defs <- c(defs, "ω² = bias-corrected partial omega-squared")
  }
  if ("partial_chi2" %in% show_columns) {
    defs <- c(defs, "χ² = partial likelihood-ratio chi-squared")
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
      "coefficient (response-scale AME is non-linear in β)."
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
build_random_effects_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)

  per_model <- lapply(seq_along(frames), function(i) {
    f <- frames[[i]]
    txt <- .format_random_effects_for_frame(f)
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


# Format the per-frame random-effects sentence. Returns NULL when the
# frame has no random-effects content (lm / glm / coxph / etc.).
.format_random_effects_for_frame <- function(frame) {
  re <- frame$info$random_effects
  if (is.null(re)) return(NULL)
  vc <- re$variance_components
  if (is.null(vc) || !is.data.frame(vc) || nrow(vc) == 0L) return(NULL)

  # Identify the (single) primary grouping factor row(s) vs the residual row.
  is_resid <- !is.na(vc$group) & vc$group == "Residual"
  group_rows <- vc[!is_resid, , drop = FALSE]
  resid_rows <- vc[is_resid,  , drop = FALSE]

  # Sample-size sentence: "18 Subjects" (or "5 Months and 18 Subjects" for
  # multiple grouping factors). Derived from frame$info$n_groups since
  # this is more reliable than the var-components nrow.
  ng <- frame$info$n_groups
  n_groups_part <- if (!is.null(ng) && length(ng) > 0L) {
    parts <- vapply(seq_along(ng), function(k) {
      sprintf("%d %s%s", ng[[k]], names(ng)[k],
              if (ng[[k]] > 1L) "s" else "")
    }, character(1))
    paste(parts, collapse = " and ")
  } else NA_character_

  # Variance components sentence: list each non-residual row + residual.
  comp_parts <- character(0)
  if (nrow(group_rows) > 0L) {
    for (i in seq_len(nrow(group_rows))) {
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
    return("Rank-deficient model: dropped coefficient(s) shown as \u2014.")
  }
  paste0(
    "Rank-deficient model(s) ",
    paste(sprintf("Model %d", affected), collapse = ", "),
    ": dropped coefficient(s) shown as \u2014."
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
  if (length(hdrs) == 1L) {
    hdr <- hdrs[1L]
    return(sprintf(
      paste0(
        "Coefficients exponentiated and displayed as %s; CI bounds ",
        "exponentiated; SE delta-method approximation: ",
        "SE_%s = %s \u00D7 SE_link."
      ),
      hdr, hdr, hdr
    ))
  }
  paste0(
    "Coefficients exponentiated and displayed as ",
    paste(hdrs, collapse = " / "),
    " (per family); CI bounds exponentiated; SE delta-method ",
    "approximation: SE_exp = exp(B) \u00D7 SE_link."
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
