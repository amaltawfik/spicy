

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
    # lowercase_first() keeps proper-noun titles intact ("Hierarchical
    # Cox proportional hazards regression", not "Hierarchical cox ...").
    return(sprintf("Hierarchical %s: %s", lowercase_first(prefix),
                   outcomes[1]))
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
    re_columns = c("est", "se", "ci"),
    re_test = "none",
    displayed_parent_vars = NULL,
    frames_display = frames) {
  # `frames_display`: the frames whose coefficient labels match the
  # DISPLAYED body. Only the multinomial columns layout passes a
  # different set (the exploded per-category pseudo-frames): the two
  # themes that read coefficient labels -- reference categories and
  # polynomial trends -- must see the bare display labels, or they
  # print category-prefixed pseudo-levels ("sex = Student: Female")
  # once per equation. Every model-facing theme keeps reading
  # `frames` (one entry per MODEL), so counts and dedupes stay
  # single-model ("Multinomial logistic regression.", not "models.").
  themes <- list(
    build_regression_type_footer_block_from_frames(frames),
    build_vcov_footer_block_from_frames(frames),
    build_ci_method_footer_block_from_frames(frames, show_columns),
    build_mixed_inference_footer_block_from_frames(frames),
    build_random_effects_footer_block_from_frames(frames,
                                                   show_re = show_re,
                                                   re_scale = re_scale,
                                                   re_columns = re_columns,
                                                   re_test = re_test),
    build_survival_footer_block_from_frames(frames),
    build_survival_estimand_footer_block_from_frames(frames),
    build_ordinal_thresholds_footer_block_from_frames(frames),
    build_scale_effects_footer_block_from_frames(frames),
    build_component_blocks_footer_block_from_frames(frames),
    build_abbreviations_footer_block_from_frames(show_columns, frames,
                                                  standardized),
    build_ame_satterthwaite_footer_block_from_frames(frames, show_columns),
    build_exponentiate_footer_block_from_frames(frames, show_columns),
    build_standardized_caveat_footer_block_from_frames(frames, standardized),
    build_p_adjust_footer_block_from_frames(frames, p_adjust),
    build_stars_footer_block(stars),
    build_singular_footer_block_from_frames(frames),
    build_re_se_skipped_footer_block_from_frames(frames),
    build_re_profile_footer_block_from_frames(frames),
    build_polynomial_contrasts_footer_block_from_frames(
      frames_display, displayed_parent_vars = displayed_parent_vars),
    build_reference_outcome_footer_block_from_frames(frames),
    build_reference_alternative_footer_block_from_frames(frames),
    build_uv_disclosure_footer_block_from_frames(frames),
    build_loo_footer_block_from_frames(frames),
    build_stan_convergence_footer_block_from_frames(frames),
    build_reference_categories_footer_block_from_frames(frames_display,
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
# preserving any acronyms further in the string. Titles that START
# with a proper noun (Cox, Poisson, Weibull, ...) keep their capital:
# "Hierarchical Cox proportional hazards regression", "Model 1:
# Poisson regression" -- lowercasing a surname is a typo, not style.
lowercase_first <- function(s) {
  if (!length(s) || !nzchar(s)) return(s)
  proper <- c("Cox", "Poisson", "Weibull", "Bayesian", "Tweedie")
  first_word <- sub("[ -].*$", "", s)
  if (first_word %in% proper) return(s)
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
    # Phase 7c23 (item c): "Fisher information" is the standard
    # publication-grade name for the glm vcov (the inverse of the
    # expected information matrix; for canonical links equals the
    # inverse Hessian at the MLE). The previous "MLE inverse Hessian"
    # was technically correct but unconventional next to Stata's
    # "OIM" / SAS PROC LOGISTIC's bare "Standard Error" labels.
    # Parallels "classical (OLS)" for lm: both name the underlying
    # mechanism that produces the SE.
    return(if (is_glm) "classical (Fisher information)" else "classical (OLS)")
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
  # Resampling estimators name the scheme and -- for the bootstrap -- the
  # VALID replicate count (failed replicates are dropped; Stata's
  # bootstrap header reports completed replications). The bare token
  # ("Std. errors: bootstrap.") violated the footer's
  # name-the-estimator-actually-applied principle.
  if (identical(vt, "bootstrap")) {
    n_valid <- frame$info$extras$boot_n_valid %||% NA_integer_
    reps <- if (is.na(n_valid)) {
      ""
    } else {
      sprintf(" (%d replicates)", as.integer(n_valid))
    }
    if (is.na(cn) || !nzchar(cn)) {
      return(sprintf("nonparametric bootstrap%s", reps))
    }
    return(sprintf("cluster bootstrap%s, clusters by %s", reps, cn))
  }
  if (identical(vt, "jackknife")) {
    if (is.na(cn) || !nzchar(cn)) return("jackknife (leave-one-out)")
    return(sprintf("jackknife (leave-one-cluster-out), clusters by %s", cn))
  }
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


# CI-method disclosure. Profile-likelihood CIs are a CI-ONLY refinement -- the
# point estimate, SE, statistic and p-value all stay Wald -- and are NOT
# reconstructable as est +/- z * SE, so the method is stated here (APA 7 /
# SAMPL / STROBE all require disclosing how uncertainty was computed; matches
# parameters::model_parameters). The note fires only when a model's CIs are
# ACTUALLY profile: ci_method == "profile" under a model-based vcov -- a robust
# vcov takes precedence (its Wald-robust CIs are used, no note). The validator
# guarantees ci_method == "profile" reaches only glm / polr / clm.
build_ci_method_footer_block_from_frames <- function(frames,
                                                     show_columns = character(0)) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  .pct <- function(idx) {
    lvl <- frames[[idx]]$info$ci_level %||% 0.95
    sub("\\.0$", "", format(round(100 * lvl, 1), nsmall = 0))
  }
  lines <- character(0)
  is_profile <- vapply(frames, function(f) {
    identical(f$info$ci_method, "profile") &&
      (f$info$vcov_kind %||% "model") %in% c("model", "classical")
  }, logical(1))
  if (any(is_profile)) {
    lines <- c(lines,
               paste0(.pct(which(is_profile)[1]),
                      "% CIs: profile likelihood."))
  }
  # Percentile bootstrap CIs (G5): a CI-only refinement of the bootstrap
  # replicates; the replicate count already sits on the Std. errors line.
  is_bperc <- vapply(frames, function(f) {
    identical(f$info$ci_method, "boot_percentile") &&
      identical(f$info$vcov_kind, "bootstrap")
  }, logical(1))
  if (any(is_bperc)) {
    lines <- c(lines,
               paste0(.pct(which(is_bperc)[1]),
                      "% CIs: bootstrap percentile."))
  }
  # Bayesian credible intervals in a MIXED table: the shared column
  # header stays "95% CI" (only all-posterior tables relabel to
  # "CrI" / "HDI"), so the Bayesian models' intervals are disclosed
  # per model here -- composing with, not suppressing, the profile /
  # bootstrap lines of the frequentist models above. Gated on the ci
  # column actually being displayed (the compact multi-model default
  # shows none): the publication table stays lean. (posterior_hdi
  # cannot appear in a mixed table: the hdi gate is all-Bayesian.)
  is_post <- vapply(frames, function(f) {
    (f$info$ci_method %||% "") %in%
      c("posterior_quantile", "posterior_hdi")
  }, logical(1))
  if (any(c("ci", "ame_ci") %in% show_columns) &&
      any(is_post) && !all(is_post)) {
    lines <- c(lines, vapply(which(is_post), function(k) {
      sprintf(paste0("Model %d: %s%% CI is an equal-tailed posterior ",
                     "credible interval."), k, .pct(k))
    }, character(1)))
  }
  if (length(lines) == 0L) return(NULL)
  paste(lines, collapse = "\n")
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
    # The footnote is a terse abbreviation key, not a tutorial: per-category
    # AME (ordinal / multinomial) only names the probability scale that sets
    # it apart from the single-outcome case. The interpretation guidance --
    # the sum-to-zero property and the percentage-points (not percent)
    # reading -- lives in vignette("table-regression-ordinal").
    has_percat <- is.list(frames) && any(vapply(frames, function(f) {
      ol <- f$coefs$outcome_level
      !is.null(ol) && any(!is.na(ol) & f$coefs$estimate_type == "ame")
    }, logical(1)))
    if (isTRUE(has_percat)) {
      defs <- c(defs,
                "AME = average marginal effect on a response-category probability")
    } else {
      defs <- c(defs, "AME = average marginal effect")
    }
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
  if ("pd" %in% show_columns) {
    # The definition travels WITH the table (BARG: a rendered artifact
    # must be self-describing); the p-value-correspondence caveat is
    # reader pedagogy and stays in the vignette.
    defs <- c(defs,
              paste0("pd = probability of direction (share of the ",
                     "posterior on the dominant side of zero; Makowski ",
                     "et al. 2019)"))
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
  # Phase 7c22: trim the methodological detail. The Pustejovsky &
  # Tipton 2018 reference and the clubSandwich function name belong in
  # the help page (`?table_regression`), not in every table footer
  # (APA Manual 7 sec. 7.10: keep table notes brief; methodological
  # references go in the surrounding text). The footer keeps the
  # actionable summary "t-test with Satterthwaite df", with a one-word
  # qualifier when the lm / glm paths diverge.
  qualifier <- if (any_lm && any_glm) {
    " (closed-form for lm; dominant-coefficient approximation for glm)"
  } else if (any_glm) {
    " (dominant-coefficient approximation)"
  } else {
    ""
  }
  paste0("AME inference: t-test with Satterthwaite df", qualifier, ".")
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

  # Fallback awareness (G2): the refit path declines on inline-transform
  # formulas and applies posthoc instead -- the footer must then carry
  # the ALGEBRAIC wording (naming the convention), not the refit one.
  used <- vapply(frames, function(f) {
    f$info$extras$standardized_used %||% NA_character_
  }, character(1))
  fell_back <- identical(standardized, "refit") &&
    any(!is.na(used) & used != "refit")

  # Table notes state what IS shown, in method terms only. The
  # negative-space contrast ("components are NOT standardised first"),
  # the convention's software genealogy (SPSS / Stata regress,beta /
  # lm.beta), and the literature citations (Friedrich 1982; Cohen et
  # al. 2003 s7.7) are reader pedagogy and live in the *Standardised
  # coefficients* section of ?table_regression -- a publication table
  # note cites no literature and names no other software. The
  # differs-from-"refit" clause stays: it is the interpretive caveat
  # about the displayed numbers themselves.
  algebraic <- paste0(
    "Standardised \u03B2: interaction / transformed terms are scaled by ",
    "the SD of the product (or transformed) design column; differs ",
    "from \"refit\" when components are correlated."
  )

  if (identical(standardized, "refit") && !fell_back) {
    return(paste0(
      "Standardised \u03B2: after refit on z-scored data, an interaction's ",
      "\u03B2 is the coefficient of the product of the z-scored components."
    ))
  }
  if (fell_back) {
    return(paste0(
      "Standardised \u03B2: \"refit\" failed; algebraic (posthoc) scaling ",
      "applied. ",
      sub("^Standardised \u03B2: ", "", algebraic)
    ))
  }
  algebraic
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

  # Rows mode: thresholds were promoted to an in-table "Thresholds" block
  # (show_thresholds = TRUE -- the orchestrator appended is_threshold rows to
  # coefs). The compact per-model line would then double-report them, so emit a
  # single one-line gloss instead. Under exponentiate, flag that the rows stay
  # on the log-odds scale (cut-points are never exponentiated).
  rows_mode <- any(vapply(frames, function(f) {
    isTRUE(any(f$coefs$is_threshold))
  }, logical(1)))
  if (isTRUE(rows_mode)) {
    exp_any <- any(vapply(frames, function(f) {
      isTRUE(f$info$extras$exp_applied)
    }, logical(1)))
    gloss <- "Thresholds: latent-scale category cut-points"
    if (isTRUE(exp_any)) {
      gloss <- paste0(gloss, " (log-odds scale, not exponentiated)")
    }
    return(paste0(gloss, "."))
  }

  # Footer mode (show_thresholds = FALSE): the compact per-model line.
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
#   coxph / cph: "Concordance C = 0.60 (SE = 0.03)." (n / events are
#                 fit-stat rows since 2026-07-09)
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
  conc  <- frame$info$extras$concordance
  parts <- character(0)
  # n and the number of events moved from this footer prose into
  # fit-stat ROWS ("n" / "N events" tokens, 2026-07-09) -- same
  # migration as the mixed-effects ICC / N (groups). The footer keeps
  # what has no row: the concordance.
  if (!is.null(conc) && is.list(conc) &&
      !is.null(conc$c) && is.finite(conc$c)) {
    if (!is.null(conc$se) && is.finite(conc$se)) {
      parts <- c(parts, sprintf("Concordance C = %.2f (SE = %.2f)",
                                conc$c, conc$se))
    } else {
      parts <- c(parts, sprintf("Concordance C = %.2f", conc$c))    # nocov
    }
  }
  # Reachable now that the Events prose moved to fit-stat rows: a
  # frame without a concordance block contributes nothing here.
  if (length(parts) == 0L) return(NULL)
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
    re_columns = c("est", "se", "ci"),
    re_test = "none") {
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

  # Per-term test disclosure (re_test = "lrt" / "rlrt"): one sentence for the
  # whole block, naming the test behind the vc rows' p column.
  # Method names only -- the literature (Self & Liang 1987; Stram & Lee
  # 1994; Crainiceanu & Ruppert 2004) and the computing package (RLRsim)
  # are cited in ?table_regression, not in a publication table note.
  test_line <- switch(re_test,
    lrt = paste0(
      "Random-effect p-values: LR test vs the reduced random structure, ",
      "chi-bar-squared reference."
    ),
    rlrt = paste0(
      "Random-effect p-value: exact restricted LRT ",
      "(simulated null distribution)."
    ),
    NULL
  )

  # Prefix with "Model k:" only when more than one frame contributes
  # content (parity with the survival footer's convention).
  if (length(per_model) == 1L) {
    return(paste(c(per_model[[1L]]$text, test_line), collapse = "\n"))
  }
  lines <- vapply(per_model, function(pm) {
    sprintf("Model %d: %s", pm$idx, pm$text)
  }, character(1))
  paste(c(lines, test_line), collapse = "\n")
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
  vk  <- frame$info$vcov_kind %||% "model"
  # Under a CR* vcov the whole inference set (SE, Satterthwaite df, p)
  # comes from clubSandwich::coef_test(), NOT from lmerTest / nlme --
  # the footer must attribute the df source truthfully (t for the
  # t-referenced engines, z for glmmTMB whose robust path is Wald-z).
  if (startsWith(vk, "CR") && cls %in% c("lmerMod", "lme")) {
    return(paste0("p-values: Satterthwaite t-test, cluster-robust df ",
                  "(clubSandwich)."))
  }
  if (startsWith(vk, "CR") && cls == "glmmTMB") {
    return("p-values: Wald-z, cluster-robust (clubSandwich).")
  }
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


# Footer glosses for the component blocks: one line per DISTINCT block
# rendered in the table (gated on is_component rows actually present, so
# show_components = FALSE prints nothing), naming what each block models and
# its scale under exponentiate. Plus the glmmTMB robust disclosure: under a
# CR* request clubSandwich covers the conditional fixed effects only, so the
# component rows stay model-based -- said explicitly, never silently.
build_component_blocks_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)

  lines <- character(0)
  seen <- character(0)
  robust_note <- FALSE
  for (f in frames) {
    cf <- f$coefs
    if (is.null(cf$is_component) || !any(cf$is_component %in% TRUE)) next
    for (blk in f$info$extras$component_blocks %||% list()) {
      key <- blk$label
      if (key %in% seen) next
      seen <- c(seen, key)
      gl <- blk$gloss
      if (isTRUE(blk$exp_applied)) {
        gl <- paste0(
          gl, " Coefficients exponentiated and displayed as odds ratios."
        )
      } else if (isTRUE(f$info$extras$exp_applied) && !isTRUE(blk$exp_ok)) {
        gl <- paste0(gl, " Left on the link scale (not exponentiated).")
      }
      lines <- c(lines, gl)
    }
    if (isTRUE(f$info$extras$component_robust_note) &&
        !f$info$vcov_kind %in% c("model", "classical")) {
      robust_note <- TRUE
    }
  }
  if (robust_note) {
    lines <- c(lines, paste0(
      "Robust SEs apply to the conditional component; zero-inflation / ",
      "dispersion SEs are model-based."
    ))
  }
  if (length(lines) == 0L) return(NULL)
  paste(lines, collapse = "\n")
}


# Promote the component blocks (zero-inflation / zero hurdle / dispersion,
# from info$extras$component_blocks) into coefs rows for labelled subordinate
# blocks. Called by the table_regression() orchestrator AFTER the central
# exponentiate (each block applies its OWN link-gated exp -- see below) and
# BEFORE p_adjust: component coefficients are substantive hypotheses and JOIN
# the p-adjust family (unlike ordinal thresholds), and their tests carry stars.
#
# Per-block exponentiation (dev/component_blocks_spec.md D3): exp only where
# the result IS an odds ratio -- the logit-link zero components. Probit /
# cauchit / cloglog zero links, count-type hurdle zero parts (log scale), and
# the dispersion model are left on the link scale (Stata's `irr` precedent:
# the inflate equation is never eform'd; parameters mislabels probit as OR --
# we gate instead). The footer gloss names each block's scale.
.append_component_rows <- function(coefs, blocks, exponentiate) {
  if (is.null(blocks) || length(blocks) == 0L) return(coefs)
  if (is.null(coefs$is_component)) coefs$is_component <- FALSE

  for (blk in blocks) {
    rows <- blk$coefs
    if (is.null(rows) || nrow(rows) == 0L) next                        # nocov

    est <- rows$estimate
    se  <- rows$std_error
    cil <- rows$ci_lower
    ciu <- rows$ci_upper
    if (isTRUE(exponentiate) && isTRUE(blk$exp_ok)) {
      keep <- !rows$is_ref & !is.na(est)
      # exp() + Delta-method SE; z / p invariant (H0: B = 0 <-> exp(B) = 1).
      cil[keep] <- exp(cil[keep])
      ciu[keep] <- exp(ciu[keep])
      se[keep]  <- exp(est[keep]) * se[keep]
      est[keep] <- exp(est[keep])
    }

    new <- data.frame(
      term             = rows$term,
      parent_var       = blk$label,
      label            = rows$label,
      factor_level_pos = seq_len(nrow(rows)),
      is_ref           = rows$is_ref,
      estimate_type    = "B",
      estimate         = est,
      std_error        = se,
      df               = ifelse(rows$is_ref, NA_real_, Inf),
      statistic        = rows$statistic,
      p_value          = rows$p_value,
      ci_lower         = cil,
      ci_upper         = ciu,
      test_type        = ifelse(rows$is_ref, NA_character_, "z"),
      outcome_level    = NA_character_,
      is_component     = TRUE,
      stringsAsFactors = FALSE
    )
    coefs <- .rbind_union(coefs, new)
  }
  coefs
}


# Promote info$random_effects$variance_components into coefs rows for a
# subordinate "Random effects" block. Called by the table_regression()
# orchestrator (NOT the frame methods) AFTER exponentiate + p_adjust, so those
# paths never touch variance components (a variance is never exponentiated or
# p-adjusted). Mirrors .append_threshold_rows().
#
# estimate_type = "vc" (variance component) is a first-class token: a variance
# parameter is NOT a mean-model coefficient, so it is honestly typed and is
# excluded by construction from the exp / standardize / stars filters (which key
# on "B" / "beta"). The renderer aliases "vc" to the B (estimate) columns for
# display.
#
# statistic / df / p_value are carried as NA: no per-row p by default (H0:
# sigma = 0 is on the boundary; a Wald z is invalid -- no reporting guideline
# asks for it). The correct significance signal is the model-level
# chi-bar-squared LRT (footer line, from info$random_effects$null_lrt). The
# columns are kept so a future opt-in per-term LRT/RLRT test can fill them.
.append_random_effects_rows <- function(coefs, re, re_scale = "sd",
                                        term_tests = NULL,
                                        re_test = "none") {
  if (is.null(re)) return(coefs)
  vc <- re$variance_components
  if (is.null(vc) || !is.data.frame(vc) || nrow(vc) == 0L) return(coefs)
  if (is.null(coefs$is_re)) coefs$is_re <- FALSE

  # Convert SD / variance / SE / CI to the requested display scale (Delta
  # method for the SD scale). `.re_components_on_scale` repurposes the
  # `variance` column as the scaled point estimate; correlation rows keep their
  # value in `corr`.
  vc <- .re_components_on_scale(vc, re_scale)
  is_corr <- if ("is_correlation" %in% names(vc)) {
    vc$is_correlation %in% TRUE
  } else {
    rep(FALSE, nrow(vc))
  }
  is_resid <- !is.na(vc$group) & vc$group == "Residual"

  est <- ifelse(is_corr, vc$corr, vc$variance)
  se  <- if ("std_error" %in% names(vc)) vc$std_error else rep(NA_real_, nrow(vc))
  cil <- if ("ci_lower" %in% names(vc)) vc$ci_lower else rep(NA_real_, nrow(vc))
  ciu <- if ("ci_upper" %in% names(vc)) vc$ci_upper else rep(NA_real_, nrow(vc))
  # NOTE: `re_columns` is a DISPLAY concern -- it is honoured at render time
  # (build_body_row em-dashes the deselected cells on vc rows). The data stays
  # complete here so broom::tidy() / as_structured() always carry full SE + CI.

  label <- vapply(seq_len(nrow(vc)), function(i) {
    .re_panel_label(vc$group[i], vc$term[i], is_corr[i], is_resid[i])
  }, character(1))
  # On the variance scale the sigma glyph would mislabel the value: promote
  # the SD labels to sigma-squared (correlations are unitless, untouched).
  if (identical(re_scale, "variance")) {
    label[!is_corr] <- sub("^\u03C3 ", "\u03C3\u00B2 ", label[!is_corr])
  }
  # Stable per-(group, term) key so RE rows align across models by structure.
  key <- paste0("re::", vc$group, "::", vc$term, ifelse(is_corr, "::cor", ""))

  new <- data.frame(
    term             = key,
    parent_var       = "Random effects",
    label            = label,
    factor_level_pos = seq_len(nrow(vc)),
    is_ref           = FALSE,
    estimate_type    = "vc",
    estimate         = as.numeric(est),
    std_error        = as.numeric(se),
    df               = NA_real_,
    statistic        = NA_real_,
    p_value          = NA_real_,
    ci_lower         = as.numeric(cil),
    ci_upper         = as.numeric(ciu),
    test_type        = NA_character_,
    outcome_level    = NA_character_,
    is_re            = TRUE,
    stringsAsFactors = FALSE
  )

  # Opt-in per-term tests (re_test = "lrt" / "rlrt"): fill statistic / df /
  # p_value on the matching SD rows (never on correlation / residual rows --
  # a correlation is tested jointly with its slope's variance, and the
  # residual has no zero-variance null).
  if (!is.null(term_tests) && is.data.frame(term_tests) &&
      nrow(term_tests) > 0L) {
    row_key  <- paste(vc$group, vc$term, sep = "\r")
    test_key <- paste(term_tests$group, term_tests$term, sep = "\r")
    idx <- match(row_key, test_key)
    hit <- !is.na(idx) & !is_corr & !is_resid
    new$statistic[hit] <- term_tests$statistic[idx[hit]]
    new$df[hit]        <- term_tests$df[idx[hit]]
    new$p_value[hit]   <- term_tests$p_value[idx[hit]]
    new$test_type[hit] <- if (identical(re_test, "rlrt")) "rlrt" else "chibar2"
  }
  .rbind_union(coefs, new)
}


# Internal: APA-ish p-value formatter for the LR test line. Returns
# strings like "< .001" or "= .034" so the line reads naturally.
format_p_value_for_panel <- function(p) {
  if (!is.finite(p)) return("= NA")
  if (p < 0.001) return("< .001")
  sprintf("= %.3f", p)
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
.format_random_effects_for_frame <- function(frame, ...) {
  re <- frame$info$random_effects
  if (is.null(re)) return(NULL)
  vc <- re$variance_components
  if (is.null(vc) || !is.data.frame(vc) || nrow(vc) == 0L) return(NULL)

  # The variance parameters render as table rows; N (groups) + ICC render as
  # fit-stat rows (aligned per model). The footer keeps only what has no row
  # home: the estimation method (REML / ML) and the model-level LR test vs the
  # no-random-effects model, referred to the chi-bar-squared mixture null
  # (Self & Liang 1987; Stram & Lee 1994) -- the correct, boundary-aware
  # significance signal for the random part. There is deliberately no per-row
  # Wald p on the variance components (no reporting guideline asks for it;
  # see dev/mixed_random_effects_rows_spec.md).
  method_tag <- re$method
  has_method <- !is.null(method_tag) && is.character(method_tag) &&
    length(method_tag) == 1L && !is.na(method_tag) && nzchar(method_tag)
  header <- if (has_method) {
    sprintf("Random effects (%s)", method_tag)
  } else {
    "Random effects"
  }

  lrt <- re$null_lrt
  lrt_part <- NULL
  if (!is.null(lrt) && is.finite(lrt$chi2) && is.finite(lrt$df)) {
    p_str <- format_p_value_for_panel(lrt$p_chibar2)
    lrt_part <- sprintf(
      "LR test vs %s, \u03C7\u0304\u00B2(%d) = %.2f, p %s",
      lrt$family_label %||% "no-random model",
      as.integer(lrt$df), lrt$chi2, p_str
    )
  }
  # Nothing informative (no method tag, no LR test): suppress entirely.
  if (!has_method && is.null(lrt_part)) return(NULL)
  if (is.null(lrt_part)) return(paste0(header, "."))
  paste0(header, ": ", lrt_part, ".")
}


build_singular_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  flags <- vapply(frames, function(f) {
    isTRUE(f$info$extras$has_singular)
  }, logical(1))
  if (!any(flags)) return(NULL)
  affected <- which(flags)
  is_mixed <- vapply(frames[affected], .is_mixed_frame, logical(1))
  if (length(frames) == 1L) {
    return(.singular_msg_for_frame(frames[[affected]], is_mixed[1L]))
  }
  per <- vapply(seq_along(affected), function(k) {
    sprintf("Model %d: %s", affected[k],
            .singular_msg_for_frame(frames[[affected[k]]], is_mixed[k]))
  }, character(1))
  paste(per, collapse = "\n")
}

# Phase 7c21: distinguish the two singular regimes:
#   * lm / glm: rank-deficient fixed-effect design -- dropped coefs
#     render as -. ASCII em-dash in the renderer.
#   * lmer / glmer / glmmTMB / lme: variance components on the
#     boundary 0 (lme4::isSingular). Random-effect SE / CI are
#     suppressed in `.merMod_attach_wald_se_ci()` to avoid
#     reporting unreliable values; the footer now tells the
#     reader why.
.singular_msg_for_frame <- function(frame, is_mixed) {
  if (isTRUE(is_mixed)) {
    # Table notes state facts about what is shown; the actionable advice
    # ("consider simplifying the random structure") is for the ANALYST
    # at build time, not the reader of a published table -- it moved to
    # the consolidated spicy_caveat warning in table_regression().
    return(paste0(
      "Singular fit: random-effect variance component(s) estimated at ",
      "the boundary (0); their Wald SE and CI are omitted."
    ))
  }
  "Rank-deficient model: dropped coefficient(s) shown as \u2013."
}

.is_mixed_frame <- function(frame) {
  cls <- frame$info$class %||% ""
  cls %in% c("lmerMod", "lmerModLmerTest", "glmerMod", "glmmTMB", "lme")
}


# re_ci = "profile": the CI cells of the variance-component rows are
# profile-likelihood intervals (asymmetric; no SE by construction), so
# the footer must disclose the method -- a profile CI cannot be
# reconstructed from any SE, mirroring the fixed-effects profile
# disclosure for polr / clm.
build_re_profile_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  flags <- vapply(frames, function(f) {
    identical(f$info$extras$re_ci %||% "wald", "profile")
  }, logical(1))
  if (!any(flags)) return(NULL)
  # `re_ci` is a single table-wide argument and only mixed frames carry
  # variance-component rows, so one shared line is always unambiguous.
  paste0("Random-effect variance components: profile likelihood CIs; ",
         "no SE (asymmetric intervals).")
}


# Variance-component SE / CI omitted for size (extras$re_se_skipped_n,
# see .re_se_skipped_by_size): the note states the FACT about what the
# table shows; the advice (raise the cap, or use re_test) lives in the
# orchestrator's spicy_caveat warning, per the singular-fit precedent.
build_re_se_skipped_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  ns <- vapply(frames, function(f) {
    as.integer(f$info$extras$re_se_skipped_n %||% NA_integer_)
  }, integer(1))
  if (all(is.na(ns))) return(NULL)
  msg <- function(n) {
    sprintf(paste0("Random-effect variance components: SE and CI not ",
                   "computed (n = %s exceeds the spicy.re_se_max_n cap)."),
            format(n, big.mark = ","))
  }
  affected <- which(!is.na(ns))
  if (length(frames) == 1L) return(msg(ns[affected]))
  # Every model affected with the same n (the common multi-model case):
  # one shared line instead of per-model repeats.
  if (length(affected) == length(frames) &&
      length(unique(ns[affected])) == 1L) {
    return(msg(ns[affected][1L]))
  }
  per <- vapply(affected, function(k) {
    sprintf("Model %d: %s", k, msg(ns[k]))
  }, character(1))
  paste(per, collapse = "\n")
}


# Conditional-logit reference (base) alternative, read from
# extras$reference_alternative -- the discrete-choice sibling of the
# multinomial reference-outcome note below (Stata asclogit's "base
# alternative"). Fact-only; deduped across models like its sibling.
build_reference_alternative_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  refs <- vapply(frames, function(f) {
    as.character(f$info$extras$reference_alternative %||% NA_character_)
  }, character(1))
  if (all(is.na(refs))) return(NULL)
  affected <- which(!is.na(refs))
  msg <- function(ref) sprintf("Reference alternative: %s.", ref)
  if (length(affected) == length(frames) &&
      length(unique(refs[affected])) == 1L) {
    return(msg(refs[affected][1L]))
  }
  per <- vapply(affected, function(k) {
    sprintf("Model %d: %s", k, msg(refs[k]))
  }, character(1))
  paste(per, collapse = "\n")
}


# Multinomial reference (base) outcome, read from
# extras$reference_outcome. Stata prints its "base outcome" under
# every mlogit table; a multinomial table is unreadable without it
# (every coefficient is a contrast AGAINST that category), so the
# note is emitted for every multinomial frame in BOTH layouts
# (outcome-as-columns and per-outcome rows). Fact-only.
build_reference_outcome_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  refs <- vapply(frames, function(f) {
    as.character(f$info$extras$reference_outcome %||% NA_character_)
  }, character(1))
  if (all(is.na(refs))) return(NULL)
  affected <- which(!is.na(refs))
  msg <- function(ref) sprintf("Reference outcome: %s.", ref)
  # One shared line only when EVERY model in the table is multinomial
  # and shares one reference -- in a mixed-class table an unqualified
  # line would read as applying to the non-multinomial models too
  # (same all-affected bar as build_re_se_skipped above).
  if (length(affected) == length(frames) &&
      length(unique(refs[affected])) == 1L) {
    return(msg(refs[affected][1L]))
  }
  per <- vapply(affected, function(k) {
    sprintf("Model %d: %s", k, msg(refs[k]))
  }, character(1))
  paste(per, collapse = "\n")
}


# Frame-aware sibling of build_exponentiate_footer_block(). Reads
#   frame$info$extras$exp_applied (was extract$exp_applied)
#   frame$info$extras$exp_header  (was extract$exp_header)
#
# G4 disclosure: when an SE column is actually displayed, the sentence
# also states the SE scale (delta method; Stata [R] logistic convention
# se(OR) = OR x se(b)) and that the CI -- the exponential of the
# link-scale CI endpoints, Wald or profile alike -- is asymmetric. The
# table note says what IS shown; the negative-space explanation (the CI
# is NOT reconstructable as estimate +/- z x SE) is reader pedagogy and
# lives in ?table_regression, not in a publication table note. Without a
# visible SE column the short sentence suffices. In a table mixing
# exponentiated and identity-link models, the sentence is scoped to the
# exponentiated models (an lm's SE column in the same table is
# OLS-scale).
build_exponentiate_footer_block_from_frames <- function(
    frames, show_columns = character(0)) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  applied <- vapply(frames,
                    function(f) isTRUE(f$info$extras$exp_applied),
                    logical(1))
  if (!any(applied)) return(NULL)
  hdrs <- unique(vapply(frames[applied],
                        function(f) f$info$extras$exp_header,
                        character(1)))

  scope <- if (all(applied)) {
    "Coefficients"
  } else {
    sprintf("%s: coefficients",
            paste(sprintf("Model %d", which(applied)), collapse = ", "))
  }

  # Bayesian frames exponentiate the DRAWS (SE = posterior MAD SD of
  # exp(draws), exact) rather than crossing scales by the delta
  # method, and under ci_method = "hdi" the interval is recomputed on
  # the exponentiated draws rather than transformed -- the gloss must
  # say which mechanism produced the displayed numbers.
  is_bayes_f <- vapply(frames, function(f) {
    !is.null(f$info$extras$posterior_engine)
  }, logical(1))
  bayes_applied <- is_bayes_f & applied
  se_gloss <- if (all(is_bayes_f[applied])) {
    "posterior MAD SD of the exponentiated draws"
  } else if (any(bayes_applied)) {
    paste0("delta method; posterior MAD SD of the exponentiated ",
           "draws for the Bayesian model(s)")
  } else {
    "delta method"
  }
  any_hdi <- any(vapply(frames[applied], function(f) {
    identical(f$info$ci_method, "posterior_hdi")
  }, logical(1)))
  ci_gloss <- if (any_hdi) {
    "CI: highest-density interval of the exponentiated draws"
  } else {
    "CI bounds exponentiated"
  }

  if ("se" %in% show_columns) {
    if (length(hdrs) == 1L) {
      return(sprintf(
        paste0("%s exponentiated and displayed as %s; SE on the %s scale ",
               "(%s); %s (asymmetric)."),
        scope, hdrs[1L], hdrs[1L], se_gloss, ci_gloss
      ))
    }
    return(sprintf(
      paste0("%s exponentiated and displayed as %s (per family); SE on ",
             "the displayed ratio scale (%s); %s (asymmetric)."),
      scope, paste(hdrs, collapse = " / "), se_gloss, ci_gloss
    ))
  }

  if (length(hdrs) == 1L) {
    return(sprintf(
      "%s exponentiated and displayed as %s; %s.",
      scope, hdrs[1L], ci_gloss
    ))
  }
  sprintf(
    "%s exponentiated and displayed as %s (per family); %s.",
    scope, paste(hdrs, collapse = " / "), ci_gloss
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
    # Mirror apply_p_adjust_to_frame_coefs() exactly: component-block
    # intercepts ("zero_(Intercept)", "zi.(Intercept)") are excluded
    # from the family by their display LABEL -- counting them here made
    # the footer's m overstate the adjustment actually performed.
    # (label may be absent on synthetic frames; treat as non-intercept.)
    lbl <- cf$label %||% rep(NA_character_, nrow(cf))
    sum(cf$estimate_type == "B" &
          cf$term != "(Intercept)" &
          !(lbl %in% "(Intercept)") &
          !cf$is_ref &
          !is.na(cf$p_value))
  }, integer(1))
  size_part <- if (length(unique(sizes)) == 1L) {
    sprintf("m = %d coefficient(s) per model", sizes[1L])
  } else {
    sprintf("m = (%s) coefficient(s) per model",
            paste(sizes, collapse = ", "))
  }
  # encodeString(quote = '"') gives a platform-independent double-quoted
  # method name; shQuote() would render '...' on Unix vs "..." on Windows,
  # making the footer inconsistent across operating systems.
  sprintf(
    "P-values adjusted via stats::p.adjust(method = %s); %s.",
    encodeString(p_adjust, quote = "\""), size_part
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
build_polynomial_contrasts_footer_block_from_frames <- function(
    frames,
    displayed_parent_vars = NULL) {
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
  # Phase 7c22 (item f): only emit the polynomial-trends note when the
  # ordered factor SURVIVES the keep / drop display filter. Without
  # the filter, the note would describe a variable that the reader
  # doesn't actually see in the table.
  if (!is.null(displayed_parent_vars)) {
    poly_vars <- intersect(poly_vars, displayed_parent_vars)
    if (length(poly_vars) == 0L) return(NULL)
  }
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


# Footer gloss for the "Scale effects" block of a heteroskedastic clm
# (`scale = ~`). The coefficients act on log(sigma) of the latent variable,
# so exp(zeta) is a ratio of latent standard deviations -- NOT an odds ratio.
# The rows are materialised after exponentiation, so under exponentiate = TRUE
# they stay on the log scale and the note says so.
build_scale_effects_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  has_scale <- any(vapply(frames, function(f) {
    ft <- f$coefs$parent_var
    !is.null(ft) && any(ft %in% "Scale effects")
  }, logical(1)))
  if (!has_scale) return(NULL)
  gloss <- paste0(
    "Scale effects: covariate effects on the log standard deviation of the ",
    "latent response"
  )
  exp_any <- any(vapply(frames, function(f) {
    isTRUE(f$info$extras$exp_applied)
  }, logical(1)))
  if (isTRUE(exp_any)) {
    gloss <- paste0(gloss, " (log scale, not exponentiated: their exponential ",
                    "is a ratio of latent SDs, not an odds ratio)")
  }
  paste0(gloss, ".")
}


# Footer note for the PSIS-LOO fit-stat rows: the elpd SE, without
# which the ELPD / LOOIC rows cannot support a comparison judgment.
# Same dedupe convention as the sibling builders.
build_loo_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  notes <- vapply(frames, function(f) {
    as.character(f$info$extras$loo_note %||% NA_character_)
  }, character(1))
  if (all(is.na(notes))) return(NULL)
  affected <- which(!is.na(notes))
  # The bare (unattributed) note is only honest when EVERY model in
  # the table carries the same note: in a mixed frequentist+Bayesian
  # table the SE(ELPD) and its reliability caveats are per-model
  # facts and keep their "Model k:" prefix (same convention as the
  # convergence and reference-outcome builders).
  if (length(unique(notes[affected])) == 1L &&
      length(affected) == length(notes)) {
    return(notes[affected][1L])
  }
  paste(vapply(affected, function(k) {
    sprintf("Model %d: %s", k, notes[k])
  }, character(1)), collapse = "\n")
}


# Footer warning from the Bayesian convergence guard (max R-hat / min
# ESS / divergent transitions past their Vehtari-et-al. targets). NULL
# -- and therefore silent -- for clean posteriors.
build_stan_convergence_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) return(NULL)
  notes <- vapply(frames, function(f) {
    as.character(f$info$extras$convergence_note %||% NA_character_)
  }, character(1))
  if (all(is.na(notes))) return(NULL)
  affected <- which(!is.na(notes))
  if (length(unique(notes[affected])) == 1L && length(affected) == length(notes)) {
    return(notes[affected][1L])
  }
  paste(vapply(affected, function(k) {
    sprintf("Model %d: %s", k, notes[k])
  }, character(1)), collapse = "\n")
}
