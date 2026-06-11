

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
  types <- vapply(frames, function(f) {
    tp <- f$info$extras$title_prefix %||% "Regression"
    tolower(tp)
  }, character(1))
  if (length(types) == 1L) {
    return(paste0(capitalize_first(types[1]), "."))
  }
  if (length(unique(types)) == 1L) {
    return(paste0(capitalize_first(types[1]), " models."))
  }
  per <- vapply(seq_along(types), function(i) {
    sprintf("Model %d: %s", i, types[i])
  }, character(1))
  paste0(paste(per, collapse = "; "), ".")
}


# Frame-aware sibling of format_vcov_label(). Reads from
#   frame$info$vcov_kind          (was extract$vcov_type)
#   frame$info$extras$cluster_name (was extract$cluster_name)
#   frame$info$class               (was extract$is_glm; derived)
# instead of the legacy extract fields. Logic identical.
format_vcov_label_from_frame <- function(frame) {
  vt <- frame$info$vcov_kind %||% "classical"
  cn <- frame$info$extras$cluster_name %||% NA_character_
  is_glm <- identical(frame$info$class, "glm")
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
