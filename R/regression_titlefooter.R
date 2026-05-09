# Title + footer auto-generation for table_regression() — Q13.
#
# Per dev/table_regression_design.md Q13:
#   Title  = "Regression: <DV>"                          (single model)
#          | "Hierarchical regression: <DV>"             (nested = TRUE)
#          | "Regression comparison: <DV>"               (multi-model, identical DVs)
#          | "Regression comparison"                     (multi-model, mixed DVs)
#   Footer = "Note. " + themes joined by \n, where each theme is one of:
#            * Std. errors block          (Q7 / Q13 — uniform single-line OR
#              per-model indented enumeration)
#            * AME-Satterthwaite block    (Q14b — affirmative declaration when
#              CR* + AME requested; positive signal, not a disclaimer)
#            * Standardized caveat        (Q15 — method-specific text when
#              non-additive terms detected)
#            * Stars mapping              (Q12 — when stars != FALSE)
#            * Singular / rank-deficient  (Q22 — when any model has dropped coefs)
#            * Hierarchical block         (Q6 — when nested = TRUE)
#
# All builders take pre-computed metadata (typically the list of
# `extract_lm_phase1()` outputs) and return a character scalar (or
# NULL if the theme does not apply for this call).


# ---- Title ---------------------------------------------------------------

build_regression_title <- function(extracts, nested = FALSE) {
  if (!is.list(extracts) || length(extracts) == 0L) {
    return("Regression")
  }
  outcomes <- vapply(extracts, function(e) e$outcome %||% NA_character_,
                     character(1))
  outcomes <- outcomes[!is.na(outcomes)]
  n <- length(extracts)

  # Family-aware prefix per model. When all models share the same
  # family / link, the prefix becomes the title's class label
  # ("Logistic regression", "Poisson regression", ...). Mixed
  # families (e.g. logit + probit) fall back to the generic
  # "Regression" so the title doesn't lie about the analysis.
  prefixes <- vapply(extracts, function(e) {
    e$title_prefix %||% "Regression"
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
    # Hierarchical assumes identical DV (validated upstream by Q11a).
    # The "Hierarchical " prefix is paired with the lowercased
    # family label for grammatical correctness ("Hierarchical
    # logistic regression: am" rather than "Hierarchical Logistic
    # regression: am").
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


# ---- Footer dispatcher ---------------------------------------------------

# Build the full multi-line footer. Returns NULL if no theme applies.
# `extracts`  : list of extract_lm_phase1() outputs (one per model)
# `standardized`: scalar token ("none", "refit", "posthoc", "basic", "smart")
# `stars`     : FALSE, TRUE, or named numeric vector (Q12)
# `nested`    : logical — nested = TRUE flips on the hierarchical theme
# `show_columns`: full vocabulary, used to detect AME path
build_regression_footer <- function(
    extracts,
    standardized = "none",
    p_adjust = "none",
    stars = FALSE,
    nested = FALSE,
    show_columns = character(0)) {
  themes <- list(
    build_vcov_footer_block(extracts),
    build_ame_satterthwaite_footer_block(extracts, show_columns),
    build_standardized_caveat_footer_block(extracts, standardized),
    build_p_adjust_footer_block(extracts, p_adjust),
    build_stars_footer_block(stars),
    build_singular_footer_block(extracts),
    build_nested_footer_block(nested)
  )
  themes <- Filter(function(x) !is.null(x) && nzchar(x), themes)
  if (length(themes) == 0L) return(NULL)
  paste0("Note. ", paste(themes, collapse = "\n"))
}


# ---- Theme: vcov block (Q7 / Q13) ----------------------------------------

# Uniform vcov across models  → single line:
#   "Std. errors: cluster-robust (CR2), clusters by clinic_id."
# Heterogeneous vcov           → indented enumeration:
#   "Std. errors:
#      Model 1: classical
#      Model 2: cluster-robust (CR2), clusters by clinic_id"
build_vcov_footer_block <- function(extracts) {
  if (!is.list(extracts) || length(extracts) == 0L) return(NULL)
  labels <- vapply(extracts, format_vcov_label, character(1))
  if (all(labels == labels[1])) {
    return(paste0("Std. errors: ", labels[1], "."))
  }
  per <- vapply(seq_along(labels), function(i) {
    sprintf("  Model %d: %s", i, labels[i])
  }, character(1))
  paste0("Std. errors:\n", paste(per, collapse = "\n"))
}

# Friendly label for one model's vcov choice. Reads
#   $vcov_type   : "classical" / "HC*" / "CR*" / "bootstrap" / "jackknife"
#   $cluster_name: NULL or character scalar (set by the orchestrator
#                  via detect_weights_column_name() at validation time)
format_vcov_label <- function(extract) {
  vt <- extract$vcov_type %||% "classical"
  cn <- extract$cluster_name %||% NA_character_
  is_glm <- isTRUE(extract$is_glm)
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


# ---- Theme: AME-Satterthwaite affirmative (Q14b) -------------------------

build_ame_satterthwaite_footer_block <- function(extracts, show_columns) {
  if (!"AME" %in% show_columns) return(NULL)
  if (!is.list(extracts) || length(extracts) == 0L) return(NULL)
  any_satt <- any(vapply(extracts,
                         function(e) isTRUE(e$use_ame_satterthwaite),
                         logical(1)))
  if (!any_satt) return(NULL)
  paste0(
    "AME inference: t-distribution with Satterthwaite-corrected df ",
    "(Pustejovsky & Tipton 2018) via `clubSandwich::linear_contrast()`."
  )
}


# ---- Theme: standardized caveat (Q15) ------------------------------------

# Method-specific caveat text emitted when standardized != "none" AND
# at least one model contains interactions or transforms. Mirrors the
# wording used by emit_standardized_caveat_if_needed() (Phase E
# warning) so the user sees the same message at runtime and in the
# table footer.
build_standardized_caveat_footer_block <- function(extracts, standardized) {
  if (identical(standardized, "none")) return(NULL)
  if (!is.list(extracts) || length(extracts) == 0L) return(NULL)
  any_problem <- FALSE
  for (e in extracts) {
    # `[[` (not `$`) — extract_lm_phase1's output has a `fit_stats`
    # field that partial-matches "fit" under `$`, which would
    # silently feed a data.frame to detect_non_additive_terms() and
    # crash on missing $terms.
    fit <- e[["fit"]]
    nonadd <- e[["non_additive"]]
    if (is.null(nonadd) && !is.null(fit)) {
      nonadd <- detect_non_additive_terms(fit)
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

# stars = FALSE                  → NULL (no footer note)
# stars = TRUE                   → APA preset:
#   "*** p < .001, ** p < .01, * p < .05."
# stars = c("†"=.10, "*"=.05)    → "* p < .05, † p < .10."
# Mapping rendered in increasing-strictness order (smallest p first
# at the lead) mirroring stargazer / modelsummary convention.
build_stars_footer_block <- function(stars) {
  if (isFALSE(stars) || is.null(stars)) return(NULL)
  if (isTRUE(stars)) {
    stars <- c("*" = 0.05, "**" = 0.01, "***" = 0.001)
  }
  if (!is.numeric(stars) || is.null(names(stars))) return(NULL)
  ord <- order(stars)               # smallest p first ⇒ strictest symbol first
  sym <- names(stars)[ord]
  thr <- stars[ord]
  parts <- vapply(seq_along(sym), function(i) {
    sprintf("%s p < %s", sym[i], format_p_threshold(thr[i]))
  }, character(1))
  paste0(paste(parts, collapse = ", "), ".")
}

# APA-style threshold formatting: leading dot, minimum 2 decimal
# digits, trailing zeros trimmed beyond that. Examples:
#   0.001 → ".001",  0.01 → ".01",  0.05 → ".05",  0.10 → ".10".
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


# ---- Theme: singular / rank-deficient (Q22) ------------------------------

build_singular_footer_block <- function(extracts) {
  if (!is.list(extracts) || length(extracts) == 0L) return(NULL)
  flags <- vapply(extracts, function(e) isTRUE(e$has_singular), logical(1))
  if (!any(flags)) return(NULL)
  affected <- which(flags)
  if (length(extracts) == 1L) {
    return("Rank-deficient model: dropped coefficient(s) shown as \u2014.")
  }
  paste0(
    "Rank-deficient model(s) ",
    paste(sprintf("Model %d", affected), collapse = ", "),
    ": dropped coefficient(s) shown as \u2014."
  )
}


# ---- Theme: p_adjust mapping ---------------------------------------------

# Footer note when a multiple-comparison adjustment was applied.
# Mirrors the modelsummary convention. Family size (per model) is
# inferred from the extracts so the user sees what `m` was.
build_p_adjust_footer_block <- function(extracts, p_adjust) {
  if (identical(p_adjust, "none") || is.null(p_adjust) ||
        !is.list(extracts) || length(extracts) == 0L) {
    return(NULL)
  }
  # Per-model family sizes (B-row coefs that were actually adjusted)
  sizes <- vapply(extracts, function(e) {
    cf <- e$coefs
    if (is.null(cf) || nrow(cf) == 0L) return(0L)
    sum(cf$estimate_type == "B" &
          !cf$is_intercept &
          !cf$is_reference &
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


# ---- Theme: nested declaration (Q6) --------------------------------------

# Returns NULL by design: the nested comparison block emitted by
# format_nested_comparison_footer() (Step 9) is self-explanatory under
# its "── Model comparison ──" header. Adding a paragraph that
# announces it would just be redundant noise above the block. The
# function is kept as a hook so Phase 2/3 can reintroduce
# class-specific declarations (LRT for glm, REML for merMod, etc.)
# without changing the dispatcher.
build_nested_footer_block <- function(nested) {
  NULL
}
