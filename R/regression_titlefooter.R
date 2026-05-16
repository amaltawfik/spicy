# Title + footer auto-generation for table_regression() -- Q13.
#
# Per dev/table_regression_design.md Q13:
#   Title  = "Regression: <DV>"                          (single model)
#          | "Hierarchical regression: <DV>"             (nested = TRUE)
#          | "Regression comparison: <DV>"               (multi-model, identical DVs)
#          | "Regression comparison"                     (multi-model, mixed DVs)
#   Footer = "Note. " + themes joined by \n, where each theme is one of:
#            * Std. errors block          (Q7 / Q13 -- uniform single-line OR
#              per-model indented enumeration)
#            * AME-Satterthwaite block    (Q14b -- affirmative declaration when
#              CR* + AME requested; positive signal, not a disclaimer)
#            * Standardized caveat        (Q15 -- method-specific text when
#              non-additive terms detected)
#            * Stars mapping              (Q12 -- when stars != FALSE)
#            * Singular / rank-deficient  (Q22 -- when any model has dropped coefs)
#            * Hierarchical block         (Q6 -- when nested = TRUE)
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
# `nested`    : logical -- nested = TRUE flips on the hierarchical theme
# `show_columns`: full vocabulary, used to detect AME path
build_regression_footer <- function(
    extracts,
    standardized = "none",
    p_adjust = "none",
    stars = FALSE,
    nested = FALSE,
    show_columns = character(0),
    reference_style = "row") {
  themes <- list(
    build_regression_type_footer_block(extracts),
    build_vcov_footer_block(extracts),
    build_abbreviations_footer_block(show_columns, extracts,
                                       standardized),
    build_ame_satterthwaite_footer_block(extracts, show_columns),
    build_exponentiate_footer_block(extracts),
    build_standardized_caveat_footer_block(extracts, standardized),
    build_p_adjust_footer_block(extracts, p_adjust),
    build_stars_footer_block(stars),
    build_singular_footer_block(extracts),
    build_polynomial_contrasts_footer_block(extracts),
    build_reference_categories_footer_block(extracts, reference_style),
    build_nested_footer_block(nested)
  )
  themes <- Filter(function(x) !is.null(x) && nzchar(x), themes)
  if (length(themes) == 0L) return(NULL)
  paste0("Note. ", paste(themes, collapse = "\n"))
}


# ---- Theme: regression-type block ----------------------------------------

# Emits a one-line declaration of each model's regression type at the
# top of the footer note. Especially useful in multi-model tables that
# mix lm / glm (or different glm families) -- the title only carries
# the DV name, not the inference family.
#
# Uniform across models -> "Linear regression."
# Heterogeneous         -> "Model 1: linear regression; Model 2: logistic regression."
build_regression_type_footer_block <- function(extracts) {
  if (!is.list(extracts) || length(extracts) == 0L) return(NULL)
  types <- vapply(extracts, function(e) {
    tp <- e$title_prefix %||% "Regression"
    # Sentence-case but keep the word "regression" lowercased after the
    # leading capital so the footer reads "Linear regression" rather
    # than "Linear Regression" (matches APA / sentence-case prose).
    tolower(tp)
  }, character(1))
  if (length(types) == 1L) {
    return(paste0(capitalize_first(types[1]), "."))
  }
  if (length(unique(types)) == 1L) {
    # Plural noun ("Linear regression models.") -- terser and more
    # natural than "All models: linear regression." Works across
    # families: "Logistic regression models.", "Poisson regression
    # models.", "Negative-binomial regression models.", ...
    return(paste0(capitalize_first(types[1]), " models."))
  }
  per <- vapply(seq_along(types), function(i) {
    sprintf("Model %d: %s", i, types[i])
  }, character(1))
  paste0(paste(per, collapse = "; "), ".")
}

capitalize_first <- function(s) {
  if (!length(s) || !nzchar(s)) return(s)
  paste0(toupper(substr(s, 1L, 1L)), substring(s, 2L))
}


# ---- Theme: vcov block (Q7 / Q13) ----------------------------------------

# Uniform vcov across models  -> single line:
#   "Std. errors: cluster-robust (CR2), clusters by clinic_id."
# Heterogeneous vcov           -> indented enumeration:
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

# ---- Theme: abbreviation definitions -------------------------------------

# APA Manual 7 Section 7.14: every non-universal abbreviation that appears in
# a table must be defined in the note. Universal stat abbreviations
# (`B`, `SE`, `CI`, `p`, `t`, `z`) are excluded. Definitions covered:
#   * AME -- when any `ame*` token is in `show_columns`
#   * \u03B2 (beta) -- when `standardized != "none"` (the `"beta"` column
#     is auto-injected by `table_regression()` in that case)
#   * OR / IRR / HR / RR / MR / exp(B) -- when one or more extracts
#     applied `exponentiate = TRUE` (`e$exp_applied`); the actual
#     headers used are read from `e$exp_header`
#   * f\u00B2 / \u03B7\u00B2 / \u03C9\u00B2 / \u03C7\u00B2 -- when any
#     `partial_*` token is in `show_columns`
build_abbreviations_footer_block <- function(show_columns,
                                              extracts = list(),
                                              standardized = "none") {
  defs <- character(0)

  if (any(c("ame", "ame_se", "ame_ci", "ame_p") %in% show_columns)) {
    defs <- c(defs, "AME = average marginal effect")
  }

  if (!identical(standardized, "none")) {
    defs <- c(defs, "\u03B2 = standardised coefficient")
  }

  # Exponentiate: define each unique header that the user actually sees.
  # Multiple extracts with different families produce multiple entries
  # ("OR = ...", "IRR = ..." both appear).
  if (is.list(extracts) && length(extracts) > 0L) {
    applied <- vapply(extracts,
                       function(e) isTRUE(e$exp_applied), logical(1))
    if (any(applied)) {
      hdrs <- unique(vapply(extracts[applied],
                              function(e) e$exp_header, character(1)))
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

  # Partial effect sizes (lm: f\u00B2 / \u03B7\u00B2 / \u03C9\u00B2;
  # glm: \u03C7\u00B2). Each token includes both the point-estimate
  # column and its `_ci` companion.
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


build_ame_satterthwaite_footer_block <- function(extracts, show_columns) {
  if (!"ame" %in% show_columns) return(NULL)
  if (!is.list(extracts) || length(extracts) == 0L) return(NULL)
  any_satt <- any(vapply(extracts,
                         function(e) isTRUE(e$use_ame_satterthwaite),
                         logical(1)))
  if (!any_satt) return(NULL)
  any_lm  <- any(vapply(extracts, function(e) !isTRUE(e$is_glm),
                        logical(1)))
  any_glm <- any(vapply(extracts, function(e) isTRUE(e$is_glm),
                        logical(1)))
  # Class-aware mechanism wording: lm uses the closed-form linear
  # contrast (`clubSandwich::linear_contrast`); glm uses the dominant-
  # coefficient approximation via `clubSandwich::coef_test` because the
  # response-scale AME is non-linear in beta (Pustejovsky & Tipton
  # 2018, Section 4).
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
    # `[[` (not `$`) -- extract_lm_phase1's output has a `fit_stats`
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


# ---- Theme: exponentiate (Q2 / Step 2 glm) -------------------------------

# Footer note when at least one extract had its B / beta rows
# exponentiated. Names the family-specific label (OR / IRR / HR / RR
# / MR / exp(B)) per Stata `logit, or` convention. The per-family
# qualifier appears only when MULTIPLE distinct headers are in
# play (e.g., logit + poisson side by side => "OR / IRR (per
# family)"); a single family -- even with a non-exponentiated lm
# alongside -- uses the unqualified header.
build_exponentiate_footer_block <- function(extracts) {
  if (!is.list(extracts) || length(extracts) == 0L) return(NULL)
  applied <- vapply(extracts, function(e) isTRUE(e$exp_applied),
                    logical(1))
  if (!any(applied)) return(NULL)
  hdrs <- unique(vapply(
    extracts[applied], function(e) e$exp_header, character(1)
  ))
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


# ---- Theme: polynomial contrasts (ordered factors) -----------------------

# Publication-grade note: ONE sentence, statistics vocabulary only
# (no software / function-name leak), and the suffix legend lists
# only the polynomial degrees actually present in the table. The
# longer pedagogical explanation -- "what does this mean for
# interpretation, how to switch parameterisations" -- is delegated
# to `inform_polynomial_pedagogy()` (once per R session).
#
# Example output (3-level ordered factor):
#   Ordered factor `education`: polynomial trends (.L = linear,
#   .Q = quadratic).
build_polynomial_contrasts_footer_block <- function(extracts) {
  if (!is.list(extracts) || length(extracts) == 0L) return(NULL)
  poly_vars <- character(0)
  poly_suffixes <- character(0)
  for (e in extracts) {
    coefs <- e$coefs
    if (is.null(coefs) || nrow(coefs) == 0L) next
    is_poly <- !is.na(coefs$factor_level) &
      (startsWith(coefs$factor_level, ".") |
         startsWith(coefs$factor_level, "^"))
    if (any(is_poly)) {
      poly_vars <- union(poly_vars,
                          unique(coefs$factor_term[is_poly]))
      poly_suffixes <- union(poly_suffixes,
                              unique(coefs$factor_level[is_poly]))
    }
  }
  if (length(poly_vars) == 0L) return(NULL)
  # Fire the one-shot pedagogical message via the rlang inform
  # frequency mechanism. NULL-return path keeps the footer pure.
  inform_polynomial_pedagogy()

  vars_str <- paste0("`", poly_vars, "`", collapse = ", ")
  plural <- length(poly_vars) > 1L

  # Suffix legend: only the degrees actually shown in the table.
  # Sorted by polynomial rank (.L < .Q < .C < ^4 < ...).
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


# ---- Theme: reference categories (reference_style = "footer") -----------

# Emits a single "Reference categories: <var1> = <lvl1>; <var2> = <lvl2>."
# line when `reference_style = "footer"`. Convention from SAS
# `PROC LOGISTIC` and SPSS "Categorical Variables Codings". Collects
# the reference level of every treatment-coded factor from every
# model (deduped on the (var, level) pair; if two models share a
# factor with the same ref, listed once).
#
# Returns NULL for other reference_style modes, or when no factor
# in any model has a dropped reference (e.g. all factors use
# `contr.poly`, or no-intercept fits).
build_reference_categories_footer_block <- function(extracts,
                                                     reference_style) {
  if (!identical(reference_style, "footer")) return(NULL)
  if (!is.list(extracts) || length(extracts) == 0L) return(NULL)
  pairs <- character(0)
  seen <- character(0)
  # Each model's `coefs` data.frame carries one row with
  # `is_reference = TRUE` per dropped factor reference (added by
  # `build_reference_rows()`). `factor_term` holds the variable
  # name, `factor_level` holds the reference level. Poly factors
  # have no reference and emit no such row -- correctly skipped.
  for (e in extracts) {
    coefs <- e$coefs
    if (is.null(coefs) || nrow(coefs) == 0L) next
    ref_rows <- coefs[isTRUE_vec(coefs$is_reference), , drop = FALSE]
    if (nrow(ref_rows) == 0L) next
    for (i in seq_len(nrow(ref_rows))) {
      ft <- ref_rows$factor_term[i]
      lvl <- ref_rows$factor_level[i]
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
