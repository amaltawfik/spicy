# Per-model coefficient transforms applied between extraction and
# alignment for table_regression(). Each helper takes a long-format
# `coefs` data.frame (the `$coefs` field of an extract_lm_phase1()
# output) and returns a modified one.


# ---- p_adjust -------------------------------------------------------------

# Apply a multiple-comparison adjustment to the p-values within a
# single model's coefficient family.
#
# The "family" is, per estimate_type, every row that:
#   * is not the intercept,
#   * is not a reference-level placeholder,
#   * has a non-NA p-value (singular coefs are excluded).
#
# Adjustment is applied independently per estimate_type ("B", "beta",
# "AME", "partial_*"), so the AME family is sized separately from
# the B family -- both are legitimately their own multiple-testing
# families.
#
# Returns the input data.frame with the `p_value` column updated
# in place. When `method == "none"` the input is returned unchanged.
apply_p_adjust <- function(coefs, method) {
  if (identical(method, "none") || is.null(method)) return(coefs)
  if (is.null(coefs) || nrow(coefs) == 0L) return(coefs)

  family_mask <- !coefs$is_intercept &
                   !coefs$is_reference &
                   !is.na(coefs$p_value)

  if (!any(family_mask)) return(coefs)

  # Per-estimate_type adjustment: split the family into independent
  # multiple-testing families (B, beta, AME, partial_f2, etc.) and
  # adjust each. Mirrors the modelsummary / parameters convention.
  for (et in unique(coefs$estimate_type[family_mask])) {
    rows <- which(family_mask & coefs$estimate_type == et)
    if (length(rows) == 0L) next
    coefs$p_value[rows] <- stats::p.adjust(
      coefs$p_value[rows], method = method
    )
  }
  coefs
}


# Frame-aware sibling of apply_p_adjust(). Operates on the frame's
# coefs tibble whose column naming differs from the legacy long coefs:
#   is_intercept -> derived (term == "(Intercept)")
#   is_reference -> is_ref
# The estimate_type / p_value column names are identical in both
# schemas, so the per-family adjustment loop is unchanged.
#
# Phase 0c sub-step C3: align_frames() now reads p-values from the
# frame side, so the orchestrator must adjust the frame coefs (not
# just the legacy extract coefs) for the displayed values to reflect
# the chosen p_adjust method.
apply_p_adjust_to_frame_coefs <- function(coefs, method) {
  if (identical(method, "none") || is.null(method)) return(coefs)
  if (is.null(coefs) || nrow(coefs) == 0L) return(coefs)

  family_mask <- coefs$term != "(Intercept)" &
                   !coefs$is_ref &
                   !is.na(coefs$p_value)

  if (!any(family_mask)) return(coefs)

  for (et in unique(coefs$estimate_type[family_mask])) {
    rows <- which(family_mask & coefs$estimate_type == et)
    if (length(rows) == 0L) next
    coefs$p_value[rows] <- stats::p.adjust(
      coefs$p_value[rows], method = method
    )
  }
  coefs
}


# ---- keep / drop filter ---------------------------------------------------

# Filter the aligned long format by regular expression match on the
# `term` column. Applied AFTER p_adjust (so adjusted p-values
# reflect the model's full coefficient family, not just the
# displayed subset) and AFTER alignment (so canonical term ordering
# is preserved for the surviving terms).
#
# `keep` and `drop` are validated upstream as mutually exclusive
# character vectors of regex patterns. Multiple patterns combine
# with logical OR (a row matches if ANY pattern matches).
#
# Side effect on `aligned`:
#   * `coefs_aligned` filtered to the surviving terms
#   * `term_order` filtered to the same set
#   * `factor_ref_levels` cleaned of factors whose group is now empty
apply_keep_drop_filter <- function(aligned, keep = NULL, drop = NULL) {
  if (is.null(keep) && is.null(drop)) return(aligned)
  if (is.null(aligned$coefs_aligned) ||
        nrow(aligned$coefs_aligned) == 0L) {
    return(aligned)
  }

  terms <- aligned$coefs_aligned$term
  matches_any <- function(term, patterns) {
    vapply(term,
           function(t) any(vapply(patterns,
                                  function(p) grepl(p, t, perl = TRUE),
                                  logical(1))),
           logical(1))
  }

  keep_mask <- if (!is.null(keep)) matches_any(terms, keep) else rep(TRUE, length(terms))
  drop_mask <- if (!is.null(drop)) matches_any(terms, drop) else rep(FALSE, length(terms))
  final_mask <- keep_mask & !drop_mask

  aligned$coefs_aligned <- aligned$coefs_aligned[final_mask, , drop = FALSE]
  rownames(aligned$coefs_aligned) <- NULL

  surviving_terms <- unique(aligned$coefs_aligned$term)
  aligned$term_order <- intersect(aligned$term_order, surviving_terms)

  # Drop factor_ref_levels entries whose factor has no remaining
  # rows (otherwise the renderer would emit an orphan
  # "<factor>: [ref: <level>]" header for a factor with no rows).
  if (length(aligned$factor_ref_levels) > 0L) {
    surviving_factor_terms <- unique(
      aligned$coefs_aligned$factor_term[
        !is.na(aligned$coefs_aligned$factor_term)
      ]
    )
    aligned$factor_ref_levels <- aligned$factor_ref_levels[
      names(aligned$factor_ref_levels) %in% surviving_factor_terms
    ]
  }

  aligned
}
