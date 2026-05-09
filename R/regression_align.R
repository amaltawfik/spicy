# Multi-model alignment + wide pivot for table_regression() — Layer 2.
#
# Per dev/table_regression_design.md Layer 2:
#   "bind_rows() of the long tables, with `model_id`. Terms appearing
#    in only one model → NA cells in the others. Trivial with the
#    long format."
#
# Two stages:
#
#   align_extracts(extracts, ...)
#     Combine per-model `coefs` long tables into one aligned long
#     frame, compute the canonical term display order (union of
#     per-model coef sequences, with factor coefs grouped, intercept
#     positioned, reference rows filtered per `reference_style`), and
#     attach an `order_idx` column so the rendering layer can sort
#     deterministically.
#
#   pivot_aligned_wide(aligned, value_fields, model_labels)
#     Pivot to one row per (term, estimate_type) with per-model
#     value columns named `<model_label>__<field>`. The double
#     underscore is intentional — unambiguous to split on for the
#     rendering layer.


# ---- align_extracts() ----------------------------------------------------

align_extracts <- function(
    extracts,
    show_intercept = TRUE,
    intercept_position = c("first", "last"),
    group_factor_levels = TRUE,
    reference_style = c("row", "annotation")) {
  intercept_position <- match.arg(intercept_position)
  reference_style <- match.arg(reference_style)

  if (length(extracts) == 0L) {
    return(list(
      coefs_aligned = empty_coefs_aligned(),
      fit_stats_aligned = empty_fit_stats_aligned(),
      term_order = character(0),
      factor_ref_levels = setNames(character(0), character(0)),
      n_models = 0L
    ))
  }

  coefs_long <- do.call(rbind, lapply(extracts, `[[`, "coefs"))
  fit_stats <- do.call(rbind, lapply(extracts, `[[`, "fit_stats"))

  # Capture the reference level for each factor BEFORE the annotation
  # branch potentially drops the ref rows. The map is keyed by the
  # factor variable name (e.g., "cyl" → "4"). Used by the renderer
  # to annotate the factor header when reference_style = "annotation".
  ref_rows_all <- coefs_long[coefs_long$is_reference, , drop = FALSE]
  factor_ref_levels <- if (nrow(ref_rows_all) == 0L) {
    setNames(character(0), character(0))
  } else {
    lvl_map <- ref_rows_all$factor_level
    nms <- ref_rows_all$factor_term
    keep <- !is.na(nms) & !duplicated(nms)
    setNames(lvl_map[keep], nms[keep])
  }

  term_order <- compute_canonical_term_order(extracts)

  if (!isTRUE(show_intercept)) {
    term_order <- term_order[term_order != "(Intercept)"]
    coefs_long <- coefs_long[coefs_long$term != "(Intercept)", , drop = FALSE]
  } else if (identical(intercept_position, "last") &&
             "(Intercept)" %in% term_order) {
    term_order <- c(setdiff(term_order, "(Intercept)"), "(Intercept)")
  }

  if (isTRUE(group_factor_levels)) {
    term_order <- group_factor_terms(term_order, coefs_long)
  }

  if (identical(reference_style, "annotation")) {
    coefs_long <- coefs_long[!coefs_long$is_reference, , drop = FALSE]
    term_order <- intersect(term_order, unique(coefs_long$term))
  }

  coefs_long$order_idx <- match(coefs_long$term, term_order)
  coefs_long <- coefs_long[order(coefs_long$order_idx,
                                  coefs_long$estimate_type,
                                  coefs_long$model_id), , drop = FALSE]
  rownames(coefs_long) <- NULL

  list(
    coefs_aligned = coefs_long,
    fit_stats_aligned = fit_stats,
    term_order = term_order,
    factor_ref_levels = factor_ref_levels,
    n_models = length(extracts)
  )
}


# Canonical term ordering — walk each model's term sequence and
# append unseen terms at the end. Preserves model 1's order as the
# anchor; later models contribute their additions.
compute_canonical_term_order <- function(extracts) {
  base <- character(0)
  for (e in extracts) {
    new_terms <- setdiff(unique(e$coefs$term), base)
    base <- c(base, new_terms)
  }
  base
}


# Reorder a term sequence so that coefs sharing the same `factor_term`
# stay contiguous (in factor-level order). Non-factor terms keep their
# relative order. Reference rows of the group land first within their
# group (factor_level == reference level → emitted by build_reference_rows
# with is_reference = TRUE; the level string is the reference label).
group_factor_terms <- function(term_order, coefs_long) {
  meta <- unique(coefs_long[, c("term", "factor_term", "factor_level",
                                  "is_reference")])
  meta <- meta[!duplicated(meta$term), , drop = FALSE]
  rownames(meta) <- meta$term

  out <- character(0)
  visited <- setNames(logical(length(term_order)), term_order)

  for (t in term_order) {
    if (isTRUE(visited[[t]])) next
    ft <- meta[t, "factor_term"]
    if (is.na(ft)) {
      out <- c(out, t)
      visited[[t]] <- TRUE
      next
    }
    # Collect entire group for this factor; ref row first, then
    # non-reference levels in factor_level order.
    group <- meta$term[!is.na(meta$factor_term) & meta$factor_term == ft]
    group_meta <- meta[group, , drop = FALSE]
    group_meta <- group_meta[order(!group_meta$is_reference,
                                    group_meta$factor_level,
                                    na.last = TRUE), , drop = FALSE]
    new <- intersect(group_meta$term, term_order)  # preserve any drops
    out <- c(out, new)
    visited[new] <- TRUE
  }
  out
}


# ---- pivot_aligned_wide() ------------------------------------------------

pivot_aligned_wide <- function(
    aligned,
    value_fields = c("estimate", "se", "ci_low", "ci_high",
                      "statistic", "df", "p_value"),
    model_labels = NULL) {
  coefs <- aligned$coefs_aligned
  if (nrow(coefs) == 0L) {
    return(empty_coefs_wide(value_fields, model_labels))
  }

  model_ids <- unique(coefs$model_id)
  n_models <- length(model_ids)
  if (is.null(model_labels)) {
    model_labels <- paste0("Model ", seq_len(n_models))
  } else if (length(model_labels) != n_models) {
    spicy_abort(
      sprintf(
        "`model_labels` length (%d) must equal number of models (%d).",
        length(model_labels), n_models
      ),
      class = "spicy_invalid_input"
    )
  }
  label_map <- setNames(model_labels, model_ids)

  # Term-level metadata: one row per (term, estimate_type), in the
  # canonical display order.
  meta_cols <- c("term", "estimate_type", "is_intercept", "is_reference",
                 "factor_term", "factor_level", "order_idx", "test_type")
  meta <- coefs[!duplicated(coefs[, c("term", "estimate_type")]),
                meta_cols, drop = FALSE]
  meta <- meta[order(meta$order_idx, meta$estimate_type), , drop = FALSE]
  rownames(meta) <- NULL

  # Spread per-model values
  meta_keys <- paste(meta$term, meta$estimate_type, sep = "::")
  for (m_id in model_ids) {
    sub <- coefs[coefs$model_id == m_id, , drop = FALSE]
    sub_keys <- paste(sub$term, sub$estimate_type, sep = "::")
    idx <- match(meta_keys, sub_keys)
    label <- label_map[[m_id]]
    for (vf in value_fields) {
      colname <- paste0(label, "__", vf)
      meta[[colname]] <- sub[[vf]][idx]
    }
  }
  meta
}


# ---- Empty-frame helpers -------------------------------------------------

empty_coefs_aligned <- function() {
  e <- empty_coefs_long()
  e$order_idx <- integer(0)
  e
}

empty_fit_stats_aligned <- function() {
  data.frame(
    model_id = character(0),
    outcome = character(0),
    nobs = integer(0),
    stringsAsFactors = FALSE
  )
}

empty_coefs_wide <- function(value_fields, model_labels) {
  base <- data.frame(
    term = character(0),
    estimate_type = character(0),
    is_intercept = logical(0),
    is_reference = logical(0),
    factor_term = character(0),
    factor_level = character(0),
    order_idx = integer(0),
    test_type = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(model_labels)) return(base)
  for (lbl in model_labels) {
    for (vf in value_fields) {
      base[[paste0(lbl, "__", vf)]] <- numeric(0)
    }
  }
  base
}
