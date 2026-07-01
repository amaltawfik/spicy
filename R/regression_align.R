

# Frame-aware sibling of compute_canonical_term_order(). Reads
# frames[[i]]$coefs$term instead of extracts[[i]]$coefs$term. The
# `term` column name is identical in both schemas, so the body of
# the loop is the same; only the input type differs.
compute_canonical_term_order_from_frames <- function(frames) {
  base <- character(0)
  for (f in frames) {
    new_terms <- setdiff(unique(f$coefs$term), base)
    base <- c(base, new_terms)
  }
  base
}


# Frame-aware sibling of align_extracts(). Takes a list of frames plus
# a parallel `model_ids` vector (frames do not carry model_id; it lives
# in the orchestrator) and produces an aligned object SHAPE-IDENTICAL
# to align_extracts()'s return value, so the downstream body builder
# can consume it unchanged until C4 lands.
#
# Internal strategy:
#   1. Build the long-format coefs data.frame by stacking each frame's
#      coefs with the legacy column names. The Phase 0b sub-step 2
#      reshape map applies in reverse:
#        se               <- std_error
#        ci_low           <- ci_lower
#        ci_high          <- ci_upper
#        is_reference     <- is_ref
#        is_intercept     <- derived: term == "(Intercept)"
#        is_singular      <- derived: term %in% info$extras$singular_terms
#        factor_term      <- NA when parent_var == term, else parent_var
#        factor_level     <- NA when label      == term, else label
#        test_type        <- coefs$test_type (preserved per row)
#        model_id         <- model_ids[i]   (injected)
#        outcome          <- frames[[i]]$info$dv  (injected)
#   2. Build the fit_stats data.frame by compacting each frame's
#      fit_stats list via .compact_fit_stats_for_legacy().
#   3. The remaining alignment logic (factor_ref_levels capture,
#      compute_canonical_term_order, group_factor_terms,
#      reference-style filter, order_idx assignment, outcome_labels
#      and exp_headers vectors) is the same as align_extracts(), just
#      reading from frames for the input-time vapply.
#
# Phase 0c sub-step C3.
align_frames <- function(
    frames,
    model_ids,
    show_intercept = TRUE,
    intercept_position = c("first", "last"),
    reference_style = c("row", "annotation", "footer", "none")) {
  intercept_position <- match.arg(intercept_position)
  reference_style <- match.arg(reference_style)

  if (length(frames) == 0L) {
    return(list(
      coefs_aligned = empty_coefs_aligned(),
      fit_stats_aligned = empty_fit_stats_aligned(),
      term_order = character(0),
      factor_ref_levels = setNames(character(0), character(0)),
      n_models = 0L
    ))
  }

  # ---- Build legacy-shape long coefs from frames ------------------------
  coefs_long <- do.call(rbind, lapply(seq_along(frames), function(i) {
    cf <- frames[[i]]$coefs
    singular_terms <- frames[[i]]$info$extras$singular_terms %||% character(0)
    test_type_col <- if (!is.null(cf$test_type)) {
      as.character(cf$test_type)
    } else {
      rep(NA_character_, nrow(cf))
    }
    data.frame(
      model_id         = rep(model_ids[i], nrow(cf)),
      outcome          = rep(frames[[i]]$info$dv, nrow(cf)),
      term             = cf$term,
      estimate_type    = cf$estimate_type,
      estimate         = cf$estimate,
      se               = cf$std_error,
      ci_low           = cf$ci_lower,
      ci_high          = cf$ci_upper,
      statistic        = cf$statistic,
      df               = cf$df,
      p_value          = cf$p_value,
      test_type        = test_type_col,
      is_singular      = cf$term %in% singular_terms,
      is_intercept     = cf$term == "(Intercept)",
      is_reference     = cf$is_ref,
      factor_term      = ifelse(cf$parent_var == cf$term,
                                NA_character_, cf$parent_var),
      factor_level     = ifelse(cf$label == cf$term,
                                NA_character_, cf$label),
      factor_level_pos = as.integer(cf$factor_level_pos),
      # Outcome category for per-category AME rows (ordinal / multinomial);
      # NA for single-outcome frames that don't carry it. Drives the
      # renderer's per-category AME column pivot.
      outcome_level    = if (!is.null(cf$outcome_level)) {
        as.character(cf$outcome_level)
      } else {
        NA_character_
      },
      stringsAsFactors = FALSE
    )
  }))

  # ---- Build legacy-shape fit_stats from frames -------------------------
  fit_stats <- do.call(rbind, lapply(seq_along(frames), function(i) {
    as.data.frame(
      .compact_fit_stats_for_legacy(frames[[i]]$info$fit_stats,
                                    model_ids[i],
                                    frames[[i]]$info$dv),
      stringsAsFactors = FALSE
    )
  }))

  # ---- Everything below mirrors align_extracts() ------------------------
  ref_rows_all <- coefs_long[coefs_long$is_reference, , drop = FALSE]
  factor_ref_levels <- if (nrow(ref_rows_all) == 0L) {
    setNames(character(0), character(0))
  } else {
    lvl_map <- ref_rows_all$factor_level
    nms <- ref_rows_all$factor_term
    keep <- !is.na(nms) & !duplicated(nms)
    setNames(lvl_map[keep], nms[keep])
  }

  term_order <- compute_canonical_term_order_from_frames(frames)

  if (!isTRUE(show_intercept)) {
    term_order <- term_order[term_order != "(Intercept)"]
    coefs_long <- coefs_long[coefs_long$term != "(Intercept)", , drop = FALSE]
  } else if (identical(intercept_position, "last") &&
             "(Intercept)" %in% term_order) {
    term_order <- c(setdiff(term_order, "(Intercept)"), "(Intercept)")
  }

  term_order <- group_factor_terms(term_order, coefs_long)

  # Ordinal cut-points always sort to the very bottom, after every predictor.
  # The synthetic "Thresholds" parent (created only by the show_thresholds
  # path) would otherwise land at its first-appearance position, which in a
  # multi-model table can fall ahead of a predictor a later model introduces.
  thr_terms <- unique(coefs_long$term[coefs_long$factor_term %in% "Thresholds"])
  if (length(thr_terms) > 0L) {
    term_order <- c(setdiff(term_order, thr_terms),
                    intersect(term_order, thr_terms))
  }

  if (!identical(reference_style, "row")) {
    coefs_long <- coefs_long[!coefs_long$is_reference, , drop = FALSE]
    term_order <- intersect(term_order, unique(coefs_long$term))
  }

  coefs_long$order_idx <- match(coefs_long$term, term_order)
  coefs_long <- coefs_long[order(coefs_long$order_idx,
                                  coefs_long$estimate_type,
                                  coefs_long$model_id), , drop = FALSE]
  rownames(coefs_long) <- NULL

  outcome_labels_auto <- vapply(frames, function(f) {
    f$info$dv_label %||% f$info$dv
  }, character(1))

  exp_headers_auto <- vapply(frames, function(f) {
    if (isTRUE(f$info$extras$exp_applied)) {
      f$info$extras$exp_header
    } else {
      NA_character_
    }
  }, character(1))
  names(exp_headers_auto) <- model_ids

  list(
    coefs_aligned = coefs_long,
    fit_stats_aligned = fit_stats,
    term_order = term_order,
    factor_ref_levels = factor_ref_levels,
    outcome_labels_auto = outcome_labels_auto,
    exp_headers_auto = exp_headers_auto,
    model_ids = model_ids,
    n_models = length(frames)
  )
}


# Reorder a term sequence so that coefs sharing the same `factor_term`
# stay contiguous (in factor-level order). Non-factor terms keep their
# relative order. Reference rows of the group land first within their
# group.
#
# Sorting key: `factor_level_pos` (an integer carried per row by the
# extract layer, see `match_coef_to_factor()`). For treatment-coded
# levels it is the level's position in the original `levels()` vector
# (so the displayed order matches `factor(x, levels = c(...))` rather
# than an alphabetical sort on the level string). For polynomial-coded
# levels (`.L`, `.Q`, `.C`, `^4`, ...) it is the polynomial degree so
# the table reads linear -> quadratic -> cubic -> ...
group_factor_terms <- function(term_order, coefs_long) {
  meta <- unique(coefs_long[, c("term", "factor_term", "factor_level",
                                  "factor_level_pos", "is_reference")])
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
    group <- meta$term[!is.na(meta$factor_term) & meta$factor_term == ft]
    group_meta <- meta[group, , drop = FALSE]
    group_meta <- group_meta[order(!group_meta$is_reference,
                                    group_meta$factor_level_pos,
                                    na.last = TRUE), , drop = FALSE]
    new <- intersect(group_meta$term, term_order)  # preserve any drops
    out <- c(out, new)
    visited[new] <- TRUE
  }
  out
}


# ---- pivot_aligned_wide() ------------------------------------------------

# Internal wide-format builder: reshapes a long aligned-coefs object into
# one row per term with per-model value columns. Intentionally retained
# (with its own unit tests) but NOT yet wired to a public output -- it is
# the foundation for a future side-by-side wide export. Kept rather than
# deleted so the tested shaping logic is ready when a consumer is added.
pivot_aligned_wide <- function(
    aligned,
    value_fields = c("estimate", "se", "ci_low", "ci_high",
                      "statistic", "df", "p_value"),
    model_labels = NULL) {
  coefs <- aligned$coefs_aligned
  if (nrow(coefs) == 0L) {
    return(empty_coefs_wide(value_fields, model_labels))
  }

  # Use canonical model_id order from `aligned` (input order from
  # the user). `unique(coefs$model_id)` would return the post-sort
  # alphabetical order, which de-aligns label_map (B5 audit fix).
  model_ids <- aligned$model_ids %||% unique(coefs$model_id)
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
