# ---- Subordinate blocks: the frozen identity ------------------------------
#
# A subordinate block -- ordinal cut-points, partial-PO effects, the scale
# equation of a heteroskedastic clm, mixed variance components, the two-part
# count components -- is named by an English string that travels the whole
# pipeline as `coefs$parent_var` and, after alignment, as `factor_term`.
#
# That string is the block's IDENTITY, not its caption. Seven producers
# write it (the ordinal / glmmTMB / pscl frames, the random-effects
# builder), nine sites match on it (the bottom-of-table sort below, the
# refs-first exemption, the section rule, the row-label branch, the
# keep / drop exemption, the cloglog refusal, the scale-effects gloss), and
# it is PUBLISHED as data by `broom::tidy()` (the `factor_term` column) and
# by `as_structured()` (`.variable`). It is frozen English and never
# translated; the caption a reader sees is resolved from it at render time
# by `.reg_block_label()`.
#
# The literals used to be re-typed at every one of those sites -- four lists
# of seven, plus three scalars, 52 occurrences in all -- so nothing in the
# source said that the sort list and the membership list had to hold the
# same seven strings. They diverged once already: the sort list was missing
# `Non-proportional effects` until 25bf3966, which let a multi-model table
# interleave that block among a later model's predictors.
#
# The ORDER of `.REG_BLOCK_TERMS` is load-bearing: it IS the bottom-of-table
# order (`align_frames()` walks it, moving each block in turn to the end).
# Every other consumer uses it as a SET, through `.reg_is_block()`.
.REG_BLOCK_TERMS <- c(
  "Zero-inflation",
  "Zero hurdle",
  "Dispersion",
  "Scale effects",
  # Before Thresholds, matching the single-model layout.
  "Non-proportional effects",
  "Thresholds",
  "Random effects"
)
.REG_BLOCK_ZI <- "Zero-inflation"
.REG_BLOCK_HURDLE <- "Zero hurdle"
.REG_BLOCK_DISP <- "Dispersion"
.REG_BLOCK_SCALE <- "Scale effects"
.REG_BLOCK_NPO <- "Non-proportional effects"
.REG_BLOCK_THRESH <- "Thresholds"
.REG_BLOCK_RE <- "Random effects"

# TRUE where `x` names a subordinate block. Vectorised, and FALSE on NA:
# `factor_term` is NA for every row that belongs to no factor group.
#
# Known limitation, pre-existing and unchanged here: a user variable
# literally named `Thresholds` (or any of the other six) is treated as a
# block by every one of these sites -- exempted from `keep` / `drop`,
# sorted to the bottom, preceded by a section rule. The identity is a
# string, so a collision is indistinguishable from the real thing.
.reg_is_block <- function(x) {
  !is.na(x) & x %in% .REG_BLOCK_TERMS
}

# The identity -> registry key map. One key per block: the block's
# CAPTION is a separate layer from its identity, and only the caption is
# ever translated.
.REG_BLOCK_STR_KEYS <- c(
  "Zero-inflation" = "label_block_zero_inflation",
  "Zero hurdle" = "label_block_zero_hurdle",
  "Dispersion" = "label_block_dispersion",
  "Scale effects" = "label_block_scale_effects",
  "Non-proportional effects" = "label_block_non_proportional",
  "Thresholds" = "label_block_thresholds",
  "Random effects" = "label_block_random_effects"
)

# The caption a reader sees for one `factor_term`. A block resolves
# through the registry; anything else -- a real factor variable -- is its
# own caption and comes back unchanged, so this is a total function of
# its input and safe to call on every header.
.reg_block_label <- function(term) {
  if (
    length(term) == 1L &&
      !is.na(term) &&
      term %in% names(.REG_BLOCK_STR_KEYS)
  ) {
    return(spicy_str(.REG_BLOCK_STR_KEYS[[term]]))
  }
  term
}


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
#        is_intercept     <- cf$is_intercept when the frame carries it
#                            (univariable-screen blocks tag their own
#                            fit's intercept under a per-block unique
#                            term); derived term == "(Intercept)"
#                            otherwise
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
  reference_style = c("row", "annotation", "footer", "none")
) {
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
  coefs_long <- do.call(
    rbind,
    lapply(seq_along(frames), function(i) {
      cf <- frames[[i]]$coefs
      singular_terms <- frames[[i]]$info$extras$singular_terms %||% character(0)
      test_type_col <- if (!is.null(cf$test_type)) {
        as.character(cf$test_type)
      } else {
        rep(NA_character_, nrow(cf))
      }
      # Optional per-row N (univariable screening frames: each predictor
      # block is its own fit, so N varies by row block). NA elsewhere.
      n_obs_col <- if (!is.null(cf$n_obs)) {
        as.numeric(cf$n_obs)
      } else {
        rep(NA_real_, nrow(cf))
      }
      # Optional per-row variance explained (univariable screening
      # frames again: the R^2 of each predictor's own fit, carried on
      # the block's first row like n_obs). NA elsewhere.
      r2_col <- if (!is.null(cf$r2)) {
        as.numeric(cf$r2)
      } else {
        rep(NA_real_, nrow(cf))
      }
      adj_r2_col <- if (!is.null(cf$adj_r2)) {
        as.numeric(cf$adj_r2)
      } else {
        rep(NA_real_, nrow(cf))
      }
      # Optional per-row outcome event counts (show_columns "n_events",
      # binomial fits): events observed in the row's factor level /
      # model totals for continuous rows. NA elsewhere.
      events_col <- if (!is.null(cf$events)) {
        as.numeric(cf$events)
      } else {
        rep(NA_real_, nrow(cf))
      }
      events_n_col <- if (!is.null(cf$events_n)) {
        as.numeric(cf$events_n)
      } else {
        rep(NA_real_, nrow(cf))
      }
      data.frame(
        model_id = rep(model_ids[i], nrow(cf)),
        outcome = rep(frames[[i]]$info$dv, nrow(cf)),
        term = cf$term,
        estimate_type = cf$estimate_type,
        estimate = cf$estimate,
        se = cf$std_error,
        ci_low = cf$ci_lower,
        ci_high = cf$ci_upper,
        statistic = cf$statistic,
        df = cf$df,
        p_value = cf$p_value,
        test_type = test_type_col,
        n_obs = n_obs_col,
        r2 = r2_col,
        adj_r2 = adj_r2_col,
        events = events_col,
        events_n = events_n_col,
        is_singular = cf$term %in% singular_terms,
        is_intercept = if (!is.null(cf$is_intercept)) {
          as.logical(cf$is_intercept)
        } else {
          cf$term == "(Intercept)"
        },
        is_reference = cf$is_ref,
        factor_term = ifelse(
          cf$parent_var == cf$term,
          NA_character_,
          cf$parent_var
        ),
        factor_level = ifelse(cf$label == cf$term, NA_character_, cf$label),
        factor_level_pos = as.integer(cf$factor_level_pos),
        # Outcome category for per-category AME rows (ordinal / multinomial);
        # NA for single-outcome frames that don't carry it. Drives the
        # renderer's per-category AME column pivot.
        outcome_level = if (!is.null(cf$outcome_level)) {
          as.character(cf$outcome_level)
        } else {
          NA_character_
        },
        # Probability of direction (Bayesian frames only; NA elsewhere).
        pd = if (!is.null(cf$pd)) {
          as.numeric(cf$pd)
        } else {
          NA_real_
        },
        # Per-parameter sampler diagnostics (Bayesian frames only).
        rhat = if (!is.null(cf$rhat)) {
          as.numeric(cf$rhat)
        } else {
          NA_real_
        },
        ess_bulk = if (!is.null(cf$ess_bulk)) {
          as.numeric(cf$ess_bulk)
        } else {
          NA_real_
        },
        ess_tail = if (!is.null(cf$ess_tail)) {
          as.numeric(cf$ess_tail)
        } else {
          NA_real_
        },
        mcse = if (!is.null(cf$mcse)) {
          as.numeric(cf$mcse)
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
    })
  )

  # ---- Build legacy-shape fit_stats from frames -------------------------
  fit_stats <- do.call(
    rbind,
    lapply(seq_along(frames), function(i) {
      as.data.frame(
        .compact_fit_stats_for_legacy(
          frames[[i]]$info$fit_stats,
          model_ids[i],
          frames[[i]]$info$dv,
          icc = frames[[i]]$info$random_effects$icc %||% NA_real_,
          n_groups = frames[[i]]$info$n_groups
        ),
        stringsAsFactors = FALSE
      )
    })
  )

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
    # Filter on is_intercept, not the literal "(Intercept)" term: the
    # univariable-screen blocks carry their own fits' intercepts under
    # per-block unique terms ("<pred>: (Intercept)"), tagged by the
    # frame's own is_intercept column. Component-block intercepts
    # (zero-inflation / dispersion) are NOT tagged and stay.
    intercept_terms <- unique(coefs_long$term[coefs_long$is_intercept])
    term_order <- setdiff(term_order, intercept_terms)
    coefs_long <- coefs_long[!coefs_long$is_intercept, , drop = FALSE]
  } else if (
    identical(intercept_position, "last") &&
      "(Intercept)" %in% term_order
  ) {
    term_order <- c(setdiff(term_order, "(Intercept)"), "(Intercept)")
  }

  term_order <- group_factor_terms(term_order, coefs_long)

  # Subordinate blocks always sort to the very bottom, after every predictor,
  # in the fixed order of `.REG_BLOCK_TERMS` -- that constant's order IS the
  # final bottom-of-table order. Their synthetic parents (created only by the
  # show_thresholds / show_re paths) would otherwise land at their
  # first-appearance position, which in a multi-model table can fall ahead of a
  # predictor a later model introduces.
  for (blk in .REG_BLOCK_TERMS) {
    blk_terms <- unique(coefs_long$term[coefs_long$factor_term %in% blk])
    if (length(blk_terms) > 0L) {
      blk_in_order <- intersect(term_order, blk_terms)
      # Within the Random effects block, the Residual row(s) sort LAST --
      # in a multi-model table the first-appearance order would otherwise
      # interleave a residual before a later model's slope / correlation
      # rows (spec section 5: residual closes the block).
      if (identical(blk, .REG_BLOCK_RE)) {
        is_resid <- startsWith(blk_in_order, "re::Residual::")
        blk_in_order <- c(blk_in_order[!is_resid], blk_in_order[is_resid])
      }
      term_order <- c(setdiff(term_order, blk_terms), blk_in_order)
    }
  }

  if (!identical(reference_style, "row")) {
    coefs_long <- coefs_long[!coefs_long$is_reference, , drop = FALSE]
    term_order <- intersect(term_order, unique(coefs_long$term))
  }

  coefs_long$order_idx <- match(coefs_long$term, term_order)
  coefs_long <- coefs_long[
    order(coefs_long$order_idx, coefs_long$estimate_type, coefs_long$model_id),
    ,
    drop = FALSE
  ]
  rownames(coefs_long) <- NULL

  outcome_labels_auto <- vapply(
    frames,
    function(f) {
      f$info$dv_label %||% f$info$dv
    },
    character(1)
  )

  exp_headers_auto <- vapply(
    frames,
    function(f) {
      if (isTRUE(f$info$extras$exp_applied)) {
        f$info$extras$exp_header
      } else {
        NA_character_
      }
    },
    character(1)
  )
  names(exp_headers_auto) <- model_ids

  # Per-model statistic-column header: the "t" TOKEN is API-stable, but
  # the displayed header follows the model's actual reference
  # distribution (coefs$test_type on the B rows): "z" for z-asymptotic
  # classes (glm, cox, ordinal, glmmTMB, resampling vcov), "t" for
  # t-referenced ones (lm, lmer-Satterthwaite). Stata prints z for logit
  # and t for regress; a hardcoded "t" header over z statistics
  # mislabels the column.
  stat_headers_auto <- vapply(
    frames,
    function(f) {
      tt <- unique(f$coefs$test_type[
        f$coefs$estimate_type == "B" &
          !f$coefs$is_ref
      ])
      tt <- tt[!is.na(tt)]
      # `tt` is the internal TOKEN; the header is its registry label.
      if (length(tt) == 1L && tt %in% c("t", "z")) {
        switch(tt, t = spicy_str("symbol_t"), z = spicy_str("symbol_z"))
      } else {
        NA_character_
      }
    },
    character(1)
  )
  names(stat_headers_auto) <- model_ids

  # Raw per-model grouping-factor counts (named integer vectors; NULL for
  # non-mixed models). Read by the fit-stat builders so the `n_groups` row
  # can carry a dynamic label ("N (Subject)") + numeric counts when every
  # model shares the same single grouping factor.
  n_groups_by_model <- stats::setNames(
    lapply(frames, function(f) f$info$n_groups),
    model_ids
  )

  # Per-model absorbed-intercept factors (character vectors; NULL for
  # non-fixest models). Drives the "Fixed effects" Yes/No block. The
  # DISTINCTION matters: n_groups counts every grouping factor (a
  # sample statistic, slope-only included), fixef_by_model lists only
  # genuinely absorbed intercepts (a design disclosure).
  fixef_by_model <- stats::setNames(
    lapply(frames, function(f) f$info$extras$fixef_intercept),
    model_ids
  )

  # Survival estimand horizons (resolved values, e.g. a numeric tau
  # even when the user passed "minmax") for the column headers.
  estimand_horizons <- NULL
  for (f in frames) {
    es <- f$info$extras$survival_estimands
    if (!is.null(es)) {
      estimand_horizons <- list(tau = es$tau, at_time = es$at_time)
      break
    }
  }

  # Models whose fit-stat NAs are display blanks by design (multinom
  # categories-as-columns pseudo-models after the first; the
  # univariable-screen bundle): the renderers keep those cells empty
  # instead of the mixed-table per-cell en-dash.
  blank_fit_stats_models <- model_ids[vapply(
    frames,
    function(f) isTRUE(f$info$extras$fit_stats_display_blank),
    logical(1)
  )]

  list(
    coefs_aligned = coefs_long,
    fit_stats_aligned = fit_stats,
    term_order = term_order,
    factor_ref_levels = factor_ref_levels,
    outcome_labels_auto = outcome_labels_auto,
    exp_headers_auto = exp_headers_auto,
    stat_headers_auto = stat_headers_auto,
    model_ids = model_ids,
    n_models = length(frames),
    n_groups_by_model = n_groups_by_model,
    fixef_by_model = fixef_by_model,
    estimand_horizons = estimand_horizons,
    blank_fit_stats_models = blank_fit_stats_models
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
  meta <- unique(coefs_long[, c(
    "term",
    "factor_term",
    "factor_level",
    "factor_level_pos",
    "is_reference"
  )])
  meta <- meta[!duplicated(meta$term), , drop = FALSE]
  rownames(meta) <- meta$term

  out <- character(0)
  visited <- setNames(logical(length(term_order)), term_order)

  for (t in term_order) {
    if (isTRUE(visited[[t]])) {
      next
    }
    ft <- meta[t, "factor_term"]
    if (is.na(ft)) {
      out <- c(out, t)
      visited[[t]] <- TRUE
      next
    }
    group <- meta$term[!is.na(meta$factor_term) & meta$factor_term == ft]
    group_meta <- meta[group, , drop = FALSE]
    # Subordinate BLOCKS (their rows all share the block label as
    # factor_term) keep their builder-supplied row order (factor_level_pos =
    # a sequence): the refs-first rule below is a per-FACTOR convention and
    # would float every reference row to the top of the whole block.
    is_block <- .reg_is_block(ft)
    group_meta <- if (is_block) {
      group_meta[
        order(group_meta$factor_level_pos, na.last = TRUE),
        ,
        drop = FALSE
      ]
    } else {
      group_meta[
        order(
          !group_meta$is_reference,
          group_meta$factor_level_pos,
          na.last = TRUE
        ),
        ,
        drop = FALSE
      ]
    }
    new <- intersect(group_meta$term, term_order) # preserve any drops
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
  value_fields = c(
    "estimate",
    "se",
    "ci_low",
    "ci_high",
    "statistic",
    "df",
    "p_value"
  ),
  model_labels = NULL
) {
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
        length(model_labels),
        n_models
      ),
      class = "spicy_invalid_input"
    )
  }
  label_map <- setNames(model_labels, model_ids)

  # Term-level metadata: one row per (term, estimate_type), in the
  # canonical display order.
  meta_cols <- c(
    "term",
    "estimate_type",
    "is_intercept",
    "is_reference",
    "factor_term",
    "factor_level",
    "order_idx",
    "test_type"
  )
  meta <- coefs[
    !duplicated(coefs[, c("term", "estimate_type")]),
    meta_cols,
    drop = FALSE
  ]
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
  if (is.null(model_labels)) {
    return(base)
  }
  for (lbl in model_labels) {
    for (vf in value_fields) {
      base[[paste0(lbl, "__", vf)]] <- numeric(0)
    }
  }
  base
}
