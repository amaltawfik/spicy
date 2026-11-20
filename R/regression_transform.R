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
  if (identical(method, "none") || is.null(method)) {
    return(coefs)
  }
  if (is.null(coefs) || nrow(coefs) == 0L) {
    return(coefs)
  }

  # Intercepts are excluded from the family: the main one by term, the
  # component-block ones (zero-inflation / dispersion, terms
  # "Zero-inflation::(Intercept)", ...) by their display label, and the
  # prefixed ones of a multinomial or Heckman model ("Student:
  # (Intercept)", "selection: (Intercept)") by `is_intercept`.
  family_mask <- coefs$term != "(Intercept)" &
    !(coefs$label %in% "(Intercept)") &
    !((coefs$is_intercept %||% FALSE) %in% TRUE) &
    !coefs$is_ref &
    !is.na(coefs$p_value)

  if (!any(family_mask)) {
    return(coefs)
  }

  # The test of a GAM's smooth term ("edf" row) is a hypothesis on the
  # model's mean, like a coefficient's: it joins the B family.
  fam_type <- ifelse(coefs$estimate_type == "edf", "B", coefs$estimate_type)
  for (et in unique(fam_type[family_mask])) {
    rows <- which(family_mask & fam_type == et)
    if (length(rows) == 0L) {
      next
    }
    coefs$p_value[rows] <- stats::p.adjust(
      coefs$p_value[rows],
      method = method
    )
  }
  coefs
}


# ---- keep / drop filter ---------------------------------------------------

# The names `keep` and `drop` match, one per row of the aligned
# coefficients: the term, except for the error terms of a selection
# model, which are coefficients of `coef(fit)` ("sigma", "rho",
# "invMillsRatio") and are matched under that name, without the block key
# their row carries ("Error terms::rho").
.reg_filter_terms <- function(ca) {
  terms <- ca$term
  err <- ca$factor_term %in% .REG_BLOCK_ERROR
  terms[err] <- sub(paste0("^", .REG_BLOCK_ERROR, "::"), "", terms[err])
  terms
}

# The keys of `labels` that the frames name and the fits do not
# (`.reg_label_keys()`), as `keep` and `drop` list them: the rows of the
# subordinate blocks ("s(age)", "rho", "Precision::persons") and the
# reference rows of the factors ("sexMale", the name the level's
# coefficient would take; a multinomial row drops its category, as its
# coefficient rows do). `labels` renames each row under that name
# (`format_term_label()`).
.reg_frame_label_keys <- function(frames) {
  unique(unlist(lapply(frames, function(f) {
    cf <- f$coefs
    blk <- .reg_is_block(cf$parent_var %||% rep(NA_character_, nrow(cf)))
    is_ref <- cf$is_ref %in% TRUE & !blk
    ref <- cf$term[is_ref]
    ol <- (cf$outcome_level %||% rep(NA_character_, nrow(cf)))[is_ref]
    pre <- !is.na(ol) & startsWith(ref, paste0(ol, ": "))
    ref[pre] <- substring(ref[pre], nchar(ol[pre]) + 3L)
    c(
      .reg_filter_terms(list(
        term = cf$term[blk],
        factor_term = cf$parent_var[blk]
      )),
      ref
    )
  })))
}

# "The terms of this table: ...", the first twelve: the names a regression
# table builds from its models, listed where a pattern of `keep` / `drop`
# or a name of `labels` matches none of them.
.reg_terms_sentence <- function(terms) {
  terms <- unique(terms)
  paste0(
    "The terms of this table: ",
    paste(.quote_val(utils::head(terms, 12L)), collapse = ", "),
    if (length(terms) > 12L) ", ..." else "",
    "."
  )
}

# Filter the aligned long format by regular expression match on the
# `term` column. Applied AFTER p_adjust (so adjusted p-values
# reflect the model's full coefficient family, not just the
# displayed subset) and AFTER alignment (so canonical term ordering
# is preserved for the surviving terms).
#
# `keep` and `drop` are validated upstream as mutually exclusive
# character vectors of patterns: term names or regular expressions
# (`.reg_pattern_hits()`). Multiple patterns combine with logical OR (a
# row matches if ANY pattern matches).
#
# Side effect on `aligned`:
#   * `coefs_aligned` filtered to the surviving terms
#   * `term_order` filtered to the same set
#   * `factor_ref_levels` cleaned of factors whose group is now empty
#
# `warn = FALSE` leaves the patterns that match no term to the caller:
# the driver of a stacked table, whose panels each see one model, checks
# them once against the terms of every model (`.reg_warn_dead_patterns()`).
apply_keep_drop_filter <- function(
  aligned,
  keep = NULL,
  drop = NULL,
  warn = TRUE,
  call = rlang::caller_env()
) {
  if (is.null(keep) && is.null(drop)) {
    return(aligned)
  }
  if (
    is.null(aligned$coefs_aligned) ||
      nrow(aligned$coefs_aligned) == 0L
  ) {
    return(aligned)
  }

  terms <- .reg_filter_terms(aligned$coefs_aligned)
  # The error terms of a selection model follow `keep` / `drop`; the
  # other blocks are exempt (below).
  err <- aligned$coefs_aligned$factor_term %in% .REG_BLOCK_ERROR
  matches_any <- function(term, patterns) {
    Reduce(
      `|`,
      lapply(patterns, .reg_pattern_hits, terms = term),
      rep(FALSE, length(term))
    )
  }

  if (isTRUE(warn)) {
    .reg_warn_dead_patterns(terms, keep, drop, call = call)
  }

  keep_mask <- if (!is.null(keep)) {
    matches_any(terms, keep)
  } else {
    rep(TRUE, length(terms))
  }
  drop_mask <- if (!is.null(drop)) {
    matches_any(terms, drop)
  } else {
    rep(FALSE, length(terms))
  }
  final_mask <- keep_mask & !drop_mask

  # Subordinate blocks are exempt from keep / drop: `keep` / `drop` select
  # PREDICTORS, and a regex accidentally matching an ordinal cut-point or a
  # synthetic random-effect key ("re::Subject::Days") would mutilate the block
  # into a statistically incoherent subset (e.g. a correlation without its
  # variances). The whole-block switches are `show_thresholds` / `show_re`.
  # The one exception: the smooth terms of a GAM are predictors of the
  # model, so their rows obey `keep` / `drop` like the coefficients, and
  # `drop = "^s\\("` removes the block with its header and its note.
  ca <- aligned$coefs_aligned
  subordinate <- (.reg_is_block(ca$factor_term) &
    !err &
    !ca$factor_term %in% .REG_BLOCK_SMOOTH) |
    (!is.null(ca$estimate_type) & ca$estimate_type == "vc")
  final_mask <- final_mask | subordinate
  # Intercepts are exempt from keep / drop matching: `show_intercept`
  # alone decides their visibility. Without the exemption the
  # univariable-screen intercepts (keyed "<pred>: (Intercept)") would
  # match any pattern containing the predictor name while the
  # multivariable "(Intercept)" would not -- an asymmetric, layout-
  # dependent filter no regex on predictors should trigger. A uv
  # intercept still FOLLOWS its own block: it survives exactly when a
  # non-intercept row of its predictor's fit does, so a dropped block
  # never leaves an orphan intercept behind.
  is_int <- if (!is.null(ca$is_intercept)) {
    ca$is_intercept %in% TRUE
  } else {
    rep(FALSE, nrow(ca))
  }
  if (any(is_int)) {
    owner <- ifelse(is.na(ca$factor_term), ca$term, ca$factor_term)
    surviving_owners <- unique(owner[final_mask & !is_int])
    # Only a univariable-screen intercept is keyed by a predictor; a
    # multinomial one ("Student: (Intercept)") is keyed by its outcome.
    uv_pred <- sub(": \\(Intercept\\)$", "", ca$term)
    uv_int <- is_int &
      grepl(": \\(Intercept\\)$", ca$term) &
      uv_pred %in% owner[!is_int]
    final_mask[is_int] <- !uv_int[is_int] |
      (uv_pred[is_int] %in% surviving_owners)
  }

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

# A pattern that matches NO term is almost always a mistake -- the
# classic one being a pattern written against a display LABEL ("Sexe")
# instead of the model term ("sexMale"). With `drop` the failure is
# invisible: the filter is inert and the table stays plausible. Name
# every dead pattern instead of staying silent (the `coef_omit` bug class
# documented in the 2026-08 field audit). `terms`: every term the
# patterns were matched against.
#
# A pattern that names no term and is no regular expression either is
# refused, in the name of `call`: it could select nothing, under any
# table.
.reg_warn_dead_patterns <- function(
  terms,
  keep = NULL,
  drop = NULL,
  call = rlang::caller_env()
) {
  dead <- character(0)
  for (p in c(keep, drop)) {
    if (!any(.reg_pattern_hits(p, terms))) {
      dead <- c(dead, p)
    }
  }
  if (length(dead) == 0L) {
    return(invisible(NULL))
  }
  arg <- if (is.null(keep)) "`drop`" else "`keep`"
  bad <- dead[!vapply(dead, .is_valid_perl_regex, logical(1))]
  if (length(bad) > 0L) {
    spicy_abort(
      c(
        sprintf(
          "%s must hold term names or regular expressions; %s %s neither.",
          arg,
          paste(.quote_val(bad), collapse = ", "),
          if (length(bad) == 1L) "is" else "are"
        ),
        "i" = paste0(
          "Escape a literal character with a double backslash: ",
          "`\"\\\\(\"` for a parenthesis. ",
          .reg_terms_sentence(terms)
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  spicy_warn(
    c(
      sprintf(
        "%s pattern(s) %s matched no coefficient term.",
        arg,
        paste(.quote_val(dead), collapse = ", ")
      ),
      "i" = paste0(
        "A pattern is a term name as in `stats::coef()` (e.g. ",
        "\"sexMale\", \"factor(cyl)8\"; an rms fit's in lm's spelling, ",
        "\"sexMale\" for \"sex=Male\"), written as is, or a regular ",
        "expression over those names, never a display label. ",
        .reg_terms_sentence(terms)
      )
    ),
    class = "spicy_no_match"
  )
}

# Which of `terms` a `keep` / `drop` pattern selects: the term it names
# exactly, as written ("log(bili)", "I(age^2)", "s(age)", whose
# parentheses and carets a regular expression reads otherwise), and the
# terms it matches as a Perl regular expression. A pattern that is no
# regular expression selects by name only.
.reg_pattern_hits <- function(pattern, terms) {
  hit <- terms %in% pattern
  if (.is_valid_perl_regex(pattern)) {
    hit <- hit | grepl(pattern, terms, perl = TRUE)
  }
  hit
}
