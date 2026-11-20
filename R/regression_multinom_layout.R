# Multinomial outcome-as-columns layout.
# Spec: dev/multinom_outcome_columns_spec.md (user-validated 2026-07-04).
#
# Published multinomial tables put predictors in rows and one column
# group per non-reference outcome -- the layout that lets a reader
# compare a predictor's effect across equations at a glance (Long &
# Freese 2014; SPSS NOMREG / Stata mlogit reporting practice). The
# frame's long shape (one "<category>: <term>" row per
# (outcome, predictor)) is a storage format, not the publication one:
# the DISPLAY path explodes the single multinom frame into
# per-category pseudo-model frames and reuses the multi-model column
# machinery verbatim. broom::tidy(), as_structured(), and
# `output = "long"` keep the original long frame (outcome_level and
# prefixed terms intact).

# Gate: the layout engages for a SINGLE nnet::multinom model without
# nested comparisons. Multi-model multinomial tables and nested
# hierarchies keep the per-outcome-rows layout (categories-within-
# models would need two spanner levels) -- documented in
# ?table_regression.
#
# A sampleSelection model (selection(), heckit(), treatReg()) follows the
# same rule: alone, its two equations are the column groups.
.multinom_columns_active <- function(models, nested) {
  if (isTRUE(nested)) {
    return(FALSE)
  }
  cls <- c("multinom", "selection")
  # A bare fit is a classed list, so test the model class FIRST --
  # `is.list()` alone cannot separate a multinom object from a
  # list-of-models.
  if (inherits(models, cls)) {
    return(TRUE)
  }
  is.list(models) && length(models) == 1L && inherits(models[[1L]], cls)
}


# Blank every numeric fit statistic (typed NA) so the pseudo-model
# frames after the first contribute empty fit-stat cells: n / AIC are
# MODEL-level quantities and must print once, under the first category
# group, not once per category.
.blank_fit_stats <- function(fs) {
  for (nm in names(fs)) {
    v <- fs[[nm]]
    if (is.numeric(v) && length(v) == 1L) {
      fs[[nm]] <- if (is.integer(v)) NA_integer_ else NA_real_
    }
  }
  fs
}


# Explode a single multinom frame into one pseudo-model frame per
# outcome category. Category order: the non-reference categories in
# response-level order, then the reference category LAST -- and only
# when it carries rows at all (per-category AME covers the reference
# outcome, which has no B rows by construction; its column group
# renders en-dashed coefficient cells next to its real AME values,
# which is exactly the message: the reference has no equation).
#
# `term` and `label` drop the "<category>: " prefix built by
# .multinom_coefs() / .multinom_reference_rows() / the AME attach, so
# the same bare predictor term keys one row across every category
# column group.
#
# Returns list(frames, model_ids) or NULL when the frame carries no
# outcome_level column (defensive; multinom frames always do).
.explode_multinom_frame <- function(frame) {
  cf <- frame$coefs
  if (is.null(cf$outcome_level)) {
    return(NULL) # nocov
  }
  present <- unique(cf$outcome_level[!is.na(cf$outcome_level)])
  if (length(present) == 0L) {
    return(NULL) # nocov
  }

  ref <- frame$info$extras$reference_outcome %||% NA_character_
  lev <- frame$info$extras$response_levels %||% present
  # Matrix-response multinom (cbind of counts) has fit$lev = NULL, so
  # response_levels arrives as character(0) -- which %||% does NOT
  # catch. Fall back to the levels actually present in the coefs, and
  # refuse an empty explode outright (NULL -> the orchestrator keeps
  # the rows layout) rather than render a silently empty table.
  if (!length(lev)) {
    lev <- present
  }
  cats <- c(setdiff(lev, ref), if (!is.na(ref) && ref %in% present) ref)
  # A selection model: the outcome equation first, as the title names its
  # outcome, then the selection equation. Its rows of the whole model
  # (the "Error terms" block) print once, under the first group.
  is_sel <- identical(frame$info$class, "selection")
  if (is_sel) {
    cats <- c("outcome", "selection")
  }
  cats <- cats[cats %in% present]
  if (!length(cats)) {
    return(NULL) # nocov
  }
  # The groups of a selection model are named by their equation: the
  # English name identifies it, the spanner prints it in the table
  # language.
  ids <- if (is_sel) vapply(cats, capitalize_first, "", USE.NAMES = FALSE)

  pseudo <- vector("list", length(cats))
  for (k in seq_along(cats)) {
    cat_k <- cats[[k]]
    rows <- cf$outcome_level %in% cat_k
    if (is_sel && k == 1L) {
      rows <- rows | is.na(cf$outcome_level)
    }
    sub <- cf[rows, , drop = FALSE]
    prefix <- paste0(cat_k, ": ")
    strip <- function(x) {
      ifelse(startsWith(x, prefix), substring(x, nchar(prefix) + 1L), x)
    }
    sub$term <- strip(sub$term)
    sub$label <- strip(sub$label)
    # Single-outcome pseudo-frame: no per-category pivot inside a group.
    sub$outcome_level <- NA_character_
    rownames(sub) <- NULL

    pk <- frame
    pk$coefs <- sub
    pk$info$model_id <- if (is_sel) ids[[k]] else cat_k
    if (k > 1L) {
      pk$info$fit_stats <- .blank_fit_stats(pk$info$fit_stats)
      # Display-blank marker: these NAs mean "the model-level value is
      # printed once, under the first category group", NOT "stat
      # undefined for this class" -- the renderers keep the cells
      # blank instead of the mixed-table per-cell en-dash.
      pk$info$extras$fit_stats_display_blank <- TRUE
    }
    pseudo[[k]] <- pk
  }
  if (!is_sel) {
    return(list(frames = pseudo, model_ids = cats))
  }
  list(
    frames = pseudo,
    model_ids = ids,
    spanners = vapply(
      cats,
      function(eq) capitalize_first(.reg_equation_display(eq)),
      "",
      USE.NAMES = FALSE
    )
  )
}


# Spanner labels for the category column groups. Default: the category
# names (the reference is named by the mandatory "Reference outcome:"
# footer note, so repeating "vs <ref>" on every spanner would be
# redundant ink). `outcome_labels` -- which has no per-model meaning in
# a single-model table -- is reused as the override for the
# publication style ("Student vs Employed", ...): a character vector
# matching the NON-reference categories in order; the reference
# category's AME-only group (when present) keeps its own name.
.multinom_columns_spanners <- function(
  cats,
  reference_outcome,
  outcome_labels,
  selection = FALSE
) {
  spanners <- cats
  if (is.null(outcome_labels)) {
    return(spanners)
  }
  non_ref_idx <- which(!(cats %in% reference_outcome))
  # A selection model alone: one label per equation, outcome first.
  if (
    selection &&
      (!is.character(outcome_labels) || length(outcome_labels) != length(cats))
  ) {
    spicy_abort(
      c(
        paste0(
          "`outcome_labels` must be a character vector with one label ",
          "per equation for a selection model alone, in order: the ",
          "outcome equation, then the selection equation."
        ),
        "x" = if (is.character(outcome_labels)) {
          sprintf(
            "You supplied %d label(s) for %d equations.",
            length(outcome_labels),
            length(cats)
          )
        } else {
          sprintf("You supplied a <%s>.", class(outcome_labels)[[1L]])
        }
      ),
      class = "spicy_invalid_input"
    )
  }
  if (!is.character(outcome_labels)) {
    spicy_abort(
      c(
        paste0(
          "`outcome_labels` must be a character vector for the ",
          "multinomial columns layout (it relabels the category ",
          "spanners)."
        ),
        "x" = sprintf("You supplied a <%s>.", class(outcome_labels)[[1L]])
      ),
      class = "spicy_invalid_input"
    )
  }
  if (length(outcome_labels) != length(non_ref_idx)) {
    spicy_abort(
      c(
        paste0(
          "`outcome_labels` must be a character vector with one ",
          "label per non-reference outcome category for the ",
          "multinomial columns layout."
        ),
        "x" = sprintf(
          "You supplied %d label(s) for %d non-reference categor%s (%s).",
          length(outcome_labels),
          length(non_ref_idx),
          if (length(non_ref_idx) == 1L) "y" else "ies",
          paste(cats[non_ref_idx], collapse = ", ")
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  # Duplicated spanners would render two indistinguishable column
  # groups (and dedupe data.frame colnames to "Same: B.2") --
  # model_labels, the argument these spanners replace, refuses
  # duplicates fail-fast; hold the override to the same bar.
  if (anyDuplicated(outcome_labels)) {
    spicy_abort(
      c(
        "`outcome_labels` must be unique (they label the category spanners).",
        "x" = sprintf(
          "Duplicated: %s.",
          paste(
            unique(outcome_labels[duplicated(outcome_labels)]),
            collapse = ", "
          )
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  spanners[non_ref_idx] <- outcome_labels
  spanners
}
