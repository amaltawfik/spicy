#' Print method for bivariate linear-model tables
#'
#' @description
#' Formats and prints a `spicy_continuous_lm_table` object as a styled
#' ASCII table using [spicy_print_table()].
#'
#' @param x A `data.frame` of class `"spicy_continuous_lm_table"` as
#'   returned by [table_continuous_lm()].
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_continuous_lm()], [spicy_print_table()]
#' @keywords internal
#' @export
print.spicy_continuous_lm_table <- function(x, ...) {
  # This method re-formats from the raw values, so a journal style used
  # to build the table has to be back in force here (see
  # `.style_stamp()`); with no style it is a no-op.
  .style_pushed <- .style_restore(x)
  on.exit(.style_end(.style_pushed), add = TRUE)
  digits <- attr(x, "digits") %||% 2L
  fit_digits <- attr(x, "fit_digits") %||% 2L
  effect_size_digits <- attr(x, "effect_size_digits") %||% 2L
  p_digits <- attr(x, "p_digits") %||% 3L
  decimal_mark <- attr(x, "decimal_mark") %||% "."
  ci_level <- attr(x, "ci_level") %||% 0.95
  by_label <- attr(x, "by_label") %||%
    spicy_str("title_continuous_lm_by_fallback")
  show_statistic <- attr(x, "show_statistic") %||% FALSE
  show_p_value <- attr(x, "show_p_value") %||% TRUE
  show_n <- attr(x, "show_n") %||% TRUE
  show_weighted_n <- attr(x, "show_weighted_n") %||% FALSE
  effect_size <- attr(x, "effect_size") %||% "none"
  show_effect_size_ci <- attr(x, "show_effect_size_ci") %||% FALSE
  r2_type <- attr(x, "r2_type") %||% "r2"
  show_ci <- attr(x, "show_ci") %||% TRUE
  align <- attr(x, "align") %||% "decimal"

  # One spec for the frame and for its headers, from the attributes
  # read above -- the same object `table_continuous_lm()` hands to the
  # exporters, rebuilt here because print() may be called on a stored
  # table long after that call returned.
  spec <- .lm_column_spec(
    x,
    ci_level = ci_level,
    show_statistic = show_statistic,
    show_p_value = show_p_value,
    show_n = show_n,
    show_weighted_n = show_weighted_n,
    effect_size = effect_size,
    effect_size_ci = show_effect_size_ci,
    r2_type = r2_type,
    ci = show_ci,
    decimal_mark = decimal_mark
  )

  display_df <- build_wide_display_df_continuous_lm(
    x,
    digits = digits,
    fit_digits = fit_digits,
    effect_size_digits = effect_size_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    ci_level = ci_level,
    show_statistic = show_statistic,
    show_p_value = show_p_value,
    show_n = show_n,
    show_weighted_n = show_weighted_n,
    effect_size = effect_size,
    effect_size_ci = show_effect_size_ci,
    r2_type = r2_type,
    ci = show_ci,
    spec = spec
  )

  # The header a reader sees is the registry label, never the frozen
  # column key. No shape guard is needed, unlike the categorical
  # console: `.lm_labels()` maps `names(display_df)` to a vector of the
  # same length by construction, so `spicy_print_table()`'s abort on a
  # mismatched label vector is unreachable from here.
  header_labels <- .lm_labels(names(display_df), .lm_spec_labels(spec))

  align_left <- 1L
  if (identical(align, "decimal")) {
    numeric_cols <- setdiff(seq_along(display_df), align_left)
    for (j in numeric_cols) {
      display_df[[j]] <- decimal_align_strings(
        display_df[[j]],
        decimal_mark = decimal_mark
      )
    }
    align_center <- numeric_cols
  } else if (identical(align, "center")) {
    align_center <- setdiff(seq_along(display_df), align_left)
  } else {
    # "right": all numeric columns right-aligned, so nothing is centred.
    align_center <- integer(0)
  }

  # Auto-select padding: use 0 (compact) when the default 2-char
  # padding would overflow the console.
  # Each column in build_ascii_table uses: 1 (gutter) + w[i] + 1
  # (gutter) chars, plus 1 char for the vertical separator after
  # column 1; `padding` is added to each w[i].
  # Measured on what is PRINTED -- the labels, not the keys: measuring
  # the keys while printing the labels would leave this decision
  # disagreeing with the header the reader actually gets.
  #
  # And measured the way `ascii_table_widths()` measures, by display
  # WIDTH rather than character count: `nchar()` counts a CJK glyph as
  # one where the renderer lays it out as two, so a wide label made this
  # decision over-estimate how much room the compact layout needed and
  # split the table into panels the console had space for.
  # `nchar(NA_character_)` is NA on top of that, which would turn the
  # comparison below into "missing value where TRUE/FALSE needed".
  padding <- 2L
  col_widths <- vapply(
    seq_along(display_df),
    function(i) {
      max(
        crayon::col_nchar(
          c(header_labels[i], as.character(display_df[[i]])),
          type = "width"
        ),
        na.rm = TRUE
      )
    },
    numeric(1)
  )
  console_w <- getOption("width", 80L)
  normal_width <- sum(col_widths + padding + 2L) + 1L
  if (normal_width > console_w) {
    padding <- 0L
  }

  # APA-style footer: covariate-adjustment estimand + non-classical
  # SE-estimator disclosure, built by the shared helper so the console
  # and every rich exporter carry the same note.
  note <- .tclm_note_text(x)

  spicy_print_table(
    display_df,
    title = .continuous_lm_title(by_label),
    note = note,
    padding = padding,
    first_column_line = TRUE,
    row_total_line = FALSE,
    bottom_line = FALSE,
    align_left_cols = align_left,
    align_center_cols = align_center,
    group_sep_rows = integer(0),
    display_labels = header_labels
  )

  invisible(x)
}

# ---- Table title -----------------------------------------------------------

# Internal: the title of a bivariate linear-model table, from the label
# of the grouping / predictor variable. Single source for the console
# header and the caption the rendering engines set, so the two can
# never drift apart (same contract as `.categorical_title()`).
.continuous_lm_title <- function(by_label) {
  spicy_fmt("title_continuous_lm_by", by_label)
}

# ---- Shared note builder ---------------------------------------------------

# Internal: build the table note shared by the console print method
# and every rich exporter from the attributes stored on a
# `spicy_continuous_lm_table` object.
#
#   * Line 1 (covariate-adjusted models only): names the covariate(s)
#     and the adjustment estimand explicitly because the
#     interpretation of the displayed `emmean` column changes with
#     the method: "proportional" = G-computation over the observed
#     covariate distribution; "balanced" = synthetic-grid
#     equal-weight marginal means. Without the method tag the user
#     cannot tell which estimand they are reading.
#   * Line 2 (non-classical `vcov` only): discloses the SE estimator.
#     Robust / resampling standard errors are never silently
#     labelled -- same doctrine as table_regression()'s footers.
#
# Returns NULL when there is nothing to disclose; otherwise a single
# string with "Note. " prefixed to the FIRST line and lines joined by
# "\n" (the console renderer prints multi-line notes as-is; the rich
# engines collapse the newlines themselves).
.tclm_note_text <- function(x) {
  covariates <- attr(x, "covariates") %||% character()
  adjustment <- attr(x, "adjustment") %||% NA_character_
  vcov_type <- attr(x, "vcov_type") %||% "classical"
  cluster_name <- attr(x, "cluster_name") %||% NA_character_
  # Valid bootstrap replicate counts, one lm fit per table variable:
  # a single shared count when every fit kept the same number, a
  # range otherwise (failed replicates are dropped per fit).
  boot_valid <- if ("boot_n_valid" %in% names(x)) {
    v <- unique(stats::na.omit(as.integer(x[["boot_n_valid"]])))
    if (length(v)) v else NULL
  } else {
    NULL
  }

  lines <- character()
  if (length(covariates) > 0L && !is.na(adjustment)) {
    # The `adjustment` TOKEN stays an API identifier; only its display form
    # comes from the registry.
    adjustment_label <- switch(
      adjustment,
      proportional = spicy_str("note_adjustment_proportional"),
      balanced = spicy_str("note_adjustment_balanced"),
      adjustment
    )
    lines <- c(
      lines,
      spicy_fmt(
        "note_adjusted_for",
        paste(covariates, collapse = ", "),
        adjustment_label
      )
    )
  }
  if (!identical(vcov_type, "classical")) {
    lines <- c(
      lines,
      spicy_fmt(
        "note_std_errors_single",
        .tclm_vcov_label(vcov_type, cluster_name, boot_valid)
      )
    )
  }
  # Missing-data disclosure (rows dropped for a missing `by` value or
  # a missing weight), assembled by table_continuous_lm() -- the same
  # ledger convention as table_continuous() / table_categorical().
  missing_note <- attr(x, "missing_note")
  if (!is.null(missing_note)) {
    lines <- c(lines, missing_note)
  }
  if (length(lines) == 0L) {
    return(NULL)
  }
  lines[1L] <- paste0(spicy_str("note_prefix"), lines[1L])
  paste(lines, collapse = "\n")
}

# Internal: human-readable label for a non-classical SE estimator.
# `cluster_name` (the resolved cluster column name, or NA when the
# cluster vector was supplied without a recoverable name) is only
# used by the CR* branch.
# `boot_valid`: integer vector of valid bootstrap replicate counts
# (one lm fit per table variable); NULL outside vcov = "bootstrap".
.tclm_vcov_label <- function(
  vcov_type,
  cluster_name = NA_character_,
  boot_valid = NULL
) {
  if (startsWith(vcov_type, "HC")) {
    return(spicy_fmt("note_vcov_hc", vcov_type))
  }
  if (startsWith(vcov_type, "CR")) {
    # The regression family's `note_vcov_cr` bundles the cluster fragment;
    # here it is optional, so the two halves are composed instead.
    label <- spicy_fmt("note_vcov_cr_bare", vcov_type)
    if (
      is.character(cluster_name) &&
        length(cluster_name) == 1L &&
        !is.na(cluster_name) &&
        nzchar(cluster_name)
    ) {
      label <- paste0(label, spicy_fmt("note_vcov_cluster_by", cluster_name))
    }
    return(label)
  }
  if (identical(vcov_type, "bootstrap")) {
    reps <- if (is.null(boot_valid) || !length(boot_valid)) {
      ""
    } else if (length(boot_valid) == 1L) {
      spicy_fmt("note_vcov_bootstrap_reps", boot_valid)
    } else {
      spicy_fmt(
        "note_vcov_bootstrap_reps_range",
        min(boot_valid),
        max(boot_valid)
      )
    }
    return(spicy_fmt("note_vcov_bootstrap", reps))
  }
  if (identical(vcov_type, "jackknife")) {
    return(spicy_str("note_vcov_jackknife_plain"))
  }
  vcov_type # nocov -- defensive: `vcov` is validated upstream
}

# ---- Coercion to plain data.frame / tibble --------------------------------

# Internal: drop spicy classes and rendering-only attributes from an
# object returned by table_continuous_lm(), keeping only the
# data.frame contract plus the `by_var` provenance attribute. Used by
# every coercion / broom S3 method.
unclass_spicy_continuous_lm_table <- function(x) {
  by_var <- attr(x, "by_var", exact = TRUE)
  attr_names <- names(attributes(x))
  drop <- setdiff(attr_names, c("names", "row.names", "class"))
  for (nm in drop) {
    attr(x, nm) <- NULL
  }
  class(x) <- "data.frame"
  if (!is.null(by_var)) {
    attr(x, "by_var") <- by_var
  }
  x
}

#' Coerce a `spicy_continuous_lm_table` to a plain data frame or tibble
#'
#' These S3 methods strip the `"spicy_continuous_lm_table"` /
#' `"spicy_table"` classes and the rendering-only attributes
#' (`digits`, `decimal_mark`, `ci_level`, ...) from an object returned
#' by [table_continuous_lm()] so the underlying long-format data can
#' be manipulated with downstream tools (`dplyr`, `tidyr`, etc.) under
#' the standard `data.frame` / `tbl_df` contract. The single attribute
#' `"by_var"` is preserved as a lightweight provenance marker; all
#' other spicy attributes are dropped. The original `x` is unaffected,
#' and `print(x)` continues to render the formatted ASCII table.
#'
#' @param x A `spicy_continuous_lm_table` returned by
#'   [table_continuous_lm()].
#' @param row.names,optional Standard [base::as.data.frame()]
#'   arguments. Currently ignored: the long format already carries
#'   integer row names and explicit columns.
#' @param ... Further arguments passed to [tibble::as_tibble()] (for
#'   the tibble method) or ignored (for the `as.data.frame()` method).
#'
#' @return A plain `data.frame` (or `tbl_df`) with the same rows and
#'   columns as the long output of [table_continuous_lm()].
#'
#' @seealso [tidy.spicy_continuous_lm_table()],
#'   [glance.spicy_continuous_lm_table()] for cleaner broom-style
#'   pivots tailored to downstream pipelines.
#'
#' @name as.data.frame.spicy_continuous_lm_table
#' @keywords internal
NULL

#' @rdname as.data.frame.spicy_continuous_lm_table
#' @exportS3Method base::as.data.frame
as.data.frame.spicy_continuous_lm_table <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...
) {
  unclass_spicy_continuous_lm_table(x)
}

#' @rdname as.data.frame.spicy_continuous_lm_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_continuous_lm_table <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    spicy_abort("Install package 'tibble'.", class = "spicy_missing_pkg")
  }
  tibble::as_tibble(unclass_spicy_continuous_lm_table(x), ...)
}

# ---- broom integration ----------------------------------------------------

#' Tidying methods for a `spicy_continuous_lm_table`
#'
#' Standard [broom::tidy()] and [broom::glance()] interfaces for an
#' object returned by [table_continuous_lm()]. They re-shape the
#' underlying long-format data into the two canonical broom views so
#' the table can be consumed by any downstream tidyverse-stats
#' pipeline.
#'
#' `tidy()` returns one row per **estimated parameter** across all
#' outcomes:
#' \itemize{
#'   \item One row per fitted level mean (`estimate_type = "emmean"`)
#'     for categorical predictors, with the level name in `term`.
#'   \item One row per **contrast** (`estimate_type = "difference"`)
#'     when a binary contrast is shown, with `term =
#'     "<level2> - <level1>"`.
#'   \item One row per **slope** (`estimate_type = "slope"`) for
#'     numeric predictors, with `term = predictor_label`.
#' }
#' Standard broom columns: `outcome`, `label`, `term`,
#' `estimate_type`, `estimate`, `std.error`, `conf.low`, `conf.high`,
#' `statistic`, `p.value`. The `outcome` column carries the original
#' variable name; `label` carries the human-readable label.
#'
#' `glance()` returns one row per outcome with model-level
#' statistics. Columns: `outcome`, `label`, `predictor_type`
#' (`"categorical"` or `"continuous"`), `test_type` (`"F"` for
#' categorical predictors, `"t"` for continuous ones),
#' `statistic`, `df`, `df.residual`, `p.value`, `r.squared`,
#' `adj.r.squared`, `es_type`, `es_value`, `es_ci_lower`,
#' `es_ci_upper`, `nobs`, `weighted_n`.
#'
#' @param x A `spicy_continuous_lm_table` returned by
#'   [table_continuous_lm()].
#' @param ... Currently ignored. Present for compatibility with the
#'   [broom::tidy()] / [broom::glance()] generics.
#'
#' @return A `tbl_df`.
#'
#' @seealso [as.data.frame.spicy_continuous_lm_table()] for the raw
#'   long-format access.
#'
#' @name tidy.spicy_continuous_lm_table
#' @keywords internal
NULL

#' @rdname tidy.spicy_continuous_lm_table
#' @exportS3Method broom::tidy
tidy.spicy_continuous_lm_table <- function(x, ...) {
  long <- unclass_spicy_continuous_lm_table(x)

  emmean_idx <- !is.na(long$emmean)
  emmean_df <- data.frame(
    outcome = long$variable[emmean_idx],
    label = long$label[emmean_idx],
    term = long$level[emmean_idx],
    estimate_type = rep("emmean", sum(emmean_idx)),
    estimate = long$emmean[emmean_idx],
    std.error = long$emmean_se[emmean_idx],
    conf.low = long$emmean_ci_lower[emmean_idx],
    conf.high = long$emmean_ci_upper[emmean_idx],
    statistic = rep(NA_real_, sum(emmean_idx)),
    p.value = rep(NA_real_, sum(emmean_idx)),
    stringsAsFactors = FALSE
  )

  effect_idx <- !is.na(long$estimate)
  if (any(effect_idx)) {
    types <- long$estimate_type[effect_idx]
    is_slope <- types == "slope"
    # A tidy VALUE, not a header: the " - " reads like the one in the
    # delta column key but names a contrast in a data column, so it must
    # never follow that header's template.
    term_strings <- ifelse(
      is_slope,
      long$predictor_label[effect_idx],
      paste0(
        long$level[effect_idx],
        " - ",
        long$reference[effect_idx]
      )
    )
    effect_df <- data.frame(
      outcome = long$variable[effect_idx],
      label = long$label[effect_idx],
      term = term_strings,
      estimate_type = types,
      estimate = long$estimate[effect_idx],
      std.error = long$estimate_se[effect_idx],
      conf.low = long$estimate_ci_lower[effect_idx],
      conf.high = long$estimate_ci_upper[effect_idx],
      statistic = long$statistic[effect_idx],
      p.value = long$p.value[effect_idx],
      stringsAsFactors = FALSE
    )
  } else {
    effect_df <- emmean_df[integer(0), , drop = FALSE]
  }

  result <- rbind(emmean_df, effect_df)
  if (nrow(result) > 0L) {
    result <- result[
      order(
        match(result$outcome, unique(long$variable)),
        result$estimate_type != "emmean",
        seq_len(nrow(result))
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }

  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(result))
  }
  result
}

#' @rdname tidy.spicy_continuous_lm_table
#' @exportS3Method broom::glance
glance.spicy_continuous_lm_table <- function(x, ...) {
  long <- unclass_spicy_continuous_lm_table(x)

  first_idx <- !duplicated(long$variable)
  per_outcome <- long[first_idx, , drop = FALSE]

  result <- data.frame(
    outcome = per_outcome$variable,
    label = per_outcome$label,
    predictor_type = per_outcome$predictor_type,
    test_type = per_outcome$test_type,
    statistic = per_outcome$statistic,
    df = as.integer(per_outcome$df1),
    # `df2` is the denominator df. For classical / HC* it is an
    # integer (`df.residual(fit)`); for CR* it is the Satterthwaite df,
    # which is genuinely fractional (e.g. 38.7) and may also arrive as
    # integer-but-with-FP-noise (e.g. 47.999999... very close to 48).
    # Coercing to integer truncates both genuinely-fractional values
    # and FP-noisy near-integers (47.999... -> 47), so keep it
    # numeric. Mirrors the broom convention for Satterthwaite-corrected
    # models (e.g. lmerTest::glance, afex output).
    df.residual = as.numeric(per_outcome$df2),
    p.value = per_outcome$p.value,
    r.squared = per_outcome$r2,
    adj.r.squared = per_outcome$adj_r2,
    es_type = per_outcome$es_type,
    es_value = per_outcome$es_value,
    es_ci_lower = per_outcome$es_ci_lower,
    es_ci_upper = per_outcome$es_ci_upper,
    nobs = as.integer(per_outcome$n),
    weighted_n = per_outcome$weighted_n,
    stringsAsFactors = FALSE
  )
  rownames(result) <- NULL

  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(result))
  }
  result
}
