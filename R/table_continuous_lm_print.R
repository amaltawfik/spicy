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
  by_label <- attr(x, "group_label") %||%
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
    spec = spec,
    digits_by_var = attr(x, "digits_by_var")
  )

  # The header a reader sees is the registry label, never the frozen
  # column key. No shape guard is needed, unlike the categorical
  # console: `.lm_labels()` maps `names(display_df)` to a vector of the
  # same length by construction, so `spicy_print_table()`'s abort on a
  # mismatched label vector is unreachable from here.
  labels <- .lm_spec_labels(spec)
  header_labels <- .lm_labels(names(display_df), labels)
  # The marginal means are headed by their levels under the mean's
  # spanner, as in every engine (none when the glyph is emptied through
  # `options(spicy.labels)`). The interval keeps its flat headers
  # ("95% CI LL"), as in the console of every descriptive table.
  mean_label <- .lm_labels(.LM_KEY_MEAN, labels)
  mean_j <- which(names(display_df) %in% .lm_spec_mean_keys(spec))
  spanners <- if (length(mean_j) > 0L && nzchar(mean_label)) {
    stats::setNames(list(mean_j), mean_label)
  }

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
    # "right": all numeric columns right-aligned, so nothing is centered.
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

  spicy_print_table(
    display_df,
    # The title stored when the table was built, as the note is.
    title = attr(x, "title") %||% .continuous_lm_title(by_label),
    # APA-style footer (covariate adjustment, non-classical SE estimator,
    # missing data), built once by `.tclm_note_text()` in
    # table_continuous_lm(), so the console and every rich exporter carry
    # the same note.
    note = attr(x, "note"),
    padding = padding,
    first_column_line = TRUE,
    row_total_line = FALSE,
    bottom_line = FALSE,
    align_left_cols = align_left,
    align_center_cols = align_center,
    spanners = spanners,
    group_sep_rows = integer(0),
    # No total row: without an index the printer looks for the word
    # "Total" in the rows and rules off an outcome labeled "Total score".
    total_row_idx = integer(0),
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

# Internal: build the table note from the attributes of the compute
# frame, once, in `table_continuous_lm()`: the console print method and
# every rich exporter read the result, the `note` attribute. One
# paragraph (`.compose_note()`), its sentences in the order of the
# family:
#
#   (a) what the numbers are: the weights (`weighted_by`, the name the
#       sentence gives them), then the covariates (`covariates`, as the
#       note names them: their labels) and the adjustment estimand,
#       named because the displayed `emmean` changes with it:
#       "proportional" = G-computation over the observed covariate
#       distribution; "balanced" = synthetic-grid equal-weight marginal
#       means;
#   (b) the missing data, `missing_note`: the sentences assembled by
#       table_continuous_lm() -- values and rows removed, the levels
#       with no data;
#   (c) the inference: the test behind the p-values when the table
#       shows them (`.tclm_test_note()`), then the SE estimator of a
#       non-classical `vcov` -- robust and resampling standard errors
#       are never silently labeled, the doctrine of table_regression()'s
#       footers;
#   (d) the definitions: the order of the subtraction the contrast
#       column reports (`.tclm_delta_note()`), which its header no longer
#       states;
#   (e) the abbreviations of the headers, read off `spec`, the column
#       spec of the table (`.lm_column_spec()`), where the rule of the
#       style in force puts them; the glyph of the difference, which (d)
#       defines, is defined there under no rule.
#
# NULL when there is nothing to disclose; otherwise one string opened by
# the lead-in in force ("Note. " by default, see `.note_lead_keys()`).
.tclm_note_text <- function(
  x,
  missing_note = NULL,
  covariates = attr(x, "covariates") %||% character(),
  weighted_by = NULL,
  spec = NULL
) {
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

  what <- character()
  if (!is.null(weighted_by)) {
    what <- spicy_fmt("note_weighted_by", weighted_by)
  }
  if (length(covariates) > 0L && !is.na(adjustment)) {
    # The `adjustment` TOKEN stays an API identifier; only its display form
    # comes from the registry.
    adjustment_label <- switch(
      adjustment,
      proportional = spicy_str("note_adjustment_proportional"),
      balanced = spicy_str("note_adjustment_balanced"),
      adjustment
    )
    what <- c(
      what,
      spicy_fmt(
        "note_adjusted_for",
        .join_note_vars(covariates),
        adjustment_label
      )
    )
  }
  inference <- if (isTRUE(attr(x, "show_p_value"))) .tclm_test_note(x)
  if (!identical(vcov_type, "classical")) {
    inference <- c(
      inference,
      spicy_fmt(
        "note_std_errors_single",
        .tclm_vcov_label(vcov_type, cluster_name, boot_valid)
      )
    )
  }
  # The order of the difference defines its glyph under every rule, so the
  # abbreviations do not define it again ("Delta = difference").
  delta_note <- .tclm_delta_note(spec)
  .compose_note(
    what = what,
    missing = missing_note,
    inference = inference,
    definitions = delta_note,
    abbreviations = .note_abbreviations(
      .lm_abbrev_items(spec, partial = length(covariates) > 0L),
      defined = if (!is.null(delta_note)) "delta"
    )
  )
}

# The order of the subtraction of the contrast column, headed by the
# increment glyph alone: "Delta = Male - Female.", with the minus sign
# (`note_lm_delta`). The levels are those of the header of each mean, in
# the order the spec recorded them; NULL when the table shows no
# contrast (more than two levels, `contrast = "none"`, a numeric `by`).
.tclm_delta_note <- function(spec) {
  for (col in spec) {
    if (identical(col$token, "delta")) {
      return(spicy_fmt(
        "note_lm_delta",
        spicy_str("symbol_delta"),
        col$contrast[[1L]],
        col$contrast[[2L]]
      ))
    }
  }
  NULL
}

# The concepts the headers of a linear-model table show, in the order of
# its columns (`spec`, from `.lm_column_spec()`), for
# `.note_abbreviations()`. Under covariate adjustment the f2 and the
# omega-squared of `by` are partial (see `effect_size`), which their
# definitions say, as the partial effect sizes of table_regression() do.
.lm_abbrev_items <- function(spec, partial = FALSE) {
  es_ci <- any(vapply(
    spec,
    function(s) identical(s$token, "es_ci"),
    logical(1)
  ))
  items <- list()
  for (col in spec) {
    items <- c(
      items,
      switch(
        col$token,
        emmean = list("mean"),
        delta = list("delta"),
        b = list("b"),
        ci = list(
          "ci",
          if (identical(col$ci_role, .LM_KEY_CI_LL)) "ll" else "ul"
        ),
        r2 = list(
          if (identical(col$key, format_r2_header_lm("adj_r2"))) {
            "adj_r2"
          } else {
            "r2"
          }
        ),
        es = c(
          list(.lm_abbrev_effect_size(col$effect_size, partial)),
          if (es_ci) list("ci")
        ),
        # variable, the test statistic (t, z, F, chi-squared), p, n,
        # weighted_n and the raw-only interval of the effect size: nothing
        # to define.
        NULL
      )
    )
  }
  items[!vapply(items, is.null, logical(1))]
}

# The concept of the effect-size header.
.lm_abbrev_effect_size <- function(effect_size, partial = FALSE) {
  switch(
    effect_size,
    f2 = if (partial) {
      .abbr("f2", term = spicy_str("abbrev_f2_partial"))
    } else {
      "f2"
    },
    omega2 = if (partial) {
      .abbr("omega2", term = spicy_str("abbrev_omega2_partial"))
    } else {
      "omega2"
    },
    cohens_d = "d",
    hedges_g = "g",
    NULL
  )
}

# Internal: the test behind the p-value of each outcome, read off its
# linear model -- the `test_type` of the row the table reads the p-value
# from (`get_test_row_index_lm()`): the t (or z) of the difference of a
# two-group `by` or of the slope of a numeric one, the global F (or Wald
# chi-squared) of a `by` with more groups. A categorical `by` names it in
# `note_group_comparison`, a numeric one in `note_slope_test`. One test
# for every outcome is named once; otherwise each test is followed by the
# outcomes it covers, as the rows name them, in the order of the test
# vocabulary. An outcome whose model did not run carries no test and is
# not named; NULL when none ran.
.tclm_test_note <- function(x) {
  vars <- unique(x$variable)
  types <- vapply(
    vars,
    function(v) {
      block <- x[x$variable == v, , drop = FALSE]
      block$test_type[get_test_row_index_lm(block)]
    },
    character(1),
    USE.NAMES = FALSE
  )
  keys <- c(
    t = "test_lm_t",
    z = "test_lm_z",
    F = "test_lm_f",
    chi2 = "test_lm_chi2"
  )
  ran <- !is.na(types) & types %in% names(keys)
  if (!any(ran)) {
    return(NULL)
  }
  template <- if (identical(x$predictor_type[[1L]], "continuous")) {
    "note_slope_test"
  } else {
    "note_group_comparison"
  }
  used <- intersect(names(keys), types[ran])
  if (length(used) == 1L) {
    return(spicy_fmt(template, spicy_str(keys[[used]])))
  }
  labels <- x$label[match(vars, x$variable)]
  items <- vapply(
    used,
    function(k) {
      spicy_fmt(
        "note_group_comparison_item",
        spicy_str(keys[[k]]),
        .join_note_vars(labels[ran & types == k])
      )
    },
    character(1)
  )
  spicy_fmt(
    template,
    paste(items, collapse = spicy_str("note_group_comparison_sep"))
  )
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
# object returned by table_continuous_lm(), keeping the compute frame
# under its internal names. Read by tidy() and glance().
unclass_spicy_continuous_lm_table <- function(x) {
  attr_names <- names(attributes(x))
  drop <- setdiff(attr_names, c("names", "row.names", "class"))
  for (nm in drop) {
    attr(x, nm) <- NULL
  }
  class(x) <- "data.frame"
  x
}

# The `output = "long"` frame, in the vocabulary of the family (decision
# 1 of the ADR of 2026-10-20): the compute frame with broom's names for
# the test's degrees of freedom and the fit, and `nobs` / `weighted_nobs`
# for the counts of the MODEL, which every row of an outcome repeats (the
# `n` of the descriptive tables is a group's). Only the note stays as an
# attribute.
.continuous_lm_publish_long <- function(x) {
  out <- .rename_cols(
    unclass_spicy_continuous_lm_table(x),
    c(
      df1 = "df",
      df2 = "df.residual",
      r2 = "r.squared",
      adj_r2 = "adj.r.squared",
      n = "nobs",
      weighted_n = "weighted_nobs"
    )
  )
  dfs <- .lm_df_cols(out$test_type, out$df, out$df.residual)
  out$df <- dfs$df
  out$df.residual <- dfs$df.residual
  out <- .tclm_hide_test(out, x)
  attr(out, "note") <- attr(x, "note", exact = TRUE)
  out
}

# `p_value = FALSE`, the rule of the family (decision of 2026-10-17): the
# machine outputs carry no p-value the table does not report, and none of
# the test (its type, statistic and degrees of freedom) unless the table
# shows the statistic. The model is fit all the same, for the means and
# the contrast, so the columns stay, NA: one shape for the long output,
# tidy() and glance(), whatever the table shows. `x` is the table, whose
# attributes say what it shows; `df` a frame under the published names.
.tclm_hide_test <- function(df, x) {
  if (!identical(attr(x, "show_p_value", exact = TRUE), FALSE)) {
    return(df)
  }
  hide <- "p.value"
  if (!isTRUE(attr(x, "show_statistic", exact = TRUE))) {
    hide <- c(hide, "test_type", "statistic", "df", "df.residual")
  }
  for (nm in intersect(hide, names(df))) {
    df[[nm]][] <- NA
  }
  df
}

# The degrees of freedom follow the statistic, in the long output and in
# glance() (decision of 2026-10-20): a t carries its own in `df` -- the
# residual df of the model, or the Satterthwaite df of a cluster-robust
# contrast -- and leaves `df.residual` empty, as the Welch t of
# table_continuous() does; so does its asymptotic twin z (`df = Inf`, as
# in table_regression()), and a chi-squared carries its one df. An F
# keeps its numerator and denominator. The compute frame keeps the F
# convention (`df1` = 1 beside a t), which the printed `t(df)` header
# reads, so the display does not move.
.lm_df_cols <- function(test_type, df1, df2) {
  one_df <- !is.na(test_type) & test_type %in% c("t", "z")
  no_denominator <- one_df | (!is.na(test_type) & test_type == "chi2")
  list(
    df = ifelse(one_df, as.numeric(df2), as.numeric(df1)),
    df.residual = ifelse(no_denominator, NA_real_, as.numeric(df2))
  )
}

# The displayed table as a data frame (decision 5 of the ADR of
# 2026-10-20): `as.data.frame()` and `output = "data.frame"` both call
# it, on the object the table prints. The typed view holds one row per
# outcome; an effect size printed with its interval keeps the bounds
# beside it, under their names in the long output.
.continuous_lm_data_frame <- function(x) {
  st <- as_structured(x)
  first <- match(st$body[[".variable"]], x$variable)
  parts <- list()
  es_col <- names(st$col_meta)[vapply(
    st$col_meta,
    function(m) identical(m$token, "es"),
    logical(1)
  )]
  if (length(es_col) && isTRUE(attr(x, "show_effect_size_ci", exact = TRUE))) {
    parts[[es_col[[1L]]]] <- list(
      es_ci_lower = x$es_ci_lower[first],
      es_ci_upper = x$es_ci_upper[first]
    )
  }
  .table_data_frame(
    st,
    stats::setNames(x$label, x$variable),
    key = NULL,
    row_role = FALSE,
    parts = parts,
    note = attr(x, "note", exact = TRUE)
  )
}

#' Coerce a `spicy_continuous_lm_table` to a plain data frame or tibble
#'
#' `as.data.frame()` returns the table [table_continuous_lm()] displays,
#' with its numbers: exactly what `output = "data.frame"` returns. One
#' row per outcome, the key columns `variable` (the column name of the
#' outcome) and `label` (the label displayed for it), then the printed
#' columns under their names (`"M (<level>)"`, the contrast, the
#' interval bounds, the test statistic, `"p"`, the fit statistic, the
#' effect size, `"n"`, `"Weighted n"`); an effect size printed with its
#' interval keeps the bounds beside it as `es_ci_lower` and
#' `es_ci_upper`. The note, as printed, is kept in `attr(, "note")`.
#' `as_tibble()` returns the same table as a tibble. The original `x` is
#' unaffected, and `print(x)` continues to render the formatted ASCII
#' table.
#'
#' For one row per level of `by` with every estimate, use
#' `output = "long"`, [tidy.spicy_continuous_lm_table()] or
#' [glance.spicy_continuous_lm_table()].
#'
#' @param x A `spicy_continuous_lm_table` returned by
#'   [table_continuous_lm()].
#' @param row.names,optional Standard [base::as.data.frame()]
#'   arguments. Currently ignored.
#' @param ... Further arguments passed to [tibble::as_tibble()] (for
#'   the tibble method) or ignored (for the `as.data.frame()` method).
#'
#' @return A plain `data.frame` (or `tbl_df`), identical to the
#'   `output = "data.frame"` of the same call.
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
  .continuous_lm_data_frame(x)
}

#' @rdname as.data.frame.spicy_continuous_lm_table
#' @exportS3Method tibble::as_tibble
as_tibble.spicy_continuous_lm_table <- function(x, ...) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    spicy_abort("Install package 'tibble'.", class = "spicy_missing_pkg")
  }
  tibble::as_tibble(.continuous_lm_data_frame(x), ...)
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
#'     for categorical predictors, with the level name in `term`, `NA`
#'     where no mean is estimated (a level in which the outcome has no
#'     data, an outcome whose model could not be fit).
#'   \item One row per **contrast** (`estimate_type = "difference"`)
#'     when a binary contrast is shown, with `term =
#'     "<level2> - <level1>"`.
#'   \item One row per **slope** (`estimate_type = "slope"`) for
#'     numeric predictors, with `term = predictor_label`.
#' }
#' Standard broom columns: `variable`, `label`, `term`,
#' `estimate_type`, `estimate`, `std.error`, `conf.low`, `conf.high`,
#' `statistic`, `p.value`. The `variable` column carries the column
#' name of the outcome; `label` carries the label the rows display for
#' it (see `labels`).
#'
#' `glance()` returns one row per outcome with model-level
#' statistics. Columns: `variable`, `label`, `predictor_type`
#' (`"categorical"` or `"continuous"`), `test_type` (the test the table
#' displays: `"t"` for a numeric `by` and for the contrast of a
#' two-level `by`, `"F"` for a `by` of three or more levels or under
#' `contrast = "none"`, and `"z"` or `"chi2"` in their place under
#' `vcov = "bootstrap"` or `"jackknife"`),
#' `statistic`, `df`, `df.residual` (the degrees of freedom follow the
#' statistic: an *F* has both, a *t* has its own in `df` and none in
#' `df.residual`), `p.value`, `r.squared`,
#' `adj.r.squared`, `es_type`, `es_value`, `es_ci_lower`,
#' `es_ci_upper`, `nobs`, `weighted_nobs` (the sum of the weights, `NA`
#' without `weights`). Beside the contrast of a two-level `by`, the *F*
#' of the model, the square of its *t*, is in the long output only.
#'
#' @param x A `spicy_continuous_lm_table` returned by
#'   [table_continuous_lm()].
#' @param ... Currently ignored. Present for compatibility with the
#'   [broom::tidy()] / [broom::glance()] generics.
#'
#' @return A `tbl_df`.
#'
#' @seealso [as.data.frame.spicy_continuous_lm_table()] for the displayed
#'   table as a data frame, and `output = "long"` of
#'   [table_continuous_lm()] for every estimate in long format.
#'
#' @name tidy.spicy_continuous_lm_table
#' @keywords internal
NULL

#' @rdname tidy.spicy_continuous_lm_table
#' @exportS3Method broom::tidy
tidy.spicy_continuous_lm_table <- function(x, ...) {
  long <- unclass_spicy_continuous_lm_table(x)

  # One row per level of a categorical `by`, as the long output has them:
  # NA where no mean is estimated, a level in which the outcome has no
  # data or an outcome whose model could not be fit. A numeric `by` has
  # no level rows.
  emmean_idx <- !is.na(long$level)
  emmean_df <- data.frame(
    variable = long$variable[emmean_idx],
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
    # A tidy VALUE, not a header: a contrast named in a data column keeps
    # the hyphen code filters on, and never follows the note sentence
    # that states the order of the subtraction (`note_lm_delta`).
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
      variable = long$variable[effect_idx],
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
        match(result$variable, unique(long$variable)),
        result$estimate_type != "emmean",
        seq_len(nrow(result))
      ),
      ,
      drop = FALSE
    ]
    rownames(result) <- NULL
  }
  result <- .tclm_hide_test(result, x)

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
  # The test of each outcome is the one the table displays, from the row
  # the wide table reads (`get_test_row_index_lm()`): with a two-level
  # `by`, the t (or z) of the contrast, not the F (or chi-squared) of the
  # model, which is its square. The fit and the size stay on the first row.
  test_rows <- vapply(
    per_outcome$variable,
    function(v) {
      rows <- which(long$variable == v)
      rows[get_test_row_index_lm(long[rows, , drop = FALSE])]
    },
    integer(1),
    USE.NAMES = FALSE
  )
  tests <- long[test_rows, , drop = FALSE]
  # The degrees of freedom follow the statistic (`.lm_df_cols()`), and
  # stay numeric: for CR* the df of a t, or the denominator df of an F,
  # is the Satterthwaite df, genuinely fractional (e.g. 38.7) or an
  # integer with FP noise (47.999... would truncate to 47). Mirrors the
  # broom convention for Satterthwaite-corrected models (e.g.
  # lmerTest::glance, afex output).
  dfs <- .lm_df_cols(tests$test_type, tests$df1, tests$df2)

  result <- data.frame(
    variable = per_outcome$variable,
    label = per_outcome$label,
    predictor_type = per_outcome$predictor_type,
    test_type = tests$test_type,
    statistic = tests$statistic,
    df = dfs$df,
    df.residual = dfs$df.residual,
    p.value = tests$p.value,
    r.squared = per_outcome$r2,
    adj.r.squared = per_outcome$adj_r2,
    es_type = per_outcome$es_type,
    es_value = per_outcome$es_value,
    es_ci_lower = per_outcome$es_ci_lower,
    es_ci_upper = per_outcome$es_ci_upper,
    nobs = as.integer(per_outcome$n),
    weighted_nobs = per_outcome$weighted_n,
    stringsAsFactors = FALSE
  )
  .as_broom_tbl(.tclm_hide_test(result, x))
}
