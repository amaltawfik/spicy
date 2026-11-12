# table_battery_svy(): the design twin of table_battery().
#
# The same table -- one row per item, one column per level of the scale,
# row percentages, the summary column of `top`, the base `n`, a test per
# item under `by` -- with every estimate delegated to survey:
#
#   %   survey::svymean() of the item on its domain, the design cut by
#       `[` so that it keeps its strata, clusters and fpc: within the
#       group, the rows of non-zero weight whose answer is a level shown
#       (and the missing answers too under `drop_na = FALSE`).
#   n   the OBSERVED count of that domain, as the `n` of
#       `table_categorical_svy()`: the respondents, not the population.
#   p   survey::svychisq() of the item by the group, on the answers on
#       the levels shown (`.cat_svy_test()`), adjusted for the family of
#       the items.
#
# Decision record: dev/decisions/2026-11-11-table-battery-svy.md. The
# scale, `levels`, `top`, `sort`, the columns and every output are those
# of `table_battery()`, from its helpers (`.bat_render()`).

#' Item battery table from a survey design
#'
#' @description
#' The design twin of [table_battery()]: a battery of items answered on
#' one common scale, one row per item and one column per level, with row
#' percentages estimated from a `survey::svydesign()` or
#' `survey::as.svrepdesign()` object instead of a data frame.
#'
#' Every statistic is survey's. `survey::svymean()` estimates the
#' percentages of each item, and, with `by`, `survey::svychisq()` tests
#' each item against the group, Rao-Scott corrected by default, the
#' p-values adjusted for the family of the items by Holm's method.
#'
#' The rows, the columns, `levels`, `top`, `sort` and the outputs are
#' those of [table_battery()].
#'
#' @param design A survey design: `survey::svydesign()` or
#'   `survey::as.svrepdesign()`.
#' @param select The items of the battery, one column of the design per
#'   item, in tidyselect syntax or as a character vector of names. Their
#'   order is the order of the rows, unless `sort` sorts them. The `by`
#'   column and the design's own columns (its weights, replicate weights,
#'   clusters and finite population corrections) are not part of the
#'   selection: a selection helper skips them, and naming one is an
#'   error. The items share one scale, read as in [table_battery()].
#' @param drop_na Logical. If `TRUE` (the default), the missing answers to
#'   an item leave its domain: its percentages are estimated on its valid
#'   answers, and its `n` counts them. If `FALSE`, a last column of
#'   missing answers (`"(Missing)"`) follows the levels, and the domain of
#'   each item takes every row of non-zero weight, its missing answers
#'   included.
#' @param show_n Logical. If `TRUE` (the default), each row (each group
#'   block under `by`) ends with `n`, the observed (unweighted) count of
#'   the respondents its percentages are estimated on.
#' @param p_value Logical or `NULL`. With `by`, `NULL` (the default) and
#'   `TRUE` show the `p` column of the item tests; `FALSE` removes it from
#'   every output. Ignored without `by`, with a warning when `TRUE`.
#' @param levels The scale: the levels to show, in the order of the
#'   columns, as in [table_battery()]. `NULL` (the default) reads the
#'   scale from the items, under the rules of the section *Items and
#'   levels* of [table_battery()]. Given, `levels` is the scale: a level
#'   that no item holds shows at 0, and any other answer is left out of
#'   the domain, and the note names it.
#' @param p_adjust Multiple-comparison adjustment of the `p` column,
#'   applied to the family of the items tested: `"holm"` (the default),
#'   `"none"`, or another method of [stats::p.adjust()] (`"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`, `"BY"`). See the
#'   section *Multiple-comparison adjustment* of [table_battery()].
#'   Typing it when the table has no test to adjust (no `by`, or
#'   `p_value = FALSE`) is an error.
#' @param note The table note. `NULL` (the default) builds it, opening
#'   with "Note.": the design, the direction of the percentages, the
#'   base, the levels left out, the rows removed and the items without
#'   data, then the tests and the sentences of `note_add`. A single string
#'   replaces it, and `FALSE` removes it, as in [table_battery()].
#' @inheritParams table_battery
#' @inheritParams table_categorical_svy
#'
#' @details
#' # Estimation
#'
#' The percentage of a level is `survey::svymean()` of the item on its
#' domain: within the group under `by`, the rows of non-zero weight whose
#' answer is a level shown. The domain is cut from the design with `[`,
#' which keeps its strata, clusters and finite population corrections.
#' Under `drop_na = FALSE`, the missing answers are a level of their own
#' and stay in the domain. An answer that `levels` leaves out is outside
#' the domain either way. An item that offers one level is 100% by
#' arithmetic: survey does not estimate a variable with one level.
#'
#' A cell that survey cannot estimate, most often because a stratum holds
#' a single primary sampling unit under its default
#' `options(survey.lonely.psu = "fail")`, shows a dash: the function
#' warns once, quoting survey's messages (class
#' `spicy_svy_estimation_failed`), and the note says what the dash means.
#' A warning of survey's own, such as the one on a stratum with a single
#' primary sampling unit in a domain under
#' `options(survey.adjust.domain.lonely = TRUE)`, is said once per table,
#' with the number of times survey raised it.
#'
#' `n` is the observed count of the domain, the respondents and not the
#' estimated population, as in [table_categorical_svy()]. A row with a
#' missing `by` leaves the table, and the note gives their count.
#'
#' # Tests
#'
#' With `by`, each item is tested against the group by
#' `survey::svychisq()` with `chisq_statistic`, on its answers on the
#' levels shown, the missing answers left out whatever `drop_na`; the
#' levels and the groups without an answer leave the test, as in
#' [table_battery()]. The default `"F"` is the Pearson chi-squared with
#' the Rao-Scott second-order correction, referred to the design degrees
#' of freedom. The p-values are adjusted for the family of the items, by
#' Holm's method by default (`p_adjust`). An item left with one level is
#' not tested: its `p` cell shows a dash, and the note names it.
#'
#' A design whose weights go negative, as linear calibration can make
#' them, keeps its percentages, and its tests are withheld, with a
#' warning (class `spicy_negative_weights_no_test`), as in
#' [table_categorical_svy()].
#'
#' # What is absent
#'
#' No interval or design effect per cell: a battery of five levels in
#' three groups would carry thirty of each, and
#' [table_categorical_svy()] gives them item by item. No column
#' percentages, and no mean of the scale.
#'
#' @return Depends on `output`, as in [table_battery()]:
#' \itemize{
#'   \item `"default"`: a `spicy_battery_svy_table` (a `data.frame`
#'     subclass), printed when the call is bare.
#'   \item `"data.frame"`: the displayed table with its numbers, the
#'     shape of [table_battery()]: `variable`, `label`, one percentage per
#'     level shown, `(Missing)` under `drop_na = FALSE`, the summary
#'     column of `top`, `n`, each prefixed with the group under `by`, and
#'     `p`.
#'   \item `"long"`: one row per item, group and level, with `variable`,
#'     `label`, `group` (with `by`), `level`, `.row_role`, `n` (the
#'     observed count) and `pct` (the estimated percentage), and, when
#'     the tests ran, `test_type` (the statistic of `survey::svychisq()`),
#'     `p.value` and `p_unadjusted` under an adjustment.
#'   \item `"tinytable"`, `"gt"`, `"flextable"`: the rendered table object.
#'   \item `"excel"` / `"word"`: writes to disk and returns the file path
#'     invisibly.
#'   \item `"clipboard"`: copies the table and returns the display
#'     `data.frame` invisibly.
#' }
#'
#' [as_structured()] and [inline()] read the default object as they read
#' that of [table_battery()]: `inline(x, item, "Yes", "pct")`, with
#' `model = "<group>"` under `by`.
#'
#' @seealso [table_battery()] for the data-frame sibling,
#'   [table_categorical_svy()] for one block of rows per variable.
#' @export
#'
#' @examplesIf requireNamespace("survey", quietly = TRUE) && utils::packageVersion("survey") >= "4.5"
#' data(api, package = "survey")
#' dstrat <- survey::svydesign(
#'   id = ~1, strata = ~stype, weights = ~pw, data = apistrat, fpc = ~fpc
#' )
#' # Four yes-no items of the schools, one row each.
#' table_battery_svy(dstrat, select = c(awards, sch.wide, comp.imp, both))
#'
#' # By school type: the estimated percentages of each type and a
#' # design-based test per item.
#' table_battery_svy(
#'   dstrat,
#'   select = c(awards, sch.wide, comp.imp, both),
#'   by = stype
#' )
#'
#' # Long data frame.
#' head(table_battery_svy(
#'   dstrat,
#'   select = c(awards, sch.wide),
#'   by = stype,
#'   output = "long"
#' ))
table_battery_svy <- function(
  design,
  select,
  by = NULL,
  levels = NULL,
  top = NULL,
  sort = "",
  drop_na = TRUE,
  show_n = TRUE,
  chisq_statistic = c("F", "Chisq", "Wald", "adjWald", "saddlepoint"),
  p_value = NULL,
  p_adjust = "holm",
  labels = getOption("spicy.var_labels", FALSE),
  note_labels = NULL,
  percent_digits = 1,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
  title = NULL,
  note = NULL,
  note_add = NULL,
  output = c(
    "default",
    "data.frame",
    "long",
    "tinytable",
    "gt",
    "flextable",
    "excel",
    "clipboard",
    "word"
  ),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  style = NULL
) {
  .style_pushed <- .style_begin(style, match.call(), environment())
  on.exit(.style_end(.style_pushed), add = TRUE)

  .require_survey("table_battery_svy")
  # The design gates first: a data frame handed to a `_svy` function is the
  # first thing wrong with the call.
  cfg <- .svy_validate_common(
    design = design,
    fn = "table_battery_svy",
    plain_fn = "table_battery",
    ci_level = 0.95,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    labels = labels,
    deff = FALSE,
    df = NULL,
    excel_sheet = excel_sheet,
    excel_key = "excel_sheet_battery"
  )
  if (missing(select)) {
    spicy_abort(
      c(
        "`select` is required: it names the items of the battery.",
        "i" = "For example `select = c(q1_a, q1_b, q1_c)` or `select = starts_with(\"q1_\")`."
      ),
      class = "spicy_invalid_input"
    )
  }
  chisq_explicit <- !missing(chisq_statistic)
  if (
    is.character(chisq_statistic) &&
      length(chisq_statistic) == 1L &&
      chisq_statistic %in% .CAT_SVY_CHISQ_REFUSED
  ) {
    .abort_cat_svy_statistic(chisq_statistic)
  }
  chisq_statistic <- spicy_match_arg(chisq_statistic)
  .check_cat_svy_statistic_design(chisq_statistic, design)
  .check_note_labels_arg(note_labels)
  align <- spicy_match_arg(align)
  output <- spicy_match_arg(output)
  excel_sheet <- cfg$excel_sheet
  p_adjust_explicit <- !missing(p_adjust)
  .bat_check_scalars(
    drop_na = drop_na,
    show_n = show_n,
    p_value = p_value,
    percent_digits = percent_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    title = title,
    sort = sort
  )
  levels_arg <- .bat_check_level_arg(levels, "levels")
  top_arg <- .bat_check_level_arg(top, "top")
  .check_note_args(note, note_add)
  .warn_ignored_output_args(output, names(match.call()))
  percent_digits <- as.integer(percent_digits)
  p_digits <- as.integer(p_digits)

  # ---- columns -------------------------------------------------------------
  vars <- design$variables
  by_quo <- rlang::enquo(by)
  has_group <- !rlang::quo_is_null(by_quo)
  by_name <- if (has_group) {
    resolve_single_column_selection(by_quo, vars, "by", where = "`design`")
  }
  select_names <- .resolve_selection_without(
    rlang::enquo(select),
    vars,
    roles = c(by = by_name),
    where = "`design`",
    design_columns = .design_columns(design),
    plain_fn = "table_battery"
  )
  if (!length(select_names)) {
    .abort_empty_selection(
      "select",
      hint = "Name the items of the battery, one column per item."
    )
  }
  .check_integer64_columns(vars, c(select_names, by_name), "table_battery_svy")

  # ---- the test switches -----------------------------------------------------
  if (p_adjust_explicit) {
    .check_desc_p_adjust(p_adjust, has_group, p_value)
  } else {
    validate_p_adjust(p_adjust)
  }
  p_value_explicit <- !is.null(p_value)
  show_p <- if (p_value_explicit) p_value else has_group
  if (show_p && !has_group) {
    spicy_warn(
      "`p_value` is ignored when `by` is not used.",
      class = "spicy_ignored_arg"
    )
    show_p <- FALSE
  }

  # ---- labels ----------------------------------------------------------------
  .warn_unknown_label_names(
    c(select_names, if (has_group) by_name),
    labels,
    note_labels
  )
  item_labels <- resolve_variable_labels(vars, select_names, labels)
  by_label <- if (has_group) resolve_variable_labels(vars, by_name, labels)
  item_notes <- .note_labels_of(select_names, item_labels, note_labels)
  by_note <- if (has_group) .note_labels_of(by_name, by_label, note_labels)
  .check_note_label_collisions(
    c(select_names, by_name),
    c(item_labels, by_label),
    c(item_notes, by_note),
    note_labels
  )

  # ---- the items and their levels --------------------------------------------
  # The rows of the sample: a row at weight zero is one `[` kept on a
  # calibrated design, not a respondent, so it gives the scale no level.
  w <- .design_weights(design)
  in_sample <- !is.na(w) & w != 0
  n_rows <- nrow(vars)
  items <- .bat_read_items(
    if (all(in_sample)) vars else vars[in_sample, , drop = FALSE],
    select_names,
    levels_arg
  )
  answers <- lapply(items$answers, function(a) {
    out <- rep(NA_character_, n_rows)
    out[in_sample] <- a
    out
  })
  shown <- items$levels
  left_out <- items$left_out
  if (!is.null(top_arg)) {
    .bat_check_top(top_arg, shown, has_group)
  }
  missing_label <- spicy_str("row_missing_level")
  sort_spec <- .bat_sort_spec(sort, shown, !is.null(top_arg))

  # ---- the groups -------------------------------------------------------------
  n_by_na <- 0L
  if (has_group) {
    g0 <- .group_display(.user_na_to_na(vars[[by_name]]))
    .check_grouping_observed(
      g0[in_sample],
      sprintf("`by` (`%s`)", by_name),
      drop_na_hint = FALSE
    )
    blocks <- .group_levels(g0[in_sample])
    gi <- match(as.character(g0), blocks)
    gi[!in_sample] <- NA_integer_
    n_by_na <- sum(in_sample & is.na(gi))
    design$variables[[by_name]] <- factor(blocks[gi], levels = blocks)
  } else {
    gi <- ifelse(in_sample, 1L, NA_integer_)
    blocks <- NA_character_
  }
  nb <- length(blocks)
  n_items <- length(select_names)
  n_levels <- length(shown)
  block_rows <- vapply(seq_len(nb), function(b) sum(gi %in% b), numeric(1))

  no_test_reason <- if (!has_group) {
    "by"
  } else if (!show_p) {
    "p_value"
  } else {
    NA_character_
  }
  test_runs <- has_group && show_p
  if (test_runs && sum(block_rows > 0) < 2L) {
    if (p_value_explicit) {
      spicy_warn(
        "`p_value` is ignored: fewer than two groups of `by` have respondents, so there is nothing to compare.",
        class = "spicy_ignored_arg"
      )
    }
    show_p <- FALSE
    test_runs <- FALSE
    no_test_reason <- "groups"
  }
  if (!is.na(no_test_reason)) {
    # The reasons are those of `.mr_warn_ignored()`, which names `test`.
    .warn_ignored(
      c("chisq_statistic", "p_adjust")[c(
        chisq_explicit,
        p_adjust_explicit && identical(no_test_reason, "groups")
      )],
      switch(
        no_test_reason,
        by = "when `by` is not used",
        p_value = "when `p_value = FALSE`",
        groups = "when fewer than two groups of `by` have respondents"
      )
    )
  }

  # A level displayed like a column the table adds is refused before any
  # estimate, as `.bat_render()` refuses it.
  .bat_check_column_keys(.bat_columns(
    blocks,
    shown,
    top_arg,
    has_group,
    drop_na,
    show_n,
    show_p,
    missing_label
  ))

  # ---- the estimates, item by item, block by block ----------------------------
  # The design variable of an item is a factor of the levels shown (and of
  # the missing answers under `drop_na = FALSE`), NA outside its domain.
  est_levels <- c(shown, if (!drop_na) missing_label)
  counts <- array(0, dim = c(nb, n_items, n_levels))
  pct <- array(NA_real_, dim = c(nb, n_items, n_levels))
  miss <- matrix(0, nb, n_items)
  pct_miss <- matrix(NA_real_, nb, n_items)
  # The percentages of all the groups together, which `sort` reads.
  pool <- has_group && sort_spec$on %in% c("level", "top")
  pooled <- array(NA_real_, dim = c(1L, n_items, n_levels))
  p_raw <- rep(NA_real_, n_items)
  testable <- rep(FALSE, n_items)
  tested <- rep(FALSE, n_items)
  n_attempted <- 0L
  n_refused <- 0L
  # survey's errors, each raised once after the table is built, and
  # survey's own warnings, each said once.
  svy_warns <- .svy_warning_log()
  svy_log <- .svy_failure_log()
  for (j in seq_len(n_items)) {
    nm <- select_names[[j]]
    a <- answers[[j]]
    vals <- ifelse(a %in% shown, a, NA_character_)
    if (!drop_na) {
      vals[in_sample & is.na(a)] <- missing_label
    }
    design$variables[[nm]] <- factor(vals, levels = est_levels)
    estimate <- function(keep) {
      if (!any(keep)) {
        return(NULL)
      }
      st <- withCallingHandlers(
        .cat_svy_level_stats(
          .design_subset(design, keep),
          nm,
          est_levels,
          ci_level = 0.95,
          df = NA_real_,
          deff_mode = FALSE,
          ci_method = "logit",
          proportion_ci = FALSE
        ),
        spicy_svy_estimation_failed = svy_log$record,
        warning = svy_warns$record
      )
      list(n = st$n, pct = 100 * st$pct)
    }
    for (b in seq_len(nb)) {
      st <- estimate(gi %in% b & !is.na(vals))
      if (is.null(st)) {
        next
      }
      counts[b, j, ] <- st$n[seq_len(n_levels)]
      pct[b, j, ] <- st$pct[seq_len(n_levels)]
      if (!drop_na) {
        miss[b, j] <- st$n[[n_levels + 1L]]
        pct_miss[b, j] <- st$pct[[n_levels + 1L]]
      }
    }
    if (pool) {
      st <- estimate(!is.na(gi) & !is.na(vals))
      if (!is.null(st)) {
        pooled[1L, j, ] <- st$pct[seq_len(n_levels)]
      }
    }
    if (!test_runs) {
      next
    }
    # The test of `table_battery()`: the answers on the levels shown by
    # the groups, the levels and the groups without an answer left out.
    # An item with answers in fewer than two groups is not testable, one
    # left with one level is not tested.
    tab <- t(matrix(counts[, j, ], nrow = nb, ncol = n_levels))
    tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
    testable[[j]] <- ncol(tab) >= 2L
    tested[[j]] <- testable[[j]] && nrow(tab) >= 2L
    if (!tested[[j]]) {
      next
    }
    tr <- withCallingHandlers(
      .cat_svy_test(
        .design_subset(design, !is.na(gi) & a %in% shown),
        nm,
        by_name,
        chisq_statistic
      ),
      spicy_svy_estimation_failed = svy_log$record,
      warning = svy_warns$record
    )
    n_attempted <- n_attempted + 1L
    n_refused <- n_refused + isTRUE(tr$refused)
    p_raw[[j]] <- tr$p
  }
  valid <- apply(counts, c(1L, 2L), sum)
  dim(valid) <- c(nb, n_items)
  base <- if (drop_na) valid else valid + miss
  top_idx <- match(top_arg, shown)
  pct_top <- if (length(top_idx)) {
    m <- apply(pct[,, top_idx, drop = FALSE], c(1L, 2L), sum)
    dim(m) <- c(nb, n_items)
    m
  }
  no_data <- valid == 0 & block_rows > 0

  # ---- the order of the rows ------------------------------------------------
  # `.bat_order()` reads a percentage as a count over a base of 100.
  key_pct <- if (has_group) pooled else pct
  ord <- .bat_order(
    sort_spec,
    item_labels,
    shown,
    key_pct,
    matrix(ifelse(is.na(key_pct[1L, , 1L]), 0, 100), nrow = 1L),
    top_idx
  )

  # ---- the tests -------------------------------------------------------------
  item_df <- data.frame(
    variable = select_names,
    test_type = ifelse(is.na(p_raw), NA_character_, chisq_statistic),
    p = p_raw,
    stringsAsFactors = FALSE
  )
  adj <- .desc_apply_p_adjust(
    item_df,
    "p",
    item_df$variable,
    if (show_p) p_adjust else "none"
  )
  item_df <- adj$df[ord, , drop = FALSE]
  p_adjust_info <- adj$info
  test_refused <- .design_refusal_regime(n_refused, n_attempted)
  svy_failures <- svy_log$failures()

  # ---- title and note ----------------------------------------------------------
  untestable <- if (test_runs) !testable[ord] else rep(FALSE, n_items)
  untested <- item_notes[ord][testable[ord] & !tested[ord]]
  no_data_note <- if (has_group) {
    .no_data_sentences(
      item_notes[ord],
      lapply(ord, function(j) blocks[no_data[, j]])
    )
  } else {
    .no_data_sentences(item_notes[ord][no_data[1L, ord]])
  }
  meta <- .design_meta(design)
  title_txt <- if (isFALSE(title)) NULL else title %||% .bat_title(by_note)
  note_txt <- .table_note(
    .bat_note(
      drop_na = drop_na,
      left_out = left_out,
      weights_name = NULL,
      n_na_weights = 0L,
      by_label = by_note,
      n_by_na = n_by_na,
      no_data_note = no_data_note,
      show_p = test_runs,
      item_types = NULL,
      n_p = sum(!is.na(item_df[["p"]])),
      p_adjust_info = p_adjust_info,
      untested = untested,
      add = note_add,
      svy = list(
        design_line = .design_note_lines(meta)[[1L]],
        negative_weights = .design_negative_weights_note(
          .design_negative_weights(design),
          meta$n_obs,
          test_refused = test_refused,
          key = "note_negative_weights_battery"
        ),
        not_estimated = .svy_failure_note(svy_failures),
        # No p-value, every test refused or none run: no test to name.
        test_label = if (any(!is.na(p_raw))) {
          .cat_svy_test_label(chisq_statistic)
        },
        definitions = if (show_n) {
          spicy_fmt("note_gloss_n_svy", spicy_str("header_n_lower"))
        }
      )
    ),
    note,
    note_add
  )
  svy_warns$resignal()
  .warn_negative_weights_no_test(test_refused)
  .warn_svy_failures(svy_failures)

  .bat_render(
    items = select_names,
    item_labels = item_labels,
    ord = ord,
    blocks = blocks,
    has_group = has_group,
    shown = shown,
    top_arg = top_arg,
    drop_na = drop_na,
    show_n = show_n,
    show_p = show_p,
    missing_label = missing_label,
    counts = counts,
    pct = pct,
    miss = miss,
    pct_miss = pct_miss,
    pct_top = pct_top,
    base = base,
    item_df = item_df,
    tested = test_runs,
    untestable = untestable,
    p_adjust_info = p_adjust_info,
    title_txt = title_txt,
    note_txt = note_txt,
    output = output,
    align = align,
    percent_digits = percent_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    excel_path = excel_path,
    excel_sheet = excel_sheet,
    clipboard_delim = clipboard_delim,
    word_path = word_path,
    by_name = by_name,
    by_note = by_note,
    class = "spicy_battery_svy_table"
  )
}
