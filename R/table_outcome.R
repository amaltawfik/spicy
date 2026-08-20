# table_outcome(): ONE continuous outcome, described across the levels
# of SEVERAL categorical variables, one block per variable.
#
# The inverse layout of `table_continuous()`. Where that function puts
# several outcomes in rows and one grouping in columns (or in a second
# stub column), this one fixes the outcome and stacks the groupings:
#
#   Descriptive statistics of Body mass index
#
#    Variable         |   M      SD    95% CI LL  95% CI UL    n      p
#    Overall          |  25.93   3.73    25.72      26.14     1188
#    Sex              |                                             .018
#      Female         |  25.69   3.78    25.39      25.98      616
#      Male           |  26.20   3.64    25.90      26.50      572
#
# Every number is produced by the machinery `table_continuous()` uses
# -- `.continuous_compute_one()` for the statistics,
# `run_group_test()` / `compute_effect_size()` for the block
# comparison, `.continuous_stat_cells()` for the strings -- so the two
# tables can never word the same cell differently. What is local to
# this file is the GEOMETRY: a one-column stub, a header row per
# block carrying the block's own statistics, indented level rows, and
# a marginal row above them all.

# Internal: the title of an outcome table, from the outcome's label.
#
# Single source for the console header and the caption every rendering
# engine sets, like `.continuous_title()` / `.categorical_title()`.
#
# It names the OUTCOME only (decision 32). The grouping variables ARE
# the rows, so a title listing them would repeat the stub, and a table
# of six blocks would have no title left.
.outcome_title <- function(outcome_label) {
  spicy_fmt("title_outcome", outcome_label)
}

# Internal: the label of the marginal row.
#
# `"Overall"`, not `"Total"` (decision 32bis), and the two are separate
# registry keys because they are separate things. `label_total` /
# `header_margin_total` is the word of a COUNT margin -- the column of
# `table_categorical()` where frequencies add up. This row is the whole
# analytic sample: a mean is recomputed on it and nothing is added, so
# calling it a total would be a reading error a translator would then
# carry into every language.
.outcome_overall_label <- function() {
  spicy_str("row_overall")
}

# Internal: the default Excel sheet name (decision 16 -- `excel_sheet =
# NULL` in the signature, resolved from the registry here, so the
# \usage line stays clean and the name can follow the table language).
.outcome_excel_sheet <- function(excel_sheet) {
  if (is.null(excel_sheet)) spicy_str("excel_sheet_outcome") else excel_sheet
}

# Internal: the two sentences an outcome table owes its reader.
#
# The first is the honest one gtsummary's equivalent does not print:
# the blocks are separate one-way comparisons and the table adjusts
# none of them for any other. It is only owed when a comparison is
# actually shown.
#
# The second says what the marginal row is, so nobody reads it as a
# total of the block below.
.outcome_structure_notes <- function(
  outcome_label,
  show_comparison,
  overall
) {
  c(
    if (isTRUE(show_comparison)) {
      spicy_fmt("note_outcome_blocks", outcome_label)
    },
    if (isTRUE(overall)) spicy_str("note_outcome_overall")
  )
}

# Internal: the display levels of one `by` variable, and the vector
# that carries them.
#
# Factors keep their DECLARED order (empty levels included: a declared
# level that nobody chose is information about the instrument). Anything
# else -- character, numeric, haven-labelled -- takes the order of first
# appearance, the family convention shared with `table_categorical()`
# and `cross_tab()`.
#
# With `drop_na = FALSE` the missing values become a display level of
# their own, guarded against a collision with a real value the way the
# rest of the family guards it: the scan covers declared levels as well
# as observed ones, so a declared-but-unobserved level literally named
# "(Missing)" cannot duplicate the row.
.outcome_by_levels <- function(g, drop_na) {
  declared <- if (is.factor(g)) levels(g) else unique(g[!is.na(g)])
  declared <- as.character(declared)
  n_na <- sum(is.na(g))
  values <- as.character(g)
  missing_label <- NA_character_
  if (!drop_na && n_na > 0L) {
    missing_label <- spicy_str("row_missing_level")
    seen <- unique(c(as.character(g[!is.na(g)]), declared))
    idx <- 1L
    while (missing_label %in% seen) {
      missing_label <- spicy_fmt("row_missing_level_dedup", idx)
      idx <- idx + 1L
    }
    declared <- c(declared, missing_label)
    values[is.na(values)] <- missing_label
  }
  list(
    levels = declared,
    values = values,
    missing_label = missing_label,
    n_na = n_na
  )
}

# Internal: the group comparison of ONE block.
#
# The producers are `table_continuous()`'s, called on vectors: one
# outcome vector, one grouping vector, the observed groups only. A
# block whose test errors on degenerate data degrades to NA cells with
# a classed warning, and the other blocks are unaffected -- the
# per-variable rule of the sibling, applied per block.
.outcome_block_inference <- function(
  x,
  g_obs,
  level_order,
  var_name,
  var_test,
  do_test,
  do_es,
  effect_size,
  effect_size_explicit,
  ci_level
) {
  test_row <- data.frame(
    test_type = NA_character_,
    statistic = NA_real_,
    df1 = NA_real_,
    df2 = NA_real_,
    p.value = NA_real_,
    stringsAsFactors = FALSE
  )
  es_row <- data.frame(
    es_type = NA_character_,
    es_value = NA_real_,
    es_ci_lower = NA_real_,
    es_ci_upper = NA_real_,
    stringsAsFactors = FALSE
  )
  out <- list(
    test = test_row,
    es = es_row,
    test_used = NA_character_,
    n_groups = NA_integer_
  )
  if (!do_test && !do_es) {
    return(out)
  }
  complete <- !is.na(x) & !is.na(g_obs)
  xvec <- x[complete]
  gvec <- g_obs[complete]
  # Pin the test's group order to the DISPLAYED order: the formula
  # interface of t.test() / wilcox.test() would otherwise re-sort a
  # bare character or numeric `by` and flip the sign convention of the
  # statistic relative to the rows above it.
  gvec <- droplevels(factor(as.character(gvec), levels = level_order))
  n_valid_groups <- length(unique(gvec))
  grp_n <- table(gvec)
  if (!(n_valid_groups >= 2L && all(grp_n >= 2L))) {
    return(out)
  }
  out$n_groups <- n_valid_groups
  if (do_test) {
    out$test_used <- var_test
    out$test <- tryCatch(
      run_group_test(xvec, gvec, n_valid_groups, var_test),
      error = function(e) {
        spicy_warn(
          c(
            sprintf(
              "The group comparison failed for `%s` (%s); its test columns are NA.",
              var_name,
              conditionMessage(e)
            ),
            "i" = "The other blocks are unaffected. For near-constant data, `test = \"nonparametric\"` may still be defined, or set `p_value = FALSE`."
          ),
          class = "spicy_undefined_stat"
        )
        test_row
      }
    )
  }
  if (do_es) {
    chosen <- resolve_effect_size_choice(
      effect_size,
      n_valid_groups,
      var_test,
      explicit = effect_size_explicit
    )
    if (!identical(chosen, "none")) {
      out$es <- tryCatch(
        compute_effect_size(
          xvec,
          gvec,
          n_valid_groups,
          var_test,
          ci_level,
          type = chosen
        ),
        error = function(e) {
          spicy_warn(
            sprintf(
              "The effect size failed for `%s` (%s); its cells are NA.",
              var_name,
              conditionMessage(e)
            ),
            class = "spicy_undefined_stat"
          )
          es_row
        }
      )
      # `is.na(NaN)` is TRUE, so an NA-first guard would let a 0/0 NaN
      # through unblanked; test NaN explicitly beside the +/-Inf case.
      undefined_es <- is.nan(out$es$es_value) ||
        (!is.na(out$es$es_value) && !is.finite(out$es$es_value))
      if (undefined_es) {
        spicy_warn(
          sprintf(
            "The %s effect size is undefined for `%s` (non-finite value); its cells are NA.",
            chosen,
            var_name
          ),
          class = "spicy_undefined_stat"
        )
        out$es$es_value <- NA_real_
        out$es$es_ci_lower <- NA_real_
        out$es$es_ci_upper <- NA_real_
      }
    }
  }
  out
}

# Internal: the compute frame of an outcome table.
#
# One row per displayed row, in display order:
#
#   * the marginal row (`.row_role == "summary"`), when `overall`;
#   * per `by` variable, a header row (`factor_header`) carrying the
#     block's own statistics -- test, p, effect size -- and no
#     descriptive cell, then one row per displayed level (`level`, or
#     `missing` for the missing-value display level) carrying the
#     descriptive cells and no block statistic.
#
# The invariant that makes the marginal row a legitimate denominator:
# with `drop_na = FALSE` the levels of a block PARTITION the
# outcome-complete sample -- the missing display level included -- so
# the block's counts sum to the marginal count exactly. With
# `drop_na = TRUE` each block loses its own missing `by` rows, and the
# note says so, per variable.
#
# `rescale` normalises the weights over the outcome's whole surviving
# sample, ONCE, never per level: a per-level rescale would destroy the
# relative weights across levels, which is the entire information a
# sampling weight carries into this table.
.outcome_compute <- function(
  outcome,
  by_list,
  by_labels,
  outcome_name,
  outcome_label,
  ci_level = 0.95,
  weights_vec = NULL,
  rescale = FALSE,
  drop_na = FALSE,
  test = "welch",
  do_test = FALSE,
  do_es = FALSE,
  effect_size = "none",
  effect_size_explicit = FALSE,
  overall = TRUE
) {
  w_var <- .prep_variable_weights(outcome, weights_vec, rescale)
  empty_block <- data.frame(
    test_type = NA_character_,
    statistic = NA_real_,
    df1 = NA_real_,
    df2 = NA_real_,
    p.value = NA_real_,
    es_type = NA_character_,
    es_value = NA_real_,
    es_ci_lower = NA_real_,
    es_ci_upper = NA_real_,
    smd_type = NA_character_,
    smd_value = NA_real_,
    stringsAsFactors = FALSE
  )
  identity_row <- function(variable, label, level, role) {
    data.frame(
      variable = variable,
      label = label,
      level = level,
      .row_role = role,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  desc_of <- function(idx) {
    .continuous_compute_one(
      outcome[idx],
      ci_level,
      w = if (is.null(w_var)) NULL else w_var[idx]
    )
  }
  blank_desc <- .continuous_compute_one(numeric(0), ci_level)
  blank_desc$n <- NA_integer_

  rows <- list()
  if (isTRUE(overall)) {
    rows[[length(rows) + 1L]] <- cbind(
      identity_row(outcome_name, outcome_label, NA_character_, "summary"),
      desc_of(seq_along(outcome)),
      empty_block
    )
  }

  test_used <- stats::setNames(
    rep(NA_character_, length(by_list)),
    names(by_list)
  )
  by_na_dropped <- stats::setNames(
    rep(0L, length(by_list)),
    names(by_list)
  )
  missing_labels <- stats::setNames(
    rep(NA_character_, length(by_list)),
    names(by_list)
  )
  n_groups <- stats::setNames(
    rep(NA_integer_, length(by_list)),
    names(by_list)
  )

  for (j in seq_along(by_list)) {
    nm <- names(by_list)[[j]]
    g <- by_list[[j]]
    geom <- .outcome_by_levels(g, drop_na)
    missing_labels[[nm]] <- geom$missing_label
    if (drop_na && geom$n_na > 0L) {
      by_na_dropped[[nm]] <- geom$n_na
    }
    inf <- .outcome_block_inference(
      outcome,
      g,
      geom$levels,
      nm,
      test,
      do_test,
      do_es,
      effect_size,
      effect_size_explicit,
      ci_level
    )
    test_used[[nm]] <- inf$test_used
    n_groups[[nm]] <- inf$n_groups
    rows[[length(rows) + 1L]] <- cbind(
      identity_row(nm, by_labels[[nm]], NA_character_, "factor_header"),
      blank_desc,
      inf$test,
      inf$es,
      data.frame(
        smd_type = NA_character_,
        smd_value = NA_real_,
        stringsAsFactors = FALSE
      )
    )
    for (lv in geom$levels) {
      idx <- which(geom$values == lv)
      rows[[length(rows) + 1L]] <- cbind(
        identity_row(
          nm,
          by_labels[[nm]],
          lv,
          if (identical(lv, geom$missing_label)) "missing" else "level"
        ),
        desc_of(idx),
        empty_block
      )
    }
  }

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  attr(result, "test_used") <- test_used
  attr(result, "by_na_dropped") <- by_na_dropped
  attr(result, "missing_labels") <- missing_labels
  attr(result, "n_groups") <- n_groups
  result
}
