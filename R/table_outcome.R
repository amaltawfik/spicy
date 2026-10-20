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

# The label of the marginal row is `row_overall`: `"Overall"`, not
# `"Total"` (decision 32bis), and the two are separate registry keys
# because they are separate things. `label_total` / `header_margin_total`
# is the word of a COUNT margin -- the column of `table_categorical()`
# where frequencies add up. This row is the whole analytic sample: a mean
# is recomputed on it and nothing is added, so calling it a total would be
# a reading error a translator would then carry into every language. A
# `select` variable displaying that label is refused (`table_outcome()`,
# `.abort_label_collision()`).

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
.outcome_structure_notes <- function(outcome_label, show_comparison, overall) {
  c(
    if (isTRUE(show_comparison)) {
      spicy_fmt("note_outcome_blocks", outcome_label)
    },
    if (isTRUE(overall)) {
      .outcome_overall_note()
    }
  )
}

# Internal: the sentence that says what the marginal row is. It quotes
# the row, so when `options(spicy.labels)` relabels the row and leaves the
# sentence alone -- the second way out of a `select` variable displaying
# the row's label -- the sentence takes the row's label as an argument
# (`note_outcome_overall_relabeled`) and never names a row the table does
# not show. A sentence the user rewrote is printed as written, and a
# language's own pair as it ships.
.outcome_overall_note <- function() {
  opt <- getOption("spicy.labels", NULL)
  overridden <- if (is.null(opt)) {
    character(0)
  } else {
    names(.spicy_labels_option(opt))
  }
  if (
    "row_overall" %in% overridden && !"note_outcome_overall" %in% overridden
  ) {
    spicy_fmt("note_outcome_overall_relabeled", spicy_str("row_overall"))
  } else {
    spicy_str("note_outcome_overall")
  }
}

# Internal: the display levels of one `select` variable, and the vector
# that carries them.
#
# Factors keep their DECLARED order (empty levels included: a declared
# level that nobody chose is information about the instrument). Anything
# else -- character, logical, numeric, haven-labelled -- is sorted in the
# C locale, the family's order of the groups (`.group_levels()`), shared
# with `table_categorical()` and `cross_tab()`.
#
# With `drop_na = FALSE` the missing values become a display level of
# their own. Every declared level is displayed, so one with the label of
# that row is refused, the way the rest of the family refuses it
# (`.abort_label_collision()`); `var` names the variable for the message.
.outcome_by_levels <- function(
  g,
  drop_na,
  var = NULL,
  call = rlang::caller_env()
) {
  declared <- .group_levels(g)
  n_na <- sum(is.na(g))
  values <- as.character(g)
  missing_label <- NA_character_
  if (!drop_na && n_na > 0L) {
    missing_label <- spicy_str("row_missing_level")
    if (missing_label %in% declared) {
      .abort_label_collision(
        missing_label,
        var,
        "level",
        "the row spicy adds for missing values",
        "row",
        "row_missing_level",
        "(Not stated)",
        call = call
      )
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
  # bare character or numeric `select` and flip the sign convention of the
  # statistic relative to the rows above it.
  gvec <- droplevels(factor(as.character(gvec), levels = level_order))
  n_valid_groups <- length(unique(gvec))
  grp_n <- table(gvec)
  if (!(n_valid_groups >= 2L && all(grp_n >= 2L))) {
    return(out)
  }
  out$n_groups <- n_valid_groups
  if (do_test && length(unique(xvec)) < 2L) {
    # An outcome with no variance has no test: `t.test()` stops on it,
    # and the rank tests return p = 1 or NaN, a p-value of nothing.
    spicy_warn(
      c(
        sprintf(
          "The group comparison failed for `%s` (the outcome has no variance); its test columns are NA.",
          var_name
        ),
        "i" = "The other blocks are unaffected."
      ),
      class = "spicy_undefined_stat"
    )
  } else if (do_test) {
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
    # The note names the test of a block only when it ran.
    if (!is.na(out$test$test_type)) {
      out$test_used <- var_test
    }
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
#   * per `select` variable, a header row (`factor_header`) carrying the
#     block's own statistics -- test, p, effect size -- and no
#     descriptive cell, then one row per displayed level (`level`, or
#     `missing` for the missing-value display level) carrying the
#     descriptive cells and no block statistic.
#
# The invariant that makes the marginal row a legitimate denominator:
# with `drop_na = FALSE` the levels of a block PARTITION the
# outcome-complete sample -- the missing display level included -- so
# the block's counts sum to the marginal count exactly. With
# `drop_na = TRUE` each block loses its own missing `select` rows, and the
# note says so, per variable.
#
# `rescale` normalizes the weights over the outcome's whole surviving
# sample, ONCE, never per level: a per-level rescale would destroy the
# relative weights across levels, which is the entire information a
# sampling weight carries into this table.
.outcome_compute <- function(
  outcome,
  select_list,
  select_labels,
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
  overall = TRUE,
  call = rlang::caller_env()
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
    rep(NA_character_, length(select_list)),
    names(select_list)
  )
  select_na_dropped <- stats::setNames(
    rep(0L, length(select_list)),
    names(select_list)
  )
  missing_labels <- stats::setNames(
    rep(NA_character_, length(select_list)),
    names(select_list)
  )
  n_groups <- stats::setNames(
    rep(NA_integer_, length(select_list)),
    names(select_list)
  )

  for (j in seq_along(select_list)) {
    nm <- names(select_list)[[j]]
    g <- select_list[[j]]
    geom <- .outcome_by_levels(g, drop_na, nm, call = call)
    missing_labels[[nm]] <- geom$missing_label
    if (drop_na && geom$n_na > 0L) {
      select_na_dropped[[nm]] <- geom$n_na
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
    # No standardized mean difference: this table has no `smd`
    # argument, and the two columns it once carried were always NA
    # (decision 9 of the ADR of 2026-10-20).
    rows[[length(rows) + 1L]] <- cbind(
      identity_row(nm, select_labels[[nm]], NA_character_, "factor_header"),
      blank_desc,
      inf$test,
      inf$es
    )
    for (lv in geom$levels) {
      idx <- which(geom$values == lv)
      rows[[length(rows) + 1L]] <- cbind(
        identity_row(
          nm,
          select_labels[[nm]],
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
  attr(result, "select_na_dropped") <- select_na_dropped
  attr(result, "missing_labels") <- missing_labels
  # The levels of each `select` variable the test compares (`NA` when no
  # test ran): `n_levels`, the family's name for a count of categories.
  attr(result, "n_levels") <- n_groups
  result
}

# ---- the public function --------------------------------------------------

# Internal: hard migration error for the renamed `by` argument.
#
# `by` survives as a default-less formal at the END of the signature --
# never in the third position, which now belongs to `select` and must
# stay reachable positionally -- purely so that old `by =` calls land
# here and get the actionable replacement message instead of R's bare
# "unused argument" error. Same device as `abort_styled_defunct()` in
# R/cross_tab.R.
#
# The name is not merely retired: `by` returns to this signature in a
# later release as the COMPARED group of the crossed variant, which is
# what `by` means everywhere else in the family. Silently accepting the
# old spelling would therefore be worse than an error -- it would mean
# two incompatible readings of one argument.
abort_outcome_by_renamed <- function() {
  spicy_abort(
    c(
      paste0(
        "`by` was renamed `select` in table_outcome() (spicy 0.13.0): ",
        "the grouping characteristics structure the rows, as `select` ",
        "does across the family."
      ),
      "i" = "Use `table_outcome(data, outcome, select = ...)`."
    ),
    class = c("spicy_defunct", "spicy_invalid_input")
  )
}

#' Describe one continuous outcome across several groupings
#'
#' @description
#' Summarizes *one* continuous outcome across the levels of *several*
#' categorical variables, one block of rows per variable. It is the
#' inverse layout of [table_continuous()], which puts several outcomes
#' in rows and one grouping in columns.
#'
#' Each block reports the outcome's statistics level by level, plus its
#' own group comparison on the block's header row, and an `Overall` row
#' gives the marginal summary of the whole analytic sample.
#'
#' @inheritSection table_categorical Multiple-comparison adjustment
#' @details
#' # Which shape do I need?
#'
#' Several continuous variables across *one* grouping is
#' [table_continuous()] (`select = `, `by = `). One continuous variable
#' across one or several groupings is this function. A single `select` is
#' legitimate here -- it is the natural way in when you know more
#' groupings are coming -- but with several outcomes and one grouping,
#' the sibling is the table you want.
#'
#' # Choosing the statistics
#'
#' `show_columns` takes the same tokens as [table_continuous()], with
#' the same meanings; see the `show_columns` section of
#' `?table_continuous` for the vocabulary. Only the character-vector
#' form is accepted here: there is one outcome, so a per-variable list
#' would name nothing.
#'
#' # Weights
#'
#' `weights` applies the frequency-expansion convention of the family:
#' all weights 1 reproduces the unweighted table, and integer weights
#' reproduce the STATISTICS of the data duplicated that many times --
#' `n` stays the raw count of rows that carried the weights. Rows
#' with a missing or zero weight leave the analytic sample; the note
#' counts the missing ones.
#'
#' `rescale` is the switch between the two readings of a weight: the
#' frequency reading above, and the sampling-weight reading, where the
#' weights are normalized to sum to the sample size. See the Weights
#' section of [table_continuous()] for the choice in full.
#'
#' `rescale = TRUE` normalizes the weights over the outcome's whole
#' surviving sample, once, never per level -- a per-level rescale
#' would destroy the relative weights across levels, which is the
#' entire information a sampling weight carries into this table. The
#' means are unchanged by it; the standard deviations move, because
#' their denominator is `sum(w) - 1`.
#'
#' A weighted table refuses the group comparison. The estimates and
#' their interval have no weighted version here, and a p-value or an
#' effect size silently computed unweighted beside weighted
#' descriptives is the one thing that must not happen: set `p_value =
#' FALSE` (and `statistic = FALSE`, `effect_size = "none"`), or use
#' [table_continuous_lm()] for a weighted comparison. The
#' order-statistic median interval is refused for the same reason.
#'
#' # Blocks and the group comparison
#'
#' Every block is a separate one-way comparison of the outcome across
#' the levels of that variable. Nothing in this table adjusts one block
#' for another, and the table note says so. Read the blocks as a set of
#' bivariate descriptions, not as a model.
#'
#' Each block chooses its test independently: with two observed levels
#' `test = "welch"` is the Welch t-test, with three or more it is the
#' Welch one-way ANOVA, and `test = "nonparametric"` is the
#' Wilcoxon rank-sum or the Kruskal-Wallis test on the same rule. A
#' block with fewer than two observed levels, or with a level holding a
#' single observation, is not tested; its statistics stay empty and the
#' other blocks are unaffected.
#'
#' # The `Overall` row
#'
#' `overall = TRUE` puts the marginal summary of the whole analytic
#' sample on the first row. Under the default `drop_na = FALSE` the
#' levels of every block partition that sample -- the `(Missing)`
#' display level included -- so each block's counts add up to the
#' `Overall` count exactly, which is what makes it a usable
#' denominator.
#'
#' The row reads **Overall**, not *Total*, and the distinction is
#' deliberate. *Total* is the word of a COUNT margin: the column of
#' [table_categorical()] where frequencies add up. This row is the
#' whole analytic sample, where a mean is recomputed over every
#' observation and nothing is added. A mean is not a total.
#'
#' A `select` variable displayed as `"Overall"` is an error (class
#' `spicy_label_collision`): choose another label in `labels`, or give the
#' row another label with
#' `options(spicy.labels = list(row_overall = "Whole sample"))`.
#'
#' # Choosing the `select` columns
#'
#' The canonical form is `select = where(is.factor)`, or an explicit
#' enumeration. Negation (`select = -c(x, y)`) is not recommended: it
#' sweeps in every remaining column, and a numeric one becomes a block
#' with one LEVEL per distinct value, in ascending order. A
#' variable producing more than 20 levels raises a warning for that
#' reason -- an arbitrary threshold, but a sixty-row block where a
#' reader expects a handful of categories is not a table.
#'
#' A `haven_labelled` column used as `select` shows its numeric CODES, not
#' its value labels, as it does in [table_continuous()]. Convert it
#' first (`haven::as_factor()`) to get the labels in the stub.
#'
#' # The table note
#'
#' One note sits under the table and states what left the analytic
#' sample, which group comparison ran in each block, what the displayed
#' columns mean, and how the blocks and the `Overall` row are to be
#' read. The rendering engines carry the same sentence as a table note.
#' On the `"tinytable"` route it is set one size down;
#' `options(spicy.note_style)` governs that (see [table_regression()]).
#'
#' @param data A data frame.
#' @param outcome The continuous outcome, unquoted or as a string.
#'   Exactly one column.
#' @param select The grouping characteristics to describe the outcome
#'   across, as a tidyselect expression or a character vector of column
#'   names. One block of rows per variable, in the order given, and in
#'   each block one row per level: the levels of a factor in their order,
#'   any other variable sorted, as the groups of `by` are in
#'   [table_continuous()] (text in the C locale, `FALSE` before `TRUE`,
#'   numbers and codes ascending), whatever the order of the rows. As
#'   everywhere in the family, `select` is what structures the rows. The
#'   outcome and the `weights` column are not part of the selection: a
#'   selection helper skips them, and naming one in `select`, or giving
#'   its position, is an error. A name that is not a column of `data` is
#'   an error (class `spicy_missing_column`), and so is an empty
#'   selection.
#' @param labels How each variable is named in the table: in the rows,
#'   in the title, and in the notes. The outcome follows
#'   the same rule as the `select` variables. Four forms:
#'   * `labels = FALSE` shows the column name.
#'   * `labels = TRUE` shows the column's `label` attribute (as set by
#'     `haven`, `labelled`, or [label_from_names()]) when it is present,
#'     not `NA`, and not empty, and the column name otherwise.
#'   * `labels = c(bmi = "Body mass index")`, a named character vector,
#'     gives the columns it names these labels. Every other column
#'     follows `options(spicy.var_labels)`: its name when the option is
#'     unset or `FALSE`, its `label` attribute when it is `TRUE`.
#'   * `labels = var_labels(bmi = "BMI (kg/m2)")` gives the columns it
#'     names these labels, and every other column its `label` attribute,
#'     whatever the option says. `var_labels()` alone is `labels = TRUE`.
#'
#'   Names that match no column are ignored, so one vector can serve
#'   every table of a project. The default reads
#'   `options(spicy.var_labels)`, and a `TRUE` or `FALSE` typed in the
#'   call wins over it. Any other value, `NULL` included, is an error. A
#'   variable label documents the data file; a published table carries
#'   the wording you set here.
#' @param overall Show the marginal `Overall` row (default `TRUE`).
#' @param drop_na Drop rows with a missing `select` value from that
#'   block. The rule of the family: the categories a table counts keep
#'   their missing values as a row by default, as in
#'   [table_categorical()]; the values it computes on lose them, counted
#'   in the note, as in [table_continuous()]. The blocks of this table are
#'   categories, so `FALSE` (the default) shows the missing values of a
#'   `select` variable as a `(Missing)` level, excluded from the
#'   comparison, and a level displayed with that label is an error, as in
#'   [table_categorical()]; the missing values of the outcome are always
#'   removed and counted in the note. Under `drop_na = TRUE`, a
#'   grouping that no observation holds is an error (class
#'   `spicy_invalid_data`).
#' @param test Group comparison for every block: `"welch"` (default),
#'   `"student"` or `"nonparametric"`. An outcome with no variance in a
#'   block has no test there, whichever is chosen: the block's test cells
#'   are left blank, with a warning (class `spicy_undefined_stat`).
#' @param p_value Show the p-value column: `TRUE`, `FALSE`, or `NULL`
#'   (the default), which shows it, every block being a comparison.
#' @param p_adjust Multiple-comparison adjustment of the p-value
#'   column, applied to the family of every test in the table (one
#'   p-value per block). One of `"none"` (default), `"holm"`,
#'   `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`, or
#'   `"BY"`, as in [table_regression()]; delegated to
#'   [stats::p.adjust()]. Needs the p-value column. See
#'   *Multiple-comparison adjustment*.
#' @param statistic Logical. If `TRUE`, the test statistic of each block
#'   is shown in an additional column, with or without the *p*-value:
#'   `p_value` and `statistic` are independent. Defaults to `FALSE`.
#' @param show_columns Statistics to display, as a character vector of
#'   the tokens of [table_continuous()] (see *Choosing the statistics*).
#'   `NULL` (the default) shows the mean, SD, min, max, the mean CI (see
#'   `ci`) and `n` (see `show_n`).
#' @param effect_size,effect_size_ci Effect size per block and its
#'   confidence interval, as in [table_continuous()] (a logical
#'   `effect_size` is an error there and here).
#' @param output One of `"default"`, `"data.frame"`, `"long"`, or a
#'   rendering engine: `"tinytable"`, `"gt"`, `"flextable"`,
#'   `"excel"`, `"clipboard"`, `"word"`. `"data.frame"` is the displayed
#'   table with its numbers, `"long"` the compute frame with every
#'   statistic, as in [table_continuous()]; see *Value*.
#' @param excel_sheet Sheet name for `output = "excel"`. `NULL` (the
#'   default) uses `"Outcome"`.
#' @param user_na Logical. If `TRUE` (the default), declared missing
#'   values are treated as missing: those of the outcome leave its
#'   statistics and are disclosed in the table note (`Declared missing
#'   values removed: ...`), those of a `select` variable join its
#'   `(Missing)` level under `drop_na = FALSE`. If `FALSE`, the declared
#'   codes are ordinary values. See the "Declared missing values" section
#'   of [freq()].
#' @param by Defunct. The grouping characteristics are selected with
#'   `select` since spicy 0.13.0; supplying `by` is an error.
#' @inheritParams table_continuous
#' @inheritParams table_categorical
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: a `spicy_outcome_table` (a `data.frame` subclass,
#'     with the parent class `"spicy_table"`), printed when the call is
#'     bare; its columns are the table's compute frame, and the display
#'     frame and the typed view are attached.
#'   \item `"data.frame"`: the displayed table with its numbers, what
#'     [as.data.frame()][as.data.frame.spicy_outcome_table] of the default
#'     object returns: one row per printed row, the key columns
#'     `variable` (the outcome on the `Overall` row, the grouping variable
#'     elsewhere), `label`, `level` and `.row_role` (`"summary"` for the
#'     `Overall` row, `"factor_header"`, `"level"`, or `"missing"`), then
#'     the printed columns under their names (`"M"`, `"SD"`, ..., `"n"`,
#'     `"p"`), a cell that prints several numbers keeping the others
#'     beside it as in [table_continuous()].
#'   \item `"long"`: the compute frame, one row per printed row with every
#'     statistic the table computes, whatever it displays: `variable`,
#'     `label`, `level`, `.row_role`, `mean`, `sd`, `min`, `max`,
#'     `ci_lower`, `ci_upper`, `median`, `q1`, `q3`, `iqr`,
#'     `med_ci_lower`, `med_ci_upper`, `n`, `weighted_n`, the test of each
#'     block on its header row (`test_type`, `statistic`, `df`,
#'     `df.residual`, `p.value`, and `p_unadjusted` under `p_adjust`) and
#'     its effect size (`es_type`, `es_value`, `es_ci_lower`,
#'     `es_ci_upper`, `NA` when none was requested).
#'   \item `"tinytable"`, `"gt"`, `"flextable"`: the rendered table.
#'   \item `"excel"` / `"word"`: writes the file and returns its path
#'     invisibly.
#'   \item `"clipboard"`: copies the table and returns the display
#'     `data.frame` invisibly.
#' }
#'
#' The default object and the two data frames carry the whole note as
#' printed, "Note." included, in `attr(x, "note")` (`NULL` when there is
#' nothing to say).
#'
#' @seealso [table_continuous()] for the transposed shape,
#'   [table_categorical()] for categorical outcomes.
#' @export
#'
#' @examples
#' table_outcome(sochealth, bmi, select = c(sex, smoking))
#' table_outcome(sochealth, wellbeing_score, select = where(is.factor))
table_outcome <- function(
  data,
  outcome,
  select,
  labels = getOption("spicy.var_labels", FALSE),
  overall = TRUE,
  drop_na = FALSE,
  weights = NULL,
  rescale = getOption("spicy.rescale", FALSE),
  test = c("welch", "student", "nonparametric"),
  p_value = NULL,
  p_adjust = "none",
  statistic = FALSE,
  show_n = TRUE,
  show_columns = NULL,
  effect_size = c(
    "none",
    "auto",
    "hedges_g",
    "eta2",
    "r_rb",
    "epsilon2"
  ),
  effect_size_ci = FALSE,
  ci = TRUE,
  ci_level = 0.95,
  digits = 2,
  effect_size_digits = 2,
  p_digits = 3,
  decimal_mark = ".",
  align = c("decimal", "center", "right"),
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
  indent_text = "  ",
  indent_text_excel_clipboard = strrep("\u00A0", 6),
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  user_na = TRUE,
  style = NULL,
  by
) {
  # Migration guard first, so old `by =` calls get the actionable
  # replacement message before any other validation can fire.
  if (!missing(by)) {
    abort_outcome_by_renamed()
  }

  # `outcome` and `select` carry no default -- one names the variable
  # being described, the other the blocks it is described across, and
  # the table is nothing without either. Left to lazy evaluation they
  # surfaced far downstream as a bare base-R `missingArgError`
  # ("argument \"expr\" is missing"), translated by the session's locale
  # and naming a variable that appears nowhere in the user's call.
  # Refuse here, classed, in the words of the argument that is actually
  # missing. `table_categorical()` / `table_continuous()` need no twin
  # of this: `select` is optional there and defaults to the whole frame.
  if (missing(outcome)) {
    spicy_abort(
      c(
        "`outcome` is required: table_outcome() describes one variable.",
        "i" = paste0(
          "Name the continuous variable to describe, e.g. ",
          "`table_outcome(data, bmi, select = c(sex, smoking))`."
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  if (missing(select)) {
    spicy_abort(
      c(
        "`select` is required: table_outcome() has no rows without it.",
        "i" = paste0(
          "Name the characteristics to describe the outcome across, ",
          "e.g. `select = c(sex, smoking)` or ",
          "`select = where(is.factor)`."
        )
      ),
      class = "spicy_invalid_input"
    )
  }

  # A journal / locale style only moves DEFAULTS (see `?spicy_style`).
  .style_pushed <- .style_begin(style, match.call(), environment())
  on.exit(.style_end(.style_pushed), add = TRUE)

  # --- scalar validation, shared with the sibling ------------------------
  .check_data_frame(data, "table_outcome")
  validate_ci_level(ci_level)
  digits <- validate_digit_arg(digits, "digits")
  effect_size_digits <- validate_digit_arg(
    effect_size_digits,
    "effect_size_digits"
  )
  p_digits <- validate_digit_arg(p_digits, "p_digits", min = 1L)
  validate_decimal_mark(decimal_mark)
  .check_labels_arg(labels)
  for (.lname in c(
    "overall",
    "drop_na",
    "statistic",
    "effect_size_ci",
    "show_n",
    "ci",
    "user_na"
  )) {
    validate_logical_scalar(get(.lname), .lname)
  }
  # `effect_size` names a measure; the logical it once took is defunct,
  # and so are the spellings renamed in 0.14.0.
  .check_effect_size_not_logical(effect_size, "`effect_size = \"auto\"`")
  .check_effect_size_renamed(effect_size, .ES_RENAMED_DESC)
  effect_size_explicit <- !missing(effect_size)
  effect_size <- spicy_match_arg(effect_size)
  if (
    !is.null(p_value) &&
      (!is.logical(p_value) || length(p_value) != 1L || is.na(p_value))
  ) {
    spicy_abort(
      "`p_value` must be TRUE, FALSE, or NULL.",
      class = "spicy_invalid_input"
    )
  }
  output <- spicy_match_arg(output)
  excel_sheet <- .outcome_excel_sheet(excel_sheet)
  test_explicit <- !missing(test)
  show_n_explicit <- !missing(show_n)
  ci_explicit <- !missing(ci)
  test <- spicy_match_arg(test)
  align <- spicy_match_arg(align)
  # An argument of one route only (`word_path` for `output = "word"`,
  # ...) typed with another `output` reaches nothing.
  .warn_ignored_output_args(output, names(match.call()))

  # --- the outcome: exactly one continuous column ------------------------
  outcome_names <- resolve_multi_column_selection(
    rlang::enquo(outcome),
    data,
    "outcome"
  )
  if (length(outcome_names) != 1L) {
    spicy_abort(
      c(
        sprintf(
          "`outcome` must select exactly one column (it selected %d).",
          length(outcome_names)
        ),
        "i" = "Several continuous variables across one grouping is the other shape: `table_continuous(select = , by = )`."
      ),
      class = "spicy_invalid_input"
    )
  }
  outcome_name <- outcome_names[[1L]]

  # --- the groupings: at least one column, all of them in `data` ---------
  # The outcome and the weights column are outside the selection: a
  # helper skips them, and pointing at one, by name or by position, is an
  # error (a variable described across its own levels, or the weights
  # used as a grouping).
  weights_quo <- rlang::enquo(weights)
  weights_name <- detect_weights_column_name(weights_quo, data)
  select_names <- .resolve_selection_without(
    rlang::enquo(select),
    data,
    roles = c(outcome = outcome_name, weights = weights_name)
  )
  if (length(select_names) == 0L) {
    .abort_empty_selection(
      "select",
      hint = "`select = where(is.factor)` selects the categorical columns."
    )
  }
  .check_integer64_columns(data, c(outcome_name, select_names), "table_outcome")

  outcome_raw <- data[[outcome_name]]
  if (!is.numeric(outcome_raw)) {
    spicy_abort(
      c(
        "A categorical outcome is not supported yet.",
        "i" = "Describe a categorical variable with `table_categorical()`.",
        "i" = "Type dispatch on the outcome is planned."
      ),
      class = "spicy_not_implemented"
    )
  }

  # --- declared missing values (see the "Declared missing values"
  # section of ?freq) ------------------------------------------------------
  resolve_user_na <- function(v) {
    if (isTRUE(user_na)) .user_na_to_na(v) else .user_na_zap(v)
  }
  n_user_na <- if (user_na) sum(.user_na_mask(outcome_raw)) else 0L
  outcome_vec <- resolve_user_na(outcome_raw)
  n_outcome_na <- sum(is.na(outcome_vec)) - n_user_na
  select_list <- lapply(select_names, function(nm) resolve_user_na(data[[nm]]))
  names(select_list) <- select_names
  # A grouping no observation holds leaves its block without a group once
  # the missing values are removed: refused, as a `by` is across the
  # family.
  if (drop_na) {
    for (nm in select_names) {
      .check_grouping_observed(
        select_list[[nm]],
        sprintf("`select` (`%s`)", nm)
      )
    }
  }

  # A grouping with too many levels is not refused -- the family never
  # refuses a numeric `select` -- but it is announced: sixty one-row blocks
  # is not a table, and the usual cause is a continuous column swept in
  # by a negation.
  for (nm in select_names) {
    k <- length(unique(stats::na.omit(as.character(select_list[[nm]]))))
    if (k > .OUTCOME_CARDINALITY_WARN) {
      spicy_warn(
        c(
          sprintf("`%s` has %d levels; is it a categorical variable?", nm, k),
          "i" = "`select = where(is.factor)` selects the categorical columns."
        ),
        class = "spicy_caveat"
      )
    }
  }

  # --- weights (decision 17) ---------------------------------------------
  weights_vec <- resolve_weights_argument(weights_quo, data, "weights")
  validate_logical_scalar(rescale, "rescale")
  if (!is.null(weights_vec)) {
    # NA weights are legal: those rows leave the analytic sample and
    # the exclusion is disclosed. Only genuinely non-finite values are
    # rejected.
    .check_weights_values(weights_vec)
  } else if (isTRUE(rescale) && !missing(rescale)) {
    .warn_ignored("rescale", "when `weights` is not supplied")
  }

  # --- what the table shows and what it tests -----------------------------
  .check_desc_p_adjust(p_adjust, TRUE, p_value)
  p_value <- if (is.null(p_value)) TRUE else p_value
  has_es_request <- !identical(effect_size, "none")
  if (effect_size_ci && !has_es_request) {
    spicy_warn(
      "`effect_size_ci` implies `effect_size != \"none\"`. Defaulting to `effect_size = \"auto\"`.",
      class = "spicy_ignored_arg"
    )
    effect_size <- "auto"
    has_es_request <- TRUE
  }
  do_test <- p_value || statistic
  do_es <- has_es_request
  if (do_es && !do_test) {
    do_test <- TRUE
  }
  # Decision 17, verbatim from the sibling. The group tests and their
  # effect sizes have no weighted version here, and the refusal
  # protects INFERENCE: a p-value or an effect size read against an
  # interval must not be silently unweighted beside weighted
  # descriptives.
  if (!is.null(weights_vec) && (do_test || do_es)) {
    spicy_abort(
      c(
        "Weighted group TESTS and effect sizes are not implemented.",
        "i" = paste0(
          "Set `p_value = FALSE` (and `statistic = FALSE`, ",
          "`effect_size = \"none\"`) for weighted descriptives; use ",
          "`table_continuous_lm(weights = )` for weighted comparisons."
        )
      ),
      class = "spicy_not_implemented"
    )
  }
  if (
    test_explicit &&
      !p_value &&
      !statistic &&
      !has_es_request &&
      !effect_size_ci
  ) {
    spicy_warn(
      "`test` is ignored when `p_value`, `statistic`, `effect_size`, and `effect_size_ci` are all turned off.",
      class = "spicy_ignored_arg"
    )
  }

  if (is.list(show_columns) && !is.null(names(show_columns))) {
    spicy_abort(
      c(
        "`show_columns` must be a character vector here.",
        "i" = "There is one outcome, so a per-variable list names nothing. Use `table_continuous()` for a table of several outcomes."
      ),
      class = "spicy_invalid_input"
    )
  }
  legacy_tokens <- order_continuous_tokens(c(
    "m",
    "sd",
    "min",
    "max",
    if (isTRUE(ci)) "ci",
    if (isTRUE(show_n)) "n"
  ))
  col_spec <- resolve_continuous_show_columns(
    show_columns,
    outcome_name,
    legacy_tokens
  )
  tokens <- col_spec$union
  if (!is.null(show_columns)) {
    if (show_n_explicit && !identical(isTRUE(show_n), "n" %in% tokens)) {
      spicy_warn(
        "`show_n` is ignored: `show_columns` decides whether the `n` column is shown (add or drop the \"n\" token).",
        class = "spicy_ignored_arg"
      )
    }
    if (ci_explicit && !identical(isTRUE(ci), "ci" %in% tokens)) {
      spicy_warn(
        "`ci` is ignored: `show_columns` decides whether the mean confidence interval is shown (add or drop the \"ci\" token).",
        class = "spicy_ignored_arg"
      )
    }
  }
  show_n <- "n" %in% tokens
  ci <- "ci" %in% tokens

  # Decision-17 token guards, verbatim from the sibling.
  if (!is.null(weights_vec) && "med_ci" %in% tokens) {
    spicy_abort(
      c(
        "The median confidence interval is not available with `weights`.",
        "i" = paste0(
          "`med_ci` is an order-statistic interval with no weighted ",
          "version; drop the \"med_ci\" token from `show_columns`."
        )
      ),
      class = "spicy_not_implemented"
    )
  }
  if (is.null(weights_vec) && "weighted_n" %in% tokens) {
    spicy_abort(
      "The \"weighted_n\" column requires `weights`.",
      class = "spicy_invalid_input"
    )
  }

  # The table tests what it shows: a display carrying a median-based
  # position statistic WITHOUT the mean takes the rank-based test
  # unless `test` was given explicitly. One outcome, so this is a
  # scalar rather than a per-variable vector.
  median_only <- any(.continuous_median_tokens %in% tokens) &&
    !("m" %in% tokens)
  if (!test_explicit && median_only) {
    test <- "nonparametric"
  } else if (test_explicit && median_only && (do_test || do_es)) {
    spicy_warn(
      c(
        sprintf(
          "`test = \"%s\"` is applied to an outcome displaying a median without a mean.",
          test
        ),
        "i" = "Drop `test` to use the rank-based test, or add \"m\" to `show_columns`."
      ),
      class = "spicy_caveat"
    )
  }

  # --- labels -------------------------------------------------------------
  all_labels <- resolve_variable_labels(
    data,
    c(outcome_name, select_names),
    labels
  )
  names(all_labels) <- c(outcome_name, select_names)
  outcome_label <- all_labels[[outcome_name]]
  # The marginal row and the variables' own rows share the stub at the
  # same indent: a variable displaying the row's label is refused.
  if (overall) {
    hit <- match(spicy_str("row_overall"), all_labels[select_names])
    if (!is.na(hit)) {
      .abort_label_collision(
        spicy_str("row_overall"),
        select_names[[hit]],
        "label",
        "the row spicy adds for the whole sample",
        "row",
        "row_overall",
        "Whole sample"
      )
    }
  }

  # --- compute ------------------------------------------------------------
  result <- .outcome_compute(
    outcome = outcome_vec,
    select_list = select_list,
    select_labels = all_labels[select_names],
    outcome_name = outcome_name,
    outcome_label = outcome_label,
    ci_level = ci_level,
    weights_vec = weights_vec,
    rescale = rescale,
    drop_na = drop_na,
    test = test,
    do_test = do_test,
    do_es = do_es,
    effect_size = effect_size,
    effect_size_explicit = effect_size_explicit,
    overall = overall
  )
  select_na_dropped <- attr(result, "select_na_dropped")
  test_used <- attr(result, "test_used")
  n_groups <- attr(result, "n_levels")
  missing_labels <- attr(result, "missing_labels")

  # `p_adjust`: one test per block, its p-value on the block header.
  # Adjusted on the compute frame, before any route formats it.
  adj <- .desc_apply_p_adjust(
    result,
    "p.value",
    ifelse(result$.row_role == "factor_header", result$variable, NA),
    p_adjust
  )
  result <- adj$df
  p_adjust_info <- adj$info
  # `p_value = FALSE`: the p-value leaves the frame, and the rest of the
  # test too when nothing ran it (it would be NA columns); `statistic` or
  # an effect size keep it (decision of 2026-10-17).
  result <- .desc_drop_test_block(result, p_value, do_test)

  n_na_weights <- if (is.null(weights_vec)) 0L else sum(is.na(weights_vec))
  note <- .outcome_note(
    outcome_name = outcome_name,
    outcome_label = outcome_label,
    n_outcome_na = n_outcome_na,
    n_user_na = n_user_na,
    select_na_dropped = .label_counts(select_na_dropped, all_labels),
    weights_name = weights_name,
    n_na_weights = n_na_weights,
    weighted = !is.null(weights_vec),
    tokens = tokens,
    result = result,
    ci_level = ci_level,
    decimal_mark = decimal_mark,
    test_used = test_used,
    n_groups = n_groups,
    var_labels = all_labels[select_names],
    # The RESULT, not the request. A table whose every block was too
    # thin to compare shows an empty `p` column, and a note promising
    # that "each block compares ..." describes something the reader
    # cannot see. `.outcome_test_note()` already reads the result; the
    # two halves of the same note must not contradict each other.
    show_comparison = do_test &&
      (p_value || statistic || do_es) &&
      any(!is.na(test_used)),
    overall = overall,
    p_adjust_info = p_adjust_info
  )

  # The two machine shapes of the family (decision 5 of the ADR of
  # 2026-10-20): `"long"` is the compute frame -- one row per level, plus
  # the block header and the overall row, which `.row_role` marks -- in
  # the vocabulary of the family; `"data.frame"` is the displayed table
  # with its numbers, `as.data.frame()` of the object built below.
  if (identical(output, "long")) {
    out <- .rename_cols(
      as.data.frame(unclass(result), stringsAsFactors = FALSE),
      c(df1 = "df", df2 = "df.residual")
    )
    rownames(out) <- NULL
    # The one attribute kept: the note, as print() shows it.
    attr(out, "note") <- note
    return(out)
  }

  attr(result, "ci_level") <- ci_level
  attr(result, "digits") <- digits
  attr(result, "effect_size_digits") <- effect_size_digits
  attr(result, "p_digits") <- p_digits
  attr(result, "decimal_mark") <- decimal_mark
  result <- .style_stamp(result)
  attr(result, "align") <- align
  attr(result, "outcome") <- outcome_name
  attr(result, "outcome_label") <- outcome_label
  attr(result, "select") <- select_names
  attr(result, "show_columns") <- tokens
  attr(result, "show_p") <- p_value
  attr(result, "show_statistic") <- statistic
  attr(result, "show_effect_size") <- has_es_request
  attr(result, "show_effect_size_ci") <- effect_size_ci
  attr(result, "show_n") <- show_n
  attr(result, "show_ci") <- ci
  attr(result, "overall") <- overall
  attr(result, "indent_text") <- indent_text
  attr(result, "indent_text_excel_clipboard") <- indent_text_excel_clipboard
  attr(result, "note") <- note
  attr(result, "missing_labels") <- missing_labels
  class(result) <- c("spicy_outcome_table", "spicy_table", "data.frame")

  display_df <- .outcome_display_df(
    result,
    tokens = tokens,
    digits = digits,
    effect_size_digits = effect_size_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    ci_level = ci_level,
    show_statistic = statistic,
    show_p = p_value,
    show_effect_size = has_es_request,
    show_effect_size_ci = effect_size_ci,
    indent_text = indent_text
  )

  if (!output %in% c("default", "data.frame")) {
    # The block geometry is handed over rather than derived: the
    # exporter's own fallback reads the label column, and every row of
    # this shape carries a label. Both vectors come from the typed
    # predicates the three descriptive families share.
    geom <- list(body = .outcome_body_geometry(result))
    return(export_desc_table(
      display_df,
      output = output,
      ci_level = ci_level,
      # ONE stub column: this shape has no group column. The keys, not
      # a count -- gt addresses columns and spanner ids by name.
      stub_keys = .CON_KEY_VARIABLE,
      align = align,
      decimal_mark = decimal_mark,
      show_n = show_n,
      sep_rows = .struct_block_sep_rows(geom),
      indent_rows = .struct_indent_rows(geom),
      indent_text = indent_text,
      indent_text_excel_clipboard = indent_text_excel_clipboard,
      title = .outcome_title(outcome_label),
      excel_path = excel_path,
      excel_sheet = excel_sheet,
      clipboard_delim = clipboard_delim,
      word_path = word_path,
      note = note
    ))
  }

  attr(result, "display_df") <- display_df
  # Typed view: the numbers come from the compute frame, the composite
  # cells from the very display frame the console renders, so the two
  # can never word a cell differently.
  attr(result, "structured") <- .struct_attach_p_adjust(
    .build_outcome_structured(
      result = result,
      display_df = display_df,
      tokens = tokens,
      digits = digits,
      effect_size_digits = effect_size_digits,
      p_digits = p_digits,
      decimal_mark = decimal_mark,
      ci_level = ci_level
    ),
    p_adjust_info
  )
  if (identical(output, "data.frame")) {
    return(as.data.frame(result))
  }
  result
}

# The level count beyond which a `select` variable is announced as
# suspicious. Arbitrary and stated as such in the Rd: the family has no
# other threshold, and this one exists because a table of sixty
# one-row blocks is not a table.
.OUTCOME_CARDINALITY_WARN <- 20L

# Internal: the display frame the console and the engines render.
#
# One stub column, then the statistic columns of the shared token
# vocabulary, then the block comparison. The geometry is the whole
# difference with `table_continuous()`: a statistic of the OUTCOME
# belongs to a level row, a statistic of the BLOCK to the header row,
# and the other place is a structural blank -- an absence, never the
# `cell_undefined` dash, which means "applies here but has no value".
.outcome_display_df <- function(
  result,
  tokens,
  digits,
  effect_size_digits,
  p_digits,
  decimal_mark,
  ci_level,
  show_statistic,
  show_p,
  show_effect_size,
  show_effect_size_ci,
  indent_text
) {
  fmts <- .continuous_cell_formatters(
    digits,
    effect_size_digits = effect_size_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark
  )
  spec <- .continuous_token_columns(ci_level)
  is_header <- result$.row_role == "factor_header"

  labels <- ifelse(
    result$.row_role == "summary",
    spicy_str("row_overall"),
    ifelse(is_header, result$label, paste0(indent_text, result$level))
  )
  df <- stats::setNames(
    data.frame(labels, stringsAsFactors = FALSE, check.names = FALSE),
    .CON_KEY_VARIABLE
  )
  # The block's own rows carry no descriptive statistic: blank them,
  # rather than let `fmt()` print the undefined dash on an NA that is
  # not a missing value but a different kind of row.
  blank_header <- function(v, token) {
    v[is_header] <- ""
    v
  }
  cells <- .continuous_stat_cells(result, tokens, spec, fmts, blank_header)
  for (nm in names(cells)) {
    df[[nm]] <- cells[[nm]]
  }

  # The inference columns blank themselves: their formatters return ""
  # on NA, and only the header rows carry a value.
  if (show_statistic) {
    df[[.CON_KEY_TEST]] <- vapply(
      seq_len(nrow(result)),
      function(i) {
        tt <- result$test_type[i]
        if (is.na(tt)) {
          tt <- "welch_t"
        }
        fmts$fmt_test(
          tt,
          result$statistic[i],
          result$df1[i],
          result$df2[i],
          decimal_mark
        )
      },
      character(1)
    )
  }
  if (show_p) {
    df[[.CON_KEY_P]] <- vapply(result$p.value, fmts$fmt_p, character(1))
  }
  if (show_effect_size) {
    df[[.CON_KEY_ES]] <- vapply(
      seq_len(nrow(result)),
      function(i) {
        fmts$fmt_es(
          result$es_type[i],
          result$es_value[i],
          result$es_ci_lower[i],
          result$es_ci_upper[i],
          show_effect_size_ci
        )
      },
      character(1)
    )
  }
  df
}

# Internal: the note under an outcome table, opened with "Note.".
#
# In reading order: what left the table, then how it was weighted, then
# how the blocks were compared and how their p-values were adjusted, then
# what the columns mean, then the two sentences this shape owes its
# reader.
.outcome_note <- function(
  outcome_name,
  outcome_label,
  n_outcome_na,
  n_user_na,
  select_na_dropped,
  weights_name,
  n_na_weights,
  weighted,
  tokens,
  result,
  ci_level,
  decimal_mark,
  test_used,
  n_groups,
  var_labels,
  show_comparison,
  overall,
  p_adjust_info = NULL
) {
  parts <- character(0)
  if (n_outcome_na > 0L) {
    parts <- c(
      parts,
      paste0(
        spicy_str("note_missing_removed"),
        spicy_fmt("note_missing_item", outcome_label, n_outcome_na),
        "."
      )
    )
  }
  if (n_user_na > 0L) {
    parts <- c(
      parts,
      paste0(
        spicy_str("note_declared_missing_removed"),
        spicy_fmt("note_missing_item", outcome_label, n_user_na),
        "."
      )
    )
  }
  # One occurrence per `select` variable: the reader must be able to see
  # which block lost which rows. By position: the names are the blocks'
  # labels, and two blocks may share one.
  for (i in seq_along(select_na_dropped)) {
    if (select_na_dropped[[i]] > 0L) {
      parts <- c(
        parts,
        spicy_fmt(
          "note_rows_missing_by_removed",
          names(select_na_dropped)[[i]],
          select_na_dropped[[i]]
        )
      )
    }
  }
  if (n_na_weights > 0L) {
    parts <- c(
      parts,
      spicy_fmt(
        "note_rows_missing_weights",
        weights_name %||% spicy_str("note_weights_fallback"),
        n_na_weights
      )
    )
  }
  if (weighted) {
    parts <- c(
      parts,
      spicy_fmt(
        "note_weighted_by",
        weights_name %||% spicy_str("note_weights_fallback")
      )
    )
  }
  if (show_comparison) {
    parts <- c(parts, .outcome_test_note(test_used, n_groups, var_labels))
  }
  parts <- c(parts, .desc_p_adjust_note(p_adjust_info))
  parts <- c(
    parts,
    build_column_glosses(tokens, result, ci_level, decimal_mark)
  )
  parts <- c(
    parts,
    .outcome_structure_notes(outcome_label, show_comparison, overall)
  )
  # The note every route prints, so it opens with "Note." here, once.
  .note_with_prefix(paste_note_parts(parts))
}

# Internal: the group-comparison disclosure of an outcome table.
#
# NOT `build_test_note()`. That one takes a SCALAR group count -- the
# sibling's blocks are variables compared across one grouping, so they
# all have the same number of groups -- and it only speaks when a
# variable was switched onto the rank family. Here every block has its
# own level count, so `test = "welch"` can be the Welch t-test in one
# block and the Welch one-way ANOVA in the next; the reader is owed
# that, always, not only after a switch. Each block is named as its header
# row names it: `var_labels` is named by column, like `test_used`.
.outcome_test_note <- function(test_used, n_groups, var_labels) {
  ran <- names(test_used)[!is.na(test_used)]
  if (length(ran) == 0L) {
    return(NULL)
  }
  labels <- vapply(
    ran,
    function(nm) continuous_test_label(test_used[[nm]], n_groups[[nm]]),
    character(1)
  )
  if (length(unique(labels)) == 1L) {
    return(spicy_fmt("note_group_comparison", labels[[1L]]))
  }
  by_label <- split(ran, labels)
  spicy_fmt(
    "note_group_comparison",
    paste(
      vapply(
        names(by_label),
        function(lb) {
          spicy_fmt(
            "note_group_comparison_item",
            lb,
            .join_note_vars(unname(var_labels[by_label[[lb]]]))
          )
        },
        character(1)
      ),
      collapse = spicy_str("note_group_comparison_sep")
    )
  )
}
