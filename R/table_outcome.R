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

# ---- the public function --------------------------------------------------

#' Describe one continuous outcome across several groupings
#'
#' @description
#' Summarises ONE continuous outcome across the levels of SEVERAL
#' categorical variables, one block of rows per variable. It is the
#' inverse layout of [table_continuous()], which puts several outcomes
#' in rows and one grouping in columns.
#'
#' Each block reports the outcome's statistics level by level, plus its
#' own group comparison on the block's header row, and an `Overall` row
#' gives the marginal summary of the whole analytic sample.
#'
#' @details
#' # Which shape do I need?
#'
#' Several continuous variables across ONE grouping is
#' [table_continuous()] (`select = `, `by = `). One continuous variable
#' across one or several groupings is this function. A single `by` is
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
#' # Blocks and the group comparison
#'
#' Every block is a separate ONE-WAY comparison of the outcome across
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
#' # Choosing the `by` columns
#'
#' The canonical form is `by = where(is.factor)`, or an explicit
#' enumeration. Negation (`by = -c(x, y)`) is not recommended: it
#' sweeps in every remaining column, and a numeric one opens a block
#' per distinct value, in order of first appearance. A variable
#' producing more than 20 levels raises a warning for that reason --
#' an arbitrary threshold, but a table of sixty one-row blocks is not
#' a table.
#'
#' @param data A data frame.
#' @param outcome The continuous outcome, unquoted or as a string.
#'   Exactly one column.
#' @param by The grouping variables, as a tidyselect expression. One
#'   block of rows per variable, in the order given.
#' @param labels Named character vector of display labels, for the
#'   outcome and for the `by` variables alike.
#' @param overall Show the marginal `Overall` row (default `TRUE`).
#' @param drop_na Drop rows with a missing `by` value from that block
#'   (default `FALSE`: they are shown as a `(Missing)` level and
#'   excluded from the comparison).
#' @param weights,rescale Frequency weights and whether to rescale them
#'   to sum to the sample size, as in [table_continuous()].
#' @param test Group comparison for every block: `"welch"` (default),
#'   `"student"` or `"nonparametric"`.
#' @param p_value Show the p-value column (default `TRUE`).
#' @param statistic Show the test statistic column.
#' @param show_n Show the count column.
#' @param show_columns Character vector of statistic tokens; `NULL`
#'   keeps the historical display.
#' @param effect_size,effect_size_ci Effect size per block and its
#'   confidence interval, as in [table_continuous()].
#' @param ci,ci_level The mean's confidence interval and its level.
#' @param digits,effect_size_digits,p_digits,decimal_mark Number
#'   formatting.
#' @param align Numeric-cell alignment: `"decimal"`, `"center"` or
#'   `"right"`.
#' @param output One of `"default"`, `"data.frame"`, `"long"`, or a
#'   rendering engine: `"tinytable"`, `"gt"`, `"flextable"`,
#'   `"excel"`, `"clipboard"`, `"word"`.
#' @param indent_text,indent_text_excel_clipboard Level-row
#'   indentation, for the console and for the plain-text engines.
#' @param excel_path,excel_sheet,clipboard_delim,word_path Output
#'   destinations, as in [table_continuous()].
#' @param user_na Honour declared missing values (see `?freq`).
#' @param style A journal style; see [spicy_style()].
#'
#' @return A `spicy_outcome_table`: the compute frame, with the display
#'   frame and the typed view attached. `output = "data.frame"` /
#'   `"long"` returns the compute frame unclassed.
#'
#' @seealso [table_continuous()] for the transposed shape,
#'   [table_categorical()] for categorical outcomes.
#' @export
#'
#' @examples
#' table_outcome(sochealth, bmi, by = c(sex, smoking))
#' table_outcome(sochealth, wellbeing_score, by = where(is.factor))
table_outcome <- function(
  data,
  outcome,
  by,
  labels = NULL,
  overall = TRUE,
  drop_na = FALSE,
  weights = NULL,
  rescale = FALSE,
  test = c("welch", "student", "nonparametric"),
  p_value = NULL,
  statistic = FALSE,
  show_n = TRUE,
  show_columns = NULL,
  effect_size = c(
    "none",
    "auto",
    "hedges_g",
    "eta_sq",
    "r_rb",
    "epsilon_sq"
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
  style = NULL
) {
  # A journal / locale style only moves DEFAULTS (see `?spicy_style`).
  .style_pushed <- .style_begin(style, match.call(), environment())
  on.exit(.style_end(.style_pushed), add = TRUE)

  # --- scalar validation, shared with the sibling ------------------------
  if (!is.data.frame(data)) {
    spicy_abort("`data` must be a data.frame.", class = "spicy_invalid_data")
  }
  if (
    !is.numeric(ci_level) ||
      length(ci_level) != 1L ||
      is.na(ci_level) ||
      ci_level <= 0 ||
      ci_level >= 1
  ) {
    spicy_abort(
      "`ci_level` must be a single number between 0 and 1.",
      class = "spicy_invalid_input"
    )
  }
  for (.dname in c("digits", "effect_size_digits")) {
    .dval <- get(.dname)
    if (
      !is.numeric(.dval) || length(.dval) != 1L || is.na(.dval) || .dval < 0
    ) {
      spicy_abort(
        sprintf("`%s` must be a single non-negative number.", .dname),
        class = "spicy_invalid_input"
      )
    }
  }
  digits <- as.integer(digits)
  effect_size_digits <- as.integer(effect_size_digits)
  if (
    !is.numeric(p_digits) ||
      length(p_digits) != 1L ||
      is.na(p_digits) ||
      p_digits < 1
  ) {
    spicy_abort(
      "`p_digits` must be a single integer >= 1 (typically 2-4).",
      class = "spicy_invalid_input"
    )
  }
  p_digits <- as.integer(p_digits)
  if (!.is_single_char(decimal_mark)) {
    spicy_abort(
      '`decimal_mark` must be a single character (e.g. "." or ",").',
      class = "spicy_invalid_input"
    )
  }
  if (!is.null(labels) && (!is.character(labels) || is.null(names(labels)))) {
    spicy_abort(
      "`labels` must be a named character vector.",
      class = "spicy_invalid_input"
    )
  }
  for (.lname in c(
    "overall",
    "drop_na",
    "statistic",
    "effect_size_ci",
    "show_n",
    "ci",
    "user_na"
  )) {
    .lval <- get(.lname)
    if (!is.logical(.lval) || length(.lval) != 1L || is.na(.lval)) {
      spicy_abort(
        sprintf("`%s` must be TRUE/FALSE.", .lname),
        class = "spicy_invalid_input"
      )
    }
  }
  if (is.logical(effect_size)) {
    if (length(effect_size) != 1L || is.na(effect_size)) {
      spicy_abort(
        "`effect_size` must be a single TRUE/FALSE or character value.",
        class = "spicy_invalid_input"
      )
    }
    effect_size <- if (isTRUE(effect_size)) "auto" else "none"
  }
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

  # --- the outcome: exactly one continuous column ------------------------
  outcome_names <- resolve_multi_column_selection(
    rlang::enquo(outcome),
    data,
    "outcome"
  )
  .outcome_check_membership(outcome_names, data, "outcome")
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
  by_names <- resolve_multi_column_selection(rlang::enquo(by), data, "by")
  if (length(by_names) == 0L) {
    spicy_abort(
      c(
        "`by` must select at least one column in `data`.",
        "i" = "`by = where(is.factor)` selects the categorical columns."
      ),
      class = "spicy_invalid_input"
    )
  }
  # `resolve_multi_column_selection()` returns a character vector
  # UNCHECKED (its tidyselect validation lives in the branch a
  # character vector never reaches). Harmless for an optional argument,
  # not for the one that drives the whole table: a typo would travel as
  # `data[["sexe"]] == NULL` and fail far from its cause.
  .outcome_check_membership(by_names, data, "by")
  if (outcome_name %in% by_names) {
    spicy_abort(
      c(
        sprintf(
          "`by` cannot contain the outcome (%s).",
          .quote_val(outcome_name)
        ),
        "i" = "A variable cannot be described across its own levels."
      ),
      class = "spicy_invalid_input"
    )
  }
  .check_integer64_columns(data, c(outcome_name, by_names), "table_outcome")

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
  by_list <- lapply(by_names, function(nm) resolve_user_na(data[[nm]]))
  names(by_list) <- by_names

  # A grouping with too many levels is not refused -- the family never
  # refuses a numeric `by` -- but it is announced: sixty one-row blocks
  # is not a table, and the usual cause is a continuous column swept in
  # by a negation.
  for (nm in by_names) {
    k <- length(unique(stats::na.omit(as.character(by_list[[nm]]))))
    if (k > .OUTCOME_CARDINALITY_WARN) {
      spicy_warn(
        c(
          sprintf("`%s` has %d levels; is it a categorical variable?", nm, k),
          "i" = "`by = where(is.factor)` selects the categorical columns."
        ),
        class = "spicy_caveat"
      )
    }
  }

  # --- weights (decision 17) ---------------------------------------------
  weights_quo <- rlang::enquo(weights)
  weights_name <- detect_weights_column_name(weights_quo, data)
  weights_vec <- resolve_weights_argument(weights_quo, data, "weights")
  if (missing(rescale)) {
    rescale <- getOption("spicy.rescale", FALSE)
  }
  if (!is.logical(rescale) || length(rescale) != 1L || is.na(rescale)) {
    spicy_abort(
      "`rescale` must be TRUE or FALSE.",
      class = "spicy_invalid_input"
    )
  }

  # --- what the table shows and what it tests -----------------------------
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
  all_labels <- resolve_variable_labels(data, c(outcome_name, by_names), labels)
  names(all_labels) <- c(outcome_name, by_names)
  outcome_label <- all_labels[[outcome_name]]

  # --- compute ------------------------------------------------------------
  result <- .outcome_compute(
    outcome = outcome_vec,
    by_list = by_list,
    by_labels = all_labels[by_names],
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
  by_na_dropped <- attr(result, "by_na_dropped")
  test_used <- attr(result, "test_used")
  n_groups <- attr(result, "n_groups")
  missing_labels <- attr(result, "missing_labels")

  n_na_weights <- if (is.null(weights_vec)) 0L else sum(is.na(weights_vec))
  note <- .outcome_note(
    outcome_name = outcome_name,
    outcome_label = outcome_label,
    n_outcome_na = n_outcome_na,
    n_user_na = n_user_na,
    by_na_dropped = by_na_dropped,
    weights_name = weights_name,
    n_na_weights = n_na_weights,
    weighted = !is.null(weights_vec),
    tokens = tokens,
    result = result,
    ci_level = ci_level,
    decimal_mark = decimal_mark,
    test_used = test_used,
    n_groups = n_groups,
    show_comparison = do_test && (p_value || statistic || do_es),
    overall = overall
  )

  if (output %in% c("data.frame", "long")) {
    attributes(result) <- attributes(result)[
      names(attributes(result)) %in% c("names", "row.names", "class")
    ]
    return(result)
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
  attr(result, "by") <- by_names
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
  class(result) <- c("spicy_outcome_table", "data.frame")

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

  if (!identical(output, "default")) {
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
  attr(result, "structured") <- .build_outcome_structured(
    result = result,
    display_df = display_df,
    tokens = tokens,
    digits = digits,
    effect_size_digits = effect_size_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    ci_level = ci_level
  )
  result
}

# The level count beyond which a `by` variable is announced as
# suspicious. Arbitrary and stated as such in the Rd: the family has no
# other threshold, and this one exists because a table of sixty
# one-row blocks is not a table.
.OUTCOME_CARDINALITY_WARN <- 20L

# Internal: every name a selection produced must be a column of `data`.
#
# `resolve_multi_column_selection()` hands a character vector back
# unchecked -- its `tidyselect::eval_select()` validation lives in the
# branch a character vector never reaches. Harmless for an optional
# argument; not for the two that drive this table.
.outcome_check_membership <- function(names_sel, data, arg) {
  miss <- setdiff(names_sel, names(data))
  if (length(miss) == 0L) {
    return(invisible(NULL))
  }
  spicy_abort(
    c(
      sprintf(
        "`%s` names %s, which %s not %s of `data`.",
        arg,
        paste(.quote_val(miss), collapse = ", "),
        if (length(miss) == 1L) "is" else "are",
        if (length(miss) == 1L) "a column" else "columns"
      ),
      "i" = paste0(
        "Available: ",
        paste(.quote_val(names(data)), collapse = ", "),
        "."
      )
    ),
    class = "spicy_invalid_input"
  )
}

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
    .outcome_overall_label(),
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

# Internal: the note under an outcome table.
#
# In reading order: what left the table, then how it was weighted, then
# how the blocks were compared, then what the columns mean, then the
# two sentences this shape owes its reader.
.outcome_note <- function(
  outcome_name,
  outcome_label,
  n_outcome_na,
  n_user_na,
  by_na_dropped,
  weights_name,
  n_na_weights,
  weighted,
  tokens,
  result,
  ci_level,
  decimal_mark,
  test_used,
  n_groups,
  show_comparison,
  overall
) {
  parts <- character(0)
  if (n_outcome_na > 0L) {
    parts <- c(
      parts,
      paste0(
        spicy_str("note_missing_removed"),
        spicy_fmt("note_missing_item", outcome_name, n_outcome_na),
        "."
      )
    )
  }
  if (n_user_na > 0L) {
    parts <- c(
      parts,
      paste0(
        spicy_str("note_declared_missing_removed"),
        spicy_fmt("note_missing_item", outcome_name, n_user_na),
        "."
      )
    )
  }
  # One occurrence per `by` variable: the reader must be able to see
  # which block lost which rows.
  for (nm in names(by_na_dropped)) {
    if (by_na_dropped[[nm]] > 0L) {
      parts <- c(
        parts,
        spicy_fmt("note_rows_missing_by_removed", nm, by_na_dropped[[nm]])
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
    parts <- c(parts, .outcome_test_note(test_used, n_groups))
  }
  parts <- c(
    parts,
    build_column_glosses(tokens, result, ci_level, decimal_mark)
  )
  parts <- c(
    parts,
    .outcome_structure_notes(outcome_label, show_comparison, overall)
  )
  paste_note_parts(parts)
}

# Internal: the group-comparison disclosure of an outcome table.
#
# NOT `build_test_note()`. That one takes a SCALAR group count -- the
# sibling's blocks are variables compared across one grouping, so they
# all have the same number of groups -- and it only speaks when a
# variable was switched onto the rank family. Here every block has its
# own level count, so `test = "welch"` can be the Welch t-test in one
# block and the Welch one-way ANOVA in the next; the reader is owed
# that, always, not only after a switch.
.outcome_test_note <- function(test_used, n_groups) {
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
            paste(by_label[[lb]], collapse = ", ")
          )
        },
        character(1)
      ),
      collapse = "; "
    )
  )
}
