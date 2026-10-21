# -- Internal helpers for per-row association measure -------------------------

# Pretty label for an association measure. Always ASCII so the
# resulting string is safe to use as a data.frame column name (the
# `out[["Kendall's Tau-b"]]` contract works on every platform) and as
# a `glance()` value. A locale-aware Unicode upgrade (`tau`, `gamma`)
# could be added later as a display-only print option without
# affecting these data-side names.
.assoc_label <- function(measure) {
  switch(
    measure,
    cramer_v = spicy_str("stat_cramer_v"),
    phi = spicy_str("stat_phi"),
    tau_b = spicy_str("stat_tau_b"),
    tau_c = spicy_str("stat_tau_c"),
    gamma = spicy_str("stat_gamma"),
    somers_d = spicy_str("stat_somers_d"),
    lambda = spicy_str("stat_lambda"),
    measure
  )
}

# Frozen twin of `.assoc_label()`: the COLUMN NAME a measure gives the
# public wide output. Same switch, English literals typed here rather
# than read from the registry -- the programmatic contract never follows
# the display language (decision 13). The two tables must agree at the
# English default; nothing but the pin in test-i18n.R can catch a drift.
.assoc_key <- function(measure) {
  switch(
    measure,
    cramer_v = "Cramer's V",
    phi = "Phi",
    tau_b = "Kendall's Tau-b",
    tau_c = "Stuart's Tau-c",
    gamma = "Goodman-Kruskal Gamma",
    somers_d = "Somers' D",
    lambda = "Lambda",
    measure
  )
}

# Every measure key `.assoc_label()` knows, in the order the registry
# lists them. Used to build the defensive note-parsing pattern below from
# the labels themselves, so the pattern can never fall behind them.
.assoc_measure_keys <- c(
  "cramer_v",
  "phi",
  "gamma",
  "tau_b",
  "tau_c",
  "somers_d",
  "lambda"
)

# -- Frozen column keys of the categorical family -----------------------------
#
# The public column name, the `col_meta` index, the value of `long$group`
# and the `total_group` attribute are one frozen English contract that
# user code indexes into (decision 13). These constants name it once so a
# read path can compare key against key instead of re-typing the string.
# The displayed HEADER is a separate layer, resolved from the registry
# into `col_meta$display_label`; the two never share a literal.
#
# `$Variable` / `$p` accessors are left as they are: `$` on a data.frame
# is unmistakably a column access, and spelling it `[[.CAT_KEY_P]]` would
# cost more in noise than it buys in intent.
.CAT_KEY_VARIABLE <- "Variable"
.CAT_KEY_P <- "p"
.CAT_MARGIN_KEY <- "Total"
.CAT_KEY_CI_LL <- "CI lower"
.CAT_KEY_CI_UL <- "CI upper"
.CAT_KEY_EFFECT_SIZE <- "Effect size"
# The standardized-mean-difference column. A NEW KIND of key for this
# family: `.CAT_KEY_EFFECT_SIZE` above is not a fixed column name (the
# measure column is named after the measure when a single one is used,
# and only collapses to the generic name on mixed measures), so "SMD" is
# the first effect column here with a name that never moves -- and it
# sits right beside one that does. Its header is `header_smd`, the ONE
# registry key shared with `.CON_KEY_SMD` of the continuous family.
.CAT_KEY_SMD <- "SMD"

# The `<group> n` / `<group> %` key pair. `paste0()`, not `sprintf()`:
# these compose a KEY, so the composition rule must not change with the
# display language -- and `paste0()` recycles a zero-length group vector
# to `" n"` where `sprintf()` would drop it.
.cat_key_n <- function(g) paste0(g, " n")
.cat_key_pct <- function(g) paste0(g, " %")

# Display labels for the same pair. Here the registry decides: these are
# HEADERS, not keys. `sprintf("%s %s", g, "n")` is `paste0(g, " n")`
# character for character at the English default, and a "%" inside a
# group name is safe because it travels as an argument, not a template.
.cat_label_n <- function(g) {
  spicy_fmt("header_group_qualified", g, spicy_str("header_n_lower"))
}

.cat_label_pct <- function(g) {
  spicy_fmt("header_group_qualified", g, spicy_str("header_percent_symbol"))
}

# Display label of a group column. Group levels are DATA and travel
# verbatim; the margin is the one member of `group_levels` that is a word
# of ours, whatever its key (see `.cat_margin_key()`): a level displayed
# with the margin's label is refused before any header is built, so the
# margin never needs another word to stand apart.
#
# One resolver for every engine: split it and the margin would translate
# in some outputs and not in others.
.cat_group_label <- function(g, margin_key = NULL) {
  if (
    length(margin_key) == 1L &&
      !is.na(margin_key) &&
      identical(g, margin_key)
  ) {
    spicy_str("header_margin_total")
  } else {
    g
  }
}

# The KEY of the margin column of `table_categorical()`, its survey twin
# and `table_multiresponse()`, and the refusal of a margin whose label one
# of the group columns displays.
#
# `shown` are the group columns the table displays: the levels of `by`,
# and the column of missing values when there is one (`missing_label`).
# With `check`, a margin labeled like one of them is refused
# (`.abort_label_collision()`): the reader could not tell the two columns
# apart, and a numbered suffix on either would be an artifact.
#
# The key is another matter. It names the columns of the machine outputs
# ("Total n") and is frozen English, so a level "Total" displayed beside a
# margin relabeled through `options(spicy.labels)` still holds the key
# "Total": the margin then takes the first free "Total_<i>", the repair
# `make.unique()` makes on names, displayed under the margin's label and
# recorded where the outputs name it (`total_group`, `margin_key`). The
# repair runs without `check` too, so that a key never matches a group
# the reading code would mistake for the margin.
.cat_margin_key <- function(
  shown,
  by_name,
  check = TRUE,
  missing_label = NA_character_,
  call = rlang::caller_env()
) {
  if (check) {
    label <- spicy_str("header_margin_total")
    if (identical(label, missing_label)) {
      spicy_abort(
        c(
          sprintf(
            "The column spicy adds for the missing values of `%s` and the margin column have the same label, %s.",
            by_name,
            .quote_val(label)
          ),
          "i" = "Give one of them another label: `options(spicy.labels = list(header_margin_total = \"All\"))`."
        ),
        class = c("spicy_label_collision", "spicy_invalid_input"),
        call = call
      )
    }
    if (label %in% shown) {
      .abort_label_collision(
        label,
        by_name,
        "level",
        "the margin column spicy adds",
        "column",
        "header_margin_total",
        "All",
        call = call
      )
    }
  }
  key <- .CAT_MARGIN_KEY
  idx <- 1L
  while (key %in% shown) {
    key <- paste0(.CAT_MARGIN_KEY, "_", idx)
    idx <- idx + 1L
  }
  key
}

# The row of missing values of one variable, before it is appended
# (`.add_missing_level()`): refused when the variable displays a level
# with its label (`.abort_label_collision()`). A declared level with that
# label and no observation is never displayed (the rows show the observed
# levels), so it is dropped instead: the two cannot share one factor.
# `levels_keep` names levels by their label and cannot tell the two rows
# apart, so it does not lift the refusal.
.cat_missing_row_guard <- function(
  x,
  missing_label,
  var,
  call = rlang::caller_env()
) {
  if (!missing_label %in% levels(x)) {
    return(x)
  }
  if (any(x == missing_label, na.rm = TRUE)) {
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
  factor(
    x,
    levels = setdiff(levels(x), missing_label),
    ordered = is.ordered(x)
  )
}

# Coerce a column selected for tabulation to the factor that flows
# through freq() / cross_tab() untouched. The factor is built BEFORE
# tabulation so the declared level order survives end-to-end: the
# pre-0.13.0 as.character() round-trip under `drop_na = FALSE` let
# cross_tab() re-sort the levels alphabetically, silently corrupting
# every order-sensitive association measure (tau-b, tau-c, gamma,
# Somers' d) and stripping haven value labels down to raw codes.
# * factors (ordered or not) pass through verbatim;
# * labelled vectors use the same conversion as freq():
#   "[code] label" levels in code order;
# * anything else becomes a factor in the family's order of the groups
#   (`.group_levels()`): sorted in the C locale, as `freq()` and
#   `cross_tab()` order it.
.tab_factor <- function(x) {
  if (is.factor(x)) {
    return(x)
  }
  if (labelled::is.labelled(x)) {
    return(labelled::to_factor(x, levels = "prefixed", nolabel_to_na = FALSE))
  }
  factor(x, levels = .group_levels(x))
}

# The cells of a group in which a variable has no data hold no number,
# so the numeric body of the sheet leaves them empty: the mark the
# console prints there is written over them, before the cells are styled.
# `display` is the character body the console prints, `cols` the
# positions of its count and percentage columns, and `first_row` the
# sheet row of its first line.
.cat_xl_mark_undefined <- function(wb, display, cols, first_row) {
  mark <- spicy_str("cell_undefined")
  for (j in cols) {
    for (i in which(!is.na(display[[j]]) & display[[j]] == mark)) {
      wb <- openxlsx2::wb_add_data(
        wb,
        x = mark,
        start_col = j,
        start_row = first_row + i - 1L
      )
    }
  }
  wb
}

# Append the "(Missing)" display level -- always LAST -- and recode the
# NA observations to it, without leaving factor territory.
.add_missing_level <- function(f, missing_label) {
  f <- factor(
    f,
    levels = c(levels(f), missing_label),
    ordered = is.ordered(f)
  )
  f[is.na(f)] <- missing_label
  f
}

# Loud guard for `levels_keep`: when the supplied levels match nothing
# for a selected variable, that variable would silently vanish from
# the table. The mismatch is systematic for labelled columns, whose
# internal level strings are the "[code] label" display form -- never
# the bare label text a user naturally supplies. Warn with the exact
# strings that WOULD match; the caller then skips the variable
# (partial matches keep their usual intersect semantics).
.warn_levels_keep_no_match <- function(var_name, available) {
  available <- available[!is.na(available)]
  listed <- if (length(available) == 0L) {
    "(none)"
  } else {
    shown <- utils::head(available, 10L)
    out <- paste0("\"", shown, "\"", collapse = ", ")
    if (length(available) > length(shown)) {
      out <- paste0(out, ", ...")
    }
    out
  }
  spicy_warn(
    paste0(
      "`levels_keep` matches no levels of `",
      var_name,
      "`; the variable is dropped from the table. Available levels: ",
      listed,
      "."
    ),
    class = "spicy_no_selection"
  )
}

# `levels_keep` resolved to one character vector per variable (NULL for a
# variable it does not filter), or NULL when it filters none. Shared by
# `table_categorical()` and `table_categorical_svy()`: a vector applies to
# every variable, a named list to the variables it names, each name a
# variable of the table. `per_variable` records which form was given.
.cat_levels_keep_spec <- function(
  levels_keep,
  select_names,
  call = rlang::caller_env()
) {
  if (is.null(levels_keep)) {
    return(NULL)
  }
  if (is.atomic(levels_keep)) {
    out <- stats::setNames(
      rep(list(as.character(levels_keep)), length(select_names)),
      select_names
    )
    attr(out, "per_variable") <- FALSE
    return(out)
  }
  if (!is.list(levels_keep) || is.null(names(levels_keep))) {
    spicy_abort(
      c(
        "`levels_keep` must be a character vector or a NAMED list.",
        "i" = "A character vector applies to every variable; a named list applies per variable."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  unknown <- setdiff(names(levels_keep), select_names)
  if (length(unknown) > 0L) {
    spicy_abort(
      sprintf(
        "`levels_keep` names variable(s) absent from the table: %s.",
        paste(.quote_val(unknown), collapse = ", ")
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  out <- stats::setNames(vector("list", length(select_names)), select_names)
  for (nm in names(levels_keep)) {
    out[[nm]] <- as.character(levels_keep[[nm]])
  }
  attr(out, "per_variable") <- TRUE
  out
}

# The levels `levels_keep` names for one variable must be levels it
# displays (`available`).
.cat_check_kept_levels <- function(
  keep,
  available,
  var,
  call = rlang::caller_env()
) {
  unknown <- setdiff(keep, available)
  if (length(unknown) > 0L) {
    spicy_abort(
      c(
        sprintf(
          "`levels_keep` names level(s) absent from `%s`: %s.",
          var,
          paste(.quote_val(unknown), collapse = ", ")
        ),
        "i" = sprintf(
          "Observed: %s.",
          paste(.quote_val(available), collapse = ", ")
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(keep)
}

# The levels one variable displays under `levels_keep` (its entry `keep`
# in the resolved spec), in the order `levels_keep` gives them. The
# vector form keeps the levels the variable has, and the caller drops the
# variable, with a warning, when it has none of them. The list form names
# the variable, so a level it does not have (`levels`) is an error, as in
# `table_categorical_svy()`.
.cat_kept_levels <- function(
  spec,
  var,
  levels,
  available = levels,
  call = rlang::caller_env()
) {
  keep <- spec[[var]]
  if (isTRUE(attr(spec, "per_variable"))) {
    .cat_check_kept_levels(keep, levels, var, call = call)
  }
  intersect(keep, available)
}

# Resolve the user-supplied `assoc_measure` into a named character vector
# of length(select_names): one resolved measure per row variable. Accepts
# four input shapes:
#
#   * `"none"`              -> rep("none", N)
#   * single string         -> rep(string, N) (uniform application)
#   * named character vec   -> per-row override; unnamed positions or
#                              missing names fall back to "auto"
#   * unnamed character vec -> positional pair-up with `select_names`
#
# Then resolves each remaining `"auto"` entry based on the variable type:
# 2x2 -> phi ; both ordered -> tau_b ; otherwise cramer_v.
#
# The strict per-row validation (e.g. phi requires a 2x2 table) lives
# here so the user gets a clear, early error instead of a silent NA in
# the cell.
#
# `user_na` must be the same flag the tabulation uses: haven's
# `is.na()` treats declared-missing codes as NA, so counting levels on
# the raw column under `user_na = FALSE` would see fewer levels than
# the table actually has -- the dispatch then picks/validates a
# measure (e.g. phi on a "2x2") that the real 3x2 table refuses
# (audit phase 2, finding 31).
.resolve_assoc_measures <- function(
  assoc_measure,
  select_names,
  data,
  by_name,
  user_na = TRUE
) {
  valid <- c(
    "auto",
    "none",
    "cramer_v",
    "phi",
    "tau_b",
    "tau_c",
    "gamma",
    "somers_d",
    "lambda"
  )

  n <- length(select_names)
  per_row <- character(n)
  names(per_row) <- select_names

  if (is.null(assoc_measure)) {
    assoc_measure <- "auto"
  }
  if (!is.character(assoc_measure)) {
    spicy_abort(
      "`assoc_measure` must be a character string or named/unnamed character vector.",
      class = "spicy_invalid_input"
    )
  }

  has_names <- !is.null(names(assoc_measure)) &&
    any(nzchar(names(assoc_measure)))

  if (length(assoc_measure) == 1L && !has_names) {
    if (!assoc_measure %in% valid) {
      spicy_abort(
        sprintf(
          "`assoc_measure = \"%s\"` is not one of: %s.",
          assoc_measure,
          paste(.quote_val(valid), collapse = ", ")
        ),
        class = "spicy_invalid_input"
      )
    }
    per_row[] <- assoc_measure
  } else if (has_names) {
    bad_names <- setdiff(
      names(assoc_measure)[nzchar(names(assoc_measure))],
      select_names
    )
    if (length(bad_names) > 0L) {
      spicy_abort(
        sprintf(
          "`assoc_measure` keys not found in `select`: %s.",
          paste(.quote_val(bad_names), collapse = ", ")
        ),
        class = "spicy_invalid_input"
      )
    }
    bad_vals <- setdiff(unique(assoc_measure), valid)
    if (length(bad_vals) > 0L) {
      spicy_abort(
        sprintf(
          "`assoc_measure` value(s) not recognized: %s.",
          paste(.quote_val(bad_vals), collapse = ", ")
        ),
        class = "spicy_invalid_input"
      )
    }
    per_row[] <- "auto" # default fallback for unnamed variables
    keyed <- assoc_measure[nzchar(names(assoc_measure))]
    per_row[names(keyed)] <- as.character(keyed)
  } else {
    # Unnamed vector, positional pair-up
    if (length(assoc_measure) != n) {
      spicy_abort(
        sprintf(
          "Unnamed `assoc_measure` has length %d but `select` chose %d variable%s. Either pass a named vector keyed by variable name, or match the lengths.",
          length(assoc_measure),
          n,
          if (n > 1L) "s" else ""
        ),
        class = "spicy_invalid_input"
      )
    }
    bad_vals <- setdiff(unique(assoc_measure), valid)
    if (length(bad_vals) > 0L) {
      spicy_abort(
        sprintf(
          "`assoc_measure` value(s) not recognized: %s.",
          paste(.quote_val(bad_vals), collapse = ", ")
        ),
        class = "spicy_invalid_input"
      )
    }
    per_row[] <- as.character(assoc_measure)
  }

  # Resolve each remaining "auto" based on the variable / by-variable
  # type. Level counts must mirror the tabulation's `user_na` regime
  # (see the roxygen comment above).
  .count_levels <- function(v) {
    v <- if (isTRUE(user_na)) .user_na_to_na(v) else .user_na_zap(v)
    length(unique(v[!is.na(v)]))
  }
  by_var <- data[[by_name]]
  by_n_levels <- .count_levels(by_var)
  by_ordered <- is.ordered(by_var)

  for (i in seq_along(per_row)) {
    if (per_row[i] != "auto") {
      next
    }
    var <- data[[select_names[i]]]
    var_n_levels <- .count_levels(var)
    var_ordered <- is.ordered(var)
    per_row[i] <- if (var_n_levels == 2L && by_n_levels == 2L) {
      "phi"
    } else if (var_ordered && by_ordered) {
      "tau_b"
    } else {
      "cramer_v"
    }
  }

  # Strict applicability check: phi only on 2x2.
  for (i in seq_along(per_row)) {
    if (per_row[i] != "phi") {
      next
    }
    var <- data[[select_names[i]]]
    var_n_levels <- .count_levels(var)
    if (var_n_levels != 2L || by_n_levels != 2L) {
      spicy_abort(
        sprintf(
          "`assoc_measure[\"%s\"] = \"phi\"` requires a 2x2 table, but `%s` x `by` is %dx%d.",
          select_names[i],
          select_names[i],
          var_n_levels,
          by_n_levels
        ),
        class = "spicy_unsupported"
      )
    }
  }

  per_row
}

# Build the sentences listing which measure was used for which variable
# when the rows of a `table_categorical()` table use more than one
# association measure (the column then takes the generic header), one
# sentence per measure. A variable with no value at all (`no_value`: no
# data in any group) has no measure, and is not named; the others still
# are, since the header stays generic. It carries no "Note." prefix: the
# note opens with it once, in `.categorical_note()`, whatever sentence
# comes first.
#
# Example output:
#   "Cramer's V: smoking, education. Kendall's Tau-b: self_rated_health."
.assoc_note_apa <- function(per_row_measures, labels, no_value = character(0)) {
  shown <- per_row_measures[per_row_measures != "none"]
  if (length(unique(unname(shown))) <= 1L) {
    return(NULL)
  }
  named <- shown[!names(shown) %in% no_value]
  if (length(named) == 0L) {
    return(NULL)
  }
  parts <- vapply(
    unique(unname(named)),
    function(m) {
      vars <- names(named)[named == m]
      lab <- labels[match(vars, names(per_row_measures))]
      spicy_fmt(
        "note_assoc_measure_item",
        .assoc_label(m),
        .join_note_vars(lab)
      )
    },
    character(1)
  )
  paste(parts, collapse = " ")
}

# -- Internal helpers for the per-variable test ------------------------------
#
# `test` chooses, per variable, between Pearson's chi-squared test and
# Fisher's exact test (decision of 2026-10-18), and for a 2 x 2 table the
# Fisher mid-p test or Campbell's N-1 chi-squared (its complement, the
# same day). The helpers below are shared with `table_multiresponse()`,
# whose item tests follow the same rule.

# The frozen test keys of the data frames (`test_type`), never a display
# string: the note words them through the registry. `chi_squared` is the
# value `glance()` has published since 0.13.0.
.CAT_TEST_CHISQ <- "chi_squared"
.CAT_TEST_N1 <- "n1_chi_squared"
.CAT_TEST_FISHER <- "fisher_exact"
.CAT_TEST_MIDP <- "fisher_midp"
.CAT_TEST_FISHER_MC <- "fisher_exact_mc"
# The order a note names them in, whatever the order of the variables.
.CAT_TEST_KEYS <- c(
  .CAT_TEST_CHISQ,
  .CAT_TEST_N1,
  .CAT_TEST_FISHER,
  .CAT_TEST_MIDP,
  .CAT_TEST_FISHER_MC
)

# The contingency table a variable is tested on: its observed levels by the
# observed groups, weighted, with the empty rows and columns dropped. It is
# the table `cross_tab()` hands `chisq.test()`, built the same way (rows
# with a missing weight out, `rescale` applied on the complete cases, empty
# margins pruned), so the expected counts the rule reads and the table the
# exact test conditions on are those of the chi-squared test.
.cat_test_table <- function(x, g, w = NULL, rescale = FALSE) {
  ok <- !is.na(x) & !is.na(g)
  if (!is.null(w)) {
    ok <- ok & !is.na(w)
  }
  ww <- if (is.null(w)) rep(1, sum(ok)) else w[ok]
  if (isTRUE(rescale) && !is.null(w) && sum(ww) > 0) {
    ww <- ww * length(ww) / sum(ww)
  }
  tab <- tapply(ww, list(x[ok], g[ok]), sum, default = 0)
  tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
}

# The test a table takes. `"auto"` is the rule of gtsummary's `add_p()` for
# a categorical variable (`.add_p_tbl_summary_default_test()`,
# R/assign_tests.R, lines 330-357 of gtsummary 2.6.1): the chi-squared test
# when every expected count is at least 5, Fisher's exact test otherwise.
# Two differences, both at the edge of the rule: the expected counts are
# read on the table the test runs on (a declared level nobody holds, which
# gtsummary's `table()` keeps as an empty row, has no expected count to be
# small), and they are `chisq.test()`'s own, `outer(sr, sc) / n`.
# `"midp"` and `"n1"` are tests of a 2 x 2 table: any other table follows
# `"auto"`. `"n1"` is the rule of Campbell (2007, pp. 18-19): the N-1
# chi-squared when every expected count is at least 1, Fisher's exact test
# otherwise.
.cat_resolve_test <- function(tab, test) {
  if (test %in% c("midp", "n1") && !all(dim(tab) == 2L)) {
    test <- "auto"
  }
  if (test %in% c("chisq", "fisher", "midp")) {
    return(test)
  }
  expected <- outer(rowSums(tab), colSums(tab)) / sum(tab)
  if (identical(test, "n1")) {
    return(if (all(expected >= 1)) "n1" else "fisher")
  }
  if (all(expected >= 5)) "chisq" else "fisher"
}

# Fisher's exact test conditions on counts: a weighted table with
# fractional cells has none to condition on.
.cat_is_count_table <- function(tab) {
  all(abs(tab - round(tab)) <= 1e-7)
}

# Fisher's exact test of an r x c table (the Freeman-Halton extension past
# 2 x 2), from `stats::fisher.test()` and its default workspace. When the
# network algorithm cannot enumerate the table in it -- FEXACT runs out on
# large sparse tables, with a message that depends on the locale -- the
# p-value is the Monte Carlo estimate of the same test over `B` tables
# drawn with the observed margins, and the key says so. That estimate draws
# from the session's RNG.
.cat_fisher_p <- function(tab, B) {
  tab <- round(tab)
  exact <- tryCatch(stats::fisher.test(tab)$p.value, error = function(e) NULL)
  if (!is.null(exact)) {
    return(list(p = exact, type = .CAT_TEST_FISHER))
  }
  list(
    p = stats::fisher.test(tab, simulate.p.value = TRUE, B = B)$p.value,
    type = .CAT_TEST_FISHER_MC
  )
}

# Fisher's mid-p test of a 2 x 2 table, two-sided as Fagerland, Lydersen
# and Laake (2017, Eq. 1.12 p. 28, section 4.4.6 p. 97) and Lydersen,
# Fagerland and Laake (2009, Eq. 12 p. 1165) define it: the tables with
# the observed margins are ordered by their hypergeometric probability,
# and the p-value sums the probabilities of the tables less probable than
# the observed one plus half those of the tables as probable, the observed
# one included. Ties are read with the relative tolerance of
# `stats::fisher.test()`, without which a table and its mirror image can
# part company.
.cat_midp_p <- function(tab) {
  tab <- round(tab)
  m <- sum(tab[, 1L])
  n <- sum(tab[, 2L])
  k <- sum(tab[1L, ])
  support <- max(0, k - n):min(k, m)
  d <- stats::dhyper(support, m, n, k)
  d_obs <- d[support == tab[1L, 1L]]
  rel_err <- 1 + 1e-7
  less <- d < d_obs / rel_err
  tie <- !less & d <= d_obs * rel_err
  sum(d[less]) + 0.5 * sum(d[tie])
}

# The N-1 chi-squared of a 2 x 2 table (Campbell 2007, test 3 p. 8):
# Pearson's statistic, with no continuity correction, times (N - 1) / N,
# referred to a chi-squared with 1 degree of freedom. It equals the
# Mantel-Haenszel statistic of a single 2 x 2 table.
.cat_n1_test <- function(tab) {
  n <- sum(tab)
  x2 <- n *
    (tab[1L, 1L] * tab[2L, 2L] - tab[1L, 2L] * tab[2L, 1L])^2 /
    prod(rowSums(tab), colSums(tab))
  statistic <- x2 * (n - 1) / n
  list(
    statistic = statistic,
    p = stats::pchisq(statistic, df = 1, lower.tail = FALSE)
  )
}

# The test `.cat_resolve_test()` chose, when it is not the Pearson
# chi-squared (which `cross_tab()` computes): its p-value, its key, and
# the statistic and degrees of freedom of a chi-squared (NA for an exact
# test). A table with an empty row or column is uninformative (Lydersen
# et al. 2009, p. 1162): no test, as for the chi-squared.
.cat_run_test <- function(tab, kind, B) {
  if (any(rowSums(tab) == 0) || any(colSums(tab) == 0)) {
    return(list(
      p = NA_real_,
      chi2 = NA_real_,
      df = NA_real_,
      type = NA_character_
    ))
  }
  if (identical(kind, "n1")) {
    n1 <- .cat_n1_test(tab)
    return(list(p = n1$p, chi2 = n1$statistic, df = 1, type = .CAT_TEST_N1))
  }
  if (identical(kind, "midp")) {
    return(list(
      p = .cat_midp_p(tab),
      chi2 = NA_real_,
      df = NA_real_,
      type = .CAT_TEST_MIDP
    ))
  }
  fisher <- .cat_fisher_p(tab, B)
  list(p = fisher$p, chi2 = NA_real_, df = NA_real_, type = fisher$type)
}

# The name a note gives a test key. A simulated chi-squared keeps the
# historical " (simulated)" of `cross_tab()`; the Monte Carlo exact test
# names its replicates. Inside a list of tests (`in_list`), where the
# variables follow in parentheses, those two qualifiers take a comma
# instead, so no item reads "test (simulated) (sex)".
.cat_test_label <- function(
  type,
  simulated = FALSE,
  B = NULL,
  in_list = FALSE
) {
  if (identical(type, .CAT_TEST_CHISQ)) {
    if (!simulated) {
      return(spicy_str("test_pearson_chisq"))
    }
    if (in_list) {
      return(spicy_str("test_pearson_chisq_simulated_in_list"))
    }
    return(paste0(
      spicy_str("test_pearson_chisq"),
      spicy_str("note_chisq_simulated")
    ))
  }
  if (identical(type, .CAT_TEST_N1)) {
    return(spicy_str("test_n1_chisq"))
  }
  if (identical(type, .CAT_TEST_FISHER)) {
    return(spicy_str("test_fisher_exact"))
  }
  if (identical(type, .CAT_TEST_MIDP)) {
    return(spicy_str("test_fisher_midp"))
  }
  spicy_fmt(
    if (in_list) "test_fisher_mc_in_list" else "test_fisher_mc",
    as.character(B)
  )
}

# The tests behind a set of p-values, worded for a note on the model of
# `table_continuous()`: the test's name alone when one test covers every
# row, or each test followed by the rows it covers, "Pearson chi-squared
# test (sex, education); Fisher exact test (smoking)". `types` holds the
# test keys (NA where nothing was tested), `labels` the row names the
# table prints. `single` says which of the two forms came out; NULL when
# nothing was tested.
.cat_test_phrase <- function(types, labels, simulated = FALSE, B = NULL) {
  ran <- !is.na(types)
  if (!any(ran)) {
    return(NULL)
  }
  # Locals, not the formals rebound: `labels` is a display formal, which
  # the shadowing lint forbids reassigning.
  tested <- types[ran]
  rows <- labels[ran]
  used <- intersect(.CAT_TEST_KEYS, tested)
  if (length(used) == 1L) {
    return(list(
      single = TRUE,
      text = .cat_test_label(used, simulated, B)
    ))
  }
  items <- vapply(
    used,
    function(k) {
      spicy_fmt(
        "note_group_comparison_item",
        .cat_test_label(k, simulated, B, in_list = TRUE),
        .join_note_vars(rows[tested == k])
      )
    },
    character(1)
  )
  list(
    single = FALSE,
    text = paste(items, collapse = spicy_str("note_group_comparison_sep"))
  )
}


#' Categorical summary table
#'
#' @description
#' Builds a publication-ready frequency or cross-tabulation table for one
#' or many categorical variables selected with tidyselect syntax.
#'
#' With `by`, produces grouped cross-tabulation summaries (using
#' [cross_tab()] internally) with a *p*-value per variable, from Pearson's
#' chi-squared test or Fisher's exact test (see `test`), and optional
#' association measures.
#' Without `by`, produces one-way frequency-style summaries.
#'
#' Multiple output formats are available via `output`: a printed ASCII
#' table (`"default"`), a numeric `data.frame` (`"data.frame"`, the
#' displayed table, or `"long"`, one row per variable, level and group),
#' or publication-ready tables (`"tinytable"`, `"gt"`, `"flextable"`,
#' `"excel"`, `"clipboard"`, `"word"`).
#'
#' @param data A data frame.
#' @param select Columns to include as row variables. Supports tidyselect
#'   syntax and character vectors of column names. When omitted,
#'   defaults to every eligible categorical column in `data`: factor,
#'   character, logical, and labelled (`haven` / `labelled`) columns,
#'   excluding the `by` column -- matching the select-less defaults of
#'   [table_continuous()] and [table_continuous_lm()]. An explicit
#'   `select` is taken verbatim (numeric columns included), so
#'   numeric-coded categorical variables can still be tabulated by
#'   naming them. The `by` and `weights` columns are not part of the
#'   selection: `everything()` and the other helpers skip them, and
#'   naming one in `select`, or giving its position, is an error. A name
#'   that is not a column of `data` is an error (class
#'   `spicy_missing_column`), and so is a selection with no column to
#'   tabulate. The levels of a variable are its rows in the order `by`
#'   gives its groups (see `by`); a labelled variable keeps the order of
#'   its codes.
#' @param by Optional grouping column used for columns/groups. Accepts an
#'   unquoted column name or a single character column name. Factor
#'   levels keep their declared order, which is the way to choose another
#'   order; any other `by` is sorted, as in [freq()] and [cross_tab()]:
#'   text in the C locale (capitals before lower case, the same on every
#'   machine), `FALSE` before `TRUE`, numbers and the codes of a haven
#'   labelled column ascending, whatever the order of the rows. The
#'   column of missing values comes last. For a haven labelled `by`, the
#'   group headers are the raw codes (value labels are not used for group
#'   headers -- the family convention shared with [table_continuous()]
#'   and [table_continuous_lm()]); declared missing values follow
#'   `user_na` as usual.
#' @param labels How each variable is named in the table: in the rows,
#'   in the title, and in the notes. Four forms:
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
#' @param levels_keep Levels to keep, in the order to display them. If
#'   `NULL` (the default), all observed levels are kept. Entries must
#'   match the level strings the table displays (for labelled columns
#'   these are the `"[code] label"` strings, not the bare label text).
#'   Two forms, as in [table_categorical_svy()]:
#'   * a character vector applies to every variable: each keeps the
#'     levels it has, and a variable with none of them is dropped from
#'     the table with a classed warning (`spicy_no_selection`) listing
#'     its level strings;
#'   * a named list applies per variable
#'     (`levels_keep = list(smoking = "Yes")`): each name must be a
#'     variable of the table, each level one the variable displays (an
#'     error otherwise), and the variables it does not name keep all
#'     their levels.
#' @param include_total Logical. If `TRUE` (the default), includes a `Total` group
#'   when available. A level of `by` displayed with the label of that
#'   margin is then an error (class `spicy_label_collision`): rename the
#'   level, or give the margin another label with
#'   `options(spicy.labels = list(header_margin_total = "All"))`.
#' @param drop_na Logical. The rule of the family: a table that counts
#'   shows the missing values by default, as a frequency table reports
#'   how many answers are missing; a table that computes a statistic
#'   removes them by default and counts them in the note (see
#'   [table_continuous()]). This table counts, so `FALSE` (the default)
#'   displays the missing values as a dedicated `"(Missing)"` level (and,
#'   under `by`, a `"(Missing)"` group column) -- the field convention for
#'   descriptive tables (gtsummary's "Unknown" row, janitor's `NA` row;
#'   see the Epidemiologist R Handbook, Descriptive tables). A level
#'   displayed beside that row or column with its label is an error
#'   (class `spicy_label_collision`): rename the level, or give the row
#'   another label with
#'   `options(spicy.labels = list(row_missing_level = "(Not stated)"))`.
#'   If `TRUE`, rows with `NA` in the tabulated variable (and in `by`,
#'   when supplied) are removed BEFORE each cross-tabulation, and the
#'   removal is disclosed in a table note ("Missing values removed: ...")
#'   rather than silent; a `by` that no observation holds is then an
#'   error (class `spicy_invalid_data`).
#' @param weights Optional weights. Either `NULL` (the default), a numeric vector
#'   of length `nrow(data)`, or a single column in `data` supplied as an
#'   unquoted name or a character string.
#' @param rescale Logical. If `FALSE`, weights are used as-is.
#'   If `TRUE`, rescales weights so total weighted N matches raw N.
#'   Passed to `spicy::cross_tab()`. The default reads
#'   `options(spicy.rescale)`, `FALSE` when it is unset, matching
#'   [cross_tab()].
#' @param correct Logical. If `FALSE` (the default), no continuity correction is
#'   applied. If `TRUE`, applies Yates correction in 2x2 chi-squared contexts.
#'   Passed to `spicy::cross_tab()`. A variable tested by Fisher's exact
#'   test, its mid-p version, or the N-1 chi-squared (see `test`) takes no
#'   correction.
#' @param simulate_p Logical. If `FALSE` (the default), uses asymptotic p-values.
#'   If `TRUE`, uses Monte Carlo simulation. Passed to `spicy::cross_tab()`.
#'   Applies to the Pearson chi-squared test only (see `test`).
#' @param simulate_B Integer. Number of Monte Carlo replicates when
#'   `simulate_p = TRUE`, and for the Monte Carlo version of Fisher's exact
#'   test (see `test`). Defaults to `2000`.
#' @param test Character. The test of each variable when `by` is used.
#'   One of `"auto"` (default), `"chisq"`, `"fisher"`, `"midp"`, or
#'   `"n1"`.
#'   - `"auto"`: Pearson's chi-squared test when every expected count of
#'     the variable's table is at least 5, Fisher's exact test otherwise,
#'     the rule of gtsummary's `add_p()`. The choice is made per variable,
#'     and the note names the test of each.
#'   - `"chisq"`: Pearson's chi-squared test for every variable, the
#'     behavior before 0.14.0.
#'   - `"fisher"`: Fisher's exact test for every variable.
#'   - `"midp"`: the Fisher mid-p test for a 2x2 table.
#'   - `"n1"`: the N-1 chi-squared test for a 2x2 table whose expected
#'     counts are all at least 1, Fisher's exact test for one with a
#'     smaller count (Campbell's rule).
#'
#'   Under `"midp"` and `"n1"`, a variable whose table is not 2x2 follows
#'   `"auto"`. `correct` and `simulate_p` apply to the Pearson chi-squared
#'   test only. When the exact enumeration of a large sparse table fails,
#'   Fisher's p-value is its Monte Carlo estimate over `simulate_B`
#'   tables, and the note says so; call [set.seed()] first to reproduce
#'   it. See *Tests*. Ignored when `by` is not used or when
#'   `p_value = FALSE`.
#' @param p_value Logical or `NULL`. If `TRUE` and `by` is used, adds
#'   the *p*-value column of the test chosen by `test`. When `NULL` (the
#'   default), the *p*-value is shown automatically whenever `by` is
#'   supplied, and hidden otherwise. Pass `p_value = FALSE` to remove
#'   the test from every output: the `p` column, and the `test_type`,
#'   `statistic`, `df` and `p.value` columns of `"long"`; `test`,
#'   `correct` and `simulate_p` are then ignored, with a warning. The
#'   association measure keeps its own switch, so
#'   `p_value = FALSE, assoc_measure = "none"` gives a purely
#'   descriptive table. Ignored when `by` is not used.
#' @param p_adjust Multiple-comparison adjustment of the `p` column,
#'   applied to the family of every test in the table (one p-value per
#'   variable). One of `"none"` (default), `"holm"`, `"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"` / `"fdr"`, or `"BY"`, as in
#'   [table_regression()]; delegated to [stats::p.adjust()]. Needs
#'   `by` and the `p` column. See *Multiple-comparison adjustment*.
#' @param percent_digits Number of digits for percentages in report outputs.
#'   Defaults to `1`. A whole number: a fraction is an error, as for
#'   every `*_digits` argument.
#' @param p_digits Integer >= 1. Number of decimal places used to
#'   render *p*-values in the `p` column (default: `3`, the APA
#'   Publication Manual standard). Both the displayed precision and
#'   the small-*p* threshold derive from this argument: `p_digits = 3`
#'   prints `.045` and `<.001`; `p_digits = 4` prints `.0451` and
#'   `<.0001`. Leading zeros are always stripped, following APA
#'   convention.
#' @param effect_size_digits Number of digits for the association measure,
#'   its confidence interval and the SMD, as for the effect sizes of the
#'   other table functions. Defaults to `2`.
#' @param assoc_measure Which association measure to report alongside the
#'   *p*-value. Accepts four input shapes:
#'
#'   * `"none"` -- drop the column entirely.
#'   * `"auto"` (the default) -- pick a measure per row variable based
#'     on the variable type: a 2x2 table (binary row variable
#'     vs. binary `by`) uses **`phi`**, a pair of ordered factors uses
#'     **`tau_b`**, every other case uses **`cramer_v`**.
#'   * a single string from
#'     `c("cramer_v", "phi", "gamma", "tau_b", "tau_c", "somers_d", "lambda")`
#'     -- applied uniformly to every row variable.
#'   * a character vector with one entry per row variable. Both
#'     **named** (`c(smoking = "phi", health = "tau_b")`, recommended;
#'     unnamed variables fall back to `"auto"`) and **unnamed**
#'     positional (`c("phi", "tau_b", "auto")`, paired up with
#'     `select`) are accepted. Named is more robust to reordering of
#'     `select`.
#'
#'   When a single measure is used for every row, the column header is
#'   that measure's name (e.g. `"Cramer's V"`). When multiple measures
#'   are used (typically with `"auto"` on a heterogeneous `select`),
#'   the header collapses to `"Effect size"` and the table note names
#'   the measure used for each variable.
#'
#'   `phi` requires a 2x2 table; if explicitly requested for a
#'   non-2x2 variable, an error is raised so the user can choose
#'   another measure or fall back to `"auto"`.
#' @param assoc_ci Passed to [cross_tab()]. If `TRUE`, includes the
#'   95\% confidence interval of the association measure. In the console
#'   and in the `"data.frame"`, `"excel"` and `"clipboard"` outputs, two
#'   extra columns headed `95\% CI LL` / `95\% CI UL` are added, the level
#'   stated as the other tables state it; in the long output (`"long"`)
#'   the bounds fill `es_ci_lower` / `es_ci_upper`. In rendered formats
#'   (`"gt"`, `"tinytable"`, `"flextable"`, `"word"`), the CI is shown
#'   inline (e.g., `.14 [.08, .19]`) under a header that states its level
#'   (`Cramer's V [95\% CI]`). Defaults to `FALSE`.
#' @param smd Logical. If `TRUE`, adds an `SMD` column holding the
#'   standardized mean difference between the two groups of `by`,
#'   the balance diagnostic of the Table 1 literature. Requires
#'   exactly two groups; the value sits on the variable row beside
#'   `p`, never on a level row. Signed for a two-category variable
#'   (group 1 minus group 2 on the second category), unsigned for
#'   three or more, where it is a distance. No confidence interval
#'   and no p-value, by design. Rounded with `effect_size_digits`. See the
#'   "Standardized mean difference" section below. Defaults to
#'   `FALSE`.
#' @param decimal_mark The decimal mark, a single character: `"."` (the
#'   default) or `","` in most styles; the Lancet style of
#'   [spicy_style()] uses the middle dot.
#' @param align Horizontal alignment of numeric columns in the
#'   printed ASCII table and in the `tinytable`, `gt`, `flextable`,
#'   `word`, and `clipboard` outputs. The first column (`Variable`)
#'   is always left-aligned. One of:
#'   - `"decimal"` (default): align numeric columns on the decimal
#'     mark, the standard scientific-publication convention used by
#'     SPSS, SAS, and LaTeX `siunitx`. Numeric cells are pre-padded
#'     with figure-spaces (U+2007, digit-width) so every string in a
#'     column has the same width with the decimal mark at the same
#'     internal position; centering those uniform-width strings then
#'     stacks the decimal points vertically. The same pad-then-center
#'     strategy is applied on every rendering engine (`gt`,
#'     `tinytable`, `flextable`, `word`, ASCII print) for a
#'     homogeneous rendering, matching `table_regression()` and
#'     `table_continuous_lm()`. The `clipboard` output is delimited
#'     text meant to be parsed rather than read at a fixed width, so
#'     its cells travel unpadded (a padded number pastes as text
#'     next to an unpadded number).
#'   - `"center"`: center-align all numeric columns.
#'   - `"right"`: right-align all numeric columns.
#'
#'   In the `excel` output, `"center"` centers the numeric columns and
#'   `"right"` is the same rendering as the default: cell-string padding
#'   does not align decimals under a proportional font, so `"decimal"`
#'   right-aligns instead, which combined with the per-column `numfmt`
#'   already produces dot-aligned columns. Same default and same three
#'   values as [table_continuous()] / [table_continuous_lm()], whose
#'   workbooks resolve `"decimal"` differently: `table_continuous()`
#'   right-aligns only the counts and the *p*-value there, and
#'   `table_continuous_lm()` applies that convention at every `align`.
#' @param output Output format. One of:
#'   - `"default"` (an ASCII table object, printed when the call is bare)
#'   - `"data.frame"` (the displayed table, numeric)
#'   - `"long"` (a long numeric `data.frame`)
#'   - `"tinytable"` (requires `tinytable`)
#'   - `"gt"` (requires `gt`)
#'   - `"flextable"` (requires `flextable`)
#'   - `"excel"` (requires `openxlsx2`)
#'   - `"clipboard"` (requires `clipr`)
#'   - `"word"` (requires `flextable` and `officer`)
#' @param indent_text Prefix used for modality labels in report table building.
#'   Defaults to `"  "` (two spaces).
#' @param indent_text_excel_clipboard Stronger indentation used in Excel and
#'   clipboard exports. Defaults to six non-breaking spaces.
#' @param add_multilevel_header Logical. If `TRUE` (the default), merges top
#'   headers in Excel export. Only consulted for `output = "excel"` on a
#'   grouped table (`by` supplied): typed with another `output`, it is
#'   ignored with a warning (class `spicy_ignored_arg`), like the other
#'   arguments of one output (`excel_path`, `excel_sheet`,
#'   `clipboard_delim`, `word_path`, ...), and a one-way table, which has
#'   a single header row to begin with, does not use it.
#' @param blank_na_wide Logical. If `FALSE` (the default), `NA` values are kept
#'   as-is in `output = "data.frame"`. If `TRUE`, the empty cells of its
#'   statistic columns become empty strings (the columns become text).
#' @param excel_path Path for `output = "excel"`, ending in `".xlsx"`, in
#'   a directory that exists. Defaults to `NULL`.
#' @param excel_sheet Sheet name for Excel export. `NULL` (the
#'   default) uses `"Categorical"`.
#' @param clipboard_delim Delimiter for clipboard text export. Defaults
#'   to `"\t"`. A cell holding the delimiter itself, a double quote or
#'   a line break is quoted RFC 4180-style, so the grid survives
#'   whatever delimiter you choose.
#' @param word_path File path for `output = "word"`, ending in `".docx"`,
#'   in a directory that exists. Defaults to
#'   `NULL`. Before 0.13.0, supplying it with `output = "flextable"`
#'   also wrote a `.docx` as a side effect; it is now used exclusively
#'   by `output = "word"` (the contract shared with the rest of the
#'   table family) and is ignored, with a warning, under any other
#'   `output`.
#' @param user_na Logical. If `TRUE` (the default), declared missing
#'   values in the row variables and in `by` are treated as missing:
#'   they join the `"(Missing)"` level under `drop_na = FALSE`, and
#'   under `drop_na = TRUE` they are removed with a dedicated
#'   disclosure line (`Declared missing values removed: ...`). If
#'   `FALSE`, the declared codes stay valid categories. See the
#'   "Declared missing values" section of [freq()].
#'
#' @param style A journal style: a theme name (`"jama"`, `"nejm"`,
#'   `"lancet"`, `"annals"`, `"apa"`, `"aer"`), a
#'   [spicy_style()] object, or `NULL` (the default). A style only
#'   changes DEFAULTS -- any argument you pass explicitly wins over it.
#'   Set `options(spicy.style = )` for document-wide scope. A theme
#'   covers numeric formatting, the word that opens the note and the
#'   abbreviations the note defines, not full editorial conformity;
#'   `?spicy_style` lists the exact rules each one encodes and the
#'   official document they come from. An unknown name is an error.
#' @param v_digits Defunct. Renamed `effect_size_digits` in spicy 0.14.0,
#'   as in the other table functions; supplying `v_digits` is an error
#'   (class `spicy_defunct`).
#'
#' @inheritSection freq Declared missing values
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: a `data.frame` of class
#'     `"spicy_categorical_table"` carrying the rendering metadata as
#'     attributes. Its columns are internal and can change without
#'     notice: read the numbers from `"data.frame"`, `"long"`,
#'     [broom::tidy()], [broom::glance()] or [as_structured()]. The object
#'     is returned visibly, so a bare `table_categorical(...)` call
#'     auto-prints the styled ASCII table at the console while
#'     `t <- table_categorical(...)` stays silent (print `t` to display
#'     the table).
#'   \item `"data.frame"`: the displayed table with its numbers, what
#'     [as.data.frame()][as.data.frame.spicy_categorical_table] of the
#'     default object returns: one row per printed row -- the header row
#'     of each variable, which carries its *p*-value and its association
#'     measure, then one row per level. The key columns are `variable`
#'     (the column name), `label` (the label the rows display for it,
#'     see `labels`), `level` and `.row_role` (`"factor_header"`,
#'     `"level"`, or `"missing"` for the row of missing values the table
#'     adds), then the printed columns: one pair `<group> n` /
#'     `<group> \%` per group level (plus `Total` when
#'     `include_total = TRUE`), `p` (absent with `p_value = FALSE`), the
#'     association measure (named after the measure, or `Effect size`
#'     when the row variables do not share one), its bounds `95\% CI LL` /
#'     `95\% CI UL` with `assoc_ci = TRUE`, and `SMD` with `smd = TRUE`.
#'     When `by = NULL`, the printed columns are `n` and `\%`.
#'   \item `"long"`: a long `data.frame`, one row per variable, level and
#'     group, with columns `variable`, `label`, `level`, `.row_role` (the
#'     role of `level`: `"level"`, or `"missing"` for the row of missing
#'     values), then, when `by` is used, `group`, `.group_role` (the role
#'     of `group`: `"group"`, `"missing"` for the column of missing `by`
#'     values, or `"total"` for the margin) and `total` (`TRUE` on the
#'     rows of the margin), then `n`, `pct` (the percentage),
#'     then, with `by`, the test of the variable unless
#'     `p_value = FALSE`: `test_type` (`"chi_squared"` for Pearson's,
#'     `"n1_chi_squared"` for the N-1 chi-squared, `"fisher_exact"`,
#'     `"fisher_midp"` for the Fisher mid-p test, or `"fisher_exact_mc"`
#'     for the Monte Carlo estimate of Fisher's exact test), `statistic`
#'     and `df` (the chi-squared, Pearson's or the N-1, `NA` for a
#'     variable tested by an exact test), `p.value` (and `p_unadjusted`
#'     under `p_adjust`); and the association measure, whichever measure
#'     it is: `es_type` names it per row (`"cramer_v"`, `"phi"`, ...),
#'     `es_value` holds it, `es_ci_lower` / `es_ci_upper` its interval
#'     (`NA` unless `assoc_ci = TRUE`), and `smd_type` (`"binary"` or
#'     `"multinomial"`, the kernel the value came from) / `smd_value` the
#'     standardized mean difference. The measure and SMD columns are
#'     there whether or not they were requested, `NA` when not, so the
#'     schema does not move with an argument. The statistics of a
#'     variable sit on every row of its block.
#'   \item `"tinytable"`: a `tinytable` object.
#'   \item `"gt"`: a `gt_tbl` object.
#'   \item `"flextable"`: a `flextable` object.
#'   \item `"excel"` / `"word"`: writes to disk and returns the file
#'     path invisibly.
#'   \item `"clipboard"`: copies the table and returns the copied text
#'     invisibly.
#' }
#'
#' The note, the `drop_na = TRUE` disclosure included, travels with the
#' table on every route, not just the console: `"default"` prints it
#' under the ASCII table, `"tinytable"` / `"gt"` / `"flextable"` /
#' `"word"` carry it as a table note, `"excel"` writes it below the body,
#' and the default object and the data frames of `"data.frame"` /
#' `"long"` keep the whole note as printed, "Note." included, in
#' `attr(x, "note")` (`NULL` when there is nothing to say), so a pipeline
#' that renders the numbers itself can still state what left the table.
#' The default object also keeps the title the console prints, in
#' `attr(x, "title")`.
#' On the `"tinytable"` route the note is set one size down;
#' `options(spicy.note_style)` governs that (see [table_regression()]).
#'
#' The Excel sheet carries the same title the console prints on its
#' first row; the table itself starts on row 3.
#'
#' @details
#' # Tests
#'
#' When `by` is used, each selected variable is cross-tabulated
#' against the grouping variable with [cross_tab()] and tested on the
#' complete cases of its observed levels, a `"(Missing)"` level and a
#' declared level nobody holds left out: one *p*-value per variable,
#' in the `p` column. A table left with a single row or column (a
#' variable observed at one level, or in one group) is uninformative and
#' gets no test, whatever `test` says: its `p` cell is left blank.
#'
#' The chi-squared approximation is unreliable when expected counts are
#' small. Under `test = "auto"`, the default, a variable takes Pearson's
#' chi-squared test ([stats::chisq.test()]) when every expected count of
#' its table is at least 5, and Fisher's exact test
#' ([stats::fisher.test()]) otherwise: the rule gtsummary's `add_p()`
#' applies, with the same p-values. Fisher's exact test of a table
#' larger than 2x2 is the Freeman-Halton extension. When its exact
#' enumeration fails, which happens on large sparse tables, the
#' p-value is its Monte Carlo estimate over `simulate_B` tables drawn
#' with the observed margins: it is random, so call [set.seed()] first
#' to reproduce it. `test = "chisq"` tests every variable by the
#' chi-squared, the behavior before 0.14.0, and `test = "fisher"` every
#' variable by the exact test.
#'
#' Fisher's exact test is conservative: its actual level is often well
#' below the nominal one, at a cost in power. `test = "midp"` gives a 2x2
#' table (a two-level variable by two groups) the Fisher mid-p test, as
#' Fagerland, Lydersen and Laake (2017, equation 1.12, p. 28, and section
#' 4.4.6, p. 97) and Lydersen, Fagerland and Laake (2009, equation 12,
#' p. 1165) define it: the tables with the observed margins are ordered
#' by their hypergeometric probability, and the *p*-value adds the
#' probabilities of the tables less probable than the observed one to
#' half the probabilities of the tables as probable, the observed one
#' included, ties being read within the relative tolerance of
#' [stats::fisher.test()]. The mid-p test can exceed the nominal level,
#' usually by little, and Fagerland, Lydersen and Laake (2017, p. 175)
#' recommend it for all sample sizes. A variable whose table is not 2x2
#' follows `"auto"`, and the note names the test of each variable. The
#' central variant, twice the smaller one-sided mid-p (the `exact2x2` and
#' `epitools` packages; Agresti, 2019, p. 49), coincides with this one
#' when the row totals or the column totals are equal, except that it
#' gives 1 for the most probable table; it differs otherwise and is not
#' computed.
#'
#' `test = "n1"` applies the rule of Campbell (2007, pp. 18-19) to a 2x2
#' table. When every expected count is at least 1, the table takes the
#' N-1 chi-squared test: Pearson's statistic, with no continuity
#' correction, multiplied by (N - 1)/N and referred to a chi-squared
#' distribution with 1 degree of freedom, which is the Mantel-Haenszel
#' statistic of a single 2x2 table; `statistic` and `df` hold that
#' statistic in the long output.
#' Otherwise it takes Fisher's exact test, two-sided by Irwin's rule as
#' [stats::fisher.test()] computes it. Campbell found the N-1 test close
#' to its nominal level under that restriction, not without it. A
#' variable whose table is not 2x2 follows `"auto"`, and the note names
#' the test of each variable.
#'
#' The note names the test of every variable, "Group comparison:
#' Pearson chi-squared test (sex, education); Fisher exact test
#' (smoking).", or the one test when it covers them all. `test_type` in
#' the long output gives the same per variable. See `@param correct` /
#' `simulate_p` to switch on Yates' continuity correction or Monte Carlo
#' *p*-values for the chi-squared test, and `@param assoc_measure` for
#' the per-row dispatch table used by `"auto"` (2x2 -> Phi, both ordered
#' -> Kendall's Tau-b, otherwise Cramer's V). Without `by`, the table
#' reports the marginal frequency distribution of each variable with no
#' inferential statistics.
#'
#' The exact tests condition on counts. Under `weights`, they run on the
#' weighted table when its cells are whole numbers (frequency weights);
#' otherwise a variable that a rule sends to Fisher's exact test keeps
#' the chi-squared test, Pearson's under `test = "auto"` and the N-1
#' under Campbell's rule, with a warning (`spicy_fallback`), and
#' `test = "fisher"`, or `test = "midp"` on a 2x2 table, is an error. For
#' survey weights, [table_categorical_svy()] gives the design-based test.
#'
#' The test and the measure are two decisions with two arguments:
#' `p_value = FALSE` removes the test, `assoc_measure = "none"` the
#' measure. Together they give a purely descriptive table, which keeps
#' the group columns and their headers; `smd` stays available.
#'
#' For model-based comparisons (cluster-robust SE, weighted contrasts,
#' fitted means) on continuous outcomes, see [table_continuous_lm()].
#' For descriptive (empirical) comparisons on continuous outcomes, see
#' [table_continuous()].
#'
#' # Groups with no data
#'
#' A variable with no observed value in a group of `by` that holds rows,
#' typically a question not asked in that group, has no data there, which
#' is not a missing answer. Its cells in that group show an en dash
#' (`NA` in the data frames, in [tidy()][tidy.spicy_categorical_table]
#' and in [as_structured()]), and its rows there are left out of
#' everything computed for that variable: the missing values the note
#' counts, the `Total` column, the `"(Missing)"` row, the test, the
#' association measure and the SMD. A variable left with fewer than two
#' groups with data is not tested: its `p` cell is left blank. The note
#' says "No data for X in G.", in one sentence for the variables with no
#' data in the same groups ("No data for X and Y in G."). Without `by`, a
#' variable with no observed value at all keeps its declared levels (its
#' one row when it declares none) with the dash, and the note says "No
#' data for X." The column of missing `by` values is never such a group,
#' and a declared level of `by` that holds no row keeps its zero column.
#'
#' # Multiple-comparison adjustment
#'
#' The p-values of a descriptive table describe: they test no
#' prespecified hypothesis. STROBE (explanation of item 14) and the
#' SAMPL guidelines advise against significance tests in descriptive and
#' baseline tables, hence the default `p_adjust = "none"`.
#'
#' When a journal or a protocol asks for adjusted p-values anyway,
#' `p_adjust` applies [stats::p.adjust()] to the family of every test in
#' the table: one p-value per variable (one per block in
#' [table_outcome()]), never the rows of one variable, which share one
#' test. `"holm"` suits a confirmatory set of comparisons, `"BH"` a
#' screening one; [table_regression()] lists the methods. A test that
#' returned no p-value stays out of the family.
#'
#' The adjustment runs before any formatting, so the `p` column and its
#' `<.001` threshold show the adjusted values, and the table note gives
#' the number of tests and the method. The raw p-values stay in the
#' object: a `p_unadjusted` column next to the p-value column of the
#' returned data frames, and `col_meta$p$p_unadjusted` (with the method
#' in `p_adjust` and the family size in `p_adjust_m`) in
#' [as_structured()]. Without `by`, or with `p_value = FALSE`, the table
#' shows no p-value to adjust, and a `p_adjust` other than `"none"` is an
#' error.
#'
#' # Standardized mean difference
#'
#' `smd = TRUE` adds an `SMD` column with the balance diagnostic of
#' the Table 1 literature, on the variable row beside `p`. For a
#' two-category variable it is the Bernoulli form,
#'
#' \deqn{\mathrm{SMD} = \frac{p_1 - p_2}{\sqrt{(p_1(1-p_1) +
#' p_2(1-p_2)) / 2}}}
#'
#' with \eqn{p} the proportion of the SECOND category, **signed**,
#' group 1 minus group 2 in the order the table displays them. Note
#' the denominator: the Bernoulli variance \eqn{p(1-p)} at *n*, not
#' `var()` at \eqn{n-1}, which would be 19% off on a small table.
#'
#' "Second category" is the order the table shows, which is worth
#' knowing for a **logical** variable: spicy displays `TRUE` then
#' `FALSE`, so the sign is taken on `FALSE`, where `tableone` and
#' `cobalt` coerce with `factor()` (`FALSE`, `TRUE`) and take it on
#' `TRUE` -- the same magnitude with the opposite sign. Convert to a
#' factor with the level order you want if the direction matters.
#'
#' For three or more categories it is the multivariate form of Yang
#' and Dalton (2012, SAS Global Forum 335-2012),
#'
#' \deqn{\mathrm{SMD} = \sqrt{T' S^{-} T}}
#'
#' with \eqn{T} the difference of the two profiles of proportions
#' (first category dropped) and \eqn{S} the mean of their multinomial
#' covariance matrices. This is a Mahalanobis distance: it is
#' **unsigned**, it is **not bounded by 1**, and \eqn{S^{-}} is a
#' pseudo-inverse, because a declared-but-unobserved category makes
#' \eqn{S} singular and `solve()` would abort where the
#' pseudo-inverse returns exactly the value that category's absence
#' implies. Which kernel a row took is published as `smd_type` in the
#' `"long"` output, and the unsigned reading is stated in the table
#' note whenever a variable has more than two categories. The
#' `MASS` package is needed for this arm only.
#'
#' Two profiles with **no category in common** have an infinite
#' standardized distance. The pseudo-inverse would quietly publish a
#' finite number there, so the cell is an en-dash and a classed
#' warning says why. The same applies when each group is constant on
#' a different category, where the naive route publishes `0` --
#' "perfectly balanced" for the most imbalanced variable possible.
#'
#' Conventions shared with [table_continuous()]: exactly two groups
#' (three or more are refused, not averaged over pairs); complete
#' cases on the observed groups, so a `drop_na = FALSE` "(Missing)"
#' level is displayed and never enters the diagnostic; no confidence
#' interval and no p-value, by design. Under `weights` the profiles
#' are the weighted proportions, which makes this column agree with
#' both the frequency and the survey-design readings -- a profile of
#' proportions is invariant to a global rescaling of the weights, so
#' `rescale` cannot move it. (Only the continuous arm has a
#' convention to choose there.)
#'
#' The `SMD` cell keeps its leading zero where the association cell
#' drops it: the APA strip belongs to a bounded measure, and this one
#' is not bounded. The two columns therefore print `0.45` and `.45`
#' side by side, on purpose.
#'
#' A balance table usually shows no test: `p_value = FALSE` removes the
#' `p` column here as it does in [table_continuous()], so a table mixing
#' continuous and categorical variables can drop both. [inline()]
#' quotes the cell from the variable row (`inline(tbl, smoking,
#' column = "smd")`).
#'
#' # Display conventions
#'
#' Decimal alignment, *p*-value formatting, and required suggested
#' packages per output engine are documented under `@param align`,
#' `@param p_digits`, and `@param output` respectively.
#'
#' Counts are displayed as integers: weighted counts are rounded
#' (ties half to even, the R convention) at display time only, in
#' cells and margins alike -- the SPSS Crosstabs convention. Cells
#' and margins are rounded independently, so small display
#' discrepancies are possible (e.g. two cells of exactly 0.5 each
#' display as `0` while their `Total` of 1.0 displays as `1`). The
#' machine outputs (`"data.frame"`, `"long"`) carry the exact
#' weighted counts and full-precision percentages.
#'
#' @references
#' Agresti, A. (2019). *An Introduction to Categorical Data Analysis* (3rd
#' ed.). Wiley.
#'
#' Campbell, I. (2007). Chi-squared and Fisher-Irwin tests of two-by-two
#' tables with small sample recommendations. *Statistics in Medicine*,
#' 26(19), 3661-3675. \doi{10.1002/sim.2832}
#'
#' Fagerland, M. W., Lydersen, S., & Laake, P. (2017). *Statistical
#' Analysis of Contingency Tables*. Chapman and Hall/CRC.
#' \doi{10.1201/9781315374116}
#'
#' Lydersen, S., Fagerland, M. W., & Laake, P. (2009). Recommended tests
#' for association in 2 x 2 tables. *Statistics in Medicine*, 28(7),
#' 1159-1175. \doi{10.1002/sim.3531}
#'
#' @family spicy tables
#' @seealso [table_continuous()] for empirical comparisons on
#'   continuous outcomes; [table_continuous_lm()] for the model-based
#'   companion (heteroskedasticity-consistent / cluster-robust /
#'   bootstrap / jackknife SE, fitted means, weighted contrasts);
#'   [cross_tab()] for two-way cross-tabulations; [freq()] for
#'   one-way frequency tables.
#'
#' @examples
#' # --- Basic usage ---------------------------------------------------------
#'
#' # Default: ASCII console table grouped by sex.
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, physical_activity),
#'   by = sex
#' )
#'
#' # One-way frequency-style table (no `by`).
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, physical_activity)
#' )
#'
#' # Purely descriptive grouped table: no test, no association measure.
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, physical_activity),
#'   by = sex,
#'   p_value = FALSE,
#'   assoc_measure = "none"
#' )
#'
#' # A small sample: `smoking` has an expected count below 5 and takes
#' # Fisher's exact test; the note names the test of each variable.
#' table_categorical(
#'   sochealth[1:40, ],
#'   select = c(smoking, physical_activity),
#'   by = sex
#' )
#'
#' # Pretty labels keyed by column name.
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, physical_activity),
#'   by = education,
#'   labels = c(
#'     smoking           = "Current smoker",
#'     physical_activity = "Physical activity"
#'   )
#' )
#'
#' # Survey weights with rescaling.
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, physical_activity),
#'   by = education,
#'   weights = "weight",
#'   rescale = TRUE
#' )
#'
#' # Confidence interval for the association measure.
#' table_categorical(
#'   sochealth,
#'   select = smoking,
#'   by = education,
#'   assoc_ci = TRUE
#' )
#'
#' # --- Per-variable association measure ----------------------------------
#'
#' # Default (`assoc_measure = "auto"`): one measure per row variable based on
#' # the variable type (2x2 -> Phi, both ordered factors -> Kendall's Tau-b,
#' # otherwise Cramer's V). When the chosen measures differ across rows, the
#' # column header collapses to `"Effect size"` and the note names the
#' # measure used for each variable.
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, education),
#'   by = sex
#' )
#'
#' # Force a uniform measure across all row variables.
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, education),
#'   by = sex,
#'   assoc_measure = "cramer_v"
#' )
#'
#' # Per-variable override (recommended named form).
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, education, self_rated_health),
#'   by = sex,
#'   assoc_measure = c(
#'     smoking           = "phi",        # binary x binary
#'     education         = "cramer_v",   # multi-category nominal
#'     self_rated_health = "tau_b"       # ordinal x binary, Tau-b
#'   )
#' )
#'
#' # --- Output formats -----------------------------------------------------
#'
#' # The rendered outputs below all wrap the same call:
#' #   table_categorical(sochealth,
#' #                     select = c(smoking, physical_activity),
#' #                     by = sex)
#' # only `output` changes. Assign each result to a variable -- some
#' # engines auto-print as a console-friendly text fallback inside
#' # the `?` help viewer.
#'
#' # Wide data.frame (one row per modality).
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, physical_activity),
#'   by = sex,
#'   output = "data.frame"
#' )
#'
#' # Long data.frame (one row per (modality x group)).
#' table_categorical(
#'   sochealth,
#'   select = c(smoking, physical_activity),
#'   by = sex,
#'   output = "long"
#' )
#'
#' \donttest{
#' # Rendered HTML / docx objects -- best viewed inside a
#' # Quarto / R Markdown document or a pkgdown article.
#' if (requireNamespace("tinytable", quietly = TRUE)) {
#'   tt <- table_categorical(
#'     sochealth, select = c(smoking, physical_activity), by = sex,
#'     output = "tinytable"
#'   )
#' }
#' if (requireNamespace("gt", quietly = TRUE)) {
#'   tbl <- table_categorical(
#'     sochealth, select = c(smoking, physical_activity), by = sex,
#'     output = "gt"
#'   )
#' }
#' if (requireNamespace("flextable", quietly = TRUE)) {
#'   ft <- table_categorical(
#'     sochealth, select = c(smoking, physical_activity), by = sex,
#'     output = "flextable"
#'   )
#' }
#'
#' # Excel and Word: write to a temporary file.
#' if (requireNamespace("openxlsx2", quietly = TRUE)) {
#'   tmp <- tempfile(fileext = ".xlsx")
#'   table_categorical(
#'     sochealth, select = c(smoking, physical_activity), by = sex,
#'     output = "excel", excel_path = tmp
#'   )
#'   unlink(tmp)
#' }
#' if (
#'   requireNamespace("flextable", quietly = TRUE) &&
#'     requireNamespace("officer", quietly = TRUE)
#' ) {
#'   tmp <- tempfile(fileext = ".docx")
#'   table_categorical(
#'     sochealth, select = c(smoking, physical_activity), by = sex,
#'     output = "word", word_path = tmp
#'   )
#'   unlink(tmp)
#' }
#' }
#'
#' \dontrun{
#' # Clipboard: writes to the system clipboard.
#' table_categorical(
#'   sochealth, select = c(smoking, physical_activity), by = sex,
#'   output = "clipboard"
#' )
#' }
#'
#' @export
table_categorical <- function(
  data,
  select = tidyselect::everything(),
  by = NULL,
  labels = getOption("spicy.var_labels", FALSE),
  levels_keep = NULL,
  include_total = TRUE,
  drop_na = FALSE,
  weights = NULL,
  rescale = getOption("spicy.rescale", FALSE),
  correct = FALSE,
  simulate_p = FALSE,
  simulate_B = 2000,
  test = c("auto", "chisq", "fisher", "midp", "n1"),
  p_value = NULL,
  p_adjust = "none",
  percent_digits = 1,
  p_digits = 3,
  effect_size_digits = 2,
  assoc_measure = "auto",
  assoc_ci = FALSE,
  smd = FALSE,
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
  add_multilevel_header = TRUE,
  blank_na_wide = FALSE,
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  user_na = TRUE,
  style = NULL,
  v_digits
) {
  # Migration guard first, so an old `v_digits =` call gets the new name
  # before any other validation can fire.
  if (!missing(v_digits)) {
    .abort_renamed_arg(
      "table_categorical",
      "v_digits",
      "effect_size_digits",
      deparse1(substitute(v_digits))
    )
  }
  select_missing <- missing(select)
  # A journal / locale style only moves DEFAULTS (see `?spicy_style`).
  .style_pushed <- .style_begin(style, match.call(), environment())
  on.exit(.style_end(.style_pushed), add = TRUE)
  output <- spicy_match_arg(output)
  # Decision 16: NULL resolves to the family's registry sheet name,
  # keeping the usage line of the Rd clean of a display string.
  if (is.null(excel_sheet)) {
    excel_sheet <- spicy_str("excel_sheet_categorical")
  }
  align <- spicy_match_arg(align)
  test <- spicy_match_arg(test)

  .check_data_frame(data, "table_categorical")
  .check_labels_arg(labels)
  by_quo <- rlang::enquo(by)
  has_group <- !rlang::quo_is_null(by_quo)
  by_name <- NULL
  if (has_group) {
    by_name <- resolve_single_column_selection(by_quo, data, "by")
  }
  # `p_value`: the switch of the chi-squared test, as in the siblings.
  # NULL shows it whenever `by` is supplied; FALSE removes `p`, `Chi2`
  # and `df` from every output (decision of 2026-10-17, issue #9).
  if (
    !is.null(p_value) &&
      (!is.logical(p_value) || length(p_value) != 1L || is.na(p_value))
  ) {
    spicy_abort(
      "`p_value` must be TRUE, FALSE, or NULL.",
      class = "spicy_invalid_input"
    )
  }
  .check_desc_p_adjust(p_adjust, has_group, p_value)
  p_value_explicit <- !is.null(p_value)
  if (!p_value_explicit) {
    p_value <- has_group
  }
  if (p_value && !has_group) {
    if (p_value_explicit) {
      spicy_warn(
        "`p_value` is ignored when `by` is not used.",
        class = "spicy_ignored_arg"
      )
    }
    p_value <- FALSE
  }
  # Validated HERE rather than beside the other logicals below: the
  # refusal of a `by`-less `smd` reads it a few lines from now, and a
  # non-logical value must not reach `&&` first.
  validate_logical_scalar(smd, "smd")
  validate_logical_scalar(assoc_ci, "assoc_ci")
  if (smd && !has_group) {
    spicy_warn(
      "`smd` is ignored when `by` is not used: a standardized mean difference compares two groups.",
      class = "spicy_ignored_arg"
    )
  }
  do_smd <- smd && has_group

  # The weights column, by name (NULL for a vector of weights): a role of
  # the selection below, and the name the note's weights sentence gives.
  weights_name <- detect_weights_column_name(rlang::enquo(weights), data)
  # The `by` and weights columns are outside the selection: `everything()`
  # and the other helpers skip them, and pointing at one in `select`, by
  # name or by position, is an error (a variable crossed with itself, or
  # the weights tabulated).
  select_names <- .resolve_selection_without(
    rlang::enquo(select),
    data,
    roles = c(by = by_name, weights = weights_name)
  )
  if (!select_missing && length(select_names) == 0L) {
    .abort_empty_selection("select")
  }
  # Select-less call: restrict the `everything()` default to eligible
  # categorical columns (factor, character, logical, labelled) -- the
  # categorical mirror of the numeric auto-restriction in the continuous
  # companions. An explicit `select` is taken verbatim so numeric-coded
  # categorical variables can still be tabulated by naming them.
  if (select_missing) {
    is_eligible <- vapply(
      select_names,
      function(nm) {
        col <- data[[nm]]
        is.factor(col) ||
          is.character(col) ||
          is.logical(col) ||
          labelled::is.labelled(col)
      },
      logical(1)
    )
    select_names <- select_names[is_eligible]
    if (length(select_names) == 0L) {
      .abort_empty_selection("select", "categorical")
    }
  }
  # bit64::integer64 columns tabulate as raw denormal doubles
  # ("9.88e-324" levels) unless the bit64 namespace happens to be
  # loaded; the select-less default already excludes them via the
  # eligibility filter above, so this fires only for explicitly
  # named columns and `by`.
  .check_integer64_columns(
    data,
    c(select_names, by_name),
    "table_categorical"
  )
  # The "by" title names the grouping variable under the same `labels`
  # as the rows (see `resolve_variable_labels()`), as table_continuous()
  # does. Resolved first: `labels` then becomes the rows' labels.
  by_label <- if (is.null(by_name)) {
    NULL
  } else {
    resolve_variable_labels(data, by_name, labels)
  }
  labels <- resolve_variable_labels(data, select_names, labels)
  # `levels_keep` as one entry per variable: a vector applies to all of
  # them, a named list to the variables it names.
  levels_keep <- .cat_levels_keep_spec(levels_keep, select_names)
  # The rows are keyed by column name, which no two variables share; two
  # may show the same label. The wide frames show the label in place of
  # the key; the long frames publish both, as `table_continuous()` does:
  # `variable` the column name, `label` beside it the label displayed.
  label_of_key <- function(key) labels[match(key, select_names)]
  with_label <- function(df) {
    nm <- names(df)
    at <- match("variable", nm)
    df$label <- label_of_key(df$variable)
    df[c(nm[seq_len(at)], "label", nm[-seq_len(at)])]
  }
  # The fixed keys of the rows a long frame carries (O10 of the coherence
  # audit), each beside the key column it describes: `.row_role` is the
  # role of `level` ("missing" on the row of missing values the table
  # adds, "level" elsewhere), `.group_role` the role of `group` ("missing"
  # on the column of missing `by` values, "total" on the margin, "group"
  # elsewhere), and `total` flags the margin. The KEY, never the label: a
  # translated "(Missing)" still reads as "missing", and a level of
  # another variable that happens to read the same as "level" -- the rule
  # of the typed view. `missing_vars`, `missing_group`, `missing_label`
  # and `margin_key` are read when the frame is built.
  with_keys <- function(df) {
    role <- ifelse(
      df$variable %in% missing_vars & df$level == missing_label,
      "missing",
      "level"
    )
    df <- .insert_cols_after(df, list(.row_role = role), "level")
    if (!"group" %in% names(df)) {
      return(df)
    }
    total <- if (isTRUE(include_total)) {
      df$group == margin_key
    } else {
      rep(FALSE, nrow(df))
    }
    group_role <- ifelse(
      total,
      "total",
      ifelse(missing_group & df$group == missing_label, "missing", "group")
    )
    .insert_cols_after(
      df,
      list(.group_role = group_role, total = total),
      "group"
    )
  }

  validate_logical_scalar(include_total, "include_total")
  validate_logical_scalar(drop_na, "drop_na")
  validate_logical_scalar(user_na, "user_na")
  # Declared missing values (see the "Declared missing values" section
  # of ?freq): with `user_na = TRUE` declared codes become regular NA
  # up front, so they join the "(Missing)" level under drop_na = FALSE
  # and the disclosed removal under drop_na = TRUE; with `user_na =
  # FALSE` the declaration is dropped and the codes stay categories.
  resolve_user_na <- function(v) {
    if (isTRUE(user_na)) .user_na_to_na(v) else .user_na_zap(v)
  }
  # Truthfulness ledger for drop_na = TRUE: per-variable NA counts (and
  # the by-variable's, in grouped tables) removed before tabulation,
  # split between regular NA and declared missing values.
  # Surfaced as a "Missing values removed: ..." table note -- dropping
  # is an analyst choice that the READER must be able to see.
  na_dropped <- integer(0)
  user_na_dropped <- integer(0)
  by_na_dropped <- 0L
  # The groups of `by` in which a variable has no data: rows there, and
  # not one observed value -- typically a question not asked in that
  # group. That is not a missing answer. The variable's cells in such a
  # group are NA in the compute frame and show `cell_undefined` on every
  # route, and its rows there leave everything computed for it: the
  # missing-value counts, the margin, the `(Missing)` row, the test, the
  # association measure and the SMD. Named by variable, the groups in
  # table order. Without `by`, `no_data_vars` lists the variables with
  # no observed value at all.
  no_data_groups <- list()
  no_data_vars <- character(0)
  # The missing-data sentences of the note, (b) of `.compose_note()`:
  # what drop_na = TRUE removed, then the groups with no data. NULL when
  # there is nothing to say. Read lazily at assembly time, after the
  # tabulation loops have filled the ledger.
  build_missing_note <- function() {
    # Each variable is named as its rows name it (`labels` above).
    label_of <- stats::setNames(labels, select_names)
    na_dropped <- .label_counts(na_dropped, label_of)
    user_na_dropped <- .label_counts(user_na_dropped, label_of)
    parts <- character(0)
    if (drop_na && length(na_dropped)) {
      parts <- c(
        parts,
        paste0(
          spicy_str("note_missing_removed"),
          .join_note_vars(
            spicy_fmt("note_missing_item", names(na_dropped), na_dropped)
          ),
          "."
        )
      )
    }
    if (drop_na && length(user_na_dropped)) {
      parts <- c(
        parts,
        paste0(
          spicy_str("note_declared_missing_removed"),
          .join_note_vars(
            spicy_fmt(
              "note_missing_item",
              names(user_na_dropped),
              user_na_dropped
            )
          ),
          "."
        )
      )
    }
    if (drop_na && by_na_dropped > 0L) {
      parts <- c(
        parts,
        spicy_fmt("note_rows_missing_by_removed", by_label, by_na_dropped)
      )
    }
    # One sentence per set of groups, the variables in the order of
    # `select`.
    no_data <- if (has_group) {
      shown <- intersect(select_names, names(no_data_groups))
      if (length(shown)) {
        .no_data_sentences(label_of_key(shown), no_data_groups[shown])
      }
    } else if (length(no_data_vars)) {
      .no_data_sentences(label_of_key(no_data_vars))
    }
    parts <- c(parts, no_data)
    if (length(parts)) paste(parts, collapse = " ") else NULL
  }
  # What the numbers are, (a) of `.compose_note()`: a weighted table says
  # so, in the words of `table_continuous()` (decision 17).
  build_weights_note <- function() {
    if (is.null(weights_vec)) {
      return(NULL)
    }
    spicy_fmt(
      "note_weighted_by",
      weights_name %||% spicy_str("note_weights_fallback")
    )
  }
  validate_logical_scalar(rescale, "rescale")
  validate_logical_scalar(correct, "correct")
  validate_logical_scalar(simulate_p, "simulate_p")
  if (
    !is.numeric(simulate_B) ||
      length(simulate_B) != 1 ||
      is.na(simulate_B) ||
      simulate_B < 1
  ) {
    spicy_abort(
      "`simulate_B` must be a positive integer.",
      class = "spicy_invalid_input"
    )
  }
  simulate_B <- as.integer(simulate_B)
  validate_logical_scalar(add_multilevel_header, "add_multilevel_header")
  validate_logical_scalar(blank_na_wide, "blank_na_wide")
  validate_decimal_mark(decimal_mark)
  percent_digits <- validate_digit_arg(percent_digits, "percent_digits")
  effect_size_digits <- validate_digit_arg(
    effect_size_digits,
    "effect_size_digits"
  )
  # `p_digits` has a stricter floor than the other digits arguments:
  # a 0-decimal p-value is meaningless, and `format_p_value()` would
  # silently fall back to 3 decimals. Same classed check as
  # table_continuous() / table_continuous_lm() / cross_tab().
  p_digits <- validate_digit_arg(p_digits, "p_digits", min = 1L)
  # An argument of one route only (`word_path` is a save path for
  # `output = "word"`, ...) typed with another `output` reaches nothing:
  # the family's warning says so, once the arguments are valid.
  .warn_ignored_output_args(output, names(match.call()))

  if (!has_group) {
    if (!include_total) {
      spicy_warn(
        "`include_total` is ignored when `by` is not used.",
        class = "spicy_ignored_arg"
      )
    }
    if (correct) {
      spicy_warn(
        "`correct` is ignored when `by` is not used.",
        class = "spicy_ignored_arg"
      )
    }
    if (simulate_p) {
      spicy_warn(
        "`simulate_p` is ignored when `by` is not used.",
        class = "spicy_ignored_arg"
      )
    }
    if (!identical(test, "auto")) {
      spicy_warn(
        "`test` is ignored when `by` is not used.",
        class = "spicy_ignored_arg"
      )
    }
    if (!isTRUE(all(as.character(assoc_measure) == "auto"))) {
      spicy_warn(
        "`assoc_measure` is ignored when `by` is not used.",
        class = "spicy_ignored_arg"
      )
    }
    if (assoc_ci) {
      spicy_warn(
        "`assoc_ci` is ignored when `by` is not used.",
        class = "spicy_ignored_arg"
      )
    }
    include_total <- TRUE
  }
  # Yates' correction and the Monte Carlo p-value change the chi-squared
  # test and nothing else (the association measures are computed apart),
  # so without the test they have nothing to change. Switched off for the
  # internal calls too: a simulation would draw from the session's RNG
  # for a p-value nobody sees.
  if (has_group && !p_value) {
    if (correct) {
      spicy_warn(
        "`correct` is ignored when `p_value = FALSE`.",
        class = "spicy_ignored_arg"
      )
    }
    if (simulate_p) {
      spicy_warn(
        "`simulate_p` is ignored when `p_value = FALSE`.",
        class = "spicy_ignored_arg"
      )
    }
    if (!identical(test, "auto")) {
      spicy_warn(
        "`test` is ignored when `p_value = FALSE`.",
        class = "spicy_ignored_arg"
      )
    }
    correct <- FALSE
    simulate_p <- FALSE
  }
  # The exact test has neither a continuity correction nor a simulated
  # chi-squared to take: under `test = "fisher"` the two switches reach
  # no variable. Yates' correction belongs to the Pearson chi-squared of a
  # 2 x 2 table, which `"midp"` and `"n1"` give a test of their own, so it
  # reaches no variable under them either. Otherwise the switches reach
  # the variables the Pearson chi-squared tests, decided per variable
  # below.
  if (has_group && p_value && test %in% c("fisher", "midp", "n1")) {
    if (correct) {
      spicy_warn(
        sprintf("`correct` is ignored when `test = \"%s\"`.", test),
        class = "spicy_ignored_arg"
      )
    }
    correct <- FALSE
  }
  if (has_group && p_value && identical(test, "fisher")) {
    if (simulate_p) {
      spicy_warn(
        "`simulate_p` is ignored when `test = \"fisher\"`.",
        class = "spicy_ignored_arg"
      )
    }
    simulate_p <- FALSE
  }

  weights_quo <- rlang::enquo(weights)
  weights_vec <- resolve_weights_argument(weights_quo, data, "weights")
  # Checked here, so the error names this function rather than the
  # `cross_tab()` / `freq()` the tabulation calls: the values those two
  # accept (NA allowed, an all-zero vector too).
  .check_weights_values(weights_vec, require_positive = FALSE)

  # Without weights there is nothing to rescale: a `rescale = TRUE` typed
  # in the call warns, one read from `options(spicy.rescale)` does not.
  if (isTRUE(rescale) && is.null(weights_vec)) {
    if (!missing(rescale)) {
      .warn_ignored("rescale", "when `weights` is not supplied")
    }
    rescale <- FALSE
  }

  # The label of the row (and, under `by`, the column) of missing values.
  # A variable that displays a level with this label beside that row is
  # refused where the row is appended (`.cat_missing_row_guard()`), and so
  # is a `by` beside that column, so the label is never renamed. The rows
  # of missing values are those of the variables in `missing_vars`, which
  # the typed view reads: a level with this label in a variable without
  # missing values is a level.
  missing_label <- spicy_str("row_missing_level")
  missing_vars <- character(0)
  # Whether `by` has a column of missing values (set where it is added).
  missing_group <- FALSE

  # `measure_key` is the measure the caller ASKED this row for, as a key
  # ("cramer_v", ..., or "none"). It rides through in `measure` so the
  # long output can publish it per row: reading it back off the
  # cross_tab() object would mean recognizing a display label.
  parse_stats <- function(ct_obj, measure_key = NA_character_) {
    # Read numeric attributes set by cross_tab()
    p_val <- attr(ct_obj, "p_value")
    v_val <- attr(ct_obj, "assoc_value")
    chi2_val <- attr(ct_obj, "chi2")
    df_val <- attr(ct_obj, "df")
    ar <- attr(ct_obj, "assoc_result")
    ci_lo <- if (!is.null(ar)) ar[["ci_lower"]] else NA_real_
    ci_hi <- if (!is.null(ar)) ar[["ci_upper"]] else NA_real_

    if (!is.null(p_val) && !is.null(v_val)) {
      # Numeric path: leave the small-p threshold to `format_p_value()`
      # via `p_digits`. Setting `p_op = "<"` here based on a hardcoded
      # 0.001 threshold would force `fmt_p()` to render `"<.0001"` for
      # any `p < 0.001`, even when `p_digits = 4` and the true value
      # (e.g. 0.000108) is *greater* than the displayed-precision
      # threshold (1e-4). The `<` override is reserved for the
      # note-parsing fallback path below where the actual p-value is
      # only available as the literal string "p < threshold".
      return(list(
        p = p_val,
        p_op = "=",
        v = v_val,
        measure = measure_key,
        chi2 = chi2_val %||% NA_real_,
        df = df_val %||% NA_real_,
        ci_lower = ci_lo,
        ci_upper = ci_hi
      ))
    }

    # Fallback: parse note text
    note_txt <- attr(ct_obj, "note")
    txt <- paste(note_txt %||% "", collapse = " ")

    pm <- regmatches(
      txt,
      regexec("p\\s*([<=>])\\s*([0-9.]+(?:e[-+]?\\d+)?)", txt, perl = TRUE)
    )[[1]]
    p_op <- if (length(pm) >= 2) pm[2] else NA_character_
    p_val <- if (length(pm) >= 3) {
      # nocov start: this fallback only fires when cross_tab() skipped its
      # stats block (degenerate table), and that block is the sole writer
      # of "p = ..." note text -- the regex can never match here. Same
      # reasoning as fmt_p()'s `op = "<"` arm below. Defensive.
      suppressWarnings(as.numeric(pm[3]))
      # nocov end
    } else {
      NA_real_
    }

    # Try to match any "Measure = value" pattern. The alternation is built
    # from the labels themselves, never re-typed: a hardcoded copy would
    # silently stop matching the day a measure is renamed or translated.
    measure_alt <- paste(
      vapply(
        .assoc_measure_keys,
        function(k) .escape_regex(.assoc_label(k)),
        character(1)
      ),
      collapse = "|"
    )
    vm <- regmatches(
      txt,
      regexec(
        paste0("(?:", measure_alt, ")\\s*=\\s*([0-9.eE+-]+)"),
        txt,
        perl = TRUE
      )
    )[[1]]
    v_val <- if (length(vm) >= 2) {
      # nocov start: unreachable for the same reason as the p-value arm
      # above -- measure text is only written together with the numeric
      # attrs that make parse_stats() take the numeric path. Defensive.
      suppressWarnings(as.numeric(vm[2]))
      # nocov end
    } else {
      NA_real_
    }

    list(
      p = p_val,
      p_op = p_op,
      v = v_val,
      measure = measure_key,
      chi2 = NA_real_,
      df = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_
    )
  }

  # The mark of a cell whose statistic applies but has no value: the
  # count and the percentage of a group in which the variable has no data
  # (`no_data_groups`), and an SMD that cannot be estimated.
  undefined_cell <- spicy_str("cell_undefined")

  # `decimal.mark` pinned on every `formatC()` of this file that can
  # write one -- the exception is `fmt_n()` below, whose `digits = 0`
  # produces no decimal mark at all. The substitution below is the ONLY
  # authority on the mark, and `formatC()` would otherwise read
  # `options(OutDec)` and slip a session comma past a table that asked
  # for a point.
  fmt_num <- function(x, digits = 1, na = "") {
    out <- ifelse(
      is.na(x),
      na,
      formatC(x, format = "f", digits = digits, decimal.mark = ".")
    )
    if (decimal_mark != ".") {
      out <- sub("\\.", decimal_mark, out)
    }
    out
  }

  # Counts DISPLAY as integers -- weighted counts included (rounded
  # ties half to even, matching round()), in cells and margins alike:
  # the SPSS Crosstabs convention. Cells and margins are rounded
  # independently, so a displayed Total may differ slightly from the
  # sum of the displayed cells (SPSS behaves the same way). The
  # machine outputs keep the exact fractional counts; rounding
  # happens here, at display time only.
  fmt_n <- function(x, na = "") {
    out <- rep(na, length(x))
    ok <- !is.na(x)
    if (any(ok)) {
      out[ok] <- formatC(round(x[ok], 0), format = "f", digits = 0)
    }
    out
  }

  # `fmt_p` defers to the shared `format_p_value()` helper so the
  # `table_*()` functions print *p*-values identically: `p_digits`
  # drives both the displayed precision AND the small-*p* threshold
  # (`p_digits = 3` -> `<.001`, `p_digits = 4` -> `<.0001`, etc.),
  # leading zeros are stripped, and the configured `decimal_mark` is
  # honored. The `op` argument carries the comparison operator
  # parsed from `cross_tab()`'s note text in the rare fallback path
  # where the numeric p-value is unavailable: `op = "<"` means the
  # underlying note literally said "p < threshold", so we honor the
  # request and render the small-p form regardless of the numeric
  # placeholder.
  fmt_p <- function(p, op = NA_character_) {
    if (is.na(p)) {
      return("")
    }
    if (!is.na(op) && identical(op, "<")) {
      # nocov start: `op = "<"` only arrives from parse_stats()'s note-text
      # fallback, which fires solely when cross_tab() omits the numeric
      # `p_value` attr (single-level table). In that degenerate case the
      # note is NULL too, so the regex never yields `p_op = "<"`. Defensive.
      return(paste0("<", decimal_mark, strrep("0", p_digits - 1L), "1"))
      # nocov end
    }
    format_p_value(p, decimal_mark, digits = p_digits)
  }

  fmt_v <- function(v) {
    if (is.na(v)) {
      return("")
    }
    s <- formatC(
      v,
      format = "f",
      digits = effect_size_digits,
      decimal.mark = "."
    )
    # The association measure is bounded, so it follows the same
    # leading-zero policy as a p-value: dropped under a decimal point,
    # kept under a comma mark or under a style that says so
    # (`p_style = "standard"`).
    s <- .strip_leading_zero(s, ".", .style_p_leading_zero(decimal_mark))
    if (decimal_mark != ".") {
      s <- sub("\\.", decimal_mark, s)
    }
    s
  }

  # The SMD takes the same precision as the association measure and
  # NOT its leading-zero policy: that policy exists because a bounded
  # measure can never reach 1, and the SMD of a variable with three or
  # more categories is a Mahalanobis distance with no upper bound
  # (1.11 and 2.45 on the pinned fixtures). Stripping the zero would
  # also make the printed cell disagree with the typed view, which
  # carries `p_style = NULL` for exactly this reason.
  #
  # An en-dash, not an empty cell, when the value is NA: the diagnostic
  # APPLIES to the row and cannot be estimated (decision 23), unlike
  # the level rows below it, which are structurally blank.
  fmt_smd <- function(v) {
    if (is.na(v)) {
      return(undefined_cell)
    }
    s <- formatC(
      v,
      format = "f",
      digits = effect_size_digits,
      decimal.mark = "."
    )
    if (decimal_mark != ".") {
      s <- sub("\\.", decimal_mark, s)
    }
    s
  }

  # Pre-pad numeric (i.e. non-Variable) columns of a display data
  # frame with figure-spaces (U+2007, digit-width) so the decimal
  # mark falls at the same horizontal position across each column.
  # Used by every body-rendering engine (`gt`, `tinytable`,
  # `flextable`, `word`, ASCII print) so the rendered
  # output is homogeneous: centering uniform-width strings stacks the
  # decimal points vertically. Same strategy as `table_regression()`
  # and `table_continuous_lm()`. The first column is the variable /
  # level label and is left untouched. No-op unless
  # `align == "decimal"`.
  pad_decimal_cols <- function(df) {
    if (!identical(align, "decimal") || ncol(df) < 2L) {
      return(df)
    }
    for (j in seq_along(df)[-1]) {
      df[[j]] <- decimal_align_strings(
        df[[j]],
        decimal_mark = decimal_mark,
        pad_char = "\u2007"
      )
    }
    df
  }

  if (!has_group) {
    rows <- list()
    rr <- 1L
    all_level_order <- character(0)

    for (i in seq_along(select_names)) {
      x_raw <- data[[select_names[i]]]
      n_user <- if (user_na) sum(.user_na_mask(x_raw)) else 0L
      # Factor built BEFORE tabulation (see .tab_factor): declared
      # level order and haven value labels survive end-to-end; the
      # data never round-trips through as.character().
      x <- .tab_factor(resolve_user_na(x_raw))
      w <- weights_vec

      var_level_order <- levels(x)

      # No observed value at all: the variable has no data, which is not
      # a missing answer (see `no_data_groups`). It keeps the row shape
      # of a variable -- its declared levels, or its one row when it
      # declares none (level NA) -- with NA counts, and its rows are not
      # counted as missing values.
      if (length(x) > 0L && all(is.na(x))) {
        keep_i <- levels_keep[[select_names[i]]]
        lv_use <- if (is.null(keep_i)) {
          var_level_order
        } else {
          .cat_kept_levels(levels_keep, select_names[i], var_level_order)
        }
        if (!is.null(keep_i) && length(lv_use) == 0L) {
          .warn_levels_keep_no_match(select_names[i], var_level_order)
          next
        }
        no_data_vars <- c(no_data_vars, select_names[i])
        if (length(lv_use) == 0L) {
          lv_use <- NA_character_
        }
        for (k in seq_along(lv_use)) {
          rows[[rr]] <- data.frame(
            variable = select_names[i],
            level = lv_use[[k]],
            .lvrank = k,
            n = NA_real_,
            pct = NA_real_,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          rr <- rr + 1L
        }
        next
      }

      keep <- if (drop_na) !is.na(x) else rep(TRUE, length(x))
      if (drop_na && sum(!keep) > 0L) {
        n_sys <- sum(!keep) - n_user
        if (n_sys > 0L) {
          na_dropped[[select_names[i]]] <- n_sys
        }
        if (n_user > 0L) {
          user_na_dropped[[select_names[i]]] <- n_user
        }
      }
      x <- x[keep]
      if (!is.null(w)) {
        w <- w[keep]
      }
      if (!length(x)) {
        next
      }
      x_has_na <- !drop_na && anyNA(x)
      if (x_has_na) {
        x <- .cat_missing_row_guard(x, missing_label, select_names[i])
        x <- .add_missing_level(x, missing_label)
        missing_vars <- c(missing_vars, select_names[i])
      }

      ft <- if (is.null(w)) {
        spicy::freq(
          x,
          rescale = rescale,
          valid = FALSE,
          output = "data.frame"
        )
      } else {
        spicy::freq(
          x,
          weights = w,
          rescale = rescale,
          valid = FALSE,
          output = "data.frame"
        )
      }
      vals <- as.character(ft$value)
      raw_levels <- vals[!is.na(vals)]

      keep_i <- levels_keep[[select_names[i]]]
      lv_use <- if (is.null(keep_i)) {
        # The row of missing values goes last, and only a variable that
        # has one: elsewhere a level with its label keeps its own place.
        ml <- if (x_has_na) missing_label else character(0)
        known <- intersect(setdiff(var_level_order, ml), raw_levels)
        extra <- setdiff(raw_levels, c(var_level_order, ml))
        missing_end <- intersect(raw_levels, ml)
        c(known, extra, missing_end)
      } else {
        .cat_kept_levels(levels_keep, select_names[i], raw_levels)
      }
      if (!is.null(keep_i) && length(lv_use) == 0L) {
        .warn_levels_keep_no_match(select_names[i], raw_levels)
        next
      }
      all_level_order <- c(all_level_order, lv_use)

      for (lv in lv_use) {
        idx <- match(lv, vals)
        if (is.na(idx)) {
          next # nocov: lv_use is built from raw_levels (a subset of `vals`), so match() is never NA here. Defensive.
        }
        rows[[rr]] <- data.frame(
          variable = select_names[i],
          level = lv,
          # Per-variable display rank: the global level factor used to
          # impose the FIRST variable's ordering on every later one,
          # hoisting "(Missing)" to the head of subsequent blocks and
          # reordering homonymous levels (field report,
          # dev/bug_missing_order_multivar.md).
          .lvrank = match(lv, lv_use),
          n = suppressWarnings(as.numeric(ft$n[idx])),
          pct = 100 * suppressWarnings(as.numeric(ft$prop[idx])),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        rr <- rr + 1L
      }
    }

    if (length(rows) == 0) {
      long_raw <- data.frame(
        variable = character(0),
        level = character(0),
        n = numeric(0),
        pct = numeric(0),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    } else {
      long_raw <- do.call(rbind, rows)
    }

    if (nrow(long_raw) > 0) {
      long_raw$variable <- factor(long_raw$variable, levels = select_names)
      # Sort on the per-variable rank captured at build time -- each
      # variable keeps its own level order (the one `levels_keep` gives
      # it, when it names its levels), "(Missing)" stays last in every
      # block.
      long_raw <- long_raw[
        order(long_raw$variable, long_raw$.lvrank, method = "radix"),
        ,
        drop = FALSE
      ]
      long_raw$.lvrank <- NULL
      long_raw$variable <- as.character(long_raw$variable)
      long_raw$level <- as.character(long_raw$level)
      rownames(long_raw) <- NULL
    }

    if (output == "long") {
      long_raw <- with_keys(with_label(long_raw))
      # Every data frame carries the note as print() shows it.
      attr(long_raw, "note") <- .categorical_note(
        build_weights_note(),
        build_missing_note()
      )
      return(long_raw)
    }

    wide_raw <- data.frame(
      Variable = label_of_key(long_raw$variable),
      Level = long_raw$level,
      n = long_raw$n,
      check.names = FALSE
    )
    wide_raw[["%"]] <- long_raw$pct

    report_cols <- c(.CAT_KEY_VARIABLE, "n", "%")
    # The header each output route prints, indexed BY the frozen column
    # name: every engine below looks its labels up with
    # `report_labels[names(df)]` instead of re-typing them, so the
    # console, the three rendering engines, the sheet and the clipboard
    # can never disagree about a header.
    report_labels <- setNames(
      c(
        spicy_str("header_variable"),
        spicy_str("header_n_lower"),
        spicy_str("header_percent_symbol")
      ),
      report_cols
    )
    make_report_wide_oneway <- function(mode = c("char", "excel")) {
      mode <- match.arg(mode)

      if (nrow(long_raw) == 0) {
        if (mode == "char") {
          return(as.data.frame(
            setNames(
              replicate(length(report_cols), character(0), simplify = FALSE),
              report_cols
            ),
            check.names = FALSE
          ))
        }
        out <- as.data.frame(
          setNames(
            replicate(length(report_cols), numeric(0), simplify = FALSE),
            report_cols
          ),
          check.names = FALSE
        )
        out$Variable <- character(0)
        return(out[, report_cols, drop = FALSE])
      }

      out <- list()
      z <- 1L
      for (k in seq_along(labels)) {
        lab <- labels[[k]]
        sv <- long_raw[long_raw$variable == select_names[[k]], , drop = FALSE]
        if (nrow(sv) == 0) {
          next
        }
        # A variable with no data and no level to show: its one row
        # carries the undefined cells itself.
        no_level <- all(is.na(sv$level))

        keep_k <- levels_keep[[select_names[[k]]]]
        lv_use <- if (no_level) {
          character(0)
        } else if (is.null(keep_k)) {
          unique(sv$level)
        } else {
          intersect(keep_k, unique(sv$level))
        }

        if (mode == "char") {
          r0 <- as.list(setNames(rep("", length(report_cols)), report_cols))
          if (no_level) {
            r0$n <- undefined_cell
            r0[["%"]] <- undefined_cell
          }
        } else {
          r0 <- as.list(setNames(rep(NA, length(report_cols)), report_cols))
        }
        r0$Variable <- lab
        out[[z]] <- as.data.frame(
          r0,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        z <- z + 1L

        for (lv in lv_use) {
          sl <- sv[sv$level == lv, , drop = FALSE]
          if (mode == "char") {
            r1 <- as.list(setNames(rep("", length(report_cols)), report_cols))
            # A count with no number is a variable with no data.
            r1$n <- fmt_n(sl$n[1], na = undefined_cell)
            r1[["%"]] <- fmt_num(sl$pct[1], percent_digits, na = undefined_cell)
          } else {
            r1 <- as.list(setNames(rep(NA, length(report_cols)), report_cols))
            r1$n <- sl$n[1]
            r1[["%"]] <- sl$pct[1]
          }
          r1$Variable <- paste0(indent_text, lv)
          out[[z]] <- as.data.frame(
            r1,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
          z <- z + 1L
        }
      }

      do.call(rbind, out)
    }

    report_wide_char <- make_report_wide_oneway("char")
    report_wide_excel <- make_report_wide_oneway("excel")

    # Read the ledger once, here: every output route below must carry
    # the same disclosure, not print() alone. `oneway_note` is that
    # disclosure as the note the engines print, assembled (and opened
    # with "Note.") by the same `.categorical_note()` as print().
    what_note <- build_weights_note()
    missing_note <- build_missing_note()
    oneway_note <- .categorical_note(what_note, missing_note)

    # Typed view of the SAME rows the display frames hold, built from
    # the compute frame, never parsed back from a label string. Every
    # output route below reads the block geometry (which rows open a
    # variable block, which rows are levels) from its `.row_role`
    # column, so a label that happens to start with `indent_text`
    # cannot be mistaken for a level row.
    structured <- .build_categorical_structured(
      long_raw = long_raw,
      select_names = select_names,
      labels = labels,
      levels_keep = levels_keep,
      missing_label = missing_label,
      missing_vars = missing_vars,
      indent_text = indent_text,
      percent_digits = percent_digits,
      p_digits = p_digits,
      effect_size_digits = effect_size_digits,
      decimal_mark = decimal_mark
    )

    if (output %in% c("default", "data.frame")) {
      out <- wide_raw
      attr(out, "display_df") <- report_wide_char
      attr(out, "group_var") <- NULL
      # The title the console prints, from the helper every engine uses.
      attr(out, "title") <- .categorical_title(NULL)
      # The whole note, as print() shows it; `what_note` and
      # `missing_note` stay the parts print() assembles it from.
      attr(out, "note") <- oneway_note
      attr(out, "what_note") <- what_note
      attr(out, "missing_note") <- missing_note
      attr(out, "indent_text") <- indent_text
      attr(out, "align") <- align
      attr(out, "decimal_mark") <- decimal_mark
      # The long frame of `output = "long"`, for tidy() and glance().
      attr(out, "long_data") <- with_keys(with_label(long_raw))
      attr(out, "structured") <- structured
      # See `.style_stamp()`: the style levers with no argument have to
      # outlive this call, because `inline()` re-formats the typed body
      # to cite a cell.
      out <- .style_stamp(out)
      class(out) <- c("spicy_categorical_table", "spicy_table", "data.frame")
      # `output = "data.frame"` is the displayed table with its numbers,
      # read off the object the call would print, so it is
      # `as.data.frame()` of that object by construction.
      if (output == "data.frame") {
        return(.categorical_blank_na(as.data.frame(out), blank_na_wide))
      }
      # Return VISIBLY and let standard auto-print dispatch to
      # print.spicy_categorical_table(): a bare table_categorical(...)
      # call still displays the table, while `t <- table_categorical(...)`
      # is silent (the freq() / cross_tab() model).
      return(out)
    }

    if (output == "tinytable") {
      if (!requireNamespace("tinytable", quietly = TRUE)) {
        spicy_abort("Install package 'tinytable'.", class = "spicy_missing_pkg")
      }
      old_tt_opt <- getOption("tinytable_print_output")
      options(tinytable_print_output = "html")
      on.exit(options(tinytable_print_output = old_tt_opt), add = TRUE)

      dat_tt <- report_wide_char
      # Pre-pad numeric cells so centering stacks the decimal points
      # vertically (same strategy as table_regression() /
      # table_continuous_lm()). The native tinytable
      # `style_tt(align = "d")` centers each cell on its own value
      # rather than on the decimal mark, which is inconsistent with
      # the rendering used by the other engines.
      dat_tt <- pad_decimal_cols(dat_tt)
      mod_rows <- .categorical_level_rows_typed(structured)
      # Rule above the first row of each variable block (the console
      # draws the same dashed separator between blocks).
      var_sep_rows <- .categorical_sep_rows_typed(structured)
      if (length(mod_rows)) {
        dat_tt[[1]][mod_rows] <- paste0(
          strrep("\u00A0", 4),
          substring(dat_tt[[1]][mod_rows], nchar(indent_text) + 1L)
        )
      }
      # "Variable" is what the console and the `by =` branch put over
      # the row labels; a blank corner made the engine disagree with
      # itself depending on whether `by` was supplied.
      names(dat_tt) <- unname(report_labels[names(dat_tt)])

      tt <- tinytable::tt(
        dat_tt,
        caption = .categorical_title(NULL),
        notes = oneway_note
      )
      tt <- .spicy_tt_bare(tt)
      # User data reaches the cells (level labels, variable labels):
      # escape it, like every other engine of the family does.
      tt <- .spicy_tt_escape(tt)
      tt <- tinytable::style_tt(tt, j = 1, align = "l")
      tt_align <- switch(
        align,
        decimal = "c",
        center = "c",
        right = "r",
        "r"
      )
      tt <- tinytable::style_tt(
        tt,
        j = 2:ncol(dat_tt),
        align = tt_align
      )
      tt <- tinytable::style_tt(tt, i = 0, j = 2:ncol(dat_tt), align = "c")
      if (length(mod_rows)) {
        tt <- tinytable::style_tt(tt, i = mod_rows, j = 1, indent = 1)
        tt <- tinytable::style_tt(
          tt,
          i = mod_rows,
          j = 1,
          html_css = "padding-left: 0.8em;"
        )
      }
      tt <- tinytable::style_tt(
        tt,
        i = 0,
        j = seq_len(ncol(dat_tt)),
        line = "t",
        line_width = 0.06
      )
      tt <- tinytable::style_tt(
        tt,
        i = 0,
        j = seq_len(ncol(dat_tt)),
        line = "b",
        line_width = 0.06
      )
      tt <- tinytable::style_tt(
        tt,
        i = nrow(dat_tt),
        j = seq_len(ncol(dat_tt)),
        line = "b",
        line_width = 0.06
      )
      # Light separators between variable blocks (same rule
      # table_continuous() draws).
      for (sr in var_sep_rows) {
        tt <- tinytable::style_tt(
          tt,
          i = sr - 1L,
          j = seq_len(ncol(dat_tt)),
          line = "b",
          line_width = 0.03
        )
      }
      return(tt)
    }

    if (output == "gt") {
      if (!requireNamespace("gt", quietly = TRUE)) {
        spicy_abort("Install package 'gt'.", class = "spicy_missing_pkg")
      }
      dat_gt <- report_wide_char
      # Pre-pad numeric cells so centering stacks the decimal points
      # vertically (same strategy as table_regression() /
      # table_continuous_lm()). gt's native `cols_align_decimal()`
      # renders visually right-aligned, which is inconsistent with
      # the rendering used by the other engines.
      dat_gt <- pad_decimal_cols(dat_gt)
      mod_rows <- .categorical_level_rows_typed(structured)
      # Same light rule the console and the other engines draw between
      # variable blocks, from the same typed geometry.
      var_sep_rows <- .categorical_sep_rows_typed(structured)
      if (length(mod_rows)) {
        dat_gt[[1]][mod_rows] <- paste0(
          strrep("\u00A0", 4),
          substring(dat_gt[[1]][mod_rows], nchar(indent_text) + 1L)
        )
      }
      names(dat_gt) <- c(.CAT_KEY_VARIABLE, "n", "pct")
      tbl <- gt::gt(dat_gt)
      # "Variable", not a blanked corner: the console, tinytable and
      # flextable all label the first column, and gt was the last
      # dissenter (lot B incident).
      # `names(dat_gt)` above are gt's machine ids and stay literal; only
      # the labels come from the registry.
      tbl <- gt::cols_label(
        tbl,
        Variable = report_labels[[.CAT_KEY_VARIABLE]],
        n = report_labels[["n"]],
        pct = report_labels[["%"]]
      )
      tbl <- gt::cols_align(tbl, align = "left", columns = .CAT_KEY_VARIABLE)
      if (identical(align, "decimal")) {
        tbl <- gt::cols_align(tbl, align = "center", columns = c("n", "pct"))
      } else if (identical(align, "center")) {
        tbl <- gt::cols_align(tbl, align = "center", columns = c("n", "pct"))
      } else {
        tbl <- gt::cols_align(tbl, align = "right", columns = c("n", "pct"))
      }
      rule <- gt::cell_borders(
        sides = "bottom",
        color = "currentColor",
        weight = gt::px(1)
      )
      rule_top <- gt::cell_borders(
        sides = "top",
        color = "currentColor",
        weight = gt::px(1)
      )
      tbl <- gt::tab_options(
        tbl,
        table.border.top.width = gt::px(0),
        table.border.bottom.width = gt::px(0),
        table_body.border.top.width = gt::px(0),
        table_body.border.bottom.width = gt::px(0),
        table_body.hlines.color = "transparent",
        column_labels.border.top.width = gt::px(0),
        column_labels.border.bottom.width = gt::px(0),
        column_labels.border.lr.color = "transparent"
      )
      tbl <- gt::tab_style(
        tbl,
        style = rule_top,
        locations = gt::cells_column_labels()
      )
      tbl <- gt::tab_style(
        tbl,
        style = rule,
        locations = gt::cells_column_labels()
      )
      tbl <- gt::tab_style(
        tbl,
        style = rule,
        locations = gt::cells_body(rows = nrow(dat_gt))
      )
      # Light separators between variable blocks (console / tinytable /
      # flextable parity; same style as table_continuous()'s gt branch).
      light_rule <- gt::cell_borders(
        sides = "bottom",
        color = "#cccccc",
        weight = gt::px(0.5)
      )
      for (sr in var_sep_rows) {
        tbl <- gt::tab_style(
          tbl,
          style = light_rule,
          locations = gt::cells_body(rows = sr - 1L)
        )
      }
      # The same title the five other engines print.
      tbl <- .spicy_gt_apa_title(tbl, .categorical_title(NULL))
      return(.spicy_gt_attach_note(tbl, oneway_note))
    }

    build_flextable_oneway <- function(df) {
      if (!requireNamespace("flextable", quietly = TRUE)) {
        spicy_abort(
          "Install package 'flextable'.",
          class = "spicy_missing_pkg",
          call = rlang::caller_env()
        )
      }
      df <- pad_decimal_cols(df)
      # Level rows carry the console's indent inside the label cell
      # and the engine indents them again below (`padding.left`). One
      # indentation is the design; two is an artifact of reading a
      # display string as data -- and Word keeps the literal spaces.
      # Keep the engine's indent, which survives every backend, and
      # hand the cell the bare level name (same rule as the gt and
      # tinytable branches).
      id_mod <- .categorical_level_rows_typed(structured)
      # Same light rule the console and tinytable draw between variable
      # blocks, from the same typed geometry.
      var_sep_rows <- .categorical_sep_rows_typed(structured)
      if (length(id_mod)) {
        df[[1L]][id_mod] <- substring(
          df[[1L]][id_mod],
          nchar(indent_text) + 1L
        )
      }
      ft <- flextable::flextable(df)
      map <- data.frame(
        col_keys = names(df),
        label = unname(report_labels[names(df)]),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      ft <- flextable::set_header_df(ft, mapping = map, key = "col_keys")
      bd <- spicy_fp_border(color = "black", width = 1)
      ft <- flextable::align(ft, j = 1, part = "all", align = "left")
      # Numeric column alignment honors `align`. For "decimal" the
      # cells were pre-padded above; right-aligning the padded strings
      # preserves the dot-aligned column. Use a monospace font in the
      # body so character widths match. For "center" / "right",
      # apply the literal alignment.
      num_j <- 2:ncol(df)
      if (identical(align, "decimal") && length(num_j) > 0L) {
        # Single-font policy (matches table_regression()):
        # cells were pre-padded above for uniform width; centering
        # in the default body font produces approximate decimal
        # alignment without forcing a monospace override.
        ft <- flextable::align(
          ft,
          j = num_j,
          part = "header",
          align = "center"
        )
        ft <- flextable::align(
          ft,
          j = num_j,
          part = "body",
          align = "center"
        )
      } else if (identical(align, "center") && length(num_j) > 0L) {
        ft <- flextable::align(ft, j = num_j, part = "all", align = "center")
      } else {
        ft <- flextable::align(ft, j = num_j, part = "all", align = "right")
      }
      ft <- flextable::hline_top(ft, part = "header", border = bd)
      ft <- flextable::hline_bottom(ft, part = "header", border = bd)
      ft <- flextable::hline_bottom(ft, part = "body", border = bd)
      # Light separators between variable blocks (console / tinytable /
      # table_continuous() flextable parity).
      bd_light <- spicy_fp_border(color = "#cccccc", width = 0.5)
      for (sr in var_sep_rows) {
        ft <- flextable::hline(
          ft,
          i = sr - 1L,
          part = "body",
          border = bd_light
        )
      }
      if (length(id_mod)) {
        ft <- flextable::padding(
          ft,
          i = id_mod,
          j = 1,
          part = "body",
          padding.left = 14
        )
      }
      ft <- flextable::autofit(ft)
      ft <- .spicy_ft_attach_note(ft, oneway_note)
      class(ft) <- c("spicy_flextable", class(ft))
      ft
    }

    if (output == "flextable") {
      # Same title the console prints, from the same helper: a table
      # that names itself on screen names itself in HTML too.
      return(.spicy_ft_html_caption(
        build_flextable_oneway(report_wide_char),
        .categorical_title(NULL)
      ))
    }

    if (output == "word") {
      .check_output_path(word_path, output)
      ft <- build_flextable_oneway(report_wide_char)
      # Same title the console prints, from the same helper: a table
      # that names itself on screen names itself in the document too.
      ft <- .spicy_ft_word_caption(ft, .categorical_title(NULL))
      flextable::save_as_docx(ft, path = word_path)
      return(invisible(word_path))
    }

    # Clipboard text is NOT decimal-padded: the padding character of
    # the fixed-width renderers (U+2007) is not whitespace to a
    # parser, so a padded number pastes as text while its unpadded
    # sibling pastes as a number. Alignment is the console's job; the
    # payload's job is to arrive intact.
    clip_body <- report_wide_char
    clip_body$Variable <- make_stronger_indent(
      clip_body$Variable,
      indent_text,
      indent_text_excel_clipboard,
      .categorical_level_rows_typed(structured)
    )
    # The payload's header row is a HEADER, like every other route's:
    # it reads the labels, not the frame's frozen names.
    clip_mat <- rbind(
      matrix(unname(report_labels[names(clip_body)]), nrow = 1),
      as.matrix(clip_body)
    )

    if (output == "excel") {
      .check_output_path(excel_path, output)
      if (!requireNamespace("openxlsx2", quietly = TRUE)) {
        spicy_abort("Install package 'openxlsx2'.", class = "spicy_missing_pkg")
      }

      body_xl <- report_wide_excel
      body_xl$Variable <- make_stronger_indent(
        body_xl$Variable,
        indent_text,
        indent_text_excel_clipboard,
        .categorical_level_rows_typed(structured)
      )

      wb <- openxlsx2::wb_workbook()
      wb <- openxlsx2::wb_add_worksheet(wb, excel_sheet)
      # Same title the console prints, from the same helper, on the
      # first row; the table starts two rows below (the layout
      # `table_regression()`'s Excel export already uses).
      xl_title <- .categorical_title(NULL)
      wb <- .spicy_xl_add_data(wb, x = xl_title, start_row = 1)
      header_row <- 3L
      # The empty cells of a variable-header row are not written at all
      # (see .spicy_xl_add_data()): written as "#N/A", they would sit in
      # the middle of the counts and any SUM over the column would
      # inherit the error.
      # `col_names = TRUE` writes `names(x)` as the sheet's header row,
      # so the frame handed to the writer carries the LABELS -- the
      # `by` branch already writes its two header rows from the display
      # vectors, and this route used to write the keys instead.
      xl_header <- unname(report_labels[names(body_xl)])
      wb <- .spicy_xl_add_data(
        wb,
        x = setNames(body_xl, xl_header),
        start_row = header_row,
        col_names = TRUE,
        row_names = FALSE
      )
      wb <- .cat_xl_mark_undefined(wb, report_wide_char, 2:3, header_row + 1L)

      nc <- ncol(body_xl)
      first_body_row <- header_row + 1L
      last_row <- header_row + nrow(body_xl)
      pct_fmt <- paste0("0.", paste(rep("0", percent_digits), collapse = ""))

      # Header borders (top + bottom on the column-labels row).
      # IMPORTANT: openxlsx2::wb_add_border() defaults every side to
      # "thin", so left/right must be explicitly NULL to avoid painting
      # vertical rules on every header cell.
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = header_row, cols = 1:nc),
        top_border = "thin",
        bottom_border = "thin",
        left_border = NULL,
        right_border = NULL
      )
      if (nrow(body_xl) > 0) {
        # Body alignment. The Variable column is always left-aligned;
        # numeric columns honor `align`. For "decimal", Excel
        # already aligns decimal points implicitly via right-alignment
        # combined with a uniform numfmt (every cell of the column
        # shares the same number of decimal places), so the visual
        # result matches the dot-aligned column in print / gt /
        # tinytable.
        num_horiz <- if (identical(align, "center")) "center" else "right"
        wb <- openxlsx2::wb_add_cell_style(
          wb,
          dims = openxlsx2::wb_dims(rows = first_body_row:last_row, cols = 1),
          horizontal = "left"
        )
        wb <- openxlsx2::wb_add_cell_style(
          wb,
          dims = openxlsx2::wb_dims(
            rows = first_body_row:last_row,
            cols = 2:nc
          ),
          horizontal = num_horiz
        )
        # Number formats: integers (col 2), percentages (col 3)
        wb <- openxlsx2::wb_add_numfmt(
          wb,
          dims = openxlsx2::wb_dims(rows = first_body_row:last_row, cols = 2),
          numfmt = "0"
        )
        wb <- openxlsx2::wb_add_numfmt(
          wb,
          dims = openxlsx2::wb_dims(rows = first_body_row:last_row, cols = 3),
          numfmt = pct_fmt
        )
        # Bottom border on last row
        wb <- openxlsx2::wb_add_border(
          wb,
          dims = openxlsx2::wb_dims(rows = last_row, cols = 1:nc),
          bottom_border = "thin",
          top_border = NULL,
          left_border = NULL,
          right_border = NULL
        )
      }

      # Disclosure note (what left the table) two rows below the body,
      # one worksheet row per line -- the placement the regression
      # export uses. Same text the console prints.
      wb <- .spicy_xl_add_note(
        wb,
        note = oneway_note,
        start_row = last_row + 2L
      )
      # Widths from the DISPLAY strings (the char body), not from the
      # raw numerics the sheet stores: "5.16949152542373" is a stored
      # value, "5.2" is what the cell shows.
      width_df <- report_wide_char
      width_df$Variable <- body_xl$Variable
      wb <- .spicy_xl_set_widths(
        wb,
        sheet = excel_sheet,
        cells = .spicy_xl_cells(width_df, headers = list(xl_header))
      )

      openxlsx2::wb_save(wb, excel_path, overwrite = TRUE)
      return(invisible(excel_path))
    }

    if (output == "clipboard") {
      .spicy_clip_preflight()
      # Same title and same disclosure note the console prints, from
      # the same helpers: a table that names itself on screen names
      # itself once pasted too.
      txt <- .clipboard_payload_desc(
        clip_mat,
        clipboard_delim,
        title = .categorical_title(NULL),
        note = oneway_note
      )
      clipr::write_clip(txt)
      spicy_inform("Categorical table copied to clipboard.")
      return(invisible(txt))
    }
  }

  g0 <- resolve_user_na(data[[by_name]])
  # A `by` no observation holds leaves no group once its missing values
  # are removed, whatever levels a factor declares: refused, as across
  # the family.
  if (drop_na) {
    .check_grouping_observed(g0, sprintf("`by` (`%s`)", by_name))
  }
  # Resolve `assoc_measure` to a named character vector, one entry per
  # row variable (validates input shape, fills "auto" via the per-row
  # rule, errors on phi-on-non-2x2). The downstream loop reads from
  # this vector instead of the raw user input so each row gets its own
  # measure.
  assoc_measures_per_row <- .resolve_assoc_measures(
    assoc_measure,
    select_names = select_names,
    data = data,
    by_name = by_name,
    user_na = user_na
  )
  show_assoc <- any(assoc_measures_per_row != "none")
  if (!show_assoc) {
    assoc_ci <- FALSE
  }
  # The family's order of the groups (`.group_levels()`): a factor keeps
  # its levels, any other `by` is sorted in the C locale.
  group_levels <- .group_levels(g0)
  # The REAL groups of the table, captured here and used by the SMD --
  # BEFORE the "(Missing)" level is appended below and BEFORE the margin
  # key joins the vector. Both would otherwise inflate the count and
  # refuse a legitimate two-group table (`include_total = TRUE` is the
  # default, so the naive count is off by one on EVERY grouped table),
  # and a `setdiff()` by name would be defeated by a user level
  # homonymous with the margin's key once it is relabeled (see
  # `.cat_margin_key()`).
  real_group_levels <- group_levels
  if (do_smd && length(real_group_levels) != 2L) {
    spicy_abort(
      c(
        sprintf(
          "`smd = TRUE` requires exactly two groups in `by` (found %d).",
          length(real_group_levels)
        ),
        "i" = "The standardized mean difference is a two-group balance diagnostic (Austin 2009); there is no published reading of an average over pairs.",
        # See the continuous twin: the count is of DECLARED levels, so
        # the hint has to name `droplevels()` as well as filtering.
        "i" = "Compare two groups at a time: filter `by` to a pair of levels, and `droplevels()` if a declared level is now empty."
      ),
      class = "spicy_not_implemented"
    )
  }
  # Pin the group-level ORDER on a factor built once, here, so the
  # internal cross_tab() calls can never re-sort it (the pre-0.13.0
  # as.character() round-trip alphabetized ordered `by` factors and
  # corrupted the ordinal association measures). Naming keeps the
  # family convention for `by`: factor levels and character values
  # verbatim, raw codes for labelled vectors.
  g_fac <- if (is.factor(g0)) {
    g0
  } else {
    factor(as.character(g0), levels = group_levels)
  }
  # The column of missing values of `by`. Every level of `by` is displayed,
  # a declared one nobody holds included (a zero column), so one with the
  # label of that column is refused.
  if (!drop_na && any(is.na(g0))) {
    if (missing_label %in% group_levels) {
      .abort_label_collision(
        missing_label,
        by_name,
        "level",
        "the column spicy adds for missing values",
        "column",
        "row_missing_level",
        "(Not stated)"
      )
    }
    group_levels <- c(group_levels, missing_label)
    missing_group <- TRUE
  }
  # A group is always left to tabulate here: data with no rows is refused
  # by `.check_data_frame()`, and a `by` every observation misses is
  # refused above under `drop_na = TRUE` and is the "(Missing)" group
  # under `drop_na = FALSE`. Without a group the columns, keyed
  # `paste0(<level>, " n")`, would be the phantom pair " n" / " %" (R
  # recycles the zero-length side to ""), which every rendering engine
  # refused with an error naming something else.
  #
  # Internal KEY for the margin column, never a label: it is a value of
  # `long$group`, the prefix of a public column name ("Total n") and the
  # `total_group` attribute, and every site that recognizes the margin --
  # the wide and report fills, the typed view's `total` flag, tidy() and
  # glance() -- compares it against another key, never against a rendered
  # header. A level of `by` displayed with the margin's label is refused
  # (`.cat_margin_key()`); the key is "Total" unless a level holds it, as
  # under a margin relabeled through `options(spicy.labels)`. (Before
  # 0.13.0 the margin silently vanished and the user's "Total" group posed
  # as it.)
  margin_key <- .cat_margin_key(
    group_levels,
    by_name,
    check = include_total,
    missing_label = if (!drop_na && any(is.na(g0))) missing_label
  )
  if (include_total) {
    group_levels <- c(group_levels, margin_key)
  }
  # The margin key names a COLUMN only when the margin is displayed;
  # otherwise it is a disambiguation artifact with no header to label.
  margin_col_key <- if (include_total) margin_key else NULL
  # The internal cross_tab() calls rename their own margin when `g` has a
  # level "Total" (the margin then relabeled here), with a warning that
  # names a column this table never shows: muffled as noise.
  quiet_margin <- function(expr) {
    withCallingHandlers(
      expr,
      spicy_renamed_column = function(w) invokeRestart("muffleWarning")
    )
  }

  # ---------------- LONG RAW ----------------
  rows <- list()
  rr <- 1L
  measure_col <- NULL
  all_level_order <- character(0)
  # The test key of each variable shown (NA where none ran), and whether
  # Yates' correction reached it; read by the note after the loop.
  test_types <- character(0)
  yates_used <- logical(0)
  # The variables with no value at all (no data in any group), whose
  # association measure the note does not name (`.assoc_note_apa()`).
  no_value_vars <- character(0)

  # One row of the compute frame: one variable, level and group. `st`
  # holds the statistics of the variable (`parse_stats()`), which every
  # row of its block carries.
  cat_row <- function(
    var,
    lv,
    rank,
    gr,
    n,
    pct,
    test_type,
    st,
    smd_val,
    smd_kind
  ) {
    row_df <- data.frame(
      variable = var,
      level = lv,
      # Per-variable display rank (see the no-by branch): keeps each
      # block's own level order under the global sort.
      .lvrank = rank,
      group = gr,
      n = n,
      pct = pct,
      test_type = test_type,
      chi2 = st$chi2 %||% NA_real_,
      df = st$df %||% NA_real_,
      p = st$p,
      p_op = st$p_op,
      ci_lower = st$ci_lower,
      ci_upper = st$ci_upper,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    if (show_assoc) {
      # Defer the rename `.assoc` -> measure_col to post-loop, where we
      # know whether all rows used the same measure (uniform column
      # header) or a mix (collapsed to the generic name).
      row_df$.assoc <- st$v
      # The measure this row actually carries. A row whose variable
      # asked for none has a value but no type.
      row_df$.assoc_type <- if (identical(st$measure, "none")) {
        NA_character_
      } else {
        st$measure
      }
    }
    if (do_smd) {
      row_df$.smd <- smd_val
      # Which kernel produced it -- and therefore whether it is signed.
      # `"binary"` is signed (group 1 minus group 2 on the second
      # level); `"multinomial"` is a distance and never is.
      row_df$.smd_type <- smd_kind
    }
    row_df
  }

  for (i in seq_along(select_names)) {
    x_raw <- data[[select_names[i]]]
    # Factor built BEFORE tabulation (see .tab_factor): declared level
    # order and haven value labels survive end-to-end, so cross_tab()
    # computes the ordinal association measures on the table in the
    # declared ordinal order. The data never round-trips through
    # as.character().
    x <- .tab_factor(resolve_user_na(x_raw))
    g <- g_fac
    w <- weights_vec

    # Original level order, BEFORE the "(Missing)" level is appended.
    var_level_order <- levels(x)

    # The groups in which this variable has no data (`no_data_groups`).
    # Their rows leave the variable here, before anything is counted, so
    # the missing-value counts, the margin, the `(Missing)` row, the test
    # and the association measure read the groups that have data; the
    # cells of those groups are NA below.
    nd_i <- .no_data_groups(x, g, real_group_levels)
    nd_rows <- !is.na(g) & as.character(g) %in% nd_i
    n_user <- if (user_na) sum(.user_na_mask(x_raw) & !nd_rows) else 0L
    if (any(nd_rows)) {
      x <- x[!nd_rows]
      g <- g[!nd_rows]
      if (!is.null(w)) {
        w <- w[!nd_rows]
      }
    }
    # With fewer than two groups that have data, nothing is compared.
    data_groups <- intersect(
      real_group_levels,
      as.character(g[!is.na(x) & !is.na(g)])
    )
    few_groups <- length(nd_i) > 0L && length(data_groups) < 2L

    keep <- rep(TRUE, length(x))
    if (drop_na) {
      keep <- !is.na(x) & !is.na(g)
      nd_x <- sum(is.na(x)) - n_user
      if (nd_x > 0L) {
        na_dropped[[select_names[i]]] <- nd_x
      }
      if (n_user > 0L) {
        user_na_dropped[[select_names[i]]] <- n_user
      }
      by_na_dropped <- max(by_na_dropped, sum(is.na(g)))
    }

    x <- x[keep]
    g <- g[keep]
    if (!is.null(w)) {
      w <- w[keep]
    }
    this_measure <- assoc_measures_per_row[[select_names[i]]]
    if (!length(x)) {
      if (!length(nd_i)) {
        next
      }
      # No data in any group that holds rows: the variable keeps its row
      # shape -- its declared levels, or its one row (level NA) when it
      # declares none -- with no count in those groups and in the margin,
      # an empty group keeping its zero, and nothing tested.
      keep_i <- levels_keep[[select_names[i]]]
      lv_use <- if (is.null(keep_i)) {
        var_level_order
      } else {
        .cat_kept_levels(levels_keep, select_names[i], var_level_order)
      }
      if (!is.null(keep_i) && length(lv_use) == 0L) {
        .warn_levels_keep_no_match(select_names[i], var_level_order)
        next
      }
      if (length(lv_use) == 0L) {
        lv_use <- NA_character_
      }
      no_data_groups[[select_names[i]]] <- nd_i
      no_value_vars <- c(no_value_vars, select_names[i])
      test_types[[select_names[i]]] <- NA_character_
      yates_used[[select_names[i]]] <- FALSE
      st_none <- list(
        p = NA_real_,
        p_op = NA_character_,
        v = NA_real_,
        measure = this_measure,
        chi2 = NA_real_,
        df = NA_real_,
        ci_lower = NA_real_,
        ci_upper = NA_real_
      )
      for (k in seq_along(lv_use)) {
        for (gr in group_levels) {
          void <- gr %in% nd_i || identical(gr, margin_key)
          rows[[rr]] <- cat_row(
            select_names[i],
            lv_use[[k]],
            k,
            gr,
            n = if (void) NA_real_ else 0,
            pct = if (void) NA_real_ else 0,
            test_type = NA_character_,
            st = st_none,
            smd_val = NA_real_,
            smd_kind = if (length(var_level_order)) {
              .smd_categorical_type(length(var_level_order))
            } else {
              NA_character_
            }
          )
          rr <- rr + 1L
        }
      }
      next
    }
    if (!drop_na) {
      x_has_na <- anyNA(x)
      g_has_na <- anyNA(g)
      if (x_has_na) {
        x <- .cat_missing_row_guard(x, missing_label, select_names[i])
        x <- .add_missing_level(x, missing_label)
        missing_vars <- c(missing_vars, select_names[i])
      }
      if (g_has_na) {
        g <- .add_missing_level(g, missing_label)
      }
    } else {
      x_has_na <- FALSE
      g_has_na <- FALSE
    }

    # Association statistics are computed on the OBSERVED cells only:
    # the "(Missing)" display level never enters the test (the
    # gtsummary / SPSS convention -- show the missing, test the
    # observed). When a "(Missing)" level is present, the displayed
    # counts / percents keep it while the statistics come from a
    # separate complete-case pass.
    has_missing_level <- !drop_na && (x_has_na || g_has_na)
    # Full precision end-to-end: `digits = 15` makes cross_tab()'s
    # cell rounding a no-op (round(x, 15) is the identity within
    # double precision), so the machine outputs carry the exact
    # values and rounding happens once, at display time. Statistics
    # come only from the call that actually feeds them
    # (`include_stats` is FALSE on the others), so disclosures such
    # as the ignored-Yates warning fire once, with the dimensions of
    # the table actually tested. The counts come first: their call
    # validates the weights before the test reads them.
    ct_n <- quiet_margin(spicy::cross_tab(
      x,
      g,
      weights = w,
      rescale = rescale,
      digits = 15L,
      include_stats = FALSE
    ))
    # The test of this variable (`test`), read on the table it runs on:
    # the complete cases of the observed levels, as the statistics below.
    # Decided before the chi-squared is computed, so that `correct` and
    # `simulate_p` reach only a variable the chi-squared tests -- a Yates
    # warning about a table the exact test reads, or Monte Carlo draws for
    # a chi-squared nobody sees, would be noise.
    # Each side drops its OWN row or column of missing values, and only
    # when it has one: a level with that label on the other side is a
    # level and is tested.
    cc <- rep(TRUE, length(x))
    if (x_has_na) {
      cc <- cc & x != missing_label
    }
    if (g_has_na) {
      cc <- cc & g != missing_label
    }
    test_kind <- NA_character_
    test_tab <- NULL
    if (p_value) {
      test_tab <- .cat_test_table(
        x[cc],
        g[cc],
        if (is.null(w)) NULL else w[cc],
        rescale
      )
      if (all(dim(test_tab) > 1L)) {
        test_kind <- .cat_resolve_test(test_tab, test)
      }
      # The exact tests condition on counts. The test the call asked for
      # by name stops; one a rule chose (`"auto"`, Campbell's rule of
      # `"n1"`) gives way to the chi-squared it would otherwise take.
      if (
        test_kind %in% c("fisher", "midp") && !.cat_is_count_table(test_tab)
      ) {
        if (identical(test_kind, test)) {
          spicy_abort(
            c(
              sprintf(
                "`test = \"%s\"` needs counts: the weighted table of `%s` has cells that are not whole numbers.",
                test,
                select_names[i]
              ),
              "i" = "Use `test = \"chisq\"` or `\"auto\"`, frequency weights in whole numbers, or `table_categorical_svy()` for survey weights."
            ),
            class = "spicy_unsupported"
          )
        }
        n1_rule <- identical(test, "n1") && all(dim(test_tab) == 2L)
        spicy_warn(
          c(
            sprintf(
              if (n1_rule) {
                "`%s` keeps the N-1 chi-squared test although an expected count is below 1: Fisher's exact test needs counts, and its weighted table has cells that are not whole numbers."
              } else {
                "`%s` keeps the chi-squared test although an expected count is below 5: Fisher's exact test needs counts, and its weighted table has cells that are not whole numbers."
              },
              select_names[i]
            ),
            "i" = "Frequency weights in whole numbers give the exact test; `table_categorical_svy()` gives the design-based test for survey weights."
          ),
          class = "spicy_fallback"
        )
        test_kind <- if (n1_rule) "n1" else "chisq"
      }
    }
    chi_correct <- correct && identical(test_kind, "chisq")
    chi_simulate <- simulate_p && identical(test_kind, "chisq")
    ct_pct <- quiet_margin(spicy::cross_tab(
      x,
      g,
      percent = "c",
      weights = w,
      rescale = rescale,
      correct = chi_correct,
      simulate_p = chi_simulate,
      simulate_B = simulate_B,
      digits = 15L,
      include_stats = !has_missing_level,
      assoc_measure = this_measure,
      assoc_ci = assoc_ci
    ))
    st <- if (has_missing_level) {
      ct_stats <- quiet_margin(spicy::cross_tab(
        x[cc],
        g[cc],
        percent = "c",
        weights = if (!is.null(w)) w[cc] else NULL,
        rescale = rescale,
        correct = chi_correct,
        simulate_p = chi_simulate,
        simulate_B = simulate_B,
        assoc_measure = this_measure,
        assoc_ci = assoc_ci
      ))
      parse_stats(ct_stats, this_measure)
    } else {
      parse_stats(ct_pct, this_measure)
    }
    # Fisher's exact test, its mid-p version, or the N-1 chi-squared
    # replaces the Pearson p-value; the association measure, computed on
    # the same table, stays. An exact test has no chi-squared statistic
    # and no degrees of freedom; the N-1 row carries its own.
    test_type <- NA_character_
    if (test_kind %in% c("fisher", "midp", "n1")) {
      res <- .cat_run_test(test_tab, test_kind, simulate_B)
      st$p <- res$p
      st$p_op <- "="
      st$chi2 <- res$chi2
      st$df <- res$df
      test_type <- res$type
    } else if (identical(test_kind, "chisq") && !is.na(st$p)) {
      test_type <- .CAT_TEST_CHISQ
    }
    # Fewer than two groups with data: no test and no association
    # measure, whatever was computed on the one group left. The `p` cell
    # stays blank, and the note names no test for this variable.
    if (few_groups) {
      st[c("p", "v", "chi2", "df", "ci_lower", "ci_upper")] <- NA_real_
      test_type <- NA_character_
    }

    # --- standardized mean difference ---
    # Complete cases on the OBSERVED groups and the observed levels,
    # the same restriction the association statistics take above: a
    # "(Missing)" display level is shown and never tested. The profiles
    # are built here from `x` / `g` / `w` directly and NEVER from
    # `cross_tab()`, whose last column is the margin.
    #
    # `rescale` does not enter: a profile of proportions is invariant to
    # a global rescaling of the weights, and the Bernoulli and
    # multinomial variances are functions of the proportions alone. The
    # weighted categorical SMD therefore matches the survey-design
    # convention as well as the frequency one -- the two only part
    # company on the continuous arm.
    smd_val <- NA_real_
    smd_kind <- NA_character_
    # A group with no data leaves the two-group SMD undefined, which is
    # not the infinite distance warned about below: NA, in silence.
    if (do_smd && any(real_group_levels %in% nd_i)) {
      smd_kind <- .smd_categorical_type(length(var_level_order))
    } else if (do_smd) {
      cc <- !is.na(x) & !is.na(g)
      if (x_has_na) {
        cc <- cc & x != missing_label
      }
      if (g_has_na) {
        cc <- cc & g != missing_label
      }
      if (!is.null(w)) {
        cc <- cc & !is.na(w) & w > 0
      }
      # The kernel follows the DECLARED level count, not the observed
      # one: a three-level factor with an empty level is a three-level
      # factor, and its SMD is an unsigned distance whether or not the
      # sample happened to fill every level. Signedness must not depend
      # on the draw.
      smd_kind <- .smd_categorical_type(length(var_level_order))
      profiles <- lapply(real_group_levels, function(lev) {
        idx <- which(cc & as.character(g) == lev)
        .smd_props_base(
          x[idx],
          var_level_order,
          if (is.null(w)) NULL else w[idx]
        )
      })
      est <- if (identical(smd_kind, "binary")) {
        # P(second level), the `propTables[, -1]` convention.
        lapply(profiles, function(p) p[[2L]])
      } else {
        profiles
      }
      smd_raw <- .smd_pair_dispatch(est[[1L]], est[[2L]], smd_kind)
      reason <- .smd_undefined_reason(smd_raw)
      if (!is.null(reason)) {
        spicy_warn(
          sprintf(
            if (identical(reason, "constant_levels")) {
              "The standardized mean difference is undefined for `%s`: each group is constant on a different category, so the standardized distance is infinite. Its cell is NA."
            } else {
              "The standardized mean difference is undefined for `%s`: the two groups have no overlapping categories, so the standardized distance is infinite. Its cell is NA."
            },
            select_names[i]
          ),
          class = "spicy_undefined_stat"
        )
      }
      smd_val <- as.numeric(smd_raw)
    }

    # The console cross_tab object is read structurally: column 1 is
    # the row identifier and the LAST column is the margin (either may
    # have been auto-renamed on collision with a user level), and the
    # margin row sits at the attribute-flagged index. Positions and
    # attributes, never reserved names, so user levels literally named
    # "Values", "Total", or "N" survive intact.
    groups_present <- names(ct_n)[-c(1L, ncol(ct_n))]
    # A `by` level declared but never observed is a real group with
    # zero observations: keep it so the wide and long outputs agree on
    # an explicit zero column (0 n, 0.0 %) instead of NA cells in wide
    # and a missing group in long (audit phase 2, finding 29).
    groups_use <- intersect(
      group_levels,
      c(groups_present, margin_key, levels(g_fac))
    )

    vals_n <- as.character(ct_n[[1L]])
    vals_p <- as.character(ct_pct[[1L]])

    margin_rows_n <- attr(ct_n, "total_row_idx")
    raw_levels <- unique(vals_n[setdiff(seq_along(vals_n), margin_rows_n)])
    keep_i <- levels_keep[[select_names[i]]]
    lv_use <- if (is.null(keep_i)) {
      # Reorder to match original factor/occurrence order, the row of
      # missing values last in the variable that has one (see the no-by
      # branch).
      ml <- if (x_has_na) missing_label else character(0)
      known <- intersect(setdiff(var_level_order, ml), raw_levels)
      extra <- setdiff(raw_levels, c(var_level_order, ml))
      missing_end <- intersect(raw_levels, ml)
      c(known, extra, missing_end)
    } else {
      .cat_kept_levels(
        levels_keep,
        select_names[i],
        raw_levels,
        available = vals_n
      )
    }
    if (!is.null(keep_i) && length(lv_use) == 0L) {
      .warn_levels_keep_no_match(select_names[i], raw_levels)
      next
    }
    all_level_order <- c(all_level_order, lv_use)
    # For the note: the test of each variable the table shows, and
    # whether Yates' correction reached it (cross_tab() applies it to a
    # tested 2 x 2 table only).
    test_types[[select_names[i]]] <- test_type
    yates_used[[select_names[i]]] <- chi_correct &&
      !is.na(test_type) &&
      all(dim(test_tab) == 2L)
    if (length(nd_i)) {
      no_data_groups[[select_names[i]]] <- nd_i
    }

    for (lv in lv_use) {
      in_n <- match(lv, vals_n)
      in_p <- match(lv, vals_p)
      if (is.na(in_n) || is.na(in_p)) {
        next # nocov: lv_use is derived from vals_n, and ct_n / ct_pct share the same x,g so they expose identical levels; neither match() is NA. Defensive.
      }

      for (gr in groups_use) {
        # The margin is read positionally (last column): its name may
        # have been auto-renamed by cross_tab() when a `by` level is
        # literally called "Total". User groups are looked up by name
        # among the INTERIOR columns only (`groups_present`): cross_tab
        # renames its margin on an OBSERVED collision, so a
        # declared-but-unobserved level named "Total" leaves the margin
        # column literally called "Total" -- a whole-table name lookup
        # would hand the margin counts to that zero-observation group.
        n_val <- if (identical(gr, margin_key)) {
          ct_n[in_n, ncol(ct_n)]
        } else if (gr %in% groups_present) {
          ct_n[in_n, gr]
        } else {
          # Declared-but-unobserved group: zero count, zero percent.
          0
        }
        pct_val <- if (identical(gr, margin_key)) {
          ct_pct[in_p, ncol(ct_pct)]
        } else if (gr %in% groups_present) {
          ct_pct[in_p, gr]
        } else {
          0
        }
        # A group in which the variable has no data has no count: NA,
        # which every route shows as `cell_undefined`.
        if (gr %in% nd_i) {
          n_val <- NA_real_
          pct_val <- NA_real_
        }
        rows[[rr]] <- cat_row(
          select_names[i],
          lv,
          match(lv, lv_use),
          gr,
          n = suppressWarnings(as.numeric(n_val)),
          pct = suppressWarnings(as.numeric(pct_val)),
          test_type = test_type,
          st = st,
          smd_val = smd_val,
          smd_kind = smd_kind
        )
        rr <- rr + 1L
      }
    }
  }

  # Collapse the per-variable measure vector into the column header
  # the printed / wide outputs will use:
  #   * one measure used everywhere -> that measure's name
  #   * mixed measures              -> the generic effect-size name
  # Row-level `.assoc` cells are then renamed once, below.
  shown_measures <- assoc_measures_per_row[assoc_measures_per_row != "none"]
  unique_shown <- unique(unname(shown_measures))
  measure_col <- if (length(unique_shown) == 1L) {
    .assoc_key(unique_shown)
  } else if (length(unique_shown) > 1L) {
    .CAT_KEY_EFFECT_SIZE
  } else {
    # show_assoc is FALSE in this branch; placeholder name. Still a
    # frozen key, or it would drift from its twin.
    .assoc_key("cramer_v")
  }
  # The word printed OVER that column. The name above is frozen; this
  # follows the registry, and the two are the same string in English.
  measure_label <- if (length(unique_shown) == 1L) {
    .assoc_label(unique_shown)
  } else if (length(unique_shown) > 1L) {
    spicy_str("header_effect_size")
  } else {
    .assoc_label("cramer_v")
  }
  # The abbreviations of the headers, where the rule in force puts them:
  # the interval of the measure ("95% CI LL", "95% CI UL"), then the SMD.
  # The measures themselves are named in full ("Cramer's V", "Phi").
  abbrev_note <- .note_abbreviations(c(
    if (show_assoc && assoc_ci) as.list(.abbrev_ci_bounds()),
    if (do_smd) list("smd")
  ))
  # The definitions of the note, (d) of `.compose_note()`: which measure
  # each variable carries, then the direction of the SMD. The multivariate
  # sentence appears only when at least one variable has more than two
  # levels -- it explains an unsigned column, and there is nothing to
  # explain when every column is signed.
  definitions_note <- .assoc_note_apa(
    assoc_measures_per_row,
    labels,
    no_value = no_value_vars
  )
  if (do_smd) {
    definitions_note <- c(
      definitions_note,
      build_smd_note(TRUE, real_group_levels, decimal_mark)
    )
    if (
      any(vapply(
        select_names,
        function(v) {
          nlevels(.tab_factor(resolve_user_na(data[[v]]))) > 2L
        },
        logical(1)
      ))
    ) {
      definitions_note <- c(
        definitions_note,
        spicy_fmt("note_gloss_smd_multinomial", spicy_str("header_smd"))
      )
    }
  }

  if (length(rows) == 0) {
    long_raw <- data.frame(
      variable = character(0),
      level = character(0),
      group = character(0),
      n = numeric(0),
      pct = numeric(0),
      test_type = character(0),
      p = numeric(0),
      p_op = character(0),
      .assoc = numeric(0),
      .assoc_type = character(0),
      ci_lower = numeric(0),
      ci_upper = numeric(0),
      .smd = numeric(0),
      .smd_type = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    long_raw <- do.call(rbind, rows)
  }
  if (show_assoc) {
    names(long_raw)[names(long_raw) == ".assoc"] <- measure_col
  } else {
    long_raw$.assoc <- NULL
    long_raw$.assoc_type <- NULL
  }
  if (!do_smd) {
    # This family DROPS an unrequested optional column rather than
    # carrying it as NA -- the rule it already applies to `.assoc`, and
    # the opposite of the continuous frame's. Each frame keeps the
    # precedent of its own family.
    long_raw$.smd <- NULL
    long_raw$.smd_type <- NULL
  }

  if (nrow(long_raw) > 0) {
    long_raw$variable <- factor(long_raw$variable, levels = select_names)
    # Per-variable rank (see the no-by branch): each block keeps its own
    # level order (the one `levels_keep` gives it, when it names its
    # levels), "(Missing)" stays last per variable.
    long_raw$group <- factor(long_raw$group, levels = group_levels)
    long_raw <- long_raw[
      order(
        long_raw$variable,
        long_raw$.lvrank,
        long_raw$group,
        method = "radix"
      ),
      ,
      drop = FALSE
    ]
    long_raw$.lvrank <- NULL
    long_raw$variable <- as.character(long_raw$variable)
    long_raw$level <- as.character(long_raw$level)
    long_raw$group <- as.character(long_raw$group)
    rownames(long_raw) <- NULL
  }

  # `p_adjust`: one test per variable, every row of a variable carrying
  # it. Adjusted here, before the wide, display and typed frames read
  # `p`; the sentence joins the statistics note, which every route
  # prints. The raw p-values are keyed by the variable NAME, the key of
  # the rows and of the typed view's `.variable`.
  # The inference of the note, (c) of `.compose_note()`: the test behind
  # each p-value (decision of 2026-10-18), one sentence naming the test of
  # every variable, then Yates' correction where it reached one, then the
  # adjustment.
  inference_note <- NULL
  if (p_value) {
    phrase <- .cat_test_phrase(
      test_types,
      labels[match(names(test_types), select_names)],
      simulated = simulate_p,
      B = simulate_B
    )
    inference_note <- c(
      if (!is.null(phrase)) spicy_fmt("note_group_comparison", phrase$text),
      if (any(yates_used)) spicy_str("note_yates_applied")
    )
  }
  adj <- .desc_apply_p_adjust(long_raw, "p", long_raw$variable, p_adjust)
  long_raw <- adj$df
  p_adjust_info <- adj$info
  inference_note <- c(inference_note, .desc_p_adjust_note(p_adjust_info))
  # The statistics part of the note, (c) then (d): the `assoc_note`
  # attribute print() rebuilds the note from, NULL when empty.
  stat_sentences <- c(inference_note, definitions_note)
  stat_sentences <- stat_sentences[nzchar(stat_sentences)]
  assoc_note_text <- if (length(stat_sentences)) {
    paste(stat_sentences, collapse = " ")
  }
  # `p_value = FALSE`: the test leaves the compute frame here, before any
  # route reads it. The long output, the wide frame, the display frames
  # and the typed view are all built from this frame.
  long_raw <- .desc_drop_test_block(
    long_raw,
    p_value,
    p_col = .CAT_KEY_P,
    stat_cols = c("test_type", "chi2", "df", "p_op")
  )

  if (output == "long") {
    # The machine-readable frame, in the vocabulary of the family: the
    # measure column has a STABLE name whatever measure it holds, and the
    # measure each row carries travels beside it as a key.
    out <- .categorical_publish_long(
      with_keys(with_label(long_raw)),
      measure_col = if (show_assoc) measure_col,
      assoc_ci = show_assoc && assoc_ci
    )
    # Every data frame carries the note as print() shows it.
    attr(out, "note") <- .categorical_note(
      build_weights_note(),
      build_missing_note(),
      assoc_note_text,
      abbrev_note
    )
    return(out)
  }
  # Only the long output publishes the per-row measure and SMD keys; the
  # display and export routes below read the measure column by name.
  # `glance()` reads them too, from the `long_data` the default object
  # carries, so they are kept aside for that copy.
  assoc_type_rows <- long_raw$.assoc_type
  smd_type_rows <- long_raw$.smd_type
  long_raw$.assoc_type <- NULL
  long_raw$.smd_type <- NULL

  # ---------------- WIDE RAW ----------------
  make_wide_raw <- function(ldf) {
    cols <- c(
      .CAT_KEY_VARIABLE,
      "Level",
      as.vector(rbind(.cat_key_n(group_levels), .cat_key_pct(group_levels))),
      if (p_value) c("test_type", "Chi2", "df", .CAT_KEY_P),
      if (!is.null(p_adjust_info)) "p_unadjusted"
    )
    if (show_assoc) {
      cols <- c(cols, measure_col)
    }
    if (show_assoc && assoc_ci) {
      cols <- c(cols, .CAT_KEY_CI_LL, .CAT_KEY_CI_UL)
    }
    if (do_smd) {
      # This frame carries DISPLAY names ("Cramer's V", never
      # `effect_size`), so the SMD enters it under its frozen key.
      cols <- c(cols, .CAT_KEY_SMD)
    }
    if (nrow(ldf) == 0) {
      return(as.data.frame(
        setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
        check.names = FALSE
      ))
    }

    key <- unique(ldf[, c("variable", "level"), drop = FALSE])
    out <- vector("list", nrow(key))

    for (k in seq_len(nrow(key))) {
      sv <- ldf[
        ldf$variable == key$variable[k] & ldf$level == key$level[k],
        ,
        drop = FALSE
      ]
      r <- as.list(setNames(rep(NA, length(cols)), cols))
      r$Variable <- label_of_key(key$variable[k])
      r$Level <- key$level[k]

      for (gr in group_levels) {
        s <- sv[sv$group == gr, , drop = FALSE]
        r[[.cat_key_n(gr)]] <- if (nrow(s)) s$n[1] else NA_real_
        r[[.cat_key_pct(gr)]] <- if (nrow(s)) s$pct[1] else NA_real_
      }

      if (p_value) {
        r$test_type <- if (nrow(sv)) sv$test_type[1] else NA_character_
        r$Chi2 <- if (nrow(sv)) sv$chi2[1] else NA_real_
        r$df <- if (nrow(sv)) sv$df[1] else NA_real_
        r$p <- if (nrow(sv)) sv$p[1] else NA_real_
      }
      if (!is.null(p_adjust_info)) {
        r$p_unadjusted <- if (nrow(sv)) sv$p_unadjusted[1] else NA_real_
      }
      if (show_assoc) {
        r[[measure_col]] <- if (nrow(sv)) sv[[measure_col]][1] else NA_real_
      }
      if (show_assoc && assoc_ci) {
        r[[.CAT_KEY_CI_LL]] <- if (nrow(sv)) sv$ci_lower[1] else NA_real_
        r[[.CAT_KEY_CI_UL]] <- if (nrow(sv)) sv$ci_upper[1] else NA_real_
      }
      if (do_smd) {
        r[[.CAT_KEY_SMD]] <- if (nrow(sv)) sv$.smd[1] else NA_real_
      }

      out[[k]] <- as.data.frame(
        r,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    do.call(rbind, out)
  }

  wide_raw <- make_wide_raw(long_raw)
  # Read the ledger once, here: every output route below must carry the
  # same disclosure, not print() alone. `table_note` is the whole note
  # every route prints, assembled by the same `.categorical_note()` as
  # print() from the same four parts.
  what_note <- build_weights_note()
  missing_note <- build_missing_note()
  table_note <- .categorical_note(
    what_note,
    missing_note,
    assoc_note_text,
    abbrev_note
  )

  # ---------------- REPORT WIDE ----------------
  report_cols <- c(
    .CAT_KEY_VARIABLE,
    as.vector(rbind(.cat_key_n(group_levels), .cat_key_pct(group_levels))),
    if (p_value) .CAT_KEY_P
  )
  if (show_assoc) {
    report_cols <- c(report_cols, measure_col)
  }
  if (show_assoc && assoc_ci) {
    report_cols <- c(report_cols, .CAT_KEY_CI_LL, .CAT_KEY_CI_UL)
  }
  if (do_smd) {
    report_cols <- c(report_cols, .CAT_KEY_SMD)
  }
  # How many trailing columns are statistics rather than counts. Every
  # engine below indexes its last columns from the right (the p column,
  # the measure, the CI bounds, and now the SMD), so the arithmetic is
  # done ONCE here instead of being re-derived five times -- an Excel
  # `text_cols` or a tinytable `stat_j` that forgot the new column would
  # format the wrong cells, silently.
  n_stat_cols <- as.integer(p_value) +
    as.integer(show_assoc) +
    2L * as.integer(show_assoc && assoc_ci) +
    as.integer(do_smd)
  # The same count once the CI bounds have been merged into the measure
  # cell, which every rendered engine does before laying out.
  n_stat_cols_merged <- as.integer(p_value) +
    as.integer(show_assoc) +
    as.integer(do_smd)

  # Group headers, resolved once: user levels are data, the margin is
  # the single entry that carries a word of ours.
  group_labels <- vapply(
    group_levels,
    .cat_group_label,
    character(1),
    margin_key = margin_col_key,
    USE.NAMES = FALSE
  )
  make_report_wide <- function(ldf, mode = c("char", "excel")) {
    mode <- match.arg(mode)

    if (nrow(ldf) == 0) {
      if (mode == "char") {
        return(as.data.frame(
          setNames(
            replicate(length(report_cols), character(0), simplify = FALSE),
            report_cols
          ),
          check.names = FALSE
        ))
      } else {
        out <- as.data.frame(
          setNames(
            replicate(length(report_cols), numeric(0), simplify = FALSE),
            report_cols
          ),
          check.names = FALSE
        )
        out$Variable <- character(0)
        if (p_value) {
          out$p <- character(0)
        }
        return(out[, report_cols, drop = FALSE])
      }
    }

    out <- list()
    z <- 1L
    # The count and percentage cells of one row, `sl` its rows of the
    # compute frame: a count with no number is a group in which the
    # variable has no data, shown as `cell_undefined`.
    fill_groups <- function(r, sl) {
      for (gr in group_levels) {
        sx <- sl[sl$group == gr, , drop = FALSE]
        n_val <- if (nrow(sx)) sx$n[1] else NA_real_
        p_val <- if (nrow(sx)) sx$pct[1] else NA_real_
        if (mode == "char") {
          r[[.cat_key_n(gr)]] <- fmt_n(n_val, na = undefined_cell)
          r[[.cat_key_pct(gr)]] <- fmt_num(
            p_val,
            percent_digits,
            na = undefined_cell
          )
        } else {
          r[[.cat_key_n(gr)]] <- n_val
          r[[.cat_key_pct(gr)]] <- p_val
        }
      }
      r
    }

    for (k in seq_along(labels)) {
      lab <- labels[[k]]
      sv <- ldf[ldf$variable == select_names[[k]], , drop = FALSE]
      if (nrow(sv) == 0) {
        next
      }
      # A variable with no data and no level to show: its one row
      # carries the cells of the groups itself.
      no_level <- all(is.na(sv$level))

      keep_k <- levels_keep[[select_names[[k]]]]
      lv_use <- if (no_level) {
        character(0)
      } else if (is.null(keep_k)) {
        unique(sv$level)
      } else {
        intersect(keep_k, unique(sv$level))
      }

      # variable row
      if (mode == "char") {
        r0 <- as.list(setNames(rep("", length(report_cols)), report_cols))
      } else {
        r0 <- as.list(setNames(rep(NA, length(report_cols)), report_cols))
      }
      if (no_level) {
        r0 <- fill_groups(r0, sv)
      }
      r0$Variable <- lab
      if (p_value) {
        r0$p <- fmt_p(sv$p[1], sv$p_op[1])
      }
      if (show_assoc) {
        r0[[measure_col]] <- fmt_v(sv[[measure_col]][1])
      }
      if (do_smd) {
        # The variable row carries it, like `p` and the association
        # measure: it is a statistic of the variable, not of a level.
        r0[[.CAT_KEY_SMD]] <- fmt_smd(sv$.smd[1])
      }
      if (show_assoc && assoc_ci) {
        if (mode == "char") {
          r0[[.CAT_KEY_CI_LL]] <- fmt_v(sv$ci_lower[1])
          r0[[.CAT_KEY_CI_UL]] <- fmt_v(sv$ci_upper[1])
        } else {
          r0[[.CAT_KEY_CI_LL]] <- sv$ci_lower[1]
          r0[[.CAT_KEY_CI_UL]] <- sv$ci_upper[1]
        }
      }
      out[[z]] <- as.data.frame(
        r0,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      z <- z + 1L

      # modality rows
      for (lv in lv_use) {
        sl <- sv[sv$level == lv, , drop = FALSE]
        if (mode == "char") {
          r1 <- as.list(setNames(rep("", length(report_cols)), report_cols))
        } else {
          r1 <- as.list(setNames(rep(NA, length(report_cols)), report_cols))
        }
        r1$Variable <- paste0(indent_text, lv)
        r1 <- fill_groups(r1, sl)

        if (p_value) {
          r1$p <- ""
        }
        if (show_assoc) {
          r1[[measure_col]] <- ""
        }
        if (do_smd) {
          # Written explicitly rather than inherited from the
          # `rep("", ...)` initialization above: a level row carries no
          # SMD by CONTRACT, not by accident of construction.
          r1[[.CAT_KEY_SMD]] <- ""
        }
        out[[z]] <- as.data.frame(
          r1,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        z <- z + 1L
      }
    }

    do.call(rbind, out)
  }

  report_wide_char <- make_report_wide(long_raw, mode = "char")
  report_wide_excel <- make_report_wide(long_raw, mode = "excel")

  # Typed view of the SAME rows the display frames hold (see the
  # one-way branch above). The group columns carry their spanner; the
  # margin is flagged rather than matched by label. Every output route
  # below reads its block geometry from the `.row_role` column.
  structured <- .build_categorical_structured(
    long_raw = long_raw,
    select_names = select_names,
    labels = labels,
    levels_keep = levels_keep,
    missing_label = missing_label,
    missing_vars = missing_vars,
    indent_text = indent_text,
    percent_digits = percent_digits,
    p_digits = p_digits,
    effect_size_digits = effect_size_digits,
    decimal_mark = decimal_mark,
    group_levels = group_levels,
    margin_key = margin_col_key,
    measure_col = measure_col,
    measure_label = measure_label,
    show_p = p_value,
    show_assoc = show_assoc,
    assoc_ci = assoc_ci,
    show_smd = do_smd
  )
  structured <- .struct_attach_p_adjust(structured, p_adjust_info)
  if (p_value) {
    structured <- .struct_attach_test_type(structured, test_types)
  }

  if (output %in% c("default", "data.frame")) {
    out <- wide_raw
    attr(out, "display_df") <- report_wide_char
    # The title the console prints, from the helper every engine uses.
    attr(out, "title") <- .categorical_title(by_label)
    # The whole note, as print() shows it; `what_note`, `missing_note`,
    # `assoc_note` and `abbrev_note` stay the four parts print()
    # assembles it from.
    attr(out, "note") <- table_note
    attr(out, "what_note") <- what_note
    attr(out, "missing_note") <- missing_note
    attr(out, "group_var") <- by_name
    attr(out, "group_label") <- by_label
    attr(out, "indent_text") <- indent_text
    attr(out, "align") <- align
    attr(out, "decimal_mark") <- decimal_mark
    # The compute frame of `output = "long"`, under its internal names,
    # for tidy() and glance(), with the measure and SMD keys of each row
    # (`.assoc_type`, `.smd_type`), which glance() reports per variable.
    long_data <- with_keys(with_label(long_raw))
    if (show_assoc) {
      long_data$.assoc_type <- assoc_type_rows
    }
    if (do_smd) {
      long_data$.smd_type <- smd_type_rows
    }
    attr(out, "long_data") <- long_data
    attr(out, "assoc_note") <- assoc_note_text
    attr(out, "abbrev_note") <- abbrev_note
    attr(out, "structured") <- structured
    if (include_total) {
      # The internal key of the margin group in `long_data` ("Total", or
      # the first free "Total_<i>" when a `by` level holds that key beside
      # a relabeled margin; see `.cat_margin_key()`). Read by tidy() /
      # glance() to drop the margin without ever mistaking a user group
      # named "Total" for it.
      attr(out, "total_group") <- margin_key
    }
    # See `.style_stamp()`: the style levers with no argument have to
    # outlive this call, because `inline()` re-formats the typed body to
    # cite a cell.
    out <- .style_stamp(out)
    class(out) <- c("spicy_categorical_table", "spicy_table", "data.frame")
    # `output = "data.frame"` is `as.data.frame()` of the object the call
    # would print (see the one-way branch).
    if (output == "data.frame") {
      return(.categorical_blank_na(as.data.frame(out), blank_na_wide))
    }
    # Return VISIBLY and let standard auto-print dispatch to
    # print.spicy_categorical_table(): a bare table_categorical(...) call
    # still displays the table, while `t <- table_categorical(...)` is
    # silent (the freq() / cross_tab() model).
    return(out)
  }

  # For rendered formats: merge CI inline into measure column, drop CI cols
  merge_ci_inline <- function(df) {
    if (!show_assoc || !assoc_ci || !(.CAT_KEY_CI_LL %in% names(df))) {
      return(df)
    }
    has_val <- nzchar(df[[measure_col]]) & nzchar(df[[.CAT_KEY_CI_LL]])
    # Same disambiguation rule as every other family: under a comma
    # decimal mark the bound separator becomes "; " (this site used to
    # hardcode ", ", printing `0,45 [0,31, 0,59]` -- the ambiguity
    # ci_bracket_separator() exists to avoid). A style may still
    # replace it.
    ci_sep <- .style_ci_sep(ci_bracket_separator(decimal_mark))
    br <- .style_ci_brackets()
    df[[measure_col]][has_val] <- paste0(
      df[[measure_col]][has_val],
      " ",
      br[[1L]],
      df[[.CAT_KEY_CI_LL]][has_val],
      ci_sep,
      df[[.CAT_KEY_CI_UL]][has_val],
      br[[2L]]
    )
    df[[.CAT_KEY_CI_LL]] <- NULL
    df[[.CAT_KEY_CI_UL]] <- NULL
    df
  }
  # The header over that merged cell states the level of its interval
  # ("Cramer's V [95% CI]"), as the console's bound headers do ("95% CI
  # LL"): the interval label is theirs, read from the typed view. The
  # console, the sheet and the clipboard keep the bounds in columns of
  # their own and the measure's bare header.
  measure_label_merged <- measure_label
  if (show_assoc && assoc_ci) {
    assoc_ci_label <- structured$col_meta[[.CAT_KEY_CI_LL]]$ci_label
    if (!is.null(assoc_ci_label)) {
      measure_label_merged <- spicy_fmt(
        "header_estimate_ci",
        measure_label,
        assoc_ci_label
      )
    }
  }

  # Headers (base: without CI; used by rendered formats). Pure DISPLAY:
  # the column keys travel separately (flextable takes
  # `col_keys = names(df)`, the sheet and the clipboard write these rows
  # verbatim), so these three vectors carry labels only.
  top_header_span <- c(
    spicy_str("header_variable"),
    rep(group_labels, each = 2),
    if (p_value) spicy_str("header_p")
  )
  top_header_flat <- c(
    spicy_str("header_variable"),
    as.vector(rbind(group_labels, rep("", length(group_labels)))),
    if (p_value) spicy_str("header_p")
  )
  bot_header <- c(
    "",
    rep(
      c(spicy_str("header_n_lower"), spicy_str("header_percent_symbol")),
      times = length(group_levels)
    ),
    if (p_value) ""
  )
  if (show_assoc) {
    # `top_header_span` heads the merged cell of flextable and Word;
    # `top_header_flat` the bound columns of the sheet and the clipboard.
    top_header_span <- c(top_header_span, measure_label_merged)
    top_header_flat <- c(top_header_flat, measure_label)
    bot_header <- c(bot_header, "")
  }
  smd_label <- spicy_str("header_smd")
  if (do_smd) {
    top_header_span <- c(top_header_span, smd_label)
    top_header_flat <- c(top_header_flat, smd_label)
    bot_header <- c(bot_header, "")
  }
  grp_j <- 2:(1 + 2 * length(group_levels))

  # ---------------- tinytable ----------------
  if (output == "tinytable") {
    if (!requireNamespace("tinytable", quietly = TRUE)) {
      spicy_abort("Install package 'tinytable'.", class = "spicy_missing_pkg")
    }

    old_tt_opt <- getOption("tinytable_print_output")
    options(tinytable_print_output = "html") # RStudio Viewer
    on.exit(options(tinytable_print_output = old_tt_opt), add = TRUE)

    dat_tt <- merge_ci_inline(report_wide_char)
    # Pre-pad numeric cells so centering stacks the decimal points
    # vertically (same strategy as table_regression() /
    # table_continuous_lm()). The native tinytable
    # `style_tt(align = "d")` centers each cell on its own value
    # rather than on the decimal mark, which is inconsistent with
    # the rendering used by the other engines.
    dat_tt <- pad_decimal_cols(dat_tt)

    mod_rows <- .categorical_level_rows_typed(structured)
    # Rule above the first row of each variable block (the console
    # draws the same dashed separator between blocks).
    var_sep_rows <- .categorical_sep_rows_typed(structured)
    if (length(mod_rows)) {
      dat_tt[[1]][mod_rows] <- paste0(
        strrep("\u00A0", 4),
        substring(dat_tt[[1]][mod_rows], nchar(indent_text) + 1L)
      )
    }

    colnames(dat_tt) <- c(
      "",
      rep(
        c(spicy_str("header_n_lower"), spicy_str("header_percent_symbol")),
        times = length(group_levels)
      ),
      rep("", n_stat_cols_merged)
    )

    # Spanners. A `group_tt()` spec is indexed BY its label, so two equal
    # labels would silently merge into one entry and drop a column from
    # the header -- already reachable with two homonymous `by` levels.
    gspec <- c(
      setNames(list(1), spicy_str("header_variable")),
      setNames(
        lapply(seq_along(group_levels), function(i) c(2 * i, 2 * i + 1)),
        group_labels
      ),
      if (p_value) {
        setNames(
          list(ncol(dat_tt) - n_stat_cols_merged + 1L),
          spicy_str("header_p")
        )
      }
    )
    if (show_assoc) {
      gspec[[measure_label_merged]] <- ncol(dat_tt) - as.integer(do_smd)
    }
    if (do_smd) {
      gspec[[smd_label]] <- ncol(dat_tt)
    }

    tt <- tinytable::tt(
      dat_tt,
      caption = .categorical_title(by_label),
      # The association-measure gloss belongs to the rendered table as
      # much as to the console: it names which measure each row
      # carries.
      notes = table_note
    )
    tt <- tinytable::group_tt(tt, j = gspec)
    tt <- .spicy_tt_bare(tt)
    # User data reaches the cells (level labels, variable labels):
    # escape it, like every other engine of the family does.
    tt <- .spicy_tt_escape(tt)

    # Alignment. Honor the `align` argument: "decimal" centers
    # uniform-width pre-padded strings (same strategy as
    # table_regression() / table_continuous_lm()); "center" / "right"
    # apply literal alignment.
    tt <- tinytable::style_tt(tt, j = 1, align = "l")
    data_j <- 2:(1 + 2 * length(group_levels))
    # `seq_len()`, not `a:b`: with `p_value = FALSE, assoc_measure =
    # "none"` no statistic column is left, and `(n + 1):n` counts down
    # to a column past the table, which tinytable refuses.
    stat_j <- ncol(dat_tt) - n_stat_cols_merged + seq_len(n_stat_cols_merged)
    tt_align <- switch(
      align,
      decimal = "c",
      center = "c",
      right = "r",
      "r"
    )
    tt <- tinytable::style_tt(tt, j = c(data_j, stat_j), align = tt_align)
    # Center n/% labels (row 0 = column labels row)
    tt <- tinytable::style_tt(tt, i = 0, j = data_j, align = "c")
    # Center spanner labels (row -1 = spanner row)
    tt <- tinytable::style_tt(tt, i = -1, j = 2:ncol(dat_tt), align = "c")
    if (length(mod_rows)) {
      tt <- tinytable::style_tt(tt, i = mod_rows, j = 1, indent = 1)
      tt <- tinytable::style_tt(
        tt,
        i = mod_rows,
        j = 1,
        html_css = "padding-left: 0.8em;"
      )
    }

    # Lines
    grp_j <- 2:(1 + 2 * length(group_levels))

    # Top of table
    tt <- tinytable::style_tt(
      tt,
      i = -1,
      j = seq_len(ncol(dat_tt)),
      line = "t",
      line_width = 0.06
    )
    # Intermediate line under spanner: group columns only
    tt <- tinytable::style_tt(
      tt,
      i = -1,
      j = grp_j,
      line = "b",
      line_width = 0.06
    )
    # Line under n/% header: full width
    tt <- tinytable::style_tt(
      tt,
      i = 0,
      j = seq_len(ncol(dat_tt)),
      line = "b",
      line_width = 0.06
    )
    # Bottom closing line
    tt <- tinytable::style_tt(
      tt,
      i = nrow(dat_tt),
      j = seq_len(ncol(dat_tt)),
      line = "b",
      line_width = 0.06
    )
    # Light separators between variable blocks (same rule
    # table_continuous() draws).
    for (sr in var_sep_rows) {
      tt <- tinytable::style_tt(
        tt,
        i = sr - 1L,
        j = seq_len(ncol(dat_tt)),
        line = "b",
        line_width = 0.03
      )
    }
    # Prevent p-value and measure columns from wrapping
    if (length(stat_j)) {
      tt <- tinytable::style_tt(
        tt,
        j = stat_j,
        html_css = "white-space: nowrap;"
      )
    }

    return(tt)
  }

  # ---------------- gt ----------------
  if (output == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      spicy_abort("Install package 'gt'.", class = "spicy_missing_pkg")
    }

    dat_gt <- merge_ci_inline(report_wide_char)
    # Pre-pad numeric cells so centering stacks the decimal points
    # vertically (same strategy as table_regression() /
    # table_continuous_lm()). gt's native `cols_align_decimal()`
    # renders visually right-aligned, which is inconsistent with
    # the rendering used by the other engines.
    dat_gt <- pad_decimal_cols(dat_gt)

    # Same light rule the console and the other engines draw between
    # variable blocks, from the same typed geometry.
    var_sep_rows <- .categorical_sep_rows_typed(structured)
    # Indent modality rows with non-breaking spaces
    mod_rows <- .categorical_level_rows_typed(structured)
    if (length(mod_rows)) {
      dat_gt[[1]][mod_rows] <- paste0(
        strrep("\u00A0", 4),
        substring(dat_gt[[1]][mod_rows], nchar(indent_text) + 1L)
      )
    }

    # Rename n/% columns to unique names for gt, then relabel
    col_ids <- character(ncol(dat_gt))
    col_ids[1] <- .CAT_KEY_VARIABLE
    for (gi in seq_along(group_levels)) {
      col_ids[2 * gi] <- paste0(group_levels[gi], "_n")
      col_ids[2 * gi + 1] <- paste0(group_levels[gi], "_pct")
    }
    if (p_value) {
      col_ids[ncol(dat_gt) - n_stat_cols_merged + 1L] <- .CAT_KEY_P
    }
    if (show_assoc) {
      col_ids[ncol(dat_gt) - as.integer(do_smd)] <- "assoc_col"
    }
    if (do_smd) {
      col_ids[ncol(dat_gt)] <- "smd_col"
    }
    # The group ids above carry `by` levels, i.e. user data, and gt
    # writes a column id RAW into the `headers="..."` attribute of every
    # body cell (and it lands in the `th[id="%s"]` selector below).
    # `.gt_safe_ids()` is the identity on an ordinary level; the two
    # derived vectors are read wherever a group column is addressed, so
    # the ids can never be typed out a second time and drift.
    col_ids <- unname(.gt_safe_ids(col_ids))
    grp_id_n <- col_ids[2L * seq_along(group_levels)]
    grp_id_pct <- col_ids[2L * seq_along(group_levels) + 1L]
    names(dat_gt) <- col_ids

    tbl <- gt::gt(dat_gt)

    # Column labels: n / % under each group; empty for single-col spanners
    label_list <- list()
    label_list[[.CAT_KEY_VARIABLE]] <- ""
    for (gi in seq_along(group_levels)) {
      label_list[[grp_id_n[gi]]] <- spicy_str("header_n_lower")
      label_list[[grp_id_pct[gi]]] <- spicy_str("header_percent_symbol")
    }
    if (p_value) {
      label_list[[.CAT_KEY_P]] <- ""
    }
    if (show_assoc) {
      label_list[["assoc_col"]] <- ""
    }
    if (do_smd) {
      label_list[["smd_col"]] <- ""
    }
    tbl <- gt::cols_label(tbl, .list = label_list)

    # Spanners: group names over n/% pairs, single-col for Variable/p/V.
    # `columns =` addresses the machine ids built above; only `label =`
    # takes a header.
    tbl <- gt::tab_spanner(
      tbl,
      label = spicy_str("header_variable"),
      columns = .CAT_KEY_VARIABLE,
      id = "spn_variable"
    )
    for (gi in seq_along(group_levels)) {
      tbl <- gt::tab_spanner(
        tbl,
        label = group_labels[gi],
        columns = c(grp_id_n[gi], grp_id_pct[gi]),
        # gt >= 1.4.0 writes an `id` on the spanner `<th>`, derived from
        # the label when none is given, after collapsing whitespace to
        # "-": two `by` levels "A B" and "A-B" then share one DOM id. The
        # machine id of the n column is already unique on that form
        # (`.gt_safe_ids()`), so the spanner borrows it.
        id = paste0("spn_", grp_id_n[gi])
      )
    }
    if (p_value) {
      tbl <- gt::tab_spanner(
        tbl,
        label = spicy_str("header_p"),
        columns = .CAT_KEY_P,
        id = "spn_p"
      )
    }
    if (show_assoc) {
      tbl <- gt::tab_spanner(
        tbl,
        label = measure_label_merged,
        columns = "assoc_col",
        id = "spn_v"
      )
    }
    if (do_smd) {
      tbl <- gt::tab_spanner(
        tbl,
        label = smd_label,
        columns = "smd_col",
        id = "spn_smd"
      )
    }

    # Alignment. The Variable column is always left-aligned; numeric
    # columns honor the `align` argument: "decimal" centers
    # uniform-width pre-padded strings (same strategy as
    # table_regression() / table_continuous_lm()); "center" / "right"
    # use gt::cols_align() literally.
    tbl <- gt::cols_align(tbl, align = "left", columns = .CAT_KEY_VARIABLE)
    grp_cols <- as.vector(rbind(grp_id_n, grp_id_pct))
    right_cols <- if (p_value) .CAT_KEY_P else character(0)
    if (show_assoc) {
      right_cols <- c(right_cols, "assoc_col")
    }
    if (do_smd) {
      right_cols <- c(right_cols, "smd_col")
    }
    numeric_cols <- c(grp_cols, right_cols)
    if (identical(align, "decimal") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "center", columns = numeric_cols)
    } else if (identical(align, "center") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "center", columns = numeric_cols)
    } else if (identical(align, "right") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "right", columns = numeric_cols)
    }
    # Left-align the Variable spanner label
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(align = "left"),
      locations = gt::cells_column_spanners(spanners = "spn_variable")
    )

    # APA-style borders ------------------------------------------------
    # gt emits "border-bottom-style: hidden" on the spanner <tr>,
    # which wins in border-collapse:collapse and blocks tab_style().
    # We use opt_css(!important) for full control, plus tab_style()
    # so inline-CSS renderers (as_raw_html) also get the rules.
    rule <- gt::cell_borders(
      sides = "bottom",
      color = "currentColor",
      weight = gt::px(1)
    )
    rule_top <- gt::cell_borders(
      sides = "top",
      color = "currentColor",
      weight = gt::px(1)
    )

    # 1) Silence every default border.  Setting width to 0 is
    #    critical: gt defaults to 2px, and in border-collapse the
    #    wider border wins regardless of color.
    tbl <- gt::tab_options(
      tbl,
      table.border.top.width = gt::px(0),
      table.border.bottom.width = gt::px(0),
      table_body.border.top.width = gt::px(0),
      table_body.border.bottom.width = gt::px(0),
      table_body.hlines.color = "transparent",
      column_labels.border.top.width = gt::px(0),
      column_labels.border.bottom.width = gt::px(0),
      column_labels.border.lr.color = "transparent"
    )

    # 2) tab_style rules (work in inline-CSS renderers)
    # Rule 1: top of spanners (full width)
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_spanners()
    )
    # Rule 2: intermediate line below spanners (group columns only)
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_labels(columns = grp_cols)
    )
    # Rule 3: below column labels (full width)
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_column_labels()
    )
    # Rule 4: bottom of last body row
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_body(rows = nrow(dat_gt))
    )
    # Light separators between variable blocks (console / tinytable /
    # flextable parity; same style as table_continuous()'s gt branch).
    light_rule <- gt::cell_borders(
      sides = "bottom",
      color = "#cccccc",
      weight = gt::px(0.5)
    )
    for (sr in var_sep_rows) {
      tbl <- gt::tab_style(
        tbl,
        style = light_rule,
        locations = gt::cells_body(rows = sr - 1L)
      )
    }

    # 3) opt_css rules (override gt's hidden borders in normal
    #    renderers: RStudio viewer, Quarto, pkgdown)
    # Build CSS selector for group-column <th> elements. These ids carry
    # the `by` level labels, i.e. user data. `.gt_safe_ids()` above has
    # already taken the quote and the backslash out of them; the
    # `.css_escape_string()` here is the second line of that defense,
    # kept because this selector is one edit away from an id that never
    # passed through the sanitizer.
    grp_css_sel <- paste(
      vapply(
        grp_cols,
        function(id) {
          sprintf(
            '.gt_table thead tr:last-child th[id="%s"]',
            .css_escape_string(id)
          )
        },
        character(1)
      ),
      collapse = ",\n"
    )
    apa_css <- paste(
      ".gt_table thead tr:first-child {",
      "  border-top: 1px solid currentColor !important;",
      "}",
      ".gt_table thead tr.gt_spanner_row {",
      "  border-bottom-style: none !important;",
      "}",
      ".gt_table thead th, .gt_table thead td {",
      "  background-color: transparent !important;",
      "}",
      # Intermediate line: only group columns
      paste0(grp_css_sel, " {"),
      "  border-top: 1px solid currentColor !important;",
      "}",
      ".gt_table thead tr:last-child {",
      "  border-bottom: 1px solid currentColor !important;",
      "}",
      ".gt_table tbody tr:last-child {",
      "  border-bottom: 1px solid currentColor !important;",
      "}",
      ".gt_table tbody tr {",
      "  border-top-style: none !important;",
      "  border-bottom-style: none !important;",
      "}",
      sep = "\n"
    )
    tbl <- gt::opt_css(tbl, css = apa_css)

    # The same title the five other engines print.
    tbl <- .spicy_gt_apa_title(tbl, .categorical_title(by_label))

    # The note, as the five other routes print it.
    return(.spicy_gt_attach_note(tbl, table_note))
  }

  # ---------------- flextable / word ----------------
  build_flextable <- function(df) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      spicy_abort(
        "Install package 'flextable'.",
        class = "spicy_missing_pkg",
        call = rlang::caller_env()
      )
    }
    df <- pad_decimal_cols(df)
    # Level rows carry the console's indent inside the label cell and
    # the engine indents them again below (`padding.left`). One
    # indentation is the design; two is an artifact of reading a
    # display string as data -- and Word keeps the literal spaces.
    # Keep the engine's indent, which survives every backend, and hand
    # the cell the bare level name (same rule as the gt and tinytable
    # branches).
    id_mod <- .categorical_level_rows_typed(structured)
    # Same light rule the console and tinytable draw between variable
    # blocks, from the same typed geometry.
    var_sep_rows <- .categorical_sep_rows_typed(structured)
    if (length(id_mod)) {
      df[[1L]][id_mod] <- substring(df[[1L]][id_mod], nchar(indent_text) + 1L)
    }
    ft <- flextable::flextable(df)

    map <- data.frame(
      col_keys = names(df),
      top = top_header_span,
      bottom = bot_header,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    ft <- flextable::set_header_df(ft, mapping = map, key = "col_keys")
    ft <- flextable::merge_h(ft, part = "header")

    bd <- spicy_fp_border(color = "black", width = 1)

    ft <- flextable::align(ft, j = 1, part = "all", align = "left")
    # Numeric column alignment honors `align`. For "decimal", cells
    # were pre-padded above by `pad_decimal_cols()`; CENTER the
    # padded strings in the default body font (single-font policy
    # matching table_regression()). For "center" / "right", apply
    # the literal alignment.
    num_j <- 2:ncol(df)
    if (identical(align, "decimal") && length(num_j) > 0L) {
      ft <- flextable::align(
        ft,
        j = num_j,
        part = "body",
        align = "center"
      )
    } else if (identical(align, "center") && length(num_j) > 0L) {
      ft <- flextable::align(
        ft,
        j = num_j,
        part = "body",
        align = "center"
      )
    } else {
      ft <- flextable::align(
        ft,
        j = num_j,
        part = "body",
        align = "right"
      )
    }
    # Center n/% labels and spanner labels in header
    ft <- flextable::align(ft, j = grp_j, part = "header", align = "center")
    # Right-align p and association measure in header (`seq_len()`: the
    # range is empty when no statistic column is left, see tinytable).
    stat_j <- ncol(df) - n_stat_cols_merged + seq_len(n_stat_cols_merged)
    if (length(stat_j)) {
      ft <- flextable::align(ft, j = stat_j, part = "header", align = "right")
    }

    ft <- flextable::hline_top(ft, part = "header", border = bd)
    ft <- flextable::hline(ft, i = 1, j = grp_j, part = "header", border = bd)
    ft <- flextable::hline_bottom(ft, part = "header", border = bd)
    ft <- flextable::hline_bottom(ft, part = "body", border = bd)
    # Light separators between variable blocks (console / tinytable /
    # table_continuous() flextable parity).
    bd_light <- spicy_fp_border(color = "#cccccc", width = 0.5)
    for (sr in var_sep_rows) {
      ft <- flextable::hline(ft, i = sr - 1L, part = "body", border = bd_light)
    }

    if (length(id_mod)) {
      ft <- flextable::padding(
        ft,
        i = id_mod,
        j = 1,
        part = "body",
        padding.left = 14
      )
    }

    ft <- flextable::autofit(ft)
    # The association-measure gloss belongs to the rendered table as
    # much as to the console: it names which measure each row carries.
    # Same note as the console printer.
    ft <- .spicy_ft_attach_note(ft, table_note)
    class(ft) <- c("spicy_flextable", class(ft))
    ft
  }

  if (output == "flextable") {
    # Same title the console prints, from the same helper -- for a
    # by-table it names the grouping variable, which nothing else in
    # the rendered table states.
    return(.spicy_ft_html_caption(
      build_flextable(merge_ci_inline(report_wide_char)),
      .categorical_title(by_label)
    ))
  }

  if (output == "word") {
    .check_output_path(word_path, output)
    ft <- build_flextable(merge_ci_inline(report_wide_char))
    ft <- .spicy_ft_word_caption(ft, .categorical_title(by_label))
    flextable::save_as_docx(ft, path = word_path)
    return(invisible(word_path))
  }

  # Extend headers with CI columns for data/export formats. The headers
  # are those of the typed view, which states the level of the interval
  # ("95% CI LL"), the console's.
  if (assoc_ci) {
    ci_headers <- c(
      structured$col_meta[[.CAT_KEY_CI_LL]]$display_label,
      structured$col_meta[[.CAT_KEY_CI_UL]]$display_label
    )
    # INSERTED before the SMD header, not appended after it: the CI
    # bounds sit between the measure and the SMD in `report_cols`, so
    # appending would print "SMD" over a CI column and a bound header
    # over the SMD -- headers and body silently out of step. With no
    # SMD column the split is at the end and this is the old append.
    cut_at <- length(top_header_flat) - as.integer(do_smd)
    top_header_flat_ex <- c(
      top_header_flat[seq_len(cut_at)],
      ci_headers,
      top_header_flat[-seq_len(cut_at)]
    )
    bot_header_ex <- c(
      bot_header[seq_len(cut_at)],
      "",
      "",
      bot_header[-seq_len(cut_at)]
    )
  } else {
    top_header_flat_ex <- top_header_flat
    bot_header_ex <- bot_header
  }

  # ---------------- clipboard matrix ----------------
  # Plain text, cell for cell: no decimal padding (the U+2007 pad
  # character of the fixed-width renderers is not whitespace to a
  # parser, so padded numbers paste as text beside unpadded numbers)
  # and no Excel text formulas (`="..."` shows up verbatim in the
  # two other documented paste targets). The p / association / CI
  # cells travel as the strings the console prints.
  clip_body <- report_wide_char
  clip_body$Variable <- make_stronger_indent(
    clip_body$Variable,
    indent_text,
    indent_text_excel_clipboard,
    .categorical_level_rows_typed(structured)
  )

  clip_mat <- rbind(top_header_flat_ex, bot_header_ex, as.matrix(clip_body))

  # ---------------- excel ----------------
  if (output == "excel") {
    .check_output_path(excel_path, output)
    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      spicy_abort("Install package 'openxlsx2'.", class = "spicy_missing_pkg")
    }

    wb <- openxlsx2::wb_workbook()
    wb <- openxlsx2::wb_add_worksheet(wb, excel_sheet)

    # Same title the console prints (it names the grouping variable,
    # which nothing else in the sheet states), then the two header
    # rows two lines below.
    wb <- .spicy_xl_add_data(
      wb,
      x = .categorical_title(by_label),
      start_row = 1
    )
    top_header_row <- 3L
    bot_header_row <- top_header_row + 1L
    first_body_row <- bot_header_row + 1L

    wb <- .spicy_xl_add_data(
      wb,
      x = as.data.frame(t(top_header_flat_ex), stringsAsFactors = FALSE),
      start_row = top_header_row,
      col_names = FALSE
    )
    wb <- .spicy_xl_add_data(
      wb,
      x = as.data.frame(t(bot_header_ex), stringsAsFactors = FALSE),
      start_row = bot_header_row,
      col_names = FALSE
    )

    body_xl <- report_wide_excel
    body_xl$Variable <- make_stronger_indent(
      body_xl$Variable,
      indent_text,
      indent_text_excel_clipboard,
      .categorical_level_rows_typed(structured)
    )
    if (p_value) {
      body_xl$p <- report_wide_char$p
    }
    if (show_assoc) {
      body_xl[[measure_col]] <- report_wide_char[[measure_col]]
    }
    if (show_assoc && assoc_ci) {
      body_xl[[.CAT_KEY_CI_LL]] <- report_wide_char[[.CAT_KEY_CI_LL]]
      body_xl[[.CAT_KEY_CI_UL]] <- report_wide_char[[.CAT_KEY_CI_UL]]
    }
    if (do_smd) {
      body_xl[[.CAT_KEY_SMD]] <- report_wide_char[[.CAT_KEY_SMD]]
    }

    # The empty cells of a variable-header row are not written at all
    # (see .spicy_xl_add_data()): written as "#N/A", they would sit in
    # the middle of the counts and any SUM over the column would
    # inherit the error.
    wb <- .spicy_xl_add_data(
      wb,
      x = body_xl,
      start_row = first_body_row,
      col_names = FALSE,
      row_names = FALSE
    )
    wb <- .cat_xl_mark_undefined(wb, report_wide_char, grp_j, first_body_row)

    nc <- ncol(body_xl)
    last_row <- bot_header_row + nrow(body_xl)
    pct_fmt <- paste0("0.", paste(rep("0", percent_digits), collapse = ""))

    if (add_multilevel_header) {
      for (i in seq_along(group_levels)) {
        c1 <- 2 + (i - 1) * 2
        wb <- openxlsx2::wb_merge_cells(
          wb,
          dims = openxlsx2::wb_dims(rows = top_header_row, cols = c1:(c1 + 1))
        )
      }
    }

    # Header alignment (center, vertically centered)
    wb <- openxlsx2::wb_add_cell_style(
      wb,
      dims = openxlsx2::wb_dims(
        rows = top_header_row:bot_header_row,
        cols = 1:nc
      ),
      horizontal = "center",
      vertical = "center"
    )
    if (nrow(body_xl) > 0) {
      # Body alignment. The Variable column is always left-aligned;
      # numeric columns honor `align`. For "decimal", Excel already
      # aligns decimal points implicitly via right-alignment combined
      # with a uniform numfmt, so the visual result matches the
      # dot-aligned column in print / gt / tinytable.
      num_horiz <- if (identical(align, "center")) "center" else "right"
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = first_body_row:last_row, cols = 1),
        horizontal = "left"
      )
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = first_body_row:last_row, cols = 2:nc),
        horizontal = num_horiz
      )
      # Text columns (p, assoc, CI, SMD) -- force text format. Counted
      # from the RIGHT, so every optional trailing column has to be in
      # the count or the sheet formats the wrong cells: `n_stat_cols`
      # is that count, computed once beside `report_cols`. Possibly
      # zero (`p_value = FALSE, assoc_measure = "none"`), hence
      # `seq_len()`: `(nc + 1):nc` would style an empty column past the
      # table.
      text_cols <- nc - n_stat_cols + seq_len(n_stat_cols)
      if (length(text_cols)) {
        wb <- openxlsx2::wb_add_numfmt(
          wb,
          dims = openxlsx2::wb_dims(
            rows = first_body_row:last_row,
            cols = text_cols
          ),
          numfmt = "@"
        )
      }
    }

    # APA borders. IMPORTANT: openxlsx2::wb_add_border() defaults
    # every side to "thin"; explicit NULLs on the unused sides
    # prevent vertical / spurious rules from being painted on
    # every styled cell.
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = top_header_row, cols = 1:nc),
      top_border = "thin",
      bottom_border = NULL,
      left_border = NULL,
      right_border = NULL
    )
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = top_header_row, cols = grp_j),
      bottom_border = "thin",
      top_border = NULL,
      left_border = NULL,
      right_border = NULL
    )
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = bot_header_row, cols = 1:nc),
      bottom_border = "thin",
      top_border = NULL,
      left_border = NULL,
      right_border = NULL
    )
    if (nrow(body_xl) > 0) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = last_row, cols = 1:nc),
        bottom_border = "thin",
        top_border = NULL,
        left_border = NULL,
        right_border = NULL
      )
    }

    # Number formats for n / % columns
    n_cols <- seq(2, 1 + 2 * length(group_levels), by = 2)
    p_cols <- n_cols + 1

    if (nrow(body_xl) > 0) {
      wb <- openxlsx2::wb_add_numfmt(
        wb,
        dims = openxlsx2::wb_dims(
          rows = first_body_row:last_row,
          cols = n_cols
        ),
        numfmt = "0"
      )
      wb <- openxlsx2::wb_add_numfmt(
        wb,
        dims = openxlsx2::wb_dims(
          rows = first_body_row:last_row,
          cols = p_cols
        ),
        numfmt = pct_fmt
      )
    }

    # The note below the table -- the same text, in the same order, as
    # the console footer.
    wb <- .spicy_xl_add_note(
      wb,
      note = table_note,
      start_row = last_row + 2L
    )
    # Widths from the DISPLAY strings (the char body), not from the
    # raw numerics the sheet stores.
    width_df <- report_wide_char
    width_df$Variable <- body_xl$Variable
    wb <- .spicy_xl_set_widths(
      wb,
      sheet = excel_sheet,
      cells = .spicy_xl_cells(
        width_df,
        headers = list(top_header_flat_ex, bot_header_ex)
      )
    )

    openxlsx2::wb_save(wb, excel_path, overwrite = TRUE)
    return(invisible(excel_path))
  }

  # ---------------- clipboard ----------------
  if (output == "clipboard") {
    .spicy_clip_preflight()
    # Same title (it names the grouping variable, which nothing else
    # in the payload states) and same disclosure notes -- what left
    # the table, then which association measure each row carries --
    # as the console, from the same helpers.
    txt <- .clipboard_payload_desc(
      clip_mat,
      clipboard_delim,
      title = .categorical_title(by_label),
      note = table_note
    )
    clipr::write_clip(txt)
    spicy_inform("Categorical table copied to clipboard.")
    return(invisible(txt))
  }
}
