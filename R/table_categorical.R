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
# of ours -- but only while it keeps its default key. Renamed on collision
# ("Total_1") it must show the disambiguated key, or a reader could no
# longer tell the margin from the user's homonymous group.
#
# One resolver for every engine: split it and the margin would translate
# in some outputs and not in others.
.cat_group_label <- function(g, margin_key = NULL) {
  if (
    !is.null(margin_key) &&
      identical(g, margin_key) &&
      identical(margin_key, .CAT_MARGIN_KEY)
  ) {
    spicy_str("header_margin_total")
  } else {
    g
  }
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
# * anything else becomes a factor in order of first appearance.
.tab_factor <- function(x) {
  if (is.factor(x)) {
    return(x)
  }
  if (labelled::is.labelled(x)) {
    return(labelled::to_factor(x, levels = "prefixed", nolabel_to_na = FALSE))
  }
  factor(x, levels = unique(x[!is.na(x)]))
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
          "`assoc_measure` value(s) not recognised: %s.",
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
          "`assoc_measure` value(s) not recognised: %s.",
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

# Build the APA-style "Note." line listing which measure was used for
# which variable when the rows of a `table_categorical()` table use
# more than one association measure.
#
# Example output:
#   "Note. Cramer's V: smoking, education; Kendall's Tau-b: self_rated_health."
.assoc_note_apa <- function(per_row_measures, labels) {
  shown <- per_row_measures[per_row_measures != "none"]
  if (length(shown) == 0L) {
    return(NULL)
  }
  unique_measures <- unique(unname(shown))
  if (length(unique_measures) <= 1L) {
    return(NULL)
  }
  parts <- vapply(
    unique_measures,
    function(m) {
      vars <- names(shown)[shown == m]
      lab <- labels[match(vars, names(per_row_measures))]
      spicy_fmt(
        "note_assoc_measure_item",
        .assoc_label(m),
        paste(lab, collapse = ", ")
      )
    },
    character(1)
  )
  paste0(spicy_str("note_prefix"), paste(parts, collapse = "; "), ".")
}


#' Categorical summary table
#'
#' @description
#' Builds a publication-ready frequency or cross-tabulation table for one
#' or many categorical variables selected with tidyselect syntax.
#'
#' With `by`, produces grouped cross-tabulation summaries (using
#' [cross_tab()] internally) with Chi-squared *p*-values and optional
#' association measures.
#' Without `by`, produces one-way frequency-style summaries.
#'
#' Multiple output formats are available via `output`: a printed ASCII
#' table (`"default"`), a wide or long numeric `data.frame`
#' (`"data.frame"`, `"long"`), or publication-ready tables
#' (`"tinytable"`, `"gt"`, `"flextable"`, `"excel"`, `"clipboard"`,
#' `"word"`).
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
#'   naming them.
#' @param by Optional grouping column used for columns/groups. Accepts an
#'   unquoted column name or a single character column name. Factor
#'   levels keep their declared order; any other `by` (character,
#'   numeric, haven labelled) forms group columns in order of first
#'   appearance in the data -- the same convention as
#'   [table_continuous()]. For a haven labelled `by`, the group
#'   headers are the raw codes (value labels are not used for group
#'   headers -- the family convention shared with [table_continuous()]
#'   and [table_continuous_lm()]); declared missing values follow
#'   `user_na` as usual.
#' @param labels An optional **named character vector** of variable
#'   labels whose names match column names in `data` (e.g.
#'   `c(smoking = "Current smoker")`) -- the same contract as
#'   [table_continuous()] and [table_continuous_lm()]. Only listed
#'   columns are relabelled. For the remaining columns (and when
#'   `labels = NULL`, the default), labels are auto-detected from the
#'   variable's label attribute (e.g. from `haven`); if none is found,
#'   the column name is used. Unnamed (positional) label vectors,
#'   accepted before 0.13.0, now raise an error.
#' @param levels_keep Optional character vector of levels to keep/order for row
#'   modalities. If `NULL`, all observed levels are kept. Entries must
#'   match the level strings the table displays (for labelled columns
#'   these are the `"[code] label"` strings, not the bare label
#'   text). When nothing matches
#'   for a selected variable, that variable is dropped from the table
#'   with a classed warning (`spicy_no_selection`) listing the
#'   available level strings.
#' @param include_total Logical. If `TRUE` (the default), includes a `Total` group
#'   when available.
#' @param drop_na Logical. If `FALSE` (the default), missing values are
#'   displayed as a dedicated `"(Missing)"` level (and, under `by`, a
#'   `"(Missing)"` group column) -- the field convention for descriptive
#'   tables (gtsummary's "Unknown" row, janitor's `NA` row; see the
#'   Epidemiologist R Handbook, Descriptive tables). If `TRUE`, rows with
#'   `NA` in the tabulated variable (and in `by`, when supplied) are
#'   removed BEFORE each cross-tabulation, and the removal is disclosed
#'   in a table note ("Missing values removed: ...") rather than silent.
#'   Before 0.13.0 the default was `TRUE` with no disclosure.
#' @param weights Optional weights. Either `NULL` (the default), a numeric vector
#'   of length `nrow(data)`, or a single column in `data` supplied as an
#'   unquoted name or a character string.
#' @param rescale Logical. If `FALSE` (the default), weights are used as-is.
#'   If `TRUE`, rescales weights so total weighted N matches raw N.
#'   Passed to `spicy::cross_tab()`. When the argument is not supplied,
#'   the default is read from `options(spicy.rescale)` (falling back to
#'   `FALSE`), matching [cross_tab()].
#' @param correct Logical. If `FALSE` (the default), no continuity correction is
#'   applied. If `TRUE`, applies Yates correction in 2x2 chi-squared contexts.
#'   Passed to `spicy::cross_tab()`.
#' @param simulate_p Logical. If `FALSE` (the default), uses asymptotic p-values.
#'   If `TRUE`, uses Monte Carlo simulation. Passed to `spicy::cross_tab()`.
#' @param simulate_B Integer. Number of Monte Carlo replicates when
#'   `simulate_p = TRUE`. Defaults to `2000`.
#' @param percent_digits Number of digits for percentages in report outputs.
#'   Defaults to `1`.
#' @param p_digits Integer >= 1. Number of decimal places used to
#'   render *p*-values in the `p` column (default: `3`, the APA
#'   Publication Manual standard). Both the displayed precision and
#'   the small-*p* threshold derive from this argument: `p_digits = 3`
#'   prints `.045` and `<.001`; `p_digits = 4` prints `.0451` and
#'   `<.0001`. Leading zeros are always stripped, following APA
#'   convention.
#' @param v_digits Number of digits for the association measure. Defaults
#'   to `2`.
#' @param assoc_measure Which association measure to report alongside the
#'   chi-squared *p*-value. Accepts four input shapes:
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
#'   the header collapses to `"Effect size"` and an APA-style
#'   `Note.` line is appended documenting which measure was used for
#'   which variable.
#'
#'   `phi` requires a 2x2 table; if explicitly requested for a
#'   non-2x2 variable, an error is raised so the user can choose
#'   another measure or fall back to `"auto"`.
#' @param assoc_ci Passed to [cross_tab()]. If `TRUE`, includes the
#'   confidence interval of the association measure. In wide raw
#'   outputs (`"data.frame"`, `"excel"`, `"clipboard"`), two extra
#'   columns `CI lower` / `CI upper` are added; in the long raw
#'   output (`"long"`) the bounds appear as `ci_lower` / `ci_upper`.
#'   In rendered formats (`"gt"`, `"tinytable"`, `"flextable"`,
#'   `"word"`), the CI is shown inline (e.g., `.14 [.08, .19]`).
#'   Defaults to `FALSE`.
#' @param smd Logical. If `TRUE`, adds an `SMD` column holding the
#'   standardized mean difference between the two groups of `by`,
#'   the balance diagnostic of the Table 1 literature. Requires
#'   exactly two groups; the value sits on the variable row beside
#'   `p`, never on a level row. Signed for a two-category variable
#'   (group 1 minus group 2 on the second category), unsigned for
#'   three or more, where it is a distance. No confidence interval
#'   and no p-value, by design. Rounded with `v_digits`. See the
#'   "Standardized mean difference" section below. Defaults to
#'   `FALSE`.
#' @param decimal_mark Decimal separator (`"."` or `","`). Defaults to `"."`.
#' @param align Horizontal alignment of numeric columns in the
#'   printed ASCII table and in the `tinytable`, `gt`, `flextable`,
#'   `word`, and `clipboard` outputs. The first column (`Variable`)
#'   is always left-aligned. One of:
#'   - `"decimal"` (default): align numeric columns on the decimal
#'     mark, the standard scientific-publication convention used by
#'     SPSS, SAS, and LaTeX `siunitx`. Numeric cells are pre-padded
#'     with figure-spaces (U+2007, digit-width) so every string in a
#'     column has the same width with the decimal mark at the same
#'     internal position; centring those uniform-width strings then
#'     stacks the decimal points vertically. The same pad-then-centre
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
#'   In the `excel` output, `"center"` centres the numeric columns and
#'   `"right"` is the same rendering as the default: cell-string padding
#'   does not align decimals under a proportional font, so `"decimal"`
#'   right-aligns instead, which combined with the per-column `numfmt`
#'   already produces dot-aligned columns. Same default and same three
#'   values as [table_continuous()] / [table_continuous_lm()], whose
#'   workbooks resolve `"decimal"` differently: `table_continuous()`
#'   right-aligns only the counts and the *p*-value there, and
#'   `table_continuous_lm()` applies that convention at every `align`.
#' @param output Output format. One of:
#'   - `"default"` (a printed ASCII table, returned invisibly)
#'   - `"data.frame"` (a wide numeric `data.frame`)
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
#'   grouped table (`by` supplied); like the other output-scoped
#'   presentation arguments (`excel_sheet`, `clipboard_delim`, ...), it
#'   is silently unused in every other output and in one-way tables,
#'   which have a single header row to begin with.
#' @param blank_na_wide Logical. If `FALSE` (the default), `NA` values are kept
#'   as-is in wide raw output. If `TRUE`, replaces them with empty strings.
#' @param excel_path Path for `output = "excel"`. Defaults to `NULL`.
#' @param excel_sheet Sheet name for Excel export. `NULL` (the
#'   default) uses `"Categorical"`.
#' @param clipboard_delim Delimiter for clipboard text export. Defaults
#'   to `"\t"`. A cell holding the delimiter itself, a double quote or
#'   a line break is quoted RFC 4180-style, so the grid survives
#'   whatever delimiter you choose.
#' @param word_path File path for `output = "word"`. Defaults to
#'   `NULL`. Before 0.13.0, supplying it with `output = "flextable"`
#'   also wrote a `.docx` as a side effect; it is now used exclusively
#'   by `output = "word"` (the contract shared with the rest of the
#'   table family) and is ignored, with a warning, under
#'   `output = "flextable"`.
#' @param user_na Logical. If `TRUE` (the default), declared missing
#'   values in the row variables and in `by` are treated as missing:
#'   they join the `"(Missing)"` level under `drop_na = FALSE`, and
#'   under `drop_na = TRUE` they are removed with a dedicated
#'   disclosure line (`Declared missing values removed: ...`). If
#'   `FALSE`, the declared codes stay valid categories. See the
#'   "Declared missing values" section of [freq()].
#'
#' @param style A journal or locale style: a theme name (`"jama"`,
#'   `"lancet"`, `"annals"`, `"apa"`, `"aer"`, `"fr"`), a
#'   [spicy_style()] object, or `NULL` (the default). A style only
#'   changes DEFAULTS -- any argument you pass explicitly wins over it.
#'   Set `options(spicy.style = )` for document-wide scope. A theme
#'   covers numeric formatting conformity only, not full editorial
#'   conformity; `?spicy_style` lists the exact rules each one encodes
#'   and the official document they come from. An unknown name is an
#'   error.
#'
#' @inheritSection freq Declared missing values
#'
#' @return Depends on `output`:
#' \itemize{
#'   \item `"default"`: prints a styled ASCII table and returns the
#'     underlying `data.frame` invisibly (S3 class
#'     `"spicy_categorical_table"`).
#'   \item `"data.frame"`: a wide `data.frame` with one row per
#'     variable--level combination.
#'     When `by` is used, the columns are `Variable`, `Level`, and one
#'     pair of `n` / `\%` columns per group level (plus `Total` when
#'     `include_total = TRUE`), followed by `Chi2`, `df`, `p`, and the
#'     association measure column.
#'     When `by = NULL`, the columns are `Variable`, `Level`, `n`, `\%`.
#'   \item `"long"`: a long `data.frame` with columns `variable`,
#'     `level`, `n`, `pct` (plus `group`, `chi2`, `df`, `p` when `by` is
#'     used). The association measure is always called `effect_size`,
#'     whichever measure it is, and `effect_size_type` names that measure
#'     per row (`"cramer_v"`, `"phi"`, ...), or is `NA` on the rows of a
#'     variable given `assoc_measure = "none"`. The wide outputs instead
#'     name the column after the measure, or `Effect size` when the row
#'     variables do not share one. With `smd = TRUE` this output also
#'     carries `smd` and `smd_type` (`"binary"` or `"multinomial"`,
#'     the kernel the value came from); the wide outputs name that
#'     column `SMD`. Like the association columns, both are ABSENT
#'     when the statistic is not requested.
#'   \item `"tinytable"`: a `tinytable` object.
#'   \item `"gt"`: a `gt_tbl` object.
#'   \item `"flextable"`: a `flextable` object.
#'   \item `"excel"` / `"word"`: writes to disk and returns the file
#'     path invisibly.
#'   \item `"clipboard"`: copies the table and returns the display
#'     `data.frame` invisibly.
#' }
#'
#' The `drop_na = TRUE` disclosure travels with the table on every
#' route, not just the console: `"default"` prints it under the ASCII
#' table, `"tinytable"` / `"gt"` / `"flextable"` / `"word"` carry it as
#' a table note, `"excel"` writes it below the body, and
#' `"data.frame"` keeps the sentence verbatim in the
#' `missing_note` attribute (`attr(x, "missing_note")`, `NULL` when
#' nothing was removed) so a pipeline that renders the numbers itself
#' can still state what left the table. On the `"tinytable"` route the
#' note is set one size down; `options(spicy.note_style)` governs that
#' (see [table_regression()]).
#'
#' The Excel sheet carries the same title the console prints on its
#' first row; the table itself starts on row 3.
#'
#' @details
#' # Tests
#'
#' When `by` is used, each selected variable is cross-tabulated
#' against the grouping variable with [cross_tab()] and the omnibus
#' chi-squared *p*-value is reported in the `p` column. See
#' `@param correct` / `simulate_p` to switch on Yates' continuity
#' correction or Monte Carlo *p*-values, and `@param assoc_measure`
#' for the per-row dispatch table used by `"auto"` (2x2 -> Phi,
#' both ordered -> Kendall's Tau-b, otherwise Cramer's V). Without
#' `by`, the table reports the marginal frequency distribution of
#' each variable with no inferential statistics.
#'
#' For model-based comparisons (cluster-robust SE, weighted contrasts,
#' fitted means) on continuous outcomes, see [table_continuous_lm()].
#' For descriptive (empirical) comparisons on continuous outcomes, see
#' [table_continuous()].
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
#' Two limits of the current grammar. This function has no `p_value`
#' argument, so the p column cannot be switched off here as it can in
#' [table_continuous()]; a complete balance table mixing continuous
#' and categorical variables will show a categorical p beside a
#' continuous column you removed. And [inline()] cannot quote this
#' `SMD` cell: like `p` and the association measure, it lives on the
#' variable row, which `inline()` cannot address on a variable that
#' has levels. The continuous `SMD` cell is quotable
#' (`inline(tbl, x, "A", column = "smd")`); for the categorical one,
#' read `output = "long"`.
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
#' # column header collapses to `"Effect size"` and an APA-style `Note.` line
#' # documents which measure was used for which variable.
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
  labels = NULL,
  levels_keep = NULL,
  include_total = TRUE,
  drop_na = FALSE,
  weights = NULL,
  rescale = FALSE,
  correct = FALSE,
  simulate_p = FALSE,
  simulate_B = 2000,
  percent_digits = 1,
  p_digits = 3,
  v_digits = 2,
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
  style = NULL
) {
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

  # Global options (mirrors cross_tab()): an explicitly supplied
  # argument always wins; the option only fills in the default.
  if (missing(rescale)) {
    rescale <- getOption("spicy.rescale", FALSE)
  }

  if (!is.data.frame(data)) {
    spicy_abort("`data` must be a data.frame.", class = "spicy_invalid_data")
  }
  # `word_path` is a save path for `output = "word"` only (the contract
  # shared with the rest of the table family). Before 0.13.0,
  # `output = "flextable"` also wrote a .docx as a side effect when
  # `word_path` was supplied; warn so old callers see the change
  # instead of silently getting no file.
  if (
    !is.null(word_path) && nzchar(word_path) && identical(output, "flextable")
  ) {
    spicy_warn(
      c(
        "`word_path` is ignored when `output = \"flextable\"`.",
        "i" = "Use `output = \"word\"` to write a .docx, or save the returned object with `flextable::save_as_docx()`."
      ),
      class = "spicy_ignored_arg"
    )
  }
  by_quo <- rlang::enquo(by)
  has_group <- !rlang::quo_is_null(by_quo)
  by_name <- NULL
  if (has_group) {
    by_name <- resolve_single_column_selection(by_quo, data, "by")
  }
  # Validated HERE rather than beside the other logicals below: the
  # refusal of a `by`-less `smd` reads it a few lines from now, and a
  # non-logical value must not reach `&&` first.
  if (!is.logical(smd) || length(smd) != 1 || is.na(smd)) {
    spicy_abort("`smd` must be TRUE/FALSE.", class = "spicy_invalid_input")
  }
  if (smd && !has_group) {
    spicy_warn(
      "`smd` is ignored when `by` is not used: a standardized mean difference compares two groups.",
      class = "spicy_ignored_arg"
    )
  }
  do_smd <- smd && has_group

  select_quo <- rlang::enquo(select)
  select_val <- tryCatch(
    rlang::eval_tidy(select_quo, env = rlang::quo_get_env(select_quo)),
    error = function(e) NULL
  )
  select_names <- if (is.character(select_val)) {
    select_val
  } else {
    tryCatch(
      names(tidyselect::eval_select(select_quo, data)),
      error = function(e) {
        spicy_abort(
          "`select` must select at least one column in `data`.",
          class = "spicy_invalid_input"
        )
      }
    )
  }
  if (length(select_names) == 0) {
    spicy_abort(
      "`select` must select at least one column in `data`.",
      class = "spicy_invalid_input"
    )
  }
  if (!all(select_names %in% names(data))) {
    spicy_abort(
      "Some `select` columns are missing in `data`.",
      class = "spicy_missing_column"
    )
  }
  # Select-less call: restrict the `everything()` default to eligible
  # categorical columns (factor, character, logical, labelled),
  # excluding `by` -- the categorical mirror of the numeric
  # auto-restriction in the continuous companions. An explicit
  # `select` is taken verbatim so numeric-coded categorical variables
  # can still be tabulated by naming them.
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
    select_names <- setdiff(select_names[is_eligible], by_name)
    if (length(select_names) == 0L) {
      spicy_warn(
        "No categorical columns selected.",
        class = "spicy_no_selection"
      )
      return(data.frame())
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
  # `labels` must be a NAMED character vector keyed by column name in
  # `data` (the contract shared with the continuous companions). Only
  # listed columns are relabelled; the rest fall back to the column's
  # `label` attribute (haven / labelled convention), then to the
  # column name. Positional (unnamed) vectors -- the spicy < 0.13.0
  # legacy form -- are rejected with a migration hint.
  if (!is.null(labels)) {
    if (
      !is.character(labels) ||
        is.null(names(labels)) ||
        !all(nzchar(names(labels)))
    ) {
      spicy_abort(
        c(
          "`labels` must be a named character vector.",
          "x" = "Unnamed (positional) `labels` vectors are no longer accepted.",
          "i" = "Name the values by column: `labels = c(smoking = \"Current smoker\")`."
        ),
        class = "spicy_invalid_input"
      )
    }
    unknown <- setdiff(names(labels), names(data))
    if (length(unknown) > 0L) {
      spicy_abort(
        sprintf(
          "Names in `labels` not found in `data`: %s.",
          paste(unknown, collapse = ", ")
        ),
        class = "spicy_missing_column"
      )
    }
  }
  labels <- resolve_variable_labels(data, select_names, labels)

  if (
    !is.logical(include_total) ||
      length(include_total) != 1 ||
      is.na(include_total)
  ) {
    spicy_abort(
      "`include_total` must be TRUE/FALSE.",
      class = "spicy_invalid_input"
    )
  }
  if (!is.logical(drop_na) || length(drop_na) != 1 || is.na(drop_na)) {
    spicy_abort("`drop_na` must be TRUE/FALSE.", class = "spicy_invalid_input")
  }
  validate_varlist_logical(user_na, "user_na")
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
  # Disclosure note for drop_na = TRUE (NULL otherwise / when nothing
  # was dropped). Read lazily at assembly time, after the tabulation
  # loops have filled the ledger; print() appends it to the
  # association note.
  build_missing_note <- function() {
    if (!drop_na) {
      return(NULL)
    }
    parts <- character(0)
    if (length(na_dropped)) {
      parts <- c(
        parts,
        paste0(
          spicy_str("note_missing_removed"),
          paste(
            spicy_fmt("note_missing_item", names(na_dropped), na_dropped),
            collapse = ", "
          ),
          "."
        )
      )
    }
    if (length(user_na_dropped)) {
      parts <- c(
        parts,
        paste0(
          spicy_str("note_declared_missing_removed"),
          paste(
            spicy_fmt(
              "note_missing_item",
              names(user_na_dropped),
              user_na_dropped
            ),
            collapse = ", "
          ),
          "."
        )
      )
    }
    if (by_na_dropped > 0L) {
      parts <- c(
        parts,
        spicy_fmt("note_rows_missing_by_removed", by_name, by_na_dropped)
      )
    }
    if (length(parts)) paste(parts, collapse = " ") else NULL
  }
  if (!is.logical(rescale) || length(rescale) != 1 || is.na(rescale)) {
    spicy_abort("`rescale` must be TRUE/FALSE.", class = "spicy_invalid_input")
  }
  if (!is.logical(correct) || length(correct) != 1 || is.na(correct)) {
    spicy_abort("`correct` must be TRUE/FALSE.", class = "spicy_invalid_input")
  }
  if (!is.logical(simulate_p) || length(simulate_p) != 1 || is.na(simulate_p)) {
    spicy_abort(
      "`simulate_p` must be TRUE/FALSE.",
      class = "spicy_invalid_input"
    )
  }
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
  if (
    !is.logical(add_multilevel_header) ||
      length(add_multilevel_header) != 1 ||
      is.na(add_multilevel_header)
  ) {
    spicy_abort(
      "`add_multilevel_header` must be TRUE/FALSE.",
      class = "spicy_invalid_input"
    )
  }
  if (
    !is.logical(blank_na_wide) ||
      length(blank_na_wide) != 1 ||
      is.na(blank_na_wide)
  ) {
    spicy_abort(
      "`blank_na_wide` must be TRUE/FALSE.",
      class = "spicy_invalid_input"
    )
  }
  if (!.is_single_char(decimal_mark)) {
    spicy_abort(
      "`decimal_mark` must be a single character (e.g. '.' or ',').",
      class = "spicy_invalid_input"
    )
  }
  for (.dname in c("percent_digits", "v_digits")) {
    .dval <- get(.dname)
    if (
      !is.numeric(.dval) || length(.dval) != 1L || is.na(.dval) || .dval < 0
    ) {
      spicy_abort(
        paste0("`", .dname, "` must be a single non-negative number."),
        class = "spicy_invalid_input"
      )
    }
  }
  # `p_digits` has a stricter floor than the other digits arguments:
  # a 0-decimal p-value is meaningless, and `format_p_value()` would
  # silently fall back to 3 decimals. Same classed check as
  # table_continuous() / table_continuous_lm() / cross_tab().
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
  percent_digits <- as.integer(percent_digits)
  p_digits <- as.integer(p_digits)
  v_digits <- as.integer(v_digits)

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

  weights_quo <- rlang::enquo(weights)
  weights_vec <- resolve_weights_argument(weights_quo, data, "weights")

  if (isTRUE(rescale) && is.null(weights_vec)) {
    spicy_warn(
      "`rescale = TRUE` has no effect without `weights`; using `rescale = FALSE`.",
      class = "spicy_ignored_arg"
    )
    rescale <- FALSE
  }

  # Scan DECLARED factor levels as well as observed values: a level
  # literally named "(Missing)" that is declared but never observed
  # would otherwise slip past the guard, and .add_missing_level()
  # would then build factor(levels = c(..., "(Missing)", "(Missing)"))
  # -- a raw "factor level is duplicated" crash.
  all_values <- unique(unlist(
    lapply(c(select_names, by_name), function(nm) {
      col <- data[[nm]]
      c(as.character(col), levels(col))
    }),
    use.names = FALSE
  ))
  missing_label <- spicy_str("row_missing_level")
  if (missing_label %in% all_values) {
    idx <- 1L
    repeat {
      candidate <- spicy_fmt("row_missing_level_dedup", idx)
      if (!(candidate %in% all_values)) {
        missing_label <- candidate
        break
      }
      idx <- idx + 1L
    }
  }

  # `measure_key` is the measure the caller ASKED this row for, as a key
  # ("cramer_v", ..., or "none"). It rides through in `measure` so the
  # long output can publish it per row: reading it back off the
  # cross_tab() object would mean recognising a display label.
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

  fmt_num <- function(x, digits = 1, na = "") {
    out <- ifelse(is.na(x), na, formatC(x, format = "f", digits = digits))
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
  # three `table_*` functions print *p*-values identically: `p_digits`
  # drives both the displayed precision AND the small-*p* threshold
  # (`p_digits = 3` -> `<.001`, `p_digits = 4` -> `<.0001`, etc.),
  # leading zeros are stripped, and the configured `decimal_mark` is
  # honoured. The `op` argument carries the comparison operator
  # parsed from `cross_tab()`'s note text in the rare fallback path
  # where the numeric p-value is unavailable: `op = "<"` means the
  # underlying note literally said "p < threshold", so we honour the
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
    s <- formatC(v, format = "f", digits = v_digits)
    # The association measure is bounded, so it follows the same
    # leading-zero policy as a p-value: dropped by default (APA), kept
    # under a style that says so (`p_style = "standard"`).
    s <- .strip_leading_zero(s, ".", .style_p_leading_zero())
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
      return(spicy_str("cell_undefined"))
    }
    s <- formatC(v, format = "f", digits = v_digits)
    if (decimal_mark != ".") {
      s <- sub("\\.", decimal_mark, s)
    }
    s
  }

  # `rows` are the level rows, read from the typed roles of the
  # structured view -- not sniffed back from the indent prefix, so a
  # variable label starting with `base_indent` keeps its label.
  make_stronger_indent <- function(x, base_indent, strong_indent, rows) {
    if (length(rows)) {
      suffix <- substring(x[rows], nchar(base_indent) + 1L)
      x[rows] <- paste0(strong_indent, suffix)
    }
    x
  }

  # Pre-pad numeric (i.e. non-Variable) columns of a display data
  # frame with figure-spaces (U+2007, digit-width) so the decimal
  # mark falls at the same horizontal position across each column.
  # Used by every body-rendering engine (`gt`, `tinytable`,
  # `flextable`, `word`, ASCII print) so the rendered
  # output is homogeneous: centring uniform-width strings stacks the
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
      if (!drop_na && anyNA(x)) {
        x <- .add_missing_level(x, missing_label)
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

      lv_use <- if (is.null(levels_keep)) {
        known <- intersect(var_level_order, raw_levels)
        extra <- setdiff(raw_levels, c(var_level_order, missing_label))
        missing_end <- intersect(raw_levels, missing_label)
        c(known, extra, missing_end)
      } else {
        intersect(as.character(levels_keep), raw_levels)
      }
      if (!is.null(levels_keep) && length(lv_use) == 0L) {
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
          variable = labels[i],
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
      long_raw$variable <- factor(long_raw$variable, levels = labels)
      if (!is.null(levels_keep)) {
        long_raw$level <- factor(
          long_raw$level,
          levels = as.character(levels_keep)
        )
        long_raw <- long_raw[
          order(long_raw$variable, long_raw$level, method = "radix"),
          ,
          drop = FALSE
        ]
      } else {
        # Sort on the per-variable rank captured at build time -- each
        # variable keeps its own level order, "(Missing)" stays last in
        # every block.
        long_raw <- long_raw[
          order(long_raw$variable, long_raw$.lvrank, method = "radix"),
          ,
          drop = FALSE
        ]
      }
      long_raw$.lvrank <- NULL
      long_raw$variable <- as.character(long_raw$variable)
      long_raw$level <- as.character(long_raw$level)
      rownames(long_raw) <- NULL
    }

    if (output == "long") {
      return(long_raw)
    }

    wide_raw <- data.frame(
      Variable = long_raw$variable,
      Level = long_raw$level,
      n = long_raw$n,
      check.names = FALSE
    )
    wide_raw[["%"]] <- long_raw$pct

    if (blank_na_wide && nrow(wide_raw) > 0) {
      for (j in seq_len(ncol(wide_raw))) {
        if (j > 2) {
          wide_raw[[j]] <- ifelse(
            is.na(wide_raw[[j]]),
            "",
            as.character(wide_raw[[j]])
          )
        }
      }
    }

    if (output == "data.frame") {
      # The raw frame keeps the ledger as an attribute: a pipeline that
      # re-renders the numbers itself must still be able to state what
      # was removed. See the `output` section of the docs.
      attr(wide_raw, "missing_note") <- build_missing_note()
      return(wide_raw)
    }

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
      for (lab in labels) {
        sv <- long_raw[long_raw$variable == lab, , drop = FALSE]
        if (nrow(sv) == 0) {
          next
        }

        lv_use <- if (is.null(levels_keep)) {
          unique(sv$level)
        } else {
          intersect(as.character(levels_keep), unique(sv$level))
        }

        if (mode == "char") {
          r0 <- as.list(setNames(rep("", length(report_cols)), report_cols))
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
            r1$n <- fmt_n(sl$n[1])
            r1[["%"]] <- fmt_num(sl$pct[1], percent_digits)
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
    # the same disclosure, not print() alone.
    missing_note <- build_missing_note()

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
      indent_text = indent_text,
      percent_digits = percent_digits,
      p_digits = p_digits,
      v_digits = v_digits,
      decimal_mark = decimal_mark
    )

    if (output == "default") {
      out <- wide_raw
      attr(out, "display_df") <- report_wide_char
      attr(out, "group_var") <- NULL
      attr(out, "missing_note") <- missing_note
      attr(out, "indent_text") <- indent_text
      attr(out, "align") <- align
      attr(out, "decimal_mark") <- decimal_mark
      attr(out, "long_data") <- long_raw
      attr(out, "structured") <- structured
      class(out) <- c("spicy_categorical_table", "spicy_table", "data.frame")
      print(out)
      return(invisible(out))
    }

    if (output == "tinytable") {
      if (!requireNamespace("tinytable", quietly = TRUE)) {
        spicy_abort("Install package 'tinytable'.", class = "spicy_missing_pkg")
      }
      old_tt_opt <- getOption("tinytable_print_output")
      options(tinytable_print_output = "html")
      on.exit(options(tinytable_print_output = old_tt_opt), add = TRUE)

      dat_tt <- report_wide_char
      # Pre-pad numeric cells so centring stacks the decimal points
      # vertically (same strategy as table_regression() /
      # table_continuous_lm()). The native tinytable
      # `style_tt(align = "d")` centres each cell on its own value
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
        escape = FALSE,
        caption = .categorical_title(NULL),
        notes = missing_note
      )
      tt <- .spicy_tt_bare(tt)
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
      # Pre-pad numeric cells so centring stacks the decimal points
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
      return(.spicy_gt_attach_note(tbl, missing_note))
    }

    build_flextable_oneway <- function(df) {
      if (!requireNamespace("flextable", quietly = TRUE)) {
        spicy_abort("Install package 'flextable'.", class = "spicy_missing_pkg")
      }
      df <- pad_decimal_cols(df)
      # Level rows carry the console's indent inside the label cell
      # and the engine indents them again below (`padding.left`). One
      # indentation is the design; two is an artefact of reading a
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
      # Numeric column alignment honours `align`. For "decimal" the
      # cells were pre-padded above; right-aligning the padded strings
      # preserves the dot-aligned column. Use a monospace font in the
      # body so character widths match. For "center" / "right",
      # apply the literal alignment.
      num_j <- 2:ncol(df)
      if (identical(align, "decimal") && length(num_j) > 0L) {
        # Single-font policy (matches table_regression()):
        # cells were pre-padded above for uniform width; centring
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
      ft <- .spicy_ft_attach_note(ft, missing_note)
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
      if (is.null(word_path) || !nzchar(word_path)) {
        spicy_abort(
          "Provide `word_path` for output = 'word'.",
          class = "spicy_invalid_input"
        )
      }
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
      if (is.null(excel_path) || !nzchar(excel_path)) {
        spicy_abort(
          "Provide `excel_path` for output = 'excel'.",
          class = "spicy_invalid_input"
        )
      }
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
      wb <- openxlsx2::wb_add_data(wb, x = xl_title, start_row = 1)
      header_row <- 3L
      # `na.strings = ""` so the empty cells of a variable-header row
      # stay blank. Without it openxlsx2 writes Excel ERROR cells
      # ("#N/A") in the middle of the counts, and any SUM over the
      # column inherits the error.
      # `col_names = TRUE` writes `names(x)` as the sheet's header row,
      # so the frame handed to the writer carries the LABELS -- the
      # `by` branch already writes its two header rows from the display
      # vectors, and this route used to write the keys instead.
      xl_header <- unname(report_labels[names(body_xl)])
      wb <- openxlsx2::wb_add_data(
        wb,
        x = setNames(body_xl, xl_header),
        start_row = header_row,
        col_names = TRUE,
        row_names = FALSE,
        na.strings = ""
      )

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
        # numeric columns honour `align`. For "decimal", Excel
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
        note = .categorical_note(missing_note, NULL),
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
        note = .categorical_note(missing_note, NULL)
      )
      clipr::write_clip(txt)
      spicy_inform("Categorical table copied to clipboard.")
      return(invisible(txt))
    }
  }

  g0 <- resolve_user_na(data[[by_name]])
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
  group_levels <- if (is.factor(g0)) {
    levels(g0)
  } else {
    unique(as.character(g0[!is.na(g0)]))
  }
  group_levels <- as.character(group_levels)
  # The REAL groups of the table, captured here and used by the SMD --
  # BEFORE the "(Missing)" level is appended below and BEFORE the margin
  # key joins the vector. Both would otherwise inflate the count and
  # refuse a legitimate two-group table (`include_total = TRUE` is the
  # default, so the naive count is off by one on EVERY grouped table),
  # and a `setdiff()` by name would be defeated by a user level
  # homonymous with either -- both of which are auto-renamed on
  # collision, precisely because they are display strings.
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
  if (!drop_na && any(is.na(g0))) {
    group_levels <- unique(c(group_levels, missing_label))
  }
  # No group left to tabulate. The group columns are keyed
  # `paste0(<level>, " n")`, and `paste0(character(0), " n")` is " n" --
  # R recycles the zero-length side to "" -- so the table used to grow a
  # pair of phantom columns named " n" and " %", with no rows under
  # them. tinytable then refused a zero-row table, gt refused two empty
  # column names, and flextable died on a recycling mismatch: three
  # different errors, none of them naming the cause.
  #
  # Two shapes reach here and the message must fit BOTH: every
  # observation missing `by` (where `drop_na = FALSE` is a real remedy,
  # verified), and no observation at all (where it is not -- the same
  # refusal fires either way, so offering it would be a lie).
  if (length(group_levels) == 0L) {
    spicy_abort(
      c(
        sprintf("`by = %s` has no level to tabulate.", by_name),
        "x" = if (length(g0) == 0L) {
          "The data has no rows."
        } else {
          "Every observation is missing it."
        },
        if (any(is.na(g0))) {
          c(
            "i" = "`drop_na = FALSE` tabulates the missing observations as their own category."
          )
        }
      ),
      class = "spicy_invalid_data"
    )
  }
  # Internal KEY for the margin column, never a label: it is a value of
  # `long$group`, the prefix of a public column name ("Total n") and the
  # `total_group` attribute, and every site that recognises the margin --
  # the wide and report fills, the typed view's `total` flag, tidy() and
  # glance() -- compares it against another key, never against a rendered
  # header.
  #
  # The key is "Total" unless the by-variable has a level literally named
  # "Total", in which case the first free "Total_<i>" is used -- mirroring
  # cross_tab()'s own margin auto-rename -- so the user's group keeps its
  # name and position among the group columns while the true margin is
  # always present and unambiguous. (Before 0.13.0 the margin silently
  # vanished and the user's "Total" group posed as it.)
  margin_key <- .CAT_MARGIN_KEY
  if (margin_key %in% group_levels) {
    idx <- 1L
    repeat {
      candidate <- paste0(.CAT_MARGIN_KEY, "_", idx)
      if (!(candidate %in% group_levels)) {
        margin_key <- candidate
        break
      }
      idx <- idx + 1L
    }
  }
  if (include_total) {
    if (!identical(margin_key, .CAT_MARGIN_KEY)) {
      spicy_warn(
        c(
          sprintf(
            "`%s` has a group level literally named \"Total\", which collides with the margin column; the margin is displayed as \"%s\".",
            by_name,
            margin_key
          ),
          "i" = "Rename the conflicting `by` level to restore the default margin label."
        ),
        class = "spicy_renamed_column"
      )
    }
    group_levels <- c(group_levels, margin_key)
  }
  # The margin key names a COLUMN only when the margin is displayed;
  # otherwise it is a disambiguation artefact with no header to label.
  # Stage-2 note: the collision loop above compares `by` levels to the
  # KEY, so once the margin's header is translated a user level may
  # print the same word as the margin without triggering the rename.
  margin_col_key <- if (include_total) margin_key else NULL
  # The margin collision is disclosed once, above; the identical
  # per-call disclosures the internal cross_tab() calls would emit
  # (three per variable) are muffled as redundant noise.
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

  for (i in seq_along(select_names)) {
    x_raw <- data[[select_names[i]]]
    n_user <- if (user_na) sum(.user_na_mask(x_raw)) else 0L
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
    if (!length(x)) {
      next
    }
    if (!drop_na) {
      x_has_na <- anyNA(x)
      g_has_na <- anyNA(g)
      if (x_has_na) {
        x <- .add_missing_level(x, missing_label)
      }
      if (g_has_na) {
        g <- .add_missing_level(g, missing_label)
      }
    } else {
      x_has_na <- FALSE
      g_has_na <- FALSE
    }

    this_measure <- assoc_measures_per_row[[select_names[i]]]
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
    # the table actually tested.
    ct_pct <- quiet_margin(spicy::cross_tab(
      x,
      g,
      percent = "c",
      weights = w,
      rescale = rescale,
      correct = correct,
      simulate_p = simulate_p,
      simulate_B = simulate_B,
      digits = 15L,
      include_stats = !has_missing_level,
      assoc_measure = this_measure,
      assoc_ci = assoc_ci
    ))
    ct_n <- quiet_margin(spicy::cross_tab(
      x,
      g,
      weights = w,
      rescale = rescale,
      digits = 15L,
      include_stats = FALSE
    ))
    st <- if (has_missing_level) {
      cc <- x != missing_label & g != missing_label
      ct_stats <- quiet_margin(spicy::cross_tab(
        x[cc],
        g[cc],
        percent = "c",
        weights = if (!is.null(w)) w[cc] else NULL,
        rescale = rescale,
        correct = correct,
        simulate_p = simulate_p,
        simulate_B = simulate_B,
        assoc_measure = this_measure,
        assoc_ci = assoc_ci
      ))
      parse_stats(ct_stats, this_measure)
    } else {
      parse_stats(ct_pct, this_measure)
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
    if (do_smd) {
      cc <- !is.na(x) & !is.na(g)
      if (has_missing_level) {
        cc <- cc & x != missing_label & g != missing_label
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
    lv_use <- if (is.null(levels_keep)) {
      # Reorder to match original factor/occurrence order
      known <- intersect(var_level_order, raw_levels)
      extra <- setdiff(raw_levels, c(var_level_order, missing_label))
      missing_end <- intersect(raw_levels, missing_label)
      c(known, extra, missing_end)
    } else {
      intersect(as.character(levels_keep), vals_n)
    }
    if (!is.null(levels_keep) && length(lv_use) == 0L) {
      .warn_levels_keep_no_match(select_names[i], raw_levels)
      next
    }
    all_level_order <- c(all_level_order, lv_use)

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
        row_df <- data.frame(
          variable = labels[i],
          level = lv,
          # Per-variable display rank (see the no-by branch): keeps
          # each block's own level order under the global sort.
          .lvrank = match(lv, lv_use),
          group = gr,
          n = suppressWarnings(as.numeric(n_val)),
          pct = suppressWarnings(as.numeric(pct_val)),
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
          # Defer the rename `.assoc` -> measure_col to post-loop, where
          # we know whether all rows used the same measure (uniform
          # column header) or a mix (collapsed to the generic name).
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
          # Which kernel produced it -- and therefore whether it is
          # signed. `"binary"` is signed (group 1 minus group 2 on the
          # second level); `"multinomial"` is a distance and never is.
          row_df$.smd_type <- smd_kind
        }
        rows[[rr]] <- row_df
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
  assoc_note_text <- .assoc_note_apa(assoc_measures_per_row, labels)
  # The SMD gloss rides in the STATISTICS note slot beside the
  # association gloss, so all six output routes carry it from one
  # place. The multivariate sentence appears only when at least one
  # variable has more than two levels -- it explains an unsigned
  # column, and there is nothing to explain when every column is
  # signed.
  if (do_smd) {
    smd_note_text <- build_smd_note(TRUE, real_group_levels, decimal_mark)
    if (
      any(vapply(
        select_names,
        function(v) {
          nlevels(.tab_factor(resolve_user_na(data[[v]]))) > 2L
        },
        logical(1)
      ))
    ) {
      smd_note_text <- paste(
        smd_note_text,
        spicy_fmt("note_gloss_smd_multinomial", spicy_str("header_smd"))
      )
    }
    assoc_note_text <- paste(
      c(assoc_note_text[nzchar(assoc_note_text)], smd_note_text),
      collapse = " "
    )
  }

  if (length(rows) == 0) {
    long_raw <- data.frame(
      variable = character(0),
      level = character(0),
      group = character(0),
      n = numeric(0),
      pct = numeric(0),
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
    long_raw$variable <- factor(long_raw$variable, levels = labels)
    if (!is.null(levels_keep)) {
      long_raw$level <- factor(
        long_raw$level,
        levels = as.character(levels_keep)
      )
      long_raw$group <- factor(long_raw$group, levels = group_levels)
      long_raw <- long_raw[
        order(
          long_raw$variable,
          long_raw$level,
          long_raw$group,
          method = "radix"
        ),
        ,
        drop = FALSE
      ]
    } else {
      # Per-variable rank (see the no-by branch): each block keeps its
      # own level order, "(Missing)" stays last per variable.
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
    }
    long_raw$.lvrank <- NULL
    long_raw$variable <- as.character(long_raw$variable)
    long_raw$level <- as.character(long_raw$level)
    long_raw$group <- as.character(long_raw$group)
    rownames(long_raw) <- NULL
  }

  if (output == "long") {
    out <- long_raw
    out$p_op <- NULL
    if (!show_assoc || !assoc_ci) {
      out$ci_lower <- NULL
      out$ci_upper <- NULL
    }
    if (show_assoc) {
      # The long format is the machine-readable one: its measure column
      # has a STABLE name, and the measure each row carries travels
      # beside it as a key. Renaming, not assigning, so both keep the
      # position they were built in.
      names(out)[names(out) == measure_col] <- "effect_size"
      names(out)[names(out) == ".assoc_type"] <- "effect_size_type"
    }
    if (do_smd) {
      # `smd` / `smd_type`, mirroring `effect_size` / `effect_size_type`
      # beside them -- NOT the `smd_value` of the continuous frame,
      # whose neighbour is `es_value`. Each frame mirrors its own.
      #
      # RULED, knowingly: one quantity therefore has two raw names
      # across the two families. The alternative -- one name everywhere
      # -- would make this column the only one here NOT matching its
      # neighbour, and the inter-family split it would fix predates the
      # SMD: `es_value`/`es_type` versus `effect_size`/`effect_size_type`
      # are already two spellings of one idea. Aligning all three pairs
      # is a pre-1.0 naming decision of its own, on the register; doing
      # it for the newest pair alone would leave the older two split
      # and add a third convention.
      names(out)[names(out) == ".smd"] <- "smd"
      names(out)[names(out) == ".smd_type"] <- "smd_type"
    }
    return(out)
  }
  # Only the long output publishes the per-row measure key; the display
  # and export routes below read the measure column by name.
  long_raw$.assoc_type <- NULL
  long_raw$.smd_type <- NULL

  # ---------------- WIDE RAW ----------------
  make_wide_raw <- function(ldf) {
    cols <- c(
      .CAT_KEY_VARIABLE,
      "Level",
      as.vector(rbind(.cat_key_n(group_levels), .cat_key_pct(group_levels))),
      "Chi2",
      "df",
      .CAT_KEY_P
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
      r$Variable <- key$variable[k]
      r$Level <- key$level[k]

      for (gr in group_levels) {
        s <- sv[sv$group == gr, , drop = FALSE]
        r[[.cat_key_n(gr)]] <- if (nrow(s)) s$n[1] else NA_real_
        r[[.cat_key_pct(gr)]] <- if (nrow(s)) s$pct[1] else NA_real_
      }

      r$Chi2 <- if (nrow(sv)) sv$chi2[1] else NA_real_
      r$df <- if (nrow(sv)) sv$df[1] else NA_real_
      r$p <- if (nrow(sv)) sv$p[1] else NA_real_
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

    w <- do.call(rbind, out)

    if (blank_na_wide) {
      for (j in seq_len(ncol(w))) {
        if (j > 2) w[[j]] <- ifelse(is.na(w[[j]]), "", as.character(w[[j]]))
      }
    }
    w
  }

  wide_raw <- make_wide_raw(long_raw)
  # Read the ledger once, here: every output route below must carry the
  # same disclosure, not print() alone.
  missing_note <- build_missing_note()
  if (output == "data.frame") {
    # The raw frame keeps the ledger as an attribute: a pipeline that
    # re-renders the numbers itself must still be able to state what was
    # removed. See the `output` section of the docs.
    attr(wide_raw, "missing_note") <- missing_note
    return(wide_raw)
  }

  # ---------------- REPORT WIDE ----------------
  report_cols <- c(
    .CAT_KEY_VARIABLE,
    as.vector(rbind(.cat_key_n(group_levels), .cat_key_pct(group_levels))),
    .CAT_KEY_P
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
  n_stat_cols <- 1L +
    as.integer(show_assoc) +
    2L * as.integer(show_assoc && assoc_ci) +
    as.integer(do_smd)
  # The same count once the CI bounds have been merged into the measure
  # cell, which every rendered engine does before laying out.
  n_stat_cols_merged <- 1L + as.integer(show_assoc) + as.integer(do_smd)

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
        out$p <- character(0)
        return(out[, report_cols, drop = FALSE])
      }
    }

    out <- list()
    z <- 1L

    for (lab in labels) {
      sv <- ldf[ldf$variable == lab, , drop = FALSE]
      if (nrow(sv) == 0) {
        next
      }

      lv_use <- if (is.null(levels_keep)) {
        unique(sv$level)
      } else {
        intersect(as.character(levels_keep), unique(sv$level))
      }

      # variable row
      if (mode == "char") {
        r0 <- as.list(setNames(rep("", length(report_cols)), report_cols))
      } else {
        r0 <- as.list(setNames(rep(NA, length(report_cols)), report_cols))
      }
      r0$Variable <- lab
      r0$p <- fmt_p(sv$p[1], sv$p_op[1])
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

        for (gr in group_levels) {
          sx <- sl[sl$group == gr, , drop = FALSE]
          n_val <- if (nrow(sx)) sx$n[1] else NA_real_
          p_val <- if (nrow(sx)) sx$pct[1] else NA_real_

          if (mode == "char") {
            r1[[.cat_key_n(gr)]] <- fmt_n(n_val)
            r1[[.cat_key_pct(gr)]] <- fmt_num(p_val, percent_digits)
          } else {
            r1[[.cat_key_n(gr)]] <- n_val
            r1[[.cat_key_pct(gr)]] <- p_val
          }
        }

        r1$p <- ""
        if (show_assoc) {
          r1[[measure_col]] <- ""
        }
        if (do_smd) {
          # Written explicitly rather than inherited from the
          # `rep("", ...)` initialisation above: a level row carries no
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
    indent_text = indent_text,
    percent_digits = percent_digits,
    p_digits = p_digits,
    v_digits = v_digits,
    decimal_mark = decimal_mark,
    group_levels = group_levels,
    margin_key = margin_col_key,
    measure_col = measure_col,
    measure_label = measure_label,
    show_assoc = show_assoc,
    assoc_ci = assoc_ci,
    show_smd = do_smd
  )

  if (output == "default") {
    out <- wide_raw
    attr(out, "display_df") <- report_wide_char
    attr(out, "missing_note") <- missing_note
    attr(out, "group_var") <- by_name
    attr(out, "indent_text") <- indent_text
    attr(out, "align") <- align
    attr(out, "decimal_mark") <- decimal_mark
    attr(out, "long_data") <- long_raw
    attr(out, "assoc_note") <- assoc_note_text
    attr(out, "structured") <- structured
    if (include_total) {
      # The internal key of the margin group in `long_data` ("Total",
      # or the auto-renamed "Total_<i>" when a `by` level collides).
      # Read by tidy() / glance() to drop the margin without ever
      # mistaking a user group named "Total" for it.
      attr(out, "total_group") <- margin_key
    }
    class(out) <- c("spicy_categorical_table", "spicy_table", "data.frame")
    print(out)
    return(invisible(out))
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

  # Headers (base: without CI; used by rendered formats). Pure DISPLAY:
  # the column keys travel separately (flextable takes
  # `col_keys = names(df)`, the sheet and the clipboard write these rows
  # verbatim), so these three vectors carry labels only.
  top_header_span <- c(
    spicy_str("header_variable"),
    rep(group_labels, each = 2),
    spicy_str("header_p")
  )
  top_header_flat <- c(
    spicy_str("header_variable"),
    as.vector(rbind(group_labels, rep("", length(group_labels)))),
    spicy_str("header_p")
  )
  bot_header <- c(
    "",
    rep(
      c(spicy_str("header_n_lower"), spicy_str("header_percent_symbol")),
      times = length(group_levels)
    ),
    ""
  )
  if (show_assoc) {
    top_header_span <- c(top_header_span, measure_label)
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
    # Pre-pad numeric cells so centring stacks the decimal points
    # vertically (same strategy as table_regression() /
    # table_continuous_lm()). The native tinytable
    # `style_tt(align = "d")` centres each cell on its own value
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
      setNames(
        list(ncol(dat_tt) - n_stat_cols_merged + 1L),
        spicy_str("header_p")
      )
    )
    if (show_assoc) {
      gspec[[measure_label]] <- ncol(dat_tt) - as.integer(do_smd)
    }
    if (do_smd) {
      gspec[[smd_label]] <- ncol(dat_tt)
    }

    tt <- tinytable::tt(
      dat_tt,
      escape = FALSE,
      caption = .categorical_title(by_name),
      # The association-measure gloss belongs to the rendered table as
      # much as to the console: it names which measure each row
      # carries.
      notes = .categorical_note(missing_note, assoc_note_text)
    )
    tt <- tinytable::group_tt(tt, j = gspec)
    tt <- .spicy_tt_bare(tt)

    # Alignment. Honour the `align` argument: "decimal" centres
    # uniform-width pre-padded strings (same strategy as
    # table_regression() / table_continuous_lm()); "center" / "right"
    # apply literal alignment.
    tt <- tinytable::style_tt(tt, j = 1, align = "l")
    data_j <- 2:(1 + 2 * length(group_levels))
    stat_j <- (ncol(dat_tt) - n_stat_cols_merged + 1L):ncol(dat_tt)
    tt_align <- switch(
      align,
      decimal = "c",
      center = "c",
      right = "r",
      "r"
    )
    tt <- tinytable::style_tt(tt, j = c(data_j, stat_j), align = tt_align)
    # Centre n/% labels (row 0 = column labels row)
    tt <- tinytable::style_tt(tt, i = 0, j = data_j, align = "c")
    # Centre spanner labels (row -1 = spanner row)
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
    tt <- tinytable::style_tt(
      tt,
      j = stat_j,
      html_css = "white-space: nowrap;"
    )

    return(tt)
  }

  # ---------------- gt ----------------
  if (output == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      spicy_abort("Install package 'gt'.", class = "spicy_missing_pkg")
    }

    dat_gt <- merge_ci_inline(report_wide_char)
    # Pre-pad numeric cells so centring stacks the decimal points
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
    p_col_pos <- ncol(dat_gt) - n_stat_cols_merged + 1L
    col_ids[p_col_pos] <- .CAT_KEY_P
    if (show_assoc) {
      col_ids[ncol(dat_gt) - as.integer(do_smd)] <- "assoc_col"
    }
    if (do_smd) {
      col_ids[ncol(dat_gt)] <- "smd_col"
    }
    names(dat_gt) <- col_ids

    tbl <- gt::gt(dat_gt)

    # Column labels: n / % under each group; empty for single-col spanners
    label_list <- list()
    label_list[[.CAT_KEY_VARIABLE]] <- ""
    for (gi in seq_along(group_levels)) {
      label_list[[paste0(group_levels[gi], "_n")]] <- spicy_str(
        "header_n_lower"
      )
      label_list[[paste0(group_levels[gi], "_pct")]] <- spicy_str(
        "header_percent_symbol"
      )
    }
    label_list[[.CAT_KEY_P]] <- ""
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
        columns = c(
          paste0(group_levels[gi], "_n"),
          paste0(group_levels[gi], "_pct")
        )
      )
    }
    tbl <- gt::tab_spanner(
      tbl,
      label = spicy_str("header_p"),
      columns = .CAT_KEY_P,
      id = "spn_p"
    )
    if (show_assoc) {
      tbl <- gt::tab_spanner(
        tbl,
        label = measure_label,
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
    # columns honour the `align` argument: "decimal" centres
    # uniform-width pre-padded strings (same strategy as
    # table_regression() / table_continuous_lm()); "center" / "right"
    # use gt::cols_align() literally.
    tbl <- gt::cols_align(tbl, align = "left", columns = .CAT_KEY_VARIABLE)
    grp_cols <- unlist(lapply(group_levels, function(g) {
      c(paste0(g, "_n"), paste0(g, "_pct"))
    }))
    right_cols <- .CAT_KEY_P
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
    #    wider border wins regardless of colour.
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
    # the `by` level labels, i.e. user data, so they are escaped for the
    # CSS string they land in (`.css_escape_string()`); the CI selectors
    # of the two other descriptive families interpolate frozen keys.
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
    tbl <- .spicy_gt_apa_title(tbl, .categorical_title(by_name))

    return(.spicy_gt_attach_note(tbl, missing_note))
  }

  # ---------------- flextable / word ----------------
  build_flextable <- function(df) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      spicy_abort("Install package 'flextable'.", class = "spicy_missing_pkg")
    }
    df <- pad_decimal_cols(df)
    # Level rows carry the console's indent inside the label cell and
    # the engine indents them again below (`padding.left`). One
    # indentation is the design; two is an artefact of reading a
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
    # Numeric column alignment honours `align`. For "decimal", cells
    # were pre-padded above by `pad_decimal_cols()`; CENTRE the
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
    # Centre n/% labels and spanner labels in header
    ft <- flextable::align(ft, j = grp_j, part = "header", align = "center")
    # Right-align p and association measure in header
    stat_j <- (ncol(df) - n_stat_cols_merged + 1L):ncol(df)
    ft <- flextable::align(ft, j = stat_j, part = "header", align = "right")

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
    # Same pair, same order as the console printer.
    ft <- .spicy_ft_attach_note(
      ft,
      .categorical_note(missing_note, assoc_note_text)
    )
    class(ft) <- c("spicy_flextable", class(ft))
    ft
  }

  if (output == "flextable") {
    # Same title the console prints, from the same helper -- for a
    # by-table it names the grouping variable, which nothing else in
    # the rendered table states.
    return(.spicy_ft_html_caption(
      build_flextable(merge_ci_inline(report_wide_char)),
      .categorical_title(by_name)
    ))
  }

  if (output == "word") {
    if (is.null(word_path) || !nzchar(word_path)) {
      spicy_abort(
        "Provide `word_path` for output = 'word'.",
        class = "spicy_invalid_input"
      )
    }
    ft <- build_flextable(merge_ci_inline(report_wide_char))
    ft <- .spicy_ft_word_caption(ft, .categorical_title(by_name))
    flextable::save_as_docx(ft, path = word_path)
    return(invisible(word_path))
  }

  # Extend headers with CI columns for data/export formats
  if (assoc_ci) {
    ci_headers <- c(spicy_str("header_ci_lower"), spicy_str("header_ci_upper"))
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
    if (is.null(excel_path) || !nzchar(excel_path)) {
      spicy_abort(
        "Provide `excel_path` for output = 'excel'.",
        class = "spicy_invalid_input"
      )
    }
    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      spicy_abort("Install package 'openxlsx2'.", class = "spicy_missing_pkg")
    }

    wb <- openxlsx2::wb_workbook()
    wb <- openxlsx2::wb_add_worksheet(wb, excel_sheet)

    # Same title the console prints (it names the grouping variable,
    # which nothing else in the sheet states), then the two header
    # rows two lines below.
    wb <- openxlsx2::wb_add_data(
      wb,
      x = .categorical_title(by_name),
      start_row = 1
    )
    top_header_row <- 3L
    bot_header_row <- top_header_row + 1L
    first_body_row <- bot_header_row + 1L

    wb <- openxlsx2::wb_add_data(
      wb,
      x = as.data.frame(t(top_header_flat_ex), stringsAsFactors = FALSE),
      start_row = top_header_row,
      col_names = FALSE
    )
    wb <- openxlsx2::wb_add_data(
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
    body_xl$p <- report_wide_char$p
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

    # `na.strings = ""` so the empty cells of a variable-header row
    # stay blank. Without it openxlsx2 writes Excel ERROR cells
    # ("#N/A") in the middle of the counts, and any SUM over the
    # column inherits the error.
    wb <- openxlsx2::wb_add_data(
      wb,
      x = body_xl,
      start_row = first_body_row,
      col_names = FALSE,
      row_names = FALSE,
      na.strings = ""
    )

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
      # numeric columns honour `align`. For "decimal", Excel already
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
      # is that count, computed once beside `report_cols`.
      text_cols <- (nc - n_stat_cols + 1L):nc
      wb <- openxlsx2::wb_add_numfmt(
        wb,
        dims = openxlsx2::wb_dims(
          rows = first_body_row:last_row,
          cols = text_cols
        ),
        numfmt = "@"
      )
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

    # Disclosure notes below the table: what left it (drop_na), then
    # the association-measure gloss -- the same text, in the same
    # order, as the console footer.
    wb <- .spicy_xl_add_note(
      wb,
      note = .categorical_note(missing_note, assoc_note_text),
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
      title = .categorical_title(by_name),
      note = .categorical_note(missing_note, assoc_note_text)
    )
    clipr::write_clip(txt)
    spicy_inform("Categorical table copied to clipboard.")
    return(invisible(txt))
  }
}
