# Structured (typed) view of the DESCRIPTIVE tables.
#
# Wave 2 of the v3 contract: `table_categorical()`,
# `table_continuous()` and `table_continuous_lm()` attach the SAME
# structured view `table_regression()` attaches, and so do
# `table_outcome()`, `table_multiresponse()` and the survey twins, so
# `as_structured()` reads every descriptive table with one schema (see
# the schema comment at the top of R/regression_structured.R -- it is
# normative for this file too).
#
# What is family-specific:
#
#   * the `token` vocabulary of `col_meta` (each family names its own
#     statistics: "n" / "pct" / "assoc" for the categorical table,
#     the `show_columns` tokens for the continuous one, the
#     marginal-mean / contrast tokens for the linear-model one);
#   * the row roles a family emits (`factor_header` + `level` for the
#     categorical table, `summary` / `group` for the continuous ones,
#     `missing` wherever a row is keyed by the missing value, `none` for
#     the respondents of a multiple-response table who chose no item).
#
# What is NOT family-specific: everything else. A consumer written
# against a regression view reads a descriptive one unchanged --
# `body` is numeric with the four identity columns appended,
# composite cells travel in `col_meta$display_cells`, cell semantics
# in `cell_status`, and `version` is the same integer.
#
# The typed body is built from the COMPUTE frames (the long frame of
# each family), never re-parsed from the displayed strings; the
# display strings are read from the very `display_df` the print
# method renders, so a composite cell and its console rendering can
# never drift apart.
#
# Two notes on geometry, both consequences of the schema rather than
# choices:
#
#   * `body` carries ONE label column (`Variable`). The second label
#     column of a grouped continuous table (`Group`) is not a cell of
#     the table: it names the row, and that name lives in `.level`
#     (with `.row_role == "group"`), exactly as a factor level of a
#     regression table lives in `.level`.
#   * `stars` is always `NULL`. Descriptive tables carry no
#     significance markers.

# The value columns of a structured body, as an all-NA numeric row.
#
# `Variable` is prefixed OUTSIDE `col_names` on purpose: by the v3
# contract `col_meta` indexes the VALUE columns, so the identity column
# has no entry there and no `display_label` (see `.desc_assemble()`).
# Its header is `header_variable` in every family, resolved by each
# renderer -- an asymmetry of the contract, not an omission.
.desc_empty_row <- function(col_names) {
  as.data.frame(
    c(
      list(Variable = NA_character_),
      stats::setNames(rep(list(NA_real_), length(col_names)), col_names)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# Assemble a descriptive structured view from a list of row specs.
#
# Each row spec is a list with:
#   label     chr    the Variable cell, exactly as the console prints it
#   values    list   named by structured column, numeric scalars
#   variable  chr    `.variable` (the SOURCE variable name)
#   level     chr    `.level` (NA outside a keyed row)
#   role      chr    `.row_role`
#   indent    int    `.indent`
#   display   chr    named per-column display overrides (optional)
#   status    chr    named per-column cell statuses (optional)
#
# The one place a descriptive row enters a body, so no row can reach
# the contract without saying what it is -- the same discipline
# `push_row()` enforces on the regression side.
.desc_assemble <- function(
  rows,
  col_names,
  col_meta,
  format_spec,
  spanners = NULL,
  ci_pairs = list()
) {
  # Decision 13 (i18n stage 1.5, lot 0): every descriptive col_meta
  # entry carries a `display_label` -- the header a rendering engine
  # prints -- distinct in ROLE from the entry's name, which is the
  # frozen programmatic key (`body` column, `col_meta` index). The
  # two are identical strings until the display decoupling fills the
  # field; the regression family has carried the same slot since the
  # v2 contract (regression_structured.R).
  for (nm in names(col_meta)) {
    if (is.null(col_meta[[nm]]$display_label)) {
      col_meta[[nm]]$display_label <- nm
    }
  }
  empty <- .desc_empty_row(col_names)
  n_rows <- length(rows)

  if (n_rows == 0L) {
    body <- empty[0L, , drop = FALSE]
  } else {
    body <- do.call(
      rbind,
      lapply(rows, function(r) {
        row <- empty
        row$Variable <- as.character(r$label)
        for (nm in names(r$values)) {
          v <- r$values[[nm]]
          if (length(v) == 1L) {
            row[[nm]] <- as.numeric(v)
          }
        }
        row
      })
    )
  }
  rownames(body) <- NULL

  chr_of <- function(field) {
    if (n_rows == 0L) {
      return(character(0))
    }
    vapply(rows, function(r) as.character(r[[field]] %||% NA_character_), "")
  }
  body[[".variable"]] <- chr_of("variable")
  body[[".level"]] <- chr_of("level")
  body[[".row_role"]] <- chr_of("role")
  body[[".indent"]] <- if (n_rows == 0L) {
    integer(0)
  } else {
    vapply(rows, function(r) as.integer(r$indent %||% 0L), integer(1))
  }

  by_row <- function(field) {
    if (n_rows == 0L) {
      return(list())
    }
    stats::setNames(
      lapply(rows, function(r) r[[field]] %||% character(0)),
      as.character(seq_len(n_rows))
    )
  }
  display_cols <- .collect_cell_overrides(
    by_row("display"),
    col_names,
    n_rows,
    empty = NA_character_
  )
  for (col_name in names(display_cols)) {
    col_meta[[col_name]][["display_cells"]] <- display_cols[[col_name]]
  }
  cell_status <- .collect_cell_overrides(
    by_row("status"),
    col_names,
    n_rows,
    empty = ""
  )

  structured <- list(
    version = .spicy_structured_version(),
    body = body,
    stars = NULL,
    cell_status = cell_status,
    outcome_labels_by_col = character(0),
    col_meta = col_meta,
    spanners = spanners,
    ci_pairs = ci_pairs,
    format_spec = format_spec
  )
  .validate_structured(structured)
  structured
}

# Column indices of `cols` in a structured body, offset by the
# `Variable` label column -- the convention `spanners` / `ci_pairs`
# already use on the regression side.
.desc_col_index <- function(col_names, cols) {
  match(cols, col_names) + 1L
}

# Rows that OPEN a block: every factor header that is not the FIRST
# row of the body. The console rules each of them off from the block
# above, and the six engines draw the same light rule.
#
# Positional on row 1, never on the RANK of the first header. The two
# agree on a categorical body, whose first row is always a
# `factor_header` -- `.build_categorical_structured()` pushes the
# header before the level loop, for the first variable like the rest.
# They part company on a body that opens on something else: a
# `summary` row above the blocks. Reading the rank there drops the
# rule between that row and the first block -- the one rule such a
# table most needs.
.struct_block_sep_rows <- function(structured) {
  setdiff(which(structured$body$.row_role == "factor_header"), 1L)
}

# Rows that OPEN a block in a family whose blocks carry NO header row:
# the first row of each run of the block KEY, the first row of the body
# excepted.
#
# The sibling of `.struct_block_sep_rows()`, for the other block shape.
# A categorical block opens on a `factor_header` row and the rule is
# drawn above it; a continuous block is a run of `group` rows sharing
# one variable, with no header to key on. Both read a typed field of
# the contract. Neither reads a display string -- which is the whole
# point: the derivation this one replaced read the label column and was
# right only because `build_display_df()` blanks the label of every
# continuation row. That invariant lives in another function, under
# another condition (`has_group`), and nothing tied the two together;
# a family that laid its labels out differently -- repeating them
# rather than blanking them, or giving two variables the same one --
# got its rules from a string that was never the block key.
#
# Takes the KEY VECTOR rather than a structured object because the
# engine path of `table_continuous()` renders without building a
# structured view. The key it passes is `result$variable`, the very
# column `.build_continuous_structured()` writes into `.variable`, so
# the console, the engines and the typed view are reading one field.
#
# NA is a VALUE here, not a missing answer: two adjacent NA keys stay
# in one block and an NA next to a name opens a new one. A bare `!=`
# would return NA on those pairs and `which()` would drop the row,
# losing the rule in silence.
.struct_run_sep_rows <- function(key) {
  n <- length(key)
  if (n < 2L) {
    return(integer(0))
  }
  prev <- key[-n]
  cur <- key[-1L]
  changed <- (is.na(prev) != is.na(cur)) |
    (!is.na(prev) & !is.na(cur) & prev != cur)
  which(c(FALSE, changed))
}

# Rows to indent: the literal reading of the field the v3 contract
# declares for exactly this.
#
# Not the complement of a role. `which(.row_role != "factor_header")`
# means "a level" only while a body has two roles; a third role at
# indent 0 -- again, a `summary` row above the blocks -- falls into
# the complement and gets indented like a level of the block below it.
# On Excel and the clipboard the damage is worse than cosmetic:
# `make_stronger_indent()` strips `nchar(indent_text)` leading
# characters from every row it is handed, so a label that never had
# the prefix loses its own first letters.
.struct_indent_rows <- function(structured) {
  which(structured$body$.indent > 0L)
}


# ---- table_categorical() --------------------------------------------------

# Structured view of a categorical table.
#
# Row geometry is the console's: one `factor_header` row per variable,
# then one row per displayed level -- `level`, or `missing` when the
# level is the missing-value display category -- and, under
# `total_row = TRUE`, the `total` row that closes the block (flagged
# `.total_row` in the compute frame, indented like a level). The role is the KEY,
# never the label: `missing_vars` names the variables whose row of
# missing values the table added, so a translated `(Missing)` still
# reads as `missing`, and a level with that label in another variable
# (one without missing values) as `level`.
#
# Columns: `n` / `pct` per group (one spanner per group level in a
# `by` table, the margin flagged `total = TRUE` in its `col_meta`),
# then the test `p` (absent under `show_p = FALSE`), the association
# measure, and its CI bounds. Every column NAME is a frozen key; the
# header an engine prints is its `display_label`, resolved from the
# registry here and nowhere else.
.build_categorical_structured <- function(
  long_raw,
  select_names,
  labels,
  levels_keep,
  missing_label,
  indent_text,
  percent_digits,
  p_digits,
  effect_size_digits,
  decimal_mark,
  group_levels = NULL,
  margin_key = NULL,
  missing_vars = character(0),
  blank_pct_vars = character(0),
  measure_col = NULL,
  measure_label = NULL,
  show_p = TRUE,
  show_assoc = FALSE,
  assoc_ci = FALSE,
  assoc_ci_level = 0.95,
  show_smd = FALSE,
  assoc_none = character(0)
) {
  has_group <- !is.null(group_levels)

  # ---- columns, in the display order of `report_cols` -------------------
  col_names <- character(0)
  col_meta <- list()
  spanners <- NULL
  ci_pairs <- list()

  num_meta <- function(token, precision, ...) {
    c(
      list(token = token, precision = as.integer(precision), p_style = NULL),
      list(...)
    )
  }

  if (has_group) {
    spanners <- list()
    for (g in group_levels) {
      n_col <- .cat_key_n(g)
      p_col <- .cat_key_pct(g)
      # The margin is the one group whose header is a word of ours; every
      # user level is data and travels into the header verbatim.
      g_label <- .cat_group_label(g, margin_key)
      col_names <- c(col_names, n_col, p_col)
      # Counts display as integers, weighted counts included (the SPSS
      # Crosstabs convention `fmt_n()` implements).
      col_meta[[n_col]] <- num_meta(
        "n",
        0L,
        group = g,
        display_label = .cat_label_n(g_label)
      )
      col_meta[[p_col]] <- num_meta(
        "pct",
        percent_digits,
        group = g,
        display_label = .cat_label_pct(g_label)
      )
      if (identical(g, margin_key)) {
        # The margin, told apart from a user group by a FLAG rather
        # than by its label: "Total" is a display string (and its key
        # can be "Total_1", see `.cat_margin_key()`), the flag is the
        # key.
        col_meta[[n_col]]$total <- TRUE
        col_meta[[p_col]]$total <- TRUE
      }
      spanners[[g]] <- .desc_col_index(col_names, c(n_col, p_col))
    }
    if (show_p) {
      col_names <- c(col_names, .CAT_KEY_P)
      col_meta[[.CAT_KEY_P]] <- list(
        token = "p",
        precision = as.integer(p_digits),
        p_style = .style_p_style_token(decimal_mark),
        # The style's floor, like the `p_threshold` of the format spec
        # below: a flat `10^(-p_digits)` disagreed with it -- and with the
        # console -- as soon as a theme set its floor apart from its
        # decimals (JAMA: two decimals, floor at .001).
        threshold = .style_p_floor(p_digits),
        display_label = spicy_str("header_p")
      )
    }
    if (show_assoc) {
      col_names <- c(col_names, measure_col)
      # Ordinal measures (tau-b, tau-c, gamma, Somers' D) are signed:
      # the validated range is [-1, 1], not the [0, 1] of a p-value,
      # although the APA leading-zero strip applies to both.
      col_meta[[measure_col]] <- list(
        token = "assoc",
        precision = as.integer(effect_size_digits),
        p_style = .style_p_style_token(decimal_mark),
        value_range = c(-1, 1),
        display_label = measure_label %||% measure_col
      )
      if (assoc_ci) {
        ll <- .CAT_KEY_CI_LL
        ul <- .CAT_KEY_CI_UL
        col_names <- c(col_names, ll, ul)
        # Display text: the coverage percentage follows `decimal_mark`
        # (decision 27). Today `assoc_ci_level` is pinned at 0.95
        # upstream, so the substitution is byte-inert -- until the level
        # is ever exposed, when this line is already right. Template and
        # interval word come from the registry, as in the other families.
        ci_label <- spicy_fmt(
          "header_ci_spanner",
          .ci_pct_display(assoc_ci_level, decimal_mark),
          spicy_str("header_ci_label_confidence")
        )
        for (nm in c(ll, ul)) {
          # A bound with no spanner above it names its own coverage
          # ("95% CI LL"), as the standalone bounds of the other tables do
          # (`.cat_svy_base_label()`, the mean interval of
          # `table_continuous()`): the level is `assoc_ci_level`, the one
          # the interval is computed at.
          col_meta[[nm]] <- list(
            token = "assoc_ci",
            precision = as.integer(effect_size_digits),
            p_style = .style_p_style_token(decimal_mark),
            value_range = c(-1, 1),
            ci_role = if (identical(nm, ll)) "LL" else "UL",
            ci_pair = if (identical(nm, ll)) ul else ll,
            ci_label = ci_label,
            display_label = spicy_fmt(
              "header_ci_bound",
              ci_label,
              spicy_str(
                if (identical(nm, ll)) "header_ci_ll" else "header_ci_ul"
              )
            )
          )
        }
        ci_pairs <- list(list(
          label = ci_label,
          cols = .desc_col_index(col_names, c(ll, ul))
        ))
      }
    }
    if (show_smd) {
      col_names <- c(col_names, .CAT_KEY_SMD)
      # Unlike the association column beside it, no `p_style` and no
      # `value_range`: the SMD of a variable with three or more
      # categories is a Mahalanobis distance and is not bounded by 1.
      # A visible consequence, and an accepted one: in the same table
      # "Effect size" prints .45 (APA leading zero stripped, bounded
      # measure) while "SMD" prints 0.45.
      col_meta[[.CAT_KEY_SMD]] <- list(
        token = "smd",
        precision = as.integer(effect_size_digits),
        display_label = spicy_str("header_smd")
      )
    }
  } else {
    col_names <- c("n", "%")
    col_meta[["n"]] <- num_meta(
      "n",
      0L,
      display_label = spicy_str("header_n_lower")
    )
    col_meta[["%"]] <- num_meta(
      "pct",
      percent_digits,
      display_label = spicy_str("header_percent_symbol")
    )
  }

  # ---- rows, in the display order of `make_report_wide()` ---------------
  # The count and percentage cells of one row, `sl` its rows of the
  # compute frame. A count with no number is a group in which the
  # variable has no data (`table_categorical()`): NA in the body, an
  # undefined cell, shown as `cell_undefined`. With `blank_pct`, the row
  # of missing values of a variable in `blank_pct_vars`, a percentage
  # beside a count is outside the base: NA in the body and a blank cell,
  # which does not apply.
  undefined <- spicy_str("cell_undefined")
  count_cells <- function(sl, blank_pct = FALSE) {
    values <- list()
    out_of_base <- character(0)
    if (has_group) {
      for (g in group_levels) {
        sx <- sl[sl$group == g, , drop = FALSE]
        values[[.cat_key_n(g)]] <- if (nrow(sx)) sx$n[1L] else NA_real_
        values[[.cat_key_pct(g)]] <- if (nrow(sx)) sx$pct[1L] else NA_real_
        if (blank_pct && !is.na(values[[.cat_key_n(g)]])) {
          out_of_base <- c(out_of_base, .cat_key_pct(g))
        }
      }
    } else {
      values[["n"]] <- sl$n[1L]
      values[["%"]] <- sl$pct[1L]
      if (blank_pct && !is.na(values[["n"]])) {
        out_of_base <- "%"
      }
    }
    void <- names(values)[vapply(values, is.na, logical(1))]
    void <- setdiff(void, out_of_base)
    list(
      values = values,
      status = stats::setNames(rep("undefined", length(void)), void),
      display = stats::setNames(rep(undefined, length(void)), void)
    )
  }
  rows <- list()
  if (nrow(long_raw) > 0L) {
    for (li in seq_along(labels)) {
      lab <- labels[[li]]
      var_name <- select_names[[li]]
      sv <- long_raw[long_raw$variable == var_name, , drop = FALSE]
      if (nrow(sv) == 0L) {
        next
      }
      # A variable with no data and no level to show is one row, which
      # carries the cells of the groups itself.
      no_level <- all(is.na(sv$level))
      # `levels_keep` is resolved to one entry per variable (NULL when it
      # filters none): `.cat_levels_keep_spec()`. The Total row of
      # `total_row = TRUE` closes the block whatever it keeps.
      keep_var <- levels_keep[[var_name]]
      lv_use <- if (no_level) {
        character(0)
      } else {
        .cat_block_levels(sv, keep_var)
      }
      total_lv <- unique(sv$level[sv$.total_row %in% TRUE])

      header_values <- list()
      header_status <- character(0)
      header_display <- character(0)
      if (no_level) {
        cells <- count_cells(sv)
        header_values <- cells$values
        header_status <- cells$status
        header_display <- cells$display
      }
      if (has_group) {
        # A statistic of the variable with no value -- a comparison that
        # cannot run (a group with no data, a constant variable), a test
        # or a measure that returns none -- is an undefined cell, as the
        # console marks it. A variable that asked for no measure
        # (`assoc_none`) leaves that cell blank: it does not apply.
        void_stats <- character(0)
        if (show_p) {
          header_values[[.CAT_KEY_P]] <- sv$p[1L]
          if (is.na(sv$p[1L])) {
            void_stats <- .CAT_KEY_P
          }
        }
        if (show_assoc) {
          header_values[[measure_col]] <- sv[[measure_col]][1L]
          if (assoc_ci) {
            header_values[[.CAT_KEY_CI_LL]] <- sv$ci_lower[1L]
            header_values[[.CAT_KEY_CI_UL]] <- sv$ci_upper[1L]
          }
          if (!var_name %in% assoc_none) {
            stat_cols <- c(
              measure_col,
              if (assoc_ci) c(.CAT_KEY_CI_LL, .CAT_KEY_CI_UL)
            )
            void_stats <- c(
              void_stats,
              stat_cols[vapply(header_values[stat_cols], is.na, logical(1))]
            )
          }
        }
        header_status[void_stats] <- "undefined"
        header_display[void_stats] <- undefined
        if (show_smd) {
          header_values[[.CAT_KEY_SMD]] <- sv$.smd[1L]
          # The console prints `cell_undefined` for an SMD that cannot be
          # estimated (a group with no data, two profiles with no category
          # in common): the undefined cell of the contract.
          if (is.na(sv$.smd[1L])) {
            header_status[[.CAT_KEY_SMD]] <- "undefined"
            header_display[[.CAT_KEY_SMD]] <- undefined
          }
        }
      }
      rows[[length(rows) + 1L]] <- list(
        label = lab,
        values = header_values,
        variable = var_name,
        level = NA_character_,
        role = "factor_header",
        indent = 0L,
        display = header_display,
        status = header_status
      )

      for (lv in lv_use) {
        out_of_base <- var_name %in%
          blank_pct_vars &&
          identical(lv, missing_label)
        cells <- count_cells(
          sv[!is.na(sv$level) & sv$level == lv, , drop = FALSE],
          blank_pct = out_of_base
        )
        rows[[length(rows) + 1L]] <- list(
          label = paste0(indent_text, lv),
          values = cells$values,
          variable = var_name,
          level = lv,
          role = if (lv %in% total_lv) {
            "total"
          } else if (
            var_name %in% missing_vars && identical(lv, missing_label)
          ) {
            "missing"
          } else {
            "level"
          },
          indent = 1L,
          display = cells$display,
          status = cells$status
        )
      }
    }
  }

  .desc_assemble(
    rows,
    col_names = col_names,
    col_meta = col_meta,
    format_spec = list(
      decimal_mark = decimal_mark,
      digits = as.integer(percent_digits),
      percent_digits = as.integer(percent_digits),
      p_digits = as.integer(p_digits),
      effect_size_digits = as.integer(effect_size_digits),
      p_style = .style_p_style_token(decimal_mark),
      p_threshold = .style_p_floor(p_digits),
      ci_level = assoc_ci_level
    ),
    spanners = spanners,
    ci_pairs = ci_pairs
  )
}


# ---- table_continuous() ---------------------------------------------------

# The column vocabulary (`.continuous_token_columns()`) lives in
# R/table_continuous.R, beside the token ORDER it has to agree with.
# `DESCRIPTION` has no `Collate` field, so files are sourced
# alphabetically and this one is read AFTER it -- harmless, because the
# vocabulary is a function, resolved when called rather than at build.

# The COLUMN half of a continuous structured view: the column names,
# their `col_meta`, and the per-column source map the row half reads.
#
# Shared by `table_continuous()` and `table_outcome()`, which display
# the same vocabulary of tokens and differ only in the geometry of
# their rows. A second copy would be a second place for a token to
# gain a precision, a `ci_pair` or a `display_label` -- and the typed
# contract is exactly what a consumer indexes into by those names.
#
# `display_df` is read for MEMBERSHIP only (which inference columns
# the table carries), key against key.
.continuous_struct_columns <- function(
  display_df,
  tokens_union,
  digits,
  effect_size_digits,
  p_digits,
  decimal_mark,
  ci_level,
  es_ci = FALSE
) {
  spec <- .continuous_token_columns(ci_level, decimal_mark)

  col_names <- character(0)
  col_meta <- list()
  ci_pairs <- list()
  # Per column: the field of `result` it reads, and whether its cell
  # is composite (display override) / integer-valued.
  col_source <- list()

  for (tok in tokens_union) {
    entries <- spec[[tok]]
    for (e in entries) {
      col_names <- c(col_names, e$name)
      col_meta[[e$name]] <- list(
        token = tok,
        # The entry NAME is the frozen key a consumer indexes into; the
        # `display_label` is the header a renderer prints. `ci_label` is
        # text too -- `inline()` writes it into a sentence -- so it
        # follows the registry while the neighboring column name does
        # not: an assumed asymmetry of the typed contract.
        display_label = e$label,
        precision = if (isTRUE(e$integer)) 0L else as.integer(digits),
        p_style = NULL,
        ci_role = e$ci_role,
        ci_label = e$ci_label
      )
      col_source[[e$name]] <- e
    }
    if (tok %in% c("ci", "med_ci")) {
      ll <- entries[[1L]]$name
      ul <- entries[[2L]]$name
      col_meta[[ll]]$ci_pair <- ul
      col_meta[[ul]]$ci_pair <- ll
      ci_pairs[[length(ci_pairs) + 1L]] <- list(
        label = col_meta[[ll]]$ci_label,
        cols = .desc_col_index(col_names, c(ll, ul))
      )
    }
  }

  # Inference columns: present only when the display carries them.
  # Membership tests on the display frame, KEY against key -- the names
  # come from the same constants `build_display_df()` writes.
  if (.CON_KEY_TEST %in% names(display_df)) {
    col_names <- c(col_names, .CON_KEY_TEST)
    # The console prints the statistic inside its own gloss
    # ("t(1196.18) = 0.28"): the body keeps the statistic, the string
    # travels as a display override.
    col_meta[[.CON_KEY_TEST]] <- list(
      token = "statistic",
      display_label = spicy_str("header_test"),
      precision = 2L
    )
    col_source[[.CON_KEY_TEST]] <- list(
      name = .CON_KEY_TEST,
      field = "statistic",
      composite = TRUE
    )
  }
  if (.CON_KEY_P %in% names(display_df)) {
    col_names <- c(col_names, .CON_KEY_P)
    col_meta[[.CON_KEY_P]] <- list(
      token = "p",
      display_label = spicy_str("header_p"),
      precision = as.integer(p_digits),
      p_style = .style_p_style_token(decimal_mark),
      threshold = .style_p_floor(p_digits)
    )
    col_source[[.CON_KEY_P]] <- list(name = .CON_KEY_P, field = "p.value")
  }
  if (.CON_KEY_ES %in% names(display_df)) {
    col_names <- c(col_names, .CON_KEY_ES)
    col_meta[[.CON_KEY_ES]] <- list(
      token = "es",
      # The console's header, the level of the interval included when the
      # cell prints one (`.continuous_label_map()`).
      display_label = .continuous_labels(
        .CON_KEY_ES,
        ci_level,
        decimal_mark,
        es_ci
      ),
      precision = as.integer(effect_size_digits)
    )
    col_source[[.CON_KEY_ES]] <- list(
      name = .CON_KEY_ES,
      field = "es_value",
      composite = TRUE
    )
  }
  if (.CON_KEY_SMD %in% names(display_df)) {
    col_names <- c(col_names, .CON_KEY_SMD)
    # No `p_style` and no `value_range`: the SMD of a k-level variable
    # is a Mahalanobis distance and is NOT bounded by 1 (1.11 and 2.45
    # on the pinned fixtures), so the APA leading-zero strip and the
    # [-1, 1] check the association column carries would both be wrong
    # here. `.validate_structured()` only range-checks when `p_style`
    # is non-NULL, so leaving it NULL settles both at once.
    col_meta[[.CON_KEY_SMD]] <- list(
      token = "smd",
      display_label = spicy_str("header_smd"),
      precision = as.integer(effect_size_digits)
    )
    # NOT composite: the cell is the bare number, so the typed body and
    # the printed cell are the same value and `inline()` can cite it.
    col_source[[.CON_KEY_SMD]] <- list(
      name = .CON_KEY_SMD,
      field = "smd_value"
    )
  }

  list(
    spec = spec,
    col_names = col_names,
    col_meta = col_meta,
    ci_pairs = ci_pairs,
    col_source = col_source
  )
}

# Structured view of a continuous summary table.
#
# One row per displayed row of the console table: a `summary` row per
# variable without `by`, one `group` row per level of `by` with it --
# `missing` for the missing-`by` group, whose label is a display
# string and whose role is the key.
#
# `display_df` is the very frame the print method renders: the
# composite cells ("Med [Q1, Q3]", the test statistic, the effect
# size) are read from it, so the typed view and the console can never
# word the same cell differently.
#
# Under a named `digits`, `digits_by_var` gives each variable its
# decimals and `col_meta$precision` stays the table's `digits`: a cell on
# the scale of a variable whose decimals differ carries its printed
# string as a display override, which every renderer prefers, so
# `inline()` cites the number the table prints.
.build_continuous_structured <- function(
  result,
  display_df,
  tokens_union,
  tokens_by_var,
  digits,
  effect_size_digits,
  p_digits,
  decimal_mark,
  ci_level,
  missing_group_label = NA_character_,
  es_ci = FALSE,
  digits_by_var = NULL
) {
  has_group <- "group" %in% names(result)
  # The column half is shared with `table_outcome()`.
  cols <- .continuous_struct_columns(
    display_df = display_df,
    tokens_union = tokens_union,
    digits = digits,
    effect_size_digits = effect_size_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    ci_level = ci_level,
    es_ci = es_ci
  )
  spec <- cols$spec
  col_names <- cols$col_names
  col_meta <- cols$col_meta
  ci_pairs <- cols$ci_pairs
  col_source <- cols$col_source

  rows <- list()
  for (i in seq_len(nrow(result))) {
    var_name <- result$variable[i]
    shown <- tokens_by_var[[var_name]] %||% tokens_union
    own_digits <- !is.null(digits_by_var) &&
      digits_by_var[[var_name]] != digits
    values <- list()
    display <- character(0)
    status <- character(0)
    for (nm in col_names) {
      src <- col_source[[nm]]
      tok <- col_meta[[nm]]$token
      # A statistic the variable does not display is a STRUCTURAL
      # blank, not an undefined cell: the column belongs to another
      # variable of the table (per-variable `show_columns`).
      if (tok %in% names(spec) && !(tok %in% shown)) {
        next
      }
      val <- result[[src$field]][i]
      if (!is.null(val) && length(val) == 1L) {
        values[[nm]] <- as.numeric(val)
      }
      shown_str <- as.character(display_df[[nm]][i])
      on_scale <- own_digits &&
        tok %in% names(spec) &&
        !isTRUE(src$integer) &&
        !isTRUE(src$table_digits)
      if (isTRUE(src$composite) || on_scale) {
        if (nzchar(shown_str)) {
          display[[nm]] <- shown_str
        }
      }
      # The console writes `cell_undefined` where a statistic applies to
      # the row but no number expresses it (a standard deviation on
      # n = 1, an interval on an empty group). That is the `undefined`
      # cell of the contract, and the override keeps the console's own
      # glyph.
      #
      # Recognized through the REGISTRY, never through a literal. Both
      # sides of this comparison are written by `build_display_df()`
      # from `spicy_str("cell_undefined")`; a literal here would hold
      # only while the registry value happened to match it, and the day
      # it moved it would silently drop `cell_status` for every consumer
      # of `as_structured()` and the display override with it.
      #
      # The glyph is now one key for the whole package (decision 23), so
      # this override no longer has to reconcile two of them: the shared
      # renderer `.cell_to_string()` prints the same `cell_undefined`
      # mark this branch recognizes.
      if (identical(shown_str, spicy_str("cell_undefined"))) {
        status[[nm]] <- "undefined"
        display[[nm]] <- shown_str
      }
    }
    rows[[length(rows) + 1L]] <- list(
      label = display_df[[.CON_KEY_VARIABLE]][i],
      values = values,
      variable = var_name,
      level = if (has_group) result$group[i] else NA_character_,
      role = if (!has_group) {
        "summary"
      } else if (
        !is.na(missing_group_label) &&
          identical(result$group[i], missing_group_label)
      ) {
        "missing"
      } else {
        "group"
      },
      indent = 0L,
      display = display,
      status = status
    )
  }

  .desc_assemble(
    rows,
    col_names = col_names,
    col_meta = col_meta,
    format_spec = list(
      decimal_mark = decimal_mark,
      digits = as.integer(digits),
      p_digits = as.integer(p_digits),
      effect_size_digits = as.integer(effect_size_digits),
      p_style = .style_p_style_token(decimal_mark),
      p_threshold = .style_p_floor(p_digits),
      ci_level = ci_level
    ),
    ci_pairs = ci_pairs
  )
}


# ---- table_continuous_lm() ------------------------------------------------

# Structured view of a bivariate linear-model table.
#
# One `summary` row per outcome: the console lays this family out one
# row per variable, with the `by` levels as COLUMNS (keyed `M (<level>)`,
# headed by the level under the mean's spanner), so the level of a
# marginal-mean column travels in that column's `col_meta$level`, its
# `display_label`, and the groups span nothing: `spanners` stays empty,
# the mean's spanner being a statistic's, not a group's, and `inline()`
# reads `spanners` as the groups `model` selects.
#
# `wide_raw` and `wide_display` are the two frames the function has
# already built for the raw and the console outputs, so the typed
# body and the printed body come from one computation each.
#
# Under a named `digits`, the means, the contrast or slope and its
# interval of an outcome whose decimals differ from the table's carry
# their printed string, as in `.build_continuous_structured()`.
.build_continuous_lm_structured <- function(
  result,
  wide_raw,
  wide_display,
  digits,
  fit_digits,
  effect_size_digits,
  p_digits,
  decimal_mark,
  ci_level,
  show_statistic,
  effect_size,
  effect_size_ci,
  r2_type,
  spec,
  digits_by_var = NULL
) {
  vars <- unique(result$variable)
  ci_pct <- .lm_ci_pct(ci_level)
  ci_ll_name <- .lm_key_ci_ll(ci_pct)
  ci_ul_name <- .lm_key_ci_ul(ci_pct)

  # Displayed columns, in the order build_wide_display_df_continuous_lm()
  # emits them. Keyed on the display frame so the two can never differ
  # in width or order.
  col_names <- setdiff(names(wide_display), .LM_KEY_VARIABLE)
  col_meta <- list()
  ci_pairs <- list()
  composite <- character(0)

  # The same spec both frames were built from, indexed by frozen key.
  # The five column names this function used to re-derive (the marginal
  # means, the delta, the test, the fit statistic, the effect size) now
  # come from it.
  spec_by_key <- stats::setNames(spec, .lm_spec_keys(spec))

  for (nm in col_names) {
    ent <- spec_by_key[[nm]]
    if (is.null(ent)) {
      # The display builder's column set is closed; a column added
      # there without an entry in the spec must FAIL here, not be
      # silently mislabeled with someone else's token. The lookup is
      # KEY against key, so this abort never sees a translated header
      # pass by -- it is the safety net of that very rule.
      spicy_abort(
        sprintf(
          "Internal: unrecognized continuous-lm display column %s.",
          .quote_val(nm)
        ),
        class = "spicy_internal_invariant"
      )
    }
    meta <- switch(
      ent$token,
      # Estimated marginal mean of one `by` level: the level is DATA
      # on the column, so a consumer never parses "M (Male)" back.
      emmean = list(
        token = "emmean",
        precision = as.integer(digits),
        level = ent$level
      ),
      delta = list(token = "delta", precision = as.integer(digits)),
      ci = list(
        token = "ci",
        precision = as.integer(digits),
        ci_role = ent$ci_role,
        # A cross-reference to the other bound: a KEY, never a label.
        ci_pair = ent$ci_pair,
        ci_label = ent$ci_label
      ),
      b = list(token = "b", precision = as.integer(digits)),
      statistic = list(token = "statistic", precision = as.integer(digits)),
      p = list(
        token = "p",
        precision = as.integer(p_digits),
        p_style = .style_p_style_token(decimal_mark),
        threshold = .style_p_floor(p_digits)
      ),
      r2 = list(token = "r2", precision = as.integer(fit_digits)),
      es = {
        # With `effect_size_ci`, the console inlines the interval in
        # the same cell ("g = 0.14 [0.02, 0.25]"): a composite the body
        # cannot hold, so it travels as a display override.
        if (isTRUE(effect_size_ci)) {
          composite <- c(composite, nm)
        }
        list(
          token = "es",
          precision = as.integer(effect_size_digits),
          effect_size = effect_size
        )
      },
      n = list(token = "n", precision = 0L),
      # A sum of weights, not a count -- displayed at the table's
      # numeric precision, like the console does.
      weighted_n = list(token = "weighted_n", precision = as.integer(digits)),
      # Second half of the closed set: the spec knows this column but
      # this function has no branch for what it IS. Without the default
      # `switch()` would return NULL and the column would enter the
      # contract with no token at all -- the silent mislabeling the
      # abort above exists to prevent.
      spicy_abort(
        sprintf(
          "Internal: unrecognized continuous-lm column token %s.",
          .quote_val(ent$token)
        ),
        class = "spicy_internal_invariant"
      )
    )
    # The header a reader sees, from the same spec the engines read: the
    # typed view is one consumer of the label layer, never its source.
    meta$display_label <- ent$label
    col_meta[[nm]] <- meta
  }
  if (all(c(ci_ll_name, ci_ul_name) %in% col_names)) {
    ci_pairs <- list(list(
      label = spec_by_key[[ci_ll_name]]$ci_label,
      cols = .desc_col_index(col_names, c(ci_ll_name, ci_ul_name))
    ))
  }

  rows <- list()
  for (i in seq_along(vars)) {
    own_digits <- !is.null(digits_by_var) &&
      digits_by_var[[vars[i]]] != digits
    values <- list()
    display <- character(0)
    status <- character(0)
    for (nm in col_names) {
      val <- wide_raw[[nm]][i]
      if (!is.null(val) && length(val) == 1L) {
        values[[nm]] <- as.numeric(val)
      }
      shown_str <- as.character(wide_display[[nm]][i])
      on_scale <- own_digits &&
        col_meta[[nm]]$token %in% c("emmean", "delta", "ci", "b")
      if (nm %in% composite || on_scale) {
        if (nzchar(shown_str)) {
          display[[nm]] <- shown_str
        }
      }
      # The mean of a level in which the outcome has no data, and the
      # contrast that involves it: the console's undefined mark, the
      # `undefined` cell of the contract (recognized through the
      # registry, as in `.build_continuous_structured()`).
      if (identical(shown_str, spicy_str("cell_undefined"))) {
        status[[nm]] <- "undefined"
        display[[nm]] <- shown_str
      }
    }
    rows[[length(rows) + 1L]] <- list(
      label = wide_display[[.LM_KEY_VARIABLE]][i],
      values = values,
      variable = vars[i],
      level = NA_character_,
      role = "summary",
      indent = 0L,
      display = display,
      status = status
    )
  }

  .desc_assemble(
    rows,
    col_names = col_names,
    col_meta = col_meta,
    format_spec = list(
      decimal_mark = decimal_mark,
      digits = as.integer(digits),
      p_digits = as.integer(p_digits),
      effect_size_digits = as.integer(effect_size_digits),
      fit_digits = as.integer(fit_digits),
      p_style = .style_p_style_token(decimal_mark),
      p_threshold = .style_p_floor(p_digits),
      ci_level = ci_level
    ),
    ci_pairs = ci_pairs
  )
}


# ---- table_outcome() ------------------------------------------------------

# Structured view of an outcome table.
#
# Same COLUMNS as a continuous table -- `.continuous_struct_columns()`
# builds them, so the two families cannot name a token differently --
# and the block geometry of a categorical one: a `summary` row for the
# whole analytic sample, then per grouping a `factor_header` row
# carrying the block's own statistics and one `level` (or `missing`)
# row per displayed level.
#
# Two roles sit at `.indent == 0` here, the `summary` row and the
# `factor_header` rows, which is why the shared geometry predicates read
# `.indent` rather than the complement of a role.
#
# The structural blanks are ABSENCES, never `undefined` cells: a
# descriptive statistic on a block header and a block statistic on a
# level row do not apply to that row, they are not values the table
# failed to compute. The `undefined` status is reserved for what the
# console itself marks with the undefined glyph -- an SD on n = 1, an
# interval on an empty level.
.build_outcome_structured <- function(
  result,
  display_df,
  tokens,
  digits,
  effect_size_digits,
  p_digits,
  decimal_mark,
  ci_level,
  es_ci = FALSE
) {
  cols <- .continuous_struct_columns(
    display_df = display_df,
    tokens_union = tokens,
    digits = digits,
    effect_size_digits = effect_size_digits,
    p_digits = p_digits,
    decimal_mark = decimal_mark,
    ci_level = ci_level,
    es_ci = es_ci
  )
  spec <- cols$spec
  col_names <- cols$col_names
  col_meta <- cols$col_meta
  col_source <- cols$col_source

  # Which columns a row is entitled to: the block's own statistics
  # belong to its header row, the outcome's to every other row.
  block_tokens <- c("statistic", "p", "es", "smd")

  rows <- list()
  for (i in seq_len(nrow(result))) {
    role <- result$.row_role[i]
    is_header <- identical(role, "factor_header")
    values <- list()
    display <- character(0)
    status <- character(0)
    for (nm in col_names) {
      tok <- col_meta[[nm]]$token
      if (identical(tok %in% block_tokens, !is_header)) {
        next
      }
      src <- col_source[[nm]]
      val <- result[[src$field]][i]
      if (!is.null(val) && length(val) == 1L) {
        values[[nm]] <- as.numeric(val)
      }
      shown_str <- as.character(display_df[[nm]][i])
      if (isTRUE(src$composite) && nzchar(shown_str)) {
        display[[nm]] <- shown_str
      }
      if (identical(shown_str, spicy_str("cell_undefined"))) {
        status[[nm]] <- "undefined"
        display[[nm]] <- shown_str
      }
    }
    rows[[length(rows) + 1L]] <- list(
      label = display_df[[.CON_KEY_VARIABLE]][i],
      values = values,
      # The marginal row is the OUTCOME's row: `inline(tbl, <outcome>)`
      # cites the marginal mean, which is the most natural citation
      # this table offers.
      variable = result$variable[i],
      level = result$level[i],
      role = role,
      indent = if (role %in% c("level", "missing")) 1L else 0L,
      display = display,
      status = status
    )
  }

  .desc_assemble(
    rows,
    col_names = col_names,
    col_meta = col_meta,
    format_spec = list(
      decimal_mark = decimal_mark,
      digits = as.integer(digits),
      p_digits = as.integer(p_digits),
      effect_size_digits = as.integer(effect_size_digits),
      p_style = .style_p_style_token(decimal_mark),
      p_threshold = .style_p_floor(p_digits),
      ci_level = ci_level
    ),
    ci_pairs = cols$ci_pairs
  )
}
