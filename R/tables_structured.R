# Structured (typed) view of the DESCRIPTIVE tables.
#
# Wave 2 of the v3 contract: `table_categorical()`,
# `table_continuous()` and `table_continuous_lm()` attach the SAME
# structured view `table_regression()` attaches, so `as_structured()`
# reads the three descriptive families with one schema (see the
# schema comment at the top of R/regression_structured.R -- it is
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
#     `missing` wherever a row is keyed by the missing value).
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


# ---- table_categorical() --------------------------------------------------

# Structured view of a categorical table.
#
# Row geometry is the console's: one `factor_header` row per variable,
# then one row per displayed level -- `level`, or `missing` when the
# level is the missing-value display category. The role is the KEY,
# never the label: `(Missing)` renamed on collision (`(Missing_1)`,
# ...) or translated one day still reads as `missing`.
#
# Columns: `n` / `pct` per group (one spanner per group level in a
# `by` table, the margin flagged `total = TRUE` in its `col_meta`),
# then the test `p`, the association measure, and its CI bounds. Every
# column NAME is a frozen key; the header an engine prints is its
# `display_label`, resolved from the registry here and nowhere else.
.build_categorical_structured <- function(
  long_raw,
  select_names,
  labels,
  levels_keep,
  missing_label,
  indent_text,
  percent_digits,
  p_digits,
  v_digits,
  decimal_mark,
  group_levels = NULL,
  margin_key = NULL,
  measure_col = NULL,
  measure_label = NULL,
  show_assoc = FALSE,
  assoc_ci = FALSE,
  assoc_ci_level = 0.95
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
        # than by its label: "Total" is a display string (and is
        # auto-renamed on collision), the flag is the key.
        col_meta[[n_col]]$total <- TRUE
        col_meta[[p_col]]$total <- TRUE
      }
      spanners[[g]] <- .desc_col_index(col_names, c(n_col, p_col))
    }
    col_names <- c(col_names, .CAT_KEY_P)
    col_meta[[.CAT_KEY_P]] <- list(
      token = "p",
      precision = as.integer(p_digits),
      p_style = .style_p_style_token(),
      threshold = 10^(-p_digits),
      display_label = spicy_str("header_p")
    )
    if (show_assoc) {
      col_names <- c(col_names, measure_col)
      # Ordinal measures (tau-b, tau-c, gamma, Somers' D) are signed:
      # the validated range is [-1, 1], not the [0, 1] of a p-value,
      # although the APA leading-zero strip applies to both.
      col_meta[[measure_col]] <- list(
        token = "assoc",
        precision = as.integer(v_digits),
        p_style = .style_p_style_token(),
        value_range = c(-1, 1),
        measure = measure_col,
        display_label = measure_label %||% measure_col
      )
      if (assoc_ci) {
        ll <- .CAT_KEY_CI_LL
        ul <- .CAT_KEY_CI_UL
        col_names <- c(col_names, ll, ul)
        ci_label <- paste0(formatC(assoc_ci_level * 100, format = "g"), "% CI")
        for (nm in c(ll, ul)) {
          col_meta[[nm]] <- list(
            token = "assoc_ci",
            precision = as.integer(v_digits),
            p_style = .style_p_style_token(),
            value_range = c(-1, 1),
            ci_role = if (identical(nm, ll)) "LL" else "UL",
            ci_pair = if (identical(nm, ll)) ul else ll,
            ci_label = ci_label,
            display_label = if (identical(nm, ll)) {
              spicy_str("header_ci_lower")
            } else {
              spicy_str("header_ci_upper")
            }
          )
        }
        ci_pairs <- list(list(
          label = ci_label,
          cols = .desc_col_index(col_names, c(ll, ul))
        ))
      }
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
  rows <- list()
  if (nrow(long_raw) > 0L) {
    for (li in seq_along(labels)) {
      lab <- labels[[li]]
      sv <- long_raw[long_raw$variable == lab, , drop = FALSE]
      if (nrow(sv) == 0L) {
        next
      }
      var_name <- select_names[[li]]
      lv_use <- if (is.null(levels_keep)) {
        unique(sv$level)
      } else {
        intersect(as.character(levels_keep), unique(sv$level))
      }

      header_values <- list()
      if (has_group) {
        header_values[[.CAT_KEY_P]] <- sv$p[1L]
        if (show_assoc) {
          header_values[[measure_col]] <- sv[[measure_col]][1L]
          if (assoc_ci) {
            header_values[[.CAT_KEY_CI_LL]] <- sv$ci_lower[1L]
            header_values[[.CAT_KEY_CI_UL]] <- sv$ci_upper[1L]
          }
        }
      }
      rows[[length(rows) + 1L]] <- list(
        label = lab,
        values = header_values,
        variable = var_name,
        level = NA_character_,
        role = "factor_header",
        indent = 0L
      )

      for (lv in lv_use) {
        sl <- sv[sv$level == lv, , drop = FALSE]
        values <- list()
        if (has_group) {
          for (g in group_levels) {
            sx <- sl[sl$group == g, , drop = FALSE]
            values[[.cat_key_n(g)]] <- if (nrow(sx)) sx$n[1L] else NA_real_
            values[[.cat_key_pct(g)]] <- if (nrow(sx)) sx$pct[1L] else NA_real_
          }
        } else {
          values[["n"]] <- sl$n[1L]
          values[["%"]] <- sl$pct[1L]
        }
        rows[[length(rows) + 1L]] <- list(
          label = paste0(indent_text, lv),
          values = values,
          variable = var_name,
          level = lv,
          role = if (identical(lv, missing_label)) "missing" else "level",
          indent = 1L
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
      v_digits = as.integer(v_digits),
      p_style = .style_p_style_token(),
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
  missing_group_label = NA_character_
) {
  has_group <- "group" %in% names(result)
  spec <- .continuous_token_columns(ci_level)

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
        # follows the registry while the neighbouring column name does
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
      p_style = .style_p_style_token(),
      threshold = 10^(-p_digits)
    )
    col_source[[.CON_KEY_P]] <- list(name = .CON_KEY_P, field = "p.value")
  }
  if (.CON_KEY_ES %in% names(display_df)) {
    col_names <- c(col_names, .CON_KEY_ES)
    col_meta[[.CON_KEY_ES]] <- list(
      token = "es",
      display_label = spicy_str("header_effect_size_short"),
      precision = as.integer(effect_size_digits)
    )
    col_source[[.CON_KEY_ES]] <- list(
      name = .CON_KEY_ES,
      field = "es_value",
      composite = TRUE
    )
  }

  rows <- list()
  for (i in seq_len(nrow(result))) {
    var_name <- result$variable[i]
    shown <- tokens_by_var[[var_name]] %||% tokens_union
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
      if (isTRUE(src$composite)) {
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
      # Recognised through the REGISTRY, never through a literal. Both
      # sides of this comparison are written by `build_display_df()`
      # from `spicy_str("cell_undefined")`, so a literal here holds only
      # while the registry value happens to be "--". The day it moves,
      # a literal would silently drop `cell_status` for every consumer
      # of `as_structured()` AND drop the display override with it --
      # and the typed view would then fall through to the shared
      # renderer, which prints its own, DIFFERENT undefined glyph
      # (U+2013, `.cell_to_string()`). This override is the only thing
      # holding those two glyphs together; unifying them is a rendering
      # change for the regression family and is not made here.
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
      p_style = .style_p_style_token(),
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
# row per variable, with the `by` levels as COLUMNS (`M (<level>)`),
# so the level of a marginal-mean column travels in that column's
# `col_meta$level` and the groups span nothing.
#
# `wide_raw` and `wide_display` are the two frames the function has
# already built for the raw and the console outputs, so the typed
# body and the printed body come from one computation each.
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
  spec
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
      # silently mislabelled with someone else's token. The lookup is
      # KEY against key, so this abort never sees a translated header
      # pass by -- it is the safety net of that very rule.
      spicy_abort(
        sprintf(
          "Internal: unrecognised continuous-lm display column %s.",
          shQuote(nm)
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
        p_style = .style_p_style_token(),
        threshold = 10^(-p_digits)
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
      weighted_n = list(token = "weighted_n", precision = as.integer(digits))
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
    values <- list()
    display <- character(0)
    for (nm in col_names) {
      val <- wide_raw[[nm]][i]
      if (!is.null(val) && length(val) == 1L) {
        values[[nm]] <- as.numeric(val)
      }
      if (nm %in% composite) {
        shown_str <- as.character(wide_display[[nm]][i])
        if (nzchar(shown_str)) {
          display[[nm]] <- shown_str
        }
      }
    }
    rows[[length(rows) + 1L]] <- list(
      label = wide_display[[.LM_KEY_VARIABLE]][i],
      values = values,
      variable = vars[i],
      level = NA_character_,
      role = "summary",
      indent = 0L,
      display = display
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
      p_style = .style_p_style_token(),
      p_threshold = .style_p_floor(p_digits),
      ci_level = ci_level
    ),
    ci_pairs = ci_pairs
  )
}
