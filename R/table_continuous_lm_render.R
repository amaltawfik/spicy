# Bivariate-layout rendering for table_continuous_lm() (one outcome x one
# predictor): build raw / display data frames and export to all 8 output
# formats.

# ---- frozen column keys ----------------------------------------------------
# The column NAME is a contract: it is what `output = "data.frame"`
# publishes, what the flextable `col_keys` / gt column ids / the gt CSS
# selector address, and what `as_structured()` indexes `col_meta` by. It
# is frozen English and is NEVER read from the registry. The header a
# reader sees is a separate layer (see `.lm_column_spec()`).
.LM_KEY_VARIABLE <- "Variable"
.LM_KEY_B <- "B"
.LM_KEY_P <- "p"
.LM_KEY_N <- "n"
.LM_KEY_WEIGHTED_N <- "Weighted n"
# The two snake_case columns of the raw frame: they have no display twin
# and never reach a header.
.LM_KEY_ES_CI_LL <- "effect_size_ci_lower"
.LM_KEY_ES_CI_UL <- "effect_size_ci_upper"
# The bare bound keys the exporters use: `rename_ci_cols_lm()` turns
# "95% CI LL" into "LL" because the engines carry the coverage in the
# spanner above the pair.
.LM_KEY_CI_LL <- "LL"
.LM_KEY_CI_UL <- "UL"
# The interval word INSIDE a column key ("95% CI LL"). Deliberately not
# the header key `header_ci_label_confidence`: a translated header must
# never move a public column name. The two are pinned equal in English
# by test-i18n.R.
.LM_KEY_CI <- "CI"
# The two glyphs a composite column key opens with.
.LM_KEY_MEAN <- "M"
.LM_KEY_DELTA <- "\u0394"
# The frozen glyph of each global test, keyed by `test_type`. The KEY
# table of `.lm_test_render()`.
.LM_TEST_GLYPHS <- c(
  z = "z",
  chi2 = "\u03C7\u00B2",
  t = "t",
  F = "F"
)

# The coverage percentage as it enters a frozen column key ("95%"). Four
# sites built this string independently (both frames, the exporter, the
# typed view); they now share one.
.lm_ci_pct <- function(ci_level) paste0(.ci_pct_str(ci_level), "%")

# The two interval-bound column keys ("95% CI LL" / "95% CI UL").
.lm_key_ci_ll <- function(ci_pct) {
  paste0(ci_pct, " ", .LM_KEY_CI, " ", .LM_KEY_CI_LL)
}
.lm_key_ci_ul <- function(ci_pct) {
  paste0(ci_pct, " ", .LM_KEY_CI, " ", .LM_KEY_CI_UL)
}

# The column key of one `by` level's marginal mean ("M (Female)"). The
# level is DATA; it travels in `col_meta$level` so a consumer never
# parses the key back. Vectorised over `level`.
.lm_key_emmean <- function(level) {
  paste0(.LM_KEY_MEAN, " (", level, ")")
}

# The columns the count / p-value alignment rule right-aligns. One rule,
# two engines (tinytable's default arm and Excel), compared KEY against
# key -- the same convention `table_continuous()` applies.
.lm_right_cols <- function(col_keys) {
  which(col_keys %in% c(.LM_KEY_N, .LM_KEY_WEIGHTED_N, .LM_KEY_P))
}

# The gt spanner ids of a set of columns, NAMED by their column key.
# Generated from the frozen KEYS, never from the labels, and read from
# this one vector by both the site that creates the spanners and the
# site that styles one, so an id can never be typed out a second time
# and drift.
#
# `make.names()` is lossy -- "M (a b)" and "M (a.b)" both become
# "M..a.b.", and so do "M (R²)" and "M (R³)" -- and gt aborts on a
# duplicate spanner id, taking the whole table down. `make.unique()`
# breaks the tie; it leaves a set with no collision exactly as it was.
#
# `make.unique()` cannot save a set whose KEYS already repeat: the two
# entries would then share a name, `span_ids[[key]]` would return the
# same id twice, and gt's duplicate-id abort would be back. The keys are
# built from `by` levels, which are unique factor levels, so this cannot
# happen today -- but the caller reads the result BY NAME, and a name
# that resolves to two things is the failure this helper exists to
# prevent. Refuse here rather than let gt refuse the table.
.lm_spanner_ids <- function(keys) {
  if (anyDuplicated(keys)) {
    spicy_abort(
      sprintf(
        "Spanner keys must be unique; %s repeats.",
        .quote_val(keys[anyDuplicated(keys)])
      ),
      class = "spicy_internal_invariant"
    )
  }
  stats::setNames(
    make.unique(paste0("spn_", make.names(keys)), sep = "_"),
    keys
  )
}

# ---- the label layer -------------------------------------------------------
# The twin of every key builder above. Same shape, same punctuation, the
# words read from the registry. Resolved in a function BODY, never in a
# top-level constant: a constant would read the registry once at build
# time and no translation could ever move it.

# The interval HEADER a reader sees ("95% CI"). Same template the
# regression, categorical and continuous families use, so the coverage /
# word order is translatable in one move.
#
# The percentage comes from `.ci_pct_str()`, the same producer the
# column key uses -- key and header cannot disagree about the coverage
# they announce.
.lm_ci_label <- function(ci_level) {
  spicy_fmt(
    "header_ci_spanner",
    .ci_pct_str(ci_level),
    spicy_str("header_ci_label_confidence")
  )
}

# The header of one `by` level's marginal mean ("M (Female)"). The level
# is DATA and travels as an argument, so a level containing a percent
# sign is safe.
.lm_label_emmean <- function(level) {
  spicy_fmt("header_lm_mean_level", spicy_str("header_mean"), level)
}

# The header of the difference between the two `by` levels.
.lm_label_delta <- function(block) {
  spicy_fmt(
    "header_lm_delta",
    spicy_str("symbol_delta"),
    block$level[2],
    block$level[1]
  )
}

# The glyph table of `.lm_test_render()`'s label twin.
.lm_test_glyph_labels <- function() {
  c(
    z = spicy_str("symbol_z"),
    chi2 = spicy_str("symbol_chi_sq"),
    t = spicy_str("symbol_t"),
    F = spicy_str("symbol_f")
  )
}

# The header of the test statistic. One format body with
# `get_test_header_lm()`, two glyph tables.
.lm_test_label <- function(block, show_statistic = TRUE, exact = TRUE) {
  .lm_test_render(
    .lm_test_parts(block, show_statistic, exact),
    .lm_test_glyph_labels()
  )
}

# The header of the goodness-of-fit column. An unknown token passes
# through unchanged, exactly as its key twin does.
.lm_r2_label <- function(r2_type = "r2") {
  switch(
    r2_type,
    r2 = spicy_str("symbol_r2"),
    adj_r2 = spicy_str("header_lm_adj_r2"),
    r2_type
  )
}

# The header of the effect-size column, same pass-through default.
.lm_es_label <- function(effect_size = "f2") {
  switch(
    effect_size,
    f2 = spicy_str("symbol_f2_partial"),
    d = spicy_str("symbol_cohens_d"),
    g = spicy_str("symbol_hedges_g"),
    omega2 = spicy_str("symbol_omega_sq_partial"),
    effect_size
  )
}

# ---- the column spec -------------------------------------------------------
# The ordered columns of one bivariate linear-model table: one entry per
# column, carrying the frozen KEY, the LABEL a reader sees and the
# semantic TOKEN. Built ONCE per table and read by the raw frame, the
# display frame, the typed view and the six exporters, so no consumer
# re-derives a column name: the four header generators used to be called
# 3, 4, 5 and 5 times per table, and the marginal-mean key was retyped
# at five sites.
#
# `raw_only` marks the two effect-size interval columns: they exist in
# `output = "data.frame"` and nowhere else, so they never reach a header.
.lm_column_spec <- function(
  x,
  ci_level = 0.95,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  show_weighted_n = FALSE,
  effect_size = "none",
  effect_size_ci = FALSE,
  r2_type = "r2",
  ci = TRUE
) {
  vars <- unique(x$variable)
  first_block <- x[x$variable == vars[1], , drop = FALSE]
  by_type <- unique(first_block$predictor_type)[1]
  ci_pct <- .lm_ci_pct(ci_level)
  ci_hdr <- .lm_ci_label(ci_level)
  include_es <- !identical(effect_size, "none")
  include_r2 <- !identical(r2_type, "none")

  spec <- list()
  add <- function(key, label, token, ...) {
    spec[[length(spec) + 1L]] <<- c(
      list(key = key, label = label, token = token),
      list(...)
    )
  }
  add(.LM_KEY_VARIABLE, spicy_str("header_variable"), "variable")

  add_bounds <- function() {
    ll_key <- .lm_key_ci_ll(ci_pct)
    ul_key <- .lm_key_ci_ul(ci_pct)
    ll_lab <- spicy_str("header_ci_ll")
    ul_lab <- spicy_str("header_ci_ul")
    add(
      ll_key,
      spicy_fmt("header_ci_bound", ci_hdr, ll_lab),
      "ci",
      ci_role = .LM_KEY_CI_LL,
      short_label = ll_lab,
      ci_pair = ul_key,
      ci_label = ci_hdr
    )
    add(
      ul_key,
      spicy_fmt("header_ci_bound", ci_hdr, ul_lab),
      "ci",
      ci_role = .LM_KEY_CI_UL,
      short_label = ul_lab,
      ci_pair = ll_key,
      ci_label = ci_hdr
    )
  }

  if (identical(by_type, "categorical")) {
    for (lev in first_block$level) {
      add(.lm_key_emmean(lev), .lm_label_emmean(lev), "emmean", level = lev)
    }
    if (nrow(first_block) == 2L) {
      add(
        get_delta_label_lm(first_block),
        .lm_label_delta(first_block),
        "delta"
      )
      if (isTRUE(ci)) {
        add_bounds()
      }
    }
  } else {
    add(.LM_KEY_B, spicy_str("header_b"), "b")
    if (isTRUE(ci)) {
      add_bounds()
    }
  }

  test_key <- get_test_header_lm(x, show_statistic, exact = TRUE)
  if (!is.null(test_key)) {
    add(
      test_key,
      .lm_test_label(x, show_statistic, exact = TRUE),
      "statistic"
    )
  }
  if (show_p_value) {
    add(.LM_KEY_P, spicy_str("header_p"), "p")
  }
  if (include_r2) {
    add(format_r2_header_lm(r2_type), .lm_r2_label(r2_type), "r2")
  }
  if (include_es) {
    add(
      format_effect_size_header_lm(effect_size),
      .lm_es_label(effect_size),
      "es",
      effect_size = effect_size
    )
    if (isTRUE(effect_size_ci)) {
      # No display twin: an internal identifier published in the wide
      # contract. Its own key IS its label -- there is no header to
      # translate.
      add(.LM_KEY_ES_CI_LL, .LM_KEY_ES_CI_LL, "es_ci", raw_only = TRUE)
      add(.LM_KEY_ES_CI_UL, .LM_KEY_ES_CI_UL, "es_ci", raw_only = TRUE)
    }
  }
  if (show_n) {
    add(.LM_KEY_N, spicy_str("header_n_lower"), "n")
  }
  if (show_weighted_n) {
    add(.LM_KEY_WEIGHTED_N, spicy_str("header_weighted_n"), "weighted_n")
  }
  spec
}

# The frozen keys of a spec, in column order.
.lm_spec_keys <- function(spec) {
  vapply(spec, function(e) e$key, character(1))
}

# The headers of a spec as one vector NAMED by frozen key -- the carrier
# every engine reads, since `col_meta` reaches only the default output.
# Each interval bound appears twice: under its full key and under the
# short key `rename_ci_cols_lm()` leaves behind.
.lm_spec_labels <- function(spec) {
  labels <- vapply(spec, function(e) e$label, character(1))
  names(labels) <- .lm_spec_keys(spec)
  for (e in spec) {
    if (!is.null(e$ci_role)) {
      labels[[e$ci_role]] <- e$short_label
    }
  }
  labels
}

# The key of the single column carrying `token`, or NULL when the table
# has none.
.lm_spec_key <- function(spec, token) {
  for (e in spec) {
    if (identical(e$token, token)) {
      return(e$key)
    }
  }
  NULL
}

# Resolve frozen column keys to the headers a reader sees. An unknown
# key returns itself, and `NULL` labels are the identity: the exporter
# is also reached with hand-built frames carrying columns no spec can
# produce. The result has the length of its input by construction, so
# no consumer needs a shape guard.
.lm_labels <- function(col_keys, labels = NULL) {
  if (is.null(labels)) {
    return(col_keys)
  }
  out <- unname(labels[col_keys])
  miss <- is.na(out)
  out[miss] <- col_keys[miss]
  out
}

build_wide_raw_continuous_lm <- function(
  x,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  show_weighted_n = FALSE,
  effect_size = "none",
  effect_size_ci = FALSE,
  r2_type = "r2",
  ci = TRUE,
  ci_level = 0.95,
  spec = NULL
) {
  vars <- unique(x$variable)
  first_block <- x[x$variable == vars[1], , drop = FALSE]
  by_type <- unique(first_block$predictor_type)[1]
  ci_pct <- .lm_ci_pct(ci_level)
  ci_ll_name <- .lm_key_ci_ll(ci_pct)
  ci_ul_name <- .lm_key_ci_ul(ci_pct)
  include_es <- !identical(effect_size, "none")
  include_es_ci <- include_es && isTRUE(effect_size_ci)
  include_r2 <- !identical(r2_type, "none")
  # `spec` is normally computed once by `table_continuous_lm()` and
  # shared with the display frame, the typed view and the exporters;
  # absent, it is derived here from the very same arguments.
  spec <- spec %||%
    .lm_column_spec(
      x,
      ci_level = ci_level,
      show_statistic = show_statistic,
      show_p_value = show_p_value,
      show_n = show_n,
      show_weighted_n = show_weighted_n,
      effect_size = effect_size,
      effect_size_ci = effect_size_ci,
      r2_type = r2_type,
      ci = ci
    )
  test_header <- .lm_spec_key(spec, "statistic")
  r2_header <- .lm_spec_key(spec, "r2")
  es_header <- .lm_spec_key(spec, "es")
  delta_name <- .lm_spec_key(spec, "delta")

  out <- data.frame(
    Variable = vapply(
      vars,
      function(v) x$label[match(v, x$variable)],
      character(1)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # The column set and its ORDER come from the spec, so the raw frame,
  # the display frame and the typed view cannot disagree about either.
  for (ent in spec) {
    if (identical(ent$key, .LM_KEY_VARIABLE)) {
      next
    }
    out[[ent$key]] <- if (identical(ent$token, "n")) NA_integer_ else NA_real_
  }

  for (i in seq_along(vars)) {
    block <- x[x$variable == vars[i], , drop = FALSE]
    test_row <- get_test_row_index_lm(block)

    if (identical(by_type, "categorical")) {
      for (j in seq_len(nrow(block))) {
        # `[<-` on an absent name ADDS a column. The level comes from
        # THIS block while the columns were created from `first_block`,
        # so the two must agree -- they do, because `by_levels` is
        # captured before the per-outcome complete-case filtering.
        out[i, .lm_key_emmean(block$level[j])] <- block$emmean[j]
      }
      if (nrow(block) == 2L) {
        out[[delta_name]][i] <- block$estimate[test_row]
        if (isTRUE(ci)) {
          out[[ci_ll_name]][i] <- block$estimate_ci_lower[test_row]
          out[[ci_ul_name]][i] <- block$estimate_ci_upper[test_row]
        }
      }
    } else {
      out[[.LM_KEY_B]][i] <- block$estimate[1]
      if (isTRUE(ci)) {
        out[[ci_ll_name]][i] <- block$estimate_ci_lower[1]
        out[[ci_ul_name]][i] <- block$estimate_ci_upper[1]
      }
    }

    if (!is.null(test_header)) {
      out[[test_header]][i] <- block$statistic[test_row]
    }
    if (show_p_value) {
      out[[.LM_KEY_P]][i] <- block$p.value[test_row]
    }
    if (include_r2) {
      out[[r2_header]][i] <- get_r2_value_lm(block, r2_type)
    }
    if (include_es) {
      out[[es_header]][i] <- block$es_value[1]
      if (include_es_ci) {
        out[[.LM_KEY_ES_CI_LL]][i] <- block$es_ci_lower[1]
        out[[.LM_KEY_ES_CI_UL]][i] <- block$es_ci_upper[1]
      }
    }
    if (show_n) {
      out[[.LM_KEY_N]][i] <- block$n[1]
    }
    if (show_weighted_n) {
      out[[.LM_KEY_WEIGHTED_N]][i] <- block$weighted_n[1]
    }
  }

  out
}

build_wide_display_df_continuous_lm <- function(
  x,
  digits = 2L,
  decimal_mark = ".",
  ci_level = 0.95,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  show_weighted_n = FALSE,
  effect_size = "none",
  effect_size_ci = FALSE,
  r2_type = "r2",
  ci = TRUE,
  fit_digits = 2L,
  effect_size_digits = 2L,
  p_digits = 3L,
  spec = NULL
) {
  vars <- unique(x$variable)
  by_type <- unique(x[x$variable == vars[1], , drop = FALSE]$predictor_type)[1]
  ci_pct <- .lm_ci_pct(ci_level)
  ci_ll_name <- .lm_key_ci_ll(ci_pct)
  ci_ul_name <- .lm_key_ci_ul(ci_pct)
  include_es <- !identical(effect_size, "none")
  include_es_ci <- include_es && isTRUE(effect_size_ci)
  include_r2 <- !identical(r2_type, "none")
  # See the raw builder: one spec per table, computed upstream when
  # there is one.
  spec <- spec %||%
    .lm_column_spec(
      x,
      ci_level = ci_level,
      show_statistic = show_statistic,
      show_p_value = show_p_value,
      show_n = show_n,
      show_weighted_n = show_weighted_n,
      effect_size = effect_size,
      effect_size_ci = effect_size_ci,
      r2_type = r2_type,
      ci = ci
    )
  test_header <- .lm_spec_key(spec, "statistic")
  r2_header <- .lm_spec_key(spec, "r2")
  es_header <- .lm_spec_key(spec, "es")
  delta_name <- .lm_spec_key(spec, "delta")

  out <- data.frame(
    Variable = vapply(
      vars,
      function(v) x$label[match(v, x$variable)],
      character(1)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Same spec, same order as the raw frame, minus the two raw-only
  # effect-size bounds, which have no displayed twin.
  for (ent in spec) {
    if (identical(ent$key, .LM_KEY_VARIABLE) || isTRUE(ent$raw_only)) {
      next
    }
    out[[ent$key]] <- ""
  }

  for (i in seq_along(vars)) {
    block <- x[x$variable == vars[i], , drop = FALSE]
    test_row <- get_test_row_index_lm(block)
    if (identical(by_type, "categorical")) {
      for (j in seq_len(nrow(block))) {
        # See the raw builder: `[<-` on an absent name would ADD a
        # column, so this block's levels must match `first_block`'s.
        out[i, .lm_key_emmean(block$level[j])] <- format_number(
          block$emmean[j],
          digits,
          decimal_mark
        )
      }
      if (nrow(block) == 2L) {
        out[[delta_name]][i] <- format_number(
          block$estimate[test_row],
          digits,
          decimal_mark
        )
        if (isTRUE(ci)) {
          out[[ci_ll_name]][i] <- format_number(
            block$estimate_ci_lower[test_row],
            digits,
            decimal_mark
          )
          out[[ci_ul_name]][i] <- format_number(
            block$estimate_ci_upper[test_row],
            digits,
            decimal_mark
          )
        }
      }
    } else {
      out[[.LM_KEY_B]][i] <- format_number(
        block$estimate[1],
        digits,
        decimal_mark
      )
      if (isTRUE(ci)) {
        out[[ci_ll_name]][i] <- format_number(
          block$estimate_ci_lower[1],
          digits,
          decimal_mark
        )
        out[[ci_ul_name]][i] <- format_number(
          block$estimate_ci_upper[1],
          digits,
          decimal_mark
        )
      }
    }

    if (!is.null(test_header)) {
      out[[test_header]][i] <- format_number(
        block$statistic[test_row],
        digits,
        decimal_mark
      )
    }
    if (show_p_value) {
      out[[.LM_KEY_P]][i] <- format_p_value(
        block$p.value[test_row],
        decimal_mark,
        digits = p_digits
      )
    }
    if (include_es) {
      es_str <- format_number(
        block$es_value[1],
        effect_size_digits,
        decimal_mark
      )
      if (include_es_ci) {
        es_lo <- format_number(
          block$es_ci_lower[1],
          effect_size_digits,
          decimal_mark
        )
        es_hi <- format_number(
          block$es_ci_upper[1],
          effect_size_digits,
          decimal_mark
        )
        if (nzchar(es_str) && nzchar(es_lo) && nzchar(es_hi)) {
          sep <- ci_bracket_separator(decimal_mark)
          br <- .style_ci_brackets()
          es_str <- paste0(es_str, " ", br[[1L]], es_lo, sep, es_hi, br[[2L]])
        }
      }
      out[[es_header]][i] <- es_str
    }
    if (include_r2) {
      out[[r2_header]][i] <- format_number(
        get_r2_value_lm(block, r2_type),
        fit_digits,
        decimal_mark
      )
    }
    if (show_n) {
      out[[.LM_KEY_N]][i] <- if (is.na(block$n[1])) {
        ""
      } else {
        as.character(as.integer(block$n[1]))
      }
    }
    if (show_weighted_n) {
      out[[.LM_KEY_WEIGHTED_N]][i] <- if (is.na(block$weighted_n[1])) {
        ""
      } else {
        format_number(block$weighted_n[1], digits, decimal_mark)
      }
    }
  }

  out
}

export_continuous_lm_table <- function(
  display_df,
  output,
  ci_level,
  align = "decimal",
  decimal_mark = ".",
  excel_path,
  excel_sheet,
  clipboard_delim,
  word_path,
  note = NULL,
  title = NULL,
  labels = NULL
) {
  ci_pct <- .lm_ci_pct(ci_level)
  ci_ll <- .lm_key_ci_ll(ci_pct)
  ci_ul <- .lm_key_ci_ul(ci_pct)
  # The headers, keyed by frozen column key. `col_meta$display_label`
  # cannot serve here: the typed view is built only for the default
  # output, and this function receives the display frame alone. NULL
  # (a frame nobody's spec produced) is the identity.
  lab <- function(keys) .lm_labels(keys, labels)
  ci_spanner_label <- .lm_ci_label(ci_level)
  # Membership, not a flag from upstream: the exporter is also reached
  # with a hand-built frame that carries no interval at all.
  has_ci <- all(c(ci_ll, ci_ul) %in% names(display_df))

  # For engines without native decimal alignment (flextable, word),
  # pre-pad numeric cells with leading/trailing spaces so decimal
  # points line up vertically. gt and tinytable have native
  # decimal alignment and are handled with their own API. Excel keeps
  # the engine-default alignment (proportional fonts make cell-string
  # padding unreliable; native decimal alignment in Excel would
  # require writing raw numbers + a number format).
  use_decimal <- identical(align, "decimal")
  # gt and tinytable join the padding engines. Their native
  # decimal primitives (`gt::cols_align_decimal()`,
  # `tinytable::style_tt(align = "d")`) do not produce visually
  # centred decimal alignment:
  #   * gt renders as right-anchored with the decimal point at
  #     a column-internal right boundary;
  #   * tinytable centres each cell on its OWN value, ignoring
  #     other cells -- so decimals do not coincide across rows.
  # Same single-font decimal-anchored convention as
  # table_regression() (regression_dispatch.R:639-661 for gt and
  # the tinytable handler nearby): pad cells to uniform width
  # upstream, centre them downstream, decimals coincide because
  # every cell has the same character width on each side of the
  # dot.
  # The clipboard is deliberately absent from the padding engines:
  # its payload is parsed, not read at a fixed width, and the U+2007
  # pad character is not whitespace to a parser (a padded number
  # pastes as text beside an unpadded number).
  needs_padding_engine <- output %in%
    c("flextable", "word", "gt", "tinytable")
  if (use_decimal && needs_padding_engine) {
    # Pad with U+2007 FIGURE SPACE so the padding survives HTML
    # whitespace collapsing and markdown-table cell-edge trimming.
    # Same convention as `.pad_for_decimal_align()` in
    # `table_regression()`.
    numeric_cols <- setdiff(seq_along(display_df), 1L)
    for (j in numeric_cols) {
      display_df[[j]] <- decimal_align_strings(
        display_df[[j]],
        decimal_mark = decimal_mark,
        pad_char = "\u2007"
      )
    }
  }

  if (identical(output, "tinytable")) {
    if (!requireNamespace("tinytable", quietly = TRUE)) {
      spicy_abort("Install package 'tinytable'.", class = "spicy_missing_pkg")
    }
    old_tt_opt <- getOption("tinytable_print_output")
    options(tinytable_print_output = "html")
    on.exit(options(tinytable_print_output = old_tt_opt), add = TRUE)

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    ll_pos <- which(col_keys == .LM_KEY_CI_LL)
    ul_pos <- which(col_keys == .LM_KEY_CI_UL)

    sub_labels <- rep("", nc)
    if (has_ci) {
      sub_labels[ll_pos] <- lab(.LM_KEY_CI_LL)
      sub_labels[ul_pos] <- lab(.LM_KEY_CI_UL)
    }
    colnames(display_df) <- sub_labels

    # `tinytable::group_tt(j = )` wants the printed spanner text as the
    # NAME of the list, so a name here IS an index: two columns whose
    # headers read the same would merge and one would vanish from the
    # header. Inherent to the upstream API, not a choice made here.
    gspec <- list()
    for (j in seq_along(col_keys)) {
      if (has_ci && col_keys[j] %in% c(.LM_KEY_CI_LL, .LM_KEY_CI_UL)) {
        next
      }
      gspec[[lab(col_keys[j])]] <- j
    }
    if (has_ci) {
      gspec[[ci_spanner_label]] <- c(ll_pos, ul_pos)
    }

    # `notes = note` keeps native footnote rendering for the LaTeX /
    # typst / markdown backends; the HTML output re-injects the note
    # outside the table grid via the finalize below.
    tt <- tinytable::tt(
      display_df,
      # Same title the console prints ("Continuous outcomes by <x>"),
      # like the captions of the other tinytable descriptive families.
      caption = if (!is.null(title) && nzchar(title)) title else NULL,
      notes = if (!is.null(note) && nzchar(note)) note else NULL
    )
    tt <- tinytable::group_tt(tt, j = gspec)
    tt <- .spicy_tt_bare(tt)
    tt <- tinytable::style_tt(tt, j = 1, align = "l")
    if (ncol(display_df) > 1L) {
      numeric_j <- setdiff(seq_len(nc), 1L)
      if (use_decimal && length(numeric_j) > 0L) {
        # Cells were pre-padded upstream; centring uniform-width
        # strings places the decimal points at the same horizontal
        # position. Same tinytable strategy as table_regression()
        # (uses align = "c" with pre-padding rather than align = "d"
        # which centres each cell on its own value independently).
        tt <- tinytable::style_tt(tt, j = numeric_j, align = "c")
      } else if (identical(align, "center") && length(numeric_j) > 0L) {
        tt <- tinytable::style_tt(tt, j = numeric_j, align = "c")
      } else if (identical(align, "right") && length(numeric_j) > 0L) {
        for (rj in numeric_j) {
          tt <- tinytable::style_tt(tt, j = rj, align = "r")
        }
      } else {
        right_j <- .lm_right_cols(col_keys)
        center_j <- setdiff(seq_len(nc), c(1L, right_j))
        if (length(center_j) > 0L) {
          tt <- tinytable::style_tt(tt, j = center_j, align = "c")
        }
        if (length(right_j) > 0L) {
          for (rj in right_j) {
            tt <- tinytable::style_tt(tt, j = rj, align = "r")
          }
        }
      }
      spanner_center_j <- setdiff(seq_len(nc), 1L)
      if (length(spanner_center_j) > 0L) {
        tt <- tinytable::style_tt(tt, i = -1, j = spanner_center_j, align = "c")
      }
      tt <- tinytable::style_tt(tt, i = -1, j = 1L, align = "l")
      tt <- tinytable::style_tt(
        tt,
        i = -1,
        j = seq_len(nc),
        line = "t",
        line_width = 0.06
      )
      if (has_ci) {
        tt <- tinytable::style_tt(
          tt,
          i = -1,
          j = c(ll_pos, ul_pos),
          line = "b",
          line_width = 0.06
        )
      }
      tt <- tinytable::style_tt(
        tt,
        i = 0,
        j = seq_len(nc),
        line = "b",
        line_width = 0.06
      )
      tt <- tinytable::style_tt(
        tt,
        i = nrow(display_df),
        j = seq_len(nc),
        line = "b",
        line_width = 0.06
      )
      p_j <- which(col_keys == .LM_KEY_P)
      if (length(p_j) == 1L) {
        tt <- tinytable::style_tt(
          tt,
          j = p_j,
          html_css = "white-space: nowrap;"
        )
      }
    }

    # ---- Note rendering (HTML): strip the rendered `<tfoot>` and ------
    # wrap the table together with the note in an `inline-block` flex
    # sibling. Same mechanism, markup and CSS as table_regression()'s
    # output_tinytable(): both call the shared pair in R/tt_theme.R, so
    # a note reads the same way whichever family produced the table.
    if (!is.null(note) && nzchar(note)) {
      note_div <- .spicy_tt_note_div(note)
      tt <- tinytable::style_tt(tt, finalize = function(x) {
        if (identical(x@output, "html")) {
          x@table_string <- .spicy_tt_wrap_html(x@table_string, note_div)
        }
        x
      })
    }
    return(tt)
  }

  if (identical(output, "gt")) {
    if (!requireNamespace("gt", quietly = TRUE)) {
      spicy_abort("Install package 'gt'.", class = "spicy_missing_pkg")
    }

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    tbl <- gt::gt(display_df)

    label_list <- stats::setNames(as.list(rep("", length(col_keys))), col_keys)
    if (has_ci && .LM_KEY_CI_LL %in% col_keys) {
      label_list[[.LM_KEY_CI_LL]] <- lab(.LM_KEY_CI_LL)
    }
    if (has_ci && .LM_KEY_CI_UL %in% col_keys) {
      label_list[[.LM_KEY_CI_UL]] <- lab(.LM_KEY_CI_UL)
    }
    tbl <- gt::cols_label(tbl, .list = label_list)

    single_cols <- setdiff(col_keys, c(.LM_KEY_CI_LL, .LM_KEY_CI_UL))
    # The ids are DOM state that `cells_column_spanners()` addresses
    # below: generated from the frozen KEYS, never from the labels, and
    # generated ONCE so the two sites cannot disagree.
    span_ids <- .lm_spanner_ids(single_cols)
    for (col in single_cols) {
      tbl <- gt::tab_spanner(
        tbl,
        label = lab(col),
        columns = col,
        id = span_ids[[col]]
      )
    }
    if (has_ci) {
      tbl <- gt::tab_spanner(
        tbl,
        label = ci_spanner_label,
        columns = c(.LM_KEY_CI_LL, .LM_KEY_CI_UL)
      )
    }

    tbl <- gt::cols_align(tbl, align = "left", columns = .LM_KEY_VARIABLE)
    numeric_cols <- setdiff(col_keys, .LM_KEY_VARIABLE)
    if (use_decimal && length(numeric_cols) > 0L) {
      # Cells were pre-padded with figure-spaces upstream; centring
      # uniform-width strings places the decimal points at the same
      # horizontal position. Same gt strategy as table_regression().
      tbl <- gt::cols_align(tbl, align = "center", columns = numeric_cols)
    } else if (identical(align, "center") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "center", columns = numeric_cols)
    } else if (identical(align, "right") && length(numeric_cols) > 0L) {
      tbl <- gt::cols_align(tbl, align = "right", columns = numeric_cols)
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
      locations = gt::cells_column_spanners()
    )
    if (has_ci) {
      tbl <- gt::tab_style(
        tbl,
        style = rule_top,
        locations = gt::cells_column_labels(
          columns = c(.LM_KEY_CI_LL, .LM_KEY_CI_UL)
        )
      )
    }
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_column_labels()
    )
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_body(rows = nrow(display_df))
    )
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(align = "left"),
      locations = gt::cells_column_labels(columns = .LM_KEY_VARIABLE)
    )
    non_variable_cols <- setdiff(col_keys, .LM_KEY_VARIABLE)
    if (length(non_variable_cols) > 0L) {
      tbl <- gt::tab_style(
        tbl,
        style = gt::cell_text(align = "center"),
        locations = gt::cells_column_labels(columns = non_variable_cols)
      )
    }
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(align = "left"),
      locations = gt::cells_column_spanners(
        spanners = span_ids[[.LM_KEY_VARIABLE]]
      )
    )

    # Interpolated into a CSS attribute selector: the ids are the frozen
    # ASCII keys, so no display label ever reaches this string.
    ci_css_sel <- if (has_ci) {
      paste(
        vapply(
          c(.LM_KEY_CI_LL, .LM_KEY_CI_UL),
          function(id) sprintf('.gt_table thead tr:last-child th[id="%s"]', id),
          character(1)
        ),
        collapse = ",\n"
      )
    } else {
      ""
    }
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
      if (has_ci) paste0(ci_css_sel, " {") else "",
      if (has_ci) "  border-top: 1px solid currentColor !important;" else "",
      if (has_ci) "}" else "",
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
      ".gt_table .gt_col_heading, .gt_table .gt_spanner {",
      "  white-space: nowrap !important;",
      "}",
      ".gt_table .gt_row .gt_right, .gt_table .gt_row .gt_center {",
      "  white-space: nowrap !important;",
      "}",
      sep = "\n"
    )
    tbl <- gt::opt_css(tbl, css = apa_css)

    # Note: NOT added via gt's native `tab_source_note()` (its
    # `<tfoot>` colspan cell widens the table in narrow viewports;
    # same pathology as the tinytable / flextable tfoot). Instead
    # stash the raw note + tag the `spicy_gt` sub-class; the shared
    # `print.spicy_gt` / `knit_print.spicy_gt` methods
    # (regression_dispatch.R) post-process the rendered HTML to
    # inject the note as a `<div>` outside the table.
    if (!is.null(note) && nzchar(note)) {
      attr(tbl, "spicy_note") <- note
      class(tbl) <- c("spicy_gt", class(tbl))
    }
    return(tbl)
  }

  if (output %in% c("flextable", "word")) {
    if (!requireNamespace("flextable", quietly = TRUE)) {
      spicy_abort("Install package 'flextable'.", class = "spicy_missing_pkg")
    }
    if (
      identical(output, "word") &&
        !requireNamespace("officer", quietly = TRUE)
    ) {
      spicy_abort("Install package 'officer'.", class = "spicy_missing_pkg")
    }

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    hdrs <- build_header_rows_lm(col_keys, ci_spanner_label, labels)
    map <- data.frame(
      col_keys = col_keys,
      top = hdrs$top,
      bottom = hdrs$bottom,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    ft <- flextable::flextable(display_df)
    ft <- flextable::set_header_df(ft, mapping = map, key = "col_keys")
    # `merge_h()` merges ADJACENT header cells carrying the same TEXT:
    # the geometry of the CI spanner is born of a string equality, not
    # of the pairing the caller already knows. Two neighbouring columns
    # whose headers read alike would merge into a phantom spanner. Kept
    # as-is: replacing it by explicit merges is a mechanism change to be
    # proved on its own.
    ft <- flextable::merge_h(ft, part = "header")

    bd <- spicy_fp_border(color = "black", width = 1)
    ci_j <- which(col_keys %in% c(.LM_KEY_CI_LL, .LM_KEY_CI_UL))
    left_j <- 1L
    numeric_j <- setdiff(seq_along(col_keys), left_j)

    ft <- flextable::align(ft, j = left_j, part = "header", align = "left")
    ft <- flextable::align(ft, j = left_j, part = "body", align = "left")

    if (use_decimal && length(numeric_j) > 0L) {
      # Cells are pre-padded for decimal alignment; CENTRE the
      # padded strings in the default body font (no monospace
      # override). Same single-font policy as table_regression()
      # (regression_dispatch.R:1345): with uniform-precision columns
      # the cells have the same character width, so centring still
      # LOOKS decimal-aligned in any font with tabular figures
      # (Calibri's default in Word tables). Trade strict decimal
      # alignment for a single-font, visually consistent table.
      ft <- flextable::align(
        ft,
        j = numeric_j,
        part = "header",
        align = "center"
      )
      ft <- flextable::align(ft, j = numeric_j, part = "body", align = "center")
    } else if (identical(align, "center") && length(numeric_j) > 0L) {
      ft <- flextable::align(ft, j = numeric_j, part = "all", align = "center")
    } else if (identical(align, "right") && length(numeric_j) > 0L) {
      ft <- flextable::align(
        ft,
        j = numeric_j,
        part = "header",
        align = "center"
      )
      ft <- flextable::align(ft, j = numeric_j, part = "body", align = "right")
    }

    ft <- flextable::hline_top(ft, part = "header", border = bd)
    if (has_ci) {
      ft <- flextable::hline(
        ft,
        i = 1,
        j = ci_j,
        part = "header",
        border = bd
      )
    }
    ft <- flextable::hline_bottom(ft, part = "header", border = bd)
    ft <- flextable::hline_bottom(ft, part = "body", border = bd)
    if (.LM_KEY_P %in% col_keys) {
      p_j <- which(col_keys == .LM_KEY_P)
      ft <- flextable::compose(
        ft,
        j = p_j,
        part = "body",
        value = flextable::as_paragraph(
          flextable::as_chunk(display_df[[p_j]])
        )
      )
    }
    ft <- flextable::autofit(ft)

    if (!is.null(note) && nzchar(note)) {
      # APA Manual 7 Section 7.14: general notes form a SINGLE
      # paragraph ("*Note.* ...") that wraps naturally within the
      # table width. Collapse embedded newlines (source-side line
      # breaks meant for ASCII rendering) into spaces, then emit one
      # footer line whose leading "Note." chunk is italicised and the
      # remainder is in regular type. Same mechanism as
      # table_regression()'s output_flextable; `fp_text_lite()` (only
      # the italic flag set) keeps the footer in the table's default
      # font -- this builder, unlike table_regression()'s, does not
      # force a font, so hard-coding one here would make the note
      # clash with the body.
      note_one_line <- gsub("\n", " ", note, fixed = TRUE)
      note_split <- .note_prefix_split(note_one_line)
      if (!is.null(note_split)) {
        note_para <- flextable::as_paragraph(
          flextable::as_chunk(
            note_split$marker,
            props = officer::fp_text_lite(italic = TRUE)
          ),
          flextable::as_chunk(note_split$rest)
        )
      } else {
        note_para <- flextable::as_paragraph(
          flextable::as_chunk(note_one_line)
        )
      }
      ft <- flextable::add_footer_lines(ft, top = FALSE, values = note_para)
    }

    if (identical(output, "word")) {
      if (is.null(word_path) || !nzchar(word_path)) {
        spicy_abort(
          "`word_path` must be provided for `output = \"word\"`.",
          class = "spicy_invalid_input"
        )
      }
      # The footer added above flows into the docx via the flextable
      # object, mirroring table_regression()'s word output (which
      # keeps the in-table footer for docx fidelity). The title the
      # console prints becomes the Word caption -- for a by-table it
      # names the grouping variable, which nothing else in the
      # document states.
      ft <- .spicy_ft_word_caption(ft, title)
      flextable::save_as_docx(ft, path = word_path)
      return(invisible(word_path))
    }

    # Tag with the shared `spicy_flextable` sub-class + stash the raw
    # note so `print.spicy_flextable` / `knit_print.spicy_flextable`
    # (regression_dispatch.R) strip the rendered `<tfoot>` and
    # re-inject the note as a `<div>` outside the table in HTML
    # contexts (same trick as the tinytable branch above).
    if (!is.null(note) && nzchar(note)) {
      attr(ft, "spicy_note") <- note
    }
    # Same title the console prints -- for a by-table it names the
    # grouping variable, which nothing else in the table states.
    ft <- .spicy_ft_html_caption(ft, title)
    class(ft) <- c("spicy_flextable", class(ft))
    return(ft)
  }

  if (identical(output, "excel")) {
    if (!requireNamespace("openxlsx2", quietly = TRUE)) {
      spicy_abort("Install package 'openxlsx2'.", class = "spicy_missing_pkg")
    }
    if (is.null(excel_path) || !nzchar(excel_path)) {
      spicy_abort(
        "`excel_path` must be provided for `output = \"excel\"`.",
        class = "spicy_invalid_input"
      )
    }

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    nc <- length(col_keys)
    hdrs <- build_header_rows_lm(col_keys, ci_spanner_label, labels)
    ci_j <- which(col_keys %in% c(.LM_KEY_CI_LL, .LM_KEY_CI_UL))

    wb <- openxlsx2::wb_workbook()
    wb <- openxlsx2::wb_add_worksheet(wb, excel_sheet)
    # Same title the console prints -- for a by-table it names the
    # grouping variable, which nothing else in the sheet states.
    if (!is.null(title) && nzchar(title)) {
      wb <- openxlsx2::wb_add_data(wb, x = title, start_row = 1)
    }
    top_header_row <- 3L
    bot_header_row <- top_header_row + 1L
    first_body_row <- bot_header_row + 1L
    wb <- openxlsx2::wb_add_data(
      wb,
      x = as.data.frame(t(hdrs$top), stringsAsFactors = FALSE),
      start_row = top_header_row,
      col_names = FALSE
    )
    wb <- openxlsx2::wb_add_data(
      wb,
      x = as.data.frame(t(hdrs$bottom), stringsAsFactors = FALSE),
      start_row = bot_header_row,
      col_names = FALSE
    )
    # `na.strings = ""`: an empty cell stays empty instead of becoming
    # an Excel error cell ("#N/A").
    wb <- openxlsx2::wb_add_data(
      wb,
      x = display_df,
      start_row = first_body_row,
      col_names = FALSE,
      row_names = FALSE,
      na.strings = ""
    )
    if (has_ci) {
      wb <- openxlsx2::wb_merge_cells(
        wb,
        dims = openxlsx2::wb_dims(rows = top_header_row, cols = ci_j)
      )
    }
    last_row <- bot_header_row + nrow(display_df)

    left_cols <- 1L
    right_cols <- .lm_right_cols(col_keys)
    center_cols <- setdiff(seq_len(nc), c(left_cols, right_cols))
    header_rows <- top_header_row:bot_header_row
    body_rows <- if (last_row >= first_body_row) {
      first_body_row:last_row
    } else {
      integer(0)
    }

    wb <- openxlsx2::wb_add_cell_style(
      wb,
      dims = openxlsx2::wb_dims(
        rows = top_header_row:last_row,
        cols = left_cols
      ),
      horizontal = "left"
    )
    if (length(center_cols) > 0L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = header_rows, cols = center_cols),
        horizontal = "center",
        vertical = "center"
      )
      if (length(body_rows) > 0L) {
        wb <- openxlsx2::wb_add_cell_style(
          wb,
          dims = openxlsx2::wb_dims(rows = body_rows, cols = center_cols),
          horizontal = "center",
          vertical = "center"
        )
      }
    }
    if (length(right_cols) > 0L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = header_rows, cols = right_cols),
        horizontal = "center",
        vertical = "center"
      )
    }
    if (length(right_cols) > 0L && length(body_rows) > 0L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        dims = openxlsx2::wb_dims(rows = body_rows, cols = right_cols),
        horizontal = "right"
      )
    }

    # APA borders. IMPORTANT: openxlsx2::wb_add_border() has formal
    # defaults `left_border = right_border = top_border =
    # bottom_border = "thin"`, so an explicit `top_border = "thin"`
    # call paints all four sides unless the others are set to NULL.
    # Pass NULL on every unused side to draw only the intended rule.
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = top_header_row, cols = 1:nc),
      top_border = "thin",
      bottom_border = NULL,
      left_border = NULL,
      right_border = NULL
    )
    if (has_ci) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = top_header_row, cols = ci_j),
        bottom_border = "thin",
        top_border = NULL,
        left_border = NULL,
        right_border = NULL
      )
    }
    wb <- openxlsx2::wb_add_border(
      wb,
      dims = openxlsx2::wb_dims(rows = bot_header_row, cols = 1:nc),
      bottom_border = "thin",
      top_border = NULL,
      left_border = NULL,
      right_border = NULL
    )
    if (nrow(display_df) > 0) {
      wb <- openxlsx2::wb_add_border(
        wb,
        dims = openxlsx2::wb_dims(rows = last_row, cols = 1:nc),
        bottom_border = "thin",
        top_border = NULL,
        left_border = NULL,
        right_border = NULL
      )
    }
    # Note below the table (one worksheet row per note line), same
    # placement as table_regression()'s output_excel.
    wb <- .spicy_xl_add_note(wb, note = note, start_row = last_row + 2L)
    wb <- .spicy_xl_set_widths(
      wb,
      sheet = excel_sheet,
      cells = .spicy_xl_cells(
        display_df,
        headers = list(hdrs$top, hdrs$bottom)
      )
    )
    openxlsx2::wb_save(wb, excel_path, overwrite = TRUE)
    return(invisible(excel_path))
  }

  if (identical(output, "clipboard")) {
    .spicy_clip_preflight()

    display_df <- rename_ci_cols_lm(display_df, ci_ll, ci_ul)
    col_keys <- names(display_df)
    hdrs <- build_header_rows_lm(col_keys, ci_spanner_label, labels)
    # The sub-label row carries the LL / UL labels of the CI pair;
    # with no CI column it is empty and is dropped rather than
    # pasted as a blank line (same rule as `clipboard_payload()`).
    # Asked of the STRUCTURE, not of the emptiness of a rendered
    # label: the sub-row is non-empty exactly where the pair is.
    clip_mat <- if (has_ci) {
      rbind(hdrs$top, hdrs$bottom, as.matrix(display_df))
    } else {
      rbind(hdrs$top, as.matrix(display_df))
    }
    # Same title (it names the predictor) and same note the console
    # prints, from the same helpers.
    clipr::write_clip(.clipboard_payload_desc(
      clip_mat,
      clipboard_delim,
      title = title,
      note = note
    ))
    spicy_inform("Linear-model table copied to clipboard.")
    return(invisible(display_df))
  }

  spicy_abort("Unknown output format.", class = "spicy_invalid_input")
}

# The engines carry the coverage in the spanner above the pair, so the
# two bound columns take the bare keys. KEY to KEY: freezing the short
# names is what keeps the flextable `col_keys`, the gt column ids and
# the `th[id="%s"]` CSS selector out of reach of a translated header.
rename_ci_cols_lm <- function(display_df, ci_ll, ci_ul) {
  hit_ll <- names(display_df) == ci_ll
  hit_ul <- names(display_df) == ci_ul
  # The two bounds are born and die together -- one `isTRUE(ci)` guards
  # both -- and both names come from one `ci_level`. Half a rename means
  # the frame and the coverage disagree, which every consumer below
  # would then read through `has_ci` as if nothing were wrong. A bug
  # here is never a user input; same doctrine as the closed column set
  # of `.build_continuous_lm_structured()`.
  if (any(hit_ll) != any(hit_ul)) {
    spicy_abort(
      "Internal: only one confidence-interval bound was renamed.",
      class = "spicy_internal_invariant"
    )
  }
  names(display_df)[hit_ll] <- .LM_KEY_CI_LL
  names(display_df)[hit_ul] <- .LM_KEY_CI_UL
  display_df
}

# The two printed header rows of the flextable / Word, Excel and
# clipboard routes -- the only place those three read a header from, and
# the place the Excel column WIDTHS are measured on. `top` used to be
# `col_keys` verbatim: this is the seam between key and text.
build_header_rows_lm <- function(col_keys, ci_spanner_label, labels = NULL) {
  nc <- length(col_keys)
  top <- .lm_labels(col_keys, labels)
  top[col_keys %in% c(.LM_KEY_CI_LL, .LM_KEY_CI_UL)] <- ci_spanner_label
  bottom <- rep("", nc)
  bottom[col_keys == .LM_KEY_CI_LL] <- .lm_labels(.LM_KEY_CI_LL, labels)
  bottom[col_keys == .LM_KEY_CI_UL] <- .lm_labels(.LM_KEY_CI_UL, labels)
  list(top = top, bottom = bottom)
}

get_delta_label_lm <- function(block) {
  paste0(.LM_KEY_DELTA, " (", block$level[2], " - ", block$level[1], ")")
}

get_test_row_index_lm <- function(block) {
  if (identical(unique(block$predictor_type)[1], "continuous")) {
    return(1L)
  }
  if (nrow(block) == 2L && any(!is.na(block$estimate))) {
    return(which(!is.na(block$estimate))[1])
  }
  1L
}

# Which test the wide table shows, and the degrees of freedom it
# carries. Pure extraction: not one character of the header is decided
# here, so `.lm_test_render()` can format the same parts twice.
#
# Choose the displayed test. For numeric or binary categorical
# predictors, the user-relevant test is the single-coefficient contrast
# (`"t"` or asymptotic `"z"`). For k > 2 categorical predictors, it is
# the multi-coefficient global Wald (`"F"` or asymptotic `"chi2"`). When
# both kinds appear in the block (binary categorical: row 1 has `"F"`,
# row 2 has `"t"`), the single-coef one wins because that is the row the
# wide table actually shows.
.lm_test_parts <- function(block, show_statistic = TRUE, exact = TRUE) {
  if (!isTRUE(show_statistic)) {
    return(NULL)
  }
  test_types <- unique(stats::na.omit(block$test_type))
  if (length(test_types) == 0L) {
    return(NULL)
  }
  single_coef <- intersect(test_types, c("t", "z"))
  multi_coef <- intersect(test_types, c("F", "chi2"))
  chosen <- if (length(single_coef) > 0L) single_coef[1] else multi_coef[1]
  if (length(chosen) == 0L || is.na(chosen)) {
    # An unrecognised test type becomes its own header, verbatim: an API
    # token turned into a visible header. Kept, not fixed here.
    return(list(kind = NA_character_, verbatim = test_types[1]))
  }
  rows_for_chosen <- which(block$test_type == chosen)
  # The df travel even when NOT FINITE. `format_df()` renders an
  # infinite Satterthwaite df as "", which is the whole reason an exact
  # t header can collapse to "t()"; dropping non-finite values here
  # would silently turn that into "t" -- and in BOTH twins at once, so
  # no key/label equality test could ever see it.
  list(
    kind = chosen,
    exact = isTRUE(exact),
    df1 = unique(stats::na.omit(block$df1[rows_for_chosen])),
    df2 = unique(stats::na.omit(block$df2[rows_for_chosen]))
  )
}

# One format body, one glyph table per layer. The parentheses, the
# ", " of the F header and the `formatC(digits = 1)` of a fractional df
# are shared by the frozen key and the displayed header, so the two can
# never drift apart in anything but the glyph itself.
.lm_test_render <- function(parts, glyphs) {
  if (is.null(parts)) {
    return(NULL)
  }
  if (is.na(parts$kind)) {
    return(parts$verbatim)
  }

  # df1 is always integer (number of constraints). df2 may be a
  # fractional Satterthwaite df under cluster-robust inference;
  # show as integer when whole, with a single decimal otherwise
  # (e.g. `t(45.3)` instead of `t(45)`). Asymptotic methods (z,
  # chi2) carry no df2 in the displayed header.
  format_df <- function(d) {
    d <- unname(d)
    if (!is.finite(d)) {
      return("")
    }
    if (abs(d - round(d)) < .Machine$double.eps^0.5) {
      return(as.character(as.integer(round(d))))
    }
    formatC(d, format = "f", digits = 1L)
  }

  glyph <- glyphs[[parts$kind]]
  if (identical(parts$kind, "z")) {
    return(glyph)
  }
  if (identical(parts$kind, "chi2")) {
    if (parts$exact && length(parts$df1) == 1L) {
      return(paste0(glyph, "(", format_df(parts$df1), ")"))
    }
    return(glyph)
  }
  if (identical(parts$kind, "t")) {
    if (parts$exact && length(parts$df2) == 1L) {
      return(paste0(glyph, "(", format_df(parts$df2), ")"))
    }
    return(glyph)
  }
  # "F": the only kind left, and the only one taking both df.
  if (parts$exact && length(parts$df1) == 1L && length(parts$df2) == 1L) {
    return(
      paste0(
        glyph,
        "(",
        format_df(parts$df1),
        ", ",
        format_df(parts$df2),
        ")"
      )
    )
  }
  glyph
}

# The frozen column KEY of the test statistic.
get_test_header_lm <- function(block, show_statistic = TRUE, exact = TRUE) {
  .lm_test_render(
    .lm_test_parts(block, show_statistic, exact),
    .LM_TEST_GLYPHS
  )
}

format_effect_size_header_lm <- function(effect_size = "f2") {
  switch(
    effect_size,
    f2 = "f\u00B2",
    d = "d",
    g = "g",
    omega2 = "\u03C9\u00B2",
    effect_size
  )
}

format_r2_header_lm <- function(r2_type = "r2") {
  switch(
    r2_type,
    r2 = "R\u00B2",
    adj_r2 = "Adj. R\u00B2",
    r2_type
  )
}

get_r2_value_lm <- function(block, r2_type = "r2") {
  switch(
    r2_type,
    r2 = block$r2[1],
    adj_r2 = block$adj_r2[1],
    NA_real_
  )
}
