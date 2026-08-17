# Output dispatch for table_regression() -- Step 11.
#
# Coverage convention:
#   The `# nocov start` / `# nocov end` pragmas in this file mark
#   missing-Suggests guards (the `spicy_abort()` body inside each
#   `if (!spicy_pkg_available(...))` block). On a CI / dev machine
#   with all Suggests installed, those branches are intentionally
#   unreachable; covr correctly excludes them from the denominator.
#   When adding a new optional output engine, wrap its
#   missing-package guard body the same way to keep the convention
#   consistent.
#
# Per dev/table_regression_design.md `output` arg: routes the rendered
# character data.frame to one of nine destinations:
#
#   default     -- printable spicy_regression_table (data.frame subclass);
#                 print() delegates to spicy_print_table().
#   data.frame  -- plain wide data.frame (stripped of spicy classes)
#   long        -- broom-style long tibble (one row per
#                 model_id x term x estimate_type)
#   tinytable   -- tinytable::tt() wrapper
#   gt          -- gt::gt() wrapper
#   flextable   -- flextable::flextable() wrapper
#   excel       -- openxlsx2::write_xlsx() side-effect; returns
#                 invisible(rendered)
#   clipboard   -- clipr::write_clip() side-effect; returns
#                 invisible(rendered)
#   word        -- flextable + officer save_as_docx; returns
#                 invisible(rendered)
#
# All optional-engine outputs guard with requireNamespace() and
# spicy_missing_pkg if absent. File-path outputs validate the path
# upstream (regression_validate.R Phase F).

dispatch_regression_output <- function(
  rendered,
  aligned,
  output = "default",
  excel_path = NULL,
  excel_sheet = NULL,
  clipboard_delim = "\t",
  word_path = NULL,
  word_template = NULL
) {
  output <- match.arg(
    output,
    c(
      "default",
      "data.frame",
      "long",
      "tinytable",
      "gt",
      "flextable",
      "excel",
      "clipboard",
      "word"
    )
  )

  switch(
    output,
    default = output_default(rendered, aligned),
    data.frame = output_data_frame(rendered, aligned),
    long = output_long(aligned),
    tinytable = output_tinytable(rendered),
    gt = output_gt(rendered),
    flextable = output_flextable(rendered),
    excel = output_excel(rendered, excel_path, excel_sheet),
    clipboard = output_clipboard(rendered, clipboard_delim),
    word = output_word(rendered, word_path, word_template),
    # nocov start -- defensive: match.arg() above forbids any other
    # value, so this branch is unreachable in practice.
    spicy_abort(
      sprintf("Unknown output \"%s\".", output),
      class = "spicy_invalid_input"
    )
    # nocov end
  )
}

# ---- default: printable spicy_regression_table ---------------------------

# Provenance attributes promised by the \value section: the model ids
# in table order, and each model's outcome variable (lifted from the
# per-row values carried in the aligned long frame). Shared by the
# default and data.frame outputs so `as.data.frame()` on the default
# object and `output = "data.frame"` carry the same attribute set.
.regression_provenance <- function(aligned) {
  cf <- aligned$coefs_aligned
  model_ids <- aligned$model_ids %||% unique(cf$model_id)
  outcome <- vapply(
    model_ids,
    function(m) {
      o <- if (!is.null(cf) && "outcome" %in% names(cf)) {
        cf$outcome[cf$model_id == m]
      } else {
        character(0)
      }
      o <- o[!is.na(o)]
      if (length(o) > 0L) o[1L] else NA_character_
    },
    character(1),
    USE.NAMES = FALSE
  )
  list(model_ids = model_ids, outcome = outcome)
}

# Carries the analytic data as attributes so broom methods (tidy,
# glance) can read them without re-running the pipeline.
output_default <- function(rendered, aligned) {
  attr(rendered, "spicy_long") <- aligned$coefs_aligned
  attr(rendered, "spicy_fit_stats") <- aligned$fit_stats_aligned
  prov <- .regression_provenance(aligned)
  attr(rendered, "model_ids") <- prov$model_ids
  attr(rendered, "outcome") <- prov$outcome
  class(rendered) <- c("spicy_regression_table", "spicy_table", "data.frame")
  rendered
}

# ---- data.frame: plain wide ----------------------------------------------

output_data_frame <- function(rendered, aligned) {
  out <- as.data.frame(rendered, stringsAsFactors = FALSE)
  attr(out, "title") <- attr(rendered, "title")
  attr(out, "note") <- attr(rendered, "note")
  # Same provenance pair as the default output, so this path and
  # `as.data.frame()` on the default object return identical objects
  # (the documented equivalence in ?as.data.frame.spicy_regression_table).
  prov <- .regression_provenance(aligned)
  attr(out, "model_ids") <- prov$model_ids
  attr(out, "outcome") <- prov$outcome
  out
}

# ---- long: broom-style long format ---------------------------------------

# Returns the aligned long extract with broom-canonical column names,
# as the long-format tibble (`tbl_df`) documented in the \value
# section. Columns:
#   model_id, term, estimate_type, estimate, std.error, conf.low,
#   conf.high, statistic, df, p.value, test_type, is_singular,
#   is_intercept, is_reference, factor_term, factor_level
output_long <- function(aligned) {
  long <- aligned$coefs_aligned
  if (is.null(long)) {
    long <- empty_coefs_aligned()
  }
  # Rename to broom convention
  ren <- c(
    se = "std.error",
    ci_low = "conf.low",
    ci_high = "conf.high",
    p_value = "p.value"
  )
  for (old in names(ren)) {
    if (old %in% names(long)) {
      names(long)[names(long) == old] <- ren[[old]]
    }
  }
  long$order_idx <- NULL
  maybe_as_tibble(long)
}

# ---- spanner helpers (shared by rich-output dispatchers) -----------------

# Strip the "Label: " prefix from spanner-covered column names. Rich
# outputs that draw their own spanner row (gt, flextable, tinytable,
# excel, word) display the model label above the sub-columns, so the
# in-column prefix would be redundant.
#
# The pattern is escaped by `.escape_regex()`, the package's one such
# helper. The local copy this used to call was the same character class
# minus the backslash, so a model label containing one produced a
# pattern that quietly matched nothing.
.strip_spanner_prefix <- function(body, spanners) {
  for (lbl in names(spanners)) {
    idx <- spanners[[lbl]]
    prefix <- paste0(lbl, ": ")
    pat <- paste0("^", .escape_regex(prefix))
    names(body)[idx] <- sub(pat, "", names(body)[idx])
  }
  body
}

# (Removed in v0.13: `.detect_level_rows` / `.trim_level_indent`.
# Their job is now performed at the renderer level: build_structured_body()
# records the indent depth of each row in `body$.indent`, so engines no
# longer need to re-parse the Variable column's whitespace prefix.)

# Parse a bracketed CI cell ("[lo, hi]" or "[lo; hi]" -- the separator
# adapts to `decimal_mark`) into its lo / hi components.
#
# Robust against:
#   * empty / whitespace-only cells  -> returns (\"\", \"\")
#   * en-dash or other non-bracket scalar  -> duplicates value to both
#     so reference-level rows and unavailable cells render uniformly
#     across the new LL / UL columns;
#   * pre-padding inserted by align_ci_strings() (leading / inner /
#     trailing whitespace) -- trimmed in the returned components.
# Build the two-row column-header structure used by `excel`,
# `clipboard`, and `flextable`. Mirrors the table_continuous_lm
# convention: the "top" header row carries the per-column labels
# (B, SE, p, ...) plus a `ci_label` (e.g., "95% CI") repeated over
# each LL / UL pair so it can be merged in rich engines; the
# "bottom" header row carries the LL / UL sub-labels only over the
# CI pairs, empty elsewhere.
#
# When `spanners` is non-null, the per-column labels are stripped
# of their multi-model "Label: " prefix (the Model spanner sits in
# its own row above the column-labels row in rich outputs).
.build_label_rows <- function(
  body,
  ci_spanners,
  spanners = NULL,
  col_meta = NULL
) {
  n <- ncol(body)
  # Prefer `col_meta$display_label` per column when available: it
  # already strips both the "Model X: " prefix and the dedup `.N`
  # suffix that R's data.frame contract appends when two blocks of
  # `show_columns` share a label (e.g. "95% CI" / "95% CI.2").
  # Falls back to the legacy strip-prefix-only path when col_meta
  # isn't supplied (older callers / non-table_regression body).
  top <- if (!is.null(col_meta)) {
    vapply(
      names(body),
      function(nm) {
        .lookup_display_label(nm, col_meta)
      },
      character(1)
    )
  } else if (!is.null(spanners) && length(spanners) > 0L) {
    stripped <- .strip_spanner_prefix(body, spanners)
    names(stripped)
  } else {
    names(body)
  }
  for (cs in ci_spanners) {
    if (length(cs$cols) >= 1L) {
      top[cs$cols] <- cs$label
    }
  }
  bot <- rep("", n)
  for (cs in ci_spanners) {
    if (length(cs$cols) >= 2L) {
      bot[cs$cols[1L]] <- spicy_str("header_ci_ll")
      bot[cs$cols[2L]] <- spicy_str("header_ci_ul")
    }
  }
  list(top = top, bottom = bot)
}

# Resolve the display label for a body (char-format) column name.
# Direct match on `col_meta[[nm]]` handles single-field tokens (B,
# SE, p, AME, t, β, partial_chi2, ...). CI tokens are split LL/UL in
# `col_meta` but merged into one `"95% CI"` / `"95% CI.2"` cell in
# the char body — the direct lookup misses, so we fall back to the
# `: LL` variant which carries the same `display_label` ("95% CI").
.lookup_display_label <- function(nm, col_meta) {
  m <- col_meta[[nm]]
  if (!is.null(m)) {
    return(m$display_label %||% nm)
  }
  m_ll <- col_meta[[.reg_ci_sub_name(nm, .REG_CI_ROLE_LL)]]
  if (!is.null(m_ll)) {
    return(m_ll$display_label %||% nm)
  }
  # The label column is the one body column with no `col_meta` entry:
  # it carries no statistic, so nothing ever wrote one for it. Its
  # header comes from the registry, not from its key -- without this
  # the console, the Excel sheet and the clipboard would each print
  # the frozen key while every other engine printed the label.
  if (identical(nm, .REG_KEY_VARIABLE)) {
    return(spicy_str("header_variable"))
  }
  nm
}

# Warn when `fit_stats_layout = "merged"` is silently ignored by
# the chosen rich output engine (gt, tinytable). Both lack a
# body-cell row-spanning merge primitive: `gt::tab_spanner()`
# covers columns only, and `tinytable::style_tt(colspan = N)`
# emits HTML `colspan` only on header rows. The user passed a
# non-default value, so a classed warning closes the loop rather
# than letting the table render in `"first_col"` mode without
# explanation. Only the engines with native body-cell merge
# (flextable, excel, word via flextable) honour the layout.
.warn_merged_layout_ignored <- function(rendered, engine) {
  fit_stats_layout <- attr(rendered, "fit_stats_layout") %||% "first_col"
  if (!identical(fit_stats_layout, "merged")) {
    return(invisible(NULL))
  }
  spicy_warn(
    c(
      sprintf(
        "`fit_stats_layout = \"merged\"` is not supported by `output = \"%s\"`.",
        engine
      ),
      "i" = "Falling back to `\"first_col\"` layout for the fit-stat rows.",
      "i" = "For merged fit-stat cells, use `output = \"flextable\"`, `\"excel\"`, or `\"word\"`."
    ),
    class = "spicy_ignored_arg"
  )
}

# Compute the cell ranges to merge for each fit-stat row when
# `fit_stats_layout = "merged"`. For each fit-stat row (i.e., body
# rows from `group_sep` to `nrow(body)`), each model's range of
# numeric sub-columns is merged into a single wide cell containing
# the fit-stat value. The value is written into the first sub-
# column of the model (the rendered body's layout); the merge
# extends that cell visually across the model's remaining sub-
# columns.
#
# Returns:
#   list of merge specs. Each spec is list(row, cols) where:
#     - row is the body row index (1-based) of the fit-stat row;
#     - cols is the integer vector of body column indices (with
#       Variable at 1) covered by the model's sub-columns.
#   For single-model output (no `spanners`), every fit-stat row is
#   merged across all data columns. For multi-model output, each
#   fit-stat row produces ONE merge spec per model.
.fit_stat_merge_ranges <- function(body, spanners, group_sep) {
  n_rows <- nrow(body)
  n_cols <- ncol(body)
  if (
    length(group_sep) == 0L ||
      group_sep[1L] < 2L ||
      group_sep[1L] > n_rows ||
      n_cols < 2L
  ) {
    return(list())
  }
  fit_rows <- seq.int(group_sep[1L], n_rows)
  # Each model's column range. Without spanners, the single "model"
  # covers cols 2..n_cols.
  model_ranges <- if (is.null(spanners) || length(spanners) == 0L) {
    list(2:n_cols)
  } else {
    unname(spanners)
  }
  specs <- list()
  for (row in fit_rows) {
    for (cols in model_ranges) {
      if (length(cols) >= 2L) {
        specs[[length(specs) + 1L]] <- list(row = row, cols = cols)
      }
    }
  }
  specs
}

# ---- tinytable -----------------------------------------------------------

output_tinytable <- function(rendered) {
  if (!spicy_pkg_available("tinytable")) {
    # nocov start -- only reachable when 'tinytable' is not installed;
    # CI / dev runs always have it via Suggests.
    spicy_abort(
      c(
        "Output `\"tinytable\"` requires the 'tinytable' package.",
        "i" = "Install it with `install.packages(\"tinytable\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  title <- attr(rendered, "title")
  note <- attr(rendered, "note")
  # Every rule the console draws between blocks: the coefficients /
  # fit-stats divide (`group_sep_rows`) AND the rule that opens a
  # subordinate block -- ordinal `Thresholds:`, mixed-model
  # `Random effects:` (`section_sep_rows`).
  group_sep <- sort(unique(c(
    as.integer(attr(rendered, "group_sep_rows") %||% integer(0)),
    as.integer(attr(rendered, "section_sep_rows") %||% integer(0))
  )))

  # ---- Read structured (typed) body, derive padded display strings ----
  # tinytable's native HTML `align = "d"` does NOT decimal-align: it
  # falls back to `text-align: center` with a uniform per-column
  # precision. To get true decimal alignment we mirror the flextable /
  # table_continuous_lm convention: pre-format each cell to its final
  # display string (en-dash for reference rows, "<.001" for
  # below-threshold p-cells, decimal-formatted numerics elsewhere) via
  # `.format_structured_to_string_body()`, then pad with figure-spaces
  # (U+2007, digit-width) via `.pad_for_decimal_align()` so every cell
  # in a column has the same character width on each side of the
  # decimal mark. Center-aligning the now-uniform-width cells produces
  # visually exact decimal alignment in any font whose figure-space
  # matches digit width (true for all common serif / sans / monospace
  # web fonts).
  struct <- attr(rendered, "structured")
  body <- .format_structured_to_string_body(struct)
  body <- .pad_for_decimal_align(body, struct)
  spanners <- struct$spanners
  ci_spanners <- struct$ci_pairs
  level_rows <- .struct_indent_rows(struct)
  format_spec <- struct$format_spec
  decimal_mark <- format_spec$decimal_mark
  has_model_spanner <- !is.null(spanners) && length(spanners) > 0L
  has_ci_spanner <- length(ci_spanners) > 0L

  # Strip "Model X: " prefix from structured col names for multi-model
  # display (the model spanner above carries the prefix), then rename
  # "95% CI: LL" / "95% CI: UL" sub-cols to their bare role for the
  # column-labels row. Keep the PRE-strip col names so `col_meta`
  # lookups (which key on the original prefixed names) still resolve
  # after `strip_spanner_prefix` rewrites `body`'s colnames.
  struct_col_names_orig <- names(.struct_display_body(struct$body))
  if (has_model_spanner) {
    body <- .strip_spanner_prefix(body, spanners)
  }
  for (cs in ci_spanners) {
    if (length(cs$cols) >= 2L) {
      names(body)[cs$cols[1L]] <- spicy_str("header_ci_ll")
      names(body)[cs$cols[2L]] <- spicy_str("header_ci_ul")
    }
  }
  ci_cols_set <- unlist(lapply(ci_spanners, function(cs) cs$cols))
  body_names_orig <- names(body)
  n_rows <- nrow(body)

  # Factor-level rows carry the console's two-space indent in the
  # Variable cell, and the engine indents them again below
  # (`indent = 1` + `padding-left`). One indentation is the design;
  # two is an artefact of reading a display string as data. The
  # engine's own indent is the one that survives every backend, so
  # the cell text is cleaned here.
  if (length(level_rows) > 0L) {
    body[[1L]][level_rows] <- sub("^[ \t]+", "", body[[1L]][level_rows])
  }

  # Header labels. The per-column label can live in one of two rows:
  #
  #   * WITH a CI pair -- the labels move up into a spanner row so the
  #     column-labels row is free to carry "LL" / "UL" under the
  #     2-wide "95% CI" spanner. Rows, top to bottom: Model spanner
  #     (multi-model only) | per-col + CI spanner | LL/UL.
  #   * WITHOUT a CI pair -- nothing needs the lower row, so the
  #     labels stay in the column-labels row where tinytable puts
  #     them. Rows: Model spanner (multi-model only) | B SE p.
  #
  # Blanking the labels unconditionally (and covering EVERY column
  # with a 1-wide spanner) left a wholly empty header strip under the
  # labels of every table without a CI column, and pushed the header
  # rules one row down: the top rule was drawn under the model names
  # instead of above them. Deriving the layout from `has_ci_spanner`
  # keeps the rule arithmetic below true by construction.
  # (CI columns were already renamed to LL / UL above.)
  col_label <- function(j) {
    # Column 1 is the label column: no `col_meta` entry, header from
    # the registry. Both callers below reach it -- the no-spanner
    # branch as a column label, the spanner branch as a 1-wide group.
    if (j == 1L) {
      return(spicy_str("header_variable"))
    }
    # Look up `col_meta` by the ORIGINAL prefixed col name (e.g.
    # "Step 1: p.2") because `col_meta` is keyed on the structured
    # col names, NOT on the post-strip body colnames. Use the
    # `display_label` (e.g. "p") -- this is what was set at the bare
    # `header_short` level, stripped of both the model prefix and any
    # dedup `.N` suffix.
    struct$col_meta[[struct_col_names_orig[j]]]$display_label %||%
      body_names_orig[j]
  }
  if (has_ci_spanner) {
    for (j in seq_along(body_names_orig)) {
      if (!(j %in% ci_cols_set)) {
        names(body)[j] <- ""
      }
    }
  } else {
    for (j in seq_along(body_names_orig)) {
      names(body)[j] <- col_label(j)
    }
  }
  n_cols <- ncol(body)
  tt <- tinytable::tt(
    body,
    caption = title %||% "",
    notes = if (!is.null(note)) note else NULL
  )
  gspec <- list()
  if (has_ci_spanner) {
    for (j in seq_along(body_names_orig)) {
      if (j %in% ci_cols_set) {
        next
      }
      gspec[[length(gspec) + 1L]] <- j
      names(gspec)[length(gspec)] <- col_label(j)
    }
    for (cs in ci_spanners) {
      gspec[[length(gspec) + 1L]] <- cs$cols
      names(gspec)[length(gspec)] <- cs$label
    }
  }
  # tinytable's `group_tt(j = list)` accesses entries via
  # `idx[[name]]` during sanitisation, so when the named list has
  # duplicate names (e.g. multiple "95% CI" / "B" / "p" / etc. when
  # `show_columns` requests overlapping blocks OR when multi-model
  # repeats per-col labels across models), only the FIRST entry per
  # name reaches the rendered header. Workaround: append zero-width
  # spaces (U+200B) to every duplicate occurrence to make names
  # internally unique while keeping the rendered label identical.
  # Each successive duplicate gets one extra ZWSP. The ZWSPs are
  # stripped from the rendered `table_string` by the finalize
  # callback below.
  if (length(gspec) > 0L) {
    seen <- character(0)
    new_names <- names(gspec)
    for (k in seq_along(new_names)) {
      nm <- new_names[k]
      n_prev <- sum(seen == nm)
      if (n_prev > 0L) {
        new_names[k] <- paste0(nm, strrep("\u200B", n_prev))
      }
      seen <- c(seen, nm)
    }
    names(gspec) <- new_names
    tt <- tinytable::group_tt(tt, j = gspec)
  }
  # Multi-model: stack the Model spanner row on TOP of the per-col
  # spanner row added above. tinytable's `group_tt(j = spanners)`
  # accepts a named list `model_label = col_range` directly when
  # labels are unique (Model spanner labels are always unique --
  # they come from `names(spanners)` which deduplicates by
  # design).
  if (has_model_spanner) {
    tt <- tinytable::group_tt(tt, j = spanners)
  }

  # Mirror the table_continuous_lm pipeline exactly:
  #   1. theme_empty() to clear default cell padding / line styling.
  #   2. format_tt(escape = TRUE) to HTML-escape special characters in
  #      body cells -- in particular, the "<" in below-threshold
  #      tokens like "<.001" would otherwise be parsed as the start of
  #      an HTML tag by the browser. Skipped for LaTeX / typst output
  #      (those backends do their own escaping).
  #      The note is deliberately EXCLUDED (tinytable's "all" minus
  #      "notes"): the table_*() descriptive families never escape it,
  #      and escaping it broke Typst compilation -- the typst escape
  #      set covers "[" / "]", so a note carrying markup such as
  #      #text(8pt)[...] came out as \#text(8pt)\[...\], leaving
  #      text() without a body ("missing argument: body"). Cell escaping
  #      still applies, so real CI brackets stay protected.
  #   3. alignment ("l" on Variable, "c" on numeric cols -- pre-padded
  #      via .pad_for_decimal_align so center-aligning yields true
  #      decimal alignment in any digit-width-figure-space font).
  #   4. spanner-row centring.
  #   5. APA borders LAST.
  tt <- .spicy_tt_bare(tt)
  tt <- tinytable::format_tt(
    tt,
    i = c("colnames", "caption", "~groupi", "groupi", "groupj", "cells"),
    escape = TRUE
  )

  tt <- tinytable::style_tt(tt, j = 1L, align = "l")
  if (n_cols >= 2L) {
    tt <- tinytable::style_tt(tt, j = 2:n_cols, align = "c")
  }
  # Header rows: Variable column left, the rest centred.
  tt <- tinytable::style_tt(tt, i = 0L, j = 1L, align = "l")
  if (n_cols >= 2L) {
    tt <- tinytable::style_tt(tt, i = 0L, j = 2:n_cols, align = "c")
  }
  # Spanner rows. The per-col + CI spanner row exists only when a CI
  # pair does (see the header-labels block above), so the Model row
  # sits one step higher in that case and directly above the column
  # labels otherwise.
  ci_i <- -1L # per-col + CI spanner row
  model_i <- if (has_ci_spanner) -2L else -1L
  if (has_ci_spanner) {
    tt <- tinytable::style_tt(tt, i = ci_i, j = seq_len(n_cols), align = "c")
    tt <- tinytable::style_tt(tt, i = ci_i, j = 1L, align = "l")
  }
  if (has_model_spanner) {
    tt <- tinytable::style_tt(tt, i = model_i, j = seq_len(n_cols), align = "c")
    tt <- tinytable::style_tt(tt, i = model_i, j = 1L, align = "l")
  }

  # APA borders applied last (table_continuous_lm convention).
  # Top rule above the outermost header row.
  top_i <- if (has_model_spanner) {
    model_i
  } else if (has_ci_spanner) {
    ci_i
  } else {
    0L
  }
  tt <- tinytable::style_tt(
    tt,
    i = top_i,
    j = seq_len(n_cols),
    line = "t",
    line_width = 0.06
  )
  # Mid-rule under each model spanner (only over spanned columns).
  if (has_model_spanner) {
    # `line_trim = "lr"` shortens the under-rule by a small amount on
    # each side (booktabs `\cmidrule(lr){...}` convention), so two
    # adjacent model spanners don't have their rules touching at the
    # column boundary -- the small gap is the visual separator
    # between models. Mirrors the console renderer where the
    # cross-column join character already breaks the rule visually.
    for (lbl in names(spanners)) {
      tt <- tinytable::style_tt(
        tt,
        i = model_i,
        j = spanners[[lbl]],
        line = "b",
        line_width = 0.06,
        line_trim = "lr"
      )
    }
  }
  # Mid-rule under each CI spanner (only over LL/UL pairs).
  if (has_ci_spanner) {
    for (cs in ci_spanners) {
      tt <- tinytable::style_tt(
        tt,
        i = ci_i,
        j = cs$cols,
        line = "b",
        line_width = 0.06
      )
    }
  }
  # Sub-rule under column labels.
  tt <- tinytable::style_tt(
    tt,
    i = 0L,
    j = seq_len(n_cols),
    line = "b",
    line_width = 0.06
  )
  for (sep in group_sep) {
    if (sep >= 2L && sep <= nrow(body)) {
      tt <- tinytable::style_tt(
        tt,
        i = sep - 1L,
        j = seq_len(n_cols),
        line = "b",
        line_width = 0.03,
        line_color = "#cccccc"
      )
    }
  }
  if (nrow(body) > 0L) {
    tt <- tinytable::style_tt(
      tt,
      i = nrow(body),
      j = seq_len(n_cols),
      line = "b",
      line_width = 0.06
    )
  }

  # Factor-level rows: indent the Variable cell.
  if (length(level_rows) > 0L) {
    tt <- tinytable::style_tt(
      tt,
      i = level_rows,
      j = 1L,
      indent = 1,
      html_css = "padding-left: 1.4em;"
    )
  }

  # `fit_stats_layout = "merged"` is NOT honoured for tinytable
  # (HTML colspan on body cells; see .warn_merged_layout_ignored).
  .warn_merged_layout_ignored(rendered, "tinytable")

  # ---- Note rendering: strip the rendered `<tfoot>` and wrap the ------
  # table together with the note in an `inline-block` flex sibling
  # (HTML output only). `.spicy_tt_note_div()` / `.spicy_tt_wrap_html()`
  # (R/tt_theme.R) hold the markup and the CSS rationale; the same pair
  # serves table_continuous_lm(), so the two families read alike.
  #
  # We still pass `notes = note` to `tt()` so LaTeX, typst and
  # markdown backends keep their native footnote rendering; this
  # finalize is gated on `x@output == "html"` and only mutates the
  # HTML output. `finalize` receives the S4 object; we mutate
  # `x@table_string` and return `x`. tinytable runs each
  # `lazy_finalize` exactly once after building `table_string`, so
  # no idempotency check is required.
  if (!is.null(note)) {
    note_div <- .spicy_tt_note_div(note)
    tt <- tinytable::style_tt(tt, finalize = function(x) {
      # Strip the U+200B disambiguator we added to duplicate spanner
      # labels so the rendered output is identical regardless of how
      # many CIs (or other identically-labelled spanners) the user
      # requested. See the "tinytable's `group_tt(j = list)`..."
      # comment above for the rationale.
      # U+200B (zero-width space); intToUtf8() keeps this source ASCII.
      zwsp <- intToUtf8(0x200B)
      if (grepl(zwsp, x@table_string, fixed = TRUE)) {
        x@table_string <- gsub(zwsp, "", x@table_string, fixed = TRUE)
      }
      if (identical(x@output, "html")) {
        x@table_string <- .spicy_tt_wrap_html(x@table_string, note_div)
      }
      x
    })
  } else {
    # No note: still register a finalize ONLY to strip the U+200B
    # spanner-label disambiguator from the rendered output.
    tt <- tinytable::style_tt(tt, finalize = function(x) {
      # U+200B (zero-width space); intToUtf8() keeps this source ASCII.
      zwsp <- intToUtf8(0x200B)
      if (grepl(zwsp, x@table_string, fixed = TRUE)) {
        x@table_string <- gsub(zwsp, "", x@table_string, fixed = TRUE)
      }
      x
    })
  }

  tt
}

# ---- gt ------------------------------------------------------------------

output_gt <- function(rendered) {
  if (!spicy_pkg_available("gt")) {
    # nocov start
    spicy_abort(
      c(
        "Output `\"gt\"` requires the 'gt' package.",
        "i" = "Install it with `install.packages(\"gt\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  # `fit_stats_layout = "merged"` is NOT honoured for gt
  # (`tab_spanner()` covers columns, not body rows; see
  # .warn_merged_layout_ignored).
  .warn_merged_layout_ignored(rendered, "gt")

  title <- attr(rendered, "title")
  note <- attr(rendered, "note")
  group_sep <- attr(rendered, "group_sep_rows")

  # ---- Read structured (typed) body, derive padded display strings ----
  # gt's native `cols_align_decimal()` does NOT cleanly decimal-align
  # cells produced by `fmt(fns = drop_leading_zero)` (the APA p-style
  # formatter): the output is treated as an opaque character string
  # that gt's aligner mis-parses, producing artefacts like `. 213` (a
  # space between dot and digits). The same machinery handles
  # `fmt_number()` output fine, but mixing the two in one column breaks
  # alignment.
  #
  # Solution: mirror the flextable / tinytable path -- pre-format every
  # body cell to its final display string via
  # `.format_structured_to_string_body()` (en-dash for reference rows,
  # "<.001" for below-threshold p-values, decimal-formatted numerics
  # elsewhere), then pad with figure-spaces via
  # `.pad_for_decimal_align()` so every cell in a column has the same
  # width on each side of the decimal mark. gt receives an already-
  # padded character data.frame; we then skip `fmt_*`, `text_transform`
  # and `cols_align_decimal` entirely. Figure-space (U+2007) is
  # digit-width in every common web font, so the visual alignment is
  # exact when the cells are centre-aligned by their fixed width.
  struct <- attr(rendered, "structured")
  body <- .format_structured_to_string_body(struct)
  body <- .pad_for_decimal_align(body, struct)
  spanners <- struct$spanners
  ci_spanners <- struct$ci_pairs
  level_rows <- .struct_indent_rows(struct)
  format_spec <- struct$format_spec
  decimal_mark <- format_spec$decimal_mark

  has_model_spanner <- !is.null(spanners) && length(spanners) > 0L
  has_ci_spanner <- length(ci_spanners) > 0L
  orig_names <- names(body)
  n_cols <- ncol(body)
  n_rows <- nrow(body)

  tbl <- gt::gt(body)

  # Match table_continuous_lm header structure: actual column
  # labels are blank for non-CI columns ("LL" / "UL" for CI
  # columns); the displayed per-column labels (B, SE, p, ...) live
  # in a `tab_spanner` row above the column labels. The "95% CI"
  # 2-wide spanner sits next to those per-column spanners. This
  # collapses the visual header to two rows (single-model: spanner
  # row + LL/UL row) instead of three.
  ci_cols_set <- unlist(lapply(ci_spanners, function(cs) cs$cols))
  label_list <- stats::setNames(as.list(rep("", n_cols)), orig_names)
  for (cs in ci_spanners) {
    if (length(cs$cols) >= 2L) {
      label_list[[orig_names[cs$cols[1L]]]] <- spicy_str("header_ci_ll")
      label_list[[orig_names[cs$cols[2L]]]] <- spicy_str("header_ci_ul")
    }
  }
  tbl <- gt::cols_label(tbl, .list = label_list)

  # Build per-column "spanner-as-label" entries for every non-CI
  # numeric column. Each spanner is 1-wide and uses the column's
  # bare label (stripped of any "Model X: " prefix when present).
  # Col 1 ("Variable") also goes here so its label sits in the
  # spanner row, mirroring the flextable / tinytable convention.
  # nocov start -- legacy fallback: build_col_spec() always populates
  # `col_meta$display_label`, so the `strip_prefix(orig_names[j])` else
  # arm below (and this helper's body) is never reached through
  # table_regression(). Kept for structured bodies predating
  # `display_label`.
  strip_prefix <- function(nm) {
    if (has_model_spanner) {
      for (lbl in names(spanners)) {
        prefix <- paste0(lbl, ": ")
        if (startsWith(nm, prefix)) {
          return(substring(nm, nchar(prefix) + 1L))
        }
      }
    }
    nm
  }
  # nocov end
  # The id of column 1's spanner, captured where it is MADE. A
  # `tab_style()` two hundred lines below has to address that spanner to
  # flush its caption left, and used to rebuild the id from
  # `orig_names[1L]`. That worked only while the caption and the key
  # were the same string: the moment column 1's label comes from the
  # registry the two derivations part, and
  # `gt::cells_column_spanners()` aborts on an id no spanner carries --
  # taking `output = "gt"` down with it. `for` does not open a scope in
  # R, so a plain `<-` writes this frame's binding.
  col1_span_id <- NA_character_
  for (j in seq_along(orig_names)) {
    if (j %in% ci_cols_set) {
      next
    } # CI cols get their own spanner
    # Use `col_meta$display_label` for the spanner text (bare,
    # no Model prefix, no dedup `.N` suffix). Falls back to the
    # legacy `strip_prefix` path for structured bodies without
    # `display_label`.
    lbl <- struct$col_meta[[orig_names[j]]]$display_label
    bare <- if (j == 1L) {
      spicy_str("header_variable")
    } else if (!is.null(lbl) && nzchar(lbl)) {
      lbl
    } else {
      strip_prefix(orig_names[j]) # nocov -- legacy: display_label always set
    }
    span_id <- paste0("col_span_", j, "_", make.names(bare))
    if (j == 1L) {
      col1_span_id <- span_id
    }
    tbl <- gt::tab_spanner(
      tbl,
      label = bare,
      columns = orig_names[j],
      id = span_id
    )
  }
  # CI 2-wide spanner ("95% CI") over each LL/UL pair.
  if (has_ci_spanner) {
    for (cs in ci_spanners) {
      tbl <- gt::tab_spanner(
        tbl,
        label = cs$label,
        columns = orig_names[cs$cols],
        id = paste0("ci_span_", paste(cs$cols, collapse = "_"))
      )
    }
  }
  # Outer Model spanner level (multi-model only). The id uses the spanner's
  # POSITION, not make.names(label): distinct labels like "Step 1" and
  # "Step.1" collapse to the same make.names() string, and gt hard-errors on
  # a duplicated spanner id (finding m2). The label still displays verbatim.
  if (has_model_spanner) {
    for (k in seq_along(spanners)) {
      lbl <- names(spanners)[k]
      cols_in_span <- orig_names[spanners[[k]]]
      tbl <- gt::tab_spanner(
        tbl,
        label = lbl,
        columns = cols_in_span,
        id = paste0("model_span_", k)
      )
    }
  }

  # APA borders + clean default styling. tab_options() suppresses
  # gt's default outer / body hlines so only the explicit APA rules
  # remain (top above the spanner / column-labels row, bottom under
  # the last body row, hair rule between coefs and fit-stats).
  # Use the same explicit hex (`#333333`, matching gt's body
  # `.gt_table { color: #333333 }`) on all three horizontal rules
  # (outer top, mid CI bracket, header-body bottom) so they read
  # as a matched typographic set. `currentColor` resolves the same
  # way for cells inside the table but isn't honoured uniformly by
  # the renderer in some viewer / quarto contexts -- the explicit
  # hex sidesteps that.
  rule <- gt::cell_borders(
    sides = "bottom",
    color = "#333333",
    weight = gt::px(1)
  )
  rule_top <- gt::cell_borders(
    sides = "top",
    color = "#333333",
    weight = gt::px(1)
  )
  rule_light <- gt::cell_borders(
    sides = "bottom",
    color = "#cccccc",
    weight = gt::px(1)
  )
  tbl <- gt::tab_options(
    tbl,
    table.border.top.width = gt::px(0),
    table.border.bottom.width = gt::px(0),
    table_body.border.top.width = gt::px(0),
    table_body.border.bottom.width = gt::px(0),
    table_body.hlines.color = "transparent",
    # `column_labels.border.top` applies to BOTH `.gt_col_headings`
    # rows in gt's HTML (spanner row + column-labels row), so it
    # would draw a continuous line between them in addition to the
    # outer top rule. We instead disable it here and add the outer
    # top rule via an explicit `tab_style(..., cells_column_spanners)`
    # call below, which targets the spanner row alone.
    column_labels.border.top.width = gt::px(0),
    column_labels.border.lr.color = "transparent",
    # Header-body bottom rule: match the outer top rule (1px #333333)
    # so both horizontal rules read as a matched pair. Setting this
    # via tab_options writes the value into gt's `.gt_col_headings`
    # CSS rule (applies to BOTH header rows -- the spanner row is
    # zeroed out by the CSS injection in the post-process so only
    # the column-labels row keeps the rule). This avoids relying on
    # cell-level CSS injection that may not win the border-collapse
    # tiebreaker against the body cells' default transparent
    # `border-top`.
    column_labels.border.bottom.width = gt::px(1),
    column_labels.border.bottom.color = "#333333",
    # APA title: match the body's font size so caption / title /
    # note / body all read in the same typographic register
    # (the gt default `gt_title { font-size: 125% }` makes the
    # caption visibly larger than the body).
    heading.title.font.size = "100%",
    heading.title.font.weight = "normal"
  )
  # Outer top rule above the top-most header row only. In
  # multi-model mode, gt builds TWO spanner levels (model_span_*
  # over the per-model col-ranges, col_span_* over each numeric
  # column). `cells_column_spanners(spanners = everything())`
  # would draw `rule_top` on BOTH levels -- the top edge of the
  # model row (correct outer rule) AND the top edge of the per-col
  # row (extra line above "Variable"). Restrict to the outermost
  # level via explicit spanner IDs.
  if (has_model_spanner) {
    model_span_ids <- paste0("model_span_", seq_along(spanners))
    # Top rule above the Model row.
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_spanners(spanners = model_span_ids)
    )
    # NOTE: we DON'T apply `rule` (bottom border) here via
    # `cells_column_spanners`: gt expands the styling to ALL `<th>`
    # cells in the same spanner row -- including the empty col 1
    # above "Variable" -- producing an unwanted hairline. Instead
    # the line under each Model spanner is drawn via a CSS rule
    # injected in `.spicy_gt_html_postprocess` that targets only
    # `th[id^="model_span_"]`.
  } else {
    # Single-model: the col_span_*'s ARE the outermost level.
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_spanners(spanners = gt::everything())
    )
  }
  # Mid-rule under the per-col + CI spanner row: APA convention is to
  # draw this rule ONLY beneath the CI bracket (separates "95% CI"
  # from its "LL" / "UL" sub-labels), not beneath the 1-wide B / SE /
  # p / Variable spanners. We apply `rule_top` only to the LL / UL
  # column-labels cells (= bottom side of the spanner row above
  # them).
  if (length(ci_cols_set) > 0L) {
    tbl <- gt::tab_style(
      tbl,
      style = rule_top,
      locations = gt::cells_column_labels(
        columns = orig_names[ci_cols_set]
      )
    )
  }
  # Header-body bottom rule: apply at the cell level on ALL
  # column-labels cells (an *inline* style, the highest-specificity
  # CSS tier) so it deterministically wins the border-collapse
  # tiebreaker against the body cells' default
  # `.gt_row { border-top: 1px solid transparent }`. Belt + braces
  # with the `column_labels.border.bottom.*` `tab_options` set
  # above.
  tbl <- gt::tab_style(
    tbl,
    style = rule,
    locations = gt::cells_column_labels(columns = gt::everything())
  )
  if (n_rows > 0L) {
    tbl <- gt::tab_style(
      tbl,
      style = rule,
      locations = gt::cells_body(rows = n_rows)
    )
  }
  if (
    length(group_sep) >= 1L && group_sep[1L] >= 2L && group_sep[1L] <= n_rows
  ) {
    tbl <- gt::tab_style(
      tbl,
      style = rule_light,
      locations = gt::cells_body(rows = group_sep[1L] - 1L)
    )
  }

  # Column-label alignment + per-column body alignment. The
  # "Variable" header is LEFT-aligned to match its left-aligned
  # body (modern data-viz convention: header alignment follows
  # body alignment for left-text columns); numeric column labels
  # stay centred above their decimal-aligned columns.
  tbl <- gt::tab_style(
    tbl,
    style = gt::cell_text(align = "left", weight = "normal"),
    locations = gt::cells_column_labels(columns = orig_names[1L])
  )
  if (n_cols >= 2L) {
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(align = "center", weight = "normal"),
      locations = gt::cells_column_labels(columns = orig_names[-1L])
    )
  }
  # Centre all spanner-row labels (B / SE / 95% CI / p / Model X),
  # then override the Variable spanner (col 1) to be flush-left so
  # the "Variable" caption aligns with the left-aligned body of
  # the same column.
  tbl <- gt::tab_style(
    tbl,
    style = gt::cell_text(align = "center", weight = "normal"),
    locations = gt::cells_column_spanners(spanners = gt::everything())
  )
  tbl <- gt::tab_style(
    tbl,
    style = gt::cell_text(align = "left", weight = "normal"),
    locations = gt::cells_column_spanners(spanners = col1_span_id)
  )
  # Belt-and-braces CI bracket: in addition to the inline
  # `border-top: 1px #333333` applied to the LL / UL column-labels
  # cells via `rule_top` above, also stamp an inline
  # `border-bottom: 1px #333333` on the CI spanner cell itself.
  # Some renderers don't always honour the LL/UL top vs the empty
  # spanner-cell bottom tie in border-collapse priority -- having
  # both sides of the shared edge marked guarantees the line.
  if (has_ci_spanner) {
    for (cs in ci_spanners) {
      tbl <- gt::tab_style(
        tbl,
        style = rule,
        locations = gt::cells_column_spanners(
          spanners = paste0("ci_span_", paste(cs$cols, collapse = "_"))
        )
      )
    }
  }
  tbl <- gt::cols_align(tbl, align = "left", columns = orig_names[1L])
  if (n_cols >= 2L) {
    # Numeric body columns are already character-padded (via
    # `.format_structured_to_string_body()` + `.pad_for_decimal_align()`
    # above) so every cell in a column has the same character width on
    # each side of the decimal mark. Centre-aligning them lines up the
    # decimal points exactly -- no per-cell `fmt_number()` /
    # `cols_align_decimal()` needed (and avoids gt's alignment bug
    # with `fmt(fns = ...)` output that displayed APA p-values as
    # `. 213`).
    tbl <- gt::cols_align(tbl, align = "center", columns = orig_names[-1L])
  }
  # Factor-level rows: indent the Variable cell.
  if (length(level_rows) > 0L) {
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(indent = gt::px(20)),
      locations = gt::cells_body(columns = orig_names[1L], rows = level_rows)
    )
  }

  if (!is.null(title) && nzchar(title)) {
    tbl <- gt::tab_header(tbl, title = title)
    # APA Manual 7 Section 7.10-Section 7.11: caption is flush-left. gt's default
    # centers the title; override via `cell_text(align = "left")` on
    # `cells_title("title")`. Also drop the title's bottom border --
    # the spanner row below already carries the outer top rule via
    # `gt_columns_top_border`, so keeping the title's
    # `gt_bottom_border` would render a redundant double line.
    tbl <- gt::tab_style(
      tbl,
      style = list(
        gt::cell_text(align = "left"),
        gt::cell_borders(
          sides = "bottom",
          color = "transparent",
          weight = gt::px(0)
        )
      ),
      locations = gt::cells_title(groups = "title")
    )
  }
  # The note is attached TWICE, on purpose.
  #
  # 1. `gt::tab_source_note()` puts it on the gt object itself, so it
  #    survives every route to a rendered table: `gt::gtsave()`,
  #    `gt::as_raw_html()`, `as_latex()`, `as_word()`, a
  #    non-interactive `print()`. Without it those deliverables shipped
  #    a table stripped of the disclosure the console prints (model
  #    family, standard errors, star legend).
  # 2. `attr(tbl, "spicy_note")` + the `spicy_gt` sub-class drive the
  #    HTML display path below, which RE-STYLES the note: gt renders a
  #    source note inside `<tfoot>` as a `<td colspan="N">` cell whose
  #    max-content widens the whole table in narrow viewports (same
  #    pathology as `output_tinytable` / `output_flextable`), so
  #    `print.spicy_gt()` / `knit_print.spicy_gt()` move it to a
  #    `<div>` sibling outside the table.
  if (!is.null(note) && nzchar(note)) {
    tbl <- gt::tab_source_note(
      tbl,
      source_note = gsub("\n", " ", note, fixed = TRUE)
    )
    attr(tbl, "spicy_note") <- note
  }
  class(tbl) <- c("spicy_gt", class(tbl))
  tbl
}

# Remove the `<tfoot>` row that carries OUR source note (matched on
# its text, so a source note the user added with their own
# `gt::tab_source_note()` call survives), leaving an emptied `<tfoot>`
# behind only if it had nothing else in it. Used by the HTML display
# path, which renders the same note as a styled `<div>` outside the
# table.
.spicy_gt_drop_source_note <- function(html_str, note_one_line) {
  # (?s): the row spans several lines in gt's rendered HTML.
  pat <- "(?s)<tr class=\"gt_sourcenotes\">.*?</tr>"
  m <- gregexpr(pat, html_str, perl = TRUE)
  blocks <- regmatches(html_str, m)[[1L]]
  if (length(blocks) == 0L) {
    return(html_str)
  }
  unescape <- function(s) {
    s <- gsub("&lt;", "<", s, fixed = TRUE)
    s <- gsub("&gt;", ">", s, fixed = TRUE)
    gsub("&amp;", "&", s)
  }
  target <- trimws(note_one_line)
  for (b in blocks) {
    txt <- trimws(unescape(gsub("<[^>]*>", "", b)))
    if (identical(txt, target)) {
      html_str <- sub(b, "", html_str, fixed = TRUE)
    }
  }
  sub("<tfoot>\\s*</tfoot>", "", html_str)
}

# Wrap rendered gt HTML so the title (already inside the `<table>`
# as a heading row) is preserved but the source note is injected as
# a `<div>` sibling immediately after `</table>`. The wrapping +
# note-styling mirror `output_tinytable` so the visual reading of
# the note matches across engines.
.spicy_gt_html_postprocess <- function(html_str, note) {
  if (is.null(note) || !nzchar(note)) {
    return(html_str)
  }
  esc <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    s <- gsub(">", "&gt;", s, fixed = TRUE)
    s
  }
  note_one_line <- gsub("\n", " ", note, fixed = TRUE)
  # The same note also sits on the gt object as a native source note
  # (so `gtsave()` / `as_raw_html()` / a non-interactive `print()` keep
  # it); this display path moves it OUT of the table, so drop the
  # `<tfoot>` row carrying it. Source notes the user added themselves
  # are left alone -- only the row whose text is ours is removed.
  html_str <- .spicy_gt_drop_source_note(html_str, note_one_line)
  note_html <- esc(note_one_line)
  note_html <- sub(
    .note_prefix_pattern(),
    paste0("<em>", spicy_str("note_prefix_emphasis"), "</em>"),
    note_html
  )
  note_div <- paste0(
    "<div class=\"spicy-gt-note\" style=\"",
    "width: min-content; min-width: 100%; box-sizing: border-box; ",
    "padding: 0.5rem 0.5rem 0.2rem 0.5rem; ",
    # Match gt's `#<id> table { font-family: ... }` rule by listing
    # the same fallback stack. gt scopes the rule to `table`
    # elements only, so a `<div>` sibling otherwise picks up the
    # page default and reads in a different typeface.
    "font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, ",
    "sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', ",
    "'Segoe UI Symbol', 'Noto Color Emoji'; ",
    "font-size: 0.9em; line-height: 1.25; ",
    "color: #333333; text-align: left;\">",
    note_html,
    "</div>"
  )
  open_outer <- paste0(
    "<div class=\"spicy-gt-outer\" ",
    "style=\"text-align: center;\">"
  )
  open_inner <- paste0(
    "<div class=\"spicy-gt-wrap\" style=\"",
    "display: inline-block; max-width: 100%; ",
    "text-align: left; vertical-align: top;\">"
  )
  # gt's default theme draws two unwanted lines inside the header:
  #   * `.gt_col_headings { border-bottom: 2px solid #D3D3D3 }` is
  #     applied to BOTH header rows (the spanner row carries class
  #     `gt_col_headings gt_spanner_row`, the column-labels row
  #     carries `gt_col_headings`), so the rule draws a 2px grey
  #     line under the spanner row in addition to the bottom of the
  #     header.
  #   * `.gt_column_spanner { border-bottom: 2px solid #D3D3D3 }` is
  #     applied inside each spanner cell (B / SE / p / Variable /
  #     95% CI), adding its own underline.
  # We neutralise both with `!important` (gt's selectors carry an id
  # which outranks a class-only selector) and at the same time
  # match the column-labels-row bottom rule to the outer top rule
  # (1px solid #333333) so they read as a matched pair. The CSS is
  # scoped to `.spicy-gt-outer` so unrelated gt tables elsewhere
  # on the page keep their stock styling.
  spanner_css <- paste0(
    "<style>",
    # Mid rule under each Model spanner in multi-model mode. The
    # rule is drawn via a `::after` pseudo-element (rather than a
    # plain `border-bottom`) so each rule is INSET inside the
    # spanner cell with a small horizontal gap on each side --
    # adjacent models therefore have visibly separated rules, the
    # booktabs `\cmidrule(lr){...}` convention. Without the inset
    # the rules would touch at the column boundaries and the
    # multi-model header would read as one continuous line.
    # Scoped via `[id^=\"model_span_\"]` so only Model spanner
    # cells (which have an id) are styled; col 1 above "Variable"
    # has no id and stays untouched.
    ".spicy-gt-outer th[id^=\"model_span_\"] { ",
    "position: relative; }",
    ".spicy-gt-outer th[id^=\"model_span_\"]::after { ",
    "content: \"\"; position: absolute; ",
    "left: 0.5em; right: 0.5em; bottom: 0; ",
    "border-bottom: 1px solid #333333; }",
    # gt emits `.gt_spanner_row { border-bottom-style: hidden }`
    # which, per CSS 2.1 Section 17.6.2.1, *suppresses any conflicting
    # border at this edge* -- including the inline `border-top` we
    # put on the LL/UL cells to draw the CI bracket. Override both
    # the style and width to `none` / 0 so the spanner row carries
    # no border-bottom contribution at all, leaving the LL/UL
    # cell-level top border (and the CI spanner cell's bottom) free
    # to render.
    ".spicy-gt-outer .gt_col_headings.gt_spanner_row, ",
    ".spicy-gt-outer .gt_spanner_row { ",
    "border-bottom-style: none !important; ",
    "border-bottom-width: 0 !important; }",
    # Mid-rule under the column-labels row (= bottom of header,
    # above body). Applied at the CELL level (`.gt_col_heading`),
    # AND we simultaneously neutralise the body cells'
    # `.gt_row { border-top: 1px solid transparent }` -- without
    # this, both sides are cell-level borders with the same width
    # and style, and the browser tie-breaker picks the later
    # element (= body row, transparent), making our colour
    # invisible. Setting body's `border-top: 0` removes the
    # competing border entirely so our #333333 always shows.
    ".spicy-gt-outer .gt_col_heading { ",
    "border-bottom: 1px solid #333333 !important; }",
    ".spicy-gt-outer .gt_table tbody .gt_row { ",
    "border-top-width: 0 !important; }",
    # Remove the 2px grey underline that gt draws inside every
    # `<div class=\"gt_column_spanner\">`. The CI bracket line stays
    # because it lives on the LL/UL cells' inline `border-top`.
    ".spicy-gt-outer .gt_column_spanner { ",
    "border-bottom-width: 0 !important; ",
    "border-bottom-color: transparent !important; }",
    "</style>"
  )
  close_both <- "</div></div>"
  html_str <- sub(
    "<table ",
    paste0(open_outer, open_inner, spanner_css, "<table "),
    html_str,
    fixed = TRUE
  )
  html_str <- sub(
    "</table>",
    paste0("</table>", note_div, close_both),
    html_str,
    fixed = TRUE
  )
  html_str
}

#' Print method for spicy-tagged gt tables
#'
#' @description
#' Prints a `spicy_gt` object -- the [gt::gt()] table returned by
#' `output = "gt"`, tagged so the table note keeps its styling in
#' interactive HTML display. Every gt verb works on the tagged object
#' directly; printing delegates to gt's own rendering.
#'
#' @param x A `spicy_gt` object.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `NULL` (HTML display path) or the result
#'   of gt's own print method.
#'
#' @seealso [table_regression()]
#' @keywords internal
#' @exportS3Method print spicy_gt
print.spicy_gt <- function(x, ...) {
  note <- attr(x, "spicy_note")
  class(x) <- setdiff(class(x), "spicy_gt")
  if (
    interactive() &&
      !isTRUE(getOption("knitr.in.progress")) &&
      requireNamespace("htmltools", quietly = TRUE)
  ) {
    h_str <- as.character(gt::as_raw_html(x, inline_css = FALSE))
    h_str <- .spicy_gt_html_postprocess(h_str, note)
    print(htmltools::browsable(htmltools::HTML(h_str)))
    return(invisible(NULL))
  }
  NextMethod()
}

#' @exportS3Method knitr::knit_print spicy_gt
knit_print.spicy_gt <- function(x, ...) {
  note <- attr(x, "spicy_note")
  class(x) <- setdiff(class(x), "spicy_gt")
  # Non-HTML knit targets (Quarto / R Markdown -> docx, pptx, pdf):
  # pandoc DROPS raw HTML, so the as_raw_html path below rendered an
  # empty document (dev/quarto_word_rendering_spec.md). Regression
  # tables already carry the note as a native source note (see
  # output_gt), so nothing to do; the descriptive builders attach the
  # attribute only (.spicy_gt_attach_note), so add it here -- where
  # the viewport-width concern of the HTML path does not apply --
  # before delegating to gt's own format-aware rendering. Outside a
  # knit (pandoc target NULL) keep the historical HTML path.
  pandoc_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (!is.null(pandoc_to) && !isTRUE(knitr::is_html_output())) {
    has_source_note <- length(x[["_source_notes"]]) > 0L
    if (!is.null(note) && nzchar(note) && !has_source_note) {
      x <- gt::tab_source_note(
        x,
        source_note = gsub("\n", " ", note, fixed = TRUE)
      )
    }
    return(knitr::knit_print(x, ...))
  }
  h_str <- as.character(gt::as_raw_html(x, inline_css = FALSE))
  h_str <- .spicy_gt_html_postprocess(h_str, note)
  knitr::asis_output(h_str)
}

# ---- flextable -----------------------------------------------------------

output_flextable <- function(rendered) {
  if (!spicy_pkg_available("flextable")) {
    # nocov start
    spicy_abort(
      c(
        "Output `\"flextable\"` requires the 'flextable' package.",
        "i" = "Install it with `install.packages(\"flextable\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  # ---- Read structured (typed) body, derive display strings ------------
  # Flextable's native formatters (colformat_double, set_formatter)
  # don't compose cleanly with per-cell precision + APA p-style +
  # below-threshold overrides + reference-row en-dash. We pre-format
  # the structured body to a CHAR body via .format_structured_to_string_body()
  # then apply figure-space decimal-alignment padding via
  # `.pad_for_decimal_align()` (flextable has no native decimal-align
  # primitive, so we emulate gt's `cols_align_decimal()` manually).
  struct <- attr(rendered, "structured")
  body <- .format_structured_to_string_body(struct)
  body <- .pad_for_decimal_align(body, struct)
  spanners <- struct$spanners
  ci_spanners <- struct$ci_pairs
  level_rows <- .struct_indent_rows(struct)
  # `group_sep` keeps the RAW attribute: its first element is where
  # the fit-stats block starts, which `.fit_stat_merge_ranges()` reads
  # below. `rule_rows` is every rule the console draws between blocks
  # -- the coefficients / fit-stats divide AND the rule that opens a
  # subordinate block (ordinal `Thresholds:`, mixed-model
  # `Random effects:`, carried by `section_sep_rows`).
  group_sep <- as.integer(attr(rendered, "group_sep_rows") %||% integer(0))
  rule_rows <- sort(unique(c(
    group_sep,
    as.integer(attr(rendered, "section_sep_rows") %||% integer(0))
  )))

  has_model_spanner <- !is.null(spanners) && length(spanners) > 0L
  has_ci_spanner <- length(ci_spanners) > 0L

  # Factor-level rows carry the console's two-space indent in the
  # Variable cell, and the engine indents them again below
  # (`padding.left = 20`). One indentation is the design; two is an
  # artefact of reading a display string as data -- and Word keeps
  # literal spaces, so the .docx showed both. The engine's own indent
  # is the one that survives every backend, so the cell text is
  # cleaned here (same rule as `output_tinytable()`).
  if (length(level_rows) > 0L) {
    body[[1L]][level_rows] <- sub("^[ \t]+", "", body[[1L]][level_rows])
  }

  orig_names <- names(body)
  n_cols <- ncol(body)
  # Pin the flextable default font BEFORE constructing the table so
  # the caption (set_caption below) picks up Calibri instead of the
  # docx / HTML stock default (which is Arial in flextable's theme).
  # `set_flextable_defaults()` mutates global state; we wrap the
  # entire body of this function in a restore-on-exit so other code
  # in the user session is unaffected.
  prev_defaults <- flextable::get_flextable_defaults()
  on.exit(do.call(flextable::set_flextable_defaults, prev_defaults), add = TRUE)
  flextable::set_flextable_defaults(font.family = "Calibri")

  ft <- flextable::flextable(body)

  # ---- Header structure (matches gt / tinytable convention) -------------
  # The per-column label ("B", "SE", "p") can live in one of two rows:
  #
  #   * WITH a CI pair -- the labels move up into their own spanner
  #     row (1-wide spanners, the gt::tab_spanner() pattern of
  #     output_gt) so the column-labels row is free to carry the bare
  #     "LL" / "UL" under the 2-wide "95% CI" spanner. Rows, top to
  #     bottom: Model spanner (multi-model only) | per-col + CI
  #     spanner | LL/UL.
  #   * WITHOUT a CI pair -- nothing needs the lower row, so the
  #     labels stay in the column-labels row flextable already has.
  #     Rows: Model spanner (multi-model only) | Variable B SE p.
  #
  # Blanking the labels unconditionally left a wholly empty header
  # strip under the labels of every table without a CI column (in the
  # HTML and in the .docx alike) and pushed the rules one row down.
  # Deriving the layout from `has_ci_spanner` keeps the rule
  # arithmetic below true by construction -- same rule as
  # `output_tinytable()`.
  ci_col_set <- if (has_ci_spanner) {
    unlist(lapply(ci_spanners, function(cs) cs$cols))
  } else {
    integer(0)
  }

  # Helper to strip the "Model X: " prefix from a structured col name.
  # nocov start -- legacy fallback: build_col_spec() always populates
  # `col_meta$display_label`, so the `strip_model_prefix(orig_names[j])`
  # else arm further below (and this helper's body) is never reached
  # through table_regression(). Kept for bodies predating display_label.
  strip_model_prefix <- function(name) {
    if (!has_model_spanner) {
      return(name)
    }
    for (lbl in names(spanners)) {
      prefix <- paste0(lbl, ": ")
      if (startsWith(name, prefix)) {
        return(substring(name, nchar(prefix) + 1L))
      }
    }
    name
  }
  # nocov end

  # Bare per-column label ("B", "SE", "p"): `col_meta$display_label`
  # (no Model prefix, no dedup `.N` suffix) when available, with the
  # legacy `strip_model_prefix()` path as a fallback so structured
  # bodies that pre-date `display_label` still render.
  col_label <- function(j) {
    lbl <- struct$col_meta[[orig_names[j]]]$display_label
    if (!is.null(lbl) && nzchar(lbl)) {
      lbl
    } else {
      strip_model_prefix(orig_names[j]) # nocov -- display_label always set
    }
  }

  # Column-labels row (bottom-most header row). With a CI pair it
  # carries only the bare "LL" / "UL" and is blank elsewhere (the
  # labels live in the spanner row built below); without one it
  # carries the labels themselves.
  display_labels <- as.list(orig_names)
  names(display_labels) <- orig_names
  display_labels[[orig_names[1L]]] <- if (has_ci_spanner) {
    ""
  } else {
    spicy_str("header_variable")
  }
  for (j in seq_len(n_cols)) {
    if (j == 1L) {
      next
    }
    if (!has_ci_spanner) {
      display_labels[[orig_names[j]]] <- col_label(j)
    } else if (j %in% ci_col_set) {
      for (cs in ci_spanners) {
        if (identical(cs$cols[1L], j)) {
          display_labels[[orig_names[j]]] <- spicy_str("header_ci_ll")
          break
        }
        if (identical(cs$cols[2L], j)) {
          display_labels[[orig_names[j]]] <- spicy_str("header_ci_ul")
          break
        }
      }
    } else {
      display_labels[[orig_names[j]]] <- ""
    }
  }
  ft <- do.call(flextable::set_header_labels, c(list(x = ft), display_labels))

  # Helper: compress contiguous cells into spanning ones (per-col
  # labels -> add_header_row(values, colwidths)). A run continues only
  # while BOTH the label and the run KEY are unchanged; the key names
  # the block a column belongs to (its model, its CI pair). Without
  # it, two neighbouring models that display the same label merged
  # into ONE header cell straddling the model spanner above -- "B" and
  # "B" became a single 2-wide "B", and two distinct CI pairs a single
  # 4-wide "95% CI". The console and the other engines print one label
  # per model.
  build_run_spec <- function(label_at_col, key_at_col) {
    n <- length(label_at_col)
    values <- character(0)
    widths <- integer(0)
    i <- 1L
    while (i <= n) {
      lbl <- label_at_col[i]
      key <- key_at_col[i]
      j <- i
      while (
        j < n && label_at_col[j + 1L] == lbl && key_at_col[j + 1L] == key
      ) {
        j <- j + 1L
      }
      values <- c(values, lbl)
      widths <- c(widths, j - i + 1L)
      i <- j + 1L
    }
    list(values = values, colwidths = widths)
  }

  if (has_ci_spanner) {
    # Per-col + CI spanner row: "Variable" on col 1, the column's bare
    # token ("B" / "SE" / "p" / ...) on non-CI numeric cols, the CI
    # label ("95% CI") on each LL/UL pair. Only a CI pair may span:
    # every other column keys on its own index.
    sub_label_at_col <- character(n_cols)
    sub_key_at_col <- paste0("col", seq_len(n_cols))
    for (j in seq_len(n_cols)) {
      if (j == 1L) {
        sub_label_at_col[j] <- spicy_str("header_variable")
      } else if (j %in% ci_col_set) {
        for (k in seq_along(ci_spanners)) {
          if (j %in% ci_spanners[[k]]$cols) {
            sub_label_at_col[j] <- ci_spanners[[k]]$label
            sub_key_at_col[j] <- paste0("ci", k)
            break
          }
        }
      } else {
        sub_label_at_col[j] <- col_label(j)
      }
    }
    rs <- build_run_spec(sub_label_at_col, sub_key_at_col)
    ft <- flextable::add_header_row(
      ft,
      top = TRUE,
      values = rs$values,
      colwidths = rs$colwidths
    )
  }

  if (has_model_spanner) {
    model_labels_at_col <- rep("", n_cols)
    model_key_at_col <- rep("gap", n_cols)
    for (k in seq_along(spanners)) {
      model_labels_at_col[spanners[[k]]] <- names(spanners)[k]
      model_key_at_col[spanners[[k]]] <- paste0("model", k)
    }
    rs <- build_run_spec(model_labels_at_col, model_key_at_col)
    ft <- flextable::add_header_row(
      ft,
      top = TRUE,
      values = rs$values,
      colwidths = rs$colwidths
    )
  }

  # APA borders. The header has up to 3 rows -- model spanner row (1,
  # multi-model only), per-col + CI spanner row (CI tables only),
  # column-labels row -- and the last one is always the column-labels
  # row, the target of `hline_bottom(part = "header")`.
  bd <- spicy_fp_border(color = "black", width = 1)
  bd_light <- spicy_fp_border(color = "#cccccc", width = 0.5)
  ft <- flextable::hline_top(ft, part = "header", border = bd)
  bd_none <- spicy_fp_border(color = "white", width = 0)
  if (has_model_spanner) {
    # First clear flextable's default 1.5pt grey bottom across the
    # WHOLE model spanner row (col 1 -- the empty cell above
    # "Variable" -- otherwise inherits that grey hairline and shows
    # an extra line right above the "Variable" label). Then draw
    # the black 1pt mid-rule under each model spanner col-range.
    ft <- flextable::hline(
      ft,
      i = 1L,
      j = seq_len(n_cols),
      part = "header",
      border = bd_none
    )
    for (k in seq_along(spanners)) {
      ft <- flextable::hline(
        ft,
        i = 1L,
        j = spanners[[k]],
        part = "header",
        border = bd
      )
    }
  }
  # Bottom border of the per-col + CI spanner row (that row exists
  # only when a CI pair does). First clear flextable's default grey
  # 1.5pt border across the whole row, THEN draw the black 1pt rule
  # only under CI spanner pair(s). Without the clearing pass, the
  # default grey bleeds across the row and overshoots the CI bracket
  # visually.
  if (has_ci_spanner) {
    sub_row_idx <- if (has_model_spanner) 2L else 1L
    ft <- flextable::hline(
      ft,
      i = sub_row_idx,
      j = seq_len(n_cols),
      part = "header",
      border = bd_none
    )
    for (cs in ci_spanners) {
      ft <- flextable::hline(
        ft,
        i = sub_row_idx,
        j = cs$cols,
        part = "header",
        border = bd
      )
    }
  }
  ft <- flextable::hline_bottom(ft, part = "header", border = bd)
  # Every rule the console draws between blocks, not only the first.
  for (sep in rule_rows) {
    if (sep >= 2L && sep <= nrow(body)) {
      ft <- flextable::hline(
        ft,
        i = sep - 1L,
        part = "body",
        border = bd_light
      )
    }
  }
  if (nrow(body) > 0L) {
    ft <- flextable::hline_bottom(ft, part = "body", border = bd)
  }

  # Alignment + font (single font across the table).
  #
  # flextable has no native decimal-alignment primitive and the default
  # body font (Calibri) has PROPORTIONAL digits in Word / DOCX -- "1"
  # is narrower than "0", so right-aligning a numeric column does NOT
  # decimal-align across rows. Two paths:
  #   (a) Monospace font (Consolas) on numeric body cells: gives true
  #       decimal alignment via uniform character width, but mixes
  #       TWO fonts in the table (Calibri for Variable + header, Consolas
  #       for numeric body) -- visually distracting.
  #   (b) CENTRE numeric cells in the default Calibri: keeps a single
  #       font everywhere, sacrifices strict decimal alignment. For
  #       columns with uniform precision (the common case after CI is
  #       split into LL/UL columns of consistent decimals), centring
  #       still LOOKS decimal-aligned because every cell has the same
  #       character width.
  # We pick (b): single font wins on readability + APA convention. Users
  # who need true decimal alignment in DOCX should export via Excel
  # (native numerics + numfmt) or PDF (via tinytable / siunitx).
  ft <- flextable::align(ft, part = "header", align = "center")
  ft <- flextable::align(ft, j = 1L, part = "header", align = "left")
  ft <- flextable::align(ft, j = 1L, part = "body", align = "left")
  if (n_cols >= 2L) {
    numeric_j <- 2:n_cols
    ft <- flextable::align(ft, j = numeric_j, part = "body", align = "center")
  }
  # Factor-level row indentation.
  if (length(level_rows) > 0L) {
    ft <- flextable::padding(
      ft,
      i = level_rows,
      j = 1L,
      part = "body",
      padding.left = 20
    )
  }

  # fit_stats_layout = "merged": merge each model's numeric sub-
  # columns into one wide centred cell per fit-stat row (Stata
  # esttab / Econometrica convention).
  fit_stats_layout <- attr(rendered, "fit_stats_layout") %||% "first_col"
  if (identical(fit_stats_layout, "merged")) {
    specs <- .fit_stat_merge_ranges(body, spanners, group_sep)
    for (sp in specs) {
      ft <- flextable::merge_at(ft, i = sp$row, j = sp$cols, part = "body")
      ft <- flextable::align(
        ft,
        i = sp$row,
        j = sp$cols,
        part = "body",
        align = "center"
      )
    }
  }

  # Content-driven autofit. plain `autofit()` sizes columns to
  # content; `layout = "autofit"` tells Word to honour the widths.
  ft <- flextable::autofit(ft)
  ft <- flextable::set_table_properties(ft, layout = "autofit")

  title <- attr(rendered, "title")
  note <- attr(rendered, "note")
  # APA Manual 7 Section 7.10-Section 7.11: the caption is flush-left
  # (italic-styled by Word's "Table Caption" style at the OOXML level
  # when rendered into docx). Same helper as the descriptive
  # families; this builder pins Calibri on the whole table, so the
  # caption is pinned with it (`font(part = "all")` does not reach
  # the caption).
  ft <- .spicy_ft_html_caption(
    ft,
    title,
    props = officer::fp_text(font.family = "Calibri", font.size = 11)
  )
  if (!is.null(note) && nzchar(note)) {
    # APA Manual 7 Section 7.14: general notes form a SINGLE paragraph
    # ("*Note.* ...") that wraps naturally within the table width.
    # Collapse embedded newlines (source-side line breaks meant for
    # ASCII rendering) into spaces, then emit one footer line whose
    # leading "Note." chunk is italicised and the remainder is in
    # regular type.
    note_one_line <- gsub("\n", " ", note, fixed = TRUE)
    fp_normal <- officer::fp_text(font.family = "Calibri", font.size = 10)
    fp_italic <- officer::fp_text(
      font.family = "Calibri",
      font.size = 10,
      italic = TRUE
    )
    note_split <- .note_prefix_split(note_one_line)
    if (!is.null(note_split)) {
      para <- flextable::as_paragraph(
        flextable::as_chunk(note_split$marker, props = fp_italic),
        flextable::as_chunk(note_split$rest, props = fp_normal)
      )
    } else {
      para <- flextable::as_paragraph(
        flextable::as_chunk(note_one_line, props = fp_normal)
      )
    }
    ft <- flextable::add_footer_lines(ft, top = FALSE, values = para)
  }
  # Force a single font across ALL parts (header, body, footer, caption)
  # AFTER all rows / lines have been added; flextable::font(part = "all")
  # only affects parts that exist at the call time, so add_footer_lines
  # above WOULD leave the freshly-added footer in the default Arial
  # otherwise.
  ft <- flextable::font(ft, part = "all", fontname = "Calibri")

  # Tag with a sub-class + stash the raw note. The custom
  # `print.spicy_flextable` / `knit_print.spicy_flextable` methods
  # use this attr to post-process the rendered HTML: strip the
  # rendered `<tfoot>` and re-inject the note as a `<div>` outside
  # the table (same trick as `output_tinytable` to keep the colspan
  # footer from widening the table). The Word / docx path
  # (`output_word`) ignores these methods and uses
  # `body_add_flextable()` on the underlying flextable, so the
  # in-table footer is preserved for docx fidelity.
  if (!is.null(note) && nzchar(note)) {
    attr(ft, "spicy_note") <- note
  }
  class(ft) <- c("spicy_flextable", class(ft))
  ft
}

# Strip the rendered `<tfoot>...</tfoot>` from a flextable's HTML
# (single `<tfoot>` always present after `add_footer_lines`) and
# inject `note` (HTML-escaped, with the leading "Note." italicised)
# as a `<div>` immediately after `</table>`, wrapped in an
# inline-block so the note inherits the table's content width
# without contributing to it. See the analogous comment in
# `output_tinytable` for the CSS rationale.
.spicy_ft_html_postprocess <- function(html_str, note) {
  if (is.null(note) || !nzchar(note)) {
    return(html_str)
  }
  esc <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE)
    s <- gsub("<", "&lt;", s, fixed = TRUE)
    s <- gsub(">", "&gt;", s, fixed = TRUE)
    s
  }
  note_one_line <- gsub("\n", " ", note, fixed = TRUE)
  note_html <- esc(note_one_line)
  note_html <- sub(
    .note_prefix_pattern(),
    paste0("<em>", spicy_str("note_prefix_emphasis"), "</em>"),
    note_html
  )
  note_div <- paste0(
    "<div class=\"spicy-ft-note\" style=\"",
    "width: min-content; min-width: 100%; box-sizing: border-box; ",
    "padding: 0.5rem 0.5rem 0.2rem 0.5rem; ",
    # Match the body's Calibri (set on every cell-content span via
    # flextable's `font(part = \"all\", fontname = \"Calibri\")`).
    # Without this the note div falls back to page default.
    "font-family: Calibri, 'Calibri', Arial, sans-serif; ",
    "font-size: 0.875rem; line-height: 1.25; ",
    "text-align: left;\">",
    note_html,
    "</div>"
  )
  open_outer <- paste0(
    "<div class=\"spicy-ft-outer\" ",
    "style=\"text-align: center;\">"
  )
  open_inner <- paste0(
    "<div class=\"spicy-ft-wrap\" style=\"",
    "display: inline-block; max-width: 100%; ",
    "text-align: left; vertical-align: top;\">"
  )
  close_both <- "</div></div>"
  # flextable's HTML emits inline borders on individual `<td>` /
  # `<th>` cells (border-bottom on row N, border-top on row N+1) and
  # the browser's default `border-collapse: separate` renders them
  # as two stacked 1pt lines instead of merging them. APA / classic
  # publication style expects a single hairline. Inject
  # `border-collapse: collapse` scoped to our wrapper -- adjacent
  # same-style borders collapse into one, and zero-width "empty"
  # borders are subsumed by their neighbours. Scoped via
  # `.spicy-ft-outer table` so we don't leak across other tables on
  # the same page.
  # Booktabs-style trimmed rules under each Model spanner cell:
  # short the rule on each side so adjacent Model spanners don't
  # have their under-rules touching at the column boundary. The
  # rule is drawn via a `::after` pseudo-element. With
  # `border-collapse: collapse`, the rule between the Model row
  # and the per-col row is contributed by BOTH sides -- the Model
  # row cells' `border-bottom` AND the per-col row cells'
  # `border-top` (both set to `1pt solid black` by flextable's
  # default classes). Killing either alone leaves the other in
  # place, so we neutralise both via `!important`, then add the
  # trimmed pseudo-rule on the Model spanner cells only.
  # In a single-model table WITH a CI pair the per-col row is the
  # first thead row, so the CI-spanner cell (`colspan="2"`) takes the
  # same treatment: its bracket rule is redrawn trimmed by the
  # pseudo-element instead of running edge to edge.
  #
  # Every `tr:first-child` rule is qualified with `:not(:last-child)`:
  # a table whose header is a SINGLE row (no model spanner, no CI
  # pair) has its first thead row as the last one too, and its
  # bottom border is the APA rule under the column labels -- the one
  # line the table cannot do without. Unqualified, the selector
  # erased it.
  border_collapse_css <- paste0(
    "<style>",
    ".spicy-ft-outer table { border-collapse: collapse; }",
    ".spicy-ft-outer thead tr:first-child:not(:last-child) th { ",
    "border-bottom: 0 none !important; }",
    ".spicy-ft-outer thead tr:nth-child(2) th { ",
    "border-top: 0 none !important; }",
    ".spicy-ft-outer thead tr:first-child:not(:last-child) ",
    "th[colspan]:not([colspan=\"1\"]) { position: relative; }",
    ".spicy-ft-outer thead tr:first-child:not(:last-child) ",
    "th[colspan]:not([colspan=\"1\"])::after { ",
    "content: \"\"; position: absolute; left: 0.5em; right: 0.5em; ",
    "bottom: 0; border-bottom: 1pt solid black; }",
    "</style>"
  )
  html_str <- sub("<tfoot>.*?</tfoot>", "", html_str, perl = TRUE)
  html_str <- sub(
    "<table ",
    paste0(open_outer, open_inner, border_collapse_css, "<table "),
    html_str,
    fixed = TRUE
  )
  html_str <- sub(
    "</table>",
    paste0("</table>", note_div, close_both),
    html_str,
    fixed = TRUE
  )
  html_str
}

#' Print method for spicy-tagged flextables
#'
#' @description
#' Prints a `spicy_flextable` object -- the [flextable::flextable()]
#' returned by `output = "flextable"`, tagged so the table note keeps
#' its styling in interactive HTML display. Every flextable verb works
#' on the tagged object directly; printing delegates to flextable's
#' own rendering ([as_flextable.spicy_flextable()] returns the
#' untagged object).
#'
#' @param x A `spicy_flextable` object.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `NULL` (HTML display path) or the result
#'   of flextable's own print method.
#'
#' @seealso [table_regression()], [as_flextable.spicy_flextable()]
#' @keywords internal
#' @exportS3Method print spicy_flextable
print.spicy_flextable <- function(x, ...) {
  note <- attr(x, "spicy_note")
  class(x) <- setdiff(class(x), "spicy_flextable")
  if (
    interactive() &&
      !isTRUE(getOption("knitr.in.progress")) &&
      requireNamespace("htmltools", quietly = TRUE)
  ) {
    h <- flextable::htmltools_value(x = x)
    h_str <- as.character(h)
    h_str <- .spicy_ft_html_postprocess(h_str, note)
    print(htmltools::browsable(htmltools::HTML(h_str)))
    return(invisible(NULL))
  }
  NextMethod()
}

#' Convert a spicy flextable output to a plain flextable
#'
#' `table_regression()` and `table_continuous_lm()` return their
#' `output = "flextable"` tables with a lightweight `spicy_flextable`
#' class tag whose only job is HTML note styling. Every flextable
#' verb already works on the tagged object; this method returns the
#' clean underlying [flextable::flextable()] -- the note is already
#' part of its footer -- for workflows that want the untagged object
#' (e.g. custom knit hooks, `flextable::save_as_docx()` pipelines, or
#' composition with other flextable tooling), mirroring
#' `gtsummary::as_flex_table()`.
#'
#' Rendering in Quarto / R Markdown does NOT require this conversion:
#' the `knit_print` method auto-detects the output format and
#' delegates to flextable's native rendering for non-HTML targets
#' (Word, PowerPoint, PDF).
#'
#' @param x A `spicy_flextable` object.
#' @param ... Unused; for generic consistency.
#' @return A `flextable` object.
#' @exportS3Method flextable::as_flextable spicy_flextable
as_flextable.spicy_flextable <- function(x, ...) {
  class(x) <- setdiff(class(x), "spicy_flextable")
  x
}

#' @exportS3Method knitr::knit_print spicy_flextable
knit_print.spicy_flextable <- function(x, ...) {
  note <- attr(x, "spicy_note")
  class(x) <- setdiff(class(x), "spicy_flextable")
  # Non-HTML knit targets (Quarto / R Markdown -> docx, pptx, pdf):
  # pandoc DROPS raw HTML, so the htmltools path below rendered an
  # empty document -- reported from a real Quarto -> Word workflow
  # (dev/quarto_word_rendering_spec.md). The flextable already
  # carries the note natively in its footer (<tfoot> added at build
  # time); the HTML post-processing below merely RELOCATES that
  # footer outside the table for viewport-width cosmetics, which
  # only matters in HTML. Delegate to flextable's own format-aware
  # knit_print for every other KNOWN target -- outside a knit
  # (pandoc target NULL, e.g. direct calls) keep the historical HTML
  # path, matching what the engines themselves do.
  pandoc_to <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (!is.null(pandoc_to) && !isTRUE(knitr::is_html_output())) {
    # Plain delegation: pandoc includes the raw {=openxml} block
    # verbatim, xmlns declarations included (verified with jgm on
    # pandoc#11772 -- an earlier "fix" here stripped the namespaces
    # on the false premise that pandoc dropped such blocks; the
    # measurement was counting the literal string "<w:tbl>" and
    # missing "<w:tbl xmlns...>". Do not mutate the fragment.)
    return(knitr::knit_print(x, ...))
  }
  h <- flextable::htmltools_value(x = x)
  h_str <- as.character(h)
  h_str <- .spicy_ft_html_postprocess(h_str, note)
  knitr::asis_output(h_str)
}

# ---- excel ---------------------------------------------------------------

output_excel <- function(rendered, excel_path, excel_sheet) {
  # Decision 16: NULL resolves to the registry sheet name.
  if (is.null(excel_sheet)) {
    excel_sheet <- spicy_str("excel_sheet_regression")
  }
  if (!spicy_pkg_available("openxlsx2")) {
    # nocov start
    spicy_abort(
      c(
        "Output `\"excel\"` requires the 'openxlsx2' package.",
        "i" = "Install it with `install.packages(\"openxlsx2\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  if (is.null(excel_path) || !nzchar(excel_path)) {
    # Reachable guard. validate_output_resources() (Phase F) only
    # rejects is.null(excel_path); it does NOT reject an empty string
    # on every platform. A direct caller of output_excel() /
    # dispatch_regression_output() that passes excel_path = "" (e.g.
    # paste0(dir, "/", fname) with an unset fname) reaches this abort
    # via the !nzchar() half of the guard.
    spicy_abort(
      "`excel_path` must be supplied for output = \"excel\".",
      class = "spicy_invalid_input"
    )
  }
  # ---- Read structured (typed) body via render_regression_table() -------
  # The renderer builds a parallel structured view (numeric body, CI
  # pre-split into LL/UL, per-cell precision + p-style + threshold
  # in col_meta, reference/fit-stat row markers). Engines consume it
  # directly -- no string parsing.
  struct <- attr(rendered, "structured")
  # Value columns only: the dot-prefixed identity columns are metadata
  # about each row, not cells of the table a reader opens.
  body <- .struct_display_body(struct$body) # numeric body
  spanners <- struct$spanners # model-level spanners
  ci_spanners <- struct$ci_pairs # CI pairs (LL/UL)
  col_meta <- struct$col_meta # per-col format spec
  format_spec <- struct$format_spec
  level_rows <- .struct_indent_rows(struct)
  outcome_row <- .struct_outcome_row(struct)
  title <- attr(rendered, "title")
  note <- attr(rendered, "note")
  group_sep <- attr(rendered, "group_sep_rows")
  # Every rule the console draws between blocks: the coefficients /
  # fit-stats divide (`group_sep_rows`) AND the rule that opens a
  # subordinate block -- ordinal `Thresholds:`, mixed-model
  # `Random effects:`, and the other component blocks -- carried by
  # `section_sep_rows`. Excel was the last engine drawing only the
  # first of them.
  rule_rows <- sort(unique(c(
    as.integer(group_sep %||% integer(0)),
    as.integer(attr(rendered, "section_sep_rows") %||% integer(0))
  )))

  n_cols <- ncol(body)
  has_model_spanner <- !is.null(spanners) && length(spanners) > 0L
  has_ci_spanner <- length(ci_spanners) > 0L

  wb <- openxlsx2::wb_workbook()
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = excel_sheet)

  start_row <- 1L
  if (!is.null(title) && nzchar(title)) {
    wb <- openxlsx2::wb_add_data(
      wb,
      sheet = excel_sheet,
      x = title,
      start_row = start_row
    )
    start_row <- start_row + 2L
  }

  # Write header rows. Layout (from top):
  #   * model spanner row (optional, when multi-model)
  #   * CI spanner row    (optional, when any CI column present)
  #   * sub-column header row (Variable, B, SE, LL, UL, p, ...)
  current_row <- start_row
  model_row_excel <- NA_integer_
  ci_row_excel <- NA_integer_

  write_spanner_row <- function(wb, label_at_col, row) {
    df <- as.data.frame(
      as.list(label_at_col),
      stringsAsFactors = FALSE,
      col.names = paste0("V", seq_along(label_at_col))
    )
    names(df) <- names(body)
    openxlsx2::wb_add_data(
      wb,
      sheet = excel_sheet,
      x = df,
      start_row = row,
      col_names = FALSE
    )
  }

  if (has_model_spanner) {
    model_row_excel <- current_row
    model_labels_at_col <- rep("", n_cols)
    for (lbl in names(spanners)) {
      model_labels_at_col[spanners[[lbl]]] <- lbl
    }
    wb <- write_spanner_row(wb, model_labels_at_col, model_row_excel)
    for (lbl in names(spanners)) {
      idx <- spanners[[lbl]]
      if (length(idx) >= 2L) {
        wb <- openxlsx2::wb_merge_cells(
          wb,
          sheet = excel_sheet,
          dims = openxlsx2::wb_dims(rows = model_row_excel, cols = idx)
        )
      }
    }
    current_row <- current_row + 1L
  }

  # Header rows (table_continuous_lm convention): two rows --
  # "top" carries the per-column labels with "95% CI" replacing
  # the LL/UL slots (merged); "bottom" carries the LL/UL
  # sub-labels only over CI pairs. Single-model: header_top +
  # (optional) header_bottom. Multi-model: model spanner row
  # (already written) + header_top + header_bottom.
  header_top_excel <- current_row
  hdr <- .build_label_rows(
    body,
    ci_spanners,
    spanners,
    col_meta = struct$col_meta
  )
  wb <- write_spanner_row(wb, hdr$top, header_top_excel)
  # Merge "95% CI" across each LL+UL pair on the top header row.
  if (has_ci_spanner) {
    for (cs in ci_spanners) {
      if (length(cs$cols) >= 2L) {
        wb <- openxlsx2::wb_merge_cells(
          wb,
          sheet = excel_sheet,
          dims = openxlsx2::wb_dims(rows = header_top_excel, cols = cs$cols)
        )
      }
    }
  }
  current_row <- current_row + 1L

  header_bottom_excel <- NA_integer_
  if (has_ci_spanner) {
    header_bottom_excel <- current_row
    wb <- write_spanner_row(wb, hdr$bottom, header_bottom_excel)
    current_row <- current_row + 1L
  }

  # `header_row_excel` aliases the LAST header row -- borders apply
  # the column-labels sub-rule there.
  header_row_excel <- if (!is.na(header_bottom_excel)) {
    header_bottom_excel
  } else {
    header_top_excel
  }

  body_first_row <- current_row
  # A factor level (and the absorbed factors of the "Fixed effects:"
  # block) is indented ONCE, by the engine: the Excel `indent` cell
  # style set below carries the offset, so the label text goes in
  # unpadded -- the console's leading spaces would indent it twice.
  if (length(level_rows) > 0L) {
    body[[1L]][level_rows] <- sub("^\\s+", "", body[[1L]][level_rows])
  }
  # `na.strings = ""` so NA numeric cells render as blank (not "#N/A");
  # below we overwrite reference-row and below-threshold cells with
  # text overrides (the `cell_undefined` glyph / "<.001").
  wb <- openxlsx2::wb_add_data(
    wb,
    sheet = excel_sheet,
    x = body,
    start_row = body_first_row,
    col_names = FALSE,
    na.strings = ""
  )
  body_end_row <- body_first_row + nrow(body) - 1L

  # Display string of every body cell, from the formatter the console,
  # the clipboard and flextable already share
  # (`.format_structured_to_string_body()` -> `.cell_to_string()`).
  # Excel keeps a REAL number wherever a number plus a number format
  # renders what the console renders; the cells a number cannot
  # express are overwritten with this text (see `text_rows` below), so
  # the sheet can never word a cell differently from the console.
  txt_body <- .format_structured_to_string_body(struct)
  txt_body[[1L]] <- body[[1L]] # row labels as written (single indent)

  # ---- Per-cell number format (using col_meta from structured body) ------
  # `wb_add_data(x = body)` above wrote real numerics directly (NA
  # cells become empty). This loop:
  #   * applies the per-column number format (".00" / "#.000" / "0"
  #     / etc.) so Excel renders each cell at the requested precision,
  #   * overwrites with text the cells no number can express (composite
  #     counts, starred estimates, Yes/No, en-dash, "<.001", ...).
  # No string parsing -- precision / p_style / threshold come from
  # the structured body's col_meta, populated by render_regression_table().
  if (nrow(body) > 0L && n_cols >= 2L) {
    body_rows_idx <- seq.int(body_first_row, body_end_row)
    for (j in 2:n_cols) {
      col_name <- names(body)[j]
      meta <- col_meta[[col_name]]
      # nocov start -- defensive: render_regression_table() emits a
      # col_meta entry for every numeric body column, so a lookup miss
      # cannot occur through table_regression().
      if (is.null(meta)) {
        next
      }
      # nocov end

      # Default per-column numfmt
      default_fmt <- .excel_numfmt(meta$precision, meta$p_style)
      wb <- openxlsx2::wb_add_numfmt(
        wb,
        sheet = excel_sheet,
        dims = openxlsx2::wb_dims(rows = body_rows_idx, cols = j),
        numfmt = default_fmt
      )
      # Per-row precision overrides (fit-stat rows: e.g. "n" integer)
      if (!is.null(meta$fit_stat_overrides)) {
        for (ov in meta$fit_stat_overrides) {
          ov_fmt <- .excel_numfmt(
            ov$precision %||% meta$precision,
            ov$p_style %||% meta$p_style
          )
          excel_row <- body_first_row + ov$row - 1L
          wb <- openxlsx2::wb_add_numfmt(
            wb,
            sheet = excel_sheet,
            dims = openxlsx2::wb_dims(rows = excel_row, cols = j),
            numfmt = ov_fmt
          )
        }
      }

      # ---- Text overrides ------------------------------------------------
      # `text_rows` flags the body rows of this column whose display
      # string cannot be produced by a number plus a number format.
      # Everything else stays a real number, so the workbook keeps its
      # arithmetic value. The strings themselves always come from
      # `txt_body` -- one formatter for every engine.
      col_vals <- body[[j]]
      text_rows <- logical(nrow(body))

      # (1) `decimal_mark`. Excel renders a numeric cell with the
      #     VIEWER's locale separator, which the file cannot set: a
      #     table asked for "," would show "65.07" next to its own
      #     "<,001" text on a period-locale machine. When a non-default
      #     mark is requested the body goes out as text, so the
      #     requested convention holds in every locale.
      if (!identical(format_spec$decimal_mark, ".")) {
        text_rows[] <- TRUE
      }
      # (2) Composite cells (`display_cells`, today the "events/N"
      #     counts): no single number expresses them. They exist on
      #     reference rows too -- a reference level HAS events.
      disp_col <- meta[["display_cells"]]
      if (!is.null(disp_col)) {
        text_rows[!is.na(disp_col)] <- TRUE
      }
      # (3) Significance markers: "64.63***" is not a number. Only the
      #     marked cells convert; the rest of the column keeps its
      #     numbers, and the explicit right-alignment below keeps the
      #     column visually uniform.
      star_col <- struct$stars$markers[[col_name]]
      if (!is.null(star_col)) {
        text_rows[nzchar(star_col)] <- TRUE
      }
      # (4) The Yes/No of the fixed-effects disclosure block
      #     (numeric-encoded 1/0 in the typed body -- a bare "1" in a
      #     coefficient column would read like an estimate).
      if (!is.null(meta$fit_stat_overrides)) {
        for (ov in meta$fit_stat_overrides) {
          if (identical(ov$fit_stat %||% "", "fixed_effects")) {
            text_rows[ov$row] <- TRUE
          }
        }
      }
      # (5) Outcome row (multi-DV): a label, not a value.
      if (
        length(outcome_row) == 1L &&
          col_name %in% names(struct$outcome_labels_by_col)
      ) {
        text_rows[outcome_row] <- TRUE
      }
      # (6) Cells the contract marks: the en-dash of a reference level
      #     (in this block, in this model) and of a statistic that
      #     applies but is not computable -- an unavailable variance-
      #     component SE, a fit statistic undefined for one model's
      #     class in a mixed table. Cells with no status keep their
      #     number; an absent cell has a blank display and writes
      #     nothing.
      status_col <- .struct_cell_status(struct, col_name)
      text_rows[nzchar(status_col)] <- TRUE
      # (7) Below-threshold p-values ("<.001").
      if (!is.null(meta$threshold)) {
        text_rows[!is.na(col_vals) & col_vals < meta$threshold] <- TRUE
      }

      txt_col <- txt_body[[j]]
      if (all(text_rows)) {
        wb <- openxlsx2::wb_add_data(
          wb,
          sheet = excel_sheet,
          x = data.frame(x = txt_col, stringsAsFactors = FALSE),
          start_row = body_first_row,
          start_col = j,
          col_names = FALSE
        )
      } else {
        for (i in which(text_rows)) {
          # A blank display leaves the cell as written above (an NA
          # numeric is already an empty cell).
          if (!nzchar(txt_col[i])) {
            next
          }
          wb <- openxlsx2::wb_add_data(
            wb,
            sheet = excel_sheet,
            x = txt_col[i],
            start_row = body_first_row + i - 1L,
            start_col = j
          )
        }
      }
    }
  }

  # ---- APA borders -------------------------------------------------------
  # Five rules, mirroring table_continuous / table_continuous_lm:
  #   1. Top rule above the first header row (spanner or sub-header).
  #   2. Spanner mid-rule beneath each spanned range (when spanners
  #      exist) -- structural twin of the CI mid-rule in continuous.
  #   3. Sub-rule below the sub-header row.
  #   4. Hair rule above each block boundary the console rules off:
  #      the coefficients / fit-stats divide (group_sep_rows) and the
  #      opening of a subordinate block -- `Thresholds:`,
  #      `Random effects:`, ... (section_sep_rows). Each value is the
  #      body index of the block's first row; -2L shifts to the Excel
  #      row above it.
  #   5. Bottom rule below the final body row.
  #
  # IMPORTANT: openxlsx2::wb_add_border() has formal defaults
  # `left_border = right_border = top_border = bottom_border = "thin"`,
  # so an explicit `top_border = "thin"` call paints all four sides
  # unless the others are set to NULL. We pass NULL on every
  # unused side to draw only the intended rule.
  draw_rule <- function(
    wb,
    rows,
    cols,
    top = NULL,
    bottom = NULL,
    weight = "thin"
  ) {
    openxlsx2::wb_add_border(
      wb,
      sheet = excel_sheet,
      dims = openxlsx2::wb_dims(rows = rows, cols = cols),
      top_border = if (isTRUE(top)) weight else NULL,
      bottom_border = if (isTRUE(bottom)) weight else NULL,
      left_border = NULL,
      right_border = NULL
    )
  }
  # Choose the top-of-header row: model spanner if present,
  # otherwise the column-labels (top) row.
  top_rule_row <- if (has_model_spanner) {
    model_row_excel
  } else {
    header_top_excel
  }
  # Belt + braces: draw the top rule BOTH as top-border on the
  # header row AND as bottom-border on the row above. Excel
  # rendering of borders on merged cells (the Model spanner or
  # the "95% CI" merge) can drop the top border on cells inside
  # the merge; the duplicate draw on the unmerged row above
  # guarantees the rule appears continuous across the full row.
  wb <- draw_rule(wb, rows = top_rule_row, cols = seq_len(n_cols), top = TRUE)
  if (top_rule_row > 1L) {
    wb <- draw_rule(
      wb,
      rows = top_rule_row - 1L,
      cols = seq_len(n_cols),
      bottom = TRUE
    )
  }
  # Mid-rule under each model spanner span. Drawn ONLY over the
  # spanner cells (cols 2..n of the model); the Variable column
  # stays without a bottom rule on the model spanner row.
  if (has_model_spanner) {
    for (lbl in names(spanners)) {
      wb <- draw_rule(
        wb,
        rows = model_row_excel,
        cols = spanners[[lbl]],
        bottom = TRUE
      )
    }
  }
  # Mid-rule under each "95% CI" merged cell on the top header
  # row (over LL/UL pairs only; matches the table_continuous /
  # table_continuous_lm CI mid-rule convention). Drawn on the
  # BOTTOM of header_top AND the TOP of header_bottom for the
  # CI cols -- belt + braces so the visual rule survives the
  # merge under any Excel renderer.
  if (has_ci_spanner) {
    for (cs in ci_spanners) {
      wb <- draw_rule(
        wb,
        rows = header_top_excel,
        cols = cs$cols,
        bottom = TRUE
      )
      if (!is.na(header_bottom_excel)) {
        wb <- draw_rule(
          wb,
          rows = header_bottom_excel,
          cols = cs$cols,
          top = TRUE
        )
      }
    }
  }
  # Sub-rule under the last header row (column-labels row, or the
  # LL/UL sub-row when present).
  wb <- draw_rule(
    wb,
    rows = header_row_excel,
    cols = seq_len(n_cols),
    bottom = TRUE
  )
  # Hair rule above each block boundary.
  for (sep in rule_rows) {
    if (sep >= 2L && sep <= nrow(body)) {
      wb <- draw_rule(
        wb,
        rows = body_first_row + sep - 2L,
        cols = seq_len(n_cols),
        bottom = TRUE,
        weight = "hair"
      )
    }
  }
  # Bottom rule under last body row.
  if (nrow(body) > 0L) {
    wb <- draw_rule(
      wb,
      rows = body_end_row,
      cols = seq_len(n_cols),
      bottom = TRUE
    )
  }

  # ---- Alignment (default Calibri font, no custom override) ---------------
  # Body numeric columns (B / SE / LL / UL / p) are RIGHT-aligned so
  # the rightmost digit anchors against the cell edge. Each column
  # carries uniform decimal precision (2 dp for B/SE/LL/UL, 3 dp for
  # p), and Calibri renders 0-9 with tabular (fixed) width, so
  # right-alignment yields visual decimal alignment without a
  # monospaced font or pre-padded strings.
  #
  # Centring numeric cells -- the table_continuous convention --
  # decimal-aligns only when every value in a column has the same
  # *character width*. In regression tables negative and positive
  # values coexist (e.g. B = "-3.19" alongside "0.84"), so centring
  # places the decimal point at different horizontal positions per
  # row. Right-align is the standard convention for numerics in
  # tables (gt's cols_align_decimal(), tinytable's align = "d",
  # flextable's table_continuous_lm decimal mode all converge on it
  # via different paths).
  #
  # Headers: spanner rows ("95% CI", model spanner) stay centred so
  # the merged label sits over its column group. Column-label row
  # (B / SE / LL / UL / p) is centred over the right-aligned body --
  # the same centred-header / right-aligned-body pattern used in
  # table_continuous_lm for n / p columns.
  if (n_cols >= 1L) {
    header_rows <- c(model_row_excel, header_top_excel, header_bottom_excel)
    header_rows <- header_rows[!is.na(header_rows)]
    if (n_cols >= 2L) {
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        sheet = excel_sheet,
        dims = openxlsx2::wb_dims(rows = header_rows, cols = 2:n_cols),
        horizontal = "center",
        vertical = "center"
      )
    }
    wb <- openxlsx2::wb_add_cell_style(
      wb,
      sheet = excel_sheet,
      dims = openxlsx2::wb_dims(rows = header_rows, cols = 1L),
      horizontal = "left",
      vertical = "center"
    )
    if (nrow(body) > 0L) {
      body_rows <- seq.int(body_first_row, body_end_row)
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        sheet = excel_sheet,
        dims = openxlsx2::wb_dims(rows = body_rows, cols = 1L),
        horizontal = "left"
      )
      # Factor-level rows in the Variable column: add Excel
      # `indent` so the rendered cell carries the visual indent
      # consistent with the other rich-output engines.
      if (length(level_rows) > 0L) {
        level_excel_rows <- body_first_row + level_rows - 1L
        wb <- openxlsx2::wb_add_cell_style(
          wb,
          sheet = excel_sheet,
          dims = openxlsx2::wb_dims(rows = level_excel_rows, cols = 1L),
          horizontal = "left",
          indent = "1"
        )
      }
      if (n_cols >= 2L) {
        wb <- openxlsx2::wb_add_cell_style(
          wb,
          sheet = excel_sheet,
          dims = openxlsx2::wb_dims(rows = body_rows, cols = 2:n_cols),
          horizontal = "right",
          vertical = "center"
        )
      }
    }
  }

  # fit_stats_layout = "merged": for each fit-stat row, merge the
  # numeric sub-columns of every model into a single wide cell
  # containing the fit-stat value. The value lives in the first
  # sub-column (rendered body layout); the merge extends it across
  # the remaining sub-columns of the model. The merged cell is
  # centred so the value sits visually under the model spanner.
  fit_stats_layout <- attr(rendered, "fit_stats_layout") %||% "first_col"
  if (identical(fit_stats_layout, "merged")) {
    specs <- .fit_stat_merge_ranges(body, spanners, group_sep)
    for (sp in specs) {
      excel_row <- body_first_row + sp$row - 1L
      wb <- openxlsx2::wb_merge_cells(
        wb,
        sheet = excel_sheet,
        dims = openxlsx2::wb_dims(rows = excel_row, cols = sp$cols)
      )
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        sheet = excel_sheet,
        dims = openxlsx2::wb_dims(rows = excel_row, cols = sp$cols),
        horizontal = "center"
      )
    }
  }

  if (!is.null(note) && nzchar(note)) {
    foot_row <- body_end_row + 2L
    note_lines <- strsplit(note, "\n", fixed = TRUE)[[1]]
    wb <- openxlsx2::wb_add_data(
      wb,
      sheet = excel_sheet,
      x = note_lines,
      start_row = foot_row
    )
  }

  # Column widths from the text each column carries (header labels +
  # the display strings), so the row labels are readable on open.
  wb <- .spicy_xl_set_widths(
    wb,
    sheet = excel_sheet,
    cells = .spicy_xl_cells(txt_body, headers = list(hdr$top, hdr$bottom))
  )

  openxlsx2::wb_save(wb, file = excel_path, overwrite = TRUE)
  invisible(rendered)
}

# ---- clipboard -----------------------------------------------------------

# nocov start -- clipboard side effect requires a system clipboard
# AND the optional `clipr` package. Exercised on user machines but
# unreachable on headless CI runners; the validator (Phase F) and
# the orchestrator's clipr / clipr_available checks short-circuit
# before reaching this function in test environments.
output_clipboard <- function(rendered, clipboard_delim) {
  if (!spicy_pkg_available("clipr")) {
    # nocov start
    spicy_abort(
      c(
        "Output `\"clipboard\"` requires the 'clipr' package.",
        "i" = "Install it with `install.packages(\"clipr\")`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  if (!clipr::clipr_available()) {
    # nocov start -- system clipboard is environment-dependent and
    # typically unavailable on headless CI runners; we test the
    # branch behaviourally elsewhere via mocking.
    spicy_abort(
      c(
        "System clipboard is not available.",
        "i" = "On Linux, install xclip or xsel."
      ),
      class = "spicy_unsupported"
    )
    # nocov end
  }
  # nocov start -- write_clip side effect cannot run on headless CI.
  txt <- clipboard_payload(rendered, clipboard_delim)
  clipr::write_clip(txt)
  invisible(rendered)
  # nocov end
}
# nocov end -- closes the output_clipboard CI-unreachable block; the
# pure clipboard_payload() builder below is NOT suppressed (it is
# reachable on any user machine and directly unit-tested).

# Build the delimited payload mirroring the Excel layout: title row,
# spanner row, header, body, note rows. Cells are joined with
# `clipboard_delim` (a tab by default) by the shared clipboard
# helpers, which also escape any cell that would break the grid and
# keep the cells free of the decimal-alignment padding: the body is
# derived from the structured contract, so it carries the display
# strings only and a paste into a spreadsheet types every column at
# once. Horizontal rules are NOT injected: delimited text cannot
# encode a single continuous line, and rule rows displayed as
# delimiter-separated dash segments produce a visually broken result.
# The same layout serves table_categorical(), table_continuous() and
# table_continuous_lm(). Kept separate from output_clipboard() so the
# layout is testable without exercising the clipboard side-effect.
clipboard_payload <- function(rendered, clipboard_delim) {
  # Source from the structured (typed) body, then derive display strings
  # via the shared formatter. CI is already split into LL / UL columns
  # in the structured body -- no string parsing required.
  struct <- attr(rendered, "structured")
  body <- .format_structured_to_string_body(struct)
  title <- attr(rendered, "title")
  note <- attr(rendered, "note")
  spanners <- struct$spanners
  ci_spanners <- struct$ci_pairs
  n_cols <- ncol(body)
  has_model_spanner <- !is.null(spanners) && length(spanners) > 0L
  has_ci_spanner <- length(ci_spanners) > 0L

  rows <- list()
  add_row <- function(r) {
    rows[[length(rows) + 1L]] <<- as.character(r)
  }
  add_text_rows <- function(text) {
    for (r in .spicy_clip_text_rows(text, n_cols)) {
      add_row(r)
    }
  }

  add_text_rows(title)

  stripped <- if (has_model_spanner) {
    .strip_spanner_prefix(body, spanners)
  } else {
    body
  }

  if (has_model_spanner) {
    # Top header row: model spanner labels repeated across each
    # spanned column. The label appears in every column of the
    # span (rather than once + empty siblings) so the user can
    # merge after paste in Excel / Word with a single selection.
    spanner_row <- rep("", n_cols)
    for (lbl in names(spanners)) {
      spanner_row[spanners[[lbl]]] <- lbl
    }
    add_row(spanner_row)
  }

  # Column-labels row with "95% CI" replacing the LL / UL slots
  # (repeated across the pair so the user can merge after paste).
  # Bottom row with the LL / UL sub-labels appears below if any
  # CI column exists. Matches the table_continuous_lm clipboard
  # convention exactly.
  hdr <- .build_label_rows(
    body,
    ci_spanners,
    spanners,
    col_meta = struct$col_meta
  )
  add_row(hdr$top)
  if (has_ci_spanner) {
    add_row(hdr$bottom)
  }

  for (r in .spicy_clip_rows(stripped)) {
    add_row(r)
  }

  add_text_rows(note)

  .spicy_clip_payload(rows, clipboard_delim)
}

# ---- word ----------------------------------------------------------------

output_word <- function(rendered, word_path, word_template = NULL) {
  if (
    !spicy_pkg_available("flextable") ||
      !spicy_pkg_available("officer")
  ) {
    # nocov start
    spicy_abort(
      c(
        "Output `\"word\"` requires the 'flextable' and 'officer' packages.",
        "i" = "Install with `install.packages(c(\"flextable\", \"officer\"))`."
      ),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  if (is.null(word_path) || !nzchar(word_path)) {
    # Reachable guard (see the output_excel() excel_path comment).
    # validate_output_resources() only rejects is.null(word_path), not
    # an empty string; a direct caller passing word_path = "" reaches
    # this abort via the !nzchar() half of the guard.
    spicy_abort(
      "`word_path` must be supplied for output = \"word\".",
      class = "spicy_invalid_input"
    )
  }
  ft <- output_flextable(rendered)

  # --- APA auto-numbered caption (overrides the plain caption set by
  # output_flextable). Shared with the descriptive families through
  # `.spicy_ft_word_caption()`; this builder pins Calibri on the whole
  # table, so the caption is given the same font explicitly.
  ft <- .spicy_ft_word_caption(
    ft,
    attr(rendered, "title"),
    props = officer::fp_text(font.family = "Calibri", font.size = 11)
  )

  # --- Repeated header on page break + cant-split rows ----------------
  # `paginate()` marks the flextable for header re-rendering on each
  # page break (APA / Stata-table convention). Word-side row-split
  # prevention is set via the underlying officer XML when the table
  # is added to the docx body below.
  ft <- flextable::paginate(ft, hdr_ftr = TRUE)

  # --- Save via officer: gives us `split = FALSE` (row cant-split) +
  # `keepnext = TRUE` (keep table together with its caption) uniformly,
  # whether the user supplied a custom template or not.
  if (!is.null(word_template) && nzchar(word_template)) {
    if (!file.exists(word_template)) {
      spicy_abort(
        sprintf("`word_template` file does not exist: %s", word_template),
        class = "spicy_invalid_input"
      )
    }
    doc <- officer::read_docx(path = word_template)
  } else {
    doc <- officer::read_docx()
  }
  doc <- flextable::body_add_flextable(
    doc,
    value = ft,
    align = "center",
    keepnext = TRUE,
    split = FALSE
  )
  print(doc, target = word_path)
  invisible(rendered)
}

# ---- as_structured() accessor --------------------------------------------

#' Extract the typed (structured) view of a spicy table
#'
#' spicy's tables return a *display* representation by default -- a
#' character `data.frame` with stars suffixes, en-dash for reference
#' rows, bracketed `"[L, U]"` confidence intervals, and APA padding on
#' p-values. This accessor returns the *typed* view that the output
#' engines (Excel, gt, tinytable, flextable, clipboard) consume
#' internally: a fully numeric body with CI pre-split into `LL` / `UL`
#' columns, NAs for non-applicable / reference cells, plus per-cell
#' markers and a format specification.
#'
#' This is the right entry point for users who want to:
#'
#' * **Filter coefficients programmatically**, e.g.
#'   `as_structured(tbl)$body[which(as_structured(tbl)$body$p < 0.05), ]`
#'   (`which()` drops the structurally empty rows -- factor headers,
#'   reference levels -- whose `p` is `NA`; a bare logical index
#'   would keep them as all-`NA` rows).
#' * **Aggregate raw values across rows**, e.g.
#'   `mean(as_structured(tbl)$body[["B"]], na.rm = TRUE)`.
#' * **Build a custom downstream renderer** that consumes the same
#'   structured contract as spicy's built-in engines.
#'
#' @param x A spicy table built with `output = "default"`:
#'   [table_regression()], [table_categorical()],
#'   [table_continuous()] or [table_continuous_lm()]. All four return
#'   the same schema.
#'
#' @return A list with the structured view (see Details for the
#'   schema).
#'
#' @section Schema:
#' * `version` -- integer contract version (see *Versioning* below).
#' * `body` -- `data.frame` with a `Variable` character column, one or
#'   more numeric value columns, and four dot-prefixed *identity*
#'   columns at the end. Confidence intervals are split into `LL` /
#'   `UL` columns named like `"95% CI: LL"` / `"95% CI: UL"` (or
#'   prefixed with the model label in multi-model output). A cell with
#'   no value (reference level, term absent from a model, factor
#'   header) is `NA`.
#' * `body$.variable` -- source variable of the row: the factor name on
#'   a factor header / level / reference row, the term otherwise, and
#'   the fit-statistic token (`"nobs"`, `"r2"`, `"fixed_effects"`,
#'   ...) on a fit-statistic row.
#' * `body$.level` -- the factor level (or the absorbed factor of a
#'   fixed-effects block, or the grouping factor of an `"N (...)"`
#'   row); `NA` outside a factor.
#' * `body$.row_role` -- what the row *is*: `"coef"`,
#'   `"factor_header"`, `"level"`, `"reference"`, `"fit_stat"`,
#'   `"outcome"`, `"vc"` (variance component) in a regression table,
#'   plus `"summary"` (a row summarising one variable), `"group"` (a
#'   row keyed by one level of `by`) and `"missing"` (a row keyed by
#'   the *missing* value) in the descriptive ones. The role is the
#'   key a consumer matches on: `"(Missing)"` is a display label --
#'   auto-renamed on collision, translatable -- and the role survives
#'   both.
#' * `body$.indent` -- display indent depth of the label (`0` or `1`).
#'   Renderers indent `.indent > 0` rows; the label text itself is
#'   already indented in the character body only.
#' * `cell_status` -- per-**cell** semantics, keyed by column name, one
#'   character vector as long as `body` per column that needs one:
#'   `"reference"` (reference level, *in this estimate block and this
#'   model*), `"undefined"` (the statistic applies to the row but no
#'   number expresses it -- an unavailable variance-component standard
#'   error, a fit statistic undefined for one model's class), `""`
#'   otherwise. Both marked statuses display as an en-dash. A cell
#'   whose value is `NA` with no status is *absent* and displays
#'   blank. Columns with no marked cell are omitted.
#' * `outcome_labels_by_col` -- for the outcome row (explicit
#'   `outcome_labels` with two or more models), the display label
#'   keyed by each model's first structured column name.
#' * `col_meta` -- per-column metadata keyed by structured column
#'   name (token, model_id, precision, p-style, below-threshold,
#'   CI pair / role / label). A column whose cells cannot be
#'   reconstructed from one number -- the `"events/N"` counts of
#'   `show_columns = "n_events"` -- also carries `display_cells`, a
#'   character vector as long as `body` holding the display string
#'   of each cell (`NA` where the number formats normally). A
#'   renderer must prefer it over the numeric value.
#' * `stars` -- `NULL` unless `stars` was requested, otherwise a list
#'   with `thresholds` (symbol to p cutoff) and `markers` (per-cell
#'   marker strings, keyed by column name, `""` where a cell takes
#'   none).
#' * `spanners` -- named list mapping a grouping label to its column
#'   indices in `body`: the model labels of a multi-model regression
#'   table, the `by` levels of a categorical one (each spanning its
#'   `n` / `%` pair).
#' * `ci_pairs` -- list of `(label, cols)` entries describing each
#'   CI pair in `body`.
#' * `format_spec` -- global format defaults (decimal mark, digits,
#'   p-style, CI level, etc.).
#'
#' @section Descriptive tables:
#' The three descriptive families return the same schema; only the
#' `col_meta` tokens and the row roles they emit differ.
#'
#' * [table_categorical()] -- one `"factor_header"` row per variable
#'   then one `"level"` (or `"missing"`) row per category, exactly as
#'   the console lays them out. Tokens: `"n"`, `"pct"`, `"p"`,
#'   `"assoc"` (the association measure) and `"assoc_ci"` (its
#'   bounds). In a `by` table each group owns an `n` / `%` pair with
#'   its own spanner, and the margin column is flagged
#'   `col_meta$<col>$total`, never matched on the label `"Total"`.
#' * [table_continuous()] -- one `"summary"` row per variable, or one
#'   `"group"` row per level of `by` (`"missing"` for the missing-`by`
#'   group), with the level in `.level`. Tokens are the
#'   `show_columns` vocabulary itself (`"m"`, `"sd"`, `"med"`,
#'   `"iqr"`, `"med_iqr"`, `"q1"`, `"q3"`, `"min"`, `"max"`, `"ci"`,
#'   `"med_ci"`, `"n"`) plus `"statistic"`, `"p"` and `"es"` for the
#'   group comparison. A statistic another variable displays is
#'   *absent* (`NA`, no status); one that applies but has no value is
#'   `"undefined"`.
#' * [table_continuous_lm()] -- one `"summary"` row per outcome.
#'   Tokens: `"emmean"` (a marginal mean; the column says which level
#'   in `col_meta$<col>$level`), `"delta"` (the contrast), `"b"` (a
#'   numeric predictor's slope), `"ci"`, `"statistic"`, `"p"`,
#'   `"r2"`, `"es"`, `"n"`, `"weighted_n"`.
#'
#' Cells the console builds from more than one number -- a
#' `"Med [Q1, Q3]"`, a test gloss, an effect size with its interval --
#' keep their numeric anchor in `body` and their exact printed string
#' in `col_meta$<col>$display_cells`. `stars` is always `NULL`:
#' descriptive tables carry no significance markers.
#'
#' @section Versioning:
#' `version` says which contract an object carries. Version `3` moved
#' row identity out of index vectors and into the body itself:
#'
#' * `reference_rows` and `reference_models_by_row` become
#'   `cell_status`, which marks the reference cell instead of the whole
#'   row -- the row-scoped flag was blanking estimate blocks that have
#'   no per-level reference at all.
#' * `factor_header_rows` becomes `.row_role == "factor_header"`.
#' * `fit_stat_rows` becomes `.row_role == "fit_stat"`.
#' * `level_rows` becomes `.indent > 0`.
#' * `outcome_row` becomes `.row_role == "outcome"`.
#'
#' Index vectors are the structure that corrupts as soon as two bodies
#' are stacked or merged, so they were removed rather than kept
#' alongside. An object carrying an older contract (or none) is
#' refused with the correspondence above; rebuild it with the function
#' that produced it. An object carrying a *newer* contract than the
#' spicy reading it is refused as well.
#'
#' Version `3` also opened the accessor to the descriptive families,
#' which had no typed view before: `.row_role` gained `"summary"`,
#' `"group"` and `"missing"`. The vocabulary is extended by addition
#' -- an existing role never changes meaning.
#'
#' @seealso [table_regression()], [table_categorical()],
#'   [table_continuous()], [table_continuous_lm()] for the
#'   user-facing entry points.
#'
#' @examples
#' fit <- lm(mpg ~ wt + factor(cyl), data = mtcars)
#' tbl <- table_regression(fit)
#' s <- as_structured(tbl)
#' s$body                               # raw numeric body
#' s$body[which(s$body$p < 0.05), ]     # filter significant rows
#' # which() drops the structural NA rows (headers, reference levels)
#' s$body$.row_role                     # what each row is
#' s$body[s$body$.variable == "wt", ]   # address a row by its variable
#' s$col_meta$B                         # column metadata for B
#'
#' # The same schema on a descriptive table.
#' ct <- table_categorical(mtcars, c(cyl, gear), by = am)
#' sc <- as_structured(ct)
#' sc$body[, c("Variable", ".variable", ".level", ".row_role")]
#' sc$spanners                          # one per `by` group
#'
#' @export
as_structured <- function(x) {
  # The four table families that carry a structured view, mapped to
  # the function that builds one -- named in the error so a user who
  # passed the wrong object is told what to call.
  builders <- c(
    spicy_regression_table = "table_regression()",
    spicy_categorical_table = "table_categorical()",
    spicy_continuous_table = "table_continuous()",
    spicy_continuous_lm_table = "table_continuous_lm()"
  )
  # spicy release that gave each family its structured view -- a table
  # pickled by an older one carries no attribute at all.
  since <- c(
    spicy_regression_table = "0.12.0",
    spicy_categorical_table = "0.13.0",
    spicy_continuous_table = "0.13.0",
    spicy_continuous_lm_table = "0.13.0"
  )
  family <- names(builders)[names(builders) %in% class(x)]
  if (length(family) == 0L) {
    spicy_abort(
      c(
        "`as_structured()` expects a spicy table object.",
        "i" = sprintf(
          "Supported: %s.",
          paste(paste0("`", names(builders), "`"), collapse = ", ")
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  s <- attr(x, "structured")
  if (is.null(s)) {
    spicy_abort(
      c(
        "`x` has no structured view attached.",
        "i" = sprintf(
          "Was it built by `%s` (>= %s)?",
          builders[[family[[1L]]]],
          since[[family[[1L]]]]
        )
      ),
      class = "spicy_invalid_input"
    )
  }
  # Version guard, both ways. A NEWER view may carry semantics this
  # spicy does not know. An OLDER one is no longer readable: v3 moved
  # row identity out of the index vectors into the body, so reading a
  # v2 view with v3 code would silently see no reference rows, no
  # fit-stat rows and no indent -- a wrong table rather than a missing
  # feature.
  current <- .spicy_structured_version()
  v <- s$version
  if (!is.numeric(v) || length(v) != 1L || is.na(v)) {
    v <- 1L
  }
  if (v > current) {
    spicy_abort(
      c(
        sprintf(
          "`x` carries structured contract v%d; this spicy reads v%d.",
          as.integer(v),
          current
        ),
        "i" = "Update spicy, or rebuild the table with this version."
      ),
      class = "spicy_invalid_input"
    )
  }
  if (v < current) {
    spicy_abort(
      c(
        sprintf(
          "`x` carries structured contract v%d; this spicy reads v%d.",
          as.integer(v),
          current
        ),
        "i" = paste(
          "v3 replaced the row-index components: `reference_rows` and",
          "`reference_models_by_row` by `cell_status`,",
          "`factor_header_rows` / `fit_stat_rows` / `outcome_row` by",
          "`body$.row_role`, `level_rows` by `body$.indent`."
        ),
        "i" = sprintf(
          "Rebuild the table with `%s`.",
          builders[[family[[1L]]]]
        )
      ),
      class = "spicy_structured_version"
    )
  }
  s
}


# ---- print method --------------------------------------------------------

#' Print method for regression tables
#'
#' @description
#' Formats and prints a `spicy_regression_table` object as a styled
#' ASCII table: title banner, spanner row for multi-model tables,
#' decimal-aligned body with factor grouping and reference rows,
#' fit-statistics block, and footer note.
#'
#' @param x A `data.frame` of class `"spicy_regression_table"` as
#'   returned by [table_regression()] with `output = "default"`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @seealso [table_regression()], [spicy_print_table()]
#' @keywords internal
#' @export
print.spicy_regression_table <- function(x, ...) {
  group_sep <- attr(x, "group_sep_rows")
  # nocov start -- defensive: render_regression_table() always sets
  # the attribute (to integer(0) when no fit-stats footer is present).
  # Reached only if a caller manually constructed an object that
  # bypasses the renderer.
  if (is.null(group_sep)) {
    group_sep <- integer(0)
  }
  # nocov end
  # The subordinate "Thresholds" section rule is ASCII-only: union it in here
  # so the gt / flextable / tinytable / excel backends -- which read the raw
  # `group_sep_rows` attr and treat its first element as the fit-stats boundary
  # (+ merge ranges) -- are left untouched.
  section_sep <- attr(x, "section_sep_rows") %||% integer(0)
  group_sep <- sort(unique(c(as.integer(group_sep), as.integer(section_sep))))
  align <- attr(x, "align") %||% "decimal"
  spanners <- attr(x, "spanners")

  body <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  # Build a parallel `display_labels` vector that overrides the body's
  # `colnames()` for the PRINTED header text only. The data.frame
  # itself keeps its deduplicated, model-prefixed column names so the
  # caller (`output = "data.frame"` / `"long"`) sees the unambiguous
  # programmatic names. The display vector lifts the bare label
  # (`display_label` field stored on each `col_meta` entry by
  # `build_col_spec()`) — already stripped of both the dedup `.N`
  # suffix AND the `"Model X: "` prefix — so the console shows
  # `B | 95% CI | p | AME | 95% CI | p` rather than
  # `B | 95% CI | p | AME | 95% CI.2 | p.2`.
  struct <- attr(x, "structured")
  display_labels <- if (!is.null(struct) && !is.null(struct$col_meta)) {
    vapply(
      names(body),
      function(nm) {
        .lookup_display_label(nm, struct$col_meta)
      },
      character(1)
    )
  } else {
    names(body)
  }
  data_col_idx <- setdiff(seq_along(body), 1L)
  align_center_cols <- if (identical(align, "center")) {
    data_col_idx
  } else {
    integer(0)
  }
  # "decimal" alignment was pre-applied in the renderer (cells are
  # already padded). "right" / "auto" delegate to the print engine
  # defaults. "center" forces center alignment of data columns.
  # `center_headers = TRUE` is the publication convention for
  # coefficient tables: numeric / CI columns get their data right- or
  # decimal-aligned, but the column LABEL ("B", "SE", "95% CI", "p")
  # sits centered above the column content. Matches the look of
  # Stata regress / parameters::model_parameters / modelsummary.
  # Use the call-site `padding` stashed by table_regression() (in
  # the `"padding"` attribute) unless the user overrides via `...`.
  padding_attr <- attr(x, "padding") %||% 2L
  dot_args <- list(...)
  if (is.null(dot_args$padding)) {
    dot_args$padding <- padding_attr
  }
  # First element of the RAW group_sep_rows attr = the fit-stats
  # boundary (rule drawn before that row). Passing it lets the panel
  # splitter drop blank n / AIC stub rows from continuation panels
  # (single-model tables split by width, e.g. the multinomial
  # columns layout at narrow console widths).
  fit_stats_start <- {
    raw_gs <- as.integer(attr(x, "group_sep_rows") %||% integer(0))
    if (length(raw_gs)) raw_gs[1L] else NULL
  }
  do.call(
    spicy_print_table,
    c(
      list(
        body,
        title = attr(x, "title"),
        note = attr(x, "note"),
        group_sep_rows = group_sep,
        align_center_cols = align_center_cols,
        center_headers = TRUE,
        spanners = spanners,
        display_labels = display_labels,
        fit_stats_start = fit_stats_start
      ),
      dot_args
    )
  )
  invisible(x)
}
