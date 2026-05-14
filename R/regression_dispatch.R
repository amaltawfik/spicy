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
    excel_sheet = "Regression",
    clipboard_delim = "\t",
    word_path = NULL) {
  output <- match.arg(output, c("default", "data.frame", "long",
                                 "tinytable", "gt", "flextable",
                                 "excel", "clipboard", "word"))

  switch(output,
    default     = output_default(rendered, aligned),
    data.frame  = output_data_frame(rendered),
    long        = output_long(aligned),
    tinytable   = output_tinytable(rendered),
    gt          = output_gt(rendered),
    flextable   = output_flextable(rendered),
    excel       = output_excel(rendered, excel_path, excel_sheet),
    clipboard   = output_clipboard(rendered, clipboard_delim),
    word        = output_word(rendered, word_path),
    # nocov start -- defensive: match.arg() above forbids any other
    # value, so this branch is unreachable in practice.
    spicy_abort(sprintf("Unknown output \"%s\".", output),
                class = "spicy_invalid_input")
    # nocov end
  )
}


# ---- default: printable spicy_regression_table ---------------------------

# Carries the analytic data as attributes so broom methods (tidy,
# glance) can read them without re-running the pipeline.
output_default <- function(rendered, aligned) {
  attr(rendered, "spicy_long")     <- aligned$coefs_aligned
  attr(rendered, "spicy_fit_stats") <- aligned$fit_stats_aligned
  class(rendered) <- c("spicy_regression_table", "spicy_table",
                       "data.frame")
  rendered
}


# ---- data.frame: plain wide ----------------------------------------------

output_data_frame <- function(rendered) {
  out <- as.data.frame(rendered, stringsAsFactors = FALSE)
  attr(out, "title") <- attr(rendered, "title")
  attr(out, "note") <- attr(rendered, "note")
  out
}


# ---- long: broom-style long format ---------------------------------------

# Returns the aligned long extract with broom-canonical column names.
# Columns:
#   model_id, term, estimate_type, estimate, std.error, conf.low,
#   conf.high, statistic, df, p.value, test_type, is_singular,
#   is_intercept, is_reference, factor_term, factor_level
output_long <- function(aligned) {
  long <- aligned$coefs_aligned
  if (is.null(long) || nrow(long) == 0L) return(long)
  # Rename to broom convention
  ren <- c(se = "std.error", ci_low = "conf.low", ci_high = "conf.high",
           p_value = "p.value")
  for (old in names(ren)) {
    if (old %in% names(long)) {
      names(long)[names(long) == old] <- ren[[old]]
    }
  }
  long$order_idx <- NULL
  long
}


# ---- spanner helpers (shared by rich-output dispatchers) -----------------

# Strip the "Label: " prefix from spanner-covered column names. Rich
# outputs that draw their own spanner row (gt, flextable, tinytable,
# excel, word) display the model label above the sub-columns, so the
# in-column prefix would be redundant.
.strip_spanner_prefix <- function(body, spanners) {
  for (lbl in names(spanners)) {
    idx <- spanners[[lbl]]
    prefix <- paste0(lbl, ": ")
    pat <- paste0("^", regex_escape(prefix))
    names(body)[idx] <- sub(pat, "", names(body)[idx])
  }
  body
}

# Detect factor-level rows in the rendered body. render_regression_table()
# prefixes each level row's Variable cell with two leading spaces under
# `factor_layout = "grouped"`; this helper returns those row indices and
# (optionally) the trimmed Variable column for engines that prefer to
# carry indentation as styling rather than as inline whitespace (HTML
# collapses runs of spaces; Word body cells lose them).
.detect_level_rows <- function(body) {
  var <- body[[1L]]
  which(grepl("^\\s+", var))
}
.trim_level_indent <- function(body, level_rows) {
  if (length(level_rows) == 0L) return(body)
  body[[1L]][level_rows] <- sub("^\\s+", "", body[[1L]][level_rows])
  body
}

# Parse a bracketed CI cell ("[lo, hi]" or "[lo; hi]" -- the separator
# adapts to `decimal_mark`) into its lo / hi components.
#
# Robust against:
#   * empty / whitespace-only cells  -> returns (\"\", \"\")
#   * em-dash or other non-bracket scalar  -> duplicates value to both
#     so reference-level rows and unavailable cells render uniformly
#     across the new LL / UL columns;
#   * pre-padding inserted by align_ci_strings() (leading / inner /
#     trailing whitespace) -- trimmed in the returned components.
.parse_ci_bracketed <- function(cells) {
  n <- length(cells)
  ll <- character(n)
  ul <- character(n)
  for (i in seq_len(n)) {
    s <- cells[i]
    if (!is.character(s) || !nzchar(trimws(s))) {
      ll[i] <- ""
      ul[i] <- ""
      next
    }
    s_t <- trimws(s)
    if (!startsWith(s_t, "[")) {
      # em-dash, single scalar -- duplicate so LL and UL each carry
      # the same marker (mirrors how table_continuous_lm displays
      # reference levels in LL / UL columns).
      ll[i] <- s_t
      ul[i] <- s_t
      next
    }
    inner <- sub("^\\s*\\[\\s*", "", s_t)
    inner <- sub("\\s*\\]\\s*$", "", inner)
    # In European mode (`decimal_mark = ","`), the separator is
    # `;` to disambiguate from intra-value commas; otherwise it is
    # `,`. Prefer splitting on `;` when one is present, falling
    # back to `,` only when no semicolon exists -- this keeps the
    # comma inside "0,18" intact under European formatting.
    sep_pattern <- if (grepl(";", inner, fixed = TRUE)) {
      "\\s*;\\s*"
    } else {
      "\\s*,\\s*"
    }
    parts <- strsplit(inner, sep_pattern, perl = TRUE)[[1L]]
    if (length(parts) >= 2L) {
      ll[i] <- trimws(parts[1L])
      ul[i] <- trimws(parts[2L])
    } else {
      ll[i] <- ""
      ul[i] <- ""
    }
  }
  list(ll = ll, ul = ul)
}

# Split every CI-bundle column ("[lo, hi]") in the rendered body into
# two separate numeric columns (LL, UL) and record a (95% CI) -- or
# AME 95% CI / f^2 95% CI / etc. -- spanner above each pair. Used by
# the clipboard + rich-output dispatchers (gt, tinytable, flextable,
# excel) so they can hand native decimal-alignment primitives clean
# numeric strings and emit an APA-style two-level header (model
# spanner > CI spanner > column labels). The console / data.frame /
# long outputs keep the bracketed CI representation -- terminal-friendly,
# downstream-compatible.
#
# Returns:
#   $body         - new wide body with LL/UL replacing each CI column;
#                   all numeric data columns are `trimws()`-ed so the
#                   engines see padding-free values.
#   $col_spec     - new col_spec; each CI entry replaced by two
#                   entries (ci_low / ci_high).
#   $spanners     - model spanners with indices widened to cover the
#                   inserted LL/UL pair when applicable.
#   $ci_spanners  - list of list(label, cols) entries describing each
#                   new CI spanner. `cols` are body-column indices in
#                   the NEW body (Variable at position 1).
.split_ci_columns <- function(body, col_spec, spanners) {
  if (is.null(col_spec) || length(col_spec) == 0L) {
    return(list(body = body, col_spec = col_spec,
                 spanners = spanners, ci_spanners = list()))
  }
  is_ci <- vapply(col_spec, function(cs) {
    !is.null(cs$fields) && identical(cs$fields, c("ci_low", "ci_high"))
  }, logical(1))
  if (!any(is_ci)) {
    return(list(body = body, col_spec = col_spec,
                 spanners = spanners, ci_spanners = list()))
  }

  var_col <- body[[1L]]
  new_data <- list()
  new_names <- character(0)
  new_spec <- list()
  old_to_new_first <- integer(length(col_spec))
  old_to_new_last  <- integer(length(col_spec))
  ci_spanners <- list()

  cursor <- 0L
  for (i in seq_along(col_spec)) {
    cs <- col_spec[[i]]
    old_col <- body[[i + 1L]]
    old_name <- names(body)[i + 1L]
    if (is_ci[i]) {
      parsed <- .parse_ci_bracketed(old_col)
      # Preserve any "Label: " prefix from multi-model spanner so the
      # downstream relabel step (rich engines) can strip it uniformly.
      pref <- if (grepl(":\\s+", old_name)) {
        sub("(:\\s+).+$", "\\1", old_name)
      } else {
        ""
      }
      ll_name <- paste0(pref, "LL")
      ul_name <- paste0(pref, "UL")

      cursor <- cursor + 1L
      new_data[[cursor]] <- parsed$ll
      new_names[cursor]  <- ll_name
      new_spec[[cursor]] <- list(
        estimate_type = cs$estimate_type,
        fields        = "ci_low",
        header_short  = "LL"
      )
      old_to_new_first[i] <- cursor

      cursor <- cursor + 1L
      new_data[[cursor]] <- parsed$ul
      new_names[cursor]  <- ul_name
      new_spec[[cursor]] <- list(
        estimate_type = cs$estimate_type,
        fields        = "ci_high",
        header_short  = "UL"
      )
      old_to_new_last[i] <- cursor

      ci_label <- cs$header_short %||% "95% CI"
      # Body col indices = +1 to skip the Variable column.
      ci_spanners[[length(ci_spanners) + 1L]] <- list(
        label = ci_label,
        cols  = c(cursor - 1L, cursor) + 1L
      )
    } else {
      cursor <- cursor + 1L
      new_data[[cursor]] <- old_col
      new_names[cursor]  <- old_name
      new_spec[[cursor]] <- cs
      old_to_new_first[i] <- cursor
      old_to_new_last[i]  <- cursor
    }
  }

  new_body <- data.frame(Variable = var_col,
                          stringsAsFactors = FALSE,
                          check.names = FALSE)
  for (k in seq_along(new_data)) {
    new_body[[new_names[k]]] <- new_data[[k]]
  }
  # Strip leading / trailing whitespace from all data columns. Native
  # decimal-alignment primitives (gt::cols_align_decimal,
  # tinytable::style_tt align = "d") and Excel's tabular-figures
  # right-align all expect clean numeric strings -- the render layer's
  # pre-padding was only useful for the console / bracketed-CI path
  # which keeps the original body.
  if (ncol(new_body) >= 2L) {
    new_body[-1L] <- lapply(new_body[-1L], trimws)
  }

  # Update model spanners: each old body col idx -> new body col idx
  # (or first-of-pair for CI), with the last spanned col extended by
  # +1 when it was a CI col (the UL companion belongs to the same
  # model span).
  new_spanners <- NULL
  if (!is.null(spanners) && length(spanners)) {
    new_spanners <- lapply(spanners, function(idxs) {
      # idxs are body col indices (Variable at 1). Map to col_spec
      # indices, look up new positions, build a contiguous range.
      cs_idxs <- idxs - 1L
      first_new <- old_to_new_first[cs_idxs[1L]] + 1L
      last_new  <- old_to_new_last[cs_idxs[length(cs_idxs)]] + 1L
      seq.int(first_new, last_new)
    })
  }

  list(
    body         = new_body,
    col_spec     = new_spec,
    spanners     = new_spanners,
    ci_spanners  = ci_spanners
  )
}


# ---- tinytable -----------------------------------------------------------

output_tinytable <- function(rendered) {
  if (!spicy_pkg_available("tinytable")) {
    # nocov start -- only reachable when 'tinytable' is not installed;
    # CI / dev runs always have it via Suggests.
    spicy_abort(
      c("Output `\"tinytable\"` requires the 'tinytable' package.",
        "i" = "Install it with `install.packages(\"tinytable\")`."),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  title <- attr(rendered, "title")
  note  <- attr(rendered, "note")
  spanners <- attr(rendered, "spanners")
  group_sep <- attr(rendered, "group_sep_rows")
  body <- as.data.frame(rendered, stringsAsFactors = FALSE,
                         check.names = FALSE)
  level_rows <- .detect_level_rows(body)
  body <- .trim_level_indent(body, level_rows)

  # Split bracketed CI cells into LL / UL numeric columns and capture
  # the (95% CI) spanners. With clean numeric strings in every CI
  # cell, tinytable's native `style_tt(align = "d")` aligns decimals
  # across rows -- no NBSP / monospace hack required.
  col_spec <- attr(rendered, "col_spec")
  split <- .split_ci_columns(body, col_spec, spanners)
  body         <- split$body
  spanners     <- split$spanners
  ci_spanners  <- split$ci_spanners
  has_model_spanner <- !is.null(spanners) && length(spanners) > 0L
  has_ci_spanner    <- length(ci_spanners) > 0L

  if (has_model_spanner) {
    body <- .strip_spanner_prefix(body, spanners)
  }
  n_cols <- ncol(body)
  tt <- tinytable::tt(body,
                       caption = title %||% "",
                       notes = if (!is.null(note)) note else NULL)

  # Nested column spanners. tinytable's group_tt stacks rows: a
  # subsequent call adds a header row ABOVE the previous one. We
  # apply CI spanners first (inner) then model spanners (outer), so
  # the layout reads top-to-bottom: model row | CI row | column
  # labels.
  if (has_ci_spanner) {
    ci_gspec <- list()
    for (cs in ci_spanners) {
      ci_gspec[[paste0("(", cs$label, ")")]] <- cs$cols
    }
    tt <- tinytable::group_tt(tt, j = ci_gspec)
  }
  if (has_model_spanner) {
    tt <- tinytable::group_tt(tt, j = spanners)
  }

  # Mirror the table_continuous_lm pipeline exactly:
  #   1. theme_empty() to clear default cell padding / line styling.
  #   2. align ("l" on Variable, "d" on every numeric column).
  #   3. spanner-row centring.
  #   4. APA borders LAST.
  tt <- tinytable::theme_empty(tt)

  tt <- tinytable::style_tt(tt, j = 1L, align = "l")
  if (n_cols >= 2L) {
    for (rj in 2:n_cols) {
      tt <- tinytable::style_tt(tt, j = rj, align = "d")
    }
  }
  # Header rows: Variable column left, the rest centred.
  tt <- tinytable::style_tt(tt, i = 0L, j = 1L, align = "l")
  if (n_cols >= 2L) {
    tt <- tinytable::style_tt(tt, i = 0L, j = 2:n_cols, align = "c")
  }
  # Spanner rows (CI level at i = -1, model level at i = -2 when
  # both present).
  ci_i <- -1L  # CI spanner row index (or -1 alone when only CI)
  model_i <- if (has_ci_spanner) -2L else -1L
  if (has_ci_spanner) {
    tt <- tinytable::style_tt(tt, i = ci_i, j = seq_len(n_cols),
                               align = "c")
    tt <- tinytable::style_tt(tt, i = ci_i, j = 1L, align = "l")
  }
  if (has_model_spanner) {
    tt <- tinytable::style_tt(tt, i = model_i, j = seq_len(n_cols),
                               align = "c")
    tt <- tinytable::style_tt(tt, i = model_i, j = 1L, align = "l")
  }

  # APA borders applied last (table_continuous_lm convention).
  # Top rule above the outermost header row.
  top_i <- if (has_model_spanner) model_i
           else if (has_ci_spanner) ci_i
           else 0L
  tt <- tinytable::style_tt(tt, i = top_i, j = seq_len(n_cols),
                             line = "t", line_width = 0.06)
  # Mid-rule under each model spanner (only over spanned columns).
  if (has_model_spanner) {
    for (lbl in names(spanners)) {
      tt <- tinytable::style_tt(tt, i = model_i, j = spanners[[lbl]],
                                 line = "b", line_width = 0.06)
    }
  }
  # Mid-rule under each CI spanner (only over LL/UL pairs).
  if (has_ci_spanner) {
    for (cs in ci_spanners) {
      tt <- tinytable::style_tt(tt, i = ci_i, j = cs$cols,
                                 line = "b", line_width = 0.06)
    }
  }
  # Sub-rule under column labels.
  tt <- tinytable::style_tt(tt, i = 0L, j = seq_len(n_cols),
                             line = "b", line_width = 0.06)
  if (length(group_sep) >= 1L && group_sep[1L] >= 2L &&
        group_sep[1L] <= nrow(body)) {
    tt <- tinytable::style_tt(tt, i = group_sep[1L] - 1L,
                               j = seq_len(n_cols),
                               line = "b", line_width = 0.03,
                               line_color = "#cccccc")
  }
  if (nrow(body) > 0L) {
    tt <- tinytable::style_tt(tt, i = nrow(body), j = seq_len(n_cols),
                               line = "b", line_width = 0.06)
  }

  # Factor-level rows: indent the Variable cell.
  if (length(level_rows) > 0L) {
    tt <- tinytable::style_tt(
      tt, i = level_rows, j = 1L,
      indent = 1, html_css = "padding-left: 1.4em;"
    )
  }
  tt
}


# ---- gt ------------------------------------------------------------------

output_gt <- function(rendered) {
  if (!spicy_pkg_available("gt")) {
    # nocov start
    spicy_abort(
      c("Output `\"gt\"` requires the 'gt' package.",
        "i" = "Install it with `install.packages(\"gt\")`."),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  title <- attr(rendered, "title")
  note  <- attr(rendered, "note")
  spanners <- attr(rendered, "spanners")
  col_spec <- attr(rendered, "col_spec")
  group_sep <- attr(rendered, "group_sep_rows")
  body <- as.data.frame(rendered, stringsAsFactors = FALSE,
                         check.names = FALSE)
  level_rows <- .detect_level_rows(body)
  body <- .trim_level_indent(body, level_rows)

  # Split bracketed CI cells into LL / UL numeric columns and capture
  # the (95% CI) spanners. With CI delivered as clean numerics,
  # `cols_align_decimal()` aligns every body cell natively -- no
  # monospaced font / white-space: pre hack required for HTML.
  split <- .split_ci_columns(body, col_spec, spanners)
  body         <- split$body
  spanners     <- split$spanners
  ci_spanners  <- split$ci_spanners
  has_model_spanner <- !is.null(spanners) && length(spanners) > 0L
  has_ci_spanner    <- length(ci_spanners) > 0L

  orig_names <- names(body)
  n_cols <- ncol(body)
  tbl <- gt::gt(body)

  # Add CI spanners FIRST so the model spanners nest above them. gt's
  # tab_spanner stacks nested spanners by call order: earlier calls
  # land closer to the column-label row.
  if (has_ci_spanner) {
    for (cs in ci_spanners) {
      tbl <- gt::tab_spanner(
        tbl, label = paste0("(", cs$label, ")"),
        columns = orig_names[cs$cols],
        id = paste0("ci_span_", paste(cs$cols, collapse = "_"))
      )
    }
  }
  if (has_model_spanner) {
    for (lbl in names(spanners)) {
      cols_in_span <- orig_names[spanners[[lbl]]]
      tbl <- gt::tab_spanner(tbl, label = lbl, columns = cols_in_span,
                              id = paste0("model_span_",
                                          make.names(lbl)))
    }
    # Strip the "Model X: " prefix from displayed column labels --
    # the prefix lives only on the unique data.frame column names
    # for gt's lookup; the visible header is the bare token.
    relabel <- list()
    for (lbl in names(spanners)) {
      idx <- spanners[[lbl]]
      prefix <- paste0(lbl, ": ")
      pat <- paste0("^", regex_escape(prefix))
      for (j in idx) {
        relabel[[orig_names[j]]] <- sub(pat, "", orig_names[j])
      }
    }
    if (length(relabel)) {
      tbl <- do.call(gt::cols_label, c(list(tbl), relabel))
    }
  }

  # APA borders + clean default styling. tab_options() suppresses
  # gt's default outer / body hlines so only the explicit APA rules
  # remain (top above the spanner / column-labels row, bottom under
  # the last body row, hair rule between coefs and fit-stats).
  rule <- gt::cell_borders(sides = "bottom", color = "currentColor",
                            weight = gt::px(1))
  rule_top <- gt::cell_borders(sides = "top", color = "currentColor",
                                weight = gt::px(1))
  rule_light <- gt::cell_borders(sides = "bottom", color = "#cccccc",
                                  weight = gt::px(1))
  tbl <- gt::tab_options(
    tbl,
    table.border.top.width = gt::px(0),
    table.border.bottom.width = gt::px(0),
    table_body.border.top.width = gt::px(0),
    table_body.border.bottom.width = gt::px(0),
    table_body.hlines.color = "transparent",
    column_labels.border.top.width = gt::px(0),
    column_labels.border.lr.color = "transparent"
  )
  tbl <- gt::tab_style(
    tbl, style = rule_top,
    locations = gt::cells_column_labels(columns = gt::everything())
  )
  if (nrow(body) > 0L) {
    tbl <- gt::tab_style(
      tbl, style = rule,
      locations = gt::cells_body(rows = nrow(body))
    )
  }
  if (length(group_sep) >= 1L && group_sep[1L] >= 2L &&
        group_sep[1L] <= nrow(body)) {
    tbl <- gt::tab_style(
      tbl, style = rule_light,
      locations = gt::cells_body(rows = group_sep[1L] - 1L)
    )
  }

  # Column-label centring + per-column alignment. With CI split,
  # `cols_align_decimal` aligns every numeric column natively
  # against its decimal mark; the Variable column stays left.
  tbl <- gt::tab_style(
    tbl,
    style = gt::cell_text(align = "center", weight = "normal"),
    locations = gt::cells_column_labels(columns = gt::everything())
  )
  if (has_model_spanner || has_ci_spanner) {
    tbl <- gt::tab_style(
      tbl,
      style = gt::cell_text(align = "center", weight = "normal"),
      locations = gt::cells_column_spanners(spanners = gt::everything())
    )
  }
  tbl <- gt::cols_align(tbl, align = "left", columns = orig_names[1L])
  if (n_cols >= 2L) {
    tbl <- gt::cols_align_decimal(tbl, columns = orig_names[-1L])
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
  }
  if (!is.null(note) && nzchar(note)) {
    tbl <- gt::tab_source_note(tbl, source_note = note)
  }
  tbl
}


# ---- flextable -----------------------------------------------------------

output_flextable <- function(rendered) {
  if (!spicy_pkg_available("flextable")) {
    # nocov start
    spicy_abort(
      c("Output `\"flextable\"` requires the 'flextable' package.",
        "i" = "Install it with `install.packages(\"flextable\")`."),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  body <- as.data.frame(rendered, stringsAsFactors = FALSE,
                         check.names = FALSE)
  spanners <- attr(rendered, "spanners")
  col_spec <- attr(rendered, "col_spec")
  group_sep <- attr(rendered, "group_sep_rows")
  level_rows <- .detect_level_rows(body)
  body <- .trim_level_indent(body, level_rows)

  # Split bracketed CI cells into LL / UL numeric columns. With clean
  # numeric strings, default proportional font + right-align gives
  # decimal alignment via Calibri's tabular figures -- no custom
  # font required.
  split <- .split_ci_columns(body, col_spec, spanners)
  body         <- split$body
  spanners     <- split$spanners
  ci_spanners  <- split$ci_spanners
  has_model_spanner <- !is.null(spanners) && length(spanners) > 0L
  has_ci_spanner    <- length(ci_spanners) > 0L

  orig_names <- names(body)
  n_cols <- ncol(body)
  ft <- flextable::flextable(body)

  # Build the spanner rows. flextable's add_header_row(top = TRUE)
  # stacks rows above the existing header; calling order
  # CI-first then model-second produces top-to-bottom:
  # model row | CI row | column-labels row.
  build_run_spec <- function(label_at_col) {
    # Compress contiguous identical labels into a single span.
    n <- length(label_at_col)
    values <- character(0)
    widths <- integer(0)
    i <- 1L
    while (i <= n) {
      lbl <- label_at_col[i]
      j <- i
      while (j < n && label_at_col[j + 1L] == lbl) j <- j + 1L
      values <- c(values, lbl)
      widths <- c(widths, j - i + 1L)
      i <- j + 1L
    }
    list(values = values, colwidths = widths)
  }

  if (has_ci_spanner) {
    ci_labels_at_col <- rep("", n_cols)
    for (cs in ci_spanners) {
      ci_labels_at_col[cs$cols] <- paste0("(", cs$label, ")")
    }
    rs <- build_run_spec(ci_labels_at_col)
    ft <- flextable::add_header_row(ft, top = TRUE,
                                     values = rs$values,
                                     colwidths = rs$colwidths)
  }
  if (has_model_spanner) {
    model_labels_at_col <- rep("", n_cols)
    for (lbl in names(spanners)) {
      model_labels_at_col[spanners[[lbl]]] <- lbl
    }
    rs <- build_run_spec(model_labels_at_col)
    ft <- flextable::add_header_row(ft, top = TRUE,
                                     values = rs$values,
                                     colwidths = rs$colwidths)
  }
  # Strip the "Model X: " prefix from the column-labels row when a
  # model spanner is present.
  if (has_model_spanner) {
    relabel <- as.list(orig_names)
    names(relabel) <- orig_names
    for (lbl in names(spanners)) {
      idx <- spanners[[lbl]]
      prefix <- paste0(lbl, ": ")
      pat <- paste0("^", regex_escape(prefix))
      for (j in idx) {
        relabel[[orig_names[j]]] <- sub(pat, "", orig_names[j])
      }
    }
    ft <- do.call(flextable::set_header_labels,
                   c(list(x = ft), relabel))
  }

  # APA borders. The header now has up to 3 rows (model spanner row
  # = 1, CI spanner row = 2, column-labels row = 3) -- the last
  # header row is always the column-labels row and is the target of
  # `hline_bottom(part = "header")`.
  bd <- spicy_fp_border(color = "black", width = 1)
  bd_light <- spicy_fp_border(color = "#cccccc", width = 0.5)
  ft <- flextable::hline_top(ft, part = "header", border = bd)
  if (has_model_spanner) {
    # Mid-rule under each model spanner range, drawn on the model
    # spanner row (row index 1 when model spanner is present).
    for (lbl in names(spanners)) {
      ft <- flextable::hline(ft, i = 1L, j = spanners[[lbl]],
                              part = "header", border = bd)
    }
  }
  if (has_ci_spanner) {
    # Mid-rule under each CI spanner pair. The CI spanner row is
    # row 2 if a model spanner sits above it, otherwise row 1.
    ci_row_idx <- if (has_model_spanner) 2L else 1L
    for (cs in ci_spanners) {
      ft <- flextable::hline(ft, i = ci_row_idx, j = cs$cols,
                              part = "header", border = bd)
    }
  }
  ft <- flextable::hline_bottom(ft, part = "header", border = bd)
  if (length(group_sep) >= 1L && group_sep[1L] >= 2L &&
        group_sep[1L] <= nrow(body)) {
    ft <- flextable::hline(ft, i = group_sep[1L] - 1L,
                            part = "body", border = bd_light)
  }
  if (nrow(body) > 0L) {
    ft <- flextable::hline_bottom(ft, part = "body", border = bd)
  }

  # Alignment + font (default proportional Calibri). Right-aligning
  # the numeric body cells against Calibri's tabular figures gives
  # decimal alignment for clean numeric strings -- no custom font
  # required.
  ft <- flextable::align(ft, part = "header", align = "center")
  ft <- flextable::align(ft, j = 1L, part = "header", align = "left")
  ft <- flextable::align(ft, j = 1L, part = "body", align = "left")
  if (n_cols >= 2L) {
    numeric_j <- 2:n_cols
    ft <- flextable::align(ft, j = numeric_j, part = "body",
                            align = "right")
  }
  # Factor-level row indentation.
  if (length(level_rows) > 0L) {
    ft <- flextable::padding(ft, i = level_rows, j = 1L,
                              part = "body", padding.left = 20)
  }
  # Content-driven autofit. plain `autofit()` sizes columns to
  # content; `layout = "autofit"` tells Word to honour the widths.
  ft <- flextable::autofit(ft)
  ft <- flextable::set_table_properties(ft, layout = "autofit")

  title <- attr(rendered, "title")
  note  <- attr(rendered, "note")
  if (!is.null(title) && nzchar(title)) {
    ft <- flextable::set_caption(ft, caption = title)
  }
  if (!is.null(note) && nzchar(note)) {
    ft <- flextable::add_footer_lines(ft, values = note)
  }
  ft
}


# ---- excel ---------------------------------------------------------------

output_excel <- function(rendered, excel_path, excel_sheet) {
  if (!spicy_pkg_available("openxlsx2")) {
    # nocov start
    spicy_abort(
      c("Output `\"excel\"` requires the 'openxlsx2' package.",
        "i" = "Install it with `install.packages(\"openxlsx2\")`."),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  if (is.null(excel_path) || !nzchar(excel_path)) {
    # nocov start -- defensive: validate_output_resources() (Phase F)
    # rejects this upstream, so the dispatch path here is unreachable
    # through table_regression(). Kept for direct callers of
    # dispatch_regression_output() (e.g., bespoke pipelines).
    spicy_abort(
      "`excel_path` must be supplied for output = \"excel\".",
      class = "spicy_invalid_input"
    )
    # nocov end
  }
  body <- as.data.frame(rendered, stringsAsFactors = FALSE,
                         check.names = FALSE)
  title <- attr(rendered, "title")
  note  <- attr(rendered, "note")
  spanners <- attr(rendered, "spanners")
  col_spec <- attr(rendered, "col_spec")
  group_sep <- attr(rendered, "group_sep_rows")

  # Split bracketed CI cells into LL / UL numeric columns and capture
  # the (95% CI) spanners. The Excel sheet ends up with a two-level
  # column header (model spanner > CI spanner > column labels) and
  # each bound becomes its own numeric cell -- Excel formats and
  # right-aligns them natively with the default Calibri font (no
  # custom font + pre-padding hack required).
  split <- .split_ci_columns(body, col_spec, spanners)
  body         <- split$body
  spanners     <- split$spanners
  ci_spanners  <- split$ci_spanners
  n_cols <- ncol(body)
  has_model_spanner <- !is.null(spanners) && length(spanners) > 0L
  has_ci_spanner    <- length(ci_spanners) > 0L

  wb <- openxlsx2::wb_workbook()
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = excel_sheet)

  start_row <- 1L
  if (!is.null(title) && nzchar(title)) {
    wb <- openxlsx2::wb_add_data(wb, sheet = excel_sheet,
                                 x = title, start_row = start_row)
    start_row <- start_row + 2L
  }

  # Write header rows. Layout (from top):
  #   * model spanner row (optional, when multi-model)
  #   * CI spanner row    (optional, when any CI column present)
  #   * sub-column header row (Variable, B, SE, LL, UL, p, ...)
  current_row <- start_row
  model_row_excel <- NA_integer_
  ci_row_excel    <- NA_integer_

  write_spanner_row <- function(wb, label_at_col, row) {
    df <- as.data.frame(as.list(label_at_col),
                        stringsAsFactors = FALSE,
                        col.names = paste0("V", seq_along(label_at_col)))
    names(df) <- names(body)
    openxlsx2::wb_add_data(wb, sheet = excel_sheet,
                            x = df, start_row = row,
                            col_names = FALSE)
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
          wb, sheet = excel_sheet,
          dims = openxlsx2::wb_dims(rows = model_row_excel, cols = idx)
        )
      }
    }
    current_row <- current_row + 1L
  }

  if (has_ci_spanner) {
    ci_row_excel <- current_row
    ci_labels_at_col <- rep("", n_cols)
    for (cs in ci_spanners) {
      ci_labels_at_col[cs$cols] <- paste0("(", cs$label, ")")
    }
    wb <- write_spanner_row(wb, ci_labels_at_col, ci_row_excel)
    for (cs in ci_spanners) {
      wb <- openxlsx2::wb_merge_cells(
        wb, sheet = excel_sheet,
        dims = openxlsx2::wb_dims(rows = ci_row_excel, cols = cs$cols)
      )
    }
    current_row <- current_row + 1L
  }

  header_row_excel <- current_row
  stripped <- if (has_model_spanner) {
    .strip_spanner_prefix(body, spanners)
  } else {
    body
  }
  header_df <- as.data.frame(as.list(names(stripped)),
                              stringsAsFactors = FALSE,
                              col.names = paste0("V", seq_len(n_cols)))
  names(header_df) <- names(body)
  wb <- openxlsx2::wb_add_data(wb, sheet = excel_sheet,
                               x = header_df, start_row = header_row_excel,
                               col_names = FALSE)
  current_row <- current_row + 1L

  body_first_row <- current_row
  wb <- openxlsx2::wb_add_data(wb, sheet = excel_sheet,
                               x = body, start_row = body_first_row,
                               col_names = FALSE)
  body_end_row <- body_first_row + nrow(body) - 1L

  # ---- APA borders -------------------------------------------------------
  # Five rules, mirroring table_continuous / table_continuous_lm:
  #   1. Top rule above the first header row (spanner or sub-header).
  #   2. Spanner mid-rule beneath each spanned range (when spanners
  #      exist) -- structural twin of the CI mid-rule in continuous.
  #   3. Sub-rule below the sub-header row.
  #   4. Hair rule between the last coefficient row and the first
  #      fit-stat row (uses group_sep_rows, the body-index of the
  #      first fit-stat row; -2L shifts into the corresponding
  #      Excel row).
  #   5. Bottom rule below the final body row.
  #
  # IMPORTANT: openxlsx2::wb_add_border() has formal defaults
  # `left_border = right_border = top_border = bottom_border = "thin"`,
  # so an explicit `top_border = "thin"` call paints all four sides
  # unless the others are set to NULL. We pass NULL on every
  # unused side to draw only the intended rule.
  draw_rule <- function(wb, rows, cols,
                         top = NULL, bottom = NULL, weight = "thin") {
    openxlsx2::wb_add_border(
      wb, sheet = excel_sheet,
      dims = openxlsx2::wb_dims(rows = rows, cols = cols),
      top_border    = if (isTRUE(top))    weight else NULL,
      bottom_border = if (isTRUE(bottom)) weight else NULL,
      left_border   = NULL,
      right_border  = NULL
    )
  }
  # Choose the top-of-header row: model spanner if present, otherwise
  # CI spanner, otherwise the column-label row.
  top_rule_row <- if (has_model_spanner) {
    model_row_excel
  } else if (has_ci_spanner) {
    ci_row_excel
  } else {
    header_row_excel
  }
  wb <- draw_rule(wb, rows = top_rule_row, cols = seq_len(n_cols),
                   top = TRUE)
  # Mid-rule under model spanner spans.
  if (has_model_spanner) {
    for (lbl in names(spanners)) {
      wb <- draw_rule(wb, rows = model_row_excel,
                       cols = spanners[[lbl]], bottom = TRUE)
    }
  }
  # Mid-rule under each (95% CI) spanner pair (matches the
  # table_continuous CI mid-rule convention).
  if (has_ci_spanner) {
    for (cs in ci_spanners) {
      wb <- draw_rule(wb, rows = ci_row_excel,
                       cols = cs$cols, bottom = TRUE)
    }
  }
  # Sub-rule under the column-label row.
  wb <- draw_rule(wb, rows = header_row_excel, cols = seq_len(n_cols),
                   bottom = TRUE)
  # Hair rule between last coefficient and first fit-stat row.
  if (length(group_sep) >= 1L && group_sep[1L] >= 2L &&
        group_sep[1L] <= nrow(body)) {
    last_coef_excel <- body_first_row + group_sep[1L] - 2L
    wb <- draw_rule(wb, rows = last_coef_excel, cols = seq_len(n_cols),
                     bottom = TRUE, weight = "hair")
  }
  # Bottom rule under last body row.
  if (nrow(body) > 0L) {
    wb <- draw_rule(wb, rows = body_end_row, cols = seq_len(n_cols),
                     bottom = TRUE)
  }

  # ---- Alignment (default Calibri font, no custom override) ---------------
  # The CI split unifies all numeric body cells into clean numeric
  # strings -- Excel's default Calibri has tabular figures for digits
  # 0-9, so right-aligning the numeric columns visually aligns the
  # decimal points without needing a monospaced font or pre-padding.
  # Variable column: left-aligned. Headers (model spanner, CI spanner,
  # column labels): centred.
  if (n_cols >= 1L) {
    header_rows <- c(model_row_excel, ci_row_excel, header_row_excel)
    header_rows <- header_rows[!is.na(header_rows)]
    wb <- openxlsx2::wb_add_cell_style(
      wb, sheet = excel_sheet,
      dims = openxlsx2::wb_dims(rows = header_rows, cols = seq_len(n_cols)),
      horizontal = "center", vertical = "center"
    )
    if (nrow(body) > 0L) {
      body_rows <- seq.int(body_first_row, body_end_row)
      wb <- openxlsx2::wb_add_cell_style(
        wb, sheet = excel_sheet,
        dims = openxlsx2::wb_dims(rows = body_rows, cols = 1L),
        horizontal = "left"
      )
      if (n_cols >= 2L) {
        numeric_cols <- 2:n_cols
        wb <- openxlsx2::wb_add_cell_style(
          wb, sheet = excel_sheet,
          dims = openxlsx2::wb_dims(rows = body_rows, cols = numeric_cols),
          horizontal = "right"
        )
      }
    }
  }

  if (!is.null(note) && nzchar(note)) {
    foot_row <- body_end_row + 2L
    note_lines <- strsplit(note, "\n", fixed = TRUE)[[1]]
    wb <- openxlsx2::wb_add_data(wb, sheet = excel_sheet,
                                 x = note_lines, start_row = foot_row)
  }

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
      c("Output `\"clipboard\"` requires the 'clipr' package.",
        "i" = "Install it with `install.packages(\"clipr\")`."),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  if (!clipr::clipr_available()) {
    # nocov start -- system clipboard is environment-dependent and
    # typically unavailable on headless CI runners; we test the
    # branch behaviourally elsewhere via mocking.
    spicy_abort(
      c("System clipboard is not available.",
        "i" = "On Linux, install xclip or xsel."),
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

# Build the TSV payload mirroring the Excel layout: title row,
# spanner row, header, body, note rows. Cells are tab-separated.
# The body is delivered with the pre-padding produced by
# render_regression_table() (via decimal_align_strings() and
# align_ci_strings()) so a paste into a fixed-width context (Word
# with Consolas applied; plain-text editor) preserves the decimal
# alignment. Horizontal rules are NOT injected: TSV cannot encode
# a single continuous line, and - rule rows displayed as
# tab-separated dash segments produce a visually broken result.
# This matches table_continuous_lm's clipboard behaviour. Exported
# so the layout is testable without exercising the clipboard
# side-effect.
clipboard_payload <- function(rendered, clipboard_delim) {
  body <- as.data.frame(rendered, stringsAsFactors = FALSE,
                         check.names = FALSE)
  title    <- attr(rendered, "title")
  note     <- attr(rendered, "note")
  spanners <- attr(rendered, "spanners")
  col_spec <- attr(rendered, "col_spec")

  # Split bracketed CI columns into LL / UL (mirrors the rich-output
  # engines and table_continuous*). After the split, each CI bound is
  # an independent numeric cell -- Excel / Word paste handles the
  # values directly, no parsing required downstream.
  split <- .split_ci_columns(body, col_spec, spanners)
  body         <- split$body
  spanners     <- split$spanners
  ci_spanners  <- split$ci_spanners
  n_cols <- ncol(body)
  has_model_spanner <- !is.null(spanners) && length(spanners) > 0L
  has_ci_spanner    <- length(ci_spanners) > 0L

  pad_row <- function(first, n) c(first, rep("", max(0L, n - 1L)))

  rows <- list()
  add_row <- function(r) {
    rows[[length(rows) + 1L]] <<- as.character(r)
  }

  if (!is.null(title) && nzchar(title)) {
    add_row(pad_row(title, n_cols))
  }

  stripped <- if (has_model_spanner) {
    .strip_spanner_prefix(body, spanners)
  } else {
    body
  }

  if (has_model_spanner) {
    # Top header row: model spanner labels repeated across each
    # spanned column. Two-level header layout (top = model, middle =
    # CI) matches table_continuous_lm clipboard output.
    spanner_row <- rep("", n_cols)
    for (lbl in names(spanners)) {
      spanner_row[spanners[[lbl]]] <- lbl
    }
    add_row(spanner_row)
  }

  if (has_ci_spanner) {
    # Middle header row: "(95% CI)" repeated above each LL / UL
    # pair, empty elsewhere. The user can merge the duplicates in
    # Excel / Word after paste.
    ci_row <- rep("", n_cols)
    for (cs in ci_spanners) {
      ci_row[cs$cols] <- paste0("(", cs$label, ")")
    }
    add_row(ci_row)
  }

  add_row(names(stripped))

  body_mat <- as.matrix(stripped)
  if (nrow(body_mat) > 0L) {
    for (i in seq_len(nrow(body_mat))) {
      add_row(body_mat[i, ])
    }
  }

  if (!is.null(note) && nzchar(note)) {
    note_lines <- strsplit(note, "\n", fixed = TRUE)[[1]]
    for (ln in note_lines) add_row(pad_row(ln, n_cols))
  }

  lines <- vapply(rows, paste, character(1), collapse = clipboard_delim)
  paste(lines, collapse = "\n")
}
# nocov end -- closes the `output_clipboard` function block


# ---- word ----------------------------------------------------------------

output_word <- function(rendered, word_path) {
  if (!spicy_pkg_available("flextable") ||
      !spicy_pkg_available("officer")) {
    # nocov start
    spicy_abort(
      c("Output `\"word\"` requires the 'flextable' and 'officer' packages.",
        "i" = "Install with `install.packages(c(\"flextable\", \"officer\"))`."),
      class = "spicy_missing_pkg"
    )
    # nocov end
  }
  if (is.null(word_path) || !nzchar(word_path)) {
    # nocov start -- defensive duplicate of the Phase F
    # validate_output_resources() check (see excel_path comment).
    spicy_abort(
      "`word_path` must be supplied for output = \"word\".",
      class = "spicy_invalid_input"
    )
    # nocov end
  }
  ft <- output_flextable(rendered)
  flextable::save_as_docx(ft, path = word_path)
  invisible(rendered)
}


# ---- print method --------------------------------------------------------

#' @export
print.spicy_regression_table <- function(x, ...) {
  group_sep <- attr(x, "group_sep_rows")
  # nocov start -- defensive: render_regression_table() always sets
  # the attribute (to integer(0) when no fit-stats footer is present).
  # Reached only if a caller manually constructed an object that
  # bypasses the renderer.
  if (is.null(group_sep)) group_sep <- integer(0)
  # nocov end
  align <- attr(x, "align") %||% "decimal"
  spanners <- attr(x, "spanners")

  body <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  # When a spanner row is present, the model label is shown above the
  # sub-columns, so the "Step 1: " prefix in each sub-column name would
  # be redundant. Strip it for the displayed headers (only); the
  # underlying data.frame and the `output = "data.frame"` / `"long"`
  # views keep the unambiguous prefixed names. `check.names = FALSE`
  # above prevents R from suffixing the now-duplicate "B" / "SE" / ...
  # names with `.1`, `.2`, etc.
  if (!is.null(spanners) && length(spanners)) {
    for (lbl in names(spanners)) {
      idx <- spanners[[lbl]]
      prefix <- paste0(lbl, ": ")
      stripped <- sub(paste0("^", regex_escape(prefix)), "",
                       names(body)[idx])
      names(body)[idx] <- stripped
    }
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
        spanners = spanners
      ),
      dot_args
    )
  )
  invisible(x)
}

# Internal: escape regex metacharacters in a string so it can be used
# as a literal pattern. Used for stripping the "Label: " prefix from
# sub-column names without surprises if a model label happens to
# contain "." or "+".
regex_escape <- function(s) {
  gsub("([.\\+*?\\^$()\\[\\]{}|])", "\\\\\\1", s, perl = TRUE)
}
