# Output dispatch for table_regression() — Step 11.
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
#   default     — printable spicy_regression_table (data.frame subclass);
#                 print() delegates to spicy_print_table().
#   data.frame  — plain wide data.frame (stripped of spicy classes)
#   long        — broom-style long tibble (one row per
#                 model_id × term × estimate_type)
#   tinytable   — tinytable::tt() wrapper
#   gt          — gt::gt() wrapper
#   flextable   — flextable::flextable() wrapper
#   excel       — openxlsx2::write_xlsx() side-effect; returns
#                 invisible(rendered)
#   clipboard   — clipr::write_clip() side-effect; returns
#                 invisible(rendered)
#   word        — flextable + officer save_as_docx; returns
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
    # nocov start — defensive: match.arg() above forbids any other
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


# ---- tinytable -----------------------------------------------------------

output_tinytable <- function(rendered) {
  if (!spicy_pkg_available("tinytable")) {
    # nocov start — only reachable when 'tinytable' is not installed;
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
  body <- as.data.frame(rendered, stringsAsFactors = FALSE,
                         check.names = FALSE)
  if (!is.null(spanners) && length(spanners)) {
    body <- .strip_spanner_prefix(body, spanners)
  }
  tt <- tinytable::tt(body,
                       caption = title %||% "",
                       notes = if (!is.null(note)) note else NULL)
  if (!is.null(spanners) && length(spanners)) {
    tt <- tinytable::group_tt(tt, j = spanners)
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
  body <- as.data.frame(rendered, stringsAsFactors = FALSE,
                         check.names = FALSE)
  # gt addresses columns by name internally, so we keep the unique
  # "Step 1: B" / "Step 2: B" names on the data.frame and use
  # `cols_label()` to relabel the displayed headers to the bare
  # tokens ("B" / "SE" / ...). The spanner row is then added via
  # `tab_spanner()`.
  orig_names <- names(body)
  tbl <- gt::gt(body)
  if (!is.null(spanners) && length(spanners)) {
    for (lbl in names(spanners)) {
      cols_in_span <- orig_names[spanners[[lbl]]]
      tbl <- gt::tab_spanner(tbl, label = lbl, columns = cols_in_span)
    }
    relabel <- list()
    for (lbl in names(spanners)) {
      idx <- spanners[[lbl]]
      prefix <- paste0(lbl, ": ")
      pat <- paste0("^", regex_escape(prefix))
      for (j in idx) {
        relabel[[ orig_names[j] ]] <- sub(pat, "", orig_names[j])
      }
    }
    if (length(relabel)) {
      tbl <- do.call(gt::cols_label, c(list(tbl), relabel))
    }
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
  orig_names <- names(body)
  ft <- flextable::flextable(body)
  if (!is.null(spanners) && length(spanners)) {
    # Build a `values` + `colwidths` spec for one prepended header
    # row covering all columns: for each leftmost-to-rightmost column
    # run, emit either a spanner label (for cols inside a span) or an
    # empty string (for cols outside any span — typically the
    # leading "Variable" column).
    n_cols <- ncol(body)
    span_by_col <- character(n_cols)
    for (lbl in names(spanners)) {
      span_by_col[spanners[[lbl]]] <- lbl
    }
    values <- character(0)
    colwidths <- integer(0)
    i <- 1L
    while (i <= n_cols) {
      run_label <- span_by_col[i]
      j <- i
      while (j < n_cols && span_by_col[j + 1L] == run_label) {
        j <- j + 1L
      }
      values <- c(values, run_label)
      colwidths <- c(colwidths, j - i + 1L)
      i <- j + 1L
    }
    ft <- flextable::add_header_row(ft, top = TRUE,
                                     values = values,
                                     colwidths = colwidths)
    # Relabel the sub-column headers (now row 2 of the header) by
    # stripping the model prefix.
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
    ft <- flextable::align(ft, i = 1L, align = "center",
                            part = "header")
  }
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
    # nocov start — defensive: validate_output_resources() (Phase F)
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

  wb <- openxlsx2::wb_workbook()
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = excel_sheet)

  start_row <- 1L
  if (!is.null(title) && nzchar(title)) {
    wb <- openxlsx2::wb_add_data(wb, sheet = excel_sheet,
                                 x = title, start_row = start_row)
    start_row <- start_row + 2L
  }
  if (!is.null(spanners) && length(spanners)) {
    # Write a spanner row above the column headers, merge each
    # label's span, then write the stripped sub-column header row,
    # then the data body (no col_names — we already wrote them).
    n_cols <- ncol(body)
    spanner_row <- rep("", n_cols)
    for (lbl in names(spanners)) {
      spanner_row[spanners[[lbl]]] <- lbl
    }
    spanner_df <- as.data.frame(as.list(spanner_row),
                                 stringsAsFactors = FALSE,
                                 col.names = paste0("V", seq_len(n_cols)))
    names(spanner_df) <- names(body)
    wb <- openxlsx2::wb_add_data(wb, sheet = excel_sheet,
                                 x = spanner_df, start_row = start_row,
                                 col_names = FALSE)
    for (lbl in names(spanners)) {
      idx <- spanners[[lbl]]
      if (length(idx) >= 2L) {
        # `dims = "A1:D1"`-style range string; openxlsx2 deprecated
        # the `rows = / cols =` form in favour of `dims =`.
        dim_range <- paste0(
          openxlsx2::wb_dims(rows = start_row, cols = idx)
        )
        wb <- openxlsx2::wb_merge_cells(
          wb, sheet = excel_sheet, dims = dim_range
        )
      }
    }
    # Sub-column header row (stripped names).
    stripped <- .strip_spanner_prefix(body, spanners)
    header_df <- as.data.frame(as.list(names(stripped)),
                                stringsAsFactors = FALSE,
                                col.names = paste0("V", seq_len(n_cols)))
    names(header_df) <- names(body)
    wb <- openxlsx2::wb_add_data(wb, sheet = excel_sheet,
                                 x = header_df,
                                 start_row = start_row + 1L,
                                 col_names = FALSE)
    wb <- openxlsx2::wb_add_data(wb, sheet = excel_sheet,
                                 x = body, start_row = start_row + 2L,
                                 col_names = FALSE)
    body_end_row <- start_row + 1L + nrow(body)
  } else {
    wb <- openxlsx2::wb_add_data(wb, sheet = excel_sheet,
                                 x = body, start_row = start_row,
                                 col_names = TRUE)
    body_end_row <- start_row + nrow(body)
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

# nocov start — clipboard side effect requires a system clipboard
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
    # nocov start — system clipboard is environment-dependent and
    # typically unavailable on headless CI runners; we test the
    # branch behaviourally elsewhere via mocking.
    spicy_abort(
      c("System clipboard is not available.",
        "i" = "On Linux, install xclip or xsel."),
      class = "spicy_unsupported"
    )
    # nocov end
  }
  # nocov start — write_clip side effect cannot run on headless CI.
  body <- as.data.frame(rendered, stringsAsFactors = FALSE)
  clipr::write_clip(body, sep = clipboard_delim, col.names = TRUE)
  invisible(rendered)
  # nocov end
}
# nocov end — closes the `output_clipboard` function block


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
    # nocov start — defensive duplicate of the Phase F
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
  # nocov start — defensive: render_regression_table() always sets
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
