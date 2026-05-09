# Output dispatch for table_regression() — Step 11.
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
  body <- as.data.frame(rendered, stringsAsFactors = FALSE)
  tinytable::tt(body,
                caption = title %||% "",
                notes = if (!is.null(note)) note else NULL)
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
  body <- as.data.frame(rendered, stringsAsFactors = FALSE)
  tbl <- gt::gt(body)
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
  body <- as.data.frame(rendered, stringsAsFactors = FALSE)
  ft <- flextable::flextable(body)
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
    spicy_abort(
      "`excel_path` must be supplied for output = \"excel\".",
      class = "spicy_invalid_input"
    )
  }
  body <- as.data.frame(rendered, stringsAsFactors = FALSE)
  title <- attr(rendered, "title")
  note  <- attr(rendered, "note")

  wb <- openxlsx2::wb_workbook()
  wb <- openxlsx2::wb_add_worksheet(wb, sheet = excel_sheet)

  start_row <- 1L
  if (!is.null(title) && nzchar(title)) {
    wb <- openxlsx2::wb_add_data(wb, sheet = excel_sheet,
                                 x = title, start_row = start_row)
    start_row <- start_row + 2L
  }
  wb <- openxlsx2::wb_add_data(wb, sheet = excel_sheet,
                               x = body, start_row = start_row,
                               col_names = TRUE)
  if (!is.null(note) && nzchar(note)) {
    foot_row <- start_row + nrow(body) + 2L
    note_lines <- strsplit(note, "\n", fixed = TRUE)[[1]]
    wb <- openxlsx2::wb_add_data(wb, sheet = excel_sheet,
                                 x = note_lines, start_row = foot_row)
  }
  openxlsx2::wb_save(wb, file = excel_path, overwrite = TRUE)
  invisible(rendered)
}


# ---- clipboard -----------------------------------------------------------

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
    spicy_abort(
      "`word_path` must be supplied for output = \"word\".",
      class = "spicy_invalid_input"
    )
  }
  ft <- output_flextable(rendered)
  flextable::save_as_docx(ft, path = word_path)
  invisible(rendered)
}


# ---- print method --------------------------------------------------------

#' @export
print.spicy_regression_table <- function(x, ...) {
  group_sep <- attr(x, "group_sep_rows")
  if (is.null(group_sep)) group_sep <- integer(0)
  align <- attr(x, "align") %||% "decimal"

  body <- as.data.frame(x, stringsAsFactors = FALSE)
  data_col_idx <- setdiff(seq_along(body), 1L)
  align_center_cols <- if (identical(align, "center")) {
    data_col_idx
  } else {
    integer(0)
  }
  # "decimal" alignment was pre-applied in the renderer (cells are
  # already padded). "right" / "auto" delegate to the print engine
  # defaults. "center" forces center alignment of data columns.
  spicy_print_table(
    body,
    title = attr(x, "title"),
    note = attr(x, "note"),
    group_sep_rows = group_sep,
    align_center_cols = align_center_cols,
    ...
  )
  invisible(x)
}
