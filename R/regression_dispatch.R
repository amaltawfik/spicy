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
    spicy_abort(sprintf("Unknown output \"%s\".", output),
                class = "spicy_invalid_input")
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
  if (!requireNamespace("tinytable", quietly = TRUE)) {
    spicy_abort(
      c("Output `\"tinytable\"` requires the 'tinytable' package.",
        "i" = "Install it with `install.packages(\"tinytable\")`."),
      class = "spicy_missing_pkg"
    )
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
  if (!requireNamespace("gt", quietly = TRUE)) {
    spicy_abort(
      c("Output `\"gt\"` requires the 'gt' package.",
        "i" = "Install it with `install.packages(\"gt\")`."),
      class = "spicy_missing_pkg"
    )
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
  if (!requireNamespace("flextable", quietly = TRUE)) {
    spicy_abort(
      c("Output `\"flextable\"` requires the 'flextable' package.",
        "i" = "Install it with `install.packages(\"flextable\")`."),
      class = "spicy_missing_pkg"
    )
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
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    spicy_abort(
      c("Output `\"excel\"` requires the 'openxlsx2' package.",
        "i" = "Install it with `install.packages(\"openxlsx2\")`."),
      class = "spicy_missing_pkg"
    )
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
  if (!requireNamespace("clipr", quietly = TRUE)) {
    spicy_abort(
      c("Output `\"clipboard\"` requires the 'clipr' package.",
        "i" = "Install it with `install.packages(\"clipr\")`."),
      class = "spicy_missing_pkg"
    )
  }
  if (!clipr::clipr_available()) {
    spicy_abort(
      c("System clipboard is not available.",
        "i" = "On Linux, install xclip or xsel."),
      class = "spicy_unsupported"
    )
  }
  body <- as.data.frame(rendered, stringsAsFactors = FALSE)
  clipr::write_clip(body, sep = clipboard_delim, col.names = TRUE)
  invisible(rendered)
}


# ---- word ----------------------------------------------------------------

output_word <- function(rendered, word_path) {
  if (!requireNamespace("flextable", quietly = TRUE) ||
      !requireNamespace("officer", quietly = TRUE)) {
    spicy_abort(
      c("Output `\"word\"` requires the 'flextable' and 'officer' packages.",
        "i" = "Install with `install.packages(c(\"flextable\", \"officer\"))`."),
      class = "spicy_missing_pkg"
    )
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
  spicy_print_table(
    as.data.frame(x, stringsAsFactors = FALSE),
    title = attr(x, "title"),
    note = attr(x, "note"),
    ...
  )
  invisible(x)
}
