# dev/gen_fr_review.R
#
# The review view of the French label set (GitHub issue #7). Regenerates
#
#   dev/i18n_fr_review_table.md     every registry key in reading order
#                                   (key, English, French, where it appears,
#                                   status), plus the title-prefix bridge
#   dev/i18n_fr_review_samples.md   four French tables, console output
#
# from the working tree's R/i18n.R and R/i18n_fr.R, the French set of
# 2026-08-23 in git (the one decision 38 was taken on), and the review marks
# in dev/i18n_fr_review_marks.tsv. The marks file is the only state: it is
# merged on every run, so regenerating never loses a mark.
#
# Run from the package root:
#
#   Rscript dev/gen_fr_review.R                      table and samples
#   Rscript dev/gen_fr_review.R --no-samples         table only
#   Rscript dev/gen_fr_review.R --mark reviewed <key> [<key> ...]
#   Rscript dev/gen_fr_review.R --mark corrected <key> [<key> ...]
#
# --mark records the marks and regenerates the table only.
# Base R, plus pkgload for the samples. Writes nothing outside dev/.
# Replaces the gen_review.R of 2026-08-23, which lived in a session
# scratchpad and was never committed.

args <- commandArgs(trailingOnly = TRUE)

if (!file.exists("R/i18n.R") || !file.exists("R/i18n_fr.R")) {
  stop("Run from the package root: Rscript dev/gen_fr_review.R", call. = FALSE)
}

PATH_TABLE <- "dev/i18n_fr_review_table.md"
PATH_SAMPLES <- "dev/i18n_fr_review_samples.md"
PATH_MARKS <- "dev/i18n_fr_review_marks.tsv"
# The French set when decision 38 was taken (208 entries): "new" and
# "changed" are measured against it.
BASELINE_REF <- "6fc0477d"
BASELINE_DATE <- "2026-08-23"
TODAY <- format(Sys.Date())
MARK_VALUES <- c("reviewed", "corrected")


# ---- Arguments --------------------------------------------------------------

mark_request <- NULL
at <- match("--mark", args)
if (!is.na(at)) {
  rest <- setdiff(args[-seq_len(at)], "--no-samples")
  if (length(rest) < 2L || !rest[[1L]] %in% MARK_VALUES) {
    stop(
      "Usage: Rscript dev/gen_fr_review.R --mark <reviewed|corrected> <key> ...",
      call. = FALSE
    )
  }
  mark_request <- list(mark = rest[[1L]], keys = unique(rest[-1L]))
}
stray_args <- setdiff(
  args,
  c("--no-samples", "--mark", mark_request$mark, mark_request$keys)
)
if (length(stray_args)) {
  stop(
    "Unknown argument(s): ",
    paste(stray_args, collapse = " "),
    call. = FALSE
  )
}
# Marks change the table, not the samples, so marking skips the slow part.
want_samples <- !("--no-samples" %in% args) && is.null(mark_request)


# ---- Helpers ----------------------------------------------------------------

.read_utf8 <- function(path) {
  sub("\r$", "", readLines(path, encoding = "UTF-8", warn = FALSE))
}

# UTF-8, LF line endings, whatever the platform.
.write_utf8 <- function(lines, path) {
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(
    charToRaw(paste0(paste(enc2utf8(lines), collapse = "\n"), "\n")),
    con
  )
}

.eval_lines <- function(lines) {
  env <- new.env(parent = globalenv())
  eval(
    parse(text = lines, keep.source = FALSE, encoding = "UTF-8"),
    envir = env
  )
  env
}

.git <- function(...) {
  out <- suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  if (!is.null(st) && st != 0L) {
    stop(
      "`git ",
      paste(c(...), collapse = " "),
      "` failed:\n",
      paste(out, collapse = "\n"),
      call. = FALSE
    )
  }
  out
}

# A string as the review shows it: the no-break space made visible, line
# breaks and tabs written as escapes.
.show <- function(x) {
  x <- gsub(" ", "<NBSP>", x, fixed = TRUE)
  x <- gsub("\n", "\\\\n", x)
  gsub("\t", "\\\\t", x)
}

# A verbatim string as a Markdown code span, safe inside a table cell.
.code <- function(x) {
  if (is.null(x) || is.na(x)) {
    return("--")
  }
  x <- gsub("\\|", "\\\\|", .show(x))
  fence <- if (grepl("`", x, fixed = TRUE)) "``" else "`"
  # CommonMark strips one space from each end of a code span only when both
  # ends have one, and a backtick at an end would merge with the fence: pad
  # only then, so the padding is the space that gets stripped.
  if (grepl("^ .* $", x) || grepl("^`|`$", x)) {
    x <- paste0(" ", x, " ")
  }
  paste0(fence, x, fence)
}

.hash8 <- function(x) {
  raw <- charToRaw(enc2utf8(x))
  if ("bytes" %in% names(formals(tools::md5sum))) {
    h <- tools::md5sum(bytes = raw)
  } else {
    f <- tempfile()
    on.exit(unlink(f))
    writeBin(raw, f)
    h <- tools::md5sum(f)
  }
  substr(unname(h), 1L, 8L)
}

.keys_inline <- function(keys) paste0("`", keys, "`", collapse = ", ")

# Equal strings, NA equal to NA, whatever encoding mark each one carries.
.same <- function(a, b) {
  if (is.na(a) || is.na(b)) {
    return(is.na(a) && is.na(b))
  }
  enc2utf8(a) == enc2utf8(b)
}


# ---- The registries ---------------------------------------------------------

lines_en <- .read_utf8("R/i18n.R")
env_en <- .eval_lines(lines_en)
env_fr <- .eval_lines(.read_utf8("R/i18n_fr.R"))
env_base <- .eval_lines(.git("show", paste0(BASELINE_REF, ":R/i18n_fr.R")))
EN <- env_en$.spicy_strings
FR <- env_fr$.spicy_strings_fr
FR_BASE <- env_base$.spicy_strings_fr
BRIDGE <- env_fr$.FR_TITLE_PREFIXES
BRIDGE_BASE <- env_base$.FR_TITLE_PREFIXES
if (is.null(BRIDGE_BASE)) {
  BRIDGE_BASE <- character(0)
}

stray_fr <- setdiff(names(FR), names(EN))
if (length(stray_fr)) {
  stop(
    "French entries with no registry key: ",
    paste(stray_fr, collapse = ", "),
    call. = FALSE
  )
}

head_hash <- .git("rev-parse", "--short", "HEAD")
dirty <- .git("status", "--porcelain", "--", "R/i18n.R", "R/i18n_fr.R")

# The registry section of every key, read off the `# -- title ---` comments
# of `.spicy_strings`. A header ends with a dash rule, on its own line or on
# the comment lines that continue it (then its text is kept up to the first
# colon); a comment that merely starts with "-- " is prose, not a header.
.key_sections <- function(lines) {
  start <- grep("^\\.spicy_strings <- c\\(\\s*$", lines)
  if (length(start) != 1L) {
    stop(
      "R/i18n.R: `.spicy_strings <- c(` not found; update .key_sections().",
      call. = FALSE
    )
  }
  is_header <- function(i) {
    if (!grepl("^\\s*# -- ", lines[[i]])) {
      return(FALSE)
    }
    j <- i
    while (j <= length(lines) && grepl("^\\s*#", lines[[j]])) {
      if (grepl("-{3,}\\s*$", lines[[j]])) {
        return(TRUE)
      }
      j <- j + 1L
    }
    FALSE
  }
  sec <- NA_character_
  key <- character()
  section <- character()
  for (i in seq(start + 1L, length(lines))) {
    ln <- lines[[i]]
    if (grepl("^\\)", ln)) {
      break
    }
    if (is_header(i)) {
      s <- sub("^\\s*# -- ", "", ln)
      sec <- if (grepl("-{3,}\\s*$", s)) {
        sub("\\s*-{3,}\\s*$", "", s)
      } else {
        sub(":.*$", "", s)
      }
      next
    }
    m <- regmatches(ln, regexec("^\\s*([a-z][a-z0-9_]*) = ", ln))[[1L]]
    if (length(m)) {
      key <- c(key, m[[2L]])
      section <- c(section, sec)
    }
  }
  data.frame(key = key, section = section, stringsAsFactors = FALSE)
}

REG <- .key_sections(lines_en)
if (!setequal(REG$key, names(EN)) || anyDuplicated(REG$key)) {
  stop(
    "The section parser does not find every key of R/i18n.R: the registry ",
    "format moved, update .key_sections().",
    call. = FALSE
  )
}


# ---- Where each key appears -------------------------------------------------

.FILE_FUNCTION <- c(
  freq_print = "freq()",
  cross_tab = "cross_tab()",
  table_categorical = "table_categorical()",
  table_categorical_print = "table_categorical()",
  table_categorical_svy = "table_categorical_svy()",
  table_categorical_svy_print = "table_categorical_svy()",
  table_categorical_svy_render = "table_categorical_svy()",
  table_continuous = "table_continuous()",
  table_continuous_print = "table_continuous()",
  table_continuous_svy = "table_continuous_svy()",
  table_continuous_lm = "table_continuous_lm()",
  table_continuous_lm_print = "table_continuous_lm()",
  table_continuous_lm_render = "table_continuous_lm()",
  table_outcome = "table_outcome()",
  table_multiresponse = "table_multiresponse()",
  table_multiresponse_print = "table_multiresponse()",
  survey_helpers = "survey twins",
  table_helpers = "descriptive tables",
  table_regression = "table_regression()",
  glm_compute = "table_regression()",
  vcov = "table_regression()",
  code_book = "code_book()",
  "code_book-xlsx" = "code_book() Excel",
  "code_book-typst" = "code_book() PDF",
  varlist = "varlist()",
  "varlist-title" = "varlist()",
  "varlist-values" = "varlist()",
  inline = "inline()",
  assoc = "association measures",
  tables_structured = "as_structured()",
  regression_structured = "as_structured()",
  tables_ascii = "console printer",
  tt_theme = "tinytable output"
)

.function_of <- function(file) {
  if (file %in% names(.FILE_FUNCTION)) {
    return(.FILE_FUNCTION[[file]])
  }
  if (startsWith(file, "regression_")) {
    return("table_regression()")
  }
  paste0(file, ".R")
}

.used_in <- function(files) {
  if (!length(files)) {
    return("--")
  }
  f <- unique(vapply(files, .function_of, character(1)))
  f <- f[order(match(f, unique(.FILE_FUNCTION)), f)]
  if (length(f) > 4L) {
    f <- c(f[1:4], sprintf("+%d more", length(f) - 4L))
  }
  paste(f, collapse = ", ")
}

# Every R/ file (but the registries) that names the key as a literal.
.callers <- function(keys) {
  files <- setdiff(
    list.files("R", pattern = "[.]R$", full.names = TRUE),
    c("R/i18n.R", "R/i18n_fr.R")
  )
  flat <- vapply(
    files,
    function(f) paste(.read_utf8(f), collapse = "\n"),
    character(1)
  )
  names(flat) <- sub("[.]R$", "", basename(files))
  lapply(keys, function(k) {
    names(flat)[grepl(paste0("[\"']", k, "[\"']"), flat)]
  })
}

.GROUPS <- c(
  title = "Titles and header lines",
  header = "Column headers",
  row = "Row labels",
  cell = "Cells",
  note = "Footer notes",
  excel = "Excel sheet names",
  message = "Messages"
)

# The role of a key, from its prefix and its callers.
.role <- function(k, files) {
  r <- function(group, where) list(group = group, where = where)
  in_codebook <- length(files) && all(startsWith(files, "code_book"))
  if (k == "note_model_prefix") {
    return(r("message", "error message (lead line)"))
  }
  if (
    k %in%
      c(
        "title_crosstab_by",
        "title_crosstab_group",
        "title_percent_row",
        "title_percent_column",
        "title_percent_none",
        "title_continuous_lm_by_fallback",
        "title_regression_fallback"
      )
  ) {
    return(r("title", "title fragment"))
  }
  if (startsWith(k, "title_")) {
    return(r(
      "title",
      if (identical(files, "code_book-typst")) "title (PDF)" else "title"
    ))
  }
  if (startsWith(k, "cover_")) {
    return(r("title", "PDF cover (date)"))
  }
  if (k == "note_field_line") {
    return(r("title", "header line (console)"))
  }
  if (k == "note_codebook_continued") {
    return(r("title", "heading suffix (PDF)"))
  }
  if (k == "label_values") {
    return(r("header", "column header"))
  }
  if (k == "label_model_name") {
    return(r("header", "column spanner; footer note"))
  }
  if (k == "header_ci_spanner") {
    return(r("header", "column spanner"))
  }
  if (k == "header_n_lower") {
    return(r("header", "column header; regression fit-statistic row"))
  }
  if (startsWith(k, "header_")) {
    return(r("header", "column header"))
  }
  if (startsWith(k, "stat_")) {
    return(r("header", "column header; footer note"))
  }
  if (startsWith(k, "symbol_star_")) {
    return(r("cell", "cell (significance stars)"))
  }
  if (startsWith(k, "symbol_")) {
    return(r("header", "glyph (header, row label, note)"))
  }
  if (startsWith(k, "fitstat_") || k == "label_weighted_n") {
    return(r("row", "row label (fit statistic)"))
  }
  if (startsWith(k, "label_r2_")) {
    return(r("row", "row label (R² qualifier)"))
  }
  if (startsWith(k, "label_block_")) {
    return(r("row", "row label (block caption)"))
  }
  if (k %in% c("label_ref", "label_ref_annotation", "label_vs_annotation")) {
    return(r("row", "row label (reference level)"))
  }
  if (identical(k, "row_missing_level")) {
    return(r("row", "row label; column name under `by`"))
  }
  if (startsWith(k, "row_") || startsWith(k, "label_")) {
    return(r(
      "row",
      if (in_codebook) "row label (codebook field)" else "row label"
    ))
  }
  if (startsWith(k, "value_summary_")) {
    return(r("cell", "cell (value summary)"))
  }
  if (startsWith(k, "marker_")) {
    return(r("cell", "cell (marker)"))
  }
  if (startsWith(k, "sep_")) {
    return(r("cell", "cell (separator)"))
  }
  if (startsWith(k, "cell_")) {
    return(r("cell", "cell"))
  }
  if (startsWith(k, "excel_sheet_")) {
    return(r("excel", "Excel sheet name"))
  }
  if (startsWith(k, "test_")) {
    return(r("note", "footer note (test name)"))
  }
  if (startsWith(k, "note_")) {
    return(r("note", "footer note"))
  }
  r("note", "unclassified prefix")
}


# ---- Statuses ---------------------------------------------------------------

# Keys with no French entry, and why. A key absent from both this list and
# the French set is a GAP ("missing: no French entry"): an English string a
# French reader sees, until it gets a French entry or a status here.
.ABSENT <- list(
  "frozen: template" = c(
    "note_missing_item",
    "title_crosstab_group",
    "header_percent_symbol",
    "header_group_qualified",
    "note_kv_pair",
    "header_with_ci_suffix",
    "header_ame_by_category",
    "header_companion_qualified",
    "fitstat_alpha",
    "fitstat_change_prefix",
    "note_re_method",
    "note_re_line",
    "note_estimand_boot_range",
    "header_med_iqr_composite",
    "header_ci_bound",
    "note_lm_delta",
    "note_group_comparison_item",
    "note_list_sep_comma",
    "value_summary_units",
    "header_pct_total"
  ),
  "frozen: notation" = c(
    "symbol_t",
    "symbol_z",
    "symbol_f",
    "symbol_chi_sq",
    "symbol_beta",
    "symbol_delta",
    "symbol_cohens_d",
    "symbol_hedges_g",
    "symbol_eta_sq_partial",
    "symbol_omega_sq_global",
    "symbol_omega_sq_partial",
    "symbol_f2_global",
    "symbol_f2_partial",
    "symbol_r2",
    "symbol_sigma_hat",
    "cell_undefined",
    "marker_na",
    "marker_nan",
    "marker_ellipsis_values",
    "marker_truncation_ellipsis",
    "marker_varlist_transformed",
    "symbol_star_001",
    "symbol_star_01",
    "symbol_star_05"
  ),
  # The exponentiated headers are resolved in the model frame and reach the
  # column name as text. "Model N" (`label_model_name`) left this list on
  # 2026-10-24: the column names now take it from the English registry,
  # so it has a French entry.
  "frozen: column name" = c(
    "header_exp_or",
    "header_exp_irr",
    "header_exp_hr",
    "header_exp_rr",
    "header_exp_mr",
    "header_exp_tr",
    "header_exp_generic",
    "header_model_prefixed"
  ),
  # "frozen: column name" on 2026-08-23. Since b8d07195 the column name
  # comes from the English registry whatever the header says
  # (build_column_spec()), and R/i18n_fr.R keeps these English because any
  # reader reads them as written.
  "kept: read as written" = c(
    "header_n_upper",
    "header_b",
    "header_p",
    "header_pd",
    "header_rhat",
    "header_ess_bulk",
    "header_ess_tail",
    "header_mcse",
    "header_rmst",
    "header_rmst_no_horizon",
    "header_risk_diff",
    "header_risk_diff_no_horizon",
    "header_ame",
    "header_ci_label_hdi",
    "header_n_lower"
  ),
  "kept: term of art (decision 38)" = c(
    "note_abbrev_hr",
    "value_summary_array"
  ),
  "kept: condition message" = "note_model_prefix",
  "same in French (confirmed)" = "excel_sheet_continuous",
  "same in French" = c(
    "title_crosstab_by",
    "title_percent_none",
    "label_total",
    "header_variable",
    "header_margin_total",
    "stat_phi",
    "stat_lambda",
    "header_deff",
    "fitstat_pseudo_r2",
    "fitstat_r2_qualified",
    "label_r2_marginal",
    "fitstat_theta",
    "fitstat_qic",
    "fitstat_qicu",
    "fitstat_elpd_loo",
    "fitstat_looic",
    "fitstat_waic",
    "fitstat_icc",
    "fitstat_rmse",
    "fitstat_aic",
    "fitstat_aicc",
    "fitstat_bic",
    "label_block_dispersion",
    "label_vs_annotation",
    "fitstat_n_groups",
    "note_prefix",
    "note_prefix_emphasis",
    "header_mean",
    "header_q1",
    "header_q3",
    "header_min",
    "header_max",
    "header_test",
    "note_vcov_jackknife_plain",
    "note_p_adjust_method_holm",
    "note_p_adjust_method_hochberg",
    "note_p_adjust_method_hommel",
    "note_p_adjust_method_bonferroni",
    "note_p_adjust_method_bh",
    "note_p_adjust_method_by",
    "note_exp_scope_all",
    # code_book(), added after 2026-08-23
    "excel_sheet_codebook",
    "excel_sheet_codebook_variables",
    "header_position",
    "header_type",
    "header_source",
    "row_date",
    "row_observations",
    "row_variables",
    "row_note",
    "cell_declared_one",
    "cell_declared_many",
    "cell_type_date",
    "title_codebook",
    "header_page",
    "header_variables",
    "row_notes",
    "marker_declared_missing"
  )
)
ABSENT_STATUS <- unlist(lapply(names(.ABSENT), function(s) {
  stats::setNames(rep(s, length(.ABSENT[[s]])), .ABSENT[[s]])
}))
if (anyDuplicated(names(ABSENT_STATUS))) {
  stop("A key is classified twice in .ABSENT.", call. = FALSE)
}

# Translated keys whose French was settled by decision 38.
ARBITRATED_38 <- c(
  "header_sd",
  "header_smd",
  "header_iqr",
  "header_freq",
  "note_design_psu",
  "note_design_calibrated",
  "row_overall",
  "row_outcome",
  "excel_sheet_outcome",
  "note_weight_rescaled",
  "label_block_zero_hurdle"
)

.STATUS_INFO <- c(
  "to review" = sprintf(
    "French entry unchanged since %s, never read through as a whole",
    BASELINE_DATE
  ),
  "to review (new)" = sprintf("French entry added after %s", BASELINE_DATE),
  "to review (changed)" = sprintf(
    "French entry reworded after %s",
    BASELINE_DATE
  ),
  "to review (changed after mark)" = "marked, then the French string changed: read it again and mark it again",
  "reviewed" = "read in context and kept (marks file)",
  "corrected" = "fixed in `R/i18n_fr.R`, then read in context (marks file)",
  "arbitrated (decision 38)" = sprintf(
    "terminology call settled by decision 38, French unchanged since %s",
    BASELINE_DATE
  ),
  "missing: no French entry" = "an English string a French reader sees: it needs a French entry, or a status in the generator",
  "kept: term of art (decision 38)" = "the field reads it in English",
  "kept: condition message" = "error text, and errors stay English",
  "kept: read as written" = "a regression header any reader reads as written; \"frozen: column name\" on 2026-08-23, but since b8d07195 translating it would no longer move the column name",
  "same in French (confirmed)" = "checked, not assumed: \"Descriptives\" is French too",
  "same in French" = "the French word IS the English one; the fallback serves it",
  "frozen: notation" = "a glyph or a marker; notation is international",
  "frozen: template" = "the value is holes and punctuation only",
  "frozen: column name" = "translating it would move a public column name (exponentiated headers, resolved in the model frame)"
)
TO_REVIEW <- c(
  "to review",
  "to review (new)",
  "to review (changed)",
  "to review (changed after mark)",
  "missing: no French entry"
)
MARKED <- c("reviewed", "corrected")

.base_status <- function(k, cur, base, absent_status) {
  if (!is.na(cur)) {
    if (!k %in% names(base)) {
      return("to review (new)")
    }
    if (!.same(unname(base[[k]]), unname(cur))) {
      return("to review (changed)")
    }
    if (k %in% ARBITRATED_38) {
      return("arbitrated (decision 38)")
    }
    return("to review")
  }
  if (k %in% names(absent_status)) {
    return(absent_status[[k]])
  }
  "missing: no French entry"
}


# ---- One row per key, then the bridge ----------------------------------------

callers <- .callers(REG$key)
roles <- Map(.role, REG$key, callers)
rows <- data.frame(
  key = REG$key,
  en = unname(EN[REG$key]),
  fr = unname(ifelse(REG$key %in% names(FR), FR[REG$key], NA_character_)),
  group = vapply(roles, `[[`, character(1), "group"),
  where = vapply(roles, `[[`, character(1), "where"),
  used_in = vapply(callers, .used_in, character(1)),
  section = REG$section,
  stringsAsFactors = FALSE
)
rows$status <- vapply(
  seq_len(nrow(rows)),
  function(i) {
    .base_status(rows$key[[i]], rows$fr[[i]], FR_BASE, ABSENT_STATUS)
  },
  character(1)
)

.bridge_key <- function(en) {
  if (!length(en)) {
    return(character(0))
  }
  paste0("bridge:", gsub("_+$", "", gsub("[^a-z0-9]+", "_", tolower(en))))
}
bridge_rows <- data.frame(
  key = .bridge_key(names(BRIDGE)),
  en = names(BRIDGE),
  fr = unname(BRIDGE),
  group = "title",
  where = "title prefix",
  used_in = "table_regression()",
  section = "the title-prefix bridge (`.FR_TITLE_PREFIXES` in R/i18n_fr.R, not registry keys)",
  stringsAsFactors = FALSE
)
bridge_base <- stats::setNames(
  unname(BRIDGE_BASE),
  .bridge_key(names(BRIDGE_BASE))
)
bridge_rows$status <- vapply(
  seq_len(nrow(bridge_rows)),
  function(i) {
    .base_status(
      bridge_rows$key[[i]],
      bridge_rows$fr[[i]],
      bridge_base,
      character(0)
    )
  },
  character(1)
)
if (anyDuplicated(bridge_rows$key)) {
  stop("Two bridge entries share a key slug.", call. = FALSE)
}
rows <- rbind(rows, bridge_rows)
rows$order <- seq_len(nrow(rows))
# What a French reader sees: the French entry, or the English default.
rows$resolved <- ifelse(is.na(rows$fr), rows$en, rows$fr)


# ---- The marks ----------------------------------------------------------------

.MARKS_HEADER <- c(
  "# Review marks for the French label set (issue #7). dev/gen_fr_review.R",
  "# merges them into dev/i18n_fr_review_table.md on every run.",
  "#",
  "# One line per key, tab-separated: key, mark, date, fr_at_mark, note.",
  "# A hand-typed line can be just `<key> reviewed` or `<key> corrected`:",
  "# the generator fills in the date and the French string the mark applies",
  "# to (fr_at_mark, no-break spaces written <NBSP>), then rewrites the line",
  "# with tabs. Or run: Rscript dev/gen_fr_review.R --mark reviewed <key> ...",
  "#",
  "#   reviewed    read in context and kept as is",
  "#   corrected   fixed in R/i18n_fr.R, then read in context",
  "#",
  "# When the French string later differs from fr_at_mark, the row turns",
  "# \"to review (changed after mark)\": read it again, then empty fr_at_mark",
  "# (or run --mark again) to accept the new string.",
  "#",
  "# Title-prefix bridge rows are marked by their bridge: key."
)
.MARKS_COLUMNS <- c("key", "mark", "date", "fr_at_mark", "note")

.read_marks <- function(path) {
  empty <- data.frame(
    key = character(),
    mark = character(),
    date = character(),
    fr_at_mark = character(),
    note = character(),
    stringsAsFactors = FALSE
  )
  if (!file.exists(path)) {
    return(list(comments = .MARKS_HEADER, rows = empty, dirty = TRUE))
  }
  lines <- .read_utf8(path)
  comments <- lines[grepl("^\\s*#", lines)]
  data <- which(!grepl("^\\s*#", lines) & nzchar(trimws(lines)))
  out <- empty
  for (i in data) {
    ln <- lines[[i]]
    if (grepl("\t", ln, fixed = TRUE)) {
      f <- strsplit(ln, "\t", fixed = TRUE)[[1L]]
    } else {
      # A hand-typed line without tabs: key, mark, then a date or a note.
      f <- strsplit(trimws(ln), "\\s+")[[1L]]
      if (length(f) >= 3L && !grepl("^\\d{4}-\\d{2}-\\d{2}$", f[[3L]])) {
        f <- c(f[1:2], "", "", paste(f[-(1:2)], collapse = " "))
      }
    }
    if (length(f) > 5L) {
      f <- c(f[1:4], paste(f[-(1:4)], collapse = " "))
    }
    f <- c(f, rep("", 5L - length(f)))
    f[1:3] <- trimws(f[1:3])
    if (identical(f[[1L]], "key")) {
      next
    }
    if (!f[[2L]] %in% MARK_VALUES) {
      stop(
        sprintf(
          "%s, line %d: the mark must be \"reviewed\" or \"corrected\", not \"%s\".",
          path,
          i,
          f[[2L]]
        ),
        call. = FALSE
      )
    }
    if (nzchar(f[[3L]]) && !grepl("^\\d{4}-\\d{2}-\\d{2}$", f[[3L]])) {
      stop(
        sprintf("%s, line %d: the date must be YYYY-MM-DD.", path, i),
        call. = FALSE
      )
    }
    out[nrow(out) + 1L, ] <- as.list(f)
  }
  if (anyDuplicated(out$key)) {
    stop(
      sprintf(
        "%s marks a key twice: %s. Keep one line per key.",
        path,
        paste(unique(out$key[duplicated(out$key)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  list(
    comments = if (length(comments)) comments else .MARKS_HEADER,
    rows = out,
    dirty = FALSE
  )
}

marks <- .read_marks(PATH_MARKS)

if (!is.null(mark_request)) {
  bad <- setdiff(mark_request$keys, rows$key)
  if (length(bad)) {
    stop("No such key: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  for (k in mark_request$keys) {
    i <- match(k, marks$rows$key)
    if (is.na(i)) {
      i <- nrow(marks$rows) + 1L
      marks$rows[i, ] <- list(k, "", "", "", "")
    }
    marks$rows$mark[[i]] <- mark_request$mark
    marks$rows$date[[i]] <- ""
    marks$rows$fr_at_mark[[i]] <- ""
  }
  marks$dirty <- TRUE
}

# Fill in what a hand-typed line left empty.
resolved_show <- stats::setNames(.show(rows$resolved), rows$key)
for (i in seq_len(nrow(marks$rows))) {
  k <- marks$rows$key[[i]]
  if (!nzchar(marks$rows$date[[i]])) {
    marks$rows$date[[i]] <- TODAY
    marks$dirty <- TRUE
  }
  if (!nzchar(marks$rows$fr_at_mark[[i]]) && k %in% names(resolved_show)) {
    marks$rows$fr_at_mark[[i]] <- resolved_show[[k]]
    marks$dirty <- TRUE
  }
}
if (isTRUE(marks$dirty)) {
  body <- if (nrow(marks$rows)) {
    apply(marks$rows[, .MARKS_COLUMNS], 1L, paste, collapse = "\t")
  } else {
    character(0)
  }
  .write_utf8(
    c(marks$comments, paste(.MARKS_COLUMNS, collapse = "\t"), body),
    PATH_MARKS
  )
}

orphan_marks <- setdiff(marks$rows$key, rows$key)
stale_marks <- character(0)
for (i in seq_len(nrow(marks$rows))) {
  k <- marks$rows$key[[i]]
  if (!k %in% rows$key) {
    next
  }
  j <- match(k, rows$key)
  if (.same(marks$rows$fr_at_mark[[i]], resolved_show[[k]])) {
    rows$status[[j]] <- marks$rows$mark[[i]]
  } else {
    rows$status[[j]] <- "to review (changed after mark)"
    stale_marks <- c(stale_marks, k)
  }
}
marked_keys <- rows$key[rows$status %in% MARKED]


# ---- Candidates from the read-through ------------------------------------------
#
# Written on 2026-10-17 against the strings whose hash each key carries. A
# candidate closes when all its keys are marked; one whose string changed
# since it was written says so, because its point may no longer hold.

.CANDIDATES <- list(
  list(
    kind = "ambiguity",
    keys = c(title_regression_hierarchical = "3d0f5d37"),
    text = "In French, \"modèles hiérarchiques\" usually names multilevel models; this title heads blocks of nested models (`nested = TRUE`, sample 4). Candidate: \"modèles emboîtés\"."
  ),
  list(
    kind = "awkward",
    keys = c(
      title_regression_comparison_dv = "59a8daf8",
      title_regression_comparison = "9dd47d96"
    ),
    text = "\"— comparaison\" leaves what is compared implicit. Candidate: \"— comparaison de modèles\"."
  ),
  list(
    kind = "sibling",
    keys = c(title_continuous_lm_by = "19095ecb"),
    text = "Drops \"outcomes\", which decision 38 renders \"Variable dépendante\" (`row_outcome`). Candidate: \"Variables dépendantes continues selon %s\"."
  ),
  list(
    kind = "awkward",
    keys = c(title_percent_row = "4d2eefb4", title_percent_column = "f78fad5d"),
    text = "The usual French is \"pourcentages en ligne / en colonne\". Candidate: \" (% en ligne)\" and \" (% en colonne)\"."
  ),
  list(
    kind = "length, sibling",
    keys = c(
      header_valid_percent = "9c54c34f",
      header_cum_percent = "e4081fc8",
      header_cum_valid_percent = "235a6340"
    ),
    text = "18 to 25 characters in a console header where the English abbreviates (\"Cum.\"), and `header_pct_valid` (code_book) already writes \"% valide\". Candidate: \"% valide\", \"% cumulé\", \"% valide cumulé\"."
  ),
  list(
    kind = "length",
    keys = c(header_effect_size_short = "d624fd3c"),
    text = "14 characters over a column the English heads with 2, and now the same string as `header_effect_size`. Deliberate per its comment in R/i18n_fr.R (no established French abbreviation); listed for the width only."
  ),
  list(
    kind = "typography, sibling",
    keys = c(header_median = "bddee140"),
    text = "A French abbreviation by truncation takes a period, as `header_freq` (\"Eff.\") and `fitstat_max_cluster_size` (\"max.\") do; \"Méd\" has none, nor do the English fallbacks \"Min\" / \"Max\". Candidate: \"Méd.\" (and French entries \"Min.\" / \"Max.\" if the rule is applied)."
  ),
  list(
    kind = "sibling",
    keys = c(
      header_ci_label_credible = "23006d5b",
      header_ci_label_hdi = "67d91663"
    ),
    text = "Kept English while the confidence interval became \"IC\": the spanner prints \"CrI à 95 %\", an English abbreviation in a French construction. Candidates: \"ICr\" (intervalle de crédibilité) and a French label for HDI, or keep both as terms of art."
  ),
  list(
    kind = "sibling",
    keys = c(header_events_n = "d6069e38"),
    text = "Kept English while the fit-statistic row says \"N événements\" (`fitstat_n_events`). It is a word, not notation, and since b8d07195 translating a header no longer moves the column name. Candidate: \"Événements/N\"."
  ),
  list(
    kind = "awkward",
    keys = c(fitstat_n_events = "27654a16"),
    text = "Calques \"N events\". Candidate: \"Nombre d'événements\"."
  ),
  list(
    kind = "awkward",
    keys = c(fitstat_eff_p = "c653fe0e"),
    text = "The row holds a count; the usual French is \"nombre effectif de paramètres\"."
  ),
  list(
    kind = "terminology",
    keys = c(fitstat_icc = "b6b8d73d"),
    text = "French writing often abbreviates the intraclass correlation coefficient \"CCI\"; decision 38's rule (francise where French has a term of authority) may apply. `note_icc_multi_group` quotes it."
  ),
  list(
    kind = "sibling",
    keys = c(
      value_summary_matrix = "24bcf9e1",
      value_summary_list = "8ebe0d3b"
    ),
    text = "Decision 38 kept \"Array(%s)\" English because it names an R class; \"Matrice\" and \"Liste\" translate the two sibling class names. One rule for the three."
  ),
  list(
    kind = "terminology",
    keys = c(cell_system_missing = "e6037cea"),
    text = "If SPSS is the reference, check the label its French output gives the system-missing category (\"Système manquant\")."
  ),
  list(
    kind = "awkward",
    keys = c(note_weights_fallback = "58052955"),
    text = "With no weights name the sentences read \"Statistiques pondérées par la pondération.\" and \"… valeur manquante sur la pondération\". Candidate: \"les poids\"."
  ),
  list(
    kind = "sibling",
    keys = c(test_chisq = "29dff978"),
    text = "\"Khi-2\" here, \"khi-deux\" in every other French string (glosses, survey tests) and \"χ²\" in the fit rows: three spellings of one statistic."
  ),
  list(
    kind = "typography",
    keys = c(
      note_p_prefix_lt = "8b105ddf",
      note_p_prefix_eq = "7c8a8edf",
      note_stars_legend_entry = "d31fb913"
    ),
    text = "No-break spaces around \"<\" in the first, plain spaces in the two others, which fall back to English: the same cross_tab() note can break a line inside \"p = 0,659\" but not inside \"p < 0,001\", and the stars legend writes \"p < .05\" with plain spaces. Candidate: French entries for the two fallbacks, with no-break spaces."
  ),
  list(
    kind = "sibling",
    keys = c(note_reference_categories = "01d68065"),
    text = "\"Catégories\" where the set calls a factor level a \"modalité\" (`note_outcome_blocks`, `header_categories`, `cell_other_categories`). Candidate: \"Modalités de référence\"."
  ),
  list(
    kind = "ambiguity, sibling",
    keys = c(note_gloss_med_ci_undefined = "a200a2a3"),
    text = "\"trop petit pour ce niveau\" can read as the confidence level, in a confidence-interval gloss; the set says \"modalité\" for a factor level elsewhere. Candidate: \"pour cette modalité\"."
  ),
  list(
    kind = "awkward",
    keys = c(note_adjusted_for = "0ae2675c", note_outcome_blocks = "7e05f818"),
    text = "\"ajusté pour\" calques \"adjusted for\"; French epidemiology writes \"ajusté sur\" (\"ajustés les uns sur les autres\")."
  ),
  list(
    kind = "awkward",
    keys = c(
      note_icc_multi_group = "0747c4cd",
      note_negative_weights_no_test = "eb494e32",
      note_negative_weights_no_test_some = "e9c15ef7"
    ),
    text = "\"n'est pas rapporté(e)\" calques \"is not reported\". Candidate: \"n'est pas présenté(e)\"."
  ),
  list(
    kind = "awkward",
    keys = c(note_rank_deficient = "841bab67"),
    text = "Three \"(s)\" in a row. Candidate: \"les coefficients écartés sont affichés sous la forme %s\"."
  ),
  list(
    kind = "awkward",
    keys = c(note_nonconvergence = "245bb411"),
    text = "\"un ajustement convergé\": \"convergé\" is not used as an adjective. Candidate: \"un ajustement ayant convergé\"."
  ),
  list(
    kind = "awkward",
    keys = c(
      note_estimand_method = "ff9c3c28",
      note_estimand_method_stratified = "d4a5cc1c"
    ),
    text = "\"ajustée … à partir du modèle ajusté\" renders \"adjusted\" and \"fitted\" with one word. Candidate: \"… à partir du modèle estimé\"."
  ),
  list(
    kind = "awkward",
    keys = c(
      note_component_gloss_zero_inflation = "d4c40c93",
      note_component_gloss_dispersion = "b77e3036",
      note_component_gloss_hurdle_binomial = "fc54f163",
      note_component_gloss_hurdle_censored = "93972b12"
    ),
    text = "The hole receives the capitalized block caption, so the line reads \"Composante Inflation de zéros : …\"; the English leads with the caption. Candidate: put the hole in guillemets, or lead with it (\"%s (composante) : …\")."
  ),
  list(
    kind = "awkward",
    keys = c(note_component_gloss_hurdle_censored = "93972b12"),
    text = "\"%s censurée\" makes a distribution identifier (\"poisson\", \"negbin\") a feminine noun with nothing before it. Candidate: \"loi %s censurée à droite\"."
  ),
  list(
    kind = "awkward",
    keys = c(
      test_design_wald_chisq = "8db2bd23",
      test_design_adj_wald = "4546883a"
    ),
    text = "\"fondé sur le plan de sondage sur les proportions\": two \"sur\" in a row. Candidate: \"test de Wald des proportions des cellules, fondé sur le plan de sondage\"."
  ),
  list(
    kind = "awkward",
    keys = c(test_design_saddlepoint = "45c459fa"),
    text = "Names the statistic after its approximation. Candidate: \"khi-deux (approximation du point de selle) fondé sur le plan de sondage\"."
  ),
  list(
    kind = "awkward",
    keys = c(note_design_n = "d943568f"),
    text = "\"(pondéré 1 250)\": a bare participle before a number. Candidate: \"N = %s (N pondéré = %s).\""
  ),
  list(
    kind = "awkward",
    keys = c(note_p_adjusted = "1ce3fedd"),
    text = "The family size joins with \"sur\", giving \"Valeurs p ajustées par stats::p.adjust(method = …) sur m = 3 coefficient(s) par modèle.\"; the English uses a semicolon. Candidate: \"… (method = %s)<NBSP>; %s.\""
  ),
  list(
    kind = "terminology",
    keys = c(abbrev_or = "26130bf2"),
    text = "\"rapport de cotes\" is the term recommended in Québec; medical writing from France mostly keeps \"odds ratio\", the ground on which decision 38 kept \"hazard ratio\"."
  ),
  list(
    kind = "awkward",
    keys = c(note_outcome_overall = "58875c22"),
    text = "\"échantillon analytique\" calques \"analytic sample\"; the usual French is \"échantillon d'analyse\". Moves with `row_overall`, which it quotes."
  ),
  list(
    kind = "decided",
    keys = c(
      note_ci_posterior_mixed = "1ec0e47d",
      label_model_name = "f37cce02"
    ),
    text = "\"Model 1\" inside a French sentence and over the columns (sample 4). Frozen by decision 42 until the model labels get the col_name/display split; listed because a reader will notice it."
  ),
  list(
    kind = "terminology",
    keys = c(title_codebook = "68900f80", excel_sheet_codebook = "f9c4c01a"),
    text = "\"Codebook\" stays English on the French PDF cover and sheet; French data archives also say \"dictionnaire des codes\" or \"dictionnaire des variables\". A terminology call."
  )
)

hash_now <- stats::setNames(
  vapply(rows$resolved, .hash8, character(1)),
  rows$key
)
cand_keys <- unique(unlist(lapply(.CANDIDATES, function(x) names(x$keys))))
if (length(setdiff(cand_keys, rows$key))) {
  stop(
    "Candidates name unknown keys: ",
    paste(setdiff(cand_keys, rows$key), collapse = ", "),
    call. = FALSE
  )
}
cand_open <- Filter(
  function(x) !all(names(x$keys) %in% marked_keys),
  .CANDIDATES
)
n_cand_closed <- length(.CANDIDATES) - length(cand_open)


# ---- Automatic checks -----------------------------------------------------------

fr_rows <- rows[!is.na(rows$fr), ]
# A no-break space before : ; ? ! (not the R `::`), and inside guillemets.
nbsp_bad <- fr_rows$key[vapply(
  fr_rows$fr,
  function(v) {
    v2 <- gsub("::", "", v, fixed = TRUE)
    grepl("[^ ][:;?!]", v2, perl = TRUE) ||
      grepl("^[:;?!]", v2, perl = TRUE) ||
      grepl("«[^ ]|[^ ]»", v, perl = TRUE)
  },
  logical(1)
)]
# A no-break space between a number (or a hole) and its percent sign.
pct_bad <- fr_rows$key[grepl(
  "([0-9]|%[0-9]*[$]?[sd])( ?)%%?([^sd0-9]|$)",
  fr_rows$fr,
  perl = TRUE
)]
reg_fr <- fr_rows[!startsWith(fr_rows$key, "bridge:"), ]
holes_bad <- reg_fr$key[
  !vapply(
    seq_len(nrow(reg_fr)),
    function(i) {
      env_en$.spicy_holes_compatible(reg_fr$fr[[i]], reg_fr$en[[i]])
    },
    logical(1)
  )
]
same_as_en <- reg_fr$key[reg_fr$fr == reg_fr$en]
dup_fr <- Filter(
  function(kk) length(unique(EN[kk])) > 1L,
  split(reg_fr$key, reg_fr$fr)
)
dup_fr <- dup_fr[lengths(dup_fr) > 1L]
long_hdr <- reg_fr[
  reg_fr$group == "header" &
    !grepl("%", reg_fr$en, fixed = TRUE) &
    nchar(reg_fr$fr) >= 12L &
    nchar(reg_fr$fr) - nchar(reg_fr$en) >= 5L,
]
gaps <- rows$key[rows$status == "missing: no French entry"]
n_straight_apos <- sum(grepl("'", fr_rows$fr, fixed = TRUE))
n_curly_apos <- sum(grepl("’", fr_rows$fr, fixed = TRUE))


# ---- The table ------------------------------------------------------------------

n_keys <- sum(!startsWith(rows$key, "bridge:"))
n_bridge <- sum(startsWith(rows$key, "bridge:"))
status_levels <- names(.STATUS_INFO)
unknown_status <- setdiff(unique(rows$status), status_levels)
if (length(unknown_status)) {
  stop(
    "Unknown status: ",
    paste(unknown_status, collapse = ", "),
    call. = FALSE
  )
}
status_n <- table(factor(rows$status, levels = status_levels))

out <- c(
  "# spicy i18n: the French set, for review (issue #7)",
  "",
  sprintf(
    "Generated %s by `dev/gen_fr_review.R` at HEAD `%s`, from `R/i18n.R` and `R/i18n_fr.R` %s.",
    TODAY,
    head_hash,
    if (length(dirty)) "with uncommitted changes" else "as committed"
  ),
  sprintf(
    "%d registry keys, %d with a French entry, plus the %d entries of the title-prefix bridge: %d rows. \"New\" and \"changed\" are measured against the French set of %s (`%s`, %d entries), the one decision 38 was taken on.",
    n_keys,
    sum(!is.na(rows$fr) & !startsWith(rows$key, "bridge:")),
    n_bridge,
    nrow(rows),
    BASELINE_DATE,
    BASELINE_REF,
    length(FR_BASE)
  ),
  "",
  "This file is generated and stays local (`dev/*_review_table.md` is gitignored): edit `R/i18n_fr.R` and the marks file, never this file, then regenerate. `<NBSP>` marks a no-break space (U+00A0, written `\\u00A0` in the source); French sets one before `:` and `;`. `--` in the French column: no French entry, the reader gets the English default.",
  "",
  "## Status counts",
  "",
  "| Status | Rows | Meaning |",
  "| --- | ---: | --- |",
  sprintf(
    "| %s | %d | %s |",
    status_levels,
    as.integer(status_n),
    .STATUS_INFO
  ),
  sprintf("| **total** | **%d** | |", nrow(rows)),
  "",
  "By where the rows appear, in the order the table below follows:",
  "",
  "| Where | Rows | To review | Reviewed or corrected | Settled otherwise |",
  "| --- | ---: | ---: | ---: | ---: |"
)
for (g in names(.GROUPS)) {
  s <- rows$status[rows$group == g]
  if (!length(s)) {
    next
  }
  out <- c(
    out,
    sprintf(
      "| %s | %d | %d | %d | %d |",
      .GROUPS[[g]],
      length(s),
      sum(s %in% TO_REVIEW),
      sum(s %in% MARKED),
      sum(!s %in% c(TO_REVIEW, MARKED))
    )
  )
}

out <- c(
  out,
  "",
  "## How to mark a row",
  "",
  "The marks live in `dev/i18n_fr_review_marks.tsv`, one line per key. The generator merges them on every run, so regenerating never loses one.",
  "",
  "1. Read the French string in context: `dev/i18n_fr_review_samples.md`, or your own tables under `options(spicy.language = \"fr\")`.",
  "2. If it is right, mark it `reviewed`. If it is not, fix it in `R/i18n_fr.R` first, then mark it `corrected`.",
  "3. Add a line `<key> reviewed` (or `<key> corrected`) to the marks file, or run `Rscript dev/gen_fr_review.R --mark reviewed <key> [<key> ...]`.",
  "4. Regenerate with `Rscript dev/gen_fr_review.R` (`--no-samples` skips the four sample tables).",
  "",
  "The generator fills in the date and the French string the mark applies to, and rewrites the line with tabs. If that string changes later, the row turns `to review (changed after mark)`: read it again, then empty the line's `fr_at_mark` field (or run `--mark` again) to accept the new string. Any row can be marked, a frozen or \"same in French\" one included. Marking every key of a candidate below closes it. Bridge rows are marked by their `bridge:` key."
)
if (length(stale_marks)) {
  out <- c(
    out,
    "",
    sprintf(
      "**Marks overtaken by an edit (%d):** %s.",
      length(stale_marks),
      .keys_inline(stale_marks)
    )
  )
}
if (length(orphan_marks)) {
  out <- c(
    out,
    "",
    sprintf(
      "**Marks naming a key the registry no longer has (%d):** %s. Remove those lines.",
      length(orphan_marks),
      .keys_inline(orphan_marks)
    )
  )
}

# Decision 38, carried over from the table it was taken on.
d38 <- data.frame(
  key = c(
    "header_sd",
    "header_smd",
    "header_iqr",
    "header_freq",
    "note_design_psu",
    "note_design_calibrated",
    "row_overall",
    "row_outcome",
    "excel_sheet_outcome",
    "note_weight_rescaled",
    "header_effect_size_short",
    "label_block_zero_hurdle",
    "note_abbrev_hr",
    "value_summary_array"
  ),
  proposed = c(
    "ET",
    "DMS",
    "EIQ",
    "Eff.",
    "UPS",
    "calé",
    "Ensemble",
    "Variable dépendante",
    "Variable étudiée",
    "(normalisée)",
    "TE",
    "Obstacle (zéro)",
    "rapport de risques instantanés",
    "Tableau(%s)"
  ),
  arbitrated = c(
    "ET",
    "DMS",
    "EIQ",
    "Eff.",
    "UPS",
    "calé",
    "Ensemble",
    "Variable dépendante",
    "Variable étudiée",
    "(normalisée)",
    "ES",
    "Hurdle (zéro)",
    "hazard ratio",
    "Array(%s)"
  ),
  outcome = c(
    "confirmed",
    "confirmed (Cochrane France)",
    "confirmed",
    "confirmed (INSEE)",
    "confirmed",
    "confirmed (calage sur marges, INSEE / CALMAR)",
    "confirmed (INSEE; carries `note_outcome_overall`)",
    "confirmed (socio / psy usage)",
    "confirmed",
    "confirmed",
    "CORRECTED: term of art, dropped from the set",
    "CORRECTED: term kept, parenthetical French",
    "CORRECTED: dropped from the set",
    "CORRECTED: R class name, dropped from the set"
  ),
  # The French value the outcome left in R/i18n_fr.R (NA: no entry).
  left = c(
    "ET",
    "DMS",
    "EIQ",
    "Eff.",
    "%d UPS",
    "calé / post-stratifié",
    "Ensemble",
    "Variable dépendante",
    "Variable étudiée",
    " (normalisée)",
    NA,
    "Hurdle (zéro)",
    NA,
    NA
  ),
  stringsAsFactors = FALSE
)
d38_now <- unname(ifelse(d38$key %in% names(FR), FR[d38$key], NA_character_))
d38_holds <- mapply(.same, d38$left, d38_now)
out <- c(
  out,
  "",
  "## Decision 38: the fourteen terminology calls (2026-08-23)",
  "",
  "Amal's rule: francise where French has a term of authority, keep the international term of art otherwise. Ten calls confirmed the proposal, four corrected it. \"French now\" is the current `R/i18n_fr.R`; `--` means no French entry, the English is served.",
  "",
  "| Key | English | Proposed | Arbitrated | Outcome | French now | Still holds |",
  "| --- | --- | --- | --- | --- | --- | --- |",
  sprintf(
    "| `%s` | %s | %s | **%s** | %s | %s | %s |",
    d38$key,
    vapply(EN[d38$key], .code, character(1)),
    d38$proposed,
    d38$arbitrated,
    d38$outcome,
    vapply(d38_now, .code, character(1)),
    ifelse(d38_holds, "yes", "**no**")
  ),
  ""
)
if (!d38_holds[[match("header_effect_size_short", d38$key)]]) {
  out <- c(
    out,
    "`header_effect_size_short` no longer holds: on 2026-10-14 the regression headers began to follow the language (b8d07195, after point 4b of the site review, `dev/constats_site_modules_2026-10-14.md`), and `header_se` took \"ES\" for *erreur standard*. The effect-size header became \"TE\" in that commit, then \"Taille d'effet\" (de9f2060). Its row is `to review (new)`.",
    ""
  )
}
out <- c(
  out,
  "`spicy_labels()` stays exported (decision 38): key discoverability for `options(spicy.labels)` is worth the pre-1.0 surface.",
  "",
  "Notes carried over from the 2026-08-23 table:",
  "",
  "* `excel_sheet_continuous` (\"Descriptives\") is the one Excel sheet name left untranslated: the French word is the same, so the fallback serves it.",
  "* `note_model_prefix` reads like a table note and is the lead line of a `spicy_abort()` (`regression_frame_geepack.R`): errors stay English, so a French version would give \"Modèle 1 : `cluster` is not used ...\".",
  "* `note_ci_posterior_mixed` opened \"Model %d\" in French too, as `label_model_name` stayed English (decision 42). Since 2026-10-24 the model name has a French entry, the column names keep the English one, and the note is led by the name of its column like every per-model line (`.model_line()`).",
  "",
  "## Pairs that move together",
  "",
  "Change one half and the other must follow.",
  "",
  "* `row_overall` / `note_outcome_overall`: the note quotes the row.",
  "* `header_effect_size` / `header_effect_size_short`: one French string for both, by choice.",
  "* The standard-error vocabulary: `header_se` (\"ES\"), `abbrev_se`, `abbrev_se_design`, `abbrev_mcse`, `note_std_errors_single`, `note_estimand_method`, `note_estimand_method_stratified` all say \"erreur(s) standard(s)\".",
  ""
)

# Suggestions.
.fmt_check <- function(label, keys) {
  if (!length(keys)) {
    return(sprintf("* %s: none.", label))
  }
  sprintf("* %s (%d): %s.", label, length(keys), .keys_inline(keys))
}
out <- c(
  out,
  "## Suggestions of the generator",
  "",
  "Candidates only, nothing here is applied. Each names the key and the reason.",
  "",
  "### Automatic checks",
  "",
  .fmt_check(
    "French string missing the no-break space before `:` `;` `?` `!` or inside guillemets",
    nbsp_bad
  ),
  .fmt_check("Percent sign not preceded by a no-break space", pct_bad),
  .fmt_check(
    "French string whose holes do not fit the English template",
    holes_bad
  ),
  .fmt_check(
    "No French entry, and no status saying why (an English string in a French table)",
    gaps
  ),
  .fmt_check(
    "French entry identical to the English, which the fallback would serve anyway (R/i18n_fr.R keeps such keys out of the set)",
    same_as_en
  )
)
if (length(dup_fr)) {
  out <- c(
    out,
    sprintf(
      "* One French string for keys whose English differs (%d): %s.",
      length(dup_fr),
      paste(
        vapply(
          names(dup_fr),
          function(v) sprintf("%s for %s", .code(v), .keys_inline(dup_fr[[v]])),
          character(1)
        ),
        collapse = "; "
      )
    )
  )
}
if (nrow(long_hdr)) {
  out <- c(
    out,
    sprintf(
      "* Column headers of 12 characters or more and at least 5 longer than the English (%d): %s. See the candidates below.",
      nrow(long_hdr),
      paste(
        sprintf(
          "`%s` (%d against %d)",
          long_hdr$key,
          nchar(long_hdr$fr),
          nchar(long_hdr$en)
        ),
        collapse = ", "
      )
    )
  )
}
out <- c(
  out,
  sprintf(
    "* Apostrophes: %d French strings use the straight one (') and %d the typographic one (’). A global choice, listed once.",
    n_straight_apos,
    n_curly_apos
  ),
  "",
  sprintf(
    "### Candidates from the read-through (%d open%s)",
    length(cand_open),
    if (n_cand_closed) sprintf(", %d closed by marks", n_cand_closed) else ""
  ),
  "",
  "Written on 2026-10-17, from a reading of every French string against its English and of the four samples. Kinds: *awkward* (calque or clumsy French), *length* (wide for a console header), *sibling* (two ways of writing one thing), *typography*, *ambiguity*, *terminology* (a call for Amal, like decision 38), *decided* (settled, listed for context).",
  ""
)
for (i in seq_along(cand_open)) {
  x <- cand_open[[i]]
  ks <- names(x$keys)
  vals <- vapply(
    ks,
    function(k) {
      j <- match(k, rows$key)
      tag <- if (k %in% marked_keys) " (marked)" else ""
      sprintf("`%s` = %s%s", k, .code(rows$resolved[[j]]), tag)
    },
    character(1)
  )
  changed <- ks[!(ks %in% marked_keys) & hash_now[ks] != x$keys]
  out <- c(
    out,
    sprintf("%d. *%s*: %s", i, x$kind, paste(vals, collapse = "; ")),
    sprintf(
      "   %s%s",
      x$text,
      if (length(changed)) {
        sprintf(
          " (Changed since this was written: %s. Check the point still holds.)",
          .keys_inline(changed)
        )
      } else {
        ""
      }
    )
  )
}

# Every row, grouped by where it appears, then by registry section.
.row_md <- function(r) {
  sprintf(
    "| `%s` | %s | %s | %s | %s | %s |",
    r$key,
    .code(r$en),
    .code(r$fr),
    r$where,
    r$used_in,
    r$status
  )
}
out <- c(
  out,
  "",
  "## Every key, in reading order",
  "",
  "Seven parts, in the order a reader meets a table: titles, column headers, row labels, cells, footer notes, then the Excel sheet names and the one message. Within a part, rows follow the registry, under the name of the registry section they come from: the Excel sheet names, for one, sit under \"missing values\", where `R/i18n.R` keeps them.",
  ""
)
gi <- 0L
for (g in names(.GROUPS)) {
  gr <- rows[rows$group == g, ]
  if (!nrow(gr)) {
    next
  }
  gi <- gi + 1L
  gr <- gr[order(gr$order), ]
  out <- c(
    out,
    sprintf(
      "### %d. %s (%d %s, %d to review)",
      gi,
      .GROUPS[[g]],
      nrow(gr),
      if (nrow(gr) == 1L) "row" else "rows",
      sum(gr$status %in% TO_REVIEW)
    ),
    ""
  )
  for (s in unique(gr$section)) {
    sr <- gr[gr$section == s, ]
    out <- c(
      out,
      sprintf("#### %s", s),
      "",
      "| Key | English | French | Where | Used in | Status |",
      "| --- | --- | --- | --- | --- | --- |",
      vapply(seq_len(nrow(sr)), function(i) .row_md(sr[i, ]), character(1)),
      ""
    )
  }
}
.write_utf8(out, PATH_TABLE)


# ---- The samples ------------------------------------------------------------------

# Does the printed table show this string? Its literal fragments (the text
# between the holes) must all appear; a short alphanumeric one ("p", "ES")
# as a whole token. NA when no fragment can tell (pure punctuation).
.shown_in <- function(value, text) {
  frags <- strsplit(value, env_en$.SPICY_FORMAT_SPEC_RX, perl = TRUE)[[1L]]
  frags <- trimws(gsub("%%", "%", frags, fixed = TRUE))
  frags <- frags[grepl("[[:alnum:]]|[^ -~]", frags, perl = TRUE)]
  if (!length(frags)) {
    return(NA)
  }
  all(vapply(
    frags,
    function(f) {
      if (grepl("^[[:alnum:]]{1,3}$", f)) {
        grepl(
          paste0("(?<![[:alnum:]])", f, "(?![[:alnum:]])"),
          text,
          perl = TRUE
        )
      } else {
        grepl(f, text, fixed = TRUE)
      }
    },
    logical(1)
  ))
}

.render_samples <- function() {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The samples need pkgload; --no-samples skips them.", call. = FALSE)
  }
  suppressMessages(pkgload::load_all(".", quiet = TRUE))
  # Log every key resolved while a table is built and printed.
  log <- new.env()
  log$keys <- character()
  orig <- get("spicy_str", envir = asNamespace("spicy"))
  utils::assignInNamespace(
    "spicy_str",
    function(key) {
      log$keys <- c(log$keys, key)
      orig(key)
    },
    ns = "spicy"
  )
  on.exit(utils::assignInNamespace("spicy_str", orig, ns = "spicy"), add = TRUE)
  old <- options(spicy.language = "fr", width = 120)
  on.exit(options(old), add = TRUE)

  samples <- list(
    list(
      title = "`table_categorical()`",
      setup = character(0),
      call = "table_categorical(sochealth, select = c(smoking, dentist_12m), by = sex)"
    ),
    list(
      title = "`table_continuous()` with an effect size",
      setup = character(0),
      call = "table_continuous(sochealth, select = c(age, bmi), by = sex, effect_size = TRUE)"
    ),
    list(
      title = "`table_regression()`, one linear model",
      setup = character(0),
      call = "table_regression(lm(wellbeing_score ~ age + sex + smoking, sochealth))"
    ),
    list(
      title = "`table_regression()`, two nested linear models",
      setup = c(
        "d <- sochealth[!is.na(sochealth$smoking), ] # one sample for both models",
        "m1 <- lm(wellbeing_score ~ age + sex, data = d)",
        "m2 <- lm(wellbeing_score ~ age + sex + smoking, data = d)"
      ),
      call = "table_regression(list(m1, m2), nested = TRUE)"
    )
  )
  env <- new.env(parent = globalenv())
  md <- c(
    "# spicy i18n: four French tables, for the read-through (issue #7)",
    "",
    sprintf(
      "Generated %s by `dev/gen_fr_review.R` at HEAD `%s` (the working tree, loaded with `pkgload::load_all()`), under `options(spicy.language = \"fr\", width = 120)`. Console output, captured as printed. No-break spaces print as plain spaces here; `dev/i18n_fr_review_table.md` writes them `<NBSP>`.",
      TODAY,
      head_hash
    ),
    "",
    "Below each table, the registry keys it shows: those with a French entry, then those served in English. A key counts when it was resolved while the table was built and printed and its text appears in the output (keys resolved for a column or a note the table does not show are left out; pure-punctuation templates cannot be told apart and are left out too)."
  )
  for (i in seq_along(samples)) {
    s <- samples[[i]]
    log$keys <- character()
    warns <- character()
    for (ln in s$setup) {
      eval(parse(text = ln), envir = env)
    }
    printed <- withCallingHandlers(
      utils::capture.output(print(eval(parse(text = s$call), envir = env))),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    keys <- unique(log$keys)
    text <- paste(printed, collapse = "\n")
    shown <- keys[vapply(
      keys,
      function(k) {
        isTRUE(.shown_in(if (k %in% names(FR)) FR[[k]] else EN[[k]], text))
      },
      logical(1)
    )]
    fr_shown <- shown[shown %in% names(FR)]
    en_shown <- shown[!shown %in% names(FR)]
    md <- c(
      md,
      "",
      sprintf("## %d. %s", i, s$title),
      "",
      "```r",
      s$setup,
      s$call,
      "```",
      "",
      "```text",
      printed,
      "```",
      "",
      sprintf(
        "French entries shown (%d): %s.",
        length(fr_shown),
        if (length(fr_shown)) .keys_inline(fr_shown) else "none"
      ),
      "",
      sprintf(
        "Shown in English, no French entry (%d): %s.",
        length(en_shown),
        if (length(en_shown)) .keys_inline(en_shown) else "none"
      )
    )
    if (length(warns)) {
      md <- c(
        md,
        "",
        sprintf("Warnings: %s", paste(unique(warns), collapse = " | "))
      )
    }
  }
  .write_utf8(md, PATH_SAMPLES)
}
if (want_samples) {
  .render_samples()
}


# ---- Summary ----------------------------------------------------------------------

cat(sprintf("Table:   %s (%d rows)\n", PATH_TABLE, nrow(rows)))
if (want_samples) {
  cat(sprintf("Samples: %s\n", PATH_SAMPLES))
}
cat(sprintf("Marks:   %s (%d marked)\n", PATH_MARKS, nrow(marks$rows)))
cat("\nStatus counts:\n")
for (s in status_levels) {
  if (status_n[[s]]) cat(sprintf("  %-34s %4d\n", s, status_n[[s]]))
}
cat(sprintf(
  "\nCandidates: %d open, %d closed. Gaps: %d. Missing NBSP: %d. Hole mismatches: %d.\n",
  length(cand_open),
  n_cand_closed,
  length(gaps),
  length(nbsp_bad),
  length(holes_bad)
))
