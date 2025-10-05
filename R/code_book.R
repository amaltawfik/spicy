#' Generate a comprehensive variable codebook
#'
#' @description
#' `code_book()` builds upon [`varlist()`] to create an interactive and exportable
#' codebook summarizing all variables of a data frame.
#'
#' It displays the results as an interactive `DT::datatable()` (sortable, searchable,
#' and exportable), and can optionally export the codebook to CSV, Excel, PDF, or Word.
#'
#' @param x A data frame or tibble.
#' @param values Logical. If `FALSE` (the default), displays a compact summary of the variable's values.
#'   For numeric, character, date/time, labelled, and factor variables, up to four unique non-missing values are shown:
#'   the first three values, followed by an ellipsis (`...`), and the last value.
#'   Values are sorted when appropriate (e.g., numeric, character, date)
#'   For factors, the levels are used directly and are not sorted.
#'   For labelled variables, prefixed labels are displayed via `labelled::to_factor(levels = "prefixed")`.
#'   If `TRUE`, all unique non-missing values are displayed.
#' @param include_na Logical. If `TRUE`, unique missing values (`NA`, `NaN`) are explicitly appended at the end of the `Values` summary
#'   when present in the variable. This applies to all variable types.
#'   If `FALSE` (the default), missing values are omitted from `Values` but still counted in the `NAs` column.
#' @param export Character string indicating export format.
#'   One of "none" (default), "csv", "excel", "pdf", or "word".
#'   When "none", no file is saved and only the interactive table is displayed.
#' @param file Optional base file name (with or without extension). Defaults to "codebook".
#'   For explicit exports, you can specify a full path. For the interactive Viewer,
#'   only the base name (without path and extension) is used.
#' @param title Optional character string. Title displayed for PDF and Word exports.
#'   Defaults to "Codebook". If `NULL`, no title is shown.
#' @param ... Additional arguments (currently unused).
#'
#' @details
#' - CSV and Excel exports are always data-only (no title row).
#' - PDF and Word exports use harmonized margins (1.2 cm) and automatically adjusted column widths.
#' - The interactive datatable version (`export = "none"`) includes export buttons (copy, csv, excel, pdf, print).
#' - If LaTeX is not installed, use `tinytex::install_tinytex()` for PDF export.
#'
#' @return
#' A `DT::datatable` object (if `export = "none"`) or a data frame (invisibly)
#' if an export format is requested.
#'
#' @export
#' @importFrom DT datatable
#' @importFrom utils write.csv
#' @importFrom cli cli_alert_success cli_alert_danger cli_alert_info
#' @importFrom htmlwidgets JS
#' @importFrom tools file_path_sans_ext
code_book <- function(x,
                      values = FALSE,
                      include_na = FALSE,
                      export = c("none", "csv", "excel", "pdf", "word"),
                      file = NULL,
                      title = "Codebook",
                      ...) {
  export <- match.arg(export)
  if (is.null(file)) file <- "codebook"

  # Info message si chemin fourni mais export = none
  if (export == "none" && grepl("[/\\\\]", file)) {
    cli::cli_alert_info(
      "No file will be saved since export = 'none'. The path {file} will only be used as a base name in the Viewer export buttons."
    )
  }

  # --- 1. Compute data -------------------------------------------------------
  res <- tryCatch(
    varlist(x, values = values, include_na = include_na, tbl = TRUE),
    error = function(e) stop("Error when calling varlist(): ", e$message, call. = FALSE)
  )
  if (!inherits(res, "data.frame")) {
    stop("varlist() did not return a data frame. Check your input.", call. = FALSE)
  }

  # --- Helper for file naming -----------------------------------------------
  with_ext <- function(path, ext_regex, add_ext) {
    if (grepl(ext_regex, path, ignore.case = TRUE)) path else paste0(path, add_ext)
  }

  # --- 2. Direct exports -----------------------------------------------------
  if (export == "csv") {
    out <- with_ext(file, "\\.csv$", ".csv")
    utils::write.csv(res, out, row.names = FALSE)
    cli::cli_alert_success("Codebook exported to {out}")
    return(invisible(res))
  }

  if (export == "excel") {
    if (!requireNamespace("writexl", quietly = TRUE)) {
      cli::cli_alert_danger("Package 'writexl' is required for Excel export.")
      stop("Install it via install.packages('writexl')", call. = FALSE)
    }
    out <- with_ext(file, "\\.xlsx$", ".xlsx")
    writexl::write_xlsx(res, out)
    cli::cli_alert_success("Codebook exported to {out}")
    return(invisible(res))
  }

  # --- 3. PDF / Word exports via R Markdown ---------------------------------
  if (export %in% c("pdf", "word")) {
    # --- Vérifications des dépendances ---
    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
      cli::cli_alert_danger("Package 'rmarkdown' is required for {toupper(export)} export.")
      stop("Install it via install.packages('rmarkdown')", call. = FALSE)
    }
    if (!requireNamespace("kableExtra", quietly = TRUE)) {
      cli::cli_alert_danger("Package 'kableExtra' is required for formatted tables.")
      stop("Install it via install.packages('kableExtra')", call. = FALSE)
    }
    if (!requireNamespace("stringr", quietly = TRUE)) {
      cli::cli_alert_danger("Package 'stringr' is required for wrapping text.")
      stop("Install it via install.packages('stringr')", call. = FALSE)
    }

    # Optionnel : avertir si LaTeX manquant
    if (export == "pdf" && requireNamespace("tinytex", quietly = TRUE)) {
      if (!tinytex::is_tinytex()) {
        cli::cli_alert_info(
          "A LaTeX engine (e.g. TinyTeX) is recommended for PDF export.\nInstall it via: tinytex::install_tinytex()"
        )
      }
    }

    # --- Construction du chemin de sortie ---
    out <- switch(export,
      pdf  = with_ext(file, "\\.pdf$", ".pdf"),
      word = with_ext(file, "\\.docx$", ".docx")
    )

    # --- Création du fichier Rmd dans le dossier courant ---
    tmp_rmd <- file.path(getwd(), paste0("codebook_", Sys.Date(), ".Rmd"))
    output_fmt <- if (export == "pdf") "pdf_document" else "word_document"
    fmt <- if (export == "word") "pandoc" else "latex"

    # --- Contenu du Rmd ---
    rmd_content <- c(
      "---",
      sprintf("title: '%s'", if (is.null(title)) "" else title),
      "geometry: margin=1.2cm",
      sprintf("output: %s", output_fmt),
      "always_allow_html: true",
      "---",
      "",
      "```{r, echo=FALSE, message=FALSE, warning=FALSE}",
      "library(kableExtra)",
      "library(stringr)",
      "",
      "# --- Wrap text to avoid overflow ---",
      "res[] <- lapply(res, function(col) if (is.character(col)) stringr::str_wrap(as.character(col), width = 50) else col)",
      "",
      "# --- Dynamic column widths ---",
      "avail_cm <- 16.5",
      "min_cm <- 1.8; max_cm <- 8",
      "",
      "score <- vapply(res, function(col) {",
      "  if (is.numeric(col) || is.integer(col)) return(1.8)",
      "  m <- mean(nchar(as.character(col)), na.rm = TRUE)",
      "  if (!is.finite(m)) m <- 2",
      "  max(2, m / 8)",
      "}, numeric(1))",
      "",
      "w <- score / sum(score) * avail_cm",
      "w <- pmax(min_cm, pmin(max_cm, w))",
      "if (sum(w) > avail_cm) w <- w * (avail_cm / sum(w))",
      "",
      sprintf("# --- Table rendering ---\n tb <- kableExtra::kbl(res, format = '%s', booktabs = TRUE, longtable = TRUE, linesep = '')", fmt),
      "for (i in seq_along(w)) tb <- kableExtra::column_spec(tb, i, width = sprintf('%.2fcm', w[i]))",
      "",
      "tb <- kableExtra::kable_styling(tb, latex_options = c('striped','scale_down'), font_size = 8)",
      "tb",
      "```"
    )

    # --- Écriture du fichier Rmd ---
    writeLines(rmd_content, tmp_rmd)

    # --- Rendu dans le dossier courant (plus dans tempdir) ---
    rmarkdown::render(
      input = tmp_rmd,
      output_file = normalizePath(out, mustWork = FALSE),
      quiet = TRUE,
      envir = new.env(),
      knit_root_dir = getwd()
    )

    # --- Suppression du Rmd temporaire ---
    unlink(tmp_rmd)

    # --- Message de confirmation ---
    cli::cli_alert_success("Codebook exported to {normalizePath(out, mustWork = FALSE)}")

    return(invisible(res))
  }


  # --- 4. Interactive DataTable --------------------------------------------
  file_base <- tools::file_path_sans_ext(basename(file))
  esc <- function(s) gsub("(['\"\\\\])", "\\\\\\1", s)

  dt_buttons <- list(
    list(extend = "copy", title = NULL, filename = file_base),
    list(extend = "csv", title = NULL, filename = file_base),
    list(extend = "excel", title = NULL, filename = file_base),
    c(
      list(extend = "pdf", filename = file_base, title = NULL),
      if (!is.null(title)) {
        list(
          customize = htmlwidgets::JS(sprintf(
            "function (doc) {
             doc.content.unshift({ text: '%s', fontSize: 14, bold: true, margin: [0,0,0,12] });
           }",
            esc(title)
          ))
        )
      } else {
        list()
      }
    ),
    list(extend = "print", title = if (is.null(title)) NULL else title)
  )

  DT::datatable(
    res,
    rownames = FALSE,
    editable = FALSE,
    filter = "none",
    selection = "none",
    extensions = list(
      "Buttons" = NULL,
      "ColReorder" = NULL,
      "FixedHeader" = NULL,
      "KeyTable" = NULL,
      "Select" = TRUE
    ),
    options = list(
      dom = "Blfrtip",
      autoWidth = TRUE,
      pageLength = 10,
      colReorder = TRUE,
      fixedHeader = TRUE,
      searchHighlight = TRUE,
      buttons = dt_buttons
    )
  )
}
