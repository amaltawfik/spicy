# ==================================================
# build_pkgdown_site.R
# Build the pkgdown site and remove internal dev docs
# ==================================================

# --- preflight ---------------------------------------------------------------

if (!file.exists("DESCRIPTION") || !file.exists("_pkgdown.yml")) {
  stop(
    "build_pkgdown_site.R must be sourced from the package root. ",
    "Current working directory: ",
    getwd(),
    call. = FALSE
  )
}

for (pkg in c("pkgdown", "jsonlite")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required.", call. = FALSE)
  }
}

docs_dir <- "docs"

internal_pages <- c("AGENTS", "CLAUDE")
legacy_pages <- c(
  "articles/table-apa",
  "reference/table_apa"
)
generated_reference_artifacts <- c(
  "reference/table_continuous.docx",
  "reference/table_continuous.xlsx"
)

internal_pattern <- paste(internal_pages, collapse = "|")
legacy_pattern <- paste(legacy_pages, collapse = "|")

# --- helpers -----------------------------------------------------------------

remove_files <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (length(existing)) unlink(existing)
  invisible(existing)
}

page_variants <- function(pages, dir = docs_dir) {
  as.vector(outer(file.path(dir, pages), c(".html", ".md"), paste0))
}

clean_sitemap <- function(sitemap = file.path(docs_dir, "sitemap.xml")) {
  if (!file.exists(sitemap)) {
    return(invisible(FALSE))
  }

  lines <- readLines(sitemap, warn = FALSE, encoding = "UTF-8")
  drop <- grepl(
    paste0(
      "/(",
      internal_pattern,
      "|",
      legacy_pattern,
      ")\\.(html|md)</loc>"
    ),
    lines
  )
  if (!any(drop)) {
    return(invisible(FALSE))
  }

  writeLines(lines[!drop], sitemap, useBytes = TRUE)
  invisible(TRUE)
}

clean_search_index <- function(
  search_index = file.path(docs_dir, "search.json")
) {
  if (!file.exists(search_index)) {
    return(invisible(FALSE))
  }

  entries <- jsonlite::fromJSON(search_index, simplifyVector = FALSE)
  internal_re <- paste0("/(", internal_pattern, ")\\.(html|md)$")
  legacy_re <- paste0(
    "^https://amaltawfik\\.github\\.io/spicy/",
    "(",
    legacy_pattern,
    ")\\.(html|md)$"
  )

  keep <- function(entry) {
    path <- entry$path
    if (!is.character(path) || length(path) != 1) {
      return(TRUE)
    }
    !grepl(internal_re, path) && !grepl(legacy_re, path)
  }

  filtered <- Filter(keep, entries)
  if (length(filtered) == length(entries)) {
    return(invisible(FALSE))
  }

  json <- jsonlite::toJSON(
    filtered,
    auto_unbox = TRUE,
    null = "null",
    force = TRUE
  )
  writeLines(json, search_index, useBytes = TRUE)
  invisible(TRUE)
}

fix_html_encoding_artifacts <- function(dir = docs_dir) {
  html_files <- list.files(
    dir,
    pattern = "\\.html$",
    recursive = TRUE,
    full.names = TRUE
  )

  changed <- character(0)
  for (path in html_files) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    new_lines <- gsub(
      'type="”image/svg+xml”"',
      'type="image/svg+xml"',
      lines,
      fixed = TRUE
    )
    if (!identical(lines, new_lines)) {
      writeLines(new_lines, path, useBytes = TRUE)
      changed <- c(changed, path)
    }
  }

  invisible(changed)
}

# --- build -------------------------------------------------------------------
# Any error here propagates and cleanup below will not run on a partial site.

pkgdown::build_site()

# --- post-build cleanup ------------------------------------------------------

removed_internal <- remove_files(page_variants(internal_pages))
removed_legacy <- remove_files(page_variants(legacy_pages))
removed_artifacts <- remove_files(
  file.path(docs_dir, generated_reference_artifacts)
)

clean_sitemap()
clean_search_index()
fixed_html <- fix_html_encoding_artifacts()

# --- report ------------------------------------------------------------------

report <- function(label, files) {
  if (!length(files)) {
    return(invisible())
  }
  message(label, ": ", paste(basename(files), collapse = ", "))
}

report("Removed internal pkgdown pages", removed_internal)
report("Removed legacy pkgdown pages", removed_legacy)
report("Removed generated reference artifacts", removed_artifacts)
report("Fixed HTML encoding artifacts", fixed_html)
