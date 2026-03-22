# ==================================================
# build_pkgdown_site.R
# Build the pkgdown site and remove internal dev docs
# ==================================================

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required to clean pkgdown search.json.")
}

internal_pages <- c("AGENTS", "CLAUDE")

internal_pages_pattern <- paste(internal_pages, collapse = "|")

remove_internal_files <- function(internal_pages, docs_dir = "docs") {
  html_md_files <- as.vector(outer(
    file.path(docs_dir, internal_pages),
    c(".html", ".md"),
    paste0
  ))

  existing_files <- html_md_files[file.exists(html_md_files)]

  if (length(existing_files) > 0) {
    unlink(existing_files)
  }

  invisible(existing_files)
}

clean_sitemap <- function(
  internal_pages,
  sitemap = file.path("docs", "sitemap.xml")
) {
  if (!file.exists(sitemap)) {
    return(invisible(FALSE))
  }

  lines <- readLines(sitemap, warn = FALSE, encoding = "UTF-8")
  keep <- !grepl(
    paste0("/(", internal_pages_pattern, ")\\.(html|md)</loc>"),
    lines
  )

  writeLines(lines[keep], sitemap, useBytes = TRUE)
  invisible(TRUE)
}

clean_search_index <- function(
  internal_pages,
  search_index = file.path("docs", "search.json")
) {
  if (!file.exists(search_index)) {
    return(invisible(FALSE))
  }

  entries <- jsonlite::fromJSON(search_index, simplifyVector = FALSE)
  pattern <- paste0("/(", internal_pages_pattern, ")\\.(html|md)$")

  keep_entry <- function(entry) {
    path <- entry$path %||% NULL

    if (!is.character(path) || length(path) != 1) {
      return(TRUE)
    }

    !grepl(pattern, path)
  }

  filtered_entries <- Filter(keep_entry, entries)
  json <- jsonlite::toJSON(
    filtered_entries,
    auto_unbox = TRUE,
    null = "null",
    force = TRUE
  )

  writeLines(json, search_index, useBytes = TRUE)
  invisible(TRUE)
}

fix_html_encoding_artifacts <- function(docs_dir = "docs") {
  html_files <- list.files(
    docs_dir,
    pattern = "\\.html$",
    recursive = TRUE,
    full.names = TRUE
  )

  for (path in html_files) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    lines <- gsub(
      'type="”image/svg+xml”"',
      'type="image/svg+xml"',
      lines,
      fixed = TRUE
    )
    writeLines(lines, path, useBytes = TRUE)
  }

  invisible(html_files)
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

pkgdown::build_site()

removed_files <- remove_internal_files(internal_pages)
clean_sitemap(internal_pages)
clean_search_index(internal_pages)
fix_html_encoding_artifacts()

message(
  "Removed internal pkgdown pages: ",
  paste(basename(removed_files), collapse = ", ")
)
