code_book_filename <- function(title, filename = NULL) {
  if (!is.null(filename)) {
    return(code_book_sanitize_filename(
      filename,
      arg = "filename",
      fallback = NULL
    ))
  }

  code_book_sanitize_filename(
    title,
    arg = "title",
    fallback = "Codebook"
  )
}


code_book_sanitize_filename <- function(filename, arg, fallback = NULL) {
  if (is.null(filename)) {
    return(fallback)
  }

  filename <- code_book_ascii_filename(trimws(filename))

  if (is.na(filename)) {
    filename <- ""
  }

  filename <- gsub("[^A-Za-z0-9_-]+", "_", filename, perl = TRUE)
  filename <- gsub("_+", "_", filename, perl = TRUE)
  filename <- gsub("^_+|_+$", "", filename, perl = TRUE)

  if (!nzchar(filename)) {
    if (!is.null(fallback)) {
      return(fallback)
    }

    spicy_abort(
      paste0(
        "`",
        arg,
        "` must contain at least one letter, number, ",
        "underscore, or hyphen after sanitization."
      ),
      class = "spicy_invalid_input"
    )
  }

  # No length cap. spicy follows the Stata / SPSS convention of
  # never silently mutating user-supplied identifiers: a title or
  # filename so long that it overflows the platform filename limit
  # (Windows MAX_PATH 260, macOS / ext4 255 bytes per component)
  # surfaces as a noisy OS-level download error from the browser
  # rather than as a silently-truncated file. Users diagnose and
  # rename; the package never lies about what it produced.
  filename
}


code_book_ascii_filename <- function(filename) {
  filename <- enc2utf8(filename)
  filename <- gsub("\\p{M}+", "", filename, perl = TRUE)
  filename <- suppressWarnings(
    iconv(filename, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  )

  if (is.na(filename)) {
    # nocov start
    return(filename)
  } # nocov end

  # Some iconv implementations transliterate accents as ASCII marks.
  filename <- gsub("\\p{M}+", "", filename, perl = TRUE)
  gsub("[`'\"^~]+", "", filename, perl = TRUE)
}
