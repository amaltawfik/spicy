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

  max_length <- 120L
  if (nchar(filename, type = "chars") > max_length) {
    filename <- substr(filename, 1L, max_length)
    filename <- gsub("_+$", "", filename, perl = TRUE)
  }

  filename
}


code_book_ascii_filename <- function(filename) {
  filename <- enc2utf8(filename)
  filename <- gsub("\\p{M}+", "", filename, perl = TRUE)
  filename <- suppressWarnings(
    iconv(filename, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  )

  if (is.na(filename)) { # nocov start
    return(filename)
  } # nocov end

  # Some iconv implementations transliterate accents as ASCII marks.
  filename <- gsub("\\p{M}+", "", filename, perl = TRUE)
  gsub("[`'\"^~]+", "", filename, perl = TRUE)
}
