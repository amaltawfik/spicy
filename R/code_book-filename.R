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

  # No trimws() first: leading and trailing blanks become "_" below and
  # are trimmed there, and the fold is the only step allowed to look at
  # the raw string, so that nothing upstream of it depends on the locale.
  filename <- code_book_ascii_filename(filename)

  if (is.na(filename)) {
    filename <- ""
  }

  # Plain ASCII from here on.
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


# Folds a string to plain ASCII, the same way on every platform.
#
# The work is done on code points, against a table shipped in the
# package (R/code_book-fold-table.R). Nothing here asks the system for an
# opinion: no `iconv()`, whose "//TRANSLIT" is implementation-defined
# (glibc folds e-acute to "e", musl substitutes "*", Windows loses the
# sharp s), no ICU, no Unicode-aware regular expression, no
# locale-sensitive function. A title therefore gives the same file name
# on every operating system, in every locale, and in ten years' time.
#
# In order:
#   1. combining marks and invisible format characters are removed, so a
#      decomposed "e" + U+0301 and a soft hyphen leave nothing behind;
#   2. a code point of the fold table is replaced by its ASCII spelling
#      (e-acute "e", sharp s "ss", the oe ligature "oe", an en dash "-");
#   3. any other non-ASCII code point becomes "_": a script the table
#      does not cover is dropped, never guessed at;
#   4. apostrophes, quotation marks and accent-like marks are removed,
#      the ASCII ones and the typographic ones that step 2 turned into
#      them, so "L'age" reads "Lage" whichever apostrophe was typed.
#
# NA comes back as NA, and so does a string declared UTF-8 that is not
# valid UTF-8. A string with no declared encoding is read in the
# session's native encoding by enc2utf8(), as everywhere else in R.
code_book_ascii_filename <- function(filename) {
  cp <- utf8ToInt(enc2utf8(filename))
  if (anyNA(cp)) {
    return(NA_character_)
  }

  cp <- cp[!code_book_is_dropped(cp)]
  out <- rep("_", length(cp))
  ascii <- cp < 128L
  out[ascii] <- intToUtf8(cp[ascii], multiple = TRUE)
  hit <- match(cp, code_book_fold_from)
  out[!is.na(hit)] <- code_book_fold_to[hit[!is.na(hit)]]

  gsub("[`'\"^~]+", "", paste(out, collapse = ""), perl = TRUE)
}


# Code points removed before the fold: the combining marks used with
# Latin letters (five Unicode blocks, whose boundaries never move) and the
# invisible format characters that text pasted from a browser, a PDF or
# a word processor brings along. An invisible character must not turn
# into a visible one in a file name, which is why the soft hyphen is
# removed here although ICU folds it to "-".
#
# This function is the only home of that list. The generator of the fold
# table, data-raw/code_book_fold_table.R, reads it from here, so a code
# point added below also leaves the table at the next regeneration.
code_book_is_dropped <- function(cp) {
  (cp >= 0x0300L & cp <= 0x036FL) | # Combining Diacritical Marks
    (cp >= 0x1AB0L & cp <= 0x1AFFL) | # Combining Diacritical Marks Extended
    (cp >= 0x1DC0L & cp <= 0x1DFFL) | # Combining Diacritical Marks Supplement
    (cp >= 0x20D0L & cp <= 0x20FFL) | # Combining Diacritical Marks for Symbols
    (cp >= 0xFE20L & cp <= 0xFE2FL) | # Combining Half Marks
    cp == 0x00ADL | # soft hyphen
    (cp >= 0x200BL & cp <= 0x200FL) | # zero-width space and joiners, direction marks
    (cp >= 0x202AL & cp <= 0x202EL) | # bidirectional embeddings and overrides
    (cp >= 0x2060L & cp <= 0x206FL) | # word joiner, invisible operators, isolates
    (cp >= 0xFE00L & cp <= 0xFE0FL) | # variation selectors
    cp == 0xFEFFL # byte order mark
}
