# The PDF codebook. Its layout is inst/typst/codebook.typ; R appends the
# codebook and the reader's words to it as Typst literals, then the call
# that lays them out, so the .typ file compiles anywhere with
# `typst compile`. code_book_quarto() has found Quarto and checked the
# fonts before the codebook is built.
code_book_write_typst <- function(cb, path) {
  template <- readLines(
    system.file("typst", "codebook.typ", package = "spicy"),
    encoding = "UTF-8"
  )
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(c(template, "", code_book_typst_source(cb)), con, useBytes = TRUE)
  invisible(path)
}


code_book_write_pdf <- function(cb, path, quarto) {
  typ <- tempfile(fileext = ".typ")
  on.exit(unlink(typ), add = TRUE)
  code_book_write_typst(cb, typ)
  # A quoted "~" would reach Typst unexpanded: R expands it first.
  log <- suppressWarnings(system2(
    quarto,
    c("typst", "compile", shQuote(typ), shQuote(path.expand(path))),
    stdout = TRUE,
    stderr = TRUE
  ))
  if (!is.null(attr(log, "status"))) {
    # The temporary source is gone on exit: no line names it.
    log <- log[!grepl(basename(typ), log, fixed = TRUE)]
    spicy_abort(
      c(
        "Typst could not compile the codebook.",
        "x" = paste(log, collapse = "\n"),
        "i" = "Write the source with `output = \"<path>.typ\"` to inspect it."
      ),
      class = "spicy_typst_failed",
      stderr = log
    )
  }
  invisible(path)
}


# The font families Typst finds: the system's and the ones it embeds.
# Quarto relays the list on stderr, so both streams are read.
code_book_typst_fonts <- function(quarto) {
  trimws(suppressWarnings(system2(
    quarto,
    c("typst", "fonts"),
    stdout = TRUE,
    stderr = TRUE
  )))
}


code_book_typst_source <- function(cb) {
  x <- code_book_typst_data(cb)
  c(
    paste("#let data =", typst_literal(x$data)),
    paste("#let strings =", typst_literal(x$strings)),
    "#codebook(data, strings)"
  )
}


# The codebook as the template reads it. Numbers are text, formatted here
# with the decimal mark of the codebook; the keys of `counts` and `stats`
# name their headers in `strings`.
code_book_typst_data <- function(cb) {
  h <- cb$header
  v <- cb$variables
  look <- attr(cb, "appearance", exact = TRUE)
  mark <- attr(cb, "decimal_mark", exact = TRUE)
  num <- function(x, digits) {
    out <- formatC(x, format = "f", digits = digits, decimal.mark = mark)
    replace(out, is.na(x), "")
  }
  # min, max and median are values of the data, written as the data is.
  # mean and sd are estimates: two decimals, or three significant digits
  # between -1 and 1. Tested for zero against the largest statistic of the
  # variable (`top`) first: no "-0.00", and no noise of a floating-point
  # mean (-1.7e-17, a z-score).
  stat <- function(x, estimate, top) {
    x <- if (abs(x) <= 1e-12 * top) 0 else x
    if (!estimate) {
      return(format(
        x,
        scientific = FALSE,
        trim = TRUE,
        drop0trailing = TRUE,
        digits = 15L,
        decimal.mark = mark
      ))
    }
    # 0.995 already prints as 1.00; a floating-point sd of 0.99999999 too.
    if (x == 0 || abs(x) >= 0.995) {
      return(num(x, 2L))
    }
    formatC(signif(x, 3L), format = "fg", decimal.mark = mark)
  }
  # The word "Codebook" heads every page with the title, and subtitles the
  # cover, unless the title already says it.
  word <- spicy_str("title_codebook")
  says <- grepl(tolower(word), tolower(h$title), fixed = TRUE)
  header <- if (is.na(h$title)) {
    word
  } else if (says) {
    h$title
  } else {
    paste(word, "\u2014", h$title)
  }
  stat_cols <- intersect(
    c("min", "max", "mean", "sd", "median", "earliest", "latest"),
    names(v)
  )
  counts <- c("n_valid", "n_missing", "n_declared_missing", "n_distinct")
  rows <- split(cb$values, factor(cb$values$variable, levels = v$name))
  numeric_cols <- intersect(stat_cols, c("min", "max", "mean", "sd", "median"))
  vars <- lapply(seq_len(nrow(v)), function(i) {
    z <- abs(vapply(numeric_cols, \(k) v[[k]][[i]], numeric(1)))
    top <- max(0, z[is.finite(z)])
    stats <- list()
    for (k in stat_cols) {
      s <- v[[k]][[i]]
      if (is.na(s)) {
        next
      }
      estimate <- k %in% c("mean", "sd")
      stats[[k]] <- if (is.character(s)) s else stat(s, estimate, top)
    }
    r <- rows[[i]]
    # The system missing row is the one the object codes "NA".
    na <- r$code == "NA" & !r$declared_missing
    label <- replace(r$label, na, spicy_str("cell_system_missing"))
    list(
      pos = as.character(v$position[[i]]),
      name = v$name[[i]],
      label = if (is.na(v$label[[i]])) "" else v$label[[i]],
      type = v$type[[i]],
      source = v$source[[i]],
      counts = as.list(vapply(counts, \(k) as.character(v[[k]][[i]]), "")),
      stats = stats,
      values = data.frame(
        code = r$code,
        label = replace(label, is.na(label), ""),
        m = r$declared_missing,
        na = na,
        n = as.character(r$n),
        pct = num(r$pct_total, 1L),
        valid = num(r$pct_valid, 1L)
      )
    )
  })
  dm <- h$declared_missing
  data <- list(
    lang = attr(cb, "language", exact = TRUE),
    paper = if (look$paper == "letter") "us-letter" else "a4",
    font = look$font,
    font_code = look$font_code,
    colors = as.list(look$colors),
    title = h$title,
    subtitle = if (!says) word,
    header = header,
    authors = h$authors,
    date = format(h$date),
    meta = data.frame(
      field = vapply(
        c("row_observations", "row_variables", "row_generated_with"),
        spicy_str,
        ""
      ),
      value = c(
        h$n_obs,
        h$n_vars,
        sprintf("spicy %s, R %s", getNamespaceVersion("spicy"), getRversion())
      )
    ),
    notes = as.list(h$notes),
    declared = data.frame(
      code = dm$code,
      label = replace(dm$label, is.na(dm$label), ""),
      variables = dm$variables
    ),
    vars = vars,
    index = as.list(order(v$name, method = "radix") - 1L)
  )
  cols <- union(names(v), names(cb$values))
  strings <- c(
    as.list(stats::setNames(code_book_headers(cols), cols)),
    list(
      page = spicy_str("header_page"),
      variables = spicy_str("header_variables"),
      marker = spicy_str("marker_declared_missing"),
      notes = spicy_str("row_notes"),
      unweighted = spicy_str("note_codebook_unweighted"),
      orcid = "ORCID",
      list = spicy_str("title_codebook_list"),
      declared = spicy_str("title_codebook_declared"),
      sheets = spicy_str("title_codebook_sheets"),
      index = spicy_str("title_codebook_index")
    )
  )
  list(data = data, strings = strings)
}


# A Typst literal: a named list is a dictionary, an unnamed or empty list
# an array, a data frame an array of rows, a vector of length one a value,
# NULL and NA `none`. Text is always a string literal, never markup, so a
# label cannot act as Typst code; line breaks are escaped to keep one
# value per line of the source, and a tab, which Typst shows as nothing,
# becomes a space.
typst_literal <- function(x, indent = "") {
  if (is.data.frame(x)) {
    x <- lapply(seq_len(nrow(x)), \(i) lapply(x, `[[`, i))
  }
  if (!is.list(x)) {
    if (is.null(x) || is.na(x)) {
      return("none")
    }
    if (!is.character(x)) {
      return(tolower(as.character(x)))
    }
    x <- gsub("([\\\\\"])", "\\\\\\1", enc2utf8(x))
    x <- gsub("\n", "\\n", gsub("\r", "\\r", x, fixed = TRUE), fixed = TRUE)
    x <- gsub("\t", " ", x, fixed = TRUE)
    return(paste0("\"", x, "\""))
  }
  if (!length(x)) {
    return("()")
  }
  keys <- names(x)
  inner <- paste0(indent, "  ")
  items <- vapply(x, typst_literal, "", indent = inner, USE.NAMES = FALSE)
  if (!is.null(keys)) {
    items <- paste0(keys, ": ", items)
  }
  # A container of plain values fits on one line, except at the top.
  if (nzchar(indent) && !any(vapply(x, is.list, logical(1)))) {
    one <- if (is.null(keys) && length(x) == 1L) ","
    return(paste0("(", paste(items, collapse = ", "), one, ")"))
  }
  paste0("(\n", paste0(inner, items, ",\n", collapse = ""), indent, ")")
}
