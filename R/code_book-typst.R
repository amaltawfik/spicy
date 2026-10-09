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


code_book_write_pdf <- function(
  cb,
  path,
  quarto,
  call = rlang::caller_env()
) {
  typ <- tempfile(fileext = ".typ")
  on.exit(unlink(typ), add = TRUE)
  code_book_write_typst(cb, typ)
  log <- code_book_typst_compile(quarto, typ, path)
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
      stderr = log,
      call = call
    )
  }
  # A PDF made despite a warning (a glyph missing from the font, ...): the
  # "warning:" lines reach the user as one R warning. The lines under each
  # point into the temporary source, which is gone.
  warned <- grep("^\\s*warning:", log, value = TRUE)
  warned <- unique(sub("^\\s*warning:\\s*", "", warned))
  if (length(warned)) {
    spicy_warn(
      c(
        "Typst warned while compiling the codebook.",
        stats::setNames(warned, rep("!", length(warned)))
      ),
      class = c("spicy_typst_warning", "spicy_passthrough"),
      stderr = log[!grepl(basename(typ), log, fixed = TRUE)]
    )
  }
  invisible(path)
}


# The compile, apart so that a test can stand in for it. Typst's messages,
# with an exit status attached when it failed. A quoted "~" would reach
# Typst unexpanded: R expands it first.
code_book_typst_compile <- function(quarto, typ, path) {
  suppressWarnings(system2(
    quarto,
    c("typst", "compile", shQuote(typ), shQuote(path.expand(path))),
    stdout = TRUE,
    stderr = TRUE
  ))
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
  # min and max are values of the data, written at the precision the two
  # carry, trailing zeros kept: 16.0 next to 38.9. Mean, SD and median are
  # summaries, written at one shared precision: three significant digits
  # of the SD, at most six decimals, or the precision of min and max when
  # there is no SD. An unrounded variable writes min and max like its
  # summaries. Zero is tested against the largest statistic of the
  # variable (`top`) first, and is never signed: no "-0.00", and no noise
  # of a floating-point mean (-1.7e-17, a z-score).
  places <- function(x) {
    if (!is.finite(x)) {
      return(NA_integer_)
    }
    for (d in 0:6) {
      y <- x * 10^d
      if (abs(y - round(y)) <= 1e-9 * max(1, abs(y))) {
        return(d)
      }
    }
    NA_integer_
  }
  # The word "Codebook" heads every page with the title, and stands above
  # it on the cover, unless the title already says it.
  word <- spicy_str("title_codebook")
  says <- grepl(tolower(word), tolower(h$title), fixed = TRUE)
  header <- if (is.na(h$title)) {
    word
  } else if (says) {
    h$title
  } else {
    paste(word, "\u2013", h$title)
  }
  stat_cols <- intersect(
    c("min", "max", "mean", "sd", "median", "earliest", "latest"),
    names(v)
  )
  # No count of declared missing values: the values table lists each code.
  counts <- c("n_valid", "n_missing", "n_distinct")
  rows <- split(cb$values, factor(cb$values$variable, levels = v$name))
  numeric_cols <- intersect(stat_cols, c("min", "max", "mean", "sd", "median"))
  vars <- lapply(seq_len(nrow(v)), function(i) {
    stat <- function(k) if (k %in% names(v)) v[[k]][[i]] else NA_real_
    z <- abs(vapply(numeric_cols, stat, numeric(1)))
    top <- max(0, z[is.finite(z)])
    tidy <- function(x) if (abs(x) <= 1e-12 * top) 0 else x
    s <- stat("sd")
    # The SD rounded to three digits first: a floating-point 0.9999999 is 1.
    e <- if (is.finite(s) && s > 0) {
      min(6, max(0, 2 - floor(log10(signif(s, 3)))))
    } else {
      NA
    }
    ends <- Filter(Negate(is.na), c(stat("min"), stat("max")))
    d <- if (length(ends)) {
      max(vapply(ends, function(x) places(tidy(x)), integer(1)))
    } else {
      NA
    }
    if (is.na(e)) {
      e <- if (is.na(d)) 2 else d
    }
    if (is.na(d)) {
      d <- e
    }
    fmt <- function(x, digits) {
      x <- tidy(x)
      if (round(x, digits) == 0) {
        x <- 0
      }
      formatC(x, format = "f", digits = digits, decimal.mark = mark)
    }
    stats <- list()
    # A time of day has its own row labels: "Earliest time", not "date".
    time <- identical(v$type[[i]], spicy_str("cell_type_time"))
    for (k in stat_cols) {
      x <- v[[k]][[i]]
      if (is.na(x)) {
        next
      }
      key <- if (time && k %in% c("earliest", "latest")) {
        paste0(k, "_time")
      } else {
        k
      }
      stats[[key]] <- if (is.character(x)) {
        x
      } else {
        fmt(x, if (k %in% c("min", "max")) d else e)
      }
    }
    r <- rows[[i]]
    # The system missing row is the one the object codes "NA".
    na <- r$code == "NA" & !r$declared_missing
    label <- replace(r$label, na, spicy_str("cell_system_missing"))
    other <- rep(FALSE, nrow(r))
    # Past `values`, the categories the object does not list are one row,
    # after the listed ones: their number, and the count and percentages
    # of the observations they hold, so that the table still adds up.
    cat <- !r$declared_missing & !na
    omitted <- v$n_categories[[i]] - sum(cat)
    if (!is.na(omitted) && omitted > 0L) {
      n_other <- v$n_valid[[i]] - sum(r$n[cat])
      at <- sum(cat)
      r <- rbind(
        r[seq_len(at), ],
        data.frame(
          position = v$position[[i]],
          variable = v$name[[i]],
          code = "",
          label = NA_character_,
          declared_missing = FALSE,
          n = n_other,
          pct_total = 100 * n_other / h$n_obs,
          pct_valid = 100 * n_other / v$n_valid[[i]]
        ),
        r[-seq_len(at), ]
      )
      na <- append(na, FALSE, after = at)
      label <- append(
        label,
        spicy_fmt("cell_other_categories", omitted),
        after = at
      )
      other <- append(other, TRUE, after = at)
    }
    list(
      pos = as.character(v$position[[i]]),
      name = v$name[[i]],
      label = if (is.na(v$label[[i]])) "" else v$label[[i]],
      type = v$type[[i]],
      source = v$source[[i]],
      declared_codes = v$declared_codes[[i]],
      counts = as.list(vapply(counts, \(k) as.character(v[[k]][[i]]), "")),
      stats = stats,
      values = data.frame(
        code = r$code,
        label = replace(label, is.na(label), ""),
        m = r$declared_missing,
        na = na,
        other = other,
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
    index_columns = look$index_columns,
    font = look$font,
    font_code = look$font_code,
    font_size = look$font_size,
    colors = as.list(look$colors),
    genre = if (!says) word,
    title = h$title,
    subtitle = h$subtitle,
    header = header,
    authors = h$authors,
    date = code_book_cover_date(h$date),
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
    # A note typed "- " or "* " is a list item, any other a paragraph.
    notes = data.frame(
      text = sub("^[-*] +", "", h$notes),
      bullet = grepl("^[-*] ", h$notes)
    ),
    # One dictionary per declared code; its variables as an array, so that
    # the template sets each name on its own (a long one in the small size).
    declared = lapply(seq_len(nrow(dm)), function(i) {
      list(
        code = dm$code[[i]],
        label = if (is.na(dm$label[[i]])) "" else dm$label[[i]],
        variables = as.list(strsplit(dm$variables[[i]], ", ", fixed = TRUE)[[
          1L
        ]])
      )
    }),
    vars = vars,
    index = as.list(order(v$name, method = "radix") - 1L)
  )
  cols <- union(names(v), names(cb$values))
  strings <- c(
    as.list(stats::setNames(code_book_headers(cols), cols)),
    list(
      earliest_time = spicy_str("header_earliest_time"),
      latest_time = spicy_str("header_latest_time"),
      page = spicy_str("header_page"),
      variables = spicy_str("header_variables"),
      marker = spicy_str("marker_declared_missing"),
      missing = spicy_str("header_marker_missing"),
      notes = spicy_str("row_notes"),
      unweighted = spicy_str("note_codebook_unweighted"),
      continued = spicy_str("note_codebook_continued"),
      about = spicy_str("title_codebook_about"),
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

# The date of the cover, in words and in the language of the codebook:
# "8 October 2026", "8 octobre 2026", "1er octobre 2026". The month names
# come from the registry, not from the locale of the machine.
code_book_cover_date <- function(date) {
  months <- strsplit(spicy_str("cover_months"), "|", fixed = TRUE)[[1L]]
  day <- as.integer(format(date, "%d"))
  first <- spicy_str("cover_day_first")
  paste(
    if (day == 1L) first else as.character(day),
    months[[as.integer(format(date, "%m"))]],
    format(date, "%Y")
  )
}
