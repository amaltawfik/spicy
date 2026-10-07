#' Build a codebook of a data frame
#'
#' @description
#' `code_book()` documents the variables of a data frame: one row per
#' variable (position, name, label, type, valid and missing counts,
#' summary statistics), and one row per category of its categorical and
#' logical variables (code, label, count, percentages). The codebook
#' prints as the list of variables, and `output` writes it to an Excel
#' workbook or to a PDF.
#'
#' The counts are unweighted: they describe the file, not a population.
#'
#' @param x A data frame or tibble. For `print()`, a `spicy_codebook`.
#' @param ... Optional tidyselect-style column selectors (e.g.
#'   `starts_with("bmi")`, `where(is.numeric)`). Columns can be selected or
#'   reordered, but renaming selections is not supported.
#' @param title Title of the codebook. Defaults to `"Codebook"`; `NULL`
#'   removes it.
#' @param authors Authors of the codebook: `NULL` (the default), a
#'   character vector whose names are the authors and whose values are
#'   their affiliations (`c("Jane Doe" = "University of Somewhere")`; an
#'   unnamed element is a name without affiliation), or a list of lists
#'   with `name` and the optional `affiliation` and `orcid`.
#' @param notes Character vector of notes on the data (source, exclusions,
#'   coding rules, ...), one note per element.
#' @param source Named character vector mapping the current column names to
#'   the codes they had in the source file, the vector
#'   `dplyr::rename(all_of())` takes. Its names must be selected columns.
#' @param values The maximum number of categories listed per variable in `values`. A
#'   variable with more keeps its count of distinct values in `variables`
#'   and has no rows in `values`. Defaults to `100`; `Inf` lists them all.
#' @param range Logical. If `TRUE` (the default), `variables` gives the
#'   minimum and maximum of each numeric variable and the earliest and
#'   latest date of each date; `FALSE` drops those four columns.
#' @param factor_levels Character. `"all"` (the default; [varlist()] uses
#'   `"observed"`) lists every declared level of a factor, every labelled
#'   code and both values of a logical, with a count of 0 when unused.
#'   `"observed"` lists only the values present in the data.
#' @param user_na Logical. If `TRUE` (the default), declared missing values
#'   count as missing: they are left out of `n_valid`, counted in
#'   `n_missing` and `n_declared_missing`, and listed in `values` with
#'   `declared_missing = TRUE`. If `FALSE`, they count as valid values. See
#'   the "Declared missing values" section.
#' @param decimal_mark Decimal mark of the numbers in the PDF, a single
#'   character such as `"."` or `","`. `NULL` (the default) takes the mark
#'   of `options(spicy.style)`, then the one of the language
#'   (`options(spicy.language = "fr")` gives the comma), then `"."`. The
#'   console list prints counts only, and the Excel file keeps numbers as
#'   numbers.
#' @param font,font_code Fonts of the PDF, for the text and for the names
#'   and codes. `NULL` (the default) uses New Computer Modern and DejaVu
#'   Sans Mono, which Typst embeds, so the PDF looks the same whatever the
#'   machine. Any other font must be one Typst finds, named exactly as
#'   `quarto typst fonts` lists it; a `.typ` output keeps the name as
#'   given, unchecked. A `font` also sets the font of the Excel file,
#'   which otherwise keeps its default font.
#' @param colors Named character vector of `"#RRGGBB"` colors replacing
#'   part of the palette of the PDF: `primary` (title, headings and the
#'   text of table headers), `accent` (links and the declared missing
#'   marker), `band` (behind table headers), `band_dark` (the band of each
#'   variable, under white text), `zebra`, `grid` (rules), `text` and
#'   `muted`. The headers of the Excel file take `primary` and `band` too.
#' @param paper Paper size of the PDF: `"a4"` (the default) or `"letter"`.
#' @param output `NULL` (the default) returns the codebook, which prints as
#'   the list of variables. A path writes the codebook to that file, in
#'   the format of its extension, and returns it invisibly: `.xlsx` for an
#'   Excel workbook (this requires `openxlsx2`), `.pdf` for a PDF (this
#'   requires the `quarto` package and Quarto 1.7 or later, found on the
#'   PATH or through the `QUARTO_PATH` environment variable), `.typ` for
#'   the Typst source of that PDF.
#'
#' @details
#' The type of a variable is read off its R class, never guessed: a factor
#' is categorical, an ordered factor ordinal, a `haven_labelled` vector
#' categorical with labelled codes, an integer or double vector numeric,
#' and a logical, character, `Date` or `POSIXct` vector logical, text, date
#' or date-time. Any other class is shown as the class itself. The R class
#' stays in its own column. With `user_na = TRUE`, a `haven_labelled`
#' vector whose value labels all sit on declared missing codes, or that has
#' no labels, is numeric, or text when it stores characters.
#'
#' `values` lists the categories of factors and labelled vectors and the
#' two values of a logical, then the declared missing values of the
#' variable and a row for its system missing values (`code = "NA"`).
#' Numeric, text and date variables have no category rows: they appear in
#' `values` only through their declared missing values. The codes of a
#' labelled vector come in code order, the levels of a factor in level
#' order.
#'
#' Dates are written in ISO 8601: a date-time in the time zone the variable
#' carries, and in UTC when it carries none, the zone being named in either
#' case.
#'
#' The language of the labels follows `options(spicy.language)` when the
#' codebook is built (see [spicy_labels()]); the labels of the data are
#' never translated.
#'
#' `print()` shortens long variable labels when that makes the list fit
#' the console; the object keeps them whole.
#'
#' @inheritSection freq Declared missing values
#'
#' @section Excel output:
#' The workbook has three sheets, named in the language of the codebook.
#' The first, `codebook`, holds the header as field-value pairs: title, one
#' row per author, date, numbers of observations and variables, declared
#' missing values, notes, and the versions of spicy and R that wrote it.
#' The other two, `variables` and `values`, are the two tables of the
#' object from the first row, with a frozen header and filters: numbers
#' stay numeric cells and dates are ISO text.
#'
#' @section PDF output:
#' The PDF opens on a cover (title, authors, date, numbers of observations
#' and variables, notes), lists the variables with the page of each, and
#' summarizes the declared missing values. One sheet per variable follows
#' (counts, statistics, and the table of its values, where `M` marks a
#' declared missing value), then an alphabetical index. A sheet breaks
#' across pages only when it does not fit on one. `code_book()`
#' writes the Typst source and compiles it with the Typst that Quarto
#' bundles. Without Quarto, `output = "<path>.typ"` writes the same
#' source, self-contained: `typst compile` makes the PDF on any machine.
#'
#' @return A `spicy_codebook` object, returned invisibly when `output` is
#' given: a list with
#' \describe{
#'   \item{`header`}{A list: `title`, `authors` (a tibble with `name`,
#'     `affiliation` and `orcid`), `date`, `n_obs`, `n_vars`, `notes`, and
#'     `declared_missing`, a tibble of the declared missing values found in
#'     the data (`code`, `label`, `variables`, `n_variables`).}
#'   \item{`variables`}{A tibble, one row per variable: `position` (the
#'     column's position in `x`), `name`, `label`, `type`, `class`,
#'     `source`, `n_valid`, `n_missing`, `n_declared_missing`,
#'     `n_distinct`, then `min`, `max`, `mean`, `sd` and `median` for
#'     numeric variables and `earliest` and `latest` for dates.}
#'   \item{`values`}{A tibble, one row per value: `variable`, `code`,
#'     `label`, `declared_missing`, `n`, `pct_total` and `pct_valid`.}
#' }
#' The attributes `language` and `decimal_mark` record the language and
#' the decimal mark the codebook was built with, and `appearance` the look
#' of its PDF: a list of `font`, `font_code`, `colors` (all eight) and
#' `paper`.
#'
#' @examples
#' code_book(sochealth)
#'
#' cb <- code_book(
#'   sochealth,
#'   sex,
#'   starts_with("bmi"),
#'   title = "Body mass index",
#'   authors = c("Jane Doe" = "University of Somewhere"),
#'   notes = "BMI computed from self-reported height and weight."
#' )
#' cb$variables
#' cb$values
#'
#' if (requireNamespace("openxlsx2", quietly = TRUE)) {
#'   path <- tempfile(fileext = ".xlsx")
#'   code_book(sochealth, output = path)
#' }
#'
#' # The Typst source of the PDF, which `output = "<path>.pdf"` compiles
#' # when Quarto is installed.
#' code_book(sochealth, output = tempfile(fileext = ".typ"))
#'
#' @seealso
#' [varlist()] to explore the variables in the Viewer; [freq()] for the
#' frequency table of one variable.
#'
#' @family variable inspection
#' @export
code_book <- function(
  x,
  ...,
  title = "Codebook",
  authors = NULL,
  notes = NULL,
  source = NULL,
  values = 100,
  range = TRUE,
  factor_levels = c("all", "observed"),
  user_na = TRUE,
  decimal_mark = NULL,
  font = NULL,
  font_code = NULL,
  colors = NULL,
  paper = c("a4", "letter"),
  output = NULL
) {
  if (!is.data.frame(x)) {
    spicy_abort(
      "`x` must be a data frame or tibble.",
      class = "spicy_invalid_data"
    )
  }

  validate_code_book_control_dots(rlang::enquos(..., .named = FALSE))
  validate_code_book_title(title)
  authors <- code_book_authors(authors)
  validate_code_book_notes(notes)
  notes <- notes[nzchar(trimws(notes))]
  validate_code_book_values(values)
  validate_varlist_logical(range, "range")
  factor_levels <- match_varlist_factor_levels(factor_levels)
  validate_varlist_logical(user_na, "user_na")
  decimal_mark <- code_book_decimal_mark(decimal_mark)
  appearance <- code_book_appearance(font, font_code, colors, paper)
  format <- code_book_output_format(output)
  quarto <- if (identical(format, "pdf")) code_book_quarto(c(font, font_code))
  lang <- getOption("spicy.language", NULL)
  lang <- if (is.null(lang)) "en" else .spicy_language_option(lang)

  vl <- varlist(
    x,
    ...,
    tbl = TRUE,
    factor_levels = factor_levels,
    user_na = user_na
  )
  validate_code_book_source(source, vl$Variable)
  cols <- x[vl$Variable]
  # An NA level (`addNA()`) is system missing, as in freq(): factor()
  # drops it and keeps the other levels and the ordering.
  na_level <- vapply(
    cols,
    \(col) is.factor(col) && anyNA(levels(col)),
    logical(1)
  )
  cols[na_level] <- lapply(
    cols[na_level],
    \(col) factor(col, levels = levels(col))
  )
  kinds <- vapply(cols, code_book_kind, character(1), user_na = user_na)
  per_var <- lapply(seq_along(cols), function(i) {
    code_book_values(
      cols[[i]],
      vl$Variable[[i]],
      kinds[[i]],
      values,
      factor_levels,
      user_na
    )
  })
  stats <- vapply(
    seq_along(cols),
    function(i) code_book_stats(cols[[i]], kinds[[i]]),
    c(min = 0, max = 0, mean = 0, sd = 0, median = 0)
  )
  dates <- vapply(
    seq_along(cols),
    function(i) code_book_dates(cols[[i]], kinds[[i]]),
    c(earliest = "", latest = "")
  )

  variables <- tibble::tibble(
    position = match(vl$Variable, names(x)),
    name = vl$Variable,
    label = vl$Label,
    type = vapply(
      seq_along(cols),
      function(i) code_book_type(kinds[[i]], cols[[i]]),
      character(1)
    ),
    class = vl$Class,
    source = unname(source[vl$Variable]) %||% NA_character_,
    n_valid = vl$N_valid,
    n_missing = vl$NAs,
    n_declared_missing = vapply(
      cols,
      function(col) if (user_na) sum(.user_na_mask(col)) else 0L,
      integer(1),
      USE.NAMES = FALSE
    ),
    n_distinct = vl$N_distinct,
    min = unname(stats["min", ]),
    max = unname(stats["max", ]),
    mean = unname(stats["mean", ]),
    sd = unname(stats["sd", ]),
    median = unname(stats["median", ]),
    earliest = unname(dates["earliest", ]),
    latest = unname(dates["latest", ])
  )
  for (i in which(na_level)) {
    col <- cols[[i]]
    variables$n_missing[[i]] <- sum(is.na(col))
    variables$n_valid[[i]] <- length(col) - sum(is.na(col))
    variables$n_distinct[[i]] <- length(unique(col[!is.na(col)]))
  }
  if (!range) {
    variables <- variables[
      setdiff(names(variables), c("min", "max", "earliest", "latest"))
    ]
  }

  values_tbl <- do.call(rbind, lapply(per_var, `[[`, "rows"))
  if (is.null(values_tbl)) {
    values_tbl <- data.frame(
      variable = character(),
      code = character(),
      label = character(),
      declared_missing = logical(),
      n = integer(),
      pct_total = numeric(),
      pct_valid = numeric()
    )
  }
  declared <- do.call(
    rbind,
    lapply(seq_along(per_var), function(i) {
      d <- per_var[[i]]$declared
      if (!is.null(d)) {
        data.frame(variable = vl$Variable[[i]], code = d$code, label = d$label)
      }
    })
  )

  cb <- structure(
    list(
      header = list(
        title = title %||% NA_character_,
        authors = authors,
        date = Sys.Date(),
        n_obs = nrow(x),
        n_vars = nrow(variables),
        notes = notes %||% character(),
        declared_missing = code_book_declared(declared)
      ),
      variables = variables,
      values = tibble::as_tibble(values_tbl)
    ),
    class = "spicy_codebook",
    language = lang,
    decimal_mark = decimal_mark,
    appearance = appearance
  )

  if (is.null(format)) {
    return(cb)
  }
  switch(
    format,
    xlsx = code_book_write_xlsx(cb, output, font),
    typ = code_book_write_typst(cb, output),
    pdf = code_book_write_pdf(cb, output, quarto)
  )
  invisible(cb)
}


#' @rdname code_book
#' @export
print.spicy_codebook <- function(x, ...) {
  # The type column was written in the language the codebook was built
  # in; the headers around it follow the same language.
  old <- options(spicy.language = attr(x, "language", exact = TRUE) %||% "en")
  on.exit(options(old), add = TRUE)

  info <- code_book_info(x$header)
  top <- info$value[info$key %in% c("title", "author")]
  facts <- info[!info$key %in% c("title", "author"), , drop = FALSE]
  v <- x$variables
  disp <- data.frame(
    position = as.character(v$position),
    name = v$name,
    label = v$label,
    type = v$type,
    n_valid = as.character(v$n_valid),
    n_missing = as.character(v$n_missing)
  )
  # build_ascii_table() rather than spicy_print_table(): the width split of
  # the latter repeats every left-aligned column on each panel, which for a
  # list of names and labels prints the labels once per panel.
  render <- function(d) {
    build_ascii_table(
      d,
      align_left_cols = 2:4,
      total_row_idx = integer(0),
      display_labels = code_book_headers(names(d))
    )
  }
  tbl <- render(disp)
  # Wider than the console: the labels give way, down to 12 characters,
  # and end in an ellipsis, but only when that makes the table fit. Names
  # and types stay whole. The object and the Excel file keep the full
  # labels.
  first <- strsplit(tbl, "\n", fixed = TRUE)[[1L]][[1L]]
  excess <- crayon::col_nchar(first, type = "width") - getOption("width")
  n <- nchar(disp$label)
  w <- max(0L, n, na.rm = TRUE) - excess
  if (excess > 0L && w >= 12L) {
    long <- which(n > w)
    disp$label[long] <- paste0(
      substr(disp$label[long], 1L, w - 1L),
      spicy_str("marker_truncation_ellipsis")
    )
    tbl <- render(disp)
  }
  lines <- c(
    top,
    if (length(top)) "",
    spicy_fmt("note_field_line", facts$field, facts$value),
    "",
    tbl
  )
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(x)
}


# Internal kind of a column, read off its class. A token, never displayed:
# `code_book_type()` turns it into the reader's word. Under `user_na`, a
# labelled vector whose labels only name declared missing codes has no
# categories: it is the numbers or the text it stores.
code_book_kind <- function(col, user_na) {
  cls <- class(col)
  if (is.ordered(col)) {
    "ordinal"
  } else if (is.factor(col)) {
    "categorical"
  } else if (inherits(col, c("haven_labelled", "labelled_spss"))) {
    labs <- attr(col, "labels", exact = TRUE)
    if (!user_na || length(labs) > length(.user_na_labels(col))) {
      "labelled"
    } else if (is.character(col)) {
      "text"
    } else {
      "numeric"
    }
  } else if (inherits(col, "POSIXct")) {
    "datetime"
  } else if (inherits(col, "Date")) {
    "date"
  } else if (identical(cls, "logical")) {
    "logical"
  } else if (identical(cls, "character")) {
    "text"
  } else if (identical(cls, "integer") || identical(cls, "numeric")) {
    "numeric"
  } else {
    "other"
  }
}


code_book_type <- function(kind, col) {
  switch(
    kind,
    categorical = spicy_str("cell_type_categorical"),
    ordinal = spicy_str("cell_type_ordinal"),
    labelled = spicy_str("cell_type_labelled"),
    numeric = spicy_str("cell_type_numeric"),
    logical = spicy_str("cell_type_logical"),
    text = spicy_str("cell_type_text"),
    date = spicy_str("cell_type_date"),
    datetime = spicy_str("cell_type_datetime"),
    class(col)[[1L]]
  )
}


code_book_stats <- function(col, kind) {
  out <- rep(NA_real_, 5L)
  if (kind != "numeric") {
    return(out)
  }
  v <- as.double(col[!is.na(col)])
  if (!length(v)) {
    return(out)
  }
  c(min(v), max(v), mean(v), stats::sd(v), stats::median(v))
}


code_book_dates <- function(col, kind) {
  if (!kind %in% c("date", "datetime") || all(is.na(col))) {
    return(c(NA_character_, NA_character_))
  }
  r <- range(col, na.rm = TRUE)
  if (kind == "date") {
    return(format(r, "%Y-%m-%d"))
  }
  tz <- attr(col, "tzone", exact = TRUE)[1L]
  if (is.null(tz) || is.na(tz) || !nzchar(tz)) {
    tz <- "UTC"
  }
  paste(format(r, "%Y-%m-%d %H:%M:%S", tz = tz), tz)
}


# The rows of `values` for one variable, and its declared missing values
# (kept for the header even when the rows are not listed). Factors,
# labelled vectors and logicals list their categories, then their declared
# and system missing values. Any other variable has rows only when it has
# declared missing values.
code_book_values <- function(col, name, kind, values, factor_levels, user_na) {
  out <- list(rows = NULL, declared = NULL)
  declared <- if (user_na) .user_na_mask(col) else logical(length(col))
  # Under factor_levels = "all", a declared code that no observation
  # carries is listed with a count of 0, as an unused level is: its label
  # joins the observed declared values with a weight of 0.
  extra <- if (user_na && factor_levels == "all") .user_na_labels(col)
  if (any(declared) || length(extra)) {
    probe <- structure(
      c(unclass(col)[declared], unname(unclass(extra))),
      labels = attr(col, "labels", exact = TRUE)
    )
    out$declared <- .user_na_info(
      probe,
      weights = rep(1:0, c(sum(declared), length(extra)))
    )
  }
  categorical <- kind %in% c("categorical", "ordinal", "labelled", "logical")
  if (!categorical && is.null(out$declared)) {
    return(out)
  }

  n_total <- length(col)
  col <- if (user_na) .user_na_to_na(col) else .user_na_zap(col)
  code <- character()
  label <- character()
  n <- integer()
  if (is.factor(col)) {
    n <- tabulate(col, nbins = nlevels(col))
    keep <- factor_levels == "all" | n > 0L
    code <- levels(col)[keep]
    label <- rep(NA_character_, length(code))
    n <- n[keep]
  } else if (categorical) {
    vals <- as.vector(unclass(col))
    labs <- attr(col, "labels", exact = TRUE)
    lab_codes <- as.vector(unname(unclass(labs)))
    obs <- vals[!is.na(vals)]
    cats <- obs
    if (factor_levels == "all") {
      cats <- c(
        lab_codes[!is.na(lab_codes)],
        obs,
        if (is.logical(obs)) c(FALSE, TRUE)
      )
    }
    cats <- safe_sort_unique(cats)
    n <- tabulate(match(obs, cats), nbins = length(cats))
    code <- .format_code(cats)
    label <- names(labs)[match(cats, lab_codes)] %||%
      rep(NA_character_, length(cats))
  }
  if (length(code) > values) {
    return(out)
  }
  # A level spelled "NA" is quoted, as varlist() does, so that it cannot
  # be read as the row of the system missing values.
  code <- format_varlist_values(code)
  n_na <- sum(is.na(col) & !declared)

  dm <- out$declared
  n_dm <- NROW(dm)
  has_na <- n_na > 0L
  pct <- function(a, b) if (b > 0) 100 * a / b else rep(NA_real_, length(a))
  counts <- as.integer(c(n, dm$n, if (has_na) n_na))
  out$rows <- data.frame(
    variable = rep(name, length(counts)),
    code = c(code, dm$code, if (has_na) "NA"),
    label = c(label, dm$label, if (has_na) NA_character_),
    declared_missing = c(
      rep(FALSE, length(code)),
      rep(TRUE, n_dm),
      rep(FALSE, has_na)
    ),
    n = counts,
    pct_total = pct(counts, n_total),
    pct_valid = c(pct(n, sum(n)), rep(NA_real_, n_dm + has_na))
  )
  out
}


# One row per declared missing value found in the data, with the
# variables that carry it, in order of first appearance.
code_book_declared <- function(d) {
  if (is.null(d)) {
    return(tibble::tibble(
      code = character(),
      label = character(),
      variables = character(),
      n_variables = integer()
    ))
  }
  key <- paste(d$code, d$label, sep = "\r")
  first <- !duplicated(key)
  vars <- lapply(key[first], function(k) d$variable[key == k])
  tibble::tibble(
    code = d$code[first],
    label = d$label[first],
    variables = vapply(vars, paste, character(1), collapse = ", "),
    n_variables = lengths(vars)
  )
}


# The header as field-value rows, for the print and the first Excel sheet.
# `key` is an internal token: nothing branches on the displayed field.
code_book_info <- function(header, orcid = FALSE) {
  a <- header$authors
  author <- a$name
  aff <- nzchar(trimws(a$affiliation))
  author[aff] <- paste0(author[aff], " \u2014 ", a$affiliation[aff])
  if (orcid) {
    id <- nzchar(trimws(a$orcid))
    author[id] <- paste0(author[id], " \u2014 ORCID ", a$orcid[id])
  }
  dm <- header$declared_missing
  declared <- vapply(
    seq_len(nrow(dm)),
    function(i) {
      code <- dm$code[[i]]
      if (!is.na(dm$label[[i]])) {
        code <- paste0(code, " = ", dm$label[[i]])
      }
      key <- if (dm$n_variables[[i]] == 1L) {
        "cell_declared_one"
      } else {
        "cell_declared_many"
      }
      spicy_fmt(key, code, dm$n_variables[[i]])
    },
    character(1)
  )
  parts <- list(
    title = if (is.na(header$title)) character() else header$title,
    author = author,
    date = format(header$date),
    observations = as.character(header$n_obs),
    variables = as.character(header$n_vars),
    declared = declared,
    note = header$notes
  )
  fields <- c(
    title = "row_title",
    author = "row_author",
    date = "row_date",
    observations = "row_observations",
    variables = "row_variables",
    declared = "row_declared_missing",
    note = "row_note"
  )
  n <- lengths(parts)
  data.frame(
    key = rep(names(parts), n),
    field = rep(vapply(fields, spicy_str, character(1), USE.NAMES = FALSE), n),
    value = unlist(parts, use.names = FALSE)
  )
}


# The reader's header for each column of `variables` and `values`.
code_book_headers <- function(cols) {
  keys <- c(
    position = "header_position",
    name = "header_variable",
    label = "header_label",
    type = "header_type",
    class = "header_r_class",
    source = "header_source",
    n_valid = "header_valid",
    n_missing = "header_missing",
    n_declared_missing = "header_declared_missing",
    n_distinct = "header_distinct",
    min = "header_min",
    max = "header_max",
    mean = "header_codebook_mean",
    sd = "header_sd",
    median = "header_codebook_median",
    earliest = "header_earliest",
    latest = "header_latest",
    variable = "header_variable",
    code = "header_code",
    declared_missing = "header_is_declared_missing",
    n = "header_n_lower",
    pct_total = "header_pct_total",
    pct_valid = "header_pct_valid"
  )
  vapply(keys[cols], spicy_str, character(1), USE.NAMES = FALSE)
}
