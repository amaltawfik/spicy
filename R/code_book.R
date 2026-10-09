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
#' @param title Title of the codebook, such as the name of the study. The
#'   PDF adds the word "Codebook" above it on the cover and before it in
#'   the page header, unless the title already contains the word.
#'   Defaults to `"Codebook"`. `NULL` leaves the console and the Excel file
#'   without a title; the cover of the PDF then shows "Codebook" once.
#' @param subtitle Subtitle of the codebook, under the title: the wave, the
#'   edition, the extract (`"Wave 3, 2026, public-use file"`).
#' @param authors Authors of the codebook: `NULL` (the default), a
#'   character vector whose names are the authors and whose values are
#'   their affiliations (`c("Jane Doe" = "University of Somewhere")`; an
#'   unnamed element is a name without affiliation), or a list of lists
#'   with `name` and the optional `affiliation` and `orcid`. An ORCID given
#'   as its `https://orcid.org/` address is kept as the identifier alone.
#' @param notes Character vector of notes on the data (source, exclusions,
#'   coding rules, ...), one note per element; blank notes are dropped. In
#'   the PDF, an element that starts with `"- "` or `"* "` is a list item,
#'   consecutive items making one list, and any other element is a
#'   paragraph:
#'   `notes = c("Wave 3 only.", "- Weight: design weight.", "- BMI: kg/m2.")`.
#'   A note may carry `*italics*`, `**bold**`, and `` `code` `` (not
#'   nested), and a web address or a `doi:` becomes a link. The PDF
#'   renders these, turns straight apostrophes into typographic ones and,
#'   in French, the space before `:`, `;`, `!`, or `?` into a non-breaking
#'   space; the console and the Excel file show the words without the
#'   marks.
#' @param source Named character vector mapping the current column names to
#'   the codes they had in the source file, the vector
#'   `dplyr::rename(all_of())` takes. Its names must be selected columns,
#'   and each code must be non-empty.
#' @param values The maximum number of categories listed per variable in
#'   the `values` table; under `factor_levels = "all"`, unused levels and
#'   codes count. A variable with more lists its first `values` categories
#'   in their order, with their counts and percentages of the whole
#'   variable, then its declared and system missing values; `n_categories`
#'   in `variables` gives the total, and the PDF adds a row for the
#'   categories not listed. Defaults to `100`; `Inf` lists them all.
#' @param range Logical. If `TRUE` (the default), `variables` gives the
#'   minimum and maximum of each numeric variable and the earliest and
#'   latest date of each date; `FALSE` drops those four columns.
#' @param factor_levels Character. `"all"` (the default; [varlist()] uses
#'   `"observed"`) lists every declared level of a factor, every labelled
#'   code, every code of `na_values`, and both values of a logical, with a
#'   count of 0 when unused. `"observed"` lists only the values present in
#'   the data.
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
#' @param font,font_code Fonts of the PDF: `font` for the text, the values
#'   and their codes included, and `font_code` for the variable names and
#'   the source codes. `NULL` (the default) uses New Computer Modern and
#'   DejaVu Sans Mono, which Typst embeds, so the PDF looks the same on
#'   every machine for the characters these fonts cover; any other
#'   character falls back to a font found on the machine. Any other font
#'   must be one Typst finds, named exactly as `quarto typst fonts` lists
#'   it; a `.typ` output keeps the name as given, unchecked. A `font` also
#'   sets the font of the Excel file, which otherwise keeps its default
#'   font.
#' @param font_size Size of the text of the PDF, in points: `10` (the
#'   default), or a number from 6 to 24. The title of the cover and the
#'   small size of a long name follow it; the margins do not.
#' @param colors Named character vector of `"#RRGGBB"` colors replacing
#'   part of the palette of the PDF: `primary` (title, headings, and the
#'   text of table headers), `accent` (the word "Codebook" above the title,
#'   and the links), `band` (behind table headers), `band_dark` (the band
#'   of each variable, under white text), `zebra` (behind the label rows of
#'   a variable), `grid` (rules), `text`, and `muted`. A `band` given
#'   without `zebra` brings a lighter tint of itself as `zebra`, and a
#'   `band_dark` given without `grid` a light tint of itself as `grid`. The
#'   headers of the Excel file take `primary` and `band` too.
#' @param paper Paper size of the PDF: `"a4"` (the default) or `"letter"`.
#' @param index_columns Columns of the index of variables at the end of the
#'   PDF: `1` or `2`. `NULL` (the default) sets two columns past 40
#'   variables when no name exceeds 40 characters, one column otherwise. A
#'   name longer than that may overflow a column of two.
#' @param output `NULL` (the default) returns the codebook, which prints as
#'   the list of variables. A path writes the codebook to that file, in
#'   the format of its extension, and returns it invisibly: `.xlsx` for an
#'   Excel workbook (this requires `openxlsx2`), `.pdf` for a PDF (this
#'   requires the `quarto` package and Quarto 1.7 or later, found on the
#'   PATH or through the `QUARTO_PATH` environment variable), `.typ` for
#'   the Typst source of that PDF. The path names a file, in a directory
#'   that exists.
#'
#' @details
#' The type of a variable is read off its R class, never guessed: a factor
#' is categorical (nominal), an ordered factor categorical (ordinal), a
#' `haven_labelled` vector categorical (labelled codes), an integer or
#' double vector numeric, a logical, character, or `Date` vector logical,
#' text, or date, a `POSIXct` or `POSIXlt` vector date-time, and an `hms`
#' vector time. The level of measurement comes from the declaration alone:
#' a factor whose order was not declared with `ordered()` is nominal. A
#' vector of any other class is shown by its first class (`difftime`,
#' ...), without statistics. The R class stays in its own column. A `haven_labelled`
#' vector without value labels is numeric, or text when it stores
#' characters, and so is one whose value labels all sit on declared
#' missing codes, with `user_na = TRUE`.
#'
#' `values` lists the categories of factors and labelled vectors and the
#' two values of a logical, then the declared missing values of the
#' variable and, when it has any, a row for its system missing values
#' (`code = "NA"`). Numeric, text, and date variables have no category
#' rows: they appear in `values` only through their declared missing
#' values. The codes of a labelled vector come in code order, the levels
#' of a factor in level order. An explicit `NA` level of a factor (from
#' [addNA()]) counts as missing, in the row of the system missing values.
#'
#' Dates are written as `YYYY-MM-DD`, date-times as
#' `YYYY-MM-DD HH:MM:SS` followed by the name of the time zone the
#' variable carries, or `UTC` when it carries none, and times as
#' `HH:MM:SS`.
#'
#' The words the codebook adds (column headers, types, field and sheet
#' names) follow `options(spicy.language)` when the codebook is built (see
#' [spicy_labels()]); the variable and value labels of the data are never
#' translated.
#'
#' `print()` shows a line break or a tab in a label as a space, and
#' shortens long variable labels, counting display columns, when that
#' makes the list fit the console; the object keeps the labels whole.
#'
#' @inheritSection freq Declared missing values
#'
#' @section Excel output:
#' The workbook has three worksheets, named in the language of the
#' codebook. The first, `codebook`, holds the header as field-value pairs:
#' title, subtitle, one row per author, date, numbers of observations and
#' variables, declared missing values, notes, and the versions of spicy and
#' R that wrote it, with the date and time of writing. The other two,
#' `variables` and `values`, hold the two tables of the object from the
#' first row, under the column headers the console and the PDF show
#' (`code` is "Value", `n_valid` "Valid"), with a frozen header and
#' filters. Numbers stay numeric cells, the percentages shown to one
#' decimal; a statistic that is not finite (of a column holding `Inf`) is
#' an empty cell, and dates stay text, written as above.
#'
#' @section PDF output:
#' The PDF opens on a cover: the word "Codebook" above the title (unless
#' the title already contains it), the subtitle, the authors with their
#' affiliations and ORCID addresses, the date, and at the foot the
#' versions of spicy and R that made it. A page about the data follows
#' (numbers of observations and variables, then the notes and the declared
#' missing values), then the list of variables with the page of each.
#'
#' One sheet per variable comes next, under the heading "Variable sheets":
#' a dark band with the position, name, and type (a name over 45
#' characters takes a row of its own), rows for the label, the declared
#' missing codes, and the source code, then the counts, the statistics,
#' and the table of the values. On a sheet that lists declared missing
#' codes, a Missing column marks them with `M`; the row of the system
#' missing values reads "System missing". Minimum and maximum are written
#' at the precision of the data; mean, SD, and median at three significant
#' digits of the SD. A sheet breaks across pages only when it does not fit
#' on one; its table of values then repeats its header under
#' "name (continued)". An index of the variables closes the document,
#' sorted by name in byte order, where uppercase letters come before
#' lowercase ones.
#'
#' `code_book()` writes the Typst source and compiles it with the Typst
#' that Quarto bundles; Typst warnings, such as a character missing from
#' the fonts, arrive as one R warning of class `spicy_typst_warning`.
#' Without Quarto, `output = "<path>.typ"` writes the same source,
#' self-contained: `typst compile` makes the PDF on any machine with Typst
#' 0.12 or later.
#'
#' @return A `spicy_codebook` object, returned invisibly when `output` is
#' given: a list with
#' \describe{
#'   \item{`header`}{A list: `title`, `subtitle`, `authors` (a tibble with
#'     `name`, `affiliation`, and `orcid`), `date`, `n_obs`, `n_vars`,
#'     `notes` (blank notes dropped), and `declared_missing`, a tibble of
#'     the declared missing values found in the data and, under
#'     `factor_levels = "all"`, of the declared codes no observation
#'     carries, sorted by code then label (`code`, `label`, `variables`,
#'     `n_variables`).}
#'   \item{`variables`}{A tibble, one row per variable: `position` (the
#'     column's position in `x`), `name`, `label`, `type`, `class`,
#'     `source`, `n_valid`, `n_missing`, `n_declared_missing`,
#'     `declared_codes` (the `na_values` and `na_range` of a
#'     `haven_labelled_spss` vector, as text, the two parts separated by a
#'     semicolon; `NA` without them or under `user_na = FALSE`),
#'     `n_distinct`, `n_categories` (the categories of a categorical or
#'     logical variable, listed or not; `NA` otherwise), then `min`,
#'     `max`, `mean`, `sd`, and `median` for
#'     numeric variables and `earliest` and `latest` for dates and times.
#'     `range = FALSE` drops `min`, `max`, `earliest`, and `latest`.}
#'   \item{`values`}{A tibble, one row per value: `variable`, `code`,
#'     `label`, `declared_missing`, `n`, `pct_total`, and `pct_valid`. The
#'     row of the system missing values (`code = "NA"`) exists only for a
#'     variable that has missing values.}
#' }
#' The attributes `language` and `decimal_mark` record the language and
#' the decimal mark the codebook was built with, and `appearance` the look
#' of its PDF: a list of `font`, `font_code`, `font_size`, `colors` (all
#' eight), `paper`, and `index_columns`.
#'
#' @examples
#' code_book(sochealth)
#'
#' cb <- code_book(
#'   sochealth,
#'   starts_with("bmi"),
#'   title = "Social health survey",
#'   subtitle = "Body mass index",
#'   authors = c("Jane Doe" = "University of Somewhere"),
#'   notes = "Simulated data (see ?sochealth)."
#' )
#' cb
#' cb$variables
#' cb$values
#'
#' # Labelled survey data: declared missing codes count as missing and are
#' # flagged in `values`.
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   trust <- haven::labelled_spss(
#'     c(1, 2, 2, 3, 4, 8, 9, 1, NA, 2),
#'     labels = c(
#'       "Not at all" = 1, "A little" = 2, "Somewhat" = 3, "A lot" = 4,
#'       "Don't know" = 8, "Refused" = 9
#'     ),
#'     na_values = c(8, 9),
#'     label = "Trust in the health system"
#'   )
#'   code_book(tibble::tibble(trust))$values
#' }
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
#' frequency table of one variable; the article
#' [Explore variables and build codebooks](https://amaltawfik.github.io/spicy/articles/variable-exploration.html).
#'
#' @family variable inspection
#' @export
code_book <- function(
  x,
  ...,
  title = "Codebook",
  subtitle = NULL,
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
  font_size = 10,
  colors = NULL,
  paper = c("a4", "letter"),
  index_columns = NULL,
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
  validate_code_book_title(subtitle, "subtitle")
  authors <- code_book_authors(authors)
  validate_code_book_notes(notes)
  notes <- notes[nzchar(trimws(notes))]
  validate_code_book_values(values)
  validate_varlist_logical(range, "range")
  factor_levels <- match_varlist_factor_levels(factor_levels)
  validate_varlist_logical(user_na, "user_na")
  decimal_mark <- code_book_decimal_mark(decimal_mark)
  appearance <- code_book_appearance(
    font,
    font_code,
    colors,
    paper,
    index_columns,
    font_size
  )
  format <- code_book_output_format(output)
  quarto <- if (identical(format, "pdf")) code_book_quarto(c(font, font_code))
  lang <- getOption("spicy.language", NULL)
  lang <- if (is.null(lang)) "en" else .spicy_language_option(lang)

  # The counts of varlist(), without its Values column, which the codebook
  # does not keep and which costs most of varlist()'s time on a large
  # file. Its errors name code_book().
  vl <- varlist_impl(
    x,
    ...,
    tbl = TRUE,
    factor_levels = factor_levels,
    user_na = user_na,
    summaries = FALSE,
    fn = "code_book()"
  )
  validate_code_book_source(source, vl$Variable)
  # As a data frame: the `[` of an sf object keeps its geometry column.
  cols <- as.data.frame(x)[vl$Variable]
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
  # A POSIXlt column is a date-time, summarised as the POSIXct it
  # converts to, in its own time zone.
  lt <- vapply(cols, inherits, logical(1), what = "POSIXlt")
  cols[lt] <- lapply(cols[lt], as.POSIXct)
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
    declared_codes = vapply(
      cols,
      code_book_declared_codes,
      character(1),
      user_na = user_na,
      USE.NAMES = FALSE
    ),
    n_distinct = vl$N_distinct,
    n_categories = vapply(
      per_var,
      function(p) p$n_categories %||% NA_integer_,
      integer(1)
    ),
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
        subtitle = subtitle %||% NA_character_,
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
  is_top <- info$key %in% c("title", "subtitle", "author")
  top <- info$value[is_top]
  facts <- info[!is_top, , drop = FALSE]
  v <- x$variables
  disp <- data.frame(
    position = as.character(v$position),
    name = v$name,
    # A line break or a tab would split the row of the label.
    label = gsub("[\r\n\t]+", " ", v$label),
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
  # Wider than the console: the labels give way, down to 12 columns, and
  # end in an ellipsis, but only when that makes the table fit. Names and
  # types stay whole. The object and the Excel file keep the full labels.
  # Widths are counted in columns of the console, where a CJK character
  # takes two.
  first <- strsplit(tbl, "\n", fixed = TRUE)[[1L]][[1L]]
  excess <- crayon::col_nchar(first, type = "width") - getOption("width")
  n <- nchar(disp$label, type = "width")
  n[is.na(disp$label)] <- 0L
  w <- max(0L, n) - excess
  if (excess > 0L && w >= 12L) {
    long <- which(n > w)
    disp$label[long] <- paste0(
      strtrim(disp$label[long], w - 1L),
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
# `code_book_type()` turns it into the reader's word. A labelled vector
# without value labels has no categories, and neither has one whose labels
# only name declared missing codes, under `user_na`: it is the numbers or
# the text it stores.
code_book_kind <- function(col, user_na) {
  cls <- class(col)
  if (is.ordered(col)) {
    "ordinal"
  } else if (is.factor(col)) {
    "categorical"
  } else if (inherits(col, "haven_labelled")) {
    labs <- attr(col, "labels", exact = TRUE)
    # The labels that name categories: under `user_na`, not those of the
    # declared missing codes.
    n_cat <- length(labs) - if (user_na) length(.user_na_labels(col)) else 0L
    if (n_cat > 0L) {
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
  } else if (inherits(col, "hms")) {
    "time"
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
    time = spicy_str("cell_type_time"),
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
  if (!kind %in% c("date", "datetime", "time") || all(is.na(col))) {
    return(c(NA_character_, NA_character_))
  }
  if (kind == "time") {
    # Not range(): it returns a difftime in seconds, not a time of day.
    v <- col[!is.na(col)]
    n <- as.double(v)
    return(format(v[c(which.min(n), which.max(n))]))
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


# The rows of `values` for one variable, and its declared missing values,
# for the header. Factors, labelled vectors and logicals list their
# categories, the first `values` of them in their order, then their
# declared and system missing values. Any other variable has rows only
# when it has declared missing values.
code_book_values <- function(col, name, kind, values, factor_levels, user_na) {
  out <- list(rows = NULL, declared = NULL)
  declared <- if (user_na) .user_na_mask(col) else logical(length(col))
  # Under factor_levels = "all", a declared code that no observation
  # carries is listed with a count of 0, as an unused level is: the codes
  # of its labels and of `na_values`, labelled or not, join the observed
  # declared values with a weight of 0.
  extra <- if (user_na && factor_levels == "all") {
    c(
      unname(unclass(.user_na_labels(col))),
      attr(col, "na_values", exact = TRUE)
    )
  }
  if (any(declared) || length(extra)) {
    probe <- structure(
      c(unclass(col)[declared], extra),
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
  # Past `values`, the first `values` categories stay, in their order, with
  # their counts and percentages of the whole variable; `n_categories`
  # says how many there are, so that the PDF can account for the rest.
  n_categories <- length(code)
  n_valid_cat <- sum(n)
  if (n_categories > values) {
    keep <- seq_len(values)
    code <- code[keep]
    label <- label[keep]
    n <- n[keep]
  }
  if (categorical) {
    out$n_categories <- n_categories
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
    pct_valid = c(pct(n, n_valid_cat), rep(NA_real_, n_dm + has_na))
  )
  out
}


# One row per declared missing value found in the data, with the
# variables that carry it. The rows go by code, then label, so that a
# code declared with two labels gives two adjacent rows: numeric codes by
# value, then the others (text codes, tagged NAs) as text.
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
  out <- tibble::tibble(
    code = d$code[first],
    label = d$label[first],
    variables = vapply(vars, paste, character(1), collapse = ", "),
    n_variables = lengths(vars)
  )
  value <- suppressWarnings(as.numeric(out$code))
  out[order(value, out$code, out$label, method = "radix"), ]
}


# How a labelled_spss vector declares its missing values, as text: its
# `na_values`, then its `na_range` from one end to the other. Tagged NAs
# are missing values, not a declaration. NA without a declaration, or
# under `user_na = FALSE`, which sets it aside.
code_book_declared_codes <- function(col, user_na) {
  if (!user_na || !inherits(col, "haven_labelled_spss")) {
    return(NA_character_)
  }
  values <- attr(col, "na_values", exact = TRUE)
  range <- attr(col, "na_range", exact = TRUE)
  # An open range reads "<= 9" or ">= 9000", a closed one "9000-9999".
  span <- if (length(range) && is.infinite(range[[1L]])) {
    paste0("\u2264 ", .format_code(range[[2L]]))
  } else if (length(range) && is.infinite(range[[2L]])) {
    paste0("\u2265 ", .format_code(range[[1L]]))
  } else if (length(range)) {
    paste(.format_code(range), collapse = "\u2013")
  }
  out <- c(
    if (length(values)) paste(.format_code(values), collapse = ", "),
    span
  )
  if (length(out)) {
    paste(out, collapse = spicy_str("sep_declared_codes"))
  } else {
    NA_character_
  }
}


# The header as field-value rows, for the print and the first Excel sheet.
# `key` is an internal token: nothing branches on the displayed field.
# The words of a note without the marks of its markup, which the PDF
# renders (the same three patterns as the template): the console and the
# Excel file show `*italics*`, `**bold**`, and `code` as plain words.
code_book_plain <- function(x) {
  x <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", x)
  x <- gsub("\\*([^*[:space:]][^*]*)\\*", "\\1", x)
  gsub("`([^`]+)`", "\\1", x)
}


code_book_info <- function(header, orcid = FALSE) {
  a <- header$authors
  author <- a$name
  aff <- nzchar(trimws(a$affiliation))
  author[aff] <- paste0(author[aff], " \u2013 ", a$affiliation[aff])
  if (orcid) {
    id <- nzchar(trimws(a$orcid))
    author[id] <- paste0(author[id], " \u2013 ORCID ", a$orcid[id])
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
    subtitle = if (is.na(header$subtitle)) character() else header$subtitle,
    author = author,
    date = format(header$date),
    observations = as.character(header$n_obs),
    variables = as.character(header$n_vars),
    declared = declared,
    note = code_book_plain(header$notes)
  )
  fields <- c(
    title = "row_title",
    subtitle = "row_subtitle",
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
    declared_codes = "header_declared_codes",
    n_distinct = "header_distinct",
    n_categories = "header_categories",
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
