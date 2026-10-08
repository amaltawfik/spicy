# The checks of code_book()'s arguments. Each takes the `call` its error
# names, code_book() by default: the frame that calls it.
validate_code_book_title <- function(
  title,
  arg = "title",
  call = rlang::caller_env()
) {
  if (is.null(title)) {
    return(invisible(title))
  }

  if (
    !is.character(title) ||
      length(title) != 1L ||
      is.na(title) ||
      !nzchar(trimws(title))
  ) {
    spicy_abort(
      paste0(
        "`",
        arg,
        "` must be NULL or a single non-empty character string."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  invisible(title)
}


validate_code_book_control_dots <- function(dots, call = rlang::caller_env()) {
  # `rlang::enquos()` names every dot, with "" for the unnamed ones.
  dot_names <- names(dots) %||% character()

  # Arguments of the former widget. They now reach `...`, where tidyselect
  # would read them as a renamed selection; name the replacement instead.
  removed <- c(
    filename = paste(
      "`filename` was removed: `code_book()` writes the file itself,",
      "with `output = \"<path>.xlsx\"`."
    ),
    include_na = paste(
      "`include_na` was removed: missing values are always counted, in",
      "the `n_missing` column of `variables`, and `values` gives an NA",
      "row to each variable it lists that has missing values."
    )
  )
  hit <- intersect(dot_names, names(removed))
  if (length(hit) > 0L) {
    spicy_abort(
      removed[[hit[[1L]]]],
      class = c("spicy_defunct", "spicy_invalid_input"),
      call = call
    )
  }

  named_idx <- which(nzchar(dot_names))

  if (length(named_idx) == 0L) {
    return(invisible(dots))
  }

  # Derived from `formals(code_book)` rather than hardcoded, so that a new
  # control argument of `code_book()` is picked up here automatically.
  # Excludes `x` (positional data argument) and `...` (the tidyselect dots
  # whose contents we are validating).
  controls <- setdiff(names(formals(code_book)), c("x", "..."))
  named_dots <- dot_names[named_idx]
  partial_controls <- vapply(
    named_dots,
    function(nm) any(startsWith(controls, nm)) && !nm %in% controls,
    logical(1)
  )
  literal_values <- vapply(
    dots[named_idx],
    function(quo) {
      expr <- rlang::quo_get_expr(quo)
      is.null(expr) || (is.atomic(expr) && length(expr) == 1L)
    },
    logical(1)
  )
  suspect_idx <- which(partial_controls & literal_values)

  if (length(suspect_idx) > 0L) {
    arg <- named_dots[[suspect_idx[[1L]]]]
    option <- controls[startsWith(controls, arg)][[1L]]
    spicy_abort(
      paste0(
        "`",
        arg,
        "` was supplied through `...`. ",
        "Use `",
        option,
        " = ...` exactly for this `code_book()` option; ",
        "`...` is reserved for tidyselect column selectors."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }

  invisible(dots)
}


# `authors`, on the contract of lssdoc's argument of the same name: NULL, a
# character vector (names are the authors, values their affiliations; an
# unnamed element is a name alone), or a list of lists with `name` and the
# optional `affiliation` and `orcid`. Returns one row per author, with ""
# for a field that was not supplied.
code_book_authors <- function(authors, call = rlang::caller_env()) {
  fail <- function() {
    spicy_abort(
      c(
        "`authors` must be NULL, a character vector, or a list of lists with a `name`.",
        "i" = paste(
          "Use `c(\"Jane Doe\" = \"Affiliation\")`, or",
          "`list(list(name = \"Jane Doe\", affiliation = \"...\", orcid = \"...\"))`."
        )
      ),
      class = c("spicy_bad_authors", "spicy_invalid_input"),
      call = call
    )
  }
  # The character form becomes the list form, checked by the same code.
  if (is.character(authors)) {
    nm <- names(authors) %||% rep("", length(authors))
    authors <- Map(
      function(n, v) {
        if (is.na(n) || !nzchar(n)) {
          list(name = v)
        } else {
          list(name = n, affiliation = v)
        }
      },
      nm,
      unname(authors)
    )
  }
  if (!is.null(authors) && !is.list(authors)) {
    fail()
  }
  field <- function(a, f) {
    v <- a[[f]]
    if (is.null(v)) {
      return("")
    }
    if (length(v) != 1L || is.na(v)) {
      fail()
    }
    # A blank field is an empty one, and a padded ORCID a clean link.
    trimws(as.character(v))
  }
  rows <- lapply(authors, function(a) {
    if (!is.list(a) || !nzchar(trimws(field(a, "name")))) {
      fail()
    }
    out <- vapply(c("name", "affiliation", "orcid"), field, character(1), a = a)
    # An ORCID given as its address is kept as the identifier alone: the
    # PDF writes the address from it.
    out[["orcid"]] <- sub(
      "^https?://(www[.])?orcid[.]org/",
      "",
      out[["orcid"]],
      ignore.case = TRUE
    )
    out
  })
  rows <- unname(rows)
  tibble::tibble(
    name = vapply(rows, `[[`, character(1), "name"),
    affiliation = vapply(rows, `[[`, character(1), "affiliation"),
    orcid = vapply(rows, `[[`, character(1), "orcid")
  )
}


validate_code_book_notes <- function(notes, call = rlang::caller_env()) {
  if (!is.null(notes) && (!is.character(notes) || anyNA(notes))) {
    spicy_abort(
      "`notes` must be NULL or a character vector, one note per element.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(notes)
}


validate_code_book_values <- function(values, call = rlang::caller_env()) {
  # The released `values = TRUE / FALSE` switched the list of values on
  # and off; a logical would otherwise read as the count 1 or 0.
  if (isTRUE(values) || isFALSE(values)) {
    spicy_abort(
      c(
        "`values` is now a count: the maximum number of categories listed per variable.",
        "i" = "`values = Inf` lists every category, and the default lists up to 100."
      ),
      class = c("spicy_defunct", "spicy_invalid_input"),
      call = call
    )
  }
  if (
    !is.numeric(values) ||
      length(values) != 1L ||
      is.na(values) ||
      values < 0 ||
      (is.finite(values) && values != round(values))
  ) {
    spicy_abort(
      c(
        "`values` must be a single non-negative whole number: the maximum number of categories listed per variable.",
        "i" = "`values = Inf` lists every category."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(values)
}


# `source` maps current column names to the codes they had in the source
# file: the vector `dplyr::rename(all_of())` takes.
validate_code_book_source <- function(
  source,
  columns,
  call = rlang::caller_env()
) {
  if (is.null(source)) {
    return(invisible(source))
  }
  nms <- names(source)
  if (
    !is.character(source) ||
      is.null(nms) ||
      anyNA(nms) ||
      !all(nzchar(nms)) ||
      anyDuplicated(nms) > 0L
  ) {
    spicy_abort(
      "`source` must be a named character vector: `c(<column> = \"<code in the source file>\")`.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  blank <- nms[!nzchar(trimws(source))]
  if (length(blank) > 0L) {
    spicy_abort(
      c(
        "`source` must give each column a non-empty code.",
        "x" = paste0(
          "No code for: ",
          paste(.quote_val(blank), collapse = ", "),
          "."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  unknown <- setdiff(nms, columns)
  if (length(unknown) > 0L) {
    spicy_abort(
      c(
        "`source` names columns that are not in the codebook.",
        "x" = paste0(
          "Not selected: ",
          paste(.quote_val(unknown), collapse = ", "),
          "."
        )
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  invisible(source)
}


# An argument you type > a style (`options(spicy.style)`, code_book() has
# no `style` argument) > the language's locale > ".". Any single
# character, as in the table families and in `spicy_style()`, so the
# mark a style stores is one the argument accepts too.
code_book_decimal_mark <- function(decimal_mark, call = rlang::caller_env()) {
  if (is.null(decimal_mark)) {
    style <- .style_resolve(NULL)
    return(
      style$decimal_mark %||% .style_locale_defaults()$decimal_mark %||% "."
    )
  }
  if (!.is_single_char(decimal_mark)) {
    spicy_abort(
      "`decimal_mark` must be a single character (e.g. \".\" or \",\").",
      class = "spicy_invalid_input",
      call = call
    )
  }
  decimal_mark
}


# The look of the PDF: the fonts, the palette of lssdoc with `colors`
# merged over it, and the paper. Typst embeds the two default fonts; a
# font the user names is checked by code_book_quarto(), for the PDF only.
code_book_appearance <- function(
  font,
  font_code,
  colors,
  paper,
  index_columns = NULL,
  call = rlang::caller_env()
) {
  fonts <- list(font = font, font_code = font_code)
  for (arg in names(fonts)) {
    f <- fonts[[arg]]
    if (!is.null(f) && !(rlang::is_string(f) && nzchar(f))) {
      spicy_abort(
        paste0("`", arg, "` must be NULL or a single font name."),
        class = "spicy_invalid_input",
        call = call
      )
    }
  }
  palette <- c(
    primary = "#133B52",
    accent = "#3A7C8C",
    band = "#E9F2F6",
    band_dark = "#1F4E5F",
    zebra = "#F4F8FA",
    grid = "#D3DCE2",
    text = "#222222",
    muted = "#6E6E6E"
  )
  if (!is.null(colors)) {
    nms <- names(colors)
    if (
      !is.character(colors) ||
        is.null(nms) ||
        !all(nms %in% names(palette)) ||
        anyDuplicated(nms) > 0L ||
        !all(grepl("^#[0-9A-Fa-f]{6}$", colors))
    ) {
      spicy_abort(
        c(
          "`colors` must be a named character vector of \"#RRGGBB\" colors.",
          "i" = paste0("Names: ", paste(names(palette), collapse = ", "), ".")
        ),
        class = "spicy_invalid_input",
        call = call
      )
    }
    palette[nms] <- colors
    # A custom band or band_dark brings its own zebra and grid, unless
    # those are given too: zebra is band halfway to white, grid band_dark
    # a fifth of the way from white. The default palette gives back
    # #F4F8FA and #D2DCDF.
    mix <- function(col, w) {
      v <- strtoi(substring(col, c(2L, 4L, 6L), c(3L, 5L, 7L)), 16L)
      rgb <- as.integer(round(w * v + (1 - w) * 255))
      paste0("#", paste(sprintf("%02X", rgb), collapse = ""))
    }
    if ("band" %in% nms && !"zebra" %in% nms) {
      palette[["zebra"]] <- mix(palette[["band"]], 0.5)
    }
    if ("band_dark" %in% nms && !"grid" %in% nms) {
      palette[["grid"]] <- mix(palette[["band_dark"]], 0.2)
    }
  }
  paper <- tryCatch(
    match.arg(paper, c("a4", "letter")),
    error = function(e) {
      spicy_abort(
        "`paper` must be \"a4\" or \"letter\".",
        class = "spicy_invalid_input",
        call = call
      )
    }
  )
  if (
    !is.null(index_columns) &&
      !(is.numeric(index_columns) &&
        length(index_columns) == 1L &&
        !is.na(index_columns) &&
        index_columns %in% c(1, 2))
  ) {
    spicy_abort(
      "`index_columns` must be `NULL`, `1`, or `2`.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  list(
    font = font %||% "New Computer Modern",
    font_code = font_code %||% "DejaVu Sans Mono",
    colors = palette,
    paper = paper,
    index_columns = if (!is.null(index_columns)) as.integer(index_columns)
  )
}


# The file format `output` asks for, read off its extension.
code_book_output_format <- function(output, call = rlang::caller_env()) {
  if (is.null(output)) {
    return(NULL)
  }
  if (
    !is.character(output) ||
      length(output) != 1L ||
      is.na(output) ||
      !nzchar(trimws(output))
  ) {
    spicy_abort(
      "`output` must be NULL or a single file path.",
      class = "spicy_invalid_input",
      call = call
    )
  }
  ext <- tolower(tools::file_ext(output))
  if (!ext %in% c("xlsx", "pdf", "typ")) {
    spicy_abort(
      c(
        "`output` must be a path ending in \".xlsx\", \".pdf\", or \".typ\".",
        "x" = paste0("Got ", .quote_val(output), "."),
        "i" = "For a CSV, write `cb$variables` or `cb$values` with `utils::write.csv()`."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (!dir.exists(dirname(output))) {
    spicy_abort(
      paste0(
        "The directory of `output` does not exist: ",
        .quote_val(dirname(output)),
        "."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (dir.exists(output)) {
    spicy_abort(
      paste0(
        "`output` names a directory, not a file: ",
        .quote_val(output),
        "."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  if (ext == "xlsx" && !spicy_pkg_available("openxlsx2")) {
    spicy_abort(
      c(
        "Writing a codebook to \".xlsx\" requires the 'openxlsx2' package.",
        "i" = "Install it with `install.packages(\"openxlsx2\")`."
      ),
      class = "spicy_missing_pkg",
      call = call
    )
  }
  ext
}


# Quarto, found by the quarto package, compiles the PDF with the Typst it
# bundles, which must be Typst 0.12 or later (sticky blocks, paragraph
# spacing): Quarto 1.7. A font the user names must be one Typst finds,
# spelled as `quarto typst fonts` lists it: Typst would otherwise
# substitute another, silently.
code_book_quarto <- function(fonts, call = rlang::caller_env()) {
  alt <- paste(
    "Or write the Typst source with `output = \"<path>.typ\"`",
    "and compile it with `typst compile`."
  )
  if (!spicy_pkg_available("quarto")) {
    spicy_abort(
      c(
        "Writing a codebook to \".pdf\" requires the 'quarto' package.",
        "i" = "Install it with `install.packages(\"quarto\")`.",
        "i" = alt
      ),
      class = "spicy_missing_pkg",
      call = call
    )
  }
  quarto <- quarto::quarto_path()
  # A stale QUARTO_PATH names a file that is gone.
  found <- !is.null(quarto) && nzchar(quarto) && file.exists(quarto)
  version <- if (found) quarto::quarto_version()
  if (!found || version < "1.7") {
    spicy_abort(
      c(
        "Writing a codebook to \".pdf\" requires Quarto 1.7 or later.",
        "x" = if (found) {
          paste0("Found Quarto ", version, ".")
        } else {
          "Quarto was not found."
        },
        "i" = "Install it from <https://quarto.org>.",
        "i" = alt
      ),
      class = "spicy_missing_quarto",
      call = call
    )
  }
  unknown <- setdiff(fonts, if (length(fonts)) code_book_typst_fonts(quarto))
  if (length(unknown)) {
    spicy_abort(
      c(
        paste0("Typst finds no font named ", .quote_val(unknown[[1L]]), "."),
        "i" = "`quarto typst fonts` lists the fonts it finds: give the name exactly as listed."
      ),
      class = "spicy_invalid_input",
      call = call
    )
  }
  quarto
}
