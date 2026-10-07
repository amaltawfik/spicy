validate_code_book_title <- function(title) {
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
      "`title` must be NULL or a single non-empty character string.",
      class = "spicy_invalid_input"
    )
  }

  invisible(title)
}


validate_code_book_control_dots <- function(dots) {
  dot_names <- names(dots)

  if (is.null(dot_names)) {
    return(invisible(dots))
  }

  dot_names[is.na(dot_names)] <- ""

  # Arguments of the former widget. They now reach `...`, where tidyselect
  # would read them as a renamed selection; name the replacement instead.
  removed <- c(
    filename = paste(
      "`filename` was removed: `code_book()` writes the file itself,",
      "with `output = \"<path>.xlsx\"`."
    ),
    include_na = paste(
      "`include_na` was removed: missing values are always counted,",
      "in the `n_missing` column of `variables` and in an NA row of",
      "`values`."
    )
  )
  hit <- intersect(dot_names, names(removed))
  if (length(hit) > 0L) {
    spicy_abort(
      removed[[hit[[1L]]]],
      class = c("spicy_defunct", "spicy_invalid_input")
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
      class = "spicy_invalid_input"
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
    as.character(v)
  }
  rows <- lapply(authors, function(a) {
    if (!is.list(a) || !nzchar(trimws(field(a, "name")))) {
      fail()
    }
    vapply(c("name", "affiliation", "orcid"), field, character(1), a = a)
  })
  rows <- unname(rows)
  tibble::tibble(
    name = vapply(rows, `[[`, character(1), "name"),
    affiliation = vapply(rows, `[[`, character(1), "affiliation"),
    orcid = vapply(rows, `[[`, character(1), "orcid")
  )
}


validate_code_book_notes <- function(notes) {
  if (!is.null(notes) && (!is.character(notes) || anyNA(notes))) {
    spicy_abort(
      "`notes` must be NULL or a character vector, one note per element.",
      class = "spicy_invalid_input"
    )
  }
  invisible(notes)
}


validate_code_book_values <- function(values) {
  # The released `values = TRUE / FALSE` switched the list of values on
  # and off; a logical would otherwise read as the count 1 or 0.
  if (isTRUE(values) || isFALSE(values)) {
    spicy_abort(
      c(
        "`values` is now a count: the maximum number of categories listed per variable.",
        "i" = "`values = Inf` lists every category, and the default lists up to 100."
      ),
      class = c("spicy_defunct", "spicy_invalid_input")
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
      class = "spicy_invalid_input"
    )
  }
  invisible(values)
}


# `source` maps current column names to the codes they had in the source
# file: the vector `dplyr::rename(all_of())` takes.
validate_code_book_source <- function(source, columns) {
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
      class = "spicy_invalid_input"
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
      class = "spicy_invalid_input"
    )
  }
  invisible(source)
}


# An argument you type > a style (`options(spicy.style)`, code_book() has
# no `style` argument) > the language's locale > ".". Any single
# character, as in the table families and in `spicy_style()`, so the
# mark a style stores is one the argument accepts too.
code_book_decimal_mark <- function(decimal_mark) {
  if (is.null(decimal_mark)) {
    style <- .style_resolve(NULL)
    return(
      style$decimal_mark %||% .style_locale_defaults()$decimal_mark %||% "."
    )
  }
  if (!.is_single_char(decimal_mark)) {
    spicy_abort(
      "`decimal_mark` must be a single character (e.g. \".\" or \",\").",
      class = "spicy_invalid_input"
    )
  }
  decimal_mark
}


# The file format `output` asks for, read off its extension.
code_book_output_format <- function(output) {
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
      class = "spicy_invalid_input"
    )
  }
  ext <- tolower(tools::file_ext(output))
  if (identical(ext, "xlsx")) {
    if (!dir.exists(dirname(output))) {
      spicy_abort(
        paste0(
          "The directory of `output` does not exist: ",
          .quote_val(dirname(output)),
          "."
        ),
        class = "spicy_invalid_input"
      )
    }
    if (!spicy_pkg_available("openxlsx2")) {
      spicy_abort(
        c(
          "Writing a codebook to \".xlsx\" requires the 'openxlsx2' package.",
          "i" = "Install it with `install.packages(\"openxlsx2\")`."
        ),
        class = "spicy_missing_pkg"
      )
    }
    return("xlsx")
  }
  if (identical(ext, "pdf")) {
    spicy_abort(
      c(
        "A PDF codebook is not available yet.",
        "i" = "Write an Excel file with `output = \"<path>.xlsx\"`."
      ),
      class = "spicy_unsupported"
    )
  }
  spicy_abort(
    c(
      "`output` must be a path ending in \".xlsx\".",
      "x" = paste0("Got ", .quote_val(output), "."),
      "i" = "For a CSV, write `cb$variables` or `cb$values` with `utils::write.csv()`."
    ),
    class = "spicy_invalid_input"
  )
}
