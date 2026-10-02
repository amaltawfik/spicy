empty_code_book_data <- function() {
  data.frame(
    Variable = character(),
    Label = character(),
    Values = character(),
    Class = character(),
    N_distinct = integer(),
    N_valid = integer(),
    NAs = integer()
  )
}


code_book_columns <- function() {
  c(
    "Variable",
    "Label",
    "Values",
    "Class",
    "N_distinct",
    "N_valid",
    "NAs"
  )
}


code_book_filenames <- function(cb) {
  vapply(
    cb$x$options$buttons[[3]]$buttons,
    function(button) button$filename,
    character(1)
  )
}


code_book_export_extends <- function(cb) {
  vapply(
    cb$x$options$buttons[[3]]$buttons,
    function(button) button$extend,
    character(1)
  )
}


test_that("code_book() runs without error on a simple data frame", {
  skip_if_not_installed("DT")

  df <- head(mtcars)

  expect_silent({
    cb <- suppressMessages(code_book(df))
  })

  expect_s3_class(cb, "datatables")
  expect_true(inherits(cb, "htmlwidget"))
  expect_named(cb$x$data, code_book_columns())
  expect_equal(cb$x$data$Variable, names(df))
  expect_equal(cb$x$options$dom, "Bfrtip")
  expect_equal(cb$x$options$pageLength, 10)
  expect_true(cb$x$options$colReorder)
  expect_true(cb$x$options$fixedHeader)
  expect_equal(
    unlist(cb$x$extensions, use.names = FALSE),
    c("Buttons", "ColReorder", "FixedHeader")
  )
})

test_that("code_book() works with values = TRUE", {
  skip_if_not_installed("DT")
  df <- data.frame(x = letters[1:6])

  cb <- suppressMessages(code_book(df, values = TRUE))

  expect_s3_class(cb, "datatables")
  expect_equal(cb$x$data$Values, "a, b, c, d, e, f")
})

test_that("code_book() works with include_na = TRUE", {
  skip_if_not_installed("DT")
  df <- data.frame(x = c(1, NA, 3), y = c("a", "b", NA))

  cb <- suppressMessages(code_book(df, include_na = TRUE))

  expect_s3_class(cb, "datatables")
  expect_match(cb$x$data$Values[[1]], "<NA>", fixed = TRUE)
  expect_match(cb$x$data$Values[[2]], "<NA>", fixed = TRUE)
})

test_that("code_book() selects and reorders variables like varlist()", {
  skip_if_not_installed("DT")
  df <- data.frame(a = 1:3, b = 4:6, c = letters[1:3])

  cb <- suppressMessages(code_book(df, b, a))
  expect_equal(cb$x$data$Variable, c("b", "a"))

  cb <- suppressMessages(code_book(df, where(is.character)))
  expect_equal(cb$x$data$Variable, "c")
})

test_that("code_book() returns the same data as varlist()", {
  skip_if_not_installed("DT")
  df <- data.frame(
    a = c(3, 1, NA),
    b = letters[1:3],
    c = factor(c("yes", "no", "yes"), levels = c("no", "yes", "missing"))
  )

  expected <- varlist(
    df,
    c,
    a,
    values = TRUE,
    include_na = TRUE,
    factor_levels = "all",
    tbl = TRUE
  )
  cb <- suppressMessages(code_book(
    df,
    c,
    a,
    values = TRUE,
    include_na = TRUE,
    factor_levels = "all"
  ))

  expect_equal(cb$x$data, as.data.frame(expected))
})

test_that("code_book() accepts custom title", {
  skip_if_not_installed("DT")

  cb <- suppressMessages(code_book(head(mtcars), title = "My Codebook"))

  expect_s3_class(cb, "datatables")
  expect_equal(cb$x$caption, "<caption>My Codebook</caption>")
  expect_equal(code_book_filenames(cb), rep("My_Codebook", 3))
})

test_that("code_book() configures export buttons consistently", {
  skip_if_not_installed("DT")

  cb <- suppressMessages(code_book(head(mtcars), title = "My Codebook"))
  buttons <- cb$x$options$buttons
  download_buttons <- buttons[[3]]$buttons

  expect_equal(buttons[[1]], "copy")
  expect_equal(buttons[[2]], "print")
  expect_equal(buttons[[3]]$extend, "collection")
  expect_equal(buttons[[3]]$text, "Download")
  expect_equal(code_book_export_extends(cb), c("csv", "excel", "pdf"))
  expect_true(all(vapply(
    download_buttons,
    function(button) is.null(button$title),
    logical(1)
  )))
  expect_equal(code_book_filenames(cb), rep("My_Codebook", 3))
})

test_that("code_book() accepts title = NULL", {
  skip_if_not_installed("DT")

  cb <- suppressMessages(code_book(head(mtcars), title = NULL))

  expect_s3_class(cb, "datatables")
  expect_null(cb$x$caption)
  expect_equal(cb$x$options$buttons[[3]]$buttons[[1]]$filename, "Codebook")
})

test_that("code_book() accepts explicit export filenames", {
  skip_if_not_installed("DT")

  cb <- suppressMessages(code_book(
    head(mtcars),
    title = "Codebook: BMI / smoking?",
    filename = "bmi smoking review"
  ))

  expect_equal(cb$x$caption, "<caption>Codebook: BMI / smoking?</caption>")
  expect_equal(code_book_filenames(cb), rep("bmi_smoking_review", 3))

  cb <- suppressMessages(code_book(
    head(mtcars),
    title = NULL,
    filename = "exports/final:codebook"
  ))

  expect_null(cb$x$caption)
  expect_equal(code_book_filenames(cb), rep("exports_final_codebook", 3))
})

test_that("code_book() sanitizes export filenames", {
  skip_if_not_installed("DT")

  cb <- suppressMessages(code_book(
    head(mtcars),
    title = "Codebook: BMI / smoking?"
  ))
  expect_equal(cb$x$caption, "<caption>Codebook: BMI / smoking?</caption>")
  expect_equal(code_book_filenames(cb), rep("Codebook_BMI_smoking", 3))

  cb <- suppressMessages(code_book(head(mtcars), title = "***"))
  expect_equal(code_book_filenames(cb), rep("Codebook", 3))

  cb <- suppressMessages(code_book(
    head(mtcars),
    title = "\u00c2ge & sant\u00e9"
  ))
  expect_equal(cb$x$caption, "<caption>\u00c2ge &amp; sant\u00e9</caption>")
  expect_equal(code_book_filenames(cb), rep("Age_sante", 3))

  cb <- suppressMessages(code_book(
    head(mtcars),
    title = "Cafe\u0301 creme brule\u0301e"
  ))
  expect_equal(code_book_filenames(cb), rep("Cafe_creme_brulee", 3))

  cb <- suppressMessages(code_book(
    head(mtcars),
    filename = "R\u00e9sum\u00e9 final"
  ))
  expect_equal(code_book_filenames(cb), rep("Resume_final", 3))
})

test_that("code_book_sanitize_filename preserves long names verbatim", {
  # Stata / SPSS convention: never silently truncate user-supplied
  # identifiers. Filenames that overflow the platform limit surface
  # as a noisy OS-level download error from the browser rather than
  # as a silently-truncated file.
  long <- paste(rep("a", 200L), collapse = "")
  out <- code_book_sanitize_filename(long, arg = "title", fallback = "Codebook")
  expect_equal(out, long)
})

# ---- the ASCII fold of export filenames (#8) ------------------------------
#
# The fold used to go through iconv(to = "ASCII//TRANSLIT"), which is
# implementation-defined: the same title gave "Age_sante" under glibc,
# "ge_sant" under musl (Alpine Linux), and Windows lost the sharp s. It
# now runs on code points against a table shipped in the package, so
# every expectation below holds on every platform and in every locale.
# None of them is guarded by a skip, and none of them should ever be.

.cb_name <- function(x, arg = "title", fallback = "Codebook") {
  code_book_sanitize_filename(x, arg = arg, fallback = fallback)
}

test_that("accented Latin letters fold to their base letter", {
  expect_identical(.cb_name("Âge & santé"), "Age_sante")
  expect_identical(.cb_name("Résumé final"), "Resume_final")
  expect_identical(
    .cb_name("Łódź, Dvořák"),
    "Lodz_Dvorak"
  )
  expect_identical(.cb_name("Việt Nam"), "Viet_Nam")
  expect_identical(
    .cb_name("İstanbul, ışık"),
    "Istanbul_isik"
  )
})

test_that("letters with no base letter get their ASCII spelling", {
  expect_identical(
    .cb_name("Straße und Größe"),
    "Strasse_und_Grosse"
  )
  expect_identical(.cb_name("Œuvre et cœur"), "OEuvre_et_coeur")
  expect_identical(
    .cb_name("Ångström æther"),
    "Angstrom_aether"
  )
  expect_identical(.cb_name("Þing ða"), "THing_da")
  expect_identical(.cb_name("ﬁn de série"), "fin_de_serie")
})

test_that("composed and decomposed spellings give the same name", {
  composed <- "Café crème brûlée"
  decomposed <- "Café crème brûlée"
  expect_false(identical(composed, decomposed))
  expect_identical(.cb_name(composed), "Cafe_creme_brulee")
  expect_identical(.cb_name(decomposed), "Cafe_creme_brulee")
})

test_that("typographic punctuation behaves like its ASCII counterpart", {
  # The curly apostrophe is removed like the straight one, a dash is
  # kept as a hyphen, a no-break space separates like a space.
  expect_identical(.cb_name("L’âge"), "Lage")
  expect_identical(.cb_name("L'âge"), "Lage")
  expect_identical(.cb_name("Santé – vague 2"), "Sante_-_vague_2")
  expect_identical(.cb_name("Santé - vague 2"), "Sante_-_vague_2")
  expect_identical(.cb_name("vague 2"), "vague_2")
  expect_identical(.cb_name("« Titre »"), "Titre")
})

test_that("invisible characters leave nothing behind", {
  # Soft hyphen, zero-width space, byte order mark: what text pasted
  # from a browser or a PDF carries along. None becomes visible.
  expect_identical(.cb_name("infor­mation"), "information")
  expect_identical(.cb_name("zéro​largeur"), "zerolargeur")
  expect_identical(.cb_name("﻿Codebook 2024"), "Codebook_2024")
})

test_that("a script the table does not cover is dropped, never guessed at", {
  cyrillic <- "Кодбук"
  expect_identical(.cb_name(cyrillic), "Codebook")
  expect_identical(.cb_name(paste(cyrillic, "BMI 2024")), "BMI_2024")
  expect_error(
    .cb_name(cyrillic, arg = "filename", fallback = NULL),
    class = "spicy_invalid_input"
  )
})

test_that("NA and invalid UTF-8 fall back like an empty name", {
  # The encoding is declared: an undeclared byte string means whatever
  # the session's native encoding says, so it would make this test, and
  # only this test, depend on the locale it runs in.
  invalid <- rawToChar(as.raw(c(0x41, 0xff, 0x42)))
  Encoding(invalid) <- "UTF-8"
  expect_identical(code_book_ascii_filename(NA_character_), NA_character_)
  expect_identical(code_book_ascii_filename(invalid), NA_character_)
  expect_identical(.cb_name(invalid), "Codebook")
  expect_identical(code_book_ascii_filename(""), "")
})

test_that("the fold gives the same names in a C locale", {
  titles <- c(
    "Âge & santé",
    "Straße",
    "Việt Nam",
    "L’âge – 2"
  )
  fold_all <- function() {
    vapply(titles, .cb_name, character(1), USE.NAMES = FALSE)
  }
  here <- fold_all()
  in_c <- withr::with_locale(c(LC_CTYPE = "C"), fold_all())
  expect_identical(here, c("Age_sante", "Strasse", "Viet_Nam", "Lage_-_2"))
  expect_identical(in_c, here)
})

test_that("the fold asks no system library for an opinion", {
  # A guard against going back to iconv(), to ICU at run time or to a
  # Unicode-aware regular expression: each of them makes the name depend
  # on the platform, and the first one is how #8 happened.
  src <- unlist(lapply(
    list(
      code_book_ascii_filename,
      code_book_is_dropped,
      code_book_sanitize_filename
    ),
    function(f) deparse(body(f))
  ))
  expect_false(any(grepl("iconv", src, fixed = TRUE)))
  expect_false(any(grepl("stri_", src, fixed = TRUE)))
  expect_false(any(grepl("\\p{", src, fixed = TRUE)))
})

test_that("the frozen fold table is well formed and untouched", {
  from <- code_book_fold_from
  to <- code_book_fold_to
  expect_identical(length(from), length(to))
  expect_false(anyNA(from))
  expect_identical(anyDuplicated(from), 0L)
  expect_true(all(from >= 128L))
  expect_false(any(code_book_is_dropped(from)))
  expect_true(all(nzchar(to)))
  expect_false(any(grepl("[^\\x01-\\x7F]", to, perl = TRUE)))

  # One anchor per kind of entry.
  at <- function(cp) to[match(cp, from)]
  expect_identical(at(0x00E9L), "e")
  expect_identical(at(0x00DFL), "ss")
  expect_identical(at(0x0152L), "OE")
  expect_identical(at(0x2013L), "-")
  expect_identical(at(0x2019L), "'")
  expect_identical(at(0xFB01L), "fi")

  # The signature of the table. A file name is an identifier in a user's
  # pipeline: these numbers change only when the table is regenerated on
  # purpose (data-raw/code_book_fold_table.R), in a release that says so.
  expect_identical(length(from), 1348L)
  expect_identical(sum(as.numeric(from)), 24195744)
  expect_identical(sum(nchar(to)), 1883L)
  expect_identical(sum(as.numeric(from) * nchar(to)), 38145028)
})

test_that("code_book_sanitize_filename: empty after sanitisation + NULL fallback errors", {
  # `???` is non-ASCII punctuation that gets stripped to nothing; with
  # `fallback = NULL`, the function must raise an actionable error
  # rather than returning an empty filename.
  expect_error(
    code_book_sanitize_filename("???", arg = "filename", fallback = NULL),
    class = "spicy_invalid_input"
  )
})

test_that("code_book() works with labelled data", {
  skip_if_not_installed("DT")
  skip_if_not_installed("labelled")
  df <- data.frame(x = labelled::labelled(1:3, labels = c(A = 1, B = 2, C = 3)))
  cb <- suppressMessages(code_book(df))
  expect_s3_class(cb, "datatables")
})

test_that("code_book() errors on non-data.frame", {
  expect_error(code_book(1:10), "`x` must be a data frame or tibble")
})

test_that("code_book() errors when DT is not available", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "DT") FALSE else TRUE,
    .package = "base"
  )
  expect_error(code_book(mtcars), "Package 'DT' is required")
})

test_that("code_book() passes arguments and selectors to varlist", {
  skip_if_not_installed("DT")
  captured <- list()

  local_mocked_bindings(
    varlist = function(x, ..., values, include_na, factor_levels, tbl) {
      captured <<- list(
        values = values,
        include_na = include_na,
        factor_levels = factor_levels,
        tbl = tbl,
        dots = as.list(substitute(list(...)))[-1]
      )
      empty_code_book_data()
    },
    .package = "spicy"
  )

  suppressMessages(code_book(
    mtcars,
    cyl,
    values = TRUE,
    include_na = TRUE,
    factor_levels = "observed"
  ))

  expect_true(captured$values)
  expect_true(captured$include_na)
  expect_equal(captured$factor_levels, "observed")
  expect_true(captured$tbl)
  expect_identical(captured$dots[[1]], quote(cyl))
})

test_that("code_book() validates factor_levels", {
  expect_error(
    code_book(mtcars, factor_levels = "bad"),
    '`factor_levels` must be "observed" or "all"'
  )
})

test_that("code_book() validates logical and title arguments", {
  expect_error(
    code_book(mtcars, values = "yes"),
    "`values` must be TRUE or FALSE"
  )
  expect_error(
    code_book(mtcars, include_na = NA),
    "`include_na` must be TRUE or FALSE"
  )
  expect_error(
    code_book(mtcars, title = NA_character_),
    "`title` must be NULL or a single non-empty character string"
  )
  expect_error(
    code_book(mtcars, title = ""),
    "`title` must be NULL or a single non-empty character string"
  )
})

test_that("code_book() validates filename arguments", {
  expect_error(
    code_book(mtcars, filename = NA_character_),
    "`filename` must be NULL or a single non-empty character string"
  )
  expect_error(
    code_book(mtcars, filename = ""),
    "`filename` must be NULL or a single non-empty character string"
  )
  expect_error(
    code_book(mtcars, filename = c("a", "b")),
    "`filename` must be NULL or a single non-empty character string"
  )
  expect_error(
    code_book(mtcars, filename = "***"),
    "`filename` must contain at least one letter"
  )
})

test_that("code_book() rejects option-like partial matches in dots", {
  expect_error(
    code_book(mtcars, value = TRUE),
    "`value` was supplied through `\\.\\.\\.`"
  )
  expect_error(
    code_book(mtcars, inc = TRUE),
    "`inc` was supplied through `\\.\\.\\.`"
  )
  expect_error(
    code_book(mtcars, tit = "x"),
    "`tit` was supplied through `\\.\\.\\.`"
  )
  expect_error(
    code_book(mtcars, fac = "observed"),
    "`fac` was supplied through `\\.\\.\\.`"
  )
  expect_error(
    code_book(mtcars, fil = "x"),
    "`fil` was supplied through `\\.\\.\\.`"
  )
})

test_that("code_book() rejects renamed selections", {
  skip_if_not_installed("DT")
  df <- data.frame(x = 1:3, y = 4:6)

  expect_error(
    code_book(df, selected = x),
    "`\\.\\.\\.` can select columns but cannot rename them"
  )
})

test_that("code_book() handles empty selections", {
  skip_if_not_installed("DT")
  df <- data.frame(x = 1:3)

  expect_warning(
    cb <- suppressMessages(code_book(df, starts_with("missing"))),
    "No columns selected"
  )

  expect_s3_class(cb, "datatables")
  expect_named(cb$x$data, code_book_columns())
  expect_equal(nrow(cb$x$data), 0L)
})

test_that("code_book() handles data frames with no columns", {
  skip_if_not_installed("DT")
  df <- data.frame()

  expect_warning(
    cb <- suppressMessages(code_book(df)),
    "No columns selected"
  )

  expect_s3_class(cb, "datatables")
  expect_named(cb$x$data, code_book_columns())
  expect_equal(nrow(cb$x$data), 0L)
  expect_equal(cb$x$caption, "<caption>Codebook</caption>")
  expect_equal(code_book_export_extends(cb), c("csv", "excel", "pdf"))
  expect_equal(code_book_filenames(cb), rep("Codebook", 3))
})

test_that("code_book() defaults to all factor levels", {
  skip_if_not_installed("DT")

  captured <- character()

  local_mocked_bindings(
    varlist = function(..., factor_levels, tbl) {
      captured <<- c(captured, factor_levels)
      empty_code_book_data()
    },
    .package = "spicy"
  )

  suppressMessages(code_book(mtcars))
  suppressMessages(code_book(mtcars, factor_levels = "observed"))

  expect_equal(captured, c("all", "observed"))
})

test_that("code_book() lets varlist() errors surface directly", {
  skip_if_not_installed("DT")

  local_mocked_bindings(
    varlist = function(...) stop("bad varlist", call. = FALSE),
    .package = "spicy"
  )

  expect_error(
    code_book(mtcars),
    "^bad varlist$"
  )
})

test_that("code_book() lets varlist() column name errors surface directly", {
  skip_if_not_installed("DT")
  df <- data.frame(x = 1:3, y = 4:6)
  names(df) <- c("x", "x")

  expect_error(
    code_book(df),
    "`x` must have unique column names"
  )
})

test_that("code_book() errors when varlist() does not return a data frame", {
  skip_if_not_installed("DT")

  local_mocked_bindings(
    varlist = function(...) list(x = 1),
    .package = "spicy"
  )

  expect_error(
    code_book(mtcars),
    "`varlist\\(\\)` did not return a data frame"
  )
})

test_that("code_book() HTML-escapes special characters in title", {
  skip_if_not_installed("DT")

  cb <- suppressMessages(code_book(
    head(mtcars),
    title = "<script>alert(1)</script>"
  ))

  expect_equal(
    cb$x$caption,
    "<caption>&lt;script&gt;alert(1)&lt;/script&gt;</caption>"
  )
  expect_false(grepl("<script>alert", cb$x$caption, fixed = TRUE))
})
