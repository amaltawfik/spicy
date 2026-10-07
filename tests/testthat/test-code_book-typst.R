# Labels holding quotes, backslashes, Typst markup and a non-ASCII letter:
# the source must keep them as text. Code 9 is declared without a label.
cbt_data <- function() {
  data.frame(
    q = labelled::labelled_spss(
      c(1, 2, 8, 9, NA),
      labels = c("Yes \"really\"" = 1, "No \\ never" = 2, "#DK *_@<" = 8),
      na_values = c(8, 9),
      label = "Quote \" backslash \\ #hash *bold* _under_ @ref <lt> été"
    ),
    n = c(1.5, 2, NA, 4, 5.5),
    day = as.Date("2024-05-01") + 0:4
  )
}

skip_without_quarto <- function() {
  skip_on_cran()
  skip_if_not_installed("quarto")
  skip_if(is.null(quarto::quarto_path()), "Quarto is not installed")
  skip_if(quarto::quarto_version() < "1.7", "Quarto is older than 1.7")
}

# The pages of the PDF, counted as the PNG files, one per page, that Typst
# makes of the source `code_book()` writes: no PDF reader is needed.
cbt_pages <- function(...) {
  dir <- withr::local_tempdir()
  typ <- file.path(dir, "cb.typ")
  code_book(..., output = typ)
  png <- file.path(dir, "page-{p}.png")
  system2(
    quarto::quarto_path(),
    c("typst", "compile", shQuote(typ), shQuote(png), "--ppi", "10")
  )
  length(list.files(dir, "^page-.*[.]png$"))
}

test_that("the Typst source is the template, then the codebook as literals", {
  path <- withr::local_tempfile(fileext = ".typ")
  code_book(
    cbt_data(),
    title = "Survey \"2026\"",
    authors = list(
      list(
        name = "Jane Doe",
        affiliation = "HESAV",
        orcid = "0000-0002-1825-0097"
      )
    ),
    notes = "Coded with # and \\.",
    source = c(q = "Q1"),
    output = path
  )
  src <- readLines(path, encoding = "UTF-8")
  tpl <- readLines(system.file("typst", "codebook.typ", package = "spicy"))
  expect_identical(src[seq_along(tpl)], tpl)
  expect_snapshot(
    cat(src[-seq_along(tpl)], sep = "\n"),
    transform = function(x) {
      x <- sub("date: \"[0-9-]+\"", "date: \"<date>\"", x)
      sub("\"spicy [^\"]+\"", "\"spicy <version>, R <version>\"", x)
    }
  )
})

test_that("the running header and the cover say Codebook once", {
  data <- function(...) {
    code_book_typst_data(code_book(data.frame(x = 1:2), ...))$data
  }
  a <- data(title = "Social health survey")
  expect_identical(a$header, "Codebook — Social health survey")
  expect_identical(a$subtitle, "Codebook")
  b <- data(title = "The CODEBOOK of 2026")
  expect_identical(b$header, "The CODEBOOK of 2026")
  expect_null(b$subtitle)
  expect_identical(data()$header, "Codebook")
  none <- data(title = NULL)
  expect_identical(none$header, "Codebook")
  expect_identical(none$subtitle, "Codebook")
  expect_true(is.na(none$title))
})

test_that("statistics: two decimals from 1, three significant digits below", {
  d <- data.frame(
    rate = c(-0.001, 0.0034, 0.0051),
    z = as.vector(scale(c(0.1, 0.2, 0.7))),
    zero = c(-0, -0, -0)
  )
  stats <- function(...) {
    vars <- code_book_typst_data(code_book(d, ...))$data$vars
    lapply(vars, \(x) unname(unlist(x$stats)))
  }
  # min, max, mean, sd, median. The mean of a z-score is noise around
  # zero, and zero is never signed.
  s <- stats()
  expect_identical(s[[1]], c("-0.001", "0.0051", "0.0025", "0.00315", "0.0034"))
  expect_identical(s[[2]][3:4], c("0.00", "1.00"))
  expect_identical(s[[3]], c("0", "0", "0.00", "0.00", "0"))
  expect_identical(stats(decimal_mark = ",")[[1]][[4]], "0,00315")
  expect_identical(typst_literal("a\tb"), "\"a b\"")
})

test_that("colors merge over the palette, and paper sets the page", {
  d <- data.frame(x = 1)
  look <- attr(code_book(d, colors = c(band = "#112233")), "appearance")
  expect_length(look$colors, 8L)
  expect_identical(
    look$colors[c("primary", "band")],
    c(primary = "#133B52", band = "#112233")
  )
  expect_identical(
    look[c("font", "font_code", "paper")],
    list(
      font = "New Computer Modern",
      font_code = "DejaVu Sans Mono",
      paper = "a4"
    )
  )
  letter <- code_book(d, paper = "letter")
  expect_identical(attr(letter, "appearance")$paper, "letter")
  expect_identical(code_book_typst_data(letter)$data$paper, "us-letter")

  bad <- list(
    "#112233",
    c(bandd = "#112233"),
    c(band = "#123"),
    c(band = "red"),
    c(band = NA),
    list(band = "#112233"),
    c(band = "#112233", band = "#445566")
  )
  for (b in bad) {
    expect_error(code_book(d, colors = b), class = "spicy_invalid_input")
  }
  expect_error(code_book(d, paper = "A3"), class = "spicy_invalid_input")
  for (f in list(1, NA_character_, "", c("a", "b"))) {
    expect_error(code_book(d, font = f), class = "spicy_invalid_input")
    expect_error(code_book(d, font_code = f), class = "spicy_invalid_input")
  }
})

test_that("a PDF needs the quarto package and Quarto 1.7", {
  pdf <- file.path(tempdir(), "cb-missing.pdf")
  with_mocked_bindings(
    expect_error(
      code_book(cbt_data(), output = pdf),
      class = "spicy_missing_pkg"
    ),
    spicy_pkg_available = function(pkg) FALSE
  )
  skip_if_not_installed("quarto")
  # No Quarto, then a stale QUARTO_PATH, then a Quarto too old.
  bin <- withr::local_tempfile(lines = "")
  for (found in list(NULL, file.path(tempdir(), "no-quarto"), bin)) {
    local_mocked_bindings(
      quarto_path = function(...) found,
      quarto_version = function() numeric_version("1.6.42"),
      .package = "quarto"
    )
    expect_error(
      code_book(cbt_data(), output = pdf),
      if (identical(found, bin)) "Found Quarto 1.6.42." else "not found",
      fixed = TRUE,
      class = "spicy_missing_quarto"
    )
  }
  expect_false(file.exists(pdf))
})

test_that("the fonts of the PDF must be ones Typst lists, exactly", {
  skip_if_not_installed("quarto")
  bin <- withr::local_tempfile(lines = "")
  local_mocked_bindings(
    quarto_path = function(...) bin,
    quarto_version = function() numeric_version("1.7"),
    .package = "quarto"
  )
  local_mocked_bindings(
    code_book_typst_fonts = function(quarto) c("Libertinus Serif", "Arial")
  )
  pdf <- file.path(tempdir(), "cb-fonts.pdf")
  for (f in list(list(font = "libertinus serif"), list(font_code = "Nope"))) {
    expect_error(
      do.call(code_book, c(list(cbt_data(), output = pdf), f)),
      class = "spicy_invalid_input"
    )
  }
  expect_false(file.exists(pdf))
  # A source compiled elsewhere keeps the font as given.
  typ <- withr::local_tempfile(fileext = ".typ")
  code_book(cbt_data(), font = "Nope", output = typ)
  expect_true(any(grepl("font: \"Nope\"", readLines(typ), fixed = TRUE)))
})

test_that("the PDF compiles with the Typst that Quarto bundles", {
  skip_without_quarto()
  pdf <- withr::local_tempfile(fileext = ".pdf")
  res <- withVisible(code_book(
    sochealth,
    authors = c("Jane Doe" = "HESAV"),
    notes = "Fictitious data.",
    output = pdf
  ))
  expect_false(res$visible)
  expect_s3_class(res$value, "spicy_codebook")
  expect_gt(file.size(pdf), 0)
  # Cover, the page about the data, list, nine pages of sheets, index.
  expect_identical(cbt_pages(sochealth), 13L)

  two <- withr::local_tempfile(fileext = ".pdf")
  code_book(
    cbt_data(),
    q,
    n,
    font = "Libertinus Serif",
    font_code = "DejaVu Sans Mono",
    paper = "letter",
    output = two
  )
  expect_gt(file.size(two), 0)
  # Cover, about, list, the two sheets on one page, index.
  expect_identical(
    cbt_pages(cbt_data(), q, n, font = "Libertinus Serif", paper = "letter"),
    5L
  )

  local_mocked_bindings(code_book_typst_source = function(cb) "#let x = (")
  err <- expect_error(
    code_book(cbt_data(), output = two),
    class = "spicy_typst_failed"
  )
  expect_gt(length(err$stderr), 0)
  # The temporary source is gone: the message names the way to inspect it.
  expect_no_match(conditionMessage(err), "file[0-9a-f]+[.]typ")
  expect_match(conditionMessage(err), "output = \"<path>.typ\"", fixed = TRUE)
})

test_that("a sheet taller than a page breaks across pages", {
  skip_without_quarto()
  item <- paste(
    "the school nurse should coordinate the health promotion activities",
    "of the whole school, with parents and teachers"
  )
  labels <- stats::setNames(1:20, sprintf("Item %02d - %s", 1:20, item))
  d <- data.frame(q = labelled::labelled(1:20, labels = labels), n = 1:20)
  # Cover, about, list, the sheet of q on pages 4 and 5, then the sheet of
  # n, index. Kept whole, the sheet of q overflowed its page, losing its
  # last rows, and pushed n to a page of its own.
  expect_identical(cbt_pages(d), 6L)
})
