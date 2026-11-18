# The place of the sample size, `spicy_style(sample_size = )`.
#
# "note" (the default) is the output of the 0.14 tables: the note opens
# with N and, under `by`, the n of each group. "header" heads each group
# column with its n in the three families whose groups are columns
# (table_categorical(), table_categorical_svy(), table_continuous_lm()),
# in every output, and leaves "N = ..." alone in the note. "none" leaves
# the sentence out of every note that writes one. The machine outputs do
# not change.

nbsp <- "\u00A0"

# The header of a group under "header", with the no-break spaces of its n.
hdr <- function(label, n) {
  paste0(label, " (n", nbsp, "=", nbsp, n, ")")
}

# The rendered console text of a table, as one string.
console <- function(x) {
  paste(utils::capture.output(print(x)), collapse = "\n")
}

# A sochealth with three rows missing `sex`.
na_by_data <- function() {
  d <- sochealth
  d$sex[c(2, 9, 40)] <- NA
  d
}

H <- spicy_style(sample_size = "header")

# Every engine of a table function, as text: `build(output, ...)` calls
# it. Returns a named list of the header text each engine carries.
engine_texts <- function(build) {
  out <- list()
  tt <- build("tinytable")
  for (f in c("html", "latex", "typst", "markdown")) {
    out[[paste0("tinytable_", f)]] <- paste(
      tinytable::save_tt(tt, output = f),
      collapse = "\n"
    )
  }
  g <- build("gt")
  out$gt <- paste(
    c(
      unlist(g[["_boxhead"]]$column_label),
      unlist(g[["_spanners"]]$spanner_label)
    ),
    collapse = " | "
  )
  ch <- flextable::information_data_chunk(build("flextable"))
  out$flextable <- paste(ch$txt[ch$.part == "header"], collapse = " | ")
  docx <- withr::local_tempfile(fileext = ".docx")
  build("word", word_path = docx)
  out$word <- paste(
    officer::docx_summary(officer::read_docx(docx))$text,
    collapse = " | "
  )
  xlsx <- withr::local_tempfile(fileext = ".xlsx")
  build("excel", excel_path = xlsx)
  sheet <- as.matrix(openxlsx2::read_xlsx(xlsx, col_names = FALSE))
  out$excel <- paste(sheet[seq_len(min(4L, nrow(sheet))), ], collapse = " | ")
  clip <- NULL
  testthat::local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(x, ...) {
      clip <<- x
      invisible(NULL)
    },
    .package = "clipr"
  )
  suppressMessages(build("clipboard"))
  out$clipboard <- paste(clip, collapse = "\n")
  out
}

skip_engines <- function() {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("clipr")
}

# Each engine carries every header; Typst writes "=" as "\=".
expect_headers <- function(texts, headers) {
  for (e in names(texts)) {
    want <- if (identical(e, "tinytable_typst")) {
      gsub("=", "\\=", headers, fixed = TRUE)
    } else {
      headers
    }
    for (h in want) {
      expect_true(grepl(h, texts[[e]], fixed = TRUE), info = paste(e, h))
    }
  }
}


# ---- The lever ----------------------------------------------------------

test_that("spicy_style() takes sample_size and refuses any other value", {
  expect_identical(spicy_style(sample_size = "header")$sample_size, "header")
  expect_identical(spicy_style(sample_size = "none")$sample_size, "none")
  expect_null(spicy_style()$sample_size)
  expect_error(
    spicy_style(sample_size = "top"),
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy_style(sample_size = c("note", "none")),
    class = "spicy_invalid_input"
  )
  expect_error(spicy_style(sample_size = NA), class = "spicy_invalid_input")
  expect_snapshot(spicy_style(sample_size = "top"), error = TRUE)
  # A theme with the lever keeps its other rules, and says it was changed.
  st <- spicy_style("jama", sample_size = "header")
  expect_identical(st$sample_size, "header")
  expect_identical(st$note_label, "")
  expect_true("sample_size" %in% attr(st, "provenance")$overrides)
})

test_that("sample_size = \"note\" is the output of a table without it", {
  d <- na_by_data()
  N <- spicy_style(sample_size = "note")
  expect_identical(
    console(table_categorical(d, c(smoking), by = sex, style = N)),
    console(table_categorical(d, c(smoking), by = sex))
  )
  expect_identical(
    console(table_continuous_lm(d, c(bmi), by = sex, style = N)),
    console(table_continuous_lm(d, c(bmi), by = sex))
  )
  expect_identical(
    console(table_continuous(d, c(bmi), by = sex, style = N)),
    console(table_continuous(d, c(bmi), by = sex))
  )
})


# ---- table_categorical() ------------------------------------------------

test_that("table_categorical(): the three places in the console", {
  d <- na_by_data()
  for (v in c("note", "header", "none")) {
    expect_snapshot(
      print(table_categorical(
        d,
        c(smoking),
        by = sex,
        style = spicy_style(sample_size = v)
      ))
    )
  }
  x <- table_categorical(d, c(smoking), by = sex, style = H)
  out <- console(x)
  # The column of missing `by` and the Total column too; the note keeps N.
  for (h in c(hdr("Female", 619), hdr("Male", 578), hdr("(Missing)", 3))) {
    expect_true(grepl(h, out, fixed = TRUE), info = h)
  }
  expect_true(grepl(hdr("Total", 1200), out, fixed = TRUE))
  expect_identical(
    attr(x, "what_note"),
    paste(
      "N = 1200.",
      "Percentages are column percentages."
    )
  )
  none <- table_categorical(
    d,
    c(smoking),
    by = sex,
    style = spicy_style(sample_size = "none")
  )
  expect_false(grepl("N = ", attr(none, "note"), fixed = TRUE))
})

test_that("table_categorical(): drop_na, weights, no Total, no by", {
  d <- na_by_data()
  # Under drop_na = TRUE the rows missing `by` leave N and the headers.
  x <- table_categorical(d, c(smoking), by = sex, drop_na = TRUE, style = H)
  out <- console(x)
  expect_true(grepl(hdr("Total", 1197), out, fixed = TRUE))
  expect_false(grepl("(Missing) (n", out, fixed = TRUE))
  expect_match(attr(x, "note"), "^Note\\. N = 1197\\. ")
  # Under weights, rows: the n stay unweighted, as the note says.
  w <- table_categorical(d, c(smoking), by = sex, weights = weight, style = H)
  expect_true(grepl(hdr("Female", 619), console(w), fixed = TRUE))
  expect_match(attr(w, "note"), "^Note\\. N = 1200 \\(unweighted\\)\\. ")
  # No Total column: the groups alone.
  nt <- table_categorical(
    d,
    c(smoking),
    by = sex,
    include_total = FALSE,
    style = H
  )
  expect_false(grepl("Total (n", console(nt), fixed = TRUE))
  expect_true(grepl(hdr("Male", 578), console(nt), fixed = TRUE))
  # Without `by`, "header" is "note".
  expect_identical(
    console(table_categorical(d, c(smoking), style = H)),
    console(table_categorical(d, c(smoking)))
  )
  expect_null(attr(table_categorical(d, c(smoking), by = sex), "header_n"))
})

test_that("table_categorical(): every engine heads the groups with n", {
  skip_engines()
  d <- na_by_data()
  texts <- engine_texts(function(o, ...) {
    table_categorical(d, c(smoking), by = sex, style = H, output = o, ...)
  })
  expect_headers(
    texts,
    c(
      hdr("Female", 619),
      hdr("Male", 578),
      hdr("(Missing)", 3),
      hdr("Total", 1200)
    )
  )
})

test_that("table_categorical(): the machine outputs do not change", {
  d <- na_by_data()
  strip <- function(z) {
    attr(z, "note") <- NULL
    z
  }
  for (o in c("data.frame", "long")) {
    expect_identical(
      strip(table_categorical(d, c(smoking), by = sex, output = o, style = H)),
      strip(table_categorical(d, c(smoking), by = sex, output = o)),
      info = o
    )
  }
  x <- table_categorical(d, c(smoking), by = sex, style = H)
  x0 <- table_categorical(d, c(smoking), by = sex)
  expect_identical(strip(as.data.frame(x)), strip(as.data.frame(x0)))
  expect_identical(as_structured(x), as_structured(x0))
  skip_if_not_installed("broom")
  expect_identical(broom::tidy(x), broom::tidy(x0))
  expect_identical(broom::glance(x), broom::glance(x0))
})

test_that("table_categorical(): French headers and note", {
  withr::local_options(spicy.language = "fr")
  d <- na_by_data()
  x <- table_categorical(d, c(smoking), by = sex, style = H)
  out <- console(x)
  expect_true(grepl(hdr("(Manquant)", 3), out, fixed = TRUE))
  expect_true(grepl(hdr("Total", 1200), out, fixed = TRUE))
  expect_match(
    attr(x, "note"),
    "^Note\\. N = 1200\\. Pourcentages en colonne\\."
  )
})


# ---- table_continuous_lm() ----------------------------------------------

test_that("table_continuous_lm(): the three places, and the engines", {
  d <- na_by_data()
  for (v in c("note", "header", "none")) {
    expect_snapshot(
      print(table_continuous_lm(
        d,
        c(bmi),
        by = sex,
        style = spicy_style(sample_size = v)
      ))
    )
  }
  x <- table_continuous_lm(d, c(bmi), by = sex, style = H)
  x0 <- table_continuous_lm(d, c(bmi), by = sex)
  expect_match(attr(x, "note"), "^Note\\. N = 1197\\. ")
  # The machine outputs and the typed view keep the levels alone.
  strip <- function(z) {
    attr(z, "note") <- NULL
    z
  }
  expect_identical(strip(as.data.frame(x)), strip(as.data.frame(x0)))
  expect_identical(as_structured(x), as_structured(x0))
  # A numeric `by` has no groups: "header" is "note".
  expect_identical(
    console(table_continuous_lm(d, c(bmi), by = age, style = H)),
    console(table_continuous_lm(d, c(bmi), by = age))
  )
  skip_engines()
  texts <- engine_texts(function(o, ...) {
    table_continuous_lm(d, c(bmi), by = sex, style = H, output = o, ...)
  })
  expect_headers(texts, c(hdr("Female", 619), hdr("Male", 578)))
})


# ---- table_categorical_svy() --------------------------------------------

test_that("table_categorical_svy(): the three places, and the engines", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  api <- apiclus1
  api$stype[c(1, 5)] <- NA
  des <- survey::svydesign(id = ~dnum, weights = ~pw, data = api, fpc = ~fpc)
  for (v in c("note", "header", "none")) {
    expect_snapshot(
      print(table_categorical_svy(
        des,
        c(sch.wide),
        by = stype,
        drop_na = FALSE,
        style = spicy_style(sample_size = v)
      ))
    )
  }
  # The unweighted count of each domain, the rows of non-zero weight the
  # note's N counts, and the margin with N; the note keeps its sentence.
  x <- table_categorical_svy(
    des,
    c(sch.wide),
    by = stype,
    drop_na = TRUE,
    style = H
  )
  x0 <- table_categorical_svy(des, c(sch.wide), by = stype, drop_na = TRUE)
  expect_identical(attr(x, "note"), attr(x0, "note"))
  expect_identical(
    attr(x, "header_n"),
    c(E = 143L, H = 13L, M = 25L, Total = 181L)
  )
  expect_identical(as_structured(x), as_structured(x0))
  expect_identical(as.data.frame(x), as.data.frame(x0))
  none <- table_categorical_svy(
    des,
    c(sch.wide),
    by = stype,
    style = spicy_style(sample_size = "none")
  )
  expect_false(grepl("N = ", attr(none, "note"), fixed = TRUE))
  skip_engines()
  texts <- engine_texts(function(o, ...) {
    table_categorical_svy(
      des,
      c(sch.wide),
      by = stype,
      drop_na = TRUE,
      style = H,
      output = o,
      ...
    )
  })
  expect_headers(
    texts,
    c(hdr("E", 143), hdr("H", 13), hdr("M", 25), hdr("Total", 181))
  )
})

test_that("table_continuous_svy(): \"none\" leaves the sentence out", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  des <- survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1,
    fpc = ~fpc
  )
  n0 <- attr(table_continuous_svy(des, c(api00), by = stype), "note")
  n1 <- attr(
    table_continuous_svy(
      des,
      c(api00),
      by = stype,
      style = spicy_style(sample_size = "none")
    ),
    "note"
  )
  expect_match(n0, "^Note\\. N = 183 \\(weighted 6194\\)\\. ")
  expect_identical(n1, sub("N = 183 (weighted 6194). ", "", n0, fixed = TRUE))
  # "header": the groups are rows, the note is the note.
  expect_identical(
    attr(table_continuous_svy(des, c(api00), by = stype, style = H), "note"),
    n0
  )
})


# ---- The families whose groups are rows ---------------------------------

test_that("table_continuous() and table_outcome(): header is note, none drops", {
  d <- na_by_data()
  for (v in c("header", "none")) {
    st <- spicy_style(sample_size = v)
    nc <- attr(table_continuous(d, c(bmi), by = sex, style = st), "note")
    no <- attr(
      table_outcome(d, wellbeing_score, select = c(sex), style = st),
      "note"
    )
    nc0 <- attr(table_continuous(d, c(bmi), by = sex), "note")
    no0 <- attr(table_outcome(d, wellbeing_score, select = c(sex)), "note")
    if (v == "header") {
      expect_identical(nc, nc0)
      expect_identical(no, no0)
    } else {
      expect_identical(
        nc,
        sub("N = [^.]*\\. ", "", nc0)
      )
      expect_identical(no, sub("N = [^.]*\\. ", "", no0))
    }
  }
})

test_that("table_multiresponse(): its Base row is not the sentence", {
  d <- sochealth
  cols <- c("info_doctor", "info_pharmacist", "info_internet")
  expect_identical(
    console(table_multiresponse(
      d,
      dplyr::all_of(cols),
      style = spicy_style(sample_size = "none")
    )),
    console(table_multiresponse(d, dplyr::all_of(cols)))
  )
})

test_that("options(spicy.style) carries the lever to a print outside the call", {
  d <- na_by_data()
  x <- withr::with_options(
    list(spicy.style = H),
    table_categorical(d, c(smoking), by = sex)
  )
  expect_true(grepl(hdr("Total", 1200), console(x), fixed = TRUE))
})
