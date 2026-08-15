# Parity of the Word (.docx) engine with the console, on the six
# documents the audit found defective:
#
#   * a multi-model table with `stars = TRUE` -- the legend was
#     written to the footer while no cell carried a star;
#   * `show_columns = "n_events"` -- the "events/N" composite lost its
#     denominator and gained coefficient decimals;
#   * the reference row of that same table -- its counts were replaced
#     by the en-dash reserved for "no estimate here";
#   * the survival estimands -- `rmst_p` / `risk_diff_p` rendered as
#     generic 2-decimal numbers, printing a p of .00098 as "0.00";
#   * the descriptive families -- the .docx opened with an untitled
#     table, and the association gloss never reached it;
#   * fixest's absorbed fixed effects -- the machine keys "FE: region"
#     leaked in place of the "Fixed effects:" block.
#
# Plus the indentation rule the tinytable engine already follows: a
# level row is indented once, by the engine, never twice.
#
# The oracle is the console rendering of the same object; the evidence
# is read back from the written file with `officer::docx_summary()`.

.wd_skip <- function() {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
}

.wd_write <- function(write_fun) {
  path <- tempfile(fileext = ".docx")
  write_fun(path)
  path
}

.wd_summary <- function(path) {
  officer::docx_summary(officer::read_docx(path))
}

# Table cells as a list of character vectors, one per rendered row.
.wd_rows <- function(path) {
  s <- .wd_summary(path)
  cells <- s[s$content_type == "table cell", ]
  lapply(split(cells, cells$row_id), function(r) r$text[order(r$cell_id)])
}

# Numeric cells are padded for decimal alignment with U+2007 FIGURE
# SPACE, which `trimws()` does not treat as whitespace.
.wd_trim <- function(x) {
  pad <- paste0("[ \t", intToUtf8(c(0x2007L, 0x00A0L)), "]+")
  gsub(paste0("^", pad, "|", pad, "$"), "", x)
}


# The row whose first cell (trimmed) is `label`.
.wd_row <- function(rows, label) {
  hit <- Filter(function(r) identical(.wd_trim(r[1L]), label), rows)
  expect_length(hit, 1L)
  hit[[1L]]
}

# Last row of the table = the note flextable writes into the footer.
.wd_note <- function(rows) {
  utils::tail(rows, 1L)[[1L]][1L]
}

.wd_caption_styles <- c("Table Caption", "table caption")

.wd_caption <- function(path) {
  p <- .wd_summary(path)
  paste(p$text[p$style_name %in% .wd_caption_styles], collapse = " ")
}

.wd_data <- function() as.data.frame(sochealth)

.wd_ndash <- "–"


# ---- (a) stars ------------------------------------------------------------

test_that("stars reach the Word cells that the footer legend documents", {
  .wd_skip()
  d <- .wd_data()
  m1 <- stats::lm(wellbeing_score ~ age + sex, data = d)
  m2 <- stats::lm(wellbeing_score ~ age + sex + income, data = d)
  path <- .wd_write(function(p) {
    table_regression(
      list(m1, m2),
      decimal_mark = ",",
      stars = TRUE,
      output = "word",
      word_path = p
    )
  })
  rows <- .wd_rows(path)

  # Console oracle: "(Intercept) | 65,07***" and "Male | 3,90***".
  expect_equal(.wd_trim(.wd_row(rows, "(Intercept)")[2L]), "65,07***")
  expect_equal(.wd_trim(.wd_row(rows, "Male")[2L]), "3,90***")
  # A non-significant coefficient carries no star.
  expect_false(grepl("*", .wd_row(rows, "age")[2L], fixed = TRUE))
  # The legend the audit found orphaned now documents symbols that are
  # in the table -- and its thresholds follow the table's decimal mark
  # (they used to stay dot-based in a comma table).
  expect_match(.wd_note(rows), "*** p < ,001", fixed = TRUE)
})


# ---- (a) events / N -------------------------------------------------------

test_that("events/N survives the Word engine, reference rows included", {
  .wd_skip()
  d <- .wd_data()
  g1 <- stats::glm(
    physical_activity ~ sex + smoking,
    data = d,
    family = stats::binomial()
  )
  g2 <- stats::glm(
    dentist_12m ~ sex + smoking,
    data = d,
    family = stats::binomial()
  )
  path <- .wd_write(function(p) {
    table_regression(
      list(g1, g2),
      show_columns = c("b", "n_events", "p"),
      output = "word",
      word_path = p
    )
  })
  rows <- .wd_rows(path)

  # Composite string, integer counts, both models (console oracle:
  # "536/1175" and "833/1175" on the intercept row).
  intercept <- .wd_row(rows, "(Intercept)")
  expect_equal(.wd_trim(intercept[3L]), "536/1175")
  expect_equal(.wd_trim(intercept[6L]), "833/1175")
  # Never the numerator alone, never coefficient decimals.
  expect_false(any(grepl("536.00", unlist(rows), fixed = TRUE)))

  # The reference row: en-dash where there is no estimate, counts
  # where there are counts (console: "Female (ref.) | - | 279/606 | -").
  ref <- .wd_row(rows, "Female (ref.)")
  expect_equal(.wd_trim(ref[2L]), .wd_ndash)
  expect_equal(.wd_trim(ref[3L]), "279/606")
  expect_equal(.wd_trim(ref[4L]), .wd_ndash)
  expect_equal(.wd_trim(ref[6L]), "430/606")
})


# ---- (f) survival estimands ----------------------------------------------

test_that("survival-estimand p columns keep the p style in Word", {
  .wd_skip()
  skip_if_not_installed("survival")
  lung <- survival::lung
  lung$female <- as.integer(lung$sex == 2)
  fit <- survival::coxph(
    survival::Surv(time, status) ~ age + female + ph.ecog,
    data = lung,
    x = TRUE
  )
  cols <- c("b", "rmst", "rmst_ci", "rmst_p", "risk_diff", "risk_diff_p")

  # The contract itself: a column whose source field is the p-value is
  # a p column, whichever estimate block it belongs to.
  set.seed(20260813)
  tab <- table_regression(
    fit,
    show_columns = cols,
    tau = 365,
    at_time = 300,
    boot_n = 30
  )
  meta <- as_structured(tab)$col_meta
  for (nm in c("p", "p.2")) {
    expect_identical(meta[[nm]]$source_field, "p_value")
    expect_identical(meta[[nm]]$p_style, "apa")
    expect_identical(meta[[nm]]$precision, 3L)
    expect_equal(meta[[nm]]$threshold, 0.001)
  }
  expect_identical(meta[["p"]]$token, "rmst_p")
  expect_identical(meta[["p.2"]]$token, "risk_diff_p")

  set.seed(20260813)
  path <- .wd_write(function(p) {
    table_regression(
      fit,
      show_columns = cols,
      tau = 365,
      at_time = 300,
      boot_n = 30,
      output = "word",
      word_path = p
    )
  })
  rows <- .wd_rows(path)
  female <- .wd_row(rows, "female")
  # Both estimand p columns in APA style (no leading zero, p_digits
  # decimals, "<.001" below threshold) -- never the 2-decimal "0.00"
  # the audit read as a p-value of exactly zero.
  expect_equal(.wd_trim(female[6L]), "<.001")
  expect_equal(.wd_trim(female[8L]), "<.001")
  age <- .wd_row(rows, "age")
  for (j in c(6L, 8L)) {
    expect_match(.wd_trim(age[j]), "^(<\\.001|\\.[0-9]{3})$")
  }
})


# ---- (b) fixest fixed-effects block ---------------------------------------

test_that("the absorbed fixed-effects block reaches Word as a block", {
  .wd_skip()
  skip_if_not_installed("fixest")
  d <- .wd_data()
  f1 <- fixest::feols(wellbeing_score ~ age + sex | region, data = d)
  f2 <- fixest::feols(
    wellbeing_score ~ age + sex | region + employment_status,
    data = d
  )
  path <- .wd_write(function(p) {
    table_regression(
      list(f1, f2),
      title = "Two-way fixed effects",
      output = "word",
      word_path = p
    )
  })
  rows <- .wd_rows(path)
  first_cells <- .wd_trim(vapply(rows, function(r) r[1L], character(1)))

  # The gloss row the console prints, and bare factor names under it.
  expect_true("Fixed effects:" %in% first_cells)
  expect_true("region" %in% first_cells)
  expect_true("employment_status" %in% first_cells)
  # The machine keys never leak.
  expect_false(any(grepl("FE: ", first_cells, fixed = TRUE)))
  # Yes / No disclosure per model.
  expect_equal(.wd_trim(.wd_row(rows, "employment_status")[2L]), "No")
  expect_equal(.wd_trim(.wd_row(rows, "employment_status")[5L]), "Yes")
})


# ---- level-row indentation ------------------------------------------------

test_that("a level row is indented once in Word -- by the engine", {
  .wd_skip()
  d <- .wd_data()
  fit <- stats::lm(wellbeing_score ~ age + sex, data = d)
  path <- .wd_write(function(p) {
    table_regression(fit, output = "word", word_path = p)
  })
  rows <- .wd_rows(path)
  first_cells <- vapply(rows, function(r) r[1L], character(1))
  level_cells <- first_cells[
    .wd_trim(first_cells) %in% c("Female (ref.)", "Male")
  ]
  expect_length(level_cells, 2L)
  # No leading whitespace in the cell text: Word keeps literal spaces,
  # so the console indent plus the engine's own padding would read as
  # two indentation steps.
  expect_false(any(grepl("^[[:space:]]", level_cells)))

  # The engine's own indent is still there -- it is the one that
  # survives every backend.
  ft <- table_regression(fit, output = "flextable")
  labels <- ft$body$dataset[[1L]]
  pad <- ft$body$styles$pars$padding.left$data[, 1L]
  level_rows <- which(labels %in% c("Female (ref.)", "Male"))
  expect_length(level_rows, 2L)
  expect_true(all(pad[level_rows] == 20))
  expect_true(all(pad[-level_rows] < 20))
  expect_false(any(grepl("^[[:space:]]", labels)))
})


# ---- (e) descriptive families: title + gloss ------------------------------

test_that("descriptive tables carry their console title as the Word caption", {
  .wd_skip()
  d <- .wd_data()

  cases <- list(
    list(
      title = "Categorical table by sex",
      write = function(p) {
        table_categorical(
          d,
          select = "smoking",
          by = "sex",
          output = "word",
          word_path = p
        )
      }
    ),
    list(
      title = "Categorical table",
      write = function(p) {
        table_categorical(d, select = "smoking", output = "word", word_path = p)
      }
    ),
    list(
      title = "Descriptive statistics",
      write = function(p) {
        table_continuous(
          d,
          select = c("age", "bmi"),
          output = "word",
          word_path = p
        )
      }
    ),
    list(
      title = "Descriptive statistics",
      write = function(p) {
        table_continuous(
          d,
          select = c("age", "bmi"),
          by = "sex",
          output = "word",
          word_path = p
        )
      }
    ),
    list(
      title = "Continuous outcomes by Sex",
      write = function(p) {
        table_continuous_lm(
          d,
          select = c("bmi", "age"),
          by = "sex",
          output = "word",
          word_path = p
        )
      }
    )
  )

  for (case in cases) {
    path <- .wd_write(case$write)
    expect_match(.wd_caption(path), case$title, fixed = TRUE)
    # APA auto-numbering, as on the regression path: a Word SEQ field
    # rather than a number frozen at write time.
    expect_match(.wd_caption(path), "SEQ tab", fixed = TRUE)
    # And the caption is written above the table.
    s <- .wd_summary(path)
    cap_idx <- min(s$doc_index[s$style_name %in% .wd_caption_styles])
    tbl_idx <- min(s$doc_index[s$content_type == "table cell"])
    expect_lt(cap_idx, tbl_idx)
  }
})

test_that("the regression Word caption keeps its APA auto-number", {
  .wd_skip()
  fit <- stats::lm(wellbeing_score ~ age, data = .wd_data())
  path <- .wd_write(function(p) {
    table_regression(
      fit,
      title = "Wellbeing model",
      output = "word",
      word_path = p
    )
  })
  expect_match(.wd_caption(path), "Wellbeing model", fixed = TRUE)
  expect_match(.wd_caption(path), "SEQ tab", fixed = TRUE)
})

test_that("the association gloss reaches the Word document", {
  .wd_skip()
  d <- .wd_data()
  path <- .wd_write(function(p) {
    table_categorical(
      d,
      select = c("smoking", "education"),
      by = "sex",
      output = "word",
      word_path = p
    )
  })
  # Console oracle: "Note. Phi: Current smoker; Cramer's V: Highest
  # education level."
  note <- .wd_note(.wd_rows(path))
  expect_match(note, "Phi: Current smoker", fixed = TRUE)
  expect_match(note, "Highest education level", fixed = TRUE)
})

test_that("a missing-value disclosure and an association gloss share the note", {
  .wd_skip()
  d <- .wd_data()
  path <- .wd_write(function(p) {
    table_categorical(
      d,
      select = c("smoking", "education"),
      by = "sex",
      drop_na = TRUE,
      output = "word",
      word_path = p
    )
  })
  note <- .wd_note(.wd_rows(path))
  expect_match(note, "Missing values removed", fixed = TRUE)
  expect_match(note, "Phi: Current smoker", fixed = TRUE)
})


# ---- the caption helper itself --------------------------------------------

test_that(".spicy_ft_word_caption leaves a table without a title alone", {
  .wd_skip()
  ft <- flextable::flextable(data.frame(a = 1))
  expect_identical(spicy:::.spicy_ft_word_caption(ft, NULL), ft)
  expect_identical(spicy:::.spicy_ft_word_caption(ft, ""), ft)
})

test_that(".spicy_ft_word_caption reports a missing 'officer'", {
  .wd_skip()
  ft <- flextable::flextable(data.frame(a = 1))
  local_mocked_bindings(spicy_pkg_available = function(pkg) FALSE)
  expect_error(
    spicy:::.spicy_ft_word_caption(ft, "A title"),
    class = "spicy_missing_pkg"
  )
})
