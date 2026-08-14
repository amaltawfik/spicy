# Parity of the Excel (.xlsx) engine with the console, on the cells the
# audit found defective:
#
#   * `show_columns = "n_events"` -- the "events/N" composite was
#     written as a bare numerator, and the reference row's counts were
#     overwritten by the en-dash reserved for "no estimate here";
#   * `stars = TRUE` -- the workbook carried the star legend in its
#     note while no cell carried a star;
#   * fixest's absorbed fixed effects -- the block was written as the
#     raw 1 / 0 of the typed body, in the same column as the
#     coefficients, instead of the Yes / No every other engine shows;
#   * `decimal_mark = ","` -- numeric cells follow the VIEWER's locale,
#     so a sheet mixed "65.07" with its own "<,001";
#   * the descriptive families -- variable-header rows were written as
#     Excel ERROR cells ("#N/A"), and neither the title nor the
#     missing-values / association notes reached the sheet;
#   * every family -- no column widths, so the row labels opened
#     clipped.
#
# The oracle is the console rendering of the same object; the evidence
# is read back from the written file with openxlsx2 (cell values, cell
# TYPES, and the sheet's <cols> widths). No binary fixture is stored.

.xl_skip <- function() {
  skip_if_not_installed("openxlsx2")
}

.xl_write <- function(write_fun) {
  path <- tempfile(fileext = ".xlsx")
  write_fun(path)
  path
}

# Sheet as a character matrix whose row names are the Excel row
# numbers and column names the Excel column letters.
.xl_grid <- function(path) {
  wb <- openxlsx2::wb_load(path)
  d <- openxlsx2::wb_to_df(wb, col_names = FALSE)
  m <- as.matrix(d)
  storage.mode(m) <- "character"
  m
}

.xl_at <- function(m, row, col) {
  unname(m[as.character(row), col])
}

# The row of the grid whose first column (trimmed) is `label`.
.xl_row <- function(m, label) {
  hit <- which(trimws(m[, "A"]) %in% label)
  expect_length(hit, 1L)
  m[hit, ]
}

# Cell type as the file stores it: "number", "text" (inline string) or
# "error" (the "#N/A" the audit found in the descriptive sheets).
.xl_type <- function(path, ref) {
  cc <- openxlsx2::wb_load(path)$worksheets[[1L]]$sheet_data$cc
  t <- cc$c_t[cc$r == ref]
  if (length(t) == 0L) {
    return(NA_character_)
  }
  switch(t[1L], "inlineStr" = "text", "e" = "error", "str" = "text", "number")
}

.xl_types <- function(path, refs) {
  vapply(refs, function(r) .xl_type(path, r), character(1), USE.NAMES = FALSE)
}

# The number-format code a cell carries ("0.00", "#.000", ...), read
# through the style chain the file stores: cell -> cellXfs -> numFmts.
.xl_numfmt <- function(path, ref) {
  wb <- openxlsx2::wb_load(path)
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  s <- cc$c_s[cc$r == ref]
  if (length(s) == 0L || !nzchar(s[1L])) {
    return(NA_character_)
  }
  xf <- wb$styles_mgr$styles$cellXfs[[as.integer(s[1L]) + 1L]]
  id <- sub('.*numFmtId="([0-9]+)".*', "\\1", xf)
  fmts <- wb$styles_mgr$styles$numFmts
  hit <- grep(paste0('numFmtId="', id, '"'), fmts, fixed = FALSE, value = TRUE)
  if (length(hit) == 0L) {
    return(NA_character_)
  }
  sub('.*formatCode="([^"]*)".*', "\\1", hit[1L])
}

# The `indent` steps a cell's alignment declares (0 when none).
.xl_indent <- function(path, refs) {
  wb <- openxlsx2::wb_load(path)
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  vapply(
    refs,
    function(ref) {
      s <- cc$c_s[cc$r == ref]
      if (length(s) == 0L || !nzchar(s[1L])) {
        return(0L)
      }
      xf <- wb$styles_mgr$styles$cellXfs[[as.integer(s[1L]) + 1L]]
      if (!grepl('indent="', xf, fixed = TRUE)) {
        return(0L)
      }
      as.integer(sub('.*indent="([0-9]+)".*', "\\1", xf))
    },
    integer(1),
    USE.NAMES = FALSE
  )
}

# The bottom-border weight a cell declares ("thin", "hair", or NA),
# read through the style chain: cell -> cellXfs -> borders.
.xl_bottom_border <- function(path, ref) {
  wb <- openxlsx2::wb_load(path)
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  s <- cc$c_s[cc$r == ref]
  if (length(s) == 0L || !nzchar(s[1L])) {
    return(NA_character_)
  }
  xf <- wb$styles_mgr$styles$cellXfs[[as.integer(s[1L]) + 1L]]
  id <- as.integer(sub('.*borderId="([0-9]+)".*', "\\1", xf))
  b <- wb$styles_mgr$styles$borders[[id + 1L]]
  if (!grepl("<bottom style=", b, fixed = TRUE)) {
    return(NA_character_)
  }
  sub('.*<bottom style="([a-z]+)".*', "\\1", b)
}

# Every cell the sheet stores as an Excel ERROR cell.
.xl_error_cells <- function(path) {
  cc <- openxlsx2::wb_load(path)$worksheets[[1L]]$sheet_data$cc
  cc$r[cc$c_t == "e"]
}

# Column widths declared in the sheet's <cols> element, one per
# column (NA where the sheet declares none).
.xl_widths <- function(path) {
  attrs <- openxlsx2::wb_load(path)$worksheets[[1L]]$cols_attr
  if (length(attrs) == 0L) {
    return(numeric(0))
  }
  out <- numeric(0)
  for (a in attrs) {
    lo <- as.integer(sub('.*min="([0-9]+)".*', "\\1", a))
    hi <- as.integer(sub('.*max="([0-9]+)".*', "\\1", a))
    w <- as.numeric(sub('.*width="([0-9.]+)".*', "\\1", a))
    out[lo:hi] <- w
  }
  out
}

.xl_data <- function() as.data.frame(sochealth)

.xl_ndash <- "–"


# ---- (a) n_events: the composite, and the reference row's counts ---------

test_that("the events/N composite reaches the Excel cells intact", {
  .xl_skip()
  d <- .xl_data()
  fit <- glm(smoking ~ age + sex, data = d, family = binomial)
  cols <- c("n_events", "b", "p")
  tab <- table_regression(fit, show_columns = cols)
  path <- .xl_write(function(p) {
    table_regression(fit, show_columns = cols, output = "excel", excel_path = p)
  })
  m <- .xl_grid(path)

  # Console oracle: both the numerator AND the denominator, on every
  # row, reference level included.
  expect_equal(trimws(tab[["Events/N"]][1L]), "249/1175")
  expect_equal(.xl_at(m, 4L, "B"), "249/1175")
  ref <- .xl_row(m, "Female (ref.)")
  expect_equal(unname(ref[["B"]]), "131/606")
  # The en-dash still governs the columns that DO carry an estimate.
  expect_equal(unname(ref[["C"]]), .xl_ndash)
  expect_equal(unname(ref[["D"]]), .xl_ndash)
  # A composite is text; the plain count of the "n" fit-stat row stays
  # a number.
  expect_equal(
    .xl_types(path, c("B4", "B7", "B9")),
    c("text", "text", "number")
  )
})


# ---- (b) stars ------------------------------------------------------------

test_that("stars reach the Excel cells that the note's legend documents", {
  .xl_skip()
  d <- .xl_data()
  fit <- lm(wellbeing_score ~ age + sex + education, data = d)
  path <- .xl_write(function(p) {
    table_regression(fit, stars = TRUE, output = "excel", excel_path = p)
  })
  m <- .xl_grid(path)

  expect_equal(.xl_at(m, 5L, "B"), "64.63***")
  expect_equal(unname(.xl_row(m, "Male")[["B"]]), "3.65***")
  # Cells with no marker keep their full numeric value (the number
  # format renders them at the requested precision).
  expect_equal(round(as.numeric(.xl_at(m, 6L, "B")), 4), 0.0258)
  expect_equal(.xl_types(path, c("B5", "B6")), c("text", "number"))
  # The legend the workbook already carried now documents markers that
  # exist in it.
  legend <- grep("\\*\\*\\* p", m[, "A"], value = TRUE)
  expect_length(legend, 1L)
})

test_that("stars = FALSE leaves every estimate cell a number", {
  .xl_skip()
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  path <- .xl_write(function(p) {
    table_regression(fit, output = "excel", excel_path = p)
  })
  expect_equal(.xl_types(path, c("B5", "B6", "B7")), rep("number", 3L))
})


# ---- (c) fixest fixed-effects block ---------------------------------------

test_that("the absorbed fixed-effects block reaches Excel as Yes / No", {
  .xl_skip()
  skip_if_not_installed("fixest")
  d <- .xl_data()
  f1 <- fixest::feols(wellbeing_score ~ age + sex | region, data = d)
  f2 <- fixest::feols(
    wellbeing_score ~ age + sex | region + education,
    data = d
  )
  path <- .xl_write(function(p) {
    table_regression(list(f1, f2), output = "excel", excel_path = p)
  })
  m <- .xl_grid(path)

  # Block header, then one indented row per absorbed factor, named
  # verbatim -- no "FE: region" machine key.
  head_i <- which(trimws(m[, "A"]) == "Fixed effects:")
  expect_length(head_i, 1L)
  region <- .xl_row(m, "region")
  education <- .xl_row(m, "education")
  expect_equal(unname(region[c("B", "E")]), c("Yes", "Yes"))
  expect_equal(unname(education[c("B", "E")]), c("No", "Yes"))
  # The disclosure is text, never the 1 / 0 of the typed body -- a
  # bare "1" in a coefficient column reads like an estimate.
  fe_rows <- head_i + 1:2
  expect_equal(
    .xl_types(path, paste0("B", fe_rows)),
    c("text", "text")
  )
  # The block's rows are indented like a factor's levels -- ONCE, by
  # the engine's own indent style, so the label text is unpadded.
  expect_equal(unname(m[fe_rows, "A"]), c("region", "education"))
  expect_true(all(.xl_indent(path, paste0("A", fe_rows)) >= 1L))
})

test_that("factor levels are indented once in the Excel stub column", {
  .xl_skip()
  d <- .xl_data()
  fit <- lm(wellbeing_score ~ age + sex, data = d)
  path <- .xl_write(function(p) {
    table_regression(fit, output = "excel", excel_path = p)
  })
  m <- .xl_grid(path)
  lvl <- which(m[, "A"] %in% c("Female (ref.)", "Male"))
  expect_length(lvl, 2L)
  # No leading spaces in the text ...
  expect_false(any(grepl("^\\s", m[lvl, "A"])))
  # ... and the engine's indent on those cells only.
  expect_true(all(.xl_indent(path, paste0("A", lvl)) >= 1L))
  expect_equal(.xl_indent(path, "A5"), 0L) # (Intercept)
})


# ---- (d) decimal_mark -----------------------------------------------------

test_that("decimal_mark = ',' holds in every Excel cell of the body", {
  .xl_skip()
  d <- .xl_data()
  fit <- lm(wellbeing_score ~ age + sex, data = d)
  path <- .xl_write(function(p) {
    table_regression(
      fit,
      decimal_mark = ",",
      output = "excel",
      excel_path = p
    )
  })
  m <- .xl_grid(path)

  # A numeric cell would follow the VIEWER's locale separator, which
  # the file cannot set: the body goes out pre-formatted instead.
  expect_equal(.xl_at(m, 5L, "B"), "65,07")
  expect_equal(.xl_at(m, 5L, "C"), "1,63")
  expect_equal(.xl_at(m, 5L, "F"), "<,001")
  expect_equal(.xl_at(m, 6L, "B"), "0,04")
  expect_equal(
    .xl_types(path, c("B5", "C5", "F5", "B10")),
    rep("text", 4L)
  )
})

test_that("the default decimal mark keeps the body numeric", {
  .xl_skip()
  d <- .xl_data()
  fit <- lm(wellbeing_score ~ age + sex, data = d)
  path <- .xl_write(function(p) {
    table_regression(fit, output = "excel", excel_path = p)
  })
  expect_equal(
    .xl_types(path, c("B5", "C5", "B10")),
    rep("number", 3L)
  )
  # Full precision stored, display precision in the number format.
  m <- .xl_grid(path)
  expect_gt(nchar(.xl_at(m, 5L, "B")), 6L)
})


# ---- (e) survival estimands (contract, pinned here for Excel) -------------

test_that("survival-estimand p columns keep the p style in Excel", {
  .xl_skip()
  skip_if_not_installed("survival")
  lung <- survival::lung
  lung$female <- as.integer(lung$sex == 2)
  fit <- survival::coxph(
    survival::Surv(time, status) ~ age + female + ph.ecog,
    data = lung,
    x = TRUE
  )
  cols <- c("b", "rmst", "rmst_p", "risk_diff", "risk_diff_p")
  set.seed(20260813)
  path <- .xl_write(function(p) {
    table_regression(
      fit,
      show_columns = cols,
      tau = 365,
      at_time = 300,
      boot_n = 30,
      output = "excel",
      excel_path = p
    )
  })
  m <- .xl_grid(path)
  female <- .xl_row(m, "female")
  # APA p style with the "<.001" threshold, in both estimand blocks --
  # never the generic 2-decimal "0.00" the audit read as p = 0.
  expect_equal(unname(female[["D"]]), "<.001")
  expect_equal(unname(female[["F"]]), "<.001")
  # Above the threshold the cell stays a number, and it is the p
  # NUMBER FORMAT that renders it in APA style (3 decimals, no
  # leading zero) -- the coefficient format "0.00" would print a p of
  # .0009 as "0.00".
  age_row <- which(trimws(m[, "A"]) == "age")
  for (j in c("D", "F")) {
    expect_equal(.xl_numfmt(path, paste0(j, age_row)), "#.000")
  }
})


# ---- (f) descriptive families: header rows, title, notes ------------------

test_that("table_categorical variable-header cells are blank, not #N/A", {
  .xl_skip()
  d <- .xl_data()
  path <- .xl_write(function(p) {
    table_categorical(
      d,
      c(self_rated_health, smoking),
      drop_na = TRUE,
      output = "excel",
      excel_path = p
    )
  })
  expect_length(.xl_error_cells(path), 0L)
  m <- .xl_grid(path)
  hdr <- .xl_row(m, "Self-rated health")
  expect_true(all(is.na(hdr[c("B", "C")])))
})

test_that("table_categorical writes its console title and disclosure note", {
  .xl_skip()
  d <- .xl_data()
  obj <- table_categorical(d, c(self_rated_health, smoking), drop_na = TRUE)
  path <- .xl_write(function(p) {
    table_categorical(
      d,
      c(self_rated_health, smoking),
      drop_na = TRUE,
      output = "excel",
      excel_path = p
    )
  })
  m <- .xl_grid(path)
  expect_equal(.xl_at(m, 1L, "A"), "Categorical table")
  # The table itself starts two rows below the title.
  expect_equal(.xl_at(m, 3L, "A"), "Variable")
  expect_true(attr(obj, "missing_note") %in% m[, "A"])
})

test_that("the by-table carries its title, missing note and assoc gloss", {
  .xl_skip()
  d <- .xl_data()
  obj <- table_categorical(
    d,
    c(self_rated_health, smoking),
    by = sex,
    drop_na = TRUE
  )
  path <- .xl_write(function(p) {
    table_categorical(
      d,
      c(self_rated_health, smoking),
      by = sex,
      drop_na = TRUE,
      output = "excel",
      excel_path = p
    )
  })
  expect_length(.xl_error_cells(path), 0L)
  m <- .xl_grid(path)
  expect_equal(.xl_at(m, 1L, "A"), "Categorical table by sex")
  expect_equal(.xl_at(m, 3L, "A"), "Variable")
  expect_equal(.xl_at(m, 4L, "B"), "n")
  col_a <- m[, "A"]
  expect_true(attr(obj, "missing_note") %in% col_a)
  expect_true(attr(obj, "assoc_note") %in% col_a)
  # Note lines sit below the body, in the reading order the console
  # prints them.
  i_missing <- which(col_a == attr(obj, "missing_note"))
  i_assoc <- which(col_a == attr(obj, "assoc_note"))
  expect_lt(i_missing, i_assoc)
})

test_that("table_continuous writes its console title and missing note", {
  .xl_skip()
  d <- .xl_data()
  obj <- table_continuous(d, c(bmi, wellbeing_score), by = sex)
  path <- .xl_write(function(p) {
    table_continuous(
      d,
      c(bmi, wellbeing_score),
      by = sex,
      output = "excel",
      excel_path = p
    )
  })
  expect_length(.xl_error_cells(path), 0L)
  m <- .xl_grid(path)
  # Decision 4 (2026-08-13): the by table states its grouping
  # variable, label resolved.
  expect_equal(.xl_at(m, 1L, "A"), "Descriptive statistics by Sex")
  expect_equal(.xl_at(m, 3L, "A"), "Variable")
  expect_equal(.xl_at(m, 4L, "G"), "LL")
  expect_true(attr(obj, "missing_note") %in% m[, "A"])
})

test_that("table_continuous_lm writes the title above its table", {
  .xl_skip()
  d <- .xl_data()
  path <- .xl_write(function(p) {
    table_continuous_lm(
      d,
      select = c(bmi, wellbeing_score),
      by = sex,
      output = "excel",
      excel_path = p
    )
  })
  m <- .xl_grid(path)
  expect_match(.xl_at(m, 1L, "A"), "^Continuous outcomes by ")
  expect_equal(.xl_at(m, 3L, "A"), "Variable")
  expect_equal(.xl_at(m, 5L, "A"), "Body mass index")
})


# ---- (g) column widths ----------------------------------------------------

test_that("every Excel family sizes its columns to the text they carry", {
  .xl_skip()
  d <- .xl_data()
  fit <- glm(smoking ~ age + sex, data = d, family = binomial)
  paths <- list(
    regression = .xl_write(function(p) {
      table_regression(fit, output = "excel", excel_path = p)
    }),
    categorical = .xl_write(function(p) {
      table_categorical(
        d,
        c(self_rated_health),
        output = "excel",
        excel_path = p
      )
    }),
    continuous = .xl_write(function(p) {
      table_continuous(
        d,
        c(bmi, wellbeing_score),
        output = "excel",
        excel_path = p
      )
    }),
    continuous_lm = .xl_write(function(p) {
      table_continuous_lm(
        d,
        select = c(bmi),
        by = sex,
        output = "excel",
        excel_path = p
      )
    })
  )
  for (nm in names(paths)) {
    w <- .xl_widths(paths[[nm]])
    expect_gt(length(w), 1L)
    # The stub column holds the row labels: wider than Excel's 8.43
    # default, and never wider than the cap.
    expect_gt(w[1L], 8.43)
    expect_lte(max(w), 60)
  }
  # The width follows the longest label of the TABLE (header + body).
  # The title and the note lines are full-width paragraphs written in
  # column A: measuring them would push the table off the screen.
  obj <- table_continuous(d, c(bmi, wellbeing_score))
  m <- .xl_grid(paths$continuous)
  table_rows <- as.character(3:(4L + nrow(obj))) # headers + body
  stub <- max(nchar(stats::na.omit(m[table_rows, "A"])))
  expect_equal(.xl_widths(paths$continuous)[1L], stub + 2, tolerance = 0.05)
})


# ---- (h) the width / note helpers -----------------------------------------

test_that(".spicy_xl_widths clamps to the default width and to the cap", {
  w <- .spicy_xl_widths(list(
    character(0),
    c("ab", NA_character_),
    strrep("x", 200L)
  ))
  expect_equal(w[[1L]], 8.43)
  expect_equal(w[[2L]], 8.43)
  expect_equal(w[[3L]], 60)
})

test_that(".spicy_xl_cells pairs each column with its header cells", {
  cells <- .spicy_xl_cells(
    data.frame(a = 1:2, b = c("x", "yy")),
    headers = list(c("A", "B"), c("", "sub"))
  )
  expect_equal(cells[[1L]], c("A", "", "1", "2"))
  expect_equal(cells[[2L]], c("B", "sub", "x", "yy"))
})

test_that(".spicy_xl_add_note writes one row per line and skips empties", {
  .xl_skip()
  wb <- openxlsx2::wb_workbook()
  wb <- openxlsx2::wb_add_worksheet(wb, "S")
  wb <- .spicy_xl_add_note(wb, note = NULL, start_row = 2L)
  wb <- .spicy_xl_add_note(wb, note = "", start_row = 2L)
  wb <- .spicy_xl_add_note(wb, note = "one\ntwo", start_row = 2L, sheet = "S")
  path <- tempfile(fileext = ".xlsx")
  openxlsx2::wb_save(wb, path, overwrite = TRUE)
  m <- .xl_grid(path)
  expect_equal(unname(m[, "A"]), c("one", "two"))
})


# ---- (h) block rules: every rule the console draws ------------------------

test_that("the sheet rules off each subordinate block, not just the fit stats", {
  .xl_skip()
  skip_if_not_installed("lme4")
  d <- .xl_data()
  fit <- suppressWarnings(
    lme4::lmer(wellbeing_score ~ age + sex + (1 | region), data = d)
  )
  path <- .xl_write(function(p) {
    suppressWarnings(table_regression(fit, output = "excel", excel_path = p))
  })
  m <- .xl_grid(path)
  rows <- as.integer(rownames(m))
  row_of <- function(label) rows[which(trimws(m[, "A"]) == label)]

  re <- row_of("Random effects:")
  n_row <- row_of("n")
  expect_length(re, 1L)
  expect_length(n_row, 1L)
  # The console rules ABOVE `Random effects:` and above the fit stats;
  # the sheet drew only the second one until section_sep_rows reached
  # the Excel writer.
  expect_identical(.xl_bottom_border(path, paste0("A", re - 1L)), "hair")
  expect_identical(.xl_bottom_border(path, paste0("C", re - 1L)), "hair")
  expect_identical(.xl_bottom_border(path, paste0("A", n_row - 1L)), "hair")
  # A coefficient row in the middle of a block carries no rule.
  age_row <- row_of("age")
  expect_true(is.na(.xl_bottom_border(path, paste0("A", age_row))))
})

test_that("a table with no subordinate block keeps its single fit-stats rule", {
  .xl_skip()
  d <- .xl_data()
  fit <- stats::lm(wellbeing_score ~ age + sex, data = d)
  path <- .xl_write(function(p) {
    table_regression(fit, output = "excel", excel_path = p)
  })
  m <- .xl_grid(path)
  rows <- as.integer(rownames(m))
  n_row <- rows[which(trimws(m[, "A"]) == "n")]
  expect_length(n_row, 1L)
  expect_identical(.xl_bottom_border(path, paste0("A", n_row - 1L)), "hair")
  hair <- vapply(
    paste0("A", rows),
    function(r) identical(.xl_bottom_border(path, r), "hair"),
    logical(1)
  )
  expect_identical(sum(hair), 1L)
})
