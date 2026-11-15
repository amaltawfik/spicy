# One rule for every Excel export: a body cell that shows one number in
# the console is a number in the sheet, which Excel shows as the console
# does; any other body cell is the console's text. The console's text is
# read on the clipboard payload of the same table (its cells are the
# console's, unpadded), row for row with the sheet; `clipr` is mocked.

.xl_inv_clip <- function(f) {
  captured <- new.env()
  local_mocked_bindings(
    clipr_available = function(...) TRUE,
    write_clip = function(content, ...) {
      captured$text <- content
      invisible(content)
    },
    .package = "clipr"
  )
  suppressMessages(f(output = "clipboard"))
  lines <- strsplit(captured$text, "\n", fixed = TRUE)[[1L]]
  lapply(lines[nzchar(lines)], function(l) {
    gsub('^"|"$', "", strsplit(l, "\t", fixed = TRUE)[[1L]])
  })
}

# The Excel number format code as a format of the emulator.
.xl_inv_spec <- function(code) {
  first <- sub(";.*$", "", sub('"[^"]*"', "", code))
  spicy:::.xl_spec(
    if (grepl(".", first, fixed = TRUE)) {
      nchar(sub("^.*\\.", "", first))
    } else {
      0L
    },
    !startsWith(sub("^[+-]", "", first), "#"),
    grepl(";", code, fixed = TRUE),
    if (grepl('"', code, fixed = TRUE)) sub('^[^"]*"([^"]*)".*$', "\\1", code)
  )
}

# Check the `n_body` body rows of the sheet of `f` against the console
# text; returns the counts of number and text cells checked.
.xl_inv_check <- function(f, n_body, mark = ".") {
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  suppressMessages(f(output = "excel", excel_path = path))
  wb <- openxlsx2::wb_load(path)
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  fmt_of <- function(s) {
    xf <- wb$styles_mgr$styles$cellXfs[[as.integer(s) + 1L]]
    id <- sub('.*numFmtId="([0-9]+)".*', "\\1", xf)
    hit <- grep(
      paste0('numFmtId="', id, '"'),
      wb$styles_mgr$styles$numFmts,
      value = TRUE
    )
    if (length(hit) == 0L) {
      return("General")
    }
    gsub("&quot;", '"', sub('.*formatCode="([^"]*)".*', "\\1", hit[[1L]]))
  }
  number <- cc$c_t %in% c("", "n") & nzchar(cc$v)
  cc <- cc[number | cc$c_t == "inlineStr", ]
  number <- cc$c_t %in% c("", "n")
  rows <- sort(unique(as.integer(cc$row_r)))
  clip <- .xl_inv_clip(f)
  expect_identical(length(rows), length(clip))
  wide <- unique(as.integer(cc$row_r[cc$c_r != "A"]))
  body <- utils::tail(sort(wide), n_body)
  counts <- c(number = 0L, text = 0L)
  for (k in which(rows %in% body)) {
    for (i in which(as.integer(cc$row_r) == rows[[k]] & cc$c_r != "A")) {
      j <- match(cc$c_r[[i]], LETTERS)
      want <- if (j <= length(clip[[k]])) clip[[k]][[j]] else ""
      # The mark of the number, not that of the df after it.
      norm <- sub(paste0("^([+-]?[0-9]*)\\", mark), "\\1.", want)
      simple <- !is.null(spicy:::.xl_cell(want, NA, NULL, mark)$numfmt)
      at <- paste0(cc$r[[i]], " '", want, "'")
      if (number[[i]]) {
        counts[["number"]] <- counts[["number"]] + 1L
        shown <- spicy:::.xl_shown(
          as.numeric(cc$v[[i]]),
          .xl_inv_spec(fmt_of(cc$c_s[[i]]))
        )
        expect_identical(shown, norm, label = at)
      } else {
        counts[["text"]] <- counts[["text"]] + 1L
        expect_false(simple, label = at)
        text <- gsub("<[^>]+>", "", cc$is[[i]])
        text <- gsub("&lt;", "<", text, fixed = TRUE)
        text <- gsub("&gt;", ">", text, fixed = TRUE)
        text <- gsub("&amp;", "&", text, fixed = TRUE)
        expect_identical(text, want, label = at)
      }
    }
  }
  counts
}

# Values half way between two displays, built on purpose: a percentage
# of 1/16 at one decimal (6.25), a weighted count of 36.5, a mean of
# 2.675 at two decimals. R prints them "6.2", "36", "2.67"; Excel would
# print the typed value "6.3", "37", "2.68". And a mean of -0.001, which
# R prints "-0.00" and Excel "0.00" under a format of one section.
.xl_inv_data <- function() {
  data.frame(
    g = factor(rep(c("A", "B"), each = 16L)),
    f = factor(rep(c("a", rep("b", 15L)), 2L)),
    f2 = factor(rep(c(rep("a", 2L), rep("b", 14L)), 2L)),
    x = rep(c(2.6, 2.75), 16L),
    w = rep(c(3.25, rep(1, 15L)), 2L),
    m1 = rep(c(TRUE, rep(FALSE, 15L)), 2L),
    m2 = rep(c(TRUE, TRUE, rep(FALSE, 14L)), 2L),
    z = rep(c(-0.002, 0), 16L)
  )
}

test_that("the descriptive sheets write each one-number cell as a number", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("clipr")
  skip_if_not_installed("survey")
  d <- .xl_inv_data()
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  tables <- list(
    function(...) table_continuous(d, x, by = g, statistic = TRUE, ...),
    function(...) table_continuous(d, x, weights = w, ...),
    function(...) table_continuous(d, z, by = g, ...),
    function(...) table_continuous_svy(des, x, by = g, ...),
    function(...) table_continuous_lm(d, x, by = g, statistic = TRUE, ...),
    function(...) {
      table_outcome(d, outcome = x, select = f, effect_size = "auto", ...)
    },
    function(...) table_battery(d, select = c(f, f2), by = g, ...),
    function(...) table_battery(d, select = c(f, f2), weights = w, ...),
    function(...) table_battery_svy(des, select = c(f, f2), ...),
    function(...) {
      table_categorical_svy(des, f, by = g, proportion_ci = TRUE, ...)
    },
    function(...) table_categorical(d, c(f, f2), ...),
    function(...) {
      table_categorical(d, c(f, f2), by = g, assoc_ci = TRUE, smd = TRUE, ...)
    },
    function(...) table_categorical(d, f, by = g, weights = w, ...),
    function(...) {
      table_multiresponse(d, select = c(m1, m2), by = g, percent = "both", ...)
    }
  )
  for (mark in c(".", ",")) {
    for (f in tables) {
      g <- function(...) f(decimal_mark = mark, ...)
      n_body <- NROW(as_structured(suppressWarnings(g()))$body)
      counts <- suppressWarnings(.xl_inv_check(g, n_body, mark))
      expect_gt(counts[["number"]], 0L)
    }
  }
})

test_that("a value half way between two displays is written as the console shows it", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")
  d <- .xl_inv_data()
  read <- function(f) {
    path <- tempfile(fileext = ".xlsx")
    on.exit(unlink(path), add = TRUE)
    f(output = "excel", excel_path = path)
    cc <- openxlsx2::wb_load(path)$worksheets[[1L]]$sheet_data$cc
    as.numeric(cc$v[cc$c_t %in% c("", "n") & nzchar(cc$v)])
  }
  expect_true(
    6.2 %in% read(function(...) table_battery(d, select = c(f, f2), ...))
  )
  expect_true(
    36 %in%
      read(function(...) {
        table_battery(d, select = c(f, f2), weights = w, ...)
      })
  )
  expect_true(2.67 %in% read(function(...) table_continuous(d, x, ...)))
  # "-0.00": the typed value, under a format with a negative section.
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  table_continuous(d, z, output = "excel", excel_path = path)
  wb <- openxlsx2::wb_load(path)
  cc <- wb$worksheets[[1L]]$sheet_data$cc
  m <- cc[cc$r == "B5", ]
  expect_equal(as.numeric(m$v), -0.001)
  xf <- wb$styles_mgr$styles$cellXfs[[as.integer(m$c_s) + 1L]]
  id <- sub('.*numFmtId="([0-9]+)".*', "\\1", xf)
  code <- grep(
    paste0('numFmtId="', id, '"'),
    wb$styles_mgr$styles$numFmts,
    value = TRUE
  )
  expect_match(code, 'formatCode="0.00;-0.00"', fixed = TRUE)
})

test_that("the regression sheets write each one-number cell as a number", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("clipr")
  d <- as.data.frame(sochealth)
  m1 <- lm(wellbeing_score ~ age + sex + education, data = d)
  m2 <- lm(bmi ~ age + sex, data = d)
  nested <- list(
    lm(wellbeing_score ~ age, data = d),
    lm(wellbeing_score ~ age + sex + education, data = d)
  )
  logit <- glm(smoking ~ age + sex, data = d, family = binomial)
  tables <- list(
    function(...) table_regression(m1, ...),
    function(...) table_regression(list(Wellbeing = m1, BMI = m2), ...),
    function(...) table_regression(nested, nested = TRUE, ...),
    function(...) {
      table_regression(m1, show_columns = c("b", "p", "partial_f"), ...)
    },
    function(...) table_regression(logit, exponentiate = TRUE, ...),
    function(...) {
      table_regression(
        m1,
        style = spicy_style(p_bands = list(c(0.05, 3), c(Inf, 2))),
        ...
      )
    },
    function(...) {
      table_regression_uv(
        d,
        outcome = "smoking",
        predictors = c("age", "sex"),
        method = "glm",
        ...
      )
    }
  )
  for (mark in c(".", ",")) {
    for (f in tables) {
      g <- function(...) f(decimal_mark = mark, ...)
      n_body <- NROW(as_structured(suppressWarnings(g()))$body)
      counts <- suppressWarnings(.xl_inv_check(g, n_body, mark))
      expect_gt(counts[["number"]], 0L)
    }
  }
})

# Each wb_add_*() call of openxlsx2 copies the whole workbook: a sheet
# written with one such call per cell or per run of cells took seconds.
# The calls that remain are per column or per sheet, never per row, so a
# longer table makes no more of them.
test_that("the Excel export does not copy the workbook per row", {
  skip_on_cran()
  skip_if_not_installed("openxlsx2")
  calls <- 0L
  real_data <- openxlsx2::wb_add_data
  real_fmt <- openxlsx2::wb_add_numfmt
  local_mocked_bindings(
    wb_add_data = function(...) {
      calls <<- calls + 1L
      real_data(...)
    },
    wb_add_numfmt = function(...) {
      calls <<- calls + 1L
      real_fmt(...)
    },
    .package = "openxlsx2"
  )
  count <- function(f) {
    calls <<- 0L
    path <- tempfile(fileext = ".xlsx")
    on.exit(unlink(path), add = TRUE)
    suppressWarnings(f(output = "excel", excel_path = path))
    calls
  }
  d <- as.data.frame(sochealth)
  expect_identical(
    count(function(...) table_continuous(d, c(bmi, age), by = sex, ...)),
    count(function(...) {
      table_continuous(d, c(bmi, age, wellbeing_score, income), by = sex, ...)
    })
  )
  expect_identical(
    count(function(...) {
      table_categorical(d, c(smoking, dentist_12m), by = sex, ...)
    }),
    count(function(...) {
      table_categorical(d, c(smoking, education, region), by = sex, ...)
    })
  )
  expect_identical(
    count(function(...) table_regression(lm(bmi ~ age + sex, data = d), ...)),
    count(function(...) {
      table_regression(lm(bmi ~ age + sex + education + region, data = d), ...)
    })
  )
})
