# spicy_style(big_mark = ): the thousands separator of the counts.

bm_data <- function() {
  set.seed(11)
  n <- 4100
  data.frame(
    g = rep(c("a", "b"), c(1500, 2600)),
    h = sample(c("x", "y"), n, TRUE),
    yr = factor(sample(c(2023, 2024), n, TRUE)),
    y = rnorm(n),
    z = rnorm(n),
    bin = rbinom(n, 1, 0.3),
    q1 = sample(1:3, n, TRUE),
    q2 = sample(1:3, n, TRUE),
    m1 = rbinom(n, 1, 0.5),
    m2 = rbinom(n, 1, 0.3),
    w = runif(n, 0.5, 1.5)
  )
}

bm_print <- function(x) paste(capture.output(print(x)), collapse = "\n")

comma <- spicy_style(big_mark = ",")

test_that("big_mark is NULL or one character that no number holds", {
  expect_null(spicy_style()$big_mark)
  expect_identical(spicy_style(big_mark = ",")$big_mark, ",")
  expect_identical(spicy_style(big_mark = "\u202f")$big_mark, "\u202f")
  expect_identical(spicy_style(big_mark = "'")$big_mark, "'")
  for (bad in list(",,", "", 1, NA_character_, c(",", "."))) {
    expect_error(spicy_style(big_mark = bad), class = "spicy_invalid_input")
  }
  for (bad in c("5", ".", "-", "\u2212")) {
    expect_error(spicy_style(big_mark = bad), class = "spicy_invalid_input")
  }
  # The decimal mark of the style, a theme's included.
  expect_error(
    spicy_style(decimal_mark = ",", big_mark = ","),
    class = "spicy_invalid_input"
  )
  expect_error(
    spicy_style("lancet", big_mark = "\u00b7"),
    class = "spicy_invalid_input"
  )
  expect_identical(
    spicy_style(decimal_mark = ",", big_mark = ".")$big_mark,
    "."
  )
  expect_identical(spicy_style("apa", big_mark = ",")$big_mark, ",")
})

test_that("no separator by default, nor from the French language", {
  d <- bm_data()
  out <- bm_print(table_categorical(d, g))
  expect_match(out, "1500", fixed = TRUE)
  withr::local_options(spicy.language = "fr")
  out_fr <- bm_print(table_categorical(d, g))
  expect_match(out_fr, "1500", fixed = TRUE)
  expect_no_match(out_fr, "1\u202f500", fixed = TRUE)
})

test_that("freq() and cross_tab() read the separator from options(spicy.style)", {
  d <- bm_data()
  withr::local_options(spicy.style = comma)
  out <- bm_print(freq(d, g))
  expect_match(out, "1,500", fixed = TRUE)
  expect_match(out, "4,100", fixed = TRUE)
  ct <- bm_print(cross_tab(d, g, h))
  expect_match(ct, "1,500", fixed = TRUE)
  ct_n <- bm_print(cross_tab(d, g, h, percent = "none"))
  expect_match(ct_n, "4,100", fixed = TRUE)
  # The data frame keeps its numbers.
  df <- freq(d, g, output = "data.frame")
  expect_true(is.numeric(df$n))
})

test_that("the descriptive tables group their counts, notes and headers", {
  d <- bm_data()
  out <- bm_print(table_categorical(d, g, by = h, style = comma))
  expect_match(out, "1,500", fixed = TRUE)
  expect_match(out, "N = 4,100", fixed = TRUE)
  hdr <- bm_print(table_categorical(
    d,
    g,
    by = h,
    style = spicy_style(big_mark = ",", sample_size = "header")
  ))
  # The spaces of "(n = 2,099)" do not break.
  expect_match(hdr, "x [(]n.=.2,[0-9]{3}[)]")
  out <- bm_print(table_continuous(d, y, by = h, style = comma))
  expect_match(out, "N = 4,100", fixed = TRUE)
  expect_match(out, "2,0[0-9]{2}")
  out <- bm_print(table_outcome(d, y, select = g, style = comma))
  expect_match(out, "1,500", fixed = TRUE)
  out <- bm_print(table_battery(d, c(q1, q2), style = comma))
  expect_match(out, "4,100", fixed = TRUE)
  out <- bm_print(table_multiresponse(d, c(m1, m2), style = comma))
  expect_match(out, "[0-9],[0-9]{3}")
  out <- bm_print(table_continuous_lm(d, y, by = g, style = comma))
  expect_match(out, "N = 4,100", fixed = TRUE)
})

test_that("the counts of a note take the separator, the degrees of freedom do not", {
  d <- bm_data()
  d$y[1:1234] <- NA
  out <- bm_print(table_continuous(d, y, by = h, style = comma))
  expect_match(out, "y (1,234)", fixed = TRUE)
  # An F test on 4,098 residual degrees of freedom keeps them whole.
  lm_out <- bm_print(table_continuous_lm(
    bm_data(),
    y,
    by = g,
    style = comma
  ))
  expect_no_match(lm_out, "4,098", fixed = TRUE)
})

test_that("a design note groups its clusters, never its degrees of freedom", {
  skip_if_not_installed("survey")
  d <- bm_data()
  d$id <- ceiling(seq_len(nrow(d)) / 2)
  des <- survey::svydesign(ids = ~id, weights = ~w, data = d)
  out <- bm_print(table_categorical_svy(des, g, style = comma))
  expect_match(out, "2,050 PSU", fixed = TRUE)
  expect_match(out, "2049 degrees of freedom", fixed = TRUE)
})

test_that("the note of a univariable screen groups its complete cases", {
  d <- bm_data()
  d$z[1:100] <- NA
  out <- bm_print(table_regression_uv(
    d,
    bin,
    c(z, g),
    family = binomial,
    style = comma
  ))
  expect_match(out, "(4,000.4,100)")
})

test_that("years and percentages are no counts", {
  d <- bm_data()
  out <- bm_print(table_categorical(d, yr, style = comma))
  expect_match(out, "2023", fixed = TRUE)
  expect_no_match(out, "2,023", fixed = TRUE)
  expect_match(out, "4,100", fixed = TRUE)
})

test_that("the survey tables group counts and weighted counts", {
  skip_if_not_installed("survey")
  d <- bm_data()
  d$w2 <- 2.5e8 / nrow(d)
  des <- survey::svydesign(ids = ~1, weights = ~w2, data = d)
  out <- bm_print(table_categorical_svy(des, g, by = h, style = comma))
  expect_match(out, "4,100", fixed = TRUE)
  expect_match(out, "250,000,000", fixed = TRUE)
  out <- bm_print(table_continuous_svy(des, y, by = h, style = comma))
  expect_match(out, "250,000,000", fixed = TRUE)
  out <- bm_print(table_battery_svy(des, c(q1, q2), style = comma))
  expect_match(out, "[0-9],[0-9]{3}")
})

test_that("table_regression() groups N, a weighted N of 2.5e8 and the groups", {
  skip_if_not_installed("survey")
  d <- bm_data()
  fit <- lm(y ~ z + g, data = d)
  out <- bm_print(table_regression(fit, style = comma))
  expect_match(out, "4,100", fixed = TRUE)
  d$w2 <- 2.5e8 / nrow(d)
  des <- survey::svydesign(ids = ~1, weights = ~w2, data = d)
  sfit <- survey::svyglm(y ~ z, design = des)
  out <- bm_print(table_regression(
    sfit,
    style = comma,
    show_fit_stats = c("nobs", "weighted_nobs")
  ))
  expect_match(out, "250,000,000", fixed = TRUE)
  expect_match(out, "4,100", fixed = TRUE)
  # The structured outputs keep numbers.
  skip_if_not_installed("broom")
  gl <- broom::glance(table_regression(
    sfit,
    style = comma,
    show_fit_stats = "weighted_nobs"
  ))
  expect_equal(gl$weighted_nobs, 2.5e8)
})

test_that("table_stack() keeps the grouped N of its panels", {
  d <- bm_data()
  out <- bm_print(table_stack(
    table_regression(lm(y ~ z, d), style = comma),
    table_regression(lm(y ~ g, d), style = comma)
  ))
  expect_identical(lengths(regmatches(out, gregexpr("4,100", out))), 2L)
})

test_that("table_regression_uv() groups the N of its models", {
  d <- bm_data()
  out <- bm_print(table_regression_uv(
    d,
    bin,
    c(z, g),
    family = binomial,
    style = comma
  ))
  expect_match(out, "4,100", fixed = TRUE)
})

test_that("the narrow no-break space groups the counts", {
  d <- bm_data()
  out <- bm_print(table_categorical(
    d,
    g,
    style = spicy_style(big_mark = "\u202f")
  ))
  expect_match(out, "1\u202f500", fixed = TRUE)
  expect_match(out, "N = 4\u202f100", fixed = TRUE)
})

test_that("a column of 12 and 1,234,567 stays aligned on its units", {
  # Console: the padder counts a separator as one column.
  local({
    .style_push(list(big_mark = ","))
    on.exit(.style_pop())
    v <- c("12", "1,234,567", "3.5")
    a <- decimal_align_strings(v)
    expect_identical(length(unique(nchar(a))), 1L)
    expect_identical(
      regexpr("2", a[[1L]]) + 0L,
      regexpr("7", a[[2L]]) + 0L
    )
    # Figure-space engines: a figure space per missing digit, a
    # punctuation space per missing comma.
    f <- decimal_align_strings(v, pad_char = "\u2007")
    expect_match(f[[1L]], "^\u2007{5}\u2008{2}12", perl = TRUE)
    p <- .pad_decimal_cells(v, logical(3), ".")
    expect_match(p[[1L]], "^\u2007{5}\u2008{2}12", perl = TRUE)
    # A narrow no-break separator pads with itself.
  })
  local({
    .style_push(list(big_mark = "\u202f"))
    on.exit(.style_pop())
    p <- .pad_decimal_cells(c("12", "1\u202f234"), logical(2), ".")
    expect_match(p[[1L]], "^\u2007{2}\u202f12", perl = TRUE)
  })
})

test_that("the rich engines carry the grouped, aligned counts", {
  d <- bm_data()
  d$g[1:12] <- "c"
  fit <- lm(y ~ z, data = d)
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  skip_if_not_installed("tinytable")
  gt_tab <- table_categorical(d, g, style = comma, output = "gt")
  gt_n <- gt_tab[["_data"]]$n
  expect_true("2,600" %in% gt_n)
  # 12 under 2,600: two figure spaces and a punctuation space.
  expect_true("\u2007\u2007\u200812" %in% gt_n)
  ft <- table_categorical(d, g, style = comma, output = "flextable")
  expect_true("2,600" %in% ft$body$dataset$n)
  tt_cat <- table_categorical(d, g, style = comma, output = "tinytable")
  expect_true(any(grepl("2,600", unlist(tt_cat@data), fixed = TRUE)))
  tt <- table_regression(fit, style = comma, output = "tinytable")
  html <- paste(tinytable::save_tt(tt, "html"), collapse = "\n")
  expect_match(html, "4,100", fixed = TRUE)
  tex <- paste(tinytable::save_tt(tt, "latex"), collapse = "\n")
  expect_match(tex, "4,100", fixed = TRUE)
  expect_no_match(tex, "\u2008", fixed = TRUE)
  typ <- paste(tinytable::save_tt(tt, "typst"), collapse = "\n")
  expect_match(typ, "4,100", fixed = TRUE)
})

test_that("a fit statistic in the Events/N column is no count", {
  skip_if_not_installed("tinytable")
  skip_if_not_installed("openxlsx2")
  d <- bm_data()
  fit <- glm(bin ~ z, data = d, family = binomial)
  aic <- sprintf("%.1f", stats::AIC(fit))
  expect_gt(stats::AIC(fit), 1000)
  sc <- c("n_events", "b", "p")
  expect_match(
    bm_print(table_regression(fit, style = comma, show_columns = sc)),
    aic,
    fixed = TRUE
  )
  tt <- table_regression(
    fit,
    style = comma,
    show_columns = sc,
    output = "tinytable"
  )
  html <- paste(tinytable::save_tt(tt, "html"), collapse = "\n")
  expect_match(html, aic, fixed = TRUE)
  expect_match(html, "4,100", fixed = TRUE)
  # Excel, side by side and stacked: the AIC takes no grouped format.
  for (layout in c("columns", "stacked")) {
    path <- withr::local_tempfile(fileext = ".xlsx")
    table_regression(
      list(fit, fit),
      layout = layout,
      style = comma,
      show_columns = sc,
      output = "excel",
      excel_path = path
    )
    fmts <- openxlsx2::wb_load(path)$styles_mgr$styles$numFmts
    expect_false(any(grepl("#,##0.0", fmts, fixed = TRUE)))
  }
})

test_that("Excel keeps a count a number, grouped by its number format", {
  skip_if_not_installed("openxlsx2")
  d <- bm_data()
  path <- withr::local_tempfile(fileext = ".xlsx")
  table_categorical(
    d,
    g,
    style = comma,
    output = "excel",
    excel_path = path
  )
  wb <- openxlsx2::wb_load(path)
  df <- openxlsx2::wb_to_df(wb, col_names = FALSE)
  vals <- unlist(df)
  expect_true(1500 %in% suppressWarnings(as.numeric(vals)))
  expect_false(any(grepl("1,500", vals, fixed = TRUE)))
  fmts <- wb$styles_mgr$styles$numFmts
  expect_true(any(grepl("#,##0", fmts, fixed = TRUE)))
  # Without the lever, the format stays "0".
  path0 <- withr::local_tempfile(fileext = ".xlsx")
  table_categorical(d, g, output = "excel", excel_path = path0)
  wb0 <- openxlsx2::wb_load(path0)
  expect_false(any(grepl("#,##0", wb0$styles_mgr$styles$numFmts, fixed = TRUE)))
})
