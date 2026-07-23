# Coverage-gap tests for the descriptive-table / helper family (group g10).
#
# Each test names the R file + line(s) it exercises. Assertions pin exact
# values (against base-R oracles), exact condition classes, or exact output
# strings -- never bare execution.

# ---- table_helpers.R lines 124-125: safe_glyph_width() fallback ------------

test_that("safe_glyph_width falls back to nchar() for zero-width elements", {
  # An empty string has display width 0, which trips the `w < 1L` guard;
  # the fallback re-measures the flagged elements with plain nchar().
  expect_identical(spicy:::safe_glyph_width(c("ab", "")), c(2L, 0L))
  # Untouched elements keep their display width.
  expect_identical(spicy:::safe_glyph_width("abc"), 3L)
})

# ---- freq.R line 477: cum = TRUE with valid = FALSE ------------------------

test_that("freq(cum = TRUE, valid = FALSE) fills cum_valid_prop with NA", {
  f <- freq(c(1, 1, 2, NA), cum = TRUE, valid = FALSE, output = "data.frame")
  expect_identical(f$value, c("1", "2", NA))
  expect_equal(f$n, c(2, 1, 1))
  expect_equal(f$prop, c(0.5, 0.25, 0.25))
  expect_equal(f$cum_prop, cumsum(f$prop))
  # valid = FALSE: the cumulative valid column is a pure NA placeholder.
  expect_identical(f$cum_valid_prop, c(NA, NA, NA))
})

# ---- cross_tab.R line 955: `by` with a single unique (non-factor) value ----

test_that("cross_tab() handles a non-factor `by` with a single unique value", {
  d <- data.frame(
    x = c("a", "b", "a", "b"),
    y = c("u", "v", "u", "v"),
    g = rep("only", 4),
    stringsAsFactors = FALSE
  )
  res <- cross_tab(d, x, y, by = g)
  expect_s3_class(res, "spicy_cross_table_list")
  expect_identical(names(res), "only")
  only <- as.data.frame(res[["only"]])
  expect_identical(as.character(only$Values), c("a", "b", "Total"))
  expect_equal(only$u, c(2, 0, 2))
  expect_equal(only$v, c(0, 2, 2))
  expect_equal(only$Total, c(2, 2, 4))
})

# ---- count_n.R line 319: verbose report of incompatible columns ------------

test_that("count_n(verbose = TRUE) reports columns whose comparison fails", {
  # A declared-UTF-8 string with an invalid byte sequence makes tolower()
  # error inside compare_fun(), so the column is dropped as incompatible.
  bad <- rawToChar(as.raw(0xe9))
  Encoding(bad) <- "UTF-8"
  df <- data.frame(x = c("a", "b"), stringsAsFactors = FALSE)
  df$mojibake <- c(bad, bad)
  expect_message(
    res <- count_n(df, count = "b", ignore_case = TRUE, verbose = TRUE),
    "Ignored incompatible columns: mojibake",
    fixed = TRUE
  )
  # The valid column still counts normally.
  expect_equal(res, c(0, 1))
})

# ---- assoc.R line 1360: symmetric Somers' d NA when d_r + d_c == 0 ---------

test_that("somers_d(symmetric) returns NA when both asymmetric d are zero", {
  # Uniform 2x2 table: C == D, so d(R|C) = d(C|R) = 0 and the harmonic
  # mean is undefined (0/0); the NA arm must fire.
  tab <- as.table(matrix(c(1, 1, 1, 1), 2, 2))
  expect_identical(somers_d(tab, direction = "row"), 0)
  expect_identical(somers_d(tab, direction = "column"), 0)
  expect_identical(somers_d(tab, direction = "symmetric"), NA_real_)
  # Contrast: with real association the symmetric value is the harmonic
  # mean of the two asymmetric values (SPSS/PSPP definition).
  t2 <- as.table(matrix(c(10, 2, 3, 8), 2, 2))
  d_r <- somers_d(t2, direction = "row")
  d_c <- somers_d(t2, direction = "column")
  expect_equal(
    somers_d(t2, direction = "symmetric"),
    2 * d_r * d_c / (d_r + d_c)
  )
})

# ---- standardize_glm.R line 346: SD(Y*) with < 2 finite linear predictors --

test_that("compute_menard_sd_y_star returns NA with fewer than 2 finite eta", {
  # na.exclude pads predict() back to the original length; only one row
  # was fitted, so exactly one finite eta remains -> var() is undefined.
  dg <- data.frame(y = c(1, NA))
  fitg <- suppressWarnings(
    glm(y ~ 1, family = binomial(), data = dg, na.action = stats::na.exclude)
  )
  eta <- suppressWarnings(as.numeric(stats::predict(fitg, type = "link")))
  expect_lt(sum(is.finite(eta)), 2)
  expect_identical(spicy:::compute_menard_sd_y_star(fitg), NA_real_)
  # Contrast oracle: a regular binomial logit fit yields
  # sqrt(var(eta) + pi^2/3) (Menard 2011 / Long & Freese 2014).
  fit2 <- glm(am ~ mpg, family = binomial(), data = mtcars)
  expected <- sqrt(
    stats::var(as.numeric(stats::predict(fit2, type = "link"))) + pi^2 / 3
  )
  expect_equal(spicy:::compute_menard_sd_y_star(fit2), expected)
})

# ---- standardize_lm.R line 528: lme/gls inline-transform refusal -----------

test_that(".compute_beta_rows_for_mixed declines lme/gls formulas with inline transforms", {
  skip_if_not_installed("nlme")
  fit_gls <- nlme::gls(mpg ~ log(disp), data = mtcars)
  # Preconditions: the data extraction works and the formula really does
  # contain a non-symbol term, so the NULL below comes from the
  # inline-transform guard, not an earlier failure path.
  expect_s3_class(nlme::getData(fit_gls), "data.frame")
  vars_gls <- as.list(attr(stats::terms(fit_gls), "variables"))[-1L]
  expect_false(all(vapply(vars_gls, is.symbol, logical(1))))
  expect_null(spicy:::.compute_beta_rows_for_mixed(fit_gls))

  fit_lme <- nlme::lme(
    distance ~ log(age),
    random = ~ 1 | Subject,
    data = nlme::Orthodont
  )
  expect_null(spicy:::.compute_beta_rows_for_mixed(fit_lme))
})

# ---- code_book-filename.R line 26: NA filename blanked before validation ---

test_that("code_book_sanitize_filename maps NA input to fallback or error", {
  # An NA filename survives the ASCII transliteration as NA, is blanked
  # (line 26), and then either falls back or aborts like an empty string.
  err <- tryCatch(
    spicy:::code_book_sanitize_filename(
      NA_character_,
      arg = "filename",
      fallback = NULL
    ),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(
    conditionMessage(err),
    "`filename` must contain at least one letter",
    fixed = TRUE
  )
  expect_identical(
    spicy:::code_book_sanitize_filename(
      NA_character_,
      arg = "title",
      fallback = "Codebook"
    ),
    "Codebook"
  )
})

# ---- code_book-validation.R line 47: dots with NULL names ------------------

test_that("validate_code_book_control_dots passes fully unnamed dots through", {
  dots <- list(1, "a")
  expect_identical(spicy:::validate_code_book_control_dots(dots), dots)
})

# ---- varlist-values.R line 134 + line 254: unsortable values / all-NA matrix

test_that("varlist reports unsortable values and 0 distinct for all-NA matrix columns", {
  dv <- data.frame(a = 1:3)
  # Complex vectors cannot be radix-sorted: show_vals()'s tryCatch must
  # surface the literal "Error: invalid values" cell.
  dv$z <- complex(real = c(1, 3, 0), imaginary = c(2, 4, 1))
  # A matrix column whose rows are all missing has zero distinct rows.
  dv$m <- matrix(NA_real_, 3, 2)
  vl <- varlist(dv, values = TRUE, tbl = TRUE)
  expect_identical(
    vl$Values[vl$Variable == "z"],
    "Error: invalid values"
  )
  expect_identical(as.integer(vl$N_distinct[vl$Variable == "m"]), 0L)
  expect_identical(as.integer(vl$N_valid[vl$Variable == "m"]), 0L)
  expect_identical(as.integer(vl$NAs[vl$Variable == "m"]), 3L)
  # Sanity: the ordinary integer column is summarised normally.
  expect_identical(vl$Values[vl$Variable == "a"], "1, 2, 3")
})

# ---- table_continuous_print.R lines 201, 283, 333: tibble-less branches ----

test_that("spicy_continuous_table broom methods degrade without tibble", {
  out <- utils::capture.output(tc <- table_continuous(mtcars, mpg))
  expect_s3_class(tc, "spicy_continuous_table")

  # Line 283: tidy() falls back to a plain data.frame.
  tidied <- with_mocked_bindings(
    spicy:::tidy.spicy_continuous_table(tc),
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  expect_identical(class(tidied), "data.frame")
  expect_equal(tidied$estimate, mean(mtcars$mpg))
  expect_equal(tidied$std.error, stats::sd(mtcars$mpg) / sqrt(nrow(mtcars)))
  expect_identical(tidied$n, 32L)

  # Line 333: glance() falls back to a plain data.frame.
  glanced <- with_mocked_bindings(
    spicy:::glance.spicy_continuous_table(tc),
    requireNamespace = function(...) FALSE,
    .package = "base"
  )
  expect_identical(class(glanced), "data.frame")
  expect_identical(glanced$n_total, 32L)
  expect_true(is.na(glanced$statistic))

  # Line 201: as_tibble() aborts with the actionable install hint.
  err <- tryCatch(
    with_mocked_bindings(
      spicy:::as_tibble.spicy_continuous_table(tc),
      requireNamespace = function(...) FALSE,
      .package = "base"
    ),
    error = function(e) e
  )
  expect_s3_class(err, "spicy_missing_pkg")
  expect_match(conditionMessage(err), "Install package 'tibble'.", fixed = TRUE)
})

# ---- table_continuous_lm_render.R lines 816-818: un-prefixed flextable note

test_that("export_continuous_lm_table renders a note without the Note. prefix as plain text", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  dd <- data.frame(
    Variable = "x",
    Estimate = "1.0",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  note <- "Custom footer text without prefix."
  ft <- spicy:::export_continuous_lm_table(
    dd,
    output = "flextable",
    ci_level = 0.95,
    align = "decimal",
    decimal_mark = ".",
    excel_path = NULL,
    excel_sheet = NULL,
    clipboard_delim = "\t",
    word_path = NULL,
    note = note
  )
  expect_s3_class(ft, "flextable")
  expect_s3_class(ft, "spicy_flextable")
  expect_identical(attr(ft, "spicy_note"), note)
  html <- paste(as.character(flextable::htmltools_value(ft)), collapse = "")
  # The whole note lands in the footer verbatim (single non-italic chunk;
  # no "Note." chunk was split off).
  expect_true(grepl(note, html, fixed = TRUE))
})

# ---- varlist-title.R line 171: skip not-loaded namespaces in verb probe ----

test_that("varlist_is_data_first skips not-loaded namespaces and rejects unknown verbs", {
  # Curated fast path stays intact.
  expect_true(spicy:::varlist_is_data_first("count"))

  probes <- c("dplyr", "tidyr", "tibble")
  # The introspection loop only `next`s over a namespace that is not
  # loaded; free tidyr up (dplyr/tibble are pinned by spicy's own import
  # chain via labelled/haven). Suggests packages loaded earlier in the
  # suite (e.g. officer imports tidyr) may hold it, so unload those
  # importers first; everything unloaded is reloaded at the end.
  unload_with_users <- function(pkg, depth = 3L) {
    if (!isNamespaceLoaded(pkg)) {
      return(TRUE)
    }
    if (depth <= 0L) {
      return(FALSE)
    }
    ok <- tryCatch(
      {
        unloadNamespace(pkg)
        TRUE
      },
      error = function(e) FALSE
    )
    if (ok) {
      return(TRUE)
    }
    users <- Filter(
      function(p) {
        imp <- tryCatch(getNamespaceImports(p), error = function(e) NULL)
        !is.null(imp) && pkg %in% names(imp)
      },
      loadedNamespaces()
    )
    for (u in users) {
      unload_with_users(u, depth - 1L)
    }
    tryCatch(
      {
        unloadNamespace(pkg)
        TRUE
      },
      error = function(e) FALSE
    )
  }
  unloaded <- character()
  if (all(vapply(probes, isNamespaceLoaded, logical(1)))) {
    before <- loadedNamespaces()
    unload_with_users("tidyr")
    unloaded <- setdiff(before, loadedNamespaces())
  }
  skip_if(
    all(vapply(probes, isNamespaceLoaded, logical(1))),
    "all probe namespaces loaded and none can be unloaded"
  )
  expect_false(spicy:::varlist_is_data_first("no_such_data_verb_spicy"))
  for (pkg in unloaded) {
    requireNamespace(pkg, quietly = TRUE)
  }
})
