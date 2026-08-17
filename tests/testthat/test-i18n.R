# Guards for the display-string registry (R/i18n.R).
#
# The registry is the single source of every string a reader of a spicy table
# sees. These tests protect three properties: the defaults do not drift
# unnoticed, the keys stay unique and alive, and every template is a valid
# `sprintf` format.

test_that("the registry itself is under snapshot", {
  # One key per line: the default print of a named character vector reflows
  # with `getOption("width")` and with the longest value, so a snapshot of it
  # would churn on every addition instead of showing the addition.
  expect_snapshot(cat(
    sprintf(
      "%s = %s",
      names(.spicy_strings),
      encodeString(unname(.spicy_strings), quote = '"')
    ),
    sep = "\n"
  ))
})

test_that("registry keys are unique and non-empty", {
  ks <- names(.spicy_strings)
  expect_false(anyDuplicated(ks) > 0L)
  expect_true(all(nzchar(ks)))
  expect_type(unname(.spicy_strings), "character")
})

test_that("no dead keys: every registry key is consumed in R/", {
  # Only meaningful against the sources; skipped for an installed
  # package. `dir.exists()` is NOT the right guard: an installed
  # package also has an R/ directory two levels up under covr's
  # layout, holding the lazy-load database and no .R source -- every
  # key then looks dead (2026-08-15 test-coverage CI failure). Probe
  # for a known source file instead.
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(
    file.exists(file.path(r_dir, "i18n.R")),
    "package sources not available"
  )
  files <- list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE)
  files <- files[basename(files) != "i18n.R"]
  src <- paste(
    unlist(lapply(files, readLines, warn = FALSE)),
    collapse = "\n"
  )
  ks <- names(.spicy_strings)
  dead <- ks[
    !vapply(
      ks,
      function(k) grepl(paste0('"', k, '"'), src, fixed = TRUE),
      logical(1)
    )
  ]
  expect_identical(dead, character(0))
})

test_that("the emphasised note prefix really is a prefix of the note prefix", {
  # The rich engines italicise `note_prefix_emphasis` and print the rest in
  # regular type. If the two ever stop agreeing, every Word / HTML note
  # silently loses (or duplicates) its opening.
  expect_true(startsWith(
    spicy_str("note_prefix"),
    spicy_str("note_prefix_emphasis")
  ))
  split <- .note_prefix_split(paste0(spicy_str("note_prefix"), "body."))
  expect_identical(split$marker, spicy_str("note_prefix_emphasis"))
  expect_identical(
    paste0(split$marker, split$rest),
    paste0(spicy_str("note_prefix"), "body.")
  )
  expect_null(.note_prefix_split("no prefix here"))
})

test_that("every frozen categorical key equals its English display label", {
  # The column NAME and the header are two layers that hold the same
  # string in English and are free to diverge at stage 2. Nothing else
  # can catch a key that stops matching its label -- the key is pinned by
  # `%in% names(...)` tests, the label by the console snapshots, and both
  # sets of pins would stay green past a drift.
  couples <- list(
    list(.CAT_KEY_VARIABLE, "header_variable"),
    list(.CAT_KEY_P, "header_p"),
    list(.CAT_MARGIN_KEY, "header_margin_total"),
    list(.CAT_KEY_CI_LL, "header_ci_lower"),
    list(.CAT_KEY_CI_UL, "header_ci_upper"),
    list(.CAT_KEY_EFFECT_SIZE, "header_effect_size"),
    list("n", "header_n_lower"),
    list("%", "header_percent_symbol")
  )
  for (cp in couples) {
    expect_identical(cp[[1L]], spicy_str(cp[[2L]]), info = cp[[2L]])
  }
  # The ninth couple is a composition rule, not a constant: `paste0()` on
  # the key side, the registry template on the label side.
  expect_identical(.cat_key_n("G"), .cat_label_n("G"))
  expect_identical(.cat_key_pct("G"), .cat_label_pct("G"))

  # `.assoc_key()` names the public column of `table_categorical()`,
  # `.assoc_label()` names the header a reader sees. Two switch tables
  # that must stay equal at the English default.
  for (k in .assoc_measure_keys) {
    expect_identical(.assoc_key(k), .assoc_label(k))
  }
  # Both pass an unknown measure through unchanged.
  expect_identical(.assoc_key("no_such_measure"), "no_such_measure")
})

test_that("every frozen continuous key equals its English display label", {
  # Same contract as the categorical test above, for lot B. The column
  # NAME is a rendering key (flextable col_keys, gt ids, the gt CSS
  # selector) and an `as_structured()` key; the header is a separate
  # layer. They hold the same string in English, and only this test can
  # see them part company -- `expect_named()` pins the keys, the console
  # snapshots pin the headers, and both stay green through a drift.
  couples <- list(
    list(.CON_KEY_VARIABLE, "header_variable"),
    list(.CON_KEY_GROUP, "header_group"),
    list(.CON_KEY_TEST, "header_test"),
    list(.CON_KEY_P, "header_p"),
    list(.CON_KEY_ES, "header_effect_size_short"),
    list(.CON_KEY_N, "header_n_lower"),
    list(.CON_KEY_WEIGHTED_N, "header_weighted_n"),
    list(.CON_KEY_CI_LL, "header_ci_ll"),
    list(.CON_KEY_CI_UL, "header_ci_ul"),
    # The interval word INSIDE a column key ("95% CI LL"). It is not the
    # header key: a translated "CI" must never move a public key.
    list(.CON_KEY_CI, "header_ci_label_confidence")
  )
  for (cp in couples) {
    expect_identical(cp[[1L]], spicy_str(cp[[2L]]), info = cp[[2L]])
  }
  # The median prefix is a composition rule, not a constant: `paste0()`
  # on the key side, the registry word plus a space on the label side.
  expect_identical(.CON_KEY_MED_PREFIX, paste0(spicy_str("header_median"), " "))

  # The vocabulary itself: fifteen columns, each of which must carry a
  # label equal to its key, and each interval bound a spanner header
  # equal to the interval key it belongs to.
  entries <- unlist(
    .continuous_token_columns(0.95),
    recursive = FALSE,
    use.names = FALSE
  )
  expect_identical(
    vapply(entries, function(e) e$name, character(1)),
    vapply(entries, function(e) e$label, character(1))
  )
  bounds <- Filter(function(e) !is.null(e$ci_role), entries)
  expect_length(bounds, 4L)
  expect_identical(
    vapply(bounds, function(e) e$ci_key, character(1)),
    vapply(bounds, function(e) e$ci_label, character(1))
  )
  expect_identical(
    vapply(bounds, function(e) e$ci_role, character(1)),
    vapply(bounds, function(e) e$short_label, character(1))
  )
  # `.continuous_labels()` is a total function of its input: an unknown
  # key comes back unchanged, so a degraded object still prints.
  expect_identical(.continuous_labels("no such column", 0.95), "no such column")
})

test_that("every frozen linear-model key equals its English display label", {
  # Same contract as the two tests above, for lot C. Here the key layer
  # is unusually load-bearing: it names the public `data.frame` columns,
  # the flextable `col_keys`, the gt spanner ids and the `th[id="%s"]`
  # CSS selector. Only this test can see a key part company with its
  # header -- `%in% names(...)` pins the keys, the console snapshots pin
  # the headers, and both stay green through a drift.
  couples <- list(
    list(.LM_KEY_VARIABLE, "header_variable"),
    list(.LM_KEY_B, "header_b"),
    list(.LM_KEY_P, "header_p"),
    list(.LM_KEY_N, "header_n_lower"),
    list(.LM_KEY_WEIGHTED_N, "header_weighted_n"),
    list(.LM_KEY_CI_LL, "header_ci_ll"),
    list(.LM_KEY_CI_UL, "header_ci_ul"),
    list(.LM_KEY_MEAN, "header_mean"),
    list(.LM_KEY_DELTA, "symbol_delta"),
    # The interval word INSIDE a column key ("95% CI LL"), not the
    # header key: a translated "CI" must never move a public key.
    list(.LM_KEY_CI, "header_ci_label_confidence")
  )
  for (cp in couples) {
    expect_identical(cp[[1L]], spicy_str(cp[[2L]]), info = cp[[2L]])
  }

  # The four composition rules: `paste0()` on the key side, a registry
  # template on the label side.
  expect_identical(.lm_key_emmean("Female"), .lm_label_emmean("Female"))
  blk <- data.frame(level = c("Female", "Male"), stringsAsFactors = FALSE)
  expect_identical(get_delta_label_lm(blk), .lm_label_delta(blk))
  expect_identical(.lm_key_ci_ll("95%"), "95% CI LL")
  expect_identical(.lm_key_ci_ul("95%"), "95% CI UL")
  expect_identical(.lm_ci_label(0.95), "95% CI")

  # The two glyph tables of `.lm_test_render()`: one frozen, one from
  # the registry, equal at the English default and named alike.
  expect_identical(.LM_TEST_GLYPHS, .lm_test_glyph_labels())

  # Every branch of the test header, through BOTH twins. The shared
  # format body makes the punctuation impossible to drift; this covers
  # the glyphs and the pass-through default.
  mk <- function(tt, df1 = 1, df2 = 10) {
    data.frame(
      test_type = tt,
      df1 = df1,
      df2 = df2,
      statistic = 2,
      predictor_type = "categorical",
      estimate = c(NA, 1),
      level = c("A", "B"),
      stringsAsFactors = FALSE
    )
  }
  blocks <- list(
    mk("z"),
    mk("chi2"),
    mk("t"),
    mk("t", df2 = 45.34),
    mk("t", df2 = Inf),
    mk("F", df1 = 2, df2 = 30),
    mk("wald_custom")
  )
  for (b in blocks) {
    for (ex in c(TRUE, FALSE)) {
      expect_identical(
        get_test_header_lm(b, TRUE, ex),
        .lm_test_label(b, TRUE, ex),
        info = paste(b$test_type[[1L]], ex)
      )
    }
  }
  # Both twins pass an unknown token through unchanged.
  for (tok in c("r2", "adj_r2", "no_such_r2")) {
    expect_identical(format_r2_header_lm(tok), .lm_r2_label(tok), info = tok)
  }
  for (tok in c("f2", "d", "g", "omega2", "no_such_es")) {
    expect_identical(
      format_effect_size_header_lm(tok),
      .lm_es_label(tok),
      info = tok
    )
  }

  # The spec IS the column set: its keys and their order must equal the
  # display frame's, which is the invariant the typed view's abort used
  # to protect by matching strings one branch at a time.
  d <- data.frame(
    a = c(1, 2, 3, 10, 11, 12),
    b = c(2, 4, 6, 8, 10, 13),
    g = factor(c("x", "x", "x", "y", "y", "y")),
    w = c(1, 1, 2, 1, 3, 0.5)
  )
  lg <- table_continuous_lm(
    d,
    select = c(a, b),
    by = g,
    weights = w,
    statistic = TRUE,
    effect_size = "f2",
    r2 = "adj_r2",
    show_weighted_n = TRUE,
    output = "long"
  )
  spec <- .lm_column_spec(
    lg,
    show_statistic = TRUE,
    effect_size = "f2",
    effect_size_ci = TRUE,
    r2_type = "adj_r2",
    show_weighted_n = TRUE
  )
  wide <- build_wide_display_df_continuous_lm(
    lg,
    show_statistic = TRUE,
    effect_size = "f2",
    effect_size_ci = TRUE,
    r2_type = "adj_r2",
    show_weighted_n = TRUE,
    spec = spec
  )
  raw <- build_wide_raw_continuous_lm(
    lg,
    show_statistic = TRUE,
    effect_size = "f2",
    effect_size_ci = TRUE,
    r2_type = "adj_r2",
    show_weighted_n = TRUE,
    spec = spec
  )
  keys <- .lm_spec_keys(spec)
  # The absolute order too: the spec is now the ONE place that decides
  # it, for the public frame, the typed view and every engine.
  expect_identical(
    keys,
    c(
      "Variable",
      "M (x)",
      "M (y)",
      "Δ (y - x)",
      "95% CI LL",
      "95% CI UL",
      "t(4)",
      "p",
      "Adj. R²",
      "f²",
      "effect_size_ci_lower",
      "effect_size_ci_upper",
      "n",
      "Weighted n"
    )
  )
  expect_identical(keys, names(raw))
  expect_identical(
    keys[!vapply(spec, function(e) isTRUE(e$raw_only), logical(1))],
    names(wide)
  )
  # Every displayed column carries a header equal to its key in English.
  shown <- Filter(function(e) !isTRUE(e$raw_only), spec)
  expect_identical(
    vapply(shown, function(e) e$key, character(1)),
    vapply(shown, function(e) e$label, character(1))
  )
  # `.lm_labels()` is a total function of its input: an unknown key and
  # a NULL label map both come back unchanged, so a hand-built frame
  # still exports.
  labs <- .lm_spec_labels(spec)
  expect_identical(.lm_labels("Estimate", labs), "Estimate")
  expect_identical(.lm_labels(c("Estimate", "p"), NULL), c("Estimate", "p"))
})

test_that("every frozen block identity equals its English caption", {
  # Same contract as the three descriptive blocks above, for lot D. Here
  # the key layer is a ROW identity rather than a column name: it is
  # `coefs$parent_var`, the `factor_term` column of `broom::tidy()` and
  # the `.variable` of the typed view, and nine sites match on it. Only
  # this test can see an identity part company with its caption -- the
  # identity is pinned by `expect_identical(tr$parent_var, ...)` tests,
  # the caption by the console goldens, and both stay green through a
  # drift.
  expect_setequal(.REG_BLOCK_TERMS, names(.REG_BLOCK_STR_KEYS))
  for (blk in .REG_BLOCK_TERMS) {
    expect_identical(
      .reg_block_label(blk),
      spicy_str(.REG_BLOCK_STR_KEYS[[blk]]),
      info = blk
    )
    expect_identical(.reg_block_label(blk), blk, info = blk)
  }
  # A total function of its input: a real factor variable is its own
  # caption, and so is a vector or a missing value, so every header can
  # be sent through it unguarded.
  expect_identical(.reg_block_label("education"), "education")
  expect_identical(.reg_block_label(NA_character_), NA_character_)
  expect_identical(
    .reg_block_label(c("Thresholds", "x")),
    c("Thresholds", "x")
  )

  # The two annotation templates hold their punctuation, and a `%` in
  # the DATA travels as an argument.
  expect_identical(
    .reg_factor_header_text("100%", NULL, "annotation", "a%sb"),
    "100%: [ref: a%sb]"
  )
  expect_identical(
    spicy_fmt("label_vs_annotation", "100%", "a%sb"),
    "100% [vs a%sb]"
  )
})

test_that("spicy_str() errors hard on an unknown key", {
  expect_error(spicy_str("no_such_key_exists"))
})

test_that("spicy_str() returns the raw default", {
  ks <- names(.spicy_strings)
  skip_if(length(ks) == 0L, "registry is empty")
  expect_identical(spicy_str(ks[[1L]]), unname(.spicy_strings[[1L]]))
})

test_that("every registry template is a well-formed sprintf format", {
  # A value carrying at least one conversion is a TEMPLATE: it must survive
  # `sprintf()` with the counted number of dummy arguments, and it must not
  # leave an unescaped literal `%` behind. Values with no conversion at all
  # (the bare `%` header, " (Row %)") are display literals read through
  # `spicy_str()` only and are left alone.
  # Deliberately strict: no space flag, and only the conversion letters the
  # registry actually uses. A loose pattern would read the "% C" of
  # ", 95% CI [" as a conversion and call a display literal a template.
  spec_rx <- "%(\\d+[$])?[-+#0]*[0-9]*([.][0-9]+)?[sdifeEgGxX]"
  dummy_for <- function(spec) {
    if (grepl("[dioxX]$", spec)) {
      1L
    } else if (grepl("[feEgG]$", spec)) {
      1
    } else {
      "x"
    }
  }
  for (k in names(.spicy_strings)) {
    v <- unname(.spicy_strings[[k]])
    if (!grepl("%", v, fixed = TRUE)) {
      next
    }
    all_m <- regmatches(v, gregexpr(spec_rx, v, perl = TRUE))[[1L]]
    m <- all_m[all_m != "%%"]
    if (!length(m)) {
      next
    }
    stripped <- v
    for (piece in unique(c(m, "%%"))) {
      stripped <- gsub(piece, "", stripped, fixed = TRUE)
    }
    expect_false(
      grepl("%", stripped, fixed = TRUE),
      label = sprintf("key '%s' carries an unescaped literal %%", k)
    )
    idx <- sub("^%(\\d+)[$].*$", "\\1", m)
    if (all(grepl("^[0-9]+$", idx))) {
      idx <- as.integer(idx)
      n_args <- max(idx)
      args <- lapply(seq_len(n_args), function(i) {
        hit <- which(idx == i)
        if (length(hit)) dummy_for(m[[hit[[1L]]]]) else "x"
      })
    } else {
      args <- lapply(m, dummy_for)
    }
    expect_no_error(do.call(sprintf, c(list(v), args)))
  }
})


# ---- The two structural locks the wiring fixed, pinned -------------------
# Both were latent bugs the census called out (section 4.1): display text
# was being re-read as a mechanism, and adversarial-but-legitimate inputs
# flipped the mechanism. No test covered either before the extraction.

test_that("a '%' in a variable name no longer buys cross_tab() a decimal", {
  d <- data.frame(
    `taux %` = c("a", "b", "a", "b"),
    g = c("x", "x", "y", "y"),
    check.names = FALSE
  )
  out <- suppressWarnings(cross_tab(d[["taux %"]], d$g, percent = "none"))
  # percent = 'none' means raw counts: zero decimals, whatever the
  # title happens to contain ("Crosstable: taux % x g"). The digits
  # decision reads the percent_mode KEY, never the rendered title.
  expect_identical(attr(out, "percent_mode"), "none")
  lines <- capture.output(suppressWarnings(print(out)))
  row_a <- grep("^[[:space:]]*a[[:space:]]", lines, value = TRUE)
  expect_length(row_a, 1L)
  # Counts render bare -- "1", "2" -- never "1.0" as the old
  # title-grep decision produced for a title containing "%".
  expect_false(grepl("[0-9][.][0-9]", row_a))
})

test_that("a level named 'Total' no longer erases freq()'s total rule", {
  x <- factor(c("Total", "Partial", "Total", "None"))
  lines <- capture.output(print(freq(x)))
  # The decoy: a LEVEL literally named "Total", shown in the Values
  # column (after the box-drawing bar).
  expect_true(any(grepl("│ Total", lines, fixed = TRUE)))
  # The summary row starts the line; it is unique, carries n = 4, and
  # the light rule is drawn on the line right above it -- from the
  # POSED row index, which the decoy cannot erase (the old \\bTotal\\b
  # grep found two matches and drew no rule at all).
  summary_i <- grep("^[[:space:]]*Total[[:space:]]*│", lines)
  expect_length(summary_i, 1L)
  expect_match(lines[summary_i], "4")
  expect_match(lines[summary_i - 1L], "┼", fixed = TRUE)
})
