# Coverage-gap tests (group g07_dispatch_render):
#   * regression_dispatch.R -- clipboard dispatch arm, interactive
#     gt / flextable display paths, Excel outcome-row overlay and
#     reference-row model skip.
#   * regression_render.R -- missing outcome_level column fallback,
#     outcome-row shift of the thresholds separator, subordinate-block
#     label fallback, blank n_groups cell for non-mixed models.
#   * regression_transform.R -- p_adjust empty-family / NA-type guards,
#     keep/drop passthrough on empty aligned input.
#   * tables_ascii.R -- zero-width spanner skip, over-wide spanner
#     truncation, per-panel spanner drop.

# Run an expression with clipr's availability + write_clip mocked in the
# clipr namespace (same pattern as test-copy_clipboard.R) so the
# clipboard dispatch arm is reachable on a headless runner without
# touching the real system clipboard.
with_clipr_capture <- function(code, sink_env) {
  ns <- asNamespace("clipr")
  old_available <- get("clipr_available", envir = ns)
  old_write <- get("write_clip", envir = ns)

  unlockBinding("clipr_available", ns)
  unlockBinding("write_clip", ns)
  assign("clipr_available", function(...) TRUE, envir = ns)
  assign("write_clip", function(content, ...) {
    sink_env$payload <- content
    invisible(content)
  }, envir = ns)
  lockBinding("clipr_available", ns)
  lockBinding("write_clip", ns)

  on.exit(
    {
      unlockBinding("clipr_available", ns)
      unlockBinding("write_clip", ns)
      assign("clipr_available", old_available, envir = ns)
      assign("write_clip", old_write, envir = ns)
      lockBinding("clipr_available", ns)
      lockBinding("write_clip", ns)
    },
    add = TRUE
  )

  eval(substitute(code), envir = parent.frame())
}

# Return a copy of `fn` whose lexical scope resolves `interactive` to
# TRUE. Direct namespace-shadowing (a child env of the function's own
# enclosure) engages regardless of byte-compilation or testthat mock
# support for base bindings, which with_mocked_bindings(.package =
# "base") lacks on some platforms.
as_interactive <- function(fn) {
  e <- new.env(parent = environment(fn))
  assign("interactive", function() TRUE, envir = e)
  environment(fn) <- e
  fn
}


# ---------------------------------------------------------------------------
# regression_dispatch.R line 56: clipboard dispatch arm
# ---------------------------------------------------------------------------

test_that("output = 'clipboard' dispatches to the clipr writer with the TSV payload", {
  skip_if_not_installed("clipr")
  fit <- lm(mpg ~ wt, data = mtcars)
  sink_env <- new.env(parent = emptyenv())

  ret <- with_clipr_capture(
    table_regression(fit, output = "clipboard"),
    sink_env = sink_env
  )

  # The payload handed to clipr::write_clip() is exactly the TSV built by
  # clipboard_payload() on the same rendered table (oracle-pinned).
  tab <- table_regression(fit)
  expect_identical(sink_env$payload, spicy:::clipboard_payload(tab, "\t"))

  # Spot-check content: header row and the wt coefficient (B = -5.34,
  # round(coef(fit)[["wt"]], 2)) are tab-separated in the payload.
  expect_match(sink_env$payload, "Variable\tB\tSE", fixed = TRUE)
  expect_match(sink_env$payload, "wt\t-5.34\t0.56", fixed = TRUE)

  # output_clipboard() returns the rendered table invisibly.
  expect_s3_class(ret, "data.frame")
  expect_true("Variable" %in% names(ret))
})


# ---------------------------------------------------------------------------
# regression_dispatch.R lines 1109-1112: print.spicy_gt interactive arm
# ---------------------------------------------------------------------------

test_that("print.spicy_gt interactive arm displays post-processed browsable HTML", {
  skip_if_not_installed("gt")
  skip_if_not_installed("htmltools")

  mt <- mtcars
  mt$cyl <- factor(mt$cyl)
  fit <- lm(mpg ~ wt + cyl, data = mt)
  g <- table_regression(fit, output = "gt", note = "Note. interactive gt path.")

  # Capture the HTML file that htmltools::html_print() hands to the viewer
  # instead of opening a browser.
  viewed <- NULL
  withr::local_options(viewer = function(url, ...) {
    viewed <<- url
    invisible(url)
  })

  res <- as_interactive(spicy:::print.spicy_gt)(g)

  # The interactive arm returns invisible(NULL) (the NextMethod() arm
  # would return the gt object).
  expect_null(res)

  # The viewer received a real HTML file containing the POST-PROCESSED
  # markup: the .spicy-gt-outer wrapper and the escaped note div are
  # injected by .spicy_gt_html_postprocess(), so their presence proves
  # the interactive branch ran end-to-end (as_raw_html -> postprocess ->
  # browsable print).
  expect_false(is.null(viewed))
  expect_true(file.exists(viewed))
  html <- paste(readLines(viewed, warn = FALSE), collapse = "\n")
  expect_match(html, "spicy-gt-outer", fixed = TRUE)
  # The note prefix "Note." is emitted as its own italic chunk, so match
  # the body text only.
  expect_match(html, "interactive gt path.", fixed = TRUE)
  expect_match(html, "<table", fixed = TRUE)
})


# ---------------------------------------------------------------------------
# regression_dispatch.R lines 1574-1578: print.spicy_flextable interactive arm
# ---------------------------------------------------------------------------

test_that("print.spicy_flextable interactive arm displays post-processed browsable HTML", {
  skip_if_not_installed("flextable")
  skip_if_not_installed("htmltools")

  mt <- mtcars
  mt$cyl <- factor(mt$cyl)
  fit <- lm(mpg ~ wt + cyl, data = mt)
  ft <- table_regression(fit, output = "flextable",
                         note = "Note. interactive ft path.")

  viewed <- NULL
  withr::local_options(viewer = function(url, ...) {
    viewed <<- url
    invisible(url)
  })

  res <- as_interactive(spicy:::print.spicy_flextable)(ft)

  expect_null(res)
  expect_false(is.null(viewed))
  expect_true(file.exists(viewed))
  html <- paste(readLines(viewed, warn = FALSE), collapse = "\n")
  # .spicy_ft_html_postprocess() markers: the note div and the
  # border-collapse CSS block are only present on the interactive path.
  expect_match(html, "spicy-ft-note", fixed = TRUE)
  # The note prefix "Note." is emitted as its own italic chunk
  # (<em>Note.</em>), so match the body text only.
  expect_match(html, "interactive ft path.", fixed = TRUE)
  expect_match(html, "border-collapse: collapse", fixed = TRUE)
})


# ---------------------------------------------------------------------------
# regression_dispatch.R lines 1781-1786 (Excel outcome-row overlay) and
# line 1796 (reference-row skip for models without the factor)
# ---------------------------------------------------------------------------

test_that("excel: outcome row is overlaid and reference dashes skip models without the factor", {
  skip_if_not_installed("openxlsx2")

  mt <- mtcars
  mt$cyl <- factor(mt$cyl)
  m1 <- lm(mpg ~ wt + cyl, data = mt)  # has the cyl factor -> reference row
  m2 <- lm(mpg ~ wt, data = mt)        # no factor
  path <- withr::local_tempfile(fileext = ".xlsx")

  table_regression(list(m1, m2), outcome_labels = c("MPG a", "MPG b"),
                   output = "excel", excel_path = path)

  wb <- openxlsx2::wb_load(path)
  cells <- openxlsx2::wb_to_df(wb, sheet = 1, col_names = FALSE)

  # --- Outcome row: label text lands in each model's FIRST sub-column ----
  orow <- which(!is.na(cells$A) & trimws(cells$A) == "Outcome")
  expect_length(orow, 1L)
  expect_identical(cells$B[orow], "MPG a")   # model 1 first sub-column (B)
  expect_identical(cells$E[orow], "MPG b")   # model 2 first sub-column (B)
  # Non-first sub-columns stay empty on the Outcome row.
  expect_true(is.na(cells$C[orow]))
  expect_true(is.na(cells$F[orow]))

  # --- Reference row: en-dash only under the model that HAS the factor ---
  rrow <- which(!is.na(cells$A) & grepl("4 \\(ref\\.\\)$", trimws(cells$A)))
  expect_length(rrow, 1L)
  expect_identical(cells$B[rrow], "\u2013")  # model 1: en-dash marker
  # Model 2 has no cyl term: its cells stay blank (line 1796 skip).
  expect_true(is.na(cells$E[rrow]))
  expect_true(is.na(cells$F[rrow]))
  expect_true(is.na(cells$G[rrow]))

  # Numeric oracle: model 1 intercept round-trips at full precision.
  irow <- which(!is.na(cells$A) & trimws(cells$A) == "(Intercept)")
  expect_length(irow, 1L)
  expect_equal(as.numeric(cells$B[irow]), unname(coef(m1)[1]),
               tolerance = 1e-10)
})


# ---------------------------------------------------------------------------
# regression_render.R line 304: outcome row shifts the thresholds separator
# ---------------------------------------------------------------------------

test_that("ordinal: explicit outcome_labels shift the Thresholds separator down one row", {
  skip_if_not_installed("MASS")

  h <- MASS::housing
  m1 <- MASS::polr(Sat ~ Infl, weights = Freq, data = h, Hess = TRUE)
  m2 <- MASS::polr(Sat ~ Cont, weights = Freq, data = h, Hess = TRUE)

  t_plain <- table_regression(list(m1, m2))
  t_out <- table_regression(list(m1, m2), outcome_labels = c("Sat A", "Sat B"))

  # Without outcome_labels there is no Outcome row.
  expect_false(any(trimws(t_plain$Variable) == "Outcome"))

  # With explicit outcome_labels the Outcome row is prepended ...
  expect_identical(trimws(t_out$Variable[1]), "Outcome")
  expect_identical(trimws(t_out[1, "Model 1: B"]), "Sat A")
  expect_identical(trimws(t_out[1, "Model 2: B"]), "Sat B")

  # ... and the Thresholds section separator moves down by exactly 1,
  # still pointing at the Thresholds block header.
  sep_plain <- attr(t_plain, "section_sep_rows")
  sep_out <- attr(t_out, "section_sep_rows")
  expect_length(sep_plain, 1L)
  expect_identical(sep_out, sep_plain + 1L)
  expect_match(t_plain$Variable[sep_plain], "^Thresholds")
  expect_match(t_out$Variable[sep_out], "^Thresholds")
})


# ---------------------------------------------------------------------------
# regression_render.R line 980: blank n_groups cell for non-mixed models
# ---------------------------------------------------------------------------

test_that("n_groups fit-stat row leaves a blank cell under a non-mixed model", {
  skip_if_not_installed("lme4")

  sl <- lme4::sleepstudy
  mm <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = sl)
  ml <- lm(Reaction ~ Days, data = sl)

  tb <- table_regression(list(mm, ml),
                         show_fit_stats = c("nobs", "n_groups"),
                         output = "data.frame")

  # Shared single grouping factor -> dynamic "N (Subject)" label.
  ngrow <- which(trimws(tb$Variable) == "N (Subject)")
  expect_length(ngrow, 1L)
  # Mixed model: numeric group count (oracle: 18 subjects in sleepstudy).
  expect_identical(trimws(tb[ngrow, "Model 1: B"]),
                   as.character(length(unique(sl$Subject))))
  # lm has no grouping structure: the cell is blank, not "0" or "NA".
  expect_identical(trimws(tb[ngrow, "Model 2: B"]), "")
  expect_identical(trimws(tb[ngrow, "Model 2: SE"]), "")
  expect_identical(trimws(tb[ngrow, "Model 2: p"]), "")
})


# ---------------------------------------------------------------------------
# regression_render.R line 141: coefs without an outcome_level column
# ---------------------------------------------------------------------------

test_that("render tolerates aligned coefs lacking the outcome_level column", {
  mt <- mtcars
  mt$cyl <- factor(mt$cyl)
  fit <- lm(mpg ~ wt + cyl, data = mt)
  fr <- spicy:::as_regression_frame(fit, model_id = "m1",
                                    show_columns = c("b", "se", "ci", "p"))
  aligned <- spicy:::align_frames(list(fr), model_ids = "m1")
  expect_true("outcome_level" %in% names(aligned$coefs_aligned))
  rt_with <- spicy:::render_regression_table(aligned)

  # Dropping the column (all-NA for a single-outcome lm) must yield the
  # exact same rendered table: the per-category AME pivot treats a
  # missing column and an all-NA column identically.
  aligned2 <- aligned
  aligned2$coefs_aligned$outcome_level <- NULL
  rt_without <- spicy:::render_regression_table(aligned2)
  expect_identical(rt_without, rt_with)
})


# ---------------------------------------------------------------------------
# regression_render.R line 836: subordinate-block label fallback to term
# ---------------------------------------------------------------------------

test_that("subordinate-block rows with NA/empty factor_level fall back to the term", {
  tr <- data.frame(term = "1|2", is_intercept = FALSE, is_reference = FALSE,
                   factor_term = "Thresholds", factor_level = NA_character_,
                   stringsAsFactors = FALSE)
  # Grouped layout: two-space indent + term used as the display label.
  expect_identical(
    spicy:::format_term_label(tr, "(ref.)", "label", TRUE, NULL),
    "  1|2"
  )
  # Flat layout: bare term.
  expect_identical(
    spicy:::format_term_label(tr, "(ref.)", "label", FALSE, NULL),
    "1|2"
  )
  # Empty-string level triggers the same fallback via !nzchar().
  tr2 <- tr
  tr2$factor_level <- ""
  expect_identical(
    spicy:::format_term_label(tr2, "(ref.)", "label", TRUE, NULL),
    "  1|2"
  )
})


# ---------------------------------------------------------------------------
# regression_transform.R line 26: p_adjust with an empty adjustment family
# ---------------------------------------------------------------------------

test_that("p_adjust on an intercept-only model is a documented no-op", {
  fit0 <- lm(mpg ~ 1, data = mtcars)
  t_adj <- table_regression(fit0, p_adjust = "holm", output = "data.frame")
  t_raw <- table_regression(fit0, output = "data.frame")

  # Intercepts are excluded from the family -> zero adjustable p-values
  # -> every displayed cell is untouched.
  for (nm in names(t_raw)) {
    expect_identical(t_adj[[nm]], t_raw[[nm]])
  }
  # Oracle: intercept p ~ 1.5e-18 < .001 in both tables.
  expect_lt(summary(fit0)$coefficients["(Intercept)", "Pr(>|t|)"], 0.001)
  expect_identical(trimws(t_adj$p[1]), "<.001")
  # The footer still documents the (empty) adjustment family.
  expect_match(attr(t_adj, "note"), "m = 0 coefficient", fixed = TRUE)
})


# ---------------------------------------------------------------------------
# regression_transform.R line 30: NA estimate_type family is skipped
# ---------------------------------------------------------------------------

test_that("apply_p_adjust_to_frame_coefs skips rows whose estimate_type is NA", {
  coefs <- data.frame(
    term = c("x1", "x2", "z"),
    label = c("x1", "x2", "z"),
    estimate_type = c("B", "B", NA),
    p_value = c(0.04, 0.03, 0.02),
    is_ref = FALSE,
    stringsAsFactors = FALSE
  )
  res <- spicy:::apply_p_adjust_to_frame_coefs(coefs, "holm")
  # The two B-family p-values are Holm-adjusted (oracle: stats::p.adjust);
  # the NA-type row matches no family (`which()` comes back empty) and its
  # p-value is passed through untouched.
  expected <- c(stats::p.adjust(c(0.04, 0.03), method = "holm"), 0.02)
  expect_identical(res$p_value, expected)
  # Everything except p_value is untouched.
  keep <- setdiff(names(coefs), "p_value")
  expect_identical(res[keep], coefs[keep])
})


# ---------------------------------------------------------------------------
# regression_transform.R line 59: keep/drop passthrough on empty aligned
# ---------------------------------------------------------------------------

test_that("apply_keep_drop_filter passes empty aligned objects through unchanged", {
  al_null <- list(coefs_aligned = NULL, term_order = character(0))
  expect_identical(
    spicy:::apply_keep_drop_filter(al_null, keep = "wt"),
    al_null
  )
  al_zero <- list(coefs_aligned = data.frame(term = character(0)),
                  term_order = character(0))
  expect_identical(
    spicy:::apply_keep_drop_filter(al_zero, drop = "wt"),
    al_zero
  )
})


# ---------------------------------------------------------------------------
# tables_ascii.R line 387: zero-width spanner span is skipped
# ---------------------------------------------------------------------------

test_that("build_ascii_table skips a spanner over a zero-width column", {
  # padding = 0 plus an all-empty column (empty header AND cells) gives a
  # zero-width cell region, so the spanned range is empty and the label
  # is dropped rather than mis-drawn or erroring.
  df <- data.frame(a = c("x", "y"), b = c("", ""), stringsAsFactors = FALSE)
  names(df) <- c("a", "")
  out <- build_ascii_table(df, padding = 0L, spanners = list(S = 2L))
  lines <- strsplit(out, "\n", fixed = TRUE)[[1]]

  # Spanner + underline rows are emitted but blank; the label never shows.
  expect_false(grepl("S", out, fixed = TRUE))
  expect_identical(trimws(lines[1]), "")
  expect_identical(trimws(lines[2]), "")
  # The rest of the table renders normally.
  expect_match(lines[3], "a", fixed = TRUE)
  expect_match(out, "x", fixed = TRUE)
})


# ---------------------------------------------------------------------------
# tables_ascii.R line 389: over-wide spanner labels are truncated
# ---------------------------------------------------------------------------

test_that("build_ascii_table truncates a spanner label wider than its span", {
  df <- data.frame(Variable = "a", B = "1.0", p = ".04",
                   stringsAsFactors = FALSE)
  lbl <- "An extremely long model label"
  out <- build_ascii_table(df, spanners = setNames(list(2:3), lbl),
                           align_left_cols = 1L)
  lines <- strsplit(out, "\n", fixed = TRUE)[[1]]

  # Span width = length of the underline rule under the spanner.
  span_w <- sum(strsplit(lines[2], "", fixed = TRUE)[[1]] == "\u2500")
  expect_gt(span_w, 0L)
  expect_lt(span_w, nchar(lbl))

  # The label is cut to the span width with a VISIBLE ellipsis marker
  # (2026-07-09 polish: a silent cut read as a complete, wrong
  # label); the full label is absent.
  expect_identical(trimws(lines[1]),
                   paste0(substr(lbl, 1, span_w - 1L), "…"))
  expect_false(grepl(lbl, out, fixed = TRUE))
})


# ---------------------------------------------------------------------------
# tables_ascii.R line 735: panels whose columns lose every spanner
# ---------------------------------------------------------------------------

test_that("spicy_print_table drops the spanner row in panels without spanned columns", {
  withr::local_options(width = 40)
  df <- data.frame(
    Variable = c("aaaaaaaaaa", "bbbbbbbbbb"),
    C1 = c("111111111", "222222222"),
    C2 = c("333333333", "444444444"),
    C3 = c("555555555", "666666666"),
    C4 = c("777777777", "888888888"),
    stringsAsFactors = FALSE
  )
  out <- capture.output(
    spicy_print_table(df, spanners = list(G1 = 2:3), align_left_cols = 1L)
  )

  # Four one-data-column panels (each header repeats "Variable").
  expect_identical(sum(grepl("Variable", out, fixed = TRUE)), 4L)
  # The G1 spanner reappears only in the two panels that contain one of
  # its columns (C1, C2); the C3 / C4 panels have no spanner row at all.
  expect_identical(sum(grepl("G1", out, fixed = TRUE)), 2L)
  # Exactly two spanner underlines (pure U+2500 runs, no header-rule
  # junction U+253C) -- one per surviving spanner.
  underlines <- grepl("^\\s*\u2500+\\s*$", out)
  expect_identical(sum(underlines), 2L)
})
