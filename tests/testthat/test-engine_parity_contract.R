# Engine parity at the CONTRACT level: everything the console prints
# must reach gt / tinytable / flextable / clipboard through the
# structured view, at the same precision and with the same
# conventions. The batteries below pin the four artefacts the display
# layer used to keep to itself:
#
#   * composite cells -- the "events/N" counts of `show_columns =
#     "n_events"`, including the reference level's own counts (the
#     reference en-dash means "no estimate by design", which does not
#     apply to a count);
#   * significance stars -- the markers the footer legend documents;
#   * the absorbed fixed-effects block -- a "Fixed effects:" header
#     plus rows named as the model names them, never the internal key;
#   * the table note on a gt object -- it must survive `gtsave()`,
#     `as_raw_html()` and a non-interactive `print()`, not only the
#     interactive HTML path.
#
# Oracles are the console strings, pinned verbatim.

# Figure spaces (U+2007) are the decimal-alignment padding; every
# engine pads differently on purpose, so parity is on the content.
.ep_norm <- function(x) trimws(gsub(" ", " ", as.character(x)))

.ep_df <- function(x) {
  out <- as.data.frame(lapply(x, .ep_norm), stringsAsFactors = FALSE)
  names(out) <- names(x)
  rownames(out) <- NULL
  out
}

.ep_console <- function(tbl) {
  .ep_df(as.data.frame(tbl, stringsAsFactors = FALSE))
}
.ep_structured <- function(tbl) {
  .ep_df(spicy:::.format_structured_to_string_body(as_structured(tbl)))
}
.ep_gt <- function(g) {
  .ep_df(as.data.frame(g[["_data"]], stringsAsFactors = FALSE))
}
.ep_tt <- function(tt) .ep_df(as.data.frame(tt@data, stringsAsFactors = FALSE))
.ep_ft <- function(ft) {
  .ep_df(as.data.frame(ft$body$dataset, stringsAsFactors = FALSE))
}

# Every engine, cell for cell, against the console body. `build` takes
# an `output` string. Engines are compared on content only -- column
# NAMES differ by design (each engine moves the model prefix into its
# own spanner row).
.ep_expect_all_engines <- function(build) {
  console <- .ep_console(build("default"))
  expect_equal(unname(.ep_structured(build("default"))), unname(console))
  if (requireNamespace("gt", quietly = TRUE)) {
    expect_equal(unname(.ep_gt(build("gt"))), unname(console))
  }
  if (requireNamespace("tinytable", quietly = TRUE)) {
    expect_equal(unname(.ep_tt(build("tinytable"))), unname(console))
  }
  if (requireNamespace("flextable", quietly = TRUE)) {
    expect_equal(unname(.ep_ft(build("flextable"))), unname(console))
  }
  invisible(console)
}

.ep_data <- function() {
  d <- as.data.frame(sochealth)
  # Unordered education: per-level contrasts, so the table has
  # reference rows with their own event counts.
  d$educ <- factor(as.character(d$education), levels = levels(d$education))
  d
}

.ep_glm <- function() {
  stats::glm(
    dentist_12m ~ age + sex + educ,
    data = .ep_data(),
    family = stats::binomial()
  )
}


# ---- composite cells: events/N -------------------------------------------

test_that("n_events renders as events/N in every engine, reference rows kept", {
  fit <- .ep_glm()
  build <- function(o) {
    table_regression(fit, show_columns = c("n_events", "b", "p"), output = o)
  }
  console <- .ep_expect_all_engines(build)
  # Console oracle, pinned: the composite on every coefficient row
  # (reference levels included), the model total on the fit-stat row.
  expect_identical(
    console[["Events/N"]],
    c(
      "846/1200",
      "846/1200",
      "",
      "437/620",
      "409/580",
      "",
      "148/261",
      "365/539",
      "333/400",
      "1200",
      "0.04",
      "0.07",
      "1403.6"
    )
  )
  # The reference exemption is specific to counts: the estimate and p
  # cells of the same rows still carry the en-dash.
  ref <- which(
    console[["Variable"]] %in%
      c("Female (ref.)", "Lower secondary (ref.)")
  )
  expect_identical(console[["B"]][ref], c("–", "–"))
  expect_identical(console[["p"]][ref], c("–", "–"))
})

test_that("the structured contract carries the composite as a display override", {
  s <- as_structured(
    table_regression(.ep_glm(), show_columns = c("n_events", "b", "p"))
  )
  meta <- s$col_meta[["Events/N"]]
  expect_identical(meta$token, "n_events")
  # Typed body keeps the numerator; the override carries the string.
  expect_identical(s$body[["Events/N"]][1L], 846)
  expect_identical(meta$display_cells[1L], "846/1200")
  expect_length(meta$display_cells, nrow(s$body))
  # Reference rows: value AND override present (they were NA before).
  i_ref <- s$reference_rows[1L]
  expect_identical(s$body[["Events/N"]][i_ref], 437)
  expect_identical(meta$display_cells[i_ref], "437/620")
  # Fit-stat rows in the same column format normally (no override).
  expect_true(all(is.na(meta$display_cells[s$fit_stat_rows])))
  # Columns that need no override do not carry one.
  expect_null(s$col_meta[["B"]]$display_cells)
})

test_that("clipboard payload carries the composite counts", {
  tbl <- table_regression(.ep_glm(), show_columns = c("n_events", "b", "p"))
  payload <- spicy:::clipboard_payload(tbl, "\t")
  lines <- strsplit(payload, "\n", fixed = TRUE)[[1L]]
  cells <- strsplit(lines, "\t", fixed = TRUE)
  events <- vapply(cells, function(r) if (length(r) >= 2L) r[2L] else "", "")
  expect_true("846/1200" %in% events)
  expect_true("437/620" %in% events) # reference level's own counts
  expect_false(any(grepl("846.00", events, fixed = TRUE)))
})


# ---- significance stars ---------------------------------------------------

test_that("stars reach every engine, matching the console cell for cell", {
  fit <- stats::lm(wellbeing_score ~ age + sex + smoking, data = .ep_data())
  build <- function(o) {
    table_regression(
      fit,
      show_columns = c("b", "se", "p"),
      stars = TRUE,
      output = o
    )
  }
  console <- .ep_expect_all_engines(build)
  expect_identical(
    console[["B"]],
    c(
      "65.20***",
      "0.05",
      "",
      "–",
      "3.86***",
      "",
      "–",
      "-1.72",
      "1175",
      "0.02",
      "0.02"
    )
  )
  # Stars belong to the estimate: neither SE nor p carries one.
  expect_false(any(grepl("*", console[["SE"]], fixed = TRUE)))
  # A legend with no markers is the defect this pins: the note
  # documents the symbols, the body must contain them.
  note <- attr(build("default"), "note")
  expect_match(note, "*** p < .001", fixed = TRUE)
  payload <- spicy:::clipboard_payload(build("default"), "\t")
  expect_true(any(grepl("65.20***", payload, fixed = TRUE)))
})

test_that("stars follow the console's token rules (B, beta, AME)", {
  d <- .ep_data()
  fit <- stats::lm(wellbeing_score ~ age + sex, data = d)
  # beta is starred only when B is not displayed beside it.
  s_beta <- as_structured(table_regression(
    fit,
    show_columns = c("beta", "p"),
    standardize = "refit",
    stars = TRUE
  ))
  expect_true(any(nzchar(s_beta$stars$markers[["β"]])))
  s_both <- as_structured(table_regression(
    fit,
    show_columns = c("b", "beta", "p"),
    standardize = "refit",
    stars = TRUE
  ))
  expect_true(any(nzchar(s_both$stars$markers[["B"]])))
  expect_null(s_both$stars$markers[["β"]])
  # AME carries its own marker, on its own p-value.
  gfit <- stats::glm(
    dentist_12m ~ age + educ,
    data = d,
    family = stats::binomial()
  )
  ame <- table_regression(
    gfit,
    show_columns = c("b", "ame", "ame_p"),
    stars = TRUE
  )
  expect_equal(
    unname(.ep_structured(ame)),
    unname(.ep_console(ame))
  )
  expect_true(any(grepl("*", .ep_console(ame)[["AME"]], fixed = TRUE)))
})

test_that("the structured contract carries thresholds and per-cell markers", {
  fit <- stats::lm(wellbeing_score ~ age + sex, data = .ep_data())
  s <- as_structured(table_regression(fit, stars = TRUE))
  expect_identical(
    s$stars$thresholds,
    c("***" = 0.001, "**" = 0.01, "*" = 0.05)
  )
  expect_length(s$stars$markers[["B"]], nrow(s$body))
  expect_identical(s$stars$markers[["B"]][1L], "***")
  # Custom symbols and cutoffs travel too.
  s2 <- as_structured(table_regression(fit, stars = c("+" = 0.2)))
  expect_identical(s2$stars$thresholds, c("+" = 0.2))
  expect_true(any(s2$stars$markers[["B"]] == "+"))
  # Off by default: no component at all.
  expect_null(as_structured(table_regression(fit))$stars)
  # Requested but nothing qualified: the cutoffs still travel, because
  # the footer legend documents them either way.
  s3 <- as_structured(table_regression(
    fit,
    show_columns = c("se", "ci"),
    stars = TRUE
  ))
  expect_identical(
    s3$stars$thresholds,
    c("***" = 0.001, "**" = 0.01, "*" = 0.05)
  )
  expect_identical(s3$stars$markers, list())
})


# ---- absorbed fixed-effects block ----------------------------------------

test_that("the fixed-effects block reaches every engine as the console draws it", {
  skip_if_not_installed("fixest")
  d <- .ep_data()
  f1 <- fixest::feols(wellbeing_score ~ age + sex | region, data = d)
  f2 <- fixest::feols(
    wellbeing_score ~ age + sex | region + employment_status,
    data = d
  )
  build <- function(o) {
    table_regression(
      list(M1 = f1, M2 = f2),
      show_columns = c("b", "p"),
      show_fit_stats = c("nobs", "fixed_effects", "r2"),
      output = o
    )
  }
  console <- .ep_expect_all_engines(build)
  expect_identical(
    console[["Variable"]],
    c(
      "age",
      "sex:",
      "Female (ref.)",
      "Male",
      "n",
      "Fixed effects:",
      "region",
      "employment_status",
      "R²"
    )
  )
  expect_identical(
    console[["M1: B"]],
    c("0.04", "", "–", "3.94", "1200", "", "Yes", "No", "0.02")
  )
  # The internal key never reaches a reader, in any engine.
  for (o in c("default", "gt", "tinytable", "flextable")) {
    cells <- unlist(switch(
      o,
      default = .ep_console(build(o)),
      gt = .ep_gt(build(o)),
      tinytable = .ep_tt(build(o)),
      flextable = .ep_ft(build(o))
    ))
    expect_false(any(grepl("FE: ", cells, fixed = TRUE)))
  }
})

test_that("the fixed-effects block registers its row roles", {
  skip_if_not_installed("fixest")
  d <- .ep_data()
  f1 <- fixest::feols(wellbeing_score ~ age + sex | region, data = d)
  s <- as_structured(table_regression(
    f1,
    show_fit_stats = c("nobs", "fixed_effects")
  ))
  hdr <- which(s$body$Variable == "Fixed effects:")
  lvl <- which(trimws(s$body$Variable) == "region")
  expect_length(hdr, 1L)
  expect_true(hdr %in% s$factor_header_rows)
  expect_true(hdr %in% s$fit_stat_rows)
  expect_true(lvl %in% s$level_rows)
  # Structured and character bodies are row-aligned: the block used to
  # exist only in the character body.
  tbl <- table_regression(f1, show_fit_stats = c("nobs", "fixed_effects"))
  expect_identical(nrow(as_structured(tbl)$body), nrow(tbl))
})


# ---- table note on the gt object -----------------------------------------

test_that("the gt note survives every route to a rendered table", {
  skip_if_not_installed("gt")
  fit <- stats::lm(wellbeing_score ~ age + sex, data = .ep_data())
  g <- table_regression(fit, stars = TRUE, output = "gt")
  note_bit <- "Std. errors: classical (OLS)."
  html <- as.character(gt::as_raw_html(g, inline_css = FALSE))
  expect_true(grepl(note_bit, html, fixed = TRUE))
  # Star legend, with gt's own HTML escaping of the "<".
  expect_true(grepl("p &lt; .001", html, fixed = TRUE))
  path <- withr::local_tempfile(fileext = ".html")
  gt::gtsave(g, path)
  saved <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_true(grepl(note_bit, saved, fixed = TRUE))
  expect_true(any(grepl(
    note_bit,
    capture.output(print(g)),
    fixed = TRUE
  )))
})

test_that("the HTML display path moves the note out of the table, once", {
  skip_if_not_installed("gt")
  fit <- stats::lm(wellbeing_score ~ age + sex, data = .ep_data())
  g <- table_regression(fit, output = "gt")
  html <- as.character(gt::as_raw_html(g, inline_css = FALSE))
  out <- spicy:::.spicy_gt_html_postprocess(html, attr(g, "spicy_note"))
  expect_true(grepl("spicy-gt-note", out, fixed = TRUE))
  expect_false(grepl("<tfoot", out, fixed = TRUE))
  n_note <- lengths(regmatches(
    out,
    gregexpr("Std. errors", out, fixed = TRUE)
  ))
  expect_identical(as.integer(n_note), 1L)
  # A source note the user added themselves is left alone.
  g2 <- gt::tab_source_note(g, "My own note")
  html2 <- as.character(gt::as_raw_html(g2, inline_css = FALSE))
  out2 <- spicy:::.spicy_gt_html_postprocess(html2, attr(g, "spicy_note"))
  expect_true(grepl("My own note", out2, fixed = TRUE))
  # Nothing to strip when the table has no note: unchanged HTML.
  expect_identical(
    spicy:::.spicy_gt_drop_source_note("<table></table>", "x"),
    "<table></table>"
  )
})


# ---- contract version -----------------------------------------------------

test_that("as_structured() reports and guards the contract version", {
  tbl <- table_regression(stats::lm(mpg ~ wt, data = mtcars))
  expect_identical(as_structured(tbl)$version, 2L)
  old <- tbl
  s_old <- attr(old, "structured")
  s_old$version <- NULL
  attr(old, "structured") <- s_old
  expect_warning(as_structured(old), class = "spicy_structured_version")
  newer <- tbl
  s_new <- attr(newer, "structured")
  s_new$version <- 99L
  attr(newer, "structured") <- s_new
  expect_error(as_structured(newer), class = "spicy_invalid_input")
  # A view carrying a nonsense version reads as the oldest contract.
  broken <- tbl
  s_bad <- attr(broken, "structured")
  s_bad$version <- "two"
  attr(broken, "structured") <- s_bad
  expect_warning(as_structured(broken), class = "spicy_structured_version")
})

test_that("invariant checks cover the added per-cell components", {
  tbl <- table_regression(.ep_glm(), show_columns = c("n_events", "b"))
  s <- as_structured(tbl)
  s$col_meta[["Events/N"]]$display_cells <- "846/1200" # too short
  expect_warning(
    spicy:::.validate_structured(s),
    class = "spicy_internal_invariant"
  )
  s2 <- as_structured(table_regression(
    stats::lm(mpg ~ wt, data = mtcars),
    stars = TRUE
  ))
  s2$stars$markers[["B"]] <- c("*", "*")
  expect_warning(
    spicy:::.validate_structured(s2),
    class = "spicy_internal_invariant"
  )
})


# ---- per-category AME columns (ordinal / multinomial) ---------------------

test_that("per-category AME columns carry each category's own cell", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("marginaleffects")
  fit <- MASS::polr(
    self_rated_health ~ age + sex,
    data = sochealth,
    Hess = TRUE
  )
  tbl <- table_regression(fit, show_columns = c("b", "ame"))
  # The long frame is the oracle: one AME per (term, outcome category).
  td <- broom::tidy(tbl)
  ame_age <- td[td$estimate_type == "ame" & td$term == "age", ]
  expect_true(nrow(ame_age) >= 3L)
  s <- as_structured(tbl)
  ame_cols <- grep("^AME ", names(s$body), value = TRUE)
  expect_identical(length(ame_cols), nrow(ame_age))
  # age opens the coefficients: the first row with a value in an AME
  # column is its row.
  age_row <- which(!is.na(s$body[[ame_cols[1L]]]))[1L]
  got <- vapply(ame_cols, function(cl) s$body[[cl]][age_row], numeric(1))
  want <- ame_age$estimate[
    match(sub("^AME ", "", ame_cols), ame_age$outcome_level)
  ]
  expect_equal(unname(got), unname(want), tolerance = 1e-10)
  # The bug's signature: every category column carried the FIRST
  # category's number.
  expect_true(length(unique(got)) > 1L)
  # And the structured strings match the console cell for cell.
  expect_equal(unname(.ep_structured(tbl)), unname(.ep_console(tbl)))
})
