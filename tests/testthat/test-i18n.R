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
  # Only meaningful against the sources; skipped for an installed package.
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "package sources not available")
  files <- list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE)
  files <- files[basename(files) != "i18n.R"]
  src <- paste(
    unlist(lapply(files, readLines, warn = FALSE)),
    collapse = "\n"
  )
  ks <- names(.spicy_strings)
  dead <- ks[!vapply(
    ks,
    function(k) grepl(paste0('"', k, '"'), src, fixed = TRUE),
    logical(1)
  )]
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
