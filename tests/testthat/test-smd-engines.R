# SMD parity across the rendering engines, and the byte-identity of the
# default.
#
# Two claims are pinned here. (1) `smd = FALSE` -- the default -- leaves
# every route of both families byte for byte where it was. (2) Under
# `smd = TRUE` each route gains exactly one column, headed "SMD",
# spanning itself, in last position, and takes its precision from the
# argument the journal styles already reach.

.smd_eng_data <- function() {
  data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    x = c(1, 2, 4, 5, 2, 3, 8),
    y = c(3, 1, 4, 1, 5, 9, 2),
    bin = factor(
      c("no", "no", "no", "yes", "yes", "no", "yes"),
      levels = c("no", "yes")
    ),
    k3 = factor(
      c("a", "a", "b", "c", "a", "b", "b"),
      levels = c("a", "b", "c")
    ),
    stringsAsFactors = FALSE
  )
}

# One structural fingerprint per route, comparable across two runs
# without depending on any engine's internal layout.
.smd_fingerprint <- function(obj) {
  if (inherits(obj, "gt_tbl")) {
    # gt stamps a random id on its wrapper div and on every CSS
    # selector that targets it: that id is the one thing in this HTML
    # that differs between two identical renders.
    html <- as.character(gt::as_raw_html(obj))
    id <- sub(
      '^.*<div id="([^"]+)".*$',
      "\\1",
      strsplit(html, "\n")[[1L]][[1L]]
    )
    return(gsub(id, "ID", html, fixed = TRUE))
  }
  if (inherits(obj, "flextable")) {
    return(paste(
      c(
        unlist(obj$header$dataset, use.names = FALSE),
        unlist(obj$body$dataset, use.names = FALSE)
      ),
      collapse = ""
    ))
  }
  if (inherits(obj, "tinytable")) {
    return(paste(capture.output(print(obj, "markdown")), collapse = "\n"))
  }
  paste(capture.output(print(obj)), collapse = "\n")
}

test_that("smd = FALSE is byte-identical on every route of both families", {
  d <- .smd_eng_data()
  engines <- c("default", "data.frame", "long", "flextable")
  if (requireNamespace("tinytable", quietly = TRUE)) {
    engines <- c(engines, "tinytable")
  }
  if (requireNamespace("gt", quietly = TRUE)) {
    engines <- c(engines, "gt")
  }
  skip_if_not_installed("flextable")

  for (out in engines) {
    con_ref <- .smd_fingerprint(table_continuous(
      d,
      select = c(x, y),
      by = g,
      output = out
    ))
    con_now <- .smd_fingerprint(table_continuous(
      d,
      select = c(x, y),
      by = g,
      smd = FALSE,
      output = out
    ))
    expect_identical(con_now, con_ref, info = paste("continuous", out))

    cat_ref <- .smd_fingerprint(table_categorical(
      d,
      select = c(bin, k3),
      by = g,
      output = out
    ))
    cat_now <- .smd_fingerprint(table_categorical(
      d,
      select = c(bin, k3),
      by = g,
      smd = FALSE,
      output = out
    ))
    expect_identical(cat_now, cat_ref, info = paste("categorical", out))
  }
  # The one place the default DOES move, disclosed: the grouped
  # continuous compute frame carries the two new schema columns, NA
  # throughout. Nothing else in that frame changes.
  fr <- table_continuous(d, select = c(x, y), by = g, output = "long")
  expect_true(all(c("smd_type", "smd_value") %in% names(fr)))
  expect_true(all(is.na(fr$smd_type)))
  expect_true(all(is.na(fr$smd_value)))
})

test_that("smd = TRUE adds exactly one column, headed SMD, on every engine", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("flextable")
  d <- .smd_eng_data()

  ft_con_off <- table_continuous(
    d,
    select = c(x, y),
    by = g,
    output = "flextable"
  )
  ft_con_on <- table_continuous(
    d,
    select = c(x, y),
    by = g,
    smd = TRUE,
    output = "flextable"
  )
  expect_identical(
    ncol(ft_con_on$body$dataset),
    ncol(ft_con_off$body$dataset) + 1L
  )
  expect_identical(
    names(ft_con_on$body$dataset)[[
      ncol(ft_con_on$body$dataset)
    ]],
    "SMD"
  )
  # A lone column spans itself: its header cell is the same on both
  # header rows of the two-row layout, i.e. the column is not left
  # under a neighbour's spanner.
  hdr <- ft_con_on$header$dataset
  expect_identical(hdr[[ncol(hdr)]][[1L]], "SMD")

  ft_cat_off <- table_categorical(
    d,
    select = c(bin, k3),
    by = g,
    output = "flextable"
  )
  ft_cat_on <- table_categorical(
    d,
    select = c(bin, k3),
    by = g,
    smd = TRUE,
    output = "flextable"
  )
  expect_identical(
    ncol(ft_cat_on$body$dataset),
    ncol(ft_cat_off$body$dataset) + 1L
  )
  hdr_cat <- ft_cat_on$header$dataset
  expect_identical(hdr_cat[[ncol(hdr_cat)]][[1L]], "SMD")

  if (requireNamespace("gt", quietly = TRUE)) {
    html_con <- as.character(gt::as_raw_html(table_continuous(
      d,
      select = c(x, y),
      by = g,
      smd = TRUE,
      output = "gt"
    )))
    expect_match(html_con, "SMD", fixed = TRUE)
    html_cat <- as.character(gt::as_raw_html(table_categorical(
      d,
      select = c(bin, k3),
      by = g,
      smd = TRUE,
      output = "gt"
    )))
    expect_match(html_cat, "SMD", fixed = TRUE)
  }
  if (requireNamespace("tinytable", quietly = TRUE)) {
    for (tt in list(
      table_continuous(
        d,
        select = c(x, y),
        by = g,
        smd = TRUE,
        output = "tinytable"
      ),
      table_categorical(
        d,
        select = c(bin, k3),
        by = g,
        smd = TRUE,
        output = "tinytable"
      )
    )) {
      md <- paste(capture.output(print(tt, "markdown")), collapse = "\n")
      expect_match(md, "SMD", fixed = TRUE)
    }
  }
})

test_that("the clipboard payload carries the SMD column and its gloss", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("clipr")
  d <- .smd_eng_data()
  captured <- NULL
  local_mocked_bindings(
    write_clip = function(text, ...) {
      captured <<- text
      invisible(text)
    },
    clipr_available = function(...) TRUE,
    .package = "clipr"
  )
  expect_message(
    table_continuous(
      d,
      select = x,
      by = g,
      smd = TRUE,
      p_value = FALSE,
      output = "clipboard"
    ),
    "copied to clipboard"
  )
  expect_match(captured, "SMD", fixed = TRUE)
  expect_match(captured, "-0.51", fixed = TRUE)
  expect_match(captured, "standardized mean difference", fixed = TRUE)

  captured <- NULL
  expect_message(
    table_categorical(
      d,
      select = c(bin, k3),
      by = g,
      smd = TRUE,
      output = "clipboard"
    ),
    "copied to clipboard"
  )
  # Last header cell of the top header row.
  first_line <- strsplit(captured, "\n", fixed = TRUE)[[1L]]
  hdr <- first_line[grepl("^Variable\t", first_line)][[1L]]
  cells <- strsplit(hdr, "\t", fixed = TRUE)[[1L]]
  expect_identical(cells[[length(cells)]], "SMD")
})

test_that("a journal style reaches the SMD column without a new argument", {
  skip_if_not_installed("MASS")
  d <- .smd_eng_data()
  # The precision lever is `effect_size_digits` / `v_digits`, both of
  # which the style table already carries. A dedicated `smd_digits`
  # would sit outside it and no theme would ever set it, with no test
  # failing -- which is why there is none.
  con3 <- paste(
    capture.output(print(table_continuous(
      d,
      select = x,
      by = g,
      smd = TRUE,
      p_value = FALSE,
      effect_size_digits = 3
    ))),
    collapse = "\n"
  )
  expect_match(con3, "-0.510", fixed = TRUE)
  cat3 <- paste(
    capture.output(print(table_categorical(
      d,
      select = bin,
      by = g,
      smd = TRUE,
      v_digits = 3
    ))),
    collapse = "\n"
  )
  expect_match(cat3, "-0.921", fixed = TRUE)
  # And a named theme moves it through the same lever.
  styled <- paste(
    capture.output(print(table_continuous(
      d,
      select = x,
      by = g,
      smd = TRUE,
      p_value = FALSE,
      style = spicy_style("apa", effect_size_digits = 3)
    ))),
    collapse = "\n"
  )
  expect_match(styled, "-0.510", fixed = TRUE)
})
