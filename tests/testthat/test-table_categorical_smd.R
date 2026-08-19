# `smd = TRUE` in table_categorical() -- the Table 1 balance column.
#
# The kernels and their oracles live in test-smd.R. This file pins the
# COLUMN through the nine sites a trailing column has to reach in this
# family: the long frame, the wide frame, the report frame's two row
# kinds, the typed view, glance()'s fallback, and the five engines whose
# geometry is written from the RIGHT.

.smd_cat_data <- function() {
  data.frame(
    g = factor(c("A", "A", "A", "A", "B", "B", "B"), levels = c("A", "B")),
    bin = factor(
      c("no", "no", "no", "yes", "yes", "no", "yes"),
      levels = c("no", "yes")
    ),
    k3 = factor(
      c("a", "a", "b", "c", "a", "b", "b"),
      levels = c("a", "b", "c")
    ),
    w = c(1, 2, 1, 3, 2, 1, 1),
    stringsAsFactors = FALSE
  )
}

test_that("the categorical SMD reproduces tableone on both kernels", {
  skip_if_not_installed("MASS")
  d <- .smd_cat_data()
  r <- table_categorical(
    d,
    select = c(bin, k3),
    by = g,
    smd = TRUE,
    output = "long"
  )
  per_var <- r[!duplicated(r$variable), , drop = FALSE]
  # tableone 0.13.2 `ExtractSmd`: bin 0.9205746178983232885784,
  # k3 1.1126972805283739109683. The binary sign is ours (A - B, the
  # display order); tableone publishes magnitudes.
  expect_equal(
    per_var$smd[[1L]],
    -0.9205746178983232885784,
    tolerance = 1e-15
  )
  expect_equal(
    per_var$smd[[2L]],
    1.1126972805283739109683,
    tolerance = 1e-15
  )
  # `smd_type` names the kernel, hence whether the value is signed.
  expect_identical(per_var$smd_type, c("binary", "multinomial"))
  expect_lt(per_var$smd[[1L]], 0)
  expect_gt(per_var$smd[[2L]], 1)
  # The value lives on the VARIABLE, so every row of a block repeats
  # it -- like `chi2`, `p` and the association measure beside it.
  expect_true(all(r$smd[r$variable == "bin"] == per_var$smd[[1L]]))
})

test_that("the weighted categorical SMD is the SMD of the expanded data", {
  skip_if_not_installed("MASS")
  d <- .smd_cat_data()
  rw <- table_categorical(
    d,
    select = c(bin, k3),
    by = g,
    smd = TRUE,
    weights = w,
    output = "long"
  )
  wv <- rw$smd[!duplicated(rw$variable)]
  # tableone on the survey design gives 0.6912858353783117859592 and
  # 1.3601470508735444830961: the categorical arms do NOT diverge
  # between the frequency and the design conventions, because the
  # Bernoulli and multinomial variances are functions of the weighted
  # proportion alone. Only the continuous arm parts company.
  expect_equal(wv[[1L]], -0.6912858353783118969815, tolerance = 1e-15)
  expect_equal(wv[[2L]], 1.3601470508735444830961, tolerance = 1e-15)
  dup <- d[rep(seq_len(nrow(d)), d$w), , drop = FALSE]
  rd <- table_categorical(
    dup,
    select = c(bin, k3),
    by = g,
    smd = TRUE,
    output = "long"
  )
  expect_equal(wv, rd$smd[!duplicated(rd$variable)], tolerance = 1e-15)
  # A profile of proportions is invariant to a global rescaling of the
  # weights, so `rescale` cannot move this column -- unlike the
  # continuous one, where it moves the variance.
  rr <- table_categorical(
    d,
    select = c(bin, k3),
    by = g,
    smd = TRUE,
    weights = w,
    rescale = TRUE,
    output = "long"
  )
  expect_equal(rr$smd[!duplicated(rr$variable)], wv, tolerance = 1e-15)
})

test_that("the SMD sits on the variable row, never on a level row", {
  skip_if_not_installed("MASS")
  d <- .smd_cat_data()
  tb <- table_categorical(d, select = c(bin, k3), by = g, smd = TRUE)
  st <- as_structured(tb)
  hdr <- st$body$.row_role == "factor_header"
  expect_true(all(!is.na(st$body[[.CAT_KEY_SMD]][hdr])))
  expect_true(all(is.na(st$body[[.CAT_KEY_SMD]][!hdr])))
  # The margin is a column, never a group of the diagnostic: no
  # `total = TRUE` column carries an SMD, and the SMD column is not one.
  smd_meta <- st$col_meta[[.CAT_KEY_SMD]]
  expect_identical(smd_meta$token, "smd")
  expect_identical(smd_meta$display_label, "SMD")
  expect_null(smd_meta$total)
  # No `p_style`, no `value_range`: unlike the bounded association
  # measure beside it, a k-level SMD is a distance and is unbounded.
  expect_null(smd_meta$p_style)
  expect_null(smd_meta$value_range)
  # ... and the printed cell agrees: the leading zero is KEPT, where
  # the association cell strips it (a visible, accepted difference).
  txt <- paste(capture.output(print(tb)), collapse = "\n")
  expect_match(txt, "-0.92", fixed = TRUE)
  expect_false(grepl(" -.92", txt, fixed = TRUE))
  expect_match(txt, "SMD = standardized mean difference (A - B)", fixed = TRUE)
  # The unsigned sentence appears because `k3` has three categories.
  expect_match(txt, "is therefore unsigned", fixed = TRUE)
  bin_only <- paste(
    capture.output(print(table_categorical(
      d,
      select = bin,
      by = g,
      smd = TRUE
    ))),
    collapse = "\n"
  )
  expect_false(grepl("is therefore unsigned", bin_only, fixed = TRUE))
})

test_that("the raw frames name the SMD by their own convention", {
  skip_if_not_installed("MASS")
  d <- .smd_cat_data()
  # `long` mirrors `effect_size` / `effect_size_type` beside it.
  lg <- table_categorical(d, select = bin, by = g, smd = TRUE, output = "long")
  expect_true(all(c("smd", "smd_type") %in% names(lg)))
  # `data.frame` is the WIDE frame, which carries display names
  # ("Phi", never `effect_size`), so the SMD enters under its frozen key.
  wd <- table_categorical(
    d,
    select = bin,
    by = g,
    smd = TRUE,
    output = "data.frame"
  )
  expect_true(.CAT_KEY_SMD %in% names(wd))
  expect_equal(
    wd[[.CAT_KEY_SMD]][[1L]],
    -0.92057461789832329,
    tolerance = 1e-15
  )
  # This family DROPS the column when it is not asked for -- its own
  # rule for `.assoc`, and the opposite of the continuous frame's.
  expect_false(any(
    c("smd", "smd_type") %in%
      names(table_categorical(
        d,
        select = bin,
        by = g,
        output = "long"
      ))
  ))
  expect_false(
    .CAT_KEY_SMD %in%
      names(table_categorical(
        d,
        select = bin,
        by = g,
        output = "data.frame"
      ))
  )
})

test_that("glance() never publishes the SMD as an association measure", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("broom")
  d <- .smd_cat_data()
  # With no association measure asked for, the SMD is the only extra
  # column in `long_data`. If it were missing from `std_cols`, the
  # degraded-object fallback would take it as the measure.
  tb <- table_categorical(
    d,
    select = bin,
    by = g,
    smd = TRUE,
    assoc_measure = "none"
  )
  bare <- tb
  attr(bare, "structured") <- NULL
  gl <- broom::glance(bare)
  expect_true(all(is.na(gl$assoc_value)))
  expect_true(all(is.na(gl$assoc_type)))
})

test_that("the SMD refuses more than two groups, on the REAL groups only", {
  skip_if_not_installed("MASS")
  d <- .smd_cat_data()
  d3 <- d
  d3$g <- factor(
    c("A", "A", "A", "B", "B", "B", "C"),
    levels = c("A", "B", "C")
  )
  msg <- tryCatch(
    table_categorical(d3, select = bin, by = g, smd = TRUE),
    spicy_not_implemented = function(e) conditionMessage(e)
  )
  expect_match(
    msg,
    "requires exactly two groups in `by` (found 3)",
    fixed = TRUE
  )
  # The four combinations of the two pseudo-level traps. `include_total`
  # is TRUE by default, so a naive count is off by one on EVERY grouped
  # table; a missing `by` adds a second phantom under `drop_na = FALSE`.
  # All four must COMPUTE, not refuse.
  dna <- d
  dna$g <- factor(
    c("A", "A", "A", NA, "B", "B", "B"),
    levels = c("A", "B")
  )
  for (tot in c(TRUE, FALSE)) {
    for (dna_mode in c(TRUE, FALSE)) {
      r <- suppressWarnings(table_categorical(
        dna,
        select = bin,
        by = g,
        smd = TRUE,
        include_total = tot,
        drop_na = dna_mode,
        output = "long"
      ))
      expect_false(
        is.na(r$smd[[1L]]),
        info = paste("include_total", tot, "drop_na", dna_mode)
      )
    }
  }
  # The "(Missing)" level is displayed and never tested: the value is
  # the complete-case one either way.
  keep <- suppressWarnings(table_categorical(
    dna,
    select = bin,
    by = g,
    smd = TRUE,
    drop_na = FALSE,
    output = "long"
  ))
  drop <- suppressWarnings(table_categorical(
    dna,
    select = bin,
    by = g,
    smd = TRUE,
    drop_na = TRUE,
    output = "long"
  ))
  expect_identical(keep$smd[[1L]], drop$smd[[1L]])
  st <- as_structured(suppressWarnings(table_categorical(
    dna,
    select = bin,
    by = g,
    smd = TRUE,
    drop_na = FALSE
  )))
  expect_true(all(is.na(
    st$body[[.CAT_KEY_SMD]][st$body$.row_role == "missing"]
  )))
})

test_that("`smd` without `by`, and a non-logical `smd`, are refused", {
  d <- .smd_cat_data()
  expect_warning(
    table_categorical(d, select = bin, smd = TRUE, output = "long"),
    class = "spicy_ignored_arg"
  )
  expect_error(
    table_categorical(d, select = bin, by = g, smd = "yes"),
    class = "spicy_invalid_input"
  )
})

test_that("an undefined categorical SMD is disclosed and NA", {
  skip_if_not_installed("MASS")
  # Each group constant on a DIFFERENT category: `ginv` alone would
  # publish 0, i.e. "perfectly balanced" for the most imbalanced
  # variable possible.
  d2 <- data.frame(
    g = factor(c("A", "A", "B", "B")),
    v = factor(c("a", "a", "b", "b"), levels = c("a", "b", "c"))
  )
  msg <- tryCatch(
    table_categorical(d2, select = v, by = g, smd = TRUE, output = "long"),
    spicy_undefined_stat = function(w) conditionMessage(w)
  )
  expect_match(
    msg,
    "each group is constant on a different category",
    fixed = TRUE
  )
  r <- suppressWarnings(table_categorical(
    d2,
    select = v,
    by = g,
    smd = TRUE,
    output = "long"
  ))
  expect_true(is.na(r$smd[[1L]]))
  # Disjoint supports on four categories: `all(S == 0)` is FALSE, so
  # tableone's own guard never fires and it publishes a finite sqrt(2)
  # where the true distance is infinite.
  d4 <- data.frame(
    g = factor(c("A", "A", "B", "B")),
    v = factor(c("a", "b", "c", "d"), levels = c("a", "b", "c", "d"))
  )
  msg4 <- tryCatch(
    table_categorical(d4, select = v, by = g, smd = TRUE, output = "long"),
    spicy_undefined_stat = function(w) conditionMessage(w)
  )
  expect_match(msg4, "no overlapping categories", fixed = TRUE)
  r4 <- suppressWarnings(table_categorical(
    d4,
    select = v,
    by = g,
    smd = TRUE,
    output = "long"
  ))
  expect_true(is.na(r4$smd[[1L]]))
  # The cell shows the en-dash of "applies but not estimable", never a
  # blank and never a number.
  txt <- paste(
    capture.output(suppressWarnings(print(table_categorical(
      d4,
      select = v,
      by = g,
      smd = TRUE
    )))),
    collapse = "\n"
  )
  expect_match(txt, spicy_str("cell_undefined"), fixed = TRUE)
  expect_false(grepl("1.41", txt, fixed = TRUE))
})

test_that("the SMD is the last column on every engine, and moves no other", {
  skip_if_not_installed("MASS")
  d <- .smd_cat_data()
  off <- table_categorical(
    d,
    select = c(bin, k3),
    by = g,
    output = "data.frame"
  )
  on <- table_categorical(
    d,
    select = c(bin, k3),
    by = g,
    smd = TRUE,
    output = "data.frame"
  )
  expect_identical(names(on), c(names(off), .CAT_KEY_SMD))
  expect_identical(on[names(off)], off)

  keys_off <- names(
    as_structured(table_categorical(
      d,
      select = c(bin, k3),
      by = g
    ))$col_meta
  )
  keys_on <- names(
    as_structured(table_categorical(
      d,
      select = c(bin, k3),
      by = g,
      smd = TRUE
    ))$col_meta
  )
  expect_identical(keys_on, c(keys_off, .CAT_KEY_SMD))

  # Every rendered engine writes its trailing geometry from the RIGHT.
  # Exercise the three shapes those expressions branch on, with and
  # without the new column, and require the SMD header to be last.
  for (ci in c(TRUE, FALSE)) {
    for (meas in c("auto", "none")) {
      lab <- paste("assoc_ci", ci, "measure", meas)
      rw <- table_categorical(
        d,
        select = c(bin, k3),
        by = g,
        smd = TRUE,
        assoc_measure = meas,
        assoc_ci = ci,
        output = "data.frame"
      )
      expect_identical(names(rw)[[ncol(rw)]], .CAT_KEY_SMD, info = lab)
      if (requireNamespace("flextable", quietly = TRUE)) {
        expect_s3_class(
          table_categorical(
            d,
            select = c(bin, k3),
            by = g,
            smd = TRUE,
            assoc_measure = meas,
            assoc_ci = ci,
            output = "flextable"
          ),
          "flextable"
        )
      }
      if (requireNamespace("openxlsx2", quietly = TRUE)) {
        path <- withr::local_tempfile(fileext = ".xlsx")
        suppressMessages(table_categorical(
          d,
          select = c(bin, k3),
          by = g,
          smd = TRUE,
          assoc_measure = meas,
          assoc_ci = ci,
          output = "excel",
          excel_path = path
        ))
        sheet <- openxlsx2::wb_to_df(
          openxlsx2::wb_load(path),
          col_names = FALSE
        )
        # The SMD header is the rightmost non-empty cell of the top
        # header row -- the one whose first cell is "Variable".
        hdr <- which(!is.na(sheet[[1L]]) & sheet[[1L]] == "Variable")
        expect_length(hdr, 1L)
        top <- unlist(sheet[hdr, ], use.names = FALSE)
        top <- top[!is.na(top) & nzchar(top)]
        expect_identical(top[[length(top)]], "SMD", info = lab)

        # The text number-format is applied to the TRAILING columns,
        # counted from the right -- the arithmetic the SMD column
        # shifts. Wrong count, wrong cells, and no visible error: so
        # read the styles back. The statistics block must be one
        # uniform style, of the right width, and different from the
        # count / percent styles to its left.
        wb <- openxlsx2::wb_load(path)
        nc <- ncol(sheet)
        style_at <- function(cc) {
          openxlsx2::wb_get_cell_style(
            wb,
            dims = openxlsx2::wb_dims(rows = hdr + 3L, cols = cc)
          )
        }
        n_stat <- 1L +
          as.integer(meas != "none") +
          2L * as.integer(ci && meas != "none") +
          1L
        stat_styles <- vapply(
          seq.int(nc - n_stat + 1L, nc),
          style_at,
          character(1)
        )
        expect_length(unique(stat_styles), 1L)
        expect_false(style_at(nc - n_stat) %in% stat_styles, info = lab)
      }
    }
  }
})
