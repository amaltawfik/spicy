# The typed (structured) view of `table_outcome()`.
#
# Same contract as the three other descriptive families, and the same
# three properties: identity (every row says what it is), fidelity
# (rendering the typed body reproduces the console cell for cell), and
# addressability (`inline()` reaches a level's mean AND the block's own
# p, which is what this shape needed the addressing fix for).

.tos_quiet <- function(expr) {
  invisible(utils::capture.output(res <- suppressWarnings(expr)))
  res
}

.tos_tbl <- function(...) {
  .tos_quiet(table_outcome(as.data.frame(spicy::sochealth), ...))
}

test_that("as_structured() accepts the family", {
  tbl <- .tos_tbl(bmi, select = c(sex, smoking))
  s <- as_structured(tbl)
  expect_type(s, "list")
  expect_identical(s$version, spicy:::.spicy_structured_version())
  expect_null(s$stars)
  expect_null(s$spanners)
  expect_identical(s$outcome_labels_by_col, character(0))
  # The refusal names every supported family, this one included.
  err <- tryCatch(as_structured(mtcars), error = identity)
  expect_match(conditionMessage(err), "spicy_outcome_table", fixed = TRUE)
})

test_that("every row says what it is", {
  tbl <- .tos_tbl(bmi, select = c(sex, smoking))
  s <- as_structured(tbl)
  expect_identical(
    s$body$.row_role,
    c(
      "summary",
      "factor_header",
      "level",
      "level",
      "factor_header",
      "level",
      "level",
      "missing"
    )
  )
  # Two roles at indent 0, `summary` and `factor_header`, which is
  # exactly why the shared geometry predicates read `.indent` and not the
  # complement of a role.
  expect_identical(s$body$.indent, c(0L, 0L, 1L, 1L, 0L, 1L, 1L, 1L))
  expect_identical(
    s$body$.variable,
    c("bmi", "sex", "sex", "sex", "smoking", "smoking", "smoking", "smoking")
  )
  # The marginal row belongs to the OUTCOME, so a bare citation of the
  # outcome lands on it.
  expect_identical(s$body$.level[[1L]], NA_character_)
  # The missing display level is keyed by its ROLE, not by its label.
  expect_identical(s$body$.row_role[[8L]], "missing")
  # All four roles are contract vocabulary: no extension.
  expect_true(all(s$body$.row_role %in% spicy:::.STRUCT_ROW_ROLES))
})

test_that("the typed body renders the console body, cell for cell", {
  corpus <- list(
    .tos_tbl(bmi, select = c(sex, smoking)),
    .tos_tbl(
      bmi,
      select = c(sex, region),
      statistic = TRUE,
      effect_size = "auto"
    ),
    .tos_tbl(bmi, select = sex, show_columns = c("med_iqr", "med_ci", "n")),
    .tos_tbl(bmi, select = sex, overall = FALSE, decimal_mark = ",")
  )
  for (tbl in corpus) {
    s <- as_structured(tbl)
    rendered <- spicy:::.format_structured_to_string_body(s)
    shown <- attr(tbl, "display_df")
    expect_identical(names(rendered), names(shown))
    for (nm in names(shown)) {
      expect_identical(trimws(rendered[[nm]]), trimws(shown[[nm]]))
    }
  }
})

test_that("the structural blanks are absences, not undefined cells", {
  tbl <- .tos_tbl(bmi, select = c(sex, smoking), statistic = TRUE)
  s <- as_structured(tbl)
  header <- which(s$body$.row_role == "factor_header")
  levels_i <- which(s$body$.indent > 0L)
  # A descriptive statistic does not apply to a block header, and a
  # block statistic does not apply to a level: NA in the body, and NO
  # `cell_status` -- "undefined" means "applies here and has no value".
  expect_true(all(is.na(s$body$M[header])))
  expect_true(all(is.na(s$body$p[levels_i])))
  status <- s$cell_status
  if (!is.null(status) && "M" %in% names(status)) {
    expect_true(all(!nzchar(status$M[header])))
  }
  if (!is.null(status) && "p" %in% names(status)) {
    expect_true(all(!nzchar(status$p[levels_i])))
  }
})

test_that("an undefined cell keeps its status", {
  # A level holding one observation has no SD: the console prints the
  # undefined glyph and the typed view says why.
  d <- data.frame(
    y = c(1, 2, 4, 5, 2, 3, 8),
    g = factor(c("A", "A", "A", "A", "A", "A", "B"))
  )
  tbl <- .tos_quiet(table_outcome(d, y, select = g, p_value = FALSE))
  s <- as_structured(tbl)
  solo <- which(s$body$.level == "B")
  expect_identical(s$cell_status$SD[solo], "undefined")
  expect_identical(
    spicy:::.format_structured_to_string_body(s)$SD[solo],
    spicy_str("cell_undefined")
  )
})

test_that("inline() reaches a level and a block", {
  tbl <- .tos_tbl(bmi, select = c(sex, smoking), effect_size = "auto")
  s <- as_structured(tbl)
  rendered <- spicy:::.format_structured_to_string_body(s)

  # A level's own statistic.
  expect_identical(inline(tbl, sex, "Female", "m"), trimws(rendered$M[[3L]]))
  # The block's statistics, addressed WITHOUT a level -- they live on
  # the header row, which carries none.
  expect_identical(inline(tbl, sex, column = "p"), trimws(rendered$p[[2L]]))
  expect_identical(inline(tbl, sex, column = "{p}"), trimws(rendered$p[[2L]]))
  expect_identical(inline(tbl, sex, column = "es"), trimws(rendered$ES[[2L]]))
  # The missing display level, by role.
  expect_identical(
    inline(tbl, smoking, "(Missing)", "m"),
    trimws(rendered$M[[8L]])
  )
})

test_that("a bare inline() on the outcome cites the marginal mean", {
  # Watch on the neighboring train: `.inline_default_token()` looks for
  # the token "m", and the count precedes it in the preference list. If
  # that ever drifts back, the most natural citation this table offers
  # would quote an N where the sentence means a mean.
  tbl <- .tos_tbl(bmi, select = sex)
  expect_identical(inline(tbl, bmi), "25.93")
  expect_identical(inline(tbl, bmi), inline(tbl, bmi, column = "m"))
  expect_false(identical(inline(tbl, bmi), inline(tbl, bmi, column = "n")))
})

test_that("the interval composes from its own bounds", {
  tbl <- .tos_tbl(bmi, select = sex)
  expect_match(inline(tbl, sex, "Female", "ci"), "^\\[.+, .+\\]$")
  expect_match(inline(tbl, bmi, column = "ci"), "^\\[.+, .+\\]$")
})

test_that("addressing survives custom labels", {
  d <- as.data.frame(spicy::sochealth)
  plain <- .tos_quiet(table_outcome(d, bmi, select = sex))
  relabelled <- .tos_quiet(table_outcome(
    d,
    bmi,
    select = sex,
    labels = c(sex = "Administrative sex", bmi = "BMI")
  ))
  expect_identical(
    inline(relabelled, sex, "Female", "m"),
    inline(plain, sex, "Female", "m")
  )
  # The label reached the title and the stub, though.
  expect_identical(
    attr(relabelled, "outcome_label"),
    "BMI"
  )
  expect_identical(
    attr(relabelled, "display_df")$Variable[[2L]],
    "Administrative sex"
  )
})


# ============================================================================
# Coercion and the broom methods
# ============================================================================

test_that("coercion returns the displayed table and keeps only the note", {
  tbl <- .tos_tbl(bmi, select = c(sex, smoking))
  # The class tree of the family.
  expect_identical(
    class(tbl),
    c("spicy_outcome_table", "spicy_table", "data.frame")
  )
  plain <- as.data.frame(tbl)
  expect_identical(class(plain), "data.frame")
  # The displayed table with its numbers, exactly `output = "data.frame"`
  # (decision 5 of the ADR of 2026-10-20), one row per printed row.
  expect_identical(
    plain,
    .tos_tbl(bmi, select = c(sex, smoking), output = "data.frame")
  )
  expect_identical(nrow(plain), nrow(as_structured(tbl)$body))
  # The note is the one attribute that travels: the provenance
  # (`outcome`, `select`) and the rendering attributes stay on the
  # printed object. The old spellings of the two renamed attributes are
  # gone, not aliased (`"by"` became `"select"`, `"by_na_dropped"`
  # became `"select_na_dropped"`).
  expect_identical(
    setdiff(names(attributes(plain)), c("names", "row.names", "class")),
    "note"
  )
  expect_identical(attr(plain, "note"), attr(tbl, "note"))
  expect_null(attr(plain, "outcome"))
  expect_null(attr(plain, "select"))
  expect_null(attr(plain, "by"))
  expect_null(attr(plain, "by_na_dropped"))
  expect_null(attr(plain, "select_na_dropped"))
  expect_null(attr(plain, "display_df"))
  expect_null(attr(plain, "structured"))
  # The original object is untouched and still prints.
  expect_s3_class(tbl, "spicy_outcome_table")
  expect_false(is.null(attr(tbl, "display_df")))
  expect_identical(attr(tbl, "outcome"), "bmi")
  expect_identical(attr(tbl, "select"), c("sex", "smoking"))

  skip_if_not_installed("tibble")
  tb <- tibble::as_tibble(tbl)
  expect_s3_class(tb, "tbl_df")
  expect_identical(names(tb), names(plain))
  expect_identical(nrow(tb), nrow(plain))
})

test_that("tidy() returns the described rows only", {
  tbl <- .tos_tbl(bmi, select = c(sex, smoking))
  td <- broom::tidy(tbl)
  expect_identical(
    names(td),
    c(
      "outcome",
      "variable",
      "label",
      "level",
      ".row_role",
      "estimate",
      "std.error",
      "conf.low",
      "conf.high",
      "n",
      "weighted_n",
      "median",
      "q1",
      "q3",
      "min",
      "max",
      "sd"
    )
  )
  # Six described rows: the marginal one plus two blocks of two and
  # three levels. The block headers describe nothing and are absent.
  expect_identical(nrow(td), 6L)
  # `outcome` is constant, `variable` changes: two identity columns
  # because they mean two different things.
  expect_identical(unique(td$outcome), "bmi")
  expect_identical(unique(td$variable), c("bmi", "sex", "smoking"))
  expect_true(is.na(td$level[[1L]]))
  # Each row says what it is: the marginal row, the levels, and the
  # missing display level of `smoking`.
  expect_identical(
    td$.row_role,
    c("summary", "level", "level", "level", "level", "missing")
  )

  raw <- .tos_tbl(bmi, select = c(sex, smoking), output = "long")
  keep <- raw$.row_role != "factor_header"
  expect_equal(td$estimate, raw$mean[keep], tolerance = 1e-12)
  expect_equal(
    td$std.error,
    raw$sd[keep] / sqrt(raw$n[keep]),
    tolerance = 1e-12
  )
  expect_equal(td$median, raw$median[keep], tolerance = 1e-12)
  expect_equal(td$q1, raw$q1[keep], tolerance = 1e-12)
  expect_equal(td$q3, raw$q3[keep], tolerance = 1e-12)
  # No weights: no weighted count.
  expect_true(all(is.na(td$weighted_n)))
})

test_that("glance() returns one row per block", {
  tbl <- .tos_tbl(bmi, select = c(sex, smoking), effect_size = "auto")
  gl <- broom::glance(tbl)
  expect_identical(
    names(gl),
    c(
      "outcome",
      "variable",
      "label",
      "n_levels",
      "test_type",
      "statistic",
      "df",
      "df.residual",
      "p.value",
      "es_type",
      "es_value",
      "es_ci_lower",
      "es_ci_upper",
      "nobs"
    )
  )
  expect_identical(nrow(gl), 2L)
  expect_identical(gl$variable, c("sex", "smoking"))
  expect_identical(unique(gl$outcome), "bmi")
  # `n_levels` counts DISPLAYED levels; `smoking` shows a missing one.
  expect_identical(gl$n_levels, c(2L, 3L))
  # Every block's count sums to the marginal count.
  raw <- .tos_tbl(
    bmi,
    select = c(sex, smoking),
    effect_size = "auto",
    output = "long"
  )
  overall_n <- raw$n[[1L]]
  expect_identical(raw$.row_role[[1L]], "summary")
  expect_true(all(gl$nobs == overall_n))

  hdr <- raw[raw$.row_role == "factor_header", ]
  expect_equal(gl$p.value, hdr$p.value, tolerance = 1e-12)
  expect_equal(gl$statistic, hdr$statistic, tolerance = 1e-12)
  # One vocabulary for the long frame and glance().
  expect_equal(gl$df, hdr$df, tolerance = 1e-12)
  expect_equal(gl$df.residual, hdr$df.residual, tolerance = 1e-12)
  expect_identical(gl$es_type, hdr$es_type)
})

test_that("the glance schema is fixed, with no SMD columns", {
  # Decision 9 of the ADR of 2026-10-20: this table has no `smd`
  # argument, so the two always-NA SMD columns are gone from glance(),
  # as from the long output and the printed object.
  tbl <- .tos_tbl(bmi, select = sex)
  gl <- broom::glance(tbl)
  expect_false(any(c("smd_type", "smd_value") %in% names(gl)))
  expect_false(any(c("smd_type", "smd_value") %in% names(tbl)))
  # And the same schema with no comparison at all.
  gl2 <- broom::glance(.tos_tbl(bmi, select = sex, p_value = FALSE))
  expect_identical(names(gl2), names(gl))
  expect_true(all(is.na(gl2$p.value)))
})

test_that("a block with no comparison still gets its glance row", {
  d <- data.frame(
    y = c(1, 2, 3, 4, 10, 20, 30, 40),
    one = rep("only", 8L),
    g = c("a", "a", "a", "a", "b", "b", "b", "b"),
    stringsAsFactors = FALSE
  )
  tbl <- .tos_quiet(table_outcome(d, y, select = c(one, g)))
  gl <- broom::glance(tbl)
  expect_identical(nrow(gl), 2L)
  expect_true(is.na(gl$p.value[gl$variable == "one"]))
  expect_false(is.na(gl$p.value[gl$variable == "g"]))
  expect_identical(gl$n_levels, c(1L, 2L))
})
