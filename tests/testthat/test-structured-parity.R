# Structured/rich-engine parity fixes from the findings re-triage, group C
# (dev/findings_retriage.md):
#   M3 -- in a multi-model table, a factor's reference row must be BLANK (not
#         en-dash) in the columns of models that lack the factor, matching
#         the char body / console.
#   m2 -- gt spanner ids must not collide when two model labels differ only
#         in characters make.names() folds together ("Step 1" vs "Step.1").
#   B-structured-outcome -- as_structured() / the rich engines carry the
#         multi-DV Outcome row that print() shows.

.sp_m_factor <- function() {
  d <- mtcars
  d$cyl <- factor(d$cyl)
  lm(mpg ~ wt + cyl, data = d)
}
.sp_m_plain <- function() lm(mpg ~ wt, data = mtcars)

test_that("M3: reference row is blank (not en-dash) for models lacking the factor", {
  x <- table_regression(list(.sp_m_factor(), .sp_m_plain()))
  s <- as_structured(x)
  sb <- spicy:::.format_structured_to_string_body(s)
  i_ref <- which(s$body$.row_role == "reference")[1L]
  m1_cols <- grep("Model 1", names(sb), value = TRUE)
  m2_cols <- grep("Model 2", names(sb), value = TRUE)
  # model WITH the factor: en-dash; model WITHOUT: blank
  expect_true(all(sb[i_ref, m1_cols] == "–"))
  expect_true(all(sb[i_ref, m2_cols] == ""))
  # v3 says the same thing per cell, without a per-row model list.
  for (cl in m1_cols) {
    expect_identical(s$cell_status[[cl]][i_ref], "reference", info = cl)
  }
  for (cl in m2_cols) {
    expect_identical(
      spicy:::.struct_cell_status(s, cl)[i_ref],
      "",
      info = cl
    )
  }
})

test_that("M3: single-model reference rows still en-dash everywhere", {
  s <- as_structured(table_regression(.sp_m_factor()))
  sb <- spicy:::.format_structured_to_string_body(s)
  i_ref <- which(s$body$.row_role == "reference")[1L]
  data_cols <- names(sb)[-1]
  expect_true(all(sb[i_ref, data_cols] == "–"))
})

test_that("m2: gt renders model labels that collide under make.names()", {
  skip_if_not_installed("gt")
  g <- table_regression(
    list("Step 1" = .sp_m_factor(), "Step.1" = .sp_m_plain()),
    output = "gt"
  )
  expect_s3_class(g, "gt_tbl")
})

test_that("B-structured-outcome: as_structured() carries the Outcome row", {
  m1 <- .sp_m_factor()
  m2 <- lm(hp ~ wt, data = mtcars)
  x <- table_regression(list(m1, m2), outcome_labels = c("MPG", "HP"))
  s <- as_structured(x)
  orow <- spicy:::.struct_outcome_row(s)
  expect_length(orow, 1L)
  expect_identical(s$body$Variable[orow], "Outcome")
  # the label text overlays in the string body (per-model first sub-column)
  sb <- spicy:::.format_structured_to_string_body(s)
  cells <- unlist(sb[orow, -1])
  expect_true("MPG" %in% cells && "HP" %in% cells)
  # parity with print()
  out <- paste(capture.output(print(x)), collapse = "\n")
  expect_match(out, "Outcome", fixed = TRUE)
})

test_that("B-structured-outcome: no Outcome row without explicit labels", {
  m1 <- .sp_m_factor()
  m2 <- lm(hp ~ wt, data = mtcars)
  s <- as_structured(table_regression(list(m1, m2)))
  expect_length(spicy:::.struct_outcome_row(s), 0L)
})


# ============================================================================
# The two spanner rows come from one producer -- they cannot drift apart
# ============================================================================

test_that("the char body and the typed view span the same models", {
  # The console reads `attr(x, "spanners")`; every rich engine reads
  # `as_structured(x)$spanners`. They were built by two loops keyed on
  # the LABEL, which agreed only because labels happened to be unique --
  # and disagreed the moment they were not (one overwrote, the other
  # unioned into a non-contiguous set). One producer keyed on `model_id`
  # now makes the two rows the same object shifted by the Variable
  # column, on every shape the family renders.
  d <- as.data.frame(sochealth)
  d$obese <- as.integer(d$bmi >= 30)
  d$obese[is.na(d$obese)] <- 0L
  q <- function(e) suppressMessages(suppressWarnings(e))

  m1 <- lm(bmi ~ age + sex, data = d)
  m2 <- lm(bmi ~ age + sex + education, data = d)
  m3 <- lm(bmi ~ age, data = d)
  m4 <- lm(wellbeing_score ~ age, data = d)
  g1 <- glm(obese ~ age + sex, data = d, family = binomial())
  g2 <- glm(obese ~ age, data = d, family = binomial())

  tables <- list(
    two = q(table_regression(list(m1, m2))),
    three = q(table_regression(list(m1, m2, m3))),
    four = q(table_regression(list(m1, m2, m3, m4))),
    named = q(table_regression(list(Base = m1, Full = m2))),
    partial = q(table_regression(list("Step 1" = m1, m2))),
    labels = q(table_regression(list(m1, m2), model_labels = c("A", "B"))),
    dv_auto = q(table_regression(list(m1, m4))),
    ci = q(table_regression(list(m1, m2), show_columns = c("b", "ci", "p"))),
    wide = q(table_regression(
      list(m1, m2),
      show_columns = c("b", "se", "ci", "t", "p")
    )),
    ame = q(table_regression(
      list(g1, g2),
      show_columns = c("b", "ci", "p", "ame")
    )),
    merged = q(table_regression(list(m1, m2), fit_stats_layout = "merged")),
    uv = q(table_regression_uv(
      d,
      outcome = bmi,
      predictors = c(age, sex),
      multivariable = TRUE
    )),
    multinom = q(table_regression(
      nnet::multinom(region ~ age, data = d, trace = FALSE)
    ))
  )

  for (nm in names(tables)) {
    x <- tables[[nm]]
    a <- attr(x, "spanners")
    s <- q(as_structured(x))
    expect_false(is.null(a), info = nm)
    expect_identical(names(a), names(s$spanners), info = nm)

    # The two rows index different column spaces on purpose: the char
    # body prints an interval as ONE bracketed column where the typed
    # view keeps ci_low and ci_high apart. So the invariant is not on
    # the integers, it is on the MODELS -- spanner k must cover exactly
    # one model, and the same one, on both sides.
    char_id <- rep(NA_character_, ncol(x))
    cs <- attr(x, "col_spec")
    pos <- match(
      vapply(cs, `[[`, character(1), "col_name"),
      names(x)
    )
    char_id[pos[!is.na(pos)]] <-
      vapply(cs, `[[`, character(1), "model_id")[!is.na(pos)]

    struct_id <- c(
      NA_character_,
      unname(vapply(
        s$col_meta,
        function(e) e$model_id %||% NA_character_,
        character(1)
      ))[names(s$col_meta) != "Variable"]
    )

    char_seen <- integer(0)
    struct_seen <- integer(0)
    for (k in seq_along(a)) {
      cm <- unique(char_id[a[[k]]])
      sm <- unique(struct_id[s$spanners[[k]]])
      expect_length(cm, 1L)
      expect_length(sm, 1L)
      expect_false(is.na(cm), info = paste(nm, k))
      expect_identical(cm, sm, info = paste(nm, k))
      # Contiguous on both sides -- a broken run is what reached the
      # engines before, and each drew it differently.
      expect_identical(a[[k]], seq(min(a[[k]]), max(a[[k]])), info = nm)
      expect_identical(
        s$spanners[[k]],
        seq(min(s$spanners[[k]]), max(s$spanners[[k]])),
        info = nm
      )
      char_seen <- c(char_seen, a[[k]])
      struct_seen <- c(struct_seen, s$spanners[[k]])
    }
    # Model order, no overlap: the ranges partition the data columns.
    expect_identical(char_seen, sort(char_seen), info = nm)
    expect_identical(struct_seen, sort(struct_seen), info = nm)
    expect_identical(anyDuplicated(char_seen), 0L, info = nm)
    expect_identical(anyDuplicated(struct_seen), 0L, info = nm)
    # And covering: a spanner shortened on one side only would leave a
    # data column bare there while the other side still rules it.
    expect_identical(char_seen, seq_len(ncol(x))[-1L], info = nm)
    expect_identical(struct_seen, seq_len(length(struct_id))[-1L], info = nm)
  }
})

test_that(".model_spanner_ranges – the contract of the shared producer", {
  lmap <- stats::setNames(c("A", "B", "C"), c("M1", "M2", "M3"))

  # Position-built: the range is `which(id == m_id)`, in column order,
  # and the label is written on top afterwards.
  expect_identical(
    spicy:::.model_spanner_ranges(
      c(NA, "M1", "M1", "M2", "M2"),
      lmap
    ),
    list(A = 2:3, B = 4:5)
  )
  # Model order follows the columns, not the label_map.
  expect_identical(
    names(spicy:::.model_spanner_ranges(
      c(NA, "M3", "M1", "M2"),
      lmap
    )),
    c("C", "A", "B")
  )
  # An empty label is skipped; its columns get no spanner.
  expect_identical(
    spicy:::.model_spanner_ranges(
      c(NA, "M1", "M2"),
      stats::setNames(c("A", ""), c("M1", "M2"))
    ),
    list(A = 2L)
  )
  # A model absent from the columns simply has no range.
  expect_identical(
    spicy:::.model_spanner_ranges(c(NA, "M2"), lmap),
    list(B = 2L)
  )
  # A model whose columns are not contiguous is dropped, not unioned
  # into a set the engines would each improvise over.
  expect_identical(
    spicy:::.model_spanner_ranges(
      c(NA, "M1", "M2", "M1"),
      lmap
    ),
    list(B = 3L)
  )
  # The property that makes the divergence unrepresentable. Two models
  # under one label can no longer be built through the public API
  # (`validate_resolved_model_labels()` refuses it), but the producer
  # must not be the piece that depends on that guard: keyed on model_id
  # and built by position, it emits TWO ranges, neither swallowing nor
  # overwriting the other. A label-keyed `out[[lbl]] <- idx` returns one
  # range here -- which is precisely how the char body used to lose the
  # first model, and the typed view to union both into 2:5.
  expect_identical(
    spicy:::.model_spanner_ranges(
      c(NA, "M1", "M1", "M2", "M2"),
      stats::setNames(c("Same", "Same"), c("M1", "M2"))
    ),
    list(Same = 2:3, Same = 4:5)
  )

  # No placed column at all -> empty list (each caller turns that into
  # NULL under its own guard).
  expect_identical(
    spicy:::.model_spanner_ranges(c(NA, NA), lmap),
    stats::setNames(list(), character(0))
  )
  # Indices are integer, as the ASCII renderer and the engines expect.
  r <- spicy:::.model_spanner_ranges(c(NA, "M1"), lmap)
  expect_type(r[["A"]], "integer")
})
