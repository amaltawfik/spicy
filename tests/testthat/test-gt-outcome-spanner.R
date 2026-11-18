# gt merges no body cells, so the Outcome row of models side by side goes
# into the header: one spanner per model over its columns, between the
# per-column spanners and the model spanners, the row's label over
# column 1. A stacked table keeps its Outcome rows in the body.

gt_outcome_fits <- function() {
  list(
    m1 = lm(bmi ~ age + sex, data = sochealth),
    m2 = glm(dentist_12m ~ age + sex, family = binomial, data = sochealth)
  )
}

gt_spanner <- function(g, id) {
  sp <- g[["_spanners"]]
  sp[sp$spanner_id == id, , drop = FALSE]
}

test_that("two models: each outcome spans its model, under the model spanner", {
  skip_if_not_installed("gt")
  f <- gt_outcome_fits()
  g <- table_regression(list(A = f$m1, B = f$m2), output = "gt")
  cols <- names(g[["_data"]])

  o1 <- gt_spanner(g, "outcome_span_1")
  o2 <- gt_spanner(g, "outcome_span_2")
  stub <- gt_spanner(g, "outcome_span_stub")
  expect_identical(unlist(o1$spanner_label), "bmi")
  expect_identical(unlist(o2$spanner_label), "dentist_12m")
  expect_identical(unlist(o1$vars), cols[2:4])
  expect_identical(unlist(o2$vars), cols[5:7])
  expect_identical(unlist(stub$spanner_label), "Outcome")
  expect_identical(unlist(stub$vars), cols[1L])

  # The level between the per-column spanners and the model spanners.
  expect_identical(o1$spanner_level, 2L)
  expect_identical(stub$spanner_level, 2L)
  expect_identical(gt_spanner(g, "col_span_2_B")$spanner_level, 1L)
  expect_identical(gt_spanner(g, "model_span_1")$spanner_level, 3L)

  # The Outcome row is gone from the body, which is the body of the same
  # models without one.
  expect_false("Outcome" %in% trimws(g[["_data"]][[1L]]))
  plain <- table_regression(list(f$m1, f$m2), output = "gt")
  expect_identical(
    unname(as.list(g[["_data"]])),
    unname(as.list(plain[["_data"]]))
  )
})

test_that("one model: one spanner over all its numeric columns", {
  skip_if_not_installed("gt")
  f <- gt_outcome_fits()
  lab <- "Body mass index (kg/m2), self-reported"
  g <- table_regression(
    f$m1,
    outcome_labels = lab,
    show_columns = c("b", "ci", "p"),
    output = "gt"
  )
  cols <- names(g[["_data"]])
  o1 <- gt_spanner(g, "outcome_span_1")
  expect_identical(unlist(o1$spanner_label), lab)
  expect_identical(unlist(o1$vars), cols[-1L])
  expect_identical(o1$spanner_level, 2L)
  # Above the 95% CI spanner, which sits at the level of B and p.
  ci <- g[["_spanners"]]$spanner_id[startsWith(
    g[["_spanners"]]$spanner_id,
    "ci_span_"
  )]
  expect_identical(gt_spanner(g, ci)$spanner_level, 1L)
  expect_identical(
    unlist(gt_spanner(g, "outcome_span_stub")$spanner_label),
    "Outcome"
  )
  expect_false("Outcome" %in% trimws(g[["_data"]][[1L]]))

  # The top rule sits on the Outcome level, not on the one below.
  st <- g[["_styles"]]
  top <- vapply(
    seq_len(nrow(st)),
    function(i) {
      identical(st$locname[i], "columns_groups") &&
        "cell_border_top" %in% names(st$styles[[i]])
    },
    logical(1)
  )
  expect_setequal(
    unique(st$grpname[top]),
    c("outcome_span_1", "outcome_span_stub")
  )
})

test_that("outcome spanner ids are positional, whatever the labels", {
  skip_if_not_installed("gt")
  f <- gt_outcome_fits()
  g <- table_regression(
    list(A = f$m1, B = f$m2),
    outcome_labels = c("Step 1", "Step.1"),
    output = "gt"
  )
  ids <- g[["_spanners"]]$spanner_id
  expect_identical(
    ids[startsWith(ids, "outcome_span_")],
    c("outcome_span_1", "outcome_span_2", "outcome_span_stub")
  )
  expect_identical(
    unlist(gt_spanner(g, "outcome_span_2")$spanner_label),
    "Step.1"
  )
})

test_that("the label over column 1 is set flush left in HTML", {
  skip_if_not_installed("gt")
  f <- gt_outcome_fits()
  g <- table_regression(list(A = f$m1, B = f$m2), output = "gt")
  class(g) <- setdiff(class(g), "spicy_gt")
  html <- as.character(gt::as_raw_html(g, inline_css = FALSE))
  expect_match(
    html,
    "th#outcome_span_stub \\{\\s*text-align: left !important;\\s*\\}"
  )
  # gt's own inlining (`gtsave()`, `as_raw_html()`) keeps it last.
  inl <- as.character(gt::as_raw_html(g))
  stub <- regmatches(inl, regexpr("<th [^>]*id=\"outcome_span_stub\"", inl))
  expect_match(stub, "text-align: left;\" scope", fixed = TRUE)
})

test_that("the knitted LaTeX draws no rule under the Outcome level", {
  skip_if_not_installed("gt")
  f <- gt_outcome_fits()
  g <- table_regression(list(A = f$m1, B = f$m2), output = "gt")
  class(g) <- setdiff(class(g), "spicy_gt")
  tex <- .spicy_gt_latex_layout(
    as.character(gt::as_latex(g)),
    outcome_level = TRUE
  )
  lines <- strsplit(tex, "\n", fixed = TRUE)[[1L]]
  at <- grep("{\\mdseries Outcome}}", lines, fixed = TRUE)
  expect_length(at, 2L) # the natural-width box and the tabular*
  expect_false(any(startsWith(lines[at + 1L], "\\cmidrule")))
  # The model rules above it stay.
  expect_true(all(startsWith(lines[at - 1L], "\\cmidrule(lr){2-4}")))
})

test_that("the knitted LaTeX drops the empty row of column labels", {
  skip_if_not_installed("gt")
  f <- gt_outcome_fits()
  above_mid <- function(g) {
    class(g) <- setdiff(class(g), "spicy_gt")
    raw <- as.character(gt::as_latex(g))
    tex <- .spicy_gt_latex_layout(raw)
    lines <- strsplit(tex, "\n", fixed = TRUE)[[1L]]
    list(
      raw = strsplit(raw, "\n", fixed = TRUE)[[1L]],
      above = lines[grep("^\\\\midrule", lines) - 1L]
    )
  }
  # Every label in a spanner: gt's own label row is empty, and goes.
  x <- above_mid(table_regression(list(A = f$m1, B = f$m2), output = "gt"))
  raw_above <- x$raw[grep("^\\\\midrule", x$raw) - 1L]
  expect_true(.spicy_latex_row_empty(raw_above))
  expect_length(x$above, 2L) # the natural-width box and the tabular*
  expect_true(all(grepl("{\\mdseries Variable}}", x$above, fixed = TRUE)))
  # A CI keeps the row: LL and UL sit in it.
  ci <- above_mid(
    table_regression(f$m1, show_columns = c("b", "ci", "p"), output = "gt")
  )
  expect_true(all(grepl("LL", ci$above, fixed = TRUE)))
  expect_true(all(grepl("UL", ci$above, fixed = TRUE)))
  expect_false(.spicy_latex_row_empty(" & A & \\\\ "))
})

test_that("stacked tables keep their Outcome rows in the body", {
  skip_if_not_installed("gt")
  f <- gt_outcome_fits()
  g <- table_regression(
    list(A = f$m1, B = f$m2),
    layout = "stacked",
    output = "gt"
  )
  expect_false(any(startsWith(g[["_spanners"]]$spanner_id, "outcome_span_")))
  expect_true("Outcome" %in% trimws(g[["_data"]][[1L]]))
  s <- table_stack(
    table_regression(list(A = f$m1, B = f$m2)),
    table_regression(f$m1, outcome_labels = "BMI"),
    output = "gt"
  )
  expect_false(any(startsWith(s[["_spanners"]]$spanner_id, "outcome_span_")))
  expect_true("Outcome" %in% trimws(s[["_data"]][[1L]]))
})

test_that("tables without an Outcome row take no Outcome level", {
  skip_if_not_installed("gt")
  f <- gt_outcome_fits()
  for (g in list(
    table_regression(list(f$m1, f$m2), output = "gt"),
    table_regression(f$m1, output = "gt"),
    table_regression(
      list(A = f$m1, B = f$m2),
      outcome_labels = FALSE,
      output = "gt"
    )
  )) {
    expect_false(any(startsWith(g[["_spanners"]]$spanner_id, "outcome_span_")))
    expect_false(any(grepl(
      "outcome_span_stub",
      g[["_options"]]$value,
      fixed = TRUE
    )))
  }
})
