# gt DOM ids built from `by` levels (register n. 213 / n. 218) and the
# margin-collision label (register n. 219).

.spn_data <- function(levels, n = 60) {
  set.seed(7)
  data.frame(
    x = stats::rnorm(n),
    y = stats::rnorm(n),
    f = factor(sample(c("u", "v"), n, TRUE)),
    g = factor(rep(levels, each = n / length(levels)), levels = levels),
    stringsAsFactors = FALSE
  )
}

.spn_html_ids <- function(tbl) {
  html <- as.character(gt::as_raw_html(tbl))
  unlist(regmatches(html, gregexpr('id="[^"]*"', html)))
}


# ---- n. 213 / n. 218: whitespace-differing levels ------------------------

# gt does not write our column id into the DOM as we hand it over:
# gt:::valid_html_id() collapses every run of whitespace to a single "-".
# Two `by` levels "A B" and "A-B" therefore both landed on "A-B_n", so the
# two <th> shared one id and every body cell's headers="A-B_n" pointed at
# both. `.gt_safe_ids()` now breaks the tie on the form gt will emit.
test_that("levels differing only in whitespace get distinct DOM ids", {
  skip_if_not_installed("gt")
  d <- .spn_data(c("A B", "A-B"))
  tbl <- expect_no_error(table_categorical(d, f, by = g, output = "gt"))
  ids <- .spn_html_ids(tbl)
  expect_identical(anyDuplicated(ids), 0L)
  # Both group column pairs are present and separated.
  expect_true(all(
    c('id="A-B_n"', 'id="A-B_n_1"', 'id="A-B_pct"', 'id="A-B_pct_1"') %in% ids
  ))
})

test_that("one space vs two spaces also survives the gt normalization", {
  skip_if_not_installed("gt")
  d <- .spn_data(c("A B", "A  B"))
  tbl <- expect_no_error(table_categorical(d, f, by = g, output = "gt"))
  expect_identical(anyDuplicated(.spn_html_ids(tbl)), 0L)
})

test_that("the visible spanner labels stay the raw levels", {
  skip_if_not_installed("gt")
  d <- .spn_data(c("A B", "A-B"))
  html <- as.character(gt::as_raw_html(
    table_categorical(d, f, by = g, output = "gt")
  ))
  # The id layer is machine state; the reader sees the level verbatim.
  expect_match(html, ">A B<", fixed = TRUE)
  expect_match(html, ">A-B<", fixed = TRUE)
})

test_that(".gt_safe_ids is still the identity on ordinary keys", {
  # The second pass must not move a set whose gt-normalized forms are
  # already distinct -- including every frozen key that holds a space.
  keys <- c("Variable", "M (Hi)", "95% CI LL", "p", "n", "Step 1: B")
  expect_identical(.gt_safe_ids(keys), stats::setNames(keys, keys))
  # And it breaks exactly the tie gt would re-create.
  expect_identical(
    unname(.gt_safe_ids(c("A B_n", "A-B_n"))),
    c("A B_n", "A-B_n_1")
  )
})


# ---- n. 219: a `by` level literally named "Total" ------------------------

# The register recorded this as the dedup "leaking from the id into the
# visible label". The margin used to take the label "Total_1"; a level
# displayed beside the margin under its label is now refused, and the
# margin, relabeled, keeps the disambiguated KEY only.
test_that("a by level named Total beside the margin is refused", {
  d <- .spn_data(c("Total", "Other"))
  expect_error(
    table_categorical(d, f, by = g),
    class = "spicy_label_collision"
  )
  withr::local_options(spicy.labels = list(header_margin_total = "All"))
  out <- paste(
    capture.output(print(table_categorical(d, f, by = g))),
    collapse = "\n"
  )
  expect_match(out, "Total n", fixed = TRUE) # the user's level, verbatim
  expect_match(out, "Other n", fixed = TRUE)
  expect_match(out, "All n", fixed = TRUE) # the margin, relabeled
  expect_no_match(out, "Total_1", fixed = TRUE)
})

test_that("the gt spanners carry three distinct ids built from the n columns", {
  skip_if_not_installed("gt")
  d <- .spn_data(c("Total", "Other"))
  withr::local_options(spicy.labels = list(header_margin_total = "All"))
  tbl <- table_categorical(d, f, by = g, output = "gt")
  ids <- .spn_html_ids(tbl)
  expect_identical(anyDuplicated(ids), 0L)
  # The spanner id is `spn_` + the machine id of the group's n column
  # (gt >= 1.4.0 writes it on the spanner `<th>`), so the margin's id
  # follows its key; the visible labels are "Total", "Other" and "All".
  expect_true(all(
    c('id="spn_Total_n"', 'id="spn_Other_n"', 'id="spn_Total_1_n"') %in% ids
  ))
  labels <- tbl[["_spanners"]][["spanner_label"]]
  expect_true(all(c("Total", "Other", "All") %in% labels))
  expect_false("Total_1" %in% labels)
})
