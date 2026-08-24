# gt DOM ids built from `by` levels (register n. 213 / n. 218).

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

test_that("one space vs two spaces also survives the gt normalisation", {
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
  # The second pass must not move a set whose gt-normalised forms are
  # already distinct -- including every frozen key that holds a space.
  keys <- c("Variable", "M (Hi)", "95% CI LL", "p", "n", "Step 1: B")
  expect_identical(.gt_safe_ids(keys), stats::setNames(keys, keys))
  # And it breaks exactly the tie gt would re-create.
  expect_identical(
    unname(.gt_safe_ids(c("A B_n", "A-B_n"))),
    c("A B_n", "A-B_n_1")
  )
})
