# Ensure global spicy options don't trigger unintended behavior
old_opts <- options(spicy.rescale = FALSE, spicy.simulate_p = FALSE)
on.exit(options(old_opts)) # restore previous user options after tests

test_that("cross_tab basic two-way table works", {
  data <- mtcars
  res <- cross_tab(data, cyl, gear, styled = FALSE)

  expect_s3_class(res, "data.frame")
  expect_true("Values" %in% names(res))

  if ("Total" %in% names(res)) {
    expect_equal(sum(res$Total, na.rm = TRUE), nrow(data))
  } else {
    expect_equal(attr(res, "n_total"), nrow(data))
  }
})


test_that("cross_tab supports grouping with by", {
  data <- mtcars
  res <- cross_tab(data, cyl, gear, by = am, styled = FALSE)

  expect_type(res, "list")
  expect_length(res, length(unique(data$am)))
  expect_true(all(vapply(res, inherits, logical(1), "data.frame")))
})

test_that("cross_tab supports interaction() in by", {
  data <- mtcars
  res <- cross_tab(data, cyl, gear, by = interaction(vs, am), styled = FALSE)

  expect_type(res, "list")
  expect_length(res, length(unique(interaction(data$vs, data$am))))
})

test_that("cross_tab handles weights and rescale properly", {
  data <- mtcars

  # Without rescale: sum(weights) ≠ N
  res1 <- cross_tab(data, cyl, gear, weights = mpg, rescale = FALSE, styled = FALSE)
  total1 <- attr(res1, "n_total")

  # With rescale: sum(weights) == N
  res2 <- cross_tab(data, cyl, gear, weights = mpg, rescale = TRUE, styled = FALSE)
  total2 <- attr(res2, "n_total")

  expect_false(isTRUE(all.equal(total1, nrow(data))))
  expect_true(isTRUE(all.equal(round(total2), nrow(data))))
})

test_that("cross_tab automatically ignores NA values", {
  df <- data.frame(
    x = c("A", "B", NA, "A", "B", NA),
    y = c("Yes", "No", "Yes", "No", "Yes", NA)
  )

  # xtabs() ignores missing values automatically
  res <- cross_tab(df, x, y, styled = FALSE)

  complete_n <- sum(stats::complete.cases(df[, c("x", "y")]))
  total_tab <- attr(res, "n_total")

  expect_equal(total_tab, complete_n)
})


test_that("cross_tab respects global options spicy.simulate_p and spicy.rescale", {
  data <- mtcars

  # Backup current options
  old_opts <- options()

  options(spicy.simulate_p = TRUE, spicy.rescale = TRUE)
  res <- cross_tab(data, cyl, gear, weights = mpg, styled = FALSE)

  # Verify attributes and global option effect
  expect_true(grepl("Chi-2:", attr(res, "note")))
  expect_true(isTRUE(all.equal(round(attr(res, "n_total")), nrow(data))))

  # Restore options
  options(old_opts)
})

test_that("cross_tab returns spicy_cross_table or list when styled = TRUE", {
  data <- mtcars
  res1 <- cross_tab(data, cyl, gear)
  res2 <- cross_tab(data, cyl, gear, by = am)

  expect_s3_class(res1, "spicy_cross_table")
  expect_s3_class(res2, "spicy_cross_table_list")
})

test_that("cross_tab accepts labelled vectors in vector mode", {
  x <- haven::labelled(
    c(1, 2, 1, 2, 1, 2),
    labels = c(Non = 1, Oui = 2)
  )
  y <- factor(c("BFH", "BFH", "HESAV", "HESAV", "ZHAW", "ZHAW"))

  res <- cross_tab(x, y, percent = "c", styled = FALSE)

  expect_s3_class(res, "data.frame")
  expect_true("Values" %in% names(res))
  expect_match(attr(res, "title"), "x x y")
})

test_that("cross_tab keeps column names with $ vector calls", {
  d <- data.frame(
    pasemploiraison_1 = c("Non", "Oui", "Non", "Oui"),
    hes = c("BFH", "BFH", "HESAV", "HESAV")
  )

  res <- cross_tab(d$pasemploiraison_1, d$hes, percent = "c", styled = FALSE)

  expect_match(attr(res, "title"), "pasemploiraison_1 x hes", fixed = TRUE)
})

test_that("cross_tab validates weights length in data.frame and vector modes", {
  df <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c("Yes", "No", "Yes", "No")
  )

  expect_error(
    cross_tab(df, x, y, weights = c(1, 2), styled = FALSE),
    "`weights` must have the same length as the number of rows.",
    fixed = TRUE
  )

  expect_error(
    cross_tab(df$x, df$y, weights = c(1, 2), styled = FALSE),
    "`weights` must have the same length as `x` and `y` in vector mode.",
    fixed = TRUE
  )
})

test_that("cross_tab rejects rescale when weight sum is zero", {
  df <- data.frame(
    x = c("A", "B"),
    y = c("Yes", "No"),
    w = c(0, 0)
  )

  expect_error(
    cross_tab(df, x, y, weights = w, rescale = TRUE, styled = FALSE),
    "`rescale = TRUE` requires a strictly positive sum of weights.",
    fixed = TRUE
  )
})

test_that("cross_tab fails early when y is explicitly NULL", {
  expect_error(
    cross_tab(mtcars, cyl, y = NULL, styled = FALSE),
    "You must specify a `y` variable",
    fixed = TRUE
  )
})

test_that("cross_tab computes by-group stats on non-empty margins", {
  df <- data.frame(
    g = c(rep("A", 6), rep("B", 8)),
    x = c("a", "a", "b", "b", "c", "c", "u", "u", "u", "u", "v", "v", "v", "v"),
    y = c("k", "l", "k", "l", "k", "l", "k", "k", "l", "l", "k", "k", "l", "l")
  )

  out <- cross_tab(df, x, y, by = g, correct = TRUE, styled = TRUE)
  note_b <- attr(out[["B"]], "note")

  expect_true(grepl("Yates continuity correction applied.", note_b, fixed = TRUE))
  expect_false(grepl("p = NA", note_b, fixed = TRUE))
})
