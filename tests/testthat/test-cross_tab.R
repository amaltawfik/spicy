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
