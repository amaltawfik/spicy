# betareg: the precision sub-model as a component block, the mean link in
# the title, and case weights read as a glm's are.

skip_if_not_installed("betareg")

food_data <- function() {
  data("FoodExpenditure", package = "betareg", envir = environment())
  d <- FoodExpenditure
  d$share <- d$food / d$income
  d$w <- seq(0.5, 2, length.out = nrow(d))
  d
}


# ---- Case weights ------------------------------------------------------------

test_that("a weighted betareg reports its weighted count as a weighted glm does", {
  d <- food_data()
  bw <- betareg::betareg(share ~ income + persons, data = d, weights = w)
  gw <- stats::glm(share ~ income + persons, data = d, weights = w)

  fb <- as_regression_frame(bw)
  fg <- as_regression_frame(gw)
  expect_true(fb$info$extras$has_weights)
  expect_identical(fb$info$weights_kind, fg$info$weights_kind)
  expect_equal(fb$info$extras$weighted_n, fg$info$extras$weighted_n)
  expect_equal(fb$info$fit_stats$weighted_nobs, sum(d$w))

  df <- as.data.frame(
    table_regression(bw, show_fit_stats = c("nobs", "weighted_nobs"))
  )
  dg <- suppressWarnings(as.data.frame(
    table_regression(gw, show_fit_stats = c("nobs", "weighted_nobs"))
  ))
  expect_identical(
    df[df$Variable == "Weighted n", "B"],
    dg[dg$Variable == "Weighted n", "B"]
  )
  expect_equal(
    broom::glance(table_regression(bw))$weighted_nobs,
    sum(d$w)
  )

  b0 <- betareg::betareg(share ~ income + persons, data = d)
  f0 <- as_regression_frame(b0)
  expect_false(f0$info$extras$has_weights)
  expect_identical(f0$info$weights_kind, "none")
  expect_true(is.na(f0$info$fit_stats$weighted_nobs))
})
