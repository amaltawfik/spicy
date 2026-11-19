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


# ---- Mean link in the title --------------------------------------------------

test_that("a mean link other than logit is named in the title", {
  d <- food_data()
  fit_link <- function(link) {
    betareg::betareg(share ~ income + persons, data = d, link = link)
  }
  title_of <- function(fit) {
    strsplit(attr(table_regression(fit), "title"), ":", fixed = TRUE)[[1L]][1L]
  }
  expect_identical(title_of(fit_link("logit")), "Beta regression")
  expect_identical(title_of(fit_link("probit")), "Probit beta regression")
  expect_identical(title_of(fit_link("loglog")), "Log-log beta regression")
  expect_identical(
    title_of(fit_link("cloglog")),
    "Complementary log-log beta regression"
  )
  expect_identical(title_of(fit_link("cauchit")), "Cauchit beta regression")

  links <- c("logit", "probit", "cloglog", "loglog", "cauchit", "log")
  prefixes <- vapply(links, spicy:::.betareg_title_prefix, character(1))
  expect_identical(anyDuplicated(prefixes), 0L)
  expect_true(all(prefixes %in% names(spicy:::.FR_TITLE_PREFIXES)))

  withr::local_options(spicy.language = "fr")
  expect_match(
    attr(table_regression(fit_link("probit")), "title"),
    "^R\u00E9gression b\u00EAta probit"
  )
})
