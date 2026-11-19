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
    df[df$label %in% "Weighted n", "B"],
    dg[dg$label %in% "Weighted n", "B"]
  )
  expect_equal(df[df$label %in% "Weighted n", "B"], sum(d$w))
  expect_equal(
    broom::glance(table_regression(bw))$weighted_nobs,
    sum(d$w)
  )

  b0 <- betareg::betareg(share ~ income + persons, data = d)
  f0 <- as_regression_frame(b0)
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


# ---- Precision block ---------------------------------------------------------

prec_rows <- function(tab) {
  td <- broom::tidy(tab)
  td[td$factor_term %in% "Precision" & !is.na(td$estimate), ]
}

expect_matches_summary <- function(fit, tol = 1e-10) {
  sm <- summary(fit)$coefficients$precision
  pr <- prec_rows(table_regression(fit))
  z <- stats::qnorm(0.975)
  expect_equal(pr$estimate, unname(sm[, "Estimate"]), tolerance = tol)
  expect_equal(pr$std.error, unname(sm[, "Std. Error"]), tolerance = tol)
  expect_equal(pr$p.value, unname(sm[, "Pr(>|z|)"]), tolerance = tol)
  expect_equal(
    pr$conf.low,
    unname(sm[, "Estimate"] - z * sm[, "Std. Error"]),
    tolerance = tol
  )
  expect_equal(
    pr$conf.high,
    unname(sm[, "Estimate"] + z * sm[, "Std. Error"]),
    tolerance = tol
  )
}

test_that("the precision sub-model is a Precision block equal to summary()", {
  skip_if_not_installed("broom")
  d <- food_data()
  b2 <- betareg::betareg(share ~ income + persons | persons, data = d)
  expect_matches_summary(b2)

  d$g <- factor(rep(c("a", "b", "c"), length.out = nrow(d)))
  for (lp in c("log", "identity", "sqrt")) {
    fit <- suppressWarnings(betareg::betareg(
      share ~ income + persons | g + income,
      data = d,
      link.phi = lp
    ))
    expect_matches_summary(fit)
  }

  # A factor of the precision formula only: its reference row sits in the
  # block, the mean block has no row for it.
  b3 <- betareg::betareg(share ~ income + persons | g, data = d)
  df3 <- as.data.frame(table_regression(b3))
  blk <- df3[df3$variable %in% "Precision", ]
  expect_identical(blk$level, c(NA, "(Intercept)", "g: a", "g: b", "g: c"))
  expect_true(is.na(blk$B[blk$level %in% "g: a"]))
  expect_false(any(df3$variable %in% "g"))

  # The console shows the block, under its caption, and its note.
  out <- paste(capture.output(print(table_regression(b2))), collapse = "\n")
  expect_match(out, "Precision:", fixed = TRUE)
  expect_match(out, "Precision component: log scale.", fixed = TRUE)
  out3 <- paste(
    capture.output(print(table_regression(suppressWarnings(betareg::betareg(
      share ~ income | persons,
      data = d,
      link.phi = "sqrt"
    ))))),
    collapse = "\n"
  )
  expect_match(out3, "Precision component: square-root scale.", fixed = TRUE)
})

test_that("a constant precision keeps the table without a block", {
  d <- food_data()
  b1 <- betareg::betareg(share ~ income + persons, data = d)
  b1i <- betareg::betareg(share ~ income + persons | 1, data = d)
  for (fit in list(b1, b1i)) {
    expect_length(as_regression_frame(fit)$info$extras$component_blocks, 0L)
    df <- as.data.frame(table_regression(fit))
    expect_false(any(grepl("Precision", df$variable, fixed = TRUE)))
  }
  # The phi token still reads the constant precision.
  df <- as.data.frame(table_regression(b1, show_fit_stats = "phi"))
  expect_true(any(grepl("\u03C6", df$label, fixed = TRUE)))
})

test_that("show_components = FALSE omits the block and its note", {
  d <- food_data()
  b2 <- betareg::betareg(share ~ income + persons | persons, data = d)
  tab <- table_regression(b2, show_components = FALSE)
  out <- paste(capture.output(print(tab)), collapse = "\n")
  expect_false(grepl("Precision", out, fixed = TRUE))
  expect_identical(
    as.data.frame(tab)$variable,
    as.data.frame(table_regression(
      betareg::betareg(share ~ income + persons, data = d)
    ))$variable
  )
})

test_that("exponentiate leaves the Precision block on its link scale", {
  skip_if_not_installed("broom")
  d <- food_data()
  b2 <- betareg::betareg(share ~ income + persons | persons, data = d)
  t0 <- prec_rows(table_regression(b2))
  tx <- prec_rows(table_regression(b2, exponentiate = TRUE))
  expect_equal(tx$estimate, t0$estimate, tolerance = 1e-12)
  out <- paste(
    capture.output(print(table_regression(b2, exponentiate = TRUE))),
    collapse = "\n"
  )
  expect_match(
    out,
    "Precision component: log scale. The component rows stay on the link scale (not exponentiated).",
    fixed = TRUE
  )
})

test_that("CR1 reaches the Precision rows through the same vcovCL matrix", {
  skip_if_not_installed("broom")
  skip_if_not_installed("sandwich")
  d <- food_data()
  b2 <- betareg::betareg(share ~ income + persons | persons, data = d)
  pr <- prec_rows(table_regression(b2, vcov = "CR1", cluster = ~persons))
  vc <- sandwich::vcovCL(b2, cluster = ~persons, )
  expect_equal(
    pr$std.error,
    unname(sqrt(diag(vc)[c("(phi)_(Intercept)", "(phi)_persons")])),
    tolerance = 1e-10
  )
})

test_that("p_adjust, keep, drop, labels and AME treat the block as the other components", {
  skip_if_not_installed("broom")
  d <- food_data()
  b2 <- betareg::betareg(share ~ income + persons | persons, data = d)

  # p_adjust: the precision slope joins the family, the intercept does not.
  raw <- prec_rows(table_regression(b2))
  adj <- prec_rows(table_regression(b2, p_adjust = "holm"))
  td_raw <- broom::tidy(table_regression(b2))
  fam <- c(
    td_raw$p.value[
      td_raw$term %in% c("income", "persons") & is.na(td_raw$factor_term)
    ],
    raw$p.value[raw$term %in% "Precision::persons"]
  )
  expect_equal(
    adj$p.value[adj$term %in% "Precision::persons"],
    stats::p.adjust(fam, "holm")[3L],
    tolerance = 1e-12
  )
  expect_equal(
    adj$p.value[adj$term %in% "Precision::(Intercept)"],
    raw$p.value[raw$term %in% "Precision::(Intercept)"]
  )

  # keep / drop act on the mean rows; the block stays.
  dk <- as.data.frame(table_regression(b2, keep = "income"))
  expect_true(any(grepl("Precision", dk$variable, fixed = TRUE)))
  # One "persons" row left: the precision one.
  expect_false(any(dk$variable %in% "persons"))
  expect_true(any(dk$variable %in% "Precision" & dk$level %in% "persons"))
  dd <- as.data.frame(table_regression(b2, drop = "persons"))
  expect_true(any(grepl("Precision", dd$variable, fixed = TRUE)))

  # The AME column reads the mean sub-model only.
  ta <- broom::tidy(table_regression(b2, show_columns = c("b", "ame")))
  expect_false(any(
    ta$factor_term %in% "Precision" & ta$estimate_type %in% "ame"
  ))
})

test_that("a constant- and a variable-precision fit share one table", {
  d <- food_data()
  b1 <- betareg::betareg(share ~ income + persons, data = d)
  b2 <- betareg::betareg(share ~ income + persons | persons, data = d)
  df <- as.data.frame(table_regression(list(b1, b2)))
  i <- which(df$variable %in% "Precision" & df$level %in% "persons")
  expect_length(i, 1L)
  expect_true(is.na(df[i, "Model 1: B"]))
  expect_equal(
    df[i, "Model 2: B"],
    unname(summary(b2)$coefficients$precision["persons", "Estimate"]),
    tolerance = 1e-10
  )
  # nested = TRUE compares them by an LR test equal to lmtest::lrtest().
  lr <- 2 * (as.numeric(stats::logLik(b2)) - as.numeric(stats::logLik(b1)))
  out <- paste(
    capture.output(print(table_regression(list(b1, b2), nested = TRUE))),
    collapse = "\n"
  )
  expect_match(out, sprintf("%.2f", lr), fixed = TRUE)
  expect_match(out, "Precision:", fixed = TRUE)
})

test_that("every output carries the Precision block", {
  skip_if_not_installed("broom")
  d <- food_data()
  b2 <- betareg::betareg(share ~ income + persons | persons, data = d)
  tab <- table_regression(b2)
  est <- unname(summary(b2)$coefficients$precision[, "Estimate"])

  df <- as.data.frame(table_regression(b2, output = "data.frame"))
  expect_true(any(grepl("Precision", df$variable, fixed = TRUE)))
  lg <- table_regression(b2, output = "long")
  expect_equal(
    lg$estimate[lg$factor_term %in% "Precision" & !is.na(lg$estimate)],
    est,
    tolerance = 1e-10
  )
  s <- as_structured(tab)
  expect_true(any(grepl("Precision", unlist(s$body), fixed = TRUE)))
  expect_true(any(grepl("5.50", unlist(s$body), fixed = TRUE)))

  if (requireNamespace("gt", quietly = TRUE)) {
    g <- table_regression(b2, output = "gt")
    html <- as.character(gt::as_raw_html(g, inline_css = FALSE))
    expect_match(html, "Precision", fixed = TRUE)
    expect_match(html, "Precision component: log scale.", fixed = TRUE)
  }
  if (requireNamespace("tinytable", quietly = TRUE)) {
    tt <- table_regression(b2, output = "tinytable")
    expect_true(any(grepl("Precision", unlist(tt@data), fixed = TRUE)))
  }
  if (requireNamespace("flextable", quietly = TRUE)) {
    ft <- table_regression(b2, output = "flextable")
    expect_true(any(grepl("Precision", unlist(ft$body$dataset), fixed = TRUE)))
  }
  if (requireNamespace("openxlsx2", quietly = TRUE)) {
    path <- withr::local_tempfile(fileext = ".xlsx")
    table_regression(b2, output = "excel", excel_path = path)
    cells <- unlist(openxlsx2::wb_to_df(path, col_names = FALSE))
    expect_true(any(grepl("Precision", cells, fixed = TRUE)))
  }
})

test_that("the French table names the block and its scale", {
  d <- food_data()
  b2 <- betareg::betareg(share ~ income + persons | persons, data = d)
  withr::local_options(spicy.language = "fr")
  out <- paste(capture.output(print(table_regression(b2))), collapse = "\n")
  expect_match(out, "Précision :", fixed = TRUE)
  expect_match(
    out,
    "Composante « Précision » : échelle logarithmique.",
    fixed = TRUE
  )
})

test_that("a glmmTMB beta_family dispersion formula is a Precision block", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("broom")
  d <- food_data()
  tm <- glmmTMB::glmmTMB(
    share ~ income + persons,
    dispformula = ~persons,
    family = glmmTMB::beta_family(),
    data = d
  )
  b2 <- betareg::betareg(share ~ income + persons | persons, data = d)
  pt <- prec_rows(table_regression(tm))
  expect_equal(
    pt$estimate,
    unname(summary(tm)$coefficients$disp[, "Estimate"]),
    tolerance = 1e-10
  )
  # The same model as betareg's precision sub-model, in the same rows.
  expect_equal(
    pt$estimate,
    prec_rows(table_regression(b2))$estimate,
    tolerance = 1e-4
  )
  td <- broom::tidy(table_regression(tm))
  expect_false(any(td$factor_term %in% "Dispersion"))
  out <- paste(capture.output(print(table_regression(tm))), collapse = "\n")
  expect_match(out, "Precision component: log scale.", fixed = TRUE)
})
