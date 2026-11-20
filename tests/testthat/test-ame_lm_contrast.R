# The AME of an lm under a cluster-robust matrix, for any formula: the
# contrast c of each AME is the sample average of the derivative of the
# design row (or of its change), so the AME is c'b, its SE sqrt(c'Vc) and
# its df those of clubSandwich::linear_contrast(). Also: the note names
# Satterthwaite only for the rows that took those df, the tobit AME is
# named as computed on the latent outcome, and an rlm fit gets its AME.

.ame_lm_data <- function() {
  set.seed(11)
  n <- 120
  d <- data.frame(
    x = rnorm(n, 10, 3),
    z = runif(n, 1, 5),
    g = factor(sample(c("a", "b", "c"), n, TRUE)),
    b = rbinom(n, 1, 0.4),
    k = sample(c(4, 6, 8), n, TRUE),
    cl = rep(1:6, each = 20)
  )
  d$y <- 1 +
    0.5 * d$x -
    0.03 * d$x^2 +
    log(d$z) +
    (d$g == "b") +
    0.3 * d$b +
    rnorm(n)
  d$w <- runif(n, 0.5, 2)
  d
}

.ame_A <- function(fit, d, type = "slope") {
  spicy:::extract_ame_satterthwaite(
    fit,
    "CR2",
    d$cl,
    0.95,
    "M1",
    "y",
    ame_type = type
  )
}

test_that("the AME of an lm with I(x^2) under CR2 is the contrast c'b, with clubSandwich's SE and df", {
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("marginaleffects")
  d <- .ame_lm_data()
  fit <- lm(y ~ x + I(x^2) + g, data = d)
  # The contrast of x: 1 on x, twice the mean of x on the square.
  cv <- spicy:::build_numeric_ame_contrast(fit, "x", slope = TRUE)
  expect_equal(
    unname(cv),
    c(0, 1, 2 * mean(d$x), 0, 0),
    tolerance = 1e-8
  )
  rows <- .ame_A(fit, d)
  me <- as.data.frame(marginaleffects::avg_slopes(fit))
  expect_equal(
    rows$estimate[rows$term == "x"],
    me$estimate[me$term == "x"],
    tolerance = 1e-8
  )
  V <- clubSandwich::vcovCR(fit, cluster = d$cl, type = "CR2")
  lc <- clubSandwich::linear_contrast(
    fit,
    vcov = V,
    contrasts = rbind(x = cv),
    test = "Satterthwaite"
  )
  expect_equal(
    rows$se[rows$term == "x"],
    sqrt(drop(t(cv) %*% as.matrix(V) %*% cv)),
    tolerance = 1e-10
  )
  expect_equal(rows$df[rows$term == "x"], lc$df, tolerance = 1e-10)
})

test_that("table_regression() keeps the Satterthwaite path for an lm with I(x^2)", {
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("marginaleffects")
  d <- .ame_lm_data()
  fit <- lm(y ~ x + I(x^2) + g, data = d)
  expect_no_warning(
    tb <- table_regression(
      fit,
      vcov = "CR2",
      cluster = ~cl,
      show_columns = c("b", "se", "p", "ame", "ame_p")
    )
  )
  lo <- as.data.frame(broom::tidy(tb))
  b <- lo[lo$estimate_type == "B" & lo$term == "gb", ]
  a <- lo[lo$estimate_type == "ame" & lo$term == "gb", ]
  # In an additive model the AME of a level is its coefficient: same
  # test, same df, same p.
  expect_equal(a$df, b$df, tolerance = 1e-10)
  expect_equal(a$p.value, b$p.value, tolerance = 1e-10)
  expect_match(
    paste(capture.output(print(tb)), collapse = " "),
    "AME inference: t-test with Satterthwaite df.",
    fixed = TRUE
  )
})

test_that("poly(), log(), factor() and interactions give the AME of avg_slopes()", {
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("marginaleffects")
  d <- .ame_lm_data()
  fits <- list(
    lm(y ~ poly(x, 2) + log(z) + g, data = d),
    lm(y ~ poly(x, 3) * g + z, data = d),
    lm(y ~ x * z + I(x^2) + b + factor(k), data = d)
  )
  ids <- list(
    c(x = "x", z = "log(z)", gb = "gb"),
    c(x = "x", z = "z", gc = "gc"),
    c(x = "x", z = "z", b = "b", `6` = "factor(k)6")
  )
  for (i in seq_along(fits)) {
    rows <- .ame_A(fits[[i]], d)
    # marginaleffects warns that factor(k) is numeric in the data.
    me <- as.data.frame(suppressWarnings(
      marginaleffects::avg_slopes(fits[[i]])
    ))
    me$id <- ifelse(
      grepl(" - ", me$contrast) & me$term %in% c("g", "k"),
      sub(" - .*$", "", paste0(me$term, me$contrast)),
      me$term
    )
    me$id <- sub("^k", "", me$id)
    for (j in seq_along(ids[[i]])) {
      expect_equal(
        rows$estimate[rows$term == ids[[i]][[j]]],
        me$estimate[me$id == names(ids[[i]])[j]],
        tolerance = 1e-8
      )
    }
    expect_true(all(is.finite(rows$df)))
    expect_true(all(rows$df < stats::df.residual(fits[[i]])))
  }
})

test_that("the unit and SD quantities of a transformed predictor match avg_comparisons()", {
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("marginaleffects")
  d <- .ame_lm_data()
  fit <- lm(y ~ x + I(x^2) + g, data = d)
  for (type in c("unit", "sd")) {
    rows <- .ame_A(fit, d, type)
    me <- as.data.frame(spicy:::.spicy_avg_ame(fit, type))
    expect_equal(
      rows$estimate[rows$term == "x"],
      me$estimate[me$term == "x"],
      tolerance = 1e-8
    )
  }
})

test_that("a weighted lm with I(x^2) averages the slopes with the fit's weights", {
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("marginaleffects")
  d <- .ame_lm_data()
  fit <- lm(y ~ x + I(x^2) + g, data = d, weights = w)
  rows <- .ame_A(fit, d)
  me <- as.data.frame(marginaleffects::avg_slopes(fit, wts = d$w))
  expect_equal(
    rows$estimate[rows$term == "x"],
    me$estimate[me$term == "x"],
    tolerance = 1e-8
  )
})

test_that("a model under another matrix in the table does not try the Satterthwaite path", {
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("marginaleffects")
  d <- .ame_lm_data()
  f1 <- lm(y ~ x + g, data = d)
  f2 <- lm(y ~ x + I(x^2) + g, data = d)
  expect_no_warning(
    tb <- table_regression(
      list(f1, f2),
      vcov = list("CR2", "HC3"),
      cluster = list(~cl, NULL),
      show_columns = c("b", "se", "p", "ame", "ame_p")
    )
  )
  lo <- as.data.frame(broom::tidy(tb))
  a2 <- lo[lo$estimate_type == "ame" & lo$model == unique(lo$model)[2], ]
  expect_true(all(a2$df == stats::df.residual(f2), na.rm = TRUE))
})

test_that("the note names Satterthwaite only for the AME rows that took its df", {
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("marginaleffects")
  d <- .ame_lm_data()
  d$yb <- as.integer(d$y > stats::median(d$y))
  sc <- c("b", "se", "p", "ame", "ame_p")
  note <- function(tb) paste(capture.output(print(tb)), collapse = " ")
  f1 <- lm(y ~ x + g, data = d)
  g1 <- glm(yb ~ x + g, family = binomial, data = d)
  # lm under HC3, glm under CR2: the qualifier is the glm one alone.
  tb <- table_regression(
    list(f1, g1),
    vcov = list("HC3", "CR2"),
    cluster = list(NULL, ~cl),
    show_columns = sc
  )
  # ...and it names the model it is about.
  expect_match(
    note(tb),
    "Model 2: AME inference: t-test with Satterthwaite df (dominant-coefficient approximation).",
    fixed = TRUE
  )
  # A Path A failure: the AME rows fall back, and the note says nothing
  # of Satterthwaite for them. The coefficient rows keep their cluster-
  # robust Satterthwaite df, which the p-value sentence names.
  testthat::local_mocked_bindings(
    extract_ame_satterthwaite = function(...) stop("synthetic failure"),
    .package = "spicy"
  )
  expect_warning(
    tb <- table_regression(
      f1,
      vcov = "CR2",
      cluster = ~cl,
      show_columns = sc
    ),
    class = "spicy_fallback"
  )
  expect_no_match(note(tb), "AME inference", fixed = TRUE)
  expect_match(
    note(tb),
    "P-values: Satterthwaite t-test, cluster-robust df.",
    fixed = TRUE
  )
})

test_that("the note says the AME of a tobit model is on the latent outcome", {
  skip_if_not_installed("AER")
  skip_if_not_installed("marginaleffects")
  d <- .ame_lm_data()
  d$yc <- pmax(d$y, stats::quantile(d$y, 0.3))
  fit <- AER::tobit(yc ~ x + g, left = min(d$yc), data = d)
  tb <- table_regression(fit, show_columns = c("b", "ame"))
  txt <- paste(capture.output(print(tb)), collapse = " ")
  expect_match(txt, spicy:::spicy_str("note_ame_tobit"), fixed = TRUE)
  expect_match(txt, "latent (uncensored) outcome", fixed = TRUE)
  withr::local_options(spicy.language = "fr")
  tb_fr <- table_regression(fit, show_columns = c("b", "ame"))
  txt_fr <- paste(capture.output(print(tb_fr)), collapse = " ")
  expect_match(txt_fr, "variable latente (non censur\u00E9e)", fixed = TRUE)
  # No AME column, no sentence.
  withr::local_options(spicy.language = "en")
  expect_no_match(
    paste(capture.output(print(table_regression(fit))), collapse = " "),
    "latent",
    fixed = TRUE
  )
  reg <- table_regression_models()
  expect_match(reg$ame[reg$class == "tobit"], "latent", fixed = TRUE)
})

test_that("an rlm fit gets its AME rows, on the fit's own variance", {
  skip_if_not_installed("marginaleffects")
  d <- .ame_lm_data()
  fit <- MASS::rlm(y ~ x + I(x^2) + g, data = d)
  lo <- as.data.frame(table_regression(
    fit,
    show_columns = c("b", "se", "ame", "ame_se"),
    output = "long"
  ))
  a <- lo[lo$estimate_type == "ame" & !is.na(lo$estimate), ]
  expect_setequal(a$term, c("x", "gb", "gc"))
  me <- as.data.frame(marginaleffects::avg_slopes(fit))
  expect_equal(
    a$estimate[a$term == "x"],
    me$estimate[me$term == "x"],
    tolerance = 1e-8
  )
  expect_equal(
    a$std.error[a$term == "x"],
    me$std.error[me$term == "x"],
    tolerance = 1e-6
  )
  # By hand: c'b and sqrt(c'Vc), c the contrast of x on the design.
  cv <- c(0, 1, 2 * mean(d$x), 0, 0)
  V <- stats::vcov(fit)
  expect_equal(
    a$estimate[a$term == "x"],
    sum(cv * stats::coef(fit)),
    tolerance = 1e-8
  )
  expect_equal(
    a$std.error[a$term == "x"],
    sqrt(drop(t(cv) %*% V %*% cv)),
    tolerance = 1e-6
  )
  expect_true(all(a$test_type == "z"))
})
