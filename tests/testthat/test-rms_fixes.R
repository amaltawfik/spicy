# Corrections to the rms family (ols, lrm, Glm, cph) found on 2026-11-19
# while preparing the article on rms fits.

rmsfx_data <- function(seed = 1, n = 300) {
  set.seed(seed)
  d <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    w = runif(n, 0.5, 3),
    g = factor(sample(15, n, TRUE)),
    sex = factor(sample(c("f", "m"), n, TRUE))
  )
  d$y <- 1 + 0.5 * d$x1 - 0.3 * d$x2^2 + rnorm(n)
  d$yb <- rbinom(n, 1, plogis(0.4 * d$x1 + 0.3 * log(d$w)))
  d$time <- rexp(n, exp(0.3 * d$x1 + 0.4 * log(d$w)))
  d$status <- rbinom(n, 1, 0.8)
  d
}

rmsfx_se <- function(tbl, term) {
  as.numeric(tbl$SE[tbl$variable == term & tbl$.row_role == "coef"])
}


# ---- F1: a robcov() or bootcov() fit names its own variance -----------------

test_that("a fit passed through robcov() or bootcov() names that variance in the note", {
  skip_if_not_installed("rms")
  d <- rmsfx_data()
  m <- rms::lrm(yb ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  g <- d$g
  set.seed(3)
  b <- rms::bootcov(m, B = 20)
  set.seed(3)
  bc <- rms::bootcov(m, cluster = g, B = 20)
  out <- capture.output(print(table_regression(
    list(
      m = m,
      r = rms::robcov(m),
      rc = rms::robcov(m, cluster = g),
      b = b,
      bc = bc
    ),
    show_columns = "se"
  )))
  note <- paste(out, collapse = " ")
  expect_match(note, "m: Wald asymptotic (z)", fixed = TRUE)
  expect_match(note, "r: robust (Huber)", fixed = TRUE)
  expect_match(note, "rc: cluster-robust (Huber), clusters by g", fixed = TRUE)
  expect_match(note, "b: nonparametric bootstrap (20 replicates)", fixed = TRUE)
  expect_match(
    note,
    "bc: cluster bootstrap (20 replicates), clusters by g",
    fixed = TRUE
  )

  o <- rms::ols(y ~ x1, data = d, x = TRUE, y = TRUE)
  out <- capture.output(print(table_regression(rms::robcov(o))))
  expect_match(
    paste(out, collapse = " "),
    "Std. errors: Robust (Huber).",
    fixed = TRUE
  )
  ph <- rms::cph(
    survival::Surv(time, status) ~ x1,
    data = d,
    x = TRUE,
    y = TRUE
  )
  out <- capture.output(print(table_regression(rms::robcov(ph, cluster = g))))
  expect_match(
    paste(out, collapse = " "),
    "cluster-robust (Lin-Wei), clusters by g",
    fixed = TRUE
  )
})

test_that("the bootstrap variance of an lrm marks its nested test as a Wald test", {
  skip_if_not_installed("rms")
  d <- rmsfx_data()
  m <- rms::lrm(yb ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  set.seed(3)
  b <- rms::bootcov(m, B = 20)
  expect_true(nested_own_robust_vcov(b))
})


# ---- F2: vcov = "CR0" on such a fit starts from the model's variance --------

test_that("vcov = 'CR0' on a robcov() fit equals robcov() on the fit itself", {
  skip_if_not_installed("rms")
  d <- rmsfx_data()
  g <- d$g
  m <- rms::lrm(yb ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  o <- rms::ols(y ~ x1 + x2, data = d, x = TRUE, y = TRUE)
  gl <- rms::Glm(yb ~ x1 + x2, family = binomial, data = d, x = TRUE, y = TRUE)
  ph <- rms::cph(
    survival::Surv(time, status) ~ x1 + x2,
    data = d,
    x = TRUE,
    y = TRUE
  )
  for (fit in list(m, o, gl, ph)) {
    want <- sqrt(diag(stats::vcov(rms::robcov(fit, cluster = g))))[["x1"]]
    for (fit_in in list(rms::robcov(fit), rms::robcov(fit, cluster = g))) {
      tbl <- table_regression(
        fit_in,
        vcov = "CR0",
        cluster = g,
        show_columns = "se",
        digits = 12,
        output = "data.frame"
      )
      expect_equal(rmsfx_se(tbl, "x1"), want, tolerance = 1e-10)
    }
  }
  # An ols bootstrap fit keeps the model's variance in orig.var.
  set.seed(3)
  ob <- rms::bootcov(o, B = 10)
  tbl <- table_regression(
    ob,
    vcov = "CR0",
    cluster = g,
    show_columns = "se",
    digits = 12,
    output = "data.frame"
  )
  want <- sqrt(diag(stats::vcov(rms::robcov(o, cluster = g))))[["x1"]]
  expect_equal(rmsfx_se(tbl, "x1"), want, tolerance = 1e-10)
})

test_that("vcov = 'CR0' on an lrm bootstrap fit, which lost the model's variance, is refused", {
  skip_if_not_installed("rms")
  d <- rmsfx_data()
  m <- rms::lrm(yb ~ x1, data = d, x = TRUE, y = TRUE)
  set.seed(3)
  b <- rms::bootcov(m, B = 10)
  expect_error(
    table_regression(b, vcov = "CR0", cluster = d$g),
    "did not keep",
    class = "spicy_unsupported_vcov"
  )
})


# ---- F4: an ols hierarchy is compared as its lm twin ------------------------

test_that("nested ols fits give the change rows of their lm twin", {
  skip_if_not_installed("rms")
  d <- rmsfx_data()
  o0 <- rms::ols(y ~ x1, data = d)
  o1 <- rms::ols(y ~ x1 + x2 + w, data = d)
  l0 <- lm(y ~ x1, data = d)
  l1 <- lm(y ~ x1 + x2 + w, data = d)
  toks <- c(
    "r2_change",
    "adj_r2_change",
    "f_change",
    "f2_change",
    "lrt_change",
    "aic_change",
    "bic_change",
    "deviance_change",
    "p_change"
  )
  fs <- function(m) {
    fr <- table_regression(
      m,
      nested = TRUE,
      show_fit_stats = toks,
      digits = 10,
      output = "data.frame"
    )
    fr <- fr[fr$.row_role == "fit_stat", ]
    attr(fr, "note") <- NULL
    fr
  }
  expect_identical(fs(list(o0, o1)), fs(list(l0, l1)))
  av <- anova(l0, l1)
  def <- table_regression(list(o0, o1), nested = TRUE, output = "data.frame")
  expect_setequal(
    def$variable[def$.row_role == "fit_stat"],
    c("nobs", "r2", "r2_change", "f_change", "p_change")
  )
  expect_true(any(grepl(
    format(round(av$F[2], 2), nsmall = 2),
    capture.output(print(table_regression(list(o0, o1), nested = TRUE))),
    fixed = TRUE
  )))
})

test_that("a hierarchy holding a penalized ols or lrm fit has no change rows and says why", {
  skip_if_not_installed("rms")
  d <- rmsfx_data()
  pairs <- list(
    ols = list(
      rms::ols(y ~ x1, data = d),
      rms::ols(y ~ x1 + x2, data = d, penalty = 5)
    ),
    lrm = list(
      rms::lrm(yb ~ x1, data = d),
      rms::lrm(yb ~ x1 + x2, data = d, penalty = 5)
    )
  )
  for (m in pairs) {
    expect_warning(
      tbl <- table_regression(m, nested = TRUE, output = "data.frame"),
      "`nested = TRUE` cannot compare penalized fits",
      class = "spicy_ignored_arg"
    )
    expect_false(any(grepl("_change$", tbl$variable)))
    # A change token named in the call leaves with the comparison.
    expect_warning(
      tbl <- table_regression(
        m,
        nested = TRUE,
        show_fit_stats = c("nobs", "lrt_change", "p_change"),
        output = "data.frame"
      ),
      "Model 2",
      class = "spicy_ignored_arg"
    )
    expect_identical(tbl$variable[tbl$.row_role == "fit_stat"], "nobs")
  }
})


# ---- F6: a transformed term reads as in the formula -------------------------

test_that("log(), sqrt() and I() terms of rms fits take the name of their lm twin", {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  d <- rmsfx_data()
  d$a <- runif(nrow(d), 20, 70)
  rows <- function(models, ...) {
    tbl <- table_regression(models, ..., output = "data.frame")
    tbl[tbl$.row_role == "coef", ]
  }
  f <- yb ~ log(w) * sex + x1 + I(a^2)
  tbl <- rows(
    list(
      glm = glm(f, data = d, family = binomial),
      lrm = rms::lrm(f, data = d),
      Glm = rms::Glm(f, data = d, family = binomial)
    ),
    show_columns = c("b", "ame")
  )
  expect_identical(
    tbl$variable[tbl$level %in% NA],
    c("(Intercept)", "log(w)", "x1", "I(a^2)", "log(w):sexm", "a")
  )
  # The three columns fill the same rows, the AME of w on the log(w) row.
  for (col in c("lrm: B", "Glm: B", "lrm: AME", "Glm: AME")) {
    expect_equal(tbl[[col]], tbl[[sub("^[^:]+", "glm", col)]], tolerance = 1e-6)
  }
  ame <- tbl[tbl$variable == "log(w)", c("glm: AME", "lrm: AME", "Glm: AME")]
  expect_false(anyNA(unlist(ame)))

  tbl <- rows(
    list(
      lm = lm(y ~ sqrt(w) + sex, data = d),
      ols = rms::ols(y ~ sqrt(w) + rms::rcs(a, 3) + sex, data = d)
    ),
    show_columns = "b"
  )
  expect_identical(
    tbl$variable[tbl$level %in% NA],
    c("(Intercept)", "sqrt(w)", "a", "a'")
  )

  fs <- survival::Surv(time, status) ~ log(w) + sex + log(w):sex
  tbl <- rows(
    list(
      coxph = survival::coxph(fs, data = d),
      cph = rms::cph(fs, data = d)
    ),
    exponentiate = TRUE,
    show_columns = "b"
  )
  expect_identical(
    tbl$variable[tbl$level %in% NA],
    c("log(w)", "log(w):sexm")
  )
  expect_equal(tbl[["cph: HR"]], tbl[["coxph: HR"]], tolerance = 1e-4)
})

test_that("keep, drop and labels take the formula's spelling of a transformed rms term", {
  skip_if_not_installed("rms")
  d <- rmsfx_data()
  m <- rms::lrm(yb ~ log(w) * sex + x1, data = d)
  kept <- function(...) {
    tbl <- table_regression(m, ..., show_columns = "b", output = "data.frame")
    tbl$variable[tbl$.row_role == "coef"]
  }
  expect_identical(
    setdiff(kept(keep = "^log\\(w\\)$"), "(Intercept)"),
    "log(w)"
  )
  expect_false("log(w):sexm" %in% kept(drop = "^log\\(w\\):"))
  tbl <- table_regression(
    m,
    labels = c("log(w)" = "Weight (log)"),
    show_columns = "b",
    output = "data.frame"
  )
  expect_true("Weight (log)" %in% tbl$label)
})


# ---- F9: the rms fitters outside the registry are refused by spicy ----------

test_that("orm, bj, npsurv and orm.fit fits are refused with a spicy condition", {
  skip_if_not_installed("rms")
  skip_if_not_installed("survival")
  d <- rmsfx_data()
  fits <- list(
    rms::orm(y ~ x1, data = d),
    rms::orm.fit(d$x1, d$y),
    rms::bj(survival::Surv(time, status) ~ x1, data = d, x = TRUE, y = TRUE),
    rms::npsurv(survival::Surv(time, status) ~ 1, data = d)
  )
  for (fit in fits) {
    expect_error(
      table_regression(fit),
      "not a supported model class",
      class = "spicy_unsupported"
    )
  }
})
