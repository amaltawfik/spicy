# The AME is tested on the reference distribution of the coefficients it
# depends on: t on their shared df, z when they are z, the smallest df of
# the coefficients it depends on when their df differ.
# dev/decisions/2026-11-21-ame-reference-distribution.md.

ard_tidy <- function(fit, ...) {
  suppressWarnings(suppressMessages(broom::tidy(table_regression(
    fit,
    show_columns = c("b", "ame", "ame_ci", "ame_p"),
    ...
  ))))
}

# The fits whose AME keeps a test of its own, with the reason. The list
# cannot grow: a new entry is a new decision.
ard_exceptions <- c(
  "lm|CR2" = "clubSandwich's Satterthwaite df of the AME contrast",
  "glm_logit|CR2" = "Satterthwaite df of the dominant coefficient",
  stanreg = "a posterior has no test",
  brmsfit = "a posterior has no test"
)

test_that("the list of exceptions does not grow", {
  expect_length(ard_exceptions, 4L)
})

test_that("every fixture's AME takes the test of its coefficients", {
  skip_on_cran()
  skip_if_not_installed("marginaleffects", minimum_version = "1.0.0")
  ids <- setdiff(names(.CLASS_FIXTURES), names(ard_exceptions))
  checked <- 0L
  for (id in ids) {
    if (isTRUE(.CLASS_FIXTURES[[id]]$stan)) {
      next
    }
    pk <- .CLASS_FIXTURES[[id]]$pkgs
    pk <- ifelse(nzchar(names(pk) %||% rep("", length(pk))), names(pk), pk)
    if (!all(vapply(pk, requireNamespace, logical(1), quietly = TRUE))) {
      next
    }
    fit <- suppressWarnings(suppressMessages(
      .CLASS_FIXTURES[[id]]$fit(.class_fixture_data())
    ))
    td <- tryCatch(ard_tidy(fit), spicy_invalid_input = function(e) NULL)
    if (is.null(td)) {
      next # the class refuses the AME (test-ame_registry_guard.R)
    }
    b <- td[td$estimate_type == "B" & !is.na(td$estimate), ]
    a <- td[td$estimate_type == "ame" & !is.na(td$estimate), ]
    if (nrow(a) == 0L) {
      next
    }
    checked <- checked + 1L
    b_df <- ifelse(b$test_type == "t", b$df, Inf)
    if (all(is.infinite(b_df))) {
      expect_true(all(a$test_type == "z"), label = id)
    } else if (length(unique(b_df)) == 1L) {
      expect_true(all(a$test_type == "t"), label = id)
      expect_equal(a$df, rep(b_df[1L], nrow(a)), label = id)
    } else {
      expect_true(all(a$df %in% b_df), label = id)
    }
  }
  expect_gt(checked, 50L)
})

test_that("lm and glm under CR2 keep their Satterthwaite AME", {
  skip_on_cran()
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("marginaleffects", minimum_version = "1.0.0")
  d <- .class_fixture_data()
  # lm: the Satterthwaite df of the AME contrast x + mean(z) x:z, none of
  # the df of the coefficients.
  td <- ard_tidy(lm(y ~ x * z, d), vcov = "CR2", cluster = .cf_cluster(NULL))
  a <- td[td$estimate_type == "ame" & td$term == "x", ]
  expect_identical(a$test_type, "t")
  expect_false(any(abs(a$df - td$df[td$estimate_type == "B"]) < 1e-6))
  # glm: the df of the dominant coefficient.
  td <- ard_tidy(
    .class_fixture("glm_logit"),
    vcov = "CR2",
    cluster = .cf_cluster(NULL)
  )
  expect_equal(
    td$df[td$estimate_type == "ame"],
    td$df[td$estimate_type == "B" & td$term == "x"]
  )
  out <- capture.output(print(table_regression(
    lm(y ~ x * z, d),
    show_columns = c("b", "ame"),
    vcov = "CR2",
    cluster = .cf_cluster(NULL)
  )))
  expect_true(any(grepl("Satterthwaite df", out, fixed = TRUE)))
  expect_false(any(grepl("smallest df", out, fixed = TRUE)))
})

# ---- Linear fits on the same data: the AME of lm -------------------------

ard_lin_data <- function() {
  set.seed(42)
  d <- data.frame(x = rnorm(24), z = rnorm(24), g = rep(1:6, each = 4))
  d$y <- 1 + 0.45 * d$x + rnorm(24)
  d
}

ard_ame_x <- function(fit) {
  td <- ard_tidy(fit)
  a <- td[td$estimate_type == "ame" & td$term == "x", ]
  c(df = a$df, p = a$p.value, lo = a$conf.low, hi = a$conf.high)
}

test_that("linear fits on the same data give the AME test of lm", {
  skip_on_cran()
  skip_if_not_installed("marginaleffects", minimum_version = "1.0.0")
  skip_if_not_installed("rms")
  skip_if_not_installed("nlme")
  skip_if_not_installed("AER")
  skip_if_not_installed("estimatr")
  d <- ard_lin_data()
  ref <- ard_ame_x(lm(y ~ x + z, d))
  expect_equal(ref[["df"]], 21)
  # marginaleffects on the same df.
  s <- marginaleffects::avg_slopes(lm(y ~ x + z, d), variables = "x", df = 21)
  expect_equal(
    unname(ref[c("p", "lo", "hi")]),
    c(s$p.value, s$conf.low, s$conf.high)
  )
  dd <- rms::datadist(d)
  withr::local_options(datadist = dd)
  fits <- list(
    ols = rms::ols(y ~ x + z, data = d),
    gls = nlme::gls(y ~ x + z, data = d),
    ivreg = AER::ivreg(y ~ x + z | x + z, data = d),
    lm_robust = estimatr::lm_robust(y ~ x + z, data = d, se_type = "classical"),
    iv_robust = estimatr::iv_robust(
      y ~ x + z | x + z,
      data = d,
      se_type = "classical"
    )
  )
  for (nm in names(fits)) {
    # The SE of the delta method differ in the fifth digit: the
    # numerical Jacobian of avg_slopes() on each class.
    expect_equal(ard_ame_x(fits[[nm]]), ref, tolerance = 1e-4, label = nm)
  }
})

# ---- Different df: the smallest of the coefficients it depends on --------

ard_lme_data <- function() {
  set.seed(7)
  d <- data.frame(g = rep(seq_len(12), each = 5))
  d$xb <- rep(rnorm(12), each = 5) # between groups
  d$z <- rnorm(60) # within groups
  d$y <- 1 +
    0.5 * d$xb +
    0.3 * d$z +
    0.2 * d$xb * d$z +
    rep(rnorm(12), each = 5) +
    rnorm(60)
  d
}

test_that("lme: the AME takes emmeans' containment df", {
  skip_on_cran()
  skip_if_not_installed("marginaleffects", minimum_version = "1.0.0")
  skip_if_not_installed("nlme")
  skip_if_not_installed("emmeans")
  d <- ard_lme_data()
  for (f in list(
    nlme::lme(y ~ xb + z, random = ~ 1 | g, data = d),
    nlme::lme(y ~ xb * z, random = ~ 1 | g, data = d)
  )) {
    td <- ard_tidy(f)
    a <- td[td$estimate_type == "ame", ]
    for (v in c("xb", "z")) {
      em <- as.data.frame(emmeans::emtrends(
        f,
        ~1,
        var = v,
        mode = "containment"
      ))
      expect_equal(a$df[a$term == v], em$df, label = v)
      expect_identical(a$test_type[a$term == v], "t")
    }
  }
  # xb (10 df) and xb:z (46 df) give the AME of xb 10 df; z and xb:z, 46.
  td <- ard_tidy(nlme::lme(y ~ xb * z, random = ~ 1 | g, data = d))
  a <- td[td$estimate_type == "ame", ]
  expect_equal(a$df[match(c("xb", "z"), a$term)], c(10, 46))
  # p and interval from the estimate, the SE and that df.
  i <- a$term == "xb"
  expect_equal(
    a$p.value[i],
    2 * stats::pt(-abs(a$estimate[i] / a$std.error[i]), 10)
  )
  expect_equal(
    a$conf.high[i],
    a$estimate[i] + stats::qt(0.975, 10) * a$std.error[i]
  )
})

test_that("lmerTest: the AME leaves out the intercept's df", {
  skip_on_cran()
  skip_if_not_installed("marginaleffects", minimum_version = "1.0.0")
  skip_if_not_installed("lmerTest")
  d <- .class_fixture_data()
  td <- ard_tidy(lmerTest::lmer(y ~ x + z + (1 | g), d))
  b <- td[td$estimate_type == "B", ]
  a <- td[td$estimate_type == "ame", ]
  expect_equal(a$df, b$df[match(c("x", "z"), b$term)])
})

# ---- The note -------------------------------------------------------------

test_that("the note says the AME test when the df differ, and only then", {
  skip_on_cran()
  skip_if_not_installed("marginaleffects", minimum_version = "1.0.0")
  skip_if_not_installed("nlme")
  d <- ard_lme_data()
  f <- nlme::lme(y ~ xb * z, random = ~ 1 | g, data = d)
  en <- "AME inference: t-test on the smallest df among the coefficients each effect depends on."
  out <- capture.output(print(table_regression(
    f,
    show_columns = c("b", "ame")
  )))
  expect_true(any(grepl(en, out, fixed = TRUE)))
  # Beside an lm, the sentence names its model.
  out <- capture.output(print(table_regression(
    list(f, lm(y ~ xb * z, d)),
    show_columns = c("b", "ame")
  )))
  expect_true(any(grepl(paste0("Model 1: ", en), out, fixed = TRUE)))
  # A shared df: no sentence.
  out <- capture.output(print(table_regression(
    lm(y ~ xb * z, d),
    show_columns = c("b", "ame")
  )))
  expect_false(any(grepl("AME inference", out, fixed = TRUE)))
  withr::local_options(spicy.language = "fr")
  out <- capture.output(print(table_regression(
    f,
    show_columns = c("b", "ame")
  )))
  expect_true(any(grepl(
    "test t sur le plus petit nombre de degr\u00e9s de libert\u00e9",
    out,
    fixed = TRUE
  )))
})
