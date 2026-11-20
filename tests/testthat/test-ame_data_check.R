# The AME is taken on the data of the fit (.ame_data_check()):
# marginaleffects reads the data again when the table is built, so a data
# frame changed after the fit (rows dropped, values recoded) refuses the
# AME with a spicy_fallback warning and leaves the coefficients as they
# are. On unchanged data the AME is the one computed without the check,
# in every case where the data of the fit legitimately differ from the
# data frame (missing values, subset, weights, offsets, transforms, data
# built in the call, fits made in a function).

skip_if_no_adc <- function() {
  skip_if_not_installed("marginaleffects", minimum_version = "1.0.0")
}

adc_data <- function(n = 200, seed = 20261120) {
  set.seed(seed)
  d <- data.frame(
    x = stats::rnorm(n),
    z = stats::runif(n, 1, 3),
    g = factor(sample(c("a", "b", "c"), n, TRUE)),
    id = factor(rep(seq_len(20), length.out = n)),
    w = stats::runif(n, 0.5, 2),
    e = stats::runif(n, 1, 4)
  )
  u <- stats::rnorm(20)[as.integer(d$id)]
  d$y <- 1 + 0.5 * d$x + 0.3 * d$z^2 + as.numeric(d$g) / 4 + u + stats::rnorm(n)
  d$yb <- stats::rbinom(n, 1, stats::plogis(0.6 * d$x - 0.2 * d$z + u))
  d$cnt <- stats::rpois(n, exp(0.2 * d$x) * d$e)
  d$ord <- cut(
    d$y + stats::rnorm(n),
    3,
    labels = c("lo", "mid", "hi"),
    ordered_result = TRUE
  )
  d
}

# The long table of a fit with its B and AME rows, and the classes of
# the warnings it raised (muffled).
adc_run <- function(fit, ...) {
  cls <- character()
  lo <- withCallingHandlers(
    as.data.frame(table_regression(
      fit,
      show_columns = c("b", "ame"),
      output = "long",
      ...
    )),
    warning = function(w) {
      cls <<- c(cls, class(w))
      invokeRestart("muffleWarning")
    }
  )
  a <- lo[lo$estimate_type == "ame" & !(lo$is_reference %in% TRUE), ]
  b <- lo[lo$estimate_type != "ame" & !(lo$is_reference %in% TRUE), ]
  list(
    ame = stats::setNames(a$estimate, paste(a$term, a$outcome_level)),
    b = stats::setNames(b$estimate, paste(b$term, b$outcome_level)),
    warnings = cls
  )
}

# The same without the data check, as before it existed.
adc_run_unchecked <- function(fit, ...) {
  testthat::local_mocked_bindings(
    .ame_data_check = function(model, data) NULL,
    .package = "spicy"
  )
  adc_run(fit, ...)
}

# The data changed after the fit: the AME is refused with its reason,
# the coefficients are those of the fit.
expect_ame_refused <- function(r, b_ref) {
  expect_true("spicy_fallback" %in% r$warnings)
  expect_true(length(r$ame) == 0L || all(is.na(r$ame)))
  expect_equal(r$b, b_ref, tolerance = 1e-10)
}

# The data are those of the fit: the AME comes out, equal to the one
# computed without the check, and no fallback warning is raised.
expect_ame_kept <- function(fit, ...) {
  r <- adc_run(fit, ...)
  expect_false("spicy_fallback" %in% r$warnings)
  expect_gt(length(r$ame), 0L)
  expect_true(all(is.finite(r$ame)))
  expect_equal(r$ame, adc_run_unchecked(fit, ...)$ame, tolerance = 1e-10)
  invisible(r)
}

adc_message <- function(fit, ...) {
  w <- NULL
  withCallingHandlers(
    table_regression(fit, show_columns = c("b", "ame"), ...),
    spicy_fallback = function(c) {
      w <<- conditionMessage(c)
      invokeRestart("muffleWarning")
    }
  )
  w
}


# ---- (a) rows dropped, (b) values changed, (c) unchanged, by class ---------

adc_classes <- list(
  lm = list(
    pkgs = NULL,
    fit = quote(lm(y ~ x + I(z^2) + g, data = d))
  ),
  glm_logit = list(
    pkgs = NULL,
    fit = quote(glm(yb ~ x + z, family = binomial, data = d))
  ),
  lmer = list(
    pkgs = "lme4",
    fit = quote(lme4::lmer(y ~ x + z + (1 | id), data = d))
  ),
  polr = list(
    pkgs = "MASS",
    fit = quote(MASS::polr(ord ~ x + z, data = d, Hess = TRUE))
  ),
  feols = list(
    pkgs = "fixest",
    fit = quote(fixest::feols(y ~ x + z | g, data = d))
  )
)

for (cl in names(adc_classes)) {
  local({
    spec <- adc_classes[[cl]]
    test_that(paste0("the AME of a ", cl, " follows the data of the fit"), {
      skip_if_no_adc()
      for (p in spec$pkgs) {
        skip_if_not_installed(p)
      }
      d <- adc_data()
      fit <- suppressMessages(eval(spec$fit))
      # (c) unchanged data: the AME of before the check.
      ref <- expect_ame_kept(fit)
      d0 <- d

      # (a) rows dropped after the fit.
      d <- d0[-(1:10), ]
      r <- adc_run(fit)
      expect_ame_refused(r, ref$b)
      m <- adc_message(fit)
      expect_match(m, "190 rows now, 200 when fitted", fixed = TRUE)
      expect_match(m, "probably modified after the model was fitted")

      # (b) the values of a predictor changed, as many rows.
      d <- d0
      d$x <- d$x * 2
      r <- adc_run(fit)
      expect_ame_refused(r, ref$b)
      expect_match(
        adc_message(fit),
        "predictions on the data of the fit no longer match",
        fixed = TRUE
      )

      # The data restored: the AME is back.
      d <- d0
      expect_equal(adc_run(fit)$ame, ref$ame, tolerance = 1e-10)
    })
  })
}

test_that("the AME of a svyglm is taken on the design, whatever the data frame", {
  skip_if_no_adc()
  skip_if_not_installed("survey")
  d <- adc_data()
  des <- survey::svydesign(id = ~1, weights = ~w, data = d)
  fit <- survey::svyglm(y ~ x + I(z^2), design = des)
  ref <- expect_ame_kept(fit)
  # The design holds its own copy of the data: changing the data frame
  # after the fit changes neither the AME nor the coefficients.
  d <- d[-(1:10), ]
  r <- adc_run(fit)
  expect_false("spicy_fallback" %in% r$warnings)
  expect_equal(r$ame, ref$ame, tolerance = 1e-10)
  d$x <- d$x * 2
  expect_equal(adc_run(fit)$ame, ref$ame, tolerance = 1e-10)
})

test_that("one warning per model, the other models keep their AME", {
  skip_if_no_adc()
  d <- adc_data()
  f1 <- lm(y ~ x + I(z^2), data = d)
  d2 <- d
  f2 <- glm(yb ~ x + z, family = binomial, data = d2)
  ref <- adc_run(f2)$ame
  d <- d[-(1:10), ]
  n <- 0L
  lo <- withCallingHandlers(
    as.data.frame(table_regression(
      list(f1, f2),
      show_columns = c("b", "ame"),
      output = "long"
    )),
    spicy_fallback = function(c) {
      n <<- n + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(n, 1L)
  a <- lo[lo$estimate_type == "ame" & !is.na(lo$estimate), ]
  expect_setequal(unique(a$model_id), "M2")
  expect_equal(unname(a$estimate), unname(ref), tolerance = 1e-10)
})


# ---- The data of the fit differ from the data frame, legitimately ---------

test_that("missing values dropped by the fit keep the AME", {
  skip_if_no_adc()
  d <- adc_data()
  d$x[c(3, 7, 11)] <- NA
  expect_ame_kept(lm(y ~ x + I(z^2), data = d))
  expect_ame_kept(lm(y ~ x + I(z^2), data = d, na.action = stats::na.exclude))
  expect_ame_kept(
    glm(yb ~ x + z, binomial, data = d, na.action = stats::na.exclude)
  )
})

test_that("a subset, weights and zero weights keep the AME", {
  skip_if_no_adc()
  d <- adc_data()
  expect_ame_kept(lm(y ~ x + I(z^2), data = d, subset = g != "a"))
  expect_ame_kept(lm(y ~ x + I(z^2), data = d, weights = w))
  d$w[1:5] <- 0
  expect_ame_kept(lm(y ~ x + I(z^2), data = d, weights = w))
  expect_ame_kept(suppressWarnings(
    glm(yb ~ x + z, quasibinomial, data = d, weights = w)
  ))
})

test_that("an offset keeps the AME, in the formula or as an argument", {
  skip_if_no_adc()
  d <- adc_data()
  expect_ame_kept(glm(cnt ~ x + z + offset(log(e)), poisson, data = d))
  expect_ame_kept(glm(cnt ~ x + z, poisson, data = d, offset = log(e)))
  skip_if_not_installed("MASS")
  # predict() of a polr leaves the offset out of a prediction on data:
  # its rows are counted, its predictions not compared.
  expect_ame_kept(MASS::polr(ord ~ x + offset(z / 3), data = d, Hess = TRUE))
})

test_that("transformations in the formula keep the AME", {
  skip_if_no_adc()
  skip_if_not_installed("splines")
  d <- adc_data()
  expect_ame_kept(lm(
    y ~ log(z) + poly(x, 2) + splines::ns(z, 2) + I(x^3),
    data = d
  ))
})

test_that("data built in the call, under another name or in a function keep the AME", {
  skip_if_no_adc()
  d <- adc_data()
  expect_ame_kept(lm(y ~ x + I(z^2), data = d[d$g != "a", ]))
  expect_ame_kept(lm(y ~ x + I(z^2), data = subset(d, g != "b")))
  dd <- d
  expect_ame_kept(lm(y ~ x + I(z^2), data = dd))
  fit_in <- function(data) lm(y ~ x + I(z^2), data = data)
  expect_ame_kept(fit_in(d))
  # A data frame built in the call is built again from the current
  # data: a change to them is seen.
  fit <- lm(y ~ x + I(z^2), data = d[d$g != "a", ])
  b <- adc_run(fit)$b
  d$x <- d$x + 1
  expect_ame_refused(adc_run(fit), b)
})

test_that("mixed, multinomial, ordinal and zero-inflated fits keep the AME", {
  skip_if_no_adc()
  d <- adc_data()
  if (requireNamespace("lme4", quietly = TRUE)) {
    expect_ame_kept(lme4::glmer(yb ~ x + z + (1 | id), binomial, data = d))
  }
  if (requireNamespace("nnet", quietly = TRUE)) {
    expect_ame_kept(nnet::multinom(g ~ x + z, data = d, trace = FALSE))
  }
  if (requireNamespace("ordinal", quietly = TRUE)) {
    expect_ame_kept(ordinal::clm(ord ~ x + z, data = d))
  }
  if (requireNamespace("pscl", quietly = TRUE)) {
    expect_ame_kept(pscl::zeroinfl(cnt ~ x + z | 1, data = d))
  }
  succeed()
})


# ---- Rows renamed, data gone ----------------------------------------------

test_that("rows renamed after the fit keep the Satterthwaite AME of an lm", {
  skip_if_no_adc()
  skip_if_not_installed("clubSandwich")
  d <- adc_data()
  fit <- lm(y ~ x + I(z^2), data = d)
  ref <- adc_run(fit, vcov = "CR2", cluster = d$id)
  rownames(d) <- paste0("r", seq_len(nrow(d)))
  # The same values in the same order: the AME is computed, without the
  # fallback of an unexpected failure.
  r <- adc_run(fit, vcov = "CR2", cluster = d$id)
  expect_false("spicy_fallback" %in% r$warnings)
  expect_equal(r$ame, ref$ame, tolerance = 1e-10)
})

test_that("an lm whose data frame is gone keeps the AME its model frame gives", {
  skip_if_no_adc()
  e <- new.env()
  e$d_gone <- adc_data()
  fit <- eval(quote(lm(y ~ x + z + g, data = d_gone)), e)
  ref <- adc_run(fit)$ame
  rm("d_gone", envir = e)
  # The model frame holds every variable: the AME is the one of the fit.
  r <- adc_run(fit)
  expect_false("spicy_fallback" %in% r$warnings)
  expect_equal(r$ame, ref, tolerance = 1e-10)
})

test_that("a transformed predictor whose data frame is gone refuses the AME", {
  skip_if_no_adc()
  e <- new.env()
  e$d_gone <- adc_data()
  fit <- eval(quote(lm(y ~ x + I(z^2), data = d_gone)), e)
  b <- adc_run(fit)$b
  rm("d_gone", envir = e)
  # The model frame holds z^2, not z: marginaleffects took the AME of z
  # on the squares.
  expect_ame_refused(adc_run(fit), b)
  m <- adc_message(fit)
  expect_match(m, "the data of the fit are no longer found", fixed = TRUE)
  expect_match(m, "probably removed or renamed", fixed = TRUE)
})
