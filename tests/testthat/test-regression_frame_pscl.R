# ---------------------------------------------------------------------------
# Phase 6c tests: as_regression_frame() methods for pscl hurdle / zeroinfl.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_hurdle_basic <- function() {
  skip_if_not_installed("pscl")
  data(bioChemists, package = "pscl", envir = environment())
  pscl::hurdle(art ~ fem + mar | fem, data = bioChemists)
}

.fit_zeroinfl_basic <- function() {
  skip_if_not_installed("pscl")
  data(bioChemists, package = "pscl", envir = environment())
  pscl::zeroinfl(art ~ fem + mar | fem, data = bioChemists)
}

.fit_hurdle_negbin <- function() {
  skip_if_not_installed("pscl")
  data(bioChemists, package = "pscl", envir = environment())
  pscl::hurdle(art ~ fem + mar, dist = "negbin", data = bioChemists)
}


# ---- 1. hurdle: schema validity + core fields ----------------------------

test_that("as_regression_frame.hurdle produces a schema-valid frame", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("hurdle: required attributes are attached", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(attr(fr, "spicy_frame_version"), spicy_frame_version())
  expect_identical(attr(fr, "fit"), fit)
})

test_that("hurdle: info$class is 'hurdle'", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "hurdle")
})

test_that("hurdle Poisson: info$family is poisson/log; title names Poisson hurdle", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$family$family, "poisson")
  expect_identical(fr$info$family$link, "log")
  expect_identical(fr$info$extras$title_prefix, "Poisson hurdle regression")
})

test_that("hurdle: info$n_obs = fit$n", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$n_obs, as.integer(fit$n))
})

test_that("hurdle negbin: title_prefix names Negative-binomial", {
  fit <- .fit_hurdle_negbin()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_match(fr$info$extras$title_prefix, "Negative-binomial", fixed = TRUE)
})


# ---- 2. hurdle: count-component coef extraction --------------------------

test_that("hurdle: coefs estimates match coef(fit, model='count')", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit, model = "count")
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(legacy[nm]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})

test_that("hurdle: p-values match summary(fit)$coefficients$count byte-equivalent", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  smc <- summary(fit)$coefficients$count
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in rownames(smc)) {
    expect_equal(
      b_rows$p_value[b_rows$term == nm],
      unname(smc[nm, "Pr(>|z|)"]),
      tolerance = 1e-10,
      info = paste("term:", nm)
    )
  }
})


# ---- 3. hurdle: factor detection works through prefix mismatch -----------

test_that("hurdle: factor predictor synthesizes a reference row", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  fem_rows <- fr$coefs[fr$coefs$parent_var == "fem", ]
  expect_identical(nrow(fem_rows), 2L)
  expect_identical(sum(fem_rows$is_ref), 1L)
  expect_identical(fem_rows$label[fem_rows$is_ref], "Men")
})


# ---- 4. hurdle: inference + supports -------------------------------------

test_that("hurdle: Wald z-asymptotic (test_type='z', df=Inf)", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  b_rows <- fr$coefs[!fr$coefs$is_ref, ]
  expect_true(all(b_rows$test_type == "z"))
  expect_true(all(is.infinite(b_rows$df)))
  expect_identical(fr$info$ci_method, "wald")
})

test_that("hurdle: supports$exponentiate = TRUE (IRR for log link)", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$supports$exponentiate)
})


# ---- 5. hurdle: zero component in extras ---------------------------------

test_that("hurdle: info$extras$has_zi = TRUE; component block populated", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(fr$info$extras$has_zi)
  blocks <- fr$info$extras$component_blocks
  expect_true(is.list(blocks) && length(blocks) == 1L)
  blk <- blocks[[1L]]
  expect_identical(blk$label, "Zero hurdle")
  # one fully-inferenced row per zero coefficient, zero_-prefixed terms
  zc <- stats::coef(fit, model = "zero")
  non_ref <- blk$coefs[!blk$coefs$is_ref, , drop = FALSE]
  expect_setequal(non_ref$term, paste0("zero_", names(zc)))
  expect_true(all(is.finite(non_ref$std_error)))
})

test_that("hurdle: zero_dist + zero_link surfaced", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$extras$zero_dist, "binomial")
  expect_true(is.character(fr$info$extras$zero_link))
})


# ---- 6. zeroinfl: schema validity ----------------------------------------

test_that("as_regression_frame.zeroinfl produces a schema-valid frame", {
  fit <- .fit_zeroinfl_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_invisible(spicy:::validate_regression_frame(fr))
})

test_that("zeroinfl: info$class is 'zeroinfl'", {
  fit <- .fit_zeroinfl_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(fr$info$class, "zeroinfl")
})

test_that("zeroinfl Poisson: title names the zero-inflated Poisson model", {
  fit <- .fit_zeroinfl_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_identical(
    fr$info$extras$title_prefix,
    "Zero-inflated Poisson regression"
  )
})

test_that("zeroinfl: coefs estimates match coef(fit, model='count')", {
  fit <- .fit_zeroinfl_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  legacy <- stats::coef(fit, model = "count")
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  for (nm in names(legacy)) {
    expect_equal(
      b_rows$estimate[b_rows$term == nm],
      unname(legacy[nm]),
      tolerance = 1e-10
    )
  }
})


# ---- 7. fit stats --------------------------------------------------------

test_that("hurdle: AIC / BIC / logLik finite", {
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")
  expect_true(is.finite(fr$info$fit_stats$aic))
  expect_true(is.finite(fr$info$fit_stats$bic))
  expect_true(is.finite(fr$info$fit_stats$log_lik))
})


# ---- 8. Oracle: parameters::model_parameters() ---------------------------

test_that("hurdle count-component coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci = 0.95,
    exponentiate = FALSE,
    component = "conditional"
  )

  # parameters prefixes every hurdle parameter with its component --
  # `count_` here, `zero_` in the twin below. The frame keeps the
  # model's bare term names and separates the components structurally,
  # so without the prefix the join missed on EVERY term, the loop
  # `next`ed past all of its expectations, and this block reported as
  # skipped while checking nothing at all (register n. 243). The
  # numbers were right the whole time; only the guard was dead.
  b_rows <- fr$coefs[fr$coefs$estimate_type == "B" & !fr$coefs$is_ref, ]
  n_checked <- 0L
  for (nm in b_rows$term) {
    oracle_row <- oracle[oracle$Parameter == paste0("count_", nm), ]
    expect_identical(
      nrow(oracle_row),
      1L,
      info = paste("no count_ oracle row for term:", nm)
    )
    spicy_row <- b_rows[b_rows$term == nm, ]
    expect_equal(
      spicy_row$estimate,
      oracle_row$Coefficient,
      tolerance = 1e-6,
      info = paste("oracle B mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$std_error,
      oracle_row$SE,
      tolerance = 1e-6,
      info = paste("oracle SE mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$p_value,
      oracle_row$p,
      tolerance = 1e-6,
      info = paste("oracle p mismatch on term:", nm)
    )
    n_checked <- n_checked + 1L
  }
  # Every predictor row compared, not merely one of them.
  expect_oracle_covered(n_checked, nrow(b_rows))
})

test_that("hurdle zero-component coefs match parameters::model_parameters() (oracle)", {
  skip_if_not_installed("parameters")
  fit <- .fit_hurdle_basic()
  fr <- as_regression_frame(fit, model_id = "M1")

  oracle <- parameters::model_parameters(
    fit,
    ci = 0.95,
    exponentiate = FALSE,
    component = "zero_inflated"
  )

  # The zero-hurdle half of the model lives in
  # `info$extras$component_blocks`, not in `coefs`, and its terms are
  # already `zero_`-prefixed -- so this side of the model had no oracle
  # at all while the count side had a dead one.
  blocks <- fr$info$extras$component_blocks
  expect_identical(length(blocks), 1L)
  zb <- blocks[[1L]]$coefs
  zb <- zb[!zb$is_ref, ]
  n_checked <- 0L
  for (nm in zb$term) {
    oracle_row <- oracle[oracle$Parameter == nm, ]
    expect_identical(
      nrow(oracle_row),
      1L,
      info = paste("no zero_ oracle row for term:", nm)
    )
    spicy_row <- zb[zb$term == nm, ]
    expect_equal(
      spicy_row$estimate,
      oracle_row$Coefficient,
      tolerance = 1e-6,
      info = paste("oracle B mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$std_error,
      oracle_row$SE,
      tolerance = 1e-6,
      info = paste("oracle SE mismatch on term:", nm)
    )
    expect_equal(
      spicy_row$p_value,
      oracle_row$p,
      tolerance = 1e-6,
      info = paste("oracle p mismatch on term:", nm)
    )
    n_checked <- n_checked + 1L
  }
  expect_oracle_covered(n_checked, nrow(zb))
})


# ---- 8. negative-binomial fits: the theta / alpha rows -----------------------

test_that("negative-binomial pscl fits carry the theta of their count part, as glmmTMB", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("glmmTMB")
  set.seed(20261104)
  n <- 400
  d <- data.frame(x = stats::rnorm(n))
  d$y <- stats::rnbinom(n, mu = exp(0.5 + 0.4 * d$x), size = 1.5)
  d$y[stats::runif(n) < 0.25] <- 0L
  # A tight reltol: under pscl's default (about 1.6e-10) the zeroinfl theta
  # is off by 1.6e-4 (relative) here.
  z <- pscl::zeroinfl(
    y ~ x | 1,
    data = d,
    dist = "negbin",
    control = pscl::zeroinfl.control(reltol = 1e-12)
  )
  h <- pscl::hurdle(
    y ~ x | 1,
    data = d,
    dist = "negbin",
    control = pscl::hurdle.control(reltol = 1e-12)
  )
  # The engines' own values, exactly: a number for zeroinfl, the "count"
  # element for a hurdle.
  theta_z <- z$theta
  theta_h <- unname(h$theta["count"])
  fz <- as_regression_frame(z, model_id = "M1")$info$fit_stats
  fh <- as_regression_frame(h, model_id = "M1")$info$fit_stats
  expect_identical(fz$theta, theta_z)
  expect_identical(fz$alpha, 1 / theta_z)
  expect_identical(fh$theta, theta_h)
  expect_identical(fh$alpha, 1 / theta_h)
  # The same models in glmmTMB, whose sigma() is the NB2 theta: nbinom2
  # with a ziformula, and truncated_nbinom2 for the hurdle. Same
  # likelihood, same theta.
  tz <- glmmTMB::glmmTMB(
    y ~ x,
    ziformula = ~1,
    data = d,
    family = glmmTMB::nbinom2
  )
  th <- glmmTMB::glmmTMB(
    y ~ x,
    ziformula = ~1,
    data = d,
    family = glmmTMB::truncated_nbinom2
  )
  expect_equal(
    as.numeric(stats::logLik(z)),
    as.numeric(stats::logLik(tz)),
    tolerance = 1e-6
  )
  expect_equal(
    as.numeric(stats::logLik(h)),
    as.numeric(stats::logLik(th)),
    tolerance = 1e-6
  )
  expect_equal(fz$theta, stats::sigma(tz), tolerance = 1e-5)
  expect_equal(fh$theta, stats::sigma(th), tolerance = 1e-5)
  # In a table, the rows are filled (they were blank, and a pscl fit
  # alone was refused).
  df <- as.data.frame(table_regression(
    list(ZINB = z, HNB = h),
    show_fit_stats = c("theta", "alpha")
  ))
  row <- function(tk) df[df$variable == tk & df$.row_role == "fit_stat", ]
  expect_equal(row("theta")[["ZINB: B"]], theta_z)
  expect_equal(row("theta")[["HNB: B"]], theta_h)
  expect_equal(row("alpha")[["ZINB: B"]], 1 / theta_z)
  expect_equal(row("alpha")[["HNB: B"]], 1 / theta_h)
})

test_that("the theta of a negbin zero hurdle is not the count dispersion", {
  skip_if_not_installed("pscl")
  set.seed(20261104)
  n <- 400
  d <- data.frame(x = stats::rnorm(n))
  d$y <- stats::rnbinom(n, mu = exp(0.5 + 0.4 * d$x), size = 1.5)
  d$y[stats::runif(n) < 0.25] <- 0L
  # Poisson counts behind a negbin zero hurdle: pscl stores the theta of
  # the zero part only.
  hp <- pscl::hurdle(
    y ~ x | x,
    data = d,
    dist = "poisson",
    zero.dist = "negbin"
  )
  expect_named(hp$theta, "zero")
  expect_null(as_regression_frame(hp, model_id = "M1")$info$fit_stats$theta)
  expect_error(
    table_regression(hp, show_fit_stats = "theta"),
    "no model in the table",
    class = "spicy_invalid_input"
  )
  # Both parts negbin: the row is the count theta.
  hn <- pscl::hurdle(y ~ x | x, data = d, dist = "negbin", zero.dist = "negbin")
  expect_named(hn$theta, c("count", "zero"))
  expect_identical(
    as_regression_frame(hn, model_id = "M1")$info$fit_stats$theta,
    unname(hn$theta["count"])
  )
  # A Poisson zeroinfl has no theta.
  zp <- pscl::zeroinfl(y ~ x | 1, data = d)
  expect_null(zp$theta)
  expect_null(as_regression_frame(zp, model_id = "M1")$info$fit_stats$theta)
})

test_that("a censored zero hurdle names its distribution, not pscl's code", {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl", envir = environment())
  gloss <- function(zero_dist, lang) {
    withr::local_options(spicy.language = lang)
    fit <- pscl::hurdle(
      art ~ fem | fem,
      data = bioChemists,
      zero.dist = zero_dist
    )
    # The code pscl stores is what the note used to print.
    expect_identical(fit$dist$zero, zero_dist)
    fr <- as_regression_frame(fit, model_id = "M1")
    fr$info$extras$component_blocks[[1L]]$gloss
  }
  expect_identical(
    gloss("negbin", "en"),
    "Zero hurdle component: right-censored negative binomial on the log scale."
  )
  expect_identical(
    gloss("poisson", "en"),
    "Zero hurdle component: right-censored Poisson on the log scale."
  )
  expect_identical(
    gloss("geometric", "en"),
    "Zero hurdle component: right-censored geometric on the log scale."
  )
  fr_open <- "Composante \u00AB\u00A0Hurdle (z\u00E9ro)\u00A0\u00BB\u00A0: loi "
  fr_close <- " censur\u00E9e \u00E0 droite sur l'\u00E9chelle logarithmique."
  expect_identical(
    gloss("negbin", "fr"),
    paste0(fr_open, "binomiale n\u00E9gative", fr_close)
  )
  expect_identical(
    gloss("poisson", "fr"),
    paste0(fr_open, "de Poisson", fr_close)
  )
  expect_identical(
    gloss("geometric", "fr"),
    paste0(fr_open, "g\u00E9om\u00E9trique", fr_close)
  )
  # The rendered note carries the same sentence.
  withr::local_options(width = 200)
  fit <- pscl::hurdle(art ~ fem | fem, data = bioChemists, zero.dist = "negbin")
  out <- paste(capture.output(print(table_regression(fit))), collapse = "\n")
  expect_match(out, "right-censored negative binomial", fixed = TRUE)
  expect_no_match(out, "negbin", fixed = TRUE)
})
