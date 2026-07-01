# Ordinal cumulative-link thresholds rendered as a subordinate "Thresholds"
# block of rows (show_thresholds = TRUE, the default), instead of the legacy
# compact footer line. The (k-1) cut-points carry B / SE / CI / p like the
# predictor rows, cross-validated to summary.polr / summary.clm to machine
# precision, are never exponentiated, and fall back to the footer line under
# show_thresholds = FALSE.

make_ord <- function(seed = 1, n = 220) {
  set.seed(seed)
  data.frame(
    x1    = rnorm(n),
    smoke = factor(sample(c("no", "yes"), n, TRUE)),
    yc    = factor(sample(c("Poor", "Fair", "Good", "Very good"), n, TRUE),
                   levels = c("Poor", "Fair", "Good", "Very good"),
                   ordered = TRUE))
}
thr_terms <- c("Poor | Fair", "Fair | Good", "Good | Very good")

# ---- structural: thresholds become rows -----------------------------------

test_that("polr thresholds render as a labelled Thresholds block (default)", {
  skip_if_not_installed("MASS")
  fit <- MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE)
  df <- table_regression(fit, output = "data.frame")
  expect_true(any(grepl("^Thresholds", trimws(df$Variable))))   # header row
  expect_setequal(trimws(df$Variable)[grepl("\\|", df$Variable)], thr_terms)
})

test_that("clm thresholds render as a labelled Thresholds block (default)", {
  skip_if_not_installed("ordinal")
  fit <- ordinal::clm(yc ~ x1 + smoke, data = make_ord())
  df <- table_regression(fit, output = "data.frame")
  expect_true(any(grepl("^Thresholds", trimws(df$Variable))))
  expect_setequal(trimws(df$Variable)[grepl("\\|", df$Variable)], thr_terms)
})

# ---- numeric: cross-validate to summary() ---------------------------------

test_that(".append_threshold_rows matches summary.polr to machine precision", {
  skip_if_not_installed("MASS")
  fit <- MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE)
  fr <- as_regression_frame(fit, show_columns = expand_show_columns("b"),
                            ci_level = 0.95)
  cf <- .append_threshold_rows(fr$coefs, fr$info$extras$thresholds, 0.95)
  tr <- cf[cf$is_threshold %in% TRUE, , drop = FALSE]

  sm <- summary(fit)$coefficients
  zr <- sm[grepl("\\|", rownames(sm)), , drop = FALSE]
  idx <- match(tr$term, rownames(zr))
  expect_false(anyNA(idx))
  expect_equal(tr$estimate,  unname(zr[idx, "Value"]),      tolerance = 1e-10)
  expect_equal(tr$std_error, unname(zr[idx, "Std. Error"]), tolerance = 1e-10)
  expect_equal(tr$statistic, unname(zr[idx, "t value"]),    tolerance = 1e-8)

  z <- stats::qnorm(0.975)
  expect_equal(tr$ci_lower, tr$estimate - z * tr$std_error, tolerance = 1e-12)
  expect_equal(tr$ci_upper, tr$estimate + z * tr$std_error, tolerance = 1e-12)
  expect_true(all(tr$estimate_type == "B"))
  expect_true(all(tr$parent_var == "Thresholds"))
  expect_true(all(tr$df == Inf) && all(tr$test_type == "z"))
})

test_that(".append_threshold_rows matches summary.clm incl. p-value", {
  skip_if_not_installed("ordinal")
  fit <- ordinal::clm(yc ~ x1 + smoke, data = make_ord())
  fr <- as_regression_frame(fit, show_columns = expand_show_columns("b"),
                            ci_level = 0.95)
  cf <- .append_threshold_rows(fr$coefs, fr$info$extras$thresholds, 0.95)
  tr <- cf[cf$is_threshold %in% TRUE, , drop = FALSE]

  sm <- summary(fit)$coefficients
  zr <- sm[grepl("\\|", rownames(sm)), , drop = FALSE]
  idx <- match(tr$term, rownames(zr))
  expect_false(anyNA(idx))
  expect_equal(tr$estimate,  unname(zr[idx, "Estimate"]),   tolerance = 1e-10)
  expect_equal(tr$std_error, unname(zr[idx, "Std. Error"]), tolerance = 1e-10)
  expect_equal(tr$p_value,   unname(zr[idx, "Pr(>|z|)"]),   tolerance = 1e-8)
})

# ---- exponentiate: thresholds stay on the log-odds scale -------------------

test_that("thresholds are NOT exponentiated under exponentiate = TRUE", {
  skip_if_not_installed("MASS")
  fit <- MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE)
  df <- table_regression(fit, exponentiate = TRUE, output = "data.frame")
  est_col <- setdiff(names(df), "Variable")[1]      # "OR" header under exp
  tr <- df[trimws(df$Variable) == "Poor | Fair", , drop = FALSE]
  shown <- as.numeric(trimws(tr[[est_col]]))
  raw <- unname(summary(fit)$coefficients["Poor|Fair", "Value"])
  # the displayed value is the RAW cut-point (here negative), NOT exp(raw) > 0
  expect_equal(shown, round(raw, 2), tolerance = 0.005)
  expect_lt(shown, 0)
})

# ---- footer builder: rows mode vs footer mode -----------------------------

test_that("footer builder switches between gloss (rows) and compact line", {
  skip_if_not_installed("MASS")
  fit <- MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE)
  fr <- as_regression_frame(fit, show_columns = expand_show_columns("b"),
                            ci_level = 0.95)
  # footer mode: no is_threshold rows -> compact numeric line
  compact <- build_ordinal_thresholds_footer_block_from_frames(list(fr))
  expect_match(compact, "Thresholds: .*= ")
  expect_match(compact, "\\|")

  # rows mode: is_threshold rows present -> one-line gloss, no numeric cuts
  fr2 <- fr
  fr2$coefs <- .append_threshold_rows(fr$coefs, fr$info$extras$thresholds, 0.95)
  gloss <- build_ordinal_thresholds_footer_block_from_frames(list(fr2))
  expect_match(gloss, "latent-scale category cut-points")
  expect_false(grepl("= -?[0-9]", gloss))
})

# ---- opt-out + gating ------------------------------------------------------

test_that("show_thresholds = FALSE drops the rows (footer fallback)", {
  skip_if_not_installed("MASS")
  fit <- MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE)
  df <- table_regression(fit, show_thresholds = FALSE, output = "data.frame")
  expect_false(any(grepl("\\|", df$Variable)))
  expect_false(any(grepl("^Thresholds", trimws(df$Variable))))
})

test_that("ame-only show_columns does not promote thresholds to empty rows", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("marginaleffects")
  fit <- MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE)
  df <- table_regression(fit, show_columns = "ame", output = "data.frame")
  expect_false(any(grepl("\\|", df$Variable)))
})

test_that("non-ordinal fits are unaffected by show_thresholds", {
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  df <- table_regression(fit, output = "data.frame")
  expect_false(any(grepl("Thresholds", df$Variable)))
})

# ---- section separator before the Thresholds block ------------------------

test_that("a section separator is emitted before the Thresholds block", {
  skip_if_not_installed("MASS")
  fit <- MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE)
  res <- table_regression(fit)                       # default (printable) output
  sep <- attr(res, "section_sep_rows")
  expect_length(sep, 1L)
  expect_gt(sep, 1L)
  # nothing when the block is opted out
  res0 <- table_regression(fit, show_thresholds = FALSE)
  expect_length(attr(res0, "section_sep_rows"), 0L)
})

# ---- ordinal fit-stats default --------------------------------------------

test_that("polr/clm default fit-stats = n + McFadden + Nagelkerke + AIC", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  for (fit in list(MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE),
                   ordinal::clm(yc ~ x1 + smoke, data = make_ord()))) {
    df <- table_regression(fit, output = "data.frame")
    v <- trimws(df$Variable)
    expect_true(any(v == "n"))
    expect_true(any(grepl("McFadden", v)))
    expect_true(any(grepl("Nagelkerke", v)))
    expect_true(any(v == "AIC"))
  }
})

test_that(".ordinal_pseudo_r2 matches the closed-form null and performance", {
  skip_if_not_installed("MASS")
  d <- make_ord()
  fit <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
  pr <- .ordinal_pseudo_r2(fit)

  # self-contained closed-form null log-likelihood (marginal proportions):
  # an intercept-only cumulative model reproduces the category frequencies.
  ll  <- as.numeric(stats::logLik(fit))
  nk  <- as.numeric(table(d$yc)); nk <- nk[nk > 0]
  ll0 <- sum(nk * log(nk / sum(nk)))
  n   <- stats::nobs(fit)
  expect_equal(pr$mcfadden, 1 - ll / ll0, tolerance = 1e-9)
  cs <- 1 - exp((ll0 - ll) * 2 / n); up <- 1 - exp(ll0 * 2 / n)
  expect_equal(pr$nagelkerke, cs / up, tolerance = 1e-9)

  skip_if_not_installed("performance")
  expect_equal(pr$mcfadden,
               as.numeric(performance::r2_mcfadden(fit)$R2), tolerance = 1e-6)
  expect_equal(pr$nagelkerke,
               as.numeric(performance::r2_nagelkerke(fit)), tolerance = 1e-6)
})

test_that("ordinal pseudo-R2 survives a refit-hostile scope (update() would fail)", {
  skip_if_not_installed("MASS")
  # table_regression() called inside a function with data local to that
  # function: update(fit, . ~ 1) can't re-find the data, but the closed-form
  # null log-likelihood does not refit, so the pseudo-R2 is still computed.
  f <- function() {
    d <- make_ord()
    fit <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
    table_regression(fit, output = "data.frame")
  }
  df <- f()
  mcf <- trimws(df[grepl("McFadden", df$Variable), 2])
  expect_true(length(mcf) == 1L && nzchar(mcf) && mcf != "NA")
})
