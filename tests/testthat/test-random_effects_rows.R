# Random effects as a subordinate "Random effects" block of table rows (D4,
# 2026-07). Variance components (SD / variance / correlation / residual) render
# as rows with estimate_type = "vc" and NO per-row p (H0: sigma = 0 is on the
# boundary; the model-level chi-bar-squared LRT in the footer is the
# significance signal). Cross-validated to lme4::VarCorr. Rationale +
# exhaustive source review: dev/mixed_random_effects_rows_spec.md.

.rr_lmer_slope <- function() {
  skip_if_not_installed("lme4")
  lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
}

# The vc rows are materialised by the orchestrator (not as_regression_frame);
# read them back via broom::tidy(), which exposes estimate_type.
.vc_rows <- function(fit, ...) {
  skip_if_not_installed("broom")
  # suppressWarnings: singular test fixtures now emit the build-time
  # spicy_caveat advice; these tests target the vc rows, not the caveat.
  td <- broom::tidy(suppressWarnings(table_regression(fit, ...)))
  td[td$estimate_type == "vc", , drop = FALSE]
}

## ---- 1. Rows block renders, last, with the right labels -------------------

test_that("RE variance components render as a 'Random effects' rows block", {
  fit <- .rr_lmer_slope()
  df <- table_regression(fit, output = "data.frame")
  v <- trimws(df$Variable)
  expect_true(any(grepl("Random effects", v, fixed = TRUE))) # block header
  expect_true(any(grepl("Subject (Intercept)", v, fixed = TRUE))) # SD intercept
  expect_true(any(grepl("(Residual)", v, fixed = TRUE))) # residual SD
  expect_true(any(grepl(", Days)", v, fixed = TRUE))) # correlation
  # the RE block sorts LAST -- after every fixed-effect row
  expect_gt(
    which(grepl("Random effects", v, fixed = TRUE))[1],
    which(v == "Days")[1]
  )
})

## ---- 2. vc token, no per-row p, values match VarCorr (SD scale) -----------

test_that("vc rows carry estimate_type 'vc', no p, and match VarCorr on the SD scale", {
  skip_if_not_installed("lme4")
  fit <- .rr_lmer_slope()
  vc <- .vc_rows(fit)
  expect_gt(nrow(vc), 0L)
  expect_true(all(vc$estimate_type == "vc"))
  expect_true(all(is.na(vc$p.value))) # no per-row p (boundary)

  V <- lme4::VarCorr(fit)
  sd_int <- attr(V$Subject, "stddev")[["(Intercept)"]]
  sd_slope <- attr(V$Subject, "stddev")[["Days"]]
  sigma <- attr(V, "sc") # residual SD
  ests <- vc$estimate
  expect_true(any(abs(ests - sd_int) < 1e-6))
  expect_true(any(abs(ests - sd_slope) < 1e-6))
  expect_true(any(abs(ests - sigma) < 1e-6))
})

## ---- 3. re_scale = "variance" ---------------------------------------------

test_that("re_scale = 'variance' puts variances (not SDs) on the vc rows", {
  skip_if_not_installed("lme4")
  fit <- .rr_lmer_slope()
  vc_var <- .vc_rows(fit, re_scale = "variance")
  V <- lme4::VarCorr(fit)
  var_int <- diag(V$Subject)[["(Intercept)"]]
  expect_true(any(abs(vc_var$estimate - var_int) < 1e-5))
})

## ---- 4. exponentiate never touches vc rows --------------------------------

test_that("exponentiate leaves the vc rows on their native scale (glmer)", {
  skip_if_not_installed("lme4")
  d <- mtcars
  d$cyl <- factor(d$cyl)
  m <- suppressWarnings(suppressMessages(
    lme4::glmer(am ~ mpg + (1 | cyl), data = d, family = binomial)
  ))
  vc0 <- .vc_rows(m)
  vcx <- .vc_rows(m, exponentiate = TRUE)
  expect_equal(vc0$estimate, vcx$estimate, tolerance = 1e-9)
})

## ---- 5. Footer: chi-bar-squared LR test, no per-row p ---------------------

test_that("footer carries the chi-bar-squared LR test and the RE summary", {
  fit <- .rr_lmer_slope()
  out <- paste(capture.output(print(table_regression(fit))), collapse = "\n")
  expect_match(out, "Random effects (REML):", fixed = TRUE)
  expect_match(out, "LR test", fixed = TRUE)
})

## ---- 6. show_re = FALSE ----------------------------------------------------

test_that("show_re = FALSE removes RE rows and the footer summary", {
  fit <- .rr_lmer_slope()
  df <- table_regression(fit, show_re = FALSE, output = "data.frame")
  expect_false(any(grepl("Random effects", df$Variable, fixed = TRUE)))
})

## ---- 7. Folded-in findings m3 / m4 ----------------------------------------

test_that("m3: an invalid re_scale errors fail-fast with spicy_invalid_input", {
  expect_error(
    table_regression(lm(mpg ~ wt, mtcars), re_scale = "bogus"),
    class = "spicy_invalid_input"
  )
})

test_that("m4: lm-only fit-stat tokens on a mixed fit are rejected with a pointer", {
  fit <- .rr_lmer_slope()
  expect_error(
    table_regression(fit, show_fit_stats = c("r2"), output = "data.frame"),
    class = "spicy_invalid_input"
  )
  # the LRT-based partial_chi2 is NOT rejected (it is defined for mixed fits)
  expect_error(
    table_regression(
      fit,
      show_fit_stats = c("r2_marginal"),
      output = "data.frame"
    ),
    NA
  )
})

## ---- 8. glmmTMB + nlme parity ---------------------------------------------

test_that("glmmTMB renders vc rows", {
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(
    Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  df <- table_regression(fit, output = "data.frame")
  v <- trimws(df$Variable)
  expect_true(any(grepl("Random effects", v, fixed = TRUE)))
  expect_true(any(grepl("Subject (Intercept)", v, fixed = TRUE)))
})

test_that("nlme::lme renders vc rows", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    distance ~ age + Sex,
    data = nlme::Orthodont,
    random = ~ age | Subject
  )
  df <- table_regression(fit, output = "data.frame")
  v <- trimws(df$Variable)
  expect_true(any(grepl("Random effects", v, fixed = TRUE)))
  expect_true(any(grepl("(Residual)", v, fixed = TRUE)))
})

## ---- 9. Audit fixes (2026-07-02) -------------------------------------------

test_that("structured body carries the vc rows (backend parity)", {
  skip_if_not_installed("merDeriv")
  fit <- .rr_lmer_slope()
  s <- as_structured(table_regression(fit, show_columns = c("b", "se", "ci")))
  b <- s$body
  re <- b[
    grepl("Subject (Intercept)", b$Variable, fixed = TRUE),
    ,
    drop = FALSE
  ]
  expect_equal(nrow(re), 1L)
  # estimate + SE populated as numerics (not NA): rich engines show values
  num_cols <- names(b)[vapply(b, is.numeric, logical(1))]
  vals <- unlist(re[, num_cols])
  expect_true(sum(is.finite(vals)) >= 3L) # est + SE + CI bounds
})

test_that("keep/drop never mutilates the Random effects block", {
  fit <- .rr_lmer_slope()
  df <- table_regression(fit, keep = "Days", output = "data.frame")
  v <- trimws(df$Variable)
  # the whole block survives a keep that matches only one predictor
  expect_true(any(grepl("Subject (Intercept)", v, fixed = TRUE)))
  expect_true(any(grepl("(Residual)", v, fixed = TRUE)))
  expect_false(any(v == "(Intercept)")) # the fixed intercept IS filtered
})

test_that("flat layout renders display labels, not internal re:: keys", {
  fit <- .rr_lmer_slope()
  df <- table_regression(fit, factor_layout = "flat", output = "data.frame")
  v <- df$Variable
  expect_false(any(grepl("re::", v, fixed = TRUE)))
  expect_true(any(grepl("Subject (Intercept)", v, fixed = TRUE)))
})

test_that("show_re is validated as a logical scalar", {
  fit <- .rr_lmer_slope()
  expect_error(
    table_regression(fit, show_re = "yes"),
    class = "spicy_invalid_input"
  )
})

test_that("beta-only display omits the RE block instead of rendering it empty", {
  fit <- .rr_lmer_slope()
  df <- suppressWarnings(table_regression(
    fit,
    standardized = "refit",
    show_columns = c("beta", "se"),
    output = "data.frame"
  ))
  expect_false(any(grepl("Random effects", df$Variable, fixed = TRUE)))
})

test_that("correlation rows align across engines (lme4 vs glmmTMB)", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("broom")
  fit_l <- .rr_lmer_slope()
  fit_t <- glmmTMB::glmmTMB(
    Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy
  )
  td_l <- broom::tidy(table_regression(fit_l))
  td_t <- broom::tidy(table_regression(fit_t))
  key_l <- td_l$term[td_l$estimate_type == "vc" & grepl("::cor", td_l$term)]
  key_t <- td_t$term[td_t$estimate_type == "vc" & grepl("::cor", td_t$term)]
  expect_identical(key_l, key_t) # same canonical key -> same table row
  # and the glmmTMB rho still carries SE after the key normalization
  expect_true(is.finite(td_t$std.error[td_t$term == key_t]))
})

test_that("multi-model RE block puts the Residual row last", {
  fit_int <- lme4::lmer(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy
  )
  fit_sl <- .rr_lmer_slope()
  df <- table_regression(list(fit_int, fit_sl), output = "data.frame")
  v <- trimws(df$Variable)
  i_res <- which(grepl("(Residual)", v, fixed = TRUE))
  i_cor <- which(grepl(", Days)", v, fixed = TRUE))
  expect_true(i_res > i_cor)
})

test_that("variance scale relabels the rows to sigma-squared", {
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  df <- table_regression(fit, re_scale = "variance", output = "data.frame")
  expect_true(any(grepl("σ²", df$Variable)))
})

test_that("N (groups) and ICC render as fit-stat rows (mixed defaults)", {
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  df <- table_regression(fit, output = "data.frame")
  v <- trimws(df$Variable)
  expect_true(any(v == "N (Subject)"))
  expect_true(any(v == "ICC"))
  n_cell <- trimws(df[[2]][v == "N (Subject)"])
  expect_match(n_cell, "18", fixed = TRUE)
  # ICC row auto-drops when not computable (random slope)
  df2 <- table_regression(.rr_lmer_slope(), output = "data.frame")
  expect_false(any(trimws(df2$Variable) == "ICC"))
})

## ---- 10. Ledger sweep (spec section 17, 2026-07-02) ------------------------

test_that("cbind-response glmer gets the chi-bar-squared LR footer line", {
  skip_if_not_installed("lme4")
  m <- suppressMessages(lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = lme4::cbpp,
    family = binomial
  ))
  nl <- spicy:::.compute_null_model_lrt(m)
  expect_false(is.null(nl))
  expect_true(is.finite(nl$chi2) && nl$chi2 > 0)
  out <- paste(capture.output(print(table_regression(m))), collapse = "\n")
  expect_match(out, "LR test", fixed = TRUE)
})

test_that("labels accepts coefficient-level keys on mixed fits", {
  skip_if_not_installed("lme4")
  m <- suppressMessages(lme4::glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = lme4::cbpp,
    family = binomial
  ))
  df <- table_regression(
    m,
    labels = c(period2 = "Period 2"),
    output = "data.frame"
  )
  expect_true(any(grepl("Period 2", df$Variable, fixed = TRUE)))
  # a grouping factor is NOT a coefficient name: still rejected
  expect_error(
    table_regression(m, labels = c(bogus_key = "X"), output = "data.frame"),
    class = "spicy_invalid_input"
  )
})

test_that("n_groups: dynamic label + numeric cell when one shared factor", {
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  df <- table_regression(fit, output = "data.frame")
  v <- trimws(df$Variable)
  expect_true(any(v == "N (Subject)"))
  expect_identical(trimws(df[[2]][v == "N (Subject)"]), "18")
  # structured body carries the numeric count
  s <- as_structured(table_regression(fit))
  b <- s$body
  expect_equal(as.numeric(b[[2]][b$Variable == "N (Subject)"]), 18)
})

test_that("n_groups: crossed factors render one N (<factor>) row each", {
  skip_if_not_installed("lme4")
  set.seed(1)
  d <- data.frame(
    y = stats::rnorm(300),
    x = stats::rnorm(300),
    g1 = factor(sample(10, 300, TRUE)),
    g2 = factor(sample(15, 300, TRUE))
  )
  m <- suppressWarnings(suppressMessages(
    lme4::lmer(y ~ x + (1 | g1) + (1 | g2), data = d)
  ))
  df <- table_regression(m, output = "data.frame")
  v <- trimws(df$Variable)
  # One row per grouping factor (was one crammed "N (groups)" cell).
  expect_false(any(v == "N (groups)"))
  expect_identical(trimws(df[[2]][v == "N (g1)"]), "10")
  expect_identical(trimws(df[[2]][v == "N (g2)"]), "15")
})

test_that("n_groups: nested factor names pass verbatim, one row each", {
  skip_if_not_installed("lme4")
  fit <- suppressMessages(
    lme4::lmer(strength ~ 1 + (1 | batch / cask), data = lme4::Pastes)
  )
  df <- table_regression(fit, output = "data.frame")
  v <- trimws(df$Variable)
  # Verbatim names in the per-factor labels (a naive "+s" used to
  # mangle batch -> "batchs" in the old crammed cell).
  expect_identical(trimws(df[[2]][v == "N (cask:batch)"]), "30")
  expect_identical(trimws(df[[2]][v == "N (batch)"]), "10")
  expect_false(any(grepl("batchs", v, fixed = TRUE)))
})

## ---- 11. Variance-component SE size cap (spicy.re_se_max_n) ---------------

test_that("variance-component SEs are skipped above the size cap", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("merDeriv")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)

  old <- options(spicy.re_se_max_n = 50L) # sleepstudy has n = 180
  on.exit(options(old), add = TRUE)

  expect_warning(tr <- table_regression(fit), class = "spicy_caveat")
  out <- paste(capture.output(print(tr)), collapse = "\n")
  # Fact-only table note; the advice lives in the warning.
  expect_match(out, "SE and CI not computed", fixed = TRUE)
  expect_match(out, "spicy.re_se_max_n", fixed = TRUE)

  # tidy: vc rows keep estimates but carry no SE.
  tt <- suppressWarnings(broom::tidy(table_regression(fit)))
  vc <- tt[tt$estimate_type == "vc", ]
  expect_gt(nrow(vc), 0L)
  expect_true(all(is.finite(vc$estimate)))
  expect_true(all(is.na(vc$std.error)))

  # The warning names the override and the re_test alternative.
  w <- tryCatch(table_regression(fit), warning = function(w) w)
  expect_match(conditionMessage(w), "spicy.re_se_max_n", fixed = TRUE)
  expect_match(conditionMessage(w), "re_test", fixed = TRUE)
})

test_that("raising the size cap restores the variance-component SEs", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("merDeriv")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)

  old <- options(spicy.re_se_max_n = Inf)
  on.exit(options(old), add = TRUE)

  expect_no_warning(tr <- table_regression(fit))
  tt <- broom::tidy(tr)
  vc <- tt[tt$estimate_type == "vc", ]
  expect_true(any(is.finite(vc$std.error)))
  out <- paste(capture.output(print(tr)), collapse = "\n")
  expect_false(grepl("SE and CI not computed", out, fixed = TRUE))
})

## ---- 12. Profile-likelihood CIs for variance components (re_ci) -----------

test_that("re_ci = 'profile' matches the confint profile oracle exactly", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), data = lme4::sleepstudy)
  orc <- suppressWarnings(suppressMessages(
    confint(
      fit,
      parm = "theta_",
      method = "profile",
      oldNames = FALSE,
      quiet = TRUE
    )
  ))

  tr <- table_regression(fit, re_ci = "profile")
  tt <- broom::tidy(tr)
  vc <- tt[tt$estimate_type == "vc", ]

  # No SE, by construction (the interval is the uncertainty summary).
  expect_true(all(is.na(vc$std.error)))

  ci_of <- function(term) {
    r <- vc[vc$term == term, ]
    c(r$conf.low, r$conf.high)
  }
  expect_equal(
    ci_of("re::Subject::(Intercept)"),
    unname(orc["sd_(Intercept)|Subject", ]),
    tolerance = 1e-6
  )
  expect_equal(
    ci_of("re::Subject::Days"),
    unname(orc["sd_Days|Subject", ]),
    tolerance = 1e-6
  )
  expect_equal(
    ci_of("re::Subject::(Intercept), Days::cor"),
    unname(orc["cor_Days.(Intercept)|Subject", ]),
    tolerance = 1e-6
  )
  expect_equal(
    ci_of("re::Residual::"),
    unname(orc["sigma", ]),
    tolerance = 1e-6
  )

  # Footer discloses the method (a profile CI has no reconstructing SE).
  out <- paste(capture.output(print(tr)), collapse = "\n")
  expect_match(out, "profile likelihood CIs", fixed = TRUE)

  # Likelihood intervals transform exactly: the variance-scale display
  # shows the squared SD bounds (the profile CI for sigma^2 itself).
  ttv <- broom::tidy(table_regression(
    fit,
    re_ci = "profile",
    re_scale = "variance"
  ))
  vcv <- ttv[ttv$estimate_type == "vc", ]
  r <- vcv[vcv$term == "re::Subject::(Intercept)", ]
  expect_equal(
    c(r$conf.low, r$conf.high),
    unname(orc["sd_(Intercept)|Subject", ])^2,
    tolerance = 1e-6
  )
})

test_that("re_ci = 'profile' sidesteps the size cap without note or warning", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  old <- options(spicy.re_se_max_n = 50L) # sleepstudy has n = 180
  on.exit(options(old), add = TRUE)

  expect_no_warning(tr <- table_regression(fit, re_ci = "profile"))
  out <- paste(capture.output(print(tr)), collapse = "\n")
  expect_false(grepl("SE and CI not computed", out, fixed = TRUE))
  expect_match(out, "profile likelihood CIs", fixed = TRUE)
  tt <- broom::tidy(tr)
  vc <- tt[tt$estimate_type == "vc", ]
  expect_true(all(is.finite(vc$conf.low)))
})

test_that("re_ci validates its value and its engine support", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  expect_error(
    table_regression(fit, re_ci = "banana"),
    class = "spicy_invalid_input"
  )
  expect_error(
    table_regression(fit, re_ci = "profile", show_re = FALSE),
    class = "spicy_invalid_input"
  )
  skip_if_not_installed("glmmTMB")
  gt <- suppressWarnings(glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy
  ))
  err <- tryCatch(table_regression(gt, re_ci = "profile"), error = function(e) {
    e
  })
  expect_s3_class(err, "spicy_invalid_input")
  expect_match(conditionMessage(err), "lme4", fixed = TRUE)
})

## ---- 13. ci_level flows into the variance-component CIs -------------------

test_that("ci_level is honored by the vc CI paths of all mixed engines", {
  skip_if_not_installed("lme4")

  vc_ci <- function(tr, term) {
    tt <- broom::tidy(tr)
    r <- tt[tt$estimate_type == "vc" & tt$term == term, ]
    c(r$conf.low, r$conf.high)
  }
  trm <- "re::Subject::(Intercept)"
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)

  # merDeriv Wald path: on the variance scale the interval is symmetric,
  # so the 90/95 half-width ratio must be exactly z_.95 / z_.975.
  skip_if_not_installed("merDeriv")
  v95 <- vc_ci(table_regression(fit), trm)^2
  v90 <- vc_ci(table_regression(fit, ci_level = 0.90), trm)^2
  expect_equal(
    (v90[2] - v90[1]) / (v95[2] - v95[1]),
    qnorm(0.95) / qnorm(0.975),
    tolerance = 1e-8
  )

  # Profile path: matches the confint oracle at the requested level.
  orc90 <- suppressWarnings(suppressMessages(
    confint(
      fit,
      parm = "theta_",
      method = "profile",
      oldNames = FALSE,
      level = 0.90,
      quiet = TRUE
    )
  ))
  p90 <- vc_ci(table_regression(fit, re_ci = "profile", ci_level = 0.90), trm)
  expect_equal(p90, unname(orc90["sd_(Intercept)|Subject", ]), tolerance = 1e-6)
})

test_that("ci_level reaches the nlme and glmmTMB vc CI paths", {
  skip_if_not_installed("lme4")
  vc_ci <- function(tr, term) {
    tt <- broom::tidy(tr)
    r <- tt[tt$estimate_type == "vc" & tt$term == term, ]
    c(r$conf.low, r$conf.high)
  }
  trm <- "re::Subject::(Intercept)"

  skip_if_not_installed("nlme")
  lf <- nlme::lme(
    Reaction ~ Days,
    random = ~ 1 | Subject,
    data = lme4::sleepstudy
  )
  o_lme <- nlme::intervals(lf, level = 0.90, which = "var-cov")
  expect_equal(
    vc_ci(table_regression(lf, ci_level = 0.90), trm),
    as.numeric(o_lme$reStruct$Subject[1, c("lower", "upper")]),
    tolerance = 1e-6
  )

  skip_if_not_installed("glmmTMB")
  gt <- suppressWarnings(glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy,
    REML = TRUE
  ))
  o_tmb <- confint(gt, method = "Wald", parm = "theta_", level = 0.90)
  expect_equal(
    vc_ci(table_regression(gt, ci_level = 0.90), trm),
    unname(as.numeric(o_tmb[1, 1:2])),
    tolerance = 1e-6
  )
})

test_that("re_ci default (wald) is unchanged and differs from profile", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("merDeriv")
  fit <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  tw <- broom::tidy(table_regression(fit))
  tp <- broom::tidy(table_regression(fit, re_ci = "profile"))
  vw <- tw[
    tw$estimate_type == "vc" &
      tw$term == "re::Subject::(Intercept)",
  ]
  vp <- tp[
    tp$estimate_type == "vc" &
      tp$term == "re::Subject::(Intercept)",
  ]
  expect_true(is.finite(vw$std.error)) # Wald SE present
  expect_true(is.na(vp$std.error)) # profile: no SE
  expect_false(isTRUE(all.equal(vw$conf.low, vp$conf.low)))
})


test_that("3-term random block SEs align slot-by-slot (merDeriv >= 0.2-6)", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("merDeriv")
  skip_if_not_installed("glmmTMB")
  # merDeriv < 0.2-6 returned the RE (co)variance results in an order
  # that did not match their labels for 3x3+ blocks (merDeriv GH #2:
  # index matrix built from Lambda instead of Lambdat), so positional
  # extraction silently mislabelled the variance SEs -- and 2x2
  # fixtures cannot catch it (both orders coincide). Pin the slot
  # alignment with an INDEPENDENT engine: glmmTMB's sdreport SEs for
  # the same ML fit must match term by term. The parametrizations
  # differ, so a few percent of drift is expected; a slot permutation
  # shows up as 50%+ discrepancies.
  set.seed(42)
  ng <- 40
  per <- 12
  n <- ng * per
  g <- factor(rep(seq_len(ng), each = per))
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  b0 <- rnorm(ng, 0, 1.2)
  b1 <- rnorm(ng, 0, 0.8)
  b2 <- rnorm(ng, 0, 0.5)
  y <- 2 +
    0.5 * x1 -
    0.3 * x2 +
    b0[g] +
    b1[g] * x1 +
    b2[g] * x2 +
    rnorm(n, 0, 1)
  d <- data.frame(y, x1, x2, g)
  m_lmer <- lme4::lmer(y ~ x1 + x2 + (x1 + x2 | g), data = d, REML = FALSE)
  m_tmb <- glmmTMB::glmmTMB(y ~ x1 + x2 + (x1 + x2 | g), data = d, REML = FALSE)
  vc_l <- as_regression_frame(m_lmer)$info$random_effects$variance_components
  vc_t <- as_regression_frame(m_tmb)$info$random_effects$variance_components
  keep <- function(vc) {
    vc[
      !(vc$is_correlation %in% TRUE) & vc$group != "Residual",
      c("group", "term", "variance", "std_error")
    ]
  }
  cmp <- merge(
    keep(vc_l),
    keep(vc_t),
    by = c("group", "term"),
    suffixes = c("_lmer", "_tmb")
  )
  expect_identical(nrow(cmp), 3L)
  expect_true(all(is.finite(cmp$std_error_lmer)))
  # Same ML optimum: variances agree tightly.
  expect_equal(cmp$variance_lmer, cmp$variance_tmb, tolerance = 1e-3)
  # SEs agree per SLOT (expected vs observed information: small drift
  # only; a permuted slot would blow far past this tolerance).
  expect_equal(cmp$std_error_lmer, cmp$std_error_tmb, tolerance = 0.05)
})
