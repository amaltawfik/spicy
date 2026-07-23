# ---------------------------------------------------------------------------
# Coverage: mixed-effects frame branches not hit by the functional tests.
# Targets the merMod profile-CI fallback paths (singular fit, profile
# failure, unmatched / non-finite profile parameters, groups without a
# correlation matrix) and the glmmTMB reference-row, title-prefix, and
# variance-component guard branches.
# ---------------------------------------------------------------------------

# Small, well-separated random-intercept data: profiling is fast (~2 s)
# and the fit is comfortably non-singular.
.cov100_lmer_fit <- function() {
  skip_if_not_installed("lme4")
  set.seed(42)
  d <- data.frame(g = factor(rep(1:10, each = 6)), x = rep(seq(0, 5), 10))
  d$y <- 2 + 0.8 * d$x + rep(rnorm(10, sd = 1.5), each = 6) + rnorm(60, sd = 1)
  lme4::lmer(y ~ x + (1 | g), data = d)
}


# ---- 1. merMod: singular fit short-circuits the profile-CI path ----------

test_that("re_ci = 'profile' on a singular fit yields NA SE/CI vc rows", {
  skip_if_not_installed("lme4")
  set.seed(11)
  d <- data.frame(g = factor(rep(1:8, each = 5)), x = rnorm(40))
  d$y <- 1 + 2 * d$x + rnorm(40) # no group effect -> singular
  fit <- suppressWarnings(lme4::lmer(y ~ x + (1 | g), data = d))
  expect_true(lme4::isSingular(fit))

  expect_warning(
    tr <- table_regression(fit, re_ci = "profile"),
    regexp = "Singular fit",
    class = "spicy_caveat"
  )
  tt <- broom::tidy(tr)
  vc <- tt[tt$estimate_type == "vc", ]
  expect_true(nrow(vc) >= 2L)
  # The boundary variance is exactly zero; no interval is produced.
  expect_equal(vc$estimate[vc$term == "re::g::(Intercept)"], 0)
  expect_true(all(is.na(vc$std.error)))
  expect_true(all(is.na(vc$conf.low)))
  expect_true(all(is.na(vc$conf.high)))
})


# ---- 2. merMod: profile rows that cannot be matched are skipped ----------

test_that("unmatched profile parameters keep NA bounds, matched rows fill", {
  fit <- .cov100_lmer_fit()
  expect_false(lme4::isSingular(fit))
  orc <- suppressWarnings(suppressMessages(
    confint(
      fit,
      parm = "theta_",
      method = "profile",
      oldNames = FALSE,
      quiet = TRUE
    )
  ))

  # A frame with (a) a variance term unknown to the fit and (b) a
  # correlation row whose pair has no >= 2x2 correlation matrix in
  # VarCorr (random-intercept-only group): both must stay NA while the
  # genuine rows carry the squared profile bounds.
  vc_df <- data.frame(
    group = c("g", "g", "g", "Residual"),
    term = c("(Intercept)", "phantom", "(Intercept), x", ""),
    variance = c(1, 1, NA, 1),
    sd = c(1, 1, NA, 1),
    corr = c(NA, NA, 0.5, NA),
    is_correlation = c(FALSE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  res <- .merMod_attach_profile_ci(vc_df, fit)

  expect_identical(res$ci_method, c("profile", NA, NA, "profile"))
  expect_true(all(is.na(res$std_error))) # profile path: never an SE
  # Variance-scale contract: squared SD-scale profile bounds, exactly.
  expect_equal(
    res$ci_lower[1],
    unname(orc["sd_(Intercept)|g", 1])^2,
    tolerance = 1e-8
  )
  expect_equal(
    res$ci_upper[1],
    unname(orc["sd_(Intercept)|g", 2])^2,
    tolerance = 1e-8
  )
  expect_equal(res$ci_lower[4], unname(orc["sigma", 1])^2, tolerance = 1e-8)
  expect_equal(res$ci_upper[4], unname(orc["sigma", 2])^2, tolerance = 1e-8)
  expect_true(all(is.na(res$ci_lower[2:3])))
  expect_true(all(is.na(res$ci_upper[2:3])))
})


# ---- 3. merMod: non-finite profile bounds are skipped ---------------------

test_that("non-finite profile bounds leave the vc CI cells NA", {
  fit <- .cov100_lmer_fit()
  vc_df <- data.frame(
    group = c("g", "Residual"),
    term = c("(Intercept)", ""),
    variance = c(1, 1),
    sd = c(1, 1),
    corr = NA_real_,
    is_correlation = FALSE,
    stringsAsFactors = FALSE
  )
  # A CI level far beyond the profiled range: confint() succeeds but
  # returns NA bounds, which must be skipped silently (no spicy warning;
  # the cells render as en-dashes).
  expect_no_warning(
    res <- .merMod_attach_profile_ci(vc_df, fit, ci_level = 0.9999999)
  )
  expect_identical(res$ci_method, rep(NA_character_, 2L))
  expect_true(all(is.na(res$ci_lower)))
  expect_true(all(is.na(res$ci_upper)))
  expect_true(all(is.na(res$std_error)))
})


# ---- 4. merMod: profile failure warns (spicy_caveat) and returns NAs ------

test_that("a profile-CI failure warns with class spicy_caveat and NA block", {
  fit <- .cov100_lmer_fit()
  # Deterministic profiling failure on a non-singular object: an
  # inconsistent cnms slot makes lme4's profiling input checks error for
  # both confint() argument spellings, while isSingular() still works.
  bad <- fit
  bad@cnms <- list()
  expect_false(isTRUE(lme4::isSingular(bad)))

  vc_df <- data.frame(
    group = c("g", "Residual"),
    term = c("(Intercept)", ""),
    variance = c(1, 1),
    sd = c(1, 1),
    corr = NA_real_,
    is_correlation = FALSE,
    stringsAsFactors = FALSE
  )
  expect_warning(
    res <- .merMod_attach_profile_ci(vc_df, bad),
    regexp = "Profile-likelihood CIs",
    class = "spicy_caveat"
  )
  expect_true(all(is.na(res$std_error)))
  expect_true(all(is.na(res$ci_lower)))
  expect_true(all(is.na(res$ci_upper)))
  expect_identical(res$ci_method, rep(NA_character_, 2L))
})


# ---- 5. glmmTMB: no-intercept factor -> no reference row ------------------

test_that("glmmTMB with 0 + factor keeps all levels and adds no ref row", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4") # sleepstudy data
  d <- lme4::sleepstudy
  d$grp <- factor(rep(c("a", "b", "c"), length.out = nrow(d)))
  fit <- glmmTMB::glmmTMB(Reaction ~ 0 + grp + Days + (1 | Subject), data = d)
  fr <- as_regression_frame(fit, model_id = "M1")

  # No level was dropped, so no synthetic reference row may appear.
  expect_false(any(fr$coefs$is_ref %in% TRUE))
  # All three levels are real coefficients matching the fixef oracle.
  fe <- glmmTMB::fixef(fit)$cond
  expect_true(all(paste0("grp", c("a", "b", "c")) %in% fr$coefs$term))
  est <- setNames(fr$coefs$estimate, fr$coefs$term)
  expect_equal(unname(est[names(fe)]), unname(fe), tolerance = 1e-10)
})


# ---- 6. glmmTMB: title prefixes by family and link -------------------------

test_that("glmmTMB title prefix covers binomial links and family arms", {
  tp <- function(family, link = "log", zi = FALSE) {
    .glmmTMB_title_prefix(list(family = family, link = link), zi)
  }
  # Binomial titles are link-aware.
  expect_identical(
    tp("binomial", "probit"),
    "Probit mixed-effects regression (glmmTMB)"
  )
  expect_identical(
    tp("binomial", "cloglog"),
    "Complementary log-log mixed-effects regression (glmmTMB)"
  )
  expect_identical(
    tp("binomial", "log"),
    "Log-binomial mixed-effects regression (glmmTMB)"
  )
  expect_identical(
    tp("binomial", "cauchit"), # fallback link label
    "Binomial mixed-effects regression (glmmTMB)"
  )
  # Family arms.
  expect_identical(tp("Gamma"), "Gamma mixed-effects regression (glmmTMB)")
  expect_identical(
    tp("inverse.gaussian"),
    "Inverse-Gaussian mixed-effects regression (glmmTMB)"
  )
  expect_identical(
    tp("nbinom1"),
    "Negative-binomial mixed-effects regression (glmmTMB)"
  )
  expect_identical(tp("tweedie"), "Tweedie mixed-effects regression (glmmTMB)")
  expect_identical(tp("beta_family"), "Beta mixed-effects regression (glmmTMB)")
  # Unknown family: sentence-cased verbatim.
  expect_identical(tp("compois"), "Compois mixed-effects regression (glmmTMB)")
  # Zero-inflation suffix.
  expect_identical(
    tp("poisson", zi = TRUE),
    "Poisson mixed-effects regression (glmmTMB) (zero-inflated)"
  )
})


# ---- 7. glmmTMB: vc guards when confint() has no theta rows ---------------

test_that("glmmTMB vc helpers pass through / NA out without theta rows", {
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(mpg ~ wt, data = mtcars) # no random effects
  # Precondition: no theta parameters -> confint has zero rows.
  ci <- tryCatch(
    confint(fit, method = "Wald", parm = "theta_"),
    error = function(e) NULL
  )
  expect_true(is.null(ci) || nrow(ci) == 0L)

  vc_df <- data.frame(
    group = "Residual",
    term = "",
    variance = 1.44,
    sd = 1.2,
    corr = NA_real_,
    stringsAsFactors = FALSE
  )

  # Correlation appender: returns the frame unchanged (plus the
  # is_correlation flag it guarantees).
  res_corr <- .glmmTMB_append_correlation_rows(vc_df, fit)
  expect_identical(nrow(res_corr), 1L)
  expect_identical(res_corr$term, vc_df$term)
  expect_identical(res_corr$variance, vc_df$variance)
  expect_false(any(res_corr$is_correlation))

  # Wald SE/CI attacher: falls back to the NA block, keeping the
  # original columns intact.
  res_wald <- .glmmTMB_attach_wald_se_ci(vc_df, fit)
  expect_identical(res_wald$variance, vc_df$variance)
  expect_identical(res_wald$sd, vc_df$sd)
  expect_identical(res_wald$std_error, NA_real_)
  expect_identical(res_wald$ci_lower, NA_real_)
  expect_identical(res_wald$ci_upper, NA_real_)
  expect_identical(res_wald$ci_method, NA_character_)
})
