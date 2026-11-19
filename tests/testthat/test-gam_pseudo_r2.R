# The R-squared rows of an mgcv GAM are those of its glm twin, the glm of
# the same family and link, computed by the same rules and with the same
# defaults. dev/decisions/2026-11-19-gam-follows-glm.md has the
# measures.

gr_rows <- function(tbl) {
  tbl[tbl$.row_role == "fit_stat", , drop = FALSE]
}

gr_cell <- function(tbl, token, col = "B") {
  r <- gr_rows(tbl)
  r[[col]][r$variable == token]
}

gr_values <- function(fit, tokens) {
  tbl <- suppressWarnings(table_regression(
    fit,
    show_fit_stats = c("nobs", tokens),
    output = "data.frame"
  ))
  vapply(tokens, function(tk) gr_cell(tbl, tk), numeric(1))
}

gr_data <- function(n = 300, seed = 5) {
  set.seed(seed)
  d <- data.frame(
    x = rnorm(n),
    z = runif(n),
    off = runif(n, -0.3, 0.3),
    w = runif(n, 0.4, 2.2)
  )
  d$yb <- rbinom(n, 1, plogis(-0.3 + 0.8 * d$x + sin(3 * d$z) + d$off))
  d$tr <- sample(3:9, n, TRUE)
  d$k <- rbinom(n, d$tr, plogis(0.2 + 0.5 * d$x + d$off))
  d$yp <- rpois(n, exp(0.4 + 0.4 * d$x + d$off))
  d$ynb <- rnbinom(n, mu = exp(0.5 + 0.5 * d$x + d$off), size = 1.5)
  d$yg <- rgamma(n, 3, rate = 3 / exp(0.3 + 0.3 * d$x + d$off))
  d$yn <- 1 + d$x + sin(3 * d$z) + d$off + rnorm(n)
  d
}

# The tokens each family's glm twin shows.
gr_tokens <- list(
  binomial = c(
    "pseudo_r2_tjur",
    "pseudo_r2_mcfadden",
    "pseudo_r2_nagelkerke"
  ),
  count = c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke", "r2_deviance"),
  continuous = "r2_deviance",
  quasi = "r2_deviance"
)


# ---- GAMs without smooth terms: the numbers of the glm twin ---------------

test_that("a GAM without smooth terms gives the R2 of its glm twin", {
  skip_if_not_installed("mgcv")
  d <- gr_data()
  # The response, the family and the tokens of each twin. Both fits run to
  # a tight convergence: under the default tolerances the fitted values of
  # a non-canonical link (probit, cloglog) differ by up to 6e-7 between
  # glm() and gam(), and Tjur's R2 with them.
  cases <- list(
    logit = list("yb", binomial(), gr_tokens$binomial),
    probit = list("yb", binomial("probit"), gr_tokens$binomial),
    cloglog = list("yb", binomial("cloglog"), gr_tokens$binomial),
    grouped = list("cbind(k, tr - k)", binomial(), gr_tokens$binomial),
    poisson = list("yp", poisson(), gr_tokens$count),
    gaussian = list("yn", gaussian(), gr_tokens$continuous),
    gaussian_log = list("yg", gaussian("log"), gr_tokens$continuous),
    gamma = list("yg", Gamma("log"), gr_tokens$continuous),
    quasipoisson = list("yp", quasipoisson(), gr_tokens$quasi),
    quasibinomial = list("cbind(k, tr - k)", quasibinomial(), gr_tokens$quasi)
  )
  for (nm in names(cases)) {
    for (off in c(FALSE, TRUE)) {
      for (wt in c(FALSE, TRUE)) {
        f <- stats::as.formula(paste(
          cases[[nm]][[1L]],
          "~ x",
          if (off) "+ offset(off)"
        ))
        args <- list(formula = f, family = cases[[nm]][[2L]], data = d)
        if (wt) {
          # Non-integer weights.
          args$weights <- d$w
        }
        glm_args <- c(
          args,
          list(control = glm.control(epsilon = 1e-15, maxit = 200))
        )
        gam_args <- c(
          args,
          list(control = mgcv::gam.control(epsilon = 1e-15, maxit = 200))
        )
        tw <- suppressWarnings(do.call(glm, glm_args))
        ga <- suppressWarnings(do.call(mgcv::gam, gam_args))
        toks <- cases[[nm]][[3L]]
        expect_equal(
          gr_values(ga, toks),
          gr_values(tw, toks),
          tolerance = 1e-8,
          info = sprintf("%s offset=%s weights=%s", nm, off, wt)
        )
      }
    }
  }
})

test_that("an offset given as an argument is kept in the null model", {
  skip_if_not_installed("mgcv")
  d <- gr_data()
  ga <- mgcv::gam(yb ~ x, family = binomial, data = d, offset = off)
  tw <- glm(yb ~ x, family = binomial, data = d, offset = off)
  expect_equal(
    gr_values(ga, gr_tokens$binomial),
    gr_values(tw, gr_tokens$binomial),
    tolerance = 1e-8
  )
})


# ---- GAMs with a smooth term: the definitions by hand ---------------------

test_that("Tjur, McFadden and Nagelkerke of a smooth logit GAM, by hand", {
  skip_if_not_installed("mgcv")
  d <- gr_data()
  ga <- mgcv::gam(yb ~ x + s(z) + offset(off), family = binomial, data = d)
  p <- fitted(ga)
  tjur <- mean(p[d$yb == 1]) - mean(p[d$yb == 0])
  ll <- sum(dbinom(d$yb, 1, p, log = TRUE))
  null <- glm(yb ~ 1 + offset(off), family = binomial, data = d)
  ll0 <- as.numeric(logLik(null))
  n <- nrow(d)
  cs <- 1 - exp(2 * (ll0 - ll) / n)
  expect_equal(
    gr_values(ga, gr_tokens$binomial),
    c(
      pseudo_r2_tjur = tjur,
      pseudo_r2_mcfadden = 1 - ll / ll0,
      pseudo_r2_nagelkerke = cs / (1 - exp(2 * ll0 / n))
    ),
    tolerance = 1e-10
  )
  # The likelihood is that of the GAM, smooth terms included.
  expect_equal(ll, as.numeric(logLik(ga)), tolerance = 1e-10)
})

test_that("a weighted smooth logit GAM weights the trials", {
  skip_if_not_installed("mgcv")
  d <- gr_data()
  ga <- suppressWarnings(
    mgcv::gam(yb ~ x + s(z), family = binomial, data = d, weights = w)
  )
  p <- fitted(ga)
  w <- d$w
  y <- d$yb
  tjur <- sum(w * y * p) / sum(w * y) - sum(w * (1 - y) * p) / sum(w * (1 - y))
  ll <- sum(w * (y * log(p) + (1 - y) * log1p(-p)))
  p0 <- sum(w * y) / sum(w)
  ll0 <- sum(w * (y * log(p0) + (1 - y) * log1p(-p0)))
  expect_equal(
    gr_values(ga, c("pseudo_r2_tjur", "pseudo_r2_mcfadden")),
    c(pseudo_r2_tjur = tjur, pseudo_r2_mcfadden = 1 - ll / ll0),
    tolerance = 1e-10
  )
})

test_that("McFadden and Nagelkerke of a smooth Poisson GAM, by hand", {
  skip_if_not_installed("mgcv")
  d <- gr_data()
  ga <- mgcv::gam(yp ~ s(x) + offset(off), family = poisson, data = d)
  ll <- sum(dpois(d$yp, fitted(ga), log = TRUE))
  ll0 <- as.numeric(logLik(glm(yp ~ 1 + offset(off), poisson, d)))
  n <- nrow(d)
  expect_equal(
    gr_values(ga, c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke")),
    c(
      pseudo_r2_mcfadden = 1 - ll / ll0,
      pseudo_r2_nagelkerke = (1 - exp(2 * (ll0 - ll) / n)) /
        (1 - exp(2 * ll0 / n))
    ),
    tolerance = 1e-10
  )
})


# ---- nb() -----------------------------------------------------------------

test_that("an nb() GAM fitted by ML gives the R2 of its glm.nb twin", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("MASS")
  d <- gr_data()
  for (off in c(FALSE, TRUE)) {
    for (wt in c(FALSE, TRUE)) {
      f <- stats::as.formula(paste("ynb ~ x", if (off) "+ offset(off)"))
      ga <- if (wt) {
        mgcv::gam(f, family = mgcv::nb(), data = d, weights = w, method = "ML")
      } else {
        mgcv::gam(f, family = mgcv::nb(), data = d, method = "ML")
      }
      tw <- if (wt) {
        MASS::glm.nb(f, data = d, weights = w)
      } else {
        MASS::glm.nb(f, data = d)
      }
      expect_equal(
        gr_values(ga, c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke")),
        gr_values(tw, c("pseudo_r2_mcfadden", "pseudo_r2_nagelkerke")),
        tolerance = 1e-8,
        info = sprintf("offset=%s weights=%s", off, wt)
      )
    }
  }
})

test_that("the null model of an nb() GAM re-estimates theta", {
  skip_if_not_installed("mgcv")
  skip_if_not_installed("MASS")
  d <- gr_data()
  # mgcv's default method for nb() is REML, whose theta is not that of
  # glm.nb(): the full model is the GAM's, the null model the glm.nb()
  # rule's, theta re-estimated by ML with the offset and the weights.
  ga <- mgcv::gam(ynb ~ s(x) + offset(off), family = mgcv::nb(), data = d)
  null <- MASS::glm.nb(ynb ~ 1 + offset(off), data = d)
  ll <- as.numeric(logLik(ga))
  ll0 <- as.numeric(logLik(null))
  expect_equal(
    gr_values(ga, "pseudo_r2_mcfadden"),
    c(pseudo_r2_mcfadden = 1 - ll / ll0),
    tolerance = 1e-10
  )
  # Not the null model at the GAM's theta, which is no ML fit.
  fixed <- glm(
    ynb ~ 1 + offset(off),
    family = MASS::negative.binomial(ga$family$getTheta(TRUE)),
    data = d
  )
  expect_false(isTRUE(all.equal(
    unname(gr_values(ga, "pseudo_r2_mcfadden")),
    1 - ll / as.numeric(logLik(fixed)),
    tolerance = 1e-8
  )))
  # nb(theta) fixes theta, as glm()'s negative.binomial(theta): its null
  # model keeps it.
  gf <- mgcv::gam(ynb ~ x, family = mgcv::nb(theta = 1.5), data = d)
  tf <- glm(ynb ~ x, family = MASS::negative.binomial(1.5), data = d)
  expect_equal(
    gr_values(gf, gr_tokens$count),
    gr_values(tf, gr_tokens$count),
    tolerance = 1e-8
  )
})


# ---- bam ------------------------------------------------------------------

test_that("bam() fits give the R2 of their glm twin", {
  skip_if_not_installed("mgcv")
  d <- gr_data()
  cases <- list(
    list(yb ~ x + offset(off), binomial(), gr_tokens$binomial),
    list(yp ~ x + offset(off), poisson(), gr_tokens$count),
    # bam()'s own deviance explained leaves the offset out of its null
    # model: the row is the twin's, which keeps it.
    list(yg ~ x + offset(off), Gamma("log"), gr_tokens$continuous)
  )
  for (cs in cases) {
    ba <- mgcv::bam(cs[[1L]], family = cs[[2L]], data = d, weights = w)
    tw <- suppressWarnings(
      glm(cs[[1L]], family = cs[[2L]], data = d, weights = w)
    )
    expect_equal(
      gr_values(ba, cs[[3L]]),
      gr_values(tw, cs[[3L]]),
      tolerance = 1e-8,
      info = cs[[2L]]$family
    )
  }
})


# ---- Defaults ---------------------------------------------------------------

test_that("a GAM shows the default R2 rows of its glm twin", {
  skip_if_not_installed("mgcv")
  # tw() looks up mgcv's ldTweedie() on the search path.
  withr::local_package("mgcv")
  d <- gr_data()
  default_rows <- function(fit) {
    gr_rows(suppressWarnings(table_regression(
      fit,
      output = "data.frame"
    )))$variable
  }
  expect_identical(
    default_rows(mgcv::gam(yb ~ x + s(z), family = binomial, data = d)),
    c("nobs", "pseudo_r2_tjur", "pseudo_r2_mcfadden", "aic")
  )
  for (fit in list(
    mgcv::gam(yb ~ x, family = binomial("probit"), data = d),
    mgcv::gam(cbind(k, tr - k) ~ x, family = binomial, data = d),
    mgcv::gam(yp ~ s(x), family = poisson, data = d),
    mgcv::gam(ynb ~ s(x), family = mgcv::nb(), data = d)
  )) {
    expect_identical(
      default_rows(fit),
      c("nobs", "pseudo_r2_mcfadden", "aic")
    )
  }
  for (fit in list(
    mgcv::gam(yn ~ s(z), data = d),
    mgcv::gam(yg ~ s(x), family = Gamma("log"), data = d),
    mgcv::gam(yg ~ s(x), family = mgcv::tw(), data = d)
  )) {
    expect_identical(default_rows(fit), c("nobs", "r2_deviance", "aic"))
  }
  # A quasi family has no likelihood, and no R2 by default.
  expect_identical(
    default_rows(mgcv::gam(yp ~ s(x), family = quasipoisson, data = d)),
    "nobs"
  )
  # mgcv's other extended families have no twin and no R2 row.
  expect_identical(
    default_rows(mgcv::gam(
      I((yb + 0.5) / 2) ~ x,
      family = mgcv::betar(),
      data = d
    )),
    c("nobs", "aic")
  )
})

test_that("a GAM and its glm twin share their rows in one table", {
  skip_if_not_installed("mgcv")
  d <- gr_data()
  ga <- mgcv::gam(yb ~ x + s(z), family = binomial, data = d)
  tw <- glm(yb ~ x, family = binomial, data = d)
  tbl <- table_regression(list(GLM = tw, GAM = ga), output = "data.frame")
  expect_identical(
    gr_rows(tbl)$variable,
    c("nobs", "pseudo_r2_tjur", "pseudo_r2_mcfadden", "aic")
  )
  expect_true(is.finite(gr_cell(tbl, "pseudo_r2_tjur", "GAM: B")))
  # Beside an lm, the binomial GAM carries Tjur's row.
  tbl <- table_regression(
    list(LM = lm(yn ~ x, d), GAM = ga),
    show_fit_stats = c("nobs", "pseudo_r2_tjur"),
    output = "data.frame"
  )
  expect_true(is.na(gr_cell(tbl, "pseudo_r2_tjur", "LM: B")))
  expect_true(is.finite(gr_cell(tbl, "pseudo_r2_tjur", "GAM: B")))
})


# ---- Refusals follow the twin -----------------------------------------------

test_that("the refusals of a GAM are those of its glm twin", {
  skip_if_not_installed("mgcv")
  d <- gr_data()
  gb <- mgcv::gam(yb ~ x + s(z), family = binomial, data = d)
  gp <- mgcv::gam(yp ~ s(x), family = poisson, data = d)
  gn <- mgcv::gam(yn ~ s(z), data = d)
  # A binomial GAM: the deviance R2 is McFadden's, and the message says so.
  err <- expect_error(
    table_regression(gb, show_fit_stats = c("nobs", "r2_deviance")),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "\"pseudo_r2_mcfadden\"", fixed = TRUE)
  # A Poisson GAM: no Tjur.
  err <- expect_error(
    table_regression(gp, show_fit_stats = c("nobs", "pseudo_r2_tjur")),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "\"pseudo_r2_mcfadden\"", fixed = TRUE)
  expect_no_match(
    sub("^[^\n]*\n", "", conditionMessage(err)),
    "\"pseudo_r2_tjur\"",
    fixed = TRUE
  )
  # A Gaussian GAM: none of the three pseudo-R2, sent to the deviance R2.
  for (tk in gr_tokens$binomial) {
    err <- expect_error(
      table_regression(gn, show_fit_stats = c("nobs", tk)),
      class = "spicy_invalid_input"
    )
    hint <- sub("^[^\n]*\n", "", conditionMessage(err))
    expect_match(hint, "\"r2_deviance\"", fixed = TRUE)
    for (refused in gr_tokens$binomial) {
      expect_no_match(hint, sprintf("\"%s\"", refused), fixed = TRUE)
    }
  }
  # Beside a gaussian glm, the continuous refusal names both.
  err <- expect_error(
    table_regression(
      list(glm(yn ~ x, gaussian, d), gn),
      show_fit_stats = c("nobs", "pseudo_r2_mcfadden")
    ),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "`glm` or GAM", fixed = TRUE)
})
