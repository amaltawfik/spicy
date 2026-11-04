# Every English title prefix an engine can produce has an entry in the
# French title bridge (`.FR_TITLE_PREFIXES`, R/i18n_fr.R). A prefix
# without one is no error at run time -- the table keeps its whole
# English title under `spicy.language = "fr"`, coherent or nothing --
# but it is a gap in the language. The prefixes are enumerated by
# calling each engine's title helper with every family, link and
# distribution it can receive, with no model fitted; a title an engine
# sets as a fixed string is listed with the engine that sets it, and read
# from the sources as well.

.bridge_keys <- function() names(spicy:::.FR_TITLE_PREFIXES)

# The title prefixes the sources write as string literals: the value of a
# `title_prefix =` argument or of an `$title_prefix <-` assignment, or of
# a branch of it (`if` / `else`, `%||%`, `{`). A prefix built by a helper
# (a call) is enumerated by calling the helper below.
.literal_prefixes <- function(r_dir) {
  leaves <- function(e) {
    if (is.character(e) && length(e) == 1L) {
      return(e)
    }
    if (!is.call(e)) {
      return(character(0))
    }
    fn <- e[[1L]]
    if (identical(fn, as.name("if"))) {
      return(unlist(lapply(as.list(e)[-c(1L, 2L)], leaves)))
    }
    if (identical(fn, as.name("{")) && length(e) > 1L) {
      return(leaves(e[[length(e)]]))
    }
    if (identical(fn, as.name("%||%")) || identical(fn, as.name("("))) {
      return(unlist(lapply(as.list(e)[-1L], leaves)))
    }
    character(0)
  }
  found <- character(0)
  walk <- function(e) {
    if (!is.call(e)) {
      return(invisible(NULL))
    }
    fn <- e[[1L]]
    is_assign <- identical(fn, as.name("<-")) || identical(fn, as.name("="))
    if (
      is_assign && is.call(e[[2L]]) && identical(e[[2L]][[1L]], as.name("$"))
    ) {
      if (identical(e[[2L]][[3L]], as.name("title_prefix"))) {
        found <<- c(found, leaves(e[[3L]]))
      }
    }
    nms <- names(e)
    for (i in seq_along(e)) {
      if (!is.null(nms) && identical(nms[i], "title_prefix")) {
        found <<- c(found, leaves(e[[i]]))
      }
      if (is.call(e[[i]])) walk(e[[i]])
    }
    invisible(NULL)
  }
  for (f in list.files(r_dir, pattern = "[.]R$", full.names = TRUE)) {
    for (e in parse(f, keep.source = FALSE, encoding = "UTF-8")) {
      walk(e)
    }
  }
  unique(found)
}

.expect_bridged <- function(prefixes) {
  missing <- setdiff(unique(prefixes), .bridge_keys())
  expect_identical(missing, character(0))
}

# The families and links a glm-style engine can carry. The title reads
# the link only for the binomial and gaussian families; the others are
# listed with their usual links all the same.
.glm_links <- list(
  gaussian = c("identity", "log", "inverse"),
  binomial = c("logit", "probit", "cloglog", "log", "cauchit"),
  poisson = c("log", "identity", "sqrt"),
  Gamma = c("inverse", "identity", "log"),
  inverse.gaussian = c("1/mu^2", "inverse", "identity", "log"),
  quasibinomial = c("logit", "probit", "cloglog"),
  quasipoisson = c("log", "identity"),
  quasi = c("identity", "log"),
  # MASS::negative.binomial(theta), mgcv's nb() and glmer.nb() put the
  # theta in the family name.
  "Negative Binomial(1.5)" = c("log", "sqrt", "identity"),
  # A family no title knows.
  tweedie = "log"
)

.fams <- function(families = names(.glm_links)) {
  out <- list()
  for (f in families) {
    for (l in .glm_links[[f]]) {
      out[[length(out) + 1L]] <- list(family = f, link = l)
    }
  }
  out
}

.titles <- function(fams, fn, ...) {
  vapply(fams, function(f) fn(f, ...), character(1))
}

# The links of a cumulative-link model: polr calls the logit "logistic";
# clm also fits the flexible Aranda-Ordaz and log-gamma links.
.polr_links <- c("logistic", "probit", "cloglog", "loglog", "cauchit")
.clm_links <- c(
  "logit",
  "probit",
  "cloglog",
  "loglog",
  "cauchit",
  "Aranda-Ordaz",
  "log-gamma"
)


test_that("the bridge has no duplicated key", {
  keys <- .bridge_keys()
  expect_identical(keys[duplicated(keys)], character(0))
})

test_that("the fixed titles are bridged", {
  .expect_bridged(c(
    # The title of a table whose models do not share a prefix.
    "Regression",
    # lm, rms::ols; nlme::lme, lme4::lmer; nlme::gls.
    "Linear regression",
    "Linear mixed-effects regression",
    "Generalized least squares",
    # rms::lrm, rms::cph, survival::coxph, survey::svycoxph.
    "Logistic regression",
    "Cox proportional hazards regression",
    "Survey-weighted Cox proportional hazards regression",
    # MASS::glm.nb, MASS::rlm.
    "Negative-binomial regression",
    "Robust linear regression (M-estimator)",
    # AER::ivreg / ivreg::ivreg, AER::tobit.
    "IV regression (2SLS)",
    "Tobit regression",
    # stats::nls, betareg, nnet::multinom, mlogit, sampleSelection.
    "Non-linear least squares regression",
    "Beta regression",
    "Multinomial logistic regression",
    "Discrete-choice multinomial logit",
    "Heckman selection model"
  ))
})

test_that("the title literals of the sources are bridged", {
  # Read from R/, so that renaming a fixed title in an engine cannot pass
  # unseen: the list above is hand-kept. Only meaningful against the
  # sources (a known source file is probed, as test-i18n.R does).
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(
    file.exists(file.path(r_dir, "regression_frame_lm.R")),
    "package sources not available"
  )
  found <- .literal_prefixes(r_dir)
  # The walker reaches the sites it was written for: an argument, an
  # assignment and an `if` branch.
  expect_true(all(
    c(
      "Heckman selection model",
      "Tobit regression",
      "Cumulative complementary log-log mixed-effects regression"
    ) %in%
      found
  ))
  .expect_bridged(found)
})

test_that("the glm titles are bridged", {
  .expect_bridged(.titles(.fams(), function(f) {
    spicy:::spicy_glm_title_prefix(f$family, f$link)
  }))
})

test_that("the rms Glm titles are bridged", {
  .expect_bridged(.titles(.fams(), function(f) {
    spicy:::.rms_title_prefix("Glm", f)
  }))
})

test_that("the fixest titles are bridged", {
  # With fixed effects and without them (a fit without them takes the
  # plain title of its family).
  for (has_fe in c(TRUE, FALSE)) {
    .expect_bridged(c(
      spicy:::.fixest_title_prefix(
        list(family = "gaussian", link = "identity"),
        is_glm = FALSE,
        has_fe = has_fe
      ),
      # The second stage of an IV feols.
      spicy:::.fixest_title_prefix(
        list(family = "gaussian", link = "identity"),
        is_glm = FALSE,
        has_fe = has_fe,
        is_iv = TRUE
      ),
      # fenegbin and femlm(family = "negbin").
      spicy:::.fixest_title_prefix(
        list(family = "negbin", link = "log"),
        is_glm = TRUE,
        has_fe = has_fe
      ),
      .titles(
        .fams(),
        spicy:::.fixest_title_prefix,
        is_glm = TRUE,
        has_fe = has_fe
      )
    ))
  }
})

test_that("the estimatr titles are bridged", {
  se <- c("classical", "none", "HC0", "HC1", "HC2", "HC3", "stata", "CR2")
  # With absorbed fixed effects (`fixed_effects = ~ f`) and without them.
  for (has_fe in c(TRUE, FALSE)) {
    .expect_bridged(c(
      vapply(
        se,
        spicy:::.estimatr_title_prefix,
        character(1),
        is_iv = FALSE,
        has_fe = has_fe
      ),
      vapply(
        se,
        spicy:::.estimatr_title_prefix,
        character(1),
        is_iv = TRUE,
        has_fe = has_fe
      )
    ))
  }
})

test_that("the GEE titles are bridged", {
  # geeglm() refuses the quasi families ("variance invalid"), so their
  # titles cannot be reached and the bridge has none.
  gee <- setdiff(names(.glm_links), c("quasibinomial", "quasipoisson", "quasi"))
  .expect_bridged(.titles(.fams(gee), spicy:::.geeglm_title_prefix))
})

test_that("the survey glm titles are bridged", {
  .expect_bridged(.titles(.fams(), spicy:::.svyglm_title_prefix))
})

test_that("the GAM titles are bridged", {
  gam <- function(f) {
    spicy:::.gam_title_prefix(
      f,
      identical(f$family, "gaussian") && identical(f$link, "identity")
    )
  }
  # The extended families of mgcv, under the names they give themselves
  # (their estimated parameters in the name).
  extended <- list(
    list(family = "Negative Binomial(1.358)", link = "log"),
    list(family = "Tweedie(p=1.892)", link = "log"),
    list(family = "Tweedie(1.5)", link = "log"),
    list(family = "Beta regression(4.108)", link = "logit"),
    list(family = "Scaled t(7.075,0.327)", link = "identity"),
    list(family = "Zero inflated Poisson(0.172,0)", link = "identity"),
    list(family = "Ordered Categorical(-1,2.95)", link = "identity"),
    list(family = "Cox PH", link = "identity"),
    list(family = "cnorm(1.254)", link = "identity"),
    list(family = "clog(0.726)", link = "identity"),
    list(family = "cpois", link = "log")
  )
  # The location-scale and multi-formula families (gaulss, ziplss, ...)
  # are refused before any title (.gam_refuse_multi_formula()); a family
  # no title knows keeps its R name and the whole English title, so it is
  # not enumerated.
  glm <- .fams(setdiff(names(.glm_links), "tweedie"))
  .expect_bridged(c(.titles(glm, gam), .titles(extended, gam)))
  expect_identical(
    gam(list(family = "tweedie", link = "log")),
    "Generalized additive model (GAM, tweedie)"
  )
})

test_that("the lme4 titles are bridged", {
  # glmer() fits the glm families without the quasi ones (a gaussian
  # identity glmer is an lmer); glmer.nb() names its family with theta.
  glmer <- function(f) {
    fit <- structure(list(family = f), class = "glm")
    paste0(spicy:::.merMod_glm_family_title(fit), " mixed-effects regression")
  }
  fams <- Filter(
    function(f) {
      !grepl("^quasi", f$family) &&
        !(identical(f$family, "gaussian") && identical(f$link, "identity"))
    },
    .fams(setdiff(names(.glm_links), "tweedie"))
  )
  .expect_bridged(.titles(fams, glmer))
})

test_that("the glmmTMB titles are bridged", {
  # Every family of the label table, alone and with a zero-inflation
  # formula (a zero-truncated family or a Gamma with one is a hurdle
  # model), and the ordinal family on the links glmmTMB fits it with (no
  # zero-inflation: "not implemented for the ordinal family"); each with
  # random effects and without (the fixed-effects title of the family).
  fams <- c(
    .fams(c("gaussian", "binomial")),
    lapply(names(spicy:::.GLMMTMB_FAMILY_LABELS), function(f) {
      list(family = f, link = "log")
    })
  )
  ordinal <- lapply(c("logit", "probit", "cloglog"), function(l) {
    list(family = "ordinal", link = l)
  })
  titles <- function(mixed) {
    c(
      .titles(fams, spicy:::.glmmTMB_title_prefix, FALSE, mixed = mixed),
      .titles(fams, spicy:::.glmmTMB_title_prefix, TRUE, mixed = mixed),
      .titles(ordinal, spicy:::.glmmTMB_title_prefix, FALSE, mixed = mixed)
    )
  }
  # The two sets differ, so the second is enumerated, not assumed.
  expect_false(any(titles(FALSE) %in% titles(TRUE)))
  .expect_bridged(c(titles(TRUE), titles(FALSE)))
})

test_that("the cumulative-link titles are bridged", {
  .expect_bridged(c(
    # polr and clm, clm with `nominal =`, clmm.
    vapply(.clm_links, spicy:::.ordinal_title_prefix, character(1)),
    vapply(.polr_links, spicy:::.ordinal_title_prefix, character(1)),
    vapply(
      .clm_links,
      spicy:::.ordinal_title_prefix,
      character(1),
      partial = TRUE
    ),
    vapply(
      .polr_links,
      spicy:::.ordinal_title_prefix,
      character(1),
      mixed = TRUE
    ),
    # svyolr fits the polr links.
    vapply(.polr_links, spicy:::.svyolr_title_prefix, character(1)),
    # clmm's cloglog title, which does not claim proportional hazards
    # (its cloglog and loglog links are swapped, runehaubo/ordinal#76).
    "Cumulative complementary log-log mixed-effects regression"
  ))
})

test_that("the hurdle and zero-inflated titles are bridged", {
  dists <- c("poisson", "negbin", "geometric")
  .expect_bridged(c(
    vapply(dists, spicy:::.pscl_title_prefix, character(1), is_hurdle = TRUE),
    vapply(dists, spicy:::.pscl_title_prefix, character(1), is_hurdle = FALSE)
  ))
})

test_that("the parametric survival titles are bridged", {
  # survreg's distributions (survival::survreg.distributions) and
  # flexsurvreg's, under the names their fits carry (`fit$dlist$name`,
  # ".quiet" dropped), flexsurvspline()'s included.
  survreg <- c(
    "extreme",
    "logistic",
    "gaussian",
    "weibull",
    "exponential",
    "rayleigh",
    "loggaussian",
    "lognormal",
    "loglogistic",
    "t"
  )
  flexsurv <- c(
    "genf",
    "genf.orig",
    "gengamma",
    "gengamma.orig",
    "exp",
    "weibull",
    "weibullPH",
    "lnorm",
    "gamma",
    "gompertz",
    "llogis",
    "survspline"
  )
  # A survreg distribution is titled by the scale survival fits it on
  # (an AFT model of log T, or a censored regression of T itself).
  survreg_title <- function(d) {
    spicy:::.survreg_title_prefix(
      d,
      spicy:::.survreg_dist(list(dist = d))$raw_time
    )
  }
  .expect_bridged(c(
    vapply(survreg, survreg_title, character(1)),
    vapply(flexsurv, spicy:::.flexsurv_title_prefix, character(1))
  ))
})

test_that("the Bayesian titles are bridged", {
  fit <- structure(list(), class = c("stanreg", "glm", "lm"))
  # The glm families as rstanarm and brms spell them (brms: "bernoulli",
  # "gamma"), then the other families they fit.
  glm <- c(
    .fams(c("gaussian", "binomial", "poisson", "Gamma", "inverse.gaussian")),
    lapply(c("logit", "probit", "cloglog", "log", "cauchit"), function(l) {
      list(family = "bernoulli", link = l)
    }),
    list(list(family = "gamma", link = "log"))
  )
  other <- lapply(
    c(
      "neg_binomial_2",
      "negbinomial",
      "beta",
      "student",
      "skew_normal",
      "lognormal",
      "weibull",
      "exponential",
      "geometric",
      "beta_binomial",
      "zero_inflated_poisson",
      "zero_inflated_negbinomial",
      "hurdle_poisson",
      "hurdle_negbinomial",
      # A family the title does not know (the ordinal and categorical
      # families of brms are refused, .brms_refuse_ordinal()).
      "frechet"
    ),
    function(f) list(family = f, link = "log")
  )
  # An ordinal stan_polr is refused (.stan_refuse_ordinal()), so it has
  # no title.
  .expect_bridged(c(
    .titles(glm, function(f) spicy:::.stan_title_prefix(fit, f)),
    .titles(other, function(f) spicy:::.stan_title_prefix(fit, f))
  ))
})

test_that("the univariable screen titles are bridged", {
  # The screen's title runs its prefix through the bridge capitalized,
  # the Tweedie families of statmod ("Tweedie") and mgcv ("Tweedie(1.5)")
  # included. The Cox model has whole templates of its own.
  glm <- c(
    Filter(
      function(f) {
        !(identical(f$family, "gaussian") && identical(f$link, "identity"))
      },
      .fams()
    ),
    list(
      list(family = "Tweedie", link = "mu^0"),
      list(family = "Tweedie(1.5)", link = "log")
    )
  )
  .expect_bridged(c(
    spicy:::capitalize_first(spicy:::.uv_title_prefix("lm", NULL)),
    .titles(glm, function(f) {
      spicy:::capitalize_first(spicy:::.uv_title_prefix("glm", f))
    })
  ))
})

test_that("a univariable screen of an unknown family keeps a French title", {
  # A family no title knows: poisson renamed. English names it by its R
  # name; French falls back to the generic title, never to English.
  fam <- stats::poisson()
  fam$family <- "foo"
  d <- mtcars
  d$cnt <- round(d$mpg)
  screen <- function(multivariable) {
    attr(
      table_regression_uv(
        d,
        outcome = "cnt",
        predictors = c("wt", "hp"),
        family = fam,
        multivariable = multivariable
      ),
      "title"
    )
  }
  expect_identical(
    screen(TRUE),
    "Univariable and multivariable foo regression: cnt"
  )
  withr::local_options(spicy.language = "fr")
  expect_identical(
    screen(TRUE),
    "Régression univariable et multivariable\u00a0: cnt"
  )
  expect_identical(
    screen(FALSE),
    "Régression univariable (présélection)\u00a0: cnt"
  )
})

test_that("a French title spells out its method, without an English acronym", {
  fr <- spicy:::.FR_TITLE_PREFIXES
  expect_false(any(grepl("(PH)", fr, fixed = TRUE)))
  expect_false(any(grepl("2SLS", fr, fixed = TRUE)))
  expect_identical(
    unname(fr["Weibull (PH) parametric survival regression"]),
    "Régression de survie paramétrique de Weibull (risques proportionnels)"
  )
  expect_identical(
    unname(fr["IV regression (2SLS)"]),
    "Régression par variables instrumentales (doubles moindres carrés)"
  )
})

test_that("the quantile title translates, its tau in the decimal comma", {
  withr::local_options(spicy.language = "fr")
  expect_identical(
    spicy:::.title_prefix_display(sprintf(
      "Quantile regression (τ = %.2f)",
      0.5
    )),
    "Régression quantile (τ = 0,50)"
  )
  # A tau with three decimals keeps them.
  expect_identical(
    spicy:::.title_prefix_display("Quantile regression (τ = 0.975)"),
    "Régression quantile (τ = 0,975)"
  )
})

test_that("a French table of quantile regressions keeps the decimals of tau", {
  skip_if_not_installed("quantreg")
  set.seed(1)
  d <- data.frame(y = stats::rnorm(200), x = stats::rnorm(200))
  q975 <- quantreg::rq(y ~ x, tau = 0.975, data = d)
  withr::local_options(spicy.language = "fr")
  tbl <- table_regression(q975)
  expect_identical(
    attr(tbl, "title"),
    "Régression quantile (τ = 0,975)\u00a0: y"
  )
  expect_match(
    attr(tbl, "note"),
    "Note. Régression quantile (τ = 0,975).",
    fixed = TRUE
  )
  # A mixed-type table (lm + rq) keeps the English title and type line
  # (coherent or nothing, decision 42), the tau in the table's mark.
  mixed <- table_regression(list(stats::lm(y ~ x, data = d), q975))
  expect_identical(attr(mixed, "title"), "Regression comparison: y")
  expect_match(
    attr(mixed, "note"),
    "Model 1: linear regression; Model 2: quantile regression (τ = 0,975).",
    fixed = TRUE
  )
})
