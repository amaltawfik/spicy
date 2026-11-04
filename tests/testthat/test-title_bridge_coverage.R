# Every English title prefix an engine can produce has an entry in the
# French title bridge (`.FR_TITLE_PREFIXES`, R/i18n_fr.R). A prefix
# without one is no error at run time -- the table keeps its whole
# English title under `spicy.language = "fr"`, coherent or nothing --
# but it is a gap in the language. The prefixes are enumerated by
# calling each engine's title helper with every family, link and
# distribution it can receive, with no model fitted; a title an engine
# sets as a fixed string is listed with the engine that sets it.

.bridge_keys <- function() names(spicy:::.FR_TITLE_PREFIXES)

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
    list(family = "gaulss", link = "identity")
  )
  .expect_bridged(c(.titles(.fams(), gam), .titles(extended, gam)))
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
  # formula (a zero-truncated family with one is a hurdle model), and
  # the ordinal family on the links glmmTMB fits it with (no
  # zero-inflation: "not implemented for the ordinal family").
  fams <- c(
    .fams(c("gaussian", "binomial")),
    lapply(names(spicy:::.GLMMTMB_FAMILY_LABELS), function(f) {
      list(family = f, link = "log")
    })
  )
  ordinal <- lapply(c("logit", "probit", "cloglog"), function(l) {
    list(family = "ordinal", link = l)
  })
  .expect_bridged(c(
    .titles(fams, spicy:::.glmmTMB_title_prefix, has_zi = FALSE),
    .titles(fams, spicy:::.glmmTMB_title_prefix, has_zi = TRUE),
    .titles(ordinal, spicy:::.glmmTMB_title_prefix, has_zi = FALSE)
  ))
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
  .expect_bridged(c(
    paste0(
      vapply(survreg, spicy:::.survreg_dist_title, character(1)),
      " AFT regression"
    ),
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
      # A family the title does not know.
      "cumulative"
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
  # The screen's title runs its prefix through the bridge capitalized.
  # A family the glm title does not know keeps its R name and the whole
  # English title, so it is not enumerated.
  glm <- Filter(
    function(f) {
      !identical(f$family, "tweedie") &&
        !(identical(f$family, "gaussian") && identical(f$link, "identity"))
    },
    .fams()
  )
  .expect_bridged(c(
    spicy:::capitalize_first(spicy:::.uv_title_prefix("lm", NULL)),
    spicy:::capitalize_first(spicy:::.uv_title_prefix("coxph", NULL)),
    .titles(glm, function(f) {
      spicy:::capitalize_first(spicy:::.uv_title_prefix("glm", f))
    })
  ))
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
})
