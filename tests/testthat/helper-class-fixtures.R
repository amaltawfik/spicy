# ---------------------------------------------------------------------------
# One light fit per class of the registry (.supported_models_registry()),
# plus the variants whose capabilities differ within a class. Shared by
# the sweeps that hold the code to the registry
# (test-fit_stats_requested_rows.R, test-registry_vcov.R).
#
# Each entry: `class` (the registry class it stands for), `pkgs` (the
# packages it needs; a named element gives a minimum version), `fit` (a
# function of the shared data), `cluster` (the `cluster` of a CR* request,
# NULL when the class clusters on its own structure), and `stan` (TRUE for
# the local-only Stan fixtures, read from the cache of helper-stan-cache.R).
# A fit is built on first use and kept for the rest of the test session.
# ---------------------------------------------------------------------------

.class_fixture_data <- function(n = 240, seed = 20261118) {
  set.seed(seed)
  d <- data.frame(
    x = rnorm(n),
    z = runif(n),
    g = factor(sample(letters[1:6], n, TRUE)),
    w = runif(n, 0.5, 2)
  )
  d$id <- rep(seq_len(n / 4), each = 4)
  d$y <- 1 + 0.5 * d$x + as.numeric(d$g) / 3 + rnorm(n)
  d$pos <- exp(0.3 + 0.2 * d$x + rnorm(n, 0, 0.3))
  d$yb <- rbinom(n, 1, plogis(0.6 * d$x))
  d$cnt <- rpois(n, exp(0.4 + 0.3 * d$x))
  d$cnb <- rnbinom(n, mu = exp(0.4 + 0.3 * d$x), size = 1.5)
  d$zcnt <- ifelse(runif(n) < 0.3, 0L, d$cnt)
  d$ord <- cut(
    d$y + rnorm(n),
    3,
    labels = c("lo", "mid", "hi"),
    ordered_result = TRUE
  )
  d$mn <- factor(sample(c("A", "B", "C"), n, TRUE))
  d$prop <- pmin(pmax(plogis(0.3 * d$x + rnorm(n, 0, 0.5)), 0.01), 0.99)
  d$time <- rexp(n, exp(0.2 * d$x))
  d$status <- rbinom(n, 1, 0.75)
  d$cens <- pmax(d$y, 0.5)
  d$sel <- rbinom(n, 1, plogis(0.5 + 0.5 * d$z))
  d$ysel <- ifelse(d$sel == 1, d$y, NA)
  d$xnl <- runif(n, 1, 5)
  d$ynl <- 3 * exp(-0.4 * d$xnl) + rnorm(n, 0, 0.05)
  d$inst <- d$x + rnorm(n)
  d$xen <- d$inst + rnorm(n, 0, 0.5)
  d$yiv <- 1 + 0.5 * d$xen + rnorm(n)
  d
}

# The default `cluster` of a CR* request: one id per row of the data, four
# rows each (a column name is looked up in the model frame, which does not
# carry `id`); one per choice situation for mlogit.
.cf_cluster <- function(fit) {
  n <- if (inherits(fit, "mlogit")) NROW(fit$probabilities) else 240L
  rep(seq_len(ceiling(n / 4)), each = 4)[seq_len(n)]
}

.cf <- function(class, fit, pkgs = NULL, cluster = .cf_cluster, stan = FALSE) {
  list(class = class, pkgs = pkgs, fit = fit, cluster = cluster, stan = stan)
}

.svy_des <- function(d) {
  survey::svydesign(id = ~1, weights = ~w, data = d)
}

.CLASS_FIXTURES <- list(
  lm = .cf("lm", function(d) lm(y ~ x, d)),
  lm_weighted = .cf("lm", function(d) lm(y ~ x, d, weights = w)),
  glm_gaussian = .cf("glm", function(d) glm(y ~ x, gaussian, d)),
  glm_logit = .cf("glm", function(d) glm(yb ~ x, binomial, d)),
  glm_probit = .cf("glm", function(d) glm(yb ~ x, binomial("probit"), d)),
  glm_poisson = .cf("glm", function(d) glm(cnt ~ x, poisson, d)),
  glm_gamma = .cf("glm", function(d) glm(pos ~ x, Gamma("log"), d)),
  glm_quasi = .cf("glm", function(d) glm(cnt ~ x, quasipoisson, d)),
  nls = .cf("nls", function(d) {
    nls(ynl ~ a * exp(-b * xnl), d, start = list(a = 3, b = 0.4))
  }),
  glm.nb = .cf("negbin", function(d) MASS::glm.nb(cnb ~ x, d), "MASS"),
  rlm = .cf("rlm", function(d) MASS::rlm(y ~ x, d), "MASS"),
  lm_robust = .cf(
    "lm_robust",
    function(d) estimatr::lm_robust(y ~ x, data = d),
    "estimatr"
  ),
  lm_robust_fe = .cf(
    "lm_robust",
    function(d) estimatr::lm_robust(y ~ x, data = d, fixed_effects = ~g),
    "estimatr"
  ),
  iv_robust = .cf(
    "iv_robust",
    function(d) estimatr::iv_robust(yiv ~ xen | inst, data = d),
    "estimatr"
  ),
  ivreg = .cf(
    "ivreg",
    function(d) AER::ivreg(yiv ~ xen | inst, data = d),
    "AER"
  ),
  tobit = .cf(
    "tobit",
    function(d) AER::tobit(cens ~ x, left = 0.5, data = d),
    "AER"
  ),
  rq = .cf("rq", function(d) quantreg::rq(y ~ x, data = d), "quantreg"),
  feols = .cf("fixest", function(d) fixest::feols(y ~ x | g, d), "fixest"),
  feols_nofe = .cf("fixest", function(d) fixest::feols(y ~ x, d), "fixest"),
  feols_iv = .cf(
    "fixest",
    function(d) fixest::feols(yiv ~ 1 | xen ~ inst, d),
    "fixest"
  ),
  fepois = .cf("fixest", function(d) fixest::fepois(cnt ~ x | g, d), "fixest"),
  fepois_nofe = .cf("fixest", function(d) fixest::fepois(cnt ~ x, d), "fixest"),
  fenegbin = .cf(
    "fixest",
    function(d) fixest::fenegbin(cnb ~ x | g, d),
    "fixest"
  ),
  lmer = .cf(
    "lmerMod",
    function(d) lme4::lmer(y ~ x + (1 | g), d),
    "lme4",
    "g"
  ),
  lmer_ml = .cf(
    "lmerMod",
    function(d) lme4::lmer(y ~ x + (1 | g), d, REML = FALSE),
    "lme4",
    "g"
  ),
  glmer = .cf(
    "glmerMod",
    function(d) lme4::glmer(yb ~ x + (1 | g), family = binomial, data = d),
    "lme4",
    "g"
  ),
  glmer_nb = .cf(
    "glmerMod",
    function(d) lme4::glmer.nb(cnb ~ x + (1 | g), data = d),
    "lme4",
    "g"
  ),
  glmmTMB = .cf(
    "glmmTMB",
    function(d) glmmTMB::glmmTMB(y ~ x + (1 | g), d),
    "glmmTMB",
    "g"
  ),
  glmmTMB_reml = .cf(
    "glmmTMB",
    function(d) glmmTMB::glmmTMB(y ~ x + (1 | g), d, REML = TRUE),
    "glmmTMB",
    "g"
  ),
  glmmTMB_ordinal = .cf(
    "glmmTMB",
    function(d) {
      glmmTMB::glmmTMB(ord ~ x + (1 | g), d, family = glmmTMB::ordinal())
    },
    c(glmmTMB = "1.1.15"),
    "g"
  ),
  glmmTMB_zi = .cf(
    "glmmTMB",
    function(d) {
      glmmTMB::glmmTMB(
        zcnt ~ x + (1 | g),
        d,
        ziformula = ~1,
        family = poisson
      )
    },
    "glmmTMB",
    "g"
  ),
  glmmTMB_hurdle = .cf(
    "glmmTMB",
    function(d) {
      glmmTMB::glmmTMB(
        zcnt ~ x + (1 | g),
        d,
        ziformula = ~1,
        family = glmmTMB::truncated_poisson()
      )
    },
    "glmmTMB",
    "g"
  ),
  glmmTMB_disp = .cf(
    "glmmTMB",
    function(d) glmmTMB::glmmTMB(y ~ x + (1 | g), d, dispformula = ~z),
    "glmmTMB",
    "g"
  ),
  glmmTMB_nbinom2 = .cf(
    "glmmTMB",
    function(d) {
      glmmTMB::glmmTMB(cnb ~ x + (1 | g), d, family = glmmTMB::nbinom2())
    },
    "glmmTMB",
    "g"
  ),
  glmmTMB_nbinom1 = .cf(
    "glmmTMB",
    function(d) {
      glmmTMB::glmmTMB(cnb ~ x + (1 | g), d, family = glmmTMB::nbinom1())
    },
    "glmmTMB",
    "g"
  ),
  glmmTMB_beta = .cf(
    "glmmTMB",
    function(d) {
      glmmTMB::glmmTMB(prop ~ x + (1 | g), d, family = glmmTMB::beta_family())
    },
    "glmmTMB",
    "g"
  ),
  lme = .cf(
    "lme",
    function(d) nlme::lme(y ~ x, random = ~ 1 | g, data = d),
    "nlme",
    "g"
  ),
  lme_contrasts = .cf(
    "lme",
    function(d) {
      nlme::lme(
        y ~ x + mn,
        random = ~ 1 | g,
        data = d,
        contrasts = list(mn = "contr.sum")
      )
    },
    "nlme",
    "g"
  ),
  gls = .cf("gls", function(d) nlme::gls(y ~ x, data = d), "nlme"),
  polr = .cf(
    "polr",
    function(d) MASS::polr(ord ~ x, d, Hess = TRUE),
    "MASS"
  ),
  clm = .cf("clm", function(d) ordinal::clm(ord ~ x, data = d), "ordinal"),
  clm_scale = .cf(
    "clm",
    function(d) ordinal::clm(ord ~ x, scale = ~z, data = d),
    "ordinal"
  ),
  clm_nominal = .cf(
    "clm",
    function(d) ordinal::clm(ord ~ x, nominal = ~z, data = d),
    "ordinal"
  ),
  clmm = .cf(
    "clmm",
    function(d) ordinal::clmm(ord ~ x + (1 | g), data = d),
    "ordinal",
    "g"
  ),
  multinom = .cf(
    "multinom",
    function(d) nnet::multinom(mn ~ x, d, trace = FALSE),
    "nnet"
  ),
  mlogit = .cf(
    "mlogit",
    function(d) {
      data("Fishing", package = "mlogit", envir = environment())
      fd <- mlogit::mlogit.data(
        Fishing,
        varying = 2:9,
        shape = "wide",
        choice = "mode"
      )
      mlogit::mlogit(mode ~ price + catch, data = fd)
    },
    "mlogit"
  ),
  zeroinfl = .cf(
    "zeroinfl",
    function(d) pscl::zeroinfl(zcnt ~ x | 1, data = d),
    "pscl"
  ),
  zeroinfl_negbin = .cf(
    "zeroinfl",
    function(d) pscl::zeroinfl(zcnt ~ x | 1, data = d, dist = "negbin"),
    "pscl"
  ),
  hurdle = .cf(
    "hurdle",
    function(d) pscl::hurdle(zcnt ~ x | 1, data = d),
    "pscl"
  ),
  hurdle_negbin = .cf(
    "hurdle",
    function(d) pscl::hurdle(zcnt ~ x | 1, data = d, dist = "negbin"),
    "pscl"
  ),
  coxph = .cf(
    "coxph",
    function(d) survival::coxph(survival::Surv(time, status) ~ x, d),
    "survival"
  ),
  coxph_strata = .cf(
    "coxph",
    function(d) {
      survival::coxph(
        survival::Surv(time, status) ~ x + survival::strata(mn),
        d
      )
    },
    "survival"
  ),
  survreg = .cf(
    "survreg",
    function(d) survival::survreg(survival::Surv(time, status) ~ x, d),
    "survival"
  ),
  survreg_strata = .cf(
    "survreg",
    function(d) {
      survival::survreg(
        survival::Surv(time, status) ~ x + survival::strata(mn),
        d
      )
    },
    "survival"
  ),
  cph = .cf(
    "cph",
    function(d) rms::cph(survival::Surv(time, status) ~ x, data = d),
    c("rms", "survival")
  ),
  flexsurvreg = .cf(
    "flexsurvreg",
    function(d) {
      flexsurv::flexsurvreg(
        survival::Surv(time, status) ~ x,
        data = d,
        dist = "weibull"
      )
    },
    c("flexsurv", "survival")
  ),
  svyglm = .cf(
    "svyglm",
    function(d) survey::svyglm(y ~ x, design = .svy_des(d)),
    "survey"
  ),
  svyglm_binomial = .cf(
    "svyglm",
    function(d) {
      survey::svyglm(yb ~ x, design = .svy_des(d), family = binomial)
    },
    "survey"
  ),
  svyglm_quasibinomial = .cf(
    "svyglm",
    function(d) {
      survey::svyglm(yb ~ x, design = .svy_des(d), family = quasibinomial)
    },
    "survey"
  ),
  svyolr = .cf(
    "svyolr",
    function(d) survey::svyolr(ord ~ x, design = .svy_des(d)),
    "survey"
  ),
  svycoxph = .cf(
    "svycoxph",
    function(d) {
      survey::svycoxph(
        survival::Surv(time, status) ~ x,
        design = .svy_des(d)
      )
    },
    c("survey", "survival")
  ),
  geeglm = .cf(
    "geeglm",
    function(d) geepack::geeglm(cnt ~ x, poisson, d, id = id),
    "geepack"
  ),
  gam = .cf("gam", function(d) mgcv::gam(y ~ x + s(z), data = d), "mgcv"),
  gam_logit = .cf(
    "gam",
    function(d) mgcv::gam(yb ~ x + s(z), family = binomial, data = d),
    "mgcv"
  ),
  bam = .cf(
    "gam",
    function(d) mgcv::bam(cnt ~ x + s(z), family = poisson, data = d),
    "mgcv"
  ),
  gam_nb = .cf(
    "gam",
    function(d) mgcv::gam(cnb ~ x + s(z), family = mgcv::nb(), data = d),
    "mgcv"
  ),
  gam_betar = .cf(
    "gam",
    function(d) mgcv::gam(prop ~ x + s(z), family = mgcv::betar(), data = d),
    "mgcv"
  ),
  betareg = .cf(
    "betareg",
    function(d) betareg::betareg(prop ~ x, data = d),
    "betareg"
  ),
  selection = .cf(
    "selection",
    function(d) sampleSelection::selection(sel ~ z, ysel ~ x, data = d),
    "sampleSelection"
  ),
  ols = .cf("ols", function(d) rms::ols(y ~ x, data = d), "rms"),
  lrm = .cf("lrm", function(d) rms::lrm(yb ~ x, data = d), "rms"),
  lrm_ordinal = .cf("lrm", function(d) rms::lrm(ord ~ x, data = d), "rms"),
  rms_Glm = .cf(
    "Glm",
    function(d) rms::Glm(cnt ~ x, family = poisson, data = d),
    "rms"
  ),
  # A binomial Glm has the R-squared rows of its glm twin, Tjur's by
  # default (as gam_logit for the GAM).
  rms_Glm_logit = .cf(
    "Glm",
    function(d) rms::Glm(yb ~ x, family = binomial, data = d),
    "rms"
  ),
  # rms::robcov() needs the model matrices: the same fits with x and y.
  ols_xy = .cf(
    "ols",
    function(d) rms::ols(y ~ x, data = d, x = TRUE, y = TRUE),
    "rms"
  ),
  lrm_xy = .cf(
    "lrm",
    function(d) rms::lrm(yb ~ x, data = d, x = TRUE, y = TRUE),
    "rms"
  ),
  rms_Glm_xy = .cf(
    "Glm",
    function(d) {
      rms::Glm(cnt ~ x, family = poisson, data = d, x = TRUE, y = TRUE)
    },
    "rms"
  ),
  cph_xy = .cf(
    "cph",
    function(d) {
      rms::cph(
        survival::Surv(time, status) ~ x,
        data = d,
        x = TRUE,
        y = TRUE
      )
    },
    c("rms", "survival")
  ),
  # The Stan fixtures of test-regression_frame_stan.R, read from the same
  # cache (the bodies match it character for character).
  stanreg = .cf(
    "stanreg",
    function(d) {
      .stan_cached_fit(
        "rstanarm_basic",
        .STAN_CACHE_PKGS_RSTANARM,
        function() {
          set.seed(4)
          rstanarm::stan_glm(
            Reaction ~ Days,
            data = lme4::sleepstudy,
            chains = 1,
            iter = 400,
            refresh = 0
          )
        }
      )
    },
    c("rstanarm", "posterior", "lme4"),
    NULL,
    stan = TRUE
  ),
  brmsfit = .cf(
    "brmsfit",
    function(d) {
      .stan_cached_fit("brms_basic", .STAN_CACHE_PKGS_BRMS, function() {
        set.seed(1)
        brms::brm(
          Reaction ~ Days,
          data = lme4::sleepstudy,
          chains = 1,
          iter = 400,
          refresh = 0,
          silent = 2,
          backend = "rstan"
        )
      })
    },
    c("brms", "posterior", "lme4"),
    NULL,
    stan = TRUE
  )
)

.class_fixture_cache <- new.env(parent = emptyenv())

# The fit of fixture `id`, built once per test session. Skips the calling
# test when a package is missing, and outside a local run for Stan.
.class_fixture <- function(id) {
  fx <- .CLASS_FIXTURES[[id]]
  if (isTRUE(fx$stan)) {
    .skip_unless_local_stan()
  }
  for (i in seq_along(fx$pkgs)) {
    p <- fx$pkgs[[i]]
    nm <- names(fx$pkgs)[i]
    if (is.null(nm) || !nzchar(nm)) {
      testthat::skip_if_not_installed(p)
    } else {
      testthat::skip_if_not_installed(nm, p)
    }
  }
  if (is.null(.class_fixture_cache[[id]])) {
    .class_fixture_cache[[id]] <- suppressWarnings(suppressMessages(
      fx$fit(.class_fixture_data())
    ))
  }
  .class_fixture_cache[[id]]
}
