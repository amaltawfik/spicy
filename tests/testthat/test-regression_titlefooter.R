# The note sentence that names an exponentiated intercept: exp(B0) sits
# under the ratio header without being a ratio (ADR
# dev/decisions/2026-09-05-exp-intercept-under-ratio-header.md).

exp_note <- function(models, ...) {
  out <- suppressWarnings(suppressMessages(table_regression(
    models,
    exponentiate = TRUE,
    ...
  )))
  paste(attr(out, "note"), collapse = "\n")
}

make_exp_d <- function(seed = 5, n = 300) {
  set.seed(seed)
  d <- data.frame(
    x = rnorm(n),
    g = factor(sample(1:12, n, TRUE)),
    time = rexp(n, 0.2) + 0.1,
    status = rbinom(n, 1, 0.7)
  )
  d$yb <- rbinom(n, 1, plogis(-0.4 + 0.6 * d$x))
  d$yr <- rbinom(n, 1, 0.2 * exp(0.2 * d$x) / max(exp(0.2 * d$x)) + 0.05)
  d$yp <- rpois(n, exp(0.5 + 0.3 * d$x))
  d$yz <- ifelse(runif(n) < 0.3, 0L, d$yp)
  d$yg <- rgamma(n, shape = 2, rate = 2 / exp(1 + 0.3 * d$x))
  d$yc <- d$x + rnorm(n)
  d
}

test_that("the exponentiated intercept is named by family and link", {
  d <- make_exp_d()
  expect_match(
    exp_note(glm(yb ~ x, binomial, d)),
    "The (Intercept) row estimates the baseline odds.",
    fixed = TRUE
  )
  expect_match(
    exp_note(glm(yp ~ x, poisson, d)),
    "The (Intercept) row estimates the baseline rate.",
    fixed = TRUE
  )
  expect_match(
    exp_note(glm(yr ~ x, binomial(link = "log"), d, start = c(-1.5, 0))),
    "The (Intercept) row estimates the baseline risk.",
    fixed = TRUE
  )
  expect_match(
    exp_note(glm(yg ~ x, Gamma(link = "log"), d)),
    "The (Intercept) row estimates the baseline mean.",
    fixed = TRUE
  )
  expect_match(
    exp_note(glm(yb ~ x, binomial(link = "cloglog"), d)),
    "The (Intercept) row estimates the baseline cumulative hazard.",
    fixed = TRUE
  )
})

test_that("no intercept shown, no sentence", {
  d <- make_exp_d()
  m <- glm(yb ~ x, binomial, d)
  expect_no_match(
    exp_note(m, show_intercept = FALSE),
    "baseline",
    fixed = TRUE
  )
  expect_no_match(exp_note(m, show_columns = "p"), "baseline", fixed = TRUE)
  skip_if_not_installed("MASS")
  d$yo <- cut(d$yc, 3, ordered_result = TRUE)
  expect_no_match(
    exp_note(MASS::polr(yo ~ x, data = d, Hess = TRUE)),
    "baseline",
    fixed = TRUE
  )
})

test_that("side-by-side models name the models whose intercept is meant", {
  d <- make_exp_d()
  ml <- lm(yc ~ x, d)
  mb <- glm(yb ~ x, binomial, d)
  mp <- glm(yp ~ x, poisson, d)
  expect_match(
    exp_note(list(ml, mb)),
    "The (Intercept) row estimates the baseline odds (Model 2).",
    fixed = TRUE
  )
  expect_match(
    exp_note(list(mb, mp)),
    "The (Intercept) row estimates the baseline odds (Model 1); the baseline rate (Model 2).",
    fixed = TRUE
  )
  expect_match(
    exp_note(list(mb, glm(yb ~ x + yc, binomial, d))),
    "The (Intercept) row estimates the baseline odds.",
    fixed = TRUE
  )
  expect_match(
    exp_note(list(A = mb, B = mp)),
    "the baseline odds (A); the baseline rate (B)",
    fixed = TRUE
  )
})

test_that("mixed, zero-inflated, survreg and multinomial fits are named", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("survival")
  skip_if_not_installed("nnet")
  d <- make_exp_d()
  expect_match(
    exp_note(lme4::glmer(yb ~ x + (1 | g), data = d, family = binomial)),
    "The (Intercept) row estimates the baseline odds.",
    fixed = TRUE
  )
  zi <- glmmTMB::glmmTMB(yz ~ x, ziformula = ~1, family = poisson, data = d)
  nz <- exp_note(zi)
  expect_match(
    nz,
    "The (Intercept) row estimates the baseline rate.",
    fixed = TRUE
  )
  expect_match(
    nz,
    "displayed as odds ratios. The (Intercept) row estimates the baseline odds.",
    fixed = TRUE
  )
  wb <- survival::survreg(
    survival::Surv(time, status) ~ x,
    data = d,
    dist = "weibull"
  )
  expect_match(exp_note(wb), "63.2nd percentile", fixed = TRUE)
  ln <- survival::survreg(
    survival::Surv(time, status) ~ x,
    data = d,
    dist = "lognormal"
  )
  expect_match(
    exp_note(ln),
    "The (Intercept) row estimates the baseline median survival time.",
    fixed = TRUE
  )
  d$yo <- cut(d$yc, 3)
  mn <- nnet::multinom(yo ~ x, data = d, trace = FALSE)
  expect_match(
    exp_note(mn),
    "The (Intercept) row estimates the baseline odds.",
    fixed = TRUE
  )
})

test_that("the sentence is translated", {
  d <- make_exp_d()
  old <- options(spicy.language = "fr")
  on.exit(options(old))
  expect_match(
    exp_note(glm(yb ~ x, binomial, d)),
    "La ligne (Intercept) estime la cote de base.",
    fixed = TRUE
  )
  expect_match(
    exp_note(glm(yp ~ x, poisson, d)),
    "La ligne (Intercept) estime le taux de base.",
    fixed = TRUE
  )
})

test_that("a stacked panel goes on in lower case after its label", {
  d <- make_exp_d()
  s <- suppressWarnings(table_stack(
    A = table_regression(lm(yc ~ x, d)),
    B = table_regression(glm(yb ~ x, binomial, d), exponentiate = TRUE)
  ))
  expect_match(
    paste(attr(s, "note"), collapse = "\n"),
    "B: the (Intercept) row estimates the baseline odds.",
    fixed = TRUE
  )
})

test_that("a family and link with no single estimand get no sentence", {
  key <- function(family, link, class = "glm") {
    .exp_intercept_key(list(
      family = list(family = family, link = link),
      class = class
    ))
  }
  expect_identical(
    key("exponential", "log", "survreg"),
    "note_exp_intercept_survival_mean"
  )
  expect_identical(key("gaussian", "identity", "survreg"), NA_character_)
  expect_identical(key("cumulative", "logit"), NA_character_)
  expect_identical(key("poisson", "sqrt"), NA_character_)
  expect_identical(key("beta", "cloglog"), NA_character_)
  expect_identical(key("someFamily", "log"), NA_character_)
  expect_identical(key(NULL, "log"), NA_character_)
  expect_null(build_exp_intercept_footer_block_from_frames(list(), "b"))
  expect_identical(
    .frame_intercept_label(list(coefs = NULL)),
    NA_character_
  )
})

test_that("the sentence names the row by the label `labels` gives it", {
  d <- make_exp_d()
  mb <- glm(yb ~ x, binomial, d)
  mp <- glm(yp ~ x, poisson, d)
  lab <- c("(Intercept)" = "Constant")
  expect_match(
    exp_note(mb, labels = lab),
    "The Constant row estimates the baseline odds.",
    fixed = TRUE
  )
  expect_match(
    exp_note(list(mb, mp), labels = lab),
    "The Constant row estimates the baseline odds (Model 1); the baseline rate (Model 2).",
    fixed = TRUE
  )
  odd <- "Base (x = 0), 50% [a-z]+ %s \\d «réf.»"
  expect_match(
    exp_note(mb, labels = c("(Intercept)" = odd)),
    paste0("The ", odd, " row estimates the baseline odds."),
    fixed = TRUE
  )
  # A blank label leaves the row to its coefficient name.
  for (blank in c("", "  ")) {
    expect_match(
      exp_note(mb, labels = c("(Intercept)" = blank)),
      "The (Intercept) row estimates the baseline odds.",
      fixed = TRUE
    )
  }
  uv <- suppressWarnings(table_regression_uv(
    d,
    outcome = "yb",
    predictors = c("x", "yc"),
    family = binomial,
    exponentiate = TRUE,
    show_intercept = TRUE,
    labels = lab
  ))
  expect_match(
    paste(attr(uv, "note"), collapse = "\n"),
    "The Constant row estimates the baseline odds.",
    fixed = TRUE
  )
  old <- options(spicy.language = "fr")
  on.exit(options(old))
  expect_match(
    exp_note(mb, labels = c("(Intercept)" = "Constante")),
    "La ligne Constante estime la cote de base.",
    fixed = TRUE
  )
})

test_that("rows `labels` does not reach keep \"(Intercept)\"", {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("mlogit")
  d <- make_exp_d()
  lab <- c("(Intercept)" = "Constant")
  # The intercept of a zero-inflation block keeps its label.
  zi <- glmmTMB::glmmTMB(yz ~ x, ziformula = ~1, family = poisson, data = d)
  nz <- exp_note(zi, labels = lab)
  expect_match(
    nz,
    "The Constant row estimates the baseline rate.",
    fixed = TRUE
  )
  expect_match(
    nz,
    "odds ratios. The (Intercept) row estimates the baseline odds.",
    fixed = TRUE
  )
  # So do the alternative-specific constants of mlogit: beside a glm
  # whose intercept is relabeled, one sentence per label.
  data("Fishing", package = "mlogit", envir = environment())
  fish <- mlogit::dfidx(
    Fishing,
    varying = 2:9,
    choice = "mode",
    shape = "wide"
  )
  ml <- mlogit::mlogit(mode ~ price | income, data = fish)
  two <- exp_note(list(glm(yb ~ x, binomial, d), ml), labels = lab)
  expect_match(
    two,
    paste(
      "The Constant row estimates the baseline odds.",
      "The (Intercept) row estimates the baseline odds."
    ),
    fixed = TRUE
  )
})

test_that("a stacked panel names its own label, in lower case after it", {
  d <- make_exp_d()
  mb <- glm(yb ~ x, binomial, d)
  s <- suppressWarnings(table_stack(
    A = table_regression(
      mb,
      exponentiate = TRUE,
      labels = c("(Intercept)" = "Constant A")
    ),
    B = table_regression(
      mb,
      exponentiate = TRUE,
      labels = c("(Intercept)" = "Constant B")
    )
  ))
  txt <- paste(attr(s, "note"), collapse = "\n")
  expect_match(
    txt,
    "A: the Constant A row estimates the baseline odds.",
    fixed = TRUE
  )
  expect_match(
    txt,
    "B: the Constant B row estimates the baseline odds.",
    fixed = TRUE
  )
})

test_that("a zero-inflation block without an intercept gets no sentence", {
  skip_if_not_installed("glmmTMB")
  d <- make_exp_d()
  zi <- glmmTMB::glmmTMB(
    yz ~ x,
    ziformula = ~ 0 + x,
    family = poisson,
    data = d
  )
  expect_no_match(
    exp_note(zi),
    "odds ratios. The (Intercept)",
    fixed = TRUE
  )
})
