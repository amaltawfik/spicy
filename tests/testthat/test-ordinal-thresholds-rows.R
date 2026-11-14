# Ordinal cumulative-link thresholds rendered as a subordinate "Thresholds"
# block of rows (show_thresholds = TRUE, the default), instead of the legacy
# compact footer line. The (k-1) cut-points carry B / SE / CI / p like the
# predictor rows, cross-validated to summary.polr / summary.clm to machine
# precision, are never exponentiated, and fall back to the footer line under
# show_thresholds = FALSE.

make_ord <- function(seed = 1, n = 220) {
  set.seed(seed)
  data.frame(
    x1 = rnorm(n),
    smoke = factor(sample(c("no", "yes"), n, TRUE)),
    yc = factor(
      sample(c("Poor", "Fair", "Good", "Very good"), n, TRUE),
      levels = c("Poor", "Fair", "Good", "Very good"),
      ordered = TRUE
    )
  )
}
thr_terms <- c("Poor | Fair", "Fair | Good", "Good | Very good")

# ---- structural: thresholds become rows -----------------------------------

test_that("polr thresholds render as a labeled Thresholds block (default)", {
  skip_if_not_installed("MASS")
  fit <- MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE)
  df <- table_regression(fit)
  expect_true(any(grepl("^Thresholds", trimws(df$Variable)))) # header row
  expect_setequal(trimws(df$Variable)[grepl("\\|", df$Variable)], thr_terms)
})

test_that("clm thresholds render as a labeled Thresholds block (default)", {
  skip_if_not_installed("ordinal")
  fit <- ordinal::clm(yc ~ x1 + smoke, data = make_ord())
  df <- table_regression(fit)
  expect_true(any(grepl("^Thresholds", trimws(df$Variable))))
  expect_setequal(trimws(df$Variable)[grepl("\\|", df$Variable)], thr_terms)
})

# ---- numeric: cross-validate to summary() ---------------------------------

test_that(".append_threshold_rows matches summary.polr to machine precision", {
  skip_if_not_installed("MASS")
  fit <- MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE)
  fr <- as_regression_frame(
    fit,
    show_columns = expand_show_columns("b"),
    ci_level = 0.95
  )
  cf <- .append_threshold_rows(fr$coefs, fr$info$extras$thresholds, 0.95)
  tr <- cf[cf$is_threshold %in% TRUE, , drop = FALSE]

  sm <- summary(fit)$coefficients
  zr <- sm[grepl("\\|", rownames(sm)), , drop = FALSE]
  idx <- match(tr$term, rownames(zr))
  expect_false(anyNA(idx))
  expect_equal(tr$estimate, unname(zr[idx, "Value"]), tolerance = 1e-10)
  expect_equal(tr$std_error, unname(zr[idx, "Std. Error"]), tolerance = 1e-10)
  expect_equal(tr$statistic, unname(zr[idx, "t value"]), tolerance = 1e-8)

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
  fr <- as_regression_frame(
    fit,
    show_columns = expand_show_columns("b"),
    ci_level = 0.95
  )
  cf <- .append_threshold_rows(fr$coefs, fr$info$extras$thresholds, 0.95)
  tr <- cf[cf$is_threshold %in% TRUE, , drop = FALSE]

  sm <- summary(fit)$coefficients
  zr <- sm[grepl("\\|", rownames(sm)), , drop = FALSE]
  idx <- match(tr$term, rownames(zr))
  expect_false(anyNA(idx))
  expect_equal(tr$estimate, unname(zr[idx, "Estimate"]), tolerance = 1e-10)
  expect_equal(tr$std_error, unname(zr[idx, "Std. Error"]), tolerance = 1e-10)
  expect_equal(tr$p_value, unname(zr[idx, "Pr(>|z|)"]), tolerance = 1e-8)
})

# ---- exponentiate: thresholds stay on the log-odds scale -------------------

test_that("thresholds are NOT exponentiated under exponentiate = TRUE", {
  skip_if_not_installed("MASS")
  fit <- MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE)
  df <- table_regression(fit, exponentiate = TRUE)
  est_col <- setdiff(names(df), "Variable")[1] # "OR" header under exp
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
  fr <- as_regression_frame(
    fit,
    show_columns = expand_show_columns("b"),
    ci_level = 0.95
  )
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
  df <- table_regression(fit, show_thresholds = FALSE)
  expect_false(any(grepl("\\|", df$Variable)))
  expect_false(any(grepl("^Thresholds", trimws(df$Variable))))
})

test_that("ame-only show_columns does not promote thresholds to empty rows", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("marginaleffects")
  fit <- MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE)
  df <- table_regression(fit, show_columns = "ame")
  expect_false(any(grepl("\\|", df$Variable)))
})

test_that("non-ordinal fits are unaffected by show_thresholds", {
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  df <- table_regression(fit)
  expect_false(any(grepl("Thresholds", df$Variable)))
})

# ---- section separator before the Thresholds block ------------------------

test_that("a section separator is emitted before the Thresholds block", {
  skip_if_not_installed("MASS")
  fit <- MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE)
  res <- table_regression(fit) # default (printable) output
  sep <- attr(res, "section_sep_rows")
  expect_length(sep, 1L)
  expect_gt(sep, 1L)
  # nothing when the block is opted out
  res0 <- table_regression(fit, show_thresholds = FALSE)
  expect_length(attr(res0, "section_sep_rows"), 0L)
})

# ---- ordinal fit-stats default --------------------------------------------

test_that("polr/clm default fit-stats = n + McFadden + AIC", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("ordinal")
  for (fit in list(
    MASS::polr(yc ~ x1 + smoke, data = make_ord(), Hess = TRUE),
    ordinal::clm(yc ~ x1 + smoke, data = make_ord())
  )) {
    df <- table_regression(fit)
    v <- trimws(df$Variable)
    expect_identical(
      v[(length(v) - 2L):length(v)],
      c("n", "R² (McFadden)", "AIC")
    )
    expect_false(any(grepl("Nagelkerke", v)))
  }
})

test_that(".ordinal_pseudo_r2 matches the closed-form null and performance", {
  skip_if_not_installed("MASS")
  d <- make_ord()
  fit <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
  pr <- .ordinal_pseudo_r2(fit)

  # self-contained closed-form null log-likelihood (marginal proportions):
  # an intercept-only cumulative model reproduces the category frequencies.
  ll <- as.numeric(stats::logLik(fit))
  nk <- as.numeric(table(d$yc))
  nk <- nk[nk > 0]
  ll0 <- sum(nk * log(nk / sum(nk)))
  n <- stats::nobs(fit)
  expect_equal(pr$mcfadden, 1 - ll / ll0, tolerance = 1e-9)
  cs <- 1 - exp((ll0 - ll) * 2 / n)
  up <- 1 - exp(ll0 * 2 / n)
  expect_equal(pr$nagelkerke, cs / up, tolerance = 1e-9)

  skip_if_not_installed("performance")
  expect_equal(
    pr$mcfadden,
    as.numeric(performance::r2_mcfadden(fit)$R2),
    tolerance = 1e-6
  )
  expect_equal(
    pr$nagelkerke,
    as.numeric(performance::r2_nagelkerke(fit)),
    tolerance = 1e-6
  )
})

test_that("ordinal pseudo-R2 survives a refit-hostile scope (update() would fail)", {
  skip_if_not_installed("MASS")
  # table_regression() called inside a function with data local to that
  # function: update(fit, . ~ 1) can't re-find the data, but the closed-form
  # null log-likelihood does not refit, so the pseudo-R2 is still computed.
  f <- function() {
    d <- make_ord()
    fit <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
    table_regression(fit)
  }
  df <- f()
  mcf <- trimws(df[grepl("McFadden", df$Variable), 2])
  expect_true(length(mcf) == 1L && nzchar(mcf) && mcf != "NA")
})

test_that("exp gloss names the true cut-point scale per link", {
  skip_if_not_installed("MASS")
  d <- make_ord()
  f_or <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
  n_or <- paste(
    attr(table_regression(f_or, exponentiate = TRUE), "note"),
    collapse = "\n"
  )
  expect_match(n_or, "log-odds scale, not exponentiated", fixed = TRUE)
  # cloglog thresholds are baseline log cumulative hazards, not log-odds.
  f_hr <- MASS::polr(
    yc ~ x1 + smoke,
    data = d,
    method = "cloglog",
    Hess = TRUE
  )
  n_hr <- paste(
    attr(table_regression(f_hr, exponentiate = TRUE), "note"),
    collapse = "\n"
  )
  expect_match(
    n_hr,
    "log-cumulative-hazard scale, not exponentiated",
    fixed = TRUE
  )
  expect_false(grepl("log-odds scale", n_hr, fixed = TRUE))
})

# ---- p_adjust: the family is the slopes ------------------------------------

test_that("p_adjust counts and adjusts an ordinal model's slopes, not its cut-points", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rms")
  d <- make_ord()
  pf <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
  lf <- rms::lrm(yc ~ x1 + smoke, data = d)
  # Raw Wald p of the two slopes and the three cut-points, from each engine.
  wald_p <- function(z) unname(2 * stats::pnorm(-abs(z)))
  sm <- summary(pf)$coefficients
  k <- lf$non.slopes
  z_l <- stats::coef(lf) / sqrt(diag(stats::vcov(lf)))
  raw <- list(
    polr = list(
      slopes = wald_p(sm[c("x1", "smokeyes"), "t value"]),
      cuts = wald_p(sm[names(pf$zeta), "t value"])
    ),
    lrm = list(
      slopes = wald_p(z_l[-seq_len(k)]),
      cuts = wald_p(z_l[seq_len(k)])
    )
  )
  fits <- list(polr = pf, lrm = lf)
  for (nm in names(fits)) {
    expect_match(
      attr(table_regression(fits[[nm]], p_adjust = "holm"), "note"),
      "P-values adjusted for 2 tests (Holm method).",
      fixed = TRUE
    )
    df <- table_regression(fits[[nm]], p_adjust = "holm", output = "data.frame")
    thr <- df$variable == "Thresholds" & df$.row_role == "level"
    slope <- df$.row_role %in% c("coef", "level") & !thr
    expect_equal(
      df$p[slope],
      stats::p.adjust(raw[[nm]]$slopes, "holm"),
      tolerance = 1e-8
    )
    expect_equal(df$p[thr], raw[[nm]]$cuts, tolerance = 1e-8)
  }
})

test_that("with the AME p-values shown, the note counts the AME family", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("rms")
  skip_if_not_installed("marginaleffects")
  d <- make_ord()
  fits <- list(
    polr = MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE),
    lrm = rms::lrm(yc ~ x1 + smoke, data = d)
  )
  # Two slopes, each with an AME on the four categories: a family of 8.
  m_ame <- 2L * nlevels(d$yc)
  note_of <- function(fit, cols, method = "bonferroni") {
    attr(table_regression(fit, show_columns = cols, p_adjust = method), "note")
  }
  is_ame <- function(x) x$estimate_type == "ame" & !x$is_reference
  for (nm in names(fits)) {
    f <- fits[[nm]]
    cols <- c("b", "ame", "ame_p")
    raw <- table_regression(f, show_columns = cols, output = "long")
    adj <- table_regression(
      f,
      show_columns = cols,
      p_adjust = "bonferroni",
      output = "long"
    )
    expect_length(raw$p.value[is_ame(raw)], m_ame)
    expect_equal(
      adj$p.value[is_ame(adj)],
      stats::p.adjust(raw$p.value[is_ame(raw)], "bonferroni"),
      tolerance = 1e-10
    )
    # The AME p-values alone: the note counts their family.
    expect_match(
      note_of(f, cols),
      sprintf("P-values adjusted for %d tests (Bonferroni method).", m_ame),
      fixed = TRUE
    )
    # Both columns of p-values: each family with its count.
    expect_match(
      note_of(f, c("b", "p", "ame", "ame_p")),
      sprintf(
        paste(
          "P-values adjusted for 2 tests (Bonferroni method).",
          "AME p-values adjusted for %d tests (Bonferroni method)."
        ),
        m_ame
      ),
      fixed = TRUE
    )
    # The AME p-values hidden: the coefficients alone (an lrm also warns
    # that the p column is the coefficients').
    n_b <- suppressWarnings(note_of(f, c("b", "p", "ame")))
    expect_match(
      n_b,
      "P-values adjusted for 2 tests (Bonferroni method).",
      fixed = TRUE
    )
    expect_false(grepl("AME p-values", n_b, fixed = TRUE))
  }
  # An AME family as large as the coefficients' needs one sentence.
  g <- stats::glm(
    I(yc >= "Good") ~ x1 + smoke,
    data = d,
    family = stats::binomial
  )
  n_g <- note_of(g, c("b", "p", "ame", "ame_p"), "holm")
  expect_match(
    n_g,
    "P-values adjusted for 2 tests (Holm method).",
    fixed = TRUE
  )
  expect_false(grepl("AME p-values", n_g, fixed = TRUE))
  # Several models: the AME counts per model, in their order.
  expect_match(
    note_of(list(fits$polr, g), c("b", "p", "ame", "ame_p")),
    sprintf(
      paste(
        "P-values adjusted within each model for 2 tests (Bonferroni method).",
        "AME p-values adjusted within each model (Bonferroni method), for",
        "%d and 2 tests respectively."
      ),
      m_ame
    ),
    fixed = TRUE
  )
  # The French sentence, from its own keys.
  withr::local_options(spicy.language = "fr")
  n_fr <- note_of(fits$polr, c("b", "p", "ame", "ame_p"))
  expect_match(n_fr, "Valeurs p des AME ajust", fixed = TRUE)
  expect_match(n_fr, sprintf("pour %d tests (m", m_ame), fixed = TRUE)
})

test_that("the note of the AME p-values leaves out a model with no AME", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("survival")
  skip_if_not_installed("marginaleffects")
  d <- make_ord()
  pf <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
  cx <- survival::coxph(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung
  )
  # Two slopes, each with an AME on the four categories: a family of 8.
  # The Cox model has no AME, hence no AME family.
  m_ame <- 2L * nlevels(d$yc)
  note_of <- function(cols) {
    attr(
      suppressWarnings(
        table_regression(list(pf, cx), show_columns = cols, p_adjust = "holm")
      ),
      "note"
    )
  }
  both <- note_of(c("b", "p", "ame", "ame_p"))
  expect_match(
    both,
    sprintf(
      paste(
        "P-values adjusted within each model for 2 tests (Holm method).",
        "AME p-values adjusted for %d tests (Holm method)."
      ),
      m_ame
    ),
    fixed = TRUE
  )
  expect_false(grepl("0 tests", both, fixed = TRUE))
  ame_only <- note_of(c("b", "ame", "ame_p"))
  expect_match(
    ame_only,
    sprintf("P-values adjusted for %d tests (Holm method).", m_ame),
    fixed = TRUE
  )
  expect_false(grepl("0 tests", ame_only, fixed = TRUE))
  withr::local_options(spicy.language = "fr")
  n_fr <- note_of(c("b", "p", "ame", "ame_p"))
  expect_match(
    n_fr,
    sprintf(
      "Valeurs p des AME ajust\u00e9es pour %d tests (m\u00e9thode de Holm).",
      m_ame
    ),
    fixed = TRUE
  )
  expect_false(grepl("0 tests", n_fr, fixed = TRUE))
})

test_that("the AME p-value note names its models when one between has none", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("survival")
  skip_if_not_installed("marginaleffects")
  d <- make_ord()
  pf <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
  cx <- survival::coxph(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung
  )
  g <- stats::glm(I(yc >= "Good") ~ x1 + smoke, data = d, family = binomial)
  # Hand count: two slopes of the polr, each with an AME on the four
  # categories (8 tests); the two slopes of the glm (2 tests); the Cox
  # model has none.
  m_pf <- 2L * nlevels(d$yc)
  m_g <- 2L
  note_of <- function() {
    attr(
      suppressWarnings(table_regression(
        list(pf, cx, g),
        show_columns = c("b", "p", "ame", "ame_p"),
        p_adjust = "holm"
      )),
      "note"
    )
  }
  en <- note_of()
  expect_match(
    en,
    sprintf(
      paste0(
        "AME p-values adjusted within each model for %d and %d tests ",
        "(Model 1 and Model 3, Holm method)."
      ),
      m_pf,
      m_g
    ),
    fixed = TRUE
  )
  expect_false(grepl("respectively", en, fixed = TRUE))
  withr::local_options(spicy.language = "fr")
  expect_match(
    note_of(),
    sprintf(
      paste0(
        "Valeurs p des AME ajust\u00e9es dans chaque mod\u00e8le pour %d et ",
        "%d tests (Mod\u00e8le 1 et Mod\u00e8le 3, m\u00e9thode de Holm)."
      ),
      m_pf,
      m_g
    ),
    fixed = TRUE
  )
})

test_that("the AME p-values alone: the note names the models it counts", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("survival")
  skip_if_not_installed("marginaleffects")
  d <- make_ord()
  pf <- MASS::polr(yc ~ x1 + smoke, data = d, Hess = TRUE)
  cx <- survival::coxph(
    survival::Surv(time, status) ~ age + sex,
    data = survival::lung
  )
  g <- stats::glm(I(yc >= "Good") ~ x1 + smoke, data = d, family = binomial)
  # Hand count: 2 slopes x 4 categories for the polr, 2 for the glm; the
  # Cox model shows no p-value and has no AME.
  note_of <- function() {
    attr(
      suppressWarnings(table_regression(
        list(pf, cx, g),
        show_columns = c("b", "ame", "ame_p"),
        p_adjust = "holm"
      )),
      "note"
    )
  }
  en <- note_of()
  expect_match(
    en,
    sprintf(
      paste0(
        "P-values adjusted within each model for %d and %d tests ",
        "(Model 1 and Model 3, Holm method)."
      ),
      2L * nlevels(d$yc),
      2L
    ),
    fixed = TRUE
  )
  expect_false(grepl("respectively", en, fixed = TRUE))
  withr::local_options(spicy.language = "fr")
  expect_match(
    note_of(),
    sprintf(
      paste0(
        "Valeurs p ajust\u00e9es dans chaque mod\u00e8le pour %d et %d tests ",
        "(Mod\u00e8le 1 et Mod\u00e8le 3, m\u00e9thode de Holm)."
      ),
      2L * nlevels(d$yc),
      2L
    ),
    fixed = TRUE
  )
})

test_that("a family of one AME test, and equal AME families, have their keys", {
  skip_if_not_installed("marginaleffects")
  # poly(wt, 2) has two coefficients and one AME (that of wt): the AME
  # family is 1, and 2 with hp beside it.
  g <- stats::glm(am ~ poly(wt, 2), data = mtcars, family = stats::binomial)
  h <- suppressWarnings(stats::glm(
    am ~ hp + poly(wt, 2),
    data = mtcars,
    family = stats::binomial
  ))
  note_of <- function(x) {
    attr(
      suppressWarnings(table_regression(
        x,
        show_columns = c("b", "p", "ame", "ame_p"),
        p_adjust = "holm"
      )),
      "note"
    )
  }
  en <- list(
    one = c(
      "P-values adjusted for 2 tests (Holm method).",
      "AME p-values adjusted for 1 test (Holm method)."
    ),
    per_model_one = c(
      "P-values adjusted within each model for 2 tests (Holm method).",
      "AME p-values adjusted within each model for 1 test (Holm method)."
    ),
    per_model = c(
      "P-values adjusted within each model for 3 tests (Holm method).",
      "AME p-values adjusted within each model for 2 tests (Holm method)."
    )
  )
  fits <- list(one = g, per_model_one = list(g, g), per_model = list(h, h))
  for (nm in names(fits)) {
    expect_match(
      note_of(fits[[nm]]),
      paste(en[[nm]], collapse = " "),
      fixed = TRUE,
      info = nm
    )
  }
  withr::local_options(spicy.language = "fr")
  fr <- list(
    one = "Valeurs p des AME ajust\u00e9es pour 1 test (m\u00e9thode de Holm).",
    per_model_one = paste0(
      "Valeurs p des AME ajust\u00e9es dans chaque mod\u00e8le pour 1 test ",
      "(m\u00e9thode de Holm)."
    ),
    per_model = paste0(
      "Valeurs p des AME ajust\u00e9es dans chaque mod\u00e8le pour 2 tests ",
      "(m\u00e9thode de Holm)."
    )
  )
  for (nm in names(fits)) {
    expect_match(note_of(fits[[nm]]), fr[[nm]], fixed = TRUE, info = nm)
  }
})
