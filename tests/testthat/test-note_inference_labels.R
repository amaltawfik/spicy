# The note names the standard errors of every class the same way
# (dev/decisions/2026-11-19-note-inference-labels.md): one name per
# estimator, a lowercase initial unless the label opens on a proper name
# or an acronym, no test statistic in the label, a French label that says
# what the English one says, and an inference sentence that holds for some
# models of a table only names them.

# The values each fixture refuses although its class cell lists them, as
# in test-registry_vcov.R.
nl_refused <- list(
  glmmTMB_reml = c("CR0", "CR1"),
  glmmTMB_ordinal = c("CR0", "CR1"),
  lme_contrasts = c("CR0", "CR1", "CR2", "CR3"),
  clm_scale = "CR1",
  clm_nominal = "CR1",
  zeroinfl_negbin = "CR1",
  hurdle_negbin = "CR1",
  cph = "CR0",
  ols = "CR0",
  lrm = "CR0",
  lrm_ordinal = "CR0",
  rms_Glm = "CR0",
  rms_Glm_logit = "CR0",
  gam_betar = "CR1"
)

# One robust value per family keeps the sweep short: HC1 and HC3 stand for
# HC0 to HC5, which share their template, and CR1 and CR2 for CR0 to CR3.
nl_modes <- c(
  "classical",
  "HC1",
  "HC3",
  "CR1",
  "CR2",
  "CR1S",
  "bootstrap",
  "jackknife",
  "nid",
  "iid",
  "ker",
  "rank"
)

nl_note <- function(fit, id, mode, lang) {
  args <- list(fit, output = "data.frame")
  if (!is.null(mode)) {
    args$vcov <- mode
    clu <- .CLASS_FIXTURES[[id]]$cluster
    if (grepl("^CR", mode) && !is.null(clu)) {
      args$cluster <- if (is.function(clu)) clu(fit) else clu
    }
    if (mode == "bootstrap") {
      args$boot_n <- 20L
    }
  }
  withr::local_options(spicy.language = lang)
  set.seed(1)
  out <- suppressWarnings(suppressMessages(do.call(table_regression, args)))
  attr(out, "note")
}

# The label after "Std. errors: " in the note of a one-model table.
nl_label <- function(note, lang) {
  lead <- if (lang == "en") "Std. errors: " else "Erreurs standards\u00A0: "
  at <- regexpr(lead, note, fixed = TRUE)
  if (at < 0L) {
    return(NA_character_)
  }
  rest <- substring(note, at + attr(at, "match.length"))
  # The label ends at the period that closes the sentence: the first one
  # followed by the next sentence, or the end of the note.
  sub("\\.(\\s+[A-Z\u00C0-\u00DD*].*)?$", "", rest)
}

nl_proper <- paste0(
  "^(Bayesian|Huber|Lin-Wei|Newey-West|Driscoll-Kraay|Conley|",
  "Hall-Sheather|Taylor|Stata|GEE|MAD SD|OLS|WLS|2SLS)\\b"
)

# The acronyms of a label, the English ones written as the French label
# writes them.
nl_acronyms <- function(s, lang) {
  tok <- regmatches(s, gregexpr("\\b[0-9]?[A-Z]{2,}[0-9A-Z]*s?\\b", s))[[1L]]
  if (lang == "en") {
    fr <- c(OLS = "MCO", WLS = "MCP", CIs = "IC")
    tok <- ifelse(tok %in% names(fr), fr[tok], tok)
    tok <- tok[tok != "2SLS"]
  }
  sort(as.character(unname(tok)))
}

nl_parens <- function(s) {
  c(
    lengths(regmatches(s, gregexpr("(", s, fixed = TRUE))),
    lengths(regmatches(s, gregexpr(")", s, fixed = TRUE)))
  )
}

for (id in names(.CLASS_FIXTURES)) {
  local({
    id <- id
    test_that(paste0("the standard-error labels of ", id, " follow one form"), {
      skip_on_cran()
      fit <- .class_fixture(id)
      reg <- .supported_models_registry()
      cl <- .CLASS_FIXTURES[[id]]$class
      ok <- setdiff(
        .reg_cell_tokens(reg$vcov[reg$class == cl]),
        nl_refused[[id]]
      )
      modes <- c(list(NULL), as.list(intersect(nl_modes, ok)))
      for (m in modes) {
        tag <- paste(id, m %||% "(default)")
        en <- nl_label(nl_note(fit, id, m, "en"), "en")
        fr <- nl_label(nl_note(fit, id, m, "fr"), "fr")
        if (is.na(en)) {
          next
        }
        # (a) a lowercase initial, or a proper name or an acronym.
        expect_true(
          grepl("^[a-z]", en) || grepl(nl_proper, en, perl = TRUE),
          label = paste(tag, en)
        )
        # (d) the label names an estimator, never a test.
        expect_false(grepl("(z)", en, fixed = TRUE), label = paste(tag, en))
        expect_false(grepl("Wald", en, fixed = TRUE), label = paste(tag, en))
        # (c) the French label has the same parentheses and acronyms.
        expect_false(is.na(fr), label = tag)
        expect_identical(nl_parens(fr), nl_parens(en), label = paste(tag, fr))
        expect_identical(
          nl_acronyms(fr, "fr"),
          nl_acronyms(en, "en"),
          label = paste(tag, en, "|", fr)
        )
      }
    })
  })
}

# The selection fixture is a selection() fit by maximum likelihood; a
# two-step heckit() fit and a treatReg() fit are held to the same form.
test_that("the standard-error labels of the selection models follow one form", {
  skip_on_cran()
  skip_if_not_installed("sampleSelection")
  data("Mroz87", package = "sampleSelection", envir = environment())
  fits <- list(
    heckit = sampleSelection::heckit(
      lfp ~ age + educ,
      wage ~ exper + educ,
      data = Mroz87
    ),
    treatReg = sampleSelection::treatReg(
      lfp ~ age + educ,
      wage ~ lfp + educ,
      data = Mroz87
    )
  )
  want <- c(
    heckit = "classical (two-step)",
    treatReg = "classical (Fisher information)"
  )
  for (id in names(fits)) {
    en <- nl_label(nl_note(fits[[id]], id, NULL, "en"), "en")
    fr <- nl_label(nl_note(fits[[id]], id, NULL, "fr"), "fr")
    expect_identical(en, want[[id]], label = id)
    expect_identical(nl_parens(fr), nl_parens(en), label = paste(id, fr))
    expect_identical(nl_acronyms(fr, "fr"), nl_acronyms(en, "en"), label = id)
  }
  expect_identical(
    nl_label(
      nl_note(.class_fixture("selection"), "selection", NULL, "en"),
      "en"
    ),
    nl_label(nl_note(.class_fixture("betareg"), "betareg", NULL, "en"), "en")
  )
})

test_that("twin fits print the same default label", {
  skip_on_cran()
  twins <- list(
    c("glm_logit", "lrm", "rms_Glm_logit"),
    c("lm", "ols"),
    c("coxph", "cph")
  )
  for (tw in twins) {
    labs <- vapply(
      tw,
      function(id) nl_label(nl_note(.class_fixture(id), id, NULL, "en"), "en"),
      character(1)
    )
    expect_length(unique(labs), 1L)
  }
})

# A fit that carries a robust variance of its own is labeled by that
# variance, never as classical: coxph and survreg keep the model's in
# fit$naive.var, an rms fit in fit$orig.var.
test_that("a robust variance carried by the fit is named, not called classical", {
  skip_on_cran()
  skip_if_not_installed("survival")
  skip_if_not_installed("rms")
  want <- list(
    coxph_cluster = c(
      "cluster-robust (Lin-Wei), clusters by id",
      "robustes aux grappes (Lin-Wei), grappes définies par id"
    ),
    survreg_robust = c("robust (Huber)", "robustes (Huber)"),
    lrm_robcov = c(
      "cluster-robust (Huber), clusters by id",
      "robustes aux grappes (Huber), grappes définies par id"
    )
  )
  for (id in names(want)) {
    fit <- .class_fixture(id)
    expect_identical(
      nl_label(nl_note(fit, id, NULL, "en"), "en"),
      want[[id]][1]
    )
    expect_identical(
      nl_label(nl_note(fit, id, NULL, "fr"), "fr"),
      want[[id]][2]
    )
  }
  d <- .class_fixture_data()
  surv <- survival::Surv(d$time, d$status)
  fits <- list(
    "robust (Lin-Wei)" = survival::coxph(surv ~ x, d, robust = TRUE),
    "cluster-robust (Lin-Wei), clusters by id" = survival::coxph(
      surv ~ x + survival::cluster(id),
      d
    ),
    "cluster-robust (Huber), clusters by id" = survival::survreg(
      surv ~ x,
      d,
      cluster = id
    )
  )
  for (lab in names(fits)) {
    expect_identical(
      nl_label(nl_note(fits[[lab]], "x", NULL, "en"), "en"),
      lab
    )
  }
  # Non-integer weights: survival turns to the robust variance on its own.
  wfit <- survival::coxph(surv ~ x, d, weights = w)
  expect_false(is.null(wfit$naive.var))
  expect_identical(
    nl_label(nl_note(wfit, "x", NULL, "en"), "en"),
    "robust (Lin-Wei)"
  )
  # The same fits without a robust variance stay classical.
  expect_identical(
    nl_label(nl_note(.class_fixture("coxph"), "coxph", NULL, "en"), "en"),
    "classical (Fisher information)"
  )
})

test_that("a tobit keeps cluster() in its formula, and the note names the cluster", {
  skip_if_not_installed("AER")
  skip_if_not_installed("survival")
  set.seed(3)
  d <- data.frame(x = rnorm(120), id = rep(1:30, each = 4))
  d$time <- pmax(0, 1 + 0.5 * d$x + rnorm(120) + rep(rnorm(30), each = 4))
  fit <- AER::tobit(time ~ x + cluster(id), data = d)
  # survival rewrites the call of coxph() and survreg(); tobit() does
  # not, so the cluster is read from the formula.
  expect_null(fit$call$cluster)
  expect_false(is.null(fit$naive.var))
  # The standard errors shown are the robust ones the fit carries.
  tab <- table_regression(
    fit,
    show_columns = c("b", "se"),
    output = "data.frame"
  )
  expect_equal(
    tab$SE[tab$.row_role == "coef"],
    unname(sqrt(diag(stats::vcov(fit)))[c("(Intercept)", "x")])
  )
  expect_false(isTRUE(all.equal(
    tab$SE[tab$.row_role == "coef"],
    sqrt(diag(fit$naive.var))[1:2]
  )))
  expect_identical(
    nl_label(nl_note(fit, "x", NULL, "en"), "en"),
    "cluster-robust (Huber), clusters by id"
  )
  expect_identical(
    nl_label(nl_note(fit, "x", NULL, "fr"), "fr"),
    "robustes aux grappes (Huber), grappes définies par id"
  )
})

test_that("an inference sentence of some models only names them", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  skip_if_not_installed("survey")
  note <- nl_note(
    list(.class_fixture("lmer"), .class_fixture("lm")),
    "lmer",
    NULL,
    "en"
  )
  expect_match(
    note,
    "Model 1: P-values: Wald z-test, large-sample approximation.",
    fixed = TRUE
  )
  note <- nl_note(
    list(.class_fixture("glm_gaussian"), .class_fixture("svyglm")),
    "svyglm",
    NULL,
    "en"
  )
  expect_match(note, "Model 2: Design: ", fixed = TRUE)
  expect_no_match(note, "(^|\\. )Design: ")
})

test_that("the standard-error sentence groups the models of one label", {
  skip_if_not_installed("lme4")
  ss <- lme4::sleepstudy
  models <- list(
    A = lme4::lmer(Reaction ~ Days + (1 | Subject), ss),
    B = lme4::lmer(Reaction ~ Days + (Days | Subject), ss),
    C = lme4::lmer(Reaction ~ 1 + (1 | Subject), ss),
    D = lm(Reaction ~ Days, ss)
  )
  out <- function(lang) {
    withr::local_options(spicy.language = lang)
    gsub(
      "\u00a0",
      " ",
      attr(suppressMessages(table_regression(models)), "note"),
      fixed = TRUE
    )
  }
  expect_match(
    out("en"),
    "Std. errors: A, B, C: classical (model-based); D: classical (OLS).",
    fixed = TRUE
  )
  expect_match(
    out("fr"),
    paste0(
      "Erreurs standards : A, B, C : classiques (fond\u00e9es sur le ",
      "mod\u00e8le) ; D : classiques (MCO)."
    ),
    fixed = TRUE
  )
})

test_that("an lm under a CR vcov names its Satterthwaite df", {
  skip_on_cran()
  fit <- .class_fixture("lm")
  sat <- "P-values: Satterthwaite t-test, cluster-robust df."
  expect_match(nl_note(fit, "lm", "CR2", "en"), sat, fixed = TRUE)
  expect_no_match(nl_note(fit, "lm", "HC3", "en"), sat, fixed = TRUE)
  expect_no_match(nl_note(fit, "lm", "CR1S", "en"), sat, fixed = TRUE)
})

test_that("model names that hold a comma are joined by a semicolon", {
  skip_on_cran()
  fit <- .class_fixture("lm")
  clu <- .CLASS_FIXTURES$lm$cluster
  clu <- if (is.function(clu)) clu(fit) else clu
  out <- function(lang) {
    withr::local_options(spicy.language = lang)
    attr(
      suppressWarnings(table_regression(
        list("A, CR1" = fit, "A, CR1S" = fit, "A, CR2" = fit),
        vcov = list("CR1", "CR1S", "CR2"),
        cluster = list(clu, clu, clu),
        output = "data.frame"
      )),
      "note"
    )
  }
  expect_match(
    out("en"),
    "A, CR1; A, CR2: P-values: Satterthwaite t-test, cluster-robust df.",
    fixed = TRUE
  )
  expect_match(
    out("fr"),
    "A, CR1 ; A, CR2 : Valeurs p :",
    fixed = TRUE
  )
})
