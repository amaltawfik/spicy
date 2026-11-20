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
  rms_Glm_logit = "CR0"
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

test_that("an lm under a CR vcov names its Satterthwaite df", {
  skip_on_cran()
  fit <- .class_fixture("lm")
  sat <- "P-values: Satterthwaite t-test, cluster-robust df."
  expect_match(nl_note(fit, "lm", "CR2", "en"), sat, fixed = TRUE)
  expect_no_match(nl_note(fit, "lm", "HC3", "en"), sat, fixed = TRUE)
  expect_no_match(nl_note(fit, "lm", "CR1S", "en"), sat, fixed = TRUE)
})
