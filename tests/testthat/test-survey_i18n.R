# ---------------------------------------------------------------------------
# Stage-2 rehearsal for the survey twins.
#
# Every string a reader of a spicy table sees must come from the
# registry (R/i18n.R). A literal typed at a call site is INVISIBLE at
# the English default -- it renders byte for byte like the key it
# should have used -- and only shows up the day a translation moves the
# key and leaves the literal behind.
#
# So the test moves them all: `spicy_str()` is mocked to return a
# marked string carrying the original's `sprintf` directives and
# nothing else. Anything English left in the rendered table is a
# literal, and the assertions name them one by one.
# ---------------------------------------------------------------------------

.svy_i18n_design <- function() {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  survey::svydesign(
    id = ~dnum,
    weights = ~pw,
    data = apiclus1,
    fpc = ~fpc
  )
}

# A translation of the whole registry into a language of one word.
#
# The `sprintf` directives are kept, in order and with their positional
# form: a template that loses them would abort inside `spicy_fmt()` and
# the test would fail for the wrong reason.
.svy_i18n_fake <- function(prefix) {
  vapply(
    spicy:::.spicy_strings,
    function(v) {
      holes <- regmatches(
        v,
        gregexpr("%[0-9]*[$]?[-+ #0]*[0-9.]*[sdfgeix%]", v)
      )[[1L]]
      holes <- holes[holes != "%%"]
      paste(c(prefix, holes), collapse = " ")
    },
    character(1)
  )
}

# Every English string the twins would print if one were typed at a
# call site instead of read from the registry. Headers, titles, note
# sentences, glosses, test names, the undefined glyph.
.SVY_ENGLISH_LEAKS <- c(
  "Design:",
  "Standard errors:",
  "degrees of freedom",
  "Taylor linearisation",
  "replicate weights",
  "finite population correction",
  "N = ",
  "Descriptive statistics",
  "Categorical table",
  "Quantiles:",
  "Percentage CIs:",
  "design effect",
  "design-based",
  "estimated percentage",
  "observed (unweighted) count",
  "Missing values removed",
  "Rows with missing",
  "Group comparison:",
  "DEff",
  "Weighted n",
  "(Missing)",
  "Total"
)

.svy_i18n_render <- function(expr) {
  paste(capture.output(print(expr)), collapse = "\n")
}

test_that("no display string of the continuous twin is typed outside the registry", {
  d <- .svy_i18n_design()
  # é / ü: the two languages the registry will meet first, and
  # a reminder that a marker is not ASCII-only.
  fake <- .svy_i18n_fake("zézü")
  local_mocked_bindings(.spicy_strings = fake, .package = "spicy")

  out <- .svy_i18n_render(table_continuous_svy(
    d,
    select = api00,
    by = stype,
    show_columns = c("m", "se", "med_iqr", "ci", "n", "weighted_n", "deff"),
    deff = TRUE,
    statistic = TRUE,
    qrule = "spicy"
  ))
  expect_true(nzchar(out))
  # The translation really is in force.
  expect_match(out, "zézü", fixed = TRUE)
  for (leak in .SVY_ENGLISH_LEAKS) {
    expect_false(
      grepl(leak, out, fixed = TRUE),
      info = paste0("English display string survived the translation: ", leak)
    )
  }
})

test_that("no display string of the categorical twin is typed outside the registry", {
  skip_if_not_installed("survey")
  data(api, package = "survey", envir = environment())
  dat <- apiclus1
  dat$stype[1:4] <- NA
  d <- survey::svydesign(id = ~dnum, weights = ~pw, data = dat, fpc = ~fpc)
  fake <- .svy_i18n_fake("zézü")
  local_mocked_bindings(.spicy_strings = fake, .package = "spicy")

  out <- .svy_i18n_render(table_categorical_svy(
    d,
    select = stype,
    by = sch.wide,
    proportion_ci = TRUE,
    deff = TRUE
  ))
  expect_true(nzchar(out))
  expect_match(out, "zézü", fixed = TRUE)
  for (leak in .SVY_ENGLISH_LEAKS) {
    expect_false(
      grepl(leak, out, fixed = TRUE),
      info = paste0("English display string survived the translation: ", leak)
    )
  }
})

test_that("the twins' frozen KEYS do not follow the translation", {
  # The other half of decision 13: the column NAMES of the raw frame
  # and of the typed view are a programmatic contract and must NOT
  # move when the headers do. Same mock, opposite assertion.
  d <- .svy_i18n_design()
  before <- names(
    as_structured(table_categorical_svy(
      d,
      select = stype,
      by = sch.wide,
      proportion_ci = TRUE
    ))$col_meta
  )
  fake <- .svy_i18n_fake("zézü")
  local_mocked_bindings(.spicy_strings = fake, .package = "spicy")
  after <- names(
    as_structured(table_categorical_svy(
      d,
      select = stype,
      by = sch.wide,
      proportion_ci = TRUE
    ))$col_meta
  )
  # Not one key moves. This is the assertion that caught a `%` column
  # key composed from `spicy_str("header_percent_symbol")` instead of
  # the literal the sibling family types: at the English default the
  # two are the same string, and only a translation tells them apart.
  expect_identical(before, after)
  expect_true(all(
    c("No n", "No %", "Yes n", "Yes %", "Total n", "Total %", "p") %in% before
  ))
})

test_that("the continuous twin's frozen keys are English whatever the headers", {
  d <- .svy_i18n_design()
  fake <- .svy_i18n_fake("zézü")
  local_mocked_bindings(.spicy_strings = fake, .package = "spicy")
  s <- as_structured(table_continuous_svy(
    d,
    select = api00,
    show_columns = c("m", "sd", "se", "ci", "n", "deff"),
    deff = TRUE
  ))
  expect_true(all(
    c("M", "SD", "SE", "95% CI LL", "95% CI UL", "n", "DEff") %in%
      names(s$col_meta)
  ))
  # And every one of them carries a TRANSLATED display label.
  labels <- vapply(s$col_meta, function(m) m$display_label, character(1))
  expect_true(all(grepl("zézü", labels, fixed = TRUE)))
})
