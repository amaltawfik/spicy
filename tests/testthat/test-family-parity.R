# ---------------------------------------------------------------------------
# Cross-family parity + package-level doc-consistency guards.
# Phase 3 matrix (lot T4):
#   * critic:vig-shared-output-grammar -- the four table builders share
#     the reporting grammar the summary-tables-reporting vignette
#     promises (same output vocabulary, same shared formals).
#   * critic:desc-apa-broom-methods -- every summary/regression table
#     class exposes working broom tidy()/glance() methods.
#   * critic:pkgrd-stability-tiers-complete -- every export belongs to
#     exactly one API-stability tier in ?spicy.
# ---------------------------------------------------------------------------

.table_family <- c(
  "table_categorical",
  "table_continuous",
  "table_continuous_lm",
  "table_regression",
  "table_multiresponse"
)

test_that("the table builders share the reporting grammar", {
  # Shared formals promised by the reporting vignette. `digits` is
  # shared by the three numeric-cell functions; the categorical
  # table's cells are percentages, so it spells it `percent_digits`
  # (the vignette says so since lot T4), and so does the
  # multiple-response table, whose cells are counts and percentages.
  shared <- c("labels", "p_digits", "decimal_mark", "align", "output")
  for (fn in .table_family) {
    fo <- names(formals(getExportedValue("spicy", fn)))
    expect_true(
      all(shared %in% fo),
      info = paste0(
        fn,
        " is missing shared formals: ",
        paste(setdiff(shared, fo), collapse = ", ")
      )
    )
  }
  for (fn in c("table_continuous", "table_continuous_lm", "table_regression")) {
    expect_true(
      "digits" %in% names(formals(getExportedValue("spicy", fn))),
      info = fn
    )
  }
  expect_true("percent_digits" %in% names(formals(table_categorical)))
  expect_true("percent_digits" %in% names(formals(table_multiresponse)))

  # One output vocabulary across the whole family.
  vocab <- lapply(.table_family, function(fn) {
    fo <- formals(getExportedValue("spicy", fn))
    eval(fo$output)
  })
  # `seq_along`, not a hardcoded `2:4`: adding a fifth builder to
  # `.table_family` must widen this comparison automatically instead of
  # leaving the newcomer's vocabulary unchecked.
  for (k in seq_along(vocab)[-1L]) {
    expect_setequal(vocab[[k]], vocab[[1L]])
  }
  expect_setequal(
    vocab[[1L]],
    c(
      "default",
      "data.frame",
      "long",
      "tinytable",
      "gt",
      "flextable",
      "excel",
      "clipboard",
      "word"
    )
  )
})

test_that("the shared `labels` formal actually renames a row everywhere", {
  # The grammar test above only checks that `labels` EXISTS in each
  # builder's formals. This one checks it does something: a formal that
  # is accepted and then ignored is worse than one that is missing.
  expect_true(any(grepl(
    "Cylinders",
    unlist(table_categorical(
      mtcars,
      select = "cyl",
      by = "am",
      labels = c(cyl = "Cylinders")
    )),
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Fuel economy",
    unlist(table_continuous(
      mtcars,
      select = "mpg",
      by = "am",
      labels = c(mpg = "Fuel economy")
    )),
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Fuel economy",
    unlist(table_continuous_lm(
      mtcars,
      select = "mpg",
      by = "am",
      labels = c(mpg = "Fuel economy")
    )),
    fixed = TRUE
  )))
  expect_true(
    "Weight" %in%
      table_regression(
        lm(mpg ~ wt, data = mtcars),
        labels = c(wt = "Weight")
      )$Variable
  )
  expect_true(any(grepl(
    "Manual transmission",
    unlist(table_multiresponse(
      mtcars,
      select = c("am", "vs"),
      by = "cyl",
      labels = c(am = "Manual transmission")
    )),
    fixed = TRUE
  )))
})

# Anti-regression witnesses for register 55. Two distinct holes let a
# `labels =` override vanish without an error or a warning:
#   * table_regression() shadowed the user's `labels` with per-model
#     names inside the mixed-fit warning blocks (merMod only);
#   * validate_predictor_labels() called `stats::terms()` unguarded --
#     then guarded, it still saw no terms on the classes whose terms
#     live off the fit (flexsurvreg, brmsfit), until it was routed
#     through `.spicy_get_terms()`, the helper the renderer uses.
# One label per family, asserted on the DISPLAYED Variable column, so
# a future regression in any shared application site reddens here even
# if the per-family files are not run. One test_that() per family: a
# missing Suggests package skips that family alone, not the block.
#
# The cap is lowered so the merMod cases actually TRIP the warning
# block that used to do the shadowing (sleepstudy has n = 180); read
# only on the mixed path, so the other families are unaffected.
parity_fams <- list(
  lm = list(
    pkgs = character(0),
    fit = function() lm(mpg ~ wt, data = mtcars),
    lbl = c(wt = "Weight")
  ),
  glm = list(
    pkgs = character(0),
    fit = function() glm(am ~ wt, data = mtcars, family = binomial),
    lbl = c(wt = "Weight")
  ),
  nls = list(
    pkgs = character(0),
    fit = function() {
      nls(
        conc ~ A * exp(-k * time),
        data = datasets::Indometh,
        start = list(A = 2, k = 0.5)
      )
    },
    lbl = c(k = "Decay rate")
  ),
  coxph = list(
    pkgs = "survival",
    fit = function() {
      survival::coxph(
        survival::Surv(time, status) ~ age,
        data = survival::lung
      )
    },
    lbl = c(age = "Age (years)")
  ),
  polr = list(
    pkgs = "MASS",
    fit = function() {
      MASS::polr(
        Sat ~ Infl + Cont,
        weights = Freq,
        data = MASS::housing,
        Hess = TRUE
      )
    },
    lbl = c(Cont = "Contact")
  ),
  lmer = list(
    pkgs = "lme4",
    fit = function() {
      lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
    },
    lbl = c(Days = "Days of deprivation")
  ),
  # lmerTest returns an lmerModLmerTest, the third class named by the
  # warning-block filter -- witnessed on its own, not assumed from lmer.
  lmerTest = list(
    pkgs = c("lme4", "lmerTest"),
    fit = function() {
      lmerTest::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
    },
    lbl = c(Days = "Days of deprivation")
  )
)
for (nm in names(parity_fams)) {
  test_that(paste0("`labels =` reaches the display for class: ", nm), {
    fam <- parity_fams[[nm]]
    for (pkg in fam$pkgs) {
      skip_if_not_installed(pkg)
    }
    withr::local_options(list(spicy.re_se_max_n = 50L))
    out <- suppressWarnings(table_regression(fam$fit(), labels = fam$lbl))
    # Substring, not equality: a factor key renames the group HEADER
    # ("Contact:"), a numeric key renames a plain row.
    expect_true(
      any(grepl(unname(fam$lbl[1L]), out$Variable, fixed = TRUE)),
      info = paste0("`labels` did not reach the display for class: ", nm)
    )
    # And the raw term name is gone from where the label landed --
    # as a plain row or as a factor header.
    expect_false(
      any(
        c(names(fam$lbl)[1L], paste0(names(fam$lbl)[1L], ":")) %in%
          out$Variable
      ),
      info = paste0("raw term name still displayed for class: ", nm)
    )
  })
}

test_that("every table class exposes working broom tidy()/glance() methods", {
  skip_if_not_installed("broom")
  objs <- list(
    spicy_categorical_table = table_categorical(
      mtcars,
      select = "cyl",
      by = "am"
    ),
    spicy_continuous_table = table_continuous(
      mtcars,
      select = "mpg",
      by = "am"
    ),
    spicy_continuous_lm_table = table_continuous_lm(
      mtcars,
      select = "mpg",
      by = "am"
    ),
    spicy_regression_table = table_regression(lm(mpg ~ wt, data = mtcars)),
    spicy_multiresponse_table = table_multiresponse(
      mtcars,
      select = c("am", "vs"),
      by = "cyl"
    )
  )
  for (cl in names(objs)) {
    expect_s3_class(objs[[cl]], cl)
    td <- broom::tidy(objs[[cl]])
    expect_s3_class(td, "data.frame")
    expect_gt(nrow(td), 0L)
    gl <- broom::glance(objs[[cl]])
    expect_s3_class(gl, "data.frame")
    expect_gt(nrow(gl), 0L)
  }
})

test_that("tidy() and glance() of the descriptive tables name `variable` and `label` alike", {
  skip_if_not_installed("broom")
  # `variable` is the column of `select` a row describes, by its name, and
  # `label` the label the table displays for it: here its `label`
  # attribute, read under `labels = TRUE`. table_outcome() puts its one
  # `outcome` before them.
  d <- sochealth
  shown <- function(v) {
    vapply(
      v,
      function(x) attr(d[[x]], "label", exact = TRUE),
      character(1),
      USE.NAMES = FALSE
    )
  }
  expect_pair <- function(fr, lead, vars, info) {
    expect_identical(names(fr)[seq_along(lead)], lead, info = info)
    expect_identical(unique(fr$variable), vars, info = info)
    expect_identical(fr$label, shown(fr$variable), info = info)
  }
  cats <- c("smoking", "education")
  nums <- c("bmi", "wellbeing_score")
  items <- c(
    "info_doctor",
    "info_pharmacist",
    "info_internet",
    "info_family",
    "info_media"
  )
  tabs <- list(
    table_categorical = list(
      table_categorical(d, c(smoking, education), by = sex, labels = TRUE),
      cats
    ),
    table_continuous = list(
      table_continuous(d, c(bmi, wellbeing_score), by = sex, labels = TRUE),
      nums
    ),
    table_continuous_lm = list(
      table_continuous_lm(d, c(bmi, wellbeing_score), by = sex, labels = TRUE),
      nums
    ),
    table_multiresponse = list(
      table_multiresponse(
        d,
        c(info_doctor, info_pharmacist, info_internet, info_family, info_media),
        by = sex,
        labels = TRUE
      ),
      items
    )
  )
  if (
    requireNamespace("survey", quietly = TRUE) &&
      utils::packageVersion("survey") >= "4.5"
  ) {
    des <- survey::svydesign(ids = ~1, weights = ~weight, data = d)
    tabs$table_categorical_svy <- list(
      table_categorical_svy(
        des,
        c(smoking, education),
        by = sex,
        labels = TRUE
      ),
      cats
    )
    tabs$table_continuous_svy <- list(
      table_continuous_svy(
        des,
        c(bmi, wellbeing_score),
        by = sex,
        labels = TRUE
      ),
      nums
    )
  }
  for (fn in names(tabs)) {
    x <- tabs[[fn]][[1L]]
    vars <- tabs[[fn]][[2L]]
    expect_pair(broom::tidy(x), c("variable", "label"), vars, paste(fn, "tidy"))
    gl <- broom::glance(x)
    if (fn == "table_multiresponse") {
      # Exempt: one row for the whole question, which no single item owns.
      expect_identical(nrow(gl), 1L)
      expect_false(any(c("variable", "label") %in% names(gl)))
    } else {
      expect_pair(gl, c("variable", "label"), vars, paste(fn, "glance"))
    }
  }
  to <- table_outcome(
    d,
    outcome = bmi,
    select = c(sex, education),
    labels = TRUE
  )
  td <- broom::tidy(to)
  gl <- broom::glance(to)
  # The marginal row of tidy() describes the outcome itself.
  lead <- c("outcome", "variable", "label")
  expect_pair(td, lead, c("bmi", "sex", "education"), "table_outcome tidy")
  expect_pair(gl, lead, c("sex", "education"), "table_outcome glance")
  expect_true(all(c(td$outcome, gl$outcome) == "bmi"))
})

# ---- The notes of the descriptive family ---------------------------------
#
# Column names that no label, and no fixed word of a note, can contain;
# every variable carries a label of its own, and a missing value, so each
# kind of sentence that names a variable is written.
.fp_note_data <- function() {
  set.seed(42)
  n <- 240
  d <- data.frame(
    qa1 = round(stats::rnorm(n, 50, 10), 1),
    qb2 = round(stats::rexp(n, 0.1), 1),
    qc3 = factor(sample(c("no", "yes"), n, TRUE)),
    qd4 = factor(sample(c("low", "mid", "high"), n, TRUE)),
    qe5 = factor(sample(c("ctl", "trt"), n, TRUE)),
    qf6 = round(stats::rnorm(n), 2),
    qg7 = factor(sample(c("a", "b", "c"), n, TRUE, prob = c(0.9, 0.07, 0.03))),
    mq1 = sample(c(TRUE, FALSE), n, TRUE),
    mq2 = sample(c(TRUE, FALSE), n, TRUE, prob = c(0.05, 0.95)),
    mq3 = sample(c(TRUE, FALSE), n, TRUE),
    mq4 = FALSE,
    ww = stats::runif(n, 0.5, 2)
  )
  d$qa1[1:4] <- NA
  d$qb2[5:6] <- NA
  d$qc3[7:9] <- NA
  d$qd4[10] <- NA
  d$qe5[11:13] <- NA
  d$qf6[14:15] <- NA
  d$qg7[16] <- NA
  d[17:18, c("mq1", "mq2", "mq3", "mq4")] <- NA
  d
}
.fp_note_labels <- c(
  qa1 = "Alpha score",
  qb2 = "Beta score",
  qc3 = "Gamma flag",
  qd4 = "Delta level",
  qe5 = "Epsilon group",
  qf6 = "Zeta covariate",
  qg7 = "Eta rare",
  mq1 = "Item one",
  mq2 = "Item two",
  mq3 = "Item three",
  mq4 = "Item four"
)
# The note block of a printed table: from the line that opens with
# "Note." to the end.
.fp_note_block <- function(x) {
  out <- utils::capture.output(print(x))
  i <- grep("^Note\\.", out)
  if (length(i) == 0L) {
    return(NULL)
  }
  sub("\n+$", "", paste(out[i[[1L]]:length(out)], collapse = "\n"))
}
# One builder per function, by output: the variables involved (`select`,
# `by`, `outcome`, `covariates`) all labelled, and the options that make
# the notes name them (dropped values, a test list, an adjustment).
.fp_note_builders <- function() {
  d <- .fp_note_data()
  lab <- .fp_note_labels
  q <- function(e) suppressMessages(suppressWarnings(e))
  out <- list(
    table_categorical = function(o) {
      q(table_categorical(
        d,
        select = c(qc3, qd4, qg7),
        by = qe5,
        labels = lab,
        drop_na = TRUE,
        p_adjust = "holm",
        output = o
      ))
    },
    table_continuous = function(o) {
      q(table_continuous(
        d,
        select = c(qa1, qb2),
        by = qe5,
        labels = lab,
        show_columns = list(qa1 = c("m", "sd"), qb2 = c("med", "iqr")),
        smd = TRUE,
        p_adjust = "holm",
        output = o
      ))
    },
    table_continuous_lm = function(o) {
      q(table_continuous_lm(
        d,
        select = c(qa1, qb2),
        by = qe5,
        covariates = c(qf6, qd4),
        vcov = "HC3",
        labels = lab,
        output = o
      ))
    },
    table_outcome = function(o) {
      q(table_outcome(
        d,
        outcome = qa1,
        select = c(qe5, qd4),
        labels = lab,
        drop_na = TRUE,
        p_adjust = "holm",
        output = o
      ))
    },
    table_multiresponse = function(o) {
      q(table_multiresponse(
        d,
        select = c(mq1, mq2, mq3, mq4),
        by = qe5,
        labels = lab,
        output = o
      ))
    }
  )
  if (
    requireNamespace("survey", quietly = TRUE) &&
      utils::packageVersion("survey") >= "4.5"
  ) {
    des <- survey::svydesign(ids = ~1, weights = ~ww, data = d)
    out$table_categorical_svy <- function(o) {
      q(table_categorical_svy(
        des,
        select = c(qc3, qd4),
        by = qe5,
        labels = lab,
        drop_na = TRUE,
        p_adjust = "holm",
        output = o
      ))
    }
    out$table_continuous_svy <- function(o) {
      q(table_continuous_svy(
        des,
        select = c(qa1, qb2),
        by = qe5,
        labels = lab,
        p_adjust = "holm",
        output = o
      ))
    }
  }
  out
}

test_that("every note of the family names a variable as the table does", {
  raw <- names(.fp_note_labels)
  builders <- .fp_note_builders()
  for (fn in names(builders)) {
    x <- builders[[fn]]("default")
    for (txt in list(.fp_note_block(x), attr(x, "note"))) {
      expect_true(!is.null(txt), info = fn)
      hits <- raw[vapply(
        raw,
        function(r) grepl(paste0("\\b", r, "\\b"), txt),
        logical(1)
      )]
      expect_identical(hits, character(0), info = fn)
    }
  }
})

test_that("every table of the family carries its printed note as `note`", {
  builders <- .fp_note_builders()
  for (fn in names(builders)) {
    x <- builders[[fn]]("default")
    block <- .fp_note_block(x)
    expect_true(startsWith(block, "Note. "), info = fn)
    expect_identical(attr(x, "note"), block, info = fn)
    for (o in c("data.frame", "long")) {
      expect_identical(
        attr(builders[[fn]](o), "note"),
        block,
        info = paste(fn, o)
      )
    }
    expect_identical(
      attr(as.data.frame(x), "note"),
      block,
      info = paste(fn, "as.data.frame()")
    )
  }
  # Nothing to say: no note, printed or attached.
  plain <- table_categorical(mtcars, select = cyl)
  expect_null(.fp_note_block(plain))
  expect_null(attr(plain, "note"))
  expect_null(attr(table_continuous(mtcars, select = mpg), "note"))
  # A note the user typed is the note, verbatim; `note = FALSE` removes it.
  d <- .fp_note_data()
  typed <- table_multiresponse(d, c(mq1, mq3), note = "Source: survey.")
  expect_identical(attr(typed, "note"), "Source: survey.")
  expect_true("Source: survey." %in% utils::capture.output(print(typed)))
  expect_null(attr(table_multiresponse(d, c(mq1, mq3), note = FALSE), "note"))
})

test_that("every table of the family opens its note with the lead-in in force", {
  # `spicy_style(note_label = )`, document-wide through the option: the
  # note attribute and the print, which some families rebuild, agree.
  builders <- .fp_note_builders()
  withr::local_options(spicy.style = spicy_style(note_label = "Note:"))
  for (fn in names(builders)) {
    x <- builders[[fn]]("default")
    out <- utils::capture.output(print(x))
    expect_true(startsWith(attr(x, "note"), "Note: "), info = fn)
    expect_true(
      any(startsWith(out, sub("\n.*$", "", attr(x, "note")))),
      info = fn
    )
    expect_false(any(grepl("^Note\\.", out)), info = fn)
  }
  withr::local_options(spicy.style = "jama")
  for (fn in names(builders)) {
    x <- builders[[fn]]("default")
    out <- utils::capture.output(print(x))
    expect_false(startsWith(attr(x, "note"), "Note"), info = fn)
    expect_true(
      any(startsWith(out, sub("\n.*$", "", attr(x, "note")))),
      info = fn
    )
    expect_false(any(grepl("^Note[.:]", out)), info = fn)
  }
})

test_that("every export belongs to exactly one API-stability tier in ?spicy", {
  # Doc-consistency guard: parse the tier lists out of
  # R/spicy-package.R and compare with the actual exports, so a new
  # export cannot ship without a stability promise (the 0.12 gap that
  # left the table_regression family untiered).
  src <- test_path("..", "..", "R", "spicy-package.R")
  skip_if(!file.exists(src), "package sources not available")
  lines <- readLines(src, warn = FALSE, encoding = "UTF-8")
  start <- grep("@section API stability", lines, fixed = TRUE)
  end <- grep("@section broom output shape", lines, fixed = TRUE)
  expect_length(start, 1L)
  expect_length(end, 1L)
  section <- paste(lines[start:end], collapse = "\n")
  tiered <- unique(unlist(regmatches(
    section,
    gregexpr("\\[([A-Za-z_0-9.]+)\\(\\)\\]", section)
  )))
  tiered <- sub("^\\[", "", sub("\\(\\)\\]$", "", tiered))
  exports <- sort(getNamespaceExports("spicy"))
  # S3 methods are registered, not exported by name, so `exports` is
  # already the user-facing function surface.
  untier <- setdiff(exports, tiered)
  expect(
    length(untier) == 0L,
    sprintf(
      "Exported function(s) missing from the ?spicy stability tiers: %s.",
      paste(untier, collapse = ", ")
    )
  )
  # And no tier entry may name a function that is not exported
  # (build_ascii_table was un-exported in this cycle and removed from
  # the Internal tier when this guard was added).
  ghost <- setdiff(tiered, exports)
  expect(
    length(ghost) == 0L,
    sprintf(
      "?spicy stability tiers list non-exported name(s): %s.",
      paste(ghost, collapse = ", ")
    )
  )
})
