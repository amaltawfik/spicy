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

# ---- one mistake, one answer: the same bad input across the builders -----
# The nine table functions answer each mistake with the same class
# (`spicy_missing_column` is a `spicy_invalid_input`), and every error
# they raise names the function the user called, never a helper.

.fv_env <- function(parent) {
  d <- as.data.frame(sochealth)
  env <- new.env(parent = parent)
  env$d <- d
  env$des <- survey::svydesign(ids = ~1, weights = ~weight, data = d)
  env$fit <- stats::lm(bmi ~ age + sex, data = d)
  env$xlsx_bad <- tempfile(fileext = ".csv")
  env$docx_bad <- tempfile(fileext = ".txt")
  env$w_neg <- c(-1, rep(1, nrow(d) - 1L))
  env$d$all_na <- factor(NA, levels = c("a", "b"))
  env$des_na <- survey::svydesign(ids = ~1, weights = ~weight, data = env$d)
  env
}

.fv_catch <- function(cl, env) {
  tryCatch(
    {
      suppressWarnings(utils::capture.output(eval(cl, envir = env)))
      NULL
    },
    error = identity
  )
}

.fv_cases <- list(
  unknown_select = list(
    class = "spicy_missing_column",
    calls = alist(
      table_categorical = table_categorical(d, select = c(smoking, nope)),
      table_categorical_svy = table_categorical_svy(
        des,
        select = c(smoking, nope)
      ),
      table_continuous = table_continuous(d, select = c(bmi, nope)),
      table_continuous_svy = table_continuous_svy(des, select = c(bmi, nope)),
      table_continuous_lm = table_continuous_lm(
        d,
        select = c(bmi, nope),
        by = sex
      ),
      table_outcome = table_outcome(d, bmi, select = c(sex, nope)),
      table_multiresponse = table_multiresponse(
        d,
        select = c(info_doctor, nope)
      ),
      table_regression_uv = table_regression_uv(
        d,
        bmi,
        predictors = c(age, nope)
      )
    )
  ),
  unknown_by = list(
    class = "spicy_missing_column",
    calls = alist(
      table_categorical = table_categorical(d, select = smoking, by = nope),
      table_categorical_svy = table_categorical_svy(
        des,
        select = smoking,
        by = "nope"
      ),
      table_continuous = table_continuous(d, select = bmi, by = nope),
      table_continuous_svy = table_continuous_svy(des, select = bmi, by = nope),
      table_continuous_lm = table_continuous_lm(d, select = bmi, by = "nope"),
      table_outcome = table_outcome(d, nope, select = sex),
      table_multiresponse = table_multiresponse(
        d,
        select = c(info_doctor, info_internet),
        by = nope
      ),
      table_regression_uv = table_regression_uv(d, nope, predictors = age)
    )
  ),
  by_in_select = list(
    class = "spicy_invalid_input",
    calls = alist(
      table_categorical = table_categorical(
        d,
        select = c(smoking, sex),
        by = sex
      ),
      table_categorical_svy = table_categorical_svy(
        des,
        select = c(smoking, sex),
        by = sex
      ),
      table_continuous = table_continuous(d, select = c(bmi, age), by = age),
      table_continuous_svy = table_continuous_svy(
        des,
        select = c("bmi", "age"),
        by = age
      ),
      table_continuous_lm = table_continuous_lm(
        d,
        select = c(bmi, age),
        by = age
      ),
      table_outcome = table_outcome(d, bmi, select = c(sex, bmi)),
      table_multiresponse = table_multiresponse(
        d,
        select = c(info_doctor, info_internet),
        by = info_doctor
      ),
      table_regression_uv = table_regression_uv(
        d,
        bmi,
        predictors = c(age, bmi)
      )
    )
  ),
  empty_selection = list(
    class = "spicy_invalid_input",
    calls = alist(
      table_categorical = table_categorical(
        d,
        select = tidyselect::starts_with("zzz")
      ),
      table_categorical_svy = table_categorical_svy(
        des,
        select = tidyselect::starts_with("zzz")
      ),
      table_continuous = table_continuous(
        d,
        select = tidyselect::starts_with("zzz")
      ),
      table_continuous_svy = table_continuous_svy(
        des,
        select = tidyselect::starts_with("zzz")
      ),
      table_continuous_lm = table_continuous_lm(
        d,
        select = tidyselect::starts_with("zzz"),
        by = sex
      ),
      table_outcome = table_outcome(
        d,
        bmi,
        select = tidyselect::starts_with("zzz")
      ),
      table_multiresponse = table_multiresponse(
        d,
        select = tidyselect::starts_with("zzz")
      ),
      table_regression_uv = table_regression_uv(
        d,
        bmi,
        predictors = tidyselect::starts_with("zzz")
      )
    )
  ),
  fractional_digits = list(
    class = "spicy_invalid_input",
    calls = alist(
      table_categorical = table_categorical(
        d,
        select = smoking,
        percent_digits = 1.5
      ),
      table_categorical_svy = table_categorical_svy(
        des,
        select = smoking,
        percent_digits = 1.5
      ),
      table_continuous = table_continuous(d, select = bmi, digits = 1.5),
      table_continuous_svy = table_continuous_svy(
        des,
        select = bmi,
        digits = 1.5
      ),
      table_continuous_lm = table_continuous_lm(
        d,
        select = bmi,
        by = sex,
        digits = 1.5
      ),
      table_outcome = table_outcome(d, bmi, select = sex, digits = 1.5),
      table_multiresponse = table_multiresponse(
        d,
        select = c(info_doctor, info_internet),
        percent_digits = 1.5
      ),
      table_regression = table_regression(fit, digits = 1.5),
      table_regression_uv = table_regression_uv(
        d,
        bmi,
        predictors = age,
        digits = 1.5
      )
    )
  ),
  logical_effect_size = list(
    class = "spicy_defunct",
    calls = alist(
      table_continuous = table_continuous(
        d,
        select = bmi,
        by = sex,
        effect_size = TRUE
      ),
      table_continuous_lm = table_continuous_lm(
        d,
        select = bmi,
        by = sex,
        effect_size = TRUE
      ),
      table_outcome = table_outcome(d, bmi, select = sex, effect_size = FALSE)
    )
  ),
  bad_excel_path = list(
    class = "spicy_invalid_input",
    calls = alist(
      table_categorical = table_categorical(
        d,
        select = smoking,
        output = "excel",
        excel_path = xlsx_bad
      ),
      table_categorical_svy = table_categorical_svy(
        des,
        select = smoking,
        output = "excel",
        excel_path = xlsx_bad
      ),
      table_continuous = table_continuous(
        d,
        select = bmi,
        output = "excel",
        excel_path = xlsx_bad
      ),
      table_continuous_svy = table_continuous_svy(
        des,
        select = bmi,
        output = "excel",
        excel_path = xlsx_bad
      ),
      table_continuous_lm = table_continuous_lm(
        d,
        select = bmi,
        by = sex,
        output = "excel",
        excel_path = xlsx_bad
      ),
      table_outcome = table_outcome(
        d,
        bmi,
        select = sex,
        output = "excel",
        excel_path = xlsx_bad
      ),
      table_multiresponse = table_multiresponse(
        d,
        select = c(info_doctor, info_internet),
        output = "excel",
        excel_path = xlsx_bad
      ),
      table_regression = table_regression(
        fit,
        output = "excel",
        excel_path = xlsx_bad
      ),
      table_regression_uv = table_regression_uv(
        d,
        bmi,
        predictors = age,
        output = "excel",
        excel_path = 1
      )
    )
  ),
  bad_word_path = list(
    class = "spicy_invalid_input",
    calls = alist(
      table_categorical = table_categorical(
        d,
        select = smoking,
        by = sex,
        output = "word",
        word_path = docx_bad
      ),
      table_categorical_svy = table_categorical_svy(
        des,
        select = smoking,
        output = "word",
        word_path = docx_bad
      ),
      table_continuous = table_continuous(
        d,
        select = bmi,
        output = "word",
        word_path = docx_bad
      ),
      table_continuous_svy = table_continuous_svy(
        des,
        select = bmi,
        output = "word",
        word_path = docx_bad
      ),
      table_continuous_lm = table_continuous_lm(
        d,
        select = bmi,
        by = sex,
        output = "word",
        word_path = docx_bad
      ),
      table_outcome = table_outcome(
        d,
        bmi,
        select = sex,
        output = "word",
        word_path = docx_bad
      ),
      table_multiresponse = table_multiresponse(
        d,
        select = c(info_doctor, info_internet),
        output = "word",
        word_path = docx_bad
      ),
      table_regression = table_regression(
        fit,
        output = "word",
        word_path = docx_bad
      ),
      table_regression_uv = table_regression_uv(
        d,
        bmi,
        predictors = age,
        output = "word",
        word_path = ""
      )
    )
  )
)

# Mistakes whose class is another test's business: here only the header.
.fv_header_calls <- alist(
  table_categorical = table_categorical(as.matrix(d), select = smoking),
  table_categorical = table_categorical(d, select = smoking, style = "nope"),
  table_categorical = table_categorical(d, select = smoking, weights = nope),
  table_categorical = table_categorical(
    d,
    select = smoking,
    by = sex,
    weights = w_neg
  ),
  table_categorical = table_categorical(
    d,
    select = smoking,
    by = sex,
    p_adjust = "nope"
  ),
  table_categorical_svy = table_categorical_svy(d, select = smoking),
  table_continuous = table_continuous(des, select = bmi),
  table_continuous = table_continuous(d, select = bmi, by = c(sex, smoking)),
  table_continuous = table_continuous(
    d,
    select = bmi,
    show_columns = "nope"
  ),
  table_continuous_svy = table_continuous_svy(
    des_na,
    select = bmi,
    by = all_na
  ),
  table_continuous_lm = table_continuous_lm(
    d,
    select = bmi,
    by = sex,
    output = "excel"
  ),
  table_outcome = table_outcome(d, bmi, select = sex, show_columns = "nope"),
  table_multiresponse = table_multiresponse(
    d,
    select = info_doctor,
    by = sex,
    test = "nope"
  ),
  table_regression = table_regression("x"),
  table_regression = table_regression(fit, p_adjust = "nope"),
  table_regression = table_regression(fit, show_columns = "nope"),
  table_regression = table_regression(fit, ci_level = 2),
  table_regression_uv = table_regression_uv(
    d,
    bmi,
    predictors = age,
    align = "left"
  ),
  table_regression_uv = table_regression_uv(
    d,
    bmi,
    predictors = age,
    nope_arg = 1
  ),
  table_regression_uv = table_regression_uv(des, bmi, predictors = age)
)

test_that("the table functions answer one mistake with one class", {
  skip_if_not_installed("survey")
  env <- .fv_env(environment())
  for (case in names(.fv_cases)) {
    spec <- .fv_cases[[case]]
    for (fn in names(spec$calls)) {
      err <- .fv_catch(spec$calls[[fn]], env)
      expect_true(
        inherits(err, spec$class),
        info = sprintf(
          "%s / %s: %s",
          case,
          fn,
          paste(class(err), collapse = " ")
        )
      )
      if (case %in% c("by_in_select", "empty_selection")) {
        # A refusal of its own, not an unknown column.
        expect_false(inherits(err, "spicy_missing_column"), info = fn)
      }
    }
  }
})

test_that("every error of a table function names that function", {
  skip_if_not_installed("survey")
  env <- .fv_env(environment())
  calls <- c(
    unlist(lapply(.fv_cases, `[[`, "calls"), use.names = FALSE),
    unname(.fv_header_calls)
  )
  fns <- c(
    unlist(lapply(.fv_cases, function(s) names(s$calls)), use.names = FALSE),
    names(.fv_header_calls)
  )
  for (k in seq_along(calls)) {
    err <- .fv_catch(calls[[k]], env)
    expect_s3_class(err, "spicy_error")
    header <- rlang::call_name(conditionCall(err))
    expect_identical(
      header,
      fns[[k]],
      info = paste(deparse(calls[[k]]), collapse = " ")
    )
  }
})

test_that("an error of the variance machinery names the public function", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("clubSandwich")
  # The estimators fail deep in R/vcov.R, under a tryCatch() whose handler
  # raises the refusal: without the public call it named that handler's
  # helper, `.abort_vcov_failed()`.
  d <- stats::na.omit(as.data.frame(sochealth)[c("bmi", "age", "sex")])
  d$cl <- rep(1:10, length.out = nrow(d))
  fit <- stats::lm(bmi ~ age + sex, data = d)
  local_mocked_bindings(
    vcovHC = function(...) stop("the estimator failed"),
    .package = "sandwich"
  )
  local_mocked_bindings(
    vcovCR = function(...) stop("the estimator failed"),
    .package = "clubSandwich"
  )
  calls <- alist(
    table_regression = table_regression(fit, vcov = "HC3"),
    table_regression = table_regression(fit, vcov = "CR2", cluster = d$cl),
    table_continuous_lm = table_continuous_lm(
      d,
      select = bmi,
      by = sex,
      vcov = "HC3"
    ),
    table_continuous_lm = table_continuous_lm(
      d,
      select = bmi,
      by = sex,
      vcov = "CR2",
      cluster = cl
    ),
    table_regression_uv = table_regression_uv(
      d,
      bmi,
      predictors = c(age, sex),
      vcov = "HC3"
    )
  )
  for (k in seq_along(calls)) {
    err <- tryCatch(eval(calls[[k]]), error = identity)
    expect_s3_class(err, "spicy_unsupported_vcov")
    expect_identical(
      rlang::call_name(conditionCall(err)),
      names(calls)[[k]],
      info = paste(deparse(calls[[k]]), collapse = " ")
    )
  }
  # A cluster of the wrong length is refused before any fit, in the same
  # name: it named `.check_cluster_length()`.
  err <- tryCatch(
    table_regression(fit, vcov = "CR2", cluster = c(d$cl, 1L)),
    error = identity
  )
  expect_s3_class(err, "spicy_invalid_input")
  expect_identical(rlang::call_name(conditionCall(err)), "table_regression")
})

test_that("a leaf error class is signaled with its parent", {
  skip_if_not_installed("survey")
  env <- .fv_env(environment())
  expect_s3_class(
    .fv_catch(quote(table_continuous(des, select = bmi)), env),
    "spicy_invalid_data"
  )
  expect_s3_class(
    .fv_catch(quote(table_continuous(as.matrix(d), select = bmi)), env),
    "spicy_invalid_data"
  )
  expect_s3_class(
    .fv_catch(quote(table_continuous_svy(d, select = bmi)), env),
    "spicy_invalid_data"
  )
  weighted <- .fv_catch(
    quote(table_continuous(d, select = bmi, by = sex, weights = weight)),
    env
  )
  expect_s3_class(weighted, "spicy_not_implemented")
  expect_s3_class(weighted, "spicy_unsupported")
  unknown <- .fv_catch(quote(table_continuous(d, select = nope)), env)
  expect_s3_class(unknown, "spicy_missing_column")
  expect_s3_class(unknown, "spicy_invalid_input")
})

test_that("a position in `select` counts the columns of the data, `by` included", {
  skip_if_not_installed("survey")
  # As in dplyr: a column's position is the one it has in the data the
  # user passed. Three tables counted it without the `by` column.
  d <- data.frame(
    g = factor(rep(c("a", "b"), 10)),
    x1 = as.numeric(1:20),
    x2 = (1:20)^2,
    f1 = factor(rep(c("u", "v"), each = 10)),
    f2 = factor(rep(c("p", "q", "r", "s"), 5)),
    w = 1
  )
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  vars <- function(x) unique(as.character(x$variable))
  expect_identical(
    vars(table_continuous(d, select = 2:3, by = g, output = "long")),
    c("x1", "x2")
  )
  expect_identical(
    vars(table_continuous_svy(des, select = 2:3, by = g, output = "long")),
    c("x1", "x2")
  )
  expect_identical(
    vars(table_continuous_lm(d, select = 2:3, by = g, output = "long")),
    c("x1", "x2")
  )
  expect_identical(
    vars(table_categorical(d, select = 4:5, by = g, output = "long")),
    c("f1", "f2")
  )
  expect_identical(
    vars(table_categorical_svy(des, select = 4:5, by = g, output = "long")),
    c("f1", "f2")
  )
  # A position is an explicit choice, like a name: one that points at
  # the `by` column is refused with the same error, never skipped.
  for (cl in alist(
    table_continuous(d, select = 1:2, by = g),
    table_continuous_svy(des, select = 1:2, by = g),
    table_continuous_lm(d, select = 1:2, by = g),
    table_categorical(d, select = c(1, 4), by = g),
    table_categorical_svy(des, select = c(1, 4), by = g)
  )) {
    err <- tryCatch(eval(cl), error = identity)
    expect_s3_class(err, "spicy_invalid_input")
    expect_match(
      conditionMessage(err),
      "`select` cannot contain the `by` column (\"g\").",
      fixed = TRUE,
      info = deparse(cl)
    )
  }
})

test_that("the weights and cluster columns are outside the selection", {
  d <- data.frame(
    g = factor(rep(c("a", "b"), 10)),
    y = as.numeric(1:20),
    f = factor(rep(c("u", "v"), each = 10)),
    w = rep(c(1, 2), 10),
    cl = rep(1:5, each = 4)
  )
  vars <- function(x) unique(as.character(x$variable))
  # A helper skips them...
  expect_identical(
    vars(table_continuous(d, weights = w, output = "long")),
    c("y", "cl")
  )
  tab <- suppressWarnings(table_categorical(
    d,
    select = tidyselect::everything(),
    weights = w,
    output = "long"
  ))
  expect_false("w" %in% vars(tab))
  fits <- suppressWarnings(table_continuous_lm(
    d,
    by = g,
    weights = w,
    cluster = cl,
    vcov = "CR2",
    output = "long"
  ))
  expect_identical(vars(fits), "y")
  # ...and naming one, or giving its position, is an error.
  for (cl in alist(
    table_continuous(d, select = c(y, w), weights = w),
    table_categorical(d, select = c(f, w), weights = w),
    table_outcome(d, y, select = c(f, w), weights = w, p_value = FALSE),
    table_continuous_lm(d, select = c(y, w), by = g, weights = w),
    table_continuous_lm(d, select = c(2, 5), by = g, cluster = cl, vcov = "CR2")
  )) {
    err <- tryCatch(eval(cl), error = identity)
    expect_s3_class(err, "spicy_invalid_input")
    expect_match(
      conditionMessage(err),
      "`select` cannot contain the `(weights|cluster)` column",
      info = deparse(cl)
    )
  }
})

test_that("a survey twin selects the columns its data-frame twin selects", {
  skip_if_not_installed("survey")
  skip_if_not_installed("clubSandwich")
  # The design's weights and clusters are outside the selection as the
  # `weights` and `cluster` columns of a data-frame table are, and naming
  # one is the same error.
  d <- data.frame(
    g = factor(rep(c("a", "b"), 20)),
    y = sin(1:40),
    x = cos(1:40),
    f = factor(rep(c("u", "v", "v", "u"), 10)),
    w = 1 + (1:40 %% 3) / 2,
    psu = rep(1:8, each = 5)
  )
  iid <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  clus <- survey::svydesign(ids = ~psu, weights = ~w, data = d)
  vars <- function(x) unique(as.character(x$variable))
  quiet <- function(x) suppressWarnings(x)
  expect_identical(
    vars(quiet(table_continuous_svy(iid, output = "long"))),
    vars(table_continuous(d, weights = w, output = "long"))
  )
  expect_identical(
    vars(quiet(table_categorical_svy(iid, select = !c(y, x), output = "long"))),
    vars(quiet(table_categorical(
      d,
      select = !c(y, x),
      weights = w,
      output = "long"
    )))
  )
  expect_identical(
    vars(quiet(table_continuous_svy(clus, by = g, output = "long"))),
    vars(quiet(table_continuous_lm(
      d,
      by = g,
      weights = w,
      cluster = psu,
      vcov = "CR2",
      output = "long"
    )))
  )
  for (cl in alist(
    table_continuous(d, select = c(y, w), weights = w),
    table_continuous_svy(iid, select = c(y, w)),
    table_categorical(d, select = c(f, w), weights = w),
    table_categorical_svy(iid, select = c(f, w)),
    table_continuous_lm(
      d,
      select = c(y, psu),
      by = g,
      cluster = psu,
      vcov = "CR2"
    ),
    table_continuous_svy(clus, select = c(y, psu), by = g)
  )) {
    err <- tryCatch(eval(cl), error = identity)
    expect_s3_class(err, "spicy_invalid_input")
    expect_match(
      conditionMessage(err),
      "^`select` cannot contain the (`weights`|design's sampling weights|`cluster`|design's cluster) column \\(\"(w|psu)\"\\)\\.",
      info = deparse(cl)
    )
  }
})

test_that("a `by` no observation holds is refused under `drop_na = TRUE`", {
  skip_if_not_installed("survey")
  d <- data.frame(
    y = as.numeric(1:20),
    f = factor(rep(c("u", "v"), 10)),
    q1 = rep(0:1, 10),
    q2 = rep(1:0, 10),
    w = 1
  )
  d$g <- factor(NA, levels = c("a", "b"))
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  for (cl in alist(
    table_continuous(d, select = y, by = g),
    table_continuous_svy(des, select = y, by = g),
    table_categorical(d, select = f, by = g, drop_na = TRUE),
    table_categorical_svy(des, select = f, by = g, drop_na = TRUE),
    table_multiresponse(d, select = c(q1, q2), by = g)
  )) {
    err <- tryCatch(eval(cl), error = identity)
    expect_s3_class(err, "spicy_invalid_data")
    expect_match(
      conditionMessage(err),
      "`by` (`g`) has no observed non-missing level.",
      fixed = TRUE,
      info = deparse(cl)
    )
  }
  # table_outcome() refuses a grouping of `select` the same way.
  expect_error(
    table_outcome(d, y, select = c(f, g), drop_na = TRUE),
    "`select` (`g`) has no observed non-missing level.",
    fixed = TRUE,
    class = "spicy_invalid_data"
  )
  # Under `drop_na = FALSE` the missing values are the one "(Missing)"
  # group, and nothing is refused.
  expect_no_error(suppressWarnings(
    table_continuous(d, select = y, by = g, drop_na = FALSE)
  ))
  expect_no_error(suppressWarnings(table_categorical(d, select = f, by = g)))
  expect_no_error(suppressWarnings(
    table_categorical_svy(des, select = f, by = g)
  ))
  expect_no_error(suppressWarnings(
    table_continuous_svy(des, select = y, by = g, drop_na = FALSE)
  ))
  expect_no_error(suppressWarnings(table_outcome(d, y, select = c(f, g))))
})

test_that("?spicy lists every condition class the package signals", {
  # Doc-consistency guard, as for the stability tiers above: the classes
  # given to spicy_abort() / spicy_warn() / spicy_inform() in R/ must each
  # be listed in the "Classed conditions" section of R/spicy-package.R.
  src_dir <- test_path("..", "..", "R")
  # An installed package has an R/ folder too (the lazy-load database),
  # so test for the source file itself, as covr runs the installed copy.
  skip_if(
    !file.exists(file.path(src_dir, "spicy-package.R")),
    "package sources not available"
  )
  doc <- paste(
    readLines(file.path(src_dir, "spicy-package.R"), encoding = "UTF-8"),
    collapse = "\n"
  )
  signaled <- character(0)
  for (f in list.files(src_dir, "\\.R$", full.names = TRUE)) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    fun <- pd[
      pd$token == "SYMBOL_FUNCTION_CALL" &
        pd$text %in% c("spicy_abort", "spicy_warn", "spicy_inform"),
    ]
    for (i in seq_len(nrow(fun))) {
      call_id <- pd$parent[pd$id == fun$parent[i]]
      kids <- pd[pd$parent == call_id, ]
      kids <- kids[order(kids$line1, kids$col1), ]
      at <- which(kids$token == "SYMBOL_SUB" & kids$text == "class")
      if (length(at) == 0L) {
        next
      }
      value <- kids[at + 2L, ]
      inside <- pd$line1 >= value$line1 &
        pd$line2 <= value$line2 &
        pd$token == "STR_CONST"
      inside <- inside &
        (pd$line1 > value$line1 | pd$col1 >= value$col1) &
        (pd$line2 < value$line2 | pd$col2 <= value$col2)
      signaled <- c(signaled, gsub("\"", "", pd$text[inside]))
    }
  }
  signaled <- unique(signaled[startsWith(signaled, "spicy_")])
  expect_true(length(signaled) > 20L)
  undocumented <- signaled[
    !vapply(
      signaled,
      function(cl) grepl(paste0("\\code{", cl, "}"), doc, fixed = TRUE),
      logical(1)
    )
  ]
  expect(
    length(undocumented) == 0L,
    sprintf(
      "Condition class(es) missing from ?spicy: %s.",
      paste(undocumented, collapse = ", ")
    )
  )
})

# ---- The machine outputs of the family -------------------------------------
#
# Decisions 1 and 5 to 9 of the ADR of 2026-10-20: one vocabulary in
# `output = "long"`, tidy() and glance(); `output = "data.frame"` is the
# displayed table and `as.data.frame()` returns it; proportions between 0
# and 1; the Total rows kept and flagged; the file outputs return their
# path; one parent class. One builder per function of the family, `by`
# and the options that bring out the optional columns.
.fo_builders <- function() {
  d <- sochealth
  q <- function(e) suppressMessages(suppressWarnings(e))
  out <- list(
    table_categorical = function(o = "default", ...) {
      q(table_categorical(
        d,
        select = c(smoking, education),
        by = sex,
        labels = TRUE,
        assoc_ci = TRUE,
        smd = TRUE,
        p_adjust = "holm",
        output = o,
        ...
      ))
    },
    table_continuous = function(o = "default", ...) {
      q(table_continuous(
        d,
        select = c(bmi, wellbeing_score),
        by = sex,
        labels = TRUE,
        statistic = TRUE,
        effect_size = "auto",
        effect_size_ci = TRUE,
        smd = TRUE,
        p_adjust = "holm",
        output = o,
        ...
      ))
    },
    table_continuous_lm = function(o = "default", ...) {
      q(table_continuous_lm(
        d,
        select = c(bmi, wellbeing_score),
        by = education,
        weights = weight,
        statistic = TRUE,
        effect_size = "omega2",
        effect_size_ci = TRUE,
        show_weighted_n = TRUE,
        labels = TRUE,
        output = o,
        ...
      ))
    },
    table_outcome = function(o = "default", ...) {
      q(table_outcome(
        d,
        outcome = bmi,
        select = c(sex, smoking),
        labels = TRUE,
        statistic = TRUE,
        effect_size = "auto",
        effect_size_ci = TRUE,
        p_adjust = "holm",
        output = o,
        ...
      ))
    },
    table_multiresponse = function(o = "default", ...) {
      q(table_multiresponse(
        d,
        select = tidyselect::starts_with("info_"),
        by = sex,
        statistic = TRUE,
        labels = TRUE,
        output = o,
        ...
      ))
    },
    table_regression = function(o = "default", ...) {
      q(table_regression(
        lm(bmi ~ age + sex + education, data = d),
        labels = TRUE,
        output = o,
        ...
      ))
    },
    table_regression_uv = function(o = "default", ...) {
      q(table_regression_uv(
        d,
        outcome = bmi,
        predictors = c(age, sex),
        labels = TRUE,
        output = o,
        ...
      ))
    }
  )
  if (
    requireNamespace("survey", quietly = TRUE) &&
      utils::packageVersion("survey") >= "4.5"
  ) {
    des <- survey::svydesign(ids = ~1, weights = ~weight, data = d)
    out$table_categorical_svy <- function(o = "default", ...) {
      q(table_categorical_svy(
        des,
        select = c(smoking, education),
        by = sex,
        labels = TRUE,
        proportion_ci = TRUE,
        deff = TRUE,
        p_adjust = "holm",
        output = o,
        ...
      ))
    }
    out$table_continuous_svy <- function(o = "default", ...) {
      q(table_continuous_svy(
        des,
        select = c(bmi, wellbeing_score),
        by = sex,
        labels = TRUE,
        statistic = TRUE,
        deff = TRUE,
        p_adjust = "holm",
        output = o,
        ...
      ))
    }
  }
  out
}

# The machine frames of one builder: the long output, tidy() and glance().
.fo_machine_frames <- function(b) {
  x <- b()
  list(
    long = b("long"),
    tidy = broom::tidy(x),
    glance = broom::glance(x)
  )
}

test_that("the machine outputs of the family speak one vocabulary", {
  skip_if_not_installed("broom")
  # Each concept has ONE name: broom's where broom has one, one snake_case
  # name otherwise. The other spellings the family once used are refused
  # everywhere.
  vocabulary <- list(
    p.value = c("p", "pvalue", "p_value"),
    p_unadjusted = "p.unadjusted",
    statistic = c("chi2", "Chi2", "stat"),
    df = "df1",
    df.residual = "df2",
    test_type = "statistic_type",
    es_value = c("effect_size", "assoc_value"),
    es_type = c("effect_size_type", "assoc_type"),
    es_ci_lower = "assoc_ci_lower",
    es_ci_upper = "assoc_ci_upper",
    smd_value = "smd",
    weighted_n = "weighted.n",
    weighted_nobs = "weighted.nobs",
    nobs = c("n_total", "n_obs"),
    r.squared = "r2",
    adj.r.squared = "adj_r2"
  )
  refused <- unlist(vocabulary, use.names = FALSE)
  builders <- .fo_builders()
  for (fn in names(builders)) {
    frames <- .fo_machine_frames(builders[[fn]])
    for (k in names(frames)) {
      bad <- intersect(names(frames[[k]]), refused)
      expect_identical(bad, character(0), info = paste(fn, k))
    }
  }
  # The test, when a table carries one, is named alike everywhere.
  for (fn in setdiff(names(builders), "table_multiresponse")) {
    gl <- broom::glance(builders[[fn]]())
    expect_true(
      all(c("test_type", "p.value") %in% names(gl)) ||
        fn %in% c("table_regression", "table_regression_uv"),
      info = fn
    )
  }
  # Counts of groups and of levels: `n_groups` counts the groups of `by`,
  # `n_levels` the categories of a variable, never the reverse.
  if (!is.null(builders$table_categorical_svy)) {
    gl <- broom::glance(builders$table_categorical_svy())
    expect_true("n_levels" %in% names(gl))
    expect_false("n_groups" %in% names(gl))
    gl <- broom::glance(builders$table_continuous_svy())
    expect_true("n_groups" %in% names(gl))
    expect_false("n_levels" %in% names(gl))
  }
  gl <- broom::glance(builders$table_outcome())
  expect_true("n_levels" %in% names(gl))
  expect_false("n_groups" %in% names(gl))
  # So does the attribute of table_outcome() that counts the levels of
  # each `select` variable its test compares.
  x <- builders$table_outcome()
  expect_identical(attr(x, "n_levels"), c(sex = 2L, smoking = 2L))
  expect_null(attr(x, "n_groups"))
  # The attributes name the `by` column and its label alike.
  by_of <- c(
    table_categorical = "sex",
    table_categorical_svy = "sex",
    table_continuous = "sex",
    table_continuous_svy = "sex",
    table_continuous_lm = "education",
    table_multiresponse = "sex"
  )
  for (fn in intersect(names(by_of), names(builders))) {
    x <- builders[[fn]]()
    expect_identical(attr(x, "group_var"), by_of[[fn]], info = fn)
    expect_false(is.null(attr(x, "group_label")), info = fn)
    expect_false(
      any(c("by_var", "by_label") %in% names(attributes(x))),
      info = fn
    )
  }
})

test_that("the degrees of freedom follow the statistic", {
  skip_if_not_installed("broom")
  # A t (Welch's, Student's, the design-based one, the contrast or the
  # slope of a linear model) carries its own degrees of freedom in `df`
  # and never a `df.residual`; an F carries both.
  is_t <- function(tt) !is.na(tt) & grepl("(^|_)t$", tt)
  is_f <- function(tt) {
    !is.na(tt) & (tt %in% c("F", "anova", "welch_anova", "design_f"))
  }
  q <- function(e) suppressMessages(suppressWarnings(e))
  lm_by <- function(by, o = "default") {
    q(table_continuous_lm(
      sochealth,
      select = bmi,
      by = !!rlang::sym(by),
      statistic = TRUE,
      output = o
    ))
  }
  builders <- .fo_builders()
  frames <- list()
  for (fn in names(builders)) {
    frames[[paste(fn, "long")]] <- builders[[fn]]("long")
    frames[[paste(fn, "glance")]] <- broom::glance(builders[[fn]]())
  }
  for (b in c("sex", "age")) {
    frames[[paste("table_continuous_lm", b, "long")]] <- lm_by(b, "long")
    frames[[paste("table_continuous_lm", b, "glance")]] <- broom::glance(
      lm_by(b)
    )
  }
  n_t <- 0L
  n_f <- 0L
  for (k in names(frames)) {
    fr <- frames[[k]]
    if (!all(c("test_type", "df", "df.residual") %in% names(fr))) {
      next
    }
    t_rows <- is_t(fr$test_type)
    f_rows <- is_f(fr$test_type)
    n_t <- n_t + sum(t_rows)
    n_f <- n_f + sum(f_rows)
    expect_true(all(is.na(fr$df.residual[t_rows])), info = k)
    expect_true(all(fr$df[t_rows] > 1), info = k)
    expect_false(
      anyNA(fr$df[f_rows]) || anyNA(fr$df.residual[f_rows]),
      info = k
    )
  }
  # Both kinds were met, so the assertions above are not vacuous.
  expect_gt(n_t, 0L)
  expect_gt(n_f, 0L)
  # The t of a linear model reports the residual df of the model, which
  # its printed header names as before.
  lg <- lm_by("sex", "long")
  expect_identical(lg$df[lg$test_type %in% "t"], 1186)
  expect_match(
    paste(capture.output(print(lm_by("sex"))), collapse = "\n"),
    "t(1186)",
    fixed = TRUE
  )
})

test_that("tidy() of a descriptive table always carries `group`", {
  skip_if_not_installed("broom")
  d <- sochealth
  tabs <- list(
    table_categorical(d, select = smoking),
    table_continuous(d, select = bmi),
    table_multiresponse(d, select = tidyselect::starts_with("info_"))
  )
  if (
    requireNamespace("survey", quietly = TRUE) &&
      utils::packageVersion("survey") >= "4.5"
  ) {
    des <- survey::svydesign(ids = ~1, weights = ~weight, data = d)
    tabs <- c(
      tabs,
      list(
        table_categorical_svy(des, select = smoking),
        table_continuous_svy(des, select = bmi)
      )
    )
  }
  for (x in tabs) {
    td <- broom::tidy(suppressWarnings(x))
    expect_true("group" %in% names(td), info = class(x)[[1L]])
    expect_true(all(is.na(td$group)), info = class(x)[[1L]])
  }
})

test_that("`output = \"data.frame\"` is the displayed table, as.data.frame() of the object", {
  builders <- .fo_builders()
  keys <- c("variable", "label", "level", "group", ".row_role")
  for (fn in names(builders)) {
    x <- builders[[fn]]()
    df <- builders[[fn]]("data.frame")
    expect_identical(as.data.frame(x), df, info = fn)
    expect_identical(
      tibble::as_tibble(x),
      tibble::as_tibble(df),
      info = paste(fn, "as_tibble()")
    )
    # The key columns first, then the printed columns, numbers throughout.
    expect_identical(names(df)[1:2], c("variable", "label"), info = fn)
    vals <- setdiff(names(df), keys)
    expect_true(
      all(vapply(df[vals], is.numeric, logical(1))),
      info = paste(fn, "numeric cells")
    )
    # One row per printed row: the typed view the engines render.
    expect_identical(nrow(df), nrow(as_structured(x)$body), info = fn)
    # The note is the one attribute of the family's data frames (the
    # global test of a multiple-response table aside).
    extra <- setdiff(
      names(attributes(df)),
      c("names", "row.names", "class", "note", "global_test")
    )
    expect_identical(extra, character(0), info = fn)
    expect_identical(attr(df, "note"), attr(x, "note"), info = fn)
    # The long output carries the same note.
    expect_identical(
      attr(builders[[fn]]("long"), "note"),
      attr(x, "note"),
      info = fn
    )
  }
})

test_that("tidy() reports proportions between 0 and 1", {
  skip_if_not_installed("broom")
  builders <- .fo_builders()
  for (fn in intersect(
    names(builders),
    c("table_categorical", "table_categorical_svy", "table_multiresponse")
  )) {
    td <- broom::tidy(builders[[fn]]())
    # A table with two bases names each proportion by its base.
    props <- if (identical(fn, "table_multiresponse")) {
      c("proportion_cases", "proportion_responses")
    } else {
      "proportion"
    }
    expect_true(all(props %in% names(td)), info = fn)
    for (nm in intersect(c(props, "conf.low", "conf.high"), names(td))) {
      v <- td[[nm]][!is.na(td[[nm]])]
      expect_true(length(v) > 0L, info = paste(fn, nm))
      expect_true(all(v >= 0 & v <= 1), info = paste(fn, nm))
    }
  }
})

test_that("tidy() keeps the Total rows and flags them", {
  skip_if_not_installed("broom")
  builders <- .fo_builders()
  for (fn in intersect(
    names(builders),
    c("table_categorical", "table_categorical_svy", "table_multiresponse")
  )) {
    x <- builders[[fn]]()
    td <- broom::tidy(x)
    expect_type(td$total, "logical")
    # One margin among the three blocks (Female, Male, Total).
    expect_identical(sum(td$total), nrow(td) %/% 3L, info = fn)
    expect_true(all(td$group[td$total] == "Total"), info = fn)
    # The long output flags the same rows.
    lg <- builders[[fn]]("long")
    expect_identical(lg$group[lg$total], rep("Total", sum(lg$total)), info = fn)
  }
  # Without `include_total`, no row is the margin.
  td <- broom::tidy(table_categorical(
    sochealth,
    select = smoking,
    by = sex,
    include_total = FALSE
  ))
  expect_false(any(td$total))
})

test_that("the missing category is keyed, never recognized by its label", {
  skip_if_not_installed("broom")
  withr::local_options(spicy.language = "fr")
  d <- sochealth
  missing_label <- spicy_str("row_missing_level")
  expect_false(identical(missing_label, "(Missing)"))
  x <- table_categorical(d, select = smoking, by = sex)
  for (fr in list(
    table_categorical(d, select = smoking, by = sex, output = "long"),
    broom::tidy(x)
  )) {
    expect_identical(unique(fr$level[fr$.row_role == "missing"]), missing_label)
    expect_false(any(fr$level[fr$.row_role == "level"] == missing_label))
  }
  # The group of missing `by` values: `.group_role`, the role of `group`,
  # in the long output and tidy() of every table with a `group` key.
  frames <- list(
    categorical_long = table_categorical(
      d,
      select = sex,
      by = smoking,
      drop_na = FALSE,
      output = "long"
    ),
    categorical_tidy = broom::tidy(table_categorical(
      d,
      select = sex,
      by = smoking,
      drop_na = FALSE
    )),
    continuous_long = suppressWarnings(table_continuous(
      d,
      select = bmi,
      by = smoking,
      drop_na = FALSE,
      output = "long"
    )),
    continuous_tidy = broom::tidy(suppressWarnings(table_continuous(
      d,
      select = bmi,
      by = smoking,
      drop_na = FALSE
    )))
  )
  if (
    requireNamespace("survey", quietly = TRUE) &&
      utils::packageVersion("survey") >= "4.5"
  ) {
    des <- survey::svydesign(ids = ~1, weights = ~weight, data = d)
    svy_cat <- function(o = "default") {
      suppressWarnings(table_categorical_svy(
        des,
        select = sex,
        by = smoking,
        drop_na = FALSE,
        output = o
      ))
    }
    svy_con <- function(o = "default") {
      suppressWarnings(table_continuous_svy(
        des,
        select = bmi,
        by = smoking,
        drop_na = FALSE,
        output = o
      ))
    }
    frames$categorical_svy_long <- svy_cat("long")
    frames$categorical_svy_tidy <- broom::tidy(svy_cat())
    frames$continuous_svy_long <- svy_con("long")
    frames$continuous_svy_tidy <- broom::tidy(svy_con())
  }
  for (k in names(frames)) {
    fr <- frames[[k]]
    expect_identical(
      unique(fr$group[fr$.group_role == "missing"]),
      missing_label,
      info = k
    )
    expect_false(
      any(fr$group[fr$.group_role != "missing"] == missing_label),
      info = k
    )
  }
  # A continuous table has no `level`, so no `.row_role`: the role of its
  # rows is that of their group.
  lg <- frames$continuous_long
  expect_false(".row_role" %in% names(lg))
  expect_identical(
    lg$.group_role[lg$group != missing_label],
    c("group", "group")
  )
})

test_that("each role column follows the key it describes", {
  skip_if_not_installed("broom")
  builders <- .fo_builders()
  roles <- c("group", "missing", "total")
  for (fn in setdiff(
    names(builders),
    c("table_regression", "table_regression_uv")
  )) {
    x <- builders[[fn]]()
    for (k in c("long", "tidy")) {
      fr <- if (identical(k, "long")) builders[[fn]]("long") else broom::tidy(x)
      nm <- names(fr)
      info <- paste(fn, k)
      if ("group" %in% nm) {
        # `.group_role` right after `group`, `NA` where `group` is.
        expect_identical(
          nm[match("group", nm) + 1L],
          ".group_role",
          info = info
        )
        has <- !is.na(fr$group)
        expect_true(all(fr$.group_role[has] %in% roles), info = info)
        expect_true(all(is.na(fr$.group_role[!has])), info = info)
        if ("total" %in% nm) {
          expect_identical(
            fr$.group_role[has] == "total",
            fr$total[has],
            info = info
          )
        }
      } else {
        expect_false(".group_role" %in% nm, info = info)
      }
      # `.row_role` right after the `level` it describes. The `level` of
      # table_continuous_lm() is a level of `by`, and that table keeps no
      # role column.
      if ("level" %in% nm && !identical(fn, "table_continuous_lm")) {
        expect_identical(nm[match("level", nm) + 1L], ".row_role", info = info)
      }
      if (fn %in% c("table_continuous", "table_continuous_svy")) {
        expect_false(".row_role" %in% nm, info = info)
      }
    }
  }
})

test_that("a multiple-response table names each share by its base", {
  skip_if_not_installed("broom")
  mr <- function(...) {
    suppressWarnings(table_multiresponse(
      sochealth,
      select = tidyselect::starts_with("info_"),
      by = sex,
      ...
    ))
  }
  shares <- function(fr, stem) grep(paste0("^", stem), names(fr), value = TRUE)
  # The long output: the percentage `percent` displays, under the name of
  # its base.
  expect_identical(shares(mr(output = "long"), "pct"), "pct_cases")
  expect_identical(
    shares(mr(percent = "responses", output = "long"), "pct"),
    "pct_responses"
  )
  expect_identical(
    shares(mr(percent = "both", output = "long"), "pct"),
    c("pct_cases", "pct_responses")
  )
  # tidy(): both proportions, whatever `percent` displays.
  for (p in c("cases", "responses", "both")) {
    expect_identical(
      shares(broom::tidy(mr(percent = p)), "proportion"),
      c("proportion_cases", "proportion_responses"),
      info = p
    )
  }
  # The same numbers, on the two scales.
  lg <- mr(percent = "both", output = "long")
  lg <- lg[lg$.row_role == "summary", ]
  td <- broom::tidy(mr(percent = "both"))
  expect_equal(lg$pct_cases / 100, td$proportion_cases)
  expect_equal(lg$pct_responses / 100, td$proportion_responses)
})

test_that("every file output of the family returns its path, invisibly", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("flextable")
  skip_if_not_installed("officer")
  builders <- .fo_builders()
  dir <- withr::local_tempdir()
  for (fn in names(builders)) {
    xl <- file.path(dir, paste0(fn, ".xlsx"))
    res <- withVisible(builders[[fn]]("excel", excel_path = xl))
    expect_false(res$visible, info = paste(fn, "excel"))
    expect_identical(res$value, xl, info = paste(fn, "excel"))
    expect_true(file.exists(xl), info = paste(fn, "excel"))
    wd <- file.path(dir, paste0(fn, ".docx"))
    res <- withVisible(builders[[fn]]("word", word_path = wd))
    expect_false(res$visible, info = paste(fn, "word"))
    expect_identical(res$value, wd, info = paste(fn, "word"))
    expect_true(file.exists(wd), info = paste(fn, "word"))
  }
})

test_that("every table of the family inherits `spicy_table`", {
  builders <- .fo_builders()
  for (fn in names(builders)) {
    x <- builders[[fn]]()
    expect_true(inherits(x, "spicy_table"), info = fn)
    expect_identical(class(x)[[2L]], "spicy_table", info = fn)
    expect_identical(class(x)[[3L]], "data.frame", info = fn)
  }
})

# ---- The arguments and addresses of the family -----------------------------
#
# Decisions 2, 3, 4, 10 and 12 of the ADR of 2026-10-20, and the rule for
# an argument with no effect (V12 of the coherence audit): one test per
# rule, so that no sibling drifts from it again.

test_that("one spelling per effect-size measure across the family", {
  # A squared measure ends in 2 (`eta2`, `epsilon2`, `omega2`, `f2`), an
  # estimator named after its author carries the name (`cohens_d`,
  # `hedges_g`), in the argument and in the `es_type` of the outputs.
  allowed <- c(
    "none",
    "auto",
    "hedges_g",
    "cohens_d",
    "r_rb",
    "eta2",
    "epsilon2",
    "omega2",
    "f2"
  )
  for (fn in c("table_continuous", "table_outcome", "table_continuous_lm")) {
    vals <- eval(formals(get(fn, envir = asNamespace("spicy")))$effect_size)
    expect_true(all(vals %in% allowed), info = fn)
  }
  d <- sochealth
  q <- function(e) suppressMessages(suppressWarnings(e))
  es <- c(
    q(table_continuous(
      d,
      select = bmi,
      by = education,
      effect_size = "eta2",
      output = "long"
    ))$es_type,
    q(table_continuous(
      d,
      select = bmi,
      by = education,
      test = "nonparametric",
      effect_size = "epsilon2",
      output = "long"
    ))$es_type,
    q(table_outcome(
      d,
      bmi,
      select = c(sex, education),
      effect_size = "auto",
      output = "long"
    ))$es_type,
    q(table_continuous_lm(
      d,
      select = bmi,
      by = sex,
      effect_size = "cohens_d",
      output = "long"
    ))$es_type,
    q(table_continuous_lm(
      d,
      select = bmi,
      by = sex,
      effect_size = "hedges_g",
      output = "long"
    ))$es_type
  )
  es <- unique(stats::na.omit(es))
  expect_true(all(es %in% allowed))
  expect_setequal(es, c("eta2", "epsilon2", "hedges_g", "cohens_d"))

  # Every spelling of 0.13.0 is refused, and the message gives the new one.
  old <- list(
    list(fn = "table_continuous", old = "eta_sq", new = "eta2"),
    list(fn = "table_continuous", old = "epsilon_sq", new = "epsilon2"),
    list(fn = "table_outcome", old = "eta_sq", new = "eta2"),
    list(fn = "table_outcome", old = "epsilon_sq", new = "epsilon2"),
    list(fn = "table_continuous_lm", old = "d", new = "cohens_d"),
    list(fn = "table_continuous_lm", old = "g", new = "hedges_g")
  )
  call_with <- function(fn, value) {
    switch(
      fn,
      table_continuous = table_continuous(
        d,
        select = bmi,
        by = sex,
        effect_size = value
      ),
      table_outcome = table_outcome(d, bmi, select = sex, effect_size = value),
      table_continuous_lm = table_continuous_lm(
        d,
        select = bmi,
        by = sex,
        effect_size = value
      )
    )
  }
  for (o in old) {
    err <- expect_error(
      call_with(o$fn, o$old),
      class = "spicy_defunct",
      info = paste(o$fn, o$old)
    )
    expect_s3_class(err, "spicy_invalid_input")
    expect_match(
      conditionMessage(err),
      sprintf("effect_size = \"%s\"", o$new),
      fixed = TRUE,
      info = paste(o$fn, o$old)
    )
  }
})

test_that("`v_digits` is `effect_size_digits` in table_categorical() and spicy_style()", {
  d <- sochealth
  err <- expect_error(
    table_categorical(d, select = smoking, by = sex, v_digits = 3),
    class = "spicy_defunct"
  )
  expect_match(conditionMessage(err), "effect_size_digits = 3", fixed = TRUE)
  err <- expect_error(spicy_style(v_digits = 3), class = "spicy_defunct")
  expect_match(conditionMessage(err), "effect_size_digits = 3", fixed = TRUE)
  # The lever of a style reaches the association measure and the SMD.
  s <- as_structured(suppressWarnings(table_categorical(
    d,
    select = smoking,
    by = sex,
    smd = TRUE,
    style = spicy_style(effect_size_digits = 4)
  )))
  expect_identical(s$col_meta[["SMD"]]$precision, 4L)
  expect_identical(s$format_spec$effect_size_digits, 4L)
})

# A character `by` with capitals and an accented letter, a logical one,
# and a factor whose level order is not the alphabetical one, the rows
# given in two orders.
.fg_data <- function() {
  set.seed(11)
  g <- rep(c("b", "B", "a", "\u00e9", "A", "e"), each = 10)
  data.frame(
    g = g,
    l = rep(c(TRUE, FALSE), 30),
    f = factor(rep(c("z", "a", "m"), 20), levels = c("z", "a", "m")),
    y = stats::rnorm(60),
    s = factor(rep(c("no", "yes"), 30)),
    i1 = rep(c(TRUE, FALSE, TRUE), 20),
    i2 = rep(c(FALSE, TRUE), 30),
    w = 1,
    stringsAsFactors = FALSE
  )
}

# The groups each builder displays for the grouping column `by` (a column
# name), in their order; the groupings of `table_outcome()` are its rows.
.fg_groups <- function(d, by) {
  q <- function(e) suppressMessages(suppressWarnings(e))
  grp <- function(x) setdiff(unique(as.character(stats::na.omit(x))), "Total")
  out <- list(
    table_categorical = grp(
      q(table_categorical(d, select = s, by = by, output = "long"))$group
    ),
    table_continuous = grp(
      q(table_continuous(d, select = y, by = by, output = "long"))$group
    ),
    table_continuous_lm = grp(
      q(table_continuous_lm(d, select = y, by = by, output = "long"))$level
    ),
    table_multiresponse = grp(
      q(table_multiresponse(
        d,
        select = c(i1, i2),
        by = by,
        output = "long"
      ))$group
    ),
    table_outcome = grp(
      q(table_outcome(d, y, select = by, output = "long"))$level
    )
  )
  if (
    requireNamespace("survey", quietly = TRUE) &&
      utils::packageVersion("survey") >= "4.5"
  ) {
    des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
    out$table_categorical_svy <- grp(
      q(table_categorical_svy(des, select = s, by = by, output = "long"))$group
    )
    out$table_continuous_svy <- grp(
      q(table_continuous_svy(des, select = y, by = by, output = "long"))$group
    )
  }
  out
}

test_that("every grouping table orders its groups one way", {
  # Decision 4: a character or logical grouping is sorted in the C locale
  # (capitals before lower case, accented letters after `z`, the same on
  # every machine), FALSE before TRUE; a factor keeps its levels. The
  # order of the rows does not matter.
  d <- .fg_data()
  shuffled <- d[rev(seq_len(nrow(d))), ]
  expected <- list(
    g = c("A", "B", "a", "b", "e", "\u00e9"),
    l = c("FALSE", "TRUE"),
    f = c("z", "a", "m")
  )
  for (by in names(expected)) {
    got <- .fg_groups(d, by)
    got_shuffled <- .fg_groups(shuffled, by)
    for (fn in names(got)) {
      expect_identical(got[[fn]], expected[[by]], info = paste(fn, by))
      expect_identical(got_shuffled[[fn]], expected[[by]], info = paste(fn, by))
    }
  }
})

test_that("the levels of a character variable follow the order of the groups", {
  # The same rule applied to rows: the levels of a character `select`
  # variable, as `cross_tab()` orders them.
  d <- .fg_data()
  q <- function(e) suppressMessages(suppressWarnings(e))
  expected <- c("A", "B", "a", "b", "e", "\u00e9")
  lv <- unique(q(table_categorical(d, select = g, output = "long"))$level)
  expect_identical(lv, expected)
  ct <- q(cross_tab(d, g, s, output = "data.frame"))
  expect_identical(as.character(ct[[1L]][seq_along(expected)]), expected)
  skip_if_not_installed("survey", "4.5")
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  lv_svy <- unique(
    q(table_categorical_svy(des, select = g, output = "long"))$level
  )
  expect_identical(lv_svy, expected)
})

test_that("`model` selects a group of `by` in every table that has groups", {
  # Decision 10: `model` names a group whether the table sets the groups
  # out as columns (spanners, the per-group columns of
  # table_continuous_lm()) or as rows (the continuous pair); `level` names
  # a category shown as a row, and naming a group with it is refused.
  d <- as.data.frame(sochealth)
  q <- function(e) suppressMessages(suppressWarnings(e))
  fem <- !is.na(d$sex) & d$sex == "Female"
  ct <- q(table_categorical(d, select = smoking, by = sex))
  expect_identical(
    inline(ct, smoking, "Yes", "n", model = "Female"),
    as.character(sum(fem & d$smoking %in% "Yes"))
  )
  mr <- q(table_multiresponse(
    d,
    select = tidyselect::starts_with("info_"),
    by = sex
  ))
  expect_identical(
    inline(mr, info_doctor, column = "n", model = "Female"),
    as.character(sum(fem & d$info_doctor %in% TRUE))
  )
  m_fem <- formatC(mean(d$bmi[fem], na.rm = TRUE), format = "f", digits = 2)
  cn <- q(table_continuous(d, select = bmi, by = sex))
  expect_identical(inline(cn, bmi, model = "Female"), m_fem)
  lm_tbl <- q(table_continuous_lm(d, select = bmi, by = sex))
  expect_identical(
    inline(lm_tbl, bmi, column = "emmean", model = "Female"),
    m_fem
  )
  for (x in list(cn, lm_tbl)) {
    err <- expect_error(
      inline(
        x,
        bmi,
        "Female",
        column = if (inherits(x, "spicy_continuous_lm_table")) "emmean"
      ),
      class = "spicy_defunct"
    )
    expect_match(conditionMessage(err), "model = \"Female\"", fixed = TRUE)
  }
  skip_if_not_installed("survey", "4.5")
  des <- survey::svydesign(ids = ~1, weights = ~weight, data = d)
  cs <- q(table_categorical_svy(des, select = smoking, by = sex))
  expect_identical(
    inline(cs, smoking, "Yes", "n", model = "Female"),
    as.character(sum(fem & d$smoking %in% "Yes"))
  )
  cvs <- q(table_continuous_svy(des, select = bmi, by = sex))
  expect_identical(
    inline(cvs, bmi, column = "n", model = "Female"),
    as.character(sum(fem & !is.na(d$bmi)))
  )
  err <- expect_error(inline(cvs, bmi, "Female", "n"), class = "spicy_defunct")
  expect_match(conditionMessage(err), "model = \"Female\"", fixed = TRUE)
})

test_that("`boot_n` has one validator, one message, one warning", {
  # Decision 12: the three tables that bootstrap check `boot_n` the same
  # way, and a `boot_n` typed in a call that runs no bootstrap warns with
  # the same words.
  d <- sochealth
  fit <- lm(bmi ~ sex, data = d)
  calls <- list(
    table_continuous_lm = function(...) {
      table_continuous_lm(d, select = bmi, by = sex, ...)
    },
    table_regression = function(...) table_regression(fit, ...),
    table_multiresponse = function(...) {
      table_multiresponse(
        d,
        select = tidyselect::starts_with("info_"),
        by = sex,
        ...
      )
    }
  )
  boot <- list(
    table_continuous_lm = list(vcov = "bootstrap"),
    table_regression = list(vcov = "bootstrap"),
    table_multiresponse = list(global_test = "bootstrap")
  )
  msgs <- vapply(
    names(calls),
    function(fn) {
      err <- expect_error(
        do.call(calls[[fn]], c(boot[[fn]], list(boot_n = 2.5))),
        class = "spicy_invalid_input",
        info = fn
      )
      conditionMessage(err)
    },
    character(1)
  )
  expect_length(unique(msgs), 1L)
  for (fn in names(calls)) {
    seen <- character(0)
    suppressMessages(suppressWarnings(withCallingHandlers(
      calls[[fn]](boot_n = 500),
      spicy_ignored_arg = function(w) {
        seen <<- c(seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )))
    expect_length(seen, 1L)
    expect_match(
      seen,
      "`boot_n` is ignored when no bootstrap runs: it applies to",
      fixed = TRUE,
      info = fn
    )
  }
})

test_that("an argument typed with no effect warns, naming only itself", {
  # V12 of the coherence audit: one rule for an argument the call typed
  # and that reaches nothing, in every function that has it.
  d <- sochealth
  ignored <- function(expr) {
    seen <- character(0)
    suppressMessages(suppressWarnings(withCallingHandlers(
      expr,
      spicy_ignored_arg = function(w) {
        seen <<- c(seen, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )))
    seen
  }
  path_docx <- file.path(tempdir(), "never-written.docx")
  path_xlsx <- file.path(tempdir(), "never-written.xlsx")
  calls <- list(
    table_categorical = function(...) {
      table_categorical(d, select = smoking, by = sex, ...)
    },
    table_continuous = function(...) {
      table_continuous(d, select = bmi, by = sex, ...)
    },
    table_continuous_lm = function(...) {
      table_continuous_lm(d, select = bmi, by = sex, ...)
    },
    table_outcome = function(...) table_outcome(d, bmi, select = sex, ...),
    table_multiresponse = function(...) {
      table_multiresponse(d, select = tidyselect::starts_with("info_"), ...)
    },
    table_regression = function(...) {
      table_regression(lm(bmi ~ sex, data = d), ...)
    },
    table_regression_uv = function(...) {
      table_regression_uv(d, outcome = bmi, predictors = c(age, sex), ...)
    }
  )
  if (
    requireNamespace("survey", quietly = TRUE) &&
      utils::packageVersion("survey") >= "4.5"
  ) {
    des <- survey::svydesign(ids = ~1, weights = ~weight, data = d)
    calls$table_categorical_svy <- function(...) {
      table_categorical_svy(des, select = smoking, by = sex, ...)
    }
    calls$table_continuous_svy <- function(...) {
      table_continuous_svy(des, select = bmi, by = sex, ...)
    }
  }
  for (fn in names(calls)) {
    w <- ignored(calls[[fn]](word_path = path_docx))
    expect_length(w, 1L)
    expect_match(w, "`word_path` is ignored", fixed = TRUE, info = fn)
    w <- ignored(calls[[fn]](excel_path = path_xlsx))
    expect_length(w, 1L)
    expect_match(w, "`excel_path` is ignored", fixed = TRUE, info = fn)
  }
  expect_false(file.exists(path_docx))
  expect_false(file.exists(path_xlsx))

  # Without `by`: the continuous pair names what was typed and nothing
  # else, and both treat `drop_na = FALSE` alike.
  w <- ignored(table_continuous(d, select = bmi, p_value = TRUE))
  expect_identical(w, "`p_value` is ignored when `by` is not used.")
  w_drop <- ignored(table_continuous(d, select = bmi, drop_na = FALSE))
  expect_length(w_drop, 1L)
  expect_match(
    w_drop,
    "`drop_na` is ignored when `by` is not used",
    fixed = TRUE
  )
  skip_if_not_installed("survey", "4.5")
  des <- survey::svydesign(ids = ~1, weights = ~weight, data = d)
  expect_identical(
    ignored(table_continuous_svy(des, select = bmi, p_value = TRUE)),
    w
  )
  expect_identical(
    ignored(table_continuous_svy(des, select = bmi, drop_na = FALSE)),
    w_drop
  )
})

test_that("identical checks of the family give identical messages", {
  # V13 of the coherence audit: one wording per check, from the shared
  # validators.
  d <- sochealth
  fit <- lm(bmi ~ sex, data = d)
  msg <- function(expr) {
    conditionMessage(expect_error(expr, class = "spicy_invalid_input"))
  }
  head1 <- function(m) strsplit(m, "\n", fixed = TRUE)[[1L]][[1L]]
  ci <- c(
    msg(table_continuous(d, select = bmi, ci_level = 2)),
    msg(table_continuous_lm(d, select = bmi, by = sex, ci_level = 2)),
    msg(table_outcome(d, bmi, select = sex, ci_level = 2)),
    msg(table_regression(fit, ci_level = 2))
  )
  expect_identical(
    unique(ci),
    "`ci_level` must be a number strictly between 0 and 1."
  )
  digits <- c(
    msg(table_continuous(d, select = bmi, digits = 1.5)),
    msg(table_continuous_lm(d, select = bmi, by = sex, digits = 1.5)),
    msg(table_regression(fit, digits = 1.5))
  )
  expect_identical(
    unique(digits),
    "`digits` must be a non-negative whole number."
  )
  flags <- c(
    msg(table_categorical(d, select = smoking, drop_na = NA)),
    msg(table_continuous(d, select = bmi, drop_na = NA)),
    msg(table_outcome(d, bmi, select = sex, drop_na = NA))
  )
  expect_identical(unique(flags), "`drop_na` must be TRUE or FALSE.")
  marks <- c(
    msg(table_categorical(d, select = smoking, decimal_mark = 1)),
    msg(table_continuous(d, select = bmi, decimal_mark = 1)),
    msg(table_multiresponse(
      d,
      select = tidyselect::starts_with("info_"),
      decimal_mark = 1
    )),
    msg(table_regression(fit, decimal_mark = 1))
  )
  expect_length(unique(vapply(marks, head1, "")), 1L)
})
