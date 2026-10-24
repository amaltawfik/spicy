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
  # Nothing to say: no note, printed or attached. Without its interval a
  # table of means shows no abbreviation to define either.
  plain <- table_categorical(mtcars, select = cyl)
  expect_null(.fp_note_block(plain))
  expect_null(attr(plain, "note"))
  expect_null(attr(table_continuous(mtcars, select = mpg, ci = FALSE), "note"))
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

test_that("a role column named in the selection is refused whatever the environment holds", {
  skip_if_not_installed("survey")
  skip_if_not_installed("clubSandwich")
  withr::local_options(lifecycle_verbosity = "warning")
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
  # Traps: each role column's name also names an object here, holding the
  # name of another column. The name is the column all the same.
  g <- y <- w <- psu <- "x"
  calls <- alist(
    table_continuous(d, select = c(y, g), by = g),
    table_outcome(d, y, select = c(f, y), p_value = FALSE),
    table_regression_uv(d, outcome = y, predictors = c(x, y)),
    table_continuous_lm(d, select = c(y, w), by = g, covariates = w),
    table_categorical(d, select = c(f, w), weights = w),
    table_continuous_lm(
      d,
      select = c(y, psu),
      by = g,
      cluster = psu,
      vcov = "CR2"
    ),
    table_continuous_svy(iid, select = c(y, w)),
    table_categorical_svy(clus, select = c(f, psu))
  )
  refused <- c(
    "`by` column (\"g\")",
    "`outcome` column (\"y\")",
    "`outcome` column (\"y\")",
    "`covariates` column (\"w\")",
    "`weights` column (\"w\")",
    "`cluster` column (\"psu\")",
    "design's sampling weights column (\"w\")",
    "design's cluster column (\"psu\")"
  )
  for (i in seq_along(calls)) {
    err <- expect_no_warning(tryCatch(eval(calls[[i]]), error = identity))
    expect_s3_class(err, "spicy_invalid_input")
    expect_match(
      conditionMessage(err),
      refused[[i]],
      fixed = TRUE,
      info = deparse(calls[[i]])
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
    # global test of a multiple-response table and the direction of the
    # percentages of a categorical one aside).
    extra <- setdiff(
      names(attributes(df)),
      c("names", "row.names", "class", "note", "global_test", "percent")
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

test_that("freq() orders text and logical values as the tables do", {
  # The rule of the groups (decision 4) applied to the values of freq():
  # text in the C locale, FALSE before TRUE, a factor keeping its levels,
  # whatever the locale of the machine. cross_tab(), varlist() and
  # code_book() already sorted that way.
  d <- .fg_data()
  q <- function(e) suppressMessages(suppressWarnings(e))
  vals <- function(x) as.character(stats::na.omit(x$value))
  expect_identical(
    vals(q(freq(d, g, output = "data.frame"))),
    c("A", "B", "a", "b", "e", "\u00e9")
  )
  expect_identical(
    vals(q(freq(d, l, output = "data.frame"))),
    c("FALSE", "TRUE")
  )
  expect_identical(
    vals(q(freq(d, f, output = "data.frame"))),
    c("z", "a", "m")
  )
})

test_that("the interval of an estimate states its level in every header", {
  # A bound in a column of its own is named after its interval ("95% CI
  # LL"), in the data frame too, and a cell that prints an estimate with
  # its interval is headed "<estimate> [95% CI]".
  d <- as.data.frame(sochealth)
  q <- function(e) suppressMessages(suppressWarnings(e))
  cat_df <- q(table_categorical(
    d,
    select = education,
    by = sex,
    assoc_ci = TRUE,
    output = "data.frame"
  ))
  expect_true(all(c("95% CI LL", "95% CI UL") %in% names(cat_df)))
  expect_false(any(c("CI lower", "CI upper") %in% names(cat_df)))
  cont_df <- q(table_continuous(
    d,
    select = bmi,
    by = sex,
    output = "data.frame"
  ))
  expect_true(all(c("95% CI LL", "95% CI UL") %in% names(cont_df)))
  # The data frame keeps an estimate under the measure's name, its bounds
  # beside it.
  lm_df <- q(table_continuous_lm(
    d,
    select = bmi,
    by = sex,
    effect_size = "cohens_d",
    effect_size_ci = TRUE,
    output = "data.frame"
  ))
  expect_true(all(c("d", "es_ci_lower", "es_ci_upper") %in% names(lm_df)))
  skip_if_not_installed("flextable")
  header <- function(ft) unlist(ft$header$dataset, use.names = FALSE)
  expect_true(
    "Cramer's V [95% CI]" %in%
      header(q(table_categorical(
        d,
        select = education,
        by = sex,
        assoc_ci = TRUE,
        output = "flextable"
      )))
  )
  expect_true(
    "d [95% CI]" %in%
      header(q(table_continuous_lm(
        d,
        select = bmi,
        by = sex,
        effect_size = "cohens_d",
        effect_size_ci = TRUE,
        output = "flextable"
      )))
  )
  expect_true(
    "d" %in%
      header(q(table_continuous_lm(
        d,
        select = bmi,
        by = sex,
        effect_size = "cohens_d",
        output = "flextable"
      )))
  )
  expect_true(
    "ES [95% CI]" %in%
      header(q(table_continuous(
        d,
        select = bmi,
        by = sex,
        effect_size = "hedges_g",
        effect_size_ci = TRUE,
        output = "flextable"
      )))
  )
  expect_true(
    "ES [95% CI]" %in%
      header(q(table_outcome(
        d,
        outcome = bmi,
        select = sex,
        effect_size = "hedges_g",
        effect_size_ci = TRUE,
        output = "flextable"
      )))
  )
  skip_if_not_installed("survey", "4.5")
  des <- survey::svydesign(ids = ~1, weights = ~weight, data = d)
  svy_ft <- q(table_categorical_svy(
    des,
    select = smoking,
    by = sex,
    proportion_ci = TRUE,
    output = "flextable"
  ))
  expect_true(all(c("95% CI LL", "95% CI UL") %in% header(svy_ft)))
})

test_that("a statistic of the variable is cited without `model`", {
  # In a table whose groups are rows, the test, its p, the effect size
  # and the SMD belong to the variable (the table prints them once, on
  # the row of the first group): asked with `model`, whichever group it
  # names, they are refused with the call that cites them.
  d <- as.data.frame(sochealth)
  q <- function(e) suppressMessages(suppressWarnings(e))
  cn <- q(table_continuous(
    d,
    select = bmi,
    by = sex,
    statistic = TRUE,
    effect_size = "hedges_g",
    smd = TRUE
  ))
  for (tk in c("p", "statistic", "es", "smd")) {
    expect_true(nzchar(inline(cn, bmi, column = tk)))
    for (g in c("Female", "Male")) {
      err <- expect_error(
        inline(cn, bmi, column = tk, model = g),
        class = "spicy_invalid_input"
      )
      msg <- conditionMessage(err)
      expect_match(msg, "statistic of the variable", fixed = TRUE)
      expect_match(
        msg,
        sprintf("inline(x, bmi, column = \"%s\")", tk),
        fixed = TRUE
      )
      expect_no_match(msg, "is empty", fixed = TRUE)
    }
  }
  err <- expect_error(inline(cn, bmi, "Male", "p"), class = "spicy_defunct")
  expect_match(
    conditionMessage(err),
    "inline(x, bmi, column = \"p\")",
    fixed = TRUE
  )
  skip_if_not_installed("survey", "4.5")
  des <- survey::svydesign(ids = ~1, weights = ~weight, data = d)
  cvs <- q(table_continuous_svy(des, select = bmi, by = sex))
  for (g in c("Female", "Male")) {
    err <- expect_error(
      inline(cvs, bmi, column = "p", model = g),
      class = "spicy_invalid_input"
    )
    expect_match(
      conditionMessage(err),
      "statistic of the variable",
      fixed = TRUE
    )
  }
})

# ---- The note: one plan for its sentences ----------------------------------
#
# The note under every table of the family follows one plan (APA
# Publication Manual, 7th ed., section 7.14): the general note, one
# paragraph whose sentences come in four kinds -- (a) what the numbers
# are, (b) the missing data, (c) the comparisons and the inference, (d)
# the definitions -- then the stars legend on a line of its own. The
# markers below are read from the registry, so one test speaks for both
# languages.
.fn_data <- function() {
  set.seed(7)
  n <- 180
  d <- data.frame(
    g = factor(rep(c("a", "b", "c"), each = 60)),
    g2 = factor(rep(c("p", "q"), times = 90)),
    xc = factor(sample(c("no", "yes"), n, TRUE)),
    xn = round(stats::rnorm(n, 10, 2), 1),
    age = round(stats::runif(n, 20, 70)),
    m1 = sample(c(TRUE, FALSE), n, TRUE),
    m2 = sample(c(TRUE, FALSE), n, TRUE),
    m3 = sample(c(TRUE, FALSE), n, TRUE),
    w = round(stats::runif(n, 0.5, 2), 3)
  )
  d$y01 <- as.integer(d$xn + stats::rnorm(n) > 10)
  # A few ordinary missing values, the grouping included.
  d$xc[c(1, 2, 61)] <- NA
  d$xn[c(3, 62)] <- NA
  d$g_na <- d$g
  d$g_na[c(4, 64)] <- NA
  d
}

# The position of each marker in `note`, in the order given: all found,
# and strictly increasing.
.fn_in_order <- function(note, markers, info) {
  pos <- vapply(
    markers,
    function(m) as.numeric(regexpr(m, note, fixed = TRUE)),
    numeric(1)
  )
  testthat::expect_true(all(pos > 0), info = paste(info, "(all found)"))
  testthat::expect_true(all(diff(pos) > 0), info = paste(info, "(in order)"))
}

# The fixed part of a registry template, before its first hole.
.fn_lead <- function(key) sub("%.*$", "", spicy_str(key))

test_that("the sentences of every note come in the four kinds, in order", {
  skip_if_not_installed("survey")
  d <- .fn_data()
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  q <- function(e) suppressMessages(suppressWarnings(e))
  for (lang in c("en", "fr")) {
    withr::local_options(spicy.language = lang)
    weighted <- spicy_fmt("note_weighted_by", "w")
    removed <- spicy_str("note_missing_removed")
    compared <- .fn_lead("note_group_comparison")
    std_err <- .fn_lead("note_std_errors_single")
    cases <- list(
      table_categorical = list(
        q(table_categorical(
          d,
          select = xc,
          by = g2,
          weights = w,
          test = "chisq",
          drop_na = TRUE,
          smd = TRUE
        )),
        c(weighted, removed, compared, paste0(spicy_str("header_smd"), " ="))
      ),
      table_categorical_svy = list(
        q(table_categorical_svy(des, select = xc, by = g2, drop_na = TRUE)),
        c(
          .fn_lead("note_design_n"),
          removed,
          std_err,
          compared,
          paste0(spicy_str("header_percent_symbol"), " =")
        )
      ),
      table_continuous_weighted = list(
        q(table_continuous(
          d,
          select = xn,
          by = g2,
          weights = w,
          p_value = FALSE,
          show_columns = c("m", "iqr")
        )),
        c(weighted, removed, paste0(spicy_str("header_iqr"), " ="))
      ),
      table_continuous = list(
        q(table_continuous(
          d,
          select = c(xn, age),
          by = g2,
          smd = TRUE,
          p_adjust = "holm"
        )),
        c(
          removed,
          compared,
          .fn_lead("note_p_adjusted"),
          paste0(spicy_str("header_smd"), " =")
        )
      ),
      table_continuous_svy = list(
        q(table_continuous_svy(
          des,
          select = xn,
          by = g2,
          show_columns = c("m", "se")
        )),
        c(
          .fn_lead("note_design_n"),
          removed,
          std_err,
          compared,
          paste0(spicy_str("header_se"), " =")
        )
      ),
      table_continuous_lm = list(
        q(table_continuous_lm(
          d,
          select = xn,
          by = g2,
          weights = w,
          covariates = age,
          vcov = "HC3"
        )),
        c(weighted, .fn_lead("note_adjusted_for"), removed, compared, std_err)
      ),
      table_outcome_weighted = list(
        q(table_outcome(
          d,
          outcome = xn,
          select = g2,
          weights = w,
          p_value = FALSE
        )),
        c(weighted, removed, spicy_str("note_outcome_overall"))
      ),
      table_outcome = list(
        q(table_outcome(d, outcome = xn, select = c(g2, g), p_adjust = "holm")),
        c(
          removed,
          compared,
          .fn_lead("note_p_adjusted"),
          spicy_str("note_outcome_overall")
        )
      ),
      table_multiresponse = list(
        q(table_multiresponse(d, select = c(m1, m2), by = g_na)),
        c(
          spicy_str("note_multiresponse_pct_cases"),
          .fn_lead("note_multiresponse_base"),
          .fn_lead("note_rows_missing_by_removed"),
          .fn_lead("note_multiresponse_items_p")
        )
      ),
      table_regression = list(
        q(table_regression(
          stats::glm(y01 ~ age + g2, data = d, family = stats::binomial()),
          exponentiate = TRUE,
          p_adjust = "holm"
        )),
        c(
          .fn_lead("note_exp_with_se_single"),
          std_err,
          .fn_lead("note_p_adjusted"),
          paste0(spicy_str("header_ci_label_confidence"), " =")
        )
      ),
      table_regression_uv = list(
        q(table_regression_uv(
          d,
          outcome = y01,
          predictors = c(age, xc),
          method = "glm",
          exponentiate = TRUE
        )),
        c(
          .fn_lead("note_exp_with_se_single"),
          .fn_lead("note_uv_varying_n"),
          std_err,
          paste0(spicy_str("header_ci_label_confidence"), " =")
        )
      )
    )
    for (fn in names(cases)) {
      .fn_in_order(
        attr(cases[[fn]][[1L]], "note"),
        cases[[fn]][[2L]],
        paste(lang, fn)
      )
    }
  }
})

test_that("the general note is one paragraph, the stars legend a line of its own", {
  skip_if_not_installed("survey")
  d <- .fn_data()
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  q <- function(e) suppressMessages(suppressWarnings(e))
  for (lang in c("en", "fr")) {
    withr::local_options(spicy.language = lang)
    stars <- build_stars_footer_block(TRUE, if (lang == "fr") "," else ".")
    # Every table with sentences of several kinds, the line breaks that
    # used to split them included (`drop_na = TRUE` in
    # table_categorical(), a non-classical `vcov` in table_continuous_lm()
    # and table_regression()).
    tables <- list(
      table_categorical = q(table_categorical(
        d,
        select = c(xc, g),
        by = g2,
        drop_na = TRUE,
        p_adjust = "holm"
      )),
      table_categorical_svy = q(table_categorical_svy(
        des,
        select = xc,
        by = g2,
        drop_na = TRUE
      )),
      table_continuous = q(table_continuous(d, select = xn, by = g2)),
      table_continuous_svy = q(table_continuous_svy(des, select = xn, by = g2)),
      table_continuous_lm = q(table_continuous_lm(
        d,
        select = xn,
        by = g2,
        vcov = "HC3"
      )),
      table_outcome = q(table_outcome(d, outcome = xn, select = g2)),
      table_multiresponse = q(table_multiresponse(
        d,
        select = c(m1, m2),
        by = g_na
      )),
      table_regression = q(table_regression(
        list(
          stats::lm(xn ~ age, data = d),
          stats::lm(xn ~ age + g2, data = d)
        ),
        vcov = list("classical", "HC3"),
        p_adjust = "holm",
        stars = TRUE
      )),
      table_regression_uv = q(table_regression_uv(
        d,
        outcome = xn,
        predictors = c(age, xc),
        stars = TRUE
      ))
    )
    for (fn in names(tables)) {
      x <- tables[[fn]]
      lines <- strsplit(attr(x, "note"), "\n", fixed = TRUE)[[1L]]
      with_stars <- fn %in% c("table_regression", "table_regression_uv")
      expect_length(lines, if (with_stars) 2L else 1L)
      if (with_stars) {
        expect_identical(lines[[2L]], stars, info = paste(lang, fn))
      }
      # The console prints the paragraph, then the legend, each whole.
      out <- utils::capture.output(print(x))
      expect_true(all(lines %in% out), info = paste(lang, fn))
    }
  }
})

test_that("every table that shows a p-value names its test", {
  skip_if_not_installed("survey")
  d <- .fn_data()
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  q <- function(e) suppressMessages(suppressWarnings(e))
  for (lang in c("en", "fr")) {
    withr::local_options(spicy.language = lang)
    compared <- .fn_lead("note_group_comparison")
    named <- list(
      table_categorical = list(
        q(table_categorical(d, select = xc, by = g)),
        spicy_fmt("note_group_comparison", spicy_str("test_pearson_chisq"))
      ),
      table_categorical_svy = list(
        q(table_categorical_svy(des, select = xc, by = g)),
        compared
      ),
      # Named whatever the display, not only after a rank switch.
      table_continuous = list(
        q(table_continuous(d, select = xn, by = g)),
        spicy_fmt("note_group_comparison", spicy_str("test_welch_oneway_anova"))
      ),
      table_continuous_svy = list(
        q(table_continuous_svy(des, select = xn, by = g)),
        compared
      ),
      table_continuous_lm = list(
        q(table_continuous_lm(d, select = xn, by = g)),
        spicy_fmt("note_group_comparison", spicy_str("test_lm_f"))
      ),
      table_continuous_lm_slope = list(
        q(table_continuous_lm(d, select = xn, by = age)),
        spicy_fmt("note_slope_test", spicy_str("test_lm_t"))
      ),
      table_outcome = list(
        q(table_outcome(d, outcome = xn, select = g)),
        spicy_fmt("note_group_comparison", spicy_str("test_welch_oneway_anova"))
      ),
      table_multiresponse = list(
        q(table_multiresponse(d, select = c(m1, m2), by = g)),
        .fn_lead("note_multiresponse_items_p")
      )
    )
    for (fn in names(named)) {
      expect_true(
        grepl(named[[fn]][[2L]], attr(named[[fn]][[1L]], "note"), fixed = TRUE),
        info = paste(lang, fn)
      )
    }
    # Without p-values, no test is named (and, without its interval, the
    # table shows no abbreviation to define).
    expect_null(attr(
      q(table_continuous(d, select = age, by = g, p_value = FALSE, ci = FALSE)),
      "note"
    ))
    expect_null(attr(
      q(table_continuous_lm(d, select = age, by = g, p_value = FALSE)),
      "note"
    ))
  }
})

test_that("every table that accepts weights says it is weighted", {
  d <- .fn_data()
  q <- function(e) suppressMessages(suppressWarnings(e))
  for (lang in c("en", "fr")) {
    withr::local_options(spicy.language = lang)
    opening <- paste0(.note_prefix_text(), spicy_fmt("note_weighted_by", "w"))
    weighted <- list(
      table_categorical = q(table_categorical(
        d,
        select = xc,
        by = g,
        weights = w
      )),
      table_categorical_noby = q(table_categorical(
        d,
        select = xc,
        weights = w
      )),
      table_continuous = q(table_continuous(
        d,
        select = age,
        by = g,
        weights = w,
        p_value = FALSE
      )),
      table_continuous_lm = q(table_continuous_lm(
        d,
        select = age,
        by = g,
        weights = w
      )),
      table_outcome = q(table_outcome(
        d,
        outcome = age,
        select = g,
        weights = w,
        p_value = FALSE
      ))
    )
    for (fn in names(weighted)) {
      expect_true(
        startsWith(attr(weighted[[fn]], "note"), opening),
        info = paste(lang, fn)
      )
    }
  }
})

# ---- A group with no data is not a missing answer -------------------------
#
# `xc`, `xn` and the item `m3` have no observed value in group "c" of `g`
# (a question not asked there); `g2` keeps one group with data for them.
.fn_nodata_data <- function() {
  d <- .fn_data()
  d$xc[d$g == "c"] <- NA
  d$xn[d$g == "c"] <- NA
  d$m3[d$g == "c"] <- NA
  d$xc2 <- d$xc
  d$xn2 <- d$xn
  d$xc2[d$g2 == "q"] <- NA
  d$xn2[d$g2 == "q"] <- NA
  d
}

test_that("a group with no data shows the mark, counts no missing value, and leaves the test", {
  skip_if_not_installed("survey")
  d <- .fn_nodata_data()
  d_ab <- droplevels(d[d$g != "c", ])
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  # The design restricted to the groups with data: a domain of the same
  # design, as the comparison of the full table reads it.
  des_ab <- subset(des, g != "c")
  q <- function(e) suppressMessages(suppressWarnings(e))
  undefined <- spicy_str("cell_undefined")
  # One entry per grouping builder: the table on the data, the same table
  # on the data without group "c", the value of the variable's cells in
  # group "c" (machine output), its p-value, and the missing-value count
  # the note gives it (NULL where the table has no such sentence).
  p_of <- function(x, col = "p.value") {
    v <- x[[col]]
    v[!is.na(v)][[1L]]
  }
  cases <- list(
    table_categorical = list(
      build = function(dd) {
        q(table_categorical(
          dd,
          select = xc,
          by = g,
          drop_na = TRUE,
          output = "long"
        ))
      },
      cell = function(l) l$n[l$group == "c"],
      p = function(l) p_of(l),
      missing = list(label = "xc", n = 3L)
    ),
    table_categorical_svy = list(
      build = function(dd) {
        des_dd <- if (nlevels(dd$g) == 3L) des else des_ab
        q(table_categorical_svy(
          des_dd,
          select = xc,
          by = g,
          drop_na = TRUE,
          output = "long"
        ))
      },
      cell = function(l) l$n[l$group == "c"],
      p = function(l) p_of(l),
      missing = list(label = "xc", n = 3L)
    ),
    table_continuous = list(
      build = function(dd) {
        q(table_continuous(dd, select = xn, by = g, output = "long"))
      },
      cell = function(l) l$n[l$group == "c"],
      p = function(l) p_of(l),
      missing = list(label = "xn", n = 2L)
    ),
    table_continuous_svy = list(
      build = function(dd) {
        des_dd <- if (nlevels(dd$g) == 3L) des else des_ab
        q(table_continuous_svy(des_dd, select = xn, by = g, output = "long"))
      },
      cell = function(l) l$n[l$group == "c"],
      p = function(l) p_of(l),
      missing = list(label = "xn", n = 2L)
    ),
    table_continuous_lm = list(
      build = function(dd) {
        q(table_continuous_lm(dd, select = xn, by = g, output = "long"))
      },
      cell = function(l) l$emmean[l$level == "c"],
      p = function(l) p_of(l),
      missing = list(label = "xn", n = 2L)
    ),
    table_outcome = list(
      build = function(dd) {
        q(table_outcome(dd, outcome = xn, select = g, output = "long"))
      },
      cell = function(l) l$n[!is.na(l$level) & l$level == "c"],
      p = function(l) p_of(l),
      missing = list(label = "xn", n = 2L)
    ),
    table_multiresponse = list(
      build = function(dd) {
        q(table_multiresponse(dd, select = m3, by = g, output = "long"))
      },
      cell = function(l) l$n[l$variable == "m3" & l$group == "c"],
      p = function(l) p_of(l),
      missing = NULL
    )
  )
  for (fn in names(cases)) {
    cs <- cases[[fn]]
    full <- cs$build(d)
    # The cells of the group show the mark: NA in the machine output.
    expect_true(length(cs$cell(full)) > 0L, info = fn)
    expect_true(all(is.na(cs$cell(full))), info = fn)
    # The comparison of the variable is the one of the groups with data.
    expect_equal(cs$p(full), cs$p(cs$build(d_ab)), tolerance = 1e-10, info = fn)
    for (lang in c("en", "fr")) {
      withr::local_options(spicy.language = lang)
      note <- attr(cs$build(d), "note")
      label <- if (fn == "table_multiresponse") "m3" else cs$missing$label
      expect_true(
        grepl(.no_data_sentences(label, list("c")), note, fixed = TRUE),
        info = paste(lang, fn)
      )
      if (!is.null(cs$missing)) {
        expect_true(
          grepl(
            paste0(
              spicy_str("note_missing_removed"),
              spicy_fmt("note_missing_item", cs$missing$label, cs$missing$n),
              "."
            ),
            note,
            fixed = TRUE
          ),
          info = paste(lang, fn)
        )
      }
    }
  }
  # The console shows the mark in the variable's cells of that group.
  out <- utils::capture.output(print(q(table_categorical(
    d,
    select = xc,
    by = g
  ))))
  expect_true(any(grepl(undefined, out, fixed = TRUE)))
})

test_that("a variable with data in fewer than two groups is not compared", {
  skip_if_not_installed("survey")
  d <- .fn_nodata_data()
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  q <- function(e) suppressMessages(suppressWarnings(e))
  tables <- list(
    table_categorical = function() {
      q(table_categorical(d, select = xc2, by = g2))
    },
    table_categorical_svy = function() {
      q(table_categorical_svy(des, select = xc2, by = g2))
    },
    table_continuous = function() q(table_continuous(d, select = xn2, by = g2)),
    table_continuous_svy = function() {
      q(table_continuous_svy(des, select = xn2, by = g2))
    },
    table_continuous_lm = function() {
      q(table_continuous_lm(d, select = xn2, by = g2))
    },
    table_outcome = function() q(table_outcome(d, outcome = xn2, select = g2))
  )
  for (lang in c("en", "fr")) {
    withr::local_options(spicy.language = lang)
    for (fn in names(tables)) {
      x <- tables[[fn]]()
      note <- attr(x, "note")
      label <- if (fn %in% c("table_categorical", "table_categorical_svy")) {
        "xc2"
      } else {
        "xn2"
      }
      expect_true(
        grepl(.no_data_sentences(label, list("q")), note, fixed = TRUE),
        info = paste(lang, fn)
      )
      # No test ran, so none is named.
      expect_false(
        grepl(.fn_lead("note_group_comparison"), note, fixed = TRUE),
        info = paste(lang, fn)
      )
      p <- as.data.frame(x)[["p"]]
      expect_true(is.null(p) || all(is.na(p)), info = paste(lang, fn))
    }
  }
  # An item of a multiple-response table: blank p, no test named for it.
  d$m3[d$g2 == "q"] <- NA
  mr <- q(table_multiresponse(d, select = c(m1, m3), by = g2, output = "long"))
  expect_true(all(is.na(mr$p.value[mr$variable == "m3"])))
  expect_false(all(is.na(mr$p.value[mr$variable == "m1"])))
})

test_that("without `by`, a variable with no observed value has no data", {
  skip_if_not_installed("survey")
  d <- .fn_data()
  d$xc <- factor(NA, levels = c("no", "yes"))
  d$xn <- NA_real_
  d$m3 <- NA
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  q <- function(e) suppressMessages(suppressWarnings(e))
  for (lang in c("en", "fr")) {
    withr::local_options(spicy.language = lang)
    notes <- list(
      table_categorical = list(
        q(table_categorical(d, select = c(xc, g2), drop_na = TRUE)),
        "xc"
      ),
      table_categorical_svy = list(
        q(table_categorical_svy(des, select = c(xc, g2), drop_na = TRUE)),
        "xc"
      ),
      table_continuous = list(
        q(table_continuous(d, select = c(xn, age))),
        "xn"
      ),
      table_continuous_svy = list(
        q(table_continuous_svy(des, select = c(xn, age))),
        "xn"
      ),
      table_multiresponse = list(
        q(table_multiresponse(d, select = c(m1, m3))),
        "m3"
      )
    )
    for (fn in names(notes)) {
      x <- notes[[fn]][[1L]]
      note <- attr(x, "note")
      expect_true(
        grepl(spicy_fmt("note_no_data", notes[[fn]][[2L]]), note, fixed = TRUE),
        info = paste(lang, fn)
      )
      # Not a missing value: the variable is not in the count.
      expect_false(
        grepl(
          spicy_fmt("note_missing_item", notes[[fn]][[2L]], nrow(d)),
          note,
          fixed = TRUE
        ),
        info = paste(lang, fn)
      )
      # The variable stays in the table, its cells marked.
      out <- utils::capture.output(print(x))
      expect_true(
        any(grepl(spicy_str("cell_undefined"), out, fixed = TRUE)),
        info = paste(lang, fn)
      )
    }
  }
})

test_that("the variables with no data in the same groups share one sentence", {
  skip_if_not_installed("survey")
  # With `by = g`: xc, xc2, xc4 (and xn, xn2; m2, m3) have no data in "c",
  # xc3 (and xn3) none in "b" and "c". One sentence per set of groups, in
  # the order the rows first meet it, each list joined in the language.
  d <- .fn_nodata_data()
  d$xc4 <- d$xc
  d$xc3 <- d$xc
  d$xc3[d$g == "b"] <- NA
  d$xn3 <- d$xn
  d$xn3[d$g == "b"] <- NA
  d$m2[d$g == "c"] <- NA
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  # Without `by`: two variables of each kind with no value at all.
  d0 <- .fn_data()
  d0$xc <- factor(NA, levels = c("no", "yes"))
  d0$xe <- factor(NA, levels = c("no", "yes"))
  d0$xn <- NA_real_
  d0$xne <- NA_real_
  d0$m2 <- NA
  d0$m3 <- NA
  des0 <- survey::svydesign(ids = ~1, weights = ~w, data = d0)
  q <- function(e) suppressMessages(suppressWarnings(e))
  tables <- list(
    table_categorical = function() {
      q(table_categorical(d, select = c(xc, xc2, xc3, xc4), by = g))
    },
    table_categorical_svy = function() {
      q(table_categorical_svy(des, select = c(xc, xc2, xc3, xc4), by = g))
    },
    table_continuous = function() {
      q(table_continuous(d, select = c(xn, xn3, xn2), by = g))
    },
    table_continuous_svy = function() {
      q(table_continuous_svy(des, select = c(xn, xn3, xn2), by = g))
    },
    table_continuous_lm = function() {
      q(table_continuous_lm(d, select = c(xn, xn3, xn2), by = g))
    },
    table_multiresponse = function() {
      q(table_multiresponse(d, select = c(m1, m2, m3), by = g))
    },
    no_by_categorical = function() {
      q(table_categorical(d0, select = c(xc, g2, xe), drop_na = TRUE))
    },
    no_by_categorical_svy = function() {
      q(table_categorical_svy(des0, select = c(xc, g2, xe), drop_na = TRUE))
    },
    no_by_continuous = function() {
      q(table_continuous(d0, select = c(xn, age, xne)))
    },
    no_by_continuous_svy = function() {
      q(table_continuous_svy(des0, select = c(xn, age, xne)))
    },
    no_by_multiresponse = function() {
      q(table_multiresponse(d0, select = c(m1, m2, m3)))
    }
  )
  # The lists as each language joins them: the serial comma in English,
  # none before "et" in French.
  lists <- list(
    en = list(
      cat = "xc, xc2, and xc4",
      num = "xn and xn2",
      mr = "m2 and m3",
      cat0 = "xc and xe",
      num0 = "xn and xne",
      groups = "b and c"
    ),
    fr = list(
      cat = "xc, xc2 et xc4",
      num = "xn et xn2",
      mr = "m2 et m3",
      cat0 = "xc et xe",
      num0 = "xn et xne",
      groups = "b et c"
    )
  )
  for (lang in c("en", "fr")) {
    withr::local_options(spicy.language = lang)
    l <- lists[[lang]]
    at <- function(vars, groups) spicy_fmt("note_no_data_in", vars, groups)
    cat_by <- paste(at(l$cat, "c"), at("xc3", l$groups))
    num_by <- paste(at(l$num, "c"), at("xn3", l$groups))
    expected <- list(
      table_categorical = cat_by,
      table_categorical_svy = cat_by,
      table_continuous = num_by,
      table_continuous_svy = num_by,
      table_continuous_lm = num_by,
      table_multiresponse = at(l$mr, "c"),
      no_by_categorical = spicy_fmt("note_no_data", l$cat0),
      no_by_categorical_svy = spicy_fmt("note_no_data", l$cat0),
      no_by_continuous = spicy_fmt("note_no_data", l$num0),
      no_by_continuous_svy = spicy_fmt("note_no_data", l$num0),
      no_by_multiresponse = spicy_fmt("note_no_data", l$mr)
    )
    for (fn in names(tables)) {
      note <- attr(tables[[fn]](), "note")
      expect_true(
        grepl(expected[[fn]], note, fixed = TRUE),
        info = paste(lang, fn, note)
      )
      # No other no-data sentence: each variable is named once.
      n_sentences <- lengths(regmatches(
        note,
        gregexpr(.fn_lead("note_no_data"), note, fixed = TRUE)
      ))
      expect_identical(
        n_sentences,
        if (fn %in% names(tables)[1:5]) 2L else 1L,
        info = paste(lang, fn)
      )
    }
  }
})

test_that("a list whose items hold a comma takes semicolons, in every language", {
  # sochealth labels three variables with a comma of their own. A list of
  # three or more such labels separates them with semicolons, as the
  # family's other lists of labels do (`.join_note_vars()`); two take the
  # conjunction alone. The p-adjust sentence joins numbers, which hold no
  # comma, and keeps its commas.
  d <- as.data.frame(sochealth)
  tert <- d$education == "Tertiary"
  for (v in c(
    "info_internet",
    "info_family",
    "info_media",
    "political_position",
    "bmi",
    "wellbeing_score"
  )) {
    d[[v]][tert] <- NA
  }
  q <- function(e) suppressMessages(suppressWarnings(e))
  tables <- list(
    mr3 = function() {
      q(table_multiresponse(
        d,
        select = c(info_internet, info_family, info_doctor, info_media),
        by = education,
        labels = TRUE
      ))
    },
    mr2 = function() {
      q(table_multiresponse(
        d,
        select = c(info_internet, info_doctor, info_media),
        by = education,
        labels = TRUE
      ))
    },
    num3 = function() {
      q(table_continuous(
        d,
        select = c(political_position, bmi, wellbeing_score),
        by = education,
        labels = TRUE
      ))
    }
  )
  # French puts a no-break space before the semicolon, and no semicolon
  # before "et".
  nbsp <- intToUtf8(160L)
  lists <- list(
    en = list(
      mr3 = paste0(
        "Internet, websites or apps; Family or friends; and Television, ",
        "radio or newspapers"
      ),
      mr2 = "Internet, websites or apps and Television, radio or newspapers",
      num3 = paste0(
        "Political position (0 = left, 10 = right); Body mass index; and ",
        "WHO-5 wellbeing index (0-100)"
      ),
      p2 = "2 and 1",
      p3 = "2, 1, and 3"
    ),
    fr = list(
      mr3 = paste0(
        "Internet, websites or apps",
        nbsp,
        "; Family or friends et Television, radio or newspapers"
      ),
      mr2 = "Internet, websites or apps et Television, radio or newspapers",
      num3 = paste0(
        "Political position (0 = left, 10 = right)",
        nbsp,
        "; Body mass index et WHO-5 wellbeing index (0-100)"
      ),
      p2 = "2 et 1",
      p3 = "2, 1 et 3"
    )
  )
  for (lang in c("en", "fr")) {
    withr::local_options(spicy.language = lang)
    l <- lists[[lang]]
    for (fn in names(tables)) {
      note <- attr(tables[[fn]](), "note")
      expect_true(
        grepl(
          spicy_fmt("note_no_data_in", l[[fn]], "Tertiary"),
          note,
          fixed = TRUE
        ),
        info = paste(lang, fn, note)
      )
    }
    holm <- spicy_str("note_p_adjust_method_holm")
    for (m in list(p2 = c(2L, 1L), p3 = c(2L, 1L, 3L))) {
      key <- paste0("p", length(m))
      expect_identical(
        .p_adjust_sentence("holm", m),
        spicy_fmt("note_p_adjusted_per_model_unequal", holm, l[[key]]),
        info = paste(lang, key)
      )
    }
  }
})

test_that("every English note spells chi-squared", {
  skip_if_not_installed("survey")
  d <- .fn_data()
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  q <- function(e) suppressMessages(suppressWarnings(e))
  notes <- c(
    attr(q(table_categorical_svy(des, select = xc, by = g)), "note"),
    attr(
      q(table_categorical_svy(
        des,
        select = xc,
        by = g,
        chisq_statistic = "Chisq"
      )),
      "note"
    ),
    attr(
      q(table_categorical_svy(
        des,
        select = xc,
        by = g,
        chisq_statistic = "saddlepoint"
      )),
      "note"
    ),
    attr(q(table_categorical(d, select = xc, by = g)), "note"),
    attr(q(table_multiresponse(d, select = c(m1, m2), by = g)), "note")
  )
  expect_false(any(grepl("chi-square(?!d)", notes, perl = TRUE)))
  expect_true(all(grepl("chi-squared", notes, fixed = TRUE)))
})

test_that("every table of the family keeps the title it prints as `title`", {
  skip_if_not_installed("survey")
  d <- .fn_data()
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  q <- function(e) suppressMessages(suppressWarnings(e))
  for (lang in c("en", "fr")) {
    withr::local_options(spicy.language = lang)
    tables <- list(
      table_categorical = q(table_categorical(d, select = xc, by = g)),
      table_categorical_noby = q(table_categorical(d, select = xc)),
      table_categorical_svy = q(table_categorical_svy(
        des,
        select = xc,
        by = g
      )),
      table_continuous = q(table_continuous(d, select = xn, by = g)),
      table_continuous_noby = q(table_continuous(d, select = xn)),
      table_continuous_svy = q(table_continuous_svy(des, select = xn, by = g)),
      table_continuous_lm = q(table_continuous_lm(d, select = xn, by = g)),
      table_outcome = q(table_outcome(d, outcome = xn, select = g)),
      table_multiresponse = q(table_multiresponse(
        d,
        select = c(m1, m2),
        by = g
      )),
      table_regression = q(table_regression(stats::lm(xn ~ age, data = d))),
      table_regression_uv = q(table_regression_uv(
        d,
        outcome = xn,
        predictors = c(age, g)
      ))
    )
    for (fn in names(tables)) {
      x <- tables[[fn]]
      title <- attr(x, "title")
      expect_true(
        is.character(title) && length(title) == 1L,
        info = paste(lang, fn)
      )
      out <- utils::capture.output(print(x))
      expect_identical(
        out[nzchar(trimws(out))][[1L]],
        title,
        info = paste(lang, fn)
      )
    }
  }
})

test_that("every engine sets the stars legend on a line of its own", {
  skip_if_not_installed("gt")
  skip_if_not_installed("flextable")
  skip_if_not_installed("tinytable")
  fit <- stats::lm(mpg ~ wt + hp, data = mtcars)
  build <- function(o, ...) {
    table_regression(fit, stars = TRUE, output = o, ...)
  }
  lines <- strsplit(attr(build("default"), "note"), "\n", fixed = TRUE)[[1L]]
  expect_length(lines, 2L)
  # gt: one source note per line, the legend a row of its own.
  g <- build("gt")
  expect_length(g[["_source_notes"]], 2L)
  # The HTML display path lifts the note out of the table: one line break,
  # before the legend, and the two source-note rows gone from the table.
  html <- .spicy_gt_html_postprocess(
    as.character(gt::as_raw_html(g, inline_css = FALSE)),
    attr(g, "spicy_note")
  )
  div <- regmatches(
    html,
    regexpr("<div class=\"spicy-gt-note\".*?</div>", html, perl = TRUE)
  )
  expect_identical(lengths(gregexpr("<br>", div, fixed = TRUE)), 1L)
  expect_match(div, "<br>*** p &lt; .001", fixed = TRUE)
  expect_no_match(html, "<tr class=\"gt_sourcenotes\">", fixed = TRUE)
  # flextable, and the Word file it feeds: one footer line per line.
  ft <- build("flextable")
  expect_identical(nrow(ft$footer$dataset), 2L)
  # The markdown grid of tinytable (a Quarto or R Markdown Word document):
  # the legend opens a row of its own.
  md <- strsplit(
    tinytable::save_tt(build("tinytable"), output = "markdown"),
    "\n",
    fixed = TRUE
  )[[1L]]
  at <- grep("***", md, fixed = TRUE)[[1L]]
  expect_true(startsWith(md[[at - 1L]], "+"))
  # A note of one line stays one row everywhere.
  plain <- function(o) {
    table_categorical(mtcars, select = cyl, by = am, output = o)
  }
  expect_length(plain("gt")[["_source_notes"]], 1L)
  expect_identical(nrow(plain("flextable")$footer$dataset), 1L)
})

test_that("every table function refuses data with no rows, first", {
  # Data with no rows leaves nothing to tabulate, with or without `by` and
  # whatever `drop_na`: one error, raised by the data gate of each
  # function before any selection or role check -- a column that does not
  # exist, or `by` named in `select`, is not what the caller hears about.
  # A survey twin refuses a design with no rows the same way.
  # `table_regression()` takes fitted models, not data.
  d <- data.frame(
    y = as.numeric(1:20),
    f = factor(rep(c("u", "v"), 10)),
    q1 = rep(c(TRUE, FALSE), 10),
    q2 = rep(c(FALSE, TRUE), 10),
    g = factor(rep(c("a", "b"), 10)),
    w = 1
  )
  d0 <- d[0, ]
  check <- function(cases, what) {
    msgs <- character(0)
    for (cl in cases) {
      info <- paste(deparse(cl), collapse = " ")
      err <- tryCatch(eval(cl), error = identity)
      expect_s3_class(err, "spicy_invalid_data")
      expect_identical(
        rlang::call_name(conditionCall(err)),
        as.character(cl[[1L]]),
        info = info
      )
      msg <- conditionMessage(err)
      expect_match(msg, sprintf("`%s` has no rows.", what), fixed = TRUE)
      msgs <- c(msgs, msg)
    }
    # One message, whatever the function and its arguments.
    expect_length(unique(msgs), 1L)
    msgs[[1L]]
  }
  msg_data <- check(
    alist(
      table_categorical(d0, select = f, by = g),
      table_categorical(d0, select = f, drop_na = TRUE),
      table_categorical(d0, select = no_such_column),
      table_continuous(d0, select = y, by = g, drop_na = FALSE),
      table_continuous(d0, select = c(y, g), by = g),
      table_continuous_lm(d0, select = y, by = g),
      table_outcome(d0, y, select = c(g, f)),
      table_multiresponse(d0, select = c(q1, q2)),
      table_multiresponse(d0, select = c(q1, q2), by = g),
      table_regression_uv(d0, outcome = y, predictors = c(f, g))
    ),
    "data"
  )
  skip_if_not_installed("survey")
  des0 <- survey::svydesign(ids = ~1, weights = ~w, data = d)[integer(0), ]
  msg_design <- check(
    alist(
      table_categorical_svy(des0, select = f, by = g),
      table_continuous_svy(des0, select = y),
      table_continuous_svy(des0, select = no_such_column)
    ),
    "design"
  )
  # The same sentence opens both.
  expect_identical(
    sub("\n.*", "", msg_design),
    sub("\n.*", "", msg_data)
  )
})

# ---- The abbreviations of the note (ADR of 2026-10-20) ---------------------
#
# Every table of the family defines the abbreviations its headers, its
# spanners and its stub show (the fit-statistic and random-effect rows of
# a regression), never the symbols of its cells, under the rule of the
# style in force (dev/decisions/2026-10-20-note-
# abbreviations.md). The exemptions below are the specification, written
# here a second time and on purpose: the registry of R/table_helpers.R must
# agree with them, theme by theme.
#
#   apa     the symbols that represent statistics (Publication Manual,
#           section 6.44, an "e.g." list, which Mdn and Hedges' g join;
#           section 7.15 adds SE), the Greek letters, and the symbols its
#           sample tables leave undefined (B, beta, R2, d);
#   jama    SD, SE and CI (AMA Manual of Style 11, section 13.11) and the
#           symbols of section 19.6 that need no expansion, B and M
#           included (section 19.6 writes them b and x-bar);
#   lancet  SD, SE, IQR and CI (model tables and published tables).
#
# n, N, p and the test statistics (t, z, F, chi-squared) have no simple
# definition at all: no rule defines them, "all" included.
.fa_exempt <- list(
  apa = c(
    "mean",
    "median",
    "sd",
    "se",
    "or",
    "b",
    "beta",
    "r2",
    "pseudo_r2",
    "adj_r2",
    "d",
    "g",
    "delta",
    "change",
    "eta2",
    "omega2",
    "sigma",
    "sigma2",
    "sigma_hat",
    "rho"
  ),
  jama = c(
    "mean",
    "b",
    "sd",
    "se",
    "ci",
    "delta",
    "change",
    "r2",
    "pseudo_r2",
    "adj_r2",
    "d",
    "sigma",
    "sigma2",
    "sigma_hat",
    "rho"
  ),
  lancet = c("sd", "se", "iqr", "ci")
)
# Abbreviated words, defined under "all" only; and the concepts whose
# definition always says more than the symbol.
.fa_words <- c("min", "max")
.fa_says_more <- c("deff", "pd", "mcse")

# The block a note must carry, computed from the specification above: the
# symbols and the terms are the registry's words, in the language in force.
.fa_expected <- function(items, rule, lever = NULL) {
  reg <- .abbrev_registry()
  ids <- sym <- term <- character(0)
  for (it in items) {
    if (is.character(it)) {
      it <- list(id = it)
    }
    says_more <- !is.null(it$term) || it$id %in% .fa_says_more
    if (!says_more) {
      if (identical(lever, "none")) {
        next
      }
      if (
        is.null(lever) &&
          (it$id %in% .fa_exempt[[rule]] || it$id %in% .fa_words)
      ) {
        next
      }
    }
    s <- it$symbol %||% spicy_str(reg[[it$id]]$symbol)
    t <- it$term %||% spicy_str(reg[[it$id]]$term)
    if (identical(tolower(s), tolower(t)) || any(ids == it$id & sym == s)) {
      next
    }
    ids <- c(ids, it$id)
    sym <- c(sym, s)
    term <- c(term, t)
  }
  if (!length(sym)) {
    return(NULL)
  }
  if (rule != "apa") {
    o <- order(tolower(sym), method = "radix")
    sym <- sym[o]
    term <- term[o]
  }
  sep <- spicy_str("note_abbrev_sep")
  switch(
    rule,
    apa = paste0(paste(paste(sym, "=", term), collapse = sep), "."),
    jama = spicy_fmt(
      if (length(sym) == 1L) {
        "note_abbrev_lead_one"
      } else {
        "note_abbrev_lead_many"
      },
      paste(paste0(sym, ", ", term), collapse = sep)
    ),
    lancet = paste(paste0(sym, "=", term, "."), collapse = " ")
  )
}

# One table of each function, with the concepts its headers show, in the
# order of its columns.
.fa_cases <- function(d, des) {
  q <- function(e) suppressMessages(suppressWarnings(e))
  iqr <- .abbr(
    "iqr",
    term = spicy_fmt(
      "abbrev_iqr_width",
      spicy_str("header_q3"),
      spicy_str("header_q1")
    )
  )
  list(
    table_categorical = list(
      q(table_categorical(
        d,
        select = c(xc, g),
        by = g2,
        assoc_ci = TRUE,
        smd = TRUE
      )),
      list("ci", "ll", "ul", "smd")
    ),
    table_categorical_svy = list(
      q(table_categorical_svy(
        des,
        select = xc,
        by = g2,
        proportion_ci = TRUE,
        deff = TRUE
      )),
      list("ci", "ll", "ul", "deff")
    ),
    table_continuous = list(
      q(table_continuous(
        d,
        select = c(xn, age),
        by = g2,
        show_columns = c(
          "m",
          "sd",
          "med",
          "iqr",
          "med_iqr",
          "min",
          "max",
          "ci",
          "n"
        ),
        effect_size = "hedges_g",
        smd = TRUE
      )),
      list(
        "mean",
        "sd",
        "median",
        iqr,
        "median",
        "q1",
        "q3",
        "min",
        "max",
        "ci",
        "ll",
        "ul",
        "es",
        "smd"
      )
    ),
    table_continuous_svy = list(
      q(table_continuous_svy(
        des,
        select = xn,
        by = g2,
        show_columns = c("m", "se", "ci", "n", "deff"),
        deff = TRUE
      )),
      list(
        "mean",
        .abbr("se", term = spicy_str("abbrev_se_design")),
        "ci",
        "ll",
        "ul",
        "deff"
      )
    ),
    table_continuous_lm = list(
      q(table_continuous_lm(
        d,
        select = xn,
        by = g2,
        statistic = TRUE,
        effect_size = "cohens_d",
        r2 = "adj_r2"
      )),
      # The t of the statistic column is a test statistic: never defined.
      list("mean", "delta", "ci", "ll", "ul", "adj_r2", "d")
    ),
    table_outcome = list(
      q(table_outcome(
        d,
        outcome = xn,
        select = g2,
        show_columns = c("m", "iqr", "med_iqr", "n"),
        effect_size = "auto"
      )),
      list("mean", iqr, "median", "q1", "q3", "es")
    ),
    table_multiresponse = list(
      q(table_multiresponse(d, select = c(m1, m2), by = g2)),
      list()
    ),
    table_regression = list(
      q(table_regression(
        stats::glm(y01 ~ age + g2, data = d, family = stats::binomial()),
        exponentiate = TRUE,
        show_columns = c("b", "se", "ci", "t", "p", "partial_chi2")
      )),
      # The headers (the z column is a test statistic), then the stub:
      # n, the two pseudo-R2, each under the label of its row, and the AIC.
      list(
        "or",
        "se",
        "ci",
        .abbr("chi2", term = spicy_str("abbrev_chi2_lrt")),
        .abbr("pseudo_r2", symbol = fit_stat_label("pseudo_r2_mcfadden")),
        .abbr("pseudo_r2", symbol = fit_stat_label("pseudo_r2_nagelkerke")),
        "aic"
      )
    ),
    table_regression_mixed = list(
      q(table_regression(lme4::lmer(xn ~ age + (1 | g), data = d))),
      # The random-effects rows (sigma), then n, N (g), the ICC, the two
      # R2 and the information criteria.
      list("b", "se", "ci", "sigma", "icc", "r2", "aic", "bic")
    ),
    table_regression_uv = list(
      q(table_regression_uv(
        d,
        outcome = y01,
        predictors = c(age, xc),
        method = "glm",
        exponentiate = TRUE
      )),
      list(
        "or",
        "ci",
        .abbr("pseudo_r2", symbol = fit_stat_label("pseudo_r2_mcfadden")),
        .abbr("pseudo_r2", symbol = fit_stat_label("pseudo_r2_nagelkerke")),
        "aic"
      )
    )
  )
}

# The rules and the values of the lever, as a user sets them.
.fa_settings <- list(
  default = list(style = NULL, rule = "apa", lever = NULL),
  apa = list(style = "apa", rule = "apa", lever = NULL),
  nejm = list(style = "nejm", rule = "apa", lever = NULL),
  jama = list(style = "jama", rule = "jama", lever = NULL),
  lancet = list(style = "lancet", rule = "lancet", lever = NULL),
  all = list(
    style = spicy_style(abbreviations = "all"),
    rule = "apa",
    lever = "all"
  ),
  none = list(
    style = spicy_style(abbreviations = "none"),
    rule = "apa",
    lever = "none"
  ),
  jama_none = list(
    style = spicy_style("jama", abbreviations = "none"),
    rule = "jama",
    lever = "none"
  ),
  lancet_all = list(
    style = spicy_style("lancet", abbreviations = "all"),
    rule = "lancet",
    lever = "all"
  )
)

test_that("every note defines the abbreviations of its headers, as its rule asks", {
  skip_if_not_installed("survey")
  skip_if_not_installed("lme4")
  d <- .fn_data()
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  for (lang in c("en", "fr")) {
    withr::local_options(spicy.language = lang)
    for (sn in names(.fa_settings)) {
      s <- .fa_settings[[sn]]
      withr::local_options(spicy.style = s$style)
      cases <- .fa_cases(d, des)
      for (fn in names(cases)) {
        info <- paste(lang, sn, fn)
        note <- attr(cases[[fn]][[1L]], "note")
        lines <- strsplit(note, "\n", fixed = TRUE)[[1L]]
        expected <- .fa_expected(cases[[fn]][[2L]], s$rule, s$lever)
        leads <- c(
          sub("%s.", "", spicy_str("note_abbrev_lead_one"), fixed = TRUE),
          sub("%s.", "", spicy_str("note_abbrev_lead_many"), fixed = TRUE)
        )
        if (is.null(expected)) {
          # Nothing to define, and no empty block.
          expect_false(any(startsWith(lines, leads[[1L]])), info = info)
          expect_false(any(startsWith(lines, leads[[2L]])), info = info)
          next
        }
        if (identical(s$rule, "jama")) {
          # The first line of the note, the rest after it.
          expect_identical(lines[[1L]], expected, info = info)
          expect_gt(length(lines), 1L)
        } else {
          # The end of the general note, before the stars legend.
          expect_true(endsWith(lines[[1L]], expected), info = info)
        }
        # Each definition once, nowhere else in the note.
        pairs <- if (identical(s$rule, "apa")) {
          strsplit(
            sub("[.]$", "", expected),
            spicy_str("note_abbrev_sep"),
            fixed = TRUE
          )[[1L]]
        } else if (identical(s$rule, "jama")) {
          body <- sub("^[^:]*:[^ ]* ", "", sub("[.]$", "", expected))
          strsplit(body, spicy_str("note_abbrev_sep"), fixed = TRUE)[[1L]]
        } else {
          regmatches(expected, gregexpr("[^ ][^=]*=[^.]*[.]", expected))[[1L]]
        }
        for (p in pairs) {
          expect_identical(
            lengths(regmatches(note, gregexpr(p, note, fixed = TRUE))),
            1L,
            info = paste(info, p)
          )
        }
      }
    }
  }
})

test_that("n, N, p and the test statistics are never defined, and what says more always is", {
  skip_if_not_installed("survey")
  d <- .fn_data()
  for (lang in c("en", "fr")) {
    withr::local_options(spicy.language = lang)
    sep <- spicy_str("note_abbrev_sep")
    pair <- function(sym) paste0(sym, " = ")
    # "all" defines every abbreviation shown, Min and Max included, but no
    # count and no p-value, though every table below shows them.
    withr::local_options(spicy.style = spicy_style(abbreviations = "all"))
    tc <- suppressWarnings(table_continuous(d, select = xn, by = g2))
    note <- attr(tc, "note")
    expect_match(note, pair(spicy_str("header_min")), fixed = TRUE)
    for (sym in c(spicy_str("header_n_lower"), spicy_str("header_p"))) {
      expect_no_match(note, paste0(sep, pair(sym)), fixed = TRUE)
      expect_no_match(note, paste0(". ", pair(sym)), fixed = TRUE)
    }
    uv <- suppressMessages(suppressWarnings(table_regression_uv(
      d,
      outcome = y01,
      predictors = c(age, xc),
      method = "glm"
    )))
    expect_no_match(
      attr(uv, "note"),
      pair(spicy_str("header_n_upper")),
      fixed = TRUE
    )
    # Nor a test statistic: the t and z columns of a regression, the F of
    # a linear-model table. Its fit statistics are defined.
    reg <- suppressMessages(table_regression(
      stats::glm(y01 ~ age + g2, data = d, family = stats::binomial()),
      show_columns = c("b", "se", "ci", "t", "p")
    ))
    lm3 <- suppressWarnings(table_continuous_lm(
      d,
      select = xn,
      by = g,
      statistic = TRUE
    ))
    for (x in list(reg, lm3)) {
      for (sym in c("symbol_t", "symbol_z", "symbol_f", "symbol_chi_sq")) {
        expect_no_match(attr(x, "note"), pair(spicy_str(sym)), fixed = TRUE)
      }
    }
    expect_match(
      attr(reg, "note"),
      paste(spicy_str("fitstat_aic"), "=", spicy_str("abbrev_aic")),
      fixed = TRUE
    )
    # "none" keeps the IQR as the width Q3 - Q1, and the partial
    # eta-squared of a regression: they say how the number was computed.
    withr::local_options(spicy.style = spicy_style(abbreviations = "none"))
    tc <- suppressWarnings(table_continuous(
      d,
      select = xn,
      by = g2,
      show_columns = c("m", "iqr")
    ))
    expect_match(
      attr(tc, "note"),
      paste(
        spicy_str("header_iqr"),
        "=",
        spicy_fmt(
          "abbrev_iqr_width",
          spicy_str("header_q3"),
          spicy_str("header_q1")
        )
      ),
      fixed = TRUE
    )
    expect_no_match(
      attr(tc, "note"),
      pair(spicy_str("header_mean")),
      fixed = TRUE
    )
    reg <- suppressMessages(table_regression(
      stats::lm(xn ~ age + g2, data = d),
      show_columns = c("b", "ci", "p", "partial_eta2")
    ))
    expect_match(
      attr(reg, "note"),
      paste(
        spicy_str("symbol_eta_sq_partial"),
        "=",
        spicy_str("abbrev_eta2_partial")
      ),
      fixed = TRUE
    )
    expect_no_match(
      attr(reg, "note"),
      pair(spicy_str("header_ci_label_confidence")),
      fixed = TRUE
    )
  }
})

test_that("a pseudo-R2 row is defined as a pseudo coefficient of determination", {
  logit <- stats::glm(am ~ wt, data = mtcars, family = stats::binomial())
  ols <- stats::lm(mpg ~ wt, data = mtcars)
  note <- function(x, ...) {
    attr(suppressMessages(table_regression(x, style = "lancet", ...)), "note")
  }
  # Under the label of its row, which names its author, alphabetical.
  expect_true(endsWith(
    note(logit),
    paste(
      "AIC=Akaike information criterion.",
      "B=unstandardized regression coefficient.",
      "R\u00B2 (McFadden)=pseudo coefficient of determination.",
      "R\u00B2 (Nagelkerke)=pseudo coefficient of determination."
    )
  ))
  # The R2 of a linear model keeps its definition beside them.
  both <- note(list(ols, logit))
  expect_match(
    both,
    "R\u00B2=coefficient of determination. R\u00B2 (McFadden)=pseudo",
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  expect_match(
    note(logit),
    "R\u00B2 (Nagelkerke)=pseudo-coefficient de d\u00E9termination.",
    fixed = TRUE
  )
})

test_that("a labelled grouping names its groups by their value labels, in every table", {
  skip_if_not_installed("haven")
  skip_if_not_installed("survey")
  d <- data.frame(
    y = c(1.2, 2.3, 3.1, 4.8, 5.5, 6.1, 2.2, 3.3),
    f = factor(c("a", "b", "a", "b", "a", "b", "a", "b")),
    m1 = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
    m2 = c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE),
    w = 1
  )
  # Labels declared in another order than the codes, one of them held by
  # nobody: the groups are the observed codes, named and ordered as the
  # rows of a labelled variable are -- every observed value carries a
  # label, so in the order of the `labels` attribute. They were headed by
  # the codes, "1" and "2", `labels = TRUE` or not.
  d$g <- haven::labelled(
    c(1, 2, 1, 2, 1, 2, 2, 1),
    labels = c(Men = 2, Women = 1, Other = 3)
  )
  groups <- c("[2] Men", "[1] Women")
  long <- function(fn, ...) fn(..., output = "long")
  grp <- function(x) unique(x$group[x$.group_role %in% "group"])
  expect_identical(grp(long(table_categorical, d, select = f, by = g)), groups)
  expect_identical(grp(long(table_continuous, d, select = y, by = g)), groups)
  expect_identical(
    grp(long(table_multiresponse, d, select = c(m1, m2), by = g)),
    groups
  )
  expect_identical(
    long(table_continuous_lm, d, select = y, by = g)$level,
    groups
  )
  out <- long(table_outcome, d, outcome = y, select = g)
  expect_identical(out$level[out$.row_role == "level"], groups)
  des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  expect_identical(
    grp(long(table_categorical_svy, des, select = f, by = g)),
    groups
  )
  expect_identical(
    grp(long(table_continuous_svy, des, select = y, by = g)),
    groups
  )
  # The headers the reader sees, under any `labels`.
  for (lab in c(FALSE, TRUE)) {
    expect_true(
      all(
        c("[1] Women n", "[2] Men n") %in%
          names(table_categorical(
            d,
            select = f,
            by = g,
            labels = lab,
            output = "data.frame"
          ))
      ),
      info = lab
    )
  }
  # The first group is the reference of the model.
  expect_true(
    "\u0394 ([1] Women - [2] Men)" %in%
      names(table_continuous_lm(d, select = y, by = g, output = "data.frame"))
  )
  # The rows of the same column, as a variable, read the same, in the
  # same order.
  rows <- table_categorical(d, select = g, output = "long")$level
  expect_identical(rows[rows %in% groups], groups)
  # An observed value with no label: ascending codes, for the groups and
  # for the rows.
  d$g2 <- haven::labelled(
    c(1, 2, 1, 4, 1, 2, 4, 1),
    labels = c(Men = 2, Women = 1)
  )
  coded <- c("[1] Women", "[2] Men", "[4] 4")
  expect_identical(grp(long(table_categorical, d, select = f, by = g2)), coded)
  expect_identical(
    unique(table_categorical(d, select = g2, output = "long")$level),
    coded
  )
})

test_that("every table that takes weights says the rows a missing weight removed", {
  # One disclosure, in the note, and no warning: the rule of the rows a
  # missing `by` removes. table_categorical() warned instead, once per
  # internal tabulation.
  d <- sochealth
  d$weight[3L] <- NA
  notes <- list(
    categorical = table_categorical(
      d,
      select = smoking,
      by = sex,
      weights = weight
    ),
    continuous = table_continuous(
      d,
      select = bmi,
      by = sex,
      weights = weight,
      p_value = FALSE
    ),
    continuous_lm = table_continuous_lm(
      d,
      select = bmi,
      by = sex,
      weights = weight
    ),
    outcome = table_outcome(
      d,
      outcome = bmi,
      select = sex,
      weights = weight,
      p_value = FALSE
    )
  )
  for (fn in names(notes)) {
    expect_match(
      attr(notes[[fn]], "note"),
      "Rows with missing weight removed: 1.",
      fixed = TRUE,
      info = fn
    )
  }
  expect_no_warning(
    table_categorical(d, select = smoking, by = sex, weights = weight)
  )
  expect_no_warning(
    table_continuous_lm(d, select = bmi, by = sex, weights = weight)
  )
})

test_that("no builder rules off a row that holds the word \"Total\"", {
  # Given no index of a total row, the console printer looked for the word
  # "Total" in the rows and drew the rule of a total row above the one row
  # that held it. The heavy rule is U+2500; block separators are dashed.
  set.seed(16)
  n <- 60L
  d <- data.frame(
    g = factor(sample(c("u", "v"), n, replace = TRUE)),
    x = factor(
      sample(c("a", "Total score"), n, replace = TRUE),
      levels = c("a", "Total score")
    ),
    a = rnorm(n),
    v = rnorm(n),
    i1 = sample(c(TRUE, FALSE), n, replace = TRUE),
    i2 = sample(c(TRUE, FALSE), n, replace = TRUE),
    w = runif(n, 1, 2)
  )
  lab <- c(v = "Total score", i2 = "Total score")
  q <- function(e) suppressMessages(suppressWarnings(e))
  tabs <- list(
    table_categorical = q(table_categorical(d, x, by = g)),
    table_continuous = q(table_continuous(d, c(a, v), labels = lab)),
    table_continuous_lm = q(table_continuous_lm(
      d,
      c(a, v),
      by = g,
      labels = lab
    )),
    table_outcome = q(table_outcome(d, outcome = a, select = x)),
    table_multiresponse = q(table_multiresponse(d, c(i1, i2), labels = lab))
  )
  if (
    requireNamespace("survey", quietly = TRUE) &&
      utils::packageVersion("survey") >= "4.5"
  ) {
    des <- survey::svydesign(ids = ~1, weights = ~w, data = d)
    tabs$table_categorical_svy <- q(table_categorical_svy(des, x))
    tabs$table_continuous_svy <- q(table_continuous_svy(
      des,
      c(a, v),
      labels = lab
    ))
  }
  for (fn in names(tabs)) {
    out <- utils::capture.output(print(tabs[[fn]]))
    at <- grep("Total score", out, fixed = TRUE)
    expect_true(length(at) > 0L, info = fn)
    expect_false(any(grepl("─", out[at - 1L])), info = fn)
  }
})
