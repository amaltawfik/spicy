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
  "table_regression"
)

test_that("the four table builders share the reporting grammar", {
  # Shared formals promised by the reporting vignette. `digits` is
  # shared by the three numeric-cell functions; the categorical
  # table's cells are percentages, so it spells it `percent_digits`
  # (the vignette says so since lot T4).
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
    spicy_regression_table = table_regression(lm(mpg ~ wt, data = mtcars))
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
