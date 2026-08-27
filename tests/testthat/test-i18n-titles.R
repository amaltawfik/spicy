# Decision 42: the regression title and the type footer translate on
# the common families, through the registry templates and the language
# file's title-prefix bridge -- and keep their WHOLE English form when
# the prefix has no bridge entry (coherent or nothing). The English
# default is pinned byte-for-byte first: these templates must be
# invisible until a language asks for them.

.tf_lm_frame <- function(formula = mpg ~ wt) {
  as_regression_frame(lm(formula, data = mtcars))
}

test_that("the English titles are byte-identical to the pre-key literals", {
  f1 <- .tf_lm_frame()
  f2 <- .tf_lm_frame(mpg ~ wt + hp)
  expect_identical(
    build_regression_title_from_frames(list(f1)),
    "Linear regression: mpg"
  )
  expect_identical(
    build_regression_title_from_frames(list(f1, f2), nested = TRUE),
    "Hierarchical linear regression: mpg"
  )
  expect_identical(
    build_regression_title_from_frames(list(f1, f2)),
    "Linear regression comparison: mpg"
  )
  expect_identical(
    build_regression_type_footer_block_from_frames(list(f1)),
    "Linear regression."
  )
  expect_identical(
    build_regression_type_footer_block_from_frames(list(f1, f2)),
    "Linear regression models."
  )
})

test_that("the French titles come out French, NBSP colon included", {
  withr::local_options(spicy.language = "fr")
  f1 <- .tf_lm_frame()
  f2 <- .tf_lm_frame(mpg ~ wt + hp)
  expect_identical(
    build_regression_title_from_frames(list(f1)),
    "R\u00e9gression lin\u00e9aire\u00a0: mpg"
  )
  expect_identical(
    build_regression_title_from_frames(list(f1, f2), nested = TRUE),
    "R\u00e9gression lin\u00e9aire \u2014 mod\u00e8les hi\u00e9rarchiques\u00a0: mpg"
  )
  expect_identical(
    build_regression_title_from_frames(list(f1, f2)),
    "R\u00e9gression lin\u00e9aire \u2014 comparaison\u00a0: mpg"
  )
  f3 <- as_regression_frame(lm(disp ~ wt, data = mtcars))
  expect_identical(
    build_regression_title_from_frames(list(f1, f3)),
    "R\u00e9gression lin\u00e9aire \u2014 comparaison"
  )
  expect_identical(
    build_regression_title_from_frames(list()),
    "R\u00e9gression"
  )
})

test_that("the French type footer translates its two coherent arms", {
  withr::local_options(spicy.language = "fr")
  f1 <- .tf_lm_frame()
  f2 <- .tf_lm_frame(mpg ~ wt + hp)
  expect_identical(
    build_regression_type_footer_block_from_frames(list(f1)),
    "R\u00e9gression lin\u00e9aire."
  )
  expect_identical(
    build_regression_type_footer_block_from_frames(list(f1, f2)),
    "Mod\u00e8les de r\u00e9gression lin\u00e9aire."
  )
})

test_that("an unmapped family keeps its WHOLE English title under fr", {
  # Coherent or nothing: a half-translated head ("Heckman selection
  # model -- comparaison") would be worse than English. The prefix is
  # planted by hand so no Suggests package is needed.
  f1 <- .tf_lm_frame()
  f1$info$extras$title_prefix <- "Heckman selection model"
  en_title <- build_regression_title_from_frames(list(f1))
  en_note <- build_regression_type_footer_block_from_frames(list(f1))
  withr::local_options(spicy.language = "fr")
  expect_identical(build_regression_title_from_frames(list(f1)), en_title)
  expect_identical(
    build_regression_type_footer_block_from_frames(list(f1)),
    en_note
  )
})

test_that("the mixed-family table stays English wholesale under fr", {
  # Coherent or nothing at TABLE level: the mixed-family type footer is
  # the frozen English "Model k: ..." list, so the title must not
  # translate either -- even though the mixed fallback prefix
  # "Regression" has a bridge entry -- and the footer's line template
  # must be the English one (ASCII colon, not the French NBSP colon).
  f1 <- .tf_lm_frame()
  f2 <- .tf_lm_frame()
  f2$info$extras$title_prefix <- "Heckman selection model"
  en_title <- build_regression_title_from_frames(list(f1, f2))
  en_note <- build_regression_type_footer_block_from_frames(list(f1, f2))
  withr::local_options(spicy.language = "fr")
  expect_identical(
    build_regression_title_from_frames(list(f1, f2)),
    en_title
  )
  note <- build_regression_type_footer_block_from_frames(list(f1, f2))
  expect_identical(note, en_note)
  expect_match(note, "Model 1: ", fixed = TRUE)
  expect_false(grepl("\u00a0", note))
})

test_that("the five template keys accept a spicy.labels override in English", {
  # The registry advertises these keys, so they must not be dead
  # letters when no language is set: the English arms resolve through
  # .spicy_fmt_en(), which bypasses the language layer but keeps the
  # labels layer live.
  withr::with_options(
    list(spicy.labels = list(row_overall = "Sample")),
    # An override of an UNRELATED key leaves the templates on their
    # English defaults (the labels layer is consulted, misses, and
    # falls through).
    expect_identical(
      build_regression_title_from_frames(list(.tf_lm_frame())),
      "Linear regression: mpg"
    )
  )
  withr::local_options(
    spicy.labels = list(
      title_regression_single = "%s <<%s>>",
      title_regression_hierarchical = "Blocks of %s: %s",
      title_regression_comparison_dv = "%s versus: %s",
      title_regression_comparison = "%s (compared)",
      note_type_models = "%s family.",
      note_model_line = "%s => %s"
    )
  )
  g1 <- .tf_lm_frame()
  g2 <- .tf_lm_frame(mpg ~ wt + hp)
  g3 <- as_regression_frame(lm(disp ~ wt, data = mtcars))
  g3$info$extras$title_prefix <- "Heckman selection model"
  expect_identical(
    build_regression_title_from_frames(list(g1)),
    "Linear regression <<mpg>>"
  )
  expect_identical(
    build_regression_title_from_frames(list(g1, g2), nested = TRUE),
    "Blocks of linear regression: mpg"
  )
  expect_identical(
    build_regression_title_from_frames(list(g1, g2)),
    "Linear regression versus: mpg"
  )
  expect_identical(
    build_regression_title_from_frames(list(g1, g3)),
    "Regression (compared)"
  )
  expect_identical(
    build_regression_type_footer_block_from_frames(list(g1, g2)),
    "Linear regression family."
  )
  expect_match(
    build_regression_type_footer_block_from_frames(list(g1, g3)),
    "Model 1 => linear regression",
    fixed = TRUE
  )
})

test_that("an NA title_prefix does not crash the bridge", {
  f1 <- .tf_lm_frame()
  f1$info$extras$title_prefix <- NA_character_
  en_title <- build_regression_title_from_frames(list(f1))
  withr::local_options(spicy.language = "fr")
  expect_identical(build_regression_title_from_frames(list(f1)), en_title)
})

test_that("the hyphenated engine spellings reach their bridge entries", {
  # MASS::glm.nb and fixest both emit "Negative-binomial regression"
  # (hyphenated); glmmTMB emits "Negative-binomial mixed-effects
  # regression (glmmTMB)". Planted by hand so no Suggests package is
  # needed -- the strings are pinned to the engine sources by
  # test-regression grep-level tests, not here.
  withr::local_options(spicy.language = "fr")
  f1 <- .tf_lm_frame()
  f1$info$extras$title_prefix <- "Negative-binomial regression"
  expect_identical(
    build_regression_title_from_frames(list(f1)),
    "R\u00e9gression binomiale n\u00e9gative\u00a0: mpg"
  )
  f1$info$extras$title_prefix <-
    "Negative-binomial mixed-effects regression (glmmTMB)"
  expect_identical(
    build_regression_title_from_frames(list(f1)),
    paste0(
      "R\u00e9gression binomiale n\u00e9gative \u00e0 effets mixtes",
      " (glmmTMB)\u00a0: mpg"
    )
  )
})

test_that("an engine suffix survives the bridge as a proper noun", {
  skip_if_not_installed("glmmTMB")
  skip_on_cran()
  fit <- suppressWarnings(glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy
  ))
  fr <- withr::with_options(list(spicy.language = "fr"), {
    build_regression_title_from_frames(list(as_regression_frame(fit)))
  })
  expect_identical(
    fr,
    "R\u00e9gression lin\u00e9aire \u00e0 effets mixtes (glmmTMB)\u00a0: Reaction"
  )
})

test_that("a spicy.labels override reaches the title template", {
  withr::local_options(
    spicy.language = "fr",
    spicy.labels = list(title_regression_single = "%s [%s]")
  )
  f1 <- .tf_lm_frame()
  expect_identical(
    build_regression_title_from_frames(list(f1)),
    "R\u00e9gression lin\u00e9aire [mpg]"
  )
})

test_that("the public render carries the French title end to end", {
  withr::local_options(spicy.language = "fr")
  out <- paste(
    utils::capture.output(print(table_regression(lm(mpg ~ wt, mtcars)))),
    collapse = "\n"
  )
  expect_match(out, "R\u00e9gression lin\u00e9aire\u00a0: mpg", fixed = TRUE)
  expect_match(out, "Note. R\u00e9gression lin\u00e9aire.", fixed = TRUE)
})
