# ---------------------------------------------------------------------------
# info$vcov_kind: one canonical vocabulary (register n. 228, plan item C1).
#
# About twenty-five builders wrote "model" and four wrote "classical" for
# the same thing -- the variance the fit itself reports -- so every
# consumer carried the ambiguity as a literal disjunction in fourteen
# files, and two consumers in ONE file disagreed about which spelling a
# missing value defaults to. The synonym is normalised out of the FRAME,
# not out of the user's vocabulary: `vcov = "classical"` is still what a
# user types.
# ---------------------------------------------------------------------------

test_that(".canon_vcov_kind normalises the synonym and the absence", {
  expect_identical(.canon_vcov_kind("classical"), "model")
  expect_identical(.canon_vcov_kind("model"), "model")
  expect_identical(.canon_vcov_kind(NULL), "model")
  expect_identical(.canon_vcov_kind(NA_character_), "model")
  expect_identical(.canon_vcov_kind(character(0)), "model")
  expect_identical(.canon_vcov_kind(c("HC1", "HC2")), "model")
  # Everything else passes through untouched -- an unknown token is the
  # validator's business, not the accessor's.
  for (tok in c(
    "HC3",
    "CR2",
    "CR1S",
    "bootstrap",
    "jackknife",
    "robust",
    "posterior",
    "survey-Taylor",
    "nid",
    "weirdtype"
  )) {
    expect_identical(.canon_vcov_kind(tok), tok, info = tok)
  }
})

test_that(".is_model_vcov answers for both spellings and for nothing", {
  expect_true(.is_model_vcov("model"))
  expect_true(.is_model_vcov("classical"))
  expect_true(.is_model_vcov(NULL))
  expect_false(.is_model_vcov("HC3"))
  expect_false(.is_model_vcov("survey-Taylor"))
  expect_false(.is_model_vcov("posterior"))
})

test_that(".frame_vcov_kind reads a hand-built frame through the canon", {
  # Frames built by older code or by hand never pass the constructor,
  # which is why the READ side normalises too.
  expect_identical(
    .frame_vcov_kind(list(info = list(vcov_kind = "classical"))),
    "model"
  )
  expect_identical(.frame_vcov_kind(list(info = list())), "model")
  expect_identical(
    .frame_vcov_kind(list(info = list(vcov_kind = "HC1"))),
    "HC1"
  )
})

test_that("a frame carries the canon whichever spelling the builder took", {
  fits <- list(
    lm = stats::lm(mpg ~ wt, data = mtcars),
    glm = stats::glm(am ~ wt, data = mtcars, family = stats::binomial)
  )
  for (nm in names(fits)) {
    expect_identical(
      as_regression_frame(fits[[nm]])$info$vcov_kind,
      "model",
      info = nm
    )
    # ...including when the user asked for "classical" by name.
    expect_identical(
      as_regression_frame(fits[[nm]], vcov = "classical")$info$vcov_kind,
      "model",
      info = nm
    )
  }
  # A robust request is untouched by the canon.
  expect_identical(
    as_regression_frame(fits$lm, vcov = "HC3")$info$vcov_kind,
    "HC3"
  )
})

test_that("the canonicalisation sits at the constructor boundary", {
  fr <- as_regression_frame(stats::lm(mpg ~ wt, data = mtcars))
  info <- fr$info
  info$vcov_kind <- "classical"
  rebuilt <- new_regression_frame(fr$coefs, info, attr(fr, "fit"))
  expect_identical(rebuilt$info$vcov_kind, "model")
})

test_that("the frame validator asserts the vcov vocabulary", {
  fr <- as_regression_frame(stats::lm(mpg ~ wt, data = mtcars))
  expect_invisible(validate_regression_frame(fr))
  # The historical spelling stays VALID input: it reads back as the canon.
  ok <- fr
  ok$info$vcov_kind <- "classical"
  expect_invisible(validate_regression_frame(ok))
  bad <- fr
  bad$info$vcov_kind <- "HC7"
  err <- expect_error(
    validate_regression_frame(bad),
    class = "spicy_invalid_frame"
  )
  expect_match(conditionMessage(err), "vcov_kind", fixed = TRUE)
  expect_match(conditionMessage(err), "not a recognised value", fixed = TRUE)
  expect_match(conditionMessage(err), "Allowed:", fixed = TRUE)
})

test_that("the frame vocabulary covers every value a builder can write", {
  kinds <- .frame_vcov_kinds()
  expect_true(all(
    c(
      "model",
      paste0("HC", 0:5),
      "HC4m",
      paste0("CR", 0:3),
      "CR1S",
      "bootstrap",
      "jackknife",
      "nid",
      "iid",
      "ker",
      "rank",
      "robust",
      "posterior",
      "survey-Taylor"
    ) %in%
      kinds
  ))
  # "classical" is not IN the vocabulary; it normalises INTO it.
  expect_false("classical" %in% kinds)
  expect_true(.canon_vcov_kind("classical") %in% kinds)
})

# C1 point 5: the plumbing changes, the rendered labels do not.
test_that("the rendered variance label is unchanged by the canon", {
  m_lm <- stats::lm(mpg ~ wt, data = mtcars)
  m_glm <- stats::glm(am ~ wt, data = mtcars, family = stats::binomial)
  expect_identical(
    format_vcov_label_from_frame(as_regression_frame(m_lm)),
    spicy_str("note_vcov_classical_lm")
  )
  expect_identical(
    format_vcov_label_from_frame(as_regression_frame(m_glm)),
    spicy_str("note_vcov_classical_glm")
  )
  # A frame still carrying the historical spelling renders identically.
  fr <- as_regression_frame(m_lm)
  fr$info$vcov_kind <- "classical"
  expect_identical(
    format_vcov_label_from_frame(fr),
    spicy_str("note_vcov_classical_lm")
  )
  # A class that reaches the derivation without an engine label keeps the
  # token.
  expect_identical(
    format_vcov_label_from_frame(
      list(info = list(class = "coxph", vcov_kind = "model", extras = list()))
    ),
    "model"
  )
  # The historical spelling, on that same hand-built frame, is where the
  # class guard CHANGES the rendering: without it the frame entered the
  # lm/glm arm and came out "classical (OLS)", labelling a Cox partial
  # likelihood as ordinary least squares. No production frame can be in
  # this position -- every builder supplies a label, and the constructor
  # canonicalises the kind -- so the guard is pinned here, by hand.
  expect_identical(
    format_vcov_label_from_frame(
      list(
        info = list(
          class = "coxph",
          vcov_kind = "classical",
          extras = list()
        )
      )
    ),
    "model"
  )
  # And the robust arms are untouched.
  expect_identical(
    format_vcov_label_from_frame(as_regression_frame(m_lm, vcov = "HC3")),
    spicy_fmt("note_vcov_hc", "HC3")
  )
})

test_that(".vcov_label_from_kind keys on the canon", {
  expect_identical(.vcov_label_from_kind("classical"), "OLS")
  expect_identical(.vcov_label_from_kind("model"), "OLS")
  expect_identical(
    .vcov_label_from_kind("classical", is_glm = TRUE),
    "Model-based (asymptotic)"
  )
  expect_identical(
    .vcov_label_from_kind("model", is_glm = TRUE),
    "Model-based (asymptotic)"
  )
  expect_identical(
    .vcov_label_from_kind("HC3"),
    "HC3 heteroskedasticity-consistent"
  )
  # An unknown kind still falls back to its own name.
  expect_identical(.vcov_label_from_kind("weirdtype"), "weirdtype")
})
