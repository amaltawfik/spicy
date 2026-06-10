# ---------------------------------------------------------------------------
# Phase 0b sub-step 3 tests: .frame_to_legacy_extract() round-trip adapter.
#
# Proves that a frame produced by as_regression_frame.lm() / .glm(), when
# round-tripped through the adapter, yields a legacy-shaped extract whose
# downstream renderer output (title, full footer, per-block footer themes)
# is byte-identical to the output from extract_lm_phase1() directly.
#
# This is the substantive deliverable of sub-step 3: it proves the frame
# carries all the information the downstream renderers need. Without this
# guarantee, sub-step 4 cannot safely flip the call sites.
# ---------------------------------------------------------------------------


# ---- Shared helpers --------------------------------------------------------

# Build a legacy extract directly from the fit (the existing live path).
.legacy_extract <- function(fit, ...) {
  extract_lm_phase1(fit = fit, model_id = "M1", ...)
}

# Build a legacy extract via the new path: fit -> frame -> adapter -> extract.
.adapter_extract <- function(fit, ...) {
  frame <- as_regression_frame(fit, model_id = "M1", ...)
  spicy:::.frame_to_legacy_extract(frame, model_id = "M1")
}

# Compare two extract lists for byte-equivalence on the fields the
# downstream renderers read. We do not require ALL fields to match
# (e.g. test_type is dropped in the schema), but we require every field
# the title / footer / abbreviation builders touch.
.expect_extracts_renderer_equivalent <- function(a, b, info = NULL) {
  # Scalar metadata
  expect_identical(a$outcome,               b$outcome,               info = info)
  expect_identical(a$outcome_label,         b$outcome_label,         info = info)
  expect_identical(a$title_prefix,          b$title_prefix,          info = info)
  expect_identical(a$vcov_type,             b$vcov_type,             info = info)
  expect_identical(a$cluster_name,          b$cluster_name,          info = info)
  expect_identical(a$is_glm,                b$is_glm,                info = info)
  expect_identical(a$has_singular,          b$has_singular,          info = info)
  expect_identical(a$use_ame_satterthwaite, b$use_ame_satterthwaite, info = info)
  expect_identical(a$exp_applied,           b$exp_applied,           info = info)
  expect_identical(a$exp_header,            b$exp_header,            info = info)
  expect_identical(as.integer(a$nobs),      as.integer(b$nobs),      info = info)

  # coefs: the columns the footer / abbreviation builders read
  for (col in c("term", "estimate_type", "is_intercept", "is_reference",
                "p_value", "factor_term", "factor_level",
                "factor_level_pos")) {
    expect_equal(a$coefs[[col]], b$coefs[[col]], info = paste(info, "/", col))
  }
}


# ---- Fixtures --------------------------------------------------------------

.fixture_lm_simple <- function() {
  lm(wellbeing_score ~ age + sex + smoking, data = sochealth)
}

.fixture_lm_factor <- function() {
  d <- sochealth
  d$inc <- factor(
    as.character(d$income_group),
    levels = c("Low", "Lower middle", "Upper middle", "High")
  )
  lm(wellbeing_score ~ inc + sex, data = d)
}

.fixture_glm_logistic <- function() {
  d <- sochealth
  d$smoker_bin <- as.integer(d$smoking == "Yes")
  glm(smoker_bin ~ age + sex + physical_activity,
      data = d, family = binomial(link = "logit"))
}


# ---- Round-trip equivalence: scalar fields + coefs columns -----------------

test_that("lm: adapter preserves all renderer-read fields", {
  fit <- .fixture_lm_simple()
  legacy <- .legacy_extract(fit)
  adapted <- .adapter_extract(fit)
  .expect_extracts_renderer_equivalent(legacy, adapted, info = "lm simple")
})

test_that("lm with multi-level factor: adapter preserves factor metadata", {
  fit <- .fixture_lm_factor()
  legacy <- .legacy_extract(fit)
  adapted <- .adapter_extract(fit)
  .expect_extracts_renderer_equivalent(legacy, adapted, info = "lm factor")
})

test_that("glm logistic: adapter preserves family + exponentiate metadata", {
  fit <- .fixture_glm_logistic()
  legacy <- .legacy_extract(fit)
  adapted <- .adapter_extract(fit)
  .expect_extracts_renderer_equivalent(legacy, adapted, info = "glm logistic")
})

test_that("glm with exponentiate = TRUE: exp_applied + exp_header round-trip", {
  fit <- .fixture_glm_logistic()
  legacy  <- .legacy_extract(fit, exponentiate = TRUE)
  adapted <- .adapter_extract(fit, exponentiate = TRUE)
  expect_true(legacy$exp_applied)
  expect_identical(adapted$exp_applied, legacy$exp_applied)
  expect_identical(adapted$exp_header,  legacy$exp_header)
})

test_that("lm with HC3 vcov: vcov_type round-trips", {
  fit <- .fixture_lm_simple()
  legacy  <- .legacy_extract(fit, vcov_type = "HC3")
  adapted <- .adapter_extract(fit, vcov = "HC3")
  expect_identical(adapted$vcov_type, "HC3")
  expect_identical(adapted$vcov_type, legacy$vcov_type)
})


# ---- Byte-identical title -------------------------------------------------

test_that("build_regression_title() produces identical output through both paths", {
  fits <- list(
    .fixture_lm_simple(),
    .fixture_lm_factor(),
    .fixture_glm_logistic()
  )
  for (fit in fits) {
    legacy_extracts  <- list(.legacy_extract(fit))
    adapted_extracts <- list(.adapter_extract(fit))
    expect_identical(
      build_regression_title(adapted_extracts),
      build_regression_title(legacy_extracts),
      info = paste("class:", class(fit)[1])
    )
  }
})


# ---- Byte-identical footer (full) -----------------------------------------

test_that("build_regression_footer() produces identical output for lm", {
  fit <- .fixture_lm_simple()
  legacy_extracts  <- list(.legacy_extract(fit))
  adapted_extracts <- list(.adapter_extract(fit))
  expect_identical(
    build_regression_footer(
      adapted_extracts,
      show_columns = c("b", "se", "ci", "p")
    ),
    build_regression_footer(
      legacy_extracts,
      show_columns = c("b", "se", "ci", "p")
    )
  )
})

test_that("build_regression_footer() produces identical output for lm with multi-level factor", {
  fit <- .fixture_lm_factor()
  legacy_extracts  <- list(.legacy_extract(fit))
  adapted_extracts <- list(.adapter_extract(fit))
  expect_identical(
    build_regression_footer(
      adapted_extracts,
      show_columns = c("b", "se", "ci", "p")
    ),
    build_regression_footer(
      legacy_extracts,
      show_columns = c("b", "se", "ci", "p")
    )
  )
})

test_that("build_regression_footer() produces identical output for glm logistic with exponentiate", {
  fit <- .fixture_glm_logistic()
  legacy_extracts  <- list(.legacy_extract(fit, exponentiate = TRUE))
  adapted_extracts <- list(.adapter_extract(fit, exponentiate = TRUE))
  expect_identical(
    build_regression_footer(
      adapted_extracts,
      show_columns = c("b", "se", "ci", "p")
    ),
    build_regression_footer(
      legacy_extracts,
      show_columns = c("b", "se", "ci", "p")
    )
  )
})

test_that("build_regression_footer() produces identical output under HC3 vcov", {
  fit <- .fixture_lm_simple()
  legacy_extracts  <- list(.legacy_extract(fit, vcov_type = "HC3"))
  adapted_extracts <- list(.adapter_extract(fit, vcov = "HC3"))
  expect_identical(
    build_regression_footer(
      adapted_extracts,
      show_columns = c("b", "se", "ci", "p")
    ),
    build_regression_footer(
      legacy_extracts,
      show_columns = c("b", "se", "ci", "p")
    )
  )
})


# ---- Byte-identical per-block footer themes -------------------------------

test_that("each footer-block builder produces identical output through both paths", {
  fit <- .fixture_lm_factor()
  legacy <- list(.legacy_extract(fit))
  adapted <- list(.adapter_extract(fit))

  # Per-theme byte equality. If any block differs, we want a precise
  # error pointing at the offending block, not a single opaque mismatch
  # on the full footer.
  expect_identical(build_regression_type_footer_block(adapted),
                   build_regression_type_footer_block(legacy))
  expect_identical(build_vcov_footer_block(adapted),
                   build_vcov_footer_block(legacy))
  expect_identical(
    build_abbreviations_footer_block(c("b","se","ci","p"), adapted, "none"),
    build_abbreviations_footer_block(c("b","se","ci","p"), legacy,  "none")
  )
  expect_identical(
    build_ame_satterthwaite_footer_block(adapted, c("b","se","ci","p")),
    build_ame_satterthwaite_footer_block(legacy,  c("b","se","ci","p"))
  )
  expect_identical(build_exponentiate_footer_block(adapted),
                   build_exponentiate_footer_block(legacy))
  expect_identical(
    build_standardized_caveat_footer_block(adapted, "none"),
    build_standardized_caveat_footer_block(legacy,  "none")
  )
  expect_identical(build_singular_footer_block(adapted),
                   build_singular_footer_block(legacy))
  expect_identical(build_polynomial_contrasts_footer_block(adapted),
                   build_polynomial_contrasts_footer_block(legacy))
  expect_identical(
    build_reference_categories_footer_block(adapted, "footer"),
    build_reference_categories_footer_block(legacy,  "footer")
  )
})


# ---- Multi-model parity ---------------------------------------------------

test_that("two-model side-by-side: title + footer identical through both paths", {
  fit1 <- .fixture_lm_simple()
  fit2 <- .fixture_lm_factor()

  legacy <- list(
    extract_lm_phase1(fit1, model_id = "M1"),
    extract_lm_phase1(fit2, model_id = "M2")
  )
  adapted <- list(
    spicy:::.frame_to_legacy_extract(as_regression_frame(fit1, model_id = "M1"),
                                     model_id = "M1"),
    spicy:::.frame_to_legacy_extract(as_regression_frame(fit2, model_id = "M2"),
                                     model_id = "M2")
  )

  expect_identical(build_regression_title(adapted),
                   build_regression_title(legacy))
  expect_identical(
    build_regression_footer(adapted,
                            show_columns = c("b","se","ci","p")),
    build_regression_footer(legacy,
                            show_columns = c("b","se","ci","p"))
  )
})


# ---- fit_stats round-trip (legacy data.frame shape preserved) -------------

test_that("legacy fit_stats round-trips through the adapter (every numeric column)", {
  fit <- .fixture_lm_simple()
  legacy  <- .legacy_extract(fit)
  adapted <- .adapter_extract(fit)

  # Same column set (legacy keys)
  expect_identical(
    sort(names(adapted$fit_stats)),
    sort(names(legacy$fit_stats))
  )
  # Same numeric values for each common column
  for (col in setdiff(names(legacy$fit_stats), c("model_id", "outcome"))) {
    expect_equal(adapted$fit_stats[[col]], legacy$fit_stats[[col]],
                 tolerance = 1e-10,
                 info = paste("fit_stats column:", col))
  }
})
