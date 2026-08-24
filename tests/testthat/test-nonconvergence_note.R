# ---------------------------------------------------------------------------
# Non-convergence disclosure for glmmTMB fits.
#
# A glmmTMB fit that stopped before converging keeps its STARTING values
# (every coefficient at 1.00) and used to print them in full, in silence:
# the degeneracy guard removed the NaN uncertainty, but nothing said the
# optimizer never arrived. The table now carries a footer note and the
# build raises a classed warning (spicy_nonconvergence, under
# spicy_caveat). The estimates still print -- they are what the object
# holds, and the note says what they are worth.
#
# The criterion is glmmTMB's own, from finalizeTMB(): a non-zero
# fit$fit$convergence, or a non-positive-definite Hessian
# (isFALSE(fit$sdr$pdHess), read AFTER glmmTMB's own numDeriv rescue).
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_glmmTMB_nonconverged <- function() {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  # Deterministic: the optimizer is capped at one iteration and one
  # function evaluation, so it cannot reach a solution whatever the
  # platform's numerics.
  suppressWarnings(glmmTMB::glmmTMB(
    Reaction ~ Days + (1 | Subject),
    data = lme4::sleepstudy,
    control = glmmTMB::glmmTMBControl(
      optCtrl = list(iter.max = 1, eval.max = 1)
    )
  ))
}

.fit_glmmTMB_converged <- function() {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("lme4")
  glmmTMB::glmmTMB(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
}


# ---- 1. The fixture really does trip glmmTMB's own flags ----------------

test_that("the capped fit trips both of glmmTMB's convergence flags", {
  fit <- .fit_glmmTMB_nonconverged()
  # If either of these stops holding, the fixture no longer tests what
  # the note is keyed on.
  expect_false(identical(as.numeric(fit$fit$convergence), 0))
  expect_false(isTRUE(fit$sdr$pdHess))
})


# ---- 2. The note names the fact, and names glmmTMB's own diagnosis ------

test_that("a non-converged glmmTMB carries a convergence note", {
  fit <- .fit_glmmTMB_nonconverged()
  out <- paste(
    capture.output(print(suppressWarnings(table_regression(fit)))),
    collapse = "\n"
  )
  expect_match(out, "Model convergence problem:", fixed = TRUE)
  # The engine's own message, verbatim, and the Hessian verdict.
  expect_match(out, fit$fit$message, fixed = TRUE)
  expect_match(out, "non-positive-definite Hessian matrix", fixed = TRUE)
  # What the printed numbers are.
  expect_match(
    out,
    "The estimates are the values the optimizer stopped at, not a converged fit.",
    fixed = TRUE
  )
  # And they ARE still printed: the object's starting values, all 1.00.
  expect_match(out, "(Intercept)", fixed = TRUE)
  expect_match(out, "Days", fixed = TRUE)

  # No suppressWarnings: the builder is pure, and asking it what the
  # note says raises nothing.
  note <- spicy:::.glmmTMB_convergence_note(
    spicy:::.glmmTMB_convergence_problems(fit)
  )
  expect_type(note, "character")
  expect_identical(
    note,
    spicy:::spicy_fmt(
      "note_nonconvergence",
      paste(fit$fit$message, "non-positive-definite Hessian matrix", sep = "; ")
    )
  )
})


# ---- 3. The warning joins the taxonomy under spicy_caveat --------------

test_that("a non-converged glmmTMB raises spicy_nonconvergence", {
  fit <- .fit_glmmTMB_nonconverged()

  caught <- NULL
  suppressWarnings(withCallingHandlers(
    table_regression(fit),
    spicy_nonconvergence = function(w) {
      caught <<- w
      invokeRestart("muffleWarning")
    }
  ))
  expect_s3_class(caught, "spicy_nonconvergence")
  # Nested under spicy_caveat (so generic caveat handlers keep catching
  # it) and under the package-wide root.
  expect_s3_class(caught, "spicy_caveat")
  expect_s3_class(caught, "spicy_warning")
  expect_match(
    conditionMessage(caught),
    "Model convergence problem (outcome: Reaction)",
    fixed = TRUE
  )

  # A generic spicy_caveat handler sees it too.
  seen <- FALSE
  suppressWarnings(withCallingHandlers(
    table_regression(fit),
    spicy_caveat = function(w) {
      seen <<- TRUE
      invokeRestart("muffleWarning")
    }
  ))
  expect_true(seen)
})


# ---- 4. Control: a converged fit is silent -----------------------------

test_that("a converged glmmTMB gets neither note nor warning", {
  fit <- .fit_glmmTMB_converged()
  expect_null(spicy:::.glmmTMB_convergence_note(
    spicy:::.glmmTMB_convergence_problems(fit)
  ))
  expect_null(
    spicy:::as_regression_frame(fit)$info$extras$convergence_note
  )

  hit <- FALSE
  out <- suppressWarnings(withCallingHandlers(
    table_regression(fit),
    spicy_nonconvergence = function(w) {
      hit <<- TRUE
      invokeRestart("muffleWarning")
    }
  ))
  expect_false(hit)
  expect_false(grepl(
    "Model convergence problem",
    paste(capture.output(print(out)), collapse = "\n"),
    fixed = TRUE
  ))
})


# ---- 4b. No fit statistic derived from a non-converged fit -------------
#
# The optimizer stopped at its starting values, so every model-level
# statistic computed FROM those values is meaningless. AIC/BIC come back
# NA on their own (logLik is NA) and so render blank; ICC and the
# Nakagawa R2 do not -- they would print a confident 0.50 / 0.81 / 0.90
# for a model that never fitted. They are blanked under the same
# criterion (decision 37). The estimates and the sigma rows still print.

# The fit-statistic tokens the structured view carries in `.variable`.
.nc_fit_stat_tokens <- function(fit) {
  st <- suppressWarnings(as_structured(table_regression(fit)))
  st$body$.variable[st$body$.row_role == "fit_stat"]
}

test_that("a non-converged glmmTMB reports no ICC and no R-squared", {
  fit <- .fit_glmmTMB_nonconverged()

  # (a) The rendered table: no ICC row, no R2 row of either flavour.
  out <- paste(
    capture.output(print(suppressWarnings(table_regression(fit)))),
    collapse = "\n"
  )
  expect_false(grepl("ICC", out, fixed = TRUE))
  expect_false(grepl("R²", out, fixed = TRUE))
  # The estimates and the variance components are still there: the note
  # says what they are worth, it does not withhold them.
  expect_match(out, "(Intercept)", fixed = TRUE)
  expect_match(out, "Subject (Intercept)", fixed = TRUE)

  # (a) The structured view, where those rows would carry a token.
  toks <- .nc_fit_stat_tokens(fit)
  expect_false(any(c("icc", "r2_marginal", "r2_conditional") %in% toks))
  # No pseudo-R2 of any spelling sneaks in either.
  expect_false(any(grepl("r2|pseudo", toks)))
  # The structural fit-stats that are NOT derived from the fit stay.
  expect_true(all(c("nobs", "n_groups") %in% toks))

  # And at the frame level, the source of both.
  frame <- suppressWarnings(spicy:::as_regression_frame(fit))
  expect_true(is.na(frame$info$random_effects$icc))
  expect_true(is.na(frame$info$fit_stats$r2_marginal))
  expect_true(is.na(frame$info$fit_stats$r2_conditional))
})

test_that("a converged glmmTMB keeps its ICC and R-squared", {
  fit <- .fit_glmmTMB_converged()

  out <- paste(capture.output(print(table_regression(fit))), collapse = "\n")
  expect_match(out, "ICC", fixed = TRUE)
  expect_match(out, "R²", fixed = TRUE)

  toks <- .nc_fit_stat_tokens(fit)
  expect_true(all(
    c("icc", "r2_marginal", "r2_conditional") %in% toks
  ))

  frame <- spicy:::as_regression_frame(fit)
  expect_false(is.na(frame$info$random_effects$icc))
  expect_false(is.na(frame$info$fit_stats$r2_marginal))
})


# ---- 4c. One condition surfaces, not four ------------------------------
#
# glmmTMB:::summary.glmmTMB and the Wald extractors raise anonymous
# "NaNs produced" warnings on such a fit (three of them reached the user
# before the mute), restating in the session's locale what the classed
# caveat says precisely. Only the classed warning is allowed out.

# Every condition the call raises, in order, muffled so the run is quiet.
.nc_conditions <- function(expr) {
  seen <- list()
  withCallingHandlers(
    force(expr),
    warning = function(w) {
      seen[[length(seen) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  seen
}

test_that("exactly one condition surfaces, and it is the classed one", {
  fit <- .fit_glmmTMB_nonconverged()
  seen <- .nc_conditions(table_regression(fit))

  expect_length(seen, 1L)
  expect_s3_class(seen[[1L]], "spicy_nonconvergence")
  # Nothing anonymous got through. Matched against base R's own string
  # in the active locale, exactly as the mute matches it, so this holds
  # in a translated session too.
  msgs <- vapply(seen, conditionMessage, character(1))
  nan_msgs <- unique(c("NaNs produced", gettext("NaNs produced", domain = "R")))
  expect_false(any(msgs %in% nan_msgs))
})

test_that("a converged glmmTMB raises no condition at all", {
  fit <- .fit_glmmTMB_converged()
  # The mute is not even armed on this path; nothing may be swallowed
  # and nothing is raised.
  expect_length(.nc_conditions(table_regression(fit)), 0L)
})


# ---- 5. The footer builder is engine-generic ---------------------------

test_that("the convergence footer builder reads any engine's note", {
  # One frame flagged out of two keeps the "Model k:" attribution -- the
  # same arm the Bayesian sampler-diagnostics note uses, now shared.
  mk <- function(note = NULL) {
    list(info = list(extras = if (is.null(note)) list() else {
      list(convergence_note = note)
    }))
  }
  expect_identical(
    spicy:::build_convergence_footer_block_from_frames(
      list(mk(), mk("Model convergence problem: capped."))
    ),
    "Model 2: Model convergence problem: capped."
  )
  expect_null(
    spicy:::build_convergence_footer_block_from_frames(list(mk(), mk()))
  )
})


# ---- 6. The diagnoses spicy words itself live in the registry -----------
#
# "non-positive-definite Hessian matrix" and "optimizer returned code %s"
# were spicy prose written at the criterion and poured into the footer
# through the data hole of `note_nonconvergence` -- table text living
# outside the registry, against the contract. They are keys now, and the
# criterion returns RECORDS (a kind + its datum) rather than sentences,
# which is what lets the two audiences diverge: the footer follows
# `spicy.language`, the warning does not.

test_that("the criterion returns records, not sentences", {
  fit <- .fit_glmmTMB_nonconverged()
  problems <- spicy:::.glmmTMB_convergence_problems(fit)
  expect_type(problems, "list")
  expect_identical(
    vapply(problems, function(p) p$kind, character(1)),
    c("engine_message", "hessian")
  )
  # The engine's message is DATA and is carried verbatim.
  expect_identical(problems[[1L]]$value, fit$fit$message)
})

test_that("the two spicy diagnoses resolve from the registry", {
  fit <- .fit_glmmTMB_nonconverged()
  problems <- spicy:::.glmmTMB_convergence_problems(fit)
  txt <- spicy:::.glmmTMB_problem_text(problems)
  expect_identical(
    txt,
    c(fit$fit$message, spicy:::spicy_str("note_nonconvergence_hessian"))
  )
  # The code arm, which no fixture reaches (glmmTMB always carries a
  # message), formats the optimizer's return code into its own key.
  expect_identical(
    spicy:::.glmmTMB_problem_text(list(list(kind = "code", value = "7"))),
    sprintf(spicy:::spicy_str("note_nonconvergence_code"), "7")
  )
})

test_that("the footer follows the language and the condition does not", {
  fit <- .fit_glmmTMB_nonconverged()
  withr::local_options(spicy.language = "fr")

  caught <- NULL
  out <- suppressWarnings(withCallingHandlers(
    table_regression(fit),
    spicy_nonconvergence = function(w) {
      caught <<- w
      invokeRestart("muffleWarning")
    }
  ))
  rendered <- paste(capture.output(print(out)), collapse = "\n")

  # The table speaks French, through the key.
  expect_match(
    rendered,
    spicy:::spicy_str("note_nonconvergence_hessian"),
    fixed = TRUE
  )
  expect_false(grepl(
    "non-positive-definite Hessian matrix",
    rendered,
    fixed = TRUE
  ))
  # The condition stays English: it is read by developers and quoted in
  # bug reports (dev/i18n_string_census.md section 6).
  expect_match(
    conditionMessage(caught),
    "non-positive-definite Hessian matrix",
    fixed = TRUE
  )
  # The engine's own message is data and appears in both, verbatim.
  expect_match(rendered, fit$fit$message, fixed = TRUE)
  expect_match(conditionMessage(caught), fit$fit$message, fixed = TRUE)
})


# ---- 7. The builder is pure; the emission site signals ------------------
#
# The note builder used to raise the classed warning while building the
# note, so one CALL was one WARNING and asking the note what it said was
# indistinguishable from reporting the problem. It held together only
# because the frame called it exactly once. Construction and signalling
# are separate now.

test_that("building the note raises nothing, however often it is asked", {
  fit <- .fit_glmmTMB_nonconverged()
  problems <- spicy:::.glmmTMB_convergence_problems(fit)
  seen <- .nc_conditions({
    for (i in 1:3) spicy:::.glmmTMB_convergence_note(problems)
  })
  expect_length(seen, 0L)
  # And it still returns the note.
  expect_type(spicy:::.glmmTMB_convergence_note(problems), "character")
})

test_that("the signal is its own function, and still classed", {
  fit <- .fit_glmmTMB_nonconverged()
  problems <- spicy:::.glmmTMB_convergence_problems(fit)
  seen <- .nc_conditions(
    spicy:::.warn_glmmTMB_nonconvergence(problems, "Reaction")
  )
  expect_length(seen, 1L)
  expect_s3_class(seen[[1L]], "spicy_nonconvergence")
  expect_s3_class(seen[[1L]], "spicy_caveat")
  expect_match(
    conditionMessage(seen[[1L]]),
    "Model convergence problem (outcome: Reaction)",
    fixed = TRUE
  )
})
