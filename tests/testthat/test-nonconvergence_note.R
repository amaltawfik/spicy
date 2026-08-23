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

  note <- suppressWarnings(spicy:::.glmmTMB_convergence_note(fit, "Reaction"))
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
  expect_null(spicy:::.glmmTMB_convergence_note(fit, "Reaction"))
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
