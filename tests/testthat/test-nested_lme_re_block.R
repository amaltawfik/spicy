# ---------------------------------------------------------------------------
# The random-effects block of a NESTED nlme::lme fit.
#
# nlme::VarCorr.lme() flattens every level of a nested fit into one
# character matrix and marks each block with a "<group> =" header row.
# Reading the group name from fit$dims$ngrps[1] instead -- which runs
# innermost-first, the reverse of VarCorr's block order -- labelled both
# levels of `random = ~ 1 | Dog/Side` as "Side", sent both intervals()
# lookups into the same reStruct block (so the two levels shared one SE
# and one CI), and left the renderer with two rows under one identical
# (group, term) key, of which it displayed one.
#
# Oracle: nlme::VarCorr(fit) for the point estimates and
# nlme::intervals(fit) for the CIs -- the engine's own output, read
# directly.
# ---------------------------------------------------------------------------

# ---- Fixtures -------------------------------------------------------------

.fit_lme_nested <- function() {
  skip_if_not_installed("nlme")
  # Pixel: 102 CT scans of 10 dogs, both sides of each dog scanned over
  # several days -- Side is nested in Dog, which is what the design is.
  nlme::lme(pixel ~ day, data = nlme::Pixel, random = ~ 1 | Dog / Side)
}

.fit_lme_nested_slope <- function() {
  skip_if_not_installed("nlme")
  # A random slope at the OUTER level only: exercises the per-group
  # correlation walk (Dog carries a cor() row, Side does not).
  nlme::lme(
    pixel ~ day,
    data = nlme::Pixel,
    random = list(Dog = ~day, Side = ~1)
  )
}

.fit_lme_single <- function() {
  skip_if_not_installed("nlme")
  nlme::lme(distance ~ age, data = nlme::Orthodont, random = ~ age | Subject)
}


# ---- 1. Each level is labelled with its OWN grouping factor -------------

test_that("a nested lme labels each variance component with its own level", {
  fit <- .fit_lme_nested()
  vc <- spicy:::as_regression_frame(fit)$info$random_effects$
    variance_components

  expect_identical(vc$group, c("Dog", "Side", "Residual"))
  expect_identical(vc$term, c("(Intercept)", "(Intercept)", ""))

  # Oracle: nlme::VarCorr() itself, block by block. Its Variance column
  # is character, so the numbers are parsed the way the frame parses
  # them -- outermost block first.
  raw <- unclass(nlme::VarCorr(fit))
  keep <- !is.na(suppressWarnings(as.numeric(raw[, "Variance"])))
  oracle_var <- unname(suppressWarnings(as.numeric(raw[keep, "Variance"])))
  oracle_sd <- unname(suppressWarnings(as.numeric(raw[keep, "StdDev"])))
  expect_equal(vc$variance, oracle_var, tolerance = 1e-8)
  expect_equal(vc$sd, oracle_sd, tolerance = 1e-8)
})


# ---- 2. The two levels no longer share one SE and one CI ----------------

test_that("a nested lme gives each level its own Wald SE and CI", {
  fit <- .fit_lme_nested()
  vc <- spicy:::as_regression_frame(fit)$info$random_effects$
    variance_components

  # The defect: identical std_error / ci_lower / ci_upper on the two
  # random-effect rows, because both read the same intervals() block.
  expect_false(isTRUE(all.equal(vc$std_error[1L], vc$std_error[2L])))
  expect_false(isTRUE(all.equal(vc$ci_lower[1L], vc$ci_lower[2L])))
  expect_false(isTRUE(all.equal(vc$ci_upper[1L], vc$ci_upper[2L])))

  # Oracle: intervals() bounds on the SD scale, squared to the variance
  # scale the frame stores, and the Delta-method SE derived from the
  # same half-width (SE(sd^2) = 2 * sd * SE(sd)).
  iv <- nlme::intervals(fit, which = "var-cov")
  z <- stats::qnorm(0.975)
  for (k in seq_len(2L)) {
    g <- vc$group[k]
    row <- iv$reStruct[[g]]["sd((Intercept))", ]
    expect_equal(vc$ci_lower[k], unname(row[["lower"]])^2, tolerance = 1e-8)
    expect_equal(vc$ci_upper[k], unname(row[["upper"]])^2, tolerance = 1e-8)
    se_sd <- (row[["upper"]] - row[["lower"]]) / (2 * z)
    expect_equal(
      vc$std_error[k],
      unname(2 * row[["est."]] * se_sd),
      tolerance = 1e-8
    )
  }

  # And the residual keeps reading intervals()$sigma, not a reStruct block.
  expect_equal(vc$ci_lower[3L], unname(iv$sigma[["lower"]])^2, tolerance = 1e-8)
  expect_equal(vc$ci_upper[3L], unname(iv$sigma[["upper"]])^2, tolerance = 1e-8)
})


# ---- 3. The rendered table shows both levels, and both group counts -----

test_that("a nested lme renders one RE row and one N row per level", {
  fit <- .fit_lme_nested()
  out <- paste(
    capture.output(print(suppressWarnings(table_regression(fit)))),
    collapse = "\n"
  )
  # Both levels are present as rows -- the collapsed-key defect showed
  # exactly one sigma row for a two-level fit.
  expect_match(out, "\u03C3 Dog (Intercept)", fixed = TRUE)
  expect_match(out, "\u03C3 Side (Intercept)", fixed = TRUE)
  expect_match(out, "\u03C3 (Residual)", fixed = TRUE)

  # `n_groups` promises one "N (<factor>)" row per grouping factor.
  expect_match(out, "N (Dog)", fixed = TRUE)
  expect_match(out, "N (Side)", fixed = TRUE)

  ng <- spicy:::as_regression_frame(fit)$info$n_groups
  # Outermost first, matching the block order above it.
  expect_identical(names(ng), c("Dog", "Side"))
  expect_identical(unname(ng), c(10L, 20L))
})


# ---- 4. A correlation belongs to the level that carries it --------------

test_that("a nested lme attributes each correlation row to its own level", {
  fit <- .fit_lme_nested_slope()
  vc <- spicy:::as_regression_frame(fit)$info$random_effects$
    variance_components

  is_cor <- vc$is_correlation %in% TRUE
  expect_identical(sum(is_cor), 1L)
  expect_identical(vc$group[is_cor], "Dog")
  expect_identical(vc$term[is_cor], "(Intercept), day")

  iv <- nlme::intervals(fit, which = "var-cov")
  oracle <- iv$reStruct$Dog["cor((Intercept),day)", ]
  expect_equal(vc$corr[is_cor], unname(oracle[["est."]]), tolerance = 1e-8)
  expect_equal(vc$ci_lower[is_cor], unname(oracle[["lower"]]), tolerance = 1e-8)
  expect_equal(vc$ci_upper[is_cor], unname(oracle[["upper"]]), tolerance = 1e-8)

  # The variance rows keep their own levels, Dog's two before Side's one.
  expect_identical(vc$group[!is_cor], c("Dog", "Dog", "Side", "Residual"))
  expect_identical(vc$term[!is_cor], c("(Intercept)", "day", "(Intercept)", ""))
})


# ---- 5. Control: a single-level fit is unchanged -------------------------

test_that("a single-level lme still reads its group from ngrps", {
  fit <- .fit_lme_single()
  fr <- spicy:::as_regression_frame(fit)
  vc <- fr$info$random_effects$variance_components

  # VarCorr carries NO block header for a single-level fit (the group
  # name lives in attr(vc, "title")), so this is the fall-back path.
  expect_false(any(grepl("=$", rownames(unclass(nlme::VarCorr(fit))))))
  expect_identical(
    vc$group,
    c("Subject", "Subject", "Subject", "Residual")
  )
  expect_identical(names(fr$info$n_groups), "Subject")
  expect_identical(unname(fr$info$n_groups), 27L)

  iv <- nlme::intervals(fit, which = "var-cov")
  expect_equal(
    vc$variance[1:2],
    unname(iv$reStruct$Subject[c("sd((Intercept))", "sd(age)"), "est."]^2),
    tolerance = 1e-8
  )
  # One correlation row, still interleaved before the residual.
  expect_identical(vc$is_correlation, c(FALSE, FALSE, TRUE, FALSE))
})
