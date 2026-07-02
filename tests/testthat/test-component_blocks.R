# Component blocks: the zero / zero-inflation / dispersion components of
# two-part count models render as labelled subordinate row blocks (was:
# invisible -- only the count/conditional component rendered). Oracles are the
# per-component summary() matrices; robust SEs cross-validate to the
# whole-model sandwich::vcovCL. Design + decisions:
# dev/component_blocks_spec.md (D1-D6, validated 2026-07-02).

.cb_zeroinfl <- function() {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl", envir = environment())
  pscl::zeroinfl(art ~ fem + mar | fem, data = bioChemists)
}
.cb_hurdle <- function() {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl", envir = environment())
  pscl::hurdle(art ~ fem + mar | fem, data = bioChemists)
}
.cb_tmb <- function() {
  skip_if_not_installed("glmmTMB")
  data("Salamanders", package = "glmmTMB", envir = environment())
  suppressWarnings(glmmTMB::glmmTMB(
    count ~ mined + (1 | site), zi = ~ mined, disp = ~ DOY,
    family = glmmTMB::nbinom2, data = Salamanders))
}

## ---- 1. Blocks render, per-semantics labels -------------------------------

test_that("zeroinfl renders a Zero-inflation block; hurdle a Zero hurdle block", {
  df_z <- table_regression(.cb_zeroinfl(), output = "data.frame")
  v_z <- trimws(df_z$Variable)
  expect_true(any(grepl("Zero-inflation", v_z, fixed = TRUE)))
  expect_true(any(v_z == "fem: Women"))

  df_h <- table_regression(.cb_hurdle(), output = "data.frame")
  v_h <- trimws(df_h$Variable)
  expect_true(any(grepl("Zero hurdle", v_h, fixed = TRUE)))
  expect_false(any(grepl("Zero-inflation", v_h, fixed = TRUE)))
})

test_that("glmmTMB renders zi + Dispersion blocks before the Random effects", {
  df <- table_regression(.cb_tmb(), output = "data.frame")
  v <- trimws(df$Variable)
  i_zi <- which(grepl("Zero-inflation", v, fixed = TRUE))[1]
  i_dp <- which(grepl("Dispersion", v, fixed = TRUE))[1]
  i_re <- which(grepl("Random effects", v, fixed = TRUE))[1]
  expect_true(i_zi < i_dp && i_dp < i_re)
})

test_that("the Dispersion block only appears when dispersion was modelled", {
  skip_if_not_installed("glmmTMB")
  data("Salamanders", package = "glmmTMB", envir = environment())
  m <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ mined + (1 | site), family = glmmTMB::nbinom2,
    data = Salamanders))   # free dispersion parameter, but NO disp model
  df <- table_regression(m, output = "data.frame")
  expect_false(any(grepl("Dispersion", df$Variable, fixed = TRUE)))
})

## ---- 2. Oracle: rows match the per-component summary ----------------------

test_that("zero rows match summary(fit)$coefficients$zero to 1e-9", {
  skip_if_not_installed("broom")
  m <- .cb_zeroinfl()
  td <- broom::tidy(table_regression(m))
  zr <- td[grepl("^zero_", td$term) & !is.na(td$estimate), ]
  smz <- summary(m)$coefficients$zero
  expect_equal(zr$estimate,  unname(smz[, "Estimate"]),   tolerance = 1e-9)
  expect_equal(zr$std.error, unname(smz[, "Std. Error"]), tolerance = 1e-9)
  expect_equal(zr$p.value,   unname(smz[, "Pr(>|z|)"]),   tolerance = 1e-9)
})

test_that("glmmTMB zi/disp rows match their summary matrices", {
  skip_if_not_installed("broom")
  m <- .cb_tmb()
  td <- broom::tidy(table_regression(m))
  zi <- td[grepl("^zi\\.", td$term) & !is.na(td$estimate), ]
  sm <- summary(m)$coefficients$zi
  expect_equal(zi$estimate, unname(sm[, "Estimate"]), tolerance = 1e-9)
  dp <- td[grepl("^disp\\.", td$term) & !is.na(td$estimate), ]
  smd <- summary(m)$coefficients$disp
  expect_equal(dp$estimate, unname(smd[, "Estimate"]), tolerance = 1e-9)
})

## ---- 3. Exponentiation: per-block, link-gated ------------------------------

test_that("exp: logit zero block becomes OR; dispersion never exponentiates", {
  skip_if_not_installed("broom")
  m <- .cb_zeroinfl()
  td0 <- broom::tidy(table_regression(m))
  tdx <- broom::tidy(table_regression(m, exponentiate = TRUE))
  z0 <- td0[grepl("^zero_", td0$term) & !is.na(td0$estimate), ]
  zx <- tdx[grepl("^zero_", tdx$term) & !is.na(tdx$estimate), ]
  expect_equal(zx$estimate, exp(z0$estimate), tolerance = 1e-9)

  mt <- .cb_tmb()
  t0 <- broom::tidy(table_regression(mt))
  tx <- broom::tidy(table_regression(mt, exponentiate = TRUE))
  d0 <- t0[grepl("^disp\\.", t0$term) & !is.na(t0$estimate), ]
  dx <- tx[grepl("^disp\\.", tx$term) & !is.na(tx$estimate), ]
  expect_equal(dx$estimate, d0$estimate, tolerance = 1e-12)  # untouched
})

test_that("exp: a probit zero link is NOT exponentiated", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("broom")
  data("bioChemists", package = "pscl", envir = environment())
  m <- pscl::zeroinfl(art ~ fem | fem, data = bioChemists, link = "probit")
  td0 <- broom::tidy(table_regression(m))
  tdx <- broom::tidy(table_regression(m, exponentiate = TRUE))
  z0 <- td0[grepl("^zero_", td0$term) & !is.na(td0$estimate), ]
  zx <- tdx[grepl("^zero_", tdx$term) & !is.na(tdx$estimate), ]
  expect_equal(zx$estimate, z0$estimate, tolerance = 1e-12)  # link scale kept
})

## ---- 4. Family membership: p_adjust + stars --------------------------------

test_that("component rows join the p_adjust family; their intercept does not", {
  skip_if_not_installed("broom")
  m <- .cb_zeroinfl()
  raw <- broom::tidy(table_regression(m))
  adj <- broom::tidy(table_regression(m, p_adjust = "holm"))
  r <- raw[raw$term == "zero_femWomen", "p.value", drop = TRUE]
  a <- adj[adj$term == "zero_femWomen", "p.value", drop = TRUE]
  expect_gt(a, r)   # holm inflates a mid-family p
  ri <- raw[raw$term == "zero_(Intercept)", "p.value", drop = TRUE]
  ai <- adj[adj$term == "zero_(Intercept)", "p.value", drop = TRUE]
  expect_equal(ai, ri, tolerance = 1e-12)   # intercepts excluded
})

test_that("stars apply to component rows", {
  m <- .cb_zeroinfl()
  df <- table_regression(m, stars = TRUE, output = "data.frame")
  b_col <- names(df)[2]
  zi_int <- grepl("[0-9]", df[[b_col]]) &
    grepl("(Intercept)", df$Variable, fixed = TRUE)
  # the zero-inflation intercept is p < .001 -> starred
  expect_true(any(grepl("[*]", df[[b_col]][zi_int])))
})

## ---- 5. Robust CL covers BOTH components (pscl upgrade) --------------------

test_that("zeroinfl CR* matches sandwich::vcovCL on both components", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("broom")
  m <- .cb_zeroinfl()
  data("bioChemists", package = "pscl", envir = environment())
  td <- broom::tidy(table_regression(m, vcov = "CR2",
                                     cluster = bioChemists$fem))
  se_orc <- sqrt(diag(sandwich::vcovCL(m, cluster = bioChemists$fem)))
  expect_equal(td$std.error[td$term == "femWomen"],
               unname(se_orc["count_femWomen"]), tolerance = 1e-7)
  expect_equal(td$std.error[td$term == "zero_femWomen"],
               unname(se_orc["zero_femWomen"]), tolerance = 1e-7)
})

test_that("HC* stays refused for pscl (no hatvalues machinery)", {
  m <- .cb_zeroinfl()
  expect_error(table_regression(m, vcov = "HC3", output = "data.frame"),
               class = "spicy_unsupported_vcov")
})

## ---- 6. AME wired (combined response) --------------------------------------

test_that("pscl AME column is populated and matches avg_slopes (response)", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("broom")
  m <- .cb_zeroinfl()
  td <- broom::tidy(table_regression(m, show_columns = c("b", "ame")))
  ame <- td[td$estimate_type == "ame" & td$term == "femWomen", ]
  orc <- suppressWarnings(marginaleffects::avg_slopes(m))
  expect_equal(ame$estimate,
               orc$estimate[orc$term == "fem"][1], tolerance = 1e-8)
})

## ---- 7. Controls + rail ----------------------------------------------------

test_that("show_components = FALSE omits the blocks; validation is fail-fast", {
  m <- .cb_zeroinfl()
  df <- table_regression(m, show_components = FALSE, output = "data.frame")
  expect_false(any(grepl("Zero-inflation", df$Variable, fixed = TRUE)))
  expect_error(table_regression(m, show_components = "yes"),
               class = "spicy_invalid_input")
})

test_that("keep/drop never mutilates a component block", {
  m <- .cb_zeroinfl()
  df <- table_regression(m, keep = "mar", output = "data.frame")
  v <- trimws(df$Variable)
  expect_true(any(grepl("Zero-inflation", v, fixed = TRUE)))
  expect_true(any(v == "fem: Women"))       # block intact
  expect_false(any(v == "Women"))           # count-side fem filtered out
})

test_that("the footer gloss names each block and the hurdle direction", {
  out_z <- paste(capture.output(print(table_regression(.cb_zeroinfl()))),
                 collapse = "\n")
  expect_match(out_z, "structural (excess) zero", fixed = TRUE)
  out_h <- paste(capture.output(print(table_regression(.cb_hurdle()))),
                 collapse = "\n")
  expect_match(out_h, "log-odds of a nonzero count", fixed = TRUE)
})

test_that("structured body carries the component rows (backend parity)", {
  m <- .cb_zeroinfl()
  s <- as_structured(table_regression(m, show_columns = c("b", "se", "p")))
  b <- s$body
  zi <- b[grepl("fem: Women", b$Variable, fixed = TRUE), , drop = FALSE]
  expect_equal(nrow(zi), 1L)
  num_cols <- names(b)[vapply(b, is.numeric, logical(1))]
  expect_true(sum(is.finite(unlist(zi[, num_cols]))) >= 2L)
})

## ---- 8. Incidental fixes ----------------------------------------------------

test_that("hurdle with a count-type zero.dist keeps its block un-exponentiated", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("broom")
  data("bioChemists", package = "pscl", envir = environment())
  m <- pscl::hurdle(art ~ fem | fem, data = bioChemists,
                    zero.dist = "poisson")
  fr <- as_regression_frame(m)
  blk <- fr$info$extras$component_blocks[[1L]]
  expect_true(is.na(blk$link))              # no fabricated "logit"
  expect_false(isTRUE(blk$exp_ok))
  td0 <- broom::tidy(table_regression(m))
  tdx <- broom::tidy(table_regression(m, exponentiate = TRUE))
  z0 <- td0[grepl("^zero_", td0$term) & !is.na(td0$estimate), ]
  zx <- tdx[grepl("^zero_", tdx$term) & !is.na(tdx$estimate), ]
  expect_equal(zx$estimate, z0$estimate, tolerance = 1e-12)
})

test_that("weighted pscl fits report has_weights", {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl", envir = environment())
  w <- rep(c(1, 2), length.out = nrow(bioChemists))
  m <- suppressWarnings(pscl::zeroinfl(art ~ fem | 1, data = bioChemists,
                                       weights = w))
  fr <- as_regression_frame(m)
  expect_true(isTRUE(fr$info$extras$has_weights))
})

test_that("glmmTMB nbinom2 exp header reads IRR", {
  mt <- .cb_tmb()
  out <- paste(capture.output(print(table_regression(mt, exponentiate = TRUE))),
               collapse = "\n")
  expect_match(out, "IRR", fixed = TRUE)
})
