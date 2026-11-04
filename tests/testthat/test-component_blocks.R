# Component blocks: the zero / zero-inflation / dispersion components of
# two-part count models render as labeled subordinate row blocks (was:
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
    count ~ mined + (1 | site),
    zi = ~mined,
    disp = ~DOY,
    family = glmmTMB::nbinom2,
    data = Salamanders
  ))
}

## ---- 1. Blocks render, per-semantics labels -------------------------------

test_that("zeroinfl renders a Zero-inflation block; hurdle a Zero hurdle block", {
  # The printed tables: `$Variable` holds the displayed row labels.
  df_z <- table_regression(.cb_zeroinfl())
  v_z <- trimws(df_z$Variable)
  expect_true(any(grepl("Zero-inflation", v_z, fixed = TRUE)))
  expect_true(any(v_z == "fem: Women"))

  df_h <- table_regression(.cb_hurdle())
  v_h <- trimws(df_h$Variable)
  expect_true(any(grepl("Zero hurdle", v_h, fixed = TRUE)))
  expect_false(any(grepl("Zero-inflation", v_h, fixed = TRUE)))
})

test_that("glmmTMB renders zi + Dispersion blocks before the Random effects", {
  df <- table_regression(.cb_tmb())
  v <- trimws(df$Variable)
  i_zi <- which(grepl("Zero-inflation", v, fixed = TRUE))[1]
  i_dp <- which(grepl("Dispersion", v, fixed = TRUE))[1]
  i_re <- which(grepl("Random effects", v, fixed = TRUE))[1]
  expect_true(i_zi < i_dp && i_dp < i_re)
})

test_that("the Dispersion block only appears when dispersion was modeled", {
  skip_if_not_installed("glmmTMB")
  data("Salamanders", package = "glmmTMB", envir = environment())
  m <- suppressWarnings(glmmTMB::glmmTMB(
    count ~ mined + (1 | site),
    family = glmmTMB::nbinom2,
    data = Salamanders
  )) # free dispersion parameter, but NO disp model
  df <- table_regression(m)
  # Positive control: the rows are read, so the absence below bites.
  expect_true("(Intercept)" %in% trimws(df$Variable))
  expect_false(any(grepl("Dispersion", df$Variable, fixed = TRUE)))
})

## ---- 2. Oracle: rows match the per-component summary ----------------------

test_that("zero rows match summary(fit)$coefficients$zero to 1e-9", {
  skip_if_not_installed("broom")
  m <- .cb_zeroinfl()
  td <- broom::tidy(table_regression(m))
  zr <- td[td$factor_term %in% "Zero-inflation" & !is.na(td$estimate), ]
  smz <- summary(m)$coefficients$zero
  expect_equal(zr$estimate, unname(smz[, "Estimate"]), tolerance = 1e-9)
  expect_equal(zr$std.error, unname(smz[, "Std. Error"]), tolerance = 1e-9)
  expect_equal(zr$p.value, unname(smz[, "Pr(>|z|)"]), tolerance = 1e-9)
})

test_that("glmmTMB zi/disp rows match their summary matrices", {
  skip_if_not_installed("broom")
  m <- .cb_tmb()
  td <- broom::tidy(table_regression(m))
  zi <- td[td$factor_term %in% "Zero-inflation" & !is.na(td$estimate), ]
  sm <- summary(m)$coefficients$zi
  expect_equal(zi$estimate, unname(sm[, "Estimate"]), tolerance = 1e-9)
  dp <- td[td$factor_term %in% "Dispersion" & !is.na(td$estimate), ]
  smd <- summary(m)$coefficients$disp
  expect_equal(dp$estimate, unname(smd[, "Estimate"]), tolerance = 1e-9)
})

## ---- 3. Exponentiation: per-block, link-gated ------------------------------

test_that("exp: logit zero block becomes OR; dispersion never exponentiates", {
  skip_if_not_installed("broom")
  m <- .cb_zeroinfl()
  td0 <- broom::tidy(table_regression(m))
  tdx <- broom::tidy(table_regression(m, exponentiate = TRUE))
  z0 <- td0[td0$factor_term %in% "Zero-inflation" & !is.na(td0$estimate), ]
  zx <- tdx[tdx$factor_term %in% "Zero-inflation" & !is.na(tdx$estimate), ]
  expect_equal(zx$estimate, exp(z0$estimate), tolerance = 1e-9)

  mt <- .cb_tmb()
  t0 <- broom::tidy(table_regression(mt))
  tx <- broom::tidy(table_regression(mt, exponentiate = TRUE))
  d0 <- t0[t0$factor_term %in% "Dispersion" & !is.na(t0$estimate), ]
  dx <- tx[tx$factor_term %in% "Dispersion" & !is.na(tx$estimate), ]
  expect_equal(dx$estimate, d0$estimate, tolerance = 1e-12) # untouched
})

test_that("exp: a probit zero link is NOT exponentiated", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("broom")
  data("bioChemists", package = "pscl", envir = environment())
  m <- pscl::zeroinfl(art ~ fem | fem, data = bioChemists, link = "probit")
  td0 <- broom::tidy(table_regression(m))
  tdx <- broom::tidy(table_regression(m, exponentiate = TRUE))
  z0 <- td0[td0$factor_term %in% "Zero-inflation" & !is.na(td0$estimate), ]
  zx <- tdx[tdx$factor_term %in% "Zero-inflation" & !is.na(tdx$estimate), ]
  expect_equal(zx$estimate, z0$estimate, tolerance = 1e-12) # link scale kept
})

## ---- 4. Family membership: p_adjust + stars --------------------------------

test_that("component rows join the p_adjust family; their intercept does not", {
  skip_if_not_installed("broom")
  m <- .cb_zeroinfl()
  raw <- broom::tidy(table_regression(m))
  adj <- broom::tidy(table_regression(m, p_adjust = "holm"))
  r <- raw[raw$term == "Zero-inflation::femWomen", "p.value", drop = TRUE]
  a <- adj[adj$term == "Zero-inflation::femWomen", "p.value", drop = TRUE]
  expect_gt(a, r) # holm inflates a mid-family p
  ri <- raw[raw$term == "Zero-inflation::(Intercept)", "p.value", drop = TRUE]
  ai <- adj[adj$term == "Zero-inflation::(Intercept)", "p.value", drop = TRUE]
  expect_equal(ai, ri, tolerance = 1e-12) # intercepts excluded
})

test_that("stars apply to component rows", {
  m <- .cb_zeroinfl()
  df <- table_regression(m, stars = TRUE)
  b_col <- names(df)[2]
  zi_int <- grepl("[0-9]", df[[b_col]]) &
    grepl("(Intercept)", df$Variable, fixed = TRUE)
  # the zero-inflation intercept is p < .001 -> starred
  expect_true(any(grepl("[*]", df[[b_col]][zi_int])))
})

## ---- 5. Robust CL covers BOTH components (pscl upgrade) --------------------

test_that("zeroinfl CR1 matches sandwich::vcovCL on both components", {
  skip_if_not_installed("sandwich")
  skip_if_not_installed("broom")
  m <- .cb_zeroinfl()
  data("bioChemists", package = "pscl", envir = environment())
  td <- broom::tidy(table_regression(
    m,
    vcov = "CR1",
    cluster = bioChemists$fem
  ))
  se_orc <- sqrt(diag(sandwich::vcovCL(m, cluster = bioChemists$fem)))
  expect_equal(
    td$std.error[td$term == "femWomen"],
    unname(se_orc["count_femWomen"]),
    tolerance = 1e-7
  )
  expect_equal(
    td$std.error[td$term == "Zero-inflation::femWomen"],
    unname(se_orc["zero_femWomen"]),
    tolerance = 1e-7
  )
})

test_that("HC* stays refused for pscl (no hatvalues machinery)", {
  m <- .cb_zeroinfl()
  expect_error(
    table_regression(m, vcov = "HC3", output = "data.frame"),
    class = "spicy_unsupported_vcov"
  )
})

## ---- 6. AME wired (combined response) --------------------------------------

test_that("pscl AME column is populated and matches avg_slopes (response)", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("broom")
  m <- .cb_zeroinfl()
  td <- broom::tidy(table_regression(m, show_columns = c("b", "ame")))
  ame <- td[td$estimate_type == "ame" & td$term == "femWomen", ]
  orc <- suppressWarnings(marginaleffects::avg_slopes(m))
  expect_equal(
    ame$estimate,
    orc$estimate[orc$term == "fem"][1],
    tolerance = 1e-8
  )
})

## ---- 7. Controls + rail ----------------------------------------------------

test_that("show_components = FALSE omits the blocks; validation is fail-fast", {
  m <- .cb_zeroinfl()
  df <- table_regression(m, show_components = FALSE)
  expect_true("(Intercept)" %in% trimws(df$Variable))
  expect_false(any(grepl("Zero-inflation", df$Variable, fixed = TRUE)))
  expect_error(
    table_regression(m, show_components = "yes"),
    class = "spicy_invalid_input"
  )
})

test_that("keep/drop never mutilates a component block", {
  m <- .cb_zeroinfl()
  df <- table_regression(m, keep = "mar")
  v <- trimws(df$Variable)
  expect_true(any(grepl("Zero-inflation", v, fixed = TRUE)))
  expect_true(any(v == "fem: Women")) # block intact
  expect_false(any(v == "Women")) # count-side fem filtered out
})

test_that("the footer gloss names each block and the hurdle direction", {
  out_z <- paste(
    capture.output(print(table_regression(.cb_zeroinfl()))),
    collapse = "\n"
  )
  expect_match(out_z, "structural (excess) zero", fixed = TRUE)
  out_h <- paste(
    capture.output(print(table_regression(.cb_hurdle()))),
    collapse = "\n"
  )
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
  m <- pscl::hurdle(art ~ fem | fem, data = bioChemists, zero.dist = "poisson")
  fr <- as_regression_frame(m)
  blk <- fr$info$extras$component_blocks[[1L]]
  expect_true(is.na(blk$link)) # no fabricated "logit"
  expect_false(isTRUE(blk$exp_ok))
  td0 <- broom::tidy(table_regression(m))
  tdx <- broom::tidy(table_regression(m, exponentiate = TRUE))
  hu <- "Zero hurdle"
  z0 <- td0[td0$factor_term %in% hu & !is.na(td0$estimate), ]
  zx <- tdx[tdx$factor_term %in% hu & !is.na(tdx$estimate), ]
  expect_equal(nrow(z0), 2L)
  expect_equal(zx$estimate, z0$estimate, tolerance = 1e-12)
})

test_that("weighted pscl fits report has_weights", {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl", envir = environment())
  w <- rep(c(1, 2), length.out = nrow(bioChemists))
  m <- suppressWarnings(pscl::zeroinfl(
    art ~ fem | 1,
    data = bioChemists,
    weights = w
  ))
  fr <- as_regression_frame(m)
  expect_true(isTRUE(fr$info$extras$has_weights))
})

test_that("glmmTMB nbinom2 exp header reads IRR", {
  mt <- .cb_tmb()
  out <- paste(
    capture.output(print(table_regression(mt, exponentiate = TRUE))),
    collapse = "\n"
  )
  expect_match(out, "IRR", fixed = TRUE)
})

test_that("p_adjust footer m matches the family actually adjusted (component intercepts excluded)", {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl")
  fit <- pscl::zeroinfl(
    art ~ fem + mar + kid5 + ment | ment,
    data = bioChemists
  )
  out <- table_regression(fit, p_adjust = "holm")
  note <- paste(attr(out, "note"), collapse = "\n")
  # Family: 4 count slopes + 1 zero slope; both intercepts excluded.
  expect_match(
    note,
    "P-values adjusted for 5 tests (Holm method).",
    fixed = TRUE
  )
})


# Phase 3 matrix – vignettes-news:multimodel-empty-blocks (lot T4)

test_that("mixed multi-model table: the zero block stays blank for the model without one", {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl", envir = environment())
  fp <- glm(art ~ fem + ment, family = poisson, data = bioChemists)
  fz <- pscl::zeroinfl(art ~ fem + ment | ment, data = bioChemists)
  df <- table_regression(list(Poisson = fp, ZIP = fz))
  v <- trimws(df$Variable)
  zi_header <- which(grepl("Zero-inflation", v, fixed = TRUE))
  expect_length(zi_header, 1L)
  # The block's coefficient rows: from the header to the next
  # non-indented section (here the fit stats start at "n").
  zi_rows <- which(v %in% c("(Intercept)", "ment"))
  zi_rows <- zi_rows[zi_rows > zi_header]
  expect_length(zi_rows, 2L)
  pois_cols <- grep("^Poisson: ", names(df), value = TRUE)
  zip_cols <- grep("^ZIP: ", names(df), value = TRUE)
  for (cl in pois_cols) {
    expect_false(any(grepl("[0-9]", df[[cl]][zi_rows])))
  }
  # Discriminating: the same rows are populated under the ZIP model.
  zip_b <- df[[zip_cols[1L]]][zi_rows]
  expect_true(all(grepl("[0-9]", zip_b)))
})


# Phase 3 matrix – vignettes-news:two-part-combined-ame (lot T4)

test_that("two-part AME: zero-component rows carry no AME of their own", {
  skip_if_not_installed("pscl")
  skip_if_not_installed("marginaleffects")
  data("bioChemists", package = "pscl", envir = environment())
  m <- pscl::zeroinfl(art ~ fem + ment | ment, data = bioChemists)
  df <- table_regression(m, show_columns = c("b", "ame"))
  v <- trimws(df$Variable)
  ame_col <- grep("AME", names(df), value = TRUE)[1L]
  zi_header <- which(grepl("Zero-inflation", v, fixed = TRUE))
  zi_rows <- which(v %in% c("(Intercept)", "ment"))
  zi_rows <- zi_rows[zi_rows > zi_header]
  expect_length(zi_rows, 2L)
  # No AME on the zero rows: the AME is the combined effect on E(Y),
  # reported on the count rows only.
  expect_false(any(grepl("[0-9]", df[[ame_col]][zi_rows])))
  count_ment <- which(v == "ment")
  count_ment <- count_ment[count_ment < zi_header]
  expect_true(all(grepl("[0-9]", df[[ame_col]][count_ment])))
})

## ---- 9. Two-part models side by side ----------------------------------------
# The zero parts below are fitted on glmmTMB's Salamanders with
# `zi = ~ mined`, one binary factor: the zero model is saturated, so its
# coefficients are the empirical log-odds of the event it models -- an
# oracle by hand, independent of both engines. pscl's zero hurdle models
# Pr(y > 0), glmmTMB's Pr(y = 0): the same numbers with opposite signs.

.cb_two_part <- function(kind) {
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("pscl")
  d <- glmmTMB::Salamanders
  switch(
    kind,
    p_hu = pscl::hurdle(count ~ mined | mined, data = d, dist = "negbin"),
    p_zi = pscl::zeroinfl(count ~ mined | mined, data = d, dist = "negbin"),
    t_hu = glmmTMB::glmmTMB(
      count ~ mined,
      ziformula = ~mined,
      data = d,
      family = glmmTMB::truncated_nbinom2
    ),
    t_zi = glmmTMB::glmmTMB(
      count ~ mined,
      ziformula = ~mined,
      data = d,
      family = glmmTMB::nbinom2
    )
  )
}

# The empirical log-odds of y > 0 in Salamanders: at mined = "yes", the
# reference level, and the contrast of mined = "no".
.cb_logit_nonzero <- function() {
  d <- glmmTMB::Salamanders
  p <- tapply(d$count > 0, d$mined, mean)
  c(
    "(Intercept)" = stats::qlogis(p[["yes"]]),
    "mined: no" = stats::qlogis(p[["no"]]) - stats::qlogis(p[["yes"]])
  )
}

# The estimates one block shows in one model's first column, named by row
# label, read off the data.frame view of the table.
.cb_block_cells <- function(df, block, model) {
  col <- grep(paste0("^", model, ": "), names(df), value = TRUE)[1L]
  rows <- df[df$variable %in% block & df$.row_role %in% "level", , drop = FALSE]
  stats::setNames(rows[[col]], rows$level)
}

test_that("a pscl and a glmmTMB hurdle keep their zero parts in two blocks", {
  p_hu <- .cb_two_part("p_hu")
  t_hu <- .cb_two_part("t_hu")
  oracle <- .cb_logit_nonzero()
  # The pscl block keeps its published identity, "Zero hurdle"; its
  # caption names the event it models.
  pos <- "Zero hurdle"
  pos_cap <- "Zero hurdle, Pr(y > 0)"
  zero <- "Zero hurdle, Pr(y = 0)"
  for (exp_on in c(FALSE, TRUE)) {
    on_scale <- if (exp_on) exp else identity
    for (ord in list(c("pscl", "glmmTMB"), c("glmmTMB", "pscl"))) {
      lab <- paste(c(ord, if (exp_on) "exponentiated"), collapse = ", ")
      tab <- table_regression(
        list(pscl = p_hu, glmmTMB = t_hu)[ord],
        exponentiate = exp_on
      )
      df <- as.data.frame(tab)
      # Each block holds its own model's zero part, and only that one.
      expect_equal(
        .cb_block_cells(df, pos, "pscl"),
        on_scale(oracle),
        tolerance = 1e-5,
        label = lab
      )
      expect_equal(
        .cb_block_cells(df, zero, "glmmTMB"),
        on_scale(-oracle),
        tolerance = 1e-5,
        label = lab
      )
      expect_true(all(is.na(.cb_block_cells(df, pos, "glmmTMB"))), label = lab)
      expect_true(all(is.na(.cb_block_cells(df, zero, "pscl"))), label = lab)
      # One header per block.
      v <- trimws(tab$Variable)
      expect_identical(sum(v == paste0(pos_cap, ":")), 1L, label = lab)
      expect_identical(sum(v == paste0(zero, ":")), 1L, label = lab)
      # Each block is glossed with its own event, whatever the order.
      note <- attr(tab, "note")
      expect_match(
        note,
        paste0(pos_cap, " component: log-odds of a nonzero count."),
        fixed = TRUE,
        label = lab
      )
      expect_match(
        note,
        paste0(zero, " component: log-odds of a zero count."),
        fixed = TRUE,
        label = lab
      )
    }
  }
})

test_that("the two zero hurdles keep their captions and glosses in French", {
  p_hu <- .cb_two_part("p_hu")
  t_hu <- .cb_two_part("t_hu")
  withr::local_options(spicy.language = "fr")
  tab <- table_regression(list(pscl = p_hu, glmmTMB = t_hu))
  v <- trimws(tab$Variable)
  expect_identical(sum(startsWith(v, "Hurdle (z\u00e9ro), Pr(y > 0)")), 1L)
  expect_identical(sum(startsWith(v, "Hurdle (z\u00e9ro), Pr(y = 0)")), 1L)
  note <- attr(tab, "note")
  expect_match(
    note,
    paste0(
      "Composante \u00ab\u00a0Hurdle (z\u00e9ro), Pr(y > 0)\u00a0\u00bb",
      "\u00a0: logarithme de la cote d'un effectif non nul."
    ),
    fixed = TRUE
  )
  expect_match(
    note,
    paste0(
      "Composante \u00ab\u00a0Hurdle (z\u00e9ro), Pr(y = 0)\u00a0\u00bb",
      "\u00a0: logarithme de la cote d'un effectif nul."
    ),
    fixed = TRUE
  )
})

test_that("two hurdles with different zero parts print both glosses, by model", {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl", envir = environment())
  m_b <- pscl::hurdle(art ~ fem | fem, data = bioChemists)
  m_p <- pscl::hurdle(
    art ~ fem | fem,
    data = bioChemists,
    zero.dist = "poisson"
  )
  # The engine's own record of each zero part.
  expect_identical(c(m_b$dist$zero, m_p$dist$zero), c("binomial", "poisson"))
  note <- attr(table_regression(list(Logit = m_b, Censored = m_p)), "note")
  expect_match(
    note,
    "Logit: Zero hurdle, Pr(y > 0) component: log-odds of a nonzero count.",
    fixed = TRUE
  )
  expect_match(
    note,
    paste0(
      "Censored: Zero hurdle, Pr(y > 0) component: right-censored Poisson ",
      "on the log scale."
    ),
    fixed = TRUE
  )
  # A gloss the models share prints once, unattributed.
  note_same <- attr(table_regression(list(A = m_b, B = m_b)), "note")
  hits <- gregexpr(
    "component: log-odds of a nonzero count.",
    note_same,
    fixed = TRUE
  )
  expect_identical(lengths(regmatches(note_same, hits)), 1L)
  expect_no_match(note_same, "A: Zero hurdle", fixed = TRUE)
})

test_that("a zero-inflated and a hurdle fit fill only their own blocks", {
  oracle <- .cb_logit_nonzero()
  zi_blk <- "Zero-inflation"
  cases <- list(
    glmmTMB = list(
      zi = .cb_two_part("t_zi"),
      hu = .cb_two_part("t_hu"),
      hu_blk = "Zero hurdle, Pr(y = 0)",
      hu_cap = "Zero hurdle, Pr(y = 0)",
      hu_sign = -1
    ),
    pscl = list(
      zi = .cb_two_part("p_zi"),
      hu = .cb_two_part("p_hu"),
      hu_blk = "Zero hurdle",
      hu_cap = "Zero hurdle, Pr(y > 0)",
      hu_sign = 1
    )
  )
  for (eng in names(cases)) {
    cs <- cases[[eng]]
    # The zero-inflation part has no closed form: the engine's own
    # estimates are its oracle.
    zi_coef <- if (identical(eng, "glmmTMB")) {
      glmmTMB::fixef(cs$zi)$zi
    } else {
      stats::coef(cs$zi, model = "zero")
    }
    zi_oracle <- stats::setNames(
      unname(zi_coef),
      c("(Intercept)", "mined: no")
    )
    for (ord in list(c("ZI", "HU"), c("HU", "ZI"))) {
      lab <- paste(eng, paste(ord, collapse = " + "))
      tab <- table_regression(list(ZI = cs$zi, HU = cs$hu)[ord])
      df <- as.data.frame(tab)
      expect_equal(
        .cb_block_cells(df, zi_blk, "ZI"),
        zi_oracle,
        tolerance = 1e-9,
        label = lab
      )
      expect_true(all(is.na(.cb_block_cells(df, zi_blk, "HU"))), label = lab)
      expect_equal(
        .cb_block_cells(df, cs$hu_blk, "HU"),
        cs$hu_sign * oracle,
        tolerance = 1e-5,
        label = lab
      )
      expect_true(
        all(is.na(.cb_block_cells(df, cs$hu_blk, "ZI"))),
        label = lab
      )
      # Each block is one run under one header: no row of one block
      # interleaved with the other's.
      v <- trimws(tab$Variable)
      for (blk in c(zi_blk, cs$hu_cap)) {
        i <- which(v == paste0(blk, ":"))
        expect_length(i, 1L)
        expect_identical(
          v[i + 1:3],
          c("(Intercept)", "mined: yes (ref.)", "mined: no"),
          label = paste(lab, blk)
        )
      }
    }
  }
})

test_that("the zero-inflation parts of pscl and glmmTMB share their rows", {
  skip_if_not_installed("broom")
  p_zi <- .cb_two_part("p_zi")
  t_zi <- .cb_two_part("t_zi")
  tab <- table_regression(list(pscl = p_zi, glmmTMB = t_zi))
  df <- as.data.frame(tab)
  p <- .cb_block_cells(df, "Zero-inflation", "pscl")
  g <- .cb_block_cells(df, "Zero-inflation", "glmmTMB")
  # One row per coefficient, each column holding its engine's estimate.
  expect_identical(names(p), c("(Intercept)", "mined: no"))
  expect_equal(
    unname(p),
    unname(stats::coef(p_zi, model = "zero")),
    tolerance = 1e-9
  )
  expect_equal(unname(g), unname(glmmTMB::fixef(t_zi)$zi), tolerance = 1e-9)
  # The two engines fit the same model.
  expect_equal(p, g, tolerance = 1e-5)
  zi_rows <- df[df$variable %in% "Zero-inflation", , drop = FALSE]
  expect_identical(nrow(zi_rows), 4L) # header, intercept, reference, no
  # tidy() keys each zero row by its block and its coefficient.
  td <- broom::tidy(tab)
  zr <- td[td$factor_term %in% "Zero-inflation", ]
  expect_identical(nrow(zr), 4L)
  expect_setequal(
    zr$term,
    c("Zero-inflation::(Intercept)", "Zero-inflation::minedno")
  )
})

test_that("a count-type hurdle zero part is named in words, in each language", {
  skip_if_not_installed("pscl")
  data("bioChemists", package = "pscl", envir = environment())
  words <- c(
    poisson = "Poisson",
    negbin = "negative binomial",
    geometric = "geometric"
  )
  words_fr <- c(
    poisson = "de Poisson",
    negbin = "binomiale n\u00e9gative",
    geometric = "g\u00e9om\u00e9trique"
  )
  for (zd in names(words)) {
    m <- suppressWarnings(
      pscl::hurdle(art ~ fem | fem, data = bioChemists, zero.dist = zd)
    )
    # The engine's record of the distribution of its zero part.
    expect_identical(m$dist$zero, zd)
    note <- attr(table_regression(m), "note")
    expect_match(
      note,
      paste0("component: right-censored ", words[[zd]], " on the log scale."),
      fixed = TRUE,
      label = zd
    )
    note_fr <- withr::with_options(
      list(spicy.language = "fr"),
      attr(table_regression(m), "note")
    )
    expect_match(
      note_fr,
      paste0("loi ", words_fr[[zd]], " censur\u00e9e \u00e0 droite"),
      fixed = TRUE,
      label = zd
    )
  }
  # A code pscl may add later prints as it is.
  expect_identical(spicy:::.pscl_zero_dist_word("other"), "other")
})

test_that("the zero blocks of mixed two-part tables hold in French", {
  t_zi <- .cb_two_part("t_zi")
  t_hu <- .cb_two_part("t_hu")
  p_zi <- .cb_two_part("p_zi")
  # The factor rows keep their ASCII identifier in French.
  fr_names <- c("(Intercept)", "mined: no")
  zi_oracle <- stats::setNames(unname(glmmTMB::fixef(t_zi)$zi), fr_names)
  withr::local_options(spicy.language = "fr")
  zi_fr <- "Inflation de z\u00e9ros"
  hu_fr <- "Hurdle (z\u00e9ro), Pr(y = 0)"
  # A zero-inflated and a hurdle glmmTMB fit: two blocks under their
  # French captions, each holding its own model's zero part only.
  tab <- table_regression(list(ZI = t_zi, HU = t_hu))
  v <- trimws(tab$Variable)
  expect_identical(sum(startsWith(v, zi_fr)), 1L)
  expect_identical(sum(startsWith(v, hu_fr)), 1L)
  df <- as.data.frame(tab)
  zero <- "Zero hurdle, Pr(y = 0)"
  expect_equal(
    .cb_block_cells(df, "Zero-inflation", "ZI"),
    zi_oracle,
    tolerance = 1e-9
  )
  expect_true(all(is.na(.cb_block_cells(df, "Zero-inflation", "HU"))))
  expect_equal(
    .cb_block_cells(df, zero, "HU"),
    stats::setNames(-.cb_logit_nonzero(), fr_names),
    tolerance = 1e-5
  )
  expect_true(all(is.na(.cb_block_cells(df, zero, "ZI"))))
  # pscl and glmmTMB zero-inflated fits: one block, rows shared.
  tab2 <- table_regression(list(pscl = p_zi, glmmTMB = t_zi))
  v2 <- trimws(tab2$Variable)
  expect_identical(sum(startsWith(v2, zi_fr)), 1L)
  df2 <- as.data.frame(tab2)
  expect_equal(
    .cb_block_cells(df2, "Zero-inflation", "pscl"),
    zi_oracle,
    tolerance = 1e-5
  )
  expect_equal(
    .cb_block_cells(df2, "Zero-inflation", "glmmTMB"),
    zi_oracle,
    tolerance = 1e-9
  )
})

test_that("a component row keeps one level identifier in every language", {
  skip_if_not_installed("broom")
  # The level of a zero-part factor row ("mined: no") is what inline() and
  # tidy() address: it never follows the table language. The oracle is
  # each engine's own zero-part coefficient, written with the French
  # decimal mark.
  oracle <- list(
    p_zi = function(m) stats::coef(m, model = "zero")[["minedno"]],
    t_zi = function(m) glmmTMB::fixef(m)$zi[["minedno"]],
    t_hu = function(m) glmmTMB::fixef(m)$zi[["minedno"]]
  )
  block <- c(
    p_zi = "Zero-inflation",
    t_zi = "Zero-inflation",
    t_hu = "Zero hurdle, Pr(y = 0)"
  )
  for (kind in names(oracle)) {
    fit <- .cb_two_part(kind)
    lvl_en <- broom::tidy(table_regression(fit))$factor_level
    withr::with_options(list(spicy.language = "fr"), {
      tab <- table_regression(fit)
      expect_identical(
        inline(tab, !!block[[kind]], "mined: no", "b"),
        formatC(
          oracle[[kind]](fit),
          format = "f",
          digits = 2,
          decimal.mark = ","
        ),
        label = kind
      )
      expect_identical(broom::tidy(tab)$factor_level, lvl_en, label = kind)
    })
  }
})

test_that("a component row shows the colon of the table language", {
  # Display only: the Variable cell of a zero-part or dispersion row
  # writes "mined: no" with the separator of the main block's factor
  # header in the same table ("mined" + header suffix), while the level
  # identifier stays "mined: no" (the test above).
  skip_if_not_installed("glmmTMB")
  fit <- glmmTMB::glmmTMB(
    count ~ mined,
    ziformula = ~mined,
    dispformula = ~mined,
    data = glmmTMB::Salamanders,
    family = glmmTMB::truncated_nbinom2
  )
  for (lang in c("en", "fr")) {
    withr::with_options(list(spicy.language = lang), {
      tab <- table_regression(fit)
      df <- as.data.frame(tab)
      v <- trimws(tab$Variable)
      main <- v[df$variable == "mined" & df$.row_role == "factor_header"]
      comp <- df$variable %in% c("Zero hurdle, Pr(y = 0)", "Dispersion")
      expect_identical(
        v[comp & df$.row_role == "level" & df$level == "mined: no"],
        rep(paste(main, "no"), 2L),
        label = lang
      )
      expect_identical(
        sum(startsWith(
          v[comp & df$.row_role == "reference"],
          paste(main, "yes")
        )),
        2L,
        label = lang
      )
      expect_identical(
        df$level[comp & df$.row_role != "factor_header"],
        rep(c("(Intercept)", "mined: yes", "mined: no"), 2L),
        label = lang
      )
    })
  }
  # In French the separator is the no-break space and colon of the header.
  withr::local_options(spicy.language = "fr")
  expect_true(any(trimws(table_regression(fit)$Variable) == "mined\u00a0: no"))
})

test_that("the reference footer never reads a block caption as an equation", {
  fit <- .cb_two_part("t_hu")
  note <- attr(table_regression(fit, reference_style = "footer"), "note")
  expect_match(
    note,
    "Reference categories: mined = yes; Zero hurdle, Pr(y = 0): mined: yes.",
    fixed = TRUE
  )
  expect_no_match(note, "Pr(y = 0) =", fixed = TRUE)
  withr::local_options(spicy.language = "fr")
  note_fr <- attr(table_regression(fit, reference_style = "footer"), "note")
  expect_match(
    note_fr,
    paste0(
      "mined = yes\u00a0; Hurdle (z\u00e9ro), Pr(y = 0)\u00a0: ",
      "mined: yes."
    ),
    fixed = TRUE
  )
  expect_no_match(note_fr, "Pr(y = 0) =", fixed = TRUE)
})
