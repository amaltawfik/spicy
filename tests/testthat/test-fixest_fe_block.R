# fixest "Fixed effects" disclosure block, within_r2 and per-factor
# n_groups rows. Oracles: fixest::fixef() / fixef_sizes / n_distinct
# triangulation for counts, fixest::r2(fit, "wr2") for within R2,
# etable()'s own Yes/No matrix for presence, modelsummary's
# "FE: <factor>" rows where installed.
# Design record: dev/fixest_fe_block_spec.md.

.fe_trade <- function() {
  e <- new.env()
  data("trade", package = "fixest", envir = e)
  e$trade[e$trade$Year %in% 2015:2016, ]
}


test_that("default fixest table leads with the FE block, within R2 exact", {
  skip_if_not_installed("fixest")
  tr <- .fe_trade()
  fit <- fixest::feols(log(Euros) ~ log(dist_km) | Origin + Product, data = tr)
  fr <- suppressWarnings(as_regression_frame(fit))
  expect_identical(fr$info$extras$fixef_intercept, c("Origin", "Product"))
  # within R2 exact vs fixest's own fitstat.
  expect_equal(
    fr$info$fit_stats$pseudo_r2$within_r2,
    as.numeric(fixest::r2(fit, "wr2")),
    tolerance = 1e-12
  )
  out <- capture.output(print(suppressWarnings(table_regression(fit))))
  i_fe <- grep("Fixed effects:", out, fixed = TRUE)
  expect_length(i_fe, 1L)
  # Block sits at the TOP of the fit block: header precedes the n row.
  i_n <- grep("^ n\\s", out)[1]
  expect_lt(i_fe, i_n)
  expect_match(out[i_fe + 1L], "Origin")
  expect_match(out[i_fe + 1L], "Yes", fixed = TRUE)
  expect_match(out[i_fe + 2L], "Product")
  expect_match(out[i_fe + 2L], "Yes", fixed = TRUE)
  expect_match(paste(out, collapse = "\n"), "(within)", fixed = TRUE)
  # Structured path stays consistent (FE rows numeric-encoded).
  expect_silent(suppressWarnings(as_structured(table_regression(fit))))
})


test_that("slope-only factors read No; combined factors pass verbatim", {
  skip_if_not_installed("fixest")
  tr <- .fe_trade()
  # Origin[[Year]] absorbs a varying slope, NOT an Origin intercept --
  # fixef_sizes lists Origin anyway (the 2026-07 design-research trap).
  f_slope <- fixest::feols(
    log(Euros) ~
      log(dist_km) |
        Origin[[Year]] +
          Product,
    data = tr
  )
  fr <- suppressWarnings(as_regression_frame(f_slope))
  expect_identical(fr$info$extras$fixef_intercept, "Product")
  # Origin[Year] (bracket, not double bracket) = intercept AND slope.
  f_both <- fixest::feols(
    log(Euros) ~
      log(dist_km) |
        Origin[Year] +
          Product,
    data = tr
  )
  fr_b <- suppressWarnings(as_regression_frame(f_both))
  expect_identical(fr_b$info$extras$fixef_intercept, c("Origin", "Product"))
  # Combined factor names pass through verbatim.
  f_comb <- fixest::feols(log(Euros) ~ log(dist_km) | Origin^Product, data = tr)
  fr_c <- suppressWarnings(as_regression_frame(f_comb))
  expect_identical(fr_c$info$extras$fixef_intercept, "Origin^Product")
  # In a mixed FE + slope-only table, the slope-only column reads No
  # for Origin and Yes for Product.
  f_fe <- fixest::feols(log(Euros) ~ log(dist_km) | Origin + Product, data = tr)
  out <- capture.output(print(suppressWarnings(
    table_regression(list(FE = f_fe, Slope = f_slope))
  )))
  i_or <- grep("^\\s+Origin\\s", out)[1]
  expect_match(out[i_or], "Yes")
  expect_match(out[i_or], "No")
})


test_that("no-FE fixest fits drop the block; refusals fire without fixest", {
  skip_if_not_installed("fixest")
  tr <- .fe_trade()
  f0 <- fixest::feols(log(Euros) ~ log(dist_km), data = tr)
  out <- capture.output(print(suppressWarnings(table_regression(f0))))
  expect_false(any(grepl("Fixed effects:", out, fixed = TRUE)))
  # Gate: fixest-only tokens on a non-fixest table are a hard error.
  fl <- stats::lm(mpg ~ wt, data = mtcars)
  for (tk in c("fixed_effects", "within_r2")) {
    expect_error(
      table_regression(fl, show_fit_stats = c("nobs", tk)),
      "fixest",
      class = "spicy_invalid_input"
    )
  }
})


test_that("per-factor group counts triangulate against fixest::fixef", {
  skip_if_not_installed("fixest")
  tr <- .fe_trade()
  fit <- fixest::feols(log(Euros) ~ log(dist_km) | Origin + Product, data = tr)
  n_or <- length(fixest::fixef(fit)$Origin)
  n_pr <- length(fixest::fixef(fit)$Product)
  expect_identical(as.integer(fit$fixef_sizes["Origin"]), as.integer(n_or))
  expect_identical(as.integer(fit$fixef_sizes["Product"]), as.integer(n_pr))
  out <- capture.output(print(suppressWarnings(table_regression(
    fit,
    show_fit_stats = c("fixed_effects", "nobs", "n_groups")
  ))))
  i_or <- grep("N (Origin)", out, fixed = TRUE)
  i_pr <- grep("N (Product)", out, fixed = TRUE)
  expect_length(i_or, 1L)
  expect_length(i_pr, 1L)
  expect_match(out[i_or], as.character(n_or))
  expect_match(out[i_pr], as.character(n_pr))
})


test_that("presence matrix matches etable's own Yes/No output", {
  skip_if_not_installed("fixest")
  tr <- .fe_trade()
  f1 <- fixest::feols(log(Euros) ~ log(dist_km) | Origin + Product, data = tr)
  f2 <- fixest::feols(log(Euros) ~ log(dist_km) | Origin, data = tr)
  et <- fixest::etable(f1, f2)
  ours <- spicy:::.fixed_effects_cells(
    list(M1 = c("Origin", "Product"), M2 = "Origin"),
    c("M1", "M2")
  )
  expect_identical(unname(ours$cells["Origin", ]), c("Yes", "Yes"))
  expect_identical(unname(ours$cells["Product", ]), c("Yes", "No"))
  # etable agrees cell for cell: its factor labels live in the FIRST
  # column (rownames are just indices), model cells in columns 2-3.
  lab <- trimws(et[[1L]])
  or_row <- et[lab == "Origin", , drop = FALSE]
  pr_row <- et[lab == "Product", , drop = FALSE]
  expect_identical(trimws(unname(unlist(or_row[1L, 2:3]))), c("Yes", "Yes"))
  expect_identical(trimws(unname(unlist(pr_row[1L, 2:3]))), c("Yes", "No"))
})


test_that("modelsummary FE rows agree where installed", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("modelsummary")
  tr <- .fe_trade()
  f1 <- fixest::feols(log(Euros) ~ log(dist_km) | Origin + Product, data = tr)
  gof <- modelsummary::get_gof(f1)
  fe_cols <- names(gof)[grepl("^FE", names(gof))]
  # modelsummary marks both absorbed factors; so do we.
  expect_length(fe_cols, 2L)
  fr <- suppressWarnings(as_regression_frame(f1))
  expect_identical(
    sort(sub("^FE: ", "", fe_cols)),
    sort(fr$info$extras$fixef_intercept)
  )
})


test_that("default glm + fixest tables render (the all-glm proxy trap)", {
  skip_if_not_installed("fixest")
  tr <- .fe_trade()
  # The historical any/any "all_glm" proxy classified {glm, fixest} as
  # all-glm and rejected the package's OWN default vector ("r2" from
  # the fixest arm) -- the FE-lot review caught the default table
  # hard-erroring. True-ALL gating renders it instead.
  tr$high <- as.integer(log(tr$Euros) > median(log(tr$Euros)))
  g <- stats::glm(high ~ log(dist_km), data = tr, family = binomial())
  fe <- fixest::feols(log(Euros) ~ log(dist_km) | Origin, data = tr)
  out <- capture.output(print(suppressWarnings(
    table_regression(list(Logit = g, FE = fe))
  )))
  expect_true(any(grepl("Fixed effects:", out, fixed = TRUE)))
  # fepois default carries fixest's McFadden pr2 (spec decision 7).
  fp <- fixest::fepois(Euros ~ log(dist_km) | Origin + Product, data = tr)
  fr <- suppressWarnings(as_regression_frame(fp))
  pr2_oracle <- as.numeric(fixest::fitstat(fp, "pr2", simplify = TRUE))
  expect_equal(
    fr$info$fit_stats$pseudo_r2_mcfadden,
    pr2_oracle,
    tolerance = 1e-12
  )
  outp <- capture.output(print(suppressWarnings(table_regression(fp))))
  expect_true(any(grepl("McFadden", outp, fixed = TRUE)))
})


test_that("structured and rich engines encode the FE cells correctly", {
  skip_if_not_installed("fixest")
  tr <- .fe_trade()
  f_fe <- fixest::feols(log(Euros) ~ log(dist_km) | Origin + Product, data = tr)
  f_or <- fixest::feols(log(Euros) ~ log(dist_km) | Origin, data = tr)
  fl <- stats::lm(log(Euros) ~ log(dist_km), data = tr)
  tb <- suppressWarnings(table_regression(
    list(OLS = fl, FE = f_fe, OrOnly = f_or)
  ))
  # Machine contract: numeric 1 / 0 / NA under "FE: <factor>" labels.
  s <- as_structured(tb)
  b <- s$body
  pr <- b[trimws(b$Variable) == "FE: Product", ]
  expect_identical(nrow(pr), 1L)
  vals <- suppressWarnings(as.numeric(unlist(pr[1, -1])))
  vals <- vals[!is.nan(vals)]
  expect_true(1 %in% vals) # FE model absorbs Product
  expect_true(0 %in% vals) # OrOnly does not
  # Console: the lm column stays blank on FE rows, and the block
  # leads the fit stats in the mixed default too.
  out <- capture.output(print(tb))
  i_fe <- grep("Fixed effects:", out, fixed = TRUE)
  expect_length(i_fe, 1L)
  expect_lt(i_fe, grep("^ n\\s", out)[1])
  # Rich engines display Yes/No, never the numeric encoding.
  skip_if_not_installed("tinytable")
  tt <- suppressWarnings(table_regression(
    list(OLS = fl, FE = f_fe, OrOnly = f_or),
    output = "tinytable"
  ))
  tt_chr <- paste(capture.output(print(tt)), collapse = "\n")
  expect_match(tt_chr, "Yes")
  expect_match(tt_chr, "No")
})


test_that("mixed-model n_groups still renders, one row per factor", {
  skip_if_not_installed("lme4")
  # Single shared factor: historical "N (Subject)" row unchanged.
  fit1 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  out1 <- capture.output(print(suppressWarnings(table_regression(fit1))))
  expect_length(grep("N (Subject)", out1, fixed = TRUE), 1L)
  # Crossed factors: one row per factor (was one crammed cell).
  fit2 <- suppressMessages(lme4::lmer(
    diameter ~ 1 + (1 | plate) + (1 | sample),
    data = lme4::Penicillin
  ))
  out2 <- capture.output(print(suppressWarnings(table_regression(fit2))))
  expect_length(grep("N (plate)", out2, fixed = TRUE), 1L)
  expect_length(grep("N (sample)", out2, fixed = TRUE), 1L)
  expect_match(out2[grep("N (plate)", out2, fixed = TRUE)], "24")
  expect_match(out2[grep("N (sample)", out2, fixed = TRUE)], "6")
})
