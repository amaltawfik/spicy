# Robust and cluster-robust variances of an ivreg fit: HC0 / HC1 by
# sandwich::vcovHC(), CR0-CR3 and CR1S by clubSandwich::vcovCR(), as for
# an lm. Oracles: sandwich and clubSandwich called directly,
# estimatr::iv_robust() on the same model, and the CR3 matrix and the
# Satterthwaite df rebuilt by hand from the second-stage design.

.ivrv_data <- function() {
  e <- new.env()
  utils::data("CigarettesSW", package = "AER", envir = e)
  d <- e$CigarettesSW
  d$rprice <- d$price / d$cpi
  d$rincome <- d$income / d$population / d$cpi
  d$tdiff <- (d$taxs - d$tax) / d$cpi
  d$rtax <- d$tax / d$cpi
  d$lpacks <- log(d$packs)
  d$lprice <- log(d$rprice)
  d$lincome <- log(d$rincome)
  d
}

.ivrv_formula <- lpacks ~ lprice + lincome | lincome + tdiff + rtax

# The formula is rebuilt here so that its environment holds `d`: a
# `cluster = ~state` is looked up in the fit's `data`.
.ivrv_fit <- function(d = .ivrv_data()) {
  f <- .ivrv_formula
  environment(f) <- environment()
  AER::ivreg(f, data = d)
}

.ivrv_rows <- function(fit, ...) {
  lo <- as.data.frame(table_regression(fit, output = "long", ...))
  lo <- lo[lo$estimate_type == "B" & !(lo$is_reference %in% TRUE), ]
  rownames(lo) <- lo$term
  lo[names(stats::coef(fit)), ]
}

test_that("HC0 and HC1 match sandwich::vcovHC() and estimatr::iv_robust()", {
  skip_if_not_installed("AER")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("estimatr")
  d <- .ivrv_data()
  fit <- .ivrv_fit(d)
  for (ty in c("HC0", "HC1")) {
    r <- .ivrv_rows(fit, vcov = ty)
    se_sw <- sqrt(diag(sandwich::vcovHC(fit, type = ty)))
    se_er <- estimatr::iv_robust(
      .ivrv_formula,
      data = d,
      se_type = ty
    )$std.error
    expect_equal(r$std.error, unname(se_sw), tolerance = 1e-10, info = ty)
    expect_equal(r$std.error, unname(se_er), tolerance = 1e-8, info = ty)
    expect_equal(r$df, rep(stats::df.residual(fit), 3), info = ty)
    expect_equal(
      r$p.value,
      2 *
        stats::pt(
          abs(r$estimate / se_sw),
          stats::df.residual(fit),
          lower.tail = FALSE
        ),
      tolerance = 1e-10,
      ignore_attr = TRUE,
      info = ty
    )
  }
})

test_that("CR0-CR3 match clubSandwich, and CR0 / CR2 match estimatr", {
  skip_if_not_installed("AER")
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("estimatr")
  d <- .ivrv_data()
  fit <- .ivrv_fit(d)
  for (ty in paste0("CR", 0:3)) {
    r <- .ivrv_rows(fit, vcov = ty, cluster = ~state)
    V <- clubSandwich::vcovCR(fit, type = ty, cluster = d$state)
    ct <- clubSandwich::coef_test(
      fit,
      vcov = V,
      cluster = d$state,
      test = "Satterthwaite"
    )
    expect_equal(r$std.error, ct$SE, tolerance = 1e-10, info = ty)
    expect_equal(r$df, ct$df_Satt, tolerance = 1e-10, info = ty)
    expect_equal(r$p.value, ct$p_Satt, tolerance = 1e-10, info = ty)
  }
  # estimatr: standard errors for CR0 and CR2, the Satterthwaite df for
  # CR2 (its CR0 df is G - 1 by convention, not a Satterthwaite df).
  for (ty in c("CR0", "CR2")) {
    r <- .ivrv_rows(fit, vcov = ty, cluster = ~state)
    er <- estimatr::iv_robust(
      .ivrv_formula,
      data = d,
      clusters = state,
      se_type = ty
    )
    expect_equal(r$std.error, unname(er$std.error), tolerance = 1e-8, info = ty)
    if (ty == "CR2") {
      expect_equal(r$df, unname(er$df), tolerance = 1e-8)
    }
  }
})

test_that("CR1S matches estimatr's stata standard errors and its G - 1 df", {
  skip_if_not_installed("AER")
  skip_if_not_installed("clubSandwich")
  skip_if_not_installed("estimatr")
  d <- .ivrv_data()
  fit <- .ivrv_fit(d)
  r <- .ivrv_rows(fit, vcov = "CR1S", cluster = ~state)
  er <- estimatr::iv_robust(
    .ivrv_formula,
    data = d,
    clusters = state,
    se_type = "stata"
  )
  expect_equal(r$std.error, unname(er$std.error), tolerance = 1e-8)
  expect_equal(r$df, unname(er$df))
  expect_equal(r$df, rep(length(unique(d$state)) - 1, 3))
})

test_that("CR1 and CR3 and the CR0 df agree with a hand computation", {
  skip_if_not_installed("AER")
  skip_if_not_installed("clubSandwich")
  d <- .ivrv_data()
  fit <- .ivrv_fit(d)
  Xh <- stats::model.matrix(fit, component = "projected")
  e <- stats::residuals(fit)
  B <- solve(crossprod(Xh))
  H <- Xh %*% B %*% t(Xh)
  M <- diag(nrow(Xh)) - H
  grp <- split(seq_len(nrow(Xh)), d$state)
  G <- length(grp)
  adj <- function(i, ty) {
    if (ty == "CR3") solve(diag(length(i)) - H[i, i]) else diag(length(i))
  }
  V_hand <- function(ty) {
    meat <- Reduce(
      `+`,
      lapply(grp, function(i) {
        u <- t(Xh[i, , drop = FALSE]) %*% adj(i, ty) %*% e[i]
        u %*% t(u)
      })
    )
    B %*% meat %*% B
  }
  # Satterthwaite df of a coefficient (Bell & McCaffrey 2002), under the
  # working model of independent homoskedastic errors.
  df_hand <- function(ty) {
    vapply(
      seq_len(ncol(Xh)),
      function(k) {
        P <- do.call(
          cbind,
          lapply(grp, function(i) {
            v <- adj(i, ty) %*% Xh[i, , drop = FALSE] %*% B[, k]
            drop(t(M[i, , drop = FALSE]) %*% v)
          })
        )
        S <- crossprod(P)
        sum(diag(S))^2 / sum(S^2)
      },
      numeric(1)
    )
  }
  r0 <- .ivrv_rows(fit, vcov = "CR0", cluster = ~state)
  r1 <- .ivrv_rows(fit, vcov = "CR1", cluster = ~state)
  r3 <- .ivrv_rows(fit, vcov = "CR3", cluster = ~state)
  expect_equal(
    r0$std.error,
    sqrt(diag(V_hand("CR0"))),
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
  expect_equal(
    r1$std.error,
    r0$std.error * sqrt(G / (G - 1)),
    tolerance = 1e-10
  )
  expect_equal(
    r3$std.error,
    sqrt(diag(V_hand("CR3"))),
    tolerance = 1e-10,
    ignore_attr = TRUE
  )
  expect_equal(r0$df, df_hand("CR0"), tolerance = 1e-8)
  expect_equal(r1$df, df_hand("CR0"), tolerance = 1e-8)
})

test_that("HC2-HC5 and the resamplers stay refused, with their reason", {
  skip_if_not_installed("AER")
  fit <- .ivrv_fit()
  for (ty in paste0("HC", 2:5)) {
    err <- expect_error(
      table_regression(fit, vcov = ty),
      class = "spicy_unsupported_vcov"
    )
    msg <- conditionMessage(err)
    expect_match(msg, "leverage", fixed = TRUE, info = ty)
    expect_match(msg, "classical, HC0, HC1", fixed = TRUE, info = ty)
  }
  for (ty in c("bootstrap", "jackknife")) {
    err <- expect_error(
      table_regression(fit, vcov = ty),
      class = "spicy_unsupported_vcov"
    )
    expect_match(conditionMessage(err), "two-stage", fixed = TRUE, info = ty)
  }
})

test_that("the note names the estimator and, under CR*, the df of the p-values", {
  skip_if_not_installed("AER")
  skip_if_not_installed("clubSandwich")
  fit <- .ivrv_fit()
  out_hc <- paste(
    capture.output(print(table_regression(fit, vcov = "HC1"))),
    collapse = "\n"
  )
  expect_match(out_hc, "heteroskedasticity-robust (HC1)", fixed = TRUE)
  out_cr <- paste(
    capture.output(print(table_regression(
      fit,
      vcov = "CR2",
      cluster = ~state
    ))),
    collapse = "\n"
  )
  expect_match(out_cr, "cluster-robust (CR2), clusters by state", fixed = TRUE)
  expect_match(
    out_cr,
    "P-values: Satterthwaite t-test, cluster-robust df.",
    fixed = TRUE
  )
  expect_no_match(out_hc, "Satterthwaite", fixed = TRUE)
})

test_that("the AME under HC1 and CR2 carries the robust matrix", {
  skip_if_not_installed("AER")
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("numDeriv")
  skip_if_not_installed("clubSandwich")
  d <- .ivrv_data()
  fit <- .ivrv_fit(d)
  rhs <- ~ lprice + lincome
  for (args in list(
    list(vcov = "HC1", V = sandwich::vcovHC(fit, type = "HC1")),
    list(
      vcov = "CR2",
      cluster = ~state,
      V = clubSandwich::vcovCR(fit, type = "CR2", cluster = d$state)
    )
  )) {
    or <- ame_oracle(
      ame_prep_rhs(rhs, d),
      ame_pf_link(identity),
      stats::coef(fit),
      args$V,
      d,
      c("lprice", "lincome")
    )
    sp <- do.call(ame_spicy, c(list(fit), args[setdiff(names(args), "V")]))
    expect_ame_matches(sp, or)
  }
})
