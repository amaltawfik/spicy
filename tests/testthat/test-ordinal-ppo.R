# Partial proportional odds (ordinal::clm with nominal = ~): the non-
# proportional terms render as a "Non-proportional effects" block of rows (one
# coefficient per cut-point), cross-validated to summary.clm; the baseline
# thresholds stay a separate block; robust SEs are refused; profile CIs cover
# the PO terms only.

ppo_fit <- function() {
  skip_if_not_installed("ordinal")
  data(wine, package = "ordinal", envir = environment())
  ordinal::clm(rating ~ temp, nominal = ~ contact, data = wine)
}
b_rows <- function(fr) fr$coefs[fr$coefs$estimate_type == "B", , drop = FALSE]

test_that("PPO clm is tabulated with a Non-proportional effects block", {
  fit <- ppo_fit()
  df <- table_regression(fit, output = "data.frame")
  v <- trimws(df$Variable)
  expect_true(any(grepl("^Non-proportional effects", v)))       # block header
  expect_true(any(grepl("contactyes @", v)))                    # per-cut rows
  expect_true(any(grepl("^Thresholds", v)))                     # thresholds kept
  expect_true(any(v == "warm"))                                 # PO term
})

test_that("non-proportional coefficients match summary.clm to machine precision", {
  fit <- ppo_fit()
  fr <- as_regression_frame(fit, show_columns = expand_show_columns("b"),
                            ci_level = 0.95)
  np <- fr$coefs[fr$coefs$parent_var == "Non-proportional effects", ]
  sm <- summary(fit)$coefficients
  expect_gt(nrow(np), 0L)
  expect_equal(np$estimate,  unname(sm[np$term, "Estimate"]),   tolerance = 1e-9)
  expect_equal(np$std_error, unname(sm[np$term, "Std. Error"]), tolerance = 1e-9)
  expect_equal(np$p_value,   unname(sm[np$term, "Pr(>|z|)"]),   tolerance = 1e-9)
  # PO location coef matches fit$beta
  po <- fr$coefs[fr$coefs$term == "tempwarm", ]
  expect_equal(po$estimate, unname(fit$beta["tempwarm"]), tolerance = 1e-9)
})

test_that("thresholds block is the baseline cut-points only (not expanded)", {
  fit <- ppo_fit()
  thr <- as_regression_frame(fit)$info$extras$thresholds
  # 4 baseline thresholds for a 5-level response, labelled by the bare cut-point
  expect_equal(nrow(thr), 4L)
  expect_false(any(grepl("Intercept|contact", thr$term)))
})

test_that("exponentiate: non-proportional cells become OR; thresholds stay log", {
  fit <- ppo_fit()
  frx <- as_regression_frame(fit, show_columns = expand_show_columns("b"),
                             exponentiate = FALSE)
  frx <- spicy:::.apply_exp_to_frame(frx$coefs, frx$info, exponentiate = TRUE)
  np <- frx$coefs[frx$coefs$parent_var == "Non-proportional effects", ]
  fr0 <- as_regression_frame(fit, show_columns = expand_show_columns("b"))
  np0 <- fr0$coefs[fr0$coefs$parent_var == "Non-proportional effects", ]
  expect_equal(np$estimate, exp(np0$estimate), tolerance = 1e-9)   # OR
  # rendered table: thresholds stay on the log-odds scale (negative allowed)
  df <- table_regression(fit, exponentiate = TRUE, output = "data.frame")
  thr <- df[trimws(df$Variable) == "1 | 2", ]
  est_col <- setdiff(names(df), "Variable")[1]
  expect_lt(as.numeric(trimws(thr[[est_col]])), 0)                # not exp()d
})

test_that("profile CIs cover the PO terms; non-proportional terms stay Wald", {
  fit <- ppo_fit()
  frp <- as_regression_frame(fit, ci_method = "profile")
  po <- frp$coefs[frp$coefs$term == "tempwarm", ]
  oracle <- suppressMessages(stats::confint(fit))       # PO terms only
  expect_equal(c(po$ci_lower, po$ci_upper), unname(oracle["tempwarm", ]),
               tolerance = 1e-6)
  # a non-proportional row keeps its Wald CI
  np <- frp$coefs[frp$coefs$term == "2|3.contactyes", ]
  z <- stats::qnorm(0.975)
  expect_equal(c(np$ci_lower, np$ci_upper),
               np$estimate + c(-1, 1) * z * np$std_error, tolerance = 1e-9)
})

test_that("a robust vcov is refused for a PPO fit", {
  fit <- ppo_fit()
  data(wine, package = "ordinal", envir = environment())
  expect_error(
    table_regression(fit, vcov = "CR2", cluster = wine$judge,
                     output = "data.frame"),
    class = "spicy_unsupported_vcov")
})

test_that("the title reports partial proportional odds", {
  fit <- ppo_fit()
  expect_match(as_regression_frame(fit)$info$extras$title_prefix,
               "partial proportional odds", fixed = TRUE)
})
