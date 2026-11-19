# The standard deviation of a pdCompSymm block of an lme fit, shared by
# its terms, has a Wald SE and CI as the sd rows of the other structures
# do. nlme estimates it on log(sd), and with natural = TRUE fit$apVar
# holds the variance of that log directly: the yardstick below reads it
# there, not off intervals().

test_that("the sd rows of a pdCompSymm block carry the SE of log(sd) from apVar", {
  skip_if_not_installed("nlme")
  fit <- nlme::lme(
    yield ~ nitro,
    data = nlme::Oats,
    random = list(Block = nlme::pdCompSymm(~ Variety - 1))
  )
  vc <- as_regression_frame(
    fit,
    model_id = "M1"
  )$info$random_effects$variance_components
  sd_rows <- vc[vc$group == "Block" & !(vc$is_correlation %in% TRUE), ]
  expect_identical(nrow(sd_rows), 3L)

  av <- fit$apVar
  expect_identical(rownames(av)[1L], "reStruct.Block1")
  sd_hat <- exp(attr(av, "Pars")[["reStruct.Block1"]])
  expect_equal(sd_rows$sd, rep(sd_hat, 3L), tolerance = 1e-6)
  # Variance scale: SE(sd^2) = 2 sd SE(sd) = 2 sd^2 SE(log sd).
  se_var <- 2 * sd_hat^2 * sqrt(av[1L, 1L])
  expect_equal(sd_rows$std_error, rep(se_var, 3L), tolerance = 1e-6)
  z <- stats::qnorm(0.975)
  half <- z * sqrt(av[1L, 1L])
  expect_equal(
    sd_rows$ci_lower,
    rep((sd_hat * exp(-half))^2, 3L),
    tolerance = 1e-6
  )
  expect_equal(
    sd_rows$ci_upper,
    rep((sd_hat * exp(half))^2, 3L),
    tolerance = 1e-6
  )

  # The SE shown on the sd scale in the table.
  tbl <- table_regression(fit, output = "data.frame")
  se_rows <- tbl$SE[
    tbl$.row_role == "vc" & grepl("Block Variety", tbl$level, fixed = TRUE)
  ]
  expect_equal(se_rows, rep(sd_hat * sqrt(av[1L, 1L]), 3L), tolerance = 1e-6)
})
