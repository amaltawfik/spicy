# The column `vcov` of the registry is the contract of each class: on the
# fixtures of helper-class-fixtures.R, a value of .VCOV_MODES is accepted
# if and only if the cell of the class lists it, and a refusal is a
# classed error. dev/decisions/2026-11-19-capability-registry.md.

# The values a fixture refuses although its class cell lists them, each
# with its reason: the parenthesis of the cell, for the fits it names.
vc_refused <- list(
  glmmTMB_reml = list(
    vcov = c("CR0", "CR1"),
    why = "glmmTMB's sandwich is defined for maximum-likelihood fits only"
  ),
  glmmTMB_ordinal = list(
    vcov = c("CR0", "CR1"),
    why = "glmmTMB's sandwich does not cover the cut-points"
  ),
  lme_contrasts = list(
    vcov = c("CR0", "CR1", "CR2", "CR3"),
    why = "clubSandwich rebuilds the design with the session contrasts"
  ),
  clm_scale = list(
    vcov = "CR1",
    why = "ordinal has no estimating functions for the scale part"
  ),
  clm_nominal = list(
    vcov = "CR1",
    why = "ordinal has no estimating functions for the nominal part"
  ),
  zeroinfl_negbin = list(
    vcov = "CR1",
    why = "the sandwich of pscl leaves out the score of theta"
  ),
  hurdle_negbin = list(
    vcov = "CR1",
    why = "the sandwich of pscl leaves out the score of theta"
  ),
  cph = list(
    vcov = "CR0",
    why = "rms::robcov() needs a fit with x = TRUE, y = TRUE"
  ),
  ols = list(
    vcov = "CR0",
    why = "rms::robcov() needs a fit with x = TRUE, y = TRUE"
  ),
  lrm = list(
    vcov = "CR0",
    why = "rms::robcov() needs a fit with x = TRUE, y = TRUE"
  ),
  lrm_ordinal = list(
    vcov = "CR0",
    why = "rms::robcov() needs a fit with x = TRUE, y = TRUE"
  ),
  rms_Glm = list(
    vcov = "CR0",
    why = "rms::robcov() needs a fit with x = TRUE, y = TRUE"
  ),
  rms_Glm_logit = list(
    vcov = "CR0",
    why = "rms::robcov() needs a fit with x = TRUE, y = TRUE"
  )
)

vc_call <- function(fit, id, mode) {
  args <- list(fit, vcov = mode, output = "data.frame")
  clu <- .CLASS_FIXTURES[[id]]$cluster
  if (grepl("^CR", mode) && !is.null(clu)) {
    args$cluster <- if (is.function(clu)) clu(fit) else clu
  }
  if (mode == "bootstrap") {
    args$boot_n <- 20L
  }
  suppressWarnings(suppressMessages(do.call(table_regression, args)))
}

test_that("every vcov cell parses to values of .VCOV_MODES", {
  reg <- .supported_models_registry()
  for (cell in reg$vcov) {
    toks <- .reg_cell_tokens(cell)
    expect_true("classical" %in% toks, label = cell)
    expect_true(all(toks %in% .VCOV_MODES), label = cell)
  }
  expect_true(all(names(vc_refused) %in% names(.CLASS_FIXTURES)))
  for (id in names(vc_refused)) {
    cl <- .CLASS_FIXTURES[[id]]$class
    expect_true(all(
      vc_refused[[id]]$vcov %in% .reg_cell_tokens(reg$vcov[reg$class == cl])
    ))
  }
})

for (id in names(.CLASS_FIXTURES)) {
  local({
    id <- id
    test_that(paste0("the registry gives the vcov values of ", id), {
      skip_on_cran()
      fit <- .class_fixture(id)
      reg <- .supported_models_registry()
      cl <- .CLASS_FIXTURES[[id]]$class
      ok <- setdiff(
        .reg_cell_tokens(reg$vcov[reg$class == cl]),
        vc_refused[[id]]$vcov
      )
      for (m in .VCOV_MODES) {
        res <- tryCatch(
          {
            vc_call(fit, id, m)
            "accepted"
          },
          spicy_error = function(e) "refused"
        )
        expect_identical(
          res,
          if (m %in% ok) "accepted" else "refused",
          label = paste(id, m)
        )
      }
    })
  })
}
