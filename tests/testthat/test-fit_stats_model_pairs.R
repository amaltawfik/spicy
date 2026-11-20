# The tokens that validate_class_appropriate_tokens() refuses when no
# model of the table carries them, asked of two-model tables: the sweeps
# of test-fit_stats_requested_rows.R ask one model at a time, and these
# refusals are about the table. Each pair crosses the families of these
# controls; the snapshots freeze which tokens give a row and the text and
# class of every refusal (helper-refusal-log.R).

fp_pairs <- list(
  c("lm", "glm_logit"),
  c("glm_poisson", "glm.nb"),
  c("glmmTMB_nbinom1", "glmmTMB_nbinom2"),
  c("betareg", "gam_betar"),
  c("coxph", "cph"),
  c("svycoxph", "svyglm_binomial"),
  c("lm", "svyglm_binomial"),
  c("feols_nofe", "lm_robust_fe"),
  c("fepois", "lm"),
  c("geeglm", "glm_poisson"),
  c("lrm_ordinal", "rms_Glm_logit"),
  c("gam", "glmmTMB_beta"),
  c("zeroinfl", "hurdle_negbin")
)

fp_tokens <- c(
  "pseudo_r2_ferrari",
  "concordance",
  "fixed_effects",
  "within_r2",
  "qic",
  "qicu",
  "scale",
  "max_cluster_size",
  "theta",
  "alpha",
  "delta",
  "phi",
  "pseudo_r2_tjur",
  "r2_deviance"
)

fp_ids <- vapply(fp_pairs, paste, character(1), collapse = " + ")

.refusal_reset("fit_stats_pairs")
fp_rows <- new.env(parent = emptyenv())

for (k in seq_along(fp_pairs)) {
  local({
    pair <- fp_pairs[[k]]
    id <- fp_ids[[k]]
    test_that(paste0("the fit-statistic tokens of a pair: ", id), {
      skip_on_cran()
      fits <- lapply(pair, .class_fixture)
      shown <- character()
      for (tk in fp_tokens) {
        tbl <- tryCatch(
          suppressWarnings(suppressMessages(table_regression(
            fits,
            show_fit_stats = c("nobs", tk),
            output = "data.frame"
          ))),
          spicy_error = function(e) {
            .refusal_note("fit_stats_pairs", id, paste0("\"", tk, "\""), e)
            NULL
          }
        )
        if (!is.null(tbl)) {
          shown <- c(shown, tk)
        }
      }
      fp_rows[[id]] <- shown
      .refusal_ran("fit_stats_pairs", id)
      succeed()
    })
  })
}

test_that("the tokens that give a row for a pair keep their list", {
  skip_on_cran()
  skip_if(
    !all(fp_ids %in% ls(fp_rows)),
    "a pair was not swept (a package missing)"
  )
  expect_snapshot(cat(
    sprintf(
      "%s: %s",
      fp_ids,
      vapply(
        fp_ids,
        function(id) paste(fp_rows[[id]], collapse = ", "),
        character(1)
      )
    ),
    sep = "\n"
  ))
})

test_that("the refusals of the pairs keep their text and class", {
  skip_on_cran()
  .refusal_snapshot("fit_stats_pairs", fp_ids)
})
