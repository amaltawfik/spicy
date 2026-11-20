# The refusals and warnings of table_regression() for the arguments whose
# outcome depends on the class of the model, frozen on the fixtures of
# helper-class-fixtures.R before the validation reads the capability
# registry (dev/decisions/2026-11-19-capability-registry.md). Each call
# succeeds, warns or is refused, and every condition of spicy is noted
# with its class and its text (helper-refusal-log.R). A condition that is
# not spicy's is a defect: it is named in `rm_unclassed`, which may not
# grow. The fit statistics and `vcov` are frozen by their own sweeps
# (test-fit_stats_requested_rows.R, test-registry_vcov.R).

.refusal_reset("args")

rm_tokens <- c(
  "df",
  "beta",
  "n_events",
  "pd",
  "rhat",
  "ess_bulk",
  "ess_tail",
  "mcse",
  "ame",
  "ame_se",
  "ame_ci",
  "ame_p",
  "rmst",
  "rmst_se",
  "rmst_ci",
  "rmst_p",
  "risk_diff",
  "risk_diff_se",
  "risk_diff_ci",
  "risk_diff_p",
  .PARTIAL_ES_TOKENS,
  "r2",
  "adj_r2"
)

# Every call of the sweep, by key "argument = value".
rm_cases <- function() {
  cs <- list()
  for (tk in rm_tokens) {
    a <- list(show_columns = c("b", tk))
    if (startsWith(tk, "rmst")) {
      a$tau <- 1
    }
    if (startsWith(tk, "risk_diff")) {
      a$at_time <- 1
    }
    cs[[paste0("show_columns = \"", tk, "\"")]] <- a
  }
  cs[["exponentiate = TRUE"]] <- list(exponentiate = TRUE)
  # `standardized = "none"` with beta is the call `show_columns = "beta"`.
  for (s in c("refit", "posthoc", "basic", "smart", "pseudo")) {
    cs[[paste0("standardized = \"", s, "\" (with beta)")]] <- list(
      show_columns = c("b", "beta"),
      standardized = s
    )
  }
  cs[["p_adjust = \"holm\""]] <- list(p_adjust = "holm")
  cs[["re_test = \"lrt\""]] <- list(re_test = "lrt")
  cs[["re_test = \"rlrt\""]] <- list(re_test = "rlrt")
  cs[["re_ci = \"profile\""]] <- list(re_ci = "profile")
  cs[["nested = TRUE"]] <- "nested"
  cs
}

# The sampling. A value whose outcome is the same for every fixture of a
# class is swept on the first fixture of the class only; the values below
# change with the fixture (measured on the full sweep, 2026-11-19) and are
# swept on every fixture of their class (on the representatives of
# `rm_variant_reps` for two classes). `nested = TRUE` and the two Stan
# fixtures are always swept.
rm_variant_dependent <- c(
  fixest = "^exponentiate|^show_columns = \"(ame|df\")",
  # The messages name the class, "gam" or "bam".
  gam = ".",
  glm = "^exponentiate|^show_columns = \"(n_events|partial_|df\")|^standardized = \"pseudo",
  glmmTMB = "^exponentiate|^show_columns = \"ame|^standardized = \"(refit|posthoc|basic|smart|pseudo)",
  survreg = "^show_columns = \"(rmst|risk_diff)",
  svyglm = "^exponentiate"
)

# For two classes, the values above split the fixtures into two outcomes
# with the same text: they are swept on one fixture of each. fixest: a
# linear fit warns that `exponentiate` has no effect and has an AME, a
# Poisson fit with absorbed fixed effects has no AME. glmmTMB: the ordinal
# family has no AME and refuses every `standardized`, the others fall back
# to "refit" with a warning.
rm_variant_reps <- list(
  fixest = c("feols", "fepois"),
  glmmTMB = c("glmmTMB", "glmmTMB_ordinal")
)

# The survival estimands are computed on these fixtures, by g-computation
# that takes 30 to 200 seconds a call; test-survival_estimands.R holds them.
rm_estimands_computed <- c("coxph", "coxph_strata", "survreg")

rm_swept <- function(id, key) {
  fx <- .CLASS_FIXTURES[[id]]
  if (isTRUE(fx$stan) || identical(key, "nested = TRUE")) {
    return(TRUE)
  }
  if (id %in% rm_estimands_computed && grepl("\"(rmst|risk_diff)", key)) {
    return(FALSE)
  }
  pat <- rm_variant_dependent[fx$class]
  if (!is.na(pat) && grepl(pat, key)) {
    reps <- rm_variant_reps[[fx$class]]
    return(is.null(reps) || id %in% reps)
  }
  same <- vapply(.CLASS_FIXTURES, `[[`, character(1), "class") == fx$class
  identical(names(.CLASS_FIXTURES)[same][1L], id)
}

# The conditions that are not spicy's, each named (fixture, call key):
# defects to fix, and a list that may not grow.
rm_unclassed <- list()

# The smaller model of a nested pair (helper-refusal-log.R).
rm_reduced <- .refusal_reduced

# One call: "ok", "error" or "not built", with the conditions it signals.
rm_call <- function(fit, id, a) {
  warns <- list()
  res <- tryCatch(
    withCallingHandlers(
      {
        if (identical(a, "nested")) {
          red <- rm_reduced(fit, id)
          if (is.null(red)) {
            return(list(kind = "not built", warns = list()))
          }
          table_regression(list(red, fit), nested = TRUE, output = "data.frame")
        } else {
          do.call(table_regression, c(list(fit, output = "data.frame"), a))
        }
        list(kind = "ok")
      },
      warning = function(w) {
        warns[[length(warns) + 1L]] <<- w
        invokeRestart("muffleWarning")
      },
      message = function(m) invokeRestart("muffleMessage")
    ),
    error = function(e) list(kind = "error", cnd = e)
  )
  res$warns <- warns
  res
}

rm_key <- function(cnd) paste(class(cnd)[1L], conditionMessage(cnd))

rm_sweep <- function(fit, id) {
  # The warnings of the default table belong to the fit, not to a value.
  base <- vapply(rm_call(fit, id, list())$warns, rm_key, character(1))
  cases <- rm_cases()
  unclassed <- character()
  accepted <- character()
  for (key in names(cases)) {
    if (!rm_swept(id, key)) {
      next
    }
    # The SE and CI of an AME or a survival estimand share the outcome of
    # the estimate (measured on the full sweep): swept where the estimate
    # is refused, and skipped where it is computed. The p-value, which a
    # Bayesian fit refuses alone, is always swept.
    lead <- sub(
      "^(show_columns = \"(ame|rmst|risk_diff))_(se|ci)\"$",
      "\\1\"",
      key
    )
    if (!identical(lead, key) && lead %in% accepted) {
      next
    }
    r <- rm_call(fit, id, cases[[key]])
    if (identical(r$kind, "ok")) {
      accepted <- c(accepted, key)
    }
    conds <- r$warns[!vapply(r$warns, rm_key, character(1)) %in% base]
    if (identical(r$kind, "error")) {
      conds <- c(conds, list(r$cnd))
    }
    for (cnd in conds) {
      if (inherits(cnd, "spicy_bayes_diagnostics")) {
        # The sampler diagnostics quote the draws of the cached fit.
        next
      }
      if (inherits(cnd, c("spicy_error", "spicy_warning"))) {
        .refusal_note(
          "args",
          id,
          key,
          cnd,
          kind = if (inherits(cnd, "error")) "error" else "warning"
        )
      } else {
        unclassed <- c(unclassed, key)
      }
    }
  }
  allowed <- vapply(
    Filter(function(u) identical(u[1L], id), rm_unclassed),
    `[`,
    character(1),
    2L
  )
  expect_identical(sort(unique(unclassed)), sort(allowed), label = id)
}

for (id in names(.CLASS_FIXTURES)) {
  local({
    id <- id
    test_that(
      paste0("every condition of the argument sweep is classed: ", id),
      {
        skip_on_cran()
        rm_sweep(.class_fixture(id), id)
        .refusal_ran("args", id)
      }
    )
  })
}

test_that("the refusals and warnings of the argument sweep keep their text and class", {
  skip_on_cran()
  .refusal_snapshot("args", .refusal_ids())
})

test_that("the refusals and warnings of the argument sweep keep their text and class: Stan", {
  .skip_unless_local_stan()
  .refusal_snapshot("args", .refusal_ids(stan = TRUE))
})

# A remedy names a token its receiver accepts (helper-refusal-log.R).
test_that("every remedy of the argument sweep is accepted by its receiver", {
  skip_on_cran()
  .refusal_check_remedies("args", .refusal_ids())
})

test_that("every remedy of the argument sweep is accepted by its receiver: Stan", {
  .skip_unless_local_stan()
  .refusal_check_remedies("args", .refusal_ids(stan = TRUE))
})
