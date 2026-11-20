# performance registers logLik() methods for ivreg and iv_robust, which
# their own packages do not define. Before the guard in
# has_usable_loglik(), a change token on these fits was refused with one
# message or another depending on whether performance had been loaded
# in the session.

test_that("2SLS fits never ride the likelihood path", {
  skip_if_not_installed("AER")
  skip_if_not_installed("performance")
  loadNamespace("performance")
  iv <- AER::ivreg(mpg ~ wt | qsec, data = mtcars)
  expect_false(has_usable_loglik(iv))
  expect_false(all_likelihood_path(list(iv)))
  err <- expect_error(
    table_regression(iv, show_fit_stats = "r2_change"),
    class = "spicy_invalid_input"
  )
  expect_match(conditionMessage(err), "needs `nested = TRUE`", fixed = TRUE)
  skip_if_not_installed("estimatr")
  ir <- estimatr::iv_robust(mpg ~ wt | qsec, data = mtcars)
  expect_false(has_usable_loglik(ir))
})

test_that("the refusal is the same in a fresh session with and without performance", {
  skip_on_cran()
  skip_if_not_installed("AER")
  skip_if_not_installed("performance")
  src <- normalizePath(test_path("..", ".."), winslash = "/", mustWork = FALSE)
  load_line <- if (file.exists(file.path(src, "DESCRIPTION"))) {
    sprintf(
      "suppressMessages(pkgload::load_all(\"%s\", quiet = TRUE, helpers = FALSE))",
      src
    )
  } else {
    "suppressMessages(library(spicy))"
  }
  run <- function(first) {
    script <- withr::local_tempfile(fileext = ".R")
    writeLines(
      c(
        if (first) "loadNamespace(\"performance\")",
        load_line,
        "iv <- AER::ivreg(mpg ~ wt | qsec, data = mtcars)",
        "e <- tryCatch(table_regression(iv, show_fit_stats = \"r2_change\"),",
        "  error = function(e) e)",
        "cat(\"RESULT:\", class(e)[1L], strsplit(conditionMessage(e), \"\\n\")[[1L]][1L], \"\\n\")"
      ),
      script
    )
    out <- suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"),
      shQuote(script),
      stdout = TRUE,
      stderr = TRUE
    ))
    grep("^RESULT:", out, value = TRUE)
  }
  with_perf <- run(TRUE)
  without <- run(FALSE)
  expect_length(with_perf, 1L)
  expect_identical(with_perf, without)
  expect_match(without, "needs `nested = TRUE`", fixed = TRUE)
})
