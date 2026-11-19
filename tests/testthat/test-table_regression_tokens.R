# ?table_regression_tokens names the vocabulary of `show_columns` and
# `show_fit_stats`. These tests hold the page to the validators: every
# token they accept is on the page, every token the page writes is one
# they accept, and the generated section has one entry per class of the
# registry.

# The page, from the source tree when available (devtools::test), from
# the built package's Rd database otherwise (R CMD check).
tokens_page_rd <- function() {
  f <- testthat::test_path("..", "..", "man", "table_regression_tokens.Rd")
  if (file.exists(f)) {
    tools::parse_Rd(f, encoding = "UTF-8")
  } else {
    tools::Rd_db("spicy")[["table_regression_tokens.Rd"]]
  }
}

# The text of each \code{} of the page, one string per span, examples
# left out.
tokens_page_spans <- function(rd) {
  spans <- character()
  leaves <- function(x) {
    if (is.list(x)) {
      paste(vapply(x, leaves, character(1)), collapse = "")
    } else {
      paste(as.character(x), collapse = "")
    }
  }
  walk <- function(x) {
    tag <- attr(x, "Rd_tag")
    if (identical(tag, "\\examples")) {
      return(invisible())
    }
    if (identical(tag, "\\code")) {
      spans <<- c(spans, leaves(x))
    } else if (is.list(x)) {
      for (el in x) {
        walk(el)
      }
    }
    invisible()
  }
  walk(rd)
  spans
}

tokens_page_code <- function(rd) {
  paste(tokens_page_spans(rd), collapse = " ")
}

# Every token the validators accept: the atomic and group tokens of
# `show_columns` and its "none", the tokens of `show_fit_stats`, the
# change tokens of `nested = TRUE` among them.
accepted_tokens <- function() {
  c(
    .regression_tokens$show_columns_atomic,
    names(.show_columns_groups),
    "none",
    .regression_tokens$show_fit_stats,
    .NESTED_CHANGE_TOKENS
  )
}

test_that("every token the validators accept is named on the page", {
  code <- tokens_page_code(tokens_page_rd())
  for (tok in unique(accepted_tokens())) {
    expect_true(
      grepl(paste0("\"", tok, "\""), code, fixed = TRUE),
      label = paste0("\"", tok, "\" on ?table_regression_tokens")
    )
  }
})

test_that("every token the page writes is one the validators accept", {
  code <- tokens_page_code(tokens_page_rd())
  written <- unique(
    gsub(
      "\"",
      "",
      regmatches(code, gregexpr("\"[a-z][a-z0-9_]*\"", code))[[1L]],
      fixed = TRUE
    )
  )
  # Values of other arguments that the text cites, each in its code span.
  other_args <- c(
    # pscl::zeroinfl() and hurdle(), `dist = "negbin"`
    "negbin",
    # car::Anova(error.estimate = "dispersion")
    "dispersion",
    # `layout = "stacked"`, refused with `show_columns = "none"`
    "stacked"
  )
  unknown <- setdiff(written, c(accepted_tokens(), other_args))
  expect_identical(unknown, character(0))
  # The exceptions are still cited: drop one that is no longer needed.
  expect_true(all(other_args %in% written))
})

test_that("the generated section has one entry per class of the registry", {
  reg <- table_regression_models()
  md <- strsplit(.render_fit_stats_by_class_md(), "\n", fixed = TRUE)[[1L]]
  entries <- md[startsWith(md, "- `")]
  expect_length(entries, nrow(reg))
  expect_identical(
    sub("^- `([^`]+)`.*$", "\\1", entries),
    reg$class
  )
  # One bold line per family, in the order of the registry.
  # (The first carries the ragged-right switch of the PDF manual.)
  expect_identical(
    regmatches(md, regexpr("\\*\\*[^*]+\\*\\*$", md)),
    paste0("**", unique(reg$family), "**")
  )
  expect_true(startsWith(md[1L], "\\if{latex}{\\out{\\raggedright{}}}**"))
  # A condition stays in its parenthesis, outside the code span.
  glm <- entries[startsWith(entries, "- `glm`")]
  expect_match(
    glm,
    "`\"r2_deviance\"` (not binomial)",
    fixed = TRUE
  )
  # The page carries the section as rendered: each class is a code span
  # of it.
  spans <- tokens_page_spans(tokens_page_rd())
  for (cl in reg$class) {
    expect_true(cl %in% spans, label = cl)
  }
})
