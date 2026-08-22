# The non-ASCII sentinel tools/run_suite.R runs before the suite.
#
# It exists because three U+00A0 NO-BREAK SPACEs reached R/ during the
# outcome campaign and no eye caught them: in an editor, in a diff and
# in a review they are ordinary spaces. The witness below therefore
# writes the offending character with `intToUtf8()` rather than as a
# literal, so this file cannot be read as "a space is a space" either.
#
# tools/ is shipped in the tarball but not in the installed package, so
# these skip rather than fail when the source tree is not at hand.

.sentinel_path <- function() {
  testthat::test_path("..", "..", "tools", "ascii_sentinel.R")
}

.sentinel_load <- function() {
  p <- .sentinel_path()
  skip_if_not(file.exists(p), "tools/ascii_sentinel.R not in this tree")
  env <- new.env(parent = globalenv())
  sys.source(p, envir = env)
  env
}

# One temp directory holding one R file with the given lines. The
# directory is cleaned up with the CALLING test, not with this helper --
# `local_tempdir()` defaults to its own frame, and the tree would be
# gone before the sentinel ever looked at it.
.sentinel_tree <- function(lines, envir = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = envir)
  writeLines(lines, file.path(d, "probe.R"), useBytes = TRUE)
  d
}

test_that("a no-break space in R/ is named with file, line and column", {
  env <- .sentinel_load()
  nbsp <- intToUtf8(0x00A0)
  d <- .sentinel_tree(c(
    "ok_line <- 1",
    paste0("x <-", nbsp, "2"),
    "ok_again <- 3"
  ))
  sites <- env$ascii_sentinel_sites(d)

  expect_identical(nrow(sites), 1L)
  expect_identical(sites$line, 2L)
  # "x <-" is four characters, so the fifth is the one nobody can see.
  expect_identical(sites$col, 5L)
  expect_identical(sites$codepoint, 0x00A0L)

  msg <- env$ascii_sentinel_report(sites)
  expect_length(msg, 1L)
  expect_match(msg, "probe.R:2:5:", fixed = TRUE)
  expect_match(msg, "U+00A0", fixed = TRUE)
  # The message says what the eye cannot: that it is invisible.
  expect_match(msg, "NO-BREAK SPACE (invisible", fixed = TRUE)
})

test_that("the allowlist is honoured, and it holds nothing invisible", {
  env <- .sentinel_load()
  allowed <- env$ASCII_SENTINEL_ALLOWED
  chars <- intToUtf8(strtoi(names(allowed), 16L), multiple = TRUE)

  # Every allowed character passes...
  d <- .sentinel_tree(paste0("# ", chars))
  expect_identical(nrow(env$ascii_sentinel_sites(d)), 0L)

  # ...and none of them is a whitespace or format character. This is
  # the property that makes the sentinel worth having: the allowlist
  # can never grow a member the reviewer cannot see.
  expect_false(any(grepl("[[:space:]]", chars)))
  expect_identical(
    nchar(trimws(paste(chars, collapse = ""))),
    length(chars)
  )
})

test_that("every disallowed character is reported, not just the first", {
  env <- .sentinel_load()
  d <- .sentinel_tree(c(
    paste0("a <- '", intToUtf8(0x200B), "'"),
    paste0("b <- '", intToUtf8(0x2013), intToUtf8(0x2019), "'")
  ))
  sites <- env$ascii_sentinel_sites(d)
  expect_identical(nrow(sites), 3L)
  expect_identical(sites$line, c(1L, 2L, 2L))
  expect_identical(sites$codepoint, c(0x200BL, 0x2013L, 0x2019L))
  expect_true(all(grepl("invisible|reads as", sites$what)))
  # And the report carries all three: a runner that names one site
  # sends the author back for a second 40-minute round.
  expect_length(env$ascii_sentinel_report(sites), 3L)
})

test_that("a character with no entry in the known list still fails", {
  env <- .sentinel_load()
  # U+00FF is neither allowed nor a usual suspect: the sentinel must
  # not silently pass what it cannot name.
  d <- .sentinel_tree(paste0("# ", intToUtf8(0x00FF)))
  sites <- env$ascii_sentinel_sites(d)
  expect_identical(nrow(sites), 1L)
  expect_identical(sites$what, "not on the allowlist")
})

test_that("R/ is clean under the sentinel", {
  env <- .sentinel_load()
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R/ not in this tree")
  sites <- env$ascii_sentinel_sites(r_dir)
  expect_identical(
    nrow(sites),
    0L,
    info = paste(env$ascii_sentinel_report(sites), collapse = "\n")
  )
})
