# ---------------------------------------------------------------------------
# The Stan fixture cache (helper-stan-cache.R), exercised WITHOUT Stan.
#
# The cache decides whether a 19-minute file takes 19 minutes or 80
# seconds, and a cache that never invalidates is worse than no cache: it
# would serve a fit sampled by a different brms, or by a model call that
# has since been edited, and the tests would pass on the wrong object.
# So the invalidation contract is pinned here, on a trivial payload.
# ---------------------------------------------------------------------------

.cache_probe_clean <- function(prefix) {
  d <- .stan_cache_dir()
  if (!is.null(d)) {
    unlink(list.files(d, pattern = paste0("^", prefix, "-"), full.names = TRUE))
  }
  invisible(NULL)
}

test_that("a fixture is built once and then read back", {
  skip_if(is.null(.stan_cache_dir()), "not a source checkout")
  withr::defer(.cache_probe_clean("probe_once"))
  .cache_probe_clean("probe_once")

  n <- 0L
  build <- function() {
    n <<- n + 1L
    list(tag = "a")
  }
  first <- .stan_cached_fit("probe_once", "testthat", build)
  second <- .stan_cached_fit("probe_once", "testthat", build)
  expect_identical(n, 1L)
  expect_identical(first, second)
})

test_that("editing the fixture's source invalidates its entry", {
  skip_if(is.null(.stan_cache_dir()), "not a source checkout")
  withr::defer(.cache_probe_clean("probe_src"))
  .cache_probe_clean("probe_src")

  n <- 0L
  # The two builders must differ in their literal SOURCE, not merely in
  # the environment they close over: the key hashes body(build).
  build_a <- function() {
    n <<- n + 1L
    list(tag = "a")
  }
  build_b <- function() {
    n <<- n + 1L
    list(tag = "b")
  }
  expect_identical(.stan_cached_fit("probe_src", "testthat", build_a)$tag, "a")
  expect_identical(.stan_cached_fit("probe_src", "testthat", build_b)$tag, "b")
  expect_identical(n, 2L)
  # Two keys, two entries -- the old fit is not evicted, so switching
  # back does not resample either.
  expect_identical(.stan_cached_fit("probe_src", "testthat", build_a)$tag, "a")
  expect_identical(n, 2L)
})

test_that("a package-version change invalidates its entry", {
  skip_if(is.null(.stan_cache_dir()), "not a source checkout")
  withr::defer(.cache_probe_clean("probe_pkg"))
  .cache_probe_clean("probe_pkg")

  n <- 0L
  build <- function() {
    n <<- n + 1L
    list(tag = "a")
  }
  # Two different installed packages stand in for two versions of one:
  # what the key reads is the version STRING, so a bump behaves the same
  # way a different package does.
  .stan_cached_fit("probe_pkg", "testthat", build)
  .stan_cached_fit("probe_pkg", "withr", build)
  expect_identical(n, 2L)
})

test_that("an unreadable entry is a miss, not a failure", {
  skip_if(is.null(.stan_cache_dir()), "not a source checkout")
  withr::defer(.cache_probe_clean("probe_bad"))
  .cache_probe_clean("probe_bad")

  n <- 0L
  build <- function() {
    n <<- n + 1L
    list(tag = "a")
  }
  .stan_cached_fit("probe_bad", "testthat", build)
  entry <- list.files(
    .stan_cache_dir(),
    pattern = "^probe_bad-",
    full.names = TRUE
  )
  expect_length(entry, 1L)
  writeLines("not an rds at all", entry)
  out <- .stan_cached_fit("probe_bad", "testthat", build)
  expect_identical(n, 2L)
  expect_identical(out$tag, "a")
  # And the entry was rewritten, so the damage does not persist.
  expect_identical(
    readRDS(list.files(
      .stan_cache_dir(),
      pattern = "^probe_bad-",
      full.names = TRUE
    )),
    out
  )
})

test_that("outside a source checkout the cache disables itself", {
  # This is the R CMD check / CRAN shape: the tests run from
  # <pkg>.Rcheck/tests/testthat, where there is no dev/ directory. The
  # fixtures must still work, and nothing may be written.
  tmp <- withr::local_tempdir()
  fake <- file.path(tmp, "tests", "testthat")
  dir.create(fake, recursive = TRUE)
  withr::local_dir(fake)

  expect_null(.stan_cache_dir())
  n <- 0L
  build <- function() {
    n <<- n + 1L
    list(tag = "a")
  }
  expect_identical(.stan_cached_fit("probe_off", "testthat", build)$tag, "a")
  expect_identical(.stan_cached_fit("probe_off", "testthat", build)$tag, "a")
  expect_identical(n, 2L)
  expect_length(list.files(tmp, recursive = TRUE), 0L)
})
