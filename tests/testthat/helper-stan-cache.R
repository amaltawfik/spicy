# ---------------------------------------------------------------------------
# Disk cache for the Stan fixtures.
#
# Every `.fit_brms_*` / `.fit_rstanarm_*` call compiles and samples a
# model, and test-regression_frame_stan.R alone calls them 22 times: the
# file takes about 19 minutes. Those fixtures are local-only
# (`skip_on_ci`) exactly because that cost is not CI's to pay -- but a
# local run is also the ONLY place the brms half of the frame schema is
# ever exercised (AGENTS.md), so the cost lands on the one run that must
# not be skipped.
#
# A fit is therefore written to disk once and read back afterwards,
# keyed on everything that could make it stale: the R version, the
# fitting package's version, the fixture's name, and a hash of the
# fixture's own source. Change one character of the model call, the
# seed included, and the key changes with it. Nothing here is ever
# invalidated by hand.
#
# The cache lives under `dev/`, which exists only in a source checkout.
# `R CMD check` runs the tests from `<pkg>.Rcheck/tests/testthat`, where
# that directory is absent, so caching disables itself and the fixtures
# behave exactly as they did before -- no writing outside the session's
# own tree, nothing for a CRAN check to see. `dev/.fixture_cache/` is
# gitignored.
# ---------------------------------------------------------------------------

# The cache directory, or NULL when we are not in a source checkout (or
# it cannot be created -- a read-only tree is a reason to skip the
# cache, never a reason to fail a test).
.stan_cache_dir <- function() {
  root <- file.path("..", "..", "dev")
  if (!dir.exists(root)) {
    return(NULL)
  }
  d <- file.path(root, ".fixture_cache")
  if (!dir.exists(d)) {
    tryCatch(
      dir.create(d, recursive = TRUE, showWarnings = FALSE),
      error = function(e) NULL
    )
  }
  if (dir.exists(d)) d else NULL
}

# Run `build()` once per (R version, package version, fixture source)
# and reuse the result on later calls. `pkg` is the package whose
# version invalidates the entry -- "brms" or "rstanarm".
#
# "Fixture source" means the BODY of `build`, not the environment it
# closes over: a builder that read its formula or its seed from an outer
# variable would keep its key when that variable changed. Every fixture
# here spells its model out literally, which is what makes the body a
# complete description of the fit -- keep it that way.
.stan_cached_fit <- function(name, pkg, build) {
  dir <- .stan_cache_dir()
  if (is.null(dir)) {
    return(build())
  }
  key <- rlang::hash(list(
    name = name,
    r = as.character(getRversion()),
    pkg = as.character(utils::packageVersion(pkg)),
    src = paste(deparse(body(build)), collapse = "\n")
  ))
  path <- file.path(dir, paste0(name, "-", substr(key, 1L, 16L), ".rds"))
  if (file.exists(path)) {
    fit <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.null(fit)) {
      return(fit)
    }
    # A truncated or unreadable entry is a cache miss, not a failure.
    unlink(path)
  }
  fit <- build()
  tryCatch(saveRDS(fit, path), error = function(e) NULL)
  fit
}
