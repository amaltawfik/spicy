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

# The package sets whose versions invalidate a fixture: the fitting
# package, the sampler toolchain beneath it, and the package supplying
# the fixture's data. Named here so all four fixtures share one list and
# none can drift out of it.
.STAN_CACHE_PKGS_BRMS <- c("brms", "rstan", "StanHeaders", "lme4")
.STAN_CACHE_PKGS_RSTANARM <- c("rstanarm", "rstan", "StanHeaders", "lme4")

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

# Run `build()` once per (R version, package versions, fixture source)
# and reuse the result on later calls.
#
# `pkgs` is every package whose version can change the fit: the fitting
# package ("brms" / "rstanarm"), the sampler toolchain underneath it
# ("rstan", "StanHeaders" -- a toolchain bump moves the draws), and any
# package supplying the fixture's DATA ("lme4" for `sleepstudy`), whose
# dataset could be revised under a stable model call. A package that is
# not installed contributes NA rather than an error, so an optional
# member of the list never breaks the key.
#
# "Fixture source" means the formals and the BODY of `build`, not the
# environment it closes over: a builder that read its formula or its
# seed from an outer variable would keep its key when that variable
# changed. Every fixture here spells its model out literally, which is
# what makes the body a complete description of the fit -- keep it that
# way.
.stan_cached_fit <- function(name, pkgs, build) {
  dir <- .stan_cache_dir()
  if (is.null(dir)) {
    return(build())
  }
  versions <- vapply(
    pkgs,
    function(p) {
      tryCatch(
        as.character(utils::packageVersion(p)),
        error = function(e) NA_character_
      )
    },
    character(1)
  )
  key <- rlang::hash(list(
    name = name,
    r = as.character(getRversion()),
    pkgs = versions,
    formals = paste(deparse(formals(build)), collapse = "\n"),
    src = paste(deparse(body(build)), collapse = "\n")
  ))
  path <- file.path(dir, paste0(name, "-", substr(key, 1L, 16L), ".rds"))
  if (file.exists(path)) {
    # suppressWarnings: a failing read raises base's own "cannot open
    # file" warning ALONGSIDE the error. The classed note below carries
    # the information; the raw warning would only move the suite's
    # warning baseline for a condition that is not a defect.
    fit <- suppressWarnings(tryCatch(readRDS(path), error = function(e) NULL))
    if (!is.null(fit)) {
      return(fit)
    }
    # A truncated or unreadable entry is a cache miss, not a failure --
    # but it is not nothing either, and swallowing it unclassed makes a
    # cache that never reads look exactly like a cache that always hits.
    # Say so through a classed condition the caller can catch or mute.
    .stan_cache_note(
      "spicy_fixture_cache_unreadable",
      sprintf("Fixture cache entry unreadable, resampling: %s", basename(path))
    )
    unlink(path)
  }
  fit <- build()
  written <- suppressWarnings(tryCatch(
    {
      saveRDS(fit, path)
      TRUE
    },
    error = function(e) FALSE
  ))
  if (!written) {
    # Same reasoning at the other end: a cache that can never write costs
    # the full sampling time on every run, silently, forever.
    .stan_cache_note(
      "spicy_fixture_cache_unwritable",
      sprintf("Fixture cache entry could not be written: %s", basename(path))
    )
  }
  fit
}

# A classed, mutable-away message. Not a warning: a cache miss is not a
# defect in the package under test, and it must not move the suite's
# warning baseline.
.stan_cache_note <- function(class, text) {
  message(structure(
    class = c(class, "spicy_fixture_cache_note", "message", "condition"),
    list(message = paste0(text, "\n"), call = NULL)
  ))
}
