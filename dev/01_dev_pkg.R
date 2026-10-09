# ==================================================
# 01_dev_pkg.R
# Development & website workflow for the spicy package
# ==================================================

library(devtools)
library(urlchecker)
library(sessioninfo)

# 00 LOAD PACKAGE -------
devtools::load_all()

# 01 FORMAT CODE -------
# Run in terminal: air format .

# 02 DOCUMENTATION & TESTS -------
devtools::document()
devtools::test()
# To test specific functions: devtools::test(filter = "function_name")

# 03 LOCAL CHECKS -------
devtools::check()

# Full check, three modes. (a) and (b) run the incoming checks (remote)
# and build the PDF manual; CLIPR_ALLOW = FALSE reproduces a machine
# without a clipboard. Expected verdict: 0 errors, 0 warnings, and at
# most the incoming NOTE.
#
# (a) Strict. devtools::check() sets NOT_CRAN = "true" by default, so
#     EVERY test runs, skip_on_cran() ones included (about 70 min).
withr::with_envvar(
  c(CLIPR_ALLOW = "FALSE"),
  devtools::check(remote = TRUE, manual = TRUE)
)
#
# (b) CRAN-faithful. skip_on_cran() tests are skipped, as on the CRAN
#     machines. NOT_CRAN must go through `env_vars`: check() overrides
#     any value set outside it (an outer NOT_CRAN = NA has no effect).
withr::with_envvar(
  c(CLIPR_ALLOW = "FALSE"),
  devtools::check(
    remote = TRUE, manual = TRUE,
    env_vars = c(NOT_CRAN = "false")
  )
)
#
# (c) Without the suggested packages, what CRAN's noSuggests flavor
#     sees. R hides every package outside Depends / Imports and their
#     own dependencies (testthat and the vignette builder stay). It
#     catches a suggested package used without a guard in an example or
#     a test, which (a) and (b) cannot see on a machine that has them
#     all. Expected verdict: 0 errors.
withr::with_envvar(
  c(CLIPR_ALLOW = "FALSE"),
  devtools::check(
    remote = FALSE, manual = FALSE,
    env_vars = c(NOT_CRAN = "false", "_R_CHECK_DEPENDS_ONLY_" = "true")
  )
)

# 04 README & WEBSITE -------
source("dev/make_codebook_showcase.R") # the codebook of the site (PDF, Excel, page excerpts)
devtools::build_readme()
source("dev/build_pkgdown_site.R") # Build site + clean internal pages

# 05 URLS & SPELLING -------
urlchecker::url_check()
devtools::spell_check()

# 06 SESSION INFO -------
sessioninfo::session_info()
