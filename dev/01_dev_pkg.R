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

# Full check, two modes. Both run the incoming checks (remote) and build
# the PDF manual; CLIPR_ALLOW = FALSE reproduces a machine without a
# clipboard. Expected verdict: 0 errors, 0 warnings, 1 NOTE (incoming).
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

# 04 README & WEBSITE -------
devtools::build_readme()
source("dev/build_pkgdown_site.R") # Build site + clean internal pages

# 05 URLS & SPELLING -------
urlchecker::url_check()
devtools::spell_check()

# 06 SESSION INFO -------
sessioninfo::session_info()
