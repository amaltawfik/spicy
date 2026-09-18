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

# Full CRAN-like check: incoming checks (remote) + PDF manual, in CRAN's
# environment (no clipboard, skip_on_cran() honoured). ~75 min: the whole
# suite runs inside. Expected before a submission: 0 errors, 0 warnings,
# 1 NOTE (the incoming one).
withr::with_envvar(
  c(CLIPR_ALLOW = "FALSE", NOT_CRAN = NA),
  devtools::check(remote = TRUE, manual = TRUE)
)

# 04 README & WEBSITE -------
devtools::build_readme()
source("dev/build_pkgdown_site.R") # Build site + clean internal pages

# 05 URLS & SPELLING -------
urlchecker::url_check()
devtools::spell_check()

# 06 SESSION INFO -------
sessioninfo::session_info()
