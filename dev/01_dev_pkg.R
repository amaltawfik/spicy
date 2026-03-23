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

# 04 README & WEBSITE -------
devtools::build_readme()
source("dev/build_pkgdown_site.R") # Build site + clean internal pages

# 05 URLS & SPELLING -------
urlchecker::url_check()
devtools::spell_check()

# 06 SESSION INFO -------
sessioninfo::session_info()
