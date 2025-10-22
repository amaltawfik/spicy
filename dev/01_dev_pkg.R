# ==================================================
# 01_dev_pkg.R
# Development & website workflow for the spicy package
# ==================================================

library(devtools)
library(styler)
library(lintr)
library(pkgdown)
library(urlchecker)
library(sessioninfo)

# 01 STYLE & LINT -------
styler::style_pkg() # Reformat all R code to tidy style
lintr::lint_package() # Static code analysis (linting)

# 02 DOCUMENTATION & TESTS -------
devtools::document() # Regenerate documentation and NAMESPACE
devtools::test() # Run all unit tests
# To test specific functions: devtools::test(filter = "function_name")

# 03 LOCAL CHECKS -------
devtools::check() # Standard local package checks

# 04 README & WEBSITE -------
devtools::build_readme() # Rebuild README from README.Rmd
pkgdown::build_site() # Rebuild pkgdown website locally

# 05 URLS & SPELLING -------
urlchecker::url_check() # Verify all URLs in documentation
devtools::spell_check() # Spell-check documentation and comments

# 06 SESSION INFO -------
sessioninfo::session_info() # Display R session information
# Optionally, save to file:
# writeLines(capture.output(sessioninfo::session_info()), "dev/session-dev.log")
