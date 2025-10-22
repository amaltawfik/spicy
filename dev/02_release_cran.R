# ==================================================
# 02_release_cran.R
# Full CRAN release workflow for the spicy package
# ==================================================

library(devtools)
library(urlchecker)
library(tools)
library(rhub)
library(usethis)
library(sessioninfo)

# 01 SYNC REPOSITORY -------
# Ensure your local branch is up to date:
#   git pull origin main

# 02 PREPARE & TEST LOCALLY -------
devtools::document() # Regenerate documentation
devtools::test() # Run all tests
devtools::check(remote = TRUE, manual = TRUE) # Comprehensive local check

# 03 QUALITY ASSURANCE -------
urlchecker::url_check() # Validate URLs
devtools::spell_check() # Spell-check documentation
tools::package_dependencies("spicy", reverse = TRUE) # Check if other CRAN packages depend on spicy

# 04 WINDOWS CHECKS (WINBUILDER) -------
devtools::check_win_release() # Current stable R
devtools::check_win_devel() # Development version of R
devtools::check_win_oldrelease() # Optional, backward compatibility

# 05 CROSS-PLATFORM TESTS (R-HUB) -------
rhub::check(
  platforms = c("windows", "macos", "ubuntu-release"),
  show_status = FALSE
)

# 06 CRAN SUBMISSION -------
usethis::use_version("minor") # Increment version ("patch" for bugfix)
devtools::release() # Interactive CRAN release process

# 07 POST-ACCEPTANCE PREP -------
usethis::use_github_release() # Prepare GitHub release
usethis::use_dev_version(push = TRUE) # Bump to next dev version

# 08 SESSION INFO -------
sessioninfo::session_info() # Display environment details
# Optionally, save to file:
# writeLines(capture.output(sessioninfo::session_info()), "dev/session-release.log")
