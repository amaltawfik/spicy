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
rhub::rc_submit(
  platforms = c("windows", "linux"),
  confirm = FALSE
)

rhub::rhub_check(
  platforms = c("windows", "macos", "ubuntu-release")
)

# 06 CRAN SUBMISSION -------
usethis::use_version("minor") # Increment version ("patch" for bugfix)
devtools::release() # Interactive CRAN release process

# After CRAN acceptance, run dev/03_post_release.R
