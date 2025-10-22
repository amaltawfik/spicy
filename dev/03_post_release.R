# ==================================================
# 03_post_release.R
# Post-CRAN tasks for the spicy package
# ==================================================

library(usethis)
library(pkgdown)
library(sessioninfo)

# 01 GITHUB TAG & RELEASE -------
# Run only after CRAN acceptance
usethis::use_github_tag() # Create Git tag (e.g., v0.2.2)
usethis::use_github_release() # Publish GitHub release using NEWS.md

# 02 UPDATE PKGDOWN WEBSITE -------
pkgdown::build_site() # Rebuild pkgdown site
system("git add docs/")
system("git commit -am 'Update pkgdown site for CRAN release'")
system("git push")

# 03 BUMP TO DEVELOPMENT VERSION -------
usethis::use_dev_version() # Move to next dev version (e.g., 0.2.2 → 0.2.2.9000)
system("git commit -am 'Start development version'")
system("git push")

# 04 SESSION INFO -------
sessioninfo::session_info() # Print R session info
# Optionally, save to file:
# writeLines(capture.output(sessioninfo::session_info()), "dev/session-postrelease.log")
