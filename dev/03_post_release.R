# ==================================================
# 03_post_release.R
# Post-CRAN tasks for the spicy package
# ==================================================

library(usethis)
library(devtools)
library(pkgdown)
library(sessioninfo)

run_cmd <- function(cmd, args = character()) {
  status <- system2(cmd, args = args)
  if (!identical(status, 0L)) {
    stop(sprintf("Command failed: %s %s", cmd, paste(args, collapse = " ")))
  }
  invisible(status)
}

# 01 GITHUB TAG & RELEASE -------
# Run only after CRAN acceptance
usethis::use_github_tag()      # Create Git tag (e.g., v0.4.2)
usethis::use_github_release()  # Publish GitHub release from NEWS.md

# 02 UPDATE README + DOCS + PKGDOWN -------
devtools::build_readme()
devtools::document()
pkgdown::build_site()

run_cmd("git", c("add", "README.md", "man", "NAMESPACE", "docs"))
run_cmd("git", c("commit", "-m", "docs: refresh README, Rd and pkgdown site for release"))
run_cmd("git", c("push"))

# 03 BUMP TO DEVELOPMENT VERSION -------
usethis::use_dev_version() # Move to next dev version (e.g., 0.4.2.9000)
run_cmd("git", c("add", "DESCRIPTION", "NEWS.md"))
run_cmd("git", c("commit", "-m", "chore: start next development version"))
run_cmd("git", c("push"))

# 04 SESSION INFO -------
sessioninfo::session_info()
# Optionally save:
# writeLines(capture.output(sessioninfo::session_info()), "dev/session-postrelease.log")
