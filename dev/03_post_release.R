# ==================================================
# 03_post_release.R
# Post-CRAN acceptance tasks for the spicy package
# ==================================================

library(usethis)
library(devtools)
library(sessioninfo)

run_cmd <- function(cmd, args = character()) {
  status <- system2(cmd, args = args)
  if (!identical(status, 0L)) {
    stop(sprintf("Command failed: %s %s", cmd, paste(args, collapse = " ")))
  }
  invisible(status)
}

# 01 CLEAN UP CRAN-SUBMISSION -------
if (file.exists("CRAN-SUBMISSION")) {
  file.remove("CRAN-SUBMISSION")
}

# 02 GITHUB TAG & RELEASE -------
usethis::use_github_release() # Create tag + publish GitHub release from NEWS.md

# 03 UPDATE README & DOCS -------
devtools::build_readme()
devtools::document()
source("dev/build_pkgdown_site.R") # Build site + clean internal pages

run_cmd("git", c("add", "README.md", "man", "NAMESPACE", "docs"))
run_cmd(
  "git",
  c("commit", "-m", "docs: refresh README, Rd and pkgdown site for release")
)
run_cmd("git", c("push"))

# 04 BUMP TO DEVELOPMENT VERSION -------
usethis::use_dev_version()
run_cmd("git", c("add", "DESCRIPTION", "NEWS.md"))
run_cmd("git", c("commit", "-m", "chore: start next development version"))
run_cmd("git", c("push"))

# 05 SESSION INFO -------
sessioninfo::session_info()
