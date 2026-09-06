# ==================================================
# 02_release_cran.R
# Full CRAN release workflow for the spicy package
#
# 0.13.0 edition: the day-J sequence below encodes decisions 49-52
# (dev/decisions_amal_2026-08.md). Target date: 2026-09-30 -- the
# rolling 6-month update count drops to 5 that day (fallback window:
# after October 11, count 4).
# ==================================================

library(devtools)
library(urlchecker)
library(tools)
library(usethis)
library(sessioninfo)

# 00 PREFLIGHT GUARDS -------
# - Local branch in sync:  git pull origin main
# - CI matrix green on the last push (macOS + Ubuntu included):
#     gh run list --workflow=R-CMD-check.yaml --limit 1
# - Stan pair guard (register n.297): the machine runs the MATCHED
#   r-universe pair StanHeaders/rstan 2.39.0.9000. Do NOT let
#   update.packages() reinstall CRAN StanHeaders alone before CRAN
#   rstan reaches >= 2.39 (it would re-break every brms fixture).
#   Stan tests are local-only, so this gates nothing on CRAN itself.

# 01 DAY-J EDITS (one dedicated commit, BEFORE any check) -------
# a. Version: 0.12.0.9000 -> 0.13.0 (kills the "large components" NOTE)
usethis::use_version("minor")
# b. DESCRIPTION Title  <- decision 50 (frozen wording):
#      Publication-Ready Tables for Descriptive Statistics and
#      Regression Models
# c. DESCRIPTION Description <- decision 51 (frozen 125-word text in
#    dev/decisions_amal_2026-08.md; wrap at 80 columns).
# d. Sync R/spicy-package.R with the new Title/Description vocabulary
#    (its roxygen title + @description still mirror the OLD text --
#    addendum to decision 51), then:
devtools::document()
# e. cran-comments.md: rewrite for 0.13.0 with the cadence sentence
#    ("this release consolidates a full development cycle; the
#    package has moved to a slower release cadence").
# f. Spell check MUST run after the Description edit -- new words may
#    need inst/WORDLIST additions:
devtools::spell_check()

# 02 LOCAL BARRIER (on the edited tree) -------
devtools::test() # expect FAIL 0 (run tools/run_suite.R for the CSV)
# The check emulates the CRAN clipboard environment; expected verdict
# 0 errors / 0 warnings / 1 NOTE (the "incoming" NOTE only: new
# submission count + any possibly-misspelled proper nouns, all
# already accepted for 0.12.0).
withr::with_envvar(
  c(CLIPR_ALLOW = "FALSE", NOT_CRAN = NA),
  devtools::check(remote = TRUE, manual = TRUE)
)

# 03 QUALITY ASSURANCE -------
urlchecker::url_check() # Validate URLs
tools::package_dependencies(
  "spicy",
  reverse = TRUE
) # Check if other CRAN packages depend on spicy

# 04 WINDOWS CHECKS (WINBUILDER) -------
devtools::check_win_release() # Current stable R
devtools::check_win_devel() # Development version of R
# devtools::check_win_oldrelease() # Optional, backward compatibility

# 04bis R-HUB, TARGETED (day-J only; workflow_dispatch on rhub.yaml) -------
# Only the two platforms that add signal beyond the CI matrix
# (3 OS x release/devel/oldrel already run on every push):
#   - nosuggests: CRAN's additional check; catches any unguarded use
#     of the ~50 Suggests behind requireNamespace().
#   - atlas: alternative BLAS; the numeric-tolerance class of failure
#     (cf. the 2026-09 Jacobian episode). Both are linux containers,
#     the stable side of R-hub's infra.
# rhub::rhub_check(platforms = c("nosuggests", "atlas"))
# (The README no longer carries the R-hub badge -- a manual-trigger
# workflow is a pre-release tool, not a live health signal.)

# 05 COMMIT + PUSH THE RELEASE STATE -------
# The day-J commit (bump + Title + Description + spicy-package.R +
# cran-comments) goes to main BEFORE submission, so the submitted
# tarball == the pushed tree, and CI re-validates the exact state.

# 06 CRAN SUBMISSION -------
devtools::release()
# or manually: https://cran.r-project.org/submit.html
# (confirm the incoming NOTE in the submission comment field).

# 07 AFTER ACCEPTANCE -------
# Immediately (standing rule):
#   git tag v0.13.0 && git push origin v0.13.0
# then run dev/03_post_release.R
