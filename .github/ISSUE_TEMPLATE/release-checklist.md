---
name: "Release Checklist"
about: "Checklist to prepare and finalize a CRAN release of spicy"
title: "Release spicy X.Y.Z"
labels: ["release"]
assignees: []
---

# Release spicy X.Y.Z

## 01 DEVELOPMENT CHECKS
- [ ] Pull latest changes from `main`
- [ ] Check [current CRAN check results](https://cran.r-project.org/web/checks/check_results_spicy.html)
- [ ] Run relevant parts of `dev/01_dev_pkg.R`
- [ ] Confirm all local tests pass (`devtools::test()`)
- [ ] Confirm `devtools::check()` has no errors or warnings
- [ ] Verify README and pkgdown site build correctly

## 02 CRAN RELEASE PREPARATION
- [ ] Run relevant sections of `dev/02_release_cran.R`
- [ ] All WinBuilder checks passed (release, devel, oldrelease)
- [ ] All R-hub checks passed (Windows, macOS, Ubuntu)
- [ ] Reverse dependencies checked with `tools::package_dependencies()` (none detected)
- [ ] URLs and spelling verified
- [ ] `NEWS.md` updated [Tidyverse Style Guide — News and Release Notes](https://style.tidyverse.org/news.html#news-release)
- [ ] `cran-comments.md` updated
- [ ] Increment version using `usethis::use_version()`
- [ ] Submit to CRAN via `devtools::release()` **only when all checks are OK**
- [ ] Approve CRAN confirmation email

## 03 AFTER CRAN ACCEPTANCE
- [ ] Run relevant parts of `dev/03_post_release.R`
- [ ] GitHub tag and release created
- [ ] Pkgdown site rebuilt and pushed
- [ ] Development version started (`usethis::use_dev_version()`)
- [ ] Close this issue 🎉

---

### Notes
- Use the scripts under `/dev/` as guides, **run them section by section**.
- Always commit and push before running release scripts.
- Keep this issue open until CRAN acceptance and post-release tasks are complete.

---

### R-hub & WinBuilder checks
Run each platform one by one and review logs manually before submission.
