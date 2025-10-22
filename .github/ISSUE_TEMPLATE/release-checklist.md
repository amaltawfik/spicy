---
name: "Release Checklist"
about: "Checklist to prepare and finalize a CRAN release of spicy"
title: "Release spicy X.Y.Z"
labels: ["release"]
assignees: []
---

# Release spicy X.Y.Z

## 01 DEVELOPMENT CHECKS -------
- [ ] Pull latest changes from `main`
- [ ] Run `source("dev/01_dev_pkg.R")`
- [ ] Confirm all local tests pass (`devtools::test()`)
- [ ] Confirm `devtools::check()` has no errors or warnings
- [ ] Verify README and pkgdown site build correctly

## 02 CRAN RELEASE PREPARATION -------
- [ ] Run `source("dev/02_release_cran.R")`
- [ ] All WinBuilder checks passed (release, devel, oldrelease)
- [ ] All R-hub checks passed (Windows, macOS, Ubuntu)
- [ ] Reverse dependency checks clean
- [ ] URLs and spelling verified
- [ ] `NEWS.md` and `cran-comments.md` updated
- [ ] Increment version using `usethis::use_version()`
- [ ] Submit to CRAN via `devtools::release()`
- [ ] Approve CRAN confirmation email

## 03 AFTER CRAN ACCEPTANCE -------
- [ ] Run `source("dev/03_post_release.R")`
- [ ] GitHub tag and release created
- [ ] Pkgdown site rebuilt and pushed
- [ ] Development version started (`usethis::use_dev_version()`)
- [ ] Close this issue 🎉

---

### Notes
- Use the scripts under `/dev/` for each step.
- Always commit and push before running release scripts.
- Keep this issue open until CRAN acceptance and post-release tasks are complete.
