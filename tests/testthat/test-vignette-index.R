# ---------------------------------------------------------------------------
# Documentation map completeness. The package ships one vignette,
# vignettes/spicy.Rmd (Get started); the twenty walk-throughs are
# pkgdown articles under vignettes/articles/, kept out of the tarball by
# .Rbuildignore and rendered at the same articles/<slug>.html URLs.
#
# Three maps point readers at those articles: the "Learn more" section
# of Get started, the _pkgdown.yml navbar articles menu, and the
# _pkgdown.yml articles index. All three are maintained by hand, so a
# new article can silently miss one. These tests enumerate
# vignettes/articles/*.Rmd and fail, naming the article, whenever a map
# is incomplete -- and, conversely, whenever a map references an article
# that no longer exists. Plain-text parsing on purpose: the yaml package
# is not a declared dependency, and line-level assertions make failures
# easy to read. Skipped when the source tree is unavailable (installed
# package / built tarball, where neither _pkgdown.yml nor the articles
# are shipped).
# ---------------------------------------------------------------------------

.site_url <- "https://amaltawfik.github.io/spicy"

# The maps live in the source tree only. An unpacked tarball carries
# vignettes/ (spicy.Rmd) but not vignettes/articles/, which
# .Rbuildignore drops -- so both must be there before any map is read.
.have_sources <- function(vignettes_dir) {
  dir.exists(vignettes_dir) && dir.exists(file.path(vignettes_dir, "articles"))
}

# The twenty articles, by slug (vignettes/articles/<slug>.Rmd).
.article_names <- function(vignettes_dir) {
  files <- list.files(file.path(vignettes_dir, "articles"), pattern = "\\.Rmd$")
  files <- files[!startsWith(files, "_")]
  sort(sub("\\.Rmd$", "", files))
}

# Every page pkgdown renders under articles/: the articles plus the one
# vignette, which pkgdown puts there too.
.page_names <- function(vignettes_dir) {
  sort(c("spicy", .article_names(vignettes_dir)))
}

# Lines of the "## Learn more" section of vignettes/spicy.Rmd (from
# the heading to the next level-2 heading or end of file).
.learn_more_lines <- function(spicy_rmd) {
  lines <- readLines(spicy_rmd, warn = FALSE, encoding = "UTF-8")
  start <- grep("^## Learn more", lines)
  if (length(start) != 1L) {
    stop("expected exactly one '## Learn more' heading in ", spicy_rmd)
  }
  rest <- lines[seq(start + 1L, length(lines))]
  next_heading <- grep("^## ", rest)
  if (length(next_heading)) {
    rest <- rest[seq_len(next_heading[1L] - 1L)]
  }
  rest
}

# Lines of one top-level section of _pkgdown.yml (from "key:" at
# column 0 to the line before the next column-0 key).
.pkgdown_section <- function(yml_lines, key) {
  top <- grep("^[A-Za-z_-]+:", yml_lines)
  start <- grep(paste0("^", key, ":"), yml_lines)
  if (length(start) != 1L) {
    stop("expected exactly one top-level '", key, ":' key in _pkgdown.yml")
  }
  after <- top[top > start]
  end <- if (length(after)) after[1L] - 1L else length(yml_lines)
  yml_lines[start:end]
}


test_that("the package ships exactly one vignette, Get started", {
  vignettes_dir <- test_path("..", "..", "vignettes")
  skip_if(!.have_sources(vignettes_dir), "article sources not available")

  top_level <- list.files(vignettes_dir, pattern = "\\.Rmd$")
  expect_equal(sort(top_level), "spicy.Rmd")

  # A walk-through added at the top level would be rebuilt by CRAN on
  # every check; it belongs in vignettes/articles/.
  expect_true(length(.article_names(vignettes_dir)) > 0L)
})


test_that("every article is linked from the Get-started 'Learn more' map", {
  vignettes_dir <- test_path("..", "..", "vignettes")
  skip_if(!.have_sources(vignettes_dir), "article sources not available")

  articles <- .article_names(vignettes_dir)
  learn_more <- paste(
    .learn_more_lines(file.path(vignettes_dir, "spicy.Rmd")),
    collapse = "\n"
  )

  # Get started is the only installed vignette: it must reach the
  # articles by absolute URL, since vignette() no longer resolves them.
  linked <- vapply(
    articles,
    function(nm) {
      grepl(
        sprintf("%s/articles/%s.html", .site_url, nm),
        learn_more,
        fixed = TRUE
      )
    },
    logical(1)
  )
  expect(
    all(linked),
    sprintf(
      paste0(
        "Article(s) missing from the 'Learn more' section of ",
        "vignettes/spicy.Rmd: %s. Add a bullet linking to ",
        "%s/articles/<name>.html."
      ),
      paste(articles[!linked], collapse = ", "),
      .site_url
    )
  )

  # Reverse direction: no stale article link in the map.
  referenced <- unique(unlist(regmatches(
    learn_more,
    gregexpr(
      paste0(.site_url, "/articles/([A-Za-z0-9._-]+)\\.html"),
      learn_more
    )
  )))
  referenced <- sub(
    paste0("^", .site_url, "/articles/"),
    "",
    sub("\\.html$", "", referenced)
  )
  stale <- setdiff(referenced, .page_names(vignettes_dir))
  expect(
    length(stale) == 0L,
    sprintf(
      paste0(
        "The 'Learn more' section of vignettes/spicy.Rmd links ",
        "article page(s) with no matching source file: %s."
      ),
      paste(stale, collapse = ", ")
    )
  )
})


test_that("every article appears in the _pkgdown.yml navbar articles menu", {
  vignettes_dir <- test_path("..", "..", "vignettes")
  pkgdown_yml <- test_path("..", "..", "_pkgdown.yml")
  skip_if(!.have_sources(vignettes_dir), "article sources not available")
  skip_if(!file.exists(pkgdown_yml), "_pkgdown.yml not available")

  pages <- .page_names(vignettes_dir)
  yml <- readLines(pkgdown_yml, warn = FALSE, encoding = "UTF-8")
  navbar <- .pkgdown_section(yml, "navbar")

  hrefs <- unlist(regmatches(
    navbar,
    gregexpr("articles/[A-Za-z0-9._-]+\\.html", navbar)
  ))
  navbar_pages <- unique(sub("^articles/", "", sub("\\.html$", "", hrefs)))

  missing <- setdiff(pages, navbar_pages)
  expect(
    length(missing) == 0L,
    sprintf(
      paste0(
        "Article(s) missing from the navbar articles menu of ",
        "_pkgdown.yml: %s. Add a '- text: <title>' / ",
        "'href: articles/<name>.html' entry."
      ),
      paste(missing, collapse = ", ")
    )
  )

  stale <- setdiff(navbar_pages, pages)
  expect(
    length(stale) == 0L,
    sprintf(
      paste0(
        "The _pkgdown.yml navbar links article page(s) with no ",
        "matching source file under vignettes/: %s."
      ),
      paste(stale, collapse = ", ")
    )
  )
})


test_that("every article appears in the _pkgdown.yml articles index", {
  vignettes_dir <- test_path("..", "..", "vignettes")
  pkgdown_yml <- test_path("..", "..", "_pkgdown.yml")
  skip_if(!.have_sources(vignettes_dir), "article sources not available")
  skip_if(!file.exists(pkgdown_yml), "_pkgdown.yml not available")

  # pkgdown names an article by its path relative to vignettes/ minus
  # the extension, so the index selects the moved ones as
  # "articles/<slug>" and Get started as plain "spicy".
  expected <- c("spicy", paste0("articles/", .article_names(vignettes_dir)))
  yml <- readLines(pkgdown_yml, warn = FALSE, encoding = "UTF-8")
  articles <- .pkgdown_section(yml, "articles")

  # Contents entries are bare names, e.g. "      - articles/table-regression".
  # Lines such as "  - title: Articles" carry a colon and do not match.
  entries <- grep("^\\s+-\\s+[A-Za-z0-9._/-]+\\s*$", articles, value = TRUE)
  index_entries <- unique(trimws(sub("^\\s*-\\s+", "", entries)))

  missing <- setdiff(expected, index_entries)
  expect(
    length(missing) == 0L,
    sprintf(
      paste0(
        "Article(s) missing from the articles index (articles: ",
        "contents:) of _pkgdown.yml: %s. Add the name to the ",
        "contents list."
      ),
      paste(missing, collapse = ", ")
    )
  )

  stale <- setdiff(index_entries, expected)
  expect(
    length(stale) == 0L,
    sprintf(
      paste0(
        "The _pkgdown.yml articles index lists entry names with no ",
        "matching source file under vignettes/: %s."
      ),
      paste(stale, collapse = ", ")
    )
  )
})


# Phase 3 matrix - vignettes-news:vignettes-exist (lot T4)

test_that("the article names promised in NEWS are all present", {
  vignettes_dir <- test_path("..", "..", "vignettes")
  skip_if(!.have_sources(vignettes_dir), "article sources not available")
  # The NEWS 'Seven new vignettes' bullet: mixed, GEE, multinomial,
  # counts, survival, ordinal, categorical-predictors -- plus the
  # supported-models map it points at.
  named <- c(
    "table-regression-mixed",
    "table-regression-gee",
    "table-regression-multinomial",
    "table-regression-counts",
    "table-regression-survival",
    "table-regression-ordinal",
    "categorical-predictors",
    "table-regression-supported-models"
  )
  present <- .article_names(vignettes_dir)
  missing <- setdiff(named, present)
  expect(
    length(missing) == 0L,
    sprintf(
      paste0(
        "Article(s) promised in NEWS.md are missing from ",
        "vignettes/articles/: %s."
      ),
      paste(missing, collapse = ", ")
    )
  )
})
