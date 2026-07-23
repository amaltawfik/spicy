# ---------------------------------------------------------------------------
# Vignette map completeness. Two maps point readers at the articles:
# the "Learn more" section of the Get-started vignette
# (vignettes/spicy.Rmd) and _pkgdown.yml (navbar articles menu +
# articles index). Both are maintained by hand, so a new vignette can
# silently miss one of them. These tests enumerate vignettes/*.Rmd and
# fail, naming the vignette, whenever a map is incomplete -- and,
# conversely, whenever a map references a vignette that no longer
# exists. Plain-text parsing on purpose: the yaml package is not a
# declared dependency, and line-level assertions make failures easy
# to read. Skipped when the source tree is unavailable (installed
# package / built tarball, where _pkgdown.yml is not shipped).
# ---------------------------------------------------------------------------


.vignette_names <- function(vignettes_dir) {
  files <- list.files(vignettes_dir, pattern = "\\.Rmd$")
  files <- files[!startsWith(files, "_")]
  sort(sub("\\.Rmd$", "", files))
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


test_that("every vignette is linked from the Get-started 'Learn more' map", {
  vignettes_dir <- test_path("..", "..", "vignettes")
  skip_if(!dir.exists(vignettes_dir), "vignette sources not available")

  vignettes <- .vignette_names(vignettes_dir)
  learn_more <- paste(
    .learn_more_lines(file.path(vignettes_dir, "spicy.Rmd")),
    collapse = "\n"
  )

  # spicy.Rmd is the map itself; every other vignette needs an entry.
  required <- setdiff(vignettes, "spicy")
  linked <- vapply(
    required,
    function(nm) {
      grepl(sprintf('vignette("%s")', nm), learn_more, fixed = TRUE)
    },
    logical(1)
  )
  expect(
    all(linked),
    sprintf(
      paste0(
        "Vignette(s) missing from the 'Learn more' section of ",
        "vignettes/spicy.Rmd: %s. Add a `vignette(\"<name>\")` bullet ",
        "for each."
      ),
      paste(required[!linked], collapse = ", ")
    )
  )

  # Reverse direction: no stale vignette() call in the map.
  referenced <- unique(unlist(regmatches(
    learn_more,
    gregexpr('vignette\\("([A-Za-z0-9._-]+)"\\)', learn_more)
  )))
  referenced <- sub('^vignette\\("', "", sub('"\\)$', "", referenced))
  stale <- setdiff(referenced, vignettes)
  expect(
    length(stale) == 0L,
    sprintf(
      paste0(
        "The 'Learn more' section of vignettes/spicy.Rmd references ",
        "vignette(s) with no matching vignettes/*.Rmd file: %s."
      ),
      paste(stale, collapse = ", ")
    )
  )
})


test_that("every vignette appears in the _pkgdown.yml navbar articles menu", {
  vignettes_dir <- test_path("..", "..", "vignettes")
  pkgdown_yml <- test_path("..", "..", "_pkgdown.yml")
  skip_if(!dir.exists(vignettes_dir), "vignette sources not available")
  skip_if(!file.exists(pkgdown_yml), "_pkgdown.yml not available")

  vignettes <- .vignette_names(vignettes_dir)
  yml <- readLines(pkgdown_yml, warn = FALSE, encoding = "UTF-8")
  navbar <- .pkgdown_section(yml, "navbar")

  hrefs <- unlist(regmatches(
    navbar,
    gregexpr("articles/[A-Za-z0-9._-]+\\.html", navbar)
  ))
  navbar_pages <- unique(sub("^articles/", "", sub("\\.html$", "", hrefs)))

  missing <- setdiff(vignettes, navbar_pages)
  expect(
    length(missing) == 0L,
    sprintf(
      paste0(
        "Vignette(s) missing from the navbar articles menu of ",
        "_pkgdown.yml: %s. Add a '- text: <title>' / ",
        "'href: articles/<name>.html' entry."
      ),
      paste(missing, collapse = ", ")
    )
  )

  stale <- setdiff(navbar_pages, vignettes)
  expect(
    length(stale) == 0L,
    sprintf(
      paste0(
        "The _pkgdown.yml navbar links article page(s) with no ",
        "matching vignettes/*.Rmd file: %s."
      ),
      paste(stale, collapse = ", ")
    )
  )
})


test_that("every vignette appears in the _pkgdown.yml articles index", {
  vignettes_dir <- test_path("..", "..", "vignettes")
  pkgdown_yml <- test_path("..", "..", "_pkgdown.yml")
  skip_if(!dir.exists(vignettes_dir), "vignette sources not available")
  skip_if(!file.exists(pkgdown_yml), "_pkgdown.yml not available")

  vignettes <- .vignette_names(vignettes_dir)
  yml <- readLines(pkgdown_yml, warn = FALSE, encoding = "UTF-8")
  articles <- .pkgdown_section(yml, "articles")

  # Contents entries are bare names, e.g. "      - table-regression".
  # Lines such as "  - title: Articles" carry a colon and do not match.
  entries <- grep("^\\s+-\\s+[A-Za-z0-9._-]+\\s*$", articles, value = TRUE)
  index_entries <- unique(trimws(sub("^\\s*-\\s+", "", entries)))

  missing <- setdiff(vignettes, index_entries)
  expect(
    length(missing) == 0L,
    sprintf(
      paste0(
        "Vignette(s) missing from the articles index (articles: ",
        "contents:) of _pkgdown.yml: %s. Add the name to the ",
        "contents list."
      ),
      paste(missing, collapse = ", ")
    )
  )

  stale <- setdiff(index_entries, vignettes)
  expect(
    length(stale) == 0L,
    sprintf(
      paste0(
        "The _pkgdown.yml articles index lists entry names with no ",
        "matching vignettes/*.Rmd file: %s."
      ),
      paste(stale, collapse = ", ")
    )
  )
})
