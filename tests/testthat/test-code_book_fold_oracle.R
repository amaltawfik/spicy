# The fold table of `code_book()` export filenames against its oracle,
# the Latin-ASCII transform of ICU (reached through stringi).
#
# The package never calls ICU: the table is frozen, so that a title gives
# the same file name on every platform and in every release (see
# data-raw/code_book_fold_table.R). This file is the one place where the
# frozen table and ICU are compared, entry for entry.
#
# Not in the CRAN tier. The comparison is exact, and it only means
# something against the ICU the table was generated with: under another
# ICU the test skips and says which one it met. A later ICU that folds a
# character differently is not a defect of the package. The frozen table
# stays, and following ICU is a decision for a release.

.cb_generated_with_icu <- "74."

test_that("the frozen table is ICU's Latin-ASCII transform, entry for entry", {
  skip_if_not_installed("stringi")
  icu <- stringi::stri_info()$ICU.version
  skip_if_not(
    startsWith(icu, .cb_generated_with_icu),
    paste0("the fold table was generated with ICU 74.1, this is ICU ", icu)
  )

  cp <- setdiff(0x80:0x2FFFF, 0xD800:0xDFFF)
  ch <- intToUtf8(cp, multiple = TRUE)
  to <- stringi::stri_trans_general(ch, "Latin-ASCII")
  folded <- !grepl("[^\\x01-\\x7F]", to, perl = TRUE) & nzchar(to) & to != ch

  # What the package removes before the lookup never reaches the table.
  kept <- folded & !code_book_is_dropped(cp)
  oracle_cp <- cp[kept]
  oracle_to <- to[kept]

  ours <- order(code_book_fold_from)
  expect_identical(code_book_fold_from[ours], oracle_cp)
  expect_identical(code_book_fold_to[ours], oracle_to)

  # The one place where the package departs from ICU on purpose: the soft
  # hyphen, which ICU folds to "-" and the package removes.
  dropped_but_folded <- cp[folded & code_book_is_dropped(cp)]
  expect_identical(dropped_but_folded, 0x00ADL)
})

test_that("every title folds as ICU folds it, apart from what is removed", {
  skip_if_not_installed("stringi")
  icu <- stringi::stri_info()$ICU.version
  skip_if_not(
    startsWith(icu, .cb_generated_with_icu),
    paste0("the fold table was generated with ICU 74.1, this is ICU ", icu)
  )

  titles <- c(
    "Âge & santé",
    "Straße und Größe",
    "Œuvre et cœur",
    "Việt Nam – enquête",
    "Łódź, Dvořák, İstanbul"
  )
  for (title in titles) {
    expect_identical(
      code_book_ascii_filename(title),
      gsub(
        "[`'\"^~]+",
        "",
        stringi::stri_trans_general(title, "Latin-ASCII"),
        perl = TRUE
      )
    )
  }
})
