# The column `blocks` of the registry is the contract of each class: on
# the fixtures of helper-class-fixtures.R, the headers of the blocks the
# default table shows (.REG_BLOCK_TERMS) are among those the cell of the
# class names, and a block the cell names without a condition is shown.
# dev/decisions/2026-11-19-capability-registry.md.
#
# A cell is a list of items separated by ";" (a block name holds commas:
# "Zero hurdle, Pr(y = 0)"). An item opens with the name of a block, the
# longest that fits; what follows the name (a parenthesis, or "for a
# zero-truncated family ...") is the condition under which the block is
# shown, and such an item may be absent from a fixture. An item that
# opens with no block name ("per-outcome blocks", "distribution
# parameters") describes rows that are not a block, and the fixtures of
# its class then show none.

blocks_of_cell <- function(cell) {
  items <- trimws(strsplit(cell, ";", fixed = TRUE)[[1L]])
  out <- list(allowed = character(0), required = character(0))
  names_by_length <- .REG_BLOCK_TERMS[order(-nchar(.REG_BLOCK_TERMS))]
  for (it in items) {
    hit <- names_by_length[startsWith(it, names_by_length)][1L]
    if (is.na(hit)) {
      next
    }
    out$allowed <- c(out$allowed, hit)
    if (!nzchar(trimws(substring(it, nchar(hit) + 1L)))) {
      out$required <- c(out$required, hit)
    }
  }
  out
}

blocks_shown <- function(fit) {
  tb <- suppressWarnings(suppressMessages(
    table_regression(fit, output = "data.frame")
  ))
  cols <- intersect(c("variable", "label", "factor_term"), names(tb))
  intersect(.REG_BLOCK_TERMS, unique(unlist(tb[cols])))
}

test_that("every block a cell names is a block of .REG_BLOCK_TERMS or a description", {
  reg <- .supported_models_registry()
  expect_identical(
    blocks_of_cell(reg$blocks[reg$class == "clm"])$required,
    "Thresholds"
  )
  expect_setequal(
    blocks_of_cell(reg$blocks[reg$class == "clm"])$allowed,
    c("Thresholds", "Non-proportional effects", "Scale effects")
  )
  expect_identical(
    blocks_of_cell(reg$blocks[reg$class == "glmmTMB"])$required,
    "Random effects"
  )
  expect_length(blocks_of_cell("per-outcome blocks")$allowed, 0L)
})

for (id in names(.CLASS_FIXTURES)) {
  local({
    id <- id
    test_that(paste0("the registry gives the blocks of ", id), {
      skip_on_cran()
      fit <- .class_fixture(id)
      reg <- .supported_models_registry()
      cell <- blocks_of_cell(reg$blocks[
        reg$class == .CLASS_FIXTURES[[id]]$class
      ])
      shown <- blocks_shown(fit)
      expect_identical(
        setdiff(shown, cell$allowed),
        character(0),
        label = paste(id, "shows a block its cell does not name")
      )
      expect_identical(
        setdiff(cell$required, shown),
        character(0),
        label = paste(id, "misses a block its cell names")
      )
    })
  })
}
