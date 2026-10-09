# An NA cell of an Excel export is not written at all, so Excel sees it
# as empty. `na.strings = ""` wrote an empty text instead, which Excel
# counts (COUNTA, filters) and refuses in arithmetic (#VALUE!). The sheet
# XML tells the two apart: an empty text is a cell of type inlineStr
# with an empty <t/>; an unwritten cell is absent.

# The number of empty text cells in every sheet of a workbook.
xlsx_empty_text_cells <- function(path) {
  files <- utils::unzip(path, list = TRUE)$Name
  sheets <- grep("^xl/worksheets/sheet[0-9]+\\.xml$", files, value = TRUE)
  sum(vapply(
    sheets,
    function(sheet) {
      con <- unz(path, sheet)
      on.exit(close(con))
      xml <- paste(readLines(con, warn = FALSE), collapse = "")
      cells <- regmatches(xml, gregexpr("<c [^>]*>.*?</c>", xml))[[1]]
      sum(grepl("t=\"inlineStr\"", cells) & grepl("<t/>|<t></t>", cells))
    },
    integer(1)
  ))
}

test_that("an NA cell of an Excel export is left unwritten, not an empty text", {
  skip_if_not_installed("openxlsx2")
  exports <- list(
    regression = function(path) {
      table_regression(
        lm(mpg ~ wt + factor(cyl), data = mtcars),
        output = "excel",
        excel_path = path
      )
    },
    categorical = function(path) {
      table_categorical(
        sochealth,
        select = smoking,
        output = "excel",
        excel_path = path
      )
    },
    categorical_by = function(path) {
      table_categorical(
        sochealth,
        select = smoking,
        by = sex,
        output = "excel",
        excel_path = path
      )
    },
    continuous = function(path) {
      table_continuous(
        iris,
        select = c(Sepal.Length, Petal.Width),
        by = Species,
        output = "excel",
        excel_path = path
      )
    },
    continuous_lm = function(path) {
      table_continuous_lm(
        iris,
        select = c(Sepal.Length, Petal.Width),
        by = Species,
        output = "excel",
        excel_path = path
      )
    }
  )
  for (name in names(exports)) {
    path <- withr::local_tempfile(fileext = ".xlsx")
    exports[[name]](path)
    # The export has empty cells (read back as NA), none written as text.
    back <- openxlsx2::read_xlsx(path, col_names = FALSE)
    expect_true(anyNA(back), label = paste(name, "has empty cells"))
    expect_identical(
      xlsx_empty_text_cells(path),
      0L,
      label = paste(name, "empty text cells")
    )
  }
})
