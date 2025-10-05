test_that("code_book basic structure works", {
  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  df <- head(mtcars) # petit dataset pour les tests CRAN rapides

  # Vérifie que la fonction retourne un datatable en mode interactif
  cb <- suppressMessages(code_book(df, export = "none", title = "Test Codebook"))
  expect_s3_class(cb, "datatables")
  expect_true(inherits(cb, "htmlwidget"))

  # Vérifie que les colonnes principales existent
  expect_true(all(c("Variable", "Label", "Values", "Class", "N_valid", "NAs") %in%
    names(cb$x$data)))
})

# ---- Tests d'export (uniquement en local) ---------------------------------
test_that("code_book exports work correctly", {
  skip_on_cran() # ne pas exécuter sur les serveurs CRAN
  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  df <- mtcars

  # 1. CSV export
  tmp_csv <- tempfile(fileext = ".csv")
  expect_silent(suppressMessages(code_book(df, export = "csv", file = tmp_csv)))
  expect_true(file.exists(tmp_csv))
  csv_data <- read.csv(tmp_csv)
  expect_true(all(c("Variable", "Label", "Values", "Class") %in% names(csv_data)))

  # 2. Excel export
  skip_if_not_installed("writexl")
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  expect_silent(suppressMessages(code_book(df, export = "excel", file = tmp_xlsx)))
  expect_true(file.exists(tmp_xlsx))

  # 3. PDF export
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("kableExtra")
  skip_if_not_installed("stringr")
  tmp_pdf <- tempfile(fileext = ".pdf")
  expect_silent(suppressMessages(code_book(df, export = "pdf", file = tmp_pdf)))
  expect_true(file.exists(tmp_pdf))
  expect_gt(file.info(tmp_pdf)$size, 1000)

  # 4. Word export
  tmp_docx <- tempfile(fileext = ".docx")
  expect_silent(suppressMessages(code_book(df, export = "word", file = tmp_docx)))
  expect_true(file.exists(tmp_docx))
  expect_gt(file.info(tmp_docx)$size, 1000)
})
