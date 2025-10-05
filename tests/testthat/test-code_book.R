test_that("code_book basic functionality works", {
  skip_on_cran() # évite de ralentir les checks CRAN
  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  # jeu de test simple
  df <- mtcars

  # 1. Export = none (doit retourner un objet datatable)
  cb <- code_book(df, export = "none", title = "Test Codebook")
  expect_s3_class(cb, "datatables")
  expect_true(inherits(cb, "htmlwidget"))

  # 2. Export = csv
  tmp_csv <- tempfile(fileext = ".csv")
  expect_silent(code_book(df, export = "csv", file = tmp_csv))
  expect_true(file.exists(tmp_csv))
  csv_data <- read.csv(tmp_csv)
  expect_true(all(c("Variable", "Label", "Values", "Class") %in% names(csv_data)))

  # 3. Export = excel
  skip_if_not_installed("writexl")
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  expect_silent(code_book(df, export = "excel", file = tmp_xlsx))
  expect_true(file.exists(tmp_xlsx))

  # 4. Export = pdf
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("kableExtra")
  skip_if_not_installed("stringr")
  tmp_pdf <- tempfile(fileext = ".pdf")
  expect_silent(code_book(df, export = "pdf", file = tmp_pdf))
  expect_true(file.exists(tmp_pdf))
  expect_gt(file.info(tmp_pdf)$size, 1000) # PDF non vide

  # 5. Export = word
  tmp_docx <- tempfile(fileext = ".docx")
  expect_silent(code_book(df, export = "word", file = tmp_docx))
  expect_true(file.exists(tmp_docx))
  expect_gt(file.info(tmp_docx)$size, 1000) # Word non vide
})
