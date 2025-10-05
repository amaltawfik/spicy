# ---- Basic structure tests -----------------------------------------------
test_that("code_book basic structure works", {
  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  df <- head(mtcars) # small dataset for quick CRAN-friendly tests

  # Check that the function returns a datatable object in interactive mode
  cb <- suppressMessages(code_book(df, export = "none", title = "Test Codebook"))
  expect_s3_class(cb, "datatables")
  expect_true(inherits(cb, "htmlwidget"))

  # Verify that the main columns exist in the generated table
  expect_true(all(c("Variable", "Label", "Values", "Class", "N_valid", "NAs") %in%
    names(cb$x$data)))
})


# ---- Export tests (skipped on CI/CRAN if dependencies are missing) -------
test_that("code_book exports work correctly", {
  skip_on_cran() # do not run heavy export tests on CRAN servers
  skip_on_ci() # skip on GitHub Actions or similar CI environments

  skip_if_not_installed("DT")
  skip_if_not_installed("cli")

  df <- mtcars

  # ---- CSV export --------------------------------------------------------
  tmp_csv <- tempfile(fileext = ".csv")
  expect_silent(suppressMessages(code_book(df, export = "csv", file = tmp_csv)))
  expect_true(file.exists(tmp_csv))

  csv_data <- read.csv(tmp_csv)
  expect_true(all(c("Variable", "Label", "Values", "Class") %in% names(csv_data)))

  # ---- Excel export ------------------------------------------------------
  skip_if_not_installed("writexl")

  tmp_xlsx <- tempfile(fileext = ".xlsx")
  expect_silent(suppressMessages(code_book(df, export = "excel", file = tmp_xlsx)))
  expect_true(file.exists(tmp_xlsx))

  # ---- PDF export --------------------------------------------------------
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("kableExtra")
  skip_if_not_installed("stringr")
  skip_if_not_installed("tinytex")

  # Check if LaTeX is available (compatible with all tinytex versions)
  latex_ok <- tryCatch(
    {
      if ("is_latex_installed" %in% getNamespaceExports("tinytex")) {
        tinytex::is_latex_installed()
      } else {
        tinytex::latexmk_available()
      }
    },
    error = function(e) FALSE
  )

  if (!latex_ok) {
    skip("LaTeX not available on this system")
  }


  tmp_pdf <- tempfile(fileext = ".pdf")
  expect_silent(suppressMessages(code_book(df, export = "pdf", file = tmp_pdf)))
  expect_true(file.exists(tmp_pdf))
  expect_gt(file.info(tmp_pdf)$size, 1000)

  # ---- Word export -------------------------------------------------------
  tmp_docx <- tempfile(fileext = ".docx")
  expect_silent(suppressMessages(code_book(df, export = "word", file = tmp_docx)))
  expect_true(file.exists(tmp_docx))
  expect_gt(file.info(tmp_docx)$size, 1000)
})
