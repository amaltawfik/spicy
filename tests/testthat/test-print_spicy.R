test_that("spicy_table printing and formatting work correctly", {
  # Create a styled frequency table
  tab <- freq(mtcars, cyl, styled = TRUE)

  # Check class
  expect_s3_class(tab, "spicy_table")
  expect_true(inherits(tab, "data.frame"))

  # --- print.spicy_table() ---
  expect_output(print(tab), "Total", ignore.case = TRUE)

  # --- format.spicy_table() ---
  txt <- format(tab)
  expect_type(txt, "character")
  expect_length(txt, 1)
  expect_true(any(grepl("Total", txt)))
  expect_true(any(grepl("cyl", txt)))

  # --- print_spicy() explicit helper ---
  expect_output(print_spicy(tab, padding = "normal"), "Total")

  # --- build_ascii_table() internal engine ---
  # (should not print, only return a string)
  internal <- spicy:::build_ascii_table(tab)
  expect_type(internal, "character")
  expect_length(internal, 1)
  expect_true(grepl("Total", internal))

  # --- capture.output compatibility ---
  captured <- capture.output(tab)
  expect_true(length(captured) > 5)
  expect_true(any(grepl("Total", captured)))
})

test_that("print_spicy() works on plain data.frames", {
  df <- head(mtcars)
  expect_output(print_spicy(df), "mpg|cyl|disp", ignore.case = TRUE)
  expect_type(capture.output(print_spicy(df)), "character")
})

test_that("spicy_table printing disables color safely when unavailable", {
  old_opt <- options(crayon.enabled = FALSE)
  on.exit(options(old_opt), add = TRUE)

  tab <- freq(mtcars, cyl, styled = TRUE)
  expect_output(print_spicy(tab), "Total")
})
