test_that("print.spicy_freq_table prints correctly and invisibly", {
  library(labelled)

  # Create a simple labelled vector
  x <- labelled(
    c(1, 2, 2, 3, 3, 3, NA),
    labels = c("Low" = 1, "Medium" = 2, "High" = 3)
  )
  var_label(x) <- "Satisfaction level"

  # Generate unstyled frequency table
  df <- freq(x, styled = FALSE)

  # Test invisibility of print method
  expect_invisible(print.spicy_freq_table(df))

  # Capture console output for inspection
  output <- capture.output(print.spicy_freq_table(df))

  # --- General checks
  expect_true(any(grepl("Frequency table:", output)))
  expect_true(any(grepl("Valid", output)))
  expect_true(any(grepl("Total", output)))

  # --- Footer checks
  expect_true(any(grepl("Label: Satisfaction level", output)))
  expect_true(any(grepl("Class:", output)))
  expect_true(any(grepl("Data:", output)))

  # --- Structural checks
  expect_true(any(grepl("\u2500", output))) # has horizontal lines
  expect_true(any(grepl("\u2502", output))) # has vertical bars
})


test_that("print.spicy_freq_table handles weighted tables", {
  df <- data.frame(
    sexe = factor(c("Male", "Female", "Female", "Male", NA)),
    poids = c(1.2, 0.8, 1.5, 1.0, 0.7)
  )

  # Weighted frequency table
  ftab <- freq(df, sexe, weights = poids, styled = FALSE)

  output <- capture.output(print.spicy_freq_table(ftab))

  expect_true(any(grepl("Weight:", output)))
  expect_true(any(grepl("rescaled", output)))
  expect_true(any(grepl("Total", output)))
})


test_that("print.spicy_freq_table handles variables without labels or missing values", {
  # Vector without label and no missing values
  x <- factor(c("A", "B", "B", "A", "C", "A"))

  df <- freq(x, styled = FALSE)
  output <- capture.output(print.spicy_freq_table(df))

  # Expect no Label line, but expect Data/Class
  expect_false(any(grepl("^Label:", output)))
  expect_true(any(grepl("^Class:", output)))
  expect_true(any(grepl("^Data:", output)))
})
