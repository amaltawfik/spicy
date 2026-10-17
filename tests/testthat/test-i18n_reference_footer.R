test_that("the reference-categories footer follows the language", {
  fit <- lm(wellbeing_score ~ sex + smoking, data = sochealth)

  en <- paste(
    capture.output(print(table_regression(fit, reference_style = "footer"))),
    collapse = "\n"
  )
  expect_match(
    en,
    "Reference categories: sex = Female; smoking = [^;]+\\.",
    fixed = FALSE
  )

  withr::local_options(spicy.language = "fr")
  fr <- paste(
    capture.output(print(table_regression(fit, reference_style = "footer"))),
    collapse = "\n"
  )
  expect_match(
    fr,
    "Modalit\u00E9s de r\u00E9f\u00E9rence\u00A0: sex = Female\u00A0; smoking = ",
    fixed = TRUE
  )
  expect_false(grepl("Reference categories", fr, fixed = TRUE))
})
