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
    "Catégories de référence : sex = Female ; smoking = ",
    fixed = TRUE
  )
  expect_false(grepl("Reference categories", fr, fixed = TRUE))
})
