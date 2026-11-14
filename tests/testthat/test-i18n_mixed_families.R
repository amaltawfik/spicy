# A side-by-side table of several model families under another language
# than English: its type sentence and its title are worded in that
# language when every type has a translation (decision 42, amended
# 2026-11-14), as the rest of its note is.

test_that("a mixed-family table is worded in French", {
  withr::local_options(spicy.language = "fr")
  m_lm <- lm(bmi ~ age + sex, data = sochealth)
  m_lg <- glm(smoking ~ age + sex, data = sochealth, family = binomial)
  x <- suppressWarnings(table_regression(list(m_lm, m_lg)))
  expect_identical(
    attr(x, "title"),
    spicy_fmt("title_regression_comparison", "Régression")
  )
  expect_true(startsWith(
    attr(x, "note"),
    paste0(
      "Note. Modèle 1 : régression linéaire ; ",
      "Modèle 2 : régression logistique. "
    )
  ))
  # The names of the list name the models.
  y <- suppressWarnings(table_regression(list(A = m_lm, B = m_lg)))
  expect_true(startsWith(
    attr(y, "note"),
    "Note. A : régression linéaire ; B : "
  ))
  # English is unchanged.
  withr::local_options(spicy.language = "en")
  z <- suppressWarnings(table_regression(list(m_lm, m_lg)))
  expect_identical(attr(z, "title"), "Regression comparison")
  expect_true(startsWith(
    attr(z, "note"),
    "Note. Model 1: linear regression; Model 2: logistic regression. "
  ))
})
