# Quarto re-parses an HTML table that does not opt out of its processing:
# it stretched a gt table of the package to the width of the page, with
# columns of equal widths and Bootstrap's stripes. Every gt table opts
# out (`.spicy_gt()`), except in a knit to a format Quarto writes from
# gt's HTML (Typst), where it reads the table (`knit_print.spicy_gt()`).

.gtq_tables <- function() {
  list(
    regression = table_regression(
      list(
        A = stats::lm(bmi ~ age + sex, data = sochealth),
        B = stats::lm(wellbeing_score ~ age + sex, data = sochealth)
      ),
      output = "gt"
    ),
    categorical = table_categorical(
      sochealth,
      select = c(smoking, physical_activity),
      by = sex,
      output = "gt"
    ),
    continuous = table_continuous(
      sochealth,
      select = c(age, bmi),
      by = sex,
      output = "gt"
    )
  )
}

.gtq_option <- function(g) {
  opt <- g[["_options"]]
  opt$value[[which(opt$parameter == "quarto_disable_processing")]]
}

.gtq_knit <- function(g, to) {
  old <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  knitr::opts_knit$set(rmarkdown.pandoc.to = to)
  on.exit(knitr::opts_knit$set(rmarkdown.pandoc.to = old), add = TRUE)
  as.character(knitr::knit_print(g))
}

test_that("every gt table opts out of Quarto's processing in HTML", {
  skip_if_not_installed("gt")
  for (g in .gtq_tables()) {
    expect_true(.gtq_option(g))
    flag <- "data-quarto-disable-processing=\"true\""
    expect_match(as.character(knit_print.spicy_gt(g)), flag, fixed = TRUE)
    expect_match(.gtq_knit(g, "html"), flag, fixed = TRUE)
    expect_match(
      as.character(gt::as_raw_html(g, inline_css = FALSE)),
      flag,
      fixed = TRUE
    )
  }
})

test_that("a knit to Typst leaves the table to Quarto's processing", {
  skip_if_not_installed("gt")
  for (g in .gtq_tables()) {
    out <- .gtq_knit(g, "typst")
    expect_match(out, "data-quarto-disable-processing=\"false\"", fixed = TRUE)
    expect_no_match(
      out,
      "data-quarto-disable-processing=\"true\"",
      fixed = TRUE
    )
  }
})
