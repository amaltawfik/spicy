test_that("sochealth carries a multiple-response question of five items", {
  items <- c(
    "info_doctor",
    "info_pharmacist",
    "info_internet",
    "info_family",
    "info_media"
  )
  expect_identical(dim(sochealth), c(1200L, 29L))
  expect_identical(
    match(items, names(sochealth)),
    match("dentist_12m", names(sochealth)) + 1:5
  )
  for (v in items) {
    expect_type(sochealth[[v]], "logical")
    expect_identical(names(attributes(sochealth[[v]])), "label", info = v)
  }
  expect_identical(
    vapply(sochealth[items], \(x) attr(x, "label"), "", USE.NAMES = FALSE),
    c(
      "Doctor or other health professional",
      "Pharmacist",
      "Internet, websites or apps",
      "Family or friends",
      "Television, radio or newspapers"
    )
  )

  # A skipped question is missing on all five items, and only then.
  m <- as.matrix(sochealth[items])
  n_na <- rowSums(is.na(m))
  expect_identical(sum(n_na == 5), 36L)
  expect_true(all(n_na %in% c(0, 5)))
  answered <- n_na == 0
  prev <- 100 * colMeans(m[answered, ])
  expect_true(all(abs(prev - c(62, 22, 45, 31, 12)) < 2))

  p_age <- function(v) {
    tab <- table(sochealth$age_group[answered], m[answered, v])
    stats::chisq.test(tab, correct = FALSE)$p.value
  }
  expect_gt(p_age("info_pharmacist"), 0.3)
  expect_lt(p_age("info_doctor"), 0.001)
})
