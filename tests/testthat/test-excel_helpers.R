# The text Excel shows for a number under a format, read on Excel 16.0
# for Windows (build 20527, decimal separator ".") on 2026-11-14 through
# the `Text` property of each cell of a sheet written by openxlsx2. Excel
# rounds the value read at 15 significant digits half away from zero,
# where R's `formatC()` rounds the binary value: 2.675 shows "2.68" in
# Excel, "2.67" in R.

test_that("the emulator shows what Excel showed, cell by cell", {
  values <- c(
    6.25,
    2.675,
    0.125,
    2.5,
    3.5,
    7.5,
    36.5,
    0.15,
    1.005,
    0.285,
    0.0233,
    0.0005,
    0.9995,
    -6.25,
    -0.125,
    -2.5,
    1 / 3,
    2 / 3,
    1234.5,
    -0.04,
    -0.0004
  )
  excel <- list(
    "0" = c(
      "6",
      "3",
      "0",
      "3",
      "4",
      "8",
      "37",
      "0",
      "1",
      "0",
      "0",
      "0",
      "1",
      "-6",
      "0",
      "-3",
      "0",
      "1",
      "1235",
      "0",
      "0"
    ),
    "0.0" = c(
      "6.3",
      "2.7",
      "0.1",
      "2.5",
      "3.5",
      "7.5",
      "36.5",
      "0.2",
      "1.0",
      "0.3",
      "0.0",
      "0.0",
      "1.0",
      "-6.3",
      "-0.1",
      "-2.5",
      "0.3",
      "0.7",
      "1234.5",
      "0.0",
      "0.0"
    ),
    "0.00" = c(
      "6.25",
      "2.68",
      "0.13",
      "2.50",
      "3.50",
      "7.50",
      "36.50",
      "0.15",
      "1.01",
      "0.29",
      "0.02",
      "0.00",
      "1.00",
      "-6.25",
      "-0.13",
      "-2.50",
      "0.33",
      "0.67",
      "1234.50",
      "-0.04",
      "0.00"
    ),
    "0.000" = c(
      "6.250",
      "2.675",
      "0.125",
      "2.500",
      "3.500",
      "7.500",
      "36.500",
      "0.150",
      "1.005",
      "0.285",
      "0.023",
      "0.001",
      "1.000",
      "-6.250",
      "-0.125",
      "-2.500",
      "0.333",
      "0.667",
      "1234.500",
      "-0.040",
      "0.000"
    ),
    "#.00" = c(
      "6.25",
      "2.68",
      ".13",
      "2.50",
      "3.50",
      "7.50",
      "36.50",
      ".15",
      "1.01",
      ".29",
      ".02",
      ".00",
      "1.00",
      "-6.25",
      "-.13",
      "-2.50",
      ".33",
      ".67",
      "1234.50",
      "-.04",
      ".00"
    ),
    "#.000" = c(
      "6.250",
      "2.675",
      ".125",
      "2.500",
      "3.500",
      "7.500",
      "36.500",
      ".150",
      "1.005",
      ".285",
      ".023",
      ".001",
      "1.000",
      "-6.250",
      "-.125",
      "-2.500",
      ".333",
      ".667",
      "1234.500",
      "-.040",
      ".000"
    )
  )
  specs <- list(
    "0" = spicy:::.xl_spec(0),
    "0.0" = spicy:::.xl_spec(1),
    "0.00" = spicy:::.xl_spec(2),
    "0.000" = spicy:::.xl_spec(3),
    "#.00" = spicy:::.xl_spec(2, lead_zero = FALSE),
    "#.000" = spicy:::.xl_spec(3, lead_zero = FALSE)
  )
  for (code in names(excel)) {
    expect_identical(spicy:::.xl_numfmt(specs[[code]]), code)
    shown <- vapply(values, spicy:::.xl_shown, "", spec = specs[[code]])
    expect_identical(shown, excel[[code]], label = code)
  }
  # The sections of a signed format, and a literal suffix.
  values <- c(
    0.001,
    -0.001,
    0,
    0.005,
    -0.005,
    12.375,
    -12.375,
    0.5,
    -0.5,
    2.675
  )
  excel <- list(
    "+0.00;-0.00;0.00" = c(
      "+0.00",
      "-0.00",
      "0.00",
      "+0.01",
      "-0.01",
      "+12.38",
      "-12.38",
      "+0.50",
      "-0.50",
      "+2.68"
    ),
    "0.00\" (1, 29)\"" = c(
      "0.00 (1, 29)",
      "0.00 (1, 29)",
      "0.00 (1, 29)",
      "0.01 (1, 29)",
      "-0.01 (1, 29)",
      "12.38 (1, 29)",
      "-12.38 (1, 29)",
      "0.50 (1, 29)",
      "-0.50 (1, 29)",
      "2.68 (1, 29)"
    ),
    "+0;-0;0" = c("+0", "-0", "0", "+0", "-0", "+12", "-12", "+1", "-1", "+3")
  )
  specs <- list(
    spicy:::.xl_spec(2, signed = TRUE),
    spicy:::.xl_spec(2, suffix = " (1, 29)"),
    spicy:::.xl_spec(0, signed = TRUE)
  )
  for (k in seq_along(excel)) {
    expect_identical(spicy:::.xl_numfmt(specs[[k]]), names(excel)[[k]])
    shown <- vapply(values, spicy:::.xl_shown, "", spec = specs[[k]])
    expect_identical(shown, excel[[k]], label = names(excel)[[k]])
  }
})

# Read on the same Excel 16.0 (build 20527) on 2026-11-14: a format with a
# section for the negative values keeps the minus of a value that rounds
# to zero, and changes nothing else.
test_that("the emulator shows the negative section as Excel showed it", {
  values <- c(
    -0.004,
    -0.04,
    -0.0004,
    -0.4,
    -0.0049,
    -0.005,
    -1.234,
    -12.5,
    -0.125,
    -6.25,
    -2.675,
    0,
    0.004,
    1.234,
    2.675,
    0.5,
    -0.5
  )
  excel <- list(
    "0.00;-0.00" = c(
      "-0.00",
      "-0.04",
      "-0.00",
      "-0.40",
      "-0.00",
      "-0.01",
      "-1.23",
      "-12.50",
      "-0.13",
      "-6.25",
      "-2.68",
      "0.00",
      "0.00",
      "1.23",
      "2.68",
      "0.50",
      "-0.50"
    ),
    "0.0;-0.0" = c(
      "-0.0",
      "-0.0",
      "-0.0",
      "-0.4",
      "-0.0",
      "-0.0",
      "-1.2",
      "-12.5",
      "-0.1",
      "-6.3",
      "-2.7",
      "0.0",
      "0.0",
      "1.2",
      "2.7",
      "0.5",
      "-0.5"
    ),
    "0.000;-0.000" = c(
      "-0.004",
      "-0.040",
      "-0.000",
      "-0.400",
      "-0.005",
      "-0.005",
      "-1.234",
      "-12.500",
      "-0.125",
      "-6.250",
      "-2.675",
      "0.000",
      "0.004",
      "1.234",
      "2.675",
      "0.500",
      "-0.500"
    ),
    "#.00;-#.00" = c(
      "-.00",
      "-.04",
      "-.00",
      "-.40",
      "-.00",
      "-.01",
      "-1.23",
      "-12.50",
      "-.13",
      "-6.25",
      "-2.68",
      ".00",
      ".00",
      "1.23",
      "2.68",
      ".50",
      "-.50"
    ),
    "#.000;-#.000" = c(
      "-.004",
      "-.040",
      "-.000",
      "-.400",
      "-.005",
      "-.005",
      "-1.234",
      "-12.500",
      "-.125",
      "-6.250",
      "-2.675",
      ".000",
      ".004",
      "1.234",
      "2.675",
      ".500",
      "-.500"
    ),
    "0;-0" = c(
      "-0",
      "-0",
      "-0",
      "-0",
      "-0",
      "-0",
      "-1",
      "-13",
      "-0",
      "-6",
      "-3",
      "0",
      "0",
      "1",
      "3",
      "1",
      "-1"
    ),
    "0.00\" (1, 29)\";-0.00\" (1, 29)\"" = c(
      "-0.00 (1, 29)",
      "-0.04 (1, 29)",
      "-0.00 (1, 29)",
      "-0.40 (1, 29)",
      "-0.00 (1, 29)",
      "-0.01 (1, 29)",
      "-1.23 (1, 29)",
      "-12.50 (1, 29)",
      "-0.13 (1, 29)",
      "-6.25 (1, 29)",
      "-2.68 (1, 29)",
      "0.00 (1, 29)",
      "0.00 (1, 29)",
      "1.23 (1, 29)",
      "2.68 (1, 29)",
      "0.50 (1, 29)",
      "-0.50 (1, 29)"
    )
  )
  specs <- list(
    spicy:::.xl_spec(2),
    spicy:::.xl_spec(1),
    spicy:::.xl_spec(3),
    spicy:::.xl_spec(2, lead_zero = FALSE),
    spicy:::.xl_spec(3, lead_zero = FALSE),
    spicy:::.xl_spec(0),
    spicy:::.xl_spec(2, suffix = " (1, 29)")
  )
  for (k in seq_along(excel)) {
    s <- specs[[k]]
    s$neg <- TRUE
    expect_identical(spicy:::.xl_numfmt(s), names(excel)[[k]])
    shown <- vapply(values, spicy:::.xl_shown, "", spec = s)
    expect_identical(shown, excel[[k]], label = names(excel)[[k]])
  }
})

test_that(".xl_cell() writes a number Excel shows as the console does, or text", {
  cell <- spicy:::.xl_cell
  spec2 <- spicy:::.xl_spec(2)
  # The typed value at full precision, under the column's format.
  expect_identical(
    cell("65.07", 65.0712, spec2),
    list(value = 65.0712, numfmt = "0.00")
  )
  # Half way: Excel would show 6.3, the console shows 6.2.
  expect_identical(
    cell("6.2", 6.25, spicy:::.xl_spec(1)),
    list(value = 6.2, numfmt = "0.0")
  )
  # A p-value shortened by the style takes the format its text shows.
  expect_identical(
    cell(".02", 0.0233, spicy:::.xl_spec(3, lead_zero = FALSE)),
    list(value = 0.0233, numfmt = "#.00")
  )
  # No typed value: the value shown.
  expect_identical(cell("12.5", NA, NULL), list(value = 12.5, numfmt = "0.0"))
  # The reader's mark: a comma is the point of a number cell.
  expect_identical(
    cell("65,07", 65.0712, spec2, ","),
    list(value = 65.0712, numfmt = "0.00")
  )
  expect_identical(cell("1·5", 1.5, spicy:::.xl_spec(1), "·")$value, 1.5)
  # Signs and suffixes of the regression's codes.
  expect_identical(
    cell("+0.07", 0.0712, spicy:::.xl_spec(2, signed = TRUE))$numfmt,
    "+0.00;-0.00;0.00"
  )
  expect_identical(
    cell("12.38 (1, 29)", 12.3812, spicy:::.xl_spec(2, suffix = " (1, 29)")),
    list(value = 12.3812, numfmt = "0.00\" (1, 29)\"")
  )
  # A negative value printed "-0.00": a section for the negative values,
  # in that cell only; the other cells keep the column's format.
  expect_identical(
    cell("-0.00", -0.0038, spec2),
    list(value = -0.0038, numfmt = "0.00;-0.00")
  )
  expect_identical(
    cell("-.00", -0.0038, spicy:::.xl_spec(2, lead_zero = FALSE))$numfmt,
    "#.00;-#.00"
  )
  expect_identical(cell("-0.004", -0.0038, spicy:::.xl_spec(3))$numfmt, "0.000")
  # With no typed value, its sign is not known: the console's text.
  expect_identical(cell("-0.00", NA, spec2), list(text = "-0.00"))
  # Text: what no number shows.
  for (x in c(
    "<.001",
    "<0,001",
    "3.40e+07",
    "12.3 (4.5)",
    "[1.20, 3.40]",
    "–",
    "64.63***",
    "Yes",
    "249/1175"
  )) {
    expect_identical(cell(x, 1, spec2, ","), list(text = x), label = x)
  }
  expect_null(cell("", 1, spec2))
  expect_null(cell(NA_character_, 1, spec2))
})
