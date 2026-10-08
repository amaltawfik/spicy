cb_data <- function() {
  d <- data.frame(
    sex = factor(c("F", "M", "F", NA, "M", "F"), levels = c("F", "M", "X")),
    grade = factor(
      c("low", "high", "mid", "mid", NA, "low"),
      levels = c("low", "mid", "high"),
      ordered = TRUE
    ),
    score = c(10, 12, 10, NA, 15, 12),
    id = 1:6,
    ok = c(TRUE, FALSE, TRUE, TRUE, NA, FALSE),
    comment = letters[1:6],
    day = as.Date("2024-05-01") + c(0, 3, 1, NA, 2, 9)
  )
  attr(d$sex, "label") <- "Sex"
  attr(d$score, "label") <- "Score (0-20)"
  d
}

# Two variables share the declared code 8, and q3 carries tagged NAs, one
# of them without a label.
cb_labelled <- function() {
  data.frame(
    q1 = haven::labelled_spss(
      c(1, 2, 8, 9, 1, NA, 2, 8),
      labels = c(Agree = 1, Disagree = 2, Neutral = 3, DK = 8, Refused = 9),
      na_values = c(8, 9)
    ),
    q2 = haven::labelled_spss(
      c(1, 1, 2, 8, NA, 2, 2, 1),
      labels = c(Yes = 1, No = 2, DK = 8),
      na_values = 8
    ),
    q3 = haven::labelled(
      c(1, 2, haven::tagged_na("a"), NA, 1, 2, 2, haven::tagged_na("b")),
      labels = c(Yes = 1, No = 2, Refused = haven::tagged_na("a"))
    )
  )
}

# The rows of one variable against freq(), matched on the code: freq()
# gives the codes with `labelled_levels = "values"`, and NA for the system
# missing row. Every row of one must be a row of the other.
expect_freq_rows <- function(rows, x, ...) {
  ref <- freq(x, labelled_levels = "values", output = "data.frame", ...)
  code <- ref$value
  code[is.na(code)] <- "NA"
  m <- match(rows$code, code)
  expect_identical(sort(m), seq_along(code))
  expect_equal(rows$n, ref$n[m])
  expect_equal(rows$pct_total, 100 * ref$prop[m])
  expect_equal(rows$pct_valid, 100 * ref$valid_prop[m])
}


test_that("code_book() returns the codebook, which prints as the list of variables", {
  res <- withVisible(code_book(cb_data()))
  expect_true(res$visible)
  cb <- res$value
  expect_output(print(cb), "Observations: 6")
  expect_s3_class(cb, "spicy_codebook")
  expect_named(cb, c("header", "variables", "values"))
  expect_identical(attr(cb, "language"), "en")
  expect_identical(attr(cb, "decimal_mark"), ".")
  expect_named(
    cb$variables,
    c(
      "position",
      "name",
      "label",
      "type",
      "class",
      "source",
      "n_valid",
      "n_missing",
      "n_declared_missing",
      "declared_codes",
      "n_distinct",
      "n_categories",
      "min",
      "max",
      "mean",
      "sd",
      "median",
      "earliest",
      "latest"
    )
  )
  expect_named(
    cb$values,
    c(
      "variable",
      "code",
      "label",
      "declared_missing",
      "n",
      "pct_total",
      "pct_valid"
    )
  )
  expect_identical(cb$header$n_obs, 6L)
  expect_identical(cb$header$n_vars, 7L)
  expect_identical(cb$header$title, "Codebook")
  expect_s3_class(cb$header$date, "Date")
})

test_that("variables follow the selection, with positions in the data", {
  cb <- code_book(cb_data(), score, sex)
  expect_identical(cb$variables$name, c("score", "sex"))
  expect_identical(cb$variables$position, c(3L, 1L))
  expect_identical(cb$variables$label, c("Score (0-20)", "Sex"))
  expect_identical(cb$variables$class, c("numeric", "factor"))
  expect_identical(cb$variables$n_valid, c(5L, 5L))
  expect_identical(cb$variables$n_missing, c(1L, 1L))
  expect_identical(cb$variables$n_distinct, c(3L, 2L))
})

test_that("counts match freq() and table()", {
  d <- cb_data()
  cb <- code_book(d)
  sex <- cb$values[cb$values$variable == "sex", ]
  expect_identical(sex$code, c("F", "M", "X", "NA"))
  expect_freq_rows(sex, d$sex, factor_levels = "all")
  expect_equal(
    sex$n,
    as.vector(table(d$sex, useNA = "ifany"))
  )

  # Levels in level order, not in alphabetical order.
  grade <- cb$values[cb$values$variable == "grade", ]
  expect_identical(grade$code, c("low", "mid", "high", "NA"))
  expect_freq_rows(grade, d$grade, factor_levels = "all")

  ok <- cb$values[cb$values$variable == "ok", ]
  expect_identical(ok$code, c("FALSE", "TRUE", "NA"))
  expect_freq_rows(ok, d$ok)
  expect_equal(ok$n, as.vector(table(d$ok, useNA = "ifany")))
  expect_true(all(!cb$values$declared_missing))
})

test_that("declared missing values follow user_na", {
  skip_if_not_installed("haven")
  d <- cb_labelled()
  cb <- code_book(d)
  q1 <- cb$values[cb$values$variable == "q1", ]
  expect_identical(q1$code, c("1", "2", "3", "8", "9", "NA"))
  expect_identical(
    q1$label,
    c("Agree", "Disagree", "Neutral", "DK", "Refused", NA)
  )
  expect_identical(
    q1$declared_missing,
    c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
  )
  expect_freq_rows(q1, d$q1, factor_levels = "all")
  expect_identical(cb$variables$n_declared_missing, c(3L, 1L, 2L))
  expect_identical(cb$variables$n_missing, c(4L, 2L, 3L))
  expect_identical(cb$variables$n_valid, c(4L, 6L, 5L))

  q3 <- cb$values[cb$values$variable == "q3", ]
  expect_identical(q3$code, c("1", "2", "NA(a)", "NA(b)", "NA"))
  expect_identical(q3$label, c("Yes", "No", "Refused", NA, NA))
  expect_freq_rows(q3, d$q3, factor_levels = "all")

  dm <- cb$header$declared_missing
  expect_identical(dm$code, c("8", "9", "NA(a)", "NA(b)"))
  expect_identical(dm$label, c("DK", "Refused", "Refused", NA))
  expect_identical(dm$variables, c("q1, q2", "q1", "q3", "q3"))
  expect_identical(dm$n_variables, c(2L, 1L, 1L, 1L))
  # By code, then label: a code declared under two labels gives two
  # adjacent rows, and 99 comes after 9.
  two <- data.frame(
    a = haven::labelled_spss(
      c(1, 99, 8),
      labels = c(Refused = 99, DK = 8),
      na_values = c(8, 99)
    ),
    b = haven::labelled_spss(
      c(1, 8, 9),
      labels = c("Don't know" = 8, X = 9),
      na_values = c(8, 9)
    )
  )
  dm <- code_book(two)$header$declared_missing
  expect_identical(dm$code, c("8", "8", "9", "99"))
  expect_identical(dm$label, c("DK", "Don't know", "X", "Refused"))
  expect_identical(dm$variables, c("a", "b", "b", "a"))

  off <- code_book(d, user_na = FALSE)
  q1_off <- off$values[off$values$variable == "q1", ]
  expect_identical(q1_off$code, c("1", "2", "3", "8", "9", "NA"))
  expect_false(any(off$values$declared_missing))
  expect_freq_rows(q1_off, d$q1, factor_levels = "all", user_na = FALSE)
  # Tagged NAs stay missing, in the single NA row.
  q3_off <- off$values[off$values$variable == "q3", ]
  expect_identical(q3_off$code, c("1", "2", "NA"))
  expect_identical(q3_off$n, c(2L, 3L, 3L))
  expect_freq_rows(q3_off, d$q3, factor_levels = "all", user_na = FALSE)
  expect_identical(off$variables$n_declared_missing, c(0L, 0L, 0L))
  expect_identical(nrow(off$header$declared_missing), 0L)
})

test_that("a declared range flags every code inside it", {
  skip_if_not_installed("haven")
  r <- haven::labelled_spss(
    c(1, 2, 97, 98, 99, 1, NA),
    labels = c(A = 1, B = 2, DK = 98),
    na_range = c(97, 99)
  )
  cb <- code_book(data.frame(r = r))
  expect_identical(cb$values$code, c("1", "2", "97", "98", "99", "NA"))
  expect_identical(
    cb$values$declared_missing,
    c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_identical(cb$values$label[[4]], "DK")
  expect_freq_rows(cb$values, r, factor_levels = "all")
  expect_identical(cb$variables$n_declared_missing, 3L)
})

test_that("declared_codes writes how each variable declares its missing values", {
  skip_if_not_installed("haven")
  spss <- function(...) haven::labelled_spss(c(1, 2, 9000, 9998), ...)
  d <- data.frame(
    values = spss(na_values = c(9998, 9999)),
    range = spss(na_range = c(9000, 9999)),
    both = spss(na_values = 9998, na_range = c(1e5, 1e6)),
    none = spss(labels = c(A = 1)),
    tagged = haven::labelled(c(1, 2, 3, haven::tagged_na("a")), c(A = 1)),
    plain = c(1, 2, 9000, 9998)
  )
  expect_identical(
    code_book(d)$variables$declared_codes,
    c("9998, 9999", "9000\u20139999", "9998; 100000\u20131000000", NA, NA, NA)
  )
  # user_na = FALSE sets the declaration aside, as it does the counts.
  off <- code_book(d, user_na = FALSE)$variables
  expect_true(all(is.na(off$declared_codes)))
  # An open range (SPSS "9000 THRU HI", "LO THRU 9") reads as a bound.
  open <- data.frame(
    hi = spss(na_range = c(9000, Inf)),
    lo = haven::labelled_spss(c(1, 2, 9000, 9998), na_range = c(-Inf, 1))
  )
  expect_identical(
    code_book(open)$variables$declared_codes,
    c("≥ 9000", "≤ 1")
  )
  # The separator of the values and the range follows the language.
  withr::local_options(spicy.language = "fr")
  expect_identical(
    code_book(d)$variables$declared_codes[[3]],
    "9998\u00A0; 100000\u20131000000"
  )
})

test_that("factor_levels = 'observed' lists only the values present", {
  cb <- code_book(cb_data(), sex, factor_levels = "observed")
  expect_identical(cb$values$code, c("F", "M", "NA"))
  skip_if_not_installed("haven")
  cb <- code_book(cb_labelled(), q1, factor_levels = "observed")
  expect_identical(cb$values$code, c("1", "2", "8", "9", "NA"))
})

test_that("numeric, text and date variables have no category rows", {
  cb <- code_book(cb_data())
  expect_identical(unique(cb$values$variable), c("sex", "grade", "ok"))
  # Character codes of a labelled vector.
  d <- data.frame(
    g = labelled::labelled(
      c("M", "F", "M"),
      labels = c(Male = "M", Female = "F")
    )
  )
  cb <- code_book(d)
  expect_identical(cb$values$code, c("F", "M"))
  expect_identical(cb$values$label, c("Female", "Male"))
})

test_that("values caps the categories listed per variable", {
  cb <- code_book(cb_data(), values = 2)
  # The first two categories of each variable stay, in their order, with
  # their percentages of the whole variable; the NA rows stay too.
  sex <- cb$values[cb$values$variable == "sex", ]
  expect_identical(sex$code, c("F", "M", "NA"))
  full <- code_book(cb_data())$values
  expect_equal(sex$pct_valid[1:2], full$pct_valid[full$variable == "sex"][1:2])
  expect_identical(cb$variables$n_distinct[cb$variables$name == "grade"], 3L)
  expect_identical(cb$variables$n_categories[cb$variables$name == "grade"], 3L)
  expect_true(is.na(cb$variables$n_categories[cb$variables$name == "score"]))
  cb <- code_book(cb_data(), values = 0)
  expect_true(all(cb$values$code == "NA" | cb$values$declared_missing))
  expect_named(cb$values, names(code_book(cb_data())$values))
  expect_identical(nrow(code_book(cb_data(), values = Inf)$values), 11L)

  # Past the cap, a variable that declares missing codes keeps the rows of
  # its declared and system missing values, as a numeric variable does.
  skip_if_not_installed("haven")
  q <- haven::labelled_spss(
    c(1, 2, 8, NA),
    labels = c(A = 1, B = 2, DK = 8),
    na_values = 8
  )
  cb <- code_book(
    data.frame(q = q, f = factor(c("a", "b", "a", NA))),
    values = 1
  )
  # One category each, then the declared and system missing values; the
  # valid percentage of the category listed is still of all valid values.
  expect_identical(cb$values$variable, c("q", "q", "q", "f", "f"))
  expect_identical(cb$values$code, c("1", "8", "NA", "a", "NA"))
  expect_identical(
    cb$values$declared_missing,
    c(FALSE, TRUE, FALSE, FALSE, FALSE)
  )
  expect_identical(cb$values$n, c(1L, 1L, 1L, 2L, 1L))
  expect_equal(cb$values$pct_valid, c(50, NA, NA, 200 / 3, NA))
  expect_identical(cb$variables$n_categories, c(2L, 2L))
})

test_that("labels that only name declared missing codes leave the stored type", {
  skip_if_not_installed("haven")
  inc <- c(1500, 2300, 2300, 4100)
  d <- data.frame(
    inc = haven::labelled_spss(
      c(inc, 99998, 99999),
      labels = c(Refused = 99999, "Don't know" = 99998),
      na_values = c(99998, 99999)
    ),
    txt = haven::labelled_spss(
      c("a", "b", "Z", "a", NA, "b"),
      labels = c(Refused = "Z"),
      na_values = "Z"
    ),
    bare = haven::labelled(c(1.5, 2.5, 2.5, 4, 5, 6))
  )
  cb <- code_book(d)
  expect_identical(cb$variables$type, c("numeric", "text", "numeric"))
  # The summaries of the valid values alone.
  stats <- c("min", "max", "mean", "sd", "median")
  plain <- code_book(data.frame(inc = inc))$variables
  expect_equal(cb$variables[1, stats], plain[stats], ignore_attr = TRUE)
  expect_identical(cb$values$variable, c("inc", "inc", "txt", "txt"))
  expect_identical(cb$values$code, c("99998", "99999", "Z", "NA"))
  expect_identical(cb$values$declared_missing, c(TRUE, TRUE, TRUE, FALSE))
  # Without the declaration, the labels are categories again; a labelled
  # vector without labels stays numeric.
  expect_identical(
    code_book(d, user_na = FALSE)$variables$type,
    c("categorical (labelled codes)", "categorical (labelled codes)", "numeric")
  )
  empty <- haven::labelled(
    c("a", "b"),
    labels = stats::setNames(character(), character())
  )
  expect_identical(
    code_book(data.frame(empty = empty), user_na = FALSE)$variables$type,
    "text"
  )
})

test_that("factor_levels = 'all' lists a declared code that nobody gave", {
  skip_if_not_installed("haven")
  d <- data.frame(
    y = haven::labelled_spss(
      c(1, 2, 1, NA),
      labels = c(Agree = 1, Disagree = 2, Refused = 9),
      na_values = 9
    )
  )
  cb <- code_book(d)
  expect_identical(cb$values$code, c("1", "2", "9", "NA"))
  expect_identical(cb$values$n, c(2L, 1L, 0L, 1L))
  expect_identical(cb$values$declared_missing, c(FALSE, FALSE, TRUE, FALSE))
  expect_identical(cb$values$label[[3]], "Refused")
  expect_identical(cb$header$declared_missing$code, "9")
  expect_identical(cb$header$declared_missing$n_variables, 1L)
  observed <- code_book(d, factor_levels = "observed")
  expect_identical(observed$values$code, c("1", "2", "NA"))
  expect_identical(nrow(observed$header$declared_missing), 0L)

  # A code of `na_values` with no label is listed too: the band of the
  # PDF says "8, 9", and so does the table.
  z <- haven::labelled_spss(
    c(1, 2, 8, NA),
    labels = c(A = 1, B = 2, DK = 8),
    na_values = c(8, 9)
  )
  cb <- code_book(data.frame(z = z))
  expect_identical(cb$variables$declared_codes, "8, 9")
  expect_identical(cb$values$code, c("1", "2", "8", "9", "NA"))
  expect_identical(cb$values$n, c(1L, 1L, 1L, 0L, 1L))
  expect_identical(cb$values$label[[4]], NA_character_)
  expect_identical(
    code_book(data.frame(z = z), factor_levels = "observed")$values$code,
    c("1", "2", "8", "NA")
  )
})

test_that("an NA level is system missing, as in freq()", {
  f <- addNA(factor(c("a", NA, "a")))
  cb <- code_book(data.frame(f = f))
  expect_identical(cb$values$code, c("a", "NA"))
  expect_identical(cb$values$n, c(2L, 1L))
  expect_equal(cb$values$pct_valid, c(100, NA))
  expect_freq_rows(cb$values, f)
  expect_identical(cb$variables$n_valid, 2L)
  expect_identical(cb$variables$n_missing, 1L)
  expect_identical(cb$variables$n_distinct, 1L)
  o <- addNA(factor(c("lo", NA), levels = c("lo", "hi"), ordered = TRUE))
  cb <- code_book(data.frame(o = o))
  expect_identical(cb$variables$type, "categorical (ordinal)")
  expect_identical(cb$values$code, c("lo", "hi", "NA"))
})

test_that("a level spelled NA is quoted, apart from the missing values", {
  d <- data.frame(g = factor(c("NA", "b", NA), levels = c("NA", "b")))
  expect_identical(code_book(d)$values$code, c("\"NA\"", "b", "NA"))
})

test_that("declared codes are written in full", {
  skip_if_not_installed("haven")
  z <- haven::labelled_spss(
    c(1, 2, 1e5, 1e6, 1e5),
    labels = c(A = 1, B = 2),
    na_values = c(1e5, 1e6)
  )
  cb <- code_book(data.frame(z = z))
  expect_identical(cb$values$code, c("1", "2", "100000", "1000000"))
  expect_identical(cb$header$declared_missing$code, c("100000", "1000000"))
})

test_that("a logical lists FALSE and TRUE under factor_levels = 'all'", {
  d <- data.frame(b = c(TRUE, TRUE, NA))
  cb <- code_book(d)
  expect_identical(cb$values$code, c("FALSE", "TRUE", "NA"))
  expect_identical(cb$values$n, c(0L, 2L, 1L))
  expect_identical(
    code_book(d, factor_levels = "observed")$values$code,
    c("TRUE", "NA")
  )
})

test_that("the type vocabulary is read off the R class, in English and French", {
  d <- data.frame(
    f = factor("a"),
    o = factor("a", ordered = TRUE),
    l = labelled::labelled(1, labels = c(A = 1)),
    i = 1L,
    n = 1.5,
    b = TRUE,
    s = "a",
    d = as.Date("2024-01-01"),
    t = as.POSIXct("2024-01-01 10:00:00", tz = "UTC"),
    h = as.difftime(1, units = "hours")
  )
  en <- c(
    "categorical (nominal)",
    "categorical (ordinal)",
    "categorical (labelled codes)",
    "numeric",
    "numeric",
    "logical",
    "text",
    "date",
    "date-time",
    "difftime"
  )
  expect_identical(code_book(d)$variables$type, en)
  withr::local_options(spicy.language = "fr")
  fr <- c(
    "catégorielle (nominale)",
    "catégorielle (ordinale)",
    "catégorielle (codes étiquetés)",
    "numérique",
    "numérique",
    "logique",
    "texte",
    "date",
    "date-heure",
    "difftime"
  )
  cb <- code_book(d)
  expect_identical(cb$variables$type, fr)
  expect_identical(attr(cb, "language"), "fr")
})

test_that("numeric summaries, and range = FALSE", {
  d <- cb_data()
  v <- code_book(d)$variables
  s <- v[v$name == "score", ]
  x <- d$score[!is.na(d$score)]
  expect_equal(
    unlist(s[c("min", "max", "mean", "sd", "median")], use.names = FALSE),
    c(min(x), max(x), mean(x), stats::sd(x), stats::median(x))
  )
  expect_true(all(is.na(v$mean[v$name %in% c("sex", "comment", "day")])))
  narrow <- code_book(d, range = FALSE)$variables
  expect_false(any(c("min", "max", "earliest", "latest") %in% names(narrow)))
  expect_true(all(c("mean", "sd", "median") %in% names(narrow)))
  allna <- code_book(data.frame(x = c(NA_real_, NA_real_)))
  expect_true(is.na(allna$variables$mean))
  expect_identical(nrow(allna$values), 0L)
})

test_that("dates are ISO text, date-times in their zone or in UTC", {
  instant <- 1717236000 # 2024-06-01 10:00:00 UTC
  d <- data.frame(day = as.Date(c("2024-06-01", NA, "2023-12-31")))
  d$tokyo <- .POSIXct(instant + c(0, 3600, NA), tz = "Asia/Tokyo")
  d$bare <- structure(instant + c(0, 60, 120), class = c("POSIXct", "POSIXt"))
  d$local <- .POSIXct(instant + c(0, 60, 120), tz = "")
  d$none <- as.Date(c(NA, NA, NA))
  v <- code_book(d)$variables
  expect_identical(
    v$earliest[1:4],
    c(
      "2023-12-31",
      "2024-06-01 19:00:00 Asia/Tokyo",
      "2024-06-01 10:00:00 UTC",
      "2024-06-01 10:00:00 UTC"
    )
  )
  expect_identical(
    v$latest[1:4],
    c(
      "2024-06-01",
      "2024-06-01 20:00:00 Asia/Tokyo",
      "2024-06-01 10:02:00 UTC",
      "2024-06-01 10:02:00 UTC"
    )
  )
  expect_true(is.na(v$earliest[[5]]))

  # A POSIXlt column is a date-time too, in its own zone.
  lt <- data.frame(id = 1:3)
  lt$t <- as.POSIXlt(.POSIXct(instant + c(0, NA, 120), tz = "Asia/Tokyo"))
  v <- code_book(lt)$variables
  expect_identical(v$type[[2]], "date-time")
  expect_identical(v$class[[2]], "POSIXlt, POSIXt")
  expect_identical(
    c(v$earliest[[2]], v$latest[[2]]),
    c("2024-06-01 19:00:00 Asia/Tokyo", "2024-06-01 19:02:00 Asia/Tokyo")
  )
  expect_identical(c(v$n_valid[[2]], v$n_missing[[2]]), c(2L, 1L))
})

test_that("source maps current names to their codes in the source file", {
  cb <- code_book(cb_data(), sex, score, source = c(score = "Q3", sex = "Q1"))
  expect_identical(cb$variables$source, c("Q1", "Q3"))
  expect_identical(code_book(cb_data(), sex)$variables$source, NA_character_)
  expect_error(
    code_book(cb_data(), source = c("Q1", "Q3")),
    class = "spicy_invalid_input"
  )
  expect_error(
    code_book(cb_data(), source = c(sex = "Q1", sex = "Q2")),
    class = "spicy_invalid_input"
  )
  expect_error(
    code_book(cb_data(), sex, source = c(score = "Q3")),
    class = "spicy_invalid_input"
  )
  # A code must say something.
  for (blank in c("", "  ")) {
    expect_error(
      code_book(cb_data(), sex, score, source = c(score = "Q3", sex = blank)),
      class = "spicy_invalid_input"
    )
  }
})

test_that("authors take the three shapes of lssdoc's argument", {
  none <- code_book(cb_data(), sex)$header$authors
  expect_named(none, c("name", "affiliation", "orcid"))
  expect_identical(nrow(none), 0L)
  expect_type(none$name, "character")

  a <- code_book_authors(c("Jane Doe" = "HESAV", "Bob"))
  expect_identical(a$name, c("Jane Doe", "Bob"))
  expect_identical(a$affiliation, c("HESAV", ""))
  expect_identical(a$orcid, c("", ""))

  a <- code_book_authors(list(
    list(
      name = "Jane Doe",
      affiliation = "HESAV",
      orcid = "0000-0002-1825-0097"
    ),
    list(name = "Bob")
  ))
  expect_identical(a$affiliation, c("HESAV", ""))
  expect_identical(a$orcid, c("0000-0002-1825-0097", ""))
  expect_identical(code_book_authors(list(j = list(name = "J")))$name, "J")
  # A blank field is an empty one; a padded one is trimmed.
  a <- code_book_authors(list(
    list(name = " Jane ", affiliation = "  ", orcid = " 0000-0002-1825-0097 "),
    list(name = "Bob", orcid = " ")
  ))
  expect_identical(a$name, c("Jane", "Bob"))
  expect_identical(
    c(a$affiliation, a$orcid),
    c("", "", "0000-0002-1825-0097", "")
  )
  # An ORCID given as its address keeps the identifier alone.
  urls <- c(
    "https://orcid.org/0000-0002-1825-0097",
    "http://www.orcid.org/0000-0002-1825-0097",
    "https://WWW.ORCID.ORG/0000-0002-1825-0097"
  )
  a <- code_book_authors(lapply(urls, \(u) list(name = "J", orcid = u)))
  expect_identical(a$orcid, rep("0000-0002-1825-0097", 3L))

  cb <- code_book(cb_data(), sex, authors = c("Jane Doe" = "HESAV"))
  expect_identical(cb$header$authors$name, "Jane Doe")

  bad <- list(
    1,
    NA_character_,
    "",
    list("Jane"),
    list(list(affiliation = "HESAV")),
    list(list(name = "Jane", orcid = c("a", "b"))),
    list(list(name = NA))
  )
  for (b in bad) {
    expect_error(
      code_book(cb_data(), authors = b),
      class = "spicy_bad_authors"
    )
  }
  expect_error(code_book(cb_data(), authors = 1), class = "spicy_invalid_input")
})

test_that("notes and title reach the header", {
  cb <- code_book(
    cb_data(),
    sex,
    notes = c("First.", "", "Second.", "  "),
    title = NULL
  )
  expect_identical(cb$header$notes, c("First.", "Second."))
  expect_identical(cb$header$title, NA_character_)
  expect_identical(cb$header$subtitle, NA_character_)
  out <- capture.output(res <- withVisible(print(cb)))
  expect_false(res$visible)
  expect_identical(res$value, cb)
  expect_match(out[[1]], "^Date: ")
  expect_false(any(grepl("Codebook", out, fixed = TRUE)))
  expect_true("Note: Second." %in% out)
  expect_error(code_book(cb_data(), notes = 1), class = "spicy_invalid_input")
  expect_error(
    code_book(cb_data(), notes = NA_character_),
    class = "spicy_invalid_input"
  )
  expect_error(code_book(cb_data(), title = ""), class = "spicy_invalid_input")
  for (s in list(NA_character_, c("a", "b"), 1, " ")) {
    expect_error(
      code_book(cb_data(), subtitle = s),
      class = "spicy_invalid_input"
    )
  }
})

test_that("the print is pinned", {
  skip_if_not_installed("haven")
  d <- cbind(cb_data()[c("sex", "score", "day")], cb_labelled()[1:6, ])
  cb <- code_book(
    d,
    subtitle = "Wave 1",
    authors = c("Jane Doe" = "HESAV", "Bob"),
    notes = c("Fictitious data.", "- Marked note.")
  )
  cb$header$date <- as.Date("2026-10-07")
  expect_snapshot(print(cb))

  withr::local_options(spicy.language = "fr")
  cb <- code_book(d, sex, q1, authors = c("Jane Doe" = "HESAV"))
  cb$header$date <- as.Date("2026-10-07")
  expect_snapshot(print(cb))
})

test_that("the print keeps the language the codebook was built in", {
  cb <- withr::with_options(
    list(spicy.language = "fr"),
    code_book(cb_data(), sex)
  )
  out <- capture.output(print(cb))
  expect_true(any(grepl("Libellé", out, fixed = TRUE)))
  expect_true(any(grepl("Observations : 6", out, fixed = TRUE)))
  expect_null(getOption("spicy.language"))
})

test_that("the print fits the console width by cutting long labels", {
  label <- "A label long enough to push the list past a narrow console"
  d <- data.frame(id = 1:3, x = c(2.5, 3, NA))
  attr(d$x, "label") <- label
  cb <- code_book(d)
  ellipsis <- spicy_str("marker_truncation_ellipsis")

  # The table closes the print: header, rule and two rows. At 60 columns
  # even a 12-character label would leave the table wider than the
  # console (names and types are never cut), so the label stays whole.
  withr::local_options(width = 60)
  tbl <- utils::tail(capture.output(print(cb)), 4L)
  expect_true(any(grepl(label, tbl, fixed = TRUE)))
  expect_false(any(grepl(ellipsis, tbl, fixed = TRUE)))
  expect_true(any(grepl("numeric", tbl, fixed = TRUE)))

  # At 70 columns the cut makes the table fit exactly.
  withr::local_options(width = 70)
  tbl <- utils::tail(capture.output(print(cb)), 4L)
  expect_identical(max(nchar(tbl)), 70L)
  expect_true(any(grepl(ellipsis, tbl, fixed = TRUE)))
  expect_identical(cb$variables$label[[2]], label)

  withr::local_options(width = 200)
  out <- capture.output(print(cb))
  expect_true(any(grepl(label, out, fixed = TRUE)))
  expect_false(any(grepl(ellipsis, out, fixed = TRUE)))
})

test_that("the print keeps a label on its row, and counts columns, not characters", {
  d <- data.frame(id = 1:3, x = c(2.5, 3, NA))
  attr(d$x, "label") <- "a\nb\tc"
  last <- utils::tail(capture.output(print(code_book(d))), 1L)
  expect_match(last, "a b c", fixed = TRUE)
  expect_match(last, "numeric", fixed = TRUE)

  # Twenty-four CJK characters take 48 columns of the console: cut to
  # fit 70 columns, the table does not exceed them.
  skip_if_not(l10n_info()[["UTF-8"]], "needs a UTF-8 locale")
  attr(d$x, "label") <- strrep("\u6f22", 24)
  withr::local_options(width = 70)
  tbl <- utils::tail(capture.output(print(code_book(d))), 4L)
  expect_lte(max(nchar(tbl, type = "width")), 70L)
  expect_true(any(grepl(
    spicy_str("marker_truncation_ellipsis"),
    tbl,
    fixed = TRUE
  )))
})

test_that("decimal_mark: argument > style > language", {
  expect_identical(attr(code_book(cb_data(), sex), "decimal_mark"), ".")
  withr::local_options(spicy.language = "fr")
  expect_identical(attr(code_book(cb_data(), sex), "decimal_mark"), ",")
  withr::local_options(spicy.style = "lancet")
  expect_identical(attr(code_book(cb_data(), sex), "decimal_mark"), "·")
  expect_identical(
    attr(code_book(cb_data(), sex, decimal_mark = "."), "decimal_mark"),
    "."
  )
  # The mark of a style is one the argument accepts.
  expect_identical(
    attr(code_book(cb_data(), sex, decimal_mark = "·"), "decimal_mark"),
    "·"
  )
  expect_error(
    code_book(cb_data(), decimal_mark = ".."),
    class = "spicy_invalid_input"
  )
})

test_that("removed and malformed arguments are classed errors", {
  d <- cb_data()
  for (arg in list(list(filename = "x"), list(include_na = TRUE))) {
    err <- expect_error(
      do.call(code_book, c(list(d), arg)),
      class = "spicy_invalid_input"
    )
    expect_s3_class(err, "spicy_defunct")
  }
  expect_error(code_book(1:3), class = "spicy_invalid_data")
  # The released logical `values` is now a count.
  for (v in list(TRUE, FALSE)) {
    err <- expect_error(code_book(d, values = v), class = "spicy_defunct")
    expect_s3_class(err, "spicy_invalid_input")
  }
  for (v in list(-1, 1.5, NA_real_, c(1, 2), "10")) {
    expect_error(code_book(d, values = v), class = "spicy_invalid_input")
  }
  expect_error(code_book(d, range = "yes"), class = "spicy_invalid_input")
  expect_error(code_book(d, user_na = NA), class = "spicy_invalid_input")
  expect_error(code_book(d, factor_levels = "x"), class = "spicy_invalid_input")
  expect_error(code_book(d, output = "cb.csv"), class = "spicy_invalid_input")
  expect_error(code_book(d, output = "xlsx"), class = "spicy_invalid_input")
  for (ext in c("xlsx", "pdf", "typ")) {
    expect_error(
      code_book(
        d,
        output = file.path(tempdir(), "no-such-dir", paste0("cb.", ext))
      ),
      class = "spicy_invalid_input"
    )
    # A directory is not a file, whatever its extension.
    dir <- withr::local_tempdir(fileext = paste0(".", ext))
    expect_error(code_book(d, output = dir), class = "spicy_invalid_input")
  }
  expect_error(code_book(d, output = NA), class = "spicy_invalid_input")
  expect_error(code_book(d, output = ""), class = "spicy_invalid_input")
  expect_error(code_book(d, value = 5), class = "spicy_invalid_input")
  expect_error(code_book(d, out = "cb.xlsx"), class = "spicy_invalid_input")
  expect_error(code_book(d, picked = sex), class = "spicy_invalid_input")
})

test_that("errors name code_book(), not the helpers that raise them", {
  d <- cb_data()
  called <- function(err) rlang::call_name(conditionCall(err))
  err <- expect_error(code_book(d, paper = "A3"), class = "spicy_invalid_input")
  expect_identical(called(err), "code_book")
  err <- expect_error(
    code_book(d, factor_levels = "x"),
    class = "spicy_invalid_input"
  )
  expect_identical(called(err), "code_book")
  # A renamed selection says so in the message, and an unknown column is
  # tidyselect's error.
  err <- expect_error(
    code_book(d, gender = sex),
    "cannot rename them in code_book().",
    fixed = TRUE
  )
  expect_identical(called(err), "code_book")
  expect_identical(called(expect_error(code_book(d, nope))), "code_book")
})

test_that("a raw or complex column is listed without a warning", {
  d <- data.frame(r = as.raw(1:3), z = complex(real = 1:3, imaginary = 1))
  expect_no_warning(cb <- code_book(d))
  expect_identical(cb$variables$type, c("raw", "complex"))
  expect_identical(cb$variables$n_valid, c(3L, 3L))
})

test_that("an empty selection gives empty tables", {
  expect_warning(
    cb <- code_book(cb_data(), starts_with("zzz")),
    class = "spicy_no_selection"
  )
  expect_identical(nrow(cb$variables), 0L)
  expect_identical(nrow(cb$values), 0L)
  expect_identical(cb$header$n_vars, 0L)
  expect_output(print(cb), "Variables: 0")
})

test_that("all-missing factors list their levels with no valid base", {
  cb <- code_book(data.frame(f = factor(c(NA, NA), levels = c("a", "b"))))
  expect_identical(cb$values$code, c("a", "b", "NA"))
  expect_true(all(is.na(cb$values$pct_valid)))
  expect_equal(cb$values$pct_total, c(0, 0, 100))
})

test_that("writing an Excel file needs openxlsx2", {
  local_mocked_bindings(spicy_pkg_available = function(pkg) FALSE)
  expect_error(
    code_book(cb_data(), output = "cb.xlsx"),
    class = "spicy_missing_pkg"
  )
})

test_that("the Excel codebook reads back", {
  skip_if_not_installed("openxlsx2")
  skip_if_not_installed("haven")
  path <- withr::local_tempfile(fileext = ".xlsx")
  authors <- list(
    list(
      name = "Jane Doe",
      affiliation = "HESAV",
      orcid = "0000-0002-1825-0097"
    ),
    list(name = "Bob")
  )
  # One declared code (8 in q2), so that its row and the notes come after
  # the two counts.
  d <- cbind(cb_data(), cb_labelled()[1:6, "q2", drop = FALSE])
  expect_silent(
    cb <- code_book(
      d,
      subtitle = "Wave 1",
      authors = authors,
      notes = c("Fictitious.", "- Second note."),
      output = path
    )
  )
  wb <- openxlsx2::wb_load(path)
  expect_identical(
    unname(openxlsx2::wb_get_sheet_names(wb)),
    c("codebook", "variables", "values")
  )

  info <- openxlsx2::read_xlsx(path, sheet = 1)
  expect_named(info, c("Field", "Value"))
  expect_identical(
    info$Field,
    c(
      "Title",
      "Subtitle",
      "Author",
      "Author",
      "Date",
      "Observations",
      "Variables",
      "Declared missing value",
      "Note",
      "Note",
      "Generated with"
    )
  )
  expect_identical(
    info$Value[2:4],
    c("Wave 1", "Jane Doe – HESAV – ORCID 0000-0002-1825-0097", "Bob")
  )
  # The notes as typed, list marker included.
  expect_identical(
    info$Value[8:10],
    c("8 = DK (1 variable)", "Fictitious.", "- Second note.")
  )
  expect_match(info$Value[[11]], "^spicy ")
  # Rows 7 and 8 of the sheet (the header is row 1) hold the two counts,
  # as numbers: written a row off, a text cell would turn the column to
  # character.
  counts <- openxlsx2::read_xlsx(
    path,
    sheet = 1,
    dims = "B7:B8",
    col_names = FALSE
  )
  expect_identical(counts[[1]], c(6, 8))

  vars <- openxlsx2::read_xlsx(path, sheet = 2)
  expect_identical(names(vars)[1:4], c("Pos.", "Variable", "Label", "Type"))
  expect_identical(vars[["Declared missing codes"]], c(rep(NA, 7), "8"))
  expect_equal(vars$Valid, cb$variables$n_valid)
  expect_equal(vars$Mean, cb$variables$mean)
  expect_identical(vars[["Earliest date"]][[7]], "2024-05-01")

  vals <- openxlsx2::read_xlsx(path, sheet = 3)
  expect_equal(vals$n, cb$values$n)
  expect_equal(vals[["Valid %"]], cb$values$pct_valid)
  expect_type(vals[["Declared missing"]], "logical")
  # A missing value leaves its cell empty, never an empty text.
  for (i in 2:3) {
    expect_false(any(wb$worksheets[[i]]$sheet_data$cc$is == "<is><t/></is>"))
  }

  props <- wb$get_properties()
  expect_identical(unname(props[["title"]]), "Codebook")
  expect_identical(unname(props[["creator"]]), "Jane Doe; Bob")
})

test_that("the Excel file leaves non-finite statistics empty, and means as they are", {
  skip_if_not_installed("openxlsx2")
  path <- withr::local_tempfile(fileext = ".xlsx")
  d <- data.frame(
    x = c(1, Inf, -Inf, NaN, 2),
    small = c(0.001, 0.002, 0.0025, 0.003, NA),
    f = factor(c("a", "b", "a", "a", "b"))
  )
  code_book(d, output = path)
  vars <- openxlsx2::read_xlsx(path, sheet = 2)
  # Min, max, mean and sd of x are -Inf, Inf, NaN and NaN: empty cells,
  # not error cells, in columns that stay numeric.
  expect_true(all(is.na(unlist(vars[1, c("Min", "Max", "Mean", "SD")]))))
  expect_type(vars$Mean, "double")
  expect_equal(vars$Median[[1]], 1.5)
  wb <- openxlsx2::wb_load(path)
  expect_false(any(wb$worksheets[[2]]$sheet_data$cc$c_t == "e"))
  # Only the percentages carry a number format: one of "0.00" would show
  # the mean of `small`, 0.0021, as 0.00.
  fmts <- wb$styles_mgr$styles$numFmts
  expect_true(any(grepl("formatCode=\"0.0\"", fmts, fixed = TRUE)))
  expect_false(any(grepl("formatCode=\"0.00\"", fmts, fixed = TRUE)))
})

test_that("the Excel header takes the PDF colors, and the font when given", {
  skip_if_not_installed("openxlsx2")
  path <- withr::local_tempfile(fileext = ".xlsx")
  colors <- c(band = "#112233", primary = "#FFEEDD")
  code_book(cb_data(), colors = colors, output = path)
  wb <- openxlsx2::wb_load(path)
  # The style cell A1 of `variables` carries: a light text on a dark band.
  styles <- wb$styles_mgr$styles
  a1 <- as.integer(openxlsx2::wb_get_cell_style(wb, 2, "A1"))
  xf <- openxlsx2::xml_attr(styles$cellXfs[[a1 + 1]], "xf")[[1]]
  fill <- styles$fills[[as.integer(xf[["fillId"]]) + 1]]
  font <- styles$fonts[[as.integer(xf[["fontId"]]) + 1]]
  expect_match(fill, "<fgColor rgb=\"FF112233\"/>", fixed = TRUE)
  expect_match(font, "<color rgb=\"FFFFEEDD\"/>", fixed = TRUE)
  default <- openxlsx2::wb_get_base_font(openxlsx2::wb_workbook())
  expect_identical(openxlsx2::wb_get_base_font(wb)$name, default$name)

  code_book(cb_data(), font = "Arial", output = path)
  wb <- openxlsx2::wb_load(path)
  expect_identical(openxlsx2::wb_get_base_font(wb)$name$val, "Arial")
  fonts <- wb$styles_mgr$styles$fonts
  expect_true(any(grepl("<b val=\"1\"/>", fonts) & grepl("Arial", fonts)))
})

test_that("the Excel codebook follows the language, with or without a title", {
  skip_if_not_installed("openxlsx2")
  path <- withr::local_tempfile(fileext = ".xlsx")
  withr::with_options(
    list(spicy.language = "fr"),
    code_book(data.frame(txt = c("a", "b")), title = NULL, output = path)
  )
  wb <- openxlsx2::wb_load(path)
  expect_identical(
    unname(openxlsx2::wb_get_sheet_names(wb)),
    c("codebook", "variables", "valeurs")
  )
  # No authors: no creator either, rather than the login of the session.
  props <- wb$get_properties()
  expect_identical(unname(props[c("creator", "modifier")]), c("", ""))
  info <- openxlsx2::read_xlsx(path, sheet = 1)
  expect_named(info, c("Champ", "Valeur"))
  expect_identical(info$Champ[[1]], "Date")
  vals <- openxlsx2::read_xlsx(path, sheet = 3)
  expect_identical(nrow(vals), 0L)
})
