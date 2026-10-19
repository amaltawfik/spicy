# Guards for the resolution layer of R/i18n.R -- `options(spicy.language)`,
# `options(spicy.labels)` and the language sets they select.
#
# Three properties, in order of what they protect:
#   1. the English default does not move by a byte, whether the option is
#      unset or set to "en";
#   2. a set is structurally sound -- known keys, sprintf-compatible
#      templates, and the invariants the recognizers depend on;
#   3. the frozen half of every couple stays English under a translation.

# ---- 1. The English default -----------------------------------------------

test_that("no option set resolves every key to the registry value", {
  expect_identical(
    vapply(names(.spicy_strings), spicy_str, character(1), USE.NAMES = FALSE),
    unname(.spicy_strings)
  )
})

test_that("language = 'en' is byte-identical to the option being unset", {
  # The whole registry, not a sample: "en" has no table of its own
  # precisely so that it cannot drift from the default, and this is the
  # test that would see it if one were ever added.
  unset <- vapply(names(.spicy_strings), spicy_str, character(1))
  withr::local_options(spicy.language = "en")
  expect_identical(
    vapply(names(.spicy_strings), spicy_str, character(1)),
    unset
  )
})

test_that("a rendered table is byte-identical under 'en' and under no option", {
  d <- data.frame(
    x = factor(c("Yes", "No", "Yes", NA)),
    g = factor(c("A", "A", "B", "B"))
  )
  plain <- capture.output(print(freq(d$x)))
  withr::local_options(spicy.language = "en")
  expect_identical(capture.output(print(freq(d$x))), plain)
})


# ---- 2. Structure of every shipped set ------------------------------------

test_that("every shipped set names only keys the registry has", {
  for (lang in .SPICY_LANGUAGES) {
    set <- .spicy_language_table(lang)
    if (is.null(set)) {
      next
    }
    expect_type(set, "character")
    expect_false(anyDuplicated(names(set)) > 0L, label = lang)
    expect_identical(
      setdiff(names(set), names(.spicy_strings)),
      character(0),
      label = lang
    )
    expect_false(anyNA(set), label = lang)
  }
})

test_that("every shipped locale names only levers a style knows", {
  # A locale is a style-lever list, so it has to speak the same
  # vocabulary; a lever the format layer does not know would be a
  # silent no-op in exactly the place a silent no-op is worst.
  for (lang in .SPICY_LANGUAGES) {
    loc <- .spicy_locale_table(lang)
    if (is.null(loc)) {
      next
    }
    expect_type(loc, "list")
    expect_true(length(loc) >= 1L, label = lang)
    expect_identical(
      setdiff(names(loc), spicy:::.STYLE_FIELDS),
      character(0),
      label = lang
    )
    # And it must survive the constructor's own validation.
    expect_s3_class(do.call(spicy_style, loc), "spicy_style")
  }
})

test_that("English carries no locale", {
  # "en" is the fallback in both layers: spicy's defaults ARE its
  # typography, and a second copy could only drift from them.
  expect_null(.spicy_locale_table("en"))
})

test_that("every translated template takes its English arguments", {
  # The holes are the CONTRACT between a template and its call site: the
  # call site passes what English asked for, so a translation that drops
  # a hole, adds one, or changes a %d to a %s raises at render time --
  # in one language only, on a code path a snapshot never sees.
  # `.spicy_holes_compatible()` is the package's own check, the one
  # `options(spicy.labels)` is validated with. Reimplementing it here
  # would leave the shipped sets and the user's overrides held to two
  # separate standards, and only one of them would be the strict one.
  for (lang in .SPICY_LANGUAGES) {
    set <- .spicy_language_table(lang)
    if (is.null(set)) {
      next
    }
    for (k in names(set)) {
      expect_true(
        .spicy_holes_compatible(
          unname(set[[k]]),
          unname(.spicy_strings[[k]])
        ),
        info = sprintf("%s / %s", lang, k)
      )
    }
  }
})

test_that("the hole check reads every way a template can go wrong", {
  ok <- .spicy_holes_compatible
  # Same holes: fine, in either order when they are positional.
  expect_true(ok("%s removed: %d.", "Rows with missing %s removed: %d."))
  expect_true(ok("%2$s then %1$s", "%1$s then %2$s"))
  # A dropped hole loses a number the table meant to show.
  expect_false(ok("Rows removed.", "Rows with missing %s removed: %d."))
  expect_false(ok("only %s", "%s and %d"))
  # An added hole, or one past the arity, raises at render time.
  expect_false(ok("a %s", "a plain label"))
  expect_false(ok("%1$s %2$s %3$s", "%1$s %2$s"))
  # A retyped conversion: `%d` handed the string the call site passes.
  expect_false(ok("%d values", "%s values"))
  # A literal percent is not a hole, on either side.
  expect_true(ok(", IC 95 % [", ", 95% CI ["))
})

test_that("the emphasized note prefix is a prefix of the note prefix, in every language", {
  # Asserted at the English default in test-i18n.R. Six rich engines
  # italicize the first and print the rest in regular type; a set that
  # translates one and not the other breaks all six at once, silently.
  for (lang in .SPICY_LANGUAGES) {
    withr::local_options(spicy.language = lang)
    expect_true(
      startsWith(spicy_str("note_prefix"), spicy_str("note_prefix_emphasis")),
      label = lang
    )
  }
})

test_that("the labels a recognizer alternates over stay distinct in every language", {
  # `.companion_header_pattern()` matches a column's header against SE /
  # p / the interval spanner to name an orphaned companion column, and
  # the exponentiated-coefficient footer looks its gloss up BY header.
  # Both are sound as long as the labels they alternate over are
  # distinct: two keys that collapse to one string in a translation make
  # one of them unreachable, with no condition raised.
  # `header_effect_size_short` is not one of them: it heads a column of
  # `table_continuous()`, which shows no standard error and never reaches
  # the recognizer (and its French "Taille d'effet" is distinct from "ES"
  # anyway).
  distinct <- list(
    companion = c(
      "header_se",
      "header_p",
      "header_ci_label_confidence",
      "header_ci_label_credible"
    ),
    exp = c(
      "header_exp_or",
      "header_exp_irr",
      "header_exp_hr",
      "header_exp_rr",
      "header_exp_mr",
      "header_exp_tr",
      "header_exp_generic"
    )
  )
  for (lang in .SPICY_LANGUAGES) {
    withr::local_options(spicy.language = lang)
    for (grp in names(distinct)) {
      labels <- vapply(distinct[[grp]], spicy_str, character(1))
      expect_false(
        anyDuplicated(labels) > 0L,
        label = paste(lang, grp)
      )
    }
  }
})

test_that("translated Excel sheet names stay legal worksheet names", {
  # openxlsx2 writes them verbatim: at most 31 characters, and none of
  # the six characters Excel reserves. A translator has no other guard.
  keys <- grep("^excel_sheet_", names(.spicy_strings), value = TRUE)
  expect_gt(length(keys), 0L)
  for (lang in .SPICY_LANGUAGES) {
    withr::local_options(spicy.language = lang)
    for (k in keys) {
      nm <- spicy_str(k)
      expect_lte(nchar(nm), 31L, label = paste(lang, k))
      expect_false(grepl("[]:\\\\/?*[]", nm), label = paste(lang, k))
    }
  }
})


# ---- 3. Resolution order, fallback, validation ----------------------------

test_that("a key absent from a set falls back to English", {
  absent <- setdiff(names(.spicy_strings), names(.spicy_strings_fr))
  expect_gt(length(absent), 0L)
  withr::local_options(spicy.language = "fr")
  for (k in head(absent, 20L)) {
    expect_identical(spicy_str(k), unname(.spicy_strings[[k]]), info = k)
  }
  # Nothing comes back blank or missing, over the whole registry.
  all_fr <- vapply(names(.spicy_strings), spicy_str, character(1))
  expect_false(anyNA(all_fr))
  expect_true(all(nzchar(all_fr)))
})

test_that("labels beat the language set, which beats English", {
  k <- "row_missing_level"
  expect_identical(spicy_str(k), "(Missing)")
  withr::local_options(spicy.language = "fr")
  expect_identical(spicy_str(k), "(Manquant)")
  withr::local_options(spicy.labels = list(row_missing_level = "(Sans objet)"))
  expect_identical(spicy_str(k), "(Sans objet)")
})

test_that("an override applies with no language set", {
  withr::local_options(spicy.labels = list(header_mean = "Mean"))
  expect_identical(spicy_str("header_mean"), "Mean")
  # And it leaves every other key alone.
  expect_identical(spicy_str("header_sd"), "SD")
})

test_that("an override is accepted as a named character vector too", {
  withr::local_options(spicy.labels = c(header_mean = "Mean"))
  expect_identical(spicy_str("header_mean"), "Mean")
})

test_that("an empty override is a no-op", {
  withr::local_options(spicy.labels = list())
  expect_identical(spicy_str("header_mean"), "M")
  withr::local_options(spicy.labels = character(0))
  expect_identical(spicy_str("header_mean"), "M")
})

test_that("an unknown language is a classed error naming the sets that exist", {
  withr::local_options(spicy.language = "de")
  expect_error(spicy_str("header_mean"), class = "spicy_invalid_input")
  expect_error(spicy_str("header_mean"), "\"en\"")
  expect_error(spicy_str("header_mean"), "\"fr\"")
})

test_that("a malformed language option is a classed error", {
  for (bad in list(1L, c("en", "fr"), NA_character_, TRUE)) {
    withr::local_options(spicy.language = bad)
    expect_error(spicy_str("header_mean"), class = "spicy_invalid_input")
  }
})

test_that("an override naming an unknown key is a classed error", {
  withr::local_options(spicy.labels = list(no_such_label = "x"))
  expect_error(spicy_str("header_mean"), class = "spicy_invalid_input")
  expect_error(spicy_str("header_mean"), "no_such_label")
})

test_that("an override that would not render is a classed error", {
  # Caught at option-read time, not inside the next table call: the
  # message can still name the key and quote the default it must match.
  withr::local_options(
    spicy.labels = list(note_rows_missing_by_removed = "Rows removed.")
  )
  expect_error(spicy_str("header_mean"), class = "spicy_invalid_input")
  expect_error(spicy_str("header_mean"), "note_rows_missing_by_removed")

  withr::local_options(spicy.labels = list(header_mean = "Mean of %s"))
  expect_error(spicy_str("header_mean"), class = "spicy_invalid_input")

  withr::local_options(
    spicy.labels = list(note_missing_item = "%d (%s)")
  )
  expect_error(spicy_str("header_mean"), class = "spicy_invalid_input")

  # A correct override of a template is accepted and renders.
  withr::local_options(
    spicy.labels = list(note_rows_missing_by_removed = "Dropped %s: %d.")
  )
  expect_identical(
    spicy_fmt("note_rows_missing_by_removed", "age", 3L),
    "Dropped age: 3."
  )
})

test_that("a malformed override is a classed error", {
  bads <- list(
    list(header_mean = 1L),
    list(header_mean = c("a", "b")),
    list(header_mean = NA_character_),
    list("unnamed"),
    c(header_mean = NA_character_),
    stats::setNames(list("a", "b"), c("header_mean", "header_mean")),
    42
  )
  for (bad in bads) {
    withr::local_options(spicy.labels = bad)
    expect_error(spicy_str("header_mean"), class = "spicy_invalid_input")
  }
})

test_that("nothing cached survives a change to the override option", {
  # The validated table is cached against the option VALUE. A cache keyed
  # on anything else -- a flag, the session -- would keep serving the
  # first labels a session ever set.
  withr::local_options(spicy.labels = list(header_mean = "First"))
  expect_identical(spicy_str("header_mean"), "First")
  options(spicy.labels = list(header_mean = "Second"))
  expect_identical(spicy_str("header_mean"), "Second")
  options(spicy.labels = list(header_sd = "Other"))
  expect_identical(spicy_str("header_mean"), "M")
  expect_identical(spicy_str("header_sd"), "Other")
  options(spicy.labels = NULL)
  expect_identical(spicy_str("header_mean"), "M")
})

test_that("an unknown key still raises, in every language", {
  for (lang in .SPICY_LANGUAGES) {
    withr::local_options(spicy.language = lang)
    expect_error(spicy_str("no_such_key_exists"))
  }
})


# ---- 4. spicy_labels() ----------------------------------------------------

test_that("spicy_labels() reports the labels in force", {
  expect_identical(spicy_labels(), .spicy_strings)
  withr::local_options(spicy.language = "fr")
  fr <- spicy_labels()
  expect_identical(names(fr), names(.spicy_strings))
  expect_identical(fr[["row_missing_level"]], "(Manquant)")
  # A key the set does not carry reports the English it falls back to.
  expect_identical(fr[["label_total"]], "Total")
})

test_that("spicy_labels(language) answers for that language", {
  expect_identical(spicy_labels("en"), .spicy_strings)
  expect_identical(spicy_labels("fr")[["header_sd"]], "ET")
  # The override is a separate layer and applies either way.
  withr::local_options(spicy.labels = list(header_sd = "s"))
  expect_identical(spicy_labels("fr")[["header_sd"]], "s")
  expect_identical(spicy_labels("en")[["header_sd"]], "s")
})

test_that("spicy_labels() rejects an unknown language", {
  expect_error(spicy_labels("de"), class = "spicy_invalid_input")
})


# ---- 5. The frozen half stays English under a translation -----------------

test_that("a translation moves no public column name", {
  d <- data.frame(
    y = c(1.2, 2.4, 3.1, 0.8, 2.2, 1.9),
    x = factor(c("Yes", "No", "Yes", "No", "Yes", "No")),
    g = factor(c("A", "A", "B", "B", "A", "B"))
  )
  fit <- stats::lm(y ~ x + g, data = d)
  frames <- function() {
    list(
      cat = names(as.data.frame(table_categorical(d, select = x, by = g))),
      con = names(as.data.frame(table_continuous(d, select = y, by = g))),
      lm = names(as.data.frame(table_continuous_lm(d, select = y, by = g))),
      reg = names(as.data.frame(table_regression(fit))),
      out = names(as.data.frame(table_outcome(
        d,
        outcome = y,
        select = c(x, g)
      ))),
      structured = names(as_structured(table_regression(fit)))
    )
  }
  en <- frames()
  withr::local_options(spicy.language = "fr")
  expect_identical(frames(), en)
})

test_that("a regression table translates its headers, not its column names", {
  skip_if_not_installed("tinytable")
  fit <- stats::lm(wellbeing_score ~ age + sex, data = sochealth)
  uv <- function() {
    table_regression_uv(
      sochealth,
      outcome = wellbeing_score,
      predictors = c(age, bmi),
      method = "lm",
      show_columns = c("n", "b", "ci", "p", "adj_r2")
    )
  }
  en <- table_regression(fit)
  en_uv <- uv()
  withr::local_options(spicy.language = "fr", width = 120)
  fr <- table_regression(fit)
  s <- as_structured(fr)
  ic <- "IC \u00E0 95\u00A0%"

  expect_identical(names(as.data.frame(fr)), names(as.data.frame(en)))
  expect_identical(names(s$body), names(as_structured(en)$body))
  expect_identical(names(s$col_meta), names(as_structured(en)$col_meta))
  expect_identical(
    unname(vapply(s$col_meta, `[[`, character(1), "display_label")),
    c("B", "ES", ic, ic, "p")
  )
  expect_identical(vapply(s$ci_pairs, `[[`, character(1), "label"), ic)

  # The console prints an interval in one cell; the engines that split it
  # name the bounds.
  console <- paste(capture.output(print(fr)), collapse = "\n")
  clip <- clipboard_payload(fr, "\t")
  md <- tinytable::save_tt(
    table_regression(fit, output = "tinytable"),
    output = "markdown"
  )
  for (txt in c("ES", ic, "R\u00B2 ajust\u00E9")) {
    expect_true(grepl(txt, console, fixed = TRUE), info = txt)
  }
  expect_true(grepl("\tLI\tLS\t", clip, fixed = TRUE))
  for (txt in c("ES", ic, "LI", "LS")) {
    expect_true(grepl(txt, md, fixed = TRUE), info = txt)
  }
  for (txt in c("SE", "95% CI", "LL", "UL", "Adj. R")) {
    expect_false(grepl(txt, console, fixed = TRUE), info = txt)
    expect_false(grepl(txt, clip, fixed = TRUE), info = txt)
    expect_false(grepl(txt, md, fixed = TRUE), info = txt)
  }

  # The univariable screen, where the adjusted R2 is a column.
  fr_uv <- uv()
  expect_identical(names(as.data.frame(fr_uv)), names(as.data.frame(en_uv)))
  expect_identical(
    as_structured(fr_uv)$col_meta[["Univariable: Adj. R\u00B2"]]$display_label,
    "R\u00B2 ajust\u00E9"
  )
})

test_that("the credible interval header follows the language, its name does not", {
  spec <- function(key) {
    spicy:::build_column_spec(
      c("b", "ci"),
      "M1",
      stats::setNames("", "M1"),
      ci_label_key = key
    )[[2L]]
  }
  withr::local_options(spicy.language = "fr")
  expect_identical(
    spec("header_ci_label_credible")[c("col_name", "display_label")],
    list(col_name = "95% CrI", display_label = "ICr \u00E0 95\u00A0%")
  )
  # HDI has no established French abbreviation and stays English.
  expect_identical(
    spec("header_ci_label_hdi")$display_label,
    "HDI \u00E0 95\u00A0%"
  )
})

test_that("a descriptive interval names its bounds in the language", {
  en <- table_continuous(sochealth, select = bmi, by = sex, ci = TRUE)
  withr::local_options(spicy.language = "fr", width = 160)
  fr <- table_continuous(sochealth, select = bmi, by = sex, ci = TRUE)
  expect_identical(names(as.data.frame(fr)), names(as.data.frame(en)))
  out <- paste(capture.output(print(fr)), collapse = "\n")
  expect_true(grepl("IC \u00E0 95\u00A0% LI", out, fixed = TRUE))
  expect_true(grepl("IC \u00E0 95\u00A0% LS", out, fixed = TRUE))
  expect_false(grepl("95% CI", out, fixed = TRUE))
})

test_that("the association interval of a categorical table is labeled in the language", {
  s_of <- function() {
    as_structured(table_categorical(
      sochealth,
      select = smoking,
      by = education,
      assoc_ci = TRUE
    ))
  }
  lab_of <- \(s) unique(unlist(lapply(s$col_meta, `[[`, "ci_label")))
  en <- s_of()
  expect_identical(lab_of(en), "95% CI")
  withr::local_options(spicy.language = "fr")
  fr <- s_of()
  ic <- "IC \u00E0 95\u00A0%"
  expect_identical(lab_of(fr), ic)
  expect_identical(vapply(fr$ci_pairs, `[[`, character(1), "label"), ic)
  expect_identical(names(fr$body), names(en$body))
})

test_that("the French effect-size header is the full word", {
  en <- table_continuous(sochealth, select = bmi, by = sex, effect_size = TRUE)
  withr::local_options(spicy.language = "fr", width = 160)
  fr <- table_continuous(sochealth, select = bmi, by = sex, effect_size = TRUE)
  expect_identical(names(as.data.frame(fr)), names(as.data.frame(en)))
  out <- capture.output(print(fr))
  header <- out[grepl("Variable", out, fixed = TRUE)]
  expect_match(header, "Taille d'effet", fixed = TRUE)
})

test_that("the read-through headers and row labels follow the language", {
  skip_if_not_installed("survival")
  withr::local_options(spicy.language = "fr")
  cat_meta <- as_structured(
    table_categorical(sochealth, select = smoking, by = sex, assoc_ci = TRUE)
  )$col_meta
  bounds <- Filter(function(m) !is.null(m$ci_role), cat_meta)
  expect_identical(
    unname(vapply(bounds, `[[`, character(1), "display_label")),
    c("IC inf.", "IC sup.")
  )
  con <- table_continuous(sochealth, select = bmi, show_columns = c("med", "n"))
  expect_identical(
    as_structured(con)$col_meta[["Med"]]$display_label,
    "M\u00E9d."
  )
  fit <- stats::glm(
    smoking ~ age + sex,
    data = sochealth,
    family = stats::binomial()
  )
  reg <- as_structured(
    table_regression(fit, show_columns = c("n_events", "b", "p"))
  )
  expect_identical(
    reg$col_meta[["Events/N"]]$display_label,
    "\u00C9v\u00E9nements/N"
  )
  cox <- survival::coxph(
    survival::Surv(time, status) ~ age,
    data = survival::lung
  )
  rows <- trimws(
    as.data.frame(
      table_regression(cox, show_fit_stats = c("nobs", "n_events"))
    )$Variable
  )
  expect_true("Nombre d'\u00E9v\u00E9nements" %in% rows)
})

test_that("a data level keeps its own spelling in a column name", {
  # The task the frozen-key doctrine exists for: `by` levels are DATA.
  # A level named "Yes" gives the column "Yes %" in every language --
  # spicy translates its own vocabulary, never the user's values.
  d <- data.frame(
    sex = factor(c("F", "M", "F", "M")),
    consent = factor(c("Yes", "No", "Yes", "Yes"))
  )
  withr::local_options(spicy.language = "fr")
  nms <- names(as.data.frame(table_categorical(d, select = sex, by = consent)))
  expect_true("Yes %" %in% nms)
  expect_true("Yes n" %in% nms)
  expect_false(any(grepl("Oui", nms)))
})

test_that("the frozen tokens and glyphs do not translate", {
  withr::local_options(spicy.language = "fr")
  frozen <- c(
    symbol_t = "t",
    symbol_z = "z",
    symbol_f = "F",
    symbol_chi_sq = "\u03C7\u00B2",
    symbol_beta = "\u03B2",
    symbol_delta = "\u0394",
    symbol_r2 = "R\u00B2",
    cell_undefined = "\u2013",
    marker_na = "<NA>",
    marker_nan = "<NaN>",
    symbol_star_001 = "***",
    symbol_star_05 = "*",
    header_variable = "Variable",
    header_p = "p",
    header_b = "B",
    header_n_lower = "n",
    header_exp_or = "OR",
    header_exp_hr = "HR",
    label_total = "Total",
    header_margin_total = "Total",
    label_model_name = "Model %d",
    note_prefix = "Note. ",
    note_prefix_emphasis = "Note."
  )
  for (k in names(frozen)) {
    expect_identical(spicy_str(k), unname(frozen[[k]]), info = k)
  }
})

test_that("a cross-reference matches the thing it points at", {
  withr::local_options(spicy.language = "fr")
  # The spanner over a model's columns is `label_model_name`, frozen
  # English because it becomes a column name. A footnote that cites a
  # model by index has to spell it the way the spanner does, or the
  # reader is sent to a "Modele 1" no column carries.
  expect_identical(spicy_fmt("label_model_name", 1L), "Model 1")
  expect_true(startsWith(
    spicy_fmt("note_ci_posterior_mixed", 1L, "95"),
    "Model 1"
  ))
})

test_that("a condition message is English whatever the language", {
  # The registry excludes conditions on purpose. `note_model_prefix`
  # reads like a table note but is the lead line of a `spicy_abort()`,
  # so translating it would put a French head on an English body.
  withr::local_options(spicy.language = "fr")
  expect_identical(spicy_str("note_model_prefix"), "Model %d: %s")
})

test_that("the block identity stays English while its caption translates", {
  withr::local_options(spicy.language = "fr")
  # `.REG_BLOCK_TERMS` is what nine sites match on and what `tidy()`
  # publishes; the caption is what the reader sees. Under a translation
  # they must part company, which is exactly what the English-default
  # test in test-i18n.R cannot see.
  expect_identical(.reg_block_label("Random effects"), "Effets al\u00E9atoires")
  expect_true("Random effects" %in% .REG_BLOCK_TERMS)
  # The mlogit section takes the same split without being a block.
  expect_identical(
    .reg_block_label(.REG_SECTION_ALT_INVARIANT),
    "Invariant entre alternatives"
  )
  expect_false(.reg_is_block(.REG_SECTION_ALT_INVARIANT))
  # The fixed-effects token the typed body encodes back to 1 / 0.
  expect_identical(.REG_FE_YES, "Yes")
  expect_identical(.reg_fe_cell_label(.REG_FE_YES), "Oui")
})

test_that("inline() still addresses a variable by its translated label", {
  d <- data.frame(
    y = c(1.2, 2.4, 3.1, 0.8, 2.2, 1.9, 3.3, 1.1),
    g = factor(c("A", "A", "B", "B", "A", "B", "A", "B"))
  )
  tr <- table_regression(stats::lm(y ~ g, data = d))
  en <- inline(tr, "g", level = "B")
  withr::local_options(spicy.language = "fr")
  # The block header now reads "g :" with a no-break space; addressing
  # by the displayed label must survive it.
  expect_identical(inline(tr, "g", level = "B"), en)
})


# ---- 5b. The locale reaches the exploration pair --------------------------

test_that("a French language writes the whole pair under the comma", {
  # Decision 44: the language sets the DEFAULT of `decimal_mark` in
  # `freq()` and `cross_tab()` too, so one option gives a coherent
  # French document. Every number the pair renders follows -- cells,
  # the chi-squared statistic, the p-value, the association estimate
  # and its interval, and the table note.
  d <- data.frame(
    sex = factor(c("F", "M", "F", "M", "F", "M", "F", "M")),
    smoke = factor(c("Yes", "No", "No", "Yes", "No", "No", "Yes", NA))
  )
  withr::local_options(spicy.language = "fr", width = 100)
  fq <- capture.output(print(freq(d$smoke)))
  ct <- capture.output(print(cross_tab(
    d,
    smoke,
    sex,
    percent = "column",
    assoc_ci = TRUE
  )))
  expect_false(any(grepl("[0-9]\\.[0-9]", c(fq, ct))))
  expect_true(any(grepl("50,0", fq, fixed = TRUE)))
  expect_true(any(grepl("Khi-2(1) = 0,2", ct, fixed = TRUE)))
  # The interval separator switches with the mark, as everywhere else.
  expect_true(any(grepl("[0,00; 0,82]", ct, fixed = TRUE)))
})

test_that("the French p-value of the pair keeps its leading zero", {
  # ",659" is the form the SI brochure forbids; the mark carries the
  # rule here, since the pair has no `p_style` lever of its own.
  d <- data.frame(
    sex = factor(c("F", "M", "F", "M", "F", "M", "F", "M")),
    smoke = factor(c("Yes", "No", "No", "Yes", "No", "No", "Yes", NA))
  )
  withr::local_options(spicy.language = "fr")
  note <- attr(cross_tab(d, smoke, sex), "note")
  expect_match(note, "p\u00A0=\u00A00,659", fixed = TRUE)
  expect_false(grepl("p\u00A0=\u00A0,", note, fixed = TRUE))
})

test_that("the pair's mark is frozen at build, not at print", {
  # What ?freq and ?cross_tab now promise: the resolved mark rides on
  # the object like every other formatting argument, so a table built
  # under a French language keeps its commas when the option is gone by
  # the time it prints.
  d <- data.frame(
    sex = factor(c("F", "M", "F", "M", "F", "M", "F", "M")),
    smoke = factor(c("Yes", "No", "No", "Yes", "No", "No", "Yes", NA))
  )
  built <- withr::with_options(
    list(spicy.language = "fr"),
    list(
      fq = freq(d$smoke),
      ct = cross_tab(d, smoke, sex, percent = "column")
    )
  )
  withr::local_options(spicy.language = NULL)
  fq <- capture.output(print(built$fq))
  ct <- capture.output(print(built$ct))
  expect_true(any(grepl("50,0", fq, fixed = TRUE)))
  expect_false(any(grepl("[0-9]\\.[0-9]", c(fq, ct))))
})

test_that("an argument beats the language in the pair too", {
  d <- data.frame(
    sex = factor(c("F", "M", "F", "M", "F", "M", "F", "M")),
    smoke = factor(c("Yes", "No", "No", "Yes", "No", "No", "Yes", NA))
  )
  en_fq <- capture.output(print(freq(d$smoke, decimal_mark = ".")))
  en_ct <- capture.output(print(cross_tab(
    d,
    smoke,
    sex,
    percent = "column",
    decimal_mark = "."
  )))
  withr::local_options(spicy.language = "fr")
  fr_fq <- capture.output(print(freq(d$smoke, decimal_mark = ".")))
  fr_ct <- capture.output(print(cross_tab(
    d,
    smoke,
    sex,
    percent = "column",
    decimal_mark = "."
  )))
  # Words translate, numbers do not: the escape hatch for a bilingual
  # document. The p-value keeps the point form as well.
  expect_false(any(grepl("[0-9],[0-9]", c(fr_fq, fr_ct))))
  expect_true(any(grepl("p\u00A0=\u00A0.659", fr_ct, fixed = TRUE)))
  # And the numbers themselves are the ones English printed -- the
  # words around them are the only difference.
  numbers <- function(x) {
    unlist(regmatches(x, gregexpr("[0-9]+[.,]?[0-9]*", x)))
  }
  expect_identical(numbers(fr_fq), numbers(en_fq))
  expect_identical(numbers(fr_ct), numbers(en_ct))
  expect_true(any(grepl("p = .659", en_ct, fixed = TRUE)))
})

test_that("the pair's internal callers are untouched by the locale", {
  # `table_categorical()` builds its rows from `freq()` and
  # `cross_tab()` and reads their NUMERIC output, never their rendered
  # text. Those inner calls name no `decimal_mark`, so under a French
  # language they now take the comma -- which must not reach a value.
  # An explicit `decimal_mark = "."` on the outer call is the sharpest
  # form of the question: French words, English numbers, correct ones.
  d <- data.frame(
    sex = factor(c("F", "M", "F", "M", "F", "M", "F", "M")),
    smoke = factor(c("Yes", "No", "No", "Yes", "No", "No", "Yes", "Yes"))
  )
  en <- as.data.frame(table_categorical(d, select = smoke, by = sex))
  withr::local_options(spicy.language = "fr")
  fr <- as.data.frame(table_categorical(
    d,
    select = smoke,
    by = sex,
    decimal_mark = "."
  ))
  expect_identical(unname(as.matrix(fr)), unname(as.matrix(en)))
})

# ---- 6. The French corpus, pinned -----------------------------------------

test_that("a French table of each family is pinned", {
  withr::local_options(spicy.language = "fr", width = 100)
  d <- data.frame(
    bmi = c(22.1, 25.4, 27.8, 24.2, 31.0, 19.8, 26.6, 23.3),
    age = c(31, 45, 52, 38, 61, 24, 49, 35),
    sex = factor(c("F", "M", "F", "M", "F", "M", "F", "M")),
    smoke = factor(c("Yes", "No", "No", "Yes", "No", "No", "Yes", NA))
  )
  expect_snapshot({
    print(freq(d$smoke))
    print(cross_tab(d, smoke, sex, percent = "column"))
    print(table_categorical(d, select = smoke, by = sex))
    print(table_continuous(d, select = c(bmi, age), by = sex))
    print(table_continuous_lm(d, select = bmi, by = sex))
    print(table_outcome(d, outcome = bmi, select = c(sex, smoke)))
    print(table_regression(stats::lm(bmi ~ age + sex, data = d)))
  })
})

test_that("a single overridden label is pinned", {
  # The case the override layer exists for: one label has to change and
  # a language does not. The missing CATEGORY of a grouping variable is
  # a refusal to answer in this questionnaire, not a missing datum.
  withr::local_options(
    spicy.labels = list(
      row_missing_level = "(No answer)",
      header_margin_total = "All"
    ),
    width = 100
  )
  d <- data.frame(
    sex = factor(c("F", "M", "F", "M", "F", "M")),
    arm = factor(c("Control", "Campus", NA, "Campus", "Control", NA))
  )
  expect_snapshot(print(table_categorical(d, select = sex, by = arm)))
})

test_that("the p_adjust sentence of the regression follows the language", {
  frame <- list(
    coefs = data.frame(
      term = c("(Intercept)", "x1", "x2"),
      estimate_type = "B",
      is_ref = FALSE,
      p_value = 0.04
    )
  )
  en <- spicy:::build_p_adjust_footer_block_from_frames(list(frame), "holm")
  expect_identical(
    en,
    'P-values adjusted via stats::p.adjust(method = "holm"); m = 2 coefficient(s) per model.'
  )
  withr::local_options(spicy.language = "fr")
  fr <- spicy:::build_p_adjust_footer_block_from_frames(list(frame), "holm")
  expect_identical(
    fr,
    paste0(
      "Valeurs p ajust\u00E9es par stats::p.adjust(method = \"holm\")\u00A0; ",
      "m = 2 coefficient(s) par mod\u00E8le."
    )
  )
})

test_that("the robust-SE and mixed p-value lines follow the language", {
  # The non-lm/glm classes build their robust label in the frame and the
  # mixed engines add a p-value line; both read the registry, through the
  # keys of the lm/glm footer for the former. English stays byte-identical.
  skip_if_not_installed("glmmTMB", "1.1.15")
  skip_if_not_installed("lme4")
  skip_if_not_installed("clubSandwich")
  set.seed(1)
  d <- data.frame(site = factor(rep(1:12, each = 10)), x = stats::rnorm(120))
  d$y <- stats::rpois(
    120,
    exp(0.3 + 0.3 * d$x + stats::rnorm(12, sd = 0.4)[d$site])
  )
  d$yg <- 1 + 0.5 * d$x + stats::rnorm(12)[d$site] + stats::rnorm(120)
  fit_tmb <- glmmTMB::glmmTMB(y ~ x + (1 | site), data = d, family = poisson)
  fit_lmer <- lme4::lmer(yg ~ x + (1 | site), data = d)
  fit_lm <- stats::lm(yg ~ x, data = d)
  notes <- function() {
    list(
      tmb = attr(table_regression(fit_tmb, vcov = "CR0"), "note"),
      lmer = attr(
        table_regression(fit_lmer, vcov = "CR2", cluster = ~site),
        "note"
      ),
      lm = attr(table_regression(fit_lm, vcov = "CR0", cluster = ~site), "note")
    )
  }
  se_line <- function(note) {
    lines <- strsplit(note, "\n", fixed = TRUE)[[1L]]
    lines[grepl("^(Std. errors|Erreurs standards)", lines)]
  }
  en <- notes()
  expect_identical(
    se_line(en$tmb),
    "Std. errors: cluster-robust (CR0), clusters by site."
  )
  expect_match(en$tmb, "p-values: Wald-z asymptotic (glmmTMB).", fixed = TRUE)
  expect_match(
    en$lmer,
    "p-values: Satterthwaite t-test, cluster-robust df (clubSandwich).",
    fixed = TRUE
  )
  expect_identical(
    withr::with_options(list(spicy.language = "en"), notes()),
    en
  )

  withr::local_options(spicy.language = "fr")
  fr <- notes()
  # A mixed engine says it in the words of the lm footer.
  expect_identical(se_line(fr$tmb), se_line(fr$lm))
  expect_identical(
    se_line(fr$tmb),
    "Erreurs standards\u00A0: robustes aux grappes (CR0), grappes d\u00E9finies par site."
  )
  expect_match(
    fr$tmb,
    "Valeurs p\u00A0: z de Wald asymptotique (glmmTMB).",
    fixed = TRUE
  )
  expect_match(
    fr$lmer,
    paste0(
      "Valeurs p\u00A0: test t de Satterthwaite, degr\u00E9s de libert\u00E9 ",
      "robustes aux grappes (clubSandwich)."
    ),
    fixed = TRUE
  )
  expect_false(any(grepl("cluster-robust|p-values", unlist(fr))))
})

test_that("the random-effects LR test line follows the language", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("glmmTMB")
  skip_if_not_installed("ordinal")
  cbpp <- lme4::cbpp
  f_binom <- cbind(incidence, size - incidence) ~ period + (1 | herd)
  fits <- list(
    lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy),
    lme4::glmer(f_binom, cbpp, family = binomial),
    lme4::glmer(f_binom, cbpp, family = binomial(link = "probit")),
    lme4::glmer(incidence ~ period + (1 | herd), cbpp, family = poisson),
    glmmTMB::glmmTMB(incidence ~ period + (1 | herd), cbpp, family = poisson),
    glmmTMB::glmmTMB(f_binom, cbpp, family = binomial),
    glmmTMB::glmmTMB(incidence ~ period + (1 | herd), cbpp, glmmTMB::nbinom2),
    ordinal::clmm(rating ~ temp + (1 | judge), data = ordinal::wine)
  )
  frames <- lapply(fits, as_regression_frame)
  lines <- function(mark) {
    vapply(
      frames,
      spicy:::.format_random_effects_for_frame,
      character(1),
      decimal_mark = mark
    )
  }
  en <- c(
    "linear regression",
    "logistic regression",
    "probit regression",
    "poisson regression",
    "poisson regression",
    "binomial regression",
    "nbinom2 regression",
    "cumulative logit regression"
  )
  fr <- paste(
    "r\u00E9gression",
    c(
      "lin\u00E9aire",
      "logistique",
      "probit",
      "de Poisson",
      "de Poisson",
      "binomiale",
      "nbinom2",
      "\u00E0 logit cumulatif"
    )
  )
  got_en <- lines(".")
  for (i in seq_along(en)) {
    expect_match(got_en[i], paste0(": LR test vs ", en[i], ", "), fixed = TRUE)
  }
  withr::local_options(spicy.language = "fr")
  got_fr <- lines(",")
  for (i in seq_along(fr)) {
    expect_match(
      got_fr[i],
      paste0(
        "\u00A0: test du rapport de vraisemblance contre la ",
        fr[i],
        ", "
      ),
      fixed = TRUE
    )
  }
  expect_identical(
    got_fr[1L],
    paste0(
      "Effets al\u00E9atoires (REML)\u00A0: test du rapport de ",
      "vraisemblance contre la r\u00E9gression lin\u00E9aire, ",
      "\u03C7\u0304\u00B2(1) = 107,20, p < 0,001."
    )
  )
})

test_that("every glmer family names its null model in the language", {
  skip_if_not_installed("lme4")
  cbpp <- lme4::cbpp
  f_binom <- cbind(incidence, size - incidence) ~ period + (1 | herd)
  fits <- list(
    lme4::glmer(f_binom, cbpp, family = binomial(link = "cloglog")),
    suppressWarnings(lme4::glmer(f_binom, cbpp, binomial(link = "log"))),
    lme4::glmer(f_binom, cbpp, family = binomial(link = "cauchit")),
    suppressWarnings(lme4::glmer(
      Reaction ~ Days + (1 | Subject),
      lme4::sleepstudy,
      family = inverse.gaussian(link = "log")
    )),
    suppressWarnings(lme4::glmer.nb(
      TICKS ~ YEAR + (1 | BROOD),
      lme4::grouseticks
    ))
  )
  # The producer and the footer line, without the rest of the frame.
  lines <- function(mark) {
    vapply(
      fits,
      function(fit) {
        spicy:::.format_random_effects_for_frame(
          list(
            info = list(
              random_effects = list(
                method = "ML",
                variance_components = data.frame(group = "g", term = "x"),
                null_lrt = spicy:::.compute_null_model_lrt(fit)
              )
            )
          ),
          mark
        )
      },
      character(1)
    )
  }
  en <- c(
    "complementary log-log regression",
    "log-binomial regression",
    "binomial regression",
    "inverse-gaussian regression",
    "negative binomial\\([0-9.]+\\) regression"
  )
  fr <- c(
    "log-log compl\u00E9mentaire",
    "log-binomiale",
    "binomiale",
    "gaussienne inverse",
    "binomiale n\u00E9gative \\([0-9.]+\\)"
  )
  got_en <- lines(".")
  for (i in seq_along(en)) {
    expect_match(got_en[i], paste0(": LR test vs ", en[i], ", "))
  }
  withr::local_options(spicy.language = "fr")
  got_fr <- lines(",")
  for (i in seq_along(fr)) {
    expect_match(
      got_fr[i],
      paste0("contre la r\u00E9gression ", fr[i], ", ")
    )
  }
})

test_that("the model-based Wald line of lmer, lme and gls follows the language", {
  skip_if_not_installed("lme4")
  skip_if_not_installed("nlme")
  d <- nlme::Orthodont
  fits <- list(
    lme4::lmer(Reaction ~ Days + (1 | Subject), lme4::sleepstudy),
    nlme::lme(distance ~ age, random = ~ 1 | Subject, data = d),
    nlme::gls(distance ~ age, data = d),
    nlme::gls(
      distance ~ age,
      data = d,
      correlation = nlme::corAR1(form = ~ 1 | Subject)
    )
  )
  se_lines <- function() {
    vapply(
      fits,
      function(fit) {
        note <- attr(table_regression(fit), "note")
        lines <- strsplit(note, "\n", fixed = TRUE)[[1L]]
        lines[grepl("^(Std. errors|Erreurs standards)", lines)]
      },
      character(1)
    )
  }
  expect_identical(
    se_lines(),
    c(
      rep("Std. errors: Wald (model-based).", 3L),
      "Std. errors: Wald (model-based, corAR1)."
    )
  )
  withr::local_options(spicy.language = "fr")
  expect_identical(
    se_lines(),
    c(
      rep(
        "Erreurs standards\u00A0: Wald (fond\u00E9es sur le mod\u00E8le).",
        3L
      ),
      "Erreurs standards\u00A0: Wald (fond\u00E9es sur le mod\u00E8le, corAR1)."
    )
  )
})

test_that("the ML-refit note of a REML hierarchy follows the language", {
  skip_if_not_installed("lme4")
  withr::local_options(spicy.language = "fr")
  m1 <- lme4::lmer(Reaction ~ 1 + (1 | Subject), data = lme4::sleepstudy)
  m2 <- lme4::lmer(Reaction ~ Days + (1 | Subject), data = lme4::sleepstudy)
  expect_match(
    attr(table_regression(list(m1, m2), nested = TRUE), "note"),
    paste0(
      "Les statistiques de variation sont calcul\u00E9es \u00E0 partir de ",
      "mod\u00E8les r\u00E9estim\u00E9s par maximum de vraisemblance et ne ",
      "correspondent pas n\u00E9cessairement aux \u00E9carts entre les ",
      "crit\u00E8res REML affich\u00E9s."
    ),
    fixed = TRUE
  )
})

test_that("the outcome comparison note joins its tests in the language", {
  note <- function() {
    attr(table_outcome(mtcars, mpg, select = c(am, vs, cyl)), "note")
  }
  expect_match(
    note(),
    "Welch one-way ANOVA (cyl); Welch t-test (am, vs).",
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  expect_match(
    note(),
    "ANOVA de Welch \u00E0 un facteur (cyl)\u00A0; test t de Welch (am, vs).",
    fixed = TRUE
  )
})

test_that("the group-comparison note of table_continuous() joins in the language", {
  d <- data.frame(
    skewed = (1:60)^2,
    age = 20 + (1:60) %% 7,
    g = rep(c("a", "b"), 30)
  )
  note <- function() {
    attr(
      table_continuous(
        d,
        select = c(skewed, age),
        by = g,
        show_columns = list(
          skewed = c("med_iqr", "n"),
          age = c("m", "sd", "n")
        ),
        output = "long"
      ),
      "missing_note"
    )
  }
  expect_match(
    note(),
    "Group comparison: Welch t-test (age); Wilcoxon rank-sum test (skewed).",
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  expect_match(
    note(),
    paste0(
      "Comparaison des groupes\u00A0: test de Wilcoxon-Mann-Whitney ",
      "(skewed)\u00A0; test t de Welch (age)."
    ),
    fixed = TRUE
  )
})

test_that("the survival estimand note joins its glosses in the language", {
  es <- list(
    tau = 365,
    at_time = 200,
    boot_n = 500L,
    boot_valid = 500L,
    stratified = FALSE,
    skipped_terms = character(0)
  )
  note <- function() {
    spicy:::build_survival_estimand_footer_block_from_frames(
      list(list(info = list(extras = list(survival_estimands = es))))
    )
  }
  expect_identical(
    note(),
    paste0(
      "dRMST = difference in restricted mean survival time over [0, 365]; ",
      "dRisk = difference in cumulative incidence at 200; adjusted by ",
      "g-computation from the fitted model, SEs by nonparametric bootstrap ",
      "(500 replicates)."
    )
  )
  withr::local_options(spicy.language = "fr")
  expect_identical(
    note(),
    paste0(
      "dRMST = diff\u00E9rence de temps de survie moyen restreint sur ",
      "[0, 365]\u00A0; dRisk = diff\u00E9rence d'incidence cumul\u00E9e ",
      "\u00E0 200\u00A0; ajust\u00E9e par g-computation \u00E0 partir du ",
      "mod\u00E8le estim\u00E9, erreurs standards par bootstrap non ",
      "param\u00E9trique (500 r\u00E9plications)."
    )
  )
})

test_that("the nonconvergence note joins its diagnoses in the language", {
  problems <- list(
    list(kind = "engine_message", value = "false convergence (8)"),
    list(kind = "hessian", value = NULL)
  )
  note <- \() spicy:::.glmmTMB_convergence_note(problems)
  expect_identical(
    note(),
    paste0(
      "Model convergence problem: false convergence (8); non-positive-definite ",
      "Hessian matrix. The estimates are the values the optimizer stopped at, ",
      "not a converged fit."
    )
  )
  withr::local_options(spicy.language = "fr")
  expect_identical(
    note(),
    paste0(
      "Probl\u00E8me de convergence du mod\u00E8le\u00A0: false convergence ",
      "(8)\u00A0; matrice hessienne non d\u00E9finie positive. Les estimations ",
      "sont les valeurs auxquelles l'optimiseur s'est arr\u00EAt\u00E9, et non ",
      "celles d'un ajustement ayant converg\u00E9."
    )
  )
})

test_that("the Bayesian sampler and predictive-accuracy notes follow the language", {
  # Pure functions of a diagnostics list: the English and the decimal mark
  # are pinned in test-footer_typography.R.
  conv <- function(...) {
    utils::modifyList(
      list(
        rhat = NA_real_,
        ess = NA_integer_,
        ess_bar = 400L,
        n_div = NA_integer_,
        bfmi = NA_real_,
        div_unavailable = FALSE
      ),
      list(...)
    )
  }
  loo <- function(...) {
    utils::modifyList(
      list(
        has_elpd = TRUE,
        elpd_se = 12.3456,
        has_waic = TRUE,
        waic_se = 11.9876,
        n_bad_k = 0L,
        n_k = 100L,
        k_thr = 0.7,
        n_bad_p = 0L
      ),
      list(...)
    )
  }
  flagged <- conv(rhat = 1.0342, ess = 120L, n_div = 5L, bfmi = 0.1234)
  en <- spicy:::.stan_convergence_text(flagged)
  withr::local_options(spicy.language = "fr")
  expect_identical(
    spicy:::.stan_convergence_text(flagged, ","),
    paste0(
      "Diagnostics de l'\u00E9chantillonneur\u00A0: R-hat max. = 1,034 ",
      "(cible < 1,01)\u00A0; ESS min. = 120 (cible > 400)\u00A0; 5 ",
      "transitions divergentes\u00A0; E-BFMI min. = 0,12 (cible > 0,2). ",
      "Ne pas rapporter tel quel\u00A0; allonger l'\u00E9chantillonnage ",
      "ou reparam\u00E9trer le mod\u00E8le (Vehtari et al. 2021)."
    )
  )
  # The classed warning quotes the English sentence in every language.
  expect_identical(
    spicy:::.stan_convergence_text(flagged, ".", translated = FALSE),
    en
  )
  expect_match(
    spicy:::.stan_convergence_text(conv(n_div = 1L), ","),
    "\u00A0: 1 transition divergente. Ne pas",
    fixed = TRUE
  )
  expect_match(
    spicy:::.stan_convergence_text(conv(n_div = 2L), ","),
    "\u00A0: 2 transitions divergentes. Ne pas",
    fixed = TRUE
  )
  expect_identical(
    spicy:::.stan_convergence_text(conv(div_unavailable = TRUE), ","),
    paste0(
      "Diagnostics de l'\u00E9chantillonneur\u00A0: R-hat et ESS dans les ",
      "cibles\u00A0; nombre de transitions divergentes indisponible pour ",
      "cet ajustement."
    )
  )
  expect_identical(
    spicy:::.stan_loo_text(loo(), ","),
    paste0(
      "Performance pr\u00E9dictive par PSIS-LOO / WAIC\u00A0; ",
      "ES(ELPD) = 12,3\u00A0; ES(WAIC) = 12,0."
    )
  )
  # Both reliability caveats; the R code they quote keeps its point.
  expect_identical(
    spicy:::.stan_loo_text(
      loo(has_elpd = FALSE, has_waic = FALSE, n_bad_k = 3L, n_bad_p = 2L),
      ","
    ),
    paste0(
      "PSIS-LOO non fiable pour 3 observations sur 100 (k de Pareto > ",
      "0,70)\u00A0; envisager loo::loo_moment_match(), la ",
      "r\u00E9estimation des plis signal\u00E9s (k_threshold = 0.7) ou ",
      "une validation crois\u00E9e en K plis. Approximation WAIC non ",
      "fiable pour 2 observation(s) (p_waic > 0,4)\u00A0; ",
      "pr\u00E9f\u00E9rer PSIS-LOO (`show_fit_stats = \"elpd_loo\"`)."
    )
  )
})

test_that("the standardized-coefficient legend follows the language", {
  d <- transform(mtcars, cyl_f = factor(cyl))
  fit <- stats::lm(mpg ~ wt + cyl_f, data = d)
  fit_glm <- stats::glm(vs ~ mpg, data = d, family = binomial)
  note <- function(method) {
    f <- if (identical(method, "pseudo")) fit_glm else fit
    attr(
      table_regression(f, show_columns = c("b", "beta"), standardized = method),
      "note"
    )
  }
  expect_match(
    note("refit"),
    paste0(
      "\u03B2 = standardized coefficient (\"refit\": outcome and numeric ",
      "predictors z-scored, factor dummies on 0/1)"
    ),
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  expect_match(
    note("refit"),
    paste0(
      "\u03B2 = coefficient standardis\u00E9 (\"refit\"\u00A0: variable ",
      "d\u00E9pendante et pr\u00E9dicteurs num\u00E9riques ",
      "centr\u00E9s-r\u00E9duits, indicatrices des facteurs en 0/1)"
    ),
    fixed = TRUE
  )
  for (method in c("posthoc", "basic", "smart", "pseudo")) {
    expect_match(
      note(method),
      paste0("\u03B2 = coefficient standardis\u00E9 (\"", method, "\"\u00A0: "),
      fixed = TRUE
    )
  }
})

# One footer line, picked by its opening words.
.note_line <- function(x, start) {
  lines <- strsplit(attr(x, "note"), "\n", fixed = TRUE)[[1L]]
  lines[startsWith(lines, start)]
}

test_that("the abbreviation and association-measure notes join in the language", {
  abbrev <- function() {
    spicy:::build_abbreviations_footer_block_from_frames(
      c("ame", "partial_eta2")
    )
  }
  assoc <- function() {
    spicy:::.assoc_note_apa(
      c(smoking = "cramer_v", education = "tau_b"),
      c("Smoking", "Education")
    )
  }
  expect_identical(
    abbrev(),
    "AME = average marginal effect; \u03B7\u00B2 = partial eta-squared."
  )
  expect_identical(
    assoc(),
    "Cramer's V: Smoking; Kendall's Tau-b: Education."
  )
  withr::local_options(spicy.language = "fr")
  expect_identical(
    abbrev(),
    paste0(
      "AME = effet marginal moyen\u00A0; \u03B7\u00B2 = \u00EAta-carr\u00E9 ",
      "partiel."
    )
  )
  expect_identical(
    assoc(),
    "V de Cram\u00E9r\u00A0: Smoking\u00A0; Tau-b de Kendall\u00A0: Education."
  )
})

test_that("the star legend takes the French no-break spaces", {
  expect_identical(
    spicy:::build_stars_footer_block(TRUE, ","),
    "*** p < 0,001, ** p < 0,01, * p < 0,05."
  )
  withr::local_options(spicy.language = "fr")
  expect_identical(
    spicy:::build_stars_footer_block(TRUE, ","),
    paste0(
      "*** p\u00A0<\u00A00,001, ** p\u00A0<\u00A00,01, ",
      "* p\u00A0<\u00A00,05."
    )
  )
})

test_that("the exponentiation note follows the language", {
  fit <- stats::glm(vs ~ mpg, data = mtcars, family = binomial)
  exp_line <- function(cols, start) {
    .note_line(
      table_regression(fit, exponentiate = TRUE, show_columns = cols),
      start
    )
  }
  expect_identical(
    exp_line(c("b", "se", "ci", "p"), "Coefficients"),
    paste0(
      "Coefficients exponentiated and displayed as OR; SE on the OR scale ",
      "(delta method); CI bounds exponentiated (asymmetric)."
    )
  )
  withr::local_options(spicy.language = "fr")
  expect_identical(
    exp_line(c("b", "se", "ci", "p"), "Coefficients"),
    paste0(
      "Coefficients exponenti\u00E9s et affich\u00E9s en OR\u00A0; erreur ",
      "standard sur l'\u00E9chelle des OR (m\u00E9thode delta)\u00A0; bornes ",
      "de l'IC exponenti\u00E9es (asym\u00E9triques)."
    )
  )
  expect_identical(
    exp_line(c("b", "ci", "p"), "Coefficients"),
    paste0(
      "Coefficients exponenti\u00E9s et affich\u00E9s en OR\u00A0; bornes ",
      "de l'IC exponenti\u00E9es."
    )
  )
  # A table mixing an identity-link model names the exponentiated one.
  mixed <- table_regression(
    list(stats::lm(mpg ~ wt, data = mtcars), fit),
    exponentiate = TRUE,
    show_columns = c("b", "ci", "p")
  )
  expect_identical(
    .note_line(mixed, "Model 2"),
    paste0(
      "Model 2\u00A0: coefficients exponenti\u00E9s et affich\u00E9s en ",
      "OR\u00A0; bornes de l'IC exponenti\u00E9es."
    )
  )
})

test_that("the component-block scale notes follow the language", {
  skip_if_not_installed("pscl")
  zi_note <- function(link) {
    fit <- pscl::zeroinfl(
      art ~ fem + ment | ment,
      data = pscl::bioChemists,
      link = link
    )
    attr(table_regression(fit, exponentiate = TRUE), "note")
  }
  expect_match(
    zi_note("logit"),
    "excess) zero. Coefficients exponentiated and displayed as odds ratios.",
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  expect_match(
    zi_note("logit"),
    paste0(
      "Composante \u00AB\u00A0Inflation de z\u00E9ros\u00A0\u00BB\u00A0: logarithme ",
      "de la cote d'un z\u00E9ro structurel (exc\u00E9dentaire). Coefficients ",
      "exponenti\u00E9s et affich\u00E9s en rapports de cotes."
    ),
    fixed = TRUE
  )
  expect_match(
    zi_note("probit"),
    paste0(
      "(exc\u00E9dentaire) sur l'\u00E9chelle probit. Laiss\u00E9s sur ",
      "l'\u00E9chelle du lien (non exponenti\u00E9s)."
    ),
    fixed = TRUE
  )
})

test_that("a zero part on another link than logit names it in its gloss", {
  # "Log-odds" holds for the logit link only.
  skip_if_not_installed("pscl")
  zi <- pscl::zeroinfl(
    art ~ fem + ment | ment,
    data = pscl::bioChemists,
    dist = "poisson",
    link = "probit"
  )
  hu <- pscl::hurdle(
    art ~ fem | fem,
    data = pscl::bioChemists,
    link = "probit"
  )
  note <- function(fit) attr(table_regression(fit), "note")
  expect_match(
    note(zi),
    paste0(
      "Zero-inflation component: probability of a structural (excess) ",
      "zero on the probit scale."
    ),
    fixed = TRUE
  )
  expect_match(
    note(hu),
    "Zero hurdle component: probability of a nonzero count on the probit scale.",
    fixed = TRUE
  )
  expect_false(grepl("log-odds", note(zi), fixed = TRUE))
  withr::local_options(spicy.language = "fr")
  expect_match(
    note(zi),
    paste0(
      "Composante \u00AB\u00A0Inflation de z\u00E9ros\u00A0\u00BB\u00A0: ",
      "probabilit\u00E9 d'un z\u00E9ro structurel (exc\u00E9dentaire) sur ",
      "l'\u00E9chelle probit."
    ),
    fixed = TRUE
  )
  expect_match(
    note(hu),
    paste0(
      "Composante \u00AB\u00A0Hurdle (z\u00E9ro)\u00A0\u00BB\u00A0: ",
      "probabilit\u00E9 d'un effectif non nul sur l'\u00E9chelle probit."
    ),
    fixed = TRUE
  )
})

test_that("the threshold and scale-effect notes follow the language", {
  skip_if_not_installed("ordinal")
  wine <- ordinal::wine
  logit <- ordinal::clm(rating ~ temp + contact, data = wine)
  cloglog <- ordinal::clm(
    rating ~ temp + contact,
    data = wine,
    link = "cloglog"
  )
  het <- ordinal::clm(rating ~ temp, scale = ~contact, data = wine)
  note <- function(fit) {
    attr(table_regression(fit, exponentiate = TRUE), "note")
  }
  withr::local_options(spicy.language = "fr")
  expect_match(
    note(logit),
    paste0(
      "Seuils\u00A0: points de coupure entre les cat\u00E9gories de ",
      "r\u00E9ponse sur l'\u00E9chelle latente (\u00E9chelle logit, non ",
      "exponenti\u00E9s)."
    ),
    fixed = TRUE
  )
  expect_match(
    note(cloglog),
    "(\u00E9chelle du log-risque cumul\u00E9, non exponenti\u00E9s).",
    fixed = TRUE
  )
  expect_match(
    note(cloglog),
    paste0(
      " HR est le rapport de risques proportionnels en temps group\u00E9 ",
      "exp(-B)\u00A0: la param\u00E9trisation cumulative cloglog"
    ),
    fixed = TRUE
  )
  expect_match(
    note(het),
    paste0(
      "de la r\u00E9ponse latente (\u00E9chelle logarithmique, non ",
      "exponenti\u00E9s\u00A0: leur exponentielle est un rapport ",
      "d'\u00E9carts-types latents, pas un rapport de cotes)."
    ),
    fixed = TRUE
  )
})

test_that("the polynomial-trend legend follows the language", {
  d <- transform(
    mtcars,
    gear_o = factor(gear, ordered = TRUE),
    carb_o = factor(carb, ordered = TRUE)
  )
  note <- function(f) {
    suppressMessages(attr(table_regression(stats::lm(f, data = d)), "note"))
  }
  expect_match(
    note(mpg ~ gear_o),
    "Ordered factor `gear_o`: polynomial trends (.L = linear, .Q = quadratic).",
    fixed = TRUE
  )
  withr::local_options(spicy.language = "fr")
  expect_match(
    note(mpg ~ carb_o),
    paste0(
      "Facteur ordonn\u00E9 `carb_o`\u00A0: tendances polynomiales ",
      "(.L = lin\u00E9aire, .Q = quadratique, .C = cubique, ^4 = quartique, ",
      "^5 = quintique)."
    ),
    fixed = TRUE
  )
  expect_match(
    note(mpg ~ gear_o + carb_o),
    "Facteurs ordonn\u00E9s `gear_o`, `carb_o`\u00A0: tendances polynomiales (",
    fixed = TRUE
  )
  expect_identical(spicy:::ordinal_label(7), "degr\u00E9 7")
})

test_that("the standardized caveat follows the language", {
  frame <- function(used) {
    list(
      info = list(
        extras = list(
          non_additive = list(has_problem = TRUE),
          standardized_used = used
        )
      )
    )
  }
  caveat <- function(method, used = method) {
    spicy:::build_standardized_caveat_footer_block_from_frames(
      list(frame(used)),
      method
    )
  }
  withr::local_options(spicy.language = "fr")
  algebraic <- paste0(
    "les termes d'interaction ou transform\u00E9s sont r\u00E9duits par ",
    "l'\u00E9cart-type de la colonne produit (ou transform\u00E9e) de la ",
    "matrice du mod\u00E8le\u00A0; diff\u00E8re de \"refit\" lorsque les ",
    "composantes sont corr\u00E9l\u00E9es."
  )
  expect_identical(
    caveat("posthoc"),
    paste0("\u03B2 standardis\u00E9\u00A0: ", algebraic)
  )
  expect_identical(
    caveat("refit"),
    paste0(
      "\u03B2 standardis\u00E9\u00A0: apr\u00E8s r\u00E9estimation sur les ",
      "donn\u00E9es centr\u00E9es-r\u00E9duites, le \u03B2 d'une interaction ",
      "est le coefficient du produit des composantes centr\u00E9es-r\u00E9duites."
    )
  )
  expect_identical(
    caveat("refit", used = "posthoc"),
    paste0(
      "\u03B2 standardis\u00E9\u00A0: \"refit\" a \u00E9chou\u00E9\u00A0; ",
      "mise \u00E0 l'\u00E9chelle alg\u00E9brique (posthoc) appliqu\u00E9e\u00A0: ",
      algebraic
    )
  )
})

test_that("the random-effects notes follow the language", {
  skip_if_not_installed("lme4")
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)
  withr::local_options(spicy.language = "fr")
  expect_identical(
    .note_line(table_regression(fit, re_test = "lrt"), "Valeurs p des"),
    paste0(
      "Valeurs p des effets al\u00E9atoires\u00A0: test du rapport de ",
      "vraisemblance contre la structure al\u00E9atoire r\u00E9duite, loi de ",
      "r\u00E9f\u00E9rence \u03C7\u0304\u00B2 (m\u00E9lange de khi-deux)."
    )
  )
  expect_match(
    spicy:::build_random_effects_footer_block_from_frames(
      list(as_regression_frame(fit)),
      re_test = "rlrt"
    ),
    paste0(
      "Valeur p des effets al\u00E9atoires\u00A0: test exact du rapport de ",
      "vraisemblance restreinte (distribution nulle simul\u00E9e)."
    ),
    fixed = TRUE
  )
  expect_identical(
    spicy:::.singular_msg_for_frame(list(), TRUE),
    paste0(
      "Ajustement singulier\u00A0: la structure des effets al\u00E9atoires ",
      "est \u00E0 la fronti\u00E8re de l'espace des param\u00E8tres ou pr\u00E8s ",
      "de celle-ci (une variance de 0 ou une corr\u00E9lation de +/-1)\u00A0; ",
      "l'erreur standard de Wald et l'IC des composantes de la variance ",
      "sont omis."
    )
  )
  expect_identical(
    spicy:::build_re_profile_footer_block_from_frames(
      list(list(info = list(extras = list(re_ci = "profile"))))
    ),
    paste0(
      "Composantes de la variance des effets al\u00E9atoires\u00A0: IC par ",
      "vraisemblance profil\u00E9e\u00A0; pas d'erreur standard (intervalles ",
      "asym\u00E9triques)."
    )
  )
  expect_identical(
    spicy:::build_re_se_skipped_footer_block_from_frames(
      list(list(info = list(extras = list(re_se_skipped_n = 180L))))
    ),
    paste0(
      "Composantes de la variance des effets al\u00E9atoires\u00A0: erreur ",
      "standard et IC non calcul\u00E9s (n = 180 d\u00E9passe le plafond ",
      "spicy.re_se_max_n)."
    )
  )
})

test_that("the reference-level, survival and AME notes follow the language", {
  skip_if_not_installed("survival")
  stub <- function(...) list(info = list(...))
  withr::local_options(spicy.language = "fr")
  expect_identical(
    spicy:::build_reference_outcome_footer_block_from_frames(
      list(stub(extras = list(reference_outcome = "Employed")))
    ),
    "Cat\u00E9gorie de r\u00E9f\u00E9rence de la variable d\u00E9pendante\u00A0: Employed."
  )
  expect_identical(
    spicy:::build_reference_alternative_footer_block_from_frames(
      list(stub(extras = list(reference_alternative = "beach")))
    ),
    "Alternative de r\u00E9f\u00E9rence\u00A0: beach."
  )
  lung <- survival::lung
  cox <- survival::coxph(survival::Surv(time, status) ~ age + sex, data = lung)
  aft <- survival::survreg(
    survival::Surv(time, status) ~ age + sex,
    data = lung
  )
  expect_identical(
    .note_line(table_regression(cox), "Concordance"),
    "Concordance C = 0,60 (erreur standard = 0,03)."
  )
  expect_identical(
    .note_line(table_regression(aft), "Distribution"),
    "Distribution\u00A0: Weibull\u00A0; \u00E9chelle = 0,75."
  )
  satt <- function(classes) {
    spicy:::build_ame_satterthwaite_footer_block_from_frames(
      lapply(classes, function(cl) {
        stub(class = cl, extras = list(use_ame_satterthwaite = TRUE))
      }),
      "ame"
    )
  }
  lead <- "Inf\u00E9rence sur les AME\u00A0: test t avec degr\u00E9s de libert\u00E9 de Satterthwaite"
  expect_identical(satt("lm"), paste0(lead, "."))
  expect_identical(
    satt("glm"),
    paste0(lead, " (approximation par le coefficient dominant).")
  )
  expect_identical(
    satt(c("lm", "glm")),
    paste0(
      lead,
      " (formule exacte pour lm\u00A0; approximation par le coefficient ",
      "dominant pour glm)."
    )
  )
})

test_that("the GEE and quantile-regression notes follow the language", {
  skip_if_not_installed("geepack")
  gee <- geepack::geeglm(
    breaks ~ tension,
    id = wool,
    data = warpbreaks,
    family = poisson,
    corstr = "exchangeable"
  )
  withr::local_options(spicy.language = "fr")
  x <- table_regression(gee)
  expect_identical(
    .note_line(x, "Erreurs standards"),
    "Erreurs standards\u00A0: sandwich robuste (GEE), grappes d\u00E9finies par wool."
  )
  expect_identical(
    .note_line(x, "Corr\u00E9lation"),
    "Corr\u00E9lation de travail GEE\u00A0: exchangeable (alpha = 0,02)."
  )
  gee_stub <- function(corstr, alpha) {
    list(
      info = list(
        class = "geeglm",
        extras = list(gee_corstr = corstr, gee_alpha = alpha)
      )
    )
  }
  expect_identical(
    spicy:::.format_gee_for_frame(gee_stub("unstructured", c(0.1, 0.2, 0.3))),
    "Corr\u00E9lation de travail GEE\u00A0: unstructured (3 param\u00E8tres de corr\u00E9lation)."
  )
  expect_identical(
    spicy:::.format_gee_for_frame(gee_stub("independence", numeric(0))),
    "Corr\u00E9lation de travail GEE\u00A0: independence."
  )
  expect_identical(
    spicy:::.geeglm_vcov_label("jack", "wool"),
    "jackknife approch\u00E9 (GEE), grappes d\u00E9finies par wool"
  )
  expect_identical(
    vapply(
      c("nid", "iid", "ker", "rank"),
      spicy:::.rq_vcov_footer_label,
      character(1),
      boot_n = 50L,
      clustered = FALSE,
      USE.NAMES = FALSE
    ),
    c(
      "sandwich quantile (nid, Hall-Sheather)",
      "quantile (iid)",
      "noyau quantile (ker)",
      "inversion du test de rang (IC seulement)"
    )
  )
  expect_identical(
    spicy:::.rq_vcov_footer_label("boot", 50L, FALSE),
    "bootstrap quantile (paires xy, R = 50)"
  )
  expect_identical(
    spicy:::.rq_vcov_footer_label("boot", 50L, TRUE),
    "bootstrap quantile sauvage par gradient, par grappes (R = 50)"
  )
})

# The standard-error line of one fit, in English and then in French.
.se_line_en_fr <- function(fit) {
  en <- .note_line(table_regression(fit), "Std. errors")
  withr::local_options(spicy.language = "fr")
  c(en, .note_line(table_regression(fit), "Erreurs standards"))
}

test_that("the classical standard-error line of nls follows the language", {
  fit <- nls(
    conc ~ A * exp(-k * time),
    data = datasets::Indometh,
    start = list(A = 2, k = 0.5)
  )
  expect_identical(
    .se_line_en_fr(fit),
    c("Std. errors: Classical.", "Erreurs standards\u00A0: classiques.")
  )
})

test_that("the classical standard-error line of ivreg follows the language", {
  skip_if_not_installed("AER")
  fit <- AER::ivreg(mpg ~ wt + cyl | hp + drat + cyl, data = mtcars)
  expect_identical(
    .se_line_en_fr(fit),
    c("Std. errors: Classical.", "Erreurs standards\u00A0: classiques.")
  )
})

test_that("the Bayesian standard-error line of mgcv follows the language", {
  skip_if_not_installed("mgcv")
  fit <- mgcv::gam(mpg ~ s(wt), data = mtcars)
  expect_identical(
    .se_line_en_fr(fit),
    c(
      "Std. errors: Bayesian (mgcv posterior covariance).",
      "Erreurs standards\u00A0: bay\u00E9siennes (covariance a posteriori de mgcv)."
    )
  )
})

test_that("the classical line of rms, fixest and estimatr follows the language", {
  skip_if_not_installed("rms")
  skip_if_not_installed("fixest")
  skip_if_not_installed("estimatr")
  fits <- list(
    rms::ols(mpg ~ wt, data = mtcars),
    fixest::feols(mpg ~ wt, data = mtcars),
    estimatr::lm_robust(mpg ~ wt, data = mtcars, se_type = "classical")
  )
  for (fit in fits) {
    expect_identical(
      .se_line_en_fr(fit),
      c("Std. errors: Classical.", "Erreurs standards\u00A0: classiques.")
    )
  }
})

test_that("the univariable table title and sample note follow the language", {
  withr::local_options(spicy.language = "fr")
  cc <- table_regression_uv(
    sochealth,
    outcome = smoking,
    predictors = c(age, bmi),
    method = "glm",
    complete_cases = TRUE
  )
  expect_identical(
    attr(cc, "title"),
    "R\u00E9gression logistique univari\u00E9e et multivariable\u00A0: smoking"
  )
  n_cc <- sum(stats::complete.cases(
    as.data.frame(sochealth)[c("smoking", "age", "bmi")]
  ))
  expect_identical(
    .note_line(cc, "Tous"),
    sprintf(
      "Tous les mod\u00E8les sont estim\u00E9s sur les %d m\u00EAmes cas complets.",
      n_cc
    )
  )
  own <- table_regression_uv(
    sochealth,
    outcome = smoking,
    predictors = c(age, bmi),
    method = "glm"
  )
  expect_match(
    attr(own, "note"),
    paste0(
      "Chaque mod\u00E8le univari\u00E9 est estim\u00E9 sur ses propres cas ",
      "complets\u00A0; N varie selon le pr\u00E9dicteur \\([0-9]+-[0-9]+\\)[.]"
    )
  )
  screen <- table_regression_uv(
    sochealth,
    outcome = smoking,
    predictors = c(age, sex),
    method = "glm",
    multivariable = FALSE
  )
  expect_identical(
    attr(screen, "title"),
    "R\u00E9gression logistique univari\u00E9e (pr\u00E9s\u00E9lection)\u00A0: smoking"
  )
  # A family the title bridge does not carry keeps the whole English title.
  gamma <- table_regression_uv(
    sochealth,
    outcome = income,
    predictors = c(age, sex),
    method = "glm",
    family = stats::Gamma(link = "log")
  )
  expect_identical(
    attr(gamma, "title"),
    "Univariable and multivariable Gamma regression: income"
  )
})
