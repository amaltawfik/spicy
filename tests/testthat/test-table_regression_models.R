# table_regression_models(): the machine-readable registry of supported
# classes. The same registry renders the documentation table, so this test
# also guards the published list against drift from the actual dispatch
# methods.

test_that("the registry matches the as_regression_frame dispatch methods", {
  reg <- table_regression_models()
  expect_s3_class(reg, "data.frame")
  expect_identical(
    names(reg),
    c(
      "family",
      "class",
      "engine",
      "ame",
      "exponentiate",
      "blocks",
      "vcov",
      "fit_stats_default",
      "fit_stats"
    )
  )
  expect_true(all(nzchar(reg$vcov)))
  expect_true(all(nzchar(reg$class)))
  expect_false(any(duplicated(reg$class)))

  # Every registry class has a dispatch method...
  dispatched <- sub(
    "^as_regression_frame\\.",
    "",
    utils::methods("as_regression_frame")
  )
  dispatched <- setdiff(dispatched, "default")
  # (registry rows name the user-facing class; a few dispatch classes are
  # umbrella/inherited entries not listed separately)
  umbrella <- c(
    "lmerModLmerTest", # inherits the lmerMod row
    "spicy_uv_screen", # internal bundle built by
    # table_regression_uv(), not a
    # user-fitted model class
    "spicy_outcomes_screen", # the same, built by
    # table_regression_outcomes()
    "gee" # refusal stub: gee::gee fits inherit glm and would
    # otherwise silently get naive SEs; the method points at
    # geepack::geeglm (the supported, registry-listed engine)
  )
  expect_true(
    all(reg$class %in% dispatched),
    info = paste(
      "registry classes without a method:",
      paste(setdiff(reg$class, dispatched), collapse = ", ")
    )
  )
  # ...and every dispatch method is in the registry (no silent additions).
  expect_true(
    all(setdiff(dispatched, umbrella) %in% reg$class),
    info = paste(
      "methods missing from the registry:",
      paste(setdiff(setdiff(dispatched, umbrella), reg$class), collapse = ", ")
    )
  )
})

test_that("the rendered documentation table covers every registry row", {
  md <- spicy:::.render_supported_models_md()
  reg <- table_regression_models()
  expect_equal(length(strsplit(md, "\n")[[1]]), nrow(reg) + 2L)
})

test_that("the table of one family keeps its classes and drops the Family column", {
  reg <- table_regression_models()
  md <- strsplit(
    spicy:::.render_supported_models_md("Survival"),
    "\n",
    fixed = TRUE
  )[[1]]
  surv <- reg$class[reg$family == "Survival"]
  # Header, rule, then one row per class of the family and no other.
  expect_length(md, length(surv) + 2L)
  expect_false(grepl("Family", md[1L], fixed = TRUE))
  expect_match(md[1L], "| Class | Engine |", fixed = TRUE)
  for (cl in surv) {
    expect_true(any(startsWith(md, paste0("| `", cl, "` |"))), info = cl)
  }
  expect_false(any(grepl("`lmerMod`", md, fixed = TRUE)))
  # A name that is no family of the registry stops the page from building.
  expect_error(
    spicy:::.render_supported_models_md("Survivl"),
    "Not a family of the registry: Survivl",
    fixed = TRUE
  )
})

test_that("the table of families of the README comes from the registry, notes included", {
  reg <- table_regression_models()
  md <- strsplit(spicy:::.render_family_engines_md(), "\n", fixed = TRUE)[[1]]
  fams <- unique(reg$family)
  # Header, rule, then one row per family, in the order of the registry.
  expect_identical(md[1:2], c("| Family | Engines |", "|---|---|"))
  expect_length(md, length(fams) + 2L)
  expect_true(all(startsWith(md[-(1:2)], paste0("| ", fams, " | "))))
  row <- function(fam) md[startsWith(md, paste0("| ", fam, " | "))]
  # Every engine of a family is in its row, one function per code span.
  for (i in seq_len(nrow(reg))) {
    for (fn in strsplit(reg$engine[i], ", ", fixed = TRUE)[[1]]) {
      expect_true(
        grepl(paste0("`", fn, "`"), row(reg$family[i]), fixed = TRUE),
        info = fn
      )
    }
  }
  # glmmTMB is lent to the families it also serves, with what it brings.
  expect_match(
    row("Ordinal"),
    "`glmmTMB::glmmTMB()` (ordinal family)",
    fixed = TRUE
  )
  expect_match(
    row("Counts, two-part"),
    "`glmmTMB::glmmTMB()` (zero-inflation, hurdle and dispersion components)",
    fixed = TRUE
  )
  # The engine whose class carries a non-proportional block says so.
  expect_match(
    row("Ordinal"),
    "`ordinal::clm()` (incl. partial proportional odds)",
    fixed = TRUE
  )
  # The note of a family closes its row, and only the families that have
  # one get a note.
  expect_match(
    row("Bayesian"),
    "(posterior median, credible intervals, no p-values) |",
    fixed = TRUE
  )
  expect_match(row("Survey-weighted"), "(design-based SEs) |", fixed = TRUE)
  expect_false(grepl(") |", row("Survival"), fixed = TRUE))
})

test_that("the README shows the table of families the registry renders", {
  readme <- testthat::test_path("..", "..", "README.md")
  skip_if_not(file.exists(readme), "README.md not in this tree")
  lines <- readLines(readme, warn = FALSE, encoding = "UTF-8")
  # pandoc writes a no-break space after an abbreviation ("incl."), and
  # its own rule under the header: the rows are compared with plain
  # spaces, and the rule is left out.
  lines <- gsub(intToUtf8(160L), " ", lines, fixed = TRUE)
  md <- strsplit(spicy:::.render_family_engines_md(), "\n", fixed = TRUE)[[1]]
  md <- md[md != "|---|---|"]
  # A README that was not rebuilt after the registry changed fails here.
  for (l in md) {
    expect_true(l %in% lines, info = l)
  }
})

test_that("every argument of table_regression() is common or of a family", {
  args <- names(formals(table_regression))
  fam <- names(.REG_FAMILY_ARGS)
  expect_length(intersect(fam, .REG_COMMON_ARGS), 0L)
  expect_setequal(c(fam, .REG_COMMON_ARGS), args)
  expect_false(anyDuplicated(.REG_COMMON_ARGS) > 0L)
  reg <- table_regression_models()
  expect_true(all(names(.REG_FAMILY_PAGE) %in% reg$family))
  expect_true(all(unlist(.REG_FAMILY_ARGS) %in% names(.REG_FAMILY_PAGE)))
})

test_that("an argument of a family is documented on the page of its family", {
  # The Usage block is generated from .REG_FAMILY_ARGS: the argument must
  # also be named in the rest of the page. Source tree when available, the
  # built package's Rd database otherwise.
  src <- testthat::test_path("..", "..", "man")
  page_text <- function(tp) {
    f <- file.path(src, paste0(tp, ".Rd"))
    rd <- if (file.exists(f)) {
      tools::parse_Rd(f)
    } else {
      tools::Rd_db("spicy")[[paste0(tp, ".Rd")]]
    }
    keep <- vapply(
      rd,
      function(x) {
        !(identical(attr(x, "Rd_tag"), "\\section") &&
          identical(paste(as.character(x[[1L]]), collapse = ""), "Usage"))
      },
      logical(1)
    )
    paste(unlist(lapply(rd[keep], as.character)), collapse = "")
  }
  for (a in names(.REG_FAMILY_ARGS)) {
    for (fam in .REG_FAMILY_ARGS[[a]]) {
      tp <- .REG_FAMILY_PAGE[[fam]]
      expect_match(
        page_text(tp),
        paste0("\\b", a, "\\b"),
        label = paste(a, "on", tp)
      )
      expect_match(
        .render_family_usage(fam),
        paste0("\n  ", a, " = "),
        fixed = TRUE,
        label = paste(a, "in the Usage of", tp)
      )
    }
  }
})

test_that("the default fit statistics of a page come from the registry", {
  expect_identical(
    .render_fit_stats_default("coxph"),
    "`c(\"nobs\", \"n_events\", \"aic\")`"
  )
  expect_identical(.render_fit_stats_default("iv_robust"), "`\"nobs\"`")
  # A token with a condition is left out.
  expect_identical(
    .render_fit_stats_default("lm_robust"),
    "`c(\"nobs\", \"r2\", \"adj_r2\")`"
  )
  expect_error(.render_fit_stats_default("nope"), "Not a class")
  expect_error(.render_family_usage("Linear"), "Not a family with a page")
  expect_error(
    .render_family_usage("Survival", "nope"),
    "Not an argument of table_regression"
  )
})

test_that("a registry cell is split on its top-level commas", {
  expect_identical(
    .reg_cell_tokens(
      "classical, HC0-HC2, CR1 (only without x = TRUE, y = TRUE)"
    ),
    c("classical", "HC0", "HC1", "HC2", "CR1")
  )
  expect_identical(
    .reg_cell_tokens("nobs, r2 (a, b), aic", conditional = FALSE),
    c("nobs", "aic")
  )
})

## ---- Phase 3 matrix (lot T2) ----------------------------------------------

# Phase 3 matrix: rd-vcov-classes:help-topic-aliases
test_that("each model family has a help page of its own", {
  wanted <- c(
    "table_regression_models",
    "table_regression_mixed",
    "table_regression_ordinal",
    "table_regression_counts",
    "table_regression_categorical",
    "table_regression_survival",
    "table_regression_survey",
    "table_regression_robust",
    "table_regression_bayesian"
  )
  rd_aliases <- function(rd) {
    tags <- vapply(
      rd,
      function(x) as.character(attr(x, "Rd_tag")),
      character(1)
    )
    unlist(lapply(rd[tags == "\\alias"], function(x) as.character(x[[1]])))
  }
  # Source tree when available (devtools::test), the built package's Rd
  # database otherwise (R CMD check on the installed copy). Each topic is
  # the one alias of its own Rd file.
  src <- testthat::test_path("..", "..", "man")
  for (tp in wanted) {
    f <- file.path(src, paste0(tp, ".Rd"))
    rd <- if (file.exists(f)) {
      tools::parse_Rd(f)
    } else {
      tools::Rd_db("spicy")[[paste0(tp, ".Rd")]]
    }
    expect_identical(rd_aliases(rd), tp)
  }
})

# Phase 3 matrix: rd-vcov-classes:shared-programmatic-methods
# One representative fit per registry family: tidy() / glance() /
# as_structured() / as.data.frame() all return non-empty objects.
test_that("tidy / glance / as_structured / as.data.frame work for every registry family", {
  skip_on_cran()
  skip_if_not_installed("quantreg")
  skip_if_not_installed("lme4")
  skip_if_not_installed("geepack")
  skip_if_not_installed("MASS")
  skip_if_not_installed("nnet")
  skip_if_not_installed("pscl")
  skip_if_not_installed("survival")
  skip_if_not_installed("survey")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("rms")
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("posterior")
  set.seed(7)
  n <- 150
  d <- data.frame(x1 = rnorm(n), g = factor(sample(10, n, TRUE)))
  d$y <- 1 + 0.5 * d$x1 + rnorm(n)
  d$yo <- factor(sample(1:3, n, TRUE), ordered = TRUE)
  d$time <- rexp(n, exp(-0.2 * d$x1))
  d$status <- rbinom(n, 1, 0.7)
  data("engel", package = "quantreg", envir = environment())
  engel <- get("engel", envir = environment())
  data("bioChemists", package = "pscl", envir = environment())
  bioChemists <- get("bioChemists", envir = environment())
  des <- suppressWarnings(survey::svydesign(id = ~1, data = d))
  fits <- list(
    "Linear and generalized linear" = stats::lm(y ~ x1, data = d),
    "Robust, IV, quantile, panel" = quantreg::rq(
      foodexp ~ income,
      data = engel,
      tau = 0.5
    ),
    "Mixed effects" = lme4::lmer(
      Reaction ~ Days + (1 | Subject),
      data = lme4::sleepstudy
    ),
    "Population-averaged (GEE)" = geepack::geeglm(
      y ~ x1,
      id = g,
      data = d,
      corstr = "exchangeable"
    ),
    "Ordinal" = MASS::polr(yo ~ x1, data = d, Hess = TRUE),
    "Categorical" = nnet::multinom(yo ~ x1, data = d, trace = FALSE),
    "Counts, two-part" = pscl::zeroinfl(
      art ~ fem + ment | ment,
      data = bioChemists
    ),
    "Survival" = survival::coxph(
      survival::Surv(time, status) ~ x1,
      data = d
    ),
    "Survey-weighted" = survey::svyglm(y ~ x1, design = des),
    "Additive, proportions, selection" = mgcv::gam(y ~ x1, data = d),
    "rms" = rms::ols(y ~ x1, data = d, x = TRUE, y = TRUE),
    "Bayesian" = suppressWarnings(rstanarm::stan_glm(
      y ~ x1,
      data = d,
      iter = 400,
      chains = 1,
      refresh = 0,
      seed = 1
    ))
  )
  # The sample really spans EVERY registry family.
  expect_setequal(names(fits), unique(table_regression_models()$family))
  for (fam in names(fits)) {
    tbl <- suppressWarnings(table_regression(fits[[fam]]))
    td <- broom::tidy(tbl)
    gl <- broom::glance(tbl)
    st <- as_structured(tbl)
    df <- as.data.frame(tbl)
    expect_s3_class(td, "data.frame")
    expect_gt(nrow(td), 0L)
    expect_identical(nrow(gl), 1L)
    expect_gt(ncol(gl), 0L)
    st_body <- if (is.data.frame(st)) st else st$body
    expect_s3_class(st_body, "data.frame")
    expect_gt(nrow(st_body), 0L)
    expect_gt(nrow(df), 0L)
  }
})

test_that("a refusal sends a fit to the help page of its family", {
  fake <- function(cls) structure(list(), class = cls)
  expect_identical(
    .reg_help_page(fake(c("tobit", "survreg"))),
    "?table_regression_robust"
  )
  expect_identical(
    .reg_help_page(fake(c("glmerMod", "merMod"))),
    "?table_regression_mixed"
  )
  # A family without a page of its own, and a class outside the registry.
  expect_identical(.reg_help_page(fake("lm")), "?table_regression")
  expect_identical(.reg_help_page(fake("foo")), "?table_regression")
  for (cl in .supported_models_registry()$class) {
    page <- sub("^[?]", "", .reg_help_page(fake(cl)))
    expect_true(page %in% c("table_regression", .REG_FAMILY_PAGE), label = cl)
  }
})
