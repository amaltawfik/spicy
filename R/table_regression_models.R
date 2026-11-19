# ---------------------------------------------------------------------------
# Supported-models registry for table_regression().
#
# ONE machine-readable source of truth (the registry data frame below) feeds:
#   * the exported accessor table_regression_models() (insight-style
#     supported_models() pattern -- the published list can never drift from
#     the code, unlike a hand-maintained Rd table);
#   * the capability table on its documentation page (rendered from the
#     registry at roxygen time via inline R);
#   * the pkgdown "Supported models" article.
#
# The documentation page is the overview of the supported classes; each
# model family has its own concept page (?table_regression_mixed, ...),
# with an Engines table rendered from the same registry and a Usage
# block rendered from the signature of table_regression().
# ---------------------------------------------------------------------------

# The registry. One row per supported engine call. Kept internal; exposed
# through table_regression_models().
.supported_models_registry <- function() {
  df <- rbind(
    c("Linear and generalized linear", "lm", "stats::lm()", "yes", "-", "-"),
    c(
      "Linear and generalized linear",
      "glm",
      "stats::glm()",
      "yes",
      "OR / IRR / RR / MR / HR (link)",
      "-"
    ),
    c(
      "Linear and generalized linear",
      "negbin",
      "MASS::glm.nb()",
      "yes",
      "IRR",
      "-"
    ),
    c("Linear and generalized linear", "rlm", "MASS::rlm()", "yes", "-", "-"),
    c("Linear and generalized linear", "nls", "stats::nls()", "no", "-", "-"),
    c(
      "Robust, IV, quantile, panel",
      "lm_robust",
      "estimatr::lm_robust()",
      "yes",
      "-",
      "-"
    ),
    c(
      "Robust, IV, quantile, panel",
      "iv_robust",
      "estimatr::iv_robust()",
      "yes",
      "-",
      "-"
    ),
    c("Robust, IV, quantile, panel", "ivreg", "AER::ivreg()", "yes", "-", "-"),
    c("Robust, IV, quantile, panel", "tobit", "AER::tobit()", "yes", "-", "-"),
    c("Robust, IV, quantile, panel", "rq", "quantreg::rq()", "yes", "-", "-"),
    c(
      "Robust, IV, quantile, panel",
      "fixest",
      "fixest::feols(), fixest::feglm(), fixest::fepois(), fixest::femlm(), fixest::fenegbin()",
      "yes (no for a GLM with fixed effects)",
      "OR / IRR (link)",
      "-"
    ),
    c("Mixed effects", "lmerMod", "lme4::lmer()", "yes", "-", .REG_BLOCK_RE),
    c(
      "Mixed effects",
      "glmerMod",
      "lme4::glmer()",
      "yes",
      "OR / IRR (link)",
      .REG_BLOCK_RE
    ),
    c(
      "Mixed effects",
      "glmmTMB",
      "glmmTMB::glmmTMB()",
      "yes (no for the ordinal family)",
      "link-dependent (IRR for count families, OR for ordinal logit)",
      paste(
        c(
          .REG_BLOCK_RE,
          .REG_BLOCK_ZI,
          paste0(
            .REG_BLOCK_HURDLE_ZERO,
            " for a zero-truncated family, ziGamma, lognormal or beta_family"
          ),
          .REG_BLOCK_DISP,
          paste0(.REG_BLOCK_THRESH, " (ordinal family)")
        ),
        collapse = "; "
      )
    ),
    c("Mixed effects", "lme", "nlme::lme()", "yes", "-", .REG_BLOCK_RE),
    c("Mixed effects", "gls", "nlme::gls()", "yes", "-", "-"),
    c(
      "Ordinal",
      "polr",
      "MASS::polr()",
      "per category",
      "OR (logit)",
      .REG_BLOCK_THRESH
    ),
    c(
      "Ordinal",
      "clm",
      "ordinal::clm()",
      "per category",
      "OR (logit)",
      paste(c(.REG_BLOCK_THRESH, .REG_BLOCK_NPO), collapse = "; ")
    ),
    c(
      "Ordinal",
      "clmm",
      "ordinal::clmm()",
      "no",
      "OR (logit)",
      paste(c(.REG_BLOCK_THRESH, .REG_BLOCK_RE), collapse = "; ")
    ),
    c(
      "Categorical",
      "multinom",
      "nnet::multinom()",
      "per outcome",
      "OR",
      "per-outcome blocks"
    ),
    c(
      "Categorical",
      "mlogit",
      "mlogit::mlogit()",
      "no",
      "OR",
      "per-alternative rows"
    ),
    c(
      "Counts, two-part",
      "zeroinfl",
      "pscl::zeroinfl()",
      "yes (combined response)",
      "IRR (count) + OR (logit zero part)",
      .REG_BLOCK_ZI
    ),
    c(
      "Counts, two-part",
      "hurdle",
      "pscl::hurdle()",
      "yes (combined response)",
      "IRR (count) + OR (logit zero part)",
      .REG_BLOCK_HURDLE
    ),
    c("Survival", "coxph", "survival::coxph()", "RMST / risk diff", "HR", "-"),
    c(
      "Survival",
      "survreg",
      "survival::survreg()",
      "yes + RMST / risk diff",
      "TR (log-scale distributions)",
      "-"
    ),
    c("Survival", "cph", "rms::cph()", "no", "HR", "-"),
    c(
      "Survival",
      "flexsurvreg",
      "flexsurv::flexsurvreg()",
      "no",
      "TR / HR (dist)",
      "distribution parameters"
    ),
    c(
      "Survey-weighted",
      "svyglm",
      "survey::svyglm()",
      "yes (design-based)",
      "OR / IRR",
      "-"
    ),
    c(
      "Survey-weighted",
      "svyolr",
      "survey::svyolr()",
      "per category (design-based)",
      "OR (logit)",
      .REG_BLOCK_THRESH
    ),
    c(
      "Survey-weighted",
      "svycoxph",
      "survey::svycoxph()",
      "no",
      "HR",
      "-"
    ),
    c(
      "Population-averaged (GEE)",
      "geeglm",
      "geepack::geeglm()",
      "yes",
      "OR / IRR / RR / MR / HR (link)",
      "-"
    ),
    c(
      "Additive, proportions, selection",
      "gam",
      "mgcv::gam(), mgcv::bam()",
      "yes",
      "OR / IRR (link)",
      "-"
    ),
    c(
      "Additive, proportions, selection",
      "betareg",
      "betareg::betareg()",
      "yes",
      "OR (mean link)",
      "-"
    ),
    c(
      "Additive, proportions, selection",
      "selection",
      "sampleSelection::selection()",
      "no",
      "-",
      "selection component"
    ),
    c("rms", "ols", "rms::ols()", "yes", "-", "-"),
    c(
      "rms",
      "lrm",
      "rms::lrm()",
      "yes (per category for an ordinal outcome)",
      "OR",
      paste0(.REG_BLOCK_THRESH, " (ordinal outcome)")
    ),
    c("rms", "Glm", "rms::Glm()", "yes", "link-dependent", "-"),
    c(
      "Bayesian",
      "stanreg",
      "rstanarm::stan_glm(), rstanarm::stan_glmer()",
      "yes (draws)",
      "link-dependent",
      paste0(.REG_BLOCK_RE, " (if multilevel)")
    ),
    c(
      "Bayesian",
      "brmsfit",
      "brms::brm()",
      "yes (draws)",
      "link-dependent",
      paste0(.REG_BLOCK_RE, " (if multilevel)")
    )
  )
  out <- as.data.frame(df, stringsAsFactors = FALSE)
  names(out) <- c("family", "class", "engine", "ame", "exponentiate", "blocks")
  out$vcov <- unname(.REG_VCOV[out$class])
  stopifnot(!anyNA(out$vcov))
  out
}

# The `vcov` values each class accepts, one string per registry class:
# the column `vcov` of the registry. Written by hand from the capability
# gate (.robust_vcov_support() in R/vcov.R, and the refusals of
# validate_vcov_cluster_lists()); each cell was checked on 2026-11-19 by
# fitting each class and calling table_regression() with every token of
# .VCOV_MODES. A parenthesis gives the condition under which the set
# shrinks, or what "classical" stands for when the class carries its own
# variance.
.REG_VCOV <- c(
  lm = "classical, HC0-HC5, CR0-CR3, CR1S, bootstrap, jackknife",
  glm = "classical, HC0-HC5, CR0-CR3, bootstrap, jackknife",
  negbin = "classical, HC0-HC5, CR0-CR3, bootstrap, jackknife",
  rlm = "classical",
  nls = "classical",
  lm_robust = "classical (the fit's own)",
  iv_robust = "classical (the fit's own)",
  ivreg = "classical",
  tobit = "classical, CR1",
  rq = "classical (= nid), nid, iid, ker, rank, bootstrap",
  fixest = "classical (the fit's own)",
  lmerMod = "classical, CR0-CR3",
  glmerMod = "classical",
  glmmTMB = "classical, CR0, CR1 (classical only for REML or the ordinal family)",
  lme = "classical, CR0-CR3",
  gls = "classical",
  geeglm = "classical (the fit's own)",
  polr = "classical, CR1",
  clm = "classical, CR1 (classical only with scale or nominal terms)",
  clmm = "classical",
  multinom = "classical, CR1",
  mlogit = "classical, CR1",
  zeroinfl = "classical, CR1 (classical only for a negative-binomial part)",
  hurdle = "classical, CR1 (classical only for a negative-binomial part)",
  coxph = "classical, CR0",
  survreg = "classical, CR1",
  cph = "classical, CR0",
  flexsurvreg = "classical",
  svyglm = "classical (design-based)",
  svyolr = "classical (design-based)",
  svycoxph = "classical (design-based)",
  gam = "classical, CR1",
  betareg = "classical, CR1",
  selection = "classical",
  ols = "classical, CR0",
  lrm = "classical, CR0",
  Glm = "classical, CR0",
  stanreg = "classical (the posterior)",
  brmsfit = "classical (the posterior)"
)

# The other families a class also serves, with what it brings to each:
# glmmTMB is a mixed engine (its row of the registry) that also fits
# ordinal models and zero-inflated, hurdle and dispersion models. One
# entry per class, a named vector family = what the class covers there.
.REG_ALSO <- list(
  glmmTMB = c(
    "Ordinal" = "ordinal family",
    "Counts, two-part" = "zero-inflation, hurdle and dispersion components"
  )
)

# What a family has in common across its engines, for the summary table
# of the README (one line per family).
.REG_FAMILY_NOTE <- c(
  "Survey-weighted" = "design-based SEs",
  "Population-averaged (GEE)" = "native sandwich SEs, working correlation disclosed",
  "Bayesian" = "posterior median, credible intervals, no p-values"
)

# One row per family, in the order of the registry: its engines, each
# function backticked, an engine whose class has a
# partial-proportional-odds block marked so, the engines another family
# lends it (.REG_ALSO), then the note of the family (.REG_FAMILY_NOTE).
# A markdown pipe table, for the README.
.render_family_engines_md <- function() {
  reg <- .supported_models_registry()
  stopifnot(
    all(names(.REG_ALSO) %in% reg$class),
    all(unlist(lapply(.REG_ALSO, names)) %in% reg$family),
    all(names(.REG_FAMILY_NOTE) %in% reg$family)
  )
  fams <- unique(reg$family)
  cells <- vapply(
    fams,
    function(f) {
      rows <- reg[reg$family == f, , drop = FALSE]
      own <- unlist(lapply(seq_len(nrow(rows)), function(i) {
        fns <- paste0(
          "`",
          strsplit(rows$engine[i], ", ", fixed = TRUE)[[1]],
          "`"
        )
        if (grepl(.REG_BLOCK_NPO, rows$blocks[i], fixed = TRUE)) {
          fns[length(fns)] <- paste(
            fns[length(fns)],
            "(incl. partial proportional odds)"
          )
        }
        fns
      }))
      lent <- unlist(lapply(names(.REG_ALSO), function(cl) {
        what <- .REG_ALSO[[cl]][f]
        if (is.na(what)) {
          return(NULL)
        }
        fns <- strsplit(reg$engine[reg$class == cl], ", ", fixed = TRUE)[[1]]
        paste0(paste0("`", fns, "`", collapse = ", "), " (", what, ")")
      }))
      out <- paste(unique(c(own, lent)), collapse = ", ")
      if (!is.na(.REG_FAMILY_NOTE[f])) {
        out <- paste0(out, " (", .REG_FAMILY_NOTE[[f]], ")")
      }
      out
    },
    character(1)
  )
  paste(
    c(
      "| Family | Engines |",
      "|---|---|",
      paste0("| ", fams, " | ", cells, " |")
    ),
    collapse = "\n"
  )
}


# Render the registry as a markdown pipe table for the roxygen page below
# (evaluated at document() time -- the published table can never drift from
# the registry).
# `family` keeps the rows of one family and drops the Family column, for
# the page of that family (?table_regression_mixed); a name that is not a
# family of the registry stops document(). `columns` keeps those columns
# only, for the table of the variance estimators on ?table_regression.
.render_supported_models_md <- function(family = NULL, columns = NULL) {
  reg <- .supported_models_registry()
  if (!is.null(family)) {
    if (!family %in% reg$family) {
      stop("Not a family of the registry: ", family, call. = FALSE)
    }
    reg <- reg[reg$family == family, setdiff(names(reg), "family")]
  }
  if (!is.null(columns)) {
    reg <- reg[, intersect(columns, names(reg)), drop = FALSE]
  }
  # Class and engine are code: backtick them (correct typography, and the
  # spell checker skips code spans). Engines are backticked ONE FUNCTION PER
  # SPAN: downlit auto-links an inline code span only when the whole span is
  # a single `pkg::fun()` call, so each function becomes clickable on the
  # pkgdown site.
  if ("class" %in% names(reg)) {
    reg$class <- paste0("`", reg$class, "`")
  }
  if ("engine" %in% names(reg)) {
    reg$engine <- vapply(
      strsplit(reg$engine, ", ", fixed = TRUE),
      function(fns) {
        paste0("`", fns, "`", collapse = ", ")
      },
      character(1)
    )
  }
  labels <- c(
    family = "Family",
    class = "Class",
    engine = "Engine",
    ame = "AME",
    exponentiate = "Exponentiate",
    blocks = "Blocks",
    vcov = "vcov"
  )[names(reg)]
  header <- paste0("| ", paste(labels, collapse = " | "), " |")
  sep <- paste0("|", strrep("---|", length(labels)))
  rows <- vapply(
    seq_len(nrow(reg)),
    function(i) {
      paste0("| ", paste(unlist(reg[i, ]), collapse = " | "), " |")
    },
    character(1)
  )
  paste(c(header, sep, rows), collapse = "\n")
}


# The Usage block of a family page (?table_regression_mixed, ...),
# rendered at document() time: the call with `models`, the arguments that
# matter for the family with their defaults as formals() gives them, then
# `...`. A family page carries no real \usage: codoc() compares a \usage
# with the whole signature and reports every argument left out, and
# checkDocFiles() asks for an alias of table_regression(), which belongs
# to the page of the function. Every name is checked against formals(),
# so an argument renamed or removed stops document() here and the block
# cannot drift from the signature.
.render_family_usage <- function(args) {
  f <- formals(table_regression)
  unknown <- setdiff(args, names(f))
  if (length(unknown) > 0L) {
    stop(
      "Not an argument of table_regression(): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  arg_line <- function(a) {
    paste0(
      "  ",
      a,
      " = ",
      paste(deparse(f[[a]], width.cutoff = 500L), collapse = " "),
      ","
    )
  }
  paste(
    c(
      "```r",
      "table_regression(",
      "  models,",
      vapply(args, arg_line, character(1), USE.NAMES = FALSE),
      "  ...  # the arguments of every class: see ?table_regression",
      ")",
      "```"
    ),
    collapse = "\n"
  )
}


#' Supported models of table_regression()
#'
#' @description
#' `table_regression_models()` returns the registry of model classes
#' supported by [table_regression()], one row per engine, with each class's
#' family, average-marginal-effects estimand, `exponentiate` semantics,
#' labeled table blocks and accepted `vcov` values. The same registry
#' drives the table of this page and those of the family pages, so the
#' published list cannot drift from the code.
#'
#' This page is the overview: the table of every supported class, the
#' rules all classes share, and the page of each family, which gives its
#' arguments, the values the shared arguments take for it, and what it
#' refuses.
#'
#' If a class is not listed: fit the model and call `table_regression(fit)`
#' anyway -- unsupported classes error with a clear message naming the
#' supported set. Feature requests are welcome on the issue tracker.
#'
#' @section Supported classes:
#'
#' `r spicy:::.render_supported_models_md()`
#'
#' AME: whether the average-marginal-effects columns are available, and
#' what they estimate (`per category` for ordinal models, `per outcome`
#' for multinomial models, `yes (draws)` for Bayesian fits). Exponentiate:
#' what `exponentiate = TRUE` gives. Blocks: the labeled blocks of rows the
#' class adds below its coefficients. vcov: the values of `vcov` the class
#' accepts; see *Robust SE availability by model class* in
#' [table_regression()].
#'
#' @section Pages by model family:
#' \itemize{
#'   \item [table_regression_mixed]: `lmer`, `glmer`, `glmmTMB`, `lme`
#'     and `gls` fits.
#'   \item [table_regression_survival]: `coxph`, `survreg`, `cph` and
#'     `flexsurvreg` fits.
#'   \item [table_regression_ordinal]: `polr`, `clm` and `clmm` fits.
#'   \item [table_regression_categorical]: `multinom` and `mlogit` fits.
#'   \item [table_regression_counts]: `zeroinfl` and `hurdle` fits, and
#'     the zero-inflation, hurdle and dispersion blocks of `glmmTMB`.
#'   \item [table_regression_survey]: `svyglm`, `svyolr` and `svycoxph`
#'     fits.
#'   \item [table_regression_bayesian]: `stanreg` and `brmsfit` fits.
#'   \item [table_regression_robust]: `lm_robust`, `iv_robust`, `ivreg`,
#'     `tobit`, `rq` and `fixest` fits.
#' }
#' The linear and generalized linear models are documented on
#' [table_regression()] itself. GEE fits are described below. The
#' additive, beta-regression, sample-selection and `rms` fits follow the
#' rules of [table_regression()] and the table above.
#'
#' @section Shared semantics (all classes):
#' \itemize{
#'   \item A robust `vcov` request is honored through the class's
#'     backend, or refused with an error that names the supported set; the
#'     footer names the estimator applied.
#'   \item `exponentiate = TRUE` is link-gated: it produces a labeled ratio
#'     (OR / IRR / HR / RR / MR / TR) only where the link warrants
#'     one. Identity-link fits warn and are left untouched; non-ratio
#'     links (probit, cauchit, inverse, ...) are refused with an error.
#'   \item Class-specific structure renders as labeled subordinate blocks
#'     of rows in the same table, each explained by a footer line.
#'   \item Fit statistics default to the family's field standard
#'     (`show_fit_stats` overrides; class-inappropriate tokens are rejected
#'     with a pointer to the right ones).
#'   \item Everything is available programmatically: `broom::tidy()`,
#'     `glance()`, [as_structured()], `as.data.frame()`.
#' }
#'
#' @section Population-averaged (GEE) models:
#' `geepack::geeglm()` fits are read on their own terms: the sandwich
#' standard errors the fit computed (its `std.err =` option, clustered
#' on its `id =`) are the displayed inference -- GEE is robust by
#' construction, so spicy's `HC*` and `CR*` tokens, the resamplers and
#' `cluster` are refused with a pointer to the fit options. Coefficients
#' are population-averaged (marginal) effects; the footer discloses the
#' working correlation structure with its estimated alpha. Wald z
#' inference; `exponentiate` follows the usual link gates (OR / IRR /
#' RR). Default fit statistics report the cluster structure (n,
#' `N (<id>)`, largest cluster); the quasi-likelihood information
#' criteria `"qic"` / `"qicu"` (Pan 2001) and the `"scale"`
#' (dispersion) parameter are opt-in -- there is no likelihood, so
#' AIC, pseudo-R-squared, `nested = TRUE`, and `standardized` are
#' refused. See the
#' [GEE regression tables](https://amaltawfik.github.io/spicy/articles/table-regression-gee.html)
#' article, and the population-averaged section of the
#' [Mixed-effects regression tables](https://amaltawfik.github.io/spicy/articles/table-regression-mixed.html)
#' article for the contrast with subject-specific mixed models.
#'
#' @references
#' Pan, W. (2001). Akaike's information criterion in generalized
#' estimating equations. *Biometrics*, 57(1), 120-125.
#' \doi{10.1111/j.0006-341X.2001.00120.x}
#'
#' @return A data frame with one row per supported engine and columns
#'   `family`, `class`, `engine`, `ame`, `exponentiate`, `blocks` and
#'   `vcov` (the values of `vcov` the class accepts).
#'
#' @seealso [table_regression()]; the family pages listed under *Pages by
#'   model family*; the
#'   [Supported models](https://amaltawfik.github.io/spicy/articles/table-regression-supported-models.html)
#'   and
#'   [Publication-ready regression tables](https://amaltawfik.github.io/spicy/articles/table-regression.html)
#'   articles.
#'
#' @examples
#' table_regression_models()
#'
#' # All engines of one family:
#' subset(table_regression_models(), family == "Mixed effects")
#' @export
table_regression_models <- function() {
  .supported_models_registry()
}
