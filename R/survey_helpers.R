# Survey-design support for the descriptive twins.
#
# `table_continuous_svy()` and `table_categorical_svy()` compute no
# design-based statistic of their own: every number they show comes
# from survey (Lumley), which is the package that owns linearisation
# and replicate weights. What lives in this file is the part survey
# does not do -- recognising a design object, reading the handful of
# design facts a table owes its reader, and cutting a domain the one
# way that keeps the variance right.
#
# The one rule the rest of the package must not break: a domain is cut
# with `[` / `subset()` ON THE DESIGN, never by rebuilding a
# `svydesign()` from `design$variables`. Measured on
# `svydesign(id = ~dnum, weights = ~pw, fpc = ~fpc, data = apiclus1)`
# restricted to the first ten clusters:
#
#   subset(dclus1, dom)                SE = 15.402503359507902
#   dclus1$prob[!dom] <- Inf           SE = 15.402503359507902
#   svyby(~api00, ~dom, dclus1)        SE = 15.402503359507902
#   svydesign(data = apiclus1[dom, ])  SE = 15.737900457093147   <- WRONG
#
# The three legitimate routes agree to the bit; the rebuilt design does
# not, because `svydesign()` recomputes `fpc$sampsize` from the rows it
# was handed (10 instead of the original 15).

# The version of survey the delegation map was measured against, and
# the floor `DESCRIPTION` declares. Two independent reasons, both from
# survey's own NEWS for 4.5: `svyciprop(method = "wilson")` is an
# addition of that release, and `svyby(covmat = TRUE)` "now works for
# estimators where subset() doesn't actually drop rows" -- which is
# every ordinary (non-calibrated, non-pps) design, i.e. exactly the
# route `by =` takes.
.SURVEY_MIN_VERSION <- "4.5"

# Every design class survey publishes. Deliberately wider than what the
# twins support: a `twophase` design must be RECOGNISED as a design so
# it earns the classed refusal below instead of "`data` must be a
# data.frame".
#
# `as.svrepdesign()` returns "svyrep.design", which does NOT inherit
# from "survey.design" -- the two branches of every reader here.
.is_survey_design <- function(x) {
  inherits(
    x,
    c(
      "survey.design",
      "svyrep.design",
      "twophase",
      "twophase2",
      "pps",
      "DBIsvydesign",
      "multiframe",
      "multiphase",
      "xdesign"
    )
  )
}

# The two classes the twins support: an ordinary linearised design and
# a replicate-weights design. Everything else is a design whose
# variance machinery the delegation map was not measured on, and it
# gets a classed refusal rather than a plausible wrong number.
.SURVEY_UNSUPPORTED_CLASSES <- c(
  "twophase",
  "twophase2",
  "pps",
  "DBIsvydesign",
  "multiframe",
  "multiphase",
  "xdesign"
)

.is_supported_design <- function(x) {
  if (inherits(x, .SURVEY_UNSUPPORTED_CLASSES)) {
    return(FALSE)
  }
  if (inherits(x, "svyrep.design")) {
    return(TRUE)
  }
  # A pps design is a plain "survey.design2" BY CLASS -- `pps = "brewer"`
  # leaves no class marker -- so the class test above cannot see it. The
  # `$pps` slot can: it is the logical FALSE on every with-replacement
  # design and a pps specification otherwise. Without this line a
  # without-replacement pps design would take the supported route and
  # be described by a footer that never mentions it.
  inherits(x, "survey.design2") && isFALSE(x$pps)
}

# survey is a Suggests: the twins are the only entry points that need
# it, and they say so with the package's usual missing-Suggests error.
#
# The VERSION is checked here too, because a Suggests floor is not
# resolved at install time: `DESCRIPTION` may say `>= 4.5` and the
# session still load 4.4 without a word. The two features the floor
# exists for fail quietly rather than loudly -- `svyciprop(method =
# "wilson")` is a 4.5 addition and would land in `.svy_try()` as a
# column of dashes, and `svyby(covmat = TRUE)` is only correct from 4.5
# on designs where `subset()` drops rows. A refusal naming the version
# found is the only thing that turns those into something a caller can
# act on.
.require_survey <- function(fn) {
  if (!spicy_pkg_available("survey")) {
    spicy_abort(
      c(
        sprintf("`%s()` requires the 'survey' package.", fn),
        "i" = sprintf(
          "Install it with `install.packages(\"survey\")` (>= %s).",
          .SURVEY_MIN_VERSION
        )
      ),
      class = "spicy_missing_pkg"
    )
  }
  found <- utils::packageVersion("survey")
  if (found < .SURVEY_MIN_VERSION) {
    spicy_abort(
      c(
        sprintf(
          "`%s()` requires survey >= %s; %s is installed.",
          fn,
          .SURVEY_MIN_VERSION,
          format(found)
        ),
        "i" = "Below that version `ci_method = \"wilson\"` does not exist and domain estimates are not reliable, so the table would be silently wrong rather than absent.",
        "i" = "Update with `install.packages(\"survey\")`."
      ),
      class = "spicy_unsupported"
    )
  }
  invisible(NULL)
}

# The classed refusal a design outside P1 gets.
#
# A pps design carries NO class marker -- `pps = "brewer"` leaves a
# plain `survey.design2` -- so naming `class(design)[1L]` produced a
# message that refused the very class the next line said was
# supported, and left the caller nothing to act on. The pps branch
# names the SPECIFICATION instead, which is the thing the caller wrote.
.abort_unsupported_design <- function(design, fn) {
  what <- if (
    !inherits(design, "svyrep.design") &&
      inherits(design, "survey.design2") &&
      !isFALSE(design$pps)
  ) {
    "a without-replacement pps design (`svydesign(pps = )`)"
  } else {
    sprintf("survey designs of class `%s`", class(design)[1L])
  }
  spicy_abort(
    c(
      sprintf("`%s()` does not support %s.", fn, what),
      "i" = "Supported: `survey::svydesign()` designs sampled with replacement (`pps = FALSE`, the default) and `survey::as.svrepdesign()` replicate-weight designs.",
      "i" = "For any other design, call survey directly (`survey::svymean()`, `survey::svyby()`) and open an issue saying which design you need."
    ),
    class = "spicy_unsupported"
  )
}

# The refusal a data.frame gets at a `_svy` entry point -- the mirror of
# the `spicy_wrong_regime` a design gets at the plain entry point.
.abort_needs_design <- function(fn, plain_fn) {
  spicy_abort(
    c(
      sprintf("`%s()` expects a survey design object, not a data.frame.", fn),
      "i" = "Build one first: `survey::svydesign(ids = , strata = , weights = , fpc = , data = )`, or `survey::as.svrepdesign()` for replicate weights.",
      "i" = sprintf(
        "For a data.frame with frequency weights and no design, use `%s()`.",
        plain_fn
      )
    ),
    class = "spicy_wrong_regime"
  )
}

# Design degrees of freedom. One accessor so no read path can slip back
# to `nrow() - 1` or to `confint()`'s `df = Inf` default: every interval
# and every test of the twins is referred to THIS number.
.design_degf <- function(design) {
  as.numeric(survey::degf(design))
}

# Cut a domain. `[` on the design, which survey implements twice: it
# drops the rows on an ordinary design and sets `prob = Inf` (weight
# zero, rows retained) on a calibrated or pps one. Both keep
# `fpc$sampsize` -- the field a rebuilt `svydesign()` would recompute,
# and the reason the header comment of this file exists.
.design_subset <- function(design, keep) {
  design[keep, ]
}

# The SAMPLING weights of a design, always asked for by name.
#
# `weights(design)` is not the same function on the two supported
# classes. On a replicate design it defaults to `type = "replication"`
# and returns the whole REPLICATION MATRIX -- 183 x 15 on the api JK1
# fixture, summing to 2745 against the 6194.0003242492676 the design
# actually carries. Every weighted count of the twins goes through this
# accessor, so that default can never reach one.
#
# On a `survey.design` the argument is right BY ACCIDENT:
# `weights.survey.design` has no `type` formal at all, so the request
# is swallowed by `...` and the method returns `1 / prob` whatever
# anyone asks for. Six of the nine classes this accessor can be handed
# inherit that method (survey.design, survey.design2, pps,
# DBIsvydesign, twophase / twophase2), and `weights.multiframe` ignores
# `type` too; only `svyrep.design` and `multiphase` read it. A test in
# test-survey_helpers.R pins the absence of that formal, so the day
# survey gives the method a `type` vocabulary of its own this call gets
# read again instead of changing meaning in silence.
.design_weights <- function(design) {
  stats::weights(design, type = "sampling")
}

# How many rows of the ANALYTIC sample carry a negative weight.
#
# Linear calibration produces them -- that is what
# `survey::calibrate(bounds = )` exists to prevent -- and they are rows
# the sampler drew, so nothing hides them (see `.svy_var_stats()`).
# But they stop the weighted mean from being a convex combination, so
# it can land outside the observed range, and they let a domain
# variance come out negative. Both are facts a reader is owed, and the
# footer says them only when the fact is there (decision 36 / ARB-3).
#
# Counted on non-zero weights, the same predicate every count of the
# twins uses: a row at weight zero is one `[` retained on a calibrated
# design, not an observation.
.design_negative_weights <- function(design) {
  w <- .design_weights(design)
  sum(!is.na(w) & w < 0)
}

# The refusal predicate itself, in one place for both twins: does this
# set of weights change sign? Applied per VARIABLE, on the weights of
# that variable's complete-case domain -- a variable whose missing
# values happen to cover the negatively weighted rows is testable, and
# is tested (decision 36 / ARB-2).
.weights_go_negative <- function(w) {
  any(!is.na(w) & w < 0)
}

# Which of a table's group comparisons the negative weights refused,
# and therefore which sentence the footer owes:
#   "none" -- nothing was refused. A comparison missing for another
#             reason (fewer than two observed groups, a survey error)
#             is not the weights' doing and is not explained by them.
#   "all"  -- every comparison that was attempted was refused. The flat
#             sentence, and no method line above it.
#   "some" -- a mixed table. Some variables were tested and their
#             p-values are printed, so the refusal has to name who it
#             applies to instead of speaking for the whole table.
.design_refusal_regime <- function(n_refused, n_attempted) {
  if (n_refused <= 0L) {
    "none"
  } else if (n_refused >= n_attempted) {
    "all"
  } else {
    "some"
  }
}

# The one sentence-block the fact earns, or nothing at all. `n_obs` is
# the count the footer has just announced, so the two agree. The
# refusal clause is appended rather than said separately: one fact, one
# block (decision 36 / ARB-3). `test_refused` is a regime from
# `.design_refusal_regime()`.
.design_negative_weights_note <- function(k, n_obs, test_refused = "none") {
  if (k <= 0L) {
    return(character(0))
  }
  txt <- spicy_fmt("note_negative_weights", as.integer(k), as.integer(n_obs))
  clause <- switch(
    test_refused,
    all = spicy_str("note_negative_weights_no_test"),
    some = spicy_str("note_negative_weights_no_test_some"),
    NULL
  )
  if (!is.null(clause)) {
    txt <- paste(txt, clause)
  }
  txt
}

# The classed half of the refusal (decision 36 / ARB-2): a note is read
# by whoever prints the table, and a condition is what a script can
# catch. ONE condition per table call, not one per variable -- the fact
# is a property of the design, and the footer says how far it reached.
# The message is the registry sentence the footer carries -- the SAME
# one per regime, so the two cannot say different things: a mixed table
# warns with the scoped sentence, not the flat one.
.warn_negative_weights_no_test <- function(test_refused) {
  msg <- switch(
    test_refused,
    all = spicy_str("note_negative_weights_no_test"),
    some = spicy_str("note_negative_weights_no_test_some"),
    NULL
  )
  if (is.null(msg)) {
    return(invisible(FALSE))
  }
  spicy_warn(
    msg,
    class = c("spicy_negative_weights_no_test", "spicy_undefined_stat")
  )
  invisible(TRUE)
}

# The number of observations a design fit actually used.
#
# Each class misreports it in its own direction, so none of them is asked
# through `nobs()` blindly:
#   * `nobs(svyolr)` is the SUM OF THE WEIGHTS -- survey sets
#     `nobs = sum(wt)` and ships no method to correct it -- so the count
#     comes from the fitted-probability matrix, one row per observation.
#     Not from `model.frame()`: survey's `model.frame.svyolr()`
#     re-evaluates the `design` argument by name from the call, in the
#     formula's environment, and on a REPLICATE design that environment
#     is survey's own -- the user's design object is not there and the
#     call fails.
#   * `nobs(svycoxph)` is the number of EVENTS (survival's deliberate
#     convention); `fit$n` is the subject count.
#   * `nobs(svyglm)` is the row count, which is what we want.
.design_fit_n_obs <- function(fit) {
  n <- if (inherits(fit, "svyolr")) {
    NROW(fit$fitted.values)
  } else if (inherits(fit, "coxph")) {
    fit$n
  } else {
    tryCatch(stats::nobs(fit), error = function(e) NULL)
  }
  if (is.null(n) || length(n) != 1L || !is.finite(n)) {
    return(NA_integer_) # nocov
  }
  as.integer(n)
}

# The design object restricted to the rows the FIT actually used.
#
# The four design-fitting functions do not agree on what they attach.
# `svyglm()` and `svyolr()` drop the incomplete rows before storing the
# design, so `fit$survey.design` already IS the analytic sample;
# `svycoxph()` stores the design first and reduces a local copy
# afterwards, so it hands back the COMPLETE design -- 200 rows for a fit
# on 180. Reading the weights or the degrees of freedom off that object
# gives a number that is plausible, wrong, and about the wrong
# population (6194 against 5487.27 on the apistrat fixture with 20
# missing `ell`).
#
# So the alignment is checked, never assumed, against the row count the
# caller knows (`n_obs`, the analytic n):
#   * the attached design already has `n_obs` rows -> take it as it is
#     (svyglm / svyolr / svrepglm), and never re-drop, which would
#     remove the missing rows a SECOND time (180 -> 160);
#   * a CALIBRATED design keeps its rows and sets their weight to zero
#     (`[.survey.design2` on a calibrated object sets `prob = Inf`), so
#     the count that matters is the number of non-zero sampling weights;
#   * otherwise drop the fit's `na.action` rows and re-check;
#   * if the result still does not line up, NULL -- the callers turn
#     that into `NA`, never into a plausible wrong number.
.design_analytic <- function(fit, n_obs) {
  des <- tryCatch(fit$survey.design, error = function(e) NULL)
  if (is.null(des) || !.is_survey_design(des)) {
    return(NULL)
  }
  if (.design_aligns(des, n_obs)) {
    return(des)
  }
  nas <- tryCatch(stats::na.action(fit), error = function(e) NULL)
  if (length(nas) == 0L) {
    return(NULL)
  }
  out <- tryCatch(.design_subset(des, -nas), error = function(e) NULL)
  if (is.null(out) || !.design_aligns(out, n_obs)) {
    return(NULL)
  }
  out
}

# Does this design describe exactly `n_obs` observations? Either by row
# count, or -- on a calibrated design, whose subsetting zeroes weights
# instead of dropping rows -- by the count of non-zero sampling weights.
.design_aligns <- function(des, n_obs) {
  # `identical()` on both sides, and `na.rm` on the count: a count that
  # is not one -- NA, empty, longer than one -- then matches nothing at
  # all, instead of matching an NA count and declaring the design
  # aligned with a sample size nobody could name.
  n_obs <- suppressWarnings(as.integer(n_obs))
  if (identical(as.integer(nrow(des)), n_obs)) {
    return(TRUE)
  }
  w <- tryCatch(.design_weights(des), error = function(e) NULL)
  !is.null(w) && identical(sum(w != 0, na.rm = TRUE), n_obs)
}

# The sampling weights of the analytic sample, one per estimation row,
# or NULL when they cannot be aligned. On a calibrated design the
# zero-weight rows are the dropped ones, so removing them restores the
# estimation-row order.
.design_analytic_weights <- function(fit, n_obs) {
  des <- .design_analytic(fit, n_obs)
  if (is.null(des)) {
    return(NULL)
  }
  w <- tryCatch(as.numeric(.design_weights(des)), error = function(e) NULL)
  if (is.null(w) || length(w) == 0L) {
    return(NULL)
  }
  if (length(w) != n_obs) {
    w <- w[w != 0]
  }
  if (length(w) != n_obs || !all(is.finite(w))) {
    return(NULL)
  }
  w
}

# Sum of the SAMPLING weights over the analytic sample: the population
# the model describes. `NA_real_` when the design is detached or cannot
# be aligned -- a weighted n is either the right population or absent.
.design_weighted_n <- function(fit, n_obs) {
  w <- .design_analytic_weights(fit, n_obs)
  if (is.null(w)) {
    return(NA_real_)
  }
  sum(w)
}

# The residual degrees of freedom survey writes ON THE FIT, read, never
# re-derived.
#
# The six design-fitting engines do not agree on the expression, and the
# differences are not principled -- `svyolr` has no `+ 1`, `svycoxph`
# has one with no intercept to cancel (a copy from `svyglm`, where it
# does cancel):
#
#   svyglm.survey.design    degf(design) + 1 - length(coef(g))   df.residual
#   svyglm.svyrep.design    idem                                 df.residual
#   svyolr.survey.design2   degf(design) - length(beta)          df.residual
#   svyolr.svyrep.design    idem                                 df.residual
#   svycoxph.survey.design  degf(design) - length(coef(g)) + 1   degf.resid
#   svycoxph.svyrep.design  degf(design) + 1 - length(coef())    degf.residual
#
# So the rule is one of READING: whatever survey posted is what
# `regTermTest()` uses as its denominator, and a table whose row p and
# omnibus p had different denominators would be indefensible.
# Harmonising the formulas across classes is a regression, not a
# tidy-up.
#
# Two slot names, one per Cox engine, read with `[[` and both tried by
# name. On survey 4.5 that is a precaution rather than a repair: `$`
# would partial-match `degf.resid` to `degf.residual` and land on the
# same number. The precaution is what survives the prefix ceasing to be
# unique -- a slot that merely shares it answers `$` and would be
# published as the residual df, and two such slots make `$` ambiguous
# and return NULL. Nothing in survey promises otherwise, and the two
# cases are under test.
#
# Never a silent fall back to `Inf`: that would publish normal p-values
# and intervals under a footer declaring a t.
.design_model_df <- function(fit) {
  df <- fit[["degf.resid"]]
  if (is.null(df)) {
    df <- fit[["degf.residual"]]
  }
  if (is.null(df)) {
    df <- tryCatch(stats::df.residual(fit), error = function(e) NULL)
  }
  if (is.null(df) || length(df) != 1L || !is.finite(df) || df <= 0) {
    spicy_abort(
      c(
        sprintf(
          "The residual degrees of freedom of this `%s` fit could not be read.",
          class(fit)[1L]
        ),
        "i" = paste0(
          "survey stores them in `df.residual`, `degf.resid` or ",
          "`degf.residual` depending on the engine; none of the three ",
          "held a usable value."
        )
      ),
      class = "spicy_internal"
    )
  }
  as.numeric(df)
}

# The replicate schemes survey names in `design$type`. `"other"` is a
# legal value of `svrepdesign(type = )` and is deliberately absent: it
# identifies nothing, so the label drops the parenthesis rather than
# printing a word that means "unspecified". `"Fay"` is in survey's own
# vocabulary but `as.svrepdesign(type = "Fay")` stores `"BRR"` (Fay's
# method is BRR with a shrinkage factor); it is listed so a design built
# another way still resolves.
.SVYREP_TYPES <- c(
  "BRR",
  "Fay",
  "JK1",
  "JKn",
  "bootstrap",
  "subbootstrap",
  "mrbbootstrap"
)

# The variance label of a replicate design, read off its scheme -- bare
# when the scheme is absent or `"other"`, a legal `svrepdesign(type = )`
# value that names nothing.
#
# One rule with two callers, on purpose: the regression footer and the
# descriptive twins print the SAME sentence for the same design, and a
# second copy of this test is how the two drifted apart in the first
# place.
.design_replicate_label <- function(rep_type) {
  type <- as.character(rep_type %||% NA_character_)
  if (length(type) != 1L || is.na(type) || !type %in% .SVYREP_TYPES) {
    return(spicy_str("note_vcov_design_replicate_bare"))
  }
  spicy_fmt("note_vcov_design_replicate", type)
}

# The variance estimator a design fit actually uses, as the footer names
# it. Indexed on the MECHANISM, which is the only thing the label is
# about:
#   * a two-phase design first, because `twophase2` also inherits
#     `survey.design` and would otherwise be called linearised;
#   * a replicate design by its scheme, or bare when the scheme is
#     absent or `"other"`;
#   * everything else built by `svydesign()` -- including calibrated,
#     post-stratified and without-replacement pps designs, whose
#     `ppsvar()` IS a linearisation -- by Taylor linearisation;
#   * a detached or unknown design: "Design-based", never `class(des)`.
#     An R class name is an implementation detail, not a variance
#     estimator, and it is not something a reader of a table can act on.
.design_vcov_label <- function(fit) {
  des <- tryCatch(fit$survey.design, error = function(e) NULL)
  if (is.null(des)) {
    return(spicy_str("note_vcov_design_bare"))
  }
  if (inherits(des, c("twophase", "twophase2"))) {
    return(spicy_str("note_vcov_design_twophase"))
  }
  if (inherits(des, "svyrep.design")) {
    return(.design_replicate_label(des$type))
  }
  if (inherits(des, "survey.design")) {
    return(spicy_str("note_vcov_design_taylor"))
  }
  spicy_str("note_vcov_design_bare")
}

# The display domains of a `by =` variable, and the vector that keys
# them.
#
# Factors keep their DECLARED order, anything else takes the order of
# first appearance -- the family convention `table_categorical()` /
# `cross_tab()` / `table_outcome()` share. An EMPTY declared level is
# dropped: a domain with no observation has no degrees of freedom and
# every one of its cells would be the undefined dash.
#
# With `drop_na = FALSE` the missing values become a domain of their
# own. That is legitimate under a design in a way a missing GROUP
# LABEL is not a legitimate level: `design[is.na(g), ]` is an ordinary
# subpopulation, with its own PSU, its own strata and therefore its own
# degrees of freedom, which survey computes exactly as it does for any
# other. The label is guarded against a collision with a real value the
# way the rest of the family guards it -- the scan covers declared
# levels as well as observed ones.
.svy_by_levels <- function(g, drop_na) {
  declared <- if (is.factor(g)) levels(g) else unique(g[!is.na(g)])
  declared <- as.character(declared)
  observed <- as.character(g[!is.na(g)])
  declared <- declared[declared %in% observed]
  n_na <- sum(is.na(g))
  values <- as.character(g)
  missing_label <- NA_character_
  if (!drop_na && n_na > 0L) {
    missing_label <- spicy_str("row_missing_level")
    seen <- unique(c(observed, declared))
    idx <- 1L
    while (missing_label %in% seen) {
      missing_label <- spicy_fmt("row_missing_level_dedup", idx)
      idx <- idx + 1L
    }
    declared <- c(declared, missing_label)
    values[is.na(values)] <- missing_label
  }
  list(
    levels = declared,
    values = values,
    missing_label = missing_label,
    n_na = n_na
  )
}

# The design facts a table discloses, read through PUBLIC accessors
# only: `survey:::is.calibrated()` / `survey:::is.pps()` say the same
# things, but a `:::` call is not something to ship to CRAN.
#
# Verified on the api fixtures:
#   dstrat  strata TRUE (3, "stype") 1 stage 200 PSU fpc  degf 197 n 200
#   dclus1  strata FALSE             1 stage  15 PSU fpc  degf  14 n 183
#   dclus2  strata FALSE             2 stages 40 PSU fpc  degf  39 n 126
#   rclus1  JK1, 15 replicates, mse FALSE                degf  14
.design_meta <- function(design) {
  if (inherits(design, "svyrep.design")) {
    return(list(
      kind = "replicate",
      rep_type = as.character(design$type %||% NA_character_),
      n_rep = ncol(as.matrix(design$repweights)),
      mse = isTRUE(design$mse),
      degf = .design_degf(design),
      n_obs = nrow(design),
      sum_weights = sum(.design_weights(design))
    ))
  }
  has_strata <- isTRUE(design$has.strata)
  list(
    kind = "linearized",
    n_stages = ncol(design$cluster),
    n_psu = length(unique(design$cluster[, 1L])),
    has_strata = has_strata,
    n_strata = if (has_strata) length(unique(design$strata[, 1L])) else 0L,
    strata_name = if (has_strata) {
      colnames(design$strata)[1L]
    } else {
      NA_character_
    },
    psu_name = colnames(design$cluster)[1L],
    has_fpc = !is.null(design$fpc$popsize),
    calibrated = !is.null(design$postStrata),
    pps = !isFALSE(design$pps),
    degf = .design_degf(design),
    n_obs = nrow(design),
    sum_weights = sum(.design_weights(design))
  )
}

# `.design_meta()` for the designs it was written for, NULL otherwise.
#
# It reads `design$cluster` / `design$strata` / `design$fpc` directly, so
# a design that does not carry them -- a two-phase design, whose PSU
# variable is empty -- fails inside it ("argument is of length zero"),
# not at its door. A `svyglm` on a two-phase design is legal and renders
# a table today, so the caller has to be able to ask without risking
# that. Everything survey builds through `svydesign()` (including
# calibrated, post-stratified and without-replacement pps designs) and
# every replicate design is covered.
.design_meta_or_null <- function(design) {
  if (
    is.null(design) ||
      !inherits(design, c("svyrep.design", "survey.design2"))
  ) {
    return(NULL)
  }
  .design_meta(design)
}

# The clause naming the sampling scheme, e.g.
# "stratified (stype), 200 PSU, with finite population correction" or
# "replicate weights (JK1), 15 replicates".
.design_scheme_parts <- function(meta) {
  if (identical(meta$kind, "replicate")) {
    return(spicy_fmt(
      "note_design_replicate",
      meta$rep_type,
      as.integer(meta$n_rep)
    ))
  }
  parts <- character(0)
  if (isTRUE(meta$has_strata)) {
    parts <- c(parts, spicy_fmt("note_design_stratified", meta$strata_name))
  }
  # One PSU per row is a design with no clustering at all: naming a
  # cluster variable there would invent a stage the user did not
  # declare.
  clustered <- meta$n_psu < meta$n_obs
  if (clustered) {
    parts <- c(parts, spicy_fmt("note_design_cluster", meta$psu_name))
  }
  if (length(parts) == 0L) {
    parts <- spicy_str("note_design_srs")
  }
  if (meta$n_stages > 1L) {
    parts <- c(
      parts,
      spicy_fmt("note_design_stages", as.integer(meta$n_stages))
    )
  }
  if (clustered) {
    parts <- c(parts, spicy_fmt("note_design_psu", as.integer(meta$n_psu)))
  }
  if (isTRUE(meta$has_fpc)) {
    parts <- c(parts, spicy_str("note_design_fpc"))
  }
  if (isTRUE(meta$calibrated)) {
    parts <- c(parts, spicy_str("note_design_calibrated"))
  }
  paste(parts, collapse = ", ")
}

# The three sentences every `_svy` table owes its reader: what the
# design is and how many degrees of freedom it carries, where the
# standard errors come from, and that the intervals and tests use the
# design df rather than the normal approximation `confint()` would
# take by default.
#
# `degf_range` is the per-group span of a `by =` table, whose domains
# each have their own df (survey recomputes it on the retained PSU and
# strata; on the api cluster design a ten-cluster domain drops from 14
# to 9). NULL for a table with one df.
.design_note_lines <- function(meta, degf_range = NULL) {
  scheme <- .design_scheme_parts(meta)
  df_clause <- if (
    !is.null(degf_range) && degf_range[[1L]] != degf_range[[2L]]
  ) {
    spicy_fmt(
      "note_design_degf_varying",
      as.integer(degf_range[[1L]]),
      as.integer(degf_range[[2L]])
    )
  } else {
    spicy_fmt("note_design_degf", as.integer(meta$degf))
  }
  # The variance sentence is the regression footer's, built from the
  # same template and the same labels: one table calling it "Standard
  # errors: Taylor linearisation (survey)." while its regression
  # neighbour called it "Std. errors: Design-based (Taylor
  # linearisation)." made the reader work out that the two were the
  # same fact.
  vcov_label <- if (identical(meta$kind, "replicate")) {
    .design_replicate_label(meta$rep_type)
  } else {
    spicy_str("note_vcov_design_taylor")
  }
  c(
    spicy_fmt("note_design_line", scheme, df_clause),
    spicy_fmt("note_std_errors_single", vcov_label),
    spicy_str("note_design_df_used")
  )
}

# The sample-size sentence. Both numbers, because neither alone is
# enough: the unweighted count is the robustness information, the sum
# of weights is the population the estimates describe (decision 28).
.design_n_note <- function(
  n_obs,
  sum_weights,
  digits = 0L,
  decimal_mark = "."
) {
  spicy_fmt(
    "note_design_n",
    format_number(n_obs, digits = 0L, decimal_mark = decimal_mark),
    format_number(sum_weights, digits = digits, decimal_mark = decimal_mark)
  )
}
