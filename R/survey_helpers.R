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
  invisible(NULL)
}

# The classed refusal a design class outside P1 gets.
.abort_unsupported_design <- function(design, fn) {
  spicy_abort(
    c(
      sprintf(
        "`%s()` does not support survey designs of class `%s`.",
        fn,
        class(design)[1L]
      ),
      "i" = "Supported: `survey::svydesign()` designs and `survey::as.svrepdesign()` replicate-weight designs.",
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
# classes: on a linearised design it returns the sampling weights,
# while on a replicate design it defaults to `type = "analysis"` and
# returns the FIRST REPLICATE's weights -- on the api JK1 fixture,
# 2745 against the 6194.0003242492676 the design actually carries, with
# the dropped cluster sitting at weight zero. Every weighted count of
# the twins goes through this accessor, so the bare default can never
# reach one.
.design_weights <- function(design) {
  stats::weights(design, type = "sampling")
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
  c(
    spicy_fmt("note_design_line", scheme, df_clause),
    if (identical(meta$kind, "replicate")) {
      spicy_str("note_se_replicate")
    } else {
      spicy_str("note_se_taylor")
    },
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
