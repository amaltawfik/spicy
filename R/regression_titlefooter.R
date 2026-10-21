# Frame-aware sibling of build_regression_title(). Reads from each
# frame's info$dv (DV name) and info$extras$title_prefix (family-aware
# label) instead of the legacy extract's $outcome and $title_prefix
# fields. Logic is identical -- byte-equivalence is tested in
# tests/testthat/test-renderer_migration_title.R.
#
# The title was the first renderer migrated to consume frames directly
# (Phase 0c sub-step C1); the rest followed. Every renderer -- footer
# blocks, body builder, alignment, nested LRT -- now reads frames, and
# the legacy extract shape and its .frame_to_legacy_extract() adapter
# no longer exist.
# Decision 42: the translated display form of an engine's title_prefix,
# or NULL when the current language is English or has no bridge entry --
# in which case the caller keeps the WHOLE English title (coherent or
# nothing, never a half-translated head). Engine suffixes are proper
# nouns: detached before the lookup, reattached verbatim. This helper
# and the bridge it reads disappear together when 251-C makes the
# prefix a registry key.
.title_prefix_display <- function(prefix) {
  # Defensive: no engine is known to emit a non-string prefix, but an
  # NA here would turn the `endsWith()` below into an error under a
  # translating language while English renders it -- the fallback must
  # never be LESS robust than the path it falls back from.
  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix)) {
    return(NULL)
  }
  lang <- .spicy_language_option(getOption("spicy.language", "en"))
  bridge <- .spicy_title_prefix_table(lang)
  if (is.null(bridge)) {
    return(NULL)
  }
  suffix <- ""
  for (s in c(" (glmmTMB)", " (nlme)", " (stanreg)", " (brmsfit)")) {
    if (endsWith(prefix, s)) {
      suffix <- s
      prefix <- substr(prefix, 1L, nchar(prefix) - nchar(s))
      break
    }
  }
  # Single-bracket on purpose: `[[` on a named vector ERRORS on a
  # missing name, `[` answers NA -- and NA here means "no entry".
  hit <- unname(bridge[prefix])
  if (is.na(hit)) {
    return(NULL)
  }
  paste0(hit, suffix)
}

build_regression_title_from_frames <- function(frames, nested = FALSE) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(spicy_str("title_regression_fallback"))
  }
  # The outcome's NAME tells whether the models share it; the title
  # prints its DISPLAY, the name or the label `labels` gives it (decision
  # 54), which table_regression() stores as `info$dv_display`.
  outcomes <- vapply(
    frames,
    function(f) {
      f$info$dv %||% NA_character_
    },
    character(1)
  )
  shown <- vapply(
    frames,
    function(f) {
      f$info$dv_display %||% f$info$dv %||% NA_character_
    },
    character(1)
  )
  shown <- shown[!is.na(outcomes)]
  outcomes <- outcomes[!is.na(outcomes)]
  n <- length(frames)

  prefixes <- vapply(
    frames,
    function(f) {
      f$info$extras$title_prefix %||% "Regression"
    },
    character(1)
  )
  mixed <- length(unique(prefixes)) > 1L
  prefix <- if (mixed) "Regression" else prefixes[1L]

  # Decision 42: a translatable prefix routes through the registry
  # templates; an untranslatable one falls through to the English arm
  # below (coherent or nothing). A MIXED-family table never translates:
  # its type footer is the frozen English "Model k: ..." list, and a
  # French title over an English footer is the hybrid the rule forbids
  # -- even though the fallback prefix "Regression" has a bridge entry.
  disp <- if (mixed) NULL else .title_prefix_display(prefix)
  if (!is.null(disp)) {
    if (n == 1L) {
      if (length(outcomes) == 0L) {
        return(disp)
      }
      return(spicy_fmt("title_regression_single", disp, shown[1]))
    }
    if (isTRUE(nested)) {
      # The French template carries its own qualifier ("%s -- modeles
      # emboites"), so the prefix keeps its capital -- no
      # lowercase_first() on this branch.
      return(spicy_fmt("title_regression_hierarchical", disp, shown[1]))
    }
    if (length(unique(outcomes)) == 1L) {
      return(spicy_fmt("title_regression_comparison_dv", disp, shown[1]))
    }
    return(spicy_fmt("title_regression_comparison", disp))
  }

  # The English arm resolves through `.spicy_fmt_en()`: the language
  # layer is bypassed (these templates must never come out half-French)
  # but a `spicy.labels` override still reaches them -- the registry
  # advertises the keys, so they cannot be dead letters in English.
  if (n == 1L) {
    if (length(outcomes) == 0L) {
      return(prefix)
    }
    return(.spicy_fmt_en("title_regression_single", prefix, shown[1]))
  }

  identical_dv <- length(unique(outcomes)) == 1L

  if (isTRUE(nested)) {
    # lowercase_first() keeps proper-noun titles intact ("Hierarchical
    # Cox proportional hazards regression", not "Hierarchical cox ...").
    return(.spicy_fmt_en(
      "title_regression_hierarchical",
      lowercase_first(prefix),
      shown[1]
    ))
  }
  if (identical_dv) {
    return(.spicy_fmt_en("title_regression_comparison_dv", prefix, shown[1]))
  }
  .spicy_fmt_en("title_regression_comparison", prefix)
}


# The footer dispatcher: reads frames and calls each builder's
# _from_frames sibling. build_stars_footer_block() and
# build_nested_footer_block() never took an extracts arg -- they are
# reused unchanged.
#
# This replaced a legacy build_regression_footer() that consumed the
# extract shape; both that dispatcher and the shape are gone, and the
# orchestrator calls this one.
build_regression_footer_from_frames <- function(
  frames,
  standardized = "none",
  p_adjust = "none",
  stars = FALSE,
  nested = FALSE,
  show_columns = character(0),
  show_fit_stats = character(0),
  reference_style = "row",
  show_re = TRUE,
  re_scale = "sd",
  re_columns = c("est", "se", "ci"),
  re_test = "none",
  displayed_parent_vars = NULL,
  frames_display = frames,
  decimal_mark = ".",
  model_labels = NULL
) {
  # Per-model footer lines cite the label the column spanners display,
  # so a table headed "Baseline / Adjusted" is not footnoted "Model 1 /
  # Model 2" -- names that appear nowhere in what the reader sees. The
  # label is stamped on each frame once, here, and every theme builder
  # reads it back through .model_line(); without user labels the stamp
  # is absent and .model_line() falls back to the historical
  # "Model %d", byte for byte.
  if (!is.null(model_labels) && is.list(frames)) {
    for (i in seq_len(min(length(frames), length(model_labels)))) {
      frames[[i]]$info$model_label <- model_labels[[i]]
    }
    if (is.list(frames_display)) {
      for (i in seq_len(min(length(frames_display), length(model_labels)))) {
        frames_display[[i]]$info$model_label <- model_labels[[i]]
      }
    }
  }
  # `frames_display`: the frames whose coefficient labels match the
  # DISPLAYED body. Only the multinomial columns layout passes a
  # different set (the exploded per-category pseudo-frames): the two
  # themes that read coefficient labels -- reference categories and
  # polynomial trends -- must see the bare display labels, or they
  # print category-prefixed pseudo-levels ("sex = Student: Female")
  # once per equation. Every model-facing theme keeps reading
  # `frames` (one entry per MODEL), so counts and dedupes stay
  # single-model ("Multinomial logistic regression.", not "models.").
  #
  # The cut-points are listed in the note (values of the model) unless
  # the table shows them as rows, which the note then glosses.
  thresholds <- build_ordinal_thresholds_footer_block_from_frames(
    frames,
    decimal_mark = decimal_mark
  )
  thresholds_as_rows <- any(vapply(
    frames,
    function(f) isTRUE(any(f$coefs$is_threshold)),
    logical(1)
  ))
  # The note of every table of the family (`.compose_note()`): the
  # general note, one paragraph whose themes come in four kinds -- each
  # kind keeping the order its themes had -- then the stars legend, the
  # probability note, on its own line.
  .compose_note(
    # (a) What the numbers are: the model, its design, its working
    # correlation, its distribution, the scale of its coefficients, the
    # state of the fit, a statistic it does not report, and what its
    # coefficients are read against.
    what = .footer_paragraph(
      build_regression_type_footer_block_from_frames(frames),
      # Before the variance label (c), so a design table reads "what the
      # design is" then "where the standard errors come from" -- the
      # order the descriptive twins use.
      build_design_footer_block_from_frames(frames),
      build_gee_footer_block_from_frames(frames, decimal_mark = decimal_mark),
      build_survival_footer_block_from_frames(
        frames,
        decimal_mark = decimal_mark
      ),
      if (!thresholds_as_rows) thresholds,
      build_exponentiate_footer_block_from_frames(frames, show_columns),
      build_singular_footer_block_from_frames(frames),
      build_icc_omitted_footer_block_from_frames(frames, show_fit_stats),
      build_reference_outcome_footer_block_from_frames(frames),
      build_reference_alternative_footer_block_from_frames(frames),
      build_convergence_footer_block_from_frames(
        frames,
        decimal_mark = decimal_mark
      ),
      build_reference_categories_footer_block_from_frames(
        frames_display,
        reference_style
      )
    ),
    # (b) The missing data: the cases the univariable models are fit on.
    missing = .footer_paragraph(
      build_uv_disclosure_footer_block_from_frames(frames)
    ),
    # (c) The inference: the standard errors, the intervals, the tests,
    # the adjustment, the predictive accuracy.
    inference = .footer_paragraph(
      build_vcov_footer_block_from_frames(frames),
      build_ci_method_footer_block_from_frames(
        frames,
        show_columns,
        decimal_mark = decimal_mark
      ),
      build_mixed_inference_footer_block_from_frames(frames),
      build_nested_ml_refit_footer_block_from_frames(
        frames,
        nested,
        show_fit_stats
      ),
      build_random_effects_footer_block_from_frames(
        frames,
        show_re = show_re,
        re_scale = re_scale,
        re_columns = re_columns,
        re_test = re_test,
        decimal_mark = decimal_mark
      ),
      build_ame_satterthwaite_footer_block_from_frames(frames, show_columns),
      build_p_adjust_footer_block_from_frames(frames, p_adjust),
      build_re_se_skipped_footer_block_from_frames(frames),
      build_re_profile_footer_block_from_frames(frames),
      build_loo_footer_block_from_frames(frames, decimal_mark = decimal_mark),
      build_nested_footer_block(nested)
    ),
    # (d) The definitions: the glosses of the estimand columns and of the
    # subordinate blocks, the scope of a standardized coefficient, the
    # labels of polynomial trends.
    definitions = .footer_paragraph(
      build_survival_estimand_footer_block_from_frames(frames),
      if (thresholds_as_rows) thresholds,
      build_scale_effects_footer_block_from_frames(frames),
      build_component_blocks_footer_block_from_frames(frames),
      build_standardized_caveat_footer_block_from_frames(frames, standardized),
      build_polynomial_contrasts_footer_block_from_frames(
        frames_display,
        displayed_parent_vars = displayed_parent_vars
      )
    ),
    # The abbreviations of the headers and of the stub, where the rule in
    # force puts them.
    abbreviations = build_abbreviations_footer_block_from_frames(
      show_columns,
      frames,
      standardized,
      show_fit_stats = show_fit_stats,
      re_scale = re_scale
    ),
    probability = build_stars_footer_block(stars, decimal_mark)
  )
}

# The sentences of footer themes, ready for the paragraph of the general
# note: the empty themes dropped, and a theme that gives one sentence per
# model ("Model 1: ...." on a line of its own) run into the paragraph,
# its sentences joined by a space.
.footer_paragraph <- function(...) {
  themes <- unlist(list(...), use.names = FALSE)
  themes <- themes[!is.na(themes) & nzchar(themes)]
  gsub("\n[ ]*", " ", themes)
}

capitalize_first <- function(s) {
  if (!length(s) || !nzchar(s)) {
    return(s)
  }
  paste0(toupper(substr(s, 1L, 1L)), substring(s, 2L))
}

# The reference a footer prints for model `idx`: the spanner label when
# the dispatcher stamped one (user-supplied `model_labels` / list
# names), the historical "Model <idx>" otherwise.
.model_ref <- function(frames, idx) {
  frames[[idx]]$info$model_label %||% spicy_fmt("label_model_name", idx)
}

# One per-model footer sentence: "<label>: <text>". The default label IS
# "Model <idx>", so tables without custom labels render byte-identical
# to the historical "Model %d: %s" template.
.model_line <- function(frames, idx, text) {
  spicy_fmt("note_model_line", .model_ref(frames, idx), text)
}


# Frame-aware sibling of build_regression_type_footer_block().
# Reads from frame$info$extras$title_prefix instead of extract$title_prefix.
# Logic and output are identical; byte-equivalence is tested in
# tests/testthat/test-renderer_migration_footer_simple.R.
#
# The footer-block builders were migrated one at a time, each with its
# own byte-equivalence gate; every one now has a _from_frames sibling
# and the dispatcher that consumed legacy extracts is gone.
build_regression_type_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  # Phase 7b: preserve the engine-supplied acronym casing. The legacy
  # tolower() clobbered "(GAM)" / "(M-estimator)" / "(OLS)" that
  # per-class methods set deliberately. For sentence-leading positions
  # we apply capitalize_first(); for mid-sentence positions (after
  # "Model 1:") we only lowercase the FIRST character so the title
  # reads naturally without losing acronyms further in.
  types <- vapply(
    frames,
    function(f) {
      f$info$extras$title_prefix %||% "Regression"
    },
    character(1)
  )
  # Decision 42: the single-model and same-family arms translate (the
  # prefix through the bridge, the plural through its whole-sentence
  # key). The mixed-family arm below embeds the frozen "Model %d:"
  # labels and stays English wholesale -- coherent or nothing --
  # including its line TEMPLATE and its separator: `.spicy_fmt_en()`
  # and `.spicy_str_en()` bypass the language so a French NBSP colon or
  # semicolon never lands inside the English fallback sentence. The
  # plural English arm routes through the registry too, so a
  # `spicy.labels` override of `note_type_models` works in English, as
  # `spicy_labels()` advertises.
  disp <- .title_prefix_display(types[1])
  if (length(types) == 1L) {
    if (!is.null(disp)) {
      return(paste0(capitalize_first(disp), "."))
    }
    return(paste0(capitalize_first(types[1]), "."))
  }
  if (length(unique(types)) == 1L) {
    if (!is.null(disp)) {
      return(spicy_fmt("note_type_models", lowercase_first(disp)))
    }
    return(.spicy_fmt_en("note_type_models", capitalize_first(types[1])))
  }
  per <- vapply(
    seq_along(types),
    function(i) {
      .spicy_fmt_en(
        "note_model_line",
        .model_ref(frames, i),
        lowercase_first(types[i])
      )
    },
    character(1)
  )
  paste0(paste(per, collapse = .spicy_str_en("note_type_models_sep")), ".")
}


# Lowercase only the first character of a string (the inverse of
# capitalize_first()). Used to make a title read naturally in
# mid-sentence position ("Model 1: linear regression") while
# preserving any acronyms further in the string. Titles that START
# with a proper noun (Cox, Poisson, Weibull, ...) keep their capital:
# "Hierarchical Cox proportional hazards regression", "Model 1:
# Poisson regression" -- lowercasing a surname is a typo, not style.
lowercase_first <- function(s) {
  if (!length(s) || !nzchar(s)) {
    return(s)
  }
  proper <- c("Cox", "Poisson", "Weibull", "Bayesian", "Tweedie")
  first_word <- sub("[ -].*$", "", s)
  if (first_word %in% proper) {
    return(s)
  }
  paste0(tolower(substr(s, 1L, 1L)), substring(s, 2L))
}


# Frame-aware sibling of format_vcov_label(). Reads from
#   frame$info$vcov_label          (Phase 1+: the engine-/class-specific
#                                   already-formatted label, e.g.
#                                   "Wald (model-based)", "Robust (HC2)",
#                                   "Bayesian (mgcv posterior covariance)", ...)
#   frame$info$vcov_kind           (the estimator token, read through
#                                   .frame_vcov_kind(): "model" / "HC*"
#                                   / "CR*" / "bootstrap" / etc.)
#   frame$info$extras$cluster_name (was extract$cluster_name)
#   frame$info$class               (was extract$is_glm; derived)
#
# Routing: lm / glm go through the legacy classical/HC*/CR* derivation
# below (so historical snapshots stay byte-stable). All other classes
# use the engine-supplied `vcov_label` verbatim (which the Phase 1+
# methods set thoughtfully -- e.g. `note_vcov_wald_asymptotic` for coxph,
# "Robust (HC2)" / "Cluster-robust (CR2)" for estimatr).
format_vcov_label_from_frame <- function(frame) {
  cls <- frame$info$class %||% ""

  # For non-lm/glm classes, the engine-supplied label is authoritative.
  if (!cls %in% c("lm", "glm")) {
    custom_label <- frame$info$vcov_label
    if (
      !is.null(custom_label) &&
        is.character(custom_label) &&
        length(custom_label) == 1L &&
        nzchar(custom_label)
    ) {
      return(custom_label)
    }
  }

  vt <- .frame_vcov_kind(frame)
  cn <- frame$info$extras$cluster_name %||% NA_character_
  is_glm <- identical(cls, "glm")
  # The model-based default, under its canonical name. Guarded by the
  # CLASS as well as the token: this arm names the mechanism that
  # produces an lm / glm standard error. The guard CHANGES what a
  # non-lm/glm frame reaching here without an engine-supplied label
  # renders -- such a frame used to enter this arm and come out
  # "classical (OLS)", so a coxph frame was labeled as ordinary least
  # squares -- and it now falls through to the tail and gets the bare
  # token. No production frame can be in that position (every builder
  # supplies a label and the constructor canonicalizes the kind), so the
  # change is reachable only by hand; it is pinned as such.
  if (.is_model_vcov(vt) && cls %in% c("lm", "glm")) {
    # Phase 7c23 (item c): "Fisher information" is the standard
    # publication-grade name for the glm vcov (the inverse of the
    # expected information matrix; for canonical links equals the
    # inverse Hessian at the MLE). The previous "MLE inverse Hessian"
    # was technically correct but unconventional next to Stata's
    # "OIM" / SAS PROC LOGISTIC's bare "Standard Error" labels.
    # Parallels "classical (OLS)" for lm: both name the underlying
    # mechanism that produces the SE.
    return(
      if (is_glm) {
        spicy_str("note_vcov_classical_glm")
      } else {
        spicy_str("note_vcov_classical_lm")
      }
    )
  }
  if (startsWith(vt, "HC")) {
    return(spicy_fmt("note_vcov_hc", vt))
  }
  if (startsWith(vt, "CR")) {
    cluster_part <- if (is.na(cn) || !nzchar(cn)) {
      spicy_str("note_vcov_cluster_vector")
    } else {
      spicy_fmt("note_vcov_cluster_named", cn)
    }
    if (identical(vt, "CR1S")) {
      # The Stata-correspondence token names its convention so a
      # reader can match the table to Stata output.
      return(spicy_fmt("note_vcov_cr1s", cluster_part))
    }
    return(spicy_fmt("note_vcov_cr", vt, cluster_part))
  }
  # Resampling estimators name the scheme and -- for the bootstrap -- the
  # VALID replicate count (failed replicates are dropped; Stata's
  # bootstrap header reports completed replications). The bare token
  # ("Std. errors: bootstrap.") violated the footer's
  # name-the-estimator-actually-applied principle.
  if (identical(vt, "bootstrap")) {
    n_valid <- frame$info$extras$boot_n_valid %||% NA_integer_
    reps <- if (is.na(n_valid)) {
      ""
    } else {
      spicy_fmt("note_vcov_bootstrap_reps", as.integer(n_valid))
    }
    if (is.na(cn) || !nzchar(cn)) {
      return(spicy_fmt("note_vcov_bootstrap", reps))
    }
    return(spicy_fmt("note_vcov_bootstrap_cluster", reps, cn))
  }
  if (identical(vt, "jackknife")) {
    if (is.na(cn) || !nzchar(cn)) {
      return(spicy_str("note_vcov_jackknife"))
    }
    return(spicy_fmt("note_vcov_jackknife_cluster", cn))
  }
  vt
}


# Frame-aware sibling of build_vcov_footer_block().
build_vcov_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  labels <- vapply(frames, format_vcov_label_from_frame, character(1))
  if (all(labels == labels[1])) {
    return(spicy_fmt("note_std_errors_single", labels[1]))
  }
  # Models with different estimators: one sentence still, the estimator
  # of each model in the order of the columns ("Std. errors: Model 1:
  # classical (OLS), Model 2: heteroskedasticity-robust (HC3)."), so the
  # general note stays one paragraph. The items take a semicolon when
  # one of them holds a comma (`.join_note_vars()`).
  per <- vapply(
    seq_along(labels),
    function(i) .model_line(frames, i, labels[i]),
    character(1)
  )
  spicy_fmt("note_std_errors_single", .join_note_vars(per))
}


# The sampling design a regression was fitted under, and the degrees of
# freedom its tests actually use.
#
# Before this block, a regression table under a design said neither. Its
# descriptive twin on the same design opens with
#   "N = 200 (weighted 6194). Design: stratified (stype), with finite
#    population correction; degrees of freedom vary by group (49 to 99)."
# while the regression footer said only "Std. errors: Design-based
# (Taylor linearization)." -- so a reader could not tell that the design
# was stratified, nor that the t had 193 degrees of freedom. That is a
# reproducibility gap, not a question of density.
#
# The number is the MODEL's residual df (193), never the design's own
# (197): the second is not what the table tests at, and printing it
# beside a t of the first would make the footer contradict the body.
#
# Two guards, both necessary:
#   * the scheme is described only when `.design_meta_or_null()` could
#     read the design (a two-phase design has no PSU variable to name);
#     otherwise the degrees of freedom are disclosed on their own, since
#     they are the part that makes the table reproducible;
#   * the meta comes from the ANALYTIC design (the frame stashes it),
#     so a fit on 180 of 200 schools is not described by the 200.
build_design_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  per_model <- lapply(seq_along(frames), function(i) {
    txt <- .format_design_for_frame(frames[[i]])
    if (is.null(txt)) NULL else list(idx = i, text = txt)
  })
  per_model <- Filter(Negate(is.null), per_model)
  if (length(per_model) == 0L) {
    return(NULL)
  }
  texts <- vapply(per_model, function(pm) pm$text, character(1))
  # One line when every design fit says the same thing -- the common case
  # of a single model, and of two models on one design.
  if (length(unique(texts)) == 1L) {
    return(texts[1L])
  }
  paste(
    vapply(
      per_model,
      function(pm) .model_line(frames, pm$idx, pm$text),
      character(1)
    ),
    collapse = "\n"
  )
}


.format_design_for_frame <- function(frame) {
  df <- frame$info$extras$design_degf_resid
  if (is.null(df) || length(df) != 1L || !is.finite(df)) {
    return(NULL)
  }
  meta <- frame$info$extras$design_meta
  df_clause <- spicy_fmt("note_design_degf_resid", as.integer(df))
  if (is.null(meta)) {
    return(spicy_fmt("note_design_degf_resid_only", as.integer(df)))
  }
  spicy_fmt("note_design_line", .design_scheme_parts(meta), df_clause)
}


# CI-method disclosure. Profile-likelihood CIs are a CI-ONLY refinement -- the
# point estimate, SE, statistic and p-value all stay Wald -- and are NOT
# reconstructable as est +/- z * SE, so the method is stated here (APA 7 /
# SAMPL / STROBE all require disclosing how uncertainty was computed; matches
# parameters::model_parameters). The note fires only when a model's CIs are
# ACTUALLY profile: ci_method == "profile" under a model-based vcov -- a robust
# vcov takes precedence (its Wald-robust CIs are used, no note). The validator
# guarantees ci_method == "profile" reaches only glm / polr / clm.
build_ci_method_footer_block_from_frames <- function(
  frames,
  show_columns = character(0),
  decimal_mark = "."
) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  # The coverage quoted in a note is display text like the header it
  # glosses: its decimal point follows `decimal_mark` (decision 27), and
  # it is produced by the SAME helper the header uses. A local `format()`
  # here read `options(OutDec)` instead, so a session set to a comma
  # printed the header "97.5% CI" over the note "97,5% CIs: profile
  # likelihood." in a table that had asked for a point -- and, under that
  # same session, the Lancet substitution below found no point left to
  # replace. `.ci_pct_display()` is `.ci_pct_str()`'s display twin: the
  # header's pinned digits, then the reader's mark.
  .pct <- function(idx) {
    .ci_pct_display(frames[[idx]]$info$ci_level %||% 0.95, decimal_mark)
  }
  lines <- character(0)
  is_profile <- vapply(
    frames,
    function(f) {
      identical(f$info$ci_method, "profile") &&
        .is_model_vcov(.frame_vcov_kind(f))
    },
    logical(1)
  )
  if (any(is_profile)) {
    lines <- c(
      lines,
      spicy_fmt("note_ci_profile", .pct(which(is_profile)[1]))
    )
  }
  # Percentile bootstrap CIs (G5): a CI-only refinement of the bootstrap
  # replicates; the replicate count already sits on the Std. errors line.
  is_bperc <- vapply(
    frames,
    function(f) {
      identical(f$info$ci_method, "boot_percentile") &&
        identical(.frame_vcov_kind(f), "bootstrap")
    },
    logical(1)
  )
  if (any(is_bperc)) {
    lines <- c(
      lines,
      spicy_fmt("note_ci_bootstrap_percentile", .pct(which(is_bperc)[1]))
    )
  }
  # Bayesian credible intervals in a MIXED table: the shared column
  # header stays "95% CI" (only all-posterior tables relabel to
  # "CrI" / "HDI"), so the Bayesian models' intervals are disclosed
  # per model here -- composing with, not suppressing, the profile /
  # bootstrap lines of the frequentist models above. Gated on the ci
  # column actually being displayed (the compact multi-model default
  # shows none): the publication table stays lean. (posterior_hdi
  # cannot appear in a mixed table: the hdi gate is all-Bayesian.)
  is_post <- vapply(
    frames,
    function(f) {
      (f$info$ci_method %||% "") %in%
        c("posterior_quantile", "posterior_hdi")
    },
    logical(1)
  )
  if (
    any(c("ci", "ame_ci") %in% show_columns) &&
      any(is_post) &&
      !all(is_post)
  ) {
    lines <- c(
      lines,
      vapply(
        which(is_post),
        function(k) {
          spicy_fmt("note_ci_posterior_mixed", k, .pct(k))
        },
        character(1)
      )
    )
  }
  if (length(lines) == 0L) {
    return(NULL)
  }
  paste(lines, collapse = "\n")
}


# The abbreviations a regression table displays, defined under the rule of
# the style in force (`.note_abbreviations()`, R/table_helpers.R), in the
# order a reader meets them: the headers of the columns -- the tokens of
# `show_columns`, model after model, as `build_column_spec()` lays them
# out -- then the rows of the random-effects block, then the fit-statistic
# rows (`show_fit_stats`, those the table prints). The test statistics are
# never defined.
build_abbreviations_footer_block_from_frames <- function(
  show_columns,
  frames = list(),
  standardized = "none",
  show_fit_stats = character(0),
  re_scale = "sd"
) {
  if (!is.list(frames)) {
    frames <- list() # nocov -- every caller passes the list of frames
  }
  # "95% CI", "95% CrI" or "95% HDI": the interval the header names.
  ci_id <- switch(
    .reg_ci_label_key(frames),
    header_ci_label_credible = "cri",
    header_ci_label_hdi = "hdi",
    "ci"
  )
  # A per-category AME (ordinal, multinomial) is an effect on the
  # probability of one response category: its definition names that scale
  # and nothing more. The interpretation guidance -- the sum-to-zero
  # property, the percentage-points reading -- lives in the "Ordinal
  # regression tables" article.
  per_category <- any(vapply(
    frames,
    function(f) {
      ol <- f$coefs$outcome_level
      !is.null(ol) && any(!is.na(ol) & f$coefs$estimate_type == "ame")
    },
    logical(1)
  ))
  ame <- if (per_category) {
    .abbr("ame", term = spicy_str("abbrev_ame_category"))
  } else {
    "ame"
  }
  # The method of a standardized beta, and its dummy convention: five
  # methods give visibly different betas (a dummy's refit beta is 0.247
  # where its basic beta is 0.123 on the same fit). A definition that says
  # how the number was computed, kept under every rule.
  beta_gloss <- switch(
    standardized,
    refit = spicy_str("note_beta_refit"),
    posthoc = spicy_str("note_beta_posthoc"),
    basic = spicy_str("note_beta_basic"),
    smart = spicy_str("note_beta_smart"),
    pseudo = spicy_str("note_beta_pseudo"),
    NULL
  )
  # A method with no gloss of its own keeps the bare definition.
  beta <- .abbr(
    "beta",
    term = if (is.null(beta_gloss)) {
      spicy_str("abbrev_beta")
    } else {
      spicy_fmt("abbrev_beta_method", beta_gloss)
    }
  )
  # The test each model ran for its partial chi-squared: a likelihood-ratio
  # test for glm, a Wald test for the mixed models (regression_partial.R).
  chi2_kind <- vapply(
    frames,
    function(f) {
      if (!any(f$coefs$estimate_type %in% "partial_chi2")) {
        return(NA_character_)
      }
      if (.is_mixed_frame(f)) "wald" else "lrt"
    },
    character(1)
  )
  chi2 <- c(
    if (any(chi2_kind %in% "lrt") || !any(chi2_kind %in% "wald")) {
      list(.abbr("chi2", term = spicy_str("abbrev_chi2_lrt")))
    },
    if (any(chi2_kind %in% "wald")) {
      list(.abbr("chi2", term = spicy_str("abbrev_chi2_wald")))
    }
  )
  f2 <- .abbr("f2", term = spicy_str("abbrev_f2_partial"))
  eta2 <- .abbr("eta2", term = spicy_str("abbrev_eta2_partial"))
  omega2 <- .abbr("omega2", term = spicy_str("abbrev_omega2_partial"))

  items <- list()
  for (f in if (length(frames)) frames else list(NULL)) {
    for (tk in show_columns) {
      items <- c(
        items,
        switch(
          tk,
          b = list(.reg_abbrev_b(f)),
          beta = list(beta),
          se = ,
          rmst_se = ,
          risk_diff_se = ,
          ame_se = list("se"),
          ci = ,
          rmst_ci = ,
          risk_diff_ci = ,
          ame_ci = list(ci_id),
          pd = list("pd"),
          rhat = list("rhat"),
          ess_bulk = ,
          ess_tail = list("ess"),
          mcse = list("mcse"),
          ame = list(ame),
          # A per-fit R2 is a column of the univariable screen only; the
          # R2 of any other model is a fit-statistic row.
          r2 = if (.reg_frame_has(f, "r2")) list("r2"),
          adj_r2 = if (.reg_frame_has(f, "adj_r2")) {
            list(.abbr("adj_r2", symbol = spicy_str("fitstat_adj_r2")))
          },
          partial_f2 = list(f2),
          partial_f2_ci = list(f2, ci_id),
          partial_eta2 = list(eta2),
          partial_eta2_ci = list(eta2, ci_id),
          partial_omega2 = list(omega2),
          partial_omega2_ci = list(omega2, ci_id),
          partial_chi2 = chi2,
          # n, n_events, p, the t or z statistic and the survival
          # estimands: N, n, p and the test statistics are never defined,
          # and the estimand sentence defines dRMST and dRisk with their
          # horizon.
          NULL
        )
      )
    }
  }
  items <- c(
    items,
    .reg_abbrev_random_effects(frames, re_scale),
    unlist(
      lapply(
        .reg_shown_fit_stats(frames, show_fit_stats),
        .reg_abbrev_fit_stat
      ),
      recursive = FALSE
    )
  )
  .note_abbreviations(items)
}

# The concepts of the random-effects block: the standard deviation of a
# random effect (its variance under `re_scale = "variance"`), the
# residual's included, and a correlation. The block is printed for the
# frames whose coefficients carry its rows.
.reg_abbrev_random_effects <- function(frames, re_scale = "sd") {
  has_sd <- FALSE
  has_cor <- FALSE
  for (f in frames) {
    if (!any(f$coefs$estimate_type %in% "vc")) {
      next
    }
    vc <- f$info$random_effects$variance_components
    is_cor <- if (!is.null(vc$is_correlation)) {
      vc$is_correlation %in% TRUE
    } else {
      rep(FALSE, NROW(vc))
    }
    has_sd <- has_sd || any(!is_cor)
    has_cor <- has_cor || any(is_cor)
  }
  c(
    if (has_sd) {
      list(if (identical(re_scale, "variance")) "sigma2" else "sigma")
    },
    if (has_cor) list("rho")
  )
}

# The fit-statistic rows the table prints: the tokens of `show_fit_stats`
# a model of the table has a value for, read as `align_frames()` reads
# them (`build_fit_stats_rows()` drops a row no model fills).
.reg_shown_fit_stats <- function(frames, show_fit_stats) {
  if (!length(show_fit_stats) || !length(frames)) {
    return(character(0))
  }
  rows <- lapply(frames, function(f) {
    .compact_fit_stats_for_legacy(
      f$info$fit_stats,
      "model",
      f$info$dv %||% NA_character_,
      icc = f$info$random_effects$icc %||% NA_real_,
      n_groups = f$info$n_groups
    )
  })
  shown <- vapply(
    show_fit_stats,
    function(tk) {
      any(vapply(
        rows,
        function(r) !is.null(r[[tk]]) && any(!is.na(r[[tk]])),
        logical(1)
      ))
    },
    logical(1)
  )
  show_fit_stats[shown]
}

# The concepts of one fit-statistic row, as `fit_stat_label()` heads it.
# A change row ("Delta AIC") shows the change and the statistic it
# changes. Not collected: n and N (n_groups, n_events, weighted_nobs),
# the test statistics (F-change, Wald F-change, Wald chi-squared, the
# Delta chi-squared of a likelihood-ratio change) and the p-value of a
# change; "theta (dispersion)", "alpha (= 1/theta)" and "phi
# (precision)", which define themselves; and the words (Scale, Max
# cluster size, Deviance, Effective parameters, the fixed-effects block).
.reg_abbrev_fit_stat <- function(tk) {
  change <- .abbr(
    "change",
    symbol = sub("%s", "", spicy_str("fitstat_change_prefix"), fixed = TRUE)
  )
  adj_r2 <- .abbr("adj_r2", symbol = spicy_str("fitstat_adj_r2"))
  f2 <- .abbr("f2", symbol = spicy_str("symbol_f2_global"))
  switch(
    tk,
    r2 = ,
    within_r2 = ,
    r2_marginal = ,
    r2_conditional = list("r2"),
    # A pseudo-R2 is no coefficient of determination, and the Bayesian R2
    # is "an alternative definition" of it (Gelman et al. 2019): defined
    # under the label of its row, which names its author.
    pseudo_r2_mcfadden = ,
    pseudo_r2_nagelkerke = ,
    pseudo_r2_tjur = ,
    r2_bayes = list(.abbr("pseudo_r2", symbol = fit_stat_label(tk))),
    adj_r2 = list(adj_r2),
    omega2 = list(.abbr(
      "omega2",
      symbol = spicy_str("symbol_omega_sq_global")
    )),
    f2 = list(f2),
    sigma = list("sigma_hat"),
    rmse = list("rmse"),
    icc = list("icc"),
    aic = list("aic"),
    aicc = list("aicc"),
    bic = list("bic"),
    qic = list("qic"),
    qicu = list("qicu"),
    elpd_loo = list("elpd", "loo"),
    looic = list("looic"),
    waic = list("waic"),
    r2_change = list(change, "r2"),
    adj_r2_change = list(change, adj_r2),
    f2_change = list(change, f2),
    aic_change = list(change, "aic"),
    aicc_change = list(change, "aicc"),
    bic_change = list(change, "bic"),
    lrt_change = ,
    deviance_change = list(change),
    NULL
  )
}

# The concept of the coefficient header of one model: B, or the ratio
# its exponentiated coefficients display (OR, HR, ...). The frame carries
# that header as text, resolved from the registry when the frame was
# built, so it is matched against the same registry entries; a header no
# entry gives is not defined.
.reg_abbrev_b <- function(f) {
  if (is.null(f) || !isTRUE(f$info$extras$exp_applied)) {
    return("b")
  }
  keys <- c(
    or = "header_exp_or",
    irr = "header_exp_irr",
    hr = "header_exp_hr",
    rr = "header_exp_rr",
    mr = "header_exp_mr",
    tr = "header_exp_tr",
    exp_b = "header_exp_generic"
  )
  hit <- vapply(
    keys,
    function(k) identical(f$info$extras$exp_header, spicy_str(k)),
    logical(1)
  )
  if (any(hit)) names(keys)[hit][[1L]]
}

# Does the frame hold a value in the per-row column `col`?
.reg_frame_has <- function(f, col) {
  !is.null(f$coefs[[col]]) && any(!is.na(f$coefs[[col]]))
}

# The registry key of the interval header of a regression table: "CrI"
# (equal-tailed) or "HDI" when EVERY frame is a posterior interval (the
# design promise in regression_frame_stan.R; the hdi gate guarantees
# posterior_hdi is all-Bayesian), "CI" otherwise -- a mixed frequentist +
# Bayesian table keeps the shared label, with a per-model disclosure from
# the CI-method block of the footer. Read by the renderer and by the
# abbreviations of the note, so the header and its definition agree.
.reg_ci_label_key <- function(frames) {
  methods <- vapply(
    frames,
    function(f) f$info$ci_method %||% "",
    character(1)
  )
  if (length(methods) > 0L && all(methods == "posterior_quantile")) {
    "header_ci_label_credible"
  } else if (length(methods) > 0L && all(methods == "posterior_hdi")) {
    "header_ci_label_hdi"
  } else {
    "header_ci_label_confidence"
  }
}


# Frame-aware sibling of build_ame_satterthwaite_footer_block(). Reads
#   frame$info$extras$use_ame_satterthwaite (was extract$use_ame_satterthwaite)
#   frame$info$class                         (was extract$is_glm; derived
#                                             as class == "glm")
build_ame_satterthwaite_footer_block_from_frames <- function(
  frames,
  show_columns
) {
  if (!"ame" %in% show_columns) {
    return(NULL)
  }
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  any_satt <- any(vapply(
    frames,
    function(f) isTRUE(f$info$extras$use_ame_satterthwaite),
    logical(1)
  ))
  if (!any_satt) {
    return(NULL)
  }
  any_lm <- any(vapply(
    frames,
    function(f) !identical(f$info$class, "glm"),
    logical(1)
  ))
  any_glm <- any(vapply(
    frames,
    function(f) identical(f$info$class, "glm"),
    logical(1)
  ))
  # Phase 7c22: trim the methodological detail. The Pustejovsky &
  # Tipton 2018 reference and the clubSandwich function name belong in
  # the help page (`?table_regression`), not in every table footer
  # (APA Manual 7 sec. 7.10: keep table notes brief; methodological
  # references go in the surrounding text). The footer keeps the
  # actionable summary "t-test with Satterthwaite df", with a one-word
  # qualifier when the lm / glm paths diverge.
  qualifier <- if (any_lm && any_glm) {
    spicy_str("note_ame_satterthwaite_qual_both")
  } else if (any_glm) {
    spicy_str("note_ame_satterthwaite_qual_glm")
  } else {
    ""
  }
  spicy_fmt("note_ame_satterthwaite", qualifier)
}


# Frame-aware sibling of build_standardized_caveat_footer_block().
# The fit lives at attr(frame, "fit") per Phase 0b sub-step 3; the
# non_additive metadata is attached by the orchestrator at
# table_regression.R:1234 to both frames[[i]]$info$extras$non_additive
# and extracts[[i]][["non_additive"]].
build_standardized_caveat_footer_block_from_frames <- function(
  frames,
  standardized
) {
  if (identical(standardized, "none")) {
    return(NULL)
  }
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  any_problem <- FALSE
  for (f in frames) {
    nonadd <- f$info$extras$non_additive
    if (is.null(nonadd)) {
      fit <- attr(f, "fit")
      if (!is.null(fit)) {
        nonadd <- detect_non_additive_terms(fit)
      }
    }
    if (isTRUE(nonadd$has_problem)) {
      any_problem <- TRUE
      break
    }
  }
  if (!any_problem) {
    return(NULL)
  }

  # Fallback awareness (G2): the refit path declines on inline-transform
  # formulas and applies posthoc instead -- the footer must then carry
  # the ALGEBRAIC wording (naming the convention), not the refit one.
  used <- vapply(
    frames,
    function(f) {
      f$info$extras$standardized_used %||% NA_character_
    },
    character(1)
  )
  fell_back <- identical(standardized, "refit") &&
    any(!is.na(used) & used != "refit")

  # Table notes state what IS shown, in method terms only. The
  # negative-space contrast ("components are NOT standardized first"),
  # the convention's software genealogy (SPSS / Stata regress,beta /
  # lm.beta), and the literature citations (Friedrich 1982; Cohen et
  # al. 2003 s7.7) are reader pedagogy and live in the *Standardized
  # coefficients* section of ?table_regression -- a publication table
  # note cites no literature and names no other software. The
  # differs-from-"refit" clause stays: it is the interpretive caveat
  # about the displayed numbers themselves.
  # The fallback sentence quotes the algebraic one after its own lead, so
  # the lead and the body are separate keys: no translated sentence is
  # ever cut back by a pattern.
  algebraic <- spicy_str("note_standardized_caveat_algebraic")

  if (identical(standardized, "refit") && !fell_back) {
    return(spicy_fmt(
      "note_standardized_caveat",
      spicy_str("note_standardized_caveat_refit")
    ))
  }
  if (fell_back) {
    return(spicy_fmt(
      "note_standardized_caveat",
      spicy_fmt("note_standardized_caveat_fallback", algebraic)
    ))
  }
  spicy_fmt("note_standardized_caveat", algebraic)
}


# ---- Theme: stars mapping (Q12) ------------------------------------------

# stars = FALSE                  -> NULL (no footer note)
# stars = TRUE                   -> APA preset:
#   "*** p < .001, ** p < .01, * p < .05."
# stars = c("+"=.10, "*"=.05)    -> "* p < .05, + p < .10."
# Mapping rendered in increasing-strictness order (smallest p first
# at the lead) mirroring stargazer / modelsummary convention.
build_stars_footer_block <- function(stars, decimal_mark = ".") {
  # Single source: the SAME resolver that computes the body markers
  # (resolve_stars_thresholds, regression_render.R). The legend and
  # the cells cannot disagree on the cutoffs -- the defaults used to
  # be written twice, here and there.
  stars <- resolve_stars_thresholds(stars)
  if (is.null(stars)) {
    return(NULL)
  }
  # Already sorted smallest p first => strictest symbol first.
  sym <- names(stars)
  thr <- stars
  parts <- vapply(
    seq_along(sym),
    function(i) {
      spicy_fmt(
        "note_stars_legend_entry",
        sym[i],
        format_p_threshold(thr[i], decimal_mark)
      )
    },
    character(1)
  )
  paste0(paste(parts, collapse = ", "), ".")
}

# APA-style threshold formatting: leading dot, minimum 2 decimal
# digits, trailing zeros trimmed beyond that. Examples:
#   0.001 -> ".001",  0.01 -> ".01",  0.05 -> ".05",  0.10 -> ".10".
format_p_threshold <- function(p, decimal_mark = ".") {
  if (!is.finite(p) || p <= 0 || p > 1) {
    return(format(p))
  }
  # Always 3 decimals first, and always with a dot: the rest of this
  # function is written against one, and `formatC()` would otherwise
  # read `options(OutDec)` and hand it a comma the table never asked
  # for (the same leak `format_number()` closes).
  s <- formatC(p, format = "f", digits = 3, decimal.mark = ".")
  # ".050" / ".100" / ".001" -- unless the style or the table's mark
  # keeps the leading zero, in which case "0.050" / "0.100" / "0.001".
  s <- .strip_leading_zero(s, ".", .style_p_leading_zero(decimal_mark))
  # Trim trailing zeros but keep at least 2 decimal digits ("." + 2
  # chars). The floor counts the integer part, which is empty under the
  # APA drop (".050" -> 3) and one digit when a style keeps the leading
  # zero ("0.050" -> 4).
  min_len <- nchar(sub("\\..*$", "", s)) + 3L
  while (nchar(s) > min_len && substr(s, nchar(s), nchar(s)) == "0") {
    s <- substr(s, 1L, nchar(s) - 1L)
  }
  # The internal logic stays dot-based; the table's decimal mark is
  # applied on the way out, so a comma table's star legend no longer
  # reads "p < 0.001" under a body printed in commas.
  if (!identical(decimal_mark, ".")) {
    s <- sub(".", decimal_mark, s, fixed = TRUE)
  }
  s
}


# Frame-aware sibling of build_singular_footer_block().
# Reads frame$info$extras$has_singular instead of extract$has_singular.
# Phase 7c3: ordinal-thresholds footer block for cumulative-link
# proportional-odds fits (polr, clm). Surfaces the (k - 1) cut-points
# from info$extras$thresholds (a data.frame with term / estimate /
# std_error / statistic / p_value).
#
# Single-model output:
#   "Thresholds: 1|2 = -1.34, 2|3 = 1.25, 3|4 = 3.47, 4|5 = 5.01."
#
# Multi-model: "Model k:" prefix only when more than one frame
# contributes content.
build_ordinal_thresholds_footer_block_from_frames <- function(
  frames,
  decimal_mark = "."
) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }

  # Rows mode: thresholds were promoted to an in-table "Thresholds" block
  # (show_thresholds = TRUE -- the orchestrator appended is_threshold rows to
  # coefs). The compact per-model line would then double-report them, so emit a
  # single one-line gloss instead. Under exponentiate, flag that the rows stay
  # on the log-odds scale (cut-points are never exponentiated).
  rows_mode <- any(vapply(
    frames,
    function(f) {
      isTRUE(any(f$coefs$is_threshold))
    },
    logical(1)
  ))
  if (isTRUE(rows_mode)) {
    exp_frames <- Filter(
      function(f) isTRUE(f$info$extras$exp_applied),
      frames
    )
    gloss <- spicy_fmt(
      "note_thresholds_rows_gloss",
      spicy_str("label_block_thresholds")
    )
    if (length(exp_frames) > 0L) {
      # The cut-point scale is the LINK scale, which is not log-odds
      # for every exponentiating link: cumulative cloglog thresholds
      # are baseline log cumulative hazards.
      links <- unique(vapply(
        exp_frames,
        function(f) f$info$family$link %||% "",
        character(1)
      ))
      scale_lbl <- if (identical(links, "logit")) {
        spicy_str("note_scale_log_odds")
      } else if (identical(links, "cloglog")) {
        spicy_str("note_scale_log_cumhazard")
      } else {
        spicy_str("note_scale_link")
      }
      gloss <- paste0(
        gloss,
        spicy_fmt("note_thresholds_scale_suffix", scale_lbl)
      )
    }
    return(paste0(gloss, "."))
  }

  # Footer mode (show_thresholds = FALSE): the compact per-model line.
  per_model <- lapply(seq_along(frames), function(i) {
    f <- frames[[i]]
    txt <- .format_ordinal_thresholds_for_frame(f, decimal_mark)
    if (is.null(txt)) NULL else list(idx = i, text = txt)
  })
  per_model <- Filter(Negate(is.null), per_model)
  if (length(per_model) == 0L) {
    return(NULL)
  }

  if (length(per_model) == 1L) {
    return(per_model[[1L]]$text)
  }
  lines <- vapply(
    per_model,
    function(pm) {
      .model_line(frames, pm$idx, pm$text)
    },
    character(1)
  )
  paste(lines, collapse = "\n")
}


# GEE working-correlation disclosure. The structure is model-defining
# (the RE-block philosophy for mixed models): two GEE fits with the
# same formula but different corstr are different models, so the
# footer states the structure -- with the estimated alpha when the
# structure has one:
#   exchangeable / ar1:  "GEE working correlation: exchangeable
#                         (alpha = 0.77)."
#   independence:        "GEE working correlation: independence."
#   unstructured:        "GEE working correlation: unstructured
#                         (10 correlation parameters)."
build_gee_footer_block_from_frames <- function(frames, decimal_mark = ".") {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }

  per_model <- lapply(seq_along(frames), function(i) {
    f <- frames[[i]]
    txt <- .format_gee_for_frame(f, decimal_mark)
    if (is.null(txt)) NULL else list(idx = i, text = txt)
  })
  per_model <- Filter(Negate(is.null), per_model)
  if (length(per_model) == 0L) {
    return(NULL)
  }

  if (length(per_model) == 1L) {
    return(per_model[[1L]]$text)
  }
  lines <- vapply(
    per_model,
    function(pm) {
      .model_line(frames, pm$idx, pm$text)
    },
    character(1)
  )
  paste(lines, collapse = "\n")
}


.format_gee_for_frame <- function(frame, decimal_mark = ".") {
  cls <- frame$info$class %||% ""
  if (!identical(cls, "geeglm")) {
    return(NULL)
  }
  corstr <- frame$info$extras$gee_corstr %||% NA_character_
  if (is.na(corstr) || !nzchar(corstr)) {
    return(NULL) # nocov -- a valid geeglm always carries corstr
  }
  alpha <- frame$info$extras$gee_alpha
  alpha <- alpha[is.finite(alpha)]
  if (length(alpha) == 1L) {
    return(spicy_fmt(
      "note_gee_corstr_alpha",
      corstr,
      # The estimate is a rendered number in a note, so it follows the
      # table's mark like the cells above it -- and, through
      # `.footer_num()`, ignores `options(OutDec)` either way.
      .footer_num(alpha, 2L, decimal_mark)
    ))
  }
  if (length(alpha) > 1L) {
    return(spicy_fmt("note_gee_corstr_params", corstr, length(alpha)))
  }
  spicy_fmt("note_gee_corstr_plain", corstr)
}


.format_ordinal_thresholds_for_frame <- function(frame, decimal_mark = ".") {
  cls <- frame$info$class %||% ""
  if (!cls %in% c("polr", "clm", "svyolr", "clmm", "glmmTMB")) {
    return(NULL)
  }
  th <- frame$info$extras$thresholds
  if (is.null(th) || !is.data.frame(th) || nrow(th) == 0L) {
    return(NULL)
  }
  parts <- vapply(
    seq_len(nrow(th)),
    function(i) {
      # A cut-point is a rendered number in a note, so it follows the
      # table's mark like the coefficient cells above it -- and, through
      # `.footer_num()`, ignores `options(OutDec)` either way. Under a
      # point this is byte-for-byte the `sprintf("%.2f")` it replaces.
      sprintf(
        "%s = %s",
        th$term[i],
        .footer_num(th$estimate[i], 2L, decimal_mark)
      )
    },
    character(1)
  )
  spicy_fmt(
    "note_thresholds_compact",
    spicy_str("label_block_thresholds"),
    paste(parts, collapse = ", ")
  )
}


# Phase 7c2: survival-specific footer block for coxph / cph / survreg /
# flexsurvreg. Surfaces events count + concordance C (Cox) or
# distribution + scale (parametric AFT).
#
# Single-model output examples:
#   coxph / cph: "Concordance C = 0.60 (SE = 0.03)." (n / events are
#                 fit-stat rows since 2026-07-09)
#   survreg:    "Distribution: Weibull; scale = 0.75."
#   flexsurv:   "Distribution: Weibull; shape = 1.33, scale = 531.05."
build_survival_footer_block_from_frames <- function(
  frames,
  decimal_mark = "."
) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }

  per_model <- lapply(seq_along(frames), function(i) {
    f <- frames[[i]]
    txt <- .format_survival_for_frame(f, decimal_mark)
    if (is.null(txt)) NULL else list(idx = i, text = txt)
  })
  per_model <- Filter(Negate(is.null), per_model)
  if (length(per_model) == 0L) {
    return(NULL)
  }

  # Prefix with "Model k:" only when MORE THAN ONE frame contributes
  # content; a single contributing model gets the line bare (no
  # misleading "Model 1:" when its peers are silent on this block).
  if (length(per_model) == 1L) {
    return(per_model[[1L]]$text)
  }
  lines <- vapply(
    per_model,
    function(pm) {
      .model_line(frames, pm$idx, pm$text)
    },
    character(1)
  )
  paste(lines, collapse = "\n")
}


.format_survival_for_frame <- function(frame, decimal_mark = ".") {
  cls <- frame$info$class %||% ""
  if (cls %in% c("coxph", "cph", "svycoxph")) {
    .format_coxph_survival(frame, decimal_mark)
  } else if (cls == "survreg") {
    .format_survreg_survival(frame, decimal_mark)
  } else if (cls == "flexsurvreg") {
    .format_flexsurv_survival(frame, decimal_mark)
  } else {
    NULL
  }
}


.format_coxph_survival <- function(frame, decimal_mark = ".") {
  conc <- frame$info$extras$concordance
  parts <- character(0)
  # n and the number of events moved from this footer prose into
  # fit-stat ROWS ("n" / "N events" tokens, 2026-07-09) -- same
  # migration as the mixed-effects ICC / N (groups). The footer keeps
  # what has no row: the concordance.
  if (
    !is.null(conc) && is.list(conc) && !is.null(conc$c) && is.finite(conc$c)
  ) {
    # The concordance and its SE are rendered numbers in a note: they
    # follow the table's mark like the cells above them, through the
    # footer twin `.footer_num()`. Under a point the two arms are
    # byte-identical to the `%.2f` they replace.
    if (!is.null(conc$se) && is.finite(conc$se)) {
      parts <- c(
        parts,
        spicy_fmt(
          "note_survival_concordance_se",
          .footer_num(conc$c, 2L, decimal_mark),
          .footer_num(conc$se, 2L, decimal_mark)
        )
      )
    } else {
      parts <- c(
        parts,
        sprintf("Concordance C = %s", .footer_num(conc$c, 2L, decimal_mark))
      )
    }
  }
  # Reachable now that the Events prose moved to fit-stat rows: a
  # frame without a concordance block contributes nothing here.
  if (length(parts) == 0L) {
    return(NULL)
  }
  paste0(paste(parts, collapse = spicy_str("note_survival_sep")), ".")
}


.format_survreg_survival <- function(frame, decimal_mark = ".") {
  dist <- frame$info$extras$distribution
  scale <- frame$info$extras$scale_parameter
  parts <- character(0)
  if (!is.null(dist) && is.character(dist) && nzchar(dist)) {
    parts <- c(
      parts,
      spicy_fmt("note_survival_distribution", .surv_title_dist(dist))
    )
  }
  if (!is.null(scale) && is.finite(scale)) {
    parts <- c(
      parts,
      spicy_fmt("note_survival_scale", .footer_num(scale, 2L, decimal_mark))
    )
  }
  if (length(parts) == 0L) {
    return(NULL) # nocov
  }
  paste0(paste(parts, collapse = spicy_str("note_survival_sep")), ".")
}


.format_flexsurv_survival <- function(frame, decimal_mark = ".") {
  dist <- frame$info$extras$distribution
  aux <- frame$info$extras$aux_parameters
  parts <- character(0)
  if (!is.null(dist) && is.character(dist) && nzchar(dist)) {
    parts <- c(
      parts,
      spicy_fmt("note_survival_distribution", .surv_title_dist(dist))
    )
  }
  if (!is.null(aux) && is.numeric(aux) && length(aux) > 0L) {
    aux_str <- paste(
      sprintf(
        "%s = %s",
        names(aux),
        .footer_num(unname(aux), 2L, decimal_mark)
      ),
      collapse = ", "
    )
    parts <- c(parts, aux_str)
  }
  if (length(parts) == 0L) {
    return(NULL) # nocov
  }
  paste0(paste(parts, collapse = spicy_str("note_survival_sep")), ".")
}


.surv_title_dist <- function(dist) {
  # Single source for parametric-survival distribution display names:
  # the union of the survreg and flexsurvreg vocabularies.
  # .survreg_dist_title() (regression_frame_survival.R) delegates
  # here -- the two used to be separate, diverging tables.
  switch(
    dist,
    weibull = "Weibull",
    weibullPH = "Weibull (PH)",
    lognormal = "Log-normal",
    lnorm = "Log-normal",
    gompertz = "Gompertz",
    gamma = "Gamma",
    exponential = "Exponential",
    exp = "Exponential",
    llogis = "Log-logistic",
    loglogistic = "Log-logistic",
    gengamma = "Generalized gamma",
    genf = "Generalized F",
    gaussian = "Gaussian",
    logistic = "Logistic",
    `t` = "Student-t",
    paste0(toupper(substr(dist, 1L, 1L)), substring(dist, 2L))
  )
}


# Phase 7c1: random-effects footer block for mixed-effects fits.
# Reads from frame$info$random_effects$variance_components (data.frame
# with group / term / variance / sd / corr columns) and
# frame$info$random_effects$icc (scalar). Skipped silently when no
# mixed-effects component is present (variance_components is empty).
#
# Single-model output:
#   "Random effects: 18 Subjects; intercept variance = 1296.87,
#    residual variance = 954.53; ICC = 0.58."
# Multi-model: one line per affected model, prefixed "Model k:".
build_random_effects_footer_block_from_frames <- function(
  frames,
  show_re = TRUE,
  re_scale = "sd",
  re_columns = c("est", "se", "ci"),
  re_test = "none",
  decimal_mark = "."
) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  # Phase 7c7d: user-disabled panel returns NULL early -- the random-
  # effects block is fully suppressed from the footer.
  if (!isTRUE(show_re)) {
    return(NULL)
  }

  per_model <- lapply(seq_along(frames), function(i) {
    f <- frames[[i]]
    txt <- .format_random_effects_for_frame(
      f,
      decimal_mark = decimal_mark,
      re_scale = re_scale,
      re_columns = re_columns
    )
    if (is.null(txt)) NULL else list(idx = i, text = txt)
  })
  per_model <- Filter(Negate(is.null), per_model)
  if (length(per_model) == 0L) {
    return(NULL)
  }

  # Per-term test disclosure (re_test = "lrt" / "rlrt"): one sentence for the
  # whole block, naming the test behind the vc rows' p column.
  # Method names only -- the literature (Self & Liang 1987; Stram & Lee
  # 1994; Crainiceanu & Ruppert 2004) and the computing package (RLRsim)
  # are cited in ?table_regression, not in a publication table note.
  test_line <- switch(
    re_test,
    lrt = spicy_str("note_re_test_lrt"),
    rlrt = spicy_str("note_re_test_rlrt"),
    NULL
  )

  # Prefix with "Model k:" only when more than one frame contributes
  # content (parity with the survival footer's convention).
  if (length(per_model) == 1L) {
    return(paste(c(per_model[[1L]]$text, test_line), collapse = "\n"))
  }
  lines <- vapply(
    per_model,
    function(pm) {
      .model_line(frames, pm$idx, pm$text)
    },
    character(1)
  )
  paste(c(lines, test_line), collapse = "\n")
}


# Phase 7c8a: per-frame fixed-effect inference annotation for mixed-
# effects fits. Returns NULL for non-mixed classes; otherwise a single
# line stating the distribution + df strategy + source package, so a
# publication-quality reader can tell at a glance whether the
# p-values / CIs are Satterthwaite (the recommended default for
# lmer when sample sizes are small), Wald-z (large-sample
# approximation), or nlme's containment df.
#
# Single-model output (no prefix):
#   "p-values: Satterthwaite t-test (lmerTest)."
# Multi-model: prefixed "Model k: ...".
build_mixed_inference_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }

  per_model <- lapply(seq_along(frames), function(i) {
    f <- frames[[i]]
    txt <- .mixed_inference_label_for_frame(f)
    if (is.null(txt)) NULL else list(idx = i, text = txt)
  })
  per_model <- Filter(Negate(is.null), per_model)
  if (length(per_model) == 0L) {
    return(NULL)
  }

  if (length(per_model) == 1L) {
    return(per_model[[1L]]$text)
  }
  # Consolidate when every model produced the SAME annotation -- avoids
  # printing three identical "p-values: Wald-z, ..." lines when the
  # whole list is, say, lme4::lmer without lmerTest. Mirrors the
  # consolidation done by build_regression_type_footer_block_from_frames().
  texts <- vapply(per_model, function(pm) pm$text, character(1))
  if (length(unique(texts)) == 1L) {
    return(texts[1L])
  }
  lines <- vapply(
    per_model,
    function(pm) {
      .model_line(frames, pm$idx, pm$text)
    },
    character(1)
  )
  paste(lines, collapse = "\n")
}


# The table says where its change statistics came from when they did not
# come from the fits the other rows describe.
#
# lme4::anova.merMod refits a REML fit by maximum likelihood before it
# compares (a one-line message spicy suppresses), so the change block of
# a REML hierarchy is computed on ML refits while the per-model AIC and
# BIC rows above it are the REML criteria of the fits themselves.
# Measured on sleepstudy: DeltaAIC -114.46 from the refits against
# -115.86 from the displayed rows, DeltaBIC -111.27 against -112.67.
# Recomputing the change on the REML criteria would be the wrong fix: a
# restricted likelihood is built on contrasts that annihilate the
# fixed-effects design, so REML criteria are not comparable across
# fixed-effects structures at all (Pinheiro & Bates 2000, Section 2.4.2
# -- the same ground as the REML refusal in check_nested_reml_pair()).
# The refit is right and the disagreement is real, so the table
# discloses it.
#
# TWO conditions, and both are about what the reader can see.
#
# (1) At least one change token is actually ON the table: selected for
#     display AND carrying a number somewhere. A note describes rows,
#     and `show_fit_stats = "nobs"` renders a table with no change rows
#     at all, while a hierarchy the admissibility rules en-dash renders
#     none either. In both cases the sentence annotated rows that are
#     not there.
# (2) An adjacent pair actually WAS refitted: both members lme4 fits
#     (the only engine in the mixed router that refits -- glmmTMB
#     refuses, nlme warns and is refused by spicy) and at least one of
#     them REML.
build_nested_ml_refit_footer_block_from_frames <- function(
  frames,
  nested,
  show_fit_stats = character(0)
) {
  if (!isTRUE(nested) || !is.list(frames) || length(frames) < 2L) {
    return(NULL)
  }
  shown_change <- intersect(show_fit_stats, .NESTED_CHANGE_TOKENS)
  has_change_row <- length(shown_change) > 0L &&
    any(vapply(
      frames,
      function(f) {
        fs <- f$info$fit_stats
        if (!is.list(fs)) {
          return(FALSE)
        }
        # Subset by NAME, not by [[: a list `[[` on an absent name is an
        # error, and a frame that never went through
        # attach_nested_stats_to_frames() carries none of these keys.
        any(vapply(
          fs[intersect(shown_change, names(fs))],
          function(v) isTRUE(is.finite(scalar_or_na(v))),
          logical(1)
        ))
      },
      logical(1)
    ))
  if (!has_change_row) {
    return(NULL)
  }
  is_lme4 <- vapply(
    frames,
    function(f) (f$info$class %||% "") %in% c("lmerMod", "glmerMod"),
    logical(1)
  )
  is_reml <- vapply(
    frames,
    function(f) identical(f$info$random_effects$method, "REML"),
    logical(1)
  )
  pairs <- seq_len(length(frames) - 1L)
  refitted <- any(
    is_lme4[pairs] &
      is_lme4[pairs + 1L] &
      (is_reml[pairs] | is_reml[pairs + 1L])
  )
  if (!refitted) {
    return(NULL)
  }
  spicy_str("note_nested_ml_refit")
}


# Pick the right one-line annotation for a single frame. NULL when
# the frame is not a mixed-effects fit.
#
# Dispatch on (class, ci_method) instead of just class: lmer fits
# are normalized to class == "lmerMod" regardless of whether
# lmerTest decorated the fit, so we read ci_method to tell apart
# Satterthwaite ("satterthwaite") from Wald-z fallback ("wald").
.mixed_inference_label_for_frame <- function(frame) {
  cls <- frame$info$class %||% ""
  ci <- frame$info$ci_method %||% ""
  vk <- .frame_vcov_kind(frame)
  # Under a CR* vcov the whole inference set (SE, Satterthwaite df, p)
  # comes from clubSandwich::coef_test(), NOT from lmerTest / nlme --
  # the footer must attribute the df source truthfully. (glmmTMB under
  # CR0 keeps its Wald-z line: its sandwich SEs are tested against the
  # normal, and the Std. errors line names the estimator.)
  if (startsWith(vk, "CR") && cls %in% c("lmerMod", "lme")) {
    return(spicy_str("note_pvalues_satterthwaite_cr"))
  }
  if (cls == "lmerMod" && identical(ci, "satterthwaite")) {
    return(spicy_str("note_pvalues_satterthwaite"))
  }
  switch(
    cls,
    "lmerMod" = spicy_str("note_pvalues_wald_lmer"),
    "glmerMod" = spicy_fmt("note_pvalues_wald", "lme4"),
    "glmmTMB" = spicy_fmt("note_pvalues_wald", "glmmTMB"),
    "lme" = spicy_str("note_pvalues_containment"),
    NULL
  )
}


# Phase 7c7b: convert a variance_components data.frame between the
# variance scale (canonical storage in info$random_effects, where SE
# and CI live on sigma^2) and the SD scale (the publication-friendly
# default per Gelman, ARM). Returns a NEW data.frame with the requested
# scale applied to the `variance` / `sd` / `std_error` / `ci_lower` /
# `ci_upper` columns.
#
# Delta-method derivations:
#   * SD <- sqrt(variance) (monotonic, fine for boundary at 0)
#   * SE(sigma)  = SE(sigma^2) / (2 * sigma)
#   * CI(sigma)  = sqrt(CI(sigma^2))  -- monotonic on sigma^2 >= 0
#   * For correlation rows (is_correlation == TRUE), no scale change
#     applies: \u03C1 is unitless, so the values pass through unchanged.
.re_components_on_scale <- function(vc_df, target_scale = c("sd", "variance")) {
  target_scale <- match.arg(target_scale)
  if (!is.data.frame(vc_df) || nrow(vc_df) == 0L) {
    return(vc_df)
  }
  if (identical(target_scale, "variance")) {
    return(vc_df)
  }

  out <- vc_df
  is_corr <- if ("is_correlation" %in% colnames(vc_df)) {
    isTRUE(vc_df$is_correlation) | vc_df$is_correlation %in% TRUE
  } else {
    rep(FALSE, nrow(vc_df))
  }
  # For non-correlation rows: convert variance -> sd via sqrt + Delta-method.
  for (i in seq_len(nrow(vc_df))) {
    if (isTRUE(is_corr[i])) {
      next
    } # correlation row, leave as-is

    var_est <- vc_df$variance[i]
    if (is.finite(var_est) && var_est >= 0) {
      out$variance[i] <- sqrt(var_est) # column repurposed as "estimate"
      out$sd[i] <- sqrt(var_est)
    } else {
      out$variance[i] <- NA_real_ # nocov
      out$sd[i] <- NA_real_ # nocov
    }

    if ("std_error" %in% colnames(vc_df)) {
      se_var <- vc_df$std_error[i]
      sd_val <- out$sd[i]
      if (is.finite(se_var) && is.finite(sd_val) && sd_val > 0) {
        out$std_error[i] <- se_var / (2 * sd_val)
      } else {
        out$std_error[i] <- NA_real_
      }
    }
    for (col in c("ci_lower", "ci_upper")) {
      if (col %in% colnames(vc_df)) {
        v <- vc_df[[col]][i]
        out[[col]][i] <- if (is.finite(v) && v >= 0) sqrt(v) else NA_real_
      }
    }
  }
  out
}


# Footer glosses for the component blocks: one line per DISTINCT block
# rendered in the table (gated on is_component rows actually present, so
# show_components = FALSE prints nothing), naming what each block models and
# its scale under exponentiate. A robust vcov covers the component rows too
# (pscl: the whole-model vcovCL; glmmTMB: the full CR0 sandwich), so the
# Std. errors line speaks for every row and no disclosure is needed here.
build_component_blocks_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }

  lines <- character(0)
  seen <- character(0)
  for (f in frames) {
    cf <- f$coefs
    if (is.null(cf$is_component) || !any(cf$is_component %in% TRUE)) {
      next
    }
    for (blk in f$info$extras$component_blocks %||% list()) {
      # Deduplicate on the block's IDENTITY, never on the gloss text: two
      # models contributing the same block must produce one footer line,
      # and that decision has to survive a translated caption.
      key <- blk$label
      if (key %in% seen) {
        next
      }
      seen <- c(seen, key)
      gl <- blk$gloss
      if (isTRUE(blk$exp_applied)) {
        gl <- paste0(gl, " ", spicy_str("note_component_exp_or"))
      } else if (isTRUE(f$info$extras$exp_applied) && !isTRUE(blk$exp_ok)) {
        gl <- paste0(gl, " ", spicy_str("note_component_not_exp"))
      }
      lines <- c(lines, gl)
    }
  }
  if (length(lines) == 0L) {
    return(NULL)
  }
  paste(lines, collapse = "\n")
}


# Promote the component blocks (zero-inflation / zero hurdle / dispersion,
# from info$extras$component_blocks) into coefs rows for labeled subordinate
# blocks. Called by the table_regression() orchestrator AFTER the central
# exponentiate (each block applies its OWN link-gated exp -- see below) and
# BEFORE p_adjust: component coefficients are substantive hypotheses and JOIN
# the p-adjust family (unlike ordinal thresholds), and their tests carry stars.
#
# Per-block exponentiation (dev/component_blocks_spec.md D3): exp only where
# the result IS an odds ratio -- the logit-link zero components. Probit /
# cauchit / cloglog zero links, count-type hurdle zero parts (log scale), and
# the dispersion model are left on the link scale (Stata's `irr` precedent:
# the inflate equation is never eform'd; parameters mislabels probit as OR --
# we gate instead). The footer gloss names each block's scale.
.append_component_rows <- function(coefs, blocks, exponentiate) {
  if (is.null(blocks) || length(blocks) == 0L) {
    return(coefs)
  }
  if (is.null(coefs$is_component)) {
    coefs$is_component <- FALSE
  }

  for (blk in blocks) {
    rows <- blk$coefs
    if (is.null(rows) || nrow(rows) == 0L) {
      next # nocov
    }

    est <- rows$estimate
    se <- rows$std_error
    cil <- rows$ci_lower
    ciu <- rows$ci_upper
    if (isTRUE(exponentiate) && isTRUE(blk$exp_ok)) {
      keep <- !rows$is_ref & !is.na(est)
      # exp() + Delta-method SE; z / p invariant (H0: B = 0 <-> exp(B) = 1).
      cil[keep] <- exp(cil[keep])
      ciu[keep] <- exp(ciu[keep])
      se[keep] <- exp(est[keep]) * se[keep]
      est[keep] <- exp(est[keep])
    }

    new <- data.frame(
      term = rows$term,
      # THE relay from the frame layer to the coefs layer: a component
      # block's `label` field (written by the glmmTMB / pscl frames from
      # `.REG_BLOCK_ZI` / `_HURDLE` / `_DISP`) becomes the `parent_var`
      # that every downstream site matches on. It is an identity string
      # on both sides, so nothing is resolved here.
      parent_var = blk$label,
      label = rows$label,
      factor_level_pos = seq_len(nrow(rows)),
      is_ref = rows$is_ref,
      estimate_type = "B",
      estimate = est,
      std_error = se,
      df = ifelse(rows$is_ref, NA_real_, Inf),
      statistic = rows$statistic,
      p_value = rows$p_value,
      ci_lower = cil,
      ci_upper = ciu,
      test_type = ifelse(rows$is_ref, NA_character_, "z"),
      outcome_level = NA_character_,
      is_component = TRUE,
      stringsAsFactors = FALSE
    )
    coefs <- .rbind_union(coefs, new)
  }
  coefs
}


# Promote info$random_effects$variance_components into coefs rows for a
# subordinate "Random effects" block. Called by the table_regression()
# orchestrator (NOT the frame methods) AFTER exponentiate + p_adjust, so those
# paths never touch variance components (a variance is never exponentiated or
# p-adjusted). Mirrors .append_threshold_rows().
#
# estimate_type = "vc" (variance component) is a first-class token: a variance
# parameter is NOT a mean-model coefficient, so it is honestly typed and is
# excluded by construction from the exp / standardize / stars filters (which key
# on "B" / "beta"). The renderer aliases "vc" to the B (estimate) columns for
# display.
#
# statistic / df / p_value are carried as NA: no per-row p by default (H0:
# sigma = 0 is on the boundary; a Wald z is invalid -- no reporting guideline
# asks for it). The correct significance signal is the model-level
# chi-bar-squared LRT (footer line, from info$random_effects$null_lrt). The
# columns are kept so a future opt-in per-term LRT/RLRT test can fill them.
.append_random_effects_rows <- function(
  coefs,
  re,
  re_scale = "sd",
  term_tests = NULL,
  re_test = "none"
) {
  if (is.null(re)) {
    return(coefs)
  }
  vc <- re$variance_components
  if (is.null(vc) || !is.data.frame(vc) || nrow(vc) == 0L) {
    return(coefs)
  }
  if (is.null(coefs$is_re)) {
    coefs$is_re <- FALSE
  }

  # Convert SD / variance / SE / CI to the requested display scale (Delta
  # method for the SD scale). `.re_components_on_scale` repurposes the
  # `variance` column as the scaled point estimate; correlation rows keep their
  # value in `corr`.
  vc <- .re_components_on_scale(vc, re_scale)
  is_corr <- if ("is_correlation" %in% names(vc)) {
    vc$is_correlation %in% TRUE
  } else {
    rep(FALSE, nrow(vc))
  }
  is_resid <- !is.na(vc$group) & vc$group == "Residual"

  est <- ifelse(is_corr, vc$corr, vc$variance)
  se <- if ("std_error" %in% names(vc)) {
    vc$std_error
  } else {
    rep(NA_real_, nrow(vc))
  }
  cil <- if ("ci_lower" %in% names(vc)) vc$ci_lower else rep(NA_real_, nrow(vc))
  ciu <- if ("ci_upper" %in% names(vc)) vc$ci_upper else rep(NA_real_, nrow(vc))
  # NOTE: `re_columns` is a DISPLAY concern -- it is honored at render time
  # (build_body_row en-dashes the deselected cells on vc rows). The data stays
  # complete here so broom::tidy() / as_structured() always carry full SE + CI.

  label <- vapply(
    seq_len(nrow(vc)),
    function(i) {
      .re_panel_label(vc$group[i], vc$term[i], is_corr[i], is_resid[i])
    },
    character(1)
  )
  # On the variance scale the sigma glyph would mislabel the value: promote
  # the SD labels to sigma-squared (correlations are unitless, untouched).
  # The two glyphs are the registry's, as the note that defines them.
  if (identical(re_scale, "variance")) {
    label[!is_corr] <- sub(
      paste0("^", .escape_regex(spicy_str("symbol_sigma")), " "),
      paste0(spicy_str("symbol_sigma_sq"), " "),
      label[!is_corr]
    )
  }
  # Stable per-(group, term) key so RE rows align across models by structure.
  key <- paste0("re::", vc$group, "::", vc$term, ifelse(is_corr, "::cor", ""))

  new <- data.frame(
    term = key,
    parent_var = .REG_BLOCK_RE,
    label = label,
    factor_level_pos = seq_len(nrow(vc)),
    is_ref = FALSE,
    estimate_type = "vc",
    estimate = as.numeric(est),
    std_error = as.numeric(se),
    df = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    ci_lower = as.numeric(cil),
    ci_upper = as.numeric(ciu),
    test_type = NA_character_,
    outcome_level = NA_character_,
    is_re = TRUE,
    stringsAsFactors = FALSE
  )

  # Opt-in per-term tests (re_test = "lrt" / "rlrt"): fill statistic / df /
  # p_value on the matching SD rows (never on correlation / residual rows --
  # a correlation is tested jointly with its slope's variance, and the
  # residual has no zero-variance null).
  if (
    !is.null(term_tests) && is.data.frame(term_tests) && nrow(term_tests) > 0L
  ) {
    row_key <- paste(vc$group, vc$term, sep = "\r")
    test_key <- paste(term_tests$group, term_tests$term, sep = "\r")
    idx <- match(row_key, test_key)
    hit <- !is.na(idx) & !is_corr & !is_resid
    new$statistic[hit] <- term_tests$statistic[idx[hit]]
    new$df[hit] <- term_tests$df[idx[hit]]
    new$p_value[hit] <- term_tests$p_value[idx[hit]]
    new$test_type[hit] <- if (identical(re_test, "rlrt")) "rlrt" else "chibar2"
  }
  .rbind_union(coefs, new)
}


# Internal: p-value formatter for the LR test line. Returns strings like
# "< .001" or "= 0.034" so the line reads naturally after "p".
#
# The number follows the table's `decimal_mark` like every other rendered
# number: the same footer carries the star legend, and a table built with
# `decimal_mark = ","` used to read "p < .001" three lines above
# "*** p < 0,001". The floor branch quotes `format_p_threshold()` -- the
# legend's own producer -- so the two can no longer disagree, and the
# ordinary branch goes through `.footer_num()`.
#
# Its LEADING ZERO is the package's, not this footer's: `.001` and
# `.500` under a point (the APA drop spicy defaults to), `0,001` and
# `0,500` under a comma (the only form the SI brochure admits), and a
# `p_style` in scope settles it outright for both branches at once.
# This line used to write "= 0.500" under a point while every p cell
# in the table above it wrote ".500".
format_p_value_for_panel <- function(p, decimal_mark = ".") {
  if (!is.finite(p)) {
    return("= NA")
  }
  if (p < 0.001) {
    return(paste0("< ", format_p_threshold(0.001, decimal_mark)))
  }
  paste0(
    "= ",
    .strip_leading_zero(
      .footer_num(p, 3L, decimal_mark),
      decimal_mark,
      .style_p_leading_zero(decimal_mark)
    )
  )
}


# Label for a single row of the random-effects panel. The glyphs are the
# registry's (`symbol_sigma`, `symbol_rho`), the very keys the note's
# definitions of these rows read.
.re_panel_label <- function(group, term, is_correlation, is_residual) {
  sigma <- spicy_str("symbol_sigma")
  if (isTRUE(is_residual)) {
    return(paste(sigma, "(Residual)"))
  }
  if (isTRUE(is_correlation)) {
    return(sprintf("%s %s (%s)", spicy_str("symbol_rho"), group, term))
  }
  if (is.na(term) || !nzchar(term) || term == "(Intercept)") {
    return(sprintf("%s %s (Intercept)", sigma, group))
  }
  sprintf("%s %s %s", sigma, group, term)
}


# Format the per-frame random-effects sentence. Returns NULL when the
# frame has no random-effects content (lm / glm / coxph / etc.).
.format_random_effects_for_frame <- function(frame, decimal_mark = ".", ...) {
  re <- frame$info$random_effects
  if (is.null(re)) {
    return(NULL)
  }
  vc <- re$variance_components
  if (is.null(vc) || !is.data.frame(vc) || nrow(vc) == 0L) {
    return(NULL)
  }

  # The variance parameters render as table rows; N (groups) + ICC render as
  # fit-stat rows (aligned per model). The footer keeps only what has no row
  # home: the estimation method (REML / ML) and the model-level LR test vs the
  # no-random-effects model, referred to the chi-bar-squared mixture null
  # (Self & Liang 1987; Stram & Lee 1994) -- the correct, boundary-aware
  # significance signal for the random part. There is deliberately no per-row
  # Wald p on the variance components (no reporting guideline asks for it;
  # see dev/mixed_random_effects_rows_spec.md).
  method_tag <- re$method
  has_method <- !is.null(method_tag) &&
    is.character(method_tag) &&
    length(method_tag) == 1L &&
    !is.na(method_tag) &&
    nzchar(method_tag)
  header <- if (has_method) {
    spicy_fmt(
      "note_re_method",
      spicy_str("label_block_random_effects"),
      method_tag
    )
  } else {
    spicy_str("label_block_random_effects")
  }

  lrt <- re$null_lrt
  lrt_part <- NULL
  if (!is.null(lrt) && is.finite(lrt$chi2) && is.finite(lrt$df)) {
    p_str <- format_p_value_for_panel(lrt$p_chibar2, decimal_mark)
    # The null model in the table language, from the key its producer
    # chose; a frame built without one keeps its English label.
    null_model <- if (is.null(lrt$family_key)) {
      lrt$family_label %||% "no-random model"
    } else {
      # A number among the holes (the theta of a negative binomial) is
      # formatted as the statistic beside it.
      args <- lapply(lrt$family_args, function(a) {
        if (is.numeric(a)) .footer_num(a, 2L, decimal_mark) else a
      })
      do.call(spicy_fmt, c(list(lrt$family_key), args))
    }
    lrt_part <- spicy_fmt(
      "note_re_lrt",
      null_model,
      as.integer(lrt$df),
      .footer_num(lrt$chi2, 2L, decimal_mark),
      p_str
    )
  }
  # Nothing informative (no method tag, no LR test): suppress entirely.
  if (!has_method && is.null(lrt_part)) {
    return(NULL)
  }
  if (is.null(lrt_part)) {
    return(spicy_fmt("note_re_line", header))
  }
  spicy_fmt("note_re_line_lrt", header, lrt_part)
}


build_singular_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  flags <- vapply(
    frames,
    function(f) {
      isTRUE(f$info$extras$has_singular)
    },
    logical(1)
  )
  if (!any(flags)) {
    return(NULL)
  }
  affected <- which(flags)
  is_mixed <- vapply(frames[affected], .is_mixed_frame, logical(1))
  if (length(frames) == 1L) {
    return(.singular_msg_for_frame(frames[[affected]], is_mixed[1L]))
  }
  per <- vapply(
    seq_along(affected),
    function(k) {
      .model_line(
        frames,
        affected[k],
        .singular_msg_for_frame(frames[[affected[k]]], is_mixed[k])
      )
    },
    character(1)
  )
  paste(per, collapse = "\n")
}

# Phase 7c21: distinguish the two singular regimes:
#   * lm / glm: rank-deficient fixed-effect design -- dropped coefs
#     render as -. ASCII en-dash in the renderer.
#   * lmer / glmer / glmmTMB / lme: variance components on the
#     boundary 0 (lme4::isSingular). Random-effect SE / CI are
#     suppressed in `.merMod_attach_wald_se_ci()` to avoid
#     reporting unreliable values; the footer now tells the
#     reader why.
.singular_msg_for_frame <- function(frame, is_mixed) {
  if (isTRUE(is_mixed)) {
    # Table notes state facts about what is shown; the actionable advice
    # ("consider simplifying the random structure") is for the ANALYST
    # at build time, not the reader of a published table -- it moved to
    # the consolidated spicy_caveat warning in table_regression().
    #
    # "at the boundary (0)" named only one of the two boundary regimes
    # and overstated the other: a collapsed component reaches 0 exactly
    # only under lme4 (glmmTMB and nlme optimize a log scale and land
    # near it), and a random-effect correlation pinned at +/-1 is a
    # boundary estimate with no zero variance at all.
    return(spicy_str("note_singular_mixed"))
  }
  # The note names the glyph the cells carry, so it takes it as an
  # argument rather than re-typing it.
  spicy_fmt("note_rank_deficient", spicy_str("cell_undefined"))
}

.is_mixed_frame <- function(frame) {
  cls <- frame$info$class %||% ""
  cls %in%
    c("lmerMod", "lmerModLmerTest", "glmerMod", "glmmTMB", "lme", "clmm")
}


# re_ci = "profile": the CI cells of the variance-component rows are
# profile-likelihood intervals (asymmetric; no SE by construction), so
# the footer must disclose the method -- a profile CI cannot be
# reconstructed from any SE, mirroring the fixed-effects profile
# disclosure for polr / clm.
build_re_profile_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  flags <- vapply(
    frames,
    function(f) {
      identical(f$info$extras$re_ci %||% "wald", "profile")
    },
    logical(1)
  )
  if (!any(flags)) {
    return(NULL)
  }
  # `re_ci` is a single table-wide argument and only mixed frames carry
  # variance-component rows, so one shared line is always unambiguous.
  spicy_str("note_re_profile_ci")
}


# Variance-component SE / CI omitted for size (extras$re_se_skipped_n,
# see .re_se_skipped_by_size): the note states the FACT about what the
# table shows; the advice (raise the cap, or use re_test) lives in the
# orchestrator's spicy_caveat warning, per the singular-fit precedent.
build_re_se_skipped_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  ns <- vapply(
    frames,
    function(f) {
      as.integer(f$info$extras$re_se_skipped_n %||% NA_integer_)
    },
    integer(1)
  )
  if (all(is.na(ns))) {
    return(NULL)
  }
  msg <- function(n) {
    spicy_fmt(
      "note_re_se_skipped",
      # No thousands separator: every other N the package prints is
      # bare, and an anglophone "," reads as a decimal under
      # decimal_mark = ",".
      format(n)
    )
  }
  affected <- which(!is.na(ns))
  if (length(frames) == 1L) {
    return(msg(ns[affected]))
  }
  # Every model affected with the same n (the common multi-model case):
  # one shared line instead of per-model repeats.
  if (
    length(affected) == length(frames) &&
      length(unique(ns[affected])) == 1L
  ) {
    return(msg(ns[affected][1L]))
  }
  per <- vapply(
    affected,
    function(k) {
      .model_line(frames, k, msg(ns[k]))
    },
    character(1)
  )
  paste(per, collapse = "\n")
}


# Why the ICC row is not there, when the reason is the model's own
# structure: `random_effects$icc_omitted` (see
# .merMod_icc_omitted_reason). A mixed table asks for `icc` by default
# and simply drops the row when it is NA, which leaves a reader of a
# two-factor fit comparing their table against a one-factor example and
# finding a line missing with nothing said. The row STAYS absent -- there
# is no single number it could hold -- and the footer states why.
#
# Gated on the token actually being asked for: a caller who dropped the
# fit-statistics block wants no fit-statistics prose either.
build_icc_omitted_footer_block_from_frames <- function(
  frames,
  show_fit_stats = character(0)
) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  if (!"icc" %in% show_fit_stats) {
    return(NULL)
  }
  reasons <- vapply(
    frames,
    function(f) {
      as.character(f$info$random_effects$icc_omitted %||% NA_character_)
    },
    character(1)
  )
  if (all(is.na(reasons))) {
    return(NULL)
  }
  # The vocabulary of `icc_omitted` is closed: `.merMod_icc_omitted_reason()`
  # emits "multi_group" or NA, and the NA half never reaches here. The
  # default therefore names an internal inconsistency rather than
  # returning a value -- because the value it used to return was
  # NA_character_, and a footer builder that hands back NA does not stay
  # silent: with one model it made the block itself NA, and with two it
  # composed the literal line "Model 1: NA". Measured, on a made-up
  # token. A reason token added without its sentence must fail here, not
  # print the word NA to a reader.
  msg <- function(reason) {
    switch(
      reason,
      multi_group = spicy_str("note_icc_multi_group"),
      spicy_abort(
        c(
          sprintf(
            "Internal: no footer sentence for `icc_omitted` reason %s.",
            sQuote(reason)
          ),
          "i" = paste0(
            "Add the reason to build_icc_omitted_footer_block_from_frames() ",
            "together with its i18n key."
          )
        ),
        class = "spicy_internal_invariant"
      )
    )
  }
  affected <- which(!is.na(reasons))
  # One shared line when every model says the same thing -- the reason is
  # a property of a structure, and several models usually share it.
  if (
    length(affected) == length(frames) &&
      length(unique(reasons[affected])) == 1L
  ) {
    return(msg(reasons[affected][1L]))
  }
  paste(
    vapply(
      affected,
      function(k) .model_line(frames, k, msg(reasons[k])),
      character(1)
    ),
    collapse = "\n"
  )
}


# Conditional-logit reference (base) alternative, read from
# extras$reference_alternative -- the discrete-choice sibling of the
# multinomial reference-outcome note below (Stata asclogit's "base
# alternative"). Fact-only; deduped across models like its sibling.
build_reference_alternative_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  refs <- vapply(
    frames,
    function(f) {
      as.character(f$info$extras$reference_alternative %||% NA_character_)
    },
    character(1)
  )
  if (all(is.na(refs))) {
    return(NULL)
  }
  affected <- which(!is.na(refs))
  msg <- function(ref) spicy_fmt("note_reference_alternative", ref)
  if (
    length(affected) == length(frames) &&
      length(unique(refs[affected])) == 1L
  ) {
    return(msg(refs[affected][1L]))
  }
  per <- vapply(
    affected,
    function(k) {
      .model_line(frames, k, msg(refs[k]))
    },
    character(1)
  )
  paste(per, collapse = "\n")
}


# Multinomial reference (base) outcome, read from
# extras$reference_outcome. Stata prints its "base outcome" under
# every mlogit table; a multinomial table is unreadable without it
# (every coefficient is a contrast AGAINST that category), so the
# note is emitted for every multinomial frame in BOTH layouts
# (outcome-as-columns and per-outcome rows). Fact-only.
build_reference_outcome_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  refs <- vapply(
    frames,
    function(f) {
      as.character(f$info$extras$reference_outcome %||% NA_character_)
    },
    character(1)
  )
  if (all(is.na(refs))) {
    return(NULL)
  }
  affected <- which(!is.na(refs))
  msg <- function(ref) spicy_fmt("note_reference_outcome", ref)
  # One shared line only when EVERY model in the table is multinomial
  # and shares one reference -- in a mixed-class table an unqualified
  # line would read as applying to the non-multinomial models too
  # (same all-affected bar as build_re_se_skipped above).
  if (
    length(affected) == length(frames) &&
      length(unique(refs[affected])) == 1L
  ) {
    return(msg(refs[affected][1L]))
  }
  per <- vapply(
    affected,
    function(k) {
      .model_line(frames, k, msg(refs[k]))
    },
    character(1)
  )
  paste(per, collapse = "\n")
}


# Frame-aware sibling of build_exponentiate_footer_block(). Reads
#   frame$info$extras$exp_applied (was extract$exp_applied)
#   frame$info$extras$exp_header  (was extract$exp_header)
#
# G4 disclosure: when an SE column is actually displayed, the sentence
# also states the SE scale (delta method; Stata [R] logistic convention
# se(OR) = OR x se(b)) and that the CI -- the exponential of the
# link-scale CI endpoints, Wald or profile alike -- is asymmetric. The
# table note says what IS shown; the negative-space explanation (the CI
# is NOT reconstructable as estimate +/- z x SE) is reader pedagogy and
# lives in ?table_regression, not in a publication table note. Without a
# visible SE column the short sentence suffices. In a table mixing
# exponentiated and identity-link models, the sentence is scoped to the
# exponentiated models (an lm's SE column in the same table is
# OLS-scale).
build_exponentiate_footer_block_from_frames <- function(
  frames,
  show_columns = character(0)
) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  applied <- vapply(
    frames,
    function(f) isTRUE(f$info$extras$exp_applied),
    logical(1)
  )
  if (!any(applied)) {
    return(NULL)
  }
  hdrs <- unique(vapply(
    frames[applied],
    function(f) f$info$extras$exp_header,
    character(1)
  ))

  scope <- if (all(applied)) {
    spicy_str("note_exp_scope_all")
  } else {
    spicy_fmt(
      "note_exp_scope_partial",
      paste(
        vapply(
          which(applied),
          function(i) .model_ref(frames, i),
          character(1)
        ),
        collapse = ", "
      )
    )
  }

  # Bayesian frames exponentiate the DRAWS (SE = posterior MAD SD of
  # exp(draws), exact) rather than crossing scales by the delta
  # method, and under ci_method = "hdi" the interval is recomputed on
  # the exponentiated draws rather than transformed -- the gloss must
  # say which mechanism produced the displayed numbers.
  is_bayes_f <- vapply(
    frames,
    function(f) {
      !is.null(f$info$extras$posterior_engine)
    },
    logical(1)
  )
  bayes_applied <- is_bayes_f & applied
  se_gloss <- if (all(is_bayes_f[applied])) {
    spicy_str("note_exp_se_gloss_bayes")
  } else if (any(bayes_applied)) {
    spicy_str("note_exp_se_gloss_mixed")
  } else {
    spicy_str("note_exp_se_gloss_delta")
  }
  any_hdi <- any(vapply(
    frames[applied],
    function(f) {
      identical(f$info$ci_method, "posterior_hdi")
    },
    logical(1)
  ))
  # A displayed SE column also makes the gloss say the interval is
  # asymmetric. The qualifier lives inside the key, so a language can
  # place it and agree it with its noun. Without a CI column the sentence
  # says nothing of an interval (the `_no_ci` keys).
  se_shown <- "se" %in% show_columns
  ci_shown <- "ci" %in% show_columns
  ci_gloss <- if (any_hdi) {
    spicy_str(
      if (se_shown) "note_exp_ci_gloss_hdi_asym" else "note_exp_ci_gloss_hdi"
    )
  } else {
    spicy_str(
      if (se_shown) {
        "note_exp_ci_gloss_standard_asym"
      } else {
        "note_exp_ci_gloss_standard"
      }
    )
  }

  # Cumulative-cloglog frames print the grouped-time proportional-
  # hazards ratio exp(-B) (the parametrization puts hazards on -B);
  # the sign convention must be disclosed next to the HR definition.
  hr_negated <- any(vapply(
    frames[applied],
    function(f) isTRUE(f$info$extras$exp_hr_negated),
    logical(1)
  ))
  negation_note <- if (hr_negated) {
    spicy_str("note_exp_hr_negated")
  } else {
    ""
  }

  single <- length(hdrs) == 1L
  shown <- if (single) hdrs[1L] else paste(hdrs, collapse = " / ")
  key <- if (se_shown && single) {
    if (ci_shown) "note_exp_with_se_single" else "note_exp_with_se_single_no_ci"
  } else if (se_shown) {
    if (ci_shown) "note_exp_with_se_multi" else "note_exp_with_se_multi_no_ci"
  } else if (single) {
    if (ci_shown) "note_exp_no_se_single" else "note_exp_no_se_single_no_ci"
  } else {
    if (ci_shown) "note_exp_no_se_multi" else "note_exp_no_se_multi_no_ci"
  }
  # The holes of the four sentences: the scope, the header (twice in the
  # single-family SE sentence), the SE gloss when an SE column is shown,
  # the CI gloss when a CI column is, the cloglog sentence.
  args <- c(
    list(scope, shown),
    if (se_shown && single) list(hdrs[1L]),
    if (se_shown) list(se_gloss),
    if (ci_shown) list(ci_gloss),
    list(negation_note)
  )
  do.call(spicy_fmt, c(list(key), args))
}


# Frame-aware sibling of build_p_adjust_footer_block().
# Column mapping (legacy -> frame, established in Phase 0b sub-step 2):
#   cf$estimate_type -> coefs$estimate_type   (same column name)
#   cf$is_intercept  -> derived (term == "(Intercept)")
#   cf$is_reference  -> coefs$is_ref          (renamed)
#   cf$p_value       -> coefs$p_value         (same)
build_p_adjust_footer_block_from_frames <- function(frames, p_adjust) {
  if (
    identical(p_adjust, "none") ||
      is.null(p_adjust) ||
      !is.list(frames) ||
      length(frames) == 0L
  ) {
    return(NULL)
  }
  sizes <- vapply(
    frames,
    function(f) {
      cf <- f$coefs
      if (is.null(cf) || nrow(cf) == 0L) {
        return(0L)
      }
      # Mirror apply_p_adjust_to_frame_coefs() exactly: component-block
      # intercepts ("zero_(Intercept)", "zi.(Intercept)") are excluded
      # from the family by their display LABEL -- counting them here made
      # the footer's m overstate the adjustment actually performed.
      # (label may be absent on synthetic frames; treat as non-intercept.)
      lbl <- cf$label %||% rep(NA_character_, nrow(cf))
      sum(
        cf$estimate_type == "B" &
          cf$term != "(Intercept)" &
          !(lbl %in% "(Intercept)") &
          !cf$is_ref &
          !is.na(cf$p_value)
      )
    },
    integer(1)
  )
  # No coefficient tested, nothing adjusted: no sentence, as in the
  # descriptive tables.
  if (all(sizes == 0L)) {
    return(NULL)
  }
  # The sentence of the descriptive tables (`.desc_p_adjust_note()`): each
  # coefficient is a test, and the family is one model.
  .p_adjust_sentence(p_adjust, sizes)
}


# Frame-aware sibling of build_polynomial_contrasts_footer_block().
# Column mapping:
#   coefs$factor_level -> coefs$label   (renamed; never NA in frame --
#                                        fallback to term for non-factor
#                                        predictors). The startsWith
#                                        check on "." or "^" still
#                                        correctly identifies poly rows.
#   coefs$factor_term  -> coefs$parent_var
build_polynomial_contrasts_footer_block_from_frames <- function(
  frames,
  displayed_parent_vars = NULL
) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  poly_vars <- character(0)
  poly_suffixes <- character(0)
  for (f in frames) {
    coefs <- f$coefs
    if (is.null(coefs) || nrow(coefs) == 0L) {
      next
    }
    is_poly <- !is.na(coefs$label) &
      (startsWith(coefs$label, ".") |
        startsWith(coefs$label, "^"))
    if (any(is_poly)) {
      poly_vars <- union(poly_vars, unique(coefs$parent_var[is_poly]))
      poly_suffixes <- union(poly_suffixes, unique(coefs$label[is_poly]))
    }
  }
  if (length(poly_vars) == 0L) {
    return(NULL)
  }
  # Phase 7c22 (item f): only emit the polynomial-trends note when the
  # ordered factor SURVIVES the keep / drop display filter. Without
  # the filter, the note would describe a variable that the reader
  # doesn't actually see in the table.
  if (!is.null(displayed_parent_vars)) {
    poly_vars <- intersect(poly_vars, displayed_parent_vars)
    if (length(poly_vars) == 0L) return(NULL)
  }
  inform_polynomial_pedagogy()

  vars_str <- paste0("`", poly_vars, "`", collapse = ", ")
  plural <- length(poly_vars) > 1L

  suffix_rank <- function(s) {
    if (s == ".L") {
      return(1)
    }
    if (s == ".Q") {
      return(2)
    }
    if (s == ".C") {
      return(3)
    }
    if (startsWith(s, "^")) {
      n <- suppressWarnings(as.integer(substring(s, 2L)))
      if (!is.na(n)) return(as.numeric(n))
    }
    NA_real_
  }
  suffix_label <- function(s) {
    switch(
      s,
      ".L" = spicy_str("note_poly_degree_linear"),
      ".Q" = spicy_str("note_poly_degree_quadratic"),
      ".C" = spicy_str("note_poly_degree_cubic"),
      if (startsWith(s, "^")) {
        n <- suppressWarnings(as.integer(substring(s, 2L)))
        if (!is.na(n)) ordinal_label(n) else s
      } else {
        s
      }
    )
  }
  suff_ord <- order(vapply(poly_suffixes, suffix_rank, numeric(1)))
  poly_suffixes <- poly_suffixes[suff_ord]
  legend <- paste(
    vapply(
      poly_suffixes,
      function(s) sprintf("%s = %s", s, suffix_label(s)),
      character(1)
    ),
    collapse = ", "
  )

  spicy_fmt(
    if (plural) "note_poly_trends_plural" else "note_poly_trends_singular",
    vars_str,
    legend
  )
}


# Ordinal labels for polynomial degrees >= 4 (.L .Q .C are
# special-cased in suffix_label). "^4" -> "quartic", "^5" -> "quintic",
# "^k" for k > 6 falls back to "degree-k".
ordinal_label <- function(k) {
  switch(
    as.character(k),
    "4" = spicy_str("note_poly_degree_quartic"),
    "5" = spicy_str("note_poly_degree_quintic"),
    "6" = spicy_str("note_poly_degree_sextic"),
    spicy_fmt("note_poly_degree_generic", k)
  )
}

# Pedagogical inform fired ONCE per R session the first time any
# ordered (contr.poly) factor is rendered by table_regression().
# Uses rlang::inform's built-in frequency mechanism: subsequent
# calls in the same session with the same `.frequency_id` are
# muffled. Resets at the start of a new R session, which is the
# correct cadence for a "did you know" hint.
inform_polynomial_pedagogy <- function() {
  rlang::inform(
    c(
      paste0(
        "Ordered factor(s) detected. Polynomial contrasts ",
        "(the R default for `ordered()`) decompose the factor ",
        "into orthogonal trend components: `.L` = linear, ",
        "`.Q` = quadratic, `.C` = cubic, `^k` = degree k. ",
        "Coefficients are trends across the ordered levels, ",
        "NOT per-level effects against a reference."
      ),
      "i" = paste0(
        "To display per-level (treatment) effects, refit ",
        "with `factor(x, ordered = FALSE)` or set ",
        "`options(contrasts = c(\"contr.treatment\", ",
        "\"contr.treatment\"))`."
      )
    ),
    # rlang::inform is called directly (spicy_inform has no
    # .frequency passthrough); the spicy_info parent is attached
    # by hand so the spicy_inform() contract holds regardless.
    class = c("spicy_polynomial_contrasts_info", "spicy_info"),
    .frequency = "once",
    .frequency_id = "spicy_polynomial_contrasts_info"
  )
}


# Frame-aware sibling of build_reference_categories_footer_block().
# Column mapping:
#   coefs$is_reference -> coefs$is_ref       (renamed)
#   coefs$factor_term  -> coefs$parent_var   (renamed; fallback to term
#                                             handled at reshape time;
#                                             for is_ref rows the
#                                             parent_var IS the factor
#                                             variable name)
#   coefs$factor_level -> coefs$label        (renamed; for is_ref rows
#                                             the label IS the reference
#                                             level name)
build_reference_categories_footer_block_from_frames <- function(
  frames,
  reference_style
) {
  if (!identical(reference_style, "footer")) {
    return(NULL)
  }
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  pairs <- character(0)
  seen <- character(0)
  for (f in frames) {
    coefs <- f$coefs
    if (is.null(coefs) || nrow(coefs) == 0L) {
      next
    }
    ref_rows <- coefs[isTRUE_vec(coefs$is_ref), , drop = FALSE]
    if (nrow(ref_rows) == 0L) {
      next
    }
    for (i in seq_len(nrow(ref_rows))) {
      ft <- ref_rows$parent_var[i]
      lvl <- ref_rows$label[i]
      if (is.na(ft) || !nzchar(ft) || is.na(lvl) || !nzchar(lvl)) {
        next
      }
      key <- paste0(ft, "=", lvl)
      if (key %in% seen) {
        next
      }
      seen <- c(seen, key)
      pairs <- c(pairs, sprintf("%s = %s", ft, lvl))
    }
  }
  if (!length(pairs)) {
    return(NULL)
  }
  spicy_fmt(
    "note_reference_categories",
    paste(pairs, collapse = spicy_str("note_reference_categories_sep"))
  )
}


# Vectorized isTRUE: maps each element to TRUE iff it is TRUE
# (length-1, non-NA, non-FALSE). Used in the footer collector
# above where `coefs$is_reference` may carry NAs for non-factor
# rows in some bind paths.
isTRUE_vec <- function(x) {
  !is.na(x) & as.logical(x)
}


# ---- Theme: nested declaration (Q6) --------------------------------------

# Returns NULL by design: the nested comparison block emitted by
# format_nested_comparison_footer() (Step 9) is self-explanatory under
# its "-- Model comparison --" header. Adding a paragraph that
# announces it would just be redundant noise above the block. The
# function is kept as a hook so Phase 2/3 can reintroduce
# class-specific declarations (LRT for glm, REML for merMod, etc.)
# without changing the dispatcher.
build_nested_footer_block <- function(nested) {
  NULL
}


# Footer gloss for the "Scale effects" block of a heteroskedastic clm
# (`scale = ~`). The coefficients act on log(sigma) of the latent variable,
# so exp(zeta) is a ratio of latent standard deviations -- NOT an odds ratio.
# The rows are materialized after exponentiation, so under exponentiate = TRUE
# they stay on the log scale and the note says so.
build_scale_effects_footer_block_from_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  has_scale <- any(vapply(
    frames,
    function(f) {
      ft <- f$coefs$parent_var
      # Deliberately the ONE block, not `.reg_is_block()`: this gloss
      # explains the scale equation and nothing else.
      !is.null(ft) && any(ft %in% .REG_BLOCK_SCALE)
    },
    logical(1)
  ))
  if (!has_scale) {
    return(NULL)
  }
  gloss <- spicy_fmt(
    "note_scale_effects_gloss",
    spicy_str("label_block_scale_effects")
  )
  exp_any <- any(vapply(
    frames,
    function(f) {
      isTRUE(f$info$extras$exp_applied)
    },
    logical(1)
  ))
  if (isTRUE(exp_any)) {
    gloss <- paste0(gloss, spicy_str("note_scale_effects_not_exp"))
  }
  paste0(gloss, ".")
}


# One Bayesian footer note, under the table's decimal mark.
#
# The Stan frame builds its two notes at BUILD time, where the mark is
# not known -- a frame is the model's data and carries no typography.
# So it stores the numeric diagnostics beside the baked sentence, and
# the render layer replays the SAME producer with the table's mark.
# Under a point the replay is byte-identical to the stored string (same
# function, same arguments), which is what makes this safe; a frame
# with no diagnostics (glmmTMB's convergence verdict, a test stub, a
# frame from an older cached fixture) keeps its string verbatim.
.marked_stan_note <- function(numbers, baked, producer, decimal_mark) {
  if (is.list(numbers)) {
    out <- tryCatch(
      list(value = producer(numbers, decimal_mark)),
      error = function(e) NULL
    )
    if (!is.null(out)) {
      return(as.character(out$value %||% NA_character_))
    }
    # An ERROR from the producer -- a malformed slot on a foreign or
    # future-schema frame -- falls back to the baked string the frame
    # carries for exactly that case. A legitimate NULL (the producer
    # has nothing to say) does NOT: the two must stay distinct, or a
    # quiet diagnostics set would resurrect a stale sentence.
  }
  as.character(baked %||% NA_character_)
}


# Footer note for the PSIS-LOO fit-stat rows: the elpd SE, without
# which the ELPD / LOOIC rows cannot support a comparison judgment.
# Same dedupe convention as the sibling builders.
#
# The sentence is re-rendered HERE, under the table's mark, from the
# numeric diagnostics the frame carries (`extras$stan_loo`) -- the
# frame's baked `loo_note` is the same producer's output at a point
# and stays the fallback for anything that supplies only the string.
build_loo_footer_block_from_frames <- function(frames, decimal_mark = ".") {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  notes <- vapply(
    frames,
    function(f) {
      .marked_stan_note(
        f$info$extras$stan_loo,
        f$info$extras$loo_note,
        .stan_loo_text,
        decimal_mark
      )
    },
    character(1)
  )
  if (all(is.na(notes))) {
    return(NULL)
  }
  affected <- which(!is.na(notes))
  # The bare (unattributed) note is only honest when EVERY model in
  # the table carries the same note: in a mixed frequentist+Bayesian
  # table the SE(ELPD) and its reliability caveats are per-model
  # facts and keep their "Model k:" prefix (same convention as the
  # convergence and reference-outcome builders).
  if (
    length(unique(notes[affected])) == 1L &&
      length(affected) == length(notes)
  ) {
    return(notes[affected][1L])
  }
  paste(
    vapply(
      affected,
      function(k) {
        .model_line(frames, k, notes[k])
      },
      character(1)
    ),
    collapse = "\n"
  )
}


# Footer note from an engine's own convergence verdict, whatever the
# engine: the Bayesian sampler-diagnostics guard (max R-hat / min ESS /
# divergent transitions past their Vehtari-et-al. targets) and the
# glmmTMB optimizer's non-convergence flags both fill
# extras$convergence_note. NULL -- and therefore silent -- for a fit
# whose engine reports no problem.
#
# The Bayesian arm carries numeric diagnostics as well (R-hat, E-BFMI),
# so its sentence is re-rendered here under the table's mark; the
# glmmTMB arm has no number in it and passes through as the string its
# engine wrote.
build_convergence_footer_block_from_frames <- function(
  frames,
  decimal_mark = "."
) {
  if (!is.list(frames) || length(frames) == 0L) {
    return(NULL)
  }
  notes <- vapply(
    frames,
    function(f) {
      .marked_stan_note(
        f$info$extras$stan_convergence,
        f$info$extras$convergence_note,
        .stan_convergence_text,
        decimal_mark
      )
    },
    character(1)
  )
  if (all(is.na(notes))) {
    return(NULL)
  }
  affected <- which(!is.na(notes))
  if (
    length(unique(notes[affected])) == 1L && length(affected) == length(notes)
  ) {
    return(notes[affected][1L])
  }
  paste(
    vapply(
      affected,
      function(k) {
        .model_line(frames, k, notes[k])
      },
      character(1)
    ),
    collapse = "\n"
  )
}
