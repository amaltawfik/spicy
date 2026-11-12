# Fixtures of the three survey tables for a stratum with a single primary
# sampling unit (test-table_categorical_svy.R, test-table_continuous_svy.R,
# test-table_battery_svy.R).

# `apistrat` with one school left in stratum H: a lonely primary sampling
# unit in the whole design, which survey's default
# `options(survey.lonely.psu = "fail")` refuses. That school is in the
# `sch.wide == "No"` domain, so under `by = sch.wide` only that domain
# meets the lonely stratum.
.svy_lonely_design <- function() {
  skip_if_not_installed("survey", "4.5")
  data(api, package = "survey", envir = environment())
  keep <- c(
    which(apistrat$stype != "H"),
    which(apistrat$stype == "H")[1L]
  )
  survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = apistrat[keep, ]
  )
}

# `apistrat` whole, with a group `B` holding one school of stratum H: no
# lonely unit in the design, one in the domain `B`, which survey reports
# only under `options(survey.adjust.domain.lonely = TRUE)`, with a warning
# of its own rather than an error.
.svy_domain_lonely_design <- function() {
  skip_if_not_installed("survey", "4.5")
  data(api, package = "survey", envir = environment())
  d <- apistrat
  d$grp <- "A"
  d$grp[c(
    which(d$stype == "E")[1:20],
    which(d$stype == "M")[1:10],
    which(d$stype == "H")[1L]
  )] <- "B"
  survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = d)
}

# `apistrat` whole, with a group `B` holding one school of stratum H and a
# group `C` holding one school of stratum M: two domains with a lonely
# unit each, which survey reports under
# `options(survey.adjust.domain.lonely = TRUE)` in two messages.
.svy_two_domain_lonely_design <- function() {
  skip_if_not_installed("survey", "4.5")
  data(api, package = "survey", envir = environment())
  d <- apistrat
  e <- which(d$stype == "E")
  m <- which(d$stype == "M")
  d$grp <- "A"
  d$grp[c(e[1:20], m[1:10], which(d$stype == "H")[1L])] <- "B"
  d$grp[c(e[21:40], m[11L])] <- "C"
  survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = d)
}

# survey's own warnings on the domains `B` and `C` of that design for
# `fun` on each of `vars`, the oracle of the deduplicated warnings: the
# estimates, the message of each domain, how many times it came, and the
# first condition.
.svy_domain_warnings <- function(d, vars, fun) {
  out <- list(est = list(), msg = character(0), n = integer(0), first = NULL)
  for (g in c("B", "C")) {
    ws <- list()
    for (v in vars) {
      r <- .svy_collect_warnings(fun(
        stats::as.formula(paste0("~", v)),
        subset(d, grp == g)
      ))
      out$est[[paste(g, v)]] <- as.numeric(stats::coef(r$value))
      ws <- c(ws, r$warnings)
    }
    out$msg[[g]] <- unique(vapply(ws, conditionMessage, character(1)))
    out$n[[g]] <- length(ws)
    out$first <- out$first %||% ws[[1L]]
  }
  out
}

# A message said `n` times, as a table says it once.
.svy_counted <- function(msg, n) {
  if (n > 1L) sprintf("%s\n(raised %d times for this table)", msg, n) else msg
}

# Every warning of a call, muffled and kept, with the value.
.svy_collect_warnings <- function(expr) {
  ws <- list()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      ws[[length(ws) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = ws)
}

.svy_failed_warnings <- function(ws) {
  Filter(function(w) inherits(w, "spicy_svy_estimation_failed"), ws)
}

# The sentence of the note, typed out: the oracle is the wording, not the
# registry.
.SVY_LONELY_EN <- "A dash marks an estimate the design could not provide: a stratum has a single primary sampling unit."
.SVY_LONELY_FR <- paste0(
  "Un tiret marque une estimation que le plan de sondage ne permet pas",
  intToUtf8(0xA0L),
  ": une strate ne compte qu'une seule unit",
  intToUtf8(0xE9L),
  " primaire de sondage."
)
.SVY_NOT_ESTIMATED_EN <- "A dash marks an estimate the design could not provide."
.SVY_NOT_ESTIMATED_FR <- "Un tiret marque une estimation que le plan de sondage ne permet pas."

# `apistrat` with one school left in stratum H (sch.wide "No") and one in
# stratum M (sch.wide "Yes"): two lonely strata, which the two groups of
# `sch.wide` meet one each.
.svy_two_lonely_design <- function() {
  skip_if_not_installed("survey", "4.5")
  data(api, package = "survey", envir = environment())
  keep <- c(
    which(apistrat$stype == "E"),
    which(apistrat$stype == "H" & apistrat$sch.wide == "No")[1L],
    which(apistrat$stype == "M" & apistrat$sch.wide == "Yes")[1L]
  )
  survey::svydesign(
    id = ~1,
    strata = ~stype,
    weights = ~pw,
    data = apistrat[keep, ]
  )
}

# Two stages, no strata: the first of six primary units holds one sampled
# secondary unit out of ten, a lonely unit at stage 2.
.svy_stage2_design <- function() {
  skip_if_not_installed("survey", "4.5")
  set.seed(7)
  d <- data.frame(
    psu = rep(1:6, c(1, 3, 3, 3, 3, 3)),
    ssu = c(1, rep(1:3, 5)),
    fpc1 = 30,
    fpc2 = 10,
    y = round(stats::rnorm(16, 50, 10), 1)
  )
  d$ssu_id <- paste(d$psu, d$ssu)
  survey::svydesign(ids = ~ psu + ssu_id, fpc = ~ fpc1 + fpc2, data = d)
}

# Where each string first occurs in a note, in the order given.
.svy_note_positions <- function(note, strings) {
  vapply(
    strings,
    function(s) regexpr(s, note, fixed = TRUE)[[1L]],
    integer(1)
  )
}
