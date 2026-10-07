# Prototype generator: data codebook of spicy::sochealth rendered to PDF
# through Typst, with lssdoc's design tokens.
#
# R computes everything (counts, percentages, French number formats) and
# writes one JSON file. The layout lives in codebook-template.typ, which
# reads that JSON. codebook.typ (two lines) is the entry point compiled by
# the Typst bundled with Quarto: `quarto typst compile codebook.typ`.

proto <- "C:/Users/at/AppData/Local/Temp/claude/c--Users-at-Documents-R-Packages-spicy/321a2714-db45-404d-ba63-04c01dd7707f/scratchpad/codebook_typst_proto"
quarto <- "C:/Program Files/Quarto/bin/quarto.exe"

suppressMessages(devtools::load_all("C:/Users/at/Documents/R/Packages/spicy", quiet = TRUE))
d <- sochealth

# ---- In-memory changes to the demo data --------------------------------
# 1. Two declared missing codes on self_rated_health (labelled_spss).
srh <- d$self_rated_health
codes <- as.integer(srh)
na_pos <- which(is.na(codes))
codes[na_pos[1:9]] <- 8L
codes[na_pos[10:15]] <- 9L          # the last 5 stay system NA
d$self_rated_health <- haven::labelled_spss(
  codes,
  labels = c(stats::setNames(seq_along(levels(srh)), levels(srh)),
             "Don't know" = 8L, "Refused" = 9L),
  na_values = c(8L, 9L),
  label = attr(srh, "label")
)
# 2. A character identifier (for the "texte" fiche).
d$id <- sprintf("R%04d", seq_len(nrow(d)))
attr(d$id, "label") <- "Respondent identifier"
# 3. One artificial categorical variable with 60 labelled categories.
communes <- c(
  "Lausanne", "Gen\u00e8ve", "Fribourg", "Neuch\u00e2tel", "Sion",
  "Yverdon-les-Bains", "Montreux", "Renens", "Nyon", "Vevey", "Pully",
  "Morges", "Gland", "\u00c9cublens", "Prilly", "La Chaux-de-Fonds",
  "Le Locle", "Bulle", "Martigny", "Monthey", "Sierre", "Del\u00e9mont",
  "Porrentruy", "Carouge", "Vernier", "Lancy", "Meyrin", "Onex",
  "Th\u00f4nex", "Versoix", "Bernex", "Plan-les-Ouates", "Ch\u00eane-Bougeries",
  "Grand-Saconnex", "Veyrier", "Aigle", "Bex", "Payerne", "Moudon",
  "\u00c9challens", "Lutry", "Bussigny", "Crissier", "Chavannes-pr\u00e8s-Renens",
  "Le Mont-sur-Lausanne", "Epalinges", "La Tour-de-Peilz", "Rolle", "Coppet",
  "Avenches", "Estavayer", "Romont", "Ch\u00e2tel-Saint-Denis",
  "Villars-sur-Gl\u00e2ne", "Marly", "Morat", "Boudry", "Val-de-Travers",
  "Saint-Imier", "Moutier"
)
stopifnot(length(communes) == 60L, !anyDuplicated(communes))
set.seed(20261006)
com <- sample.int(60L, nrow(d), replace = TRUE, prob = 1 / seq_len(60)^0.8)
com[sample.int(nrow(d), 7L)] <- NA_integer_
d$commune <- haven::labelled(com, labels = stats::setNames(seq_len(60L), communes),
                             label = "Municipality of residence")
# File order: id first, commune right after region.
ord <- c("id", names(sochealth))
ord <- append(ord, "commune", after = match("region", ord))
d <- d[ord]

# Source codes ("Question" line) declared for a few variables.
source_codes <- c(sex = "Q1", age = "Q2", self_rated_health = "Q12",
                  smoking = "Q15", institutional_trust = "Q21")

# ---- Formatting helpers (French) ----------------------------------------
nnbsp <- "\u202f"                       # narrow no-break space
fr_int <- function(x) formatC(x, format = "d", big.mark = nnbsp)
fr_num <- function(x, digits) {
  formatC(x, format = "f", digits = digits, big.mark = nnbsp, decimal.mark = ",")
}
fr_pct <- function(x) fr_num(x, 1)
fr_range <- function(x) {                 # min/max without trailing zeros
  if (all(x == round(x))) fr_int(x) else fr_num(x, 2)
}

type_fr <- function(x) {
  if (inherits(x, "haven_labelled")) return("entier \u00e9tiquet\u00e9")
  if (is.ordered(x)) return("facteur ordonn\u00e9")
  if (is.factor(x)) return("facteur")
  if (inherits(x, "POSIXct")) return("date-heure")
  if (inherits(x, "Date")) return("date")
  if (is.character(x)) return("texte")
  if (is.logical(x)) return("logique")
  if (is.integer(x)) return("entier")
  if (is.numeric(x)) return("num\u00e9rique")
  class(x)[1]
}
kind_of <- function(x) {
  if (inherits(x, "haven_labelled") || is.factor(x)) return("categorical")
  if (inherits(x, c("POSIXct", "Date"))) return("date")
  if (is.character(x)) return("character")
  "numeric"
}

# Missing in the codebook sense: system NA plus declared missing codes.
is_missing <- function(x) {
  if (inherits(x, "haven_labelled_spss")) {
    v <- unclass(x)
    return(is.na(v) | v %in% attr(x, "na_values"))
  }
  is.na(x)
}

freq_rows <- function(x) {
  n_tot <- length(x)
  miss <- is_missing(x)
  n_val <- sum(!miss)
  if (is.factor(x)) {
    codes <- seq_along(levels(x)); labs <- levels(x); v <- as.integer(x)
    na_codes <- integer(0)
  } else {
    v <- as.vector(unclass(x))
    lab <- attr(x, "labels")
    na_codes <- if (inherits(x, "haven_labelled_spss")) attr(x, "na_values") else integer(0)
    codes <- sort(unique(c(unname(lab), v[!is.na(v)])))
    labs <- vapply(codes, function(cd) {
      hit <- names(lab)[lab == cd]
      if (length(hit)) hit[1] else ""
    }, character(1))
  }
  rows <- lapply(seq_along(codes), function(i) {
    n <- sum(v == codes[i], na.rm = TRUE)
    m <- codes[i] %in% na_codes
    list(code = as.character(codes[i]), label = labs[i], missing = m,
         n = fr_int(n), pct = fr_pct(100 * n / n_tot),
         vpct = if (m) "" else fr_pct(100 * n / n_val))
  })
  # Valid codes first, declared missing codes after, then system NA.
  rows <- c(Filter(function(r) !r$missing, rows), Filter(function(r) r$missing, rows))
  n_na <- sum(is.na(v))
  if (n_na > 0) {
    rows[[length(rows) + 1L]] <- list(
      code = "NA", label = "manquant syst\u00e8me", missing = TRUE,
      n = fr_int(n_na), pct = fr_pct(100 * n_na / n_tot), vpct = "")
  }
  list(rows = rows,
       total = list(n = fr_int(n_tot), n_valid = fr_int(n_val),
                    pct = fr_pct(100), vpct = fr_pct(100)))
}

describe_var <- function(nm, pos) {
  x <- d[[nm]]
  miss <- is_missing(x)
  out <- list(
    pos = pos, name = nm,
    label = { l <- attr(x, "label", exact = TRUE); if (is.null(l)) "" else l },
    type = type_fr(x), kind = kind_of(x),
    valid = fr_int(sum(!miss)), missing = fr_int(sum(miss)),
    source = if (nm %in% names(source_codes)) unname(source_codes[nm]) else NULL
  )
  if (out$kind == "categorical") {
    f <- freq_rows(x)
    out$rows <- f$rows
    out$total <- f$total
    out$breakable <- length(f$rows) > 30L      # only the long fiche may split
  } else if (out$kind == "numeric") {
    xv <- as.numeric(x[!miss])
    rng <- fr_range(range(xv))            # same number of decimals for both
    out$stats <- list(mean = fr_num(mean(xv), 2), sd = fr_num(stats::sd(xv), 2),
                      min = rng[1], max = rng[2])
  } else if (out$kind == "date") {
    xv <- x[!miss]
    fmt <- if (inherits(x, "POSIXct") &&
               any(format(xv, "%H:%M:%S") != "00:00:00")) "%Y-%m-%d %H:%M" else "%Y-%m-%d"
    out$stats <- list(min = format(min(xv), fmt), max = format(max(xv), fmt))
  } else {
    out$stats <- list(distinct = fr_int(length(unique(x[!miss]))))
  }
  out
}

vars <- Map(describe_var, names(d), seq_along(d))
names(vars) <- NULL

# Declared missing codes found in the file (for the cover).
decl <- unlist(lapply(names(d), function(nm) {
  nv <- attr(d[[nm]], "na_values")
  if (length(nv)) paste0(nm, "\u00a0: ", paste(nv, collapse = ", "))
}))

mois <- c("janvier", "f\u00e9vrier", "mars", "avril", "mai", "juin", "juillet",
          "ao\u00fbt", "septembre", "octobre", "novembre", "d\u00e9cembre")
today <- Sys.Date()
meta <- list(
  title = "Enqu\u00eate sant\u00e9 et soci\u00e9t\u00e9",
  subtitle = "Dictionnaire des variables",
  date_long = sprintf("%d %s %s", as.integer(format(today, "%d")),
                      mois[as.integer(format(today, "%m"))], format(today, "%Y")),
  fields = list(
    list(key = "Jeu de donn\u00e9es", value = "sochealth (package spicy)"),
    list(key = "Observations", value = fr_int(nrow(d))),
    list(key = "Variables", value = fr_int(ncol(d))),
    list(key = "Manquants d\u00e9clar\u00e9s", value = paste(decl, collapse = "\n")),
    list(key = "G\u00e9n\u00e9r\u00e9", value = format(Sys.time(), "%Y-%m-%d %H:%M"))
  ),
  notes = c(
    "Donn\u00e9es simul\u00e9es fournies avec le package spicy, \u00e0 des fins de d\u00e9monstration.",
    "Les variables id et commune ont \u00e9t\u00e9 ajout\u00e9es pour ce prototype. Les codes 8 et 9 de self_rated_health sont des manquants d\u00e9clar\u00e9s ajout\u00e9s en m\u00e9moire."
  )
)

jsonlite::write_json(list(meta = meta, vars = vars),
                     file.path(proto, "codebook-data.json"),
                     auto_unbox = TRUE, pretty = TRUE, null = "null")
writeLines(c('#import "codebook-template.typ": codebook',
             '#codebook(json("codebook-data.json"))'),
           file.path(proto, "codebook.typ"))

# ---- Compile ---------------------------------------------------------------
old <- setwd(proto)
t0 <- Sys.time()
status <- system2(quarto, c("typst", "compile", "codebook.typ", "codebook.pdf"),
                  stdout = TRUE, stderr = TRUE)
t1 <- Sys.time()
setwd(old)
if (length(status)) cat(status, sep = "\n")
cat(sprintf("compile time: %.2f s\n", as.numeric(difftime(t1, t0, units = "secs"))))
cat("variables:", length(vars), " pages:", pdftools::pdf_info(file.path(proto, "codebook.pdf"))$pages, "\n")
