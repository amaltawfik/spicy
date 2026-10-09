# The showcase codebook of sochealth, for the README and the article
# "Explore variables and build codebooks": the PDF in English and in
# French, the Excel workbook, and three page excerpts as PNG. Run it from
# the package root after any change to the output of code_book(), and
# before a release (dev/02_release_cran.R):
#
#   Rscript dev/make_codebook_showcase.R
#
# The files go to pkgdown/assets/codebook/, which pkgdown copies to the
# root of the site: https://amaltawfik.github.io/spicy/codebook/<file>.
# Nothing here enters the tarball (`^pkgdown$` is in .Rbuildignore).
# Needs Quarto 1.7 (Typst), pdftools and magick.

devtools::load_all(".", quiet = TRUE)
dir <- "pkgdown/assets/codebook"
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

# Example authors, as in the rest of the documentation: the ORCID is the
# test record of ORCID itself (Josiah Carberry).
authors <- list(
  list(
    name = "Jane Doe",
    affiliation = "University of Somewhere",
    orcid = "0000-0002-1825-0097"
  ),
  list(name = "John Doe", affiliation = "Somewhere Institute of Public Health")
)
source <- stats::setNames(paste0("Q", seq_along(sochealth)), names(sochealth))

# The notes show what a note can carry: paragraphs, list items, italics,
# bold, code, a web address and a DOI. Every statement is true of the
# data (see ?sochealth).
notes_en <- c(
  "Simulated data shipped with *spicy* (`?sochealth`): 1,200 respondents of a fictitious social health survey, built to document the package. Nothing here describes a real population.",
  "Source and citation: https://amaltawfik.github.io/spicy/ and doi:10.32614/CRAN.package.spicy.",
  "- `weight` is the survey design weight (0.29 to 3.45): the counts of this codebook are **unweighted**.",
  "- `bmi` is in kg/m2, and `bmi_category` follows it (normal weight, overweight, obesity).",
  "- The four `life_sat_*` items run from 1 to 5 (Likert scale).",
  "- `response_date` is the time of the interview, Europe/Zurich."
)
notes_fr <- c(
  "Données simulées livrées avec *spicy* (`?sochealth`) : 1 200 répondants d'une enquête fictive sur la santé sociale, construite pour documenter le package. Rien ici ne décrit une population réelle.",
  "Source et citation : https://amaltawfik.github.io/spicy/ et doi:10.32614/CRAN.package.spicy.",
  "- `weight` est le poids de sondage (0,29 à 3,45) : les effectifs de ce codebook sont **non pondérés**.",
  "- `bmi` est en kg/m2, et `bmi_category` en découle (poids normal, surpoids, obésité).",
  "- Les quatre items `life_sat_*` vont de 1 à 5 (échelle de Likert).",
  "- `response_date` est l'heure de l'entretien, Europe/Zurich."
)

pdf <- file.path(dir, "sochealth_codebook.pdf")
code_book(
  sochealth,
  title = "Social health survey",
  subtitle = "Simulated data shipped with spicy",
  authors = authors,
  notes = notes_en,
  source = source,
  output = pdf
)
code_book(
  sochealth,
  title = "Social health survey",
  subtitle = "Simulated data shipped with spicy",
  authors = authors,
  notes = notes_en,
  source = source,
  output = file.path(dir, "sochealth_codebook.xlsx")
)
withr::with_options(
  list(spicy.language = "fr"),
  code_book(
    sochealth,
    title = "Enquête sur la santé sociale",
    subtitle = "Données simulées livrées avec spicy",
    authors = authors,
    notes = notes_fr,
    source = source,
    output = file.path(dir, "sochealth_codebook_fr.pdf")
  )
)

# ---- Page excerpts -----------------------------------------------------------
# Cropped to their content from the words of the page (points, 72 per
# inch), rendered at `dpi`. The running header and the folio are left
# out; so is the colophon of the cover.
dpi <- 160
pages <- pdftools::pdf_data(pdf)
page_height <- pdftools::pdf_pagesize(pdf)$height[[1]]
body <- function(d) d[d$y > 60 & d$y + d$height < page_height - 60, ]

crop_page <- function(page, file, words, pad = 18) {
  box <- c(
    x0 = min(words$x),
    y0 = min(words$y),
    x1 = max(words$x + words$width),
    y1 = max(words$y + words$height)
  )
  tmp <- tempfile(fileext = ".png")
  # pdf_convert() runs the file name through sprintf(): a warning for a
  # name without a format.
  suppressWarnings(
    pdftools::pdf_convert(pdf, pages = page, dpi = dpi, filenames = tmp, verbose = FALSE)
  )
  s <- dpi / 72
  geometry <- sprintf(
    "%dx%d+%d+%d",
    round((box[["x1"]] - box[["x0"]] + 2 * pad) * s),
    round((box[["y1"]] - box[["y0"]] + 2 * pad) * s),
    round((box[["x0"]] - pad) * s),
    round((box[["y0"]] - pad) * s)
  )
  img <- magick::image_crop(magick::image_read(tmp), geometry)
  magick::image_write(img, file, format = "png")
  unlink(tmp)
}

# The cover, without the colophon at its foot.
cover <- pages[[1]]
crop_page(1, file.path(dir, "cover.png"), cover[cover$y < page_height * 0.75, ])
# The page about the data: facts, caution, notes.
crop_page(2, file.path(dir, "about.png"), body(pages[[2]]))
# One sheet: the page where the variable heads a band, between the list
# of variables and the index, from its band to the band of the next one.
sheet_of <- function(name, next_name) {
  hits <- which(vapply(pages, function(d) any(d$text == name), logical(1)))
  page <- setdiff(hits, c(3, length(pages)))
  stopifnot(length(page) == 1)
  d <- body(pages[[page]])
  top <- min(d$y[d$text == name])
  after <- d$y[d$text == next_name]
  bottom <- if (length(after)) min(after) - 24 else max(d$y + d$height)
  list(page = page, words = d[d$y >= top - 6 & d$y + d$height <= bottom, ])
}
s <- sheet_of("self_rated_health", "wellbeing_score")
crop_page(s$page, file.path(dir, "sheet.png"), s$words)

for (f in list.files(dir, full.names = TRUE)) {
  cat(sprintf("%-28s %6.0f KB\n", basename(f), file.size(f) / 1024))
}
cat("pages:", length(pages), " sheet page:", s$page, "\n")
