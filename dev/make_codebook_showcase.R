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
  output = pdf
)
code_book(
  sochealth,
  title = "Social health survey",
  subtitle = "Simulated data shipped with spicy",
  authors = authors,
  notes = notes_en,
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
    output = file.path(dir, "sochealth_codebook_fr.pdf")
  )
)

# ---- Pages ------------------------------------------------------------------
# Whole A4 pages, as the site of lssdoc shows its documents, with a
# hairline round the page so that its edge shows on a white background.
dpi <- 150
pages <- pdftools::pdf_data(pdf)
render_page <- function(page, file) {
  tmp <- tempfile(fileext = ".png")
  # pdf_convert() runs the file name through sprintf(): a warning for a
  # name without a format.
  suppressWarnings(
    pdftools::pdf_convert(pdf, pages = page, dpi = dpi, filenames = tmp, verbose = FALSE)
  )
  img <- magick::image_border(magick::image_read(tmp), "#D3DCE2", "1x1")
  magick::image_write(img, file, format = "png")
  unlink(tmp)
}
render_page(1, file.path(dir, "cover.png"))
render_page(2, file.path(dir, "about.png"))
# The page of one sheet: where the variable heads a band, between the
# list of variables (page 3) and the index (the last page).
sheet_page <- function(name) {
  hits <- which(vapply(pages, function(d) any(d$text == name), logical(1)))
  page <- setdiff(hits, c(3, length(pages)))
  stopifnot(length(page) == 1)
  page
}
s <- list(page = sheet_page("self_rated_health"))
render_page(s$page, file.path(dir, "sheet.png"))

for (f in list.files(dir, full.names = TRUE)) {
  cat(sprintf("%-28s %6.0f KB\n", basename(f), file.size(f) / 1024))
}
cat("pages:", length(pages), " sheet page:", s$page, "\n")
