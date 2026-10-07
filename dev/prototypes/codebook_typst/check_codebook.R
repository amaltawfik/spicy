# Programmatic checks of codebook.pdf.
# A = Typst introspection (typst eval / query on the source).
# B = the PDF alone (pdftools word positions and fonts, raw link annotations),
#     independent of Typst's own bookkeeping.
proto <- "C:/Users/at/AppData/Local/Temp/claude/c--Users-at-Documents-R-Packages-spicy/321a2714-db45-404d-ba63-04c01dd7707f/scratchpad/codebook_typst_proto"
quarto <- "C:/Program Files/Quarto/bin/quarto.exe"
pdf <- file.path(proto, "codebook.pdf")
data <- jsonlite::read_json(file.path(proto, "codebook-data.json"))
vars <- vapply(data$vars, function(v) v$name, "")
long <- vars[vapply(data$vars, function(v) isTRUE(v$breakable), NA)]

typst_eval <- function(expr) {
  r <- processx::run(quarto, c("typst", "eval", expr, "--in", "codebook.typ"),
                     wd = proto, error_on_status = TRUE)
  # The quarto wrapper relays typst's result on stderr, not stdout.
  out <- if (nzchar(trimws(r$stdout))) r$stdout else r$stderr
  jsonlite::fromJSON(out, simplifyVector = FALSE)
}
pairs <- function(x) stats::setNames(vapply(x, function(p) as.integer(p[[2]]), 1L),
                                     vapply(x, function(p) p[[1]], ""))

# ---- A. Typst introspection --------------------------------------------------
a_start <- pairs(typst_eval("query(<fiche-start>).map(it => (it.value, it.location().page()))"))
a_end   <- pairs(typst_eval("query(<fiche-end>).map(it => (it.value, it.location().page()))"))
a_head  <- typst_eval("query(heading.where(level: 1)).map(it => (it.body.text, it.location().page()))")
a_head  <- pairs(a_head)

# ---- B. PDF only ---------------------------------------------------------------
pd <- pdftools::pdf_data(pdf, font_info = TRUE)
words <- do.call(rbind, lapply(seq_along(pd), function(p) cbind(page = p, pd[[p]])))
words <- words[order(words$page, words$y, words$x), ]
is_font <- function(pat) grepl(pat, words$font_name)

# Band variable names are the only Consolas-Bold words in the document.
band <- words[is_font("Consolas-Bold"), c("page", "y", "text")]
stopifnot(identical(band$text, vars))
b_start <- stats::setNames(band$page, band$text)
# The k-th grey note "Effectifs non ponderes..." closes the k-th fiche.
notes <- words[words$text == "Effectifs" & is_font("Calibri-Italic"), c("page", "y")]
stopifnot(nrow(notes) == length(vars))
b_end <- stats::setNames(notes$page, vars)

cat("== Check 1: fiches never split (except the long one) ==\n")
chk1 <- data.frame(var = vars, typst_start = a_start[vars], typst_end = a_end[vars],
                   pdf_start = b_start[vars], pdf_end = b_end[vars], row.names = NULL)
chk1$split <- chk1$pdf_end != chk1$pdf_start
print(chk1)
cat(sprintf("fiches: %d | split according to Typst: %s | split according to the PDF: %s | Typst and PDF agree: %s\n",
            length(vars),
            paste(vars[a_end[vars] != a_start[vars]], collapse = ", "),
            paste(vars[chk1$split], collapse = ", "),
            identical(unname(a_start[vars]), unname(b_start[vars])) &&
              identical(unname(a_end[vars]), unname(b_end[vars]))))
ordinary_split <- setdiff(vars[chk1$split], long)
cat("ordinary fiches split:", if (length(ordinary_split)) ordinary_split else "none", "\n\n")

cat("== Check 2: repeated header rows ==\n")
first_fiche_page <- min(b_start)
mono <- words[is_font("Consolas") & !is_font("Bold"), ]
list_pages <- sort(unique(mono$page[mono$page < first_fiche_page]))
for (p in list_pages) {
  w <- words[words$page == p, ]
  hdr_y <- w$y[w$text == "Manquants" & grepl("Calibri-Bold", w$font_name)]
  rows <- mono[mono$page == p, ]
  cat(sprintf("list page %d: header row at y = %s, first variable row at y = %d (%s), last at y = %d (%s), %d rows\n",
              p, paste(hdr_y, collapse = ","), min(rows$y), rows$text[which.min(rows$y)],
              max(rows$y), rows$text[which.max(rows$y)], nrow(rows)))
}
list_ok <- all(vapply(list_pages, function(p) {
  w <- words[words$page == p, ]
  hy <- w$y[w$text == "Manquants" & grepl("Calibri-Bold", w$font_name)]
  length(hy) == 1L && hy < min(mono$y[mono$page == p])
}, NA))
cat("list spans pages", paste(list_pages, collapse = ", "), "| header above the rows on every page:", list_ok, "\n")
for (v in long) {
  pages <- seq(b_start[[v]], b_end[[v]])
  for (p in pages) {
    w <- words[words$page == p, ]
    hy <- w$y[w$text == "valides" & grepl("Calibri-Bold", w$font_name)]
    # value rows of this fiche: codes in the code column, below its header
    codes <- w[(grepl("^[0-9]+$", w$text) | w$text == "NA") & w$x < 160 &
                 !grepl("Italic", w$font_name) &      # not the "dont 1 193 valides" cell
                 w$y > hy[1] & w$y < if (p == b_end[[v]]) notes$y[match(v, vars)] else Inf, ]
    cat(sprintf("long fiche %s, page %d: header row at y = %s, first value row y = %d (code %s), last value row code %s, %d value rows\n",
                v, p, paste(hy, collapse = ","), min(codes$y),
                codes$text[which.min(codes$y)], codes$text[which.max(codes$y)], nrow(codes)))
  }
}
cat("\n")

cat("== Check 3: index page numbers and links ==\n")
last_fiche_page <- max(b_end)
idx_words <- words[words$page > last_fiche_page, ]
idx_names <- idx_words[grepl("Consolas", idx_words$font_name), ]
idx <- do.call(rbind, lapply(seq_len(nrow(idx_names)), function(i) {
  r <- idx_names[i, ]
  same <- idx_words[idx_words$page == r$page & abs(idx_words$y - r$y) <= 1 &
                      grepl("^[0-9]+$", idx_words$text), ]
  same <- same[order(same$x), ]
  data.frame(var = r$text, page_in_index = as.integer(utils::tail(same$text, 1)),
             index_page = r$page, y = r$y)
}))
idx$pdf_fiche_page <- b_start[idx$var]
idx$typst_fiche_page <- a_start[idx$var]
cat("index entries:", nrow(idx), "| alphabetical:", identical(idx$var, vars[order(tolower(vars))]),
    "| page numbers equal to the PDF page of the fiche:", sum(idx$page_in_index == idx$pdf_fiche_page),
    "of", nrow(idx), "| equal to Typst's location:", sum(idx$page_in_index == idx$typst_fiche_page), "\n")

# Raw link annotations: /Annots of each page, /Dest -> destination page.
raw <- readBin(pdf, "raw", file.info(pdf)$size)
txt <- rawToChar(raw[raw != as.raw(0)])
kids <- regmatches(txt, regexpr("/Type/Pages/Count [0-9]+/Kids\\[[^]]*\\]", txt, useBytes = TRUE))
page_ids <- as.integer(regmatches(kids, gregexpr("[0-9]+(?= 0 R)", kids, perl = TRUE))[[1]])
obj <- function(id) {
  m <- regexpr(paste0("(?s)\n", id, " 0 obj\n(.*?)\nendobj"), txt, perl = TRUE, useBytes = TRUE)
  sub(paste0("(?s)^\n", id, " 0 obj\n"), "", sub("\nendobj$", "", regmatches(txt, m), useBytes = TRUE), perl = TRUE, useBytes = TRUE)
}
links <- do.call(rbind, lapply(seq_along(page_ids), function(p) {
  pg <- obj(page_ids[p])
  a <- regmatches(pg, regexpr("/Annots\\[[^]]*\\]", pg, useBytes = TRUE))
  if (!length(a)) return(NULL)
  ids <- as.integer(regmatches(a, gregexpr("[0-9]+(?= 0 R)", a, perl = TRUE))[[1]])
  do.call(rbind, lapply(ids, function(id) {
    an <- obj(id)
    rect <- as.numeric(strsplit(sub(".*/Rect\\[([^]]*)\\].*", "\\1", an), " ")[[1]])
    dest <- as.integer(sub(".*/Dest ([0-9]+) 0 R.*", "\\1", an))
    d <- obj(dest)
    target <- match(as.integer(sub("^\\[([0-9]+) 0 R.*", "\\1", d)), page_ids)
    data.frame(page = p, x0 = rect[1], y0 = rect[2], x1 = rect[3], y1 = rect[4], target = target)
  }))
}))
cat("link annotations:", nrow(links), "| all resolve to a page:", all(!is.na(links$target)), "\n")
print(table(on_page = links$page))
# Index rows: the link drawn over the page number must jump to the fiche page.
ph <- pdftools::pdf_pagesize(pdf)$height[1]
idx$link_target <- vapply(seq_len(nrow(idx)), function(i) {
  l <- links[links$page == idx$index_page[i], ]
  ytop <- ph - l$y1; ybot <- ph - l$y0                      # PDF y is from the bottom
  hit <- l[ytop <= idx$y[i] + 6 & ybot >= idx$y[i] & l$x0 > 200, ]
  if (nrow(hit)) hit$target[1] else NA_integer_
}, 1L)
print(idx[, c("var", "page_in_index", "pdf_fiche_page", "typst_fiche_page", "link_target")], row.names = FALSE)
cat("index links landing on the fiche page:", sum(idx$link_target == idx$pdf_fiche_page, na.rm = TRUE),
    "of", nrow(idx), "\n\n")

cat("== Bonus: table of contents ==\n")
toc_page <- 2L
tw <- words[words$page == toc_page & grepl("Calibri", words$font_name), ]
for (h in c("Liste", "Fiches", "Index")) {
  y <- tw$y[tw$text == h][1]
  line <- tw[abs(tw$y - y) <= 1, ]
  num <- utils::tail(line$text[grepl("^[0-9]+$", line$text)], 1)
  real <- a_head[grep(paste0("^", h), names(a_head))]
  cat(sprintf("TOC '%s ...' says page %s | Typst heading page %s\n", h, num, real))
}
outline <- pdftools::pdf_toc(pdf)
cat("PDF bookmarks: top level", length(outline$children), "| under 'Fiches des variables':",
    length(outline$children[[which(vapply(outline$children, function(x) x$title, "") == "Fiches des variables")]]$children), "\n")
