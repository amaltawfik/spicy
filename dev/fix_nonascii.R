files <- c("R/regression_titlefooter.R", "R/regression_frame_quantreg_AER.R")
pairs <- list(
  c("—", "\\u2014"),
  c("σ", "\\u03C3"),
  c("ρ", "\\u03C1"),
  c("β", "\\u03B2"),
  c("η", "\\u03B7"),
  c("ω", "\\u03C9"),
  c("χ", "\\u03C7"),
  c("φ", "\\u03C6"),
  c("²", "\\u00B2"),
  c("Δ", "\\u0394"),
  c("µ", "\\u00B5"),
  c("×", "\\u00D7"),
  c("τ", "\\u03C4")
)
for (path in files) {
  src <- readLines(path, warn = FALSE, encoding = "UTF-8")
  for (p in pairs) {
    src <- gsub(p[1], p[2], src, fixed = TRUE)
  }
  con <- file(path, "wb")
  writeLines(src, con = con, sep = "\n", useBytes = FALSE)
  close(con)
  cat(path, " done\n")
  tools::showNonASCIIfile(path)
}
