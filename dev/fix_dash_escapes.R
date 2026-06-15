# Repair the corrupted \2013 sequences left by sed back to –.
files <- c(
  "R/regression_dispatch.R",
  "R/regression_render.R",
  "R/regression_structured.R",
  "R/regression_titlefooter.R",
  "R/regression_validate.R"
)
for (path in files) {
  src <- readLines(path, warn = FALSE, encoding = "UTF-8")
  # \2013 -> – (re-inject the missing `u`).
  src <- gsub("\\\\2013", "\\\\u2013", src)
  con <- file(path, "wb")
  writeLines(src, con = con, sep = "\n", useBytes = FALSE)
  close(con)
  n <- sum(grepl("\\\\u2013", src))
  cat(path, ":", n, "en-dash escapes\n")
}
