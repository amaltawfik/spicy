#!/usr/bin/env Rscript

# Benchmark old (HEAD) vs current cross_tab() implementation.
# Run from package root:
#   Rscript tools/benchmark_cross_tab.R

suppressPackageStartupMessages({
  library(pkgload)
})

pkgload::load_all(".", quiet = TRUE)
new_cross_tab <- get("cross_tab", envir = parent.frame())

tmp_old <- tempfile(fileext = ".R")
old_src <- system2(
  "git",
  c("show", "HEAD:R/cross_tab.R"),
  stdout = TRUE,
  stderr = FALSE
)
if (length(old_src) == 0) {
  stop("Could not read HEAD version of R/cross_tab.R", call. = FALSE)
}
writeLines(old_src, tmp_old, useBytes = TRUE)

old_env <- new.env(parent = globalenv())
source(tmp_old, local = old_env, encoding = "UTF-8")
old_cross_tab <- old_env$cross_tab
unlink(tmp_old)

set.seed(20260306)
n <- 100000L
df <- data.frame(
  x = sample(c("A", "B", "C", "D"), n, replace = TRUE),
  y = sample(c("Yes", "No", "Maybe"), n, replace = TRUE),
  g = sample(c("G1", "G2", "G3"), n, replace = TRUE),
  w = runif(n, min = 0.1, max = 3),
  stringsAsFactors = FALSE
)

time_expr <- function(expr, n_rep = 7L) {
  out <- numeric(n_rep)
  for (i in seq_len(n_rep)) {
    gc(FALSE)
    out[i] <- system.time(eval(expr))[["elapsed"]]
  }
  out
}

bench_case <- function(name, expr_old, expr_new) {
  old_t <- time_expr(expr_old)
  new_t <- time_expr(expr_new)

  old_med <- stats::median(old_t)
  new_med <- stats::median(new_t)
  speedup <- if (new_med > 0) old_med / new_med else NA_real_

  data.frame(
    case = name,
    old_median_s = round(old_med, 4),
    new_median_s = round(new_med, 4),
    speedup_x = round(speedup, 3),
    stringsAsFactors = FALSE
  )
}

results <- do.call(
  rbind,
  list(
    bench_case(
      "df_no_group_stats",
      quote(old_cross_tab(
        df,
        x,
        y,
        percent = "c",
        include_stats = TRUE,
        styled = FALSE
      )),
      quote(new_cross_tab(
        df,
        x,
        y,
        percent = "c",
        include_stats = TRUE,
        styled = FALSE
      ))
    ),
    bench_case(
      "df_grouped_stats",
      quote(old_cross_tab(
        df,
        x,
        y,
        by = g,
        weights = w,
        percent = "c",
        include_stats = TRUE,
        styled = FALSE
      )),
      quote(new_cross_tab(
        df,
        x,
        y,
        by = g,
        weights = w,
        percent = "c",
        include_stats = TRUE,
        styled = FALSE
      ))
    ),
    bench_case(
      "vector_mode",
      quote(old_cross_tab(
        df$x,
        df$y,
        weights = df$w,
        percent = "c",
        include_stats = TRUE,
        styled = FALSE
      )),
      quote(new_cross_tab(
        df$x,
        df$y,
        weights = df$w,
        percent = "c",
        include_stats = TRUE,
        styled = FALSE
      ))
    )
  )
)

print(results, row.names = FALSE)
