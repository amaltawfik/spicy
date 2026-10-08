# The Excel codebook: a first sheet with the header as field-value rows,
# then `variables` and `values` as plain tables from row 1, so that each
# keeps its filters and frozen header and reads back with `read_xlsx()`
# without skipping rows. openxlsx2 is checked by code_book_output_format()
# before the codebook is built. The header takes the band and primary
# colors of the PDF; the font is set only when `font` is given.
code_book_write_xlsx <- function(cb, path, font = NULL) {
  h <- cb$header
  info <- code_book_info(h, orcid = TRUE)
  info <- rbind(
    info,
    data.frame(
      key = "generated",
      field = spicy_str("row_generated_with"),
      value = paste0(
        "spicy ",
        getNamespaceVersion("spicy"),
        ", R ",
        getRversion(),
        ", ",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    )
  )
  first <- stats::setNames(
    info[c("field", "value")],
    c(spicy_str("header_field"), spicy_str("header_value"))
  )
  tables <- list(first, cb$variables, cb$values)
  sheets <- c(
    spicy_str("excel_sheet_codebook"),
    spicy_str("excel_sheet_codebook_variables"),
    spicy_str("excel_sheet_codebook_values")
  )

  wb <- openxlsx2::wb_workbook()
  if (!is.null(font)) {
    wb <- openxlsx2::wb_set_base_font(wb, font_name = font)
  }
  head_font <- openxlsx2::wb_get_base_font(wb)$name$val
  argb <- sub("#", "FF", attr(cb, "appearance")$colors, fixed = TRUE)
  for (i in seq_along(tables)) {
    df <- as.data.frame(tables[[i]])
    # Inf, -Inf and NaN (statistics of a column holding Inf) would become
    # error cells: they are left empty, and the column stays numeric.
    dbl <- vapply(df, is.double, logical(1))
    df[dbl] <- lapply(df[dbl], function(v) replace(v, !is.finite(v), NA))
    if (i > 1L) {
      names(df) <- code_book_headers(names(df))
    }
    s <- sheets[[i]]
    head <- openxlsx2::wb_dims(rows = 1L, cols = seq_along(df))
    wb <- openxlsx2::wb_add_worksheet(wb, sheet = s)
    # NULL leaves a missing value's cell out; "" would write empty text
    # into numeric columns.
    wb <- openxlsx2::wb_add_data(wb, sheet = s, x = df, na.strings = NULL)
    wb <- openxlsx2::wb_add_fill(
      wb,
      sheet = s,
      dims = head,
      color = openxlsx2::wb_color(hex = argb[["band"]])
    )
    wb <- openxlsx2::wb_add_font(
      wb,
      sheet = s,
      dims = head,
      bold = TRUE,
      name = head_font,
      color = openxlsx2::wb_color(hex = argb[["primary"]])
    )
    wb <- openxlsx2::wb_freeze_pane(wb, sheet = s, first_row = TRUE)
    wb <- openxlsx2::wb_add_filter(
      wb,
      sheet = s,
      rows = 1L,
      cols = seq_along(df)
    )
    wb <- .spicy_xl_set_widths(wb, s, .spicy_xl_cells(df, list(names(df))))
  }

  # The two counts of the first sheet stay numbers.
  for (k in c("observations", "variables")) {
    wb <- openxlsx2::wb_add_data(
      wb,
      sheet = sheets[[1L]],
      x = if (k == "observations") h$n_obs else h$n_vars,
      dims = openxlsx2::wb_dims(rows = which(info$key == k) + 1L, cols = 2L)
    )
  }
  # Percentages shown to one decimal; the cells keep their full precision.
  # Means and SDs keep the General format, which a fixed number of
  # decimals would turn to 0.00 on a small scale.
  n <- nrow(cb$values)
  if (n > 0L) {
    wb <- openxlsx2::wb_add_numfmt(
      wb,
      sheet = sheets[[3L]],
      dims = openxlsx2::wb_dims(
        rows = 1L + seq_len(n),
        cols = which(names(cb$values) %in% c("pct_total", "pct_valid"))
      ),
      numfmt = "0.0"
    )
  }
  # Without authors the creator is "", not NULL: openxlsx2 would write the
  # login of the session as creator and last modifier.
  wb <- openxlsx2::wb_set_properties(
    wb,
    title = if (!is.na(h$title)) h$title,
    creator = paste(h$authors$name, collapse = "; ")
  )
  openxlsx2::wb_save(wb, file = path, overwrite = TRUE)
  invisible(path)
}
