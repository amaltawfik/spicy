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
    # The column widths come from the numbers as displayed, not from their
    # full precision: a mean of 49.2641666666667 would take an 18-wide
    # column.
    shown <- df
    if (i == 2L) {
      dec <- xl_decimals(df$sd)
      for (k in intersect(c("mean", "sd", "median"), names(df))) {
        shown[[k]] <- xl_fixed(df[[k]], dec)
      }
    }
    if (i == 3L) {
      for (k in c("pct_total", "pct_valid")) {
        shown[[k]] <- xl_fixed(df[[k]], 1L)
      }
    }
    if (i > 1L) {
      names(df) <- code_book_headers(names(df))
      names(shown) <- names(df)
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
    # The header row stays in view; in `variables`, so do the position,
    # the name and the label while the statistics scroll.
    wb <- if (i == 2L) {
      openxlsx2::wb_freeze_pane(
        wb,
        sheet = s,
        first_active_row = 2L,
        first_active_col = 4L
      )
    } else {
      openxlsx2::wb_freeze_pane(wb, sheet = s, first_row = TRUE)
    }
    wb <- openxlsx2::wb_add_filter(
      wb,
      sheet = s,
      rows = 1L,
      cols = seq_along(df)
    )
    wb <- .spicy_xl_set_widths(
      wb,
      s,
      .spicy_xl_cells(shown, list(names(shown)))
    )
    if (i == 1L) {
      # The notes wrap in a wide Value column, every row aligned at its top.
      wb <- openxlsx2::wb_set_col_widths(wb, sheet = s, cols = 2L, widths = 90)
      wb <- openxlsx2::wb_add_cell_style(
        wb,
        sheet = s,
        dims = openxlsx2::wb_dims(rows = 1L + seq_len(nrow(df)), cols = 1:2),
        wrap_text = "1",
        vertical = "top"
      )
    }
    # On paper: landscape, fitted to the width of the page, the header row
    # repeated on every page.
    wb <- openxlsx2::wb_page_setup(
      wb,
      sheet = s,
      orientation = "landscape",
      fit_to_width = TRUE,
      fit_to_height = FALSE,
      print_title_rows = 1L
    )
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
  # The mean, SD and median of a variable at the precision of the PDF,
  # three significant digits of its SD; the cells keep their full
  # precision. Min and max stay General: they are values of the data.
  v <- cb$variables
  stat_cols <- which(names(v) %in% c("mean", "sd", "median"))
  if (nrow(v) > 0L && length(stat_cols) > 0L) {
    dec <- xl_decimals(v$sd)
    for (r in which(!is.na(v$mean))) {
      wb <- openxlsx2::wb_add_numfmt(
        wb,
        sheet = sheets[[2L]],
        dims = openxlsx2::wb_dims(rows = r + 1L, cols = stat_cols),
        numfmt = paste0(
          "0",
          if (dec[[r]] > 0L) paste0(".", strrep("0", dec[[r]]))
        )
      )
    }
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


# The decimals of the mean, SD and median of each variable: three
# significant digits of the SD (the rule of the PDF), from none to six;
# two without an SD.
xl_decimals <- function(sd) {
  dec <- rep(2L, length(sd))
  ok <- !is.na(sd) & is.finite(sd) & sd > 0
  dec[ok] <- pmin(6L, pmax(0L, 2L - floor(log10(signif(sd[ok], 3L)))))
  as.integer(dec)
}


# A number written with `digits` decimals (one per element, or one for
# all), NA kept, for the width of its column.
xl_fixed <- function(x, digits) {
  digits <- rep_len(digits, length(x))
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  out[ok] <- vapply(
    which(ok),
    function(j) formatC(x[[j]], format = "f", digits = digits[[j]]),
    character(1)
  )
  out
}
