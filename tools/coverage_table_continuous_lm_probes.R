data("sochealth", package = "spicy", envir = environment())

long_cat <- table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  statistic = TRUE,
  effect_size = "f2",
  output = "long"
)
long_cat_no_ci <- table_continuous_lm(
  sochealth,
  select = c(wellbeing_score, bmi),
  by = sex,
  statistic = FALSE,
  p_value = FALSE,
  show_n = FALSE,
  effect_size = "none",
  r2 = "none",
  ci = FALSE,
  output = "long"
)
long_num <- table_continuous_lm(
  sochealth,
  select = wellbeing_score,
  by = age,
  statistic = TRUE,
  effect_size = "f2",
  r2 = "adj_r2",
  output = "long"
)
long_3lvl <- table_continuous_lm(
  sochealth,
  select = wellbeing_score,
  by = education,
  statistic = TRUE,
  output = "long"
)
invisible(table_continuous_lm(
  sochealth,
  select = wellbeing_score,
  by = sex,
  labels = c(wellbeing_score = "Named wellbeing"),
  output = "long"
))

invisible(build_wide_raw_continuous_lm(
  long_cat,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  effect_size = "f2",
  r2_type = "r2",
  ci = TRUE
))
invisible(build_wide_raw_continuous_lm(
  long_cat_no_ci,
  show_statistic = FALSE,
  show_p_value = FALSE,
  show_n = FALSE,
  effect_size = "none",
  r2_type = "none",
  ci = FALSE
))
invisible(build_wide_raw_continuous_lm(
  long_num,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  effect_size = "f2",
  r2_type = "adj_r2",
  ci = TRUE
))

display_cat <- build_display_df_continuous_lm(
  long_cat,
  digits = 2L,
  decimal_mark = ".",
  ci_level = 0.95,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  effect_size = "f2",
  r2_type = "r2",
  ci = TRUE
)
invisible(build_display_df_continuous_lm(
  long_cat_no_ci,
  digits = 2L,
  decimal_mark = ",",
  ci_level = 0.95,
  show_statistic = FALSE,
  show_p_value = FALSE,
  show_n = FALSE,
  effect_size = "none",
  r2_type = "none",
  ci = FALSE
))
invisible(build_display_df_continuous_lm(
  long_num,
  digits = 2L,
  decimal_mark = ".",
  ci_level = 0.95,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  effect_size = "f2",
  r2_type = "adj_r2",
  ci = TRUE
))
invisible(build_display_df_continuous_lm(
  make_empty_lm_rows("y", "Y", "continuous"),
  digits = 2L,
  decimal_mark = ".",
  ci_level = 0.95,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  effect_size = "none",
  r2_type = "none",
  ci = TRUE
))

wide_cat <- build_wide_display_df_continuous_lm(
  long_cat,
  digits = 2L,
  decimal_mark = ".",
  ci_level = 0.95,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  effect_size = "f2",
  r2_type = "r2",
  ci = TRUE
)
invisible(build_wide_display_df_continuous_lm(
  long_cat_no_ci,
  digits = 2L,
  decimal_mark = ",",
  ci_level = 0.95,
  show_statistic = FALSE,
  show_p_value = FALSE,
  show_n = FALSE,
  effect_size = "none",
  r2_type = "none",
  ci = FALSE
))
invisible(build_wide_display_df_continuous_lm(
  long_num,
  digits = 2L,
  decimal_mark = ".",
  ci_level = 0.95,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  effect_size = "f2",
  r2_type = "adj_r2",
  ci = TRUE
))
invisible(build_wide_display_df_continuous_lm(
  make_empty_lm_rows("y", "Y", "continuous"),
  digits = 2L,
  decimal_mark = ".",
  ci_level = 0.95,
  show_statistic = TRUE,
  show_p_value = TRUE,
  show_n = TRUE,
  effect_size = "none",
  r2_type = "none",
  ci = TRUE
))

tmp_xlsx <- tempfile(fileext = ".xlsx")
tmp_docx <- tempfile(fileext = ".docx")

invisible(export_continuous_lm_table(
  wide_cat,
  output = "tinytable",
  ci_level = 0.95,
  excel_path = NULL,
  excel_sheet = "Sheet1",
  clipboard_delim = "\t",
  word_path = NULL
))
invisible(export_continuous_lm_table(
  wide_cat,
  output = "gt",
  ci_level = 0.95,
  excel_path = NULL,
  excel_sheet = "Sheet1",
  clipboard_delim = "\t",
  word_path = NULL
))
invisible(export_continuous_lm_table(
  wide_cat[,
    setdiff(names(wide_cat), c("95% CI LL", "95% CI UL")),
    drop = FALSE
  ],
  output = "gt",
  ci_level = 0.95,
  excel_path = NULL,
  excel_sheet = "Sheet1",
  clipboard_delim = "\t",
  word_path = NULL
))
invisible(export_continuous_lm_table(
  wide_cat,
  output = "flextable",
  ci_level = 0.95,
  excel_path = NULL,
  excel_sheet = "Sheet1",
  clipboard_delim = "\t",
  word_path = NULL
))
local({
  testthat::local_mocked_bindings(
    wb_save = function(...) invisible(NULL),
    .package = "openxlsx2"
  )
  invisible(export_continuous_lm_table(
    wide_cat,
    output = "excel",
    ci_level = 0.95,
    excel_path = tmp_xlsx,
    excel_sheet = "Sheet1",
    clipboard_delim = "\t",
    word_path = NULL
  ))
})
local({
  testthat::local_mocked_bindings(
    save_as_docx = function(..., path) path,
    .package = "flextable"
  )
  invisible(export_continuous_lm_table(
    wide_cat,
    output = "word",
    ci_level = 0.95,
    excel_path = NULL,
    excel_sheet = "Sheet1",
    clipboard_delim = "\t",
    word_path = tmp_docx
  ))
})

rename_ci_cols_lm(
  setNames(data.frame(1, 2, check.names = FALSE), c("95% CI LL", "95% CI UL")),
  "95% CI LL",
  "95% CI UL"
)
build_header_rows_lm(c("Variable", "LL", "UL"), "95%")
get_delta_label_lm(long_cat[
  long_cat$variable == "wellbeing_score",
  ,
  drop = FALSE
])
get_test_row_index_lm(long_cat[
  long_cat$variable == "wellbeing_score",
  ,
  drop = FALSE
])
get_test_header_lm(
  long_cat[long_cat$variable == "wellbeing_score", , drop = FALSE],
  TRUE,
  FALSE
)
get_test_header_lm(
  long_3lvl[long_3lvl$variable == "wellbeing_score", , drop = FALSE],
  TRUE,
  TRUE
)
get_test_header_lm(
  long_3lvl[long_3lvl$variable == "wellbeing_score", , drop = FALSE],
  TRUE,
  FALSE
)
format_effect_size_header_lm("f2")
format_effect_size_header_lm("none")
format_r2_header_lm("r2")
format_r2_header_lm("adj_r2")
get_r2_value_lm(
  long_num[long_num$variable == "wellbeing_score", , drop = FALSE],
  "adj_r2"
)
format_number_lm(c(1.23, NA_real_), 2L, ",")
format_p_value_lm(NA_real_, ".")
format_p_value_lm(0.0005, ".")
format_p_value_lm(0.045, ",")
format_test_value_lm(NA_character_, NA_real_, 1, 10, 2L, ".")
format_test_value_lm("t", 2.34, 1, 10, 2L, ".")
format_test_value_lm("F", 3.21, 2, 10, 2L, ".")
format_test_value_lm("z", 1.11, 1, 10, 2L, ".")
format_effect_size_lm(NA_character_, NA_real_, 2L, ".")
format_effect_size_lm("f²", 0.23, 2L, ".")

make_empty_lm_rows("y", "Y", "continuous")
fit_numeric_predictor_lm_rows(
  y = c(1, 2),
  x = c(1, 1),
  weights = NULL,
  outcome_name = "y",
  outcome_label = "Y",
  predictor_label = "X",
  vcov_type = "classical",
  ci_level = 0.95
)
fit_numeric_predictor_lm_rows(
  y = c(1, 2, 3),
  x = c(1, 2, 3),
  weights = c(1, 2, 3),
  outcome_name = "y",
  outcome_label = "Y",
  predictor_label = "X",
  vcov_type = "HC3",
  ci_level = 0.95
)
fit_numeric_predictor_lm_rows(
  y = c(1, 2),
  x = c(1, 2),
  weights = NULL,
  outcome_name = "y",
  outcome_label = "Y",
  predictor_label = "X",
  vcov_type = "classical",
  ci_level = 0.95
)
fit_categorical_predictor_lm_rows(
  y = c(1, 2),
  x = factor(c("A", "A")),
  weights = NULL,
  outcome_name = "y",
  outcome_label = "Y",
  predictor_label = "G",
  vcov_type = "classical",
  contrast = "auto",
  ci_level = 0.95
)
fit_categorical_predictor_lm_rows(
  y = c(1, 2),
  x = factor(c("A", "B")),
  weights = NULL,
  outcome_name = "y",
  outcome_label = "Y",
  predictor_label = "G",
  vcov_type = "classical",
  contrast = "auto",
  ci_level = 0.95
)
is_supported_lm_predictor(TRUE)
coerce_lm_factor(c("a", "b"))
detect_weights_column_name(rlang::quo(NULL), sochealth)
detect_weights_column_name(rlang::quo(survey_weight), sochealth)
detect_weights_column_name(rlang::quo("survey_weight"), sochealth)
detect_weights_column_name(rlang::quo("w"), data.frame(y = 1:3, w = 1:3))

fit <- lm(mpg ~ wt, data = mtcars)
compute_lm_model_stats(fit)
compute_lm_vcov(fit, "classical")
compute_lm_vcov(fit, "HC0")
compute_lm_vcov(fit, "HC1")
compute_lm_vcov(fit, "HC2")
compute_lm_vcov(fit, "HC3")
compute_lm_vcov(fit, "HC4")
compute_lm_vcov(fit, "HC4m")
compute_lm_vcov(fit, "HC5")

local({
  testthat::local_mocked_bindings(
    requireNamespace = function(package, quietly = TRUE) FALSE,
    .package = "base"
  )
  try(
    export_continuous_lm_table(
      wide_cat,
      "tinytable",
      0.95,
      NULL,
      "Sheet1",
      "\t",
      NULL
    ),
    silent = TRUE
  )
})
local({
  testthat::local_mocked_bindings(
    requireNamespace = function(package, quietly = TRUE) package != "gt",
    .package = "base"
  )
  try(
    export_continuous_lm_table(
      wide_cat,
      "gt",
      0.95,
      NULL,
      "Sheet1",
      "\t",
      NULL
    ),
    silent = TRUE
  )
})
local({
  testthat::local_mocked_bindings(
    requireNamespace = function(package, quietly = TRUE) package != "flextable",
    .package = "base"
  )
  try(
    export_continuous_lm_table(
      wide_cat,
      "flextable",
      0.95,
      NULL,
      "Sheet1",
      "\t",
      NULL
    ),
    silent = TRUE
  )
})
local({
  testthat::local_mocked_bindings(
    requireNamespace = function(package, quietly = TRUE) package != "officer",
    .package = "base"
  )
  try(
    export_continuous_lm_table(
      wide_cat,
      "word",
      0.95,
      NULL,
      "Sheet1",
      "\t",
      tmp_docx
    ),
    silent = TRUE
  )
})
local({
  testthat::local_mocked_bindings(
    requireNamespace = function(package, quietly = TRUE) package != "openxlsx2",
    .package = "base"
  )
  try(
    export_continuous_lm_table(
      wide_cat,
      "excel",
      0.95,
      tmp_xlsx,
      "Sheet1",
      "\t",
      NULL
    ),
    silent = TRUE
  )
})
try(
  export_continuous_lm_table(
    wide_cat,
    "word",
    0.95,
    NULL,
    "Sheet1",
    "\t",
    NULL
  ),
  silent = TRUE
)
try(
  export_continuous_lm_table(
    wide_cat,
    "excel",
    0.95,
    NULL,
    "Sheet1",
    "\t",
    NULL
  ),
  silent = TRUE
)
local({
  testthat::local_mocked_bindings(
    requireNamespace = function(package, quietly = TRUE) package != "clipr",
    .package = "base"
  )
  try(
    export_continuous_lm_table(
      wide_cat,
      "clipboard",
      0.95,
      NULL,
      "Sheet1",
      "\t",
      NULL
    ),
    silent = TRUE
  )
})
try(
  export_continuous_lm_table(
    wide_cat,
    "bogus",
    0.95,
    NULL,
    "Sheet1",
    "\t",
    NULL
  ),
  silent = TRUE
)

unlink(c(tmp_xlsx, tmp_docx), force = TRUE)
