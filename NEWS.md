# spicy (development version)

# spicy 0.2.0

* New `label_from_names()` to derive and assign variable labels from headers of the form `"name<sep>label"` (e.g. `"name. label"`).
  Especially useful for LimeSurvey CSV exports (*Export results* → *CSV* → *Headings: Question code & question text*), where the default separator is `". "`.

# spicy 0.1.0

## Initial release

* Introduces a collection of tools for variable inspection, descriptive summaries, and data exploration.
* Provides functions to:
  - Extract variable metadata and display compact summaries (`varlist()`).
  - Compute frequency tables (`freq()`), cross-tabulations (`cross_tab()`), and Cramer's V for categorical associations (`cramer_v()`).
  - Generate descriptive statistics such as means (`mean_n()`), sums (`sum_n()`), and counts (`count_n()`) with automatic handling of missing data.
  - Copy data (`copy_clipboard()`) directly to the clipboard for quick export.
