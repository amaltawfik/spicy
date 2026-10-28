#' Label attributes with a few renames
#'
#' @description
#' Builds a value for the `labels` argument of the descriptive tables
#' ([table_categorical()], [table_continuous()], [table_continuous_lm()],
#' [table_outcome()], [table_categorical_svy()] and
#' [table_continuous_svy()]) and of the regression tables
#' ([table_regression()] and [table_regression_uv()]). The variables it
#' names take the labels given here, and every other variable shows its
#' `label` attribute (as set by `haven`, `labelled` or
#' [label_from_names()]), or its name when it has none, whatever
#' `options(spicy.var_labels)` says. `var_labels()` with no argument is
#' the same as `labels = TRUE`.
#'
#' A plain named vector, `labels = c(bmi = "Body mass index")`, differs
#' in one point: the variables it leaves out follow
#' `options(spicy.var_labels)`, which shows their names unless it is
#' `TRUE`.
#'
#' In a regression table, the names are the model's: the outcome, the
#' term labels (`age`, or `sex` for the header of a factor), and the
#' coefficient names (`sexMale`, `(Intercept)`). The attribute names the
#' outcome, the numeric terms, and the factor headers. Factor levels,
#' interactions, and transformed terms keep their names.
#'
#' @param ... `name = "label"` pairs, one per variable to rename, each
#'   label a single string. A named character vector can be spliced with
#'   `!!!`. A name that matches nothing in a table warns there and is
#'   left out. For
#'   `print()`, passed on to [print()].
#' @param x A `spicy_var_labels` object, for `print()`.
#'
#' @return A named character vector of class `spicy_var_labels`, which
#'   prints as the plain named vector. `print()` returns `x` invisibly.
#'
#' @seealso The `labels` argument of [table_categorical()] and its
#'   siblings, and of [table_regression()], which lists the four forms
#'   it takes.
#'
#' @examples
#' # The label attribute of every column, one rename.
#' table_continuous(
#'   sochealth,
#'   select = c(bmi, age),
#'   labels = var_labels(bmi = "BMI (kg/m2)")
#' )
#'
#' # A vector of renames kept for a whole project, spliced.
#' renames <- c(bmi = "BMI (kg/m2)", smoking = "Smoker")
#' tbl <- table_categorical(
#'   sochealth,
#'   select = smoking,
#'   by = sex,
#'   labels = var_labels(!!!renames)
#' )
#'
#' # A regression table: the outcome and the factor header keep their
#' # label attribute, the numeric term takes the one given here.
#' table_regression(
#'   lm(wellbeing_score ~ age + sex, data = sochealth),
#'   labels = var_labels(age = "Age in years")
#' )
#' @export
var_labels <- function(...) {
  dots <- rlang::list2(...)
  single <- vapply(
    dots,
    function(v) is.character(v) && length(v) == 1L && !is.na(v),
    logical(1)
  )
  if (!all(single)) {
    spicy_abort(
      c(
        "Every `var_labels()` entry must be a single string.",
        "i" = "Name each label by its column: `var_labels(bmi = \"Body mass index\")`."
      ),
      class = "spicy_invalid_input"
    )
  }
  out <- stats::setNames(
    as.character(unlist(dots, use.names = FALSE)),
    names(dots) %||% rep("", length(dots))
  )
  class(out) <- c("spicy_var_labels", "character")
  # The names are checked by the one validator of `labels`.
  .check_labels_arg(out)
  out
}

#' @rdname var_labels
#' @export
print.spicy_var_labels <- function(x, ...) {
  print(unclass(x), ...)
  invisible(x)
}
