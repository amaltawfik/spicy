#' Label attributes with a few renames
#'
#' @description
#' Builds a value for the `labels` argument of the descriptive tables
#' ([table_categorical()], [table_continuous()], [table_continuous_lm()],
#' [table_outcome()], [table_categorical_svy()] and
#' [table_continuous_svy()]). The columns it names take the labels given
#' here, and every other column shows its `label` attribute (as set by
#' `haven`, `labelled` or [label_from_names()]), or its name when it has
#' none, whatever `options(spicy.var_labels)` says. `var_labels()` with
#' no argument is the same as `labels = TRUE`.
#'
#' A plain named vector, `labels = c(bmi = "Body mass index")`, differs
#' in one point: the columns it leaves out follow
#' `options(spicy.var_labels)`, which shows their names unless it is
#' `TRUE`.
#'
#' @param ... `name = "label"` pairs, one per column to rename, each label
#'   a single string. A named character vector can be spliced with `!!!`.
#'   Names that match no column of a table are ignored.
#'
#' @return A named character vector of class `spicy_var_labels`.
#'
#' @seealso The `labels` argument of [table_categorical()] and its
#'   siblings, which lists the four forms it takes.
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
