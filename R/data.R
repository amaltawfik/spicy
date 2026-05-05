#' Simulated social-health survey
#'
#' @description
#' A simulated dataset of 1200 respondents from a fictional
#' social-health survey, designed to illustrate the main features of
#' the spicy package: variable labels, ordered factors, survey
#' weights, association measures, and APA-style reporting.
#'
#' @details
#' Every variable carries a `"label"` attribute (read by
#' [labelled::var_label()] and surfaced by [varlist()] /
#' [code_book()]). The mix of factor types is deliberate: nominal
#' factors (`sex`, `region`, ...) and ordered factors (`education`,
#' `self_rated_health`, ...) live side by side so that
#' [cross_tab()] and [table_categorical()] can demonstrate the
#' automatic ordinal-vs-nominal dispatch (Cramer's V, Phi, Kendall's
#' Tau-b, Goodman-Kruskal Gamma) on the same dataset.
#'
#' Survey weights (`weight`) are calibrated: `sum(weight)` matches
#' the unweighted N to within rounding (\eqn{\approx 1200}) and
#' `mean(weight)` is \eqn{\approx 1}. Weighted means therefore agree
#' with unweighted means up to sampling noise without further
#' rescaling.
#'
#' @format A tibble with 1200 rows and 24 variables:
#' \describe{
#'   \item{sex}{Factor. Sex of the respondent.}
#'   \item{age}{Numeric. Age in years (25--75).}
#'   \item{age_group}{Ordered factor. Age group
#'     (25--34, 35--49, 50--64, 65--75).}
#'   \item{education}{Ordered factor. Highest education level
#'     (Lower secondary, Upper secondary, Tertiary).}
#'   \item{social_class}{Ordered factor. Subjective social class
#'     (Lower, Working, Lower middle, Middle, Upper middle).}
#'   \item{region}{Factor. Region of residence (6 regions).}
#'   \item{employment_status}{Factor. Employment status
#'     (Employed, Student, Unemployed, Inactive).}
#'   \item{income_group}{Ordered factor. Household income group
#'     (Low, Lower middle, Upper middle, High). Contains missing
#'     values.}
#'   \item{income}{Numeric. Monthly household income in CHF
#'     (1000--7400).}
#'   \item{smoking}{Factor. Current smoker (No, Yes). Contains
#'     missing values.}
#'   \item{physical_activity}{Factor. Regular physical activity
#'     (No, Yes).}
#'   \item{dentist_12m}{Factor. Dentist visit in the last 12 months
#'     (No, Yes).}
#'   \item{self_rated_health}{Ordered factor. Self-rated health
#'     (Poor, Fair, Good, Very good). Contains missing values.}
#'   \item{wellbeing_score}{Numeric. WHO-5 wellbeing index
#'     (0--100).}
#'   \item{bmi}{Numeric. Body mass index in kg/m\eqn{^2}
#'     (16--39). Contains missing values.}
#'   \item{bmi_category}{Ordered factor. BMI category
#'     (Normal weight, Overweight, Obesity). Contains missing values.}
#'   \item{institutional_trust}{Ordered factor. Trust in institutions
#'     (Very low, Low, High, Very high).}
#'   \item{political_position}{Numeric. Political position on a
#'     0 (left) to 10 (right) scale. Contains missing values.}
#'   \item{life_sat_health}{Integer. Satisfaction with own health
#'     (1--5 Likert scale). Contains missing values.}
#'   \item{life_sat_work}{Integer. Satisfaction with work or main
#'     activity (1--5 Likert scale). Contains missing values.}
#'   \item{life_sat_relationships}{Integer. Satisfaction with
#'     personal relationships (1--5 Likert scale). Contains missing
#'     values.}
#'   \item{life_sat_standard}{Integer. Satisfaction with standard
#'     of living (1--5 Likert scale). Contains missing values.}
#'   \item{response_date}{POSIXct. Date and time of survey response
#'     (September--November 2024).}
#'   \item{weight}{Numeric. Survey design weight (range
#'     0.29--3.45); calibrated so that `sum(weight)` matches the
#'     unweighted N and `mean(weight)` is approximately 1. See
#'     `Details`.}
#' }
#'
#' @source Simulated data for illustration purposes; reproducible by
#'   sourcing `data-raw/sochealth.R`. The script seeds the main
#'   generation block with `set.seed(2025)`, and the two
#'   missing-value injection blocks with `set.seed(2027)` (the
#'   four `life_sat_*` items) and `set.seed(2026)` (`smoking`,
#'   `self_rated_health`, `income_group`, `political_position`,
#'   `bmi`).
#'
#' @examples
#' data(sochealth)
#' varlist(sochealth)
#' freq(sochealth, education)
#' cross_tab(sochealth, education, self_rated_health)
"sochealth"
