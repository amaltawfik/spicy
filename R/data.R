#' Simulated social-health survey
#'
#' A simulated dataset of 1200 respondents from a fictional
#' social-health survey, designed to illustrate the main features of
#' the spicy package: variable labels, ordered factors, survey
#' weights, association measures, and APA-style reporting.
#'
#' All variables carry labels (accessible via [labelled::var_label()]
#' and displayed by [varlist()]). Several ordered factors are included
#' so that [cross_tab()] can demonstrate automatic ordinal measure
#' selection.
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
#'   \item{income}{Numeric. Monthly household income in CHF.}
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
#'   \item{bmi}{Numeric. Body mass index. Contains missing values.}
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
#'   \item{weight}{Numeric. Survey design weight.}
#' }
#'
#' @source Simulated data for illustration purposes.
#'
#' @examples
#' data(sochealth)
#' varlist(sochealth)
#' freq(sochealth, education)
#' cross_tab(sochealth, education, self_rated_health)
"sochealth"
