## data-raw/sochealth.R
## Generate the sochealth example dataset
## -------------------------------------------------------

library(dplyr)

set.seed(2025)
n <- 1200L

# -- Helper: vectorized per-row sampling -----------------
sample_by_prob <- function(prob_list, levels) {
  vapply(prob_list, \(p) sample(levels, 1L, prob = p), character(1L))
}

# -- Core demographics -----------------------------------
sochealth <- tibble::tibble(
  sex = sample(
    c("Male", "Female"),
    n,
    replace = TRUE,
    prob = c(0.48, 0.52)
  ),
  age = as.numeric(sample(25:75, n, replace = TRUE)),
  education = sample(
    c("Lower secondary", "Upper secondary", "Tertiary"),
    n,
    replace = TRUE,
    prob = c(0.23, 0.43, 0.34)
  ),
  region = sample(
    c("North", "South", "East", "West", "Central", "Other"),
    n,
    replace = TRUE,
    prob = c(0.18, 0.16, 0.14, 0.16, 0.20, 0.16)
  )
)

# -- Age group -------------------------------------------
sochealth <- sochealth |>
  mutate(
    age_group = cut(
      age,
      breaks = c(24, 34, 49, 64, 75),
      labels = c("25-34", "35-49", "50-64", "65-75"),
      ordered_result = TRUE
    )
  )

# -- Social class (depends on education) -----------------
sc_levels <- c("Lower", "Working", "Lower middle", "Middle", "Upper middle")
sc_probs <- list(
  "Lower secondary" = c(0.30, 0.38, 0.20, 0.09, 0.03),
  "Upper secondary" = c(0.10, 0.28, 0.32, 0.22, 0.08),
  "Tertiary" = c(0.03, 0.10, 0.24, 0.40, 0.23)
)
sochealth$social_class <- sample_by_prob(
  lapply(sochealth$education, \(e) sc_probs[[e]]),
  sc_levels
)

# -- Smoking ---------------------------------------------
smoke_prob <- c(
  "Lower secondary" = 0.36,
  "Upper secondary" = 0.24,
  "Tertiary" = 0.13
)
sochealth$smoking <- sample_by_prob(
  lapply(sochealth$education, \(e) c(1 - smoke_prob[[e]], smoke_prob[[e]])),
  c("No", "Yes")
)

# -- Self-rated health -----------------------------------
srh_levels <- c("Poor", "Fair", "Good", "Very good")
srh_probs <- list(
  "Lower secondary" = c(0.13, 0.32, 0.39, 0.16),
  "Upper secondary" = c(0.05, 0.20, 0.50, 0.25),
  "Tertiary" = c(0.02, 0.13, 0.48, 0.37)
)
sochealth$self_rated_health <- sample_by_prob(
  lapply(sochealth$education, \(e) srh_probs[[e]]),
  srh_levels
)

# -- Physical activity -----------------------------------
pa_prob <- c(
  "Lower secondary" = 0.30,
  "Upper secondary" = 0.42,
  "Tertiary" = 0.55
)
sochealth$physical_activity <- sample_by_prob(
  lapply(sochealth$education, \(e) c(1 - pa_prob[[e]], pa_prob[[e]])),
  c("No", "Yes")
)

# -- Dentist visit ---------------------------------------
dent_prob <- c(
  "Lower secondary" = 0.58,
  "Upper secondary" = 0.72,
  "Tertiary" = 0.84
)
sochealth$dentist_12m <- sample_by_prob(
  lapply(sochealth$education, \(e) c(1 - dent_prob[[e]], dent_prob[[e]])),
  c("No", "Yes")
)

# -- Employment status -----------------------------------
emp_levels <- c("Employed", "Student", "Unemployed", "Inactive")
emp_probs <- list(
  "Lower secondary" = c(0.55, 0.10, 0.20, 0.15),
  "Upper secondary" = c(0.62, 0.12, 0.14, 0.12),
  "Tertiary" = c(0.74, 0.10, 0.07, 0.09)
)
sochealth$employment_status <- sample_by_prob(
  lapply(sochealth$education, \(e) emp_probs[[e]]),
  emp_levels
)

# -- Income group (depends on social class) --------------
inc_levels <- c("Low", "Lower middle", "Upper middle", "High")
inc_probs <- list(
  "Lower" = c(0.58, 0.28, 0.11, 0.03),
  "Working" = c(0.34, 0.40, 0.20, 0.06),
  "Lower middle" = c(0.15, 0.42, 0.31, 0.12),
  "Middle" = c(0.06, 0.23, 0.44, 0.27),
  "Upper middle" = c(0.03, 0.10, 0.36, 0.51)
)
sochealth$income_group <- sample_by_prob(
  lapply(sochealth$social_class, \(s) inc_probs[[s]]),
  inc_levels
)

# -- Institutional trust ---------------------------------
trust_levels <- c("Very low", "Low", "High", "Very high")
trust_probs <- list(
  "Lower secondary" = c(0.26, 0.36, 0.24, 0.14),
  "Upper secondary" = c(0.28, 0.42, 0.20, 0.10),
  "Tertiary" = c(0.38, 0.39, 0.16, 0.07)
)
sochealth$institutional_trust <- sample_by_prob(
  lapply(sochealth$education, \(e) trust_probs[[e]]),
  trust_levels
)

# -- Political position (0-10, numeric) ------------------
sochealth <- sochealth |>
  mutate(
    political_position = {
      mu <- 4.8 +
        if_else(sex == "Male", 0.4, -0.1) +
        case_when(
          age < 35 ~ -0.6,
          age < 50 ~ 0.0,
          age < 65 ~ 0.5,
          TRUE ~ 0.9
        ) +
        case_when(
          income_group == "Low" ~ -0.5,
          income_group == "Lower middle" ~ 0.0,
          income_group == "Upper middle" ~ 0.5,
          income_group == "High" ~ 0.9
        ) +
        case_when(
          region == "North" ~ -0.2,
          region == "South" ~ 0.4,
          region == "East" ~ 0.3,
          region == "West" ~ -0.3,
          region == "Central" ~ 0.5,
          TRUE ~ 0.0
        )
      pmin(pmax(round(rnorm(n(), mean = mu, sd = 1.9)), 0), 10)
    }
  )

# -- Wellbeing score (0-100) -----------------------------
sochealth <- sochealth |>
  mutate(
    wellbeing_score = {
      base <- case_when(
        education == "Lower secondary" ~ 55,
        education == "Upper secondary" ~ 63,
        education == "Tertiary" ~ 70
      )
      adj_sex <- if_else(sex == "Male", 4, 0)
      adj_health <- case_when(
        self_rated_health == "Very good" ~ 12,
        self_rated_health == "Good" ~ 5,
        self_rated_health == "Fair" ~ -6,
        self_rated_health == "Poor" ~ -18,
        TRUE ~ 0
      )
      round(
        pmin(
          pmax(rnorm(n(), mean = base + adj_sex + adj_health, sd = 12), 0),
          100
        ),
        1
      )
    }
  )

# -- Life satisfaction items (1-5 Likert) ----------------
# Inspired by the Personal Wellbeing Index (International Wellbeing Group).
# A shared latent factor drives inter-item correlations (target r ~ 0.3-0.5).
# Each item also has a domain-specific component.

# Shared latent factor based on wellbeing, education, and health
latent_sat <- with(sochealth, {
  base <- (wellbeing_score - 65) *
    0.03 +
    case_when(
      education == "Tertiary" ~ 0.3,
      education == "Upper secondary" ~ 0.1,
      education == "Lower secondary" ~ -0.2
    ) +
    case_when(
      self_rated_health == "Very good" ~ 0.4,
      self_rated_health == "Good" ~ 0.1,
      self_rated_health == "Fair" ~ -0.2,
      self_rated_health == "Poor" ~ -0.5,
      TRUE ~ 0
    )
  rnorm(nrow(sochealth), mean = base, sd = 0.3)
})

sochealth <- sochealth |>
  mutate(
    life_sat_health = {
      mu <- 3.2 +
        latent_sat +
        case_when(
          self_rated_health == "Very good" ~ 0.6,
          self_rated_health == "Good" ~ 0.2,
          self_rated_health == "Fair" ~ -0.3,
          self_rated_health == "Poor" ~ -0.8,
          TRUE ~ 0
        )
      as.integer(pmin(pmax(round(rnorm(n(), mu, 0.9)), 1), 5))
    },
    life_sat_work = {
      mu <- 3.0 +
        latent_sat +
        case_when(
          employment_status == "Employed" ~ 0.3,
          employment_status == "Student" ~ 0.1,
          employment_status == "Unemployed" ~ -0.6,
          employment_status == "Inactive" ~ -0.2
        )
      as.integer(pmin(pmax(round(rnorm(n(), mu, 0.9)), 1), 5))
    },
    life_sat_relationships = {
      mu <- 3.5 +
        latent_sat +
        if_else(sex == "Female", 0.1, -0.05) +
        case_when(
          age < 35 ~ 0.1,
          age < 50 ~ 0.0,
          age < 65 ~ -0.1,
          TRUE ~ 0.05
        )
      as.integer(pmin(pmax(round(rnorm(n(), mu, 0.9)), 1), 5))
    },
    life_sat_standard = {
      mu <- 3.1 +
        latent_sat +
        case_when(
          income_group == "High" ~ 0.5,
          income_group == "Upper middle" ~ 0.2,
          income_group == "Lower middle" ~ -0.1,
          income_group == "Low" ~ -0.4,
          TRUE ~ 0
        )
      as.integer(pmin(pmax(round(rnorm(n(), mu, 0.9)), 1), 5))
    }
  )

rm(latent_sat)

# Inject missing values in life_sat items (8 per item)
set.seed(2027)
sochealth <- sochealth |>
  mutate(
    life_sat_health = replace(life_sat_health, sample(seq_len(n), 8), NA),
    life_sat_work = replace(life_sat_work, sample(seq_len(n), 8), NA),
    life_sat_relationships = replace(
      life_sat_relationships,
      sample(seq_len(n), 8),
      NA
    ),
    life_sat_standard = replace(life_sat_standard, sample(seq_len(n), 8), NA)
  )

# -- Income (continuous, CHF) ----------------------------
sochealth <- sochealth |>
  mutate(
    income = {
      base <- case_when(
        education == "Lower secondary" & sex == "Female" ~ 2200,
        education == "Lower secondary" & sex == "Male" ~ 2600,
        education == "Upper secondary" & sex == "Female" ~ 3200,
        education == "Upper secondary" & sex == "Male" ~ 3800,
        education == "Tertiary" & sex == "Female" ~ 4700,
        education == "Tertiary" & sex == "Male" ~ 5600
      )
      adj <- (wellbeing_score - 65) * 10
      round(pmin(pmax(rnorm(n(), mean = base + adj, sd = 800), 1000), 12000))
    }
  )

# -- BMI -------------------------------------------------
sochealth <- sochealth |>
  mutate(
    bmi = {
      mu <- case_when(
        education == "Lower secondary" ~ 27.0,
        education == "Upper secondary" ~ 25.6,
        education == "Tertiary" ~ 24.2
      ) +
        if_else(sex == "Male", 0.8, 0) +
        if_else(physical_activity == "Yes", -0.8, 0.5) +
        if_else(smoking == "Yes", -0.3, 0) +
        case_when(
          social_class == "Lower" ~ 0.9,
          social_class == "Working" ~ 0.5,
          social_class == "Lower middle" ~ 0.1,
          social_class == "Middle" ~ -0.3,
          social_class == "Upper middle" ~ -0.6
        ) +
        (age - 45) * 0.04
      round(pmin(pmax(rnorm(n(), mean = mu, sd = 3.4), 16), 45), 1)
    },
    bmi_category = case_when(
      bmi < 25 ~ "Normal weight",
      bmi < 30 ~ "Overweight",
      TRUE ~ "Obesity"
    )
  )

# -- Response date (POSIXct, Sep-Nov 2024) ---------------
sochealth$response_date <- as.POSIXct(
  "2024-09-02 08:00:00",
  tz = "Europe/Zurich"
) +
  sample(0:(89 * 86400L), n, replace = TRUE)

# -- Survey weight ---------------------------------------
sochealth <- sochealth |>
  mutate(
    weight = {
      w <- case_when(
        sex == "Female" & age < 35 ~ 1.45,
        sex == "Female" & age < 50 ~ 1.15,
        sex == "Female" & age < 65 ~ 0.80,
        sex == "Female" ~ 0.65,
        sex == "Male" & age < 35 ~ 1.35,
        sex == "Male" & age < 50 ~ 1.05,
        sex == "Male" & age < 65 ~ 0.70,
        TRUE ~ 0.55
      )
      round(w * rlnorm(n(), 0, 0.25), 3)
    }
  )

# -- Inject missing values -------------------------------
set.seed(2026)
sochealth <- sochealth |>
  mutate(
    smoking = replace(smoking, sample(seq_len(n), 25), NA),
    self_rated_health = replace(self_rated_health, sample(seq_len(n), 20), NA),
    income_group = replace(income_group, sample(seq_len(n), 18), NA),
    political_position = replace(
      political_position,
      sample(seq_len(n), 15),
      NA
    ),
    bmi = replace(bmi, sample(seq_len(n), 12), NA),
    bmi_category = if_else(
      is.na(bmi),
      NA_character_,
      as.character(bmi_category)
    )
  )

# -- Factor encoding -------------------------------------
sochealth <- sochealth |>
  mutate(
    sex = factor(sex),
    smoking = factor(smoking, levels = c("No", "Yes")),
    physical_activity = factor(physical_activity, levels = c("No", "Yes")),
    dentist_12m = factor(dentist_12m, levels = c("No", "Yes")),
    employment_status = factor(
      employment_status,
      levels = c("Employed", "Student", "Unemployed", "Inactive")
    ),
    education = factor(
      education,
      levels = c("Lower secondary", "Upper secondary", "Tertiary"),
      ordered = TRUE
    ),
    age_group = factor(
      age_group,
      levels = c("25-34", "35-49", "50-64", "65-75"),
      ordered = TRUE
    ),
    social_class = factor(
      social_class,
      levels = c("Lower", "Working", "Lower middle", "Middle", "Upper middle"),
      ordered = TRUE
    ),
    self_rated_health = factor(
      self_rated_health,
      levels = c("Poor", "Fair", "Good", "Very good"),
      ordered = TRUE
    ),
    bmi_category = factor(
      bmi_category,
      levels = c("Normal weight", "Overweight", "Obesity"),
      ordered = TRUE
    ),
    income_group = factor(
      income_group,
      levels = c("Low", "Lower middle", "Upper middle", "High"),
      ordered = TRUE
    ),
    institutional_trust = factor(
      institutional_trust,
      levels = c("Very low", "Low", "High", "Very high"),
      ordered = TRUE
    ),
    region = factor(region)
  )

# -- Variable labels -------------------------------------
labelled::var_label(sochealth) <- list(
  sex = "Sex",
  age = "Age (years)",
  age_group = "Age group",
  education = "Highest education level",
  social_class = "Subjective social class",
  region = "Region of residence",
  employment_status = "Employment status",
  income_group = "Household income group",
  income = "Monthly household income (CHF)",
  smoking = "Current smoker",
  physical_activity = "Regular physical activity",
  dentist_12m = "Dentist visit in last 12 months",
  self_rated_health = "Self-rated health",
  wellbeing_score = "WHO-5 wellbeing index (0-100)",
  bmi = "Body mass index",
  bmi_category = "BMI category",
  institutional_trust = "Trust in institutions",
  political_position = "Political position (0 = left, 10 = right)",
  life_sat_health = "Satisfaction with health (1-5)",
  life_sat_work = "Satisfaction with work (1-5)",
  life_sat_relationships = "Satisfaction with relationships (1-5)",
  life_sat_standard = "Satisfaction with standard of living (1-5)",
  response_date = "Survey response date",
  weight = "Survey design weight"
)

# -- Column order ----------------------------------------
sochealth <- sochealth |>
  select(
    sex,
    age,
    age_group,
    education,
    social_class,
    region,
    employment_status,
    income_group,
    income,
    smoking,
    physical_activity,
    dentist_12m,
    self_rated_health,
    wellbeing_score,
    bmi,
    bmi_category,
    institutional_trust,
    political_position,
    life_sat_health,
    life_sat_work,
    life_sat_relationships,
    life_sat_standard,
    response_date,
    weight
  )

# -- Save ------------------------------------------------
usethis::use_data(sochealth, overwrite = TRUE)
