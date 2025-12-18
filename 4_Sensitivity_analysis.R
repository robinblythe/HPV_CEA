# Evaluate the minimum income modifier and vaccine cost for which vaccination is cost-saving in men and women
source("0_Functions.R")
load("model_data.Rdata")

library(tidyverse)

cancers <- c(
  "Oropharyngeal", "Cervical", "Vulval", "Vaginal", "Anal",
  "Oropharyngeal", "Penile", "Anal"
)
sexes <- c(rep("Female", 5), rep("Male", 3))

# Set up loop over each cancer/sex/income modifier (note income in monthly increments)

# Take EV of each parameter except income:
sim_incomes <- median_income |>
  filter(
    Sex == gender,
    Age >= age_start
  ) |>
  mutate(
    Participation_rate_cancer =
      if (gender == "Female" & cancer_type %in% c("Cervical", "Vaginal", "Vulval", "Anal")) {
        get_odds(1 / (1 - 0.509) / (1 / (1 - 0.617)), Participation_rate)
      } else if (cancer_type == "Oropharyngeal") {
        get_odds(1 / (1 - 0.547) / (1 / (1 - 0.817)), Participation_rate)
      } else {
        get_odds(1 / (1 - 0.662) / (1 / 0.848), Participation_rate)
      },
    Weighted_income_cancer = Participation_rate_cancer * Predicted_income * 12
  )

lifetime_income <- list()
lifetime_income$healthy <- lapply(
  seq(age_start, age_end, 1),
  function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_income_annual", gender = gender)
)

lifetime_income$cancer <- lapply(
  seq(age_start, age_end, 1),
  function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_income_cancer", gender = gender)
)

mort_adj <- cancer_mortality |>
  filter(
    Group == gender,
    Diagnosis == cancer_type,
    Age >= age_start
  ) |>
  mutate(
    join_age = Age + survival_time - 1,
    p_mort = 1 - p_survival
  ) |>
  left_join(
    tibble(
      Age = seq(age_start, age_end, 1),
      income_cancer = as.vector(do.call(rbind, lifetime_income$cancer))
    ),
    by = join_by(join_age == Age)
  ) |>
  group_by(Age) |>
  mutate(
    p_mort_annual = p_mort - lag(p_mort),
    p_mort_annual = ifelse(is.na(p_mort_annual), p_mort, p_mort_annual),
    p_mort_annual = ifelse(p_mort_annual == 0, NA, p_mort_annual)
    ) |>
  fill(p_mort_annual, .direction = c("down")) |>
  rename(mort_prob = p_mort_annual) |>
  group_by(Age) |>
  mutate(annual_decrement = income_cancer * mort_prob) |>
  summarise(mort_decrement = sum(annual_decrement))

mort_adj[nrow(mort_adj) + 1, ] <- list(84, 0)

# Rate of cancer diagnoses in the population
cancer_rate <- cancer_incidence |>
  filter(
    Group == gender,
    Diagnosis == cancer_type,
    Age >= age_start
  ) |>
  mutate(
    diag_prob = pred_rate
  ) |>
  ungroup() |>
  select(Age, Group, Diagnosis, diag_prob)

rtw <- if (cancer_type == "Oropharyngeal") {
  2.456 * 3.069
} else if (gender == "Male") {
  659.678 * 0.159 / 30.438
} else {
  148.905 * 1.088 / 30.438
}

pct_hpv <- case_when(
  cancer_type == "Cervical" ~ 1,
  cancer_type == "Anal" ~ 35000/40000,
  cancer_type == "Vaginal" ~ 12000/15000,
  cancer_type == "Vulval" ~ 8500/34000,
  cancer_type == "Penile" ~ 13000/26000,
  cancer_type == "Oropharyngeal" ~ 29000/96000
)

# Probability that HPV vaccination protects against cancer diagnosis (compared to naive)
vaccine_eff <- case_when(
  # Cervical BIVALENT: https://www.nejm.org/doi/full/10.1056/NEJMoa1917338
  cancer_type == "Cervical" ~ 0.773 / (0.773 + 7.787),
  # Oral BIVALENT: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0068329#s3
  cancer_type == "Oropharyngeal" ~ 0.981 / (0.981 + 7.770),
  # Other/genital BIVALENT: https://www.sciencedirect.com/science/article/pii/S0755498214004771
  cancer_type == "Vaginal" | cancer_type == "Vulval" | (cancer_type == "Anal" & gender == "Female") ~ 0.930 / (0.930 + 18.548),
  cancer_type == "Penile" | (cancer_type == "Anal" & gender == "Male") ~ 7.524 / (7.524 + 39.539)
)




income_sims <- tibble(
  Income = NA,
  Diagnosis = cancer_type,
  Gender = gender,
  Diagnosis_age = seq(age_start, age_end, 1)
) |>
  rowwise() |>
  mutate(
    Income_healthy = lifetime_income$healthy[[Diagnosis_age - age_start + 1]],
    Income_cancer = lifetime_income$cancer[[Diagnosis_age - age_start + 1]],
    Sick_leave_decrement = (rtw / 12) * -median_income$Weighted_income_annual[median_income$Age == Diagnosis_age & median_income$Sex == gender],
    Mortality_decrement = -mort_adj$mort_decrement[[Diagnosis_age - age_start + 1]], # Lost income due to excess cancer mortality
    EV_income_cancer = Income_cancer + Sick_leave_decrement + Mortality_decrement, # Expected lifetime income after cancer diagnosis
    Lost_income_cancer = EV_income_cancer - Income_healthy, # Lost income due to cancer diagnosis
    EV_cancer_no_vc = Lost_income_cancer * cancer_rate$diag_prob[[Diagnosis_age - age_start + 1]], # Prevalence adjusted lost income
    EV_cancer_vc =
      Lost_income_cancer * cancer_rate$diag_prob[[Diagnosis_age - age_start + 1]] * (1 - pct_hpv) + # Diagnoses not due to HPV
      Lost_income_cancer * cancer_rate$diag_prob[[Diagnosis_age - age_start + 1]] * (pct_hpv * vaccine_eff), # Diagnoses preventable by vaccination
    Vaccination_benefit = EV_cancer_vc - EV_cancer_no_vc
  )



# Calculate the proportion of explained variation in the outcome for each model input