options(scipen = 100, digits = 5)
library(vroom)
library(tidyverse)

source("./0_Functions.R")

# Population by age
df_census <- vroom("./Data/population_by_age.csv", show_col_types = F)
census <- df_census |>
  select(Age, Gender, `2024`) |>
  rename(
    Pop_total = `2024`,
    Group = Gender
  ) |>
  mutate(Age = as.numeric(substr(Age, 1, 2)))
remove(df_census)

# Add cause-specific mortality rates
cancer_mortality <- vroom("./Data/NRDO extract/predictions_coxph.csv") |>
  filter(age_at_diagnosis + survival_time < 85) |>
  mutate(
    Age = age_at_diagnosis,
    Group = ifelse(gender == "F", "Female", "Male"),
    Diagnosis = case_when(
      cancer_type == "Cervix" ~ "Cervical",
      cancer_type == "Anus/Anal canal" ~ "Anal",
      cancer_type == "Penis" ~ "Penile",
      cancer_type == "Oropharynx" ~ "Oropharyngeal",
      cancer_type == "Vagina" ~ "Vaginal",
      cancer_type == "Vulva" ~ "Vulval"
    )
  ) |>
  select(Age, Group, Diagnosis, survival_time, p_survival, se_survival)

# Age-specific diagnosis rates
cancer_incidence <- vroom("./Data/NRDO extract/n_cases_predicted.csv") |>
  mutate(Group = ifelse(gender == "F", "Female", "Male")) |>
  left_join(census, join_by(age_at_diagnosis == Age, Group)) |>
  mutate(Rate = (n / 29) / Pop_total) |> # 30 years worth of registry data (1992 - 2021)
  group_by(cancer_type, Group) |>
  nest() %>%
  mutate(
    model = map(data, ~ glm(Rate ~ age_at_diagnosis, family = "quasibinomial", data = .)),
    pred_rate = map(model, ~ fitted(.)),
    se_pred_rate = map(model, ~ predict(.x, type = "response", se.fit = TRUE)$se.fit)
  ) |>
  unnest(cols = c(pred_rate, se_pred_rate)) |>
  mutate(
    Age = seq(10, 84, 1),
    Diagnosis = case_when(
      cancer_type == "Cervix" ~ "Cervical",
      cancer_type == "Anus/Anal canal" ~ "Anal",
      cancer_type == "Penis" ~ "Penile",
      cancer_type == "Oropharynx" ~ "Oropharyngeal",
      cancer_type == "Vagina" ~ "Vaginal",
      cancer_type == "Vulva" ~ "Vulval"
    )
  ) |>
  ungroup() |>
  select(Age, Group, Diagnosis, pred_rate, se_pred_rate)


# Median income by age group
df_median_income <- vroom("./Data/Median_income_by_age_sex.csv", delim = ",", show_col_types = F)

incomes <- df_median_income |>
  rowwise() |>
  mutate(
    start_age = substr(Age, 1, 2),
    end_age = substr(Age, 6, 7),
    Age = list(seq(start_age, end_age, 1))
  ) |>
  unnest(Age) |>
  select(Age, Sex, Monthly_income) |>
  arrange(Age)

remove(df_median_income)


# Labour force participation by age group
df_employment <- vroom("./Data/labour_participation_rate.csv", show_col_types = F)
employment <- df_employment |>
  select(-year) |>
  rowwise() |>
  mutate(
    start_age = substr(age, 1, 2),
    end_age = substr(age, 4, 5),
    Age = list(seq(start_age, end_age, 1))
  ) |>
  unnest(Age) |>
  mutate(
    Participation_rate = resident_labour_force_participation_rate / 100,
    Sex = ifelse(sex == "male", "Male", "Female")
  ) |>
  select(Age, Sex, Participation_rate) |>
  full_join(
    tibble(
      Age = rep(seq(10, 14, 1), 2),
      Sex = c(rep("Male", 5), rep("Female", 5)),
      Participation_rate = rep(0, 10)
    ),
    by = join_by(Age, Sex, Participation_rate)
  )

fit_m <- with(subset(employment, Sex == "Male"), smooth.spline(Age, Participation_rate, spar = 0.65))
fit_f <- with(subset(employment, Sex == "Female"), smooth.spline(Age, Participation_rate, spar = 0.65))
employment$Participation_rate <- ifelse(
  employment$Sex == "Male", 
  pmax(predict(fit_m, x = employment$Age)$y, 0),
  pmax(predict(fit_f, x = employment$Age)$y, 0)
)

remove(df_employment, get_lifetime_income, fit_m, fit_f)

#########################################
## Income estimation
# Estimate median wage across the workforce to generate senior incomes:
# Spar term chosen by visual analysis to create a smoothed quadratic curve
fit_m <- with(subset(incomes, Sex == "Male"), smooth.spline(Age, Monthly_income, spar = 0.9))
fit_f <- with(subset(incomes, Sex == "Female"), smooth.spline(Age, Monthly_income, spar = 0.9))
incomes$Predicted_income <- ifelse(
  incomes$Sex == "Male",
  pmax(predict(fit_m, x = incomes$Age)$y, 0),
  pmax(predict(fit_f, x = incomes$Age)$y, 0)
)


# Fit check
# incomes |> ggplot(aes(x = Age, y = Total)) +
#   geom_line() +
#   geom_line(data = income, aes(x = Age, y = Monthly_income))

# Merge employment with incomes to get an adjusted total income (annual)
median_income <- employment |>
  left_join(incomes, by = join_by(Age, Sex)) |>
  mutate(Weighted_income_annual = Participation_rate * Predicted_income * 12) %>%
  replace(is.na(.), 0) |>
  arrange(Sex, Age) |>
  select(-Monthly_income)

remove(incomes, employment, fit_m, fit_f)

save(median_income, cancer_incidence, cancer_mortality, file = "model_data.Rdata")
