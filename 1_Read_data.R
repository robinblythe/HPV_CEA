options(scipen = 100, digits = 5)
library(vroom)
library(tidyverse)

source("./0_Functions.R")

# Population by age
df_census <- vroom("./Data/population_by_age.csv", show_col_types = F)
census <- df_census |>
  select(Age, Gender, `2019`) |>
  rename(
    Pop_total = `2019`,
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
  mutate(Rate = (n / 20) / Pop_total) |> # 20 years worth of registry data (1992 - 2022)
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
df_median_income <- vroom("./Data/Median_income_by_age.csv", delim = ",", show_col_types = F)

incomes <- df_median_income |>
  rowwise() |>
  mutate(
    start_age = substr(Age, 1, 2),
    end_age = substr(Age, 6, 7),
    Age = list(seq(start_age, end_age, 1))
  ) |>
  unnest(Age) |>
  select(Age, Total) |>
  bind_rows(tibble(
    Age = c(
      seq(10, 14, 1),
      seq(65, 84, 1)
    ),
    Total = c(
      rep(0, 5),
      rep(NA_real_, 20)
    )
  )) |>
  arrange(Age)

remove(df_median_income)


# Labour force participation by age group
# Should add a smoother here
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

remove(df_employment, get_lifetime_income)

#########################################
## Income estimation
# Estimate median wage across the workforce to generate senior incomes:
# Spar term chosen by visual analysis to create a smoothed quadratic curve
fit <- with(na.omit(incomes), smooth.spline(Age, Total, spar = 0.94))
incomes <- do.call(cbind, predict(fit, x = seq(15, 84, 1))) |>
  as_tibble() |>
  rename(
    Age = x,
    Total = y
  ) |>
  mutate(Monthly_income = ifelse(Total < 0, 0, Total)) |>
  select(-Total)

# Fit check
# incomes |> ggplot(aes(x = Age, y = Total)) +
#   geom_line() +
#   geom_line(data = income, aes(x = Age, y = Monthly_income))

# Merge employment with incomes to get an adjusted total income (annual)
median_income <- employment |>
  left_join(incomes, by = join_by(Age)) |>
  mutate(Weighted_income_annual = Participation_rate * Monthly_income * 12) %>%
  replace(is.na(.), 0) |>
  arrange(Sex, Age)

remove(incomes, employment, fit)

save(median_income, cancer_incidence, cancer_mortality, file = "model_data.Rdata")
