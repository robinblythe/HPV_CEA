library(vroom)
library(tidyverse)

source("./0_Functions.R")

# Read in incidence data
df_cervical <- vroom("./Data/hpv_cervical.csv", col_types = c(gender = "c"), show_col_types = F)
df_vaginal <- vroom("./Data/hpv_vaginal.csv", col_types = c(gender = "c"), show_col_types = F)
df_oropharyngeal <- vroom("./Data/hpv_oropharyngeal.csv", col_types = c(gender = "c"), show_col_types = F)
df_anal <- vroom("./Data/hpv_anal.csv", col_types = c(gender = "c"), show_col_types = F)
df_penile <- vroom("./Data/hpv_penile.csv", col_types = c(gender = "c"), show_col_types = F)

# Create incidence dataset with census-defined denominators for rates
incidence <- list()
incidence$Female <- list()
incidence$Male <- list()

incidence$Female$Cervical <- fn_incidence(
  "F",
  df_cervical,
  "Cervical"
)

incidence$Female$Vaginal <- fn_incidence(
  "F",
  df_vaginal,
  "Vaginal"
)

incidence$Female$Oropharyngeal <- fn_incidence(
  "F",
  df_oropharyngeal,
  "Oropharyngeal"
)

incidence$Female$Anal <- fn_incidence(
  "F",
  df_anal,
  "Anal"
)

incidence$Male$Oropharyngeal <- fn_incidence(
  "M",
  df_oropharyngeal,
  "Oropharyngeal"
)

incidence$Male$Anal <- fn_incidence(
  "M",
  df_anal,
  "Anal"
)

incidence$Male$Penile <- fn_incidence(
  "M",
  df_penile,
  "Penile"
)


remove(df_cervical, df_vaginal, df_oropharyngeal, df_anal, df_penile)

# Repeat process for cancer-associated mortality rates
df_cervical_mort <- vroom("./Data/hpv_cervical_mort.csv", col_types = c(gender = "c"), show_col_types = F)
df_vaginal_mort <- vroom("./Data/hpv_vaginal_mort.csv", col_types = c(gender = "c"), show_col_types = F)
df_oropharyngeal_mort <- vroom("./Data/hpv_oropharyngeal_mort.csv", col_types = c(gender = "c"), show_col_types = F)
df_anal_mort <- vroom("./Data/hpv_anal_mort.csv", col_types = c(gender = "c"), show_col_types = F)
df_penile_mort <- vroom("./Data/hpv_penile_mort.csv", col_types = c(gender = "c"), show_col_types = F)

# Cancer-specific death rates
mortality <- list()
mortality$Female <- list()
mortality$Male <- list()

mortality$Female$Cervical <- fn_incidence(
  "F",
  df_cervical_mort,
  "Cervical"
)

mortality$Female$Vaginal <- fn_incidence(
  "F",
  df_vaginal_mort,
  "Vaginal"
)

mortality$Female$Oropharyngeal <- fn_incidence(
  "F",
  df_oropharyngeal_mort,
  "Oropharyngeal"
)

mortality$Female$Anal <- fn_incidence(
  "F",
  df_anal_mort,
  "Anal"
)

mortality$Male$Oropharyngeal <- fn_incidence(
  "M",
  df_oropharyngeal_mort,
  "Oropharyngeal"
)

mortality$Male$Anal <- fn_incidence(
  "M",
  df_anal_mort,
  "Anal"
)

mortality$Male$Penile <- fn_incidence(
  "M",
  df_penile_mort,
  "Penile"
)


remove(df_cervical_mort, df_vaginal_mort, df_oropharyngeal_mort, df_anal_mort, df_penile_mort)

# Population of 10-year-olds for model start condition
df_census <- vroom("./Data/population_by_age.csv", show_col_types = F)
census <- df_census |>
  select(Age, Gender, `2019`) |>
  rename(Pop_total = `2019`,
         Group = Gender) |>
  mutate(Age = as.numeric(substr(Age, 1, 2)))

# Probabilities by age, sex, cancer
probabilities <- bind_rows(
  do.call(rbind, incidence$Female),
  do.call(rbind, incidence$Male)
) |>
  rename(N_cases = N) |>
  left_join(
    bind_rows(
      do.call(rbind, mortality$Female),
      do.call(rbind, mortality$Male)
    ),
    by = join_by(Age, Group, Diagnosis)
  ) |>
  rename(N_deaths = N) |>
  left_join(census, by = join_by(Age, Group)) |>
  mutate(Pr_cancer = N_cases / Pop_total) |>
  ungroup()

remove(df_census, incidence, mortality)

# Baseline mortality
baseline_mortality <- vroom("./Data/census_deaths.csv") |>
  rowwise() |>
  mutate(
    start_age = substr(Age, 1, 2),
    end_age = substr(Age, 6, 7),
    Age = list(seq(start_age, end_age, 1))
  ) |>
  unnest(Age) |>
  select(Age, Group, Rate_per_1000) |>
  mutate(Mort_rate = Rate_per_1000/1000)

# Smooth out the predictions with a quasibinomial model
baseline_mortality <- baseline_mortality |>
  mutate(Pr_death = predict(glm(Mort_rate ~ Age + Group, family = quasibinomial), type = "response")) |>
  select(Age, Group, Pr_death)

probabilities <- left_join(probabilities, baseline_mortality)
remove(baseline_mortality)

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
  full_join(tibble(
    Age = rep(seq(10, 14, 1), 2),
    Sex = c(rep("Male", 5), rep("Female", 5)),
    Participation_rate = rep(0, 10)
  ),
  by = join_by(Age, Sex, Participation_rate))

remove(df_employment, fn_incidence, get_lifetime_income)

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

save(census, median_income, probabilities, file = "model_data.Rdata")
