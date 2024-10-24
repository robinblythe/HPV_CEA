library(vroom)
library(tidyverse)


source("./Functions.R")

# Read in incidence data and census for 2021
df_census <- vroom("./Data/population_by_age.csv", show_col_types = F)
df_census_mort <- vroom("./Data/census_deaths.csv", show_col_types = F)

df_cervical <- vroom("./Data/hpv_cervical.csv", col_types = c(gender = "c"), show_col_types = F)
df_vaginal <- vroom("./Data/hpv_vaginal.csv", col_types = c(gender = "c"), show_col_types = F)
df_oropharyngeal <- vroom("./Data/hpv_oropharyngeal.csv", col_types = c(gender = "c"), show_col_types = F)
df_anal <- vroom("./Data/hpv_anal.csv", col_types = c(gender = "c"), show_col_types = F)
df_penile <- vroom("./Data/hpv_penile.csv", col_types = c(gender = "c"), show_col_types = F)

# Obtain 2021 census data by age (discrete)
census_mort <- df_census_mort |>
  rowwise() |>
  mutate(
    start_age = substr(Age, 1, 2),
    end_age = substr(Age, 6, 7),
    Age = list(seq(start_age, end_age, 1))
  ) |>
  unnest(Age) |>
  select(Age, Group, Rate_per_1000)

census <- df_census |>
  select(Age, Gender, `2021`) |>
  rename(
    Pop_total = `2021`,
    Group = Gender
  ) |>
  mutate(Age = as.numeric(substr(Age, 1, 2))) |>
  left_join(census_mort) |>
  mutate(
    Pr_death_annual = Rate_per_1000 / 1000,
    N_deaths = Pr_death_annual * Pop_total
  ) |>
  select(Age, Group, Pr_death_annual, Pop_total, N_deaths)

remove(df_census_mort, census_mort)

# Create incidence dataset with census-defined denominators for rates
incidence_rates <- rbind(
  fn_incidence(
    "F",
    df_cervical,
    census,
    "Cervical"
  ),
  fn_incidence(
    "F",
    df_vaginal,
    census,
    "Vaginal"
  ),
  fn_incidence(
    "F",
    df_oropharyngeal,
    census,
    "Oropharyngeal"
  ),
  fn_incidence(
    "F",
    df_anal,
    census,
    "Anal"
  ),
  fn_incidence(
    "M",
    df_oropharyngeal,
    census,
    "Oropharyngeal"
  ),
  fn_incidence(
    "M",
    df_anal,
    census,
    "Anal"
  ),
  fn_incidence(
    "M",
    df_penile,
    census,
    "Penile"
  )
)

remove(df_cervical, df_vaginal, df_oropharyngeal, df_anal, df_penile)

# Repeat process for cancer-associated mortality rates
df_cervical_mort <- vroom("./Data/hpv_cervical_mort.csv", col_types = c(gender = "c"), show_col_types = F)
df_vaginal_mort <- vroom("./Data/hpv_vaginal_mort.csv", col_types = c(gender = "c"), show_col_types = F)
df_oropharyngeal_mort <- vroom("./Data/hpv_oropharyngeal_mort.csv", col_types = c(gender = "c"), show_col_types = F)
df_anal_mort <- vroom("./Data/hpv_anal_mort.csv", col_types = c(gender = "c"), show_col_types = F)
df_penile_mort <- vroom("./Data/hpv_penile_mort.csv", col_types = c(gender = "c"), show_col_types = F)

# Cancer-specific death rates
death_rates <- rbind(
  fn_incidence(
    "F",
    df_cervical_mort,
    census,
    "Cervical"
  ),
  fn_incidence(
    "F",
    df_vaginal_mort,
    census,
    "Vaginal"
  ),
  fn_incidence(
    "F",
    df_oropharyngeal_mort,
    census,
    "Oropharyngeal"
  ),
  fn_incidence(
    "F",
    df_anal_mort,
    census,
    "Anal"
  ),
  fn_incidence(
    "M",
    df_oropharyngeal_mort,
    census,
    "Oropharyngeal"
  ),
  fn_incidence(
    "M",
    df_anal_mort,
    census,
    "Anal"
  ),
  fn_incidence(
    "M",
    df_penile_mort,
    census,
    "Penile"
  )
)

remove(df_cervical_mort, df_vaginal_mort, df_oropharyngeal_mort, df_anal_mort, df_penile_mort)

# Baseline mortality less cancer-related deaths
# Merge the cancer-specific mortality numbers, subtract from the overall mortality numbers
baseline_less_cancer <- death_rates |>
  group_by(Age, Group) |>
  summarise(Deaths = sum(N)) |>
  full_join(census) |>
  mutate(
    Deaths_adjusted = N_deaths - Deaths,
    Rate = Deaths_adjusted / Pop_total
  ) |>
  select(Age, Group, Deaths_adjusted, Rate)

remove(census, fn_incidence, fn_cancer_mortality)

# Population of 10-year-olds, prepare for ARIMA
population_age_10 <- df_census |>
  rename(Group = Gender) |>
  filter(Age == "10 Year") |>
  mutate(across(c(where(is.character), -c(Age, Group)), as.numeric)) |>
  pivot_longer(!c(Age, Group), names_to = "Year", values_to = "Population") |>
  select(-Age) |>
  mutate(Year = as.numeric(Year))

remove(df_census)

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
  ))

remove(df_employment)

save.image(file = "model_data.Rdata")
