library(vroom)
library(tidyverse)

source("./Functions.R")

# Read in incidence data and census
df_census <- vroom("./Data/census_population.csv", show_col_types = F)
df_census_mort <- vroom("./Data/census_deaths.csv", show_col_types = F)

df_cervical <- vroom("./Data/hpv_cervical.csv", col_types = c(gender = "c"), show_col_types = F)
df_vaginal <- vroom("./Data/hpv_vaginal.csv", col_types = c(gender = "c"), show_col_types = F)
df_oropharyngeal <- vroom("./Data/hpv_oropharyngeal.csv", col_types = c(gender = "c"), show_col_types = F)
df_anal <- vroom("./Data/hpv_anal.csv", col_types = c(gender = "c"), show_col_types = F)
df_penile <- vroom("./Data/hpv_penile.csv", col_types = c(gender = "c"), show_col_types = F)

# Split census by age
census <- df_census |>
  full_join(df_census_mort) |>
  rowwise() |>
  mutate(
    start_age = substr(Age, 1, 2),
    end_age = substr(Age, 6, 7),
    Age = list(seq(start_age, end_age, 1))
  ) |>
  unnest(Age) |>
  mutate(
    Pop_total = Number / 5,
    Pr_death_annual = Rate_per_1000 / 1000,
    N_deaths = Pr_death_annual * Pop_total
  ) |>
  select(Age, Group, Pr_death_annual, Pop_total, N_deaths)

remove(df_census, df_census_mort)

# Create incidence dataset with census-defined denominators for rates
incidence_rates <- rbind(
  fn_incidence(
    "F",
    df_cervical,
    subset(census, Group == "Female"),
    "Cervical"
  ),
  fn_incidence(
    "F",
    df_vaginal,
    subset(census, Group == "Female"),
    "Vaginal"
  ),
  fn_incidence(
    "F",
    df_oropharyngeal,
    subset(census, Group == "Female"),
    "Oropharyngeal"
  ),
  fn_incidence(
    "F",
    df_anal,
    subset(census, Group == "Female"),
    "Anal"
  ),
  fn_incidence(
    "M",
    df_oropharyngeal,
    subset(census, Group == "Male"),
    "Oropharyngeal"
  ),
  fn_incidence(
    "M",
    df_anal,
    subset(census, Group == "Male"),
    "Anal"
  ),
  fn_incidence(
    "M",
    df_penile,
    subset(census, Group == "Male"),
    "Penile"
  )
) |>
  mutate(Group = ifelse(Group == "F", "Female", "Male"))

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
    subset(census, Group == "Female"),
    "Cervical"
  ),
  fn_incidence(
    "F",
    df_vaginal_mort,
    subset(census, Group == "Female"),
    "Vaginal"
  ),
  fn_incidence(
    "F",
    df_oropharyngeal_mort,
    subset(census, Group == "Female"),
    "Oropharyngeal"
  ),
  fn_incidence(
    "F",
    df_anal_mort,
    subset(census, Group == "Female"),
    "Anal"
  ),
  fn_incidence(
    "M",
    df_oropharyngeal_mort,
    subset(census, Group == "Male"),
    "Oropharyngeal"
  ),
  fn_incidence(
    "M",
    df_anal_mort,
    subset(census, Group == "Male"),
    "Anal"
  ),
  fn_incidence(
    "M",
    df_penile_mort,
    subset(census, Group == "Male"),
    "Penile"
  )
) |>
  mutate(Group = ifelse(Group == "F", "Female", "Male"))

remove(df_cervical_mort, df_vaginal_mort, df_oropharyngeal_mort, df_anal_mort, df_penile_mort)

# Baseline mortality less cancer-related deaths
# Merge the cancer-specific mortality numbers, subtract from the overall mortality numbers

baseline_less_cancer <- death_rates |>
  group_by(Age, Group) |>
  summarise(Freq = sum(Freq)) |>
  full_join(census) |>
  mutate(
    Deaths_adjusted = N_deaths - Freq,
    Rate = Deaths_adjusted / Pop_total
  )

remove(census, fn_incidence)
