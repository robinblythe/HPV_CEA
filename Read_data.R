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
  rename(Pop_total = `2021`,
         Group = Gender) |>
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
  fn_cancer_mortality(
    "F",
    df_cervical_mort,
    incidence_rates,
    "Cervical"
  ),
  fn_cancer_mortality(
    "F",
    df_vaginal_mort,
    incidence_rates,
    "Vaginal"
  ),
  fn_cancer_mortality(
    "F",
    df_oropharyngeal_mort,
    incidence_rates,
    "Oropharyngeal"
  ),
  fn_cancer_mortality(
    "F",
    df_anal_mort,
    incidence_rates,
    "Anal"
  ),
  fn_cancer_mortality(
    "M",
    df_oropharyngeal_mort,
    incidence_rates,
    "Oropharyngeal"
  ),
  fn_cancer_mortality(
    "M",
    df_anal_mort,
    incidence_rates,
    "Anal"
  ),
  fn_cancer_mortality(
    "M",
    df_penile_mort,
    incidence_rates,
    "Penile"
  )
)

remove(df_cervical_mort, df_vaginal_mort, df_oropharyngeal_mort, df_anal_mort, df_penile_mort)

# Baseline mortality less cancer-related deaths
# Merge the cancer-specific mortality numbers, subtract from the overall mortality numbers
baseline_less_cancer <- death_rates |>
  group_by(Age, Group) |>
  summarise(Deaths = sum(Deaths)) |>
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
