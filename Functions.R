# Functions for analysis

# Convert incidence to rate using census data
fn_incidence <- function(Gender, data, dx_group) {
  data |>
    select(year, age, gender, Freq) |>
    filter(
      !(age %in% c("<10", ">85")),
      gender == Gender
    ) |>
    rowwise() |>
    mutate(
      start_year = as.integer(substr(year, 1, 4)),
      end_year = as.integer(substr(year, 6, 9)),
      Year = list(seq(start_year, end_year, 1)),
      Freq = as.integer(ifelse(Freq == -99, runif(1, 1, 4), Freq)) / (end_year - start_year + 1),
      Diagnosis = dx_group,
      Group = ifelse(gender == "F", "Female", "Male")
    ) |>
    unnest(Year) |>
    rowwise() |>
    mutate(
      start_age = as.integer(substr(age, 1, 2)),
      end_age = as.integer(substr(age, 4, 5)),
      Age = list(seq(start_age, end_age, 1)),
      N = Freq / (end_age - start_age + 1)
    ) |>
    unnest(Age) |>
    group_by(Age, Group, Diagnosis) |>
    summarise(N = mean(N)) |>
    select(Age, Group, Diagnosis, N)
}

get_odds <- function(OR, p0) {
  p_odds <- p0 / (1 - p0)
  o <- p_odds * OR
  p1 <- o / (1 + o)
  return(p1)
}



# Get lifetime expected income for each group
get_lifetime_income <- function(data, age, income, gender) {
  data |>
    filter(
      Age >= age,
      Sex == gender
    ) |>
    rowwise() |>
    mutate(discounted_income = get({{ income }}) / (1 + discount)^row_number()) |>
    group_by(Sex) |>
    summarise(lifetime_income = sum(discounted_income)) |>
    ungroup() |>
    pull()
}


run_sim <- function(cancer_type, gender) {
  sims <- list()
  for (i in 1:iter) {
    sims[[i]] <- tibble(
      Diagnosis = cancer_type,
      Gender = gender,
      Diagnosis_age = seq(age_start, age_end, 1),
      Treatment_duration_months = case_when(
        gender == "Male" & cancer_type != "Oropharyngeal cancer" ~ rtw$male_genital[[i]],
        gender == "Female" & cancer_type != "Oropharyngeal cancer" ~ rtw$female_genital[[i]],
        .default = rtw$oropharyngeal[[i]]
      )
    ) |>
      rowwise() |>
      mutate(
        Income_healthy = lifetime_income[[tolower(gender)]]$healthy[[Diagnosis_age - 9]][[i]],
        Income_cancer = case_when(
          gender == "Female" & cancer_type == "Reproductive cancer" ~ lifetime_income$female$repro[[Diagnosis_age - 9]][[i]],
          gender == "Female" & cancer_type == "Anal cancer" ~ lifetime_income$female$other[[Diagnosis_age - 9]][[i]],
          gender == "Female" & cancer_type == "Oropharyngeal cancer" ~ lifetime_income$female$oro[[Diagnosis_age - 9]][[i]],
          gender == "Male" & cancer_type == "Oropharyngeal cancer" ~ lifetime_income$male$oro[[Diagnosis_age - 9]][[i]],
          .default = lifetime_income$male$other[[Diagnosis_age - 9]][[i]]
        ),
        Sick_leave_decrement = Treatment_duration_months / 12 * -median_income$Weighted_income_annual[median_income$Age == Diagnosis_age & median_income$Sex == gender],
        Expected_income = Income_cancer + Sick_leave_decrement,
        Lost_income = Income_healthy - Expected_income
      )
  }
  return(sims)
}
