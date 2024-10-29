# Functions for analysis

# Convert incidence to rate using census data
fn_incidence <- function(Gender, data, census_data, dx_group) {
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
    left_join(census_data) |>
    mutate(
      Rate = N / Pop_total
    ) |>
    select(Age, Group, Diagnosis, N, Rate)
}


# Fitting a quasibinomial glm quickly:
tp_model <- function(df) {
  glm(Rate ~ rcs(Age, 5),
    family = "quasibinomial",
    data = df
  )
}

get_b <- function()

# Run the model
run_sim <- function(Gender, cancer_type, Vaccine_type, n) {
  
  # Load relevant datasets
  income <- median_income |>
    filter(Sex == Gender) |>
    select(case_when(cancer_type == "oropharyngeal" ~ c("Age", "Weighted_income_annual", "Weighted_income_oro"),
      cancer_type != "oropharyngeal" & Gender == "Female" ~ c("Age", "Weighted_income_annual", "Weighted_income_female_repro"),
      .default = c("Age", "Weighted_income_annual", "Weighted_income_other_cancer")
    ))

  pct_hpv <- ifelse(Vaccine_type == "None", 1, pct_hpv[[cancer_type]]())

  vaccine_eff <- if (Vaccine_type == "None") {
    1
    } else if (cancer_type == "anal") {
    ifelse(Gender == "Male", vaccine_eff$anal_male(), vaccine_eff$anal_female())
  } else {
    vaccine_eff[[cancer_type]]()
  }

  cost_vc <- cost_vc[[Vaccine_type]]

  pr_cancer <- probabilities |>
    filter(
      grepl("incidence", probabilities$Group, ignore.case = T),
      grepl(cancer_type, probabilities$Group, ignore.case = T),
      case_when(
        cancer_type %in% c("oropharyngeal", "anal") ~
          grepl(Gender, probabilities$Group, ignore.case = T),
        .default = TRUE
      )
    )

  pr_cancer_mortality <- probabilities |>
    filter(
      grepl("mortality", probabilities$Group, ignore.case = T),
      grepl(cancer_type, probabilities$Group, ignore.case = T),
      !grepl("Baseline", probabilities$Group, ignore.case = T),
      case_when(
        cancer_type %in% c("oropharyngeal", "anal") ~
          grepl(Gender, probabilities$Group, ignore.case = T),
        .default = TRUE
      )
    )

  baseline_mortality <- probabilities |>
    filter(
      grepl("Baseline", probabilities$Group, ignore.case = T),
      grepl(Gender, probabilities$Group, ignore.case = T)
    )

  sick_leave <- case_when(
    cancer_type == "oropharyngeal" ~ rtw$oropharyngeal(),
    cancer_type != "oropharyngeal" & Gender == "Female" ~ rtw$female_genital(),
    cancer_type != "oropharyngeal" & Gender == "Female" ~ rtw$male_genital()
  )

  browser()

  # Initialise tibble with entry group
  df <- tibble(
    Age = age_start, # column 1
    Gender = Gender, # column 2
    Cancer_type = cancer_type, # column 3
    Vaccine = Vaccine_type, # column 4
    Healthy = n, # column 5
    Cancer_diagnoses = 0, # column 6
    Cancer_survivors = 0, # column 7
    Dead = 0, # column 8
    NMB = -cost_vc * n # column 9
  )

  # Create for loop that adds a row each cycle based on previous row
  for (i in 1:74) {
    df[(i+1), 1] <- age_start + i
    df[(i+1), 2] <- Gender
    df[(i+1), 3] <- cancer_type
    df[(i+1), 4] <- Vaccine_type
    # Cancer incidence and survivors
    df[(i+1), 6] <- df[i,5] * rnorm(1, mean = pr_cancer$Fit[i], sd = pr_cancer$SE.fit[i]) * vaccine_eff * pct_hpv
    df[(i+1)]
  }
}
