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

# Run the model
run_sim <- function(gender, cancer_type, vaccine_type, n){
  
  browser()
  
  # Load relevant datasets
  income <- median_income |>
    filter(Sex == gender) |>
    select(case_when(cancer_type == "oropharyngeal" ~ c("Age", "Weighted_income_annual", "Weighted_income_oro"),
                     cancer_type != "oropharyngeal" & gender == "Female" ~ c("Age", "Weighted_income_annual", "Weighted_income_female_repro"),
                     .default = c("Age", "Weighted_income_annual", "Weighted_income_other_cancer")))
  
  pct_hpv <- pct_hpv[[cancer_type]]
  
  pr_cancer <- probabilities |>
    filter(grepl("incidence", probabilities$Group, ignore.case = T),
           grepl(cancer_type, probabilities$Group, ignore.case = T),
           case_when(cancer_type %in% c("oropharyngeal", "anal") ~ 
                       grepl(gender, probabilities$Group, ignore.case = T),
                     .default = TRUE))
  
  pr_cancer_mortality <- probabilities |>
    filter(grepl("mortality", probabilities$Group, ignore.case = T),
           grepl(cancer_type, probabilities$Group, ignore.case = T),
           !grepl("Baseline", probabilities$Group, ignore.case = T),
           case_when(cancer_type %in% c("oropharyngeal", "anal") ~ 
                       grepl(gender, probabilities$Group, ignore.case = T),
                     .default = TRUE))
  
  baseline_mortality <- probabilities |>
    filter(grepl("Baseline", probabilities$Group, ignore.case = T),
           grepl(gender, probabilities$Group, ignore.case = T))
  
  sick_leave <- case_when(
    cancer_type == "oropharyngeal" ~ rtw$oropharyngeal,
    cancer_type != "oropharyngeal" & gender == "Female" ~ rtw$female_genital,
    cancer_type != "oropharyngeal" & gender == "Female" ~ rtw$male_genital
  )

# next step - check vaccine effectiveness
  
  # Initialise tibble with entry group
  df <- tibble(
    Age = 10,
    Gender = gender,
    Cancer_type = cancer_type,
    Vaccine = vaccine_type,
    Healthy = n,
    Cancer = 0,
    Dead = 0,
    Cost = case_when(
      vaccine_type == "None" ~ 0,
      vaccine_type == "Bivalent" ~ n * -cost_vc[[1]],
      vaccine_type == "Quadrivalent" ~ n * -cost_vc[[2]],
      vaccine_type == "Nonavalent" ~ n * -cost_vc[[3]]
    )
  )
  
  # Create for loop that adds a row each cycle based on previous row
  for (i in 1:74){
    next_year = df |>
      slice(n) |>
      mutate()
    
  }
  
}