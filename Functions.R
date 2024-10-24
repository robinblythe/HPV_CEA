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

run_sim <- function(gender, cancer_type, vaccine_type, n){
  
  # Initialise tibble with entry group
  df <- tibble(
    Gender = gender,
    Cancer_type = cancer_type,
    Vaccine = vaccine_type,
    Age = 10,
    Healthy = n,
    Cancer = 0,
    Dead = 0,
    Cost = case_when(
      vaccine_type == "None" ~ 0,
      vaccine_type == "Bivalent" ~ n * cost_vc[[1]],
      vaccine_type == "Quadrivalent" ~ n * cost_vc[[2]],
      vaccine_type == "Nonavalent" ~ n * cost_vc[[3]]
    )
  )
  
  # Create for loop that adds a row each cycle based on previous row
  for (i in 1:74){
    next_year = df |>
      slice(n) |>
      mutate()
    
  }
  
}