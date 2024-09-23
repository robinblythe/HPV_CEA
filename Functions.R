# Functions for analysis

# Convert incidence to rate using census data
fn_incidence <- function(gender, incidence_data, census_data, dx_group) {
  incidence_data |>
    select(year, age, gender, Freq) |>
    filter(
      !(age %in% c("<10", ">85")),
      gender == gender
    ) |>
    rowwise() |>
    mutate(
      start_year = as.integer(substr(year, 1, 4)),
      end_year = as.integer(substr(year, 6, 9)),
      Year = list(seq(start_year, end_year, 1)),
      Freq = as.integer(ifelse(Freq == -99, runif(1, 1, 4), Freq)) / (end_year - start_year + 1)
    ) |>
    unnest(Year) |>
    rowwise() |>
    mutate(
      start_age = as.integer(substr(age, 1, 2)),
      end_age = as.integer(substr(age, 4, 5)),
      Age = list(seq(start_age, end_age, 1)),
      Freq = Freq / (end_age - start_age + 1)
    ) |>
    unnest(Age) |>
    group_by(Age, gender) |>
    summarise(Freq = mean(Freq)) |>
    left_join(census_data) |>
    mutate(
      Rate = Freq / Pop_total,
      Diagnosis = dx_group,
      Group = gender
    ) |>
    select(Age, Group, Diagnosis, Freq, Rate)
}

# Fitting a quasibinomial glm quickly:
tp_model <- function(df){
  glm(Rate ~ rcs(Age, 5),
      family = "quasibinomial",
      data = df)
}
