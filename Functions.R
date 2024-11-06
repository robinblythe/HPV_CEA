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
