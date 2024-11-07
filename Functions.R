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



run_model_loop <- function(cancer_type, gender) {
  sims <- list()
  for (i in 1:iter) {
    sim_incomes <- median_income |>
      filter(Sex == gender) |>
      mutate(
        Participation_rate_cancer =
          if (gender == "Female" & cancer_type == "reproductive") {
            get_odds(1 / (1 - rbeta(1, 671, 648)) / (1 / (1 - rbeta(1, 816, 506))), Participation_rate)
          } else if (cancer_type == "oropharyngeal") {
            get_odds(1 / (1 - rbeta(1, 75, 62)) / (1 / (1 - rbeta(1, 116, 26))), Participation_rate)
          } else {
            get_odds(1 / (1 - rbeta(1, 13480, 6886)) / (1 / (1 - rbeta(1, 133588, 24015))), Participation_rate)
          },
        Weighted_income_cancer = Participation_rate_cancer * Monthly_income * 12
      )
    
    lifetime_income <- list()
    lifetime_income$healthy <- lapply(
      seq(age_start, age_end, 1),
      function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_income_annual", gender = gender)
    )
    
    lifetime_income$cancer <- lapply(
      seq(age_start, age_end, 1),
      function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_income_cancer", gender = gender)
    )
    
    rtw <- if (cancer_type == "oropharyngeal") {
      rgamma(1, shape = 2.456, scale = 3.069)
    } else if (gender == "Male") {
      rgamma(1, shape = 659.678, scale = 0.159) / 30.438
    } else {
      rgamma(1, shape = 148.905, 1.088) / 30.438
    }
    
    # Don't use right now - need to find a way to incorporate probabilities in the final outcome
    # probabilities <- incidence[[tolower(gender)]][[cancer_type]] |>
    #   rename(N_cases = N) |>
    #   left_join(mortality[[tolower(gender)]][[cancer_type]],
    #     by = join_by(Age, Group, Diagnosis)
    #   ) |>
    #   ungroup() |>
    #   rename(N_deaths = N) |>
    #   mutate(Cumulative_cases = cumsum(N_cases)) |>
    #   rowwise() |>
    #   mutate(Pr_mortality = rbeta(1, N_deaths, (Cumulative_cases - N_deaths)),
    #          Pr_mortality = ifelse(N_deaths == 0 & (Cumulative_cases - N_deaths == 0), 0, Pr_mortality))
    
    sims[[i]] <- tibble(
      Iteration = i,
      Diagnosis = cancer_type,
      Gender = gender,
      Diagnosis_age = seq(age_start, age_end, 1),
    ) |>
      rowwise() |>
      mutate(
        Income_healthy = lifetime_income$healthy[[Diagnosis_age - 9]],
        Income_cancer = lifetime_income$cancer[[Diagnosis_age - 9]],
        Sick_leave_decrement = rtw / 12 * -median_income$Weighted_income_annual[median_income$Age == Diagnosis_age & median_income$Sex == gender],
        Expected_income = Income_cancer + Sick_leave_decrement,
        Lost_income = Income_healthy - Expected_income
      )
  }
  return(do.call(rbind, sims))
}

