# Evaluate the impact of a reduction of 70% of prevalence at 80% population vaccine coverage
options(scipen = 100, digits = 5)

source("0_Functions.R")

# Change OR function to deterministic
get_or <- function(a, b, c, d) {
  p_employed_cancer <- a / (a + b)
  p_employed_no_cancer <- c / (c + d)
  (p_employed_cancer / (1 - p_employed_cancer))/(p_employed_no_cancer / (1 - p_employed_no_cancer))
}
remove(run_model)

load("model_data.Rdata")

library(tidyverse)
library(furrr)
library(progressr)

discount <- 0.03
iter <- 10000
seed <- 80126
age_start <- 16
age_end <- 84
model_year <- 2025

handlers(global = TRUE)
handlers("progress")

run_sensitivity <- function(cancer_type, gender, seed, iteration) {
  
  set.seed(seed + iteration)
  ages <- seq(age_start, age_end, 1)
  idx   <- ages - age_start + 1
  
  sim_incomes <- median_income |>
    filter(
      Sex == gender,
      Age >= age_start
    ) |>
    mutate(
      Participation_rate_cancer =
        if (gender == "Female" & cancer_type %in% c("Cervical", "Vaginal", "Vulval", "Anal")) {
          apply_odds(get_or(671, 648, 816, 506), Participation_rate)
        } else if (cancer_type == "Oropharyngeal") {
          apply_odds(get_or(75, 62, 116, 26), Participation_rate)
        } else {
          apply_odds(get_or(423, 275, 1770, 659), Participation_rate)
        },
      Weighted_income_cancer = Participation_rate_cancer * Predicted_income * 12
    )
  
  ages <- seq(age_start, age_end)
  
  income_healthy <- vapply(
    ages,
    function(x)
      get_lifetime_income(
        sim_incomes,
        age = x,
        income = "Weighted_income_annual",
        gender = gender
      ),
    numeric(1)
  )
  
  income_cancer <- vapply(
    ages,
    function(x)
      get_lifetime_income(
        sim_incomes,
        age = x,
        income = "Weighted_income_cancer",
        gender = gender
      ),
    numeric(1)
  )
  
  mort_adj <- cancer_mortality |>
    filter(
      Group == gender,
      Diagnosis == cancer_type,
      Age >= age_start
    ) |>
    mutate(
      join_age = Age + survival_time - 1,
      p_mort = 1 - p_survival
    ) |>
    left_join(
      tibble(
        Age = ages,
        income_cancer = income_cancer
      ),
      by = join_by(join_age == Age)
    ) |>
    group_by(Age) |>
    mutate(
      p_mort_annual = p_mort - lag(p_mort),
      p_mort_annual = ifelse(is.na(p_mort_annual), p_mort, p_mort_annual),
      mort_prob = ifelse(p_mort_annual == 0, NA, p_mort_annual)
    ) |>
    fill(mort_prob, .direction = "down") |>
    group_by(Age) |>
    summarise(
      mort_decrement = sum(income_cancer * mort_prob),
      .groups = "drop"
    )
  
  mort_adj[nrow(mort_adj) + 1, ] <- list(84, 0)
  
  rtw <- case_when(
    cancer_type == "Oropharyngeal" ~ 2.75,
    gender == "Male" & cancer_type != "Oropharyngeal" ~ 6.96,
    gender == "Female" & cancer_type != "Oropharyngeal" ~ 3.53
  )
  
  pct_hpv <- case_when(
    cancer_type == "Cervical" ~ 0.7,
    cancer_type == "Anal" ~ 0.75,
    cancer_type == "Vaginal" ~ 0.49,
    cancer_type == "Vulval" ~ 0.18,
    cancer_type == "Penile" ~ 0.35,
    cancer_type == "Oropharyngeal" ~ 0.85
  )
  
  vaccine_eff <- case_when(
    cancer_type == "Cervical" ~ 0.09,
    cancer_type == "Oropharyngeal" ~ 0.11,
    cancer_type == "Vaginal" | cancer_type == "Vulval" |
      (cancer_type == "Anal" & gender == "Female") ~ 0.05,
    cancer_type == "Penile" | (cancer_type == "Anal" & gender == "Male") ~ 0.16
  )
  
  cancer_rate_base <- cancer_incidence |>
    filter(
      Group == gender,
      Diagnosis == cancer_type,
      Age >= age_start
    ) |>
    arrange(Age)
  
  herd_immunity <- 1 - rbeta(1, 2.975, 1.275)
  
  annual_income <- median_income$Weighted_income_annual[median_income$Age %in% ages & median_income$Sex == gender]
  
  Sick_leave_decrement <- (rtw / 12) * -annual_income
  Mortality_decrement  <- -mort_adj$mort_decrement[idx]
  
  diag_prob <- cancer_rate_base$pred_rate[idx]
  diag_prob_herd <- cancer_rate_base$pred_rate[idx] * herd_immunity
  
  sim <- tibble(
    Iteration = iteration,
    Diagnosis = cancer_type,
    Gender = gender,
    Diagnosis_age = ages,
    
    Income_healthy = income_healthy,
    Income_cancer  = income_cancer,
    
    Sick_leave_decrement = Sick_leave_decrement,
    Mortality_decrement  = Mortality_decrement,
    
    EV_income_cancer = income_cancer + Sick_leave_decrement + Mortality_decrement,
    
    Lost_income_cancer = EV_income_cancer - income_healthy,
    
    EV_cancer_no_vc = Lost_income_cancer * diag_prob,
    
    EV_cancer_vc =
      Lost_income_cancer * diag_prob * (1 - pct_hpv) +
      Lost_income_cancer * diag_prob_herd * (pct_hpv * vaccine_eff),
    
    Vaccination_benefit =
      EV_cancer_vc - EV_cancer_no_vc
  )
}

combs <- tibble(
  cancers = c(
    "Oropharyngeal", "Cervical", "Vulval", "Vaginal", "Anal",
    "Oropharyngeal", "Penile", "Anal"
  ),
  sexes = c(rep("Female", 5), rep("Male", 3))
)

plan(multisession, workers = cores)
on.exit(plan(sequential), add = TRUE)

total_iter <- nrow(combs) * iter
cores <- availableCores()

results <- with_progress({
  p <- progressor(steps = total_iter)
  
  pmap_dfr(
    combs,
    function(cancers, sexes) {
      future_map_dfr(
        1:iter,
        function(i) {
          p()
          run_sensitivity(
            cancer_type = cancers,
            gender = sexes,
            seed = seed,
            iteration = i
          )
        },
        .options = furrr_options(seed = TRUE, scheduling = Inf)
      )
    }
  )
})

arrow::write_parquet(results, "sensitivity_analysis.parquet")
