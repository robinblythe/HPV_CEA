# Evaluate the impact of a reduction of 70% of prevalence at 80% population vaccine coverage
options(scipen = 100, digits = 5)

source("0_Functions.R")
remove(run_model_loop)
load("model_data.Rdata")

library(tidyverse)
library(furrr)

discount <- 0.03
iter <- 1000
age_start <- 16
age_end <- 84
model_year <- 2025

run_sensitivity_analysis <- function(cancer_type, gender, seed) {
  
  set.seed(seed)
  sims <- list()
  
  for (i in 1:iter) {
    # Convert baseline participation rate to odds and multiply by cancer employment OR
    # Workforce participation due to cancers
    # https://jamanetwork.com/journals/jama/fullarticle/183387
    
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
    
    # Lifetime income function summarises expected remaining income for each year (x) until end of working life
    lifetime_income <- list()
    lifetime_income$healthy <- lapply(
      seq(age_start, age_end, 1),
      function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_income_annual", gender = gender)
    )
    
    lifetime_income$cancer <- lapply(
      seq(age_start, age_end, 1),
      function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_income_cancer", gender = gender)
    )
    
    # Mortality adjustment
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
          Age = seq(age_start, age_end, 1),
          income_cancer = as.vector(do.call(rbind, lifetime_income$cancer))
        ),
        by = join_by(join_age == Age)
      ) |>
      group_by(Age) |>
      mutate(
        p_mort_annual = p_mort - lag(p_mort),
        p_mort_annual = ifelse(is.na(p_mort_annual), p_mort, p_mort_annual),
        p_mort_annual = ifelse(p_mort_annual == 0, NA, p_mort_annual),
        se_mort_diff = sqrt(se_survival + lag(se_survival, default = 0))
      ) |>
      fill(p_mort_annual, .direction = c("down")) |>
      rowwise() |>
      mutate(
        beta_params = list(est_beta(p_mort_annual, se_mort_diff)),
        alpha = beta_params[[1]],
        beta = beta_params[[2]],
        mort_prob = rbeta(1, alpha, beta)
      ) |>
      group_by(Age) |>
      mutate(annual_decrement = income_cancer * mort_prob) |>
      summarise(mort_decrement = sum(annual_decrement))
    
    # Add back in the cut-off 84 y/o remainder
    mort_adj[nrow(mort_adj) + 1, ] <- list(84, 0)
    
    # Rate of cancer diagnoses in the population
    cancer_rate <- cancer_incidence |>
      filter(
        Group == gender,
        Diagnosis == cancer_type,
        Age >= age_start
      ) |>
      rowwise() |>
      mutate(
        beta_params = list(est_beta(pred_rate, se_pred_rate)),
        alpha = beta_params[[1]],
        beta = beta_params[[2]],
        diag_prob = rbeta(1, alpha, beta),
        diag_prob_herd = diag_prob *  (1 - rbeta(1, 2.347, 0.982))
      ) |>
      ungroup() |>
      select(Age, Group, Diagnosis, diag_prob, diag_prob_herd)
    
    # Return to work/sick leave due to diagnosis (in)
    # https://doi.org/10.1002/pon.1820
    rtw <- case_when(
      cancer_type == "Oropharyngeal" ~ rgamma(1, shape = 2.456, scale = 3.069),
      gender == "Male" & cancer_type != "Oropharyngeal" ~ rgamma(1, shape = 659.678, scale = 0.159) / 30.438,
      gender == "Female" & cancer_type != "Oropharyngeal" ~ rgamma(1, shape = 148.905, scale = 1.088) / 30.438
    )
    
    # Percent of cancer attributable to HPV 16/18
    # https://onlinelibrary.wiley.com/doi/epdf/10.1002/ijc.30716
    pct_hpv <- case_when(
      cancer_type == "Cervical" ~ rbeta(1, shape1 = 370000, shape2 = (530000 - 370000)),
      cancer_type == "Anal" ~ rbeta(1, shape1 = 30000, shape2 = (40000 - 30000)),
      cancer_type == "Vaginal" ~ rbeta(1, shape1 = 7400, shape2 = (15000 - 7400)),
      cancer_type == "Vulval" ~ rbeta(1, shape1 = 6200, shape2 = (34000 - 6200)),
      cancer_type == "Penile" ~ rbeta(1, shape1 = 9100, shape2 = (26000 - 9100)),
      # Note the paper doesn't split oro in the 16/18 vs other HPV strains - used the same proportion as an assumption
      cancer_type == "Oropharyngeal" ~ rbeta(1, shape1 = 0.849*96000, shape2 = (96000 - 0.849*96000))
    )
    
    # Probability that HPV vaccination protects against cancer diagnosis (compared to naive)
    vaccine_eff <- case_when(
      # Cervical BIVALENT: https://www.nejm.org/doi/full/10.1056/NEJMoa1917338
      cancer_type == "Cervical" ~ rbeta(1, shape1 = 0.773, shape2 = 7.787),
      # Oral BIVALENT: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0068329#s3
      cancer_type == "Oropharyngeal" ~ rbeta(1, shape1 = 0.981, shape2 = 7.770),
      # Other/genital BIVALENT: https://www.sciencedirect.com/science/article/pii/S0755498214004771
      cancer_type == "Vaginal" | cancer_type == "Vulval" | (cancer_type == "Anal" & gender == "Female") ~ rbeta(1, shape1 = 0.930, shape2 = 18.548),
      cancer_type == "Penile" | (cancer_type == "Anal" & gender == "Male") ~ rbeta(1, shape1 = 7.524, shape2 = 39.539)
    )
    
    # Populate table of results
    sims[[i]] <- tibble(
      Iteration = i,
      Diagnosis = cancer_type,
      Gender = gender,
      Diagnosis_age = seq(age_start, age_end, 1)
    ) |>
      rowwise() |>
      mutate(
        Income_healthy = lifetime_income$healthy[[Diagnosis_age - age_start + 1]],
        Income_cancer = lifetime_income$cancer[[Diagnosis_age - age_start + 1]],
        Sick_leave_decrement = (rtw / 12) * -median_income$Weighted_income_annual[median_income$Age == Diagnosis_age & median_income$Sex == gender],
        Mortality_decrement = -mort_adj$mort_decrement[[Diagnosis_age - age_start + 1]], # Lost income due to excess cancer mortality
        EV_income_cancer = Income_cancer + Sick_leave_decrement + Mortality_decrement, # Expected lifetime income after cancer diagnosis
        Lost_income_cancer = EV_income_cancer - Income_healthy, # Lost income due to cancer diagnosis
        EV_cancer_no_vc = Lost_income_cancer * cancer_rate$diag_prob[[Diagnosis_age - age_start + 1]], # Prevalence adjusted lost income
        EV_cancer_vc =
          Lost_income_cancer * cancer_rate$diag_prob[[Diagnosis_age - age_start + 1]] * (1 - pct_hpv) + # Diagnoses not due to HPV
          Lost_income_cancer * cancer_rate$diag_prob_herd[[Diagnosis_age - age_start + 1]] * (pct_hpv * vaccine_eff), # Diagnoses preventable by vaccination
        Vaccination_benefit = EV_cancer_vc - EV_cancer_no_vc
      )
  }
  return(do.call(rbind, sims))
}

combs <- tibble(
  cancers = c(
    "Oropharyngeal", "Cervical", "Vulval", "Vaginal", "Anal",
    "Oropharyngeal", "Penile", "Anal"
  ),
  sexes = c(rep("Female", 5), rep("Male", 3))
)

plan(multisession, workers = availableCores() - 2)

results <- future_map(1:nrow(combs), function(i){
  inputs = combs[i,]
  run_sensitivity_analysis(cancer_type = inputs$cancers, gender = inputs$sexes, seed = 80126)
}, 
.options = furrr_options(seed = TRUE)
)

output <- do.call(rbind, results)
arrow::write_parquet(output, "sensitivity_analysis.parquet")
