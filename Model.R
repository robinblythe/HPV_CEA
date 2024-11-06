options(scipen = 100, digits = 5)
load("model_data.Rdata")

library(rms)
library(tidyverse)
library(foreach)
library(doParallel)

set.seed(888)

discount <- 0.03
iter <- 1000
age_start <- 10
age_end <- 84
model_year <- 2019

source("./Functions.R")

run_model_loop <- function(cancer_type, gender) {
  sims <- list()
  for (i in 1:iter) {
    sim_incomes <- median_income |>
      filter(Sex == gender) |>
      mutate(
        Participation_rate =
          if (gender == "Female" & cancer_type == "reproductive") {
            get_odds(1 / (1 - rbeta(1, 671, 648)) / (1 / (1 - rbeta(1, 816, 506))), Participation_rate)
          } else if (cancer_type == "oropharyngeal") {
            get_odds(1 / (1 - rbeta(1, 75, 62)) / (1 / (1 - rbeta(1, 116, 26))), Participation_rate)
          } else {
            get_odds(1 / (1 - rbeta(1, 13480, 6886)) / (1 / (1 - rbeta(1, 133588, 24015))), Participation_rate)
          },
        Weighted_income_cancer = Participation_rate * Monthly_income * 12
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

# Prep parallel compute
cores = detectCores() - 2
cl <- makeCluster(cores)
registerDoParallel(cl)

sims <- list()
cancers <- c("oropharyngeal", "oropharyngeal", "reproductive", "other HPV cancers")
genders <- c("Female", "Male", "Female", "Male")

sims <- foreach(i = 1:4, .packages = "dplyr") %dopar% {
  run_model_loop(cancers[i], genders[i])
}

results <- do.call(rbind, sims)


# Viz
summary <- results |>
  group_by(Diagnosis, Gender, Diagnosis_age) |>
  summarise(
    Lost_income_median = median(Lost_income),
    Lost_income_low = quantile(Lost_income, 0.025),
    Lost_income_high = quantile(Lost_income, 0.975)
  ) 

p <- summary |>
  ggplot(aes(
    x = Diagnosis_age,
    y = Lost_income_median,
    ymin = Lost_income_low,
    ymax = Lost_income_high
  ))

p +
  geom_line() +
  geom_ribbon(fill = "grey", alpha = 0.5) +
  facet_wrap(vars(Gender, Diagnosis)) +
  theme_bw()

models <- summary |>
  group_by(Diagnosis, Gender) |>
  nest() |>
  mutate(model = map(data, function(df) lm(Lost_income_median ~ Diagnosis_age, data = df)))
