# Evaluate the impact of a reduction of 70% of prevalence at 80% population vaccine coverage
options(scipen = 100, digits = 5)
source("0_Functions.R")
load("model_data.Rdata")

library(tidyverse)
library(furrr)
library(progressr)

discount <- 0.03
iter <- 1000
seed <- 80126
age_start <- 16
age_end <- 84
model_year <- 2025

handlers(global = TRUE)
handlers("progress")

combs <- tibble(
  cancers = c(
    "Oropharyngeal", "Cervical", "Vulval", "Vaginal", "Anal",
    "Oropharyngeal", "Penile", "Anal"
  ),
  sexes = c(rep("Female", 5), rep("Male", 3))
)

total_iter <- nrow(combs) * iter
cores <- availableCores()

plan(multisession, workers = cores)
on.exit(plan(sequential), add = TRUE)

# To run full model sensitivity
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

# To run deterministic sensitivity
results_det <- with_progress({
  p <- progressor(steps = total_iter)
  
  pmap_dfr(
    combs,
    function(cancers, sexes) {
      future_map_dfr(
        1:iter,
        function(i) {
          p()
          run_sensitivity_deterministic(
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

arrow::write_parquet(results, "sensitivity_analysis_incremental.parquet")
