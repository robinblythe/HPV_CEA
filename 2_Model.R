options(scipen = 100, digits = 5)
load("model_data.Rdata")

### Run models
library(rms)
library(tidyverse)
library(furrr)

# Model starting parameters
discount <- 0.03
iter <- 10000
age_start <- 16
age_end <- 84
model_year <- 2025

# Model functions
source("./0_Functions.R")

# Set model inputs
combs <- tibble(
  cancers = c(
    "Oropharyngeal", "Cervical", "Vulval", "Vaginal", "Anal",
    "Oropharyngeal", "Penile", "Anal"
  ),
  sexes = c(rep("Female", 5), rep("Male", 3))
)

plan(multisession, workers = availableCores() - 2)
set.seed(80126)

results <- future_map(1:nrow(combs), function(i){
  inputs = combs[i,]
  run_model_loop(cancer_type = inputs$cancers, gender = inputs$sexes)
  }, 
  .options = furrr_options(seed = TRUE)
  )

output <- do.call(rbind, results)
arrow::write_parquet(output, "simulation_results.parquet")
