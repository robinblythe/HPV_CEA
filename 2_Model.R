options(scipen = 100, digits = 5)
load("model_data.Rdata")

### Run models
library(rms)
library(tidyverse)
library(foreach)
library(doParallel)

set.seed(888)

discount <- 0.03
iter <- 10000
age_start <- 16
age_end <- 84
model_year <- 2025

source("./0_Functions.R")


# Prep parallel compute
cores <- detectCores() - 2
cl <- makeCluster(cores)
registerDoParallel(cl)

sims <- list()
cancers <- c(
  "Oropharyngeal", "Cervical", "Vulval", "Vaginal", "Anal",
  "Oropharyngeal", "Penile", "Anal"
)
sexes <- c(rep("Female", 5), rep("Male", 3))

sims <- foreach(i = 1:length(cancers), .packages = c("dplyr", "tidyr")) %dopar% {
  run_model_loop(cancers[i], sexes[i])
}

results <- do.call(rbind, sims) |> ungroup()
arrow::write_parquet(results, "simulation_results.parquet")
