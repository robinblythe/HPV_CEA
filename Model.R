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
