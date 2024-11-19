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
cost_vc <- 123

source("./Functions.R")


# Prep parallel compute
cores <- detectCores() - 2
cl <- makeCluster(cores)
registerDoParallel(cl)

sims <- list()
cancers <- c("Oropharyngeal", "Oropharyngeal", "Reproductive", "Anal/genital")
sexes <- c("Female", "Male", "Female", "Male")

sims <- foreach(i = 1:4, .packages = "dplyr") %dopar% {
  run_model_loop(cancers[i], sexes[i])
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
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(breaks = seq(0, 800000, 100000),
                     name = "Lost income per cancer survivor (2024 SGD)", 
                     label = scales::comma)

ggsave(file = "prelim_findings.png", height = 8, width = 8)


models <- summary |>
  group_by(Diagnosis, Gender) |>
  nest() |>
  mutate(model = map(data, function(df) lm(Lost_income_median ~ Diagnosis_age, data = df)))
