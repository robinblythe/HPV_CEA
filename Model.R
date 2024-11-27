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
saveRDS(results, file = "simulation_results.rds", compress = FALSE)

#############################
results <- read_rds(file = "simulation_results.rds")

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

results_summary <- models |>
  mutate(Annual_loss = model[[row_number()]]$coefficients[[2]]) |>
  select(Diagnosis, Gender, Annual_loss)

p_check <- oro |>
  ggplot(aes(x = Diagnosis_age, colour = Gender))

p_check +
  geom_line(aes(y = Income_healthy, linetype = "Income healthy")) +
  geom_line(aes(y = Income_cancer, linetype = "Income cancer")) +
  scale_linetype_manual(values = c("Income healthy" = "solid", "Income cancer" = "dashed")) +
  theme_bw()

p_check +
  geom_line(aes(y = Participation_rate_cancer_by_age)) +
  theme_bw()

# Interesting result here - women have higher net income losses than men
# Reason being - in Singapore, women have greater labour force participation than men in the early career stage
# Therefore they lose out more with an early diagnosis