options(scipen = 100, digits = 5)
load("model_data.Rdata")

library(rms)
library(tidyverse)
library(foreach)
library(doParallel)

set.seed(888)

discount <- 0.03
iter <- 1000
age_start <- 16
age_end <- 84
model_year <- 2025
cost_vc <- 123

source("./0_Functions.R")


# Prep parallel compute
cores <- detectCores() - 2
cl <- makeCluster(cores)
registerDoParallel(cl)

sims <- list()
cancers = c("Oropharyngeal", "Cervical", "Vulval", "Vaginal", "Anal",
            "Oropharyngeal", "Penile", "Anal")
sexes = c(rep("Female", 5), rep("Male", 3))

sims <- foreach(i = 1:length(cancers), .packages = "dplyr") %dopar% {
  run_model_loop(cancers[i], sexes[i])
}

results <- do.call(rbind, sims) |> ungroup()
saveRDS(results, file = "simulation_results.rds", compress = FALSE)

#############################
results <- read_rds(file = "simulation_results.rds")

# Net benefit of vaccination:
df_nmb <- results |>
  group_by(Iteration) |>
  summarise(NMB = sum(Vaccination_benefit) - cost_vc)

c("Median benefit" = median(df_nmb$NMB),
  "Benefit (lower)" = quantile(df_nmb$NMB, 0.025),
  "Benefit (upper)" = quantile(df_nmb$NMB, 0.975))

# Net benefit - oropharyngeal only
df_cancer <- results |>
  group_by(Iteration, Diagnosis) |>
  summarise(NMB = sum(Vaccination_benefit) - cost_vc) |>
  group_by(Diagnosis) |>
  summarise(NMB_median = median(NMB),
            NMB_lower = quantile(NMB, 0.025),
            NMB_upper = quantile(NMB, 0.975))


# Explaining results
summary <- results |>
  group_by(Diagnosis, Gender, Diagnosis_age) |>
  summarise(
    Lost_income_median = median(Lost_income_cancer),
    Lost_income_low = quantile(Lost_income_cancer, 0.025),
    Lost_income_high = quantile(Lost_income_cancer, 0.975),
    .groups = "keep"
  )

dd <- datadist(summary); options(datadist = "dd")
fit <- ols(Lost_income_median ~ rcs(Diagnosis_age, 5) * Gender, data = subset(summary, Diagnosis == "Oropharyngeal"))
plot(Predict(fit))

p <- summary |>
  mutate(Diagnosis = factor(Diagnosis, levels = c("Anal", "Cervical", "Penile", "Oropharyngeal", "Vaginal", "Vulval"))) |>
  ggplot(aes(
    x = Diagnosis_age,
    y = -Lost_income_median,
    ymin = Lost_income_low,
    ymax = Lost_income_high
  ))

p +
  geom_smooth() +
  facet_wrap(vars(Gender, Diagnosis), nrow = 2, ncol = 5) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Lost income per cancer survivor (2024 SGD)", 
                     label = scales::comma)

ggsave(file = "income_losses_diagnosis.png", height = 8, width = 8)

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