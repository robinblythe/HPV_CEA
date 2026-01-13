options(scipen = 100, digits = 3)

library(dplyr)

results <- arrow::read_parquet(file = "sensitivity_analysis.parquet")
discount = 0.03
model_year = 2025
cost_vc <- 123*(1+discount)^(model_year - 2021)

df_nmb <- results |>
  group_by(Iteration, Gender) |>
  summarise(NMB = sum(EV_cancer_no_vc),
            NMB_vc = sum(Vaccination_benefit) - cost_vc,
            NMB_no_vc_herd = sum(Vaccination_benefit_herd_no_vc)
            ) |>
  group_by(Gender) |>
  summarise(
    NMB_median = median(NMB),
    NMB_low = quantile(NMB, 0.025),
    NMB_high = quantile(NMB, 0.975),
    NMB_vc_median = median(NMB_vc),
    NMB_vc_low = quantile(NMB_vc, 0.025),
    NMB_vc_high = quantile(NMB_vc, 0.975),
    NMB_herd_median = median(NMB_no_vc_herd),
    NMB_herd_low = quantile(NMB_no_vc_herd, 0.025),
    NMB_herd_high = quantile(NMB_no_vc_herd, 0.975)
  )

## Impact of herd immunity
girls <- 20649
boys <- 21432
herd_pr <- 0.8
pop_vaccinated <- (girls + boys) * herd_pr

total_cost <- round(pop_vaccinated * cost_vc)
total_benefit <- girls * herd_pr * df_nmb$NMB_vc_median[df_nmb$Gender == "Female"] + # vaccinated girls
  boys * herd_pr * df_nmb$NMB_vc_median[df_nmb$Gender == "Male"] + # vaccinated boys
  girls * (1 - herd_pr) * df_nmb$NMB_herd