options(scipen = 100, digits = 3)
library(tidyverse)
library(rms)
library(ggridges)
library(viridis)
results <- arrow::read_parquet(file = "simulation_results.parquet")
cost_vc <- 123

# Net benefit of vaccination by gender:
df_nmb <- results |>
  group_by(Iteration, Gender) |>
  summarise(NMB = sum(Vaccination_benefit) - cost_vc) |>
  mutate(Diagnosis = "All cancers")

# Net benefit - cancer-specific
# Note: "All cancers" NMB is significantly greater than the other 3 put together;
## This is because the cost of vaccination should only be applied once
## i.e. it's (NMB_Penile + NMB_Oropharyngeal + NMB_Anal) - cost_vc, not the sum of (each one - cost_vc)
df_cancer <- results |>
  group_by(Iteration, Gender, Diagnosis) |>
  summarise(NMB = sum(Vaccination_benefit) - cost_vc) |>
  bind_rows(df_nmb) |>
  arrange(Iteration, Diagnosis)

df_cancer_results <- df_cancer |>
  group_by(Gender, Diagnosis) |>
  summarise(NMB_median = median(NMB),
            NMB_low = quantile(NMB, 0.025),
            NMB_high = quantile(NMB, 0.975))

df_final_results <- df_nmb |>
  group_by(Gender) |>
  summarise(
    NMB_median = median(NMB),
    NMB_low = quantile(NMB, 0.025),
    NMB_high = quantile(NMB, 0.975)
  )

remove(df_nmb)

p_cancer <- df_cancer |>
  rename(Sex = Gender) |>
  ggplot(aes(x = NMB, y = Diagnosis, fill = Diagnosis))

p_cancer +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  geom_vline(xintercept = 0) +
  scale_fill_viridis_d(guide = "none") +
  theme_bw() +
  facet_wrap(vars(Sex), scales = "free") +
  xlab("Net economic benefit of HPV vaccination per individual (2025 SGD)")

ggsave(filename = "NMB_by_diagnosis.jpg", height = 8, width = 12)

# Explaining results for cancer survivors
summary <- results |>
  group_by(Diagnosis, Gender, Diagnosis_age) |>
  summarise(
    Lost_income_median = median(Lost_income_cancer),
    Lost_income_low = quantile(Lost_income_cancer, 0.025),
    Lost_income_high = quantile(Lost_income_cancer, 0.975),
    .groups = "keep"
  )

# Some spot results:
summary_table <- summary |>
  filter(Diagnosis_age %in% c(33, 50, 67)) |>
  mutate(
    "Median lost income" = scales::dollar(Lost_income_median),
    "95% uncertainty interval" = paste0(
    "[", scales::dollar(round(Lost_income_low)), " to ", scales::dollar(round(Lost_income_high)), "]")
    ) |>
  select(-c(Lost_income_low, Lost_income_high, Lost_income_median)) |>
  rename(Sex = Gender, `Cancer diagnosis` = Diagnosis, Age = Diagnosis_age)

write.csv(summary_table, file = "summary_results.csv")

dd <- datadist(summary)
options(datadist = "dd")
fit <- ols(Lost_income_median ~ rcs(Diagnosis_age, 5) * Gender, data = subset(summary, Diagnosis == "Oropharyngeal"))
plot(Predict(fit))

p <- summary |>
  mutate(Diagnosis = factor(Diagnosis, levels = c("Anal", "Cervical", "Penile", "Oropharyngeal", "Vaginal", "Vulval"))) |>
  ggplot(aes(
    x = Diagnosis_age,
    y = -Lost_income_median,
    ymin = -Lost_income_low,
    ymax = -Lost_income_high
  ))

p +
  geom_smooth() +
  geom_ribbon(fill = "grey", alpha = 0.3) +
  facet_wrap(vars(Gender, Diagnosis), nrow = 2, ncol = 5, axes = "all_x") +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(
    name = "Productivity losses per cancer diagnosis (2025 SGD)",
    label = scales::comma
  ) +
  scale_x_continuous(
    name = "Age at cancer diagnosis",
    limits = c(16, 84), breaks = seq(16, 84, 17)
  )

ggsave(filename = "income_losses_diagnosis.jpg", height = 8, width = 10)

models <- summary |>
  group_by(Diagnosis, Gender) |>
  nest() |>
  mutate(model = map(data, function(df) lm(Lost_income_median ~ Diagnosis_age, data = df)))

results_summary <- models |>
  mutate(Annual_loss = model[[row_number()]]$coefficients[[2]]) |>
  select(Diagnosis, Gender, Annual_loss) |>
  rename(Sex = Gender)

remove(models)

## Visualise prevalence
p <- vroom::vroom("./Data/NRDO extract/n_cases_predicted.csv") |>
  mutate(Sex = ifelse(gender == "F", "Female", "Male"),
         avg_cases = n/29) |>
  ggplot(aes(x = age_at_diagnosis, y = avg_cases, colour = Sex))

p +
  geom_smooth(method = "gam", se = F, linewidth = 1.5) +
  facet_wrap(vars(cancer_type), scales = "free_y") +
  labs(x = "Age at diagnosis",
       y = "Cases per year (average)") +
  scale_colour_manual(values = c("Female" = "#D55E00", "Male" = "#0072B2")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")

ggsave(file = "annual_prevalence.jpg", height = 6, width = 8)  
