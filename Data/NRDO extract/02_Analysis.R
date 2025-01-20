options(scipen = 100, digits = 5)
df_surv <- readRDS("cleaned_data.rds")

library(tidyverse)
library(survival)
library(rms)

### Obtain survival probabilities
df_surv$survival_time[df_surv$survival_time == 0.0] <- 0.1

# Define datadist
dd <- datadist(df_surv)
options(datadist = "dd")

# Standard Cox regression
fit <- coxph(Surv(survival_time, death_hpv) ~ rcs(age_at_diagnosis, 4) + gender +
             strata(cancer_type), 
           data = df_surv, x = T, y = T)

# Create hypothetical datasets of individuals
# 8 groups: Male and female oropharyngeal; male and female anal; female cervical, vaginal, vulval; male penile
df_predicted <- expand.grid(
  age_at_diagnosis = seq(10, 84, 1),
  gender = c("M", "F"),
  cancer_type = unique(df_surv$cancer_type),
  survival_time = seq(1, 30, 1),
  death_hpv = 0
)
df_predicted <- df_predicted[!(df_predicted$gender == "M" & df_predicted$cancer_type %in% c("Cervix", "Vagina", "Vulva")),]
df_predicted <- df_predicted[!(df_predicted$gender == "F" & df_predicted$cancer_type == "Penis"),]

preds_coxph <- predict(fit, newdata = df_predicted, type = "survival", se.fit = TRUE)
df_predicted$p_survival <- preds_coxph$fit
df_predicted$se_survival <- preds_coxph$se.fit

model_stats <- summary(fit)
saveRDS(model_stats, file = "Z:/output/coxph_model_stats.rds")
write_csv(df_predicted, file = "Z:/output/predictions_coxph.csv")

### Obtain predicted incidence rates using splines
df_diagnosis <- df_predicted |>
  select(age_at_diagnosis, gender, cancer_type) |>
  unique()

df_count <- df_surv |>
  group_by(cancer_type, gender) |>
  count(floor(age_at_diagnosis)) |>
  rename(age_at_diagnosis = "floor(age_at_diagnosis)") |>
  full_join(df_diagnosis) |>
  filter(age_at_diagnosis > 9 & age_at_diagnosis < 85) |>
  arrange(cancer_type, gender, age_at_diagnosis)

df_count$n[is.na(df_count$n)] <- 0

df_count <- df_count |>
  group_by(cancer_type, gender) |>
  mutate(n_predicted = fitted(smooth.spline(x = age_at_diagnosis, y = n))) |>
  ungroup()

write_csv(df_count, file = "Z:/output/n_cases_predicted.csv")
