# Time-varying transition probabilities
# Manipulate the HPV diagnosis set to represent an annual probability
source("./Read_data.R")
library(rms)

# Obtaining probabilities with SE
probs <- list()
# Baseline mortality
baseline_mortality <- baseline_less_cancer |>
  group_by(Group) |>
  nest() 
models <- map(baseline_mortality$data, tp_model)
probs$baseline_mortality_female <- do.call(cbind, predict(models[[1]], type = "response", se.fit = T)[1:2])
probs$baseline_mortality_male <- do.call(cbind, predict(models[[2]], type = "response", se.fit = T)[1:2])
remove(baseline_mortality, baseline_less_cancer, models)

# Cancer incidence
cancer_rates <- incidence_rates |>
  group_by(Group, Diagnosis) |>
  nest()
models <- map(cancer_rates$data, tp_model)

probs$i_cerv_female <- do.call(cbind, predict(models[[1]], type = "response", se.fit = T)[1:2])
probs$i_vaginal_female <- do.call(cbind, predict(models[[2]], type = "response", se.fit = T)[1:2])
probs$i_oro_female <- do.call(cbind, predict(models[[3]], type = "response", se.fit = T)[1:2])
probs$i_oro_male <- do.call(cbind, predict(models[[4]], type = "response", se.fit = T)[1:2])
probs$i_anal_female <- do.call(cbind, predict(models[[5]], type = "response", se.fit = T)[1:2])
probs$i_anal_male <- do.call(cbind, predict(models[[6]], type = "response", se.fit = T)[1:2])
probs$i_penile_male <- do.call(cbind, predict(models[[7]], type = "response", se.fit = T)[1:2])

remove(cancer_rates, incidence_rates, models)

# Cancer mortality
cancer_deaths <- death_rates |>
  group_by(Group, Diagnosis) |>
  nest()
models <- map(cancer_deaths$data, tp_model)

probs$mort_cerv_female <- do.call(cbind, predict(models[[1]], type = "response", se.fit = T)[1:2])
probs$mort_vaginal_female <- do.call(cbind, predict(models[[2]], type = "response", se.fit = T)[1:2])
probs$mort_oro_female <- do.call(cbind, predict(models[[3]], type = "response", se.fit = T)[1:2])
probs$mort_oro_male <- do.call(cbind, predict(models[[4]], type = "response", se.fit = T)[1:2])
probs$mort_anal_female <- do.call(cbind, predict(models[[5]], type = "response", se.fit = T)[1:2])
probs$mort_anal_male <- do.call(cbind, predict(models[[6]], type = "response", se.fit = T)[1:2])
probs$mort_penile_male <- do.call(cbind, predict(models[[7]], type = "response", se.fit = T)[1:2])

remove(cancer_deaths, death_rates, models)


# Health utilities