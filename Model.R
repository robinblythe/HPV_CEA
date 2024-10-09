options(scipen = 100, digits = 5)
library(heemod)

source("./Parameters.R")

# heemod requires defining transition matrices, states (costs, utilities)
# Two separate cohorts: Male and female
# Three HPV-associated cancers in men: oropharyngeal, penile, and anal
# Four HPV-associated cancers in women: oropharyngeal, cervical, vaginal/vulval, and anal
# Three model states: Healthy, Diagnosed, Dead

# Transition probability for cancer: vaccine efficacy * baseline probability of cancer
# e.g., efficacy of 96% for an annual incidence of 0.5% = (1-0.96)*(0.005) = 0.0002

# Mortality rates: try Beta regression if doing Markov, or Gamma regression/AFT if doing DES
# Get the regression first, with/without HPV vaccination if possible
# Then build the model out of the regression, with age + vaccination as predictors

# States
state_healthy <- define_state(
  cost = 0,
  utility = 1
)

state_dead <- define_state(
  cost = 0,
  utility = 0
)

# Remission can be associated with reduced quality of life and increased costs (e.g. from screening)
# https://doi.org/10.1186/s12885-019-5614-4


# Can now obtain samples for probability of contracting cervical cancer each year using:
rnorm(1, mean = baseline_mortality_female$fit[1], sd = baseline_mortality_female$se.fit[1])
# This can be integrated into the heemod package by using rnorm(1, preds$fit[model_time], preds$se.fit[model_time])
