options(scipen = 100, digits = 5)

library(heemod)
library(tidyverse)

# heemod requires defining transition matrices, states (costs, utilities)
# Two separate cohorts: Male and female
# Four HPV-associated cancers: oropharyngeal, cervical, penile, and anal
# Female model: oropharyngeal, cervical, anal, vulval?, vaginal?
# Male model: oropharyngeal, penile, anal

# Two options for model states:
# 1) either have a cancer state where people stay until they die
# 2) or have a cancer - remission state where people can cycle between
# States: healthy, dead, cancer 1, cancer 2, cancer 3, remission 1, remission 2, remission 3

# Need the incremental cost of HPV treatment compared to standard
# Baseline mortality will include HPV mortality, so will be double counting (limiting vaccine effectiveness)

# Cohort needs to be sufficient size to get individual cases

# If HPV vaccination is widespread then we may also be able to avoid pap smears - cost savings?

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
