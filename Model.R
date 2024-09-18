options(scipen = 100, digits = 5)

library(heemod)

# heemod requires defining transition matrices, states (costs, utilities) 
# Two separate cohorts: Male and female
# Four HPV-associated cancers: oropharyngeal, cervical, penile, and anal
# Female model: oropharyngeal, cervical, anal, vulval?, vaginal?
# Male model: oropharyngeal, penile, anal
# States: healthy, dead, cancer 1, cancer 2, cancer 3, remission 1, remission 2, remission 3

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

