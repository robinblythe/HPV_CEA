### Model parameters
load("model_data.Rdata")

library(rms)
library(tidyverse)

#########################################
## Income estimation
# Estimate median wage across the workforce to generate senior incomes:
# Spar term chosen by visual analysis to create a smoothed quadratic curve
fit <- with(na.omit(incomes), smooth.spline(Age, Total, spar = 0.94))
income <- do.call(cbind, predict(fit, x = seq(15, 84, 1))) |>
  as_tibble() |>
  rename(
    Age = x,
    Total = y
  ) |>
  mutate(Monthly_income = ifelse(Total < 0, 0, Total)) |>
  select(-Total)

# Fit check
# incomes |> ggplot(aes(x = Age, y = Total)) +
#   geom_line() +
#   geom_line(data = income, aes(x = Age, y = Monthly_income))

# Merge employment with incomes to get an adjusted total income (annual)
median_income <- employment |>
  left_join(income, by = join_by(Age)) |>
  mutate(Weighted_income_annual = Participation_rate * Monthly_income * 12) %>%
  replace(is.na(.), 0) |>
  arrange(Sex, Age)

remove(income, incomes, employment, fit)


##########################################
## Time-varying transition probabilities

# Number of patients entering model
cases <- list()
cases$female <- list()
cases$male <- list()

cases$female$cervical <- lapply(incidence$female$cervical$N, function(x) rpois(iter, x))
cases$female$vaginal <- lapply(incidence$female$vaginal$N, function(x) rpois(iter, x))
cases$female$anal <- lapply(incidence$female$anal$N, function(x) rpois(iter, x))
cases$female$oropharyngeal <- lapply(incidence$female$oropharyngeal$N, function(x) rpois(iter, x))
cases$male$vaginal <- lapply(incidence$male$penile$N, function(x) rpois(iter, x))
cases$male$vaginal <- lapply(incidence$male$anal$N, function(x) rpois(iter, x))
cases$male$vaginal <- lapply(incidence$male$oropharyngeal$N, function(x) rpois(iter, x))



# Probability of cancer attributable to HPV
# https://onlinelibrary.wiley.com/doi/epdf/10.1002/ijc.30716
pct_hpv <- list()
pct_hpv$cervical <- 1
pct_hpv$anal <- function() rbeta(1, shape1 = 35000, shape2 = 5000)
pct_hpv$vaginal <- function() rbeta(1, shape1 = (8500 + 12000), shape2 = ((34000 - 8500) + (15000 - 12000)))
pct_hpv$penile <- function() rbeta(1, shape1 = 13000, shape2 = 26000)
pct_hpv$oropharyngeal <- function() rbeta(1, shape1 = 29000, shape2 = (96000 - 29000))

# Probability that HPV vaccination protects against cancer diagnosis (compared to naive)
vaccine_eff <- list()
# Cervical BIVALENT: https://www.nejm.org/doi/full/10.1056/NEJMoa1917338
vaccine_eff$cervical <- function() rbeta(1, shape1 = 0.773, shape2 = 7.787)
# Oral BIVALENT: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0068329#s3
vaccine_eff$oropharyngeal <- function() rbeta(1, shape1 = 0.981, shape2 = 7.770)
# Other/genital BIVALENT: https://www.sciencedirect.com/science/article/pii/S0755498214004771#bib0165
vaccine_eff$vaginal <- vaccine_eff$anal_female <- function() rbeta(1, shape1 = 0.930, shape2 = 18.548)
vaccine_eff$penile <- vaccine_eff$anal_male <- function() rbeta(1, shape1 = 7.524, shape2 = 39.539)


################################################

## Costs

# HPV vaccination costs
# https://www.sciencedirect.com/science/article/pii/S0264410X21003212
cost_vc <- c(None = 0, Bivalent = 123, Quadrivalent = 320, Nonavalent = 376)

# Cost savings from pap smears averted (if done)
# costs$pap <- 43 * (1 + discount)^(2024 - 2011)
# costs$colposcopy <- 109.73 * (1 + discount)^(2024 - 2011)
# Intending to calculate cancer cost savings?


# Workforce participation due to cancers
# https://jamanetwork.com/journals/jama/fullarticle/183387
median_income <- median_income |>
  rowwise() |>
  mutate(
    Participation_rate_female_repro = ifelse(Sex == "Female", Participation_rate / 1.37 / 1.28, NA_real_),
    Participation_rate_oro = Participation_rate / 1.37 / 2.47,
    Participation_rate_other_cancer = Participation_rate / 1.37,
    Weighted_income_female_repro = Participation_rate_female_repro * Monthly_income * 12,
    Weighted_income_oro = Participation_rate_oro * Monthly_income * 12,
    Weighted_income_other_cancer = Participation_rate_other_cancer * Monthly_income * 12
  )

# Return to work following diagnosis
# Assign as a wage decrement to median income (cancer)
# Labour force participation (baseline) * monthly income * -leave duration (months)
# https://doi.org/10.1002/pon.1820
# transformation with https://aushsi.shinyapps.io/ShinyPrior/ to Gamma dists
# reported in days so divide by 30.438

rtw <- list()
rtw$male_genital <- function() rgamma(1, shape = 659.678, scale = 0.159) / 30.438
rtw$female_genital <- function() rgamma(1, shape = 148.905, 1.088) / 30.438

# Australian study on oropharyngeal cancer is the best current evidence
# https://doi.org/10.1016/j.ijrobp.2019.09.001
# Transformed with Shinyprior to Gamma dist using IQR
# reported in months
rtw$oropharyngeal <- function() rgamma(1, shape = 2.456, scale = 3.069)
