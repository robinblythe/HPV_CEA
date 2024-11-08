### Model parameters
load("model_data.Rdata")

library(rms)
library(tidyverse)

set.seed(888)

# Workforce participation due to cancers
# https://jamanetwork.com/journals/jama/fullarticle/183387
# Using odds from Figure 2 (cancer employed/unemployed / no cancer employed/unemployed)
sim_incomes <- list()
for (i in 1:iter) {
  sim_incomes[[i]] <- median_income |>
    mutate(
      Participation_rate_female_repro = ifelse(Sex == "Female",
        get_odds(1 / (1 - rbeta(1, 671, 648)) / (1 / (1 - rbeta(1, 816, 506))), Participation_rate),
        NA_real_
      ),
      Participation_rate_oro = get_odds(1 / (1 - rbeta(1, 75, 62)) / (1 / (1 - rbeta(1, 116, 26))), Participation_rate),
      Participation_rate_other_cancer = get_odds(1 / (1 - rbeta(1, 13480, 6886)) / (1 / (1 - rbeta(1, 133588, 24015))), Participation_rate),
      Weighted_annual_female_repro = Participation_rate_female_repro * Monthly_income * 12,
      Weighted_annual_oro = Participation_rate_oro * Monthly_income * 12,
      Weighted_annual_other = Participation_rate_other_cancer * Monthly_income * 12,
      iteration = i
    )
}
sim_incomes <- do.call(rbind, sim_incomes)

################################################
# Summarise as expected lifetime income for each age
# lifetime_income <- list()
# lifetime_income$male <- list()
# lifetime_income$female <- list()
#
# Healthy - no disability from cancer
lifetime_income$male$healthy <- lapply(
  seq(age_start, age_end, 1),
  function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_income_annual", gender = "Male", i = iteration)
)

lifetime_income$female$healthy <- lapply(
  seq(age_start, age_end, 1),
  function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_income_annual", gender = "Female", i = iteration)
)

# Including cancer disability
lifetime_income$male$oro <- lapply(
  seq(age_start, age_end, 1),
  function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_annual_oro", gender = "Male", i = iteration)
)

lifetime_income$male$other <- lapply(
  seq(age_start, age_end, 1),
  function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_annual_other", gender = "Male", i = iteration)
)

lifetime_income$female$oro <- lapply(
  seq(age_start, age_end, 1),
  function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_annual_oro", gender = "Female", i = iteration)
)

lifetime_income$female$repro <- lapply(
  seq(age_start, age_end, 1),
  function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_annual_female_repro", gender = "Female", i = iteration)
)

lifetime_income$female$other <- lapply(
  seq(age_start, age_end, 1),
  function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_annual_other", gender = "Female", i = iteration)
)

saveRDS(lifetime_income, file = "./income_sims.RDS")
lifetime_income <- readRDS("./income_sims.RDS")

# Return to work following diagnosis
# Assign as a wage decrement to median income (cancer)
# Labour force participation (baseline) * monthly income * -leave duration (months)
# https://doi.org/10.1002/pon.1820
# transformation with https://aushsi.shinyapps.io/ShinyPrior/ to Gamma dists
# reported in days so divide by 30.438

rtw <- list()
rtw$male_genital <- rgamma(iter, shape = 659.678, scale = 0.159) / 30.438
rtw$female_genital <- rgamma(iter, shape = 148.905, 1.088) / 30.438

# Australian study on oropharyngeal cancer is the best current evidence
# https://doi.org/10.1016/j.ijrobp.2019.09.001
# Transformed with Shinyprior to Gamma dist using IQR
# reported in months
rtw$oropharyngeal <- rgamma(iter, shape = 2.456, scale = 3.069)

###############################
# Incidence and mortality data
probabilities <- bind_rows(
  do.call(rbind, incidence$female),
  do.call(rbind, incidence$male)
) |>
  rename(N_cases = N) |>
  left_join(
    bind_rows(
      do.call(rbind, mortality$female),
      do.call(rbind, mortality$male)
    ),
    by = join_by(Age, Group, Diagnosis)
  ) |>
  rename(N_deaths = N) |>
  group_by(Group, Diagnosis) |>
  mutate(
    Cumulative_cases = cumsum(N_cases),
    Pr_mortality = list(rbeta(iter, N_deaths, (Cumulative_cases - N_deaths)))
  ) |>
  ungroup()



# Probability of cancer attributable to HPV
# https://onlinelibrary.wiley.com/doi/epdf/10.1002/ijc.30716
pct_hpv <- list()
pct_hpv$cervical <- 1
pct_hpv$anal <- rbeta(iter, shape1 = 35000, shape2 = 5000)
pct_hpv$vaginal <- rbeta(iter, shape1 = (8500 + 12000), shape2 = ((34000 - 8500) + (15000 - 12000)))
pct_hpv$penile <- rbeta(iter, shape1 = 13000, shape2 = 26000)
pct_hpv$oropharyngeal <- rbeta(iter, shape1 = 29000, shape2 = (96000 - 29000))

# Probability that HPV vaccination protects against cancer diagnosis (compared to naive)
vaccine_eff <- list()
# Cervical BIVALENT: https://www.nejm.org/doi/full/10.1056/NEJMoa1917338
vaccine_eff$cervical <- rbeta(iter, shape1 = 0.773, shape2 = 7.787)
# Oral BIVALENT: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0068329#s3
vaccine_eff$oropharyngeal <- rbeta(iter, shape1 = 0.981, shape2 = 7.770)
# Other/genital BIVALENT: https://www.sciencedirect.com/science/article/pii/S0755498214004771
vaccine_eff$vaginal <- vaccine_eff$anal_female <- rbeta(iter, shape1 = 0.930, shape2 = 18.548)
vaccine_eff$penile <- vaccine_eff$anal_male <- rbeta(iter, shape1 = 7.524, shape2 = 39.539)


## Costs

# HPV vaccination costs
# https://www.sciencedirect.com/science/article/pii/S0264410X21003212
cost_vc <- 123

# Cost savings from pap smears averted (if done)
# costs$pap <- 43 * (1 + discount)^(2024 - 2011)
# costs$colposcopy <- 109.73 * (1 + discount)^(2024 - 2011)
# Intending to calculate cancer cost savings?
