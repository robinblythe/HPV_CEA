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
# Obtain probabilities from incidence, baseline mortality rates
probs <- list()
groupnames <- list(
  Baseline_mortality = c("Baseline mortality (female)", "Baseline mortality (male)"),
  Incidence = c(
    "Cervical cancer incidence",
    "Vaginal/vulval cancer incidence",
    "Oropharyngeal cancer incidence (female)",
    "Oropharyngeal cancer incidence (male)",
    "Anal cancer incidence (female)",
    "Anal cancer incidence (male)",
    "Penile cancer incidence"
  ),
  Cancer_mortality = c(
    "Cervical cancer mortality",
    "Vaginal/vulval cancer mortality",
    "Oropharyngeal cancer mortality (female)",
    "Oropharyngeal cancer mortality (male)",
    "Anal cancer mortality (female)",
    "Anal cancer mortality (male)",
    "Penile cancer mortality"
  )
)

# Baseline mortality, non-HPV related deaths
baseline_mortality <- baseline_less_cancer |>
  group_by(Group) |>
  nest()
models <- map(baseline_mortality$data, tp_model)

for (i in 1:length(groupnames$Baseline_mortality)) {
  probs[[i]] <- tibble(
    Group = groupnames$Baseline_mortality[[i]],
    Age = seq(10, 84, 1),
    do.call(data.frame, predict(models[[i]], type = "response", se.fit = T)[1:2])
  )
}
remove(baseline_mortality, baseline_less_cancer, models)

# Cancer incidence
cancer_rates <- incidence_rates |>
  group_by(Group, Diagnosis) |>
  nest()
models <- map(cancer_rates$data, tp_model)

for (i in 1:length(groupnames$Incidence)) {
  probs[[i + 2]] <- tibble(
    Group = groupnames$Incidence[[i]],
    Age = seq(10, 84, 1),
    do.call(data.frame, predict(models[[i]], type = "response", se.fit = T)[1:2])
  )
}
remove(cancer_rates, incidence_rates, models)

# Cancer mortality
# Can use KM curves from the data request to get risk of death? No censoring
cancer_deaths <- death_rates |>
  group_by(Group, Diagnosis) |>
  nest()
models <- map(cancer_deaths$data, tp_model)

for (i in 1:length(groupnames$Cancer_mortality)) {
  probs[[i + 9]] <- tibble(
    Group = groupnames$Cancer_mortality[[i]],
    Age = seq(10, 84, 1),
    do.call(data.frame, predict(models[[i]], type = "response", se.fit = T)[1:2])
  )
}
remove(cancer_deaths, death_rates, models)

probs1 <- lapply(probs, setNames, c("Group", "Age", "Fit", "SE.fit"))
probabilities <- do.call(rbind, probs1)
remove(probs, probs1, i, groupnames)


# Probability of cancer attributable to HPV
# https://onlinelibrary.wiley.com/doi/epdf/10.1002/ijc.30716
pct_hpv <- list()
pct_hpv$cervical <- rep(1, iter)
pct_hpv$anal <- rbeta(iter, shape1 = 35000, shape2 = 5000)
pct_hpv$vaginal <- rbeta(iter, shape1 = (8500 + 12000), shape2 = ((34000 - 8500) + (15000 - 12000)))
pct_hpv$penile <- rbeta(iter, shape1 = 13000, shape2 = 26000)
pct_hpv$oropharyngeal <- rbeta(iter, shape1 = 29000, shape2 = (96000 - 29000))

# Probability that HPV vaccination protects against cancer diagnosis (compared to naive)
vaccine_eff <- list()
# Cervical: https://www.nejm.org/doi/full/10.1056/NEJMoa1917338
vaccine_eff$cervical <- rbeta(iter, shape1 = 0.773, shape2 = 7.787)
# Oral: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0068329#s3
vaccine_eff$oral <- rbeta(iter, shape1 = 0.981, shape2 = 7.770)
# Other/genital: source from https://www.sciencedirect.com/science/article/pii/S0755498214004771#bib0165
vaccine_eff$female_genital <- rbeta(iter, shape1 = 0.930, shape2 = 18.548)
vaccine_eff$male_genital <- rbeta(iter, shape1 = 7.524, shape2 = 39.539)


################################################

## Costs

# HPV vaccination costs
# https://www.sciencedirect.com/science/article/pii/S0264410X21003212
cost_vc <- c(Bivalent = 123, Quadrivalent = 320, Nonavalent = 376)

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
rtw$male_genital <- rgamma(iter, shape = 659.678, scale = 0.159)/30.438
rtw$female_genital <- rgamma(iter, shape = 148.905, 1.088)/30.438

# Australian study on oropharyngeal cancer is the best current evidence
# https://doi.org/10.1016/j.ijrobp.2019.09.001
# Transformed with Shinyprior to Gamma dist using IQR
# reported in months
rtw$oropharyngeal <- rgamma(iter, shape = 2.456, scale = 3.069)