### Model parameters
source("./Read_data.R")

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




# Currently unsure whether to add to model
# Probabilities for routine screening from age 10 to 84
# https://doi.org/10.1002/ijgo.12126
# Pap smear every 3 years from 25 to 65
# pap <- tibble(
#   Group = "Pap smear (female)",
#   Age = seq(10, 84, 1),
#   Fit = case_when(
#     Age >= 25 & Age < 65 ~ (1/3),
#     .default = 0
#   ),
#   SE.fit = 0
# )
# probabilities <- bind_rows(probabilities, pap)
# remove(pap)

# Probability that pap smear requires follow-up colposcopy
# https://doi.org/10.47102/annals-acadmedsg.2023329
# pr_positive_pap <- c(Events = 292, Non_events = (10967 - 292))







# Probability of cancer attributable to HPV
# https://onlinelibrary.wiley.com/doi/epdf/10.1002/ijc.30716
pct_hpv <- list()
pct_hpv$cervical <- 1
pct_hpv$anal <- c(
  Events = 35000,
  Non_events = 5000
)
pct_hpv$vaginal <- c(
  Events = (8500 + 12000),
  Non_events = ((34000 - 8500) + (15000 - 12000))
)
pct_hpv$penile <- c(
  Events = 13000,
  Non_events = 26000
)
pct_hpv$oropharyngeal <- c(
  Events = 29000,
  Non_events = (96000 - 29000)
)


# Probability that HPV vaccination protects against cancer diagnosis
vaccine_eff <- list()
vaccine_eff$cervical

# Current rate of vaccine uptake
# https://journals.sagepub.com/doi/full/10.1177/2158244014554961

pr_vaccinated_female <- c(Events = 28, Non_events = (208 - 26))

################################################

## Costs
discount <- 0.03

# HPV vaccination costs
# https://www.sciencedirect.com/science/article/pii/S0264410X21003212
cost_vc <- 376

# Cost savings from pap smears averted (if done)
# costs$pap <- 43 * (1 + discount)^(2024 - 2011)
# costs$colposcopy <- 109.73 * (1 + discount)^(2024 - 2011)
# Intending to calculate cancer cost savings?


# Workforce participation due to cancers
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
# Assign as a wage decrement to median income
# Labour force participation (baseline) * monthly income * -leave duration (months)

# Female pelvic cancer RTW ~ 3.2 months
# https://www.mdpi.com/2072-6694/14/9/2330
# Used ShinyPrior to estimate Gamma dist based on 3.2 months median (range 0 - 33.1)
# Roughly equivalent to Gamma dist with mean = 3.9 and SE = 3/sqrt(101)
# rgamma(iter, shape = 114.916, scale = 0.028)

# Oropharyngeal cancer RTW
# https://www.sciencedirect.com/science/article/pii/S0360301619337514
# ShinyPrior: Mean 6.8 months, SE = 5.8/sqrt(58)
# rgamma(iter, shape = 79.724, rate = 0.085)

# Colorectal cancer RTW
# https://academic.oup.com/bjs/article/107/1/140/6121017
# Used ShinyPrior to estimate Gamma dist based on (95% CI 379, 467) days, n = 317
# This is roughly equivalent to Gamma(93890.743,0.005)
# rgamma(iter, shape = 93890.743, scale = 0.005)/(30.437)
