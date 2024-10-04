### Model parameters
source("./Read_data.R")

##########################################
## Time-varying transition probabilities
# Obtain probabilities from incidence, baseline mortality rates
probs <- list()
groupnames <- list(
  Baseline_mortality = c("Baseline mortality (female)", "Baseline mortality (male)"),
  Incidence = c("Cervical cancer incidence",
                "Vaginal/vulval cancer incidence",
                "Oropharyngeal cancer incidence (female)",
                "Oropharyngeal cancer incidence (male)",
                "Anal cancer incidence (female)",
                "Anal cancer incidence (male)",
                "Penile cancer incidence"),
  Cancer_mortality = c("Cervical cancer mortality",
                       "Vaginal/vulval cancer mortality",
                       "Oropharyngeal cancer mortality (female)",
                       "Oropharyngeal cancer mortality (male)",
                       "Anal cancer mortality (female)",
                       "Anal cancer mortality (male)",
                       "Penile cancer mortality")
)

# Baseline mortality, non-HPV related deaths
baseline_mortality <- baseline_less_cancer |>
  group_by(Group) |>
  nest() 
models <- map(baseline_mortality$data, tp_model)

for (i in 1:length(groupnames$Baseline_mortality)){
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
  probs[[i+2]] <- tibble(
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
  probs[[i+9]] <- tibble(
    Group = groupnames$Cancer_mortality[[i]],
    Age = seq(10, 84, 1),
    do.call(data.frame, predict(models[[i]], type = "response", se.fit = T)[1:2])
  )
}
remove(cancer_deaths, death_rates, models)

probs1 <- lapply(probs, setNames, c("Group", "Age", "Fit", "SE.fit"))
probabilities <- do.call(rbind, probs1)
remove(probs, probs1, i, groupnames)

# Probabilities for routine screening from age 10 to 84
# https://doi.org/10.1002/ijgo.12126
# Pap smear every 3 years from 25 to 65
pap <- tibble(
  Group = "Pap smear (female)",
  Age = seq(10, 84, 1),
  Fit = case_when(
    Age >= 25 & Age < 65 ~ (1/3),
    .default = 0
  ),
  SE.fit = 0
)
probabilities <- bind_rows(probabilities, pap)
remove(pap)

# Probability that pap smear requires follow-up colposcopy
# https://doi.org/10.47102/annals-acadmedsg.2023329
pr_positive_pap <- c(Events = 292, Non_events = (10967 - 292))


################################################

## Program costs
discount <- 0.03

# Note that the employee is worth more than their wage - could use a 'wage multiplier'
# https://pubmed.ncbi.nlm.nih.gov/16200550/
# Or just GDP per capita???

# Estimate median wage across the workforce:
# Spar term chosen by visual analysis to create a smoothed quadratic curve
fit <- with(na.omit(incomes), smooth.spline(Age, Total, spar = 0.94))
income <- do.call(cbind, predict(fit, x = seq(10, 84, 1))) |>
  as_tibble() |>
  rename(Age = x,
         Total = y)

# Fit check
p <- incomes |> ggplot(aes(x = Age, y = Total)) +
  geom_line() +
  geom_line(data = income, aes(x = Age, y = Total))
p
remove(fit, incomes)


# Estimate employment rate across the workforce



costs <- list()

# HPV vaccination costs
# https://www.sciencedirect.com/science/article/pii/S0264410X21003212
costs$vc <- 376

# Cost savings from pap smears
costs$pap <- 43 * (1 + discount)^(2024 - 2011)
costs$colposcopy <- 109.73 * (1 + discount)^(2024 - 2011)
