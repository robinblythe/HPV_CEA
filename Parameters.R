# Time-varying transition probabilities
source("./Read_data.R")
library(rms)
library(forecast)

# Obtaining probabilities with SE
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

# Visual test of model fitted values
# p1 <- probabilities |> ggplot()
# p1 +
#   geom_smooth(aes(x = Age, y = Fit)) +
#   facet_wrap(vars(Group), scales = "free_y") +
#   theme_bw()


# Predicted population of 10-year-olds
pop10_boys <- ts(subset(population_age_10, Group == "Male")$Population, 
                 start = 1980)

train <- window(pop10_boys, end = 2013)
test <- window(pop10_boys, start = 2014)

fit1 <- meanf(train, h = 12)
fit2 <- rwf(train, h = 12)
fit3 <- snaive(train, h = 12)

autoplot(pop10_boys) +
  autolayer(fit1, series = "Mean", PI = FALSE) +
  autolayer(fit2, series = "Naive", PI = FALSE) +
  autolayer(fit3, series = "Seasonal naive", PI = FALSE)


pop10_boys |> diff(lag = 12) |> diff() |> ggtsdisplay()
pop10_boys |> Arima(order = c(0, 1, 1), seasonal = c(0, 1, 1)) |> autoplot()


pop10_girls <- ts(subset(population_age_10, Group == "Female")$Population, 
                  start = 1980)