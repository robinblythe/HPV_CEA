options(scipen = 100, digits = 5)

# Global model settings
discount <- 0.03
iter <- 100
age_start <- 10
age_end <- 84
model_year <- 2019

source("./Functions.R")
source("./Parameters.R", echo = F)

# Two separate cohorts: Male and female
# Three HPV-associated cancers in men: oropharyngeal, penile, and anal
# Four HPV-associated cancers in women: oropharyngeal, cervical, vaginal/vulval, and anal
# Three model states: Healthy, Diagnosed, Dead

sims <- list()
sims$male <- list()
sims$female <- list()

sims$male$oropharyngeal <- do.call(
  rbind,
  replicate(
    iter,
    run_sim("Oropharyngeal cancer", "Male"),
    simplify = FALSE
  )
)

sims$male$other <- do.call(
  rbind,
  replicate(
    iter,
    run_sim("Penile/anal cancer", "Male"),
    simplify = FALSE
  )
)

sims$female$oropharyngeal <- do.call(
  rbind,
  replicate(
    iter,
    run_sim("Oropharyngeal cancer", "Female"),
    simplify = FALSE
  )
)

sims$female$reproductive <- do.call(
  rbind,
  replicate(
    iter,
    run_sim("Reproductive cancer", "Female"),
    simplify = FALSE
  )
)

sims$female$anal <- do.call(
  rbind,
  replicate(
    iter,
    run_sim("Anal cancer", "Female"),
    simplify = FALSE
  )
)

results <- bind_rows(
  do.call(rbind, sims$male),
  do.call(rbind, sims$female)
)

p <- results |>
  group_by(Diagnosis, Gender, Diagnosis_age) |>
  summarise(Lost_income_mean = mean(Lost_income),
            Lost_income_low = quantile(Lost_income, 0.025),
            Lost_income_high = quantile(Lost_income, 0.975)) |>
  ggplot(aes(x = Diagnosis_age,
             y = Lost_income_mean,
             ymin = Lost_income_low,
             ymax = Lost_income_high))

p +
  geom_line() +
  geom_ribbon(fill = "grey", alpha = 0.5) +
  facet_wrap(vars(Gender, Diagnosis)) +
  theme_bw()
