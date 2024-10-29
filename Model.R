options(scipen = 100, digits = 5)

# Global model settings
discount <- 0.03
iter <- 10000
age_start <- 10
age_end <- 84
model_year <- 2019

source("./Parameters.R", echo = F)
source("./Functions.R")

# Two separate cohorts: Male and female
# Three HPV-associated cancers in men: oropharyngeal, penile, and anal
# Four HPV-associated cancers in women: oropharyngeal, cervical, vaginal/vulval, and anal
# Three model states: Healthy, Diagnosed, Dead

n_male_10 <- population_age_10$Population[population_age_10$Group == "Male" & population_age_10$Year == model_year]
n_female_10 <- population_age_10$Population[population_age_10$Group == "Female" & population_age_10$Year == model_year]

run_sim(Gender = "Female", cancer_type = "oropharyngeal", Vaccine_type = "None", n = 20000)
