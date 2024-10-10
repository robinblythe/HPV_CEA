options(scipen = 100, digits = 5)
options(dplyr.summarise.inform = F)

# Global model settings
discount <- 0.03
iter <- 10000

source("./Parameters.R", echo = F)


# Two separate cohorts: Male and female
# Three HPV-associated cancers in men: oropharyngeal, penile, and anal
# Four HPV-associated cancers in women: oropharyngeal, cervical, vaginal/vulval, and anal
# Three model states: Healthy, Diagnosed, Dead

