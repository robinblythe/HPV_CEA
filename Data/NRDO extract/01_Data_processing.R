options(scipen = 100, digits = 3)
library(tidyverse)

df_raw <- vroom::vroom("Z:/nrdo data/1968-2021 cancer staple dataset.csv")

hpv_death_codes <- c("C10", "146.3", "146.4", "146.5", "146.6", "146.7", "146.8", "146.9", # Oropharynx
                     "C21", "154.2", "154.3", "154.8", # Anus and anal canal
                     "C53", "180", # Cervix
                     "C52", "184.0", # Vagina
                     "C51", "184.1", "184.2", "184.3", "184.4", # Vulva
                     "C60", "187.1", "187.2", "187.3", "187.4") # Penis

df_processed <- df_raw |>
  mutate(
    cancer_type = case_when(
      grepl("C10.", pri_site) ~ "Oropharynx",
      grepl("C21.", pri_site) ~ "Anus/Anal canal",
      grepl("C51.", pri_site) ~ "Vulva",
      grepl("C52.", pri_site) ~ "Vagina",
      grepl("C53.", pri_site) ~ "Cervix",
      grepl("C60.", pri_site) ~ "Penis"
      ),
    death_hpv = ifelse(grepl(paste(hpv_death_codes, collapse = "|"), death_code), 1, 0)
    ) |>
  filter(year_of_diagnosis > 1992,
         !is.na(cancer_type)
         ) |>
  mutate(ID = row_number()) |>
  select(ID, cancer_type, gender, ethnic_group, year_of_diagnosis, age_at_diagnosis, 
         year_of_death, age_at_death, survival_time, death_hpv)

saveRDS(df_processed, file = "cleaned_data.rds")
