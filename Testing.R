# Time-varying probabilities
# Manipulate the HPV diagnosis set to represent an annual probability

library(tidyverse)
library(rms)
library(vroom)

df_census <- vroom("./Data/census_women.csv", show_col_types = F)
df_cerv <- vroom("./Data/hpv_cervical.csv", show_col_types = F)

census <- df_census |>
  rowwise() |>
  mutate(start_age = substr(Age, 1, 2),
         end_age = substr(Age, 6, 7),
         Age = list(seq(start_age, end_age, 1)),
         Pop_total = Number/5) |>
  unnest(Age) |>
  select(Age, Pop_total)
remove(df_census)

set.seed(888)
cases <- df_cerv |>
  select(year, age, Freq) |>
  filter(!(age %in% c("<10", ">85"))) |>
  mutate(Freq = as.integer(ifelse(Freq == -99, runif(1, 1, 4), Freq))) |>
  group_by(age) |>
  summarise(Freq = sum(Freq)) |>
  rowwise() |>
  mutate(start_age = substr(age, 1, 2),
         end_age = substr(age, 4, 5),
         Age = list(seq(start_age, end_age, 1))) |>
  unnest(Age) |>
  group_by(Age) |>
  summarise(Freq = Freq/5) |>
  full_join(census) |>
  mutate(Rate = Freq/Pop_total)
remove(df_cerv, census)

# Obtaining probabilities with SE
fit <- glm(Rate ~ rcs(Age, 5), family = "quasibinomial", data = cases, x = T, y = T)
preds <- predict(fit, type = "response", se.fit = T)

# Can now obtain samples for probability of contracting cervical cancer each year using:
rnorm(1, mean = preds$fit[1], sd = preds$se.fit[1])
# This can be integrated into the heemod package by using rnorm(1, preds$fit[model_time], preds$se.fit[model_time])
