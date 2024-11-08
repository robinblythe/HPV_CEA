# Functions for analysis

# Convert incidence to rate using census data
fn_incidence <- function(Gender, data, dx_group) {
  data |>
    select(year, age, gender, Freq) |>
    filter(
      !(age %in% c("<10", ">85")),
      gender == Gender
    ) |>
    rowwise() |>
    mutate(
      start_year = as.integer(substr(year, 1, 4)),
      end_year = as.integer(substr(year, 6, 9)),
      Year = list(seq(start_year, end_year, 1)),
      Freq = as.integer(ifelse(Freq == -99, runif(1, 1, 4), Freq)) / (end_year - start_year + 1),
      Diagnosis = dx_group,
      Group = ifelse(gender == "F", "Female", "Male")
    ) |>
    unnest(Year) |>
    rowwise() |>
    mutate(
      start_age = as.integer(substr(age, 1, 2)),
      end_age = as.integer(substr(age, 4, 5)),
      Age = list(seq(start_age, end_age, 1)),
      N = Freq / (end_age - start_age + 1)
    ) |>
    unnest(Age) |>
    group_by(Age, Group, Diagnosis) |>
    summarise(N = mean(N)) |>
    select(Age, Group, Diagnosis, N)
}

get_odds <- function(OR, p0) {
  p_odds <- p0 / (1 - p0)
  o <- p_odds * OR
  p1 <- o / (1 + o)
  return(p1)
}



# Get lifetime expected income for each group
get_lifetime_income <- function(data, age, income, gender) {
  data |>
    filter(
      Age >= age,
      Sex == gender
    ) |>
    rowwise() |>
    mutate(discounted_income = get({{ income }}) / (1 + discount)^row_number()) |>
    group_by(Sex) |>
    summarise(lifetime_income = sum(discounted_income)) |>
    ungroup() |>
    pull()
}



run_model_loop <- function(cancer_type, gender) {
  
  sims <- list()
  
  for (i in 1:iter) {
    
    # https://jamanetwork.com/journals/jama/fullarticle/183387
    sim_incomes <- median_income |>
      filter(Sex == gender) |>
      mutate(
        Participation_rate_cancer =
          if (gender == "Female" & cancer_type %in% c("Cervical", "Vaginal", "Anal")) {
            get_odds(1 / (1 - rbeta(1, 671, 648)) / (1 / (1 - rbeta(1, 816, 506))), Participation_rate)
          } else if (cancer_type == "Oropharyngeal") {
            get_odds(1 / (1 - rbeta(1, 75, 62)) / (1 / (1 - rbeta(1, 116, 26))), Participation_rate)
          } else {
            get_odds(1 / (1 - rbeta(1, 13480, 6886)) / (1 / (1 - rbeta(1, 133588, 24015))), Participation_rate)
          },
        Weighted_income_cancer = Participation_rate_cancer * Monthly_income * 12
      )
    
    lifetime_income <- list()
    lifetime_income$healthy <- lapply(
      seq(age_start, age_end, 1),
      function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_income_annual", gender = gender)
    )
    
    lifetime_income$cancer <- lapply(
      seq(age_start, age_end, 1),
      function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_income_cancer", gender = gender)
    )
    
    # https://doi.org/10.1002/pon.1820
    rtw <- if (cancer_type == "Oropharyngeal") {
      rgamma(1, shape = 2.456, scale = 3.069)
    } else if (gender == "Male") {
      rgamma(1, shape = 659.678, scale = 0.159) / 30.438
    } else {
      rgamma(1, shape = 148.905, 1.088) / 30.438
    }
    
    # Percent of cancer attributable to HPV
    # https://onlinelibrary.wiley.com/doi/epdf/10.1002/ijc.30716
    pct_hpv <- case_when(
      cancer_type == "Cervical" ~ 1,
      cancer_type == "Anal" ~ rbeta(1, shape1 = 35000, shape2 = 5000),
      cancer_type == "Vaginal" ~ rbeta(1, shape1 = (8500 + 12000), shape2 = ((34000 - 8500) + (15000 - 12000))),
      cancer_type == "Penile" ~ rbeta(1, shape1 = 13000, shape2 = 26000),
      cancer_type == "Oropharyngeal" ~ rbeta(1, shape1 = 29000, shape2 = (96000 - 29000))
    )
    
    # Probability that HPV vaccination protects against cancer diagnosis (compared to naive)
    vaccine_eff <- case_when(
      # Cervical BIVALENT: https://www.nejm.org/doi/full/10.1056/NEJMoa1917338
      cancer_type == "Cervical" ~ rbeta(1, shape1 = 0.773, shape2 = 7.787),
      # Oral BIVALENT: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0068329#s3
      cancer_type == "Oropharyngeal" ~ rbeta(1, shape1 = 0.981, shape2 = 7.770),
      # Other/genital BIVALENT: https://www.sciencedirect.com/science/article/pii/S0755498214004771
      cancer_type == "Vaginal" | (cancer_type == "Anal" & gender == "Female") ~ rbeta(1, shape1 = 0.930, shape2 = 18.548),
      cancer_type == "Penile" | (cancer_type == "Anal" & gender == "Male") ~ rbeta(1, shape1 = 7.524, shape2 = 39.539)
    )
      
    incidence <- probabilities |>
      select(Age, Group, Diagnosis, Pr_cancer) |>
      filter(Group == gender,
             Diagnosis == cancer_type) |>
      rowwise() |>
      mutate(Pr_cancer = rbeta(1, N_cases, (Pop_total - N_cases)))
    
    sims[[i]] <- tibble(
      Iteration = i,
      Diagnosis = cancer_type,
      Gender = gender,
      Diagnosis_age = seq(age_start, age_end, 1),
    ) |>
      rowwise() |>
      mutate(
        Income_healthy = lifetime_income$healthy[[Diagnosis_age - 9]],
        Income_cancer = lifetime_income$cancer[[Diagnosis_age - 9]],
        Sick_leave_decrement = rtw / 12 * -median_income$Weighted_income_annual[median_income$Age == Diagnosis_age & median_income$Sex == gender],
        Expected_income = Income_cancer + Sick_leave_decrement,
        Lost_income = Income_healthy - Expected_income
      )
  }
  return(do.call(rbind, sims))
}

