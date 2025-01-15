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
  
  # Workforce participation due to cancers
  # https://jamanetwork.com/journals/jama/fullarticle/183387

  for (i in 1:iter) {
    
    # Convert unemployment odds ratio to probability and combine with cancer-specific probabilities
    # https://jamanetwork.com/journals/jama/fullarticle/183387
    sim_incomes <- median_income |>
      filter(Sex == gender) |>
      mutate(
        Participation_rate_cancer =
          if (gender == "Female" & cancer_type %in% c("Cervical", "Vaginal", "Vulval", "Anal")) {
            get_odds(1 / (1 - rbeta(1, 671, 648)) / (1 / (1 - rbeta(1, 816, 506))), Participation_rate)
          } else if (cancer_type == "Oropharyngeal") {
            get_odds(1 / (1 - rbeta(1, 75, 62)) / (1 / (1 - rbeta(1, 116, 26))), Participation_rate)
          } else {
            get_odds(1 / (1 - rbeta(1, 13480, 6886)) / (1 / (1 - rbeta(1, 133588, 24015))), Participation_rate)
          },
        Weighted_income_cancer = Participation_rate_cancer * Monthly_income * 12
      )
    
    # Lifetime income function summarises expected remaining income for each year (x) until end of working life
    lifetime_income <- list()
    lifetime_income$healthy <- lapply(
      seq(age_start, age_end, 1),
      function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_income_annual", gender = gender)
    )
    
    lifetime_income$cancer <- lapply(
      seq(age_start, age_end, 1),
      function(x) get_lifetime_income(sim_incomes, age = x, income = "Weighted_income_cancer", gender = gender)
    )
    
    # Mortality adjustment:
    # Two ways to get additional decrement due to mortality
    # Should reflect that mortality after diagnosis can occur in any year, so need the average survivorship after diagnosis by age
    # Can model this with a Poisson distribution, or do a survival model with age at diagnosis as a covariate
    # If the latter, can use the Kalbfleisch-Prentice estimator to get median survival time at each age for each gender and cancer
    # Turn this into mean age at death = Diagnosis_age + years of cancer survivorship
    # Then the decrement = -lifetime_income$cancer[[Diagnosis_age + years_survived - 9]] (minus 9 needed for indexing)
    # Alternative approach using probabilities, given difficulties in above method:
    # Obtain annual survivorship = S and 1 - S = annual cancer-specific mortality
    # Obtain sum of lifetime cancer income * (1 - S) for each length of time survived for each age of diagnosis
    # Subtract this sum as a mortality decrement
    
    mort_adj <- probabilities |>
      filter(Group == gender,
             Diagnosis == cancer_type) |>
      # Need the remaining income from age + survival time to get foregone income
      # Otherwise it's a huge overestimate because it assumes same remaining lifetime income regardless of survival
      mutate(join_age = Age + survival_time - 1) |>
      left_join(
        tibble(
          Age = seq(10, 84, 1),
          income_cancer = as.vector(do.call(rbind, lifetime_income$cancer))
          ),
        by = join_by(join_age == Age)
      ) |>
      mutate(annual_decrement = income_cancer * (1 - p_survival)) |>
      group_by(Age) |>
      summarise(mort_decrement = sum(annual_decrement))
    
    # Add back in the cut-off 84 y/o remainder
    mort_adj[nrow(mort_adj) + 1, ] = list(84, 0)
      
    # Return to work/sick leave due to diagnosis
    # https://doi.org/10.1002/pon.1820
    rtw <- if (cancer_type == "Oropharyngeal") {
      rgamma(1, shape = 2.456, scale = 3.069)
    } else if (gender == "Male") {
      rgamma(1, shape = 659.678, scale = 0.159) / 30.438
    } else {
      rgamma(1, shape = 148.905, 1.088) / 30.438
    }
    
    # Rate of cancer diagnoses in the population
    
    
    # Percent of cancer attributable to HPV
    # https://onlinelibrary.wiley.com/doi/epdf/10.1002/ijc.30716
    pct_hpv <- case_when(
      cancer_type == "Cervical" ~ 1,
      cancer_type == "Anal" ~ rbeta(1, shape1 = 35000, shape2 = (40000 - 35000)),
      cancer_type == "Vaginal" ~ rbeta(1, shape1 = 12000, shape2 = (15000 - 12000)),
      cancer_type == "Vulval" ~ rbeta(1, shape1 = 8500, shape2 = (34000 - 8500)),
      cancer_type == "Penile" ~ rbeta(1, shape1 = 13000, shape2 = (26000 - 13000)),
      cancer_type == "Oropharyngeal" ~ rbeta(1, shape1 = 29000, shape2 = (96000 - 29000))
    )

    # Probability that HPV vaccination protects against cancer diagnosis (compared to naive)
    vaccine_eff <- case_when(
      # Cervical BIVALENT: https://www.nejm.org/doi/full/10.1056/NEJMoa1917338
      cancer_type == "Cervical" ~ rbeta(1, shape1 = 0.773, shape2 = 7.787),
      # Oral BIVALENT: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0068329#s3
      cancer_type == "Oropharyngeal" ~ rbeta(1, shape1 = 0.981, shape2 = 7.770),
      # Other/genital BIVALENT: https://www.sciencedirect.com/science/article/pii/S0755498214004771
      cancer_type == "Vaginal" | cancer_type == "Vulval" | (cancer_type == "Anal" & gender == "Female") ~ rbeta(1, shape1 = 0.930, shape2 = 18.548),
      cancer_type == "Penile" | (cancer_type == "Anal" & gender == "Male") ~ rbeta(1, shape1 = 7.524, shape2 = 39.539)
    )

    # Derive benefit modifier for income losses
    vaccine_benefit <- (1 - pct_hpv) + (vaccine_eff * pct_hpv)
    
    # Need the final piece - the prevalence of each cancer to quantify the vaccine benefit
    
    # Populate table of results
    sims[[i]] <- tibble(
      Iteration = i,
      Diagnosis = cancer_type,
      Gender = gender,
      Diagnosis_age = seq(age_start, age_end, 1),
      Participation_rate_cancer_by_age = sim_incomes$Participation_rate_cancer,
      Weighted_income_cancer_by_age = sim_incomes$Weighted_income_cancer,
      Mortality_decrement = -mort_adj$mort_decrement
    ) |>
      rowwise() |>
      mutate(
        Income_healthy = lifetime_income$healthy[[Diagnosis_age - 9]],
        Income_cancer = lifetime_income$cancer[[Diagnosis_age - 9]],
        Sick_leave_decrement = rtw / 12 * -median_income$Weighted_income_annual[median_income$Age == Diagnosis_age & median_income$Sex == gender],
        Adjusted_income_cancer = Income_cancer + Sick_leave_decrement + Mortality_decrement,
        Lost_income_cancer = Income_healthy - Adjusted_income_cancer,
        Income_protection_vaccination = Income_healthy - Adjusted_income_cancer * vaccine_benefit, # add HPV prevalence here
        Net_benefit_vaccination = Income_protection_vaccination - cost_vc
      )
  }
  return(do.call(rbind, sims))
}

