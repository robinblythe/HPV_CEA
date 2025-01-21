# Functions for analysis
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

est_beta <- function(p, se){
  alpha <- (p * (1 - p)) / (se^2) - p
  beta <- alpha * (1 - p) / p
  return(c(alpha, beta))
}


run_model_loop <- function(cancer_type, gender) {
  
  sims <- list()
  
  # Workforce participation due to cancers
  # https://jamanetwork.com/journals/jama/fullarticle/183387

  for (i in 1:iter) {
    
    # Convert unemployment odds ratio to probability and combine with cancer-specific probabilities
    # https://jamanetwork.com/journals/jama/fullarticle/183387
    sim_incomes <- median_income |>
      filter(Sex == gender,
             Age >= age_start) |>
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
    
    # Mortality adjustment
    mort_adj <- cancer_mortality |>
      filter(Group == gender,
             Diagnosis == cancer_type,
             Age >= age_start) |>
      # Need the remaining income from age + survival time to get foregone income
      # Otherwise it's a huge overestimate because it assumes same remaining lifetime income regardless of survival
      mutate(join_age = Age + survival_time - 1) |>
      left_join(
        tibble(
          Age = seq(age_start, age_end, 1),
          income_cancer = as.vector(do.call(rbind, lifetime_income$cancer))
          ),
        by = join_by(join_age == Age)
      ) |>
      rowwise() |>
      mutate(beta_params = list(est_beta(p_survival, se_survival)),
             alpha = beta_params[[1]],
             beta = beta_params[[2]],
             surv_prob = rbeta(1, alpha, beta),
             annual_decrement = income_cancer * (1 - surv_prob)) |>
      ungroup() |>
      group_by(Age) |>
      summarise(mort_decrement = sum(annual_decrement))
    
    # Add back in the cut-off 84 y/o remainder
    mort_adj[nrow(mort_adj) + 1, ] = list(84, 0)
    
    # Rate of cancer diagnoses in the population
    cancer_rate <- cancer_incidence |>
      filter(Group == gender,
             Diagnosis == cancer_type,
             Age >= age_start) |>
      rowwise() |>
      mutate(beta_params = list(est_beta(pred_rate, se_pred_rate)),
             alpha = beta_params[[1]],
             beta = beta_params[[2]],
             diag_prob = rbeta(1, alpha, beta)) |>
      ungroup() |>
      select(Age, Group, Diagnosis, diag_prob)
      
    # Return to work/sick leave due to diagnosis (in)
    # https://doi.org/10.1002/pon.1820
    rtw <- if (cancer_type == "Oropharyngeal") {
      rgamma(1, shape = 2.456, scale = 3.069)
    } else if (gender == "Male") {
      rgamma(1, shape = 659.678, scale = 0.159) / 30.438
    } else {
      rgamma(1, shape = 148.905, scale = 1.088) / 30.438
    }

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

    # Populate table of results
    sims[[i]] <- tibble(
      Iteration = i,
      Diagnosis = cancer_type,
      Gender = gender,
      Diagnosis_age = seq(age_start, age_end, 1)
      ) |>
      rowwise() |>
      mutate(
        Income_healthy = lifetime_income$healthy[[Diagnosis_age - age_start + 1]],
        Income_cancer = lifetime_income$cancer[[Diagnosis_age - age_start + 1]],
        Sick_leave_decrement = (rtw / 12) * -median_income$Weighted_income_annual[median_income$Age == Diagnosis_age & median_income$Sex == gender],
        Mortality_decrement = -mort_adj$mort_decrement[[Diagnosis_age - age_start + 1]], # Lost income due to excess cancer mortality
        EV_income_cancer = Income_cancer + Sick_leave_decrement + Mortality_decrement, # Expected lifetime income after cancer diagnosis
        Lost_income_cancer = EV_income_cancer - Income_healthy, # Lost income due to cancer diagnosis
        EV_cancer_no_vc = Lost_income_cancer * cancer_rate$diag_prob[[Diagnosis_age - age_start + 1]], # Prevalence adjusted lost income
        EV_cancer_vc = 
          Lost_income_cancer * cancer_rate$diag_prob[[Diagnosis_age - age_start + 1]] * (1 - pct_hpv) +
          Lost_income_cancer * cancer_rate$diag_prob[[Diagnosis_age - age_start + 1]] * (pct_hpv * vaccine_eff),
        Vaccination_benefit = EV_cancer_vc - EV_cancer_no_vc
      )
  }
  return(do.call(rbind, sims))
}

