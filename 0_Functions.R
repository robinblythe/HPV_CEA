# Functions for analysis

# Helper function to derive odds ratio from 2 x 2 tables in literature
get_or <- function(a, b, c, d) {
  p_employed_cancer <- rbeta(1, a, b)
  p_employed_no_cancer <- rbeta(1, c, d)
  (p_employed_cancer / (1 - p_employed_cancer))/(p_employed_no_cancer / (1 - p_employed_no_cancer))
}

# Helper function to convert probabilities to odds and multiply by OR, then convert back to probability
apply_odds <- function(OR, p0) {
  p_odds <- p0 / (1 - p0)
  o <- p_odds * OR
  p1 <- o / (1 + o)
  return(p1)
}

# Helper function to get lifetime expected income for each group
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

# Helper function to estimate Beta distribution parameters for each line in dataframe
est_beta <- function(p, se) {
  alpha <- (p * (1 - p)) / (se^2) - p
  beta <- alpha * (1 - p) / p
  return(c(alpha, beta))
}


# Main model function
run_model <- function(cancer_type, gender, seed, iteration) {
  
  set.seed(seed + iteration)
  ages <- seq(age_start, age_end, 1)
  idx   <- ages - age_start + 1

    # Convert baseline participation rate to odds and multiply by cancer employment OR
    # Workforce participation due to cancers
    # https://jamanetwork.com/journals/jama/fullarticle/183387
    
    sim_incomes <- median_income |>
      filter(
        Sex == gender,
        Age >= age_start
      ) |>
      mutate(
        Participation_rate_cancer =
          if (gender == "Female" & cancer_type %in% c("Cervical", "Vaginal", "Vulval", "Anal")) {
            apply_odds(get_or(671, 648, 816, 506), Participation_rate)
          } else if (cancer_type == "Oropharyngeal") {
            apply_odds(get_or(75, 62, 116, 26), Participation_rate)
          } else {
            apply_odds(get_or(423, 275, 1770, 659), Participation_rate)
          },
        Weighted_income_cancer = Participation_rate_cancer * Predicted_income * 12
      )

    # Lifetime income function summarises expected remaining income for each year (x) until end of working life
    income_healthy <- vapply(
      ages,
      function(x)
        get_lifetime_income(
          sim_incomes,
          age = x,
          income = "Weighted_income_annual",
          gender = gender
        ),
      numeric(1)
    )

    income_cancer <- vapply(
      ages,
      function(x)
        get_lifetime_income(
          sim_incomes,
          age = x,
          income = "Weighted_income_cancer",
          gender = gender
        ),
      numeric(1)
    )

    # Mortality adjustment
    mort_adj <- cancer_mortality |>
      filter(
        Group == gender,
        Diagnosis == cancer_type,
        Age >= age_start
      ) |>
      mutate(
        join_age = Age + survival_time - 1,
        p_mort = 1 - p_survival
      ) |>
      left_join(
        tibble(
          Age = seq(age_start, age_end, 1),
          income_cancer = income_cancer
        ),
        by = join_by(join_age == Age)
      ) |>
      group_by(Age) |>
      mutate(
        p_mort_annual = p_mort - lag(p_mort),
        p_mort_annual = ifelse(is.na(p_mort_annual), p_mort, p_mort_annual),
        p_mort_annual = ifelse(p_mort_annual == 0, NA, p_mort_annual),
        se_mort_diff = sqrt(se_survival + lag(se_survival, default = 0))
      ) |>
      fill(p_mort_annual, .direction = c("down")) |>
      rowwise() |>
      mutate(
        beta_params = list(est_beta(p_mort_annual, se_mort_diff)),
        alpha = beta_params[[1]],
        beta = beta_params[[2]],
        mort_prob = rbeta(1, alpha, beta)
      ) |>
      group_by(Age) |>
      mutate(annual_decrement = income_cancer * mort_prob) |>
      summarise(mort_decrement = sum(annual_decrement))

    # Add back in the cut-off 84 y/o remainder
    mort_adj[nrow(mort_adj) + 1, ] <- list(84, 0)

    # Rate of cancer diagnoses in the population
    cancer_rate <- cancer_incidence |>
      filter(
        Group == gender,
        Diagnosis == cancer_type,
        Age >= age_start
      ) |>
      rowwise() |>
      mutate(
        beta_params = list(est_beta(pred_rate, se_pred_rate)),
        alpha = beta_params[[1]],
        beta = beta_params[[2]],
        diag_prob = rbeta(1, alpha, beta)
      ) |>
      ungroup() |>
      select(Age, Group, Diagnosis, diag_prob)

    # Return to work/sick leave due to diagnosis (in months)
    # Oropharyngeal: Assume 1 to 52 weeks is the 99.9th range of percentiles
    # From: https://doi.org/10.1186/s41199-016-0002-0
    # Solve using ShinyPrior
    # For RTW for non-oropharyngeal: https://doi.org/10.1002/pon.1820
    # Assume patients have 14 days sick leave covered by employer, so no personal cost to income
    rtw <- case_when(
      cancer_type == "Oropharyngeal" ~ max(0, (rgamma(1, shape = 3.545, scale = 3.972) - 2) / 4.345), # weeks to months
      gender == "Male" & cancer_type != "Oropharyngeal" ~ max(0, (rgamma(1, shape = 659.678, scale = 0.159) - 14) / 30.438), # days to months
      gender == "Female" & cancer_type != "Oropharyngeal" ~ max(0, (rgamma(1, shape = 148.905, scale = 1.088) - 14) / 30.438) # days to months
    )
      
    # Percent of cancer attributable to HPV 16/18
    # https://onlinelibrary.wiley.com/doi/epdf/10.1002/ijc.30716
    pct_hpv <- case_when(
      cancer_type == "Cervical" ~ rbeta(1, shape1 = 370000, shape2 = (530000 - 370000)),
      cancer_type == "Anal" ~ rbeta(1, shape1 = 30000, shape2 = (40000 - 30000)),
      cancer_type == "Vaginal" ~ rbeta(1, shape1 = 7400, shape2 = (15000 - 7400)),
      cancer_type == "Vulval" ~ rbeta(1, shape1 = 6200, shape2 = (34000 - 6200)),
      cancer_type == "Penile" ~ rbeta(1, shape1 = 9100, shape2 = (26000 - 9100)),
      # Note the paper doesn't split oro in the 16/18 vs other HPV strains - used the same proportion as an assumption
      cancer_type == "Oropharyngeal" ~ rbeta(1, shape1 = 0.849*96000, shape2 = (96000 - 0.849*96000))
    )

    # Probability that HPV vaccination protects against cancer diagnosis (compared to naive)
    vaccine_eff <- case_when(
      # Cervical BIVALENT: https://www.nejm.org/doi/full/10.1056/NEJMoa1917338
      # IRR used as proxy for probability due to rare events
      cancer_type == "Cervical" ~ rbeta(1, shape1 = 0.773, shape2 = 7.787),
      # Oral BIVALENT: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0068329#s3
      cancer_type == "Oropharyngeal" ~ rbeta(1, shape1 = 0.981, shape2 = 7.770),
      # Other/genital BIVALENT: https://www.sciencedirect.com/science/article/pii/S0755498214004771
      cancer_type == "Vaginal" | cancer_type == "Vulval" | (cancer_type == "Anal" & gender == "Female") ~ rbeta(1, shape1 = 0.930, shape2 = 18.548),
      cancer_type == "Penile" | (cancer_type == "Anal" & gender == "Male") ~ rbeta(1, shape1 = 7.524, shape2 = 39.539)
    )

    annual_income <- median_income$Weighted_income_annual[median_income$Age %in% ages & median_income$Sex == gender]
    
    Sick_leave_decrement <- (rtw / 12) * -annual_income
    Mortality_decrement  <- -mort_adj$mort_decrement[idx]
    
    diag_prob <- cancer_rate$diag_prob[idx]
    
    # Populate table of results
    sim <- tibble(
      Iteration = iteration,
      Diagnosis = cancer_type,
      Gender = gender,
      Diagnosis_age = ages,
      
      Income_healthy = income_healthy,
      Income_cancer  = income_cancer,
      
      Sick_leave_decrement = Sick_leave_decrement,
      Mortality_decrement  = Mortality_decrement,
      
      EV_income_cancer = income_cancer + Sick_leave_decrement + Mortality_decrement,
      
      Lost_income_cancer = EV_income_cancer - income_healthy,
      
      EV_cancer_no_vc = Lost_income_cancer * diag_prob,
      
      EV_cancer_vc =
        Lost_income_cancer * diag_prob * (1 - pct_hpv) +
        Lost_income_cancer * diag_prob * (pct_hpv * vaccine_eff),
      
      Vaccination_benefit =
        EV_cancer_vc - EV_cancer_no_vc
    )
}
