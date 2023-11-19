simulate_portfolio <- function(sim_years, initial_policy_count, ptf_growth,
                               n_expo_per_policy, policy_length,
                               initial_avg_premium, premium_trend,
                               rate_change_data) {

  # Policy data
  policy_df <- simulate_policies(initial_policy_count, sim_years, ptf_growth)
  policy_df$expiration_date <- calc_exp_date(
    policy_df$inception_date, policy_length
  )

  # Expo data
  policy_df$n_expo <- simulate_expo(policy_df, n_expo_per_policy)

  # Premium data
  policy_df$premium <- simulate_premium(policy_df$n_expo, initial_avg_premium)
  policy_df$premium <- simulate_rate_change(
    policy_df$premium,
    policy_df$inception_date,
    rate_change_data
  )
  start_of_period <- first_day_of_year(sim_years[1])
  policy_df$premium <- apply_trend(
    policy_df$premium,
    start_of_period,
    policy_df$inception_date,
    premium_trend
  )

  policy_df
}

simulate_policies <- function(initial_policy_count, sim_years, ptf_growth) {
  # number of policies to simulate
  annual_policy_count <- apply_trend(
    value = initial_policy_count,
    from = sim_years[1],
    to = sim_years,
    trend = ptf_growth
  )

  # generate policies
  generate_one_year_policy_data <- function(year, n_policies) { # <1>
    tibble::tibble(
      policy_id = paste("policy", year, seq(n_policies), sep = "_"),
      inception_date = seq_date_in_year(year, n_policies),
    )
  }
  policy_df <- purrr::map2_dfr(
    sim_years,
    annual_policy_count,
    generate_one_year_policy_data
  )

  policy_df
}

simulate_expo <- function(policy_df, n_expo_per_policy) {
  rep(n_expo_per_policy, nrow(policy_df))
}

calc_exp_date <- function(inception_date, policy_length) {
  lubridate::add_with_rollback(
    inception_date,
    lubridate::months(policy_length)
  ) - lubridate::days(1)
}

simulate_premium <- function(n_expo, initial_avg_premium) {
  n_expo * initial_avg_premium
}

simulate_rate_change <- function(premium, inc_date, rate_change_data) {
  purrr::pwalk(rate_change_data, function(eff_date, rate_change) {
    premium <<- apply_rate_change(rate_change, eff_date, premium, inc_date)
  })
  premium
}

apply_rate_change <- function(rate_change_data, eff_date, premium, inc_date) {
  ifelse(inc_date >= eff_date, premium * (1 + rate_change_data), premium)
}
