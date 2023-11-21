test_that("same results as in blog post", {

  simulated_years <- 2010:2015
  initial_policy_count <- 12842
  portfolio_growth_rate <- .5 / 100
  initial_avg_premium <- 87
  rate_changes <- tibble::tibble(
    effective_date = lubridate::dmy(c(
      "04/01/2011", "07/01/2012", "10/01/2013",
      "07/01/2014", "10/01/2015", "01/01/2016"
    )),
    rate_change = c(-5.0, 10.0, 5.0, -2.0, 5.0, 5.0) / 100,
  )
  premium_trend <- 2 / 100
  policy_length <- 6
  n_expo_per_policy <- 1

  #---------------------------------------------------------------------------

  policy_df <- simulate_policies(
    initial_policy_count = initial_policy_count,
    sim_years = simulated_years,
    ptf_growth = portfolio_growth_rate
  )

  expect_equal(nrow(policy_df), 78022)

  #---------------------------------------------------------------------------

  policy_df$expiration_date <- calc_exp_date(
    policy_df$inception_date, policy_length
  )

  expect_equal(
    round(mean(policy_df$expiration_date)),
    lubridate::ymd("2013-07-06")
  )

  #---------------------------------------------------------------------------

  policy_df$n_expo <- simulate_expo(policy_df, n_expo_per_policy)

  expect_equal(sum(policy_df$n_expo), 78022)

  #---------------------------------------------------------------------------

  policy_df$premium <- simulate_premium(policy_df$n_expo, initial_avg_premium)

  expect_equal(sum(policy_df$premium), 78022 * initial_avg_premium)

  #---------------------------------------------------------------------------

  policy_df$premium <- simulate_rate_change(
    policy_df$premium,
    policy_df$inception_date,
    rate_changes
  )

  expect_equal(round(sum(policy_df$premium)), 7122656)

  #---------------------------------------------------------------------------
  start_of_period <- first_day_of_year(simulated_years[1])
  policy_df$premium <- apply_trend(
    policy_df$premium,
    start_of_period,
    policy_df$inception_date,
    premium_trend
  )

  expect_equal(round(sum(policy_df$premium)), 7577980)

  #---------------------------------------------------------------------------
  policy_df <- simulate_portfolio(
    sim_years = simulated_years,
    initial_policy_count = initial_policy_count,
    ptf_growth = portfolio_growth_rate,
    n_expo_per_policy = n_expo_per_policy,
    policy_length = policy_length,
    initial_avg_premium = initial_avg_premium,
    premium_trend = premium_trend,
    rate_change_data = rate_changes
  )

  expect_equal(nrow(policy_df), 78022)
  expect_equal(
    round(mean(policy_df$expiration_date)),
    lubridate::ymd("2013-07-06")
  )
  expect_equal(sum(policy_df$n_expo), 78022)
  expect_equal(round(sum(policy_df$premium)), 7577980)

})
