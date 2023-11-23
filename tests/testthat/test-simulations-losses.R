test_that("same results as blog post", {

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

  initial_freq <- 0.0587
  initial_freq_date <- lubridate::ymd("2010-01-01")
  freq_trend <- -1 / 100

  set.seed(100)
  loss_count <- simulate_loss_count(
    portfolio = policy_df,
    initial_freq = initial_freq,
    initial_freq_date = initial_freq_date,
    freq_trend = freq_trend
  )
  
  expect_equal(length(loss_count), nrow(policy_df))
  expect_equal(sum(loss_count), 2191)

})
