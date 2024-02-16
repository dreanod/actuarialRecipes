#' Simulate the Loss Count by Policies in a Portfolio
#'
#' Given a portfolio of policies and frequency assumptions
#' this function will simulate the loss count of each policy.
#'
#' @param portfolio A data frame containing the policy data.
#'   It needs to contain the following columns:
#'
#'   * `inception_date` <Date>
#'   * `expiration_date` <Date>
#'   * `n_expo` <numeric>
#' @param initial_freq The expected annual loss frequency per
#'   exposure at date `initial_freq_date`
#' @param initial_freq_date The date for the initial frequency
#' @param freq_trend The annual rate of change of the frequency
#' @export
simulate_loss_count <- function(portfolio, initial_freq, initial_freq_date,
                                freq_trend) {
  annual_frequency_per_expo <- apply_trend(
    value = initial_freq,
    from = initial_freq_date,
    to = portfolio$inception_date,
    trend = freq_trend
  )
  annual_frequency_per_policy <- annual_frequency_per_expo * portfolio$n_expo
  policy_duration <- period_length(
    begin = portfolio$inception_date,
    end = portfolio$expiration_date,
    unit = "year"
  )
  frequency_per_policy <- annual_frequency_per_policy * policy_duration

  loss_count <- stats::rpois(nrow(portfolio), frequency_per_policy)
  loss_count
}
