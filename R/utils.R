#-------- Display ------------

#' Create an HTML Table to Display a Data Frame
#' @param df data frame to display
#' @export
display_table <- function(df) {
  df |>
    knitr::kable() |>
    kableExtra::kable_styling(
      full_width = FALSE,
      bootstrap_options = c("striped", "condensed")
    )
}

#--------- Dates ------------

#' Sample uniformly dates withing a given year
#'
#' @param y the year from which the dates are sampled
#' @param n the number of dates to sample in the year
#' @return a vector of dates
#' @export
seq_date_in_year <- function(y, n) {
  from <- first_day_of_year(y)
  to <- first_day_of_year(y + 1)
  round(seq(from, to, length.out = n + 1)[1:n])
}

#' Return the first/last day of a given year as a Date
#'
#' @param y the year for which to return the first/last day
#' @export
first_day_of_year <- function(y) {
  lubridate::ymd(paste0(y, "-01-01"))
}

#' @rdname first_day_of_year
last_day_of_year <- function(y) {
  lubridate::ymd(paste0(y, "-12-31"))
}

#' Calculates the duration between two dates
#'
#' @param begin beginning date of the period
#' @param end end date of the period
#' @param unit unit of measure for the duration of the period.
#'   Can be seconds, minutes, hours, days, weeks, months, or
#'   years.
#'
#' @export
period_length <- function(begin, end, unit = "year") {
  end <- end + lubridate::days(1)
  lubridate::interval(begin, end) / lubridate::period(1, units = unit)
}

#------- On-Leveling -----------

apply_trend <- function(value, from, to, trend) {
  from <- if (lubridate::is.Date(from)) from else first_day_of_year(from)
  to <- if (lubridate::is.Date(to)) to else first_day_of_year(to)

  trend_period <- lubridate::interval(from, to) / lubridate::years(1)
  value * (1 + trend) ^ trend_period
}
