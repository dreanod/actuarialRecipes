#-------- Display ------------

#' Create an HTML Table to Display a Data Frame
#'
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

seq_date_in_year <- function(y, n) {
  from <- first_day_of_year(y)
  to <- first_day_of_year(y + 1)
  seq(from, to, length.out = n + 1)[1:n]
}

first_day_of_year <- function(y) {
  lubridate::ymd(paste0(y, "-01-01"))
}

last_day_of_year <- function(y) {
  lubridate::ymd(paste0(y, "-12-31"))
}

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
