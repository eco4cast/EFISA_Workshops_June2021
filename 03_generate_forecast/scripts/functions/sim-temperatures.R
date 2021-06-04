stopifnot(
  requireNamespace("dplyr", quietly = TRUE),
  requireNamespace("lubridate", quietly = TRUE)
)

#' Simulate air temperatures (in degrees C)
#'
#' @param start Start date. Default = 2021-01-01
#' @param end End date. Deafult = 2021-12-31
#' @param avg_temp Average annual temperature. Default = 15 C.
#' @param range_temp Temperature range. Default = 40 C.
#' @param sd_temp Random variation in temperature, expressed as standard deviation. Default = 5 C.
#' @return data.frame of dates and simulated air temperature values
#' @author Alexey Shiklomanov
simulate_airtemp <- function(start = "2021-01-01", end = "2021-12-31",
                             avg_temp = 15, range_temp = 40, sd_temp = 5) {
  dates_all <- seq.Date(as.Date(start), as.Date(end), "days")
  istart <- lubridate::yday(start)
  iend <- lubridate::yday(end) + 365 * (lubridate::year(end) - lubridate::year(start))
  pi_seq <- seq(istart, iend) * 2 * pi / 365 - pi / 2
  airtemp_c_all <- avg_temp +
    range_temp / 2 * sin(pi_seq) +
    rnorm(length(pi_seq), 0, sd_temp)
  dplyr::tibble(time = dates_all, airtemp_c = airtemp_c_all)
}
