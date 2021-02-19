#' function to generate vector of start times for a given ticker
#'
#' helper function for historical performance of forecasting system.
#'
#' @param ticker security identifier
#' @param df dataframe containing historical data about security price
#' @param num_times number of timeframes we want per calendar month (defaults to one)
#'
#' @importFrom magrittr %>%
#'
#' @export

generate_timevec <- function(ticker, df, num_times) {
  ticker1 <- ticker
  df_ticker <- dplyr::filter(df, .data$ticker == ticker1) %>%
    dplyr::mutate(year = lubridate::year(.data$date),
                  month = lubridate::month(.data$date)) %>%
    dplyr::group_by(.data$year, .data$month) %>%
    dplyr::slice_sample(n = num_times)
  date_vec <- df_ticker$date
  return(date_vec)
}
