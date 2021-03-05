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

#' function to query wikipedia and pull metadata regarding tickers on the S and P 500
#'
#' what the title says
#' @param keep_metadata logical. should we keep metadata?
#'
#' @importFrom magrittr %>%
#'
#' @export
#'

get_sp500 <- function(keep_metadata = FALSE) {
  url_sp <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  tickers <- url_sp %>%
    # read the HTML from the webpage
    xml2::read_html() %>%
    # one way to get table
    #html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
    # easier way to get table
    rvest::html_nodes(xpath = '//*[@id="constituents"]') %>%
    rvest::html_table()
  #create a vector of tickers
  sp500tickers <- tickers[[1]]
  sp500tickers = sp500tickers %>%
    dplyr::mutate(Symbol = dplyr::case_when(Symbol == "BRK.B" ~ "BRK-B",
                                            Symbol == "BF.B" ~ "BF-B",
                                            TRUE ~ as.character(Symbol)))
  if(keep_metadata == TRUE) {
    return(sp500tickers)
  } else {
    return(sp500tickers$Symbol)
  }
}


