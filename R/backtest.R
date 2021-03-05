#' function to backtest model parameters.
#'
#' compare model forecasts to the efficient market hypothesis
#'  (i.e. assuming share price is the same the next day as it was the previous day)
#'
#' @param ncores number of cores to use for parallel computing.
#' @param ticker which ticker or tickers should we evaluate?
#' @param n_tests how many tests per ticker per month of data should we use?
#' @inheritParams renarin_short_historic
#' @param ... additional parameters passed to renarin_short_historic
#'
#' @importFrom magrittr %>%
#' @importFrom foreach %dopar% %:%
#'
#' @export

backtest_short <- function(ticker, n_tests, ncores, vendor = "quandl",
                           ...) {


  #get a time_list for multiple tickers
  time_list <- list()


  if(length(ticker > 1)) {
    #figure out which ones failed and exclude
    fail_tickers <- c()
    for(i in 1:length(ticker)) {
      time_vec <-  get_times(ticker[i], n_tests = n_tests, vendor = vendor)
      if(length(time_vec) > 0) {
        time_list[[ticker[i]]] <- time_vec
      } else {
        fail_tickers <- c(fail_tickers, ticker[i])
      }

    }
    ticker <- ticker[!(ticker %in% fail_tickers)] #exclude any tickers in fail_tickers
  } else { #if theres only one ticker then ticker[i] will break so we have to use the below code
    time_list[[1]] <- get_times(ticker, n_tests = n_tests, vendor = vendor)
  }

  #gonna try and debug in a regular for loop if any combination of tickers breaks the parallelized one

  if(ncores == 1) {
    forecast_list <- list()
    for(i in 1:length(time_list)) {
      ticker_list <- list()
      ticker_i <- ticker[i]
      for(j in 1:length(time_list[[i]])) {
        ticker_list[[j]] <- renarin_short_historic(time_list[[ticker_i]][j],
                                                   ticker = ticker_i,
                                                   vendor = vendor, ...)
      }
      forecast_list[[i]] <- ticker_list
    }
  } else {
    cl <- parallel::makeCluster(ncores)
    doParallel::registerDoParallel(cl)
    forecast_list <-
      foreach::foreach(i = 1:length(time_list),
                       .packages = c("stonkr")) %:%
      foreach::foreach(j = 1:length(time_list[[i]]),
                       .packages = c("stonkr"),
                       .inorder = FALSE) %dopar%
      {
        renarin_short_historic(time_list[[i]][j],
                               ticker = ticker[i],
                               vendor = vendor, ...)
      }
    doParallel::stopImplicitCluster()
  }

  forecast_df <- dplyr::bind_rows(forecast_list) %>%
    dplyr::mutate(resid_fcast = .data$close_forecast - .data$close,
                  resid_emh = .data$index_close - .data$close)
  return(forecast_df)
}

#' generate vector of timevecs for a given ticker
#'
#' helper function for backtest_short - this function only works with the Sharadar
#' data feed.
#'
#' @inheritParams backtest_short
#'
#' @importFrom magrittr %>%
#'

get_times <- function(ticker, n_tests, vendor) {

  key <- get_api_key(vendor = vendor)
  Quandl::Quandl.api_key(key)
  df <- Quandl::Quandl.datatable('SHARADAR/SEP', ticker= ticker)

  if(nrow(df) < n_tests) {
    return(df$date)
  } else {
    df <- df %>%
      dplyr::sample_n(size = n_tests)
    return(df$date)
  }

}




