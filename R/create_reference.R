#' wrapper around tidyquant functions to create a reference dataset for our forecasting system
#'
#' was going to set this up entirely via API but turns out the easiest way to do this by far is to download the full
#' tables via batch download and start from there
#'
#' @param vendor which data warehouse - currently only works with the Sharadar tables from quandl
#' @param ncores number of cores to utilize for data processing and ingestion
#' @param data_repo filepath where function can find the Sharadar/SF1 table,
#' @param year year or range of years to create the reference database from.
#'     good idea to choose range with similar macroeconomic conditions to current economy.
#'
#' @importFrom magrittr %>%
#'
#' @return tibble containing time series data for our analysis.


create_reference <- function(vendor = "quandl", ncores =  4,
                             data_repo = "G:/My Drive/pkg/data/sharadar/", year = 2018) {
  key <- get_api_key(vendor = vendor)

  #set API key for use.
  if(vendor == "alphavantage") {
    tidyquant::av_api_key(key)
  } else if(vendor == "quandl") {
    tidyquant::quandl_api_key(key)
  }

  #start with the fundamentals data
  df_sf1_raw <- read.csv(paste0(data_repo, "SHARADAR_SF1.csv"))
  df_sf1 <- clean_fundamentals(df_sf1_raw, year = year)

  #then add metadata
  df_meta <- read.csv(paste0(data_repo, "SHARADAR_TICKERS.csv"))

  #
  df_price <- read.csv(paste0(data_repo, "SHARADAR_SEP"))

}

#' internal function to help process fundamentals data
#'
#' This is filtering out non-quarterly reports and adjusted reports, limiting to a certain year or range of years, and
#'
#' @param df data frame
#' @inheritParams create_reference
#'
#' @importFrom magrittr %>%

clean_fundamentals <- function(df, year) {
  #start by
  df <- df %>% dplyr::filter(.data$dimension == "ARQ",
                             lubridate::year(.data$calendardate) == year) %>%
    dplyr::select(.data$ticker, .data$calendardate, .data$datekey,
                  .data$reportperiod, .data$debt, .data$gp,
                  .data$marketcap, .data$payoutratio, .data$revenue, .data$price,
                  .data$grossmargin, .data$pe1, .data$ps1,
                  .data$rnd, .data$opex, .data$netinc,
                  .data$shareswa, .data$divyield, .data$liabilities, .data$assets,
                  .data$ev)
  return(df)
}


#' internal function (me only) to return API key for different data vendors
#'
#' @inheritParams create_reference
#'

get_api_key <- function(vendor = "alphavantage") {
  if(vendor == "alphavantage") {
    return("5O8HJ8R8H9001QRC")
  } else if(vendor == "quandl") {
    return("swwZpTLG2pozy2rLidxc")
  }
}

