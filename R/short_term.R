#' function to generate forecast for a given ticker
#'
#' use nnet forecasting to evaluate future stock price.
#'
#' @param ticker security name, e.g. AAPL
#' @param vendor provider for data: leave at yahoo unless you have paid for the quandl sharadar equity prices data feed.
#' @param look_back number of days to look back for predictions: default is 200
#' @param look_ahead number of days to forecast: default is 14
#' @param PI logical: should we compute prediction intervals?: under development
#' @inheritParams renarin_short_historic
#'
#' @importFrom magrittr %>%
#' @export

renarin_short <- function(ticker, vendor = "yahoo", look_back = 200,
                          look_ahead = 14, lag = 10, decay = 0.2,
                          zoom_in = TRUE, PI = FALSE) {
  ticker1 = ticker #just to avoid any bugs due to non-standard eval

  date <- Sys.Date()
  min_date <- date - lubridate::days(look_back)

  if(vendor == "quandl") {
    key <- get_api_key(vendor = vendor)
    Quandl::Quandl.api_key(key)
    df_model <- Quandl::Quandl.datatable('SHARADAR/SEP', date.gte=min_date,
                                         ticker= ticker1) %>%
      dplyr::mutate(close_log = log(.data$close),
                    rank = rank(date))
  } else if(vendor == "yahoo") {
    df_model <- tidyquant::tq_get(x = ticker1, from = min_date, to = date) %>%
      dplyr::mutate(close_log = log(.data$close),
                    rank = rank(date))
  }


  #create differenced ts for modeling
  ts_close <- diff(zoo::zoo(x = df_model$close_log, order.by = df_model$rank))

  #don't want to try to fit if we don't have at least 80% of the points we wanted
  if(length(ts_close) < 0.6*look_back) {
    message(paste("not enough data to predict", ticker1, date))
    return()
  }

  #fit the model and forecast
  fit <- forecast::nnetar(y = ts_close, p = lag, P = 1, decay = decay)
  fcast <- forecast::forecast(fit, h = look_ahead, PI=PI,
                              bootstrap = TRUE)

  #tidy this shit up
  predic_df <- process_fit(fit = fit, fcast = fcast, df = df_model,
                           look_ahead = look_ahead, PI = PI)

  #add model fit and forecast back to data frame
  df_model <- predic_df %>% dplyr::left_join(df_model) %>%
    dplyr::mutate(fitted_actual = exp(undiff(x= .data$.fitted,
                                             y = .data$close_log,
                                             z = .data$rank)),
                  fitted_log = undiff(x= .data$.fitted,
                                      y = .data$close_log,
                                      z = .data$rank))

  if(PI == TRUE) {
    df_model <- undiff_intervals(df_model)
  }

  df_model <- add_dates(df_model, date = date, look_ahead = look_ahead)
  if(zoom_in == TRUE) {
    return(df_model)
  } else {
    return(summarize_forecast(df_model))
  }

}

#' helper function to undiff forecasts from renarin_short -
#'
#' tricky because the differences were lagged by one but the forecast lags by 10.
#'
#' @param x vector of differenced values to undiff
#' @param y vector of reference values
#' @param z vector of indices.
#' @importFrom magrittr %>%


undiff <- function(x,y,z) {
  y_diff = c(NA, diff(y))
  df = data.frame(x=x, y=y, y_diff = y_diff, z=z) %>%
    dplyr::mutate(x = ifelse(is.na(.data$x), .data$y_diff, .data$x)) #impute missing x with y


  #grab index value:
  index <- dplyr::filter(df, .data$z %in% (min(.data$z))) %>% #some magic numbers - essentially we want to start at one behind the forecast + lag
    dplyr::select(.data$y) %>% unlist()

  #doesn't work with the one NA
  df <- dplyr::filter(df, !is.na(.data$x))
  x <- stats::diffinv((df$x), lag = 1, xi = index) #only diff between this and the historic one is that we don't need to reverse it.
  return(x)
}

#' helper function to undiff forecasts from renarin_short -
#'
#' tricky because the differences were lagged by one but the forecast lags by 10.
#'
#' @param df model output
#' @importFrom magrittr %>%

undiff_intervals <- function(df) {
  df <- df %>%
    dplyr::mutate(hi_95 = .data$hi_95*.data$fitted_actual + .data$fitted_actual,
                  hi_80 = .data$hi_80*.data$fitted_actual + .data$fitted_actual,
                  lo_80 = .data$lo_80*.data$fitted_actual + .data$fitted_actual,
                  lo_95 = .data$lo_95*.data$fitted_actual + .data$fitted_actual)
  return(df)

}


#' helper function to process and join forecast output from renarin_short
#'
#' @param fit model fit object (from nnetar)
#' @param fcast model forecast object
#' @param look_ahead how many days did we forecast? inherits from \code{\link{renarin_short}}
#' @param df model_df
#' @importFrom magrittr %>%

process_fit<- function(fit, fcast, df, look_ahead, PI) {
  #put together tidy data frame of model output
  fit_df <- sweep::sw_augment(fit)
  match_df <- data.frame(index = 1, .actual = NA, .fitted = NA, .resid = NA)
  fit_df <- rbind(match_df, fit_df) #have to do this step because the differencing step fucks it.
  colnames(fit_df)[colnames(fit_df) == "index"] <- "rank"

  if(PI == TRUE) {
    fit_df <- fit_df %>%
      dplyr::mutate(point_forecast = NA,
                    lo_80 = NA, hi_80 = NA,
                    lo_95 = NA, hi_95 = NA)
  } else {
    fit_df <- fit_df %>%
      dplyr::mutate(point_forecast = NA)
  }



  #tidy up forecast output.
  fcast_df <- as.data.frame(fcast) %>% janitor::clean_names()
  fcast_df$rank <- (nrow(fit_df)+1):(nrow(df) + look_ahead)
  fcast_df <- fcast_df %>%
    dplyr::mutate(`.actual` = NA,
                  `.fitted` = .data$point_forecast,
                  `.resid` = NA)

  return(rbind(fit_df, fcast_df))
}

#' helper function to add dates to the projected time points
#'
#' @param df model output df
#' @param date sys.date()
#' @inheritParams renarin_short
#' @importFrom magrittr %>%

add_dates <- function(df, date, look_ahead) {
  end_bizday = bizdays::add.bizdays(date, n = look_ahead)
  start_bizday = date + lubridate::days()
  bizday_sequence <- bizdays::bizseq(from = start_bizday, to = end_bizday)
  df$date[is.na(df$date)] <- bizday_sequence
  return(df)
}

#' helper function to summarize forecast
#'
#' @param df model output df
#' @importFrom magrittr %>%
#'

summarize_forecast <- function(df) {
  forecast_period <- df %>%
    dplyr::filter(is.na(close))

  #get data frame of final forecast price
  final_price <- forecast_period %>%
    dplyr::filter(rank == max(.data$rank)) %>%
    dplyr::select(.data$fitted_actual) %>% unlist()

  index_price <- df %>%
    dplyr::filter(!is.na(close)) %>%
    dplyr::filter(rank == max(.data$rank))

  tbl <- data.frame(index_price = index_price$close,
                    final_price = final_price,
                    max_price = max(forecast_period$fitted_actual),
                    min_price = min(forecast_period$fitted_actual),
                    ticker = unique(index_price$ticker)) %>%
    dplyr::mutate(maximum_return = (.data$max_price/.data$index_price -1) * 100,
                  maximum_loss = (.data$min_price/.data$index_price - 1) * 100,
                  expected_return = (.data$final_price/.data$index_price -1) * 100)

  return(tbl)

}


