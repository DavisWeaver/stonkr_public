#' function to generate forecast for a given ticker (using historical data).
#'
#' use nnet forecasting to evaluate future stock price.
#'
#' @param ticker security name, e.g. AAPL
#' @param date what is our index date for projection?
#' @param look_back number of days to look back for predictions
#' @param look_ahead number of days to forecast
#' @param zoom_in should function return full model and forecast results? useful for highlighting speicific issues
#' @param lag number of days to use to fit the next day in the model
#' @param decay NNET parametar
#' @param vendor data provider - defaults to quandl but most users may prefer "yahoo"
#'
#'
#' @importFrom magrittr %>%
#' @export

renarin_short_historic <- function(date = NULL, ticker,
                                   look_back = 200, look_ahead = 14,
                                   zoom_in = FALSE, lag = 20, decay = 0.2,
                                   vendor = "quandl", PI = FALSE) {

  #first filter data frame down to the specified date range
  max_date <- date + lubridate::days(look_ahead)

  #throw an error if the max date is after current date
  if(max_date > Sys.Date()) {
    message("provided date too close to current date")
    return()
  }

  min_date <- date - lubridate::days(look_back)

  #need to rename user-provided ticker because the dplyr non-standard evaluation gets confused
  ticker1 <- ticker

  if(vendor == "quandl") {
    key <- get_api_key(vendor = vendor)
    Quandl::Quandl.api_key(key)
    df_model <- Quandl::Quandl.datatable('SHARADAR/SEP', date.gte=min_date,
                                         date.lte = max_date,
                                         ticker= ticker1) %>%
      dplyr::mutate(close_log = log(.data$close),
                    rank = rank(.data$date))
  } else if(vendor == "yahoo") {
    df_model <- tidyquant::tq_get(x = ticker1, from = min_date, to = max_date) %>%
      dplyr::mutate(close_log = log(.data$close),
                    rank = rank(.data$date))
  }

  #add logical vec for training
  df_model$training <- (df_model$date <= date)

  #create zoo ts
  df_model_train <- dplyr::filter(df_model, .data$training == TRUE)
  ts_close <- diff(zoo::zoo(x = df_model_train$close_log, order.by = df_model_train$rank))

  #don't want to try to fit if we don't have at least 80% of the points we wanted
  if(length(ts_close) < 0.6*look_back) {
    message(paste("not enough data to predict", ticker1, date))
    return()
  }

  #fit all the training points.
  fit <- try(forecast::nnetar(y = ts_close, p = lag, P = 1, decay = decay))

  #sometimes we still get an error for missing values near the training set
  if(inherits(fit, "try-error")){
    message(paste("not enough data to predict", ticker1, date))
    return()
  }
  fcast <- try(forecast::forecast(fit, h = sum(df_model$training == FALSE), PI=PI,
                              bootstrap = TRUE))
  if(inherits(fcast, "try-error")){
    message(paste("not enough data to predict", ticker1, date))
    return()
  }


  #tidy this shit up
  predic_df <- process_fit_historic(fit = fit, fcast = fcast,
                                    df = df_model, PI = PI)

  #add model fit and forecast back to data frame
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
  #summarize performance of forecast.

  if(zoom_in == FALSE) {
    out <- summarize_forecast_historic(df_model, PI = PI)
    return(out)
  } else {
    return(df_model)
  }


}

#' function to summarize the output of short-term model (for back-testing)
#'
#'
#' @param df dataframe containing projections for a given time period
#' @inheritParams renarin_short_historic
#'
#' @importFrom magrittr %>%

summarize_forecast_historic <- function(df, PI) {

  #first move limit to the forecast points
  forecast_df <- df %>% dplyr::filter(.data$training == FALSE) %>%
    dplyr::mutate(resid = .data$fitted_actual - .data$close)

  index_point <- df %>% dplyr::filter(.data$training == TRUE) %>%
    dplyr::filter(.data$rank == max(.data$rank)) %>%
    dplyr::select(.data$close) %>%
    unlist()

  last_point <- forecast_df %>% dplyr::filter(.data$date == max(.data$date))

  if(PI == TRUE) {
    sum_tbl <- forecast_df %>%
      dplyr::summarise(mse = mean(.data$resid^2),
                       mae = max(.data$resid),
                       ticker = unique(.data$ticker)) %>%
      dplyr::mutate(index_close = index_point,
                    above_median = (last_point$close > last_point$fitted_actual),
                    below_median = (last_point$close < last_point$fitted_actual),
                    within_80 = (last_point$close < last_point$hi_80 &
                                   last_point$close > last_point$lo_80),
                    within_95 = (last_point$close < last_point$hi_95 &
                                   last_point$close > last_point$lo_95),
                    close = last_point$close,
                    close_forecast = last_point$fitted_actual,
                    close_hi95 = last_point$hi_95,
                    close_lo95 = last_point$lo_95,
                    date = last_point$date,
      )
  } else {
    sum_tbl <- forecast_df %>%
      dplyr::summarise(mse = mean(.data$resid^2),
                       mae = max(.data$resid),
                       ticker = unique(.data$ticker)) %>%
      dplyr::mutate(index_close = index_point,
                    above_median = (last_point$close > last_point$fitted_actual),
                    below_median = (last_point$close < last_point$fitted_actual),
                    close = last_point$close,
                    close_forecast = last_point$fitted_actual,
                    date = last_point$date,
      )
  }

  return(sum_tbl)
}

#' helper function to process and join forecast output from renarin_short
#'
#' @param fit model fit object (from nnetar)
#' @param fcast model forecast object
#' @param df model df
#' @inheritParams renarin_short_historic
#'
#' @importFrom magrittr %>%

process_fit_historic <- function(fit, fcast, df, PI) {
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
  fcast_df$rank <- (nrow(fit_df)+1):nrow(df)
  fcast_df <- fcast_df %>%
    dplyr::mutate(`.actual` = NA,
                  `.fitted` = .data$point_forecast,
                  `.resid` = NA)

  return(rbind(fit_df, fcast_df))
}


