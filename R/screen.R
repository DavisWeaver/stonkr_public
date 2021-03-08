#' function to iterate over many different tickers
#'
#' @param ncores number of cores to use for parallel processing. Defaults to 1
#' @param ... nnet model parameters to pass to renarin_short
#' @inheritParams renarin_short
#'
#'
#' @importFrom magrittr %>%
#' @importFrom foreach %dopar% %:%
#' @export
#'

renarin_screen <- function(tickers, ncores = 1, vendor = "yahoo",
                           ...) {

  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  forecast_list <-
    foreach::foreach(i = 1:length(tickers),
            .packages = c("stonkr"),
            .errorhandling = "pass") %dopar% {
              renarin_short(
                ticker = tickers[i],
                zoom_in = FALSE,
                PI = FALSE,
                vendor = vendor ,...)
            }
  doParallel::stopImplicitCluster()

  return(dplyr::bind_rows(forecast_list))
}

#' Convenience function to screen the S and P 500
#'
#' @param ncores number of cores to use for parallel processing. Defaults to 1
#' @param ... nnet model parameters to pass to renarin_short
#' @inheritParams renarin_screen
#'
#'
#' @importFrom magrittr %>%
#' @importFrom foreach %dopar% %:%
#' @export
#'

screen_SP500 <- function(ncores = 1, vendor = "yahoo", ...) {
  tickers = get_sp500()
  renarin_screen(tickers = tickers, ncores = ncores,
                 vendor = vendor, ...)
}
