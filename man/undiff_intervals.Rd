% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/short_term.R
\name{undiff_intervals}
\alias{undiff_intervals}
\title{helper function to undiff forecasts from renarin_short -}
\usage{
undiff_intervals(df)
}
\arguments{
\item{df}{model output}
}
\description{
tricky because the differences were lagged by one but the forecast lags by 10.
}
