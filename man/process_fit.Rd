% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/short_term.R
\name{process_fit}
\alias{process_fit}
\title{helper function to process and join forecast output from renarin_short}
\usage{
process_fit(fit, fcast, df, look_ahead, PI)
}
\arguments{
\item{fit}{model fit object (from nnetar)}

\item{fcast}{model forecast object}

\item{df}{model_df}

\item{look_ahead}{how many days did we forecast? inherits from \code{\link{renarin_short}}}
}
\description{
helper function to process and join forecast output from renarin_short
}
