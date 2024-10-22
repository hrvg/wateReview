% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manual_download.R
\name{plot_estimates}
\alias{plot_estimates}
\title{Plot a time estimate matrix for a different number of downlaoders}
\usage{
plot_estimates(downloaders = seq(7, 7 + 5), times = seq(1, 5, 0.5))
}
\arguments{
\item{downloaders}{numeric, vector of number of downloaders}

\item{times}{numeric, vector of times to download one article}
}
\value{
a heat map plot
}
\description{
Plot a time estimate matrix for a different number of downlaoders
}
