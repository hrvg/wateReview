% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlr_predictions.R
\name{transform_DTM}
\alias{transform_DTM}
\title{This function formats the DTM to be used by the ML models
In particular, merge together terms corresponding to one country, e.g. \code{costa} and \code{rica}}
\usage{
transform_DTM(DTM, change.col = TRUE)
}
\arguments{
\item{DTM, }{a document term matrix}

\item{change.col}{logical, default to \code{TRUE}, controls the modification of the column names for compatibility with other functions}
}
\value{
a DTM
}
\description{
This function formats the DTM to be used by the ML models
In particular, merge together terms corresponding to one country, e.g. \code{costa} and \code{rica}
}
